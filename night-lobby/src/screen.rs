use crate::glasshouse::{self, FURNITURE};
use crate::guest::{Cell, Guest};
use crate::hud;
use crate::pixel_fit::PixelFit;
use crate::pixels::{Bounds, H, Pixels, W, harbor, iso};
use crate::{App, BOARD};
use crate::{plaza::Place, square};
use playbit_api::ui::{
    CanvasExtent, CanvasFrames, CanvasItem, CanvasOptions, CanvasPixels, CanvasRect, CanvasText,
    Color, MenuItem, ThemeColor, ThemeOverride, ViewEncoder, WindowSizeConstraints,
};

const WINDOW_PADDING: u32 = 2 * 16;
const SESSION_TITLEBAR_HEIGHT: u32 = 32;
const CHAT_DOCK_HEIGHT: u32 = 116;

fn window_size_constraints(chat_open: bool) -> WindowSizeConstraints {
    // Playbit measures window constraints before its session titlebar and padding.
    let min_width = (W as u32 + WINDOW_PADDING) as f32;
    let chat_height = if chat_open { CHAT_DOCK_HEIGHT } else { 0 };
    let min_height = (H as u32 + WINDOW_PADDING + SESSION_TITLEBAR_HEIGHT + chat_height) as f32;
    WindowSizeConstraints {
        min_width,
        min_height,
        // Round toward a taller window so AppKit can use the exact minimum width.
        aspect_ratio: (min_width / min_height).next_down(),
        ..Default::default()
    }
}

pub fn theme(view: &mut ViewEncoder, app: &App) {
    use ThemeColor::*;
    let colors = [
        (WindowBackground, harbor::sky_color(app.outside())),
        (ControlBackground, 0x283947),
        (Text, 0xffeed7),
        (TextSecondary, 0xc1cfcc),
        (PrimaryBackground, 0x345c58),
        (PrimaryPressed, 0x294c49),
        (PrimaryText, 0xfff6df),
        (Border, 0x52646a),
        (FocusRing, 0xb27d46),
        (TextSelection, 0xc5dccd),
        (TextSelectionText, 0x172d32),
    ]
    .map(|(role, c)| ThemeOverride {
        role,
        color: Color(c << 8 | 255),
    });
    view.theme(&colors);
    view.window_size_constraints(window_size_constraints(app.chat_open));
}

pub struct Layout {
    width: u32,
    height: u32,
    left: u32,
    top: u32,
    pub bounds: Bounds,
    fit: PixelFit,
}

impl Layout {
    pub fn new(width: u32, height: u32, density: f32) -> Self {
        let max = CanvasExtent::Viewport.max_edge();
        Self::fit_area(width.clamp(1, max), height.clamp(1, max), density, true)
    }

    pub fn with_chat(width: u32, height: u32, density: f32) -> Self {
        // SurfaceChanged already excludes the host's document margins. Reserve
        // room for a scrollable transcript, the native editor, and layout gaps.
        let width = width.clamp(1, playbit_api::ui::MAX_CANVAS_EDGE);
        let height = height
            .saturating_sub(CHAT_DOCK_HEIGHT)
            .clamp(1, width * H as u32 / W as u32 + 1);
        Self::fit_area(width, height, density, false)
    }

    fn fit_area(width: u32, height: u32, density: f32, clip: bool) -> Self {
        let mut fit = PixelFit::new(width, height, W as u32, H as u32, density);
        let backing = |n: u32| (n as f32 * density).ceil() as u32;
        let zoom = backing(fit.width) / fit.tex_w;
        // Keep the room's integer zoom and extend the harbor to the window
        // edges. The window clips the last texel when a resize splits it.
        let grow = |limit: u32, minimum: u32, base: i32| {
            let max_backing = (2 * base as u32 / fit.sample) * zoom;
            let cap = ((max_backing as f32 / density).ceil() as u32).max(minimum);
            let aligned = |n: &u32| backing(*n) % zoom == 0;
            let cap = cap.min(if clip {
                CanvasExtent::Viewport.max_edge()
            } else {
                limit
            });
            let extent = (limit..=cap)
                .find(aligned)
                .or_else(|| (minimum..=limit.min(cap)).rev().find(aligned))
                .unwrap_or(minimum);
            (extent, backing(extent) / zoom)
        };
        (fit.width, fit.tex_w) = grow(width, fit.width, W);
        (fit.height, fit.tex_h) = grow(height, fit.height, H);
        let bounds = Bounds::centered(
            (fit.tex_w * fit.sample) as i32,
            (fit.tex_h * fit.sample) as i32,
        );
        Self {
            width: width.max(fit.width),
            height: height.max(fit.height),
            left: width.saturating_sub(fit.width) / 2,
            top: height.saturating_sub(fit.height) / 2,
            bounds,
            fit,
        }
    }

    pub fn at(&self, x: u32, y: u32) -> Option<(i32, i32)> {
        let f = &self.fit;
        if !(self.left..self.left + f.width).contains(&x)
            || !(self.top..self.top + f.height).contains(&y)
        {
            return None;
        }
        let px = u64::from(x - self.left) * u64::from(f.tex_w * f.sample) / u64::from(f.width);
        let py = u64::from(y - self.top) * u64::from(f.tex_h * f.sample) / u64::from(f.height);
        (px < self.bounds.w as u64 && py < self.bounds.h as u64)
            .then_some((self.bounds.x + px as i32, self.bounds.y + py as i32))
    }

    fn point(&self, x: i32, y: i32) -> (u32, u32) {
        let f = &self.fit;
        let axis = |p: i32, span: u32, tex: u32| {
            (p.max(0) as u64 * u64::from(span) / u64::from(tex * f.sample)) as u32
        };
        (
            self.left + axis(x - self.bounds.x, f.width, f.tex_w),
            self.top + axis(y - self.bounds.y, f.height, f.tex_h),
        )
    }
}

// Invert the projection on a horizontal plane. The artwork uses 16 world
// units per cell and projects one unit of elevation to two screen pixels.
fn world_at(x: i32, y: i32, z: i32) -> (i32, i32) {
    let (ox, oy) = iso(0, 0, z);
    let a = x - ox;
    let b = 2 * (y - oy);
    ((a + b).div_euclid(4), (b - a).div_euclid(4))
}

pub fn pick(place: Place, x: i32, y: i32) -> Option<Cell> {
    if square::door_hit(place, x, y) {
        return Some(place.door());
    }
    // Pick the raised cushion or stool top before looking at the floor.
    if place == Place::Bar {
        let (u, v) = world_at(x, y, 13);
        if (9..=24).contains(&u) && (32..=76).contains(&v) {
            return Some((1, 2 + ((v - 32) / 15).min(2)));
        }
        let (u, v) = world_at(x, y, 14);
        if (84..=97).contains(&u) && (93..=105).contains(&v) {
            return Some((5, 6));
        }
    } else {
        for (u, v) in [(3, 7), (5, 7)] {
            let (cx, cy) = iso(16 * u + 8, 16 * v + 8, 8);
            let (dx, dy) = (x - cx, y - cy);
            if (dx * 5).pow(2) + (dy * 11).pow(2) <= (11_i32 * 5).pow(2) {
                return Some((u, v));
            }
        }
    }
    let (u, v) = world_at(x, y, 0);
    let cell = (u.div_euclid(16), v.div_euclid(16));
    place.walkable(cell).then_some(cell)
}

fn rect(x: u32, y: u32, w: u32, h: u32, color: u32) -> CanvasRect {
    CanvasRect {
        x,
        y,
        width: w,
        height: h,
        color: Color(color),
    }
}
fn paint(art: &mut Pixels, app: &App, now: u64) {
    if app.outside() {
        square::ambient(art, now as u32);
    } else {
        glasshouse::ambient(art, now as u32);
    }
    enum Layer<'a> {
        Guest(&'a Guest, bool),
        Prop(usize),
        Street(Cell),
    }
    let mut layers: Vec<_> = if app.outside() {
        square::PROPS
            .iter()
            .map(|&(u, v)| ((u + v + 1) * 16, Layer::Street((u, v))))
            .collect()
    } else {
        FURNITURE
            .iter()
            .enumerate()
            .map(|(i, f)| (f.3, Layer::Prop(i)))
            .collect()
    };
    for (id, guest) in app.actors() {
        let (u, v) = guest.position(now);
        let mut key = ((u + v + 1.) * 16.) as i32;
        if guest.place == Place::Bar && guest.place.seat(guest.cell) && !guest.walking(now) {
            key = if guest.cell.0 == 1 { 82 } else { 176 };
        }
        layers.push((key, Layer::Guest(guest, id == app.room.local.account)));
    }
    layers.sort_by_key(|(z, _)| *z);
    for (_, layer) in layers {
        match layer {
            Layer::Guest(g, local) => g.draw(art, now, local),
            Layer::Street(cell) => square::prop_at(art, cell, now as u32),
            Layer::Prop(i) => {
                let (kind, u, v, _) = FURNITURE[i];
                glasshouse::furniture_at(art, kind, u, v, now as u32);
            }
        }
    }
}

pub fn portrait(app: &App, view: &mut ViewEncoder) {
    let mut sprite = app.local().clone();
    sprite.sleeping = false;
    app.appearance_draft.apply(&mut sprite);
    sprite.cell = (4, 4);
    sprite.from = (4., 4.);
    sprite.direction = 0;
    sprite.wave_until = 0;
    let (x, y) = sprite.foot(app.now);
    const WIDTH: u32 = 40;
    const HEIGHT: u32 = 52;
    let mut art = Pixels::new(Bounds {
        x: x - 20,
        y: y - 47,
        w: WIDTH as i32,
        h: HEIGHT as i32,
    });
    art.rect(x - 20, y - 47, WIDTH as i32, HEIGHT as i32, 0x1d2e40);
    art.oval(x, y, 13, 4, 0x324a52);
    sprite.draw(&mut art, app.now, false);
    let fit = PixelFit::new(120, 156, WIDTH, HEIGHT, app.density);
    let bytes = fit.pixels(&art.rgba, WIDTH, HEIGHT);
    let item = CanvasItem::Pixels(CanvasPixels {
        bounds: rect(
            (120 - fit.width) / 2,
            (156 - fit.height) / 2,
            fit.width,
            fit.height,
            0,
        ),
        source_width: fit.tex_w,
        source_height: fit.tex_h,
        rgba: &bytes,
    });
    view.canvas(11, 120, 156, CanvasFrames::Static, &[item]);
}

pub fn render(app: &mut App, view: &mut ViewEncoder) {
    let layout = app.layout();
    let fit = &layout.fit;
    let sample = fit.sample as i32;
    let place = app.place();
    if app.backdrop.as_ref().is_some_and(|(cached_place, pixels)| {
        *cached_place != place || pixels.bounds != layout.bounds || pixels.sample != sample
    }) {
        app.backdrop = None;
    }
    let (_, background) = app.backdrop.get_or_insert_with(|| {
        let mut pixels = Pixels::sampled(layout.bounds, sample);
        match place {
            Place::Square => square::background(&mut pixels),
            Place::Bar => glasshouse::background(&mut pixels),
        }
        (place, pixels)
    });
    let mut art = background.clone();
    paint(&mut art, app, app.now);
    let native = hud::draw(&mut art, app);
    let mut items = vec![CanvasItem::Pixels(CanvasPixels {
        bounds: rect(layout.left, layout.top, fit.width, fit.height, 0),
        source_width: fit.tex_w,
        source_height: fit.tex_h,
        rgba: &art.rgba,
    })];
    for line in &native {
        let (x, y) = layout.point(line.x, line.y);
        let (right, bottom) = layout.point(line.x + line.width + 1, line.y + 15);
        let width = right.min(layout.width).saturating_sub(x);
        let height = bottom.min(layout.height).saturating_sub(y);
        if width == 0 || height == 0 {
            continue;
        }
        items.push(CanvasItem::Text(CanvasText {
            bounds: rect(x, y, width, height, line.color << 8 | 255),
            font_size: (11 * u64::from(fit.height) / u64::from(fit.tex_h * fit.sample)).clamp(1, 96)
                as u32,
            bold: false,
            text: &line.text,
        }));
    }
    let menu: Vec<_> = hud::labels(app.outside(), app.chat_open)
        .into_iter()
        .chain([(crate::PROFILE, "My avatar")])
        .map(|(id, label)| MenuItem::Action {
            id,
            label,
            icon: None,
            checked: None,
            disabled: false,
            destructive: false,
        })
        .collect();
    view.context_menu_start(&menu);
    view.canvas_with_options(
        BOARD,
        layout.width,
        layout.height,
        CanvasOptions {
            frames: CanvasFrames::Continuous,
            extent: if app.chat_open {
                CanvasExtent::Inline
            } else {
                CanvasExtent::Viewport
            },
            clear_color: Some(Color(harbor::sky_color(app.outside()) << 8 | 255)),
        },
        &items,
    );
    view.context_menu_end();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sampled_frame_matches_the_full_scene_at_each_texel_center() {
        let app = App::default();
        let bounds = Bounds::centered(W + 24, H + 8);
        let draw = |sample| {
            let mut pixels = Pixels::sampled(bounds, sample);
            glasshouse::background(&mut pixels);
            paint(&mut pixels, &app, 133);
            hud::draw(&mut pixels, &app);
            pixels.rgba
        };
        let original = draw(1);
        for sample in [2, 4, 8] {
            let small = draw(sample);
            for (i, pixel) in small.as_chunks::<4>().0.iter().enumerate() {
                let width = (bounds.w / sample) as usize;
                let x = (i % width) * sample as usize + (sample / 2) as usize;
                let y = (i / width) * sample as usize + (sample / 2) as usize;
                assert_eq!(
                    *pixel,
                    original.as_chunks::<4>().0[y * bounds.w as usize + x]
                );
            }
        }
    }

    #[test]
    fn resizes_keep_the_integer_zoom_and_hud_at_the_window_edges() {
        for density in [1.0, 1.25, 1.5, 2.0, 3.0] {
            for width in (640..=1440).step_by(7) {
                let height = (width * 7 + 5) / 10;
                let layout = Layout::new(width, height, density);
                let fit = &layout.fit;
                let backing = |n: u32| (n as f32 * density).ceil() as u32;
                let base = PixelFit::new(width, height, W as u32, H as u32, density);
                let zoom = backing(base.width) / base.tex_w;
                assert_eq!(backing(fit.width), fit.tex_w * zoom);
                assert_eq!(backing(fit.height), fit.tex_h * zoom);
                assert!(
                    (width..=width + 4).contains(&fit.width)
                        && (height..=height + 4).contains(&fit.height),
                    "{width}×{height} at {density}x is covered by {}×{} points",
                    fit.width,
                    fit.height
                );
                assert!(layout.bounds.contains(0, 0) && layout.bounds.contains(W - 1, H - 1));
                for (_, _, hit) in hud::actions(layout.bounds, false, false) {
                    let (sx, sy) = layout.point(hit.x + hit.w / 2, hit.y + hit.h / 2);
                    let (x, y) = layout.at(sx, sy).unwrap();
                    assert!(hit.contains(x, y));
                }
            }
        }
    }

    #[test]
    fn the_minimum_window_fits_the_full_scale_canvas() {
        for chat_open in [false, true] {
            let window = window_size_constraints(chat_open);
            assert!(
                f64::from(window.min_height) * f64::from(window.aspect_ratio)
                    <= f64::from(window.min_width)
            );
            let width = window.min_width as u32 - WINDOW_PADDING;
            let height = window.min_height as u32 - WINDOW_PADDING - SESSION_TITLEBAR_HEIGHT;
            for density in [1., 2.] {
                let layout = if chat_open {
                    Layout::with_chat(width, height, density)
                } else {
                    Layout::new(width, height, density)
                };
                assert_eq!(layout.fit.sample, 1);
                assert_eq!((layout.fit.width, layout.fit.height), (W as u32, H as u32));
            }
        }
    }

    #[test]
    fn the_chat_dock_leaves_space_for_the_transcript_and_keeps_the_board_clickable() {
        for (width, height) in [(640, 520), (768, 648), (992, 840), (1504, 1280)] {
            for density in [1., 1.25, 1.5, 2., 3.] {
                let layout = Layout::with_chat(width, height, density);
                assert!(layout.width <= width && layout.height + CHAT_DOCK_HEIGHT <= height);
                let f = &layout.fit;
                let backing = |n: u32| (n as f32 * density).ceil() as u32;
                assert_eq!(backing(f.width) % f.tex_w, 0);
                assert_eq!(backing(f.height) % f.tex_h, 0);
                assert_eq!(backing(f.width) / f.tex_w, backing(f.height) / f.tex_h);
                let (x, y) = iso(136, 136, 0);
                let (sx, sy) = layout.point(x, y);
                let (x, y) = layout.at(sx, sy).unwrap();
                assert_eq!(pick(Place::Bar, x, y), Some((8, 8)));
                assert!(layout.at(sx, layout.height + 1).is_none());
            }
        }
    }

    #[test]
    fn screen_clicks_match_floor_positions_at_integer_scales() {
        for (w, h, density) in [(768, 616, 2.), (608, 480, 2.), (1320, 1000, 1.)] {
            let layout = Layout::new(w, h, density);
            for (u, v) in [(8, 8), (3, 8), (8, 3), (2, 6)] {
                let (x, y) = iso(u * 16 + 8, v * 16 + 8, 0);
                let (sx, sy) = layout.point(x, y);
                let (px, py) = layout.at(sx, sy).unwrap();
                for place in [Place::Bar, Place::Square] {
                    assert_eq!(pick(place, px, py), Some((u, v)));
                }
            }
        }
    }
}
