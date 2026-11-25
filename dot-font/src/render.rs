use crate::font::{self, Glyph};
use crate::layout::{Box2, Layout};
use crate::{App, BOARD};
use playbit_api::ui::{
    CanvasFrames, CanvasItem, CanvasPixels, CanvasRect, CanvasText, Color, ViewEncoder,
};
use std::collections::BTreeMap;
use tiny_skia::{FillRule, LineCap, Paint, Path, PathBuilder, Pixmap, Stroke, Transform};

const PAPER: u8 = 255;
const INK: u8 = 16;
const MUTED: u8 = 104;

pub type Cache = BTreeMap<char, (Glyph, Option<Path>, Option<Path>)>;

fn outline<'a>(cache: &'a mut Cache, ch: char, g: &Glyph, curves: bool) -> Option<&'a Path> {
    let item = cache.entry(ch).or_insert_with(|| {
        let (a, b) = g.outlines();
        (g.clone(), a, b)
    });
    if item.0 != *g {
        let (a, b) = g.outlines();
        *item = (g.clone(), a, b);
    }
    if curves {
        item.2.as_ref()
    } else {
        item.1.as_ref()
    }
}

struct Label {
    rect: Box2,
    size: u32,
    value: String,
    gray: u8,
    bold: bool,
}
enum Graphic {
    Solid(CanvasRect),
    Image(CanvasRect, Pixmap),
}

struct Sheet {
    graphics: Vec<Graphic>,
    width: i32,
    height: i32,
    scale: f32,
    labels: Vec<Label>,
}

fn paint(k: u8) -> Paint<'static> {
    let mut p = Paint::default();
    p.set_color_rgba8(k, k, k, 255);
    p
}

impl Sheet {
    fn bounds(&self, b: Box2, color: Color) -> Option<CanvasRect> {
        let x = b.x.max(0);
        let y = b.y.max(0);
        let w = b.right().min(self.width) - x;
        let h = b.bottom().min(self.height) - y;
        (w > 0 && h > 0).then_some(CanvasRect {
            x: x as u32,
            y: y as u32,
            width: w as u32,
            height: h as u32,
            color,
        })
    }
    fn rect(&mut self, b: Box2, gray: u8) {
        if let Some(r) = self.bounds(b, Color(u32::from(gray) * 0x01010100 | 255)) {
            self.graphics.push(Graphic::Solid(r));
        }
    }
    // Playbit samples RGBA images at their destination bounds. Rasterize only
    // the ink, at the display density; grid lines and flat fills stay native.
    fn image(&mut self, path: &Path, t: Transform, b: Box2, gray: u8) {
        let Some(rect) = self.bounds(b, Color(0)) else {
            return;
        };
        let pw = ((rect.width as f32 * self.scale).round() as u32).max(1);
        let ph = ((rect.height as f32 * self.scale).round() as u32).max(1);
        let mut pix = Pixmap::new(pw, ph).unwrap();
        let transform = t
            .post_translate(-(rect.x as f32), -(rect.y as f32))
            .post_scale(
                pw as f32 / rect.width as f32,
                ph as f32 / rect.height as f32,
            );
        pix.fill_path(path, &paint(255), FillRule::Winding, transform, None);
        for pixel in pix.data_mut().chunks_exact_mut(4) {
            pixel[..3].fill(gray);
        }
        self.graphics.push(Graphic::Image(rect, pix));
    }
    fn path(&mut self, p: &Path, t: Transform, gray: u8) {
        if let Some(b) = p.bounds().transform(t).and_then(|r| r.round_out()) {
            self.image(
                p,
                t,
                Box2::new(b.x(), b.y(), b.width() as i32, b.height() as i32),
                gray,
            );
        }
    }
    fn round(&mut self, b: Box2, r: f32, gray: u8) {
        let (x, y, u, v) = (b.x as f32, b.y as f32, b.right() as f32, b.bottom() as f32);
        let mut p = PathBuilder::new();
        p.move_to(x + r, y);
        p.line_to(u - r, y);
        p.quad_to(u, y, u, y + r);
        p.line_to(u, v - r);
        p.quad_to(u, v, u - r, v);
        p.line_to(x + r, v);
        p.quad_to(x, v, x, v - r);
        p.line_to(x, y + r);
        p.quad_to(x, y, x + r, y);
        p.close();
        let Some(path) = p.finish() else { return };
        let n = r.ceil() as i32;
        self.rect(Box2::new(b.x + n, b.y, b.w - 2 * n, b.h), gray);
        self.rect(Box2::new(b.x, b.y + n, n, b.h - 2 * n), gray);
        self.rect(Box2::new(b.right() - n, b.y + n, n, b.h - 2 * n), gray);
        for x in [b.x, b.right() - n] {
            for y in [b.y, b.bottom() - n] {
                self.image(&path, Transform::identity(), Box2::new(x, y, n, n), gray);
            }
        }
    }
    fn circle(&mut self, x: f32, y: f32, r: f32, gray: u8) {
        self.fill(PathBuilder::from_circle(x, y, r), gray);
    }
    fn fill(&mut self, path: Option<Path>, gray: u8) {
        if let Some(p) = path {
            self.path(&p, Transform::identity(), gray);
        }
    }
    fn line(&mut self, points: &[(f32, f32)], width: f32, gray: u8) {
        let mut p = PathBuilder::new();
        p.move_to(points[0].0, points[0].1);
        for &(x, y) in &points[1..] {
            p.line_to(x, y);
        }
        let stroke = Stroke {
            width,
            line_cap: LineCap::Round,
            ..Default::default()
        };
        self.fill(p.finish().and_then(|p| p.stroke(&stroke, 2.)), gray);
    }
    fn label(&mut self, rect: Box2, value: impl Into<String>, size: u32, gray: u8, bold: bool) {
        self.labels.push(Label {
            rect,
            size,
            value: value.into(),
            gray,
            bold,
        });
    }
    fn glyph(
        &mut self,
        cache: &mut Cache,
        ch: char,
        g: &Glyph,
        x: f32,
        base: f32,
        unit: f32,
        curves: bool,
        gray: u8,
    ) {
        if let Some(path) = outline(cache, ch, g, curves) {
            self.path(path, Transform::from_row(unit, 0., 0., unit, x, base), gray);
        }
    }
    fn chip(&mut self, b: Box2, title: &str) {
        self.round(b, 5., 231);
        self.round(b.inset(1), 4., 252);
        self.label(Box2::new(b.x + 7, b.y, b.w - 8, b.h), title, 11, 59, false);
    }
}

fn chevron(s: &mut Sheet, b: Box2, forward: bool, enabled: bool) {
    let x = (b.x + b.w / 2) as f32;
    let y = (b.y + b.h / 2) as f32;
    let d = if forward { 1. } else { -1. };
    s.round(b, 5., 247);
    s.line(
        &[(x - 2. * d, y - 4.), (x + 2. * d, y), (x - 2. * d, y + 4.)],
        1.5,
        if enabled { INK } else { 195 },
    );
}

fn specimen(s: &mut Sheet, app: &App, l: &Layout, cache: &mut Cache) {
    let b = l.sample;
    s.round(b, 8., PAPER);
    s.label(Box2::new(12, 7, 120, 23), "Specimen", 12, INK, true);
    s.chip(l.export, "Export…");
    s.chip(l.size, &format!("{} px", app.size));
    let unit = app.size as f32 / 11.;
    let left = 12.;
    let mut x = left;
    let mut base = 43. + 8. * unit;
    let advance = 7. * unit;
    let leading = (14. * unit).ceil();
    // Submit each typeset line as one mask so long specimens stay within the
    // host’s per-scene item limit, even in a very tall window.
    let mut line_path = PathBuilder::new();
    let flush = |s: &mut Sheet, line: &mut PathBuilder| {
        s.fill(std::mem::take(line).finish(), INK);
    };
    for line in app.sample.split('\n') {
        for (i, word) in line.split(' ').enumerate() {
            if i != 0 {
                if x + advance * (1 + word.chars().count()) as f32 > (b.right() - 10) as f32 {
                    flush(s, &mut line_path);
                    x = left;
                    base += leading;
                } else {
                    x += advance;
                }
            }
            for requested in word.chars() {
                if x + advance > (b.right() - 10) as f32 {
                    flush(s, &mut line_path);
                    x = left;
                    base += leading;
                }
                if base + 3. * unit > (b.bottom() - 9) as f32 {
                    flush(s, &mut line_path);
                    return;
                }
                let ch = if app.doc.glyphs.contains_key(&requested) {
                    requested
                } else {
                    '?'
                };
                let glyph = app.display_glyph(ch, &app.doc.glyphs[&ch]);
                if let Some(shape) = outline(cache, ch, glyph, app.curves) {
                    let t = Transform::from_row(unit, 0., 0., unit, x, base);
                    if let Some(p) = shape.clone().transform(t) {
                        line_path.push_path(&p);
                    }
                }
                x += advance;
            }
        }
        flush(s, &mut line_path);
        base += leading;
        x = left;
    }
}

fn palette(s: &mut Sheet, app: &App, l: &Layout, cache: &mut Cache) {
    let b = l.palette;
    s.round(b, 8., PAPER);
    s.label(Box2::new(12, b.y + 7, 72, 23), "Glyphs", 12, INK, true);
    s.label(
        Box2::new(b.right() - 50, b.y + 7, 42, 23),
        app.doc.order.len().to_string(),
        11,
        MUTED,
        false,
    );
    let start = app.page * l.count;
    for (i, &ch) in app.doc.order.iter().skip(start).take(l.count).enumerate() {
        let tile = l.glyph_tile(i);
        let selected = ch == app.selected;
        if selected {
            s.round(tile, 5., INK);
        } else if app.hover.is_some_and(|(x, y)| tile.contains(x, y)) {
            s.round(tile, 5., 244);
        }
        let size = (tile.w as f32 / 14.).floor().max(1.);
        let x = (tile.x as f32 + (tile.w as f32 - 7. * size) / 2.).round();
        let y = (tile.y as f32 + tile.h as f32 / 2. + 3. * size).round();
        s.glyph(
            cache,
            ch,
            app.display_glyph(ch, &app.doc.glyphs[&ch]),
            x,
            y,
            size,
            app.curves,
            if selected { PAPER } else { INK },
        );
    }
    s.round(l.add, 5., 240);
    let (x, y) = ((l.add.x + 13) as f32, (l.add.y + 12) as f32);
    s.line(&[(x - 4., y), (x + 4., y)], 1.5, INK);
    s.line(&[(x, y - 4.), (x, y + 4.)], 1.5, INK);
    let pages = app.doc.order.len().div_ceil(l.count);
    s.label(
        Box2::new(45, l.add.y, b.w - 120, 25),
        format!("{} / {}", app.page + 1, pages),
        11,
        MUTED,
        false,
    );
    chevron(s, l.prev, false, app.page > 0);
    chevron(s, l.next, true, app.page + 1 < pages);
}

fn board(s: &mut Sheet, app: &App, l: &Layout, cache: &mut Cache) {
    s.round(l.editor, 8., PAPER);
    let b = l.grid;
    s.round(b, 5., 241);
    s.rect(Box2::new(l.left, b.y, 7 * l.unit, b.h), PAPER);
    let mut x = l.left - ((l.left - b.x) / l.unit) * l.unit;
    while x < b.right() - 2 {
        let edge = x == l.left || x == l.left + 7 * l.unit;
        s.rect(
            Box2::new(x, b.y + 2, 1, b.h - 4),
            if edge {
                202
            } else if x < l.left || x > l.left + 7 * l.unit {
                227
            } else {
                239
            },
        );
        x += l.unit;
    }
    let mut y = l.base - ((l.base - b.y) / l.unit) * l.unit;
    while y < b.bottom() - 2 {
        let gray = if y == l.base {
            171
        } else if y == l.base - 8 * l.unit || y == l.base - 6 * l.unit {
            209
        } else {
            234
        };
        s.rect(Box2::new(b.x + 2, y, b.w - 4, 1), gray);
        y += l.unit;
    }
    for (row, title) in [(8, "cap"), (6, "x"), (0, "base")] {
        let y = l.base - row * l.unit;
        s.label(Box2::new(b.x + 8, y - 17, 37, 16), title, 10, 145, false);
    }
    let glyph = app.display_glyph(app.selected, &app.doc.glyphs[&app.selected]);
    let u = l.unit as f32;
    if !app.curves {
        for cell in glyph.pixels() {
            let x = l.left + i32::from(cell.0) * l.unit;
            let y = l.base - (i32::from(cell.1) + 1) * l.unit;
            s.rect(Box2::new(x, y, l.unit, l.unit), 0);
        }
    } else {
        s.glyph(
            cache,
            app.selected,
            glyph,
            l.left as f32,
            l.base as f32,
            u,
            true,
            0,
        );
        if app.lines {
            let mut guide = PathBuilder::new();
            for &font::Link(a, b) in &glyph.links {
                if glyph.at(a) != 0 && glyph.at(b) != 0 {
                    let (x, y) = l.center(a);
                    guide.move_to(x, y);
                    let (x, y) = l.center(b);
                    guide.line_to(x, y);
                }
            }
            let stroke = Stroke {
                width: 1.1,
                line_cap: LineCap::Round,
                ..Default::default()
            };
            s.fill(guide.finish().and_then(|p| p.stroke(&stroke, 2.)), 145);
            if let Some(crate::Gesture {
                tool: crate::Tool::Line { from, cursor, .. },
                ..
            }) = &app.gesture
            {
                let start = l.center(*from);
                let end = (cursor.0 as f32, cursor.1 as f32);
                s.line(&[start, end], 2.8, 255);
                s.line(&[start, end], 1.2, 48);
            }
        }
        for (c, _) in glyph.points() {
            let (x, y) = l.center(c);
            let selected = app.point == Some(c);
            let ink = if selected { 255 } else { 114 };
            if selected {
                s.circle(x, y, u * 0.243, 255);
                s.circle(x, y, u * 0.179, 16);
            }
            s.circle(x, y, u * 0.109, ink);
            if !app.lines && glyph.show_handle(c) {
                let (a, b) = glyph.tangent(c);
                let len = f32::from(a).hypot(f32::from(b));
                let (dx, dy) = (f32::from(a) / len, -f32::from(b) / len);
                let reach = if selected { 0.44 } else { 0.41 };
                let (ex, ey) = (x + u * reach * dx, y + u * reach * dy);
                let d = u * 0.115;
                s.line(
                    &[(x + u * 0.19 * dx, y + u * 0.19 * dy), (ex, ey)],
                    1.6,
                    ink,
                );
                s.line(
                    &[
                        (ex - d * (dx - dy), ey - d * (dy + dx)),
                        (ex, ey),
                        (ex - d * (dx + dy), ey - d * (dy - dx)),
                    ],
                    1.6,
                    ink,
                );
            }
        }
        if app.lines {
            s.round(l.lines, 5., INK);
            s.label(
                Box2::new(l.lines.x + 8, l.lines.y, 40, l.lines.h),
                "Lines",
                11,
                PAPER,
                true,
            );
        } else {
            s.chip(l.lines, "Lines");
        }
        if app.point.is_some() {
            if !app.lines {
                s.chip(l.auto, "Auto");
            }
            s.chip(l.erase, "Delete");
        }
    }
    if !app.undo.is_empty() || !app.redo.is_empty() {
        s.chip(l.undo, "↶");
        s.chip(l.redo, "↷");
    }
    s.chip(l.jump, &font::codepoint(app.selected));
    let status = if !app.store.error.is_empty() {
        app.store.error.as_str()
    } else if app.store.pending() {
        "Saving…"
    } else if !app.store.loaded {
        "Loading…"
    } else if app.curves && app.lines {
        "Drag between dots to join or cut"
    } else if app.curves {
        "Drag a point to set its direction"
    } else {
        "Drag to paint or erase"
    };
    let hint_w = l.mode.x - l.jump.right() - 14;
    if hint_w > 45 {
        let short = if hint_w < 164
            && app.store.error.is_empty()
            && app.store.loaded
            && !app.store.pending()
        {
            if app.curves && app.lines {
                "Join or cut"
            } else if app.curves {
                "Drag to aim"
            } else {
                "Drag to paint"
            }
        } else {
            status
        };
        s.label(
            Box2::new(l.jump.right() + 8, l.jump.y, hint_w, l.jump.h),
            short,
            11,
            MUTED,
            false,
        );
    }
    let b = l.mode;
    s.round(b, 6., 241);
    let (x, y) = ((b.x + 15) as f32, (b.y + 13) as f32);
    let square = Box2::new(b.right() - 21, b.y + 7, 12, 12);
    s.circle(x, y, 6.5, INK);
    s.rect(square, INK);
    if app.curves {
        s.rect(square.inset(2), 241);
    } else {
        s.circle(x, y, 4.5, 241);
    }
}

pub fn scene(app: &App, view: &mut ViewEncoder, cache: &mut Cache) {
    let (w, h) = app.surface;
    let scale = app
        .density
        .min(2.)
        .min((2_000_000. / (w * h) as f32).sqrt())
        .min(4096. / w.max(h) as f32);
    let mut s = Sheet {
        graphics: Vec::new(),
        width: w as i32,
        height: h as i32,
        scale,
        labels: Vec::new(),
    };
    s.rect(Box2::new(0, 0, w as i32, h as i32), 235);
    let l = app.layout();
    specimen(&mut s, app, &l, cache);
    palette(&mut s, app, &l, cache);
    board(&mut s, app, &l, cache);
    let mut items: Vec<_> = s
        .graphics
        .iter()
        .map(|g| match g {
            Graphic::Solid(rect) => CanvasItem::Rect(*rect),
            Graphic::Image(bounds, pix) => CanvasItem::Pixels(CanvasPixels {
                bounds: *bounds,
                source_width: pix.width(),
                source_height: pix.height(),
                rgba: pix.data(),
            }),
        })
        .collect();
    for label in &s.labels {
        let b = label.rect;
        if b.x < 0
            || b.y < 0
            || b.w <= 0
            || b.h <= 0
            || b.right() > w as i32
            || b.bottom() > h as i32
        {
            continue;
        }
        let k = u32::from(label.gray);
        items.push(CanvasItem::Text(CanvasText {
            bounds: CanvasRect {
                x: b.x as u32,
                y: b.y as u32,
                width: b.w as u32,
                height: b.h as u32,
                color: Color(k * 0x01010100 | 255),
            },
            font_size: label.size,
            bold: label.bold,
            text: &label.value,
        }));
    }
    view.viewport_canvas(BOARD, w, h, CanvasFrames::Static, &items);
}
