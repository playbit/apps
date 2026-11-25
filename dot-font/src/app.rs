mod export;
mod font;
mod layout;
mod opentype;
mod render;
mod storage;

use font::{AUTO, Cell, EMPTY, Glyph, Link};
use layout::Layout;
use playbit_api::abi::{
    environment::Surface,
    syscall::{EventKind, KeyInput, PointerInput},
};
use playbit_api::context::Context;
use playbit_api::ui::{
    ButtonStyle, Color, TextStyle, ThemeColor, ThemeOverride, ViewEncoder, WindowSizeConstraints,
    WindowStyle,
};
use storage::{Document, Store};

const BOARD: u32 = 1;
const FIELD: u32 = 2;
const APPLY: u32 = 3;
const CANCEL: u32 = 4;
const EXPORT_PIXELS: u32 = 5;
const EXPORT_CURVES: u32 = 6;
const COPY_COMMAND: u32 = 7;
const TOGGLE_LINES: u32 = 8;
const DELETE_POINT: u32 = 9;
const DESTINATION: u32 = 100;

const SPECIMEN: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789!?.\nTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG\nthe quick brown fox jumps over the lazy dog\n12.4pt  64%  90px  45kg";

#[derive(Clone, Copy)]
enum Form {
    Character,
    Specimen,
    Export,
}

enum Tool {
    Paint {
        value: Option<bool>,
        last: Option<Cell>,
    },
    Aim {
        cell: Cell,
    },
    Line {
        from: Cell,
        to: Option<Cell>,
        cursor: (i32, i32),
    },
}

struct Gesture {
    tool: Tool,
    original: Glyph,
    draft: Glyph,
    start: (i32, i32),
    moved: bool,
}

struct History {
    ch: char,
    changes: Vec<(Cell, u8, u8)>,
    lines: Vec<(Link, bool, bool)>,
    pixels: Vec<(Cell, Option<bool>, Option<bool>)>,
}

impl History {
    fn is_empty(&self) -> bool {
        self.changes.is_empty() && self.lines.is_empty() && self.pixels.is_empty()
    }

    fn save(
        &self,
        store: &mut Store,
        ctx: &mut Context,
        doc: &mut Document,
        reverse: bool,
    ) -> bool {
        fn values<K: Copy, V: Copy>(changes: &[(K, V, V)], reverse: bool) -> Vec<(K, V)> {
            changes
                .iter()
                .map(|&(key, old, new)| (key, if reverse { old } else { new }))
                .collect()
        }
        store.save(
            ctx,
            doc,
            self.ch,
            values(&self.changes, reverse),
            values(&self.lines, reverse),
            values(&self.pixels, reverse),
        )
    }
}

struct App {
    doc: Document,
    store: Store,
    export: export::Export,
    surface: (u32, u32),
    density: f32,
    selected: char,
    point: Option<Cell>,
    curves: bool,
    lines: bool,
    reset_pixels: bool,
    last_click: Option<(Cell, i32, i32, u64)>,
    sample: String,
    size: u8,
    palette_anchor: char,
    gesture: Option<Gesture>,
    hover: Option<(i32, i32)>,
    undo: Vec<History>,
    redo: Vec<History>,
    form: Option<Form>,
    input: String,
    form_error: String,
    cache: render::Cache,
}

impl Default for App {
    fn default() -> Self {
        let selected = 'a';
        Self {
            doc: Document::default(),
            store: Store::default(),
            export: Default::default(),
            surface: (800, 500),
            density: 1.,
            selected,
            point: None,
            curves: true,
            lines: false,
            reset_pixels: false,
            last_click: None,
            sample: SPECIMEN.to_owned(),
            size: 11,
            palette_anchor: selected,
            gesture: None,
            hover: None,
            undo: Vec::new(),
            redo: Vec::new(),
            form: None,
            input: String::new(),
            form_error: String::new(),
            cache: Default::default(),
        }
    }
}

impl App {
    fn layout(&self) -> Layout {
        Layout::new(self.surface.0 as i32, self.surface.1 as i32)
    }

    fn glyph_index(&self, ch: char) -> usize {
        self.doc.order.binary_search(&ch).expect("glyph in palette")
    }

    fn palette_page(&self, layout: &Layout) -> usize {
        self.glyph_index(self.palette_anchor) / layout.count
    }

    fn display_glyph<'a>(&'a self, ch: char, g: &'a Glyph) -> &'a Glyph {
        if ch == self.selected
            && let Some(drag) = &self.gesture
        {
            &drag.draft
        } else {
            g
        }
    }

    fn select(&mut self, ch: char) {
        self.last_click = None;
        self.selected = ch;
        self.point = None;
        self.gesture = None;
        self.palette_anchor = ch;
    }

    fn show_form(&mut self, form: Form) {
        self.last_click = None;
        self.form = Some(form);
        self.form_error.clear();
        self.input = match form {
            Form::Character | Form::Export => String::new(),
            Form::Specimen => self.sample.clone(),
        };
    }

    fn apply_form(&mut self, ctx: &mut Context) {
        match self.form {
            Some(Form::Character) => {
                let Some(ch) = font::parse_character(&self.input) else {
                    self.form_error =
                        "Enter one character or a Unicode value such as U+0061.".into();
                    return;
                };
                if !self.doc.glyphs.contains_key(&ch) {
                    if !self.store.loaded {
                        self.form_error = "Waiting for the font to finish loading.".into();
                        return;
                    }
                    if !self
                        .store
                        .save(ctx, &mut self.doc, ch, vec![], vec![], vec![])
                    {
                        self.form_error = self.store.error.clone();
                        return;
                    }
                }
                self.select(ch);
            }
            Some(Form::Specimen) => self.sample = self.input.chars().take(4096).collect(),
            Some(Form::Export) | None => return,
        }
        self.form = None;
    }

    fn change(
        &mut self,
        ctx: &mut Context,
        changes: Vec<(Cell, u8, u8)>,
        lines: Vec<(Link, bool, bool)>,
        pixels: Vec<(Cell, Option<bool>, Option<bool>)>,
    ) -> bool {
        let edit = History {
            ch: self.selected,
            changes,
            lines,
            pixels,
        };
        if edit.is_empty() || !self.store.loaded {
            return false;
        }
        if edit.save(&mut self.store, ctx, &mut self.doc, false) {
            if self.undo.len() >= 128 {
                self.undo.remove(0);
            }
            self.undo.push(edit);
            self.redo.clear();
            return true;
        }
        false
    }

    fn history(&mut self, ctx: &mut Context, reverse: bool) {
        let source = if reverse {
            &mut self.undo
        } else {
            &mut self.redo
        };
        let Some(mut h) = source.pop() else { return };
        // Preserve cells subsequently changed by another editor.
        h.changes.retain(|&(c, old, new)| {
            self.doc.glyphs[&h.ch].at(c) == if reverse { new } else { old }
        });
        h.lines.retain(|&(edge, old, new)| {
            self.doc.glyphs[&h.ch].links.contains(&edge) == if reverse { new } else { old }
        });
        h.pixels.retain(|&(c, old, new)| {
            self.doc.glyphs[&h.ch].pixel_overrides.get(&c).copied()
                == if reverse { new } else { old }
        });
        if h.is_empty() {
            return;
        }
        if !h.save(&mut self.store, ctx, &mut self.doc, reverse) {
            source.push(h);
            return;
        }
        self.select(h.ch);
        if reverse {
            self.redo.push(h);
        } else {
            self.undo.push(h);
        }
    }

    fn set_point(&mut self, ctx: &mut Context, v: u8) {
        if let Some(c) = self.point {
            let old = self.doc.glyphs[&self.selected].at(c);
            if old != v && old != EMPTY {
                self.change(ctx, vec![(c, old, v)], vec![], vec![]);
            }
        }
    }

    fn delete_selected(&mut self, ctx: &mut Context) {
        let Some(c) = self.point else { return };
        let original = &self.doc.glyphs[&self.selected];
        let old = original.at(c);
        if old == EMPTY {
            self.point = None;
            return;
        }
        self.gesture = None;
        self.last_click = None;
        let mut draft = original.clone();
        let next = draft.delete_point(c);
        let lines = original
            .links
            .symmetric_difference(&draft.links)
            .map(|&e| (e, original.links.contains(&e), draft.links.contains(&e)))
            .collect();
        if self.change(ctx, vec![(c, old, EMPTY)], lines, vec![]) {
            self.point = next;
        }
    }

    fn toggle_mode(&mut self) {
        self.curves = !self.curves;
        self.reset_pixels = false;
        self.point = None;
        self.gesture = None;
        self.last_click = None;
    }
    fn down(&mut self, ctx: &mut Context, l: &Layout, x: i32, y: i32) {
        self.gesture = None;
        if l.cell(x, y).is_none() {
            self.last_click = None;
        }
        if self.curves && l.lines.contains(x, y) {
            self.lines = !self.lines;
            self.point = None;
            self.last_click = None;
            return;
        }
        if !self.curves && l.reset_pixels.contains(x, y) {
            self.reset_pixels = !self.reset_pixels;
            return;
        }
        if l.mode.contains(x, y) {
            self.toggle_mode();
            return;
        }
        if l.export.contains(x, y) {
            self.export = Default::default();
            self.export.discover(ctx);
            self.show_form(Form::Export);
            return;
        }
        if l.size.contains(x, y) {
            self.size = match self.size {
                11 => 16,
                16 => 22,
                22 => 33,
                _ => 11,
            };
            return;
        }
        if l.sample.contains(x, y) {
            self.show_form(Form::Specimen);
            return;
        }
        if l.jump.contains(x, y) || l.add.contains(x, y) {
            self.show_form(Form::Character);
            return;
        }
        let page = self.palette_page(l);
        if l.prev.contains(x, y) {
            if page > 0 {
                self.palette_anchor = self.doc.order[(page - 1) * l.count];
            }
            return;
        }
        if l.next.contains(x, y) {
            if let Some(&ch) = self.doc.order.get((page + 1) * l.count) {
                self.palette_anchor = ch;
            }
            return;
        }
        if l.palette.contains(x, y) {
            for i in 0..l.count {
                if l.glyph_tile(i).contains(x, y) {
                    if let Some(&ch) = self.doc.order.get(page * l.count + i) {
                        self.select(ch);
                    }
                    break;
                }
            }
            return;
        }
        if !self.store.loaded {
            return;
        }
        if (!self.undo.is_empty() || !self.redo.is_empty()) && l.undo.contains(x, y) {
            self.history(ctx, true);
            return;
        }
        if (!self.undo.is_empty() || !self.redo.is_empty()) && l.redo.contains(x, y) {
            self.history(ctx, false);
            return;
        }
        if self.curves && self.point.is_some() {
            if !self.lines && l.auto.contains(x, y) {
                self.set_point(ctx, AUTO);
                return;
            }
            if l.erase.contains(x, y) {
                self.delete_selected(ctx);
                return;
            }
        }
        let Some(cell) = l.cell(x, y) else {
            self.point = None;
            return;
        };
        let original = self.doc.glyphs[&self.selected].clone();
        let mut draft = original.clone();
        let tool = if self.curves && self.lines {
            if draft.at(cell) == EMPTY {
                return;
            }
            self.point = Some(cell);
            Tool::Line {
                from: cell,
                to: None,
                cursor: (x, y),
            }
        } else if self.curves {
            self.point = Some(cell);
            if draft.at(cell) == EMPTY {
                draft.set(cell, font::NO_DIRECTION);
            }
            Tool::Aim { cell }
        } else {
            let value = (!self.reset_pixels).then(|| !draft.pixels().contains(&cell));
            draft.set_pixel_override(cell, value);
            Tool::Paint {
                value,
                last: Some(cell),
            }
        };
        self.gesture = Some(Gesture {
            original,
            draft,
            tool,
            start: (x, y),
            moved: false,
        });
    }

    fn drag(&mut self, l: &Layout, x: i32, y: i32) {
        let Some(g) = &mut self.gesture else { return };
        let (dx, dy) = ((x - g.start.0) as f32, (y - g.start.1) as f32);
        let away = dx.hypot(dy) >= (l.unit as f32 * 0.12).max(3.);
        if away {
            g.moved = true;
            self.last_click = None;
        }
        match &mut g.tool {
            Tool::Paint { value, last } => {
                let dest = l.cell(x, y);
                if let Some(c) = dest {
                    let from = last.unwrap_or(c);
                    let dx = c.0 - from.0;
                    let dy = c.1 - from.1;
                    let n = dx.abs().max(dy.abs()).max(1);
                    for i in 1..=n {
                        let t = f32::from(i) / f32::from(n);
                        let p = Cell(
                            from.0 + (dx as f32 * t).round() as i8,
                            from.1 + (dy as f32 * t).round() as i8,
                        );
                        g.draft.set_pixel_override(p, *value);
                    }
                }
                *last = dest;
            }
            Tool::Line { from, to, cursor } => {
                *cursor = (x, y);
                let end = l
                    .cell(x, y)
                    .filter(|&p| p != *from && g.original.at(p) != EMPTY);
                if end == *to {
                    return;
                }
                g.draft.links.clone_from(&g.original.links);
                if let Some(p) = end {
                    let edge = Link::between(*from, p);
                    g.draft.set_link(edge, !g.original.links.contains(&edge));
                }
                *to = end;
            }
            Tool::Aim { cell } => {
                if away {
                    g.draft.set(*cell, font::direction(dx, dy));
                }
            }
        }
    }

    fn pointer(&mut self, ctx: &mut Context, p: PointerInput) -> bool {
        let (x, y) = (p.x as i32, p.y as i32);
        let l = self.layout();
        match p.phase {
            1 => self.down(ctx, &l, x, y),
            0 if self.gesture.is_some() && p.buttons == 1 => self.drag(&l, x, y),
            0 => {
                let hover = if l.palette.contains(x, y) {
                    Some((x, y))
                } else {
                    None
                };
                if self.hover == hover {
                    return false;
                }
                self.hover = hover;
            }
            2 => {
                if self.gesture.is_none() {
                    return false;
                }
                self.drag(&l, x, y);
                let mut g = self.gesture.take().unwrap();
                if let Tool::Aim { cell } | Tool::Line { from: cell, .. } = g.tool
                    && !g.moved
                    && g.original.at(cell) != EMPTY
                {
                    let start = g.start;
                    let now = ctx.clock_read();
                    let double = self.last_click.take().is_some_and(|(prev, px, py, t)| {
                        prev == cell
                            && (px - start.0).abs() <= 4
                            && (py - start.1).abs() <= 4
                            && now.saturating_sub(t) <= 500_000_000
                    });
                    if double {
                        g.draft.set(cell, g.original.toggle_direction(cell));
                    } else {
                        self.last_click = Some((cell, start.0, start.1, now));
                    }
                }
                let mut changes = Vec::new();
                let mut pixels = Vec::new();
                let current = &self.doc.glyphs[&self.selected];
                for y in font::BOTTOM..=font::TOP {
                    for x in 0..font::WIDTH {
                        let c = Cell(x, y);
                        if g.original.at(c) != g.draft.at(c) {
                            changes.push((c, current.at(c), g.draft.at(c)));
                        }
                        let new = g.draft.pixel_overrides.get(&c).copied();
                        let old = current.pixel_overrides.get(&c).copied();
                        if new != g.original.pixel_overrides.get(&c).copied() && new != old {
                            pixels.push((c, old, new));
                        }
                    }
                }
                let lines = g
                    .original
                    .links
                    .symmetric_difference(&g.draft.links)
                    .map(|&e| (e, current.links.contains(&e), g.draft.links.contains(&e)))
                    .collect();
                self.change(ctx, changes, lines, pixels);
            }
            3 => {
                self.gesture = None;
            }
            _ => return false,
        }
        true
    }

    fn key(&mut self, ctx: &mut Context, key: u32) {
        self.last_click = None;
        if key == 5 {
            self.toggle_mode();
            return;
        }
        if self.curves && self.point.is_some() {
            self.set_point(
                ctx,
                match key {
                    1 => 6,
                    2 => 2,
                    3 => 8,
                    4 => 4,
                    _ => return,
                },
            );
        } else {
            let cols = self.layout().cols as isize;
            let d = match key {
                1 => -1,
                2 => 1,
                3 => -cols,
                4 => cols,
                _ => return,
            };
            let i = self.glyph_index(self.selected);
            let j = (i as isize + d).clamp(0, self.doc.order.len() as isize - 1) as usize;
            self.select(self.doc.order[j]);
        }
    }

    fn render(&mut self, view: &mut ViewEncoder) {
        view.window_style(WindowStyle::STANDARD | WindowStyle::THEME_BRIGHT);
        view.window_size_constraints(WindowSizeConstraints {
            min_width: 820.,
            min_height: 540.,
            ..Default::default()
        });
        view.theme(&[ThemeOverride {
            role: ThemeColor::WindowBackground,
            color: Color(0xebebebff),
        }]);
        if matches!(self.form, Some(Form::Export)) {
            let busy = self.export.busy();
            view.column_start();
            view.text(TextStyle::Title, "Export OpenType font");
            view.text(TextStyle::Body, "Choose a destination and an outline style. Repeat exports create numbered copies in a Mac folder or update the font in the Playbit volume.");
            for (index, target) in self.export.destinations.iter().enumerate() {
                if self.export.destinations.len() == 1 {
                    view.text(TextStyle::Label, &target.name);
                } else {
                    let style = if index == self.export.selected {
                        ButtonStyle::Primary
                    } else {
                        ButtonStyle::Default
                    };
                    view.button(DESTINATION + index as u32, &target.name, style, busy);
                }
            }
            let disabled = busy || !self.store.loaded || self.export.destinations.is_empty();
            view.row_start();
            view.button(
                EXPORT_PIXELS,
                "Export Pixels",
                if !self.curves {
                    ButtonStyle::Primary
                } else {
                    ButtonStyle::Default
                },
                disabled,
            );
            view.button(
                EXPORT_CURVES,
                "Export Curves",
                if self.curves {
                    ButtonStyle::Primary
                } else {
                    ButtonStyle::Default
                },
                disabled,
            );
            view.row_end();
            if !self.export.message.is_empty() {
                view.text(TextStyle::Body, &self.export.message);
            }
            if !self.export.command.is_empty() {
                view.text(
                    TextStyle::Muted,
                    "Copy this command to save the font in your current Mac folder.",
                );
                view.text_field(
                    COPY_COMMAND,
                    &mut self.export.command,
                    "",
                    false,
                    false,
                    true,
                );
            }
            view.button(CANCEL, "Done", ButtonStyle::Default, busy);
            view.column_end();
        } else if let Some(form) = self.form {
            let (title, help, placeholder, multi, action) = match form {
                Form::Export => unreachable!(),
                Form::Character => (
                    "Open or add a glyph",
                    "Enter a character or its Unicode code point. New characters start with an empty grid.",
                    "a or U+0061",
                    false,
                    "Open glyph",
                ),
                Form::Specimen => (
                    "Edit specimen",
                    "The specimen uses the current font and rendering mode.",
                    "Type a specimen",
                    true,
                    "Apply",
                ),
            };
            view.column_start();
            view.text(TextStyle::Title, title);
            view.text(TextStyle::Muted, help);
            view.text_field(FIELD, &mut self.input, placeholder, multi, false, false);
            if !self.form_error.is_empty() {
                view.text(TextStyle::Body, &self.form_error);
            }
            view.row_start();
            view.button(CANCEL, "Cancel", ButtonStyle::Default, false);
            view.button(APPLY, action, ButtonStyle::Primary, false);
            view.row_end();
            view.column_end();
        } else {
            let mut cache = std::mem::take(&mut self.cache);
            if self.curves {
                view.context_menu_start(&[
                    playbit_api::ui::MenuItem::Action {
                        id: TOGGLE_LINES,
                        label: "Lines",
                        icon: None,
                        checked: Some(self.lines),
                        disabled: false,
                        destructive: false,
                    },
                    playbit_api::ui::MenuItem::Action {
                        id: DELETE_POINT,
                        label: "Delete point",
                        icon: None,
                        checked: None,
                        disabled: self.point.is_none(),
                        destructive: true,
                    },
                ]);
            }
            render::scene(self, view, &mut cache);
            if self.curves {
                view.context_menu_end();
            }
            self.cache = cache;
        }
    }
}

#[unsafe(no_mangle)]
pub fn playbit_app() -> playbit_api::App {
    playbit_api::app(App::default(), turn, true)
}

fn turn(app: &mut App, ctx: &mut Context, view: &mut ViewEncoder) {
    let mut dirty = false;
    let status = (
        app.store.error.clone(),
        app.store.loaded,
        app.store.pending(),
    );
    for e in ctx.raw_events() {
        match e.kind {
            EventKind::Started => {
                app.store.refresh = true;
                dirty = true;
            }
            EventKind::SurfaceChanged => {
                if let Ok(s) = Surface::decode(e.data)
                    && s.width > 0
                    && s.height > 0
                {
                    app.surface = (s.width.min(4096), s.height.min(4096));
                    app.density = s.pixel_scale;
                    dirty = true;
                }
            }
            EventKind::Pointer if e.target == BOARD && app.form.is_none() => {
                if let Ok(p) = PointerInput::decode(e.data) {
                    dirty |= app.pointer(ctx, p);
                }
            }
            EventKind::Key if e.target == BOARD && app.form.is_none() => {
                if let Ok(k) = KeyInput::decode(e.data)
                    && k.pressed
                {
                    app.key(ctx, k.key);
                    dirty = true;
                }
            }
            EventKind::TextChanged if e.target == FIELD && app.form.is_some() => {
                if let Ok(input) = playbit_api::abi::text::TextInput::decode(e.data) {
                    app.input = input.text.to_owned();
                    app.form_error.clear();
                    dirty = true;
                }
            }
            EventKind::Submit if e.target == FIELD && app.form.is_some() => {
                if let Ok(input) = std::str::from_utf8(e.data) {
                    app.input = input.to_owned();
                    app.apply_form(ctx);
                    dirty = true;
                }
            }
            EventKind::Click
                if matches!(app.form, Some(Form::Export))
                    && matches!(e.target, EXPORT_PIXELS | EXPORT_CURVES) =>
            {
                if app.store.loaded {
                    app.export
                        .start(ctx, &app.doc.glyphs, e.target == EXPORT_CURVES);
                }
                dirty = true;
            }
            EventKind::Click
                if matches!(app.form, Some(Form::Export))
                    && e.target >= DESTINATION
                    && !app.export.busy() =>
            {
                let i = (e.target - DESTINATION) as usize;
                if i < app.export.destinations.len() {
                    app.export.selected = i;
                    dirty = true;
                }
            }
            EventKind::Click if e.target == TOGGLE_LINES && app.curves && app.form.is_none() => {
                app.lines = !app.lines;
                app.point = None;
                app.gesture = None;
                app.last_click = None;
                dirty = true;
            }
            EventKind::Click if e.target == DELETE_POINT && app.curves && app.form.is_none() => {
                app.delete_selected(ctx);
                dirty = true;
            }
            EventKind::Click if e.target == APPLY => {
                app.apply_form(ctx);
                dirty = true;
            }
            EventKind::Click if e.target == CANCEL && !app.export.busy() => {
                app.form = None;
                dirty = true;
            }
            EventKind::DataChanged => app.store.refresh = true,
            EventKind::DataReady => {
                if let Ok(op) = e.operation() {
                    dirty |= app.export.ready(ctx, op);
                    dirty |= app.store.ready(ctx, &mut app.doc, op);
                }
            }
            EventKind::Wake => app.store.retry_wait = false,
            _ => {}
        }
    }
    app.store.service(ctx);
    if dirty
        || status
            != (
                app.store.error.clone(),
                app.store.loaded,
                app.store.pending(),
            )
    {
        app.render(view);
        view.submit(ctx).expect("submit Dot font scene");
    }
}
