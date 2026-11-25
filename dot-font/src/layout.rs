use crate::font::{BOTTOM, Cell, TOP, WIDTH};

#[derive(Clone, Copy, Default)]
pub struct Box2 {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Box2 {
    pub fn new(x: i32, y: i32, w: i32, h: i32) -> Self {
        Self { x, y, w, h }
    }
    pub fn right(self) -> i32 {
        self.x + self.w
    }
    pub fn bottom(self) -> i32 {
        self.y + self.h
    }
    pub fn inset(self, n: i32) -> Self {
        Self::new(self.x + n, self.y + n, self.w - 2 * n, self.h - 2 * n)
    }
    pub fn contains(self, x: i32, y: i32) -> bool {
        x >= self.x && x < self.right() && y >= self.y && y < self.bottom()
    }
}

pub struct Layout {
    pub sample: Box2,
    pub size: Box2,
    pub palette: Box2,
    pub add: Box2,
    pub prev: Box2,
    pub next: Box2,
    pub tile: i32,
    pub cols: usize,
    pub count: usize,
    pub editor: Box2,
    pub grid: Box2,
    pub unit: i32,
    pub left: i32,
    pub base: i32,
    pub jump: Box2,
    pub mode: Box2,
    pub undo: Box2,
    pub redo: Box2,
    pub auto: Box2,
    pub erase: Box2,
    pub lines: Box2,
    pub export: Box2,
}

impl Layout {
    pub fn new(w: i32, h: i32) -> Self {
        let side = (w * 35 / 100).clamp(180, 390);
        let sample = Box2::new(0, 0, side, (h * 36 / 100).max(154));
        let size = Box2::new(side - 62, 8, 52, 25);
        let palette = Box2::new(0, sample.bottom() + 8, side, h - sample.h - 8);
        let cols = ((side - 18) / 32).clamp(4, 10) as usize;
        let tile = (side - 18) / cols as i32;
        let count = cols * ((palette.h - 77) / tile).clamp(1, 12) as usize;
        let add = Box2::new(10, h - 34, 26, 25);
        let prev = Box2::new(side - 63, h - 34, 25, 25);
        let next = Box2::new(side - 36, h - 34, 25, 25);
        let editor = Box2::new(side + 8, 0, w - side - 8, h);
        let grid = Box2::new(editor.x + 4, 4, editor.w - 8, h - 50);
        let unit = ((grid.h - 32) / 15).min((grid.w - 20) / 11).max(1);
        let left = grid.x + (grid.w - WIDTH as i32 * unit) / 2;
        let base = grid.y + (grid.h + 8 * unit) / 2;
        let jump = Box2::new(editor.x + 10, h - 35, 74, 25);
        let mode = Box2::new(editor.right() - 65, h - 36, 55, 27);
        let redo = Box2::new(grid.right() - 31, grid.y + 7, 24, 23);
        let undo = Box2::new(redo.x - 26, redo.y, 24, 23);
        let lines = Box2::new(grid.x + 8, grid.y + 7, 50, 23);
        let auto = Box2::new(lines.right() + 4, grid.y + 7, 42, 23);
        let export = Box2::new(size.x - 66, size.y, 62, size.h);
        let erase = Box2::new(auto.right() + 4, auto.y, 45, 23);
        Self {
            sample,
            size,
            palette,
            add,
            prev,
            next,
            tile,
            cols,
            count,
            editor,
            grid,
            unit,
            left,
            base,
            jump,
            mode,
            undo,
            redo,
            auto,
            erase,
            lines,
            export,
        }
    }

    pub fn glyph_tile(&self, index: usize) -> Box2 {
        Box2::new(
            9 + (index % self.cols) as i32 * self.tile,
            self.palette.y + 37 + (index / self.cols) as i32 * self.tile,
            self.tile - 2,
            self.tile - 2,
        )
    }

    pub fn cell(&self, x: i32, y: i32) -> Option<Cell> {
        if !self.grid.contains(x, y) {
            return None;
        }
        let col = (x - self.left).div_euclid(self.unit);
        let row = (self.base - 1 - y).div_euclid(self.unit);
        ((0..i32::from(WIDTH)).contains(&col)
            && (i32::from(BOTTOM)..=i32::from(TOP)).contains(&row))
        .then_some(Cell(col as i8, row as i8))
    }

    pub fn center(&self, cell: Cell) -> (f32, f32) {
        let (x, y) = cell.center();
        (
            self.left as f32 + x * self.unit as f32,
            self.base as f32 + y * self.unit as f32,
        )
    }
}
