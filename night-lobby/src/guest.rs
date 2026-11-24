use crate::pixels::{Ink, Pixels, iso};
use crate::plaza::Place;
use std::collections::VecDeque;

#[path = "avatar_art.rs"]
mod avatar_art;

pub type Id = [u8; 16];
pub type Cell = (i32, i32);
pub const STEP_MS: u64 = 350;
pub const LOOKS: [(&str, Ink, Ink); 6] = [
    ("Marigold", 0xf0b054, 0xba7644),
    ("Lagoon", 0x77bcb2, 0x428481),
    ("Persimmon", 0xe3836b, 0xb85852),
    ("Wisteria", 0xaf98cd, 0x776993),
    ("Juniper", 0x8baa75, 0x5c805d),
    ("Sky", 0x8daedd, 0x607da9),
];

#[derive(Clone)]
pub struct Guest {
    pub account: Id,
    pub place: Place,
    pub sleeping: bool,
    pub look: u8,
    pub face: u8,
    pub cell: Cell,
    pub from: (f32, f32),
    pub started: u64,
    pub direction: u8,
    pub wave_until: u64,
    pub bubble: Option<(String, u64)>,
}

impl Guest {
    pub fn new(id: Id) -> Self {
        let spots = [
            (4, 5),
            (5, 4),
            (6, 5),
            (3, 5),
            (6, 4),
            (4, 6),
            (3, 6),
            (6, 6),
        ];
        let seed = id.into_iter().fold(2166136261_u32, |s, b| {
            (s ^ u32::from(b)).wrapping_mul(16777619)
        });
        let [_, _, style, face] = seed.to_le_bytes();
        let cell = spots[(seed as usize >> 15) % spots.len()];
        Self {
            account: id,
            place: Place::Bar,
            sleeping: false,
            look: style % 6,
            face,
            cell,
            from: (cell.0 as f32, cell.1 as f32),
            started: 0,
            direction: 0,
            wave_until: 0,
            bubble: None,
        }
    }
    pub fn checkpoint(&self) -> Vec<u8> {
        vec![
            1,
            self.look,
            self.face,
            self.cell.0 as u8,
            self.cell.1 as u8,
            self.direction,
        ]
    }
    pub fn restore(account: Id, bytes: &[u8]) -> Option<Self> {
        Self::restore_in(account, bytes, Place::Bar)
    }
    pub fn restore_in(account: Id, bytes: &[u8], place: Place) -> Option<Self> {
        let &[1, look, face, u, v, direction] = bytes else {
            return None;
        };
        let cell = (i32::from(u), i32::from(v));
        if usize::from(look) >= LOOKS.len() || direction > 3 || !place.walkable(cell) {
            return None;
        }
        let mut guest = Self::new(account);
        guest.place = place;
        guest.look = look;
        guest.face = face;
        guest.cell = cell;
        guest.from = (cell.0 as f32, cell.1 as f32);
        guest.direction = direction;
        guest.sleeping = true;
        Some(guest)
    }
    pub fn stand(&mut self, cell: Cell) {
        self.cell = cell;
        self.from = (cell.0 as f32, cell.1 as f32);
        self.bubble = None;
        self.wave_until = 0;
    }
    pub fn follow(&mut self, other: &Guest, now: u64) {
        self.go(other.cell, now);
        self.direction = other.direction;
        self.look = other.look;
        self.face = other.face;
    }
    pub fn position(&self, now: u64) -> (f32, f32) {
        let t = (now.saturating_sub(self.started) as f32 / STEP_MS as f32).min(1.);
        (
            self.from.0 + (self.cell.0 as f32 - self.from.0) * t,
            self.from.1 + (self.cell.1 as f32 - self.from.1) * t,
        )
    }
    pub fn walking(&self, now: u64) -> bool {
        self.from != (self.cell.0 as f32, self.cell.1 as f32)
            && now.saturating_sub(self.started) < STEP_MS
    }
    pub fn go(&mut self, cell: Cell, now: u64) {
        if self.cell == cell {
            return;
        }
        self.from = self.position(now);
        let (dx, dy) = (cell.0 - self.cell.0, cell.1 - self.cell.1);
        self.direction = if dx > 0 {
            0
        } else if dy > 0 {
            1
        } else if dx < 0 {
            2
        } else {
            3
        };
        self.cell = cell;
        self.started = now;
    }
    pub fn foot(&self, now: u64) -> (i32, i32) {
        let (u, v) = self.position(now);
        let sitting = self.place.seat(self.cell) && !self.walking(now);
        iso(
            (u * 16.).round() as i32 + 8,
            (v * 16.).round() as i32 + 8,
            if sitting { 6 } else { 0 },
        )
    }
    pub fn draw(&self, p: &mut Pixels, now: u64, local: bool) {
        let (x, y) = self.foot(now);
        if self.sleeping {
            p.half_opacity(x - 20, y - 47, 41, 52, |p| {
                avatar_art::paint(self, p, now, local)
            });
        } else {
            avatar_art::paint(self, p, now, local);
        }
    }
}

pub fn path(place: Place, from: Cell, to: Cell) -> VecDeque<Cell> {
    if !place.walkable(to) || from == to {
        return VecDeque::new();
    }
    let mut parents = [[None; 9]; 9];
    let mut queue = VecDeque::from([from]);
    parents[from.0 as usize][from.1 as usize] = Some(from);
    while let Some(cell) = queue.pop_front() {
        for (dx, dy) in [(1, 0), (0, 1), (-1, 0), (0, -1)] {
            let next = (cell.0 + dx, cell.1 + dy);
            if !place.walkable(next) || ((place.seat(next) || next == place.door()) && next != to) {
                continue;
            }
            let parent = &mut parents[next.0 as usize][next.1 as usize];
            if parent.is_some() {
                continue;
            }
            *parent = Some(cell);
            if next == to {
                let mut route = VecDeque::new();
                let mut step = to;
                while step != from {
                    route.push_front(step);
                    step = parents[step.0 as usize][step.1 as usize].unwrap();
                }
                return route;
            }
            queue.push_back(next);
        }
    }
    VecDeque::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pixels::{H, W};
    #[test]
    fn sleeping_sprite_blends_each_pixel_once_over_the_room() {
        let guest = Guest::restore([1; 16], &[1, 3, 75, 5, 6, 1]).unwrap();
        for (w, h) in [(W, H), (800, 560)] {
            let mut background = Pixels::new(crate::pixels::Bounds::centered(w, h));
            crate::glasshouse::background(&mut background);
            let mut sleeping = background.clone();
            guest.draw(&mut sleeping, 0, false);
            let mut opaque = background.clone();
            avatar_art::paint(&guest, &mut opaque, 0, false);
            assert_ne!(opaque.rgba, background.rgba);
            for ((a, b), s) in opaque.rgba.iter().zip(&background.rgba).zip(sleeping.rgba) {
                assert_eq!(u16::from(s), (u16::from(*a) + u16::from(*b)) / 2);
            }
        }
    }
}
