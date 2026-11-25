use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use tiny_skia::{LineCap, Path, PathBuilder, Rect, Stroke};

pub const WIDTH: i8 = 7;
pub const BOTTOM: i8 = -3;
pub const TOP: i8 = 10;
pub const EMPTY: u8 = 0;
pub const AUTO: u8 = 1;
// 10 disables an automatic tangent; 11–18 retain a disabled compass bearing.
pub const NO_DIRECTION: u8 = 10;
pub const MAX_VALUE: u8 = 18;

// Coordinates match the source font: y increases above the baseline.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Cell(pub i8, pub i8);

impl Cell {
    pub fn valid(self) -> bool {
        (0..WIDTH).contains(&self.0) && (BOTTOM..=TOP).contains(&self.1)
    }

    pub fn center(self) -> (f32, f32) {
        (self.0 as f32 + 0.5, -self.1 as f32 - 0.5)
    }

    fn offset(self, dx: i8, dy: i8) -> Cell {
        Cell(self.0 + dx, self.1 + dy)
    }
}

// Values 2 through 9 store an explicit compass direction, clockwise from east.
pub const COMPASS: [(i8, i8); 8] = [
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
];

pub fn direction(dx: f32, dy_screen: f32) -> u8 {
    let eighths = dy_screen.atan2(dx) / std::f32::consts::FRAC_PI_4;
    2 + (eighths.round() as i32).rem_euclid(8) as u8
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Link(pub Cell, pub Cell);

impl Link {
    pub fn between(a: Cell, b: Cell) -> Self {
        if a < b { Self(a, b) } else { Self(b, a) }
    }
    pub fn valid(self) -> bool {
        self.0.valid() && self.1.valid() && self.0 < self.1
    }
    fn spans_cells(self) -> bool {
        let Link(a, b) = self;
        (b.0 - a.0).abs().max((b.1 - a.1).abs()) > 1
    }
    pub fn other(self, c: Cell) -> Option<Cell> {
        if self.0 == c {
            Some(self.1)
        } else if self.1 == c {
            Some(self.0)
        } else {
            None
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Glyph {
    cells: [u8; (WIDTH * (TOP - BOTTOM + 1)) as usize],
    pub links: BTreeSet<Link>,
}

impl Default for Glyph {
    fn default() -> Self {
        Self {
            cells: [EMPTY; (WIDTH * (TOP - BOTTOM + 1)) as usize],
            links: BTreeSet::new(),
        }
    }
}

impl Glyph {
    pub fn at(&self, c: Cell) -> u8 {
        if c.valid() {
            self.cells[((c.1 - BOTTOM) * WIDTH + c.0) as usize]
        } else {
            EMPTY
        }
    }

    pub fn set(&mut self, c: Cell, value: u8) {
        self.cells[((c.1 - BOTTOM) * WIDTH + c.0) as usize] = value;
    }

    pub fn points(&self) -> impl Iterator<Item = (Cell, u8)> + '_ {
        self.cells
            .iter()
            .enumerate()
            .filter(|(_, v)| **v != EMPTY)
            .map(|(i, v)| (Cell(i as i8 % WIDTH, BOTTOM + i as i8 / WIDTH), *v))
    }

    pub fn set_link(&mut self, link: Link, connected: bool) {
        if connected {
            self.links.insert(link);
        } else {
            self.links.remove(&link);
        }
    }

    pub fn erase(&mut self, c: Cell) {
        self.set(c, EMPTY);
        self.links.retain(|e| e.other(c).is_none());
    }

    /// Delete an anchor, joining the two sides of a stroke. At a junction,
    /// pair the straightest continuations first and leave unmatched arms open.
    pub fn delete_point(&mut self, c: Cell) -> Option<Cell> {
        let mut ends: Vec<_> = self.neighbors(c).map(|(x, y)| c.offset(x, y)).collect();
        self.erase(c);
        match ends.as_slice() {
            [next] => return self.neighbors(*next).next().map(|_| *next),
            [a, b] => {
                self.set_link(Link::between(*a, *b), true);
                return None;
            }
            _ => {}
        }
        let bearing = |p: Cell| {
            let (x, y) = (f32::from(p.0 - c.0), f32::from(p.1 - c.1));
            let len = x.hypot(y);
            (x / len, y / len)
        };
        while ends.len() >= 2 {
            let mut best = None;
            let mut best_dot = 0.;
            for (i, &a) in ends.iter().enumerate() {
                let (ax, ay) = bearing(a);
                for (j, &b) in ends.iter().enumerate().skip(i + 1) {
                    let (bx, by) = bearing(b);
                    let dot = ax * bx + ay * by;
                    if dot < best_dot {
                        best_dot = dot;
                        best = Some((i, j));
                    }
                }
            }
            let Some((i, j)) = best else { break };
            let b = ends.remove(j);
            self.set_link(Link::between(ends.remove(i), b), true);
        }
        None
    }

    // Two steep legs leaving a common tip should take separate columns on
    // the very next row. Use the same rule transposed for a sideways tip.
    fn is_tip(&self, c: Cell, dx: i8, dy: i8, steep: bool) -> bool {
        let mut arms = self.neighbors(c);
        let (Some(a), Some(b), None) = (arms.next(), arms.next(), arms.next()) else {
            return false;
        };
        let (other_x, other_y) = if a == (dx, dy) { b } else { a };
        let (major, minor, other_major, other_minor) = if steep {
            (dy, dx, other_y, other_x)
        } else {
            (dx, dy, other_x, other_y)
        };
        major.signum() == other_major.signum()
            && minor.signum() == -other_minor.signum()
            && minor != 0
            && other_major.abs() > other_minor.abs()
    }

    fn line_pixels(&self, Link(a, b): Link) -> Vec<Cell> {
        let (dx, dy) = (b.0 - a.0, b.1 - a.1);
        let steep = dy.abs() > dx.abs();
        let (major, minor) = if steep { (dy, dx) } else { (dx, dy) };
        let n = i16::from(major.abs());
        if n <= 1 {
            return vec![a, b];
        }
        // An outward rounding bias preserves a sharp tip at small sizes.
        // All other slopes take the nearest cell along their dominant axis.
        let bias = match (
            self.is_tip(a, dx, dy, steep),
            self.is_tip(b, -dx, -dy, steep),
        ) {
            (true, false) => n - 1,
            (false, true) => 0,
            _ => n / 2,
        };
        (0..=n)
            .map(|i| {
                let u = i as i8 * major.signum();
                let v = ((i * i16::from(minor.abs()) + bias) / n) as i8 * minor.signum();
                if steep {
                    a.offset(v, u)
                } else {
                    a.offset(u, v)
                }
            })
            .collect()
    }

    pub fn pixels(&self) -> BTreeSet<Cell> {
        let mut pixels: BTreeSet<_> = self.points().map(|(c, _)| c).collect();
        for &link in &self.links {
            if link.spans_cells() && self.linked(link.0, link.1) {
                pixels.extend(self.line_pixels(link));
            }
        }
        pixels
    }

    // Pixel erasing cuts the shared geometry. Materialize just the affected
    // long strokes so the unpainted cell stays empty in the derived bitmap.
    pub fn erase_pixel(&mut self, c: Cell) {
        let mut long_strokes: Vec<_> = self
            .links
            .iter()
            .copied()
            .filter(|link| link.spans_cells() && self.linked(link.0, link.1))
            .map(|link| (link, self.line_pixels(link)))
            .collect();
        self.erase(c);
        // A cut at a shared tip can change how its other leg rounds. Keep
        // that leg's previous ink too. Each pass consumes one long stroke.
        while let Some(i) = long_strokes
            .iter()
            .position(|(link, cells)| cells.contains(&c) || self.line_pixels(*link) != *cells)
        {
            let (link, cells) = long_strokes.swap_remove(i);
            self.set_link(link, false);
            for run in cells.split(|p| *p == c) {
                for &p in run {
                    if self.at(p) == EMPTY {
                        self.set(p, NO_DIRECTION);
                    }
                }
                for pair in run.windows(2) {
                    self.set_link(Link::between(pair[0], pair[1]), true);
                }
            }
        }
    }

    // Import legacy point-only bitmaps once. Subsequent editing uses these
    // explicit edges, including the choice to leave two nearby dots apart.
    pub fn import_links(&mut self, ch: char) {
        let mut links = BTreeSet::new();
        for (c, _) in self.points() {
            for (dx, dy) in COMPASS {
                let end = c.offset(dx, dy);
                if c < end
                    && self.at(end) != EMPTY
                    && (dx == 0
                        || dy == 0
                        || (self.at(c.offset(dx, 0)) == EMPTY && self.at(c.offset(0, dy)) == EMPTY))
                {
                    links.insert(Link(c, end));
                }
            }
        }
        // The DM lowercase s ends at the upper-right dot beside its shoulder.
        if ch == 's' {
            links.remove(&Link::between(Cell(5, 4), Cell(4, 3)));
        }
        self.links = links;
    }

    fn linked(&self, a: Cell, b: Cell) -> bool {
        self.at(a) != EMPTY && self.at(b) != EMPTY && self.links.contains(&Link::between(a, b))
    }

    fn neighbors(&self, c: Cell) -> impl Iterator<Item = (i8, i8)> + '_ {
        self.links.iter().filter_map(move |link| {
            let end = link.other(c)?;
            (self.at(end) != EMPTY).then_some((end.0 - c.0, end.1 - c.1))
        })
    }

    // A diagonal step on a bitmap curve usually joins the ends of two runs.
    // Extend the tangent of each run into the gap to make a quarter turn.
    fn auto_handle(&self, c: Cell, dx: i8, dy: i8) -> (i8, i8) {
        if dx.abs() == 1 && dy.abs() == 1 {
            let horizontal = self.linked(c, c.offset(-dx, 0));
            let vertical = self.linked(c, c.offset(0, -dy));
            // A one-cell spur into a perpendicular stem ends the bowl of
            // letters such as “a”. Follow the stem at this end of the diagonal.
            let end = c.offset(dx, dy);
            let hstem = c.offset(-dx, 0);
            let vstem = c.offset(0, -dy);
            if horizontal
                && !vertical
                && self.linked(end, end.offset(dx, 0))
                && self.linked(hstem, c.offset(-dx, 1))
                && self.linked(hstem, c.offset(-dx, -1))
            {
                return (0, dy);
            }
            if vertical
                && !horizontal
                && self.linked(end, end.offset(0, dy))
                && self.linked(vstem, c.offset(1, -dy))
                && self.linked(vstem, c.offset(-1, -dy))
            {
                return (dx, 0);
            }
            if !horizontal && !vertical {
                if self.linked(c, c.offset(dx, -dy)) || self.linked(end, end.offset(dx, 0)) {
                    return (0, dy);
                }
                if self.linked(c, c.offset(-dx, dy)) || self.linked(end, end.offset(0, dy)) {
                    return (dx, 0);
                }
            }
            match (horizontal, vertical) {
                (true, false) => return (dx, 0),
                (false, true) => return (0, dy),
                _ => {}
            }
        }
        (dx, dy)
    }

    pub fn natural_direction(&self, c: Cell) -> (i8, i8) {
        if let Some((dx, dy)) = self.neighbors(c).find(|(dx, dy)| *dx != 0 && *dy != 0) {
            self.auto_handle(c, dx, dy)
        } else {
            self.neighbors(c).next().unwrap_or((1, 0))
        }
    }

    pub fn show_handle(&self, c: Cell) -> bool {
        (2..=9).contains(&self.at(c))
            || (self.at(c) == AUTO && self.neighbors(c).any(|(dx, dy)| dx != 0 && dy != 0))
    }

    pub fn tangent(&self, c: Cell) -> (i8, i8) {
        match self.at(c) {
            2..=9 => COMPASS[(self.at(c) - 2) as usize],
            _ => self.natural_direction(c),
        }
    }

    pub fn toggle_direction(&self, c: Cell) -> u8 {
        let v = self.at(c);
        match v {
            2..=9 => v + 9,
            11..=18 => v - 9,
            AUTO if self.show_handle(c) => NO_DIRECTION,
            NO_DIRECTION if self.neighbors(c).any(|(x, y)| x != 0 && y != 0) => AUTO,
            _ => {
                let (dx, dy) = self.natural_direction(c);
                direction(dx as f32, -(dy as f32))
            }
        }
    }

    fn handle(&self, c: Cell, dx: i8, dy: i8) -> (f32, f32) {
        let (mut x, mut y) = self.auto_handle(c, dx, dy);
        if self.at(c) >= NO_DIRECTION {
            return (0., 0.);
        }
        if (2..=9).contains(&self.at(c)) {
            let (ax, ay) = self.natural_direction(c);
            let sign = if i32::from(x) * i32::from(ax) + i32::from(y) * i32::from(ay) < 0 {
                -1
            } else {
                1
            };
            (x, y) = self.tangent(c);
            x *= sign;
            y *= sign;
        }
        let length = if dx != 0 && dy != 0 { 0.552285 } else { 0.45 };
        let span = f32::from(dx.abs().max(dy.abs()));
        let scale = length * span / f32::from(x).hypot(f32::from(y));
        (f32::from(x) * scale, -f32::from(y) * scale)
    }

    pub fn outlines(&self) -> (Option<Path>, Option<Path>) {
        let mut squares = PathBuilder::new();
        let mut edges = PathBuilder::new();
        let mut outline = PathBuilder::new();
        let pixels = self.pixels();
        for &c in &pixels {
            let (cx, cy) = c.center();
            if !pixels.contains(&c.offset(-1, 0)) {
                let mut width = 1;
                while pixels.contains(&c.offset(width, 0)) {
                    width += 1;
                }
                squares.push_rect(Rect::from_xywh(cx - 0.5, cy - 0.5, width as f32, 1.).unwrap());
            }
        }
        for (c, _) in self.points() {
            let (cx, cy) = c.center();
            if self.neighbors(c).next().is_none() {
                outline.push_circle(cx, cy, 0.5);
            }
            for (dx, dy) in self.neighbors(c) {
                let end = c.offset(dx, dy);
                if end <= c {
                    continue;
                }
                let (ex, ey) = end.center();
                let a = self.handle(c, dx, dy);
                let b = self.handle(end, -dx, -dy);
                edges.move_to(cx, cy);
                edges.cubic_to(cx + a.0, cy + a.1, ex + b.0, ey + b.1, ex, ey);
            }
        }
        let stroke = Stroke {
            width: 1.,
            line_cap: LineCap::Round,
            ..Stroke::default()
        };
        if let Some(path) = edges.finish().and_then(|p| p.stroke(&stroke, 100.)) {
            outline.push_path(&path);
        }
        (squares.finish(), outline.finish())
    }
}

#[derive(Deserialize)]
struct SourceGlyph {
    px: Vec<(i8, i8)>,
}

pub fn seed() -> BTreeMap<char, Glyph> {
    let source: BTreeMap<char, SourceGlyph> =
        serde_json::from_slice(include_bytes!("../assets/dm.json")).expect("bundled DM font");
    source
        .into_iter()
        .map(|(ch, source)| {
            let mut g = Glyph::default();
            for (y, x) in source.px {
                let c = Cell(x, y);
                assert!(c.valid());
                g.set(c, AUTO);
            }
            g.import_links(ch);
            (ch, g)
        })
        .collect()
}

pub fn alphabet(font: &BTreeMap<char, Glyph>) -> Vec<char> {
    ('a'..='z')
        .chain('A'..='Z')
        .chain('0'..='9')
        .chain(
            font.keys()
                .copied()
                .filter(|ch| !ch.is_ascii_alphanumeric()),
        )
        .collect()
}

pub fn codepoint(ch: char) -> String {
    format!("U+{:04X}", ch as u32)
}

pub fn parse_character(input: &str) -> Option<char> {
    let mut chars = input.chars();
    if let (Some(ch), None) = (chars.next(), chars.next()) {
        return (!ch.is_control()).then_some(ch);
    }
    let s = input.trim();
    let hex = s.strip_prefix("U+").or_else(|| s.strip_prefix("u+"))?;
    if !(1..=6).contains(&hex.len()) {
        return None;
    }
    char::from_u32(u32::from_str_radix(hex, 16).ok()?).filter(|ch| !ch.is_control())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stroke(points: &[Cell]) -> Glyph {
        let mut g = Glyph::default();
        for &p in points {
            g.set(p, NO_DIRECTION);
        }
        for pair in points.windows(2) {
            g.set_link(Link::between(pair[0], pair[1]), true);
        }
        g
    }

    #[test]
    fn delete_splices_the_stroke_and_advances_only_along_connected_endpoints() {
        let [a, b, c, d] = [Cell(1, 2), Cell(2, 2), Cell(3, 2), Cell(4, 2)];
        let mut g = stroke(&[a, b, c, d]);
        g.set(Cell(1, 3), AUTO); // Spatially closer than c, on another stroke.
        let pixels = g.pixels();
        assert_eq!(g.delete_point(b), None);
        assert_eq!(g.at(b), EMPTY);
        assert!(g.linked(a, c));
        assert_eq!(g.pixels(), pixels);
        assert_eq!(g.delete_point(a), Some(c));
        assert_eq!(g.delete_point(c), None);
        assert_ne!(g.at(d), EMPTY);
        assert!(g.links.is_empty());
    }

    #[test]
    fn deleting_a_junction_preserves_crossing_strokes() {
        let c = Cell(3, 2);
        let a = Cell(1, 2);
        let b = Cell(4, 2);
        let top = Cell(3, 4);
        let bottom = Cell(3, 0);
        let mut g = stroke(&[a, c, b]);
        for p in [top, bottom] {
            g.set(p, AUTO);
            g.set_link(Link::between(c, p), true);
        }
        assert_eq!(g.delete_point(c), None);
        assert_eq!(
            g.links,
            [Link::between(a, b), Link::between(top, bottom)].into()
        );
    }

    #[test]
    fn three_anchor_v_matches_the_hand_drawn_dm_bitmap() {
        let tip = Cell(3, 0);
        let mut v = stroke(&[Cell(1, 5), tip, Cell(5, 5)]);
        let pixels = v.pixels();
        assert_eq!(pixels, seed()[&'v'].points().map(|(c, _)| c).collect());
        assert_eq!(v.points().count(), 3);
        assert_eq!(v.neighbors(tip).count(), 2);
        let cut = Cell(2, 1);
        assert_eq!(v.at(cut), EMPTY);
        v.erase_pixel(cut);
        let mut remaining = pixels;
        remaining.remove(&cut);
        assert_eq!(v.pixels(), remaining);
        assert_eq!(v.neighbors(tip).count(), 1);
    }

    #[test]
    fn tangents_round_the_bowl_without_changing_its_pixels() {
        let mut glyph = seed().remove(&'a').unwrap();
        assert_eq!(glyph.points().count(), 16);
        assert_eq!(glyph.tangent(Cell(4, 5)), (1, 0));
        assert_eq!(glyph.tangent(Cell(5, 4)), (0, 1));
        assert_eq!(glyph.tangent(Cell(4, 1)), (0, -1));
        let (pixels, curves) = glyph.outlines();
        glyph.set(Cell(5, 4), direction(1., -1.));
        let (edited_pixels, edited_curves) = glyph.outlines();
        assert_eq!(pixels, edited_pixels);
        assert_ne!(curves, edited_curves);
        glyph.set(Cell(5, 4), AUTO);
        assert_eq!(glyph.outlines().1, curves);
    }

    #[test]
    fn diagonal_connections_skip_filled_corners() {
        let mut glyph = Glyph::default();
        let a = Cell(1, 1);
        glyph.set(a, AUTO);
        glyph.set(Cell(2, 2), AUTO);
        glyph.import_links('x');
        assert_eq!(glyph.neighbors(a).collect::<Vec<_>>(), [(1, 1)]);
        glyph.set(Cell(2, 1), AUTO);
        glyph.import_links('x');
        assert_eq!(glyph.neighbors(a).collect::<Vec<_>>(), [(1, 0)]);
    }
    #[test]
    fn terminals_and_disconnected_points_survive_direction_edits() {
        let mut font = seed();
        let s = font.get_mut(&'s').unwrap();
        let tip = Cell(5, 4);
        let shoulder = Cell(4, 3);
        assert_eq!(s.neighbors(tip).count(), 1);
        assert!(!s.linked(tip, shoulder));
        let link = Link::between(tip, shoulder);
        let before = s.outlines();
        s.set_link(link, true);
        assert_ne!(s.outlines().1, before.1);
        s.set_link(link, false);
        assert_eq!(s.outlines(), before);
        s.set(tip, 9);
        s.set(tip, s.toggle_direction(tip));
        assert!(!s.show_handle(tip));
        assert_eq!(s.handle(tip, -1, 1), (0., 0.));
        s.set(tip, s.toggle_direction(tip));
        assert_eq!(s.at(tip), 9);
        let mut isolated = Glyph::default();
        for c in [Cell(1, 1), Cell(2, 1), Cell(6, 10)] {
            isolated.set(c, NO_DIRECTION);
        }
        assert_eq!(isolated.neighbors(Cell(1, 1)).count(), 0);
        isolated.set_link(Link::between(Cell(1, 1), Cell(6, 10)), true);
        assert!(isolated.outlines().1.is_some());
        isolated.erase(Cell(6, 10));
        assert!(isolated.links.is_empty());
    }
}
