pub const W: i32 = 640;
pub const H: i32 = 448;
pub type Point = (i32, i32);
pub type Ink = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bounds {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Bounds {
    pub fn contains(self, x: i32, y: i32) -> bool {
        (self.x..self.x + self.w).contains(&x) && (self.y..self.y + self.h).contains(&y)
    }
    pub fn intersects(self, other: Self) -> bool {
        self.x < other.x + other.w
            && other.x < self.x + self.w
            && self.y < other.y + other.h
            && other.y < self.y + self.h
    }
    pub fn centered(w: i32, h: i32) -> Self {
        Self {
            x: (W - w) / 2,
            y: (H - h) / 2,
            w,
            h,
        }
    }
}

#[derive(Clone)]
pub struct Pixels {
    pub rgba: Vec<u8>,
    pub bounds: Bounds,
    pub sample: i32,
    stride: i32,
}

impl Pixels {
    #[cfg(test)]
    pub fn blank() -> Self {
        Self::new(Bounds::centered(W, H))
    }

    pub fn new(bounds: Bounds) -> Self {
        Self::sampled(bounds, 1)
    }

    pub fn sampled(bounds: Bounds, sample: i32) -> Self {
        debug_assert!(bounds.w % sample == 0 && bounds.h % sample == 0);
        let stride = bounds.w / sample;
        Self {
            rgba: vec![0; (stride * (bounds.h / sample) * 4) as usize],
            bounds,
            sample,
            stride,
        }
    }

    fn index(&self, x: i32, y: i32) -> Option<usize> {
        if !self.bounds.contains(x, y) {
            return None;
        }
        let (x, y) = (x - self.bounds.x, y - self.bounds.y);
        let s = self.sample;
        if s == 1 {
            return Some(((y * self.stride + x) * 4) as usize);
        }
        if x % s != s / 2 || y % s != s / 2 {
            return None;
        }
        Some(((y / s * self.stride + x / s) * 4) as usize)
    }

    pub fn dot(&mut self, x: i32, y: i32, color: Ink) {
        if let Some(i) = self.index(x, y) {
            self.rgba[i..i + 4].copy_from_slice(&((color << 8) | 255).to_be_bytes());
        }
    }

    pub fn tint(&mut self, x: i32, y: i32, color: Ink, amount: i32, span: i32) {
        let Some(i) = self.index(x, y) else { return };
        let original = u32::from_be_bytes([0, self.rgba[i], self.rgba[i + 1], self.rgba[i + 2]]);
        let color = mix(original, color, amount, span);
        self.rgba[i..i + 4].copy_from_slice(&((color << 8) | 255).to_be_bytes());
    }

    // Map a half-open world interval onto the texels whose centers it covers.
    fn extent(&self, from: i32, length: i32, origin: i32, span: i32) -> std::ops::Range<i32> {
        let edge = |p: i32| {
            let p = (p - origin).clamp(0, span);
            if self.sample == 1 {
                p
            } else {
                (p + (self.sample - 1) / 2) / self.sample
            }
        };
        edge(from)..edge(from + length)
    }

    pub fn rect(&mut self, x: i32, y: i32, w: i32, h: i32, color: Ink) {
        let columns = self.extent(x, w, self.bounds.x, self.bounds.w);
        let rows = self.extent(y, h, self.bounds.y, self.bounds.h);
        let (left, right) = (columns.start, columns.end);
        if left >= right {
            return;
        }
        let pixel = ((color << 8) | 255).to_be_bytes();
        for row in rows {
            let start = ((row * self.stride + left) * 4) as usize;
            for slot in self.rgba[start..start + ((right - left) * 4) as usize].chunks_exact_mut(4)
            {
                slot.copy_from_slice(&pixel);
            }
        }
    }

    // Composite a whole sprite once so overlapping strokes share one opacity.
    pub fn half_opacity(&mut self, x: i32, y: i32, w: i32, h: i32, draw: impl FnOnce(&mut Self)) {
        let columns = self.extent(x, w, self.bounds.x, self.bounds.w);
        let rows = self.extent(y, h, self.bounds.y, self.bounds.h);
        let stride = self.stride;
        let range = |row| {
            ((row * stride + columns.start) * 4) as usize
                ..((row * stride + columns.end) * 4) as usize
        };
        let before: Vec<u8> = rows
            .clone()
            .flat_map(|row| self.rgba[range(row)].iter().copied())
            .collect();
        draw(self);
        let mut original = before.into_iter();
        for row in rows {
            for channel in &mut self.rgba[range(row)] {
                *channel = ((u16::from(*channel) + u16::from(original.next().unwrap())) / 2) as u8;
            }
        }
    }

    pub fn line(&mut self, (mut x, mut y): Point, (ex, ey): Point, c: Ink) {
        let dx = (ex - x).abs();
        let dy = -(ey - y).abs();
        let sx = (ex - x).signum();
        let sy = (ey - y).signum();
        let mut err = dx + dy;
        loop {
            self.dot(x, y, c);
            if (x, y) == (ex, ey) {
                break;
            }
            let twice = 2 * err;
            if twice >= dy {
                err += dy;
                x += sx;
            }
            if twice <= dx {
                err += dx;
                y += sy;
            }
        }
    }

    pub fn poly(&mut self, vertices: &[Point], c: Ink) {
        let ymin = vertices.iter().map(|v| v.1).min().unwrap();
        let ymax = vertices.iter().map(|v| v.1).max().unwrap();
        for y in ymin.max(self.bounds.y)..=ymax.min(self.bounds.y + self.bounds.h - 1) {
            let mut lo = i32::MAX;
            let mut hi = i32::MIN;
            for (&(x1, y1), &(x2, y2)) in vertices.iter().zip(vertices.iter().cycle().skip(1)) {
                if y1 == y2 || y < y1.min(y2) || y > y1.max(y2) {
                    continue;
                }
                let x = x1 + (y - y1) * (x2 - x1) / (y2 - y1);
                lo = lo.min(x);
                hi = hi.max(x);
            }
            if hi >= lo {
                self.rect(lo, y, hi - lo + 1, 1, c);
            }
        }
    }

    pub fn oval(&mut self, x: i32, y: i32, rx: i32, ry: i32, c: Ink) {
        for dy in -ry..=ry {
            let span = (rx as f32 * (1.0 - (dy as f32 / ry as f32).powi(2)).max(0.0).sqrt()).round()
                as i32;
            self.rect(x - span, y + dy, span * 2 + 1, 1, c);
        }
    }
}

// Sixteen world units make one floor tile.
pub fn iso(u: i32, v: i32, z: i32) -> Point {
    (320 + 2 * (u - v), 126 + u + v - 2 * z)
}

pub fn tile(p: &mut Pixels, u: i32, v: i32, w: i32, d: i32, z: i32, c: Ink) {
    p.poly(
        &[
            iso(u, v, z),
            iso(u + w, v, z),
            iso(u + w, v + d, z),
            iso(u, v + d, z),
        ],
        c,
    );
}

pub fn block(p: &mut Pixels, u: i32, v: i32, w: i32, d: i32, z: i32, h: i32, colors: [Ink; 3]) {
    let corners = [(u, v), (u + w, v), (u + w, v + d), (u, v + d)];
    let upper = corners.map(|(x, y)| iso(x, y, z + h));
    let lower = corners.map(|(x, y)| iso(x, y, z));
    p.poly(&[upper[2], upper[3], lower[3], lower[2]], colors[1]);
    p.poly(&[upper[1], upper[2], lower[2], lower[1]], colors[2]);
    p.poly(&upper, colors[0]);
}

pub fn hash(x: u32) -> u32 {
    let n = (x ^ (x >> 16)).wrapping_mul(0x45d9f3b);
    (n ^ (n >> 16)).wrapping_mul(0x45d9f3b)
}

// Coordinates on a vertical wall: s runs along its base, h rises from the floor.
// One unit here is one screen pixel, so details can be painted without scaling.
#[derive(Clone, Copy)]
pub struct Wall {
    pub x: i32,
    pub y: i32,
    pub dir: i32,
}
impl Wall {
    pub fn at(self, s: i32, h: i32) -> Point {
        (self.x + self.dir * s, self.y + s.div_euclid(2) - h)
    }
    pub fn dot(self, p: &mut Pixels, s: i32, h: i32, c: Ink) {
        let (x, y) = self.at(s, h);
        p.dot(x, y, c);
    }
    pub fn pane(self, p: &mut Pixels, a: i32, b: i32, low: i32, high: i32, color: Ink) {
        for s in a..b {
            let (x, y) = self.at(s, high - 1);
            p.rect(x, y, 1, high - low, color);
        }
    }
    pub fn stroke(self, p: &mut Pixels, a: Point, b: Point, c: Ink) {
        p.line(self.at(a.0, a.1), self.at(b.0, b.1), c);
    }
}

pub fn mix(a: Ink, b: Ink, t: i32, span: i32) -> Ink {
    let f = |shift: u32| {
        let start = ((a >> shift) & 255) as i32;
        let end = ((b >> shift) & 255) as i32;
        (start + (end - start) * t.clamp(0, span) / span) as u32
    };
    f(16) << 16 | f(8) << 8 | f(0)
}

#[path = "harbor.rs"]
pub mod harbor;
