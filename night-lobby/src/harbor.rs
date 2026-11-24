// The two rooms share the silhouettes of the tidal harbor beyond the windows.
use super::{H, Ink, Pixels, W, hash, mix};

fn noise(x: i32, salt: u32) -> i32 {
    (hash((x as u32).wrapping_add(salt)) & 255) as i32
}
fn ridge(x: i32, period: i32, salt: u32, amplitude: i32) -> i32 {
    let k = x.div_euclid(period);
    let t = x.rem_euclid(period);
    ((period - t) * noise(k, salt) + t * noise(k + 1, salt)) * amplitude / (period * 255)
}
fn cloud(p: &mut Pixels, x: i32, y: i32, width: i32, height: i32, salt: u32, colors: [Ink; 3]) {
    for i in 0..width {
        let t = (i as f32 / width as f32 * std::f32::consts::PI).sin();
        let hump = (ridge(i, 21, salt, 10) + ridge(i, 8, salt + 80, 4)) * height / 11;
        let top = y - (hump as f32 * t) as i32;
        let bottom = y + (ridge(i, 43, salt + 2, 5) as f32 * t) as i32;
        p.rect(x + i, top, 1, (bottom - top).max(1), colors[0]);
        if top + 3 < bottom {
            p.rect(x + i, bottom - 3, 1, 2, colors[1]);
            if ridge(i, 13, salt + 65, 5) > 2 {
                p.dot(x + i, bottom - 1, colors[2]);
            }
        }
    }
}
fn clocktower(p: &mut Pixels, x: i32, y: i32, col: Ink, lamp: Ink) {
    p.rect(x - 5, y - 23, 11, 30, col);
    p.rect(x - 7, y - 25, 15, 3, col);
    p.poly(
        &[
            (x - 7, y - 25),
            (x - 3, y - 33),
            (x, y - 39),
            (x + 3, y - 33),
            (x + 7, y - 25),
        ],
        col,
    );
    p.line((x, y - 37), (x, y - 44), col);
    p.rect(x - 2, y - 20, 5, 3, lamp);
    p.rect(x - 1, y - 21, 3, 5, lamp);
    p.rect(x, y - 20, 1, 2, col);
    p.dot(x + 1, y - 18, col);
}
const NIGHT_SKY: Ink = 0x192338;

fn shade(color: Ink, outside: bool) -> Ink {
    if outside {
        color
    } else {
        mix(color, 0x102433, 3, 7)
    }
}

pub fn sky_color(outside: bool) -> Ink {
    shade(NIGHT_SKY, outside)
}

pub fn paint(p: &mut Pixels, outside: bool) {
    let tone = |c| shade(c, outside);
    let view = p.bounds;
    let right = view.x + view.w;
    let bottom = view.y + view.h;
    let sky = [
        (0, NIGHT_SKY),
        (36, 0x283650),
        (80, 0x53516b),
        (113, 0x9e747a),
        (142, 0xd89b83),
        (167, 0xe2b387),
    ];
    let mut i = 0;
    for y in view.y..170 {
        if i + 2 < sky.len() && y > sky[i + 1].0 {
            i += 1;
        }
        let (a, ac) = sky[i];
        let (b, bc) = sky[i + 1];
        // Narrow ramps, with staggered one-pixel transitions in the open sky.
        let step = ((y - a) / 3) * 3;
        p.rect(view.x, y, view.w, 1, tone(mix(ac, bc, step, b - a)));
    }
    for i in 0..(185 * view.w * (107 - view.y) / (W * 107)) as u32 {
        let n = hash(i + 743);
        let x = view.x + (n % view.w as u32) as i32;
        let y = view.y + ((n >> 12) % (107 - view.y) as u32) as i32;
        if y > 60 && i % 3 != 0 {
            continue;
        }
        let c = tone([0x58617d, 0x92879a, 0x797e93, 0xc3ada7][i as usize % 4]);
        p.dot(x, y, c);
        if i % 47 == 0 {
            p.rect(x - 1, y, 3, 1, c);
            p.rect(x, y - 1, 1, 3, c);
            p.dot(x, y, tone(0xded2bf));
        }
    }
    let (mx, my) = (492, 52);
    for r in [24, 20, 17] {
        let c = match r {
            24 => 0x40445e,
            20 => 0x494961,
            _ => 0x565066,
        };
        p.oval(mx, my, r, r, tone(c));
    }
    p.oval(mx, my, 14, 14, tone(0xe8c7a2));
    p.oval(mx + 5, my - 4, 11, 12, tone(0x565066));
    for (x, y, w, h, k) in [
        (-30, 48, 190, 11, 91),
        (127, 36, 79, 5, 48),
        (376, 75, 104, 7, 71),
        (529, 47, 148, 12, 128),
        (-18, 88, 251, 14, 39),
        (361, 102, 195, 17, 66),
        (520, 113, 165, 14, 16),
    ] {
        let cs = if y < 65 {
            [0x41445f, 0x56516b, 0x675b71]
        } else {
            [0x755f78, 0xa07983, 0xc18b89]
        };
        cloud(p, x, y, w, h, k, cs.map(tone));
    }
    for x in view.x..right {
        let a = 144 - ridge(x, 79, 273, 32) - ridge(x, 18, 114, 5);
        p.rect(x, a, 1, 167 - a, tone(0x74697d));
        let b = 147 - ridge(x, 54, 817, 23) - ridge(x, 11, 91, 4);
        p.rect(x, b, 1, 166 - b, tone(0x5b5b71));
    }
    let mut x = -4;
    let mut i = 0;
    while x > view.x {
        i -= 1;
        x -= 7 + noise(i, 198) % 12;
    }
    while x < right {
        let n = noise(i, 198);
        let w = 8 + n % 12;
        let y = 144 + (n / 13) % 9;
        let col = tone(if i % 3 == 0 { 0x494d65 } else { 0x55566b });
        p.rect(x, y, w, 166 - y, col);
        match i % 5 {
            0 | 1 => p.poly(
                &[(x - 1, y), (x + w / 2, y - 4 - n % 6), (x + w + 1, y)],
                col,
            ),
            2 => {
                p.rect(x + 2, y - 4, w - 4, 5, col);
                p.rect(x + w - 5, y - 8, 2, 5, col);
            }
            _ => p.rect(x + 1, y - 1, w - 2, 2, col),
        }
        for j in (3..w - 1).step_by(4) {
            if noise(i + j, 71) % 4 != 0 {
                p.rect(
                    x + j,
                    y + 3,
                    2,
                    2,
                    tone(if i % 4 == 2 { 0xe3b77e } else { 0x9c8c88 }),
                );
            }
        }
        if i == 5 || i == 32 {
            clocktower(p, x + w / 2, y, col, tone(0xd5ac7d));
        }
        x += w - 1;
        i += 1;
    }
    // Behind the old wharf, a stone viaduct cuts eight open arches across the sunset.
    for anchor in [-12, 485] {
        for n in 0..7 {
            let x = anchor + n * 23;
            for s in 0i32..23 {
                let dist = (s - 11).abs() as f32 / 10.;
                let arch = if dist < 1. {
                    (7. * (1. - dist * dist).sqrt()) as i32
                } else {
                    0
                };
                p.rect(x + s, 152, 1, 12 - arch, tone(0x363f55));
            }
            p.rect(x, 149, 23, 2, tone(0x7b727b));
            p.rect(x + 4, 145, 1, 6, tone(0x454656));
            p.dot(x + 4, 144, tone(0xffcb86));
        }
    }
    for y in 164..bottom {
        let c = if y < 227 {
            mix(0x515c70, 0x2e4557, (y - 164) / 3 * 3, 63)
        } else {
            mix(0x2e4557, 0x152c40, (y - 227) / 4 * 4, H - 227)
        };
        p.rect(view.x, y, view.w, 1, tone(c));
    }
    for i in
        0..(2600_i64 * i64::from(view.w) * i64::from(bottom - 163) / (i64::from(W) * 285)) as u32
    {
        let k = hash(i + 1421);
        let x = view.x + (k % (view.w + 15) as u32) as i32 - 7;
        let y = 164 + ((k >> 13) % (bottom - 163) as u32) as i32;
        let len = 2 + ((k >> 6) % if y < 210 { 8 } else { 17 }) as i32;
        let mut c = mix(0x737780, 0x314958, y - 164, 230);
        if k % 4 == 0 {
            c = mix(0x3b4c62, 0x203747, y - 164, 230);
        }
        let reflection = ((x + 3).rem_euclid(51) - 25).abs();
        if y < 220 && reflection < 2 + (y - 164) / 13 && k % 3 == 0 {
            c = mix(0xb19584, 0x6b6d76, y - 164, 56);
        }
        p.rect(x, y, len, 1, tone(c));
    }
    // Small moored sailboats punctuate the channel at the far ends of the quay.
    for (x, y, h) in [(32, 207, 33), (61, 194, 19), (580, 213, 39), (611, 197, 24)] {
        let hull = tone(0x29394c);
        p.poly(
            &[(x - 11, y), (x + 13, y - 1), (x + 8, y + 3), (x - 7, y + 3)],
            hull,
        );
        p.line((x, y - 1), (x, y - h), hull);
        p.line((x - 9, y - 1), (x, y - h + 3), tone(0x697180));
        p.line((x, y - h + 4), (x + 9, y - 3), tone(0x747582));
        p.rect(x - 4, y - 2, 8, 2, tone(0x8e817e));
        p.rect(x - 4, y + 5, 12, 1, tone(0x374b5c));
    }
}

pub fn waterlight(p: &mut Pixels, now: u32) {
    for i in 0..56 {
        let n = hash(i + 846);
        let x = (n % 640) as i32;
        let y = 168 + ((n >> 12) % 230) as i32;
        let d = (x - 320).abs();
        if !(y > 436 - d / 2 || (d > 145 && y < 123 + d / 2)) {
            continue;
        }
        let phase = (now / 270 + i * 7) % 12;
        if phase < 4 {
            p.rect(
                x,
                y,
                2 + phase as i32,
                1,
                if y < 218 { 0x9b8e88 } else { 0x4b6170 },
            );
        }
    }
}
