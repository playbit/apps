use crate::font;
use crate::guest::Cell;
use crate::pixels::{Ink, Pixels, Wall, block, harbor, hash, iso, mix, tile};
use crate::plaza::Place;

const TRIM: [Ink; 3] = [0xa3977c, 0x706d60, 0x53605b];
const IRON: Ink = 0x32444a;

pub fn background(p: &mut Pixels) {
    harbor::paint(p, true);
    embankment(p);
    cobbles(p);
    railing(p);
    bar(p);
}
fn embankment(p: &mut Pixels) {
    block(p, 0, 0, 144, 144, -10, 10, [0x9f9790, 0x625f67, 0x4e5261]);
    for d in [1, -1] {
        let (x, y) = if d == 1 {
            iso(0, 144, 0)
        } else {
            iso(144, 0, 0)
        };
        let w = Wall { x, y, dir: d };
        for row in 0..2 {
            let mut s = 0;
            while s < 288 {
                let k = hash((s * 13 + row * 433) as u32);
                let len = (14 + (k % 12) as i32).min(288 - s);
                let col = if d == 1 { 0x857679 } else { 0x696776 };
                w.pane(
                    p,
                    s,
                    s + len - 1,
                    -18 + row * 8,
                    -11 + row * 8,
                    mix(col, 0xada59a, (k % 4) as i32, 12),
                );
                w.pane(
                    p,
                    s + 1,
                    s + len - 2,
                    -13 + row * 8,
                    -12 + row * 8,
                    mix(col, 0xb9a58a, 2, 7),
                );
                s += len;
            }
        }
        w.pane(p, 0, 288, -3, -1, 0xc3aa90);
        w.pane(p, 0, 288, -1, 1, 0xf0cda1);
        for s in [45, 139, 241] {
            let (x, y) = w.at(s, -10);
            p.oval(x, y, 3, 4, 0x414952);
            p.oval(x, y, 1, 2, 0x817979);
            p.dot(x - 1, y - 3, 0xb09d87);
        }
    }
}
fn cobbles(p: &mut Pixels) {
    tile(p, 0, 0, 144, 144, 0, 0x7a7884);
    // Hand-set granite grows paler under each lamp. A few thin low spots hold water.
    for row in 0i32..36 {
        for col in 0i32..19 {
            let v = row * 4;
            let a = (col * 8 - row % 2 * 4).max(0);
            let b = (col * 8 + 8 - row % 2 * 4).min(144);
            if a >= b {
                continue;
            }
            let n = hash((row * 313 + col * 17 + 67) as u32);
            let palette = [
                0x9b939a, 0x9b9093, 0xa19798, 0x898994, 0x958e9a, 0x8f8c96, 0xa09699,
            ];
            let lamp = ((a - 120).pow(2) + (v - 24).pow(2)).min((a - 24).pow(2) + (v - 120).pow(2));
            let w = (4000 - lamp).max(0) / 700;
            let mut c = mix(palette[n as usize % 7], 0xd9b597, w, 12);
            if (a < 64 && v < 61) || (a > 61 && v < 40 && a + v < 133) {
                c = mix(c, 0x626678, 3, 7);
            }
            tile(p, a, v, b - a, 4, 0, c);
            let l = iso(a, v, 0);
            let r = iso(b, v, 0);
            let btm = iso(b, v + 4, 0);
            let left = iso(a, v + 4, 0);
            p.line(l, r, mix(c, 0xcfc0b1, 3, 9));
            p.line(r, btm, mix(c, 0x565766, 3, 7));
            p.line(left, btm, mix(c, 0x585866, 3, 7));
            if n % 4 == 0 && b - a > 4 {
                p.line(
                    iso(a + 2, v + 1, 0),
                    iso(a + 4, v + 1, 0),
                    mix(c, 0xc4c2b7, 4, 9),
                );
            }
        }
    }
    // A broken field of honey-colored window light on the damp cobblestones.
    for y in 184i32..274 {
        for x in 163..312 {
            let u = (x - 320 + 2 * (y - 126)) / 4;
            let v = (2 * (y - 126) - (x - 320)) / 4;
            let depth = v - 61;
            let along = u + depth / 3;
            let gap = along.rem_euclid(13);
            if (0..32).contains(&depth)
                && ((2..24).contains(&along) || (31..50).contains(&along))
                && gap > 1
            {
                let sparkle = hash((x / 5 + y * 917) as u32) % 5 == 0;
                let strength = (32 - depth) / 7 + if sparkle { 2 } else { 0 };
                p.tint(x, y, 0xefb577, strength, 18);
            }
        }
    }
    for e in [2, 5, 139, 142] {
        let c = if e == 5 || e == 139 {
            0x82777a
        } else {
            0xc9b59d
        };
        p.line(iso(2, e, 0), iso(142, e, 0), c);
        p.line(iso(e, 2, 0), iso(e, 142, 0), c);
    }
    let (x, y) = iso(83, 78, 0);
    // A brass wind rose sunk into blue slate. The ellipse shares the floor plane.
    for (r, c) in [
        (34, 0x716973),
        (32, 0xccae85),
        (30, 0x696c76),
        (27, 0x9c9c99),
        (25, 0x727c83),
    ] {
        p.oval(x, y, r, r / 2, c);
    }
    for (dx, dy) in [
        (0, -14),
        (0, 14),
        (-27, 0),
        (27, 0),
        (14, 8),
        (-14, 8),
        (14, -8),
        (-14, -8),
    ] {
        let sx = if dy == 0 { 0 } else { 3 };
        let sy = if dx == 0 { 0 } else { 2 };
        p.poly(&[(x + dx, y + dy), (x + sx, y - sy), (x, y)], 0xe1bc89);
        p.poly(&[(x + dx, y + dy), (x - sx, y + sy), (x, y)], 0xb29779);
    }
    p.oval(x, y, 4, 2, 0xead3a5);
}
fn railing(p: &mut Pixels) {
    for axis in [false, true] {
        let point = |s, z| if axis { iso(s, 0, z) } else { iso(0, s, z) };
        for s in (64..143).step_by(16) {
            let (a, b) = (point(s, 10), point(s + 16, 10));
            // Slack twin chains between granite bollards.
            for k in 0..=16 {
                let sag = (k * (16 - k) * 4) / 64;
                let x = a.0 + (b.0 - a.0) * k / 16;
                let y = a.1 + (b.1 - a.1) * k / 16 + sag;
                p.rect(x.min(x + (b.0 - a.0) / 16), y, 3, 2, 0x475557);
                if k % 2 == 0 {
                    p.dot(x, y, 0xa3a095);
                }
            }
        }
        for s in (64..=144).step_by(16) {
            let (u, v) = if axis { (s, 0) } else { (0, s) };
            block(p, u - 1, v - 1, 2, 2, 0, 11, TRIM);
            let (x, y) = point(s, 11);
            p.oval(x, y, 4, 2, 0xb8ac96);
            p.oval(x, y - 2, 2, 2, 0x7d807a);
            p.dot(x - 1, y - 3, 0xc9b99b);
        }
    }
}
fn facade_bricks(p: &mut Pixels, w: Wall, side: bool) {
    w.pane(p, 0, 122, 4, 76, if side { 0x75524f } else { 0x986c58 });
    for row in 0..11 {
        for col in 0..10 {
            let s = (col * 16 - 8 * (row % 2)).max(0);
            let b = (col * 16 + 15 - 8 * (row % 2)).min(122);
            if b <= s {
                continue;
            }
            let n = hash((row * 31 + col * 59) as u32);
            let h = 7 + row * 6;
            let c = [0xb57a61, 0xaa755e, 0xa46a55, 0xb47e66, 0xbc8064][n as usize % 5];
            w.pane(
                p,
                s,
                b,
                h,
                h + 5,
                if side { mix(c, 0x514451, 3, 7) } else { c },
            );
            if n % 3 != 0 {
                w.pane(
                    p,
                    s + 1,
                    b - 2,
                    h + 4,
                    h + 5,
                    if side { 0x9b796b } else { 0xc09173 },
                );
            }
        }
    }
    w.pane(p, 0, 122, 0, 6, 0x4c5751);
    w.pane(p, 0, 122, 5, 7, 0xaaa088);
}
fn shop_window(p: &mut Pixels, w: Wall, a: i32, width: i32, side: bool) {
    let b = a + width;
    w.pane(p, a - 3, b + 3, 12, 64, 0x76534a);
    w.pane(p, a - 2, b + 2, 13, 64, 0xc7a27b);
    w.pane(p, a, b, 14, 62, 0x314b49);
    for s in a + 3..b - 2 {
        for h in 19..58 {
            let local = s - a;
            let mut col = mix(0xd58756, 0xffdb91, (h - 19) / 4 * 4, 39);
            if side {
                col = mix(col, 0xdfa270, 1, 5);
            }
            // A curtain turned back at the sill, bottles and a copper pendant in the glass.
            let curtain = ((h - 37).max(0) / 4 + 3).max((35 - h).max(0) / 5 + 2);
            if local < curtain || width - local < curtain {
                col = if local % 4 <= 1 { 0x9b5951 } else { 0xc17a61 };
            }
            let lamp_x = width / 2 - 4;
            if local == lamp_x && h > 50 {
                col = 0x75594d;
            }
            if (local - lamp_x).abs() <= ((52 - h).clamp(0, 4)) && (47..=52).contains(&h) {
                col = 0xfbe4aa;
            }
            if h == 47 && (local - lamp_x).abs() < 6 {
                col = 0xbc8c62;
            }
            w.dot(p, s, h, col);
        }
    }
    for j in 0..(width / 9) {
        let (x, y) = w.at(a + 6 + j * 8, 20);
        crate::glasshouse::bottle(p, x, y, 8 + (j % 2) * 3, 2, j as usize + 3);
    }
    for s in [a + width / 2] {
        w.pane(p, s, s + 2, 16, 61, 0x4d564b);
        w.pane(p, s + 2, s + 3, 16, 61, 0xc2a478);
    }
    w.pane(p, a, b, 41, 44, 0x485449);
    w.pane(p, a, b, 43, 44, 0xb6976c);
    w.pane(p, a - 3, b + 3, 11, 14, 0x515a50);
    w.pane(p, a - 4, b + 4, 14, 16, 0xbda387);
}
fn front_door(p: &mut Pixels, w: Wall, a: i32, b: i32, inside: bool) {
    let c = if inside { 0x79573f } else { 0x335956 };
    w.pane(p, a - 4, b + 4, 0, 84, 0x343f40);
    w.pane(p, a - 2, b + 2, 0, 82, 0xc89c6c);
    w.pane(p, a, b, 1, 80, 0x493d38);
    w.pane(p, a + 2, b - 2, 1, 78, c);
    w.pane(p, a + 5, b - 5, 27, 71, 0x473e3c);
    for s in a + 7..b - 7 {
        for h in 29..70 {
            let col = if inside {
                let base = mix(0xa68a84, 0x637a88, (h - 29) / 4 * 4, 40);
                if (s + h / 2) % 29 < 3 {
                    mix(base, 0xcbb6a3, 2, 6)
                } else {
                    base
                }
            } else {
                let base = mix(0xc08a5f, 0xffdf9e, (h - 29) / 4 * 4, 39);
                if (s + h / 2) % 25 < 3 {
                    mix(base, 0xffe4a9, 2, 7)
                } else {
                    base
                }
            };
            w.dot(p, s, h, col);
        }
    }
    let mid = (a + b) / 2;
    w.pane(p, mid - 1, mid + 1, 27, 72, 0x715b45);
    w.pane(p, a + 5, b - 5, 50, 52, 0x715b45);
    for (lo, hi) in [(6, 21), (71, 76)] {
        w.pane(p, a + 5, b - 5, lo, hi, 0x534538);
        w.pane(p, a + 6, b - 6, lo + 2, hi, 0x9d7654);
        if hi - lo > 5 {
            w.pane(p, a + 8, b - 8, lo + 3, hi - 2, c);
        }
    }
    let (x, y) = w.at(b - 6, 25);
    p.rect(x - 1, y - 2, 2, 6, 0xf2cd83);
    p.rect(x - 3, y, 4, 2, 0xd7a66b);
}
fn flowerbox(p: &mut Pixels, w: Wall, a: i32, b: i32) {
    w.pane(p, a, b, 8, 16, 0x6a4d43);
    w.pane(p, a, b, 14, 16, 0xb28160);
    for i in a..b {
        let n = hash((i + 439) as u32);
        let h = 15 + (n % 5) as i32;
        w.pane(
            p,
            i,
            i + 1,
            14,
            h + 1,
            [0x35534f, 0x5d7958, 0x738459][n as usize % 3],
        );
        if n % 4 == 0 {
            let (x, y) = w.at(i, h);
            p.rect(
                x - 1,
                y - 2,
                3,
                3,
                if n % 3 == 0 { 0xdb9c86 } else { 0xb46b69 },
            );
            p.dot(x - 1, y - 2, 0xeab797);
        }
    }
}
fn roof(p: &mut Pixels) {
    let crest = |u, v| {
        iso(
            u,
            v,
            if v < 24 {
                62 - (24 - v) * 24 / 27
            } else {
                62 - (v - 24) * 24 / 41
            },
        )
    };
    // Scalloped slate overlaps one course at a time. One edge catches the moon.
    for (start, end, dir) in [(-3, 24, 1), (65, 24, -1)] {
        let mut v = start;
        let mut row = 0;
        while v != end {
            let v2 = if dir > 0 {
                (v + 5).min(end)
            } else {
                (v - 5).max(end)
            };
            for n in 0..11 {
                let a = (n * 8 - (row % 2) * 4 - 3).max(-3);
                let b = (n * 8 + 8 - (row % 2) * 4 - 3).min(65);
                if a >= b {
                    continue;
                }
                let seed = hash((row * 37 + n * 31 + 41) as u32);
                let mut c =
                    [0x52677a, 0x5a6c7a, 0x586676, 0x657787, 0x617280, 0x536477][seed as usize % 6];
                if dir > 0 {
                    c = mix(c, 0x9b9996, 2, 7);
                }
                let (a1, b1, a2, b2) = (crest(a, v), crest(b, v), crest(a, v2), crest(b, v2));
                p.poly(&[a1, b1, b2, a2], c);
                p.line(a1, b1, 0x404e60);
                p.line((a1.0, a1.1 - 1), (b1.0, b1.1 - 1), mix(c, 0xb0a59d, 2, 7));
                p.line(b1, b2, mix(c, 0x334758, 3, 8));
                if seed % 5 == 0 {
                    p.line(crest(a + 1, v2), crest((a + 4).min(b), v2), 0x7f8c92);
                }
            }
            v = v2;
            row += 1;
        }
    }
    let (r1, r2) = (crest(-3, 24), crest(65, 24));
    p.line(r1, r2, 0xd1b492);
    p.line((r1.0, r1.1 + 1), (r2.0, r2.1 + 1), 0x957f72);
    // Timber framing on the gable wall.
    let p1 = crest(65, -3);
    let p2 = crest(65, 65);
    let pk = crest(65, 24);
    p.poly(&[p1, p2, pk], 0xc49c7c);
    for v in [4, 16, 24, 32, 44, 56] {
        p.line(iso(65, v, 38), crest(65, v), 0x75594f);
        p.line(iso(65, v + 1, 38), crest(65, v + 1), 0x98765e);
    }
    for (a, b) in [(3, 23), (25, 45), (45, 60)] {
        p.line(iso(65, a, 38), crest(65, b), 0x8a6956);
    }
    for (a, b) in [(p1, pk), (pk, p2)] {
        p.line((a.0, a.1 + 2), (b.0, b.1 + 2), 0x705e55);
        p.line((a.0, a.1 + 1), (b.0, b.1 + 1), 0xc0ac89);
    }
    p.line(p1, pk, 0x4c5058);
    p.line(pk, p2, 0x4c5058);
    let w = Wall {
        x: 446,
        y: 189,
        dir: -1,
    };
    let (x, y) = w.at(48, 96);
    p.oval(x, y, 7, 8, 0x7b5b4e);
    p.oval(x, y, 5, 6, 0xd4a976);
    for dy in [-3, 0, 3] {
        p.rect(x - 3, y + dy, 7, 1, 0x695b51);
    }
    // A small dormer on the street side of the roof.
    let (x, y) = iso(23, 45, 50);
    p.poly(
        &[
            (x - 13, y + 2),
            (x - 13, y - 10),
            (x - 1, y - 22),
            (x + 13, y - 13),
            (x + 13, y + 13),
        ],
        0x514c4d,
    );
    p.poly(
        &[
            (x - 11, y),
            (x - 11, y - 10),
            (x - 1, y - 19),
            (x + 8, y - 10),
            (x + 8, y + 9),
        ],
        0xb1896c,
    );
    p.poly(
        &[
            (x - 8, y - 1),
            (x - 8, y - 9),
            (x - 2, y - 15),
            (x + 5, y - 9),
            (x + 5, y + 5),
        ],
        0x455760,
    );
    p.rect(x - 5, y - 8, 7, 7, 0xdbab76);
    p.rect(x - 2, y - 8, 1, 7, 0x756b5d);
    p.rect(x - 5, y - 5, 7, 1, 0x756b5d);
    p.poly(
        &[
            (x - 17, y - 9),
            (x - 1, y - 26),
            (x + 3, y - 25),
            (x - 12, y - 9),
        ],
        0x879397,
    );
    p.poly(
        &[
            (x - 1, y - 26),
            (x + 17, y - 17),
            (x + 23, y - 7),
            (x + 12, y - 9),
        ],
        0x5e6b76,
    );
    p.line((x - 1, y - 24), (x + 13, y - 9), 0xb1a58f);
    // Double terracotta chimney pots. The smoke originates at the left pot.
    block(p, 7, 12, 8, 8, 51, 13, [0xc38b70, 0xa36f5e, 0x795552]);
    for z in [55, 60] {
        p.line(iso(7, 20, z), iso(15, 20, z), 0xd8a083);
        p.line(iso(15, 12, z), iso(15, 20, z), 0x99736b);
    }
    block(p, 6, 11, 10, 10, 64, 2, [0xd4ae90, 0xae816b, 0x91695e]);
    for (u, v) in [(9, 16), (13, 14)] {
        let (x, y) = iso(u, v, 66);
        p.rect(x - 2, y - 7, 5, 7, 0xac745e);
        p.rect(x - 2, y - 7, 1, 6, 0xd99e7d);
        p.rect(x - 3, y - 9, 7, 3, 0xc48e73);
        p.rect(x - 2, y - 9, 5, 1, 0x78594f);
    }
}
fn ivy(p: &mut Pixels, x: i32, y: i32, dir: i32, lengths: &[i32]) {
    for (i, &length) in lengths.iter().enumerate() {
        let base = x + i as i32 * 3 * dir;
        let mut last = (base, y + i as i32);
        for t in 0..length {
            let bend = (t / 4).rem_euclid(4);
            let a = base + dir * (if bend < 2 { bend } else { 3 - bend });
            let b = y + i as i32 + t;
            p.line(last, (a, b), 0x545e48);
            if t % 4 == 1 {
                let d = if (t + i as i32) % 3 == 0 { -1 } else { 1 };
                p.poly(
                    &[
                        (a, b),
                        (a + d * 2, b - 1),
                        (a + d * 4, b),
                        (a + d * 3, b + 3),
                        (a + d, b + 2),
                    ],
                    if t % 3 == 0 { 0x859160 } else { 0x677c55 },
                );
                p.dot(a + d * 2, b, 0x9fa674);
            }
            last = (a, b);
        }
    }
}
fn roof_cat(p: &mut Pixels, now: u32) {
    let (x, y) = iso(40, 43, 51);
    p.oval(x + 2, y, 9, 3, 0x465563);
    p.poly(
        &[
            (x - 8, y - 1),
            (x - 8, y - 4),
            (x - 6, y - 7),
            (x - 1, y - 8),
            (x + 4, y - 6),
            (x + 5, y - 2),
            (x + 4, y),
            (x - 4, y + 1),
        ],
        0x3b414c,
    );
    p.poly(
        &[
            (x + 1, y - 5),
            (x + 1, y - 11),
            (x + 4, y - 9),
            (x + 6, y - 9),
            (x + 9, y - 12),
            (x + 9, y - 6),
            (x + 8, y - 3),
            (x + 4, y - 2),
        ],
        0x343e49,
    );
    p.line((x - 6, y - 6), (x - 2, y - 7), 0x716965);
    p.line((x - 6, y), (x - 11, y - 2), 0x3c424c);
    p.line((x - 11, y - 2), (x - 12, y - 5), 0x3c424c);
    p.rect(x + 4, y - 3, 3, 2, 0xa8a398);
    p.rect(x + 1, y - 1, 4, 1, 0x98988e);
    if now / 250 % 17 != 0 {
        p.dot(x + 4, y - 6, 0xdaba81);
        p.dot(x + 8, y - 6, 0xdaba81);
    }
}

pub fn sign_center() -> (i32, i32) {
    iso(38, 66, 42)
}
fn sign(p: &mut Pixels) {
    let (x, y) = sign_center();
    let width = font::width("Session", 2) + 18;
    let a = x - width / 2;
    let b = a + width;
    for sx in [a + 7, b - 8] {
        p.rect(sx, y - 21, 2, 19, 0x4f5149);
        p.rect(sx, y - 19, 1, 14, 0xb5a079);
    }
    p.poly(
        &[
            (a - 3, y - 8),
            (a + 2, y - 13),
            (b - 2, y - 13),
            (b + 3, y - 8),
            (b + 3, y + 8),
            (b - 2, y + 12),
            (a + 2, y + 12),
            (a - 3, y + 8),
        ],
        0x323d42,
    );
    p.poly(
        &[
            (a - 1, y - 7),
            (a + 3, y - 11),
            (b - 3, y - 11),
            (b + 1, y - 7),
            (b + 1, y + 7),
            (b - 3, y + 10),
            (a + 3, y + 10),
            (a - 1, y + 7),
        ],
        0xdda96c,
    );
    p.poly(
        &[
            (a + 1, y - 6),
            (a + 4, y - 9),
            (b - 4, y - 9),
            (b - 1, y - 6),
            (b - 1, y + 6),
            (b - 4, y + 8),
            (a + 4, y + 8),
            (a + 1, y + 6),
        ],
        0x365957,
    );
    font::draw(
        p,
        x - font::width("Session", 2) / 2,
        y - 9,
        "Session",
        0x253e40,
        2,
    );
    font::draw(
        p,
        x - font::width("Session", 2) / 2,
        y - 10,
        "Session",
        0xffe7ad,
        2,
    );
    for xx in [a + 4, b - 5] {
        p.rect(xx, y - 1, 2, 2, 0xe0b77b);
    }
}
fn bar(p: &mut Pixels) {
    for pad in [0, 1] {
        block(
            p,
            -pad,
            -pad,
            62 + 2 * pad,
            62 + 2 * pad,
            0,
            3,
            [0xb9a697, 0x957f79, 0x776d72],
        );
    }
    let (x, y) = iso(0, 61, 0);
    let front = Wall { x, y, dir: 1 };
    let (x, y) = iso(61, 0, 0);
    let side = Wall { x, y, dir: -1 };
    facade_bricks(p, front, false);
    facade_bricks(p, side, true);
    shop_window(p, front, 7, 44, false);
    flowerbox(p, front, 6, 52);
    shop_window(p, side, 9, 36, true);
    shop_window(p, side, 61, 38, true);
    flowerbox(p, side, 60, 100);
    front_door(p, front, 60, 101, false);
    tile(p, 29, 61, 24, 7, 1, 0xd5b295);
    tile(p, 28, 67, 26, 4, 0, 0xba9f88);
    for i in 0..8 {
        p.line(iso(32 + i * 2, 63, 1), iso(32 + i * 2, 67, 1), 0x94816f);
    }
    for w in [front, side] {
        w.pane(p, 0, 122, 71, 75, 0x415852);
        w.pane(p, 0, 122, 74, 77, 0xb29b7b);
        for s in [1, 112] {
            w.pane(p, s, s + 7, 6, 74, 0x38534e);
            w.pane(p, s + 1, s + 3, 8, 72, 0x78917d);
            w.pane(p, s - 1, s + 8, 7, 11, 0x667867);
            w.pane(p, s - 1, s + 8, 68, 72, 0x9a9c7e);
        }
    }
    roof(p);
    let (ix, iy) = side.at(3, 73);
    ivy(p, ix, iy, -1, &[7, 18, 12, 27, 16, 8]);
    let (ix, iy) = side.at(98, 73);
    ivy(p, ix, iy, -1, &[8, 14, 22, 30, 15]);

    // A scalloped oxblood awning, bleaching to apricot at the corners.
    for i in 0..12 {
        let u = 1 + i * 5;
        let c = if i % 2 == 0 { 0xa95046 } else { 0xe4b388 };
        p.poly(
            &[
                iso(u, 61, 38),
                iso(u + 5, 61, 38),
                iso(u + 5, 68, 34),
                iso(u, 68, 34),
            ],
            c,
        );
        let (a, b) = (iso(u, 68, 34), iso(u + 5, 68, 34));
        p.poly(
            &[
                a,
                b,
                (b.0, b.1 + 3),
                (b.0 - 3, b.1 + 5),
                (a.0 + 3, a.1 + 5),
                (a.0, a.1 + 3),
            ],
            if i % 2 == 0 { 0x884740 } else { 0xc99775 },
        );
        p.line(a, b, mix(c, 0xffce9d, 2, 7));
    }
    sign(p);
}
fn lamp(p: &mut Pixels, x: i32, y: i32, now: u32) {
    block(
        p,
        (x - 320 + 2 * (y - 126)) / 4 - 4,
        (2 * (y - 126) - (x - 320)) / 4 - 4,
        8,
        8,
        0,
        4,
        TRIM,
    );
    p.oval(x, y - 8, 6, 3, 0x56605b);
    p.oval(x, y - 10, 4, 3, 0x657067);
    p.poly(
        &[
            (x - 4, y - 11),
            (x - 2, y - 23),
            (x - 2, y - 48),
            (x + 2, y - 48),
            (x + 2, y - 23),
            (x + 4, y - 11),
        ],
        IRON,
    );
    p.line((x - 1, y - 12), (x - 1, y - 48), 0x929880);
    for h in [13, 19, 43, 47] {
        p.rect(x - 3, y - h, 7, 2, 0x637366);
        p.dot(x - 3, y - h, 0xafaa8f);
    }
    p.poly(
        &[
            (x - 8, y - 65),
            (x + 8, y - 65),
            (x + 6, y - 51),
            (x + 2, y - 47),
            (x - 2, y - 47),
            (x - 6, y - 51),
        ],
        0x394d4d,
    );
    p.poly(
        &[
            (x - 6, y - 64),
            (x + 6, y - 64),
            (x + 4, y - 52),
            (x, y - 50),
            (x - 4, y - 52),
        ],
        0xf7c37e,
    );
    p.poly(
        &[
            (x - 5, y - 63),
            (x - 1, y - 63),
            (x - 1, y - 52),
            (x - 3, y - 53),
        ],
        0xf9dea0,
    );
    p.poly(
        &[
            (x + 1, y - 62),
            (x + 5, y - 62),
            (x + 3, y - 53),
            (x + 1, y - 51),
        ],
        0xe1a769,
    );
    p.rect(x - 1, y - 63, 1, 12, 0xa68960);
    let flicker = if now / 420 % 4 > 0 {
        0xffe9ac
    } else {
        0xffdfa0
    };
    p.rect(x + 1, y - 59, 2, 7, flicker);
    p.dot(x + 2, y - 61, flicker);
    p.poly(
        &[
            (x - 10, y - 65),
            (x - 9, y - 67),
            (x - 4, y - 70),
            (x - 2, y - 73),
            (x + 2, y - 73),
            (x + 4, y - 70),
            (x + 9, y - 67),
            (x + 10, y - 65),
        ],
        0x405557,
    );
    p.line((x - 9, y - 67), (x - 3, y - 70), 0x93a28d);
    p.rect(x - 9, y - 65, 18, 1, 0xab9b79);
    p.oval(x, y - 75, 2, 3, 0x58665d);
    p.dot(x - 1, y - 76, 0xc7b28b);
    for d in [-1, 1] {
        p.line((x + 2 * d, y - 40), (x + 8 * d, y - 46), 0x495c56);
        p.line((x + 8 * d, y - 46), (x + 6 * d, y - 49), 0x73806d);
        p.line((x + 6 * d, y - 49), (x + 4 * d, y - 47), 0x73806d);
    }
}
fn stools(p: &mut Pixels, x: i32, y: i32) {
    p.oval(x + 2, y + 1, 10, 3, 0x79747a);
    for (dx, dy, e) in [(-4, -2, -7), (4, -2, 7), (-4, 2, -7), (4, 2, 7)] {
        p.line((x + dx, y - 14 + dy), (x + e, y + dy), 0x554b4c);
        p.line((x + dx - 1, y - 13 + dy), (x + e - 1, y + dy), 0x926f56);
    }
    p.oval(x, y - 13, 11, 5, 0x755544);
    p.oval(x, y - 16, 11, 5, 0xbf8d65);
    p.oval(x, y - 16, 8, 3, 0xd1a077);
    p.line((x - 5, y - 15), (x + 4, y - 17), 0xc29671);
    p.dot(x - 8, y - 17, 0xf2c690);
}
fn cafe(p: &mut Pixels, x: i32, y: i32, now: u32) {
    p.oval(x + 1, y + 1, 12, 4, 0x817b7e);
    p.oval(x, y - 1, 10, 4, 0x4c5152);
    p.oval(x, y - 3, 7, 3, 0x6b6c60);
    p.rect(x - 2, y - 21, 5, 18, 0x4d5653);
    p.rect(x - 1, y - 21, 1, 16, 0x95967e);
    p.oval(x, y - 22, 21, 9, 0x775b50);
    p.oval(x, y - 25, 21, 9, 0xc99d73);
    p.oval(x, y - 25, 18, 7, 0xdab68a);
    p.oval(x, y - 25, 12, 5, 0xbf8e70);
    p.oval(x, y - 25, 11, 4, 0xe0ba8e);
    crate::glasshouse::glass(p, x + 8, y - 24, 0xffe0aa);
    let (cx, cy) = (x - 5, y - 26);
    p.oval(cx, cy, 5, 2, 0xa57956);
    p.poly(
        &[
            (cx - 4, cy - 8),
            (cx + 4, cy - 8),
            (cx + 3, cy),
            (cx - 3, cy),
        ],
        0xd6a266,
    );
    p.rect(cx - 2, cy - 9, 4, 7, 0xf2c789);
    p.rect(cx - 1, cy - 8, 2, 5, 0xffe5a6);
    p.rect(cx, cy - 12, 1, 4, 0xffe9b0);
    p.dot(cx - 1 + ((now / 500) % 2) as i32, cy - 13, 0xf8cd87);
}
fn citrus(p: &mut Pixels, u: i32, v: i32) {
    let (x, y) = iso(u, v, 0);
    block(
        p,
        u - 5,
        v - 5,
        10,
        10,
        0,
        8,
        [0xc19876, 0x9c7159, 0x815e53],
    );
    block(
        p,
        u - 6,
        v - 6,
        12,
        12,
        7,
        2,
        [0xd1a580, 0xac8468, 0x926c5a],
    );
    tile(p, u - 4, v - 4, 8, 8, 9, 0x5d594b);
    for a in [(-1, 0), (-10, -13), (1, -20), (13, -13)] {
        p.line((x - 1, y - 18), (x + a.0, y - 27 + a.1), 0x6c5949);
        p.line((x, y - 18), (x + 1 + a.0, y - 27 + a.1), 0xa08560);
    }
    // Branches carry faceted clusters of bay leaves and small bitter oranges.
    for i in 0..31 {
        let n = hash(i + 684);
        let theta = (i as f32) * 2.39996;
        let rr = (i as f32 / 31.).sqrt();
        let dx = (theta.cos() * rr * 20.) as i32;
        let dy = (theta.sin() * rr * 17.) as i32;
        let a = x + dx;
        let b = y - 47 + dy;
        let size = 6 + (n % 4) as i32;
        let palette = if dy + dx / 2 < 0 {
            [0x416e61, 0x678769, 0x9ca57b]
        } else {
            [0x365c55, 0x517662, 0x7f916b]
        };
        let pts = [
            (a - size, b),
            (a - size + 2, b - 4),
            (a - 4, b - size + 1),
            (a + 2, b - size),
            (a + size, b - 3),
            (a + size, b + 2),
            (a + 3, b + size - 1),
            (a - 3, b + size),
        ];
        p.poly(&pts, palette[0]);
        p.poly(
            &[
                (a - size + 1, b),
                (a - 3, b - size + 2),
                (a + 1, b - size + 1),
                (a + 4, b - 2),
                (a, b + 2),
            ],
            palette[1],
        );
        for j in 0..3 {
            let f = hash(n.wrapping_add(j));
            let xx = a - 4 + (f % 8) as i32;
            let yy = b - 5 + ((f >> 7) % 9) as i32;
            p.line((xx - 1, yy + 1), (xx + 1, yy - 1), palette[2]);
        }
    }
    for (dx, dy) in [
        (-15, -43),
        (12, -45),
        (-8, -55),
        (1, -35),
        (15, -52),
        (-1, -61),
        (7, -52),
    ] {
        p.oval(x + dx, y + dy, 2, 2, 0xb77a4d);
        p.rect(x + dx - 1, y + dy - 1, 2, 2, 0xe5a359);
        p.dot(x + dx - 1, y + dy - 1, 0xf2bd6c);
    }
}
fn fountain(p: &mut Pixels, x: i32, y: i32, now: u32) {
    p.oval(x + 2, y + 1, 23, 9, 0x827e83);
    p.oval(x, y - 1, 23, 9, 0x737882);
    p.oval(x, y - 4, 24, 10, 0x989a99);
    p.oval(x, y - 6, 24, 10, 0xd2c8af);
    p.oval(x, y - 7, 21, 8, 0x7a9493);
    p.oval(x, y - 7, 19, 6, 0x6b9093);
    for dx in [-16, -10, 9, 14] {
        let yy = y - 7 + (dx % 4);
        let t = (now / 210 + dx.unsigned_abs()) % 5;
        p.rect(
            x + dx - 1,
            yy,
            2 + t as i32,
            1,
            if t < 3 { 0xaac7b9 } else { 0x90b3ae },
        );
    }
    for dx in [-17, -8, 1, 10, 19] {
        p.rect(x + dx, y + 1 - (dx as i32).abs() / 6, 2, 3, 0x9c9e95);
    }
    p.oval(x, y - 7, 7, 3, 0x8e9b94);
    p.poly(
        &[
            (x - 5, y - 7),
            (x - 3, y - 16),
            (x - 5, y - 19),
            (x + 5, y - 19),
            (x + 3, y - 16),
            (x + 5, y - 7),
        ],
        0xb6b9a7,
    );
    p.rect(x - 2, y - 16, 2, 7, 0xd7cdb0);
    p.oval(x, y - 18, 11, 4, 0x919c96);
    p.oval(x, y - 20, 12, 4, 0xe1d5b8);
    p.oval(x, y - 21, 10, 3, 0x92b1a7);
    // Airborne spray rises from a patinated bronze fish, then rings the lower basin.
    for dx in [-1i32, 1] {
        let mut last = (x, y - 29);
        for k in 1..=12 {
            let pt = (x + dx * k, y - 29 - k + (k * k * 28) / 144);
            if k % 3 != 2 || (now / 180) % 3 != 0 {
                p.line(last, pt, if dx < 0 { 0xc1d9c4 } else { 0xa2c6ba });
            }
            last = pt;
        }
        let t = ((now / 180) % 5) as i32;
        p.rect(x + dx * (11 + t / 2), y - 10 + t / 2, 2, 2, 0xe4e6c9);
    }
    p.poly(
        &[
            (x - 5, y - 21),
            (x - 2, y - 25),
            (x - 4, y - 30),
            (x - 1, y - 28),
            (x + 1, y - 33),
            (x + 4, y - 30),
            (x + 3, y - 26),
            (x + 1, y - 24),
            (x + 4, y - 21),
        ],
        0x6f9284,
    );
    p.line((x, y - 28), (x + 2, y - 31), 0xc1c8a1);
    p.dot(x + 2, y - 30, 0x586f64);
}
pub const PROPS: [Cell; 7] = [(7, 1), (6, 2), (1, 7), (7, 6), (3, 7), (4, 7), (5, 7)];
pub fn prop_at(p: &mut Pixels, cell: Cell, now: u32) {
    let (u, v) = (cell.0 * 16 + 8, cell.1 * 16 + 8);
    let (x, y) = iso(u, v, 0);
    match cell {
        (3, 7) | (5, 7) => stools(p, x, y),
        (4, 7) => cafe(p, x, y, now),
        (7, 1) | (1, 7) => lamp(p, x, y, now),
        (6, 2) => citrus(p, u, v),
        _ => fountain(p, x, y, now),
    }
}
pub fn inside_door(p: &mut Pixels) {
    let w = Wall {
        x: 320,
        y: 126,
        dir: 1,
    };
    front_door(p, w, 60, 101, true);
    let (x, y) = w.at(80, 88);
    let wide = font::width("EXIT", 1) + 10;
    p.rect(x - wide / 2, y - 7, wide, 13, 0x3a4741);
    p.rect(x - wide / 2 + 1, y - 6, wide - 2, 11, 0xbc9b71);
    p.rect(x - wide / 2 + 2, y - 5, wide - 4, 9, 0x486358);
    font::draw(
        p,
        x - font::width("EXIT", 1) / 2,
        y - 5,
        "EXIT",
        0xffe0a3,
        1,
    );
    tile(p, 29, 0, 23, 5, 1, 0xc49971);
    for u in 32..50 {
        if u % 2 == 0 {
            p.line(iso(u, 1, 1), iso(u, 4, 1), 0x9c775c);
        }
    }
}
pub fn door_hit(place: Place, x: i32, y: i32) -> bool {
    let (sx, sy, base) = if place == Place::Bar {
        (320, 126, 0)
    } else {
        (198, 187, 61)
    };
    let s = x - sx;
    let h = sy + s.div_euclid(2) - y;
    if (56..=105).contains(&s) && (-4..=85).contains(&h) {
        return true;
    }
    if base == 0 {
        let (cx, cy) = iso(40, 0, 44);
        (x - cx).abs() <= 18 && (y - cy).abs() <= 8
    } else {
        let (cx, cy) = sign_center();
        (x - cx).abs() <= font::width("Session", 2) / 2 + 11 && (y - cy).abs() <= 13
    }
}
pub fn ambient(p: &mut Pixels, now: u32) {
    harbor::waterlight(p, now);
    roof_cat(p, now);
    let (x, y) = iso(9, 16, 66);
    for i in 0..4 {
        let phase = ((now / 220 + i * 5) % 20) as i32;
        let a = x - phase / 2 + (phase / 5 % 2);
        let b = y - 11 - phase;
        if b < 1 {
            continue;
        }
        let c = mix(0x767583, 0x454c65, phase, 20);
        p.rect(a, b, 2 + phase / 7, 1, c);
        if phase < 13 {
            p.dot(a - 1, b + 1, c);
        }
    }
}
