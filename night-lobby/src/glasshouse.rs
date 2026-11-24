use crate::pixels::{Ink, Pixels, Wall, block, harbor, hash, iso, mix, tile};

const BRASS: [Ink; 3] = [0xf6d693, 0xbb8656, 0x785148];
const WALNUT: [Ink; 3] = [0xb07758, 0x855441, 0x603d39];

const JADE: [Ink; 3] = [0x6a997e, 0x426e68, 0x325856];
const VELVET: [Ink; 3] = [0xce806b, 0x9e5653, 0x784249];

pub fn background(p: &mut Pixels) {
    harbor::paint(p, false);
    block(p, 0, 0, 144, 144, -10, 10, [0x765447, 0x493a41, 0x37313c]);
    block(p, 0, 0, 144, 144, -7, 2, [0xb1855a, 0x986c4e, 0x775349]);
    block(p, 0, 0, 144, 144, -4, 3, [0xaa825d, 0x695048, 0x5a4140]);
    parquet(p);
    rug(p);
    for dir in [-1, 1] {
        wall(
            p,
            Wall {
                x: 320,
                y: 126,
                dir,
            },
        );
    }
    crate::square::inside_door(p);
}
fn parquet(p: &mut Pixels) {
    // Alternating twelve-board parquet panels. Joints, scrape marks and reflected
    // lamplight stay within their own pieces of wood.
    let colors = [
        0xb98461, 0xc68f67, 0xac775c, 0xd19a70, 0xbe8867, 0xb27e61, 0xc59470,
    ];
    for a in 0..6 {
        for b in 0..6 {
            let across = (a + b) % 2 == 0;
            for i in 0..6 {
                let (u, v, w, d) = if across {
                    (a * 24, b * 24 + i * 4, 24, 4)
                } else {
                    (a * 24 + i * 4, b * 24, 4, 24)
                };
                let n = hash((a * 73 + b * 19 + i * 7) as u32);
                let c = colors[n as usize % colors.len()];
                tile(p, u, v, w, d, 0, c);
                p.line(iso(u, v, 0), iso(u + w, v, 0), mix(c, 0x513943, 2, 7));
                p.line(iso(u, v, 0), iso(u, v + d, 0), mix(c, 0x6d4c49, 2, 7));
                if n % 3 != 0 {
                    let start = 2 + ((n >> 8) % 12) as i32;
                    let end = start + 3 + (n % 7) as i32;
                    let (from, to) = if across {
                        (iso(u + start, v + 2, 0), iso(u + end, v + 2, 0))
                    } else {
                        (iso(u + 1, v + start, 0), iso(u + 1, v + end, 0))
                    };
                    p.line(from, to, mix(c, 0xf2bc87, 2, 7));
                }
            }
        }
    }
    for e in [1, 3, 141, 143] {
        p.line(iso(e, 1, 0), iso(e, 143, 0), 0x82584d);
        p.line(iso(1, e, 0), iso(143, e, 0), 0x82584d);
    }
}
fn rug(p: &mut Pixels) {
    let (u, v, w, d) = (34, 36, 88, 77);
    for i in (2..d).step_by(2) {
        for j in [u - 2, u + w] {
            p.line(iso(j, v + i, 0), iso(j + 2, v + i, 0), 0xe6c198);
        }
    }
    for (pad, c) in [
        (0, 0x81574e),
        (1, 0xd0aa7f),
        (2, 0x493f4b),
        (3, 0xb98572),
        (4, 0x8b5e59),
        (6, 0xe0ae7e),
        (7, 0x3e5c61),
        (8, 0x547374),
    ] {
        tile(p, u + pad, v + pad, w - 2 * pad, d - 2 * pad, 0, c);
    }
    let diamond = |p: &mut Pixels, a, b, r, s, c| {
        p.poly(
            &[
                iso(a - r, b, 0),
                iso(a, b - s, 0),
                iso(a + r, b, 0),
                iso(a, b + s, 0),
            ],
            c,
        )
    };
    for a in (u + 10..u + w - 8).step_by(7) {
        for b in [v + 4, v + d - 4] {
            diamond(p, a, b, 2, 1, 0xf1cd91);
        }
    }
    for b in (v + 9..v + d - 6).step_by(7) {
        for a in [u + 4, u + w - 4] {
            diamond(p, a, b, 1, 2, 0xe6bd88);
        }
    }
    for a in (u + 12..u + w - 8).step_by(10) {
        for b in (v + 13..v + d - 8).step_by(10) {
            let t = if ((a + b) / 10) % 2 == 1 {
                0x73918a
            } else {
                0x627f7b
            };
            diamond(p, a, b, 2, 2, t);
            diamond(p, a, b, 1, 1, 0xb3aa8f);
        }
    }
    for (r, s, c) in [
        (29, 24, 0x3c595f),
        (26, 22, 0xc29778),
        (24, 19, 0x805a59),
        (21, 16, 0x9b695d),
        (17, 13, 0xd5ad7c),
        (14, 10, 0x405d62),
        (9, 7, 0x729287),
        (5, 4, 0xd8b78b),
        (2, 2, 0x926d5c),
    ] {
        diamond(p, 78, 75, r, s, c);
    }
    for sx in [-1, 1] {
        for sy in [-1, 1] {
            for (du, dv, r, c) in [
                (17, 10, 3, 0xc29a77),
                (20, 10, 2, 0x9a7764),
                (17, 13, 2, 0x97a08a),
                (10, 17, 2, 0xc89876),
            ] {
                diamond(p, 78 + sx * du, 75 + sy * dv, r, r, c);
            }
        }
    }
    for (a, b) in [(52, 54), (104, 54), (52, 96), (104, 96)] {
        for (dx, dy) in [(-3, 0), (3, 0), (0, -3), (0, 3)] {
            diamond(p, a + dx, b + dy, 2, 2, 0xb49d80);
        }
        diamond(p, a, b, 2, 2, 0xe0bc86);
    }
}
fn wall(p: &mut Pixels, w: Wall) {
    let left = w.dir < 0;
    w.pane(p, 0, 289, 0, 102, if left { 0x466a63 } else { 0x3b5b59 });
    // Small gold fan figures were block printed onto the indigo wallpaper.
    for s in (3..286).step_by(12) {
        for h in (41..97).step_by(14) {
            let a = s + if h % 4 == 1 { 0 } else { 6 };
            if a >= 286 {
                continue;
            }
            for dx in [-3, 0, 3] {
                w.stroke(
                    p,
                    (a, h),
                    (a + dx, h + 3),
                    if left { 0x658379 } else { 0x536f69 },
                );
            }
            w.dot(p, a, h - 2, 0x798d7c);
        }
    }
    w.pane(p, 0, 289, 0, 36, if left { 0x673f3a } else { 0x523739 });
    for s in (2..282).step_by(21) {
        w.pane(p, s, s + 18, 5, 29, 0x995f46);
        w.pane(p, s + 1, s + 17, 6, 29, 0x583c39);
        w.pane(
            p,
            s + 3,
            s + 15,
            8,
            26,
            if left { 0x80513f } else { 0x724a3f },
        );
        w.pane(p, s + 3, s + 4, 9, 26, 0xa66949);
    }
    for (lo, hi, c) in [
        (0, 3, 0x403537),
        (3, 5, 0xad7851),
        (28, 30, 0x5e403b),
        (30, 32, 0xd29665),
        (32, 36, 0x865742),
        (36, 37, 0xcfad76),
        (96, 99, 0x263f43),
        (99, 101, 0xa28865),
        (101, 105, 0x40554e),
        (105, 107, 0xd2b381),
    ] {
        w.pane(p, 0, 289, lo, hi, c);
    }
    if left {
        for (i, s) in [13, 101, 189].into_iter().enumerate() {
            window(p, w, s, i as i32);
        }
    } else {
        bottle_shelf(p, w, 133, 270);
        // An old brass hotel clock in the narrow bay by the door.
        let (x, y) = w.at(31, 73);
        p.oval(x, y + 1, 13, 14, 0x293e40);
        p.oval(x, y, 12, 13, 0xa57e57);
        p.oval(x, y, 10, 11, 0x5b5549);
        p.oval(x, y, 9, 10, 0xe6cda1);
        for (dx, dy) in [
            (0, -8),
            (7, 0),
            (0, 8),
            (-7, 0),
            (-5, -6),
            (5, 6),
            (5, -6),
            (-5, 6),
        ] {
            p.dot(x + dx, y + dy, 0x765e4d);
        }
        p.line((x, y), (x - 4, y - 2), 0x574b44);
        p.line((x, y), (x + 1, y - 6), 0x574b44);
        p.dot(x, y, 0xac7450);
    }
    let posts: &[i32] = if left {
        &[0, 92, 180, 280]
    } else {
        &[0, 119, 279]
    };
    for &s in posts {
        for (a, b, lo, hi, c) in [
            (0, 8, 0, 99, 0x304746),
            (1, 6, 4, 94, 0x667763),
            (2, 3, 7, 90, 0xa19e78),
            (-1, 9, 91, 95, 0x9a8767),
            (-2, 10, 95, 97, 0xd4b282),
            (-1, 8, 4, 6, 0xb08c66),
        ] {
            w.pane(p, s + a, s + b, lo, hi, c);
        }
    }
}
fn window(p: &mut Pixels, w: Wall, a: i32, idx: i32) {
    const WIDTH: i32 = 70;
    for i in -3..WIDTH + 3 {
        let dx = (i - 34) as f32 / 38.;
        let arch = (19. * (1. - dx * dx).max(0.).sqrt()) as i32;
        let top = 76 + arch;
        for h in 39..=top {
            let c = if i < 0 || i >= WIDTH || h < 42 || h > top - 3 {
                0x263d42
            } else if i == 0 || i == WIDTH - 1 || h == top - 3 {
                0xc4af82
            } else if i < 3 || i > WIDTH - 4 || h < 45 || h >= top - 5 {
                0x766b5c
            } else {
                let s = idx * WIDTH + i;
                let mut c = mix(0xf1ac83, 0x506485, ((h - 43) / 4) * 4, 50);
                // Mountains, tall cypresses and a crescent seen through colored panes.
                let far =
                    56 + ((s as f32 * 0.063).sin() * 4. + (s as f32 * 0.14).cos() * 3.) as i32;
                if h < far {
                    c = 0x797488;
                }
                let near = 50 + ((s as f32 * 0.12).sin() * 3.) as i32;
                if h < near {
                    c = 0x466775;
                }
                if idx == 1
                    && (i - 36).pow(2) + (h - 74).pow(2) < 44
                    && (i - 39).pow(2) + (h - 76).pow(2) > 24
                {
                    c = 0xffe3ab;
                }
                if (s * 71 + h * 19) % 207 == 0 && h > 61 {
                    c = 0xe8d4c2;
                }
                if ((i - 9).abs() <= ((65 - h) / 5).clamp(0, 4) && (49..65).contains(&h))
                    || ((i - 56).abs() <= ((61 - h) / 5).clamp(0, 3) && (49..61).contains(&h))
                {
                    c = 0x355c62;
                }
                if h == 64 || [22, 46].contains(&i) || (h > 74 && (i - 34).abs() == (h - 74) * 2) {
                    c = 0x47565d;
                }
                if h == 65 || [23, 47].contains(&i) {
                    c = 0xb2a79a;
                }
                c
            };
            w.dot(p, a + i, h, c);
        }
    }
    w.pane(p, a - 4, a + 74, 37, 40, 0x523e39);
    w.pane(p, a - 5, a + 75, 40, 43, 0xc4a07b);
}
fn bottle_shelf(p: &mut Pixels, w: Wall, a: i32, b: i32) {
    w.pane(p, a - 5, b + 4, 36, 96, 0x263d41);
    w.pane(p, a - 2, b + 2, 39, 94, 0xa17f57);
    w.pane(p, a, b, 40, 93, 0x455f5c);
    for s in a + 1..b {
        let dx = (s - (a + b) / 2).abs();
        let ht = 83 + (12. * (1. - (dx as f32 / 75.).powi(2)).max(0.).sqrt()) as i32;
        w.pane(p, s, s + 1, 40, ht, 0x758a79);
        if (s - a) % 41 < 3 {
            w.pane(p, s, s + 1, 42, ht, 0x54695f);
        }
        if (s - a) % 41 == 3 {
            w.pane(p, s, s + 1, 42, ht, 0xa4aa8b);
        }
        for h in 46..ht - 1 {
            if (s - a + h * 2) % 94 < 4 {
                w.dot(p, s, h, 0x8fa18a);
            }
        }
    }
    for (row, z) in [(0, 44), (1, 66)] {
        for i in 0..12 {
            let s = a + 5 + i * 10 + (i / 4) * 3;
            let n = hash((i * 43 + row * 263) as u32);
            let (x, y) = w.at(s, z);
            if i % 5 == 3 {
                for dx in [0, 5] {
                    glass(p, x + dx, y, 0xe9d9b1);
                }
            } else {
                bottle(
                    p,
                    x,
                    y,
                    8 + ((n >> 6) % 9) as i32,
                    2 + (n % 2) as i32,
                    n as usize,
                );
            }
        }
        w.pane(p, a - 3, b + 3, z - 4, z - 1, 0x4b3837);
        w.pane(p, a - 4, b + 4, z - 1, z + 1, 0xc59c70);
    }
}
pub fn bottle(p: &mut Pixels, x: i32, y: i32, h: i32, r: i32, i: usize) {
    let (dark, body, shine) = [
        (0x304b44, 0x547952, 0x9fac70),
        (0x52413f, 0x9b6546, 0xe1a96d),
        (0x45595a, 0x66918a, 0xb6c9ae),
        (0x543f4b, 0x8f5762, 0xc78b88),
        (0x3e5141, 0x718349, 0xc5ba7a),
    ][i % 5];
    p.rect(x - 1, y - h, 3, 5, dark);
    p.rect(x - 1, y - h, 3, 2, 0xe9bd7d);
    p.rect(x, y - h + 2, 1, 3, shine);
    p.rect(x - r + 1, y - h + 4, 2 * r - 1, 3, body);
    p.rect(x - r, y - h + 6, 2 * r + 1, h - 6, dark);
    p.rect(x - r + 1, y - h + 5, 2 * r - 2, h - 6, body);
    p.rect(x - r + 1, y - h + 6, 1, (h / 3).max(1), shine);
    if h >= 10 {
        p.rect(x - r + 1, y - 5, 2 * r - 1, 3, 0xe1c995);
        p.dot(x, y - 4, 0x986d55);
    }
}
pub fn glass(p: &mut Pixels, x: i32, y: i32, c: Ink) {
    p.line((x - 2, y - 9), (x + 2, y - 9), c);
    p.rect(x - 2, y - 8, 1, 3, c);
    p.rect(x + 2, y - 8, 1, 3, 0xbaaa91);
    p.rect(x - 1, y - 6, 3, 2, 0xcb9c72);
    p.rect(x, y - 4, 1, 4, c);
    p.rect(x - 2, y, 5, 1, c);
}
#[derive(Clone, Copy)]
pub enum Furniture {
    Sofa,
    Chair,
    Table,
    RecordCabinet,
    Palm,
    Fern,
    Lamp,
    Flowers,
    Bookshelf,
}
pub const FURNITURE: &[(Furniture, i32, i32, i32)] = &[
    (Furniture::Sofa, 5, 31, 79),
    (Furniture::Chair, 82, 90, 173),
    (Furniture::Table, 50, 46, 121),
    (Furniture::RecordCabinet, 63, 3, 97),
    (Furniture::Palm, 15, 12, 27),
    (Furniture::Fern, 10, 133, 143),
    (Furniture::Lamp, 15, 95, 110),
    (Furniture::Flowers, 132, 12, 144),
    (Furniture::Bookshelf, 4, 106, 131),
];
pub fn seat(u: i32, v: i32) -> bool {
    (u == 1 && (2..=4).contains(&v)) || (u == 5 && v == 6)
}
pub fn walkable(u: i32, v: i32) -> bool {
    (0..9).contains(&u)
        && (0..9).contains(&v)
        && (seat(u, v)
            || !matches!(
                (u, v),
                (0, 0..=8) | (1, 0) | (4..=8, 0) | (3..=4, 3) | (5, 5)
            ))
}

fn piped(
    p: &mut Pixels,
    u: i32,
    v: i32,
    w: i32,
    d: i32,
    z: i32,
    h: i32,
    palette: [Ink; 3],
    seam: Ink,
) {
    block(p, u, v, w, d, z, h, palette);
    p.line(iso(u, v + d, z + h), iso(u + w, v + d, z + h), seam);
    p.line(
        iso(u + w, v, z + h),
        iso(u + w, v + d, z + h),
        mix(palette[0], seam, 1, 2),
    );
}
fn settle(p: &mut Pixels, u: i32, v: i32, w: i32, d: i32) {
    let (cx, cy) = iso(u + w / 2 + 2, v + d / 2, 0);
    for y in cy - (w + d) / 2 - 2..=cy + (w + d) / 2 + 3 {
        for x in cx - w - d - 3..=cx + w + d + 3 {
            let dx = x - cx;
            let dy = y - cy;
            let du = (2 * dy + dx).abs();
            let dv = (2 * dy - dx).abs();
            if du < 2 * w + 6 && dv < 2 * d + 6 {
                let soft = du >= 2 * w || dv >= 2 * d;
                p.tint(x, y, 0x533d42, if soft { 1 } else { 3 }, 8);
            }
        }
    }
}
fn chesterfield(p: &mut Pixels, u: i32, v: i32) {
    settle(p, u, v, 17, 46);
    for a in [2, 17] {
        for b in [2, 45] {
            block(p, u + a, v + b, 2, 2, 0, 5, WALNUT);
        }
    }
    piped(p, u, v, 20, 47, 4, 3, WALNUT, 0xc88a63);
    piped(p, u, v, 20, 47, 7, 4, VELVET, 0xd88a74);
    block(p, u, v, 5, 47, 9, 10, VELVET);
    for t in 0..92 {
        // Rolled back, with a shallow scallop at every upholstery button.
        let (x, y) = iso(u + 3, v, 19);
        let yy = y + t / 2;
        for (off, c) in [
            (-4, 0x73454a),
            (-3, 0xb77a65),
            (-2, 0xe19a76),
            (-1, 0xe8a37c),
            (0, 0xce836d),
            (1, 0xb57265),
            (2, 0xb57265),
            (3, 0x935b57),
        ] {
            p.dot(x - t, yy + off, c);
        }
    }
    for b in [1, 16, 31] {
        piped(
            p,
            u + 4,
            v + b,
            15,
            14,
            10,
            3,
            [0xd38b72, 0xb26a5d, 0x995950],
            0xf0ae83,
        );
        for (off, len, c) in [(2, 8, 0xda9175), (4, 4, 0xd99175), (10, 3, 0xc47b67)] {
            p.line(
                iso(u + 5, v + b + off, 13),
                iso(u + 5 + len, v + b + off, 13),
                c,
            );
        }
        let (x, y) = iso(u + 13, v + b + 7, 13);
        p.line((x - 4, y - 1), (x - 1, y), 0xc27c67);
        p.dot(x, y + 1, 0xb77061);
        p.line((x + 1, y + 1), (x + 3, y), 0xe49b7b);
        for g in [3, 10] {
            let (x, y) = iso(u + 5, v + b + g, 16);
            p.line((x - 4, y - 2), (x, y + 1), 0xa15f58);
            p.line((x, y + 1), (x + 4, y), 0xa36158);
            p.rect(x - 1, y, 2, 2, 0x925950);
            p.dot(x, y + 2, 0xf3ac82);
        }
    }
    for b in [0, 45] {
        piped(p, u + 2, v + b, 19, 3, 8, 7, VELVET, 0xf0b08a);
        let (x, y) = iso(u + 20, v + b + 2, 15);
        p.oval(x, y, 4, 4, 0x874c4e);
        p.oval(x, y - 1, 3, 3, 0xc07864);
        p.dot(x - 1, y - 2, 0xf0ac7f);
    }
    for b in (2..45).step_by(4) {
        let (x, y) = iso(u + 20, v + b, 6);
        p.rect(x, y, 2, 2, 0xca9167);
        p.dot(x, y, 0xe1b27b);
    }
    // A small embroidered pillow.
    block(p, u + 5, v + 3, 4, 6, 13, 5, [0xe6bf7d, 0xca925c, 0xa66d4f]);
    let cushion = Wall {
        x: iso(u + 9, v + 3, 0).0,
        y: iso(u + 9, v + 3, 0).1,
        dir: -1,
    };
    for s in [1, 4, 7, 10] {
        cushion.dot(p, s, 32, 0xab7c50);
        cushion.dot(p, s + 1, 33, 0xf6d69a);
    }
    let (x, y) = iso(u + 9, v + 6, 16);
    p.line((x - 3, y), (x, y + 2), 0xffdf92);
    p.line((x, y + 2), (x + 3, y), 0xffdf92);
}
fn armchair(p: &mut Pixels, u: i32, v: i32) {
    settle(p, u, v, 14, 15);
    for a in [2, 13] {
        for b in [1, 14] {
            block(p, u + a, v + b, 2, 2, 0, 5, WALNUT);
        }
    }
    piped(p, u, v, 17, 16, 4, 4, WALNUT, 0xc1976f);
    piped(p, u, v, 17, 16, 8, 4, JADE, 0x93aa88);
    piped(p, u, v - 1, 17, 4, 10, 12, JADE, 0xa8ba96);
    for a in [3, 8, 13] {
        let (x, y) = iso(u + a, v + 3, 18);
        p.rect(x - 1, y - 1, 3, 2, 0x3e635c);
        p.dot(x, y + 1, 0x91aa88);
    }
    piped(
        p,
        u + 2,
        v + 3,
        13,
        12,
        12,
        2,
        [0x80a38a, 0x5e8475, 0x537b70],
        0xb1bb96,
    );
    for a in [0, 15] {
        piped(p, u + a, v + 2, 3, 15, 8, 7, JADE, 0xadba8e);
    }
}
fn counter(p: &mut Pixels, u: i32, v: i32) {
    let len = 55;
    block(p, u, v, len, 14, 0, 3, WALNUT);
    block(p, u, v, len, 14, 3, 16, [0xa67251, 0x8c5841, 0x60443c]);
    let (x, y) = iso(u, v + 14, 0);
    let wall = Wall { x, y, dir: 1 };
    for s in (3..104).step_by(20) {
        wall.pane(p, s, s + 17, 9, 34, 0x503937);
        wall.pane(p, s + 2, s + 16, 10, 33, 0xc18a5d);
        wall.pane(p, s + 3, s + 14, 12, 31, 0x966145);
        wall.pane(p, s + 6, s + 12, 15, 28, 0xaa704d);
    }
    piped(
        p,
        u - 2,
        v - 1,
        len + 4,
        17,
        18,
        3,
        [0xc99d78, 0xa87554, 0x79523f],
        0xffd2a0,
    );
    tile(p, u - 1, v, len + 2, 14, 21, 0xb88869);
    for a in 0..23 {
        let n = hash(a + 788);
        let aa = u + (n % 51) as i32;
        let bb = v + ((n >> 10) % 14) as i32;
        p.line(iso(aa, bb, 21), iso(aa + 2, bb + 1, 21), 0xd3ac8d);
    }
    let (tx, ty) = iso(u + 15, v + 7, 21);
    p.oval(tx, ty, 12, 5, 0x9b795e);
    p.oval(tx, ty - 1, 11, 4, 0xe5bd81);
    for dx in [-6, 2] {
        glass(p, tx + dx, ty - 1, 0xffeac0);
    }
    let (x, y) = iso(u + 32, v + 7, 21);
    p.oval(x + 2, y, 11, 4, 0x7d6150);
    p.oval(x, y - 1, 7, 3, 0xc59159);
    p.rect(x - 2, y - 20, 5, 19, 0x916644);
    p.rect(x - 1, y - 19, 2, 16, 0xf1c682);
    p.oval(x, y - 21, 4, 4, 0xc18b59);
    p.oval(x - 1, y - 22, 2, 2, 0xf5d991);
    for dx in [-8, 8] {
        p.rect(x + dx.min(0), y - 18, 9, 3, 0xae8156);
        p.rect(x + dx, y - 21, 2, 10, 0xe5bf81);
        p.rect(x + dx - 1, y - 24, 4, 5, 0x4a4941);
        p.dot(x + dx, y - 23, 0xa4a287);
    }
    let (x, y) = iso(u + 47, v + 5, 21);
    bottle(p, x, y, 16, 3, 1);
    bottle(p, x + 9, y + 4, 12, 2, 0);
    // Brass foot rail wrapping the front of the mahogany bar.
    for a in [6, 26, 50] {
        block(p, u + a, v + 14, 1, 3, 2, 2, BRASS);
    }
    p.line(iso(u, v + 17, 4), iso(u + len, v + 17, 4), 0x805747);
    p.line(iso(u, v + 17, 5), iso(u + len, v + 17, 5), 0xe1b67b);
}
fn coffee_table(p: &mut Pixels, u: i32, v: i32, now: u32) {
    for a in [2, 22] {
        for b in [2, 12] {
            block(p, u + a, v + b, 2, 2, 0, 8, WALNUT);
            block(p, u + a, v + b, 2, 2, 1, 1, BRASS);
        }
    }
    piped(p, u, v, 26, 15, 8, 2, WALNUT, 0xe6b78b);
    tile(p, u + 1, v + 1, 24, 13, 10, 0xc4936f);
    for (a, b, c) in [(3, 3, 0x56756c), (4, 3, 0x91545a)] {
        let z = if a == 3 { 10 } else { 12 };
        block(p, u + a, v + b, 8, 6, z, 2, [c, 0xdfc59c, 0x7d665a]);
        p.line(
            iso(u + a + 1, v + b + 1, z + 2),
            iso(u + a + 7, v + b + 1, z + 2),
            0xdbb77a,
        );
    }
    let (x, y) = iso(u + 18, v + 8, 10);
    p.oval(x, y, 8, 3, 0x9a715e);
    p.oval(x, y - 1, 7, 3, 0xf0d3a5);
    p.oval(x, y - 4, 4, 4, 0xecc9a0);
    p.rect(x - 4, y - 7, 9, 4, 0xecc9a0);
    p.oval(x, y - 7, 4, 2, 0xf8e3ba);
    p.oval(x, y - 7, 3, 1, 0x754847);
    p.line((x + 4, y - 6), (x + 7, y - 5), 0xf5d9aa);
    p.line((x + 7, y - 5), (x + 5, y - 2), 0xf5d9aa);
    let phase = (now / 320 % 6) as i32;
    for i in 0..2 {
        let dy = (phase + i * 3) % 6;
        p.line(
            (x - 2 + i * 3, y - 10 - dy),
            (x - 1 + i * 3, y - 12 - dy),
            0xd8bc99,
        );
    }
}
pub fn pot(p: &mut Pixels, x: i32, y: i32, r: i32) {
    let h = r + 4;
    p.oval(x + 1, y, r - 2, 3, 0x645346);
    p.poly(
        &[
            (x - r, y - h),
            (x + r, y - h),
            (x + r - 3, y - 2),
            (x + r - 6, y),
            (x - r + 6, y),
            (x - r + 3, y - 2),
        ],
        0x925a48,
    );
    p.poly(
        &[
            (x - r + 2, y - h),
            (x + 1, y - h),
            (x + 1, y - 2),
            (x - r + 4, y - 3),
        ],
        0xc17a57,
    );
    p.rect(x - r + 4, y - h + 3, 2, h - 7, 0xd78e66);
    p.oval(x, y - h, r + 1, 4, 0x805043);
    p.oval(x, y - h - 1, r + 1, 4, 0xd39369);
    p.oval(x, y - h - 1, r - 2, 2, 0x53433d);
}
fn fern(p: &mut Pixels, x: i32, y: i32, tall: bool) {
    pot(p, x, y, if tall { 11 } else { 10 });
    let root = (x, y - 15);
    let length = if tall { 29 } else { 17 };
    let height = if tall { 45 } else { 22 };
    for i in 0..13 {
        let s = if i % 2 == 0 { -1 } else { 1 };
        let spread = (i * 11 % 17 + 9) * length / 24;
        let rise = (15 + i * 13 % 17) * height / 28;
        let colors = if i < 4 {
            [0x254c47, 0x3e6756, 0x55765b]
        } else if i % 3 == 0 {
            [0x35594c, 0x6d8e63, 0xa0ad77]
        } else {
            [0x2b524b, 0x527d60, 0x809a68]
        };
        let mut last = root;
        for k in 1..=16 {
            let f = k as f32 / 16.;
            let xx = x + s * (spread as f32 * f) as i32;
            let yy = root.1 - (rise as f32 * (f * 2.5 - f * f * 1.8)) as i32;
            p.line(last, (xx, yy), colors[1]);
            if k % 2 == 0 && k < 16 {
                let l = (6. * (std::f32::consts::PI * f).sin()) as i32 + 1;
                p.poly(
                    &[(xx, yy), (xx - s * 2, yy - 1), (xx + s * l, yy - l - 1)],
                    colors[2],
                );
                p.poly(
                    &[(xx, yy), (xx + s * 2, yy), (xx + s * (l + 1), yy + l)],
                    colors[0],
                );
            }
            last = (xx, yy);
        }
    }
}
fn flowers(p: &mut Pixels, x: i32, y: i32) {
    pot(p, x, y, 9);
    for i in 0..14 {
        let n = hash(i + 432);
        let dx = (n % 31) as i32 - 15;
        let dy = 20 + ((n >> 9) % 24) as i32;
        p.line((x, y - 12), (x + dx, y - dy), 0x587353);
        p.poly(
            &[
                (x + dx / 2, y - 17),
                (x + dx / 2 - 5, y - 24),
                (x + dx / 2 + 2, y - 20),
            ],
            0x739363,
        );
        if i % 3 != 0 {
            let (a, b) = (x + dx, y - dy);
            for (s, t) in [(-2, 0), (2, 0), (-1, -2), (1, 2)] {
                p.rect(
                    a + s - 1,
                    b + t - 1,
                    3,
                    2,
                    if i % 2 == 0 { 0xe7b4a0 } else { 0xca807f },
                );
            }
            p.dot(a, b, 0xffdc85);
        }
    }
}
fn lamp(p: &mut Pixels, x: i32, y: i32) {
    p.oval(x + 2, y, 11, 4, 0x735a49);
    p.oval(x, y - 1, 10, 4, 0x9d7856);
    p.oval(x, y - 3, 7, 3, 0xd9b57c);
    p.rect(x - 2, y - 56, 5, 52, 0x705a45);
    p.rect(x - 1, y - 55, 2, 48, 0xf3cd8b);
    for h in [9, 12, 46, 49] {
        p.oval(x, y - h, 3, 2, 0xb79863);
    }
    p.poly(
        &[
            (x - 8, y - 72),
            (x + 8, y - 72),
            (x + 16, y - 54),
            (x + 13, y - 51),
            (x - 13, y - 51),
            (x - 16, y - 54),
        ],
        0x986e4d,
    );
    for dx in -15i32..=15 {
        let h = if dx.abs() < 8 {
            19
        } else {
            (16 - dx.abs()) * 2 + 3
        };
        let top = y - 53 - h;
        let c = if dx.abs() > 12 {
            0xd99659
        } else if dx < 0 {
            0xf3c47b
        } else {
            0xe9b16a
        };
        p.rect(x + dx, top, 1, h + (dx / 4).abs() % 2, c);
        if dx % 5 == 0 {
            p.rect(x + dx, top + 1, 1, h - 1, 0xddaa69);
        }
    }
    p.oval(x, y - 72, 8, 2, 0xc69561);
    p.rect(x - 1, y - 76, 3, 4, 0xc7a271);
    p.line((x + 7, y - 52), (x + 7, y - 42), 0xe4bc7d);
    p.oval(x + 7, y - 41, 1, 2, 0xf7cf87);
}
fn gramophone(p: &mut Pixels, u: i32, v: i32) {
    for a in [1, 11] {
        for b in [1, 17] {
            block(p, u + a, v + b, 2, 2, 0, 4, WALNUT);
        }
    }
    piped(p, u, v, 14, 19, 4, 14, WALNUT, 0xc39068);
    piped(p, u - 1, v - 1, 16, 21, 17, 2, WALNUT, 0xecbd8a);
    for i in 0..12 {
        let (x, y) = iso(u + 14, v + 2 + i, 16);
        p.rect(
            x,
            y,
            2,
            18,
            [0x754843, 0xa48060, 0x415b5a, 0xbe966b, 0x824e4f][i as usize % 5],
        );
    }
    let (x, y) = iso(u + 7, v + 8, 19);
    p.oval(x, y, 10, 5, 0x403a3d);
    p.oval(x, y - 1, 7, 3, 0x565156);
    p.oval(x, y - 1, 2, 1, 0xe9b472);
    p.line((x + 9, y - 4), (x + 2, y - 1), 0xe2b777);
    p.line((x + 8, y - 2), (x + 11, y - 7), 0x8e724e);
    p.line((x + 11, y - 7), (x + 6, y - 16), 0xd6a161);
    p.poly(
        &[
            (x + 7, y - 10),
            (x + 10, y - 17),
            (x + 3, y - 25),
            (x - 10, y - 32),
            (x - 14, y - 17),
        ],
        0xc98d4f,
    );
    p.poly(
        &[
            (x + 8, y - 12),
            (x + 2, y - 20),
            (x - 9, y - 24),
            (x - 10, y - 17),
        ],
        0xe8b769,
    );
    let mouth = [
        (x - 17, y - 31),
        (x - 12, y - 35),
        (x - 5, y - 32),
        (x - 3, y - 24),
        (x - 6, y - 16),
        (x - 13, y - 13),
        (x - 19, y - 18),
        (x - 20, y - 25),
    ];
    p.poly(&mouth, 0xffd88a);
    for (n, (&a, &b)) in mouth.iter().zip(mouth.iter().cycle().skip(1)).enumerate() {
        let inner = |q: (i32, i32)| ((q.0 * 5 + (x - 11)) / 6, (q.1 * 5 + (y - 24)) / 6);
        p.poly(
            &[inner(a), inner(b), (x - 9, y - 22)],
            [
                0x98623f, 0xb7804b, 0xda9f5a, 0xeeb96c, 0xd69a56, 0xb07348, 0xaa6b42, 0x885a40,
            ][n],
        );
    }
    p.oval(x - 9, y - 22, 2, 2, 0x795239);
}
pub fn furniture_at(p: &mut Pixels, kind: Furniture, u: i32, v: i32, now: u32) {
    let (x, y) = iso(u, v, 0);
    match kind {
        Furniture::Sofa => chesterfield(p, u, v),
        Furniture::Chair => armchair(p, u, v),
        Furniture::Table => coffee_table(p, u, v, now),
        Furniture::RecordCabinet => counter(p, u, v),
        Furniture::Palm => fern(p, x, y, true),
        Furniture::Fern => fern(p, x, y, false),
        Furniture::Lamp => lamp(p, x, y),
        Furniture::Flowers => flowers(p, x, y),
        Furniture::Bookshelf => gramophone(p, u, v),
    }
}
pub fn ambient(p: &mut Pixels, now: u32) {
    for i in 0..9 {
        let n = hash(i + 359);
        let phase = (now / 300 + i * 3) % 18;
        if phase < 4 {
            let x = 157 + (n % 45) as i32;
            let y = 173 + ((n >> 8) % 51) as i32 - (phase / 2) as i32;
            p.dot(x, y, if phase == 1 { 0xe0c18b } else { 0xb99c76 });
        }
    }
}
