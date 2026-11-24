// Pixel coordinates are relative to a ground contact. Positive local x is the
// direction of travel. The rear drawings look up the street, the front down it.
use super::{Guest, LOOKS};
use crate::pixels::{Ink, Pixels, Point, mix};
use crate::plaza::Place;

#[derive(Clone, Copy)]
struct Inks {
    line: Ink,
    hair: [Ink; 3],
    skin: [Ink; 5],
    coat: [Ink; 4],
    scarf: [Ink; 2],
}

impl Inks {
    fn new(g: &Guest) -> Self {
        let (_, cloth, shade) = LOOKS[g.look as usize];
        let hair = [
            [0x463b42, 0x715046, 0x9c7057],
            [0x68493f, 0x9b6549, 0xc78b59],
            [0x54404a, 0x805762, 0xad7a76],
            [0x875741, 0xb98555, 0xe2b374],
        ][(g.face as usize / 4) % 4];
        let skin = [
            [0xf9ce9f, 0xe7b18c, 0xc78e73, 0xe19487, 0x975f58],
            [0xedbd8d, 0xd5a17c, 0xb77f65, 0xcf8678, 0x8f5a4c],
            [0xdaab81, 0xbb896a, 0x9d7059, 0xbd7d70, 0x784d44],
            [0xbe956f, 0xa27558, 0x865d4c, 0xae6d61, 0x67453f],
        ][g.face as usize % 4];
        let scarf = [
            [0xab5b50, 0xe3946f],
            [0xa26047, 0xe1a36c],
            [0x536d70, 0x95aba0],
            [0x586880, 0xa8afbd],
            [0x935851, 0xd59573],
            [0x7f5b72, 0xc89ba0],
        ][g.look as usize];
        Self {
            line: 0x302e39,
            hair,
            skin,
            coat: [
                mix(shade, 0x3c3642, 1, 3),
                shade,
                cloth,
                mix(cloth, 0xffe4ad, 2, 5),
            ],
            scarf,
        }
    }
    fn ink(self, key: u8) -> Ink {
        match key {
            b'k' => self.line,
            b'h' => self.hair[0],
            b'H' => self.hair[1],
            b't' => self.hair[2],
            b'l' => self.skin[0],
            b's' => self.skin[1],
            b'S' => self.skin[2],
            b'r' => self.skin[3],
            b'm' => self.skin[4],
            b'v' => mix(self.skin[4], self.line, 1, 3),
            b'e' => 0x3e303b,
            b'i' => 0x604248,
            b'I' => 0x9b7059,
            b'g' => 0x775747,
            b'G' => 0xb49770,
            b'd' => self.coat[0],
            b'j' => self.coat[1],
            b'J' => self.coat[2],
            b'A' => self.coat[3],
            b'c' => self.scarf[0],
            b'C' => self.scarf[1],
            b'w' => 0xffe5bd,
            b'z' => 0xe4bb79,
            b'p' => 0x586778,
            b'P' => 0x7b8894,
            b'D' => 0x414c5c,
            b'B' => 0x92674e,
            b'b' => 0x624c45,
            b'T' => 0xc5986f,
            _ => unreachable!(),
        }
    }
}
struct Pen<'a> {
    p: &'a mut Pixels,
    x: i32,
    y: i32,
    mirror: i32,
    inks: Inks,
}
impl Pen<'_> {
    fn pos(&self, (x, y): Point) -> Point {
        (self.x + self.mirror * x, self.y + y)
    }
    fn rect(&mut self, x: i32, y: i32, w: i32, h: i32, c: Ink) {
        let left = if self.mirror > 0 { x } else { -x - w + 1 };
        self.p.rect(self.x + left, self.y + y, w, h, c);
    }
    fn line(&mut self, a: Point, b: Point, c: Ink) {
        self.p.line(self.pos(a), self.pos(b), c);
    }
    fn shape(&mut self, pts: &[Point], c: Ink) {
        let mut screen = [(0, 0); 8];
        for (s, &q) in screen.iter_mut().zip(pts) {
            *s = self.pos(q);
        }
        self.p.poly(&screen[..pts.len()], c);
    }
    fn stamp(&mut self, x: i32, y: i32, rows: &[&[u8]]) {
        for (dy, row) in rows.iter().enumerate() {
            for (dx, &cell) in row.iter().enumerate() {
                if cell != b'.' {
                    self.rect(x + dx as i32, y + dy as i32, 1, 1, self.inks.ink(cell));
                }
            }
        }
    }
}

// Paint the features on one cheek plane. The two hairstyles share the same
// eye line so a blink or an accessory stays registered through each pose.
const CHEEKS: &[&[u8]] = &[
    b".......SSsssssS",
    b".....SSssslllllsS",
    b"....SSssllllllllssS",
    b"...vSSssllllllllssv",
    b"..vSlsSssslllllllsv",
    b"..vslsSsslllllllslsv",
    b"..vSrSSssllllllsssSv",
    b"...vSSsrrssllslllssv",
    b"....vSSrrrsssllsrrSv",
    b"....vSSsrrssssllssSv",
    b".....vSSsssssssssSv",
    b"......vSSsssssssSv",
    b"........vSSssssSv",
    b"..........vSSSvv",
];
const FRINGE: &[&[u8]] = &[
    b"......kkhhhkkk",
    b"...kkHHttttHHhk",
    b"..kHttttHHHHHHHhk",
    b".kHHttHHHHhHttHHhk",
    b".kHHHHHHHhhhHtttHHk",
    b"khHHHHHhhhhhHHHHHHk",
    b"khHHhhHhtSssShHHHhk",
    b"khHhhHHhS.....ShHh",
    b"khHhhHhS........Sh",
    b".khhHHh..........S",
    b".khhHh",
    b".khhH",
    b"..khh",
    b"..khh",
    b"...khh",
    b"...kh",
];
const CURLS: &[&[u8]] = &[
    b"......kkHHHkk",
    b"....kHHttHHHkkHkk",
    b"..kkHttHHHtHHttHk",
    b".kHHtHHHhhHHtHHHkk",
    b"kHttHHhhHHHtHHHHHk",
    b"kHHHHhHHHhhHHHHtHhk",
    b"khHHhHHHh.hhHHttHhk",
    b"khHhhhHh....hhHHhHk",
    b"khHHhhh......hh..hk",
    b".khHhS",
    b"khHHh",
    b"khHHh",
    b"khHHhk",
    b".khHHhk",
    b".khhHHk",
    b"..khhhh",
    b"..khh",
    b".kh",
];
const BACK_FRINGE: &[&[u8]] = &[
    b"......kkhhhkkk",
    b"...kkHHttttHHhk",
    b"..kHttttHHHHHHHhk",
    b".kHHttHHHHhHttHHhk",
    b".kHHHHHHHhhhHtttHHk",
    b"khHHHHHhhhHHHHHHHHk",
    b"khHHHHhhhHHHHHHHHhk",
    b"khHHhhhHHHHHHHhhhHhk",
    b"khHhhHHHHHHHhhhhHHhk",
    b".khhhHHHHhhhHHhhHHhk",
    b".khhHHHhhhHHhHhhHSsk",
    b".khhHHhhHHHHHhhhHslv",
    b"..khHHhhHHHHhhhhhSsv",
    b"..khhhhHHHHhhhHhhhSv",
    b"...khhhHHHhhhHhhhhh",
    b"....khhHhhhhHHhhhhk",
    b".....khhhhhhhhhhhk",
    b".......kkhhhhhSSkk",
    b"..........kvSSvk",
];
const BACK_CURLS: &[&[u8]] = &[
    b"......kkHHHkk",
    b"....kHHttHHHkkHkk",
    b"..kkHttHHHtHHttHk",
    b".kHHtHHHhhHHtHHHkk",
    b"kHttHHhhHHHtHHHHHk",
    b"kHHHHhHHHhhHHHHtHhk",
    b"khHHhHHHhhhHHHttHhk",
    b"khHhhhHHHHhhHHHHhHk",
    b"khHHhhhHHHHhhHHHHhk",
    b".khHHHhhhHHhhhHHhhk",
    b"khHHHtHhHhhHHHhhhsk",
    b"khHHttHhhHHHtHHhhlv",
    b"khHHHHhhHHHttHHhSsv",
    b".khhHHhhHHHHHHhhhsSv",
    b".khhHhhHhhHHhhhHhhk",
    b"..khhhhHHhHHhhhHHhhk",
    b"..khhhhhHhhhhHhHHhk",
    b".kh..kkhhhhhhSkk",
    b"..........kvSSvk",
];
const PONY: &[&[u8]] = &[
    b"..kkkk",
    b".kHttHkk",
    b"kHttHHHk",
    b"kHHHHhhCk",
    b"khHHHhcCk",
    b".khHHhhk",
    b".khhHHhk",
    b".khhHHhk",
    b"..khHHhk",
    b"..khHHk",
    b".khhhk",
    b".khhk",
    b"khk",
];
const FRONT_CAP: &[&[u8]] = &[
    b".......kkkkkkk",
    b".....kkjJAJJJkk",
    b"...kkJAAAJJJJjjk",
    b"..kJAAAJJJjjJJJjk",
    b"..kAAJJJJJjjJJJJjk",
    b".kjJJJJJjJCcjjJJjk",
    b".kdjjccCCCCccccccck",
    b"..kdcccjjjjjjJAAAJk",
    b".........kkkjjjjjjjk",
];
const BACK_CAP: &[&[u8]] = &[
    b".......kkkkkkk",
    b".....kkjJAJJJkk",
    b"...kkJJAAJAJJjjk",
    b"..kJJAAJAAJAJJJjk",
    b"..kAAJJJJJJjjJJJjk",
    b".kjJJJJJJJJjjJJJjk",
    b".kdcccccCCccccccck",
    b"..kjjjjjdddjjjjjdk",
    b"....kkkddddddkkkk",
];

fn head(a: &mut Pen, g: &Guest, back: bool, x: i32, y: i32, closed: bool, waving: bool) {
    let x = x - 1;
    let curls = g.face & 128 != 0;
    if g.face & 16 != 0 {
        a.stamp(x - 3, y + 4, PONY);
    }
    if back {
        a.stamp(x, y, if curls { BACK_CURLS } else { BACK_FRINGE });
        if g.face & 64 != 0 {
            a.stamp(x + 16, y + 9, &[b"...g", b".Gg"]);
        }
    } else {
        a.stamp(x, y + 5, CHEEKS);
        a.stamp(x, y, if curls { CURLS } else { FRINGE });
        a.stamp(x + 8, y + 7, &[b"........m", b".mm....m."]);
        if closed {
            a.stamp(x + 8, y + 10, &[b"........m", b"m..m..mm.", b".mm......"]);
        } else if curls {
            a.stamp(x + 8, y + 9, &[b".eie...ee", b".wie..wei", b"..Ie...Ie"]);
        } else {
            a.stamp(
                x + 8,
                y + 9,
                &[b".eee...ee", b"ewie..wei", b".iIe...Ie", b"..i......"],
            );
        }
        a.stamp(x + 13, y + 12, &[b".l", b".r"]);
        if g.face & 4 != 0 {
            a.stamp(x + 6, y + 13, &[b"S.r.......r"]);
        }
        if waving {
            a.stamp(x + 11, y + 14, &[b"m...m", b".emmm", b"..rr."]);
        } else if curls {
            a.stamp(x + 11, y + 14, &[b"r...m", b".mmm.", b"..r.."]);
        } else {
            a.stamp(x + 11, y + 14, &[b"r..m", b".mm."]);
        }
        if g.face & 64 != 0 {
            // Open lower-contrast wire rims leave the iris and its glint clear.
            a.stamp(
                x + 5,
                y + 8,
                &[
                    b"....Ggg...Ggg",
                    b".ggg...g.g...g",
                    b"g..g...ggg...g",
                    b"...g...g.g...g",
                    b"....ggg...ggg",
                ],
            );
        }
    }
    if g.face & 32 != 0 {
        a.stamp(x, y - 2, if back { BACK_CAP } else { FRONT_CAP });
    }
}

const FRONT_JACKET: &[&[u8]] = &[
    b".....kcssk....",
    b"...kjCClSCjk..",
    b"..kJjCCcCjJjk.",
    b".kAJJJcCcdJJjk",
    b"kAJJJJcCcjJJjk",
    b"kjJJJJjCjzJJjk",
    b".kJjJJJjJJJJdk",
    b".kjJJJJjzJJJdk",
    b".kdJJJJjJJJJdk",
    b".kjJJJJjzJJjjk",
    b".kJJjjJjJJjjjk",
    b".kjJJJJjJJJJdk",
    b".kdjJJjdjjJjdk",
    b"..kddjdkddjdk.",
    b"...kkk..kkkk..",
];
const BACK_JACKET: &[&[u8]] = &[
    b".....kcssk....",
    b"...kjCCCccjk..",
    b"..kAAJccCCcjk.",
    b".kAAJJJJccCcjk",
    b"kAJJJJJJJcCcdk",
    b"kjJJAJJJJjcCdk",
    b".kJJJAJJJJccjk",
    b".kjJJJJJJJjjdk",
    b".kjJJJJJJJjjdk",
    b".kdjjJJJJjjjdk",
    b".kjJJJjJjJJjjk",
    b".kJJJjjkjJJJjk",
    b".kjJjjd.djjJdk",
    b"..kjjd...djjk.",
    b"...kk.....kk..",
];

#[derive(Clone, Copy)]
enum Shoe {
    Heel,
    Flat,
    Roll,
    Lift,
}
#[derive(Clone, Copy)]
struct Key {
    knee: Point,
    foot: Point,
    shoe: Shoe,
}
const fn key(kx: i32, ky: i32, x: i32, y: i32, shoe: Shoe) -> Key {
    Key {
        knee: (kx, ky),
        foot: (x, y),
        shoe,
    }
}
// Half a cycle belongs to a planted foot. These contacts move exactly as far
// back as the body moves along the street. The recovery bends forward at the
// knee as the foot swings past. A rear heel rises toward the viewer at toe-off.
use Shoe::{Flat, Heel, Lift, Roll};
const FRONT_STEP: [Key; 8] = [
    key(3, -6, 7, 3, Heel),
    key(1, -5, 3, 1, Flat),
    key(-1, -6, -1, -1, Flat),
    key(-3, -7, -5, -3, Flat),
    key(-3, -9, -7, -4, Roll),
    key(0, -10, -5, -5, Lift),
    key(3, -9, 0, -3, Lift),
    key(5, -8, 5, 0, Heel),
];
const BACK_STEP: [Key; 8] = [
    key(4, -9, 7, -4, Flat),
    key(2, -8, 3, -2, Flat),
    key(0, -7, -1, 0, Flat),
    key(-2, -6, -5, 2, Flat),
    key(-3, -7, -7, 3, Roll),
    key(-1, -9, -6, 0, Lift),
    key(3, -10, -1, -1, Lift),
    key(5, -11, 5, -3, Heel),
];

fn boot(a: &mut Pen, foot: Point, back: bool, pose: Shoe) {
    let (x, y) = foot;
    let (dx, dy, stamp): (_, _, &[&[u8]]) = match (back, pose) {
        (false, Heel) => (
            -3,
            -4,
            &[
                b".kBBk...",
                b".bBTBk..",
                b".bBBBTBk",
                b".kbbbkk.",
                b"..kk....",
            ],
        ),
        (false, Flat) => (
            -3,
            -4,
            &[
                b".kBBk...",
                b".bBTBk..",
                b".bBBBTBBk",
                b".kbBBBBbk",
                b".kkkkkkkk",
            ],
        ),
        (false, Roll) => (
            -4,
            -5,
            &[
                b"kkbk...", b"bBBbk..", b".bBBBk.", b"..bBTBk", b"...kBBk", b"....kk.",
            ],
        ),
        (false, Lift) => (
            -3,
            -4,
            &[b".kbbk..", b".bBTBk.", b".kBBBTk", b"..kbBBk", b"...kkk."],
        ),
        (true, Flat) => (
            -4,
            -4,
            &[
                b"....kkk.",
                b"..kBBBBbk",
                b".kBTBBbbk",
                b"kBTTBbkk.",
                b".kkkkk...",
            ],
        ),
        (true, Heel) => (
            -4,
            -4,
            &[
                b"....kkk.",
                b"...kBBBBk",
                b".kbBBBbbk",
                b"kBTTBbkk.",
                b".kkkkk...",
            ],
        ),
        (true, Roll) => (
            -4,
            -4,
            &[b"..kkk..", b".kBBBk.", b"kBTBBbk", b"kTbbBbk", b".kkkkk."],
        ),
        (true, Lift) => (
            -4,
            -4,
            &[b"..kkk..", b".kBBTbk", b"kBTTBbk", b"kTBbbk.", b".kkkk.."],
        ),
    };
    a.stamp(x + dx, y + dy, stamp);
}
fn leg(a: &mut Pen, hip: Point, k: Point, foot: Point, back: bool, near: bool, pose: Shoe) {
    let (hx, hy) = hip;
    let (kx, ky) = k;
    let (fx, fy) = foot;
    let thigh = if near { 0x667383 } else { 0x4a5668 };
    let shin = if near { 0x556476 } else { 0x414d60 };
    let edge = a.inks.line;
    // Two overlapping quadrilaterals keep a bent knee solid even when the
    // lifted heel doubles back underneath it.
    a.shape(
        &[
            (hx - 3, hy),
            (hx + 3, hy),
            (kx + 2, ky + 1),
            (kx - 3, ky + 2),
        ],
        edge,
    );
    a.shape(
        &[
            (kx - 2, ky - 1),
            (kx + 2, ky - 1),
            (fx + 1, fy - 2),
            (fx - 2, fy - 2),
        ],
        edge,
    );
    a.shape(
        &[
            (hx - 2, hy),
            (hx + 2, hy),
            (kx + 1, ky + 1),
            (kx - 2, ky + 1),
        ],
        thigh,
    );
    a.shape(
        &[(kx - 1, ky), (kx + 1, ky), (fx, fy - 3), (fx - 1, fy - 3)],
        shin,
    );
    if near {
        a.line((hx - 1, hy + 2), (kx - 2, ky), 0x7e8992);
        a.rect(fx - 2, fy - 5, 3, 1, 0x7f8990);
    }
    if back {
        a.rect(kx - 1, ky + 1, 2, 1, if near { 0x485668 } else { 0x3d4756 });
        a.rect(fx - 2, fy - 4, 3, 1, if near { 0x82909a } else { 0x627181 });
    }
    boot(a, foot, back, pose);
}
fn midpoint(a: Point, b: Point, sub: i32) -> Point {
    (
        a.0 + ((b.0 - a.0) * sub).div_euclid(2),
        a.1 + ((b.1 - a.1) * sub).div_euclid(2),
    )
}
fn moving_leg(a: &mut Pen, back: bool, near: bool, phase: i32, bob: i32) {
    let keys = if back { &BACK_STEP } else { &FRONT_STEP };
    let i = (phase / 2) as usize;
    let next = (i + 1) % 8;
    // On the return swing, the far shoe travels outside the standing calf.
    // These camera-specific arcs keep its heel and ankle distinct at crossover.
    let arc = match (back, near) {
        (false, false) => [0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -4, -5, -2, -1, 0, 0],
        (false, true) => [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 1, 0],
        (true, false) => [0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 2, 5, 4, 2],
        (true, true) => [0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -2, -2, -1, 0, 0],
    };
    let bend = arc[phase as usize];
    let k = midpoint(keys[i].knee, keys[next].knee, phase % 2);
    let f = midpoint(keys[i].foot, keys[next].foot, phase % 2);
    let x = if back == near { 2 } else { -2 };
    let y = if near { 0 } else { -3 };
    leg(
        a,
        (x, -15 + bob),
        (x + k.0 + bend / 2, k.1 + y / 2),
        (x + f.0 + bend, y + f.1),
        back,
        near,
        keys[i].shoe,
    );
}

#[derive(Clone, Copy)]
struct Arm {
    elbow: Point,
    hand: Point,
}
const fn arm(ex: i32, ey: i32, x: i32, y: i32) -> Arm {
    Arm {
        elbow: (ex, ey),
        hand: (x, y),
    }
}
// As the opposing heel strikes, the near hand reaches just past the jacket.
const FRONT_ARM: [Arm; 8] = [
    arm(-2, 4, -4, 7),
    arm(-2, 5, -3, 8),
    arm(-1, 5, 0, 9),
    arm(1, 5, 4, 8),
    arm(2, 4, 6, 6),
    arm(1, 5, 4, 7),
    arm(0, 5, 1, 8),
    arm(-2, 4, -2, 8),
];
const BACK_ARM: [Arm; 8] = [
    arm(-1, 4, -3, 9),
    arm(-1, 5, -2, 10),
    arm(0, 5, 0, 9),
    arm(1, 5, 3, 7),
    arm(2, 4, 5, 4),
    arm(2, 4, 4, 5),
    arm(1, 5, 1, 7),
    arm(-1, 5, -2, 9),
];
fn sleeve(a: &mut Pen, shoulder: Point, arm: Arm, near: bool) {
    let (x, y) = shoulder;
    let (ex, ey) = (x + arm.elbow.0, y + arm.elbow.1);
    let (hx, hy) = (x + arm.hand.0, y + arm.hand.1);
    let [deep, shade, cloth, light] = a.inks.coat;
    a.shape(
        &[(x - 2, y), (x + 2, y), (ex + 2, ey), (ex - 2, ey + 1)],
        a.inks.line,
    );
    a.shape(
        &[
            (ex - 2, ey - 1),
            (ex + 2, ey - 1),
            (hx + 2, hy),
            (hx - 2, hy),
        ],
        a.inks.line,
    );
    a.shape(
        &[(x - 1, y), (x + 1, y), (ex + 1, ey), (ex - 1, ey)],
        if near { cloth } else { shade },
    );
    a.shape(
        &[
            (ex - 1, ey),
            (ex + 1, ey - 1),
            (hx + 1, hy - 2),
            (hx - 1, hy - 2),
        ],
        if near { shade } else { deep },
    );
    a.line(
        (x - 1, y),
        (ex - 2, ey - 1),
        if near { light } else { cloth },
    );
    a.rect(hx - 1, hy - 2, 3, 1, if near { light } else { cloth });
    a.rect(hx - 1, hy - 1, 3, 3, a.inks.skin[2]);
    a.rect(hx, hy - 1, 2, 2, a.inks.skin[1]);
    a.rect(hx, hy + 2, 2, 1, a.inks.skin[4]);
}
fn raised_hand(a: &mut Pen, x: i32, y: i32, now: u64) {
    let spread = [0, 1, 2, 1][(now / 135 % 4) as usize];
    let (hx, hy) = (x + 8 + spread, y - 9);
    a.shape(
        &[
            (x - 1, y),
            (x + 2, y),
            (x + 6, y + 3),
            (x + 4, y + 6),
            (x + 1, y + 4),
        ],
        a.inks.line,
    );
    a.shape(
        &[(x, y), (x + 2, y), (x + 5, y + 3), (x + 3, y + 4)],
        a.inks.coat[1],
    );
    a.shape(
        &[
            (x + 3, y + 4),
            (x + 5, y + 5),
            (hx + 2, hy + 4),
            (hx - 1, hy + 3),
        ],
        a.inks.skin[2],
    );
    a.line((x + 5, y + 3), (hx, hy + 3), a.inks.skin[1]);
    a.stamp(
        hx - 2,
        hy - 3,
        &[
            b"..l...", b"l.s.l.", b"slsls.", b".slssk", b".slsSk", b"..sSkk", b"..SS..",
        ],
    );
}
fn walk_bob(phase: i32) -> i32 {
    [0, 1, 1, 1, 0, 0, -1, -1][phase as usize % 8]
}
pub(super) fn paint(g: &Guest, p: &mut Pixels, now: u64, local: bool) {
    let (x, y) = g.foot(now);
    let walk = g.walking(now);
    let sitting = g.place.seat(g.cell) && !walk;
    let dir = if sitting {
        if (g.place == Place::Bar && g.cell.0 == 1) || (g.place == Place::Square && g.cell.0 == 3) {
            0
        } else {
            1
        }
    } else {
        g.direction
    };
    let back = dir >= 2;
    let facing = if dir == 1 || dir == 2 { -1 } else { 1 };
    if !sitting {
        p.oval(x, y, 10, 3, 0x5a5d5e);
        if local {
            for d in [-1, 1] {
                p.line((x + 10 * d, y + 1), (x + 5 * d, y + 3), 0xf4ca87);
            }
            p.rect(x - 3, y + 3, 7, 1, 0xffdfa1);
        }
    }
    let phase = if walk {
        let (u, v) = g.position(now);
        let distance = (u * 16.).round() as i32 + (v * 16.).round() as i32;
        (if back { -distance } else { distance }).rem_euclid(16)
    } else {
        0
    };
    let bob = if walk { walk_bob(phase) } else { 0 };
    let sleeping = g.sleeping && !walk;
    let mut a = Pen {
        p,
        x,
        y: y + if sitting {
            if g.place == Place::Bar { -2 } else { 3 }
        } else {
            0
        },
        mirror: facing,
        inks: Inks::new(g),
    };
    if walk {
        moving_leg(&mut a, back, false, (phase + 8) % 16, bob);
        moving_leg(&mut a, back, true, phase, bob);
    } else if sitting {
        leg(&mut a, (1, -15), (10, -13), (9, -2), false, false, Flat);
        leg(&mut a, (-4, -14), (4, -10), (3, 0), false, true, Flat);
    } else {
        let near = if back { 3 } else { -3 };
        leg(
            &mut a,
            (-near, -15),
            (-near, -9),
            (-near + 1, -3),
            back,
            false,
            Flat,
        );
        leg(&mut a, (near, -15), (near, -8), (near, 0), back, true, Flat);
    }
    let armset = if back { &BACK_ARM } else { &FRONT_ARM };
    let lean = if walk { 1 } else { 0 };
    let ty = -28 + bob + if sleeping { 1 } else { 0 };
    let far = if back { -5 } else { 5 };
    let near = -far;
    let wave = g.wave_until > now && !sleeping;
    if wave && !back {
        raised_hand(&mut a, lean + 6, ty + 4, now);
    } else {
        sleeve(
            &mut a,
            (lean + far, ty + 4),
            if walk {
                armset[((phase / 2 + 4) % 8) as usize]
            } else {
                arm(1, 4, 1, 9)
            },
            false,
        );
    }
    a.stamp(lean - 7, ty, if back { BACK_JACKET } else { FRONT_JACKET });
    let hanging = if sitting {
        arm(-1, 5, 4, 8)
    } else if sleeping {
        arm(-1, 5, 0, 10)
    } else {
        arm(-1, 5, 0, 9)
    };
    if wave && back {
        raised_hand(&mut a, lean + near, ty + 4, now);
    } else {
        let roll = if walk {
            [1, 1, 0, 0, -1, -1, 0, 0][(phase / 2) as usize]
        } else {
            0
        };
        sleeve(
            &mut a,
            (lean + near + roll, ty + 4),
            if walk {
                armset[(phase / 2) as usize]
            } else {
                hanging
            },
            true,
        );
    }
    let hx = lean - 8 + if sleeping { 1 } else { 0 };
    let hy = -42 + bob + if sleeping { 2 } else { 0 };
    head(
        &mut a,
        g,
        back,
        hx,
        hy,
        sleeping || (now + u64::from(g.face) * 17) % 4700 > 4520,
        wave,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pixels::W;

    #[test]
    fn both_boots_remain_visible_as_the_legs_pass() {
        let inks = Inks::new(&Guest::new([0; 16]));
        let leather = [inks.ink(b'b'), inks.ink(b'B'), inks.ink(b'T')];
        let origin = (40, 48);
        for back in [false, true] {
            for phase in 0..16 {
                let render_leg = |near| {
                    let mut pixels = Pixels::blank();
                    let mut pen = Pen {
                        p: &mut pixels,
                        x: origin.0,
                        y: origin.1,
                        mirror: 1,
                        inks,
                    };
                    let timing = if near { phase } else { (phase + 8) % 16 };
                    moving_leg(&mut pen, back, near, timing, walk_bob(phase));
                    pixels
                };
                let far = render_leg(false);
                let near = render_leg(true);
                let mut exposed_far = 0;
                let mut exposed_near = 0;
                for y in origin.1 - 18..=origin.1 + 5 {
                    for x in origin.0 - 20..=origin.0 + 20 {
                        let i = ((y * W + x) * 4) as usize;
                        let color = |p: &Pixels| {
                            u32::from_be_bytes([0, p.rgba[i], p.rgba[i + 1], p.rgba[i + 2]])
                        };
                        if near.rgba[i + 3] == 0 && leather.contains(&color(&far)) {
                            exposed_far += 1;
                        }
                        if leather.contains(&color(&near)) {
                            exposed_near += 1;
                        }
                    }
                }
                assert!(
                    exposed_far >= 4 && exposed_near >= 4,
                    "boot obscured at back={back}, phase={phase}: {exposed_far} far, {exposed_near} near"
                );
            }
        }
    }
}
