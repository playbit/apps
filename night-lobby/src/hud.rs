use crate::font;
use crate::pixels::{Bounds, Pixels, W};
use crate::{App, DOOR, TOGGLE_CHAT, WAVE, plaza::Place};
use std::borrow::Cow;

pub fn map_bounds(view: Bounds) -> Bounds {
    Bounds {
        x: view.x + view.w - 70,
        y: view.y + view.h - 50,
        w: 64,
        h: 44,
    }
}

pub fn labels(outside: bool, chat_open: bool) -> [(u32, &'static str); 3] {
    [
        (
            TOGGLE_CHAT,
            if chat_open { "Hide chat" } else { "Show chat" },
        ),
        (WAVE, "Wave"),
        (
            DOOR,
            if outside {
                "Enter Session"
            } else {
                "Go outside"
            },
        ),
    ]
}

pub fn actions(view: Bounds, outside: bool, chat_open: bool) -> [(u32, &'static str, Bounds); 3] {
    let labels = labels(outside, chat_open);
    let widths = labels.map(|(_, label)| font::width(label, 1) + 20);
    let mut x = view.x + (view.w - widths.iter().sum::<i32>() - 8) / 2;
    std::array::from_fn(|i| {
        let bounds = Bounds {
            x,
            y: view.y + view.h - 30,
            w: widths[i],
            h: 24,
        };
        x += widths[i] + 4;
        (labels[i].0, labels[i].1, bounds)
    })
}

const INK: u32 = 0x172231;
const CREAM: u32 = 0xffe5af;

// The bitmap covers its original Latin and punctuation repertoire. Let the
// host shape other scripts and emoji so names and conversations stay readable.
pub struct NativeLine {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub text: String,
    pub color: u32,
}

fn line_width(s: &str) -> i32 {
    if font::supported(s) {
        font::width(s, 1)
    } else {
        s.chars().map(|c| if c.is_ascii() { 8 } else { 12 }).sum()
    }
}

const MAX_NAME_WIDTH: i32 = 174;

// Keep the full account name in chat and the avatar panel. Canvas badges have
// limited room alongside the other HUD controls.
fn badge_name(name: &str) -> Cow<'_, str> {
    if line_width(name) <= MAX_NAME_WIDTH {
        return Cow::Borrowed(name);
    }
    let mut prefix = String::from("...");
    for ch in name.chars() {
        let end = prefix.len() - 3;
        prefix.insert(end, ch);
        if line_width(&prefix) > MAX_NAME_WIDTH {
            prefix.remove(end);
            break;
        }
    }
    Cow::Owned(prefix)
}

fn lettering(p: &mut Pixels, native: &mut Vec<NativeLine>, x: i32, y: i32, s: &str, color: u32) {
    if font::supported(s) {
        font::draw(p, x, y, s, color, 1);
    } else {
        native.push(NativeLine {
            x,
            y,
            width: line_width(s),
            text: s.to_owned(),
            color,
        });
    }
}

pub fn name_bounds(name: &str, view: Bounds) -> Bounds {
    Bounds {
        x: view.x + 6,
        y: view.y + view.h - 28,
        w: line_width(name).min(MAX_NAME_WIDTH) + 12,
        h: 22,
    }
}

// Both the board and this plan look down the same two isometric axes.
fn map_point(map: Bounds, u: f32, v: f32) -> (i32, i32) {
    (
        map.x + 32 + ((u - v) * 3.0).round() as i32,
        map.y + 6 + ((u + v) * 2.0).round() as i32,
    )
}

fn map_marker(p: &mut Pixels, x: i32, y: i32, own: bool, ink: u32) {
    // A five-pixel circle, drawn from the same outline for every guest.
    p.rect(x - 1, y - 2, 3, 1, ink);
    p.rect(x - 1, y + 2, 3, 1, ink);
    p.rect(x - 2, y - 1, 1, 3, ink);
    p.rect(x + 2, y - 1, 1, 3, ink);
    if own {
        p.rect(x - 1, y - 1, 3, 3, ink);
    }
}

fn minimap(p: &mut Pixels, app: &App) {
    let map = map_bounds(p.bounds);
    p.rect(map.x, map.y, map.w, map.h, 0x546472);
    p.rect(map.x + 1, map.y + 1, map.w - 2, map.h - 2, INK);
    let place = app.place();
    for u in 0..9 {
        for v in 0..9 {
            let (x, y) = map_point(map, u as f32, v as f32);
            let ink = if (u, v) == place.door() {
                0xb28857
            } else if !place.walkable((u, v)) {
                0x303947
            } else if place == Place::Bar {
                0x665258
            } else {
                0x455c63
            };
            p.poly(&[(x, y - 2), (x + 3, y), (x, y + 2), (x - 3, y)], ink);
        }
    }
    for (id, g) in app.actors() {
        if id != app.room.local.account {
            let (u, v) = g.position(app.now);
            let (x, y) = map_point(map, u, v);
            map_marker(p, x, y, false, if g.sleeping { 0xa0acb2 } else { 0xc4f3e3 });
        }
    }
    let (u, v) = app.local().position(app.now);
    let (x, y) = map_point(map, u, v);
    map_marker(p, x, y, true, CREAM);
}

fn speech_lines(mut s: &str) -> Vec<String> {
    let mut lines = Vec::new();
    while !s.is_empty() && lines.len() < 3 {
        let mut end = 0;
        let mut last_space = 0;
        let mut width = 0;
        let mut adjacent = false;
        for (i, c) in s.char_indices() {
            let mut encoded = [0u8; 4];
            let advance = line_width(c.encode_utf8(&mut encoded)) + i32::from(adjacent && c != ' ');
            if width + advance > 220 {
                break;
            }
            width += advance;
            adjacent = c != ' ';
            end = i + c.len_utf8();
            if c == ' ' {
                last_space = end;
            }
        }
        if end < s.len() && last_space > 0 {
            end = last_space;
        }
        let (line, rest) = s.split_at(end);
        s = rest.trim_start();
        let mut line = line.trim_end().to_owned();
        if lines.len() == 2 && !s.is_empty() {
            line.push_str("...");
        }
        lines.push(line);
    }
    lines
}

pub fn draw(p: &mut Pixels, app: &App) -> Vec<NativeLine> {
    let mut native = Vec::new();
    let mut occupied = Vec::<Bounds>::new();
    let mut guests: Vec<_> = app.actors().collect();
    // Nearby figures have lower feet. Set their label first, then stack any
    // label behind them upward so neither name covers the other.
    guests.sort_by_key(|(_, g)| std::cmp::Reverse(g.foot(app.now).1));
    for (id, g) in guests {
        let (x, y) = g.foot(app.now);
        let speech = g.bubble.as_ref().filter(|(_, until)| *until > app.now);
        let lines = match speech {
            Some((words, _)) => speech_lines(words),
            None if g.sleeping => vec![format!("Zzz · {}", badge_name(app.player_name(id)))],
            None => vec![badge_name(app.player_name(id)).into_owned()],
        };
        let w = lines.iter().map(|s| line_width(s)).max().unwrap_or(1) + 8;
        let h = lines.len() as i32 * font::HEIGHT + 2;
        let mut b = Bounds {
            x: (x - w / 2).clamp(1, W - w - 1),
            y: (y - 47 - h).max(2),
            w,
            h,
        };
        while let Some(other) = occupied.iter().find(|o| b.intersects(**o)) {
            let next = (other.y - h - 3).max(2);
            if next >= b.y {
                break;
            }
            b.y = next;
        }
        occupied.push(b);
        let (bg, fg) = if speech.is_some() {
            (0xffebce, 0x302a36)
        } else if app.room.local.account == id {
            (INK, CREAM)
        } else {
            (INK, 0xe1eddf)
        };
        p.rect(b.x, b.y, b.w, b.h, bg);
        let tail = x.clamp(b.x + 2, b.x + b.w - 3);
        if speech.is_some() {
            p.line((tail - 2, b.y + b.h), (tail, b.y + b.h + 2), bg);
            p.rect(tail, b.y + b.h, 1, 2, bg);
        }
        for (i, s) in lines.iter().enumerate() {
            lettering(
                p,
                &mut native,
                b.x + 4,
                b.y + 1 + font::HEIGHT * i as i32,
                s,
                fg,
            );
        }
    }

    let title = if app.outside() {
        "Lantern Square"
    } else {
        "Session"
    };
    let view = p.bounds;
    let x = view.x + 6;
    font::draw(p, x + 1, view.y + 9, title, INK, 2);
    font::draw(p, x, view.y + 7, title, CREAM, 2);
    let connected = app.outside() || app.room.connected;
    let status = if connected {
        format!(
            "{} online",
            app.actors().filter(|(_, g)| !g.sleeping).count()
        )
    } else {
        String::from("Connecting")
    };
    let width = font::width(&status, 1);
    let map = map_bounds(view);
    let x = view.x + view.w - width - 21;
    let y = map.y - 19;
    p.rect(x, y, width + 15, 17, INK);
    p.rect(
        x + 4,
        y + 7,
        3,
        3,
        if connected { 0x99d2ae } else { 0xdbb372 },
    );
    font::draw(p, x + 10, y + 3, &status, 0xe8ecde, 1);

    let name = app.player_name(app.room.local.account);
    let b = name_bounds(name, view);
    p.rect(b.x, b.y, b.w, b.h, INK);
    lettering(p, &mut native, b.x + 6, b.y + 5, &badge_name(name), CREAM);
    p.rect(b.x + 6, b.y + 17, b.w - 12, 1, 0x937b60);
    for (_, label, b) in actions(view, app.outside(), app.chat_open) {
        p.rect(b.x, b.y, b.w, b.h, INK);
        p.rect(b.x, b.y, b.w, 1, 0x69756e);
        font::draw(p, b.x + 10, b.y + 6, label, CREAM, 1);
    }
    if let Some(error) = app.notices().find(|s| !s.is_empty()) {
        let message: String = error.chars().take_while(char::is_ascii).take(60).collect();
        let y = b.y - 19;
        p.rect(b.x, y, font::width(&message, 1) + 12, 17, INK);
        font::draw(p, b.x + 6, y + 3, &message, CREAM, 1);
    }
    minimap(p, app);
    native
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::guest::Guest;

    #[test]
    fn full_width_names_fit_both_badges() {
        let mut app = App::default();
        let id = app.room.local.account;
        app.room.local.sleeping = true;
        let full_name = "猫".repeat(85);
        app.room.display_names.insert(id, full_name.clone());
        let mut art = Pixels::blank();
        let tiny = badge_name(&full_name);
        assert!(tiny.ends_with("...") && line_width(&tiny) <= MAX_NAME_WIDTH);
        let native = draw(&mut art, &app);
        assert!(native.iter().any(|line| line.text == tiny));
        assert!(
            native
                .iter()
                .any(|line| line.text == format!("Zzz · {tiny}"))
        );
        assert_eq!(app.player_name(id), full_name);
    }

    #[test]
    fn mini_map_places_discs_and_rings_at_the_interpolated_guest_positions() {
        let mut app = App::default();
        app.room.local = Guest::new([1; 16]);
        app.room.local.stand((0, 8));
        app.room.local.go((1, 8), 0);
        app.now = crate::guest::STEP_MS / 2;
        let mut other = Guest::new([2; 16]);
        other.stand((8, 0));
        app.room.guests.insert(other.account, other);
        let mut art = Pixels::blank();
        let map = map_bounds(art.bounds);
        minimap(&mut art, &app);
        let pixel = |x: i32, y: i32| {
            let i = ((y * W + x) * 4) as usize;
            u32::from_be_bytes(art.rgba[i..i + 4].try_into().unwrap()) >> 8
        };
        let (x, y) = map_point(map, 0.5, 8.);
        assert_eq!(pixel(x, y), CREAM);
        assert_eq!(pixel(x + 2, y), CREAM);
        let (x, y) = map_point(map, 8., 0.);
        assert_ne!(pixel(x, y), pixel(x + 2, y));
        assert_eq!(pixel(x - 2, y), pixel(x + 2, y));
        assert_eq!(pixel(x - 2, y), 0xc4f3e3);
        assert_eq!(pixel(map.x - 1, map.y), 0);
    }
}
