mod avatar_editor;
mod chat;
mod font;
mod glasshouse;
mod guest;
mod hud;
mod pixel_fit;
mod pixels;
mod plaza;
mod room;
mod screen;
mod square;
mod storage;
mod world;
use world::WorldKey;

use avatar_editor::Appearance;
use chat::{Conversation, Line};
use guest::{Cell, Guest, Id};
use playbit_api::abi::{
    syscall::{EventKind, FrameInput, KeyInput, MIN_WAKE_DELAY_MS, PointerInput},
    text::TextInput,
};
use playbit_api::context::{Context, Surface};
use playbit_api::ui::{ButtonStyle, TextStyle, ViewEncoder};
use plaza::{Place, Plaza};
use room::Room;
use std::collections::VecDeque;
use storage::Storage;

const BOARD: u32 = 1;
const ROOM_SAY: u32 = 2;
const ROOM_SEND: u32 = 3;
const WAVE: u32 = 4;
const TOGGLE_CHAT: u32 = 5;
const PROFILE: u32 = 6;
const SAVE: u32 = 9;
const DOOR: u32 = 12;
const PLAZA_SAY: u32 = 13;
const PLAZA_SEND: u32 = 14;
const ROOM_TRANSCRIPT: u32 = 10;
const PLAZA_TRANSCRIPT: u32 = 15;

#[derive(PartialEq)]
enum Panel {
    Room,
    Avatar,
}

struct App {
    room: Room,
    storage: Storage,
    held: Vec<u32>,
    density: f32,
    archive: Vec<Line>,
    bar_chat: Conversation,
    square_chat: Conversation,
    plaza: Plaza,
    at_door: bool,
    backdrop: Option<(Place, pixels::Pixels)>,
    route: VecDeque<Cell>,
    now: u64,
    frame_since_wake: bool,
    frame_us: u32,
    clear_field: bool,
    surface: (u32, u32),
    panel: Panel,
    chat_open: bool,
    draft: String,
    appearance_draft: Appearance,
    avatar_field: Option<u32>,
    started: bool,
}
#[allow(unsafe_code)]
#[unsafe(no_mangle)]
pub fn playbit_app() -> playbit_api::App {
    playbit_api::app(App::default(), turn, true)
}
impl Default for App {
    fn default() -> Self {
        Self {
            room: Room::default(),
            storage: Storage::default(),
            held: Vec::new(),
            density: 1.,
            archive: Vec::new(),
            bar_chat: Conversation::default(),
            square_chat: Conversation::default(),
            plaza: Plaza::default(),
            at_door: false,
            backdrop: None,
            route: VecDeque::new(),
            now: 0,
            frame_since_wake: false,
            frame_us: 0,
            clear_field: false,
            surface: (pixels::W as u32, pixels::H as u32),
            panel: Panel::Room,
            chat_open: true,
            draft: String::new(),
            appearance_draft: Appearance::default(),
            avatar_field: None,
            started: false,
        }
    }
}

impl App {
    fn outside(&self) -> bool {
        self.plaza.people.contains_key(&self.room.local.account)
    }
    fn place(&self) -> Place {
        if self.outside() {
            Place::Square
        } else {
            Place::Bar
        }
    }
    fn local(&self) -> &Guest {
        self.plaza
            .local(self.room.local.account)
            .unwrap_or(&self.room.local)
    }
    fn actors(&self) -> impl Iterator<Item = (Id, &Guest)> {
        let outside = self.outside();
        self.room
            .actors()
            .filter(move |(id, _)| !outside && !self.plaza.people.contains_key(id))
            .chain(
                self.plaza
                    .people
                    .iter()
                    .filter(move |_| outside)
                    .map(|(&id, v)| (id, &v.avatar)),
            )
    }
    fn conversation(&self) -> &Conversation {
        if self.outside() {
            &self.square_chat
        } else {
            &self.bar_chat
        }
    }
    fn reset_walk(&mut self) {
        self.held.clear();
        self.route.clear();
        self.at_door = false;
        self.draft.clear();
        self.clear_field = true;
    }
    fn cross(&mut self) {
        let id = self.room.local.account;
        if let Some(outdoor) = self.plaza.local(id) {
            let bar = &mut self.room.local;
            bar.look = outdoor.look;
            bar.face = outdoor.face;
            bar.stand(Place::Bar.landing());
            bar.direction = 1;
            self.storage.players.put(id, bar.checkpoint());
            self.plaza.leave(&mut self.storage.world, id);
        } else {
            // Retain the last indoor appearance and a walkable return position.
            self.room.local.stand(Place::Bar.landing());
            self.storage.players.put(id, self.room.local.checkpoint());
            self.plaza.arrive(
                &mut self.storage.world,
                self.room.local.clone(),
                self.room.id,
                self.now,
            );
        }
        self.room.changed = true;
        self.reset_walk();
    }
    fn layout(&self) -> screen::Layout {
        if self.chat_open {
            screen::Layout::with_chat(self.surface.0, self.surface.1, self.density)
        } else {
            screen::Layout::new(self.surface.0, self.surface.1, self.density)
        }
    }
    fn move_to(&mut self, to: Cell) {
        let route = guest::path(self.place(), self.local().cell, to);
        if !route.is_empty() || to == self.local().cell {
            self.route = route;
        }
        if to == self.place().door() && to == self.local().cell {
            self.at_door = true;
        }
        self.step();
    }
    fn step(&mut self) {
        if self.local().walking(self.now) {
            return;
        }
        if self.at_door {
            if self.storage.restored && self.storage.world.loaded {
                self.cross();
            }
            return;
        }
        let place = self.place();
        let tile = if let Some(key) = self.held.last() {
            let (u, v) = self.local().cell;
            let to = match key {
                1 => (u - 1, v),
                2 => (u + 1, v),
                3 => (u, v - 1),
                _ => (u, v + 1),
            };
            place.walkable(to).then_some(to)
        } else {
            self.route.pop_front()
        };
        if let Some(tile) = tile {
            self.at_door = tile == place.door();
            let account = self.room.local.account;
            if let Some(g) = self.plaza.local_mut(account) {
                g.go(tile, self.now);
                self.plaza.publish(
                    &mut self.storage.world,
                    account,
                    self.room.id,
                    self.now,
                    false,
                );
            } else {
                let g = &mut self.room.local;
                g.go(tile, self.now);
                self.room.changed = true;
                self.storage.moved = true;
                if self.storage.restored && !self.at_door {
                    self.storage.players.put(account, g.checkpoint());
                }
            }
        }
    }
    fn key(&mut self, key: KeyInput) {
        if key.key == 5 {
            if key.pressed {
                self.wave();
            }
            return;
        }
        if key.pressed {
            if self.held.contains(&key.key) {
                return;
            }
            self.held.push(key.key);
            self.route.clear();
        } else {
            self.held.retain(|k| *k != key.key);
        }
        self.step();
    }
    fn load_players(&mut self) {
        let account = self.room.local.account;
        let previous = self.room.saved.get(&account).map(Guest::checkpoint);
        self.room.saved = self
            .storage
            .players
            .rows
            .iter()
            .filter_map(|(&id, bytes)| Guest::restore(id, bytes).map(|g| (id, g)))
            .collect();
        self.room.restore_peers(self.now);
        if self.storage.restored {
            // Another window of the same account may have moved or changed clothes.
            if !self.storage.players.writing()
                && let Some(saved) = self.room.saved.get(&account)
                && previous.as_ref() != Some(&saved.checkpoint())
            {
                let local = &mut self.room.local;
                self.appearance_draft
                    .refresh(Appearance::from(&*local), Appearance::from(saved));
                local.follow(saved, self.now);
            }
            return;
        }
        self.storage.restored = true;
        let local = &mut self.room.local;
        if let Some(saved) = self.room.saved.get(&local.account) {
            if !self.storage.moved {
                local.cell = saved.cell;
                local.from = saved.from;
                local.direction = saved.direction;
            }
            if !self.storage.dressed {
                let saved = Appearance::from(saved);
                self.appearance_draft
                    .refresh(Appearance::from(&*local), saved);
                saved.apply(local);
            }
        }
        let value = local.checkpoint();
        if self.storage.players.rows.get(&local.account) != Some(&value) {
            self.storage.players.put(local.account, value);
        }
        self.room.changed = true;
    }
    fn known_display_name(&self, id: Id) -> Option<&str> {
        let observed = self.room.display_names.get(&id).map(String::as_str);
        if (id == self.room.local.account || self.room.guests.contains_key(&id))
            && let Some(name) = observed
        {
            return Some(name);
        }
        self.storage
            .world
            .rows
            .get(&WorldKey::Name(id))
            .and_then(|bytes| world::cached_display_name(bytes))
            .or(observed)
    }

    fn player_name(&self, id: Id) -> &str {
        self.known_display_name(id).unwrap_or(room::UNNAMED)
    }

    fn share_display_name(&mut self) {
        let id = self.room.local.account;
        let Some(name) = self.room.display_names.get(&id) else {
            return;
        };
        let world = &mut self.storage.world;
        if !world.loaded || id == [0; 16] {
            return;
        }
        let key = WorldKey::Name(id);
        let record = world::encode_display_name(name);
        if world.rows.get(&key) != Some(&record) || world.pending_where(|k| *k == key) != 0 {
            world.put(key, record);
        }
    }

    fn wave(&mut self) {
        let account = self.room.local.account;
        if let Some(g) = self.plaza.local_mut(account) {
            g.wave_until = self.now + 2300;
            self.plaza.publish(
                &mut self.storage.world,
                account,
                self.room.id,
                self.now,
                true,
            );
        } else {
            self.room.local.wave_until = self.now + 2300;
            self.room.changed = true;
        }
    }
    fn speak(&mut self, c: &mut Context, words: &str) {
        let id = self.room.local.account;
        let name = self.player_name(id).to_owned();
        let outside = self.outside();
        let chat = if outside {
            &mut self.square_chat
        } else {
            &mut self.bar_chat
        };
        let (loaded, pending) = if outside {
            (
                self.storage.world.loaded,
                self.storage
                    .world
                    .pending_where(|k| matches!(k, WorldKey::Chat(_))),
            )
        } else {
            (self.storage.chat.loaded, self.storage.chat.pending())
        };
        if let Some((key, body, words)) = chat.send(c, id, &name, words, loaded, pending) {
            if outside {
                self.storage.world.put(WorldKey::Chat(key), body);
            } else {
                self.storage.chat.put(key, body);
            }
            let g = self.plaza.local_mut(id).unwrap_or(&mut self.room.local);
            g.bubble = Some((words, self.now + 8000));
            self.draft.clear();
            self.clear_field = true;
        }
    }
    fn save(&mut self) {
        let local = &mut self.room.local;
        if self.appearance_draft != Appearance::from(&*local) {
            self.appearance_draft.apply(local);
            self.storage.dressed = true;
            if self.storage.restored {
                self.storage.players.put(local.account, local.checkpoint());
            }
        }
        if let Some(g) = self.plaza.local_mut(local.account) {
            self.appearance_draft.apply(g);
            self.plaza.publish(
                &mut self.storage.world,
                local.account,
                self.room.id,
                self.now,
                false,
            );
        }
        self.room.changed = true;
        self.panel = Panel::Room;
    }
    fn click(&mut self, c: &mut Context, target: u32) {
        self.held.clear();
        match target {
            DOOR if self.panel == Panel::Room => self.move_to(self.place().door()),
            ROOM_SEND | PLAZA_SEND if self.chat_visible() && target == self.chat_controls().1 => {
                self.speak(c, &self.draft.clone())
            }
            WAVE => self.wave(),
            TOGGLE_CHAT if self.panel == Panel::Room => self.chat_open = !self.chat_open,
            PROFILE => {
                if self.panel == Panel::Avatar {
                    self.panel = Panel::Room;
                } else {
                    self.appearance_draft = Appearance::from(self.local());
                    self.avatar_field = None;
                    self.panel = Panel::Avatar;
                }
            }
            SAVE if self.panel == Panel::Avatar => self.save(),
            action if self.panel == Panel::Avatar => {
                self.appearance_draft.click(action, &mut self.avatar_field)
            }
            _ => {}
        }
    }
    fn active(&self) -> bool {
        !self.route.is_empty()
            || (self.at_door && self.storage.world.loaded && self.storage.restored)
            || self
                .actors()
                .any(|(_, g)| g.walking(self.now) || g.wave_until > self.now)
    }
    fn advance(&mut self, elapsed: u64) -> bool {
        let mut dirty = self.active() || self.clear_field;
        self.clear_field = false;
        let previous = self.now;
        self.now += elapsed;
        // Background details animate slowly while idle. Movement still redraws
        // at the faster frame cadence and uses the same elapsed clock.
        dirty |= self.panel == Panel::Room && self.now / 240 != previous / 240;
        self.step();
        dirty |= self.plaza.tick(
            &mut self.storage.world,
            self.room.local.account,
            self.room.id,
            self.now,
        );
        for g in self
            .room
            .guests
            .values_mut()
            .chain([&mut self.room.local])
            .chain(self.plaza.people.values_mut().map(|v| &mut v.avatar))
        {
            if g.bubble
                .as_ref()
                .is_some_and(|(_, until)| *until <= self.now)
            {
                g.bubble = None;
                dirty = true;
            }
        }
        dirty
    }
    fn notices(&self) -> impl Iterator<Item = &str> {
        [
            &self.storage.players.error,
            &self.storage.world.error,
            &self.storage.chat.error,
            &self.conversation().notice,
        ]
        .into_iter()
        .map(String::as_str)
    }

    fn chat_visible(&self) -> bool {
        self.panel == Panel::Room && self.chat_open
    }

    // Separate native editors and scroll positions also keep late input for
    // the previous location from being sent into the new conversation.
    fn chat_controls(&self) -> (u32, u32, u32) {
        if self.outside() {
            (PLAZA_SAY, PLAZA_SEND, PLAZA_TRANSCRIPT)
        } else {
            (ROOM_SAY, ROOM_SEND, ROOM_TRANSCRIPT)
        }
    }

    fn chat_dock(&self, view: &mut ViewEncoder) {
        let (field, send, transcript) = self.chat_controls();
        view.scroll_start(transcript, false);
        view.column_start();
        let mut history = self
            .conversation()
            .lines
            .values()
            .rev()
            .chain(self.archive.iter().rev().filter(|_| !self.outside()))
            .peekable();
        let (loaded, error) = if self.outside() {
            (self.storage.world.loaded, &self.storage.world.error)
        } else {
            (self.storage.chat.loaded, &self.storage.chat.error)
        };
        if !self.conversation().notice.is_empty() {
            view.text(TextStyle::Muted, &self.conversation().notice);
        } else if !error.is_empty() {
            view.text(TextStyle::Muted, error);
        } else if history.peek().is_none() {
            view.text(
                TextStyle::Muted,
                if loaded {
                    "No messages yet."
                } else {
                    "Loading chat…"
                },
            );
        }
        for line in history {
            let name = self.known_display_name(line.guest).unwrap_or(&line.name);
            view.text(TextStyle::Body, &format!("{name}: {}", line.words));
        }
        view.column_end();
        view.scroll_end();

        view.row_start();
        view.column_start();
        view.text_field(
            field,
            &mut self.draft.clone(),
            if self.outside() {
                "Message Lantern Square…"
            } else {
                "Message Session…"
            },
            false,
            false,
            false,
        );
        view.column_end();
        view.button(
            send,
            "Send",
            ButtonStyle::Primary,
            self.draft.trim().is_empty(),
        );
        view.row_end();
    }

    fn render(&mut self, view: &mut ViewEncoder) {
        screen::theme(view, self);
        if self.panel == Panel::Room {
            if self.chat_open {
                view.column_start();
            }
            screen::render(self, view);
            if self.chat_open {
                self.chat_dock(view);
                view.column_end();
            }
            return;
        }
        view.column_start();
        view.text(TextStyle::Title, "Your avatar");
        view.row_start();
        view.column_start();
        screen::portrait(self, view);
        view.text(TextStyle::Label, self.player_name(self.room.local.account));
        view.column_end();
        view.column_start();
        self.appearance_draft.controls(view, self.avatar_field);
        view.column_end();
        view.row_end();
        view.row_start();
        view.button(SAVE, "Save avatar", ButtonStyle::Primary, false);
        view.button(PROFILE, "Cancel", ButtonStyle::Default, false);
        view.row_end();
        for e in self.notices().filter(|e| !e.is_empty()) {
            view.text(TextStyle::Muted, e);
        }
        view.column_end();
    }
}

fn turn(app: &mut App, c: &mut Context, view: &mut ViewEncoder) {
    let mut dirty = !app.started;
    let old_storage_errors = (
        app.storage.world.error.clone(),
        app.storage.players.error.clone(),
        app.storage.chat.error.clone(),
    );

    if !app.started {
        app.started = true;
        app.room.start(c);

        let _ = c.wake_after(MIN_WAKE_DELAY_MS);
    }
    for e in c.raw_events() {
        match e.kind {
            EventKind::Wake => {
                if !app.frame_since_wake {
                    dirty |= app.advance(u64::from(MIN_WAKE_DELAY_MS));
                }
                app.frame_since_wake = false;
                let _ = c.wake_after(MIN_WAKE_DELAY_MS);
            }
            EventKind::Frame if e.target == BOARD => {
                if let Ok(frame) = FrameInput::decode(e.data) {
                    app.frame_since_wake = true;
                    app.frame_us += frame.elapsed_us;
                    if app.frame_us >= 32_000 {
                        let elapsed = app.frame_us / 1000;
                        app.frame_us %= 1000;
                        dirty |= app.advance(elapsed as u64);
                    }
                }
            }
            EventKind::SurfaceChanged => {
                if let Ok(s) = Surface::decode(e.data) {
                    if s.width == 0 || s.height == 0 {
                        app.held.clear();
                        continue;
                    }
                    app.density = s.pixel_scale;
                    app.surface = (s.width, s.height);
                    dirty = true;
                }
            }
            EventKind::Pointer if e.target == BOARD && app.panel == Panel::Room => {
                let layout = app.layout();
                if let Ok(p) = PointerInput::decode(e.data)
                    && p.phase == 1
                    && let Some((x, y)) = layout.at(p.x, p.y)
                {
                    app.held.clear();
                    let bounds = layout.bounds;
                    if let Some((target, _, _)) = hud::actions(bounds, app.outside(), app.chat_open)
                        .into_iter()
                        .find(|(_, _, hit)| hit.contains(x, y))
                    {
                        app.click(c, target);
                        dirty = true;
                    } else if hud::name_bounds(app.player_name(app.room.local.account), bounds)
                        .contains(x, y)
                    {
                        app.click(c, PROFILE);
                        dirty = true;
                    } else if !hud::map_bounds(bounds).contains(x, y)
                        && let Some(tile) = screen::pick(app.place(), x, y)
                    {
                        app.move_to(tile);
                        dirty = true;
                    }
                }
            }
            EventKind::Key if e.target == BOARD && app.panel == Panel::Room => {
                if let Ok(k) = KeyInput::decode(e.data) {
                    app.key(k);
                    dirty = true;
                }
            }
            EventKind::Click => {
                app.click(c, e.target);
                dirty = true;
            }
            EventKind::TextChanged if app.chat_visible() && e.target == app.chat_controls().0 => {
                app.held.clear();
                if let Ok(input) = TextInput::decode(e.data) {
                    input.text.clone_into(&mut app.draft);
                    dirty = true;
                }
            }
            EventKind::Submit if app.chat_visible() && e.target == app.chat_controls().0 => {
                if let Ok(text) = std::str::from_utf8(e.data) {
                    text.clone_into(&mut app.draft);
                    app.speak(c, text);
                    dirty = true;
                }
            }
            EventKind::IdentityChanged => {
                if let Ok(identity) = c.identity()
                    && app.room.identity_name(&identity)
                {
                    app.share_display_name();
                    dirty = true;
                }
            }
            EventKind::PresenceChanged => {
                app.room.presence(c);
                dirty = true;
            }
            EventKind::ChannelStatus => {
                if let Ok(err) = e.channel_status() {
                    app.room.failure(c, err, app.now);
                    dirty = true;
                }
            }
            EventKind::ChannelMessage => {
                if app.room.receive(e, app.now) {
                    dirty = true;
                }
            }
            EventKind::DataChanged | EventKind::VolumesChanged => {
                app.storage.world.refresh = true;
                app.storage.chat.refresh = true;
                app.storage.players.refresh = true;
            }
            EventKind::DataReady => {
                if let Ok(op) = e.operation() {
                    let roster = app.storage.players.ready(c, op, app.now);
                    if roster {
                        app.load_players();
                    }
                    let loaded = app.storage.world.loaded;
                    let public = app.storage.world.ready(c, op, app.now);
                    let mut square_lines = Vec::new();
                    if public {
                        let prev = app.place();
                        let world = &app.storage.world;
                        app.plaza
                            .reconcile(world, app.room.local.account, app.now, !loaded);
                        app.plaza.observe_pulses(world, app.now, !loaded);
                        square_lines = app.square_chat.update(
                            world.rows.iter().filter_map(|(key, row)| {
                                if let WorldKey::Chat(message) = key {
                                    Some((message, row))
                                } else {
                                    None
                                }
                            }),
                            loaded,
                        );
                        app.archive = world
                            .rows
                            .iter()
                            .filter_map(|(key, row)| match key {
                                WorldKey::Archive(session, _) if *session == app.room.session => {
                                    Conversation::decode(row)
                                }
                                _ => None,
                            })
                            .collect();
                        if prev != app.place() {
                            app.reset_walk();
                        }
                    }
                    dirty |= public || roster;
                    if public && !loaded {
                        app.share_display_name();
                    }
                    let before = (app.room.connected, app.room.guests.len());
                    app.room.ready(c, op, app.now);
                    dirty |= before != (app.room.connected, app.room.guests.len());
                    let chat_loaded = app.storage.chat.loaded;
                    let bar = app.storage.chat.ready(c, op, app.now);
                    let bar_lines = if bar {
                        app.bar_chat
                            .update(app.storage.chat.rows.iter(), chat_loaded)
                    } else {
                        Vec::new()
                    };
                    dirty |= bar || public;
                    if app.outside() {
                        for line in square_lines {
                            if let Some(g) = app.plaza.local_mut(line.guest) {
                                g.bubble = Some((line.words, app.now + 8000));
                            }
                        }
                    } else {
                        for line in bar_lines {
                            if let Some(g) = app.room.guests.get_mut(&line.guest) {
                                g.bubble = Some((line.words, app.now + 8000));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    let archive = (app.chat_visible() && !app.outside()).then_some(app.room.session);
    app.storage.world.select_archive(archive);
    let busy = [
        app.storage.world.busy(),
        app.storage.players.busy(),
        app.storage.chat.busy(),
    ]
    .into_iter()
    .filter(|b| *b)
    .count();
    let mut slots = 2_usize.saturating_sub(busy);
    app.storage.world.service(c, app.now, &mut slots);
    app.storage.players.service(c, app.now, &mut slots);
    if !app.outside() || app.storage.chat.writing() {
        app.storage.chat.service(c, app.now, &mut slots);
    }
    app.room.tick(c, app.now);
    dirty |= old_storage_errors.0 != app.storage.world.error
        || old_storage_errors.1 != app.storage.players.error
        || old_storage_errors.2 != app.storage.chat.error;
    if dirty {
        app.render(view);
        if let Err(err) = view.submit(c) {
            c.log(&format!("Room view: {err:?}"));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn the_square_and_session_have_separate_players_and_chat() {
        let mut app = App::default();
        app.room.local = Guest::new([1; 16]);
        let bar_guest = [2; 16];
        app.room.saved.insert(bar_guest, Guest::new(bar_guest));
        let passerby = Guest::new([3; 16]);
        app.plaza
            .arrive(&mut app.storage.world, passerby.clone(), [4; 16], 0);
        assert_eq!(
            app.actors().map(|(id, _)| id).collect::<Vec<_>>(),
            [[1; 16], bar_guest]
        );
        assert!(std::ptr::eq(app.conversation(), &app.bar_chat));
        let indoor_controls = app.chat_controls();
        app.draft = "private draft".into();
        app.cross();
        assert!(app.chat_visible() && app.draft.is_empty());
        let (field, send, transcript) = app.chat_controls();
        assert_ne!(field, indoor_controls.0);
        assert_ne!(send, indoor_controls.1);
        assert_ne!(transcript, indoor_controls.2);
        let outside: Vec<_> = app.actors().map(|(id, _)| id).collect();
        assert_eq!(outside, [[1; 16], passerby.account]);
        assert!(std::ptr::eq(app.conversation(), &app.square_chat));
        app.cross();
        assert_eq!(app.place(), Place::Bar);
        assert!(app.actors().all(|(id, _)| id != passerby.account));
    }
    #[test]
    fn walking_through_both_doors_shares_only_the_outdoor_position() {
        let mut a = App::default();
        let id = [5; 16];
        a.room.local = Guest::new(id);
        a.room.id = [7; 16];
        a.room.local.stand((8, 8));
        a.storage.restored = true;
        a.storage.world.loaded = true;
        a.move_to(Place::Bar.door());
        for _ in 0..24 {
            a.advance(guest::STEP_MS);
        }
        assert_eq!(a.place(), Place::Square);
        assert_eq!(a.local().cell, Place::Square.landing());
        assert_eq!(a.room.local.cell, Place::Bar.landing());
        a.held.push(2);
        for _ in 0..3 {
            a.advance(guest::STEP_MS);
        }
        a.held.clear();
        a.advance(guest::STEP_MS);
        assert_eq!(a.local().cell, (5, 5));
        assert_eq!(a.room.local.cell, Place::Bar.landing());
        a.move_to(Place::Square.door());
        for _ in 0..18 {
            a.advance(guest::STEP_MS);
        }
        assert_eq!(a.place(), Place::Bar);
        assert_eq!(a.local().cell, Place::Bar.landing());
        assert!(!a.plaza.people.contains_key(&id));
    }
}
