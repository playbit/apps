use crate::world::WorldKey;
use crate::{
    glasshouse,
    guest::{Cell, Guest, Id},
    storage::Table,
};
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Place {
    Bar,
    Square,
}
impl Place {
    pub fn door(self) -> Cell {
        match self {
            Self::Bar => (2, 0),
            Self::Square => (2, 4),
        }
    }
    pub fn landing(self) -> Cell {
        match self {
            Self::Bar => (2, 1),
            Self::Square => (2, 5),
        }
    }
    pub fn seat(self, cell: Cell) -> bool {
        match self {
            Self::Bar => glasshouse::seat(cell.0, cell.1),
            Self::Square => matches!(cell, (3, 7) | (5, 7)),
        }
    }
    pub fn walkable(self, (u, v): Cell) -> bool {
        match self {
            Self::Bar => glasshouse::walkable(u, v),
            Self::Square => {
                (0..9).contains(&u)
                    && (0..9).contains(&v)
                    && !(u <= 3 && v <= 3)
                    && !matches!((u, v), (7, 1) | (7, 6) | (1, 7) | (6, 2) | (4, 7))
            }
        }
    }
}

// The host reports presence within a session only. Across sessions a changed
// heartbeat proves liveness; an unchanged first snapshot may be days old.
const PULSE_MS: u64 = 6_000;
const SLEEP_MS: u64 = 20_000;
pub struct Visitor {
    pub avatar: Guest,
    record: Vec<u8>,
    pulse: Option<Vec<u8>>,
    seen: Option<u64>,
}
#[derive(Default)]
pub struct Plaza {
    pub people: BTreeMap<Id, Visitor>,
    serial: u64,
    next_pulse: u64,
    wave: u64,
}
impl Plaza {
    pub fn local(&self, id: Id) -> Option<&Guest> {
        self.people.get(&id).map(|v| &v.avatar)
    }
    pub fn local_mut(&mut self, id: Id) -> Option<&mut Guest> {
        self.people.get_mut(&id).map(|v| &mut v.avatar)
    }
    pub fn arrive(
        &mut self,
        world: &mut Table<WorldKey>,
        mut avatar: Guest,
        instance: Id,
        now: u64,
    ) {
        let id = avatar.account;
        avatar.place = Place::Square;
        avatar.stand(Place::Square.landing());
        avatar.sleeping = false;
        self.people.insert(
            id,
            Visitor {
                avatar,
                record: vec![],
                pulse: None,
                seen: Some(now),
            },
        );
        self.publish(world, id, instance, now, false);
    }
    pub fn leave(&mut self, world: &mut Table<WorldKey>, id: Id) {
        self.people.remove(&id);
        world.put(WorldKey::Avatar(id), vec![0]); // Only the indoor marker is public.
    }
    pub fn publish(
        &mut self,
        world: &mut Table<WorldKey>,
        id: Id,
        instance: Id,
        now: u64,
        wave: bool,
    ) {
        let Some(v) = self.people.get_mut(&id) else {
            return;
        };
        self.serial = self.serial.wrapping_add(1);
        if wave {
            self.wave = self.serial;
        }
        let mut record = vec![1];
        record.extend(v.avatar.checkpoint());
        record.extend(instance);
        record.extend(self.serial.to_le_bytes());
        record.extend(self.wave.to_le_bytes());
        v.record = record.clone();
        world.put(WorldKey::Avatar(id), record);
        self.next_pulse = now + PULSE_MS;
    }
    pub fn tick(&mut self, world: &mut Table<WorldKey>, id: Id, instance: Id, now: u64) -> bool {
        // A delayed heartbeat from another window must never overwrite a
        // newer movement or an indoor marker. Heartbeats have separate keys.
        if self.people.contains_key(&id) && now >= self.next_pulse {
            self.serial = self.serial.wrapping_add(1);
            let mut pulse = instance.to_vec();
            pulse.extend(self.serial.to_le_bytes());
            world.put(WorldKey::Pulse(id), pulse);
            self.next_pulse = now + PULSE_MS;
        }
        let mut dirty = false;
        for (&key, v) in &mut self.people {
            if key != id
                && !v.avatar.sleeping
                && v.seen.is_none_or(|t| now.saturating_sub(t) >= SLEEP_MS)
            {
                v.avatar.sleeping = true;
                dirty = true;
            }
        }
        dirty
    }
    pub fn observe_pulses(&mut self, world: &Table<WorldKey>, now: u64, first: bool) {
        for (&id, visitor) in &mut self.people {
            let next = world
                .rows
                .get(&WorldKey::Pulse(id))
                .filter(|p| p.len() == 24);
            if next == visitor.pulse.as_ref() {
                continue;
            }
            visitor.pulse = next.cloned();
            if next.is_some() && !first {
                visitor.avatar.sleeping = false;
                visitor.seen = Some(now);
            }
        }
    }
    pub fn reconcile(&mut self, world: &Table<WorldKey>, local: Id, now: u64, initial: bool) {
        let pending = world.pending_where(|key| *key == WorldKey::Avatar(local)) > 0;
        self.people.retain(|key, _| {
            (*key == local && pending)
                || world
                    .rows
                    .get(&WorldKey::Avatar(*key))
                    .is_some_and(|b| b.len() == 39 && b[0] == 1)
        });
        for (key, b) in &world.rows {
            let &WorldKey::Avatar(id) = key else { continue };
            if b.len() != 39 || b[0] != 1 || (id == local && pending) {
                continue;
            }
            let Some(mut avatar) = Guest::restore_in(id, &b[1..7], Place::Square) else {
                self.people.remove(&id);
                continue;
            };
            if let Some(v) = self.people.get_mut(&id) {
                if &v.record == b {
                    continue;
                }
                let waved = v.record.get(31..39) != b.get(31..39) && b[31..39] != [0; 8];
                v.avatar.follow(&avatar, now);
                if waved {
                    v.avatar.wave_until = now + 2300;
                }
                v.avatar.sleeping = false;
                v.seen = Some(now);
                v.record = b.clone();
            } else {
                avatar.sleeping = initial && id != local;
                self.people.insert(
                    id,
                    Visitor {
                        avatar,
                        record: b.clone(),
                        pulse: world.rows.get(&WorldKey::Pulse(id)).cloned(),
                        seen: (!initial || id == local).then_some(now),
                    },
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::Storage;
    #[test]
    fn edits_in_the_plaza_reach_the_room_and_the_saved_avatar() {
        use crate::{App, avatar_editor::Appearance};
        let mut app = App::default();
        app.storage.restored = true;
        app.cross();
        app.appearance_draft = Appearance::from(app.local());
        for (field, value) in [(20, 26), (30, 34), (40, 44), (50, 54), (60, 64)] {
            app.appearance_draft.click(field, &mut app.avatar_field);
            app.appearance_draft.click(value, &mut app.avatar_field);
        }
        app.save();
        assert_eq!((app.local().look, app.local().face), (5, 255));
        let id = app.local().account;
        let wire = &app.plaza.people[&id].record;
        let remote = Guest::restore_in(id, &wire[1..7], Place::Square).unwrap();
        assert_eq!((remote.look, remote.face), (5, 255));
        app.cross();
        let saved = Guest::restore(id, &app.room.local.checkpoint()).unwrap();
        assert_eq!((saved.look, saved.face), (5, 255));
    }

    #[test]
    fn late_pulse_cannot_put_an_indoor_player_back_outside() {
        let id = [1; 16];
        let observer = [2; 16];
        let mut remote = Plaza::default();
        let mut world = Storage::default().world;
        world.rows.insert(WorldKey::Avatar(id), vec![0]);
        remote.reconcile(&world, observer, 1000, false);
        world.rows.insert(WorldKey::Pulse(id), vec![7; 24]);
        remote.observe_pulses(&world, 1100, false);
        assert!(remote.local(id).is_none());
    }
    #[test]
    fn remote_session_observes_arrival_sleep_wake_and_return_to_private() {
        let mut sender = Plaza::default();
        let mut world = Storage::default().world;
        let mut other = Storage::default().world;
        let mut observer = Plaza::default();
        let id = [7; 16];
        let reader = [9; 16];
        let instance = [1; 16];
        sender.arrive(&mut world, Guest::new(id), instance, 0);
        other
            .rows
            .insert(WorldKey::Avatar(id), sender.people[&id].record.clone());
        observer.reconcile(&other, reader, 0, true);
        assert!(observer.people[&id].avatar.sleeping);
        sender.local_mut(id).unwrap().go((1, 5), 500);
        sender.publish(&mut world, id, instance, 500, true);
        other
            .rows
            .insert(WorldKey::Avatar(id), sender.people[&id].record.clone());
        observer.reconcile(&other, reader, 500, false);
        let visitor = &observer.people[&id].avatar;
        assert_eq!(visitor.cell, (1, 5));
        assert!(!visitor.sleeping && visitor.wave_until > 500);
        observer.tick(&mut other, reader, [3; 16], 500 + SLEEP_MS);
        assert!(observer.people[&id].avatar.sleeping);
        other.rows.insert(WorldKey::Avatar(id), vec![0]);
        observer.reconcile(&other, reader, 30_000, false);
        assert!(!observer.people.contains_key(&id));
    }
}
