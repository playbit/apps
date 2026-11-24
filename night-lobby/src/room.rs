use crate::glasshouse::walkable;
use crate::guest::{Guest, Id};
use playbit_api::abi::{environment::DISPLAY_NAME_BYTES, operations::Identity, syscall::Error};
use playbit_api::context::{Channel, Completion, Context, Event, OperationHandle, Participant};
use std::collections::BTreeMap;

pub fn clean(s: &str, max: usize) -> String {
    s.trim()
        .chars()
        .filter(|ch| !ch.is_control() && !matches!(*ch, '\u{2028}' | '\u{2029}'))
        .take(max)
        .collect()
}

pub const UNNAMED: &str = "Guest";

fn display_name(raw: Option<&str>) -> String {
    let name = clean(raw.unwrap_or(""), DISPLAY_NAME_BYTES);
    if name.trim().is_empty() {
        UNNAMED.to_owned()
    } else {
        name
    }
}

pub struct Room {
    pub id: Id,
    pub local: Guest,
    pub guests: BTreeMap<Id, Guest>,
    pub saved: BTreeMap<Id, Guest>,
    // Keep the last presence observation while a departed player's World record syncs.
    pub display_names: BTreeMap<Id, String>,
    instances: BTreeMap<Id, (Id, u64)>,
    sequence: u64,
    pub connected: bool,
    channel: Option<Channel>,
    opening: Option<OperationHandle>,
    sending: Option<OperationHandle>,
    pub changed: bool,
    hello: bool,
    last_error: Option<Error>,
    retry_at: u64,
    last_send: u64,
    pub session: [u8; 32],
}

impl Default for Room {
    fn default() -> Self {
        Self {
            id: [0; 16],
            local: Guest::new([0; 16]),
            guests: BTreeMap::new(),
            saved: BTreeMap::new(),
            display_names: BTreeMap::new(),
            instances: BTreeMap::new(),
            sequence: 0,
            connected: false,
            channel: None,
            opening: None,
            sending: None,
            changed: true,
            hello: true,
            last_error: None,
            retry_at: 0,
            last_send: 0,
            session: [0; 32],
        }
    }
}

impl Room {
    pub fn actors(&self) -> impl Iterator<Item = (Id, &Guest)> {
        std::iter::once((self.local.account, &self.local))
            .chain(self.guests.iter().map(|(&id, g)| (id, g)))
            .chain(
                self.saved
                    .iter()
                    .filter(|(account, _)| {
                        **account != self.local.account && !self.guests.contains_key(*account)
                    })
                    .map(|(&account, g)| (account, g)),
            )
    }

    pub fn start(&mut self, c: &mut Context) {
        if let Ok(identity) = c.identity() {
            self.id = identity.instance_id;
            self.session = identity.session_id;
            self.local = Guest::new(identity.account_id);
            self.local.sleeping = true;
            self.identity_name(&identity);
            self.presence(c);
        }
        self.tick(c, 0);
    }
    pub fn identity_name(&mut self, identity: &Identity) -> bool {
        let Ok(name) = identity.display_name() else {
            return false;
        };
        if identity.account_id != self.local.account {
            return false;
        }
        let name = display_name(name);
        if self.display_names.get(&identity.account_id) == Some(&name) {
            return false;
        }
        self.display_names.insert(identity.account_id, name);
        self.changed = true;
        true
    }

    pub fn presence(&mut self, c: &mut Context) {
        let mut people = vec![Participant::default(); 256];
        let Ok(list) = c.presence(&mut people) else {
            return;
        };
        self.apply_presence(list);
    }
    fn apply_presence(&mut self, list: &[Participant]) {
        let mut online = BTreeMap::new();
        for p in list {
            let name = online.entry(p.account_id).or_insert(None);
            if name.is_none() {
                *name = p.display_name().ok().flatten();
            }
        }
        self.local.sleeping = !online.contains_key(&self.local.account);
        self.instances
            .retain(|id, _| list.iter().any(|p| p.instance_id == *id));
        self.guests.retain(|id, _| online.contains_key(id));
        for (id, name) in online {
            // The local identity may already be newer than this roster snapshot.
            if id != self.local.account || !self.display_names.contains_key(&id) {
                self.display_names.insert(id, display_name(name));
            }
        }
        for p in list {
            self.instances
                .entry(p.instance_id)
                .or_insert((p.account_id, 0));
            if p.account_id != self.local.account
                && let std::collections::btree_map::Entry::Vacant(slot) =
                    self.guests.entry(p.account_id)
            {
                let mut g = self
                    .saved
                    .get(&p.account_id)
                    .cloned()
                    .unwrap_or_else(|| Guest::new(p.account_id));
                g.sleeping = false;
                slot.insert(g);
                self.changed = true;
            }
        }
    }
    pub fn restore_peers(&mut self, now: u64) {
        for (id, guest) in &mut self.guests {
            let live = self.connected
                && self
                    .instances
                    .values()
                    .any(|(account, seq)| account == id && *seq > 0);
            if !live && let Some(saved) = self.saved.get(id) {
                guest.follow(saved, now);
            }
        }
    }
    pub fn tick(&mut self, c: &mut Context, now: u64) {
        if self.id == [0; 16] {
            return;
        }
        if !self.connected {
            if self.opening.is_none() && now >= self.retry_at {
                let channel = match self.channel {
                    Some(ch) => Ok(ch),
                    None => c
                        .session()
                        .and_then(|s| s.channel(c, "night-lobby.glasshouse.v1")),
                };
                match channel.and_then(|ch| {
                    self.channel = Some(ch);
                    ch.subscribe(c)
                }) {
                    Ok(op) => self.opening = Some(op),
                    Err(e) => self.failure(c, e, now),
                }
                self.retry_at = now + 2500;
            }
            return;
        }
        if self.sending.is_some() || now < self.retry_at {
            return;
        }
        // Movement, waves and joins send immediately. An occasional snapshot
        // repairs a dropped final step while people are otherwise idle.
        if !self.changed && (self.guests.is_empty() || now - self.last_send < 15_000) {
            return;
        }
        let ch = self.channel.unwrap();
        let a = &mut self.local;
        self.sequence += 1;
        let mut msg = vec![
            1,
            self.hello as u8,
            a.look,
            a.face,
            a.cell.0 as u8,
            a.cell.1 as u8,
            a.direction,
        ];
        msg.extend_from_slice(&((a.wave_until.saturating_sub(now)).min(2400) as u16).to_le_bytes());
        msg.extend_from_slice(&self.sequence.to_le_bytes());
        // Keep the v1 name suffix readable by older guests. Current guests use presence.
        let name = self
            .display_names
            .get(&a.account)
            .map_or(UNNAMED, String::as_str);
        let end = name.char_indices().nth(16).map_or(name.len(), |(i, _)| i);
        msg.extend_from_slice(name[..end].as_bytes());
        match ch.publish(c, &msg) {
            Ok(op) => {
                self.sending = Some(op);
                self.changed = false;
                self.hello = false;
                self.last_send = now;
            }
            Err(e) => self.failure(c, e, now),
        }
    }
    pub fn ready(&mut self, c: &mut Context, op: OperationHandle, now: u64) {
        if self.opening == Some(op) {
            match c.take_channel(op) {
                Ok(Completion::Pending) => return,
                Ok(Completion::Ready(())) => {
                    self.connected = true;
                    self.last_error = None;
                    self.retry_at = 0;
                    self.hello = true;
                    self.changed = true;
                    self.presence(c);
                }
                Err(e) => self.failure(c, e, now),
            }
            self.opening = None;
        } else if self.sending == Some(op) {
            match c.take_channel(op) {
                Ok(Completion::Pending) => return,
                Err(e) => self.failure(c, e, now),
                _ => {}
            }
            self.sending = None;
        }
    }
    pub fn failure(&mut self, c: &mut Context, e: Error, now: u64) {
        if self.last_error != Some(e) {
            c.log(&format!("Live room connection: {e:?}"));
            self.last_error = Some(e);
        }
        self.changed = true;
        self.retry_at = now + 2000;
        if matches!(
            e,
            Error::Unavailable | Error::AccessDenied | Error::StaleHandle
        ) {
            self.connected = false;
            self.presence(c);
            self.hello = true;
            if e == Error::StaleHandle {
                self.channel = None;
            }
        }
    }
    pub fn receive(&mut self, event: Event, now: u64) -> bool {
        let Ok(m) = event.channel_message() else {
            return false;
        };
        if Some(m.channel) != self.channel || m.sender.instance_id == self.id {
            return false;
        }
        let b = m.data;
        if !(18..=81).contains(&b.len()) || b[0] != 1 || b[2] > 5 || b[6] > 3 {
            return false;
        }
        let cell = (b[4] as i32, b[5] as i32);
        if !walkable(cell.0, cell.1) {
            return false;
        }
        let seq = u64::from_le_bytes(b[9..17].try_into().unwrap());
        let Some((account, previous)) = self.instances.get_mut(&m.sender.instance_id) else {
            return false;
        };
        if *account != m.sender.account_id {
            return false;
        }
        if b[1] == 1 {
            self.changed = true;
        } else if seq <= *previous {
            return false;
        }
        let first = *previous == 0;
        *previous = seq;
        let Some(peer) = self.guests.get_mut(account) else {
            return false;
        };
        let old = (
            peer.cell,
            peer.look,
            peer.face,
            peer.direction,
            peer.wave_until > now,
        );
        if first || b[1] == 1 {
            peer.cell = cell;
            peer.from = (cell.0 as f32, cell.1 as f32);
        } else {
            peer.go(cell, now);
        }
        peer.look = b[2];
        peer.face = b[3];
        peer.direction = b[6];
        peer.wave_until = now + u16::from_le_bytes([b[7], b[8]]).min(2400) as u64;
        old != (
            peer.cell,
            peer.look,
            peer.face,
            peer.direction,
            peer.wave_until > now,
        ) || b[1] == 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_and_presence_names_take_precedence_over_saved_nicknames() {
        use crate::{App, WorldKey, world::encode_display_name};
        let mut app = App::default();
        let me = [1; 16];
        let peer = [2; 16];
        app.room.local = Guest::new(me);
        app.storage.world.loaded = true;
        app.storage
            .world
            .rows
            .insert(WorldKey::Name(me), b"old nickname".to_vec());
        assert_eq!(app.player_name(me), UNNAMED);

        let mut identity = Identity {
            account_id: me,
            ..Default::default()
        };
        identity.written = identity.size;
        identity.display_name[..3].copy_from_slice(b"Ras");
        assert!(app.room.identity_name(&identity));
        app.share_display_name();
        assert_eq!(
            app.storage
                .world
                .pending_where(|k| *k == WorldKey::Name(me)),
            1
        );

        let participant = |account, instance, name: &str| {
            let mut p = Participant {
                account_id: account,
                instance_id: [instance; 16],
                ..Default::default()
            };
            p.written = p.size;
            p.display_name[..name.len()].copy_from_slice(name.as_bytes());
            p
        };
        let local = participant(me, 3, "stale roster");
        let empty = participant(peer, 4, "");
        app.room
            .apply_presence(&[local, participant(peer, 5, "李明"), empty]);
        assert_eq!(app.player_name(me), "Ras");
        assert_eq!(app.player_name(peer), "李明");
        app.room
            .apply_presence(&[empty, participant(peer, 5, "李明 🐈")]);
        assert_eq!(app.player_name(peer), "李明 🐈");
        app.room.apply_presence(&[]);
        assert_eq!(app.player_name(peer), "李明 🐈");
        app.storage
            .world
            .rows
            .insert(WorldKey::Name(peer), encode_display_name("New plaza name"));
        assert_eq!(app.player_name(peer), "New plaza name");
        identity.display_name.fill(0);
        assert!(app.room.identity_name(&identity));
        app.room.apply_presence(&[local]);
        assert_eq!(app.player_name(me), UNNAMED);
    }

    #[test]
    fn last_connection_sleeps_and_rejoining_wakes_one_saved_avatar() {
        let mut room = Room {
            id: [1; 16],
            local: Guest::new([2; 16]),
            ..Room::default()
        };
        let visitor = [3; 16];
        let state = [1, 3, 49, 6, 4, 1];
        room.saved
            .insert(visitor, Guest::restore(visitor, &state).unwrap());
        let local = Participant {
            instance_id: room.id,
            account_id: room.local.account,
            ..Participant::default()
        };
        let a = Participant {
            instance_id: [4; 16],
            account_id: visitor,
            ..Participant::default()
        };
        let b = Participant {
            instance_id: [5; 16],
            ..a
        };
        for (list, asleep) in [
            (vec![local], true),
            (vec![local, a, b], false),
            (vec![local, b], false),
            (vec![local], true),
            (vec![local, a], false),
        ] {
            room.apply_presence(&list);
            assert_eq!(room.actors().count(), 2);
            let guest = room.actors().find(|(id, _)| id == &visitor).unwrap().1;
            assert_eq!(guest.sleeping, asleep);
            assert_eq!(guest.checkpoint(), state);
        }
    }
}
