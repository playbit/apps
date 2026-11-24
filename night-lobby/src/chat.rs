use crate::guest::Id;
use crate::room::clean;
use playbit_api::abi::environment::DISPLAY_NAME_BYTES;
use playbit_api::context::Context;
use std::collections::BTreeMap;

const MAX_MESSAGE_CHARS: usize = 240;
pub const MAX_RECORD_BYTES: u32 = (18 + DISPLAY_NAME_BYTES + 4 * MAX_MESSAGE_CHARS) as u32;

#[derive(Clone)]
pub struct Line {
    pub guest: Id,
    pub name: String,
    pub words: String,
}

// Chat keys sort by logical time, then by a random message ID for simultaneous
// sends. Their 24-byte length separates private chat from 16-byte avatar keys
// in existing Session stores, including rooms created with an older build.
#[derive(Default)]
pub struct Conversation {
    pub lines: BTreeMap<[u8; 24], Line>,
    counter: u64,
    pub notice: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chat_round_trips_full_profile_names_and_reads_legacy_messages() {
        let words = "🐈".repeat(MAX_MESSAGE_CHARS);
        for name in [
            "Alf".to_owned(),
            "Renée with a long display name".to_owned(),
            "猫".repeat(85),
            "🪴".repeat(64),
        ] {
            let record = encode_line([7; 16], &name, &words);
            assert!(record.len() <= MAX_RECORD_BYTES as usize);
            let line = Conversation::decode(&record).unwrap();
            assert_eq!(
                (line.guest, line.name, &line.words),
                ([7; 16], name, &words)
            );
        }
        let legacy = [&[1][..], &[3; 16], &[3], b"Eli", b"Hello"].concat();
        let line = Conversation::decode(&legacy).unwrap();
        assert_eq!((line.name.as_str(), line.words.as_str()), ("Eli", "Hello"));
    }
}
fn encode_line(account: Id, name: &str, words: &str) -> Vec<u8> {
    debug_assert!(!name.is_empty() && name.len() <= DISPLAY_NAME_BYTES);
    let mut body = vec![1];
    body.extend(account);
    body.push(name.len() as u8);
    body.extend(name.as_bytes());
    body.extend(words.as_bytes());
    body
}

impl Conversation {
    pub fn decode(v: &[u8]) -> Option<Line> {
        if v.len() < 19 || v[0] != 1 {
            return None;
        }
        // v1 has a one-byte name length. Zero was unused because names cannot
        // be empty; it represents a full 256-byte Playbit display name.
        let name_bytes = if v[17] == 0 {
            DISPLAY_NAME_BYTES
        } else {
            usize::from(v[17])
        };
        let (name, text) = v[18..].split_at_checked(name_bytes)?;
        let name = clean(std::str::from_utf8(name).ok()?, DISPLAY_NAME_BYTES);
        let words = clean(std::str::from_utf8(text).ok()?, MAX_MESSAGE_CHARS);
        if name.is_empty() || words.is_empty() {
            return None;
        }
        Some(Line {
            guest: v[1..17].try_into().ok()?,
            name,
            words,
        })
    }
    pub fn send(
        &mut self,
        c: &mut Context,
        account: Id,
        name: &str,
        s: &str,
        loaded: bool,
        pending: usize,
    ) -> Option<([u8; 24], Vec<u8>, String)> {
        let s = clean(s, MAX_MESSAGE_CHARS + 1);
        if s.is_empty() {
            return None;
        }
        if s.chars().count() > MAX_MESSAGE_CHARS {
            self.notice = "Keep each message to 240 characters.".into();
            return None;
        }
        if !loaded {
            self.notice = "Chat is still loading.".into();
            return None;
        }
        if pending >= 8 {
            self.notice = "Waiting for earlier messages to save.".into();
            return None;
        }
        let mut key = [0; 24];
        if c.random_fill(&mut key[8..]).is_err() {
            self.notice = "Couldn’t send. Try again.".into();
            return None;
        }
        self.counter = self.counter.checked_add(1)?;
        key[..8].copy_from_slice(&self.counter.to_be_bytes());
        let body = encode_line(account, name, &s);
        self.notice.clear();
        Some((key, body, s))
    }
    pub fn update<'a>(
        &mut self,
        rows: impl Iterator<Item = (&'a [u8; 24], &'a Vec<u8>)>,
        loaded: bool,
    ) -> Vec<Line> {
        let next: BTreeMap<_, _> = rows
            .filter_map(|(&k, v)| Self::decode(v).map(|line| (k, line)))
            .collect();
        if let Some((k, _)) = next.last_key_value() {
            self.counter = self
                .counter
                .max(u64::from_be_bytes(k[..8].try_into().unwrap()));
        }
        let fresh = next
            .iter()
            .filter(|(k, _)| loaded && !self.lines.contains_key(*k))
            .map(|(_, v)| v.clone())
            .collect();
        self.lines = next;
        self.notice.clear();
        fresh
    }
}
