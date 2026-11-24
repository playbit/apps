// World keys sort by namespace, then by account or chronological message key.
// The legacy lounge log included a session ID; keep that boundary in its key.
use crate::guest::Id;
use crate::storage::{Key, RowKind};
use playbit_api::abi::environment::DISPLAY_NAME_BYTES;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum WorldKey {
    Name(Id),
    Avatar(Id),
    Pulse(Id),
    Chat([u8; 24]),
    Archive([u8; 32], [u8; 24]),
}

impl Key for WorldKey {
    fn encode(&self) -> Vec<u8> {
        let (prefix, bytes): (u8, &[u8]) = match self {
            Self::Name(id) => (1, id),
            Self::Avatar(id) => (2, id),
            Self::Pulse(id) => (3, id),
            Self::Chat(message) => (4, message),
            Self::Archive(session, message) => return [&[5][..], session, message].concat(),
        };
        [&[prefix], bytes].concat()
    }

    fn decode(key: &[u8]) -> Option<Self> {
        let (&prefix, bytes) = key.split_first()?;
        Some(match prefix {
            1 => Self::Name(bytes.try_into().ok()?),
            2 => Self::Avatar(bytes.try_into().ok()?),
            3 => Self::Pulse(bytes.try_into().ok()?),
            4 => Self::Chat(bytes.try_into().ok()?),
            5 => {
                let (session, message) = bytes.split_at_checked(32)?;
                Self::Archive(session.try_into().ok()?, message.try_into().ok()?)
            }
            _ => return None,
        })
    }

    fn row_kind(&self, archive: Option<&[u8; 32]>) -> RowKind {
        match self {
            Self::Name(_) => RowKind::Current(1 + DISPLAY_NAME_BYTES as u32),
            Self::Avatar(_) => RowKind::Current(39),
            Self::Pulse(_) => RowKind::Current(24),
            Self::Chat(_) => RowKind::Chat,
            Self::Archive(session, _) if Some(session) == archive => RowKind::Archive,
            Self::Archive(..) => RowKind::Skip,
        }
    }
}

// Earlier builds stored editable nicknames as raw UTF-8 under this same key.
// A tagged value identifies a name read from the Playbit profile.
pub fn encode_display_name(name: &str) -> Vec<u8> {
    [&[1][..], name.as_bytes()].concat()
}

pub fn cached_display_name(bytes: &[u8]) -> Option<&str> {
    let raw = bytes.strip_prefix(&[1])?;
    let name = std::str::from_utf8(raw).ok()?;
    (!name.is_empty()
        && raw.len() <= DISPLAY_NAME_BYTES
        && !name
            .chars()
            .any(|c| c.is_control() || matches!(c, '\u{2028}' | '\u{2029}')))
    .then_some(name)
}
