use crate::chat::MAX_RECORD_BYTES;
use crate::world::WorldKey;
use playbit_api::abi::{data, kv, syscall::Error};
use playbit_api::context::{Completion, Context, EntryHandle, OperationHandle, ReadError};
use std::collections::{BTreeMap, VecDeque};

pub enum RowKind {
    Current(u32),
    Chat,
    Archive,
    Skip,
}

pub trait Key: Copy + Ord {
    fn encode(&self) -> Vec<u8>;
    fn decode(bytes: &[u8]) -> Option<Self>;
    fn row_kind(&self, archive: Option<&[u8; 32]>) -> RowKind;
}

impl Key for [u8; 16] {
    fn encode(&self) -> Vec<u8> {
        self.to_vec()
    }
    fn decode(bytes: &[u8]) -> Option<Self> {
        bytes.try_into().ok()
    }
    fn row_kind(&self, _: Option<&[u8; 32]>) -> RowKind {
        RowKind::Current(6)
    }
}

impl Key for [u8; 24] {
    fn encode(&self) -> Vec<u8> {
        self.to_vec()
    }
    fn decode(bytes: &[u8]) -> Option<Self> {
        bytes.try_into().ok()
    }
    fn row_kind(&self, _: Option<&[u8; 32]>) -> RowKind {
        RowKind::Chat
    }
}

enum Request<K> {
    Open,
    Keys,
    Read(K),
    Put(K, Vec<u8>),
}

// Each page and its values use the same immutable KV root. Publish the new
// map in one step so a partially loaded roster never makes avatars disappear.
struct Scan<K> {
    archive: Option<[u8; 32]>,
    root: Option<kv::Root>,
    after: Option<Vec<u8>>,
    keys: VecDeque<K>,
    more: bool,
    recent: [VecDeque<K>; 2],
    rows: BTreeMap<K, Vec<u8>>,
}

impl<K> Default for Scan<K> {
    fn default() -> Self {
        Self {
            archive: None,
            root: None,
            after: None,
            keys: VecDeque::new(),
            more: false,
            rows: BTreeMap::new(),
            recent: Default::default(),
        }
    }
}

pub struct Table<K: Key = [u8; 16]> {
    role: data::VolumeRole,
    name: &'static str,
    max_bytes: u32,
    label: &'static str,
    entry: Option<EntryHandle>,
    active: Option<(OperationHandle, Request<K>)>,
    scan: Option<Scan<K>>,
    last_root: Option<kv::Root>,
    pub loaded: bool,
    pub rows: BTreeMap<K, Vec<u8>>,
    archive: Option<[u8; 32]>,
    queued: VecDeque<(K, Vec<u8>)>,
    pub refresh: bool,
    last_archive: Option<[u8; 32]>,
    retry_at: u64,
    scan_retry_at: u64,
    scan_failures: u32,
    pub error: String,
}

impl<K: Key> Table<K> {
    pub fn new(
        role: data::VolumeRole,
        name: &'static str,
        max_bytes: u32,
        label: &'static str,
    ) -> Self {
        Self {
            role,
            name,
            max_bytes,
            label,
            entry: None,
            active: None,
            scan: None,
            last_root: None,
            loaded: false,
            rows: BTreeMap::new(),
            archive: None,
            queued: VecDeque::new(),
            refresh: true,
            last_archive: None,
            retry_at: 0,
            scan_retry_at: 0,
            scan_failures: 0,
            error: String::new(),
        }
    }

    pub fn select_archive(&mut self, session: Option<[u8; 32]>) {
        if self.archive != session {
            self.archive = session;
            self.refresh = true;
        }
    }

    pub fn put(&mut self, key: K, value: Vec<u8>) {
        self.queued.retain(|(queued, _)| *queued != key);
        self.queued.push_back((key, value));
    }

    pub fn pending_where(&self, matches: impl Fn(&K) -> bool) -> usize {
        self.queued.iter().filter(|(key, _)| matches(key)).count()
            + usize::from(matches!(&self.active, Some((_, Request::Put(key, _))) if matches(key)))
    }

    pub fn pending(&self) -> usize {
        self.queued.len() + usize::from(matches!(self.active, Some((_, Request::Put(..)))))
    }
    pub fn writing(&self) -> bool {
        self.pending() != 0
    }

    fn open(&self, c: &mut Context) -> Result<OperationHandle, Error> {
        let mut records = [data::Volume::default(); 256];
        let mut tags = [data::Range::default(); 1024];
        let mut names = vec![0; 16384];
        let volumes = c
            .available_volumes(&mut records, &mut tags, &mut names)
            .map_err(|e| match e {
                ReadError::Host(e) => e,
                ReadError::BufferTooSmall { .. } => Error::BufferTooSmall,
            })?;
        let handle = volumes
            .volume(self.role)
            .or_else(|| {
                volumes
                    .iter()
                    .find(|v| self.role == data::VolumeRole::Root && v.is_root)
            })
            .ok_or(Error::Unavailable)?
            .handle;
        c.begin_resolve_entry(handle, self.name)
    }

    fn failed(&mut self, request: Request<K>, error: Error, now: u64) -> Option<String> {
        let reading = matches!(request, Request::Keys | Request::Read(_));
        let stage = match request {
            Request::Open => "resolve",
            Request::Keys => "list",
            Request::Read(_) => "read",
            Request::Put(id, value) => {
                if !self.queued.iter().any(|(key, _)| *key == id) {
                    self.queued.push_front((id, value));
                }
                "write"
            }
        };
        // Playbit can retire a completed read if a newer change reaches its
        // guest first. Retry the same page or row. A root conflict specifically
        // means the contents changed, so start a new snapshot in that case.
        if reading && matches!(error, Error::Unavailable | Error::Conflict) {
            if error == Error::Conflict {
                self.restart_scan();
            }
            self.scan_failures = self.scan_failures.saturating_add(1);
            let delay = match self.scan_failures {
                1 => 80,
                2 => 200,
                3 => 500,
                _ => 3000,
            };
            self.scan_retry_at = now.saturating_add(delay);
            if self.scan_failures <= 3 {
                return None;
            }
        } else {
            if matches!(error, Error::StaleHandle | Error::NotFound) {
                self.entry = None;
            }
            self.restart_scan();
            self.retry_at = now.saturating_add(3000);
        }
        let message = format!("{} ({stage}): {error:?}. Retrying…", self.label);
        if self.error == message {
            return None;
        }
        self.error = message.clone();
        Some(message)
    }

    fn restart_scan(&mut self) {
        self.scan = None;
        self.refresh = true;
    }

    fn accept_page(&mut self, page: kv::Page) {
        let scan = self.scan.as_mut().unwrap();
        let first = scan.after.is_none();
        if first && self.loaded && page.root == self.last_root && scan.archive == self.last_archive
        {
            self.scan = None;
            return;
        }
        scan.root = page.root;
        for key in page.keys {
            let Some(id) = K::decode(key.bytes) else {
                continue;
            };
            let recent = match id.row_kind(scan.archive.as_ref()) {
                RowKind::Current(max) if key.len_bytes <= u64::from(max) => {
                    scan.keys.push_back(id);
                    continue;
                }
                RowKind::Chat if key.len_bytes <= u64::from(MAX_RECORD_BYTES) => {
                    &mut scan.recent[0]
                }
                RowKind::Archive if key.len_bytes <= u64::from(MAX_RECORD_BYTES) => {
                    &mut scan.recent[1]
                }
                _ => continue,
            };
            if recent.len() == 80 {
                recent.pop_front();
            }
            recent.push_back(id);
        }
        scan.after = page.next.map(Vec::from);
        scan.more = page.next.is_some();
        if !scan.more {
            for history in &mut scan.recent {
                scan.keys.append(history);
            }
        }
    }

    fn finish_read(&mut self) -> (bool, Option<String>) {
        if self
            .scan
            .as_ref()
            .is_some_and(|s| !s.keys.is_empty() || s.more)
        {
            return (false, None);
        }
        let mut changed = false;
        if let Some(scan) = self.scan.take() {
            if scan.archive != self.archive {
                self.refresh = true;
                return (false, None);
            }
            self.last_archive = scan.archive;
            changed = !self.loaded || self.rows != scan.rows;
            self.rows = scan.rows;
            self.last_root = scan.root;
            self.loaded = true;
        }
        let reported = !self.error.is_empty();
        self.error.clear();
        self.scan_retry_at = 0;
        let failures = std::mem::take(&mut self.scan_failures);
        let log = (reported && failures != 0).then(|| {
            format!(
                "{} snapshot recovered ({failures} read retries)",
                self.label
            )
        });
        (changed, log)
    }

    pub fn busy(&self) -> bool {
        self.active.is_some()
    }
    pub fn service(&mut self, c: &mut Context, now: u64, slots: &mut usize) {
        if *slots == 0 || self.busy() {
            return;
        }
        self.tick(c, now);
        *slots -= usize::from(self.busy());
    }
    fn next_request(&mut self, now: u64) -> Option<Request<K>> {
        if self.busy() || now < self.retry_at {
            return None;
        }
        if self.entry.is_none() {
            return Some(Request::Open);
        }
        if (self.scan.is_none() || now < self.scan_retry_at)
            && let Some((id, value)) = self.queued.pop_front()
        {
            return Some(Request::Put(id, value));
        }
        if now < self.scan_retry_at {
            return None;
        }
        if self.scan.is_none() && self.refresh {
            self.refresh = false;
            self.scan = Some(Scan {
                archive: self.archive,
                ..Default::default()
            });
        }
        let scan = self.scan.as_ref()?;
        if let Some(&id) = scan.keys.front() {
            return Some(Request::Read(id));
        }
        Some(Request::Keys)
    }

    fn tick(&mut self, c: &mut Context, now: u64) {
        let Some(request) = self.next_request(now) else {
            return;
        };
        let result = match (self.entry, &request) {
            (_, Request::Open) => self.open(c),
            (Some(e), Request::Put(id, value)) => c.begin_kv_put(e, &id.encode(), value),
            (Some(e), Request::Read(id)) => c.begin_kv_read(
                e,
                &id.encode(),
                0,
                self.max_bytes,
                self.scan.as_ref().unwrap().root,
            ),
            (Some(e), Request::Keys) => {
                let scan = self.scan.as_ref().unwrap();
                c.begin_kv_keys(e, scan.after.as_deref(), scan.root, kv::MAX_KEYS as u32)
            }
            (None, _) => unreachable!("resolve the KV entry before accessing it"),
        };
        match result {
            Ok(op) => self.active = Some((op, request)),
            Err(e) => {
                if let Some(message) = self.failed(request, e, now) {
                    c.log(&message);
                }
            }
        }
    }

    pub fn ready(&mut self, c: &mut Context, op: OperationHandle, now: u64) -> bool {
        if self.active.as_ref().map(|v| v.0) != Some(op) {
            return false;
        }
        let (_, request) = self.active.take().unwrap();
        let mut changed = false;
        let result = match &request {
            Request::Open => c.take_resolve_entry(op).map(|r| match r {
                Completion::Pending => false,
                Completion::Ready(e) => {
                    self.entry = Some(e);
                    true
                }
            }),
            Request::Put(_, _) => c.take_kv_put(op).map(|r| match r {
                Completion::Pending => false,
                Completion::Ready(()) => {
                    // Only a complete snapshot becomes visible to readers.
                    self.restart_scan();
                    true
                }
            }),
            Request::Keys => {
                let mut buffer = vec![0; kv::MAX_PAGE_BYTES];
                c.take_kv_keys(op, &mut buffer).map(|r| match r {
                    Completion::Pending => false,
                    Completion::Ready(page) => {
                        self.accept_page(page);
                        true
                    }
                })
            }
            Request::Read(id) => {
                let mut buffer = vec![0; 49 + self.max_bytes as usize];
                match c.take_kv_read(op, &mut buffer) {
                    Ok(Completion::Pending) => Ok(false),
                    Ok(Completion::Ready(chunk)) => {
                        let scan = self.scan.as_mut().unwrap();
                        if Some(chunk.root) != scan.root
                            || chunk.offset != 0
                            || chunk.total_bytes != chunk.bytes.len() as u64
                        {
                            Err(Error::Conflict)
                        } else {
                            scan.rows.insert(*id, chunk.bytes.to_vec());
                            Ok(true)
                        }
                    }
                    Err(Error::NotFound) => Err(Error::Conflict),
                    Err(e) => Err(e),
                }
            }
        };
        match result {
            Ok(false) => self.active = Some((op, request)),
            Ok(true) => {
                if matches!(request, Request::Read(_)) {
                    self.scan.as_mut().unwrap().keys.pop_front();
                }
                if matches!(request, Request::Keys | Request::Read(_)) {
                    let (published, log) = self.finish_read();
                    changed = published;
                    if let Some(message) = log {
                        c.log(&message);
                    }
                }
            }
            Err(err) => {
                if let Some(message) = self.failed(request, err, now) {
                    c.log(&message);
                }
            }
        }
        changed
    }
}

pub struct Storage {
    pub players: Table,
    pub world: Table<WorldKey>,
    pub chat: Table<[u8; 24]>,
    pub restored: bool,
    pub moved: bool,
    pub dressed: bool,
}
impl Default for Storage {
    fn default() -> Self {
        Self {
            players: Table::new(data::VolumeRole::Session, "room-players", 6, "Room roster"),
            world: Table::new(
                data::VolumeRole::Root,
                "world-state",
                MAX_RECORD_BYTES,
                "World state",
            ),
            chat: Table::new(
                data::VolumeRole::Session,
                "room-players",
                MAX_RECORD_BYTES,
                "Room chat",
            ),
            restored: false,
            moved: false,
            dressed: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roster() -> Table<[u8; 16]> {
        let mut table = Storage::default().players;
        table.entry = Some(EntryHandle {
            entry_id: [1; 15],
            ..Default::default()
        });
        table
    }

    fn page(root: kv::Root, ids: &[[u8; 16]], next: Option<&[u8]>) -> Vec<u8> {
        let mut buffer = vec![0; kv::MAX_PAGE_BYTES];
        let keys = ids.iter().map(|id| kv::Key {
            bytes: id,
            len_bytes: 1,
        });
        let length = kv::encode_page(&mut buffer, Some(root), keys, next).unwrap();
        buffer.truncate(length);
        buffer
    }

    #[test]
    fn mixed_world_pages_publish_together_and_keep_each_rooms_archive_separate() {
        use WorldKey::*;
        let mut world = Storage::default().world;
        let id = [70; 16];
        let room = [3; 32];
        let root = kv::Root {
            level: 0,
            digest: [1; 32],
        };
        world.select_archive(Some(room));
        world.rows.insert(Name(id), b"old".to_vec());
        world.scan = Some(Scan {
            archive: Some(room),
            ..Default::default()
        });
        let receive = |table: &mut Table<WorldKey>, keys: Vec<WorldKey>, more: bool| {
            let encoded: Vec<_> = keys.iter().map(Key::encode).collect();
            let mut buf = vec![0; kv::MAX_PAGE_BYTES];
            let length = kv::encode_page(
                &mut buf,
                Some(root),
                encoded.iter().map(|k| kv::Key {
                    bytes: k,
                    len_bytes: 1,
                }),
                more.then(|| encoded.last().unwrap().as_slice()),
            )
            .unwrap();
            table.accept_page(kv::decode_page(&buf[..length]).unwrap());
            let scan = table.scan.as_mut().unwrap();
            for key in scan.keys.drain(..) {
                scan.rows.insert(key, vec![10]);
            }
        };
        receive(&mut world, vec![Name(id), Avatar(id), Pulse(id)], true);
        assert!(!world.finish_read().0);
        assert_eq!(world.rows.get(&Name(id)).unwrap(), b"old");
        for start in [0, 32, 64] {
            receive(
                &mut world,
                (start..start + 32).map(|n| Chat([n; 24])).collect(),
                true,
            );
        }
        receive(
            &mut world,
            vec![Archive(room, [4; 24]), Archive([9; 32], [5; 24])],
            false,
        );
        assert!(world.finish_read().0);
        assert_eq!(world.last_root, Some(root));
        assert_eq!(world.rows.len(), 3 + 80 + 1);
        assert!(!world.rows.contains_key(&Chat([15; 24])));
        for key in [
            Name(id),
            Avatar(id),
            Pulse(id),
            Chat([16; 24]),
            Archive(room, [4; 24]),
        ] {
            assert_eq!(world.rows.get(&key), Some(&vec![10]));
            let encoded = key.encode();
            assert_eq!(WorldKey::decode(&encoded), Some(key));
            assert_eq!(WorldKey::decode(&encoded[..encoded.len() - 1]), None);
        }
        assert!(!world.rows.contains_key(&Archive([9; 32], [5; 24])));
    }

    #[test]
    fn crossing_the_door_discards_an_incomplete_private_archive_scan() {
        let mut world = Storage::default().world;
        world.entry = roster().entry;
        let root = kv::Root {
            level: 0,
            digest: [9; 32],
        };
        let room = [7; 32];
        let archive = WorldKey::Archive(room, [3; 24]);
        let key = archive.encode();
        let mut bytes = vec![0; kv::MAX_PAGE_BYTES];
        let n = kv::encode_page(
            &mut bytes,
            Some(root),
            [kv::Key {
                bytes: &key,
                len_bytes: 1,
            }]
            .into_iter(),
            None,
        )
        .unwrap();
        let page = || kv::decode_page(&bytes[..n]).unwrap();
        world.select_archive(Some(room));
        assert!(matches!(world.next_request(0), Some(Request::Keys)));
        world.accept_page(page());
        assert!(matches!(world.next_request(0), Some(Request::Read(id)) if id == archive));
        world.select_archive(None);
        let scan = world.scan.as_mut().unwrap();
        scan.rows.insert(scan.keys.pop_front().unwrap(), vec![1]);
        assert_eq!(world.finish_read(), (false, None));
        assert!(matches!(world.next_request(0), Some(Request::Keys)));
        world.accept_page(page());
        assert_eq!(world.finish_read(), (true, None));
        assert!(world.rows.is_empty());
        world.select_archive(Some(room));
        assert!(matches!(world.next_request(0), Some(Request::Keys)));
        world.accept_page(page());
        assert!(matches!(world.next_request(0), Some(Request::Read(id)) if id == archive));
    }

    #[test]
    fn retired_page_or_row_retries_without_publishing_partial_roster() {
        let mut table = roster();
        table.loaded = true;
        table.rows.insert([1; 16], vec![10]);
        let root = kv::Root {
            level: 0,
            digest: [5; 32],
        };

        let list = table.next_request(0).unwrap();
        assert!(matches!(list, Request::Keys));
        assert!(table.failed(list, Error::Unavailable, 0).is_none());
        assert!(table.error.is_empty());
        assert!(table.next_request(79).is_none());
        assert!(matches!(table.next_request(80), Some(Request::Keys)));
        let first = page(root, &[[2; 16]], Some(&[2; 16]));
        table.accept_page(kv::decode_page(&first).unwrap());

        let row = table.next_request(90).unwrap();
        assert!(matches!(row, Request::Read(id) if id == [2; 16]));
        table.failed(row, Error::Unavailable, 90);
        assert_eq!(table.finish_read(), (false, None));
        assert_eq!(table.rows[&[1; 16]], vec![10]);
        assert!(table.next_request(289).is_none());
        assert!(matches!(table.next_request(290), Some(Request::Read(id)) if id == [2; 16]));
        let scan = table.scan.as_mut().unwrap();
        assert_eq!(scan.root, Some(root));
        assert_eq!(scan.after, Some(vec![2; 16]));
        scan.rows.insert(scan.keys.pop_front().unwrap(), vec![20]);
        assert_eq!(table.finish_read(), (false, None)); // Another page is due.
        assert!(matches!(table.next_request(300), Some(Request::Keys)));
        let last = page(root, &[], None);
        table.accept_page(kv::decode_page(&last).unwrap());

        let (changed, log) = table.finish_read();
        assert!(changed && log.is_none());
        assert_eq!(table.rows, BTreeMap::from([([2; 16], vec![20])]));
        assert_eq!(table.last_root, Some(root));
        assert_eq!(table.scan_failures, 0);
    }

    #[test]
    fn changed_root_restarts_scan_and_continued_failure_becomes_visible() {
        let mut table = roster();
        table.rows.insert([1; 16], vec![10]);
        table.next_request(0);
        table.scan.as_mut().unwrap().rows.insert([2; 16], vec![20]);
        table.failed(Request::Keys, Error::Conflict, 100);
        assert!(table.scan.is_none() && table.refresh);
        for now in [180, 380, 880] {
            let list = table.next_request(now).unwrap();
            assert!(matches!(list, Request::Keys));
            assert!(table.scan.as_ref().unwrap().rows.is_empty());
            table.failed(list, Error::Unavailable, now);
        }
        assert!(table.error.contains("Room roster (list): Unavailable"));
        assert!(table.next_request(3879).is_none());
        assert_eq!(table.rows[&[1; 16]], vec![10]);
        assert!(matches!(table.next_request(3880), Some(Request::Keys)));
        let root = kv::Root {
            level: 0,
            digest: [8; 32],
        };
        let latest = page(root, &[], None);
        table.accept_page(kv::decode_page(&latest).unwrap());
        let (changed, log) = table.finish_read();
        assert!(changed && log.unwrap().contains("4 read retries"));
        assert!(table.rows.is_empty() && table.error.is_empty());

        // A later notification with the same root also clears a read retry.
        table.refresh = true;
        let list = table.next_request(3900).unwrap();
        table.failed(list, Error::Unavailable, 3900);
        assert!(matches!(table.next_request(3980), Some(Request::Keys)));
        table.accept_page(kv::decode_page(&latest).unwrap());
        let (changed, log) = table.finish_read();
        assert!(!changed && log.is_none());
        assert!(table.error.is_empty());
        assert_eq!(table.scan_failures, 0);
    }

    #[test]
    fn read_backoff_allows_writes_and_a_failed_write_keeps_the_latest_value() {
        let mut table = roster();
        for _ in 0..4 {
            table.failed(Request::Keys, Error::Unavailable, 0);
        }
        table.put([1; 16], vec![1]);
        let write = table.next_request(10).unwrap();
        assert!(matches!(write, Request::Put(_, _)));
        table.put([1; 16], vec![2]);
        table.failed(write, Error::Unavailable, 20);
        assert!(table.error.contains("(write)") && table.next_request(3019).is_none());
        assert!(matches!(table.next_request(3020), Some(Request::Put(_, v)) if v == [2]));
        assert!(!table.writing());
    }
}
