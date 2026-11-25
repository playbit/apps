use crate::font::{self, Cell, Glyph, Link};
use playbit_api::abi::{bloblog, data, syscall::Error};
use playbit_api::context::{Completion, Context, EntryHandle, OperationHandle};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, VecDeque};

#[derive(Clone, Serialize, Deserialize)]
struct Edit {
    clock: u64,
    id: [u8; 16],
    glyph: char,
    points: Vec<(Cell, u8)>,
    // None identifies a record written by the original bitmap editor.
    #[serde(default)]
    lines: Option<Vec<(Link, bool)>>,
}

type Stamp = (u64, [u8; 16]);
pub struct Document {
    pub glyphs: BTreeMap<char, Glyph>,
    pub order: Vec<char>,
    clock: u64,
    versions: BTreeMap<(char, Cell), Stamp>,
    legacy: BTreeMap<char, Glyph>,
    legacy_versions: BTreeMap<(char, Cell), Stamp>,
    line_versions: BTreeMap<char, BTreeMap<Link, (Stamp, bool)>>,
}

impl Default for Document {
    fn default() -> Self {
        let glyphs = font::seed();
        Self {
            order: font::alphabet(&glyphs),
            legacy: glyphs.clone(),
            legacy_versions: Default::default(),
            line_versions: Default::default(),
            glyphs,
            clock: 0,
            versions: BTreeMap::new(),
        }
    }
}

impl Document {
    fn apply(&mut self, edit: &Edit) -> bool {
        if edit.clock == u64::MAX
            || edit.glyph.is_control()
            || edit
                .points
                .iter()
                .any(|(c, v)| !c.valid() || *v > font::MAX_VALUE)
        {
            return false;
        }
        if edit
            .lines
            .as_ref()
            .is_some_and(|lines| lines.iter().any(|(l, _)| !l.valid()))
        {
            return false;
        }
        self.clock = self.clock.max(edit.clock);
        let mut changed = !self.glyphs.contains_key(&edit.glyph);
        if changed {
            self.glyphs.insert(edit.glyph, Glyph::default());
            self.order = font::alphabet(&self.glyphs);
        }
        let glyph = self.glyphs.get_mut(&edit.glyph).unwrap();
        let stamp = (edit.clock, edit.id);
        for &(cell, value) in &edit.points {
            let version = self.versions.entry((edit.glyph, cell)).or_default();
            if stamp > *version {
                *version = stamp;
                changed |= glyph.at(cell) != value;
                glyph.set(cell, value);
            }
        }
        let overrides = self.line_versions.entry(edit.glyph).or_default();
        if let Some(lines) = &edit.lines {
            for &(line, on) in lines {
                let entry = overrides.entry(line).or_default();
                if stamp > entry.0 {
                    *entry = (stamp, on);
                    changed |= glyph.links.contains(&line) != on;
                    glyph.set_link(line, on);
                }
            }
        } else {
            // Rebuild the imported topology from legacy edits only. Explicit
            // line edits win even if an older log page arrives afterwards.
            let base = self.legacy.entry(edit.glyph).or_default();
            let mut occupancy_changed = false;
            for &(cell, v) in &edit.points {
                let previous = self.legacy_versions.entry((edit.glyph, cell)).or_default();
                if stamp > *previous {
                    *previous = stamp;
                    occupancy_changed |= (base.at(cell) == 0) != (v == 0);
                    base.set(cell, v);
                }
            }
            if occupancy_changed {
                base.import_links(edit.glyph);
                glyph.links.clone_from(&base.links);
                for (&line, &(_, on)) in overrides.iter() {
                    glyph.set_link(line, on);
                }
                changed = true;
            }
        }
        changed
    }
}

#[derive(Default)]
pub struct Store {
    entry: Option<EntryHandle>,
    opening: Option<OperationHandle>,
    reading: Option<OperationHandle>,
    writing: Option<OperationHandle>,
    queue: VecDeque<([u8; 16], Vec<u8>)>,
    cursor: Option<String>,
    pub refresh: bool,
    pub loaded: bool,
    pub error: String,
    pub retry_wait: bool,
}

impl Store {
    pub fn save(
        &mut self,
        ctx: &mut Context,
        doc: &mut Document,
        ch: char,
        points: Vec<(Cell, u8)>,
        lines: Vec<(Link, bool)>,
    ) -> bool {
        let mut id = [0; 16];
        if let Err(e) = ctx.random_fill(&mut id) {
            self.failed(ctx, e);
            return false;
        }
        let edit = Edit {
            clock: doc.clock + 1,
            id,
            glyph: ch,
            points,
            lines: Some(lines),
        };
        self.queue
            .push_back((id, serde_json::to_vec(&edit).expect("encode glyph edit")));
        doc.apply(&edit);
        true
    }

    fn failed(&mut self, ctx: &mut Context, e: Error) {
        self.error = format!(
            "{} · {e:?}",
            if self.queue.is_empty() {
                "Font storage"
            } else {
                "Unsaved edits"
            }
        );
        ctx.log(&self.error);
        self.retry_wait = true;
        let _ = ctx.wake_after(2000);
    }

    fn open(ctx: &mut Context) -> Result<OperationHandle, Error> {
        let mut volumes = [data::Volume::default(); 256];
        let mut tags = [data::Range::default(); 1024];
        let mut bytes = vec![0; 16_384];
        let available = ctx
            .available_volumes(&mut volumes, &mut tags, &mut bytes)
            .map_err(|_| Error::Unavailable)?;
        let root = available
            .iter()
            .find(|v| v.is_root)
            .ok_or(Error::Unavailable)?
            .handle;
        ctx.begin_resolve_entry(root, "glyph-edits")
    }

    pub fn pending(&self) -> bool {
        !self.queue.is_empty()
    }

    pub fn service(&mut self, ctx: &mut Context) {
        if self.retry_wait {
            return;
        }
        let Some(entry) = self.entry else {
            if self.opening.is_none() {
                match Self::open(ctx) {
                    Ok(op) => self.opening = Some(op),
                    Err(e) => self.failed(ctx, e),
                }
            }
            return;
        };
        if self.refresh && self.reading.is_none() {
            match ctx.begin_bloblog_read(entry, self.cursor.as_deref()) {
                Ok(op) => {
                    self.reading = Some(op);
                    self.refresh = false;
                }
                Err(e) => {
                    self.failed(ctx, e);
                    return;
                }
            }
        }
        if self.writing.is_none()
            && let Some((id, data)) = self.queue.front()
        {
            match ctx.begin_bloblog_append(entry, id, &[data]) {
                Ok(op) => self.writing = Some(op),
                Err(e) => self.failed(ctx, e),
            }
        }
    }

    pub fn ready(&mut self, ctx: &mut Context, doc: &mut Document, op: OperationHandle) -> bool {
        if self.opening == Some(op) {
            match ctx.take_resolve_entry(op) {
                Ok(Completion::Pending) => return false,
                Ok(Completion::Ready(entry)) => {
                    self.entry = Some(entry);
                    self.refresh = true;
                }
                Err(e) => self.failed(ctx, e),
            }
            self.opening = None;
        } else if self.reading == Some(op) {
            let mut data = vec![0; bloblog::MAX_PAGE_BYTES];
            let mut changed = false;
            match ctx.take_bloblog_read(op, &mut data) {
                Ok(Completion::Pending) => return false,
                Ok(Completion::Ready(page)) => {
                    for record in page.entries {
                        if let Ok(edit) = serde_json::from_slice::<Edit>(record.data) {
                            changed |= doc.apply(&edit);
                        }
                    }
                    self.cursor = Some(page.cursor.to_owned());
                    self.refresh |= page.more;
                    changed |= !self.loaded && !page.more;
                    self.loaded |= !page.more;
                    if self.queue.is_empty() {
                        self.error.clear();
                    }
                }
                Err(e) => {
                    self.refresh = true;
                    self.failed(ctx, e);
                    changed = true;
                }
            }
            self.reading = None;
            return changed;
        } else if self.writing == Some(op) {
            match ctx.take_bloblog_append(op, &mut [0; bloblog::MAX_CURSOR_BYTES]) {
                Ok(Completion::Pending) => return false,
                Ok(Completion::Ready(_)) => {
                    self.queue.pop_front();
                    self.refresh = true;
                    self.error.clear();
                }
                Err(e) => self.failed(ctx, e),
            }
            self.writing = None;
        } else {
            return false;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edits_merge_per_cell_even_when_replayed_out_of_order() {
        let glyph = '\u{e045}';
        let a = Cell(1, 3);
        let b = Cell(2, 3);
        let first = Edit {
            clock: 1,
            id: [1; 16],
            glyph,
            lines: None,
            points: vec![(a, 1), (b, 8)],
        };
        let later = Edit {
            clock: 2,
            id: [2; 16],
            glyph,
            lines: None,
            points: vec![(a, 0)],
        };
        let mut doc = Document::default();
        doc.apply(&later);
        doc.apply(&first);
        doc.apply(&first);
        assert_eq!(doc.glyphs[&glyph].at(a), 0);
        assert_eq!(doc.glyphs[&glyph].at(b), 8);
        assert_eq!(doc.order.iter().filter(|&&c| c == glyph).count(), 1);
    }
    #[test]
    fn manual_disconnect_survives_out_of_order_legacy_import_and_replay() {
        let mut doc = Document::default();
        let a = Cell(0, 0);
        let b = Cell(1, 0);
        let edge = Link::between(a, b);
        let mut edit = Edit {
            clock: 4,
            id: [1; 16],
            glyph: '\u{e045}',
            points: vec![],
            lines: Some(vec![(edge, false)]),
        };
        doc.apply(&edit);
        edit.clock = 1;
        edit.lines = None;
        edit.points = vec![(a, 1), (b, 1)];
        doc.apply(&edit);
        doc.apply(&edit);
        assert!(!doc.glyphs[&edit.glyph].links.contains(&edge));
        edit.clock = 5;
        edit.points.clear();
        edit.lines = Some(vec![(edge, true)]);
        doc.apply(&edit);
        assert!(doc.glyphs[&edit.glyph].links.contains(&edge));
    }
}
