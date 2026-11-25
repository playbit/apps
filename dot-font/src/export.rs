//! Export into a granted directory. A writer stays private until close commits.
use crate::{font::Glyph, opentype};
use playbit_api::{
    abi::{data, files, syscall::Error},
    context::{Completion, Context, File, FileOperation, OperationHandle, VolumeHandle},
};
use std::collections::BTreeMap;

pub struct Destination {
    pub name: String,
    file: File,
    volume: Option<([u8; 16], VolumeHandle)>,
}

enum Stage {
    Open(FileOperation),
    Write(File, FileOperation, usize),
    Close(FileOperation),
    Resolve(OperationHandle),
}

#[derive(Default)]
pub struct Export {
    pub destinations: Vec<Destination>,
    pub selected: usize,
    pub message: String,
    pub command: String,
    stage: Option<Stage>,
    bytes: Vec<u8>,
    base_name: &'static str,
    filename: String,
    copy_number: u32,
}

impl Export {
    pub fn discover(&mut self, ctx: &mut Context) {
        if self.busy() {
            return;
        }
        self.destinations.clear();
        self.selected = 0;
        let mut records = [data::Volume::default(); 256];
        let mut tags = [data::Range::default(); 1024];
        let mut bytes = vec![0; 16384];
        let mut volumes = match ctx.available_volumes(&mut records, &mut tags, &mut bytes) {
            Ok(v) => v,
            Err(e) => {
                self.message = format!("Could not list export destinations: {e:?}");
                return;
            }
        };
        let targets = volumes
            .iter()
            .enumerate()
            .filter(|(_, v)| v.rights & 2 != 0 && (v.is_root || v.backing == data::VOLUME_HOST_FS))
            .map(|(i, v)| {
                (
                    i,
                    v.is_root,
                    if v.is_root {
                        format!("Playbit volume · {}", v.name)
                    } else {
                        v.name.to_string()
                    },
                    v.id,
                    v.handle,
                )
            })
            .collect::<Vec<_>>();
        for (index, root, name, id, handle) in targets {
            if let Some(file) = volumes.take_file(index) {
                self.destinations.push(Destination {
                    name,
                    file,
                    volume: root.then_some((id, handle)),
                });
            }
        }
        self.destinations.sort_by_key(|d| d.volume.is_some());
        if self.destinations.is_empty() {
            self.message = "No writable export destination is available.".into();
        }
    }
    pub fn busy(&self) -> bool {
        self.stage.is_some()
    }
    pub fn start(&mut self, ctx: &mut Context, glyphs: &BTreeMap<char, Glyph>, curves: bool) {
        if self.busy() {
            return;
        }
        if self.destinations.get(self.selected).is_none() {
            return;
        }
        self.command.clear();
        self.base_name = opentype::filename(curves);
        self.filename = self.base_name.into();
        self.copy_number = 1;
        match opentype::encode(glyphs, curves) {
            Ok(bytes) => self.bytes = bytes,
            Err(e) => {
                self.message = e;
                return;
            }
        }
        match self.open(ctx) {
            Ok(stage) => {
                self.stage = Some(stage);
                self.message = format!("Writing {}…", self.filename);
            }
            Err(e) => self.fail(e),
        }
    }

    fn open(&self, ctx: &mut Context) -> Result<Stage, Error> {
        let target = &self.destinations[self.selected];
        let mode = if target.volume.is_none() {
            files::FILE_WRITE | files::FILE_EXCLUSIVE
        } else {
            files::FILE_WRITE
        };
        target
            .file
            .open(ctx, self.filename.as_bytes(), mode)
            .map(Stage::Open)
    }

    fn fail(&mut self, e: Error) {
        self.message = format!("Could not finish exporting {}: {e:?}", self.filename);
        self.bytes = Vec::new();
    }

    fn write_chunk(&self, ctx: &mut Context, file: File, offset: usize) -> Result<Stage, Error> {
        let end = (offset + files::MAX_FILE_WRITE as usize).min(self.bytes.len());
        let op = file.write(ctx, offset as u64, &self.bytes[offset..end])?;
        Ok(Stage::Write(file, op, end))
    }

    pub fn ready(&mut self, ctx: &mut Context, id: OperationHandle) -> bool {
        let Some(stage) = &self.stage else {
            return false;
        };
        let active = match stage {
            Stage::Open(op) | Stage::Write(_, op, _) | Stage::Close(op) => op.handle(),
            Stage::Resolve(op) => *op,
        };
        if active != id {
            return false;
        }
        let stage = self.stage.take().unwrap();
        match self.advance(ctx, stage) {
            Ok(stage) => self.stage = stage,
            Err(e) => self.fail(e),
        }
        true
    }
    fn advance(&mut self, ctx: &mut Context, mut stage: Stage) -> Result<Option<Stage>, Error> {
        match &mut stage {
            Stage::Open(op) => match op.take_open(ctx) {
                Ok(Completion::Ready(file)) => return self.write_chunk(ctx, file, 0).map(Some),
                Ok(Completion::Pending) => {}
                Err(Error::Conflict) if self.destinations[self.selected].volume.is_none() => {
                    self.copy_number = self
                        .copy_number
                        .checked_add(1)
                        .ok_or(Error::LimitExceeded)?;
                    let stem = self.base_name.trim_end_matches(".ttf");
                    self.filename = format!("{stem}-{}.ttf", self.copy_number);
                    return self.open(ctx).map(Some);
                }
                Err(error) => return Err(error),
            },
            Stage::Write(_, op, _) => {
                if let Completion::Ready(()) = op.take_write(ctx)? {
                    let Stage::Write(mut file, _, end) = stage else {
                        unreachable!()
                    };
                    if end == self.bytes.len() {
                        self.bytes = Vec::new();
                        return Ok(Some(Stage::Close(file.close(ctx, 0)?)));
                    }
                    return self.write_chunk(ctx, file, end).map(Some);
                }
            }
            Stage::Close(op) => {
                if let Completion::Ready(receipt) = op.take_close(ctx)? {
                    if let Some(e) = receipt.error {
                        return Err(e);
                    }
                    if receipt.publication != files::FILE_PUBLISHED {
                        return Err(Error::Uncertain);
                    }
                    let target = &self.destinations[self.selected];
                    self.message = format!("Saved {} in {}.", self.filename, target.name);
                    if let Some((_, volume)) = target.volume {
                        return match ctx.begin_resolve_entry(volume, &self.filename) {
                            Ok(op) => Ok(Some(Stage::Resolve(op))),
                            Err(e) => {
                                self.message
                                    .push_str(&format!(" File ID unavailable: {e:?}"));
                                Ok(None)
                            }
                        };
                    }
                    return Ok(None);
                }
            }
            Stage::Resolve(op) => match ctx.take_resolve_entry(*op) {
                Ok(Completion::Ready(entry)) => {
                    let (id, _) = self.destinations[self.selected].volume.unwrap();
                    self.command = format!(
                        "pb file-read {} {} {}",
                        bs58::encode(id).into_string(),
                        bs58::encode(entry.entry_id).into_string(),
                        self.filename
                    );
                    return Ok(None);
                }
                Err(e) => {
                    self.message
                        .push_str(&format!(" File ID unavailable: {e:?}"));
                    return Ok(None);
                }
                Ok(Completion::Pending) => {}
            },
        }
        Ok(Some(stage))
    }
}
