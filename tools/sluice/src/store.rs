//! The on-disk queue. The state directory is a constructor argument and is
//! never read from the environment here — a library-level ambient read is what
//! let a test address a live chamber and discard a merge product on
//! 2026-09-04.

use crate::row::Row;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// The queue's durable state, rooted at one directory.
pub struct Store {
    dir: PathBuf,
}

impl Store {
    /// Root the store at `dir`. The directory is created if absent.
    pub fn new(dir: PathBuf) -> io::Result<Store> {
        fs::create_dir_all(&dir)?;
        Ok(Store { dir })
    }

    /// `queue.tsv` under the store's directory.
    pub fn queue_path(&self) -> PathBuf {
        self.dir.join("queue.tsv")
    }

    /// `queue.lock` under the store's directory.
    pub fn lock_path(&self) -> PathBuf {
        self.dir.join("queue.lock")
    }

    /// Every parseable row, in file order. A missing file is an empty queue,
    /// not an error: the first `add` on a fresh box must not have to
    /// special-case its own creation.
    pub fn read_rows(&self) -> io::Result<Vec<Row>> {
        let p = self.queue_path();
        if !Path::new(&p).exists() {
            return Ok(Vec::new());
        }
        Ok(fs::read_to_string(&p)?
            .lines()
            .filter_map(Row::parse)
            .collect())
    }

    /// Replace the file with these rows, via a temp file and a rename so a
    /// reader never sees a half-written queue.
    pub fn write_rows(&self, rows: &[Row]) -> io::Result<()> {
        let tmp = self.dir.join(".queue.tmp");
        let body: String = rows
            .iter()
            .map(|r| r.render() + "\n")
            .collect();
        fs::write(&tmp, body)?;
        fs::rename(&tmp, self.queue_path())
    }
}
