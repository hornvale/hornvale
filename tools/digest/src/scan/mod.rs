//! Derived facts — scanned from source at render time, never stored.
//!
//! PROC-11's rule governs what belongs here: store only geological-rate
//! facts; derive-on-read anything faster (spec §4.4).

pub mod capability;
pub mod decisions;

use std::path::PathBuf;

/// The repository root, resolved from this crate's manifest directory.
pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("tools/digest sits two levels below the repo root")
        .to_path_buf()
}
