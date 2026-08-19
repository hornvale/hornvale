//! The committed phonology page's audio references and the committed audio
//! directory must be exactly the same set: every referenced clip exists,
//! and no orphaned clip lingers after a phonology change. Content-addressed
//! names make this existence check the entire freshness story — CI never
//! compares waveforms (espeak-ng promises no cross-version byte stability).

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// The repository root, resolved from the CLI crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ sits directly under the repo root")
        .to_path_buf()
}

#[test]
fn committed_audio_is_exactly_the_page_referenced_set() {
    let root = repo_root();
    let page = fs::read_to_string(root.join("book/src/reference/phonology.md"))
        .expect("committed phonology page");
    let mut referenced = BTreeSet::new();
    for chunk in page.split("src=\"../audio/").skip(1) {
        let name = chunk.split('"').next().expect("a closing quote");
        assert!(
            name.len() == 12 && name.ends_with(".mp3"),
            "malformed audio reference {name:?}"
        );
        referenced.insert(name.to_string());
    }
    assert!(
        !referenced.is_empty(),
        "the page must reference at least one audio clip"
    );

    let mut committed = BTreeSet::new();
    for entry in fs::read_dir(root.join("book/src/audio")).expect("book/src/audio exists") {
        let entry = entry.expect("a readable directory entry");
        committed.insert(entry.file_name().to_string_lossy().into_owned());
    }
    assert_eq!(
        referenced, committed,
        "committed clips (right) must be exactly the page's references \
         (left); run `hornvale voice` for missing clips and delete orphans"
    );
}
