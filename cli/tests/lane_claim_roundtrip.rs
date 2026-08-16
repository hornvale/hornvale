//! A lane-written claim must round-trip through the claim parser (The Sluice).
//!
//! DIRECTION THIS CHECK ENFORCES: every writer of the shared claim file emits
//! the field set the parser requires. It is blind to the opposite direction —
//! a parser that stopped requiring a field would not fail here.
//!
//! WHY IT EXISTS: `scripts/lane-run.sh` shipped writing seven of the eight
//! fields `parse_claim` requires, so `census-run.sh status`, `make
//! heavy-status` and `lab claim-status` all reported "no heavy run in
//! progress" for the whole duration of every lane job — while the job held the
//! box. The lock was never affected; only the answer to "is the box busy?"
//! was, which is the question CLAUDE.md tells every session to ask first.

use std::path::{Path, PathBuf};

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The claim keys a writer emits, scraped from its `echo "<key>=…"` lines.
fn claim_keys_written_by(script: &str) -> Vec<String> {
    let text = std::fs::read_to_string(repo_root().join(script))
        .unwrap_or_else(|e| panic!("{script} must be readable: {e}"));
    let mut keys = Vec::new();
    for line in text.lines() {
        let t = line.trim();
        let Some(rest) = t.strip_prefix("echo \"") else {
            continue;
        };
        if let Some((key, _)) = rest.split_once('=')
            && !key.is_empty()
            && key.chars().all(|c| c.is_ascii_lowercase())
        {
            keys.push(key.to_string());
        }
    }
    keys
}

/// Exactly the fields `parse_claim` requires. Kept as a literal on purpose: if
/// the parser gains a required field, this list must be updated deliberately,
/// which is the review moment this test exists to force.
const REQUIRED: [&str; 8] = [
    "pid", "host", "user", "started", "goldens", "label", "ref", "cmdline",
];

#[test]
fn the_scraper_can_see_a_known_good_writer() {
    // Guards the vacuous case: a scraper that matched nothing would make the
    // assertion below pass for every script, including a broken one.
    let keys = claim_keys_written_by("scripts/census-run.sh");
    assert!(
        REQUIRED.iter().all(|r| keys.iter().any(|k| k == r)),
        "the scraper failed on census-run.sh, a writer known to be complete — \
         it has gone vacuous. found: {keys:?}"
    );
}

#[test]
fn every_claim_writer_emits_every_required_field() {
    for script in [
        "scripts/lane-run.sh",
        "scripts/census-run.sh",
        "scripts/heavy-run.sh",
        "scripts/sluice-run.sh",
    ] {
        let keys = claim_keys_written_by(script);
        let missing: Vec<&str> = REQUIRED
            .iter()
            .copied()
            .filter(|r| !keys.iter().any(|k| k == r))
            .collect();
        assert!(
            missing.is_empty(),
            "{script} omits {missing:?} from the claim file. parse_claim \
             requires all of {REQUIRED:?} and returns None otherwise, so \
             `census-run.sh status` would report no run while this job holds \
             the box."
        );
    }
}
