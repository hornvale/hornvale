//! The MCP surface: ergonomic read/write over the JSONL store.
//!
//! MCP is ERGONOMICS, NOT SUBSTRATE (spec §4.8). Every operation here must
//! remain possible by editing `docs/digest/facts.jsonl` by hand; the
//! round-trip test asserts exactly that.

use crate::store::{ProjectLedger, StoreError, fact};
use crate::vocabulary::project_registry;
use hornvale_kernel::ledger::{EntityId, Fact, Value};
use std::num::NonZeroU64;
use std::path::Path;

/// Load the store, treating a missing file as a legitimately empty ledger
/// (the first assert into a fresh repo) but propagating every other read
/// failure — permission denied, an I/O fault, a partially-written file —
/// rather than defaulting to empty. Defaulting on those would let
/// `handle_assert` overwrite a transiently unreadable store with a
/// truncated one, destroying the ledger.
fn load(path: &Path) -> Result<ProjectLedger, StoreError> {
    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => String::new(),
        Err(e) => return Err(StoreError::Unreadable(e.to_string())),
    };
    ProjectLedger::from_jsonl(&text, project_registry())
}

/// Assert one fact and rewrite the store, compacted.
pub fn handle_assert(
    path: &Path,
    subject: u64,
    predicate: &str,
    object: &str,
) -> Result<(), StoreError> {
    let subject = NonZeroU64::new(subject).ok_or(StoreError::ZeroSubject)?;
    let mut led = load(path)?;
    led.assert(fact(
        EntityId(subject),
        predicate,
        Value::Text(object.to_string()),
    ))?;
    std::fs::write(path, led.to_jsonl()).expect("store is writable");
    Ok(())
}

/// Query facts by subject and/or predicate.
pub fn handle_query(
    path: &Path,
    subject: Option<u64>,
    predicate: Option<&str>,
) -> Result<Vec<Fact>, StoreError> {
    let led = load(path)?;
    Ok(led
        .facts()
        .iter()
        .filter(|f| subject.is_none_or(|s| f.subject.0.get() == s))
        .filter(|f| predicate.is_none_or(|p| f.predicate == p))
        .cloned()
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_then_query_round_trips_through_the_store_file() {
        let dir = std::env::temp_dir().join(format!("digest-mcp-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        let _ = std::fs::remove_file(&path);

        handle_assert(&path, 1, "decision-status", "accepted").expect("assert");
        handle_assert(&path, 1, "decision-status", "superseded").expect("re-assert");

        let text = std::fs::read_to_string(&path).expect("read back");
        assert_eq!(
            text.lines().count(),
            1,
            "the functional predicate compacted on disk"
        );
        assert!(text.contains("superseded"));

        let hits = handle_query(&path, Some(1), None).expect("query");
        assert_eq!(hits.len(), 1);
    }

    #[test]
    fn the_store_is_plain_text_editable_without_the_tool() {
        let dir = std::env::temp_dir().join(format!("digest-mcp-test2-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        std::fs::write(
            &path,
            "{\"subject\":1,\"predicate\":\"decision-status\",\"object\":{\"Text\":\"accepted\"},\
             \"place\":null,\"day\":null,\"provenance\":\"asserted\"}\n",
        )
        .expect("hand-write");
        let hits = handle_query(&path, Some(1), None).expect("query a hand-written store");
        assert_eq!(hits.len(), 1, "MCP is ergonomics, not substrate");
    }

    #[test]
    fn a_missing_store_file_is_a_legitimate_empty_ledger() {
        let dir =
            std::env::temp_dir().join(format!("digest-mcp-test-notfound-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        let _ = std::fs::remove_file(&path);

        let hits = handle_query(&path, None, None).expect("a missing file reads as empty");
        assert!(hits.is_empty(), "no file yet is not a read failure");
    }

    #[test]
    fn a_zero_subject_is_rejected_not_panicked() {
        let dir = std::env::temp_dir()
            .join(format!("digest-mcp-test-zero-subject-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        let _ = std::fs::remove_file(&path);

        let err = handle_assert(&path, 0, "decision-status", "accepted")
            .expect_err("subject 0 must be rejected, not panic");
        assert!(matches!(err, StoreError::ZeroSubject));
    }

    // Unix-only: proves the *destructive* case Finding 1 was raised against.
    // `chmod 000` induces a read failure that is NOT `NotFound` (permission
    // denied), portably across the Unix hosts this project actually runs on
    // (Mac, lefford). There is no portable way to induce a non-NotFound read
    // failure on a hypothetical non-Unix target from a test, so that branch
    // stays covered only by `a_missing_store_file_is_a_legitimate_empty_ledger`
    // (the NotFound branch) plus code review of the `ErrorKind` match arm.
    #[cfg(unix)]
    #[test]
    fn an_unreadable_store_is_never_truncated_by_a_failed_assert() {
        use std::os::unix::fs::PermissionsExt;

        let dir = std::env::temp_dir()
            .join(format!("digest-mcp-test-unreadable-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        let path = dir.join("facts.jsonl");
        let original = "{\"subject\":1,\"predicate\":\"decision-status\",\"object\":{\"Text\":\"accepted\"},\
             \"place\":null,\"day\":null,\"provenance\":\"asserted\"}\n";
        std::fs::write(&path, original).expect("seed the store");

        let mut perms = std::fs::metadata(&path).expect("metadata").permissions();
        perms.set_mode(0o000);
        std::fs::set_permissions(&path, perms).expect("chmod unreadable");

        let result = handle_assert(&path, 2, "decision-status", "accepted");

        // Restore permissions unconditionally so the file can be inspected
        // (and the temp dir cleaned up) regardless of the outcome above.
        let mut restore = std::fs::metadata(&path).expect("metadata").permissions();
        restore.set_mode(0o644);
        std::fs::set_permissions(&path, restore).expect("chmod readable again");

        assert!(
            matches!(result, Err(StoreError::Unreadable(_))),
            "a non-NotFound read failure must be reported, not swallowed as empty: {result:?}"
        );
        let text = std::fs::read_to_string(&path).expect("read back");
        assert_eq!(
            text, original,
            "a failed assert must never truncate an unreadable store"
        );
    }
}
