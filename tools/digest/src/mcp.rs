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

fn load(path: &Path) -> Result<ProjectLedger, StoreError> {
    let text = std::fs::read_to_string(path).unwrap_or_default();
    ProjectLedger::from_jsonl(&text, project_registry())
}

/// Assert one fact and rewrite the store, compacted.
pub fn handle_assert(
    path: &Path,
    subject: u64,
    predicate: &str,
    object: &str,
) -> Result<(), StoreError> {
    let mut led = load(path)?;
    led.assert(fact(
        EntityId(NonZeroU64::new(subject).expect("subject is nonzero")),
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
        let dir = std::env::temp_dir().join("digest-mcp-test");
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
        let dir = std::env::temp_dir().join("digest-mcp-test2");
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
}
