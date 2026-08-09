//! A post: two required fields and anything else the poster wants.
//!
//! D12 — the schema is open. This module knows `kind` and `by` and refuses to
//! know more: unrecognised kinds and unrecognised fields survive untouched, so
//! sessions can evolve the convention set without a code change.

use crate::BoardError;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// One immutable board post.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Post {
    /// The convention this post follows (`claim`, `notice`, `technique`, …).
    /// Not validated: an unknown kind renders generically.
    pub kind: String,
    /// The authoring branch or worktree. Never optional (D7c).
    pub by: String,
    /// Every other field, preserved verbatim. `BTreeMap` so serialization is
    /// order-stable, which is what makes the content address deterministic.
    #[serde(flatten)]
    pub extra: BTreeMap<String, serde_json::Value>,
}

impl Post {
    /// A post with no fields beyond the two required ones.
    pub fn new(kind: &str, by: &str) -> Self {
        Self {
            kind: kind.to_string(),
            by: by.to_string(),
            extra: BTreeMap::new(),
        }
    }

    /// Builder: set one convention field.
    pub fn with(mut self, key: &str, value: serde_json::Value) -> Self {
        self.extra.insert(key.to_string(), value);
        self
    }

    /// The bytes this post is addressed by: compact JSON plus a trailing
    /// newline, so the file is a well-behaved text blob.
    pub fn canonical_bytes(&self) -> Result<Vec<u8>, BoardError> {
        let mut v = serde_json::to_vec(self).map_err(|e| BoardError::Json(e.to_string()))?;
        v.push(b'\n');
        Ok(v)
    }

    /// Parse, requiring only `kind` and `by`.
    pub fn from_json(text: &str) -> Result<Self, BoardError> {
        let post: Self = serde_json::from_str(text).map_err(|e| BoardError::Json(e.to_string()))?;
        if post.kind.trim().is_empty() {
            return Err(BoardError::Json("post has no kind".into()));
        }
        if post.by.trim().is_empty() {
            return Err(BoardError::Json(
                "post has no by (D7c: attribution is not optional)".into(),
            ));
        }
        Ok(post)
    }

    /// A convention field as a string, if it is one.
    pub fn str_field(&self, key: &str) -> Option<&str> {
        self.extra.get(key)?.as_str()
    }

    /// A convention field as an unsigned integer, if it is one.
    pub fn u64_field(&self, key: &str) -> Option<u64> {
        self.extra.get(key)?.as_u64()
    }

    /// The `paths` convention field, or empty.
    pub fn paths(&self) -> Vec<String> {
        match self.extra.get("paths").and_then(|v| v.as_array()) {
            Some(items) => items
                .iter()
                .filter_map(|v| v.as_str().map(str::to_string))
                .collect(),
            None => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn canonical_bytes_are_stable_across_construction_order() {
        let a = Post::new("notice", "campaign/x")
            .with("subject", json!("elevation"))
            .with("polarity", json!("hold-off"));
        let b = Post::new("notice", "campaign/x")
            .with("polarity", json!("hold-off"))
            .with("subject", json!("elevation"));
        assert_eq!(
            a.canonical_bytes().expect("a"),
            b.canonical_bytes().expect("b"),
            "field insertion order must not change the content address"
        );
    }

    #[test]
    fn an_unknown_kind_and_unknown_fields_round_trip_byte_identically() {
        // D12: the tool has no enum of kinds and no schema beyond kind + by.
        let text =
            r#"{"kind":"weather-report","by":"campaign/x","cumulus":7,"nested":{"a":[1,2]}}"#;
        let post = Post::from_json(text).expect("parses despite unknown kind");
        assert_eq!(post.kind, "weather-report");
        let again = String::from_utf8(post.canonical_bytes().expect("bytes")).expect("utf8");
        let reparsed = Post::from_json(&again).expect("reparses");
        assert_eq!(post, reparsed, "unknown fields survive a round trip");
        assert_eq!(reparsed.u64_field("cumulus"), Some(7));
    }

    #[test]
    fn a_post_missing_kind_or_by_is_rejected() {
        // D7c: attribution is not optional.
        assert!(Post::from_json(r#"{"kind":"notice"}"#).is_err(), "no by");
        assert!(
            Post::from_json(r#"{"by":"campaign/x"}"#).is_err(),
            "no kind"
        );
    }

    #[test]
    fn paths_reads_a_string_array_and_tolerates_its_absence() {
        let p = Post::new("notice", "b").with("paths", json!(["domains/terrain/", "kernel/"]));
        assert_eq!(
            p.paths(),
            vec!["domains/terrain/".to_string(), "kernel/".to_string()]
        );
        assert!(Post::new("claim", "b").paths().is_empty());
    }
}
