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
    ///
    /// **The invariant this method exists to guarantee: anything
    /// `canonical_bytes()` emits, `from_json` accepts.** Task 3 content-
    /// addresses a post by hashing exactly these bytes into an immutable,
    /// append-only store, so any way of building a `Post` that write-time
    /// could accept and read-time would reject is permanent corruption, not
    /// a transient error — every check below exists to close one such gap.
    ///
    /// `#[serde(flatten)]` does not deduplicate `extra` against the named
    /// `kind`/`by` fields, so a reserved key left in `extra` (e.g. via
    /// `with("kind", ..)`) would otherwise serialize to a literal duplicate
    /// JSON key that `from_json` then refuses to parse. And the named
    /// `kind`/`by` fields are themselves `pub` and unvalidated, so a blank or
    /// whitespace-only value would serialize cleanly and then be rejected by
    /// `from_json`'s own guards on the next read.
    pub fn canonical_bytes(&self) -> Result<Vec<u8>, BoardError> {
        for reserved in ["kind", "by"] {
            if self.extra.contains_key(reserved) {
                return Err(BoardError::Json(format!(
                    "extra field \"{reserved}\" is reserved and cannot be set via with()"
                )));
            }
        }
        if self.kind.trim().is_empty() {
            return Err(BoardError::Json("post has no kind".into()));
        }
        if self.by.trim().is_empty() {
            return Err(BoardError::Json(
                "post has no by (D7c: attribution is not optional)".into(),
            ));
        }
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

    /// Convention fields the read path only honours when they are JSON
    /// numbers: a decay check reads each one through
    /// [`u64_field`](Self::u64_field), which returns `None` for anything else.
    pub const NUMERIC_CONVENTION_FIELDS: [&'static str; 2] = ["ttl_s", "pid"];
    /// Which [`NUMERIC_CONVENTION_FIELDS`](Self::NUMERIC_CONVENTION_FIELDS)
    /// this post carries as something other than a number.
    ///
    /// A typo'd `ttl_s=900s` becomes the JSON *string* `"900s"`, `u64_field`
    /// returns `None`, and the TTL check is skipped entirely — producing an
    /// immortal claim that no reap will ever drop. That is decision 0080's
    /// stuck alarm, arrived at by typo, and it is the exact failure this
    /// board's claims exist to avoid.
    ///
    /// Deliberately a *report*, not a rejection: D12 says the tool requires
    /// only `kind` and `by` and must not validate the convention set, so a
    /// post with a malformed `ttl_s` is still a legal post. What it must not
    /// be is silent — callers warn on both the write and the read path, so a
    /// typo announces itself when it is made and again every time its
    /// consequence is drawn.
    pub fn non_numeric_convention_fields(&self) -> Vec<&'static str> {
        Self::NUMERIC_CONVENTION_FIELDS
            .into_iter()
            .filter(|name| match self.extra.get(*name) {
                Some(v) => v.as_u64().is_none(),
                None => false,
            })
            .collect()
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

/// High-confidence, prefixed credential shapes this scanner refuses a post
/// for (B9).
///
/// **Why prevention, and why here.** Task 8 shipped `board redact`, and its
/// own measurement is why this module exists: byte removal from history is
/// prohibited *and does not work* — a canary force-pushed out of a probe ref
/// was still served by GitHub, commit and blob plaintext both. Task 8's
/// review also found that redaction's two halves have different scopes
/// (suppression is board-wide, eviction is per-log), so a secret that
/// reaches another host's log stays in that host's bytes until someone acts
/// there. Prevention is the only control that fully works, because it acts
/// **before there is anything to propagate** — before `canonical_bytes()`
/// is even called on the post that would carry it.
///
/// **Why so narrow.** `PROC-claim-shape-s-heuristic` is this crate's own
/// standing example of a predicate broad enough to produce three false
/// positives in one week across two campaigns — and a guard that is
/// habitually overridden trains the override, which is worse than no guard
/// at all. Board prose is full of the shapes a looser scanner would trip
/// on: environment-variable names (`HV_CENSUS_CLAIM_PATH`), assignments
/// (`REBASELINE=1`), flags (`--format=%ct`), and the word "token" used to
/// describe a token, not to leak one. So this checks only high-confidence,
/// *structured and prefixed* shapes that do not occur in ordinary technical
/// prose: `ghp_`/`github_pat_` (GitHub PATs), `AKIA` + 16 (AWS access key
/// ids), a PEM private-key header, and `xox[baprs]-` (Slack tokens). No
/// entropy heuristics, no generic `password=`/`token=`/`secret=` matching —
/// deliberately, because those are exactly the shapes ordinary board prose
/// contains.
///
/// Scans every string field of `post`, not only `note`: `kind`, `by`, and
/// every string reachable inside `extra` (recursing through nested arrays
/// and objects). Returns the names of the patterns that matched, empty when
/// clean.
pub fn credential_shapes(post: &Post) -> Vec<&'static str> {
    let mut names: Vec<&'static str> = credential_shape_matches(post)
        .into_iter()
        .map(|(_, pattern)| pattern)
        .collect();
    names.sort_unstable();
    names.dedup();
    names
}

/// [`credential_shapes`]'s per-field detail: which field, and which pattern
/// it matched, as `(field name, pattern name)` pairs.
///
/// Split out from `credential_shapes` because a refusal must be
/// diagnosable, not merely present: naming *which field* tripped the guard
/// is what turns a false positive into a two-minute fix instead of a
/// mystery. The field name is the top-level key (`kind`, `by`, or an
/// `extra` key) — nested JSON under a key is scanned but reported under
/// that key's name, which is precise enough to point a human at the right
/// spot without this becoming a JSON-path implementation.
pub fn credential_shape_matches(post: &Post) -> Vec<(String, &'static str)> {
    let mut out = Vec::new();
    for (field, value) in string_fields(post) {
        for pattern in matched_patterns(value) {
            out.push((field.clone(), pattern));
        }
    }
    out
}

/// Every string field of `post`, paired with the top-level name it is
/// reported under.
fn string_fields(post: &Post) -> Vec<(String, &str)> {
    let mut out = vec![
        ("kind".to_string(), post.kind.as_str()),
        ("by".to_string(), post.by.as_str()),
    ];
    for (key, value) in &post.extra {
        collect_strings(key, value, &mut out);
    }
    out
}

/// Recurse through a JSON value, collecting every string leaf under `field`'s
/// name — arrays and objects are walked, everything else is skipped.
fn collect_strings<'a>(
    field: &str,
    value: &'a serde_json::Value,
    out: &mut Vec<(String, &'a str)>,
) {
    match value {
        serde_json::Value::String(s) => out.push((field.to_string(), s.as_str())),
        serde_json::Value::Array(items) => {
            for item in items {
                collect_strings(field, item, out);
            }
        }
        serde_json::Value::Object(map) => {
            for v in map.values() {
                collect_strings(field, v, out);
            }
        }
        serde_json::Value::Null | serde_json::Value::Bool(_) | serde_json::Value::Number(_) => {}
    }
}

/// Which of the high-confidence patterns `s` matches, in a fixed order.
fn matched_patterns(s: &str) -> Vec<&'static str> {
    let mut hits = Vec::new();
    if contains_prefixed_run(s, "ghp_", 20) || contains_prefixed_run(s, "github_pat_", 20) {
        hits.push("github-pat");
    }
    if contains_aws_akid(s) {
        hits.push("aws-akid");
    }
    if contains_pem_private_key_header(s) {
        hits.push("private-key");
    }
    if contains_slack_token(s) {
        hits.push("slack-token");
    }
    hits
}

/// True if `s` contains `prefix` immediately followed by at least `min_run`
/// ASCII alphanumeric-or-underscore characters — a GitHub PAT's *shape*
/// (`ghp_<36 base62>`, `github_pat_<82 base62-and-underscore>`), not merely
/// its prefix. Requiring the run is what keeps a bare mention of "ghp_" in
/// prose (there is none in the corpus this was checked against, but there
/// could be) from tripping this.
fn contains_prefixed_run(s: &str, prefix: &str, min_run: usize) -> bool {
    let bytes = s.as_bytes();
    let mut start = 0;
    while let Some(rel) = s[start..].find(prefix) {
        let after = start + rel + prefix.len();
        let run = bytes[after..]
            .iter()
            .take_while(|b| b.is_ascii_alphanumeric() || **b == b'_')
            .count();
        if run >= min_run {
            return true;
        }
        // Advance past this occurrence's prefix, not past the whole run, so
        // an overlapping second prefix later in the string is still found.
        start = after;
    }
    false
}

/// True if `s` contains `AKIA` immediately followed by at least 16 uppercase
/// ASCII letters or digits — an AWS access key id's shape (`AKIA` + 16
/// chars, 20 total).
fn contains_aws_akid(s: &str) -> bool {
    let bytes = s.as_bytes();
    let mut start = 0;
    while let Some(rel) = s[start..].find("AKIA") {
        let after = start + rel + 4;
        let run = bytes[after..]
            .iter()
            .take_while(|b| b.is_ascii_uppercase() || b.is_ascii_digit())
            .count();
        if run >= 16 {
            return true;
        }
        start = after;
    }
    false
}

/// True if `s` contains a PEM private-key header: `-----BEGIN ` followed,
/// within a short window, by `PRIVATE KEY-----` — matches `RSA`, `EC`,
/// `DSA`, `OPENSSH`, and the bare `PRIVATE KEY` header forms alike without
/// enumerating every key-type word.
fn contains_pem_private_key_header(s: &str) -> bool {
    const BEGIN: &str = "-----BEGIN ";
    const TAIL: &str = "PRIVATE KEY-----";
    let mut start = 0;
    while let Some(rel) = s[start..].find(BEGIN) {
        let after = start + rel + BEGIN.len();
        let window_end = (after + 40).min(s.len());
        // `.get()` (not slicing) so a multi-byte char straddling `window_end`
        // yields `None` rather than panicking on a non-boundary index.
        if s.get(after..window_end).is_some_and(|w| w.contains(TAIL)) {
            return true;
        }
        start = after;
    }
    false
}

/// True if `s` contains a Slack token's shape: `xox` + one of `b`/`a`/`p`/
/// `r`/`s` + `-`.
fn contains_slack_token(s: &str) -> bool {
    ["xoxb-", "xoxa-", "xoxp-", "xoxr-", "xoxs-"]
        .iter()
        .any(|p| s.contains(p))
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
    fn a_present_but_blank_kind_or_by_is_rejected_by_the_explicit_guards() {
        // These cases have `kind` and `by` present as strings, so serde's own
        // deserialization succeeds; only the `.trim().is_empty()` guards in
        // `from_json` catch them. Distinct from `a_post_missing_kind_or_by_is_rejected`,
        // which exercises serde's missing-field failure instead.
        assert!(
            Post::from_json(r#"{"kind":"","by":"campaign/x"}"#).is_err(),
            "blank kind"
        );
        assert!(
            Post::from_json(r#"{"kind":"notice","by":""}"#).is_err(),
            "blank by"
        );
        let err = Post::from_json(r#"{"kind":"notice","by":"   "}"#)
            .expect_err("whitespace-only by is not attribution");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("attribution"),
            "the guard's own message should name attribution, not serde's parse error; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_rejects_a_reserved_kind_key_in_extra() {
        let post = Post::new("notice", "campaign/x").with("kind", json!("evil"));
        let err = post
            .canonical_bytes()
            .expect_err("a duplicate `kind` key must not be allowed to serialize");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("kind"),
            "message should name the key; got: {msg}"
        );
        assert!(
            msg.contains("reserved"),
            "message should say the key is reserved; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_rejects_a_reserved_by_key_in_extra() {
        let post = Post::new("notice", "campaign/x").with("by", json!("someone-else"));
        let err = post
            .canonical_bytes()
            .expect_err("a duplicate `by` key must not be allowed to serialize");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("by"),
            "message should name the key; got: {msg}"
        );
        assert!(
            msg.contains("reserved"),
            "message should say the key is reserved; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_rejects_a_blank_kind() {
        let post = Post::new("", "campaign/x");
        let err = post
            .canonical_bytes()
            .expect_err("a blank kind must not be allowed to serialize");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("kind"),
            "message should name the field; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_rejects_a_blank_by() {
        let post = Post::new("notice", "");
        let err = post
            .canonical_bytes()
            .expect_err("a blank by must not be allowed to serialize");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("by"),
            "message should name the field; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_rejects_a_whitespace_only_by() {
        // `.trim()` is there for exactly this: a naive `.is_empty()` would
        // let a whitespace-only value through.
        let post = Post::new("notice", "   ");
        let err = post
            .canonical_bytes()
            .expect_err("a whitespace-only by is not attribution");
        let BoardError::Json(msg) = err else {
            panic!("expected BoardError::Json, got {err:?}")
        };
        assert!(
            msg.contains("attribution"),
            "the guard's own message should name attribution; got: {msg}"
        );
    }

    #[test]
    fn canonical_bytes_output_always_reparses_across_a_table_of_posts() {
        // Positive form of the invariant this module exists to guarantee:
        // anything `canonical_bytes` emits, `from_json` accepts.
        let cases: Vec<Post> = vec![
            Post::new("notice", "campaign/x"),
            Post::new("notice", "campaign/x")
                .with("subject", json!("elevation"))
                .with("paths", json!(["kernel/"])),
            Post::new("weather-report", "campaign/y").with("cumulus", json!(7)),
            Post::new("notice", "campaign/z")
                .with("nested", json!({"a": [1, 2], "b": {"c": true}})),
            Post::new("notice", "campaign/unicode")
                .with("subject", json!("caf\u{e9} \u{1f9ed} \u{5730}\u{5f62}")),
        ];
        for post in cases {
            let bytes = post
                .canonical_bytes()
                .unwrap_or_else(|e| panic!("post {post:?} failed to serialize: {e}"));
            let text = String::from_utf8(bytes).expect("utf8");
            let reparsed = Post::from_json(&text).unwrap_or_else(|e| {
                panic!("canonical_bytes output for {post:?} did not reparse: {e}")
            });
            assert_eq!(post, reparsed, "round trip must be exact for {post:?}");
        }
    }

    #[test]
    fn a_non_numeric_ttl_or_pid_is_reported_rather_than_silently_ignored() {
        // I8 / decision 0080's stuck alarm, by typo. `ttl_s=900s` parses as
        // the JSON string "900s", `u64_field` returns None, the TTL check is
        // SKIPPED, and the claim becomes immortal -- exactly what the spec set
        // out to avoid. The tool still accepts the post (D12: it validates
        // nothing beyond `kind` and `by`); what it must not do is stay quiet.
        let p = Post::new("claim", "campaign/x")
            .with("ttl_s", json!("900s"))
            .with("pid", json!("abc"));
        assert_eq!(p.non_numeric_convention_fields(), vec!["ttl_s", "pid"]);
        assert_eq!(
            p.u64_field("ttl_s"),
            None,
            "sanity: this really is the value the read path cannot use"
        );
    }

    #[test]
    fn a_numeric_or_absent_ttl_reports_nothing() {
        let good = Post::new("claim", "campaign/x").with("ttl_s", json!(900));
        assert!(good.non_numeric_convention_fields().is_empty());
        let absent = Post::new("claim", "campaign/x");
        assert!(
            absent.non_numeric_convention_fields().is_empty(),
            "an ABSENT ttl_s is a legitimate open-ended claim, not a typo"
        );
        let negative = Post::new("claim", "campaign/x").with("ttl_s", json!(-5));
        assert_eq!(
            negative.non_numeric_convention_fields(),
            vec!["ttl_s"],
            "a negative ttl_s is not a u64 either, so the check is skipped and \
             the reader must be told"
        );
    }

    #[test]
    fn an_ordinary_technique_post_is_not_refused() {
        // Deliberately adversarial: real board prose full of the words a naive
        // scanner would trip on.
        for note in [
            "git mktree rejects any path containing a slash; use a temp index",
            "the census claim key is HV_CENSUS_CLAIM_PATH and it defaults to /tmp/hv-census.claim",
            "set REBASELINE=1 to accept drifted goldens",
            "ssh lefford and check the token count in the run log",
        ] {
            let post = Post::new("technique", "main").with("note", json!(note));
            assert!(
                credential_shapes(&post).is_empty(),
                "false positive on ordinary prose: {note:?}"
            );
        }
    }

    #[test]
    fn a_post_carrying_a_credential_shape_is_refused() {
        for (name, note) in [
            (
                "github-pat",
                "use ghp_0123456789abcdefghijklmnopqrstuvwxyzAB to auth",
            ),
            ("aws-akid", "AKIAIOSFODNN7EXAMPLE is the key"),
            ("private-key", "-----BEGIN OPENSSH PRIVATE KEY-----"),
        ] {
            let post = Post::new("technique", "main").with("note", json!(note));
            assert!(
                !credential_shapes(&post).is_empty(),
                "missed {name} in {note:?}"
            );
        }
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
