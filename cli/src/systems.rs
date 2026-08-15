//! The Compendium: score a frozen corpus of game-system capability against
//! Hornvale's own declared state. Sibling family to `tropes/`, never a member
//! of it — see the spec's §3.
//!
//! Unlike `tropes`, this resolver builds **no world**. Every anchor resolves
//! against `docs/digest/decisions-in-force.md`, the idea registry, and the
//! filesystem, so the ratchet costs a few file reads rather than a genesis.

use serde::Deserialize;

/// The declared corpora, in matrix-column order.
/// type-audit: bare-ok(artifact)
pub const CORPORA: &[&str] = &["systems/wolverson-2021.system.json"];

/// How one catalogue item stands against Hornvale.
///
/// Five verdicts, not decision 0095's three. An unmet capability is three
/// different facts — deliberately refused, planned but unbuilt, or a genuine
/// hole — and an instrument that cannot tell them apart reports a deficiency
/// list that is mostly false against a corpus like this one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Verdict {
    /// Hornvale does this. Cites a mechanism anchor.
    Present,
    /// Hornvale deliberately will not. Cites a decision anchor.
    Refused,
    /// Planned, not built. Cites a registry anchor.
    Deferred,
    /// A genuine hole. Cites nothing — the honest red.
    Absent,
    /// About the tutorial's toolchain, not a world capability. Cites a reason.
    Inapplicable,
}

/// One catalogue item as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(identifier-text: kind), bare-ok(prose: title), bare-ok(identifier-text: anchor), bare-ok(prose: note)
#[derive(Debug, Clone, Deserialize)]
pub struct Item {
    /// Corpus-local identifier, e.g. `2.6`.
    pub id: String,
    /// What this row is: `chapter`, `front-matter`, `feature`, `mechanic`.
    pub kind: String,
    /// The item's title as the source gives it.
    pub title: String,
    /// How it stands against Hornvale.
    pub verdict: Verdict,
    /// The anchor backing the verdict, absent only for `absent`.
    #[serde(default)]
    pub anchor: Option<String>,
    /// One line of human context. Never parsed.
    #[serde(default)]
    pub note: String,
}

/// A frozen, provenance-stamped capability corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(identifier-text: unit), bare-ok(flag: ordered), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Clone, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `wolverson-2021`.
    pub corpus: String,
    /// What the items are: `chapter`, `feature`, …
    pub unit: String,
    /// Whether the items form a meaningful sequence. Gates ordinal readings.
    pub ordered: bool,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// The items themselves, in corpus order.
    pub items: Vec<Item>,
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// A verdict's backing evidence, parsed from its `anchor` string.
/// type-audit: bare-ok(identifier-text: Decision.0), bare-ok(identifier-text: Registry.0), bare-ok(identifier-text: Test.0), bare-ok(artifact: Path.0), bare-ok(prose: Reason.0)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Anchor {
    /// `decision:0070` — must be in force.
    Decision(String),
    /// `registry:CLIENT-action-clock` — must exist and not read `shipped`.
    Registry(String),
    /// `test:<crate>::<fn>` — the preferred mechanism anchor.
    Test(String),
    /// `path:<file>` — the weaker mechanism anchor.
    Path(String),
    /// `reason:<prose>` — for `inapplicable` only.
    Reason(String),
}

impl Anchor {
    /// Parse an anchor string. `None` when the prefix is unknown.
    /// type-audit: bare-ok(identifier-text: s)
    pub fn parse(s: &str) -> Option<Anchor> {
        let (kind, rest) = s.split_once(':')?;
        match kind {
            "decision" => Some(Anchor::Decision(rest.to_string())),
            "registry" => Some(Anchor::Registry(rest.to_string())),
            "test" => Some(Anchor::Test(rest.to_string())),
            "path" => Some(Anchor::Path(rest.to_string())),
            "reason" => Some(Anchor::Reason(rest.to_string())),
            _ => None,
        }
    }
}

/// The live repo state every anchor resolves against. Gathered once.
#[derive(Debug, Clone)]
pub struct RepoFacts {
    /// Decision numbers present in `docs/digest/decisions-in-force.md`.
    in_force: BTreeSet<String>,
    /// Idea-registry row ID → its `status` cell, NORMALIZED (see
    /// `normalize_status`) at gather time: `**shipped (C1)**` and `shipped`
    /// both store as `shipped`, and `elaborated (slice-2 shipped)` stores as
    /// `elaborated`, never `shipped`. Storing normalized rather than raw
    /// means any future consumer gets the same answer this one does.
    registry: BTreeMap<String, String>,
    /// Repo root, for path and test-symbol resolution.
    root: PathBuf,
    /// Crate name → its directory, e.g. `hornvale-vessel` → `windows/vessel`.
    /// Also carries `kernel/` and `cli/` (`hornvale-kernel`, `hornvale`), so
    /// a `test:` anchor may cite mechanism living there. Task 5's surplus
    /// read enumerates `domains/*`/`windows/*` directories directly, not
    /// this map's keys, so the two extra entries never change what counts
    /// as a "subsystem" there — they only extend which crates a `test:`
    /// anchor may resolve against.
    crates: BTreeMap<String, String>,
    /// The `domains/*`/`windows/*` subsystem directories (`domains/terrain`,
    /// `windows/vessel`, …; 25 today), sorted, as `<parent>/<name>` strings.
    /// Task 5's surplus read (spec §6) enumerates exactly these. Gathered by
    /// a walk kept DELIBERATELY SEPARATE from `gather_crate_directories`:
    /// that walk also folds in `kernel/` and `cli/`, which are legitimate
    /// `test:` anchor targets but never "a subsystem the corpus has no
    /// vocabulary for" — reading the surplus list off `crates`' keys would
    /// silently count those two among the 25 and change the count.
    subsystems: Vec<String>,
    /// A stamp of `docs/digest/decisions-in-force.md`'s current content —
    /// its byte length and a rolling checksum, never a shell-out to `git`.
    /// Printed only in `Decision`-anchor `Dangling` findings: that file is
    /// the only one of the two anchor sources the digest actually
    /// generates (`tools/digest/src/main.rs`'s `all_decisions()` scans
    /// `docs/decisions/*.md` directly and renders this file); the idea
    /// registry is hand-authored and carries no digest relationship at
    /// all, so registry-related findings say that instead of printing a
    /// stamp that would misleadingly imply one.
    decisions_stamp: String,
}

impl RepoFacts {
    /// Gather the live repo facts every anchor resolves against, reading
    /// only from `root` — never shelling out to `cargo` or `git`.
    /// type-audit: bare-ok(prose: return)
    pub fn gather(root: &Path) -> Result<RepoFacts, String> {
        let decisions_path = root.join("docs/digest/decisions-in-force.md");
        let decisions_text = std::fs::read_to_string(&decisions_path)
            .map_err(|e| format!("reading {}: {e}", decisions_path.display()))?;
        let in_force = parse_decisions_in_force(&decisions_text);
        if in_force.is_empty() {
            return Err(format!(
                "{} parsed to zero in-force decisions. Treating this as a \
                 parse failure, not a (false) claim that no decision is in \
                 force: check the file still exists and its lines still \
                 match `- **NNNN** …` (the `digest render decisions` output \
                 format).",
                decisions_path.display()
            ));
        }
        let decisions_stamp = stamp_text(&decisions_text);

        let registry_path = root.join("book/src/frontier/idea-registry.md");
        let registry_text = std::fs::read_to_string(&registry_path)
            .map_err(|e| format!("reading {}: {e}", registry_path.display()))?;
        let registry = parse_registry_statuses(&registry_text);
        if registry.is_empty() {
            return Err(format!(
                "{} parsed to zero registry rows. Treating this as a parse \
                 failure, not a (false) claim that the registry is empty: \
                 check the file still exists and its rows still look like \
                 `| ID | … |`.",
                registry_path.display()
            ));
        }

        Ok(RepoFacts {
            in_force,
            registry,
            crates: gather_crate_directories(root)?,
            subsystems: gather_subsystem_directories(root)?,
            decisions_stamp,
            root: root.to_path_buf(),
        })
    }

    /// How `spec` (`<crate>::<fn>`) resolves against the live repo. A cheap
    /// text search, not a compile — the resolver never shells out to
    /// `cargo`. See [`TestResolution`] for what each outcome means.
    fn test_resolution(&self, spec: &str) -> TestResolution {
        let Some((crate_name, symbol)) = spec.rsplit_once("::") else {
            return TestResolution::Missing;
        };
        let Some(dir) = self.crates.get(crate_name) else {
            return TestResolution::Missing;
        };
        directory_defines_symbol(&self.root.join(dir), symbol)
    }
}

/// Parse `text` (the content of `docs/digest/decisions-in-force.md`) and
/// collect every in-force decision number. Lines have the form
/// `- **NNNN** …`; a wholly superseded decision is absent by construction
/// (the file's own generation rule), so its number simply never enters the
/// set.
fn parse_decisions_in_force(text: &str) -> BTreeSet<String> {
    let mut in_force = BTreeSet::new();
    for line in text.lines() {
        let Some(rest) = line.strip_prefix("- **") else {
            continue;
        };
        let Some(end) = rest.find("**") else {
            continue;
        };
        let code = &rest[..end];
        if code.len() == 4 && code.chars().all(|c| c.is_ascii_digit()) {
            in_force.insert(code.to_string());
        }
    }
    in_force
}

/// A cheap, traceable stamp of `text`'s content — its byte length and a
/// rolling checksum, never a shell-out to `git`.
fn stamp_text(text: &str) -> String {
    let mut checksum: u64 = 0;
    for b in text.bytes() {
        checksum = checksum.wrapping_mul(31).wrapping_add(u64::from(b));
    }
    format!("{}b/{checksum:016x}", text.len())
}

/// True when `cell` is a registry ID. Mirrors the shape of
/// `looks_like_registry_id` in `cli/tests/docs_consistency.rs` — the same
/// rule, deliberately not a second one: a category prefix, a hyphen, then
/// either a number with an optional sub-letter (`MAP-9`, `MAP-9a`) or a
/// lowercase slug (`SKY-eclipse-seasons`).
fn looks_like_registry_id(cell: &str) -> bool {
    cell.split_once('-').is_some_and(|(pre, post)| {
        let numbered = post.starts_with(|c: char| c.is_ascii_digit())
            && post
                .trim_end_matches(|c: char| c.is_ascii_lowercase())
                .chars()
                .all(|c| c.is_ascii_digit());
        let slug = post.starts_with(|c: char| c.is_ascii_lowercase())
            && post
                .chars()
                .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
        !pre.is_empty()
            && pre.chars().all(|c| c.is_ascii_uppercase())
            && !post.is_empty()
            && (numbered || slug)
    })
}

/// Parse `text` (the content of `book/src/frontier/idea-registry.md`) and
/// collect every registry row's ID → NORMALIZED `status` cell (`id |
/// description | status | confidence | where`; see `normalize_status`).
fn parse_registry_statuses(text: &str) -> BTreeMap<String, String> {
    let mut registry = BTreeMap::new();
    for line in text.lines() {
        if !line.starts_with("| ") {
            continue;
        }
        let masked = line.replace("\\|", "\u{1}");
        let pieces: Vec<String> = masked
            .split('|')
            .map(|p| p.replace('\u{1}', "\\|").trim().to_string())
            .collect();
        // pieces[0] is the empty text before the leading `|`; pieces[1] is
        // the id, pieces[3] is the status cell.
        let Some(id) = pieces.get(1) else { continue };
        if !looks_like_registry_id(id) {
            continue;
        }
        let status = pieces
            .get(3)
            .map(|s| normalize_status(s))
            .unwrap_or_default();
        registry.insert(id.clone(), status);
    }
    registry
}

/// Reduce a Status cell to its bare token: strip `**` emphasis, a trailing
/// `→ <status>` transition, and any trailing parenthetical (`ratified
/// (0009)`, `shipped (field half)`). Mirrors `normalize_status` in
/// `cli/tests/docs_consistency.rs` — the same rule, deliberately not a
/// second one, for the same reason `looks_like_registry_id` above is
/// mirrored rather than reinvented: a naive exact match on the bare word
/// `shipped` misses `shipped (C1)`, `**shipped**`, and `shipped →
/// superseded (Goldengrove)` (which DOES normalize to `shipped` — a row
/// that shipped and was later superseded is still not something to defer
/// against), while a naive substring search on `"shipped"` wrongly fires on
/// `elaborated (slice-2 shipped)`, which never shipped itself.
fn normalize_status(cell: &str) -> String {
    let mut s = cell.replace('*', "");
    if let Some((head, _)) = s.split_once('→') {
        s = head.to_string();
    }
    if let Some((head, _)) = s.split_once('(') {
        s = head.to_string();
    }
    s.trim().to_string()
}

/// Read one crate's `name = "…"` from its `Cargo.toml`, if it has one.
fn crate_name_at(dir: &Path) -> Option<String> {
    let manifest_text = std::fs::read_to_string(dir.join("Cargo.toml")).ok()?;
    manifest_text.lines().find_map(|l| {
        l.trim()
            .strip_prefix("name = \"")
            .and_then(|rest| rest.strip_suffix('"'))
            .map(|s| s.to_string())
    })
}

/// Walk `domains/*/Cargo.toml` and `windows/*/Cargo.toml`, mapping each
/// crate's `name = "…"` to its directory (`hornvale-terrain` →
/// `domains/terrain`) — the 25 subsystem directories Task 5's surplus read
/// enumerates directly. Also adds `kernel/` and `cli/` themselves
/// (`hornvale-kernel`, `hornvale`): neither is a "subsystem" in that sense,
/// but `test:` anchors legitimately cite mechanism living there
/// (determinism/`WorldTime`/`quantize` in `kernel/`, the CLI surface in
/// `cli/`), and without them any such anchor would DANGLE unconditionally
/// regardless of correctness.
fn gather_crate_directories(root: &Path) -> Result<BTreeMap<String, String>, String> {
    let mut crates = BTreeMap::new();
    for parent in ["domains", "windows"] {
        let parent_path = root.join(parent);
        let entries = std::fs::read_dir(&parent_path)
            .map_err(|e| format!("reading {}: {e}", parent_path.display()))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("reading {}: {e}", parent_path.display()))?;
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let Some(name) = crate_name_at(&path) else {
                continue;
            };
            let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            crates.insert(name, format!("{parent}/{dir_name}"));
        }
    }
    for dir_name in ["kernel", "cli"] {
        let path = root.join(dir_name);
        if let Some(name) = crate_name_at(&path) {
            crates.insert(name, dir_name.to_string());
        }
    }
    Ok(crates)
}

/// Walk `domains/*` and `windows/*`, collecting each crate directory as
/// `<parent>/<name>` (`domains/terrain`, `windows/vessel`, …), sorted — the
/// subsystem directories Task 5's surplus read enumerates (spec §6). A
/// SECOND walk, not a projection of [`gather_crate_directories`]'s `crates`
/// map: that map also carries `kernel/` and `cli/`, and unifying the two
/// would make this list silently count those as subsystems too. Requires
/// each entry to actually be a crate (has a readable `name = "…"` in its
/// `Cargo.toml`), the same filter `gather_crate_directories` applies, so a
/// stray non-crate directory under either parent is not mistaken for one.
fn gather_subsystem_directories(root: &Path) -> Result<Vec<String>, String> {
    let mut dirs = Vec::new();
    for parent in ["domains", "windows"] {
        let parent_path = root.join(parent);
        let entries = std::fs::read_dir(&parent_path)
            .map_err(|e| format!("reading {}: {e}", parent_path.display()))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("reading {}: {e}", parent_path.display()))?;
            let path = entry.path();
            if !path.is_dir() || crate_name_at(&path).is_none() {
                continue;
            }
            let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            dirs.push(format!("{parent}/{dir_name}"));
        }
    }
    dirs.sort();
    Ok(dirs)
}

/// True when the character `c` cannot continue a Rust identifier — i.e. it
/// is a valid boundary after a matched symbol name.
fn is_identifier_boundary(c: char) -> bool {
    !(c.is_alphanumeric() || c == '_')
}

/// How a `test:` anchor's symbol resolved against the live repo — three
/// outcomes, not two, because "the source defines this symbol" and "the
/// gate runs it" are different facts. Collapsing them is the fourth
/// false-clean this resolver has produced: a raw-text search for `fn
/// <symbol>` cannot see that the match is `#[ignore]`d, so an anchor citing
/// a heavy or otherwise-skipped battery read as resolved exactly like one
/// the gate actually exercises.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestResolution {
    /// No boundary-valid `fn <symbol>` definition found anywhere searched
    /// (an unknown crate counts as this too).
    Missing,
    /// A definition was found, and at least one occurrence is NOT governed
    /// by a preceding `#[ignore]` — the gate runs it.
    Runs,
    /// A definition was found, but every occurrence found is `#[ignore]`d —
    /// the test exists in source, but the gate never executes it.
    Ignored,
}

impl TestResolution {
    /// Fold another text's or subdirectory's result into this one. `Runs`
    /// wins over `Ignored`, which wins over `Missing` — one clean,
    /// executable definition anywhere under the searched directory is
    /// enough to call the anchor resolved, even if another same-named
    /// definition elsewhere happens to be ignored.
    fn combine(self, other: TestResolution) -> TestResolution {
        use TestResolution::{Ignored, Missing, Runs};
        match (self, other) {
            (Runs, _) | (_, Runs) => Runs,
            (Ignored, _) | (_, Ignored) => Ignored,
            (Missing, Missing) => Missing,
        }
    }
}

/// How many lines above a matched `fn` definition to scan for a governing
/// `#[ignore]` attribute, and why 10: attributes stack in either order
/// (`#[test]` then `#[ignore]`, or the reverse — both occur in this repo
/// today), a doc comment may sit between the definition and its attributes,
/// and an `#[ignore]`'s string reason may itself wrap across several
/// physical lines. Measured across the whole tree (the `fn` line minus the line that
/// actually contains the literal text `#[ignore`), the longest observed gap
/// is 6 — a wrapped multi-line reason string in `windows/worldgen/src/
/// lib.rs`. This scans 10 for headroom past that measured maximum rather
/// than tuning to it exactly.
const IGNORE_SCAN_LINES: usize = 10;

/// Whether the `fn` definition starting at byte offset `match_start` in
/// `text` is governed by a preceding `#[ignore]` attribute within
/// [`IGNORE_SCAN_LINES`] lines.
///
/// A flat line-count window, not a parse of the attribute block, because
/// the window must also see INSIDE a multi-line `#[ignore]` reason string —
/// a smarter scan that stopped at the first line not shaped like an
/// attribute, doc comment, or blank line would stop on the string's own
/// continuation lines (plain prose, no leading `#[` or `///`) before ever
/// reaching the line that names the attribute.
///
/// **Known trade-off, not silently assumed safe:** a window this wide can
/// in principle see an `#[ignore]` that governs a different, closely
/// preceding item rather than this one, misreading a clean test as
/// ignored. Checked against all 26 real `test:` anchors in
/// `systems/wolverson-2021.system.json` (2026-08-15): none false-positive.
/// If one someday did, the failure direction is a confusing-but-safe
/// DANGLING, never the false-CLEAN this function exists to close.
fn definition_is_ignored(text: &str, match_start: usize) -> bool {
    let preceding = &text[..match_start];
    let lines: Vec<&str> = preceding.lines().collect();
    let start = lines.len().saturating_sub(IGNORE_SCAN_LINES);
    lines[start..].iter().any(|l| l.contains("#[ignore"))
}

/// How `text` defines a function named exactly `symbol` — `fn {symbol}`
/// found at a word boundary, so the character immediately after `symbol`
/// (if any) cannot continue an identifier. A plain substring search on `fn
/// {symbol}` would read a short symbol as already "defining" any longer
/// function name it happens to prefix — the wrong direction of error for an
/// instrument whose job is to notice when an anchor stops resolving.
///
/// Scans every boundary-valid occurrence (not just the first) and folds
/// their [`TestResolution`]s with [`TestResolution::combine`], so one
/// `#[ignore]`d match earlier in the file cannot hide a later clean one.
fn text_defines_symbol(text: &str, symbol: &str) -> TestResolution {
    let needle = format!("fn {symbol}");
    let mut search_from = 0;
    let mut result = TestResolution::Missing;
    while let Some(offset) = text[search_from..].find(needle.as_str()) {
        let match_start = search_from + offset;
        let after = match_start + needle.len();
        let boundary_ok = text[after..]
            .chars()
            .next()
            .map(is_identifier_boundary)
            .unwrap_or(true);
        if boundary_ok {
            let this = if definition_is_ignored(text, match_start) {
                TestResolution::Ignored
            } else {
                TestResolution::Runs
            };
            result = result.combine(this);
            if result == TestResolution::Runs {
                return result;
            }
        }
        search_from = match_start + 1;
    }
    result
}

/// How any `.rs` file under `dir` (recursively, skipping `target`) defines
/// `fn <symbol>` at a word boundary — folded across every file the same way
/// [`text_defines_symbol`] folds across every match within one file.
fn directory_defines_symbol(dir: &Path, symbol: &str) -> TestResolution {
    let mut result = TestResolution::Missing;
    let Ok(entries) = std::fs::read_dir(dir) else {
        return result;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().and_then(|n| n.to_str()) == Some("target") {
                continue;
            }
            result = result.combine(directory_defines_symbol(&path, symbol));
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs")
            && let Ok(text) = std::fs::read_to_string(&path)
        {
            result = result.combine(text_defines_symbol(&text, symbol));
        }
        if result == TestResolution::Runs {
            return result;
        }
    }
    result
}

#[cfg(test)]
mod boundary_tests {
    use super::{TestResolution, text_defines_symbol};

    /// The bug this function exists to fix: `verdict` must NOT read as
    /// defining `verdict_name`.
    #[test]
    fn a_strict_prefix_does_not_count_as_a_definition() {
        assert_eq!(
            text_defines_symbol("fn verdict_name(v: Verdict) {}", "verdict"),
            TestResolution::Missing
        );
    }

    #[test]
    fn an_exact_match_counts() {
        assert_eq!(
            text_defines_symbol("fn verdict_name(v: Verdict) {}", "verdict_name"),
            TestResolution::Runs
        );
    }

    #[test]
    fn a_match_at_end_of_file_counts() {
        assert_eq!(
            text_defines_symbol("pub fn resolve", "resolve"),
            TestResolution::Runs
        );
    }

    #[test]
    fn a_match_followed_by_punctuation_counts() {
        assert_eq!(
            text_defines_symbol("pub fn resolve<T>(x: T) -> T { x }", "resolve"),
            TestResolution::Runs
        );
        assert_eq!(
            text_defines_symbol("pub fn resolve(x: T) -> T { x }", "resolve"),
            TestResolution::Runs
        );
        assert_eq!(
            text_defines_symbol("pub fn resolve\n(x: T) -> T { x }", "resolve"),
            TestResolution::Runs
        );
    }

    /// 5b's positive case: an ordinary `#[test]` (no `#[ignore]` anywhere
    /// nearby) resolves `Runs`.
    #[test]
    fn an_ordinary_test_attribute_resolves_as_running() {
        assert_eq!(
            text_defines_symbol("#[test]\nfn plain_test() {}", "plain_test"),
            TestResolution::Runs
        );
    }

    /// 5b, the immediately-preceding case: `#[ignore]` directly above `fn`.
    #[test]
    fn an_ignore_attribute_immediately_above_fn_is_ignored() {
        assert_eq!(
            text_defines_symbol("#[test]\n#[ignore]\nfn skipped_test() {}", "skipped_test"),
            TestResolution::Ignored
        );
    }

    /// 5b, the stacking-order case this repo actually has (`windows/lab/
    /// tests/anomaly_injection.rs`): `#[ignore]` BEFORE `#[test]`,
    /// two lines above `fn`. The immediately-preceding-line case above is
    /// not the only shape the scan must catch.
    #[test]
    fn an_ignore_attribute_before_a_test_attribute_is_still_seen() {
        assert_eq!(
            text_defines_symbol(
                "#[ignore = \"reason\"]\n#[test]\nfn skipped_test() {}",
                "skipped_test"
            ),
            TestResolution::Ignored
        );
    }

    /// 5b, the multi-line reason case this repo actually has
    /// (`windows/worldgen/src/lib.rs`): the `#[ignore]` reason string itself
    /// wraps across several physical lines before `fn`.
    #[test]
    fn an_ignore_attribute_with_a_wrapped_multiline_reason_is_still_seen() {
        let text = "#[test]\n#[ignore = \"line one \\\n            line two \\\n            line three\"]\nfn skipped_test() {}";
        assert_eq!(
            text_defines_symbol(text, "skipped_test"),
            TestResolution::Ignored
        );
    }

    /// A same-named definition elsewhere in the SAME file that runs must
    /// win over an earlier ignored one — `combine` prefers `Runs`. The two
    /// occurrences are separated by more than `IGNORE_SCAN_LINES` blank
    /// lines so the second's scan window cannot see the first's
    /// `#[ignore]` — isolating `combine`'s own behaviour from the window's
    /// separately-documented reach.
    #[test]
    fn a_running_definition_elsewhere_in_the_file_wins_over_an_ignored_one() {
        let filler = "\n".repeat(super::IGNORE_SCAN_LINES + 2);
        let text = format!("#[ignore]\nfn dup() {{}}\n{filler}fn dup() {{}}\n");
        assert_eq!(text_defines_symbol(&text, "dup"), TestResolution::Runs);
    }
}

/// One thing wrong with a verdict's evidence. Every variant carries enough
/// to diagnose it WITHOUT knowing this instrument exists — the spec's §5
/// requirement, because the idea registry is now a gated interface and the
/// session that reddens it may have been editing a registry row.
/// type-audit: bare-ok(identifier-text: Unjustified.id), bare-ok(prose: Unjustified.why), bare-ok(identifier-text: Dangling.id), bare-ok(identifier-text: Dangling.anchor), bare-ok(prose: Dangling.why), bare-ok(identifier-text: StaleDeferred.id), bare-ok(identifier-text: StaleDeferred.row), bare-ok(prose: StaleDeferred.why)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Finding {
    /// A non-`absent` verdict with no anchor, or the wrong kind of anchor.
    Unjustified {
        /// The item's corpus-local id.
        id: String,
        /// What is wrong, in a sentence.
        why: String,
    },
    /// The anchor stopped resolving.
    Dangling {
        /// The item's corpus-local id.
        id: String,
        /// The anchor as authored.
        anchor: String,
        /// What would have caused this, and the two legitimate repairs.
        why: String,
    },
    /// A `deferred` verdict whose registry row now reads `shipped`.
    StaleDeferred {
        /// The item's corpus-local id.
        id: String,
        /// The registry row that shipped.
        row: String,
        /// What to do about it.
        why: String,
    },
}

/// Human name of a verdict, for failure text.
fn verdict_name(v: Verdict) -> &'static str {
    match v {
        Verdict::Present => "present",
        Verdict::Refused => "refused",
        Verdict::Deferred => "deferred",
        Verdict::Absent => "absent",
        Verdict::Inapplicable => "inapplicable",
    }
}

/// The anchor kind(s) a verdict requires, for failure text.
fn expected_anchor_kind(v: Verdict) -> &'static str {
    match v {
        Verdict::Present => "a `test:` or `path:` anchor",
        Verdict::Refused => "a `decision:` anchor",
        Verdict::Deferred => "a `registry:` anchor",
        Verdict::Inapplicable => "a `reason:` anchor",
        Verdict::Absent => "no anchor",
    }
}

/// The anchor's own kind, for failure text.
fn anchor_kind_name(a: &Anchor) -> &'static str {
    match a {
        Anchor::Decision(_) => "decision:",
        Anchor::Registry(_) => "registry:",
        Anchor::Test(_) => "test:",
        Anchor::Path(_) => "path:",
        Anchor::Reason(_) => "reason:",
    }
}

/// Whether `a`'s kind is one the verdict `v` permits.
fn anchor_matches_verdict(v: Verdict, a: &Anchor) -> bool {
    matches!(
        (v, a),
        (Verdict::Present, Anchor::Test(_))
            | (Verdict::Present, Anchor::Path(_))
            | (Verdict::Refused, Anchor::Decision(_))
            | (Verdict::Deferred, Anchor::Registry(_))
            | (Verdict::Inapplicable, Anchor::Reason(_))
    )
}

/// Audit every item's verdict against its anchor, verifying that each
/// anchor still resolves against the live repo (`facts`). Returns one
/// finding per problem; a clean corpus returns an empty vector.
pub fn audit(corpus: &Corpus, facts: &RepoFacts) -> Vec<Finding> {
    corpus
        .items
        .iter()
        .filter_map(|item| audit_item(item, facts))
        .collect()
}

/// Audit a single item. `None` means clean.
fn audit_item(item: &Item, facts: &RepoFacts) -> Option<Finding> {
    if item.verdict == Verdict::Absent {
        return item.anchor.as_ref().map(|anchor| Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `absent` but carries anchor `{anchor}`. An `absent` \
                 verdict claims nothing and must carry no anchor — remove the anchor, \
                 or change the verdict to the one the anchor actually supports.",
                item.id
            ),
        });
    }

    let Some(anchor_str) = &item.anchor else {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}` but no anchor. Every verdict except `absent` \
                 must cite evidence: {} is required. Add an anchor, or change the \
                 verdict to `absent` if there is truly nothing to cite.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict)
            ),
        });
    };

    let Some(anchor) = Anchor::parse(anchor_str) else {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} cites `{anchor_str}`, which has an unrecognized anchor prefix. \
                 Expected one of decision:, registry:, test:, path:, reason:.",
                item.id
            ),
        });
    };

    if !anchor_matches_verdict(item.verdict, &anchor) {
        return Some(Finding::Unjustified {
            id: item.id.clone(),
            why: format!(
                "{} has verdict `{}`, which requires {}, but its anchor `{anchor_str}` \
                 is a {} anchor.",
                item.id,
                verdict_name(item.verdict),
                expected_anchor_kind(item.verdict),
                anchor_kind_name(&anchor)
            ),
        });
    }

    resolve_anchor(item, anchor_str, &anchor, facts)
}

/// Verify `anchor` still resolves against `facts`. `None` means clean.
fn resolve_anchor(
    item: &Item,
    anchor_str: &str,
    anchor: &Anchor,
    facts: &RepoFacts,
) -> Option<Finding> {
    match anchor {
        Anchor::Decision(d) => {
            if facts.in_force.contains(d) {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: anchor_str.to_string(),
                    why: format!(
                        "{} cites decision:{d}, which is not in docs/digest/decisions-in-force.md.\n\
                         A decision leaves that file when it is wholly superseded. Either re-verdict\n\
                         this item against the superseding decision, or restore the anchor if the\n\
                         supersession was partial. (decisions-in-force stamp: {})",
                        item.id, facts.decisions_stamp
                    ),
                })
            }
        }
        Anchor::Registry(r) => match facts.registry.get(r) {
            None => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: anchor_str.to_string(),
                why: format!(
                    "{} cites registry:{r}, which does not appear in\n\
                     book/src/frontier/idea-registry.md. Either the row ID changed (fix the\n\
                     anchor to match) or the row was removed (re-verdict this item against\n\
                     whatever replaced it). The idea registry is hand-authored, not\n\
                     digest-generated, so there is no stamp to print here — check that file's\n\
                     own history (`git log -p -- book/src/frontier/idea-registry.md`) for when\n\
                     the row changed.",
                    item.id
                ),
            }),
            Some(status) if status == "shipped" => Some(Finding::StaleDeferred {
                id: item.id.clone(),
                row: r.clone(),
                why: format!(
                    "{} defers to registry:{r}, which now reads `shipped` in\n\
                     book/src/frontier/idea-registry.md. A deferral is a promise that has not\n\
                     yet been kept; once the registry says it shipped, promote this item to\n\
                     `present` (citing the shipping mechanism) or to `refused` if what shipped\n\
                     does not actually satisfy it. The idea registry is hand-authored, not\n\
                     digest-generated, so there is no stamp to print here — check that file's\n\
                     own history for when the row shipped.",
                    item.id
                ),
            }),
            Some(_) => None,
        },
        Anchor::Path(p) => {
            if facts.root.join(p).exists() {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: anchor_str.to_string(),
                    why: format!(
                        "{} cites path:{p}, which does not exist in the repo. Either the file\n\
                         moved (fix the anchor to its new location) or it was deleted\n\
                         (re-verdict this item).",
                        item.id
                    ),
                })
            }
        }
        Anchor::Test(t) => match facts.test_resolution(t) {
            TestResolution::Runs => None,
            TestResolution::Missing => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: anchor_str.to_string(),
                why: format!(
                    "{} cites test:{t}, which does not resolve to a known crate and a\n\
                     `fn` definition of that name at a word boundary. Either the function\n\
                     moved or was renamed (fix the anchor) or it was removed (re-verdict\n\
                     this item).",
                    item.id
                ),
            }),
            TestResolution::Ignored => Some(Finding::Dangling {
                id: item.id.clone(),
                anchor: anchor_str.to_string(),
                why: format!(
                    "{} cites test:{t}, which exists but is `#[ignore]`d — the test does\n\
                     not run under the gate, so it proves nothing about this item. Either\n\
                     cite a test the gate actually runs, or weaken the verdict.",
                    item.id
                ),
            }),
        },
        Anchor::Reason(_) => None,
    }
}

/// How many of `corpus`'s `present` items' anchors cite each of `facts`'
/// subsystem directories. The shared derivation behind both [`surplus`]
/// (which only needs the set of keys) and [`render_matrix`]'s declared
/// limitation (which needs the busiest one's count) — one pass over the
/// corpus, not two, so the two readings cannot silently disagree about what
/// "cited" means.
///
/// A subsystem counts as cited when some `present` item's anchor either:
/// - is a `path:` anchor whose path starts with that directory, or
/// - is a `test:` anchor whose crate maps (via `RepoFacts::crates`) to
///   exactly that directory.
fn citation_counts(corpus: &Corpus, facts: &RepoFacts) -> BTreeMap<String, usize> {
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for item in &corpus.items {
        if item.verdict != Verdict::Present {
            continue;
        }
        let Some(anchor_str) = &item.anchor else {
            continue;
        };
        let Some(anchor) = Anchor::parse(anchor_str) else {
            continue;
        };
        match anchor {
            Anchor::Path(p) => {
                for dir in &facts.subsystems {
                    if p.starts_with(&format!("{dir}/")) || p == *dir {
                        *counts.entry(dir.clone()).or_insert(0) += 1;
                    }
                }
            }
            Anchor::Test(t) => {
                if let Some((crate_name, _)) = t.rsplit_once("::")
                    && let Some(dir) = facts.crates.get(crate_name)
                    && facts.subsystems.contains(dir)
                {
                    *counts.entry(dir.clone()).or_insert(0) += 1;
                }
            }
            Anchor::Decision(_) | Anchor::Registry(_) | Anchor::Reason(_) => {}
        }
    }
    counts
}

/// The surplus read (spec §6): which of `facts`' subsystem directories no
/// `present` item's anchor cites — the corpus has no vocabulary for them.
/// Derived from the corpus and the live directory tree on every call, never
/// authored, the same discipline the anchor audit itself follows: an
/// authored list would rot exactly as the anchor requirement exists to
/// prevent, and this one moves on its own as domains land.
///
/// **Declared limitation** (spec §6, printed by [`render_matrix`] next to
/// the list this produces): subsystem granularity is coarse. A directory
/// cited by a single item's single anchor reads as fully covered here, even
/// when that anchor is one test among hundreds this instrument never
/// inspects.
/// type-audit: bare-ok(prose: return)
pub fn surplus(corpus: &Corpus, facts: &RepoFacts) -> Vec<String> {
    let cited = citation_counts(corpus, facts);
    facts
        .subsystems
        .iter()
        .filter(|d| !cited.contains_key(d.as_str()))
        .cloned()
        .collect()
}

// --- Rendering ---------------------------------------------------------
//
// `tropes::wrap` and `tropes::percent` are private (`cli/src/tropes.rs:144`
// and `:458`) and this family is a deliberate sibling, never a member (spec
// §3) — so the two are reimplemented here rather than widened to `pub` for a
// sibling's convenience. Same shape, same reasoning, on purpose.

/// Hard-wrap a prose paragraph at 76 columns on word boundaries. See
/// `tropes::wrap` for the full rationale (a byte-ratcheted artifact needs
/// wrapped prose so a single word edit stays a single-line diff); duplicated
/// here rather than imported because `tropes::wrap` is private by design.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
fn wrap(text: &str) -> String {
    let mut out = String::new();
    let mut col = 0;
    for word in text.split_whitespace() {
        let w = word.chars().count();
        if col > 0 && col + 1 + w > 76 && !word.starts_with('-') {
            out.push('\n');
            col = 0;
        } else if col > 0 {
            out.push(' ');
            col += 1;
        }
        out.push_str(word);
        col += w;
    }
    out
}

/// `n` of `total` as a whole percent, rounded half up. Integer arithmetic on
/// purpose — this figure lands in a byte-ratcheted artifact, and decision
/// 0033 keeps floats away from serialization boundaries. Duplicated from
/// `tropes::percent` for the same reason as [`wrap`].
fn percent(n: usize, total: usize) -> usize {
    if total == 0 {
        0
    } else {
        (n * 200 + total) / (total * 2)
    }
}

/// Where a corpus's committed report lives, derived from the corpus's own
/// identifier so a caller cannot pair the wrong corpus with the wrong
/// artifact.
/// type-audit: bare-ok(identifier-text: return)
pub fn artifact_path(corpus: &Corpus) -> String {
    format!("docs/audits/system-coverage-{}.md", corpus.corpus)
}

/// The command that regenerates a report, for the header. Takes the path the
/// caller actually used rather than deriving one from the corpus id, exactly
/// as `tropes::regenerate_command` does and for the same reason: a derived
/// stem could print a command naming a file that does not exist.
/// type-audit: bare-ok(identifier-text: path), bare-ok(identifier-text: return)
pub fn regenerate_command(path: &str) -> String {
    format!("hornvale systems --corpus {path} report")
}

/// Verdict counts across `corpus`, in decision 0095's five-verdict order —
/// the order the report's tally prints them in.
fn tally(corpus: &Corpus) -> [(Verdict, usize); 5] {
    let mut present = 0usize;
    let mut refused = 0usize;
    let mut deferred = 0usize;
    let mut absent = 0usize;
    let mut inapplicable = 0usize;
    for item in &corpus.items {
        match item.verdict {
            Verdict::Present => present += 1,
            Verdict::Refused => refused += 1,
            Verdict::Deferred => deferred += 1,
            Verdict::Absent => absent += 1,
            Verdict::Inapplicable => inapplicable += 1,
        }
    }
    [
        (Verdict::Present, present),
        (Verdict::Refused, refused),
        (Verdict::Deferred, deferred),
        (Verdict::Absent, absent),
        (Verdict::Inapplicable, inapplicable),
    ]
}

/// The first item in corpus order whose capability is not met — not
/// `present` (the capability exists) and not `inapplicable` (the row makes
/// no capability claim, e.g. the tutorial's own toolchain front-matter). For
/// an `ordered` corpus this is a pedagogical ladder, so this is "the first
/// chapter Hornvale cannot replicate" (spec §8) — the corpus's single most
/// useful reading. Callers must gate this on `corpus.ordered` themselves
/// (spec §14): ranking an unordered catalogue by `id` would manufacture a
/// ladder its source never had.
fn first_unmet(corpus: &Corpus) -> Option<&Item> {
    corpus
        .items
        .iter()
        .find(|item| !matches!(item.verdict, Verdict::Present | Verdict::Inapplicable))
}

/// Extract the `absent` count from a previously rendered report's tally —
/// used only to compare a live corpus's absent count against what the last
/// committed artifact recorded, never to reconstruct the whole corpus.
/// `None` when `report` does not look like one of this family's reports (no
/// tally line found), which a caller should treat as "nothing to compare
/// against," not as zero.
/// type-audit: bare-ok(prose: report), bare-ok(count: return)
pub fn committed_absent_count(report: &str) -> Option<usize> {
    for line in report.lines() {
        // NOT `strip_prefix(..)?`: that `?` would propagate a non-matching
        // line's `None` out of the whole function on the very first line
        // that fails to match — almost always line 1, the generated-file
        // banner — so the loop would never reach the tally at all. A `let
        // else { continue }` skips only this line.
        let Some(rest) = line.strip_prefix("- absent: ") else {
            continue;
        };
        let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
        if digits.is_empty() {
            return None;
        }
        return digits.parse().ok();
    }
    None
}

/// Render the coverage report. Order is fixed by decision 0095 — provenance
/// before any number — with one addition this corpus's bias demands (spec
/// §7): the `present`-is-weakly-checked caveat prints above the tally too, so
/// a reader cannot reach the score without passing the statement that
/// `present` is the verdict this instrument is least entitled to.
///
/// `path` is the corpus source path the caller actually resolved (mirroring
/// `tropes::render`), so the regenerate command in the banner names a file
/// that really exists rather than a stem derived from `corpus.corpus`.
/// type-audit: bare-ok(identifier-text: path), bare-ok(prose: return)
pub fn render(corpus: &Corpus, path: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{}`. -->\n\n",
        regenerate_command(path)
    ));
    s.push_str("# System coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!("- **Source:** {}\n", wrap(&corpus.provenance)));
    s.push_str(&format!("- **Frozen:** {}\n", wrap(&corpus.frozen)));

    s.push_str("\n## Reading this report\n\n");
    s.push_str(&wrap(
        "`refused` and `deferred` are strongly checked: a decision must be in force, and a \
         registry row must exist and must not read `shipped`. `present` is only weakly \
         checked — a path that exists is not a working feature, and a resolvable test name \
         is not proof that the capability is met. `present` is the verdict this instrument \
         is least entitled to, and that is printed here, above the tally it most affects.",
    ));
    s.push_str("\n\n");

    let counts = tally(corpus);
    let total = corpus.items.len();
    s.push_str("## Tally\n\n");
    for (v, n) in counts {
        s.push_str(&format!(
            "- {}: {n} ({}%)\n",
            verdict_name(v),
            percent(n, total)
        ));
    }

    if corpus.ordered {
        s.push_str("\n## First unmet\n\n");
        match first_unmet(corpus) {
            Some(item) => s.push_str(&format!(
                "{} — **{}** ({}).\n",
                item.id,
                item.title,
                verdict_name(item.verdict)
            )),
            None => s.push_str("Every item is `present` or `inapplicable`.\n"),
        }
    }

    // The `note` column (Task 5, 5a) surfaces the author's own
    // qualification alongside the verdict — including the seven items
    // whose note starts `ARGUABLE`, the strongest form of self-doubt this
    // corpus records. Without it, those rows printed exactly as flat as a
    // strongly-anchored one, which is the same "scorecard, not an
    // instrument" failure the caveat above this table already exists to
    // avoid for the tally. `|` is escaped defensively: today no note
    // contains one, but a table cell silently breaks the moment one does,
    // and nothing else in this rendering pipeline checks for that.
    s.push_str("\n## Items\n\n| id | title | verdict | anchor | note |\n|---|---|---|---|---|\n");
    for item in &corpus.items {
        s.push_str(&format!(
            "| {} | {} | {} | {} | {} |\n",
            item.id,
            item.title,
            verdict_name(item.verdict),
            item.anchor.as_deref().unwrap_or(""),
            item.note.replace('|', "\\|")
        ));
    }
    s
}

/// The matrix over every corpus in [`CORPORA`] (one today).
///
/// The trope matrix's most valuable table is the demand read — what the
/// catalogues disagree about. This family's analogue is the surplus read
/// (spec §6), so that is the one thing this document holds that no single
/// corpus's own report can: which subsystems no corpus's `present` verdicts
/// cite AT ALL. With one corpus that reduces to that corpus's own
/// [`surplus`]; the intersection below is written for the day a second
/// corpus lands, so this function does not need to change shape then.
///
/// `corpora` and `facts` are both caller-supplied rather than loaded here
/// (mirroring `tropes::render_matrix`): the caller reads every path in
/// [`CORPORA`] once and gathers `facts` once, so two columns cannot
/// silently disagree about what the live repo looked like when they were
/// scored.
/// type-audit: bare-ok(prose: return)
pub fn render_matrix(corpora: &[&Corpus], facts: &RepoFacts) -> String {
    let mut s = String::new();
    s.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale systems matrix`. -->\n\n",
    );
    s.push_str("# The system matrix\n\n");

    s.push_str(&wrap(
        "One row per corpus in `systems::CORPORA`. Each column is that corpus's own \
         five-verdict tally, recomputed here from the corpus rather than parsed back out \
         of its committed report, so this document cannot inherit a report's mistake.",
    ));
    s.push_str("\n\n");

    s.push_str("## Corpora\n\n");
    s.push_str(
        "| Corpus | present | refused | deferred | absent | inapplicable | Report |\n\
         |---|---|---|---|---|---|---|\n",
    );
    for corpus in corpora {
        let counts = tally(corpus);
        let path = artifact_path(corpus);
        let file = path.rsplit('/').next().unwrap_or(&path);
        s.push_str(&format!(
            "| `{}` | {} | {} | {} | {} | {} | [{file}](./{file}) |\n",
            corpus.corpus, counts[0].1, counts[1].1, counts[2].1, counts[3].1, counts[4].1,
        ));
    }
    s.push('\n');

    // The surplus read: subsystems no corpus's `present` verdicts cite AT
    // ALL. Starts from every subsystem and narrows by intersecting each
    // corpus's own `surplus` — a directory only belongs here if it is
    // surplus to EVERY corpus, i.e. cited by NONE of them.
    let mut surplus_across = facts.subsystems.clone();
    for corpus in corpora {
        let this_surplus: BTreeSet<String> = surplus(corpus, facts).into_iter().collect();
        surplus_across.retain(|d| this_surplus.contains(d));
    }

    // The caveat this list must never be read without (spec §6): the busiest
    // cited subsystem across every corpus, named and counted from the SAME
    // `citation_counts` the surplus read itself uses, so the illustration
    // can never drift from what the list above it actually says. Derived
    // fresh every run rather than a hand-picked example, because a hand-
    // picked example is exactly the kind of thing that goes stale the
    // moment a verdict moves.
    let mut combined_counts: BTreeMap<String, usize> = BTreeMap::new();
    for corpus in corpora {
        for (dir, n) in citation_counts(corpus, facts) {
            *combined_counts.entry(dir).or_insert(0) += n;
        }
    }
    let busiest = combined_counts.iter().max_by_key(|(_, n)| **n);

    s.push_str("## The surplus read\n\n");
    s.push_str(&wrap(
        "Enumerate `domains/*` and `windows/*`. Any subsystem that no chapter's `present` \
         verdict cites, in any corpus above, is surplus — the catalogue family has no \
         vocabulary for it. Derived from the corpora and the live directory tree on every \
         run, never authored, so it moves on its own as domains land (spec §6).",
    ));
    s.push_str("\n\n");
    match busiest {
        Some((dir, n)) => s.push_str(&wrap(&format!(
            "**Declared limitation:** subsystem granularity is coarse, and a domain cited by \
             a single chapter reads as fully covered here. `{dir}` is the most-cited \
             subsystem below, at {n} anchor(s) — it does not appear in the surplus list, and \
             reads as fully covered. It is not: {n} anchors are not {n} anchors' worth of the \
             crate's actual surface, and this instrument does not measure that surface at \
             all. This read shows the instrument's own bias in its own output; it does not \
             correct for it.",
        ))),
        None => s.push_str(&wrap(
            "**Declared limitation:** subsystem granularity is coarse, and a domain cited by \
             a single chapter would read as fully covered here — no subsystem is cited by \
             any corpus above today, so this run has no example to name, but the limitation \
             holds regardless. This read shows the instrument's own bias in its own output; \
             it does not correct for it.",
        )),
    }
    s.push_str("\n\n");

    if surplus_across.is_empty() {
        s.push_str("Every subsystem is cited by at least one corpus's `present` verdict.\n");
    } else {
        for dir in &surplus_across {
            s.push_str(&format!("- `{dir}`\n"));
        }
    }
    s
}
