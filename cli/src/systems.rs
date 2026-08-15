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
    /// Idea-registry row ID → its `status` cell.
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
            decisions_stamp,
            root: root.to_path_buf(),
        })
    }

    /// Whether `spec` (`<crate>::<fn>`) resolves to a known crate whose
    /// source defines `fn <symbol>` at a word boundary. A cheap text
    /// search, not a compile — the resolver never shells out to `cargo`.
    fn test_resolves(&self, spec: &str) -> bool {
        let Some((crate_name, symbol)) = spec.rsplit_once("::") else {
            return false;
        };
        let Some(dir) = self.crates.get(crate_name) else {
            return false;
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
/// collect every registry row's ID → `status` cell (`id | description |
/// status | confidence | where`).
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
        let status = pieces.get(3).cloned().unwrap_or_default();
        registry.insert(id.clone(), status);
    }
    registry
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

/// True when the character `c` cannot continue a Rust identifier — i.e. it
/// is a valid boundary after a matched symbol name.
fn is_identifier_boundary(c: char) -> bool {
    !(c.is_alphanumeric() || c == '_')
}

/// True when `text` defines a function named exactly `symbol` — `fn
/// {symbol}` found at a word boundary, so the character immediately after
/// `symbol` (if any) cannot continue an identifier. A plain substring
/// search on `fn {symbol}` would read a short symbol as already "defining"
/// any longer function name it happens to prefix — the wrong direction of
/// error for an instrument whose job is to notice when an anchor stops
/// resolving.
fn text_defines_symbol(text: &str, symbol: &str) -> bool {
    let needle = format!("fn {symbol}");
    let mut search_from = 0;
    while let Some(offset) = text[search_from..].find(needle.as_str()) {
        let match_start = search_from + offset;
        let after = match_start + needle.len();
        let boundary_ok = text[after..]
            .chars()
            .next()
            .map(is_identifier_boundary)
            .unwrap_or(true);
        if boundary_ok {
            return true;
        }
        search_from = match_start + 1;
    }
    false
}

/// True when any `.rs` file under `dir` (recursively, skipping `target`)
/// defines `fn <symbol>` at a word boundary.
fn directory_defines_symbol(dir: &Path, symbol: &str) -> bool {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return false;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().and_then(|n| n.to_str()) == Some("target") {
                continue;
            }
            if directory_defines_symbol(&path, symbol) {
                return true;
            }
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs")
            && let Ok(text) = std::fs::read_to_string(&path)
            && text_defines_symbol(&text, symbol)
        {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod boundary_tests {
    use super::text_defines_symbol;

    /// The bug this function exists to fix: `verdict` must NOT read as
    /// defining `verdict_name`.
    #[test]
    fn a_strict_prefix_does_not_count_as_a_definition() {
        assert!(!text_defines_symbol(
            "fn verdict_name(v: Verdict) {}",
            "verdict"
        ));
    }

    #[test]
    fn an_exact_match_counts() {
        assert!(text_defines_symbol(
            "fn verdict_name(v: Verdict) {}",
            "verdict_name"
        ));
    }

    #[test]
    fn a_match_at_end_of_file_counts() {
        assert!(text_defines_symbol("pub fn resolve", "resolve"));
    }

    #[test]
    fn a_match_followed_by_punctuation_counts() {
        assert!(text_defines_symbol(
            "pub fn resolve<T>(x: T) -> T { x }",
            "resolve"
        ));
        assert!(text_defines_symbol(
            "pub fn resolve(x: T) -> T { x }",
            "resolve"
        ));
        assert!(text_defines_symbol(
            "pub fn resolve\n(x: T) -> T { x }",
            "resolve"
        ));
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
        Anchor::Test(t) => {
            if facts.test_resolves(t) {
                None
            } else {
                Some(Finding::Dangling {
                    id: item.id.clone(),
                    anchor: anchor_str.to_string(),
                    why: format!(
                        "{} cites test:{t}, which does not resolve to a known crate and a\n\
                         `fn` definition of that name at a word boundary. Either the function\n\
                         moved or was renamed (fix the anchor) or it was removed (re-verdict\n\
                         this item).",
                        item.id
                    ),
                })
            }
        }
        Anchor::Reason(_) => None,
    }
}
