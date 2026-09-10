//! Every world build in this workspace is a named site on a roster
//! (decision 0606).
//!
//! A full world build costs ~3.0 s in a debug build, 80.8% of it in the
//! `climate+settlements` stage. Reading the committed seed-42 world instead
//! costs ~15 ms — a ~200x difference. Measured 2026-09-02; see
//! `docs/superpowers/specs/2026-09-02-the-reservoir-design.md` §1.
//!
//! nextest is process-per-test, so a per-process memo recovers nothing: of
//! 100 world-building processes observed in one run, 93 built exactly one
//! world. Decision 0032 recorded the same conclusion for the census.
//!
//! So this is a **ratchet, not a wall** — the shape `tropes check`, the
//! timings baseline, type-audit's `waiver(...)` and seam-guard all use. A
//! guard that failed on the mere existence of 350 grandfathered sites would
//! be red on day one and trained away by day two.
//!
//! **Reason codes** (spec §3.2). `build-path` asserts on the build itself
//! (byte-identity, stream consumption order, pin isolation). `artifacts`
//! needs `GeneratedTerrain`/`GeneratedClimate`, which are `Clone` but not
//! `Serialize`, so no fixture can supply them — **declared and currently
//! unused**, because the artifact-needing callers were migrated by
//! re-deriving both from the *loaded* world, so nothing reaches for a build
//! on those grounds today. `identity` needs a world identity with no
//! committed fixture: a seed, a pin set, a build depth, or a component set
//! (only seed 42 at default pins, generated sky, full depth and the shipped
//! roster has a fixture). `production` is a real build on a real code path.
//! `unmigrated` is grandfathered — **the only reason that may not grow**, and
//! `UNMIGRATED_CEILING` is what stops it.
//!
//! **Direction, and imprecision, stated rather than implied.** This asserts
//! *live ⊆ roster* and *roster ⊆ live*. It does NOT assert a row's reason is
//! the right reason — only a human reading the site can say that, which is
//! why every row starts `unmigrated`. And it is a TEXTUAL scan, so an entry
//! point named in a comment or a string counts as a site;
//! `build_path_embedding.rs` accepts the same tradeoff, a false positive
//! being worth taking over a missed real build. If a comment ever forces a
//! spurious row, reword the comment rather than start parsing Rust.
//!
//! **To add a build site deliberately:** add or bump its row in
//! `cli/tests/fixtures/world-build-sites.tsv` in the same commit, with a
//! reason that is not `unmigrated`, and say in the commit message why the
//! fixture could not serve it. **To migrate one:** lower the row's
//! `unmigrated` tally and lower `UNMIGRATED_CEILING` by the same amount — in
//! the same commit, because the tally is asserted by EQUALITY, not as an
//! upper bound (see [`UNMIGRATED_CEILING`]).
//!
//! **And sweep a migrated helper's callers for double-call comparisons.**
//! A helper that returns the fixture at seed 42 makes any caller comparing
//! *two of its calls* vacuous — it compares two reads of one file. Two tests
//! in `windows/worldgen/src/lib.rs` were made vacuous exactly this way,
//! passing in 0.06 s for what should have been four ~3.0 s builds, and both
//! now keep local builders instead. Migrating at the helper body is one edit
//! and N behaviour changes; the N is what needs reading.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The build entry points a roster row can name. Any call to one of these in
/// workspace source is a world build.
///
/// **Seven, and `simulate_world` is deliberately not the eighth.**
/// `hornvale_lab::health::simulate_world(world: &World) -> Vec<AffectTrace>`
/// takes an ALREADY-BUILT world and derives terrain and climate from it, so
/// it is a decision-0092 weir site — already governed by clippy's
/// `disallowed-methods`, and carrying 0092's scoped `#[allow]` and its
/// "Named construction site" comment. 0092 governs derivation *from* a
/// world; this roster governs construction *of* one. Adding it here would
/// add 5 spurious rows across 3 files and blur two mechanisms that are
/// separate on purpose. Do not "complete" this list with it.
///
/// **`build_world_from_components` was the missing sixth, and the way it went
/// missing is the lesson.** It is the full build `build_world` wraps
/// (`windows/worldgen/src/lib.rs`). A needle is a name plus an open paren,
/// and `build_world` is not a prefix of the longer name *at the paren*, so
/// `build_world`'s needle could never match a call to it: a list that
/// *looked* audited, having adjudicated `simulate_world` explicitly, silently
/// omitted 9 live sites across 5 files, one of which (`repose_exposure.rs`)
/// carried a full-depth build helper and no roster row at all. Adjudicating
/// one candidate is not the same as enumerating the entry points; the second
/// requires reading the composition root's `pub fn`s, which is what found
/// this. Added 2026-09-02 by The Reservoir's final whole-branch review.
///
/// (Both names are spelled here without a following paren, deliberately.
/// Writing either as an adjacent `<name>(` literal would make this very
/// comment a build site by the scan's own textual rule — which is exactly
/// what happened while this paragraph was being drafted, and the module doc's
/// stated remedy is to reword the comment rather than start parsing Rust.)
///
/// **Scope, stated so it is not read wider than it is.** Only `src/` and
/// `tests/` are scanned (see [`scanned_dirs`]); `examples/` is not, so build
/// sites in the four `examples/` trees that carry them are invisible to this
/// roster. Examples compile under `--all-targets` but never run, so they
/// cost no test time — the reason the exclusion is tolerable, and the reason
/// it is stated here rather than left to be inferred, exactly as the
/// textual-scan imprecision above is stated.
const ENTRY_POINTS: &[&str] = &[
    "build_world_from_components",
    "build_world_with_exchange_treatment",
    "build_world_to_with_artifacts",
    "build_world_observed",
    "build_world_to",
    "build_world",
    "history_for",
];

/// The number of `unmigrated` sites the roster carries — asserted by
/// EQUALITY, not as an upper bound. Lower it as migrations land; never raise
/// it.
///
/// **Equality closes a laundering path an inequality left open.** Under
/// `total <= CEILING`, reclassifying N sites away from `unmigrated` lowers
/// `total` and leaves the constant untouched, silently minting N points of
/// headroom that a later commit could spend on new grandfathered debt without
/// any human deciding to. Equality makes the documented workflow ("lower the
/// row's tally and lower this constant by the same amount") mandatory instead
/// of aspirational: a migration and its ceiling drop land in one commit, or
/// the guard reddens.
const UNMIGRATED_CEILING: usize = 325;

/// The workspace root — the parent of `cli/`, where this test crate lives.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Recursively collect `.rs` files under `dir`, sorted.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Every workspace crate's `src/` and `tests/` tree. Discovered, not listed,
/// so a new domain or window is covered the day it is added.
///
/// **`src/` is scanned as well as `tests/`, deliberately.** The single largest
/// helper in the repository, `seam_world` with 84 callers, lives in a
/// `#[cfg(test)] mod tests` block inside `windows/vessel/src/session.rs`. A
/// tests-only scan would miss it, and locating a test module's textual span
/// would be fragile. `production` is a reason code precisely so real build
/// paths in `src/` have somewhere honest to sit.
fn scanned_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let mut crates = vec![root.join("kernel"), root.join("cli")];
    for layer in ["domains", "windows"] {
        let Ok(entries) = std::fs::read_dir(root.join(layer)) else {
            continue;
        };
        let mut kids: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
        kids.sort();
        crates.extend(kids.into_iter().filter(|p| p.is_dir()));
    }
    for c in crates {
        for sub in ["src", "tests"] {
            let d = c.join(sub);
            if d.is_dir() {
                dirs.push(d);
            }
        }
    }
    dirs
}

/// Count build-entry-point calls in `text`, longest name first so that
/// `build_world_to` is not also counted as `build_world`.
fn count_sites(text: &str) -> usize {
    let mut remaining = text.to_string();
    let mut total = 0;
    for ep in ENTRY_POINTS {
        let needle = format!("{ep}(");
        total += remaining.matches(&needle).count();
        remaining = remaining.replace(&needle, "");
    }
    total
}

/// `repo-relative path -> number of build sites`, over the scanned trees.
fn live_sites() -> BTreeMap<String, usize> {
    let root = workspace_root();
    let mut found = BTreeMap::new();
    for dir in scanned_dirs(&root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let text = std::fs::read_to_string(&file).unwrap_or_default();
            let count = count_sites(&text);
            if count > 0 {
                let rel = file
                    .strip_prefix(&root)
                    .unwrap_or(&file)
                    .to_string_lossy()
                    .into_owned();
                found.insert(rel, count);
            }
        }
    }
    found
}

/// One roster row: the total, and the per-reason tally.
struct Row {
    count: usize,
    reasons: BTreeMap<String, usize>,
}

/// The roster, as `path -> Row`.
fn roster() -> BTreeMap<String, Row> {
    include_str!("../fixtures/world-build-sites.tsv")
        .lines()
        .map(str::trim_end)
        .filter(|l| !l.trim().is_empty() && !l.starts_with('#'))
        .map(|l| {
            let mut cols = l.split('\t');
            let path = cols
                .next()
                .unwrap_or_else(|| panic!("malformed roster row: {l:?}"))
                .trim()
                .to_string();
            let count: usize = cols
                .next()
                .unwrap_or_else(|| panic!("roster row {l:?} has no count"))
                .trim()
                .parse()
                .unwrap_or_else(|e| panic!("roster row {l:?} count: {e}"));
            let mut reasons = BTreeMap::new();
            for pair in cols
                .next()
                .unwrap_or_else(|| panic!("roster row {l:?} has no reason tally"))
                .split_whitespace()
            {
                let (name, n) = pair
                    .rsplit_once(':')
                    .unwrap_or_else(|| panic!("roster row {l:?}: want `reason:N`, got {pair:?}"));
                let n: usize = n
                    .parse()
                    .unwrap_or_else(|e| panic!("roster row {l:?} reason {pair:?}: {e}"));
                *reasons.entry(name.to_string()).or_insert(0) += n;
            }
            (path, Row { count, reasons })
        })
        .collect()
}

/// Every reason code a row may name.
const REASONS: &[&str] = &[
    "build-path",
    "artifacts",
    "identity",
    "production",
    "unmigrated",
];

#[test]
fn no_unrostered_world_build_appears() {
    let live = live_sites();
    let roster = roster();

    let mut offenders = Vec::new();
    for (path, count) in &live {
        match roster.get(path) {
            None => offenders.push(format!("  {path}: {count} build(s), no roster row")),
            Some(row) if *count > row.count => offenders.push(format!(
                "  {path}: {count} build(s), roster allows {}",
                row.count
            )),
            Some(_) => {}
        }
    }

    assert!(
        offenders.is_empty(),
        "this workspace gained a world build:\n{}\n\n\
         A full build costs ~3.0 s in a debug build; reading the committed \
         seed-42 world costs ~15 ms. nextest is process-per-test, so each new \
         site pays in full, once per test, forever.\n\n\
         If the test only reads FACTS from the default seed-42 world, call \
         `hornvale_worldgen::seed_42_world()` instead and add no row. If it \
         genuinely must build, add the row to \
         cli/tests/fixtures/world-build-sites.tsv with a reason that is not \
         `unmigrated` ({REASONS:?}) and say in the commit why the fixture \
         cannot serve it.",
        offenders.join("\n")
    );
}

#[test]
fn the_roster_only_shrinks() {
    let live = live_sites();
    let roster = roster();

    let mut stale = Vec::new();
    for (path, row) in &roster {
        match live.get(path) {
            None => stale.push(format!("  {path}: row says {}, file has none", row.count)),
            Some(count) if *count < row.count => stale.push(format!(
                "  {path}: row says {}, file has {count}",
                row.count
            )),
            Some(_) => {}
        }
    }

    assert!(
        stale.is_empty(),
        "the roster over-counts — good news, builds were removed:\n{}\n\n\
         Lower or delete those rows in \
         cli/tests/fixtures/world-build-sites.tsv, and lower \
         UNMIGRATED_CEILING in this file by the number of `unmigrated` sites \
         you just retired. Checking both directions is what stops the roster \
         from rotting into a list nobody trusts.",
        stale.join("\n")
    );
}

#[test]
fn the_unmigrated_tally_never_grows() {
    let roster = roster();
    let total: usize = roster
        .values()
        .filter_map(|r| r.reasons.get("unmigrated"))
        .sum();
    assert_eq!(
        total, UNMIGRATED_CEILING,
        "the roster carries {total} `unmigrated` sites, UNMIGRATED_CEILING \
         says {UNMIGRATED_CEILING}.\n\n\
         The tally is the SUM of every row's `unmigrated:N`, not a count of \
         rows, and it is checked by equality in both directions.\n\n\
         If the tally GREW: `unmigrated` is the one reason that may not grow \
         — it means 'grandfathered, nobody has looked yet'. A genuinely \
         necessary build gets a real reason ({REASONS:?}) instead. Raising \
         this ceiling undoes the campaign that set it.\n\n\
         If the tally SHRANK: good news — lower UNMIGRATED_CEILING by the \
         same amount in this commit. Leaving it high would bank the \
         difference as headroom for future debt nobody adjudicated, which is \
         exactly what the equality exists to prevent."
    );
}

#[test]
fn every_row_is_well_formed() {
    let roster = roster();
    for (path, row) in &roster {
        let tallied: usize = row.reasons.values().sum();
        assert_eq!(
            tallied, row.count,
            "roster row {path}: reasons sum to {tallied}, count says {}",
            row.count
        );
        for name in row.reasons.keys() {
            assert!(
                REASONS.contains(&name.as_str()),
                "roster row {path}: unknown reason {name:?}; want one of {REASONS:?}"
            );
        }
    }
}

#[test]
fn the_scan_actually_resolves_the_workspace() {
    // Anti-vacuity: every assertion above passes trivially if the scan finds
    // nothing, so pin that it looks in real directories and sees known sites.
    let root = workspace_root();
    let dirs = scanned_dirs(&root);
    assert!(
        dirs.iter().all(|d| d.is_dir()),
        "every scanned dir must exist: {dirs:?}"
    );
    assert!(
        dirs.len() >= 15,
        "expected src+tests for the kernel, cli, and every domain/window; got {}",
        dirs.len()
    );

    let live = live_sites();
    assert!(
        live.contains_key("windows/vessel/src/session.rs"),
        "the scan must see seam_world's build in vessel's session.rs; got {} entries",
        live.len()
    );
    assert!(
        live.contains_key("windows/scene/src/surrounds.rs"),
        "the scan must see the surrounds test helper's build; got {} entries",
        live.len()
    );

    // `build_world_to` must not double-count as `build_world`. Assembled
    // from fragments at runtime, rather than spelled as an adjacent
    // `<name>(` literal, so this test's own source is not itself mistaken
    // for a build site by the very scan it exercises -- the module doc's
    // own stated remedy for a comment or string forcing a spurious row
    // ("reword the comment, not to start parsing Rust") applies just as
    // much to this file's source as to any other.
    let two_calls = format!("{}(a); {}(b);", "build_world_to", "build_world");
    assert_eq!(
        count_sites(&two_calls),
        2,
        "longest-first counting must not count build_world_to twice"
    );
    assert_eq!(count_sites("// nothing here"), 0);

    // The sixth entry point, pinned two ways: that the scan sees a file whose
    // ONLY build site is a `build_world_from_components` call (so dropping it
    // from ENTRY_POINTS makes this file vanish and reddens the roster rather
    // than passing quietly), and that it counts once rather than also as a
    // `build_world`. Names assembled at runtime for the same reason as above.
    assert!(
        live.contains_key("windows/worldgen/tests/suite/repose_exposure.rs"),
        "the scan must see repose_exposure's build_world_from_components \
         helper — its only build site; got {} entries",
        live.len()
    );
    let from_components = format!("{}(a);", "build_world_from_components");
    assert_eq!(
        count_sites(&from_components),
        1,
        "build_world_from_components counts once, never also as build_world"
    );
}
