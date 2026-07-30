//! The ignore-reason conventions. An `#[ignore]` is a promise deferred, and a
//! promise nobody can find again is a promise broken — so every class of
//! deferral here carries a TOKEN in its reason string and is held to that
//! string verbatim by a test, which is what keeps the class greppable rather
//! than tribal.
//!
//! Two classes so far:
//!
//! - `heavy:` (fast-gate-tiers spec) — a live-worldgen battery deferred from
//!   `make gate` to `make gate-full`, so the two stay in sync.
//! - `stale-second-opinion:` (F11 discharge) — a row whose claim is blocked
//!   because a metric's deliberately-duplicated second opinion has fallen out
//!   of step with the code it duplicates, so the row reads a defect in the
//!   instrument rather than in the worlds.
//!
//! ## What a token guard does NOT do (F11 discharge, 2026-07-30)
//!
//! Worth writing here, at the guard, because this is where the next person to
//! add a deferral class will read it.
//!
//! The Wearing left 38 rows ignored under a `stale-census:` token. The census
//! staleness those markers described was fixed some days later by a different
//! campaign's regen; the markers stayed put, reading as current fact, until
//! somebody went looking. At the discharge, 23 of them needed nothing but
//! deletion — their claims and their pinned values had both survived
//! untouched.
//!
//! The guard below was not broken and did not fail. It did exactly what it
//! promises: it kept the reason strings canonical, so one grep found the whole
//! debt. But **canonical is not current.** A token guard can prove that a
//! deferral is findable; it cannot prove that the deferral is still true,
//! because the condition being deferred on lives outside the string. An
//! ignore-token debt marker does not know when its debt is discharged by
//! someone else.
//!
//! There is no cheap assertion that closes that gap — "is this reason still
//! true" is exactly as hard as running the deferred work. What can be done is
//! to keep the classes SMALL and the reason strings SPECIFIC enough that a
//! reader can check them by hand, and to treat a long-lived token as a
//! question rather than a fact. A deferral that has outlived two campaigns is
//! more likely to be spent than to be waiting.

use std::fs;
use std::path::{Path, PathBuf};

/// The one reason string every heavy-tier test must use verbatim.
const CANONICAL: &str =
    "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full";

/// The workspace root: the parent of this crate's manifest dir (`cli/`).
/// Filesystem-based, not git-based — the remote gate runs the suite in an
/// rsync'd tree that is not a git repository.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Recursively collect every `.rs` file under `dir`, skipping `target/` and
/// dot-directories (the same source set `git grep -- '*.rs'` covered).
fn collect_rs(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("directory is readable") {
        let entry = entry.expect("directory entry is readable");
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().into_owned();
        if path.is_dir() {
            if name == "target" || name.starts_with('.') {
                continue;
            }
            collect_rs(&path, out);
        } else if name.ends_with(".rs") {
            out.push(path);
        }
    }
}

/// All `#[ignore = "..."]` reason strings in the repo's Rust sources
/// (std-only filesystem scan; reason strings never contain quotes).
fn ignore_reasons() -> Vec<String> {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    sources.sort();
    let mut reasons = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        for line in text.lines() {
            if let Some((_, rest)) = line.split_once("#[ignore = \"")
                && let Some((reason, _)) = rest.split_once("\"]")
            {
                reasons.push(reason.to_string());
            }
        }
    }
    reasons
}

#[test]
fn heavy_tier_reason_strings_are_canonical() {
    let reasons = ignore_reasons();
    let heavy: Vec<&String> = reasons.iter().filter(|r| r.contains("heavy:")).collect();
    assert!(
        !heavy.is_empty(),
        "expected at least one heavy-tier #[ignore] test; found none"
    );
    for r in &heavy {
        assert_eq!(
            *r, CANONICAL,
            "heavy-tier ignore reason must be verbatim canonical; found: {r:?}"
        );
    }
}

/// The canonical `stale-second-opinion:` reason strings (F11 discharge,
/// 2026-07-30). Successor to `STALE_CENSUS`, which guarded The Wearing's 38
/// deferred rows and was retired when the last one was re-derived.
///
/// Retired rather than kept: a guard whose population is empty asserts
/// nothing, and the non-emptiness check below would have turned it into a
/// permanently red test demanding its own deletion. Keeping an
/// always-failing guard around to describe work that is finished is its own
/// small lie about the state of the tree, so the class was replaced by the
/// one class that genuinely remains.
///
/// That class is narrow and specific. Two rows are blocked on ONE diagnosed
/// defect: `windows/lab/src/metrics.rs::independently_steeped_concepts` is a
/// deliberate hand-maintained duplicate of
/// `hornvale_worldgen::exposure_of`'s Steeped rules — duplicated on purpose,
/// since a check that called the code under test would assert nothing — and it
/// has not learned The Watershed's staple rules. So `exposure-sound-*` reads
/// false on every world where a people is placed. The census is CURRENT; the
/// instrument is not, which is why this needed a new token rather than
/// reusing the old one. Repairing the duplicate changes two census columns and
/// therefore owes a full regeneration, which is a campaign and not a followup.
///
/// The two blocked rows are
/// `windows/lab/tests/calibration.rs::lexicon_is_exposure_sound_for_both_species`
/// and
/// `windows/lab/src/metrics.rs::exposure_sound_reports_false_when_the_toponymic_gates_are_removed`.
/// Each carries its own full diagnosis at its own doc comment; the strings
/// here only have to be greppable and verbatim.
/// The untokenised `#[ignore]` reasons currently in the tree, pinned as a
/// roster by [`the_untokenised_ignore_reasons_are_exactly_this_roster`].
///
/// The first entry is not a real test. `ignore_reasons()` scans source text,
/// so it matches the literal `#[ignore = "..."]` written inside its own doc
/// comment above. Pinned as-is rather than special-cased: teaching the scanner
/// to skip doc comments would make it disagree with what `git grep` sees,
/// which is the one property the whole convention rests on.
const EXPECTED_UNTOKENISED: [&str; 18] = [
    "...",
    "PREREGISTERED, not met: awaits BIO-supply-drowns-niche (supply magnitude drowns the condition niche)",
    "TODO: re-enable once the number settles",
    "compiles the workspace in release; CI runs it with -- --ignored",
    "failing — investigate later",
    "flaky after the refactor",
    "measurement: builds eight full worlds; run explicitly with --ignored",
    "measurement: builds one full world; run explicitly with --ignored",
    "probe: Stage-0 rift instrument, run by hand (spec §6)",
    "probe: measurement only, run explicitly",
    "readout: chronicle evidence, run manually with --nocapture",
    "regenerates the committed occupancy fixture; run by hand - the drift check above is the gate",
    "runs the full gathering census; the fixture is drift-checked in CI",
    "runs the full live census sweep; the fixture is drift-checked in CI",
    "runs the full ~450s (debug) census; fixtures are drift-checked in CI",
    "search: re-derives the wear fixture's seed; run explicitly with --ignored",
    "superseded by decision 0016; kept for one release",
    "timekeeper: reads the run.json `make ci` writes; not a standalone test",
];

const STALE_SECOND_OPINION: [&str; 2] = [
    "stale-second-opinion: the lab's independently_steeped_concepts duplicate has not \
     learned The Watershed's staple Steeped rules, so exposure-sound reads false on every \
     world where a species is placed. The census is current; the metric is not. Repair owes \
     a regen — see the doc comment",
    "stale-second-opinion: the lab's independently_steeped_concepts duplicate has not \
     learned The Watershed's staple Steeped rules, so this mutation test's Flag(true) \
     baseline is false and the mutation would prove nothing. Repair owes a regen — see the \
     doc comment",
];

#[test]
fn stale_second_opinion_reason_strings_are_canonical() {
    let reasons = ignore_reasons();
    let stale: Vec<&String> = reasons
        .iter()
        .filter(|r| r.contains("stale-second-opinion:"))
        .collect();
    assert!(
        !stale.is_empty(),
        "expected at least one stale-second-opinion #[ignore] test; found none. If \
         `independently_steeped_concepts` has been taught The Watershed's staple rules, \
         the census regenerated and both rows re-derived, delete this test and its \
         constant along with the last ignore — do not leave a guard standing over an \
         empty set."
    );
    for r in &stale {
        assert!(
            STALE_SECOND_OPINION.contains(&r.as_str()),
            "stale-second-opinion ignore reason must be one of the two canonical \
             strings verbatim; found: {r:?}"
        );
    }
}

/// The other direction the two token guards do not cover, added at the F11
/// discharge (2026-07-30).
///
/// Those guards check that a row CARRYING a token spells it canonically. They
/// say nothing whatever about a row that carries no token at all — and a
/// deferral with no token is precisely the case the whole convention exists to
/// prevent, since it is unfindable by construction. This pins the exact set of
/// untokenised ignore reasons in the tree, in the spirit of
/// `cli/src/streams.rs::the_stamp_is_exactly_this_roster` (decision 0073):
/// adding one becomes a review decision rather than a silent change.
///
/// The roster is deliberately NOT a taxonomy. An earlier draft of this guard
/// tried to assert that every untokenised reason "reads as a cost-based
/// one-off" by matching keywords, and that was guesswork dressed as a rule —
/// it failed immediately on a `readout:` class it did not know existed. A flat
/// roster asserts only what can actually be known here: this is the set, and
/// it changed under review.
///
/// Most members are honest cost-based one-offs — a battery too slow for the
/// commit gate, a fixture regenerator run by hand. **Three are not, and are
/// listed here without being chased, because they are outside F11's scope and
/// naming them is better than leaving them invisible:** `"TODO: ..."`,
/// `"flaky after the refactor"` and `"failing — investigate later"` are
/// deferred promises with no token, no owner and no date. They are exactly
/// what the module doc above describes. A future pass that adopts them should
/// give them a token class and delete them from this roster.
#[test]
fn the_untokenised_ignore_reasons_are_exactly_this_roster() {
    let reasons = ignore_reasons();
    let mut untokenised: Vec<String> = reasons
        .iter()
        .filter(|r| !r.contains("heavy:") && !r.contains("stale-second-opinion:"))
        .cloned()
        .collect();
    untokenised.sort();
    untokenised.dedup();
    assert_eq!(
        untokenised, EXPECTED_UNTOKENISED,
        "the set of untokenised #[ignore] reasons changed. Adding one is a review \
         decision: if it defers a PROMISE it needs a greppable token class in this \
         file, not a bare sentence. If it is a cost-based one-off, add it here."
    );
}
