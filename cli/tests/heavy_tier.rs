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
//!
//! ## What the serialization-pin guard does NOT do (The Scatter, 2026-08-05)
//!
//! The second guard in this file — the one holding
//! `.config/nextest.toml`'s `threads-required` roster to the set of heavy
//! batteries that scatter their own seed sweeps — recognises a battery as
//! internally parallel by the literal `seed_sweep::map_seeds(` call.
//!
//! **So a battery that hand-rolls its own `std::thread::scope` is invisible to
//! it**, and would go unpinned and unnoticed. The guard's non-emptiness assert
//! does not close this: it catches the HELPER being renamed out from under the
//! guard, which is a different failure. The residual is accepted rather than
//! chased — the helper exists precisely so that batteries do not hand-roll,
//! `TOOL-seed-sweep-reach` is an open row to widen its reach, and an unguarded
//! hand-rolled sweep is the status quo the guard inherited rather than
//! anything it introduces. Worth knowing before adding the fourth battery.

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
/// `hornvale_worldgen::exposure_from`'s Steeped rules — duplicated on purpose,
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
const EXPECTED_UNTOKENISED: [&str; 20] = [
    "...",
    "PREREGISTERED, not met: awaits BIO-supply-drowns-niche (supply magnitude drowns the condition niche)",
    "TODO: re-enable once the number settles",
    "calibration: run by hand, prints the approach_ease quantiles",
    "compiles the workspace in release; CI runs it with -- --ignored",
    "failing — investigate later",
    "flaky after the refactor",
    "measurement: builds eight full worlds; run explicitly with --ignored",
    "measurement: builds one full world; run explicitly with --ignored",
    "measurement: builds one world to BuildDepth::Terrain; run explicitly with --ignored",
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

// ============================================================================
// The serialized-battery filter (The Scatter). `.config/nextest.toml` pins
// three heavy batteries to `threads-required = "num-cpus"` so each runs ALONE
// on the canonical box: since The Scatter they parallelise their own 200-seed
// sweeps across every core, and `gate-full-heavy.sh` sets no `test-threads`
// limit, so without the pin the box runs up to 40 heavy processes each
// wanting 40 worker threads.
//
// That filter is a HAND-MAINTAINED LIST OF THREE NAMES, and its failure mode
// is silent and expensive. Rename a battery, or add a fourth that sweeps
// seeds, and the filter simply matches fewer tests: nothing reddens, the box
// is oversubscribed again, and the FIRST SYMPTOM is a spurious
// `hornvale::scene_cost` failure that reads like a performance regression —
// which is exactly the 341 s of wall clock the pin was bought with.
//
// The precedent is two files away: `scripts/gate-full-heavy.sh` already
// asserts `tag_count == name_count` so a heavy tag that drifts off its `fn`
// cannot silently vanish from `make gate-full`. This is the same guard for
// the same class of drift, one directory over.
// ============================================================================

/// Where the serialized-battery pin lives.
const NEXTEST_CONFIG: &str = ".config/nextest.toml";

/// The marker that makes an override a serialization pin.
const THREADS_REQUIRED: &str = "threads-required = \"num-cpus\"";

/// The call that makes a battery internally parallel — the property the pin
/// exists for. Matching the CALL (not the module) is deliberate: a test that
/// merely mentions the helper in prose is not the thing that saturates a box.
const SWEEP_CALL: &str = "seed_sweep::map_seeds(";

/// The test names `.config/nextest.toml`'s serialization override selects,
/// parsed out of its `test(/<name>$/)` filterset. Std-only string scanning —
/// this workspace admits no TOML parser (decision 0004).
fn serialized_filter_names() -> Vec<String> {
    let text = fs::read_to_string(repo_root().join(NEXTEST_CONFIG))
        .expect(".config/nextest.toml is readable");

    // SETTINGS ONLY, never comments. Found by mutation-testing this guard:
    // deleting the real `threads-required` line left the check GREEN, because
    // the section comment above the override quotes the setting verbatim while
    // explaining it. A guard that a comment can satisfy is not a guard.
    let settings: Vec<&str> = text
        .lines()
        .map(str::trim)
        .filter(|l| !l.starts_with('#') && !l.is_empty())
        .collect();
    assert!(
        settings.contains(&THREADS_REQUIRED),
        "{NEXTEST_CONFIG} has no live {THREADS_REQUIRED:?} SETTING (a comment \
         mentioning it does not count). The serialization pin for the \
         internally-parallel heavy batteries is GONE, which silently \
         re-oversubscribes the canonical box — see this file's section comment."
    );
    let filter_lines: Vec<&str> = settings
        .iter()
        .copied()
        .filter(|l| l.starts_with("filter = ") && l.contains("test(/"))
        .collect();
    assert_eq!(
        filter_lines.len(),
        1,
        "expected exactly one `filter = ` line naming tests in {NEXTEST_CONFIG}; \
         found {}. This guard reads a single override block; teach it about the \
         others before adding one.",
        filter_lines.len()
    );

    let mut names = Vec::new();
    let mut rest = filter_lines[0];
    while let Some((_, after)) = rest.split_once("test(/") {
        let (name, tail) = after
            .split_once("$/)")
            .expect("a test(/…/) term in the filterset is end-anchored with `$/)`");
        names.push(name.to_string());
        rest = tail;
    }
    names.sort();
    names
}

/// Every heavy-tagged test whose body calls [`SWEEP_CALL`] — i.e. every heavy
/// battery that parallelises its own seed sweep and therefore MUST be pinned.
///
/// Line-oriented, matching `gate-full-heavy.sh`'s own grep-based discovery, so
/// the two agree about what a heavy test is. A heavy `#[ignore]` tag sits
/// directly above its `fn`; a test's region runs from that `fn` to the next
/// `#[test]` attribute or end of file.
fn internally_parallel_heavy_tests() -> Vec<String> {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    sources.sort();

    let mut found = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        let mut next_fn_is_heavy = false;
        let mut current: Option<String> = None;
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("#[test]") {
                current = None;
            }
            if trimmed.starts_with("#[ignore = \"") {
                next_fn_is_heavy = trimmed.contains("heavy:");
                current = None;
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("fn ")
                && let Some((name, _)) = rest.split_once('(')
            {
                current = next_fn_is_heavy.then(|| name.to_string());
                next_fn_is_heavy = false;
                continue;
            }
            if line.contains(SWEEP_CALL)
                && let Some(name) = &current
                && !found.contains(name)
            {
                found.push(name.clone());
            }
        }
    }
    found.sort();
    found
}

/// The pin's roster is exactly the set of heavy batteries that actually
/// scatter their own seed sweeps — checked in BOTH directions, because both
/// failure modes are silent.
///
/// A renamed battery drops out of the filter; a newly-added sweeping battery
/// never enters it. Either way nextest resumes scheduling forty heavy
/// processes against a box whose batteries each want forty worker threads,
/// and the first thing anyone sees is a wall-clock budget test going red for
/// reasons that have nothing to do with the code it measures.
#[test]
fn the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps() {
    let pinned = serialized_filter_names();
    let parallel = internally_parallel_heavy_tests();

    assert!(
        !parallel.is_empty(),
        "found no heavy test calling {SWEEP_CALL:?}. Either the sweep helper was \
         renamed (update SWEEP_CALL) or this guard is now asserting nothing — \
         which is the one outcome it must never quietly reach."
    );
    assert_eq!(
        pinned, parallel,
        "\n{NEXTEST_CONFIG}'s serialization filter and the set of heavy batteries \
         that scatter their own seed sweeps have diverged.\n  pinned in config: \
         {pinned:?}\n  actually parallel: {parallel:?}\nAdd the missing name(s) to \
         the `filter = ` line, or drop the stale one. Left alone this does NOT \
         redden on its own: nextest schedules the unpinned battery alongside \
         everything else, the canonical box is oversubscribed, and the symptom \
         is a spurious hornvale::scene_cost failure that looks like a real \
         performance regression."
    );
}
