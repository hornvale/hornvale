//! The ignore-reason conventions. An `#[ignore]` is a promise deferred, and a
//! promise nobody can find again is a promise broken — so every class of
//! deferral here carries a TOKEN in its reason string and is held to that
//! string verbatim by a test, which is what keeps the class greppable rather
//! than tribal.
//!
//! Two classes so far:
//!
//! - `heavy:` (fast-gate-tiers spec) — a live-worldgen battery deferred from
//!   `gate-commit` to `make gate-campaign`, so the two stay in sync.
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
//!
//! ## A second serialization class (The Ballast, 2026-08-15)
//!
//! The scatter-sweep guard above protects the tier from three batteries that
//! saturate the box. `.config/nextest.toml` now carries a SECOND
//! `threads-required` override, protecting two wall-clock BUDGET tests FROM
//! that saturation instead: `session_cost.rs::a_possessed_turn_stays_within_
//! its_ceilings` reddened under tier contention on the canonical box even
//! though the code was unchanged, because a wall-clock ceiling cannot
//! distinguish a slow machine from slow code. `scene_cost.rs`'s heavy test is
//! pinned alongside it — this file's own comment two classes up already names
//! it as the historical symptom of exactly this failure mode.
//!
//! This class is recognised the same way the scatter-sweep one is: by a
//! literal marker (`CO_SCHEDULE_SENSITIVE_MARKER`) rather than by guessing
//! which tests measure wall clock, so a test opts in explicitly and the same
//! two-directional guard applies. See `pinned_filter_names_for_class` and
//! `co_schedule_sensitive_heavy_tests` below.
//!
//! ## A third class, sized this time (The Governor, Task 8)
//!
//! The scatter-sweep guard above is a strict two-way match against every
//! heavy/probe test calling [`SWEEP_CALL`], with no notion of HOW BIG a
//! sweep is. Task 9 gives several hearsay/worldgen tests their own
//! `map_seeds` call, but on small, bounded panels — parallelising a sweep
//! that used to run its seeds one at a time, not scattering two hundred of
//! them across every core. The moment one of those calls lands, this
//! guard's strict equality fails, and the NAIVE fix — adding the newly
//! parallel test to `.config/nextest.toml`'s `"num-cpus"` table — turns the
//! guard green while creating one full-drain-then-cold-restart barrier PER
//! CONVERTED TEST (see that table's comment for the measured 484-485 s
//! cost of just one). So a `map_seeds` caller marked [`SIZED_SWEEP_MARKER`]
//! is excluded from the scatter-sweep roster and moved to a THIRD,
//! independent class instead — `.config/nextest.toml`'s `# class:
//! sized-sweep` table, `threads-required = <a bounded integer>` rather than
//! `"num-cpus"` — checked by its own two-directional guard,
//! `the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel`.
//!
//! **This class started empty, and its guard says why that was acceptable
//! here and would not have been for the two above** — see that guard's own
//! doc comment rather than duplicating the reasoning in two places. Task 9
//! then filled it: the table pins three names today, and the guard carries
//! the same non-emptiness assert as the other two.
//!
//! **The detector-fragility residual named two sections up applied to THIS
//! class too, and Task 9 resolved it by moving the helper rather than by
//! accepting the risk.** `SWEEP_CALL` is matched as literal call text
//! (`seed_sweep::map_seeds(`), and the helper it names used to live in
//! `windows/lab/tests/seed_sweep/mod.rs` — a test-only module `windows/
//! hearsay` and `windows/worldgen` tests could not reach (spec §8.1). Task 9
//! moved it to `hornvale_worldgen::seed_sweep` (see that module's own doc
//! comment for the reachability argument) — **and every caller, old and
//! new, still imports it under the short name `seed_sweep` and calls
//! `seed_sweep::map_seeds(...)`, so the literal text `SWEEP_CALL` matches is
//! UNCHANGED by the move.** Nothing here needed updating as a result, which
//! is itself worth recording: the risk this paragraph used to warn about
//! (a silent detector blind spot from a respelled call) was avoided by
//! construction, not discovered and patched. A FUTURE move that changes the
//! import spelling (`use hornvale_worldgen::seed_sweep as sweep;`, say)
//! would still need `SWEEP_CALL` updated in the same commit, or BOTH
//! `internally_parallel_heavy_tests` and `sized_sweep_heavy_tests` go blind
//! to every caller using the new spelling — silently for a sized-sweep
//! conversion (that class tolerates zero matches by design, so a
//! mis-spelled call just never shows up), loudly for the three existing
//! scatter-sweep batteries (their names stay pinned in
//! `.config/nextest.toml` while detection drops to zero, which is exactly
//! the mismatch
//! [`the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps`]'s
//! non-emptiness assert exists to catch).

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// The one reason string every heavy-tier test must use verbatim.
const CANONICAL: &str =
    "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)";

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

#[test]
fn the_canonical_heavy_reason_states_no_duration() {
    // A duration baked into a ratchet freezes a measurement. This campaign
    // measured the claim the string used to carry ("minutes") at 4.31 s.
    assert!(
        !CANONICAL.contains("minute")
            && !CANONICAL.contains("second")
            && !CANONICAL.contains("hour"),
        "the canonical reason must not assert a duration: {CANONICAL}"
    );
}

/// The frozen `heavy:` roster (The Governor, Task 7; spec §4 — "the `heavy:`
/// tag is unpriced"). Each entry is `<repo-relative source path>::<fn
/// name>` — see [`heavy_tagged_tests`] for why the path is part of the key.
const FROZEN_HEAVY_ROSTER: &str = include_str!("../fixtures/heavy-roster.txt");

/// Every `#[ignore = "..."]` test whose reason contains `heavy:`, keyed as
/// `<repo-relative source path>::<fn name>` rather than the bare function
/// name.
///
/// **Why the path is part of the key, not an afterthought.** A bare-name
/// roster (`sort -u` over function names alone) is structurally blind to two
/// tests in different files sharing a name — and this repo already has that
/// hazard live: `windows/worldgen/tests/suite/deep_realm_substrate.rs` and
/// `windows/worldgen/tests/suite/hollow_readout.rs` both define
/// `report_cave_substrate` (the former's own doc comment calls out the
/// coincidence). Neither is `heavy:`-tagged today, but a bare-name roster
/// would silently collapse a future `heavy:` tag on one of them into an
/// entry already satisfied by the other — hiding that a SECOND heavy
/// battery had been added under cover of a name already on the list. Keying
/// on the source path — necessarily unique, since two files cannot share a
/// path — closes that hole without needing to replicate nextest's own
/// `binary-id::module::fn` naming, which would require re-deriving
/// crate/binary boundaries and any enclosing `mod tests { ... }` nesting
/// from source text alone; the source path already carries strictly more
/// disambiguating power than that scheme needs.
///
/// Scans every `.rs` file in the repo (mirroring [`ignore_reasons`] and
/// [`internally_parallel_heavy_tests`]), matching only an `#[ignore = "..."]`
/// line whose `fn` follows on the VERY NEXT line — the same single-line,
/// no-intervening-attribute shape [`internally_parallel_heavy_tests`]
/// already assumes for this tag. Verified by hand against every one of the
/// `heavy:` sites in the tree while writing the fixture this checks against
/// (63 at that moment; 64 since the campaign's final review restored
/// `occupancy_readout_is_current`): all are single-line reasons with no
/// attribute between `#[ignore = "..."]` and their `fn`.
fn heavy_tagged_tests() -> Vec<String> {
    let root = repo_root();
    let mut sources = Vec::new();
    collect_rs(&root, &mut sources);
    sources.sort();

    let mut found = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        let lines: Vec<&str> = text.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if !(trimmed.starts_with("#[ignore = \"") && trimmed.contains("heavy:")) {
                continue;
            }
            let Some(next_trimmed) = lines.get(i + 1).map(|l| l.trim()) else {
                continue;
            };
            let Some(rest) = next_trimmed.strip_prefix("fn ") else {
                continue;
            };
            let Some((name, _)) = rest.split_once('(') else {
                continue;
            };
            let rel = path
                .strip_prefix(&root)
                .expect("scanned path is under the repo root")
                .to_string_lossy()
                .replace('\\', "/");
            found.push(format!("{rel}::{name}"));
        }
    }
    found.sort();
    found
}

/// The `heavy:` roster is exactly [`FROZEN_HEAVY_ROSTER`] — checked in BOTH
/// directions, with its own anti-vacuity assertion, the same shape as
/// [`the_untokenised_ignore_reasons_are_exactly_this_roster`] below and
/// `test_binary_ratchet.rs::no_new_top_level_test_binary_appears`.
///
/// **THIS GUARD CATCHES THE ROSTER GROWING OR SHRINKING; IT DOES NOT PRICE
/// ANY SINGLE ENTRY'S DURATION.** Nothing charged a `heavy:` tag before this
/// (spec §4) — this closes exactly that gap and no more: adding one requires
/// editing this committed fixture in the same commit, a deliberate,
/// reviewable, visible diff, rather than a tag nobody sees. To add a test
/// deliberately, append its `<path>::<fn>` line to
/// `cli/tests/fixtures/heavy-roster.txt` in the same commit and say why in
/// the message. To remove one — demoting it to `probe:` or deleting it —
/// delete its line; this direction is checked too, so the roster cannot rot
/// into a permission slip nobody re-reads.
///
/// **WHAT AN ADDITION NOW COSTS CHANGED UNDER DECISION 0426.** When this
/// ratchet was written the tier ran only when a human typed `make
/// heavy-remote`, so a new tag could only ever be made *visible* — a test no
/// gate ran cost nobody anything. 0426 (The Governor, 2026-08-28) put `heavy`
/// back on `scripts/sluice-run.sh`'s **merge** phase list, so a line appended
/// here is charged to **every subsequent merge** on the one strictly serial
/// box, forever. That is the coupling spec §4 wanted and could not have while
/// the tier ran by hand. (Not the stage gate: 0426 leaves `stage_phases`
/// alone, because heavy's live-vs-committed census-fixture check would red
/// predictably for the whole middle of any world-touching campaign.)
///
/// It still prices **membership**, not duration — §4 chose a frozen roster
/// over a wall-clock budget on purpose, because a committed baseline is a
/// claim with a date and roster membership does not decay. 0426 records that
/// as a named residual: the ~465.8 s figure that makes the phase affordable is
/// exactly the quantity nothing ratchets.
#[test]
fn the_heavy_roster_is_exactly_this_fixture() {
    let frozen: BTreeSet<String> = FROZEN_HEAVY_ROSTER
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .map(str::to_string)
        .collect();
    let found: BTreeSet<String> = heavy_tagged_tests().into_iter().collect();

    assert!(
        !found.is_empty(),
        "found no heavy:-tagged test in the tree. Either every heavy battery was \
         demoted (then FROZEN_HEAVY_ROSTER must be emptied deliberately, and this \
         assertion updated to say so) or the scanner's #[ignore = \"...\"]/heavy: \
         match broke — the one outcome this guard must never quietly reach."
    );

    let added: Vec<&String> = found.difference(&frozen).collect();
    assert!(
        added.is_empty(),
        "new heavy:-tagged test(s) not in the frozen roster (spec §4, The \
         Governor):\n{}\n\nSince decision 0426 the tier is a chamber phase again, so \
         each of these is charged to EVERY subsequent merge. Append \
         the line(s) to cli/tests/fixtures/heavy-roster.txt in the same commit and \
         say why in the message.",
        added
            .iter()
            .map(|p| format!("  {p}"))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let stale: Vec<&String> = frozen.difference(&found).collect();
    assert!(
        stale.is_empty(),
        "the frozen roster names heavy:-tagged test(s) that no longer exist as such:\n{}\n\n\
         If they were demoted (e.g. to probe:) or deleted, remove their lines — this \
         direction is checked so the roster cannot rot into a permission slip nobody \
         re-reads.",
        stale
            .iter()
            .map(|p| format!("  {p}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
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
///
/// # THIS ROSTER IS A FLOOR, NOT A CENSUS (The Gnomon, 2026-08-14)
///
/// `ignore_reasons()` calls `split_once("#[ignore = \"")` and then
/// `split_once("\"]")` **on the same line**, so any `#[ignore]` whose reason
/// wraps across lines with a trailing `\` is invisible to it — it is not
/// rostered, it does not have to be canonical, and adding one never moves the
/// array length. Replicating the scanner's logic over the tree at that date
/// found **123 single-line reasons it sees and 7 multi-line ones it does not**:
///
/// - `windows/worldgen/src/lib.rs:13381` — a PREREGISTERED `>= 6`
///   distinct-dominant target, honestly unmet
/// - `windows/worldgen/src/lib.rs:13405`, `:13449` — mass/sovereignty
///   calibration findings
/// - `windows/vessel/src/liveness.rs:14716` — documents a DISPROVEN hypothesis
/// - `windows/worldgen/tests/insolation_probe.rs:372` — a live Stage-0 probe
/// - `windows/worldgen/tests/beta_calibration_sweep.rs:409` — a live sweep
/// - `windows/lab/tests/depth_ladder.rs:112` — a full-depth registry evaluation
///
/// The first two of those are exactly the class this roster exists to surface,
/// so the count here understates real deferrals by at least that many. **The
/// evidence that this is a live hazard and not a theoretical one is in this
/// campaign's own diff**: at `83eadd34` the roster went 29 → 30 without a
/// single deferral being added. The Domesday exclusion test was *already*
/// `#[ignore]`d, under a reason beginning `FALSIFIED (spec §3.3's partition
/// claim, …` that wrapped over two lines and so was invisible; rewriting that
/// reason onto one line is what made it appear. (Quoted without its attribute
/// syntax on purpose — a complete literal in a doc comment would itself be
/// scanned, which is why the roster's first entry is `"..."`.) The 30 → 31
/// move at `a917db11` was, by contrast, a genuinely new pin, written
/// single-line from the start — so this campaign contributed one of each,
/// which is why the shape was visible at all.
///
/// Not fixed here deliberately: widening the scanner re-validates every
/// currently-invisible reason against a verbatim rule they were never checked
/// against, which is a campaign-sized change to a ratchet. Tracked as
/// `TOOL-ignore-scanner-is-line-oriented` in the idea registry.
///
/// # ONE REASON STRING MUST SATISFY TWO GUARDS THAT DO NOT KNOW ABOUT EACH OTHER
///
/// This file's verbatim `heavy:` check and
/// `windows/lab/tests/preregistration_guard.rs`'s default-deny scan both
/// police `#[ignore]` reasons, and neither is aware the other exists. Their
/// domains do not nest, they *overlap*:
///
/// - `heavy_tier.rs` scans **every `.rs` file in the repo** (`collect_rs` from
///   the repo root) but only adjudicates reasons containing `heavy:` or
///   `stale-second-opinion:`, plus this roster.
/// - `preregistration_guard.rs` scans **only `windows/lab/tests/*calibration*.rs`**
///   (see its `calibration_files()`), and demands every reason there name a
///   cost or cite a decision number.
///
/// So a `heavy:` battery inside a lab calibration file must satisfy both at
/// once — the canonical string is written to do that, which is why it is
/// verbatim rather than a prefix. And an `#[ignore]` in any `src/` file is
/// outside **both**: outside `preregistration_guard`'s path filter, and
/// outside this file's adjudication unless its reason happens to carry a
/// token. Four of the seven blind spots listed above are exactly that case.
const EXPECTED_UNTOKENISED: [&str; 34] = [
    "...",
    "PREREGISTERED, cannot adjudicate at n=120: awaits TOOL-anomaly-ranking-concentrates-injection (recall@10 = 0.6000 over 120 pairs, exactly ON the 0.60 bar; six census epochs of one unchanged report read 0.5667, 0.6083, 0.6000, 0.6083, 0.6083 and 0.6000, all inside one SE of the bar, so the battery separates nothing. The sixth is the first taken after the evaluable surface grew, 117 -> 118 columns; an ablation dropping the new column re-reads 72/120 arm for arm, so the surface contributed nothing and the reading stays comparable)",
    "PREREGISTERED, not met: awaits BIO-gause-distinctness-vacuous (the corrected climate collapsed all three arms of the cv-ratio instrument - real 0.9945, goblin-niche-substituted 0.9964, width-only 0.9964 against 0.9747 when last authored - so the real gap 0.0055 no longer clears the 0.007 floor and the statistic can no longer separate human from a goblin-substituted human; lowering the floor would retune away the very vacuity it exists to announce)",
    "PREREGISTERED, not met: awaits BIO-raid-partition-order-statistic (decision 0138; drow fell to 14/60 = 0.233 under the 0.30 raider floor when The Glasshouse corrected the climate, to 12/60 = 0.200 at The Underworld's close, to 10/60 = 0.167 at The Granary's close (named at The Governor's close, 2026-08-28: eeaa011fd, BAKE stream epoch v2 -> v3), and to 9/60 = 0.150 at The Winze's close (mechanism measured, not assumed: the breach hazard, NOT the working's placement - see the witness), denominator held at 60 throughout, and the floor's stated mechanism - that the raid branch stopped running - is refuted by 9 live re-seats, so the floor is reading a post-epoch world at a pre-epoch scale)",
    "PREREGISTERED, not met: awaits BIO-rung-weighted-concentration (a stronghold-only axis reads relocation one rung down as suppression)",
    "PREREGISTERED, not met: awaits BIO-supply-drowns-niche (supply magnitude drowns the condition niche)",
    "PREREGISTERED, not met: awaits CLIM-shelf-single-rung-threshold (an unmeasured 5% ceiling on shelf-only ocean vertices; measured 5.85%, unremarkable against Earth's ~7-8% shelf fraction)",
    "PREREGISTERED, not met: awaits LOC-riparian-dry-overlap (1 of 35 riparian rooms on seed 42 reads dry; the riparian noun and the dry clause are two different functions of moisture, which R-8's by-construction wording assumed away, and at n=1 a tolerance is indistinguishable from switching the test off)",
    "PREREGISTERED, not met: awaits MAP-waterfall-threshold-mis-scaled (WATERFALL_MIN_DRAINAGE = 80 was calibrated on pre-epoch catchments; the sea-level epoch shortened drainage paths, so seed 42's loud vertices fell 34 -> 16 against a floor of 17 and strong crossings 8 -> 2 against a floor of 4, and lowering either floor would delete the only instrument that noticed)",
    "PREREGISTERED, not met: awaits PROC-domesday-all-absent-blind-spot (5 zero-present-value columns are invisible to D2/D4 — stats::numeric returns None on an empty column)",
    "PREREGISTERED, not met: awaits TOOL-min-vs-max-separation-compares-an-overlap (decision 0134 retires it; the whole-roster Spearman rho, already asserted above, carries the direction)",
    "PREREGISTERED, not met: awaits TOOL-touchstone-aggregate-tolerance-is-absolute-on-a-small-count (the qualifying rule pairs a >= 40% held-telling churn bar with an ABSOLUTE |delta| <= 4 tolerance on the mutually-exclusive aggregate, and that aggregate is a count in the tens, not the ~100 the rule was written against: across three arms of The Winze it read 23 / 17 / 9 with recency's delta at +2 / +7 / +7, so the arm nearest main passed by two events and the quantity's natural range dwarfs its tolerance. The churn half still reproduces - 62.08% against the committed note's 62.64%, clearing the frozen 20% criterion 3x - and is asserted UNIGNORED as a witness in touchstone_controls_probe_positive_signature, which also keeps this file's load-bearing shipped_absent == 0 control running per heavy-tier-adjudication row 81. Widening the tolerance would retune away the only instrument that noticed the aggregate is small, so decision 0016 keeps the unmet criterion on the record instead; a successor re-derives the tolerance from the aggregate's own base and re-freezes it)",
    "TODO: re-enable once the number settles",
    "The Hand Task 3: NEITHER route to this test works, and the second one is a finding about the sim (docs/retrospectives/the-hand.md). (1) THE SEAM DOES NOT SERVE IT: place_creature_at_me/place_creature_out_of_my_sight only place a body at the possessions OWN room, so they can manufacture co-location but not an ARRIVAL, which needs before=false at the wait's own start and after=true from the TICK's own commit -- something only the drive simulation can produce mid-call. Measured: placed at the flagship then relocated by its own drive-seeking, a wild creature departs reliably (see the departure test above) but never returns in 8 subsequent waits; six placed or unplaced companions (2 settled, 4 wild) produce zero arrivals across 40 unmodified waits. (2) THE SEED SEARCH STILL IN THIS FILE PASSED ON MAIN AND NOW FAILS ON EVERY SEED: world_where_an_unsensed_creature_arrives exhausts 0..64 and panics with its own message, re-measured 2026-08-24 at 233.72 s -- so, in that panics own words, either the arrival narration or the sight narrowing regressed, or no world in the range exercises the pair any more. That is a finding about the sim, not a flaky fixture. CONSEQUENCE, RECORDED DELIBERATELY: !wait's ARRIVAL narration has NO witness of any kind right now -- the departure half is covered, the arrival half is not. Closing this needs either a day-parameterised placement seam able to pre-stage a same-tick position change, or a measurement of why the search went empty, or accepting the null -- a design decision beyond a co-location fixture.",
    "calibration: run by hand, prints the approach_ease quantiles",
    "compiles the workspace in release; CI runs it with -- --ignored",
    "failing — investigate later",
    "flaky after the refactor",
    "measurement: builds eight full worlds; run explicitly with --ignored",
    "measurement: builds one full world; run explicitly with --ignored",
    "measurement: builds one world to BuildDepth::Terrain; run explicitly with --ignored",
    "one-shot before-arm capture (The Fathom, Task 4 Step 1); run by hand, not a standing regression test - see module doc",
    "one-shot before-arm capture (The Sources, Task 9 Step 1); run by hand, not a standing regression test - see module doc",
    "readout: chronicle evidence, run manually with --nocapture",
    "regenerates the committed occupancy fixture; run by hand - the drift check above is the gate",
    "regenerates the committed repose exposure fixture; run by hand - the drift check above is the gate",
    "runs the full gathering census; the fixture is drift-checked in CI",
    "runs the full live census sweep; the fixture is drift-checked in CI",
    "runs the full ~450s (debug) census; fixtures are drift-checked in CI",
    "search: re-derives the annihilate fixture's seed; run explicitly with --ignored",
    "search: re-derives the wear fixture's seed; run explicitly with --ignored",
    "search: re-derives the wear-probe fixture's seed; run explicitly with --ignored",
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
///
/// **A fourth kind is now here five times, and is neither of those things.**
/// The `"PREREGISTERED, not met: awaits <registry-slug> (<reason>)"` entries
/// carry a preregistered prediction that was MEASURED and NOT MET — the
/// test's failure is the record, and the slug names the registry row a
/// successor must discharge it against. They are findable (the prefix greps
/// cleanly) and they name an owner (the row), so they lack nothing a token
/// class would give them except membership in the filter above; they sit in
/// this roster rather than in a token class because that filter governs
/// which tier RUNS a test, and a preregistered-not-met pin must be run by
/// neither tier. Adding one is a review decision like any other entry here:
/// The Radiation's is `BIO-rung-weighted-concentration`, reviewed
/// 2026-08-10, evidence at
/// `windows/worldgen/tests/radiation_readout.rs::desert_elf_concentrates_in_its_authored_stronghold_biomes`.
/// The Fathom's is `CLIM-shelf-single-rung-threshold`, reviewed 2026-08-12 —
/// not a falsified mechanism but an unmeasured threshold (5.85% measured
/// against a 5% ceiling authored before anyone measured a real world's
/// shelf fraction), which the ruling deliberately left unmoved rather than
/// widened to fit the measurement; evidence at
/// `windows/worldgen/tests/fathom_column_probe.rs::h1_clause_3_single_rung_share_preregistered_not_met`.
/// The Gnomon added two, reviewed 2026-08-13:
/// `PROC-domesday-all-absent-blind-spot` (evidence at
/// `windows/lab/src/domesday/anomaly.rs`), and
/// `TOOL-anomaly-ranking-concentrates-injection` — the campaign's own
/// **headline falsification**, evidence at
/// `windows/lab/tests/anomaly_injection.rs::h1_recall_at_10`.
/// `BIO-supply-drowns-niche` predates all of these and carries no sentence
/// here; its site is
/// `windows/worldgen/tests/occupancy_readout.rs`.
/// The Ballast's is `TOOL-min-vs-max-separation-compares-an-overlap`, reviewed
/// 2026-08-15 — the retirement, under [decision
/// 0134](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0134-a-partition-statistic-refuted-by-its-own-mechanism-is-retired-not-rescued.md),
/// of a PRIMARY sign claim The Radiation had diagnosed but deliberately left
/// standing; evidence at
/// `windows/lab/tests/disposition_calibration.rs::the_weakest_raider_beats_the_strongest_abstainer_primary_claim`.
/// The Glasshouse added three at its close, reviewed 2026-08-15, all of one
/// kind: an instrument that noticed something true about a world that moved
/// under it, deferred at its pre-epoch value rather than widened to fit the
/// new measurement.
/// `LOC-riparian-dry-overlap` — R-8's zero-tolerance invariant, evidence at
/// `windows/locale/tests/wetness_reading.rs::no_room_reads_riparian_and_dry`;
/// at n=1 offender a tolerance would be indistinguishable from switching the
/// test off, so it is ignored whole.
/// `BIO-gause-distinctness-vacuous` — evidence at
/// `windows/worldgen/tests/generalist_distinctness.rs`. All three arms of the
/// cv-ratio instrument collapsed together under the corrected climate, so the
/// statistic can no longer separate human from a goblin-substituted human.
/// `BIO-raid-partition-order-statistic` — evidence at
/// `windows/lab/tests/disposition_calibration.rs::every_raider_clears_the_floor_preregistered_not_met`.
/// **A SECOND, INDEPENDENT failure in the file decision 0134 had just
/// retired the PRIMARY claim from, and the two must not be read as one
/// retirement**: 0134 explicitly KEPT the raider floor, which was true when
/// written, and the climate correction then put drow under it at 14/60.
/// **Two of the three were SPLIT out of a larger test rather than ignoring
/// it**, which is worth doing whenever an ignore would take live assertions
/// down with the stale one. `MAP-waterfall-threshold-mis-scaled` left the
/// ordering claim, its straddle anti-vacuity and the universal "every
/// crossing above the threshold is Impassable" running in
/// `the_discharge_clause_makes_the_strongest_crossing_impassable`; the raider
/// floor left the whole-roster Spearman rho running in the battery above,
/// which is the claim that actually survives.
/// All four carry the witness this doc asks for
/// (`the_riparian_dry_overlap_is_pinned_as_a_witness`,
/// `the_loud_reach_population_is_pinned_as_a_witness`,
/// `the_collapsed_cv_ratio_arms_are_pinned_as_witnesses`,
/// `the_sub_floor_raider_reading_is_pinned_as_a_witness`), so the deferred
/// figures stay measured.
///
/// The Winze added one, reviewed 2026-08-29:
/// `TOOL-touchstone-aggregate-tolerance-is-absolute-on-a-small-count`, at
/// `windows/hearsay/tests/touchstone_controls_probe.rs::touchstone_controls_probe_positive_aggregate_tolerance_preregistered_not_met`. It is the Glasshouse
/// SPLIT pattern again, and for a documented reason rather than a stylistic
/// one: `docs/audits/heavy-tier-adjudication.md` row 81 says the failing
/// assertion is *"report-shaped in isolation"* and that the KEEP verdict rests
/// on `shipped_absent == 0` in the SAME test body, so an `#[ignore]` on the
/// pair would have disabled the load-bearing half to defer the report-shaped
/// one. Its witness is not a pinned integer, and the reason is worth reading
/// before the next entry copies the pattern: the finding IS that the deferred
/// counts move for any world change (23 / 17 / 9 across three arms of one
/// campaign, against a ±4 tolerance), so a pinned integer would re-import the
/// brittleness being deferred. The witness is the campaign's own frozen
/// criterion instead — `positive_tail >= 20%`, cleared 3x — asserted unignored
/// beside it.
///
/// **The Gnomon also found the rot this convention carries, and closed it for
/// its own entry.** An `#[ignore]`d measurement stops being measured: the
/// figure quoted in the reason above is a claim nothing re-derives, so a later
/// change to the instrument turns it silently into fiction. The remedy is not
/// a sixth ratchet — it is one always-running assertion beside the ignored
/// test that pins the measured integers as a **witness**
/// (`the_falsified_recall_is_pinned_as_a_witness`). An entry added here in
/// future should carry the same, and the reviewer should ask for it.
#[test]
fn the_untokenised_ignore_reasons_are_exactly_this_roster() {
    let reasons = ignore_reasons();
    let mut untokenised: Vec<String> = reasons
        .iter()
        .filter(|r| {
            !r.contains("heavy:") && !r.contains("stale-second-opinion:") && !r.contains("probe:")
        })
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
// The serialized-battery filters. `.config/nextest.toml` pins two DIFFERENT
// classes of heavy test to `threads-required = "num-cpus"` — the WHOLE
// runner — each in its own override table, so each runs ALONE on the
// canonical box:
//
//   scatter-sweep      three batteries that parallelise their own 200-seed
//                      sweeps across every core (The Scatter) — pinned so
//                      they do not saturate everything ELSE.
//   wall-clock-budget  two cost-ceiling tests that cannot tell contention
//                      from a regression on their own (The Ballast) — pinned
//                      so the tier's OWN saturation does not redden them.
//
// A THIRD class, `sized-sweep` (The Governor, Task 8), pins
// `threads-required = <a bounded integer>` instead — a panel-width
// reservation for `map_seeds` callers that do NOT want the whole runner, so
// they never drain it and never need front-loading either. Its own guard,
// [`the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel`],
// is below the two guards for the classes above, and its own doc comment
// covers why its anti-vacuity check has a different shape than theirs. It is
// not folded into the "two classes" language throughout the rest of this
// section comment because everything below was written before it existed
// and still describes those two accurately; the sized class's own functions
// and guard are self-contained rather than threaded through this comment.
//
// `gate-full-heavy.sh` (now run by the `heavy` lane set — `make lane
// SET=heavy REF=<full-sha>`, `make heavy-remote REF=<full-sha>`, or as a
// merge-queue chamber phase, decision 0139) sets no
// `test-threads` limit, so without either pin the box runs up to 40 heavy
// processes at once, each wanting 40 worker threads.
//
// Both filters are HAND-MAINTAINED LISTS, and their failure mode is silent
// and expensive. Rename a battery, or add a fourth that sweeps seeds, and the
// scatter-sweep filter simply matches fewer tests: nothing reddens, the box
// is oversubscribed again, and the FIRST SYMPTOM is a spurious
// `hornvale::scene_cost` failure that reads like a performance regression —
// which is exactly the 341 s of wall clock the pin was bought with, and
// exactly the class of failure the wall-clock-budget pin now exists to
// prevent even when the scatter-sweep roster itself is correct (a fully
// loaded tier, with every battery correctly serialized, still runs many
// OTHER heavy tests concurrently, and that alone was enough to redden
// `session_cost.rs`'s ceiling — see `.config/nextest.toml`'s comment).
//
// The precedent is two files away: `scripts/gate-full-heavy.sh` already
// asserts `tag_count == name_count` so a heavy tag that drifts off its `fn`
// cannot silently vanish from `make gate-full`. This is the same guard for
// the same class of drift, one directory over — now applied to two classes
// rather than one, each still checked in both directions.
//
// A THIRD MARKER, `front-loaded` (The Governor, Task 1), tags BOTH tables
// above rather than adding a table of its own. `threads-required =
// "num-cpus"` makes nextest drain the WHOLE runner before starting the
// pinned test, then restart everything scheduled after it from cold — a
// barrier tax measured at 484 s and 485 s on two runs of this tier, at
// different SHAs, against a ~1068 s theoretical optimum on a 1552 s tier.
// `priority = 100` (nextest 0.9.140, `i8`, higher runs earlier) makes
// nextest schedule the reserving test FIRST instead, so the drain happens
// once, at the start, instead of mid-run. Verified on a seven-test scratch
// probe outside this repo (four cores): no `priority`, wall 10.076 s, the
// reserving test completes 3rd; `priority = 100` on the reserving test and
// `priority = 50` on the tier's next-longest test, wall 8.041 s, the
// reserving test completes 1st. A positive control (`priority = 500`) is
// rejected by nextest with `invalid type: 64-bit integer 500, expected an
// signed 8 bit integer`, which is why the config parses `priority` as
// `i8` at all rather than silently ignoring an out-of-range value.
// ============================================================================

/// Where the serialized-battery pins live.
const NEXTEST_CONFIG: &str = ".config/nextest.toml";

/// The marker that makes an override a serialization pin.
const THREADS_REQUIRED: &str = "threads-required = \"num-cpus\"";

/// The setting prefix that opts a whole-runner test into scheduling FIRST
/// (The Governor, Task 1) — checked as a live SETTING, mirroring
/// [`THREADS_REQUIRED`], for the same reason: the `# class: front-loaded`
/// marker COMMENT records intent, but only a live `priority = ` line
/// carries it out. Found by mutation-testing this guard: deleting the real
/// `priority = 100` line and leaving the marker comment and
/// `threads-required` line untouched left the guard GREEN, because nothing
/// checked for this line at all. Matched by prefix, not by parsing the
/// `i8` value — this guard verifies a whole-runner test IS front-loaded,
/// not that the chosen priority is well-chosen.
const PRIORITY_SETTING_PREFIX: &str = "priority = ";

/// Markers allowed to tag MORE THAN ONE override table (The Governor,
/// Task 1). Every other marker must tag at most one table — the original,
/// stricter invariant — because a duplicated table under the SAME marker is
/// the copy-paste mistake that silently reintroduces an unprimed
/// `threads-required` reservation: found by mutation-testing this guard,
/// duplicating the `# class: scatter-sweep` table (same filter, same
/// `threads-required`, no `priority`, no `front-loaded` tag on the copy)
/// left the whole heavy_tier suite GREEN once per-marker uniqueness was
/// dropped for every marker rather than just this one. `front-loaded` is
/// the deliberate exception: it tags BOTH the scatter-sweep and
/// wall-clock-budget tables, because every test that reserves the whole
/// runner must be front-loaded and those two classes are what reserves it.
///
/// THIS LIST BINDS TWO STRUCTURALLY DIFFERENT PREDICATES TO ONE ALLOWLIST —
/// "may tag multiple override tables" and "must carry a live `priority`
/// setting" (see `requires_priority` in [`pinned_filter_names_for_class`]) —
/// and they coincide today only because `front-loaded` is the sole marker
/// with both properties. The sized-sweep class (The Governor, Task 8) needs
/// NEITHER: one shared bounded width can serve every sized-sweep test in a
/// single table (no barrier tax means no reason yet to split by width), and
/// a bounded reservation never drains the whole runner, so there is nothing
/// to front-load. So `sized-sweep` is deliberately NOT added here — the
/// default (single table, no `priority` required) is already correct for
/// it, and forcing it into this list for either property would grant it a
/// behaviour it does not need and this test suite would then have to justify.
const MARKERS_ALLOWING_MULTIPLE_TABLES: &[&str] = &["front-loaded"];

/// The call that makes a battery internally parallel — the property BOTH
/// the scatter-sweep pin and the sized-sweep pin exist for (The Governor,
/// Task 8 split membership between the two by [`SIZED_SWEEP_MARKER`], not
/// by a different call). Matching the CALL (not the module) is deliberate: a
/// test that merely mentions the helper in prose is not the thing that
/// saturates a box.
const SWEEP_CALL: &str = "seed_sweep::map_seeds(";

/// The doc-comment marker that opts a wall-clock budget test INTO serialized
/// scheduling — the mirror of [`SWEEP_CALL`] for tests that are VICTIMS of
/// tier contention rather than a SOURCE of it (The Ballast). Matching this
/// literal line, not the whole doc comment it sits in, is deliberate for the
/// same reason [`SWEEP_CALL`] matches a call and not a module: a test that
/// merely discusses co-scheduling in prose elsewhere is not the thing that
/// needs isolating.
const CO_SCHEDULE_SENSITIVE_MARKER: &str = "nextest: co-schedule-sensitive";

/// The doc-comment marker that opts a [`SWEEP_CALL`] caller OUT of the
/// whole-runner scatter-sweep class and INTO the bounded sized-sweep class
/// instead (The Governor, Task 8) — the mirror-image of
/// [`CO_SCHEDULE_SENSITIVE_MARKER`]: that one pulls a test INTO serialized
/// scheduling that would otherwise run unpinned; this one pulls a
/// `map_seeds` caller OUT of the reservation it would otherwise default
/// into. A test carrying this marker but never calling [`SWEEP_CALL`] pins
/// nothing on its own — see [`map_seeds_callers`].
const SIZED_SWEEP_MARKER: &str = "nextest: sized-sweep";

/// The setting PREFIX shared by both forms `threads-required` can take —
/// distinct from [`THREADS_REQUIRED`], which is the exact whole-runner
/// STRING form. A `# class: sized-sweep` table's setting must start with
/// this prefix but must NOT equal [`THREADS_REQUIRED`]: using the
/// `"num-cpus"` string on the sized class would silently reproduce the
/// whole-runner reservation the class exists to avoid — the exact "naive
/// fix" trap this campaign's spec names explicitly (The Governor, Task 8).
const THREADS_REQUIRED_PREFIX: &str = "threads-required = ";

/// Which form of `threads-required` a class's override table(s) must carry
/// — the whole-runner `"num-cpus"` string, or a bounded integer (The
/// Governor, Task 8). Passed to [`pinned_filter_names_for_class`] so one
/// scanner serves all three classes without silently accepting the wrong
/// shape for any of them.
enum ThreadsRequiredKind {
    /// The exact [`THREADS_REQUIRED`] string — reserves the WHOLE runner.
    WholeRunner,
    /// Any live `threads-required = ` setting OTHER than [`THREADS_REQUIRED`]
    /// — reserves a bounded number of slots, never the whole runner.
    Bounded,
}

/// Where an override table starting at `start` (the index of its own
/// `[[profile.default.overrides]]` header line) ENDS: the next such header,
/// or the next `[profile.` SECTION header (e.g. `[profile.ci]`), whichever
/// comes first — never simply "end of file" (The Governor, Task 9,
/// precondition).
///
/// **The bug this replaces, and why it was latent rather than loud.** The
/// original scan bounded every table at the next
/// `[[profile.default.overrides]]` header only, via
/// `block_starts.get(bi + 1).unwrap_or(lines.len())` — so the LAST override
/// table's block ran all the way to end-of-file, silently absorbing
/// whatever non-override content came after it. That was harmless while
/// `# class: wall-clock-budget` was the last table in the file, because
/// nothing below it (`[profile.ci]`, `[profile.heavy]`) carried a stray
/// `filter = ` or `threads-required = ` line for the scan to mis-attribute.
/// Task 8 made `# class: sized-sweep` the new last table, with those same
/// two profiles still sitting below it — so the LATENT bug is still latent
/// today, but the next profile section that happens to declare either
/// setting (for an unrelated reason of its own) would silently corrupt the
/// sized-sweep table's scan, and nothing would say so.
fn override_table_end(lines: &[&str], start: usize) -> usize {
    lines
        .iter()
        .enumerate()
        .skip(start + 1)
        .find(|(_, l)| {
            let t = l.trim();
            t == "[[profile.default.overrides]]" || t.starts_with("[profile.")
        })
        .map(|(i, _)| i)
        .unwrap_or(lines.len())
}

/// The regression proof for [`override_table_end`]: a `[profile.*]` section
/// appended after the last override table must never be folded into that
/// table's scan, even though it is not itself another override header.
///
/// **Fails without the fix.** The old inline logic
/// (`block_starts.get(bi + 1).unwrap_or(lines.len())`) has no notion of a
/// `[profile.*]` boundary at all — with only one override header in this
/// synthetic text, it would return `lines.len()` unconditionally, and the
/// block would swallow `[profile.ci]` whole: the assertions below (no
/// `[profile.ci]` line in the block, exactly one `filter = ` line) would
/// both fail against that behaviour.
#[test]
fn override_table_end_stops_at_the_next_profile_section_not_just_eof() {
    let text = "\
[[profile.default.overrides]]
# class: alpha
filter = 'test(/a$/)'
threads-required = 4

[profile.ci]
# a profile section appended after the last override table — its settings
# must never be mistaken for the override table's own.
filter = 'test(/stray$/)'
threads-required = \"num-cpus\"
";
    let lines: Vec<&str> = text.lines().collect();
    let start = lines
        .iter()
        .position(|l| l.trim() == "[[profile.default.overrides]]")
        .expect("synthetic text has an override header");
    let end = override_table_end(&lines, start);
    let block = &lines[start..end];

    assert!(
        !block.iter().any(|l| l.trim() == "[profile.ci]"),
        "the override table's bound must stop before the next `[profile.*]` \
         section, not swallow it: {block:?}"
    );
    assert_eq!(
        block
            .iter()
            .filter(|l| l.trim().starts_with("filter = "))
            .count(),
        1,
        "exactly one `filter = ` line belongs to the override table itself; \
         found in block {block:?}"
    );
}

/// Extracts the sorted, deduped test names from the `filter = 'test(/…/) |
/// …'` line inside the `.config/nextest.toml` override table(s) tagged
/// `# class: <marker>` — an INLINE marker line living INSIDE the
/// `[[profile.default.overrides]]` table it identifies, not the banner
/// comment above it, so the two `threads-required` tables The Ballast leaves
/// behind cannot be confused with each other. Std-only string scanning —
/// this workspace admits no TOML parser (decision 0004).
///
/// Every matched table must carry a live `threads-required` SETTING (never
/// just a comment mentioning it) matching `kind` — the exact
/// [`THREADS_REQUIRED`] string for [`ThreadsRequiredKind::WholeRunner`], or
/// any OTHER live `threads-required = ` setting for
/// [`ThreadsRequiredKind::Bounded`] (The Governor, Task 8 — a `Bounded`
/// class whose table used the whole-runner string would silently reproduce
/// the reservation it exists to avoid, so that exact value is rejected, not
/// just any-value-accepted). A marker in [`MARKERS_ALLOWING_MULTIPLE_TABLES`]
/// may tag more than one table — every other marker may tag at most one, or
/// this panics — and for exactly those multiple-table markers, every matched
/// table must ALSO carry a live `priority = ` setting (see
/// [`PRIORITY_SETTING_PREFIX`]). The `i8` value itself is never inspected,
/// only whether the line is present.
///
/// The `filter = ` line's TEST NAMES are extracted only from `test(/…/)`
/// terms; a filter with none (nextest's `none()` predicate, the sized-sweep
/// table's value while it has no members — see `.config/nextest.toml`'s
/// comment) yields an empty roster rather than a parse failure, which is
/// what makes a legitimately empty class distinguishable from a missing
/// table: the table-existence and setting checks above still run and still
/// panic on absence; only the NAME list is allowed to be empty.
fn pinned_filter_names_for_class(marker: &str, kind: ThreadsRequiredKind) -> Vec<String> {
    let text = fs::read_to_string(repo_root().join(NEXTEST_CONFIG))
        .expect(".config/nextest.toml is readable");
    let class_line = format!("# class: {marker}");

    let lines: Vec<&str> = text.lines().collect();
    let block_starts: Vec<usize> = lines
        .iter()
        .enumerate()
        .filter(|(_, l)| l.trim() == "[[profile.default.overrides]]")
        .map(|(i, _)| i)
        .collect();
    assert!(
        !block_starts.is_empty(),
        "{NEXTEST_CONFIG} has no `[[profile.default.overrides]]` table at all"
    );

    // A class marker may tag MORE THAN ONE override table only if it is
    // listed in `MARKERS_ALLOWING_MULTIPLE_TABLES` (The Governor, Task 1;
    // see that const's comment for why `front-loaded` is the one marker
    // that needs this and why every other marker keeps the original,
    // stricter one-table invariant).
    let allows_multiple = MARKERS_ALLOWING_MULTIPLE_TABLES.contains(&marker);
    let mut targets: Vec<&[&str]> = Vec::new();
    for &start in &block_starts {
        let end = override_table_end(&lines, start);
        let block = &lines[start..end];
        if block.iter().any(|l| l.trim() == class_line) {
            assert!(
                allows_multiple || targets.is_empty(),
                "more than one override table in {NEXTEST_CONFIG} is tagged \
                 {class_line:?} — the marker must be unique per class (add it to \
                 MARKERS_ALLOWING_MULTIPLE_TABLES if that is now deliberate). A \
                 duplicated override table under the same marker is the copy-paste \
                 mistake that silently reintroduces an unprimed reservation: the \
                 stray copy inherits the marker and the filter but not necessarily \
                 the settings that made the original one safe."
            );
            targets.push(block);
        }
    }
    assert!(
        !targets.is_empty(),
        "no override table in {NEXTEST_CONFIG} is tagged {class_line:?}. The \
         `{class_line}` marker line lives INSIDE the `[[profile.default.overrides]]` \
         table it identifies (see this file's section comment)."
    );

    // Markers in MARKERS_ALLOWING_MULTIPLE_TABLES additionally certify a
    // live `priority = ` setting per table, mirroring the THREADS_REQUIRED
    // check below — see PRIORITY_SETTING_PREFIX's comment for why: the
    // `# class: front-loaded` marker comment records intent, and only a
    // live setting carries it out.
    let requires_priority = allows_multiple;

    let mut names = Vec::new();
    for block in targets {
        // SETTINGS ONLY, never comments. Found by mutation-testing this guard:
        // deleting the real `threads-required` line left the check GREEN, because
        // the section comment above the override quotes the setting verbatim while
        // explaining it. A guard that a comment can satisfy is not a guard.
        let settings: Vec<&str> = block
            .iter()
            .map(|l| l.trim())
            .filter(|l| !l.starts_with('#') && !l.is_empty())
            .collect();
        match kind {
            ThreadsRequiredKind::WholeRunner => {
                assert!(
                    settings.contains(&THREADS_REQUIRED),
                    "the {class_line:?} table in {NEXTEST_CONFIG} has no live \
                     {THREADS_REQUIRED:?} SETTING (a comment mentioning it does not \
                     count). The serialization pin for this class is GONE, which \
                     silently re-exposes it to canonical-box contention — see this \
                     file's section comment."
                );
            }
            ThreadsRequiredKind::Bounded => {
                let bounded = settings
                    .iter()
                    .find(|l| l.starts_with(THREADS_REQUIRED_PREFIX));
                assert!(
                    bounded.is_some(),
                    "the {class_line:?} table in {NEXTEST_CONFIG} has no live \
                     {THREADS_REQUIRED_PREFIX:?}<int> SETTING (a comment mentioning it \
                     does not count). A sized-sweep table with no reservation at all \
                     re-exposes its members to canonical-box contention exactly like a \
                     missing whole-runner pin does."
                );
                assert_ne!(
                    *bounded.expect("checked above"),
                    THREADS_REQUIRED,
                    "the {class_line:?} table in {NEXTEST_CONFIG} sets \
                     {THREADS_REQUIRED:?} — the WHOLE-RUNNER value. That is exactly the \
                     naive-fix trap this class exists to avoid: a bounded class must \
                     reserve a bounded integer, never `\"num-cpus\"`, or every member \
                     pays a full-drain barrier again."
                );
            }
        }
        if requires_priority {
            assert!(
                settings
                    .iter()
                    .any(|l| l.starts_with(PRIORITY_SETTING_PREFIX)),
                "the {class_line:?} table in {NEXTEST_CONFIG} has no live \
                 {PRIORITY_SETTING_PREFIX:?} SETTING (the `{class_line}` marker \
                 comment does not count — see PRIORITY_SETTING_PREFIX's doc \
                 comment). This test reserves the whole runner but is not \
                 scheduled first, which reintroduces the drain-then-cold-restart \
                 barrier tax this class exists to remove."
            );
        }
        // NOT filtered on `.contains("test(/")` — a class may legitimately have
        // no `test(/…/)` terms at all (nextest's `none()` predicate, the
        // sized-sweep table's value while it has no members). The `filter = `
        // line must still be present exactly once; the loop below simply
        // extracts zero names from a filter that names none.
        let filter_lines: Vec<&str> = settings
            .iter()
            .copied()
            .filter(|l| l.starts_with("filter = "))
            .collect();
        assert_eq!(
            filter_lines.len(),
            1,
            "expected exactly one `filter = ` line in the {class_line:?} \
             table of {NEXTEST_CONFIG}; found {}.",
            filter_lines.len()
        );

        let mut rest = filter_lines[0];
        while let Some((_, after)) = rest.split_once("test(/") {
            let (name, tail) = after
                .split_once("$/)")
                .expect("a test(/…/) term in the filterset is end-anchored with `$/)`");
            names.push(name.to_string());
            rest = tail;
        }
    }
    names.sort();
    names.dedup();
    names
}

/// The scatter-sweep class's pinned roster (The Scatter).
fn serialized_filter_names() -> Vec<String> {
    pinned_filter_names_for_class("scatter-sweep", ThreadsRequiredKind::WholeRunner)
}

/// The wall-clock-budget class's pinned roster (The Ballast).
fn budget_filter_names() -> Vec<String> {
    pinned_filter_names_for_class("wall-clock-budget", ThreadsRequiredKind::WholeRunner)
}

/// The sized-sweep class's pinned roster (The Governor, Task 8) — a bounded
/// `threads-required` integer rather than the whole-runner string, so this
/// is the one caller of [`pinned_filter_names_for_class`] passing
/// [`ThreadsRequiredKind::Bounded`]. It was empty on the tree Task 8 landed
/// on — `.config/nextest.toml`'s table pinned `filter = 'none()'`,
/// deliberately — and Task 9 then gave it its three members
/// (`the_blast_radius_readout`,
/// `is_the_raid_proxy_ambiguous_or_is_its_population_stale`,
/// `zero_dispersion_collapses_between_settlement_variance`). The roster of
/// record is that table, never this comment; see
/// [`the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel`]'s
/// doc comment for why an empty roster was the correct state then and is not
/// now.
fn sized_filter_names() -> Vec<String> {
    pinned_filter_names_for_class("sized-sweep", ThreadsRequiredKind::Bounded)
}

/// Every test that reserves the whole runner — the union of both
/// `threads-required = "num-cpus"` classes above (The Governor, Task 1).
/// Built from [`serialized_filter_names`] and [`budget_filter_names`]
/// unchanged, so it is exactly "whichever named classes reserve the
/// runner today", not a blanket scan of the config file.
fn whole_runner_filter_names() -> Vec<String> {
    let mut names = serialized_filter_names();
    names.extend(budget_filter_names());
    names.sort();
    names.dedup();
    names
}

/// The `front-loaded` class's pinned roster (The Governor, Task 1): every
/// test given a `priority` so nextest starts it before draining the runner
/// for it mid-tier. Unlike the two classes above, this marker tags BOTH the
/// scatter-sweep and wall-clock-budget tables — see
/// [`pinned_filter_names_for_class`]'s comment on why that is safe.
fn front_loaded_filter_names() -> Vec<String> {
    pinned_filter_names_for_class("front-loaded", ThreadsRequiredKind::WholeRunner)
}

/// Every heavy/probe-tagged test whose body calls [`SWEEP_CALL`], paired
/// with whether its doc comment ALSO carries [`SIZED_SWEEP_MARKER`] — the
/// single scan [`internally_parallel_heavy_tests`] and
/// [`sized_sweep_heavy_tests`] both build on (The Governor, Task 8), so the
/// one subtle piece of state tracking here — the marker is written in the
/// doc comment ABOVE `#[test]`/`#[ignore]`/`fn`, but must still be readable
/// while scanning the test's BODY below `fn`, which is a different span than
/// [`co_schedule_sensitive_heavy_tests`]'s marker (consumed immediately, at
/// the `fn` line) — is written and gets-it-right exactly once. The marker is
/// snapshotted into `current_is_sized` AT the `fn` line (alongside `current`
/// itself), then the accumulating flag resets so a later, unrelated test's
/// doc comment cannot inherit it.
///
/// Line-oriented, matching `gate-full-heavy.sh`'s own grep-based discovery, so
/// the two agree about what a heavy test is. A heavy `#[ignore]` tag sits
/// directly above its `fn`; a test's region runs from that `fn` to the next
/// `#[test]` attribute or end of file.
///
/// **Consequence: a [`SWEEP_CALL`] inside a HELPER defined ABOVE the test is
/// invisible.** `current` only becomes `Some(name)` at the test's own `fn`
/// line, so a call textually earlier in the file — inside a helper the test
/// happens to invoke — is scanned while `current` is still `None` or names an
/// unrelated earlier test, and is silently never attributed to this test at
/// all (The Governor, Task 9; found converting
/// `tolerance_mutation.rs::zero_dispersion_collapses_between_settlement_
/// variance`, whose sweep originally lived in a `population` helper defined
/// earlier in the file). The fix is not to widen this scanner — it is to
/// inline the `map_seeds` call into the test body, the shape every other
/// caller in this tree already uses. See
/// [`the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel`]'s
/// failure message, which names this before it names dropping the pin.
fn map_seeds_callers() -> Vec<(String, bool)> {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    sources.sort();

    let mut found: Vec<(String, bool)> = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        let mut next_fn_is_heavy = false;
        let mut marker_seen = false;
        let mut current: Option<String> = None;
        let mut current_is_sized = false;
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.contains(SIZED_SWEEP_MARKER) {
                marker_seen = true;
            }
            if trimmed.starts_with("#[test]") {
                current = None;
            }
            if trimmed.starts_with("#[ignore = \"") {
                // `probe:` COUNTS HERE TOO, AND THE REASON IS THAT THIS
                // ROSTER IS NOT ABOUT THE TIER. What it selects is a test that
                // parallelises its OWN sweep across every core, so the pin
                // exists to stop it saturating the box while other work runs.
                // That property belongs to the test, not to which set invokes
                // it: decision 0148 moved `the_fares_*` from `heavy:` to
                // `probe:`, and a probe run BY HAND wants the pin for exactly
                // the same reason a heavy one did. Keying on `heavy:` alone
                // would have silently dropped two of the three batteries out
                // of `.config/nextest.toml`'s scatter-sweep class — the pin
                // would still have named them, and nothing would have said so.
                next_fn_is_heavy = trimmed.contains("heavy:") || trimmed.contains("probe:");
                current = None;
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("fn ")
                && let Some((name, _)) = rest.split_once('(')
            {
                current = next_fn_is_heavy.then(|| name.to_string());
                current_is_sized = marker_seen;
                next_fn_is_heavy = false;
                marker_seen = false;
                continue;
            }
            if line.contains(SWEEP_CALL)
                && let Some(name) = &current
                && !found.iter().any(|(n, _)| n == name)
            {
                found.push((name.clone(), current_is_sized));
            }
        }
    }
    found.sort();
    found
}

/// Every heavy/probe battery that parallelises its own seed sweep and is
/// NOT marked [`SIZED_SWEEP_MARKER`] — i.e. the batteries that MUST reserve
/// the whole runner (The Scatter). A [`SWEEP_CALL`] caller carrying the
/// marker is deliberately excluded here: it belongs to
/// [`sized_sweep_heavy_tests`] instead (The Governor, Task 8).
fn internally_parallel_heavy_tests() -> Vec<String> {
    let mut found: Vec<String> = map_seeds_callers()
        .into_iter()
        .filter(|(_, is_sized)| !is_sized)
        .map(|(name, _)| name)
        .collect();
    found.sort();
    found.dedup();
    found
}

/// Every heavy/probe battery that parallelises its own seed sweep AND is
/// marked [`SIZED_SWEEP_MARKER`] — the bounded-panel counterpart of
/// [`internally_parallel_heavy_tests`] (The Governor, Task 8). Empty until
/// Task 9 marks its first conversion.
fn sized_sweep_heavy_tests() -> Vec<String> {
    let mut found: Vec<String> = map_seeds_callers()
        .into_iter()
        .filter(|(_, is_sized)| *is_sized)
        .map(|(name, _)| name)
        .collect();
    found.sort();
    found.dedup();
    found
}

/// Every heavy-tagged test whose doc comment carries
/// [`CO_SCHEDULE_SENSITIVE_MARKER`] somewhere above its `fn` line — i.e.
/// every wall-clock budget test that opted itself into serialized scheduling
/// because it cannot tell contention from a regression on its own.
///
/// Same line-oriented scan as [`internally_parallel_heavy_tests`], with the
/// marker line taking the place of the [`SWEEP_CALL`] line: it is consumed
/// (reset) at every `fn` boundary, so a module-level mention of the marker
/// text can never leak onto an unrelated test below it.
fn co_schedule_sensitive_heavy_tests() -> Vec<String> {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    sources.sort();

    let mut found = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        let mut marker_seen = false;
        let mut next_fn_is_heavy = false;
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.contains(CO_SCHEDULE_SENSITIVE_MARKER) {
                marker_seen = true;
            }
            if trimmed.starts_with("#[ignore = \"") {
                next_fn_is_heavy = trimmed.contains("heavy:");
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("fn ")
                && let Some((name, _)) = rest.split_once('(')
            {
                if marker_seen && next_fn_is_heavy && !found.contains(&name.to_string()) {
                    found.push(name.to_string());
                }
                marker_seen = false;
                next_fn_is_heavy = false;
                continue;
            }
        }
    }
    found.sort();
    found
}

/// The scatter-sweep pin's roster is exactly the set of heavy batteries that
/// actually scatter their own seed sweeps — checked in BOTH directions,
/// because both failure modes are silent.
///
/// A renamed battery drops out of the filter; a newly-added sweeping battery
/// never enters it. Either way nextest resumes scheduling forty heavy
/// processes against a box whose batteries each want forty worker threads,
/// and the first thing anyone sees is a wall-clock budget test going red for
/// reasons that have nothing to do with the code it measures.
///
/// A [`SWEEP_CALL`] caller marked [`SIZED_SWEEP_MARKER`] is EXCLUDED from
/// `internally_parallel_heavy_tests` (The Governor, Task 8) — it belongs to
/// the sized-sweep class checked below instead, by
/// [`the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel`].
/// A newly-converted, unmarked `map_seeds` caller still lands HERE by
/// default and still demands `"num-cpus"` — the marker is an opt-OUT into
/// the bounded class, not a silent default.
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
        "\n{NEXTEST_CONFIG}'s scatter-sweep filter and the set of heavy batteries \
         that scatter their own seed sweeps have diverged.\n  pinned in config: \
         {pinned:?}\n  actually parallel: {parallel:?}\nAdd the missing name(s) to \
         the `filter = ` line in the `# class: scatter-sweep` table, or drop the \
         stale one. Left alone this does NOT redden on its own: nextest schedules \
         the unpinned battery alongside everything else, the canonical box is \
         oversubscribed, and the historical symptom was a spurious \
         hornvale::scene_cost failure that looked like a real performance \
         regression — that specific test is now independently guarded by the \
         wall-clock-budget class below, but any OTHER budget test would still be \
         exposed the same way."
    );
}

/// The wall-clock-budget pin's mirror of the guard above (The Ballast): its
/// roster is exactly the set of heavy tests that marked themselves
/// co-schedule-sensitive — checked in BOTH directions, for the same reason.
///
/// A renamed budget test drops out of the filter silently; a newly-marked one
/// never enters it. Either way the tier resumes scheduling it alongside
/// everything else, and it can redden for reasons that have nothing to do
/// with the code it measures — exactly what happened to
/// `session_cost.rs::a_possessed_turn_stays_within_its_ceilings` before this
/// pin existed: 14.324 ms against an 8 ms ceiling under tier contention,
/// 15.897 s and PASS run alone on the same, otherwise-quiet, canonical box.
#[test]
fn the_serialization_pin_names_exactly_the_wall_clock_budget_tests_marked_co_schedule_sensitive() {
    let pinned = budget_filter_names();
    let sensitive = co_schedule_sensitive_heavy_tests();

    assert!(
        !sensitive.is_empty(),
        "found no heavy test carrying {CO_SCHEDULE_SENSITIVE_MARKER:?}. Either the \
         marker was renamed (update CO_SCHEDULE_SENSITIVE_MARKER) or this guard is \
         now asserting nothing — which is the one outcome it must never quietly \
         reach."
    );
    assert_eq!(
        pinned, sensitive,
        "\n{NEXTEST_CONFIG}'s wall-clock-budget filter and the set of heavy tests \
         marked {CO_SCHEDULE_SENSITIVE_MARKER:?} have diverged.\n  pinned in \
         config: {pinned:?}\n  marked in source: {sensitive:?}\nAdd the missing \
         name(s) to the `filter = ` line in the `# class: wall-clock-budget` table, \
         or drop the stale one. Left alone this does NOT redden on its own: nextest \
         schedules the unpinned budget test alongside the rest of the tier, and it \
         may fail under contention for reasons that have nothing to do with the code \
         it measures."
    );
}

/// The sized-sweep pin's roster is exactly the set of heavy/probe batteries
/// marked [`SIZED_SWEEP_MARKER`] that also call [`SWEEP_CALL`] — the third
/// guard, the same two-directional shape as the two above (The Governor,
/// Task 8).
///
/// **NOW CARRIES THE SAME NON-EMPTINESS ASSERT AS THE OTHER TWO, SINCE THE
/// GAP THIS PARAGRAPH USED TO DESCRIBE CLOSED (The Governor, Task 9).** At
/// the commit that introduced this class (Task 8) it deliberately omitted
/// `assert!(!sized.is_empty(), …)`: both sides of the comparison below were
/// genuinely, correctly empty, and reusing the other two guards' belt would
/// have failed the guard on arrival, before there was anything to guard —
/// the exact trap that task's brief warned against. Task 9 landed the
/// class's first real member (`warren_readout.rs::the_blast_radius_readout`,
/// this campaign's own pole test, and two more after it), so "empty" stopped
/// being this class's
/// correct, default state, and the belt is added now exactly as that
/// earlier version of this comment said a future editor should.
///
/// **"EMPTY" WAS DISTINGUISHABLE FROM "THE CLASS WAS DELETED" EVEN BEFORE
/// THIS CHANGE, AND THAT DISTINCTION IS STILL WHAT PART OF THIS GUARD LEANS
/// ON** — the same fixture-vs-source shape
/// [`the_heavy_roster_is_exactly_this_fixture`] uses a committed file for,
/// done here with the config table itself as the fixture:
/// - **The override table's existence is hard-required**, non-emptiness
///   assert or not. [`pinned_filter_names_for_class`] panics if no table in
///   `.config/nextest.toml` carries `# class: sized-sweep`, or if it has no
///   live `threads-required = <int>` setting, or if that setting is the
///   whole-runner `"num-cpus"` string — so deleting the class outright, or
///   quietly reintroducing the naive-fix trap, both still fail loudly with
///   nothing else touched.
/// - **The two-way equality below is checked first**, so a name added to
///   only one side is always caught with a precise diff, before the
///   coarser non-emptiness assert would even have a chance to fire.
#[test]
fn the_sized_sweep_pin_names_exactly_the_batteries_marked_for_a_bounded_panel() {
    let pinned = sized_filter_names();
    let sized = sized_sweep_heavy_tests();

    assert!(
        !sized.is_empty(),
        "found no heavy/probe test carrying {SIZED_SWEEP_MARKER:?} alongside a \
         {SWEEP_CALL:?} call. Either the marker was renamed (update \
         SIZED_SWEEP_MARKER) or this guard is now asserting nothing — which is \
         the one outcome it must never quietly reach. (This assert was \
         deliberately absent while the class was empty by design — see this \
         test's doc comment for when and why it was added.)"
    );
    assert_eq!(
        pinned, sized,
        "\n{NEXTEST_CONFIG}'s sized-sweep filter and the set of heavy/probe \
         batteries marked {SIZED_SWEEP_MARKER:?} have diverged.\n  pinned in \
         config: {pinned:?}\n  marked in source: {sized:?}\nIf a name is \
         pinned but not found in source, check whether its `{SWEEP_CALL:?}` \
         call lives in a HELPER defined above the test rather than in the \
         test body itself — `map_seeds_callers` cannot see it there (see that \
         function's own doc comment), and inlining the call into the test is \
         the fix, NOT dropping the name from the filter: the name is real and \
         still needs its reservation. Only once the call is confirmed gone \
         (or never existed) does dropping it apply. Otherwise add the missing \
         name(s) to the `filter = ` line in the `# class: sized-sweep` table. \
         Left alone this does NOT redden on its own: nextest either schedules \
         an unpinned sized-sweep battery without any reservation at all, or \
         keeps reserving slots for a battery that no longer needs them."
    );
}

/// Every test that reserves the whole runner must also be front-loaded, and
/// nothing else may be.
///
/// PRECISELY WHAT IS CHECKED, SO THIS CANNOT BE MISREAD AS EITHER MORE OR
/// LESS THAN IT IS: `front_loaded_filter_names()` only admits a table into
/// its roster if it carries a live `priority = ` SETTING line — a
/// `# class: front-loaded` marker COMMENT alone gets the table rejected by
/// `pinned_filter_names_for_class` before this test ever runs (see
/// [`PRIORITY_SETTING_PREFIX`]). So PRESENCE of a `priority` setting on
/// every whole-runner test IS checked, both by construction (a table
/// without one cannot contribute to `front_loaded`, so it would make
/// `reserving` and `front_loaded` diverge) and directly. What is NOT
/// checked is the priority VALUE — this does not assert `100`, or any
/// particular number, or that a lower-priority test hasn't been given a
/// HIGHER one; it asserts set EQUALITY between the `threads-required =
/// "num-cpus"` roster and the roster of tables that live-set `priority`.
///
/// WHY: a `threads-required = "num-cpus"` test that is not front-loaded makes
/// nextest drain the entire runner mid-run and restart the remainder cold.
/// Measured on the canonical box, twice, at different SHAs: a 484-485 s
/// barrier tax on a 1551 s tier.
#[test]
fn every_whole_runner_test_is_front_loaded() {
    let reserving = whole_runner_filter_names();
    let front_loaded = front_loaded_filter_names();

    assert!(
        !reserving.is_empty(),
        "found no test reserving the whole runner in {NEXTEST_CONFIG}. Either \
         the roster emptied (then this guard asserts nothing) or the key was \
         renamed — the one outcome it must never quietly reach."
    );
    assert_eq!(
        reserving, front_loaded,
        "\n{NEXTEST_CONFIG}: the whole-runner roster and the front-loaded \
         roster have diverged.\n  reserves the runner: {reserving:?}\n  \
         front-loaded:        {front_loaded:?}\nA reserving test that is not \
         front-loaded costs a full drain plus a cold restart of everything \
         scheduled after it."
    );
}
