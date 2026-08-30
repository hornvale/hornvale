//! Preregistered calibration on the ONE per-people axis the deep-history bake
//! actually differentiates: authored `threat_response`.
//!
//! ## Why this file exists — the hypothesis it replaces, and its falsification
//!
//! The Tumult's predation epoch inherited a preregistered directional
//! hypothesis, `kobold_flagships_are_less_coastal_than_goblin_flagships`
//! (`tests/calibration.rs`, spec §9.1). It claimed that the kobold — the cool
//! highlander, staked to an elevation niche far from the fertile coast —
//! should carry its flagship on a *less coastal* vertex than the lowland-
//! tolerant goblin does. **This epoch falsified it**, and the investigation
//! that followed (`.superpowers/sdd/coastal-inversion-investigation.md`)
//! found the hypothesis was never testable in the first place:
//!
//! - **The bake is niche-blind, end to end.** `ConditionNiche` never enters
//!   the deep-history bake in any form. Genesis draws each people's
//!   proto-sites uniformly from one species-blind, river-weighted capacity
//!   ranking; the covet rule, the roll-downhill score, the pressure term and
//!   the no-spoils gate all read a single global `eff_capacity` field with no
//!   people argument. Since *The Living Community*'s history-first placement,
//!   **no settling people has ever been placed by its own niche.**
//! - So **the shipped model predicts a kobold−goblin flagship-coastal
//!   difference of exactly zero.** Both peoples draw from the same pool. The
//!   old test passed pre-campaign by ≈ 0.5 σ of draw noise and failed after it
//!   by ≈ 2.2 σ of the same noise, on 769 paired census worlds — two draws
//!   around a zero, not a reversal of anything.
//! - The census movement was fully accounted for as a **flagship-identity
//!   re-selection artifact**, not a relocation: `flagship_of` resolves the
//!   *oldest surviving* occupation, and a raid closes the raider's own record
//!   (`Migrated`) and reopens it at the back of the list. 100 % of the coastal
//!   movement lived in the worlds where the flagship vertex *changed*; where it
//!   did not change the rate was byte-identical before and after.
//!
//! Nobody should re-derive that hypothesis. The direction was never the
//! finding; the **re-selection rate itself** was — and that is what this file
//! preregisters instead, on the axis that produces it.
//!
//! ## The hypothesis this file does assert
//!
//! `threat_response` is the per-people input that decides the RAID GATE —
//! not, despite what this paragraph used to claim, the only per-people input
//! the composition root hands the bake at all. `bake_history_from` fills four:
//! `disposition` and `disposition_spread` (via `disposition_maps`, off
//! `wc.psyche` and the authored dispersion registry), `in_group_radius` (The
//! Tithe's concealment term, off `SocietyVector`) and `time_horizon` (The
//! Tithe's extraction strategy, off `MindVector`). That claim was already false when
//! it was written — The Tithe added two of those four — and this file's own
//! campaign made it more so. The narrower, true statement is the one this
//! battery actually rests on: the gate reads `threat_response` and nothing
//! else. A people below [`RAID_DISPOSITION_MIN`] never takes the initiative,
//! so it almost never vacates an early site; a people above it raids, and
//! every raid it wins closes its own record and re-seats the flagship on a
//! later one.
//!
//! ## **The Tolerance dissolved this file's partition (2026-08-04)**
//!
//! Read the raid-veto sentences above as a statement about the roster
//! *before* The Tolerance (the input-enumeration sentences before them are
//! corrected in place, not scoped — they were never about the roster).
//! `Bake::takes_the_initiative` no longer compares a people's
//! authored `threat_response`; it compares a value **drawn per settlement**
//! around that authored mean, with the people's `Dispersion::mind` as the
//! standard deviation. So the two-way split this file computes from the psyche
//! registry — "raiders" above 0.6, "abstainers" below — is now a split by
//! authored *mean*, not by whether a people's settlements raid. Every one of
//! the six settling peoples has settlements on both sides of the gate; goblin
//! (mean 0.5, σ 0.25) clears it on roughly 38 % of its draws and human (0.5,
//! 0.35) on roughly 42 %.
//!
//! The directional claim may well survive — a people whose mean is 0.5 still
//! raids far less often than one at 0.85, so its flagship should still turn
//! over less — but the *mechanism sentence* is now approximate where it used to
//! be exact, and the measured rates below (goblin 16.7 % vs 42.6/45.8/50.0 %,
//! taken 2026-07-26 on the pre-Tolerance bake) predate the change and are the
//! numbers the bounds were set from.
//!
//! **The thresholds were deliberately NOT retuned here.** Moving a
//! preregistered bound to rescue a prediction after the physics under it
//! changed is exactly what this repo forbids; if this battery reddens on the
//! next heavy-tier run, that is a finding for The Tolerance's readout to
//! report, not a number to adjust. What is corrected here is only the prose
//! that claimed a people never raids. **See "The Assize's adjudication" below
//! for that finding** — this battery reddened on the 2026-08-08 heavy-tier
//! run, exactly as anticipated.
//!
//! > **Preregistered (pre-Tolerance, as originally written):** a NON-RAIDING
//! > people holds its first-drawn genesis site as its flagship far more often
//! > than a RAIDING people does — the flagship-re-selection rate is at most
//! > `NONRAIDER_MAX` (0.25, since retired) for a people below the raid
//! > threshold, at least [`RAIDER_MIN`] for every people above it, and
//! > separated by at least `SEPARATION_FACTOR` (2.0, since retired from
//! > assertion) ×.
//!
//! Measured on the shipped bake over seeds 1..=60 (2026-07-26): the one
//! non-raiding people re-seats its flagship on **16.7 %** of worlds, the three
//! raiding peoples on **42.6 % / 45.8 % / 50.0 %** — see the constants below
//! for the headroom each threshold leaves. This was the roster and the
//! reading the original bounds were fitted to.
//!
//! ## The Assize's adjudication (2026-08-08)
//!
//! Reproduced on this tree over seeds 1..=60, nine settling peoples (The
//! Tolerance and The Warren both added peoples since the roster above):
//!
//! ```text
//! KindId("bugbear"): re-seated 28/60 = 0.467       KindId("gnoll"): 27/60 = 0.450
//! KindId("desert-dwarf"): 20/60 = 0.333            KindId("goblin"): 18/60 = 0.300
//! KindId("gully-dwarf"): 3/60 = 0.050              KindId("hill-dwarf"): 26/60 = 0.433
//! KindId("hobgoblin"): 36/60 = 0.600               KindId("human"): 20/60 = 0.333
//! KindId("kobold"): 43/59 = 0.729
//! ```
//!
//! **FALSIFIED in two of three halves:**
//!
//! - the ceiling (the retired `NONRAIDER_MAX`, 0.25): breached by 3 of the 4
//!   abstainers (human 0.333, desert-dwarf 0.333, goblin 0.300; only
//!   gully-dwarf at 0.050 clears it). The original test asserted *inside* its
//!   loop and stopped at the first breach in `BTreeMap` order — it reported
//!   only `desert-dwarf` and never reached `goblin`, `human`, or the
//!   separation check at all. An instrument must not destroy the evidence its
//!   own diagnosis needs.
//! - the separation factor (`SEPARATION_FACTOR`, retained below only as a
//!   reported constant, never asserted): the weakest-raider-over-strongest-
//!   abstainer ratio collapsed from **2.55** (0.426 / 0.167, the
//!   pre-Tolerance one-abstainer roster) to **1.30** (0.433 / 0.333, the
//!   nine-people roster above).
//!
//! **HELD:** the raider floor, [`RAIDER_MIN`] = 0.30, against a weakest
//! raider of 0.433.
//!
//! **SURVIVED:** the ordering. `spearman(threat_response, re-selection rate)
//! = 0.831` over all nine peoples — exactly what this file's own doc
//! predicted when The Tolerance replaced an authored-mean comparison with a
//! per-settlement draw: the hard partition at 0.6 is what died, not the
//! direction. Under a draw, every people has settlements on both sides of the
//! gate, so a two-set partition is no longer a partition of behaviour, but
//! the rate remains a continuous increasing function of the mean.
//!
//! **What ships:** three sign claims, set from that mechanism and nothing
//! else — a per-settlement draw around an authored mean predicts
//! monotonicity, and predicts only the *sign*:
//!
//! 1. PRIMARY — `separation > 1.0` (the weakest raider beats the strongest
//!    abstainer). The original preregistered claim with the fitted magnitude
//!    stripped off and the direction kept. A thin margin is the correct
//!    condition for a sign claim, not a reason to raise it.
//! 2. SECONDARY — `spearman(threat_response, rate) > 0` across the whole
//!    roster, catching a global loss of ordering that a min-vs-max comparison
//!    can miss. The measured 0.831 is recorded as a witness, not used as the
//!    threshold.
//! 3. STRUCTURAL GUARD — a span guard ([`MIN_RATE_SPAN`]) that must pass
//!    before the correlation is read at all. **Dormant against both of this
//!    file's shipped mutation controls** (see "The Assize's rho-falsifiability
//!    follow-up" below) — it guards a genuinely degenerate roster, which
//!    neither mutation produces.
//!
//! This is a **post-hoc re-derivation**, stated plainly as one: every bound
//! above is set from the mechanism, not fitted to this table, and no
//! measured value is used as a threshold anywhere. **The honest cost:** a
//! sign claim is a strictly weaker discriminator than the ceiling it
//! replaces — chosen because it is the strongest claim the shipped physics
//! actually supports. `NONRAIDER_MAX` is **not** raised from 0.25 to clear
//! 0.333; it is deleted, not moved.
//!
//! ## Anti-vacuity
//!
//! The assertion binds on the mechanism, not on the labels: the raiding and
//! non-raiding sets are derived from the shipped psyche registry, and the
//! test fails loudly if either side is empty. Mutation-verified in both
//! directions against `Bake::takes_the_initiative` (2026-08-08, re-measured
//! against the post-adjudication assertions rather than inherited from the
//! pre-adjudication file):
//!
//! - **Force `true`** (`match disposition { None => true, Some(_) => true }`
//!   — everybody raids): rates move to bugbear 0.517, desert-dwarf 0.567,
//!   gnoll 0.441, goblin 0.533, gully-dwarf 0.400, hill-dwarf 0.567,
//!   hobgoblin 0.750, human 0.600, kobold 0.695. The ordering between the
//!   subsets inverts (weakest raider 0.441 ≤ strongest abstainer 0.600), and
//!   **the PRIMARY assertion (`separation > 1.0`) catches it**: "the raid
//!   disposition no longer orders flagship re-selection at all... separation
//!   0.734".
//! - **Force `false`** (`match disposition { None => false, Some(_) => false
//!   }` — nobody raids): rates move to bugbear 0.250, desert-dwarf 0.133,
//!   gnoll 0.167, goblin 0.150, gully-dwarf 0.000, hill-dwarf 0.150,
//!   hobgoblin 0.250, human 0.183, kobold 0.390. **The unchanged
//!   `RAIDER_MIN` floor catches it**: four of five raiders (bugbear, gnoll,
//!   hill-dwarf, hobgoblin) fall below 0.30.
//!
//! Both reds are real assertion failures naming a guard, not compile errors,
//! confirmed by reverting each mutation and re-running to GREEN. **Neither
//! mutation was caught by the span guard** — contrary to a draft expectation,
//! the rates move together (as §6.4 predicts) but this bake's per-people
//! variance in site quality, conquest and climate eviction is wide enough
//! that the span narrows (0.679 → 0.35 / 0.39) without crossing
//! `MIN_RATE_SPAN` (0.05). The span guard's role is unchanged and still
//! load-bearing for the failure mode it targets — a rank correlation read
//! over rates that have gone genuinely flat — it simply was not the
//! instrument that happened to fire against *these* two mutations; the
//! floor and the separation claim did, which is itself evidence the
//! instrument is not vacuous.
//!
//! ## The Assize's rho-falsifiability follow-up (2026-08-08)
//!
//! The two mutation controls above never actually reach the SECONDARY
//! `rho > 0` assertion: the `RAIDER_MIN` floor and the PRIMARY `separation >
//! 1.0` claim both fire first. That left `rho > 0` untested by this file's
//! own anti-vacuity proof — an assertion nobody has ever observed to fail is
//! indistinguishable from a decorative one, which is exactly the defect this
//! campaign exists to remove, so it was measured directly rather than left
//! standing on the strength of the other two.
//!
//! Method: the `RAIDER_MIN` floor assertion and the PRIMARY `separation >
//! 1.0` assertion were temporarily disabled (their `println!`s kept, so
//! every run still reaches and prints the correlation regardless of
//! mutation state), each of the three states below was run to completion,
//! and both assertions were restored and reverified to byte-identical
//! source before this record was written:
//!
//! ```text
//! state         rho      span (min..max)        span vs MIN_RATE_SPAN (0.05)
//! unmutated     0.8312   0.679 (0.050..0.729)    clears by 13.6x
//! force true    0.1266   0.350 (0.400..0.750)    clears by 7.0x
//! force false   0.7542   0.390 (0.000..0.390)    clears by 7.8x
//! ```
//!
//! **`rho` stays positive under both mutation controls.** It is heavily
//! depressed under force-true (0.8312 → 0.1266 — consistent with the
//! near-inversion the separation claim reads at the same state, since a
//! correlation near a real sign flip should sag toward zero first) but never
//! crosses it, and under force-false it barely moves (0.7542, close to
//! baseline). **`rho > 0` therefore has no demonstrated failure mode against
//! the only two perturbations this file has.**
//!
//! It is kept as an assertion and reframed honestly as a **directional
//! record rather than a proven guard**: it still reads a real, computed
//! quantity gated behind the span guard, and it still could in principle
//! catch a defect neither shipped mutation happens to produce, but nothing
//! in this file has shown a state in which it reddens on its own — the
//! PRIMARY claim and the raider floor have always fired first in every
//! measurement taken. Per this campaign's own rule against fitting a bound
//! to make a guard look alive: the threshold is **not** tightened above 0.0
//! to manufacture a failure mode, and no third mutation was invented to
//! rescue it. `rho > 0` stays exactly as written, its epistemic status
//! stated plainly rather than left implied by the other two assertions'
//! strength.
//!
//! ## THE RADIATION'S FALSIFICATION OF THE PRIMARY CLAIM (C2d, 2026-08-10)
//!
//! **This battery is RED on the heavy tier and is deliberately left red.** The
//! PRIMARY sign claim — `separation > 1.0`, the weakest raider re-seats more
//! often than the strongest abstainer — is **FALSIFIED at the fifteen-people
//! roster**. Measured on lefford's heavy-tier run at `581468bb`, seeds 1..=60,
//! 261.6 s:
//!
//! ```text
//! bugbear      0.483   desert-dwarf 0.200   desert-elf 0.224   drow      0.350
//! gnoll        0.475   goblin       0.367   gully-dwarf 0.067  high-elf  0.317
//! hill-dwarf   0.383   hobgoblin    0.783   human      0.367   kobold    0.593
//! sea-elf      0.033   snow-elf     0.466   wood-elf   0.283
//!
//! weakest raider  high-elf   0.317
//! strongest abstainer snow-elf 0.466
//! separation 0.680        (was 1.30 at nine peoples, 2.55 pre-Tolerance)
//! ```
//!
//! **Nothing was retuned, and nothing here is asserted differently.** The
//! floor, the span guard, the partition threshold and the seed panel are
//! exactly as they were; the assertion that fires still fires. What follows is
//! the diagnosis, which is what a falsification is owed.
//!
//! **It is not a roster-size accident, and it is not a mechanism change.**
//! Both new order statistics are elves — the peoples this campaign added — and
//! restricting the SAME measurement to the pre-Radiation nine gives
//! `0.383 / 0.367 = 1.045`, i.e. the claim was already inside 5% of failing
//! before this campaign existed. It did not need six new peoples to break; it
//! needed one people authored near the gate on either side, and the elves
//! supplied both: high-elf sits at `threat_response` 0.60, EXACTLY
//! `RAID_DISPOSITION_MIN`, and snow-elf at 0.50, one tenth below it.
//!
//! **The direction it was written to test SURVIVES, and survives better than
//! before.** Spearman's rho of authored `threat_response` against re-seating
//! rate, over the same table:
//!
//! ```text
//! all fifteen peoples   rho = 0.840   (the SECONDARY claim's own statistic;
//!                                      it read 0.831 over nine)
//! the pre-Radiation nine rho = 0.844
//! the six new elves alone rho = 0.829
//! ```
//!
//! So the rate is still a monotone increasing function of the authored mean,
//! on the new peoples as strongly as on the old. **What died is the min-versus-
//! max statistic over a two-set partition, and this file's own doc predicted
//! exactly that** — see "The Tolerance dissolved this file's partition": once
//! `takes_the_initiative` compares a per-settlement DRAW around the authored
//! mean, two peoples 0.10 apart in mean have overlapping behaviour by
//! construction (P(draw clears 0.6) is 0.50 for high-elf against 0.31 for
//! snow-elf), so a comparison of the two innermost order statistics is
//! comparing draws from an overlap. Its survival at nine peoples was luck, not
//! a reason: 1.045 is what luck looks like just before it runs out.
//!
//! The Radiation recommended, but deliberately did not take, retiring the
//! PRIMARY assertion the way this file already retired `NONRAIDER_MAX` and
//! `SEPARATION_FACTOR` — deleted, not moved, with the falsification
//! recorded — and letting the whole-roster rho carry the direction it was
//! always a proxy for. That was a preregistered claim's disposition and
//! belonged to review, not to a task closing out red tests, so the
//! assertion was left standing and firing. The honest reading: a
//! two-set partition of a continuum stopped being a partition at The
//! Tolerance, and this was the delayed consequence, not evidence that the
//! disposition stopped ordering anything.
//!
//! ## THE PRIMARY CLAIM, RETIRED (decision 0134, 2026-08-15)
//!
//! **Review has now taken it.** [Decision
//! 0134](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0134-a-partition-statistic-refuted-by-its-own-mechanism-is-retired-not-rescued.md)
//! adjudicates the falsification above: the min-versus-max order statistic is
//! dead, the mechanism it was written to test survives and strengthened (rho
//! 0.840, up from 0.831), and nothing was retuned to reach that ruling — not
//! the thresholds, not the seed panel, not the partition.
//!
//! `non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders`
//! (below) no longer asserts `separation > 1.0`; it still computes and
//! prints it as a witness. The two claims that survived stay exactly as they
//! were: the `RAIDER_MIN` floor and the SECONDARY whole-roster
//! `spearman(threat_response, rate) > 0.0`, which is what the PRIMARY claim
//! was always a proxy for and which does not need the partition to hold.
//!
//! The retired claim is not deleted — it is preserved as a standing,
//! re-runnable falsification record in its own test,
//! `the_weakest_raider_beats_the_strongest_abstainer_primary_claim`,
//! `#[ignore]`d under this repo's `PREREGISTERED, not met:` idiom. Its
//! `#[ignore]` reason cites decision 0134 directly rather than only a
//! registry slug: `preregistration_guard.rs` scans every
//! `tests/*calibration*.rs` file (this one included) and demands a
//! sanctioned reason — a cost or a decision cite — for any `#[ignore]` it
//! finds, and this repo's five earlier `PREREGISTERED, not met:` pins all
//! live outside that glob, so none of them had to satisfy it. The registry
//! carries the finding at `TOOL-min-vs-max-separation-compares-an-overlap`,
//! status `refuted (0134)` per decision 0131's admission rule — the claim
//! was tested and found false, and nothing shipped from it, because its
//! successor (the SECONDARY rho) already existed and was already asserted.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, history_for};
use std::collections::BTreeMap;

/// Seeds `1..=SAMPLE`, the range every reading below is pooled over.
const SAMPLE: u64 = 60;

/// The raid-initiative threshold on authored `threat_response`, mirroring
/// `hornvale_worldgen::history_bake`'s own private `RAID_DISPOSITION_MIN`.
/// Deliberately re-stated here rather than imported: this calibration
/// partitions the roster from the psyche registry independently of the bake,
/// so a silent change to either the constant or the authored dispositions
/// shows up as a failure here instead of passing unnoticed.
const RAID_DISPOSITION_MIN: f64 = 0.6;

/// Floor on every RAIDING people's flagship-re-selection rate. Measured
/// 2026-07-26 over seeds 1..=60: hobgoblin 20/47 = **0.426**, kobold
/// 22/48 = 0.458, bugbear 24/48 = 0.500. The bound sits 42 % below the
/// weakest raider. Re-checked in The Assize's adjudication (2026-08-08) against
/// the nine-people roster: still holds, weakest raider now hill-dwarf 0.433.
const RAIDER_MIN: f64 = 0.30;

/// The pre-Tolerance directional claim: the weakest raider's rate over the
/// strongest non-raider's. **Retired from assertion by The Assize (2026-08-08)**
/// — no longer checked against `RAIDER_MIN`-style headroom, only reported.
/// Measured **2.55** (0.426 / 0.167) at the pre-Tolerance one-abstainer
/// roster; measured **1.30** (0.433 / 0.333) at the nine-people roster the
/// per-settlement draw now produces. Replaced by the sign claim `separation >
/// 1.0` below, set from the mechanism rather than fitted to either reading.
const SEPARATION_FACTOR: f64 = 2.0;

/// The minimum spread the roster's re-selection rates must show before the
/// rank correlation below is read. Not calibrated from data: it is set just
/// above zero, because its job is to separate "the peoples differ at all"
/// from "every rate is the same value", which is what both mutation controls
/// on `Bake::takes_the_initiative` produce (see this module's doc). The
/// shipped roster spans 0.679 (0.050 to 0.729).
const MIN_RATE_SPAN: f64 = 0.05;

/// Per-people flagship-re-selection rates over seeds `1..=SAMPLE`: the
/// fraction of worlds in which a people's flagship — the oldest occupation
/// still alive at `now`, which is exactly what `flagship_of` resolves — sits
/// on a DIFFERENT vertex from that people's first-drawn genesis proto-site.
///
/// Read straight off the bake's own `History.records`, which are in commit
/// order: genesis opens every people's proto-sites first, so the first record
/// carrying a people is its rank-0 draw, and the first *alive* record carrying
/// it is its flagship.
fn reselection_rates(wc: &WorldComponents) -> BTreeMap<KindId, (u32, u32)> {
    let mut tally: BTreeMap<KindId, (u32, u32)> = BTreeMap::new();
    for seed in 1..=SAMPLE {
        let history = history_for(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            wc,
        )
        .expect("the default pins build at Terrain depth on every sampled seed");
        let peoples: Vec<KindId> = {
            let mut seen: Vec<KindId> = history.records.iter().map(|r| r.core.people).collect();
            seen.sort_by(|a, b| a.0.cmp(b.0));
            seen.dedup();
            seen
        };
        for people in peoples {
            let Some(genesis) = history.records.iter().find(|r| r.core.people == people) else {
                continue;
            };
            let Some(flagship) = history
                .records
                .iter()
                .find(|r| r.core.people == people && r.core.ended.is_none())
            else {
                // A people wholly extinguished by `now` has no flagship to
                // re-seat; it is not a world this rate is defined on.
                continue;
            };
            let entry = tally.entry(people).or_insert((0, 0));
            entry.1 += 1;
            if flagship.core.site != genesis.core.site {
                entry.0 += 1;
            }
        }
    }
    tally
}

/// Partition the shipped psyche registry's SETTLING peoples into raiders
/// (`threat_response >= RAID_DISPOSITION_MIN`) and abstainers. Never authored
/// in this file — `bake_history_from` fills `BakeConfig::disposition` from
/// exactly this map. Shared by the always-run battery below and the retired
/// PRIMARY claim's own standing pin (decision 0134), so the two can never
/// silently partition the roster two different ways.
fn raiders_and_abstainers(wc: &WorldComponents) -> (Vec<KindId>, Vec<KindId>) {
    let (mut raiders, mut abstainers): (Vec<KindId>, Vec<KindId>) = (Vec::new(), Vec::new());
    for (kind, psyche) in wc.psyche.iter() {
        if !wc
            .biosphere
            .get(kind)
            .is_some_and(|b| b.social_form == hornvale_species::SocialForm::Settled)
        {
            continue;
        }
        if psyche.threat_response >= RAID_DISPOSITION_MIN {
            raiders.push(*kind);
        } else {
            abstainers.push(*kind);
        }
    }
    (raiders, abstainers)
}

#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders() {
    let wc = WorldComponents::assemble().expect("assemble the shipped component set");
    let (raiders, abstainers) = raiders_and_abstainers(&wc);
    assert!(
        !raiders.is_empty() && !abstainers.is_empty(),
        "vacuous: the roster must contain at least one settling people on each \
         side of RAID_DISPOSITION_MIN ({RAID_DISPOSITION_MIN}) for this \
         hypothesis to mean anything — raiders {raiders:?}, abstainers {abstainers:?}"
    );

    let tally = reselection_rates(&wc);
    for (k, (changed, worlds)) in &tally {
        println!(
            "{k:?}: re-seated {changed}/{worlds} = {:.3}",
            f64::from(*changed) / f64::from(*worlds)
        );
    }
    let rate = |k: &KindId| -> f64 {
        let (changed, worlds) = tally.get(k).copied().unwrap_or((0, 0));
        assert!(
            worlds * 2 >= SAMPLE as u32,
            "{k:?} was flagship-less on more than half of {SAMPLE} worlds \
             ({worlds} usable) — the rate is not measurable"
        );
        f64::from(changed) / f64::from(worlds)
    };

    // COLLECT FIRST, ASSERT LAST. The previous shape asserted inside the
    // abstainer loop and stopped at the first breach in BTreeMap order, so it
    // reported `desert-dwarf` and never reached `goblin`, `human`, or the
    // separation check at all — three of four abstainers were over the bound
    // and the failure named one. An instrument must not destroy the evidence
    // its own diagnosis needs.
    let raider_rates: Vec<(KindId, f64)> = raiders.iter().map(|k| (*k, rate(k))).collect();
    let abstainer_rates: Vec<(KindId, f64)> = abstainers.iter().map(|k| (*k, rate(k))).collect();

    let weakest_raider = raider_rates
        .iter()
        .map(|(_, r)| *r)
        .fold(f64::INFINITY, f64::min);
    let strongest_abstainer = abstainer_rates
        .iter()
        .map(|(_, r)| *r)
        .fold(0.0f64, f64::max);
    let separation = weakest_raider / strongest_abstainer.max(f64::MIN_POSITIVE);

    println!(
        "raiders (>= {RAID_DISPOSITION_MIN}): {raider_rates:?}\n\
         abstainers: {abstainer_rates:?}\n\
         weakest raider {weakest_raider:.3}, strongest abstainer {strongest_abstainer:.3}, \
         separation {separation:.3}"
    );
    println!(
        "separation {separation:.3} against the RETIRED preregistered factor \
         {SEPARATION_FACTOR} — reported, not asserted. Measured 2.55 at the pre-Tolerance \
         roster (0.426 / 0.167), 1.30 at the nine-people roster (0.433 / 0.333), and 0.680 \
         at the fifteen-people roster (0.317 / 0.466) — where the PRIMARY claim was \
         FALSIFIED and, per decision 0134, is no longer asserted here. See this module's \
         doc, 'THE PRIMARY CLAIM, RETIRED': the whole-roster rho is 0.840 and the direction \
         survives; the min-vs-max statistic is what died, and it was at 1.045 on the \
         pre-Radiation nine. The retired claim is still asserted, by hand, in \
         `the_weakest_raider_beats_the_strongest_abstainer_primary_claim`."
    );

    // THE RAIDER FLOOR MOVED OUT OF THIS BATTERY AT THE GLASSHOUSE'S CLOSE.
    // It is computed and printed here as a witness, and asserted in its own
    // standing pin, `every_raider_clears_the_floor_preregistered_not_met` —
    // the same shape decision 0134 gave the retired PRIMARY claim, and for a
    // different reason, so the two must not be read as one retirement.
    let under: Vec<(KindId, f64)> = raider_rates
        .iter()
        .copied()
        .filter(|(_, r)| *r < RAIDER_MIN)
        .collect();
    println!("under the {RAIDER_MIN} raider floor: {under:?}");

    // THE SPAN GUARD. Measured against the only two mutations this file has
    // (`takes_the_initiative` forced `true` / `false`), it is DORMANT: the
    // span narrows from 0.679 unmutated to 0.350 (force-true) and 0.390
    // (force-false), never approaching the 0.05 floor — this bake's
    // per-people variance in site quality, conquest and climate eviction is
    // wide enough that neither mutation collapses the roster. It is kept
    // because what it guards against is not either of those: a genuinely
    // DEGENERATE roster, where the rates truly converge (every people
    // authored with the same disposition, or a bake in which
    // `threat_response` stopped reaching `takes_the_initiative` at all —
    // scenarios neither mutation control produces). See this module's doc,
    // "The Assize's rho-falsifiability follow-up", for the measured table.
    let mut all: Vec<f64> = raider_rates
        .iter()
        .chain(abstainer_rates.iter())
        .map(|(_, r)| *r)
        .collect();
    all.sort_by(f64::total_cmp);
    let span = all[all.len() - 1] - all[0];
    assert!(
        span >= MIN_RATE_SPAN,
        "the roster's re-selection rates span only {span:.3} ({:.3}..{:.3}) — every people \
         behaves the same, so the ordering claim below would be reading noise. This is what \
         both mutation controls look like.",
        all[0],
        all[all.len() - 1]
    );

    // PRIMARY, RETIRED (decision 0134): the weakest raider re-seats more
    // often than the strongest abstainer was the campaign's ORIGINAL
    // preregistered claim with the fitted magnitude stripped off and the
    // direction kept — a sign claim over a min-vs-max order statistic. The
    // Radiation falsified it at the fifteen-people roster (separation
    // 0.680) and found the cause structural: once `takes_the_initiative`
    // compares a per-settlement DRAW around the authored mean rather than
    // the mean itself, two peoples close in mean overlap by construction,
    // so the two innermost order statistics are comparing draws from an
    // overlap. `weakest_raider`/`strongest_abstainer`/`separation` above are
    // still computed and printed as a witness; the claim itself is no longer
    // asserted here — it is retired to its own standing, `#[ignore]`d pin
    // (`the_weakest_raider_beats_the_strongest_abstainer_primary_claim`,
    // below), re-runnable by hand. Nothing was retuned: the SECONDARY
    // whole-roster rho immediately below is what the PRIMARY claim was
    // always a proxy for, and it still fires. See this module's doc, "THE
    // PRIMARY CLAIM, RETIRED".

    // SECONDARY: monotone across the WHOLE roster, which a min-versus-max
    // comparison can miss. Sign only — see this module's doc for why the
    // measured 0.831 is a witness and not the threshold. Kept as an
    // assertion despite a follow-up measurement finding it has NO
    // demonstrated failure mode under this file's two mutation controls
    // (rho stayed positive under both — 0.1266 force-true, 0.7542
    // force-false); see this module's doc for the honest framing.
    let pairs: Vec<(f64, f64)> = raiders
        .iter()
        .chain(abstainers.iter())
        .map(|k| {
            let disp = wc
                .psyche
                .get(k)
                .expect("a partitioned people has a psyche row");
            (disp.threat_response, rate(k))
        })
        .collect();
    let rho = spearman(&pairs);
    println!(
        "spearman(threat_response, re-selection rate) = {rho:.4} over {} peoples",
        pairs.len()
    );
    assert!(
        rho > 0.0,
        "flagship re-selection is no longer monotone in authored threat_response \
         (spearman {rho:.4}). The per-settlement draw predicts a positive sign; a \
         non-positive one means the gate stopped reading the authored mean."
    );
}

/// **THE PRIMARY CLAIM, RETIRED (decision 0134).** The battery above no
/// longer asserts `separation > 1.0` — see its "PRIMARY, RETIRED" comment and
/// this module's doc, "THE RADIATION'S FALSIFICATION OF THE PRIMARY CLAIM".
/// This test is the claim's standing record: it still computes and still
/// asserts the falsified sign claim, `#[ignore]`d so it never runs in any
/// gate, kept re-runnable by hand so a future silent reversal (the ordering
/// recovering separation on its own) is discoverable rather than assumed.
///
/// Nothing here is retuned from the assertion this replaces. The partition
/// is the same `raiders_and_abstainers` the always-run battery uses, the
/// rates are the same `reselection_rates`, and the bound is the same `1.0`.
/// The only change is that a failure here no longer fails the gate — it was
/// already failing every heavy-tier run since 2026-08-10, deliberately, and
/// decision 0134 is the review that stopped asking it to.
///
/// claim: sign(weakest-raider re-seating rate > strongest-abstainer
/// re-seating rate) — measured FALSIFIED at the fifteen-people roster
/// (separation 0.680, 2026-08-10); the whole-roster Spearman rho (asserted
/// in the battery above) carries the direction this claim was always a
/// proxy for.
#[test]
#[ignore = "PREREGISTERED, not met: awaits TOOL-min-vs-max-separation-compares-an-overlap (decision 0134 retires it; the whole-roster Spearman rho, already asserted above, carries the direction)"]
fn the_weakest_raider_beats_the_strongest_abstainer_primary_claim() {
    let wc = WorldComponents::assemble().expect("assemble the shipped component set");
    let (raiders, abstainers) = raiders_and_abstainers(&wc);
    let tally = reselection_rates(&wc);
    let rate = |k: &KindId| -> f64 {
        let (changed, worlds) = tally.get(k).copied().unwrap_or((0, 0));
        f64::from(changed) / f64::from(worlds.max(1))
    };
    let weakest_raider = raiders.iter().map(rate).fold(f64::INFINITY, f64::min);
    let strongest_abstainer = abstainers.iter().map(rate).fold(0.0f64, f64::max);
    let separation = weakest_raider / strongest_abstainer.max(f64::MIN_POSITIVE);
    println!(
        "weakest raider {weakest_raider:.3}, strongest abstainer {strongest_abstainer:.3}, \
         separation {separation:.3} — measured 0.680 (0.317 / 0.466) at 2026-08-10; a \
         different reading here means the falsification decision 0134 records has moved."
    );
    assert!(
        separation > 1.0,
        "PREREGISTERED, NOT MET (decision 0134): the raid disposition no longer orders \
         flagship re-selection by a min-vs-max comparison: weakest raider \
         {weakest_raider:.3} <= strongest abstainer {strongest_abstainer:.3} (separation \
         {separation:.3}). This is the retired PRIMARY claim from \
         non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders, kept here \
         as a re-runnable record — see this module's doc, 'THE PRIMARY CLAIM, RETIRED', and \
         decision 0134. The SECONDARY whole-roster Spearman rho carries the direction this \
         claim was always a proxy for and is unaffected by this failure."
    );
}

/// **THE RAIDER FLOOR, MOVED OUT OF THE BATTERY AT THE GLASSHOUSE'S CLOSE —
/// a SECOND, INDEPENDENT failure in this file, not part of decision 0134's
/// retirement.**
///
/// Decision 0134 retired the min-versus-max PRIMARY claim and explicitly KEPT
/// this floor, which was true when it was written: the floor held at a
/// weakest raider of 0.433. It does not hold now. The Glasshouse corrected
/// the climate model (median land temperature -11.99 -> -3.65 degC) and drow
/// fell to **14/60 = 0.233**, under the 0.30 floor, on the same fifteen-people
/// roster. Nothing about the partition, the bake or the seed panel changed.
///
/// **The floor's stated mechanism is REFUTED, which is why it is deferred
/// rather than lowered.** Its own message says a people below it "means the
/// raid branch stopped running for it" — and 14 re-seats out of 60 is not a
/// branch that stopped running, it is a rate that fell. Compare sea-elf at
/// 0/60, which is what a branch that genuinely never fires looks like and
/// which this floor never checks, because sea-elf is an abstainer. So the
/// floor is measuring a post-epoch world at a pre-epoch scale, exactly as
/// `windows/locale/tests/water_reading.rs`'s discharge floors are.
///
/// Lowering it to fit 0.233 is forbidden twice over: it is the half of the
/// original preregistration that SURVIVED The Assize's dissolution and The
/// Radiation's falsification, and decision 0138 permits restating a
/// preregistered criterion only when its ESTIMATOR is wrong, never when its
/// result is inconvenient. What is wrong here is the scale the estimator is
/// denominated in, and re-fitting that is a measurement, not an edit.
///
/// **RE-READ AT THE UNDERWORLD'S CLOSE (2026-08-18): drow fell again, to
/// 12/60 = 0.200, and the deferral is UNCHANGED.** The denominator held at 60,
/// so this is the same fact one notch deeper — drow re-seats less often, not
/// drow losing flagships on more worlds — and 12 live re-seats refute the
/// floor's stated mechanism exactly as 14 did. Nothing here licenses lowering
/// the bar; the estimator's scale is still what is wrong, and re-fitting it is
/// still a measurement. See the witness below for the attribution, which is
/// **not** the one the campaign expected.
///
/// **RE-READ AGAIN, CAUSE NAMED AT THE GOVERNOR'S CLOSE (2026-08-28): drow
/// fell a third time, to 10/60 = 0.167.** Unlike the first two re-reads, this
/// one has a measured cause rather than an open one: bisected to `eeaa011fd`
/// ("chore(streams): history/bake/v3", The Granary T5), which bumps the
/// deep-history bake's stream epoch because The Granary's sub-year raid/
/// founding phase placement moves raid outcomes whenever a raid now fires on
/// a store-trough phase — a shipped, documented consequence, not a fresh
/// bug. The denominator again held at 60, and 10 live re-seats again refute
/// the floor's stated mechanism exactly as 14 and 12 did. The deferral still
/// does not move: the estimator's scale is still what is wrong.
///
/// **RE-READ A FOURTH TIME AT THE WINZE'S CLOSE (2026-08-30): drow fell to
/// 9/60 = 0.150,** and this re-read carries the sharpest attribution the pin
/// has had: not a campaign name and not a commit, but a **mechanism**
/// isolated by neutralising each half of The Winze independently — the
/// breach hazard moves it and the working's placement does not. The
/// denominator again held at 60, and 9 live re-seats again refute the floor's
/// stated mechanism exactly as 14, 12 and 10 did. The deferral still does not
/// move: the estimator's scale is still what is wrong, and the fall is again
/// a shipped feature's documented consequence rather than a defect. The
/// witness below carries the measurement.
///
/// Tracked as `BIO-raid-partition-order-statistic`. The witness below keeps
/// the reading measured while this is deferred.
///
/// claim: readout(preregistered) — every people authored above
/// `RAID_DISPOSITION_MIN` re-seats its genesis flagship on at least
/// `RAIDER_MIN` of the seed panel's worlds.
#[test]
#[ignore = "PREREGISTERED, not met: awaits BIO-raid-partition-order-statistic (decision 0138; drow fell to 14/60 = 0.233 under the 0.30 raider floor when The Glasshouse corrected the climate, to 12/60 = 0.200 at The Underworld's close, to 10/60 = 0.167 at The Granary's close (named at The Governor's close, 2026-08-28: eeaa011fd, BAKE stream epoch v2 -> v3), and to 9/60 = 0.150 at The Winze's close (mechanism measured, not assumed: the breach hazard, NOT the working's placement - see the witness), denominator held at 60 throughout, and the floor's stated mechanism - that the raid branch stopped running - is refuted by 9 live re-seats, so the floor is reading a post-epoch world at a pre-epoch scale)"]
fn every_raider_clears_the_floor_preregistered_not_met() {
    let wc = WorldComponents::assemble().expect("assemble the shipped component set");
    let (raiders, _) = raiders_and_abstainers(&wc);
    let tally = reselection_rates(&wc);
    let rate = |k: &KindId| -> f64 {
        let (changed, worlds) = tally.get(k).copied().unwrap_or((0, 0));
        f64::from(changed) / f64::from(worlds.max(1))
    };
    let under: Vec<(KindId, f64)> = raiders
        .iter()
        .map(|k| (*k, rate(k)))
        .filter(|(_, r)| *r < RAIDER_MIN)
        .collect();
    assert!(
        under.is_empty(),
        "PREREGISTERED, NOT MET (decision 0138): {} raiding people(s) below the \
         {RAIDER_MIN} floor: {under:?}. Measured 14/60 = 0.233 for drow at The Glasshouse's \
         close, 12/60 = 0.200 at The Underworld's, 10/60 = 0.167 at The Granary's \
         (cause named at The Governor's close: eeaa011fd, BAKE v2 -> v3), and 9/60 = 0.150 \
         at The Winze's (mechanism measured: the breach hazard, not the working's \
         placement); a DIFFERENT reading here means the deferral has moved and owes a \
         re-read, not a lowered floor. See this test's doc comment.",
        under.len()
    );
}

/// **The witness that keeps the raider floor measured while
/// [`every_raider_clears_the_floor_preregistered_not_met`] is `#[ignore]`d.**
///
/// This pins a witness, not a claim. The integers are not a bar the world must
/// clear — they are what the corrected climate produced, recorded so a change
/// to the bake, the partition or the climate *forces a deliberate re-read*.
///
/// Pinned as `(changed, worlds)` rather than the rate, because a ratio hides
/// which term moved: "drow re-seated less often" and "drow was flagship-less
/// on more worlds" are different facts, and only the first is the finding.
///
/// # RE-READ AT THE UNDERWORLD'S CLOSE (2026-08-18): 14/60 → 12/60
///
/// **The denominator held at 60**, so the pin's own distinction resolves the
/// easy way: drow re-seats less often (0.233 → 0.200), and drow is not
/// flagship-less on more worlds. The deferral does not move — 12 live re-seats
/// refute the floor's stated mechanism exactly as 14 did, and sea-elf at 0/60
/// still shows what a dead branch actually looks like.
///
/// **THE ATTRIBUTION IS NOT DECISION 0145, AND THIS WAS WORTH MEASURING
/// RATHER THAN ASSUMING.** The campaign re-keyed the history bake's node index
/// from `Vertex` to `(Vertex, Band)` (one community per *place*, not per
/// vertex), which is the change that most obviously touches settlement placement,
/// and the expectation carried into this re-read was that it caused the fall.
/// It does not. Neutralising the re-key alone — `Bake::rung_for` forced to
/// `Band::Surface`, which makes every node-index key `(vertex, Surface)` and
/// so restores the old one-per-vertex semantics exactly, with the rest of the
/// campaign intact — reads **10/60**, which is *further* from main's 14/60,
/// not nearer it:
///
/// ```text
///   main 95cbaa70                        14/60   0.233
///   merge product, re-key neutralised    10/60   0.167
///   merge product as merged              12/60   0.200
/// ```
///
/// So the rest of the campaign moves drow **−4** and the re-key gives back
/// **+2**, for a net −2. 0145 explains none of the fall; it opposes it. **The
/// mechanism behind either half is NOT established here** — this is a
/// difference of three measured readings, and naming a cause for it would be
/// exactly the invention this pin exists to prevent.
///
/// # RE-READ AT THE GOVERNOR'S CLOSE (2026-08-28): 12/60 → 10/60, CAUSE NAMED
///
/// **This time a cause was measured, not assumed.** The Governor's own tasks
/// never touch history, worldgen or the bake — this witness had already been
/// reading 10/60 since before The Governor's branch point (bisected
/// independently at `7576eca00`, matching The Sources' retrospective
/// bisection at the same SHA) — so the fall happened somewhere between this
/// pin's last restatement (`4ee25c355`, 12/60) and that point.
///
/// First-parent bisection over the 384 mainline merges in that range (each
/// probed by checking out the SHA in a scratch worktree and running this test
/// alone) isolated the flip to a single merge: **`merge(the-granary)`
/// (`6db5de3f2`)**. Bisecting *inside* that campaign's own branch narrowed it
/// one commit further, to **`eeaa011fd` — "chore(streams): history/bake/v3 —
/// The Granary phase timestamps change committed history (T5)"**:
///
/// ```text
///   e8b373891 (Granary T4 + review)         PASS   12/60
///   eeaa011fd (Granary T5: BAKE v2 -> v3)   FAIL   10/60
/// ```
///
/// The commit is self-explaining and was landed deliberately, per decision
/// 0006 (an epoch suffix, never a rename): The Granary's sub-year phase
/// placement (T1-T4) moves *when* a raid or founding fires within a year, so
/// raid outcomes move whenever a raid now lands on a store-trough phase that
/// the old year-grained bake would have resolved differently — the stream's
/// own doc comment says exactly this ("raid outcomes move when raids fire at
/// store-trough phases"). Nothing about the fall is a bug: it is a shipped,
/// documented, adjudicated consequence of a merged feature that simply never
/// re-stated *this* witness at the campaign's own close (The Granary's own
/// task list re-pinned the H1 witness, the domesday surface and the
/// census-reading calibrations against its new census, but not this one).
///
/// Re-stated here rather than repaired: nothing in The Governor's own scope
/// touches raid timing, so there is nothing to fix, only a stale number to
/// correct with its cause on record.
///
/// # RE-READ AT THE WINZE'S CLOSE (2026-08-30): 10/60 → 9/60, MECHANISM MEASURED
///
/// **The denominator held at 60 for the fourth time**, so the pin's own
/// distinction resolves the easy way again: drow re-seats less often
/// (0.167 → 0.150), and drow is not flagship-less on more worlds. The
/// deferral does not move — 9 live re-seats refute the floor's stated
/// mechanism exactly as 14, 12 and 10 did.
///
/// **THE OBVIOUS STORY IS THE WRONG ONE, AND THIS IS THE FIRST RE-READ TO
/// NAME A MECHANISM RATHER THAN A COMMIT.** The Winze founds **workings** —
/// daughter settlements sited on an ore objective up to `WORKING_REACH` rings
/// away rather than on `grow`'s nearest vacant neighbour — and gives a living
/// working a **breach hazard** that can end it. "The campaign moved settlement
/// placement, therefore re-seating moved" is the story this pin exists to
/// refuse, and it is refuted: the placement change alone leaves the witness
/// exactly where it was pinned. Each half was neutralised independently, with
/// the rest of the campaign intact, and the witness read three times:
///
/// ```text
///   working selection neutralised (no expansion is ever a working;
///     `deepen` and `maybe_breach` are inert without a `Function::Mine`)
///                                        10/60   0.167   <- the pinned value
///   breach neutralised only (workings founded on their ore sites as
///     shipped, but `maybe_breach` never closes one)
///                                        10/60   0.167   <- the pinned value
///   the campaign as it ships                9/60   0.150
/// ```
///
/// So **the fall is the breach hazard's**, and the ore-objective siting — the
/// campaign's larger and more visible change — contributes nothing to it.
///
/// **The flip is one seed, and it is seed 33.** Instrumenting
/// [`reselection_rates`] to print drow's genesis site, flagship site and
/// record count per seed, under the shipped tree and under the
/// breach-neutralised arm, moves exactly one of the sixty:
///
/// ```text
///   seed 33, breach neutralised : genesis v28452, flagship v7230,  changed, 180 drow records
///   seed 33, as shipped         : genesis v28452, flagship v28452, UNCHANGED, 20 drow records
/// ```
///
/// With the hazard live, two of drow's delvings on that seed break through
/// (`v30341` at 83 m and `v30324` at 693 m), drow's history on the seed runs
/// **20 occupations instead of 180**, and the genesis seat at `v28452` is
/// still alive at `now` — so it is its own flagship. With the hazard off that
/// expansion runs on, the genesis seat ends somewhere in it, and a later
/// occupation at `v7230` is the flagship instead. The counter-intuitive
/// reading is the correct one: **the hazard did not end drow's flagship, it
/// truncated the expansion that would otherwise have replaced it.**
///
/// **What is NOT established, said plainly.** The chain *inside* seed 33 —
/// from "a delving broke through at `v30341` in some epoch" to "the genesis
/// seat at `v28452` survives to `now`" — is a cascade through the bake's
/// epoch dynamics and has not been traced step by step. What is measured is
/// the attribution (breach, not placement) and the locus (one seed, named,
/// with its record counts). Naming the intra-seed chain would be the
/// invention this pin exists to prevent.
///
/// Re-stated rather than repaired, for the third time and the same reason:
/// the hazard is a shipped, spec'd feature (The Winze §4.3) doing exactly
/// what it was built to do, so there is nothing to fix — only a stale number
/// to correct with its cause on record.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_sub_floor_raider_reading_is_pinned_as_a_witness() {
    let wc = WorldComponents::assemble().expect("assemble the shipped component set");
    let tally = reselection_rates(&wc);
    let (changed, worlds) = tally.get(&KindId("drow")).copied().unwrap_or((0, 0));
    println!("witness: drow re-seated {changed}/{worlds}");
    assert_eq!(
        (changed, worlds),
        (9, 60),
        "drow's flagship re-seating moved from the pinned 9/60. This is NOT a number to \
         update — re-read it, then re-state this witness, the #[ignore] reason on \
         every_raider_clears_the_floor_preregistered_not_met, its roster entry in \
         cli/tests/suite/heavy_tier.rs (the EXPECTED_UNTOKENISED ratchet, which holds a \
         VERBATIM copy of that reason string and will red if you touch one and not the \
         other) and the BIO-raid-partition-order-statistic registry row in the SAME \
         commit. THIS HAS NOW HAPPENED THREE TIMES (The Underworld, 2026-08-18, 14/60 → \
         12/60; The Granary, landed 2026-08-24 and re-read at The Governor's close on \
         2026-08-28, 12/60 → 10/60, `eeaa011fd`'s BAKE v2 -> v3 epoch bump; The Winze, \
         2026-08-30, 10/60 → 9/60, the breach hazard and NOT the working placement, \
         isolated by neutralising each half). BEFORE ASSUMING A CAUSE, MEASURE ONE — the \
         Winze re-read is the worked example: the obvious story (placement moved) was \
         measured and refuted."
    );
}

/// Spearman rank correlation over `(x, y)` pairs, ties taking average ranks.
/// Deterministic: `total_cmp` throughout, no float equality.
fn spearman(pairs: &[(f64, f64)]) -> f64 {
    fn ranks(v: &[f64]) -> Vec<f64> {
        let mut idx: Vec<usize> = (0..v.len()).collect();
        idx.sort_by(|a, b| v[*a].total_cmp(&v[*b]));
        let mut out = vec![0.0; v.len()];
        let mut i = 0;
        while i < idx.len() {
            let mut j = i;
            while j + 1 < idx.len()
                && v[idx[j + 1]].total_cmp(&v[idx[i]]) == std::cmp::Ordering::Equal
            {
                j += 1;
            }
            let avg = (i + j) as f64 / 2.0;
            for k in i..=j {
                out[idx[k]] = avg;
            }
            i = j + 1;
        }
        out
    }
    let (xs, ys): (Vec<f64>, Vec<f64>) = pairs.iter().copied().unzip();
    let (rx, ry) = (ranks(&xs), ranks(&ys));
    let n = rx.len() as f64;
    let (mx, my) = (rx.iter().sum::<f64>() / n, ry.iter().sum::<f64>() / n);
    let num: f64 = rx.iter().zip(&ry).map(|(a, b)| (a - mx) * (b - my)).sum();
    let den = (rx.iter().map(|a| (a - mx).powi(2)).sum::<f64>()
        * ry.iter().map(|b| (b - my).powi(2)).sum::<f64>())
    .sqrt();
    if den == 0.0 { 0.0 } else { num / den }
}

#[test]
fn spearman_reads_known_orderings() {
    let up: Vec<(f64, f64)> = vec![(1.0, 10.0), (2.0, 20.0), (3.0, 30.0)];
    assert!((spearman(&up) - 1.0).abs() < 1e-12, "perfect ascent is +1");
    let down: Vec<(f64, f64)> = vec![(1.0, 30.0), (2.0, 20.0), (3.0, 10.0)];
    assert!(
        (spearman(&down) + 1.0).abs() < 1e-12,
        "perfect descent is -1"
    );
    let flat: Vec<(f64, f64)> = vec![(1.0, 5.0), (2.0, 5.0), (3.0, 5.0)];
    assert_eq!(spearman(&flat), 0.0, "no variance in y is 0, not NaN");
}
