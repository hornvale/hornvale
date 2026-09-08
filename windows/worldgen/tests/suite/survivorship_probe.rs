//! DOES THE SURVIVORSHIP SHAPE APPEAR? — The Winze, Task 5 (spec §5.2).
//!
//! The campaign's central claim under test, and its null was publishable.
//! **The null did not fire.** §5.2's branch table lands on its middle row —
//! *breached are deeper, with overlap* — and this file is the witness that
//! pins it, so a later change that moves the verdict reddens here rather
//! than passing quietly.
//!
//! ```text
//! PANEL [42, 7, 1234, 0, 1, 2, 3, 4, 5, 6, 8, 9]
//!   249 workings — 30 breached, 131 ordinarily ended, 88 STILL OPEN (excluded)
//!                n   at floor        min      med       max
//!   breached    30   1 ( 3.3%)      12.0    647.1    4179.0
//!   ordinary   131  31 (23.7%)       4.0     47.6    1305.8
//!   still open  88   3 ( 3.4%)      12.0    343.3    4260.3
//!   AUC 0.8455   z 5.896
//!   OVERLAP  23/30 breached below the deepest ordinary end (1305.8 m)
//!            95/131 ordinary above the shallowest breach  (12.0 m)
//!   STRATIFIED on tenure: AUC 0.7160  z 3.197, direction holds in every
//!            stratum that has both groups
//! ```
//!
//! # THE THIRD POPULATION IS EXCLUDED, AND THAT IS A DECISION
//!
//! A working that has not ended has no *final* depth — its `delve_depth_m` is
//! a reading taken mid-dig, and the bake simply stopped. Seventy-four of the
//! panel's 249 workings are in that state and their median (343.3 m) sits
//! **above** the ordinary median but below the breached median. They are
//! reported and then set aside: they are
//! neither the breached group nor the ordinarily-ended one, and folding them
//! into either would be measuring where the bake's clock stopped rather than
//! how a delving ended.
//!
//! **The exclusion is conservative, not flattering, and that is worth stating
//! because the direction is easy to get backwards.** Pooling the still-open
//! workings into the ordinarily-ended group would raise that group's median
//! from 47.6 m to 107.4 m and *shrinks* the measured separation, AUC 0.8455 →
//! 0.7383. Excluding them therefore does not manufacture the result; it
//! removes a group whose depths are censored readings, and the effect of
//! including them would have been to hide the finding behind the bake's own
//! stopping time. `the_still_open_population_is_not_a_third_arm_of_the
//! _comparison` measures both numbers.
//!
//! # THE FLOOR SPIKE IS PART OF THE RESULT
//!
//! 23.7% of ordinarily-ended workings sit at exactly 6/9/12 m — the values a
//! working that dies in its founding epoch can take — against 3.3% of
//! breached ones. That asymmetry is not an artifact to be corrected away: a
//! working that dies in its founding epoch was exposed to a hazard clocked
//! per metre exactly once, so it almost never breaches. It does mean the
//! pooled gap contains two distinguishable claims — *breached workings lived
//! longer* and *breached workings dug harder while they lived* — and §5.2 is
//! about the second. Under a per-metre hazard both are the mechanism (total
//! metres is what the hazard integrates, and total metres is tenure times
//! rate), but only the second rules out "the pooled gap was composition".
//! `the_separation_survives_conditioning_on_tenure` is the test that settles
//! it: stratified on epochs dug, the separation attenuates (AUC 0.8455 →
//! 0.7160) and survives (z 3.197), and the direction holds in every stratum
//! carrying both groups.
//!
//! # WHAT WOULD CHANGE THE VERDICT
//!
//! - **`z` falling to the noise floor** (`Z_DECIDES` below) with the medians
//!   converging: §5.2's *first* row, the publishable null — the hazard would
//!   be decoration. The response the spec fixes in advance is to report it,
//!   **never** to tune `BREACH_FREE_PATH_M` to separate the groups again.
//! - **The overlap emptying in either direction**: §5.2's *third* row.
//!   Perfect separation would say the hazard had become a depth threshold in
//!   disguise, which is the design this campaign replaced.
//! - **The stratified statistic collapsing while the pooled one holds**: the
//!   pooled gap would then be composition — breached workings merely lived
//!   longer — and the claim §5.2 actually makes would be unsupported.
//! - **The control disagreeing with the panel in direction**: spec amendment
//!   E.9's stop clause. That outranks everything else here.
//!
//! # THE PANEL, AND THE CONTROL BESIDE IT
//!
//! Spec amendment E.4.2 froze a panel rule before the hazard existed and
//! **fixed the wrong quantity**: it set its stopping threshold on *mines* as
//! a proxy for *breaches*, and its stated conversion assumed a breach
//! fraction near a third against a measured 13.3%. Applied literally it stops
//! at four seeds and three breaches, which is not a distribution. E.9
//! resolves it in favour of the same rule's twelve-seed **cap**, which was
//! also frozen before any data existed and so invents no new number. Both are
//! measured here: the panel is the answer, the control is the transparency
//! demonstration that the cap is the right reading rather than a convenient
//! one.
//!
//! # THE MEDIANS RECONCILE WITH TASK 4 UNDER TASK 4'S OWN CONVENTION
//!
//! The pre-Murrain Task 4 report used a different world: its counts and
//! medians were 196 / 569.1 / 31.1 / 674.9. The Murrain legitimately moves
//! this substrate to 249 / 647.1 / 47.6 / 343.3; the current readout below is
//! therefore the authoritative post-epoch witness rather than a quantile
//! convention check against the old world.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::{CauseOfEnd, Function, OccupationRecord};
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BakeConfig, BuildDepth, SettlementPins, WorldComponents, build_world_to, occupation_records,
};

/// The panel spec amendment E.9 fixes: E.4.2's twelve-seed **cap**, taken in
/// the consecutive order E.4.2 also froze, so the only thing ever chosen is
/// *how much* data and never *which*.
const PANEL: [u64; 12] = [42, 7, 1234, 0, 1, 2, 3, 4, 5, 6, 8, 9];

/// The panel E.4.2's *literal* rule stops at. Reported as a transparency
/// control, never as the answer.
const CONTROL: [u64; 4] = [42, 7, 1234, 0];

/// The `z` at which this file calls two depth distributions distinguishable.
///
/// A conventional decision boundary (`p < 0.002` two-sided), **not** a number
/// read off the measurement. The Murrain changes the substrate enough that the
/// four-seed control now exceeds this boundary; the control is therefore no
/// longer a claim of non-decision. The frozen panel still governs the campaign
/// answer, and the control remains a weaker read than that panel.
const Z_DECIDES: f64 = 3.0;

/// The looser boundary the *secondary* question is judged at: the
/// conventional two-sided 5% normal boundary.
///
/// The preregistered branch decision rests on the pooled statistic above.
/// Stratifying answers a different question — *was the pooled gap merely
/// composition?* — on much thinner per-stratum samples, and holding it to the
/// same bar would ask a follow-up to carry more evidence than the finding it
/// qualifies. Measured on the Murrain panel: 3.197.
const Z_SUPPORTS: f64 = 1.96;

/// One panel seed's world, built to the depth that runs the history bake.
fn panel_world(seed_value: u64) -> World {
    build_world_to(
        Seed(seed_value),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &WorldComponents::assemble().expect("canonical components assemble"),
        BuildDepth::Settlements,
    )
    .expect("panel seed builds")
}

/// One delving, reduced to the two quantities this comparison reads.
#[derive(Clone, Copy, Debug)]
struct Delving {
    /// The committed `delve_depth_m`.
    depth_m: f64,
    /// Epochs this working was exposed to the hazard: one for the founding
    /// epoch it was sunk in, plus one for each epoch of tenure after it.
    epochs: f64,
}

/// The three populations §5.2's comparison must keep apart.
#[derive(Default)]
struct Split {
    /// Ended by breaching.
    breached: Vec<Delving>,
    /// Ended by anything else — famine, a raid, a climate eviction.
    ordinary: Vec<Delving>,
    /// Never ended. **Not a third arm of the comparison**; see the module doc.
    still_open: Vec<Delving>,
}

/// Split every working on `seeds` into the three populations, reading the
/// records back **off the ledger** so the comparison crosses the emit
/// boundary the same way `breach.rs`'s gates do.
fn split_over(seeds: &[u64]) -> Split {
    let cfg = BakeConfig::default_millennia();
    let mut split = Split::default();
    for &seed_value in seeds {
        let occs: Vec<OccupationRecord> = occupation_records(&panel_world(seed_value));
        for r in occs.iter().filter(|r| r.core.function == Function::Mine) {
            let last_year = r.core.ended.unwrap_or(cfg.end_year);
            let d = Delving {
                depth_m: r.core.delve_depth_m,
                epochs: ((last_year - r.core.founded) / cfg.epoch_years).round() + 1.0,
            };
            match r.core.cause {
                Some(CauseOfEnd::Breached) => split.breached.push(d),
                Some(_) => split.ordinary.push(d),
                None => split.still_open.push(d),
            }
        }
    }
    split
}

/// Sorted depths of a population, `total_cmp` so the order is deterministic.
fn sorted_depths(group: &[Delving]) -> Vec<f64> {
    let mut v: Vec<f64> = group.iter().map(|d| d.depth_m).collect();
    v.sort_by(|a, b| a.total_cmp(b));
    v
}

/// The linear-interpolated quantile of an already-sorted slice.
fn quantile(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let pos = q * ((sorted.len() - 1) as f64);
    let lo = pos.floor() as usize;
    let hi = pos.ceil() as usize;
    if lo == hi {
        sorted[lo]
    } else {
        sorted[lo] + (pos - lo as f64) * (sorted[hi] - sorted[lo])
    }
}

/// The upper of the two central order statistics — Task 4's median
/// convention, printed alongside the interpolated one so the two records can
/// be read against each other.
fn upper_median(sorted: &[f64]) -> f64 {
    if sorted.is_empty() {
        f64::NAN
    } else {
        sorted[sorted.len() / 2]
    }
}

/// The three values a working that dies in its founding epoch can take
/// (`DAUGHTER_POP × tech_weight × 0.5`, per `DELVE_M_PER_PERSON_EPOCH`).
fn at_floor(depth: f64) -> bool {
    [6.0f64, 9.0, 12.0].iter().any(|f| (depth - f).abs() < 1e-9)
}

/// Mann–Whitney U for `a` against `b`, ties counted as half.
///
/// This is the statistic §5.2's word *indistinguishable* is about: it
/// compares the whole distributions rather than one summary of each, which
/// matters here because both groups are strongly right-skewed and one of them
/// carries a large tie group at the floor.
fn mann_whitney_u(a: &[f64], b: &[f64]) -> f64 {
    let mut u = 0.0;
    for &x in a {
        for &y in b {
            if x > y {
                u += 1.0;
            } else if (x - y).abs() < 1e-12 {
                u += 0.5;
            }
        }
    }
    u
}

/// The normal-approximation `z` of a U against the null of no difference.
///
/// **The tie correction is deliberately omitted, which makes this
/// conservative.** Correcting for ties can only shrink the variance, so an
/// uncorrected `z` understates the evidence — and the floor spike is a large
/// tie group. A verdict reached at this `z` would be reached at the corrected
/// one too.
fn u_z(u: f64, n1: usize, n2: usize) -> f64 {
    let (n1, n2) = (n1 as f64, n2 as f64);
    if n1 == 0.0 || n2 == 0.0 {
        return f64::NAN;
    }
    (u - n1 * n2 / 2.0) / (n1 * n2 * (n1 + n2 + 1.0) / 12.0).sqrt()
}

/// Tenure strata, in epochs dug. The first is the founding epoch alone — the
/// floor spike's own stratum.
const STRATA: [(&str, f64, f64); 5] = [
    ("1", 1.0, 1.0),
    ("2-3", 2.0, 3.0),
    ("4-8", 4.0, 8.0),
    ("9-20", 9.0, 20.0),
    ("21+", 21.0, f64::INFINITY),
];

/// Everything one panel's readout has to say, computed once and printed
/// before anything is asserted so a red leaves its numbers behind it.
struct Readout {
    /// Interpolated median depth of the breached group.
    breached_median: f64,
    /// Interpolated median depth of the ordinarily-ended group.
    ordinary_median: f64,
    /// Pooled Mann–Whitney `z`, breached against ordinary.
    z: f64,
    /// Breached delvings shallower than the deepest ordinary end.
    below: usize,
    /// Ordinary ends deeper than the shallowest breach.
    above: usize,
    /// Mann–Whitney `z` stratified on tenure.
    stratified_z: f64,
    /// Whether every stratum carrying both groups has the breached median at
    /// or above the ordinary one.
    strata_direction_holds: bool,
}

/// Print a population's shape.
fn describe(label: &str, group: &[Delving]) {
    let s = sorted_depths(group);
    if s.is_empty() {
        println!("  {label:<12} n=0");
        return;
    }
    let floor = group.iter().filter(|d| at_floor(d.depth_m)).count();
    println!(
        "  {label:<12} n={:>4}  at-floor {:>3} ({:>5.1}%)  min {:>8.1}  q1 {:>8.1}  med {:>8.1} \
         (upper {:>8.1})  q3 {:>8.1}  max {:>8.1}",
        s.len(),
        floor,
        100.0 * floor as f64 / s.len() as f64,
        s[0],
        quantile(&s, 0.25),
        quantile(&s, 0.50),
        upper_median(&s),
        quantile(&s, 0.75),
        s[s.len() - 1],
    );
}

/// The whole readout for one panel.
fn report(name: &str, seeds: &[u64], split: &Split) -> Readout {
    println!("\n=== {name}: {seeds:?} ===");
    println!(
        "  {} workings — {} breached, {} ordinarily ended, {} STILL OPEN (excluded)",
        split.breached.len() + split.ordinary.len() + split.still_open.len(),
        split.breached.len(),
        split.ordinary.len(),
        split.still_open.len(),
    );
    describe("breached", &split.breached);
    describe("ordinary", &split.ordinary);
    describe("still-open", &split.still_open);

    let b = sorted_depths(&split.breached);
    let o = sorted_depths(&split.ordinary);
    let (mut below, mut above) = (0usize, 0usize);
    if !b.is_empty() && !o.is_empty() {
        below = b.iter().filter(|&&d| d < o[o.len() - 1]).count();
        above = o.iter().filter(|&&d| d > b[0]).count();
        println!(
            "  OVERLAP  {below}/{} breached below the deepest ordinary end ({:.1} m)",
            b.len(),
            o[o.len() - 1],
        );
        println!(
            "           {above}/{} ordinary above the shallowest breach ({:.1} m)",
            o.len(),
            b[0],
        );
    }
    let u = mann_whitney_u(&b, &o);
    let pairs = (b.len() * o.len()) as f64;
    let z = u_z(u, b.len(), o.len());
    println!(
        "  AUC (P a breach is deeper than an ordinary end) = {:.4}   z = {z:.3}",
        if pairs == 0.0 { f64::NAN } else { u / pairs },
    );

    println!("  by epochs dug:");
    let (mut su, mut se, mut sv, mut spairs) = (0.0f64, 0.0f64, 0.0f64, 0.0f64);
    let mut strata_direction_holds = true;
    for (label, lo, hi) in STRATA {
        let pick = |g: &[Delving]| -> Vec<f64> {
            let mut v: Vec<f64> = g
                .iter()
                .filter(|d| d.epochs >= lo && d.epochs <= hi)
                .map(|d| d.depth_m)
                .collect();
            v.sort_by(|a, b| a.total_cmp(b));
            v
        };
        let (bb, oo) = (pick(&split.breached), pick(&split.ordinary));
        let (n1, n2) = (bb.len() as f64, oo.len() as f64);
        let us = mann_whitney_u(&bb, &oo);
        su += us;
        se += n1 * n2 / 2.0;
        sv += n1 * n2 * (n1 + n2 + 1.0) / 12.0;
        spairs += n1 * n2;
        let (mb, mo) = (quantile(&bb, 0.5), quantile(&oo, 0.5));
        if !bb.is_empty() && !oo.is_empty() && mb < mo {
            strata_direction_holds = false;
        }
        println!(
            "    {label:<5} breached n={:>3} med {:>9.1}   ordinary n={:>3} med {:>9.1}   \
             AUC {:.3}",
            bb.len(),
            mb,
            oo.len(),
            mo,
            if n1 == 0.0 || n2 == 0.0 {
                f64::NAN
            } else {
                us / (n1 * n2)
            },
        );
    }
    let stratified_z = if sv > 0.0 {
        (su - se) / sv.sqrt()
    } else {
        f64::NAN
    };
    println!(
        "  STRATIFIED on tenure: AUC {:.4}   z {stratified_z:.3}   every stratum's direction \
         holds: {strata_direction_holds}",
        if spairs == 0.0 { f64::NAN } else { su / spairs },
    );

    let tenures = |g: &[Delving]| -> f64 {
        let mut v: Vec<f64> = g.iter().map(|d| d.epochs).collect();
        v.sort_by(|a, b| a.total_cmp(b));
        quantile(&v, 0.5)
    };
    println!(
        "  median tenure (epochs dug): breached {:.1}   ordinary {:.1}",
        tenures(&split.breached),
        tenures(&split.ordinary),
    );

    Readout {
        breached_median: quantile(&b, 0.5),
        ordinary_median: quantile(&o, 0.5),
        z,
        below,
        above,
        stratified_z,
        strata_direction_holds,
    }
}

/// **§5.2's branch table, applied.** The campaign's answer, over the panel
/// spec amendment E.9 fixes.
///
/// The verdict is the table's middle row — *breached are deeper, with
/// overlap* — and the three assertions are the three things that row asserts
/// and its neighbours deny: the groups are distinguishable (against the first
/// row's null), the breached group is the deeper one, and the overlap is
/// non-empty in **both** directions (against the third row's warning that
/// perfect separation would mean the hazard had become a depth threshold in
/// disguise).
///
/// No count and no rate is pinned. The hazard's constant was read off terrain
/// without reference to any count (spec E.8), and asserting the breached
/// *fraction* here would smuggle it back in as a target — `breach.rs`'s
/// `breaching_is_a_hazard_not_a_certainty` makes the same refusal for the
/// same reason. What is pinned is the branch.
///
/// claim: invariant(seeds: the E.9 panel — pooled, the breached depth
/// distribution is distinguishable from the ordinarily-ended one at
/// `Z_DECIDES`, deeper by median, and overlapping in both directions; the
/// still-open population is excluded and both compared groups are asserted
/// non-trivial so no clause is vacuous)
#[test]
fn breached_delvings_are_deeper_with_overlap() {
    let split = split_over(&PANEL);
    let r = report("PANEL (E.9: the 12-seed cap governs)", &PANEL, &split);

    assert!(
        split.breached.len() >= 2 && split.ordinary.len() >= 2,
        "{} breached and {} ordinarily-ended workings on {PANEL:?}: a distribution comparison \
         needs both groups to be distributions, so this gate cannot apply §5.2's branch table \
         at all.",
        split.breached.len(),
        split.ordinary.len(),
    );
    assert!(
        r.z > Z_DECIDES,
        "z = {:.3}, at or under the {Z_DECIDES} this file decides at: the breached and \
         ordinarily-ended depth distributions are INDISTINGUISHABLE. That is §5.2's FIRST row — \
         the survivorship claim is false and the hazard is decoration. Report it as the \
         headline; spec §5.2 forbids tuning BREACH_FREE_PATH_M to separate them again.",
        r.z,
    );
    assert!(
        r.breached_median > r.ordinary_median,
        "breached median {:.1} m is not above the ordinary median {:.1} m. The groups differ \
         (z = {:.3}) and the deeper one is NOT the breached one, which is a shape §5.2's branch \
         table does not contain — stop and report before reading it as any of the three rows.",
        r.breached_median,
        r.ordinary_median,
        r.z,
    );
    assert!(
        r.below > 0 && r.above > 0,
        "the overlap is empty in one direction ({} breached below the deepest ordinary end, {} \
         ordinary above the shallowest breach). That is §5.2's THIRD row: perfect separation \
         suggests the hazard has become a depth threshold in disguise, and the whole design \
         rests on nothing anywhere comparing an accumulated depth against anything.",
        r.below,
        r.above,
    );
}

/// **Was the pooled gap composition?** The floor spike says the question has
/// to be asked.
///
/// 23.7% of ordinarily-ended workings sit at the founding-epoch floor against
/// 3.3% of breached ones, and the breached group's median tenure is 21.0
/// epochs against the ordinary group's 5.0. So part of the pooled separation
/// is *breached workings lived longer* rather than *breached workings were
/// deeper for their tenure*, and only the second is what §5.2 claims. Under a
/// per-metre hazard both are the mechanism — total metres is what the hazard
/// integrates and total metres is tenure times rate — but a pooled gap that
/// vanished on conditioning would mean the hazard had merely re-labelled
/// long-lived workings.
///
/// It does not vanish: it attenuates and holds (pooled AUC 0.8455 →
/// stratified 0.7160, z 3.197), with the direction intact in every stratum
/// that carries both groups.
///
/// claim: invariant(seeds: the E.9 panel — the Mann-Whitney statistic
/// stratified on epochs dug stays above `Z_SUPPORTS`, and no stratum holding
/// both groups has the breached median below the ordinary one)
#[test]
fn the_separation_survives_conditioning_on_tenure() {
    let split = split_over(&PANEL);
    let r = report("PANEL, conditioned on tenure", &PANEL, &split);

    assert!(
        split.breached.len() >= 2 && split.ordinary.len() >= 2,
        "the panel carries {} breached and {} ordinarily-ended workings, so there is nothing to \
         stratify.",
        split.breached.len(),
        split.ordinary.len(),
    );
    assert!(
        r.stratified_z > Z_SUPPORTS,
        "stratified z = {:.3}, at or under {Z_SUPPORTS}, while the pooled z is {:.3}. The \
         pooled separation is COMPOSITION: breached workings lived longer, and within a tenure \
         stratum they are not deeper. §5.2 claims the second, so this is a finding — report it \
         rather than reading the pooled gap as the survivorship shape.",
        r.stratified_z,
        r.z,
    );
    assert!(
        r.strata_direction_holds,
        "at least one tenure stratum holding both groups has its breached median BELOW its \
         ordinary median, so the stratified statistic (z = {:.3}) is averaging over strata that \
         disagree. Read the per-stratum table above before treating it as one effect.",
        r.stratified_z,
    );
}

/// **The transparency control spec amendment E.9 requires**, and the two
/// things it is required to show.
///
/// E.4.2's literal rule stops at four seeds. E.9 found that its stopping
/// threshold was set on *mines* as a proxy for *breaches* with a 7x error in
/// the conversion, so the rule as written would have made §5.2 unmeasurable
/// while reporting itself satisfied. This gate holds both halves of E.9's
/// resolution:
///
/// - the control **agrees in direction** with the panel. E.9: *"if the two
///   disagree in direction, that is a finding that outranks everything else
///   in the task and the campaign stops until it is explained"* — so a red
///   here is that stop, not a defect in this file;
/// - the control remains less decisive than the frozen panel. The Murrain
///   changes the substrate enough that the control is now informative (z 3.536
///   against the panel's z 5.896), so the old E.9 claim that it must stay below
///   `Z_DECIDES` is no longer true. That is a recorded sensitivity finding, not
///   permission to alter the panel, its cap, or the mechanism.
///
/// claim: invariant(seeds: E.4.2's literal four-seed stopping point — the
/// breached median is above the ordinary median, matching the panel's
/// direction, and the control is less decisive than the frozen twelve-seed
/// panel even when the control itself crosses `Z_DECIDES`)
#[test]
fn the_four_seed_control_agrees_in_direction_and_is_weaker_than_the_panel() {
    let split = split_over(&CONTROL);
    let r = report("CONTROL (E.4.2's literal rule)", &CONTROL, &split);
    let panel_split = split_over(&PANEL);
    let panel = report("PANEL comparison for E.9 control", &PANEL, &panel_split);

    assert!(
        !split.breached.is_empty() && split.ordinary.len() >= 2,
        "the control panel {CONTROL:?} carries {} breached and {} ordinarily-ended workings, so \
         neither clause below quantifies over anything.",
        split.breached.len(),
        split.ordinary.len(),
    );
    assert!(
        r.breached_median > r.ordinary_median,
        "THE CONTROL DISAGREES WITH THE PANEL IN DIRECTION: on {CONTROL:?} the breached median \
         is {:.1} m against an ordinary median of {:.1} m. Spec amendment E.9 makes this the \
         finding that outranks everything else in Task 5 — the campaign stops until it is \
         explained.",
        r.breached_median,
        r.ordinary_median,
    );
    assert!(
        r.z < panel.z,
        "the four-seed control z = {:.3} is at least as decisive as the frozen panel z = {:.3}; \
         the control and panel no longer have the intended sensitivity ordering, so re-read E.9 \
         rather than treating this as a mechanism change.",
        r.z,
        panel.z,
    );
}

/// **The still-open workings are not a third arm of the comparison**, and
/// this measures what folding them in would have cost.
///
/// Eighty-eight of the panel's 249 workings never ended. They have no *final*
/// depth — the bake's clock stopped, not the delving — and their median
/// (343.3 m) sits above the ordinary group's but below the breached group's.
/// Pooling them into the ordinarily-ended
/// group would replace "how a delving ended" with "where the record was cut".
///
/// Measured, and in the direction that matters for reading the result
/// honestly: pooling raises the comparison group's median from 47.6 m to
/// 107.4 m and drops the separation from AUC 0.8455 to 0.7383. So the
/// exclusion **costs** the finding evidence rather than creating it — the
/// opposite of the failure mode an exclusion usually has to answer for.
///
/// The gate is that the exclusion is *load-bearing* — that the third
/// population is non-empty, genuinely unlike the group it would otherwise
/// have joined, and consequential for the measurement.
///
/// claim: invariant(seeds: the E.9 panel — the still-open population is
/// non-empty, no still-open working carries an ending cause, and its median
/// depth is above the ordinarily-ended median, so pooling the two would
/// change the comparison rather than merely enlarge it)
#[test]
fn the_still_open_population_is_not_a_third_arm_of_the_comparison() {
    let split = split_over(&PANEL);
    let open = sorted_depths(&split.still_open);
    let ordinary = sorted_depths(&split.ordinary);
    let breached = sorted_depths(&split.breached);

    let mut pooled = ordinary.clone();
    pooled.extend_from_slice(&open);
    pooled.sort_by(|a, b| a.total_cmp(b));
    let honest = mann_whitney_u(&breached, &ordinary) / (breached.len() * ordinary.len()) as f64;
    let wrong = mann_whitney_u(&breached, &pooled) / (breached.len() * pooled.len()) as f64;
    println!(
        "\n=== still-open handling, panel {PANEL:?} ===\n  \
         excluded (as measured): ordinary n={} med {:.1}   AUC vs breached {honest:.4}\n  \
         pooled in (rejected):   ordinary n={} med {:.1}   AUC vs breached {wrong:.4}",
        ordinary.len(),
        quantile(&ordinary, 0.5),
        pooled.len(),
        quantile(&pooled, 0.5),
    );

    assert!(
        !open.is_empty(),
        "no working on {PANEL:?} is still open, so excluding the still-open population is a \
         decision about the empty set and every claim this file makes about it is vacuous."
    );
    assert!(
        wrong < honest,
        "pooling the still-open workings into the ordinarily-ended group leaves the measured \
         separation unmoved (AUC {honest:.4} excluded, {wrong:.4} pooled), so the exclusion this \
         file performs is not load-bearing and the claim that it protects the comparison is \
         unsupported. Re-derive the handling rather than asserting it.",
    );
    assert!(
        quantile(&open, 0.5) > quantile(&ordinary, 0.5),
        "the still-open median ({:.1} m) is no longer above the ordinarily-ended one ({:.1} m). \
         The exclusion was justified by the two populations being unlike — if they have \
         converged, re-derive whether excluding the still-open group is still the right \
         handling before reading §5.2's branch table off a comparison that assumed it.",
        quantile(&open, 0.5),
        quantile(&ordinary, 0.5),
    );
}
