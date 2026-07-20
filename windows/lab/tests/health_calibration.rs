//! The population health metric (The Temperament, Stage 3B): a self-scoring
//! distress read over a simulated span, anchored to a control. Preregistered:
//! a healthy world reads ~0 (no false alarm), and the family's reduction FIRES
//! correctly on injected distress patterns — spike-then-recover vs
//! spike-and-persist — while excluding normal Searching.
//!
//! Two tiers of evidence sit here. The CONSTRUCTED-trace tests exercise the
//! reduction logic in isolation (hand-built `Affect` sequences → `health_report`
//! — fast, precise, edge-case sharp). The END-TO-END tests (The Temperament
//! followup #5, the synthetic distress harness) close the gap the reduction
//! tier left open: because genuine blocked-distress is rare in the drive model
//! (a creature SEARCHES rather than despairs — a real barren mesh yields endless
//! Searching, not Lost), the harness hand-builds terrain + creatures the drive
//! model can be FORCED into distress, runs the real sim, and scores the sim's
//! own affect output — so chronicity/recovery/by-cause/by-species are proven on
//! the path from a world into distress, not just on affect structs typed by
//! hand. The null control runs on real generated worlds throughout.
use hornvale_lab::health::{AffectTrace, HealthReport, health_report, simulate_world};
use hornvale_lab::synthetic::{
    a_heat_wave_that_passes, a_stricken_and_a_healthy_people, stranded_from_known_water,
};
use hornvale_vessel::liveness::{Affect, AffectLabel, DriveKind};

/// The span the harness scenarios simulate — matches `health.rs`'s
/// `HEALTH_TICKS` so a scenario's chronic run spans the same window a real
/// sweep would read it over.
const HARNESS_TICKS: usize = 40;

fn world(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A one-species trace from a sequence of labels (object: thirst for the
/// distress regions, so by-cause attributes them).
fn trace(labels: &[AffectLabel]) -> AffectTrace {
    AffectTrace {
        species: "goblin".to_string(),
        affects: labels
            .iter()
            .map(|&label| Affect {
                arousal: if matches!(label, AffectLabel::Content) {
                    0.1
                } else {
                    0.9
                },
                valence: match label {
                    AffectLabel::Content | AffectLabel::Eager => 1.0,
                    AffectLabel::Searching => 0.0,
                    _ => -1.0,
                },
                label,
                object: Some(DriveKind::Thirst),
            })
            .collect(),
    }
}

#[test]
fn the_null_control_reads_no_distress() {
    // A real, healthy world (seed 42's flagship settlement condenses onto fresh
    // water — its creatures drink in place and never enter distress) must read
    // ZERO on every distress axis. This is the anchor: the metric does not false
    // alarm on a world that is fine.
    let a = health_report(&simulate_world(&world(42)));
    let b = health_report(&simulate_world(&world(42)));
    assert_eq!(a, b, "same world -> same report (deterministic)");
    assert_eq!(
        a.prevalence, 0.0,
        "healthy world: zero instantaneous distress"
    );
    assert_eq!(a.chronicity, 0.0, "healthy world: no one chronically stuck");
    assert_eq!(
        a.recovery_ticks, None,
        "healthy world: no spikes to recover from"
    );
    assert_eq!(a.by_cause["thirst"], 0.0);
    assert_eq!(a.by_cause["thermal"], 0.0);
}

#[test]
fn the_null_control_holds_across_a_seed_sweep() {
    // Over a small sweep of real worlds, no population reads CHRONIC distress —
    // the zero is not a seed-42 accident. (Genuine blocked-distress needs a
    // creature boxed in or knowing-but-blocked, which condensed on-water
    // settlements avoid; a healthy world stays healthy.) The bug alarm is armed
    // precisely because this stays quiet.
    for seed in [0u64, 1, 2, 7, 42] {
        let r = health_report(&simulate_world(&world(seed)));
        assert_eq!(
            r.chronicity, 0.0,
            "seed {seed} shows chronic distress (the alarm fired): {r:?}"
        );
    }
}

#[test]
fn searching_is_not_distress() {
    // THE LOAD-BEARING EXCLUSION (spec §7/§8): a creature seeking not-yet-found
    // water is puttering normally, not distressed. A run of pure Searching must
    // read zero — counting it would make the metric meaningless.
    use AffectLabel::*;
    let r = health_report(&[trace(&[
        Content, Searching, Searching, Searching, Eager, Content,
    ])]);
    assert_eq!(
        r.prevalence, 0.0,
        "searching is normal seeking, not distress"
    );
    assert_eq!(r.chronicity, 0.0);
}

#[test]
fn an_injected_spike_recovers() {
    // A NOVEL EVENT the mind recovers from: a short distress run bracketed by a
    // return to health. It contributes prevalence, but NOT chronicity (the run
    // is below the chronic threshold), and its length is the recovery half-life.
    use AffectLabel::*;
    let r = health_report(&[trace(&[
        Content, Content, Lost, Lost, Lost, Content, Content, Content,
    ])]);
    assert_eq!(r.prevalence, 3.0 / 8.0, "3 of 8 ticks in distress");
    assert_eq!(r.chronicity, 0.0, "a 3-tick spike is not chronic (< 8)");
    assert_eq!(
        r.recovery_ticks,
        Some(3.0),
        "the recovered spike's length is the recovery signal"
    );
    assert_eq!(
        r.by_cause["thirst"], 1.0,
        "all distress attributed to thirst"
    );
}

#[test]
fn an_unsatisfiable_need_persists() {
    // THE BUG ALARM (spec §8): distress that does NOT recover — a run at least
    // CHRONIC_TICKS long that never returns to health. It reads chronic, and
    // because the spike never ended, there is no recovery to measure.
    use AffectLabel::*;
    let mut labels = vec![Content, Content];
    labels.extend(std::iter::repeat_n(Lost, 12)); // >> the 8-tick chronic threshold
    let r = health_report(&[trace(&labels)]);
    assert_eq!(r.chronicity, 1.0, "the one creature is chronically stuck");
    assert!(
        r.prevalence > 0.8,
        "distress dominates the span: {}",
        r.prevalence
    );
    assert_eq!(
        r.recovery_ticks, None,
        "a never-ending spike has no recovery half-life"
    );
}

#[test]
fn by_species_separates_a_stricken_people_from_a_healthy_one() {
    // Diagnostic (spec §8): distress is attributed per species, so a cold-niche
    // people in a warm world reads high while its neighbour reads fine.
    use AffectLabel::*;
    let stricken = AffectTrace {
        species: "kobold".to_string(),
        affects: trace(&[Lost, Lost, Lost, Lost]).affects,
    };
    let healthy = AffectTrace {
        species: "goblin".to_string(),
        affects: trace(&[Content, Content, Content, Content]).affects,
    };
    let r: HealthReport = health_report(&[stricken, healthy]);
    assert_eq!(
        r.by_species["kobold"], 1.0,
        "the stricken people reads distressed"
    );
    assert_eq!(r.by_species["goblin"], 0.0, "the healthy people reads fine");
}

// --- END-TO-END: the synthetic distress harness (followup #5) ---------------
// The scenarios below drive the REAL sim (`run_simulation` → `affect_of`) into
// distress and score its own affect output, closing the gap the constructed-
// trace tests left: the reduction was proven, but never the path from a world
// into distress. Each asserts on the distress FAMILY (prevalence / chronicity /
// recovery / by-cause), not on exact Content-vs-Eager tick counts (a creature's
// mid-cycle drink lands an incidental positive tick whose timing is a sim
// artifact — irrelevant to distress).

#[test]
fn a_stranded_creature_is_scored_chronic_end_to_end() {
    // THE BUG ALARM, now end to end: a creature that believes in a spring it
    // drank from but is stranded past the plan budget from it Holds in thirst
    // `Frustrated` for the rest of the run — a real sim producing a chronic
    // distress run the metric scores, not a hand-typed `[Lost, Lost, …]`.
    let r = health_report(&stranded_from_known_water().simulate(HARNESS_TICKS));
    assert_eq!(
        r.chronicity, 1.0,
        "the stranded creature is chronically stuck: {r:?}"
    );
    assert_eq!(
        r.recovery_ticks, None,
        "a never-ending stranding has no recovery half-life"
    );
    assert!(
        r.prevalence > 0.5,
        "distress dominates the span: {}",
        r.prevalence
    );
    assert_eq!(
        r.by_cause["thirst"], 1.0,
        "the distress is entirely thirst (unreachable water)"
    );
    assert_eq!(r.by_cause["thermal"], 0.0);
}

#[test]
fn a_passing_heat_wave_is_scored_a_recovered_spike_end_to_end() {
    // THE RESILIENT SPIKE, now end to end: a creature on its spring (thirst
    // stays serviceable) is boxed in by a blistering heat wave, then the wave
    // breaks and it returns to Content — a real distress spike that recovers
    // BELOW the chronic threshold, the resilient counterpart to stranding.
    let r = health_report(&a_heat_wave_that_passes().simulate(HARNESS_TICKS));
    assert_eq!(
        r.chronicity, 0.0,
        "a wave that breaks is not chronic distress: {r:?}"
    );
    assert!(
        r.recovery_ticks.is_some(),
        "the recovered spike yields a recovery half-life: {r:?}"
    );
    assert!(
        r.prevalence > 0.0 && r.prevalence < 0.5,
        "a transient spike, not a dominating one: {}",
        r.prevalence
    );
    assert_eq!(
        r.by_cause["thermal"], 1.0,
        "the distress is entirely thermal (the heat wave)"
    );
    assert_eq!(r.by_cause["thirst"], 0.0);
}

#[test]
fn by_species_separates_a_stricken_people_end_to_end() {
    // The by-species diagnostic on real sim output: a stranded kobold beside a
    // goblin on its own reachable spring in a mild climate — the reduction
    // attributes the distress to the stricken species alone, from the sim's own
    // traces rather than constructed ones.
    let r = health_report(&a_stricken_and_a_healthy_people().simulate(HARNESS_TICKS));
    assert!(
        r.by_species["kobold"] > 0.5,
        "the stranded people reads distressed: {r:?}"
    );
    assert_eq!(
        r.by_species["goblin"], 0.0,
        "the well-watered people reads fine: {r:?}"
    );
}

#[test]
fn the_harness_scenarios_are_deterministic() {
    // Same scenario -> byte-identical report (the constitutional guarantee, on
    // the synthetic path too): the harness is a pure function of its hand-built
    // ledger and terrain, the same determinism the null control asserts on real
    // worlds.
    let a = health_report(&stranded_from_known_water().simulate(HARNESS_TICKS));
    let b = health_report(&stranded_from_known_water().simulate(HARNESS_TICKS));
    assert_eq!(a, b, "same scenario -> same report");
}
