//! The population health metric (The Temperament, Stage 3B): a self-scoring
//! distress read over a simulated span, anchored to a control. Preregistered:
//! a healthy world reads ~0 (no false alarm), and the family's reduction FIRES
//! correctly on injected distress patterns — spike-then-recover vs
//! spike-and-persist — while excluding normal Searching. Because genuine
//! blocked-distress is rare in the drive model (a creature SEARCHES rather than
//! despairs — a real barren mesh yields endless Searching, not Lost), the
//! injected-fault signatures are exercised on constructed affect traces (the
//! metric's detection logic), and the null control on a real world (end to end).
use hornvale_lab::health::{AffectTrace, HealthReport, health_report, simulate_world};
use hornvale_vessel::liveness::{Affect, AffectLabel, DriveKind};

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
