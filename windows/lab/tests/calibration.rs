//! Calibration: at tier 0, belief kind is a pure function of rotation.
//! The instrument must reproduce known ground truth exactly (spec §2.5).
use hornvale_culture::{BiomeClass, subsistence};
use hornvale_lab::{MetricValue, load_study, run};
use std::path::Path;

/// Map a `flagship-biome` metric's kebab-case name back to culture's coarse
/// `BiomeClass`, mirroring `hornvale_worldgen::biome_class`'s grouping. A
/// small duplicate is unavoidable here: the metric reports the biome as a
/// committed Text fact (a `String`), not the `hornvale_climate::Biome` enum
/// that `biome_class` maps from, so the calibration re-derives its
/// expectation from this independent metric column rather than the enum
/// itself — which is the point (spec §10): it checks the committed
/// subsistence fact against biome + coastal, not against its own inputs.
fn biome_class_from_name(name: &str) -> BiomeClass {
    match name {
        "temperate-forest"
        | "temperate-rainforest"
        | "tropical-seasonal-forest"
        | "tropical-rainforest"
        | "taiga" => BiomeClass::Forest,
        "savanna" | "temperate-grassland" => BiomeClass::Grassland,
        "desert" | "shrubland" => BiomeClass::Arid,
        "tundra" => BiomeClass::Cold,
        _ => BiomeClass::Barren,
    }
}

#[test]
fn eternal_beliefs_coincide_exactly_with_tidal_locking() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (locked_i, belief_i) = (idx("tidally-locked"), idx("belief-kind"));
    for row in &result.rows {
        let locked = matches!(row.values[locked_i], MetricValue::Flag(true));
        let eternal = matches!(&row.values[belief_i], MetricValue::Text(t) if t == "eternal");
        assert_eq!(locked, eternal, "seed {}: calibration violated", row.seed);
    }
}

#[test]
fn band_count_matches_the_known_function_of_rotation() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (day_i, band_i) = (idx("day-length-hours"), idx("band-count"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        // Locked worlds report Absent day length and "locked" band count.
        let expected = match &row.values[day_i] {
            MetricValue::Number(hours) => {
                if *hours >= 40.0 {
                    "1".to_string()
                } else if *hours >= 20.0 {
                    "3".to_string()
                } else if *hours >= 10.0 {
                    "5".to_string()
                } else {
                    "7".to_string()
                }
            }
            _ => "locked".to_string(),
        };
        let actual = match &row.values[band_i] {
            MetricValue::Text(t) => t.clone(),
            other => panic!("seed {}: band-count not text: {other:?}", row.seed),
        };
        assert_eq!(
            actual, expected,
            "seed {}: band-count calibration violated",
            row.seed
        );
    }
}

#[test]
fn flagship_subsistence_matches_biome_and_coastal_columns() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (subsistence_i, biome_i, coastal_i) = (
        idx("flagship-subsistence"),
        idx("flagship-biome"),
        idx("flagship-coastal"),
    );
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        // Absent means no flagship (or no committed subsistence) in this
        // world; nothing to calibrate.
        let MetricValue::Text(actual) = &row.values[subsistence_i] else {
            continue;
        };
        let biome = match &row.values[biome_i] {
            MetricValue::Text(b) => b,
            other => panic!("seed {}: flagship-biome not text: {other:?}", row.seed),
        };
        let coastal = match &row.values[coastal_i] {
            MetricValue::Flag(c) => *c,
            other => panic!("seed {}: flagship-coastal not a flag: {other:?}", row.seed),
        };
        let class = biome_class_from_name(biome);
        let expected = subsistence(class, coastal).name();
        assert_eq!(
            actual, expected,
            "seed {}: subsistence-biome calibration violated (biome={}, coastal={})",
            row.seed, biome, coastal
        );
    }
}

#[test]
fn pantheon_verticality_matches_stratification() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (vert_i, size_i) = (idx("pantheon-verticality"), idx("flagship-structure-size"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if matches!(row.values[vert_i], MetricValue::Absent) {
            continue;
        }
        let ranked = matches!(&row.values[vert_i], MetricValue::Text(t) if t == "ranked");
        let stratified = matches!(&row.values[size_i], MetricValue::Number(n) if *n >= 4.0);
        assert_eq!(
            ranked, stratified,
            "seed {}: verticality calibration violated",
            row.seed
        );
    }
}

#[test]
fn head_deity_is_eternal_exactly_when_tidally_locked() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (head_i, lock_i) = (idx("head-deity-periodicity"), idx("tidally-locked"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if matches!(row.values[head_i], MetricValue::Absent) {
            continue;
        }
        let eternal = matches!(&row.values[head_i], MetricValue::Text(t) if t == "eternal");
        let locked = matches!(row.values[lock_i], MetricValue::Flag(true));
        assert_eq!(
            eternal, locked,
            "seed {}: head-deity calibration violated",
            row.seed
        );
    }
}

#[test]
fn goblin_flagship_coastal_split_is_pinned() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let coastal_i = idx("flagship-coastal");
    let (mut coastal, mut inland) = (0u32, 0u32);
    for row in &result.rows {
        match row.values[coastal_i] {
            MetricValue::Flag(true) => coastal += 1,
            MetricValue::Flag(false) => inland += 1,
            _ => {}
        }
    }
    // Campaign Y2-0: seawater is not freshwater. Before the fix every
    // censused flagship was coastal (100% at 10,000 seeds; Study 003 records
    // the history). Exact-count pin over the 500-seed drift study
    // (deterministic): the fixed model's realized split, measured 2026-07
    // at re-baseline.
    //
    // Campaign Y2-1: `flagship-coastal` now names the goblin flagship
    // specifically (religion's community, spec §6), not just whichever
    // species' settlement happened to place first. Under joint-greedy
    // placement the two seeds that used to report an inland goblin flagship
    // (172 and 257) instead lose that site to a higher-scoring kobold
    // placement — both are total-kobold-exclusion worlds where goblins
    // place nothing at all, so `flagship-coastal` reports `Absent` for
    // them, independently verified. Coastal is unchanged at 498; inland
    // drops from 2 to 0 (renamed from
    // `flagships_are_sometimes_inland_and_sometimes_coastal`, now false).
    assert_eq!(coastal, 498, "coastal flagship count drifted");
    assert_eq!(inland, 0, "inland flagship count drifted");
}

#[test]
fn kobold_structures_never_enslave_and_top_out_with_elders() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (kob_i, gob_i) = (idx("kobold-flagship-roles"), idx("goblin-flagship-roles"));
    for row in &result.rows {
        if let MetricValue::Text(roles) = &row.values[kob_i] {
            assert!(
                !roles.contains("slave"),
                "seed {}: kobold slavery",
                row.seed
            );
            assert!(
                roles.ends_with("elders"),
                "seed {}: kobold top rung",
                row.seed
            );
        }
        if let MetricValue::Text(roles) = &row.values[gob_i] {
            assert!(
                roles.ends_with("chief"),
                "seed {}: goblin top rung",
                row.seed
            );
        }
    }
}

#[test]
fn the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale() {
    // Preregistered (spec §9.2): slave ⇔ Rank ∧ surplus > 0.6 ∧ population >
    // 300, checked on goblin rows (Rank) and kobold rows (¬Rank) from
    // independent recomputed columns.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    for species in ["goblin", "kobold"] {
        let (r_i, s_i, p_i) = (
            idx(&format!("{species}-flagship-roles")),
            idx(&format!("{species}-flagship-surplus")),
            idx(&format!("{species}-flagship-population")),
        );
        for row in &result.rows {
            let MetricValue::Text(roles) = &row.values[r_i] else {
                continue;
            };
            let MetricValue::Number(surplus) = &row.values[s_i] else {
                continue;
            };
            let MetricValue::Number(pop) = &row.values[p_i] else {
                continue;
            };
            let expected = species == "goblin" && *surplus > 0.6 && *pop > 300.0;
            assert_eq!(
                roles.split(',').any(|r| r == "slave"),
                expected,
                "seed {}: slave calibration violated ({species}, surplus={surplus}, pop={pop})",
                row.seed
            );
        }
    }
}

#[test]
fn kobold_flagships_are_less_coastal_than_goblin_flagships() {
    // Preregistered directional hypothesis (spec §9.1); Task 10 pins exact
    // counts after measurement.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let rate = |col: usize| {
        let (mut t, mut n) = (0u32, 0u32);
        for row in &result.rows {
            match row.values[col] {
                MetricValue::Flag(true) => {
                    t += 1;
                    n += 1
                }
                MetricValue::Flag(false) => n += 1,
                _ => {}
            }
        }
        f64::from(t) / f64::from(n.max(1))
    };
    let goblin = rate(idx("goblin-flagship-coastal"));
    let kobold = rate(idx("kobold-flagship-coastal"));
    assert!(kobold < goblin, "kobold {kobold:.3} !< goblin {goblin:.3}");
}

#[test]
fn goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (g_i, k_i, moons_i) = (
        idx("head-deity-domain-goblin"),
        idx("head-deity-domain-kobold"),
        idx("moons-admitted"),
    );
    let (mut moonless_solar, mut moonless_lunar) = (0u32, 0u32);
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if let MetricValue::Text(domain) = &row.values[g_i] {
            assert_eq!(domain, "solar", "seed {}: goblin head not solar", row.seed);
        }
        let mooned = matches!(&row.values[moons_i], MetricValue::Text(n) if n != "0");
        if mooned && let MetricValue::Text(domain) = &row.values[k_i] {
            assert_eq!(
                domain, "lunar",
                "seed {}: kobold head not lunar despite a moon",
                row.seed
            );
        }
        if !mooned && let MetricValue::Text(domain) = &row.values[k_i] {
            // Moonless kobold heads split night-star/sun by star
            // brightness — spec §9.2 declines to preregister this split,
            // pinning it as a calibration row after measurement instead.
            match domain.as_str() {
                "solar" => moonless_solar += 1,
                "lunar" => moonless_lunar += 1,
                other => panic!(
                    "seed {}: unexpected moonless kobold head domain {other}",
                    row.seed
                ),
            }
        }
    }
    // Pinned calibration row (measured at the Y2-2 re-baseline, 500-seed
    // drift study): moonless kobold heads split 62 solar / 10 lunar — the
    // sun wins most moonless skies, but a bright-enough night-star still
    // outshines it in a minority of cases.
    assert_eq!(
        moonless_solar, 62,
        "moonless-solar kobold head count drifted"
    );
    assert_eq!(
        moonless_lunar, 10,
        "moonless-lunar kobold head count drifted"
    );
}

#[test]
fn blind_attribution_beats_chance_decisively() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (a_i, moons_i) = (idx("blind-attribution-correct"), idx("moons-admitted"));
    let (mut correct, mut total) = (0u32, 0u32);
    let (mut correct_mooned, mut total_mooned) = (0u32, 0u32);
    for row in &result.rows {
        let mooned = matches!(&row.values[moons_i], MetricValue::Text(n) if n != "0");
        match &row.values[a_i] {
            MetricValue::Flag(true) => {
                correct += 1;
                total += 1;
                if mooned {
                    correct_mooned += 1;
                    total_mooned += 1;
                }
            }
            MetricValue::Flag(false) => {
                total += 1;
                if mooned {
                    total_mooned += 1;
                }
            }
            _ => {}
        }
    }
    assert!(total > 0, "no attributable world pairs in the drift study");
    // Directional preregistration (spec §9.2): decisively above chance.
    // The plan's original preregistered floor was 0.9; the first measurement
    // (2026-07-08, 500-seed drift study) came in at 0.875 (434/496). The
    // miss is entirely the 62 moonless pairs, where the cyclic-share tier
    // inverts because night-stars are eternal (period None) — recorded as a
    // discovery for Study 007. The spec's directional preregistration
    // ("well above chance") is satisfied; by owner decision the
    // preregistered rule stays untouched and the honest measured rate is
    // pinned. Exact counts are pinned at the re-baseline task.
    let accuracy = f64::from(correct) / f64::from(total);
    assert!(
        accuracy >= 0.875,
        "blind attribution at {accuracy:.3} — below the pinned floor"
    );
    // Pinned calibration row (measured at the Y2-2 re-baseline; the drift
    // study is 500 seeds, so this is an exact count, not a rate):
    assert_eq!(correct, 434, "blind-attribution count drifted");
    assert_eq!(total, 496, "attributable-pair count drifted");
    // Pinned calibration row, first measured 2026-07-08 — the anti-reskin
    // claim at the head-domain calibration's own scope: restricted to pairs
    // on worlds with at least one moon, the fixed rule attributes the
    // kobold pantheon perfectly.
    assert!(total_mooned > 0, "no mooned attributable pairs");
    assert_eq!(
        correct_mooned, total_mooned,
        "mooned blind attribution not perfect: {correct_mooned}/{total_mooned}"
    );
}

#[test]
fn phonotactic_validity_is_true_for_every_generated_name() {
    // Preregistered (ADR 0016, spec §9.2): the instrument must reproduce its
    // own grammar exactly. Every generated name — settlement, deity,
    // epithet — must re-validate against its species' own re-derived
    // phonotactics. If this is ever false the engine is producing names it
    // calls invalid: this is a STOP-and-report-BLOCKED condition (task
    // brief), never an assertion to loosen.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    for species in ["goblin", "kobold"] {
        let v_i = idx(&format!("phonotactic-validity-{species}"));
        for row in &result.rows {
            match &row.values[v_i] {
                MetricValue::Flag(valid) => assert!(
                    *valid,
                    "seed {}: {species} produced a name that fails its own phonotactics — BLOCKED",
                    row.seed
                ),
                MetricValue::Absent => {} // species placed nothing, held no pantheon
                other => panic!(
                    "seed {}: phonotactic-validity-{species} not a flag: {other:?}",
                    row.seed
                ),
            }
        }
    }
}

#[test]
fn epithet_honorific_is_true_for_goblin_and_false_for_kobold() {
    // Preregistered (ADR 0016, spec §9.2), directional: goblin's Rank status
    // basis draws honorific-prefixed epithets (spec §7's morph_options
    // mapping); kobold's Knowledge status basis does not. Row-by-row, since
    // Absent (no pantheon this world) is a legitimate skip.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (g_i, k_i) = (
        idx("epithet-honorific-goblin"),
        idx("epithet-honorific-kobold"),
    );
    for row in &result.rows {
        match &row.values[g_i] {
            MetricValue::Flag(v) => {
                assert!(*v, "seed {}: goblin epithet-honorific false", row.seed)
            }
            MetricValue::Absent => {}
            other => panic!(
                "seed {}: epithet-honorific-goblin not a flag: {other:?}",
                row.seed
            ),
        }
        match &row.values[k_i] {
            MetricValue::Flag(v) => {
                assert!(!*v, "seed {}: kobold epithet-honorific true", row.seed)
            }
            MetricValue::Absent => {}
            other => panic!(
                "seed {}: epithet-honorific-kobold not a flag: {other:?}",
                row.seed
            ),
        }
    }
}

#[test]
fn name_collision_rate_is_measured_and_pinned() {
    // Preregistered (spec §9.2): names are pure per-(seed, species, kind,
    // salt) draws with no re-draw, so uniqueness is de-facto rather than
    // enforced (Task 9) — this pins the MEASURED collision rate over the
    // 500-seed drift study as a calibration row, not an invariant. A
    // regression that widened or narrowed the drawn name space would move
    // these counts; a broken collision detector would too.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let rate_i = idx("name-collision-rate");
    let (mut zero, mut nonzero, mut absent) = (0u32, 0u32, 0u32);
    let mut sum = 0.0_f64;
    for row in &result.rows {
        match row.values[rate_i] {
            MetricValue::Number(r) if r == 0.0 => {
                zero += 1;
                sum += r;
            }
            MetricValue::Number(r) => {
                nonzero += 1;
                sum += r;
            }
            MetricValue::Absent => absent += 1,
            ref other => panic!(
                "seed {}: name-collision-rate not a number: {other:?}",
                row.seed
            ),
        }
    }
    // Pinned calibration row (measured at the Y2-3 re-baseline, 500-seed
    // drift study): most worlds draw no colliding names at all, but the
    // combinatorially large name space is not infinite, so a minority of
    // worlds show some collision.
    assert_eq!(zero, 336, "zero-collision world count drifted");
    assert_eq!(nonzero, 164, "nonzero-collision world count drifted");
    assert_eq!(absent, 0, "absent name-collision-rate count drifted");
    let present = zero + nonzero;
    assert!(present > 0, "no worlds with a measurable collision rate");
    let mean = sum / f64::from(present);
    assert!(
        (mean - 0.023_389_245_6).abs() < 1e-6,
        "mean name-collision-rate drifted: {mean:.10}"
    );
}

#[test]
fn name_length_distributions_are_measured_and_pinned() {
    // Preregistered (spec §9.2): mean generated-name length, per species,
    // pinned over the 500-seed drift study as a calibration row after
    // measurement — the naming/voice baseline's other half (contrast
    // `phonotactic_validity_is_true_for_every_generated_name`, which is an
    // invariant, not a measurement).
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    for (species, expected_present, expected_mean) in [
        ("goblin", 498u32, 9.869_276_105_4),
        ("kobold", 498u32, 9.799_462_903_6),
    ] {
        let (len_i,) = (idx(&format!("name-length-{species}")),);
        let (mut present, mut absent) = (0u32, 0u32);
        let mut sum = 0.0_f64;
        for row in &result.rows {
            match row.values[len_i] {
                MetricValue::Number(n) => {
                    present += 1;
                    sum += n;
                }
                MetricValue::Absent => absent += 1,
                ref other => panic!(
                    "seed {}: name-length-{species} not a number: {other:?}",
                    row.seed
                ),
            }
        }
        assert_eq!(
            present, expected_present,
            "{species} name-length present-row count drifted"
        );
        assert_eq!(
            present + absent,
            500,
            "{species} name-length row count drifted"
        );
        let mean = sum / f64::from(present);
        assert!(
            (mean - expected_mean).abs() < 1e-6,
            "{species} mean name length drifted: {mean:.10}"
        );
    }
}
