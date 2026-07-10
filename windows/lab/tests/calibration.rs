//! Calibration: at tier 0, belief kind is a pure function of rotation.
//! The instrument must reproduce known ground truth exactly (spec §2.5).
use hornvale_culture::{BiomeClass, subsistence};
use hornvale_lab::{MetricValue, RunResult, load_rows, load_study, run};
use std::path::Path;
use std::sync::LazyLock;

/// Load a census from its committed `rows.csv` fixture rather than recomputing
/// it. The fixture is published by `lab run` and regenerated + drift-checked in
/// CI (the "Artifacts are current" step), so `load_rows(fixture)` equals
/// `run(&study)` by construction — the `census_fixture_matches_live_run` guard
/// below pins that equality directly. This is what keeps the ~145s census off
/// every local `cargo test`; before this the suite recomputed it behind a
/// `LazyLock` (TOOL-7). Init panics on a load error (a test-setup failure, not
/// a calibration).
fn load_census(study_path: &str, rows_path: &str) -> RunResult {
    let study = load_study(Path::new(study_path)).expect("load study");
    let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
    load_rows(&study, &csv).expect("reconstruct census from fixture")
}

/// The 500-seed drift census, loaded ONCE and shared by every calibration in
/// this file (the shipped `{goblin, kobold}` roster).
static DRIFT: LazyLock<RunResult> = LazyLock::new(|| {
    load_census(
        "../../studies/census-lands-drift.study.json",
        "../../book/src/laboratory/generated/census-lands-drift/rows.csv",
    )
});

/// The 500-seed solo null-control census (spec §4), loaded ONCE and shared by
/// both null-control calibrations. A genuinely different population from
/// `DRIFT` (solo rosters), so it is its own fixture.
static MEETING: LazyLock<RunResult> = LazyLock::new(|| {
    load_census(
        "../../studies/census-of-the-meeting.study.json",
        "../../book/src/laboratory/generated/census-of-the-meeting/rows.csv",
    )
});

/// Guard — ignored by default because it pays the full census (~145s): the
/// committed fixtures reconstruct *exactly* what a live `run` produces, so
/// every other test in this file may trust the fixture. Run it explicitly
/// after regenerating the fixtures, or in CI:
/// `cargo test -p hornvale-lab --test calibration -- --ignored`.
#[test]
#[ignore = "runs the full ~145s census; fixtures are drift-checked in CI"]
fn census_fixture_matches_live_run() {
    for (study_path, rows_path) in [
        (
            "../../studies/census-lands-drift.study.json",
            "../../book/src/laboratory/generated/census-lands-drift/rows.csv",
        ),
        (
            "../../studies/census-of-the-meeting.study.json",
            "../../book/src/laboratory/generated/census-of-the-meeting/rows.csv",
        ),
    ] {
        let study = load_study(Path::new(study_path)).expect("load study");
        let live = run(&study).expect("run study");
        let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
        let loaded = load_rows(&study, &csv).expect("reconstruct census from fixture");
        assert_eq!(
            loaded, live,
            "fixture {rows_path} diverged from a live run — regenerate it with `lab run`"
        );
    }
}

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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
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
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (g_i, k_i, moons_i, locked_i) = (
        idx("head-deity-domain-goblin"),
        idx("head-deity-domain-kobold"),
        idx("moons-admitted"),
        idx("tidally-locked"),
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
        let locked = matches!(&row.values[locked_i], MetricValue::Flag(true));
        let Some(MetricValue::Text(domain)) = row.values.get(k_i) else {
            continue;
        };
        if locked {
            // The placed observer (Plan 2 Task 4, SEQ-4/SEQ-5): every
            // flagship this census ever places on a tidally-locked world
            // sits on the day side (the night hemisphere never clears the
            // habitability floor), so the kobold head is solar exactly
            // like the goblin's — moons or no moons, the night sky is
            // never in view from where either species actually settled.
            assert_eq!(
                domain, "solar",
                "seed {}: locked-world kobold head not solar",
                row.seed
            );
            continue;
        }
        if mooned {
            assert_eq!(
                domain, "lunar",
                "seed {}: kobold head not lunar despite a moon",
                row.seed
            );
        } else {
            // Spinning, moonless kobold heads split night-star/sun by star
            // brightness — spec §9.2 declines to preregister this split,
            // pinning it as a calibration row after measurement instead.
            // Locked, moonless worlds are folded into the always-solar
            // invariant above, not this split.
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
    // Pinned calibration row (re-measured for the placed observer, Plan 2
    // Task 4, 500-seed drift study): among SPINNING moonless worlds, the sun
    // wins most nights, but a bright-enough night-star still outshines it in
    // a minority of cases.
    assert_eq!(
        moonless_solar, 60,
        "moonless-solar kobold head count drifted"
    );
    assert_eq!(
        moonless_lunar, 10,
        "moonless-lunar kobold head count drifted"
    );
}

#[test]
fn blind_attribution_beats_chance_decisively() {
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (a_i, moons_i, locked_i) = (
        idx("blind-attribution-correct"),
        idx("moons-admitted"),
        idx("tidally-locked"),
    );
    let (mut correct, mut total) = (0u32, 0u32);
    let (mut correct_mooned, mut total_mooned) = (0u32, 0u32);
    for row in &result.rows {
        let mooned = matches!(&row.values[moons_i], MetricValue::Text(n) if n != "0");
        let locked = matches!(&row.values[locked_i], MetricValue::Flag(true));
        match &row.values[a_i] {
            MetricValue::Flag(true) => {
                correct += 1;
                total += 1;
                if mooned && !locked {
                    correct_mooned += 1;
                    total_mooned += 1;
                }
            }
            MetricValue::Flag(false) => {
                total += 1;
                if mooned && !locked {
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
    // discovery for Study 007. Re-measured for the placed observer (Plan 2
    // Task 4, SEQ-4/SEQ-5): a tidally-locked world's habitability floor
    // keeps every flagship this census places on the day side, so a locked
    // pair's domains no longer separate goblin from kobold (both solar) —
    // Rule 1 of `pick_kobold` goes dark for every locked, mooned pair
    // (measured below), pulling the rate down further. The spec's
    // directional preregistration ("well above chance") is still satisfied;
    // by owner decision the preregistered rule stays untouched and the
    // honest measured rate is pinned. Exact counts are pinned at the
    // re-baseline task.
    let accuracy = f64::from(correct) / f64::from(total);
    assert!(
        accuracy >= 0.8,
        "blind attribution at {accuracy:.3} — below the pinned floor"
    );
    // Pinned calibration row (re-measured for the placed observer, Plan 2
    // Task 4; the drift study is 500 seeds, so this is an exact count, not a
    // rate):
    assert_eq!(correct, 413, "blind-attribution count drifted");
    assert_eq!(total, 496, "attributable-pair count drifted");
    // Pinned calibration row — the anti-reskin claim at the head-domain
    // calibration's own scope: restricted to SPINNING pairs on worlds with
    // at least one moon (a tidally-locked pair's domains no longer separate
    // the two species — see above), the fixed rule attributes the kobold
    // pantheon perfectly.
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
    //
    // The Words (Task 9) briefly broke this: glossed names compound evolved
    // lexicon roots, and sound change guarantees inventory membership, not
    // template conformance. The resolution keeps the invariant binding
    // rather than loosening it: `Namer::glossed_name` applies deterministic
    // phonotactic repair (epenthesis first, deletion second — the
    // loanword-adaptation move real languages make; the permanent formula
    // is documented on `repair_phonotactics` in
    // `domains/language/src/naming.rs`) after compounding, so every
    // committed name is template-conform again.
    let result = &*DRIFT;
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
    // Absent (no pantheon this world) is a legitimate skip. Since The Words
    // (Task 9) the metric detects the affix against a re-derived
    // honorific-free GLOSSED epithet (the /v2 epoch), re-composing the
    // belief's site concepts exactly as worldgen did — see
    // `epithet_honorific` in windows/lab/src/metrics.rs.
    let result = &*DRIFT;
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
fn name_gloss_true_is_100_percent_row_by_row() {
    // Preregistered (spec §9.3, Study 011 H1): every committed settlement
    // name-gloss fact composes truthfully from that SAME settlement's own
    // INDEPENDENTLY re-derived site concepts (biome + presiding
    // phenomenon). A broken gloss pipeline is falsifiably caught here —
    // this would read false, not skip silently.
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let gloss_i = idx("name-gloss-true");
    let (mut checked, mut absent) = (0u32, 0u32);
    for row in &result.rows {
        match row.values[gloss_i] {
            MetricValue::Flag(v) => {
                checked += 1;
                assert!(
                    v,
                    "seed {}: a settlement name-gloss is not truthful to its own site facts",
                    row.seed
                );
            }
            MetricValue::Absent => absent += 1,
            ref other => panic!("seed {}: name-gloss-true not a flag: {other:?}", row.seed),
        }
    }
    assert!(
        checked > 0,
        "no world in the drift census glossed a settlement"
    );
    assert_eq!(
        checked + absent,
        result.rows.len() as u32,
        "row count drifted"
    );
}

#[test]
fn lexicon_is_regular_and_exposure_sound_for_both_species() {
    // Preregistered (spec §9.1/§9.2, Study 011 H2): every Root's recorded
    // derivation replays exactly through evolve, and exposure
    // classification is sound (no Root minted for a concept an
    // INDEPENDENT re-derivation classifies Unknown, every Gap reasoned).
    // Row-by-row, both species; Absent is a legitimate skip (no Root / no
    // lexicon entries this world).
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    for species in ["goblin", "kobold"] {
        let (reg_i, sound_i) = (
            idx(&format!("lexicon-regular-{species}")),
            idx(&format!("exposure-sound-{species}")),
        );
        for row in &result.rows {
            match row.values[reg_i] {
                MetricValue::Flag(v) => {
                    assert!(v, "seed {}: {species} lexicon is not regular", row.seed)
                }
                MetricValue::Absent => {}
                ref other => panic!(
                    "seed {}: lexicon-regular-{species} not a flag: {other:?}",
                    row.seed
                ),
            }
            match row.values[sound_i] {
                MetricValue::Flag(v) => assert!(
                    v,
                    "seed {}: {species} lexicon is not exposure-sound",
                    row.seed
                ),
                MetricValue::Absent => {}
                ref other => panic!(
                    "seed {}: exposure-sound-{species} not a flag: {other:?}",
                    row.seed
                ),
            }
        }
    }
}

#[test]
fn goblin_hue_depth_exceeds_kobold_hue_depth() {
    // Preregistered (spec §9.4, Study 011 H3): the shipped roster's
    // night-vision values predict goblin hue-depth strictly exceeds kobold
    // hue-depth in every present world — a structural constant of the
    // authored perception vectors, not a per-seed draw, so the directional
    // claim and the exact pin below are expected to hold identically at
    // every seed.
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (g_i, k_i) = (idx("hue-depth-goblin"), idx("hue-depth-kobold"));
    let mut checked = 0u32;
    for row in &result.rows {
        if let (MetricValue::Number(g), MetricValue::Number(k)) =
            (&row.values[g_i], &row.values[k_i])
        {
            checked += 1;
            assert!(
                g > k,
                "seed {}: goblin hue-depth {g} does not exceed kobold hue-depth {k}",
                row.seed
            );
            // Pinned: the shipped roster's structural constant.
            assert_eq!(*g, 4.0, "seed {}: goblin hue-depth drifted", row.seed);
            assert_eq!(*k, 2.0, "seed {}: kobold hue-depth drifted", row.seed);
        }
    }
    assert!(checked > 0, "no world carried both species' hue-depth");
}

#[test]
fn name_collision_rate_is_measured_and_pinned() {
    // Preregistered (spec §9.2/§9.5, Study 011 H4): names are pure per-
    // (seed, species, kind, salt) draws with no re-draw, so uniqueness is
    // de-facto rather than enforced (Task 9) — this pins the MEASURED
    // collision rate over the 500-seed drift study as a calibration row,
    // not an invariant.
    //
    // The DIRECTIONAL claim FAILED (reportable per ADR 0016, not adjusted):
    // Study 011 preregistered "below 2x the Tongues-era pinned rate"
    // (2.339% x 2 = 4.678%). The first measurement read 86.28%: pure
    // site-concept compounds (biome + one presiding phenomenon, largely
    // constant across a species' settlements within one world) gave a
    // species only a handful of distinct names against up to ~120
    // settlements. Fix 1 (the settlement stem — a per-salt drawn toponymic
    // unique element compounded with the site words,
    // `Namer::glossed_name`) re-widened the space to a 10.71% mean; fix 2
    // (stem widened from 1-2 to 2-3 syllables, the retired Tongues-era
    // stem's own range) reached 4.91% — a ~17.6x improvement over the
    // defect, but STILL (narrowly) above the preregistered bound, so H4's
    // verdict remains failed (Study 011 records all three measurements;
    // whether 4.678% was the right bound is the campaign owner's call).
    // The honest rate is pinned here exactly as Study 007/008 pin an
    // honest rate that misses its own floor (0.875 blind attribution) —
    // never loosened to fit.
    let result = &*DRIFT;
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
    // Pinned calibration row (re-measured after collision fix 2 AND the
    // merge of main: The Words' glossed compounds set the base rate; main's
    // placed-observer hemisphere culling, extended to per-settlement
    // vantages for glossed naming, means each settlement's own culled sky
    // feeds its presiding concept — re-pinned on the merged code, 500-seed
    // drift study). The per-settlement skies IMPROVED the rate (pre-merge:
    // 148 zero / 352 nonzero, mean 4.91%): more distinct presiding
    // concepts, a wider descriptor space, fewer repeated compounds.
    assert_eq!(zero, 159, "zero-collision world count drifted");
    assert_eq!(nonzero, 341, "nonzero-collision world count drifted");
    assert_eq!(absent, 0, "absent name-collision-rate count drifted");
    let present = zero + nonzero;
    assert!(present > 0, "no worlds with a measurable collision rate");
    let mean = sum / f64::from(present);
    assert!(
        (mean - 0.047_015_587_357_954).abs() < 1e-6,
        "mean name-collision-rate drifted: {mean:.15}"
    );
}

#[test]
fn name_length_distributions_are_measured_and_pinned() {
    // Preregistered (spec §9.2, Study 011's H4 companion): mean generated-
    // name length, per species, pinned over the 500-seed drift study as a
    // calibration row after measurement — the naming/voice baseline's
    // other half (contrast `phonotactic_validity_is_true_for_every_
    // generated_name`, which is an invariant, not a measurement).
    // Re-measured after collision fix 2: a glossed settlement name is now
    // site word(s) + a drawn 2-3-syllable unique stem, so names run LONGER
    // than the pure-compound first measurement (goblin 6.69, kobold 6.91),
    // the fix-1 1-2-syllable stems (10.77 / 11.13), and the Tongues-era
    // free-stem draw (9.87 / 9.80) — consistent with the collision-rate
    // improvement above: a wider, less-repeated vocabulary of longer
    // compound words.
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    // Re-measured on the merged code (was goblin 13.8119 / kobold 14.2369
    // pre-merge): main's placed-observer hemisphere culling, extended to
    // per-settlement vantages for glossed naming, shifts which presiding
    // concept each settlement compounds over, moving both means by a
    // fraction of a character.
    for (species, expected_present, expected_mean) in [
        ("goblin", 498u32, 13.869_961_501_975_723),
        ("kobold", 498u32, 14.262_681_953_972_956),
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
            "{species} mean name length drifted: {mean:.15}"
        );
    }
}

#[test]
fn null_control_blind_attribution_is_at_chance() {
    let result = &*MEETING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    // Collect (domain, cyclic_share, size) per seed for each solo pin set.
    let g = collect_sig(
        result,
        "goblin-solo",
        idx("head-deity-domain-goblin"),
        idx("pantheon-cyclic-share-goblin"),
        idx("pantheon-size-goblin"),
    );
    let t = collect_sig(
        result,
        "goblin-twin-solo",
        idx("head-deity-domain-goblin-twin"),
        idx("pantheon-cyclic-share-goblin-twin"),
        idx("pantheon-size-goblin-twin"),
    );
    let (mut picks_twin, mut decided, mut indistinguishable, mut pairs) = (0u32, 0u32, 0u32, 0u32);
    for (seed, gs) in &g {
        let Some(ts) = t.get(seed) else { continue };
        pairs += 1;
        match pick_second([gs, ts]) {
            Some(1) => {
                decided += 1;
                picks_twin += 1;
            }
            Some(_) => {
                decided += 1;
            }
            None => {
                indistinguishable += 1;
            }
        }
    }
    // Direction (preregistered): decisively NOT separable — most pairs
    // indistinguishable, and among decided pairs the twin is picked ~half.
    assert!(pairs > 0, "no attributable solo pairs");
    assert!(
        indistinguishable as f64 / pairs as f64 > 0.5,
        "expected the null control to be mostly indistinguishable, got {indistinguishable}/{pairs}"
    );
    // Dormant by design for this perfect vector-clone: `decided` is asserted
    // to be exactly 0 below, so this branch never runs today. It stays as a
    // defensive guard that would only activate if a future non-identical
    // null control ever produced a decided pair.
    if decided > 0 {
        let rate = picks_twin as f64 / decided as f64;
        assert!(
            (rate - 0.5).abs() < 0.2,
            "twin-pick rate {rate:.3} not at chance"
        );
    }
    // Pinned calibration row (measured 2026-07-09, 500-seed census-of-the-meeting).
    // The null control is even stronger than the directional floor: EVERY one of
    // the 500 solo pairs is indistinguishable under the pick_kobold rule. Both
    // goblin-vectored species land in identical cells, draw the same head-deity
    // domain and pantheon cyclic-share and size, so no tier of the rule ever
    // separates them — decided (and thus picks_twin) is exactly zero.
    assert_eq!(indistinguishable, 500, "indistinguishable count drifted");
    assert_eq!(decided, 0, "decided count drifted");
    assert_eq!(picks_twin, 0, "twin-pick count drifted");
}

#[test]
fn null_control_distributions_are_within_the_sampling_bound() {
    let result = &*MEETING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    // Categorical: total-variation distance; numeric: standardized mean diff.
    // Bound: the conservative independent-two-sample envelope (spec §4.2). The
    // two solo builds share seed/cell/phenomena ⇒ POSITIVELY correlated ⇒ true
    // distances are smaller than independence predicts, so this bound is safe.
    let cat = |a: &str, b: &str| {
        tv_distance(
            text_dist(result, "goblin-solo", idx(a)),
            text_dist(result, "goblin-twin-solo", idx(b)),
        )
    };
    let num = |a: &str, b: &str| {
        std_mean_diff(
            nums(result, "goblin-solo", idx(a)),
            nums(result, "goblin-twin-solo", idx(b)),
        )
    };
    let head = cat("head-deity-domain-goblin", "head-deity-domain-goblin-twin");
    let cult = cat("cult-form-goblin", "cult-form-goblin-twin");
    let size = num("pantheon-size-goblin", "pantheon-size-goblin-twin");
    let namelen = num("name-length-goblin", "name-length-goblin-twin");
    // Directional (preregistered): all small — the twin is a goblin.
    // n≈480 present rows/side; a 3σ two-sample envelope: TVD < ~0.15, |SMD| < ~0.2.
    assert!(head < 0.15, "head-domain TVD {head:.4} exceeds the bound");
    assert!(cult < 0.15, "cult-form TVD {cult:.4} exceeds the bound");
    assert!(
        size.abs() < 0.2,
        "pantheon-size SMD {size:.4} exceeds the bound"
    );
    assert!(
        namelen.abs() < 0.2,
        "name-length SMD {namelen:.4} exceeds the bound"
    );
    // Pinned STRUCTURAL rows (exact zeroes, not measurements): the two solo
    // builds share seed, cell, and phenomena, so the head-deity domain and
    // cult form distributions and the pantheon-size mean are byte-identical
    // (TVD = SMD = 0) regardless of what names are drawn — naming never
    // feeds back into pantheon structure. Exact even after the merge of
    // main (placed observer, astronomy synodic fix): those shift name salts,
    // not pantheon structure. Only name-length diverges (the lone structural
    // trace of the two distinct names); its exact pinned SMD is a measurement
    // and lives in the Task-12-owned sibling test below.
    assert!((head - 0.0).abs() < 1e-9, "head-domain TVD drifted: {head}");
    assert!((cult - 0.0).abs() < 1e-9, "cult-form TVD drifted: {cult}");
    assert!(
        (size - 0.0).abs() < 1e-9,
        "pantheon-size SMD drifted: {size}"
    );
}

#[test]
fn null_control_name_length_smd_is_pinned() {
    // Re-measured on the merged code (was -0.118235 at the Tongues-era
    // measurement, -0.045751 at Study 011's first, pre-fix measurement,
    // -0.050617 after fix 1, -0.066905 after fix 2, all 2026-07-09; the
    // merge added per-settlement culled vantages for glossed naming): each
    // naming re-baseline shifts the underlying name-length distribution
    // (see `name_length_distributions_are_measured_and_pinned`), so the
    // twin's SMD against the goblin moves too — still comfortably inside
    // the ±0.2 sampling-theory bound `null_control_distributions_are_
    // within_the_sampling_bound` asserts, unaffected by this re-pin.
    let result = &*MEETING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let namelen = std_mean_diff(
        nums(result, "goblin-solo", idx("name-length-goblin")),
        nums(result, "goblin-twin-solo", idx("name-length-goblin-twin")),
    );
    assert!(
        (namelen - -0.065_377_251_231_494).abs() < 1e-9,
        "name-length SMD drifted: {namelen}"
    );
}

/// A solo pantheon's pick_kobold-relevant signature.
struct Sig {
    domain: String,
    cyclic_share: f64,
    size: f64,
}

/// Per-seed signatures for one pin set (rows where the pantheon exists).
fn collect_sig(
    r: &RunResult,
    pin_set: &str,
    d: usize,
    c: usize,
    s: usize,
) -> std::collections::BTreeMap<u64, Sig> {
    let mut out = std::collections::BTreeMap::new();
    for row in r.rows.iter().filter(|row| row.pin_set == pin_set) {
        if let (MetricValue::Text(domain), MetricValue::Number(cyclic), MetricValue::Number(size)) =
            (&row.values[d], &row.values[c], &row.values[s])
        {
            out.insert(
                row.seed,
                Sig {
                    domain: domain.clone(),
                    cyclic_share: *cyclic,
                    size: *size,
                },
            );
        }
    }
    out
}

/// The pick_kobold rule (spec §4), reimplemented independently: lunar, then
/// more-cyclic, then larger; None when identical. Returns the index picked.
fn pick_second(pair: [&Sig; 2]) -> Option<usize> {
    match (pair[0].domain == "lunar", pair[1].domain == "lunar") {
        (true, false) => return Some(0),
        (false, true) => return Some(1),
        _ => {}
    }
    if pair[0].cyclic_share != pair[1].cyclic_share {
        return Some(if pair[0].cyclic_share > pair[1].cyclic_share {
            0
        } else {
            1
        });
    }
    if pair[0].size != pair[1].size {
        return Some(if pair[0].size > pair[1].size { 0 } else { 1 });
    }
    None
}

/// Empirical categorical distribution of a Text column over a pin set.
fn text_dist(r: &RunResult, pin_set: &str, col: usize) -> std::collections::BTreeMap<String, f64> {
    let mut counts: std::collections::BTreeMap<String, u32> = std::collections::BTreeMap::new();
    let mut n = 0u32;
    for row in r.rows.iter().filter(|row| row.pin_set == pin_set) {
        if let MetricValue::Text(t) = &row.values[col] {
            *counts.entry(t.clone()).or_default() += 1;
            n += 1;
        }
    }
    counts
        .into_iter()
        .map(|(k, c)| (k, f64::from(c) / f64::from(n.max(1))))
        .collect()
}

/// Total-variation distance between two categorical distributions.
fn tv_distance(
    a: std::collections::BTreeMap<String, f64>,
    b: std::collections::BTreeMap<String, f64>,
) -> f64 {
    let mut keys: std::collections::BTreeSet<String> = a.keys().cloned().collect();
    keys.extend(b.keys().cloned());
    0.5 * keys
        .iter()
        .map(|k| (a.get(k).copied().unwrap_or(0.0) - b.get(k).copied().unwrap_or(0.0)).abs())
        .sum::<f64>()
}

/// Present numeric values of a column over a pin set.
fn nums(r: &RunResult, pin_set: &str, col: usize) -> Vec<f64> {
    r.rows
        .iter()
        .filter(|row| row.pin_set == pin_set)
        .filter_map(|row| {
            if let MetricValue::Number(n) = row.values[col] {
                Some(n)
            } else {
                None
            }
        })
        .collect()
}

#[test]
fn obliquity_range_is_wider_on_moonless_worlds() {
    // A moonless world keeps the full drawn obliquity wobble; any moon's
    // tidal stabilization damps it (SKY-21, generate_forcing's `damping =
    // 1/(1+stabilization)` term). At equal base draw this is exact — but the
    // base wobble is itself an independent per-seed draw (`base_wobble`,
    // 0-2.5°), so a strict per-row claim (every moonless row exceeds every
    // mooned row) is too strong across a 500-seed population: a moonless
    // world can draw a small base wobble and a mooned world a large one.
    // The population-level claim the moon-coupling calibration authorizes
    // (spec §8, the sixth calibration in the family) is the MEAN comparison:
    // moonless worlds' mean obliquity-range strictly exceeds mooned worlds'.
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (range_i, moons_i) = (idx("obliquity-range"), idx("moons-admitted"));
    let (mut moonless_sum, mut moonless_n) = (0.0_f64, 0u32);
    let (mut mooned_sum, mut mooned_n) = (0.0_f64, 0u32);
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        let MetricValue::Number(range) = row.values[range_i] else {
            panic!("seed {}: obliquity-range not a number", row.seed);
        };
        let mooned = matches!(&row.values[moons_i], MetricValue::Text(n) if n != "0");
        if mooned {
            mooned_sum += range;
            mooned_n += 1;
        } else {
            moonless_sum += range;
            moonless_n += 1;
        }
    }
    assert!(moonless_n > 0, "no moonless worlds in the drift study");
    assert!(mooned_n > 0, "no mooned worlds in the drift study");
    let moonless_mean = moonless_sum / f64::from(moonless_n);
    let mooned_mean = mooned_sum / f64::from(mooned_n);
    assert!(
        moonless_mean > mooned_mean,
        "moonless mean obliquity-range {moonless_mean:.4} !> mooned mean {mooned_mean:.4}"
    );
}

/// Standardized mean difference (mean gap in pooled-standard-deviation units).
fn std_mean_diff(a: Vec<f64>, b: Vec<f64>) -> f64 {
    let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len().max(1) as f64;
    let var = |v: &[f64], m: f64| {
        v.iter().map(|x| (x - m).powi(2)).sum::<f64>() / (v.len().max(1) as f64)
    };
    let (ma, mb) = (mean(&a), mean(&b));
    let pooled = ((var(&a, ma) + var(&b, mb)) / 2.0).sqrt();
    if pooled == 0.0 {
        0.0
    } else {
        (ma - mb) / pooled
    }
}
