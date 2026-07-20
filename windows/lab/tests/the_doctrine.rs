//! The Doctrine (C6 T4): standing laws restated at the lab level. Both
//! `windows/worldgen/tests/doctrine.rs` (Task 2) and this file measure the
//! SAME committed worlds — the point of the restatement is redundancy: a
//! future campaign that reshapes the placed-people roster, the SOC-1 gate,
//! or the dial's folk-param derivation should redden HERE too, not only in
//! worldgen's own test crate (a lab-scoped change that never touches
//! `windows/worldgen/tests/` at all must still catch a roster regression).
//!
//! Preregistered (before this file was written, against the already-
//! measured values Task 2 pinned): the SOC-1 gate holds at seed 1 (every
//! placed culture's flagship is organized, so `doctrine_of` gates every one
//! to `Some`); the dial-roster law holds structurally (`accounts_of`
//! returns `ChorusVoice`, never `DoctrineVoice` — a compile-time fact this
//! file also exercises at the value level: goblin's folk `sky_capability`
//! is the measured `0.5` exactly, seeds 1..=5, completely unperturbed by
//! the doctrine voice's own `+0.25` delta living only in `doctrines_of`).

use hornvale_worldgen::{SettlementPins, SkyChoice, accounts_of, doctrine_of, doctrines_of};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// worldgen-adjacent integration test uses (`windows/worldgen/tests/doctrine.rs`,
/// `windows/lab/tests/the_dial.rs`).
fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// The SOC-1 gate law, restated: at seed 1, a placed culture's flagship
/// cult-form gates `doctrine_of` exactly — `"organized"` <=> `Some`,
/// `"folk"` <=> `None` — and `doctrines_of`'s length matches the organized
/// subset. Post-Demesne (BIO-35 Stage 1 recalibration), seed-1's goblin is
/// organized while its hobgoblin flipped to folk, so this seed exercises
/// BOTH arms of the gate directly (the anticipated genesis-change reddening
/// this docstring warned of: it no longer holds that *every* placed culture
/// is organized). Verified from the lab crate, without touching
/// `windows/worldgen/tests/` at all.
#[test]
fn the_soc1_gate_holds_at_seed_1() {
    let world = generated(1);
    let placed = hornvale_worldgen::placed_peoples(&world);
    assert!(!placed.is_empty(), "seed 1 must place at least one culture");
    let mut organized_count = 0usize;
    let mut goblin_organized = false;
    for (kind, village) in &placed {
        let cult_form = hornvale_religion::cult_form_held_by(&world, village.id);
        let is_organized = cult_form.as_deref() == Some("organized");
        assert_eq!(
            doctrine_of(&world, kind).is_some(),
            is_organized,
            "seed 1's {kind}: doctrine_of must be Some iff its flagship cult-form is organized \
             (cult_form={cult_form:?})"
        );
        if is_organized {
            organized_count += 1;
        }
        if *kind == "goblin" {
            goblin_organized = is_organized;
        }
    }
    assert!(
        goblin_organized,
        "seed 1's goblin flagship is organized (the seed-1 anchor)"
    );
    assert_eq!(
        doctrines_of(&world).len(),
        organized_count,
        "doctrines_of must cover exactly every organized placed culture"
    );
}

/// The dial-roster law, restated: `accounts_of` never carries a doctrine
/// voice — structurally true (its return type is `Vec<ChorusVoice>`, which
/// has no doctrine field at all), and restated here at the value level: the
/// goblin folk voice's `sky_capability` is the measured `0.5` EXACTLY,
/// seeds 1..=5, with no `+0.25` doctrine-delta leak ever touching the folk
/// account (that delta lives only in `doctrine_of`'s own params, read
/// separately below and confirmed to differ). A future change to the dial
/// that let the doctrine delta leak into the folk roster's own params
/// would redden this exact-value pin.
#[test]
fn the_dial_roster_law_folk_params_are_stable() {
    for seed in 1u64..=5 {
        let world = generated(seed);
        let voices = accounts_of(&world);
        let Some(goblin) = voices.iter().find(|v| v.kind == "goblin") else {
            // Not every seed places a goblin; the law only constrains seeds
            // that do (mirrors `windows/worldgen/tests/chorus_params.rs`'s
            // posture on a kind that a given seed doesn't place).
            continue;
        };
        assert_eq!(
            goblin.params.sky_capability, 0.5,
            "seed {seed}: goblin's folk sky_capability must be the measured 0.5 exactly, \
             unperturbed by the doctrine voice's own +0.25 delta"
        );

        // The doctrine voice's own params DO carry the delta — confirming
        // the two rosters are genuinely separate values, not merely
        // separate types that happen to agree.
        if let Some(doctrine) = doctrine_of(&world, "goblin") {
            assert_eq!(
                doctrine.params.sky_capability, 0.75,
                "seed {seed}: goblin's doctrine sky_capability must be folk (0.5) + 0.25"
            );
        }
    }
}
