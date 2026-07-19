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

/// The SOC-1 gate law, restated: at seed 1, every placed culture's
/// flagship is committed `"organized"` (Task 2's ledger #1 measurement),
/// so `doctrine_of` gates every one to `Some`, and `doctrines_of`'s length
/// matches `placed_peoples`'s exactly. A future roster or genesis change
/// that stops placing an organized culture at seed 1 must redden this
/// assertion, from the lab crate, without needing to touch
/// `windows/worldgen/tests/` at all.
#[test]
fn the_soc1_gate_holds_at_seed_1() {
    let world = generated(1);
    let placed = hornvale_worldgen::placed_peoples(&world);
    assert!(!placed.is_empty(), "seed 1 must place at least one culture");
    for (kind, village) in &placed {
        let cult_form = hornvale_religion::cult_form_held_by(&world, village.id);
        assert_eq!(
            cult_form.as_deref(),
            Some("organized"),
            "seed 1's {kind} flagship must be organized"
        );
        assert!(
            doctrine_of(&world, kind).is_some(),
            "{kind}'s organized flagship must gate doctrine_of to Some"
        );
    }
    assert_eq!(
        doctrines_of(&world).len(),
        placed.len(),
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
