//! Test-only scaffolding shared by `illumination_hypotheses.rs`.
//!
//! `baseline_band` below is a **deliberate, verbatim transcription** of
//! `windows/scene/examples/illumination_probe.rs::baseline_band`. Cargo
//! examples are binary targets, not libraries — they cannot be imported
//! cross-crate, or even from tests in their own crate — so re-exporting it
//! is not an option, and this is the precedented pattern in this repo
//! (`windows/vessel/tests/common/mod.rs`, `windows/hearsay/tests/common/
//! mod.rs`). Do not "clean up" or refactor this copy independently of the
//! probe: if the two drift, `the_h1_band_is_still_the_population_the_
//! baseline_was_taken_over` (`illumination_hypotheses.rs`) is the guard —
//! it fails loudly on a shape change rather than silently comparing today's
//! colour count against a different population.
#![allow(dead_code)]

use hornvale_kernel::color::standard_observer;
use hornvale_kernel::math::unit_sphere_from_lat_lon;
use hornvale_kernel::{RoomAddr, Seed, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_scene::{Sight, SurroundsScene, surrounds_scene, surrounds_scene_colored_in};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The canonical fixture seed this campaign's spec measures against
/// throughout (§6.1, §7/H1). Transcribed from the probe.
const SEED: u64 = 42;

/// The walk band's sense radius, in BFS rings — `PURVIEW_RADIUS`
/// (`windows/vessel/src/purview.rs:17`), the chart radius the vessel
/// actually ships. Transcribed from the probe (see its own comment for why
/// the value is re-stated rather than imported: `windows/scene` cannot
/// depend on `windows/vessel`).
pub const WALK_BAND_RADIUS: u32 = 4;

/// Build the seed-42 world the CLI's `new --seed 42` does: generated sky,
/// default terrain/settlement pins. Transcribed from the probe.
pub fn genesis() -> World {
    build_world(
        Seed(SEED),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

/// The exact walk-band population Task 1's H1 bedrock baseline was measured
/// over. Transcribed verbatim from `illumination_probe.rs::baseline_band` —
/// see this module's doc comment for why a duplicate, not a shared import,
/// is the deliberate choice here.
pub fn baseline_band(world: &World) -> SurroundsScene {
    let ctx = LocaleContext::build(world).expect("seed 42 builds a locale context");
    let village = hornvale_settlement::village_info(world).expect("seed 42 has a village");
    let lat = match world
        .ledger
        .value_of(village.id, hornvale_settlement::LATITUDE)
    {
        Some(Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no latitude fact"),
    };
    let lon = match world
        .ledger
        .value_of(village.id, hornvale_settlement::LONGITUDE)
    {
        Some(Value::Number(n)) => *n,
        _ => panic!("flagship settlement has no longitude fact"),
    };
    let depth = ctx.globe_level() + 6;
    let observer_room = RoomAddr::containing(unit_sphere_from_lat_lon(lat, lon), depth);

    let star = hornvale_astronomy::star::generate_star(
        world.seed.derive(hornvale_astronomy::streams::ROOT),
    );
    let light = hornvale_astronomy::illuminant::daylight(&star);

    surrounds_scene_colored_in(
        world,
        &ctx,
        &observer_room,
        WALK_BAND_RADIUS,
        WorldTime::GENESIS,
        &standard_observer(),
        &light,
        Sight {
            observer: "standard".to_string(),
            channels: 0,
            chromatic: 0,
            projection: String::new(),
            preserves: String::new(),
            sun_altitude_deg: 0.0,
            // Overwritten by the builder — see `Sight`'s own doc.
            channel_roles: Vec::new(),
            projection_slots: None,
        },
    )
    .expect("colored surrounds scene builds over the flagship band")
}

/// A REAL, non-flagship seed-42 walk band at `(lat, lon)`, coloured the same
/// way [`baseline_band`] is. Not a duplicate of that function in spirit —
/// [`baseline_band`] is pinned to the flagship's own address and must stay
/// that way (Step 0's population guard depends on it); this is the general
/// "colour a band anywhere on the globe" builder Task 6's fix round needed
/// once the flagship band turned out to be unusable for H3 (see
/// [`REAL_H3_BAND_LAT_LON`] and the task-6 report for why).
pub fn real_band(world: &World, lat: f64, lon: f64) -> SurroundsScene {
    let ctx = LocaleContext::build(world).expect("world builds a locale context");
    let depth = ctx.globe_level() + 6;
    let observer_room = RoomAddr::containing(unit_sphere_from_lat_lon(lat, lon), depth);

    let star = hornvale_astronomy::star::generate_star(
        world.seed.derive(hornvale_astronomy::streams::ROOT),
    );
    let light = hornvale_astronomy::illuminant::daylight(&star);

    surrounds_scene_colored_in(
        world,
        &ctx,
        &observer_room,
        WALK_BAND_RADIUS,
        WorldTime::GENESIS,
        &standard_observer(),
        &light,
        Sight {
            observer: "standard".to_string(),
            channels: 0,
            chromatic: 0,
            projection: String::new(),
            preserves: String::new(),
            sun_altitude_deg: 0.0,
            // Overwritten by the builder — see `Sight`'s own doc.
            channel_roles: Vec::new(),
            projection_slots: None,
        },
    )
    .expect("colored surrounds scene builds")
}

/// The same real band, through the **uncoloured** path — no `Observer` at
/// all, every cell's `color` stays `None`. The closest honest proxy for "an
/// observer with no chromatic channel": `hornvale_kernel::color::Observer`
/// has no constructor for a true zero-chromatic eye (`Observer::with_roles`
/// refuses a role set with zero `Chromatic` channels), so this is the one
/// path in the codebase that actually produces the picture such an eye
/// would leave behind.
pub fn real_band_uncolored(world: &World, lat: f64, lon: f64) -> SurroundsScene {
    let ctx = LocaleContext::build(world).expect("world builds a locale context");
    let depth = ctx.globe_level() + 6;
    let observer_room = RoomAddr::containing(unit_sphere_from_lat_lon(lat, lon), depth);
    surrounds_scene(world, &observer_room, WALK_BAND_RADIUS, WorldTime::GENESIS)
        .expect("uncoloured surrounds scene builds")
}

/// A real, non-flagship seed-42 observer position whose walk band carries
/// dry land — Task 6's fix round, Finding 1. The flagship band is unusable
/// for H3 (it is 100% river on every seed sampled — 42, 13, 7, 1, 100 — so
/// colour never reaches the picture regardless of chromatic capability; see
/// the task-6 report). A 288-point globe sweep
/// (`illumination_probe.rs::h3_real_band_sweep`) found this the FIRST
/// qualifying point — a dry-land band whose ground cells actually exhibit
/// the same-glyph/different-colour pairing that makes a coloured render
/// more distinguishable than a monochrome one. That pairing turned out to be
/// the modal case, not a rarity: 53 of 53 seed-42 dry-land bands sampled
/// exhibit it (97 of 101 on seed 13), so this point is representative, not
/// cherry-picked for the result.
pub const REAL_H3_BAND_LAT_LON: (f64, f64) = (-52.5, -150.0);
