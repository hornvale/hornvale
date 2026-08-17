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
use hornvale_scene::{Sight, SurroundsScene, surrounds_scene_colored_in};
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
        },
    )
    .expect("colored surrounds scene builds over the flagship band")
}
