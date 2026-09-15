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
//! **The lexicon inventory's counts for this file and for
//! `illumination_hypotheses.rs` were RAISED by The Pavement's task-10
//! measurement (`docs/audits/lexicon-inventory.tsv`, 2 -> 17 and 37 -> 57),
//! and this is the human reason `cli/tests/suite/lexicon_guard.rs` asks for.**
//! The H1 band's *population* is the subject under measurement — it was 31
//! cells and is now 81 — and `SurroundsScene.cells` is the field's own name,
//! so every occurrence is the AREA/collection sense the inventory already
//! admits, not the mesh-VERTEX sense the guard prohibits. Rewording to
//! synonyms was considered and refused: it would damage accurate prose about a
//! population of lattice squares to satisfy a ratchet aimed at a different
//! defect, and would make the measurement harder to read.
#![allow(dead_code)]

use hornvale_kernel::color::standard_observer;
use hornvale_kernel::math::unit_sphere_from_lat_lon;
use hornvale_kernel::{Facet, Seed, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_scene::{Sight, SurroundsScene, surrounds_scene, surrounds_scene_colored_in};
use hornvale_worldgen::{SettlementPins, build_world};
use std::collections::BTreeSet;

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
    // The Tidemark, Task 3: the DEMO subject, chosen rather than inherited.
    // `village_info` is "the first `is-settlement` fact in ledger order",
    // which was never a claim about where a band should be sampled — and
    // when the marine peoples moved it into open sea, this band went all
    // water and the cover and colour layers stopped varying at all (H1's
    // mixture arm and its bedrock control both collapsed to ONE colour, so
    // the differential could not fire in either direction). See
    // `hornvale_worldgen::land_settlement`'s own doc.
    let village = hornvale_worldgen::land_settlement(world).expect("seed 42 has a dry village");
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
    let depth = hornvale_locale::walk_depth(&ctx);
    let observer_room = Facet::containing(unit_sphere_from_lat_lon(lat, lon), depth);

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
            projection_norms: None,
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
    let depth = hornvale_locale::walk_depth(&ctx);
    let observer_room = Facet::containing(unit_sphere_from_lat_lon(lat, lon), depth);

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
            projection_norms: None,
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
    let depth = hornvale_locale::walk_depth(&ctx);
    let observer_room = Facet::containing(unit_sphere_from_lat_lon(lat, lon), depth);
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

/// The corner vertex that dominates a room's blend: the highest-weight of
/// the four, tie-broken by lowest `Vertex` — the rule `windows/locale/src/
/// lib.rs`'s private `dominant_corner` applies, and the rule
/// `LocaleContext::reflectance_mixture_at` resolves its rock class through.
/// Re-stated here for the same reason `baseline_band` is (that helper is not
/// `pub`, and this file cannot import a private item); the probe carries the
/// identical re-statement as `dominant_vertex`.
fn dominant_corner(weights: &[(hornvale_kernel::Vertex, u64); 4]) -> hornvale_kernel::Vertex {
    let mut best = weights[0];
    for &candidate in &weights[1..] {
        if candidate.1 > best.1 || (candidate.1 == best.1 && candidate.0 < best.0) {
            best = candidate;
        }
    }
    best.0
}

/// **H1's bedrock arm: the distinct sRGB colours `band`'s cells would carry
/// with the surface mixture deleted.** The set, not the count, so a caller
/// can compare cardinalities or membership.
///
/// This is the arm H1's floor is founded on since The Pavement, and the
/// reason it exists is worth stating exactly, because the thing it replaces
/// was declared unrecoverable and *was*.
///
/// H1's floor used to compare against `BEDROCK_BASELINE = 1` — a literal,
/// captured once at commit `b0f20c71` over that era's 31-cell band, with
/// `illumination_probe.rs` recording that "THIS IS THE ONLY MOMENT THIS
/// NUMBER CAN BE TAKEN". That sentence is true of the **number** and false
/// of the **arm**. What `b0f20c71` measured was the output of
/// `LocaleContext::reflectance_at` as it then stood, whose whole body was:
///
/// ```text
/// let weights = addr.corner_weights(geo, &self.index)?;
/// let cell = dominant_corner(&weights).0;
/// let buffer = self.terrain.material_at(cell);
/// let rock = self.terrain.rock_at(cell);
/// Ok(hornvale_terrain::lithology::reflectance(&buffer, rock).integrate())
/// ```
///
/// Every call in it still exists and is still `pub`, so the function can be
/// re-run over ANY band — including one it never saw. The loop below is that
/// body, transcribed, then put through the same `Observer`/`Illuminant` pair
/// [`baseline_band`] colours with, so the two arms differ in exactly one
/// term: the surface cover layer
/// (`LocaleContext::reflectance_mixture_at`'s `mineral * (1 - covered) +
/// cover`).
///
/// **Why the literal could not survive and this can.** The Pavement moved
/// the walk band from icosphere triangles at `globe_level + 6` to
/// cube-sphere quads at `globe_level + 7` (decisions
/// [0506] and [0511]), and the band's extent is defined in BFS rings at the
/// walk depth — rooms, never radians. Measured at the flagship on seed 42:
/// the region grew from 31 cells reaching ~4.3 km to 81 cells reaching
/// 6.49 km, ~47 km² to ~121 km², at essentially unchanged resolution
/// (1.52 km² per triangle, 1.49 km² per quad — 0511 chose the depth to
/// preserve step length, and it preserved cell area with it). So the new
/// band is not the old region sampled more densely; it is a **larger,
/// differently shaped region** containing ~73 km² of ground the baseline
/// never sampled, and "the bedrock was uniform over the old patch" says
/// nothing about it. A same-world, same-band differential needs no such
/// carry-over: it is re-measured from scratch every run.
///
/// [0506]: `docs/decisions/0506-the-occupancy-lattice-is-a-cube-sphere.md`
/// [0511]: `docs/decisions/0511-walk-depth-is-globe-level-plus-seven.md`
pub fn bedrock_colours(world: &World, band: &SurroundsScene) -> BTreeSet<Option<[u8; 3]>> {
    let ctx = LocaleContext::build(world).expect("world builds a locale context");
    let star = hornvale_astronomy::star::generate_star(
        world.seed.derive(hornvale_astronomy::streams::ROOT),
    );
    let light = hornvale_astronomy::illuminant::daylight(&star);
    let observer = standard_observer();
    let geo = ctx.climate().geosphere();
    let mut out = BTreeSet::new();
    for cell in &band.cells {
        let addr = hornvale_kernel::FacetId(cell.room)
            .unpack()
            .expect("a band cell's room id unpacks");
        let weights = addr
            .corner_weights(geo, ctx.nearest_index())
            .expect("a band cell sits at or below the grid");
        let vertex = dominant_corner(&weights);
        let buffer = ctx.terrain().material_at(vertex);
        let rock = ctx.terrain().rock_at(vertex);
        let reflectance = hornvale_terrain::lithology::reflectance(&buffer, rock).integrate();
        out.insert(observer.to_srgb(&observer.sense(&reflectance, &light)));
    }
    out
}
