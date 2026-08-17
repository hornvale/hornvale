//! Illumination task-1 probe. Answers spec §6.1, §6.2, §6.3 with real
//! output, and captures H1's bedrock baseline (spec §7). Not a test: it
//! measures, it does not assert.
//!
//! Run: `cargo run -p hornvale-scene --example illumination_probe`
//!
//! §6.2 (how many callers does `reflectance_at` have) is answered by grep,
//! not by this binary — see the campaign report for the count and the
//! file:line list.

use hornvale_kernel::color::standard_observer;
use hornvale_kernel::math::unit_sphere_from_lat_lon;
use hornvale_kernel::{RoomAddr, RoomId, Seed, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_scene::{Sight, SurroundsScene, surrounds_scene, surrounds_scene_colored_in};
use hornvale_terrain::water::WaterKind;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::collections::BTreeSet;

/// The canonical fixture seed this campaign's spec measures against
/// throughout (§6.1, §7/H1).
const SEED: u64 = 42;

/// The walk band's sense radius, in BFS rings — `PURVIEW_RADIUS`
/// (`windows/vessel/src/purview.rs:17`), the chart radius the vessel
/// actually ships. `windows/scene` cannot depend on `windows/vessel`
/// (layering: a window may not depend on a sibling window that itself
/// depends on it — see `windows/scene/Cargo.toml`), so the value is
/// re-stated here rather than imported; it is a `u32` constant, not a
/// piece of logic that could drift out of sync silently.
pub const WALK_BAND_RADIUS: u32 = 4;

/// Build the seed-42 world the CLI's `new --seed 42` does: generated sky,
/// default terrain/settlement pins.
fn genesis() -> World {
    build_world(
        Seed(SEED),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

/// The exact walk-band population this probe's H1 bedrock baseline is
/// measured over, and the population Task 6 must recount over to answer
/// whether H1 held. Defined ONCE, here, and exported so a later task can
/// import it rather than re-deriving a population that might not match.
///
/// The band: seed 42, centred on the flagship settlement's own address
/// (the same observer `windows/scene/tests/golden.rs::
/// surrounds_seed_42_flagship_json` pins — a verified 31-cell, all-land
/// neighbourhood), at `WALK_BAND_RADIUS`, `WorldTime::GENESIS`, coloured
/// under the CIE standard observer and the world star's own daylight (the
/// same construction `windows/scene/src/surrounds.rs`'s test-only `colored`
/// helper uses). Colouring the band here — rather than handing back the
/// uncoloured committed-path scene — is deliberate: H1 and Task 6 both need
/// `cell.color` populated to count distinct colours at all.
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

/// The corner cell that dominates a room's blend: the highest-weight of the
/// three, tie-broken by lowest `CellId` — the same rule
/// `windows/locale/src/lib.rs`'s private `dominant_corner` applies (biome,
/// water, cave all inherit from it), reimplemented here because that helper
/// is not `pub`.
fn dominant_cell(weights: &[(hornvale_kernel::CellId, u64); 3]) -> hornvale_kernel::CellId {
    let mut best = weights[0];
    for &candidate in &weights[1..] {
        if candidate.1 > best.1 || (candidate.1 == best.1 && candidate.0 < best.0) {
            best = candidate;
        }
    }
    best.0
}

/// The highest-relief LAND cell reachable in `scene`'s band, as a
/// `(CellId, elevation_m)` pair — `None` if the band has no land cell at
/// all (every cell's dominant corner is water).
fn highest_relief_land_cell(
    world: &World,
    ctx: &LocaleContext,
    scene: &SurroundsScene,
) -> Option<(hornvale_kernel::CellId, f64)> {
    let geo = ctx.climate().geosphere();
    let mut best: Option<(hornvale_kernel::CellId, f64)> = None;
    for cell in &scene.cells {
        let addr = match RoomId(cell.room).unpack() {
            Ok(addr) => addr,
            Err(_) => continue,
        };
        let Some(weights) = addr.corner_weights(geo, ctx.nearest_index()) else {
            continue;
        };
        let dominant = dominant_cell(&weights);
        // "Land" here means terrestrial, not "no water feature at all": a
        // river cell is still ground you can stand relief on, and this
        // seed's flagship band turns out to run entirely along one (see the
        // task-1 report). Only `Ocean`/`SaltBasin` are non-terrestrial.
        if matches!(
            ctx.terrain().water_kind_at(dominant),
            WaterKind::Ocean | WaterKind::SaltBasin
        ) {
            continue;
        }
        let elevation_m = ctx.terrain().elevation_at(dominant).get();
        if best.is_none_or(|(_, best_elev)| elevation_m > best_elev) {
            best = Some((dominant, elevation_m));
        }
    }
    let _ = world; // kept in the signature: a future caller may need the world too
    best
}

/// The single highest-elevation LAND cell anywhere on the seed-42 globe
/// (not restricted to any one walk band) — the widened check §6.1's branch
/// table calls for when the first, band-restricted sample comes back flat.
/// Same land predicate as [`highest_relief_land_cell`].
fn global_highest_relief_land_cell(ctx: &LocaleContext) -> (hornvale_kernel::CellId, f64) {
    let globe = ctx.terrain().globe();
    globe
        .elevation
        .iter()
        .filter(|&(id, _)| {
            !matches!(
                *globe.water_kind.get(id),
                WaterKind::Ocean | WaterKind::SaltBasin
            )
        })
        .map(|(id, elev)| (id, elev.get()))
        .max_by(|(_, a), (_, b)| a.total_cmp(b))
        .expect("seed 42 has at least one land cell")
}

/// The land cell whose ANNUAL-MEAN temperature sits closest to the freeze
/// line (0 C) — the place a small seasonal swing is most likely to carry a
/// cell across `is_frozen_at`'s threshold, if the swing can do that
/// anywhere on this seed. Cheaper than a seed sweep and a sharper test of
/// "is the seasonal term observable on seed 42 AT ALL" than either extreme
/// alone: the global peak sampled by [`global_highest_relief_land_cell`]
/// sits so far below freezing (annual mean, not just the sampled days) that
/// no plausible seasonal amplitude would carry it back above 0 C, so a
/// year-round "frozen" reading there is uninformative about whether the
/// term itself is observable.
fn nearest_to_freezing_land_cell(ctx: &LocaleContext) -> (hornvale_kernel::CellId, f64) {
    let globe = ctx.terrain().globe();
    globe
        .elevation
        .iter()
        .filter(|&(id, _)| {
            !matches!(
                *globe.water_kind.get(id),
                WaterKind::Ocean | WaterKind::SaltBasin
            )
        })
        .map(|(id, _)| (id, ctx.climate().mean_temperature_at(id).get()))
        .min_by(|(_, a), (_, b)| a.abs().total_cmp(&b.abs()))
        .expect("seed 42 has at least one land cell")
}

/// Sample `temperature_at`/`is_frozen_at` at `cell` across 8 evenly spaced
/// days of one std year, print each sample, and return
/// `(min_c, max_c, frozen_count_of_8)`.
fn sample_year(
    ctx: &LocaleContext,
    cell: hornvale_kernel::CellId,
    year_length: f64,
) -> (f64, f64, usize) {
    let mut temps = Vec::new();
    let mut frozen_flags = Vec::new();
    for i in 0..8u32 {
        let day = year_length * (i as f64) / 8.0;
        let temp = ctx.climate().temperature_at(cell, day).get();
        let frozen = ctx.climate().is_frozen_at(cell, day);
        println!("  day {day:8.2}  temperature_c {temp:8.3}  is_frozen_at {frozen}");
        temps.push(temp);
        frozen_flags.push(frozen);
    }
    let min_t = temps.iter().cloned().fold(f64::INFINITY, f64::min);
    let max_t = temps.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let frozen_count = frozen_flags.iter().filter(|&&f| f).count();
    println!(
        "  temperature range across the year: {min_t:.3} .. {max_t:.3} C (swing {:.3} C)",
        max_t - min_t
    );
    println!(
        "  frozen at {frozen_count}/{} sampled days",
        frozen_flags.len()
    );
    (min_t, max_t, frozen_count)
}

fn main() {
    let world = genesis();
    let ctx = LocaleContext::build(&world).expect("seed 42 builds a locale context");
    let year_length = ctx.climate().year_length_std();

    println!("=== Illumination task-1 probe (seed {SEED}) ===\n");
    println!("year length (std days): {year_length}\n");

    // ---------------------------------------------------------------
    // §6.1 — does seed 42 have a season worth seeing?
    // ---------------------------------------------------------------
    println!(
        "--- §6.1a: seasonal variation at the highest-relief land cell IN the H1 walk band ---"
    );
    let band = baseline_band(&world);
    match highest_relief_land_cell(&world, &ctx, &band) {
        None => {
            println!(
                "no land cell found in the {WALK_BAND_RADIUS}-radius walk band \
                 (every dominant corner is water) — cannot sample seasonality here"
            );
        }
        Some((cell, elevation_m)) => {
            println!("highest-relief land cell in band: {cell:?}, elevation {elevation_m:.1} m");
            sample_year(&ctx, cell, year_length);
        }
    }
    println!();

    // The band above sits entirely within ONE canonical grid cell (a
    // tropical rainforest river cell) — every one of its 31 rooms shares the
    // same dominant corner, so "the highest-relief cell in the band" is not
    // a meaningful contrast on this seed. §6.1's branch table calls this out
    // explicitly ("unobservable on this seed -> widen ... before
    // concluding"), so widen within budget: the true global relief maximum
    // on seed 42, still not a seed sweep, but no longer pinned to a flat
    // river cell.
    println!("--- §6.1b: widened — the GLOBAL highest-relief land cell on seed {SEED} ---");
    let (global_cell, global_elev) = global_highest_relief_land_cell(&ctx);
    println!("global highest-relief land cell: {global_cell:?}, elevation {global_elev:.1} m");
    sample_year(&ctx, global_cell, year_length);
    println!();

    // A second widening: the annual mean at the global relief peak is so far
    // below freezing that "frozen 8/8" there says nothing about whether a
    // seasonal swing can ever cross the threshold on this seed. Find the
    // land cell whose ANNUAL MEAN sits closest to 0 C instead.
    println!("--- §6.1c: widened — the land cell nearest the freeze line on its annual MEAN ---");
    let (marginal_cell, marginal_mean) = nearest_to_freezing_land_cell(&ctx);
    println!("nearest-to-freezing land cell: {marginal_cell:?}, annual mean {marginal_mean:.3} C");
    sample_year(&ctx, marginal_cell, year_length);
    println!();

    // ---------------------------------------------------------------
    // §6.2 — how many callers does reflectance_at have?
    // ---------------------------------------------------------------
    println!("--- §6.2: reflectance_at caller count ---");
    println!("Answered by grep, not by this binary — see the task-1 report.");
    println!();

    // ---------------------------------------------------------------
    // §6.3 — is `color` populated in the committed fixtures' build path?
    // ---------------------------------------------------------------
    println!("--- §6.3: is `color` populated on the committed (uncoloured) path? ---");
    let seed1_world = build_world(
        Seed(1),
        &Default::default(),
        SkyChoice::Constant,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 1 builds");
    let observer = RoomAddr::containing(unit_sphere_from_lat_lon(0.0, 0.0), 6);
    let committed_scene = surrounds_scene(
        &seed1_world,
        &observer,
        WALK_BAND_RADIUS,
        WorldTime::GENESIS,
    )
    .expect("uncoloured surrounds scene builds (the committed-fixture path)");
    let any_colored = committed_scene.cells.iter().any(|c| c.color.is_some());
    let all_none = committed_scene.cells.iter().all(|c| c.color.is_none());
    println!(
        "committed path (surrounds_scene, the fixtures' own builder): \
         {} cells, any Some(color)? {any_colored}, all None? {all_none}",
        committed_scene.cells.len()
    );
    println!(
        "(for contrast) the coloured path (surrounds_scene_colored_in) on the \
         H1 band: any Some(color)? {}",
        band.cells.iter().any(|c| c.color.is_some())
    );
    println!();

    // ---------------------------------------------------------------
    // §7 / H1 — the bedrock-era distinct-colour count. THIS IS THE ONLY
    // MOMENT THIS NUMBER CAN BE TAKEN: every later task in this campaign
    // changes what `color` means.
    // ---------------------------------------------------------------
    println!("--- §7/H1: bedrock baseline over the seed-42 walk band ---");
    let cell_count = band.cells.len();
    let distinct_colors: BTreeSet<Option<[u8; 3]>> = band.cells.iter().map(|c| c.color).collect();
    println!("cells.len() = {cell_count}");
    println!("distinct color count = {}", distinct_colors.len());
    println!(
        "(H1 floor for later tasks: strictly greater than {}; \
         H1 ceiling: strictly less than {cell_count})",
        distinct_colors.len()
    );
}
