//! [`source_standing`] — the leader-and-margin readout — and the two things
//! that had to be proved before it could ship.
//!
//! 1. **`dominant_source` still answers exactly what it answered before.** It
//!    now delegates here instead of deriving the leader itself, which removes
//!    a second derivation that could drift — but the old behaviour, including
//!    its tie-break, reaches committed prose (`book/src/gallery/
//!    possession-seed-42.md` names the source in every chamber a walk enters).
//!    A silent change there would re-word chambers on every seed. So the
//!    equivalence is asserted against an **independent** re-derivation of the
//!    original expression, not assumed from the refactor being small.
//! 2. **"Contested" describes some chambers and not others.** A threshold that
//!    called every chamber contested, or none, would be a word carrying no
//!    information. This is the same occupancy argument the metabolite bands
//!    are built on, applied to a narrative distinction instead of a numeric
//!    one.
use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed};
use hornvale_terrain::{TerrainPins, delve::rung_evaluation_depth_m};
use hornvale_worldgen::energy::{CONTESTED_SHARE, EnergySource, dominant_source, source_standing};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to_with_artifacts, climate_of,
    substrate_field, subterranean_substrate_field_per_rung,
};

/// Seeds sampled — three, because one world is an anecdote (ledger #45).
const SEEDS: [u64; 3] = [0, 7, 42];

/// The five underground rungs.
const RUNGS: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// One sampled reading's inputs, so both derivations see identical arguments.
struct Reading {
    material: hornvale_terrain::MaterialBuffer,
    gradient: hornvale_terrain::GeothermalGradient,
    depth_m: f64,
    moisture: f64,
    drainage: f64,
}

/// Every (vertex, rung) reading over `SEEDS` that carries a chamber.
fn readings(wc: &WorldComponents) -> Vec<Reading> {
    let mut out = Vec::new();
    for seed in SEEDS {
        let artifacts = build_world_to_with_artifacts(
            Seed(seed),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let world = artifacts.world;
        let terrain = artifacts.terrain.expect("terrain at BuildDepth::Terrain");
        let climate = climate_of(&world).expect("climate reconstructs");
        let geo = terrain.geosphere();
        let surface = substrate_field(
            geo,
            &terrain,
            &climate,
            climate.obliquity_deg(),
            climate.insolation(),
            &climate.regime(),
        );
        let per_rung = subterranean_substrate_field_per_rung(geo, &terrain, &surface);
        for vertex in geo.vertices() {
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            let material = terrain.material_at(vertex);
            let gradient = terrain.geothermal_gradient_at(vertex);
            let drainage = terrain.drainage_at(vertex);
            let subs = per_rung.get(vertex);
            for rung in RUNGS {
                let Some(sub) = subs[rung as usize].as_ref() else {
                    continue;
                };
                let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                else {
                    continue;
                };
                out.push(Reading {
                    material,
                    gradient,
                    depth_m,
                    moisture: sub.moisture,
                    drainage,
                });
            }
        }
    }
    out
}

/// claim: invariant(delegation-is-behaviour-preserving) — `dominant_source`
/// delegates to [`source_standing`] now, and this is the assertion that the
/// refactor changed nothing a reader can see. The right-hand side is the
/// ORIGINAL expression, re-derived here rather than referenced, so the two
/// sides are independent: if the delegation had quietly altered the tie-break,
/// this fails rather than agreeing with itself.
#[test]
fn dominant_source_still_matches_the_expression_it_replaced() {
    let wc = WorldComponents::assemble().expect("registries");
    let sample = readings(&wc);
    assert!(
        sample.len() > 10_000,
        "a delegation-equivalence check over {} readings is too small to be evidence",
        sample.len()
    );
    let mut ties = 0usize;
    for r in &sample {
        let original = EnergySource::ALL
            .iter()
            .copied()
            .max_by(|a, b| {
                a.yield_at(&r.material, r.gradient, r.depth_m, r.moisture, r.drainage)
                    .total_cmp(&b.yield_at(
                        &r.material,
                        r.gradient,
                        r.depth_m,
                        r.moisture,
                        r.drainage,
                    ))
            })
            .expect("EnergySource::ALL is non-empty");
        let delegated = dominant_source(&r.material, r.gradient, r.depth_m, r.moisture, r.drainage);
        assert_eq!(
            original, delegated,
            "the delegated dominant_source disagrees with the expression it replaced at a \
             reading with depth {} moisture {} drainage {} — committed chamber prose names this \
             source, so a disagreement re-words the world",
            r.depth_m, r.moisture, r.drainage
        );
        let standing = source_standing(&r.material, r.gradient, r.depth_m, r.moisture, r.drainage);
        if standing.leader_yield == standing.runner_up_yield {
            ties += 1;
        }
        assert!(
            standing.leader_yield >= standing.runner_up_yield,
            "the runner-up out-yields the leader — the standing is not ordered"
        );
    }
    eprintln!(
        "STANDING equivalence: {} readings agree, {ties} exact ties exercised the tie-break",
        sample.len()
    );
}

/// claim: invariant(contested-and-settled-both-occur) — occupancy for a
/// narrative distinction. A "contested" that never fires, or that fires
/// everywhere, is a word that tells a reader nothing.
#[test]
fn a_chamber_can_be_contested_and_can_be_settled() {
    let wc = WorldComponents::assemble().expect("registries");
    let sample = readings(&wc);
    let (mut contested, mut settled, mut barren) = (0usize, 0usize, 0usize);
    for r in &sample {
        let s = source_standing(&r.material, r.gradient, r.depth_m, r.moisture, r.drainage);
        if s.is_barren() {
            barren += 1;
        } else if s.is_contested() {
            contested += 1;
        } else {
            settled += 1;
        }
    }
    let n = sample.len() as f64;
    let share = contested as f64 / n;
    eprintln!(
        "STANDING contested={contested} ({:.1}%) settled={settled} barren={barren} at \
         CONTESTED_SHARE={CONTESTED_SHARE}",
        100.0 * share
    );
    assert!(
        contested > 0 && settled > 0,
        "at CONTESTED_SHARE={CONTESTED_SHARE} the distinction is vacuous: contested={contested}, \
         settled={settled}. One of the two words never applies, so saying it carries no \
         information. Move the threshold against the measured margin distribution."
    );
    assert!(
        (0.02..=0.60).contains(&share),
        "contested fires on {:.1}% of readings, outside the [2%, 60%] band a meaningful \
         narrative distinction needs — too rare to be worth the words, or so common that \
         'contested' is just what a chamber is.",
        100.0 * share
    );
}
