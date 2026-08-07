//! THE DELVERS — the condition-axis bind audit.
//!
//! `tolerance_liebig` floors temperature/moisture/insolation by
//! `sovereignty_floor(mass, potency)` and passes elevation a literal `0.0`.
//! `ConditionResponse::eval` is `floor + (1 - floor) * devotion * bump`, so
//! elevation's value never exceeds its `devotion` while the other three never
//! fall below `floor_buf`. Elevation therefore binds on EVERY cell whenever
//! `devotion_elev < floor_buf`, regardless of terrain.
//!
//! That is the mechanism behind The Tilth's measured "elevation binds on 100%
//! of land for goblin, gnoll and human", and it means the silence of the
//! climate axes is an AUTHORING consequence, not a model constraint.
//!
//! ## Measured, 2026-08-07, seeds 42 / 7 / 1234
//!
//! Share of LAND cells on which elevation is the Liebig minimum. `dev_el` is
//! the kind's authored elevation `devotion`; `floor` is
//! `sovereignty_floor(mass, potency)`; `below` is the closed form's predicate
//! `dev_el < floor`, which predicts a 100% share.
//!
//! ```text
//!   kind        mass    floor  dev_el  below     s42       s7    s1234
//!   kobold      13.6   0.3078    0.95     no   43.72%   41.55%   51.45%
//!   goblin      18.1   0.3347    0.35     no  100.00%  100.00%   97.04%
//!   hobgoblin   74.8   0.4527    0.70     no   74.77%   77.32%   69.26%
//!   bugbear    132.0   0.4933    0.70     no   72.89%   78.12%   71.40%
//!   gnoll      136.1   0.4954    0.40    YES  100.00%  100.00%  100.00%
//!   human       70.0   0.4477    0.30    YES  100.00%  100.00%  100.00%
//!   land cells:                                11,066   19,046   11,571
//! ```
//!
//! **The theorem is confirmed and the plan's generalisation of it is
//! falsified.** Every `below == YES` row is 100% on every seed — exactly, not
//! approximately; the assertion below is `== 1.0` and would catch one cell.
//! But this campaign's plan predicted **> 99% for all six settling peoples**,
//! extending The Tilth's result from the three kinds it named to the whole
//! roster. Three of the six refute that immediately, and they are precisely
//! the three the closed form never covered: kobold (43–51%), hobgoblin
//! (69–77%) and bugbear (71–78%) all carry `devotion_elev` ABOVE their floor.
//!
//! Goblin is the instructive boundary. Its devotion 0.35 clears its floor
//! 0.3347 by 0.0153, so the theorem does not apply — and on seeds 42 and 7 it
//! is 100% anyway, while on 1234 temperature takes 2.96% of land. A near-miss
//! on the predicate is a near-miss on the result, and no wider claim survives
//! it.
//!
//! **The consequence for the spec.** §1.1 infers dwarf behaviour from the mass
//! class ("dwarves sit in those kinds' mass class at `potency = 0.0`, so the
//! same result is expected to follow"). That inference is wrong in its stated
//! form: hobgoblin at 74.8 kg is in human's mass class and is *not* elevation-
//! bound everywhere. **Mass sets the floor; the authored devotion decides the
//! bind.** §3.1's conclusion stands and is in fact strengthened — climate
//! silence is an authoring choice, and a dwarf authored above its floor gets
//! climate curves that speak, as hobgoblin and bugbear demonstrate live.

// `terrain_of` and friends are named derivation entry points (decision 0092);
// a probe measuring a handful of worlds is exactly the site the allowance is
// for.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{ConditionResponse, Mass, sovereignty_floor};
use hornvale_species::ConditionNiche;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, Substrate, build_world, climate_of, sky_of, substrate_field,
    terrain_of,
};

/// The Liebig-binding axis, mirroring `tolerance_liebig`
/// (`windows/worldgen/src/lib.rs:1092`), which is private.
///
/// **This mirrors production code and is a standing maintenance obligation.**
/// If the tolerance model changes — a floor moved onto elevation, an axis
/// added, the `min()` replaced by the gate/modifier split `tolerance_tiered`
/// sketches — this function goes stale silently and must be updated with it.
/// It is the same contract `warren_gate.rs`'s realm-free reference formula
/// carries, and for the same reason: a mirror that has drifted measures a
/// model the library no longer has.
fn binding_axis(cn: &ConditionNiche, s: &Substrate, floor_buf: f64) -> &'static str {
    let t = cn.temperature.eval(s.temperature_c, floor_buf);
    let m = cn.moisture.eval(s.moisture, floor_buf);
    let i = cn.insolation.eval(s.insolation, floor_buf);
    let e = cn.elevation.eval(s.height_asl_m.get(), 0.0);
    let mut best = ("temperature", t);
    for cand in [("moisture", m), ("insolation", i), ("elevation", e)] {
        if cand.1 < best.1 {
            best = cand;
        }
    }
    best.0
}

#[test]
fn elevation_binds_everywhere_when_its_devotion_is_below_the_sovereignty_floor() {
    let floor_buf = sovereignty_floor(Mass::new(70.0).unwrap(), 0.0);
    assert!(
        (floor_buf - 0.4477).abs() < 1e-3,
        "a 70 kg potency-0 kind's sovereignty floor is 0.4477; got {floor_buf:.4}"
    );

    // Human's authored elevation devotion, from `human_condition_niche()`.
    let low = ConditionResponse {
        optimum: 1500.0,
        width: 4000.0,
        devotion: 0.30,
    };
    assert!(
        low.devotion < floor_buf,
        "human's elevation devotion 0.30 sits below the floor 0.4477, which is \
         WHY The Tilth measured elevation binding on 100% of land"
    );
    // The bound is tight: elevation's value can never exceed its devotion.
    assert!(
        low.eval(1500.0, 0.0) <= low.devotion + 1e-12,
        "eval at the optimum equals devotion exactly"
    );

    // And a devotion ABOVE the floor breaks the guarantee: at the optimum,
    // elevation is no longer the smallest term.
    let high = ConditionResponse {
        optimum: 1500.0,
        width: 4000.0,
        devotion: 0.60,
    };
    assert!(
        high.eval(1500.0, 0.0) > floor_buf,
        "at devotion 0.60 the elevation term rises above the floor at its \
         optimum, so a climate axis can bind there"
    );
}

/// The six settling peoples, in registry order — the population whose bind
/// behaviour The Tilth measured and this reproduces.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// Per settling kind at `seed`: its name, the share of LAND cells on which
/// elevation is the Liebig minimum, and the land-cell count that share is over.
///
/// Land is `!terrain.is_ocean(cell)`, never `elevation < 0` — a world's sea
/// level is not zero (seed 42's sits at −2,936 m).
fn bind_shares(seed_value: u64) -> Vec<(&'static str, f64, usize)> {
    let wc = WorldComponents::assemble().expect("components assemble");
    let seed = hornvale_kernel::Seed(seed_value);
    let world = build_world(
        seed,
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("probe seed builds");
    let terrain = terrain_of(&world).expect("terrain");
    let climate = climate_of(&world).expect("climate");
    let geo = terrain.geosphere();
    let sky = sky_of(&world).expect("sky");
    // The stellar-input triple, resolved exactly as `niche_breadth_probe.rs`
    // does (`stellar_inputs` itself is private to worldgen).
    let generated = match &sky {
        hornvale_worldgen::Sky::Generated(g) => g,
        _ => panic!("probe expects a generated sky"),
    };
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let land: Vec<hornvale_kernel::CellId> =
        geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();

    println!("== seed {seed_value} ==  land cells: {}", land.len());
    println!(
        "{:<10} {:>7} {:>7} {:>7} {:>6}  {:>8} {:>8} {:>8} {:>8}",
        "kind", "mass", "floor", "dev_el", "below", "temp%", "moist%", "insol%", "ELEV%"
    );

    let mut out = Vec::new();
    for name in SETTLERS {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(name))
            .expect("settler has biosphere traits");
        let floor_buf = sovereignty_floor(bio.mass, bio.potency);
        let mut counts = [0usize; 4];
        for &c in &land {
            let s = substrate.get(c);
            let idx = match binding_axis(&bio.condition_niche, s, floor_buf) {
                "temperature" => 0,
                "moisture" => 1,
                "insolation" => 2,
                _ => 3,
            };
            counts[idx] += 1;
        }
        let n = land.len();
        let share = |k: usize| counts[k] as f64 / n as f64;
        let dev = bio.condition_niche.elevation.devotion;
        println!(
            "{:<10} {:>7.1} {:>7.4} {:>7.2} {:>6}  {:>7.2}% {:>7.2}% {:>7.2}% {:>7.2}%  \
             (elev on {}/{})",
            name,
            bio.mass.kilograms(),
            floor_buf,
            dev,
            if dev < floor_buf { "YES" } else { "no" },
            share(0) * 100.0,
            share(1) * 100.0,
            share(2) * 100.0,
            share(3) * 100.0,
            counts[3],
            n
        );
        out.push((name, share(3), n));
    }
    println!();
    out
}

/// **The theorem, live.** For every settling kind whose authored
/// `devotion_elev` sits BELOW its `sovereignty_floor`, elevation must be the
/// Liebig minimum on *every* land cell — not 99% of them, all of them. The
/// closed form admits no exception, so this asserts exact equality with 1.0
/// and would catch a single dissenting cell.
///
/// **This test replaced a falsified one.** The plan predicted the whole
/// six-kind settling roster would be elevation-bound on > 99% of land,
/// generalising The Tilth's measurement from the three kinds it named to all
/// six. Seed 42 refutes that at once: kobold binds elevation on 43.72% of
/// land, hobgoblin on 74.77%, bugbear on 72.89%. Those three are exactly the
/// three whose `devotion_elev` sits ABOVE their floor, so the closed form was
/// never violated — the plan's empirical generalisation was simply wider than
/// the theorem that justified it. See the module doc for the recorded table.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn every_kind_below_its_floor_is_elevation_bound_on_all_land() {
    let mut checked = 0usize;
    for seed in [42u64, 7, 1234] {
        let wc = WorldComponents::assemble().expect("components assemble");
        let report = bind_shares(seed);
        for (kind, elev_share, n) in &report {
            let bio = wc
                .biosphere
                .get(&hornvale_kernel::KindId(kind))
                .expect("settler has biosphere traits");
            let floor_buf = sovereignty_floor(bio.mass, bio.potency);
            if bio.condition_niche.elevation.devotion >= floor_buf {
                continue;
            }
            checked += 1;
            assert_eq!(
                *elev_share, 1.0,
                "seed {seed}: {kind}'s elevation devotion {:.4} is below its \
                 sovereignty floor {floor_buf:.4}, so elevation must bind on ALL \
                 {n} land cells; it bound on {:.6}. If this dropped, the tolerance \
                 model changed and the spec's §3.1 premise is void.",
                bio.condition_niche.elevation.devotion, elev_share
            );
        }
    }
    assert!(
        checked >= 3,
        "the theorem must have kinds to bite on: expected at least three \
         (one per seed for gnoll and human at minimum), got {checked}"
    );
}

/// **The instrument must be shown able to report a NON-elevation bind.** A
/// bind audit whose every assertion reads "elevation won" cannot be told apart
/// from one that hardcodes the answer — The Benchmark shipped a guard that was
/// vacuous and green because it sampled the one cell where the bug was
/// invisible. Kobold's elevation devotion (0.95) sits far above its floor
/// (0.3078), so the closed form says a climate axis MUST bind somewhere.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn a_kind_above_its_floor_lets_a_climate_axis_bind() {
    for seed in [42u64, 7, 1234] {
        let report = bind_shares(seed);
        let (_, kobold_share, n) = report
            .iter()
            .find(|(k, _, _)| *k == "kobold")
            .expect("kobold in the settling roster");
        assert!(
            *kobold_share < 0.99,
            "seed {seed}: kobold's elevation devotion 0.95 is far above its floor \
             0.3078, so temperature/moisture/insolation must bind on a real share \
             of the {n} land cells. Elevation bound on {kobold_share:.6}, which \
             means this probe cannot discriminate and every elevation result it \
             reports is worthless."
        );
    }
}
