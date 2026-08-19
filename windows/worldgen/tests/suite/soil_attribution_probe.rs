//! The Glasshouse Task 6, soil half: **why** `dominant-soil-order` is frozen
//! at `leptosol` on 1000/1000 census worlds, before *and* after this
//! campaign warmed the population by 8.3 K.
//!
//! ## Why this probe exists
//!
//! Spec §3.4's gate has two halves. The biome half asks whether any class
//! still exceeds 50%; the soil half asks whether `dominant-soil-order` is
//! "no longer frozen". The refreshed census answers the first (max class
//! 25.0%, down from 65.1%) and the second (`leptosol` 100.0%, **unmoved**
//! from 100.0%).
//!
//! Spec §2.3 predicted the soil half would move for the same reason the
//! biome half did: "`leptosol` (thin, steep, rocky soil) follows the same
//! elevation". That prediction is falsified by the refreshed census, and
//! this probe establishes the cause rather than inferring it — a frozen
//! column that does not move when its supposed driver moves 8.3 K is
//! evidence about the *classifier*, but only if the branch that fires is
//! measured rather than assumed.
//!
//! ## Method
//!
//! `classify_soil` (`domains/terrain/src/lithology.rs`) opens with a
//! climate-independent early return:
//!
//! ```text
//! if depth.get() < 0.25 || slope_m > 300.0 { return SoilOrder::Leptosol; }
//! ```
//!
//! Every branch below it reads `mean_temp_c` or `moisture`. So for each land
//! cell this probe recomputes the two early-return inputs exactly as
//! `hornvale_worldgen::soil_of` supplies them (slope = the maximum elevation
//! *drop* to any neighbour) and records which predicate fires. For the cells
//! that reach the climate ladder it records what the ladder actually says —
//! the counterfactual distribution, i.e. what `dominant-soil-order` would
//! report if the early return were not pre-empting it.
//!
//! This distinguishes three outcomes the frozen column cannot:
//!
//! 1. the early return fires on a *minority* of land, and `leptosol` still
//!    dominates — the ladder itself is degenerate;
//! 2. the early return fires on a *majority* of land — the ladder never gets
//!    a vote, and the freeze is a slope/depth fact, not a climate fact;
//! 3. the early return fires rarely and the counterfactual is also
//!    `leptosol` — impossible by construction, and therefore a check that
//!    the probe is wired to the same inputs the shipped path uses.
//!
//! Run by hand (release; live worldgen):
//! `cargo test -p hornvale-worldgen --release --test suite --
//! soil_attribution_probe --ignored --nocapture`
//!
//! Test fixture (decision 0092): calls the derivation entry points directly to
//! build its own world state, once per seed — the sanctioned test-fixture
//! posture, which is why `terrain_of`/`climate_of` are allowed here.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{SoilOrder, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, climate_of, soil_of,
    terrain_of,
};

/// Seeds swept. Twenty is an attribution sample, not a distribution one —
/// the distribution is already settled by the committed 1000-world census
/// (`leptosol` 100.0%); what is unknown is which predicate produces it, and
/// that does not need 1000 worlds to establish.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=20;

/// `classify_soil`'s thin-soil early-return threshold, in metres. Mirrored
/// here rather than imported because it is a private literal in
/// `lithology.rs`; the test's closing assertion checks this copy still agrees
/// with the shipped classification, so a drift here fails loudly rather than
/// silently attributing the freeze to the wrong branch.
const THIN_SOIL_M: f64 = 0.25;

/// `classify_soil`'s steep-slope early-return threshold, in metres of drop
/// to the lowest neighbour. Mirrored for the same reason as [`THIN_SOIL_M`].
const STEEP_DROP_M: f64 = 300.0;

/// Which arm of `classify_soil`'s early return a land cell took.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Arm {
    /// `depth < 0.25` fired (whether or not the slope also would have).
    Thin,
    /// `slope > 300.0` fired and the soil was deep enough.
    Steep,
    /// Neither fired; the cell reached the temperature/moisture ladder.
    Ladder,
}

/// Human name for a soil order, matching the census's `soil_order_name`.
fn order_name(o: SoilOrder) -> &'static str {
    match o {
        SoilOrder::Chernozem => "chernozem",
        SoilOrder::Podzol => "podzol",
        SoilOrder::Laterite => "laterite",
        SoilOrder::Aridisol => "aridisol",
        SoilOrder::Loam => "loam",
        SoilOrder::Andosol => "andosol",
        SoilOrder::Leptosol => "leptosol",
        SoilOrder::Histosol => "histosol",
        SoilOrder::Gley => "gley",
    }
}

/// One world's land-cell attribution.
struct WorldAttribution {
    seed: u64,
    land: usize,
    thin: usize,
    steep: usize,
    ladder: usize,
    /// What the climate ladder said, for the cells that reached it.
    ladder_orders: std::collections::BTreeMap<SoilOrder, usize>,
    /// The shipped `dominant-soil-order` for this world, recomputed exactly
    /// as `windows/lab/src/metrics.rs` does it.
    shipped_dominant: Option<SoilOrder>,
}

/// Attribute one world, or `None` if genesis refuses the seed.
fn attribute(seed: u64, wc: &WorldComponents) -> Option<WorldAttribution> {
    let world = build_world_to(
        hornvale_kernel::Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .ok()?;
    let terrain = terrain_of(&world).ok()?;
    let climate = climate_of(&world).ok()?;
    let geo = terrain.geosphere();
    let soils = soil_of(&terrain, &climate, geo);

    let mut a = WorldAttribution {
        seed,
        land: 0,
        thin: 0,
        steep: 0,
        ladder: 0,
        ladder_orders: std::collections::BTreeMap::new(),
        shipped_dominant: None,
    };
    let mut shipped: std::collections::BTreeMap<SoilOrder, usize> =
        std::collections::BTreeMap::new();

    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        a.land += 1;
        *shipped.entry(*soils.get(cell)).or_insert(0) += 1;

        let mat = terrain.material_at(cell);
        let here = terrain.elevation_at(cell).get();
        // Exactly `soil_of`'s slope: the maximum DROP to any neighbour.
        let slope = geo
            .neighbors(cell)
            .iter()
            .map(|n| here - terrain.elevation_at(*n).get())
            .fold(0.0_f64, f64::max);

        let arm = if mat.soil_depth.get() < THIN_SOIL_M {
            Arm::Thin
        } else if slope > STEEP_DROP_M {
            Arm::Steep
        } else {
            Arm::Ladder
        };
        match arm {
            Arm::Thin => a.thin += 1,
            Arm::Steep => a.steep += 1,
            Arm::Ladder => {
                a.ladder += 1;
                *a.ladder_orders.entry(*soils.get(cell)).or_insert(0) += 1;
            }
        }
    }
    // Same tie-break as the census metric: highest count, then lowest order.
    a.shipped_dominant = shipped
        .iter()
        .max_by(|x, y| x.1.cmp(y.1).then(y.0.cmp(x.0)))
        .map(|(&o, _)| o);
    Some(a)
}

/// claim: readout(preregistered) — the share of land cells taking each arm
/// of `classify_soil`'s climate-independent early return, and the
/// counterfactual soil distribution over the cells that reach the climate
/// ladder. Prints; asserts only the wiring invariant (§3 of the header),
/// because the *quantity* is the finding and pinning it would freeze a
/// number this campaign is trying to explain.
#[test]
#[ignore = "live-worldgen attribution probe (The Glasshouse Task 6, soil half): builds 20 \
            worlds to BuildDepth::Terrain to attribute classify_soil's early return; \
            a one-shot readout, not a fast-gate battery"]
fn leptosol_freeze_is_attributed_to_a_named_branch() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let worlds: Vec<WorldAttribution> = SEEDS.filter_map(|s| attribute(s, &wc)).collect();
    assert!(
        !worlds.is_empty(),
        "no seed in {SEEDS:?} produced a world — the probe measured nothing"
    );

    println!("== classify_soil early-return attribution, land cells ==");
    println!(
        "{:>6}  {:>7}  {:>16}  {:>16}  {:>16}  shipped dominant",
        "seed", "land", "thin(<0.25m)", "steep(>300m)", "reached ladder"
    );
    let (mut tl, mut tt, mut ts, mut tld) = (0usize, 0usize, 0usize, 0usize);
    let mut ladder_total: std::collections::BTreeMap<SoilOrder, usize> =
        std::collections::BTreeMap::new();
    let mut non_leptosol_dominant = 0usize;

    for w in &worlds {
        let pct = |n: usize| 100.0 * n as f64 / w.land.max(1) as f64;
        println!(
            "{:>6}  {:>7}  {:>9} {:>5.1}%  {:>9} {:>5.1}%  {:>9} {:>5.1}%  {}",
            w.seed,
            w.land,
            w.thin,
            pct(w.thin),
            w.steep,
            pct(w.steep),
            w.ladder,
            pct(w.ladder),
            w.shipped_dominant.map(order_name).unwrap_or("(landless)")
        );
        tl += w.land;
        tt += w.thin;
        ts += w.steep;
        tld += w.ladder;
        for (&o, &n) in &w.ladder_orders {
            *ladder_total.entry(o).or_insert(0) += n;
        }
        if w.shipped_dominant.is_some_and(|o| o != SoilOrder::Leptosol) {
            non_leptosol_dominant += 1;
        }
    }

    let pct = |n: usize| 100.0 * n as f64 / tl.max(1) as f64;
    println!(
        "\n== aggregate over {} worlds, {tl} land cells ==",
        worlds.len()
    );
    println!(
        "   thin  (depth < {THIN_SOIL_M} m):  {tt:>8}  {:>6.2}%",
        pct(tt)
    );
    println!(
        "   steep (drop  > {STEEP_DROP_M} m):  {ts:>8}  {:>6.2}%",
        pct(ts)
    );
    println!(
        "   reached the climate ladder:  {tld:>8}  {:>6.2}%",
        pct(tld)
    );
    println!(
        "   worlds whose shipped dominant is NOT leptosol: {non_leptosol_dominant}/{}",
        worlds.len()
    );

    println!("\n== counterfactual: what the ladder says, for cells that reach it ==");
    if tld == 0 {
        println!("   (no land cell anywhere reached the ladder)");
    } else {
        let mut rows: Vec<(SoilOrder, usize)> =
            ladder_total.iter().map(|(&o, &n)| (o, n)).collect();
        rows.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        for (o, n) in rows {
            println!(
                "   {:<12} {n:>8}  {:>6.2}% of ladder cells",
                order_name(o),
                100.0 * n as f64 / tld as f64
            );
        }
    }

    // The wiring invariant (header outcome 3). A cell that reaches the
    // ladder cannot be classified leptosol by the shipped path: every
    // remaining arm of `classify_soil` returns something else. If this
    // fires, the probe's mirrored thresholds have drifted from
    // `lithology.rs` and every number above is measuring the wrong branch.
    assert_eq!(
        ladder_total.get(&SoilOrder::Leptosol).copied().unwrap_or(0),
        0,
        "a land cell reached the climate ladder yet the shipped path classified it \
         leptosol. That is unreachable in `classify_soil` as written, so the probe's \
         copies of THIN_SOIL_M/STEEP_DROP_M no longer match lithology.rs and this \
         probe is attributing the freeze to the wrong branch. Re-read \
         `domains/terrain/src/lithology.rs::classify_soil` before trusting any figure \
         printed above."
    );
}
