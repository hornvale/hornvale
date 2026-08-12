//! The land-elevation attribution probe (The Glasshouse, Stage A Task 4).
//!
//! Hornvale's land stands far too high — 54.5% of it above 2000 m against
//! Earth's ~11%, mean land elevation 2266.87 m (median over the census seed
//! range) against Earth's ~840 m — which is a near-constant lapse-rate
//! penalty of about −14 K and the real driver of the biome/soil uniformity.
//! Fixing it needs a *target*, and the target is whichever additive term of
//! the elevation assembly carries the height. This module measures that.
//!
//! ## Why the terms are read, not zeroed
//!
//! The obvious probe — neutralise one term, regenerate, diff the land mean —
//! is unusable here, for two independent reasons:
//!
//! 1. `relief_scale` has a hard floor. It is
//!    `(0.25 + 0.75·induration) · belt` with `belt ≥ 1.0`, so even at
//!    `induration = 0` the relief term survives at a quarter amplitude. No
//!    argument reaches past it.
//! 2. Removing any term moves sea level, which moves *which cells are land*.
//!    [`crate::elevation::derive_sea_level`] is the ocean-fraction
//!    **percentile** of the elevation distribution, so a before/after land
//!    mean would be taken over two different cell sets and be confounded by
//!    construction.
//!
//! So this reads the real terms off the real world instead:
//! [`crate::elevation::globe_elevation_terms`] rebuilds each cell's
//! [`crate::elevation::ElevationTerms`] by calling the same
//! `cell_elevation_terms` the pipeline calls, and the carve's own net delta is
//! taken from the globe's retained `carve_delta_m`. Nothing is reimplemented,
//! and [`Component::ALL`] is checked against the elevation the pipeline
//! actually produced on **every land cell of every seed** before a single
//! statistic is reported.
//!
//! ## What the decomposition is, and how it corrects the brief
//!
//! The task brief named five terms — `assemble_elevation`'s `base`,
//! `boundary`, `hotspot`, `relief` and the tie-breaking `epsilon`. Those five
//! sum to `elevation_pre`, the **pre-carve** surface. The quantity the
//! hypsometry metric measures is `elevation − sea_level` on the **final**
//! surface, and `globe::generate` closes with
//! `elevation == elevation_pre + carve_delta_m` (the carve plus the sea-trim).
//! So the honest decomposition of what the metric sees has **seven**
//! components, not five: those five, the carve delta, and sea level itself
//! (entering negatively, since the reported quantity is a height *above* it).
//!
//! Two of the seven can be reasoned about before measuring. `relief` is
//! zero-mean by construction (`(fbm − 0.5) · 2`), so it can add variance but
//! not height; `epsilon` totals ~0.04 m across a level-6 globe and is a
//! tie-breaker, not physics. Everything else is open.
//!
//! ## The two variance decompositions, and why both are printed
//!
//! Naive per-term variance shares (`Var(term)/Var(total)`) do not sum to one,
//! because the terms are correlated — thick crust and collision boundaries
//! coincide, and the carve preferentially erodes what stands high. The
//! decomposition that *is* exact is the covariance one: since
//! `y = Σᵢ cᵢ`, `Var(y) = Σᵢ Cov(cᵢ, y)`, so `Cov(cᵢ, y)/Var(y)` is a share
//! summing to 1 and may legitimately be negative for a term that opposes the
//! total. Both are printed: the naive column says how much a term varies, the
//! covariance column says how much of the *land surface's* variation it
//! explains.
//!
//! Statistics are reported twice more: **pooled** over every land cell of
//! every seed (where sea level varies world to world and therefore carries
//! variance), and **within-world**, averaged over seeds (where sea level is a
//! constant and drops out). A term that dominates both is the target.

use crate::elevation::globe_elevation_terms;
use crate::globe::generate;
use crate::pins::TerrainPins;
use hornvale_kernel::{Geosphere, Seed};

/// The canonical globe level ([`crate::GLOBE_LEVEL`]), so the probe measures
/// the same surface the census does rather than a cheaper proxy.
const LEVEL: u32 = crate::GLOBE_LEVEL;

/// World seeds the probe sweeps. Contiguous from 0 so it samples the head of
/// the census seed range (`studies/the-census.study.json` uses
/// `{from: 0, count: 1000}`), and `hornvale_terrain::generate` takes the world
/// seed directly — `worldgen` passes `world.seed` through unchanged — so these
/// are literally the census's own first worlds.
const SEED_COUNT: u64 = 12;

/// The seven additive components of `elevation − sea_level` on a land cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Component {
    /// Airy isostasy over crust thickness: `ISOSTASY_M_PER_KM · (crust − ref)`.
    Base,
    /// The nearest same-plate boundary's profile contribution (signed).
    Boundary,
    /// Hotspot-trail seamount domes, summed (≥ 0).
    Hotspot,
    /// Induration- and belt-scaled fBm relief (zero-mean by construction).
    Relief,
    /// The strict-ordering micro-epsilon.
    Epsilon,
    /// The carve's net generate-level delta (incision, repose, deposition,
    /// wedge/delta/atoll, and the sea-trim), from `TectonicGlobe::carve_delta_m`.
    Carve,
    /// Minus this world's sea level — the datum the height is measured above.
    MinusSeaLevel,
}

impl Component {
    /// Every component, in the order `elevation` assembles them (sea level
    /// last, since it is subtracted at read time rather than assembled).
    const ALL: [Component; 7] = [
        Component::Base,
        Component::Boundary,
        Component::Hotspot,
        Component::Relief,
        Component::Epsilon,
        Component::Carve,
        Component::MinusSeaLevel,
    ];

    /// The column label used in the printed tables.
    fn label(self) -> &'static str {
        match self {
            Component::Base => "base (isostasy)",
            Component::Boundary => "boundary",
            Component::Hotspot => "hotspot",
            Component::Relief => "relief",
            Component::Epsilon => "epsilon",
            Component::Carve => "carve delta",
            Component::MinusSeaLevel => "-sea level",
        }
    }
}

/// Mean, standard deviation, and both variance shares for one component over
/// one sample of land cells.
struct Stat {
    /// Component mean, metres.
    mean: f64,
    /// Component standard deviation, metres.
    sd: f64,
    /// `Var(cᵢ)/Var(y)` — how much this component varies. Does not sum to 1.
    naive_share: f64,
    /// `Cov(cᵢ, y)/Var(y)` — how much of the land surface's variance this
    /// component explains. Sums to exactly 1 across components; may be
    /// negative.
    cov_share: f64,
}

/// One land cell's seven components, in [`Component::ALL`] order.
type Row = [f64; 7];

/// Every component's [`Stat`] over `rows`, plus the sample's own mean and
/// variance of `y = Σ components`.
fn statistics(rows: &[Row]) -> ([Stat; 7], f64, f64) {
    let n = rows.len() as f64;
    let mut means = [0.0_f64; 7];
    let mut y_mean = 0.0_f64;
    for r in rows {
        for i in 0..7 {
            means[i] += r[i];
        }
        y_mean += r.iter().sum::<f64>();
    }
    for m in &mut means {
        *m /= n;
    }
    y_mean /= n;
    let mut var = [0.0_f64; 7];
    let mut cov = [0.0_f64; 7];
    let mut y_var = 0.0_f64;
    for r in rows {
        let dy = r.iter().sum::<f64>() - y_mean;
        y_var += dy * dy;
        for i in 0..7 {
            let d = r[i] - means[i];
            var[i] += d * d;
            cov[i] += d * dy;
        }
    }
    for v in &mut var {
        *v /= n;
    }
    for c in &mut cov {
        *c /= n;
    }
    y_var /= n;
    let stats = std::array::from_fn(|i| Stat {
        mean: means[i],
        sd: var[i].sqrt(),
        naive_share: var[i] / y_var,
        cov_share: cov[i] / y_var,
    });
    (stats, y_mean, y_var)
}

/// Print one decomposition table.
fn print_table(title: &str, stats: &[Stat; 7], y_mean: f64, y_var: f64, cells: usize) {
    println!("\n{title}  (n = {cells} land cells)");
    println!(
        "{:<16} {:>12} {:>12} {:>12} {:>12}",
        "component", "mean (m)", "sd (m)", "Var/VarY", "Cov/VarY"
    );
    for (c, s) in Component::ALL.iter().zip(stats.iter()) {
        println!(
            "{:<16} {:>12.2} {:>12.2} {:>12.4} {:>12.4}",
            c.label(),
            s.mean,
            s.sd,
            s.naive_share,
            s.cov_share
        );
    }
    let mean_sum: f64 = stats.iter().map(|s| s.mean).sum();
    let cov_sum: f64 = stats.iter().map(|s| s.cov_share).sum();
    println!(
        "{:<16} {:>12.2} {:>12.2} {:>12} {:>12.4}",
        "TOTAL (e - sea)",
        mean_sum,
        y_var.sqrt(),
        "-",
        cov_sum
    );
    println!("  mean(e - sea) recomputed directly: {y_mean:.2} m");
}

/// Where the isostatic base puts a crust thickness, and back again: the
/// inverse of [`crate::elevation::isostatic_m`], used only to translate a
/// measured mean elevation into the crust thickness that would produce it, so
/// the audit can quote the finding in the units of the field that has to be
/// fixed. The forward direction always calls the real function.
fn crust_km_at(elevation_m: f64) -> f64 {
    crate::elevation::ISOSTASY_REF_KM + elevation_m / crate::elevation::ISOSTASY_M_PER_KM
}

/// The attribution readout. Sweeps [`SEED_COUNT`] worlds at [`LEVEL`],
/// asserts the seven components reconstruct the pipeline's own elevation on
/// every land cell, and prints the pooled and within-world decompositions.
///
/// Cheap enough to stay in the commit gate: **roughly 3–6 s at ordinary load**
/// for twelve level-6 globes (terrain-only genesis, dev profile optimized since
/// decision 0113) — 3.26–3.98 s bare, 4.755–5.702 s under nextest, 3.80 s on an
/// independent re-run at loadavg ~16. The figure is load-dependent, not a
/// single number: at loadavg ~28 it reads 14.8 s while the whole crate suite
/// scales by the same ~2.7×. All of it is well inside the ~30 s gate
/// threshold, so this carries no `heavy:` deferral and the conservation assert
/// below runs on every gate rather than only in `make gate-full`. The table
/// with conditions is in `docs/audits/land-elevation-attribution.md` §6.
///
/// Read the numbers with
/// `cargo test -p hornvale-terrain --lib the_land_elevation_terms_attribute_their_variance -- --nocapture`.
/// The committed finding is `docs/audits/land-elevation-attribution.md`.
///
/// claim: readout(prints the variance decomposition over the first
/// [`SEED_COUNT`] census seeds; the seed loop pools a sample rather than
/// hunting one, and the only assertions are the per-cell conservation guard,
/// the exactness of the covariance shares, and relief's zero mean — decision
/// 0093, the `hollow_readout::report_cave_substrate` shape)
#[test]
fn the_land_elevation_terms_attribute_their_variance() {
    let geo = Geosphere::new(LEVEL);
    let mut pooled: Vec<Row> = Vec::new();
    // Per-seed within-world shares, for the stability table.
    let mut per_seed: Vec<(u64, f64, usize, [f64; 7])> = Vec::new();
    // Land-crust thickness and sea level, for the derived reading.
    let (mut crust_sum, mut crust_n) = (0.0_f64, 0_usize);
    let mut sea_levels: Vec<f64> = Vec::new();
    // The three areas that decide where the sea-level percentile lands: the
    // analytic continental budget `draw_cratons` rescaled the majors to, the
    // grid area that actually clears the continental threshold, and the land
    // area the percentile ended up granting.
    let (mut supply_sum, mut threshold_sum, mut land_sum) = (0.0_f64, 0.0_f64, 0.0_f64);
    for seed in 0..SEED_COUNT {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .expect("default pins never refuse a world");
        let globe = &outcome.globe;
        let terms = globe_elevation_terms(&geo, globe, Seed(seed));
        let sea = globe.sea_level.get();
        sea_levels.push(sea);
        let mut rows: Vec<Row> = Vec::new();
        for cell in geo.cells() {
            let elevation = globe.elevation.get(cell).get();
            if elevation < sea {
                continue; // ocean: land is `e >= sea`, matching the metric
            }
            let t = terms.get(cell);
            let carve = *globe.carve_delta_m.get(cell);
            // CONSERVATION. If the components do not re-add to the elevation
            // the pipeline produced, the decomposition is describing some
            // other world and every share below it is void. `total()` is the
            // pipeline's own summation order; `+ carve` is `globe::generate`'s
            // documented `elevation == elevation_pre + carve_delta_m`
            // identity, exact to a rounding of the last bit because generate
            // composes it as `(pre + carve) + trim` while the retained delta
            // is `carve + trim`.
            let reconstructed = t.total() + carve;
            assert!(
                (reconstructed - elevation).abs() < 1e-9,
                "seed {seed} cell {cell:?}: components sum to {reconstructed} but the \
                 pipeline produced {elevation}"
            );
            rows.push([
                t.base, t.boundary, t.hotspot, t.relief, t.epsilon, carve, -sea,
            ]);
            crust_sum += *globe.crust.get(cell);
            crust_n += 1;
        }
        assert!(!rows.is_empty(), "seed {seed} has no land cells");
        supply_sum += crate::crust::continental_supply(&globe.cratons);
        threshold_sum += geo
            .cells()
            .filter(|c| *globe.crust.get(*c) >= crate::crust::CONTINENTAL_THRESHOLD_KM)
            .count() as f64
            / globe.crust.len() as f64;
        land_sum += rows.len() as f64 / globe.crust.len() as f64;
        let (stats, y_mean, _) = statistics(&rows);
        per_seed.push((
            seed,
            y_mean,
            rows.len(),
            std::array::from_fn(|i| stats[i].cov_share),
        ));
        pooled.extend_from_slice(&rows);
    }

    let (stats, y_mean, y_var) = statistics(&pooled);
    println!("\n=== land-elevation attribution: {SEED_COUNT} worlds, level {LEVEL} ===");
    print_table(
        "POOLED over every land cell of every seed",
        &stats,
        y_mean,
        y_var,
        pooled.len(),
    );

    println!("\nWITHIN-WORLD Cov/VarY per seed (sea level is constant, so its share is 0)");
    print!("{:<6} {:>10} {:>8}", "seed", "mean (m)", "land");
    for c in &Component::ALL {
        print!(" {:>12}", c.label());
    }
    println!();
    for (seed, mean, cells, shares) in &per_seed {
        print!("{seed:<6} {mean:>10.2} {cells:>8}");
        for s in shares {
            print!(" {s:>12.4}");
        }
        println!();
    }
    // The mean of the within-world shares, which is what the audit cites as
    // the seed-robust attribution.
    let mut mean_shares = [0.0_f64; 7];
    for (_, _, _, shares) in &per_seed {
        for (m, s) in mean_shares.iter_mut().zip(shares.iter()) {
            *m += s / per_seed.len() as f64;
        }
    }
    print!("{:<6} {:>10} {:>8}", "MEAN", "", "");
    for s in &mean_shares {
        print!(" {s:>12.4}");
    }
    println!();

    // The derived reading: the same measured means, restated in the units of
    // the field a fix would have to move. `base` is exactly
    // `ISOSTASY_M_PER_KM · (crust − ISOSTASY_REF_KM)`, so every elevation here
    // is a crust thickness in disguise, and the interesting comparison is
    // where sea level lands relative to the isostatic SHELF BREAK — the
    // elevation of the `crust::CONTINENTAL_THRESHOLD_KM` contour, which is
    // what a world with Earth-like hypsometry would drown to.
    let mean_sea: f64 = sea_levels.iter().sum::<f64>() / sea_levels.len() as f64;
    let mean_land_crust = crust_sum / crust_n as f64;
    let shelf_break_m = crate::elevation::isostatic_m(crate::crust::CONTINENTAL_THRESHOLD_KM);
    println!("\nDERIVED READING (the same means, in crust-thickness terms)");
    println!("  mean land crust thickness            {mean_land_crust:>10.2} km");
    println!(
        "  isostatic base at that thickness     {:>10.2} m",
        crate::elevation::isostatic_m(mean_land_crust)
    );
    println!("  mean sea level                       {mean_sea:>10.2} m");
    println!(
        "  crust thickness at sea level         {:>10.2} km",
        crust_km_at(mean_sea)
    );
    println!(
        "  the isostatic shelf break (crust = {:.0} km) {shelf_break_m:>10.2} m",
        crate::crust::CONTINENTAL_THRESHOLD_KM
    );
    println!(
        "  => sea level sits {:.2} m BELOW the shelf break, i.e. {:.2} km of crust below it",
        shelf_break_m - mean_sea,
        crate::crust::CONTINENTAL_THRESHOLD_KM - crust_km_at(mean_sea)
    );
    let n = SEED_COUNT as f64;
    println!("\nWHY SEA LEVEL LANDS THERE (sphere fractions, mean over the sweep)");
    println!(
        "  analytic continental supply (majors, the rescale's budget) {:>8.4}",
        supply_sum / n
    );
    println!(
        "  cells actually at or above the continental threshold       {:>8.4}",
        threshold_sum / n
    );
    println!(
        "  land the sea-level percentile granted                      {:>8.4}",
        land_sum / n
    );
    println!(
        "  => {:>8.4} of the sphere is land standing on SUB-threshold crust",
        land_sum / n - threshold_sum / n
    );
    // The shelf-break fallback (`effective_ocean_target`, decision 0053) is
    // the mechanism that exists to stop exactly this — a world filling its
    // land quota below the shelf break. It fires only when supply falls below
    // `SUPPLY_SHORTFALL_FACTOR` of the quota, so print the ratio it tests.
    println!(
        "  supply / land quota                                        {:>8.4}  (the shelf-break \
         fallback fires below SUPPLY_SHORTFALL_FACTOR = {})",
        supply_sum / land_sum,
        crate::elevation::SUPPLY_SHORTFALL_FACTOR
    );

    // The covariance decomposition is exact by construction; assert it so a
    // future edit to `statistics` cannot quietly break the arithmetic the
    // audit's shares rest on.
    let cov_sum: f64 = stats.iter().map(|s| s.cov_share).sum();
    assert!(
        (cov_sum - 1.0).abs() < 1e-9,
        "covariance shares must sum to 1, got {cov_sum}"
    );
    // Relief is zero-mean by construction; a nonzero mean here would mean the
    // fBm rebalance had drifted, and would change the reading above.
    let relief_mean = stats[3].mean;
    assert!(
        relief_mean.abs() < 60.0,
        "relief is supposed to be near zero-mean over land; got {relief_mean} m"
    );
}
