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
//! 2. Removing any term moves sea level, which moves *which vertices are land*.
//!    [`crate::elevation::derive_sea_level`] is the ocean-fraction
//!    **percentile** of the elevation distribution, so a before/after land
//!    mean would be taken over two different vertex sets and be confounded by
//!    construction.
//!
//! So this reads the real terms off the real world instead:
//! [`crate::elevation::globe_elevation_terms`] rebuilds each vertex's
//! [`crate::elevation::ElevationTerms`] by calling the same
//! `vertex_elevation_terms` the pipeline calls, and the carve's own net delta is
//! taken from the globe's retained `carve_delta_m`. Nothing is reimplemented,
//! and [`Component::ALL`] is checked against the elevation the pipeline
//! actually produced on **every land vertex of every seed** before a single
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
//! Statistics are reported twice more: **pooled** over every land vertex of
//! every seed (where sea level varies world to world and therefore carries
//! variance), and **within-world**, averaged over seeds (where sea level is a
//! constant and drops out). A term that dominates both is the target.
//!
//! ## What Stage B added, and why each accumulator is here
//!
//! The attribution above located the lever (the crust field and where the
//! coastline cuts it); Stage B has to *judge a change to it*, and three
//! quantities the first pass never computed are what a judgement needs:
//!
//! 1. **The conditional mean crust over the retained set.** §3.5 of the audit
//!    names this as a debt: the 1113 m the coastline is cut below the shelf
//!    break is the depth of the cut, not the elevation a fix recovers, since
//!    raising sea level also drops the lowest band of today's land and so
//!    raises the mean of what remains. Only the conditional mean turns the
//!    cut depth into a defensible budget.
//! 2. **The craton radius distribution, with its coefficient of variation and
//!    the count at [`crate::crust::CRATON_RADIUS_MAX_RAD`].** Continent-size
//!    variety is the axis the obvious fix to the rescale destroys — an exact
//!    solve under a binding clamp can only pin every craton at the clamp — so
//!    the CV is what says whether a change bought area at the cost of a world
//!    of identical continents.
//! 3. **Post-repulsion pair separation.** `repel_cratons` guarantees
//!    reduction, not attainment. Recording what it achieves *today* is what
//!    lets a later reading distinguish a working repulsion pass from one
//!    saturating against radii it can no longer separate.
//!
//! None of the three changes a world: this module reads, and Stage B Task 1
//! is byte-inert by construction.

use crate::elevation::globe_elevation_terms;
use crate::globe::generate;
use crate::pins::TerrainPins;
use hornvale_kernel::{Geosphere, Seed};

/// The canonical globe level ([`crate::GLOBE_LEVEL`]), so the probe measures
/// the same surface the census does rather than a cheaper proxy.
/// plumb: pending(wave-1)
const LEVEL: u32 = crate::GLOBE_LEVEL;

/// World seeds the probe sweeps. Contiguous from 0 so it samples the head of
/// the census seed range (`studies/the-census.study.json` uses
/// `{from: 0, count: 1000}`), and `hornvale_terrain::generate` takes the world
/// seed directly — `worldgen` passes `world.seed` through unchanged — so these
/// are literally the census's own first worlds.
/// plumb: pending(wave-1)
const SEED_COUNT: u64 = 12;

/// The seven additive components of `elevation − sea_level` on a land vertex.
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
/// one sample of land vertices.
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

/// One land vertex's seven components, in [`Component::ALL`] order.
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
fn print_table(title: &str, stats: &[Stat; 7], y_mean: f64, y_var: f64, vertices: usize) {
    println!("\n{title}  (n = {vertices} land vertices)");
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

/// Minimum, mean, maximum and coefficient of variation (population sd over
/// mean) of a sample. The CV is the variety axis: it is dimensionless, so a
/// craton set that is uniformly large and one that is uniformly small are
/// both near 0, and only a set with a genuine spread of sizes reads high.
fn distribution(xs: &[f64]) -> (f64, f64, f64, f64) {
    let n = xs.len() as f64;
    let mean = xs.iter().sum::<f64>() / n;
    let min = xs.iter().copied().fold(f64::INFINITY, f64::min);
    let max = xs.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    let var = xs.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>() / n;
    (min, mean, max, var.sqrt() / mean)
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
/// every land vertex, and prints the pooled and within-world decompositions.
///
/// Cheap enough to stay in the commit gate: **roughly 3–6 s at ordinary load**
/// for twelve level-6 globes (terrain-only genesis, dev profile optimized since
/// decision 0113) — 3.26–3.98 s bare, 4.755–5.702 s under nextest, 3.80 s on an
/// independent re-run at loadavg ~16, and 2.85 s bare / 4.086 s under nextest
/// on a quiet box *after* Stage B's three accumulators, which therefore did not
/// move the cost. The figure is load-dependent, not a
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
/// claim: readout(prints the variance decomposition, the retained set's
/// conditional mean crust, and the majors' radius and pair-separation
/// distributions over the first [`SEED_COUNT`] census seeds; the seed loop
/// pools a sample rather than hunting one, and the only assertions are the
/// per-vertex conservation guard, the exactness of the covariance shares, and
/// relief's zero mean — decision 0093, the
/// `hollow_readout::report_cave_substrate` shape)
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
    // analytic continental supply the majors' final (post-rescale,
    // post-clamp) radii realise — not the budget the rescale aimed at, which
    // this probe does not compute — the grid area that actually clears the
    // continental threshold, and the land area the percentile ended up
    // granting.
    let (mut supply_sum, mut threshold_sum, mut land_sum) = (0.0_f64, 0.0_f64, 0.0_f64);
    // THE RETAINED SET (Stage B, the accumulator the audit's §3.5 says is
    // owed). The vertices whose crust clears `CONTINENTAL_THRESHOLD_KM` are
    // exactly the land a world would keep if sea level were re-placed at the
    // isostatic shelf break, so their *conditional mean crust* is the only
    // honest way to turn the 1113 m cut depth into an elevation a fix
    // recovers. Pooled vertex-weighted, the same weighting `mean_land_crust`
    // uses, so the two are directly comparable.
    let (mut retained_crust_sum, mut retained_vertices) = (0.0_f64, 0_usize);
    // Craton geometry, majors only — `globe.cratons` excludes microcontinents
    // and terranes by construction (see its field doc), and the variety and
    // crowding numbers below would silently describe a different population
    // if they were pooled in.
    let mut craton_radii: Vec<f64> = Vec::new();
    // Post-repulsion pair crowding: the centre-to-centre angle of every
    // major pair against the target `repel_cratons` aims at.
    let (mut pair_angles, mut pair_ratios) = (Vec::new(), Vec::new());
    for seed in 0..SEED_COUNT {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .expect("default pins never refuse a world");
        let globe = &outcome.globe;
        let terms = globe_elevation_terms(&geo, globe, Seed(seed));
        let sea = globe.sea_level.get();
        sea_levels.push(sea);
        let mut rows: Vec<Row> = Vec::new();
        for vertex in geo.vertices() {
            let elevation = globe.elevation.get(vertex).get();
            if elevation < sea {
                continue; // ocean: land is `e >= sea`, matching the metric
            }
            let t = terms.get(vertex);
            let carve = *globe.carve_delta_m.get(vertex);
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
                "seed {seed} vertex {vertex:?}: components sum to {reconstructed} but the \
                 pipeline produced {elevation}"
            );
            rows.push([
                t.base, t.boundary, t.hotspot, t.relief, t.epsilon, carve, -sea,
            ]);
            crust_sum += *globe.crust.get(vertex);
            crust_n += 1;
        }
        assert!(!rows.is_empty(), "seed {seed} has no land vertices");
        supply_sum += crate::crust::continental_supply(&globe.cratons);
        let mut seed_retained = 0_usize;
        for vertex in geo.vertices() {
            let crust_km = *globe.crust.get(vertex);
            if crust_km >= crate::crust::CONTINENTAL_THRESHOLD_KM {
                seed_retained += 1;
                retained_crust_sum += crust_km;
            }
        }
        retained_vertices += seed_retained;
        threshold_sum += seed_retained as f64 / globe.crust.len() as f64;
        land_sum += rows.len() as f64 / globe.crust.len() as f64;
        for c in &globe.cratons {
            craton_radii.push(c.radius_rad);
        }
        for (i, a) in globe.cratons.iter().enumerate() {
            for b in globe.cratons.iter().skip(i + 1) {
                let angle = hornvale_kernel::math::acos(
                    crate::plates::dot(a.center, b.center).clamp(-1.0, 1.0),
                );
                let target = crate::crust::REPEL_SEPARATION_FACTOR * (a.radius_rad + b.radius_rad);
                pair_angles.push(angle);
                pair_ratios.push(angle / target);
            }
        }
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
        "POOLED over every land vertex of every seed",
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
    for (seed, mean, vertices, shares) in &per_seed {
        print!("{seed:<6} {mean:>10.2} {vertices:>8}");
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
        "  analytic continental supply (majors, realised post-clamp)  {:>8.4}",
        supply_sum / n
    );
    println!(
        "  vertices actually at or above the continental threshold       {:>8.4}",
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

    // THE RETAINED SET. The audit's §3.5 is explicit that the 1113 m cut
    // depth is NOT a budget: raising sea level to the shelf break also drops
    // the lowest band of the current land, and removing a band that lies
    // entirely below the mean raises the mean of what remains. The inequality
    // is rigorous; the size of the gap needs this measurement.
    let retained_mean_crust = retained_crust_sum / retained_vertices as f64;
    let shelf_break_head_m = crate::elevation::isostatic_m(retained_mean_crust) - shelf_break_m;
    let today_head_m = crate::elevation::isostatic_m(mean_land_crust)
        - crate::elevation::isostatic_m(crust_km_at(mean_sea));
    println!(
        "\nTHE RETAINED SET (the vertices that would still be land if sea level rose to the shelf break)"
    );
    println!("  retained vertices                                {retained_vertices:>10}");
    println!("  conditional mean crust over the retained set  {retained_mean_crust:>10.2} km");
    println!("  mean crust over TODAY's land                  {mean_land_crust:>10.2} km");
    println!("  retained land would stand above the shelf break by {shelf_break_head_m:>10.2} m");
    println!("  today's land stands above today's coastline by     {today_head_m:>10.2} m");
    println!(
        "  => raising sea level to the shelf break recovers   {:>10.2} m of mean land elevation \
         (NOT the {:.2} m cut depth)",
        today_head_m - shelf_break_head_m,
        shelf_break_m - mean_sea
    );

    // CRATON GEOMETRY. Majors only: `globe.cratons` excludes microcontinents
    // and terranes by construction, and `continental_supply` counts the same
    // set, so this is the population the rescale actually budgets for.
    let (r_min, r_mean, r_max, r_cv) = distribution(&craton_radii);
    let at_clamp = craton_radii
        .iter()
        .filter(|r| **r >= crate::crust::CRATON_RADIUS_MAX_RAD - 1e-9)
        .count();
    println!("\nCRATON RADIUS DISTRIBUTION (majors only — microcontinents and terranes excluded)");
    println!(
        "  cratons over the sweep                        {:>10}",
        craton_radii.len()
    );
    println!("  min / mean / max radius (rad)     {r_min:>8.4} {r_mean:>8.4} {r_max:>8.4}");
    println!("  coefficient of variation                      {r_cv:>10.4}");
    println!(
        "  at the clamp (>= {:.2} rad)                    {:>10}  ({:.1}% of cratons)",
        crate::crust::CRATON_RADIUS_MAX_RAD,
        at_clamp,
        100.0 * at_clamp as f64 / craton_radii.len() as f64
    );

    // POST-REPULSION CROWDING. `repel_cratons` guarantees *reduction*, not
    // attainment, so the interesting number is not whether pairs meet their
    // target but how far short they fall — the baseline any change to the
    // radii has to be read against.
    let (a_min, a_mean, a_max, _) = distribution(&pair_angles);
    let (q_min, q_mean, q_max, _) = distribution(&pair_ratios);
    let short = pair_ratios.iter().filter(|q| **q < 1.0 - 1e-9).count();
    println!(
        "\nPOST-REPULSION PAIR SEPARATION (majors only; target = {} x (r_i + r_j))",
        crate::crust::REPEL_SEPARATION_FACTOR
    );
    println!(
        "  pairs over the sweep                          {:>10}",
        pair_angles.len()
    );
    println!("  min / mean / max centre angle (rad)  {a_min:>8.4} {a_mean:>8.4} {a_max:>8.4}");
    println!("  min / mean / max angle / target      {q_min:>8.4} {q_mean:>8.4} {q_max:>8.4}");
    println!(
        "  pairs still inside their target               {:>10}  ({:.1}% of pairs)",
        short,
        100.0 * short as f64 / pair_angles.len() as f64
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
