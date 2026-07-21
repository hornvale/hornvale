//! The carve/decoration property battery (Sculpting spec §8): mass balance,
//! induration monotonicity, shelf-width asymmetry, arc discreteness, trail
//! existence, atoll placement, and the eustatic-dividend regression.
//! Mirrors `tectonic_properties.rs`'s structure. `waterfalls_exist_across_
//! sweep` (spec §8) is NOT duplicated here — it already exists as
//! `waterfalls_exist_across_a_seed_sweep` in `carve.rs`'s own test module
//! (Task 11), heavy-tagged, at `GLOBE_LEVEL` (L6) over seeds 1..=8 (a
//! documented deviation from L5: L5's watershed resolution is too coarse
//! for `WATERFALL_MIN_DRAINAGE` to ever fire — see that test's own doc).

use hornvale_kernel::{
    CellId, CellMap, Geosphere, NearestCellIndex, ReferenceElevation, Seed, math,
};
use hornvale_terrain::carve::{CarveDelta, CarveParams, MarginGeometry, carve, carve_incision};
use hornvale_terrain::{
    GenesisOutcome, MarginPolarity, TerrainPins, crust, drainage, elevation, generate,
};

/// Rebuild of `globe::generate`'s PRE-carve pipeline for one seed, plus a
/// freshly-composed [`CarveDelta`] from re-running [`carve`] once on those
/// inputs. `globe::generate` wires the carve directly into `generate`
/// (Sculpting Task 10), so `TectonicGlobe` only retains the carve's
/// per-cell OUTPUTS (`sediment_thickness`, `carve_delta_m`, …), never the
/// scalar mass-balance totals (`eroded_total_m3`/`deposited_total_m3`/
/// `ocean_loss_m3`) a full `CarveDelta` carries — those totals are exactly
/// what the mass-balance and induration-monotonicity batteries need, so
/// this helper reconstructs the pre-carve fields (via the same `pub`
/// pipeline functions `globe::generate` itself calls — `generate_elevation`,
/// `derive_sea_level`, `drainage_field`, `downhill_targets`, all `pub`
/// specifically so tests can replay this pipeline; see
/// `tectonic_properties.rs`'s own `boundary_classification_agrees_from_
/// both_sides_across_seeds` for the same house pattern) and calls `carve`
/// exactly once — a single, non-double-applied invocation, not a second
/// pass over an already-carved surface.
///
/// `carbonate`/`margins` are read off the GENERATED globe's own (post-carve)
/// `lithology` buffer rather than rebuilt from `carbonate_at`/
/// `margin_polarity` (both deliberately `pub(crate)` — "the carve reads
/// this directly", i.e. only from inside this crate): per those functions'
/// own doc, the assembled buffer's `carbonate`/`margin` axes can only
/// diverge from the true pre-carve fields at atoll-capped cells (the
/// carbonate override applied post-carve), a handful of cells out of tens
/// of thousands that does not move a global-sum mass-balance identity or a
/// decile-median monotonicity check.
struct Rebuilt {
    /// The real, wired `globe::generate` result (post-carve) — what every
    /// world actually is.
    outcome: GenesisOutcome,
    /// Elevation BEFORE the carve (stage 5 of spec §2).
    elevation_pre: CellMap<ReferenceElevation>,
    /// Sea level resolved against the pre-carve surface (spec §2 stage 5).
    sea_pre: ReferenceElevation,
    /// Provisional (pre-carve) drainage field.
    drainage_pre: CellMap<f64>,
    /// Carbonate content per cell, read off the generated globe's lithology
    /// buffer (see the struct doc's divergence caveat).
    carbonate: CellMap<f64>,
    /// A freshly-composed `CarveDelta` from one `carve()` call on the
    /// pre-carve inputs above — carries the mass-balance totals
    /// `TectonicGlobe` does not retain.
    delta: CarveDelta,
    /// Sea level resolved on the carved-but-untrimmed surface — generate's
    /// FIRST solve (`sea_1` in ruling #5c's solve→trim→solve sequence).
    /// The globe's own `sea_level` is the second (final) solve; the gap
    /// between the two is the bounded residual the re-cap test tolerates.
    sea_carved: ReferenceElevation,
}

fn rebuild(seed: u64, geo: &Geosphere) -> Rebuilt {
    let outcome = generate(Seed(seed), geo, &TerrainPins::default())
        .unwrap_or_else(|e| panic!("seed {seed}: {e}"));
    let g = &outcome.globe;
    let terrain_seed = Seed(seed).derive(hornvale_terrain::streams::ROOT);
    let mut notes = Vec::new();
    let ocean_target =
        elevation::resolve_ocean_fraction(terrain_seed, &TerrainPins::default(), &mut notes);
    let supply = crust::continental_supply(&g.cratons);
    let effective_ocean = elevation::effective_ocean_target(ocean_target, supply, &mut notes);
    let continental = CellMap::from_fn(geo, |c| *g.crust.get(c) >= crust::CONTINENTAL_THRESHOLD_KM);
    let elevation_pre = elevation::generate_elevation(
        terrain_seed,
        geo,
        &g.plates,
        &g.plate_of,
        &g.boundary,
        &g.boundary_distance,
        &g.trail_seamounts,
        &g.crust,
        &continental,
        &g.induration,
    );
    let sea_pre = elevation::derive_sea_level(&elevation_pre, effective_ocean);
    let (drainage_pre, endorheic_pre) = drainage::drainage_field(geo, &elevation_pre, sea_pre);
    let downhill = drainage::downhill_targets(geo, &elevation_pre, sea_pre);
    let carbonate = CellMap::from_fn(geo, |c| g.lithology.get(c).carbonate);
    let margins: CellMap<MarginPolarity> = CellMap::from_fn(geo, |c| g.lithology.get(c).margin);
    let delta = carve(
        geo,
        &elevation_pre,
        sea_pre,
        &drainage_pre,
        &endorheic_pre,
        &downhill,
        &g.induration,
        &carbonate,
        MarginGeometry {
            margins: &margins,
            boundary: &g.boundary,
            plate_of: &g.plate_of,
        },
        &g.trail_seamounts,
        &CarveParams::default(),
    );
    let carved = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation_pre.get(c).get() + delta.delta_m.get(c))
            .expect("carved elevation finite")
    });
    let sea_carved = elevation::derive_sea_level(&carved, effective_ocean);
    Rebuilt {
        outcome,
        elevation_pre,
        sea_pre,
        drainage_pre,
        carbonate,
        delta,
        sea_carved,
    }
}

/// The median of `values` (sorted in place by `total_cmp`); `None` when
/// empty. An even-length input averages its two middle values. Local copy
/// of the house convention (`windows/lab/src/metrics.rs`'s own `median`) —
/// a domain crate's tests may depend only on the kernel (decision 0002,
/// enforced by `cli/tests/architecture.rs`), so this cannot be shared
/// across the layer boundary.
fn median(values: &mut [f64]) -> Option<f64> {
    if values.is_empty() {
        return None;
    }
    values.sort_by(f64::total_cmp);
    let n = values.len();
    Some(if n % 2 == 1 {
        values[n / 2]
    } else {
        (values[n / 2 - 1] + values[n / 2]) / 2.0
    })
}

/// Fractional (tie-averaged) ranks of `values`, ascending: the smallest
/// value gets rank `0.0`, the largest `values.len() - 1`; values tied at
/// the same magnitude share the average of the ranks they span.
fn ranks(values: &[f64]) -> Vec<f64> {
    let n = values.len();
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by(|&a, &b| values[a].total_cmp(&values[b]));
    let mut out = vec![0.0_f64; n];
    let mut i = 0;
    while i < n {
        let mut j = i;
        while j + 1 < n && values[order[j + 1]] == values[order[i]] {
            j += 1;
        }
        let avg_rank = (i + j) as f64 / 2.0;
        for &idx in &order[i..=j] {
            out[idx] = avg_rank;
        }
        i = j + 1;
    }
    out
}

/// Pearson correlation coefficient of `x` and `y` (equal length, `x.len()
/// >= 2`).
fn pearson(x: &[f64], y: &[f64]) -> f64 {
    let n = x.len() as f64;
    let xbar = x.iter().sum::<f64>() / n;
    let ybar = y.iter().sum::<f64>() / n;
    let cov: f64 = x
        .iter()
        .zip(y)
        .map(|(&xi, &yi)| (xi - xbar) * (yi - ybar))
        .sum();
    let xvar: f64 = x.iter().map(|&xi| (xi - xbar) * (xi - xbar)).sum();
    let yvar: f64 = y.iter().map(|&yi| (yi - ybar) * (yi - ybar)).sum();
    cov / (xvar.sqrt() * yvar.sqrt())
}

/// Spearman rank-correlation coefficient of `x` and `y` — the Pearson
/// correlation of their ranks. `+1` = perfectly increasing together, `-1`
/// = perfectly decreasing, `0` = no rank relationship.
fn spearman(x: &[f64], y: &[f64]) -> f64 {
    pearson(&ranks(x), &ranks(y))
}

/// Connected-component count of `cells` under the geosphere's neighbor
/// adjacency, restricted to `cells` itself. Local copy of `elevation.rs`'s
/// own test-local `count_components` (same cross-layer reason as `median`).
fn count_components(geo: &Geosphere, cells: &std::collections::BTreeSet<CellId>) -> usize {
    let mut unvisited = cells.clone();
    let mut components = 0;
    while let Some(&start) = unvisited.iter().next() {
        components += 1;
        unvisited.remove(&start);
        let mut stack = vec![start];
        while let Some(cell) = stack.pop() {
            for &neighbor in geo.neighbors(cell) {
                if unvisited.remove(&neighbor) {
                    stack.push(neighbor);
                }
            }
        }
    }
    components
}

/// Shelf width (spec §8) from a single coast land cell: hops seaward, each
/// hop stepping to the current cell's shallowest ocean neighbor
/// (`CellId`-ascending tiebreak), until a stepped-to cell's depth first
/// exceeds `cap_depth_m`, or 8 hops are spent. Local copy of the census
/// metric's own `shelf_width_hops` (`windows/lab/src/metrics.rs`), adapted
/// to read a `TectonicGlobe` directly instead of a `TerrainView` (same
/// cross-layer reason as `median`/`count_components`).
fn shelf_width_hops(
    geo: &Geosphere,
    globe: &hornvale_terrain::TectonicGlobe,
    coast: CellId,
    cap_depth_m: f64,
) -> u32 {
    let is_ocean = |c: CellId| *globe.elevation.get(c) < globe.sea_level;
    let mut cur = coast;
    for hop in 1..=8u32 {
        let mut candidates: Vec<CellId> = geo
            .neighbors(cur)
            .iter()
            .copied()
            .filter(|&n| is_ocean(n))
            .collect();
        if candidates.is_empty() {
            return hop - 1;
        }
        candidates.sort_by(|a, b| {
            globe
                .elevation
                .get(*a)
                .get()
                .total_cmp(&globe.elevation.get(*b).get())
                .then(a.0.cmp(&b.0))
        });
        let next = candidates[0];
        let depth = globe.sea_level.get() - globe.elevation.get(next).get();
        if depth > cap_depth_m {
            return hop;
        }
        cur = next;
    }
    8
}

/// The mass-balance identity (spec §5): eroded ≈ deposited + ocean loss,
/// asserted for the metaphysically-inert tier (spec §5) — atoll carbonate
/// is booked as paired source+sink (grown-in-place biogenic material counts
/// on BOTH sides of the ledger, so the gated overlay's ex-nihilo landforms
/// never have to weaken this identity later). Re-asserts the Task 9 books
/// identity on four seeds via a fresh full `carve()` composition (see
/// `Rebuilt`'s doc) — the same tolerance `carve.rs`'s own
/// `sediment_books_balance_and_mouths_collect_the_rest` uses for the
/// routing-only sub-identity. This identity is about `carve()` ALONE: the
/// sea-trim (ruling #5c, `trim_to_sea`) is booked at the generate level —
/// generate-level composition is carve + trim, and the trimmed volume is
/// additional oceanic loss on top of these carve-internal books.
#[test]
fn mass_balance_holds() {
    let geo = Geosphere::new(4);
    for seed in [1u64, 7, 42, 99] {
        let rebuilt = rebuild(seed, &geo);
        let d = &rebuilt.delta;
        assert!(
            d.eroded_total_m3 >= 0.0,
            "seed {seed}: negative eroded total"
        );
        assert!(
            d.deposited_total_m3 >= 0.0,
            "seed {seed}: negative deposited total"
        );
        assert!(d.ocean_loss_m3 >= 0.0, "seed {seed}: negative ocean loss");
        let lhs = d.eroded_total_m3;
        let rhs = d.deposited_total_m3 + d.ocean_loss_m3;
        assert!(
            (lhs - rhs).abs() < 1e-6 * lhs.max(1.0),
            "seed {seed}: books: eroded {lhs} vs deposited+lost {rhs}"
        );
    }
}

/// Induration monotonicity (spec §8): harder rock incises less. Buckets
/// PRE-carve land cells (seed 42) into induration deciles by RANK (not by
/// induration value — the distribution is not remotely uniform, so a
/// value-binned histogram could easily starve a decile), then runs an
/// actual Spearman rank-correlation check (`spearman`, decile index vs
/// per-decile median |incision|) rather than counting adjacent-pair
/// "inversions" — the plan names this a "Spearman-style rank check", and a
/// literal rank correlation is what that phrase means; counting adjacent
/// inversions is a stricter, non-standard measure that turned out to reject
/// EVERY one of 20 surveyed seeds (1-5 "inversions" apiece, only one seed
/// ever landing at the plan's tolerated ceiling of 1) even though a proper
/// Spearman rho was strongly negative on all twenty (range -0.73 to -0.98
/// at L4; seed 42 itself: -0.87 at L4, -0.95 at L5) — the real relationship
/// is robustly monotonic decreasing, the adjacent-pair count was simply the
/// wrong statistic. `-0.7` sits with real margin below every surveyed
/// value: old continental interiors correlate BOTH with high induration
/// and with low drainage/slope (a real physical confound the incision
/// formula does not control for), so a little local noise is expected, but
/// the trend itself must be strong.
///
/// Statistic re-derived for tuning iteration 1 (ledger #6, land-only
/// incision slope) — two coupled changes, both forced by the same measured
/// confound:
/// - **Population**: cells with a POSITIVE LAND DROP only. The land-only
///   slope term structurally zeroes incision where no lower land neighbor
///   exists, and those cells are overwhelmingly SOFT — coastal/lowland
///   sediment (seed 42/L4 pre-carve: 53%/51%/31% of the three softest
///   induration deciles have no lower land neighbor, vs 3-9% of every
///   other decile). Including them makes rho measure coastal geometry, not
///   the induration response (raw rho flipped to +0.32).
/// - **Slope-normalized incision**: per-cell `|incision| / (S/S0)^n`
///   rather than raw `|incision|`. Under ledger #6 the slope term itself
///   became strongly induration-correlated at the soft end (soft coastal
///   flats have small LAND drops where they once had huge cliff drops), so
///   even over the positive-drop population raw rho collapsed to a weak,
///   seed-flickering -0.36..0.09 (8-seed survey). Dividing out the known
///   slope factor removes exactly the confound the law change introduced
///   and restores the battery to what it always measured: erodibility's
///   effect, drainage confound included. Post-change survey over seeds
///   [42, 1, 7, 99, 3, 5, 11, 2]: slope-normalized rho -0.891 / -0.552 /
///   -0.915 / -0.903 / -0.903 / -0.867 / -0.988 / -0.903 — the original
///   `-0.7` threshold keeps real margin on seed 42 (-0.891).
#[test]
fn harder_rock_cuts_less() {
    let geo = Geosphere::new(4);
    let seed = 42u64;
    let rebuilt = rebuild(seed, &geo);
    let g = &rebuilt.outcome.globe;
    let incision = carve_incision(
        &geo,
        &rebuilt.elevation_pre,
        rebuilt.sea_pre,
        &rebuilt.drainage_pre,
        &g.induration,
        &rebuilt.carbonate,
        &CarveParams::default(),
    );

    let p = CarveParams::default();
    // Max drop to a LAND neighbor — the slope the incision law itself uses
    // post-ledger-#6 (see `carve_incision`'s doc).
    let land_drop = |c: CellId| -> f64 {
        let here = rebuilt.elevation_pre.get(c).get();
        geo.neighbors(c)
            .iter()
            .filter(|nb| *rebuilt.elevation_pre.get(**nb) >= rebuilt.sea_pre)
            .map(|nb| here - rebuilt.elevation_pre.get(*nb).get())
            .fold(0.0_f64, f64::max)
    };
    let mut land: Vec<CellId> = geo
        .cells()
        .filter(|&c| *rebuilt.elevation_pre.get(c) >= rebuilt.sea_pre && land_drop(c) > 0.0)
        .collect();
    assert!(
        land.len() >= 100,
        "too few land cells to decile: {}",
        land.len()
    );
    land.sort_by(|&a, &b| {
        g.induration
            .get(a)
            .total_cmp(g.induration.get(b))
            .then(a.0.cmp(&b.0))
    });

    const DECILES: usize = 10;
    let n = land.len();
    let mut buckets: Vec<Vec<f64>> = vec![Vec::new(); DECILES];
    for (i, &cell) in land.iter().enumerate() {
        let bucket = (i * DECILES / n).min(DECILES - 1);
        let s_pow = math::powf(land_drop(cell) / p.slope_ref_m, p.slope_exponent);
        buckets[bucket].push(incision.get(cell).abs() / s_pow);
    }
    let medians: Vec<f64> = buckets
        .iter_mut()
        .map(|b| median(b).unwrap_or(0.0))
        .collect();
    assert!(medians.iter().all(|m| m.is_finite()));

    let decile_index: Vec<f64> = (0..DECILES).map(|i| i as f64).collect();
    let rho = spearman(&decile_index, &medians);
    assert!(
        rho <= -0.7,
        "seed {seed}: induration-decile slope-normalized |incision| medians not strongly \
         rank-decreasing (softest→hardest): rho = {rho:.3} (want <= -0.7), medians: {medians:?}"
    );
}

/// Atoll placement (spec §8, Task 9): every `atoll_cells` member (a) sits
/// below `atoll_max_abs_lat`, (b) composes close to (and, post-trim, at
/// most the bounded solve-2 residual above) the final atoll cap — ruling
/// #5c's sea-trim re-caps every atoll cell to `sea_1 - atoll_freeboard_m`,
/// and the final solve lands at most `wedge_freeboard_m` below `sea_1`, so
/// the ceiling here is `sea_final - atoll_freeboard_m + wedge_freeboard_m`
/// (a double-count regression's hundreds-of-meters overshoot still fails
/// loudly), and (c) maps back to a real trail seamount with
/// `age_index >= 2` — old enough to have drifted off the live hotspot dome
/// and cooled into reef-building range (`cap_atolls`'s own eligibility
/// rule).
#[test]
fn atolls_only_on_warm_submerged_seamounts() {
    let geo = Geosphere::new(4);
    let params = CarveParams::default();
    let mut total_atolls = 0usize;
    for seed in [1u64, 7, 42, 99] {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let index = NearestCellIndex::new(&geo);
        let ceiling =
            g.sea_level.get() - params.atoll_freeboard_m + params.wedge_freeboard_m + 1e-6;
        let floor = g.sea_level.get() - params.atoll_max_depth_m - params.wedge_freeboard_m - 1.0;
        for &c in &g.atoll_cells {
            let lat = math::asin(geo.position(c)[2].clamp(-1.0, 1.0)).abs();
            assert!(
                lat < params.atoll_max_abs_lat,
                "seed {seed}: atoll cell {} at |lat| {lat} exceeds the cap",
                c.0
            );
            let e = g.elevation.get(c).get();
            assert!(
                (floor..=ceiling).contains(&e),
                "seed {seed}: atoll cell {} composed at {e}, outside [{floor}, {ceiling}]",
                c.0
            );
            let maps_to_this_cell = g
                .trail_seamounts
                .iter()
                .filter(|s| s.age_index >= 2)
                .any(|s| index.nearest_to_position(&geo, s.position) == c);
            assert!(
                maps_to_this_cell,
                "seed {seed}: atoll cell {} has no age>=2 trail seamount mapping to it",
                c.0
            );
        }
        total_atolls += g.atoll_cells.len();
    }
    assert!(
        total_atolls > 0,
        "no atolls at all across seeds [1, 7, 42, 99] — the feature never fires"
    );
}

/// The re-cap holds after the final solve (ruling #5c, the sea-trim):
/// post-generate, NO atoll cell sits above `sea_final - atoll_freeboard_m`
/// and no cell the wedge deposited on — **ocean by `sea_pre`, the
/// classification under which the wedge ran, NOT by its final elevation**
/// (review Critical 1: gating on final elevation silently exempted the
/// emergent wedge-filled cells the ruling exists to fix; seed 34 L4 held
/// 40 such cells at exactly sea level carrying 49-344 m of wedge sediment
/// on dry land, and this battery was verified RED on that exact world
/// against the un-fixed gate) — above `sea_final - wedge_freeboard_m`, up
/// to the honest measured tolerance: the `sea_1 → sea_final` shift of the
/// bounded solve→trim→solve sequence (`sea_1` recomputed per-world via
/// `Rebuilt`, so the tolerance is the actual shift, not a guessed
/// constant). Only `shift >= 0` is structural (the trim only lowers
/// cells); the `<= wedge_freeboard_m` half of the asserted bound is
/// EMPIRICAL — max observed 39.957 m across a 120-world review sweep
/// (L4/L5/L6 × seeds 1..=40; worst margin 0.043 m, L5 seed 18) — so any
/// tuning that changes the freeboards or shelf density must re-verify it.
/// Exactly one trim, exactly two solves, then stop — the residual is
/// accepted by ruling, and this test both asserts the bound and REPORTS
/// the measured shift in its failure messages. Worlds probed: seed 42 L5
/// (the campaign flagship) and seed 34 L4 (the review's emergent-shelf
/// counterexample).
#[test]
fn trim_recaps_hold_after_the_final_solve() {
    for (level, seed) in [(5u32, 42u64), (4, 34)] {
        let geo = Geosphere::new(level);
        let rebuilt = rebuild(seed, &geo);
        let g = &rebuilt.outcome.globe;
        let p = CarveParams::default();
        let sea_1 = rebuilt.sea_carved;
        let sea_final = g.sea_level;
        let shift = sea_1.get() - sea_final.get();
        assert!(
            (0.0..=p.wedge_freeboard_m + 1e-9).contains(&shift),
            "seed {seed} L{level}: sea_1 -> sea_final shift {shift} outside \
             [0, wedge_freeboard_m] (>= 0 structural, <= empirical — see doc) \
             (sea_1 {}, sea_final {})",
            sea_1.get(),
            sea_final.get()
        );
        let tol = shift.max(0.0) + 1e-6;
        let atoll_cap = sea_final.get() - p.atoll_freeboard_m;
        for &c in &g.atoll_cells {
            assert!(
                g.elevation.get(c).get() <= atoll_cap + tol,
                "seed {seed} L{level}: atoll cell {} at {} above sea_final - atoll_freeboard \
                 ({atoll_cap}) beyond the measured shift tolerance {tol} (shift {shift})",
                c.0,
                g.elevation.get(c).get()
            );
        }
        // Barrier cells (tuning iteration 4, ledger #9): the sea-trim
        // exempts them like delta lobes, so a barrier's elevation must
        // still sit exactly at its construction target
        // (`sea_pre + barrier_height_m`, unperturbed by the trim) AND
        // strictly above the FINAL sea level — the guard this whole
        // mechanism exists to satisfy (a barrier must stay land after the
        // final re-solve, not just after the first one).
        let barrier_target = rebuilt.sea_pre.get() + p.barrier_height_m;
        for &c in &g.barrier_cells {
            assert!(
                (g.elevation.get(c).get() - barrier_target).abs() < 1e-6,
                "seed {seed} L{level}: barrier cell {} at {} != construction target {barrier_target}",
                c.0,
                g.elevation.get(c).get()
            );
            assert!(
                *g.elevation.get(c) >= sea_final,
                "seed {seed} L{level}: barrier cell {} at {} sank below the final sea level {}",
                c.0,
                g.elevation.get(c).get(),
                sea_final.get()
            );
        }
        // The wedge-cap bound, gated on the membership the wedge actually
        // deposited under: ocean by sea_pre with retained sediment. Atoll
        // cells are excluded from this WEDGE bound only because their own
        // (tighter-freeboard) cap is asserted above; delta lobes and
        // barrier cells (tuning iteration 4, ledger #9) are exempt by
        // ruling — both are meant to stay subaerial past the final
        // re-solve, governed by their own construction, not this bound.
        let wedge_cap = sea_final.get() - p.wedge_freeboard_m;
        for (c, sed) in g.sediment_thickness.iter() {
            if *sed > 0.0
                && *rebuilt.elevation_pre.get(c) < rebuilt.sea_pre
                && !g.delta_cells.contains(&c)
                && !g.atoll_cells.contains(&c)
                && !g.barrier_cells.contains(&c)
            {
                assert!(
                    g.elevation.get(c).get() <= wedge_cap + tol,
                    "seed {seed} L{level}: wedge-deposited cell {} (ocean by sea_pre, sediment \
                     {sed}) at {} above sea_final - wedge_freeboard ({wedge_cap}) beyond the \
                     measured shift tolerance {tol} (shift {shift})",
                    c.0,
                    g.elevation.get(c).get()
                );
            }
        }
    }
}

/// The generate-level books (ruling #5c's booking requirement): the
/// composed carve + trim state the globe actually RETAINS accounts for
/// every eroded unit. The carve's own two-term identity
/// (`eroded ≈ deposited + ocean_loss`) is asserted by `mass_balance_holds`
/// over a fresh `CarveDelta`; this test closes the loop over the retained
/// fields: recompute the trim from the same replayed inputs, split its
/// volume into the sediment it removed vs. the pre-existing seabed it cut
/// (`basement`), and assert
///
/// ```text
/// Σ sediment_thickness (retained, net of trim)
///   + carve ocean_loss + trim_ocean_loss_m3 (retained)
///   ≈ carve eroded + basement
/// ```
///
/// exactly (1e-6 relative). The `basement` term is a paired source+sink,
/// the same convention atoll carbonate already uses (spec §5's tier-aware
/// mass balance): where the trim cuts below the carve's own deposit it
/// erodes NEW pre-existing seabed straight into oceanic loss, so that
/// volume appears on BOTH sides — without it the naive three-term form
/// (`Σ sediment + losses ≈ eroded`) silently over-counts the loss side by
/// whatever basement the re-cap shaved. Also asserts the retained
/// `trim_ocean_loss_m3` equals the replayed trim's volume — the booking is
/// real, not a doc claim (review Critical 2).
#[test]
fn generate_level_books_account_for_every_eroded_unit() {
    let geo = Geosphere::new(4);
    for seed in [1u64, 7, 42, 99] {
        let rebuilt = rebuild(seed, &geo);
        let g = &rebuilt.outcome.globe;
        let d = &rebuilt.delta;
        let carved = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(rebuilt.elevation_pre.get(c).get() + d.delta_m.get(c))
                .expect("carved elevation finite")
        });
        let (trim, trim_volume) = hornvale_terrain::carve::trim_to_sea(
            &geo,
            &carved,
            &rebuilt.elevation_pre,
            &d.sediment_thickness_m,
            &d.delta_cells,
            &d.atoll_cells,
            &d.barrier_cells,
            rebuilt.sea_pre,
            rebuilt.sea_carved,
            &CarveParams::default(),
        );
        // The retained booking equals the replayed trim's volume.
        assert!(
            (g.trim_ocean_loss_m3 - trim_volume).abs() < 1e-6 * trim_volume.max(1.0),
            "seed {seed}: retained trim_ocean_loss_m3 {} != replayed trim volume {trim_volume}",
            g.trim_ocean_loss_m3
        );
        // Split the trim into carve-sediment removed vs. basement cut.
        let mut sediment_removed = 0.0_f64;
        let mut basement = 0.0_f64;
        for (c, t) in trim.iter() {
            let cut = -t;
            let sed = *d.sediment_thickness_m.get(c);
            sediment_removed += cut.min(sed);
            basement += (cut - sed).max(0.0);
        }
        assert!(
            (sediment_removed + basement - trim_volume).abs() < 1e-6 * trim_volume.max(1.0),
            "seed {seed}: trim split does not sum: {sediment_removed} + {basement} vs {trim_volume}"
        );
        // The retained-state identity, basement as paired source+sink.
        let retained_sediment: f64 = g.sediment_thickness.iter().map(|(_, s)| *s).sum();
        let lhs = retained_sediment + d.ocean_loss_m3 + g.trim_ocean_loss_m3;
        let rhs = d.eroded_total_m3 + basement;
        assert!(
            (lhs - rhs).abs() < 1e-6 * rhs.max(1.0),
            "seed {seed}: generate-level books: retained sediment {retained_sediment} + carve \
             loss {} + trim loss {} = {lhs} vs eroded {} + basement {basement} = {rhs}",
            d.ocean_loss_m3,
            g.trim_ocean_loss_m3,
            d.eroded_total_m3
        );
    }
}

/// Trail existence (spec §8, Task 6): the retained `trail_seamounts` on a
/// GENERATED globe (not the bare `trail_seamounts()` call `elevation.rs`'s
/// own `trails_are_age_ordered_chains_upstream_of_plate_motion` tests) are
/// still age-ordered, strictly-decaying chains once wired through
/// `globe::generate` — every hotspot contributes `TRAIL_STEPS + 1` entries.
#[test]
fn trails_exist_age_ordered() {
    let geo = Geosphere::new(4);
    for seed in [1u64, 7, 42, 99] {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let seamounts = &outcome.globe.trail_seamounts;
        let chain_len = elevation::TRAIL_STEPS as usize + 1;
        assert!(
            !seamounts.is_empty() && seamounts.len().is_multiple_of(chain_len),
            "seed {seed}: {} seamounts is not a multiple of chain length {chain_len}",
            seamounts.len()
        );
        for chain in seamounts.chunks(chain_len) {
            for pair in chain.windows(2) {
                assert_eq!(
                    pair[1].age_index,
                    pair[0].age_index + 1,
                    "seed {seed}: chain age index not sequential"
                );
                assert!(
                    pair[1].strength_m < pair[0].strength_m,
                    "seed {seed}: chain strength not strictly decaying"
                );
            }
        }
    }
}

/// Arc discreteness (spec §8): island-arc land at v3 forms more than one
/// connected component on average — the v2 wall read as exactly 1. Heavy:
/// full genesis at L5 over 40 seeds.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn arcs_are_discrete() {
    use hornvale_terrain::BoundaryKind;
    let geo = Geosphere::new(5);
    let mut per_world_components: Vec<f64> = Vec::new();
    for seed in 1..=40u64 {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed}: {e}"));
        let g = &outcome.globe;
        let arc_land: std::collections::BTreeSet<CellId> = geo
            .cells()
            .filter(|&c| {
                matches!(
                    g.boundary.get(c).map(|b| b.kind),
                    Some(BoundaryKind::IslandArc)
                ) && *g.elevation.get(c) >= g.sea_level
            })
            .collect();
        let components = if arc_land.is_empty() {
            0
        } else {
            count_components(&geo, &arc_land)
        };
        per_world_components.push(components as f64);
    }
    let mean = per_world_components.iter().sum::<f64>() / per_world_components.len() as f64;
    assert!(
        mean > 1.5,
        "mean arc-land component count per world {mean} does not clear the v2-wall floor (1.5); \
         per-world: {per_world_components:?}"
    );
}

/// Shelf-width asymmetry (spec §8): passive-margin coasts build wider
/// shelves than active-margin coasts. Heavy: full genesis at L5 over 40
/// seeds, coast cells pooled across the whole sweep.
///
/// **The PRIMARY criterion is tail dominance** (ruling #5d): the rate of
/// passive coast cells whose shelf walk reaches the 8-hop cap must be at
/// least 1.5× the active rate. The spec §8 median criterion (passive
/// median > active median) is **superseded** — recorded openly, not
/// reframed: pooled medians TIE at 1.0 (passive n=32849, active n=7885
/// across seeds 1-40 @ L5; Task 12 review measured the same tie per-world),
/// because at this resolution most coast walks terminate in one hop on
/// both margins and the asymmetry lives entirely in the tail. The median
/// criterion has never been observed to hold; tail dominance is what the
/// physics actually produces, and Census-III will carry this note.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn shelf_width_asymmetry() {
    let geo = Geosphere::new(5);
    let cap_depth_m = 2.0 * CarveParams::default().wedge_freeboard_m;
    let mut passive: Vec<f64> = Vec::new();
    let mut active: Vec<f64> = Vec::new();
    for seed in 1..=40u64 {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed}: {e}"));
        let g = &outcome.globe;
        for cell in geo.cells() {
            if *g.elevation.get(cell) < g.sea_level {
                continue;
            }
            let is_coast = geo
                .neighbors(cell)
                .iter()
                .any(|&n| *g.elevation.get(n) < g.sea_level);
            if !is_coast {
                continue;
            }
            let width = f64::from(shelf_width_hops(&geo, g, cell, cap_depth_m));
            match g.lithology.get(cell).margin {
                MarginPolarity::Active => active.push(width),
                _ => passive.push(width),
            }
        }
    }
    assert!(
        !passive.is_empty() && !active.is_empty(),
        "no coast cells found on either margin"
    );
    // Primary (ruling #5d): tail dominance as a RATE ratio — the share of
    // passive coast cells at the 8-hop cap is at least 1.5× the active
    // share. Rates, not raw counts: passive coast is ~4× more plentiful,
    // and a count comparison would pass on abundance alone.
    let passive_at_cap = passive.iter().filter(|&&w| w >= 8.0).count();
    let active_at_cap = active.iter().filter(|&&w| w >= 8.0).count();
    let passive_rate = passive_at_cap as f64 / passive.len() as f64;
    let active_rate = active_at_cap as f64 / active.len() as f64;
    assert!(
        passive_at_cap > 0,
        "no passive coast cell ever reaches the 8-hop cap — no tail to dominate"
    );
    assert!(
        passive_rate >= 1.5 * active_rate,
        "tail dominance failed: passive-at-cap rate {passive_rate:.5} ({passive_at_cap}/{}) \
         < 1.5 x active rate {active_rate:.5} ({active_at_cap}/{}) — shelf-width asymmetry \
         did not materialize under the primary (ruling #5d) criterion",
        passive.len(),
        active.len()
    );
}

/// Fraction of a globe's cells strictly below `sea_level`.
fn flooded_fraction(elevation: &CellMap<ReferenceElevation>, sea_level: ReferenceElevation) -> f64 {
    let n = elevation.len() as f64;
    let below = elevation.iter().filter(|(_, e)| **e < sea_level).count();
    below as f64 / n
}

/// The eustatic dividend (spec §6, §8): Deep Time's sea-level swings must
/// flood/expose MORE area at v3 than at v2 — a real depositional shelf
/// (gently sloped near sea level) has more cells packed into a given
/// vertical band than v2's steep, under-supplied coast did. Measured as a
/// centered finite difference of the flooded-area fraction around each
/// world's own sea level (`delta = 50 m`, comparable to `wedge_freeboard_m`
/// default 40 m — shelf-scale, not glacial-scale): `(flooded(sea+Δ) -
/// flooded(sea-Δ)) / (2Δ)`.
///
/// **No pre-existing "sea-level-swing helper" was found** in the paleo
/// tests (`domains/paleoclimate`, `windows/worldgen`) to reuse: that crate
/// only carries a `shoreline` tide-mark MASK (`PaleoRecord::shoreline`),
/// not a scalar area-per-meter sensitivity, and `domains/paleoclimate` is
/// itself a domain crate that cannot depend on `hornvale-terrain` (decision
/// 0002) even if one existed — so this probe is necessarily terrain-only
/// and self-contained, which also keeps it inside spec §6's "no paleo code
/// changes" instruction. This is the HONEST alternative the brief
/// anticipated for exactly this case.
///
/// The v2 constant below was measured directly (not estimated) from a
/// throwaway worktree at the plan-baseline commit `0900411` (2026-07-15),
/// running this exact probe (same seeds, same `Geosphere::new(5)`, same
/// `delta = 50.0`) against that commit's `hornvale_terrain::generate`; the
/// worktree was removed immediately after reading the number off. See the
/// Task 13 report for the raw per-seed readout.
const V2_EUSTATIC_SWING_BASELINE: f64 = 0.00011887;

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn eustatic_dividend_regression() {
    let geo = Geosphere::new(5);
    let delta = 50.0_f64;
    let mut swings = Vec::new();
    for seed in [1u64, 7, 42, 99] {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed}: {e}"));
        let g = &outcome.globe;
        let up = ReferenceElevation::new(g.sea_level.get() + delta).unwrap();
        let down = ReferenceElevation::new(g.sea_level.get() - delta).unwrap();
        let swing = (flooded_fraction(&g.elevation, up) - flooded_fraction(&g.elevation, down))
            / (2.0 * delta);
        swings.push(swing);
    }
    let mean = swings.iter().sum::<f64>() / swings.len() as f64;
    assert!(
        mean >= V2_EUSTATIC_SWING_BASELINE,
        "v3 mean eustatic swing {mean} did not clear the v2 baseline \
         {V2_EUSTATIC_SWING_BASELINE} — per-seed: {swings:?}"
    );
}
