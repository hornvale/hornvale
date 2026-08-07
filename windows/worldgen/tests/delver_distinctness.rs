//! THE DELVERS — the pairwise capacity-distinctness instrument.
//!
//! Two authored kinds are *the same kind wearing two names* exactly when the
//! world cannot tell them apart: when the per-cell suitability field
//! [`hornvale_worldgen::per_species_suitability`] computes for one is a linear
//! image of the other's. This measures that directly — Pearson `r` between two
//! kinds' suitability over the LAND cells of a built world.
//!
//! **Built before the five dwarves exist, deliberately.** The campaign's P2 and
//! P3 are *nulls*: they predict pairs that will read as near-identical. An
//! instrument authored after seeing those kinds could be tuned — a wider land
//! mask, a different denominator — until it reported the answer already
//! expected. Frozen first, it cannot be.
//!
//! ## The controls are the deliverable
//!
//! A probe whose every assertion reads "these two are the same" is
//! indistinguishable from a probe that computes nothing and returns a constant.
//! The Benchmark shipped exactly that: a guard that was vacuous and green
//! because it sampled the one cell where the bug was invisible. So this file
//! pins the instrument in BOTH directions before any dwarf is authored —
//! it must separate two kinds known to differ, and it must return exactly
//! `1.0` for a kind against itself.
//!
//! ## Measured, 2026-08-07, seed 42, on the pre-dwarf roster
//!
//! ```text
//!   pair                            r          land cells
//!   gnoll   vs kobold        0.291124            11,066
//!   goblin  vs goblin        1.000000            11,066
//! ```
//!
//! Kobold (a highland specialist, `devotion_elev` 0.95) and gnoll (arid,
//! `devotion_elev` 0.40, elevation-bound on all land — Task 1's table) share
//! `r² ≈ 8.5%` of their variance and no more, while a kind against itself
//! returns unity to within a float ulp. The instrument discriminates across
//! that whole span, so a null it later reports is a measurement rather than a
//! tautology.
//!
//! ## What `r` is and is not
//!
//! Pearson `r` is invariant under a positive affine rescale, so two kinds whose
//! fields differ only in overall magnitude read as `1.0`. That is the intended
//! reading: this asks whether the world SORTS the two kinds differently across
//! space, not whether it supports them at the same density. A pair that
//! correlates at 1.0 still admits different absolute capacities, and a campaign
//! claiming two kinds are distinct in *amount* must measure amount separately.

// `terrain_of` and friends are named derivation entry points (decision 0092);
// a probe measuring a handful of worlds is exactly the site the allowance is
// for.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::CellId;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, per_species_suitability, sky_of, terrain_of,
};

/// Pearson's `r` between two equal-length samples.
///
/// Two-pass (means first, then centred sums) rather than the algebraically
/// equivalent `E[xy] - E[x]E[y]` form, which cancels catastrophically when the
/// mean is large relative to the spread — suitability fields are small positive
/// numbers with most of their mass near a common value, which is precisely that
/// case.
///
/// Panics on a constant sample. A field with zero variance has no correlation
/// with anything (`0/0`), and silently returning `NaN` would let a comparison
/// against a threshold pass by accident: `NaN < 0.95` is false, but so is
/// `NaN >= 0.95`. A degenerate field is a finding about the kind, not a number
/// this function should invent.
fn pearson(xs: &[f64], ys: &[f64]) -> f64 {
    assert_eq!(
        xs.len(),
        ys.len(),
        "correlation needs paired samples over the same cells"
    );
    assert!(!xs.is_empty(), "correlation over an empty land mask");
    let n = xs.len() as f64;
    let mean_x = xs.iter().sum::<f64>() / n;
    let mean_y = ys.iter().sum::<f64>() / n;
    let mut sxx = 0.0;
    let mut syy = 0.0;
    let mut sxy = 0.0;
    for (x, y) in xs.iter().zip(ys.iter()) {
        let dx = x - mean_x;
        let dy = y - mean_y;
        sxx += dx * dx;
        syy += dy * dy;
        sxy += dx * dy;
    }
    assert!(
        sxx > 0.0 && syy > 0.0,
        "a suitability field that is CONSTANT over land has no correlation \
         with anything; variances were {sxx:.6e} and {syy:.6e}"
    );
    // `sqrt(sxx * syy)` rather than `sqrt(sxx) * sqrt(syy)`: for a kind against
    // itself the numerator is bit-identically `sxx`, and this denominator
    // returns it to within one ulp — which is what the identity control asserts.
    sxy / (sxx * syy).sqrt()
}

/// Pearson correlation of each requested pair of kinds' per-cell suitability,
/// over the land cells of the world at `seed`, ascending by pair.
///
/// `kinds` is a roster, not a set: every unordered pair of *positions* is
/// reported, so `["goblin", "goblin"]` yields the one self-pair rather than
/// being deduplicated away — that pair is the identity control. Each returned
/// key is the two names in ascending order.
///
/// Land is `!terrain.is_ocean(cell)`, never `elevation < 0`: a world's sea
/// level is not zero (seed 42's sits at −2,936 m).
///
/// **The species slices are built exactly as the live path builds them**
/// (`windows/worldgen/src/lib.rs`, `demography_report_with_beta_from`): the
/// WHOLE `wc.biosphere` in ascending-`KindId` order, with `species_realm`
/// derived from the SAME iteration so the two stay index-aligned. A realm slice
/// that drifted out of alignment would score a subterranean kind against the
/// surface substrate and every number here would be quietly wrong, with nothing
/// to notice it. `per_species_suitability` scores each kind independently, so
/// passing the whole roster costs a little time and changes no value.
///
/// The `u32` in the returned `Vec<(u32, CellMap<f64>)>` is a **build-local
/// dense index, not a stable species id** — it is a position in the
/// `species_biosphere` slice, which is why the kind order is captured from that
/// same iteration and used to map a name back to a column.
fn pairwise_correlations(seed: u64, kinds: &[&str]) -> Vec<((String, String), f64)> {
    let wc = WorldComponents::assemble().expect("components assemble");
    let world = build_world(
        hornvale_kernel::Seed(seed),
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
    // The stellar-input triple, resolved exactly as `delver_bind_audit.rs` and
    // `niche_breadth_probe.rs` do (`stellar_inputs` is private to worldgen).
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

    // The three parallel slices, from one `wc.biosphere` iteration each, in the
    // store's ascending-`KindId` order — the order the returned `u32` indexes.
    let roster: Vec<&'static str> = wc.biosphere.iter().map(|(kind, _)| kind.0).collect();
    let species_biosphere: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, bio)| bio).collect();
    let species_realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();

    let per_species = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &species_biosphere,
        &species_realm,
    );

    let land: Vec<CellId> = geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();

    // A kind's suitability over land, found by mapping its NAME through the
    // roster order above to the build-local index, then through the returned
    // tag. Looked up by tag rather than by vector position so a future change
    // to the return's ordering cannot silently pair the wrong two kinds.
    let column = |name: &str| -> Vec<f64> {
        let idx = roster
            .iter()
            .position(|k| *k == name)
            .unwrap_or_else(|| panic!("{name:?} has no biosphere row; roster is {roster:?}"));
        let (_, k) = per_species
            .iter()
            .find(|(tag, _)| *tag as usize == idx)
            .unwrap_or_else(|| panic!("no suitability field returned for {name:?} at index {idx}"));
        land.iter().map(|&c| *k.get(c)).collect()
    };

    println!("== seed {seed} ==  land cells: {}", land.len());
    let mut out: Vec<((String, String), f64)> = Vec::new();
    for (i, first) in kinds.iter().enumerate() {
        for second in &kinds[i + 1..] {
            let (a, b) = if first <= second {
                (*first, *second)
            } else {
                (*second, *first)
            };
            let r = pearson(&column(a), &column(b));
            println!("{a:<16} vs {b:<16} r = {r:.6}");
            out.push(((a.to_string(), b.to_string()), r));
        }
    }
    // Ascending by pair — a name ordering, so no float comparison is involved
    // and `total_cmp` has nothing to adjudicate here.
    out.sort_by(|x, y| x.0.cmp(&y.0));
    out
}

/// **The instrument must be shown able to report DIFFERENCE.** A distinctness
/// probe whose only assertions are "these two are identical" cannot be
/// distinguished from one that computes nothing. Kobold is a deliberate
/// highland specialist and gnoll is documented for desert; if this probe
/// cannot separate them it is broken, and every null it later reports is
/// worthless.
///
/// Measured 2026-08-07 on seed 42's pre-dwarf roster: `r = 0.291124` over
/// 11,066 land cells. The 0.95 threshold is a discrimination floor, not a
/// tuned one — the measured value clears it by a wide margin, and if a future
/// change pushes the pair above it, the correct response is to report that the
/// roster has gone degenerate, never to relax the number.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_probe_separates_two_kinds_known_to_differ() {
    let pairs = pairwise_correlations(42, &["kobold", "gnoll"]);
    let (_, r) = &pairs[0];
    assert!(
        *r < 0.95,
        "kobold and gnoll must correlate below 0.95 or this probe cannot \
         discriminate; got {r:.6}"
    );
}

/// And able to report IDENTITY, against a pair that is identical by
/// construction: a kind compared with itself. This is the other half of the
/// discrimination check — a probe that always reports "different" would fail
/// here, and the pair enumeration must NOT deduplicate a repeated name away.
///
/// Measured 2026-08-07: `r = 1.000000` for goblin against goblin on seed 42.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_probe_reports_unity_for_a_kind_against_itself() {
    let pairs = pairwise_correlations(42, &["goblin", "goblin"]);
    assert_eq!(
        pairs.len(),
        1,
        "a kind against itself is ONE pair; deduplicating it away would remove \
         the identity control"
    );
    let (_, r) = &pairs[0];
    assert!((r - 1.0).abs() < 1e-12, "expected exactly 1.0, got {r:.12}");
}
