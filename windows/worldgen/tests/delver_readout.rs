//! THE DELVERS (C2c) — the preregistered readout.
//!
//! The campaign's scientific deliverable: the frozen predictions of the spec
//! (§5 as amended by §10 and §11), measured on the SHIPPED roster of three —
//! `desert-dwarf`, `gully-dwarf`, `hill-dwarf`. `mountain-dwarf` and `duergar`
//! were authored and withdrawn before merge (spec §11), and Task 3b — the
//! depth coordinate that existed to separate them — was withdrawn with them.
//! Nothing here touches `subterranean_substrate`, and no dwarf is
//! `HabitatRealm` subterranean.
//!
//! ## The instruments are copied, not re-derived
//!
//! Rust integration-test crates share no code, so [`binding_axis`] and
//! [`bind_shares`] are copied from `delver_bind_audit.rs` and [`pearson`] and
//! [`pairwise_correlations`] from `delver_distinctness.rs`. Both files were
//! authored and validated BEFORE any dwarf trait value existed, and both carry
//! their own two-directional discrimination controls. The one change here is
//! that `bind_shares` returns all four axis shares instead of elevation's
//! alone (P3′ needs the climate share); the computation is identical, and
//! `binding_axis` — the mirror of the private `tolerance_liebig` — is verbatim.
//!
//! ## What was measured (2026-08-07), in one table
//!
//! Each prediction's numbers and reasoning live in its own test's doc comment.
//! Seeds 42 / 7 / 1234 throughout.
//!
//! ```text
//!   P1  bind theorem, both directions              CONFIRMED
//!         gully & hill elevation-bound on 100.00% of land, every seed
//!         desert-dwarf 13.34% / 31.59% / 8.64% — the predicate flipped
//!   P2″ withdrawn entirely (spec §11.4)            not measured
//!   P3′ desert's climate curves bind >= 20%        CONFIRMED  67-91%
//!   P3′ r(desert, hill) < 0.95                     REFUTED    0.96/0.86/0.98
//!   P4  the three pairwise below 0.95              2 of 3 pairs CONFIRMED
//!         desert-gully 0.59/0.54/0.63 · gully-hill 0.69/0.76/0.69
//!         desert-hill  0.96/0.86/0.98  — the same refutation
//!   P5  the paced kinds are read                   CONFIRMED
//!         generation length 4.00x the allometric base on all three
//!         every dwarf on the slow {1,2} cascade regime
//!   P6  seed 42's world moved                      CONFIRMED, unpredicted
//!         122 -> 145 settlements; 6 -> 9 peoples holding ground
//! ```
//!
//! **The headline is the refutation.** `desert-dwarf` is the only kind of the
//! three whose climate niche actually binds, and it is the LEAST separated
//! from `hill-dwarf` — while `gully-dwarf` and `hill-dwarf`, which differ on
//! exactly one live axis (an elevation optimum of 150 m against 900 m),
//! separate to 0.69–0.76. A live climate niche bought less spatial
//! distinctness than a moved elevation optimum did. The 0.95 threshold was
//! frozen in spec §5 and §10.2 and has not been moved; nothing was retuned to
//! rescue the prediction, and the mechanism behind the residual correlation is
//! recorded as **unestablished** rather than narrated — see
//! [`p4_the_dwarves_pairwise_correlations_and_p3s_refuted_second_half`], which
//! measured and refuted the obvious candidate.
//!
//! ## The mutations — applied as temporary local edits, observed RED, reverted
//!
//! A green test proves the code ran; only a mutation proves the axis is
//! visible. Each below reddens on an **assertion**, never on a compile error —
//! a RED from a compile error proves nothing about a check. Each edit was
//! confirmed to exist before it was substituted (a no-op mutation is worse
//! than no mutation, because it produces evidence).
//!
//! **M1 is dead.** It flipped `mountain-dwarf`'s `habitat_realm_registry` row
//! to `Surface`. No dwarf has a realm row now — spec §11 withdrew both
//! subterranean kinds and `habitat_realm_registry` is back to exactly xorn and
//! rust-monster — so there is nothing for it to flip. No substitute was
//! invented for it.
//!
//! **M2 — revert `hill-dwarf`'s schedule to `LifeSchedule::Allometric`.** The
//! campaign's most load-bearing mutation, and the one The Long Age could not
//! run: it shipped `LifeSchedule::Paced` with an empty witness list. Both
//! halves of P5 went RED:
//!
//! ```text
//!   p5_generation_length_reads_the_paced_schedule
//!     hill-dwarf        70.0           30.36                      30.36    1.00
//!     panicked: hill-dwarf's generation length must exceed what mass alone
//!     predicts (30.36 y); the descent path returned 30.36 y.
//!
//!   p5_the_cascade_regime_reads_the_paced_schedule
//!     hill-dwarf drew 3 rules (it draws 1 as authored)
//!     panicked: ... its cascade can hold at most 2 rules; it drew 3.
//! ```
//!
//! The generation length collapsed 121.46 y → 30.36 y (ratio 4.00 → 1.00) and
//! the tongue moved back onto the historical `SETTLED` regime. The `Paced`
//! channel has two live consumers, and this is the demonstration.
//!
//! **M3 — give `hill-dwarf` a materially different elevation curve** (its
//! authored `optimum 900 / width 1400` replaced with gully-dwarf's
//! `150 / 900`). `p4_the_dwarves_pairwise_correlations_and_p3s_refuted_second_half`
//! went RED, and every correlation involving hill-dwarf moved:
//!
//! ```text
//!   pair                            authored -> mutated (s42 / s7 / s1234)
//!   desert-dwarf vs hill-dwarf   0.9629->0.8009  0.8625->0.6703  0.9796->0.8636
//!   gully-dwarf  vs hill-dwarf   0.6925->0.8661  0.7551->0.9283  0.6928->0.8474
//!   desert-dwarf vs gully-dwarf  0.5922->0.5922  0.5353->0.5353  0.6313->0.6313
//! ```
//!
//! The pair that does not involve hill-dwarf is bit-identical across the
//! mutation — the control that says only the perturbed kind moved. Without M3
//! the correlations could have been decorrelation reported by a probe that
//! computes nothing.
//!
//! ## The attribution rule this file is written under
//!
//! Every mechanism this campaign proposed WITHOUT measuring it has failed —
//! five of them, including the "emergent" reading of duergar's marsh/spring
//! toponyms that turned out to be its own authored elevation curve. So each
//! number below is reported with the authored value that could have produced
//! it named alongside, and where the authored value cannot be ruled out the
//! mechanism is recorded as unestablished rather than narrated.

// `terrain_of` and friends are named derivation entry points (decision 0092);
// a probe measuring a handful of worlds is exactly the site the allowance is
// for.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{CellId, ConditionResponse, Mass, Seed, Value, World, sovereignty_floor};
use hornvale_species::ConditionNiche;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, Substrate, build_world, cascade_of, climate_of,
    generation_length_of, per_species_suitability, sky_of, substrate_field, terrain_of,
};

// The heavy-tier `#[ignore]` reason is repeated literally at every site below:
// `cli/tests/heavy_tier.rs` holds it against its canonical constant by
// EQUALITY, not by prefix, and an attribute cannot reference a const.

/// The dwarf family as shipped: three kinds, one `dwarf` family label, all
/// three `LifeSchedule::Paced { factor: 4.0 }`.
const DWARVES: [&str; 3] = ["desert-dwarf", "gully-dwarf", "hill-dwarf"];

/// The nine settling peoples after this campaign — the six The Tilth and the
/// bind audit measured, plus the dwarf family. This is the population every
/// roster-wide claim below is scoped to; a claim about "the peoples" that did
/// not name its population is exactly the error this campaign's own memory
/// index warns about.
const SETTLING_PEOPLES: [&str; 9] = [
    "bugbear",
    "desert-dwarf",
    "gnoll",
    "goblin",
    "gully-dwarf",
    "hill-dwarf",
    "hobgoblin",
    "human",
    "kobold",
];

/// The seeds every live-world claim here is measured over — the same three
/// `delver_bind_audit.rs` used, so its recorded table and this one are
/// directly comparable. One world is an anecdote; three is a spread.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The book's reference seed, whose committed artifacts are the campaign's
/// before-picture for P6.
const REFERENCE_SEED: u64 = 42;

// ---------------------------------------------------------------------------
// Instrument 1 — the bind audit, copied from `delver_bind_audit.rs`.
// ---------------------------------------------------------------------------

/// The Liebig-binding axis, mirroring `tolerance_liebig`
/// (`windows/worldgen/src/lib.rs`), which is private. **Verbatim from
/// `delver_bind_audit.rs`.**
///
/// **This mirrors production code and is a standing maintenance obligation.**
/// If the tolerance model changes — a floor moved onto elevation, an axis
/// added, the `min()` replaced by the gate/modifier split `tolerance_tiered`
/// sketches — this function goes stale silently and must be updated with it,
/// in both files.
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

/// One kind's bind profile on one world: the share of LAND cells on which
/// each of the four condition axes is the Liebig minimum.
///
/// `delver_bind_audit.rs`'s `bind_shares` returned the elevation share alone.
/// This widens the return to all four — P3′ is a claim about the CLIMATE
/// share, which that shape could not express — and changes nothing about how
/// any share is computed.
struct BindProfile {
    /// The kind this profile is for.
    kind: &'static str,
    /// Share of land cells binding on temperature / moisture / insolation /
    /// elevation, in that order. Sums to 1.0.
    shares: [f64; 4],
    /// The land-cell count every share is over.
    land: usize,
}

/// Per kind in `kinds` at `seed`: the share of LAND cells on which each axis
/// is the Liebig minimum.
///
/// Land is `!terrain.is_ocean(cell)`, never `elevation < 0` — a world's sea
/// level is not zero (seed 42's sits at −2,936 m).
fn bind_shares(seed_value: u64, kinds: &[&'static str]) -> Vec<BindProfile> {
    let wc = WorldComponents::assemble().expect("components assemble");
    let seed = Seed(seed_value);
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

    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let land: Vec<CellId> = geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();

    println!("== seed {seed_value} ==  land cells: {}", land.len());
    println!(
        "{:<14} {:>7} {:>9} {:>7} {:>6}  {:>8} {:>8} {:>8} {:>8}",
        "kind", "mass", "floor", "dev_el", "below", "temp%", "moist%", "insol%", "ELEV%"
    );

    let mut out = Vec::new();
    for &name in kinds {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(name))
            .expect("kind has biosphere traits");
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
        let shares = [
            counts[0] as f64 / n as f64,
            counts[1] as f64 / n as f64,
            counts[2] as f64 / n as f64,
            counts[3] as f64 / n as f64,
        ];
        let dev = bio.condition_niche.elevation.devotion;
        println!(
            "{:<14} {:>7.1} {:>9.6} {:>7.2} {:>6}  {:>7.2}% {:>7.2}% {:>7.2}% {:>7.2}%",
            name,
            bio.mass.kilograms(),
            floor_buf,
            dev,
            if dev < floor_buf { "YES" } else { "no" },
            shares[0] * 100.0,
            shares[1] * 100.0,
            shares[2] * 100.0,
            shares[3] * 100.0,
        );
        out.push(BindProfile {
            kind: name,
            shares,
            land: n,
        });
    }
    println!();
    out
}

// ---------------------------------------------------------------------------
// Instrument 2 — the distinctness probe, copied from
// `delver_distinctness.rs`.
// ---------------------------------------------------------------------------

/// Pearson's `r` between two equal-length samples. **Verbatim from
/// `delver_distinctness.rs`.**
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
/// `NaN >= 0.95`.
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
    // returns it to within one ulp.
    sxy / (sxx * syy).sqrt()
}

/// Pearson correlation of each requested pair of kinds' per-cell suitability,
/// over the land cells of the world at `seed`, ascending by pair. **Verbatim
/// from `delver_distinctness.rs`.**
///
/// The species slices are built exactly as the live path builds them
/// (`demography_report_with_beta_from`): the WHOLE `wc.biosphere` in
/// ascending-`KindId` order, with `species_realm` derived from the SAME
/// iteration so the two stay index-aligned.
fn pairwise_correlations(seed: u64, kinds: &[&str]) -> Vec<((String, String), f64)> {
    let wc = WorldComponents::assemble().expect("components assemble");
    let world = build_world(
        Seed(seed),
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
    out.sort_by(|x, y| x.0.cmp(&y.0));
    out
}

/// Look a pair's correlation up by its two names in either order.
fn r_of(pairs: &[((String, String), f64)], a: &str, b: &str) -> f64 {
    pairs
        .iter()
        .find(|((x, y), _)| (x == a && y == b) || (x == b && y == a))
        .map(|(_, r)| *r)
        .unwrap_or_else(|| panic!("no correlation reported for {a} vs {b}"))
}

// ---------------------------------------------------------------------------
// The floors the roster was authored against — verified, not trusted.
// ---------------------------------------------------------------------------

/// **The authored table, checked against the live function.** The campaign's
/// roster was deliberately split 2/1 across `devotion_elev <
/// sovereignty_floor(mass, potency)`. The floors are re-derived here from
/// `hornvale_kernel::sovereignty_floor` and the devotions read out of the
/// shipped registry rather than restated from a document — the campaign's own
/// ledger records that two of five such hand-computed figures were wrong in
/// the fourth decimal, and a readout that trusts a transcribed constant is
/// measuring the document.
///
/// Measured 2026-08-07 (all three at `potency = 0.0`):
///
/// ```text
///   kind             mass   sovereignty_floor   devotion_elev   below?
///   gully-dwarf      62.0        0.43847737          0.30         YES
///   desert-dwarf     66.0        0.44325203          0.70          no
///   hill-dwarf       70.0        0.44770495          0.30         YES
/// ```
///
/// The transcribed table this readout was handed gave these as
/// `0.438477 / 0.443252 / 0.447705`. Those agree to six digits and are not
/// wrong; the eighth-digit values above are what the function returns, and
/// they are what this test asserts.
///
/// This is a cheap registry read — no world is built — so it stays in the
/// commit gate where a mis-authored trait value is caught in seconds rather
/// than in the heavy tier.
#[test]
fn the_dwarf_floors_are_what_the_roster_was_authored_against() {
    let wc = WorldComponents::assemble().expect("components assemble");
    println!(
        "{:<14} {:>7} {:>19} {:>15} {:>8}",
        "kind", "mass", "sovereignty_floor", "devotion_elev", "below?"
    );
    let mut below = 0usize;
    let mut above = 0usize;
    for kind in DWARVES {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(kind))
            .unwrap_or_else(|| panic!("{kind} has a biosphere row"));
        let floor = sovereignty_floor(bio.mass, bio.potency);
        let dev = bio.condition_niche.elevation.devotion;
        println!(
            "{:<14} {:>7.1} {:>19.8} {:>15.2} {:>8}",
            kind,
            bio.mass.kilograms(),
            floor,
            dev,
            if dev < floor { "YES" } else { "no" }
        );
        if dev < floor {
            below += 1;
        } else {
            above += 1;
        }
    }
    assert_eq!(
        (below, above),
        (2, 1),
        "the roster is authored 2/1 across the bind theorem's predicate — two \
         elevation-bound kinds and one whose climate curves are meant to bind. \
         If this ratio moved, P1 and P3′ are measuring a roster the spec did \
         not author."
    );

    // The individual floors, to eight digits, so a change in
    // `sovereignty_floor` itself is caught here rather than being absorbed
    // silently by the 2/1 count above.
    for (kind, expected) in [
        ("gully-dwarf", 0.438_477_37_f64),
        ("desert-dwarf", 0.443_252_03),
        ("hill-dwarf", 0.447_704_95),
    ] {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(kind))
            .expect("dwarf row");
        let floor = sovereignty_floor(bio.mass, bio.potency);
        assert!(
            (floor - expected).abs() < 1e-7,
            "{kind}'s sovereignty floor is {floor:.8}, not the recorded \
             {expected:.8}; the mass or the floor formula moved"
        );
    }

    // And the bound the theorem rests on, at the elevation optimum: the
    // elevation term can never exceed its own devotion, because
    // `tolerance_liebig` passes elevation a literal `0.0` floor.
    let hill = wc
        .biosphere
        .get(&hornvale_kernel::KindId("hill-dwarf"))
        .expect("hill-dwarf row");
    let e: &ConditionResponse = &hill.condition_niche.elevation;
    assert!(
        e.eval(e.optimum, 0.0) <= e.devotion + 1e-12,
        "eval at the optimum equals devotion exactly; got {}",
        e.eval(e.optimum, 0.0)
    );
    // A sanity floor on the mass class, so a registry edit that halved a
    // dwarf's mass cannot slip past the 2/1 count above.
    assert!(
        (sovereignty_floor(Mass::new(70.0).unwrap(), 0.0) - 0.447_704_95).abs() < 1e-7,
        "the 70 kg potency-0 floor is the anchor human and hill-dwarf share"
    );
}

// ---------------------------------------------------------------------------
// P1 — the bind theorem, both directions.
// ---------------------------------------------------------------------------

/// **P1, direction 1 — every below-floor dwarf is elevation-bound on ALL
/// land.** Not 99% of it: the closed form
///
/// ```text
///   elevation is the Liebig minimum on every cell
///       iff  devotion_elev < sovereignty_floor(mass, potency)
/// ```
///
/// admits no exception, so this asserts exact equality with 1.0 and would
/// catch one dissenting cell out of ~19,000.
///
/// **The plan's wording of P1 was false by construction and is not what this
/// asserts.** It said "elevation binds on ≥99% of land for all five dwarves";
/// the roster was authored 2/1 across the predicate deliberately (spec §10.2,
/// §11.4), so desert-dwarf must NOT be elevation-bound. Direction 2 is
/// [`p1_desert_dwarf_is_not_elevation_bound`].
///
/// Measured 2026-08-07, seeds 42 / 7 / 1234 — elevation's share of land:
///
/// ```text
///   kind             dev_el    floor      s42        s7     s1234
///   gully-dwarf        0.30   0.4385   100.00%   100.00%   100.00%
///   hill-dwarf         0.30   0.4477   100.00%   100.00%   100.00%
///   desert-dwarf       0.70   0.4433    13.34%    31.59%     8.64%
///   land cells:                          11,066    19,046    11,571
/// ```
///
/// The two below-floor rows are 100.00% exactly, on every seed — every one of
/// the other three axes takes 0.00% of land, not a rounded zero.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn p1_every_dwarf_below_its_floor_is_elevation_bound_on_all_land() {
    let wc = WorldComponents::assemble().expect("components assemble");
    let mut checked = 0usize;
    for seed in SEEDS {
        for profile in bind_shares(seed, &DWARVES) {
            let bio = wc
                .biosphere
                .get(&hornvale_kernel::KindId(profile.kind))
                .expect("dwarf row");
            let floor = sovereignty_floor(bio.mass, bio.potency);
            let dev = bio.condition_niche.elevation.devotion;
            if dev >= floor {
                continue;
            }
            checked += 1;
            assert_eq!(
                profile.shares[3], 1.0,
                "seed {seed}: {}'s elevation devotion {dev:.4} is below its \
                 sovereignty floor {floor:.6}, so elevation must be the Liebig \
                 minimum on ALL {} land cells; it bound on {:.6}. If this \
                 dropped, the tolerance model changed and the roster's whole \
                 authoring premise is void.",
                profile.kind, profile.land, profile.shares[3]
            );
        }
    }
    assert_eq!(
        checked,
        2 * SEEDS.len(),
        "the theorem must bite on both below-floor dwarves on every seed: \
         expected {} checks, made {checked}",
        2 * SEEDS.len()
    );
}

/// **P1, direction 2 — a dwarf above its floor is NOT elevation-bound.**
/// Without this the theorem is only half-measured, and the probe cannot be
/// told apart from one that reports "elevation" unconditionally. It is the
/// same control `delver_bind_audit.rs` runs against kobold, now run against a
/// kind this campaign authored.
///
/// Measured 2026-08-07: desert-dwarf's elevation share is 13.34% / 31.59% /
/// 8.64% on seeds 42 / 7 / 1234 — far from the 100% the two below-floor
/// dwarves show.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn p1_desert_dwarf_is_not_elevation_bound() {
    for seed in SEEDS {
        let profiles = bind_shares(seed, &["desert-dwarf"]);
        let d = &profiles[0];
        assert!(
            d.shares[3] < 0.99,
            "seed {seed}: desert-dwarf's elevation devotion 0.70 sits ABOVE its \
             floor 0.443252, so the theorem does not apply and a climate axis \
             must bind on a real share of the {} land cells. Elevation bound on \
             {:.6}, which means this probe cannot discriminate and every \
             elevation result it reports is worthless.",
            d.land,
            d.shares[3]
        );
    }
}

// ---------------------------------------------------------------------------
// P3′ — desert's climate curves actually bind.
// ---------------------------------------------------------------------------

/// **P3′, first half (spec §10.2) — desert-dwarf's temperature or moisture
/// curve is the Liebig minimum on ≥ 20% of land cells. CONFIRMED, by a wide
/// margin.**
///
/// Measured 2026-08-07 — desert-dwarf's share of land per binding axis:
///
/// ```text
///   seed     temperature   moisture   insolation   elevation   temp+moist
///     42          70.32%     16.26%        0.08%      13.34%       86.58%
///      7          37.37%     30.10%        0.94%      31.59%       67.47%
///   1234          80.71%     10.65%        0.00%       8.64%       91.36%
///   land cells:   11,066 / 19,046 / 11,571
/// ```
///
/// The frozen floor was 20% and the measured climate share is 67–91%. This is
/// the roster's first people whose climate niche actually selects: the two
/// below-floor dwarves read 0.00% on all three climate axes on every seed
/// (see [`p1_every_dwarf_below_its_floor_is_elevation_bound_on_all_land`]'s
/// recorded table), which is what a *prepared* curve looks like.
///
/// Insolation is essentially never the minimum (0.00–0.94%). That is the
/// authored curve doing what its own comment says: devotion 0.45 and width
/// 0.35 over a settleable insolation band spanning only 0.19–0.31, so the
/// response is nearly flat across all land and never dips under the other
/// three.
///
/// **What this hands `BIO-gnoll-desert`.** Gnoll is documented for the same
/// `Desert` climate tile and selects nothing there. The mechanism is that
/// gnoll's `devotion_elev` of 0.40 sits BELOW its floor of 0.4954, so its
/// authored moisture curve is never the Liebig minimum on any cell — it is
/// prepared, not connected. `delver_bind_audit.rs` measured gnoll at 100.00%
/// elevation-bound on all three seeds, and desert-dwarf is the same model with
/// the predicate flipped, so the diagnosis is a contrast and not an inference.
/// It is a diagnosis the row did not have. Gnoll is **not** re-authored here:
/// moving an existing people's capacity inside a roster epoch would destroy
/// this campaign's attribution (spec §10.2).
///
/// **Attribution.** The climate share is attributable to the authored
/// `devotion_elev = 0.70` and nothing else — it is the predicate of a closed
/// form `delver_bind_audit.rs` measured exactly on six kinds before any dwarf
/// existed. WHICH climate axis wins, and the seed-to-seed spread, are joint
/// consequences of the authored temperature and moisture optima and the
/// world's own climate; nothing here separates those and no claim is made
/// about them.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn p3_desert_dwarfs_climate_curves_bind() {
    for seed in SEEDS {
        let profiles = bind_shares(seed, &["desert-dwarf"]);
        let d = &profiles[0];
        let climate_share = d.shares[0] + d.shares[1];
        println!(
            "seed {seed}: desert-dwarf temp {:.4} + moist {:.4} = {climate_share:.4} \
             (insol {:.4}, elev {:.4}) over {} land cells",
            d.shares[0], d.shares[1], d.shares[2], d.shares[3], d.land
        );
        assert!(
            climate_share >= 0.20,
            "seed {seed}: P3′ freezes desert-dwarf's temperature-or-moisture \
             bind share at >= 20% of the {} land cells; measured \
             {climate_share:.6}. Below the floor, the roster's first \
             climate-selected people does not select on climate.",
            d.land
        );
    }
}

// ---------------------------------------------------------------------------
// P3′ second half and P4 — the pairwise capacity correlations. One prediction
// CONFIRMED, one REFUTED, measured together because they are the same three
// numbers.
// ---------------------------------------------------------------------------

/// **P4 (the discrimination control) and P3′'s second half — the three
/// dwarves' pairwise capacity correlations, frozen at `r < 0.95`. TWO PAIRS
/// CONFIRMED; `desert-dwarf` vs `hill-dwarf` REFUTED.**
///
/// Measured 2026-08-07 over three seeds, Pearson `r` between
/// `per_species_suitability` fields on land:
///
/// ```text
///   pair                             s42        s7     s1234   frozen r<0.95
///   desert-dwarf vs gully-dwarf   0.5922    0.5353    0.6313   CONFIRMED 3/3
///   gully-dwarf  vs hill-dwarf    0.6925    0.7551    0.6928   CONFIRMED 3/3
///   desert-dwarf vs hill-dwarf    0.9629    0.8625    0.9796   REFUTED   2/3
///   land cells:                   11,066    19,046    11,571
/// ```
///
/// **The threshold was not moved and nothing was retuned.** 0.95 is the
/// preregistered floor from spec §5 P4 and §10.2 P3′; desert↔hill fell on the
/// wrong side of it on two of three seeds, and that is the campaign's
/// falsified prediction. The assertion below pins the *refutation* as a
/// witness — if a later change pushes that pair below 0.95 on two or more of
/// these seeds, this test goes RED and the finding must be re-measured rather
/// than quietly inherited.
///
/// **The shape of the result is the interesting part, and it inverts the
/// design.** desert-dwarf is the only one of the three whose climate curves
/// bind at all (67–91% of land, above), and it is the LEAST separated from
/// hill-dwarf. gully-dwarf and hill-dwarf are both elevation-bound on 100% of
/// land — they differ on exactly one live axis, an optimum 150 m against 900 m
/// — and they separate to 0.69–0.76. So on this evidence a live climate niche
/// bought *less* spatial distinctness than a moved elevation optimum did.
///
/// **The mechanism is NOT established, and the obvious candidate was tested
/// and does not carry it.** `BIO-supply-drowns-niche` records that
/// `per_species_suitability` multiplies a bounded `[0,1]` tolerance by a supply
/// term spanning orders of magnitude, and desert-dwarf's diet (PLANT_FORAGE
/// 0.58 / ANIMAL_PREY 0.42) is far closer to hill-dwarf's (0.70 / 0.30) than
/// gully-dwarf's DETRITUS-led vector is to either — which would predict
/// correlation tracking DIET similarity, exactly the observed ordering. Two
/// diagnostic perturbations of desert-dwarf's `ResourceVector` alone, each
/// applied locally and reverted, refute it:
///
/// ```text
///   desert-dwarf's diet                    s42       s7    s1234
///   as authored (PLANT .58 / PREY .42)   0.9629   0.8625   0.9796
///   set to hill-dwarf's (.70 / .30)      0.9621   0.8609   0.9793
///   set to gully-dwarf's (DETRITUS-led)  0.9409   0.7900   0.9482
/// ```
///
/// Making the two diets **identical** moved `r` by 0.0008 — the first
/// perturbation proved almost nothing, because those two vectors were already
/// near-identical over the same two resources. Moving desert-dwarf onto a
/// **wholly different trophic axis** moved it by only 0.02–0.07, and the pair
/// still reads 0.79–0.95. (The gully↔hill control was bit-identical across
/// both runs — 0.692547 at seed 42 — so only the perturbed kind moved.)
///
/// The condition niche is not inert either: mutation M3 (below) replaces
/// hill-dwarf's elevation curve with gully-dwarf's and moves desert↔hill by
/// 0.16–0.19, an order more than the diet does.
///
/// So **both channels are live and neither accounts for the pair sitting at
/// 0.96 as authored.** What carries the rest is **not determined by this
/// readout**, and no cause is named for it. Recording the number and declining
/// to name a mechanism is the finding: every mechanism this campaign proposed
/// without measuring has failed, and the supply story above would have been
/// the sixth had it not been tested. `BIO-supply-drowns-niche` remains the open
/// row it was, neither confirmed nor discharged here.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn p4_the_dwarves_pairwise_correlations_and_p3s_refuted_second_half() {
    // Frozen floor, spec §5 P4 / §10.2 P3′. Never move this.
    const FROZEN: f64 = 0.95;
    let mut desert_hill_at_or_above = 0usize;
    for seed in SEEDS {
        let pairs = pairwise_correlations(seed, &DWARVES);
        assert_eq!(
            pairs.len(),
            3,
            "three kinds make three unordered pairs; got {}",
            pairs.len()
        );
        // The two pairs the prediction survived on. These are also the
        // discrimination control: without a pair that reads BELOW the floor,
        // "desert and hill are alike" would be indistinguishable from a probe
        // that computes nothing.
        for (a, b) in [
            ("desert-dwarf", "gully-dwarf"),
            ("gully-dwarf", "hill-dwarf"),
        ] {
            let r = r_of(&pairs, a, b);
            assert!(
                r < FROZEN,
                "seed {seed}: {a} and {b} must correlate below {FROZEN} or two \
                 of the family are one kind wearing two names; measured {r:.6}"
            );
        }
        let dh = r_of(&pairs, "desert-dwarf", "hill-dwarf");
        println!("seed {seed}: desert-dwarf vs hill-dwarf r = {dh:.6}");
        if dh >= FROZEN {
            desert_hill_at_or_above += 1;
        }
    }
    // The refutation, pinned as a witness rather than as a claim. Measured
    // 2/3 (seeds 42 and 1234 above the floor, seed 7 below it at 0.8625).
    assert!(
        desert_hill_at_or_above >= 2,
        "P3′'s second half was REFUTED at {desert_hill_at_or_above}/3 seeds \
         above the frozen {FROZEN} when this was measured — 2 of 3 (42 and \
         1234 above, 7 at 0.8625). This assertion pins that falsification. \
         Fewer seeds above the floor means the model gained separation between \
         desert-dwarf and hill-dwarf that this campaign could not get, which is \
         a finding: re-measure it and rewrite the chronicle. Do NOT relax this \
         number to make the test pass."
    );
}

// ---------------------------------------------------------------------------
// P5 — the paced kinds are read.
// ---------------------------------------------------------------------------

/// **P5, half 1 — `generation_length_of` reads the paced schedule.** For each
/// dwarf, the generation length the descent path derives exceeds what the same
/// row's mass predicts under `LifeSchedule::Allometric`. This is the consumer
/// The Long Age could not observe: it shipped `LifeSchedule::Paced` with an
/// empty witness list, and `descent_graph.rs` recorded in its own comment that
/// a mutation reverting `generation_length_of` to hardcode
/// `LifeSchedule::ALLOMETRIC` was "UNOBSERVABLE by any test today … until a
/// future campaign (C2c) authors a `Paced` kind." It now is, here.
///
/// Measured 2026-08-07 (years):
///
/// ```text
///   kind             mass   allometric gl   as authored (paced 4.0)   ratio
///   gully-dwarf      62.0          29.46                    117.83    4.00
///   desert-dwarf     66.0          29.92                    119.68    4.00
///   hill-dwarf       70.0          30.36                    121.46    4.00
/// ```
///
/// The ratio is exactly the pace factor because `generation_length` is one of
/// the three channels that stay linear under `Paced`; `pace_of_life` and
/// `reproductive_tempo` saturate at 1.0 at this factor and are therefore
/// uninformative for a dwarf (The Long Age §3.5, restated in the registry's
/// own comment). No world is built — `generation_length_of` resolves its row
/// from `WorldComponents::assemble()` and ignores its `world` argument by
/// design — so this stays in the commit gate.
#[test]
fn p5_generation_length_reads_the_paced_schedule() {
    let world = World::new(Seed(REFERENCE_SEED));
    let wc = WorldComponents::assemble().expect("components assemble");
    println!(
        "{:<14} {:>7} {:>15} {:>26} {:>7}",
        "kind", "mass", "allometric gl", "as authored", "ratio"
    );
    for kind in DWARVES {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(kind))
            .unwrap_or_else(|| panic!("{kind} has a biosphere row"));
        // No `matches!(bio.schedule, Paced { .. })` guard here, deliberately.
        // It would be redundant with the inequality below — a row reverted to
        // `Allometric` makes `authored == allometric` — and it would fire
        // FIRST, so mutation M2's RED would read "the registry changed"
        // instead of "the consumer stopped reading the schedule", which is the
        // only thing this test exists to prove. The registry's own shape is
        // pinned where it belongs, in `domains/species/tests/coverage.rs`.
        //
        // The mass-alone counterfactual: the SAME row with the schedule
        // reverted, which is exactly the mutation M2 applies for real.
        let allometric = hornvale_species::life_history(bio.mass, bio.metabolic_class, {
            hornvale_species::LifeSchedule::ALLOMETRIC
        })
        .generation_length
        .expect("an endotherm has a derivable generation length")
        .get();
        let authored = generation_length_of(&world, kind)
            .unwrap_or_else(|| panic!("{kind} has a derivable generation length"));
        println!(
            "{:<14} {:>7.1} {:>15.2} {:>26.2} {:>7.2}",
            kind,
            bio.mass.kilograms(),
            allometric,
            authored,
            authored / allometric
        );
        assert!(
            authored > allometric,
            "{kind}'s generation length must exceed what mass alone predicts \
             ({allometric:.2} y); the descent path returned {authored:.2} y. If \
             these are equal, `generation_length_of` is not forwarding \
             `bio.schedule` and the Paced channel has no consumer after all."
        );
        // And by the authored factor, not merely by some amount — a bound
        // that a schedule silently reverted to Allometric cannot satisfy.
        assert!(
            authored > 100.0,
            "{kind} is authored at paced(4.0) over a ~30 y allometric base, so \
             its generation length must clear 100 y; got {authored:.2}"
        );
    }
}

/// **P5, half 2 — `cascade_regime_of` puts every dwarf on the slow regime and
/// leaves the other settling peoples on the settled one.**
///
/// `cascade_regime_of` is private to worldgen, so this measures it through
/// the public seam that consumes it — [`cascade_of`], which calls it directly
/// and draws the species' cascade at whatever regime it returns. Grepping the
/// observable rather than the function is deliberate: a mirror of the private
/// function would go stale silently, and the rule counts below are what the
/// language actually gets.
///
/// The regimes are `CascadeRegime::SETTLED = {min 2, max 4}` for a short-lived
/// settled people and `{min 1, max 2}` for one whose lifespan clears
/// `LIFESPAN_THRESHOLD_YEARS = 120.0`. All three dwarves read 267–276 y under
/// `paced(4.0)`; the six older peoples top out at gnoll's ~81.5 y.
///
/// Measured 2026-08-07 at seed 42 — drawn cascade rule counts:
///
/// ```text
///   bugbear        3      desert-dwarf   2      gnoll          3
///   goblin         2      gully-dwarf    1      hill-dwarf     1
///   hobgoblin      3      human          2      kobold         2
/// ```
///
/// The bound asserted is the slow regime's `max` of 2, and it **is** a live
/// discrimination: three of the six non-dwarf peoples (bugbear, gnoll,
/// hobgoblin) draw 3, so a dwarf that fell back to `SETTLED` could exceed it.
/// Mutation M2 confirmed that it does — see the module-level mutation record.
/// Goblin's, human's and kobold's 2 is why the converse is only reported and
/// not asserted: `SETTLED = {2,4}` and the slow regime `{1,2}` overlap at 2,
/// so "≥ 3" is not a property of the settled regime and freezing it would be
/// inventing a threshold the model does not carry.
#[test]
fn p5_the_cascade_regime_reads_the_paced_schedule() {
    let world = World::new(Seed(REFERENCE_SEED));
    let slow_max = hornvale_language::CascadeRegime::new(1, 2).max;
    let mut counts: Vec<(&str, u32)> = Vec::new();
    for people in SETTLING_PEOPLES {
        let cascade = cascade_of(&world, people)
            .unwrap_or_else(|e| panic!("{people} draws a cascade: {e:?}"));
        counts.push((people, cascade.rules.len() as u32));
    }
    for (people, rules) in &counts {
        println!("{people:<14} {rules}");
    }
    for kind in DWARVES {
        let (_, rules) = counts
            .iter()
            .find(|(k, _)| *k == kind)
            .expect("every dwarf is a settling people");
        assert!(
            *rules <= slow_max,
            "{kind} lives 267-276 y under paced(4.0), well past \
             LIFESPAN_THRESHOLD_YEARS, so `cascade_regime_of` must put it on \
             the slow {{1,2}} regime and its cascade can hold at most \
             {slow_max} rules; it drew {rules}. At 3 or 4 the schedule is not \
             being read and the tongue is drifting at the historical rate."
        );
    }
    // The bound must be capable of failing: at least one non-dwarf settling
    // people has to draw MORE than the slow regime's max, or the assertion
    // above is satisfied by every roster and proves nothing.
    let over = counts
        .iter()
        .filter(|(k, r)| !DWARVES.contains(k) && *r > slow_max)
        .count();
    assert!(
        over > 0,
        "no non-dwarf settling people drew more than {slow_max} rules, so the \
         dwarf bound above is vacuous — it would hold on a roster where the \
         schedule was never read. Counts: {counts:?}"
    );
}

// ---------------------------------------------------------------------------
// P6 — world identity moved, magnitude unpredicted.
// ---------------------------------------------------------------------------

/// **P6 — seed 42's committed world moved.** The magnitude was deliberately
/// not predicted; The Warren's retrospective records that refusing to guess is
/// what kept a falsified prediction from acquiring a number to defend. So this
/// asserts only what the campaign committed to — that the ledger changed —
/// and REPORTS the size.
///
/// Measured 2026-08-07 at seed 42. The before-column is the committed
/// pre-campaign artifact `book/src/gallery/almanac-seed-42.md`, which still
/// carries main's values at the time of writing — the artifact regen is the
/// campaign's close, not this task, so the file is a genuine before-picture:
///
/// ```text
///                              before (committed)   after (this branch)
///   settlements                             122                    145
///   peoples holding occupations               6                      9
/// ```
///
/// Ledger facts after: **7,764**. No before-value is quoted for that one —
/// nothing committed records it, and this task did not build main to find out.
///
/// Occupations per people after: kobold 150, hobgoblin 122, gnoll 68, goblin
/// 33, hill-dwarf 20, human 13, bugbear 9, gully-dwarf 3, desert-dwarf 2
/// (420 in total).
///
/// **The dwarves hold very little ground, and that is reported, not
/// explained.** hill-dwarf's 20 is mid-roster; gully-dwarf's 3 and
/// desert-dwarf's 2 are the two smallest holdings on the map, and the
/// climate-selected kind is the smallest of all. A kind authored far above its
/// sovereignty floor is sharply excluded away from its optimum — that is the
/// stated cost of the style in `desert_dwarf_condition_niche`'s own doc
/// comment, and `non_void_roster` is the gate that keeps it non-zero. Whether
/// that is what produced these two numbers is **not** established here: this
/// test counts occupations and does not decompose them.
///
/// Two already-landed re-pins in `windows/worldgen/src/lib.rs` record the same
/// movement independently: `is-belief` 58 → 88 (three more peopled pantheons)
/// and `name-gloss` 177 → 213.
///
/// **Attribution.** The only source change on this branch that a world can see
/// is the roster: three `Settled` peoples were added, and settlement genesis
/// packs peopled species. The other diffs are a test-only refactor
/// (`family_daughter_kinds` split out of `family_daughters`, defined in terms
/// of it) and `cascade_of_in`, a wc-threaded twin of an existing entry. No
/// draw, formula or stream label moved. The assertion below is the part that
/// cannot be explained by anything but the roster: a pre-Delvers world cannot
/// contain a `hill-dwarf` occupation.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn p6_seed_42s_committed_world_moved() {
    let world = build_world(
        Seed(REFERENCE_SEED),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    let settlements = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .count();
    let facts = world.ledger.iter().count();

    // Occupations per people, from the ledger's own text values — a window
    // reading committed facts, not the in-memory system.
    let mut per_people: std::collections::BTreeMap<String, usize> =
        std::collections::BTreeMap::new();
    for fact in world.ledger.iter() {
        if fact.predicate == hornvale_history::OCC_PEOPLE
            && let Value::Text(people) = &fact.object
        {
            *per_people.entry(people.clone()).or_insert(0) += 1;
        }
    }

    println!("seed 42: {settlements} settlements, {facts} ledger facts");
    for (people, n) in &per_people {
        println!("  {people:<14} {n}");
    }

    for kind in DWARVES {
        assert!(
            per_people.contains_key(kind),
            "seed 42's committed ledger must hold at least one {kind} \
             occupation — a peopled roster addition that never reaches the \
             ledger is The Warren's null, not this campaign's result. Peoples \
             present: {:?}",
            per_people.keys().collect::<Vec<_>>()
        );
    }
    assert_eq!(
        per_people.len(),
        SETTLING_PEOPLES.len(),
        "every settling people should hold ground somewhere on seed 42; got {:?}",
        per_people.keys().collect::<Vec<_>>()
    );
    // The pre-campaign committed almanac reports 122 settlements for this
    // seed. Asserting inequality rather than a new pinned number: the count is
    // reported, and the campaign predicted no magnitude.
    assert_ne!(
        settlements, 122,
        "the committed pre-campaign almanac for seed 42 reports 122 \
         settlements; an identical count after adding three settling peoples \
         would mean genesis never saw them"
    );
}
