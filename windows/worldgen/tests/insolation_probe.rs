//! Stage-0 Terminator probe (spec §6, preregistered): applies the corrected
//! Locked insolation field SYNTHETICALLY on locked seeds and reports where
//! dominant-species carrying capacity peaks — substellar (the bug's
//! outcome) or terminator (the fix's goal) — plus the presiding-sentiment
//! inference the zone licenses. Read-only against the generator: nothing
//! here wires into `generate`, no world bytes move, and the OLD (buggy)
//! `hornvale_worldgen::annual_mean_insolation` is never edited. Run by hand:
//! `cargo test -p hornvale-worldgen --release --test insolation_probe --
//! --ignored --nocapture`.
//!
//! ## The corrected formula (spec §3)
//!
//! For a Locked world, per cell: `insolation = insolation_scalar *
//! substellar_cosine(position).max(0.0)` — Lambert cosine law, night side
//! floored at 0. This mirrors the branch `hornvale_climate::temperature`
//! and `hornvale_climate::moisture` already take on `RotationRegime::
//! Locked` (Task 1's shared `substellar_cosine` helper); the CURRENT bug is
//! that `hornvale_worldgen::substrate_field`'s insolation term calls
//! `annual_mean_insolation` (latitude-only) unconditionally, never branching
//! on rotation regime.
//!
//! ## Method
//!
//! 1. Scan seeds for the `Locked` rotation regime (a minority draw) at the
//!    cheapest depth that exposes it (`BuildDepth::Astronomy`).
//! 2. For each locked seed, build to `BuildDepth::Terrain` (the shallowest
//!    rung carrying the terrain/climate this probe reads) and assemble the
//!    REAL `Substrate` field via the shipped `substrate_field`, then swap
//!    only its `insolation` term for the corrected Locked formula — every
//!    other axis (temperature, moisture, elevation) is untouched and
//!    already regime-aware upstream.
//! 3. Run the SAME per-species suitability product `niche_per_species_k`
//!    computes (replicated here as `niche_k_over`, since that function
//!    builds its own internal `Substrate` and cannot take an injected one)
//!    over the corrected substrate, for the roster's PEOPLED species only
//!    (the four settling goblinoids — settlement genesis itself filters to
//!    `peopled.is_some()`, since only settling species found cultures and
//!    thus presiding religions; fauna in the wider menagerie never do).
//! 4. Find the world-dominant peopled species (highest total K over
//!    habitable cells) and the cell where ITS K is maximal; classify that
//!    cell by `substellar_cosine`.
//!
//! ## The sentiment proxy (accepted, stated openly)
//!
//! Running the full culture/religion derivation under a synthetically
//! patched substrate is invasive for a read-only probe (it would require
//! re-deriving settlement placement, culture, and religion facts against a
//! field the committed ledger never actually saw). The accepted proxy: a
//! terminator-ring K-peak is the mechanism that yields a tide-Ambient
//! presiding sentiment (a terminator culture's most salient phenomenon is
//! the tide, not a fixed sun); a substellar K-peak instead reproduces the
//! bug's Eternal-noon outcome. This page reports the K-peak zone
//! distribution as the honest, cheaper stand-in — stated here per the
//! brief's fallback clause, not silently substituted.

use hornvale_astronomy::{Rotation, SkyPins};
use hornvale_climate::substellar_cosine;
use hornvale_kernel::{CellMap, Geosphere, Seed};
use hornvale_species::SpeciesDef;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, Substrate, build_world_to, carrying_inputs_of,
    climate_of, default_roster, sky_of, substrate_field, terrain_of,
};

/// First scan window: seeds `1..=200` (brief step 1). Widened by
/// [`locked_seeds`] if this yields fewer than [`MIN_LOCKED`].
const FIRST_SCAN_MAX: u64 = 200;

/// Widened scan ceiling, used only if the first window under-yields.
const WIDE_SCAN_MAX: u64 = 1000;

/// Minimum locked-seed count this probe wants before it stops widening the
/// scan (brief: "if too few, widen N").
const MIN_LOCKED: usize = 8;

/// Terminator-ring half-width on `substellar_cosine` for the probe's
/// headline statistic: `|cos_theta| <= TERMINATOR_HEADLINE_HALF_WIDTH` at
/// the dominant species' K-argmax cell (brief point 4: "`|cos| <= ~0.3`").
const TERMINATOR_HEADLINE_HALF_WIDTH: f64 = 0.3;

/// Is this seed's rotation regime `Locked`? Built at the cheapest depth
/// that exposes the sky (`BuildDepth::Astronomy` — no terrain, no
/// settlements). Returns `false` for any seed whose generated-sky genesis
/// is refused (never happens for the unpinned default pins in practice,
/// but a refusal is not "locked" either way).
fn is_locked(seed: u64) -> bool {
    let Ok(world) = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &[],
        BuildDepth::Astronomy,
    ) else {
        return false;
    };
    let Ok(sky) = sky_of(&world) else {
        return false;
    };
    let Some(system) = sky.system() else {
        return false;
    };
    matches!(system.anchor.rotation, Rotation::Locked)
}

/// Scan ascending seeds for the `Locked` regime, widening the window once
/// if the first pass under-yields (brief step 1). Returns seeds in
/// ascending order.
fn locked_seeds() -> Vec<u64> {
    let mut found: Vec<u64> = (1..=FIRST_SCAN_MAX).filter(|&s| is_locked(s)).collect();
    if found.len() < MIN_LOCKED {
        let mut wider: Vec<u64> = (FIRST_SCAN_MAX + 1..=WIDE_SCAN_MAX)
            .filter(|&s| is_locked(s))
            .collect();
        found.append(&mut wider);
    }
    found
}

/// The corrected Locked insolation field (spec §3), synthetic: Lambert
/// cosine off the substellar point, night side floored at `0.0`. The
/// fixed contract every implementation of the SKY-24 fix must satisfy.
fn corrected_locked_insolation(geo: &Geosphere, insolation_scalar: f64) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let cos_theta = substellar_cosine(geo.position(cell));
        insolation_scalar * cos_theta.max(0.0)
    })
}

/// The shipped `Substrate` field with ONLY its `insolation` term replaced
/// by [`corrected_locked_insolation`] — temperature/moisture/elevation stay
/// exactly what `substrate_field` (the real, shipped path) computed, since
/// those axes are already regime-aware upstream (Task 1). This is the
/// probe's one and only injection point; nothing about the generator
/// changes.
fn substrate_with_corrected_insolation(
    geo: &Geosphere,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
    obliquity_deg: f64,
    insolation_scalar: f64,
) -> CellMap<Substrate> {
    let shipped = substrate_field(geo, terrain, climate, obliquity_deg, insolation_scalar);
    let corrected_insolation = corrected_locked_insolation(geo, insolation_scalar);
    CellMap::from_fn(geo, |cell| {
        let s = *shipped.get(cell);
        Substrate {
            insolation: *corrected_insolation.get(cell),
            ..s
        }
    })
}

/// Mirrors `hornvale_worldgen::niche_per_species_k`'s suitability product
/// exactly (supply term × temperature × moisture × insolation × elevation
/// condition responses), but reads every axis off a caller-supplied
/// `Substrate` field instead of rebuilding one internally — the injection
/// seam this probe needs and `niche_per_species_k` does not expose.
fn niche_k_over(
    geo: &Geosphere,
    base_carrying: &CellMap<f64>,
    substrate: &CellMap<Substrate>,
    species_set: &[&SpeciesDef],
) -> Vec<(u32, CellMap<f64>)> {
    species_set
        .iter()
        .enumerate()
        .map(|(tag, def)| {
            let total_uptake: f64 = hornvale_kernel::v1_basis()
                .iter()
                .map(|axis| def.biosphere.niche.weight(*axis))
                .sum();
            let floor_buf =
                hornvale_kernel::sovereignty_floor(def.biosphere.mass, def.biosphere.potency);
            let cn = &def.biosphere.condition_niche;
            let k = CellMap::from_fn(geo, |cell| {
                let s = substrate.get(cell);
                let supply = base_carrying.get(cell) * total_uptake;
                let saturated = supply / (1.0 + supply);
                saturated
                    * cn.temperature.eval(s.temperature_c, floor_buf)
                    * cn.moisture.eval(s.moisture, floor_buf)
                    * cn.insolation.eval(s.insolation, floor_buf)
                    * cn.elevation.eval(s.elevation, 0.0)
            });
            (tag as u32, k)
        })
        .collect()
}

/// The K-peak zone classification (brief point 4), a clean partition of
/// `substellar_cosine`'s `[-1, 1]` range: `(0.6, 1]` substellar, `(0.2,
/// 0.6]` mid/terminator-day, `[-0.2, 0.2]` terminator, `[-1, -0.2)`
/// night/antistellar.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PeakZone {
    /// The bug's outcome: dominance pinned to the fixed noon point.
    Substellar,
    /// Between the substellar cap and the terminator ring.
    MidTerminatorDay,
    /// The fix's goal: dominance rings the day/night boundary.
    Terminator,
    /// The permanent night side.
    NightAntistellar,
}

impl PeakZone {
    fn of(cos_theta: f64) -> PeakZone {
        if cos_theta > 0.6 {
            PeakZone::Substellar
        } else if cos_theta > 0.2 {
            PeakZone::MidTerminatorDay
        } else if cos_theta >= -0.2 {
            PeakZone::Terminator
        } else {
            PeakZone::NightAntistellar
        }
    }

    fn label(self) -> &'static str {
        match self {
            PeakZone::Substellar => "substellar",
            PeakZone::MidTerminatorDay => "mid/terminator-day",
            PeakZone::Terminator => "terminator",
            PeakZone::NightAntistellar => "night/antistellar",
        }
    }
}

/// One locked seed's measurement: the world-dominant peopled species
/// (highest total K over habitable cells), its K-argmax cell's
/// `substellar_cosine`, and the resulting [`PeakZone`].
#[derive(Debug)]
struct SeedRow {
    seed: u64,
    insolation_scalar: f64,
    obliquity_deg: f64,
    dominant_species: &'static str,
    dominant_total_k: f64,
    peak_cos_theta: f64,
    zone: PeakZone,
}

/// Measure one locked seed: build to `BuildDepth::Terrain`, assemble the
/// corrected substrate, run the peopled roster's K, and reduce to a
/// [`SeedRow`]. Panics loudly (via `expect`) on any reconstruction failure
/// — a locked seed identified by [`is_locked`] must build cleanly at the
/// deeper rung too, since terrain genesis reads no sky-regime-specific
/// pin.
fn measure_seed(seed: u64, roster: &[SpeciesDef]) -> SeedRow {
    let world = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        roster,
        BuildDepth::Terrain,
    )
    .expect("a locked seed identified at Astronomy depth builds at Terrain depth too");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let sky = sky_of(&world).expect("sky reconstructs");
    let system = sky
        .system()
        .expect("a locked seed always has a generated star system");
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    assert!(
        matches!(system.anchor.rotation, Rotation::Locked),
        "seed {seed} was selected as locked at Astronomy depth but reads non-Locked at Terrain depth"
    );

    let substrate = substrate_with_corrected_insolation(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
    );
    let base_inputs = carrying_inputs_of(geo, &terrain, &climate);
    let base_carrying = hornvale_demography::carrying_capacity(geo, &base_inputs);

    let peopled: Vec<&SpeciesDef> = roster.iter().filter(|d| d.peopled.is_some()).collect();
    let per_species_k = niche_k_over(geo, &base_carrying, &substrate, &peopled);
    let habitable = climate.habitability();

    // The world-dominant species: highest total K summed over habitable
    // cells. Ties broken by species-name order (ascending `tag` index
    // matches `peopled`'s — itself `roster`'s, itself the registry's
    // alphabetical — order, so a tie is already deterministic without an
    // explicit tiebreak; the `max_by` below keeps the FIRST maximum, i.e.
    // the alphabetically-earliest species on an exact tie).
    let (dominant_tag, dominant_total_k) = per_species_k
        .iter()
        .map(|(tag, k)| {
            let total: f64 = geo
                .cells()
                .filter(|c| *habitable.get(*c))
                .map(|c| *k.get(c))
                .sum();
            (*tag, total)
        })
        .max_by(|(_, a), (_, b)| a.total_cmp(b))
        .expect("the peopled roster is non-empty");

    let dominant_k = per_species_k
        .iter()
        .find(|(tag, _)| *tag == dominant_tag)
        .map(|(_, k)| k)
        .expect("dominant_tag came from per_species_k itself");
    let dominant_species = peopled[dominant_tag as usize].name;

    let peak_cell = geo
        .cells()
        .filter(|c| *habitable.get(*c))
        .max_by(|a, b| dominant_k.get(*a).total_cmp(dominant_k.get(*b)))
        .expect("at least one habitable cell exists");
    let peak_cos_theta = substellar_cosine(geo.position(peak_cell));

    SeedRow {
        seed,
        insolation_scalar,
        obliquity_deg,
        dominant_species,
        dominant_total_k,
        peak_cos_theta,
        zone: PeakZone::of(peak_cos_theta),
    }
}

/// The Stage-0 synthetic probe: identify locked seeds, measure each under
/// the corrected Locked insolation field, and print the per-seed table plus
/// the terminator-share headline. `#[ignore]`d — a live-worldgen instrument
/// (release profile still takes several seconds per locked seed once
/// terrain genesis runs), not part of the commit gate or `make gate-full`'s
/// heavy tier; run explicitly:
/// `cargo test -p hornvale-worldgen --release --test insolation_probe --
/// --ignored --nocapture`.
#[test]
#[ignore = "live-worldgen Stage-0 probe (SKY-24 the-terminator Task 2): scans up to 1000 seeds \
            for the Locked rotation regime and rebuilds terrain/climate per locked seed; \
            a one-shot preregistered readout, not a fast-gate battery"]
fn dominant_k_peak_under_corrected_locked_insolation() {
    let roster = default_roster();
    let locked = locked_seeds();
    assert!(
        !locked.is_empty(),
        "expected at least one locked seed in 1..={WIDE_SCAN_MAX}"
    );

    println!("\n=== Stage-0 Terminator probe: locked-seed K-peak zones ===");
    println!("locked seeds found: {} (scanned 1..={})", locked.len(), {
        if locked.len() < MIN_LOCKED {
            WIDE_SCAN_MAX
        } else {
            FIRST_SCAN_MAX
        }
    });
    println!(
        "{:>6} | {:>10} | {:>9} | {:>16} | {:>14} | {:>10} | {:>20}",
        "seed", "insol_scl", "obliq_deg", "dominant", "total_k", "peak_cos", "zone"
    );
    println!("{}", "-".repeat(100));

    let rows: Vec<SeedRow> = locked
        .iter()
        .map(|&seed| measure_seed(seed, &roster))
        .collect();
    for row in &rows {
        println!(
            "{:>6} | {:>10.4} | {:>9.2} | {:>16} | {:>14.4} | {:>10.4} | {:>20}",
            row.seed,
            row.insolation_scalar,
            row.obliquity_deg,
            row.dominant_species,
            row.dominant_total_k,
            row.peak_cos_theta,
            row.zone.label()
        );
    }

    let terminator_share = rows
        .iter()
        .filter(|r| r.peak_cos_theta.abs() <= TERMINATOR_HEADLINE_HALF_WIDTH)
        .count() as f64
        / rows.len() as f64;
    let substellar_share = rows
        .iter()
        .filter(|r| r.zone == PeakZone::Substellar)
        .count() as f64
        / rows.len() as f64;

    println!(
        "\nterminator-ring share (|cos| <= {TERMINATOR_HEADLINE_HALF_WIDTH}): {:.4} ({}/{})",
        terminator_share,
        rows.iter()
            .filter(|r| r.peak_cos_theta.abs() <= TERMINATOR_HEADLINE_HALF_WIDTH)
            .count(),
        rows.len()
    );
    println!("substellar-zone share: {:.4}", substellar_share);

    // Sanity floor, not a freeze: every reading must be finite and every
    // cosine in range — a NaN/out-of-range value would mean the harness
    // itself is broken, not that a seed is "wrong".
    for row in &rows {
        assert!(
            row.dominant_total_k.is_finite() && row.dominant_total_k >= 0.0,
            "seed {} dominant total K must be finite and non-negative, got {}",
            row.seed,
            row.dominant_total_k
        );
        assert!(
            (-1.0..=1.0).contains(&row.peak_cos_theta),
            "seed {} peak cos_theta out of [-1, 1]: {}",
            row.seed,
            row.peak_cos_theta
        );
    }
    assert!(
        terminator_share.is_finite() && (0.0..=1.0).contains(&terminator_share),
        "terminator_share must be a finite fraction in [0, 1], got {terminator_share}"
    );
}
