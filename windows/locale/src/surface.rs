//! The surface cover layer: what grows, falls, or settles *on top of* the
//! mineral ground [`hornvale_terrain::lithology::reflectance`] already
//! projects (spec §3). `domains/terrain` stays a mineral mixture; this
//! window composes cover above it, per [`crate::LocaleContext::
//! reflectance_mixture_at`].
//!
//! **Why climate alone cannot supply this.** Every climate accessor
//! (`biome_expr_at`, `snow_fraction_at`, `temperature_at`, `moisture_at`) is
//! keyed on a canonical-grid [`CellId`], and a walk-depth room addresses six
//! refinement levels below that grid — `4^6` rooms share one corner. A cover
//! model built from climate alone would therefore return one colour for
//! every room in a band, exactly reproducing the bedrock-era defect this
//! campaign exists to fix (H1's own baseline: 31 cells, 1 colour). So
//! [`cover_weights`] takes the room's own [`MicroField`] as well as the
//! cell's climate, and lets the micro-field modulate a climate-set *regime*
//! rather than replace it — see [`cover_weights`]'s own doc for the split
//! and the bound kept on how far the modulation is allowed to move things.

use crate::regime::MicroField;
use hornvale_climate::{Formation, GeneratedClimate, Realm};
use hornvale_kernel::color::Reflectance;
use hornvale_kernel::{CellId, WorldTime};

/// Visible-band reflectance curves for surface cover, the surface analogue
/// of [`hornvale_terrain::lithology::endmembers`]. Authored constants, not
/// measured spectra: they need to be ordinally right against each other
/// (snow brighter than litter, litter brighter than living green, sand
/// warmer than silt), not photometrically exact.
pub(crate) mod endmembers {
    use hornvale_kernel::color::BANDS;

    /// Living foliage — low in red, a rise into green, dark again at the
    /// blue end. The near-infrared "red edge" is deliberately absent: it
    /// sits at 720 nm, outside the visible bands this array spans.
    /// type-audit: bare-ok(ratio)
    pub const CHLOROPHYLL: [f64; BANDS] =
        [0.04, 0.05, 0.09, 0.18, 0.14, 0.07, 0.06, 0.06, 0.07, 0.08];
    /// Dead leaf litter and dry stems — flat and brownish, brighter than
    /// foliage at the red end and darker in green.
    /// type-audit: bare-ok(ratio)
    pub const LITTER: [f64; BANDS] = [0.08, 0.10, 0.13, 0.16, 0.19, 0.23, 0.26, 0.28, 0.29, 0.30];
    /// Fresh snow — the brightest surface in the set and nearly flat, which
    /// is what makes a peak read white rather than pale-blue.
    /// type-audit: bare-ok(ratio)
    pub const SNOW: [f64; BANDS] = [0.92, 0.93, 0.94, 0.94, 0.94, 0.93, 0.93, 0.92, 0.91, 0.90];
    /// Quartz sand — bright and warm, rising steadily toward red.
    /// type-audit: bare-ok(ratio)
    pub const SAND: [f64; BANDS] = [0.18, 0.22, 0.28, 0.34, 0.40, 0.45, 0.49, 0.52, 0.54, 0.55];
    /// Wet silt and mud — dark, slightly warm, and flatter than sand.
    /// type-audit: bare-ok(ratio)
    pub const SILT: [f64; BANDS] = [0.07, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.19, 0.20, 0.21];
}

/// The named cover classes [`cover_weights`] composes from — the categorical
/// read a consumer that wants "what covers this room" (Task 9) can take
/// instead of re-deriving it from the blended colour.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CoverClass {
    /// No cover at all — bare mineral ground, i.e. the terrain layer alone.
    Bare,
    /// Living foliage.
    Chlorophyll,
    /// Dead litter and dry stems.
    Litter,
    /// Snow.
    Snow,
    /// Wind-worked sand.
    Sand,
    /// Wet silt and mud.
    Silt,
}

/// One weighted cover component, tagged with the class it came from so
/// [`cover_class_at`] can pick the dominant one without redoing the weight
/// math [`cover_weights`] already did.
struct Component {
    class: CoverClass,
    reflectance: [f64; hornvale_kernel::color::BANDS],
    weight: f64,
}

/// A land formation's base "how green can this cell get" ceiling, `[0, 1]`,
/// at full vegetative cover (before the room's own [`MicroField::openness`]
/// splits it into chlorophyll vs. litter, and before snow occludes it
/// seasonally). Ordinal against the [`Formation`] roster's own sense of
/// "how much does this place grow": the rainforests top out highest, the
/// treeless/frozen/marine formations return `0.0`. Not a measured statistic
/// — an authored ranking, the same spirit as the endmember curves above.
fn vegetation_ceiling(formation: Formation) -> f64 {
    match formation {
        Formation::TropicalRainforest => 0.85,
        Formation::TemperateRainforest => 0.80,
        Formation::TropicalSeasonalForest => 0.72,
        Formation::TemperateForest => 0.70,
        Formation::Taiga => 0.62,
        Formation::TemperateGrassland => 0.55,
        Formation::Savanna => 0.45,
        Formation::Shrubland => 0.35,
        Formation::Tundra => 0.15,
        Formation::Alpine => 0.10,
        // Bare, frozen, submerged or subterranean formations grow nothing a
        // surface cover model represents.
        Formation::Desert
        | Formation::Ice
        | Formation::SeaIce
        | Formation::Reef
        | Formation::KelpForest
        | Formation::Vent
        | Formation::Upwelling
        | Formation::OpenWater
        | Formation::KarstCave
        | Formation::LavaTube
        | Formation::FractureCave => 0.0,
    }
}

/// A land formation's share of exposed bare ground (sand/silt, as opposed to
/// the vegetation or bedrock that would otherwise show) once vegetation and
/// snow have taken their share — deserts read almost entirely as sand/silt,
/// closed forests show barely a sliver of it at their margins.
fn bare_ground_share(formation: Formation) -> f64 {
    match formation {
        Formation::Desert => 0.75,
        Formation::Savanna => 0.30,
        Formation::Shrubland => 0.25,
        Formation::TemperateGrassland => 0.18,
        Formation::TropicalSeasonalForest => 0.12,
        Formation::Tundra => 0.20,
        Formation::Alpine => 0.15,
        _ => 0.05,
    }
}

/// Band a `[-1, 1]` micro axis to a coarse tier: shaded/closed/dry (`-1`),
/// mid (`0`), or sunlit/open/wet (`+1`). The `±0.33` split is not a new
/// threshold invented for this module — it is `grammar.rs`'s own
/// `land_micro_habitat`/`micro_habitat` clause boundary, reused so the
/// colour layer and the room's own rendered descriptor agree about what
/// counts as "close to an extreme" along the same axis.
///
/// **Why banded rather than continuous.** All three of `aspect`, `openness`
/// and (partly) `wetness` are address noise (`regime.rs`'s own doc) — a
/// continuous read of them produces a continuous colour, which is a
/// continuum of near-unique colours over a walk band that shares one
/// canonical cell (address noise has no reason to repeat between
/// neighbouring rooms), exactly the "31 cells, 31 colours" ceiling breach
/// the campaign warns against. Banding into three tiers caps how many
/// distinguishable covers one climate regime can produce, independent of
/// how many rooms sample it.
fn tier3(axis: f64) -> f64 {
    if axis > 0.33 {
        1.0
    } else if axis < -0.33 {
        -1.0
    } else {
        0.0
    }
}

/// How far the snow weight moves per aspect tier, as a fraction of it. A
/// sunlit face (`aspect` tier `+1`) sheds snow faster than a shaded one
/// (tier `-1`) — bounded small so this perturbs the climate-set snow
/// regime, it does not decide whether a cell has snow at all.
const ASPECT_SNOW_SWING: f64 = 0.2;

/// How far the chlorophyll share of the vegetation total moves per openness
/// tier. A closed canopy (tier `-1`) reads greener; an open one (tier `+1`)
/// shows more litter and understory.
const OPENNESS_CANOPY_SWING: f64 = 0.3;

/// How far the silt share of the bare-ground total moves per wetness tier,
/// added on top of the climate-set base split (`moisture_at`). Grounded in
/// hydrology on bare ground under open air (The Rill), so this one gets the
/// same three-tier treatment as the two address-noise axes above rather
/// than a wider, continuous swing.
const WETNESS_TIER_DELTA: f64 = 0.2;

/// The cover components at `cell` on `at`, modulated by this room's own
/// sub-cell [`MicroField`] — the weighted endmember/weight pairs
/// [`crate::LocaleContext::reflectance_mixture_at`] appends to the mineral
/// mixture, plus the class each one came from (kept internal; see
/// [`cover_weights`] and [`cover_class_at`] for the two public-in-crate
/// projections of this).
///
/// **Climate sets the regime; micro modulates within it.** Snow, the
/// vegetation ceiling, and the bare-ground share are all read from `climate`
/// (formation, moisture, frozen/snow fraction) — the same value for every
/// room sharing this cell. `micro`'s three address-noise axes (`aspect`,
/// `openness`, and — grounded, since The Rill — `wetness`) then perturb that
/// shared regime by a bounded amount each, which is what makes two rooms in
/// one canonical cell differ at all without repainting raw noise: the
/// perturbation is capped well under the regime's own swing (deserts vs.
/// rainforest, frozen vs. not), so a band shows a handful of distinguishable
/// covers, not a continuum of 31 unique ones.
///
/// **Snow first, off the top.** It occludes everything under it, so its
/// weight is subtracted from the ground available to vegetation and
/// sand/silt rather than competing with them for the same budget — see the
/// `remaining_after_snow` split below. `is_frozen_at` is the seasonal gate
/// (no snow lying on unfrozen ground); `snow_fraction_at` sets how much of
/// the year's precipitation the climate expects as snow at all, so a
/// marginal, rain-heavy cell doesn't go fully white the moment it dips below
/// freezing.
///
/// Every weight is non-negative and the returned total never exceeds `1.0`
/// (the caller's `1 - covered` mineral remainder is therefore always
/// non-negative too).
fn cover_components(
    climate: &GeneratedClimate,
    cell: CellId,
    micro: &MicroField,
    at: WorldTime,
) -> Vec<Component> {
    let expr = climate.biome_expr_at(cell);

    // --- Snow: seasonal gate x annual propensity x a bounded aspect swing.
    let frozen = climate.is_frozen_at(cell, at.day());
    let annual_snow = climate.snow_fraction_at(cell).clamp(0.0, 1.0);
    let aspect_factor = (1.0 - ASPECT_SNOW_SWING * tier3(micro.aspect)).clamp(0.0, 2.0);
    // A frozen cell always carries a snow floor (SNOW_FLOOR) even at zero
    // recorded snow_fraction — ground frost / rime rather than bare frozen
    // dirt — topped up toward SNOW_CEILING by how snow-heavy the climate
    // expects this cell to be.
    const SNOW_FLOOR: f64 = 0.25;
    const SNOW_CEILING: f64 = 0.85;
    let snow_weight = if frozen {
        (SNOW_FLOOR + (SNOW_CEILING - SNOW_FLOOR) * annual_snow) * aspect_factor
    } else {
        0.0
    }
    // Clamped to `1.0`, not just `SNOW_CEILING`: `aspect_factor` can push
    // past the ceiling on a maximally shaded face, and a snow weight above
    // `1.0` would leave `remaining_after_snow` at `0.0` while `covered`
    // (this weight alone) still exceeded `1.0` — a negative mineral weight
    // downstream, which `Mixture::new` would reject.
    .clamp(0.0, 1.0);

    let remaining_after_snow = (1.0 - snow_weight).max(0.0);

    // --- Vegetation: land only, split chlorophyll/litter by canopy openness.
    let mut out = Vec::with_capacity(5);
    if snow_weight > 0.0 {
        out.push(Component {
            class: CoverClass::Snow,
            reflectance: endmembers::SNOW,
            weight: snow_weight,
        });
    }

    let on_land = expr.realm == Realm::OVERWORLD;
    let veg_ceiling = if on_land {
        vegetation_ceiling(expr.formation)
    } else {
        0.0
    };
    let veg_total = veg_ceiling * remaining_after_snow;
    if veg_total > 0.0 {
        // openness tier: -1 (closed) .. +1 (open). Closed canopy reads
        // greener, so the chlorophyll share moves the OPPOSITE way from the
        // tier (tier -1 -> share +swing).
        let chlorophyll_share =
            (0.5 - OPENNESS_CANOPY_SWING * tier3(micro.openness)).clamp(0.0, 1.0);
        let chlorophyll_weight = veg_total * chlorophyll_share;
        let litter_weight = veg_total - chlorophyll_weight;
        if chlorophyll_weight > 0.0 {
            out.push(Component {
                class: CoverClass::Chlorophyll,
                reflectance: endmembers::CHLOROPHYLL,
                weight: chlorophyll_weight,
            });
        }
        if litter_weight > 0.0 {
            out.push(Component {
                class: CoverClass::Litter,
                reflectance: endmembers::LITTER,
                weight: litter_weight,
            });
        }
    }

    // --- Sand/silt: land only, the ground vegetation and snow left bare.
    if on_land {
        let ground_remaining = (remaining_after_snow - veg_total).max(0.0);
        let bare_total = ground_remaining * bare_ground_share(expr.formation);
        if bare_total > 0.0 {
            // wetness tier: -1 (dry) .. +1 (wet). Climate moisture sets the
            // base split; the room's own (partly grounded) wetness axis
            // perturbs it by one of three fixed deltas.
            let base_wet = climate.moisture_at(cell).clamp(0.0, 1.0);
            let wet_share = (base_wet + WETNESS_TIER_DELTA * tier3(micro.wetness)).clamp(0.0, 1.0);
            let silt_weight = bare_total * wet_share;
            let sand_weight = bare_total - silt_weight;
            if sand_weight > 0.0 {
                out.push(Component {
                    class: CoverClass::Sand,
                    reflectance: endmembers::SAND,
                    weight: sand_weight,
                });
            }
            if silt_weight > 0.0 {
                out.push(Component {
                    class: CoverClass::Silt,
                    reflectance: endmembers::SILT,
                    weight: silt_weight,
                });
            }
        }
    }

    out
}

/// The surface cover at `cell` on `at`, modulated by this room's own
/// sub-cell micro-field, as endmember/weight pairs summing to the covered
/// fraction. The bare-ground remainder is the caller's mineral mixture,
/// weighted `1 - covered` — see [`crate::LocaleContext::
/// reflectance_mixture_at`] for the composition. See [`cover_components`]
/// for the model itself; this is its public-in-crate projection, dropping
/// the class tag a colour caller has no use for.
pub(crate) fn cover_weights(
    climate: &GeneratedClimate,
    cell: CellId,
    micro: &MicroField,
    at: WorldTime,
) -> Vec<(Reflectance, f64)> {
    cover_components(climate, cell, micro, at)
        .into_iter()
        .map(|c| {
            (
                Reflectance::new(c.reflectance).expect("authored endmember is within [0, 1]"),
                c.weight,
            )
        })
        .collect()
}

/// The dominant cover class at `cell` on `at` — the categorical read a
/// consumer that wants "what covers this room" (Task 9) can take instead of
/// a blended colour. `Bare` when [`cover_components`] returns nothing (the
/// mineral ground shows through undisturbed). Ties break toward whichever
/// class [`cover_components`] pushed first (snow, then chlorophyll/litter,
/// then sand/silt) — the same "stable, deterministic tie-break" spirit
/// [`crate::dominant_corner`] uses for a room's categorical corner, chosen
/// here rather than left to `Iterator::max_by`'s "last on a tie" default.
// Not yet called from production code — Task 9 (spec §3, the interfaces
// list on this campaign's Task 2b brief) is the consumer, and this task's
// job is to land the categorical projection Task 9 will read, not to wire
// a caller for it early. Exercised by this module's own tests in the
// meantime, which is why `cargo test` never warns, only the plain
// `cargo build`/`clippy` lib target does.
#[allow(dead_code)]
pub(crate) fn cover_class_at(
    climate: &GeneratedClimate,
    cell: CellId,
    micro: &MicroField,
    at: WorldTime,
) -> CoverClass {
    let mut best: Option<(CoverClass, f64)> = None;
    for c in cover_components(climate, cell, micro, at) {
        match best {
            Some((_, w)) if w >= c.weight => {}
            _ => best = Some((c.class, c.weight)),
        }
    }
    best.map(|(class, _)| class).unwrap_or(CoverClass::Bare)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, World};
    use hornvale_worldgen::{climate_from, terrain_of};

    /// Seed 42's tier-1 climate, built the same way [`crate::LocaleContext::
    /// build`] does — but standalone, so these tests don't need a full
    /// `LocaleContext` (a `GeneratedClimate` and a `CellId` are all
    /// [`cover_weights`]/[`cover_class_at`] take). `World::new` (no sky pin
    /// committed) defaults to `ConstantSun`, which is fine here: these tests
    /// probe the cover model's shape, not the seasonal swing H2 measures
    /// (that needs the generated sky — see `windows/locale/tests/
    /// surface_mixture.rs`'s `high_ground_is_brighter_in_the_cold_half_of_the_year`).
    // Named construction site (decision 0092): a standalone derive-once
    // wrapper for these tests, mirroring `LocaleContext::build`'s own
    // scoped allow rather than reaching for the disallowed methods inline.
    #[allow(clippy::disallowed_methods)]
    fn climate_seed_42() -> GeneratedClimate {
        let world = World::new(Seed(42));
        let terrain = terrain_of(&world).expect("seed 42 sculpts");
        climate_from(&world, &terrain).expect("seed 42 fits a climate")
    }

    fn neutral() -> MicroField {
        MicroField {
            relief: 0.0,
            aspect: 0.0,
            wetness: 0.0,
            openness: 0.0,
        }
    }

    /// The first cell (in ascending `CellId` order, for a deterministic
    /// pick) whose climate satisfies `pred` — a real cell, not a synthetic
    /// one, because every accessor `cover_weights` reads is inherent on
    /// `GeneratedClimate` and cannot be mocked.
    fn find_cell(climate: &GeneratedClimate, pred: impl Fn(CellId) -> bool) -> Option<CellId> {
        let geo = climate.geosphere();
        (0..geo.cell_count() as u32).map(CellId).find(|&c| pred(c))
    }

    #[test]
    fn covered_fraction_never_exceeds_the_bare_ground_budget() {
        let climate = climate_seed_42();
        let geo = climate.geosphere();
        let micro = MicroField {
            relief: 0.0,
            // The extremes on both axes that widen the budget furthest
            // (fully shaded holds the most snow; fully wet/dry still sums
            // to the same bare-ground total) are folded into the sweep
            // below rather than fixed here.
            aspect: -1.0,
            wetness: 1.0,
            openness: -1.0,
        };
        let mut sampled = 0;
        for i in (0..geo.cell_count() as u32).step_by(29) {
            let cell = CellId(i);
            for day in [0.0, 91.0, 182.0, 273.0] {
                let at = WorldTime::new(day).expect("finite day");
                let cover = cover_weights(&climate, cell, &micro, at);
                let covered: f64 = cover.iter().map(|(_, w)| w).sum();
                assert!(
                    (0.0..=1.0 + 1e-9).contains(&covered),
                    "cell {cell:?} day {day} covered={covered} out of [0, 1]"
                );
                for (_, w) in &cover {
                    assert!(*w >= 0.0, "cell {cell:?} day {day} has a negative weight");
                }
                sampled += 1;
            }
        }
        assert!(sampled > 100, "too few cells sampled to trust this sweep");
    }

    #[test]
    fn a_frozen_cell_carries_snow_and_reads_snow_dominant() {
        let climate = climate_seed_42();
        let cell = find_cell(&climate, |c| climate.is_frozen_at(c, 0.0))
            .expect("seed 42 has at least one cell frozen at day 0");
        let micro = neutral();
        let at = WorldTime::GENESIS;
        let cover = cover_weights(&climate, cell, &micro, at);
        let snow_weight: f64 = cover
            .iter()
            .find(|(r, _)| r.get() == &endmembers::SNOW)
            .map(|(_, w)| *w)
            .unwrap_or(0.0);
        assert!(
            snow_weight > 0.0,
            "a frozen cell must carry a nonzero snow weight"
        );
        assert_eq!(cover_class_at(&climate, cell, &micro, at), CoverClass::Snow);
    }

    #[test]
    fn an_unfrozen_open_water_cell_is_bare() {
        let climate = climate_seed_42();
        let cell = find_cell(&climate, |c| {
            climate.biome_expr_at(c).realm == Realm::WATERWORLD && !climate.is_frozen_at(c, 0.0)
        })
        .expect("seed 42 has at least one unfrozen water cell");
        let micro = neutral();
        let at = WorldTime::GENESIS;
        let cover = cover_weights(&climate, cell, &micro, at);
        assert!(
            cover.is_empty(),
            "an unfrozen water cell should carry no surface cover: {cover:?}"
        );
        assert_eq!(cover_class_at(&climate, cell, &micro, at), CoverClass::Bare);
    }

    #[test]
    fn a_desert_cell_leans_sand_or_silt_not_vegetation() {
        let climate = climate_seed_42();
        let cell = find_cell(&climate, |c| {
            climate.biome_expr_at(c).formation == Formation::Desert && !climate.is_frozen_at(c, 0.0)
        })
        // FINDING 5 (Task 2b fix round): this used to `return` silently on
        // `None`, on the reasoning that "seed 42 may simply have no desert
        // cell" is a legitimate finding. Measured: seed 42 has 15 Desert
        // cells of 40962 (0.037%) — rare, but real, so a silent skip here
        // was one climate tune away from this test going permanently green
        // without ever running its own assertions. `expect` makes that
        // failure loud instead: a future seed/tune with zero desert cells
        // now fails this test explicitly, which is the correct outcome —
        // it means the test needs a different cell-finding strategy, not
        // that it should quietly stop checking anything.
        .expect("seed 42 must have at least one unfrozen Desert cell (measured: 15 of 40962)");
        let micro = neutral();
        let cover = cover_weights(&climate, cell, &micro, WorldTime::GENESIS);
        let has_chlorophyll = cover
            .iter()
            .any(|(r, _)| r.get() == &endmembers::CHLOROPHYLL);
        assert!(
            !has_chlorophyll,
            "a desert cell should carry no chlorophyll weight: {cover:?}"
        );
        let mineral_like: f64 = cover
            .iter()
            .filter(|(r, _)| r.get() == &endmembers::SAND || r.get() == &endmembers::SILT)
            .map(|(_, w)| *w)
            .sum();
        assert!(
            mineral_like > 0.0,
            "a desert cell should carry some sand/silt weight: {cover:?}"
        );
    }

    #[test]
    fn openness_moves_the_chlorophyll_litter_split_but_not_the_total() {
        let climate = climate_seed_42();
        let cell = find_cell(&climate, |c| {
            let expr = climate.biome_expr_at(c);
            expr.realm == Realm::OVERWORLD
                && vegetation_ceiling(expr.formation) > 0.0
                && !climate.is_frozen_at(c, 0.0)
        })
        .expect("seed 42 has at least one unfrozen vegetated land cell");
        let at = WorldTime::GENESIS;
        let closed = cover_weights(
            &climate,
            cell,
            &MicroField {
                relief: 0.0,
                aspect: 0.0,
                wetness: 0.0,
                openness: -1.0,
            },
            at,
        );
        let open = cover_weights(
            &climate,
            cell,
            &MicroField {
                relief: 0.0,
                aspect: 0.0,
                wetness: 0.0,
                openness: 1.0,
            },
            at,
        );
        let green = |cover: &[(Reflectance, f64)]| -> f64 {
            cover
                .iter()
                .find(|(r, _)| r.get() == &endmembers::CHLOROPHYLL)
                .map(|(_, w)| *w)
                .unwrap_or(0.0)
        };
        let veg_total = |cover: &[(Reflectance, f64)]| -> f64 {
            cover
                .iter()
                .filter(|(r, _)| {
                    r.get() == &endmembers::CHLOROPHYLL || r.get() == &endmembers::LITTER
                })
                .map(|(_, w)| *w)
                .sum()
        };
        assert!(
            green(&closed) > green(&open),
            "a closed canopy must read greener than an open one at the same cell"
        );
        assert!(
            (veg_total(&closed) - veg_total(&open)).abs() < 1e-9,
            "openness must split the vegetation total, not change it"
        );
    }
}
