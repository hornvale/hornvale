//! The surface cover layer: what grows, falls, or settles *on top of* the
//! mineral ground [`hornvale_terrain::lithology::reflectance`] already
//! projects (spec §3). `domains/terrain` stays a mineral mixture; this
//! window composes cover above it, per [`crate::LocaleContext::
//! reflectance_mixture_at`].
//!
//! **Why climate alone cannot supply this.** Every climate accessor
//! (`biome_expr_at`, `snow_fraction_at`, `temperature_at`, `moisture_at`) is
//! keyed on a canonical-grid [`Vertex`], and a walk-depth room addresses seven
//! refinement levels below that grid — `4^7` rooms share one corner. A cover
//! model built from climate alone would therefore return one colour for
//! every room in a band, exactly reproducing the bedrock-era defect this
//! campaign exists to fix (H1's own baseline: 31 vertices, 1 colour). So
//! [`cover_weights`] takes the room's own [`MicroField`] as well as the
//! vertex's climate, and lets the micro-field modulate a climate-set *regime*
//! rather than replace it — see [`cover_weights`]'s own doc for the split
//! and the bound kept on how far the modulation is allowed to move things.

use crate::regime::MicroField;
use hornvale_climate::{Formation, GeneratedClimate, Realm};
use hornvale_kernel::color::Reflectance;
use hornvale_kernel::{Vertex, WorldTime};

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
/// instead of re-deriving it from the blended colour. `pub` (not
/// `pub(crate)`): `windows/scene`'s `cover_legend` is built from
/// [`CoverClass::LEGEND`], and a `SurroundsCell.cover` index is meaningless
/// without it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoverClass {
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

impl CoverClass {
    /// The six cover-class names in stable index order — the self-describing
    /// legend for scene emission (mirrors `WaterKind::LEGEND`).
    pub const LEGEND: [&'static str; 6] = ["bare", "chlorophyll", "litter", "snow", "sand", "silt"];

    /// Stable numeric index into `LEGEND`, independent of enum discriminant
    /// layout (explicit `match`, not `self as u32`, so reordering variants
    /// can never silently change a committed index).
    /// type-audit: bare-ok(index: return)
    pub fn index(self) -> u32 {
        match self {
            CoverClass::Bare => 0,
            CoverClass::Chlorophyll => 1,
            CoverClass::Litter => 2,
            CoverClass::Snow => 3,
            CoverClass::Sand => 4,
            CoverClass::Silt => 5,
        }
    }

    /// Stable name, consistent with `LEGEND`.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(self) -> &'static str {
        match self {
            CoverClass::Bare => "bare",
            CoverClass::Chlorophyll => "chlorophyll",
            CoverClass::Litter => "litter",
            CoverClass::Snow => "snow",
            CoverClass::Sand => "sand",
            CoverClass::Silt => "silt",
        }
    }
}

/// One weighted cover component, tagged with the class it came from so
/// [`cover_class_at`] can pick the dominant one without redoing the weight
/// math [`cover_weights`] already did.
#[derive(Debug)]
struct Component {
    class: CoverClass,
    reflectance: [f64; hornvale_kernel::color::BANDS],
    weight: f64,
}

/// A land formation's base "how green can this vertex get" ceiling, `[0, 1]`,
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
        | Formation::Cave(_) => 0.0,
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
/// canonical vertex (address noise has no reason to repeat between
/// neighbouring rooms), exactly the "31 vertices, 31 colours" ceiling breach
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
/// regime, it does not decide whether a vertex has snow at all.
/// plumb: pending(wave-1)
const ASPECT_SNOW_SWING: f64 = 0.2;

/// How far the chlorophyll share of the vegetation total moves per openness
/// tier. A closed canopy (tier `-1`) reads greener; an open one (tier `+1`)
/// shows more litter and understory.
/// plumb: pending(wave-1)
const OPENNESS_CANOPY_SWING: f64 = 0.3;

/// How far the silt share of the bare-ground total moves per wetness tier,
/// added on top of the climate-set base split (`moisture_at`). Grounded in
/// hydrology on bare ground under open air (The Rill), so this one gets the
/// same three-tier treatment as the two address-noise axes above rather
/// than a wider, continuous swing.
/// plumb: pending(wave-1)
const WETNESS_TIER_DELTA: f64 = 0.2;

/// The cover components at `vertex` on `at`, modulated by this room's own
/// sub-vertex [`MicroField`] — the weighted endmember/weight pairs
/// [`crate::LocaleContext::reflectance_mixture_at`] appends to the mineral
/// mixture, plus the class each one came from (kept internal; see
/// [`cover_weights`] and [`cover_class_at`] for the two public-in-crate
/// projections of this).
///
/// **Climate sets the regime; micro modulates within it.** Snow, the
/// vegetation ceiling, and the bare-ground share are all read from `climate`
/// (formation, moisture, frozen/snow fraction) — the same value for every
/// room sharing this vertex. `micro`'s three address-noise axes (`aspect`,
/// `openness`, and — grounded, since The Rill — `wetness`) then perturb that
/// shared regime by a bounded amount each, which is what makes two rooms in
/// one canonical vertex differ at all without repainting raw noise: the
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
/// marginal, rain-heavy vertex doesn't go fully white the moment it dips below
/// freezing.
///
/// Every weight is non-negative and the returned total never exceeds `1.0`
/// (the caller's `1 - covered` mineral remainder is therefore always
/// non-negative too).
fn cover_components(
    climate: &GeneratedClimate,
    vertex: Vertex,
    micro: &MicroField,
    at: WorldTime,
) -> Vec<Component> {
    let expr = climate.biome_expr_at(vertex);
    cover_components_for_regime(
        expr.formation,
        expr.realm == Realm::OVERWORLD,
        climate.is_frozen_at(vertex, at),
        climate.snow_fraction_at(vertex),
        climate.moisture_at(vertex),
        micro,
    )
}

/// Pure cover-composition seam once the climate lookup has selected a
/// regime. Keeping the lookup outside lets unit tests construct a formation
/// directly when a committed world's population does not happen to contain
/// that rare formation; it does not add a second production derivation.
fn cover_components_for_regime(
    formation: Formation,
    on_land: bool,
    frozen: bool,
    annual_snow: f64,
    base_wet: f64,
    micro: &MicroField,
) -> Vec<Component> {
    // --- Snow: seasonal gate x annual propensity x a bounded aspect swing.
    let annual_snow = annual_snow.clamp(0.0, 1.0);
    let aspect_factor = (1.0 - ASPECT_SNOW_SWING * tier3(micro.aspect)).clamp(0.0, 2.0);
    // A frozen vertex always carries a snow floor (SNOW_FLOOR) even at zero
    // recorded snow_fraction — ground frost / rime rather than bare frozen
    // dirt — topped up toward SNOW_CEILING by how snow-heavy the climate
    // expects this vertex to be.
    /// plumb: pending(wave-1)
    const SNOW_FLOOR: f64 = 0.25;
    /// plumb: pending(wave-1)
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

    let veg_ceiling = if on_land {
        vegetation_ceiling(formation)
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
        let bare_total = ground_remaining * bare_ground_share(formation);
        if bare_total > 0.0 {
            // wetness tier: -1 (dry) .. +1 (wet). Climate moisture sets the
            // base split; the room's own (partly grounded) wetness axis
            // perturbs it by one of three fixed deltas.
            let base_wet = base_wet.clamp(0.0, 1.0);
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

/// The surface cover at `vertex` on `at`, modulated by this room's own
/// sub-vertex micro-field, as endmember/weight pairs summing to the covered
/// fraction. The bare-ground remainder is the caller's mineral mixture,
/// weighted `1 - covered` — see [`crate::LocaleContext::
/// reflectance_mixture_at`] for the composition. See [`cover_components`]
/// for the model itself; this is its public-in-crate projection, dropping
/// the class tag a colour caller has no use for.
pub(crate) fn cover_weights(
    climate: &GeneratedClimate,
    vertex: Vertex,
    micro: &MicroField,
    at: WorldTime,
) -> Vec<(Reflectance, f64)> {
    cover_components(climate, vertex, micro, at)
        .into_iter()
        .map(|c| {
            (
                Reflectance::new(c.reflectance).expect("authored endmember is within [0, 1]"),
                c.weight,
            )
        })
        .collect()
}

/// The dominant cover class at `vertex` on `at` — the categorical read a
/// consumer that wants "what covers this room" (Task 9) can take instead of
/// a blended colour. `Bare` when [`cover_components`] returns nothing (the
/// mineral ground shows through undisturbed). Ties break toward whichever
/// class [`cover_components`] pushed first (snow, then chlorophyll/litter,
/// then sand/silt) — the same "stable, deterministic tie-break" spirit
/// [`crate::dominant_corner`] uses for a room's categorical corner, chosen
/// here rather than left to `Iterator::max_by`'s "last on a tie" default.
/// Called from production code via [`crate::LocaleContext::cover_class_at`]
/// (Task 9), which resolves `vertex` from a `Facet` the same way
/// [`crate::LocaleContext::reflectance_mixture_at`] does.
pub(crate) fn cover_class_at(
    climate: &GeneratedClimate,
    vertex: Vertex,
    micro: &MicroField,
    at: WorldTime,
) -> CoverClass {
    let mut best: Option<(CoverClass, f64)> = None;
    for c in cover_components(climate, vertex, micro, at) {
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
    use hornvale_worldgen::{climate_from, terrain_of};

    /// Seed 42's climate, built the same way [`crate::LocaleContext::
    /// build`] does — but standalone, so these tests don't need a full
    /// `LocaleContext` (a `GeneratedClimate` and a `Vertex` are all
    /// [`cover_weights`]/[`cover_class_at`] take). The committed fixture
    /// supplies the mandatory astronomy facts without paying for genesis.
    // Named construction site (decision 0092): a standalone derive-once
    // wrapper for these tests, mirroring `LocaleContext::build`'s own
    // scoped allow rather than reaching for the disallowed methods inline.
    #[allow(clippy::disallowed_methods)]
    fn climate_seed_42() -> GeneratedClimate {
        let world = hornvale_worldgen::fixture::seed_42_world();
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

    /// The first vertex (in ascending `Vertex` order, for a deterministic
    /// pick) whose climate satisfies `pred` — a real vertex, not a synthetic
    /// one, because every accessor `cover_weights` reads is inherent on
    /// `GeneratedClimate` and cannot be mocked.
    fn find_vertex(climate: &GeneratedClimate, pred: impl Fn(Vertex) -> bool) -> Option<Vertex> {
        let geo = climate.geosphere();
        (0..geo.vertex_count() as u32)
            .map(Vertex)
            .find(|&c| pred(c))
    }

    /// FINDING 4 (fix round 1): `LEGEND`, `index()`, and `name()` are three
    /// hand-maintained parallel lists with nothing else tying them
    /// together — the same discipline `WaterKind`'s own test uses
    /// (`domains/terrain/src/water.rs`), reused here so a variant added to
    /// one and not the other two reddens immediately rather than silently
    /// mismatching the wire's `cover_legend` against `CoverClass::name()`'s
    /// prose.
    #[test]
    fn legend_index_and_name_agree_for_every_variant() {
        assert_eq!(CoverClass::Bare.name(), "bare");
        assert_eq!(CoverClass::Chlorophyll.name(), "chlorophyll");
        assert_eq!(CoverClass::Litter.name(), "litter");
        assert_eq!(CoverClass::Snow.name(), "snow");
        assert_eq!(CoverClass::Sand.name(), "sand");
        assert_eq!(CoverClass::Silt.name(), "silt");
        assert_eq!(
            CoverClass::LEGEND,
            ["bare", "chlorophyll", "litter", "snow", "sand", "silt"]
        );
        for c in [
            CoverClass::Bare,
            CoverClass::Chlorophyll,
            CoverClass::Litter,
            CoverClass::Snow,
            CoverClass::Sand,
            CoverClass::Silt,
        ] {
            assert_eq!(CoverClass::LEGEND[c.index() as usize], c.name());
        }
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
        for i in (0..geo.vertex_count() as u32).step_by(29) {
            let vertex = Vertex(i);
            for day in [0.0, 91.0, 182.0, 273.0] {
                let at = WorldTime::from_std_days(day).expect("finite day");
                let cover = cover_weights(&climate, vertex, &micro, at);
                let covered: f64 = cover.iter().map(|(_, w)| w).sum();
                assert!(
                    (0.0..=1.0 + 1e-9).contains(&covered),
                    "vertex {vertex:?} day {day} covered={covered} out of [0, 1]"
                );
                for (_, w) in &cover {
                    assert!(
                        *w >= 0.0,
                        "vertex {vertex:?} day {day} has a negative weight"
                    );
                }
                sampled += 1;
            }
        }
        assert!(
            sampled > 100,
            "too few vertices sampled to trust this sweep"
        );
    }

    #[test]
    fn a_frozen_vertex_carries_snow_and_reads_snow_dominant() {
        let climate = climate_seed_42();
        let vertex = find_vertex(&climate, |c| climate.is_frozen_at(c, WorldTime::GENESIS))
            .expect("seed 42 has at least one vertex frozen at day 0");
        let micro = neutral();
        let at = WorldTime::GENESIS;
        let cover = cover_weights(&climate, vertex, &micro, at);
        let snow_weight: f64 = cover
            .iter()
            .find(|(r, _)| r.get() == &endmembers::SNOW)
            .map(|(_, w)| *w)
            .unwrap_or(0.0);
        assert!(
            snow_weight > 0.0,
            "a frozen vertex must carry a nonzero snow weight"
        );
        assert_eq!(
            cover_class_at(&climate, vertex, &micro, at),
            CoverClass::Snow
        );
    }

    #[test]
    fn an_unfrozen_open_water_vertex_is_bare() {
        let climate = climate_seed_42();
        let vertex = find_vertex(&climate, |c| {
            climate.biome_expr_at(c).realm == Realm::WATERWORLD
                && !climate.is_frozen_at(c, WorldTime::GENESIS)
        })
        .expect("seed 42 has at least one unfrozen water vertex");
        let micro = neutral();
        let at = WorldTime::GENESIS;
        let cover = cover_weights(&climate, vertex, &micro, at);
        assert!(
            cover.is_empty(),
            "an unfrozen water vertex should carry no surface cover: {cover:?}"
        );
        assert_eq!(
            cover_class_at(&climate, vertex, &micro, at),
            CoverClass::Bare
        );
    }

    #[test]
    fn a_desert_vertex_leans_sand_or_silt_not_vegetation() {
        let micro = neutral();
        // The Zenith's valid generated-sky population has no seed-42 desert.
        // Construct the authored Desert regime directly through the same pure
        // seam production uses after climate lookup. This is not a convenient
        // seed search: the test fixes every relevant input and exercises the
        // production composition rules deterministically.
        let cover = cover_components_for_regime(Formation::Desert, true, false, 0.0, 0.2, &micro);
        let has_chlorophyll = cover
            .iter()
            .any(|component| component.reflectance == endmembers::CHLOROPHYLL);
        assert!(
            !has_chlorophyll,
            "a desert vertex should carry no chlorophyll weight: {cover:?}"
        );
        let mineral_like: f64 = cover
            .iter()
            .filter(|component| {
                component.reflectance == endmembers::SAND
                    || component.reflectance == endmembers::SILT
            })
            .map(|component| component.weight)
            .sum();
        assert!(
            mineral_like > 0.0,
            "a desert vertex should carry some sand/silt weight: {cover:?}"
        );
    }

    #[test]
    fn openness_moves_the_chlorophyll_litter_split_but_not_the_total() {
        let climate = climate_seed_42();
        let vertex = find_vertex(&climate, |c| {
            let expr = climate.biome_expr_at(c);
            expr.realm == Realm::OVERWORLD
                && vegetation_ceiling(expr.formation) > 0.0
                && !climate.is_frozen_at(c, WorldTime::GENESIS)
        })
        .expect("seed 42 has at least one unfrozen vegetated land vertex");
        let at = WorldTime::GENESIS;
        let closed = cover_weights(
            &climate,
            vertex,
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
            vertex,
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
            "a closed canopy must read greener than an open one at the same vertex"
        );
        assert!(
            (veg_total(&closed) - veg_total(&open)).abs() < 1e-9,
            "openness must split the vegetation total, not change it"
        );
    }
}
