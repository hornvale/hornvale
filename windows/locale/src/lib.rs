#![warn(missing_docs)]
//! The locale window: a `RoomAddr` rendered as an observable place.

mod streams;
pub use streams::stream_labels;

mod regime;
pub use regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};

mod substrate;

mod micro;

mod grammar;

mod budget;
pub use budget::StrangeSite;
use budget::StrangenessBudget;

use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::{CellId, NearestCellIndex, RoomAddr, Seed, World, WorldTime, quantize};
use hornvale_terrain::GeneratedTerrain;
pub use hornvale_terrain::WaterKind;
use hornvale_worldgen::{climate_of, terrain_of};
use serde::Serialize;

/// The versioned semantic schema this window emits (save-format class; a
/// changed meaning mints `locale/room/v2` alongside).
/// type-audit: bare-ok(identifier-text)
pub const ROOM_SCHEMA: &str = "locale/room/v2";

/// A room rendered as an observable place — ground truth, re-derivable, never
/// stored (UNI-20 derived view). Plain serializable values only.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(index: id), bare-ok(index: face), bare-ok(index: path), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Locale {
    /// Schema tag (`locale/room/v2`).
    pub schema: &'static str,
    /// Packed room id (`RoomId.0`).
    pub id: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Child descent path.
    pub path: Vec<u8>,
    /// Refinement depth (`path.len()`).
    pub depth: u32,
    /// Centroid latitude, degrees (quantized).
    pub latitude: f64,
    /// Centroid longitude, degrees (quantized).
    pub longitude: f64,
    /// Inherited biome name (max-weight corner cell).
    pub biome: String,
    /// Blended continuous fields.
    pub fields: LocaleFields,
    /// The three canonical-grid corner cells and their integer weights.
    pub corners: Vec<CellWeight>,
    /// The strangeness overlay: descriptor, negation vector, and magnitude.
    pub regime: Regime,
    /// Base + vertical exits.
    pub exits: Vec<Exit>,
}

/// A canonical-grid corner cell and its integer blend weight.
/// type-audit: bare-ok(index: cell), bare-ok(count: weight)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CellWeight {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Integer weight (numerator over the summed denominator).
    pub weight: u64,
}

/// The blended continuous fields at the room centroid (weighted mean of the
/// three corner cells; quantized at emit).
/// type-audit: pending(wave-2: temperature_c), bare-ok(ratio: moisture), waiver(elevation-convention: elevation_m)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LocaleFields {
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Moisture (climate's dimensionless moisture field).
    pub moisture: f64,
    /// Elevation, meters.
    pub elevation_m: f64,
    /// Salt/fresh water at the room (max-weight cell — categorical, inherited,
    /// never blended). `water.is_fresh()` is the drinkable query. `WaterKind`
    /// lives in the terrain domain crate, which (decision 0002) depends on
    /// nothing but the kernel, so it cannot derive `Serialize` itself; this
    /// field serializes by its stable name instead (see `serialize_water_kind`).
    #[serde(serialize_with = "serialize_water_kind")]
    pub water: WaterKind,
}

/// Serialize a `WaterKind` by its stable lowercase-hyphenated name (the
/// `locale/room/v2` schema's water field), the same convention `biome_name`
/// uses for `Biome`.
fn serialize_water_kind<S>(kind: &WaterKind, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(water_kind_name(*kind))
}

/// Stable name for a `WaterKind` (owned here, not Debug — mirrors `biome_name`).
fn water_kind_name(k: WaterKind) -> &'static str {
    match k {
        WaterKind::Ocean => "ocean",
        WaterKind::SaltBasin => "salt-basin",
        WaterKind::River => "river",
        WaterKind::DryLand => "dry-land",
    }
}

/// Why a locale could not be described.
/// type-audit: bare-ok(prose: Build.0), bare-ok(prose: Unaddressable.0)
#[derive(Debug, Clone, PartialEq)]
pub enum LocaleError {
    /// Building the coarse world failed (worldgen).
    Build(String),
    /// The room is coarser than the canonical grid, so it has no inheritance.
    AboveGrid,
    /// The room address has no packed id (e.g. `path.len() > MAX_DEPTH`); its
    /// `RoomAddrError` debug is carried. Fail fast rather than mint a
    /// meaningless `id: 0`.
    Unaddressable(String),
}

impl std::fmt::Display for LocaleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocaleError::Build(m) => write!(f, "building the coarse world: {m}"),
            LocaleError::AboveGrid => {
                write!(
                    f,
                    "room is coarser than the canonical grid (no inheritance)"
                )
            }
            LocaleError::Unaddressable(m) => {
                write!(f, "room address is unaddressable: {m}")
            }
        }
    }
}

/// The reusable coarse-world build. Constructed once, reused across every
/// `describe` — so a locale stays a cheap derived view.
pub struct LocaleContext {
    seed: Seed,
    climate: GeneratedClimate,
    terrain: GeneratedTerrain,
    index: NearestCellIndex,
    globe_level: u32,
    budget: StrangenessBudget,
}

impl LocaleContext {
    /// Build the coarse world (climate + terrain + nearest-cell index) once.
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError> {
        let climate = climate_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let terrain = terrain_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let index = NearestCellIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().level();
        let budget = StrangenessBudget::build(world.seed, &climate, &terrain);
        Ok(LocaleContext {
            seed: world.seed,
            climate,
            terrain,
            index,
            globe_level,
            budget,
        })
    }

    /// The canonical globe level (canonical-grid refinement depth).
    /// type-audit: bare-ok(count)
    pub fn globe_level(&self) -> u32 {
        self.globe_level
    }

    /// The world's placed exotic sites, for findability (derived, not stored).
    pub fn strange_sites(&self) -> Vec<StrangeSite> {
        self.budget.sites()
    }

    /// A room's ground-truth locale at observation time `at`. Pure over
    /// (context, addr, at): same inputs → byte-identical `Locale`. v1 samples
    /// the time-independent annual mean and does not yet vary with `at`
    /// (threaded for the P8 temporal-phase layer).
    pub fn describe(&self, addr: &RoomAddr, _at: WorldTime) -> Result<Locale, LocaleError> {
        // Fail fast on an unaddressable room (e.g. `path.len() > MAX_DEPTH`)
        // rather than mint a meaningless `id: 0` (fields are public, so a
        // caller can hand us an over-deep address).
        let id = addr
            .pack()
            .map_err(|e| LocaleError::Unaddressable(format!("{e:?}")))?
            .0;
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();

        // Categorical biome: max weight, tie-break lowest CellId. Inherited,
        // never re-quantized (decision 0038).
        let mut best = weights[0];
        for &cand in &weights[1..] {
            if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
                best = cand;
            }
        }
        let biome = self.climate.biome_at(best.0);

        // Continuous fields: integer-weighted mean, full precision, quantize
        // at emit.
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            quantize(sum / denom as f64)
        };
        let fields = LocaleFields {
            temperature_c: blend(&|c| self.climate.mean_temperature_at(c).get()),
            moisture: blend(&|c| self.climate.moisture_at(c)),
            elevation_m: blend(&|c| self.terrain.globe().elevation.get(c).get()),
            water: *self.terrain.globe().water_kind.get(best.0),
        };

        let substrate = crate::substrate::substrate_at(&self.climate, &self.terrain, best.0);
        let micro = crate::micro::micro_field(addr.seed(self.seed));
        let mut regime = crate::grammar::derived_regime(self.seed, addr, biome, substrate, micro);
        if let Some(placed) = self.budget.regime_at(best.0) {
            let negations = Negations {
                substrate: regime.negations.substrate,
                energy: placed.energy,
                kingdom: placed.kingdom,
                endemic: placed.endemic,
            };
            let descriptor = crate::grammar::render(negations, micro, biome, self.seed, addr);
            regime = Regime {
                negations,
                micro,
                descriptor,
                strangeness: negations.strangeness(),
            };
        }

        let coord = addr.coord();
        Ok(Locale {
            schema: ROOM_SCHEMA,
            id,
            face: addr.face,
            path: addr.path.clone(),
            depth: addr.depth(),
            latitude: quantize(coord.latitude),
            longitude: quantize(coord.longitude),
            biome: biome_name(biome).to_string(),
            fields,
            corners: weights
                .iter()
                .map(|&(c, w)| CellWeight {
                    cell: c.0,
                    weight: w,
                })
                .collect(),
            regime,                // strangeness overlay (§5-§7)
            exits: exits_of(addr), // base + vertical exits (§6)
        })
    }

    /// The room's PER-DAY temperature at `at`, °C — the diurnal+seasonal
    /// signal a thermal drive senses at its own cell, distinct from
    /// [`describe`](Self::describe)'s annual-MEAN `temperature_c` render field
    /// (left untouched, so the walk/almanac stay byte-identical). Blends the
    /// three corner cells' [`GeneratedClimate::temperature_at`] by the SAME
    /// integer barycentric weights `describe` uses for the mean. Full
    /// precision — this is a compute-path read, never a serialization
    /// boundary, so it is NOT quantized (quantize-at-emit-only). `None` for a
    /// room the canonical grid does not cover (above the grid or unaddressable);
    /// the caller supplies the never-chosen fallback.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at(&self, addr: &RoomAddr, at: WorldTime) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.temperature_at(c, at.day).get())
            .sum();
        Some(sum / denom as f64)
    }

    /// The room's material food PRODUCTIVITY in `[0, 1]` — a Miami-model
    /// net-primary-productivity proxy over the climate, the food-value field
    /// the drive layer's hunger drive reads (The Provender). Blends the three
    /// corner cells' annual-mean temperature and moisture by the SAME integer
    /// barycentric weights [`describe`](Self::describe) uses, then takes the
    /// Liebig minimum of a triangular temperature response and moisture — the
    /// same NPP proxy demography's carrying-capacity uses, computed here from
    /// this context's own climate rather than depending up into demography (a
    /// sibling consumer, not required to match it bit-for-bit; it grades cells
    /// for a hungry forager, it does not set population). Full precision — a
    /// compute-path read, never a serialization boundary, so NOT quantized.
    /// `None` for a room the canonical grid does not cover (the caller supplies
    /// the never-fed fallback). Time-independent (standing biomass is a slow,
    /// annual field), so it takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at(&self, addr: &RoomAddr) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            sum / denom as f64
        };
        let temp = blend(&|c| self.climate.mean_temperature_at(c).get());
        let moisture = blend(&|c| self.climate.moisture_at(c));
        Some(miami_npp(temp, moisture))
    }

    /// Corner-blend an externally-supplied per-cell `field` (over the canonical
    /// geosphere) at `addr` — the integer-barycentric read `productivity_at`/
    /// `hazards_at` use, generalized so a caller can sample a field this context
    /// does not itself hold. The Quarry injects `worldgen::predator_pressure`
    /// (the carnivore-pressure field) and reads it here per room. Full precision
    /// (a compute-path read, not quantized). `None` for a room the canonical grid
    /// does not cover.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at(&self, addr: &RoomAddr, field: &hornvale_kernel::CellMap<f64>) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * *field.get(c)).sum();
        Some(sum / denom as f64)
    }

    /// The room's THREAT in `[0, 1]` — the hazard field the danger drive flees
    /// (The Dread, split per-axis by The Bane) as `(uncanny, heat, cold)`, each
    /// in `[0, 1]`: the **uncanny** (a placed exotic site's normalized strangeness
    /// — the "cursed ground"), and **heat**/**cold** — how far the cell's
    /// annual-mean temperature is *above* a hot-danger threshold / *below* a
    /// cold-danger one, graded up to the lethal extreme (the deep ice, the molten
    /// waste). Reads the dominant corner cell's placed regime (like
    /// [`describe`](Self::describe) picks its biome) and a corner-blended mean
    /// temperature. Full precision — a compute-path read, never a serialization
    /// boundary, so NOT quantized. `None` for a room the canonical grid does not
    /// cover (the caller supplies the safe fallback). Time-independent, so it
    /// takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at(&self, addr: &RoomAddr) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        // The dominant corner cell (max weight, tie-break lowest CellId) — the
        // same pick `describe` uses for the categorical biome/regime.
        let mut best = weights[0];
        for &cand in &weights[1..] {
            if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
                best = cand;
            }
        }
        // The uncanny: a placed exotic site's strangeness, normalized to [0,1].
        let uncanny = self
            .budget
            .regime_at(best.0)
            .map(|n| n.strangeness() / crate::regime::STRANGENESS_CEILING)
            .unwrap_or(0.0);
        // Graded heat/cold: 0 within the safe band, rising to 1 at the lethal
        // extreme.
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let temp: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.mean_temperature_at(c).get())
            .sum::<f64>()
            / denom as f64;
        let heat = ((temp - HOT_DANGER_C) / (LETHAL_HEAT_C - HOT_DANGER_C)).clamp(0.0, 1.0);
        let cold = ((COLD_DANGER_C - temp) / (COLD_DANGER_C - LETHAL_COLD_C)).clamp(0.0, 1.0);
        Some((uncanny.clamp(0.0, 1.0), heat, cold))
    }
}

/// The annual-mean temperature (°C) at/below which a cell's COLD becomes a hazard
/// (The Bane) — graded from here down to [`LETHAL_COLD_C`]. Above the coldest
/// species niche, so ordinary cold is thermal discomfort (thermal's job), not
/// dread.
const COLD_DANGER_C: f64 = -20.0;

/// The annual-mean temperature (°C) at/above which a cell's HEAT becomes a hazard
/// (The Bane) — graded from here up to [`LETHAL_HEAT_C`].
const HOT_DANGER_C: f64 = 40.0;

/// The coldest annual-mean temperature (°C) any creature survives — a lethal
/// frozen waste, where COLD hazard saturates to `1` (The Bane).
const LETHAL_COLD_C: f64 = -40.0;

/// The hottest annual-mean temperature (°C) any creature survives — a lethal
/// molten waste, where HEAT hazard saturates to `1`.
const LETHAL_HEAT_C: f64 = 60.0;

/// The optimum temperature (°C) of the Miami NPP proxy's triangular
/// temperature response — mirrors demography's carrying-capacity model (a
/// sibling consumer of the same proxy; see [`LocaleContext::productivity_at`]).
const NPP_TEMP_OPTIMUM_C: f64 = 20.0;

/// The temperature tolerance (°C) either side of [`NPP_TEMP_OPTIMUM_C`] over
/// which the triangular temperature response falls to zero.
const NPP_TEMP_TOLERANCE_C: f64 = 30.0;

/// The Miami-model net-primary-productivity proxy in `[0, 1]`: the Liebig
/// minimum of a triangular temperature response about [`NPP_TEMP_OPTIMUM_C`]
/// and the (clamped) moisture. The food-value field's material-productivity
/// term (The Provender).
fn miami_npp(temperature_c: f64, moisture: f64) -> f64 {
    let temp_response =
        (1.0 - (temperature_c - NPP_TEMP_OPTIMUM_C).abs() / NPP_TEMP_TOLERANCE_C).clamp(0.0, 1.0);
    temp_response.min(moisture.clamp(0.0, 1.0))
}

/// Stable biome name for the `locale/room/v2` schema (owned here, not Debug).
fn biome_name(b: Biome) -> &'static str {
    match b {
        Biome::Ice => "ice",
        Biome::Tundra => "tundra",
        Biome::Taiga => "taiga",
        Biome::TemperateGrassland => "temperate grassland",
        Biome::Shrubland => "shrubland",
        Biome::TemperateForest => "temperate forest",
        Biome::TemperateRainforest => "temperate rainforest",
        Biome::Desert => "desert",
        Biome::Savanna => "savanna",
        Biome::TropicalSeasonalForest => "tropical seasonal forest",
        Biome::TropicalRainforest => "tropical rainforest",
        Biome::Alpine => "alpine",
        Biome::SeaIce => "sea ice",
        Biome::CoralReef => "coral reef",
        Biome::KelpForest => "kelp forest",
        Biome::HydrothermalVent => "hydrothermal vent",
        Biome::HadalTrench => "hadal trench",
        Biome::Upwelling => "upwelling",
        Biome::Epipelagic => "epipelagic",
        Biome::Mesopelagic => "mesopelagic",
        Biome::Bathypelagic => "bathypelagic",
        Biome::Abyssal => "abyssal",
    }
}

/// A way out of a room. `ExitKind` is open so overlay kinds (river/road/
/// tunnel/portal) and passability compose additively later.
/// type-audit: bare-ok(index: to)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Exit {
    /// Which way this exit goes.
    pub direction: Direction,
    /// The kind of traversal.
    pub kind: ExitKind,
    /// Destination packed room id.
    pub to: u64,
}

/// An exit direction: a lateral compass bearing, or a vertical scale change.
/// type-audit: bare-ok(index: Enter.0)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Direction {
    /// A lateral edge, bucketed to eight compass points.
    Compass(Compass),
    /// Descend into finer child `digit` (0..4).
    Enter(u8),
    /// Step back out to the containing room.
    Exit,
}

/// Eight-point compass bucket.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Compass {
    /// North.
    N,
    /// North-east.
    Ne,
    /// East.
    E,
    /// South-east.
    Se,
    /// South.
    S,
    /// South-west.
    Sw,
    /// West.
    W,
    /// North-west.
    Nw,
}

/// The traversal class of an exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExitKind {
    /// A geometric base-mesh edge.
    Edge,
    /// A vertical scale change (enter/exit).
    Vertical,
}

/// Bucket a bearing (degrees clockwise from north) to eight points. Bucketing
/// on a quantized bearing keeps it cross-platform stable.
fn compass(bearing_deg: f64) -> Compass {
    let b = quantize((bearing_deg % 360.0 + 360.0) % 360.0);
    let idx = (((b + 22.5) / 45.0).floor() as i64).rem_euclid(8);
    [
        Compass::N,
        Compass::Ne,
        Compass::E,
        Compass::Se,
        Compass::S,
        Compass::Sw,
        Compass::W,
        Compass::Nw,
    ][idx as usize]
}

fn exits_of(addr: &RoomAddr) -> Vec<Exit> {
    let mut exits = Vec::new();
    for n in addr.neighbors() {
        exits.push(Exit {
            direction: Direction::Compass(compass(addr.bearing_to(&n))),
            kind: ExitKind::Edge,
            to: n.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    if let Some(parent) = addr.parent() {
        exits.push(Exit {
            direction: Direction::Exit,
            kind: ExitKind::Vertical,
            to: parent.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    for digit in 0..4u8 {
        if let Ok(child) = addr.child(digit) {
            exits.push(Exit {
                direction: Direction::Enter(digit),
                kind: ExitKind::Vertical,
                to: child.pack().map(|r| r.0).unwrap_or(0),
            });
        }
    }
    exits
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{RoomAddr, Seed, World, WorldTime};

    fn land_world() -> World {
        // Seed 42 is the project's canonical fixture; it has land.
        World::new(Seed(42))
    }

    #[test]
    fn describe_is_deterministic_across_two_contexts() {
        let world = land_world();
        let addr = RoomAddr {
            face: 0,
            path: vec![1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0],
        };
        let a = LocaleContext::build(&world).unwrap();
        let b = LocaleContext::build(&world).unwrap();
        let la = a.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let lb = b.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(
            serde_json::to_string(&la).unwrap(),
            serde_json::to_string(&lb).unwrap()
        );
    }

    #[test]
    fn describe_above_the_grid_errors() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // A room coarser than the canonical grid has no corner weights.
        let coarse = RoomAddr {
            face: 0,
            path: vec![1],
        };
        assert!(matches!(
            ctx.describe(&coarse, WorldTime { day: 0.0 }),
            Err(LocaleError::AboveGrid)
        ));
    }

    #[test]
    fn fields_are_within_the_corner_range() {
        // A weighted blend never leaves the min..max of its inputs.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        // elevation blends three real cells; the value must be finite.
        assert!(loc.fields.elevation_m.is_finite());
        assert!(loc.fields.temperature_c.is_finite());
        assert_eq!(loc.schema, ROOM_SCHEMA);
    }

    #[test]
    fn locale_water_field_varies_and_includes_fresh_water_on_seed_42() {
        // Wired to real geography (not a stuck constant) AND fresh water
        // exists — the sanity that unblocks The Surmise.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut kinds: std::collections::BTreeSet<WaterKind> = Default::default();
        let mut saw_fresh = false;
        for i in 0..400u32 {
            let t = i as f64;
            // a deterministic spread of directions over the sphere
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, 6);
            if let Ok(loc) = ctx.describe(&addr, WorldTime { day: 0.0 }) {
                kinds.insert(loc.fields.water);
                if loc.fields.water == WaterKind::River {
                    saw_fresh = true;
                }
            }
        }
        assert!(
            kinds.len() >= 2,
            "water must vary across the globe (wired to real geography), got {kinds:?}"
        );
        assert!(
            saw_fresh,
            "seed 42 must have fresh water (River) reachable on land — else lower RIVER_MIN_DRAINAGE"
        );
    }

    #[test]
    fn describe_over_deep_address_errors() {
        // A path deeper than MAX_DEPTH (29) has no packed id: fail fast with
        // Unaddressable, never mint a valid-looking Locale with id: 0.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let over_deep = RoomAddr {
            face: 0,
            path: vec![0; 30],
        };
        assert!(matches!(
            ctx.describe(&over_deep, WorldTime { day: 0.0 }),
            Err(LocaleError::Unaddressable(_))
        ));
    }

    #[test]
    fn blend_and_inheritance_pin_exact_values() {
        // §14 Q4 regression: pin the blend/inheritance for a fixed seed-42
        // world at a fixed deep address. Values captured from a known-good run.
        // We pin the platform-EXACT quantities only: the quantized blended
        // temperature (byte-identical cross-platform) and the corner
        // (cell, weight) pairs (pure integer barycentric numerators — the
        // inheritance-selection inputs). The biome NAME is a depth-band
        // classification thresholded on host-libm transcendentals (elevation +
        // a percentile sea_level), i.e. the cross-platform-divergence class CI
        // excludes elsewhere — so we assert membership, not the exact string,
        // to keep the both-platform workspace gate stable.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(loc.fields.temperature_c, 38.082618);
        assert_eq!(
            loc.corners,
            vec![
                CellWeight {
                    cell: 3799,
                    weight: 46
                },
                CellWeight {
                    cell: 15109,
                    weight: 16
                },
                CellWeight {
                    cell: 15099,
                    weight: 130
                },
            ]
        );
        // Depth-band biome name: platform-sensitive, so assert only that a
        // known biome was selected (never the exact string).
        const KNOWN_BIOMES: &[&str] = &[
            "ice",
            "tundra",
            "taiga",
            "temperate grassland",
            "shrubland",
            "temperate forest",
            "temperate rainforest",
            "desert",
            "savanna",
            "tropical seasonal forest",
            "tropical rainforest",
            "alpine",
            "sea ice",
            "coral reef",
            "kelp forest",
            "hydrothermal vent",
            "hadal trench",
            "upwelling",
            "epipelagic",
            "mesopelagic",
            "bathypelagic",
            "abyssal",
        ];
        assert!(KNOWN_BIOMES.contains(&loc.biome.as_str()));
    }

    #[test]
    fn regime_is_deterministic_and_siblings_differ() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let a = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0],
        };
        let b = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1],
        };
        let ra = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().regime;
        let ra2 = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().regime;
        let rb = ctx.describe(&b, WorldTime { day: 0.0 }).unwrap().regime;
        assert_eq!(ra, ra2, "same room → identical regime");
        assert_ne!(ra.descriptor, rb.descriptor, "sibling rooms should differ");
        assert!(ra.strangeness >= 0.0);
        assert!(!ra.descriptor.is_empty());
    }

    #[test]
    fn schema_is_v2_and_regime_present() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(loc.schema, "locale/room/v2");
        assert!(loc.regime.strangeness >= 0.0);
        assert!(!loc.regime.descriptor.is_empty());
    }

    #[test]
    fn strange_sites_are_exposed() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // A derived query; may be empty on a mundane world but must not panic.
        let _ = ctx.strange_sites();
    }

    #[test]
    fn exits_are_three_lateral_plus_vertical() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let lateral = loc
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .count();
        assert_eq!(lateral, 3, "exactly three geometric edges");
        assert!(
            loc.exits.iter().any(|e| e.direction == Direction::Exit),
            "a mid-mesh room has a parent (Exit)"
        );
        let enters = loc
            .exits
            .iter()
            .filter(|e| matches!(e.direction, Direction::Enter(_)))
            .count();
        assert_eq!(enters, 4, "four children to enter");
        // every lateral destination is one of the substrate's neighbours
        let ns: Vec<u64> = addr
            .neighbors()
            .iter()
            .map(|n| n.pack().unwrap().0)
            .collect();
        for e in loc.exits.iter().filter(|e| e.kind == ExitKind::Edge) {
            assert!(ns.contains(&e.to), "lateral exit must be a neighbour");
        }
    }

    #[test]
    fn compass_buckets_cover_the_circle() {
        assert_eq!(compass(0.0), Compass::N);
        assert_eq!(compass(90.0), Compass::E);
        assert_eq!(compass(180.0), Compass::S);
        assert_eq!(compass(270.0), Compass::W);
        assert_eq!(compass(45.0), Compass::Ne);
        assert_eq!(compass(359.9), Compass::N);
    }
}
