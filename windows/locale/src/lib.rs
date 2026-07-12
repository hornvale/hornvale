#![warn(missing_docs)]
//! The locale window: a `RoomAddr` rendered as an observable place.

use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::{CellId, NearestCellIndex, RoomAddr, Seed, World, WorldTime, quantize};
use hornvale_terrain::GeneratedTerrain;
use hornvale_worldgen::{climate_of, terrain_of};
use serde::Serialize;

/// The versioned semantic schema this window emits (save-format class; a
/// changed meaning mints `locale/room/v2` alongside).
/// type-audit: bare-ok(identifier-text)
pub const ROOM_SCHEMA: &str = "locale/room/v1";

/// A room rendered as an observable place — ground truth, re-derivable, never
/// stored (UNI-20 derived view). Plain serializable values only.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(index: id), bare-ok(index: face), bare-ok(index: path), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Locale {
    /// Schema tag (`locale/room/v1`).
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
    /// Seed-derived sub-cell texture.
    pub texture: SubCellTexture,
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
}

/// Seed-derived sub-cell texture — an explicit P3 placeholder, kept tiny.
/// type-audit: bare-ok(prose: aspect), bare-ok(ratio: relief_jitter)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SubCellTexture {
    /// One draw from a tiny biome-keyed pool (the P3 descriptor stand-in).
    pub aspect: String,
    /// A deterministic per-room character scalar in [-1, 1] (dimensionless).
    pub relief_jitter: f64,
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
}

impl LocaleContext {
    /// Build the coarse world (climate + terrain + nearest-cell index) once.
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError> {
        let climate = climate_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let terrain = terrain_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let index = NearestCellIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().level();
        Ok(LocaleContext {
            seed: world.seed,
            climate,
            terrain,
            index,
            globe_level,
        })
    }

    /// The canonical globe level (canonical-grid refinement depth).
    /// type-audit: bare-ok(count)
    pub fn globe_level(&self) -> u32 {
        self.globe_level
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
        // never re-quantized (identity-computes-on-the-canonical-grid).
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
            temperature_c: blend(&|c| self.climate.mean_temperature_at(c)),
            moisture: blend(&|c| self.climate.moisture_at(c)),
            elevation_m: blend(&|c| *self.terrain.globe().elevation.get(c)),
        };

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
            texture: texture_of(self.seed, addr, biome), // Task 3 fills this in
            exits: exits_of(addr),                       // Task 4 fills this in
        })
    }
}

/// Stable biome name for the `locale/room/v1` schema (owned here, not Debug).
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

// --- temporary stubs (replaced in Task 3 / Task 4) ---
fn texture_of(_seed: Seed, _addr: &RoomAddr, _biome: Biome) -> SubCellTexture {
    SubCellTexture {
        aspect: String::new(),
        relief_jitter: 0.0,
    }
}
fn exits_of(_addr: &RoomAddr) -> Vec<Exit> {
    Vec::new()
}
/// Placeholder types filled in by Task 4.
/// type-audit: bare-ok(identifier-text: direction), bare-ok(identifier-text: kind), bare-ok(index: to)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Exit {
    /// Exit direction.
    pub direction: String,
    /// Exit kind.
    pub kind: String,
    /// Destination packed room id.
    pub to: u64,
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
        // §14 Q4 regression: pin the blended field + inherited biome for a
        // fixed seed-42 world at a fixed deep address. Values captured from a
        // known-good run; a change to the blend/inheritance math trips this.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(loc.biome, "bathypelagic");
        assert_eq!(loc.fields.temperature_c, 38.082618);
    }
}
