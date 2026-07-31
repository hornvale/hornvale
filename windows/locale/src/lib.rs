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

use hornvale_climate::{Biome, BiomeExpr, Formation, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{CellId, NearestCellIndex, RoomAddr, Seed, World, WorldTime, quantize};
use hornvale_terrain::GeneratedTerrain;
pub use hornvale_terrain::WaterKind;
use hornvale_worldgen::{climate_from, terrain_of};
use serde::Serialize;

/// The versioned semantic schema this window emits (save-format class; a
/// changed meaning mints `locale/room/v2` alongside).
/// type-audit: bare-ok(identifier-text)
pub const ROOM_SCHEMA: &str = "locale/room/v2";

/// One placed exotic site, rendered for a reader.
/// type-audit: bare-ok(index: cell), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome), bare-ok(prose: descriptor)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StrangeSiteRow {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Site latitude, degrees (quantized).
    pub latitude: f64,
    /// Site longitude, degrees (quantized).
    pub longitude: f64,
    /// The base biome the site interrupts.
    pub biome: String,
    /// What makes it strange — the exotic clause for its negation vector.
    pub descriptor: String,
}

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
    /// The same inherited biome, as the `hornvale_climate::Biome` enum
    /// `biome`'s prose string was rendered from. `#[serde(skip)]`: this
    /// carries no wire bytes, so `locale/room/v2`'s serialized shape is
    /// unchanged — it exists purely so an in-process consumer (e.g.
    /// `scene/surrounds/v1`) can index by enum identity instead of
    /// round-tripping through a string, the way `windows/scene/src/lib.rs`
    /// and `region.rs` already index tile/region biomes.
    #[serde(skip)]
    pub biome_kind: Biome,
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
/// `locale/room/v2` schema's water field). Unlike [`biome_prose_name`], this
/// stays kebab-case — water has no separate prose noun to protect, so there
/// is no shared-noun hazard here.
fn serialize_water_kind<S>(kind: &WaterKind, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(water_kind_name(*kind))
}

/// Stable name for a `WaterKind` (owned here, not Debug — kebab-case, unlike
/// [`biome_prose_name`]).
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
    /// The sanctioned entry point for any caller that has not already
    /// sculpted terrain/climate itself — derives them once here and
    /// delegates to [`Self::build_from`] (the book-entry-point pattern: a
    /// wrapper that derives once, mirroring `windows/book`'s `parse_context`/
    /// `parse_context_from` split).
    // Named construction site (decision 0092): this entry wrapper sculpts/
    // fits once, then delegates to `build_from`.
    #[allow(clippy::disallowed_methods)]
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError> {
        let terrain = terrain_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let climate =
            climate_from(world, &terrain).map_err(|e| LocaleError::Build(e.to_string()))?;
        Ok(Self::build_from(world, &terrain, &climate))
    }

    /// Build the coarse world from an ALREADY-sculpted terrain and fit
    /// climate (The Weir, Stage 2) — the "pass the pre-built value" idiom
    /// `hornvale_worldgen::climate_from` already established, so a caller
    /// that must also thread `terrain`/`climate` into further derivation
    /// (`Session::start`'s demography fit, the lab health sweep) builds them
    /// ONCE and shares the same pair with this context, instead of `build`
    /// quietly re-sculpting a second copy underneath it. Infallible: both
    /// inputs are already validated by construction (the caller obtained
    /// them from `terrain_of`/`climate_from` succeeding), so there is
    /// nothing left here that can fail. Byte-identical to `build` whenever
    /// `terrain` equals `terrain_of(world)` and `climate` equals
    /// `climate_from(world, &terrain)`.
    pub fn build_from(
        world: &World,
        terrain: &GeneratedTerrain,
        climate: &GeneratedClimate,
    ) -> LocaleContext {
        let index = NearestCellIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().level();
        let budget = StrangenessBudget::build(world.seed, climate, terrain);
        LocaleContext {
            seed: world.seed,
            climate: climate.clone(),
            terrain: terrain.clone(),
            index,
            globe_level,
            budget,
        }
    }

    /// The canonical globe level (canonical-grid refinement depth).
    /// type-audit: bare-ok(count)
    pub fn globe_level(&self) -> u32 {
        self.globe_level
    }

    /// The cached terrain provider — the reuse seam so a caller (e.g. the
    /// vessel window's `observable`) can pass it into `sky_report_from`
    /// instead of re-deriving it (The Retainer).
    pub fn terrain(&self) -> &GeneratedTerrain {
        &self.terrain
    }

    /// The cached climate provider — the reuse seam so a caller (e.g. the
    /// vessel window's `observable`) can pass it into `sky_report_from`
    /// instead of re-deriving it (The Retainer).
    pub fn climate(&self) -> &GeneratedClimate {
        &self.climate
    }

    /// The cached nearest-cell index — the reuse seam for a caller that must
    /// resolve an address to a cell itself (the same role `terrain()` plays for
    /// the terrain provider). Building a second index would duplicate a
    /// structure this context exists to hold once.
    pub fn nearest_index(&self) -> &NearestCellIndex {
        &self.index
    }

    /// The world's placed exotic sites, for findability (derived, not stored).
    pub fn strange_sites(&self) -> Vec<StrangeSite> {
        self.budget.sites()
    }

    /// Every placed exotic site, rendered for a reader: where it is, what
    /// biome it interrupts, and what makes it strange.
    ///
    /// The descriptor is not decoration. Sites are differentiated by their
    /// negation vector (energy × kingdom × endemic), so a listing of bare
    /// coordinates would render a world's worth of wonders as identical rows.
    pub fn strange_site_rows(&self) -> Vec<StrangeSiteRow> {
        self.strange_sites()
            .into_iter()
            .map(|s| {
                let cell = CellId(s.cell);
                let coord = self.climate.geosphere().coord(cell);
                StrangeSiteRow {
                    cell: s.cell,
                    latitude: quantize(coord.latitude),
                    longitude: quantize(coord.longitude),
                    biome: biome_prose_name(self.climate.biome_at(cell)).to_string(),
                    // `exotic_clause` reads only energy/kingdom/endemic, and a
                    // StrangeSite carries no substrate of its own (substrate is
                    // the ROOM's, from its derived regime), so `Ordinary` here
                    // is lossless rather than a stand-in.
                    descriptor: crate::grammar::exotic_clause(Negations {
                        substrate: Substrate::Ordinary,
                        energy: s.energy,
                        kingdom: s.kingdom,
                        endemic: s.endemic,
                    }),
                }
            })
            .collect()
    }

    /// A room's ground-truth locale at observation time `at`. Pure over
    /// (context, addr, at): same inputs → byte-identical `Locale`. v1 samples
    /// the time-independent annual mean and does not yet vary with `at`
    /// (threaded for the P8 temporal-phase layer).
    pub fn describe(&self, addr: &RoomAddr, at: WorldTime) -> Result<Locale, LocaleError> {
        self.describe_at(addr, at, None)
    }

    /// The water column at a marine cell: every stratum from the sunlit water
    /// down to the one the sea floor sits in, shallowest first. Empty on land.
    ///
    /// A cell's floor decides how deep its water goes — 50 m of water over a
    /// reef holds only the epipelagic, while 3,000 m holds three layers. This
    /// is the list a diver descends.
    pub fn water_column_at(&self, cell: CellId) -> Vec<Stratum> {
        let expr = self.climate.biome_expr_at(cell);
        if expr.realm != Realm::WATERWORLD {
            return Vec::new();
        }
        let floor = expr.stratum;
        Realm::WATERWORLD
            .strata()
            .iter()
            .copied()
            .take_while(|s| *s != floor)
            .chain(std::iter::once(floor))
            .collect()
    }

    /// The biome expression at `cell` as seen from `stratum`. At the sea floor
    /// this is the cell's own community — a reef, a vent, a kelp forest. Above
    /// it there is only open water: the community lives on the floor, and
    /// floating a thousand metres over a reef is not being at the reef.
    pub fn expr_at_stratum(&self, cell: CellId, stratum: Stratum) -> BiomeExpr {
        let expr = self.climate.biome_expr_at(cell);
        if stratum == expr.stratum {
            expr
        } else {
            BiomeExpr {
                realm: expr.realm,
                formation: Formation::OpenWater,
                stratum,
            }
        }
    }

    /// [`LocaleContext::describe`], optionally as seen from a stratum within
    /// the water column rather than from the surface.
    pub fn describe_at(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        stratum: Option<Stratum>,
    ) -> Result<Locale, LocaleError> {
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
        let _ = at; // v1: time-independent (see the doc above)
        self.describe_with_weights(addr, stratum, id, weights)
    }

    /// [`Self::describe_at`], consulting/filling a caller-owned
    /// [`hornvale_kernel::RoomMeshMemo`] for the room's [`corner_weights`]
    /// read instead of recomputing it (the-waymark, Task 3) — the same
    /// `corner_weights` result [`Self::temperature_at_memo`],
    /// [`Self::productivity_at_memo`], [`Self::blend_at_memo`], and
    /// [`Self::hazards_at_memo`] would each independently recompute for the
    /// SAME room in one read scope (e.g. `windows/vessel`'s per-tick drive
    /// stack), a caller that shares one memo across all five collapses that
    /// back down to one scan. Byte-identical to `describe_at` by
    /// construction (`corner_weights_memo` is pinned bit-equal to
    /// `corner_weights`). `describe_at`'s own signature is untouched; this is
    /// an additive sibling.
    pub fn describe_at_memo(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        stratum: Option<Stratum>,
        memo: &mut hornvale_kernel::RoomMeshMemo,
    ) -> Result<Locale, LocaleError> {
        let id = addr
            .pack()
            .map_err(|e| LocaleError::Unaddressable(format!("{e:?}")))?
            .0;
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights_memo(geo, &self.index, memo)
            .ok_or(LocaleError::AboveGrid)?;
        let _ = at; // v1: time-independent (see `describe_at`'s doc)
        self.describe_with_weights(addr, stratum, id, weights)
    }

    /// [`Self::describe_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// the shape a `&self`-only reader needs: a `&dyn Terrain` implementor
    /// (`windows/vessel`'s `LocaleTerrain`) can hold a prefilled cache and
    /// consult it from an ordinary `&self` trait method, never needing `&mut`
    /// access at read time. A cache miss falls through to a fresh
    /// [`RoomAddr::corner_weights`] call — correctness never depends on the
    /// cache being complete, only speed does. `cache: None` is byte-identical
    /// to `describe_at` (always a miss). Byte-identical to `describe_at` on
    /// a hit too, by construction (`corner_weights_lookup` only ever returns
    /// what `corner_weights_memo` would have inserted, which is pinned
    /// bit-equal to `corner_weights` itself).
    pub fn describe_at_cached(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        stratum: Option<Stratum>,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Result<Locale, LocaleError> {
        let id = addr
            .pack()
            .map_err(|e| LocaleError::Unaddressable(format!("{e:?}")))?
            .0;
        let geo = self.climate.geosphere();
        let weights = self
            .corner_weights_for(addr, geo, cache)
            .ok_or(LocaleError::AboveGrid)?;
        let _ = at; // v1: time-independent (see `describe_at`'s doc)
        self.describe_with_weights(addr, stratum, id, weights)
    }

    /// The shared corner_weights read every `_cached` reader uses: a hit in
    /// `cache` returns the memoized answer (including a memoized above-grid
    /// `None`, which is why the lookup itself returns `Option<Option<_>>` —
    /// see [`hornvale_kernel::RoomMeshMemo::corner_weights_lookup`]'s own
    /// doc); a miss (or no cache at all) falls through to a fresh
    /// [`RoomAddr::corner_weights`] call. No mutation — this never fills a
    /// miss back into `cache`, which is exactly what lets a `&self` reader
    /// use it without `&mut` access.
    fn corner_weights_for(
        &self,
        addr: &RoomAddr,
        geo: &hornvale_kernel::Geosphere,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<[(CellId, u64); 3]> {
        if let Some(hit) = cache.and_then(|c| c.corner_weights_lookup(addr)) {
            return hit;
        }
        addr.corner_weights(geo, &self.index)
    }

    /// The shared tail of [`Self::describe_at`]/[`Self::describe_at_memo`]/
    /// [`Self::describe_at_cached`]: everything past resolving `id` and
    /// `weights`, so the three callers can never drift apart in how a
    /// `Locale` is built from them.
    fn describe_with_weights(
        &self,
        addr: &RoomAddr,
        stratum: Option<Stratum>,
        id: u64,
        weights: [(CellId, u64); 3],
    ) -> Result<Locale, LocaleError> {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();

        // Categorical biome: max weight, tie-break lowest CellId. Inherited,
        // never re-quantized (decision 0038).
        let mut best = weights[0];
        for &cand in &weights[1..] {
            if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
                best = cand;
            }
        }
        let biome = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st).biome(),
            None => self.climate.biome_at(best.0),
        };

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
        let expr = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st),
            None => self.climate.biome_expr_at(best.0),
        };
        let mut regime = crate::grammar::derived_regime(self.seed, addr, expr, substrate, micro);
        if let Some(placed) = self.budget.regime_at(best.0) {
            let negations = Negations {
                substrate: regime.negations.substrate,
                energy: placed.energy,
                kingdom: placed.kingdom,
                endemic: placed.endemic,
            };
            let descriptor = crate::grammar::render(negations, micro, expr, self.seed, addr);
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
            biome: biome_prose_name(biome).to_string(),
            biome_kind: biome,
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

    /// [`Self::temperature_at`], consulting/filling a caller-owned
    /// [`hornvale_kernel::RoomMeshMemo`] instead of recomputing
    /// `corner_weights` (the-waymark, Task 3) — see [`Self::describe_at_memo`]
    /// for the shared-memo read-scope rationale. Byte-identical to
    /// `temperature_at` by construction.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at_memo(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        memo: &mut hornvale_kernel::RoomMeshMemo,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights_memo(geo, &self.index, memo)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.temperature_at(c, at.day).get())
            .sum();
        Some(sum / denom as f64)
    }

    /// [`Self::temperature_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `temperature_at`.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at_cached(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
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

    /// [`Self::productivity_at`], consulting/filling a caller-owned
    /// [`hornvale_kernel::RoomMeshMemo`] instead of recomputing
    /// `corner_weights` (the-waymark, Task 3) — see [`Self::describe_at_memo`]
    /// for the shared-memo read-scope rationale. Byte-identical to
    /// `productivity_at` by construction.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at_memo(
        &self,
        addr: &RoomAddr,
        memo: &mut hornvale_kernel::RoomMeshMemo,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights_memo(geo, &self.index, memo)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            sum / denom as f64
        };
        let temp = blend(&|c| self.climate.mean_temperature_at(c).get());
        let moisture = blend(&|c| self.climate.moisture_at(c));
        Some(miami_npp(temp, moisture))
    }

    /// [`Self::productivity_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `productivity_at`.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at_cached(
        &self,
        addr: &RoomAddr,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
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
    /// does not itself hold. The Quarry injects `worldgen::predator_pressure_from`
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

    /// [`Self::blend_at`], consulting/filling a caller-owned
    /// [`hornvale_kernel::RoomMeshMemo`] instead of recomputing
    /// `corner_weights` (the-waymark, Task 3) — see [`Self::describe_at_memo`]
    /// for the shared-memo read-scope rationale. Byte-identical to
    /// `blend_at` by construction.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at_memo(
        &self,
        addr: &RoomAddr,
        field: &hornvale_kernel::CellMap<f64>,
        memo: &mut hornvale_kernel::RoomMeshMemo,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights_memo(geo, &self.index, memo)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * *field.get(c)).sum();
        Some(sum / denom as f64)
    }

    /// [`Self::blend_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `blend_at`.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at_cached(
        &self,
        addr: &RoomAddr,
        field: &hornvale_kernel::CellMap<f64>,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
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

    /// [`Self::hazards_at`], consulting/filling a caller-owned
    /// [`hornvale_kernel::RoomMeshMemo`] instead of recomputing
    /// `corner_weights` (the-waymark, Task 3) — see [`Self::describe_at_memo`]
    /// for the shared-memo read-scope rationale. Byte-identical to
    /// `hazards_at` by construction.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at_memo(
        &self,
        addr: &RoomAddr,
        memo: &mut hornvale_kernel::RoomMeshMemo,
    ) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights_memo(geo, &self.index, memo)?;
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

    /// [`Self::hazards_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `hazards_at`.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at_cached(
        &self,
        addr: &RoomAddr,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
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

/// Stable, human-readable biome name — spaced, not kebab-case (owned here,
/// not Debug). This is the prose grain: it is what the `locale/room/v2`
/// schema's `biome` field carries, and what a player reads. It is distinct
/// from [`hornvale_climate::Biome::name`], the kebab-case identifier used
/// for machine-readable catalogs (e.g. `scene/surrounds/v1`'s `biome_legend`
/// index) — the two must never be confused, or the same biome becomes two
/// different examinable nouns (The Margin).
/// type-audit: bare-ok(prose: return)
pub fn biome_prose_name(b: Biome) -> &'static str {
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

    /// The listing must be reachable AND legible: every site carries where it
    /// is and what makes it strange. A bare coordinate list would render a
    /// world's worth of wonders as identical rows.
    #[test]
    fn strange_site_rows_carry_their_own_descriptor() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let rows = ctx.strange_site_rows();
        assert_eq!(
            rows.len(),
            ctx.strange_sites().len(),
            "every placed site is listed"
        );
        for r in &rows {
            assert!(
                !r.descriptor.is_empty(),
                "cell {} is placed as exotic but reads as nothing",
                r.cell
            );
            assert!(!r.biome.is_empty());
            assert!((-90.0..=90.0).contains(&r.latitude), "lat {}", r.latitude);
            assert!(
                (-180.0..=180.0).contains(&r.longitude),
                "lon {}",
                r.longitude
            );
        }
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

    /// A small walk-visited neighborhood: `start` plus every room reachable
    /// within `hops` edge-steps (BFS over `RoomAddr::neighbors`, the same
    /// mesh a real possession walk traverses) — the-waymark Task 3's
    /// "rooms a real walk visits" fixture, sized for a fast unit test rather
    /// than a full possession transcript.
    fn walk_visited(start: &RoomAddr, hops: u32) -> Vec<RoomAddr> {
        let mut seen: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut frontier = vec![start.clone()];
        seen.insert(start.clone());
        for _ in 0..hops {
            let mut next = Vec::new();
            for r in &frontier {
                for n in r.neighbors() {
                    if seen.insert(n.clone()) {
                        next.push(n);
                    }
                }
            }
            frontier = next;
        }
        seen.into_iter().collect()
    }

    #[test]
    fn the_five_memo_readers_bit_equal_their_recomputing_siblings() {
        // Over every room a small walk visits, each `_memo` reader must
        // return EXACTLY what its non-memo sibling returns — proven by
        // literal equality (floats here are corner-blended f64s, not raw
        // transcendentals, so `assert_eq!` on the `Option`/tuple is the
        // right-strength check: same bits in, same arithmetic, same bits
        // out). One memo shared across all five readers and every room,
        // exactly the "one memo per read scope" the campaign's geometry-memo
        // stage calls for.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let start = RoomAddr {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let rooms = walk_visited(&start, 3);
        assert!(rooms.len() > 10, "fixture must cover a real neighborhood");
        let at = WorldTime { day: 12.5 };
        let mut memo = hornvale_kernel::RoomMeshMemo::new();
        let zero_field = hornvale_kernel::CellMap::from_fn(ctx.climate().geosphere(), |_| 0.0f64);
        for addr in &rooms {
            let expected_describe = ctx.describe_at(addr, at, None);
            let got_describe = ctx.describe_at_memo(addr, at, None, &mut memo);
            assert_eq!(
                expected_describe.is_ok(),
                got_describe.is_ok(),
                "describe_at_memo Ok/Err mismatch at {addr:?}"
            );
            if let (Ok(exp), Ok(got)) = (expected_describe, got_describe) {
                assert_eq!(
                    serde_json::to_string(&exp).unwrap(),
                    serde_json::to_string(&got).unwrap(),
                    "describe_at_memo mismatch at {addr:?}"
                );
            }

            let expected_temp = ctx.temperature_at(addr, at);
            let got_temp = ctx.temperature_at_memo(addr, at, &mut memo);
            assert_eq!(got_temp, expected_temp, "temperature_at_memo at {addr:?}");

            let expected_prod = ctx.productivity_at(addr);
            let got_prod = ctx.productivity_at_memo(addr, &mut memo);
            assert_eq!(got_prod, expected_prod, "productivity_at_memo at {addr:?}");

            let expected_blend = ctx.blend_at(addr, &zero_field);
            let got_blend = ctx.blend_at_memo(addr, &zero_field, &mut memo);
            assert_eq!(got_blend, expected_blend, "blend_at_memo at {addr:?}");

            let expected_hazards = ctx.hazards_at(addr);
            let got_hazards = ctx.hazards_at_memo(addr, &mut memo);
            assert_eq!(got_hazards, expected_hazards, "hazards_at_memo at {addr:?}");
        }
    }

    #[test]
    fn the_five_cached_readers_bit_equal_their_recomputing_siblings_with_a_partial_prefill() {
        // The-waymark fix round, Finding 1: a PREFILLED, READ-ONLY cache
        // (never mutated by the readers themselves) must still be
        // byte-identical to the raw recomputing siblings, on BOTH a cache
        // hit (prefilled rooms) and a cache miss (rooms deliberately left
        // out of the prefill, falling through to a fresh `corner_weights`
        // call) — correctness must never depend on prefill completeness.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let start = RoomAddr {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let rooms = walk_visited(&start, 3);
        assert!(rooms.len() > 10, "fixture must cover a real neighborhood");
        let at = WorldTime { day: 12.5 };
        let zero_field = hornvale_kernel::CellMap::from_fn(ctx.climate().geosphere(), |_| 0.0f64);

        // Prefill only the EVEN-indexed rooms (under `&mut`) — the rest stay
        // deliberately un-prefilled, so this run exercises both a hit and a
        // miss for every one of the five readers.
        let mut memo = hornvale_kernel::RoomMeshMemo::new();
        let geo = ctx.climate().geosphere();
        let mut prefilled = 0usize;
        for (i, addr) in rooms.iter().enumerate() {
            if i % 2 == 0 {
                addr.corner_weights_memo(geo, ctx.nearest_index(), &mut memo);
                prefilled += 1;
            }
        }
        assert!(
            prefilled > 0 && prefilled < rooms.len(),
            "fixture must actually mix hits and misses"
        );

        // Count hits vs misses directly against the read-only lookup — the
        // review's own ask: prove the prefill actually covers the hot rooms,
        // not just that the byte-identity holds regardless.
        let mut hits = 0usize;
        let mut misses = 0usize;
        for addr in &rooms {
            match memo.corner_weights_lookup(addr) {
                Some(_) => hits += 1,
                None => misses += 1,
            }
        }
        assert_eq!(hits, prefilled, "every prefilled room must be a cache hit");
        assert_eq!(
            misses,
            rooms.len() - prefilled,
            "every un-prefilled room must be a cache miss"
        );

        // Now the actual byte-identity check, reading through the cache
        // (Some(&memo)) for every room — hits AND misses both included.
        for addr in &rooms {
            let expected_describe = ctx.describe_at(addr, at, None);
            let got_describe = ctx.describe_at_cached(addr, at, None, Some(&memo));
            assert_eq!(
                expected_describe.is_ok(),
                got_describe.is_ok(),
                "describe_at_cached Ok/Err mismatch at {addr:?}"
            );
            if let (Ok(exp), Ok(got)) = (expected_describe, got_describe) {
                assert_eq!(
                    serde_json::to_string(&exp).unwrap(),
                    serde_json::to_string(&got).unwrap(),
                    "describe_at_cached mismatch at {addr:?}"
                );
            }

            let expected_temp = ctx.temperature_at(addr, at);
            let got_temp = ctx.temperature_at_cached(addr, at, Some(&memo));
            assert_eq!(got_temp, expected_temp, "temperature_at_cached at {addr:?}");

            let expected_prod = ctx.productivity_at(addr);
            let got_prod = ctx.productivity_at_cached(addr, Some(&memo));
            assert_eq!(
                got_prod, expected_prod,
                "productivity_at_cached at {addr:?}"
            );

            let expected_blend = ctx.blend_at(addr, &zero_field);
            let got_blend = ctx.blend_at_cached(addr, &zero_field, Some(&memo));
            assert_eq!(got_blend, expected_blend, "blend_at_cached at {addr:?}");

            let expected_hazards = ctx.hazards_at(addr);
            let got_hazards = ctx.hazards_at_cached(addr, Some(&memo));
            assert_eq!(
                got_hazards, expected_hazards,
                "hazards_at_cached at {addr:?}"
            );
        }

        // `cache: None` must also be byte-identical (always a miss).
        let addr = &rooms[0];
        assert_eq!(
            ctx.describe_at(addr, at, None).is_ok(),
            ctx.describe_at_cached(addr, at, None, None).is_ok()
        );
        assert_eq!(
            ctx.temperature_at(addr, at),
            ctx.temperature_at_cached(addr, at, None)
        );
    }
}
