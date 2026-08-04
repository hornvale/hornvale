//! Tier-1 metrics extractors: analyzable properties of generated worlds.

use hornvale_astronomy::{
    Calendar, NeighborClass, Rotation, StarSystem, streams::ROOT as ASTRONOMY_STREAM_ROOT,
};
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, EntityId, Phenomenon, Seed, Value, World};
use hornvale_language::{
    GapReason, LexEntry, Manner, NameKind, Namer, Phonology, Segment, concept_domain,
    distinctiveness, distortion, domain_distortion, recoverability, romanize,
};
use hornvale_religion::beliefs_of;
use hornvale_terrain::{
    CarveParams, Commodity, GlobeSummary, Hydro, MarginPolarity, RockClass, SoilOrder, fertility,
};
use hornvale_worldgen::{
    BuildDepth, BuildError, ChorusVoice, HazardKind, Sky, SkyChoice, Valence, WorldComponents,
    accounts_from, build_world_from_components, build_world_to_with_artifacts, climate_from,
    commodity_name, flagship_of, language_of_in, observed_phenomena_as_at_from,
    observed_phenomena_as_in_from, occupation_records, rock_class_name,
    settlement_site_concepts as worldgen_settlement_site_concepts, sky_of, soil_of,
    soil_order_name, terrain_of, vestiges_field,
};

use hornvale_astronomy::SkyPins;

/// A world and its derived astronomy/calendar/belief context.
/// type-audit: bare-ok(prose: notes)
pub struct WorldView {
    /// The world ledger.
    pub world: World,
    /// The star system, reconstructed or constant.
    pub system: StarSystem,
    /// The calendar, derived from the system.
    pub calendar: Calendar,
    /// Genesis notes recorded during sky generation.
    pub notes: Vec<String>,
    /// The tectonic globe summary (plates, ocean fraction, sea level, peak).
    pub globe: GlobeSummary,
    /// The full tectonic globe (for coverage metrics over cells).
    pub terrain: hornvale_terrain::GeneratedTerrain,
    /// The derived climate (biome + habitability).
    pub climate: GeneratedClimate,
    /// The species roster this view was built from (default = shipped).
    pub components: WorldComponents,
}

impl WorldView {
    /// Build a world view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<WorldView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build a world view with an explicit species roster (spec §3).
    // Named construction site (decision 0092): the lab's view-chain
    // composition root — sculpts/fits once per view build.
    #[allow(clippy::disallowed_methods)]
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<WorldView, BuildError> {
        let world = build_world_from_components(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
            &wc,
        )?;
        let sky = sky_of(&world)?;
        let Sky::Generated(sky) = sky else {
            return Err(BuildError::Pins(
                "expected Generated sky, got Constant".to_string(),
            ));
        };
        let terrain = terrain_of(&world)?;
        let globe = hornvale_terrain::summarize(terrain.globe());
        let climate = climate_from(&world, &terrain)?;
        Ok(WorldView {
            world,
            system: sky.system().clone(),
            calendar: sky.calendar().clone(),
            notes: sky.notes().to_vec(),
            globe,
            terrain,
            climate,
            components: wc,
        })
    }
}

// --- The narrowed view chain (spec §4 / MAP-25): AstronomyView ⊂
// TerrainView ⊂ ClimateView ⊂ SettlementView ⊂ FullView. Each deeper view
// *contains* the shallower one, so a coercion down the chain is a field
// borrow (no recompute), and each rung's constructor builds the world
// exactly once, at its own target depth, then reconstructs the cheap
// derived pieces (sky/terrain/climate) the same way `WorldView::build`
// always has. `WorldView` above is untouched; these live alongside it
// until a later task migrates extractors off it. ---

/// Astronomy rung: star system, calendar, genesis notes. The narrowest view
/// in the chain — every deeper rung coerces down to this one via `AsRef`,
/// so an astronomy-only metric (including the two that read only `roster`)
/// never pays for terrain/climate/settlement/full generation.
/// type-audit: bare-ok(prose: notes)
pub struct AstronomyView {
    /// The world ledger (astronomy-depth facts, or deeper if this view was
    /// reconstructed as part of a deeper rung's build).
    pub world: World,
    /// The reconstructed or constant star system.
    pub system: StarSystem,
    /// The calendar derived from the system.
    pub calendar: Calendar,
    /// Genesis notes recorded during sky generation.
    pub notes: Vec<String>,
    /// The species roster this view was built from (default = shipped).
    pub components: WorldComponents,
}

impl AstronomyView {
    /// Build an astronomy-rung view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<AstronomyView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build an astronomy-rung view with an explicit species roster.
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<AstronomyView, BuildError> {
        Self::build_to(seed, pins, wc, BuildDepth::Astronomy)
    }

    /// Build the world to `depth` and reconstruct the astronomy-rung fields.
    /// Deeper rungs call this directly with their own target depth so the
    /// world is built exactly once per view, never once per rung.
    fn build_to(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<AstronomyView, BuildError> {
        Self::build_to_with_artifacts(seed, pins, wc, depth).map(|(view, _, _)| view)
    }

    /// Build the world to `depth` ONCE and return the astronomy rung together
    /// with the artifacts that build produced. Deeper rungs use these instead
    /// of re-deriving with `terrain_of` / `climate_from` — the double sculpt
    /// The Hoist removes. An artifact is `None` only when the requested depth
    /// genuinely never built it, in which case the caller derives as before.
    fn build_to_with_artifacts(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<
        (
            AstronomyView,
            Option<hornvale_terrain::GeneratedTerrain>,
            Option<hornvale_climate::GeneratedClimate>,
        ),
        BuildError,
    > {
        let built = build_world_to_with_artifacts(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
            &wc,
            depth,
        )?;
        let sky = sky_of(&built.world)?;
        let Sky::Generated(sky) = sky else {
            return Err(BuildError::Pins(
                "expected Generated sky, got Constant".to_string(),
            ));
        };
        Ok((
            AstronomyView {
                system: sky.system().clone(),
                calendar: sky.calendar().clone(),
                notes: sky.notes().to_vec(),
                world: built.world,
                components: wc,
            },
            built.terrain,
            built.climate,
        ))
    }
}

/// Terrain rung: astronomy + the tectonic globe.
pub struct TerrainView {
    /// The astronomy rung this view extends.
    pub astronomy: AstronomyView,
    /// The tectonic globe summary (plates, ocean fraction, sea level, peak).
    pub globe: GlobeSummary,
    /// The full tectonic globe (for coverage metrics over cells).
    pub terrain: hornvale_terrain::GeneratedTerrain,
}

impl TerrainView {
    /// Build a terrain-rung view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<TerrainView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build a terrain-rung view with an explicit species roster.
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<TerrainView, BuildError> {
        Self::build_to(seed, pins, wc, BuildDepth::Terrain)
    }

    /// Build the world to `depth` and reconstruct the terrain-rung fields
    /// atop the astronomy rung built at the same depth.
    fn build_to(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<TerrainView, BuildError> {
        Self::build_to_with_climate(seed, pins, wc, depth).map(|(view, _)| view)
    }

    /// Build the terrain rung, reusing the terrain the world build already
    /// sculpted, and pass the build's climate artifact up to the climate rung.
    // Named construction site (decision 0092): the view-chain's own
    // build path — the fallback re-derive only fires when the world build
    // sculpted nothing at this depth.
    #[allow(clippy::disallowed_methods)]
    fn build_to_with_climate(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<(TerrainView, Option<hornvale_climate::GeneratedClimate>), BuildError> {
        let (astronomy, hoisted_terrain, hoisted_climate) =
            AstronomyView::build_to_with_artifacts(seed, pins, wc, depth)?;
        // The build sculpted this at any depth >= Terrain and handed it back;
        // re-derive only when it genuinely sculpted none (an Astronomy-depth
        // build), which keeps this strictly not-slower on every path.
        let terrain = match hoisted_terrain {
            Some(terrain) => terrain,
            None => terrain_of(&astronomy.world)?,
        };
        let globe = hornvale_terrain::summarize(terrain.globe());
        Ok((
            TerrainView {
                astronomy,
                globe,
                terrain,
            },
            hoisted_climate,
        ))
    }
}

impl AsRef<AstronomyView> for TerrainView {
    fn as_ref(&self) -> &AstronomyView {
        &self.astronomy
    }
}

/// Climate rung: terrain + reconstructed climate. Climate commits no facts
/// of its own, so this rung builds the world only to `BuildDepth::Terrain`
/// (or deeper, if reached from a deeper rung's constructor) and adds the
/// `climate_of` reconstruction.
pub struct ClimateView {
    /// The terrain rung this view extends.
    pub terrain: TerrainView,
    /// The derived climate (biome + habitability).
    pub climate: GeneratedClimate,
}

impl ClimateView {
    /// Build a climate-rung view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<ClimateView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build a climate-rung view with an explicit species roster.
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<ClimateView, BuildError> {
        Self::build_to(seed, pins, wc, BuildDepth::Terrain)
    }

    /// Build the world to `depth` and reconstruct the climate-rung fields
    /// atop the terrain rung built at the same depth.
    // Named construction site (decision 0092): the view-chain's own
    // build path — the fallback re-derive only fires on rungs shallower
    // than Settlements, which build no climate at all.
    #[allow(clippy::disallowed_methods)]
    fn build_to(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<ClimateView, BuildError> {
        let (terrain, hoisted_climate) = TerrainView::build_to_with_climate(seed, pins, wc, depth)?;
        // Reuse the climate the world build already derived off this very
        // terrain. The `climate_from` fallback is the pre-Hoist path, kept for
        // rungs shallower than Settlements, which build no climate at all —
        // it still avoids re-sculpting terrain (The Single Sculpt).
        let climate = match hoisted_climate {
            Some(climate) => climate,
            None => climate_from(&terrain.astronomy.world, &terrain.terrain)?,
        };
        Ok(ClimateView { terrain, climate })
    }

    /// The full tectonic globe, reached through the terrain rung this view
    /// extends. A passthrough (spec MAP-25 Stage 2 Task 5) so climate-rung
    /// closures read `v.terrain()` rather than `v.terrain.terrain`.
    pub fn terrain(&self) -> &hornvale_terrain::GeneratedTerrain {
        &self.terrain.terrain
    }
}

impl AsRef<TerrainView> for ClimateView {
    fn as_ref(&self) -> &TerrainView {
        &self.terrain
    }
}
impl AsRef<AstronomyView> for ClimateView {
    fn as_ref(&self) -> &AstronomyView {
        self.terrain.as_ref()
    }
}

/// Settlement rung: climate + a world built to settlement depth (spec §4 /
/// MAP-25). A metric handed a `SettlementView` reads a world whose
/// religion/culture/species/deep-time facts do not exist yet — the type
/// enforces the write-side boundary (this view has no `FullView`-only
/// fields); the metamorphic guard is the read-side backstop.
pub struct SettlementView {
    /// The climate rung this view extends.
    pub climate: ClimateView,
}

impl SettlementView {
    /// Build a settlement-rung view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<SettlementView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build a settlement-rung view with an explicit species roster.
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<SettlementView, BuildError> {
        let climate = ClimateView::build_to(seed, pins, wc, BuildDepth::Settlements)?;
        Ok(SettlementView { climate })
    }

    /// The world ledger, reached through the climate/terrain/astronomy
    /// rungs this view extends. A passthrough (spec MAP-25 Stage 2 Task 5)
    /// so settlement-rung closures read `v.world()` rather than a deep
    /// field chain.
    pub fn world(&self) -> &World {
        &self.climate.terrain.astronomy.world
    }

    /// The full tectonic globe, reached through the climate/terrain rungs
    /// this view extends.
    pub fn terrain(&self) -> &hornvale_terrain::GeneratedTerrain {
        self.climate.terrain()
    }

    /// The derived climate, reached through the climate rung this view
    /// extends.
    pub fn climate(&self) -> &GeneratedClimate {
        &self.climate.climate
    }

    /// The species roster this view was built from, reached through the
    /// climate/terrain/astronomy rungs this view extends.
    pub fn components(&self) -> &WorldComponents {
        &self.climate.terrain.astronomy.components
    }
}

impl AsRef<ClimateView> for SettlementView {
    fn as_ref(&self) -> &ClimateView {
        &self.climate
    }
}
impl AsRef<TerrainView> for SettlementView {
    fn as_ref(&self) -> &TerrainView {
        self.climate.as_ref()
    }
}
impl AsRef<AstronomyView> for SettlementView {
    fn as_ref(&self) -> &AstronomyView {
        self.climate.as_ref()
    }
}

/// Full rung: a world built to full depth (culture, religion, species,
/// deep time — today's full build).
pub struct FullView {
    /// The settlement rung this view extends.
    pub settlement: SettlementView,
}

impl FullView {
    /// Build a full-rung view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<FullView, BuildError> {
        Self::build_with_components(seed, pins, WorldComponents::assemble()?)
    }

    /// Build a full-rung view with an explicit species roster.
    pub fn build_with_components(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
    ) -> Result<FullView, BuildError> {
        let climate = ClimateView::build_to(seed, pins, wc, BuildDepth::Full)?;
        Ok(FullView {
            settlement: SettlementView { climate },
        })
    }

    /// The world ledger, reached through the settlement rung this view
    /// extends. A passthrough (spec MAP-25 Stage 2 Task 5) so full-rung
    /// closures read `v.world()` rather than a deep field chain.
    pub fn world(&self) -> &World {
        self.settlement.world()
    }

    /// The species roster this view was built from, reached through the
    /// settlement/climate/terrain/astronomy rungs this view extends.
    pub fn components(&self) -> &WorldComponents {
        &self.settlement.climate.terrain.astronomy.components
    }

    /// The derived climate, reached through the settlement/climate rungs
    /// this view extends.
    pub fn climate(&self) -> &GeneratedClimate {
        self.settlement.climate()
    }

    /// The sculpted terrain globe, reached through the settlement/climate/
    /// terrain rungs this view extends — so lexicon metrics can thread the
    /// already-built globe into `lexicon_from` instead of re-sculpting it.
    pub fn terrain(&self) -> &hornvale_terrain::GeneratedTerrain {
        self.settlement.terrain()
    }
}

impl AsRef<SettlementView> for FullView {
    fn as_ref(&self) -> &SettlementView {
        &self.settlement
    }
}
impl AsRef<ClimateView> for FullView {
    fn as_ref(&self) -> &ClimateView {
        self.settlement.as_ref()
    }
}
impl AsRef<TerrainView> for FullView {
    fn as_ref(&self) -> &TerrainView {
        self.settlement.as_ref()
    }
}
impl AsRef<AstronomyView> for FullView {
    fn as_ref(&self) -> &AstronomyView {
        self.settlement.as_ref()
    }
}

/// A metric's extractor, tagged by the view rung it reads. The tag *is* the
/// metric's build-depth: the runner builds each study only as deep as its
/// deepest selected metric. Under-building is impossible — an extractor
/// physically cannot name a field its view type does not expose.
pub enum Extractor {
    /// Reads astronomy only.
    Astronomy(fn(&AstronomyView) -> MetricValue),
    /// Reads terrain (+ astronomy).
    Terrain(fn(&TerrainView) -> MetricValue),
    /// Reads climate (+ terrain).
    Climate(fn(&ClimateView) -> MetricValue),
    /// Reads settlement/culture facts.
    Settlement(fn(&SettlementView) -> MetricValue),
    /// Reads religion/language/species facts.
    Full(fn(&FullView) -> MetricValue),
}

impl Extractor {
    /// The build depth this extractor requires. Climate maps to
    /// `BuildDepth::Terrain` because climate commits no facts — a
    /// Climate-rung metric needs a Terrain-depth world plus the climate
    /// reconstruction, which `ClimateView`'s constructor performs.
    pub fn rung(&self) -> BuildDepth {
        match self {
            Extractor::Astronomy(_) => BuildDepth::Astronomy,
            Extractor::Terrain(_) | Extractor::Climate(_) => BuildDepth::Terrain,
            Extractor::Settlement(_) => BuildDepth::Settlements,
            Extractor::Full(_) => BuildDepth::Full,
        }
    }

    /// Apply to a built view. The built view is always >= the extractor's
    /// rung (the runner guarantees it by building to the max selected rung),
    /// so the needed narrower view is reachable by `AsRef`. A shallower
    /// built view than the extractor's rung is a runner bug and panics
    /// loudly.
    pub fn apply(&self, view: &BuiltView) -> MetricValue {
        match (self, view) {
            (Extractor::Astronomy(f), v) => f(v.astronomy()),
            (Extractor::Terrain(f), v) => f(v.terrain()),
            (Extractor::Climate(f), v) => f(v.climate()),
            (Extractor::Settlement(f), v) => f(v.settlement()),
            (Extractor::Full(f), BuiltView::Full(fv)) => f(fv),
            (Extractor::Full(_), _) => panic!("Full extractor on a shallow view: runner bug"),
        }
    }
}

/// The view a study was built to — the runner's single per-world artifact.
/// Built once, at the study's deepest selected metric's rung, then every
/// metric's `Extractor` reads its own narrower view out of it via `AsRef`.
pub enum BuiltView {
    /// Built to `BuildDepth::Astronomy`.
    Astronomy(AstronomyView),
    /// Built to `BuildDepth::Terrain`.
    Terrain(TerrainView),
    /// A `Terrain`-depth build reconstructed with climate (Climate is a view
    /// rung, not a build stop — see `Extractor::rung`).
    Climate(ClimateView),
    /// Built to `BuildDepth::Settlements`.
    Settlement(SettlementView),
    /// Built to `BuildDepth::Full`.
    Full(FullView),
}

impl BuiltView {
    /// Build a world to `depth` and wrap the result in the matching
    /// variant. `BuildDepth` has no `Climate` rung (climate commits no
    /// facts — see `Extractor::rung`), so this always produces one of
    /// `Astronomy`/`Terrain`/`Settlement`/`Full`; a `BuiltView::Climate` is
    /// only ever constructed by widening the view of a `Terrain`-depth
    /// build for a Climate-rung metric, never returned here.
    pub fn build_to(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<BuiltView, BuildError> {
        match depth {
            BuildDepth::Astronomy => Ok(BuiltView::Astronomy(
                AstronomyView::build_with_components(seed, pins, wc)?,
            )),
            BuildDepth::Terrain => Ok(BuiltView::Terrain(TerrainView::build_with_components(
                seed, pins, wc,
            )?)),
            BuildDepth::Settlements => Ok(BuiltView::Settlement(
                SettlementView::build_with_components(seed, pins, wc)?,
            )),
            BuildDepth::Full => Ok(BuiltView::Full(FullView::build_with_components(
                seed, pins, wc,
            )?)),
        }
    }

    /// The astronomy-rung view, reached by `AsRef` from whichever variant
    /// this built view actually is.
    fn astronomy(&self) -> &AstronomyView {
        match self {
            BuiltView::Astronomy(v) => v,
            BuiltView::Terrain(v) => v.as_ref(),
            BuiltView::Climate(v) => v.as_ref(),
            BuiltView::Settlement(v) => v.as_ref(),
            BuiltView::Full(v) => v.as_ref(),
        }
    }

    /// The terrain-rung view. Panics on `BuiltView::Astronomy` — a
    /// Terrain-or-deeper extractor applied to a shallower built view is a
    /// runner bug (the runner guarantees the build depth matches the
    /// deepest selected metric's rung).
    fn terrain(&self) -> &TerrainView {
        match self {
            BuiltView::Astronomy(_) => {
                panic!("terrain-rung extractor on an astronomy-only built view: runner bug")
            }
            BuiltView::Terrain(v) => v,
            BuiltView::Climate(v) => v.as_ref(),
            BuiltView::Settlement(v) => v.as_ref(),
            BuiltView::Full(v) => v.as_ref(),
        }
    }

    /// The climate-rung view. Panics on `BuiltView::Astronomy` or
    /// `BuiltView::Terrain` — a climate-rung extractor applied to a
    /// shallower built view is a runner bug.
    fn climate(&self) -> &ClimateView {
        match self {
            BuiltView::Astronomy(_) | BuiltView::Terrain(_) => {
                panic!("climate-rung extractor on a shallower built view: runner bug")
            }
            BuiltView::Climate(v) => v,
            BuiltView::Settlement(v) => v.as_ref(),
            BuiltView::Full(v) => v.as_ref(),
        }
    }

    /// The settlement-rung view. Panics on any shallower built view — a
    /// settlement-rung extractor applied to one is a runner bug.
    fn settlement(&self) -> &SettlementView {
        match self {
            BuiltView::Astronomy(_) | BuiltView::Terrain(_) | BuiltView::Climate(_) => {
                panic!("settlement-rung extractor on a shallower built view: runner bug")
            }
            BuiltView::Settlement(v) => v,
            BuiltView::Full(v) => v.as_ref(),
        }
    }
}

/// A single-value outcome from a metric extractor.
/// type-audit: pending(wave-3: Number.0), bare-ok(identifier-text: Text.0), bare-ok(flag: Flag.0)
#[derive(Clone, Debug, PartialEq)]
pub enum MetricValue {
    /// A floating-point quantity.
    Number(f64),
    /// A string descriptor.
    Text(String),
    /// A true/false flag.
    Flag(bool),
    /// The metric does not apply to this world (e.g., no local day).
    Absent,
}

/// The kind of analysis a metric supports, with derived bucket edges.
/// type-audit: pending(wave-3: Numeric.bucket_edges)
#[derive(Clone, Debug, PartialEq)]
pub enum SummaryKind {
    /// Open-ended enumeration of distinct text values.
    Categorical,
    /// Binary state.
    Flag,
    /// Continuous range with histogram buckets.
    Numeric {
        /// Bucket upper-bound edges, in order.
        bucket_edges: &'static [f64],
    },
}

/// An analyzable property of a world, with extraction logic.
/// type-audit: bare-ok(identifier-text: name), bare-ok(prose: doc)
pub struct Metric {
    /// The metric's canonical name.
    pub name: &'static str,
    /// Human-readable documentation.
    pub doc: &'static str,
    /// The kind of analysis this metric supports.
    pub summary: SummaryKind,
    /// Extract this metric from the narrowest view it reads, tagged by rung.
    pub extract: Extractor,
}

impl Metric {
    /// The build depth this metric requires — delegates to its extractor's
    /// rung (the tag *is* the metric's build-depth, spec MAP-25).
    pub fn rung(&self) -> BuildDepth {
        self.extract.rung()
    }
}

/// The most-dreaded layer in a vestige stack (The Vestige, spec §9.2) — the
/// one a wanderer would sense most strongly. Mirrors
/// `hornvale_worldgen::render`'s private `most_dread` (re-derived here since
/// metrics can't reach across that module boundary): ties keep the
/// first-encountered (oldest, per `vestiges_at`'s ordering) layer, so the
/// pick is deterministic without depending on float tie-breaking.
fn most_dread_vestige(stack: &[hornvale_worldgen::Vestige]) -> Option<&hornvale_worldgen::Vestige> {
    let mut best: Option<&hornvale_worldgen::Vestige> = None;
    for vestige in stack {
        if best.is_none_or(|b| vestige.dread > b.dread) {
            best = Some(vestige);
        }
    }
    best
}

/// Human-readable hazard-kind name (The Vestige, spec §9.2): lowercase, for
/// census categorical values — a pure naming projection, no new draws.
/// Exhaustive (mirrors `commodity_name`/`rock_class_name`): a future
/// `HazardKind` variant fails to compile here rather than falling through a
/// wildcard.
fn hazard_name(hazard: HazardKind) -> &'static str {
    use HazardKind::*;
    match hazard {
        Structural => "structural",
        ToxicGas => "toxic-gas",
        Pestilent => "pestilent",
        Flooded => "flooded",
        Numinous => "numinous",
        Cursed => "cursed",
    }
}

/// The 100-year dated-eclipse scan shared by the cadence metrics — fixed
/// in standard days (a schedule constant, never a function of the drawn
/// year, so cost and precision are seed-independent).
fn scan_century(v: &AstronomyView) -> Vec<hornvale_astronomy::EclipseEvent> {
    hornvale_astronomy::eclipse_events(
        &v.system,
        &v.calendar,
        hornvale_astronomy::StdDays::new(0.0).unwrap(),
        hornvale_astronomy::StdDays::new(100.0 * 365.25).unwrap(),
    )
}

/// Count `scan_century`'s dated events of one body, as a metric value.
fn century_cadence(v: &AstronomyView, body: hornvale_astronomy::EclipseBody) -> MetricValue {
    let n = scan_century(v).iter().filter(|e| e.body == body).count();
    MetricValue::Number(n as f64)
}

/// Build the registry of tier-1 metrics.
pub fn registry() -> Vec<Metric> {
    vec![
        Metric {
            name: "star-class",
            doc: "Spectral class of the host star",
            summary: SummaryKind::Categorical,
            // Deliberately reads the in-memory system's `class_name` display
            // (e.g. "yellow dwarf (G)"), not the ledger's committed
            // `star-class` concept id (e.g. "yellow-dwarf") — the census is
            // an author-frame instrument, same justification as the "In
            // truth" register `windows/book` renders for the ground-truth
            // line. This is why the census rows didn't move when the ledger
            // switched from prose to a concept id.
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Text(v.system.star.class_name.clone())
            }),
        },
        Metric {
            name: "tidally-locked",
            doc: "Whether the world is tidally locked to its star",
            summary: SummaryKind::Flag,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Flag(matches!(v.system.anchor.rotation, Rotation::Locked))
            }),
        },
        Metric {
            name: "day-length-hours",
            doc: "Length of the solar day in standard hours; Absent if tidally locked",
            summary: SummaryKind::Numeric {
                bucket_edges: &[16.0, 20.0, 24.0, 28.0, 32.0, 36.0, 40.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| match &v.system.anchor.rotation {
                Rotation::Locked => MetricValue::Absent,
                Rotation::Spinning { day, .. } => MetricValue::Number(day.get() * 24.0),
            }),
        },
        Metric {
            name: "year-std-days",
            doc: "Length of the year in standard days",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 200.0, 400.0, 600.0, 800.0, 1000.0, 1200.0, 1400.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(v.system.anchor.year.get())
            }),
        },
        Metric {
            name: "year-local-days",
            doc: "Length of the year in local days; Absent if tidally locked",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 200.0, 400.0, 600.0, 800.0, 1000.0, 1200.0, 1400.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                if let Some(day_len) = v.calendar.day_length() {
                    MetricValue::Number(v.system.anchor.year.get() / day_len.get())
                } else {
                    MetricValue::Absent
                }
            }),
        },
        Metric {
            name: "obliquity-degrees",
            doc: "Axial tilt in degrees",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(v.system.anchor.obliquity.get())
            }),
        },
        Metric {
            name: "obliquity-range",
            doc: "Peak-to-peak obliquity swing over one obliquity period (2× the \
                   deep-time forcing amplitude, SKY-21); a moonless world keeps the \
                   full drawn wobble, a moon damps it",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(2.0 * v.system.forcing.obliquity_amp)
            }),
        },
        Metric {
            name: "moons-admitted",
            doc: "Number of moons in orbit",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Text(v.system.moons.len().to_string())
            }),
        },
        Metric {
            name: "refused-a-moon",
            doc: "Whether moon genesis recorded refusals",
            summary: SummaryKind::Flag,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Flag(!v.notes.is_empty())
            }),
        },
        Metric {
            name: "total-tide",
            doc: "Sum of all moon tidal forces",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                let total: f64 = v.system.moons.iter().map(|m| m.tide_rel).sum();
                MetricValue::Number(total)
            }),
        },
        Metric {
            name: "months-per-year-innermost",
            doc: "How many cycles of the nearest moon fit in one year; Absent if no moons",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 700.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                if let Some(months) = v.calendar.months_per_year(0) {
                    MetricValue::Number(months)
                } else {
                    MetricValue::Absent
                }
            }),
        },
        Metric {
            name: "neighbor-count",
            doc: "Number of notable neighbor stars",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Text(v.system.neighbors.len().to_string())
            }),
        },
        Metric {
            name: "brightest-neighbor-class",
            doc: "Spectral class of the brightest neighbor, in kebab-case",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                if let Some(neighbor) = v.system.neighbors.first() {
                    // These are the census's OWN author-frame labels, not the
                    // ledger's registered concept ids (`SPECTRAL_CLASSES` in
                    // `domains/astronomy/src/star.rs`) — this is a fourth,
                    // independent kebab-case spelling of the spectral
                    // classes, deliberately uncoupled from that table so a
                    // published census column never moves for a ledger
                    // reason. Five of six are byte-identical to the concept
                    // ids; `"sun-like"` here is NOT `"sun-like-star"` there.
                    // Do not join census rows to ledger `Value::Text` facts
                    // on this column.
                    let class_name = match neighbor.class {
                        NeighborClass::RedDwarf => "red-dwarf",
                        NeighborClass::SunLike => "sun-like",
                        NeighborClass::WhiteDwarf => "white-dwarf",
                        NeighborClass::OrangeGiant => "orange-giant",
                        NeighborClass::RedGiant => "red-giant",
                        NeighborClass::BlueGiant => "blue-giant",
                    };
                    MetricValue::Text(class_name.to_string())
                } else {
                    MetricValue::Absent
                }
            }),
        },
        Metric {
            name: "figure-count",
            doc: "Number of star figures the reference observer's sky holds",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                let astronomy_seed = v.world.seed.derive(ASTRONOMY_STREAM_ROOT);
                MetricValue::Text(
                    hornvale_astronomy::figures(astronomy_seed, &v.system)
                        .len()
                        .to_string(),
                )
            }),
        },
        Metric {
            name: "largest-figure-members",
            doc: "Member count of the largest star figure (0 if none)",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                let astronomy_seed = v.world.seed.derive(ASTRONOMY_STREAM_ROOT);
                let largest = hornvale_astronomy::figures(astronomy_seed, &v.system)
                    .iter()
                    .map(|f| f.member_count)
                    .max()
                    .unwrap_or(0);
                MetricValue::Text(largest.to_string())
            }),
        },
        Metric {
            name: "ecliptic-figure-count",
            doc: "Number of star figures standing on the ecliptic (the sun's road)",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                let astronomy_seed = v.world.seed.derive(ASTRONOMY_STREAM_ROOT);
                let count = hornvale_astronomy::figures(astronomy_seed, &v.system)
                    .iter()
                    .filter(|f| f.on_ecliptic)
                    .count();
                MetricValue::Text(count.to_string())
            }),
        },
        Metric {
            name: "genesis-note-count",
            doc: "Number of genesis notes recorded",
            summary: SummaryKind::Categorical,
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Text(v.notes.len().to_string())
            }),
        },
        Metric {
            name: "eclipse-year-days",
            doc: "Eclipse year (the sun's return to the innermost moon's node line), standard days; Absent if moonless",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 600.0, 1000.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| match v.system.moons.first() {
                None => MetricValue::Absent,
                Some(m) => {
                    let p = hornvale_astronomy::node_regression_period(
                        v.system.anchor.year,
                        m.period,
                        m.inclination_deg,
                    );
                    MetricValue::Number(
                        hornvale_astronomy::eclipse_year(v.system.anchor.year, p).get(),
                    )
                }
            }),
        },
        Metric {
            name: "brightening-per-gyr",
            doc: "The star's fractional main-sequence brightening per gigayear",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.10, 0.15, 0.20, 0.25],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(hornvale_astronomy::brightening_per_gyr(&v.system.star))
            }),
        },
        Metric {
            name: "alignment-drift-deg-per-kyr",
            doc: "Absolute solstice-sunrise azimuth drift over the first kiloyear at the \
                   flagship settlement's latitude; Absent when locked, unplaced, or polar",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0],
            },
            extract: Extractor::Settlement(|v| {
                let a: &AstronomyView = v.as_ref();
                let Some(lat) = flagship_latitude(v) else {
                    return MetricValue::Absent;
                };
                let t0 = hornvale_astronomy::StdDays::new(0.0).unwrap();
                let t1 = hornvale_astronomy::StdDays::new(1000.0 * 365.25).unwrap();
                match a.calendar.alignment_drift_deg(lat, t0, t1) {
                    Some(d) => MetricValue::Number(d.abs()),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "solar-eclipses-per-century",
            doc: "Dated solar eclipses anywhere on the world across a 100-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 50.0, 100.0, 200.0, 400.0, 800.0, 1600.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                century_cadence(v, hornvale_astronomy::EclipseBody::Solar)
            }),
        },
        Metric {
            name: "lunar-eclipses-per-century",
            doc: "Dated lunar eclipses across a 100-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 50.0, 100.0, 200.0, 400.0, 800.0, 1600.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                century_cadence(v, hornvale_astronomy::EclipseBody::Lunar)
            }),
        },
        Metric {
            name: "coincidence-days-per-century",
            doc: "Days in a 100-year scan carrying eclipse events from two or more different moons; zero for 0–1-moon worlds",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 5.0, 10.0, 25.0, 50.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(hornvale_astronomy::coincidence_days(&scan_century(v)) as f64)
            }),
        },
        Metric {
            name: "plate-count",
            doc: "Number of tectonic plates the globe drew or was pinned to",
            summary: SummaryKind::Categorical,
            extract: Extractor::Terrain(|v: &TerrainView| {
                MetricValue::Text(v.globe.plate_count.to_string())
            }),
        },
        Metric {
            name: "ocean-fraction",
            doc: "Fraction of globe cells below sea level",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                MetricValue::Number(v.globe.ocean_fraction)
            }),
        },
        Metric {
            name: "mountain-coverage",
            doc: "Fraction of land cells standing above 2000 m over the sea",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let sea = v.terrain.sea_level();
                let (mut land, mut high) = (0usize, 0usize);
                for cell in geo.cells() {
                    let e = v.terrain.elevation_at(cell);
                    if e >= sea {
                        land += 1;
                        if e - sea > 2000.0 {
                            high += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    high as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "band-count",
            doc: "Circulation bands per hemisphere; 'locked' if tidally locked",
            summary: SummaryKind::Categorical,
            extract: Extractor::Climate(|v: &ClimateView| match v.climate.band_count() {
                Some(n) => MetricValue::Text(n.to_string()),
                None => MetricValue::Text("locked".to_string()),
            }),
        },
        Metric {
            name: "habitable-fraction",
            doc: "Fraction of cells that are habitable (land, water, tolerable season)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5],
            },
            extract: Extractor::Climate(|v: &ClimateView| {
                MetricValue::Number(v.climate.habitable_fraction())
            }),
        },
        Metric {
            name: "unrest-coverage",
            doc: "Fraction of cells with tectonic unrest above 0.3",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.5],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let total = geo.cell_count();
                let restless = geo
                    .cells()
                    .filter(|c| v.terrain.unrest_at(*c) > 0.3)
                    .count();
                MetricValue::Number(if total == 0 {
                    0.0
                } else {
                    restless as f64 / total as f64
                })
            }),
        },
        // --- The Ground (Task 7): rock/soil/hydrogeology census metrics,
        // over land cells only (`terrain.is_ocean` guards each). ---
        Metric {
            name: "dominant-rock",
            doc: "The most common land rock class by cell count, spec §4's fine \
                  taxonomy (The Ground); Absent on a landless world",
            summary: SummaryKind::Categorical,
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let mut counts: std::collections::BTreeMap<RockClass, usize> =
                    std::collections::BTreeMap::new();
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        *counts.entry(v.terrain.rock_at(cell)).or_insert(0) += 1;
                    }
                }
                match counts.iter().max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(a.0))) {
                    Some((&rock, _)) => MetricValue::Text(rock_class_name(rock).to_string()),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "karst-fraction",
            doc: "Fraction of land cells whose hydrogeology classifies as karst \
                  (The Ground, spec §3)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut karst) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.hydro_at(cell) == Hydro::Karst {
                            karst += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    karst as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "aquifer-fraction",
            doc: "Fraction of land cells whose hydrogeology classifies as an \
                  aquifer (The Ground, spec §3)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.4],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut aquifer) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.hydro_at(cell) == Hydro::Aquifer {
                            aquifer += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    aquifer as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "mean-depth-to-basement",
            doc: "Mean depth to crystalline basement over land (m) — the sedimentary archive's thickness.",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 300.0, 600.0, 1000.0, 2000.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut sum) = (0usize, 0.0f64);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        sum += v.terrain.depth_to_basement_at(cell);
                    }
                }
                MetricValue::Number(if land == 0 { 0.0 } else { sum / land as f64 })
            }),
        },
        Metric {
            name: "unconformity-fraction",
            doc: "Fraction of land cells recording a nonconformity (missing time) — the archive's floating gaps.",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut gaps) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.unconformity_at(cell) {
                            gaps += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    gaps as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "mean-geothermal-gradient",
            doc: "Mean geothermal gradient over land (K/km) — the deep's energy base.",
            summary: SummaryKind::Numeric {
                bucket_edges: &[15.0, 18.0, 21.0, 24.0, 27.0, 30.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut sum) = (0usize, 0.0f64);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        sum += v.terrain.geothermal_gradient_at(cell).get();
                    }
                }
                MetricValue::Number(if land == 0 { 0.0 } else { sum / land as f64 })
            }),
        },
        // --- The Lode (Task 7): cave and ore-deposit census metrics, over
        // land cells only (`terrain.is_ocean` guards each), mirroring The
        // Ground. ---
        Metric {
            name: "cave-fraction",
            doc: "Fraction of land cells with a cave (The Lode, spec §5; \
                  MAP-10's lab candidate)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut caves) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.cave_at(cell).is_some() {
                            caves += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    caves as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "deposit-density",
            doc: "Fraction of land cells with an ore deposit (The Lode, spec §5)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut deposits) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.deposit_at(cell).is_some() {
                            deposits += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    deposits as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "dominant-commodity",
            doc: "The most common land ore commodity by cell count (The \
                  Lode, spec §5); Absent where no land cell has a deposit",
            summary: SummaryKind::Categorical,
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let mut counts: std::collections::BTreeMap<Commodity, usize> =
                    std::collections::BTreeMap::new();
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell)
                        && let Some(deposit) = v.terrain.deposit_at(cell)
                    {
                        *counts.entry(deposit.commodity).or_insert(0) += 1;
                    }
                }
                match counts.iter().max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(a.0))) {
                    Some((&commodity, _)) => {
                        MetricValue::Text(commodity_name(commodity).to_string())
                    }
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "mean-ore-grade",
            doc: "Mean ore grade [0,1] over land cells with a deposit (The \
                  Lode, spec §5); 0.0 where no land cell has a deposit",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.1, 0.2, 0.4, 0.6, 0.8],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut deposits, mut sum) = (0usize, 0.0f64);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell)
                        && let Some(deposit) = v.terrain.deposit_at(cell)
                    {
                        deposits += 1;
                        sum += deposit.grade;
                    }
                }
                MetricValue::Number(if deposits == 0 {
                    0.0
                } else {
                    sum / deposits as f64
                })
            }),
        },
        // --- The Vestige (Task 7): subsurface historical-residue census
        // metrics, over land cells only (`terrain.is_ocean` guards each),
        // mirroring The Ground/The Lode. `Extractor::Full`, not `Terrain`:
        // residue is derived from committed settlement history
        // (`vestiges_field`), which does not exist until `BuildDepth::Full`.
        // Each metric computes `vestiges_field` exactly ONCE (a single
        // grouped ledger scan) and then iterates its `CellMap` — never
        // `vestiges_at` per cell, which would rescan the whole ledger once
        // per cell. ---
        Metric {
            name: "vestige-density",
            doc: "Fraction of land cells with a non-empty vestige stack \
                  (The Vestige, spec §9.2)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Full(|v: &FullView| {
                let terrain = v.terrain();
                let geo = terrain.geosphere();
                let field = vestiges_field(v.world(), terrain);
                let (mut land, mut bearing) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !terrain.is_ocean(cell) {
                        land += 1;
                        if !field.get(cell).is_empty() {
                            bearing += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    bearing as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "forgotten-fraction",
            doc: "Over land cells with a non-empty vestige stack, the fraction \
                  whose most-dread layer is Forgotten rather than Venerated \
                  (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8],
            },
            extract: Extractor::Full(|v: &FullView| {
                let terrain = v.terrain();
                let geo = terrain.geosphere();
                let field = vestiges_field(v.world(), terrain);
                let (mut bearing, mut forgotten) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !terrain.is_ocean(cell)
                        && let Some(top) = most_dread_vestige(field.get(cell))
                    {
                        bearing += 1;
                        if top.valence == Valence::Forgotten {
                            forgotten += 1;
                        }
                    }
                }
                MetricValue::Number(if bearing == 0 {
                    0.0
                } else {
                    forgotten as f64 / bearing as f64
                })
            }),
        },
        Metric {
            name: "dominant-hazard",
            doc: "The most common hazard kind among land-cell vestiges by \
                  layer count (The Vestige, spec §9.2); Absent where no land \
                  cell bears a vestige",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| {
                let terrain = v.terrain();
                let geo = terrain.geosphere();
                let field = vestiges_field(v.world(), terrain);
                let kinds = [
                    HazardKind::Structural,
                    HazardKind::ToxicGas,
                    HazardKind::Pestilent,
                    HazardKind::Flooded,
                    HazardKind::Numinous,
                    HazardKind::Cursed,
                ];
                let mut counts = [0usize; 6];
                for cell in geo.cells() {
                    if !terrain.is_ocean(cell) {
                        for vestige in field.get(cell) {
                            let idx = kinds
                                .iter()
                                .position(|k| *k == vestige.hazard)
                                .expect("kinds lists every HazardKind variant");
                            counts[idx] += 1;
                        }
                    }
                }
                let mut best = 0usize;
                for i in 1..counts.len() {
                    if counts[i] > counts[best] {
                        best = i;
                    }
                }
                if counts[best] == 0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Text(hazard_name(kinds[best]).to_string())
                }
            }),
        },
        Metric {
            name: "mean-warning-legibility",
            doc: "Mean warning_legibility over every land-cell vestige layer \
                  (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8],
            },
            extract: Extractor::Full(|v: &FullView| {
                let terrain = v.terrain();
                let geo = terrain.geosphere();
                let field = vestiges_field(v.world(), terrain);
                let (mut count, mut sum) = (0usize, 0.0f64);
                for cell in geo.cells() {
                    if !terrain.is_ocean(cell) {
                        for vestige in field.get(cell) {
                            count += 1;
                            sum += vestige.warning_legibility;
                        }
                    }
                }
                MetricValue::Number(if count == 0 { 0.0 } else { sum / count as f64 })
            }),
        },
        Metric {
            name: "dominant-land-biome",
            doc: "The most common land biome by cell count, kebab-case",
            summary: SummaryKind::Categorical,
            extract: Extractor::Climate(|v: &ClimateView| {
                let biomes = v.climate.biome_map();
                // Count land biomes in ascending name order for determinism.
                let mut counts: std::collections::BTreeMap<&'static str, usize> =
                    std::collections::BTreeMap::new();
                for (_, b) in biomes.iter() {
                    if !b.is_marine() {
                        *counts.entry(b.name()).or_insert(0) += 1;
                    }
                }
                match counts
                    .into_iter()
                    .max_by(|a, b| a.1.cmp(&b.1).then(b.0.cmp(a.0)))
                {
                    Some((name, _)) => MetricValue::Text(name.to_string()),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "mean-land-temperature-c",
            doc: "Annual-mean temperature averaged over land cells, °C; Absent \
                   if the world has no land",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0],
            },
            extract: Extractor::Climate(|v: &ClimateView| {
                let geo = v.terrain().geosphere();
                let (mut sum, mut count) = (0.0_f64, 0_u32);
                for cell in geo.cells() {
                    if !v.terrain().is_ocean(cell) {
                        sum += v.climate.mean_temperature_at(cell).get();
                        count += 1;
                    }
                }
                if count == 0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(sum / f64::from(count))
                }
            }),
        },
        Metric {
            name: "dominant-soil-order",
            doc: "The most common land soil order by cell count, spec §4's soil \
                  taxonomy (The Ground); Absent on a landless world",
            summary: SummaryKind::Categorical,
            extract: Extractor::Climate(|v: &ClimateView| {
                let geo = v.terrain().geosphere();
                let soils = soil_of(v.terrain(), &v.climate, geo);
                let mut counts: std::collections::BTreeMap<SoilOrder, usize> =
                    std::collections::BTreeMap::new();
                for cell in geo.cells() {
                    if !v.terrain().is_ocean(cell) {
                        *counts.entry(*soils.get(cell)).or_insert(0) += 1;
                    }
                }
                match counts.iter().max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(a.0))) {
                    Some((&order, _)) => MetricValue::Text(soil_order_name(order).to_string()),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "fertile-land-fraction",
            doc: "Fraction of land cells whose soil fertility's grain-suitability \
                  exceeds 0.6 (The Ground, spec §3/§4)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8],
            },
            extract: Extractor::Climate(|v: &ClimateView| {
                let geo = v.terrain().geosphere();
                let soils = soil_of(v.terrain(), &v.climate, geo);
                let (mut land, mut fertile) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain().is_ocean(cell) {
                        land += 1;
                        let depth = v.terrain().material_at(cell).soil_depth;
                        let f = fertility(*soils.get(cell), &depth);
                        if f.grain_suit > 0.6 {
                            fertile += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    fertile as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "settlement-count",
            doc: "Number of settlements placed in the world",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 10.0, 20.0, 40.0, 60.0, 80.0, 120.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                MetricValue::Number(hornvale_terrain::places(v.world()).len() as f64)
            }),
        },
        Metric {
            name: "mean-population",
            doc: "Mean population across every settlement's committed population fact; \
                   Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let places = hornvale_terrain::places(v.world());
                let pops: Vec<f64> = places
                    .iter()
                    .filter_map(|p| {
                        match v
                            .world()
                            .ledger
                            .value_of(p.id, hornvale_settlement::POPULATION)
                        {
                            Some(Value::Number(n)) => Some(*n),
                            _ => None,
                        }
                    })
                    .collect();
                if pops.is_empty() {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(pops.iter().sum::<f64>() / pops.len() as f64)
                }
            }),
        },
        Metric {
            name: "total-population",
            doc: "Sum of every settlement's committed population fact; Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 500.0, 1000.0, 2000.0, 4000.0, 8000.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let places = hornvale_terrain::places(v.world());
                let pops: Vec<f64> = places
                    .iter()
                    .filter_map(|p| {
                        match v
                            .world()
                            .ledger
                            .value_of(p.id, hornvale_settlement::POPULATION)
                        {
                            Some(Value::Number(n)) => Some(*n),
                            _ => None,
                        }
                    })
                    .collect();
                if pops.is_empty() {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(pops.iter().sum::<f64>())
                }
            }),
        },
        Metric {
            name: "capacity-by-abs-latitude",
            doc: "The carrying-capacity field's headline calibration (design spec §5): the \
                   ratio of mean per-land-cell K (summed over the roster's PEOPLED kinds' \
                   individual fields, each species' own psychology folded in — fauna kinds \
                   have no psychology and are excluded, preserving this metric's \
                   pre-menagerie population) in the \
                   low-latitude band (|latitude| < 30) to the \
                   polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of \
                   the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP \
                   proxy's honest reading of hard cold, not a bug — reports a large-but-bounded \
                   ratio rather than a division blowup. A field grounded in the real biomass \
                   gradient reads well above 1 here; Absent if either band has no land cells (a \
                   wholly ocean or wholly polar world)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 3.0, 5.0, 10.0, 20.0, 40.0, 60.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let geo = v.terrain().geosphere();
                let base_inputs =
                    hornvale_worldgen::carrying_inputs_of(geo, v.terrain(), v.climate());
                let (mut trop_sum, mut trop_n, mut pole_sum, mut pole_n) =
                    (0.0_f64, 0u32, 0.0_f64, 0u32);
                // Peopled kinds only: `peopled()` panics on fauna (the
                // pass-boundary contract, worldgen lib.rs), and this
                // metric's population has always been the psych-bearing
                // roster — fauna carrying capacity is the biosphere/niche
                // path, not this field.
                for (kind, psych) in v.components().psyche.iter() {
                    // Peoples-only (the settling roster); skip the minded
                    // solitaries (dragons carry a psyche but never settle) so
                    // this metric is byte-identical to before The Eremite.
                    if v.components().biosphere.get(kind).map(|b| b.social_form)
                        != Some(hornvale_species::SocialForm::Settled)
                    {
                        continue;
                    }
                    let inputs = hornvale_kernel::CellMap::from_fn(geo, |c| {
                        hornvale_worldgen::species_carrying_input(*base_inputs.get(c), psych)
                    });
                    let k = hornvale_demography::carrying_capacity(geo, &inputs);
                    for cell in geo.cells() {
                        if v.terrain().is_ocean(cell) {
                            continue;
                        }
                        let lat = geo.coord(cell).latitude.abs();
                        let kv = k.at(cell);
                        if lat < 30.0 {
                            trop_sum += kv;
                            trop_n += 1;
                        } else if lat > 60.0 {
                            pole_sum += kv;
                            pole_n += 1;
                        }
                    }
                }
                if trop_n == 0 || pole_n == 0 {
                    return MetricValue::Absent;
                }
                // A floor comparable to the smallest physically meaningful K
                // unit (the NPP proxy's baseline scale is O(1)): an exactly-
                // zero polar band reads as a bounded ratio, not a division
                // blowup, while a genuinely-small-but-measured polar K (e.g.
                // a mild subpolar cell) still moves the ratio honestly.
                const POLE_FLOOR: f64 = 0.01;
                let trop_mean = trop_sum / f64::from(trop_n);
                let pole_mean = pole_sum / f64::from(pole_n);
                MetricValue::Number(trop_mean / pole_mean.max(POLE_FLOOR))
            }),
        },
        Metric {
            name: "per-cell-diversity",
            doc: "Mean per-cell species diversity of the coexistence density stack (task \
                   A16a; feeds the A16b β calibration): the mean, over habitable land cells, \
                   of the demography report's `byproducts.strife` field — already the \
                   per-cell inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species \
                   dominates a cell, →N when N species share it evenly). Recomputed via \
                   `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL \
                   report the settlement-genesis path builds internally (the shared-assembly \
                   refactor of task A16a), so this measures the stack the world actually \
                   ships, not a parallel one. Absent if the report fails to build or the \
                   world has no habitable cells",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                // Named construction site (decision 0092): a metric extractor
                // deliberately recomputes the fit per read against already-derived
                // artifacts (documented in the metric's doc string above).
                #[allow(clippy::disallowed_methods)]
                let Ok(report) = hornvale_worldgen::demography_report_from(
                    v.world(),
                    v.components(),
                    v.terrain(),
                    v.climate(),
                ) else {
                    return MetricValue::Absent;
                };
                let geo = v.terrain().geosphere();
                let habitability = v.climate().habitability();
                let (mut sum, mut n) = (0.0_f64, 0u32);
                for cell in geo.cells() {
                    if *habitability.get(cell) {
                        sum += *report.byproducts.strife.get(cell);
                        n += 1;
                    }
                }
                if n == 0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(sum / f64::from(n))
                }
            }),
        },
        Metric {
            name: "composition-variance",
            doc: "Spatial heterogeneity of settlement composition (The Niche): the sum \
                   over roster species of the variance, across the demography report's \
                   `stack_settlements`, of each species' composition fraction. 0.0 iff \
                   every settlement has the identical species mix (the pre-Niche \
                   'oatmeal' — one flat blend worldwide); > 0 when composition varies \
                   across space (species dominant in different strongholds). Recomputed \
                   via `hornvale_worldgen::demography_report_from` (the niche-differentiated \
                   coexistence shadow). Absent if the report fails to build or the world \
                   has fewer than 2 settlements",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.005, 0.01, 0.02, 0.05, 0.1],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                // Named construction site (decision 0092): a metric extractor
                // deliberately recomputes the fit per read against already-derived
                // artifacts (documented in the metric's doc string above).
                #[allow(clippy::disallowed_methods)]
                let Ok(report) = hornvale_worldgen::demography_report_from(
                    v.world(),
                    v.components(),
                    v.terrain(),
                    v.climate(),
                ) else {
                    return MetricValue::Absent;
                };
                let settlements = &report.stack_settlements;
                if settlements.len() < 2 {
                    return MetricValue::Absent;
                }
                let n = settlements.len() as f64;
                let mut total_var = 0.0_f64;
                for sid in 0..v.components().biosphere.len() as u32 {
                    // this species' fraction in each settlement (0.0 if absent from the mix)
                    let fracs = settlements.iter().map(|s| {
                        s.composition
                            .iter()
                            .find(|(id, _)| *id == sid)
                            .map(|(_, f)| *f)
                            .unwrap_or(0.0)
                    });
                    let mean = fracs.clone().sum::<f64>() / n;
                    let var = fracs.map(|f| (f - mean) * (f - mean)).sum::<f64>() / n;
                    total_var += var;
                }
                MetricValue::Number(total_var)
            }),
        },
        Metric {
            name: "pop-weighted-abs-latitude",
            doc: "The population-weighted mean absolute latitude across every settlement: \
                   Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and \
                   LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is \
                   ≈32.7°; people concentrating off the poles (design spec §5) should read \
                   below that baseline. Absent if there are no settlements with both facts",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let places = hornvale_terrain::places(v.world());
                let (mut weighted_sum, mut pop_sum) = (0.0_f64, 0.0_f64);
                for p in &places {
                    let pop = match v
                        .world()
                        .ledger
                        .value_of(p.id, hornvale_settlement::POPULATION)
                    {
                        Some(Value::Number(n)) => *n,
                        _ => continue,
                    };
                    let lat = match v
                        .world()
                        .ledger
                        .value_of(p.id, hornvale_settlement::LATITUDE)
                    {
                        Some(Value::Number(n)) => *n,
                        _ => continue,
                    };
                    weighted_sum += pop * lat.abs();
                    pop_sum += pop;
                }
                if pop_sum <= 0.0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(weighted_sum / pop_sum)
                }
            }),
        },
        Metric {
            name: "rank-size-slope",
            doc: "The OLS slope of log(population) on log(rank) across every settlement in the \
                   world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED \
                   metric only — this campaign's interim per-species condensation is \
                   deliberately NOT tuned to a rank-size target (design spec §5; full Zipf \
                   calibration is the later MAP-22 coexistence-stack campaign's job, once size \
                   is measured by mass and composition is real). Absent if fewer than 2 \
                   settlements exist",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let mut pops: Vec<f64> = hornvale_terrain::places(v.world())
                    .iter()
                    .filter_map(|p| {
                        match v
                            .world()
                            .ledger
                            .value_of(p.id, hornvale_settlement::POPULATION)
                        {
                            Some(Value::Number(n)) if *n > 0.0 => Some(*n),
                            _ => None,
                        }
                    })
                    .collect();
                if pops.len() < 2 {
                    return MetricValue::Absent;
                }
                // Descending by population; rank 1 is the largest. Tie order
                // among equal populations does not affect the regression (it
                // is a pure function of the sorted VALUES), so no further
                // tie-break is needed for determinism.
                pops.sort_by(|a, b| b.total_cmp(a));
                let xs: Vec<f64> = (1..=pops.len())
                    .map(|r| hornvale_kernel::math::ln(r as f64))
                    .collect();
                let ys: Vec<f64> = pops.iter().map(|p| hornvale_kernel::math::ln(*p)).collect();
                let n = xs.len() as f64;
                let mean_x = xs.iter().sum::<f64>() / n;
                let mean_y = ys.iter().sum::<f64>() / n;
                let (mut num, mut den) = (0.0_f64, 0.0_f64);
                for (x, y) in xs.iter().zip(ys.iter()) {
                    num += (x - mean_x) * (y - mean_y);
                    den += (x - mean_x) * (x - mean_x);
                }
                if den == 0.0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(num / den)
                }
            }),
        },
        Metric {
            name: "flagship-subsistence",
            doc: "The goblin flagship settlement's committed subsistence mode (the pantheon's \
                   community, spec §6); Absent if there is no goblin flagship or no committed \
                   subsistence",
            summary: SummaryKind::Categorical,
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "goblin") {
                    Some(info) => match hornvale_culture::subsistence_of(v.world(), info.id) {
                        Some(s) => MetricValue::Text(s),
                        None => MetricValue::Absent,
                    },
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "flagship-biome",
            doc: "The goblin flagship settlement's committed biome; Absent if there is no \
                   goblin flagship",
            summary: SummaryKind::Categorical,
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "goblin") {
                    Some(info) => {
                        match v
                            .world()
                            .ledger
                            .text_of(info.id, hornvale_settlement::BIOME)
                        {
                            Some(b) => MetricValue::Text(b.to_string()),
                            None => MetricValue::Absent,
                        }
                    }
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "flagship-coastal",
            doc: "Whether the goblin flagship settlement's cell borders an ocean cell, \
                   recomputed from the terrain provider; Absent if there is no goblin flagship",
            summary: SummaryKind::Flag,
            extract: Extractor::Settlement(|v: &SettlementView| {
                let Some(info) = flagship_of(v.world(), "goblin") else {
                    return MetricValue::Absent;
                };
                let Some(Value::Number(cell_id)) = v
                    .world()
                    .ledger
                    .value_of(info.id, hornvale_settlement::CELL_ID)
                else {
                    return MetricValue::Absent;
                };
                let cell = CellId(*cell_id as u32);
                let coastal = v
                    .terrain()
                    .geosphere()
                    .neighbors(cell)
                    .iter()
                    .any(|n| v.terrain().is_ocean(*n));
                MetricValue::Flag(coastal)
            }),
        },
        Metric {
            name: "flagship-structure-size",
            doc: "Number of castes present in the goblin flagship settlement's emergent \
                   structure (a stratification proxy, matched against the same community \
                   religion's pantheon-verticality reasons about); Absent if there is no \
                   goblin flagship",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "goblin") {
                    Some(info) => MetricValue::Number(
                        hornvale_culture::castes_of(v.world(), info.id).len() as f64,
                    ),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "endorheic-coverage",
            doc: "Fraction of land cells that are endorheic (interior-draining)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let geo = v.terrain.geosphere();
                let (mut land, mut endorheic) = (0usize, 0usize);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        land += 1;
                        if v.terrain.is_endorheic(cell) {
                            endorheic += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 {
                    0.0
                } else {
                    endorheic as f64 / land as f64
                })
            }),
        },
        Metric {
            name: "pantheon-size",
            doc: "Number of beliefs in the goblin flagship's pantheon; Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Full(|v: &FullView| {
                let Some(info) = flagship_of(v.world(), "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(v.world(), info.id);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(beliefs.len() as f64)
                }
            }),
        },
        Metric {
            name: "cult-form",
            doc: "The goblin flagship's pantheon's shared cult form ('organized' or 'folk'); \
                   Absent if no goblin beliefs",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| {
                let Some(info) = flagship_of(v.world(), "goblin") else {
                    return MetricValue::Absent;
                };
                match hornvale_religion::cult_form_held_by(v.world(), info.id) {
                    Some(form) => MetricValue::Text(form),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "pantheon-verticality",
            doc: "Whether the goblin flagship's pantheon is ranked (a high god presides) or \
                   flat; Absent if there is no goblin flagship pantheon",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| {
                let Some(info) = flagship_of(v.world(), "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(v.world(), info.id);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else if beliefs.iter().any(|b| b.high_god) {
                    MetricValue::Text("ranked".to_string())
                } else {
                    MetricValue::Text("flat".to_string())
                }
            }),
        },
        Metric {
            name: "head-deity-periodicity",
            doc: "The sentiment tag of the goblin flagship's head deity (the most salient \
                   belief): 'eternal', 'cyclic', or 'ambient'; Absent if no goblin beliefs",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| {
                let Some(info) = flagship_of(v.world(), "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(v.world(), info.id);
                match beliefs.first() {
                    Some(head) => MetricValue::Text(head.sentiment.as_str().to_string()),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "goblin-flagship-roles",
            doc: "The goblin flagship's committed role ladder, comma-joined, \
                   lowest to highest; Absent if goblins placed no settlement",
            summary: SummaryKind::Categorical,
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "goblin") {
                    Some(info) => {
                        let castes = hornvale_culture::castes_of(v.world(), info.id);
                        if castes.is_empty() {
                            MetricValue::Absent
                        } else {
                            MetricValue::Text(castes.join(","))
                        }
                    }
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "kobold-flagship-roles",
            doc: "The kobold flagship's committed role ladder, comma-joined, \
                   lowest to highest; Absent if kobolds placed no settlement",
            summary: SummaryKind::Categorical,
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "kobold") {
                    Some(info) => {
                        let castes = hornvale_culture::castes_of(v.world(), info.id);
                        if castes.is_empty() {
                            MetricValue::Absent
                        } else {
                            MetricValue::Text(castes.join(","))
                        }
                    }
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "goblin-flagship-population",
            doc: "The goblin flagship settlement's committed population; \
                   Absent if goblins placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "goblin") {
                    Some(info) => MetricValue::Number(f64::from(info.population)),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "kobold-flagship-population",
            doc: "The kobold flagship settlement's committed population; \
                   Absent if kobolds placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                match flagship_of(v.world(), "kobold") {
                    Some(info) => MetricValue::Number(f64::from(info.population)),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "goblin-flagship-surplus",
            doc: "The goblin flagship cell's subsistence surplus, recomputed \
                   from providers as fertility(biome_class) × moisture (the \
                   independent column the slave calibration needs); Absent \
                   if goblins placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| flagship_surplus(v, "goblin")),
        },
        Metric {
            name: "kobold-flagship-surplus",
            doc: "The kobold flagship cell's subsistence surplus, recomputed \
                   from providers as fertility(biome_class) × moisture (the \
                   independent column the slave calibration needs); Absent \
                   if kobolds placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| flagship_surplus(v, "kobold")),
        },
        Metric {
            name: "goblin-flagship-coastal",
            doc: "Whether the goblin flagship settlement's cell borders an \
                   ocean cell, recomputed from the terrain provider; Absent \
                   if goblins placed no settlement",
            summary: SummaryKind::Flag,
            extract: Extractor::Settlement(|v: &SettlementView| flagship_coastal(v, "goblin")),
        },
        Metric {
            name: "kobold-flagship-coastal",
            doc: "Whether the kobold flagship settlement's cell borders an \
                   ocean cell, recomputed from the terrain provider; Absent \
                   if kobolds placed no settlement",
            summary: SummaryKind::Flag,
            extract: Extractor::Settlement(|v: &SettlementView| flagship_coastal(v, "kobold")),
        },
        Metric {
            name: "goblin-settlement-count",
            doc: "Number of settlements peopled by goblins",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                MetricValue::Number(species_settlement_count(v, "goblin"))
            }),
        },
        Metric {
            name: "kobold-settlement-count",
            doc: "Number of settlements peopled by kobolds",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                MetricValue::Number(species_settlement_count(v, "kobold"))
            }),
        },
        Metric {
            name: "head-deity-domain-goblin",
            doc: "Venue domain of the goblin flagship's head deity: solar, lunar, or ambient; Absent without a goblin pantheon",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "head-deity-domain-kobold",
            doc: "Venue domain of the kobold flagship's head deity: solar, lunar, or ambient; Absent without a kobold pantheon",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "pantheon-size-goblin",
            doc: "Number of deities in the goblin flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "pantheon-size-kobold",
            doc: "Number of deities in the kobold flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "cult-form-goblin",
            doc: "Cult form of the goblin flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "cult-form-kobold",
            doc: "Cult form of the kobold flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-bugbear",
            doc: "Sentiment of the bugbear flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "bugbear") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-goblin",
            doc: "Sentiment of the goblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "goblin") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-hobgoblin",
            doc: "Sentiment of the hobgoblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(
                |v: &FullView| match species_head_sentiment(v, "hobgoblin") {
                    Some(s) => MetricValue::Text(s),
                    None => MetricValue::Absent,
                },
            ),
        },
        Metric {
            name: "belief-kind-kobold",
            doc: "Sentiment of the kobold flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "kobold") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "blind-attribution-correct",
            doc: "Whether the fixed structural rule (lunar head, then cyclic share, then size — no lexical input) attributes the kobold pantheon correctly; Absent unless both peoples hold pantheons",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| {
                let (Some(g), Some(k)) = (pantheon_sig(v, "goblin"), pantheon_sig(v, "kobold"))
                else {
                    return MetricValue::Absent;
                };
                // The rule is a symmetric function of the unordered pair;
                // presenting (goblin, kobold) and requiring index 1 is the
                // correctness check, not a labeling leak.
                MetricValue::Flag(pick_kobold([&g, &k]) == Some(1))
            }),
        },
        Metric {
            name: "phonotactic-validity-goblin",
            doc: "Whether every generated name (settlement, deity, epithet) attributed to \
                   goblins in this world re-validates against the goblin phonology, \
                   independently re-derived and re-parsed from the surface string; \
                   Absent if goblins produced no names",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| phonotactic_validity(v, "goblin")),
        },
        Metric {
            name: "phonotactic-validity-kobold",
            doc: "Whether every generated name (settlement, deity, epithet) attributed to \
                   kobolds in this world re-validates against the kobold phonology, \
                   independently re-derived and re-parsed from the surface string; \
                   Absent if kobolds produced no names",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| phonotactic_validity(v, "kobold")),
        },
        Metric {
            name: "epithet-honorific-goblin",
            doc: "Whether every committed goblin deity epithet carries a prepended honorific \
                   affix — DETECTED from the committed epithet content: the committed word, \
                   case-folded, must end with the independently re-derived honorific-OFF stem \
                   and be strictly longer (Rank status basis → honorifics on, spec §7); Absent \
                   if goblins hold no pantheon",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| epithet_honorific(v, "goblin")),
        },
        Metric {
            name: "epithet-honorific-kobold",
            doc: "Whether every committed kobold deity epithet carries a prepended honorific \
                   affix — DETECTED from the committed epithet content (see \
                   epithet-honorific-goblin); kobold's Knowledge status basis leaves honorifics \
                   off, so the committed epithet equals the plain stem and this reads false; \
                   Absent if kobolds hold no pantheon",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| epithet_honorific(v, "kobold")),
        },
        Metric {
            name: "name-length-goblin",
            doc: "Mean character length of every generated name (settlement, deity, epithet) \
                   attributed to goblins in this world; Absent if goblins produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: Extractor::Full(|v: &FullView| mean_name_length(v, "goblin")),
        },
        Metric {
            name: "name-length-kobold",
            doc: "Mean character length of every generated name (settlement, deity, epithet) \
                   attributed to kobolds in this world; Absent if kobolds produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: Extractor::Full(|v: &FullView| mean_name_length(v, "kobold")),
        },
        // --- The Wearing (Task 11): the two readings the campaign's own
        // claim needs and the character-length column cannot give (spec §7).
        Metric {
            name: "name-syllables-goblin",
            doc: "Mean syllable count of every generated name (settlement, deity, epithet) \
                   attributed to goblins in this world, counted as maximal vowel runs in the \
                   committed surface (an orthographic proxy — see the metric's own doc \
                   comment for its measured error bound); the reading The Wearing's claim \
                   needs, since character length cannot tell shorter words from the same \
                   words spelled tighter. Target 2-3; Absent if goblins produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
            },
            extract: Extractor::Full(|v: &FullView| mean_name_syllables(v, "goblin")),
        },
        Metric {
            name: "name-syllables-kobold",
            doc: "Mean syllable count of every generated name attributed to kobolds in this \
                   world (see name-syllables-goblin); Absent if kobolds produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
            },
            extract: Extractor::Full(|v: &FullView| mean_name_syllables(v, "kobold")),
        },
        Metric {
            name: "name-transparency",
            doc: "Share of this world's committed settlement names whose surface still \
                   contains, verbatim, the modern citation form of EVERY concept its own \
                   committed name-gloss names — read from the ledger and the lexicon, never \
                   from the naming code. The target is explicitly NOT 1.0 (The Wearing, spec \
                   §8): transparency was 100% by construction before this campaign, and that \
                   uniformity is the defect — most real toponyms are opaque to their own \
                   speakers. A distribution, pinned as a drift witness, never bounded. Absent \
                   if no settlement carries a non-empty gloss",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(name_transparency),
        },
        Metric {
            name: "name-collision-rate",
            doc: "Fraction of this world's settlement + deity names (across every species) \
                   that duplicate another name in the same world — uniqueness is de-facto, not \
                   enforced (Task 9), so this MEASURES the collision rate rather than asserting \
                   zero. Scope: settlement and deity proper nouns only; epithets are \
                   deliberately EXCLUDED (they are descriptive words expected to repeat by \
                   design, so they are not collision candidates). Absent if the world has no \
                   settlement or deity names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.4],
            },
            extract: Extractor::Full(name_collision_rate),
        },
        Metric {
            name: "head-deity-domain-goblin-twin",
            doc: "Venue domain of the goblin-twin flagship's head deity (null control, spec §4); Absent without a goblin-twin pantheon",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "pantheon-size-goblin-twin",
            doc: "Number of deities in the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "cult-form-goblin-twin",
            doc: "Cult form of the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "name-length-goblin-twin",
            doc: "Mean character length of every generated name attributed to the goblin-twin (null control); Absent if it produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: Extractor::Full(|v: &FullView| mean_name_length(v, "goblin-twin")),
        },
        Metric {
            name: "pantheon-cyclic-share-goblin",
            doc: "Fraction of the goblin flagship pantheon's source phenomena that are periodic (the pick_kobold input the null control needs); Absent without a goblin pantheon",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "pantheon-cyclic-share-goblin-twin",
            doc: "Fraction of the goblin-twin flagship pantheon's source phenomena that are periodic (null control); Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            }),
        },
        // --- The Words (Task 12): name-gloss truthfulness, lexicon
        // regularity, exposure soundness, and the pack-depth baseline
        // (spec §9). ---
        Metric {
            name: "name-gloss-true",
            doc: "Whether every committed settlement name-gloss fact in this world is a \
                   truthful composition of that SAME settlement's own re-derived site \
                   concepts — up to twelve: the nine toponymic terrain concepts its own \
                   cell offers (hydrography, elevation extrema, landmass size, wetness), \
                   The Toponym's characteristic climate variant, the biome, and the \
                   presiding sky phenomenon. The Wearing's Task 5 widened this vector \
                   past the original biome + presiding pair, and the close merge with The \
                   Toponym added the variant. Rather than restate the vector, this metric \
                   re-derives it by calling worldgen's own settlement_site_concepts, so \
                   there is no hand-maintained parallel definition to go stale — only \
                   this sentence, which has now gone stale twice; Absent if no settlement \
                   in this world carries a gloss",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(name_gloss_true),
        },
        Metric {
            name: "lexicon-regular-goblin",
            doc: "Whether every goblin lexicon Root entry's recorded sound-change \
                   derivation replays byte-identically through evolve (Neogrammarian \
                   regularity, spec §9.1); Absent if the goblin lexicon minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| lexicon_regular(v, "goblin")),
        },
        Metric {
            name: "cascade-rules-fired-goblin",
            doc: "How many DISTINCT sound rules in the goblin cascade actually fire on \
                   at least one lexicon Root. Zero means the etymological layer is inert \
                   for this species (The Namesake §5.0); Absent if goblin is unrostered \
                   or minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0],
            },
            extract: Extractor::Full(|v: &FullView| cascade_rules_fired(v, "goblin")),
        },
        Metric {
            name: "cascade-rules-fired-bugbear",
            doc: "How many DISTINCT sound rules in the bugbear cascade actually fire on \
                   at least one lexicon Root. Zero means the etymological layer is inert \
                   for this species (The Namesake §5.0); Absent if bugbear is unrostered \
                   or minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0],
            },
            extract: Extractor::Full(|v: &FullView| cascade_rules_fired(v, "bugbear")),
        },
        Metric {
            name: "name-pattern-signatures",
            doc: "How many DISTINCT (ElementSource, Author) naming-pattern signatures \
                   this world's placed peoples derive from their society vectors (The \
                   Namesake §5.1(1); target >= 3); Absent if no placed people carries \
                   both psychology vectors",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0],
            },
            extract: Extractor::Full(name_pattern_signatures),
        },
        Metric {
            name: "peoples-placed",
            doc: "How many peoples hold a flagship settlement in this world — the n in \
                   the 1/n chance baseline The Namesake §5.1(2) is judged against, \
                   published so that verdict is re-derivable from rows.csv without \
                   inferring n; Absent if no people is placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0],
            },
            extract: Extractor::Full(peoples_placed),
        },
        Metric {
            name: "name-people-recoverability",
            doc: "The share of this world's placed peoples whose naming-pattern \
                   signature is unique among them — the structure-alone recoverability \
                   of a figure's people (The Namesake §5.1(2); target >= 2x the \
                   1/n_peoples chance baseline); Absent if fewer than two peoples are \
                   placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(name_people_recoverability),
        },
        Metric {
            name: "name-prefix-settlement-scope",
            doc: "The share of this world's occupation founders whose name renders in \
                   exactly one element against the other founders of their own site \
                   (The Namesake §5.2(1); target >= 0.80); Absent if the world has no \
                   founders",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(name_prefix_settlement_scope),
        },
        Metric {
            name: "name-prefix-region-scope",
            doc: "The MEDIAN number of elements this world's occupation founders render \
                   in against every other founder in the world (The Namesake §5.2(2); \
                   target >= 2); Absent if the world has no founders",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0],
            },
            extract: Extractor::Full(name_prefix_region_scope),
        },
        Metric {
            name: "name-prefix-region-full-stack",
            doc: "The share of this world's occupation founders whose region-scope \
                   render spends every element their name carries — the second, \
                   opposite half of The Namesake §5.2(2) (target < 0.50); Absent if the \
                   world has no founders",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(name_prefix_region_full_stack),
        },
        Metric {
            name: "lexicon-regular-kobold",
            doc: "Whether every kobold lexicon Root entry's recorded sound-change \
                   derivation replays byte-identically through evolve (Neogrammarian \
                   regularity, spec §9.1); Absent if the kobold lexicon minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| lexicon_regular(v, "kobold")),
        },
        Metric {
            name: "exposure-sound-goblin",
            doc: "Whether the goblin lexicon is exposure-sound: no concept an INDEPENDENT \
                   re-derivation of exposure classifies Unknown ever backs a Root entry, and \
                   every committed Gap carries a non-empty reason (spec §9.2); Absent if the \
                   goblin lexicon has no entries",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| exposure_sound(v, "goblin")),
        },
        Metric {
            name: "exposure-sound-kobold",
            doc: "Whether the kobold lexicon is exposure-sound: no concept an INDEPENDENT \
                   re-derivation of exposure classifies Unknown ever backs a Root entry, and \
                   every committed Gap carries a non-empty reason (spec §9.2); Absent if the \
                   kobold lexicon has no entries",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| exposure_sound(v, "kobold")),
        },
        Metric {
            name: "hue-depth-goblin",
            doc: "The goblin hue-ladder acquisition depth (2-5) derived from its perception \
                   vector's night-vision (spec §7's pack-depth model card); Absent if goblin \
                   is not in this world's roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| hue_depth(v, "goblin")),
        },
        Metric {
            name: "hue-depth-kobold",
            doc: "The kobold hue-ladder acquisition depth (2-5) derived from its perception \
                   vector's night-vision (spec §7's pack-depth model card); Absent if kobold \
                   is not in this world's roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| hue_depth(v, "kobold")),
        },
        Metric {
            name: "shoreline-development",
            doc: "Shoreline development index: coastline length over the \
                  circumference of the circle with the land's area (1 = \
                  maximally compact); Absent without a shoreline",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 6.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::shoreline_development(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "coast-roughness-slope",
            doc: "Multi-scale coastline-roughness slope, unbanded: the \
                  least-squares slope of ln(shoreline development) against \
                  mesh level, measured at L4/L5/L6 by projecting each \
                  level's cells onto the canonical L6 land/ocean truth \
                  (NearestCellIndex). A companion to shoreline-development, \
                  not a replacement — that estimator is unchanged. Positive \
                  means roughness concentrates at fine scales, which makes \
                  this slope immune to the single-hex land/ocean \
                  alternation exploit that inflates shoreline-development \
                  without changing the coast's coarse shape; Absent if any \
                  of the three levels has no shoreline",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-0.5, 0.0, 0.25, 0.5, 1.0, 1.5],
            },
            extract: Extractor::Terrain(coast_roughness_slope),
        },
        Metric {
            name: "hypsometric-bimodality",
            doc: "Ashman's D between land and ocean elevation populations \
                  (Earth is strongly bimodal); Absent when a world lacks land \
                  or ocean",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::hypsometric_bimodality(
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "shelf-fraction",
            doc: "Fraction of cells within the shelf band (±200 m) of sea \
                  level — the populated shelf Earth's hypsometry keeps",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.15, 0.2, 0.3],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                MetricValue::Number(hornvale_terrain::shape::shelf_fraction(
                    &globe.elevation,
                    globe.sea_level,
                ))
            }),
        },
        Metric {
            name: "continent-count",
            doc: "Connected land components at least 0.5% of the world's \
                  total land cells (Task 9 iteration 3's size floor, \
                  Earth-calibrated: Greenland is ~1.4% of Earth's land and \
                  qualifies, Iceland ~0.07% does not) — the unfloored \
                  fringe of sub-floor fragments is preserved separately by \
                  landmass-count",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                let sizes = hornvale_terrain::shape::land_component_sizes(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                );
                let land: usize = sizes.iter().sum();
                let floor = 0.005 * land as f64;
                MetricValue::Number(sizes.iter().filter(|&&s| s as f64 >= floor).count() as f64)
            }),
        },
        Metric {
            name: "largest-continent-share",
            doc: "Largest land component's share of all land cells; Absent \
                  on a landless world",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 0.9],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                let sizes = hornvale_terrain::shape::land_component_sizes(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                );
                let land: usize = sizes.iter().sum();
                match sizes.first() {
                    Some(largest) if land > 0 => MetricValue::Number(*largest as f64 / land as f64),
                    _ => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "plate-size-gini",
            doc: "Gini coefficient over plate cell counts (Earth's plate \
                  sizes are heavy-tailed; uniform Voronoi scores low)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                let mut counts = vec![0usize; globe.plates.len()];
                for (_, plate) in globe.plate_of.iter() {
                    counts[*plate as usize] += 1;
                }
                match hornvale_terrain::shape::gini(&counts) {
                    Some(g) => MetricValue::Number(g),
                    None => MetricValue::Absent,
                }
            }),
        },
        Metric {
            name: "landmass-count",
            doc: "Every connected land component regardless of size — the \
                  unfloored companion continent-count superseded away \
                  from; reported alongside forever",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                MetricValue::Number(
                    hornvale_terrain::shape::land_component_sizes(
                        v.terrain.geosphere(),
                        &globe.elevation,
                        globe.sea_level,
                    )
                    .len() as f64,
                )
            }),
        },
        // --- Sculpting (Task 12): the carve's own census columns — shelf
        // width by margin polarity, sediment volume, waterfall/delta
        // counts, and the A→B→C escalation diagnostic (spec §8). ---
        Metric {
            name: "shelf-width-passive-median",
            doc: "Median shelf width over PASSIVE-margin coast land cells \
                  (Passive/Interior/Oceanic, mirroring the carve's own \
                  wedge-reach margin split): hops seaward from the coast \
                  cell, each hop to the deepest ocean neighbor, until \
                  depth first exceeds twice the sediment wedge's freeboard \
                  cap or 8 hops are spent — spec §8's passive/active shelf \
                  asymmetry battery (passive median should exceed active); \
                  Absent if the world has no passive-margin coast",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| shelf_width_median(v, false)),
        },
        Metric {
            name: "shelf-width-active-median",
            doc: "Median shelf width over ACTIVE-margin coast land cells: \
                  hops seaward from the coast cell, each hop to the \
                  deepest ocean neighbor, until depth first exceeds twice \
                  the sediment wedge's freeboard cap or 8 hops are spent — \
                  spec §8's passive/active shelf asymmetry battery (active \
                  median should be narrower than passive); Absent if the \
                  world has no active-margin coast",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| shelf_width_median(v, true)),
        },
        Metric {
            name: "sediment-volume",
            doc: "Total deposited sediment volume proxy: Σ sediment \
                  thickness (meters) over every cell, one cell-area unit \
                  per cell — the carve's own volume-proxy convention (spec \
                  §5): repose's receiver-side gains, routing's floodplain/ \
                  playa deposit, the marine wedge/delta fill, and atoll \
                  cap material, all summed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1e5, 2e5, 4e5, 8e5, 1.6e6, 3.2e6],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                let globe = v.terrain.globe();
                MetricValue::Number(globe.sediment_thickness.iter().map(|(_, s)| *s).sum())
            }),
        },
        Metric {
            name: "waterfall-count",
            doc: "Count of waterfall (knickpoint) sites the carve found: \
                  land cells where a high-drainage watercourse crosses a \
                  sharp PRE-carve induration step (spec §5's derived point \
                  observations)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                MetricValue::Number(v.terrain.waterfalls().len() as f64)
            }),
        },
        Metric {
            name: "delta-count",
            doc: "Count of cells a river-mouth delta lobe raised above sea \
                  level (spec §5's top-K discrete deltas) — a cell count, \
                  not a mouth count: each of the top-K mouths can raise the \
                  mouth cell itself plus up to two adjacent hop-1 ocean \
                  cells",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                MetricValue::Number(v.terrain.deltas().len() as f64)
            }),
        },
        Metric {
            name: "rerouted-flow-fraction",
            doc: "The A→B→C escalation diagnostic (spec §8, preregistered, \
                  a permanent census column): the flux-weighted fraction of \
                  the world's 20 largest pre-carve rivers' mainstem cells \
                  whose downhill target changed across the carve. \
                  Thresholds: < 0.10 engine A self-consistent; 0.10-0.30 \
                  flag, Nathan decides; > 0.30 A rejected as sole engine, \
                  engine B enters evaluation",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.5],
            },
            extract: Extractor::Terrain(|v: &TerrainView| {
                MetricValue::Number(v.terrain.globe().carve_reroute_fraction)
            }),
        },
        // --- The Branches (Task 10): the family battery, seed-swept —
        // regularity, monophyly, clean outgroup, inventory closure,
        // divergence magnitude/reality, and merger-induced homophony over
        // the goblinoid family (goblin, hobgoblin, bugbear) against the
        // kobold outgroup (spec §7's family model). ---
        Metric {
            name: "lexicon-regular-family",
            doc: "Whether every daughter's lexicon (goblin, hobgoblin, bugbear, kobold) \
                   is Neogrammarian-regular: every Root's recorded derivation replays \
                   byte-identically through evolve, checked for EVERY daughter in this \
                   world's roster (spec §9.1, generalized family-wide); Absent if no \
                   daughter minted a Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(lexicon_regular_family),
        },
        Metric {
            name: "monophyly-goblinoid",
            doc: "Whether every goblinoid daughter's (goblin, hobgoblin, bugbear) Root \
                   derivation.proto matches an INDEPENDENT re-draw of the shared \
                   \"goblinoid\" family proto-root for that concept (spec §3: cognates \
                   share a proto ancestor) — never reading the family proto back from a \
                   sibling's own recorded derivation; Absent if no goblinoid daughter \
                   minted a Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(monophyly_goblinoid),
        },
        Metric {
            name: "clean-outgroup-kobold",
            doc: "Whether kobold — the family with no siblings — never coincides with the \
                   goblinoid family: for every concept kobold holds as a Root, its \
                   recorded proto-root differs from an INDEPENDENT re-draw of the \
                   \"goblinoid\" family proto-root for that same concept (spec §3's clean \
                   outgroup); Absent if kobold minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(clean_outgroup_kobold),
        },
        Metric {
            name: "inventory-closure-goblin",
            doc: "Whether every goblin lexicon Root's modern form draws only segments in \
                   goblin's own drawn inventory (spec §2.2's nativization contract); \
                   Absent if goblin minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| inventory_closure(v, "goblin")),
        },
        Metric {
            name: "inventory-closure-hobgoblin",
            doc: "Whether every hobgoblin lexicon Root's modern form draws only segments \
                   in hobgoblin's own drawn inventory (spec §2.2's nativization \
                   contract); Absent if hobgoblin minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| inventory_closure(v, "hobgoblin")),
        },
        Metric {
            name: "inventory-closure-bugbear",
            doc: "Whether every bugbear lexicon Root's modern form draws only segments in \
                   bugbear's own drawn inventory (spec §2.2's nativization contract); \
                   Absent if bugbear minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| inventory_closure(v, "bugbear")),
        },
        Metric {
            name: "inventory-closure-kobold",
            doc: "Whether every kobold lexicon Root's modern form draws only segments in \
                   kobold's own drawn inventory (spec §2.2's nativization contract); \
                   Absent if kobold minted no Root",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(|v: &FullView| inventory_closure(v, "kobold")),
        },
        Metric {
            name: "divergence-magnitude-goblin",
            doc: "Count of DISTINCT proto segments (drawn from the shared goblinoid \
                   family proto-phonology) appearing in goblin's own Root proto-roots \
                   that nativize.rs collapses onto an existing goblin inventory segment \
                   (i.e. absent from goblin's own inventory) — the measured cost of \
                   goblin's nativization under the loudness-drawn inventory (spec §3); \
                   Absent if goblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0],
            },
            extract: Extractor::Full(|v: &FullView| divergence_magnitude(v, "goblin")),
        },
        Metric {
            name: "divergence-magnitude-hobgoblin",
            doc: "Count of DISTINCT proto segments (drawn from the shared goblinoid \
                   family proto-phonology) appearing in hobgoblin's own Root proto-roots \
                   that nativize.rs collapses onto an existing hobgoblin inventory \
                   segment (i.e. absent from hobgoblin's own inventory) — the measured \
                   cost of hobgoblin's nativization under the loudness-drawn inventory \
                   (spec §3); Absent if hobgoblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0],
            },
            extract: Extractor::Full(|v: &FullView| divergence_magnitude(v, "hobgoblin")),
        },
        Metric {
            name: "divergence-magnitude-bugbear",
            doc: "Count of DISTINCT proto segments (drawn from the shared goblinoid \
                   family proto-phonology) appearing in bugbear's own Root proto-roots \
                   that nativize.rs collapses onto an existing bugbear inventory segment \
                   (i.e. absent from bugbear's own inventory) — the measured cost of \
                   bugbear's nativization under the loudness-drawn inventory (spec §3); \
                   Absent if bugbear minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0],
            },
            extract: Extractor::Full(|v: &FullView| divergence_magnitude(v, "bugbear")),
        },
        Metric {
            name: "divergence-real",
            doc: "Whether some concept rooted in ALL THREE goblinoid daughters (goblin, \
                   hobgoblin, bugbear) has \u{2265}2 distinct present-day forms — the \
                   seed-swept stemmatics guard (spec §3): descent is proven by shared \
                   INNOVATIONS, not a shared ancestor alone, so a degenerate family whose \
                   daughters are silent aliases of one another must read false; Absent if \
                   no concept is rooted in all three",
            summary: SummaryKind::Flag,
            extract: Extractor::Full(divergence_real),
        },
        Metric {
            name: "homophony-count-goblin",
            doc: "Count of distinct-concept pairs whose goblin Root.modern forms \
                   coincide (two proto-roots merged onto one surface form) — an \
                   observation, not a pass/fail invariant: homophony is legal and \
                   realistic, and this banks the confound L4's reconstruction will fight \
                   (homophones read as one word); Absent if goblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_count(v, "goblin")),
        },
        Metric {
            name: "homophony-count-hobgoblin",
            doc: "Count of distinct-concept pairs whose hobgoblin Root.modern forms \
                   coincide (two proto-roots merged onto one surface form) — an \
                   observation, not a pass/fail invariant; Absent if hobgoblin minted no \
                   Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_count(v, "hobgoblin")),
        },
        Metric {
            name: "homophony-count-bugbear",
            doc: "Count of distinct-concept pairs whose bugbear Root.modern forms \
                   coincide (two proto-roots merged onto one surface form) — an \
                   observation, not a pass/fail invariant; expected highest among the \
                   goblinoid daughters, bugbear drawing the smallest family inventory; \
                   Absent if bugbear minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_count(v, "bugbear")),
        },
        Metric {
            name: "homophony-count-kobold",
            doc: "Count of distinct-concept pairs whose kobold Root.modern forms \
                   coincide (two proto-roots landed on one surface form) — an \
                   observation, not a pass/fail invariant, banked for the clean-outgroup \
                   comparison; Absent if kobold minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_count(v, "kobold")),
        },
        // --- Lexicon homophony, functional-load restricted + attributed
        // (the confusable-core count Nathan targets at ~zero, and the
        // draw-vs-merger split that decides whether family-proto injective
        // assignment alone suffices). `homophony-count-*` above stays as the
        // raw, meaning-blind pair count; these refine it. ---
        Metric {
            name: "core-homophony-goblin",
            doc: "Count of goblin homophone pairs where BOTH concepts are core vocabulary \
                   (universal + body + kin packs) — the functional-load-restricted homophony \
                   the fix drives to zero; always \u{2264} homophony-count-goblin; Absent if \
                   goblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| core_homophony(v, "goblin")),
        },
        Metric {
            name: "core-homophony-hobgoblin",
            doc: "Count of hobgoblin homophone pairs where BOTH concepts are core vocabulary \
                   (universal + body + kin packs); always \u{2264} homophony-count-hobgoblin; \
                   Absent if hobgoblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| core_homophony(v, "hobgoblin")),
        },
        Metric {
            name: "core-homophony-bugbear",
            doc: "Count of bugbear homophone pairs where BOTH concepts are core vocabulary \
                   (universal + body + kin packs); always \u{2264} homophony-count-bugbear; \
                   Absent if bugbear minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| core_homophony(v, "bugbear")),
        },
        Metric {
            name: "core-homophony-kobold",
            doc: "Count of kobold homophone pairs where BOTH concepts are core vocabulary \
                   (universal + body + kin packs); always \u{2264} homophony-count-kobold; \
                   Absent if kobold minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| core_homophony(v, "kobold")),
        },
        Metric {
            name: "homophony-merger-share-goblin",
            doc: "Fraction of goblin colliding surface forms that are MERGERS (colliding roots \
                   carry \u{2265}2 distinct proto-forms — the cascade or nativization made the \
                   collision after the proto) rather than draw-collisions (one shared proto); \
                   Absent if goblin has no collision (an undefined ratio, never reported as 0)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_merger_share(v, "goblin")),
        },
        Metric {
            name: "homophony-merger-share-hobgoblin",
            doc: "Fraction of hobgoblin colliding surface forms that are MERGERS (\u{2265}2 distinct \
                   proto-forms) rather than draw-collisions; Absent if hobgoblin has no collision",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_merger_share(v, "hobgoblin")),
        },
        Metric {
            name: "homophony-merger-share-bugbear",
            doc: "Fraction of bugbear colliding surface forms that are MERGERS (\u{2265}2 distinct \
                   proto-forms) rather than draw-collisions; Absent if bugbear has no collision",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_merger_share(v, "bugbear")),
        },
        Metric {
            name: "homophony-merger-share-kobold",
            doc: "Fraction of kobold colliding surface forms that are MERGERS (\u{2265}2 distinct \
                   proto-forms) rather than draw-collisions; Absent if kobold has no collision",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| homophony_merger_share(v, "kobold")),
        },
        // --- Confusable-vs-free core homophony (spec §10 Q3): the
        // same-semantic-domain subset of core-homophony — the genuinely
        // parsing-costly collisions a listener cannot resolve by topic. Its
        // complement within core-homophony is FREE (cross-domain) homophony,
        // tolerable the way codon degeneracy is. This is what turns "accept the
        // atonal tail" into a measurement. ---
        Metric {
            name: "confusable-homophony-goblin",
            doc: "Count of goblin core homophone pairs that are CONFUSABLE (both concepts \
                   share a semantic domain — universal/body/kin — so they compete in one \
                   context); the same-domain subset of core-homophony-goblin, always \u{2264} it; \
                   the complement is FREE cross-domain homophony; Absent if goblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| confusable_homophony(v, "goblin")),
        },
        Metric {
            name: "confusable-homophony-hobgoblin",
            doc: "Count of hobgoblin core homophone pairs that are CONFUSABLE (both concepts \
                   share a semantic domain); the same-domain subset of core-homophony-hobgoblin, \
                   always \u{2264} it; Absent if hobgoblin minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| confusable_homophony(v, "hobgoblin")),
        },
        Metric {
            name: "confusable-homophony-bugbear",
            doc: "Count of bugbear core homophone pairs that are CONFUSABLE (both concepts \
                   share a semantic domain); the same-domain subset of core-homophony-bugbear, \
                   always \u{2264} it; Absent if bugbear minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| confusable_homophony(v, "bugbear")),
        },
        Metric {
            name: "confusable-homophony-kobold",
            doc: "Count of kobold core homophone pairs that are CONFUSABLE (both concepts \
                   share a semantic domain); the same-domain subset of core-homophony-kobold, \
                   always \u{2264} it; Absent if kobold minted no Root",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 5.0, 8.0, 12.0],
            },
            extract: Extractor::Full(|v: &FullView| confusable_homophony(v, "kobold")),
        },
        // --- The tone tier (spec §11): the realized tone-inventory size (1 for
        // the shipped atonal peoples) and the distinguishable-syllable capacity
        // the floor guarantees. Tone-count >1 and the floor are exercised by a
        // test-only tone-capable roster (see the lab's roster controls). ---
        Metric {
            name: "tone-count-goblin",
            doc: "Size of goblin's realized tone inventory (spec §11): 1 for an atonal \
                   people; >1 only for a tone-capable species; Absent if goblin is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0],
            },
            extract: Extractor::Full(|v: &FullView| tone_count_metric(v, "goblin")),
        },
        Metric {
            name: "tone-count-kobold",
            doc: "Size of kobold's realized tone inventory (spec §11): 1 for an atonal \
                   people; Absent if kobold is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0],
            },
            extract: Extractor::Full(|v: &FullView| tone_count_metric(v, "kobold")),
        },
        Metric {
            name: "distinguishable-capacity-goblin",
            doc: "Goblin's distinguishable-syllable capacity (spec §2.3): onset × nucleus × \
                   coda fillings, a lower bound on distinct syllables (tone folded into the \
                   nucleus); Absent if goblin is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0],
            },
            extract: Extractor::Full(|v: &FullView| distinguishable_capacity_metric(v, "goblin")),
        },
        Metric {
            name: "distinguishable-capacity-bugbear",
            doc: "Bugbear's distinguishable-syllable capacity (spec §2.3): onset × nucleus × \
                   coda fillings; bugbear draws the smallest family inventory; Absent if bugbear \
                   is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0],
            },
            extract: Extractor::Full(|v: &FullView| distinguishable_capacity_metric(v, "bugbear")),
        },
        Metric {
            name: "distinguishable-capacity-kobold",
            doc: "Kobold's distinguishable-syllable capacity (spec §2.3): onset × nucleus × \
                   coda fillings; Absent if kobold is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[24.0, 48.0, 96.0, 192.0, 384.0, 768.0, 1536.0],
            },
            extract: Extractor::Full(|v: &FullView| distinguishable_capacity_metric(v, "kobold")),
        },
        // --- BIO-2 (Task 6): the six life-history traits (spec §4/§5), a
        // pure f(Mass, MetabolicClass) with zero draws — every row of a
        // study reads the same value for a given roster. Registered per
        // species (goblin, kobold), matching the `tone-count-{species}`
        // family's convention (see above) — the campaign's headline
        // cross-species claim (ectotherm kobold vs endotherm goblinoids)
        // is only queryable if both species are metrics. ---
        Metric {
            name: "lifespan-years-goblin",
            doc: "Goblin's maximum lifespan in years (BIO-2 spec §4); Absent \
                   if goblin is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[20.0, 40.0, 60.0, 80.0, 100.0],
            },
            extract: Extractor::Full(|v: &FullView| species_lifespan_metric(v, "goblin")),
        },
        Metric {
            name: "lifespan-years-kobold",
            doc: "Kobold's maximum lifespan in years (BIO-2 spec §4); Absent \
                   if kobold is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[20.0, 40.0, 60.0, 80.0, 100.0],
            },
            extract: Extractor::Full(|v: &FullView| species_lifespan_metric(v, "kobold")),
        },
        Metric {
            name: "age-at-maturity-years-goblin",
            doc: "Goblin's age at first reproduction in years (BIO-2 spec §4); \
                   Absent if goblin is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[5.0, 10.0, 15.0, 20.0, 25.0],
            },
            extract: Extractor::Full(|v: &FullView| species_age_at_maturity_metric(v, "goblin")),
        },
        Metric {
            name: "age-at-maturity-years-kobold",
            doc: "Kobold's age at first reproduction in years (BIO-2 spec §4); \
                   Absent if kobold is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[5.0, 10.0, 15.0, 20.0, 25.0],
            },
            extract: Extractor::Full(|v: &FullView| species_age_at_maturity_metric(v, "kobold")),
        },
        Metric {
            name: "basal-metabolic-rate-w-goblin",
            doc: "Goblin's reference-temperature basal metabolic rate in watts \
                   (BIO-2 spec §4); Absent only if goblin is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[10.0, 20.0, 30.0, 40.0, 50.0],
            },
            extract: Extractor::Full(|v: &FullView| {
                species_basal_metabolic_rate_metric(v, "goblin")
            }),
        },
        Metric {
            name: "basal-metabolic-rate-w-kobold",
            doc: "Kobold's reference-temperature basal metabolic rate in watts \
                   (BIO-2 spec §4); Absent only if kobold is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[10.0, 20.0, 30.0, 40.0, 50.0],
            },
            extract: Extractor::Full(|v: &FullView| {
                species_basal_metabolic_rate_metric(v, "kobold")
            }),
        },
        Metric {
            name: "reproductive-tempo-goblin",
            doc: "Goblin's reproductive output on the r-K axis, 0 (fast/prolific) \
                   ... 1 (slow/sparse) (BIO-2 spec §4/CAP-2); Absent if goblin is \
                   off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| species_reproductive_tempo_metric(v, "goblin")),
        },
        Metric {
            name: "reproductive-tempo-kobold",
            doc: "Kobold's reproductive output on the r-K axis, 0 (fast/prolific) \
                   ... 1 (slow/sparse) (BIO-2 spec §4/CAP-2); Absent if kobold is \
                   off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| species_reproductive_tempo_metric(v, "kobold")),
        },
        Metric {
            name: "generation-length-years-goblin",
            doc: "Goblin's generation length in years (BIO-2 spec §5, MEM-7's \
                   handle); Absent if goblin is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[10.0, 20.0, 30.0, 40.0, 50.0],
            },
            extract: Extractor::Full(|v: &FullView| species_generation_length_metric(v, "goblin")),
        },
        Metric {
            name: "generation-length-years-kobold",
            doc: "Kobold's generation length in years (BIO-2 spec §5, MEM-7's \
                   handle); Absent if kobold is off-roster or Ametabolic",
            summary: SummaryKind::Numeric {
                bucket_edges: &[10.0, 20.0, 30.0, 40.0, 50.0],
            },
            extract: Extractor::Full(|v: &FullView| species_generation_length_metric(v, "kobold")),
        },
        Metric {
            name: "pace-of-life-goblin",
            doc: "Goblin's overall life-history speed, 0 (fast) ... 1 (slow) — \
                   absolute and roster-independent (BIO-2 spec §5); Absent only \
                   if goblin is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| species_pace_of_life_metric(v, "goblin")),
        },
        Metric {
            name: "pace-of-life-kobold",
            doc: "Kobold's overall life-history speed, 0 (fast) ... 1 (slow) — \
                   absolute and roster-independent (BIO-2 spec §5); Absent only \
                   if kobold is off-roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Full(|v: &FullView| species_pace_of_life_metric(v, "kobold")),
        },
        // --- The Chorus (C4, LANG-41): the six census-visible dial metrics
        // over `accounts_from` — how far each placed culture's epistemic
        // account of the ground truth strays from the truth (distortion),
        // from each other (distinctiveness), and how much of a substituted
        // fact a listener could still recover (recoverability); plus the
        // dial's calibration checks (variance vs param-spread, and whether
        // sky capability actually predicts sky distortion). ---
        Metric {
            name: "chorus-distortion",
            doc: "Mean distortion() over every placed culture's account (C4 LANG-41); \
                   Absent if no culture placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.1, 0.25, 0.5, 0.75, 0.9],
            },
            extract: Extractor::Full(chorus_distortion_metric),
        },
        Metric {
            name: "chorus-distinctiveness",
            doc: "Mean pairwise distinctiveness() across every placed culture's account \
                   (C4 LANG-41); Absent if fewer than 2 cultures placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.1, 0.25, 0.5, 0.75, 0.9],
            },
            extract: Extractor::Full(chorus_distinctiveness_metric),
        },
        Metric {
            name: "chorus-recoverability",
            doc: "Mean recoverability() over every placed culture's account (C4 LANG-41); \
                   Absent if no culture placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.1, 0.25, 0.5, 0.75, 0.9],
            },
            extract: Extractor::Full(chorus_recoverability_metric),
        },
        Metric {
            name: "chorus-variance",
            doc: "Population variance of per-culture distortion() (C4 LANG-41) — the \
                   vacuity number, read against chorus-param-spread: a low variance can mean \
                   either every voice is genuinely alike or every voice hit the same floor; \
                   Absent if fewer than 2 cultures placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.001, 0.01, 0.05, 0.1],
            },
            extract: Extractor::Full(chorus_variance_metric),
        },
        Metric {
            name: "chorus-param-spread",
            doc: "Mean pairwise absolute difference in sky_capability across every placed \
                   culture (C4 LANG-41) — the input-side companion to chorus-variance's \
                   output-side number; Absent if fewer than 2 cultures placed",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.05, 0.1, 0.2, 0.4],
            },
            extract: Extractor::Full(chorus_param_spread_metric),
        },
        Metric {
            name: "chorus-sky-calibration",
            doc: "Kendall tau between per-culture sky_capability and per-culture \
                   domain_distortion(..., \"sky\") over strictly-comparable pairs (both the \
                   capability and the distortion differ) (C4 LANG-41); expected sign \u{2264} 0 \
                   (distortion falls as capability rises); Absent if fewer than 2 cultures \
                   placed or no strictly-comparable pair exists (e.g. every pair ties on \
                   sky distortion)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-0.5, 0.0, 0.5],
            },
            extract: Extractor::Full(chorus_sky_calibration_metric),
        },
        // --- The Contour (Task 4): the measurement instrument, built ahead
        // of the mechanism (spec 2.2/decision 0096) so Task 5's baseline is
        // honest. M2/M3 read `occupation_records` — the same decoder
        // `windows/almanac` and The Vestige already share — filtered to
        // still-alive occupations at bake end.
        //
        // M2's `peak_population` caveat: `OccupationRecord::peak_population`
        // is each occupation's historical HIGH-WATER MARK
        // (`Bake::touch` only ever raises it — `history_bake.rs`), not a
        // bake-end census. There is no end-state population accessor in the
        // data model today: the live per-epoch figure
        // (`Bake::Community::population`) is bake-internal state that
        // `history_bake::bake` discards when it returns `History` (only
        // `records` — carrying `peak_population` — survives). So M2 reads
        // "largest peak share among communities alive at bake end," not a
        // literal simultaneous snapshot; see task-4-report.md round 2 for
        // the finding and the proposed accessor if a true end-state figure
        // is ever needed.
        //
        // M4 (defensibility-capacity-rank-corr), round 3 / spec §2.4
        // amendment 4: registered on PRESENT-DAY terrain, not the bake's
        // own final era. `bake_history_from` computes and discards its own
        // final-era `(ConnectionGraph, capacity)` on every build path, and
        // `FullView` has no field for it (round 2's finding, still true);
        // present-day terrain/climate is a DIFFERENT, honestly-labelled
        // reading — spec §2.2's claim is about whether defensible ground is
        // also poor ground, a structural fact about the geography that
        // present-day terrain samples fully, so the substitution is
        // legitimate as long as it says so out loud (the metric's own `doc`
        // carries the label, not just this comment — see
        // `spearman_defensibility_capacity`).
        Metric {
            name: "peoples-alive-at-bake-end",
            doc: "M3: how many distinct peoples still hold a live community when the \
                  bake ends — the decision-0089 compliance reading",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: Extractor::Full(|v: &FullView| {
                let mut peoples = std::collections::BTreeSet::new();
                for occ in occupation_records(v.world())
                    .into_iter()
                    .filter(|o| o.is_alive())
                {
                    peoples.insert(occ.core.people);
                }
                MetricValue::Number(peoples.len() as f64)
            }),
        },
        Metric {
            name: "largest-holding-share",
            doc: "M2: the largest live community's PEAK population as a share of the \
                  summed peak population of every community alive at bake end \
                  (peak_population is each occupation's historical high-water mark, not \
                  a true bake-end census — no end-state population accessor exists \
                  today; see task-4-report.md) — the entity-size reading the criticality \
                  campaigns never took",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.05, 0.1, 0.2, 0.3, 0.5, 0.7],
            },
            extract: Extractor::Full(|v: &FullView| {
                let pops: Vec<f64> = occupation_records(v.world())
                    .into_iter()
                    .filter(|o| o.is_alive())
                    .map(|o| f64::from(o.core.peak_population))
                    .collect();
                let total: f64 = pops.iter().sum();
                if total <= 0.0 {
                    return MetricValue::Absent;
                }
                let max = pops.iter().copied().fold(f64::NEG_INFINITY, f64::max);
                MetricValue::Number(max / total)
            }),
        },
        Metric {
            name: "defensibility-capacity-rank-corr",
            doc: "M4: Spearman rank correlation between a habitable cell's weakest-point \
                  defensibility and its carrying capacity, BOTH READ FROM PRESENT-DAY \
                  terrain, climate, and connection graph — NOT the bake's own final era, \
                  which can differ on a world with real orbital forcing (spec §2.4 \
                  amendment 4). Checks §2.2's structural claim that defensible ground \
                  is also poor ground, on the geography as it stands today. Ties get \
                  average ranks; Absent if fewer than 2 habitable cells, or if either \
                  series is constant (no variance, so no correlation is defined)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-0.6, -0.3, 0.0, 0.3, 0.6],
            },
            extract: Extractor::Full(spearman_defensibility_capacity),
        },
    ]
}

/// Every placed culture's account, read straight off the world (C4
/// LANG-41): a thin passthrough so the six chorus metrics below share one
/// call site rather than each re-deriving voices.
fn chorus_voices(v: &FullView) -> Vec<ChorusVoice> {
    accounts_from(v.world(), v.terrain(), v.climate())
}

/// Mean `distortion()` over `voices` (C4 LANG-41). `Absent` if `voices` is
/// empty — there is no culture to average over. Pure over an explicit
/// voice list (rather than a `&FullView`) so the empty/singleton edge
/// cases can be driven directly, without hunting for a 0-people seed.
fn chorus_distortion_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.is_empty() {
        return MetricValue::Absent;
    }
    let mean = voices
        .iter()
        .map(|voice| distortion(&voice.account))
        .sum::<f64>()
        / voices.len() as f64;
    MetricValue::Number(mean)
}

fn chorus_distortion_metric(v: &FullView) -> MetricValue {
    chorus_distortion_metric_over(&chorus_voices(v))
}

/// Mean `recoverability()` over `voices` (C4 LANG-41). `Absent` if `voices`
/// is empty.
fn chorus_recoverability_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.is_empty() {
        return MetricValue::Absent;
    }
    let mean = voices
        .iter()
        .map(|voice| recoverability(&voice.account))
        .sum::<f64>()
        / voices.len() as f64;
    MetricValue::Number(mean)
}

fn chorus_recoverability_metric(v: &FullView) -> MetricValue {
    chorus_recoverability_metric_over(&chorus_voices(v))
}

/// Population variance of `distortion()` over `voices` (C4 LANG-41): the
/// vacuity number — a low reading here is ambiguous between "every voice is
/// genuinely alike" and "every voice hit the same floor," which is why it's
/// read against [`chorus_param_spread_metric_over`]. `Absent` if fewer than
/// 2 voices (a single value has no variance worth reporting).
fn chorus_variance_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.len() < 2 {
        return MetricValue::Absent;
    }
    let distortions: Vec<f64> = voices
        .iter()
        .map(|voice| distortion(&voice.account))
        .collect();
    let n = distortions.len() as f64;
    let mean = distortions.iter().sum::<f64>() / n;
    let variance = distortions.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n;
    MetricValue::Number(variance)
}

fn chorus_variance_metric(v: &FullView) -> MetricValue {
    chorus_variance_metric_over(&chorus_voices(v))
}

/// Mean pairwise `distinctiveness()` across every unordered pair of `voices`
/// (C4 LANG-41). `Absent` if fewer than 2 voices — a pair requires two.
fn chorus_distinctiveness_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.len() < 2 {
        return MetricValue::Absent;
    }
    let mut total = 0.0;
    let mut pairs = 0usize;
    for i in 0..voices.len() {
        for j in (i + 1)..voices.len() {
            total += distinctiveness(&voices[i].account, &voices[j].account);
            pairs += 1;
        }
    }
    MetricValue::Number(total / pairs as f64)
}

fn chorus_distinctiveness_metric(v: &FullView) -> MetricValue {
    chorus_distinctiveness_metric_over(&chorus_voices(v))
}

/// Mean pairwise absolute difference in `sky_capability` across every
/// unordered pair of `voices` (C4 LANG-41) — the input-side spread that
/// [`chorus_variance_metric_over`]'s output-side number is read against.
/// `Absent` if fewer than 2 voices.
fn chorus_param_spread_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.len() < 2 {
        return MetricValue::Absent;
    }
    let mut total = 0.0;
    let mut pairs = 0usize;
    for i in 0..voices.len() {
        for j in (i + 1)..voices.len() {
            total += (voices[i].params.sky_capability - voices[j].params.sky_capability).abs();
            pairs += 1;
        }
    }
    MetricValue::Number(total / pairs as f64)
}

fn chorus_param_spread_metric(v: &FullView) -> MetricValue {
    chorus_param_spread_metric_over(&chorus_voices(v))
}

/// Kendall tau between per-voice `sky_capability` and per-voice
/// `domain_distortion(..., "sky")`, restricted to strictly-comparable pairs
/// (C4 LANG-41): a pair where either coordinate ties contributes nothing —
/// neither concordant nor discordant, and is excluded from the denominator
/// entirely (this is not the tau-b tie correction; it is a stricter
/// same-plane-only tau). Expected sign \u{2264} 0 (distortion should fall as
/// capability rises). `Absent` if fewer than 2 voices, or if every pair
/// ties on at least one coordinate (no strictly-comparable pair survives —
/// e.g. every voice's sky distortion is pinned to the same value).
fn chorus_sky_calibration_metric_over(voices: &[ChorusVoice]) -> MetricValue {
    if voices.len() < 2 {
        return MetricValue::Absent;
    }
    let points: Vec<(f64, f64)> = voices
        .iter()
        .map(|voice| {
            let cap = voice.params.sky_capability;
            let sky_dist = domain_distortion(&voice.account, &voice.params, "sky");
            (cap, sky_dist)
        })
        .collect();

    let mut concordant = 0i64;
    let mut discordant = 0i64;
    let mut strict_pairs = 0i64;
    for i in 0..points.len() {
        for j in (i + 1)..points.len() {
            let (x1, y1) = points[i];
            let (x2, y2) = points[j];
            if x1 == x2 || y1 == y2 {
                continue;
            }
            strict_pairs += 1;
            let same_sign = (x1 - x2).signum() == (y1 - y2).signum();
            if same_sign {
                concordant += 1;
            } else {
                discordant += 1;
            }
        }
    }

    if strict_pairs == 0 {
        return MetricValue::Absent;
    }
    MetricValue::Number((concordant - discordant) as f64 / strict_pairs as f64)
}

fn chorus_sky_calibration_metric(v: &FullView) -> MetricValue {
    chorus_sky_calibration_metric_over(&chorus_voices(v))
}

// --- The Contour (Task 4, round 3): the Spearman rank-correlation helpers
// M4 (`defensibility-capacity-rank-corr`) uses. ---

/// Average-rank ranking of `values` (Spearman's standard tie handling):
/// ascending order via `f64::total_cmp` (never `partial_cmp().unwrap()`),
/// with a tied group of values sharing the MEAN of the 1-based integer
/// ranks its group spans, rather than an arbitrary tie-break order. Returns
/// one rank per input value, in `values`' original order (not sorted
/// order), so the caller can zip it against a second series' ranks.
fn average_ranks(values: &[f64]) -> Vec<f64> {
    let mut order: Vec<usize> = (0..values.len()).collect();
    order.sort_by(|&a, &b| values[a].total_cmp(&values[b]));
    let mut ranks = vec![0.0; values.len()];
    let mut i = 0;
    while i < order.len() {
        let mut j = i;
        while j + 1 < order.len() && values[order[j + 1]] == values[order[i]] {
            j += 1;
        }
        // Positions i..=j (0-based, already sorted) share the mean of the
        // 1-based ranks (i+1)..=(j+1) their tie group spans.
        let shared_rank = ((i + 1) + (j + 1)) as f64 / 2.0;
        for &idx in &order[i..=j] {
            ranks[idx] = shared_rank;
        }
        i = j + 1;
    }
    ranks
}

/// Pearson correlation of `xs` and `ys` (equal length) — Spearman IS this,
/// applied to `average_ranks`' output rather than the raw values. `None`
/// when fewer than 2 points, or when either series is constant (zero
/// variance leaves the coefficient undefined, not zero).
fn pearson_correlation(xs: &[f64], ys: &[f64]) -> Option<f64> {
    debug_assert_eq!(xs.len(), ys.len(), "paired series must be the same length");
    let n = xs.len();
    if n < 2 {
        return None;
    }
    let mean_x = xs.iter().sum::<f64>() / n as f64;
    let mean_y = ys.iter().sum::<f64>() / n as f64;
    let (mut cov, mut var_x, mut var_y) = (0.0, 0.0, 0.0);
    for i in 0..n {
        let dx = xs[i] - mean_x;
        let dy = ys[i] - mean_y;
        cov += dx * dy;
        var_x += dx * dx;
        var_y += dy * dy;
    }
    if var_x <= 0.0 || var_y <= 0.0 {
        return None;
    }
    Some(cov / (var_x.sqrt() * var_y.sqrt()))
}

/// M4's extractor (spec §2.4 amendment 4): Spearman rank correlation
/// between [`hornvale_worldgen::weakest_point_defensibility`] and
/// [`hornvale_demography::carrying_capacity`], over every PRESENT-DAY
/// habitable cell (`v.climate().habitability()`) — NOT the bake's own final
/// era. `hornvale_worldgen::connection_graph_of` is the crate's existing
/// present-day-graph entry point (already used by the legibility surface
/// and the DoD check), reused here wholesale rather than reconstructed by
/// hand; `hornvale_demography::carrying_capacity` over
/// `hornvale_worldgen::carrying_inputs_of` is the SAME species-agnostic
/// capacity field `bake_history_from` itself feeds into the bake (up to
/// its private `SETTLERS_PER_CAPACITY` scale, which cannot move a RANK
/// correlation — Spearman is invariant under any positive linear
/// rescaling). Cells iterate in ascending `CellId` order
/// (`Geosphere::cells()`), so this is deterministic without an explicit
/// sort of the cell set itself.
fn spearman_defensibility_capacity(v: &FullView) -> MetricValue {
    let geo = v.terrain().geosphere();
    let habitability = v.climate().habitability();
    let capacity = hornvale_demography::carrying_capacity(
        geo,
        &hornvale_worldgen::carrying_inputs_of(geo, v.terrain(), v.climate()),
    );
    let graph = hornvale_worldgen::connection_graph_of(
        v.world(),
        &hornvale_worldgen::GraphConfig::default(),
    );

    let mut defs: Vec<f64> = Vec::new();
    let mut caps: Vec<f64> = Vec::new();
    for cell in geo.cells() {
        if !*habitability.get(cell) {
            continue;
        }
        defs.push(hornvale_worldgen::weakest_point_defensibility(&graph, cell));
        caps.push(capacity.at(cell));
    }
    if defs.len() < 2 {
        return MetricValue::Absent;
    }
    match pearson_correlation(&average_ranks(&defs), &average_ranks(&caps)) {
        Some(rho) => MetricValue::Number(rho),
        None => MetricValue::Absent,
    }
}

/// The median of `values` (sorted in place by `total_cmp`); `None` when
/// empty. An even-length input averages its two middle values.
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

/// Shelf width (Sculpting Task 12, spec §8) from a single coast land cell:
/// hops seaward, each hop stepping to the current cell's deepest ocean
/// neighbor (`CellId`-ascending tiebreak among equally deep candidates),
/// until a stepped-to cell's depth first exceeds `cap_depth_m`, or 8 hops
/// are spent. A coast cell always has at least one ocean neighbor by
/// definition; a dead end thereafter (an ocean cell with no further ocean
/// neighbor — a landlocked single-cell inlet) returns however many hops
/// were completed.
fn shelf_width_hops(v: &TerrainView, coast: CellId, cap_depth_m: f64) -> u32 {
    let geo = v.terrain.geosphere();
    let mut cur = coast;
    for hop in 1..=8u32 {
        let mut candidates: Vec<CellId> = geo
            .neighbors(cur)
            .iter()
            .copied()
            .filter(|&n| v.terrain.is_ocean(n))
            .collect();
        if candidates.is_empty() {
            return hop - 1;
        }
        candidates.sort_by(|a, b| {
            v.terrain
                .elevation_at(*a)
                .get()
                .total_cmp(&v.terrain.elevation_at(*b).get())
                .then(a.0.cmp(&b.0))
        });
        let next = candidates[0];
        let depth = v.terrain.sea_level().get() - v.terrain.elevation_at(next).get();
        if depth > cap_depth_m {
            return hop;
        }
        cur = next;
    }
    8
}

/// Median shelf width (`shelf_width_hops`) over every coast land cell
/// (a land cell with at least one ocean neighbor) whose own `MarginPolarity`
/// is Active (`active_only == true`) or not (`Passive`/`Interior`/
/// `Oceanic`, mirroring `deposit_wedge`'s own margin split, `active_only ==
/// false`). The cap depth is twice `CarveParams::wedge_freeboard_m` — the
/// carve's own physical shelf cap, doubled so a coast sitting right at the
/// cap still registers a nonzero width; tracks any future retuning of
/// `wedge_freeboard_m` automatically rather than duplicating the constant.
/// `Absent` when the requested margin group has no coast cells at all.
fn shelf_width_median(v: &TerrainView, active_only: bool) -> MetricValue {
    let geo = v.terrain.geosphere();
    let cap_depth_m = 2.0 * CarveParams::default().wedge_freeboard_m;
    let mut widths: Vec<f64> = Vec::new();
    for cell in geo.cells() {
        if v.terrain.is_ocean(cell) {
            continue;
        }
        let is_coast = geo.neighbors(cell).iter().any(|&n| v.terrain.is_ocean(n));
        if !is_coast {
            continue;
        }
        let is_active = matches!(v.terrain.material_at(cell).margin, MarginPolarity::Active);
        if is_active != active_only {
            continue;
        }
        widths.push(f64::from(shelf_width_hops(v, cell, cap_depth_m)));
    }
    match median(&mut widths) {
        Some(m) => MetricValue::Number(m),
        None => MetricValue::Absent,
    }
}

/// Multi-scale coastline-roughness slope (rift-and-fit spec §7): a
/// companion to `shoreline-development`, not a replacement — that estimator
/// is unchanged. Builds the level 4/5/6 `Geosphere`s and, for each, derives
/// a land mask by looking up each cell's NEAREST canonical L6 cell (by
/// unit-sphere position, `NearestCellIndex::nearest_to_position` — the same
/// projection the scene/region window uses to sample canonical truth from a
/// coarser or finer mesh) and testing that L6 cell's elevation against the
/// world's sea level; at k = 6 every cell is its own nearest, so the mapping
/// is the identity. `D_k = shoreline_development_of_mask` at each level,
/// then the function returns the least-squares slope of `ln D_k` regressed
/// on `k` (three points, k = 4, 5, 6). A positive slope means roughness
/// concentrates at fine scales; a single-hex land/ocean alternation (the
/// exploit that inflates the plain `shoreline-development` index without
/// changing the coast's coarse shape) reads as a steep positive slope here,
/// since `D_4`/`D_5` stay modest while `D_6` spikes. `Absent` if any of the
/// three levels has no shoreline (`shoreline_development_of_mask` returns
/// `None` there).
fn coast_roughness_slope(v: &TerrainView) -> MetricValue {
    let l6_geo = v.terrain.geosphere();
    let globe = v.terrain.globe();
    let l6_index = hornvale_kernel::NearestCellIndex::new(l6_geo);
    let mut ks: Vec<f64> = Vec::new();
    let mut ys: Vec<f64> = Vec::new();
    for k in [4u32, 5, 6] {
        let geo_k = hornvale_kernel::Geosphere::new(k);
        let land = hornvale_kernel::CellMap::from_fn(&geo_k, |cell| {
            let pos = geo_k.position(cell);
            let l6_cell = l6_index.nearest_to_position(l6_geo, pos);
            *globe.elevation.get(l6_cell) >= globe.sea_level
        });
        match hornvale_terrain::shape::shoreline_development_of_mask(&geo_k, &land) {
            Some(d) => {
                ks.push(k as f64);
                ys.push(hornvale_kernel::math::ln(d));
            }
            None => return MetricValue::Absent,
        }
    }
    let n = ks.len() as f64;
    let mean_k = ks.iter().sum::<f64>() / n;
    let mean_y = ys.iter().sum::<f64>() / n;
    let (mut num, mut den) = (0.0_f64, 0.0_f64);
    for (k, y) in ks.iter().zip(ys.iter()) {
        num += (k - mean_k) * (y - mean_y);
        den += (k - mean_k) * (k - mean_k);
    }
    if den == 0.0 {
        MetricValue::Absent
    } else {
        MetricValue::Number(num / den)
    }
}

/// A flagship pantheon's structural signature — every lexical channel
/// (names, epithets, tenets) deliberately absent (spec §9.2).
struct PantheonSig {
    /// solar | lunar | ambient — the venue of the perceived top phenomenon.
    domain: &'static str,
    /// Number of deities.
    size: usize,
    /// Whether one deity presides. Part of the structural signature (spec
    /// §9.2); `pick_kobold`'s fixed rule doesn't need it to separate the
    /// two pantheons in this task, but it stays on the struct for the
    /// distributional-twin control and other structure-only consumers to
    /// come (spec §9.2, §13).
    #[allow(dead_code)]
    ranked: bool,
    /// organized | folk.
    cult: String,
    /// Fraction of the pantheon's source phenomena that are periodic.
    cyclic_share: f64,
}

fn pantheon_sig(v: &FullView, species: &str) -> Option<PantheonSig> {
    let flagship = flagship_of(v.world(), species)?;
    let beliefs = hornvale_religion::beliefs_held_by(v.world(), flagship.id);
    if beliefs.is_empty() {
        return None;
    }
    // Pantheons derive from the same first-place, hemisphere-culled vantage
    // religion's genesis observes (SEQ-4/SEQ-5).
    let seen =
        observed_phenomena_as_in_from(v.world(), v.components(), species, v.climate()).ok()?;
    let top = seen.first()?;
    let domain = match top.venue {
        hornvale_kernel::Venue::DaySky => "solar",
        hornvale_kernel::Venue::NightSky => "lunar",
        hornvale_kernel::Venue::Ambient => "ambient",
    };
    let members = &seen[..beliefs.len().min(seen.len())];
    let cyclic = members.iter().filter(|p| p.period_days.is_some()).count();
    Some(PantheonSig {
        domain,
        size: beliefs.len(),
        ranked: beliefs.iter().any(|b| b.high_god),
        cult: hornvale_religion::cult_form_held_by(v.world(), flagship.id)
            .unwrap_or_else(|| "folk".to_string()),
        cyclic_share: cyclic as f64 / members.len().max(1) as f64,
    })
}

/// The sentiment of `species`' pantheon head — the deity that presides over
/// THAT people, which is the only scale at which "presides" means anything.
/// `None` when the people placed no flagship or holds no beliefs.
///
/// A people's beliefs mint salience-descending, so its first belief is its
/// head; `beliefs_held_by` preserves that order.
fn species_head_sentiment(v: &FullView, species: &str) -> Option<String> {
    let flagship = flagship_of(v.world(), species)?;
    let beliefs = hornvale_religion::beliefs_held_by(v.world(), flagship.id);
    beliefs.first().map(|b| b.sentiment.as_str().to_string())
}

/// The fixed blind-attribution rule (spec §9.2, preregistered): given two
/// unlabeled signatures, pick the kobold. Structure only — no lexical
/// input. Returns the index (0/1), or None when indistinguishable.
fn pick_kobold(pair: [&PantheonSig; 2]) -> Option<usize> {
    // Rule 1: exactly one lunar-headed pantheon → it is the kobolds'.
    match (pair[0].domain == "lunar", pair[1].domain == "lunar") {
        (true, false) => return Some(0),
        (false, true) => return Some(1),
        _ => {}
    }
    // Rule 2: the more cyclic pantheon (moon-and-star gods recur).
    if pair[0].cyclic_share != pair[1].cyclic_share {
        return Some(if pair[0].cyclic_share > pair[1].cyclic_share {
            0
        } else {
            1
        });
    }
    // Rule 3: the larger pantheon (the boosted night sky seats more gods).
    if pair[0].size != pair[1].size {
        return Some(if pair[0].size > pair[1].size { 0 } else { 1 });
    }
    None // indistinguishable: scored as a miss
}

/// Recompute a species flagship's subsistence surplus directly from the
/// climate/terrain providers, independent of the culture-committed inputs —
/// the independence the slave calibration needs (spec §9.2).
fn flagship_surplus(v: &SettlementView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(v.world(), species) else {
        return MetricValue::Absent;
    };
    let Some(Value::Number(cell_id)) = v
        .world()
        .ledger
        .value_of(info.id, hornvale_settlement::CELL_ID)
    else {
        return MetricValue::Absent;
    };
    let cell = CellId(*cell_id as u32);
    let class = hornvale_worldgen::biome_class(v.climate().biome_at(cell));
    let surplus =
        (hornvale_culture::fertility(class) * v.climate().moisture_at(cell)).clamp(0.0, 1.0);
    MetricValue::Number(surplus)
}

/// Recompute whether a species flagship's cell borders an ocean cell,
/// directly from the terrain provider.
fn flagship_coastal(v: &SettlementView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(v.world(), species) else {
        return MetricValue::Absent;
    };
    let Some(Value::Number(cell_id)) = v
        .world()
        .ledger
        .value_of(info.id, hornvale_settlement::CELL_ID)
    else {
        return MetricValue::Absent;
    };
    let cell = CellId(*cell_id as u32);
    let coastal = v
        .terrain()
        .geosphere()
        .neighbors(cell)
        .iter()
        .any(|n| v.terrain().is_ocean(*n));
    MetricValue::Flag(coastal)
}

/// The flagship settlement's committed latitude (the ledger's first
/// `IS_SETTLEMENT` subject) — the alignment-drift metric's observing site.
/// `None` if there is no settlement, or the settlement carries no latitude
/// fact (the pre-vantage behavior, matching `place_coord` in
/// `windows/worldgen/src/lib.rs`).
fn flagship_latitude(v: &SettlementView) -> Option<f64> {
    let subject = v
        .world()
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .next()?
        .subject;
    match v
        .world()
        .ledger
        .value_of(subject, hornvale_settlement::LATITUDE)?
    {
        Value::Number(n) => Some(*n),
        _ => None,
    }
}

/// Count settlements peopled by `species`.
fn species_settlement_count(v: &SettlementView, species: &str) -> f64 {
    v.world()
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .filter(|f| hornvale_species::species_of(v.world(), f.subject).as_deref() == Some(species))
        .count() as f64
}

/// Every generated name attributed to `species` in this world: its
/// settlement names (from the places registry) plus, if it holds a
/// pantheon, every deity name and epithet its flagship's beliefs carry.
/// Empty if the species placed nothing and holds no pantheon.
fn species_generated_names(v: &FullView, species: &str) -> Vec<String> {
    let mut names: Vec<String> = hornvale_terrain::places(v.world())
        .into_iter()
        .filter(|p| hornvale_species::species_of(v.world(), p.id).as_deref() == Some(species))
        .map(|p| p.name)
        .collect();
    if let Some(info) = flagship_of(v.world(), species) {
        for belief in hornvale_religion::beliefs_held_by(v.world(), info.id) {
            names.push(belief.deity);
            names.push(belief.epithet);
        }
    }
    names
}

/// Whether every generated name attributed to `species` in this world
/// re-validates against `species`' own re-derived phonology; `Absent` if it
/// produced no names.
fn phonotactic_validity(v: &FullView, species: &str) -> MetricValue {
    let names = species_generated_names(v, species);
    if names.is_empty() {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    let attested = lex(v, species)
        .map(|lex| attested_roman_forms(&lex))
        .unwrap_or_default();
    MetricValue::Flag(
        names
            .iter()
            .all(|n| is_phonotactically_valid(n, &ph, &attested)),
    )
}

/// Whether `committed` carries surface material IN FRONT of `plain`'s
/// consonantal frame — the reduction-proof shape of "something was
/// prepended to this word" (The Wearing, Task 11c).
///
/// # Why the frame, and not the word
///
/// The natural test is `committed.ends_with(plain)`, and that is what this
/// metric ran until Task 9. Task 9's position-conditioned reduction voided
/// it: `glossed_name` reduces the compound under
/// `Prominence::None` when an honorific is prefixed (the affix takes the
/// word's stress) and under `Prominence::InitialVowel` when it is not, so
/// the honorific-free re-derivation is no longer a literal tail of the
/// honorific-bearing form. Seed 0, belief 1: committed `Teeloflof` against
/// an off-re-derivation of `loeflof` — the same stem, reduced `lo-` in one
/// and `loe-` in the other.
///
/// The projection that survives is the **consonant skeleton**.
/// `reduce_nuclei` only ever shortens a vowel run and never touches a
/// consonant, and it keeps at least `min(ph.nuclei)` vowels of every run,
/// so the consonant sequence of a word is invariant under the reduction
/// that broke the old test. Comparing skeletons puts the two forms back on
/// speaking terms without asking the generator anything.
///
/// # Why `contains`, and not `ends_with`, even on the skeleton
///
/// Measured, not assumed: over 862 committed epithets across seeds 0-39
/// and four peoples, a `ends_with` test on the skeleton read true on 861
/// and false on one — seed 26, bugbear, belief 1, committed
/// `Sxaoxddoapzhdapddoo` against an off-re-derivation of `Ddoapzhdap`. The
/// committed form is `Sxaox` + `ddoapzhdap` + `ddoo`: the plain word is
/// there whole, with the affix in front of it AND extra material behind it.
///
/// That trailing divergence was chased to its cause rather than assumed.
/// Both derivations of that belief return the gloss `"shadow-moon"`, so
/// both chose the same two morphemes; only the honorific-free surface
/// dropped one of them. `worn_compound` repairs the compound AFTER reducing
/// it, and repair may annihilate a morpheme, in which case the ladder gives
/// that morpheme's wear back and retries — so two differently-reduced forms
/// of the same compound can land on different rungs and surface different
/// numbers of morphemes. Reduction is invariant on consonants; the repair
/// that runs downstream of it is not. The claim this function can honestly
/// make is therefore about the FRONT of the word, which is the only place
/// an honorific can be, and it makes no claim about the tail.
///
/// # The narrowing Task 11c did not go far enough on (The Wearing, 11d)
///
/// Task 11c's sweep of seeds 0-199 saw no goblin world read false, and
/// concluded the front was safe. The 1000-world census disagreed: two
/// worlds, seeds **386** and **976**, read `false` for
/// `epithet-honorific-goblin`. Both were chased, and both are THIS
/// function's blind spot rather than a missing affix — the very same
/// repair-ladder divergence described above, landing at the FRONT of the
/// word instead of the tail, which is the one place the narrowed claim
/// still asserted something.
///
/// The two witnesses, re-derived rather than quoted (both are the world's
/// only belief whose gloss is a two-morpheme compound, `"gloom-day"`):
///
/// | seed | committed | honorific-free reference |
/// |---|---|---|
/// | 386 | `Zfaawmoffof` | `Foafmoffof` |
/// | 976 | `Vabozhbzas`  | `Boozhbozhbzas` |
///
/// In both, the honorific-free form surfaced the `gloom` morpheme and the
/// honorific-bearing form did not. That identification is not a guess: at
/// seed 386 the honorific-free surface of `gloom` standing alone is `Foaf`
/// (beliefs 4 and 6 of that same world), and at seed 976 it is `Boozh`
/// (beliefs 12-15). Strip it and the alignment is there —
/// `prepended_material("Zfaawmoffof", "moffof", …)` yields `zfaaw` and
/// `prepended_material("Vabozhbzas", "bozhbzas", …)` yields `va` — and
/// `va` is itself an affix this function detects on belief 10 of seed 976.
/// The reference word contains material the committed word genuinely does
/// not, so no offset aligns and `None` is the only answer available.
///
/// So the honest statement is stronger than "no claim about the tail": the
/// honorific-free re-derivation is an unreliable reference at EITHER end
/// whenever a belief's gloss is a multi-morpheme compound, because the
/// wear/repair ladder that runs downstream of reduction is not
/// reduction-invariant and may surface a different number of morphemes in
/// the two forms.
///
/// **The error is one-directional, which is what keeps the metric usable.**
/// A missing morpheme in the reference can only make an alignment fail, so
/// this function under-detects and never over-detects. `Some(_)` therefore
/// remains a strong positive — an affix was structurally located — and it
/// is `None` that became ambiguous between "no affix was prepended" and
/// "the reference diverged at the front". Task 11c's mutation evidence
/// bears exactly on the direction that still matters: with the prepend
/// deleted from BOTH of `glossed_name`'s paths, all fifteen probed beliefs
/// read `None`, zero false positives. A genuinely broken honorific
/// pipeline would turn hundreds of census worlds false, not two.
///
/// The follow-up that would close the blind spot is unchanged from 11c's:
/// let a caller re-derive the plain word under `Prominence::None` so both
/// forms take the same rung of the ladder. That widens
/// `hornvale_language`'s public surface for a lab metric's benefit, so it
/// stays an owner's design call rather than a repair — and it would move
/// two census values, so it owes a regen.
///
/// Returns the prepended surface material for the earliest reading that
/// places at least one consonant before the frame, or `None` if the frame
/// only ever sits at the very start (which is exactly the honorific-free
/// case: a species that commits its plain word commits `plain` itself, so
/// the skeletons are equal, the only alignment is at offset zero, and this
/// returns `None`). Every candidate offset is tried rather than only the
/// first, so a coincidental early alignment cannot mask the real one.
fn prepended_material(
    committed: &str,
    plain: &str,
    vowels: &std::collections::BTreeSet<char>,
) -> Option<String> {
    let c: Vec<char> = bare_surface(committed).chars().collect();
    let frame: Vec<char> = bare_surface(plain)
        .chars()
        .filter(|ch| !vowels.contains(ch))
        .collect();
    // A frameless plain word gives nothing to align against. Unreachable
    // for a drawn phonology (`draw_manner_slots` gives every onset template
    // at least one slot, so every drawn syllable has an onset consonant),
    // but a hand-built phonology in a test could reach it and a silent
    // `true` there would be the worst possible answer.
    if frame.is_empty() {
        return None;
    }
    // Surface positions of the committed form's own consonants, so an
    // alignment found in skeleton space can be read back as a surface cut.
    let cons: Vec<usize> = (0..c.len()).filter(|&i| !vowels.contains(&c[i])).collect();
    if cons.len() < frame.len() {
        return None;
    }
    for k in 1..=(cons.len() - frame.len()) {
        if (0..frame.len()).all(|j| c[cons[k + j]] == frame[j]) {
            let cut = cons[k];
            let prefix: String = c[..cut].iter().collect();
            // A whole syllable, not a stray consonant: the honorific is a
            // drawn template syllable, so the material in front of the
            // frame must carry a nucleus of its own.
            if prefix.chars().any(|ch| vowels.contains(&ch)) {
                return Some(prefix);
            }
        }
    }
    None
}

/// Whether every committed deity epithet of `species`' flagship pantheon
/// carries a prepended honorific affix, DETECTED from the committed epithet
/// content (not read back from the config that drove generation — that would
/// be tautological). `Absent` if `species` holds no pantheon (or, for a
/// non-default roster, if the species' lexicon cannot be re-derived — the
/// epithet-honorific columns are only registered for the shipped species).
///
/// Since The Words (Task 9) an epithet is glossed (`Namer::glossed_name`,
/// the `/v2` epoch): its draw depends on the belief's own site concepts
/// (its phenomenon's concept + its sentiment's), so detection re-derives
/// those exactly as worldgen composed them — the flagship's observed
/// phenomena in salience order pair 1:1 with its beliefs in commit order
/// (religion's genesis names members in phenomena order), and
/// `hornvale_worldgen::deity_site_concepts` maps each pair. Since The
/// Self-Describing Sky's naming epoch (`religion/deity/v2`), the name seed
/// itself is no longer the belief id — it is
/// `hornvale_worldgen::deity_name_seed_for(world_seed, species,
/// phenomenon.kind, rank)`, re-derived here with the same species/kind/rank
/// the committed epithet was generated with, never the belief id.
///
/// The honorific affix is one template syllable drawn AFTER the site-concept
/// picks and PREPENDED, so the honorific-free re-derivation of the same
/// belief is the plain word the committed epithet was built from — and the
/// committed epithet carries the honorific iff it holds that plain word's
/// consonantal frame with a whole syllable in front of it
/// ([`prepended_material`], which carries the argument for why the frame
/// and not the word itself, and the measurement behind it). `Flag(true)`
/// iff EVERY committed epithet carries it (goblin, Rank), `Flag(false)` iff
/// any does not — which for a non-`Rank` people means every one of them
/// (kobold, Knowledge).
///
/// # `false` is the CORRECT reading for a non-`Rank` people
///
/// Worth stating flatly, because a `Flag` whose right answer is `false` on
/// half the roster is a trap for the next reader (The Wearing, Task 11d).
/// [`hornvale_worldgen::morph_options`] switches honorifics on exactly for
/// `StatusBasis::Rank`. Kobold is the roster's only `Knowledge` people, so
/// its `MorphOptions` already carry `honorifics: false`, its epithets are
/// committed as plain glossed words, and the honorific-free re-derivation
/// this metric measures against is byte-identical to the committed form —
/// the skeletons are equal, the only alignment sits at offset zero, and
/// `prepended_material` returns `None` **by construction**. Over the
/// 1000-world census `epithet-honorific-kobold` reads 762 false and 238
/// absent, with not one `true`: the flag is reporting kobold morphology
/// faithfully, not failing to see something.
///
/// That this is a property of kobold and not a dead detector is settled by
/// the goblin column, which runs the identical code path and reads `true`
/// on 764 of the same 1000 worlds — the detector demonstrably can and does
/// report `true`. It was confirmed positively as well: across seeds 386,
/// 976, 42, 7 and 13, all 42 kobold beliefs commit an epithet equal to
/// their own honorific-free re-derivation, character for character.
///
/// **This is still a detection, not a reading.** Nothing here asks the
/// generator whether it prefixed an affix, and nothing compares the
/// committed epithet against an honorific-ON re-derivation of itself; the
/// only re-derivation is the honorific-FREE one, used as the reference
/// word the committed content is measured against. A broken honorific
/// pipeline — a goblin epithet committed without its affix — would BE its
/// plain word here, the frame would align only at offset zero, and the flag
/// would go false. That was measured, not argued: with the affix prepend
/// deleted from BOTH of `glossed_name`'s paths (the compound path and the
/// no-word-in-the-lexicon `build_name` fallback), all ten of seed 42's
/// hobgoblin epithets and all five of seed 26's bugbear epithets read "no
/// affix" — no false positive on either, including the one belief whose
/// tail diverges. The in-repo half of that mutation is
/// `prepended_material_detects_the_affix_and_reports_none_without_it`; the
/// live half is kobold, which commits plain glossed words on every census
/// row and reads false there.
fn epithet_honorific(v: &FullView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(v.world(), species) else {
        return MetricValue::Absent;
    };
    let beliefs = hornvale_religion::beliefs_held_by(v.world(), info.id);
    if beliefs.is_empty() {
        return MetricValue::Absent;
    }
    // Religion (and the deity glosses drawn inside it) observes from the
    // world's first place, hemisphere-culled (SEQ-4/SEQ-5) — re-derive from
    // exactly that vantage so the check tracks the pipeline's real sky.
    let Ok(seen) = observed_phenomena_as_in_from(v.world(), v.components(), species, v.climate())
    else {
        return MetricValue::Absent;
    };
    let Ok(lexicon) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let ph = language_of_in(v.world(), v.components(), species);
    // The language's own vowel letters, so the consonantal frame
    // `prepended_material` aligns on tracks `romanize` rather than a
    // hardcoded `aeiou`.
    let vowels = vowel_graphemes(&ph);
    let namer = Namer::new(&v.world().seed, species, &ph);
    // Resolved through the composition root's own mapping — never a
    // parallel definition of "this people's naming morphology".
    let (Some((_, mind)), Some((_, society))) = (
        v.components().psyche.iter().find(|(k, _)| k.0 == species),
        v.components().society.iter().find(|(k, _)| k.0 == species),
    ) else {
        return MetricValue::Absent;
    };
    let species_morph = hornvale_worldgen::morph_options(mind, society);
    let carries = |(i, b): (usize, &hornvale_religion::Belief)| {
        let Some(phenomenon) = seen.get(i) else {
            // More beliefs than observed phenomena would mean the
            // belief↔phenomenon pairing above is broken; count it as a
            // failed detection rather than guessing.
            return false;
        };
        let concepts = hornvale_worldgen::deity_site_concepts(phenomenon, b.sentiment);
        let site = hornvale_language::SiteConcepts {
            concepts: &concepts,
        };
        let name_seed =
            hornvale_worldgen::deity_name_seed_for(&v.world().seed, species, &phenomenon.kind, i);
        // The empty corpus, matching what worldgen passes for epithets:
        // toponymic wear is settlement-only, so re-deriving the plain word
        // here must wear nothing either.
        // The species' real naming morphology with the honorific SWITCHED
        // OFF: the honorific affix is drawn after the concepts are picked,
        // so clearing that one flag yields the plain word for this same
        // belief — the reference the committed epithet is measured against.
        // It is NOT the committed epithet minus its prefix, and the
        // campaign's own defect was a comment that said it was: since Task
        // 9 the plain word also reduces differently, because the affix
        // takes the word's stress (see `prepended_material`). Everything
        // else — in particular the shape weights, which decide how many
        // concepts get compounded — must match what worldgen passed, or
        // this re-derivation names a different deity and the flag flips
        // for a reason that has nothing to do with honorifics.
        let morph = hornvale_language::MorphOptions {
            honorifics: false,
            ..species_morph
        };
        let (plain, _) = namer.glossed_name(
            NameKind::Epithet,
            name_seed,
            &morph,
            &site,
            &lexicon,
            &hornvale_language::NameCorpus::none(),
        );
        prepended_material(&b.epithet, &plain.roman, &vowels).is_some()
    };
    MetricValue::Flag(beliefs.iter().enumerate().all(carries))
}

/// Mean character length of every generated name attributed to `species` in
/// this world; `Absent` if it produced no names.
fn mean_name_length(v: &FullView, species: &str) -> MetricValue {
    let names = species_generated_names(v, species);
    if names.is_empty() {
        return MetricValue::Absent;
    }
    let total: usize = names.iter().map(|n| n.chars().count()).sum();
    MetricValue::Number(total as f64 / names.len() as f64)
}

/// A committed surface string reduced to its comparable form: case-folded,
/// with the combining tone diacritics [`hornvale_language::tone_mark_roman`]
/// appends to a toned vowel (U+0300 grave, U+0301 acute, U+0304 macron)
/// dropped. Tone is a mark ON a nucleus, never a nucleus of its own, so it
/// must not break a vowel run — and it must not defeat a substring test
/// either, since a lexicon's citation form and a name that carries it are
/// rendered by the same `render_views` and so agree on tone by construction.
/// The filter takes the whole combining-diacritical block (U+0300–U+036F)
/// rather than the three marks in use, so a fourth tone level added later
/// cannot silently change either reading.
fn bare_surface(name: &str) -> String {
    name.to_lowercase()
        .chars()
        .filter(|c| !('\u{0300}'..='\u{036f}').contains(c))
        .collect()
}

/// The romanized vowel graphemes of `ph`'s own inventory — every letter that
/// can stand as (part of) a nucleus in a word of this language. Derived from
/// the inventory rather than hard-coded `aeiou` so the reading tracks
/// [`hornvale_language::romanize`] instead of duplicating it; every one is a
/// single `char` today, and `flat_map` over `chars()` keeps a future
/// multi-char vowel romanization from silently dropping out of the set.
///
/// Completeness argument (why a vowel letter can never fall outside this
/// set): `phonotactic-validity-<species>` — an invariant, green on every
/// census row — re-parses every committed name of this species against this
/// same `ph.inventory`, so a name containing a vowel letter the inventory
/// does not romanize could not have parsed.
fn vowel_graphemes(ph: &Phonology) -> std::collections::BTreeSet<char> {
    ph.inventory
        .iter()
        .filter(|s| matches!(s, Segment::Vowel { .. }))
        .flat_map(|s| romanize(s).chars())
        .collect()
}

/// Syllables in `name`, counted as **maximal runs of `vowels`** in its
/// case-folded, tone-mark-stripped surface (see [`bare_surface`]) — the
/// orthographic syllable proxy The Wearing (2026-07-27) measured its whole
/// before/after on, kept identical here so the metric and the campaign's
/// recorded figures are the same instrument.
///
/// It is a proxy, not the namer's own count, and the direction of its error
/// is known and measured. A run is read as ONE nucleus. Where the phonology
/// licenses a two-vowel nucleus (`ph.nuclei` containing 2) that is exact.
/// Where a two-vowel run appears in a language whose only licensed nucleus
/// is a simple vowel, a strict phonological parse would read two — evolved
/// roots guarantee inventory membership, not template conformance, and The
/// Speakable's attested tier admits a native word verbatim rather than
/// repairing it, so such runs do occur. Measured over the four seeds the
/// campaign reports (42, 1, 99, 777; 650 settlement names): 478 of 2,130
/// runs are two vowels long, and the ones in a nuclei-`[1]` language are 17
/// of 50 goblin runs and 24 of 75 hobgoblin runs — reading every such run as
/// two nuclei instead of one moves the four-seed mean from 3.277 to 3.340
/// (+1.9%), and seed 42's from 2.953 to 3.195 (+8.2%). **So this count is a
/// lower bound, loose by at most those few percent.**
///
/// Two abutting onsetless syllables would merge the same way, but that is a
/// third-order concern at most: `draw_manner_slots` draws a minimum of one
/// onset slot, so a drawn syllable is never onsetless (Task 9 measured 0 in
/// 245,613 syllables over 4,096 seeds).
fn syllable_count(name: &str, vowels: &std::collections::BTreeSet<char>) -> usize {
    let mut runs = 0usize;
    let mut in_run = false;
    for c in bare_surface(name).chars() {
        if vowels.contains(&c) {
            if !in_run {
                runs += 1;
            }
            in_run = true;
        } else {
            in_run = false;
        }
    }
    runs
}

/// Mean syllable count ([`syllable_count`]) of every generated name
/// attributed to `species` in this world — the same name population
/// [`mean_name_length`] reads, so the two columns are directly comparable;
/// `Absent` if it produced no names.
///
/// Why this exists at all (The Wearing, spec §7): character length cannot
/// tell "shorter words" from "the same words spelled tighter," and the
/// campaign's §2.2 diagnosis established that spelling was never the defect
/// — 3.4 characters per syllable is unremarkable (Bristol 3.5, Winchester
/// 3.3). Syllable count is the reading that measures the claim. The reading
/// is taken from the COMMITTED surface string, never from the namer's
/// internal syllable structures, so it is a measurement of what shipped
/// rather than an echo of how it was built.
fn mean_name_syllables(v: &FullView, species: &str) -> MetricValue {
    // A no-op today, kept because the reason it is a no-op is an argument
    // rather than a check, and `language_of_in` PANICS on a species outside
    // `v.components()`. The argument: every name `species_generated_names`
    // returns comes from a settlement or flagship this world committed, and a
    // world can only commit those for kinds in the roster it was built from —
    // so a non-empty name list already implies membership, and the empty list
    // returns above. `name_transparency`'s roster guard is the same hazard
    // met from the other side, where the argument does NOT hold because the
    // species comes from the ledger rather than from this call's own literal.
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let names = species_generated_names(v, species);
    if names.is_empty() {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    let vowels = vowel_graphemes(&ph);
    let total: usize = names.iter().map(|n| syllable_count(n, &vowels)).sum();
    MetricValue::Number(total as f64 / names.len() as f64)
}

/// Fraction of this world's settlement + deity names (across every species)
/// that duplicate another name in the same world; `Absent` if there are
/// none. Uniqueness is de-facto, not enforced (Task 9) — this is a measured
/// property of the name space, not an assertion of zero collisions.
fn name_collision_rate(v: &FullView) -> MetricValue {
    let mut names: Vec<String> = hornvale_terrain::places(v.world())
        .into_iter()
        .map(|p| p.name)
        .collect();
    names.extend(beliefs_of(v.world()).into_iter().map(|b| b.deity));
    if names.is_empty() {
        return MetricValue::Absent;
    }
    let mut counts: std::collections::BTreeMap<&str, usize> = std::collections::BTreeMap::new();
    for n in &names {
        *counts.entry(n.as_str()).or_insert(0) += 1;
    }
    let duplicated = names.iter().filter(|n| counts[n.as_str()] > 1).count();
    MetricValue::Number(duplicated as f64 / names.len() as f64)
}

/// The concept a phenomenon glosses to, read from the shared roster
/// (`hornvale_worldgen::GLOSSING_KINDS`) and the phenomenon's own referent.
///
/// This is a READ, not a derivation — the derivation this crate owns is
/// [`referent_is_nameable`] below, which answers the same roster from the
/// concept registry and the lexicon rather than from worldgen's codomain.
/// Decision 0094: share the roster, never the derivation. Before The
/// Vernacular this function re-implemented worldgen's mapping by grepping the
/// phenomenon's English description, which made the gloss a function of
/// prose.
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&str> {
    hornvale_worldgen::GLOSSING_KINDS
        .contains(&phenomenon.kind.as_str())
        .then_some(phenomenon.referent.concept.as_str())
}

/// This crate's own derivation over the shared roster: is a rostered
/// phenomenon's referent a concept the world can actually *say*?
///
/// Independent of worldgen by construction — it consults the concept
/// registry and the culture's lexicon, which the gloss path never reads. A
/// referent that is unregistered, outside the presiding codomain, or a
/// lexical `Gap` for this culture is a phenomenon whose deity could never be
/// named after it, which is exactly the defect The Vernacular exists to make
/// visible. Reserved integration seam: exercised today only by
/// `every_rostered_referent_is_nameable` below; a metric wiring it into the
/// registry is a follow-up outside this task's scope. Present in all builds
/// so that seam is real, not test-only.
#[allow(dead_code)]
fn referent_is_nameable(
    phenomenon: &Phenomenon,
    registry: &hornvale_kernel::ConceptRegistry,
    lexicon: &hornvale_language::Lexicon,
) -> Option<bool> {
    let concept = phenomenon_concept(phenomenon)?;
    Some(
        registry.concept(concept).is_some()
            && PRESIDING_CONCEPTS.contains(&concept)
            && !matches!(lexicon.entry(concept), None | Some(LexEntry::Gap { .. })),
    )
}

/// This world's `species` lexicon, reusing the view's already-built terrain
/// and climate instead of re-sculpting the globe inside `exposure_from` — the
/// census's dominant cost once the name-gloss sculpts were removed (the
/// terrain pipeline ran twice per `lexicon_from` call, ~14 metrics deep). The
/// Single Sculpt, applied to the lexicon path; byte-identical to
/// `lex(v, species)`.
fn lex(v: &FullView, species: &str) -> Result<hornvale_language::Lexicon, BuildError> {
    hornvale_worldgen::lexicon_from(v.world(), species, v.terrain(), v.climate())
}

/// A settlement's own re-derived site concepts: calls worldgen's own
/// [`worldgen_settlement_site_concepts`] — the SAME composition the naming
/// pass itself used (Task 5, F2) — over this settlement's committed cell,
/// this view's terrain/climate, and the presiding phenomenon its species
/// observes from THIS settlement's own vantage (its committed coordinates
/// cull the sky — SEQ-5; spec §9.3 defines gloss truthfulness against the
/// entity's own facts). A real cross-check, not an echo: it never calls
/// worldgen's internal name-drawing code, only its committed `CELL_ID`/
/// `NAME_GLOSS` facts and this SAME public site-concept function every
/// other name-truthfulness consumer (the worldgen keystone test, this
/// metric) also calls, so all three stay in lockstep by construction
/// rather than by three hand-kept copies — true for every slot
/// `worldgen_settlement_site_concepts` computes itself. The presiding slot
/// is the one exception: this function cannot pass it through that
/// call (see the comment on `presiding` below), so it is appended here
/// instead, in a position that has to be hand-kept in sync with where
/// worldgen appends it internally. `None` if the settlement is
/// missing a cell-id/species fact, which `name_gloss_true` below treats as
/// an unverifiable (failing) row rather than skipping it silently.
fn settlement_site_concepts(
    v: &FullView,
    id: EntityId,
    climate: &GeneratedClimate,
) -> Option<Vec<String>> {
    let Value::Number(cell_id) = v
        .world()
        .ledger
        .value_of(id, hornvale_settlement::CELL_ID)?
    else {
        return None;
    };
    let cell = CellId(*cell_id as u32);
    let species = hornvale_species::species_of(v.world(), id)?;
    let phenomena =
        observed_phenomena_as_at_from(v.world(), v.components(), &species, id, climate).ok()?;
    // `phenomenon_concept` now borrows from the phenomenon's own referent
    // (decision 0094 stopped this being a `&'static` codomain match), so it
    // cannot feed `worldgen_settlement_site_concepts`'s `presiding:
    // Option<&'static str>` parameter directly — `phenomena` doesn't outlive
    // that call. Own the string instead and pass `None` for `presiding`,
    // appending it here after the fact.
    //
    // This reproduces the exact vector `worldgen_settlement_site_concepts`
    // would have returned, not merely an order-insensitive equivalent of it:
    // that function appends `presiding` LAST
    // (`windows/worldgen/src/lib.rs:4902`, `concepts.extend(presiding)` as
    // its final line before returning), and appending it last here matches
    // that exactly. This is now a real assumption this crate hand-keeps
    // about worldgen's push order, not something decision 0094 lets us
    // avoid — if that push ever moves,
    // `settlement_site_concepts_orders_a_real_multi_concept_vector_most_
    // specific_first` (`windows/worldgen/src/lib.rs:10471`) reds and gets
    // updated on that side, and this `.extend(presiding)` must move with it
    // or this crate silently drifts out of the composition it claims to
    // reproduce.
    let presiding = phenomena
        .first()
        .and_then(phenomenon_concept)
        .map(str::to_string);
    let mut concepts: Vec<String> = worldgen_settlement_site_concepts(
        v.world(),
        &v.world().seed,
        &species,
        cell,
        v.terrain(),
        climate,
        None,
    )
    .into_iter()
    .map(str::to_string)
    .collect();
    concepts.extend(presiding);
    Some(concepts)
}

/// Whether `gloss` reads as a truthful composition of `concepts`: it must
/// segment (uniquely) into a `"-"`-joined sequence of distinct members of
/// `concepts`, which is exactly what `Namer::glossed_name` writes — its
/// `chosen.join("-")` over concepts it picked, without repetition, from the
/// site vector it was handed.
///
/// **This replaced an enumerated accept set** (each concept alone plus every
/// ordered pair) at The Wearing's close, for two reasons, the first of which
/// was a live defect:
///
/// 1. **The pair ceiling was wrong after Task 7.** `NameShape::Qualified`
///    composes THREE concepts, and a three-concept gloss is not a single or
///    a pair, so it matched nothing and `name-gloss-true` read `false` for
///    any world containing one. It read false at every one of the four seeds
///    the campaign measures (36 of 650 settlement glosses are three-part),
///    while the gloss was in fact perfectly truthful — the metric had gone
///    stale against the shapes the namer can now produce. The stale census
///    fixture hid it: `name_gloss_true_is_100_percent_row_by_row` reads
///    pre-campaign rows, so it stayed green while the live metric was false.
///    A parse has no arity ceiling, so it cannot go stale that way again.
/// 2. **The accept set was loosening as the site vector widened.** Task 5
///    took the vector from 2 concepts (4 candidates) to up to 11 — 11
///    singles plus 110 ordered pairs, 121 candidates — roughly 30x more
///    strings a wrong gloss could coincidentally equal, and admitting
///    three-part compositions would have taken it past 1,100. Segmenting
///    instead of enumerating checks the same property without ever
///    building that set.
///
/// The distinctness requirement is not decoration: it is the one thing the
/// old ordered-pair enumeration (`i != j`) still carried that a bare
/// segmentation would drop, and dropping it would let `coast-coast` pass.
fn gloss_is_a_composition_of(gloss: &str, concepts: &[String]) -> bool {
    let vocab: std::collections::BTreeSet<&str> = concepts.iter().map(String::as_str).collect();
    let parses = gloss_parses(gloss, &vocab);
    if parses.len() != 1 {
        return false;
    }
    let parts = &parses[0];
    let distinct: std::collections::BTreeSet<&str> = parts.iter().copied().collect();
    distinct.len() == parts.len()
}

/// Whether every committed settlement `name-gloss` fact in this world is a
/// truthful composition of that SAME settlement's own re-derived site
/// concepts (spec §9.3). Still a real cross-check, not a tautology: it
/// never calls worldgen's internal name-drawing code (`glossed_name` or
/// anything downstream of it), only the committed `CELL_ID`/`NAME_GLOSS`
/// facts plus this view's own re-derived terrain/climate, so it still
/// catches a gloss committed against the wrong cell or a `NAME_GLOSS`
/// written by a broken pipeline.
///
/// Since Task 5 this is NOT independent of worldgen's own composition,
/// though — `settlement_site_concepts` above calls
/// `hornvale_worldgen::settlement_site_concepts` directly (F2), which two
/// things genuinely weakens rather than only appearing to:
/// - it no longer reads the committed `BIOME` fact, so nothing in this
///   metric cross-checks `BIOME` against the settlement's own re-derived
///   `biome_at(CELL_ID)` any more (that check now lives only in worldgen's
///   own keystone test,
///   `a_settlement_name_gloss_is_truthful_to_its_own_site_facts`, which
///   restored it after the same widening dropped it there too).
/// - the accept set was growing roughly 30x with the site vector, so a wrong
///   gloss was far likelier to slip through by coincidence than before Task
///   5. The Wearing closed that half: [`gloss_is_a_composition_of`] segments
///   the gloss instead of enumerating an accept set, which is tighter at
///   every arity and does not loosen as the vector widens. (It also fixed a
///   live staleness the enumeration had — see that function's own comment.)
///
/// `Absent` if no settlement in this world carries a gloss.
fn name_gloss_true(v: &FullView) -> MetricValue {
    let mut checked = false;
    let mut all_true = true;
    // Build the climate ONCE (the view already holds it) and thread it through
    // every settlement's observation, rather than re-sculpting terrain per
    // settlement (The Single Sculpt, applied to the Lab metric path).
    let climate = v.climate();
    for f in v.world().ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(gloss) = v.world().ledger.text_of(id, hornvale_kernel::NAME_GLOSS) else {
            continue;
        };
        checked = true;
        match settlement_site_concepts(v, id, climate) {
            Some(concepts) if gloss_is_a_composition_of(gloss, &concepts) => {}
            _ => all_true = false,
        }
    }
    if !checked {
        return MetricValue::Absent;
    }
    MetricValue::Flag(all_true)
}

/// A SUPERSET of [`phenomenon_concept`]'s codomain — every concept a
/// presiding phenomenon can contribute to a settlement's site vector, and
/// therefore to its gloss. Not exact: since decision 0094 opened
/// `phenomenon_concept` to read a phenomenon's own `referent.concept`
/// verbatim for any rostered kind, its codomain is open (any registered
/// concept a producer chooses to name), so this list is an upper bound rather
/// than a codomain pinned exactly to it. Named as a set here because
/// [`name_transparency`] has to READ a committed gloss back into the concepts
/// it names, and the presiding slot is the one site concept that cannot be
/// re-derived from terrain and climate alone (it needs the settlement's own
/// culled sky — `SEQ-5`, the expensive half of `settlement_site_concepts`
/// above). Taking the whole list as the candidate set instead costs the parse
/// nothing: it is a superset of the one concept that actually fired, and the
/// segmentation stays unique anyway (see [`gloss_parses`]).
/// `presiding_concepts_cover_seed_42s_rostered_concepts` checks this list
/// against a real generated world's rostered concepts, rather than pinning
/// exact codomain equality against hand-written fixtures.
/// type-audit: bare-ok(identifier-text)
const PRESIDING_CONCEPTS: &[&str] = &["day", "moon", "star", "sun", "wind"];

/// Every way `gloss` reads as a `"-"`-joined sequence of `vocab` members —
/// the inverse of `glossed_name`'s own `chosen.join("-")`.
///
/// A gloss cannot simply be `split('-')`: a biome concept id is itself
/// hyphenated (`tropical-seasonal-forest`, `sea-ice`), so `coast-sea-ice`
/// must read as two concepts and not four. This returns EVERY segmentation
/// so the caller can insist on a unique one rather than silently preferring
/// a longest-match; over the four seeds The Wearing measured (650 glossed
/// settlement names) the parse was unique for every single one, and none
/// failed to parse.
fn gloss_parses<'a>(gloss: &str, vocab: &std::collections::BTreeSet<&'a str>) -> Vec<Vec<&'a str>> {
    let mut out: Vec<Vec<&'a str>> = Vec::new();
    for word in vocab {
        if gloss == *word {
            out.push(vec![word]);
        } else if let Some(rest) = gloss
            .strip_prefix(*word)
            .and_then(|rest| rest.strip_prefix('-'))
        {
            for tail in gloss_parses(rest, vocab) {
                let mut parse = vec![*word];
                parse.extend(tail);
                out.push(parse);
            }
        }
    }
    out
}

/// The share of this world's committed settlement names whose SURFACE still
/// contains, verbatim, the modern citation form of every concept its own
/// committed `name-gloss` names; `Absent` if no settlement carries a
/// non-empty gloss.
///
/// **The target is explicitly not 1.0** (The Wearing, spec §8). Before this
/// campaign transparency was 100% by construction — a name was its site
/// words, unworn, plus a drawn stem — and that uniformity is the defect the
/// campaign names: most real toponyms are opaque to their own speakers, and
/// no English speaker hears *hām* in "Birmingham". A metric asserting 1.0
/// would be asserting the defect back. What is wanted is a DISTRIBUTION, so
/// this is registered `Numeric` and pinned as a drift witness, never bounded.
/// Measured at the four seeds the campaign reports: 100% before (650 of 650),
/// 68.8% after (447 of 650).
///
/// **Route, and why it is not an echo.** Two committed facts and one
/// re-derivation, none of them the naming code:
/// - the committed `name` and `name-gloss` of each settlement, read from the
///   ledger;
/// - that settlement's own site vector, from worldgen's own
///   `settlement_site_concepts` — used only as the VOCABULARY the gloss is
///   segmented against (with `presiding: None`, so the expensive per-
///   settlement sky observation `name_gloss_true` pays is skipped and
///   [`PRESIDING_CONCEPTS`] stands in for that slot);
/// - the species' lexicon, for each named concept's **citation form** — the
///   dictionary word, exactly as unworn and unreduced as the lexicon minted
///   it.
///
/// Nothing here calls `glossed_name`, `wear`, `reduce_nuclei`,
/// `worn_compound` or `repair_phonotactics`. The reading falls precisely
/// when those change the surface away from the citation form, which is what
/// makes it a measurement of wear rather than a restatement of it: forced to
/// the pre-campaign tree it reads 1.0 at every seed, which is the strongest
/// statement available that it is not vacuous.
///
/// **Strictness.** A name counts as transparent only if EVERY concept in its
/// gloss is present verbatim — the conjunction, not "at least one". A
/// two-concept name whose second morpheme reduced is therefore opaque even
/// though its first morpheme still reads; 61 of the 650 measured names are
/// this partial case, and they count against transparency.
///
/// An unparseable or ambiguous gloss counts against transparency too, since
/// a gloss the metric cannot read into concepts cannot be shown to be
/// readable in the surface either. That case is not reachable through a
/// healthy pipeline: `name-gloss-true` (an invariant, pinned 100% row by row
/// in `calibration.rs`) already asserts every committed gloss is a
/// composition of that settlement's own site concepts, and this vocabulary
/// is a superset of that one on its only lossy axis.
///
/// **The denominator, and the one thing excluded from it.** A settlement
/// whose species the canonical roster does not know as a speaking kind is
/// excluded from BOTH the numerator and the denominator — it is not counted
/// as opaque, it is not counted at all.
///
/// This is the null control's synthetic twin. `census-of-the-meeting` builds
/// worlds under a `goblin-twin-solo` roster whose settlements are peopled by
/// `goblin-twin`, a comparison species that exists only to be a goblin with a
/// different name salt. It has no entry in the canonical roster that
/// `lexicon_from` reconstructs against, so there is no citation form to look
/// for — the question this metric asks cannot be put about it at all.
///
/// Exclusion, rather than counting it opaque or making the world `Absent`:
/// - counting it opaque would drag the reading down for a reason with nothing
///   to do with wear, which is exactly how a drift witness starts lying;
/// - `Absent` for the whole world would throw away the real peoples'
///   settlements in any world that also held a twin.
///
/// Exclusion needs no special case to get the null control right, either: in
/// `goblin-twin-solo` EVERY settlement is the twin, so the denominator is
/// zero and the world reads `Absent` — correctly, there is nothing measurable
/// — while `goblin-solo` is fully measured.
///
/// **And it cannot silently shift the census denominator**, which is the
/// property that keeps the drift witness honest: `the-census` builds under
/// the default roster, which IS the roster `lexicon_from` assembles, so every
/// species that can people a settlement there resolves by construction and
/// nothing is ever excluded. The rule can only fire under a synthetic roster.
///
/// The check is a precondition, deliberately, not a caught failure: both
/// `resolve_kind` (unknown species) and `language_of_wc` (a known kind with
/// no articulation row) panic rather than returning `Err` on the path
/// `lexicon_from` takes, so `lex(...).ok()` below cannot see either. This is
/// the first metric that asks for a lexicon for whatever species happens to
/// people a settlement rather than for a hardcoded `"goblin"`/`"kobold"`,
/// which is what made it the first to meet them.
fn name_transparency(v: &FullView) -> MetricValue {
    let world = v.world();
    // The roster `lexicon_from` reconstructs against — assembled once here so
    // an unresolvable species is skipped BEFORE the call that would panic on
    // it, rather than after. Both stores are checked: biosphere membership is
    // what `resolve_kind` needs, an articulation row is what `language_of_wc`
    // needs, and each panics separately.
    let Ok(canonical) = WorldComponents::assemble() else {
        return MetricValue::Absent;
    };
    let speaks = |species: &str| {
        canonical.biosphere.get_by_label(species).is_some()
            && canonical.articulation.get_by_label(species).is_some()
    };
    let mut glossed = 0usize;
    let mut transparent = 0usize;
    // One lexicon per species per world, not per settlement (the phenomena
    // seam lesson: `lexicon_from` is a whole-lexicon build). `None` records a
    // species whose lexicon will not build, so the failure is paid once.
    let mut lexicons: std::collections::BTreeMap<String, Option<hornvale_language::Lexicon>> =
        std::collections::BTreeMap::new();
    for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(gloss) = world.ledger.text_of(id, hornvale_kernel::NAME_GLOSS) else {
            continue;
        };
        if gloss.is_empty() {
            continue;
        }
        // Out of the denominator entirely, before it is counted — see the
        // denominator note above. A settlement MISSING its species fact is a
        // different case and stays in: that is a broken ledger, not a
        // synthetic species, and it counts against transparency below.
        let species = hornvale_species::species_of(world, id);
        if species.as_deref().is_some_and(|s| !speaks(s)) {
            continue;
        }
        glossed += 1;
        let (Some(name), Some(species), Some(Value::Number(cell))) = (
            world.ledger.text_of(id, hornvale_kernel::NAME),
            species,
            world.ledger.value_of(id, hornvale_settlement::CELL_ID),
        ) else {
            continue;
        };
        let lexicon = lexicons
            .entry(species.clone())
            .or_insert_with(|| lex(v, &species).ok());
        let Some(lexicon) = lexicon.as_ref() else {
            continue;
        };
        let mut vocab: std::collections::BTreeSet<&str> = worldgen_settlement_site_concepts(
            v.world(),
            &v.world().seed,
            &species,
            CellId(*cell as u32),
            v.terrain(),
            v.climate(),
            None,
        )
        .into_iter()
        .collect();
        vocab.extend(PRESIDING_CONCEPTS.iter().copied());
        let parses = gloss_parses(gloss, &vocab);
        if parses.len() != 1 {
            continue;
        }
        let surface = bare_surface(name);
        let reads = parses[0].iter().all(|concept| {
            match lexicon.entry(concept) {
                Some(LexEntry::Root { views, .. }) | Some(LexEntry::Compound { views, .. }) => {
                    let citation = bare_surface(&views.roman);
                    !citation.is_empty() && surface.contains(&citation)
                }
                // A `Gap` concept has no word to look for, and could not have
                // been chosen for a name in the first place.
                _ => false,
            }
        });
        if reads {
            transparent += 1;
        }
    }
    if glossed == 0 {
        return MetricValue::Absent;
    }
    MetricValue::Number(transparent as f64 / glossed as f64)
}

/// Whether every `species` lexicon `Root` entry's recorded sound-change
/// derivation replays byte-identically through `evolve` (Neogrammarian
/// regularity, spec §9.1) — the per-species aggregate of
/// `cli/tests/branches_coverage.rs`'s `derivations_replay`. `Absent` if
/// `species` is not in this world's roster or its lexicon minted no `Root`.
fn lexicon_regular(v: &FullView, species: &str) -> MetricValue {
    if !v.components().biosphere.ids().any(|k| k.0 == species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    // NOT routed through hornvale_worldgen::cascade_of (The Solitary Tongue,
    // Task 4): every call site passes a fixed people, never a dragon-
    // reachable roster scan — the two direct registrations below pass the
    // literal "goblin"/"kobold", and lexicon_regular_family's only other
    // caller iterates the fixed ALL_DAUGHTERS = ["goblin", "hobgoblin",
    // "bugbear", "kobold"] constant. The default SETTLED regime is
    // therefore always the correct one here.
    let cascade = hornvale_language::draw_cascade(&v.world().seed, species, &ph);
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let mut any = false;
    let mut regular = true;
    for (_, entry) in lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            any = true;
            let replayed = hornvale_language::evolve(&derivation.proto, &cascade, &ph);
            if replayed.modern != derivation.modern {
                regular = false;
            }
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(regular)
}

/// How many DISTINCT sound rules in `species`' drawn cascade actually fire
/// on at least one of its lexicon's `Root` entries.
///
/// The Namesake, Task 1. A `Cascade` is 2-4 drawn rules
/// (`hornvale_language::Cascade`), but `evolve` adopts a rule's proposed
/// output only when the resulting segment is already in the phonology's
/// inventory (the codomain constraint), so a rule can be drawn and then
/// rejected on every word. This metric asks how many survive that filter.
///
/// Zero means the species' whole etymological layer is inert: every word's
/// modern form equals its proto-form's nativization, and an inherited name
/// and a re-derived one are byte-identical. `Absent` if `species` is not in
/// this world's roster or its lexicon minted no `Root`.
fn cascade_rules_fired(v: &FullView, species: &str) -> MetricValue {
    if !v.components().biosphere.ids().any(|k| k.0 == species) {
        return MetricValue::Absent;
    }
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    // A BTreeSet, not a HashSet: the workspace bans hashed containers, and
    // the count must not depend on iteration order anyway. Keyed on
    // `RuleKind` (which the fix-round review established `hornvale_language`
    // already derives `Ord`/`Eq` for), not on step index: `draw_cascade`
    // samples each of its 2-4 slots independently, so the same `RuleKind`
    // can be drawn twice at different indices, and "distinct sound rules"
    // (this fn's own doc string, and `divergence_magnitude`'s established
    // reading of "distinct" elsewhere in this file) means dedup-by-value,
    // not dedup-by-position.
    let mut fired: std::collections::BTreeSet<hornvale_language::RuleKind> =
        std::collections::BTreeSet::new();
    let mut any_root = false;
    // `Lexicon::entries()` yields (&str, &LexEntry) pairs — it is an
    // iterator, not a map, so there is no `.values()`.
    for (_concept, entry) in lex.entries() {
        if let hornvale_language::LexEntry::Root { derivation, .. } = entry {
            any_root = true;
            for step in &derivation.steps {
                if step.changed {
                    fired.insert(step.rule.kind);
                }
            }
        }
    }
    if !any_root {
        return MetricValue::Absent;
    }
    MetricValue::Number(fired.len() as f64)
}

// --- The Namesake (Task 7): the two preregistered claims. ---
//
// §5.1 asks whether the `SocietyVector`-derived naming patterns actually
// differ between peoples; §5.2 asks whether the shortest-prefix render rule
// earns its keep. Four metrics below read the first pair, and a fifth reads
// §5.2(2)'s second half — see `name_prefix_region_full_stack` for why that
// one exists.
//
// WHAT A `Rendered` NAME CONTAINS HERE, AND WHAT IT DOES NOT. `name_pattern`
// (Task 5) emits five kinds of element across the shipped roster:
// `Stem`, `Relation(Parent)`, `Relation(Clan)`, `Relation(Mentor)` and
// `Deed`, plus `Gloss(Bearing)` for a wide-in-group people. This campaign
// shipped resolvers for exactly three of them — the person-name draw
// (`NameKind::Person`, Task 4) and the descent graph's `forebear_of` /
// `clan_root_of` (Task 3). There is no mentorship relation anywhere in the
// repo, no deed-name derivation, and no per-person gloss basis, so
// `Relation(Mentor)`, `Relation(Community)`, `Deed` and `Gloss(..)` have
// nothing to resolve to.
//
// An element with no resolver is **dropped**, not filled with a placeholder.
// A per-figure placeholder would fabricate disambiguating entropy the engine
// does not have (and would flatter §5.2); a constant placeholder would
// inflate the element counts §5.2 measures without adding any disambiguating
// power. Dropping states the honest position: a name carries only the
// elements this slice of the engine can actually speak. The consequence is
// measured and reported rather than hidden — a kobold (pattern
// `[Stem, Relation(Mentor)]`) speaks one element, and a gnoll (pattern
// `[Stem, Deed, Relation(Clan), Gloss(Bearing)]`) speaks two.

/// One occupation founder, resolved to the words their culture's naming
/// pattern actually produces.
struct FounderName {
    /// The `occ-site` cell the occupation sits on — the settlement scope.
    site: u32,
    /// The name, element by element, in the culture's own order.
    rendered: hornvale_language::anthroponym::Rendered,
}

/// Every occupation founder in the world, with their name resolved to words.
///
/// The Namesake §5.2. One entry per occupation carrying an `occ-people`, an
/// `occ-site`, and a people this world's component set knows — the whole
/// nameable population, since §2 of the spec restricts naming to the figures
/// a role implies.
///
/// Empty (never `Absent` on its own account) when the world has no
/// occupations at all; the callers turn that into `MetricValue::Absent`.
fn rendered_founders(v: &FullView) -> Vec<FounderName> {
    use hornvale_language::anthroponym::{Cite, ElementSource, Rendered};

    let world = v.world();
    let wc = v.components();

    // Per speaking, minded people: its phonology (the single construction
    // site is worldgen's `language_of_in`), its morphology, and its naming
    // pattern. Keyed on the `'static` `KindId` label so the `Namer`s below
    // can borrow the phonologies without cloning them per founder.
    let mut phonologies: std::collections::BTreeMap<&'static str, Phonology> =
        std::collections::BTreeMap::new();
    let mut kits: std::collections::BTreeMap<
        &'static str,
        (
            hornvale_language::MorphOptions,
            hornvale_language::anthroponym::NamePattern,
        ),
    > = std::collections::BTreeMap::new();
    for kid in wc.articulation.ids() {
        let (Some(mind), Some(society)) = (wc.psyche.get(kid), wc.society.get(kid)) else {
            continue;
        };
        phonologies.insert(kid.0, language_of_in(world, wc, kid.0));
        kits.insert(
            kid.0,
            (
                hornvale_worldgen::morph_options(mind, society),
                hornvale_worldgen::name_pattern(mind, society),
            ),
        );
    }
    let namers: std::collections::BTreeMap<&'static str, Namer> = phonologies
        .iter()
        .map(|(name, ph)| (*name, Namer::new(&world.seed, name, ph)))
        .collect();

    // A figure's given name: the `NameKind::Person` draw off the persona
    // seed their role handle expands to. This is the ONLY material any
    // element resolves from — a patronymic is the forebear's given name, a
    // clan name is the chain root's.
    let stem_of = |species: &'static str, handle: hornvale_history::flesh::RoleHandle| -> String {
        let persona = hornvale_history::flesh::persona_of(handle, world.seed);
        namers[species]
            .name(NameKind::Person, persona.name_seed, &kits[species].0)
            .roman
    };

    let mut out = Vec::new();
    for fact in world.ledger.find(hornvale_history::IS_OCCUPATION) {
        let occupation = fact.subject;
        let Some(Value::Text(people)) = world
            .ledger
            .value_of(occupation, hornvale_history::OCC_PEOPLE)
        else {
            continue;
        };
        let Some(species) = kits.keys().find(|k| **k == people.as_str()).copied() else {
            continue;
        };
        let Some(Value::Number(site)) = world
            .ledger
            .value_of(occupation, hornvale_history::OCC_SITE)
        else {
            continue;
        };

        let mut parts: Vec<String> = Vec::new();
        for (source, _author) in &kits[species].1.elements {
            match source {
                ElementSource::Stem => {
                    parts.push(stem_of(
                        species,
                        hornvale_worldgen::founder_of(world, occupation),
                    ));
                }
                ElementSource::Relation(Cite::Parent) => {
                    // A genesis founder has no forebear, so the patronymic
                    // slot stays empty rather than citing the figure
                    // themselves.
                    if let Some((forebear, _kinship)) =
                        hornvale_worldgen::forebear_of(world, occupation)
                    {
                        parts.push(stem_of(species, forebear));
                    }
                }
                ElementSource::Relation(Cite::Clan) => {
                    let root = hornvale_worldgen::clan_root_of(world, occupation);
                    parts.push(stem_of(species, hornvale_worldgen::founder_of(world, root)));
                }
                // No resolver in this slice — see the module comment above.
                ElementSource::Relation(_) | ElementSource::Index(_) | ElementSource::Deed => {}
                ElementSource::Gloss(_) => {}
            }
        }
        if parts.is_empty() {
            continue;
        }
        out.push(FounderName {
            site: *site as u32,
            rendered: Rendered { parts },
        });
    }
    out
}

/// How many elements the shipped [`hornvale_language::anthroponym::render`]
/// spent on `name` against `competitors`.
///
/// Asks the real render rule and then reads its answer back, rather than
/// re-deriving the prefix length here: a second copy of the loop would
/// measure the copy, not the rule. The spoken form is
/// `parts[..k].join(" ")` for exactly one smallest `k`, which is what this
/// recovers.
fn rendered_element_count(
    name: &hornvale_language::anthroponym::Rendered,
    competitors: &[hornvale_language::anthroponym::Rendered],
) -> usize {
    let spoken = hornvale_language::anthroponym::render(name, competitors);
    for take in 1..=name.parts.len() {
        if name.parts[..take].join(" ") == spoken {
            return take;
        }
    }
    name.parts.len()
}

/// Each founder's region-scope rendered element count, in ledger order.
///
/// The competitor scope is every OTHER founder in the world. The exclusion
/// is by position, not by value: `render` treats a competitor identical to
/// `name` as undistinguishable, so leaving the figure in their own
/// competitor set would drive every name to its full stack. Swapping the
/// figure to the end and passing the head of the vector excludes exactly one
/// entry without cloning the other n-1.
fn region_scope_counts(v: &FullView) -> Vec<usize> {
    let founders = rendered_founders(v);
    let mut names: Vec<hornvale_language::anthroponym::Rendered> =
        founders.into_iter().map(|f| f.rendered).collect();
    let n = names.len();
    if n == 0 {
        return Vec::new();
    }
    let mut counts = Vec::with_capacity(n);
    for i in 0..n {
        names.swap(i, n - 1);
        let subject = names[n - 1].clone();
        counts.push(rendered_element_count(&subject, &names[..n - 1]));
        names.swap(i, n - 1);
    }
    counts
}

/// How many DISTINCT naming-pattern signatures this world's placed peoples
/// produce (The Namesake, preregistered criterion §5.1(1); target >= 3).
///
/// A signature is the ordered list of `(ElementSource, Author)` pairs
/// `name_pattern` derives from a people's society vector. If every people
/// produced the same signature the naming system would be one shape with
/// cosmetic variation, which is the failure this metric exists to catch.
/// `Absent` if no placed people carries both psychology vectors.
fn name_pattern_signatures(v: &FullView) -> MetricValue {
    let sigs = placed_pattern_signatures(v);
    if sigs.is_empty() {
        return MetricValue::Absent;
    }
    let distinct: std::collections::BTreeSet<&String> = sigs.iter().collect();
    MetricValue::Number(distinct.len() as f64)
}

/// The naming-pattern signature of every placed people, one entry per
/// people, in registry order.
fn placed_pattern_signatures(v: &FullView) -> Vec<String> {
    let wc = v.components();
    let mut sigs = Vec::new();
    for kind in hornvale_worldgen::placed_peoples(v.world()) {
        let (Some(mind), Some(society)) = (
            wc.psyche.get(&hornvale_kernel::KindId(kind.0)),
            wc.society.get(&hornvale_kernel::KindId(kind.0)),
        ) else {
            continue;
        };
        let p = hornvale_worldgen::name_pattern(mind, society);
        sigs.push(format!("{:?}", p.elements));
    }
    sigs
}

/// How many peoples are PLACED in this world — the `n` in the chance
/// baseline `1/n` that The Namesake's preregistered criterion §5.1(2) is
/// judged against.
///
/// This metric exists so that verdict is **re-derivable from `rows.csv`
/// alone**. `name-people-recoverability` reports a share, `u/n`, and the
/// criterion is `share >= 2/n`; without `n` on the row a reader has to infer
/// it by inverting the (signature-count, share) pair against the roster's
/// signature classes. That inversion happens to be sound for the shipped
/// roster, but it is arithmetic done in prose over data the artifact does not
/// carry, and a preregistered verdict should not rest on it. `Absent` when no
/// people is placed, matching `name-pattern-signatures`' empty case.
fn peoples_placed(v: &FullView) -> MetricValue {
    let n = hornvale_worldgen::placed_peoples(v.world()).len();
    if n == 0 {
        return MetricValue::Absent;
    }
    MetricValue::Number(n as f64)
}

/// The share of this world's placed peoples whose naming-pattern signature
/// is UNIQUE among them (The Namesake, preregistered criterion §5.1(2)).
///
/// A directly interpretable stand-in for "a figure's people is recoverable
/// from its name structure alone": a people whose signature no other people
/// shares is recoverable with certainty from the structure, and one that
/// shares its signature is not recoverable from the structure at all. The
/// criterion compares this share against twice the chance baseline
/// (1/n_peoples). `Absent` if fewer than two peoples are placed, where
/// "recoverable above chance" has no content.
fn name_people_recoverability(v: &FullView) -> MetricValue {
    let sigs = placed_pattern_signatures(v);
    if sigs.len() < 2 {
        return MetricValue::Absent;
    }
    let unique = sigs
        .iter()
        .filter(|s| sigs.iter().filter(|o| o == s).count() == 1)
        .count();
    MetricValue::Number(unique as f64 / sigs.len() as f64)
}

/// The share of this world's founders who resolve in EXACTLY ONE element at
/// settlement scope (The Namesake, preregistered criterion §5.2(1); target
/// >= 80%).
///
/// The scope is the other founders of occupations sharing this founder's
/// `occ-site` cell — every community that has ever stood on that site, which
/// is the population a name uttered there has to pick out. `Absent` if the
/// world has no founders.
fn name_prefix_settlement_scope(v: &FullView) -> MetricValue {
    let founders = rendered_founders(v);
    if founders.is_empty() {
        return MetricValue::Absent;
    }
    // Group by site, preserving ledger order inside each group.
    let mut by_site: std::collections::BTreeMap<
        u32,
        Vec<hornvale_language::anthroponym::Rendered>,
    > = std::collections::BTreeMap::new();
    for f in &founders {
        by_site.entry(f.site).or_default().push(f.rendered.clone());
    }
    let mut single = 0usize;
    let mut total = 0usize;
    for names in by_site.values_mut() {
        let n = names.len();
        for i in 0..n {
            names.swap(i, n - 1);
            let subject = names[n - 1].clone();
            if rendered_element_count(&subject, &names[..n - 1]) == 1 {
                single += 1;
            }
            names.swap(i, n - 1);
            total += 1;
        }
    }
    MetricValue::Number(single as f64 / total as f64)
}

/// The MEDIAN number of elements this world's founders resolve in at region
/// scope (The Namesake, preregistered criterion §5.2(2); target >= 2).
///
/// The scope is every other founder in the world. `Absent` if the world has
/// no founders.
fn name_prefix_region_scope(v: &FullView) -> MetricValue {
    let mut counts = region_scope_counts(v);
    if counts.is_empty() {
        return MetricValue::Absent;
    }
    counts.sort_unstable();
    let mid = counts.len() / 2;
    let median = if counts.len() % 2 == 1 {
        counts[mid] as f64
    } else {
        (counts[mid - 1] as f64 + counts[mid] as f64) / 2.0
    };
    MetricValue::Number(median)
}

/// The share of this world's founders whose region-scope render spends EVERY
/// element their name carries (The Namesake, preregistered criterion
/// §5.2(2)'s second half; target < 50%).
///
/// §5.2(2) is two-sided by design and the median alone reads only one side
/// of it: a median of 2 is compatible both with a rule that usually saves an
/// element and with one that never does. This metric reads the other side —
/// how often the shortest-prefix rule buys nothing because the whole stack
/// is spent anyway. Note that a figure whose name carries a single speakable
/// element (see this section's module comment) counts here by construction,
/// having nothing shorter to fall back to. `Absent` if the world has no
/// founders.
fn name_prefix_region_full_stack(v: &FullView) -> MetricValue {
    let founders = rendered_founders(v);
    if founders.is_empty() {
        return MetricValue::Absent;
    }
    let lengths: Vec<usize> = founders.iter().map(|f| f.rendered.parts.len()).collect();
    let counts = region_scope_counts(v);
    let full = counts
        .iter()
        .zip(lengths.iter())
        .filter(|(spent, len)| spent == len)
        .count();
    MetricValue::Number(full as f64 / counts.len() as f64)
}

// --- The Wearing (Task 11c): the lab's own reading of the toponymic gates.
//
// Task 4 gave `hornvale_worldgen::exposure_from` seven new `Steeped` rules —
// `river`, `ford`, `hill`, `valley`, `marsh`, `spring`, `island` — each
// gated on a real terrain query over a species' settled cells rather than
// on roster membership. `independently_steeped_concepts` never learned
// them, so from the regen at `f32d6ce2` it classified as non-Steeped seven
// concepts worldgen classifies `Steeped`, and `exposure-sound-{goblin,
// kobold}` read false on 252 of 1000 worlds for a lexicon that was doing
// exactly what `exposure_from` told it to.
//
// The rules below are re-derived here rather than imported, and that is
// the whole point of the duplicate: the metric is a SECOND OPINION on
// `exposure_from`, so calling `hornvale_worldgen::exposure_from` (or its
// private `is_river_cell`/`is_hill_cell`/… helpers, which are not `pub`
// in any case) would turn the check into an echo of the thing it exists
// to check. What these functions share with worldgen is only the terrain
// domain's own public readings — `water_kind_at`, `drainage_at`,
// `elevation_at`, `sea_level`, `hydro_at`, `is_ocean`, and the geosphere's
// adjacency — which are measurements of the world, not classifications of
// it. The classification is restated.
//
// The two thresholds worldgen keeps private (its `MARSH_MIN_DRAINAGE` and
// `ISLAND_CELL_CAP`) are therefore restated as literals below rather than
// imported, and the drift that creates is the CORRECT drift for a
// soundness check. `exposure_sound` asserts "no `Root` at a concept this
// set does not hold", so an over-inclusive set here is silent and an
// under-inclusive one is loud: if worldgen ever RELAXES a gate (a lower
// marsh floor, a larger island cap) it will mint a `Root` for a concept
// this reading does not steep and the invariant goes red, which is the
// alarm firing. If worldgen TIGHTENS one, this reading is merely a
// superset and nothing fires — a soundness check has no business
// complaining that the generator was more conservative than its second
// opinion expected.
//
// `coast` and `lake` are deliberately absent: `exposure_from` classes both
// `KnowsOf`, and `build_lexicon` mints a `Root` only from `Steeped`, so
// they cannot reach the check (the same argument the doc comment below
// already makes for the KnowsOf-via-neighbour and sea-proximity rules).

/// One toponymic gate: does this cell satisfy the terrain condition that
/// steeps a concept? Named so the seven-entry table below reads as a table
/// rather than as a type signature.
type TerrainGate = fn(&hornvale_terrain::GeneratedTerrain, CellId) -> bool;

/// The wetness floor `marsh` sits above. Restated, not imported —
/// worldgen's own `MARSH_MIN_DRAINAGE` is private, and see the module note
/// above for why restating is the intended relationship rather than a
/// workaround.
const LAB_MARSH_MIN_DRAINAGE: f64 = 5.0;

/// The small-landmass ceiling `island` sits under. Restated for the same
/// reason as [`LAB_MARSH_MIN_DRAINAGE`].
const LAB_ISLAND_CELL_CAP: usize = 200;

/// Whether `cell` is a river channel: its water kind is exactly `River`.
fn lab_is_river_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    terrain.water_kind_at(cell) == hornvale_terrain::WaterKind::River
}

/// Whether `cell` is a river shallow enough to cross: a river cell whose
/// drainage has not reached waterfall scale.
fn lab_is_ford_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    lab_is_river_cell(terrain, cell)
        && terrain.drainage_at(cell) < hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE
}

/// Whether `cell` is a strict local elevation maximum over its full
/// neighbour ring, with an ocean neighbour read at sea level rather than at
/// its true depth — otherwise every coastal cell is a hill, since an ocean
/// cell is by definition below any land cell.
fn lab_is_hill_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    let sea_level = terrain.sea_level().get();
    let here = terrain.elevation_at(cell).get();
    let neighbors = terrain.geosphere().neighbors(cell);
    !neighbors.is_empty()
        && neighbors
            .iter()
            .all(|&n| terrain.elevation_at(n).get().max(sea_level) < here)
}

/// The symmetric counterpart of [`lab_is_hill_cell`]: a strict local
/// elevation minimum over the same sea-level-clamped ring.
fn lab_is_valley_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    let sea_level = terrain.sea_level().get();
    let here = terrain.elevation_at(cell).get();
    let neighbors = terrain.geosphere().neighbors(cell);
    !neighbors.is_empty()
        && neighbors
            .iter()
            .all(|&n| terrain.elevation_at(n).get().max(sea_level) > here)
}

/// Whether `cell` is damp ground that has not channelized: dry land whose
/// drainage clears [`LAB_MARSH_MIN_DRAINAGE`].
fn lab_is_marsh_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    terrain.water_kind_at(cell) == hornvale_terrain::WaterKind::DryLand
        && terrain.drainage_at(cell) >= LAB_MARSH_MIN_DRAINAGE
}

/// Whether `cell` reads directly as `Hydro::Spring`. Previously a Karst
/// proxy (`hydro_at == Karst && drainage_at >= RIVER_MIN_DRAINAGE`), because
/// `Hydro::Spring` was analytically unreachable under the original
/// carbonate-scale gate (The Witness, F5), and F5's own replacement gate was
/// itself mismeasured — pinned to a level-4 sweep but applied at the
/// model's real level-6 resolution, it made 69.64% of land Aquifer (The
/// Witness, Task 5b). Both are fixed now: `hydrogeology` gates the clastic
/// case on a porosity threshold measured on the correct population, and
/// `Spring` is no longer a still-vs-flowing drainage split at all — it is a
/// geometric descending contact (`GeneratedTerrain::hydro_at` promotes an
/// `Aquifer` cell with a lower non-`Aquifer` neighbour) — independently
/// restated here rather than calling `worldgen`'s `is_spring_cell` (the lab
/// does not depend on worldgen's window-local predicates).
fn lab_is_spring_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    terrain.hydro_at(cell) == Hydro::Spring
}

/// Whether the contiguous non-ocean landmass under `cell` stays within
/// [`LAB_ISLAND_CELL_CAP`] cells — a flood-fill that stops as soon as it
/// has seen more than the cap, so a continent costs the same bounded walk
/// as an islet. Tests the ground underfoot, not proximity to open water.
fn lab_is_island_cell(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> bool {
    let geo = terrain.geosphere();
    let mut visited: std::collections::BTreeSet<CellId> = std::collections::BTreeSet::new();
    visited.insert(cell);
    let mut frontier = vec![cell];
    while !frontier.is_empty() {
        let mut next = Vec::new();
        for &c in &frontier {
            for &n in geo.neighbors(c) {
                if !terrain.is_ocean(n) && visited.insert(n) {
                    if visited.len() > LAB_ISLAND_CELL_CAP {
                        return false;
                    }
                    next.push(n);
                }
            }
        }
        frontier = next;
    }
    true
}

/// The seven toponymic terrain gates (Task 4) and the concept each steeps
/// when satisfied — declared once, here, so [`independently_steeped_concepts`]
/// and [`steepable_concept_roster`] read the same table instead of each
/// keeping its own copy of the concept names (The Witness, Task 3: a second
/// copy of this list is exactly the drift F13 recurred on three times).
const TOPONYMIC_GATES: [(&str, TerrainGate); 7] = [
    ("river", lab_is_river_cell),
    ("ford", lab_is_ford_cell),
    ("hill", lab_is_hill_cell),
    ("valley", lab_is_valley_cell),
    ("marsh", lab_is_marsh_cell),
    ("spring", lab_is_spring_cell),
    ("island", lab_is_island_cell),
];

/// The four settlement/religion social concepts steeped unconditionally for
/// any settled species, once the registry carries them — declared once for
/// the same reason as [`TOPONYMIC_GATES`].
const FIXED_STEEPED_CONCEPTS: [&str; 4] = ["home", "hearth", "god", "spirit"];

/// The concepts an INDEPENDENT re-derivation of `species`' exposure would
/// classify `Steeped` — duplicating `exposure_from`'s own Steeped rules
/// (`windows/worldgen/src/lib.rs`) directly from ledger/roster/terrain/
/// climate data rather than calling `exposure_from` itself (spec §9.2: "the
/// flag re-derives the exposure class from the ledger independently of the
/// lexicon pipeline" — calling `exposure_from` would be the config-echo trap
/// `epithet_honorific`'s doc comment already names for a different metric).
/// Sufficient for `exposure_sound`'s "no Root at Unknown" check:
/// `build_lexicon` only ever mints a `Root` from a `Steeped` classification
/// (`KnowsOf`/`Unknown` both fall through to `Compound`/`Gap`), so the
/// KnowsOf-via-neighbor and sea-proximity rules `exposure_from` also carries
/// are irrelevant to this specific soundness check and are not reproduced
/// here. The seven toponymic terrain rules Task 4 added ARE reproduced,
/// because those ones are `Steeped` and so do reach the check — see the
/// module note above `LAB_MARSH_MIN_DRAINAGE` for how they are restated
/// and why that keeps the second opinion second. `None` if `species` is
/// not in this world's roster.
fn independently_steeped_concepts(
    v: &FullView,
    species: &str,
) -> Option<std::collections::BTreeSet<String>> {
    let perception = v
        .components()
        .perception
        .iter()
        .find(|(k, _)| k.0 == species)
        .map(|(_, p)| p)?;
    let depths = hornvale_worldgen::pack_depths(perception);
    let mut steeped: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();

    for entry in hornvale_language::universal_stratum() {
        steeped.insert(entry.concept.to_string());
    }
    for entry in hornvale_language::color_pack()
        .iter()
        .chain(hornvale_language::body_pack())
        .chain(hornvale_language::kin_pack())
    {
        if hornvale_language::in_ladder(entry, &depths) {
            steeped.insert(entry.concept.to_string());
        }
    }

    let settled: Vec<CellId> = hornvale_terrain::places(v.world())
        .into_iter()
        .filter(|p| hornvale_species::species_of(v.world(), p.id).as_deref() == Some(species))
        .filter_map(|p| {
            match v
                .world()
                .ledger
                .value_of(p.id, hornvale_settlement::CELL_ID)
            {
                Some(Value::Number(n)) => Some(CellId(*n as u32)),
                _ => None,
            }
        })
        .collect();
    for &cell in &settled {
        steeped.insert(v.climate().biome_at(cell).concept_name().to_string());
        // The Toponym: a people is steeped in the VARIANT of every cell it
        // settled, as it is in the biome. Re-derived here independently of
        // `exposure_from`, which is the point of this function.
        let expr = v.climate().biome_expr_at(cell);
        if let Some(var) = hornvale_climate::variant_at_cell(
            v.world().seed,
            cell,
            expr.formation,
            expr.stratum,
            hornvale_climate::GroundKind::Ordinary,
        ) {
            steeped.insert(var.concept_name().to_string());
        }
    }

    // Steeped: the STAPLE of every settled cell whose subsistence is
    // Farming (The Watershed). Re-derived independently of `exposure_of`,
    // which is the point of this function: worldgen reads
    // `hornvale_culture::subsistence(biome_class(biome_at(cell)), coastal)`
    // and gates on `Subsistence::Farming`; this reading calls the same
    // public climate/culture functions (never `exposure_of` itself, and
    // `hornvale_worldgen::biome_class` is a domain-agnostic biome→culture
    // classifier already used elsewhere in this file, not an exposure
    // predicate) to reach the same verdict. A crop known only through
    // herding, fishing, or foraging is an experiential gap, not a
    // perceptual one, and is deliberately never inserted here — the six
    // staples (`hornvale_climate::Crop::catalog()`) are F13's third
    // recurrence: `exposure-sound-{goblin,kobold}` read false on 767/759 of
    // 1000 worlds because this loop did not exist.
    for &cell in &settled {
        let expr = v.climate().biome_expr_at(cell);
        let Some(crop) = hornvale_climate::crop_at(
            expr.formation,
            v.climate().mean_temperature_at(cell),
            v.climate().moisture_at(cell),
        ) else {
            continue;
        };
        let coastal = v
            .terrain()
            .geosphere()
            .neighbors(cell)
            .iter()
            .any(|&n| v.terrain().is_ocean(n));
        let subsistence = hornvale_culture::subsistence(
            hornvale_worldgen::biome_class(v.climate().biome_at(cell)),
            coastal,
        );
        if subsistence == hornvale_culture::Subsistence::Farming {
            steeped.insert(crop.concept_name().to_string());
        }
    }

    let own_kind = format!("{species}-kind");
    if v.world().registry.concept(&own_kind).is_some() {
        steeped.insert(own_kind);
    }
    if !settled.is_empty() {
        let coexisting: std::collections::BTreeSet<String> = v
            .world()
            .ledger
            .find(hornvale_species::PEOPLED_BY)
            .filter_map(|f| match &f.object {
                Value::Text(s) => Some(s.clone()),
                _ => None,
            })
            .collect();
        for placed in &coexisting {
            let kind = format!("{placed}-kind");
            if v.world().registry.concept(&kind).is_some() {
                steeped.insert(kind);
            }
        }
        for concept in FIXED_STEEPED_CONCEPTS {
            if v.world().registry.concept(concept).is_some() {
                steeped.insert(concept.to_string());
            }
        }
    }

    // The seven toponymic terrain gates (Task 4), each fired by a settled
    // cell that actually satisfies it. The tuple table is deliberate: the
    // rules are uniform ("any settled cell where this predicate holds
    // steeps this concept"), so writing them as seven near-identical loops
    // would only invite one of them to drift out of the shape. `valley` is
    // in the table even though the census never saw it fire — a rule left
    // out because it is currently rare is a rule that goes stale silently
    // the first time terrain moves. The table itself lives at
    // [`TOPONYMIC_GATES`] (module scope), shared with
    // [`steepable_concept_roster`].
    let terrain = v.terrain();
    for (concept, holds) in TOPONYMIC_GATES {
        if settled.iter().any(|&cell| holds(terrain, cell)) {
            steeped.insert(concept.to_string());
        }
    }

    Some(steeped)
}

/// Every concept [`independently_steeped_concepts`] is CAPABLE of steeping,
/// for some world and some species — the roster half of that function,
/// exposed so `windows/lab/tests/roster_parity.rs` can check it against
/// worldgen's own roster without importing either side's predicates.
///
/// **Roster parity, predicate independence** (The Witness, Task 3): this
/// returns WHAT is considered, never HOW any of it is decided for a
/// particular world. F13 recurred three times (The Wearing's toponymic
/// concepts, The Toponym's variants, The Watershed's staples) because the
/// independent reading's ROSTER silently lost entries while its PREDICATES
/// stayed fine — this function exists so a test can catch the next one.
///
/// Built from exactly the same tables `independently_steeped_concepts`
/// reads for its unconditional/static rules ([`TOPONYMIC_GATES`],
/// [`FIXED_STEEPED_CONCEPTS`]) so there is only one copy of each list, plus
/// the closed catalogs the *dynamic per-cell* rules draw their concept
/// names from:
///
/// - `biome`/`variant`/`staple` are read per settled CELL (a species is
///   steeped in whichever biome/variant/crop that cell's geography and
///   climate actually produce), so no fixed cell-by-cell comparison is
///   meaningful — a census sweep would only ever witness the biomes this
///   run's seeds happen to generate. What IS meaningful, and what this
///   returns, is the full closed catalog each rule draws from:
///   [`hornvale_climate::biome::ALL`], [`hornvale_climate::Variant::catalog`],
///   [`hornvale_climate::Crop::catalog`]. Parity here means "the lab knows
///   every biome/variant/crop NAME that could ever appear," not "the lab
///   saw the same biome as some particular cell."
/// - `{species}-kind` is read per COEXISTING roster (a species is steeped in
///   the kind-concept of every OTHER species that places a settlement in
///   the same world, which varies seed to seed — see
///   `windows/worldgen/tests/exposure.rs`'s `world()` doc comment for how
///   much that placement moves). The closed universe this rule can ever
///   draw from is not seed-dependent, though: it is the set of KINDS THAT
///   SPEAK, fixed at composition root by
///   [`hornvale_worldgen::WorldComponents::assemble`]'s `articulation`
///   store (The Eremite: a family may hold a non-speaking minded kind, so
///   articulation — not the full biosphere roster — is the right closed
///   set). Enumerated here rather than hardcoded, so a sixth people joining
///   the roster enrolls automatically.
///
/// The universal stratum and the ladder-gated color/body/kin packs are
/// listed here UNCONDITIONALLY (every pack member, not only the ones a
/// given species' perception ladder reaches) for the same reason: capacity
/// to steep, not achievement for one species. Whether a given species
/// actually reaches a given ladder rung is real per-species work done only
/// in `independently_steeped_concepts`.
/// type-audit: bare-ok(identifier-text: return)
pub fn steepable_concept_roster() -> std::collections::BTreeSet<String> {
    let mut roster: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();

    for entry in hornvale_language::universal_stratum()
        .iter()
        .chain(hornvale_language::color_pack())
        .chain(hornvale_language::body_pack())
        .chain(hornvale_language::kin_pack())
    {
        roster.insert(entry.concept.to_string());
    }

    for (concept, _) in TOPONYMIC_GATES {
        roster.insert(concept.to_string());
    }
    for concept in FIXED_STEEPED_CONCEPTS {
        roster.insert(concept.to_string());
    }

    for biome in hornvale_climate::biome::ALL {
        roster.insert(biome.concept_name().to_string());
    }
    for variant in hornvale_climate::Variant::catalog() {
        roster.insert(variant.concept_name().to_string());
    }
    for crop in hornvale_climate::Crop::catalog() {
        roster.insert(crop.concept_name().to_string());
    }

    let wc = hornvale_worldgen::WorldComponents::assemble()
        .expect("the shipped composition root always assembles");
    for kind in wc.articulation.ids() {
        roster.insert(format!("{}-kind", kind.0));
    }

    roster
}

/// Whether `species`' lexicon is exposure-sound (spec §9.2): no concept the
/// independent re-derivation above classifies outside `Steeped` ever backs
/// a `Root` entry, and every committed `Gap` carries a non-empty reason.
/// `Absent` if `species` is not in this world's roster or its lexicon has
/// no entries.
fn exposure_sound(v: &FullView, species: &str) -> MetricValue {
    let Some(steeped) = independently_steeped_concepts(v, species) else {
        return MetricValue::Absent;
    };
    exposure_sound_against(v, species, &steeped)
}

/// [`exposure_sound`]'s scan over `species`' committed lexicon, against an
/// EXPLICIT `steeped` set rather than the one
/// [`independently_steeped_concepts`] derives.
///
/// Split out for one reason (The Wearing, Task 11c): it makes the metric
/// mutation-testable in-repo. A soundness flag that cannot be made to read
/// false is worse than one that reads false wrongly, and the only way to
/// break the property this flag detects — "no `Root` stands at a concept
/// the independent reading does not steep" — is to hand it a set missing a
/// concept the lexicon really did root. See
/// `exposure_sound_reports_false_when_the_toponymic_gates_are_removed`,
/// which strips exactly the seven toponymic concepts Task 4 added and
/// confirms the flag flips. Nothing in the shipped metric path passes a set
/// from anywhere but the independent derivation.
fn exposure_sound_against(
    v: &FullView,
    species: &str,
    steeped: &std::collections::BTreeSet<String>,
) -> MetricValue {
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let mut any = false;
    let mut sound = true;
    for (concept, entry) in lex.entries() {
        any = true;
        match entry {
            LexEntry::Root { .. } => {
                if !steeped.contains(concept) {
                    sound = false;
                }
            }
            LexEntry::Gap { reason } => {
                let text = match reason {
                    GapReason::Experiential(s) => s,
                    GapReason::Perceptual(s) => s,
                    GapReason::Unnameable(s) => s,
                };
                if text.is_empty() {
                    sound = false;
                }
            }
            LexEntry::Compound { .. } => {}
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(sound)
}

/// `species`' hue-ladder acquisition depth (spec §7's pack-depth model
/// card), read straight from `pack_depths` over the roster's own
/// perception vector. `Absent` if `species` is not in this world's roster.
fn hue_depth(v: &AstronomyView, species: &str) -> MetricValue {
    match v.components.perception.iter().find(|(k, _)| k.0 == species) {
        Some((_, perception)) => {
            MetricValue::Number(f64::from(hornvale_worldgen::pack_depths(perception).hue))
        }
        None => MetricValue::Absent,
    }
}

// --- The Branches (Task 10): the family battery. ---

/// The daughters whose lexicons draw from a shared goblinoid family proto
/// phonology (spec §3): goblin, hobgoblin, bugbear.
const GOBLINOID_DAUGHTERS: [&str; 3] = ["goblin", "hobgoblin", "bugbear"];

/// Every daughter this world's roster carries a lexicon for, goblinoid
/// family and the kobold outgroup alike — the population `lexicon-regular-
/// family` and `inventory-closure-*`/`homophony-count-*` range over.
const ALL_DAUGHTERS: [&str; 4] = ["goblin", "hobgoblin", "bugbear", "kobold"];

/// Whether `species` is a member of THIS view's own roster (not the global
/// species registry) — every family-battery function below must check this
/// before calling `language_of_in` (which panics on a species outside
/// `v.roster`) or before treating a `lexicon_from` result as meaningful: a
/// study pin set may build with a non-default roster (e.g.
/// `census-of-the-meeting`'s solo `[goblin]`/`[goblin-twin]` rosters), and
/// `lexicon_from` alone would silently keep resolving hobgoblin/bugbear/
/// kobold against the GLOBAL default roster even when they were never part
/// of this particular world.
fn in_roster(v: &FullView, species: &str) -> bool {
    v.components().biosphere.ids().any(|k| k.0 == species)
}

/// Whether every daughter in [`ALL_DAUGHTERS`] is lexicon-regular
/// ([`lexicon_regular`]), ANDed together — the family-wide generalization
/// of the single-species `lexicon-regular-{goblin,kobold}` metrics (spec
/// §9.1). `Absent` if no daughter in this world's roster minted a Root
/// (every daughter `Absent`).
fn lexicon_regular_family(v: &FullView) -> MetricValue {
    let mut any = false;
    let mut regular = true;
    for species in ALL_DAUGHTERS {
        match lexicon_regular(v, species) {
            MetricValue::Flag(f) => {
                any = true;
                if !f {
                    regular = false;
                }
            }
            MetricValue::Absent => {}
            other => panic!("lexicon_regular({species}) returned non-flag {other:?}"),
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(regular)
}

/// The concepts `lex` holds as a bare [`LexEntry::Root`] (mirrors
/// `windows/worldgen/src/lib.rs`'s test-only `root_concepts` helper,
/// re-implemented here since that one is private to worldgen's own test
/// module).
fn root_concepts(lex: &hornvale_language::Lexicon) -> Vec<&str> {
    lex.entries()
        .filter(|(_, e)| matches!(e, LexEntry::Root { .. }))
        .map(|(c, _)| c)
        .collect()
}

/// Re-derive the "goblinoid" family's injective proto-root assignment (epoch
/// `root/v2`) INDEPENDENTLY of any daughter's recorded derivation — over the
/// world's full registered concept universe (`exposure_from` classifies every
/// registered concept, so its key set is exactly the registry), exactly as
/// `build_lexicon` does. The shared basis for the monophyly and clean-outgroup
/// checks: it proves shared ancestry, never mere self-consistency.
fn goblinoid_proto_assignment(v: &FullView) -> std::collections::BTreeMap<String, Vec<Segment>> {
    let proto_ph = hornvale_worldgen::proto_phonology_of(v.world(), "goblinoid");
    // The universe comes from `build_lexicon`'s OWN rule, not from a second
    // copy of it. This function used to build it from every registered
    // concept, which silently disagreed with `proto_root_universe`'s
    // `Unnameable` exclusion (the nine spectral classes). That cost nothing
    // for as long as the excluded cohort sorted last — `assign_proto_roots`
    // is epoch-first and an assignment depends only on the concepts at or
    // before it — and then the compass added accession epoch 7 (`east`,
    // `west`), the first concepts ever to sort AFTER them, and this metric
    // reported a monophyly break on 14 of 1000 seeds in worlds that were
    // monophyletic. Re-deriving the DRAW independently is the point of this
    // check; re-deriving the universe RULE was the bug.
    //
    // Any goblinoid daughter's exposures serve: the map's keys are always
    // exactly `world.registry.concepts()`'s names, and `Unnameable` is a
    // property of the concept rather than of the species, so the filtered
    // universe is species-invariant — as it must be, since a family-level
    // assignment that differed per daughter could not produce cognates.
    let exposures = GOBLINOID_DAUGHTERS
        .iter()
        .filter(|s| in_roster(v, s))
        .find_map(|s| {
            hornvale_worldgen::exposure_from(v.world(), s, v.terrain(), v.climate()).ok()
        });
    let Some(exposures) = exposures else {
        return std::collections::BTreeMap::new();
    };
    let universe = hornvale_language::proto_root_universe(&exposures);
    let daughters = hornvale_worldgen::family_daughters(v.world(), v.components(), "goblinoid");
    hornvale_language::assign_proto_roots(
        &v.world().seed,
        "goblinoid",
        &proto_ph,
        &universe,
        &daughters,
    )
}

/// Whether every goblinoid daughter's Root `derivation.proto` matches its
/// concept's slot in an INDEPENDENT re-derivation of the "goblinoid" family
/// proto-root assignment (spec §3 monophyly: every daughter's rooted
/// vocabulary traces to the one family ancestor). `Absent` if no goblinoid
/// daughter in this world's roster minted a Root.
fn monophyly_goblinoid(v: &FullView) -> MetricValue {
    let assignment = goblinoid_proto_assignment(v);
    let mut any = false;
    let mut monophyletic = true;
    for species in GOBLINOID_DAUGHTERS {
        if !in_roster(v, species) {
            continue;
        }
        let Ok(lex) = lex(v, species) else {
            continue;
        };
        for (concept, entry) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = entry {
                any = true;
                if assignment.get(concept) != Some(&derivation.proto) {
                    monophyletic = false;
                }
            }
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(monophyletic)
}

/// Whether kobold — the singleton family with no siblings — never
/// coincides with the goblinoid family: for every concept kobold holds as a
/// Root, its recorded proto-root must differ from an INDEPENDENT re-draw of
/// the "goblinoid" family proto-root for that same concept (spec §3's clean
/// outgroup). `Absent` if kobold minted no Root.
fn clean_outgroup_kobold(v: &FullView) -> MetricValue {
    if !in_roster(v, "kobold") {
        return MetricValue::Absent;
    }
    let Ok(kobold_lex) = lex(v, "kobold") else {
        return MetricValue::Absent;
    };
    let assignment = goblinoid_proto_assignment(v);
    let mut any = false;
    let mut clean = true;
    for (concept, entry) in kobold_lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            any = true;
            if assignment.get(concept) == Some(&derivation.proto) {
                clean = false;
            }
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(clean)
}

/// Whether `species`' every lexicon Root's modern form draws only segments
/// present in its own drawn inventory (spec §2.2's nativization contract —
/// the per-daughter aggregate of `windows/worldgen/src/lib.rs`'s test-only
/// `every_goblinoid_word_is_in_its_inventory`, generalized to include the
/// kobold outgroup). `Absent` if `species` minted no Root.
fn inventory_closure(v: &FullView, species: &str) -> MetricValue {
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let mut any = false;
    let mut closed = true;
    for (_, entry) in lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            any = true;
            if !derivation.modern.iter().all(|s| ph.inventory.contains(s)) {
                closed = false;
            }
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Flag(closed)
}

/// Count of DISTINCT proto segments — drawn from the shared goblinoid
/// family proto-phonology — appearing in `species`' own Root proto-roots
/// that [`hornvale_language::etymology::nativize`] collapses onto an
/// existing `species` inventory segment (i.e. a proto segment absent from
/// `species`' own drawn inventory): the measured count of proto-contrasts
/// this daughter's nativization merges away (spec §3's divergence
/// magnitude — the loudness-drawn inventory decides how much of the shared
/// ancestor's phonemic space a daughter keeps distinct versus collapses).
/// `species` is expected to be a goblinoid daughter (a singleton family
/// like kobold draws its own proto directly from its own inventory, so this
/// is always 0 there — not a meaningful measurement, though not excluded by
/// this function). This is a PROXY — "how many proto contrasts this
/// daughter's inventory cannot hold," probed by re-nativizing each raw
/// proto segment in isolation — not a literal count of the substitutions
/// `evolve` performed on the surface forms (a cascade rule may change or
/// delete a segment before word-level nativization ever sees it). `Absent`
/// if `species` minted no Root.
fn divergence_magnitude(v: &FullView, species: &str) -> MetricValue {
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let mut any = false;
    let mut merged: std::collections::BTreeSet<Segment> = std::collections::BTreeSet::new();
    for (_, entry) in lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            any = true;
            for &seg in &derivation.proto {
                let nativized = hornvale_language::etymology::nativize(&[seg], &ph);
                if nativized[0] != seg {
                    merged.insert(seg);
                }
            }
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    MetricValue::Number(merged.len() as f64)
}

/// Whether some concept rooted in ALL THREE goblinoid daughters has \u{2265}2
/// distinct present-day forms (spec §3's divergence-reality guard,
/// generalized from `windows/worldgen/src/lib.rs`'s test-only
/// `goblinoid_daughters_actually_diverge` to every seed): stemmatics proves
/// descent by shared INNOVATIONS, not a shared ancestor alone, so a
/// degenerate family whose daughters are silent aliases of one another must
/// read false here. Compares recorded `derivation.modern` segment
/// sequences directly (not romanized views) to avoid any rendering-layer
/// false negative. `Absent` if no concept is rooted in all three daughters.
fn divergence_real(v: &FullView) -> MetricValue {
    if !GOBLINOID_DAUGHTERS.iter().all(|s| in_roster(v, s)) {
        return MetricValue::Absent;
    }
    let lexes: Vec<hornvale_language::Lexicon> = GOBLINOID_DAUGHTERS
        .iter()
        .filter_map(|s| lex(v, s).ok())
        .collect();
    if lexes.len() < GOBLINOID_DAUGHTERS.len() {
        return MetricValue::Absent;
    }
    let Some((first, rest)) = lexes.split_first() else {
        return MetricValue::Absent;
    };
    let shared: Vec<&str> = root_concepts(first)
        .into_iter()
        .filter(|c| rest.iter().all(|lex| root_concepts(lex).contains(c)))
        .collect();
    if shared.is_empty() {
        return MetricValue::Absent;
    }
    let diverges = shared.iter().any(|c| {
        let forms: Vec<&[Segment]> = lexes
            .iter()
            .map(|lex| match lex.entry(c) {
                Some(LexEntry::Root { derivation, .. }) => derivation.modern.as_slice(),
                _ => unreachable!("{c} confirmed rooted in every daughter above"),
            })
            .collect();
        !forms.windows(2).all(|w| w[0] == w[1])
    });
    MetricValue::Flag(diverges)
}

/// Count of distinct-concept pairs whose `species` Root `derivation.modern`
/// forms coincide (spec §3's merger-induced homophony: two proto-roots
/// collapsed onto one surface form by nativization) — an OBSERVATION, not a
/// pass/fail invariant; homophony is legal and realistic. Groups every Root
/// entry by its exact modern segment sequence and sums \u{2211} C(group_size, 2)
/// over every group larger than one. `Absent` if `species` minted no Root.
fn homophony_count(v: &FullView, species: &str) -> MetricValue {
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let Ok(lex) = lex(v, species) else {
        return MetricValue::Absent;
    };
    let mut by_form: std::collections::BTreeMap<Vec<Segment>, usize> =
        std::collections::BTreeMap::new();
    let mut any = false;
    for (_, entry) in lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            any = true;
            *by_form.entry(derivation.modern.clone()).or_insert(0) += 1;
        }
    }
    if !any {
        return MetricValue::Absent;
    }
    let pairs: usize = by_form.values().map(|&n| n * n.saturating_sub(1) / 2).sum();
    MetricValue::Number(pairs as f64)
}

/// The homophony breakdown [`classify_homophony`] returns over a set of
/// rooted words: the confusable-core pair count and the draw-vs-merger
/// cluster split. Pure data, so the classifier is unit-testable without
/// building a world.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct HomophonyStats {
    /// Number of distinct-concept collision PAIRS where BOTH concepts are
    /// core — the functional-load-restricted homophony. `\u{2211} C(core_members, 2)`
    /// over every surface form held by more than one root.
    core_pairs: usize,
    /// Of the core pairs, the **confusable** subset: both concepts share a
    /// semantic domain (universal/body/kin), so they compete in the same
    /// context and a listener cannot separate them by topic — the genuinely
    /// parsing-costly homophony (spec §10 Q3). The complement (`core_pairs -
    /// confusable_pairs`) is FREE: cross-domain core collisions a listener
    /// resolves by topic, the codon-degeneracy case where a collision is
    /// harmless because the classes don't compete. Always \u{2264} `core_pairs`.
    confusable_pairs: usize,
    /// Number of surface forms held by more than one root (any collision).
    collision_clusters: usize,
    /// Of those, the number that are **mergers**: the colliding roots carry
    /// \u{2265}2 DISTINCT proto-forms, so the collision was created after the proto
    /// by the sound-change cascade or nativization — not present at draw
    /// time. The complement (one shared proto) are draw-collisions. This
    /// split decides whether family-proto injective assignment alone
    /// suffices (draw-dominated) or a post-evolution re-merger check is also
    /// required (merger share material).
    merger_clusters: usize,
}

/// Group `(modern_form, proto_form, domain)` triples by surface form and tally
/// the core-pair count, the confusable (same-domain) subset, and the
/// draw-vs-merger cluster split. `domain` is `Some(field)` for a core concept
/// (its semantic domain) and `None` for periphery — so `domain.is_some()` is
/// core-hood and two core members are confusable iff their domains are equal.
/// Pure and total; generic over the form/proto/domain types so it can be
/// unit-tested with plain strings and driven from real `Vec<Segment>` forms
/// alike. A form held by a single root is not a collision and contributes
/// nothing.
fn classify_homophony<F: Ord, P: Ord, D: Ord>(entries: &[(F, P, Option<D>)]) -> HomophonyStats {
    let mut by_form: std::collections::BTreeMap<&F, Vec<(&P, Option<&D>)>> =
        std::collections::BTreeMap::new();
    for (form, proto, domain) in entries {
        by_form
            .entry(form)
            .or_default()
            .push((proto, domain.as_ref()));
    }
    let mut stats = HomophonyStats {
        core_pairs: 0,
        confusable_pairs: 0,
        collision_clusters: 0,
        merger_clusters: 0,
    };
    for members in by_form.values() {
        if members.len() < 2 {
            continue;
        }
        stats.collision_clusters += 1;
        let distinct_protos: std::collections::BTreeSet<&P> =
            members.iter().map(|(p, _)| *p).collect();
        if distinct_protos.len() >= 2 {
            stats.merger_clusters += 1;
        }
        let core_members = members.iter().filter(|(_, d)| d.is_some()).count();
        stats.core_pairs += core_members * core_members.saturating_sub(1) / 2;
        // Confusable pairs: core members grouped by shared domain, summed as
        // C(n, 2) within each domain — same-field collisions the listener
        // cannot resolve by topic.
        let mut by_domain: std::collections::BTreeMap<&D, usize> =
            std::collections::BTreeMap::new();
        for (_, d) in members {
            if let Some(dom) = d {
                *by_domain.entry(*dom).or_insert(0) += 1;
            }
        }
        for &n in by_domain.values() {
            stats.confusable_pairs += n * n.saturating_sub(1) / 2;
        }
    }
    stats
}

/// Extract every `species` Root's `(modern, proto, is_core)` triple and
/// classify it — the shared body under both the `core-homophony-*` and
/// `homophony-merger-share-*` metrics. `None` if `species` is off-roster or
/// minted no Root.
fn homophony_stats(v: &FullView, species: &str) -> Option<HomophonyStats> {
    if !in_roster(v, species) {
        return None;
    }
    let lex = lex(v, species).ok()?;
    let mut triples: Vec<(Vec<Segment>, Vec<Segment>, Option<&'static str>)> = Vec::new();
    for (concept, entry) in lex.entries() {
        if let LexEntry::Root { derivation, .. } = entry {
            triples.push((
                derivation.modern.clone(),
                derivation.proto.clone(),
                concept_domain(concept),
            ));
        }
    }
    if triples.is_empty() {
        return None;
    }
    Some(classify_homophony(&triples))
}

/// Count of confusable-core homophone pairs in `species`' lexicon — the
/// functional-load-restricted homophony the fix targets (both concepts of
/// the colliding pair are core vocabulary). `Absent` if `species` is
/// off-roster or minted no Root. Always `\u{2264}` the unrestricted
/// `homophony-count-{species}`.
fn core_homophony(v: &FullView, species: &str) -> MetricValue {
    match homophony_stats(v, species) {
        Some(s) => MetricValue::Number(s.core_pairs as f64),
        None => MetricValue::Absent,
    }
}

/// Count of **confusable** core homophone pairs — the same-semantic-domain
/// subset of [`core_homophony`], the genuinely parsing-costly collisions a
/// listener cannot separate by topic (spec §10 Q3). Its complement within
/// `core-homophony-{species}` is FREE (cross-domain) homophony, the
/// codon-degeneracy case where a collision is harmless. This is the number
/// that justifies "accept the atonal tail" as a measurement rather than an
/// assertion. `Absent` if `species` is off-roster or minted no Root; always
/// `\u{2264}` `core-homophony-{species}`.
fn confusable_homophony(v: &FullView, species: &str) -> MetricValue {
    match homophony_stats(v, species) {
        Some(s) => MetricValue::Number(s.confusable_pairs as f64),
        None => MetricValue::Absent,
    }
}

/// `species`' derived life-history profile (BIO-2 spec §5), read from the
/// biosphere component's `mass`/`metabolic_class` — a pure `f(Mass,
/// MetabolicClass)`, no draws. `None` if `species` is off-roster.
fn species_life_history(v: &FullView, species: &str) -> Option<hornvale_species::LifeHistory> {
    let bio = v
        .components()
        .biosphere
        .iter()
        .find(|(k, _)| k.0 == species)
        .map(|(_, b)| b)?;
    Some(hornvale_species::life_history(
        bio.mass,
        bio.metabolic_class,
    ))
}

/// `species`' maximum lifespan in years (BIO-2 spec §4/§5). `Absent` if
/// `species` is off-roster or `Ametabolic` (a construct has no mass-derived
/// lifespan).
fn species_lifespan_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species).and_then(|lh| lh.lifespan) {
        Some(years) => MetricValue::Number(years.get()),
        None => MetricValue::Absent,
    }
}

/// `species`' age at first reproduction in years (BIO-2 spec §4/§5). `Absent`
/// if `species` is off-roster or `Ametabolic`.
fn species_age_at_maturity_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species).and_then(|lh| lh.age_at_maturity) {
        Some(years) => MetricValue::Number(years.get()),
        None => MetricValue::Absent,
    }
}

/// `species`' reference-temperature basal metabolic rate in watts (BIO-2
/// spec §4). Always present — `0.0` for `Ametabolic`, never `None`. `Absent`
/// only if `species` is off-roster.
fn species_basal_metabolic_rate_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species) {
        Some(lh) => MetricValue::Number(lh.basal_metabolic_rate_w),
        None => MetricValue::Absent,
    }
}

/// `species`' reproductive output on the r–K axis, 0 (fast/prolific) … 1
/// (slow/sparse) (BIO-2 spec §4/CAP-2). `Absent` if `species` is off-roster
/// or `Ametabolic`.
fn species_reproductive_tempo_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species).and_then(|lh| lh.reproductive_tempo) {
        Some(tempo) => MetricValue::Number(tempo),
        None => MetricValue::Absent,
    }
}

/// `species`' generation length in years (BIO-2 spec §5, MEM-7's handle).
/// `Absent` if `species` is off-roster or `Ametabolic`.
fn species_generation_length_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species).and_then(|lh| lh.generation_length) {
        Some(years) => MetricValue::Number(years.get()),
        None => MetricValue::Absent,
    }
}

/// `species`' overall life-history speed, 0 (fast) … 1 (slow) — an absolute,
/// roster-independent position defined for anything with mass (BIO-2 spec
/// §5), so this is present even for `Ametabolic`. `Absent` only if `species`
/// is off-roster.
fn species_pace_of_life_metric(v: &FullView, species: &str) -> MetricValue {
    match species_life_history(v, species) {
        Some(lh) => MetricValue::Number(lh.pace_of_life),
        None => MetricValue::Absent,
    }
}

/// The size of `species`' realized tone inventory (spec §11): 1 for an atonal
/// people (the shipped humanoids), >1 for a tone-capable one. `Absent` if
/// `species` is off-roster.
fn tone_count_metric(v: &FullView, species: &str) -> MetricValue {
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    MetricValue::Number(hornvale_language::tone_inventory(&ph).len() as f64)
}

/// `species`' distinguishable-syllable capacity (spec §2.3, §11): a lower
/// bound on the distinct syllables its phonology can form (onset × nucleus ×
/// coda fillings, tone folded into the nucleus). The channel capacity the
/// floor guarantees a minimum of for tone-capable species. `Absent` if
/// `species` is off-roster.
fn distinguishable_capacity_metric(v: &FullView, species: &str) -> MetricValue {
    if !in_roster(v, species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(v.world(), v.components(), species);
    MetricValue::Number(hornvale_language::distinguishable_capacity(&ph) as f64)
}

/// Fraction of `species`' colliding surface forms that are **mergers** (the
/// colliding roots carry \u{2265}2 distinct proto-forms — the cascade or
/// nativization created the collision after the proto) rather than
/// draw-collisions (one shared proto). `Absent` if `species` is off-roster,
/// minted no Root, or has no collision at all (an undefined ratio, never
/// reported as 0). Decides whether proto-injective assignment alone suffices.
fn homophony_merger_share(v: &FullView, species: &str) -> MetricValue {
    match homophony_stats(v, species) {
        Some(s) if s.collision_clusters > 0 => {
            MetricValue::Number(s.merger_clusters as f64 / s.collision_clusters as f64)
        }
        _ => MetricValue::Absent,
    }
}

/// The attested tier at the roman level (The Speakable): the lowercased
/// roman rendering of every modern root form `lexicon` holds, deduped,
/// longest-first. The surface-string twin of
/// `hornvale_language`'s segment-level attested tier, so this validator
/// accepts exactly the names `glossed_name` now emits.
fn attested_roman_forms(lexicon: &hornvale_language::Lexicon) -> Vec<String> {
    let mut forms: Vec<String> = lexicon
        .entries()
        .filter_map(|(_, entry)| match entry {
            hornvale_language::LexEntry::Root { derivation, .. }
                if !derivation.modern.is_empty() =>
            {
                Some(
                    hornvale_language::render_views(&derivation.modern)
                        .roman
                        .to_lowercase(),
                )
            }
            _ => None,
        })
        .collect();
    forms.sort_by(|a, b| {
        b.chars()
            .count()
            .cmp(&a.chars().count())
            .then_with(|| a.cmp(b))
    });
    forms.dedup();
    forms
}

/// Whether `name` parses as a legal sequence of syllables under `ph`, OR is
/// exactly an attested word from `attested_roman`, independently of
/// `hornvale_language::naming`'s generation code path: this walks the
/// SURFACE STRING back into [`Segment`]s and re-checks phonotactic legality
/// from scratch — every syllable's onset/coda manner-sequence must match
/// one of `ph.onsets`/`ph.codas` (the very templates `draw_phonology`
/// drew), its nucleus must be a vowel run of one of `ph.nuclei`'s admissible
/// sizes (the syllable picks a nucleus template the same way it picks an
/// onset and a coda — The Wearing), and every segment
/// consumed must be a member of `ph.inventory`. Several romanizations are
/// literal PREFIXES of others sharing the same manner (`z`/`zh`, `s`/`sh`,
/// `n`/`ng`, `k`/`kx`), so a single greedy match per slot is unsound (a "z"
/// false-match can swallow what was really a "zh"); every matcher below
/// returns every reachable position and `parse_syllables` backtracks over
/// the full cross product of segment choice, template choice, and attested
/// word choice. Two tiers, both admissible: the canon template tier (every
/// syllable legally built from `ph`) and the attested tier (a whole word
/// lifted verbatim from `attested_roman`, mirroring the segment-level
/// attested tier `domains/language` gained first) — a name may mix both,
/// consuming attested words and template syllables in any order. Callers
/// deriving `attested_roman` from a lexicon must resolve `species` within
/// the roster first, same as every other lexicon-derived caller in this
/// module (see the caveat at [`in_roster`]'s doc, and `phonotactic_validity`
/// for the pattern: `language_of_in`/`lexicon_from` together against
/// `v.roster()`).
fn is_phonotactically_valid(name: &str, ph: &Phonology, attested_roman: &[String]) -> bool {
    let chars: Vec<char> = name.to_lowercase().chars().collect();
    !chars.is_empty() && parse_syllables(&chars, 0, ph, attested_roman)
}

/// Recursively consume one syllable — or one attested word from
/// `attested_roman` — at a time from `chars[pos..]`; true iff the
/// remainder parses as a sequence of legal syllables and/or attested
/// words. The base case (`pos == chars.len()`) is only reachable after a
/// caller has already consumed at least one syllable or word, so an empty
/// name never validates (see [`is_phonotactically_valid`]'s explicit empty
/// check).
fn parse_syllables(chars: &[char], pos: usize, ph: &Phonology, attested_roman: &[String]) -> bool {
    if pos == chars.len() {
        return true;
    }
    for word in attested_roman {
        let w: Vec<char> = word.chars().collect();
        if chars[pos..].starts_with(&w[..])
            && parse_syllables(chars, pos + w.len(), ph, attested_roman)
        {
            return true;
        }
    }
    let mut onsets: Vec<&Vec<Manner>> = ph.onsets.iter().collect();
    onsets.sort();
    onsets.dedup();
    let mut codas: Vec<&Vec<Manner>> = ph.codas.iter().collect();
    codas.sort();
    codas.dedup();
    for onset in &onsets {
        for after_onset in match_manner_run(chars, pos, onset, ph) {
            for &size in &ph.nuclei {
                for after_nucleus in match_nucleus(chars, after_onset, size, ph) {
                    for coda in &codas {
                        for after_coda in match_manner_run(chars, after_nucleus, coda, ph) {
                            if parse_syllables(chars, after_coda, ph, attested_roman) {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

/// Every position reachable by consuming a consonant cluster matching
/// `template` (one inventory consonant of each listed manner, in order)
/// starting at `chars[pos..]`, trying every same-manner candidate at each
/// slot (see the module note on prefix ambiguity).
fn match_manner_run(chars: &[char], pos: usize, template: &[Manner], ph: &Phonology) -> Vec<usize> {
    let mut positions = vec![pos];
    for &manner in template {
        let mut next = Vec::new();
        for &p in &positions {
            for seg in ph
                .inventory
                .iter()
                .filter(|s| matches!(s, Segment::Consonant { manner: m, .. } if *m == manner))
            {
                let r = romanize(seg);
                if matches_literal(chars, p, r) {
                    next.push(p + r.chars().count());
                }
            }
        }
        next.sort_unstable();
        next.dedup();
        if next.is_empty() {
            return Vec::new();
        }
        positions = next;
    }
    positions
}

/// Every position reachable by consuming exactly `count` inventory vowels
/// from `chars[pos..]`, in sequence.
fn match_nucleus(chars: &[char], pos: usize, count: usize, ph: &Phonology) -> Vec<usize> {
    let mut positions = vec![pos];
    for _ in 0..count {
        let mut next = Vec::new();
        for &p in &positions {
            for seg in ph
                .inventory
                .iter()
                .filter(|s| matches!(s, Segment::Vowel { .. }))
            {
                let r = romanize(seg);
                if matches_literal(chars, p, r) {
                    next.push(p + r.chars().count());
                }
            }
        }
        next.sort_unstable();
        next.dedup();
        if next.is_empty() {
            return Vec::new();
        }
        positions = next;
    }
    positions
}

/// Whether `s`'s characters literally match `chars` starting at `pos`.
fn matches_literal(chars: &[char], pos: usize, s: &str) -> bool {
    let needle: Vec<char> = s.chars().collect();
    pos + needle.len() <= chars.len() && chars[pos..pos + needle.len()] == needle[..]
}

/// Render the metrics list as a markdown table, in registry order.
///
/// Each row names the metric, its summary kind, its histogram bucket edges
/// (populated for `SummaryKind::Numeric`; blank for `Categorical`/`Flag`,
/// which have no buckets), and its one-line doc.
/// type-audit: bare-ok(artifact: return)
pub fn render_metric_list() -> String {
    let metrics = registry();
    let mut table = String::from("| Name | Kind | Buckets | Doc |\n");
    table.push_str("|---|---|---|---|\n");
    for m in metrics {
        let (kind_str, buckets_str) = match &m.summary {
            SummaryKind::Categorical => ("Categorical".to_string(), String::new()),
            SummaryKind::Flag => ("Flag".to_string(), String::new()),
            SummaryKind::Numeric { bucket_edges } => {
                let bucket_list = bucket_edges
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ");
                ("Numeric".to_string(), format!("[{}]", bucket_list))
            }
        };
        table.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            m.name, kind_str, buckets_str, m.doc
        ));
    }
    table
}

#[cfg(test)]
mod tests {
    // Test fixture (decision 0092): calls the sculpt/fit derivation entry
    // points directly to build its own world state, once per test — the
    // sanctioned test-fixture posture the weir's spec carves out.
    #![allow(clippy::disallowed_methods)]
    use super::*;

    #[test]
    fn narrowed_views_build_and_coerce() {
        let pins = SkyPins::default();
        assert!(AstronomyView::build(Seed(42), &pins).is_ok());
        let terrain = TerrainView::build(Seed(42), &pins);
        assert!(terrain.is_ok());
        let terrain = terrain.unwrap();
        assert!(ClimateView::build(Seed(42), &pins).is_ok());
        assert!(SettlementView::build(Seed(42), &pins).is_ok());
        let full = FullView::build(Seed(42), &pins);
        assert!(full.is_ok());
        let full = full.unwrap();

        let coerced: &TerrainView = full.as_ref();
        assert_eq!(coerced.globe.plate_count, terrain.globe.plate_count);
    }

    /// Extract `name`'s metric from an already-built `BuiltView`, panicking
    /// if the metric isn't registered — a small test convenience so each
    /// test doesn't hand-roll the registry lookup.
    fn extract_from(built: &BuiltView, name: &str) -> MetricValue {
        registry()
            .into_iter()
            .find(|m| m.name == name)
            .unwrap_or_else(|| panic!("metric {name} not registered"))
            .extract
            .apply(built)
    }

    #[test]
    fn seed_42_default_builds_successfully() {
        let view = AstronomyView::build(Seed(42), &SkyPins::default());
        assert!(view.is_ok());
        let view = view.unwrap();
        assert!(!view.system.moons.is_empty());
        assert!(!view.system.neighbors.is_empty());
    }

    #[test]
    fn seed_42_star_class_is_text() {
        let view = AstronomyView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "star-class");
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    #[test]
    fn seed_42_moons_admitted_is_text() {
        let view = AstronomyView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "moons-admitted");
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    #[test]
    fn seed_42_belief_kind_goblin_is_text_and_not_absent() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "belief-kind-goblin");
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    /// The Presiding (SKY-25): a world has no religion, its peoples do. The
    /// retired `belief-kind` read `beliefs_of(&world).first()` — whichever
    /// people sorted first in the alphabetical component registry, which on
    /// every measured seed is a single founder-floor goblin.
    #[test]
    fn belief_kind_is_per_species_and_the_world_belief_is_gone() {
        let reg = registry();
        assert!(
            !reg.iter().any(|m| m.name == "belief-kind"),
            "the world-level belief-kind is retired: a world has no presiding belief"
        );
        for species in ["bugbear", "goblin", "hobgoblin", "kobold"] {
            let name = format!("belief-kind-{species}");
            assert!(
                reg.iter().any(|m| m.name == name),
                "{name} is registered — every people gets its own reading"
            );
        }
    }

    /// Mutation guard: `belief-kind-<species>` must read THAT people's head, not
    /// the world's first-minted belief. On every measured seed the first-minted
    /// belief is goblin's (a single founder-floor soul), so a metric that read
    /// `beliefs_of().first()` would give every species goblin's answer.
    #[test]
    fn each_peoples_belief_kind_is_its_own_not_the_first_minted() {
        // Re-pointed under The Living Community epoch (this merge) from seed 42
        // to seed 8: the deep-history bake now seeds all four peoples together,
        // so at most seeds every placed people shares the same head sentiment
        // (all "cyclic" at seed 42) — which cannot discriminate a species-aware
        // reader from a first-minted reader. Seed 8 is the nearest seed whose
        // placed roster splits: bugbear's flagship head is "ambient" while
        // goblin's is "eternal", so the two readings MUST differ if (and only
        // if) the metric honours its species argument.
        let v = FullView::build(Seed(8), &SkyPins::default()).unwrap();
        let goblin = species_head_sentiment(&v, "goblin");
        let bugbear = species_head_sentiment(&v, "bugbear");
        assert!(
            goblin.is_some() && bugbear.is_some(),
            "seed 8 places both peoples"
        );
        // The reading must depend on the species argument, not ignore it. A
        // species-ignoring implementation (reading `beliefs_of().first()`)
        // would return the world's first-minted answer for every people,
        // collapsing all four readings to one value. Assert the argument
        // actually discriminates: goblin ("eternal") and bugbear ("ambient")
        // read differently.
        assert_ne!(
            goblin, bugbear,
            "the reading discriminates on the species asked for, not the world's first belief"
        );
    }

    #[test]
    fn locked_world_is_tidally_locked() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = AstronomyView::build(Seed(42), &pins).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "tidally-locked");
        assert_eq!(value, MetricValue::Flag(true));
    }

    #[test]
    fn locked_world_has_no_day_length() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = AstronomyView::build(Seed(42), &pins).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "day-length-hours");
        assert_eq!(value, MetricValue::Absent);
    }

    #[test]
    fn locked_world_has_no_local_day_year() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = AstronomyView::build(Seed(42), &pins).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "year-local-days");
        assert_eq!(value, MetricValue::Absent);
    }

    #[test]
    fn locked_world_goblin_belief_kind_is_eternal() {
        // SEQ-1 realized by SKY-5: a locked world's sky is frozen, so a
        // low-sky-attention first observer's felt tide (Venue::Ambient)
        // out-ranks the motionless sun in ITS OWN pantheon — that
        // observation is unchanged. What changed is WHICH species commits
        // the world's first pantheon (and so whose ranking `beliefs_of`'s
        // first entry reflects): under the niche-differentiated-K
        // coexistence-stack cutover (The Niche), only goblin and hobgoblin
        // win a settlement's dominance at seed 42 (bugbear — the
        // low-sky-attention species this test used to observe through —
        // never flagships anymore; see
        // `bugbear_and_kobold_are_present_in_settlement_composition` in
        // `cli/tests/branches_identity.rs`), and `culture+religion+species`
        // genesis walks `species_set` in roster order, so goblin (the
        // first alphabetically among the two that still flagship) commits
        // the world's first pantheon now. Goblin's own perception ranks
        // the motionless sun (Sentiment::Eternal, source_kind
        // "celestial-body") ahead of the tide — the baseline sky-attention
        // reading, not bugbear's low one — so the pantheon's head is the
        // sun again, sentiment "eternal".
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        // Re-pinned under The Living Community epoch (this merge): the
        // deep-history bake re-placed every world and seeds all four peoples,
        // so the species that commits the world's FIRST pantheon changed — the
        // first-minted belief now reads a felt-tide head (source_kind "tide"),
        // not the sun. Goblin's OWN head, however, is still the motionless sun
        // ("eternal"), the property this test actually checks: the epoch moved
        // which species mints first, not goblin's own perception.
        let view = FullView::build(Seed(42), &pins).unwrap();
        let first = beliefs_of(view.world())
            .into_iter()
            .next()
            .expect("locked world has beliefs");
        assert_eq!(first.source_kind, "tide");
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "belief-kind-goblin");
        assert_eq!(value, MetricValue::Text("eternal".to_string()));
    }

    #[test]
    fn spinning_world_goblin_belief_kind_is_cyclic() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        };
        let view = FullView::build(Seed(42), &pins).unwrap();
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "belief-kind-goblin");
        assert_eq!(value, MetricValue::Text("cyclic".to_string()));
    }

    #[test]
    fn seed_23_refused_a_moon() {
        let view = AstronomyView::build(Seed(23), &SkyPins::default()).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "refused-a-moon");
        assert_eq!(value, MetricValue::Flag(true));
    }

    #[test]
    fn seed_23_genesis_note_count_is_one() {
        let view = AstronomyView::build(Seed(23), &SkyPins::default()).unwrap();
        let built = BuiltView::Astronomy(view);
        let value = extract_from(&built, "genesis-note-count");
        assert_eq!(value, MetricValue::Text("1".to_string()));
    }

    #[test]
    fn registry_metric_count_is_pinned() {
        // The Meeting's 63, +7 for The Words (Task 12: name-gloss-true,
        // lexicon-regular-{goblin,kobold}, exposure-sound-{goblin,kobold},
        // hue-depth-{goblin,kobold}), plus the terrain-shape and later
        // The terrain-shape metrics + landmass-count (Crust Task 9: continent-count
        // gained a size floor, and this unfloored companion preserves the old series),
        // UNIONED with main's campaigns merged here: The Branches' family battery
        // (lexicon-regular-family, monophyly-goblinoid, clean-outgroup-kobold,
        // inventory-closure-{goblin,hobgoblin,bugbear,kobold},
        // divergence-magnitude-{goblin,hobgoblin,bugbear}, divergence-real,
        // homophony-count-{goblin,hobgoblin,bugbear,kobold}) and the phonology epoch
        // (confusable-homophony-{goblin,hobgoblin,bugbear,kobold},
        // tone-count-{goblin,kobold}, distinguishable-capacity-{goblin,bugbear,kobold}),
        // +2 for the-gathering (Task 8: capacity-by-abs-latitude, rank-size-slope),
        // +2 more for the Task 8 review fix (total-population,
        // pop-weighted-abs-latitude — the two metrics the brief named that
        // were never built), +3 for night-sky stage 3 (Task 10: figure-count,
        // largest-figure-members, ecliptic-figure-count), +4 for Eclipse
        // Seasons (Task 11: eclipse-year-days, solar-eclipses-per-century,
        // lunar-eclipses-per-century, coincidence-days-per-century), +5 for
        // The Ground (Task 7: dominant-rock, karst-fraction,
        // aquifer-fraction, dominant-soil-order, fertile-land-fraction),
        // +2 for The Long Count (Task 6: brightening-per-gyr,
        // alignment-drift-deg-per-kyr), +1 for the coexistence stack
        // (Task A16a: per-cell-diversity),
        // +12 for BIO-2 (Task 6, per-species goblin+kobold pairs matching
        // the tone-count-{species} convention: lifespan-years-{goblin,kobold},
        // age-at-maturity-years-{goblin,kobold},
        // basal-metabolic-rate-w-{goblin,kobold},
        // reproductive-tempo-{goblin,kobold},
        // generation-length-years-{goblin,kobold},
        // pace-of-life-{goblin,kobold}),
        // +1 for The Niche (composition-variance), +6 for Sculpting
        // (Task 12: shelf-width-passive-median, shelf-width-active-median,
        // sediment-volume, waterfall-count, delta-count,
        // rerouted-flow-fraction), +1 for rift-and-fit (Task 9:
        // coast-roughness-slope), +3 for The Presiding (SKY-25: the
        // world-level belief-kind is retired, replaced by
        // belief-kind-{bugbear,goblin,hobgoblin,kobold} — net -1 +4),
        // +6 for The Chorus (C4 Task 5, LANG-41: chorus-distortion,
        // chorus-distinctiveness, chorus-recoverability, chorus-variance,
        // chorus-param-spread, chorus-sky-calibration), +3 for The Deep
        // (Task 5: mean-depth-to-basement, unconformity-fraction,
        // mean-geothermal-gradient), +4 for The Lode (Task 7:
        // cave-fraction, deposit-density, dominant-commodity,
        // mean-ore-grade), +4 for The Vestige (Task 7: vestige-density,
        // forgotten-fraction, dominant-hazard, mean-warning-legibility),
        // +3 for The Wearing (Task 11: name-syllables-{goblin,kobold} —
        // per-species, beside the name-length-{species} pair they are read
        // against — and the world-level name-transparency), +3 for The
        // Contour (Task 4: peoples-alive-at-bake-end, largest-holding-share,
        // and — round 3, spec §2.4 amendment 4 —
        // defensibility-capacity-rank-corr, registered on present-day
        // terrain/connection-graph rather than the bake's own final era,
        // labelled as such in its own doc string),
        // +2 for The Namesake (Task 1: cascade-rules-fired-{goblin,bugbear}),
        // +5 more for The Namesake (Task 7, the preregistered claims:
        // name-pattern-signatures and name-people-recoverability read §5.1;
        // name-prefix-settlement-scope reads §5.2(1); name-prefix-region-scope
        // and name-prefix-region-full-stack read the two OPPOSITE halves of
        // §5.2(2), which the median alone cannot separate), +1 more at Task 7's
        // fix round (peoples-placed: the n behind §5.1(2)'s 1/n chance
        // baseline, so that verdict is re-derivable from rows.csv rather than
        // from an inversion done in prose).
        //
        // The Contour and The Namesake were developed in parallel off the same
        // base and both moved this pin: 172 -> 175 there, 172 -> 180 here. The
        // merged value is neither — it is 172 + 3 + 8. This line is the one
        // place the two campaigns' metric sets could have been silently
        // reconciled to a wrong number, which is why both provenance comments
        // are kept rather than one replacing the other.
        assert_eq!(registry().len(), 183);
    }

    // --- The Wearing (Task 11): the syllable and transparency readings. ---

    /// `syllable_count` on hand-built inputs whose syllable structure is not
    /// in dispute — the counting rule itself, checked against ground truth
    /// stated by hand rather than against anything the namer produced.
    #[test]
    fn syllable_count_reads_maximal_vowel_runs() {
        let vowels: std::collections::BTreeSet<char> = "aeiou".chars().collect();
        for (name, want) in [
            ("", 0),
            ("k", 0),
            ("ba", 1),
            // B-o-d-o-b-aa-d-o: four runs (`aa` is one).
            ("Bodobaado", 4),
            ("dzoxgzhofdzha", 3),
            // A run of two vowels is ONE nucleus (the documented proxy).
            ("baado", 2),
            // Capitalization must not matter: names are committed capitalized.
            ("BODOBAADO", 4),
            // Word-initial and word-final runs both count.
            ("ak", 1),
            ("ka", 1),
        ] {
            assert_eq!(
                syllable_count(name, &vowels),
                want,
                "{name} should read {want} syllables"
            );
        }
    }

    /// A combining tone mark rides ON a nucleus; it must neither break a
    /// vowel run nor add one. The three marks in use plus a fourth from the
    /// same block that no tone level claims today.
    #[test]
    fn a_tone_mark_neither_breaks_nor_adds_a_syllable() {
        let vowels: std::collections::BTreeSet<char> = "aeiou".chars().collect();
        // "báado" — an acute on the first vowel of a two-vowel run.
        assert_eq!(syllable_count("ba\u{0301}ado", &vowels), 2);
        // "bàdò" — grave marks on two separate nuclei.
        assert_eq!(syllable_count("ba\u{0300}do\u{0300}", &vowels), 2);
        // A macron, and a combining mark from the same block nothing uses.
        assert_eq!(syllable_count("ba\u{0304}do\u{0308}", &vowels), 2);
    }

    /// The vowel set comes from the phonology's own inventory, so a language
    /// that never drew `u` does not count a `u` as a nucleus. Guards the
    /// tempting hard-coded `aeiou`.
    #[test]
    fn vowel_graphemes_come_from_the_inventory_not_a_hardcoded_alphabet() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let ph = language_of_in(view.world(), view.components(), "goblin");
        let vowels = vowel_graphemes(&ph);
        assert!(!vowels.is_empty(), "a phonology always has vowels");
        assert!(
            vowels.iter().all(|c| "aeiou".contains(*c)),
            "every romanized vowel is one of the five roman vowel letters: {vowels:?}"
        );
        let inventory_vowels = ph
            .inventory
            .iter()
            .filter(|s| matches!(s, Segment::Vowel { .. }))
            .count();
        assert!(
            vowels.len() <= inventory_vowels,
            "the set never invents a vowel the inventory lacks"
        );
    }

    /// A gloss cannot be `split('-')`: biome concept ids are themselves
    /// hyphenated. The segmentation must read `coast-temperate-forest` as
    /// two concepts, and must stay unique when a hyphenated id sits beside
    /// short ones.
    #[test]
    fn a_gloss_parses_into_whole_concepts_not_hyphen_pieces() {
        let vocab: std::collections::BTreeSet<&str> = ["coast", "river", "temperate-forest", "sun"]
            .into_iter()
            .collect();
        assert_eq!(
            gloss_parses("coast-temperate-forest", &vocab),
            vec![vec!["coast", "temperate-forest"]]
        );
        assert_eq!(
            gloss_parses("temperate-forest", &vocab),
            vec![vec!["temperate-forest"]]
        );
        assert_eq!(
            gloss_parses("river-coast-sun", &vocab),
            vec![vec!["river", "coast", "sun"]]
        );
        // A naive `split('-')` would read four concepts here and find no
        // words for "temperate" or "forest"; the parser reads two.
        assert_eq!(gloss_parses("coast-temperate-forest", &vocab)[0].len(), 2);
        // Nothing in the vocabulary: no parse at all, rather than a wrong one.
        assert!(gloss_parses("hill-marsh", &vocab).is_empty());
    }

    /// `PRESIDING_CONCEPTS` covers every concept a REAL seed-42 world's
    /// rostered phenomena actually gloss to — checked against
    /// `hornvale_worldgen::observed_phenomena` on a built world, not against
    /// hand-written fixtures. A hand-typed fixture's `concept` field is
    /// whatever the test author wrote, so it can only be changed by editing
    /// the fixture, never by a production change — that shape was found to
    /// make this test tautological (final fix wave, campaign close review).
    /// Deriving the cases from a live world instead means a later campaign
    /// that teaches a new phenomenon kind to gloss, or points an existing
    /// kind's referent at an unlisted concept, actually reds this test rather
    /// than leaving `name-transparency` to silently fail to parse the
    /// glosses that carry it.
    #[test]
    fn presiding_concepts_cover_seed_42s_rostered_concepts() {
        let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
        let phenomena =
            hornvale_worldgen::observed_phenomena(view.world(), 0.0).expect("phenomena");
        let mut checked = false;
        for p in &phenomena {
            if let Some(concept) = phenomenon_concept(p) {
                checked = true;
                assert!(
                    PRESIDING_CONCEPTS.contains(&concept),
                    "{concept} is a live presiding gloss concept (phenomenon kind {:?}) but is \
                     missing from PRESIDING_CONCEPTS",
                    p.kind
                );
            }
        }
        assert!(
            checked,
            "seed 42 should carry at least one rostered (glossing) phenomenon"
        );
    }

    /// Every rostered phenomenon in seed 42 names a concept the world can
    /// say. The lab's own derivation over the shared roster (decision 0094)
    /// — it asks the registry and the lexicon, never worldgen's codomain.
    #[test]
    fn every_rostered_referent_is_nameable() {
        let view = FullView::build(Seed(42), &SkyPins::default()).expect("seed 42 builds");
        let lexicon = lex(&view, "goblin").expect("goblin has a lexicon");
        for p in hornvale_worldgen::observed_phenomena(view.world(), 0.0).expect("phenomena") {
            if let Some(nameable) = referent_is_nameable(&p, &view.world().registry, &lexicon) {
                assert!(
                    nameable,
                    "rostered phenomenon {:?} refers to {:?}, which this world cannot name",
                    p.kind, p.referent.concept
                );
            }
        }
    }

    /// Seed 42, pinned. The syllable columns exist to say the campaign's own
    /// claim out loud: both peoples read in (or beside) the 2-3 target, where
    /// the pre-wear tree read 6.04 over the same four seeds' settlements.
    ///
    /// F11 discharge re-measurement (2026-07-30). This is a LIVE pin — it
    /// builds seed 42 rather than reading the census — so no regen was ever
    /// owed for it; it moved because the history bake that landed on `main`
    /// between The Wearing's close and this pass re-decides settlement
    /// placement, and every generated name follows placement. Goblin
    /// 3.031_25 -> 2.466_666_666_666_667, kobold 2.241_379_310_344_827_6 ->
    /// 2.742_574_257_425_743.
    ///
    /// The Witness, Task 5b re-measurement (2026-07-30): kobold moved again,
    /// 2.742_574_257_425_743 (277/101) -> 2.752_475_247_524_752_3 (278/101)
    /// — the same 101-settlement denominator, one more syllable across the
    /// roster. `hydrogeology`'s clastic aquifer threshold moved from a
    /// mismeasured `0.25` to the correctly-measured `0.46`, and `Spring`
    /// stopped being a drainage split and became a geometric descending
    /// contact — both reclassify which cells read `Aquifer`/`Spring`, which
    /// moves settlement placement exactly the way the history-bake landing
    /// did above. Goblin is untouched by this pass.
    ///
    /// The claim above is re-checked, not assumed: both peoples still read
    /// inside the 2-3 target, which is the whole point of the row. They moved
    /// in OPPOSITE directions to get there — goblin down by 0.56, kobold up by
    /// 0.50 — so this is placement reshuffling which sites each people names
    /// and not a drift of the naming machinery in one direction. Goblin was
    /// outside the target on the high side before and is inside it now; kobold
    /// was inside and still is.
    ///
    /// Task 8b (The Witness, same campaign): the phonology-hosting gate in
    /// `draw_rule` reseeds every cascade once more, so kobold moved a third
    /// time, 2.752_475_247_524_752_3 (278/101) -> 2.663_366_336_633_663_5
    /// (269/101) — the same 101-settlement denominator, one fewer syllable
    /// across the roster this time (not monotone: F11 added a syllable, this
    /// removes one, which is expected of a value-level cascade reseed rather
    /// than a directional trend). Goblin is untouched (unaffected by this
    /// change per the golden-fixture diff this same commit re-pins). Both
    /// peoples still read inside the 2-3 target.
    ///
    /// The Contour absorb (2026-08-02): re-measured on the merged tree, which
    /// carries both `defensibility`-gated raid dominance (spec section
    /// 2.3a/2.4, decision 0096 clause 1) and the cascade/v2 reseeds above —
    /// neither branch's prior delta alone predicts the combined result, so
    /// this is a fresh measurement, not an arithmetic combination of the two
    /// histories above it. Both peoples still read inside the 2-3 target.
    #[test]
    fn seed_42_name_syllables_are_pinned() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        // The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
        // lefford, 0063): the BAKE label bump reseats settlements again,
        // moving goblin from 2.724_637_681_159_420_4 to exactly 2.6.
        //
        // The Generalist re-pin (2026-08-03): human joins the coexistence
        // stack as a sixth competitor, redeciding seed 42's settlement
        // placement once more — goblin moves 2.6 -> 67/28
        // (2.392_857_142_857_143).
        assert_eq!(
            extract_from(&built, "name-syllables-goblin"),
            MetricValue::Number(67.0 / 28.0)
        );
        // The Watershed, Item 0: sonority sequencing collapses equal-sonority
        // neighbours inside a template, so kobold falls 2.743 -> 2.683. Goblin
        // is untouched at 2.467 — its drawn templates were already in
        // sonority order, which is the expected shape of this change rather
        // than a surprise: SSP reorders only the templates that violate it.
        //
        // Absorbing The Witness's Task 8b phonology-hosting gate alongside
        // The Watershed's sonority merge reseeds the cascade a further time:
        // 2.663_366_336_633_663_5 (269/101) -> 2.584_158_415_841_584_2
        // (261/101) — same 101-settlement denominator, eight fewer syllables
        // across the roster. Goblin is untouched.
        //
        // The Contour absorb (2026-08-02): 261/101 -> 344/137. Re-measured on
        // the merged tree, which additionally carries `defensibility`-gated
        // raid dominance (spec section 2.3a/2.4, decision 0096 clause 1) — a
        // fresh measurement, not an arithmetic combination of the two
        // histories above it. The denominator move (101 -> 137 settlements)
        // is kobold's own settlement-survival shift under The Contour's
        // re-pin; goblin's count above is untouched by it, consistent with
        // every prior entry in this history.
        //
        // The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
        // lefford, 0063): 344/137 -> 56/23. The BAKE label bump reseats
        // settlements again, moving both the syllable total and the
        // denominator.
        //
        // The Generalist re-pin (2026-08-03): human joins the coexistence
        // stack as a sixth competitor, redeciding seed 42's settlement
        // placement once more — 56/23 -> 254/97, moving both the syllable
        // total and the denominator.
        assert_eq!(
            extract_from(&built, "name-syllables-kobold"),
            MetricValue::Number(254.0 / 97.0)
        );
    }

    /// Seed 42, pinned — and pinned strictly BETWEEN the two degenerate
    /// answers, because both of them would be defects. 1.0 is the
    /// pre-campaign constant the whole campaign exists to break (spec §3's
    /// table: "transparency — today 100%, by construction; after: a
    /// distribution"); 0.0 would mean wear had eaten every name's gloss,
    /// which the survival guard exists to prevent.
    #[test]
    fn seed_42_name_transparency_is_a_distribution_not_a_constant() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "name-transparency");
        let MetricValue::Number(share) = value else {
            panic!("name-transparency reads a number at seed 42: {value:?}");
        };
        assert!(
            share > 0.0 && share < 1.0,
            "transparency is a distribution, not a constant: {share}"
        );
        // F11 discharge re-measurement (2026-07-30): 129/169 -> 202/329. A
        // LIVE pin, so no regen was owed for it — it moved with the history
        // bake that landed on `main` between The Wearing's close and this
        // pass, which re-decides settlement placement. The denominator is the
        // tell: seed 42 fields 329 glossed settlement names now against 169
        // then, which is the same near-doubling of the surviving roster The
        // Tithe recorded (seed 42: 203 -> 329 live settlements).
        //
        // The Witness, Task 5b re-measurement (2026-07-30): 202/329 ->
        // 207/329 — the denominator (329 glossed names) is unchanged, so
        // this is NOT another placement-count shift; five more of the same
        // 329 names now carry a transparent gloss. `hydrogeology`'s clastic
        // aquifer threshold moved from a mismeasured `0.25` to the
        // correctly-measured `0.46`, and `Spring` became a geometric
        // descending contact rather than a drainage split — both
        // reclassify which cells read `Aquifer`/`Spring`/`Aquitard`/`Runoff`,
        // which is exactly the exposure vocabulary transparency's gloss
        // check reads against.
        //
        // The Witness, Task 7 re-measurement (2026-07-30, F7): 207/329 ->
        // 188/329 — the denominator is still unchanged (no placement moved),
        // but the numerator dropped: gating `Tonogenesis` on a prior merger
        // reseeds every cascade, so `evolve`'s output moves for essentially
        // every root, and `namer.wear`'s cascade limb draws different rules
        // too. Some names that used to contain an audible reflex of their
        // glossed concept no longer do under the new draws (and vice versa,
        // net down 19). This is the same wear-audibility surface
        // `speakable_properties.rs` measures; it is expected to move on any
        // cascade-affecting change and is not itself evidence of a defect.
        //
        // The Witness, Task 8b re-measurement (2026-07-30/31): 188/329 ->
        // 149/329 — the denominator is still unchanged (no placement moved),
        // and the numerator dropped again: the phonology-hosting gate in
        // `draw_rule` reseeds every cascade once more (removing the dead
        // Tonogenesis/VowelShift roster slots for every atonal/narrow-vowel
        // species), so `evolve`'s output and `namer.wear`'s cascade limb both
        // move again. Same story as F7: expected on any cascade-affecting
        // change, not itself evidence of a defect.
        //
        // What the row exists to assert is untouched and is re-checked above
        // rather than assumed: transparency is strictly between 0 and 1, so it
        // is still a DISTRIBUTION and neither degenerate answer has crept back.
        // 149 of 329 glossed settlement names.
        //
        // Absorbing The Watershed's sonority merge alongside The Witness's
        // Task 8b gate re-measures once more: 149/329 -> 144/329 — same
        // denominator, five fewer names carry a transparent gloss under the
        // combined reseed. Same story: expected on any cascade-affecting
        // change, not itself evidence of a defect.
        //
        // The Contour absorb (2026-08-02): 144/329 -> 165/324. Re-measured on
        // the merged tree, which additionally carries `defensibility`-gated
        // raid dominance (spec section 2.3a/2.4, decision 0096 clause 1) —
        // a fresh measurement, not an arithmetic combination of the two
        // histories above it (see this test's doc comment). The denominator
        // move (329 -> 324, 5 fewer glossed settlement names) is the same
        // settlement-survival shift The Contour's own re-pin always produces
        // on this seed.
        //
        // The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
        // lefford, 0063): 165/324 -> 93/158. The BAKE label bump reseats
        // settlements again, moving both the glossed-name total and the
        // denominator. Still strictly between 0 and 1, so the distribution
        // claim above holds unweakened.
        //
        // The Generalist re-pin (2026-08-03): human joins the coexistence
        // stack as a sixth competitor, redeciding seed 42's settlement
        // placement once more — 93/158 -> 97/232. Still strictly between 0
        // and 1.
        assert_eq!(share, 97.0 / 232.0, "seed 42 transparency drifted");
    }

    /// The arity regression `name-gloss-true` had, stated as a test so it
    /// cannot come back: a THREE-concept gloss is truthful, and the retired
    /// ordered-pair enumeration called it false. Also pins what the check
    /// still rejects — a concept outside the site vector, and a repeat.
    #[test]
    fn a_three_concept_gloss_is_a_truthful_composition() {
        let site: Vec<String> = ["coast", "river", "temperate-forest", "sun"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        // One, two and three concepts all read as compositions.
        assert!(gloss_is_a_composition_of("coast", &site));
        assert!(gloss_is_a_composition_of("coast-river", &site));
        assert!(gloss_is_a_composition_of(
            "coast-temperate-forest-river",
            &site
        ));
        // A concept the site never offered.
        assert!(!gloss_is_a_composition_of("coast-marsh", &site));
        // A repeat: `glossed_name` picks distinct concepts, and the retired
        // `i != j` pair enumeration rejected this too.
        assert!(!gloss_is_a_composition_of("coast-coast", &site));
        // An empty gloss is not a composition of anything.
        assert!(!gloss_is_a_composition_of("", &site));
    }

    /// The end-to-end statement of the same regression: `name-gloss-true`
    /// reads TRUE on the shipped code at the four seeds The Wearing measures.
    /// It read false on all four before this fix, on glosses that were
    /// themselves perfectly truthful.
    #[test]
    fn seed_42_name_gloss_is_true_under_the_three_concept_shape() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        assert_eq!(
            extract_from(&built, "name-gloss-true"),
            MetricValue::Flag(true)
        );
    }

    /// The null control's synthetic twin, which panicked the first census
    /// regen 934 seconds in. `goblin-twin` peoples every settlement in this
    /// roster and is NOT in the canonical roster `lexicon_from` reconstructs
    /// against, so asking for its lexicon panics inside `resolve_kind`.
    /// `name-transparency` must skip it — and with every settlement skipped
    /// the denominator is zero, so the world reads `Absent`, which is the
    /// right answer: nothing here is measurable.
    ///
    /// The two other new columns must survive the same roster: both are
    /// hardcoded to `goblin`/`kobold`, neither of which this roster holds, so
    /// both read `Absent` rather than panicking on `language_of_in`.
    #[test]
    fn the_null_controls_twin_is_skipped_not_panicked_on() {
        let view = FullView::build_with_components(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_twin_solo_components(),
        )
        .unwrap();
        // Not vacuous: the twin really did people this world's settlements,
        // so the skip below is exercised rather than trivially satisfied.
        let twin_settlements = view
            .world()
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .filter(|f| {
                hornvale_species::species_of(view.world(), f.subject).as_deref()
                    == Some("goblin-twin")
            })
            .count();
        assert!(
            twin_settlements > 0,
            "the twin-solo roster places twin settlements"
        );
        let built = BuiltView::Full(view);
        assert_eq!(
            extract_from(&built, "name-transparency"),
            MetricValue::Absent,
            "every settlement is the twin, so nothing is measurable"
        );
        assert_eq!(
            extract_from(&built, "name-syllables-goblin"),
            MetricValue::Absent
        );
        assert_eq!(
            extract_from(&built, "name-syllables-kobold"),
            MetricValue::Absent
        );
    }

    /// The other half of the null control: the `goblin-solo` roster peoples
    /// its settlements with a species the canonical roster DOES know, so
    /// nothing is excluded and the metric reads a real number. Pins that the
    /// twin guard did not simply switch the metric off for solo rosters.
    #[test]
    fn the_null_controls_goblin_solo_still_reads_a_number() {
        let view = FullView::build_with_components(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_solo_components(),
        )
        .unwrap();
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "name-transparency");
        let MetricValue::Number(share) = value else {
            panic!("goblin-solo is fully measurable: {value:?}");
        };
        assert!(
            (0.0..=1.0).contains(&share),
            "transparency is a share: {share}"
        );
    }

    #[test]
    fn the_wearing_metrics_are_registered() {
        let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
        for want in [
            "name-syllables-goblin",
            "name-syllables-kobold",
            "name-transparency",
        ] {
            assert!(names.contains(&want), "{want} is registered");
        }
    }

    #[test]
    fn the_deep_metrics_are_registered() {
        let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
        for want in [
            "mean-depth-to-basement",
            "unconformity-fraction",
            "mean-geothermal-gradient",
        ] {
            assert!(names.contains(&want), "missing metric {want}");
        }
    }

    #[test]
    fn the_lode_metrics_are_registered() {
        let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
        for want in [
            "cave-fraction",
            "deposit-density",
            "dominant-commodity",
            "mean-ore-grade",
        ] {
            assert!(names.contains(&want), "missing metric {want}");
        }
    }

    #[test]
    fn the_vestige_metrics_are_registered() {
        let names: Vec<&str> = registry().iter().map(|m| m.name).collect();
        for want in [
            "vestige-density",
            "forgotten-fraction",
            "dominant-hazard",
            "mean-warning-legibility",
        ] {
            assert!(names.contains(&want), "missing metric {want}");
        }
    }

    #[test]
    fn sculpting_metrics_are_registered() {
        let list = render_metric_list();
        for name in [
            "shelf-width-passive-median",
            "shelf-width-active-median",
            "sediment-volume",
            "waterfall-count",
            "delta-count",
            "rerouted-flow-fraction",
        ] {
            assert!(list.contains(name), "{name} missing from the metric list");
        }
    }

    #[test]
    fn the_eclipse_seasons_metrics_extract_on_seed_42() {
        let names = [
            "eclipse-year-days",
            "solar-eclipses-per-century",
            "lunar-eclipses-per-century",
            "coincidence-days-per-century",
        ];
        let reg = registry();
        for name in names {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
        let view = AstronomyView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Astronomy(view);
        let m = |name: &str| extract_from(&built, name);
        assert!(matches!(
            m("eclipse-year-days"),
            MetricValue::Number(_) | MetricValue::Absent
        ));
        assert!(matches!(m("solar-eclipses-per-century"), MetricValue::Number(n) if n >= 0.0));
        assert!(matches!(m("lunar-eclipses-per-century"), MetricValue::Number(n) if n >= 0.0));
        assert!(matches!(m("coincidence-days-per-century"), MetricValue::Number(n) if n >= 0.0));
    }

    #[test]
    fn the_long_count_metrics_extract_on_seed_42() {
        let names = ["brightening-per-gyr", "alignment-drift-deg-per-kyr"];
        let reg = registry();
        for name in names {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
        let view = SettlementView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Settlement(view);
        for name in names {
            let value = extract_from(&built, name);
            match value {
                MetricValue::Number(_) | MetricValue::Absent => {}
                _ => panic!("Expected Number or Absent for {name}, got {value:?}"),
            }
        }
    }

    #[test]
    fn phonotactic_validity_holds_for_every_species_name_at_seed_0() {
        // Seed 0 caught a real bug during development: a greedy single-match
        // parser mistook "z" for a false-positive prefix of "zh" (also true
        // of "s"/"sh", "n"/"ng", "k"/"kx") and rejected genuinely valid
        // names. Regression coverage for that fix, independent of the
        // calibration test's full 500-seed study run.
        //
        // Since The Words (Task 9), committed names are glossed compounds
        // of evolved lexicon roots (`Namer::glossed_name`, the `/v2`
        // epoch); sound change only guarantees inventory membership, so
        // `glossed_name` applies deterministic phonotactic repair
        // (epenthesis, then deletion — see
        // `hornvale_language::naming`'s repair formula) to keep every
        // committed name template-conform. This probes the live committed
        // names, exactly as before the epoch bump.
        let view = FullView::build(Seed(0), &SkyPins::default()).unwrap();
        for species in ["goblin", "kobold"] {
            let ph = hornvale_worldgen::language_of(view.world(), species);
            let attested = hornvale_worldgen::lexicon_from(
                view.world(),
                species,
                view.terrain(),
                view.climate(),
            )
            .map(|lex| attested_roman_forms(&lex))
            .unwrap_or_default();
            for n in species_generated_names(&view, species) {
                assert!(
                    is_phonotactically_valid(&n, &ph, &attested),
                    "{species} name {n:?} failed its own phonotactics"
                );
            }
        }
    }

    #[test]
    fn epithet_honorific_is_detected_from_committed_content_at_seed_42() {
        // The metric reads the COMMITTED epithet fact and detects the
        // prepended affix structurally against a re-derived plain word —
        // not the config that drove generation. Since The Words (Task 9)
        // the plain word is the belief's honorific-free glossed epithet,
        // re-derived from the same site concepts worldgen composed (see
        // `epithet_honorific`'s doc). Rank-status species commit
        // honorific-bearing epithets → true; Knowledge-status species
        // commit plain glossed words → false; `epithet_honorific` reads
        // `Absent` when the species has no flagship at all (no beliefs to
        // read epithets from — see its own doc, `flagship_of(...).is_none()`
        // short-circuits first). Under the niche-differentiated-K
        // coexistence-stack cutover (The Niche), only goblin and hobgoblin
        // win a settlement's dominance at seed 42 (bugbear and kobold are
        // present in every settlement's composition but never dominant —
        // see `bugbear_and_kobold_are_present_in_settlement_composition` in
        // `cli/tests/branches_identity.rs`), so `religion::genesis` never
        // fires for kobold this seed: hobgoblin is Rank-status (per
        // `hornvale_species::psyche_registry`) and still places, so it still
        // commits honorific-bearing epithets — this metric is per-species
        // and does not depend on which OTHER Rank-status people (goblin)
        // also places. Re-pinned under The Living Community epoch (history is
        // the sole settlement placer, this merge): the deep-history bake seeds
        // all four peoples, so kobold flagships at seed 42 again, restoring the
        // live Knowledge-status witness The Niche's dominant-only placement had
        // lost. kobold is the roster's only Knowledge-status people, so it
        // commits plain glossed words -> false (the honorific-free branch).
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            epithet_honorific(&view, "hobgoblin"),
            MetricValue::Flag(true),
            "hobgoblin committed epithets must carry the honorific affix"
        );
        assert_eq!(
            epithet_honorific(&view, "kobold"),
            MetricValue::Flag(false),
            "kobold (Knowledge-status) flagships under the epoch and commits \
             plain glossed epithets, the honorific-free 'false' branch"
        );
    }

    /// The mutation test for [`prepended_material`], on the two real
    /// witnesses The Wearing's Task 11b and Task 11c recorded (The
    /// Wearing, Task 11c).
    ///
    /// The property under test is "an honorific syllable stands in front of
    /// the plain word." Breaking it means committing the plain word itself
    /// — which is what a species with `honorifics: false` does, and what a
    /// goblin epithet would become if `glossed_name` stopped prepending the
    /// affix — so the mutation here is to hand the detector the plain word
    /// in the committed slot and require `None`.
    #[test]
    fn prepended_material_detects_the_affix_and_reports_none_without_it() {
        // Seed 0's goblin phonology, which is the language the first
        // witness is a word of; its vowel letters are {a, e, o}. Read from
        // the drawn phonology rather than written down, so the test cannot
        // outlive a change to `romanize`.
        let view = AstronomyView::build(Seed(0), &SkyPins::default()).unwrap();
        let goblin = vowel_graphemes(&hornvale_worldgen::language_of(&view.world, "goblin"));

        // Witness 1 (Task 11b, seed 0, belief 1): the committed epithet and
        // the honorific-free re-derivation of the same belief. The old
        // `ends_with` test failed on this pair — `loeflof` is not a tail of
        // `Teeloflof`, because the stem reduces `lo-` under the affix's
        // stress and `loe-` without it. The frame reading finds it.
        assert_eq!(
            prepended_material("Teeloflof", "loeflof", &goblin).as_deref(),
            Some("tee"),
            "the honorific syllable must be detected in front of the plain word's frame"
        );

        // THE MUTATION: the same belief with the affix never prepended —
        // the committed epithet IS the plain word. The detector must say so.
        assert_eq!(
            prepended_material("loeflof", "loeflof", &goblin),
            None,
            "an epithet committed without its affix must not read as carrying one"
        );

        // Witness 2 (Task 11c, seed 26, bugbear, belief 1): the one pair in
        // 862 where the honorific-bearing form also diverges at the TAIL,
        // because a differently-reduced compound took a different rung of
        // the wear/repair ladder. `Sxaox` + `ddoapzhdap` + `ddoo`. This is
        // the case that rules out `ends_with` on the skeleton as well as on
        // the word; bugbear's seed-26 vowel letters are {a, o}.
        let bugbear_view = AstronomyView::build(Seed(26), &SkyPins::default()).unwrap();
        let bugbear = vowel_graphemes(&hornvale_worldgen::language_of(
            &bugbear_view.world,
            "bugbear",
        ));
        assert_eq!(
            prepended_material("Sxaoxddoapzhdapddoo", "Ddoapzhdap", &bugbear).as_deref(),
            Some("sxaox"),
            "a tail that diverged through repair must not hide the affix at the front"
        );
        assert_eq!(
            prepended_material("Ddoapzhdap", "Ddoapzhdap", &bugbear),
            None,
            "the same word standing alone carries no affix"
        );
    }

    /// The two census worlds [`prepended_material`] cannot see, pinned as
    /// witnesses so the blind spot is defended by a test and not only by
    /// prose (The Wearing, Task 11d).
    ///
    /// Seeds 386 and 976 were the entire `false` population of
    /// `epithet-honorific-goblin` over the 1000-world census WHEN THIS TEST
    /// WAS WRITTEN (Task 11d, against census `46a148a2`). Both are this
    /// function's front-divergence limit rather than a missing affix: the
    /// honorific-free reference surfaced the `gloom` morpheme where the
    /// committed form did not, so the reference holds material the committed
    /// word does not and no offset aligns.
    ///
    /// **Neither seed is in that population any more** (F11 discharge,
    /// 2026-07-30, census `4cd19ff9`): every belief of both worlds now detects
    /// its affix unaided, because the committed form and the honorific-free
    /// reference have landed back on the same rung of the wear/repair ladder
    /// at both seeds. The census's sole `false` is seed 400, diagnosed at
    /// `calibration.rs::HONORIFIC_DETECTOR_BLIND_SEEDS`.
    ///
    /// This test is nonetheless kept, unchanged and passing, and the
    /// distinction matters: it operates on LITERAL word pairs, so it is a
    /// characterisation of `prepended_material`'s alignment limit and not a
    /// claim about which census worlds exhibit it. The limit is real whether
    /// or not any current seed happens to hit it, and the two literal pairs
    /// are the clearest witnesses of it on record. What was corrected here is
    /// only the sentence that read those literals as a live census fact — the
    /// defect class this campaign is named for, found in the campaign's own
    /// prose.
    ///
    /// Each seed is asserted in BOTH directions — `None` against the
    /// reference the metric actually uses, and the affix recovered against
    /// the reference with that extra morpheme removed. The second half is
    /// what makes this a diagnosis rather than a restatement of the
    /// symptom: it shows the affix is right there, and names the exact
    /// material whose presence in the reference hides it. If a future
    /// change puts both forms on the same rung of the wear/repair ladder,
    /// the first assertion of each pair fails and sends the reader here.
    #[test]
    fn the_two_census_falses_are_a_front_divergence_and_not_a_missing_affix() {
        // Seed 386, goblin, belief 5 (gloss "gloom-day"). The world's own
        // honorific-free surface for `gloom` alone is `Foaf` — beliefs 4
        // and 6 of this same world — and that is precisely the material
        // standing in front of the shared tail `moffof` in the reference.
        let v386 = AstronomyView::build(Seed(386), &SkyPins::default()).unwrap();
        let g386 = vowel_graphemes(&hornvale_worldgen::language_of(&v386.world, "goblin"));
        assert_eq!(
            prepended_material("Zfaawmoffof", "Foafmoffof", &g386),
            None,
            "seed 386's reference carries a morpheme the committed form dropped, so nothing aligns"
        );
        assert_eq!(
            prepended_material("Zfaawmoffof", "moffof", &g386).as_deref(),
            Some("zfaaw"),
            "with that morpheme gone from the reference, the affix is exactly where it should be"
        );

        // Seed 976, goblin, belief 16 (gloss "gloom-day"). Same shape: the
        // world's honorific-free surface for `gloom` alone is `Boozh`
        // (beliefs 12-15), and the recovered affix `va` is one this
        // function already detects unaided on belief 10 of the same world.
        let v976 = AstronomyView::build(Seed(976), &SkyPins::default()).unwrap();
        let g976 = vowel_graphemes(&hornvale_worldgen::language_of(&v976.world, "goblin"));
        assert_eq!(
            prepended_material("Vabozhbzas", "Boozhbozhbzas", &g976),
            None,
            "seed 976's reference carries a morpheme the committed form dropped, so nothing aligns"
        );
        assert_eq!(
            prepended_material("Vabozhbzas", "bozhbzas", &g976).as_deref(),
            Some("va"),
            "with that morpheme gone from the reference, the affix is exactly where it should be"
        );
    }

    /// The mutation test for [`exposure_sound`] (The Wearing, Task 11c).
    ///
    /// The property under test is "no lexicon `Root` stands at a concept
    /// the INDEPENDENT exposure reading does not steep." Seed 7's goblins
    /// root five of the seven toponymic concepts Task 4 added — `river`,
    /// `ford`, `hill`, `marsh`, `spring` — so this seed exercises the
    /// elevation gates and the karst gate, not just the river one every
    /// seed hits.
    ///
    /// Breaking the property means removing those gates from the
    /// independent reading, which is precisely the stale state this task
    /// repaired: before Task 11c the lab's duplicate knew none of them and
    /// `exposure-sound-{goblin,kobold}` read false on 252 of 1000 census
    /// worlds. The stripped set below reconstructs that state exactly, and
    /// the flag must flip.
    ///
    /// # Re-enabled (The Witness, Task 3 Step 4b, 2026-07-30)
    ///
    /// This test was `#[ignore]`d (F11 discharge, 2026-07-30) for two
    /// reasons, and the doc comment at that ignore said it plainly: "It
    /// comes back with the staple repair, and both halves must be
    /// re-derived then." The Witness's Task 2 *is* that staple repair
    /// (`independently_steeped_concepts` learned The Watershed's six
    /// staples), so both halves are re-derived here:
    ///
    /// - The small one, unrelated to the staple repair: seed 7's goblins now
    ///   root `river`, `ford`, `hill`, `marsh`, `spring` — `marsh` where
    ///   `valley` used to be — because `main`'s history bake re-decides
    ///   settlement placement between F11's discharge and this task. Still
    ///   five of the seven toponymic concepts, still the elevation and karst
    ///   gates and not just the river one, so the seed is as good a witness
    ///   as it was. The `rooted` assertion below is updated to match.
    /// - The blocking one: with the staple repair in place,
    ///   `exposure_sound(&view, "goblin")` reads `Flag(true)` again (seed 7's
    ///   goblins no longer read unsound on `millet`, `rice`, `vine`), so the
    ///   pre-mutation baseline this test asserts is true again, and the test
    ///   mutates for real: `Flag(true)` before stripping the toponymic gates,
    ///   `Flag(false)` after. Verified by re-running this test standalone
    ///   after the re-derivation (both assertions passed in the same run,
    ///   which is the flip firing, not merely typechecking).
    #[test]
    fn exposure_sound_reports_false_when_the_toponymic_gates_are_removed() {
        const TOPONYMIC: [&str; 7] = [
            "river", "ford", "hill", "valley", "marsh", "spring", "island",
        ];
        let view = FullView::build(Seed(7), &SkyPins::default()).unwrap();
        let steeped = independently_steeped_concepts(&view, "goblin")
            .expect("goblin is in the default roster");
        let lexicon = lex(&view, "goblin").expect("seed 7 goblins hold a lexicon");

        // The precondition, asserted rather than assumed: this seed must
        // actually root toponymic concepts, or the mutation below would
        // pass for the wrong reason (an unbroken flag on a world with
        // nothing to break).
        //
        // The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
        // lefford, 0063): the BAKE label bump reseats settlements, so
        // seed 7's goblins now root only three of the five ("hill" and
        // "marsh" drop out). The test still bites — three rooted concepts is
        // still a nonempty precondition — so the set is re-pinned rather
        // than the seed swapped.
        let rooted: Vec<&str> = TOPONYMIC
            .iter()
            .copied()
            .filter(|c| matches!(lexicon.entry(c), Some(LexEntry::Root { .. })))
            .collect();
        assert_eq!(
            rooted,
            vec!["river", "ford", "spring"],
            "seed 7 goblins must root these toponymic concepts for this test to bite"
        );
        for concept in &rooted {
            assert!(
                steeped.contains(*concept),
                "the independent reading must steep {concept}, which the lexicon rooted"
            );
        }

        assert_eq!(
            exposure_sound(&view, "goblin"),
            MetricValue::Flag(true),
            "seed 7 goblins are exposure-sound"
        );

        // THE MUTATION: the pre-Task-11c reading, which knew no toponymic
        // gate at all.
        let mut stale = steeped.clone();
        for concept in TOPONYMIC {
            stale.remove(concept);
        }
        assert_eq!(
            exposure_sound_against(&view, "goblin", &stale),
            MetricValue::Flag(false),
            "stripping the toponymic gates must flip the flag — a soundness check \
             that cannot report false is worse than one that reports it wrongly"
        );
    }

    /// The independent toponymic reading agrees with the lexicon it is
    /// checking on the two rarest gates, `island` and `hill` (The Wearing,
    /// Task 11c), so the flood-fill and the elevation-maximum gates both get a
    /// live witness rather than resting on the census alone.
    ///
    /// F11 discharge re-derivation (2026-07-30). The witness was seed 1's
    /// kobolds and is now **seed 0's gnolls**. This is the seed change F11
    /// flagged as a judgement rather than a re-pin, and the judgement is
    /// recorded here so it is not mistaken for a number that was swapped to
    /// make a red test green.
    ///
    /// What happened: `main`'s history bake re-decides settlement placement,
    /// and seed 1's kobolds no longer root `island`. They still root `hill`
    /// (and `river`, `ford`, `valley`, `marsh`, `spring`), so only the
    /// flood-fill half of the witness was lost. The test's own precondition —
    /// "seed 1 kobolds must root island for this test to bite" — is what
    /// caught it, working exactly as designed: it refused to pass on a world
    /// that no longer exhibited the property, instead of quietly asserting
    /// nothing.
    ///
    /// Why seed 0 / gnoll: a sweep of seeds 0..60 over all five placed peoples
    /// was measured, and `island` is rooted somewhere on 30 of those seeds —
    /// so the gate is emphatically live, and the loss at seed 1 is a
    /// population that moved, not a rule that died. Seed 0's gnolls are the
    /// EARLIEST world in that sweep rooting BOTH `island` and `hill`, which is
    /// the pair this witness needs; taking the earliest rather than a
    /// hand-picked one keeps the choice reproducible and free of selection.
    /// The species changed only because the seed did — nothing in the claim is
    /// about kobolds, and the gates being witnessed are terrain gates.
    #[test]
    fn the_independent_reading_steeps_island_and_hill_where_the_lexicon_roots_them() {
        let view = FullView::build(Seed(0), &SkyPins::default()).unwrap();
        let steeped =
            independently_steeped_concepts(&view, "gnoll").expect("gnoll is in the default roster");
        let lexicon = lex(&view, "gnoll").expect("seed 0 gnolls hold a lexicon");
        for concept in ["island", "hill"] {
            assert!(
                matches!(lexicon.entry(concept), Some(LexEntry::Root { .. })),
                "seed 0 gnolls must root {concept} for this test to bite"
            );
            assert!(
                steeped.contains(concept),
                "the independent reading must steep {concept} too"
            );
        }
        // This test used to close with a whole-lexicon
        // `exposure_sound(&view, ..) == Flag(true)`. That line is REMOVED, not
        // relaxed, and the reason is worth writing down: it asserted a
        // different and much broader claim than the one this test is named
        // for, and that broader claim is currently false for a diagnosed
        // reason — `independently_steeped_concepts` has not learned The
        // Watershed's staple Steeped rules, so whole-lexicon soundness reads
        // false on every world where a people is placed. The full diagnosis
        // and the decision to defer its repair live at
        // `calibration.rs::lexicon_is_exposure_sound_for_both_species`, which
        // is the row that owns that claim and carries the ignore for it.
        //
        // Keeping the line would have forced this test to be ignored too, and
        // with it the island/hill agreement it exists to witness — which does
        // hold, and is asserted above. One blocked claim should not take a
        // sound one down with it.
    }

    #[test]
    fn phonotactic_validator_rejects_garbage_and_empty_strings() {
        let view = AstronomyView::build(Seed(0), &SkyPins::default()).unwrap();
        let ph = hornvale_worldgen::language_of(&view.world, "goblin");
        assert!(!is_phonotactically_valid("", &ph, &[]));
        // "qw" (uvular stop q + labial approximant w): q is never a Stop
        // candidate in this drawn inventory (only p/t/d/g appear, per the
        // seed-0 debug dump), so this must not parse.
        assert!(!is_phonotactically_valid("qw", &ph, &[]));
    }

    #[test]
    fn attested_roman_words_validate_where_canon_rejects_them() {
        // A name that is exactly an attested word must validate even when
        // no canon template hosts it; a name that is neither canon-parseable
        // nor attested must still fail. "qw" is this fixture's known-
        // unparseable string (see the test above: q is never a Stop
        // candidate in this drawn inventory), so it doubles as the bare
        // sequence the fixture's templates cannot parse — attest it
        // directly rather than inventing a new one.
        let view = AstronomyView::build(Seed(0), &SkyPins::default()).unwrap();
        let ph = hornvale_worldgen::language_of(&view.world, "goblin");
        let attested = vec!["qw".to_string()];
        assert!(is_phonotactically_valid("Qw", &ph, &attested));
        assert!(!is_phonotactically_valid("Qw", &ph, &[]));
        assert!(!is_phonotactically_valid("qq", &ph, &attested));
    }

    #[test]
    fn land_metrics_extract_for_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        assert!(matches!(m("plate-count"), MetricValue::Text(_)));
        assert!(matches!(m("ocean-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
        assert!(
            matches!(m("habitable-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
        assert!(matches!(m("band-count"), MetricValue::Text(_)));
        assert!(matches!(m("dominant-land-biome"), MetricValue::Text(_)));
        assert!(matches!(
            m("mean-land-temperature-c"),
            MetricValue::Number(_) | MetricValue::Absent
        ));
    }

    #[test]
    fn per_cell_diversity_is_finite_and_bounded_by_species_count_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let n_species = view.components().biosphere.len() as f64;
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        match m("per-cell-diversity") {
            MetricValue::Number(v) => {
                assert!(v.is_finite(), "per-cell-diversity must be finite, got {v}");
                // `strife` is 0.0 (not >= 1.0) at a habitable-but-unclaimed
                // cell — byproducts.rs: "a cell with zero total density...
                // reports 0.0 rather than dividing by zero... there is no
                // contest where nobody is contesting". Averaged over EVERY
                // habitable cell (this metric's definition), the mean
                // therefore ranges over [0, N_species], not [1, N_species]:
                // measured directly (debug instrumentation, since removed)
                // at seed 42 with today's pre-calibration BETA it reads
                // ~0.75 — every occupied cell is currently winner-take-all
                // (strife == 1.0 exactly, nowhere higher) and ~25% of
                // habitable land is claimed by no roster species at all.
                // That floor-diversity reading is exactly the signal task
                // A16b's β calibration exists to move.
                assert!(
                    (0.0..=n_species).contains(&v),
                    "per-cell-diversity {v} must lie in [0.0, {n_species}]"
                );
            }
            other => panic!("expected a Number, got {other:?}"),
        }
    }

    #[test]
    fn capacity_metric_skips_fauna_kinds_at_seed_42() {
        // Regression (2026-07-16): the menagerie put fauna kinds (no psyche
        // component) into the roster, and this metric's roster loop assumed
        // a peopled component unguarded — panicking at census
        // scale only, because its calibration battery is heavy-tier and
        // the commit gate never evaluated the metric. This test keeps the
        // evaluation in the commit gate.
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert!(
            view.components()
                .biosphere
                .ids()
                .any(|k| !view.components().psyche.contains(k)),
            "premise: the default roster carries at least one fauna kind"
        );
        let built = BuiltView::Full(view);
        match extract_from(&built, "capacity-by-abs-latitude") {
            MetricValue::Number(v) => {
                assert!(
                    v.is_finite() && v > 0.0,
                    "capacity ratio finite and positive: {v}"
                )
            }
            other => panic!("expected a Number, got {other:?}"),
        }
    }

    #[test]
    fn composition_varies_across_settlements_at_seed_42() {
        // The Niche's headline: refutes the task-C "oatmeal" (identical
        // composition in all 276 settlements). Composition now VARIES and
        // strife is spatially structured. (Per Nathan's E2 call: 2-way
        // differentiation is sufficient — NOT asserting 4 strongholds or
        // refugia>0; the menagerie supplies those.)
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);

        // composition-variance > 0 (the metric)
        match m("composition-variance") {
            MetricValue::Number(cv) => {
                assert!(cv > 0.0, "composition varies across settlements: {cv}")
            }
            other => panic!("composition-variance should be a Number, got {other:?}"),
        }

        // More than one species dominates somewhere, and strife is
        // non-flat. Build the seed-42 report directly for these structural
        // asserts (the built view above no longer exposes its fields).
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let report = hornvale_worldgen::demography_report_from(
            view.world(),
            view.components(),
            view.settlement.terrain(),
            view.settlement.climate(),
        )
        .unwrap();
        let dominants: std::collections::BTreeSet<u32> = report
            .stack_settlements
            .iter()
            .map(|s| s.dominant)
            .collect();
        assert!(
            dominants.len() >= 2,
            "more than one species dominates: {dominants:?}"
        );

        let geo = view.settlement.terrain().geosphere();
        let mut strife: Vec<f64> = geo
            .cells()
            .map(|c| *report.byproducts.strife.get(c))
            .filter(|x| *x > 0.0)
            .collect();
        assert!(strife.len() > 10, "enough cells have strife");
        strife.sort_by(f64::total_cmp);
        let (lo, hi) = (strife.first().unwrap(), strife.last().unwrap());
        assert!(
            hi - lo > 1e-3,
            "strife is spatially structured, not flat: lo={lo} hi={hi}"
        );
    }

    #[test]
    fn ground_metrics_extract_for_seed_42() {
        // The Ground (Task 7): rock/soil/hydrogeology census metrics, over
        // land cells only; a landed seed like 42 must name a rock and a
        // soil order and report every fraction inside [0, 1].
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        match m("dominant-rock") {
            MetricValue::Text(name) => assert!(!name.is_empty()),
            other => panic!("dominant-rock: {other:?}"),
        }
        match m("dominant-soil-order") {
            MetricValue::Text(name) => assert!(!name.is_empty()),
            other => panic!("dominant-soil-order: {other:?}"),
        }
        assert!(matches!(m("karst-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
        assert!(
            matches!(m("aquifer-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
        assert!(
            matches!(m("fertile-land-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
    }

    #[test]
    fn the_lode_metrics_extract_for_seed_42() {
        // The Lode (Task 7): cave/deposit census metrics, over land cells
        // only; fractions and grade must land in [0, 1], and the dominant
        // commodity is either a named commodity or Absent (no land deposit).
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        assert!(matches!(m("cave-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
        assert!(matches!(m("deposit-density"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
        match m("dominant-commodity") {
            MetricValue::Text(name) => assert!(!name.is_empty()),
            MetricValue::Absent => {}
            other => panic!("dominant-commodity: {other:?}"),
        }
        assert!(matches!(m("mean-ore-grade"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
    }

    #[test]
    fn the_vestige_metrics_extract_for_seed_42() {
        // The Vestige (Task 7): subsurface historical-residue census
        // metrics, over land cells only; fractions must land in [0, 1], and
        // the dominant hazard is either a named hazard or Absent (no land
        // cell bears a vestige).
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        assert!(matches!(m("vestige-density"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
        assert!(
            matches!(m("forgotten-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
        match m("dominant-hazard") {
            MetricValue::Text(name) => assert!(!name.is_empty()),
            MetricValue::Absent => {}
            other => panic!("dominant-hazard: {other:?}"),
        }
        assert!(
            matches!(m("mean-warning-legibility"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
    }

    #[test]
    fn census_of_peoples_metrics_extract_for_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        assert!(matches!(m("settlement-count"), MetricValue::Number(n) if n > 0.0));
        assert!(matches!(m("mean-population"), MetricValue::Number(n) if n > 0.0));
        // The four `flagship-*` metrics are documented as specifically the
        // GOBLIN flagship's data (see their own doc comments above). Since
        // the founder floor (settlement's founder-reservation pass, MAP-22
        // K=1), goblin places its own flagship again at seed 42 —
        // farming, a 3-caste structure (farmer, shaman, chief). Sculpting
        // Task 3 (belt anatomy, discrete arcs, the trench) had redrawn the
        // flagship coastal and tropical-rainforest; Task 5 (induration-
        // scaled fBm relief) redrew every world's elevation again and
        // reverted the flagship to non-coastal, temperate-forest. Tuning
        // iteration 3 (Task 14, RELIEF_FREQUENCY 48→8: the sub-Nyquist fix)
        // redraws elevation once more and moves the flagship BACK to
        // coastal, tropical-rainforest — but that was Sculpting-alone. The
        // niche-differentiated-K coexistence-stack cutover (The Niche,
        // merged here at Sculpting's close) repacked settlement genesis
        // onto a competitive per-species K, relocating which cell goblin's
        // flagship wins world-wide on the composed tree: re-derived
        // empirically post-merge (not carried from either parent), the
        // flagship lands back on non-coastal, temperate-forest — matching
        // neither parent's solo-tree finding on its own, since both
        // campaigns' world-byte changes compose (see `almanac`'s seed-42
        // output and `cli/tests/branches_identity.rs`).
        //
        // The Tithe's adaptive demand (spec §4.3) moved it to a **coastal
        // tropical-rainforest** cell. Nothing about biomes changed: a patron
        // that corrects its demand each epoch collects a different amount, so
        // its subordinates grow and fail on a different schedule, and seed
        // 42's occupation history — which cells are held when the bake closes
        // — is redrawn. Which cell goblin's flagship wins follows the history,
        // as it has followed every world-byte change before it. **The Tithe's
        // bleed (task 5b, spec §4.2b) then moved it back**: letting a greedy
        // patron take from the standing stock and not only from the epoch's
        // surplus holds every vassal near `FARM_FLOOR`, which throws off far
        // fewer daughters, so seed 42 closes with 97 live records instead of
        // 292 and a different cell wins. Re-derived empirically at each step,
        // never carried.
        //
        // **The Tithe's vassal agency (task 5f, spec §4.3d) moves it to a
        // coastal tropical-rainforest cell.** Eight vassals on seed 42 walk
        // away from patrons whose demand they could not regrow, and a
        // departure both frees a cell and re-seats a people elsewhere — so the
        // `DAUGHTER_PROB` draw sequence downstream of the first flight shifts
        // and the whole occupation history is redrawn, exactly as every
        // world-byte change in this list has redrawn it. Nothing about biomes
        // changed.
        //
        // In parallel, The Vacancy T9 added a fifth competing Settled people
        // (the gnoll), which shifts the world-wide competitive landscape
        // settlement genesis resolves — and which, on its own tree, moved
        // goblin's flagship to COASTAL tropical-rainforest as well, since a
        // new competitor claiming the interior cell the old flagship held
        // pushed goblin's own flagship elsewhere. **The two campaigns
        // arrived at the same cell reading by different routes**, and the
        // composed tree is re-derived empirically here rather than carried
        // from either parent — as it has been at every entry in this list.
        assert_eq!(
            m("flagship-subsistence"),
            MetricValue::Text("farming".to_string())
        );
        assert_eq!(
            m("flagship-biome"),
            MetricValue::Text("tropical-rainforest".to_string())
        );
        assert_eq!(m("flagship-coastal"), MetricValue::Flag(true));
        assert_eq!(m("flagship-structure-size"), MetricValue::Number(3.0));
        assert!(
            matches!(m("endorheic-coverage"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
    }

    #[test]
    fn per_species_metrics_have_the_expected_kinds_for_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let m = |name: &str| extract_from(&built, name);
        for species in ["goblin", "kobold"] {
            assert!(matches!(
                m(&format!("{species}-flagship-roles")),
                MetricValue::Text(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("{species}-flagship-population")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("{species}-flagship-surplus")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("{species}-flagship-coastal")),
                MetricValue::Flag(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("{species}-settlement-count")),
                MetricValue::Number(_)
            ));
        }

        // BIO-2 (Task 6, review fix): the six life-history metrics,
        // following the `tone-count-{species}` per-species PAIR convention
        // (goblin + kobold both registered) so the campaign's headline
        // cross-species claim (ectotherm kobold vs endotherm goblinoids) is
        // queryable. Both species are always on the default roster and
        // neither is `Ametabolic` (goblin is Endotherm, kobold is
        // Ectotherm), so these read `Number` at seed 42, but `Absent` stays
        // a legal kind for a roster where a species is missing or
        // ametabolic.
        let names: std::collections::BTreeSet<&str> =
            registry().iter().map(|metric| metric.name).collect();
        assert!(names.contains("lifespan-years-goblin"));
        assert!(names.contains("lifespan-years-kobold"));
        assert!(names.contains("age-at-maturity-years-goblin"));
        assert!(names.contains("age-at-maturity-years-kobold"));
        assert!(names.contains("basal-metabolic-rate-w-goblin"));
        assert!(names.contains("basal-metabolic-rate-w-kobold"));
        assert!(names.contains("reproductive-tempo-goblin"));
        assert!(names.contains("reproductive-tempo-kobold"));
        assert!(names.contains("generation-length-years-goblin"));
        assert!(names.contains("generation-length-years-kobold"));
        assert!(names.contains("pace-of-life-goblin"));
        assert!(names.contains("pace-of-life-kobold"));
        for species in ["goblin", "kobold"] {
            assert!(matches!(
                m(&format!("lifespan-years-{species}")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("age-at-maturity-years-{species}")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("basal-metabolic-rate-w-{species}")),
                MetricValue::Number(_)
            ));
            assert!(matches!(
                m(&format!("reproductive-tempo-{species}")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("generation-length-years-{species}")),
                MetricValue::Number(_) | MetricValue::Absent
            ));
            assert!(matches!(
                m(&format!("pace-of-life-{species}")),
                MetricValue::Number(_)
            ));
        }
    }

    #[test]
    fn locked_world_band_count_metric_is_locked() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = FullView::build(Seed(42), &pins).unwrap();
        let built = BuiltView::Full(view);
        let bc = extract_from(&built, "band-count");
        assert_eq!(bc, MetricValue::Text("locked".to_string()));
    }

    #[test]
    fn registry_has_unique_names() {
        let metrics = registry();
        let mut names: Vec<&str> = metrics.iter().map(|m| m.name).collect();
        let original_len = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), original_len, "Found duplicate metric names");
    }

    #[test]
    fn render_metric_list_contains_all_names() {
        let table = render_metric_list();
        let metrics = registry();
        for m in metrics {
            assert!(table.contains(m.name), "Missing {}", m.name);
        }
    }

    #[test]
    fn render_metric_list_contains_metric_docs() {
        let table = render_metric_list();
        let metrics = registry();
        let moons_admitted = metrics.iter().find(|m| m.name == "moons-admitted").unwrap();
        assert!(
            table.contains(moons_admitted.doc),
            "Missing doc for moons-admitted: {}",
            moons_admitted.doc
        );
        let belief_kind = metrics
            .iter()
            .find(|m| m.name == "belief-kind-goblin")
            .unwrap();
        assert!(
            table.contains(belief_kind.doc),
            "Missing doc for belief-kind-goblin: {}",
            belief_kind.doc
        );
    }

    #[test]
    fn solo_goblin_and_twin_share_head_domain_at_seed_42() {
        // Superseded under The Living Community epoch (this merge): the draft
        // demography attractor placed by vectors alone, so identical-vector
        // peoples with no competitor landed in the SAME cell (the old spec-§3
        // assertion). History is the sole settlement placer now, and its
        // genesis seeds each people's site from a per-people (identity-labelled)
        // draw stream so co-placed peoples don't collide — which necessarily
        // makes a solo world's placement depend on the people's identity too.
        // Measured: solo goblin and goblin-twin now land in DIFFERENT cells
        // (28487 vs 17567). The same-cell claim is retired; what survives, and
        // is asserted below, is that identical vectors still yield the SAME
        // head-deity domain (the religion cascade is vector-pure) while the
        // independent name stream still yields DIFFERENT names.
        let g = FullView::build_with_components(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_solo_components(),
        )
        .unwrap();
        let t = FullView::build_with_components(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_twin_solo_components(),
        )
        .unwrap();
        let gf = flagship_of(g.world(), "goblin").unwrap();
        let tf = flagship_of(t.world(), "goblin-twin").unwrap();
        // Identical vectors ⇒ same head-deity domain (the religion cascade is
        // a pure function of the vectors, independent of the identity-seeded
        // placement cell).
        let reg = registry();
        let dom = |built: &BuiltView, name: &str| match reg
            .iter()
            .find(|m| m.name == name)
            .unwrap()
            .extract
            .apply(built)
        {
            MetricValue::Text(s) => s,
            other => panic!("expected domain text, got {other:?}"),
        };
        let g_built = BuiltView::Full(g);
        let t_built = BuiltView::Full(t);
        assert_eq!(
            dom(&g_built, "head-deity-domain-goblin"),
            dom(&t_built, "head-deity-domain-goblin-twin")
        );
        // But names differ (independent stream).
        assert_ne!(gf.name, tf.name, "twin names must differ from goblin's");
    }

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn shape_metrics_are_present_deterministic_and_sane() {
        let names = [
            "shoreline-development",
            "coast-roughness-slope",
            "hypsometric-bimodality",
            "shelf-fraction",
            "continent-count",
            "largest-continent-share",
            "plate-size-gini",
            "landmass-count",
            "shelf-width-passive-median",
            "shelf-width-active-median",
            "sediment-volume",
            "waterfall-count",
            "delta-count",
            "rerouted-flow-fraction",
        ];
        let registry = registry();
        let a =
            BuiltView::Terrain(TerrainView::build(Seed(7), &SkyPins::default()).expect("seed 7"));
        let b = BuiltView::Terrain(
            TerrainView::build(Seed(7), &SkyPins::default()).expect("seed 7 again"),
        );
        for name in names {
            let metric = registry
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("metric {name} not registered"));
            let va = metric.extract.apply(&a);
            assert_eq!(va, metric.extract.apply(&b), "{name} not deterministic");
            if let MetricValue::Number(x) = va {
                assert!(x.is_finite(), "{name} not finite: {x}");
            }
        }
    }

    #[test]
    fn seed_42_coast_roughness_slope_is_finite() {
        let view = TerrainView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Terrain(view);
        let value = extract_from(&built, "coast-roughness-slope");
        match value {
            MetricValue::Number(x) => assert!(x.is_finite(), "slope not finite: {x}"),
            other => panic!("expected a finite Number, got {other:?}"),
        }
    }

    #[test]
    fn build_with_components_resolves_a_renamed_solo_species() {
        // The twin is a goblin clone re-keyed as `goblin-twin` (NOT in the
        // global registry) — it resolves only through the view's own
        // component set.
        let view = WorldView::build_with_components(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_twin_solo_components(),
        )
        .unwrap();
        let ph = hornvale_worldgen::language_of_in(&view.world, &view.components, "goblin-twin");
        assert!(!ph.inventory.is_empty(), "twin phonology must draw");
        // And it placed a flagship peopled by the twin's name.
        assert!(flagship_of(&view.world, "goblin-twin").is_some());
    }

    // ---- The Branches (Task 10): the family battery. ----

    /// Look up a registered metric by name and extract it — a small
    /// convenience shared by the family-battery tests below, mirroring the
    /// `m` closures the older per-metric tests already build inline. Every
    /// family-battery metric is Full-rung (per the rung map), so this calls
    /// the extractor's `Full` fn pointer directly rather than routing
    /// through a `BuiltView` (the tests hold a borrowed `&FullView`, not an
    /// owned one a `BuiltView` could wrap).
    fn extract(view: &FullView, name: &str) -> MetricValue {
        let reg = registry();
        let metric = reg
            .iter()
            .find(|m| m.name == name)
            .unwrap_or_else(|| panic!("metric {name} not registered"));
        match &metric.extract {
            Extractor::Full(f) => f(view),
            other => panic!("metric {name} is {:?}-rung, not Full", other.rung()),
        }
    }

    #[test]
    fn lexicon_regular_family_holds_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "lexicon-regular-family"),
            MetricValue::Flag(true),
            "every daughter's lexicon must replay regularly at seed 42"
        );
    }

    #[test]
    fn monophyly_goblinoid_holds_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "monophyly-goblinoid"),
            MetricValue::Flag(true),
            "every goblinoid daughter's Root proto must match the family proto-root"
        );
    }

    /// Regression: three seeds where this metric reported a monophyly break
    /// in a world that was monophyletic.
    ///
    /// Seed 42 could never have caught it. The defect needed a *collision* in
    /// `assign_proto_roots`'s reject-and-reprobe loop on an accession-epoch-7
    /// concept (`east`/`west`), which is what makes the extra nine
    /// `Unnameable` concepts in the old unfiltered universe change the
    /// answer — rare enough to hit 14 of 1000 seeds and to miss the one seed
    /// every unit test in this file uses. These three are taken from that
    /// failing set; the full list was `[21, 70, 130, 153, 187, 308, 371, 471,
    /// 502, 571, 836, 847, 849, 855]`.
    #[test]
    fn monophyly_goblinoid_holds_on_the_seeds_the_unfiltered_universe_broke() {
        for seed in [21u64, 70, 130] {
            let view = FullView::build(Seed(seed), &SkyPins::default()).unwrap();
            assert_eq!(
                extract(&view, "monophyly-goblinoid"),
                MetricValue::Flag(true),
                "seed {seed}: the daughters agree with each other; a reported break                  here means the metric's universe has drifted from build_lexicon's again"
            );
        }
    }

    #[test]
    fn clean_outgroup_kobold_holds_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "clean-outgroup-kobold"),
            MetricValue::Flag(true),
            "kobold's proto-roots must never coincide with the goblinoid family's"
        );
    }

    #[test]
    fn inventory_closure_holds_for_every_daughter_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for species in ALL_DAUGHTERS {
            assert_eq!(
                extract(&view, &format!("inventory-closure-{species}")),
                MetricValue::Flag(true),
                "{species}: every Root modern form must draw only its own inventory"
            );
        }
    }

    #[test]
    fn divergence_magnitude_is_a_nonnegative_number_for_every_goblinoid_daughter_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for species in GOBLINOID_DAUGHTERS {
            match extract(&view, &format!("divergence-magnitude-{species}")) {
                MetricValue::Number(n) => assert!(n >= 0.0, "{species}: {n} must be >= 0"),
                other => panic!("{species}: divergence-magnitude not a number: {other:?}"),
            }
        }
    }

    #[test]
    fn divergence_real_holds_at_seed_42() {
        // Seed-42 form of the Task 6 guard (`goblinoid_daughters_actually_diverge`
        // in `windows/worldgen/src/lib.rs`): the family is not aliases.
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "divergence-real"),
            MetricValue::Flag(true),
            "some concept rooted in all three goblinoid daughters must diverge"
        );
    }

    #[test]
    fn classify_homophony_counts_core_confusable_and_splits_draw_from_merger() {
        // Pure classifier, no world. Forms are plain strings; the third tuple
        // field is the concept's semantic domain (Some = core, None = periphery).
        //   noa <- P1 body       (hand)  \ draw-collision (shared P1); 1 core pair,
        //   noa <- P1 universal  (night) / but DIFFERENT domains => FREE (0 confusable)
        //   koo <- P2 body       (hand2) \ merger (P2 != P3); 1 core pair AND
        //   koo <- P3 body       (foot)  / SAME domain => 1 confusable pair
        //   ted <- P4 universal  (green) \ merger; only one core member => 0 core pairs
        //   ted <- P5 None       (color) /
        //   wo  <- P6 kin        (alone)   not a collision
        let entries = [
            ("noa", "P1", Some("body")),
            ("noa", "P1", Some("universal")),
            ("koo", "P2", Some("body")),
            ("koo", "P3", Some("body")),
            ("ted", "P4", Some("universal")),
            ("ted", "P5", None),
            ("wo", "P6", Some("kin")),
        ];
        let s = classify_homophony(&entries);
        assert_eq!(
            s.collision_clusters, 3,
            "noa, koo, ted collide; wo does not"
        );
        assert_eq!(
            s.merger_clusters, 2,
            "koo (P2!=P3) and ted (P4!=P5) are mergers; noa shares P1 (draw)"
        );
        assert_eq!(
            s.core_pairs, 2,
            "noa and koo each contribute one core pair; ted has one core member"
        );
        assert_eq!(
            s.confusable_pairs, 1,
            "only koo's pair is same-domain (body/body); noa's is cross-domain (FREE)"
        );
        assert!(s.confusable_pairs <= s.core_pairs, "confusable ⊆ core");
    }

    #[test]
    fn shipped_daughters_are_atonal_with_tone_count_one() {
        let v = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for daughter in ["goblin", "kobold"] {
            assert_eq!(
                tone_count_metric(&v, daughter),
                MetricValue::Number(1.0),
                "{daughter} must ship atonal (one tone: Neutral)"
            );
        }
    }

    #[test]
    fn a_tone_capable_species_realizes_more_than_one_tone_and_clears_the_capacity_floor() {
        // The test-only serpent roster exercises the tonal path (spec §11): a
        // tone-capable species realizes >1 tone and its capacity meets the
        // floor via pitch, across seeds.
        for seed in [1u64, 7, 42] {
            let v = FullView::build_with_components(
                Seed(seed),
                &SkyPins::default(),
                crate::serpent_tonal_solo_components(),
            )
            .unwrap();
            let tones = match tone_count_metric(&v, "serpent") {
                MetricValue::Number(n) => n,
                other => panic!("tone-count not a number: {other:?}"),
            };
            assert!(
                tones > 1.0,
                "seed {seed}: a tonal species must realize >1 tone"
            );
            let cap = match distinguishable_capacity_metric(&v, "serpent") {
                MetricValue::Number(n) => n,
                other => panic!("capacity not a number: {other:?}"),
            };
            assert!(
                cap >= 24.0,
                "seed {seed}: a tone-capable species must clear the capacity floor (got {cap})"
            );
        }
    }

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn core_homophony_is_zero_for_every_daughter_under_the_merger_aware_assignment() {
        // The root/v3 merger-aware family assignment chooses core proto-roots
        // that survive every daughter's cascade distinct, so core homophony —
        // the number Nathan targets — is zero for every shipped people on every
        // seed (not merely the confusable subset). Absent (no Root minted) is
        // vacuously fine.
        for seed in [1u64, 7, 42, 123, 500] {
            let v = FullView::build(Seed(seed), &SkyPins::default()).unwrap();
            for daughter in ["goblin", "hobgoblin", "bugbear", "kobold"] {
                match extract(&v, &format!("core-homophony-{daughter}")) {
                    MetricValue::Number(n) => assert_eq!(
                        n, 0.0,
                        "seed {seed}: {daughter} core homophony must be zero, got {n}"
                    ),
                    MetricValue::Absent => {}
                    other => panic!("core-homophony-{daughter} not numeric: {other:?}"),
                }
            }
        }
    }

    #[test]
    fn confusable_homophony_never_exceeds_core_homophony_for_every_daughter() {
        // Q3: the confusable (same-domain) count is a subset of core homophony —
        // the honest measurement that lets the atonal tail be accepted.
        let v = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for daughter in ["goblin", "hobgoblin", "bugbear", "kobold"] {
            let core = extract(&v, &format!("core-homophony-{daughter}"));
            let confusable = extract(&v, &format!("confusable-homophony-{daughter}"));
            if let (MetricValue::Number(c), MetricValue::Number(f)) = (&core, &confusable) {
                assert!(
                    f <= c,
                    "{daughter}: confusable {f} must not exceed core {c}"
                );
            }
        }
    }

    #[test]
    fn core_homophony_is_eliminated_at_seed_42_by_the_injective_assignment() {
        // Before the fix, seed 42 goblin rooted hand = many = night = *Noa*
        // (three core concepts → 3 core pairs). The injective family-proto
        // assignment resolves every draw-side core collision, so
        // core-homophony-goblin is 0 here. (Residual cascade/nativize mergers
        // are Stage 3's target and seed-dependent; seed 42's goblin cascade is
        // identity, so none arise.)
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let core = match extract(&view, "core-homophony-goblin") {
            MetricValue::Number(n) => n,
            other => panic!("core-homophony-goblin not a number: {other:?}"),
        };
        assert_eq!(
            core, 0.0,
            "the injective assignment must eliminate seed-42 core homophony; got {core}"
        );
        // Functional-load restriction can only ever be a subset of the raw
        // count, for every daughter.
        for species in ALL_DAUGHTERS {
            let (MetricValue::Number(c), MetricValue::Number(total)) = (
                extract(&view, &format!("core-homophony-{species}")),
                extract(&view, &format!("homophony-count-{species}")),
            ) else {
                continue; // Absent for a daughter with no Root — nothing to bound.
            };
            assert!(
                c <= total,
                "{species}: core-homophony {c} must not exceed homophony-count {total}"
            );
        }
    }

    #[test]
    fn homophony_merger_share_is_a_unit_fraction_or_absent_for_every_daughter() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for species in ALL_DAUGHTERS {
            match extract(&view, &format!("homophony-merger-share-{species}")) {
                MetricValue::Number(f) => {
                    assert!((0.0..=1.0).contains(&f), "{species}: {f} out of [0,1]")
                }
                MetricValue::Absent => {} // no collision → undefined ratio, fine.
                other => panic!("{species}: merger-share unexpected: {other:?}"),
            }
        }
    }

    #[test]
    fn homophony_count_is_a_nonnegative_number_for_every_daughter_at_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        for species in ALL_DAUGHTERS {
            match extract(&view, &format!("homophony-count-{species}")) {
                MetricValue::Number(n) => assert!(n >= 0.0, "{species}: {n} must be >= 0"),
                other => panic!("{species}: homophony-count not a number: {other:?}"),
            }
        }
    }

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn family_battery_metrics_are_deterministic_across_two_builds() {
        let names = [
            "lexicon-regular-family",
            "monophyly-goblinoid",
            "clean-outgroup-kobold",
            "inventory-closure-goblin",
            "inventory-closure-hobgoblin",
            "inventory-closure-bugbear",
            "inventory-closure-kobold",
            "divergence-magnitude-goblin",
            "divergence-magnitude-hobgoblin",
            "divergence-magnitude-bugbear",
            "divergence-real",
            "homophony-count-goblin",
            "homophony-count-hobgoblin",
            "homophony-count-bugbear",
            "homophony-count-kobold",
        ];
        let a = FullView::build(Seed(11), &SkyPins::default()).expect("seed 11");
        let b = FullView::build(Seed(11), &SkyPins::default()).expect("seed 11 again");
        for name in names {
            assert_eq!(
                extract(&a, name),
                extract(&b, name),
                "{name} not deterministic"
            );
        }
    }

    #[test]
    fn divergence_magnitude_and_inventory_closure_use_the_species_own_phonology_not_the_family_proto()
     {
        // NON-VACUITY GUARD: `divergence_magnitude` must count segments
        // against the DAUGHTER's own inventory, not the family proto's —
        // else it would always read 0 (every proto segment trivially "in
        // inventory" against itself). At seed 42 the goblinoid family
        // draws a real proto/daughter phonology split (Task 6/7's
        // family-vs-species stream keying), so at least one daughter must
        // show nonzero divergence, or this metric is measuring nothing.
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let any_nonzero = GOBLINOID_DAUGHTERS.iter().any(|species| {
            matches!(
                extract(&view, &format!("divergence-magnitude-{species}")),
                MetricValue::Number(n) if n > 0.0
            )
        });
        assert!(
            any_nonzero,
            "at least one goblinoid daughter must show nonzero divergence magnitude at seed 42"
        );
    }

    // --- The Chorus (C4, LANG-41): the six dial metrics over
    // `accounts_from`. Seed 1 places goblin (sky_capability 0.5) and
    // hobgoblin (0.55) only — both below the moon-count SkyGraded
    // threshold (0.6), so both lose every sky fact and their sky
    // distortions tie at 1.0. Seed 2 additionally places kobold
    // (sky_capability 1.0), which keeps the moon-count fact — see
    // `windows/worldgen/tests/chorus_params.rs`'s
    // `kobold_keeps_the_moons_goblin_loses` for the disposition ground
    // truth these expectations are read off of. ---

    #[test]
    fn chorus_metrics_are_registered() {
        let reg = registry();
        for name in [
            "chorus-distortion",
            "chorus-distinctiveness",
            "chorus-recoverability",
            "chorus-variance",
            "chorus-param-spread",
            "chorus-sky-calibration",
        ] {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
    }

    #[test]
    fn seed_1_sky_calibration_is_negative_one_across_the_full_roster() {
        // Re-pinned under The Living Community epoch (this merge): the
        // deep-history bake now seeds all four peoples at seed 1, not just
        // goblin/hobgoblin. kobold (sky_capability 1.0) keeps the moons
        // where the others lose them, so strictly-comparable discordant
        // pairs exist and the sky-calibration tau is exactly -1.0 (no longer
        // the old two-people all-tied Absent). The tie-EXCLUSION rule the old
        // Absent illustrated is still covered by `seed_2_sky_calibration_is_
        // exactly_negative_one` (the goblin/hobgoblin tie is excluded there)
        // and by the direct-helper degenerate tests.
        let view = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "chorus-sky-calibration"),
            MetricValue::Number(-1.0),
            "seed 1 (full four-people roster): strict discordant pairs give tau = -1.0"
        );
        for name in [
            "chorus-distortion",
            "chorus-distinctiveness",
            "chorus-recoverability",
            "chorus-variance",
            "chorus-param-spread",
        ] {
            assert!(
                matches!(extract(&view, name), MetricValue::Number(_)),
                "{name} should be present at seed 1 (2 cultures placed)"
            );
        }
    }

    #[test]
    fn seed_1_distinctiveness_is_positive_from_stance_alone() {
        // Even where dispositions tie in magnitude, goblin and hobgoblin's
        // accounts differ in VALUE: goblin reads hobgoblin as Neighbors,
        // hobgoblin reads goblin as Rivals (the stance asymmetry —
        // `hobgoblin_reads_rivals_where_goblin_reads_neighbors` in
        // `chorus_params.rs`). distinctiveness must catch this.
        let view = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        match extract(&view, "chorus-distinctiveness") {
            MetricValue::Number(n) => assert!(n > 0.0, "expected > 0.0, got {n}"),
            other => panic!("expected Number, got {other:?}"),
        }
    }

    #[test]
    fn seed_2_sky_calibration_is_exactly_negative_one() {
        // Kobold (cap 1.0, sky distortion lower) forms a strictly-comparable
        // pair with each of goblin and hobgoblin (cap 0.5/0.55, sky
        // distortion 1.0): higher capability, lower distortion in both
        // pairs — both discordant under the metric's sign convention
        // (concordant means capability and distortion move the SAME way).
        // Goblin/hobgoblin still ties (excluded). Two strict pairs, both
        // discordant: tau = (0 - 2) / 2 = -1.0 exactly.
        let view = FullView::build(Seed(2), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "chorus-sky-calibration"),
            MetricValue::Number(-1.0),
            "expected exact -1.0: both strict pairs (g-k, h-k) discordant"
        );
    }

    #[test]
    fn seed_8_distinctiveness_exceeds_seed_1() {
        // Re-pointed under The Living Community epoch (this merge): the bake
        // now seeds the same four-people roster at every placing seed, so the
        // old "seed 2 adds kobold on top of seed 1" comparison is void —
        // kobold places at both, and distinctiveness is identical across most
        // seeds. What still moves it is the sky regime: at seed 8 the placed
        // peoples' heads split (bugbear "ambient" vs the rest "eternal"),
        // adding head-sentiment divergence on top of the stance asymmetry
        // present at seed 1 (all "cyclic"), so the mean pairwise
        // distinctiveness strictly increases from seed 1 to seed 8.
        let seed1 = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        let seed8 = FullView::build(Seed(8), &SkyPins::default()).unwrap();
        let d1 = match extract(&seed1, "chorus-distinctiveness") {
            MetricValue::Number(n) => n,
            other => panic!("seed 1: expected Number, got {other:?}"),
        };
        let d8 = match extract(&seed8, "chorus-distinctiveness") {
            MetricValue::Number(n) => n,
            other => panic!("seed 8: expected Number, got {other:?}"),
        };
        assert!(
            d8 > d1,
            "seed 8 distinctiveness ({d8}) should exceed seed 1's ({d1})"
        );
    }

    #[test]
    fn chorus_metrics_are_absent_on_empty_and_singleton_voice_lists() {
        // Drive the metric helpers directly with an empty/singleton voice
        // list, rather than hunting for a 0-people seed.
        assert_eq!(chorus_distortion_metric_over(&[]), MetricValue::Absent);
        assert_eq!(chorus_recoverability_metric_over(&[]), MetricValue::Absent);
        assert_eq!(chorus_distinctiveness_metric_over(&[]), MetricValue::Absent);
        assert_eq!(chorus_variance_metric_over(&[]), MetricValue::Absent);
        assert_eq!(chorus_param_spread_metric_over(&[]), MetricValue::Absent);
        assert_eq!(chorus_sky_calibration_metric_over(&[]), MetricValue::Absent);

        let view = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        let one_voice: Vec<ChorusVoice> = chorus_voices(&view).into_iter().take(1).collect();
        assert_eq!(one_voice.len(), 1, "seed 1 must place at least one voice");
        assert!(matches!(
            chorus_distortion_metric_over(&one_voice),
            MetricValue::Number(_)
        ));
        assert!(matches!(
            chorus_recoverability_metric_over(&one_voice),
            MetricValue::Number(_)
        ));
        assert_eq!(
            chorus_distinctiveness_metric_over(&one_voice),
            MetricValue::Absent
        );
        assert_eq!(chorus_variance_metric_over(&one_voice), MetricValue::Absent);
        assert_eq!(
            chorus_param_spread_metric_over(&one_voice),
            MetricValue::Absent
        );
        assert_eq!(
            chorus_sky_calibration_metric_over(&one_voice),
            MetricValue::Absent
        );
    }

    // --- The Contour (Task 4): the measurement instrument. ---

    /// M2/M3/M4 are all registered now (round 3 landed M4 on present-day
    /// terrain, spec §2.4 amendment 4), read the full stack, and carry a
    /// doc string.
    #[test]
    fn the_contour_metrics_are_registered_and_full_rung() {
        let reg = registry();
        for name in [
            "peoples-alive-at-bake-end",
            "largest-holding-share",
            "defensibility-capacity-rank-corr",
        ] {
            let m = reg
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("metric {name} is not registered"));
            assert_eq!(
                m.rung(),
                BuildDepth::Full,
                "{name} must read the full stack"
            );
            assert!(!m.doc.is_empty(), "{name} needs a doc");
        }
    }

    /// Seed 42 places a live roster at bake end, so M2/M3/M4 all extract
    /// real numbers, not `Absent`: at least one people alive; the largest
    /// community's share strictly between 0 (something must hold
    /// population) and 1 (a lone community would be the whole world, which
    /// seed 42's four-people roster does not produce); and M4's rank
    /// correlation in `[-1, 1]` (seed 42 places land varied enough that the
    /// series isn't constant).
    #[test]
    fn the_contour_metrics_extract_sane_values_for_seed_42() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        match extract_from(&built, "peoples-alive-at-bake-end") {
            MetricValue::Number(n) => assert!(n >= 1.0, "expected at least one live people"),
            other => panic!("peoples-alive-at-bake-end: {other:?}"),
        }
        match extract_from(&built, "largest-holding-share") {
            MetricValue::Number(share) => {
                assert!(
                    share > 0.0 && share <= 1.0,
                    "share must be in (0, 1]: {share}"
                );
            }
            other => panic!("largest-holding-share: {other:?}"),
        }
        match extract_from(&built, "defensibility-capacity-rank-corr") {
            MetricValue::Number(rho) => {
                assert!((-1.0..=1.0).contains(&rho), "rho out of range: {rho}");
            }
            other => panic!("defensibility-capacity-rank-corr: {other:?}"),
        }
    }

    // --- The Contour (Task 4, round 3): the Spearman helpers themselves,
    // driven by hand-built inputs whose rank correlation is not in
    // dispute — the counting rule, checked against ground truth stated by
    // hand, mirroring `syllable_count_reads_maximal_vowel_runs`' idiom
    // above. ---

    #[test]
    fn average_ranks_gives_ties_the_mean_of_the_ranks_they_span() {
        // 10, 20, 20, 30 -> tied pair at positions 2-3 (1-based) share 2.5;
        // the untied ends keep their plain rank.
        assert_eq!(
            average_ranks(&[10.0, 20.0, 20.0, 30.0]),
            vec![1.0, 2.5, 2.5, 4.0]
        );
        // A three-way tie at the front: positions 1-3 share (1+2+3)/3 = 2.0.
        assert_eq!(
            average_ranks(&[5.0, 5.0, 5.0, 9.0]),
            vec![2.0, 2.0, 2.0, 4.0]
        );
        // Ranking is by VALUE, not input position: descending input order
        // must still rank ascending by value.
        assert_eq!(average_ranks(&[3.0, 2.0, 1.0]), vec![3.0, 2.0, 1.0]);
        // All tied: every rank is the mean of 1..=n.
        assert_eq!(average_ranks(&[7.0, 7.0, 7.0]), vec![2.0, 2.0, 2.0]);
    }

    #[test]
    fn pearson_correlation_reads_perfect_and_inverse_and_undefined() {
        // Exact equality would be fragile here (the sqrt/division chain
        // lands at 0.9999999999999998, not bitwise 1.0), so these check a
        // tight tolerance instead of `assert_eq!` — the ONE place in this
        // battery that isn't exact, because floating-point summation, not a
        // logic choice, is why.
        let perfect = pearson_correlation(&[1.0, 2.0, 3.0], &[10.0, 20.0, 30.0]).unwrap();
        assert!((perfect - 1.0).abs() < 1.0e-12, "got {perfect}");
        let inverse = pearson_correlation(&[1.0, 2.0, 3.0], &[30.0, 20.0, 10.0]).unwrap();
        assert!((inverse - (-1.0)).abs() < 1.0e-12, "got {inverse}");
        // A constant series has no defined correlation (zero variance), not
        // a zero correlation.
        assert_eq!(
            pearson_correlation(&[1.0, 1.0, 1.0], &[1.0, 2.0, 3.0]),
            None
        );
        assert_eq!(pearson_correlation(&[1.0], &[1.0]), None);
    }

    #[test]
    fn spearman_over_ranks_is_invariant_to_a_positive_rescaling() {
        // The whole reason a rank correlation, rather than a raw Pearson
        // correlation, is the right read for M4: capacity's scale
        // (SETTLERS_PER_CAPACITY, private to history_bake.rs) must not be
        // able to move the answer. Scale one series by an arbitrary
        // positive constant and the ranks -- hence the Spearman value --
        // must be untouched.
        let xs = [3.0, 1.0, 4.0, 1.0, 5.0];
        let ys = [9.0, 2.0, 6.0, 2.0, 1.0];
        let scaled_ys: Vec<f64> = ys.iter().map(|y| y * 100.0).collect();
        let rho_a = pearson_correlation(&average_ranks(&xs), &average_ranks(&ys));
        let rho_b = pearson_correlation(&average_ranks(&xs), &average_ranks(&scaled_ys));
        assert_eq!(rho_a, rho_b);
    }

    /// The Watershed's six staples (`hornvale_climate::Crop::catalog()`,
    /// gated `Steeped` in `exposure_of` only where a settled cell's
    /// subsistence is `Farming`) reach `Steeped` through the crop gate that
    /// `independently_steeped_concepts` never learned — F13, the third
    /// recurrence of the duplicate going stale. Named individually so ADDING
    /// a staple that the lab does not know about reds this test rather than
    /// slipping past it.
    ///
    /// Seed 5's bugbear was the original witness: diagnosed by sweeping
    /// seeds 0..20 and every placed people for which staple concepts
    /// actually reach a `Root` in the committed lexicon (which only happens
    /// when `exposure_of` classified them `Steeped`), seed 5's bugbear was
    /// the only (seed, species) pair in that sweep whose settlements span
    /// all six crop bands at once. No single seed need witness all six for
    /// the campaign's claim to hold — this test only needs one that does, so
    /// the assertion is not vacuous.
    ///
    /// **The Contour re-witness (2026-08-02):** position-aware conflict
    /// (defensibility as a second contest axis) reseats settlements on
    /// nearly every world, and seed 5's bugbear no longer spans all six
    /// bands. Re-diagnosed the same way, widened: swept seeds 0..150 (a
    /// fresh sweep, not a re-pin of the old one — the old witness's range
    /// no longer contains a hit) against every placed people, dynamically
    /// read off `FullView::components().perception` per seed rather than a
    /// hardcoded roster, so a new people entering the roster would still be
    /// swept. Seven (seed, species) pairs in that range clear all six bands:
    /// (42, gnoll), (50, goblin), (83, bugbear), (83, kobold), (90, kobold),
    /// (133, hobgoblin), (148, kobold). Seed 83's bugbear is the new
    /// witness — same species as before, for continuity, and independently
    /// corroborated by seed 83's kobold clearing the same six bands in the
    /// same world.
    const STAPLE_CONCEPTS: [&str; 6] = ["barley", "wheat", "rice", "millet", "tuber", "vine"];

    #[test]
    fn the_independent_reading_covers_every_staple_worldgen_can_steep() {
        // The Contour epoch v2 re-witness (2026-08-02, history/bake/v2 regen
        // on lefford, 0063): the BAKE label bump reseats settlements, and
        // seed 83's bugbear no longer clears all six staple bands at its new
        // site — a witness-seed invalidation, not a code regression. Re-swept
        // seeds 0..150 against every placed people, dynamically read off
        // `FullView::components().perception` (the same method the prior
        // witness search used, not a fresh one); (16, bugbear) clears all six
        // and independently, (16, kobold) clears the same six bands in the
        // same world — the same same-seed-two-species corroboration the
        // previous witness had. Bugbear kept for continuity with the prior
        // witness species.
        let view = FullView::build(Seed(16), &SkyPins::default()).unwrap();
        let steeped =
            independently_steeped_concepts(&view, "bugbear").expect("bugbear is placed at seed 16");
        for staple in STAPLE_CONCEPTS {
            assert!(
                steeped.contains(staple),
                "the lab's independent reading does not steep {staple}, which \
                 worldgen does — the duplicate is stale again"
            );
        }
    }
}
