//! Tier-1 metrics extractors: analyzable properties of generated worlds.

use hornvale_astronomy::{
    Calendar, NeighborClass, Rotation, StarSystem, streams::ROOT as ASTRONOMY_STREAM_ROOT,
};
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, EntityId, Phenomenon, Seed, Value, World};
use hornvale_language::{
    GapReason, LexEntry, Manner, MorphOptions, NameKind, Namer, Phonology, Segment,
    distinctiveness, distortion, domain_distortion, recoverability, romanize,
};
use hornvale_religion::beliefs_of;
use hornvale_terrain::{
    CarveParams, GlobeSummary, Hydro, MarginPolarity, RockClass, SoilOrder, fertility,
};
use hornvale_worldgen::{
    BuildDepth, BuildError, ChorusVoice, Sky, SkyChoice, WorldComponents, accounts_from,
    build_world_from_components, build_world_to, climate_from, climate_of, flagship_of,
    language_of_in, observed_phenomena_as_at_from, observed_phenomena_as_in_from, rock_class_name,
    sky_of, soil_of, soil_order_name, terrain_of,
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
        let climate = climate_of(&world)?;
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
        let world = build_world_to(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
            &wc,
            depth,
        )?;
        let sky = sky_of(&world)?;
        let Sky::Generated(sky) = sky else {
            return Err(BuildError::Pins(
                "expected Generated sky, got Constant".to_string(),
            ));
        };
        Ok(AstronomyView {
            system: sky.system().clone(),
            calendar: sky.calendar().clone(),
            notes: sky.notes().to_vec(),
            world,
            components: wc,
        })
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
        let astronomy = AstronomyView::build_to(seed, pins, wc, depth)?;
        let terrain = terrain_of(&astronomy.world)?;
        let globe = hornvale_terrain::summarize(terrain.globe());
        Ok(TerrainView {
            astronomy,
            globe,
            terrain,
        })
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
    fn build_to(
        seed: Seed,
        pins: &SkyPins,
        wc: WorldComponents,
        depth: BuildDepth,
    ) -> Result<ClimateView, BuildError> {
        let terrain = TerrainView::build_to(seed, pins, wc, depth)?;
        // Reuse the terrain rung just built rather than re-sculpting it inside
        // `climate_of` (The Single Sculpt, applied to the Lab view chain).
        let climate = climate_from(&terrain.astronomy.world, &terrain.terrain)?;
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
                for (_kind, psych) in v.components().psyche.iter() {
                    let inputs = hornvale_kernel::CellMap::from_fn(geo, |c| {
                        hornvale_worldgen::species_carrying_input(*base_inputs.get(c), psych)
                    });
                    let k = hornvale_demography::carrying_capacity(geo, &inputs);
                    for cell in geo.cells() {
                        if v.terrain().is_ocean(cell) {
                            continue;
                        }
                        let lat = geo.coord(cell).latitude.abs();
                        let kv = *k.get(cell);
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
                   `hornvale_worldgen::demography_report`, which reconstructs the IDENTICAL \
                   report the settlement-genesis path builds internally (the shared-assembly \
                   refactor of task A16a), so this measures the stack the world actually \
                   ships, not a parallel one. Absent if the report fails to build or the \
                   world has no habitable cells",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let Ok(report) = hornvale_worldgen::demography_report(v.world(), v.components())
                else {
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
                   via `hornvale_worldgen::demography_report` (the niche-differentiated \
                   coexistence shadow). Absent if the report fails to build or the world \
                   has fewer than 2 settlements",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.005, 0.01, 0.02, 0.05, 0.1],
            },
            extract: Extractor::Settlement(|v: &SettlementView| {
                let Ok(report) = hornvale_worldgen::demography_report(v.world(), v.components())
                else {
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
                   concepts (biome + presiding phenomenon), independently re-derived here \
                   rather than read back from worldgen's own composition; Absent if no \
                   settlement in this world carries a gloss",
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
        // over `accounts_of` — how far each placed culture's epistemic
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
/// the committed epithet was generated with, never the belief id. The
/// honorific affix is one template syllable drawn AFTER the site-concept
/// picks and PREPENDED, so re-deriving the same glossed epithet with
/// honorifics OFF yields exactly the plain word the committed epithet was
/// built from: the committed epithet carries the honorific iff, lowercased,
/// it ends with the lowercased plain word AND is strictly longer.
/// `Flag(true)` iff EVERY committed epithet carries it (goblin, Rank),
/// `Flag(false)` iff none does (kobold, Knowledge). A broken honorific
/// pipeline — a goblin epithet committed without its affix — would equal
/// its plain word here and flip the flag to false, which the preregistered
/// calibration catches.
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
    let namer = Namer::new(&v.world().seed, species, &ph);
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
        let (plain, _) = namer.glossed_name(
            NameKind::Epithet,
            name_seed,
            &MorphOptions { honorifics: false },
            &site,
            &lexicon,
        );
        let plain = plain.roman.to_lowercase();
        let committed = b.epithet.to_lowercase();
        committed.ends_with(&plain) && committed.chars().count() > plain.chars().count()
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

/// The concept a phenomenon kind glosses to (spec §9.3) — mirrors
/// worldgen's own private `phenomenon_concept` and the independent copy in
/// `cli/tests/words_identity.rs`'s `phenomenon_concept`. Deliberately
/// duplicated rather than imported: this is a composition-root judgment
/// call, not a save-format contract, so re-deriving it here from the same
/// public phenomenon-kind constants is what makes `name-gloss-true` a real
/// cross-check rather than an echo of worldgen's own private mapping.
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&'static str> {
    match phenomenon.kind.as_str() {
        hornvale_astronomy::CELESTIAL_BODY => {
            if phenomenon.description.contains("moon") {
                Some("moon")
            } else if phenomenon.description.contains("star") {
                Some("star")
            } else {
                Some("sun")
            }
        }
        hornvale_astronomy::SEASONAL_CYCLE => Some("day"),
        hornvale_astronomy::NIGHT_STAR => Some("star"),
        hornvale_climate::AMBIENT => Some("wind"),
        _ => None,
    }
}

/// This world's `species` lexicon, reusing the view's already-built terrain
/// and climate instead of re-sculpting the globe inside `exposure_of` — the
/// census's dominant cost once the name-gloss sculpts were removed (the
/// terrain pipeline ran twice per `lexicon_of` call, ~14 metrics deep). The
/// Single Sculpt, applied to the lexicon path; byte-identical to
/// `lex(v, species)`.
fn lex(v: &FullView, species: &str) -> Result<hornvale_language::Lexicon, BuildError> {
    hornvale_worldgen::lexicon_from(v.world(), species, v.terrain(), v.climate())
}

/// A settlement's own re-derived site concepts (mirrors
/// `cli/tests/words_identity.rs`'s `settlement_site_concepts`): its
/// committed biome fact plus the presiding phenomenon concept its species
/// observes from THIS settlement's own vantage (its committed coordinates
/// cull the sky — SEQ-5; spec §9.3 defines gloss truthfulness against the
/// entity's own facts), if any. `None` if the settlement is missing a
/// biome/species fact, which `name_gloss_true` below treats as an
/// unverifiable (failing) row rather than skipping it silently.
fn settlement_site_concepts(
    v: &FullView,
    id: EntityId,
    climate: &GeneratedClimate,
) -> Option<Vec<String>> {
    let biome = v
        .world()
        .ledger
        .text_of(id, hornvale_settlement::BIOME)?
        .to_string();
    let species = hornvale_species::species_of(v.world(), id)?;
    let phenomena =
        observed_phenomena_as_at_from(v.world(), v.components(), &species, id, climate).ok()?;
    let mut concepts = vec![biome];
    if let Some(concept) = phenomena.first().and_then(phenomenon_concept) {
        concepts.push(concept.to_string());
    }
    Some(concepts)
}

/// Every gloss composition `Namer::glossed_name` could truthfully produce
/// from `concepts` (mirrors `cli/tests/words_identity.rs`'s
/// `candidate_glosses`): each concept alone, plus every ordered pair joined
/// with `"-"`.
fn candidate_glosses(concepts: &[String]) -> std::collections::BTreeSet<String> {
    let mut set = std::collections::BTreeSet::new();
    for c in concepts {
        set.insert(c.clone());
    }
    for i in 0..concepts.len() {
        for j in 0..concepts.len() {
            if i != j {
                set.insert(format!("{}-{}", concepts[i], concepts[j]));
            }
        }
    }
    set
}

/// Whether every committed settlement `name-gloss` fact in this world is a
/// truthful composition of that SAME settlement's own re-derived site
/// concepts (spec §9.3) — the per-world aggregate of
/// `cli/tests/words_identity.rs`'s `names_wellformed_and_glosses_true`
/// settlement half, re-implemented independently here (never calling
/// worldgen's internal name-drawing code, only its committed `NAME_GLOSS`
/// fact and this module's own re-derivation of the site concepts that fact
/// should be true to). `Absent` if no settlement in this world carries a
/// gloss.
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
            Some(concepts) if candidate_glosses(&concepts).contains(gloss) => {}
            _ => all_true = false,
        }
    }
    if !checked {
        return MetricValue::Absent;
    }
    MetricValue::Flag(all_true)
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
    let cascade = hornvale_language::draw_cascade(&v.world().seed, species);
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

/// The concepts an INDEPENDENT re-derivation of `species`' exposure would
/// classify `Steeped` — duplicating `exposure_of`'s own Steeped rules
/// (`windows/worldgen/src/lib.rs`) directly from ledger/roster/terrain/
/// climate data rather than calling `exposure_of` itself (spec §9.2: "the
/// flag re-derives the exposure class from the ledger independently of the
/// lexicon pipeline" — calling `exposure_of` would be the config-echo trap
/// `epithet_honorific`'s doc comment already names for a different metric).
/// Sufficient for `exposure_sound`'s "no Root at Unknown" check:
/// `build_lexicon` only ever mints a `Root` from a `Steeped` classification
/// (`KnowsOf`/`Unknown` both fall through to `Compound`/`Gap`), so the
/// KnowsOf-via-neighbor and sea-proximity rules `exposure_of` also carries
/// are irrelevant to this specific soundness check and are not reproduced
/// here. `None` if `species` is not in this world's roster.
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
        for concept in ["home", "hearth", "god", "spirit"] {
            if v.world().registry.concept(concept).is_some() {
                steeped.insert(concept.to_string());
            }
        }
    }
    Some(steeped)
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
/// `v.roster`) or before treating a `lexicon_of` result as meaningful: a
/// study pin set may build with a non-default roster (e.g.
/// `census-of-the-meeting`'s solo `[goblin]`/`[goblin-twin]` rosters), and
/// `lexicon_of` alone would silently keep resolving hobgoblin/bugbear/
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
/// world's full registered concept universe (`exposure_of` classifies every
/// registered concept, so its key set is exactly the registry), exactly as
/// `build_lexicon` does. The shared basis for the monophyly and clean-outgroup
/// checks: it proves shared ancestry, never mere self-consistency.
fn goblinoid_proto_assignment(v: &FullView) -> std::collections::BTreeMap<String, Vec<Segment>> {
    let proto_ph = hornvale_worldgen::proto_phonology_of(v.world(), "goblinoid");
    let universe: Vec<&str> = v
        .world()
        .registry
        .concepts()
        .map(|c| c.name.as_str())
        .collect();
    // The merger-aware assignment (epoch root/v3) build_lexicon consumes, so
    // this reconstruction matches every daughter's recorded proto exactly.
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

/// Whether `concept` is **core** vocabulary — high functional load, where
/// homophony genuinely confuses (Nathan's "near-zero for core" target). Core
/// is the authored, always-lexicalized Swadesh strata: the universal
/// stratum, the body pack, and the kin pack. Everything else — the
/// exposure-gated color ladder (`color_pack`, ranked) and the biome-class
/// Terrain concepts a culture only names where it settles — is periphery,
/// where incidental homophony is tolerable. The split is entirely
/// data-driven (pack membership), never a doc-string heuristic.
/// The **semantic domain** of a core concept — the authored Swadesh stratum it
/// belongs to (universal / body / kin), or `None` for periphery (a concept in
/// no core pack). `domain.is_some()` is therefore core-hood — the
/// functional-load split the fix targets. Two core concepts are *confusable*
/// when their domains match (they compete in the same context; a listener
/// cannot separate them by topic) and *free* when they differ. Data-driven from
/// pack membership (decision 0011: studies are data).
fn concept_domain(concept: &str) -> Option<&'static str> {
    if hornvale_language::universal_stratum()
        .iter()
        .any(|e| e.concept == concept)
    {
        Some("universal")
    } else if hornvale_language::body_pack()
        .iter()
        .any(|e| e.concept == concept)
    {
        Some("body")
    } else if hornvale_language::kin_pack()
        .iter()
        .any(|e| e.concept == concept)
    {
        Some("kin")
    } else {
        None
    }
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
/// drew), its nucleus must be exactly `ph.nuclei` vowels, and every segment
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
/// for the pattern: `language_of_in`/`lexicon_of` together against
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
            for after_nucleus in match_nucleus(chars, after_onset, ph.nuclei, ph) {
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
        let v = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let goblin = species_head_sentiment(&v, "goblin");
        let hobgoblin = species_head_sentiment(&v, "hobgoblin");
        assert!(
            goblin.is_some() && hobgoblin.is_some(),
            "seed 42 places both peoples"
        );
        // The reading must depend on the species argument, not ignore it. A
        // species-ignoring implementation (reading `beliefs_of().first()`)
        // would return goblin's answer for every people, collapsing all four
        // readings to one value. Assert the argument actually discriminates:
        // goblin and bugbear must not read alike, because bugbear places no
        // flagship on seed 42 and goblin does.
        let bugbear = species_head_sentiment(&v, "bugbear");
        assert_ne!(
            goblin, bugbear,
            "the reading discriminates on the species asked for, not the world's first belief"
        );
        assert!(
            bugbear.is_none(),
            "bugbear places no flagship on seed 42 — Absent is the honest reading, \
             and it is exactly the fact SKY-25, the terminator battery, and the \
             frozen-sky calibration all got wrong"
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
        let view = FullView::build(Seed(42), &pins).unwrap();
        let first = beliefs_of(view.world())
            .into_iter()
            .next()
            .expect("locked world has beliefs");
        assert_eq!(first.source_kind, "celestial-body");
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
        // chorus-param-spread, chorus-sky-calibration).
        assert_eq!(registry().len(), 158);
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
            let attested = hornvale_worldgen::lexicon_of(view.world(), species)
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
        // also places. kobold is the roster's ONLY Knowledge-status
        // people, so the "false" (plain glossed word) branch has no live
        // seed-42 witness anymore; what remains true and checkable is that
        // a non-flagshipping species reads `Absent`, not a stale `false`.
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            epithet_honorific(&view, "hobgoblin"),
            MetricValue::Flag(true),
            "hobgoblin committed epithets must carry the honorific affix"
        );
        assert_eq!(
            epithet_honorific(&view, "kobold"),
            MetricValue::Absent,
            "kobold never flagships under the niche-differentiated-K \
             coexistence stack at seed 42, so it commits no epithets to \
             detect — Absent, not a stale true/false"
        );
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
        let report = hornvale_worldgen::demography_report(view.world(), view.components()).unwrap();
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
        assert_eq!(
            m("flagship-subsistence"),
            MetricValue::Text("farming".to_string())
        );
        assert_eq!(
            m("flagship-biome"),
            MetricValue::Text("temperate-forest".to_string())
        );
        assert_eq!(m("flagship-coastal"), MetricValue::Flag(false));
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
    fn solo_goblin_and_twin_share_placement_and_head_domain_at_seed_42() {
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
        // Identical vectors + no competitor ⇒ identical cell (spec §3).
        let gcell = g
            .world()
            .ledger
            .value_of(gf.id, hornvale_settlement::CELL_ID)
            .cloned();
        let tcell = t
            .world()
            .ledger
            .value_of(tf.id, hornvale_settlement::CELL_ID)
            .cloned();
        assert_eq!(
            gcell, tcell,
            "solo goblin and twin must land in the same cell"
        );
        // Same cell, same sky, same perception ⇒ same head-deity domain.
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
    // `accounts_of`. Seed 1 places goblin (sky_capability 0.5) and
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
    fn seed_1_sky_calibration_is_absent_on_the_tied_pair() {
        // Goblin and hobgoblin both lose every sky fact (both below the
        // 0.6 SkyGraded threshold): their sky distortions tie at 1.0, so
        // no strictly-comparable pair exists — this Absent IS the tie
        // rule's test.
        let view = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        assert_eq!(
            extract(&view, "chorus-sky-calibration"),
            MetricValue::Absent,
            "seed 1's only pair (goblin/hobgoblin) ties on sky distortion at 1.0"
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
    fn seed_2_distinctiveness_exceeds_seed_1() {
        // Kobold's knowledge divergence on the moons (it keeps what goblin
        // and hobgoblin lose) stacks on top of the stance asymmetry already
        // present at seed 1, so the mean pairwise distinctiveness must
        // strictly increase.
        let seed1 = FullView::build(Seed(1), &SkyPins::default()).unwrap();
        let seed2 = FullView::build(Seed(2), &SkyPins::default()).unwrap();
        let d1 = match extract(&seed1, "chorus-distinctiveness") {
            MetricValue::Number(n) => n,
            other => panic!("seed 1: expected Number, got {other:?}"),
        };
        let d2 = match extract(&seed2, "chorus-distinctiveness") {
            MetricValue::Number(n) => n,
            other => panic!("seed 2: expected Number, got {other:?}"),
        };
        assert!(
            d2 > d1,
            "seed 2 distinctiveness ({d2}) should exceed seed 1's ({d1})"
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
}
