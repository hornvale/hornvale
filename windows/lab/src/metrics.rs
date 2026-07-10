//! Tier-1 metrics extractors: analyzable properties of generated worlds.

use hornvale_astronomy::{Calendar, NeighborClass, Rotation, StarSystem};
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, EntityId, Phenomenon, Seed, Value, World};
use hornvale_language::{
    GapReason, LexEntry, Manner, MorphOptions, NameKind, Namer, Phonology, Segment, romanize,
};
use hornvale_religion::beliefs_of;
use hornvale_terrain::GlobeSummary;
use hornvale_worldgen::{
    BuildError, Sky, SkyChoice, build_world_with_roster, climate_of, flagship_of, language_of_in,
    observed_phenomena_as_in, sky_of, terrain_of,
};

use hornvale_astronomy::SkyPins;

/// A world and its derived astronomy/calendar/belief context.
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
    pub roster: Vec<hornvale_species::SpeciesDef>,
}

impl WorldView {
    /// Build a world view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<WorldView, BuildError> {
        Self::build_with_roster(seed, pins, hornvale_worldgen::default_roster())
    }

    /// Build a world view with an explicit species roster (spec §3).
    pub fn build_with_roster(
        seed: Seed,
        pins: &SkyPins,
        roster: Vec<hornvale_species::SpeciesDef>,
    ) -> Result<WorldView, BuildError> {
        let world = build_world_with_roster(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
            &roster,
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
            roster,
        })
    }
}

/// A single-value outcome from a metric extractor.
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
pub struct Metric {
    /// The metric's canonical name.
    pub name: &'static str,
    /// Human-readable documentation.
    pub doc: &'static str,
    /// The kind of analysis this metric supports.
    pub summary: SummaryKind,
    /// Extract this metric from a view of a world.
    pub extract: fn(&WorldView) -> MetricValue,
}

/// Build the registry of tier-1 metrics.
pub fn registry() -> Vec<Metric> {
    vec![
        Metric {
            name: "star-class",
            doc: "Spectral class of the host star",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.system.star.class_name.clone()),
        },
        Metric {
            name: "tidally-locked",
            doc: "Whether the world is tidally locked to its star",
            summary: SummaryKind::Flag,
            extract: |v| MetricValue::Flag(matches!(v.system.anchor.rotation, Rotation::Locked)),
        },
        Metric {
            name: "day-length-hours",
            doc: "Length of the solar day in standard hours; Absent if tidally locked",
            summary: SummaryKind::Numeric {
                bucket_edges: &[16.0, 20.0, 24.0, 28.0, 32.0, 36.0, 40.0],
            },
            extract: |v| match &v.system.anchor.rotation {
                Rotation::Locked => MetricValue::Absent,
                Rotation::Spinning { day } => MetricValue::Number(day.get() * 24.0),
            },
        },
        Metric {
            name: "year-std-days",
            doc: "Length of the year in standard days",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 200.0, 400.0, 600.0, 800.0, 1000.0, 1200.0, 1400.0],
            },
            extract: |v| MetricValue::Number(v.system.anchor.year.get()),
        },
        Metric {
            name: "year-local-days",
            doc: "Length of the year in local days; Absent if tidally locked",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 200.0, 400.0, 600.0, 800.0, 1000.0, 1200.0, 1400.0],
            },
            extract: |v| {
                if let Some(day_len) = v.calendar.day_length() {
                    MetricValue::Number(v.system.anchor.year.get() / day_len.get())
                } else {
                    MetricValue::Absent
                }
            },
        },
        Metric {
            name: "obliquity-degrees",
            doc: "Axial tilt in degrees",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0],
            },
            extract: |v| MetricValue::Number(v.system.anchor.obliquity.get()),
        },
        Metric {
            name: "obliquity-range",
            doc: "Peak-to-peak obliquity swing over one obliquity period (2× the \
                   deep-time forcing amplitude, SKY-21); a moonless world keeps the \
                   full drawn wobble, a moon damps it",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0],
            },
            extract: |v| MetricValue::Number(2.0 * v.system.forcing.obliquity_amp),
        },
        Metric {
            name: "moons-admitted",
            doc: "Number of moons in orbit",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.system.moons.len().to_string()),
        },
        Metric {
            name: "refused-a-moon",
            doc: "Whether moon genesis recorded refusals",
            summary: SummaryKind::Flag,
            extract: |v| MetricValue::Flag(!v.notes.is_empty()),
        },
        Metric {
            name: "total-tide",
            doc: "Sum of all moon tidal forces",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0],
            },
            extract: |v| {
                let total: f64 = v.system.moons.iter().map(|m| m.tide_rel).sum();
                MetricValue::Number(total)
            },
        },
        Metric {
            name: "months-per-year-innermost",
            doc: "How many cycles of the nearest moon fit in one year; Absent if no moons",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 700.0],
            },
            extract: |v| {
                if let Some(months) = v.calendar.months_per_year(0) {
                    MetricValue::Number(months)
                } else {
                    MetricValue::Absent
                }
            },
        },
        Metric {
            name: "neighbor-count",
            doc: "Number of notable neighbor stars",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.system.neighbors.len().to_string()),
        },
        Metric {
            name: "brightest-neighbor-class",
            doc: "Spectral class of the brightest neighbor, in kebab-case",
            summary: SummaryKind::Categorical,
            extract: |v| {
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
            },
        },
        Metric {
            name: "belief-kind",
            doc: "The first belief's sentiment tag ('eternal', 'cyclic', or 'ambient'); \
                   Absent if no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let beliefs = beliefs_of(&v.world);
                match beliefs.first() {
                    Some(first) => MetricValue::Text(first.sentiment.as_str().to_string()),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "genesis-note-count",
            doc: "Number of genesis notes recorded",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.notes.len().to_string()),
        },
        Metric {
            name: "plate-count",
            doc: "Number of tectonic plates the globe drew or was pinned to",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.globe.plate_count.to_string()),
        },
        Metric {
            name: "ocean-fraction",
            doc: "Fraction of globe cells below sea level",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9],
            },
            extract: |v| MetricValue::Number(v.globe.ocean_fraction),
        },
        Metric {
            name: "mountain-coverage",
            doc: "Fraction of land cells standing above 2000 m over the sea",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: |v| {
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
            },
        },
        Metric {
            name: "band-count",
            doc: "Circulation bands per hemisphere; 'locked' if tidally locked",
            summary: SummaryKind::Categorical,
            extract: |v| match v.climate.band_count() {
                Some(n) => MetricValue::Text(n.to_string()),
                None => MetricValue::Text("locked".to_string()),
            },
        },
        Metric {
            name: "habitable-fraction",
            doc: "Fraction of cells that are habitable (land, water, tolerable season)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5],
            },
            extract: |v| MetricValue::Number(v.climate.habitable_fraction()),
        },
        Metric {
            name: "unrest-coverage",
            doc: "Fraction of cells with tectonic unrest above 0.3",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.5],
            },
            extract: |v| {
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
            },
        },
        Metric {
            name: "dominant-land-biome",
            doc: "The most common land biome by cell count, kebab-case",
            summary: SummaryKind::Categorical,
            extract: |v| {
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
            },
        },
        Metric {
            name: "mean-land-temperature-c",
            doc: "Annual-mean temperature averaged over land cells, °C; Absent \
                   if the world has no land",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0],
            },
            extract: |v| {
                let geo = v.terrain.geosphere();
                let (mut sum, mut count) = (0.0_f64, 0_u32);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        sum += v.climate.mean_temperature_at(cell);
                        count += 1;
                    }
                }
                if count == 0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(sum / f64::from(count))
                }
            },
        },
        Metric {
            name: "settlement-count",
            doc: "Number of settlements placed in the world",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 10.0, 20.0, 40.0, 60.0, 80.0, 120.0],
            },
            extract: |v| MetricValue::Number(hornvale_terrain::places(&v.world).len() as f64),
        },
        Metric {
            name: "mean-population",
            doc: "Mean population across every settlement's committed population fact; \
                   Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: |v| {
                let places = hornvale_terrain::places(&v.world);
                let pops: Vec<f64> = places
                    .iter()
                    .filter_map(|p| {
                        match v
                            .world
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
            },
        },
        Metric {
            name: "flagship-subsistence",
            doc: "The goblin flagship settlement's committed subsistence mode (the pantheon's \
                   community, spec §6); Absent if there is no goblin flagship or no committed \
                   subsistence",
            summary: SummaryKind::Categorical,
            extract: |v| match flagship_of(&v.world, "goblin") {
                Some(info) => match hornvale_culture::subsistence_of(&v.world, info.id) {
                    Some(s) => MetricValue::Text(s),
                    None => MetricValue::Absent,
                },
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "flagship-biome",
            doc: "The goblin flagship settlement's committed biome; Absent if there is no \
                   goblin flagship",
            summary: SummaryKind::Categorical,
            extract: |v| match flagship_of(&v.world, "goblin") {
                Some(info) => match v.world.ledger.text_of(info.id, hornvale_settlement::BIOME) {
                    Some(b) => MetricValue::Text(b.to_string()),
                    None => MetricValue::Absent,
                },
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "flagship-coastal",
            doc: "Whether the goblin flagship settlement's cell borders an ocean cell, \
                   recomputed from the terrain provider; Absent if there is no goblin flagship",
            summary: SummaryKind::Flag,
            extract: |v| {
                let Some(info) = flagship_of(&v.world, "goblin") else {
                    return MetricValue::Absent;
                };
                let Some(Value::Number(cell_id)) = v
                    .world
                    .ledger
                    .value_of(info.id, hornvale_settlement::CELL_ID)
                else {
                    return MetricValue::Absent;
                };
                let cell = CellId(*cell_id as u32);
                let coastal = v
                    .terrain
                    .geosphere()
                    .neighbors(cell)
                    .iter()
                    .any(|n| v.terrain.is_ocean(*n));
                MetricValue::Flag(coastal)
            },
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
            extract: |v| match flagship_of(&v.world, "goblin") {
                Some(info) => {
                    MetricValue::Number(hornvale_culture::castes_of(&v.world, info.id).len() as f64)
                }
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "endorheic-coverage",
            doc: "Fraction of land cells that are endorheic (interior-draining)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: |v| {
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
            },
        },
        Metric {
            name: "pantheon-size",
            doc: "Number of beliefs in the goblin flagship's pantheon; Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| {
                let Some(info) = flagship_of(&v.world, "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(&v.world, info.id);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(beliefs.len() as f64)
                }
            },
        },
        Metric {
            name: "cult-form",
            doc: "The goblin flagship's pantheon's shared cult form ('organized' or 'folk'); \
                   Absent if no goblin beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let Some(info) = flagship_of(&v.world, "goblin") else {
                    return MetricValue::Absent;
                };
                match hornvale_religion::cult_form_held_by(&v.world, info.id) {
                    Some(form) => MetricValue::Text(form),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "pantheon-verticality",
            doc: "Whether the goblin flagship's pantheon is ranked (a high god presides) or \
                   flat; Absent if there is no goblin flagship pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let Some(info) = flagship_of(&v.world, "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(&v.world, info.id);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else if beliefs.iter().any(|b| b.high_god) {
                    MetricValue::Text("ranked".to_string())
                } else {
                    MetricValue::Text("flat".to_string())
                }
            },
        },
        Metric {
            name: "head-deity-periodicity",
            doc: "The sentiment tag of the goblin flagship's head deity (the most salient \
                   belief): 'eternal', 'cyclic', or 'ambient'; Absent if no goblin beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let Some(info) = flagship_of(&v.world, "goblin") else {
                    return MetricValue::Absent;
                };
                let beliefs = hornvale_religion::beliefs_held_by(&v.world, info.id);
                match beliefs.first() {
                    Some(head) => MetricValue::Text(head.sentiment.as_str().to_string()),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "goblin-flagship-roles",
            doc: "The goblin flagship's committed role ladder, comma-joined, \
                   lowest to highest; Absent if goblins placed no settlement",
            summary: SummaryKind::Categorical,
            extract: |v| match flagship_of(&v.world, "goblin") {
                Some(info) => {
                    let castes = hornvale_culture::castes_of(&v.world, info.id);
                    if castes.is_empty() {
                        MetricValue::Absent
                    } else {
                        MetricValue::Text(castes.join(","))
                    }
                }
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "kobold-flagship-roles",
            doc: "The kobold flagship's committed role ladder, comma-joined, \
                   lowest to highest; Absent if kobolds placed no settlement",
            summary: SummaryKind::Categorical,
            extract: |v| match flagship_of(&v.world, "kobold") {
                Some(info) => {
                    let castes = hornvale_culture::castes_of(&v.world, info.id);
                    if castes.is_empty() {
                        MetricValue::Absent
                    } else {
                        MetricValue::Text(castes.join(","))
                    }
                }
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "goblin-flagship-population",
            doc: "The goblin flagship settlement's committed population; \
                   Absent if goblins placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: |v| match flagship_of(&v.world, "goblin") {
                Some(info) => MetricValue::Number(f64::from(info.population)),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "kobold-flagship-population",
            doc: "The kobold flagship settlement's committed population; \
                   Absent if kobolds placed no settlement",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 500.0],
            },
            extract: |v| match flagship_of(&v.world, "kobold") {
                Some(info) => MetricValue::Number(f64::from(info.population)),
                None => MetricValue::Absent,
            },
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
            extract: |v| flagship_surplus(v, "goblin"),
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
            extract: |v| flagship_surplus(v, "kobold"),
        },
        Metric {
            name: "goblin-flagship-coastal",
            doc: "Whether the goblin flagship settlement's cell borders an \
                   ocean cell, recomputed from the terrain provider; Absent \
                   if goblins placed no settlement",
            summary: SummaryKind::Flag,
            extract: |v| flagship_coastal(v, "goblin"),
        },
        Metric {
            name: "kobold-flagship-coastal",
            doc: "Whether the kobold flagship settlement's cell borders an \
                   ocean cell, recomputed from the terrain provider; Absent \
                   if kobolds placed no settlement",
            summary: SummaryKind::Flag,
            extract: |v| flagship_coastal(v, "kobold"),
        },
        Metric {
            name: "goblin-settlement-count",
            doc: "Number of settlements peopled by goblins",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: |v| MetricValue::Number(species_settlement_count(v, "goblin")),
        },
        Metric {
            name: "kobold-settlement-count",
            doc: "Number of settlements peopled by kobolds",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 4.0, 8.0, 16.0],
            },
            extract: |v| MetricValue::Number(species_settlement_count(v, "kobold")),
        },
        Metric {
            name: "head-deity-domain-goblin",
            doc: "Venue domain of the goblin flagship's head deity: solar, lunar, or ambient; Absent without a goblin pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "head-deity-domain-kobold",
            doc: "Venue domain of the kobold flagship's head deity: solar, lunar, or ambient; Absent without a kobold pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-goblin",
            doc: "Number of deities in the goblin flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-kobold",
            doc: "Number of deities in the kobold flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-goblin",
            doc: "Cult form of the goblin flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-kobold",
            doc: "Cult form of the kobold flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "blind-attribution-correct",
            doc: "Whether the fixed structural rule (lunar head, then cyclic share, then size — no lexical input) attributes the kobold pantheon correctly; Absent unless both peoples hold pantheons",
            summary: SummaryKind::Flag,
            extract: |v| {
                let (Some(g), Some(k)) = (pantheon_sig(v, "goblin"), pantheon_sig(v, "kobold"))
                else {
                    return MetricValue::Absent;
                };
                // The rule is a symmetric function of the unordered pair;
                // presenting (goblin, kobold) and requiring index 1 is the
                // correctness check, not a labeling leak.
                MetricValue::Flag(pick_kobold([&g, &k]) == Some(1))
            },
        },
        Metric {
            name: "phonotactic-validity-goblin",
            doc: "Whether every generated name (settlement, deity, epithet) attributed to \
                   goblins in this world re-validates against the goblin phonology, \
                   independently re-derived and re-parsed from the surface string; \
                   Absent if goblins produced no names",
            summary: SummaryKind::Flag,
            extract: |v| phonotactic_validity(v, "goblin"),
        },
        Metric {
            name: "phonotactic-validity-kobold",
            doc: "Whether every generated name (settlement, deity, epithet) attributed to \
                   kobolds in this world re-validates against the kobold phonology, \
                   independently re-derived and re-parsed from the surface string; \
                   Absent if kobolds produced no names",
            summary: SummaryKind::Flag,
            extract: |v| phonotactic_validity(v, "kobold"),
        },
        Metric {
            name: "epithet-honorific-goblin",
            doc: "Whether every committed goblin deity epithet carries a prepended honorific \
                   affix — DETECTED from the committed epithet content: the committed word, \
                   case-folded, must end with the independently re-derived honorific-OFF stem \
                   and be strictly longer (Rank status basis → honorifics on, spec §7); Absent \
                   if goblins hold no pantheon",
            summary: SummaryKind::Flag,
            extract: |v| epithet_honorific(v, "goblin"),
        },
        Metric {
            name: "epithet-honorific-kobold",
            doc: "Whether every committed kobold deity epithet carries a prepended honorific \
                   affix — DETECTED from the committed epithet content (see \
                   epithet-honorific-goblin); kobold's Knowledge status basis leaves honorifics \
                   off, so the committed epithet equals the plain stem and this reads false; \
                   Absent if kobolds hold no pantheon",
            summary: SummaryKind::Flag,
            extract: |v| epithet_honorific(v, "kobold"),
        },
        Metric {
            name: "name-length-goblin",
            doc: "Mean character length of every generated name (settlement, deity, epithet) \
                   attributed to goblins in this world; Absent if goblins produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: |v| mean_name_length(v, "goblin"),
        },
        Metric {
            name: "name-length-kobold",
            doc: "Mean character length of every generated name (settlement, deity, epithet) \
                   attributed to kobolds in this world; Absent if kobolds produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: |v| mean_name_length(v, "kobold"),
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
            extract: name_collision_rate,
        },
        Metric {
            name: "head-deity-domain-goblin-twin",
            doc: "Venue domain of the goblin-twin flagship's head deity (null control, spec §4); Absent without a goblin-twin pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-goblin-twin",
            doc: "Number of deities in the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-goblin-twin",
            doc: "Cult form of the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "name-length-goblin-twin",
            doc: "Mean character length of every generated name attributed to the goblin-twin (null control); Absent if it produced no names",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
            },
            extract: |v| mean_name_length(v, "goblin-twin"),
        },
        Metric {
            name: "pantheon-cyclic-share-goblin",
            doc: "Fraction of the goblin flagship pantheon's source phenomena that are periodic (the pick_kobold input the null control needs); Absent without a goblin pantheon",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-cyclic-share-goblin-twin",
            doc: "Fraction of the goblin-twin flagship pantheon's source phenomena that are periodic (null control); Absent without one",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            },
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
            extract: name_gloss_true,
        },
        Metric {
            name: "lexicon-regular-goblin",
            doc: "Whether every goblin lexicon Root entry's recorded sound-change \
                   derivation replays byte-identically through evolve (Neogrammarian \
                   regularity, spec §9.1); Absent if the goblin lexicon minted no Root",
            summary: SummaryKind::Flag,
            extract: |v| lexicon_regular(v, "goblin"),
        },
        Metric {
            name: "lexicon-regular-kobold",
            doc: "Whether every kobold lexicon Root entry's recorded sound-change \
                   derivation replays byte-identically through evolve (Neogrammarian \
                   regularity, spec §9.1); Absent if the kobold lexicon minted no Root",
            summary: SummaryKind::Flag,
            extract: |v| lexicon_regular(v, "kobold"),
        },
        Metric {
            name: "exposure-sound-goblin",
            doc: "Whether the goblin lexicon is exposure-sound: no concept an INDEPENDENT \
                   re-derivation of exposure classifies Unknown ever backs a Root entry, and \
                   every committed Gap carries a non-empty reason (spec §9.2); Absent if the \
                   goblin lexicon has no entries",
            summary: SummaryKind::Flag,
            extract: |v| exposure_sound(v, "goblin"),
        },
        Metric {
            name: "exposure-sound-kobold",
            doc: "Whether the kobold lexicon is exposure-sound: no concept an INDEPENDENT \
                   re-derivation of exposure classifies Unknown ever backs a Root entry, and \
                   every committed Gap carries a non-empty reason (spec §9.2); Absent if the \
                   kobold lexicon has no entries",
            summary: SummaryKind::Flag,
            extract: |v| exposure_sound(v, "kobold"),
        },
        Metric {
            name: "hue-depth-goblin",
            doc: "The goblin hue-ladder acquisition depth (2-5) derived from its perception \
                   vector's night-vision (spec §7's pack-depth model card); Absent if goblin \
                   is not in this world's roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| hue_depth(v, "goblin"),
        },
        Metric {
            name: "hue-depth-kobold",
            doc: "The kobold hue-ladder acquisition depth (2-5) derived from its perception \
                   vector's night-vision (spec §7's pack-depth model card); Absent if kobold \
                   is not in this world's roster",
            summary: SummaryKind::Numeric {
                bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| hue_depth(v, "kobold"),
        },
        Metric {
            name: "shoreline-development",
            doc: "Shoreline development index: coastline length over the \
                  circumference of the circle with the land's area (1 = \
                  maximally compact); Absent without a shoreline",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 6.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::shoreline_development(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "hypsometric-bimodality",
            doc: "Ashman's D between land and ocean elevation populations \
                  (Earth is strongly bimodal); Absent when a world lacks land \
                  or ocean",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::hypsometric_bimodality(
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "shelf-fraction",
            doc: "Fraction of cells within the shelf band (±200 m) of sea \
                  level — the populated shelf Earth's hypsometry keeps",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.15, 0.2, 0.3],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                MetricValue::Number(hornvale_terrain::shape::shelf_fraction(
                    &globe.elevation,
                    globe.sea_level,
                ))
            },
        },
        Metric {
            name: "continent-count",
            doc: "Connected land components",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                MetricValue::Number(
                    hornvale_terrain::shape::land_component_sizes(
                        v.terrain.geosphere(),
                        &globe.elevation,
                        globe.sea_level,
                    )
                    .len() as f64,
                )
            },
        },
        Metric {
            name: "largest-continent-share",
            doc: "Largest land component's share of all land cells; Absent \
                  on a landless world",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 0.9],
            },
            extract: |v| {
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
            },
        },
        Metric {
            name: "plate-size-gini",
            doc: "Gini coefficient over plate cell counts (Earth's plate \
                  sizes are heavy-tailed; uniform Voronoi scores low)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                let mut counts = vec![0usize; globe.plates.len()];
                for (_, plate) in globe.plate_of.iter() {
                    counts[*plate as usize] += 1;
                }
                match hornvale_terrain::shape::gini(&counts) {
                    Some(g) => MetricValue::Number(g),
                    None => MetricValue::Absent,
                }
            },
        },
    ]
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

fn pantheon_sig(v: &WorldView, species: &str) -> Option<PantheonSig> {
    let flagship = flagship_of(&v.world, species)?;
    let beliefs = hornvale_religion::beliefs_held_by(&v.world, flagship.id);
    if beliefs.is_empty() {
        return None;
    }
    // Pantheons derive from the same first-place, hemisphere-culled vantage
    // religion's genesis observes (SEQ-4/SEQ-5).
    let seen = observed_phenomena_as_in(&v.world, &v.roster, species).ok()?;
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
        cult: hornvale_religion::cult_form_held_by(&v.world, flagship.id)
            .unwrap_or_else(|| "folk".to_string()),
        cyclic_share: cyclic as f64 / members.len().max(1) as f64,
    })
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
fn flagship_surplus(v: &WorldView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(&v.world, species) else {
        return MetricValue::Absent;
    };
    let Some(Value::Number(cell_id)) = v
        .world
        .ledger
        .value_of(info.id, hornvale_settlement::CELL_ID)
    else {
        return MetricValue::Absent;
    };
    let cell = CellId(*cell_id as u32);
    let class = hornvale_worldgen::biome_class(v.climate.biome_at(cell));
    let surplus =
        (hornvale_culture::fertility(class) * v.climate.moisture_at(cell)).clamp(0.0, 1.0);
    MetricValue::Number(surplus)
}

/// Recompute whether a species flagship's cell borders an ocean cell,
/// directly from the terrain provider.
fn flagship_coastal(v: &WorldView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(&v.world, species) else {
        return MetricValue::Absent;
    };
    let Some(Value::Number(cell_id)) = v
        .world
        .ledger
        .value_of(info.id, hornvale_settlement::CELL_ID)
    else {
        return MetricValue::Absent;
    };
    let cell = CellId(*cell_id as u32);
    let coastal = v
        .terrain
        .geosphere()
        .neighbors(cell)
        .iter()
        .any(|n| v.terrain.is_ocean(*n));
    MetricValue::Flag(coastal)
}

/// Count settlements peopled by `species`.
fn species_settlement_count(v: &WorldView, species: &str) -> f64 {
    v.world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .filter(|f| hornvale_species::species_of(&v.world, f.subject).as_deref() == Some(species))
        .count() as f64
}

/// Every generated name attributed to `species` in this world: its
/// settlement names (from the places registry) plus, if it holds a
/// pantheon, every deity name and epithet its flagship's beliefs carry.
/// Empty if the species placed nothing and holds no pantheon.
fn species_generated_names(v: &WorldView, species: &str) -> Vec<String> {
    let mut names: Vec<String> = hornvale_terrain::places(&v.world)
        .into_iter()
        .filter(|p| hornvale_species::species_of(&v.world, p.id).as_deref() == Some(species))
        .map(|p| p.name)
        .collect();
    if let Some(info) = flagship_of(&v.world, species) {
        for belief in hornvale_religion::beliefs_held_by(&v.world, info.id) {
            names.push(belief.deity);
            names.push(belief.epithet);
        }
    }
    names
}

/// Whether every generated name attributed to `species` in this world
/// re-validates against `species`' own re-derived phonology; `Absent` if it
/// produced no names.
fn phonotactic_validity(v: &WorldView, species: &str) -> MetricValue {
    let names = species_generated_names(v, species);
    if names.is_empty() {
        return MetricValue::Absent;
    }
    let ph = language_of_in(&v.world, &v.roster, species);
    MetricValue::Flag(names.iter().all(|n| is_phonotactically_valid(n, &ph)))
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
/// `hornvale_worldgen::deity_site_concepts` maps each pair. The honorific
/// affix is one template syllable drawn AFTER the site-concept picks and
/// PREPENDED, so re-deriving the same glossed epithet with honorifics OFF
/// yields exactly the plain word the committed epithet was built from: the
/// committed epithet carries the honorific iff, lowercased, it ends with
/// the lowercased plain word AND is strictly longer. `Flag(true)` iff
/// EVERY committed epithet carries it (goblin, Rank), `Flag(false)` iff
/// none does (kobold, Knowledge). A broken honorific pipeline — a goblin
/// epithet committed without its affix — would equal its plain word here
/// and flip the flag to false, which the preregistered calibration catches.
fn epithet_honorific(v: &WorldView, species: &str) -> MetricValue {
    let Some(info) = flagship_of(&v.world, species) else {
        return MetricValue::Absent;
    };
    let beliefs = hornvale_religion::beliefs_held_by(&v.world, info.id);
    if beliefs.is_empty() {
        return MetricValue::Absent;
    }
    // Religion (and the deity glosses drawn inside it) observes from the
    // world's first place, hemisphere-culled (SEQ-4/SEQ-5) — re-derive from
    // exactly that vantage so the check tracks the pipeline's real sky.
    let Ok(seen) = observed_phenomena_as_in(&v.world, &v.roster, species) else {
        return MetricValue::Absent;
    };
    let Ok(lexicon) = hornvale_worldgen::lexicon_of(&v.world, species) else {
        return MetricValue::Absent;
    };
    let ph = language_of_in(&v.world, &v.roster, species);
    let namer = Namer::new(&v.world.seed, species, &ph);
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
        let (plain, _) = namer.glossed_name(
            NameKind::Epithet,
            b.id.0,
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
fn mean_name_length(v: &WorldView, species: &str) -> MetricValue {
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
fn name_collision_rate(v: &WorldView) -> MetricValue {
    let mut names: Vec<String> = hornvale_terrain::places(&v.world)
        .into_iter()
        .map(|p| p.name)
        .collect();
    names.extend(beliefs_of(&v.world).into_iter().map(|b| b.deity));
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

/// A settlement's own re-derived site concepts (mirrors
/// `cli/tests/words_identity.rs`'s `settlement_site_concepts`): its
/// committed biome fact plus the presiding phenomenon concept its species
/// observes from THIS settlement's own vantage (its committed coordinates
/// cull the sky — SEQ-5; spec §9.3 defines gloss truthfulness against the
/// entity's own facts), if any. `None` if the settlement is missing a
/// biome/species fact, which `name_gloss_true` below treats as an
/// unverifiable (failing) row rather than skipping it silently.
fn settlement_site_concepts(v: &WorldView, id: EntityId) -> Option<Vec<String>> {
    let biome = v
        .world
        .ledger
        .text_of(id, hornvale_settlement::BIOME)?
        .to_string();
    let species = hornvale_species::species_of(&v.world, id)?;
    let phenomena =
        hornvale_worldgen::observed_phenomena_as_at(&v.world, &v.roster, &species, id).ok()?;
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
fn name_gloss_true(v: &WorldView) -> MetricValue {
    let mut checked = false;
    let mut all_true = true;
    for f in v.world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(gloss) = v.world.ledger.text_of(id, hornvale_worldgen::NAME_GLOSS) else {
            continue;
        };
        checked = true;
        match settlement_site_concepts(v, id) {
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
/// `cli/tests/words_identity.rs`'s `derivations_replay`. `Absent` if
/// `species` is not in this world's roster or its lexicon minted no `Root`.
fn lexicon_regular(v: &WorldView, species: &str) -> MetricValue {
    if !v.roster.iter().any(|d| d.name == species) {
        return MetricValue::Absent;
    }
    let ph = language_of_in(&v.world, &v.roster, species);
    let cascade = hornvale_language::draw_cascade(&v.world.seed, species);
    let Ok(lex) = hornvale_worldgen::lexicon_of(&v.world, species) else {
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
    v: &WorldView,
    species: &str,
) -> Option<std::collections::BTreeSet<String>> {
    let def = v.roster.iter().find(|d| d.name == species)?;
    let depths = hornvale_worldgen::pack_depths(&def.perception);
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

    let settled: Vec<CellId> = hornvale_terrain::places(&v.world)
        .into_iter()
        .filter(|p| hornvale_species::species_of(&v.world, p.id).as_deref() == Some(species))
        .filter_map(
            |p| match v.world.ledger.value_of(p.id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => Some(CellId(*n as u32)),
                _ => None,
            },
        )
        .collect();
    for &cell in &settled {
        steeped.insert(v.climate.biome_at(cell).concept_name().to_string());
    }

    let own_kind = format!("{species}-kind");
    if v.world.registry.concept(&own_kind).is_some() {
        steeped.insert(own_kind);
    }
    if !settled.is_empty() {
        let coexisting: std::collections::BTreeSet<String> = v
            .world
            .ledger
            .find(hornvale_species::PEOPLED_BY)
            .filter_map(|f| match &f.object {
                Value::Text(s) => Some(s.clone()),
                _ => None,
            })
            .collect();
        for placed in &coexisting {
            let kind = format!("{placed}-kind");
            if v.world.registry.concept(&kind).is_some() {
                steeped.insert(kind);
            }
        }
        for concept in ["home", "hearth", "god", "spirit"] {
            if v.world.registry.concept(concept).is_some() {
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
fn exposure_sound(v: &WorldView, species: &str) -> MetricValue {
    let Some(steeped) = independently_steeped_concepts(v, species) else {
        return MetricValue::Absent;
    };
    let Ok(lex) = hornvale_worldgen::lexicon_of(&v.world, species) else {
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
fn hue_depth(v: &WorldView, species: &str) -> MetricValue {
    match v.roster.iter().find(|d| d.name == species) {
        Some(def) => MetricValue::Number(f64::from(
            hornvale_worldgen::pack_depths(&def.perception).hue,
        )),
        None => MetricValue::Absent,
    }
}

/// Whether `name` parses as a legal sequence of syllables under `ph`,
/// independently of `hornvale_language::naming`'s generation code path: this
/// walks the SURFACE STRING back into [`Segment`]s and re-checks
/// phonotactic legality from scratch — every syllable's onset/coda
/// manner-sequence must match one of `ph.onsets`/`ph.codas` (the very
/// templates `draw_phonology` drew), its nucleus must be exactly
/// `ph.nuclei` vowels, and every segment consumed must be a member of
/// `ph.inventory`. Several romanizations are literal PREFIXES of others
/// sharing the same manner (`z`/`zh`, `s`/`sh`, `n`/`ng`, `k`/`kx`), so a
/// single greedy match per slot is unsound (a "z" false-match can swallow
/// what was really a "zh"); every matcher below returns every reachable
/// position and `parse_syllables` backtracks over the full cross product of
/// segment choice and template choice.
fn is_phonotactically_valid(name: &str, ph: &Phonology) -> bool {
    let chars: Vec<char> = name.to_lowercase().chars().collect();
    !chars.is_empty() && parse_syllables(&chars, 0, ph)
}

/// Recursively consume one syllable at a time from `chars[pos..]`; true iff
/// the remainder parses as a sequence of legal syllables. The base case
/// (`pos == chars.len()`) is only reachable after a caller has already
/// consumed at least one syllable, so an empty name never validates (see
/// [`is_phonotactically_valid`]'s explicit empty check).
fn parse_syllables(chars: &[char], pos: usize, ph: &Phonology) -> bool {
    if pos == chars.len() {
        return true;
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
                        if parse_syllables(chars, after_coda, ph) {
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
    fn seed_42_default_builds_successfully() {
        let view = WorldView::build(Seed(42), &SkyPins::default());
        assert!(view.is_ok());
        let view = view.unwrap();
        assert!(!view.system.moons.is_empty());
        assert!(!view.system.neighbors.is_empty());
    }

    #[test]
    fn seed_42_star_class_is_text() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let metrics = registry();
        let star_class = metrics.iter().find(|m| m.name == "star-class").unwrap();
        let value = (star_class.extract)(&view);
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    #[test]
    fn seed_42_moons_admitted_is_text() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let metrics = registry();
        let moons = metrics.iter().find(|m| m.name == "moons-admitted").unwrap();
        let value = (moons.extract)(&view);
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    #[test]
    fn seed_42_belief_kind_is_text_and_not_absent() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let metrics = registry();
        let belief = metrics.iter().find(|m| m.name == "belief-kind").unwrap();
        let value = (belief.extract)(&view);
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }

    #[test]
    fn locked_world_is_tidally_locked() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let metrics = registry();
        let tidally_locked = metrics.iter().find(|m| m.name == "tidally-locked").unwrap();
        let value = (tidally_locked.extract)(&view);
        assert_eq!(value, MetricValue::Flag(true));
    }

    #[test]
    fn locked_world_has_no_day_length() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let metrics = registry();
        let day_length = metrics
            .iter()
            .find(|m| m.name == "day-length-hours")
            .unwrap();
        let value = (day_length.extract)(&view);
        assert_eq!(value, MetricValue::Absent);
    }

    #[test]
    fn locked_world_has_no_local_day_year() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let metrics = registry();
        let year_local = metrics
            .iter()
            .find(|m| m.name == "year-local-days")
            .unwrap();
        let value = (year_local.extract)(&view);
        assert_eq!(value, MetricValue::Absent);
    }

    #[test]
    fn locked_world_belief_kind_is_eternal() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let metrics = registry();
        let belief = metrics.iter().find(|m| m.name == "belief-kind").unwrap();
        let value = (belief.extract)(&view);
        assert_eq!(value, MetricValue::Text("eternal".to_string()));
    }

    #[test]
    fn spinning_world_belief_kind_is_cyclic() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let metrics = registry();
        let belief = metrics.iter().find(|m| m.name == "belief-kind").unwrap();
        let value = (belief.extract)(&view);
        assert_eq!(value, MetricValue::Text("cyclic".to_string()));
    }

    #[test]
    fn seed_23_refused_a_moon() {
        let view = WorldView::build(Seed(23), &SkyPins::default()).unwrap();
        let metrics = registry();
        let refused = metrics.iter().find(|m| m.name == "refused-a-moon").unwrap();
        let value = (refused.extract)(&view);
        assert_eq!(value, MetricValue::Flag(true));
    }

    #[test]
    fn seed_23_genesis_note_count_is_one() {
        let view = WorldView::build(Seed(23), &SkyPins::default()).unwrap();
        let metrics = registry();
        let notes = metrics
            .iter()
            .find(|m| m.name == "genesis-note-count")
            .unwrap();
        let value = (notes.extract)(&view);
        assert_eq!(value, MetricValue::Text("1".to_string()));
    }

    #[test]
    fn registry_metric_count_is_pinned() {
        // The Meeting's 63, +7 for The Words (Task 12: name-gloss-true,
        // lexicon-regular-{goblin,kobold}, exposure-sound-{goblin,kobold},
        // hue-depth-{goblin,kobold}), plus the terrain-shape and later
        // metrics merged from main's campaigns.
        assert_eq!(registry().len(), 77);
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
        let view = WorldView::build(Seed(0), &SkyPins::default()).unwrap();
        for species in ["goblin", "kobold"] {
            let ph = hornvale_worldgen::language_of(&view.world, species);
            for n in species_generated_names(&view, species) {
                assert!(
                    is_phonotactically_valid(&n, &ph),
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
        // commit plain glossed words → false. Since The Branches, all
        // four peoples place a flagship at seed 42 (the founder floor,
        // MAP-22 K=1); hobgoblin is Rank-status (per
        // `hornvale_species::registry`) and placed, so it commits
        // honorific-bearing epithets — this metric is per-species and
        // does not depend on which OTHER Rank-status people (goblin) also
        // places. kobold (Knowledge) is unaffected.
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(
            epithet_honorific(&view, "hobgoblin"),
            MetricValue::Flag(true),
            "hobgoblin committed epithets must carry the honorific affix"
        );
        assert_eq!(
            epithet_honorific(&view, "kobold"),
            MetricValue::Flag(false),
            "kobold committed epithets must be plain glossed words"
        );
    }

    #[test]
    fn phonotactic_validator_rejects_garbage_and_empty_strings() {
        let view = WorldView::build(Seed(0), &SkyPins::default()).unwrap();
        let ph = hornvale_worldgen::language_of(&view.world, "goblin");
        assert!(!is_phonotactically_valid("", &ph));
        // "qw" (uvular stop q + labial approximant w): q is never a Stop
        // candidate in this drawn inventory (only p/t/d/g appear, per the
        // seed-0 debug dump), so this must not parse.
        assert!(!is_phonotactically_valid("qw", &ph));
    }

    #[test]
    fn land_metrics_extract_for_seed_42() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let reg = registry();
        let m = |name: &str| (reg.iter().find(|m| m.name == name).unwrap().extract)(&view);
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
    fn census_of_peoples_metrics_extract_for_seed_42() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let reg = registry();
        let m = |name: &str| (reg.iter().find(|m| m.name == name).unwrap().extract)(&view);
        assert!(matches!(m("settlement-count"), MetricValue::Number(n) if n > 0.0));
        assert!(matches!(m("mean-population"), MetricValue::Number(n) if n > 0.0));
        // The four `flagship-*` metrics are documented as specifically the
        // GOBLIN flagship's data (see their own doc comments above). Since
        // the founder floor (settlement's founder-reservation pass, MAP-22
        // K=1), goblin places its own flagship again at seed 42 — Xnebsvob,
        // farming, temperate-rainforest, coastal, a 5-caste structure
        // (slave, farmer, artisan, shaman, chief; see `almanac`'s seed-42
        // output and `cli/tests/branches_identity.rs`).
        assert_eq!(
            m("flagship-subsistence"),
            MetricValue::Text("farming".to_string())
        );
        assert_eq!(
            m("flagship-biome"),
            MetricValue::Text("temperate-rainforest".to_string())
        );
        assert_eq!(m("flagship-coastal"), MetricValue::Flag(true));
        assert_eq!(m("flagship-structure-size"), MetricValue::Number(5.0));
        assert!(
            matches!(m("endorheic-coverage"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
    }

    #[test]
    fn per_species_metrics_have_the_expected_kinds_for_seed_42() {
        let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
        let reg = registry();
        let m = |name: &str| (reg.iter().find(|m| m.name == name).unwrap().extract)(&view);
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
    }

    #[test]
    fn locked_world_band_count_metric_is_locked() {
        let pins = SkyPins {
            rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
            ..SkyPins::default()
        };
        let view = WorldView::build(Seed(42), &pins).unwrap();
        let reg = registry();
        let bc = (reg.iter().find(|m| m.name == "band-count").unwrap().extract)(&view);
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
        let belief_kind = metrics.iter().find(|m| m.name == "belief-kind").unwrap();
        assert!(
            table.contains(belief_kind.doc),
            "Missing doc for belief-kind: {}",
            belief_kind.doc
        );
    }

    #[test]
    fn solo_goblin_and_twin_share_placement_and_head_domain_at_seed_42() {
        let g = WorldView::build_with_roster(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_solo_roster(),
        )
        .unwrap();
        let t = WorldView::build_with_roster(
            Seed(42),
            &SkyPins::default(),
            crate::goblin_twin_solo_roster(),
        )
        .unwrap();
        let gf = flagship_of(&g.world, "goblin").unwrap();
        let tf = flagship_of(&t.world, "goblin-twin").unwrap();
        // Identical vectors + no competitor ⇒ identical cell (spec §3).
        let gcell = g
            .world
            .ledger
            .value_of(gf.id, hornvale_settlement::CELL_ID)
            .cloned();
        let tcell = t
            .world
            .ledger
            .value_of(tf.id, hornvale_settlement::CELL_ID)
            .cloned();
        assert_eq!(
            gcell, tcell,
            "solo goblin and twin must land in the same cell"
        );
        // Same cell, same sky, same perception ⇒ same head-deity domain.
        let reg = registry();
        let dom = |view: &WorldView, name: &str| match (reg
            .iter()
            .find(|m| m.name == name)
            .unwrap()
            .extract)(view)
        {
            MetricValue::Text(s) => s,
            other => panic!("expected domain text, got {other:?}"),
        };
        assert_eq!(
            dom(&g, "head-deity-domain-goblin"),
            dom(&t, "head-deity-domain-goblin-twin")
        );
        // But names differ (independent stream).
        assert_ne!(gf.name, tf.name, "twin names must differ from goblin's");
    }

    #[test]
    fn shape_metrics_are_present_deterministic_and_sane() {
        let names = [
            "shoreline-development",
            "hypsometric-bimodality",
            "shelf-fraction",
            "continent-count",
            "largest-continent-share",
            "plate-size-gini",
        ];
        let registry = registry();
        let a = WorldView::build(Seed(7), &SkyPins::default()).expect("seed 7");
        let b = WorldView::build(Seed(7), &SkyPins::default()).expect("seed 7 again");
        for name in names {
            let metric = registry
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("metric {name} not registered"));
            let va = (metric.extract)(&a);
            assert_eq!(va, (metric.extract)(&b), "{name} not deterministic");
            if let MetricValue::Number(x) = va {
                assert!(x.is_finite(), "{name} not finite: {x}");
            }
        }
    }

    #[test]
    fn build_with_roster_resolves_a_renamed_solo_species() {
        use hornvale_species::SpeciesDef;
        let goblin = hornvale_species::registry()["goblin"];
        let twin = SpeciesDef {
            name: "goblin-twin",
            ..goblin
        };
        let view = WorldView::build_with_roster(Seed(42), &SkyPins::default(), vec![twin]).unwrap();
        // The twin resolves through the view's roster (it is NOT in the global registry).
        let ph = hornvale_worldgen::language_of_in(&view.world, &view.roster, "goblin-twin");
        assert!(!ph.inventory.is_empty(), "twin phonology must draw");
        // And it placed a flagship peopled by the twin's name.
        assert!(flagship_of(&view.world, "goblin-twin").is_some());
    }
}
