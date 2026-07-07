//! Tier-1 metrics extractors: analyzable properties of generated worlds.

use hornvale_astronomy::{Calendar, NeighborClass, Rotation, StarSystem};
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, Seed, Value, World};
use hornvale_religion::beliefs_of;
use hornvale_terrain::GlobeSummary;
use hornvale_worldgen::{BuildError, Sky, SkyChoice, build_world, climate_of, sky_of, terrain_of};

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
}

impl WorldView {
    /// Build a complete world view: generate the world, extract the sky,
    /// derive the calendar, and gather the notes. Generated sky only.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<WorldView, BuildError> {
        let world = build_world(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
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
            doc: "Category of the first belief: 'eternal' if its tenet contains 'never', \
                   else 'cyclic'; Absent if no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let beliefs = beliefs_of(&v.world);
                if let Some(first) = beliefs.first() {
                    if first.tenet.contains("never") {
                        MetricValue::Text("eternal".to_string())
                    } else {
                        MetricValue::Text("cyclic".to_string())
                    }
                } else {
                    MetricValue::Absent
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
            doc: "The flagship settlement's committed subsistence mode; Absent if there is \
                   no flagship or no committed subsistence",
            summary: SummaryKind::Categorical,
            extract: |v| match hornvale_settlement::village_info(&v.world) {
                Some(info) => match hornvale_culture::subsistence_of(&v.world, info.id) {
                    Some(s) => MetricValue::Text(s),
                    None => MetricValue::Absent,
                },
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "flagship-biome",
            doc: "The flagship settlement's committed biome; Absent if there is no flagship",
            summary: SummaryKind::Categorical,
            extract: |v| match hornvale_settlement::village_info(&v.world) {
                Some(info) => match v.world.ledger.text_of(info.id, hornvale_settlement::BIOME) {
                    Some(b) => MetricValue::Text(b.to_string()),
                    None => MetricValue::Absent,
                },
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "flagship-coastal",
            doc: "Whether the flagship settlement's cell borders an ocean cell, recomputed \
                   from the terrain provider; Absent if there is no flagship",
            summary: SummaryKind::Flag,
            extract: |v| {
                let Some(info) = hornvale_settlement::village_info(&v.world) else {
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
            doc: "Number of castes present in the flagship settlement's emergent structure \
                   (a stratification proxy); Absent if there is no flagship",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| match hornvale_settlement::village_info(&v.world) {
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
            doc: "Number of beliefs in the world's pantheon; Absent if there are none",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            },
            extract: |v| {
                let beliefs = beliefs_of(&v.world);
                if beliefs.is_empty() {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(beliefs.len() as f64)
                }
            },
        },
        Metric {
            name: "cult-form",
            doc: "The pantheon's shared cult form ('organized' or 'folk'); Absent if there \
                   are no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| match hornvale_religion::cult_form_of(&v.world) {
                Some(form) => MetricValue::Text(form),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-verticality",
            doc: "Whether the flagship pantheon is ranked (a high god presides) or flat; \
                   Absent if there are no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let beliefs = beliefs_of(&v.world);
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
            doc: "Category of the head deity (the most salient belief): 'eternal' if its \
                   tenet contains 'never', else 'cyclic'; Absent if no beliefs",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let beliefs = beliefs_of(&v.world);
                if let Some(head) = beliefs.first() {
                    if head.tenet.contains("never") {
                        MetricValue::Text("eternal".to_string())
                    } else {
                        MetricValue::Text("cyclic".to_string())
                    }
                } else {
                    MetricValue::Absent
                }
            },
        },
    ]
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
    fn registry_has_thirty_three_metrics_after_firm_ground() {
        assert_eq!(registry().len(), 33);
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
        assert!(matches!(m("flagship-subsistence"), MetricValue::Text(_)));
        assert!(matches!(m("flagship-biome"), MetricValue::Text(_)));
        assert!(matches!(m("flagship-coastal"), MetricValue::Flag(_)));
        assert!(matches!(m("flagship-structure-size"), MetricValue::Number(n) if n >= 1.0));
        assert!(
            matches!(m("endorheic-coverage"), MetricValue::Number(f) if (0.0..=1.0).contains(&f))
        );
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
}
