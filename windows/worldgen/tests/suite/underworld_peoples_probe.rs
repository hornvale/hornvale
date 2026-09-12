//! Task 1 measurement for the four proposed Underworld peoples.
//!
//! This probe is intentionally a gate: it records whether the existing delve
//! substrate can offer distinct, reachable seats before species rows are
//! authored.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{ENERGY, LIGHT, PHYSIOGNOMY, SUBSTRATE, Seed, WATER};
use hornvale_species::{AxisPreference, EnvironmentNiche};
use hornvale_terrain::{TerrainPins, water_table_depth_m};
use hornvale_worldgen::delve_seating::seat_at;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to_with_artifacts,
};

const SEEDS: [u64; 3] = [42, 7, 1234];

#[test]
fn underworld_peoples_have_reachable_admission_niches() {
    let reports = measure_admission();
    assert_eq!(reports.len(), 3, "the pinned seed set must be measured");
    assert!(
        reports
            .iter()
            .all(|report| report.reachable_rungs == report.caves * 4),
        "each candidate must reach a rung in every cave-bearing column"
    );
    for report in &reports {
        println!(
            "seed {}: caves={} reachable={} wet={} dry={}",
            report.seed, report.caves, report.reachable_rungs, report.wet, report.dry
        );
    }
}

#[derive(Debug)]
struct AdmissionReport {
    seed: u64,
    caves: usize,
    reachable_rungs: usize,
    wet: usize,
    dry: usize,
}

fn measure_admission() -> Vec<AdmissionReport> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let niches = [
        mountain_dwarf_niche(),
        duergar_niche(),
        kuo_toa_niche(),
        svirfneblin_niche(),
    ];
    SEEDS
        .into_iter()
        .map(|seed_value| {
            let terrain = build_world_to_with_artifacts(
                Seed(seed_value),
                &SkyPins::default(),
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                BuildDepth::Terrain,
            )
            .expect("probe seed builds")
            .terrain
            .expect("terrain is present at Terrain depth");
            let sea = terrain.sea_level().get();
            let mut caves = 0;
            let mut reachable_rungs = 0;
            let mut wet = 0;
            let mut dry = 0;
            for vertex in terrain.geosphere().vertices() {
                if terrain.is_ocean(vertex) {
                    continue;
                }
                let Some(cave) = terrain.cave_at(vertex) else {
                    continue;
                };
                caves += 1;
                let table = water_table_depth_m(
                    terrain.drainage_at(vertex),
                    terrain.material_at(vertex).porosity,
                    terrain.elevation_at(vertex).get() - sea,
                );
                for niche in &niches {
                    if let Some(seat) =
                        seat_at(niche, &cave, terrain.geothermal_gradient_at(vertex), table)
                    {
                        reachable_rungs += 1;
                        if seat.works {
                            wet += 1;
                        } else {
                            dry += 1;
                        }
                    }
                }
            }
            AdmissionReport {
                seed: seed_value,
                caves,
                reachable_rungs,
                wet,
                dry,
            }
        })
        .collect()
}

fn mountain_dwarf_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.7)),
        (ENERGY, AxisPreference::graded(0.6)),
        (WATER, AxisPreference::graded(0.3)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("mountain dwarf candidate niche is valid")
}

fn duergar_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.3)),
        (ENERGY, AxisPreference::graded(0.9)),
        (WATER, AxisPreference::graded(0.8)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("duergar candidate niche is valid")
}

fn kuo_toa_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.2)),
        (ENERGY, AxisPreference::graded(0.8)),
        (WATER, AxisPreference::graded(1.0)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("kuo-toa candidate niche is valid")
}

fn svirfneblin_niche() -> EnvironmentNiche {
    EnvironmentNiche::new(&[
        (PHYSIOGNOMY, AxisPreference::graded(0.8)),
        (ENERGY, AxisPreference::graded(0.3)),
        (WATER, AxisPreference::graded(0.5)),
        (SUBSTRATE, AxisPreference::class(0.6)),
        (LIGHT, AxisPreference::graded(0.0)),
    ])
    .expect("svirfneblin candidate niche is valid")
}
