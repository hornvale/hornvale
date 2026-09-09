//! Deterministic, bounded movement and influence for the Skyworld overlay.
//!
//! The generator asks for a fixed number of coarse samples. This module
//! derives only those samples and the sparse records attached to them: it
//! never materializes a planet-by-time field. Continuous atmospheric values
//! remain [`hornvale_kernel::ecology::ResourceKind::Field`] inputs; the
//! orchard quantities and propagation records are compact generated state.

use std::collections::BTreeSet;

use hornvale_climate::{GeneratedClimate, ocean_current, prevailing_wind};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{NearestVertexIndex, Vertex, World, WorldTime};
use hornvale_terrain::GeneratedTerrain;

use crate::skyworld::{
    SkyFields, SkyFootprint, SkyMobility, SkyPosition, SkyTerritory, SkyWorld, SkyWorldConfig,
};

/// plumb: pending(wave-1)
const EXCHANGE_RINGS: u16 = 1;
/// plumb: pending(wave-1)
const ALIGNMENT_WEIGHT: f64 = 0.90;
/// plumb: pending(wave-1)
const VARIATION_WEIGHT: f64 = 0.10;
/// plumb: pending(wave-1)
const CYCLIC_PHASE_RADIANS_PER_DAY: f64 = 0.23;
/// plumb: pending(wave-1)
const STELLAR_PHASE_MULTIPLIER: f64 = 1.70;
/// plumb: pending(wave-1)
const ALTITUDE_MODULATION_FRACTION: f64 = 0.04;
/// plumb: pending(wave-1)
const MINIMUM_ALTITUDE_M: f64 = 1_000.0;

struct TrajectoryContext<'a> {
    world: &'a World,
    terrain: &'a GeneratedTerrain,
    climate: &'a GeneratedClimate,
    fields: &'a SkyFields,
    index: NearestVertexIndex,
}

/// What an ordered wind corridor carries.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SkyCorridorKind {
    /// Orchard seeds.
    Seeds,
    /// Fungal and floral spores.
    Spores,
    /// Free-floating sky plankton.
    Plankton,
    /// A route a mobile habitat or later traveller may follow.
    Route,
}

/// One ordered, sparse wind corridor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkyCorridor {
    /// Cargo represented by this corridor, in stable enum order.
    pub carries: Vec<SkyCorridorKind>,
    /// Surface projection of the corridor, in trajectory order.
    pub projected: Vec<Vertex>,
}

/// Sparse event kinds represented without lifecycle mutation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyEventKind {
    /// A productive plankton or flowering bloom.
    Bloom,
    /// A storm encounter.
    Storm,
    /// A possible collapse readout; this does not mutate lifecycle state.
    Collapse,
}

/// One sparse event record along a sampled trajectory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SkyEvent {
    /// Event kind.
    pub kind: SkyEventKind,
    /// Exact sampled time.
    pub time_slice: WorldTime,
    /// Surface projection at the event sample.
    pub surface: Vertex,
}

/// The three explicit bounded propagation channels.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SkyPropagation {
    /// Immediate local influence vertices, in ascending vertex order.
    pub local: Vec<Vertex>,
    /// Ordered wind corridors.
    pub corridors: Vec<SkyCorridor>,
    /// Sparse bloom, storm, or collapse records.
    pub events: Vec<SkyEvent>,
}

/// One selected propagation channel, or all channels together.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyPropagationDetail {
    /// Immediate local kernel only.
    Local,
    /// Wind corridors only.
    Corridors,
    /// Sparse events only.
    Events,
    /// Every explicit channel.
    All,
}

/// Temporal adjacency at one trajectory sample.
/// type-audit: bare-ok(identifier-text: lateral_territories), bare-ok(index: lateral_corridors)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkyAdjacency {
    /// Fixed land or ocean vertex directly below the sample.
    pub vertical: Vertex,
    /// Reachable territory ids, in ascending id order.
    pub lateral_territories: Vec<u32>,
    /// Reachable corridor indices owned by this territory, in ascending order.
    pub lateral_corridors: Vec<u16>,
}

/// One requested coarse trajectory sample.
#[derive(Clone, Debug, PartialEq)]
pub struct SkyTrajectorySample {
    /// Exact world-time slice represented by this sample.
    pub time_slice: WorldTime,
    /// Coarse position at this slice.
    pub position: SkyPosition,
    /// Physical body projection at this slice.
    pub physical: SkyFootprint,
    /// Exchange envelope at this slice.
    pub exchange: SkyFootprint,
    /// Ecological influence footprint at this slice.
    pub influence: SkyFootprint,
    /// Vertical and bounded lateral adjacency at this slice.
    pub adjacency: SkyAdjacency,
}

/// Look up one materialized sample without changing cache or draw state.
/// type-audit: bare-ok(identifier-text: territory_id)
pub fn trajectory_at(
    skyworld: &SkyWorld,
    territory_id: u32,
    time_slice: WorldTime,
) -> Option<&SkyTrajectorySample> {
    let territory = skyworld
        .territories
        .iter()
        .find(|territory| territory.id == territory_id)?;
    territory
        .trajectory
        .binary_search_by_key(&time_slice, |sample| sample.time_slice)
        .ok()
        .map(|index| &territory.trajectory[index])
}

/// Return only the selected explicit propagation channel.
/// type-audit: bare-ok(identifier-text: territory_id)
pub fn propagation_at(
    skyworld: &SkyWorld,
    territory_id: u32,
    detail: SkyPropagationDetail,
) -> Option<SkyPropagation> {
    let propagation = &skyworld
        .territories
        .iter()
        .find(|territory| territory.id == territory_id)?
        .influence;
    Some(match detail {
        SkyPropagationDetail::Local => SkyPropagation {
            local: propagation.local.clone(),
            ..SkyPropagation::default()
        },
        SkyPropagationDetail::Corridors => SkyPropagation {
            corridors: propagation.corridors.clone(),
            ..SkyPropagation::default()
        },
        SkyPropagationDetail::Events => SkyPropagation {
            events: propagation.events.clone(),
            ..SkyPropagation::default()
        },
        SkyPropagationDetail::All => propagation.clone(),
    })
}

pub(crate) fn derive_movement_and_propagation(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    fields: &SkyFields,
    config: &SkyWorldConfig,
    territories: &mut [SkyTerritory],
) {
    let context = TrajectoryContext {
        world,
        terrain,
        climate,
        fields,
        index: NearestVertexIndex::new(terrain.geosphere()),
    };
    for territory in territories.iter_mut() {
        territory.trajectory = derive_trajectory(&context, config, territory);
        if let Some(first) = territory.trajectory.first() {
            territory.physical = first.physical.clone();
            territory.exchange = first.exchange.clone();
            territory.influence.local = first.influence.projected.clone();
        }
        let projected: Vec<Vertex> = territory
            .trajectory
            .iter()
            .map(|sample| sample.position.surface)
            .fold(Vec::new(), |mut path, vertex| {
                if path.last() != Some(&vertex) {
                    path.push(vertex);
                }
                path
            });
        territory.influence.corridors = if projected.is_empty() {
            Vec::new()
        } else {
            vec![SkyCorridor {
                carries: vec![
                    SkyCorridorKind::Seeds,
                    SkyCorridorKind::Spores,
                    SkyCorridorKind::Plankton,
                    SkyCorridorKind::Route,
                ],
                projected,
            }]
        };
        territory.influence.events = sparse_events(world, territory);
    }
    derive_adjacency(territories);
}

fn derive_trajectory(
    context: &TrajectoryContext<'_>,
    config: &SkyWorldConfig,
    territory: &SkyTerritory,
) -> Vec<SkyTrajectorySample> {
    let mut samples = Vec::with_capacity(usize::from(config.trajectory_samples));
    let mut surface = territory.origin.surface;
    for sample_index in 0..config.trajectory_samples {
        let time_slice =
            WorldTime::from_ticks(i64::from(sample_index) * WorldTime::TICKS_PER_STD_DAY);
        if sample_index > 0 {
            surface = next_surface(context, territory, surface, time_slice);
        }
        let phase = time_slice.as_std_days() * CYCLIC_PHASE_RADIANS_PER_DAY;
        let forcing = forcing_modulation(context.fields, phase, territory.phenotype.mobility);
        let altitude_offset = territory.origin.altitude_m * ALTITUDE_MODULATION_FRACTION * forcing;
        let altitude_m = (territory.origin.altitude_m + altitude_offset).max(MINIMUM_ALTITUDE_M);
        let physical = footprint_with_size(
            context.terrain,
            surface,
            territory.physical.projected.len().max(1),
        );
        let exchange = expand_footprint(context.terrain, &physical, EXCHANGE_RINGS);
        let influence = expand_footprint(context.terrain, &exchange, config.propagation_radius);
        #[cfg(test)]
        crate::skyworld::record_trajectory_sample();
        samples.push(SkyTrajectorySample {
            time_slice,
            position: SkyPosition {
                surface,
                altitude_m,
            },
            physical,
            exchange,
            influence,
            adjacency: SkyAdjacency {
                vertical: surface,
                lateral_territories: Vec::new(),
                lateral_corridors: Vec::new(),
            },
        });
    }
    samples
}

fn next_surface(
    context: &TrajectoryContext<'_>,
    territory: &SkyTerritory,
    surface: Vertex,
    time_slice: WorldTime,
) -> Vertex {
    let geo = context.terrain.geosphere();
    let bands = context.climate.band_count();
    let wind = bands
        .map(|count| prevailing_wind(geo, surface, count))
        .unwrap_or([0.0; 3]);
    let current = bands
        .map(|count| {
            ocean_current(
                geo,
                &|vertex| context.terrain.is_ocean(vertex),
                surface,
                count,
            )
        })
        .unwrap_or([0.0; 3]);
    let driver = movement_driver(territory.phenotype.mobility, wind, current, time_slice);
    let here = geo.position(surface);
    let mut candidates: Vec<Vertex> = geo.neighbors(surface).to_vec();
    candidates.sort();
    let mut best = surface;
    let mut best_score = f64::NEG_INFINITY;
    for candidate in candidates {
        let there = geo.position(candidate);
        let direction = [there[0] - here[0], there[1] - here[1], there[2] - here[2]];
        let key = format!(
            "territory/{}/slice/{}/candidate/{}",
            territory.id,
            time_slice.ticks(),
            candidate.0
        );
        let variation = context
            .world
            .seed
            .derive(crate::streams::SKYWORLD_MOVEMENT)
            .derive(StreamLabel::dynamic(&key))
            .stream()
            .next_f64();
        let phase = time_slice.as_std_days() * CYCLIC_PHASE_RADIANS_PER_DAY;
        let modulation = forcing_modulation(context.fields, phase, territory.phenotype.mobility);
        let score = dot(direction, driver) * ALIGNMENT_WEIGHT
            + variation * VARIATION_WEIGHT * (1.0 + modulation);
        if score.total_cmp(&best_score).is_gt()
            || (score.total_cmp(&best_score).is_eq() && candidate < best)
        {
            best = candidate;
            best_score = score;
        }
    }
    context.index.nearest_to_position(geo, geo.position(best))
}

fn movement_driver(
    mobility: SkyMobility,
    wind: [f64; 3],
    current: [f64; 3],
    time_slice: WorldTime,
) -> [f64; 3] {
    let phase = time_slice.as_std_days() * CYCLIC_PHASE_RADIANS_PER_DAY;
    match mobility {
        SkyMobility::Drifting => wind,
        SkyMobility::Cyclic => scale(wind, hornvale_kernel::math::cos(phase)),
        SkyMobility::CurrentFollowing => nonzero_or(current, wind),
        SkyMobility::FieldFollowing => scale(wind, 1.0 + hornvale_kernel::math::sin(phase)),
        SkyMobility::Mixed => add(wind, current),
    }
}

fn forcing_modulation(fields: &SkyFields, phase: f64, mobility: SkyMobility) -> f64 {
    let mobility_factor = match mobility {
        SkyMobility::Drifting => 0.2,
        SkyMobility::Cyclic => 1.0,
        SkyMobility::CurrentFollowing => 0.4,
        SkyMobility::FieldFollowing => 0.6,
        SkyMobility::Mixed => 0.8,
    };
    let lunar = fields.lunar_forcing * hornvale_kernel::math::sin(phase);
    let stellar =
        fields.stellar_forcing * hornvale_kernel::math::sin(phase * STELLAR_PHASE_MULTIPLIER);
    (lunar + stellar) * mobility_factor
}

fn footprint_with_size(
    terrain: &GeneratedTerrain,
    center: Vertex,
    target_size: usize,
) -> SkyFootprint {
    let mut projected = BTreeSet::from([center]);
    let mut frontier = vec![center];
    let mut cursor = 0;
    while projected.len() < target_size && cursor < frontier.len() {
        let current = frontier[cursor];
        cursor += 1;
        for &neighbor in terrain.geosphere().neighbors(current) {
            if projected.insert(neighbor) {
                frontier.push(neighbor);
                if projected.len() == target_size {
                    break;
                }
            }
        }
    }
    SkyFootprint {
        projected: projected.into_iter().collect(),
    }
}

fn expand_footprint(
    terrain: &GeneratedTerrain,
    footprint: &SkyFootprint,
    rings: u16,
) -> SkyFootprint {
    let mut projected: BTreeSet<Vertex> = footprint.projected.iter().copied().collect();
    let mut frontier = footprint.projected.clone();
    for _ in 0..rings {
        let mut next = Vec::new();
        for current in frontier {
            for &neighbor in terrain.geosphere().neighbors(current) {
                if projected.insert(neighbor) {
                    next.push(neighbor);
                }
            }
        }
        frontier = next;
        if frontier.is_empty() {
            break;
        }
    }
    SkyFootprint {
        projected: projected.into_iter().collect(),
    }
}

fn sparse_events(world: &World, territory: &SkyTerritory) -> Vec<SkyEvent> {
    let Some(sample) = territory.trajectory.get(territory.trajectory.len() / 2) else {
        return Vec::new();
    };
    let kind = match (u64::from(territory.id) + world.seed.0) % 3 {
        0 => SkyEventKind::Bloom,
        1 => SkyEventKind::Storm,
        _ => SkyEventKind::Collapse,
    };
    vec![SkyEvent {
        kind,
        time_slice: sample.time_slice,
        surface: sample.position.surface,
    }]
}

fn derive_adjacency(territories: &mut [SkyTerritory]) {
    let snapshots: Vec<Vec<(Vertex, BTreeSet<Vertex>)>> = territories
        .iter()
        .map(|territory| {
            territory
                .trajectory
                .iter()
                .map(|sample| {
                    (
                        sample.position.surface,
                        sample.influence.projected.iter().copied().collect(),
                    )
                })
                .collect()
        })
        .collect();
    let ids: Vec<u32> = territories.iter().map(|territory| territory.id).collect();

    for (territory_index, territory) in territories.iter_mut().enumerate() {
        for (sample_index, sample) in territory.trajectory.iter_mut().enumerate() {
            let influence = &snapshots[territory_index][sample_index].1;
            sample.adjacency.lateral_territories = snapshots
                .iter()
                .enumerate()
                .filter_map(|(other_index, other_samples)| {
                    if other_index == territory_index {
                        return None;
                    }
                    let other = other_samples.get(sample_index)?;
                    influence.contains(&other.0).then_some(ids[other_index])
                })
                .collect();
            sample.adjacency.lateral_territories.sort();
            sample.adjacency.lateral_corridors = territory
                .influence
                .corridors
                .iter()
                .enumerate()
                .filter_map(|(index, corridor)| {
                    corridor
                        .projected
                        .contains(&sample.position.surface)
                        .then_some(index as u16)
                })
                .collect();
        }
    }
}

fn dot(left: [f64; 3], right: [f64; 3]) -> f64 {
    left[0] * right[0] + left[1] * right[1] + left[2] * right[2]
}

fn scale(vector: [f64; 3], factor: f64) -> [f64; 3] {
    [vector[0] * factor, vector[1] * factor, vector[2] * factor]
}

fn add(left: [f64; 3], right: [f64; 3]) -> [f64; 3] {
    [left[0] + right[0], left[1] + right[1], left[2] + right[2]]
}

fn nonzero_or(preferred: [f64; 3], fallback: [f64; 3]) -> [f64; 3] {
    if preferred == [0.0; 3] {
        fallback
    } else {
        preferred
    }
}
