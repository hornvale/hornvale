//! The Staple D3B fixed-roster gradient-sufficiency falsifier.

#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_astronomy::SkyPins;
use hornvale_history::record::Function;
use hornvale_kernel::{KindId, Seed, Vertex, VertexMap, World, quantize};
use hornvale_species::{BiomeAffinity, BiosphereTraits, HabitatRealm, SocialForm};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::delve_seating::{Seating, seating_for};
use hornvale_worldgen::{
    BakeId, D3bCapacityBand, D3bProjectionSignature, D3bSourceSignature, D3bTernaryBand,
    DiagnosticSubsistenceWitness, ExchangeCensus, ExchangeTreatment, ExchangeTreatmentBuild,
    History, SettlementPins, WorldComponents, biome_class, build_world_with_exchange_treatment,
    census, climate_from, d3b_projection_signature, d3b_source_signature, emit_history,
    per_species_capacity, sky_of, terrain_of,
};

const PROBE_WORLD_DENOMINATOR: usize = 200;
const PROBE_SEEDS: std::ops::RangeInclusive<u64> = 1..=200;
const FIXED_COMPLEMENTARY_DEMAND: [f64; 2] = [0.5, 0.5];
/// The private D2 `PHASES_PER_YEAR`, mirrored so an incomplete seasonal
/// observation cannot masquerade as a complete projection.
const D2_PHASES_PER_EPOCH: u32 = 12;

#[derive(Clone, Copy, Debug, PartialEq)]
struct RawSourceVector {
    surplus: f64,
    river_access: f64,
    capacity: f64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct SourceObservation {
    community: BakeId,
    site: Vertex,
    raw: RawSourceVector,
}

impl SourceObservation {
    fn signature(self) -> Option<D3bSourceSignature> {
        d3b_source_signature(self.raw.surplus, self.raw.river_access, self.raw.capacity)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct ProjectionObservation {
    witness: DiagnosticSubsistenceWitness,
    demand: [f64; 2],
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct RawProjectionVector {
    coverage: [f64; 2],
    shortfall: [f64; 2],
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct TypedAccess {
    attempts: [u64; 2],
    proposed: [u64; 2],
    accepted: [u64; 2],
    settled: [u64; 2],
    partial: [u64; 2],
    refused: [u64; 2],
    impossible: [u64; 2],
}

impl TypedAccess {
    fn from_witness(witness: DiagnosticSubsistenceWitness) -> Self {
        Self {
            attempts: witness.attempts,
            proposed: witness.proposed,
            accepted: witness.accepted,
            settled: witness.settled,
            partial: witness.partial,
            refused: witness.refused,
            impossible: witness.impossible,
        }
    }

    fn add_assign(&mut self, other: Self) {
        for index in 0..2 {
            self.attempts[index] += other.attempts[index];
            self.proposed[index] += other.proposed[index];
            self.accepted[index] += other.accepted[index];
            self.settled[index] += other.settled[index];
            self.partial[index] += other.partial[index];
            self.refused[index] += other.refused[index];
            self.impossible[index] += other.impossible[index];
        }
    }

    fn is_coherent(self) -> bool {
        (0..2).all(|index| {
            self.proposed[index] == self.attempts[index]
                && self.accepted[index] + self.refused[index] + self.impossible[index]
                    == self.attempts[index]
                && self.settled[index] + self.partial[index] == self.accepted[index]
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LiveUnit {
    community: BakeId,
    site: Vertex,
}

#[derive(Clone, Debug, PartialEq)]
struct SeedInput {
    seed: u64,
    denominator: u64,
    live_units: Vec<LiveUnit>,
    sources: Vec<SourceObservation>,
    projections: Vec<ProjectionObservation>,
    treatment: ExchangeTreatment,
    exchange: ExchangeCensus,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum UnderpoweredReason {
    Empty,
    FewerThanTwoJoinedUnits,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SeedVerdict {
    Underpowered(UnderpoweredReason),
    DisabledTreatment,
    JoinFailure,
    ConservationFailure,
    InvalidMeasurement,
    IncompleteMeasurement,
    NoUsableSourceGradient,
    ProjectionCollapse,
    MeasurementSaturation,
    Cleared,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct IntegrityBranches {
    denominator_mismatch: bool,
    duplicate_live_units: Vec<BakeId>,
    missing_sources: Vec<BakeId>,
    duplicate_sources: Vec<BakeId>,
    orphan_sources: Vec<BakeId>,
    missing_projections: Vec<BakeId>,
    duplicate_projections: Vec<BakeId>,
    orphan_projections: Vec<BakeId>,
    source_site_mismatches: Vec<BakeId>,
    projection_site_mismatches: Vec<BakeId>,
    invalid_sources: Vec<BakeId>,
    zero_demand_units: Vec<BakeId>,
    phase_incomplete_units: Vec<BakeId>,
    invalid_projection_units: Vec<BakeId>,
    incoherent_access_units: Vec<BakeId>,
    non_finite_conservation_resources: Vec<usize>,
    non_zero_conservation_resources: Vec<usize>,
    disabled_treatment: bool,
}

impl IntegrityBranches {
    fn has_join_failure(&self) -> bool {
        self.denominator_mismatch
            || !self.duplicate_live_units.is_empty()
            || !self.missing_sources.is_empty()
            || !self.duplicate_sources.is_empty()
            || !self.orphan_sources.is_empty()
            || !self.missing_projections.is_empty()
            || !self.duplicate_projections.is_empty()
            || !self.orphan_projections.is_empty()
            || !self.source_site_mismatches.is_empty()
            || !self.projection_site_mismatches.is_empty()
    }

    fn has_invalid_measurement(&self) -> bool {
        !self.invalid_sources.is_empty()
            || !self.zero_demand_units.is_empty()
            || !self.invalid_projection_units.is_empty()
            || !self.incoherent_access_units.is_empty()
    }

    fn has_incomplete_measurement(&self) -> bool {
        !self.phase_incomplete_units.is_empty()
    }

    fn has_conservation_failure(&self) -> bool {
        !self.non_finite_conservation_resources.is_empty()
            || !self.non_zero_conservation_resources.is_empty()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct SourceOccupancy {
    surplus_bands: BTreeSet<D3bTernaryBand>,
    river_bands: BTreeSet<D3bTernaryBand>,
    capacity_bands: BTreeSet<D3bCapacityBand>,
    signatures: BTreeMap<D3bSourceSignature, usize>,
    singleton_signatures: usize,
}

impl SourceOccupancy {
    fn observe(&mut self, signature: D3bSourceSignature) {
        self.surplus_bands.insert(signature.surplus);
        self.river_bands.insert(signature.river_access);
        self.capacity_bands.insert(signature.capacity);
        *self.signatures.entry(signature).or_default() += 1;
    }

    fn finish(&mut self) {
        self.singleton_signatures = self
            .signatures
            .values()
            .filter(|&&count| count == 1)
            .count();
    }

    fn passes_guard(&self) -> bool {
        let mut surplus = BTreeSet::new();
        let mut river = BTreeSet::new();
        let mut capacity = BTreeSet::new();
        let mut supported_signatures = 0;
        for (signature, occupants) in &self.signatures {
            if *occupants <= 1 {
                continue;
            }
            supported_signatures += 1;
            surplus.insert(signature.surplus);
            river.insert(signature.river_access);
            capacity.insert(signature.capacity);
        }
        let varied_axes = [surplus.len(), river.len(), capacity.len()]
            .into_iter()
            .filter(|&occupied| occupied > 1)
            .count();
        varied_axes >= 2 && supported_signatures >= 2
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct RawProjectionSpread {
    coverage_min: [f64; 2],
    coverage_max: [f64; 2],
    shortfall_min: [f64; 2],
    shortfall_max: [f64; 2],
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ProjectionOccupancy {
    signatures: BTreeMap<D3bProjectionSignature, usize>,
    committed_vectors: BTreeMap<[u64; 4], usize>,
    raw_vectors: Vec<RawProjectionVector>,
    raw_spread: Option<RawProjectionSpread>,
    access: TypedAccess,
}

impl ProjectionOccupancy {
    fn observe(
        &mut self,
        raw: RawProjectionVector,
        signature: D3bProjectionSignature,
        access: TypedAccess,
    ) {
        *self.signatures.entry(signature).or_default() += 1;
        let committed = [
            quantize(raw.coverage[0]).to_bits(),
            quantize(raw.coverage[1]).to_bits(),
            quantize(raw.shortfall[0]).to_bits(),
            quantize(raw.shortfall[1]).to_bits(),
        ];
        *self.committed_vectors.entry(committed).or_default() += 1;
        self.raw_vectors.push(raw);
        self.access.add_assign(access);
    }

    fn finish(&mut self) {
        let Some(first) = self.raw_vectors.first().copied() else {
            return;
        };
        let mut spread = RawProjectionSpread {
            coverage_min: first.coverage,
            coverage_max: first.coverage,
            shortfall_min: first.shortfall,
            shortfall_max: first.shortfall,
        };
        for raw in &self.raw_vectors[1..] {
            for index in 0..2 {
                spread.coverage_min[index] = spread.coverage_min[index].min(raw.coverage[index]);
                spread.coverage_max[index] = spread.coverage_max[index].max(raw.coverage[index]);
                spread.shortfall_min[index] = spread.shortfall_min[index].min(raw.shortfall[index]);
                spread.shortfall_max[index] = spread.shortfall_max[index].max(raw.shortfall[index]);
            }
        }
        self.raw_spread = Some(spread);
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct JoinedUnit {
    community: BakeId,
    site: Vertex,
    source_raw: RawSourceVector,
    source_signature: D3bSourceSignature,
    projection_raw: RawProjectionVector,
    projection_signature: D3bProjectionSignature,
    access: TypedAccess,
}

#[derive(Clone, Debug, PartialEq)]
struct SeedReport {
    seed: u64,
    denominator: u64,
    live_units: usize,
    joined_units: Vec<JoinedUnit>,
    source: SourceOccupancy,
    projection: ProjectionOccupancy,
    branches: IntegrityBranches,
    verdict: SeedVerdict,
}

fn summarize_seed(input: &SeedInput) -> SeedReport {
    let mut branches = IntegrityBranches {
        denominator_mismatch: input.denominator != input.live_units.len() as u64,
        disabled_treatment: input.treatment != ExchangeTreatment::Enabled,
        ..IntegrityBranches::default()
    };
    for (resource, residual) in input.exchange.conservation_residuals.iter().enumerate() {
        if !residual.is_finite() {
            branches.non_finite_conservation_resources.push(resource);
        } else if *residual != 0.0 {
            branches.non_zero_conservation_resources.push(resource);
        }
    }

    let mut live = BTreeMap::<BakeId, Vec<LiveUnit>>::new();
    let mut sources = BTreeMap::<BakeId, Vec<SourceObservation>>::new();
    let mut projections = BTreeMap::<BakeId, Vec<ProjectionObservation>>::new();
    for unit in &input.live_units {
        live.entry(unit.community).or_default().push(*unit);
    }
    for source in &input.sources {
        sources.entry(source.community).or_default().push(*source);
    }
    for projection in &input.projections {
        projections
            .entry(projection.witness.community)
            .or_default()
            .push(*projection);
    }

    branches.duplicate_live_units = live
        .iter()
        .filter_map(|(&community, units)| (units.len() != 1).then_some(community))
        .collect();
    branches.duplicate_sources = sources
        .iter()
        .filter_map(|(&community, rows)| (rows.len() != 1).then_some(community))
        .collect();
    branches.duplicate_projections = projections
        .iter()
        .filter_map(|(&community, rows)| (rows.len() != 1).then_some(community))
        .collect();
    branches.orphan_sources = sources
        .keys()
        .filter(|community| !live.contains_key(community))
        .copied()
        .collect();
    branches.orphan_projections = projections
        .keys()
        .filter(|community| !live.contains_key(community))
        .copied()
        .collect();

    let mut joined_units = Vec::new();
    let mut source_occupancy = SourceOccupancy::default();
    let mut projection_occupancy = ProjectionOccupancy::default();
    for (&community, live_rows) in &live {
        if live_rows.len() != 1 {
            continue;
        }
        let unit = live_rows[0];
        let Some(source_rows) = sources.get(&community) else {
            branches.missing_sources.push(community);
            continue;
        };
        let Some(projection_rows) = projections.get(&community) else {
            branches.missing_projections.push(community);
            continue;
        };
        if source_rows.len() != 1 || projection_rows.len() != 1 {
            continue;
        }
        let source = source_rows[0];
        let projection = projection_rows[0];
        if source.site != unit.site {
            branches.source_site_mismatches.push(community);
            continue;
        }
        if projection.witness.site != unit.site {
            branches.projection_site_mismatches.push(community);
            continue;
        }
        let Some(source_signature) = source.signature() else {
            branches.invalid_sources.push(community);
            continue;
        };
        if projection.demand.contains(&0.0) {
            branches.zero_demand_units.push(community);
            continue;
        }
        if projection
            .demand
            .iter()
            .any(|demand| !demand.is_finite() || *demand < 0.0)
        {
            branches.invalid_projection_units.push(community);
            continue;
        }
        if projection.witness.phase_count == 0 {
            branches.phase_incomplete_units.push(community);
            continue;
        }
        if projection.witness.phase_count % D2_PHASES_PER_EPOCH != 0 {
            branches.invalid_projection_units.push(community);
            continue;
        }
        let projection_raw = RawProjectionVector {
            coverage: projection.witness.coverage,
            shortfall: projection.witness.shortfall,
        };
        if projection_raw
            .coverage
            .iter()
            .chain(&projection_raw.shortfall)
            .any(|ratio| !ratio.is_finite() || !(0.0..=1.0).contains(ratio))
        {
            branches.invalid_projection_units.push(community);
            continue;
        }
        if (0..2)
            .any(|index| projection_raw.coverage[index] != 1.0 - projection_raw.shortfall[index])
        {
            branches.invalid_projection_units.push(community);
            continue;
        }
        let Some(projection_signature) = d3b_projection_signature(projection_raw.coverage) else {
            branches.invalid_projection_units.push(community);
            continue;
        };
        let access = TypedAccess::from_witness(projection.witness);
        if !access.is_coherent() {
            branches.incoherent_access_units.push(community);
            continue;
        }

        let joined = JoinedUnit {
            community,
            site: unit.site,
            source_raw: source.raw,
            source_signature,
            projection_raw,
            projection_signature,
            access,
        };
        source_occupancy.observe(source_signature);
        projection_occupancy.observe(projection_raw, projection_signature, access);
        joined_units.push(joined);
    }

    source_occupancy.finish();
    projection_occupancy.finish();
    let verdict = if branches.disabled_treatment {
        SeedVerdict::DisabledTreatment
    } else if branches.has_join_failure() {
        SeedVerdict::JoinFailure
    } else if branches.has_conservation_failure() {
        SeedVerdict::ConservationFailure
    } else if branches.has_invalid_measurement() {
        SeedVerdict::InvalidMeasurement
    } else if branches.has_incomplete_measurement() {
        SeedVerdict::IncompleteMeasurement
    } else if input.denominator == 0 {
        SeedVerdict::Underpowered(UnderpoweredReason::Empty)
    } else if input.denominator < 2 || joined_units.len() < 2 {
        SeedVerdict::Underpowered(UnderpoweredReason::FewerThanTwoJoinedUnits)
    } else if !source_occupancy.passes_guard() {
        SeedVerdict::NoUsableSourceGradient
    } else if projection_occupancy.committed_vectors.len() == 1 {
        SeedVerdict::ProjectionCollapse
    } else if projection_occupancy.signatures.len() == 1 {
        SeedVerdict::MeasurementSaturation
    } else {
        SeedVerdict::Cleared
    };

    SeedReport {
        seed: input.seed,
        denominator: input.denominator,
        live_units: input.live_units.len(),
        joined_units,
        source: source_occupancy,
        projection: projection_occupancy,
        branches,
        verdict,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RosterVerdict {
    NoUsableSourceGradient,
    ProjectionCollapse,
    MeasurementSaturation,
    Cleared,
    MixedOrUnderpowered,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct PooledTotals {
    denominator: u64,
    joined_units: usize,
    source: SourceOccupancy,
    projection: ProjectionOccupancy,
}

#[derive(Clone, Debug, PartialEq)]
struct RosterReport {
    seeds: Vec<SeedReport>,
    pooled: PooledTotals,
    verdict: RosterVerdict,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RosterContractError {
    WrongSeedCount {
        actual: usize,
    },
    UnexpectedSeed {
        index: usize,
        expected: u64,
        actual: u64,
    },
}

fn roster_verdict(seeds: &[SeedReport]) -> RosterVerdict {
    if seeds
        .iter()
        .all(|report| report.verdict == SeedVerdict::NoUsableSourceGradient)
    {
        RosterVerdict::NoUsableSourceGradient
    } else if seeds
        .iter()
        .all(|report| report.verdict == SeedVerdict::ProjectionCollapse)
    {
        RosterVerdict::ProjectionCollapse
    } else if seeds
        .iter()
        .all(|report| report.verdict == SeedVerdict::MeasurementSaturation)
    {
        RosterVerdict::MeasurementSaturation
    } else if seeds
        .iter()
        .all(|report| report.verdict == SeedVerdict::Cleared)
    {
        RosterVerdict::Cleared
    } else {
        RosterVerdict::MixedOrUnderpowered
    }
}

fn pooled_totals(seeds: &[SeedReport]) -> PooledTotals {
    let mut pooled = PooledTotals::default();
    for seed in seeds {
        pooled.denominator += seed.denominator;
        for joined in &seed.joined_units {
            pooled.source.observe(joined.source_signature);
            pooled.projection.observe(
                joined.projection_raw,
                joined.projection_signature,
                joined.access,
            );
            pooled.joined_units += 1;
        }
    }
    pooled.source.finish();
    pooled.projection.finish();
    pooled
}

fn summarize_fixed_roster(inputs: &[SeedInput]) -> Result<RosterReport, RosterContractError> {
    if inputs.len() != PROBE_WORLD_DENOMINATOR {
        return Err(RosterContractError::WrongSeedCount {
            actual: inputs.len(),
        });
    }
    for (index, (input, expected)) in inputs.iter().zip(PROBE_SEEDS).enumerate() {
        if input.seed != expected {
            return Err(RosterContractError::UnexpectedSeed {
                index,
                expected,
                actual: input.seed,
            });
        }
    }

    let seeds: Vec<_> = inputs.iter().map(summarize_seed).collect();
    Ok(RosterReport {
        pooled: pooled_totals(&seeds),
        verdict: roster_verdict(&seeds),
        seeds,
    })
}

/// Reproduce the bake's present-era, per-people capacity field through the
/// same public path and delve-seating multiplier used by the Hidage probe.
/// Surplus and river access are read as continuous site values before any
/// `Function`, subsistence label, or portfolio can enter the observation.
fn source_observations(
    world: &World,
    history: &History,
    wc: &WorldComponents,
) -> Vec<SourceObservation> {
    let terrain = terrain_of(world).expect("fixed probe terrain re-derives");
    let climate = climate_from(world, &terrain).expect("fixed probe climate re-derives");
    let geo = terrain.geosphere();
    let sky = sky_of(world).expect("fixed probe sky re-derives");
    let generated = sky.generated();
    let system = generated.system();
    let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity_deg = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning {
                day_std: day.as_std_days(),
            }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let peoples: Vec<KindId> = wc
        .biosphere
        .iter()
        .filter(|(_, traits)| traits.social_form == SocialForm::Settled)
        .map(|(kind, _)| *kind)
        .collect();
    let species_biosphere: Vec<&BiosphereTraits> = peoples
        .iter()
        .map(|kind| {
            wc.biosphere
                .get(kind)
                .expect("a settling people has biosphere traits")
        })
        .collect();
    let species_realm: Vec<HabitatRealm> = peoples
        .iter()
        .map(|kind| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect();
    let species_affinity: Vec<Option<BiomeAffinity>> = peoples
        .iter()
        .map(|kind| wc.biome_affinity.get(kind).cloned())
        .collect();
    let capacities = per_species_capacity(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &species_biosphere,
        &species_realm,
        &species_affinity,
    );
    let niches = hornvale_species::environment_niche_registry();
    let capacity_fields: BTreeMap<KindId, VertexMap<f64>> = peoples
        .iter()
        .enumerate()
        .map(|(index, &people)| {
            let (tag, capacity) = &capacities[index];
            assert_eq!(
                *tag as usize, index,
                "per_species_capacity tags must match the bake's people order"
            );
            let seating = match species_realm[index] {
                HabitatRealm::Surface => Seating::all_surface(geo),
                HabitatRealm::Subterranean => seating_for(geo, &terrain, niches.get(&people)),
            };
            let field = VertexMap::from_fn(geo, |vertex| {
                capacity.at(vertex) * seating.multiplier.get(vertex)
            });
            (people, field)
        })
        .collect();
    let water_kind = VertexMap::from_fn(geo, |vertex| terrain.water_kind_at(vertex));
    let river_access =
        hornvale_terrain::river_proximity(geo, &water_kind, hornvale_terrain::RIVER_REACH);

    let mut observations: Vec<_> = history
        .records
        .iter()
        .filter(|record| record.core.is_alive())
        .map(|record| {
            let site = record.core.site;
            let class = biome_class(climate.biome_at(site));
            let surplus =
                (hornvale_culture::fertility(class) * climate.moisture_at(site)).clamp(0.0, 1.0);
            let capacity = capacity_fields
                .get(&record.core.people)
                .unwrap_or_else(|| {
                    panic!(
                        "live community {:?} has no capacity field for {:?}",
                        record.community, record.core.people
                    )
                })
                .get(site);
            SourceObservation {
                community: record.community,
                site,
                raw: RawSourceVector {
                    surplus,
                    river_access: *river_access.get(site),
                    capacity: *capacity,
                },
            }
        })
        .collect();
    observations.sort_by_key(|observation| observation.community);
    observations
}

fn seed_input_from_history(
    seed: u64,
    world: &World,
    history: &History,
    exchange: ExchangeCensus,
    wc: &WorldComponents,
) -> SeedInput {
    let sources = source_observations(world, history, wc);
    let live_units = history
        .records
        .iter()
        .filter(|record| record.core.is_alive())
        .map(|record| LiveUnit {
            community: record.community,
            site: record.core.site,
        })
        .collect();
    let projections = history
        .diagnostic_subsistence
        .iter()
        .copied()
        .map(|witness| ProjectionObservation {
            witness,
            demand: FIXED_COMPLEMENTARY_DEMAND,
        })
        .collect();
    SeedInput {
        seed,
        denominator: census(history).alive_at_now,
        live_units,
        sources,
        projections,
        treatment: ExchangeTreatment::Enabled,
        exchange,
    }
}

fn seed_input_from_build(
    seed: u64,
    built: &ExchangeTreatmentBuild,
    wc: &WorldComponents,
) -> SeedInput {
    seed_input_from_history(seed, &built.world, &built.history, built.exchange, wc)
}

fn enabled_build(seed: u64, wc: &WorldComponents) -> ExchangeTreatmentBuild {
    build_world_with_exchange_treatment(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        ExchangeTreatment::Enabled,
    )
    .expect("fixed enabled-treatment seed builds to settlements")
}

fn enabled_seed_input(seed: u64, wc: &WorldComponents) -> SeedInput {
    let built = enabled_build(seed, wc);
    seed_input_from_build(seed, &built, wc)
}

fn emitted_ledger_bytes(history: &History) -> Vec<u8> {
    let mut world = World::new(Seed(42));
    hornvale_history::register_concepts(&mut world.registry).expect("history concepts register");
    hornvale_settlement::register_concepts(&mut world.registry)
        .expect("settlement concepts register");
    hornvale_epidemiology::register_concepts(&mut world.registry)
        .expect("epidemiology concepts register");
    emit_history(&mut world, history).expect("history emits");
    serde_json::to_vec(&world.ledger).expect("ledger serializes")
}

#[cfg(test)]
fn witness(community: u64, site: u32, coverage: [f64; 2]) -> ProjectionObservation {
    ProjectionObservation {
        witness: DiagnosticSubsistenceWitness {
            community: BakeId(community),
            site: Vertex(site),
            phase_count: 12,
            coverage,
            shortfall: coverage.map(|ratio| 1.0 - ratio),
            attempts: [1, 1],
            proposed: [1, 1],
            accepted: [1, 1],
            settled: [1, 1],
            partial: [0, 0],
            refused: [0, 0],
            impossible: [0, 0],
        },
        demand: FIXED_COMPLEMENTARY_DEMAND,
    }
}

#[cfg(test)]
fn source(
    community: u64,
    site: u32,
    surplus: f64,
    river_access: f64,
    capacity: f64,
) -> SourceObservation {
    SourceObservation {
        community: BakeId(community),
        site: Vertex(site),
        raw: RawSourceVector {
            surplus,
            river_access,
            capacity,
        },
    }
}

#[cfg(test)]
fn two_unit_input(sources: [SourceObservation; 2]) -> SeedInput {
    SeedInput {
        seed: 1,
        denominator: 2,
        live_units: vec![
            LiveUnit {
                community: BakeId(1),
                site: Vertex(10),
            },
            LiveUnit {
                community: BakeId(2),
                site: Vertex(20),
            },
        ],
        sources: sources.into(),
        projections: vec![witness(1, 10, [0.25, 0.75]), witness(2, 20, [0.75, 0.25])],
        treatment: ExchangeTreatment::Enabled,
        exchange: ExchangeCensus::default(),
    }
}

#[cfg(test)]
fn varied_source_input(coverage: [[f64; 2]; 2]) -> SeedInput {
    let mut input = two_unit_input([
        source(1, 10, 0.2, 0.0, 100.0),
        source(2, 20, 0.2, 0.0, 100.0),
    ]);
    input.denominator = 4;
    input.live_units.extend([
        LiveUnit {
            community: BakeId(3),
            site: Vertex(30),
        },
        LiveUnit {
            community: BakeId(4),
            site: Vertex(40),
        },
    ]);
    input.sources.extend([
        source(3, 30, 0.8, 1.0, 250.0),
        source(4, 40, 0.8, 1.0, 250.0),
    ]);
    input.projections = vec![
        witness(1, 10, coverage[0]),
        witness(2, 20, coverage[0]),
        witness(3, 30, coverage[1]),
        witness(4, 40, coverage[1]),
    ];
    input
}

#[test]
fn variation_on_only_one_source_axis_is_no_usable_gradient() {
    let input = two_unit_input([
        source(1, 10, 0.2, 0.5, 175.0),
        source(2, 20, 0.8, 0.5, 175.0),
    ]);

    assert_eq!(
        summarize_seed(&input).verdict,
        SeedVerdict::NoUsableSourceGradient
    );
}

#[test]
fn a_singleton_source_signature_cannot_carry_the_positive_guard() {
    let input = SeedInput {
        seed: 1,
        denominator: 3,
        live_units: vec![
            LiveUnit {
                community: BakeId(1),
                site: Vertex(10),
            },
            LiveUnit {
                community: BakeId(2),
                site: Vertex(20),
            },
            LiveUnit {
                community: BakeId(3),
                site: Vertex(30),
            },
        ],
        sources: vec![
            source(1, 10, 0.2, 0.0, 100.0),
            source(2, 20, 0.2, 0.0, 100.0),
            source(3, 30, 0.8, 1.0, 250.0),
        ],
        projections: vec![
            witness(1, 10, [0.0, 1.0]),
            witness(2, 20, [0.0, 1.0]),
            witness(3, 30, [1.0, 0.0]),
        ],
        treatment: ExchangeTreatment::Enabled,
        exchange: ExchangeCensus::default(),
    };
    let report = summarize_seed(&input);

    assert_eq!(report.source.singleton_signatures, 1);
    assert_eq!(report.verdict, SeedVerdict::NoUsableSourceGradient);
}

#[test]
fn singleton_only_axis_variation_cannot_complete_the_source_guard() {
    let mut input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    input.sources[2] = source(3, 30, 0.8, 0.0, 100.0);
    input.sources[3] = source(4, 40, 0.8, 0.0, 100.0);
    input.denominator = 5;
    input.live_units.push(LiveUnit {
        community: BakeId(5),
        site: Vertex(50),
    });
    input.sources.push(source(5, 50, 0.8, 1.0, 250.0));
    input.projections.push(witness(5, 50, [0.25, 0.75]));
    let report = summarize_seed(&input);

    assert_eq!(report.source.singleton_signatures, 1);
    assert_eq!(report.verdict, SeedVerdict::NoUsableSourceGradient);
}

#[test]
fn equal_committed_projection_vectors_are_projection_collapse() {
    let input = varied_source_input([[0.5, 0.5], [0.5, 0.5]]);
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::ProjectionCollapse);
    assert_eq!(report.projection.signatures.len(), 1);
    assert_eq!(report.projection.committed_vectors.len(), 1);
    assert_eq!(report.projection.raw_vectors.len(), 4);
}

#[test]
fn subquantum_ordering_differences_cannot_clear_equal_committed_vectors() {
    let next_up = f64::from_bits(0.5_f64.to_bits() + 1);
    let input = varied_source_input([[0.5, next_up], [next_up, 0.5]]);
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::ProjectionCollapse);
    assert_eq!(report.projection.signatures.len(), 2);
    assert_eq!(report.projection.committed_vectors.len(), 1);
    assert_eq!(report.projection.raw_vectors.len(), 4);
}

#[test]
fn within_signature_raw_spread_is_measurement_saturation_not_collapse() {
    let input = varied_source_input([[0.25, 0.75], [0.4, 0.8]]);
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::MeasurementSaturation);
    assert_eq!(report.projection.signatures.len(), 1);
    assert_eq!(report.projection.committed_vectors.len(), 2);
    assert_eq!(
        report.projection.raw_spread,
        Some(RawProjectionSpread {
            coverage_min: [0.25, 0.75],
            coverage_max: [0.4, 0.8],
            shortfall_min: [0.6, 0.199_999_999_999_999_96],
            shortfall_max: [0.75, 0.25],
        })
    );
}

#[test]
fn varied_structural_projection_clears_without_pooled_or_label_weighting() {
    let input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::Cleared);
    assert_eq!(report.denominator, 4);
    assert_eq!(report.live_units, 4);
    assert_eq!(report.joined_units.len(), 4);
    assert_eq!(report.source.signatures.len(), 2);
    assert_eq!(report.projection.signatures.len(), 2);
}

#[test]
fn empty_and_singleton_seeds_are_explicitly_underpowered() {
    let empty = SeedInput {
        seed: 1,
        denominator: 0,
        live_units: Vec::new(),
        sources: Vec::new(),
        projections: Vec::new(),
        treatment: ExchangeTreatment::Enabled,
        exchange: ExchangeCensus::default(),
    };
    assert_eq!(
        summarize_seed(&empty).verdict,
        SeedVerdict::Underpowered(UnderpoweredReason::Empty)
    );

    let mut singleton = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    singleton.denominator = 1;
    singleton.live_units.truncate(1);
    singleton.sources.truncate(1);
    singleton.projections.truncate(1);
    assert_eq!(
        summarize_seed(&singleton).verdict,
        SeedVerdict::Underpowered(UnderpoweredReason::FewerThanTwoJoinedUnits)
    );
}

#[test]
fn missing_duplicate_and_disabled_joins_are_not_flat_projection_evidence() {
    let mut missing = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    missing.projections.pop();
    let missing_report = summarize_seed(&missing);
    assert_eq!(missing_report.verdict, SeedVerdict::JoinFailure);
    assert_eq!(missing_report.branches.missing_projections, vec![BakeId(4)]);

    let mut duplicate = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    duplicate.sources.push(duplicate.sources[0]);
    let duplicate_report = summarize_seed(&duplicate);
    assert_eq!(duplicate_report.verdict, SeedVerdict::JoinFailure);
    assert_eq!(duplicate_report.branches.duplicate_sources, vec![BakeId(1)]);

    let mut duplicate_projection = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    duplicate_projection
        .projections
        .push(duplicate_projection.projections[0]);
    let duplicate_projection_report = summarize_seed(&duplicate_projection);
    assert_eq!(
        duplicate_projection_report.verdict,
        SeedVerdict::JoinFailure
    );
    assert_eq!(
        duplicate_projection_report.branches.duplicate_projections,
        vec![BakeId(1)]
    );

    let mut disabled = varied_source_input([[0.5, 0.5], [0.5, 0.5]]);
    disabled.treatment = ExchangeTreatment::Disabled;
    assert_eq!(
        summarize_seed(&disabled).verdict,
        SeedVerdict::DisabledTreatment,
        "disabled D2 is uninstantiated, never universal equality"
    );
}

#[test]
fn zero_demand_and_invalid_access_are_visible_exclusions() {
    let mut zero_demand = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    zero_demand.projections[0].demand[0] = 0.0;
    let zero_report = summarize_seed(&zero_demand);
    assert_eq!(zero_report.verdict, SeedVerdict::InvalidMeasurement);
    assert_eq!(zero_report.branches.zero_demand_units, vec![BakeId(1)]);

    let mut incoherent = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    incoherent.projections[0].witness.proposed[0] = 0;
    let incoherent_report = summarize_seed(&incoherent);
    assert_eq!(incoherent_report.verdict, SeedVerdict::InvalidMeasurement);
    assert_eq!(
        incoherent_report.branches.incoherent_access_units,
        vec![BakeId(1)]
    );
}

#[test]
fn explicit_zero_phase_incompleteness_is_non_clearing_without_being_malformed() {
    let mut input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    input.projections[1].witness.phase_count = 0;
    input.projections[1].witness.coverage = [0.0; 2];
    input.projections[1].witness.shortfall = [0.0; 2];
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::IncompleteMeasurement);
    assert_eq!(
        report.branches,
        IntegrityBranches {
            phase_incomplete_units: vec![BakeId(2)],
            ..IntegrityBranches::default()
        }
    );
}

#[test]
fn nonzero_partial_phases_are_invalid_measurements() {
    let mut input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    input.projections[1].witness.phase_count = D2_PHASES_PER_EPOCH - 1;
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::InvalidMeasurement);
    assert_eq!(
        report.branches,
        IntegrityBranches {
            invalid_projection_units: vec![BakeId(2)],
            ..IntegrityBranches::default()
        }
    );
}

#[test]
fn nonnegative_and_conservation_failures_do_not_enter_the_verdict_population() {
    let mut invalid_source = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    invalid_source.sources[0].raw.capacity = -1.0;
    let invalid_source_report = summarize_seed(&invalid_source);
    assert_eq!(
        invalid_source_report.verdict,
        SeedVerdict::InvalidMeasurement
    );
    assert_eq!(
        invalid_source_report.branches.invalid_sources,
        vec![BakeId(1)]
    );

    let mut invalid = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    invalid.projections[0].witness.shortfall[0] = -0.25;
    let invalid_report = summarize_seed(&invalid);
    assert_eq!(invalid_report.verdict, SeedVerdict::InvalidMeasurement);
    assert_eq!(
        invalid_report.branches.invalid_projection_units,
        vec![BakeId(1)]
    );

    let mut nonconserving = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    nonconserving.exchange.conservation_residuals[1] = 0.25;
    let conservation_report = summarize_seed(&nonconserving);
    assert_eq!(
        conservation_report.verdict,
        SeedVerdict::ConservationFailure
    );
    assert_eq!(
        conservation_report.branches.non_zero_conservation_resources,
        vec![1]
    );
}

#[test]
fn coverage_and_shortfall_must_remain_exact_complements() {
    let mut input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
    input.projections[0].witness.shortfall = [0.5, 0.5];
    let report = summarize_seed(&input);

    assert_eq!(report.verdict, SeedVerdict::InvalidMeasurement);
    assert_eq!(report.branches.invalid_projection_units, vec![BakeId(1)]);
}

#[test]
fn a_varied_seed_cannot_rescue_a_flat_seed_through_pooled_totals() {
    let cleared = summarize_seed(&varied_source_input([[0.0, 1.0], [1.0, 0.0]]));
    let flat = summarize_seed(&two_unit_input([
        source(1, 10, 0.2, 0.5, 175.0),
        source(2, 20, 0.8, 0.5, 175.0),
    ]));
    let seeds = vec![cleared, flat];
    let pooled = pooled_totals(&seeds);

    assert!(pooled.source.passes_guard());
    assert!(pooled.projection.signatures.len() > 1);
    assert_eq!(
        roster_verdict(&seeds),
        RosterVerdict::MixedOrUnderpowered,
        "pooled variation is descriptive and cannot rescue one flat seed"
    );
}

/// claim: invariant(fixed synthetic seed roster: 1..=200) — the reducer
/// refuses a short or reordered roster before any pooled verdict is formed.
#[test]
fn the_fixed_roster_contract_keeps_every_seed_in_order() {
    let mut inputs: Vec<_> = PROBE_SEEDS
        .map(|seed| {
            let mut input = varied_source_input([[0.0, 1.0], [1.0, 0.0]]);
            input.seed = seed;
            input
        })
        .collect();
    let report = summarize_fixed_roster(&inputs).expect("the literal roster is complete");
    assert_eq!(report.seeds.len(), PROBE_WORLD_DENOMINATOR);
    assert_eq!(report.verdict, RosterVerdict::Cleared);

    let mut short = inputs.clone();
    short.pop();
    assert_eq!(
        summarize_fixed_roster(&short),
        Err(RosterContractError::WrongSeedCount { actual: 199 })
    );

    inputs[41].seed = 4_242;
    assert_eq!(
        summarize_fixed_roster(&inputs),
        Err(RosterContractError::UnexpectedSeed {
            index: 41,
            expected: 42,
            actual: 4_242,
        })
    );
}

#[test]
fn enabled_same_seed_witnesses_are_deterministic_label_blind_and_save_inert() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let first = enabled_build(11, &components);
    let second = enabled_build(11, &components);
    assert!(!first.history.diagnostic_portfolios.is_empty());
    assert_eq!(
        first.history.diagnostic_portfolios,
        second.history.diagnostic_portfolios
    );
    assert_eq!(
        first.history.diagnostic_subsistence, second.history.diagnostic_subsistence,
        "the same seed must produce the same ordered typed witnesses"
    );

    let first_input = seed_input_from_build(11, &first, &components);
    let second_input = seed_input_from_build(11, &second, &components);
    assert_eq!(first_input, second_input);
    let report = summarize_seed(&first_input);

    let mut relabeled = first.history.clone();
    for record in &mut relabeled.records {
        record.core.function = Function::Fort;
    }
    let relabeled_input =
        seed_input_from_history(11, &first.world, &relabeled, first.exchange, &components);
    assert_eq!(
        summarize_seed(&relabeled_input),
        report,
        "Function labels are downstream and cannot create source or projection variation"
    );

    let mut without_sidecar = first.history.clone();
    without_sidecar.diagnostic_subsistence.clear();
    without_sidecar.diagnostic_portfolios.clear();
    assert_eq!(
        emitted_ledger_bytes(&first.history),
        emitted_ledger_bytes(&without_sidecar),
        "the per-community witness must remain absent from emitted history bytes"
    );
}

#[test]
/// claim: readout(forall-seed in 1..=200, off-gate) — joins one live
/// occupation to one pre-role source vector and one enabled, phase-integrated
/// typed realization/access witness; every seed keeps its own verdict and the
/// pooled totals remain descriptive.
#[ignore = "probe: one fixed 200-seed Staple D3B gradient-sufficiency report; run exactly once"]
fn fixed_200_seed_d3b_gradient_sufficiency_report() {
    let components = WorldComponents::assemble().expect("canonical components assemble");
    let inputs: Vec<_> = PROBE_SEEDS
        .map(|seed| enabled_seed_input(seed, &components))
        .collect();
    let report = summarize_fixed_roster(&inputs).expect("the preregistered roster is exact");

    println!("D3B fixed-roster gradient-sufficiency report");
    for seed in &report.seeds {
        println!(
            "seed={} denominator={} live={} joined={} source_bands=({},{},{}) \
             source_signatures={:?} singleton_source_signatures={} \
             projection_signatures={:?} committed_vectors={} raw_spread={:?} \
             access={:?} branches={:?} verdict={:?}",
            seed.seed,
            seed.denominator,
            seed.live_units,
            seed.joined_units.len(),
            seed.source.surplus_bands.len(),
            seed.source.river_bands.len(),
            seed.source.capacity_bands.len(),
            seed.source.signatures,
            seed.source.singleton_signatures,
            seed.projection.signatures,
            seed.projection.committed_vectors.len(),
            seed.projection.raw_spread,
            seed.projection.access,
            seed.branches,
            seed.verdict,
        );
        println!("  joined_units={:?}", seed.joined_units);
    }
    println!(
        "pooled_descriptive denominator={} joined={} source_signatures={:?} \
         projection_signatures={:?} committed_vectors={} raw_spread={:?} access={:?}",
        report.pooled.denominator,
        report.pooled.joined_units,
        report.pooled.source.signatures,
        report.pooled.projection.signatures,
        report.pooled.projection.committed_vectors.len(),
        report.pooled.projection.raw_spread,
        report.pooled.projection.access,
    );
    println!("fixed_roster_verdict={:?}", report.verdict);

    assert_eq!(report.seeds.len(), PROBE_WORLD_DENOMINATOR);
    for seed in &report.seeds {
        assert!(
            !matches!(
                seed.verdict,
                SeedVerdict::DisabledTreatment
                    | SeedVerdict::JoinFailure
                    | SeedVerdict::ConservationFailure
                    | SeedVerdict::InvalidMeasurement
            ),
            "seed {} must resolve every preregistered integrity branch before its scientific verdict: {:?}",
            seed.seed,
            seed.branches,
        );
    }
}
