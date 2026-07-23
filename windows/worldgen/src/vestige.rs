//! Subsurface historical residue — sealed wards, abandoned delvings, buried
//! ruins, gate-scars (The Vestige). A derived reading of the narrated past
//! (settlement-abandonment history + terrain): no live mutation, no committed
//! facts, no metaphysics — the door and its dread, not the entity.

use crate::history_emit::{occupations_at, present_day};
use hornvale_history::record::{CauseOfEnd, Function, Notability, OccupationRecord};
use hornvale_kernel::{CellId, World, math};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::crust::sphere_fbm01;

/// What a residue site is, by maker → purpose.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VestigeKind {
    /// Dormant dimensional wound in the primordial deep (pre-human).
    GateScar,
    /// The deep sealed its own chamber (pre-human, no maker).
    NaturalSeal,
    /// An abandoned mine / exhausted delving.
    AbandonedDelving,
    /// A buried ruin or, at Seat scale, an undercity / necropolis.
    BuriedRuin,
    /// A custodial ward or tomb (a Fort/Cult site).
    SealedVault,
}

/// How intact the containment is, read from the keeper's fate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SealState {
    /// A living keeper still tends it; containment is sound.
    Maintained,
    /// The keeper is gone but the seal has not had time to fail.
    Lapsing,
    /// The seal has failed; the site lies open.
    Breached,
}

/// Remembered-sacred vs forgotten-dreaded — the same axis as seal-state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Valence {
    /// Still tended and held in reverence.
    Venerated,
    /// No longer remembered for what it was; only dread remains.
    Forgotten,
}

/// The kind of danger the residue now poses (feeds the dread field).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HazardKind {
    /// Collapse risk from decayed construction.
    Structural,
    /// Foul or asphyxiating gas pooled underground.
    ToxicGas,
    /// Disease-bearing residue left by a plague end.
    Pestilent,
    /// Standing or seeping water.
    Flooded,
    /// A lingering supernatural charge (pre-human sites).
    Numinous,
    /// Held to be cursed by those who remember it.
    Cursed,
}

/// A located residue feature (one layer of a cell's palimpsest).
/// type-audit: bare-ok(ratio: dread), bare-ok(ratio: warning_legibility), bare-ok(diagnostic-value: founded_day)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vestige {
    /// What the site is, by maker and purpose.
    pub kind: VestigeKind,
    /// How intact the containment is.
    pub seal_state: SealState,
    /// Remembered-sacred vs forgotten-dreaded.
    pub valence: Valence,
    /// The kind of danger the residue now poses.
    pub hazard: HazardKind,
    /// Dread the site radiates, [0,1]: forgotten + breached is highest.
    pub dread: f64,
    /// How readable the old warning still is, [0,1]: decays fast with age.
    pub warning_legibility: f64,
    /// The historical day it was founded (people-made); None for pre-human.
    pub founded_day: Option<f64>,
}

/// A ruin older than this (days since it ended) has an all-but-lost warning.
const WARNING_HALF_LIFE_DAYS: f64 = 300.0;

/// Derive a people-made vestige from one occupation, as of `now` (days).
/// Read backward: no simulation.
/// type-audit: bare-ok(diagnostic-value: now)
pub fn vestige_from_occupation(occ: &OccupationRecord, now: f64) -> Vestige {
    let kind = match (occ.function, occ.notability) {
        (Function::Mine, _) => VestigeKind::AbandonedDelving,
        (Function::Fort, _) | (Function::Cult, _) => VestigeKind::SealedVault,
        (_, Notability::Seat) => VestigeKind::BuriedRuin, // undercity scale (see size, later)
        _ => VestigeKind::BuriedRuin,
    };
    let (seal_state, valence) = match occ.ended {
        None => (SealState::Maintained, Valence::Venerated), // a living keeper
        Some(end) => {
            let age = (now - end).max(0.0);
            if age < WARNING_HALF_LIFE_DAYS {
                (SealState::Lapsing, Valence::Forgotten)
            } else {
                (SealState::Breached, Valence::Forgotten)
            }
        }
    };
    let hazard = match occ.cause {
        Some(CauseOfEnd::Plague) => HazardKind::Pestilent,
        Some(CauseOfEnd::Burned) => HazardKind::Cursed,
        _ => match occ.function {
            Function::Mine => HazardKind::Structural,
            _ => HazardKind::Structural,
        },
    };
    // Warning decays fast (the fastest of the three rates); recent = legible.
    let warning_legibility = match occ.ended {
        None => 1.0,
        Some(end) => math::exp(-((now - end).max(0.0)) / WARNING_HALF_LIFE_DAYS),
    };
    // Dread: forgotten + breached + hazardous is highest; venerated is low.
    let base = match valence {
        Valence::Venerated => 0.1,
        Valence::Forgotten => 0.6,
    };
    let dread = (base + 0.4 * (1.0 - warning_legibility)).clamp(0.0, 1.0);
    Vestige {
        kind,
        seal_state,
        valence,
        hazard,
        dread,
        warning_legibility,
        founded_day: Some(occ.founded),
    }
}

/// Winning-craton age above which crust counts as ancient enough to have
/// witnessed the pre-human deep (`[0,1]`, `GeneratedTerrain::crust_age_at`'s
/// scale).
/// type-audit: bare-ok(ratio)
const ANCIENT_CRUST_AGE: f64 = 0.8;

/// Spatial frequency for the pre-human presence noise. Distinct from The
/// Lode's cave (5.0) and deposit (7.0) frequencies sampled off the same
/// seed, so the three point processes decorrelate on the sphere.
/// type-audit: bare-ok(ratio)
const PREHUMAN_FREQ: f64 = 11.0;

/// fBm octaves for the pre-human presence noise (matches The Lode's caves/deposits).
/// type-audit: bare-ok(count)
const PREHUMAN_OCTAVES: u32 = 4;

/// Presence threshold for the pre-human noise test: `sphere_fbm01`
/// compresses variance toward 0.5 (see `domains/terrain/src/crust.rs`), so
/// this is not a small absolute probability but is still sparse relative to
/// the ancient-crust population it gates within — at seed 42 it selects 1 of
/// ~1900 ancient-crust cells.
/// type-audit: bare-ok(ratio)
const PREHUMAN_PRESENCE_THRESHOLD: f64 = 0.30;

/// A rare pre-human gate-scar, if this cell's deep crust is old enough and
/// the shared hash-noise presence test fires. Pure and deterministic: no
/// draws, no facts, no epoch — reuses The Lode's FEATURES noise seed
/// (`GeneratedTerrain::globe().features_noise_seed()`) exactly as
/// `cave_at`/`deposit_at` do, so no new stream label is introduced. Ocean
/// cells never qualify (nothing pre-human is legible under open water in
/// this model). `founded_day` is `None`: pre-human residue predates the
/// narrated (people) timeline entirely.
pub fn prehuman_vestige(terrain: &GeneratedTerrain, cell: CellId) -> Option<Vestige> {
    if terrain.is_ocean(cell) {
        return None;
    }
    if terrain.crust_age_at(cell) <= ANCIENT_CRUST_AGE {
        return None;
    }
    let pos = terrain.geosphere().position(cell);
    let noise = sphere_fbm01(
        terrain.globe().features_noise_seed(),
        pos,
        PREHUMAN_FREQ,
        PREHUMAN_OCTAVES,
    );
    if noise >= PREHUMAN_PRESENCE_THRESHOLD {
        return None;
    }
    Some(Vestige {
        kind: VestigeKind::GateScar,
        seal_state: SealState::Breached,
        valence: Valence::Forgotten,
        hazard: HazardKind::Numinous,
        // Forgotten before there was anyone to forget it: maximally dreaded,
        // with no warning ever legible to begin with.
        dread: 0.9,
        warning_legibility: 0.0,
        founded_day: None,
    })
}

/// The full palimpsest stack at a cell, oldest layer first: the pre-human
/// residue (if any) — deep-time-old, so always first — then every people
/// occupation (`occupations_at`, already oldest-founded-first), each read
/// forward to `now` via `vestige_from_occupation`. Pure derived read: no
/// facts are written, nothing is mutated. `now` is the world's committed
/// present day (see [`crate::present_day`]).
pub fn vestiges_at(world: &World, terrain: &GeneratedTerrain, cell: CellId) -> Vec<Vestige> {
    let now = present_day(world);
    let mut layers = Vec::new();
    if let Some(prehuman) = prehuman_vestige(terrain, cell) {
        layers.push(prehuman);
    }
    layers.extend(
        occupations_at(world, cell)
            .iter()
            .map(|occ| vestige_from_occupation(occ, now)),
    );
    layers
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

    fn eid(n: u64) -> EntityId {
        EntityId::new(n).unwrap()
    }

    fn occ(
        function: Function,
        ended: Option<f64>,
        cause: Option<CauseOfEnd>,
        notability: Notability,
    ) -> OccupationRecord {
        OccupationRecord {
            people: KindId("test-people"),
            community: eid(1),
            lineage: eid(1),
            site: CellId(0),
            founded: 0.0,
            ended,
            peak_population: 100,
            tech: TechHorizon::Iron,
            function,
            deity: None,
            tongue: None,
            cause,
            ended_by: Ended::Nature,
            founded_from: Founding::Genesis(CellId(0)),
            notability,
        }
    }

    #[test]
    fn a_living_ward_is_maintained_and_venerated() {
        let v = vestige_from_occupation(&occ(Function::Fort, None, None, Notability::Seat), 2000.0);
        assert_eq!(v.seal_state, SealState::Maintained);
        assert_eq!(v.valence, Valence::Venerated);
    }

    #[test]
    fn an_ancient_ruin_is_breached_and_forgotten_with_a_lost_warning() {
        let v = vestige_from_occupation(
            &occ(
                Function::Mine,
                Some(50.0),
                Some(CauseOfEnd::Fled),
                Notability::Common,
            ),
            2000.0,
        );
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.valence, Valence::Forgotten);
        assert!(
            v.warning_legibility < 0.2,
            "ancient warning decayed to near-illegible"
        );
    }

    #[test]
    fn a_plague_end_yields_a_pestilent_hazard() {
        let v = vestige_from_occupation(
            &occ(
                Function::Cult,
                Some(1900.0),
                Some(CauseOfEnd::Plague),
                Notability::Common,
            ),
            2000.0,
        );
        assert_eq!(v.hazard, HazardKind::Pestilent);
    }

    use crate::{SettlementPins, SkyChoice, build_world, terrain_of};
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    /// Build the seed-42 Full world (and its terrain provider) once per test,
    /// the same helper `history_emit`'s tests use.
    fn seed_42_world_and_terrain() -> (hornvale_kernel::World, GeneratedTerrain) {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let terrain = terrain_of(&world).unwrap();
        (world, terrain)
    }

    /// Found by an exploratory scan of seed 42's terrain: ancient continental
    /// crust (`crust_age_at` ~0.992, well past [`ANCIENT_CRUST_AGE`]) whose
    /// presence noise (~0.277) falls below [`PREHUMAN_PRESENCE_THRESHOLD`] —
    /// the one pre-human hit among that seed's ~1900 ancient-crust cells.
    const DEEP_ANCIENT_NUMINOUS_CELL: CellId = CellId(21966);

    /// A land cell at seed 42 whose crust falls short of the ancient
    /// threshold (`crust_age_at` ~0.716) — fails the age gate regardless of
    /// the noise draw.
    const SHALLOW_YOUNG_CELL: CellId = CellId(5);

    #[test]
    fn a_deep_ancient_cell_can_yield_a_prehuman_gate_scar() {
        let (_world, terrain) = seed_42_world_and_terrain();
        assert!(
            !terrain.is_ocean(DEEP_ANCIENT_NUMINOUS_CELL),
            "the fixture cell must be land"
        );
        assert!(
            terrain.crust_age_at(DEEP_ANCIENT_NUMINOUS_CELL) > ANCIENT_CRUST_AGE,
            "the fixture cell must be ancient"
        );
        let v = prehuman_vestige(&terrain, DEEP_ANCIENT_NUMINOUS_CELL)
            .expect("ancient crust + a firing presence draw yields a pre-human vestige");
        assert_eq!(v.kind, VestigeKind::GateScar);
        assert_eq!(v.hazard, HazardKind::Numinous);
        assert_eq!(v.valence, Valence::Forgotten);
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.founded_day, None);
    }

    #[test]
    fn a_shallow_young_cell_yields_no_prehuman_vestige() {
        let (_world, terrain) = seed_42_world_and_terrain();
        assert!(
            terrain.crust_age_at(SHALLOW_YOUNG_CELL) <= ANCIENT_CRUST_AGE,
            "the fixture cell must fall short of the ancient threshold"
        );
        assert_eq!(prehuman_vestige(&terrain, SHALLOW_YOUNG_CELL), None);
    }

    #[test]
    fn vestiges_at_orders_prehuman_first_then_people_and_is_deterministic() {
        let (world_a, terrain_a) = seed_42_world_and_terrain();
        let (world_b, terrain_b) = seed_42_world_and_terrain();

        let stack_a = vestiges_at(&world_a, &terrain_a, DEEP_ANCIENT_NUMINOUS_CELL);
        let stack_b = vestiges_at(&world_b, &terrain_b, DEEP_ANCIENT_NUMINOUS_CELL);
        assert_eq!(stack_a, stack_b, "vestiges_at must be deterministic");

        assert!(
            !stack_a.is_empty(),
            "the fixture cell has at least the pre-human layer"
        );
        assert_eq!(
            stack_a[0].kind,
            VestigeKind::GateScar,
            "the pre-human layer comes first (oldest)"
        );
        assert_eq!(stack_a[0].founded_day, None);
        for layer in &stack_a[1..] {
            assert!(
                layer.founded_day.is_some(),
                "every layer after the pre-human one is a people occupation"
            );
        }

        // Founded-day ordering among the people layers is oldest-first,
        // inherited from `occupations_at`.
        let founded: Vec<f64> = stack_a[1..].iter().filter_map(|v| v.founded_day).collect();
        assert!(
            founded.windows(2).all(|w| w[0] <= w[1]),
            "people layers must stay oldest-founded-first"
        );
    }

    #[test]
    fn vestiges_at_with_no_prehuman_layer_starts_with_people_only() {
        let (world, terrain) = seed_42_world_and_terrain();
        let stack = vestiges_at(&world, &terrain, SHALLOW_YOUNG_CELL);
        assert!(
            stack.iter().all(|v| v.founded_day.is_some()),
            "with no pre-human layer, every entry is a people occupation"
        );
    }
}
