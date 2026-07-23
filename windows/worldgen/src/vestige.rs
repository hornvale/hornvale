//! Subsurface historical residue — sealed wards, abandoned delvings, buried
//! ruins, gate-scars (The Vestige). A derived reading of the narrated past
//! (settlement-abandonment history + terrain): no live mutation, no committed
//! facts, no metaphysics — the door and its dread, not the entity.

use crate::history_emit::{occupations_at, occupations_by_cell, present_day};
use hornvale_history::record::{CauseOfEnd, Function, Notability, OccupationRecord};
use hornvale_kernel::{CellId, CellMap, World, math};
use hornvale_terrain::GeneratedTerrain;

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

/// A rare pre-human gate-scar, if this cell's deep crust is old enough and
/// the shared hash-noise presence test fires
/// (`GeneratedTerrain::prehuman_scar_at`, mirroring `cave_at`/`deposit_at`'s
/// pattern). Pure and deterministic: no draws, no facts, no epoch — the
/// presence gate reuses The Lode's FEATURES noise seed, so no new stream
/// label is introduced. Ocean cells never qualify (nothing pre-human is
/// legible under open water in this model). `founded_day` is `None`:
/// pre-human residue predates the narrated (people) timeline entirely.
pub fn prehuman_vestige(terrain: &GeneratedTerrain, cell: CellId) -> Option<Vestige> {
    if !terrain.prehuman_scar_at(cell) {
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

/// The whole world's palimpsest field, one layer-stack per cell — the
/// batched sibling of [`vestiges_at`] the per-cell dread field and residue
/// lens now build over. `vestiges_at` calls `occupations_at`, which rescans
/// the entire ledger (`occupation_records`) on every call; asking for it once
/// per cell in a `CellMap::from_fn` loop is `O(cells × occupations)` ledger
/// reconstructions. This instead scans the ledger exactly once
/// ([`occupations_by_cell`]) and reads `present_day` once, so the whole field
/// costs `O(occupations + cells)`. Per cell, the stack is built in the exact
/// same order `vestiges_at` produces (the pre-human layer first, if any, then
/// that cell's occupations oldest-founded-first via
/// [`vestige_from_occupation`]) — so `vestiges_field(world, terrain).get(cell)`
/// is byte-for-byte identical to `&vestiges_at(world, terrain, cell)` for
/// every cell (see this module's `vestiges_field_matches_vestiges_at_per_cell`
/// test).
pub fn vestiges_field(world: &World, terrain: &GeneratedTerrain) -> CellMap<Vec<Vestige>> {
    let now = present_day(world);
    let by_cell = occupations_by_cell(world);
    let geo = terrain.geosphere();
    CellMap::from_fn(geo, |cell| {
        let mut layers = Vec::new();
        if let Some(prehuman) = prehuman_vestige(terrain, cell) {
            layers.push(prehuman);
        }
        if let Some(occs) = by_cell.get(&cell) {
            layers.extend(occs.iter().map(|occ| vestige_from_occupation(occ, now)));
        }
        layers
    })
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

    use crate::{SettlementPins, SkyChoice, build_world, occupation_records, terrain_of};
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
    /// crust (`crust_age_at` ~0.992, well past terrain's ancient-crust
    /// threshold) whose presence noise (~0.277) falls below terrain's
    /// pre-human scar threshold (`GeneratedTerrain::prehuman_scar_at`) — the
    /// one pre-human hit among that seed's ~1900 ancient-crust cells.
    const DEEP_ANCIENT_NUMINOUS_CELL: CellId = CellId(21966);

    /// A land cell at seed 42 whose crust falls short of the ancient
    /// threshold (`crust_age_at` ~0.716) — fails the age gate regardless of
    /// the noise draw.
    const SHALLOW_YOUNG_CELL: CellId = CellId(5);

    /// Terrain's ancient-crust threshold, mirrored here only for these
    /// fixture assertions (`GeneratedTerrain::prehuman_scar_at`'s internal
    /// gate; see `domains/terrain/src/provider.rs`'s `ANCIENT_CRUST_AGE`).
    const ANCIENT_CRUST_AGE_FOR_TEST: f64 = 0.8;

    #[test]
    fn a_deep_ancient_cell_can_yield_a_prehuman_gate_scar() {
        let (_world, terrain) = seed_42_world_and_terrain();
        assert!(
            !terrain.is_ocean(DEEP_ANCIENT_NUMINOUS_CELL),
            "the fixture cell must be land"
        );
        assert!(
            terrain.crust_age_at(DEEP_ANCIENT_NUMINOUS_CELL) > ANCIENT_CRUST_AGE_FOR_TEST,
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
            terrain.crust_age_at(SHALLOW_YOUNG_CELL) <= ANCIENT_CRUST_AGE_FOR_TEST,
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

    #[test]
    fn vestiges_field_matches_vestiges_at_per_cell() {
        // The batched, one-scan field must be byte-identical, cell by cell,
        // to the per-cell path it replaces in the hot loops (`vestige_dread`,
        // the residue lens) — same ordering (pre-human first, then
        // oldest-founded-first occupations), same values.
        let (world, terrain) = seed_42_world_and_terrain();
        let field = vestiges_field(&world, &terrain);

        // The pre-human fixture cell (a breached/forgotten gate-scar).
        assert_eq!(
            field.get(DEEP_ANCIENT_NUMINOUS_CELL),
            &vestiges_at(&world, &terrain, DEEP_ANCIENT_NUMINOUS_CELL),
            "the pre-human fixture cell must match"
        );
        // A shallow cell with no pre-human layer at all.
        assert_eq!(
            field.get(SHALLOW_YOUNG_CELL),
            &vestiges_at(&world, &terrain, SHALLOW_YOUNG_CELL),
            "a cell with no pre-human layer must match"
        );
        // A sample of cells that actually carry people occupations, so the
        // grouped-by-site path is checked against populated stacks too, not
        // just the two fixture cells above.
        let occupied_cells: std::collections::BTreeSet<CellId> =
            occupation_records(&world).iter().map(|o| o.site).collect();
        for cell in occupied_cells.into_iter().take(25) {
            assert_eq!(
                field.get(cell),
                &vestiges_at(&world, &terrain, cell),
                "occupied cell {cell:?} must match between the batched and per-cell paths"
            );
        }
    }
}
