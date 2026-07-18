//! The genesis pipeline as declared systems, and the keystone that pins the
//! declarations to the working pipeline: the genesis hand-order is a valid
//! topological sort of the declared read/write DAG (and a dependency-violating
//! order is rejected — the anti-vacuity half). Genesis still executes via
//! `build_to`'s hand-order; this schema validates it (shadow, spec §4.5).
//!
//! **Scope boundary (deliberate):** the classification tail of `build_to` —
//! the `"planet"` stage (C1: `is-a` + the world's endonym `name` on the world
//! root) and the `"peoples"` stage (C2: one `instance-of` collective per
//! placed people, `name` = autonym) — is **outside** this schema. Declaring
//! those stages truthfully (`writes: NAME`) manufactures a false cycle:
//! `religion` reads `NAME` (place names committed by `settlement`), and the
//! schema's edges are predicate-granular but subject-blind, so a late
//! world/collective `NAME` writer is forced *before* an early place-`NAME`
//! reader. Their real ordering constraint (peoples must be placed first) is
//! an in-memory dependency this predicate-level schema cannot express, so an
//! edge exemption would not repair it — the derived schedule would falsely
//! float the tail to the front. Subject-aware (or exemption-aware) edges are
//! an ECS-program follow-up; until then this schema covers exactly
//! [`GENESIS_HAND_ORDER`]'s eight core systems, and the tail's ledger effects
//! remain governed by the ledger's own per-`(subject, predicate)` checks.
use hornvale_kernel::{CapabilitySchema, System};

/// The order genesis stages actually commit in `build_to` — the ground truth
/// the derived schedule is validated against.
/// type-audit: bare-ok(identifier-text)
pub const GENESIS_HAND_ORDER: &[&str] = &[
    "world-entity",
    "sky",
    "terrain",
    "settlement",
    "culture",
    "religion",
    "species",
    "paleoclimate",
];

/// One capability declaration per genesis stage. Reads/writes are the predicate
/// constants each stage actually reads and commits, derived by reading each
/// stage's genesis code in `windows/worldgen/src/lib.rs::build_to` (spec §4.5):
///
/// - `world-entity` mints the world entity and commits the sky-provider
///   choice and its scenario-pin strings — bookkeeping astronomy owns the
///   predicates for, but which the composition root commits directly,
///   ahead of astronomy's own genesis call.
/// - `sky` (`hornvale_astronomy::facts::genesis`) commits the generated
///   system's headline parameters; a pure function of the already-generated
///   `GenesisOutcome`, so it reads nothing from the ledger.
/// - `terrain` (`hornvale_terrain::facts::genesis`, plus the terrain-pin
///   facts `build_to` commits alongside it) is likewise pure over its own
///   `GenesisOutcome`.
/// - `settlement` reconstructs terrain and sky via `terrain_of`/`climate_of`/
///   `sky_of` to place its scatter (real reads: `TERRAIN_PIN`, the
///   `sky-provider`/`scenario-pin` bookkeeping), then commits each
///   settlement's place facts, its settlement-pin echoes, its glossed-name
///   facts, and (in the founding-alignment pass) each settlement's solstice
///   azimuth.
/// - `culture` derives each flagship's subsistence/caste structure purely
///   from the in-memory placement scatter and the world's component set — no
///   ledger read.
/// - `religion` reads culture's committed castes (`castes_of` →
///   `HAS_CASTE`) to size the society, and observes phenomena from the
///   world's first place (`hornvale_terrain::places` → `IS_PLACE`/`BIOME`/
///   `NAME`, then that place's `LATITUDE`/`LONGITUDE` — all settlement
///   writes) before committing each deity's facts, including its own
///   glossed-name facts.
/// - `species` mints one entity per kind from the world's component set
///   (no ledger read) and links each settlement to its dominant species
///   (`PEOPLED_BY`), reading only the in-memory placement scatter.
/// - `paleoclimate` reconstructs terrain and sky the same way `settlement`
///   does (same `TERRAIN_PIN`/`sky-provider`/`scenario-pin` reads) before
///   committing its deep-time summary facts.
///
/// `settlement` and `religion` both declare [`hornvale_kernel::NAME_GLOSS`]
/// in `writes` — both commits are real (a settlement's name-gloss, a
/// deity's name-gloss), on disjoint subjects. `NAME_GLOSS` is kernel-core
/// (registered by `World::new`, not by any one domain), which is exactly
/// why [`hornvale_kernel::KERNEL_CORE_PREDICATES`] exists: it is shared
/// infrastructure `single_writer_check`'s exemption list excuses from the
/// single-writer rule, rather than a declaration this schema should hide
/// (ecs-c6 T3 — see `genesis_declarations_pass_the_single_writer_check`).
pub fn genesis_systems() -> CapabilitySchema {
    CapabilitySchema::new(vec![
        System::new(
            "world-entity",
            &[],
            &[
                hornvale_astronomy::facts::SKY_PROVIDER,
                hornvale_astronomy::facts::SCENARIO_PIN,
            ],
        ),
        System::new(
            "sky",
            &[],
            &[
                hornvale_astronomy::facts::STAR_CLASS,
                hornvale_astronomy::facts::TIDALLY_LOCKED,
                hornvale_astronomy::facts::DAY_LENGTH_STD,
                hornvale_astronomy::facts::YEAR_LENGTH_STD,
                hornvale_astronomy::facts::OBLIQUITY_DEGREES,
                hornvale_astronomy::facts::MOON_COUNT,
                hornvale_astronomy::facts::MOON_PERIOD_STD,
                hornvale_astronomy::facts::MOON_TIDE_REL,
                hornvale_astronomy::facts::RETROGRADE_SPIN,
                hornvale_astronomy::facts::MOON_INCLINATION_DEGREES,
                hornvale_astronomy::facts::MOON_NODE_LONGITUDE_DEGREES,
                hornvale_astronomy::facts::MOON_NODE_PERIOD_DAYS,
                hornvale_astronomy::facts::MOON_MASS_LUNAR,
                hornvale_astronomy::facts::MOON_DISTANCE_MM,
                hornvale_astronomy::facts::MOON_ANGULAR_SIZE_REL,
                hornvale_astronomy::facts::IS_NEIGHBOR,
                hornvale_astronomy::facts::NEIGHBOR_CLASS,
                hornvale_astronomy::facts::NEIGHBOR_DISTANCE_LY,
                hornvale_astronomy::facts::NEIGHBOR_BRIGHTNESS_REL,
                hornvale_astronomy::facts::NEIGHBOR_DECLINATION_DEG,
                hornvale_astronomy::facts::NEIGHBOR_RA_DEG,
                hornvale_astronomy::facts::GENESIS_NOTE,
                hornvale_astronomy::facts::ECCENTRICITY_MEAN,
                hornvale_astronomy::facts::OBLIQUITY_AMPLITUDE,
                hornvale_astronomy::facts::POLE_STAR_NORTH,
                hornvale_astronomy::facts::POLE_STAR_SOUTH,
                hornvale_astronomy::facts::WANDERER_COUNT_FACT,
                hornvale_astronomy::facts::WANDERER_ORBIT_AU,
                hornvale_astronomy::facts::WANDERER_PERIOD_STD,
                hornvale_astronomy::facts::WANDERER_CLASS,
                hornvale_astronomy::facts::STAR_MASS_SOLAR,
                hornvale_astronomy::facts::STAR_LUMINOSITY_SOLAR,
                hornvale_astronomy::facts::HAB_ZONE_INNER_AU,
                hornvale_astronomy::facts::HAB_ZONE_OUTER_AU,
                hornvale_astronomy::facts::ANCHOR_MASS_EARTH,
                hornvale_astronomy::facts::ANCHOR_ORBIT_AU,
                hornvale_astronomy::facts::INSOLATION_REL,
                hornvale_astronomy::facts::BRIGHTENING_PER_GYR,
                hornvale_astronomy::facts::FIGURE_COUNT,
                hornvale_astronomy::facts::FIGURE_MEMBERS,
                hornvale_astronomy::facts::FIGURE_REGION,
                hornvale_astronomy::facts::FIGURE_ON_ECLIPTIC,
                hornvale_astronomy::facts::STAR_AGE_GYR,
                hornvale_astronomy::facts::MOON_FORMATION,
                hornvale_astronomy::facts::MOON_AGE_GYR,
                hornvale_astronomy::facts::MOON_DENSITY,
            ],
        ),
        System::new(
            "terrain",
            &[],
            &[
                hornvale_terrain::facts::TERRAIN_PIN,
                hornvale_terrain::facts::PLATE_COUNT,
                hornvale_terrain::facts::OCEAN_FRACTION,
                hornvale_terrain::facts::SEA_LEVEL_M,
                hornvale_terrain::facts::HIGHEST_ELEVATION_M,
                hornvale_terrain::facts::TERRAIN_NOTE,
                hornvale_terrain::facts::SPREADING_RATE,
                hornvale_terrain::facts::RIFTED_FROM,
                hornvale_terrain::facts::BREAKUP_AGE,
            ],
        ),
        System::new(
            "settlement",
            &[
                hornvale_terrain::facts::TERRAIN_PIN,
                hornvale_astronomy::facts::SKY_PROVIDER,
                hornvale_astronomy::facts::SCENARIO_PIN,
            ],
            &[
                hornvale_settlement::SETTLEMENT_PIN,
                hornvale_kernel::NAME,
                hornvale_settlement::IS_PLACE,
                hornvale_settlement::BIOME,
                hornvale_settlement::IS_SETTLEMENT,
                hornvale_settlement::POPULATION,
                hornvale_settlement::CELL_ID,
                hornvale_settlement::LATITUDE,
                hornvale_settlement::LONGITUDE,
                hornvale_kernel::NAME_GLOSS,
                hornvale_astronomy::facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES,
            ],
        ),
        System::new(
            "culture",
            &[],
            &[hornvale_culture::SUBSISTENCE, hornvale_culture::HAS_CASTE],
        ),
        System::new(
            "religion",
            &[
                hornvale_culture::HAS_CASTE,
                hornvale_settlement::IS_PLACE,
                hornvale_settlement::BIOME,
                hornvale_kernel::NAME,
                hornvale_settlement::LATITUDE,
                hornvale_settlement::LONGITUDE,
                hornvale_astronomy::facts::SKY_PROVIDER,
                hornvale_astronomy::facts::SCENARIO_PIN,
            ],
            &[
                hornvale_religion::IS_BELIEF,
                hornvale_religion::HELD_BY,
                hornvale_religion::DERIVED_FROM_PHENOMENON,
                hornvale_religion::HIGH_GOD,
                hornvale_religion::CULT_FORM,
                hornvale_religion::DEITY_NAME,
                hornvale_religion::DEITY_NAME_IPA,
                hornvale_religion::DEITY_EPITHET,
                hornvale_religion::DEITY_EPITHET_IPA,
                hornvale_religion::SENTIMENT,
                hornvale_kernel::NAME_GLOSS,
            ],
        ),
        System::new(
            "species",
            &[],
            &[
                hornvale_species::SPECIES_NAME,
                hornvale_species::THREAT_RESPONSE,
                hornvale_species::DELIBERATION_LATENCY,
                hornvale_species::IN_GROUP_RADIUS,
                hornvale_species::TIME_HORIZON,
                hornvale_species::SOCIALITY_MODE,
                hornvale_species::STATUS_BASIS,
                hornvale_species::SPECIES_ACTIVITY_CYCLE,
                hornvale_species::SPECIES_NIGHT_VISION,
                hornvale_species::SPECIES_SKY_ATTENTION,
                hornvale_species::SPECIES_LABIALITY,
                hornvale_species::SPECIES_VOWEL_SPACE,
                hornvale_species::SPECIES_VOICING,
                hornvale_species::SPECIES_SIBILANCE,
                hornvale_species::SPECIES_VOICE_LOUDNESS,
                hornvale_species::SPECIES_TONALITY,
                hornvale_species::SPECIES_EXOTIC_MANNER,
                hornvale_species::PEOPLED_BY,
            ],
        ),
        System::new(
            "paleoclimate",
            &[
                hornvale_terrain::facts::TERRAIN_PIN,
                hornvale_astronomy::facts::SKY_PROVIDER,
                hornvale_astronomy::facts::SCENARIO_PIN,
            ],
            &[
                hornvale_paleoclimate::facts::GLACIAL_MAXIMUM_ERA,
                hornvale_paleoclimate::facts::MAX_ICE_FRACTION,
                hornvale_paleoclimate::facts::FOSSIL_SHORELINE,
                hornvale_paleoclimate::facts::REFUGIUM,
                hornvale_paleoclimate::facts::FROST_RETREAT,
            ],
        ),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hand_order_is_a_valid_topological_sort_of_the_declarations() {
        // POSITIVE half: the order genesis actually runs respects every declared
        // dependency edge. (Byte-identity is automatic — genesis uses this order;
        // this proves the DECLARATIONS capture the pipeline's real dependencies.)
        let schema = genesis_systems();
        assert!(
            schema.is_valid_order(GENESIS_HAND_ORDER),
            "GENESIS_HAND_ORDER must respect every declared read/write edge"
        );
        // schedule() must also succeed (no declared cycle) and be a valid order.
        let derived = schema.schedule().expect("declared DAG is acyclic");
        assert!(schema.is_valid_order(&derived));
    }

    #[test]
    fn a_dependency_violating_order_is_rejected() {
        // ANTI-VACUITY half: an order that runs a reader before its writer must
        // be REJECTED. If the declarations under-specify (miss a real edge), this
        // wrongly passes — so this test guards against vacuous declarations.
        // Pick two stages with a real dependency: settlement READS terrain's
        // output, so [terrain before settlement] is required; the swap must fail.
        let schema = genesis_systems();
        let mut broken: Vec<&str> = GENESIS_HAND_ORDER.to_vec();
        let ti = broken.iter().position(|&s| s == "terrain").unwrap();
        let si = broken.iter().position(|&s| s == "settlement").unwrap();
        broken.swap(ti, si);
        assert!(
            !schema.is_valid_order(&broken),
            "swapping terrain and settlement must violate a declared edge — if this \
             passes, the settlement->terrain read dependency is not declared"
        );
    }

    #[test]
    fn genesis_declarations_pass_the_single_writer_check() {
        // The §7 contract MEASURED on the real declarations against the full
        // registry (all domains registered), WITH the kernel-core exemption
        // (ecs-c6 T3 resolution): `settlement` and `religion` both really do
        // write `NAME_GLOSS` (a settlement's name-gloss, a deity's
        // name-gloss — disjoint subjects, both genuine, neither hidden from
        // this schema). That is no longer a violation because `NAME_GLOSS`
        // moved to the kernel as shared kernel-core infrastructure
        // (`hornvale_kernel::KERNEL_CORE_PREDICATES`), which
        // `single_writer_check`'s `exempt` list excuses by design — the
        // ledger's own per-`(subject, predicate)` functional-uniqueness
        // still applies and is untouched; only this coarser, predicate-only
        // schema check is relaxed for kernel-core predicates.
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        crate::register_all(&mut world.registry).expect("concept registration");
        genesis_systems()
            .single_writer_check(&world.registry, hornvale_kernel::KERNEL_CORE_PREDICATES)
            .expect("no non-exempt functional predicate has two declared genesis writers");
    }

    #[test]
    fn derived_schedule_is_pinned() {
        // The derived schedule of the real genesis declarations is a fixed,
        // drift-checkable sequence (spec §6). If this changes, a declaration's
        // reads/writes moved — re-derive deliberately and confirm byte-identity
        // still holds (genesis executes GENESIS_HAND_ORDER, not this, so worlds
        // are unaffected either way). This is label-tie-broken (Kahn's algorithm,
        // ties resolved alphabetically), NOT GENESIS_HAND_ORDER: independent
        // stages (culture, sky, species, terrain, world-entity all start at
        // indegree 0) sort alphabetically before the dependency chain
        // (world-entity -> {settlement, religion, paleoclimate};
        // terrain -> {settlement, paleoclimate}; culture -> religion;
        // settlement -> religion) resolves the rest — the
        // hand_order_is_a_valid_topological_sort_of_the_declarations test above
        // already proves both orders are valid linearizations of the same DAG.
        let order = genesis_systems()
            .schedule()
            .expect("declared DAG is acyclic");
        let expected: Vec<&str> = vec![
            "culture",
            "sky",
            "species",
            "terrain",
            "world-entity",
            "paleoclimate",
            "settlement",
            "religion",
        ];
        assert_eq!(order, expected);
    }
}
