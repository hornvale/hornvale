//! Roster parity between worldgen's exposure classification and the lab's
//! deliberately-independent second opinion.
//!
//! The lab restates worldgen's `Steeped` rules rather than importing them, so
//! the metric is a SECOND OPINION and not an echo — that is the design, and it
//! has caught real bugs twice. Its one actual failure mode, three campaigns
//! running (The Wearing's toponymic concepts, The Toponym's variants, The
//! Watershed's staples), is that the duplicate's ROSTER silently loses
//! entries while its PREDICATES stay fine.
//!
//! Nothing in the independence argument requires the roster to be
//! hand-maintained too. This guard asserts parity of the concept SET while
//! leaving every predicate independently restated: adding a concept worldgen
//! can steep now reds this test instead of quietly reading false on three
//! quarters of the census.
//!
//! ## The parity unit (The Witness, Task 3)
//!
//! `exposure_of_impl` (`windows/worldgen/src/lib.rs`) steeps via eight rules.
//! Five have a concept SET that is fixed at composition root, independent of
//! any one seed:
//!
//! 1. `universal_stratum()` — unconditional for every species.
//! 2. the color/body/kin packs — ladder-gated per species, but every pack
//!    MEMBER is a fixed, authored concept regardless of which species reaches
//!    it.
//! 3. the seven toponymic terrain gates (river/ford/hill/valley/marsh/
//!    spring/island) — derived here from the registry's own
//!    `concept_domain`, the way `windows/worldgen/tests/exposure.rs` already
//!    does, rather than duplicated as a literal.
//! 4. `home`/`hearth`/`god`/`spirit` — the same fixed four literal both
//!    `exposure_of_impl` and `independently_steeped_concepts` already carry.
//! 5. the six staples, `hornvale_climate::Crop::catalog()`.
//!
//! Three rules read PER-CELL or per-coexisting-roster state, and a
//! cell-by-cell comparison would not be meaningful — this run's seed sweep
//! would only ever witness whichever biomes/variants/kinds those particular
//! cells happen to produce, and a test that froze that would be a golden on
//! geography, not a soundness check. The honest unit of parity for these
//! three is the CATEGORY each draws from, which — unlike the specific value
//! any one cell yields — IS fixed and enumerable:
//!
//! - `biome` draws from the closed `hornvale_climate::biome::ALL`.
//! - the Toponym's variant draws from `hornvale_climate::Variant::catalog()`.
//! - `{species}-kind` draws from the peoples that SPEAK, which
//!   `hornvale_worldgen::WorldComponents::assemble()`'s `articulation` store
//!   fixes at composition root (The Eremite: a family may hold a
//!   non-speaking minded kind) — not seed-dependent even though WHICH of
//!   those peoples actually coexist in a given world is (Task 2's `world()`
//!   in `exposure.rs` documents that shift in detail).
//!
//! So: every statically enumerable source, plus the closed catalog each
//! dynamic rule draws its concept names from — never a specific cell's,
//! seed's, or species' pairing verdict. `steepable_concept_roster` computes
//! exactly this ceiling; this test re-derives the same ceiling independently
//! (from the registry and the same public catalogs) and asserts the two
//! agree.
//!
//! No rule needed to be left unchecked. All eight have a well-defined
//! roster-level unit of parity once "capable of," not "achieved for this
//! world," is the question being asked.
use hornvale_astronomy::SkyPins;
use hornvale_climate::{Crop, Variant, biome};
use hornvale_kernel::Seed;
use hornvale_worldgen::WorldComponents;

/// The set of concepts worldgen's `exposure_of_impl` can ever classify
/// `Steeped`, derived independently of `hornvale_lab::steepable_concept_roster`
/// (never imported from it, and never from `exposure_of` or its private gate
/// helpers — see this file's module doc comment for why each of the eight
/// rules is represented here by its closed CATEGORY rather than a literal).
fn worldgen_can_steep() -> std::collections::BTreeSet<String> {
    let mut can_steep: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();

    // Rules 1-2: the universal stratum and the ladder-gated packs. Every
    // pack member, not only the ones some particular species' perception
    // ladder reaches — capacity to steep, not achievement for one species.
    for entry in hornvale_language::universal_stratum()
        .iter()
        .chain(hornvale_language::color_pack())
        .chain(hornvale_language::body_pack())
        .chain(hornvale_language::kin_pack())
    {
        can_steep.insert(entry.concept.to_string());
    }

    // Rule 8 (the seven toponymic gates): derived from the registry's own
    // `concept_domain`, exactly as `some_census_world_steeps_every_
    // toponymic_concept` in `windows/lab/tests/calibration.rs` does
    // (originally `every_core_toponymic_concept_wins_a_root_somewhere_in_a_
    // seed_sweep` in `windows/worldgen/tests/exposure.rs`, The Assay Task 9,
    // retired once the census carried the same coverage over 1,000 worlds)
    // — never a hardcoded seven-item literal, so adding an eighth toponymic
    // concept enrolls here automatically.
    let w = hornvale_worldgen::build_world(
        Seed(42),
        &SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    for concept in w
        .registry
        .concepts()
        .filter(|c| hornvale_language::concept_domain(&c.name) == Some("toponymic"))
    {
        can_steep.insert(concept.name.clone());
    }

    // Rule 7: the fixed four, the same literal both `exposure_of_impl` and
    // `independently_steeped_concepts` already carry.
    for concept in ["home", "hearth", "god", "spirit"] {
        can_steep.insert(concept.to_string());
    }

    // Rule 6 (the STAPLE of a Farming cell): the closed crop catalog.
    for crop in Crop::catalog() {
        can_steep.insert(crop.concept_name().to_string());
    }

    // Rule 3 (biome of a settled cell): the closed biome catalog — the
    // CATEGORY the per-cell rule draws from, not any one cell's reading.
    for b in biome::ALL {
        can_steep.insert(b.concept_name().to_string());
    }

    // Rule 4 (The Toponym's variant of a settled cell): the closed variant
    // catalog, on the same reasoning as biome above.
    for variant in Variant::catalog() {
        can_steep.insert(variant.concept_name().to_string());
    }

    // Rule 5 (`{species}-kind`, own and coexisting): the closed set of
    // KINDS THAT SPEAK, fixed at composition root regardless of which
    // subset actually coexists in any one seed's world.
    let wc = WorldComponents::assemble().expect("the shipped composition root always assembles");
    for kind in wc.articulation.ids() {
        can_steep.insert(format!("{}-kind", kind.0));
    }

    can_steep
}

#[test]
fn the_lab_considers_every_concept_worldgen_can_steep() {
    let worldgen_can_steep = worldgen_can_steep();
    assert!(
        !worldgen_can_steep.is_empty(),
        "the derivation itself is broken — an empty requirement would make \
         this test vacuously green"
    );
    let lab_considers = hornvale_lab::steepable_concept_roster();
    let missing: Vec<&String> = worldgen_can_steep.difference(&lab_considers).collect();
    assert!(
        missing.is_empty(),
        "the lab's independent reading does not consider {missing:?} — \
         worldgen can steep them, so the second opinion is blind there"
    );
}
