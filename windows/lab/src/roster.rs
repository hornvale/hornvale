//! Null-control component sets (spec §3). The twin never ships — it exists
//! only for Lab studies and tests. Two identical-vector species cannot coexist
//! in one world (placement ties break by species order, so the second places
//! nothing), so the null control runs each goblin-vectored species ALONE.
//!
//! ECS c3: each control is a single-kind [`WorldComponents`] composed from the
//! canonical `species`/`language` registries (goblin's own components),
//! re-keyed under a fresh `KindId` and, for the serpent, with an articulation
//! override. Byte-identical to composing the equivalent component-set roster.

use hornvale_kernel::{ComponentStore, KindId};
use hornvale_language::ArticulationVector;
use hornvale_worldgen::WorldComponents;

/// Compose a single-kind component set from goblin's canonical components,
/// re-keyed under `key`, with taxonomy `family` and — for the serpent — an
/// `articulation` override (the canonical goblin articulation otherwise). The
/// family-proto store stays canonical; a singleton family (`serpent`) simply
/// has no proto row, the same as an unknown family does when composed
/// component-first.
fn goblin_derived(
    key: &'static str,
    family: &'static str,
    articulation: Option<ArticulationVector>,
) -> WorldComponents {
    let g = KindId("goblin");
    let k = KindId(key);
    let biosphere: ComponentStore<KindId, _> = [(
        k,
        hornvale_species::biosphere_registry()
            .get(&g)
            .expect("the shipped goblin has a biosphere row")
            .clone(),
    )]
    .into_iter()
    .collect();
    let psyche: ComponentStore<KindId, _> = [(
        k,
        *hornvale_species::psyche_registry()
            .get(&g)
            .expect("the shipped goblin has a psyche row"),
    )]
    .into_iter()
    .collect();
    let perception: ComponentStore<KindId, _> = [(
        k,
        *hornvale_species::perception_registry()
            .get(&g)
            .expect("the shipped goblin has a perception row"),
    )]
    .into_iter()
    .collect();
    let art = articulation.unwrap_or_else(|| {
        *hornvale_language::articulation_registry()
            .get(&g)
            .expect("the shipped goblin has an articulation row")
    });
    let articulation_store: ComponentStore<KindId, _> = [(k, art)].into_iter().collect();
    let lexicon: ComponentStore<KindId, _> = [(
        k,
        hornvale_language::lexicon_registry()
            .get(&g)
            .expect("the shipped goblin has a lexicon row")
            .clone(),
    )]
    .into_iter()
    .collect();
    let family_of: ComponentStore<KindId, &'static str> = [(k, family)].into_iter().collect();
    WorldComponents::from_stores(
        biosphere,
        psyche,
        perception,
        articulation_store,
        lexicon,
        hornvale_language::family_proto(),
        family_of,
        ComponentStore::new(),
        ComponentStore::new(),
        ComponentStore::new(),
    )
    .expect("a goblin-derived single-kind component set is well-formed")
}

/// The shipped goblin, placed alone (`[goblin]`) as a one-kind component set.
pub fn goblin_solo_components() -> WorldComponents {
    goblin_derived("goblin", "goblinoid", None)
}

/// A clone of goblin under the fresh name `goblin-twin`, placed alone. Identical
/// vectors, independent name-salted streams — the null-control twin (spec §3).
pub fn goblin_twin_solo_components() -> WorldComponents {
    goblin_derived("goblin-twin", "goblinoid", None)
}

/// A test-only **tone-capable** kind, placed alone: a goblin clone renamed
/// `serpent` with `tonality = 1.0` and a compressed vowel space (a few-place
/// serpentine profile). It never ships — like the twin, it exists only to
/// exercise the phonology epoch's tonal path in Lab tests (tone-count > 1, the
/// capacity floor met by pitch). The shipped peoples stay atonal by authoring
/// (spec §9); tone is for the future bestiary.
pub fn serpent_tonal_solo_components() -> WorldComponents {
    let base = *hornvale_language::articulation_registry()
        .get(&KindId("goblin"))
        .expect("the shipped goblin has an articulation row");
    let serpent_articulation = ArticulationVector {
        tonality: 1.0,
        vowel_space: 0.3,
        ..base
    };
    goblin_derived("serpent", "serpent", Some(serpent_articulation))
}

/// The Individuation's two-kind test roster: `owlbear` (the canonical beast,
/// biosphere only) and `awakened-owlbear` (the same body, `potency: 9/30`,
/// plus the full goblin-derived peopled cluster — an awakened beast speaks,
/// and check_integrity's peopled invariant is kept, not relaxed). Test/lab
/// only: canonical registries must never carry these rows (genesis would
/// mint and place them; spec §4.4).
pub fn awakened_owlbear_components() -> WorldComponents {
    let g = KindId("goblin");
    let beast = KindId("owlbear");
    let awakened = KindId("awakened-owlbear");
    let canon_bio = hornvale_species::biosphere_registry();
    let beast_traits = canon_bio
        .get(&beast)
        .expect("the shipped owlbear has a biosphere row")
        .clone();
    let mut awakened_traits = beast_traits.clone();
    awakened_traits.potency = 9.0 / 30.0; // Lab synthetic: treant-tier magic (CR 9/30)

    let mut biosphere: ComponentStore<KindId, _> = ComponentStore::new();
    biosphere.insert(beast, beast_traits);
    biosphere.insert(awakened, awakened_traits);

    let psyche: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_species::psyche_registry()
            .get(&g)
            .expect("the shipped goblin has a psyche row"),
    )]
    .into_iter()
    .collect();
    let perception: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_species::perception_registry()
            .get(&g)
            .expect("the shipped goblin has a perception row"),
    )]
    .into_iter()
    .collect();
    let articulation: ComponentStore<KindId, _> = [(
        awakened,
        *hornvale_language::articulation_registry()
            .get(&g)
            .expect("the shipped goblin has an articulation row"),
    )]
    .into_iter()
    .collect();
    let lexicon: ComponentStore<KindId, _> = [(
        awakened,
        hornvale_language::lexicon_registry()
            .get(&g)
            .expect("the shipped goblin has a lexicon row")
            .clone(),
    )]
    .into_iter()
    .collect();
    // Singleton families need no family_proto entry.
    let family_of: ComponentStore<KindId, &'static str> =
        [(beast, "owlbear"), (awakened, "awakened-owlbear")]
            .into_iter()
            .collect();

    WorldComponents::from_stores(
        biosphere,
        psyche,
        perception,
        articulation,
        lexicon,
        hornvale_language::family_proto(),
        family_of,
        ComponentStore::new(), // deity
        ComponentStore::new(), // culture
        ComponentStore::new(), // material
    )
    .expect("the awakened-owlbear roster is well-formed")
}
