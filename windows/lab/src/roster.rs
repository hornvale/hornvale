//! Null-control rosters (spec §3). The twin never ships — it exists only for
//! Lab studies and tests. Two identical-vector species cannot coexist in one
//! world (placement ties break by species order, so the second places nothing),
//! so the null control runs each goblin-vectored species ALONE.

use hornvale_species::SpeciesDef;

/// The shipped goblin, placed alone (`[goblin]`).
pub fn goblin_solo_roster() -> Vec<SpeciesDef> {
    vec![hornvale_species::registry()["goblin"].clone()]
}

/// A clone of goblin under the fresh name `goblin-twin`, placed alone. Identical
/// vectors, independent name-salted streams — the null-control twin (spec §3).
pub fn goblin_twin_solo_roster() -> Vec<SpeciesDef> {
    let goblin = hornvale_species::registry()["goblin"].clone();
    vec![SpeciesDef {
        name: "goblin-twin",
        ..goblin
    }]
}

/// A test-only **tone-capable** species, placed alone: a goblin clone renamed
/// `serpent` with `tonality = 1.0` and a compressed vowel space (a few-place
/// serpentine profile). It never ships — like the twin, it exists only to
/// exercise the phonology epoch's tonal path in Lab tests (tone-count > 1, the
/// capacity floor met by pitch). The shipped peoples stay atonal by authoring
/// (spec §9); tone is for the future bestiary.
pub fn serpent_tonal_solo_roster() -> Vec<SpeciesDef> {
    let goblin = hornvale_species::registry()["goblin"].clone();
    let goblin_peopled = goblin
        .peopled
        .clone()
        .expect("the shipped goblin carries peopled traits");
    vec![SpeciesDef {
        name: "serpent",
        family: "serpent",
        peopled: Some(hornvale_species::PeopledTraits {
            articulation: hornvale_species::ArticulationVector {
                tonality: 1.0,
                vowel_space: 0.3,
                ..goblin_peopled.articulation
            },
            ..goblin_peopled
        }),
        ..goblin
    }]
}
