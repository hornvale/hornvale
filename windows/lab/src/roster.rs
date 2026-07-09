//! Null-control rosters (spec §3). The twin never ships — it exists only for
//! Lab studies and tests. Two identical-vector species cannot coexist in one
//! world (placement ties break by species order, so the second places nothing),
//! so the null control runs each goblin-vectored species ALONE.

use hornvale_species::SpeciesDef;

/// The shipped goblin, placed alone (`[goblin]`).
pub fn goblin_solo_roster() -> Vec<SpeciesDef> {
    vec![hornvale_species::registry()["goblin"]]
}

/// A clone of goblin under the fresh name `goblin-twin`, placed alone. Identical
/// vectors, independent name-salted streams — the null-control twin (spec §3).
pub fn goblin_twin_solo_roster() -> Vec<SpeciesDef> {
    let goblin = hornvale_species::registry()["goblin"];
    vec![SpeciesDef {
        name: "goblin-twin",
        ..goblin
    }]
}
