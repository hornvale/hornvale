//! The place sentence (The Plat, spec §3.4): what a possession reads in a
//! rung a people cut, keyed by the plan's [`Role`] of the region it stands
//! in and the rung's [`Tenancy`]. Appended to `look`, never woven into it,
//! so a wild descent reads byte for byte as it did; and every noun it names
//! answers `examine` (the Gallery's both-directions rule), which is what
//! [`place_nouns`] is for.

use hornvale_worldgen::chamber::ChamberOrigin;
use hornvale_worldgen::delve_seating::Tenancy;
use hornvale_worldgen::plat::Role;

use crate::underground::Underground;

/// The sentence for the region the possession stands in, in a `Made` rung.
/// `None` in a wild rung, and `None` on a divider cell (a `Threshold`
/// belongs to no region — the same rule `drop` applies).
pub(crate) fn place_sentence(ug: &Underground) -> Option<String> {
    if ug.origins[ug.rung] != ChamberOrigin::Made {
        return None;
    }
    let node = crate::descent_thing::node_here(ug, ug.cell)?;
    let role = ug.reading.roles[node];
    let tenancy = ug.tenancy[ug.rung];
    let mut out = role_sentence(role, tenancy).to_string();
    if ug.reading.landing[node] {
        out.push(' ');
        out.push_str(LANDING_CLAUSE);
    }
    Some(out)
}

/// Alexander 133: a stair comes down into it and another leaves it.
const LANDING_CLAUSE: &str = "A stair comes down into it and another leaves it.";

/// The role × tenancy table of spec §3.4, verbatim.
fn role_sentence(role: Role, tenancy: Tenancy) -> &'static str {
    let alive = tenancy == Tenancy::Inhabited;
    match (role, alive) {
        (Role::Entry, true) => {
            "This is the entry of a cut place; the rock is squared where it was worked."
        }
        (Role::Entry, false) => {
            "This was the entry of a cut place, long empty; the squared rock has dulled."
        }
        (Role::Heart, true) => {
            "This hall is the heart of the place; every way through it passes near here."
        }
        (Role::Heart, false) => {
            "This hall was the heart of a place; every way through it still passes near here."
        }
        (Role::Sanctum, true) => "This is the innermost chamber of the place.",
        (Role::Sanctum, false) => "This was the innermost chamber of a place, long empty.",
        (Role::Chamber, true) => "This is a chamber of a cut place.",
        (Role::Chamber, false) => "This is a chamber of a cut place, long empty.",
    }
}

/// The nouns [`place_sentence`] names, each answering `examine` with the
/// same sentence: `entry`, `hall`, `chamber` by role, and `stair` on a
/// landing. Empty exactly when the sentence is `None`.
pub(crate) fn place_nouns(ug: &Underground) -> Vec<crate::focalize::Noun> {
    let Some(sentence) = place_sentence(ug) else {
        return Vec::new();
    };
    let node = crate::descent_thing::node_here(ug, ug.cell).expect("a sentence implies a node");
    let word = match ug.reading.roles[node] {
        Role::Entry => "entry",
        Role::Heart => "hall",
        Role::Sanctum | Role::Chamber => "chamber",
    };
    let mut out = vec![
        crate::focalize::Noun::new(word, word, &sentence)
            .with_kind(crate::focalize::NounKind::Place),
    ];
    if ug.reading.landing[node] {
        out.push(crate::focalize::Noun::new("stair", "stair", LANDING_CLAUSE));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Eight cells of the table, each a distinct sentence; the past tense
    /// never names the place as present.
    #[test]
    fn the_table_has_eight_distinct_sentences_and_the_past_tense_is_past() {
        let roles = [Role::Entry, Role::Heart, Role::Sanctum, Role::Chamber];
        let mut seen = std::collections::BTreeSet::new();
        for role in roles {
            for tenancy in [Tenancy::Inhabited, Tenancy::Abandoned] {
                let s = role_sentence(role, tenancy);
                assert!(seen.insert(s), "duplicate sentence: {s}");
                if tenancy == Tenancy::Abandoned {
                    assert!(s.contains("was ") || s.contains("long empty"), "{s}");
                }
            }
        }
        assert_eq!(seen.len(), 8);
    }

    /// A wild rung says nothing (the byte-for-byte rule for a wild descent).
    #[test]
    #[allow(clippy::disallowed_methods)] // decision 0092: named construction site
    fn a_wild_rung_has_no_place_sentence_and_no_place_nouns() {
        let world = hornvale_worldgen::fixture::seed_42_world();
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let (vertex, cave) = terrain
            .geosphere()
            .vertices()
            .filter(|&v| !terrain.is_ocean(v))
            .find_map(|v| terrain.cave_at(v).map(|c| (v, c)))
            .expect("a cave");
        let n = crate::underground::habitation_rungs().len();
        let ug = Underground::enter(
            &terrain,
            vertex,
            cave,
            world.seed,
            &crate::underground::wild_origins(n),
        );
        assert_eq!(place_sentence(&ug), None);
        assert!(place_nouns(&ug).is_empty());
    }

    /// A Made rung names its Entry where the possession arrives, in the
    /// tense its tenancy says; the test seam supplies the origins.
    #[test]
    #[allow(clippy::disallowed_methods)] // decision 0092: named construction site
    fn a_made_rung_names_the_entry_in_the_tenancys_tense() {
        let world = hornvale_worldgen::fixture::seed_42_world();
        let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
        let (vertex, cave) = terrain
            .geosphere()
            .vertices()
            .filter(|&v| !terrain.is_ocean(v))
            .find_map(|v| terrain.cave_at(v).map(|c| (v, c)))
            .expect("a cave");
        let n = crate::underground::habitation_rungs().len();
        for (tenancy, needle) in [
            (Tenancy::Inhabited, "This is the entry of a cut place"),
            (Tenancy::Abandoned, "This was the entry of a cut place"),
        ] {
            let mut origins = crate::underground::wild_origins(n);
            origins[0] = (ChamberOrigin::Made, tenancy);
            let ug = Underground::enter(&terrain, vertex, cave, world.seed, &origins);
            let s = place_sentence(&ug).expect("a Made rung has a sentence");
            assert!(s.starts_with(needle), "{s}");
            let nouns = place_nouns(&ug);
            assert_eq!(nouns.len(), 1 + usize::from(s.ends_with(LANDING_CLAUSE)));
        }
    }
}
