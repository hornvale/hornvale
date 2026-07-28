//! Decision 0024's deferred remedy, under test (The Wearing, Task 10):
//! a rendered document that names two same-named settlements disambiguates
//! them from their own site facts, and one that names only one leaves it
//! bare.
//!
//! The scope under test is the **document**, not the world. Every test here
//! renders through the real public surface (`render_connections`) rather than
//! poking at the qualifier directly, because the claim 0024 makes is about
//! what a reader sees, not about an internal API.
//!
//! Like `connections_render.rs`, this needs no `hornvale-worldgen`: the graph
//! is hand-built and the world is a hand-committed ledger.

use hornvale_almanac::connections::render_connections;
use hornvale_kernel::{CellId, EntityId, Fact, Seed, Value, World};
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};

/// One settlement to plant in the fixture world: which cell it sits on, its
/// (possibly colliding) name, its people, its biome, and its coordinate.
struct Site {
    cell: u32,
    name: &'static str,
    people: &'static str,
    biome: &'static str,
    latitude: f64,
    longitude: f64,
}

/// A world holding `sites` as real settlements, committed against the same
/// predicate definitions the settlement/species domains register.
fn world_with(sites: &[Site]) -> World {
    let mut world = World::new(Seed(1));
    for (predicate, doc) in [
        (
            hornvale_settlement::IS_SETTLEMENT,
            "subject is a settlement",
        ),
        (hornvale_settlement::CELL_ID, "cell id"),
        (hornvale_settlement::BIOME, "biome of a place"),
        (
            hornvale_settlement::LATITUDE,
            "settlement latitude, degrees",
        ),
        (
            hornvale_settlement::LONGITUDE,
            "settlement longitude, degrees",
        ),
        (
            hornvale_species::PEOPLED_BY,
            "the species that peoples a settlement",
        ),
        (hornvale_kernel::NAME, "canonical name of an entity"),
    ] {
        world
            .registry
            .register_predicate(predicate, true, doc)
            .expect("fixture predicate registers");
    }

    for (index, site) in sites.iter().enumerate() {
        let id = EntityId::new(index as u64 + 1).expect("nonzero entity id");
        let registry = world.registry.clone();
        let mut commit = |predicate: &str, object: Value| {
            world
                .ledger
                .commit(
                    Fact {
                        subject: id,
                        predicate: predicate.to_string(),
                        object,
                        place: None,
                        day: None,
                        provenance: "test fixture".to_string(),
                    },
                    &registry,
                )
                .expect("fixture fact commits");
        };
        commit(hornvale_settlement::IS_SETTLEMENT, Value::Flag(true));
        commit(hornvale_kernel::NAME, Value::Text(site.name.to_string()));
        commit(
            hornvale_settlement::CELL_ID,
            Value::Number(f64::from(site.cell)),
        );
        commit(
            hornvale_settlement::BIOME,
            Value::Text(site.biome.to_string()),
        );
        commit(
            hornvale_species::PEOPLED_BY,
            Value::Text(site.people.to_string()),
        );
        commit(hornvale_settlement::LATITUDE, Value::Number(site.latitude));
        commit(
            hornvale_settlement::LONGITUDE,
            Value::Number(site.longitude),
        );
    }
    world
}

/// A land route from `from` to each of `to`, on a graph of `size` cells.
fn routes_from(size: u32, from: u32, to: &[u32]) -> ConnectionGraph {
    let mut graph = ConnectionGraph::new(size as usize);
    for &t in to {
        graph.add_edge(
            CellId(from),
            Edge {
                to: CellId(t),
                kind: EdgeKind::LandRoute,
                conductance: 0.5,
            },
        );
    }
    graph
}

/// Two same-named settlements differing in biome, one differently-named
/// neighbour, and a hub (cell 0, no settlement) whose land routes name all
/// three.
fn three_neighbours() -> World {
    world_with(&[
        Site {
            cell: 1,
            name: "Ice-Home",
            people: "kobold",
            biome: "taiga",
            latitude: 61.5,
            longitude: -12.25,
        },
        Site {
            cell: 2,
            name: "Ice-Home",
            people: "kobold",
            biome: "desert",
            latitude: -30.0,
            longitude: 44.5,
        },
        Site {
            cell: 3,
            name: "Sun-Home",
            people: "goblin",
            biome: "taiga",
            latitude: 5.0,
            longitude: 5.0,
        },
    ])
}

#[test]
fn two_same_named_settlements_in_one_document_are_both_qualified() {
    let world = three_neighbours();
    let text = render_connections(&world, CellId(0), &routes_from(4, 0, &[1, 2, 3]));

    // Both Ice-Homes are named in this document, so both carry a qualifier
    // drawn from their own site facts -- and the two qualifiers differ, which
    // is the whole point (a qualifier that does not qualify is worthless).
    assert!(
        text.contains("Ice-Home (taiga)"),
        "the taiga Ice-Home is qualified:\n{text}"
    );
    assert!(
        text.contains("Ice-Home (desert)"),
        "the desert Ice-Home is qualified:\n{text}"
    );
    // Non-vacuity: the document must not still be carrying a bare "Ice-Home"
    // anywhere -- if it did, the assertions above could be satisfied while a
    // third, unqualified mention sat beside them.
    assert!(
        !bare_mention(&text, "Ice-Home"),
        "no bare Ice-Home survives in a document naming two of them:\n{text}"
    );
    // Lazy qualification: Sun-Home has no twin here, so it pays nothing.
    assert!(
        bare_mention(&text, "Sun-Home"),
        "the unambiguous neighbour stays bare:\n{text}"
    );
}

#[test]
fn a_settlement_named_alone_is_bare_even_when_the_world_holds_its_twin() {
    // The scope is the DOCUMENT, not the world. Same world both times; only
    // the roster the document names changes.
    let world = three_neighbours();

    let alone = render_connections(&world, CellId(0), &routes_from(4, 0, &[1, 3]));
    assert!(
        bare_mention(&alone, "Ice-Home"),
        "an Ice-Home whose twin this document never names stays bare:\n{alone}"
    );
    assert!(
        bare_mention(&alone, "Sun-Home"),
        "nor is its unambiguous neighbour qualified:\n{alone}"
    );
    assert!(
        !alone.contains("(taiga)"),
        "no site fact is spent as a qualifier at all:\n{alone}"
    );

    // Positive control, same world: the twin-naming document DOES qualify.
    // Without this half the test above would pass on a tree that never
    // qualifies anything.
    let together = render_connections(&world, CellId(0), &routes_from(4, 0, &[1, 2]));
    assert!(
        together.contains("Ice-Home (taiga)") && together.contains("Ice-Home (desert)"),
        "the same world qualifies when both twins are in the room:\n{together}"
    );
}

#[test]
fn twins_of_two_different_peoples_are_qualified_by_their_people() {
    // The other prose shape 0024 names. Biome could not separate these two
    // anyway (both taiga), so this is the only rung that can fire.
    let world = world_with(&[
        Site {
            cell: 1,
            name: "Ice-Home",
            people: "kobold",
            biome: "taiga",
            latitude: 61.5,
            longitude: -12.25,
        },
        Site {
            cell: 2,
            name: "Ice-Home",
            people: "goblin",
            biome: "taiga",
            latitude: -30.0,
            longitude: 44.5,
        },
    ]);
    let text = render_connections(&world, CellId(0), &routes_from(3, 0, &[1, 2]));
    assert!(
        text.contains("Ice-Home of the kobolds"),
        "the kobold Ice-Home is qualified by its people:\n{text}"
    );
    assert!(
        text.contains("Ice-Home of the goblins"),
        "the goblin Ice-Home is qualified by its people:\n{text}"
    );
    assert!(
        !text.contains("(taiga)"),
        "a qualifier that cannot separate them must not be spent:\n{text}"
    );
}

#[test]
fn twins_alike_in_people_and_biome_fall_through_to_their_coordinates() {
    let world = world_with(&[
        Site {
            cell: 1,
            name: "Ice-Home",
            people: "kobold",
            biome: "taiga",
            latitude: 61.5,
            longitude: -12.25,
        },
        Site {
            cell: 2,
            name: "Ice-Home",
            people: "kobold",
            biome: "taiga",
            latitude: -30.0,
            longitude: 44.5,
        },
    ]);
    let text = render_connections(&world, CellId(0), &routes_from(3, 0, &[1, 2]));
    assert!(
        text.contains("Ice-Home (61.5°N, 12.2°W)"),
        "the northern twin falls through to its coordinate:\n{text}"
    );
    assert!(
        text.contains("Ice-Home (30.0°S, 44.5°E)"),
        "the southern twin falls through to its coordinate:\n{text}"
    );
}

#[test]
fn one_settlement_reached_twice_is_not_an_ambiguity() {
    // Cell 1 is reachable by both a sea-lane and a land route, so it is named
    // twice in the same document -- but it is one place, not two, and must
    // not be qualified. The contrast half (cell 2's genuine twin) keeps this
    // from passing on a tree that never qualifies anything.
    let world = three_neighbours();
    let mut graph = ConnectionGraph::new(4);
    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(1),
            kind: EdgeKind::WaterRoute,
            conductance: 0.5,
        },
    );
    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(1),
            kind: EdgeKind::LandRoute,
            conductance: 0.5,
        },
    );
    let text = render_connections(&world, CellId(0), &graph);
    assert_eq!(
        text.matches("Ice-Home").count(),
        2,
        "the one place is named on both route lines:\n{text}"
    );
    assert!(
        bare_mention(&text, "Ice-Home"),
        "naming one place twice is not an ambiguity:\n{text}"
    );

    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(2),
            kind: EdgeKind::LandRoute,
            conductance: 0.5,
        },
    );
    let contrast = render_connections(&world, CellId(0), &graph);
    assert!(
        !bare_mention(&contrast, "Ice-Home"),
        "adding the genuine twin does qualify both:\n{contrast}"
    );
}

#[test]
fn the_documents_own_subject_is_qualified_against_its_neighbours() {
    // The subject is a member of its own document's roster, not a privileged
    // outsider: at seed 42, 38 of 169 `connections` documents have a
    // qualified subject, so a roster that omitted it would render a bare
    // header above qualified destinations of the same name.
    let world = world_with(&[
        Site {
            cell: 0,
            name: "Ice-Home",
            people: "kobold",
            biome: "taiga",
            latitude: 61.5,
            longitude: -12.25,
        },
        Site {
            cell: 1,
            name: "Ice-Home",
            people: "kobold",
            biome: "desert",
            latitude: -30.0,
            longitude: 44.5,
        },
        Site {
            cell: 2,
            name: "Sun-Home",
            people: "goblin",
            biome: "taiga",
            latitude: 5.0,
            longitude: 5.0,
        },
    ]);
    let text = render_connections(&world, CellId(0), &routes_from(3, 0, &[1, 2]));
    assert!(
        text.starts_with("The connections of Ice-Home (taiga)\n"),
        "the subject's own header carries its qualifier:\n{text}"
    );
    assert!(
        text.contains("Ice-Home (desert)"),
        "and its same-named neighbour carries a different one:\n{text}"
    );
    assert!(
        !bare_mention(&text, "Ice-Home"),
        "no bare Ice-Home survives, subject included:\n{text}"
    );
    // The header rule underlines to the label's own width; a qualifier that
    // widened the title without widening the rule would be a visible defect.
    let mut lines = text.lines();
    let header = lines.next().expect("a header line");
    let rule = lines.next().expect("an underline");
    assert_eq!(
        rule.chars().count(),
        header.chars().count(),
        "the underline tracks the qualified header:\n{text}"
    );
}

#[test]
fn qualification_is_deterministic() {
    let world = three_neighbours();
    let graph = routes_from(4, 0, &[1, 2, 3]);
    assert_eq!(
        render_connections(&world, CellId(0), &graph),
        render_connections(&world, CellId(0), &graph)
    );
}

/// Whether `text` mentions `name` at least once with no qualifier attached --
/// that is, followed by something other than the ` (` or ` of the ` a
/// qualifier opens with. Deliberately not `contains`: every qualified
/// mention contains the bare name as a prefix, so `contains` can never tell
/// the two apart.
fn bare_mention(text: &str, name: &str) -> bool {
    text.match_indices(name).any(|(at, _)| {
        let rest = &text[at + name.len()..];
        !rest.starts_with(" (") && !rest.starts_with(" of the ")
    })
}
