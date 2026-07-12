//! Localized golden for proto-goblinoid's ancestor draw (Task 9).
//!
//! `draw_phonology` is a hidden hub: it builds BOTH the proto-goblinoid
//! inventory AND every daughter's own inventory (goblin, hobgoblin,
//! bugbear each nativize from it). A change to it silently re-baselines
//! the whole family through this one shared proto — a diamond dependency.
//! The dictionary/proto reference pages catch that drift at page scale
//! (regenerate-and-diff), but a page-scale diff is an opaque wall of text.
//! This test fails LOUDLY at one spot with a clear message instead: if
//! either assertion below breaks, the ancestor moved, and every daughter's
//! cognate forms moved with it.
use hornvale_kernel::{Seed, World};
use hornvale_language::{ipa, proto_root, render_views, romanize};
use hornvale_worldgen::{proto_phonology_of, register_all};

/// The family this golden pins — the campaign's only multi-member family.
const FAMILY: &str = "goblinoid";

/// The reference seed the committed snapshots were captured at — the same
/// seed 42 used throughout the book's reference pages.
const REFERENCE_SEED: u64 = 42;

/// A bare world at the reference seed with the full concept registry
/// registered (`register_all`), but no terrain/settlement genesis: both
/// snapshots below need only the seed and the concept-name inventory,
/// which `register_all` alone provides.
fn reference_world() -> World {
    let mut world = World::new(Seed(REFERENCE_SEED));
    register_all(&mut world.registry).expect("register_all cannot fail on an empty registry");
    world
}

/// `proto_phonology_of(seed42, "goblinoid").inventory`, rendered one
/// segment per line as `<romanization> /<ipa>/` — a stable, human-readable
/// snapshot currency (rather than the enum's raw `Debug` form).
fn render_inventory_snapshot(world: &World) -> String {
    let phonology = proto_phonology_of(world, FAMILY);
    let mut lines: Vec<String> = phonology
        .inventory
        .iter()
        .map(|seg| format!("{} /{}/", romanize(seg), ipa(seg)))
        .collect();
    lines.push(String::new());
    lines.join("\n")
}

/// The seed-42 proto-root table: every registered concept's proto-root
/// (`hornvale_language::proto_root`, family-keyed), one `<concept>: <roman>
/// /<ipa>/` line per concept in concept-id order — the same reduction the
/// `hornvale proto` reference page renders, reconstructed independently
/// here so this test never depends on the `cli` crate (layering:
/// `windows/worldgen` sits below `cli`).
fn render_root_table_snapshot(world: &World) -> String {
    let phonology = proto_phonology_of(world, FAMILY);
    let mut lines = Vec::new();
    for concept in world.registry.concepts() {
        let proto = proto_root(&world.seed, FAMILY, &concept.name, &phonology);
        let views = render_views(&proto);
        lines.push(format!("{}: {} /{}/", concept.name, views.roman, views.ipa));
    }
    lines.push(String::new());
    lines.join("\n")
}

#[test]
fn proto_goblinoid_inventory_matches_the_committed_snapshot() {
    let world = reference_world();
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/proto-goblinoid-inventory-seed-42.txt"
        )),
        &render_inventory_snapshot(&world),
        "proto-goblinoid's inventory drifted from the committed snapshot — see this file's \
         module doc: every daughter (goblin/hobgoblin/bugbear) nativizes from this exact \
         inventory, so this is the shared-ancestor diamond dependency moving. If deliberate, \
         regenerate the fixture in this commit and record why in the chronicle.",
    );
}

#[test]
fn proto_goblinoid_root_table_matches_the_committed_snapshot() {
    let world = reference_world();
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/proto-goblinoid-root-table-seed-42.txt"
        )),
        &render_root_table_snapshot(&world),
        "proto-goblinoid's proto-root table drifted from the committed snapshot — see this \
         file's module doc: every daughter's cognate forms are evolved from these exact \
         proto-roots, so this is the shared-ancestor diamond dependency moving. If deliberate, \
         regenerate the fixture in this commit and record why in the chronicle.",
    );
}

/// Not a regression guard on its own (a full duplicate would defeat the
/// point of a localized golden), but a cheap sanity check that the two
/// snapshots stay mutually consistent: an inventory-changing edit that
/// somehow left the root table byte-identical would be a stranger bug than
/// either test alone catches.
#[test]
fn both_snapshots_are_non_empty_and_reference_a_shared_seed() {
    let world = reference_world();
    assert!(!render_inventory_snapshot(&world).trim().is_empty());
    assert!(!render_root_table_snapshot(&world).trim().is_empty());
}
