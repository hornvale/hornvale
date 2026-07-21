//! The legibility surface's acceptance test: a hand-built deep history
//! committed to a fresh world, read back as prose. The headline is the
//! findable goblin doll — the whole campaign in one string.

use hornvale_almanac::history::render_site;
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed, World};
use hornvale_worldgen::{History, emit_history};

fn eid(n: u64) -> EntityId {
    EntityId::new(n).unwrap()
}

fn test_world() -> World {
    let mut w = World::new(Seed(42));
    hornvale_history::register_concepts(&mut w.registry).unwrap();
    hornvale_settlement::register_concepts(&mut w.registry).unwrap();
    w
}

/// A record with neutral defaults; each case overrides what it cares about.
fn base_record(community: u64, people: &'static str, site: u32, founded: f64) -> OccupationRecord {
    OccupationRecord {
        people: KindId(people),
        community: eid(community),
        lineage: eid(community),
        site: CellId(site),
        founded,
        ended: None,
        peak_population: 50,
        tech: TechHorizon::Neolithic,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: None,
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(CellId(site)),
        notability: Notability::Common,
    }
}

/// A single young goblin hamlet at cell 3, put to the torch — the brief's
/// acceptance case.
fn history_with_a_recently_burned_goblin_hamlet_at(cell: u32) -> History {
    let mut hamlet = base_record(10, "goblin", cell, 340.0);
    hamlet.ended = Some(1980.0);
    hamlet.peak_population = 40; // hamlet-scale (<= 150)
    hamlet.tech = TechHorizon::Bronze;
    hamlet.cause = Some(CauseOfEnd::Burned);
    hamlet.ended_by = Ended::Nature;
    hamlet.notability = Notability::Backwater;
    History::new(vec![hamlet], 2000.0)
}

#[test]
fn a_burned_goblin_clearing_shows_its_doll() {
    let mut w = test_world();
    emit_history(&mut w, &history_with_a_recently_burned_goblin_hamlet_at(3)).unwrap();
    let text = render_site(&w, CellId(3));
    assert!(text.contains("goblin"), "names the people:\n{text}");
    assert!(text.contains("burned"), "names the cause:\n{text}");
    assert!(
        text.to_lowercase().contains("doll"),
        "the doll in the grass — the whole campaign, in one string:\n{text}"
    );
}

/// A migrated (climate-abandoned) young goblin hamlet also leaves a doll —
/// the Nathan-approved archaeological-realism extension that makes the real
/// world's ruins (all `Migrated`) legible.
#[test]
fn a_migrated_goblin_clearing_also_shows_its_doll() {
    let mut w = test_world();
    let mut hamlet = base_record(10, "goblin", 3, 340.0);
    hamlet.ended = Some(1980.0);
    hamlet.peak_population = 40;
    hamlet.cause = Some(CauseOfEnd::Migrated);
    let hist = History::new(vec![hamlet], 2000.0);
    emit_history(&mut w, &hist).unwrap();
    let text = render_site(&w, CellId(3));
    assert!(text.to_lowercase().contains("doll"), "{text}");
    assert!(text.contains("migrated"), "{text}");
}

/// A restacked site reads as a stratigraphy: two layers, the newer founded
/// from the older's fleeing survivors — the ★ `founded_from` thread rendered
/// in prose.
#[test]
fn a_restacked_site_reads_as_stratigraphy() {
    let mut w = test_world();
    // Deep layer: a goblin hamlet that fled the ice (migrated) off cell 7.
    let mut first = base_record(1, "goblin", 7, 100.0);
    first.ended = Some(400.0);
    first.cause = Some(CauseOfEnd::Migrated);
    // Later layer at cell 3, founded by the first's survivors.
    let mut second = base_record(2, "goblin", 3, 420.0);
    second.founded_from = Founding::From(eid(1)); // settlers from the first community
    second.ended = Some(900.0);
    second.cause = Some(CauseOfEnd::Famine);
    // A third, still-living layer restacked on cell 3.
    let mut third = base_record(3, "kobold", 3, 1500.0);
    third.founded_from = Founding::Genesis(CellId(3));
    let hist = History::new(vec![first, second, third], 2000.0);
    emit_history(&mut w, &hist).unwrap();

    let text = render_site(&w, CellId(3));
    // Two layers stacked on cell 3 (the deep goblin layer + the living kobold).
    assert!(
        text.contains("lives have passed over this ground"),
        "{text}"
    );
    // The founded_from thread: the goblin layer's founders fled cell 7.
    assert!(
        text.contains("fled the ice"),
        "the founded-from thread:\n{text}"
    );
    assert!(text.contains("cell 7"), "{text}");
}

#[test]
fn an_empty_cell_says_so() {
    let w = test_world();
    let text = render_site(&w, CellId(99));
    assert!(text.contains("Nothing ever settled here"), "{text}");
}
