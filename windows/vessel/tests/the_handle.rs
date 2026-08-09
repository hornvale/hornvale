//! The Handle's gate: every significant word of every catalog entry's display
//! name resolves. It fails 6-of-7 against pre-campaign code, which is why it
//! exists — all four defects this campaign fixed were words a player would
//! obviously type against a name the prose had just used.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

#[test]
fn every_significant_word_of_every_catalog_entry_resolves() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Two rooms, because one room is an anecdote.
    for step in 0..2 {
        if step > 0 {
            let _ = session.handle("go n");
        }
        let nouns = session.lens_nouns().expect("the lens has nouns");
        assert!(!nouns.is_empty(), "a room names something");
        for n in &nouns {
            for word in &n.words {
                let reply = match session.handle(&format!("examine {word}")) {
                    Turn::Out(t) => t,
                    Turn::Released(_) => panic!("examine must not release"),
                };
                assert!(
                    !reply.starts_with("You see no"),
                    "room {step}: entry {:?} declares the word {word:?}, and examine refuses it",
                    n.display
                );
            }
        }
    }
}
