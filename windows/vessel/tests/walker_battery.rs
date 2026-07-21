//! The walker battery: a deterministic pseudo-random walk asserting the
//! seam's four invariants in every room visited (spec: Determinism and
//! testing, tier 2). Bounded to one binary, one world, WALK_STEPS steps —
//! measured fast-gate cheap; if it outgrows that, move it behind a
//! `heavy:` ignore-reason token (see cli/tests/heavy_tier.rs).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World, WorldTime};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn, knowledge_is_subset, streams::VESSEL_WALK};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

const WALK_STEPS: usize = 48;

fn seam_world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

#[test]
fn the_walker_battery_holds_the_four_invariants() {
    let world = seam_world();
    let at = WorldTime { day: 0.0 };
    let (mut session, _) = Session::start(
        &world,
        &PossessOpts {
            day: at,
            echo: false,
        },
    )
    .unwrap();
    let mut stream = world.seed.derive_typed(VESSEL_WALK).stream();

    for step in 0..WALK_STEPS {
        let here = session.agent().position.pack().unwrap().0;

        // Invariant 3+4: focalizer totality and examine totality.
        let f = session.focalized().unwrap();
        assert!(!f.prose.is_empty(), "step {step}: prose renders");
        let prose = f.prose.to_lowercase();
        for (noun, _) in &f.nouns {
            assert!(
                prose.contains(&noun.to_lowercase()),
                "step {step}: look mentions '{noun}'"
            );
            match session.handle(&format!("examine {noun}")) {
                Turn::Out(t) => assert!(
                    !t.starts_with("You see no"),
                    "step {step}: examine contract broken for '{noun}'"
                ),
                Turn::Released(_) => panic!("step {step}: examine must not release"),
            }
        }

        // Invariant 1: the accumulated knowledge is a subset of truth.
        knowledge_is_subset(session.knowledge(), &world, session.context(), at)
            .unwrap_or_else(|e| panic!("step {step}: subset broken: {e}"));

        // Walk one deterministic step. The stream draw happens
        // unconditionally (never inside a branch) so the walk consumes the
        // same draws regardless of assertion outcomes above. Ways come
        // from the mesh's neighbors (three per room, always); move through
        // the public verb surface only (`go <dir>`), never by mutating
        // position directly.
        let ways = session.ways();
        let pick = (stream.next_f64() * ways.len() as f64) as usize;
        let pick = pick.min(ways.len() - 1);
        let (dir, target) = ways[pick];
        let word = format!("{dir:?}").to_lowercase();
        match session.handle(&format!("go {word}")) {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("step {step}: go must not release"),
        }
        let after = session.agent().position.pack().unwrap().0;
        assert_eq!(
            after, target,
            "step {step}: go {word} reached the picked neighbor"
        );

        // Invariant 2: exit reciprocity — the new room's lateral exits
        // include where we came from.
        let back_ids: Vec<u64> = session.ways().into_iter().map(|(_, to)| to).collect();
        assert!(
            back_ids.contains(&here),
            "step {step}: reciprocity broken — no way back to {here}"
        );
    }
}
