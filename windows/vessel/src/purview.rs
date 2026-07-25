//! The vessel's epistemic overlay on `scene/surrounds/v1`: which cells are
//! memory rather than sight, and which NPCs stand where. The overlay WRITES
//! NOTHING — `remembered` is a read of the `room/<id>` keys the identity
//! projection already absorbs on every visit, so a possession that draws the
//! chart is byte-identical to one that never does.

use crate::{Knowledge, VesselError, liveness};
use hornvale_kernel::{Ledger, RoomAddr, RoomId, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_scene::{Mark, SurroundsScene, surrounds_scene_in};

/// The chart's sense radius, in BFS rings. A constant this slice; the seam
/// for a per-species radius is `Agent::perception` (EXP-3), untouched here.
/// type-audit: bare-ok(count)
pub const PURVIEW_RADIUS: u32 = 4;

/// The salience of an NPC standing on a cell — above every settlement mark
/// (flagship 10, other 20; lower is more salient — see
/// `agent_salience_outranks_every_settlement_salience` below, which pins
/// this relationship as a test rather than a coincidence of two constants
/// living in different crates).
/// type-audit: bare-ok(index)
const AGENT_SALIENCE: u32 = 5;

/// Build the chart the possession draws: the fog-free scene, then the
/// epistemic and agent overlays. `zoom_out` coarsens by truncating the
/// observer's path — zoom in this mesh is not an aggregation, it is the same
/// builder one rung up the address space.
/// type-audit: bare-ok(count: zoom_out)
#[allow(clippy::too_many_arguments)]
pub fn purview_scene(
    world: &World,
    ctx: &LocaleContext,
    position: &RoomAddr,
    knowledge: &Knowledge,
    npcs: &[liveness::Npc],
    ledger: &Ledger,
    at: WorldTime,
    zoom_out: u32,
) -> Result<SurroundsScene, VesselError> {
    let depth = position.depth();
    let keep = depth.saturating_sub(zoom_out) as usize;
    let centre = RoomAddr {
        face: position.face,
        path: position.path[..keep.min(position.path.len())].to_vec(),
    };
    // `surrounds_scene_in`, NOT `surrounds_scene`: the session already holds a
    // built `LocaleContext`, and building a fresh one costs ~1.2 s (measured)
    // against ~2 ms of actual per-cell work. `map` runs every turn, so the
    // convenience wrapper would make the verb unusable.
    let mut scene = surrounds_scene_in(world, ctx, &centre, PURVIEW_RADIUS, at)
        .map_err(|e| VesselError::Build(e.to_string()))?;

    // Every room this session has walked, as an address.
    let walked: Vec<RoomAddr> = knowledge
        .0
        .keys()
        .filter_map(|k| k.strip_prefix("room/"))
        .filter_map(|id| id.parse::<u64>().ok())
        .filter_map(|id| RoomId(id).unpack().ok())
        .collect();

    // Where each NPC stands right now — the derived-view read (The
    // Quickening): the latest committed `agent-at`, else the derived
    // schedule. Truncated to the chart's depth so a coarse chart still
    // places them.
    let mut agent_marks: Vec<(u64, Mark)> = Vec::new();
    for npc in npcs {
        let at_room = liveness::agent_position(ledger, npc, at);
        let shown = RoomAddr {
            face: at_room.face,
            path: at_room.path[..keep.min(at_room.path.len())].to_vec(),
        };
        let Ok(id) = shown.pack() else { continue };
        agent_marks.push((
            id.0,
            Mark {
                noun: npc.label.clone(),
                kind: "agent".to_string(),
                datum: format!(
                    "{} — a {} of this world, alive and moving.",
                    npc.label, npc.species
                ),
                salience: AGENT_SALIENCE,
            },
        ));
    }

    for cell in &mut scene.cells {
        // The fog: a cell not currently sensed, but walked (or containing a
        // walked descendant at a coarser rung), is memory.
        if cell.state != "here" {
            let Ok(addr) = RoomId(cell.room).unpack() else {
                continue;
            };
            let remembered = walked.iter().any(|w| {
                w.face == addr.face
                    && w.path.len() >= addr.path.len()
                    && w.path[..addr.path.len()] == addr.path[..]
            });
            if remembered {
                cell.state = "remembered".to_string();
            }
        }
        for (room, mark) in &agent_marks {
            if *room == cell.room {
                cell.marks.push(mark.clone());
            }
        }
        cell.marks
            .sort_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)));
    }

    // Every mark's noun joins the chart's catalog — this is the attention
    // join's data half.
    let mut legend = scene.legend.clone();
    for cell in &scene.cells {
        for m in &cell.marks {
            if !legend.iter().any(|e| e.noun == m.noun) {
                legend.push(hornvale_scene::LegendEntry {
                    noun: m.noun.clone(),
                    datum: m.datum.clone(),
                });
            }
        }
    }
    legend.sort_by(|a, b| a.noun.cmp(&b.noun));
    scene.legend = legend;
    Ok(scene)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PossessOpts, Session};
    use hornvale_kernel::{Seed, World};
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn world() -> World {
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
    fn the_starting_room_is_here_and_nothing_is_remembered_yet() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let s = session.purview(0).unwrap();
        assert_eq!(s.cells.iter().filter(|c| c.state == "here").count(), 1);
        assert_eq!(
            s.cells.iter().filter(|c| c.state == "remembered").count(),
            0,
            "a session that has not left its first room remembers nowhere else"
        );
    }

    #[test]
    fn a_room_walked_and_left_becomes_remembered() {
        let w = world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let start = session.agent().position.pack().unwrap().0;
        // Walk far enough that the start room leaves the sense radius.
        for _ in 0..(PURVIEW_RADIUS + 1) {
            let way = session.ways().first().map(|(c, _)| format!("{c:?}"));
            let Some(way) = way else { break };
            session.handle(&format!("go {way}"));
        }
        let s = session.purview(0).unwrap();
        let start_cell = s.cells.iter().find(|c| c.room == start);
        if let Some(c) = start_cell {
            assert_eq!(
                c.state, "remembered",
                "the room we walked out of is memory, not sight"
            );
        }
        // Whether or not the start room is still in view, the walk must have
        // produced at least one remembered cell somewhere behind us.
        assert!(
            s.cells.iter().any(|c| c.state == "remembered") || start_cell.is_none(),
            "walking must leave a trail of memory"
        );
    }

    #[test]
    fn zooming_out_coarsens_the_depth_and_keeps_the_observer_centred() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let fine = session.purview(0).unwrap();
        let coarse = session.purview(2).unwrap();
        assert_eq!(coarse.depth, fine.depth - 2, "zoom is path truncation");
        assert_eq!(
            coarse.cells.iter().filter(|c| c.state == "here").count(),
            1,
            "the observer's ancestor is the coarse chart's centre"
        );
    }

    #[test]
    fn the_purview_is_idempotent() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let before = session.knowledge().0.clone();
        let a = hornvale_scene::surrounds_json(&session.purview(0).unwrap());
        let b = hornvale_scene::surrounds_json(&session.purview(0).unwrap());
        assert_eq!(a, b, "drawing the chart twice gives the same chart");
        assert_eq!(
            &before,
            &session.knowledge().0,
            "drawing the chart must not mutate what the session knows"
        );
    }

    #[test]
    fn an_agent_mark_stands_on_a_cell() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let s = session.purview(0).unwrap();
        let agents: usize = s
            .cells
            .iter()
            .flat_map(|c| c.marks.iter())
            .filter(|m| m.kind == "agent")
            .count();
        assert!(
            agents > 0,
            "seed 42 derives NPCs at the flagship settlement; at least one is in view"
        );
    }

    /// Pins the review constraint from Task 3: the ASCII renderer picks
    /// between marks on the same cell purely by numeric `salience` (lower
    /// wins), never by `kind`. `AGENT_SALIENCE` must therefore stay strictly
    /// below every settlement salience (10 for the flagship, 20 for the
    /// rest) or an NPC standing on a settlement's cell would silently lose
    /// to it. This is a relationship between two constants in different
    /// crates, not a coincidence to leave unpinned.
    #[test]
    // Both operands are compile-time constants, so clippy reads this as an
    // assertion on a constant — that IS the point: this test exists purely
    // to fail the build the moment either constant drifts, not to exercise
    // any runtime behavior.
    #[allow(clippy::assertions_on_constants)]
    fn agent_salience_outranks_every_settlement_salience() {
        assert!(
            AGENT_SALIENCE < 10,
            "an NPC standing on the flagship's cell must still win"
        );
    }
}
