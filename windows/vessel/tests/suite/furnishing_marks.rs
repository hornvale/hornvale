//! The Hearth reaches the wire (The Legend, Task 10): a chamber's own
//! furnishing anchors now emit `PlanMark`s carrying `kind: "furnishing"`,
//! sight-gated exactly as a creature mark is — reusing `Session::sighting`'s
//! own shadowcast rather than inventing a second gating rule.
//!
//! **Both halves of the test, and the negative one matters more.** A single
//! positive-only test passes on an implementation that emits every anchor
//! unconditionally, which is precisely the leak this campaign's brief warns
//! against. Seed 35's second chamber (`role_for`'s chamber index 1, always
//! `Role::Hearthroom`) is the fixed geometry both tests stand on: it composes
//! a real hearth — `examine "a hearth"` answers unconditional of sight,
//! proving the anchor exists — that sits just OUTSIDE the doorway's own
//! shadowcast at the doorway itself and just INSIDE it two steps further in.
//! Found by breadth-first search over the plan's own published grid data,
//! not guessed: seed, chamber index and step counts are this file's own
//! fixed facts, not a coincidence to leave unpinned.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PlanMark, PossessOpts, Session, SpatialChannel, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> World {
    build_world(
        Seed(35),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 35 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// Enter the structure and cross into its SECOND chamber — seed 35's
/// `Hearthroom` — landing exactly at the doorway five steps east of the
/// entry. Panics loudly on any disagreement rather than silently testing
/// nothing: the geometry this file stands on is a fixed fact about seed 35,
/// and any drift here needs a human's attention, not a quietly-skipped test.
fn enter_the_hearthroom(session: &mut Session) {
    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession did not get indoors, so nothing below is tested: {reply}"
    );
    for _ in 0..5 {
        out(session.handle("go e"));
    }
    let snap = session.snapshot().expect("a live session snapshots");
    let SpatialChannel::Chamber { plan } = &snap.spatial else {
        panic!("crossing a threshold must land indoors, not out of doors");
    };
    assert_eq!(
        plan.at, 1,
        "seed 35's geometry drifted: five steps east from the entry no \
         longer crosses into the chamber this file's fixture depends on"
    );
}

/// The `"furnishing"`-kind marks on the current chamber plan.
fn furnishing_marks(session: &Session) -> Vec<PlanMark> {
    let SpatialChannel::Chamber { plan } = &session
        .snapshot()
        .expect("a live session snapshots")
        .spatial
    else {
        panic!("expected the chamber band");
    };
    plan.marks
        .iter()
        .filter(|m| m.kind == "furnishing")
        .cloned()
        .collect()
}

#[test]
fn an_unlit_hearth_is_not_emitted() {
    // The negative half, and the one that would actually leak: a single
    // positive-only test passes on an implementation that emits every
    // furnishing anchor unconditionally, sight or no sight.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    enter_the_hearthroom(&mut session);

    // The hearth is really here — `examine` reads the chamber's own anchor
    // graph unconditional of sight (`examine_chamber`'s own contract), so a
    // refusal here would mean this scenario stopped composing a hearth at
    // all and no longer tests what it claims to.
    let reply = out(session.handle("examine a hearth"));
    assert!(
        !reply.starts_with("You see no"),
        "this test's premise is a hearth that EXISTS but is unlit; \
         `examine` denies it, so the geometry has drifted: {reply}"
    );

    let marks = furnishing_marks(&session);
    assert!(
        !marks.iter().any(|m| m.noun == "a hearth"),
        "the hearth stands outside the shadowcast from the doorway, \
         so a mark here would fabricate a placement the fine layer could \
         not itself have produced: {marks:?}"
    );
}

#[test]
fn a_chamber_with_a_hearth_emits_a_furnishing_mark() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    enter_the_hearthroom(&mut session);
    // Two steps further in than the negative test above: the SAME hearth,
    // now inside the shadowcast — the only thing that changed is sight.
    out(session.handle("go e"));
    out(session.handle("go e"));

    let marks = furnishing_marks(&session);
    let hearth = marks
        .iter()
        .find(|m| m.noun == "a hearth")
        .unwrap_or_else(|| panic!("no furnishing mark named \"a hearth\" among {marks:?}"));
    assert_eq!(hearth.kind, "furnishing");

    // One noun, one datum (§6): the mark's datum must be the SAME line
    // `examine` gives for the identical anchor, not a second wording minted
    // for the wire.
    let examined = out(session.handle("examine a hearth"));
    assert_eq!(
        hearth.datum, examined,
        "the mark's datum and `examine`'s own reply must agree — two \
         sentences for one hearth is the drift §6 exists to prevent"
    );
}
