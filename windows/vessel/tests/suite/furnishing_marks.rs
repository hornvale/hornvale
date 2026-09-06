//! The Hearth reaches the wire (The Legend, Task 10): a chamber's own
//! furnishing anchors now emit `PlanMark`s carrying `kind: "furnishing"`,
//! sight-gated exactly as a creature mark is — reusing `Session::sighting`'s
//! own shadowcast rather than inventing a second gating rule.
//!
//! **Both halves of the test, and the negative one matters more.** A single
//! positive-only test passes on an implementation that emits every anchor
//! unconditionally, which is precisely the leak this campaign's brief warns
//! against. Chamber index 1 is always `Role::Hearthroom` — that part of
//! the geometry is a property of the *structure band*, not of any one
//! seed, and is not this file's to re-derive. It composes a real hearth —
//! `examine "a hearth"` answers unconditional of sight, proving the anchor
//! exists — that sits just OUTSIDE the doorway's own shadowcast somewhere
//! near the doorway and just INSIDE it somewhere further in.
//!
//! **This file used to hardcode seed 35's own coordinates: "five steps
//! east" from the entry, then "two steps further in" for the lit view.**
//! That was exactly the fragility Task 10's own report predicted for
//! itself ("a future genesis change could move the hearth and break
//! [these tests] for an unrelated reason") — and a 593-commit absorption of
//! main was precisely such a change: seed 35's hearth moved, and the fixed
//! step counts pointed at the wrong cells.
//!
//! **The Pavement's 159-commit absorption (2026-08-30/31) hit the SAME
//! prediction a second time, from a different direction.** That campaign
//! moved the walk band to `globe_level + 7` (12 → 13), which moved the
//! sampled locale for every seed and dropped seed 35 below
//! `Terrain::is_cold`'s threshold there. `the-alcove` (`needs_cold: false`)
//! still draws; `the-fire` (`needs_cold: true`, see
//! `crate::interior::pattern`) no longer does, so seed 35's Hearthroom
//! composes "a doorway and an alcove" with no hearth in it at all — this
//! file's own depth-first floor search, described below, could not save it,
//! because the anchor does not exist anywhere in that room at all.
//!
//! **The obvious fix — search seeds, not just floor positions, for one whose
//! Hearthroom still has a hearth — was measured and rejected.** A bounded
//! probe over seeds 0..60 found the first cold-at-chamber-1 seed at 13, but
//! paid ~2.6s per candidate `build_world` + session walk, ~36.6s total to
//! land on it. That is roughly 8-13x this file's own per-test cost (see the
//! `world()` doc below), which the campaign brief that fixed this
//! explicitly gated against ("if a bounded search pushes either test
//! materially slower, the acceptable fallback is a pinned seed"). So this
//! file pins to **seed 13** instead — already confirmed, live, to compose a
//! hearth in chamber 1 — and leans on a premise assertion
//! (`assert_seed_has_a_hearth`) to fail loudly, naming the seed, the moment
//! a future genesis or walk-band change drops seed 13 below the cold
//! threshold too.
//!
//! So neither within-room geometry fact is hardcoded any more:
//!
//! - **Which chamber is the Hearthroom** is never found by walking a
//!   compass direction a fixed number of times. `Session::handle("enter
//!   further in")` (`FURTHER_IN_WORDS`) is direction-agnostic: it always
//!   steps to the lowest-numbered neighbour deeper than the chamber stood
//!   in, landing exactly on the far side of that doorway — the same
//!   arrival cell a directional walk across the same threshold would land
//!   on (`Session::enter`'s own doorway-arrival rule), without needing to
//!   know which compass heading that doorway sits on.
//! - **Which cell inside the Hearthroom is lit and which is not** is found
//!   by a real depth-first search over the room's own floor cells, reading
//!   `SessionPlan`'s published palette (`PaletteEntry::kind`/`chambers`) to
//!   stay off walls and off any threshold to a different chamber, and
//!   using `SessionPlan.marks` itself as the search's stopping condition.
//!   A cell that already satisfies what a test wants is accepted
//!   immediately (a doorway-adjacent unlit cell is the common case, and is
//!   found at depth zero, no different from before); failing to find such
//!   a cell at all is a loud panic, not a silently-vacuous pass.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PlanMark, PossessOpts, Session, SessionPlan, SpatialChannel, Turn};
use hornvale_worldgen::{SettlementPins, build_world};
use std::collections::BTreeSet;

/// PINNED PREMISE: seed 13, chosen (2026-08-31, absorbing The Pavement) as
/// the first seed a bounded 0..60 probe found whose chamber-1 Hearthroom
/// still composes a hearth after the walk band moved to `globe_level + 7` —
/// see the module doc for why a live seed search wasn't kept instead
/// (measured ~36.6s to find one vs. ~4-5s per test today). If this ever
/// starts failing at `assert_seed_has_a_hearth` below, seed 13 has fallen
/// below `Terrain::is_cold`'s threshold at the (possibly again-relocated)
/// walk-band locale, the same way seed 35 did; re-run a bounded seed probe
/// (or re-derive climate) to find the next cold one, rather than assuming
/// this is a geometry regression elsewhere.
fn world() -> World {
    build_world(
        Seed(13),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 13 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// Enter the structure and cross into its deeper chamber — index 1,
/// always `Role::Hearthroom` — landing on the doorway cell,
/// however many steps and whichever heading that doorway happens to sit at
/// on this seed's own layout. Panics loudly on any disagreement rather than
/// silently testing nothing.
fn enter_the_hearthroom(session: &mut Session) {
    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession did not get indoors, so nothing below is tested: {reply}"
    );
    let reply = out(session.handle("enter further in"));
    assert!(
        reply.starts_with("[chamber "),
        "could not step further in from the threshold chamber: {reply}"
    );
    let snap = session.snapshot().expect("a live session snapshots");
    let SpatialChannel::Chamber { plan } = &snap.spatial else {
        panic!("crossing a threshold must land indoors, not out of doors");
    };
    assert_eq!(
        plan.at, 1,
        "`enter further in` from the threshold chamber (index 0) landed in \
         chamber {} instead of 1: the structure's own contract is that the \
         lowest-numbered neighbour deeper than the threshold is always the \
         Hearthroom, so this would mean that contract broke, not that a \
         seed's geometry drifted",
        plan.at
    );
}

/// Assert the pinned-seed premise `world()`'s own doc comment states: seed
/// 13's chamber-1 Hearthroom actually composes a hearth (`examine "a
/// hearth"` answers, unconditional of sight, exactly as it does for every
/// other anchor). Called once per test, before either the negative or
/// positive DFS runs, so a failure here reads as "the pinned seed's premise
/// broke" rather than being buried in the DFS's own per-step assertion of
/// the same fact.
fn assert_seed_has_a_hearth(session: &mut Session) {
    let reply = out(session.handle("examine a hearth"));
    assert!(
        !reply.starts_with("You see no"),
        "PINNED PREMISE BROKEN: seed 13's chamber-1 Hearthroom no longer \
         composes a hearth ({reply:?}). This file pins to seed 13 because a \
         bounded seed probe found it cold at the walk-band-sampled locale \
         after The Pavement moved the walk band to `globe_level + 7` — see \
         this file's module doc. If seed 13 has since fallen below \
         `Terrain::is_cold`'s threshold too (most likely because the walk \
         band moved again), re-run a bounded seed probe to find the next \
         cold seed and re-pin; do not weaken this test to tolerate an unlit \
         room instead."
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

/// Whether `(x, y)` is a floor cell serving chamber `at` — never a wall,
/// never a threshold (a doorway back to the chamber this search started
/// from, or further in to a third), read off the plan's own published
/// palette rather than discovered by bumping into it.
fn is_own_floor(plan: &SessionPlan, x: i32, y: i32, at: usize) -> bool {
    let e = plan.extent;
    if x < e.x || x >= e.x + e.w || y < e.y || y >= e.y + e.h {
        return false;
    }
    let col = (x - e.x) as usize;
    let row = (y - e.y) as usize;
    let idx = row * (e.w as usize) + col;
    let Some(&palette_index) = plan.cells.get(idx) else {
        return false;
    };
    let Some(entry) = plan.palette.get(palette_index as usize) else {
        return false;
    };
    entry.kind == "floor" && entry.chambers == [at]
}

/// The cardinal step back the other way, so a depth-first search can
/// backtrack a floor-to-floor step it already knows is reversible.
fn opposite(dir: &str) -> &'static str {
    match dir {
        "n" => "s",
        "s" => "n",
        "e" => "w",
        "w" => "e",
        _ => unreachable!("opposite only ever receives one of our own four directions"),
    }
}

/// Depth-first search of the chamber `session` currently stands in for a
/// cell whose furnishing marks say `"a hearth"` is lit (`want_lit: true`)
/// or unlit (`want_lit: false`), moving with real `session.handle("go
/// ...")` steps and backtracking with the opposite one. Never leaves the
/// chamber: a neighbour is only ever stepped into once [`is_own_floor`]
/// has already confirmed it belongs to `at`.
///
/// Returns whether a matching cell was found; on success the session is
/// left standing on it (backtracking stops the moment the search
/// succeeds), which is exactly where the caller wants to be to make its
/// own assertions.
fn seek_hearth_view(
    session: &mut Session,
    at: usize,
    want_lit: bool,
    visited: &mut BTreeSet<(i32, i32)>,
) -> bool {
    let (here, lit, neighbours) = {
        let snap = session.snapshot().expect("a live session snapshots");
        let SpatialChannel::Chamber { plan } = &snap.spatial else {
            panic!("expected the chamber band");
        };
        assert_eq!(
            plan.at, at,
            "wandered out of the hearthroom mid-search: now in chamber {}",
            plan.at
        );
        let here = (plan.you.x, plan.you.y);
        let lit = plan
            .marks
            .iter()
            .any(|m| m.kind == "furnishing" && m.noun == "a hearth");
        let neighbours: Vec<&'static str> = [("n", 0, -1), ("e", 1, 0), ("s", 0, 1), ("w", -1, 0)]
            .into_iter()
            .filter_map(|(dir, dx, dy)| {
                let (x, y) = (here.0 + dx, here.1 + dy);
                (!visited.contains(&(x, y)) && is_own_floor(plan, x, y, at)).then_some(dir)
            })
            .collect();
        (here, lit, neighbours)
    };
    visited.insert(here);

    // Every visited cell must still show the hearth as PRESENT to
    // `examine` (unconditional of sight) — the invariant the original
    // fixed-geometry test checked once, held here at every cell the search
    // touches, so a hearth that stopped existing anywhere in the room is
    // caught rather than quietly steered around.
    let hearth_reply = out(session.handle("examine a hearth"));
    assert!(
        !hearth_reply.starts_with("You see no"),
        "the hearth stopped existing in the hearthroom: {hearth_reply}"
    );

    if lit == want_lit {
        return true;
    }
    for dir in neighbours {
        let reply = out(session.handle(&format!("go {dir}")));
        assert!(
            reply.starts_with("You step"),
            "a palette-checked floor step refused: {reply}"
        );
        if seek_hearth_view(session, at, want_lit, visited) {
            return true;
        }
        let back = out(session.handle(&format!("go {}", opposite(dir))));
        assert!(
            back.starts_with("You step"),
            "could not backtrack a reversible floor step: {back}"
        );
    }
    false
}

#[test]
fn an_unlit_hearth_is_not_emitted() {
    // The negative half, and the one that would actually leak: a single
    // positive-only test passes on an implementation that emits every
    // furnishing anchor unconditionally, sight or no sight.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    enter_the_hearthroom(&mut session);
    assert_seed_has_a_hearth(&mut session);

    let mut visited = BTreeSet::new();
    let found = seek_hearth_view(&mut session, 1, false, &mut visited);
    assert!(
        found,
        "no cell in the hearthroom shows the hearth WITHOUT a furnishing \
         mark, searching {} cells — either every cell can see it (the leak \
         this test exists to catch) or the room's geometry could not be \
         walked at all",
        visited.len()
    );

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
        "the found cell stands outside the shadowcast, \
         so a mark here would fabricate a placement the fine layer could \
         not itself have produced: {marks:?}"
    );
}

#[test]
fn a_chamber_with_a_hearth_emits_a_furnishing_mark() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    enter_the_hearthroom(&mut session);
    assert_seed_has_a_hearth(&mut session);

    // The SAME hearth as the negative test above, now sought from a
    // position inside the shadowcast — the only thing that changes between
    // the two tests is where the search stops, never the anchor itself.
    let mut visited = BTreeSet::new();
    let found = seek_hearth_view(&mut session, 1, true, &mut visited);
    assert!(
        found,
        "no cell in the hearthroom brings the hearth into the shadowcast, \
         searching {} cells",
        visited.len()
    );

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
