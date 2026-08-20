//! The Deed, Task 6: group B's out-of-character halves render the
//! **objective** view — the existing renderer's own gating parameters taken
//! to their permissive limit, never a second rendering path.
//!
//! # Every test here is a DISCRIMINATOR, and that is the whole point
//!
//! An out-of-character form that is observationally identical to its
//! in-character twin is a no-op, and shipping it would be worse than not
//! shipping it: it would advertise a capability the surface does not have.
//! So each verb gets a test that shows the two forms **differ**, on a
//! fixture where the gate actually withholds something, and only the verbs
//! that have one are shipped.
//!
//! Two of spec §3.2's six group-B verbs are **not** shipped, and
//! [`neither_look_nor_knows_has_an_objective_half`] records that as a
//! finding rather than leaving it to be rediscovered:
//!
//! - **`knows`** (`Session::knows`) dumps `self.knowledge` wholesale with no
//!   gate at all. The player's knowledge *is* the subject, so there is no
//!   permissive limit to take.
//! - **`look`** (`Session::describe_here` / `describe_chamber_here` /
//!   `describe_underground_here`) has no perceptual gate either — none of
//!   the three band arms consults `sensed_npcs`, `sighting`, `eyes`, `lens`
//!   or `knowledge`. `look` renders the place, and the place is objective
//!   already. Making `!look` list creatures would be a *new object*, not a
//!   permissive limit, which is precisely the fork this task forbids.
//!
//! # Why the fixtures are SEARCHED, not pinned
//!
//! Three of the four discriminators need a world where a creature stands in
//! the possession's chamber and the shadowcast does **not** reach it. That
//! is a contingent property of whichever chamber the possession opens in —
//! `tests/common/mod.rs` records at length how pinning one seed for exactly
//! this class of precondition rotted the moment the flagship was reseeded.
//! So each search states its precondition, stops at its first hit, and
//! **panics loudly** naming the predicate when the range yields nothing: a
//! search that quietly found nothing and let its test pass would be strictly
//! worse than the hardcoded seed it replaces.
//!
//! The preconditions here are read through the **public** surface only —
//! `Session::purview`'s walk-band agent marks for "a creature is co-located"
//! and `SessionSnapshot::sensed.present` for "the body senses it" — so the
//! fixture is established by something other than the feature under test.

use hornvale_vessel::{PossessOpts, Session, Turn};

/// The seeds searched, matching `tests/common/mod.rs`'s `SIGHT_SEEDS`. Wide
/// enough that "no world in here withholds a creature" would be a real
/// finding about the sim rather than about the sample.
const SIGHT_SEEDS: std::ops::Range<u64> = 0..64;

/// A world at `seed`, or `None` if this seed has no world to build.
fn world_at(seed: u64) -> Option<hornvale_kernel::World> {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .ok()
}

/// The seed-42 default world every non-fixture test in this file uses.
fn world() -> hornvale_kernel::World {
    world_at(42).expect("seed 42 builds")
}

/// The text of a turn that must not end the possession.
fn out(session: &mut Session<'_>, line: &str) -> String {
    match session.handle(line) {
        Turn::Out(text) => text,
        Turn::Released(_) => panic!("`{line}` must not end the possession"),
    }
}

/// This crate's unknown-verb refusal, lowercased for a case-insensitive
/// check — the same probe `ooc_namespace.rs` uses. Every test below asserts
/// the objective form is **not** this before asserting it differs from its
/// twin: a refusal differs from any real answer, so `assert_ne!` alone would
/// pass vacuously against an unimplemented arm.
fn is_unknown_verb(text: &str) -> bool {
    text.to_lowercase().contains("no verb")
}

/// The nouns of every creature standing in the possession's **own** room, as
/// the walk-band chart marks them.
///
/// This is the public read of "who is co-located", and it is exact rather
/// than approximate: `purview_scene` places a mark from
/// `liveness::agent_position(ledger, npc, at)` and `Session::colocated_npcs`
/// selects on `agent_position(...) == self.agent.position`, so at zoom 0 the
/// mark set on the `me.room` cell **is** the co-located set.
///
/// Only callable out of doors — `Session::purview` `debug_assert!`s that,
/// because the walk-band chart marks every derived NPC ungated. Every caller
/// below therefore reads it BEFORE `enter`, which is also the moment that
/// matters: `enter` runs no tick, so nobody has moved by the time the
/// chamber-band gate applies.
fn colocated_creature_nouns(session: &Session<'_>) -> Vec<String> {
    let here = session
        .snapshot()
        .expect("a live session snapshots")
        .me
        .room;
    session
        .purview(0)
        .expect("the walk-band chart draws")
        .cells
        .iter()
        .filter(|cell| cell.room == here)
        .flat_map(|cell| cell.marks.iter())
        .filter(|mark| mark.kind == "agent")
        .map(|mark| mark.noun.clone())
        .collect()
}

/// The labels of every creature the possessed body currently **senses**.
fn sensed_labels(session: &Session<'_>) -> Vec<String> {
    session
        .snapshot()
        .expect("a live session snapshots")
        .sensed
        .present
        .iter()
        .map(|entry| entry.label.clone())
        .collect()
}

/// Whether the possession is in the chamber band.
fn is_inside(session: &Session<'_>) -> bool {
    matches!(
        session
            .snapshot()
            .expect("a live session snapshots")
            .spatial,
        hornvale_vessel::SpatialChannel::Chamber { .. }
    )
}

/// A world whose opening chamber holds a creature the possession's own sight
/// does not reach, with that creature's noun — the fixture `!needs` and
/// `!examine` both discriminate on.
///
/// The precondition is read entirely through the public surface and entirely
/// **without** the feature under test: the walk-band chart says the creature
/// is here, and the chamber-band snapshot says the body does not sense it.
fn world_withholding_a_colocated_creature() -> (u64, hornvale_kernel::World, String) {
    for seed in SIGHT_SEEDS {
        let Some(world) = world_at(seed) else {
            continue;
        };
        let found = {
            let Ok((mut session, _)) = Session::start(&world, &PossessOpts::default()) else {
                continue;
            };
            // The tick is load-bearing: the within-room `Occupancy` a
            // creature's cell is resolved from is only populated by
            // `DriveMovements::step_with_occupancy`, so before the first
            // `wait` the embedding has nothing to place and sight has
            // nothing to narrow.
            session.handle("wait");
            let colocated = colocated_creature_nouns(&session);
            session.handle("enter");
            if !is_inside(&session) || colocated.is_empty() {
                None
            } else {
                let sensed = sensed_labels(&session);
                colocated.into_iter().find(|noun| !sensed.contains(noun))
            }
        };
        if let Some(noun) = found {
            return (seed, world, noun);
        }
    }
    panic!(
        "no seed in {SIGHT_SEEDS:?} opens into a chamber holding a creature the \
         possession cannot sense — the search found nothing, so nothing below \
         could be tested. This is a finding about the sim, not a flaky fixture: \
         either the sight narrowing regressed or every world in the range \
         stopped exercising it."
    );
}

/// A world where, one `wait` after entering, a creature the possession could
/// **not** sense leaves the chamber — the fixture `!wait` discriminates on.
///
/// Read through the public surface, in three observations that between them
/// pin the whole transition: the walk-band chart before `enter` (the
/// creature is here), the chamber-band snapshot after it (the body does not
/// sense it), and the walk-band chart again after `out` (it has gone). `out`
/// runs no tick — `Session::leave` only takes `self.inside` and redescribes
/// — so the third read observes the state the `wait` produced and nothing
/// later.
fn world_where_an_unsensed_creature_departs() -> (u64, hornvale_kernel::World, String) {
    for seed in SIGHT_SEEDS {
        let Some(world) = world_at(seed) else {
            continue;
        };
        let found = {
            let Ok((mut session, _)) = Session::start(&world, &PossessOpts::default()) else {
                continue;
            };
            session.handle("wait");
            let before = colocated_creature_nouns(&session);
            session.handle("enter");
            if !is_inside(&session) || before.is_empty() {
                None
            } else {
                let sensed = sensed_labels(&session);
                session.handle("wait");
                session.handle("out");
                let after = colocated_creature_nouns(&session);
                before
                    .into_iter()
                    .find(|noun| !sensed.contains(noun) && !after.contains(noun))
            }
        };
        if let Some(noun) = found {
            return (seed, world, noun);
        }
    }
    panic!(
        "no seed in {SIGHT_SEEDS:?} produces a chamber a creature the possession \
         cannot sense then LEAVES — the search found nothing, so `!wait`'s \
         discriminator could not be built. Either the departure narration or the \
         sight narrowing regressed, or every world in the range stopped \
         exercising the pair."
    );
}

/// `!map` — the gate is `eyes`, and its permissive limit is the observer
/// step declined (`Eyes::Off` → `eyes::resolve` → `None`): the objective,
/// uncoloured terrain chart, not the one this body's photoreceptors
/// project.
///
/// The caption is the sharp end of it. `render_surrounds_ascii` interpolates
/// the lens name into `[lens: … ]` precisely so a render through a second
/// lens cannot silently caption itself as the first, which makes it the one
/// assertion that names *which* view was drawn rather than merely that two
/// strings differ.
#[test]
fn the_objective_map_draws_the_uncoloured_chart() {
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let subjective = out(&mut s, "map");
    let (mut s2, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let objective = out(&mut s2, "!map");

    assert!(
        !is_unknown_verb(&objective),
        "`!map` must be a recognised out-of-character verb, got: {objective}"
    );
    assert!(
        subjective.contains("[lens: colour"),
        "precondition: the default possession draws through its own eyes, so the \
         bare chart must be captioned `colour` — without that this pair cannot \
         discriminate. Got: {subjective}"
    );
    assert!(
        objective.contains("[lens: terrain"),
        "`!map` must draw the uncoloured chart — `eyes` at its permissive limit \
         declines the observer step entirely. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!map` must not be an alias for `map`: it draws the objective terrain, \
         not this body's colour projection of it"
    );
}

/// `!needs` — the gate is `sensed_npcs(self.sighting())`, and its permissive
/// limit is `sensed_npcs(None)`: every co-located creature, read by the same
/// predicate with nothing to narrow it.
#[test]
fn the_objective_needs_reads_a_creature_the_body_cannot_sense() {
    let (seed, w, hidden) = world_withholding_a_colocated_creature();

    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("enter");
    let subjective = out(&mut s, "needs");
    let objective = out(&mut s, "!needs");

    assert!(
        !is_unknown_verb(&objective),
        "`!needs` must be a recognised out-of-character verb, got: {objective}"
    );
    assert!(
        !subjective.contains(&hidden),
        "precondition (seed {seed}): bare `needs` must withhold `{hidden}` — that \
         is the gate this pair exists to discriminate. Got: {subjective}"
    );
    assert!(
        objective.contains(&hidden),
        "`!needs` must read the felt state of `{hidden}`, who is standing here but \
         beyond the body's sight. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!needs` must not be an alias for `needs` — it takes the sight gate to \
         its permissive limit"
    );
}

/// `!examine` — the gate is the same `sensed_npcs(self.sighting())`, applied
/// to the chamber-band creature arm of `examine_chamber`. Bare `examine`
/// answers a withheld creature with the band's ordinary absence refusal;
/// the objective form answers with the creature's own datum.
#[test]
fn the_objective_examine_answers_for_a_creature_the_body_cannot_sense() {
    let (seed, w, hidden) = world_withholding_a_colocated_creature();

    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("enter");
    let subjective = out(&mut s, &format!("examine {hidden}"));
    let objective = out(&mut s, &format!("!examine {hidden}"));

    assert!(
        !is_unknown_verb(&objective),
        "`!examine` must be a recognised out-of-character verb, got: {objective}"
    );
    assert_eq!(
        subjective,
        format!("You see no {hidden} here."),
        "precondition (seed {seed}): bare `examine` must refuse a creature sight \
         withheld, in the band's own absence wording"
    );
    assert!(
        objective.contains(&hidden) && objective.contains("alive and moving"),
        "`!examine` must answer with the creature's own datum — the SAME sentence \
         the chart's legend gives, never a second wording. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!examine` must not be an alias for `examine`"
    );
}

/// `!wait` — the gate is the `sensed_before` roster captured at
/// `Session::wait` and the `sensed_now` roster `narrate_motion` recomputes;
/// the permissive limit is both taken over every co-located creature. A
/// creature the possession could not see leaves, and only the objective form
/// says so.
#[test]
fn the_objective_wait_narrates_a_departure_the_body_could_not_see() {
    let (seed, w, hidden) = world_where_an_unsensed_creature_departs();

    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("enter");
    let subjective = out(&mut s, "wait");

    let (mut s2, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s2.handle("wait");
    s2.handle("enter");
    let objective = out(&mut s2, "!wait");

    assert!(
        !is_unknown_verb(&objective),
        "`!wait` must be a recognised out-of-character verb, got: {objective}"
    );
    assert!(
        !subjective.contains(&hidden),
        "precondition (seed {seed}): bare `wait` must drop the departure of \
         `{hidden}`, whom the body never saw. Got: {subjective}"
    );
    assert!(
        objective.contains("You watch") && objective.contains(&hidden),
        "`!wait` must narrate `{hidden}` leaving — the departure the body could \
         not witness. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!wait` must not be an alias for `wait`"
    );
}

/// `!wait` is the one out-of-character act that moves the clock (spec §3.4:
/// out-of-character "charges nothing by default — `!wait` is the exception
/// that moves the clock"). Without it, being asleep would be
/// indistinguishable from the game having hung, which is the whole argument
/// for the verb.
///
/// Asserted against the bare form's own advance rather than a literal, so a
/// future change to what one `wait` costs cannot make this pass by moving
/// both numbers.
#[test]
fn the_objective_wait_moves_the_clock_exactly_as_the_bare_form_does() {
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let start = s.snapshot().unwrap().day;
    s.handle("wait 3");
    let bare = s.snapshot().unwrap().day;

    let (mut s2, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s2.handle("!wait 3");
    let sigilled = s2.snapshot().unwrap().day;

    assert!(
        bare > start,
        "precondition: a bare `wait 3` must advance the clock, or this test \
         compares two stationary numbers"
    );
    assert_eq!(
        bare, sigilled,
        "`!wait` must advance the day exactly as `wait` does (spec §3.4)"
    );
}

/// The two verbs of spec §3.2's group B that this task deliberately did
/// **not** ship, pinned so a later campaign cannot add a silent alias
/// without this going red first.
///
/// `knows` has no gate: it prints every entry of `self.knowledge`, and the
/// player's knowledge is the subject rather than something withheld from
/// them. `look` has no gate either: its three band arms
/// (`describe_here`, `describe_chamber_here`, `describe_underground_here`)
/// read the place, and none of them consults sight, eyes, lens or
/// knowledge. Neither has a permissive limit to take, so an out-of-character
/// half would have to render a **different object** — which is the renderer
/// fork this task forbids.
#[test]
fn neither_look_nor_knows_has_an_objective_half() {
    let w = world();
    for verb in ["look", "knows"] {
        let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let bare = out(&mut s, verb);
        let (mut s2, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let sigilled = out(&mut s2, &format!("!{verb}"));
        assert!(
            is_unknown_verb(&sigilled),
            "`!{verb}` is deliberately unshipped (it has no gate to relax) and must \
             refuse as an unknown verb rather than alias the bare form; got: {sigilled}"
        );
        assert_ne!(
            bare, sigilled,
            "`!{verb}` must not have fallen through to the bare arm's behaviour"
        );
    }
}
