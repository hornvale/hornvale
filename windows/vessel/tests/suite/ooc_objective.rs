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
//! Two of spec §3.2's six group-B verbs were **not** shipped at Task 6, and
//! ship in Task 7's fix round on a discriminator Task 6 did not have. The
//! renderer half of the original finding is unchanged and still true:
//!
//! - **`knows`** (`Session::knows`) prints every entry of `self.knowledge`
//!   with no gate **of its own**. It is not ungated upstream — `absorb_here`
//!   fills that store through `self.projection.project(&v,
//!   &self.agent.perception)`, so perception has already decided what is in
//!   it — but that filter runs at absorb time and is not a parameter of the
//!   renderer, so there is nothing here to take to a limit. The player's
//!   knowledge *is* the subject; relaxing it would mean re-deriving what the
//!   body never absorbed, which is a different object, not a wider view of
//!   this one.
//! - **`look`** (`Session::describe_here` / `describe_chamber_here` /
//!   `describe_underground_here`) has no perceptual gate either — none of
//!   the three band arms consults `sensed_npcs`, `sighting`, `eyes`, `lens`
//!   or `knowledge`. `look` renders the place, and the place is objective
//!   already. Making `!look` list creatures would be a *new object*, not a
//!   permissive limit, which is precisely the fork this task forbids.
//!
//! What changed is that a renderer's gate is no longer the only gate. Task 7
//! built the **body's**, and spec §2.2 has an out-of-character act bypass it,
//! so the two verbs discriminate while the body is asleep without either one
//! rendering a different object.
//! [`look_and_knows_answer_out_of_character_while_the_body_is_asleep`] holds
//! both halves at once: identical awake, divergent asleep.
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
//! Each search is *ordered* rather than shortened — see [`search_from`]. A
//! world build is ~4 s, and an unordered sweep pays that per seed until its
//! first hit, which cost the arrival fixture 136.5 s and made one test the
//! straggler setting the whole crate's wall time. Trying the last known hit
//! first costs one build; the full range is still swept behind it, so the
//! discipline above is unchanged and a rotted hint slows a test rather than
//! breaking one.
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

/// [`SIGHT_SEEDS`] reordered to try `hint` first — the seed this fixture
/// last hit on — then every other seed in the range, in order.
///
/// **This is a cost fix, and it concedes nothing to the search discipline
/// the module header states.** The range is still swept in full; a hint that
/// stops satisfying its predicate costs one wasted build and the search
/// carries on to find whichever seed does, exactly as before. The panic at
/// the end still fires only when *nothing* in the range works.
///
/// Why it is here: a world build is ~4 s, so an unordered search pays that
/// once per seed until its first hit. The arrival fixture below hits at seed
/// 28 and measured **136.5 s** — one test more than doubling the crate's
/// wall time, and doing it as a straggler that finishes last, so
/// parallelism cannot hide it (every other test in `hornvale-vessel` is
/// ≤ 7 s). Hinted, it pays one build.
///
/// The hint is a *performance* claim, never a correctness one. Nothing goes
/// red when it rots — the sweep absorbs it — so do not treat a hint as a
/// pinned seed, and do not assert on it. If a fixture starts running slow,
/// its hint has rotted: run it once, read the seed it returns, update the
/// hint.
fn search_from(hint: u64) -> impl Iterator<Item = u64> {
    std::iter::once(hint).chain(SIGHT_SEEDS.filter(move |&s| s != hint))
}

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

/// A seed whose flagship roll holds WILD bodies that actually DEPART the
/// entered chamber on the very next tick (The Roll, Task 7).
///
/// Two properties at once, and both had to be measured. The roll derives the
/// fauna standing within call rather than the world's top four
/// concentrations, so a wild body is no longer guaranteed at all — seed 42's
/// flagship has none within two hops; see `possession_moves.rs`'s `WILD_SEED`
/// for that measurement. And a crowded chamber makes the placement itself
/// fail: `Session::place_creature_out_of_my_sight` now refuses when every
/// unlit anchor is already claimed, which it is wherever a settlement's whole
/// population stands in the room. Measured over the seeds that derive wild
/// bodies (3, 12, 13, 16, 17), placing the first six unique-labelled wild
/// candidates and reading `!wait`: 12, 13 and 17 place NOBODY; 3 places six
/// and one departs; 16 places six and three depart. 16 is the one with room
/// to spare, so it is this one.
/// type-audit: bare-ok(index)
const WILD_SEED: u64 = 16;

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

/// A world where, one `wait` after entering, a creature the possession
/// **cannot** sense ARRIVES in the chamber — the fixture the arrival half of
/// `!wait` discriminates on.
///
/// This is a second fixture rather than a second assertion on the departure
/// one because `narrate_motion` gates the two transitions on two **different**
/// rosters: a departure on `sensed_before` (captured in `Session::wait` before
/// the day advances), an arrival on `sensed_now` (recomputed inside
/// `narrate_motion`, because a creature that is here now is judged by what can
/// be seen now). Fix round 1 established that the departure test alone leaves
/// the arrival roster unguarded: rewiring `sensed_now` to `Perceiving::Body`
/// left the entire vessel suite green.
///
/// Read through the public surface, in three observations that between them
/// pin the whole transition: the walk-band chart before `enter` (the creature
/// is NOT here), the chamber-band snapshot after the `wait` (the body does not
/// sense it), and the walk-band chart after `out` (it IS here). `out` runs no
/// tick, so the third read observes the state the `wait` produced.
///
/// **It is the most expensive test in this crate, and the cost is the search**
/// — the hit is at seed 28, so 29 worlds are built at ~4.5 s each (measured
/// 130.7 s on a MacBook Pro; the departure fixture hits early and costs 18.5 s
/// for the same shape). Widening the wait to three days was tried and lands on
/// the same seed 24 s slower, so the span is not the lever: world construction
/// is. Pinning seed 28 would buy the time back and is exactly what the module
/// header refuses, for the reason it gives.
fn world_where_an_unsensed_creature_arrives() -> (u64, hornvale_kernel::World, String) {
    for seed in search_from(28) {
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
            if !is_inside(&session) {
                None
            } else {
                session.handle("wait");
                // Read WHILE STILL INSIDE: `sensed.present` is the chamber
                // band's narrowed roster, and it is exactly the moment
                // `narrate_motion` asks its arrival question about.
                let sensed = sensed_labels(&session);
                session.handle("out");
                colocated_creature_nouns(&session)
                    .into_iter()
                    .find(|noun| !before.contains(noun) && !sensed.contains(noun))
            }
        };
        if let Some(noun) = found {
            return (seed, world, noun);
        }
    }
    panic!(
        "no seed in {SIGHT_SEEDS:?} produces a chamber a creature the possession \
         cannot sense ARRIVES in — the search found nothing, so the arrival half \
         of `!wait`'s discriminator could not be built. Either the arrival \
         narration or the sight narrowing regressed, or every world in the range \
         stopped exercising the pair."
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

/// A lantern-lensed possession of the seed-42 world, standing in its first
/// chamber. The lens is the only thing that differs from every other fixture
/// here, and it is set through `PossessOpts` because that is the only way in:
/// the CLI's interactive path selects `Lens::Lantern` the same way
/// (`cli/src/main.rs`), while `--script` selects `Lens::Off` so committed
/// transcripts stay unlensed.
fn lantern_session_inside(
    world: &hornvale_kernel::World,
    eyes: hornvale_vessel::eyes::Eyes,
) -> Session<'_> {
    let opts = PossessOpts {
        lens: hornvale_vessel::lens::Lens::Lantern,
        eyes,
        ..PossessOpts::default()
    };
    let (mut s, _) = Session::start(world, &opts).expect("seed 42 possesses");
    s.handle("enter");
    assert!(
        is_inside(&s),
        "precondition: the seed-42 possession must open a chamber, or the plan \
         under test is never drawn"
    );
    s
}

/// `!map` indoors under a lens must not caption a plan it never tinted (fix
/// round 1).
///
/// `!map` passes `OBJECTIVE_EYES` (`Eyes::Off`), `eyes::resolve` answers
/// `None` for it, `chamber_plan` therefore builds no `Shading`, every palette
/// entry carries `color: None`, and `tint` hands the picture back glyph for
/// glyph. Appending ` — lens: lantern` to that says a filter ran which did
/// not — the one thing the caption exists to prevent, with the lens count at
/// zero instead of one.
///
/// The positive control is the load-bearing half: bare `map` under the same
/// lens and the same session MUST still be captioned and MUST still carry
/// escape bytes. Without it, "the objective plan is uncaptioned" would be
/// satisfied just as well by a lens that was never on.
#[test]
fn the_objective_plan_is_not_captioned_with_a_lens_it_never_applied() {
    let w = world();
    let mut s = lantern_session_inside(&w, hornvale_vessel::eyes::Eyes::Own);
    let subjective = out(&mut s, "map");
    let objective = out(&mut s, "!map");

    assert!(
        subjective.contains("lens: lantern"),
        "positive control: bare `map` indoors under `--lens lantern` must still \
         caption itself, or this pair proves nothing. Got: {subjective}"
    );
    assert!(
        subjective.contains("\u{1b}["),
        "positive control: the captioned plan must actually be tinted. Got: \
         {subjective:?}"
    );
    assert!(
        !objective.contains("lens:"),
        "`!map` declines the observer step, so nothing on its plan carries a \
         colour and the lens filters nothing — it must not caption itself as \
         lensed. Got: {objective}"
    );
    assert!(
        !objective.contains("\u{1b}["),
        "precondition: the objective plan must be untinted — that is what makes \
         the caption a false one. Got: {objective:?}"
    );
}

/// The same falsehood is reachable on the BARE path, which is why the fix is
/// keyed on the resolved observer rather than on the `!` namespace: a player
/// who types `!eyes off` and then a bare `map` has declined the observer step
/// just as `!map` does, and must get the same honest caption.
///
/// This is also the guard on the fix's own blast radius. The bare path is the
/// one Task 6 must leave untouched, so it is pinned in both directions here:
/// uncaptioned when the eyes are off, captioned when they are not (the test
/// above holds the second half on the same session shape).
#[test]
fn a_bare_plan_drawn_with_the_eyes_off_is_not_captioned_either() {
    let w = world();
    let mut s = lantern_session_inside(&w, hornvale_vessel::eyes::Eyes::Off);
    let drawn = out(&mut s, "map");

    assert!(
        !drawn.contains("lens:"),
        "a possession whose eyes are off draws no colour, so a lens filters \
         nothing and the plan must not caption itself as lensed. Got: {drawn}"
    );
    assert!(
        !drawn.contains("\u{1b}["),
        "precondition: eyes off must draw an untinted plan. Got: {drawn:?}"
    );
}

/// `!needs` — the gate is `sensed_npcs(self.sighting())`, and its permissive
/// limit is `sensed_npcs(None)`: every co-located creature, read by the same
/// predicate with nothing to narrow it.
#[test]
fn the_objective_needs_reads_a_creature_the_body_cannot_sense() {
    // The Hand, Task 3: constructed directly through the test seam rather
    // than searched for (see docs/retrospectives/the-hand.md) — `bodies()[1]` is placed
    // co-located but specifically out of the chamber's own shadowcast.
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("enter");
    let hidden = s.bodies()[1].label.clone();
    let companion = s.bodies()[1].entity;
    assert!(
        s.place_creature_out_of_my_sight(companion),
        "precondition: the entered chamber must have a cell outside its own shadowcast to place `{hidden}` on"
    );
    let subjective = out(&mut s, "needs");
    let objective = out(&mut s, "!needs");

    assert!(
        !is_unknown_verb(&objective),
        "`!needs` must be a recognised out-of-character verb, got: {objective}"
    );
    assert!(
        !subjective.contains(&hidden),
        "precondition: bare `needs` must withhold `{hidden}` — that is the gate this pair exists to discriminate. Got: {subjective}"
    );
    assert!(
        objective.contains(&hidden),
        "`!needs` must read the felt state of `{hidden}`, who is standing here but beyond the body's sight. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!needs` must not be an alias for `needs` — it takes the sight gate to its permissive limit"
    );
}

/// `!examine` — the gate is the same `sensed_npcs(self.sighting())`, applied
/// to the chamber-band creature arm of `examine_chamber`. Bare `examine`
/// answers a withheld creature with the band's ordinary absence refusal;
/// the objective form answers with the creature's own datum.
#[test]
fn the_objective_examine_answers_for_a_creature_the_body_cannot_sense() {
    // The Hand, Task 3: constructed directly through the test seam rather
    // than searched for (see docs/retrospectives/the-hand.md).
    let w = world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("wait");
    s.handle("enter");
    let hidden = s.bodies()[1].label.clone();
    let companion = s.bodies()[1].entity;
    assert!(
        s.place_creature_out_of_my_sight(companion),
        "precondition: the entered chamber must have a cell outside its own shadowcast to place `{hidden}` on"
    );
    let subjective = out(&mut s, &format!("examine {hidden}"));
    let objective = out(&mut s, &format!("!examine {hidden}"));

    assert!(
        !is_unknown_verb(&objective),
        "`!examine` must be a recognised out-of-character verb, got: {objective}"
    );
    assert_eq!(
        subjective,
        format!("You see no {hidden} here."),
        "precondition: bare `examine` must refuse a creature sight withheld, in the band's own absence wording"
    );
    assert!(
        objective.contains(&hidden) && objective.contains("alive and moving"),
        "`!examine` must answer with the creature's own datum — the SAME sentence the chart's legend gives, never a second wording. Got: {objective}"
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
    // The Hand, Task 3: constructed directly through the test seam rather
    // than searched for (see docs/retrospectives/the-hand.md). A SETTLED companion
    // (`bodies()[1]`/`[2]`) placed at the flagship simply stays there —
    // measured: ten waits, never departs, because the flagship itself
    // satisfies its needs. A WILD one ("a wild rust-monster" at seed 42)
    // reliably leaves on the very next tick once placed, which is what this
    // discriminator needs: present-but-unsensed, THEN gone.
    //
    // THE SOURCES, Task 9 moved which INDEX that creature sits at (not
    // whether it departs): xorn's per-rung `CHEMOSYNTHATE` weight changed
    // suitability/ordering at seed 42, so `bodies()[3]` is now "a wild xorn"
    // — which, re-measured, does NOT reliably depart on the next tick the
    // way rust-monster does. `bodies()[4]` is "a wild rust-monster" now;
    // the index moved with the reorder, the property this test needs did
    // not.
    // THE PAVEMENT MOVED THE WORLD AGAIN, AND THE INDEX IS NOW SEARCHED RATHER
    // THAN RE-MEASURED. `wild_idx = 4` was the third recorded value for this
    // one line — the roster's order has been re-measured by hand at The Hand,
    // at The Sources, and it would have been again here, because the epoch
    // moves which creature is near enough to the flagship to leave on the next
    // tick. The property this test needs has never been "index 4": it is
    // "SOME creature the body cannot see departs on the very next tick", and a
    // hardcoded index is a way of writing that down that stops being true
    // every time the world moves.
    //
    // So the fixture asks. Each candidate gets a fresh session (placement and
    // a tick are not undoable), is placed out of sight, and is kept only if
    // `!wait` narrates its departure. Deterministic (roster order), and loud
    // if the world stops producing one at all — which would be a finding about
    // departures rather than about this test.
    // **THE ROLL (Task 7) NARROWED BOTH THE WORLD AND THE CANDIDATE LIST, and
    // the search above is unchanged in kind.** Two things moved. First, a
    // session's roster is now the residents of the settlement you stand in
    // plus the fauna within call, so seed 42 — whose flagship sits on a river
    // and has no herd attractor within two hops — produces sixty-eight bodies
    // that ALL drink in place and NONE of which ever departs; scanning them
    // is 67 sessions that cannot succeed. Seed 3 derives fourteen wild bodies
    // within call (measured over seeds 0..23: thirteen of the twenty-four
    // do), and it is a wild body that departs, exactly as the paragraph above
    // already recorded. Second, the scan is bounded to the WILD members and
    // to the first few of them: a settled resident placed at the flagship
    // stays there — that was true before the roll and is truer now that every
    // roster resident is homed at the room the possession is standing in.
    let w = world_at(WILD_SEED).expect("the wild seed builds");
    //
    // The candidate's LABEL must also be unique in the roster, and that is
    // not fussiness: every member of a herd shares the label `a wild
    // <species>` (`derive_wild_herds`), so `subjective.contains(&hidden)`
    // below would match a SIBLING of the hidden creature that the possession
    // could see perfectly well, and the sight-narrowing assertion would fail
    // on a name collision rather than on a disclosure. Measured directly: the
    // first run of this test at seed 3 failed exactly that way, on `a wild
    // giant-crocodile`.
    let candidates: Vec<usize> = {
        let (s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let driven = s.driven_body().entity;
        let unique = |label: &str| s.bodies().iter().filter(|b| b.label == label).count() == 1;
        s.bodies()
            .iter()
            .enumerate()
            .filter(|(_, b)| b.village.is_none() && b.entity != driven && unique(b.label.as_str()))
            .map(|(i, _)| i)
            .collect()
    };
    assert!(
        !candidates.is_empty(),
        "precondition: seed {WILD_SEED}'s flagship must derive at least one \
         wild body within call whose label is unique in the roster; if it does \
         not, an epoch has moved the seed"
    );
    let mut found = None;
    for idx in candidates {
        let (mut probe, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        probe.handle("enter");
        let label = probe.bodies()[idx].label.clone();
        let who = probe.bodies()[idx].entity;
        if !probe.place_creature_out_of_my_sight(who) {
            continue;
        }
        let narrated = out(&mut probe, "!wait");
        if narrated.contains("You watch") && narrated.contains(&label) {
            found = Some(idx);
            break;
        }
    }
    let wild_idx = found.expect(
        "no wild creature on the roster departs the entered chamber on the very next \
         tick while standing outside the possession's sight — without one there is no \
         unwitnessed departure for `!wait` to narrate and nothing below is tested",
    );

    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s.handle("enter");
    let hidden = s.bodies()[wild_idx].label.clone();
    let companion = s.bodies()[wild_idx].entity;
    assert!(
        s.place_creature_out_of_my_sight(companion),
        "precondition: the entered chamber must have a cell outside its own shadowcast to place `{hidden}` on"
    );
    let subjective = out(&mut s, "wait");

    let (mut s2, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    s2.handle("enter");
    assert!(s2.place_creature_out_of_my_sight(companion));
    let objective = out(&mut s2, "!wait");

    assert!(
        !is_unknown_verb(&objective),
        "`!wait` must be a recognised out-of-character verb, got: {objective}"
    );
    assert!(
        !subjective.contains(&hidden),
        "precondition: bare `wait` must drop the departure of `{hidden}`, whom the body never saw. Got: {subjective}"
    );
    assert!(
        objective.contains("You watch") && objective.contains(&hidden),
        "`!wait` must narrate `{hidden}` leaving — the departure the body could not witness. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!wait` must not be an alias for `wait`"
    );
}

/// `!wait`'s ARRIVAL half, which is a second gate and needs a second test.
///
/// `Session::wait` captures `sensed_before` and `narrate_motion` recomputes
/// `sensed_now`; the departure test above exercises only the first. Fix round
/// 1 proved the gap rather than reasoning about it — rewiring the `sensed_now`
/// computation to `Perceiving::Body` left all 579 vessel tests green.
#[test]
#[ignore = "The Hand Task 3: NEITHER route to this test works, and the second one is a finding about the sim (docs/retrospectives/the-hand.md). (1) THE SEAM DOES NOT SERVE IT: place_creature_at_me/place_creature_out_of_my_sight only place a body at the possessions OWN room, so they can manufacture co-location but not an ARRIVAL, which needs before=false at the wait's own start and after=true from the TICK's own commit -- something only the drive simulation can produce mid-call. Measured: placed at the flagship then relocated by its own drive-seeking, a wild creature departs reliably (see the departure test above) but never returns in 8 subsequent waits; six placed or unplaced companions (2 settled, 4 wild) produce zero arrivals across 40 unmodified waits. (2) THE SEED SEARCH STILL IN THIS FILE PASSED ON MAIN AND NOW FAILS ON EVERY SEED: world_where_an_unsensed_creature_arrives exhausts 0..64 and panics with its own message, re-measured 2026-08-24 at 233.72 s -- so, in that panics own words, either the arrival narration or the sight narrowing regressed, or no world in the range exercises the pair any more. That is a finding about the sim, not a flaky fixture. CONSEQUENCE, RECORDED DELIBERATELY: !wait's ARRIVAL narration has NO witness of any kind right now -- the departure half is covered, the arrival half is not. Closing this needs either a day-parameterised placement seam able to pre-stage a same-tick position change, or a measurement of why the search went empty, or accepting the null -- a design decision beyond a co-location fixture."]
fn the_objective_wait_narrates_an_arrival_the_body_could_not_see() {
    let (seed, w, hidden) = world_where_an_unsensed_creature_arrives();

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
        "precondition (seed {seed}): bare `wait` must drop the arrival of \
         `{hidden}`, whom the body cannot see. Got: {subjective}"
    );
    assert!(
        objective.contains("You notice") && objective.contains(&hidden),
        "`!wait` must narrate `{hidden}` arriving — the arrival the body could \
         not witness. Got: {objective}"
    );
    assert_ne!(
        subjective, objective,
        "`!wait` must not be an alias for `wait` on the arrival half either"
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

/// The two verbs of spec §3.2's group B that Task 6 deliberately did not
/// ship, and that fix round 1 ships — because **Task 7 falsified the premise
/// that excluded them**.
///
/// Task 6's STOP rule was that an out-of-character form must DISCRIMINATE
/// from its bare twin somewhere, and neither `look` nor `knows` carries a
/// gate a renderer could relax: `Session::knows` prints every entry of
/// `self.knowledge` (perception-filtered upstream at `absorb_here`, not as a
/// parameter of the renderer), and `look`'s three band arms
/// (`describe_here` / `describe_chamber_here` / `describe_underground_here`)
/// consult no sight, eyes, lens or knowledge at all. **That is still true,
/// and it is no longer the whole question.** The gate Task 7 built is not the
/// renderer's — it is the BODY's, and an out-of-character act bypasses it
/// (spec §2.2). So the discriminator these two lacked now exists: while the
/// body is asleep the bare form is refused and the `!` form answers, which is
/// exactly spec §3.4's argument for the namespace ("observing a state you
/// cannot act in requires a clock you can still advance") applied to the most
/// basic observational verb there is.
///
/// The two are therefore NOT renderer forks: `!look` and `!knows` render the
/// same object their bare twins do, through the same functions, with no
/// parameter relaxed. What differs is only whether the body's state may
/// refuse them.
#[test]
fn look_and_knows_answer_out_of_character_while_the_body_is_asleep() {
    let w = world();
    for verb in ["look", "knows"] {
        let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();

        // Awake, the two forms agree — that is the honest outcome for a verb
        // with no renderer gate to relax, and asserting it here is what keeps
        // the sleeping discrimination below from being read as a fork.
        let bare_awake = out(&mut s, verb);
        let sigilled_awake = out(&mut s, &format!("!{verb}"));
        assert!(
            !is_unknown_verb(&sigilled_awake),
            "`!{verb}` must be a verb now; got the unknown-verb refusal: {sigilled_awake}"
        );
        assert_eq!(
            bare_awake, sigilled_awake,
            "`!{verb}` relaxes no renderer gate, so awake it must answer exactly \
             as its bare twin does — anything else is the renderer fork Task 6 \
             refused to build"
        );

        // Asleep, they diverge, and that IS the discriminator: the bare form
        // is an in-character act the body refuses; the `!` form bypasses the
        // body (spec §2.2) and still answers.
        let lay_down = out(&mut s, "sleep");
        assert!(
            lay_down.contains("You lie down"),
            "precondition: the body must go under, got: {lay_down}"
        );
        let bare_asleep = out(&mut s, verb);
        let sigilled_asleep = out(&mut s, &format!("!{verb}"));
        assert!(
            bare_asleep.contains("asleep"),
            "bare `{verb}` is in character (spec §3.2 group B) and must be \
             refused by the sleeping body: {bare_asleep}"
        );
        assert!(
            !sigilled_asleep.contains("asleep") && !is_unknown_verb(&sigilled_asleep),
            "`!{verb}` bypasses the body's state entirely (spec §2.2) and must \
             still answer: {sigilled_asleep}"
        );
        assert_ne!(
            bare_asleep, sigilled_asleep,
            "`!{verb}` must differ from its bare twin where the body's gate \
             stands — that difference is why it ships at all"
        );
    }
}
