//! Searching for a world that exercises a precondition, instead of pinning one.
//!
//! **Why this module exists.** The Sighting's evidence originally rested on an
//! accident of the seed-42 world: after one tick, a creature happened to be
//! co-located in the chamber the possession enters, and standing somewhere the
//! shadowcast could reach. Five tests and one client fixture were written
//! against that. The Tense then reseeded the flagship — the settlement went
//! `Goodogododaga` → `Googo`, the structure went from two chambers to four —
//! and seed 42 stopped being a world that exercises the feature at all. Sight
//! still worked; the *fixture world* had moved out from under the tests.
//!
//! Re-pointing at some other single seed would reproduce that fragility
//! exactly, one campaign later. So the tests that need "a creature is drawn on
//! the chamber plan" now **search** for such a world, the same way
//! `lattice::anchor_cells`'s property batteries sweep `0u64..64` rather than
//! asserting over one fixture.
//!
//! **The search is loud in both directions.** It returns the FIRST seed whose
//! world satisfies the caller's predicate — 19 of the first 24 seeds do, so in
//! practice it costs one or two world builds — and it PANICS, naming the range
//! and the predicate, when none does. A sweep that quietly found nothing and
//! let its test pass would be strictly worse than the hardcoded seed it
//! replaces: the loud precondition assertions are what caught The Tense's
//! reseed in the first place.

#![allow(dead_code)]

use hornvale_kernel::{EntityId, Facet, Seed, World, WorldTime};
use hornvale_vessel::liveness::{Hazards, Terrain};
use hornvale_vessel::{PlanMark, PossessOpts, Session, SpatialChannel, Turn};

/// The seeds searched. Wide enough that "no world in here draws a creature" is
/// a real finding about the sim rather than about the sample, and cheap in
/// practice because the search stops at its first hit.
pub const SIGHT_SEEDS: std::ops::Range<u64> = 0..64;

/// A world built at `seed`, or `None` if this seed has no world to build.
pub fn build(seed: u64) -> Option<World> {
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .ok()
}

/// The marks a live session's snapshot draws on its chamber plan.
///
/// Panics if the session is not in the chamber band, because every caller has
/// just walked it there and a `Walk` here would mean `enter` silently failed.
pub fn marks_of(session: &Session<'_>) -> Vec<PlanMark> {
    match session
        .snapshot()
        .expect("a live session snapshots")
        .spatial
    {
        SpatialChannel::Chamber { plan } => plan.marks,
        SpatialChannel::Walk { .. } | SpatialChannel::Underground { .. } => {
            panic!("expected the chamber band")
        }
    }
}

/// Whether the possession is in the chamber band — i.e. `enter` found a
/// structure to enter. Read off the wire tag rather than a private field,
/// because that is all an integration test can see.
pub fn is_inside(session: &Session<'_>) -> bool {
    matches!(
        session
            .snapshot()
            .expect("a live session snapshots")
            .spatial,
        SpatialChannel::Chamber { .. }
    )
}

/// One tick in and one `enter` deep — the script every sight test walks.
///
/// The `wait` is load-bearing: the within-room `Occupancy` is populated by
/// `DriveMovements::step_with_occupancy`, which only runs on a tick, so before
/// the first `wait` no creature has a fine-layer anchor and the embedding has
/// nothing to place.
pub fn step_inside(session: &mut Session<'_>) {
    session.handle("wait");
    session.handle("enter");
}

/// Place `who` at the possession, and walk further in until the chamber it
/// stands in actually DRAWS the mark. The caller has already entered.
///
/// **Entering alone stopped being enough at The Pavement, and the cause
/// is the epoch rather than anything about sight.** A structure is drawn from
/// its own room's seed and the cube-sphere mesh moved every room address, so
/// the flagship's ENTRANCE chamber is drawn from a different seed than it was.
/// Measured on seed 42: its five room anchors resolve to three cells and none
/// of them lies inside the shadowcast, while one chamber further in has two
/// lit and one unlit. Every fixture that entered and then asserted a mark was
/// reading a chamber that draws none, and each said so in its own words —
/// *"the placed companion was chosen BECAUSE it draws a mark"*, *"'x' must be
/// drawn on the plan indoors"*.
///
/// Walking until the property holds is the same discipline as
/// [`world_where`] one function down: ask for the property, do not pin the
/// place that happened to have it. It panics rather than returning a chamber
/// that draws nothing, for the reason that module doc gives — a search that
/// quietly found nothing is worse than the hardcoded fixture it replaces.
pub fn deepen_until_the_plan_draws(session: &mut Session<'_>, who: EntityId) {
    assert!(
        is_inside(session),
        "the possession is not indoors, so nothing below is tested"
    );
    // EVERY chamber, in pre-order, not "further in until it stops" (The Cruck,
    // Task 3). A structure is a rooted tree now: `enter further in` refuses at
    // a fork rather than guess a direction, so the old loop asserted its own
    // far-end message in the threshold of any forking structure and never
    // looked at a second chamber. The property is unchanged — a chamber whose
    // plan DRAWS the placed creature — and it still refuses loudly rather than
    // leaving a caller in one that draws nothing.
    let found = find_chamber_where(session, &mut |s| {
        s.place_creature_at_me(who);
        !marks_of(s).is_empty()
    });
    assert!(
        found,
        "no chamber of this structure draws a placed creature on its plan, so \
         nothing below is tested"
    );
}

/// The first seed in [`SIGHT_SEEDS`] whose fresh possession satisfies `pred` —
/// with the world it was built from.
///
/// `pred` receives the session as `Session::start` returns it, having taken no
/// turns, so a caller whose precondition spans the walk band AND the chamber
/// band (the doorway-parity test does) can walk it itself rather than being
/// handed a session already indoors.
///
/// `what` names the property being searched for and appears in the panic
/// message, so a search that comes up empty says what the sim stopped doing
/// rather than merely that a test failed.
pub fn world_where(what: &str, pred: impl Fn(&mut Session<'_>) -> bool) -> (u64, World) {
    for seed in SIGHT_SEEDS {
        let Some(world) = build(seed) else { continue };
        let hit = {
            let Ok((mut session, _)) = Session::start(&world, &PossessOpts::default()) else {
                continue;
            };
            pred(&mut session)
        };
        if hit {
            return (seed, world);
        }
    }
    panic!(
        "no seed in {SIGHT_SEEDS:?} produces a world where {what} — the search \
         found nothing, so nothing below could be tested. This is a finding \
         about the sim, not a flaky fixture: either the feature regressed or \
         every world in the range stopped exercising it."
    );
}

/// A world whose opening chamber, one tick in, draws at least one creature on
/// its plan — the precondition The Sighting's evidence rests on.
pub fn world_that_draws_a_creature() -> (u64, World) {
    world_where("a creature is drawn on the entered chamber's plan", |s| {
        step_inside(s);
        is_inside(s) && !marks_of(s).is_empty()
    })
}

/// A `Terrain` that delegates every read to `inner` and counts the calls —
/// The Detent's instrument. Counts are deterministic and load-independent,
/// which is why the campaign's gated witnesses assert on them rather than
/// on wall time.
pub struct CountingTerrain<'a> {
    inner: &'a dyn Terrain,
    hazards: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    water: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    temperature: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    elevation: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
}

impl<'a> CountingTerrain<'a> {
    /// Wrap `inner`, all counters at zero.
    pub fn new(inner: &'a dyn Terrain) -> Self {
        Self {
            inner,
            hazards: Default::default(),
            water: Default::default(),
            temperature: Default::default(),
            elevation: Default::default(),
        }
    }
    /// How many `hazards()` calls since construction or the last `reset`.
    pub fn hazards_calls(&self) -> u64 {
        self.hazards.get()
    }
    /// How many `is_fresh_water()` calls since construction or the last `reset`.
    pub fn water_calls(&self) -> u64 {
        self.water.get()
    }
    /// How many `temperature()` calls since construction or the last `reset`.
    pub fn temperature_calls(&self) -> u64 {
        self.temperature.get()
    }
    /// How many `elevation()` calls since construction or the last `reset`.
    pub fn elevation_calls(&self) -> u64 {
        self.elevation.get()
    }
    /// Zero every counter.
    pub fn reset(&self) {
        self.hazards.set(0);
        self.water.set(0);
        self.temperature.set(0);
        self.elevation.set(0);
    }
}

impl Terrain for CountingTerrain<'_> {
    fn elevation(&self, room: &Facet) -> f64 {
        self.elevation.set(self.elevation.get() + 1);
        self.inner.elevation(room)
    }
    fn is_fresh_water(&self, room: &Facet) -> bool {
        self.water.set(self.water.get() + 1);
        self.inner.is_fresh_water(room)
    }
    fn temperature(&self, room: &Facet, day: WorldTime) -> f64 {
        self.temperature.set(self.temperature.get() + 1);
        self.inner.temperature(room, day)
    }
    fn solar_altitude(&self, room: &Facet, day: WorldTime) -> Option<f64> {
        self.inner.solar_altitude(room, day)
    }
    fn day_ticks(&self) -> Option<hornvale_kernel::units::TickSpan> {
        self.inner.day_ticks()
    }
    fn forage_value(&self, room: &Facet) -> f64 {
        self.inner.forage_value(room)
    }
    fn hazards(&self, room: &Facet) -> Hazards {
        self.hazards.set(self.hazards.get() + 1);
        self.inner.hazards(room)
    }
    fn is_built(&self, room: &Facet) -> bool {
        self.inner.is_built(room)
    }
    fn is_cold(&self, room: &Facet) -> bool {
        self.inner.is_cold(room)
    }
    fn prey_value(&self, room: &Facet) -> f64 {
        self.inner.prey_value(room)
    }
}

/// Every chamber of the structure the possession is standing in, visited in
/// pre-order, each one's [`Session::chamber_nouns_here`] in visit order.
///
/// **A structure is a TREE now (The Cruck), so "walk in until it stops" is no
/// longer a traversal.** `enter further in` refuses at a fork and names the
/// ways instead of guessing one, so a loop of `enter further in` visits the
/// threshold and then stops — silently, with a plausible-looking one-element
/// result. Every test that wanted "every chamber" or "the deepest chamber"
/// goes through here instead.
///
/// It drives the SESSION, never the structure: the ways come off the rendered
/// `Ways on:` footer, each `the <noun>` way is entered by name, and the walk
/// back up is `enter <parent role noun>` — the aperture-by-name relaxation
/// spec §5.3 gives. So this is a claim about what a player can type, which is
/// the claim the tests calling it actually make.
///
/// Panics if the possession is not indoors, or if a way the footer advertises
/// does not lead to a chamber — both are findings rather than fixture noise.
pub fn visit_every_chamber(session: &mut Session<'_>) -> Vec<Vec<String>> {
    assert!(
        is_inside(session),
        "the possession is not indoors, so there are no chambers to visit"
    );
    let mut out = Vec::new();
    visit_from_here(session, true, &mut out);
    out
}

/// The reply to `line`, refusing to accept a release — every caller here is
/// mid-walk and a release would mean the possession ended under it.
fn say(session: &mut Session<'_>, line: &str) -> String {
    match session.handle(line) {
        Turn::Out(t) => t,
        Turn::Released(t) => panic!("`{line}` released the possession: {t}"),
    }
}

/// The ways a chamber's footer advertises, `out` dropped: either the single
/// `further in`, or one `the <noun>` per child at a fork.
fn ways_on(text: &str) -> Vec<String> {
    let line = text
        .lines()
        .find_map(|l| l.strip_prefix("Ways on: "))
        .unwrap_or_else(|| panic!("a chamber rendering carries a `Ways on:` line: {text:?}"));
    line.trim_end_matches('.')
        .split(", ")
        .filter(|w| *w != "out")
        .map(str::to_string)
        .collect()
}

/// Visit this chamber and everything under it, then walk back to the parent
/// unless this is the chamber `enter` landed in.
fn visit_from_here(session: &mut Session<'_>, is_root: bool, out: &mut Vec<Vec<String>>) {
    out.push(session.chamber_nouns_here());
    let ways = ways_on(&say(session, "look"));
    for way in &ways {
        let reply = say(session, &format!("enter {way}"));
        assert!(
            reply.starts_with("[chamber "),
            "the footer advertised `{way}` and it did not lead to a chamber: {reply}"
        );
        visit_from_here(session, false, out);
    }
    if !is_root {
        step_back(session, &ways);
    }
}

/// Walk one aperture back toward the door, by the parent's role noun.
///
/// The noun is DISCOVERED rather than derived: the footer names a chamber's
/// children but never itself, so this tries every role noun that is not one of
/// this chamber's own `the <noun>` ways. A role noun is unique among a built
/// structure's apertures (spec §5.1 invariant 3), so exactly one of them is the
/// parent — and a refusal costs nothing, since `enter` charges only after it
/// has decided to move.
fn step_back(session: &mut Session<'_>, own_ways: &[String]) {
    let children: Vec<&str> = own_ways
        .iter()
        .filter_map(|w| w.strip_prefix("the "))
        .collect();
    for role in hornvale_vessel::structure::EVERY_ROLE {
        let noun = role.noun();
        if children.contains(&noun) {
            continue;
        }
        if say(session, &format!("enter {noun}")).starts_with("[chamber ") {
            return;
        }
    }
    panic!("no role noun walked back toward the door from this chamber");
}

/// Walk the structure the possession stands in, in pre-order, stopping in the
/// first chamber where `pred` holds — the search half of
/// [`visit_every_chamber`].
///
/// Returns `true` with the possession LEFT STANDING in the satisfying chamber,
/// or `false` with it back where it started. `pred` may act on the session (it
/// is what places a creature before reading the plan); it is called exactly
/// once per chamber.
pub fn find_chamber_where(
    session: &mut Session<'_>,
    pred: &mut dyn FnMut(&mut Session<'_>) -> bool,
) -> bool {
    assert!(
        is_inside(session),
        "the possession is not indoors, so there are no chambers to search"
    );
    search_from_here(session, true, pred)
}

fn search_from_here(
    session: &mut Session<'_>,
    is_root: bool,
    pred: &mut dyn FnMut(&mut Session<'_>) -> bool,
) -> bool {
    if pred(session) {
        return true;
    }
    let ways = ways_on(&say(session, "look"));
    for way in &ways {
        let reply = say(session, &format!("enter {way}"));
        assert!(
            reply.starts_with("[chamber "),
            "the footer advertised `{way}` and it did not lead to a chamber: {reply}"
        );
        if search_from_here(session, false, pred) {
            return true;
        }
    }
    if !is_root {
        step_back(session, &ways);
    }
    false
}

/// Walk the possession to the chamber whose ROLE noun is `noun` — `hearth`,
/// `loomroom`, `store`, … — wherever the tree puts it. Returns whether it was
/// found, leaving the possession standing in it; on a miss the possession is
/// back where it started.
///
/// **A specific room is named now, not counted** (The Cruck, Task 3). Tests
/// that wanted "the loomroom with the key" or "the store with the strongbox"
/// walked a fixed number of `enter further in`s, which reached them only while
/// a structure was a chain. The role a room plays is what those tests were
/// ever about, and a role noun is unique among a built structure's apertures
/// (spec §5.1 invariant 3), so it names the room whatever depth the brief puts
/// it at.
///
/// The search is over the SESSION, not the structure: at each chamber it tries
/// `enter <noun>`, and on a refusal descends into each way the footer
/// advertises and tries again. That also finds the chamber the possession is
/// already standing in — the first child it descends into can name it — so a
/// caller need not know where it starts.
pub fn walk_to_role_noun(session: &mut Session<'_>, noun: &str) -> bool {
    assert!(
        is_inside(session),
        "the possession is not indoors, so there is no chamber to walk to"
    );
    seek_from_here(session, true, noun)
}

fn seek_from_here(session: &mut Session<'_>, is_root: bool, noun: &str) -> bool {
    if say(session, &format!("enter {noun}")).starts_with("[chamber ") {
        return true;
    }
    let ways = ways_on(&say(session, "look"));
    for way in &ways {
        let reply = say(session, &format!("enter {way}"));
        assert!(
            reply.starts_with("[chamber "),
            "the footer advertised `{way}` and it did not lead to a chamber: {reply}"
        );
        if seek_from_here(session, false, noun) {
            return true;
        }
    }
    if !is_root {
        step_back(session, &ways);
    }
    false
}
