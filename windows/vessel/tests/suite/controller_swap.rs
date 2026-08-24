//! The Hand, Task 5: the arc's acceptance test. GOAP becomes
//! `DefaultController`, player input becomes a second `Controller`
//! implementation of the same trait, and arbitration runs for every body
//! — the driven one included, which is the co-present decision (spec
//! §2.3) made mechanical.

use crate::body_fields::seed_42;
use hornvale_vessel::liveness::{AffectLabel, DriveKind, Mode};
use hornvale_vessel::{PossessOpts, PossessTarget, Session};

/// The Bridle's Arc II acceptance test: swap controllers and both paths
/// still produce acts. If either special-cases the other this fails.
#[test]
fn a_creature_on_player_input_and_a_body_on_goap_both_act() {
    let (world, _ctx) = seed_42();

    // A body driven by GOAP: give the player's controller nothing to say and
    // confirm the body still acts on its own drives over several ticks.
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let before = s.committed_fact_count();
    s.handle("!wait 5");
    assert!(
        s.committed_fact_count() > before,
        "the world acts while the player says nothing"
    );

    // A creature on player input: drive body 1 explicitly and confirm ITS
    // entity is the subject of a committed act.
    let second = s.bodies()[1].entity;
    let (mut t, _) = Session::start(
        &world,
        &PossessOpts {
            target: PossessTarget::Creature(second),
            ..Default::default()
        },
    )
    .unwrap();
    let who = t.agent_entity();
    t.handle("go n");
    assert!(
        t.committed_agent_at_count_for(who) > 0,
        "a player-driven creature commits its OWN agent-at"
    );
}

/// Co-present (spec §2.3): the host has feelings while you ride it. This is
/// the mechanical content of that decision, and nothing else observes it
/// yet — so if this test is deleted the decision silently stops holding.
///
/// **D4 (Task 5 fix round 2's review): the assertion used to be
/// `matches!(mode, Pursuing(_) | Homing | Idle)`** — exhaustive over every
/// `Mode` variant, so it could never fail; the only thing actually load-
/// bearing was the `.expect(...)` a call earlier. This now asserts a
/// SPECIFIC, empirically-verified value: seed 42's flagship body, waited 30
/// days from a fresh session, arbitrates to `Pursuing(Fatigue)` every time
/// (confirmed stable across single calls of 1/5/30 days, both before and
/// after D5's `catch_up` fix) — a real computation over this body's actual
/// state, discriminating against every other `Mode` value rather than
/// vacuously accepting all of them.
#[test]
fn a_driven_body_still_arbitrates_its_own_drives() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let mode = s.driven_mode().expect("a driven body has a mode");
    assert_eq!(
        mode,
        Mode::Pursuing(DriveKind::Fatigue),
        "the driven body's own arbitration produced: {mode:?}"
    );
}

/// An independent reviewer replaced the ENTIRE `step_one_with_controller`
/// call in `Session::wait` with `let _ = PlayerController::new();
/// self.driven_mode = Some(Mode::Idle);` and the whole crate stayed green —
/// the driven walk had a real mechanism underneath (proven at the unit level
/// by `liveness::tests::a_default_controller_passes_through_and_a_player_controller_holds`)
/// but nothing observed its OWN OUTPUT changing tick to tick, so a literal
/// could stand in for the entire call undetected. That is round 0's defect
/// in a thinner form: a mechanism nothing observes is one refactor away from
/// deletion while the suite stays green.
///
/// **This test alone does NOT close that finding (N1, Task 5 fix round 3's
/// review).** It pins that the mode changes across two check-ins within ONE
/// seed — real content, but a loophole survives: `self.day.day() < 5.0 {
/// Homing } else { Pursuing(Fatigue) }`, reading nothing but elapsed time and
/// never walking or asking a controller, ALSO changes between `!wait 1` and
/// `!wait 30` and passes this assertion. The decisive pin is the test below,
/// which this one still complements (it catches a mode frozen for the
/// SESSION's whole lifetime, which the cross-seed test below does not by
/// itself rule out).
#[test]
fn a_driven_bodys_mode_tracks_its_own_state_as_time_passes() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 1");
    let early = s.driven_mode().expect("a driven body has a mode");
    s.handle("!wait 30");
    let later = s.driven_mode().expect("a driven body has a mode");
    assert_ne!(
        early, later,
        "the driven body's own arbitration must track its OWN evolving state \
         across ticks, not report a fixed value ({early:?} both times)"
    );
}

/// **N1 (Task 5 fix round 3), the decisive pin.** A literal keyed on elapsed
/// time alone — the reviewer's own worked counter-example to the test above —
/// cannot ALSO vary by WHICH WORLD it is asked about, because `self.day`
/// carries no seed identity. Real arbitration does: seed 42's flagship
/// arbitrates to `Pursuing(Fatigue)` after one day; seed 13's arbitrates to
/// `Pursuing(Thermal)` (different species, different home, a genuinely
/// different drive trajectory — empirically checked, not assumed; five
/// seeds sampled for this fix round showed three distinct early-mode values).
/// This compares the SAME single checkpoint across two different seeds and
/// asserts the reported mode differs — a check no function of `self.day`
/// alone can pass, because it never reads which world it is even in.
///
/// Verified as this round's acceptance criterion: `scripts/mutate.py`
/// applied to the reviewer's exact substitution reddens this test (quoted
/// verbatim in the fix-round report).
#[test]
fn a_driven_bodys_early_mode_depends_on_which_seeds_population_not_merely_elapsed_time() {
    let world_a = world_at_seed(42);
    let (mut a, _) = Session::start(&world_a, &PossessOpts::default()).unwrap();
    a.handle("!wait 1");
    let mode_a = a.driven_mode().expect("a driven body has a mode");

    let world_b = world_at_seed(13);
    let (mut b, _) = Session::start(&world_b, &PossessOpts::default()).unwrap();
    b.handle("!wait 1");
    let mode_b = b.driven_mode().expect("a driven body has a mode");

    assert_ne!(
        mode_a, mode_b,
        "two different seeds' driven bodies produced the SAME mode at the \
         same checkpoint ({mode_a:?}) — a function of elapsed time alone \
         cannot tell worlds apart, so this can only pass by actually reading \
         each body's own real state"
    );
}

/// Builds a world at an arbitrary seed — [`seed_42`]'s own construction,
/// parameterized, for [`a_driven_bodys_early_mode_depends_on_which_seeds_population_not_merely_elapsed_time`]'s
/// cross-seed comparison.
fn world_at_seed(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("world builds")
}

/// **Relabelled honestly (D2, Task 5 fix round 2's review).** This used to
/// claim it carried "the mechanical content of spec §5.2's commits-on-`Do`
/// argument". It does not, and the review's own mutation proves it: forcing
/// the driven walk's controller to answer `Intent::Do(Action::Rest)`
/// unconditionally leaves this test GREEN, because `Session::wait` discards
/// `step_one_with_controller`'s returned facts UNCONDITIONALLY — regardless
/// of what the walk decided, nothing it computes ever reaches
/// `self.ledger`. That discard is the coordinator's own ruling, and it is
/// correct (the player's verbs are what the body DOES; the walk supplies
/// what the host WANTS — committing both would give the driven body two
/// competing position sources) — but it means spec §5.2's argument is not
/// actually testable in THIS design: the invariant this test guards is
/// structural (a discard) and seed-independent, not a live Hold-vs-Do gate.
/// Spec §5.2 is being corrected at Task 8; this comment records the
/// mechanism as it actually is rather than as the spec currently claims.
///
/// What this test actually guards: a regression where some future change
/// makes `Session::wait` commit the driven walk's own facts CONDITIONALLY
/// (e.g. "only when the controller says Hold" becomes "only when it says
/// Do") would very likely show up here, since real GOAP arbitration over 200
/// days does eventually want to act. It is a legitimate guard against
/// reintroducing the double-position-source risk, not a proof of spec §5.2.
#[test]
fn the_driven_walks_own_facts_never_reach_the_ledger_while_the_player_says_nothing() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let who = s.agent_entity();
    let before = s.committed_fact_count_for(who);
    // Long enough that seed 42's flagship body (fix round 1's mutation probe:
    // `Pursuing(Fatigue)` inside 200 days) would actually reach `Rest` under
    // real GOAP — `committed_agent_at_count_for` alone cannot see a `Rest`
    // (it never moves), so this reads every predicate the body could emit.
    s.handle("!wait 200");
    assert_eq!(
        s.committed_fact_count_for(who),
        before,
        "the driven body's own walk must never reach the ledger while the \
         player says nothing — Session::wait discards its facts \
         unconditionally (D2: this is a structural guard, not spec 5.2's \
         Hold-vs-Do argument)"
    );
}

/// Co-present, for felt state as well as mode (The Confidant, Task 2):
/// `driven_affect` is a SPECIFIC, empirically-verified value, not the
/// exhaustive-over-all-six-variants trap `AffectLabel`'s six-variant enum
/// invites (`matches!(a, Content | Eager | Searching | Frustrated | Lost |
/// Helpless)` is vacuous — it can never fail). Seed 42's flagship body,
/// waited 30 days from a fresh session, arbitrates to `AffectLabel::Eager`
/// every time (checked directly: stable across single calls of 1, 5, 30, 100
/// and 200 days, and down to a 0.001-day wait) — a real computation over
/// this body's actual state, discriminating against every other
/// `AffectLabel` value rather than vacuously accepting all of them.
#[test]
fn a_driven_bodys_felt_state_is_a_specific_circumplex_region() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let affect = s.driven_affect().expect("a driven body has a felt state");
    assert_eq!(
        affect,
        AffectLabel::Eager,
        "the driven body's own arbitration produced: {affect:?}"
    );
}

/// **The decisive pin, mirroring N1 above for mode.** A literal keyed on
/// elapsed time alone cannot ALSO vary by WHICH WORLD it is asked about,
/// because `self.day` carries no seed identity. Real arbitration does: seed
/// 42's flagship arbitrates to `AffectLabel::Eager` after one day; seed 13's
/// arbitrates to `AffectLabel::Frustrated` (checked directly — ten seeds
/// sampled for this task showed both `Eager` and `Content` alongside
/// `Frustrated`, so this is not a two-value coincidence). This compares the
/// SAME single checkpoint across two different seeds and asserts the
/// reported label differs — a check no function of elapsed time alone can
/// pass, because it never reads which world it is even in.
///
/// This is the trap-avoiding assertion the task brief names by name: a
/// same-seed "as time passes" test (mirroring
/// [`a_driven_bodys_mode_tracks_its_own_state_as_time_passes`] above) was
/// tried first and does NOT discriminate here — seed 42's felt state stays
/// `Eager` from day 0.001 through day 200 even while its `Mode` cycles
/// through `Pursuing(Fatigue)`/`Pursuing(Thirst)`/others, so asserting
/// `early != later` within one seed would be checking a fact that happens to
/// be false, not a vacuous tautology, but still the wrong axis to vary.
/// Varying the SEED is what actually discriminates.
#[test]
fn a_driven_bodys_early_affect_depends_on_which_seeds_population_not_merely_elapsed_time() {
    let world_a = world_at_seed(42);
    let (mut a, _) = Session::start(&world_a, &PossessOpts::default()).unwrap();
    a.handle("!wait 1");
    let affect_a = a.driven_affect().expect("a driven body has a felt state");

    let world_b = world_at_seed(13);
    let (mut b, _) = Session::start(&world_b, &PossessOpts::default()).unwrap();
    b.handle("!wait 1");
    let affect_b = b.driven_affect().expect("a driven body has a felt state");

    assert_ne!(
        affect_a, affect_b,
        "two different seeds' driven bodies produced the SAME felt state at \
         the same checkpoint ({affect_a:?}) — a function of elapsed time \
         alone cannot tell worlds apart, so this can only pass by actually \
         reading each body's own real state"
    );
}

/// **The cognitive gap, at the Session boundary this time** (The Confidant,
/// Task 5). Seed 7's flagship body, waited 30 days from a fresh session,
/// arbitrates to `AffectLabel::Eager` while its own Fatigue drive stays
/// genuinely ACTIVE and unpursued — checked directly (stable across single
/// calls of 5, 10, 30, 100 and 200 days). This is a real two-drive conflict
/// the world actually produces, not a constructed one:
/// `driven_affect` reports the winner alone (a SPECIFIC value, not the
/// exhaustive-over-all-six-variants trap `AffectLabel`'s enum invites); the
/// loser is retrievable only through `suppressed_drives`, which nothing
/// routes into what the host says.
#[test]
fn the_driven_bodys_suppressed_drive_is_retrievable_but_absent_from_what_it_says() {
    let world = world_at_seed(7);
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert_eq!(
        s.suppressed_drives(),
        &[] as &[DriveKind],
        "before the first !wait there has been no arbitration to discard \
         anything from"
    );
    s.handle("!wait 30");
    let affect = s.driven_affect().expect("a driven body has a felt state");
    assert_eq!(
        affect,
        AffectLabel::Eager,
        "the driven body's own arbitration produced: {affect:?}"
    );
    assert_eq!(
        s.suppressed_drives(),
        &[DriveKind::Fatigue],
        "fatigue is genuinely active alongside the pursued drive this tick \
         and must stay retrievable through suppressed_drives, absent from \
         driven_affect's single label"
    );
}
