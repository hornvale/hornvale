//! The Reticence, Task 7: the four preregistered readouts (spec section 5,
//! decision 0016). Frozen BEFORE this file was written — H1's exact counts
//! are copied from the spec; H2-H4 are told only the PROPERTY the instrument
//! must demonstrate, and this file is where that instrument was chosen.
//!
//! **A falsified prediction is a finding, not a failure** (root `CLAUDE.md`).
//! If H1's counts move, that means the WORLD moved and the number is
//! reported, not adjusted to match. If H4's count is zero, that IS the
//! headline: it means the doctrine prior is decorative once the fold is
//! running, the same shape as The Cupel's finding and The Cant's own
//! measured null (0/210 admiration).
//!
//! `stance::patience()`'s three thresholds (Guarded 2 / Wary 4 / Open 8) are
//! never retuned here, whatever a test below reports.

use hornvale_vessel::doctrine::{ImprovisedName, Openness, improvised_name, openness};
use hornvale_vessel::liveness::DriveKind;
use hornvale_vessel::stance::{Stance, stance_for};
use hornvale_vessel::{PossessOpts, Session};

/// A generated world at `seed` — the shared construction site every
/// worldgen-adjacent lab test uses (`windows/lab/tests/suite/the_doctrine.rs`'s
/// own `generated`), the shipped four-people component set, generated sky,
/// default terrain/settlement pins.
fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// `world`'s doctrine prior for `species` — the same `improvised_name` /
/// `openness` pair `windows/lab/src/reticence.rs`'s own report computes,
/// read here instead of re-derived a second, divergent way.
fn prior_of(
    world: &hornvale_kernel::World,
    species: &str,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> Openness {
    let lexicon = hornvale_worldgen::lexicon_from(world, species, terrain, climate)
        .expect("a placed species' lexicon builds");
    openness(&improvised_name(world, &lexicon, species))
}

/// Every reachable [`Openness`] value — the roster H4 and its positive
/// control both compare `real_prior` against.
const ALL_PRIORS: [Openness; 3] = [Openness::Guarded, Openness::Wary, Openness::Open];

/// Would ANY prior other than `real_prior` have selected a different
/// [`Stance`] than `real_prior` does, at the SAME `overrides` count? This is
/// the exact comparison H4's sweep uses at every observed ask-point, pulled
/// out so the positive control below runs the identical detector rather
/// than a hand-written stand-in that could silently drift from it (the
/// review finding this responds to: a detector never proven capable of
/// returning `true` cannot be trusted when it returns `false` 70 times).
fn any_other_prior_diverges(real_prior: Openness, overrides: u32) -> bool {
    let real_stance = stance_for(real_prior, overrides);
    ALL_PRIORS
        .iter()
        .filter(|&&alt| alt != real_prior)
        .any(|&alt| stance_for(alt, overrides) != real_stance)
}

/// **Positive control for H4's divergence detector.** A null with no
/// evidence the detector CAN return non-zero is an unexamined zero, not a
/// finding (review finding on this task: H4's own sweep only ever sampled
/// `overrides == 0`, where `stance_for`'s first match arm returns
/// `Stance::Forthcoming` unconditionally on `prior` — `stance.rs:53` — so
/// agreement there is guaranteed by the function's own shape, before any
/// tick is simulated). At `overrides == 3`: `stance_for(Guarded, 3) ==
/// Dissembling` (`step = patience(Guarded) = 2`; `3 > step`, `3 <= step*2`),
/// while `stance_for(Wary, 3) == Costly` (`step = patience(Wary) = 4`;
/// `3 <= step`) and `stance_for(Open, 3) == Costly` likewise — so `Guarded`
/// is a real outlier at this count, through the SAME `any_other_prior_diverges`
/// call H4's sweep makes. If this fails, the detector itself is broken and
/// H4's 0/70 says nothing.
#[test]
fn h4_positive_control_the_divergence_detector_can_fire() {
    assert_eq!(stance_for(Openness::Guarded, 3), Stance::Dissembling);
    assert_eq!(stance_for(Openness::Wary, 3), Stance::Costly);
    assert_eq!(stance_for(Openness::Open, 3), Stance::Costly);
    assert!(
        any_other_prior_diverges(Openness::Guarded, 3),
        "the detector must register a divergence at overrides=3, prior=Guarded — \
         Wary and Open both select Costly where Guarded selects Dissembling"
    );
    // The degenerate case H4's real sweep landed in every time, confirmed
    // to agree for the structural reason named above (stance_for's 0 arm
    // ignores `prior`), not because the detector is blind to divergence in
    // general.
    assert!(
        !any_other_prior_diverges(Openness::Guarded, 0),
        "at overrides=0 every prior agrees BY CONSTRUCTION (stance_for's first \
         match arm), so a 0 result here is expected and uninformative on its own"
    );
}

/// **H1 — the improvising arms are distributed as preregistered** (spec
/// section 5). Frozen before the code: 15 peoples, organized 9, folk 6,
/// doctrine 0. Enumerated with `hornvale_species::society_registry()` — the
/// same fifteen-people roster Task 6's `render_reticence_report`
/// (`windows/lab/src/reticence.rs`) already walks, so this test's
/// denominator is provably the committed report's own. A moved number here
/// means the WORLD moved (a genesis or lexicon change), not that the
/// prediction was loose — chase it, do not adjust the assertion to match.
// Named construction site (decision 0092): sculpts/fits once, for this
// test's own denominator — never a second, independent draw the sim depends
// on.
#[allow(clippy::disallowed_methods)]
#[test]
fn h1_the_improvising_arms_are_distributed_as_preregistered() {
    let world = generated(42);
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");

    let mut god = 0usize;
    let mut spirit = 0usize;
    let mut wordless = 0usize;
    for kind in hornvale_species::society_registry().ids() {
        let people = kind.0;
        let Ok(lexicon) = hornvale_worldgen::lexicon_from(&world, people, &terrain, &climate)
        else {
            continue;
        };
        match improvised_name(&world, &lexicon, people) {
            ImprovisedName::God => god += 1,
            ImprovisedName::Spirit => spirit += 1,
            ImprovisedName::Wordless { .. } => wordless += 1,
        }
    }
    assert_eq!((god, spirit), (9, 6), "frozen arm counts over 15 peoples");
    assert_eq!(
        god + spirit + wordless,
        15,
        "every people resolves to exactly one arm"
    );
}

/// **H2 — the fold discriminates per-drive** (spec section 5). After the
/// rider overrides drive `d` some number of times, the host's stance on `d`
/// moves off [`Stance::Forthcoming`] and its stance on another drive that
/// was never overridden stays [`Stance::Forthcoming`] — the same host, the
/// same doctrine prior, two different readings, because
/// `Session::driven_overrides` is a per-[`DriveKind`] `BTreeMap`
/// (`windows/vessel/src/session.rs`), never one scalar count for the whole
/// host.
///
/// Instrument chosen by reading `Session`'s own accessors rather than going
/// through `ask()`/`Session::handle`: `overrides_of(drive)` and
/// `override_record()` are both already `pub`, and reading them directly
/// avoids the confound H4 below found in the `ask()` pipeline (its topic is
/// structurally almost never the overridden drive) — H2 is a claim about the
/// FOLD's own bookkeeping, not about what one `ask()` call happens to
/// surface.
///
/// Mutation-proved (below, in the report, not in this file — see this
/// task's report for the paired red/green run): substituting a constant for
/// `overrides_of`'s accumulated-count read collapses both readings to the
/// same value and reddens the two assertions below.
// Named construction site (decision 0092).
#[allow(clippy::disallowed_methods)]
#[test]
fn h2_the_fold_discriminates_per_drive() {
    let world = generated(42);
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..8 {
        s.handle("!wait 30");
    }
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
    let species = s.driven_body().species.clone();
    let prior = prior_of(&world, &species, &terrain, &climate);

    let record = s.override_record().clone();
    let overridden = *record
        .iter()
        .max_by_key(|&(_, &n)| n)
        .map(|(d, _)| d)
        .expect("precondition: 8 waits at seed 42 override something (see overrides.rs)");
    let all_drives = [
        DriveKind::Thirst,
        DriveKind::Thermal,
        DriveKind::Fatigue,
        DriveKind::Hunger,
        DriveKind::Danger,
        DriveKind::Social,
    ];
    let untouched = all_drives
        .into_iter()
        .find(|d| s.overrides_of(*d) == 0)
        .expect("precondition: some drive is never overridden across 8 waits at seed 42");

    let moved_stance = stance_for(prior, s.overrides_of(overridden));
    let untouched_stance = stance_for(prior, s.overrides_of(untouched));

    assert_ne!(
        moved_stance,
        Stance::Forthcoming,
        "the overridden drive {overridden:?} (overrides={}) must move off Forthcoming",
        s.overrides_of(overridden)
    );
    assert_eq!(
        untouched_stance,
        Stance::Forthcoming,
        "a co-active drive {untouched:?} never overridden must stay Forthcoming even though \
         {overridden:?} on the SAME host, SAME prior, moved"
    );
}

/// **H3 — refusal is selective, not global** (spec section 5). A count of
/// drives answered vs refused in one session, both sides `> 0`. Instrument:
/// the same per-drive `stance_for` reading H2 uses, but over the FULL
/// six-`DriveKind` roster rather than a single overridden/untouched pair —
/// H2 proves the mechanism discriminates; this counts how it lands across
/// every drive the host carries, which is the shape the spec's own example
/// asks for ("still answers on fatigue in the same session").
// Named construction site (decision 0092).
#[allow(clippy::disallowed_methods)]
#[test]
fn h3_refusal_is_selective_not_global() {
    let world = generated(42);
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    for _ in 0..8 {
        s.handle("!wait 30");
    }
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
    let species = s.driven_body().species.clone();
    let prior = prior_of(&world, &species, &terrain, &climate);

    let all_drives = [
        DriveKind::Thirst,
        DriveKind::Thermal,
        DriveKind::Fatigue,
        DriveKind::Hunger,
        DriveKind::Danger,
        DriveKind::Social,
    ];
    let denominator = all_drives.len();
    let mut refused = 0usize;
    let mut answered = 0usize;
    for drive in all_drives {
        match stance_for(prior, s.overrides_of(drive)) {
            Stance::Withholding => refused += 1,
            _ => answered += 1,
        }
    }
    assert_eq!(
        refused + answered,
        denominator,
        "every one of the {denominator} drives must land in exactly one bucket"
    );
    assert!(
        refused > 0,
        "H3 needs at least one refused drive over {denominator} to be a real test of \
         selectivity, got refused={refused} answered={answered} record={:?}",
        s.override_record()
    );
    assert!(
        answered > 0,
        "H3 needs at least one answered drive over {denominator} to be a real test of \
         selectivity, got refused={refused} answered={answered} record={:?}",
        s.override_record()
    );
}

/// **H4 — the null this campaign is prepared to report** (spec section 5),
/// with a structural claim and an empirical one kept SEPARATE (review
/// correction: an earlier version of this test reported "0/70" as if it
/// were a broad sweep with 70 independent chances to falsify the null; it
/// is not, and the doc comment below is corrected to say so).
///
/// **THE MECHANISM CLAIM WAS WRONG, AND IS RESTATED HERE (final-fix wave).**
/// This doc used to say `ask()`'s topic and the override record are
/// **disjoint by construction**, so the prior *cannot* move observable
/// testimony. The per-tick half of that is true and is not in dispute:
/// `liveness.rs`'s `suppressed` is computed by explicitly filtering OUT
/// `pursued_kind`, so a drive is never recorded as overridden on the very
/// tick it wins. **The conclusion does not follow.** `Session`'s
/// `driven_overrides` is a `BTreeMap` that is created once at `Session::start`
/// and thereafter only ever `+= 1` — it is **never reset** — and
/// `overrides_of(topic)` reads that accumulated history, not the current
/// tick. A drive that piles up overrides while LOSING would carry every one
/// of them into `overrides_of(topic)` the moment it later WON. The two sets
/// are disjoint *per tick*; the quantity `ask()` actually reads is not a
/// per-tick quantity.
///
/// **So the null is empirical, not structural — and it is empirically
/// STRONGER than "disjoint by construction" would have made it, because a
/// contingent fact that keeps holding is a finding, where an impossibility
/// is just a restatement.** What actually keeps `overrides_of(topic)` at
/// zero is that **arbitration is sticky**: measured directly, a drive holds
/// the topic only during an opening prefix in which it has not yet lost
/// anything, and once a drive begins losing it is never observed to win the
/// topic back. Seed 42 is the clean picture — `Fatigue` is the topic at the
/// first sample while the record is still literally empty, `Thirst` takes
/// over one sample later, and `Thirst` then holds the topic for every
/// remaining sample while `Fatigue` and `Hunger` climb past 100 overrides
/// each.
///
/// **The topic genuinely does switch, which is why "impossible" was the
/// wrong word.** Three of this test's own six seeds (3, 4 and 42) see the
/// pursued drive change inside the 70 sampled points, and the sweep below
/// asserts that switching is still real rather than assuming it. Two
/// direct probes, both run for this correction:
///
/// - at this test's exact parameters — 6 seeds x 12 x `!wait 5` — **70/70**
///   ask-observable points sit at `overrides_of(topic) == 0`, with 3/6 seeds
///   switching topic;
/// - at a far longer horizon — 6 seeds x 120 x `!wait 30`, 720 points —
///   **0/720** points had a non-zero count, while the losing drives reached
///   119-120 overrides apiece.
///
/// At `overrides == 0`, `stance_for`'s first match arm is
/// `0 => Stance::Forthcoming`, **unconditional on `prior`**, so every one of
/// the 70 points below was guaranteed to agree across all three priors
/// before a single tick was simulated. The sweep therefore CORROBORATES the
/// null; it does not independently test it. The positive control above
/// (`h4_positive_control_the_divergence_detector_can_fire`) proves the
/// comparison ITSELF can register a divergence when `overrides != 0`; this
/// sweep never reaches that regime.
///
/// **Why the distinction is load-bearing rather than pedantic:** a follow-up
/// campaign reading "impossible by construction" would conclude the only
/// lever is widening what a host can be ASKED. That is one lever, but a
/// change to arbitration stickiness — anything that lets a drive regain the
/// topic after a spell of losing — would make this mechanism live without
/// touching `ask()` at all.
///
/// **Reachability, checked directly rather than assumed:** the state H4
/// would need — the pursued drive changing to one that already carries
/// override history — was **never observed**, and is **not reachable
/// through any currently-shipped verb** by the argument below. Note the two
/// are different claims: the paragraph below rules out the PLAYER forcing
/// the switch, while the stickiness measured above is about the sim never
/// producing it unprompted. Neither is an impossibility proof.
/// `IN_CHARACTER_VERBS` (`session.rs`) is a closed, exhaustive
/// 18-verb roster with no drink/eat/relief verb; the driven body's own
/// passive `!wait` walk discards every fact it would otherwise commit
/// (`_driven_facts` is unconditionally dropped, `session.rs:4090` and the
/// surrounding comment), so a driven body's own `DRANK`/`EATEN` facts are
/// never written by ANY path. The only verb that resolves a drive at all is
/// `sleep`, which resolves `Fatigue` alone (`rested_fact`,
/// `session.rs:2127`) — it cannot unseat `Thirst`/`Hunger` (the two
/// observed dominant drives; both ceiling-1.0 survival drives, tie broken
/// `Thirst`-first by declared `DriveKind` order), because it never touches
/// them. Not exhaustively ruled out: whether extreme positioning via `go`
/// could spike the position-dependent `Thermal` flow drive high enough to
/// win over an already-saturated survival drive was not tested; that
/// remains an open, undemonstrated door rather than a closed one.
///
/// claim: readout(seeds: 1,2,3,4,5,42 — H4's own preregistered measurement,
/// decision 0016; a fixed, small, named roster, never a sweep to FIND an
/// instance, decision 0093)
// Named construction site (decision 0092): one sculpt/fit per sampled
// session.
#[allow(clippy::disallowed_methods)]
#[test]
fn h4_does_the_prior_move_observable_testimony_at_all() {
    let seeds = [1u64, 2, 3, 4, 5, 42];
    let ticks_per_session = 12;

    let mut denominator = 0usize;
    let mut diverged = 0usize;
    let mut degenerate_zero_overrides = 0usize;
    let mut sessions_with_observations = 0usize;
    let mut sessions_whose_topic_switched = 0usize;

    for seed in seeds {
        let world = generated(seed);
        let Ok((mut s, _)) = Session::start(&world, &PossessOpts::default()) else {
            continue;
        };
        let terrain = hornvale_worldgen::terrain_of(&world).expect("sculpts");
        let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("fits");
        let species = s.driven_body().species.clone();
        let prior = prior_of(&world, &species, &terrain, &climate);

        let mut this_session_observed = false;
        let mut topics_seen: std::collections::BTreeSet<DriveKind> =
            std::collections::BTreeSet::new();
        for _ in 0..ticks_per_session {
            s.handle("!wait 5");
            let Some(topic) = s.driven_affect_object() else {
                continue;
            };
            let overrides = s.overrides_of(topic);
            denominator += 1;
            this_session_observed = true;
            topics_seen.insert(topic);
            if overrides == 0 {
                degenerate_zero_overrides += 1;
            }
            if any_other_prior_diverges(prior, overrides) {
                diverged += 1;
            }
        }
        if this_session_observed {
            sessions_with_observations += 1;
        }
        if topics_seen.len() > 1 {
            sessions_whose_topic_switched += 1;
        }
    }

    assert!(
        sessions_with_observations > 0,
        "H4 needs at least one session with an observable ask topic to be a real measurement"
    );
    assert!(
        denominator > 0,
        "H4's denominator must be non-zero for the count below to mean anything"
    );

    // THE MEASURED NUMBERS, PINNED. The chronicle quotes "seventy observable
    // points across six sessions" and "zero divergences" as this campaign's
    // headline null; before the final-fix wave this test asserted only
    // `denominator > 0`, so the prose and the instrument could drift apart
    // silently. These four assertions are the campaign's reported result,
    // not a threshold to tune. If the WORLD moves them, that is a finding to
    // report and re-quote in the chronicle — do NOT retune
    // `stance::patience()` to restore them (root `CLAUDE.md`, decision 0016).
    assert_eq!(
        (denominator, sessions_with_observations),
        (70, 6),
        "H4's reported denominator moved; the chronicle and retrospective quote \
         seventy observable points across six sessions and must be re-quoted together \
         with this assertion"
    );
    assert_eq!(
        diverged, 0,
        "H4's headline null moved: the doctrine prior now DOES move observable \
         testimony at {diverged}/{denominator} points. That is a real finding and \
         belongs in the chronicle — it is not a reason to change stance::patience()"
    );
    assert_eq!(
        degenerate_zero_overrides, denominator,
        "every H4 point is expected to sit at overrides_of(topic) == 0, where all \
         three priors agree unconditionally. If this is no longer ALL of them, the \
         sweep has entered the regime that could actually falsify the null and the \
         doc comment above (which says it never does) is now wrong"
    );

    // The stickiness premise the doc comment above rests on, asserted rather
    // than assumed: the pursued drive DOES change within these 70 points (3
    // of the 6 seeds when measured), which is precisely why the null cannot
    // be explained as "the topic and the record are disjoint by
    // construction". A drop to zero would mean every session pursued one
    // drive forever, and the restatement above would need rewriting.
    assert!(
        sessions_whose_topic_switched > 0,
        "H4's sampled sessions never once changed the pursued drive, so this sweep \
         can no longer distinguish 'sticky arbitration' from 'a single drive \
         forever' — the mechanism paragraph in this test's doc comment depends on \
         the switch being real"
    );

    // The measurement itself, split into the two halves the doc comment
    // above states separately: how many points were even capable of
    // showing divergence (non-degenerate, overrides != 0) vs how many
    // actually diverged. ZERO divergence over a denominator where EVERY
    // point is degenerate is the corroborating-not-independent result this
    // campaign reports; it would read differently (and be a real 70-trial
    // sweep) if `non_degenerate` below were not also 0. Do NOT retune
    // `stance::patience()` regardless of what this prints.
    let non_degenerate = denominator - degenerate_zero_overrides;
    println!(
        "H4: prior moved observable testimony in {diverged}/{denominator} ask-observable \
         points across {sessions_with_observations}/{} sessions \
         ({degenerate_zero_overrides}/{denominator} were the degenerate overrides=0 case, \
         where every prior agrees unconditionally; only {non_degenerate}/{denominator} could \
         have shown divergence at all; {sessions_whose_topic_switched}/{} sessions changed \
         the pursued drive at least once, so the zero is stickiness rather than \
         per-tick disjointness)",
        seeds.len(),
        seeds.len()
    );
}
