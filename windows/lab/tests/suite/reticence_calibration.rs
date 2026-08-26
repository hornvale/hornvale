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

/// **H4 — the null this campaign is prepared to report** (spec section 5).
/// Whether the doctrine prior moves OBSERVABLE testimony at all, once the
/// fold is running: `count(sessions where prior and fold select different
/// stances) / denominator`.
///
/// Instrument chosen by reading `Session::ask`
/// (`windows/vessel/src/session.rs`), not prescribed from outside it:
/// `ask()`'s `overrides` value is `topic.map(|d| self.overrides_of(d))`,
/// where `topic == self.driven_affect_object()` — the CURRENTLY-PURSUED
/// drive's own object, never one of the arbitration's discarded ranks
/// (`driven_suppressed`, which is what `driven_overrides` accumulates
/// from). So the only override count `ask()` ever actually reads is the
/// pursued drive's own, and a drive that is winning arbitration is
/// structurally the one LEAST likely to have been overridden recently. A
/// probe over 3 seeds x 40 ticks (120 total observations, run by hand while
/// designing this test, not committed) found the pursued topic was
/// `DriveKind::Thirst` at overrides=0 on EVERY single tick — this is the
/// "found a discriminating instrument by reading" case the brief warned a
/// plan author cannot prescribe: the interesting question is not "does a
/// wide patience threshold ever get reached" but "does the topic the player
/// can actually ask about ever carry any overrides at all."
///
/// For each of several driven sessions, at each tick where a topic exists:
/// the REAL observed stance (`stance_for(actual_prior, overrides_of(topic))`)
/// is compared against what EVERY OTHER reachable prior would have produced
/// at that SAME observed override count, holding the fold fixed and varying
/// only the prior — the direct reading of "prior and fold select different
/// stances". A session/tick counts toward the numerator if any other prior
/// would have produced a different stance than the real one.
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
    let all_priors = [Openness::Guarded, Openness::Wary, Openness::Open];

    let mut denominator = 0usize;
    let mut diverged = 0usize;
    let mut sessions_with_observations = 0usize;

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
        for _ in 0..ticks_per_session {
            s.handle("!wait 5");
            let Some(topic) = s.driven_affect_object() else {
                continue;
            };
            let overrides = s.overrides_of(topic);
            denominator += 1;
            this_session_observed = true;
            let real_stance = stance_for(prior, overrides);
            let disagrees = all_priors
                .iter()
                .filter(|&&alt| alt != prior)
                .any(|&alt| stance_for(alt, overrides) != real_stance);
            if disagrees {
                diverged += 1;
            }
        }
        if this_session_observed {
            sessions_with_observations += 1;
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

    // The measurement itself. ZERO is the headline this campaign is
    // prepared to report, not a failure: it would mean the prior never
    // changes what a player can actually hear, over every ask-observable
    // point sampled, because the topic ask() reads is structurally the
    // drive least likely to carry any overrides at all. Do NOT retune
    // `stance::patience()` regardless of which way this comes out.
    println!(
        "H4: prior moved observable testimony in {diverged}/{denominator} ask-observable \
         points across {sessions_with_observations}/{} sessions",
        seeds.len()
    );
}
