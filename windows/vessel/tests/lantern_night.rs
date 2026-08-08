//! How dark a chamber actually gets (The Lantern, spec §6 H4a).
//!
//! **This file takes a READING, it does not make a claim.** H4 — that a
//! rod-dominant eye sees where a human does not — is stated and tested at the
//! *model* level, on `to_srgb` at a named illuminance, in
//! `kernel/src/color.rs`. H4a asks the separate, genuinely uncertain
//! question of whether the shipped game ever puts a cell in that regime.
//!
//! Spec §4.2 makes the answer doubtful by construction, and the plan's
//! Task 4 nailed the reason down: `SIGHT_RADIUS = CHAMBER_SIDE / 2 = 4`, the
//! implicit torch rides on the observer at that same radius, and `shadowcast`
//! is symmetric — so **every cell you can see is lit by your own torch**, and
//! the dimmest possible cell is the one at the sight radius, at
//! `1 / (1 + 4²)` = 0.0588 of full. Only attenuation darkens anything, and
//! chambers are a few cells across.
//!
//! **The attenuation constant may not be tuned to change this number**
//! (spec §11 risk 2). If the reading says H4's regime is unreachable here,
//! that is a finding about where the campaign's drama lives — a second
//! source, a doused torch, a hearth that breaks a purely radial gradient —
//! not a failure to be fixed by moving a constant.

use hornvale_kernel::{Seed, World};
use hornvale_vessel::{PossessOpts, Session, SessionPlan, SpatialChannel};

/// The seeds the reading sweeps. Four rather than one: one world is an
/// anecdote, and seed 42 alone has given four wrong readings in this
/// project's history.
const NIGHT_SEEDS: [u64; 4] = [1, 42, 99, 256];

fn world_at(seed: u64) -> World {
    hornvale_worldgen::build_world(
        Seed(seed),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .unwrap_or_else(|e| panic!("seed {seed} builds: {e:?}"))
}

/// A real world's chamber plan, taken through a live possession — the whole
/// seam (fabric, light field, observer, `sense`, `to_srgb`, palette) runs
/// inside `Session::snapshot`, so this is what the game actually emits.
///
/// `None` when `enter` does not put the possession inside a building at this
/// seed; the caller reports that rather than counting it as a dark chamber.
fn chamber_plan_at_seed(seed: u64) -> Option<SessionPlan> {
    let world = world_at(seed);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).ok()?;
    session.handle("enter");
    match session.snapshot().ok()?.spatial {
        SpatialChannel::Chamber { plan } => Some(plan),
        SpatialChannel::Walk { .. } => None,
    }
}

/// Every colour a seed's entered chamber puts on the screen.
fn chamber_colours(seed: u64) -> Option<Vec<[u8; 3]>> {
    let plan = chamber_plan_at_seed(seed)?;
    let colours: Vec<[u8; 3]> = plan.palette.iter().filter_map(|e| e.color).collect();
    (!colours.is_empty()).then_some(colours)
}

/// H4a — **REPORTED, NOT PREDICTED** (spec §6).
///
/// The reading printed here is the finding; the assertions below only keep it
/// honest. Two of them:
///
/// 1. **The reading was taken at all.** An empty sweep would satisfy every
///    other assertion in this file vacuously.
/// 2. **A TRIPWIRE, deliberately inverted rather than relaxed.** A cell whose
///    cones have fallen below the display's first count emits a triple whose
///    three slots are *exactly equal* — that is what the scotopic term does,
///    and a torch-lit stone wall is never achromatic (its blue slot runs
///    about a tenth of its red). So an achromatic cell in a chamber means
///    H4's regime has become reachable on the chamber band. **A red here is a
///    finding to read, not a bug to fix**: re-run this reading, say in the
///    chronicle what made the chamber dark, and re-word the sentence below.
///    It must never be fixed by moving `light::ATTENUATION`.
#[test]
fn report_h4a_how_dark_a_chamber_gets() {
    let mut measured = 0;
    let mut dimmest: Option<(u64, [u8; 3])> = None;
    let mut achromatic = 0;

    for seed in NIGHT_SEEDS {
        let Some(colours) = chamber_colours(seed) else {
            eprintln!("H4a seed {seed}: `enter` reached no chamber with a colour — skipped");
            continue;
        };
        measured += 1;
        achromatic += colours
            .iter()
            .filter(|c| c[0] == c[1] && c[1] == c[2])
            .count();
        // "Dimmest" is by the BRIGHTEST slot: that is the quantity the
        // photopic threshold is stated in, so it is the one that decides
        // whether a cell has left cone vision behind.
        let here = colours
            .iter()
            .copied()
            .min_by_key(|c| c.iter().copied().max().unwrap_or(0))
            .expect("a non-empty colour list");
        eprintln!(
            "H4a seed {seed}: {} coloured cells, dimmest {here:?}",
            colours.len()
        );
        if dimmest.is_none_or(|(_, best)| here.iter().max() < best.iter().max()) {
            dimmest = Some((seed, here));
        }
    }

    assert!(
        measured >= 2,
        "only {measured} of {NIGHT_SEEDS:?} produced a chamber, so H4a was not \
         really read at all"
    );
    let (seed, triple) = dimmest.expect("a measured seed always yields a dimmest cell");
    eprintln!(
        "H4a READING: across seeds {NIGHT_SEEDS:?} the dimmest visible chamber cell \
         renders {triple:?} (seed {seed}); {achromatic} of the sweep's cells are \
         achromatic."
    );
    assert_eq!(
        achromatic, 0,
        "an achromatic chamber cell appeared: H4's regime is now reachable on the \
         chamber band, and this reading's sentence needs rewriting"
    );
    eprintln!(
        "H4a FINDING: H4's regime is UNREACHABLE on the chamber band. Every visible \
         cell is lit by the observer's own torch (shadowcast is symmetric), so the \
         darkest cell is the one at the sight radius, at 1/(1+4^2) of full — plainly \
         visible, not black. The campaign's drama lives in the hearth and in a second \
         source, not in darkness."
    );
}
