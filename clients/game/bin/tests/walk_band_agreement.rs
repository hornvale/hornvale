//! The client's finest zoom rung **is** the sim's walk band, and this file is
//! what keeps that true.
//!
//! ## Why it exists
//!
//! `plate::BAND_B_RUNG` restates the walk-band depth **absolutely** — a bare
//! integer, with none of the arithmetic a scanner could recognise. When The
//! Pavement moved the walk band one rung finer (decision 0511), the constant
//! stayed where it was and **no client test failed**: every one of its ~80
//! readers reads the constant, so a wrong constant is perfectly
//! self-consistent, and `make game-check` — the only gate that can see this
//! tree — had nothing in it that compared the number to the sim. The client
//! drew a whole band coarser than the possession it was drawing, quietly.
//! Fixing the number without fixing the silence would leave the next move to
//! be found the same way.
//!
//! ## What it enforces, and what it is blind to
//!
//! Decision 0456 asks a check to state its direction, because a check that
//! does not reads as total. This one enforces exactly one equality:
//!
//! > the client's drawn rung equals the sim's walk depth **at the canonical
//! > globe level**.
//!
//! Both halves come from running code, never from a second copy of the
//! formula. The depth is whatever `hornvale_vessel::walk_depth` returns — a
//! `pub use` of `hornvale_locale::walk_depth`, which is the one definition of
//! the walk band in this repository — asked of a real
//! context built from a real world. Re-deriving the offset here would agree
//! with nothing: it would pass against a stale sim just as happily as against
//! a current one, which is the failure mode this file exists to close.
//!
//! It is blind three ways, all deliberate:
//!
//! 1. **A world pinned to another globe level is not covered.** Both
//!    constants track one canonical level and the client has no per-world
//!    rung ladder; `plate::GLOBE_RUNG`'s own doc records that the ladder's
//!    floor is the canonical grid. A world generated at a different level
//!    would move the sim's walk depth and leave both client constants where
//!    they are, and nothing here would notice. The premise assertion below
//!    at least makes the canonical level a *checked* premise rather than an
//!    assumed one, so this blindness cannot widen without being seen.
//! 2. **It says nothing about the rungs BETWEEN the two ends.** That the
//!    ladder is walkable and each rung distinct is `plate.rs`'s and
//!    `driver.rs`'s own business.
//! 3. **It is a value check on one seed.** `walk_depth` reads nothing but the
//!    context's globe level, so one world is the whole domain of the
//!    function — but if that ever stops being true, one seed stops being
//!    enough.
//!
//! ## Cost
//!
//! One world's terrain and climate derivation, shared through
//! `hornvale_vessel::WorldContext` — the cheapest publicly reachable way to
//! obtain a real context from this crate, which already depends on
//! `hornvale-vessel` by path. No new dependency, in-repo or otherwise. The
//! measured cost is in this task's report; it is a few seconds, against a
//! `make game-check` already measured in minutes.

use hornvale_game::plate::{BAND_B_RUNG, GLOBE_RUNG};
use hornvale_kernel::{Seed, World};
use hornvale_vessel::{WorldContext, walk_depth};

/// The client's band-B rung equals the sim's own walk depth.
#[test]
fn band_b_is_drawn_at_the_sims_own_walk_depth() {
    let world = hornvale_worldgen::fixture::seed_42_world();
    let wctx = WorldContext::build(&world).expect("seed 42 derives a world context");
    let ctx = wctx.context();

    // THE PREMISE, checked and not assumed: the equality below is only about
    // the CANONICAL globe level, so a context whose level is not the client's
    // own `GLOBE_RUNG` would make the assertion mean something else entirely
    // — and it would still pass or fail for reasons no reader could name.
    assert_eq!(
        ctx.globe_level(),
        GLOBE_RUNG,
        "this check compares the client's rungs against the sim at the CANONICAL globe \
         level, and the sim's canonical level has moved off `plate::GLOBE_RUNG`. Move \
         `GLOBE_RUNG` with it (and re-read `BAND_B_RUNG` below, which is stated relative \
         to the same level)."
    );

    assert_eq!(
        BAND_B_RUNG,
        walk_depth(ctx),
        "`plate::BAND_B_RUNG` is the mesh depth this client draws band B at — one tile per \
         facet, the walk band. It restates the sim's walk depth absolutely, so it must move \
         whenever `hornvale_locale::walk_depth` does. It does NOT match: the client is \
         drawing a different band from the one the possession walks, and every reader of \
         the constant (the whole `GLOBE_RUNG..=BAND_B_RUNG` zoom ladder included) is \
         following it there. Set the constant to what the function returns; do not restate \
         the arithmetic here."
    );
}
