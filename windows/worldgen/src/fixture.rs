//! The committed seed-42 world, read instead of rebuilt (decision 0607).
//!
//! A full build costs ~3.0 s in a debug build; this read costs ~15 ms — a
//! ~200x difference, measured 2026-09-02. nextest is process-per-test, so an
//! in-process memo recovers nothing (93 of 100 world-building processes in
//! one observed run built exactly one world), which is why the carrier is a
//! file and not a `OnceLock`. Decision 0032 reached the same conclusion for
//! the census.
//!
//! **This module reads a byte-golden and does not author it.**
//! `cli/tests/suite/lens_purity.rs` and `cli/tests/suite/repose_byte_identity.rs`
//! build seed 42 and assert the file's bytes, so the fixture's freshness is
//! guarded by tests inside the suite rather than by a CI step — decision 0125
//! deleted CI, which is where decision 0032's own guarantee-split used to
//! live. `windows/worldgen/tests/suite/fixture.rs` pins load == build here.
//!
//! **What the fixture cannot carry:** `GeneratedTerrain` and
//! `GeneratedClimate` are `Clone` but deliberately not `Serialize`
//! ("Recomputed on demand, never serialized"). A caller needing those pays
//! the sculpt and the fit on top of the read, so its saving is measured, not
//! the ~200x above: 4.0x-4.2x for the two artifact-needing modules measured
//! (decision 0607 has the full spread).
//!
//! **That does NOT mean every caller wanting those objects must build.** Both
//! artifact-needing modules this campaign migrated re-derive from the *loaded*
//! world instead — `terrain_of(&w)` and `climate_from(&w, &terrain)`, the same
//! derivation they always ran — so the read serves them and neither carries a
//! build site. They are rostered `identity`, not `artifacts`.
//!
//! **The `artifacts` reason has exactly one row, and it arrived from
//! elsewhere.** This paragraph twice said the wrong thing about that: first it
//! asserted a row in the present tense when there were none, and then, having
//! been corrected to "no such caller exists today", it went stale within hours
//! when absorbing `main` brought `windows/worldgen/src/circuit_readout.rs` —
//! whose `terrain_for(seed)` helper *returns* a `GeneratedTerrain` and so could
//! not be served by a read at any seed, the fixture's own included. That is the
//! case the taxonomy was reserved for, and a campaign on another branch supplied
//! it without knowing the reason code existed.
//!
//! The lesson for this comment specifically: **a count belongs in the roster,
//! not in prose.** `cli/tests/fixtures/world-build-sites.tsv` is the place to
//! check which reasons carry rows, and it cannot go stale against itself.

use hornvale_kernel::World;

/// The committed seed-42 world, relative to THIS crate's manifest directory.
///
/// `env!("CARGO_MANIFEST_DIR")` expands against the crate containing the
/// macro — `windows/worldgen` — so this one fixed prefix resolves correctly
/// no matter which crate calls [`seed_42_world`]. Verified rather than
/// assumed: a probe placed a `pub fn` here and called it from an example in
/// `cli`, and it reported worldgen's directory, while `cli`'s own `../../`
/// resolved outside the repository. The prefix is crate-specific, which is
/// exactly why the loader lives in one crate instead of being copied.
const FIXTURE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../cli/tests/fixtures/world-seed-42.json"
);

/// The seed-42 world under default pins and a generated sky — byte-identical
/// to what `build_world` returns for `Seed(42), &SkyPins::default(),
/// &TerrainPins::default(), &SettlementPins::default()`, read from the
/// committed fixture instead of rebuilt.
///
/// Read at runtime rather than `include_str!`-ed: the file is 5.5 MB, and
/// baking it into every test binary that wants a world would pay the
/// compilation-unit cost that dominates this project's gate.
///
/// # Panics
///
/// If the fixture is missing or does not parse. Both mean the checkout is
/// broken in a way no caller can sensibly handle, and a panic naming the path
/// is more useful than a `Result` every call site would `expect` anyway.
pub fn seed_42_world() -> World {
    let json = std::fs::read_to_string(FIXTURE)
        .unwrap_or_else(|e| panic!("the committed seed-42 world is unreadable at {FIXTURE}: {e}"));
    World::from_json(&json)
        .unwrap_or_else(|e| panic!("the committed seed-42 world does not parse: {e}"))
}
