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

/// The first settlement in ledger order whose occupying people resolves to
/// [`hornvale_species::HabitatRealm::Surface`] —
/// [`hornvale_vessel::PossessTarget::LandSettlement`]'s resolution, and the
/// walk fixtures' subject (The Tidemark, Task 3).
///
/// **Ledger order, exactly as the flagship's is**, so this is
/// `hornvale_settlement::village_info` with one filter rather than a second
/// ranking rule: a caller that asks for land gets the settlement it always
/// got unless that settlement's people is not a surface one.
///
/// # Why a caller must ask
///
/// `village_info` means "the first `is-settlement` fact in ledger order". It
/// was never a claim about where a demo should stand — it is an ordering
/// artifact that stayed on land only because every people in the roster lived
/// on land. The Tidemark authored six obligate MARINE peoples, seed 42's
/// first settlement became an abyssal-elf one on the bathypelagic band, and
/// roughly sixty walk and scene fixtures failed at once — not as re-pins:
/// `there_is_nothing_to_dive_into_on_dry_land` cannot pass at a vertex that
/// is water, and the chart's cover and colour layers stopped varying because
/// the whole walk band was open sea. **If you are adding a people and the
/// walk suite has gone strange, this is the paragraph you were looking for.**
/// Adding ANY people reorders the ledger; choose the subject you mean.
///
/// # Why the realm, not the biome
///
/// An earlier revision filtered on the settlement's own committed `biome`
/// fact being non-marine (`!hornvale_climate::Biome::is_marine`). That
/// predicate answers a narrower question than the one this function needs:
/// `Biome::is_marine` is an explicit match over **surface** marine variants,
/// and cannot see realm at all — a subterranean settlement commits its
/// vertex's *surface* biome (a drow or duergar hold under a forest commits
/// `"temperate forest"`), so it passed the old filter and was handed to a
/// caller that asked for land. Four subterranean peoples (`kuo-toa`,
/// `duergar`, `svirfneblin`, `mountain-dwarf`) landed on main in the recent
/// absorb, making this reachable rather than hypothetical. Filtering on the
/// occupying people's [`hornvale_species::HabitatRealm`] instead asks the
/// positive question directly: is this a surface people, full stop — the
/// same axis `Marine` peoples were added to distinguish, applied to its
/// `Subterranean` sibling as well.
///
/// # Why it lives here
///
/// It needs `hornvale_settlement`'s ledger accessor and
/// `hornvale_species`'s habitat-realm registry at once, and a domain may not
/// depend on a sibling — so the composition root is the only place both are
/// visible. It is the same constraint that put `axis_geometry`'s
/// reconciliation here.
///
/// Reads the settlement's own committed `peopled-by` fact
/// (`hornvale_species::species_of`) rather than re-deriving a species from
/// terrain or placement, so this and the room a walk then renders cannot
/// disagree about which people occupies the subject. A subject chosen by one
/// reading and rendered by another is the defect this function exists to
/// remove, not a smaller version of it.
///
/// `None` when the world holds no settlement, and when EVERY settlement is
/// non-surface (marine or subterranean). Both are the honest answer; falling
/// back to the flagship would hand a non-land subject to a caller that asked
/// for land.
pub fn land_settlement(world: &World) -> Option<hornvale_settlement::VillageInfo> {
    hornvale_settlement::all_settlements(world)
        .into_iter()
        .find(|v| {
            let Some(species) = hornvale_species::species_of(world, v.id) else {
                // A settlement with no committed `peopled-by` fact is not
                // evidence of land. Skip it: this function's value is that a
                // `Some` answer is a settlement KNOWN to house a surface
                // people.
                return false;
            };
            hornvale_species::habitat_realm_registry()
                .get_by_label(&species)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
                == hornvale_species::HabitatRealm::Surface
        })
}

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
