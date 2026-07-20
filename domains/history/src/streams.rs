//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations (Task 2);
//! the deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here as they are implemented (Task 3 onward), the same way
//! this crate's flesh derivations will draw against `history/flesh/*`.

/// Root stream label for history — reserved for the deep-history bake
/// (Task 3). The composition root derives `history/flesh` once per
/// occupation (`world.seed.derive("history/flesh").derive(&occ.community.0.to_string())`)
/// before calling into this crate's flesh derivations (Task 2), which
/// further derive their own sub-labels ([`RESIDUE`], [`STRUCTURES`]) from
/// that already-scoped seed.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "history";
/// Sub-label for `flesh::residue_of`'s deterministic flavor draws (Task 2).
/// type-audit: bare-ok(identifier-text)
pub const RESIDUE: &str = "residue";
/// Sub-label for `flesh::structures_of`'s dwelling-count variance draws
/// (Task 2).
/// type-audit: bare-ok(identifier-text)
pub const STRUCTURES: &str = "structures";
