//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations (Task 2);
//! the deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here as they are implemented (Task 3 onward), the same way
//! this crate's flesh derivations will draw against `history/flesh/*`.

/// Root stream label for history — reserved for the deep-history bake
/// (Task 3) and the local flesh derivations (Task 2). No draw is made
/// against it yet; this crate is presently data-model-only.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "history";
