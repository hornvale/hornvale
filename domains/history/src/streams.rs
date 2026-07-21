//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations (Task 2);
//! the deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here as they are implemented (Task 3 onward), the same way
//! this crate's flesh derivations draw against `history/flesh/*`.

use hornvale_kernel::seed::StreamLabel;

/// Root stream label for history — reserved for the deep-history bake
/// (Task 3). The composition root derives `history/flesh` once per
/// occupation before calling into this crate's flesh derivations (Task 2),
/// which further derive their own sub-labels ([`RESIDUE`], [`STRUCTURES`])
/// from that already-scoped seed.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("history");
/// Sub-label for `flesh::residue_of`'s deterministic flavor draws (Task 2).
/// type-audit: bare-ok(identifier-text: return)
pub const RESIDUE: StreamLabel<'static> = StreamLabel::from_static("residue");
/// Sub-label for `flesh::structures_of`'s dwelling-count variance draws
/// (Task 2).
/// type-audit: bare-ok(identifier-text: return)
pub const STRUCTURES: StreamLabel<'static> = StreamLabel::from_static("structures");
/// Root stream label for the deep-history bake's epoch dynamics (Task 3):
/// grow/found/migrate/raid/collapse draws, taken sequentially from one
/// stream in commit order at the composition root
/// (`windows/worldgen::history_bake::bake`).
/// type-audit: bare-ok(identifier-text: return)
pub const BAKE: StreamLabel<'static> = StreamLabel::from_static("history/bake");
/// Root stream label for the deep-history bake's genesis draws (Task 3):
/// how many proto-communities a people seeds with, which sites they take,
/// and their tech-advance offset. The bake further derives a per-people
/// sub-stream `history/genesis/<people-kind>` from this label (a
/// `StreamLabel::dynamic(people.0)` derive), one per entry in the bake's
/// `peoples` list, so each people's genesis draws are independent of draw
/// order across peoples.
/// type-audit: bare-ok(identifier-text: return)
pub const GENESIS: StreamLabel<'static> = StreamLabel::from_static("history/genesis");
/// The per-occupation flesh seed the legibility surface derives before
/// expanding residue/structures on demand: `seed.derive(FLESH).derive(
/// StreamLabel::dynamic(&entity_id))`. Flesh is never committed, so this
/// label scopes only on-demand rendering, but it is a permanent derivation
/// contract and is declared here like every other.
/// type-audit: bare-ok(identifier-text: return)
pub const FLESH: StreamLabel<'static> = StreamLabel::from_static("history/flesh");
