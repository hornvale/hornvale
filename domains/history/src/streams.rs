//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations; the
//! deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here, the same way this crate's flesh derivations draw
//! against `history/flesh/*`.
//!
//! Declared via `stream_labels!`'s root+legs+flat form (PROC-19): `ROOT`/
//! `RESIDUE`/`STRUCTURES` are a genuine root+leg group (real `.derive()`
//! chains compose them), while `BAKE`/`GENESIS`/`FLESH` are independent,
//! already-fully-qualified roots of their own — the bake that draws from
//! them runs at a different call site entirely and never chains off this
//! crate's `ROOT`.

hornvale_kernel::stream_labels! {
    /// Root stream label for history — reserved for the deep-history bake.
    /// The composition root derives `history/flesh` once per occupation
    /// before calling into this crate's flesh derivations, which further
    /// derive their own sub-labels (`RESIDUE`, `STRUCTURES`) from that
    /// already-scoped seed.
    root: ROOT = "history" => "root stream for history: reserved for the deep-history bake (run at the composition root); no draw is made against it directly";
    legs {
        /// Sub-label for `flesh::residue_of`'s deterministic flavor draws.
        RESIDUE = "residue" => "flesh::residue_of's deterministic flavor draws";
        /// Sub-label for `flesh::structures_of`'s dwelling-count variance draws.
        STRUCTURES = "structures" => "flesh::structures_of's dwelling-count variance draws";
    }
    flat {
        /// Root stream label for the deep-history bake's epoch dynamics:
        /// grow/found/migrate/raid/collapse draws, taken sequentially from
        /// one stream in commit order at the composition root
        /// (`windows/worldgen::history_bake::bake`).
        BAKE = "history/bake" => "the deep-history bake's epoch dynamics: grow/found/migrate/raid/collapse draws, taken sequentially from one stream in commit order at the composition root";
        /// Root stream label for the deep-history bake's genesis draws:
        /// how many proto-communities a people seeds with, which sites
        /// they take, and their tech-advance offset. The bake further
        /// derives a per-people sub-stream `history/genesis/<people-kind>`
        /// from this label (a `StreamLabel::dynamic(people.0)` derive),
        /// one per entry in the bake's `peoples` list, so each people's
        /// genesis draws are independent of draw order across peoples.
        GENESIS = "history/genesis" => "the deep-history bake's genesis draws: proto-community count, site picks, and tech-advance offset; further derives a per-people sub-stream history/genesis/<people-kind> via StreamLabel::dynamic";
        /// The per-occupation flesh seed the legibility surface derives
        /// before expanding residue/structures on demand:
        /// `seed.derive(FLESH).derive(StreamLabel::dynamic(&entity_id))`.
        /// Flesh is never committed, so this label scopes only on-demand
        /// rendering, but it is a permanent derivation contract and is
        /// declared here like every other.
        FLESH = "history/flesh" => "the per-occupation flesh seed the legibility surface derives before expanding residue/structures on demand (never committed)";
    }
}
