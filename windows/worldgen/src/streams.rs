//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

hornvale_kernel::stream_labels! {
    /// The folk causal-schema-selection sub-leg, under a culture's own
    /// `hornvale_language::streams::ROOT` derivation.
    SCHEMA = "schema" => "the folk causal-schema-selection sub-leg";
    /// The sky-domain fact-shape sub-leg, under `SCHEMA`.
    SKY = "sky" => "the sky-domain fact-shape sub-leg";
    /// The lexicalization sub-leg for a chosen schema's rendered sentence.
    LEXEME = "lexeme" => "the lexicalization sub-leg for a chosen schema";
    /// The doctrine (institutional) causal-schema-selection sub-leg — the
    /// doctrine-voice twin of `SCHEMA`.
    DOCTRINE_SCHEMA = "doctrine-schema" => "the doctrine-voice twin of the folk schema-selection leg";
    /// The doctrine-voice twin of `LEXEME`.
    DOCTRINE_LEXEME = "doctrine-lexeme" => "the doctrine-voice twin of the lexeme leg";
    /// The deity-naming stream, epoch v2 (a full flat path, not a composed
    /// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
    RELIGION_DEITY_V2 = "religion/deity/v2" => "the deity-naming stream, epoch v2";
    /// The per-settlement disposition draw (The Tolerance). A flat path, like
    /// `RELIGION_DEITY_V2`: a composition-root concern, because the draw needs
    /// `hornvale_species::Dispersion` AND the occupation's own site, and no
    /// domain crate may depend on a sibling.
    ///
    /// **Keyed on the occupation record's `(site, founded-year)` pair** — see
    /// [`crate::disposition`] for why that key, and not the settlement's
    /// `EntityId`, its `BakeId`, or its bare current `cell-id`.
    SETTLEMENT_DISPOSITION = "settlement/disposition/v1" => "the per-settlement disposition draw, keyed on the occupation's (site, founded-year)";
    /// The underworld chamber derivation (The Deep Realm). Spelled by
    /// `crate::chamber`'s private `chamber_key`, but **`chamber_key` is a
    /// DISPLAY FORMATTER now, not a derivation key** (The Drift, spec
    /// amendment A.6, correcting an earlier A.4 that got the mechanism
    /// wrong): Task 1 deleted the chamber existence draw, the only
    /// production reader of a stream composed under this label, so nothing
    /// in a shipped world derives from `CHAMBER` any more. What actually
    /// carries the underworld's seed-derivation key is [`RUN_FLOORS`] and
    /// the three per-branch labels in `crate::character`
    /// (`BRANCH_COUNT`/`BRANCH_CHARACTER`/`BRANCH_BARRIER`).
    ///
    /// **Stays at v3 through The Drift, deliberately** (amendment A.6): The
    /// Drift dropped `entrance` from `ChamberAddr` (amendment A.3), which
    /// does change `chamber_key`'s spelling — but since nothing derives from
    /// it, bumping this label would record a discontinuity that never
    /// happened through it, an **empty epoch**. `chamber_key`'s sole
    /// production caller is `underworld_readout.rs`'s witness, which prints
    /// it as a `key` column for a human to read.
    ///
    /// **Epoch v3 (The Stope, spec §3.1 and amendment B.3).** The ADDRESS
    /// changed shape, in three ways that each re-key every chamber and which
    /// therefore ride one epoch rather than three:
    ///
    /// 1. `ChamberAddr` gained a **`floor`** (since renamed `level`), the
    ///    rung the lattice was missing — a band used to be one
    ///    interior-less point per column. A key that did not spell it would
    ///    derive one stream for every floor of a run, which is to say the
    ///    floors would all be one chamber.
    /// 2. `slot` was renamed **`branch`** (spec §3.1: "slot reads as a
    ///    position and it is an identity"), and the key's field order changed
    ///    with it.
    /// 3. The deepest rung was renamed **`Sunless` -> `Nadir`** (amendment
    ///    B.3), and `chamber_key` spells the rung's NAME, so the rename alone
    ///    relocates every chamber that sits at rank 4.
    ///
    /// **Epoch v2 (The Underworld, spec §4.1)** was the previous one:
    /// `ChamberAddr.band` stopped indexing the stratigraphic ladder
    /// (`Horizon`/`Stratum`) and started indexing the **delve ladder**
    /// (`hornvale_terrain::DelveRung`), whose rungs are ΔT thresholds above
    /// the surface datum rather than rock units.
    ///
    /// `chamber/v1` and `chamber/v2` are retired and must never be reused.
    CHAMBER = "chamber/v3" => "a display-only address formatter; the underworld's real derivation key is RUN_FLOORS and the per-branch legs";
    /// How many levels one **run** (a branch's own place within one band)
    /// realizes (The Stope, Task 2; spec §3.1's per-band ranges). Keyed on a
    /// [`crate::chamber::RunAddr`]: cell, branch and band, a place in the
    /// fixed lattice and never a generation ordinal (decision 0102).
    ///
    /// **Epoch v2 (The Drift, amendment A.3/A.6).** `entrance` dropped out of
    /// `RunAddr` — every entrance of a system now addresses INTO the same
    /// shared lattice rather than realizing its own private sublattice — and
    /// this label is a LIVE production leg (`levels_in_branch` is
    /// `chamber_exists`'s own floor gate), so the re-keying rides an epoch
    /// rather than landing silently under `/v1`. `chamber/run-floors/v1` is
    /// retired and must never be reused.
    ///
    /// **A SEPARATE ROOT LEG FROM [`CHAMBER`], AND THAT IS THE COLLISION
    /// ARGUMENT.** A run key (`cell/branch/band`) is a strict prefix of a
    /// chamber key (`cell/branch/band/level`), so the two strings can never
    /// be equal — but prefix-inequality is a property of today's spelling,
    /// and a later campaign that made `level` optional in the key would
    /// break it silently. Deriving the run draw under its own permanent
    /// label instead means the two dynamic legs hang off **different parent
    /// seeds**, so even a byte-identical key string yields a different
    /// stream. `the_run_leg_and_the_chamber_leg_cannot_collide` in
    /// `crate::chamber` asserts exactly that, on the same string.
    ///
    /// What else changes with this leg: before it every in-budget run
    /// admitted all [`crate::chamber::LEVELS_PER_BRANCH_CEILING`] levels, and
    /// now it admits the drawn count. That is a world change, carried by the
    /// gate rather than by the key.
    RUN_FLOORS = "chamber/run-floors/v2" => "how many levels one run realizes, keyed on (cell, branch, band)";
    /// Which [`crate::character::Character`] one branch carries (The Stope,
    /// Task 3; spec B.4/B.5). Keyed on a **branch at a band** — cell, branch
    /// and band, a place in the fixed lattice and never a generation ordinal
    /// (decision 0102). A SEPARATE root leg from [`CHAMBER`] for the same
    /// collision argument [`RUN_FLOORS`]'s doc states: additive, perturbs no
    /// existing draw.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5).** `entrance` dropped
    /// out of the key and `band` moved in: before this change a branch's
    /// character was a per-SYSTEM fact (one answer for the whole depth),
    /// after it a per-`(system, band)` fact — a branch can carry a different
    /// character at each band it occupies. This is a LIVE production leg
    /// (`character_at` is `chamber_at`'s own character read), so the
    /// re-keying rides an epoch. `chamber/branch-character/v1` is retired
    /// and must never be reused.
    BRANCH_CHARACTER = "chamber/branch-character/v2" => "which character one branch carries, keyed on (cell, branch, band)";
    /// How thin the barrier between the underworld and what lies beyond it
    /// is, on one branch (The Stope, Task 3; spec B.5). Same key shape as
    /// [`BRANCH_CHARACTER`] — character and barrier are ONE object per B.5:
    /// same owner, same lattice key — but its own parent leg, so the two
    /// draws cannot collide even by accident.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5)**, same shape and same
    /// reason as [`BRANCH_CHARACTER`]'s own v2 note: `entrance` out, `band`
    /// in, re-keyed at the identical granularity so character and barrier
    /// stay one object (B.5) rather than splitting across two. `chamber/
    /// branch-barrier/v1` is retired and must never be reused.
    BRANCH_BARRIER = "chamber/branch-barrier/v2" => "the barrier thinness of one branch, keyed on (cell, branch, band)";
    /// How many of the lattice's four branch columns one cave system
    /// realizes (The Stope, Task 3; amendment C.1) — the drawn realization
    /// half of the lattice-ceiling/drawn-realization split, with
    /// `BRANCHES_PER_SYSTEM` as the ceiling. Keyed on the SYSTEM **at a
    /// band**: cell and band, no branch.
    ///
    /// **Epoch v2 (The Drift, amendment A.3, Task 5).** `entrance` dropped
    /// out and `band` moved in: before this change a system had ONE branch
    /// width for its whole depth; after it, a system may realize a
    /// different width at each band — a system can be two branches wide in
    /// the Undercroft and one wide in the Shallows. `chamber_exists` reads
    /// this leg directly (`addr.branch >= branch_count_of(seed, addr.cell,
    /// addr.band)`), so it is a LIVE production leg and the re-keying rides
    /// an epoch. `chamber/branch-count/v1` is retired and must never be
    /// reused.
    BRANCH_COUNT = "chamber/branch-count/v2" => "how many branches one cave system realizes, keyed on (cell, band)";
    /// How many apertures one cave system opens to the surface (The Stope,
    /// Task 5; amendment C.3). Keyed on the SYSTEM's cell alone — no
    /// entrance index, because the count is a fact about the system as a
    /// whole and an entrance index could not be defined before this draw
    /// answered. Terrain reports one cave per cell with no aperture count,
    /// so the plural is derived here at the composition root rather than
    /// read off the cave.
    ///
    /// A separate root leg from [`CHAMBER`] for the same collision argument
    /// [`RUN_FLOORS`]'s doc states: additive, perturbs no existing draw,
    /// and the `/v1` epoch discipline applies to any later re-shaping of
    /// the key.
    ENTRANCE_COUNT = "chamber/entrance-count/v1" => "how many surface apertures one cave system opens, keyed on cell";
    /// Which floor of the system's lattice one entrance opens into (The
    /// Stope, Task 5; amendment C.3) — main-line floor 0, or a branch's
    /// root floor (C.2). Keyed on the ENTRANCE's place: cell and entrance
    /// index, a place in the fixed lattice and never a generation ordinal
    /// (decision 0102). Same separate-root-leg and `/v1` discipline as
    /// [`ENTRANCE_COUNT`].
    ENTRANCE_MOUTH = "chamber/entrance-mouth/v1" => "which floor of the lattice one entrance opens into, keyed on (cell, entrance)";
    /// Where a non-main-line branch hangs off its parent (The Stope, Task 3;
    /// amendment C.2) — a floor of the main line, drawn over the floors that
    /// parent actually realizes. Keyed on the CHILD branch's place: cell,
    /// entrance, branch (the child names itself; its parent is always the
    /// main line, whose own root is the surface).
    BRANCH_ROOT = "chamber/branch-root/v1" => "where a branch roots on its parent, keyed on (cell, entrance, branch)";
    /// The volcano-identity derivation (The Repose). Keyed on the edifice's
    /// **source contact cell** — a place in the fixed geosphere, never a
    /// generation ordinal, and never the query cell a caller happened to ask
    /// about (an edifice spans 1-2 cells, so the query cell would mint two
    /// mountains for one). `crate::volcano`'s private `volcano_key` is the one
    /// place the composed key is spelled. The third time this project has met
    /// the "generation order is never an identity" wall (decision 0102, The
    /// Salt, The Tolerance); nothing here carries an ordinal so that mistake
    /// cannot recur.
    VOLCANO = "volcano/v1" => "the volcano-identity derivation, keyed on the edifice's source contact cell";
    /// The per-cell hazard-event draw (The Repose). Keyed on a **place in
    /// space and a place in time** — the cell, the process (seismic or
    /// eruption), and the index of a fixed 1,000-year block of world time —
    /// and **never on the window a caller asked about**. That is the whole
    /// design: the event sequence of a `(seed, cell)` exists independently of
    /// who asks, so a narrower query filters the same sequence rather than
    /// drawing an unrelated one. A block index is a coordinate on a lattice
    /// that tiles the timeline before anything is generated into it, exactly
    /// as `ChamberAddr`'s `band`/`branch`/`floor` are — not a generation ordinal
    /// (decision 0102, The Salt, The Tolerance). `crate::hazard`'s private
    /// `event_key` is the one place the composed key is spelled.
    HAZARD_EVENT = "hazard/event/v1" => "the per-cell hazard-event draw, keyed on (cell, process, world-time block)";
}
