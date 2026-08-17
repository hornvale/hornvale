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
    /// The underworld chamber derivation (The Deep Realm). Keyed on a
    /// ChamberAddr — a place in a fixed lattice, never a generation
    /// ordinal. `crate::chamber`'s private `chamber_key` is the one place
    /// the composed key is spelled.
    ///
    /// **Epoch v2 (The Underworld, spec §4.1).** `ChamberAddr.band` used to
    /// index the stratigraphic ladder (`BandKind`/`Stratum`) and now indexes
    /// the **delve ladder** (`hornvale_terrain::DelveRung`), whose rungs are
    /// ΔT thresholds above the surface datum rather than rock units. The
    /// composed key therefore spells a different set of names for the same
    /// `(cell, entrance, slot)`, which re-derives every chamber in every
    /// world — exactly the case `chamber_key`'s own doc said would be "an
    /// epoch, not a fix to that assertion". `chamber/v1` is retired and must
    /// never be reused.
    CHAMBER = "chamber/v2" => "the underworld chamber derivation, keyed on a delve-ladder address";
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
    /// as `ChamberAddr`'s `band`/`slot` are — not a generation ordinal
    /// (decision 0102, The Salt, The Tolerance). `crate::hazard`'s private
    /// `event_key` is the one place the composed key is spelled.
    HAZARD_EVENT = "hazard/event/v1" => "the per-cell hazard-event draw, keyed on (cell, process, world-time block)";
}
