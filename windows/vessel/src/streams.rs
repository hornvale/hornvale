//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

hornvale_kernel::stream_labels! {
    /// Stream label for the minted agent's id draw.
    VESSEL_AGENT = "vessel/agent" => "minted agent id draw";
    /// Stream label for the walker battery's deterministic walk.
    VESSEL_WALK = "vessel/walk" => "walker-battery deterministic walk";
    /// Stream label for a room's furnishing draw (The Hearth). Versioned from
    /// its first commit (decision 0073): this layer is expected to churn, so
    /// bumping it must not disturb `room/child` or `room/face`.
    ///
    /// **STILL AT v1 AFTER The Blocking, and NOTHING DRAWS FROM IT.** Verified by
    /// grep: this literal has exactly one occurrence in the workspace — this
    /// declaration. `selection` and `selection_for` take no seed, so the label
    /// versions a derivation rather than a stream, and bumping it on its own
    /// re-mints nothing. A bump with no moved derivation is an *empty* epoch: it
    /// declares a discontinuity that did not occur and costs a permanent manifest
    /// row. The Blocking measured before deciding and did not bump.
    ///
    /// **LIVE as of The Threshold — read this before adding a pattern.** Under
    /// The Hearth this label versioned nothing that could move a world: no room
    /// derived an interior and every warmth read was `None`, so the inventory
    /// was free to grow. That is no longer true. A creature now stands at an
    /// anchor and its thermal drive reads the warmth there, so a *locale's*
    /// composed interior implies a warmth a committed drive reads.
    ///
    /// **The exact condition, corrected by The Blocking.** This comment used to
    /// say flatly that adding or reordering a pattern is an epoch. That became
    /// over-strict the moment role gating landed, and an over-strict warning is
    /// one that gets ignored — which is precisely how an *undeclared* epoch
    /// ships. The condition now has three cases, stated in full on
    /// [`crate::interior::INVENTORY`] and in summary here:
    ///
    /// - **Reordering or inserting: always an epoch.** The inventory's ORDER is
    ///   the grammar's dependency order, because a pattern requiring another is
    ///   admitted only once that other is present.
    /// - **Appending with `at_locale: true`: an epoch.** It changes what a locale
    ///   composes, so it changes warmth, so it changes committed drive history.
    ///   Worlds are reproducible within an epoch and not across one (0072).
    /// - **Appending with `at_locale: false`: LATENT.** No live read reaches it.
    ///   The Blocking appended five such patterns (the chamber roles' vocabulary)
    ///   and moved no metric golden. The gate opens on the first mark committed
    ///   *inside a chamber* — followup: `docs/followups.md`, TOOL/idea-registry
    ///   row — and on that day the deferred epoch becomes a real one.
    ROOM_FURNISHING = "room/furnishing/v1" => "which patterns a room draws";
    /// Stream label for which chambers a structure has (The Lintel).
    ///
    /// Deliberately NOT `room/furnishing/v1`, which already exists and is live:
    /// chamber existence and pattern selection churn independently, and 0073
    /// splits labels by blast radius before the first bump. Merging them would
    /// put a frequent bump inside a label whose blast radius includes every
    /// creature's committed thermal-drive history.
    ///
    /// This is the first furnishing-family stream anything actually DRAWS from —
    /// `selection` takes no seed.
    ROOM_CHAMBERS = "room/chambers/v1" => "which chambers a structure has";
    /// Stream label for WHERE the RECTILINEAR method puts things (The Blocking).
    ///
    /// Split from `room/furnishing` on purpose (decision 0073): furnishing
    /// governs WHAT a place has and has a large blast radius; layout governs
    /// WHERE and is expected to churn as legibility is tuned. **This label is
    /// CAUSAL** — Amendment 2 §1b.7 supersedes 0075's promise that a layout
    /// solve is free to retune, because promoted incidental relations gate
    /// outcomes. Bumping it is an epoch whose blast radius is future outcomes
    /// only; committed history survives.
    ///
    /// **One label PER METHOD**, because the unit of independent change is the
    /// algorithm: retuning the grower's flood order has nothing to do with the
    /// cut band here, and a shared label would make a grower tweak relocate
    /// every built place's floor plan too. 0073 fixes epoch granularity at
    /// declaration, so the split is made before either label has been bumped.
    ///
    /// **A change to something the two methods SHARE — `extent_for`, or what a
    /// `Lattice` means — bumps BOTH literals.** That obligation is the price of
    /// the flat form; there is no shared root segment to bump once.
    ROOM_LAYOUT_RECTILINEAR = "room/layout/v1/rectilinear"
        => "where the rectilinear method places chambers";
    /// Stream label for WHERE the REGION-GROWING method puts things.
    /// See [`ROOM_LAYOUT_RECTILINEAR`] for why this is a second label rather
    /// than a shared one, and for what bumps both.
    ROOM_LAYOUT_GROWN = "room/layout/v1/grown"
        => "where the growing method places chambers";
}
