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
    /// **LIVE as of The Threshold — read this before adding a pattern.** Under
    /// The Hearth this label versioned nothing that could move a world: no room
    /// derived an interior and every warmth read was `None`, so the inventory
    /// was free to grow. That is no longer true. A creature now stands at an
    /// anchor and its thermal drive reads the warmth there, so a larger
    /// inventory means a different composed interior, which means different
    /// warmth, which means different committed drive history. **Adding or
    /// reordering a pattern is an EPOCH, not a tweak** — worlds are reproducible
    /// within one and not across it (decision 0072). Note also that `selection`
    /// admits a pattern requiring another only once that other is present, so
    /// the inventory's ORDER is load-bearing even though the draw keys by name:
    /// inserting a pattern before its requirement silently drops it.
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
