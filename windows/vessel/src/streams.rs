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
}
