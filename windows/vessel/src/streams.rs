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
    ROOM_FURNISHING = "room/furnishing/v1" => "which patterns a room draws";
}
