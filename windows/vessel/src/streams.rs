//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

hornvale_kernel::stream_labels! {
    /// Stream label for the minted agent's id draw.
    VESSEL_AGENT = "vessel/agent" => "minted agent id draw";
    /// Stream label for the walker battery's deterministic walk.
    VESSEL_WALK = "vessel/walk" => "walker-battery deterministic walk";
}
