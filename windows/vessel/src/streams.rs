//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

/// Stream label for the minted agent's id draw.
/// type-audit: bare-ok(identifier-text)
pub const VESSEL_AGENT: &str = "vessel/agent";
/// Stream label for the walker battery's deterministic walk.
/// type-audit: bare-ok(identifier-text)
pub const VESSEL_WALK: &str = "vessel/walk";

/// Every vessel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (VESSEL_AGENT, "minted agent id draw"),
        (VESSEL_WALK, "walker-battery deterministic walk"),
    ]
}
