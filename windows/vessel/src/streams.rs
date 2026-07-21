//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

use hornvale_kernel::seed::StreamLabel;

/// Stream label for the minted agent's id draw.
/// type-audit: bare-ok(identifier-text: return)
pub const VESSEL_AGENT: StreamLabel<'static> = StreamLabel::from_static("vessel/agent");
/// Stream label for the walker battery's deterministic walk.
/// type-audit: bare-ok(identifier-text: return)
pub const VESSEL_WALK: StreamLabel<'static> = StreamLabel::from_static("vessel/walk");

/// Every vessel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (VESSEL_AGENT.as_str(), "minted agent id draw"),
        (VESSEL_WALK.as_str(), "walker-battery deterministic walk"),
    ]
}
