//! Seed-derivation labels for `windows/chronicle` (the living-community
//! macro-history sounding). Save-format contracts; a rename silently
//! corrupts every world. Every entry here is a full flat path (this
//! crate never composes legs — matches `domains/climate`'s own
//! `WEATHER_PHASE` pattern), authored by PROC-17 (this crate had zero
//! centralization before).

use hornvale_kernel::seed::StreamLabel;

/// The species-generation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const SPECIES: StreamLabel<'static> = StreamLabel::from_static("chronicle/species");
/// The carrying-capacity draw.
/// type-audit: bare-ok(identifier-text: return)
pub const CAPACITY: StreamLabel<'static> = StreamLabel::from_static("chronicle/capacity");
/// The community-formation draw.
/// type-audit: bare-ok(identifier-text: return)
pub const COMMUNITIES: StreamLabel<'static> = StreamLabel::from_static("chronicle/communities");
/// The connection-graph draw.
/// type-audit: bare-ok(identifier-text: return)
pub const GRAPH: StreamLabel<'static> = StreamLabel::from_static("chronicle/graph");
/// The event-simulation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const EVENTS: StreamLabel<'static> = StreamLabel::from_static("chronicle/events");
/// The event-delivery/propagation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const DELIVER: StreamLabel<'static> = StreamLabel::from_static("chronicle/deliver");
/// The replay-measurement stream.
/// type-audit: bare-ok(identifier-text: return)
pub const REPLAY: StreamLabel<'static> = StreamLabel::from_static("chronicle/replay");

/// Every seed-derivation label this crate uses, for the generated stream
/// manifest (mirrors every other crate's own `stream_labels()`).
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SPECIES.as_str(), "the species-generation stream"),
        (CAPACITY.as_str(), "the carrying-capacity draw"),
        (COMMUNITIES.as_str(), "the community-formation draw"),
        (GRAPH.as_str(), "the connection-graph draw"),
        (EVENTS.as_str(), "the event-simulation stream"),
        (DELIVER.as_str(), "the event-delivery/propagation stream"),
        (REPLAY.as_str(), "the replay-measurement stream"),
    ]
}
