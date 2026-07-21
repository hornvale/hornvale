//! Seed-derivation labels for `windows/chronicle` (the living-community
//! macro-history sounding). Save-format contracts; a rename silently
//! corrupts every world. Every entry here is a full flat path (this
//! crate never composes legs — matches `domains/climate`'s own
//! `WEATHER_PHASE` pattern), authored by PROC-17 (this crate had zero
//! centralization before) and declared via `stream_labels!` (PROC-18).

hornvale_kernel::stream_labels! {
    /// The species-generation stream.
    SPECIES = "chronicle/species" => "the species-generation stream";
    /// The carrying-capacity draw.
    CAPACITY = "chronicle/capacity" => "the carrying-capacity draw";
    /// The community-formation draw.
    COMMUNITIES = "chronicle/communities" => "the community-formation draw";
    /// The connection-graph draw.
    GRAPH = "chronicle/graph" => "the connection-graph draw";
    /// The event-simulation stream.
    EVENTS = "chronicle/events" => "the event-simulation stream";
    /// The event-delivery/propagation stream.
    DELIVER = "chronicle/deliver" => "the event-delivery/propagation stream";
    /// The replay-measurement stream.
    REPLAY = "chronicle/replay" => "the replay-measurement stream";
}
