/// Not named `streams.rs` — an inline literal here must be flagged.
pub fn make() -> StreamLabel<'static> {
    StreamLabel::from_static("room/face")
}
