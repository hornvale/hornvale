/// Identical body to `inline_literal.rs`, but this file is named
/// `streams.rs` — the one exempt filename — so the same literal must NOT
/// be flagged.
pub fn make() -> StreamLabel<'static> {
    StreamLabel::from_static("room/face")
}
