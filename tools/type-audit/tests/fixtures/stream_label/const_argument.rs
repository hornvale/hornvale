/// A named-constant argument (not a literal) is never flagged, even
/// outside `streams.rs`.
const LABEL_TEXT: &str = "room/face";

/// Uses the constant above, not an inline literal.
pub fn make() -> StreamLabel<'static> {
    StreamLabel::from_static(LABEL_TEXT)
}
