/// `dynamic` is never flagged, literal argument or not, in any file.
pub fn make(name: &str) -> StreamLabel<'_> {
    StreamLabel::dynamic(name)
}
