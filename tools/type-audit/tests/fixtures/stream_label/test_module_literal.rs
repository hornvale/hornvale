/// Production code — no literal here, so this file's only violation risk
/// is from the test module below, which must be exempt.
pub fn real() -> StreamLabel<'static> {
    StreamLabel::from_static(LABEL)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parity_with_the_untyped_derive() {
        // A literal used purely to test API behavior, not a save-format
        // label — must NOT be flagged even though it's outside `streams.rs`.
        let _typed = StreamLabel::from_static("astronomy");
    }
}
