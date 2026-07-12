//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's texture.

/// Stream label for a room's aspect draw.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_ASPECT: &str = "locale/aspect";
/// Stream label for a room's relief jitter.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_JITTER: &str = "locale/jitter";

/// Every locale seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (LOCALE_ASPECT, "room aspect draw"),
        (LOCALE_JITTER, "room relief jitter"),
    ]
}
