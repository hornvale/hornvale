//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's texture.

/// Stream label for a room's aspect draw.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_ASPECT: &str = "locale/aspect";
/// Stream label for a room's relief jitter.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_JITTER: &str = "locale/jitter";
/// Stream label for a room's sub-cell micro-field.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_MICRO: &str = "locale/regime/micro";
/// Stream label for a room's descriptor variety draw.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_VARIETY: &str = "locale/regime/variety";
/// Stream label for a room's substrate-detail draw.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_SUBSTRATE_DETAIL: &str = "locale/regime/substrate";
/// Stream label for the world's rarity-budget exotic placement pass.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_PLACE: &str = "locale/strangeness/place";

/// Every locale seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (LOCALE_ASPECT, "room aspect draw"),
        (LOCALE_JITTER, "room relief jitter"),
        (LOCALE_MICRO, "room sub-cell micro-field"),
        (LOCALE_VARIETY, "room descriptor variety draw"),
        (LOCALE_SUBSTRATE_DETAIL, "room substrate-detail draw"),
        (LOCALE_PLACE, "world rarity-budget placement pass"),
    ]
}
