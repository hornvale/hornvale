//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's regime.

use hornvale_kernel::seed::StreamLabel;

/// Stream label for a room's sub-cell micro-field.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_MICRO: StreamLabel<'static> = StreamLabel::from_static("locale/regime/micro");
/// Stream label for a room's descriptor variety draw.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_VARIETY: StreamLabel<'static> = StreamLabel::from_static("locale/regime/variety");
/// Stream label for a room's substrate-detail draw.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_SUBSTRATE_DETAIL: StreamLabel<'static> =
    StreamLabel::from_static("locale/regime/substrate");
/// Stream label for the world's rarity-budget exotic placement pass.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_PLACE: StreamLabel<'static> = StreamLabel::from_static("locale/strangeness/place");

/// Every locale seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (LOCALE_MICRO.as_str(), "room sub-cell micro-field"),
        (LOCALE_VARIETY.as_str(), "room descriptor variety draw"),
        (
            LOCALE_SUBSTRATE_DETAIL.as_str(),
            "room substrate-detail draw",
        ),
        (LOCALE_PLACE.as_str(), "world rarity-budget placement pass"),
    ]
}
