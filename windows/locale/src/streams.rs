//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's regime.

hornvale_kernel::stream_labels! {
    /// Stream label for a room's sub-cell micro-field.
    LOCALE_MICRO = "locale/regime/micro" => "room sub-cell micro-field";
    /// Stream label for a room's descriptor variety draw.
    LOCALE_VARIETY = "locale/regime/variety" => "room descriptor variety draw";
    /// Stream label for a room's substrate-detail draw.
    LOCALE_SUBSTRATE_DETAIL = "locale/regime/substrate" => "room substrate-detail draw";
    /// Stream label for the world's rarity-budget exotic placement pass.
    LOCALE_PLACE = "locale/strangeness/place" => "world rarity-budget placement pass";
}
