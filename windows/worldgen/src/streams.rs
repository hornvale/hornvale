//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

hornvale_kernel::stream_labels! {
    /// The folk causal-schema-selection sub-leg, under a culture's own
    /// `hornvale_language::streams::ROOT` derivation.
    SCHEMA = "schema" => "the folk causal-schema-selection sub-leg";
    /// The sky-domain fact-shape sub-leg, under `SCHEMA`.
    SKY = "sky" => "the sky-domain fact-shape sub-leg";
    /// The lexicalization sub-leg for a chosen schema's rendered sentence.
    LEXEME = "lexeme" => "the lexicalization sub-leg for a chosen schema";
    /// The doctrine (institutional) causal-schema-selection sub-leg — the
    /// doctrine-voice twin of `SCHEMA`.
    DOCTRINE_SCHEMA = "doctrine-schema" => "the doctrine-voice twin of the folk schema-selection leg";
    /// The doctrine-voice twin of `LEXEME`.
    DOCTRINE_LEXEME = "doctrine-lexeme" => "the doctrine-voice twin of the lexeme leg";
    /// The deity-naming stream, epoch v2 (a full flat path, not a composed
    /// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
    RELIGION_DEITY_V2 = "religion/deity/v2" => "the deity-naming stream, epoch v2";
}
