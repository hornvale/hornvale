//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

use hornvale_kernel::seed::StreamLabel;

/// The folk causal-schema-selection sub-leg, under a culture's own
/// `hornvale_language::streams::ROOT` derivation.
/// type-audit: bare-ok(identifier-text: return)
pub const SCHEMA: StreamLabel<'static> = StreamLabel::from_static("schema");
/// The sky-domain fact-shape sub-leg, under [`SCHEMA`].
/// type-audit: bare-ok(identifier-text: return)
pub const SKY: StreamLabel<'static> = StreamLabel::from_static("sky");
/// The lexicalization sub-leg for a chosen schema's rendered sentence.
/// type-audit: bare-ok(identifier-text: return)
pub const LEXEME: StreamLabel<'static> = StreamLabel::from_static("lexeme");
/// The doctrine (institutional) causal-schema-selection sub-leg — the
/// doctrine-voice twin of [`SCHEMA`].
/// type-audit: bare-ok(identifier-text: return)
pub const DOCTRINE_SCHEMA: StreamLabel<'static> = StreamLabel::from_static("doctrine-schema");
/// The doctrine-voice twin of [`LEXEME`].
/// type-audit: bare-ok(identifier-text: return)
pub const DOCTRINE_LEXEME: StreamLabel<'static> = StreamLabel::from_static("doctrine-lexeme");
/// The deity-naming stream, epoch v2 (a full flat path, not a composed
/// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
/// type-audit: bare-ok(identifier-text: return)
pub const RELIGION_DEITY_V2: StreamLabel<'static> = StreamLabel::from_static("religion/deity/v2");

/// Every seed-derivation label this crate itself owns (not any domain
/// crate's), for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SCHEMA.as_str(), "the folk causal-schema-selection sub-leg"),
        (SKY.as_str(), "the sky-domain fact-shape sub-leg"),
        (
            LEXEME.as_str(),
            "the lexicalization sub-leg for a chosen schema",
        ),
        (
            DOCTRINE_SCHEMA.as_str(),
            "the doctrine-voice twin of the folk schema-selection leg",
        ),
        (
            DOCTRINE_LEXEME.as_str(),
            "the doctrine-voice twin of the lexeme leg",
        ),
        (
            RELIGION_DEITY_V2.as_str(),
            "the deity-naming stream, epoch v2",
        ),
    ]
}
