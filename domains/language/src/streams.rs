//! Seed-derivation labels for the language domain (save-format contract
//! — a rename silently corrupts every world; deliberate regeneration
//! uses an epoch suffix, e.g. `.../v2`). Authored by PROC-17 — this
//! crate previously had no centralized leg constants at all; every one
//! of these already existed as an inline literal, repeated identically
//! at every call site, before this file existed.

use hornvale_kernel::seed::StreamLabel;

/// Root label for the language domain.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("language");
/// The phonology sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const PHONOLOGY: StreamLabel<'static> = StreamLabel::from_static("phonology");
/// The tone-inventory draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const TONES: StreamLabel<'static> = StreamLabel::from_static("tones");
/// The phoneme-inventory draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const INVENTORY: StreamLabel<'static> = StreamLabel::from_static("inventory");
/// The syllable-phonotactics draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const PHONOTACTICS: StreamLabel<'static> = StreamLabel::from_static("phonotactics");
/// The grammar sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const GRAMMAR: StreamLabel<'static> = StreamLabel::from_static("grammar");
/// Constituent-order draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const CONSTITUENT_ORDER: StreamLabel<'static> = StreamLabel::from_static("constituent-order");
/// Copula-presence/form draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const COPULA: StreamLabel<'static> = StreamLabel::from_static("copula");
/// Article-presence draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const ARTICLES: StreamLabel<'static> = StreamLabel::from_static("articles");
/// The numeracy-rung draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMERACY_RUNG: StreamLabel<'static> = StreamLabel::from_static("numeracy-rung");
/// The name sub-tree (settlement/deity/epithet name generation).
/// type-audit: bare-ok(identifier-text: return)
pub const NAME: StreamLabel<'static> = StreamLabel::from_static("name");
/// The generic epoch-2 suffix leg, appended one level deeper than a v1
/// name draw (settlement/deity/epithet all reuse this exact leg).
/// type-audit: bare-ok(identifier-text: return)
pub const V2: StreamLabel<'static> = StreamLabel::from_static("v2");
/// The lexicon sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const LEXICON: StreamLabel<'static> = StreamLabel::from_static("lexicon");
/// Compound-headedness draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const HEADEDNESS: StreamLabel<'static> = StreamLabel::from_static("headedness");
/// Sound-change cascade draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const CASCADE: StreamLabel<'static> = StreamLabel::from_static("cascade");
/// The proto-root draw leg, under lexicon (named `PROTO_ROOT`, not
/// `ROOT`, to avoid colliding with this file's own crate-root constant —
/// the literal value is `"root"`, distinct from `ROOT`'s `"language"`).
/// type-audit: bare-ok(identifier-text: return)
pub const PROTO_ROOT: StreamLabel<'static> = StreamLabel::from_static("root");
/// The probe sub-stream for open-addressing re-draws during merger-aware
/// proto-root assignment.
/// type-audit: bare-ok(identifier-text: return)
pub const PROBE: StreamLabel<'static> = StreamLabel::from_static("probe");
/// The static leg-name literal `"family"`, used by morphology's own
/// family-level derivation (distinct from a `family` VARIABLE holding a
/// dynamic family identifier elsewhere in this crate — that usage wraps
/// via `StreamLabel::dynamic`, never this constant).
/// type-audit: bare-ok(identifier-text: return)
pub const FAMILY_LEG: StreamLabel<'static> = StreamLabel::from_static("family");
/// The morphology sub-leg, under the family-level derivation.
/// type-audit: bare-ok(identifier-text: return)
pub const MORPH: StreamLabel<'static> = StreamLabel::from_static("morph");
/// Grammaticalization-depth draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const DEPTH: StreamLabel<'static> = StreamLabel::from_static("depth");
/// Evidentiality depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const EVIDENTIAL: StreamLabel<'static> = StreamLabel::from_static("evidential");
/// Noun-class depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const NOUN_CLASS: StreamLabel<'static> = StreamLabel::from_static("noun-class");
/// Noun-class marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const CLASS_POSITION: StreamLabel<'static> = StreamLabel::from_static("class-position");
/// Number-marking depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMBER: StreamLabel<'static> = StreamLabel::from_static("number");
/// Tense-marking depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const TENSE: StreamLabel<'static> = StreamLabel::from_static("tense");
/// Number-marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMBER_POSITION: StreamLabel<'static> = StreamLabel::from_static("number-position");
/// Tense-marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const TENSE_POSITION: StreamLabel<'static> = StreamLabel::from_static("tense-position");
