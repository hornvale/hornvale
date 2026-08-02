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
/// **Retired** by [`V3`] (The Wearing, 2026-07-27) but never deleted — an
/// epoch is a save-format contract, so a superseded leg stays declared.
/// type-audit: bare-ok(identifier-text: return)
pub const V2: StreamLabel<'static> = StreamLabel::from_static("v2");
/// The generic epoch-3 suffix leg, in the same position [`V2`] occupied
/// (settlement/deity/epithet all reuse this exact leg).
///
/// The Wearing (2026-07-27) retires the 2–3 syllable drawn settlement stem
/// and inserts toponymic wear between compounding and repair, both of which
/// change what `Namer::glossed_name` consumes from its stream. Deliberate
/// regeneration uses an epoch suffix, never a rename (the save-format
/// contract), so `v3` reseeds every glossed name and `v2`'s forms are gone
/// by design, regenerated with the world.
/// type-audit: bare-ok(identifier-text: return)
pub const V3: StreamLabel<'static> = StreamLabel::from_static("v3");
/// The lexicon sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const LEXICON: StreamLabel<'static> = StreamLabel::from_static("lexicon");
/// Compound-headedness draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const HEADEDNESS: StreamLabel<'static> = StreamLabel::from_static("headedness");
/// Sound-change cascade draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const CASCADE: StreamLabel<'static> = StreamLabel::from_static("cascade");
/// The epoch-2 suffix leg for the sound-change cascade, one level below
/// [`CASCADE`] and above [`WEAR`].
///
/// The Witness (2026-07-30) makes the cascade draw position-aware:
/// [`crate::etymology::RuleKind::Tonogenesis`] is no longer offered at a
/// position where no merger has been drawn, because `evolve` opens with no
/// pending conditioning and such a rule is provably the identity. Draw COUNT
/// is unchanged (`Stream::pick` is one draw at any slice length); the drawn
/// VALUES move, so every cascade in every world is reseeded. Deliberate
/// regeneration uses an epoch suffix, never a rename — the save-format
/// contract — so `v2` reseeds every cascade and v1's forms are gone by
/// design, regenerated with the world.
///
/// **Why this leg and not `name/settlement/v4` or `lexicon/root/v4`.** 0083
/// puts a label on the algorithm that changed, and the algorithm that changed
/// is `draw_rule`, which lives here. `Namer::glossed_name` consumes exactly
/// what it consumed before (`Namer::wear` takes no `&mut Stream` — it draws
/// nothing), so 0089's freeze on `name/settlement/v3` is not tripped. And
/// `ROOT_EPOCH` stays at `v3` on 0089's own precedent: a `v4` minted for
/// exactly this reason — an input to the assignment moving, not the
/// assignment itself — was withdrawn on 2026-07-29.
/// type-audit: bare-ok(identifier-text: return)
pub const CASCADE_V2: StreamLabel<'static> = StreamLabel::from_static("v2");
/// The toponymic-wear cascade draw, one leg below [`CASCADE`].
///
/// A leg of its own, and that is load-bearing rather than tidy. Drawing the
/// wear cascade from [`CASCADE`] itself yields a strict *prefix* of the
/// language's historical cascade, because the rule draws follow the count
/// draw and consume identically — and a lexicon's modern forms are exactly
/// that historical cascade's own output. Re-running its opening rules on
/// its own output finds no environment left to fire in: `ClusterSimplify`
/// has already removed the word-initial cluster it looks for, `FinalLoss`
/// the word-final consonant, and the codomain constraint blocks a second
/// `Lenition`/`Fortition`/`VowelShift` whose target is no longer in the
/// inventory. The result is not *provably* inert — a CCC onset lets
/// `ClusterSimplify` fire a second time, and a 200-seed × 5-species sweep
/// found 44 such counterexamples in 3600 forms — but it is degenerate to
/// the point of uselessness: on seed 42 it changed **154 of 154** wear
/// applications not at all, and the whole world came out byte-identical to
/// one with wear switched off. Off this leg the same sweep changes 906 of
/// 3600. The wear must draw rules the words have *not* already undergone.
/// type-audit: bare-ok(identifier-text: return)
pub const WEAR: StreamLabel<'static> = StreamLabel::from_static("wear");
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
