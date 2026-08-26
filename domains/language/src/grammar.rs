//! A tongue's drawn surface grammar: constituent order, copula presence
//! (and, when present, its drawn form), and article presence. This is the
//! floor slice of LANG-40's grammaticalization-depth vector — C7 (the
//! morphology campaign) extends [`TongueGrammar`], never replaces it.
//!
//! Word order is historically **contingent**, not derivable from a
//! species' psychology or subsistence pattern (spec §3): deriving it from
//! existing culture vectors would be astrology shipped as science, so
//! these parameters are DRAWN from three new permanent stream labels
//! (`language/<species>/grammar/constituent-order`,
//! `language/<species>/grammar/copula`, `language/<species>/grammar/articles`)
//! — build-state (decision 0058): drawn at composition/render time, never
//! serialized, so adding them is byte-identical to every existing world.
//!
//! The copula's overt form is never authored text: when a tongue draws a
//! copula, its one-syllable form is filled from the tongue's own
//! [`Phonology`] by the same syllable-fill mechanism [`crate::etymology::proto_root`]
//! and [`crate::naming::Namer`] use for every other generated word — zero
//! authored surface text anywhere in a generated tongue (the program
//! thesis).

use crate::clause::{Adjunct, Argument, Clause, Subject};
use crate::lexicon::{LexEntry, Lexicon};
use crate::morphology::{
    ClassPosition, Evidential, MorphDepth, MorphForm, NounClass, TongueMorphology, affix,
};
use crate::naming::{Namer, render_views_with, segments_of};
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use crate::streams;
use crate::typology::Orthography;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::world::IS_A;
use hornvale_kernel::{Seed, Stream};

/// The six constituent orders of a subject–copula–complement clause
/// (a nominal-predication clause: "The Vavako are goblins").
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstituentOrder {
    /// Subject–Object(complement)–Verb(copula).
    Sov,
    /// Subject–Verb–Object.
    Svo,
    /// Verb–Subject–Object.
    Vso,
    /// Verb–Object–Subject.
    Vos,
    /// Object–Verb–Subject.
    Ovs,
    /// Object–Subject–Verb.
    Osv,
}

/// A tongue's drawn surface grammar — the floor slice of the
/// grammaticalization-depth vector (LANG-40): C7 extends this struct,
/// never replaces it. Word order is historically contingent, so these are
/// DRAWN, never derived from culture vectors (spec §3). Build-state
/// (decision 0058): drawn at composition time, never serialized.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueGrammar {
    /// Constituent order for predication clauses.
    pub order: ConstituentOrder,
    /// The overt copula's roman form for copula-bearing tongues — drawn
    /// from the tongue's own phonology, never authored — or `None` for a
    /// zero-copula tongue.
    pub copula: Option<String>,
    /// The overt copula's own segments, retained alongside its roman form
    /// (C7) so [`realize_tongue_deep`] can affix the evidential marker onto
    /// it at the SEGMENT level (never string concatenation) when the tongue
    /// draws `Affix`-depth evidential marking. Populated by
    /// [`tongue_grammar`] exactly when `copula` is populated; a hand-built
    /// grammar used only to test the C3 roman-level surface may leave this
    /// `None` even with `copula: Some(..)` — [`realize_tongue_deep`] then
    /// PANICS if asked to Affix-mark it (`layer_affix`'s loud arm; the T1
    /// review removed the silent roman-level fallback).
    pub copula_segments: Option<Vec<Segment>>,
    /// Whether the language has articles. The floor realizer renders no
    /// article surface (no article lexeme exists yet — C7's morphology
    /// campaign gives this parameter its surface); it is drawn now because
    /// the stream label is a permanent save-format contract and
    /// article-hood is a fact of the language, not of its current
    /// renderer.
    pub articles: bool,
}

/// The `range_u32(1, 100)` roll boundaries for [`ConstituentOrder`]
/// (authored typology, approximate WALS frequencies): SOV 45%, SVO 42%,
/// VSO 9%, VOS 2%, OVS 1%, OSV 1%.
fn order_from_roll(roll: u32) -> ConstituentOrder {
    match roll {
        1..=45 => ConstituentOrder::Sov,
        46..=87 => ConstituentOrder::Svo,
        88..=96 => ConstituentOrder::Vso,
        97..=98 => ConstituentOrder::Vos,
        99 => ConstituentOrder::Ovs,
        _ => ConstituentOrder::Osv,
    }
}

/// Draw the overt copula's one-syllable roman form from `namer`'s
/// phonology, consuming `stream` — the same stream the presence roll
/// already drew from, so presence and form share the one permanent
/// `.../grammar/copula` stream. Uses the exact syllable-fill mechanism
/// [`crate::etymology::proto_root`] uses for proto-roots: one template
/// syllable via [`Namer::draw_syllables`], flattened via [`segments_of`]
/// and rendered via [`render_views_with`] under `orth` (the tongue's own
/// [`Phonology::orthography`]) — the same reduction every lexicon word and
/// generated name goes through, so a drawn copula is indistinguishable in
/// kind from any other word in the tongue.
fn draw_copula_form(
    stream: &mut Stream,
    namer: &Namer,
    orth: Orthography,
) -> (Vec<Segment>, String) {
    let syllables = namer.draw_syllables(stream, 1, 1, false);
    let segments = segments_of(&syllables);
    let roman = render_views_with(&segments, orth).roman;
    (segments, roman)
}

/// Draw `species`' tongue grammar from the three permanent grammar streams
/// (`language/<species>/grammar/…`): constituent order, copula presence
/// (and drawn form), and article presence.
/// type-audit: bare-ok(identifier-text)
pub fn tongue_grammar(seed: &Seed, species: &str, ph: &Phonology) -> TongueGrammar {
    let namer = Namer::new(seed, species, ph);

    let mut order_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::CONSTITUENT_ORDER)
        .stream();
    let order = order_from_roll(order_stream.range_u32(1, 100));

    let mut copula_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::COPULA)
        .stream();
    let (copula_segments, copula) = if copula_stream.range_u32(1, 100) <= 60 {
        let (segments, roman) = draw_copula_form(&mut copula_stream, &namer, ph.orthography);
        (Some(segments), Some(roman))
    } else {
        (None, None)
    };

    let mut articles_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::ARTICLES)
        .stream();
    let articles = articles_stream.range_u32(1, 100) <= 30;

    TongueGrammar {
        order,
        copula,
        copula_segments,
        articles,
    }
}

/// A whole-sentence gap: the tongue could not say this clause because its
/// complement concept has no word (spec §4 — a clause renders fully or gaps
/// entirely, never partially).
/// type-audit: bare-ok(identifier-text), bare-ok(prose)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueGap {
    /// The concept that failed to lexicalize.
    pub concept: String,
    /// The recountable reason (from the lexicon's own gap, or "no entry"
    /// when the concept has no entry at all).
    pub reason: String,
}

/// Resolve one argument to its surface text in this tongue: a concept
/// through the lexicon, everything else passed through or rendered at the
/// tongue's own grain.
///
/// **Shared by the object slot and the adjunct slot**, and that sharing is
/// the point. Before The Scarf the adjunct slot resolved all four shapes
/// and the object slot resolved only `Concept`, so a tongue could say a
/// bare numeral in an adjunct and not in the object — an accident of two
/// clause structs drifting, never a design.
fn resolve_argument(argument: &Argument, lexicon: &Lexicon) -> Result<String, TongueGap> {
    match argument {
        Argument::Concept(id) => Ok(resolve_concept_marked(id, lexicon)?.roman),
        Argument::Name(text) => Ok(text.clone()),
        Argument::Count(n) => Ok(n.to_string()),
        Argument::Quantity(x) => Ok(x.to_string()),
    }
}

/// Resolve a concept id to a word mid-assembly, keeping its segments when
/// the lexicon has them so a later affix layer can join at the segment
/// level. A [`LexEntry::Compound`] yields `segments: None` — a pre-existing
/// lexicon gap, which [`layer_affix`] PANICS on rather than silently
/// degrading.
fn resolve_concept_marked(id: &str, lexicon: &Lexicon) -> Result<Marked, TongueGap> {
    match lexicon.entry(id) {
        Some(LexEntry::Root { derivation, views }) => Ok(Marked {
            segments: Some(derivation.modern.clone()),
            roman: views.roman.clone(),
        }),
        Some(LexEntry::Compound { views, .. }) => Ok(Marked {
            segments: None,
            roman: views.roman.clone(),
        }),
        // `GapReason`'s Display is the canonical recountable rendering —
        // never `{reason:?}`; the reason is prose to recount, not debug.
        Some(LexEntry::Gap { reason }) => Err(TongueGap {
            concept: id.to_string(),
            reason: reason.to_string(),
        }),
        None => Err(TongueGap {
            concept: id.to_string(),
            reason: "no entry in this lexicon".to_string(),
        }),
    }
}

/// Realize a clause's adjuncts through the tongue's own lexicon, shared by
/// both [`realize_tongue`] and [`realize_tongue_deep`] so the two agree on
/// adjunct handling exactly (the shallow-identity guarantee needs this: with
/// no adjuncts the two functions must already produce identical text, and
/// with adjuncts present they still must, at `MorphDepth::None`).
///
/// An [`Argument::Concept`] resolves via `lexicon`, with the same
/// `LexEntry` matching (and the same recountable-gap surfacing) the
/// complement itself uses — **the whole clause gaps** on the first unknown
/// adjunct concept, never a partial render (spec §4). The other `Argument`
/// variants pass through directly: a tongue's own numeral/name system is
/// out of this task's scope, so `Count`/`Quantity` render as bare digits and
/// `Name` passes through unresolved, exactly as `realize_common` does for
/// the complement slot.
fn realize_adjuncts(adjuncts: &[Adjunct], lexicon: &Lexicon) -> Result<Vec<String>, TongueGap> {
    adjuncts
        .iter()
        .map(|adjunct| resolve_argument(&adjunct.argument, lexicon))
        .collect()
}

/// The subject's surface text in a tongue.
///
/// A [`Subject::Pronoun`] GAPS: no tongue draws a pronoun inventory, so
/// there is nothing to realize. Before The Scarf the projection into
/// `TongueClause` stringified Common's own English pronoun and handed it
/// to a tongue, which is precisely the leak The Interlinear existed to
/// close. The reason names the inventory rather than the people: a tongue
/// with no drawn pronouns cannot RE-MENTION; its speakers are not thereby
/// unable to refer.
fn tongue_subject(subject: &Subject) -> Result<String, TongueGap> {
    match subject {
        Subject::Name(name) => Ok(name.clone()),
        Subject::Pronoun(_) => Err(TongueGap {
            concept: "subject/pronoun".to_string(),
            reason: "this tongue draws no pronoun inventory, so it cannot re-mention a referent"
                .to_string(),
        }),
    }
}

/// Refuse a clause whose predicate no tongue construction covers.
///
/// **Panics rather than gapping**, and the difference is the whole of spec
/// §3.3. A [`TongueGap`] asserts something TRUE ABOUT THE WORLD -- this
/// people has no word for the sea. A missing construction is an authoring
/// hole in this repository, so reporting it as a gap would put a false claim
/// about a people into a rendered artifact. `realize_common` panics on the
/// same condition for the same reason.
fn assert_tongue_construction(predicate: &str) {
    assert!(
        predicate == IS_A,
        "no tongue construction for predicate {predicate:?} (only {IS_A:?} is covered)"
    );
}

/// Realize a nominal-predication clause in a tongue: lexicalize the
/// complement, order the constituents per the grammar, include the copula
/// if the tongue bears one, then append each adjunct's own resolved word.
/// Renders fully or gaps entirely (spec §4) — a gap on the complement OR on
/// any adjunct concept fails the whole clause.
/// type-audit: bare-ok(prose)
pub fn realize_tongue(
    clause: &Clause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
) -> Result<String, TongueGap> {
    assert_tongue_construction(&clause.predicate);
    let subject = tongue_subject(&clause.subject)?;
    let complement = resolve_argument(&clause.object, lexicon)?;
    let adjunct_words = realize_adjuncts(&clause.adjuncts, lexicon)?;
    let s = subject.as_str();
    let v = grammar.copula.as_deref();
    let o = complement.as_str();
    // Order the present constituents; a `None` copula simply drops out.
    let ordered: Vec<&str> = match grammar.order {
        ConstituentOrder::Sov => [Some(s), Some(o), v],
        ConstituentOrder::Svo => [Some(s), v, Some(o)],
        ConstituentOrder::Vso => [v, Some(s), Some(o)],
        ConstituentOrder::Vos => [v, Some(o), Some(s)],
        ConstituentOrder::Ovs => [Some(o), v, Some(s)],
        ConstituentOrder::Osv => [Some(o), Some(s), v],
    }
    .into_iter()
    .flatten()
    .collect();
    let mut out = ordered.join(" ");
    for word in &adjunct_words {
        out.push(' ');
        out.push_str(word);
    }
    out.push('.');
    Ok(out)
}

/// A word mid-assembly: its segments when known (so a further affix layer
/// can join at the segment level via [`layer_affix`]) and its current roman
/// form. `segments` is `None` only for a [`LexEntry::Compound`] complement —
/// the lexicon does not retain a compound's joined segments today (a
/// pre-existing gap in `lexicon.rs`, out of this task's scope) — in which
/// case `layer_affix` PANICS rather than silently degrading (the loud
/// arm; close the lexicon gap before Affix-marking a Compound).
struct Marked {
    /// The word's segments, when known.
    segments: Option<Vec<Segment>>,
    /// The word's current roman form.
    roman: String,
}

/// Join one more affix layer onto `current`: a genuine segment-level
/// [`affix`] join when `current`'s segments are known. A segment-less word
/// (a [`LexEntry::Compound`] complement) PANICS — the loud arm the T1
/// review demanded; author the lexicon's compound-segment retention before
/// Affix-marking a Compound. `orth` is the tongue's own
/// [`crate::phonology::Phonology::orthography`] (spec §3.6), passed down
/// from [`realize_tongue_deep`].
fn layer_affix(
    current: Marked,
    marker: &MorphForm,
    position: ClassPosition,
    orth: Orthography,
) -> Marked {
    match &current.segments {
        Some(segments) => {
            let joined = affix(segments, &marker.segments, position, orth);
            Marked {
                segments: Some(joined.segments),
                roman: joined.roman,
            }
        }
        None => panic!(
            "layer_affix: cannot segment-affix onto a word with unknown segments \
             (a LexEntry::Compound complement — lexicon.rs does not retain a \
             compound's joined segments today; T1 review made this arm loud). \
             Close that gap before Affix-marking a Compound complement."
        ),
    }
}

/// A token's grammatical role, tracked through assembly so a `Particle`
/// insertion can find "the complement" or "the predicate" (the copula, or
/// the complement itself for a zero-copula tongue) regardless of the
/// tongue's drawn constituent order. `Marker` tags a spliced-in particle so
/// it is never mistaken for one of the three base roles by a later splice.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Role {
    /// The clause's subject.
    Subject,
    /// The overt copula, when present.
    Copula,
    /// The lexicalized complement.
    Complement,
    /// A spliced-in particle marker (evidential or noun-class).
    Marker,
}

/// Realize a nominal-predication clause with C7's full morphology bundle:
/// evidential and noun-class marking, at whatever depth `morph` draws
/// (`None`/`Particle`/`Affix`), layered on top of [`realize_tongue`]'s floor
/// assembly (constituent order, copula). `None` depth on both axes
/// reproduces [`realize_tongue`]'s C3 surface exactly (the shallow-identity
/// guarantee) — `realize_tongue` itself is untouched by this function's
/// addition, so every existing caller keeps compiling and behaving
/// identically.
///
/// Evidential marking is predicate-final: `Affix` suffixes the marker onto
/// the overt copula (segment-level, via [`layer_affix`]), or — for a
/// zero-copula tongue — encliticizes it onto the predicate nominal (the
/// complement, possibly already noun-class-marked); `Particle` places the
/// marker as a free word immediately after whichever of those served as
/// "the predicate". Noun-class marking always targets the complement noun:
/// `Affix` joins the marker onto it per `morph.class_position`; `Particle`
/// places the marker as a free word on that same side of the noun.
///
/// `orth` is the tongue's own [`crate::phonology::Phonology::orthography`]
/// (spec §3.6): a VIEW, so it changes only the `Affix`-depth marker joins'
/// spelling, never `morph`/`grammar`/`lexicon` or which draw fired.
/// type-audit: bare-ok(prose)
pub fn realize_tongue_deep(
    clause: &Clause,
    grammar: &TongueGrammar,
    morph: &TongueMorphology,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &Lexicon,
    orth: Orthography,
) -> Result<String, TongueGap> {
    assert_tongue_construction(&clause.predicate);
    let subject = tongue_subject(&clause.subject)?;
    // Spec §4.3: only a LEXICAL object may bear morphology. `object_concept`
    // is `Some` only for a `Concept` -- and it must not be conflated with
    // `Marked.segments == None`, which a `Compound` also has. A `Compound`'s
    // missing segments are a lexicon BUG that `layer_affix` panics on, on
    // purpose; a numeral has no segments BY NATURE and there is nothing to
    // fix. Guarding on the concept id keeps the panic reachable for the
    // first case while declining to affix in the second.
    let (mut complement, object_concept): (Marked, Option<String>) = match &clause.object {
        Argument::Concept(id) => (resolve_concept_marked(id, lexicon)?, Some(id.clone())),
        other => (
            Marked {
                segments: None,
                roman: resolve_argument(other, lexicon)?,
            },
            None,
        ),
    };
    let adjunct_words = realize_adjuncts(&clause.adjuncts, lexicon)?;

    // Noun-class marking: always on the complement noun — but only a lexical
    // object has a concept id to ask `noun_class_of` about (spec §4.3).
    let mut class_particle: Option<String> = None;
    if let Some(concept) = &object_concept {
        let class_value = match noun_class_of(concept) {
            NounClass::Animate => "animate",
            NounClass::Inanimate => "inanimate",
        };
        if let Some(marker) = morph.class.get(class_value) {
            match morph.noun_class_depth {
                MorphDepth::None => {}
                MorphDepth::Affix => {
                    complement = layer_affix(complement, marker, morph.class_position, orth);
                }
                MorphDepth::Particle => class_particle = Some(marker.roman.clone()),
            }
        }
    }

    // Evidential marking: predicate-final — the overt copula, or (zero
    // copula) the predicate nominal, i.e. the (possibly already
    // class-marked) complement.
    let evidential_value = match clause.evidential {
        Evidential::Witnessed => "witnessed",
        Evidential::Taught => "taught",
        Evidential::Inferred => "inferred",
    };
    let mut copula_roman = grammar.copula.clone();
    let mut evidential_particle: Option<String> = None;
    if let Some(marker) = morph.evidential.get(evidential_value) {
        match morph.evidential_depth {
            MorphDepth::None => {}
            MorphDepth::Affix => {
                if let Some(copula) = &grammar.copula {
                    let cop = Marked {
                        segments: grammar.copula_segments.clone(),
                        roman: copula.clone(),
                    };
                    copula_roman =
                        Some(layer_affix(cop, marker, ClassPosition::Suffix, orth).roman);
                } else if object_concept.is_some() {
                    // Zero copula: the marker falls to the predicate nominal.
                    // A non-lexical object cannot bear it, so the clause goes
                    // unmarked for evidentiality -- the same outcome a tongue
                    // that drew `MorphDepth::None` already has (spec §4.3).
                    complement = layer_affix(complement, marker, ClassPosition::Suffix, orth);
                }
            }
            MorphDepth::Particle => evidential_particle = Some(marker.roman.clone()),
        }
    }

    let s = subject.as_str();
    let v = copula_roman.as_deref();
    let o = complement.roman.as_str();
    let mut ordered: Vec<(Role, String)> = match grammar.order {
        ConstituentOrder::Sov => [
            Some((Role::Subject, s.to_string())),
            Some((Role::Complement, o.to_string())),
            v.map(|v| (Role::Copula, v.to_string())),
        ],
        ConstituentOrder::Svo => [
            Some((Role::Subject, s.to_string())),
            v.map(|v| (Role::Copula, v.to_string())),
            Some((Role::Complement, o.to_string())),
        ],
        ConstituentOrder::Vso => [
            v.map(|v| (Role::Copula, v.to_string())),
            Some((Role::Subject, s.to_string())),
            Some((Role::Complement, o.to_string())),
        ],
        ConstituentOrder::Vos => [
            v.map(|v| (Role::Copula, v.to_string())),
            Some((Role::Complement, o.to_string())),
            Some((Role::Subject, s.to_string())),
        ],
        ConstituentOrder::Ovs => [
            Some((Role::Complement, o.to_string())),
            v.map(|v| (Role::Copula, v.to_string())),
            Some((Role::Subject, s.to_string())),
        ],
        ConstituentOrder::Osv => [
            Some((Role::Complement, o.to_string())),
            Some((Role::Subject, s.to_string())),
            v.map(|v| (Role::Copula, v.to_string())),
        ],
    }
    .into_iter()
    .flatten()
    .collect();

    // Splice in the noun-class particle, adjacent to the complement per the
    // drawn class position (before it for Prefix, after for Suffix).
    if let Some(particle) = class_particle
        && let Some(idx) = ordered.iter().position(|(r, _)| *r == Role::Complement)
    {
        let insert_at = match morph.class_position {
            ClassPosition::Prefix => idx,
            ClassPosition::Suffix => idx + 1,
        };
        ordered.insert(insert_at, (Role::Marker, particle));
    }

    // Splice in the evidential particle immediately after "the predicate":
    // the copula token if the tongue is copula-bearing, else the complement
    // (the predicate nominal in a zero-copula clause). Tagging spliced
    // tokens `Role::Marker` above means this search still finds the right
    // anchor even after the class-particle splice shifted later indices.
    if let Some(particle) = evidential_particle {
        let predicate_role = if grammar.copula.is_some() {
            Role::Copula
        } else {
            Role::Complement
        };
        if let Some(idx) = ordered.iter().position(|(r, _)| *r == predicate_role) {
            ordered.insert(idx + 1, (Role::Marker, particle));
        }
    }

    let mut sentence = ordered
        .into_iter()
        .map(|(_, token)| token)
        .collect::<Vec<_>>()
        .join(" ");
    for word in &adjunct_words {
        sentence.push(' ');
        sentence.push_str(word);
    }
    sentence.push('.');
    Ok(sentence)
}

#[cfg(test)]
mod tests {
    use super::*;
    // Test-only: `Number` and `Definiteness` reach a tongue realizer and go
    // UNREAD (spec 3.2), so no non-test code in this module names them.
    // Importing them at module level would be an unused import outside
    // `cfg(test)`.
    use crate::clause::{Definiteness, Number};
    use crate::etymology::CascadeRegime;
    use crate::lexicon::{ExposureClass, LexEntry, build_lexicon};
    use crate::naming::render_views;
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use std::collections::BTreeMap;

    /// The manikin's articulation envelope — per `phonology.rs`'s own
    /// test-constructor pattern (`manikin_env`), reconstructed locally here
    /// since that helper is private to `phonology`'s own test module.
    fn test_envelope() -> Envelope {
        Envelope {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        }
    }

    /// A small phonology to draw grammars against — species-agnostic; the
    /// tests below vary the `species` argument to `tongue_grammar` itself,
    /// not this shared phonology.
    fn test_phonology() -> Phonology {
        draw_phonology(
            &Seed(1),
            "test-tongue",
            &test_envelope(),
            &crate::typology::concatenative(),
        )
    }

    /// claim: structural(seed: 42) — determinism/species-keying at one fixed
    /// seed, with an embedded reachability check (any(1..=20)) confirming the
    /// draw is not a constant function
    #[test]
    fn tongue_grammar_is_deterministic_and_species_keyed() {
        let ph = test_phonology();
        let seed = Seed(42);
        let a = tongue_grammar(&seed, "goblin", &ph);
        let b = tongue_grammar(&seed, "goblin", &ph);
        assert_eq!(a, b, "same seed+species → same grammar");
        let kobold = tongue_grammar(&seed, "kobold", &ph);
        let hobgoblin = tongue_grammar(&seed, "hobgoblin", &ph);
        // Not all three species may differ on every field, but the draw
        // must be species-keyed: assert the tuple of all three grammars
        // is not identical across seeds 1..=20 (a constant function would
        // fail this).
        let differs = (1..=20).any(|s| {
            let seed = Seed(s);
            let g = tongue_grammar(&seed, "goblin", &ph);
            let k = tongue_grammar(&seed, "kobold", &ph);
            g != k
        });
        assert!(differs, "grammar draws are species-keyed, not constant");
        let _ = (kobold, hobgoblin);
    }

    /// claim: rate(forall-seed, sov_svo > 120 / 200) — typology-weight check
    #[test]
    fn constituent_order_weights_favor_sov_and_svo() {
        // Authored typology (approximate WALS frequencies): SOV+SVO must
        // dominate the draw. Measure over 200 seeds.
        let ph = test_phonology();
        let mut sov_svo = 0;
        for s in 1..=200u64 {
            let g = tongue_grammar(&Seed(s), "goblin", &ph);
            if matches!(g.order, ConstituentOrder::Sov | ConstituentOrder::Svo) {
                sov_svo += 1;
            }
        }
        assert!(
            sov_svo > 120,
            "SOV+SVO should dominate (~87% authored): {sov_svo}/200"
        );
    }

    /// claim: rate(forall-seed, [100, 140] / 200) — authored copula-presence
    /// weight, wide smoke-test band
    #[test]
    fn copula_presence_rate_matches_authored_weight() {
        // Authored: 60% of tongues bear an overt copula. Measure over 200
        // seeds and allow a wide band (this is a typology-weight smoke
        // test, not a statistical calibration).
        let ph = test_phonology();
        let mut present = 0;
        for s in 1..=200u64 {
            if tongue_grammar(&Seed(s), "goblin", &ph).copula.is_some() {
                present += 1;
            }
        }
        assert!(
            (100..=140).contains(&present),
            "~60% authored copula presence: {present}/200"
        );
    }

    /// claim: rate(forall-seed, [40, 80] / 200) — authored article-presence
    /// weight
    #[test]
    fn articles_presence_rate_matches_authored_weight() {
        // Authored: 30% of tongues have articles.
        let ph = test_phonology();
        let mut present = 0;
        for s in 1..=200u64 {
            if tongue_grammar(&Seed(s), "goblin", &ph).articles {
                present += 1;
            }
        }
        assert!(
            (40..=80).contains(&present),
            "~30% authored article presence: {present}/200"
        );
    }

    /// claim: reachability(seed: 1..=50, find_map for a copula-bearing draw) —
    /// then checks non-emptiness and determinism at the found seed
    #[test]
    fn copula_form_is_nonempty_and_deterministic() {
        // The copula's form (when present) is a real drawn word: never
        // empty, and a pure function of (seed, species) like every other
        // draw in this crate.
        let ph = test_phonology();
        let (seed_val, form) = (1..=50u64)
            .find_map(|s| {
                tongue_grammar(&Seed(s), "goblin", &ph)
                    .copula
                    .map(|c| (s, c))
            })
            .expect("at least one seed in 1..=50 draws a copula at a 60% rate");
        assert!(!form.is_empty(), "a drawn copula form must not be empty");
        let again = tongue_grammar(&Seed(seed_val), "goblin", &ph)
            .copula
            .expect("re-draw at the same seed must also draw a copula");
        assert_eq!(form, again, "copula form is a deterministic draw");
    }

    /// Build a tiny real lexicon (via `build_lexicon`'s own machinery, per
    /// `lexicon.rs`'s test pattern — never a mock) exposing exactly
    /// `concepts`, all `family == species == "test-tongue"` and
    /// `proto_ph == ph` (a singleton stock, collapsing family-level
    /// cognate-sharing to a single tongue drawing its own roots directly).
    fn tiny_lexicon_with(concepts: &[(&str, ExposureClass)]) -> Lexicon {
        let ph = test_phonology();
        let mut exposures = BTreeMap::new();
        for (concept, class) in concepts {
            exposures.insert((*concept).to_string(), class.clone());
        }
        build_lexicon(
            &Seed(1),
            "test-tongue",
            "test-tongue",
            &ph,
            &ph,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        )
    }

    #[test]
    fn every_argument_shape_resolves_the_same_way_in_either_slot() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        // A concept goes through the lexicon; everything else is passed through
        // or rendered at the tongue's own grain. This is what `realize_adjuncts`
        // has always done -- naming it is what lets the OBJECT slot do it too.
        assert_eq!(
            resolve_argument(&Argument::Name("Nwamvam".to_string()), &lex).unwrap(),
            "Nwamvam"
        );
        assert_eq!(
            resolve_argument(&Argument::Count(8835), &lex).unwrap(),
            "8835"
        );
        assert_eq!(
            resolve_argument(&Argument::Quantity(1.5), &lex).unwrap(),
            "1.5"
        );
        // A concept with no entry gaps, and the gap names the concept.
        let gap =
            resolve_argument(&Argument::Concept("no-such-concept".to_string()), &lex).unwrap_err();
        assert_eq!(gap.concept, "no-such-concept");
        assert_eq!(gap.reason, "no entry in this lexicon");
    }

    #[test]
    fn realize_tongue_orders_and_copula() {
        // Grammar fixed by hand (not drawn) to pin each transform: the
        // copula's TEST value "gha" is arbitrary — production forms are
        // always drawn (see the module doc and `draw_copula_form`).
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let word = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![],
        };
        let svo = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        assert_eq!(
            realize_tongue(&clause, &svo, &lex).unwrap(),
            format!("Vavako gha {word}.")
        );
        let sov = TongueGrammar {
            order: ConstituentOrder::Sov,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        assert_eq!(
            realize_tongue(&clause, &sov, &lex).unwrap(),
            format!("Vavako {word} gha.")
        );
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
        };
        assert_eq!(
            realize_tongue(&clause, &zero_copula, &lex).unwrap(),
            format!("Vavako {word}.")
        );
    }

    #[test]
    fn realize_tongue_gaps_whole_sentence() {
        let lex = tiny_lexicon_with(&[]); // no entries → concept is a gap
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let gap = realize_tongue(&clause, &g, &lex).unwrap_err();
        assert_eq!(gap.concept, "planet");
        assert!(!gap.reason.is_empty(), "recountable reason required");
    }

    #[test]
    fn realize_tongue_gaps_with_the_lexicon_own_gap_reason() {
        // A concept that IS in the lexicon but as a `LexEntry::Gap` (not
        // simply absent) must surface that gap's own recountable reason,
        // rendered via its Display-style text (not `{:?}`).
        let mut exposures = BTreeMap::new();
        exposures.insert(
            "blue".to_string(),
            ExposureClass::Unknown {
                reason: crate::lexicon::GapReason::Perceptual(
                    "hue ladder depth 3 from night-vision 0.8".to_string(),
                ),
            },
        );
        let ph = test_phonology();
        let lex = build_lexicon(
            &Seed(1),
            "test-tongue",
            "test-tongue",
            &ph,
            &ph,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        );
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("blue".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let gap = realize_tongue(&clause, &g, &lex).unwrap_err();
        assert_eq!(gap.concept, "blue");
        assert!(
            gap.reason
                .contains("hue ladder depth 3 from night-vision 0.8"),
            "gap reason must recount the lexicon's own reason text, got {:?}",
            gap.reason
        );
        assert!(
            !gap.reason.contains("Perceptual("),
            "gap reason must not be the Debug form, got {:?}",
            gap.reason
        );
    }

    #[test]
    fn a_tongue_realizes_an_adjunct_whose_concept_it_knows() {
        let lex = tiny_lexicon_with(&[
            ("planet", ExposureClass::Steeped),
            ("yellow-white-dwarf", ExposureClass::Steeped),
        ]);
        let star_word = match lex.entry("yellow-white-dwarf").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("expected a Root, got {other:?}"),
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            }],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let out = realize_tongue(&clause, &g, &lex).expect("both concepts are known");
        assert!(
            out.contains(&star_word),
            "the tongue's own word for the star class must appear: {out}"
        );
        assert!(
            !out.contains("orbiting"),
            "Common's role surface must not leak into a tongue: {out}"
        );
    }

    #[test]
    fn a_tongue_gaps_on_an_adjunct_concept_it_lacks_rather_than_emitting_common() {
        // The COMPLEMENT is known; only the ADJUNCT's concept is missing, so a
        // partial render is the tempting wrong answer. Spec section 4 of this
        // module: renders fully or gaps entirely, never partially.
        let lex = tiny_lexicon_with(&[("planet", ExposureClass::Steeped)]);
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            }],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let gap = realize_tongue(&clause, &g, &lex).unwrap_err();
        assert_eq!(gap.concept, "yellow-white-dwarf");
        assert!(!gap.reason.is_empty(), "recountable reason required");
    }

    #[test]
    fn shallow_identity_holds_with_nonempty_adjuncts() {
        // `realize_tongue_marks_by_depth`'s own shallow-identity assertion
        // (and `hornvale-book`'s `shallow_species_lines_are_byte_identical_to_c3`)
        // only ever exercise `adjuncts: vec![]` — a proof that would still
        // pass even if the deep realizer's adjunct handling diverged from
        // the floor realizer's. This test is the one that actually depends
        // on the adjunct path agreeing between the two realizers.
        let lex = tiny_lexicon_with(&[
            ("goblin-kind", ExposureClass::Steeped),
            ("yellow-white-dwarf", ExposureClass::Steeped),
        ]);
        let star_word = match lex.entry("yellow-white-dwarf").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("expected a Root, got {other:?}"),
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            }],
        };
        let grammar = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let shallow = TongueMorphology {
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: BTreeMap::new(),
            class: BTreeMap::new(),
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let floor = realize_tongue(&clause, &grammar, &lex).unwrap();
        let deep = realize_tongue_deep(
            &clause,
            &grammar,
            &shallow,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        assert!(
            floor.contains(&star_word),
            "sanity: the adjunct's own word must actually appear: {floor}"
        );
        assert_eq!(
            deep, floor,
            "MorphDepth::None on both axes must reproduce the floor surface \
             exactly, including a non-empty adjunct list"
        );
    }

    #[test]
    fn realize_tongue_exhaustive_orders_and_copula() {
        // All 6 orders × copula Some/None = 12 exact-string assertions for a
        // fixed clause — pins every transform's exact surface shape.
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let word = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![],
        };
        let cases: [(ConstituentOrder, Option<&str>, String); 12] = [
            (
                ConstituentOrder::Sov,
                Some("gha"),
                format!("Vavako {word} gha."),
            ),
            (ConstituentOrder::Sov, None, format!("Vavako {word}.")),
            (
                ConstituentOrder::Svo,
                Some("gha"),
                format!("Vavako gha {word}."),
            ),
            (ConstituentOrder::Svo, None, format!("Vavako {word}.")),
            (
                ConstituentOrder::Vso,
                Some("gha"),
                format!("gha Vavako {word}."),
            ),
            (ConstituentOrder::Vso, None, format!("Vavako {word}.")),
            (
                ConstituentOrder::Vos,
                Some("gha"),
                format!("gha {word} Vavako."),
            ),
            (ConstituentOrder::Vos, None, format!("{word} Vavako.")),
            (
                ConstituentOrder::Ovs,
                Some("gha"),
                format!("{word} gha Vavako."),
            ),
            (ConstituentOrder::Ovs, None, format!("{word} Vavako.")),
            (
                ConstituentOrder::Osv,
                Some("gha"),
                format!("{word} Vavako gha."),
            ),
            (ConstituentOrder::Osv, None, format!("{word} Vavako.")),
        ];
        for (order, copula, expected) in cases {
            let grammar = TongueGrammar {
                order,
                copula: copula.map(String::from),
                copula_segments: None,
                articles: false,
            };
            assert_eq!(
                realize_tongue(&clause, &grammar, &lex).unwrap(),
                expected,
                "order {order:?} copula {copula:?}"
            );
        }
    }

    /// A grammar drawn with a real overt copula (segments AND roman
    /// together, per `tongue_grammar`'s own draw) — the fixture the C7
    /// realizer tests below share, so the Affix/Particle evidential arms
    /// exercise the SAME segment-level copula plumbing production code
    /// uses, not a hand-typed roman-only stand-in.
    fn overt_copula_grammar(ph: &Phonology) -> TongueGrammar {
        (1..=50u64)
            .find_map(|s| {
                let g = tongue_grammar(&Seed(s), "goblin", ph);
                g.copula.is_some().then_some(g)
            })
            .expect("at least one seed in 1..=50 draws an overt copula at a 60% rate")
    }

    #[test]
    fn realize_tongue_marks_by_depth() {
        use crate::etymology::proto_root;

        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let complement_segments = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { derivation, .. } => derivation.modern.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: vec![],
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;

        // Synthetic marker forms (real drawn words, just not family-cognate
        // here — `morph_forms`' own cognate law has its own test in
        // `morphology.rs`; this test is about the REALIZER's placement
        // logic).
        let witnessed_segments = proto_root(&Seed(99), "goblin", "witness-marker", &ph);
        let witnessed_roman = render_views(&witnessed_segments).roman;
        let mut evidential_map = BTreeMap::new();
        evidential_map.insert(
            "witnessed",
            MorphForm {
                segments: witnessed_segments.clone(),
                roman: witnessed_roman.clone(),
            },
        );
        let class_segments = proto_root(&Seed(98), "goblin", "class-marker", &ph);
        let mut class_map = BTreeMap::new();
        class_map.insert(
            "inanimate",
            MorphForm {
                segments: class_segments.clone(),
                roman: render_views(&class_segments).roman,
            },
        );

        let grammar = overt_copula_grammar(&ph);

        // 1. `MorphDepth::None` on both axes reproduces the C3 floor
        // surface exactly (the shallow-identity guarantee), even with real
        // marker forms sitting unused in the bundle.
        let shallow = TongueMorphology {
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map.clone(),
            class: class_map.clone(),
        };
        assert_eq!(
            realize_tongue_deep(
                &clause,
                &grammar,
                &shallow,
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap(),
            realize_tongue(&clause, &grammar, &lex).unwrap(),
            "MorphDepth::None on both axes must reproduce the C3 floor surface exactly"
        );

        // 2. Affix evidential (overt copula) -> the witnessed marker
        // appears predicate-finally, suffixed onto the copula at the
        // SEGMENT level.
        let affix_evidential = TongueMorphology {
            evidential_depth: MorphDepth::Affix,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map.clone(),
            class: class_map.clone(),
        };
        let copula_segments = grammar
            .copula_segments
            .clone()
            .expect("overt_copula_grammar draws copula_segments alongside copula");
        let expected_copula = affix(
            &copula_segments,
            &witnessed_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let marked = realize_tongue_deep(
            &clause,
            &grammar,
            &affix_evidential,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();
        assert!(
            marked.contains(&expected_copula),
            "Affix evidential must suffix the marker onto the copula: {marked:?} \
             (expected token {expected_copula:?})"
        );

        // 3. Particle evidential -> a free word immediately after the
        // predicate (the copula, here).
        let particle_evidential = TongueMorphology {
            evidential_depth: MorphDepth::Particle,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map.clone(),
            class: class_map.clone(),
        };
        let particled = realize_tongue_deep(
            &clause,
            &grammar,
            &particle_evidential,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();
        let tokens: Vec<&str> = particled.trim_end_matches('.').split(' ').collect();
        let copula_roman = grammar.copula.as_deref().unwrap();
        let copula_idx = tokens
            .iter()
            .position(|t| *t == copula_roman)
            .expect("the bare copula token must still be present, unmodified");
        assert_eq!(
            tokens.get(copula_idx + 1),
            Some(&witnessed_roman.as_str()),
            "the evidential particle must sit immediately after the predicate: {tokens:?}"
        );

        // 4. class Affix + Prefix -> the marker precedes the complement
        // noun (joined at the segment level).
        let class_affix_prefix = TongueMorphology {
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::Affix,
            class_position: ClassPosition::Prefix,
            evidential: evidential_map.clone(),
            class: class_map.clone(),
        };
        let expected_noun = affix(
            &complement_segments,
            &class_segments,
            ClassPosition::Prefix,
            Orthography::Digraph,
        )
        .roman;
        let class_marked = realize_tongue_deep(
            &clause,
            &grammar,
            &class_affix_prefix,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();
        assert!(
            class_marked.contains(&expected_noun),
            "class Affix + Prefix must precede the complement noun with the marker: \
             {class_marked:?} (expected token {expected_noun:?})"
        );

        // 5. Zero-copula + Affix evidential -> enclitic on the predicate
        // nominal (the fixed-position rule's zero-copula arm).
        let zero_copula_grammar = TongueGrammar {
            order: grammar.order,
            copula: None,
            copula_segments: None,
            articles: grammar.articles,
        };
        let expected_enclitic = affix(
            &complement_segments,
            &witnessed_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let zero_marked = realize_tongue_deep(
            &clause,
            &zero_copula_grammar,
            &affix_evidential,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();
        assert!(
            zero_marked.contains(&expected_enclitic),
            "zero-copula Affix evidential must enclitic onto the predicate nominal: \
             {zero_marked:?} (expected token {expected_enclitic:?})"
        );
    }

    /// Final-review fix 2 (C7): the layer_affix Compound arm is loud AND
    /// tested — a segment-less Marked (the Compound-complement shape)
    /// asked to take an affix panics rather than silently roman-concats.
    #[test]
    #[should_panic(expected = "layer_affix: cannot segment-affix")]
    fn layer_affix_panics_on_a_segmentless_word() {
        let marker = MorphForm {
            segments: Vec::new(),
            roman: "bo".to_string(),
        };
        let current = Marked {
            segments: None,
            roman: "Manywater".to_string(),
        };
        let _ = layer_affix(
            current,
            &marker,
            ClassPosition::Suffix,
            Orthography::Digraph,
        );
    }

    #[test]
    fn inferred_is_defined_and_loud() {
        use crate::etymology::proto_root;

        // Inferred is floor-unreachable today (no T1/T2 readout path
        // constructs it — the readout fns beyond this task own that
        // guard), but it must already render correctly when passed
        // explicitly: exhaustive-match future-proofing, not a live path.
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Inferred,
            adjuncts: vec![],
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let inferred_segments = proto_root(&Seed(77), "goblin", "inferred-marker", &ph);
        let inferred_roman = render_views(&inferred_segments).roman;
        let mut evidential_map = BTreeMap::new();
        evidential_map.insert(
            "inferred",
            MorphForm {
                segments: inferred_segments,
                roman: inferred_roman.clone(),
            },
        );
        let morph = TongueMorphology {
            evidential_depth: MorphDepth::Particle,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map,
            class: BTreeMap::new(),
        };
        let grammar = overt_copula_grammar(&ph);

        let rendered = realize_tongue_deep(
            &clause,
            &grammar,
            &morph,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .expect("a Steeped complement must realize");
        assert!(
            rendered.contains(&inferred_roman),
            "Inferred must render with its drawn form when passed explicitly: {rendered:?}"
        );
    }

    /// A hand-fixed SVO grammar with an overt copula. The value "gha" is
    /// arbitrary and follows the neighbouring tests' convention; production
    /// copula forms are always drawn.
    fn svo_with_copula() -> TongueGrammar {
        TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        }
    }

    #[test]
    fn a_tongue_gaps_on_a_pronoun_subject_and_names_the_missing_inventory() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun("it"),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: Vec::new(),
        };
        let gap = realize_tongue(&clause, &grammar, &lex).unwrap_err();
        // Spec 3.3: the reason must name the INVENTORY hole. A tongue with no
        // drawn pronouns cannot re-mention; that is not the same claim as "this
        // people cannot refer to things twice", and the reason must not imply it.
        assert!(
            gap.reason.contains("pronoun"),
            "the gap must name pronouns, got: {}",
            gap.reason
        );
        // And no English may cross: the Common pronoun must not appear in any
        // tongue-side output. Before The Scarf, tongue_view stringified it.
        assert_ne!(
            gap.concept, "it",
            "the English pronoun must not be reported as a tongue concept"
        );
    }

    #[test]
    fn a_tongue_predicates_a_bare_count_in_the_object_slot() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Count(8835),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: Vec::new(),
        };
        // Impossible before The Scarf: tongue_view panicked on any object that
        // was not a Concept, while realize_adjuncts rendered exactly this shape
        // in the adjunct slot. The two slots now agree.
        let out =
            realize_tongue(&clause, &grammar, &lex).expect("a numeral needs no lexicon entry");
        assert!(out.contains("8835"), "got: {out}");
    }

    #[test]
    fn a_tongue_ignores_number_and_definiteness() {
        // The mirror of `common_ignores_the_evidential` (Task 1), and the PAIR is
        // what makes spec 3.2 a LAW rather than two separate defects. Read alone,
        // either half invites the wrong repair.
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let base = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: Vec::new(),
        };
        let plural_indef = Clause {
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            ..base.clone()
        };
        assert_eq!(
            realize_tongue(&base, &grammar, &lex).unwrap(),
            realize_tongue(&plural_indef, &grammar, &lex).unwrap(),
            "no tongue reads number or definiteness yet: paradigm.rs's drawn \
             number_depth is the next campaign's work, not a gap in this one"
        );
    }

    #[test]
    #[should_panic(expected = "no tongue construction for predicate")]
    fn a_tongue_panics_on_an_uncovered_predicate_rather_than_gapping() {
        // Spec 3.3. A TongueGap asserts something TRUE ABOUT THE WORLD; a missing
        // construction is an authoring hole in this repository. Gapping here would
        // put a false claim about a people into a rendered artifact.
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let clause = Clause {
            predicate: "dwells-in".to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: Vec::new(),
        };
        let _ = realize_tongue(&clause, &grammar, &lex);
    }

    #[test]
    fn a_non_lexical_object_bears_no_morphology_and_does_not_panic() {
        use crate::etymology::proto_root;

        // SPEC 4.3, AND THIS IS THE CAMPAIGN'S LOWEST-CONFIDENCE DECISION, so it
        // gets the sharpest construction available. Every choice here is aimed at
        // the ONE branch that would otherwise panic: an Affix-depth tongue with a
        // ZERO COPULA sends the evidential marker to the predicate NOMINAL, which
        // here is a bare numeral carrying no segments. `layer_affix` panics on
        // exactly that word shape.
        //
        // The guard is on the CONCEPT ID, never on `Marked.segments == None` -- a
        // Compound has that too, and its missing segments are a lexicon bug the
        // panic exists to expose. Guarding on segments would silence both.
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let witnessed_segments = proto_root(&Seed(99), "goblin", "witness-marker", &ph);
        let witnessed_roman = render_views(&witnessed_segments).roman;
        let mut evidential_map = BTreeMap::new();
        evidential_map.insert(
            "witnessed",
            MorphForm {
                segments: witnessed_segments,
                roman: witnessed_roman.clone(),
            },
        );
        let class_segments = proto_root(&Seed(98), "goblin", "class-marker", &ph);
        let class_roman = render_views(&class_segments).roman;
        let mut class_map = BTreeMap::new();
        class_map.insert(
            "inanimate",
            MorphForm {
                segments: class_segments,
                roman: class_roman.clone(),
            },
        );

        let affixing = TongueMorphology {
            evidential_depth: MorphDepth::Affix,
            noun_class_depth: MorphDepth::Affix,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map,
            class: class_map,
        };
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Count(8835),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            adjuncts: Vec::new(),
        };

        let out = realize_tongue_deep(
            &clause,
            &zero_copula,
            &affixing,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .expect("a numeral needs no lexicon entry");

        assert!(
            out.contains("8835"),
            "the numeral must reach the sentence: {out}"
        );
        assert!(
            !out.contains(&witnessed_roman),
            "a numeral must bear no evidential affix: {out}"
        );
        assert!(
            !out.contains(&class_roman),
            "a numeral must bear no noun-class affix: {out}"
        );
    }
}
