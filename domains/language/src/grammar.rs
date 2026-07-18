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

use crate::lexicon::{LexEntry, Lexicon};
use crate::naming::{Namer, render_views, segments_of};
use crate::phonology::Phonology;
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
/// and rendered via [`render_views`] — the same reduction every lexicon
/// word and generated name goes through, so a drawn copula is
/// indistinguishable in kind from any other word in the tongue.
fn draw_copula_form(stream: &mut Stream, namer: &Namer) -> String {
    let syllables = namer.draw_syllables(stream, 1, 1, false);
    render_views(&segments_of(&syllables)).roman
}

/// Draw `species`' tongue grammar from the three permanent grammar streams
/// (`language/<species>/grammar/…`): constituent order, copula presence
/// (and drawn form), and article presence.
/// type-audit: bare-ok(identifier-text)
pub fn tongue_grammar(seed: &Seed, species: &str, ph: &Phonology) -> TongueGrammar {
    let namer = Namer::new(seed, species, ph);

    let mut order_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("constituent-order")
        .stream();
    let order = order_from_roll(order_stream.range_u32(1, 100));

    let mut copula_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("copula")
        .stream();
    let copula = if copula_stream.range_u32(1, 100) <= 60 {
        Some(draw_copula_form(&mut copula_stream, &namer))
    } else {
        None
    };

    let mut articles_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("articles")
        .stream();
    let articles = articles_stream.range_u32(1, 100) <= 30;

    TongueGrammar {
        order,
        copula,
        articles,
    }
}

/// One nominal-predication clause for a tongue: an already-surfaced subject
/// (autonym / proper name — tongue words already) and the complement as a
/// CONCEPT id to lexicalize in the speaker's lexicon.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueClause {
    /// The subject, already in surface form.
    pub subject: String,
    /// The complement concept id (e.g. `"goblin-kind"`), lexicalized via
    /// the speaker's lexicon.
    pub complement_concept: String,
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

/// Realize a nominal-predication clause in a tongue: lexicalize the
/// complement, order the constituents per the grammar, include the copula
/// if the tongue bears one. Renders fully or gaps entirely (spec §4).
/// type-audit: bare-ok(prose)
pub fn realize_tongue(
    clause: &TongueClause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
) -> Result<String, TongueGap> {
    let complement = match lexicon.entry(&clause.complement_concept) {
        Some(LexEntry::Root { views, .. }) | Some(LexEntry::Compound { views, .. }) => {
            views.roman.clone()
        }
        Some(LexEntry::Gap { reason }) => {
            // `GapReason`'s Display is the canonical recountable rendering
            // ("gap (experiential): ..." / "gap (perceptual): ...") — never
            // `{reason:?}`; the reason is prose to recount, not debug.
            return Err(TongueGap {
                concept: clause.complement_concept.clone(),
                reason: reason.to_string(),
            });
        }
        None => {
            return Err(TongueGap {
                concept: clause.complement_concept.clone(),
                reason: "no entry in this lexicon".to_string(),
            });
        }
    };
    let s = clause.subject.as_str();
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
    Ok(format!("{}.", ordered.join(" ")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexicon::{ExposureClass, LexEntry, build_lexicon};
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use std::collections::BTreeMap;

    /// A goblin-baseline articulation envelope — per `phonology.rs`'s own
    /// test-constructor pattern (`goblin_env`), reconstructed locally here
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
        draw_phonology(&Seed(1), "test-tongue", &test_envelope())
    }

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
        )
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
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "goblin-kind".into(),
        };
        let svo = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            articles: false,
        };
        assert_eq!(
            realize_tongue(&clause, &svo, &lex).unwrap(),
            format!("Vavako gha {word}.")
        );
        let sov = TongueGrammar {
            order: ConstituentOrder::Sov,
            copula: Some("gha".into()),
            articles: false,
        };
        assert_eq!(
            realize_tongue(&clause, &sov, &lex).unwrap(),
            format!("Vavako {word} gha.")
        );
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
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
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "planet".into(),
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
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
        );
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "blue".into(),
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
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
    fn realize_tongue_exhaustive_orders_and_copula() {
        // All 6 orders × copula Some/None = 12 exact-string assertions for a
        // fixed clause — pins every transform's exact surface shape.
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let word = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "goblin-kind".into(),
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
                articles: false,
            };
            assert_eq!(
                realize_tongue(&clause, &grammar, &lex).unwrap(),
                expected,
                "order {order:?} copula {copula:?}"
            );
        }
    }
}
