//! A tongue's drawn surface grammar: constituent order, copula presence
//! (and, when present, its drawn form), article presence, and — since The
//! Mortise — subordination strategy (Task 5: whether an embedded clause is
//! marked with a complementizer, and its drawn form) and coordination
//! strategy (Task 6: whether coordinated clauses are joined with a
//! conjunction, and its drawn form). This is the floor slice of LANG-40's
//! grammaticalization-depth vector — C7 (the morphology campaign) extends
//! [`TongueGrammar`], never replaces it.
//!
//! Word order is historically **contingent**, not derivable from a
//! species' psychology or subsistence pattern (spec §3): deriving it from
//! existing culture vectors would be astrology shipped as science, so
//! these parameters are DRAWN from five permanent stream labels
//! (`language/<species>/grammar/constituent-order`,
//! `language/<species>/grammar/copula`, `language/<species>/grammar/articles`,
//! `language/<species>/grammar/subordinator`,
//! `language/<species>/grammar/conjunction`)
//! — build-state (decision 0058): drawn at composition/render time, never
//! serialized, so adding them is byte-identical to every existing world.
//!
//! The copula's overt form is never authored text: when a tongue draws a
//! copula, its one-syllable form is filled from the tongue's own
//! [`Phonology`] by the same syllable-fill mechanism [`crate::etymology::proto_root`]
//! and [`crate::naming::Namer`] use for every other generated word — zero
//! authored surface text anywhere in a generated tongue (the program
//! thesis). A drawn subordinator's form is filled the identical way (spec
//! §4.6): a hardcoded complementizer would make every tongue subordinate
//! like English, the exact failure this module's realizers exist to
//! prevent, and a tongue that draws none subordinates by bare parataxis —
//! a legitimate grammar, not a gap. A drawn conjunction's form is filled
//! the identical way again (spec §4.10): a hardcoded "and" would make
//! every tongue coordinate like English, and a tongue that draws none
//! coordinates by bare juxtaposition — also a legitimate grammar, not a
//! gap.
//!
//! **A vocabulary word costs zero stream labels; a function word costs
//! one.** A word is a `dynamic(concept)` value on the existing
//! `PROTO_ROOT` axis ([`crate::etymology::proto_root`]) — every concept a
//! tongue ever lexicalizes rides that one draw, so the vocabulary itself is
//! free. The copula, the subordinator and the conjunction each cost a
//! label instead, because a function word's PRESENCE is typological rather
//! than lexical: the tongue that draws no conjunction is not missing a
//! word the way it might lack a word for "sea" — it has a different
//! grammar, and that grammar fact is what the label records. This is why
//! this campaign adds two labels (§4.6, §4.10) and not one per generated
//! word.

use crate::clause::{
    Adjunct, Argument, CLAUSE_EMBED_MAX_DEPTH, Clause, Coordination, Number, Person, Polarity,
    Subject, Tense, Valence, clause_embed_depth, elide_coordinated_subjects, predicate_valence,
    subject_embed_depth,
};
use crate::lexicon::{LexEntry, Lexicon};
use crate::morphology::{
    ClassPosition, Evidential, MorphDepth, MorphForm, NounClass, TongueMorphology, affix,
};
use crate::naming::{Namer, render_views_with, segments_of};
use crate::paradigm::ParadigmDepths;
use crate::phoneme::Segment;
use crate::phonology::Phonology;
use crate::streams;
use crate::typology::Orthography;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, Stream};
use std::collections::BTreeMap;

/// The six constituent orders of a subject–verb–object clause.
///
/// The V slot was always a verb slot; until The Inquest the only thing that
/// could stand in it was the drawn copula of a nominal predication ("The
/// Vavako are goblins"), so the variant docs below name the copula. A
/// transitive clause ("The Vavako eat bread") supplies a real lexical verb
/// there, ordered by exactly these six and nothing else.
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
    /// The overt subordinator's roman form for tongues that mark an
    /// embedded clause with a free complementizer word — drawn from the
    /// tongue's own phonology, never authored — or `None` for a tongue
    /// that subordinates by bare parataxis: juxtaposition, no marker, a
    /// legitimate drawn value and not a degenerate one (The Mortise, Task
    /// 5, spec §4.6). A free word, unlike the copula: it never hosts an
    /// affix layer, so unlike [`Self::copula`] no parallel `_segments`
    /// field is carried.
    pub subordinator: Option<String>,
    /// The overt coordinating conjunction's roman form for tongues that
    /// mark clause coordination with a free word — drawn from the tongue's
    /// own phonology, never authored — or `None` for a tongue that
    /// coordinates by bare juxtaposition: no marker, a legitimate drawn
    /// value and not a degenerate one (The Mortise, Task 6, spec §4.10).
    /// On the identical shape [`Self::subordinator`] carries and for the
    /// identical reason: a free word that never hosts an affix layer, so no
    /// parallel `_segments` field.
    pub conjunction: Option<String>,
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

/// Draw the overt subordinator's one-syllable roman form from `namer`'s
/// phonology, consuming `stream` — the same stream the presence roll
/// already drew from, so presence and form share the one permanent
/// `.../grammar/subordinator` stream (The Mortise, Task 5). Uses the exact
/// syllable-fill mechanism [`draw_copula_form`] uses for the copula: one
/// template syllable via [`Namer::draw_syllables`], flattened via
/// [`segments_of`] and rendered via [`render_views_with`] under `orth` —
/// the same reduction every generated word in the tongue goes through.
/// Unlike [`draw_copula_form`], only the roman form is returned: a
/// subordinator is a free boundary word that never hosts an affix layer
/// (spec §4.6 names it a free word), so it needs no parallel segments to
/// join at.
fn draw_subordinator_form(stream: &mut Stream, namer: &Namer, orth: Orthography) -> String {
    let syllables = namer.draw_syllables(stream, 1, 1, false);
    let segments = segments_of(&syllables);
    render_views_with(&segments, orth).roman
}

/// Draw the overt coordinating conjunction's one-syllable roman form from
/// `namer`'s phonology, consuming `stream` — the same stream the presence
/// roll already drew from, so presence and form share the one permanent
/// `.../grammar/conjunction` stream (The Mortise, Task 6). Uses the exact
/// syllable-fill mechanism [`draw_copula_form`]/[`draw_subordinator_form`]
/// use: one template syllable via [`Namer::draw_syllables`], flattened via
/// [`segments_of`] and rendered via [`render_views_with`] under `orth`.
/// Only the roman form is returned, on the identical reasoning
/// [`draw_subordinator_form`] states: a coordinating conjunction is a free
/// word that never hosts an affix layer, so it needs no parallel segments
/// to join at.
fn draw_conjunction_form(stream: &mut Stream, namer: &Namer, orth: Orthography) -> String {
    let syllables = namer.draw_syllables(stream, 1, 1, false);
    let segments = segments_of(&syllables);
    render_views_with(&segments, orth).roman
}

/// Draw `species`' tongue grammar from the five permanent grammar streams
/// (`language/<species>/grammar/…`): constituent order, copula presence
/// (and drawn form), article presence, subordination strategy
/// (complementizer presence and drawn form, or bare parataxis), and
/// coordination strategy (conjunction presence and drawn form, or bare
/// juxtaposition).
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

    let mut subordinator_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::SUBORDINATOR)
        .stream();
    // 50/50: unlike the copula's approximate WALS-informed 60%, no
    // literature-backed skew is cited for this axis, and neither strategy
    // is degenerate (spec §4.6) — a complementizer and bare parataxis are
    // both attested, so the two split evenly.
    let subordinator = if subordinator_stream.range_u32(1, 100) <= 50 {
        Some(draw_subordinator_form(
            &mut subordinator_stream,
            &namer,
            ph.orthography,
        ))
    } else {
        None
    };

    let mut conjunction_stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::CONJUNCTION)
        .stream();
    // 50/50, on the identical reasoning the subordinator draw states: no
    // literature-backed skew is cited for this axis, and neither strategy
    // is degenerate (spec §4.10) — a coordinating conjunction and bare
    // juxtaposition (asyndetic coordination) are both attested, so the two
    // split evenly.
    let conjunction = if conjunction_stream.range_u32(1, 100) <= 50 {
        Some(draw_conjunction_form(
            &mut conjunction_stream,
            &namer,
            ph.orthography,
        ))
    } else {
        None
    };

    TongueGrammar {
        order,
        copula,
        copula_segments,
        articles,
        subordinator,
        conjunction,
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

/// Apply the tongue's drawn subordination strategy to an already-realized
/// embedded clause's surface text (spec §4.6): prefix the drawn
/// subordinator's form at the clause's boundary, or leave `text` bare for a
/// tongue that drew parataxis — juxtaposition with no marker, a legitimate
/// grammar and not a gap. Also trims a trailing full stop before deciding —
/// the caller's own outer clause supplies the sentence's one terminal
/// `.` — matching [`crate::clause::realize_common`]'s identical trim for the
/// identical reason.
///
/// **Takes the already-assembled `text`, never rebuilds it.** The caller has
/// just finished a full realize call (floor or deep) for the inner clause;
/// re-deriving anything from `grammar` here rather than consuming that
/// result is exactly the "rebuilt the copula from `grammar.copula`" trap a
/// third consumer of an assembled value could fall into.
fn mark_embedded_clause(grammar: &TongueGrammar, mut text: String) -> String {
    if text.ends_with('.') {
        text.pop();
    }
    match &grammar.subordinator {
        Some(marker) => format!("{marker} {text}"),
        None => text,
    }
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
///
/// An [`Argument::Pronoun`] resolves through `pronouns` — the tongue's own
/// drawn inventory — at `number`, the clause's own. It does **not** go
/// through the lexicon: a pronoun is a closed-class grammatical word every
/// tongue draws, not vocabulary a people may or may not have been exposed
/// to, so no exposure gap is possible on it.
///
/// **`Argument::Clause` realizes the inner clause through [`realize_tongue`]
/// (the FLOOR realizer, not `realize_tongue_deep`) and marks its boundary
/// per [`mark_embedded_clause`]** (The Mortise, Task 5). This is the FLOOR
/// path deliberately: `realize_tongue_deep` special-cases `Argument::Clause`
/// and `Subject::Clause` itself, before ever reaching this function, so the
/// inner clause's own evidential/tense/polarity marking survives through a
/// DEEP recursive call (spec §4.5) rather than being flattened to the floor
/// here. This arm is therefore reached only through the shared
/// `realize_tongue_with_subject` dispatch — [`realize_tongue`]'s own direct
/// call and [`realize_tongue_coordination`]'s per-clause call (Task 7),
/// both floor callers — never from `realize_tongue_deep`'s object slot.
/// `realize_adjuncts` never reaches this arm for an adjunct's own clause
/// either way (it refuses that case itself, with the real reason, before
/// ever calling this function).
///
/// The depth check runs BEFORE the recursive call, the same ordering
/// [`crate::clause::realize_common`] uses, so a clause past the cap panics
/// without ever realizing the offending text.
fn resolve_argument(
    argument: &Argument,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    number: Number,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<String, TongueGap> {
    match argument {
        Argument::Concept(id) => Ok(resolve_concept_marked(id, lexicon)?.roman),
        Argument::Name(text) => Ok(text.clone()),
        Argument::Count(n) => Ok(n.to_string()),
        Argument::Quantity(x) => Ok(x.to_string()),
        Argument::Pronoun(person) => Ok(tongue_pronoun(*person, number, pronouns)?),
        // The intransitive frame's object slot: no argument at all, and
        // callers that reach here for it (this crate's floor realizer) have
        // nothing to resolve. Task 3 gives the deep realizer its own
        // ordering-aware treatment of an absent object; this arm only keeps
        // the match exhaustive for a caller of the floor path.
        Argument::Absent => Ok(String::new()),
        Argument::Clause(inner) => {
            let depth = clause_embed_depth(argument);
            assert!(
                depth <= CLAUSE_EMBED_MAX_DEPTH,
                "a clause complement nests {depth} deep, past the cap of \
                 {CLAUSE_EMBED_MAX_DEPTH}: a clause complement may not \
                 itself contain a clause complement"
            );
            let text = realize_tongue(inner, grammar, lexicon, pronouns)?;
            Ok(mark_embedded_clause(grammar, text))
        }
    }
}

/// One pronoun's surface in this tongue: the drawn form for `person` crossed
/// with `number`.
///
/// **The Scarf's blanket refusal is gone from here** (spec §4.6). Until this
/// campaign, every pronoun — subject or object — returned a [`TongueGap`]
/// unconditionally, because no tongue drew an inventory at all. Drawing one
/// falsifies that antecedent, so the arm was deleted rather than left
/// unreachable, and this lookup replaced it. What survives is narrower and
/// still true: an inventory that does not carry the row cannot say the word.
/// A bundle assembled by `windows/worldgen` always carries all six, so the
/// refusal is reachable only from a caller that assembled a partial one.
fn tongue_pronoun(
    person: Person,
    number: Number,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<String, TongueGap> {
    let key = person.paradigm_key(number);
    pronouns
        .get(key)
        .map(|form| form.roman.clone())
        .ok_or_else(|| TongueGap {
            concept: format!("pronoun/{key}"),
            reason: format!("this tongue's pronoun inventory carries no {key} form"),
        })
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
///
/// **Refuses an [`Argument::Clause`] itself, by panic, before it ever
/// reaches [`resolve_argument`].** `Adjunct` holds an `Argument`, so an
/// adjunct carrying a clause type-checks; left to fall through to
/// `resolve_argument`'s own `Argument::Clause` arm, it would silently embed
/// the clause and mark its boundary exactly as the object slot does —
/// correct there, but the WRONG behavior here. This function's own message
/// names the real rule: adjuncts may not carry clauses at all, ever, tongue
/// or Common alike (spec §4.1) — see `clause.rs`'s `common_role_surface`
/// for Common's half of the same refusal.
fn realize_adjuncts(
    adjuncts: &[Adjunct],
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    number: Number,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<Vec<String>, TongueGap> {
    adjuncts
        .iter()
        .map(|adjunct| {
            if let Argument::Clause(_) = &adjunct.argument {
                panic!(
                    "an adjunct may not carry an embedded clause (role {:?}): \
                     adverbial subordination is a separate construction, spec §4.1",
                    adjunct.role
                );
            }
            resolve_argument(&adjunct.argument, grammar, lexicon, number, pronouns)
        })
        .collect()
}

/// The subject's surface text in a tongue.
///
/// A [`Subject::Pronoun`] resolves through `pronouns`, the tongue's own
/// drawn inventory, at the clause's own `number`. It carried an
/// unconditional [`TongueGap`] from The Scarf until The Inquest drew that
/// inventory; see [`tongue_pronoun`] for what replaced it and why deleting
/// the arm satisfies the rule rather than reversing it.
///
/// **No case is read here.** A tongue's inventory is person crossed with
/// number and nothing else (spec §4.5), so the subject and object slots ask
/// it the same question. Common's own nominative/accusative split lives in
/// `clause.rs` and stops at Common's edge — the asymmetry decision 0286
/// licenses.
///
/// **[`Subject::Clause`] realizes through [`realize_tongue`] (the FLOOR
/// realizer) and marks its boundary per [`mark_embedded_clause`]** (The
/// Mortise, Task 5) — the same design [`resolve_argument`]'s own
/// `Argument::Clause` arm states, for the same reason: `realize_tongue_deep`
/// special-cases `Subject::Clause` itself, before ever reaching this
/// function, so the inner clause's own morphology survives through a DEEP
/// recursive call rather than being flattened here. This arm is therefore
/// reached only through the shared `realize_tongue_with_subject` dispatch —
/// [`realize_tongue`]'s own direct call and
/// [`realize_tongue_coordination`]'s per-clause call (Task 7), both floor
/// callers — never from `realize_tongue_deep`'s subject slot. The depth
/// check runs BEFORE the recursive call, the same ordering
/// [`crate::clause::realize_common`] uses.
fn tongue_subject(
    subject: &Subject,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    number: Number,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<String, TongueGap> {
    match subject {
        Subject::Name(name) => Ok(name.clone()),
        Subject::Pronoun(person) => tongue_pronoun(*person, number, pronouns),
        Subject::Clause(inner) => {
            let depth = subject_embed_depth(subject);
            assert!(
                depth <= CLAUSE_EMBED_MAX_DEPTH,
                "a clause subject nests {depth} deep, past the cap of \
                 {CLAUSE_EMBED_MAX_DEPTH}: a clause bound to the subject \
                 slot may not itself contain a clause complement"
            );
            let text = realize_tongue(inner, grammar, lexicon, pronouns)?;
            Ok(mark_embedded_clause(grammar, text))
        }
    }
}

/// What a tongue must put in its verb slot for this clause -- and a
/// REFUSAL, by panic, for a predicate no realizer covers.
///
/// **Panics rather than gapping**, and the difference is the whole of spec
/// §3.3. A [`TongueGap`] asserts something TRUE ABOUT THE WORLD -- this
/// people has no word for the sea. A predicate outside the inventory is an
/// authoring hole in this repository, so reporting it as a gap would put a
/// false claim about a people into a rendered artifact. `realize_common`
/// panics on the same condition for the same reason.
///
/// **The tongue asks [`predicate_valence`], never Common's construction
/// table.** What it needs is what the predicate RELATES -- a subject to a
/// state, or an actor to a patient -- which decides whether the verb slot
/// belongs to the tongue's own drawn copula or to the predicate itself. That
/// is a fact about the predicate, true before any language says it, and
/// reading it out of Common's part list would have made the tongue path
/// depend on Common's spelling, which decision 0286 exists to prevent.
fn tongue_valence(predicate: &str) -> Valence {
    predicate_valence(predicate).unwrap_or_else(|| {
        panic!(
            "no tongue construction for predicate {predicate:?} \
             (no entry in the clause layer's predicate inventory)"
        )
    })
}

/// The tongue's verb slot for this clause, as a word mid-assembly.
///
/// **Nominal predication** puts the tongue's own drawn copula there, or
/// nothing at all for a zero-copula tongue -- unchanged from every campaign
/// before this one. **A transitive clause** puts the clause's own predicate
/// there, lexicalized through THIS TONGUE's lexicon exactly as the object is,
/// so a people with no word for the act gaps the whole clause (spec §4:
/// render fully or gap entirely) rather than borrowing Common's verb.
///
/// Returned as a [`Marked`] and built ONCE, because the tense, polarity and
/// evidential layers each join onto the segments the last one produced;
/// re-reading the drawn form (or re-resolving the lexicon entry)
/// mid-assembly would silently discard the join before it. That was a real
/// defect for the copula, fixed in this campaign's Task 2, and a lexical
/// verb inherits the same hazard.
fn tongue_verb(
    valence: Valence,
    clause: &Clause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
) -> Result<Option<Marked>, TongueGap> {
    match valence {
        Valence::Nominal => Ok(grammar.copula.as_ref().map(|roman| Marked {
            segments: grammar.copula_segments.clone(),
            roman: roman.clone(),
        })),
        Valence::Transitive => Ok(Some(resolve_concept_marked(&clause.predicate, lexicon)?)),
        // The verb slot is filled identically to the transitive case — the
        // clause's own predicate, lexicalized through this tongue's own
        // lexicon — and only the object differs. The ordering half (how a
        // tongue with a drawn constituent order places a verb with no
        // object) is Task 3's business, not this function's.
        Valence::Intransitive => Ok(Some(resolve_concept_marked(&clause.predicate, lexicon)?)),
    }
}

/// Realize a clause in a tongue: lexicalize the object, fill the verb slot
/// (the drawn copula for a nominal predication, the lexicalized predicate
/// for a transitive clause), order the constituents per the grammar, then
/// append each adjunct's own resolved word. Renders fully or gaps entirely
/// (spec §4) — a gap on the object, on the VERB, or on any adjunct concept
/// fails the whole clause.
///
/// **`pronouns` is the tongue's drawn personal-pronoun inventory** — the
/// same `BTreeMap` [`TongueMorphology::pronouns`] holds, passed separately
/// here because this function is the pre-morphology floor and a pronoun is
/// a free word, so it belongs to the floor rather than to a marking layer.
/// Pass an empty map for a call site that models no pronouns; a pronoun
/// subject or object then gaps, which is true of that inventory.
/// type-audit: bare-ok(prose)
pub fn realize_tongue(
    clause: &Clause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<String, TongueGap> {
    realize_tongue_with_subject(clause, grammar, lexicon, pronouns, true)
}

/// [`realize_tongue`]'s own body, widened with one caller-only knob:
/// whether to realize `clause.subject` at all. `realize_tongue` itself
/// always passes `true`, so its behaviour is unchanged byte for byte;
/// [`realize_tongue_coordination`] is the only caller that ever passes
/// `false`, for a coordinated clause whose subject is identical to the
/// LAST STATED subject before it (see
/// [`crate::clause::elide_coordinated_subjects`] for exactly what that
/// means and why it is not simply "the first clause", The Mortise, Task 7
/// fix round 1, spec §4.10's tier 2).
///
/// **Elision happens HERE, inside per-clause realization, never as
/// post-processing on an already-joined sentence.** [`join_coordinated`]
/// receives finished strings and has no idea where a subject sits within
/// one — the constituent order is drawn (`grammar.order`, six possible
/// shapes) and the subject lands first, middle or last depending on it.
/// So when `include_subject` is `false`, `tongue_subject` is never even
/// called: the subject slot is simply `None` in the `ordered` array below,
/// wherever this tongue's drawn order would otherwise have placed it —
/// there is no leftover token to find or trim, in any order.
fn realize_tongue_with_subject(
    clause: &Clause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    pronouns: &BTreeMap<&'static str, MorphForm>,
    include_subject: bool,
) -> Result<String, TongueGap> {
    let valence = tongue_valence(&clause.predicate);
    let subject = if include_subject {
        Some(tongue_subject(
            &clause.subject,
            grammar,
            lexicon,
            clause.number,
            pronouns,
        )?)
    } else {
        None
    };
    let complement = resolve_argument(&clause.object, grammar, lexicon, clause.number, pronouns)?;
    let verb = tongue_verb(valence, clause, grammar, lexicon)?.map(|marked| marked.roman);
    let adjunct_words =
        realize_adjuncts(&clause.adjuncts, grammar, lexicon, clause.number, pronouns)?;
    let s = subject.as_deref();
    let v = verb.as_deref();
    let o = complement.as_str();
    // Order the present constituents; an absent verb (a zero-copula tongue
    // predicating nominally) simply drops out, exactly as an elided subject
    // now does. A transitive clause always fills the verb slot, so all six
    // orders emit three tokens with a subject present and two without one.
    let ordered: Vec<&str> = match grammar.order {
        ConstituentOrder::Sov => [s, Some(o), v],
        ConstituentOrder::Svo => [s, v, Some(o)],
        ConstituentOrder::Vso => [v, s, Some(o)],
        ConstituentOrder::Vos => [v, Some(o), s],
        ConstituentOrder::Ovs => [Some(o), v, s],
        ConstituentOrder::Osv => [Some(o), s, v],
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

/// Trim each coordinated clause's own trailing period and join with the
/// tongue's drawn `conjunction` — or, for a tongue that drew none, simple
/// juxtaposition (a space, no marker): a legitimate coordination strategy
/// and not a gap (spec §4.10), the same argument [`mark_embedded_clause`]
/// makes for a tongue that drew no subordinator. The whole utterance takes
/// exactly one trailing period.
///
/// **Shared by [`realize_tongue_coordination`] (the floor realizer) and
/// [`realize_tongue_deep_coordination`] (C7's full morphology bundle).**
/// The two differ only in which per-clause realizer produced `texts` —
/// [`realize_tongue`] or [`realize_tongue_deep`] — never in how the
/// results are joined, so the join itself is written once.
fn join_coordinated(mut texts: Vec<String>, conjunction: Option<&str>) -> String {
    for text in &mut texts {
        if text.ends_with('.') {
            text.pop();
        }
    }
    let separator = match conjunction {
        Some(word) => format!(" {word} "),
        None => " ".to_string(),
    };
    let mut out = texts.join(&separator);
    out.push('.');
    out
}

/// Realize a [`Coordination`] in a tongue: each clause realizes through
/// [`realize_tongue_with_subject`] (the FLOOR realizer — the same
/// deliberate choice [`tongue_subject`]'s and [`resolve_argument`]'s own
/// `Clause` arms make for embedding, spec §4.5's floor/deep split), then
/// joined via [`join_coordinated`].
///
/// **Tier 2: a clause whose `(subject, number)` equals the LAST STATED
/// subject before it is realized WITHOUT its subject constituent** — see
/// [`realize_tongue_with_subject`] for where that omission actually
/// happens (inside per-clause realization, never as surgery on the joined
/// text), [`elide_coordinated_subjects`] (the SAME decision function
/// [`crate::clause::realize_common_coordination`] uses — one rule, not
/// three copies) for exactly what "last stated" means and why comparing
/// only to the first clause misattributes a 3+-clause coordination, and
/// that function's doc for why the comparison is `(subject, number)` and
/// not bare [`Subject`] equality. **An elided clause never calls
/// [`tongue_subject`] at all** — since `grammar`/`lexicon`/`pronouns` are
/// the same across every clause in one coordination and the compared
/// subject+number are equal by construction, re-deriving the omitted
/// clause's subject would be a deterministic no-op: it can only reproduce
/// the `Ok` text the matching, unskipped call already produced, so
/// skipping it costs nothing and cannot mask a gap that call would not
/// already have surfaced.
///
/// **Not the production path** — see [`realize_tongue_deep_coordination`]
/// for the one `windows/book`/`windows/almanac` actually need, where each
/// coordinated clause keeps its own drawn tense/polarity/evidential/
/// noun-class marking. This floor entry point exists for the same reason
/// bare [`realize_tongue`] does: a shallow, morphology-free baseline, kept
/// for testing and for a caller that genuinely wants it, not because any
/// production narration calls it today.
///
/// Renders fully or gaps entirely (spec §4): a gap realizing any one
/// coordinated clause fails the whole coordination, via `?` on
/// [`realize_tongue_with_subject`]'s own `Result`.
///
/// Panics if `coord.clauses` holds fewer than two clauses, on the identical
/// reasoning [`crate::clause::realize_common_coordination`] states.
/// type-audit: bare-ok(prose)
pub fn realize_tongue_coordination(
    coord: &Coordination,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
    pronouns: &BTreeMap<&'static str, MorphForm>,
) -> Result<String, TongueGap> {
    assert!(
        coord.clauses.len() >= 2,
        "a coordination joins at least two clauses; {} is not a list to \
         coordinate",
        coord.clauses.len()
    );
    let elisions = elide_coordinated_subjects(&coord.clauses);
    let mut texts = Vec::with_capacity(coord.clauses.len());
    for (clause, elide) in coord.clauses.iter().zip(elisions) {
        texts.push(realize_tongue_with_subject(
            clause, grammar, lexicon, pronouns, !elide,
        )?);
    }
    Ok(join_coordinated(texts, grammar.conjunction.as_deref()))
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
/// insertion can find "the complement" or "the predicate" (the verb, or the
/// complement itself when the verb slot is empty) regardless of the tongue's
/// drawn constituent order. `Marker` tags a spliced-in particle so it is
/// never mistaken for one of the three base roles by a later splice.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Role {
    /// The clause's subject.
    Subject,
    /// The verb slot's token, when filled: the tongue's drawn copula in a
    /// nominal predication, the lexicalized predicate in a transitive
    /// clause. **Named `Copula` until The Inquest**, when the slot stopped
    /// being the copula's alone — `ConstituentOrder`'s `Sov`/`Svo`/… always
    /// meant a V the copula merely stood in for.
    Verb,
    /// The object slot's token: the predicate nominal in a nominal
    /// predication, the patient in a transitive clause.
    Complement,
    /// A spliced-in particle marker (evidential, tense or noun-class).
    Marker,
}

/// A tongue's drawn paradigm bundle: its [`ParadigmDepths`] (how deeply
/// Number and Tense grammaticalize, and which side each affix binds)
/// together with the family's marker forms for each axis's MARKED member,
/// already evolved into this daughter — the paradigm sibling of
/// [`TongueMorphology`], and what [`realize_tongue_deep`] consumes when a
/// caller models tense.
///
/// **This is the first consumer `paradigm.rs` has ever had.** Its whole
/// public surface shipped drawn and unread; that is why the depths are
/// carried here by composition rather than duplicated as fields —
/// `paradigm.rs` stays the one place they are defined and drawn.
///
/// Each axis map is keyed by the MARKED value's label — `tense` by
/// `"past"`, `polarity` by `"negative"` — because the other member of each
/// pair is the zero member (spec §4.1) and no marker is drawn for it. A
/// bundle missing a key degrades to "no marking on that axis" rather than
/// panicking, exactly as [`TongueMorphology`]'s own marker maps do, which is
/// what lets a synthetic fixture supply one axis and leave the other empty.
///
/// **A new paradigm axis extends this bundle rather than adding a parameter
/// to [`realize_tongue_deep`]** — the stated reason the depths were bundled
/// here in the first place instead of passed as a signature full of options.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueParadigm {
    /// The tongue's drawn Number/Tense/Polarity depths and attachment sides.
    pub depths: ParadigmDepths,
    /// The family's tense marker forms, keyed by marked value (`"past"`).
    pub tense: BTreeMap<&'static str, MorphForm>,
    /// The family's polarity marker forms, keyed by marked value
    /// (`"negative"`).
    pub polarity: BTreeMap<&'static str, MorphForm>,
}

/// The marked member of the tense axis for `tense`, or `None` when the
/// clause's tense is the zero member. Present is unmarked (spec §4.1): only
/// `past` is drawn, and inventing a present marker would be authoring.
fn marked_tense_value(tense: Tense) -> Option<&'static str> {
    match tense {
        Tense::Present => None,
        Tense::Past => Some("past"),
    }
}

/// The marked member of the polarity axis, or `None` when the clause is
/// positive. Negative is the marked member and positive is zero — the same
/// shape tense has, and for the same reason: only `negative` is drawn, and
/// inventing a positive marker would be authoring.
fn marked_polarity_value(polarity: Polarity) -> Option<&'static str> {
    match polarity {
        Polarity::Pos => None,
        Polarity::Neg => Some("negative"),
    }
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
/// Tense marking (The Inquest) reads `paradigm`'s drawn `tense_depth` and
/// `tense_position`. **Past marks the verb** (spec §4.2) — the LEXICAL verb
/// in a transitive clause, the copula in a nominal one; under a ZERO COPULA
/// and a nominal predication it falls to the predicate nominal, exactly as
/// the evidential already does in that position. A transitive clause always
/// fills the verb slot, so the fallback cannot fire there: the reason
/// transitivity is in this campaign at all is that a tense marker had no
/// host until it existed.
/// Present is the zero member and is never marked. `paradigm` is `None` for
/// a caller that does not model tense at all, which reproduces this
/// function's pre-Inquest surface byte for byte — as does a `Some` bundle
/// whose `tense_depth` is `MorphDepth::None`.
///
/// Polarity marking (The Inquest) reads `paradigm`'s drawn `polarity_depth`
/// and `polarity_position` and targets the same host tense does — the
/// copula, or the predicate nominal under a zero copula. **Negative is the
/// marked member and positive is zero**, exactly as past/present are on the
/// tense axis: only a negative marker is drawn, and inventing a positive one
/// would be authoring.
///
/// The tense layer is applied first, then polarity, then the evidential —
/// the evidential last so it stays predicate-FINAL, which is the rule its
/// own doc above states.
///
/// `orth` is the tongue's own [`crate::phonology::Phonology::orthography`]
/// (spec §3.6): a VIEW, so it changes only the `Affix`-depth marker joins'
/// spelling, never `morph`/`grammar`/`lexicon` or which draw fired.
/// type-audit: bare-ok(prose)
pub fn realize_tongue_deep(
    clause: &Clause,
    grammar: &TongueGrammar,
    morph: &TongueMorphology,
    paradigm: Option<&TongueParadigm>,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &Lexicon,
    orth: Orthography,
) -> Result<String, TongueGap> {
    realize_tongue_deep_with_subject(
        clause,
        grammar,
        morph,
        paradigm,
        noun_class_of,
        lexicon,
        orth,
        true,
    )
}

/// [`realize_tongue_deep`]'s own body, widened with one caller-only knob:
/// whether to realize `clause.subject` at all. `realize_tongue_deep` itself
/// always passes `true`, so its behaviour is unchanged byte for byte;
/// [`realize_tongue_deep_coordination`] is the only caller that ever passes
/// `false`, for a coordinated clause whose subject is identical to the
/// LAST STATED subject before it (see
/// [`crate::clause::elide_coordinated_subjects`], and note it is NOT
/// simply "the first clause", The Mortise, Task 7 fix round 1) — the
/// same knob [`realize_tongue_with_subject`] gives the floor realizer, kept
/// on the deep path too so elision does not re-open the floor/deep
/// asymmetry Task 6's fix round closed for coordination itself.
///
/// **When `include_subject` is `false`, `clause.subject` is never even
/// matched on** — no `tongue_subject` call and no recursive
/// `realize_tongue_deep` call for a `Subject::Clause`, so an elided
/// subject costs nothing beyond the one comparison
/// [`realize_tongue_deep_coordination`] already made to decide to elide it.
///
/// `#[allow(clippy::too_many_arguments)]`: the bundle is exactly
/// [`realize_tongue_deep`]'s own seven arguments plus this function's one
/// caller-only `include_subject` knob — the same widen-in-place shape
/// [`realize_tongue_with_subject`] takes over [`realize_tongue`]'s four.
#[allow(clippy::too_many_arguments)]
fn realize_tongue_deep_with_subject(
    clause: &Clause,
    grammar: &TongueGrammar,
    morph: &TongueMorphology,
    paradigm: Option<&TongueParadigm>,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &Lexicon,
    orth: Orthography,
    include_subject: bool,
) -> Result<String, TongueGap> {
    let valence = tongue_valence(&clause.predicate);
    // The pronoun inventory rides on `morph` (spec §4.6): it is drawn by the
    // same family-cognate machinery every other form in that bundle is, and
    // `windows/worldgen`'s `tongue_morphology_of` fills it, so every
    // production caller of this function has one.
    let pronouns = &morph.pronouns;
    // `Subject::Clause` is special-cased HERE rather than inside
    // `tongue_subject`, so the inner clause realizes through a DEEP
    // recursive call — threading the SAME `morph`/`paradigm`/`noun_class_of`/
    // `orth` this call was given, never rebuilding them — and so its own
    // evidential/tense/polarity marking survives (spec §4.5, The Mortise
    // Task 5). `tongue_subject`'s own `Subject::Clause` arm handles only the
    // FLOOR path (`realize_tongue`'s direct dispatch). The depth check runs
    // BEFORE the recursive call, the same ordering `realize_common` uses.
    let subject = if include_subject {
        Some(match &clause.subject {
            Subject::Clause(inner) => {
                let depth = subject_embed_depth(&clause.subject);
                assert!(
                    depth <= CLAUSE_EMBED_MAX_DEPTH,
                    "a clause subject nests {depth} deep, past the cap of \
                     {CLAUSE_EMBED_MAX_DEPTH}: a clause bound to the subject \
                     slot may not itself contain a clause complement"
                );
                let text = realize_tongue_deep(
                    inner,
                    grammar,
                    morph,
                    paradigm,
                    noun_class_of,
                    lexicon,
                    orth,
                )?;
                mark_embedded_clause(grammar, text)
            }
            other => tongue_subject(other, grammar, lexicon, clause.number, pronouns)?,
        })
    } else {
        None
    };
    // Spec §4.3: only a LEXICAL object may bear morphology. `object_concept`
    // is `Some` only for a `Concept` -- and it must not be conflated with
    // `Marked.segments == None`, which a `Compound` also has. A `Compound`'s
    // missing segments are a lexicon BUG that `layer_affix` panics on, on
    // purpose; a numeral has no segments BY NATURE and there is nothing to
    // fix. Guarding on the concept id keeps the panic reachable for the
    // first case while declining to affix in the second.
    //
    // `Argument::Clause` is special-cased the same way `Subject::Clause` is
    // above, for the identical reason: a DEEP recursive call, threading the
    // same assembled `morph`/`paradigm`/`noun_class_of`/`orth` through
    // unchanged, so the inner clause's own grounding survives (spec §4.5).
    let (mut complement, object_concept): (Marked, Option<String>) = match &clause.object {
        Argument::Concept(id) => (resolve_concept_marked(id, lexicon)?, Some(id.clone())),
        Argument::Clause(inner) => {
            let depth = clause_embed_depth(&clause.object);
            assert!(
                depth <= CLAUSE_EMBED_MAX_DEPTH,
                "a clause complement nests {depth} deep, past the cap of \
                 {CLAUSE_EMBED_MAX_DEPTH}: a clause complement may not \
                 itself contain a clause complement"
            );
            let text = realize_tongue_deep(
                inner,
                grammar,
                morph,
                paradigm,
                noun_class_of,
                lexicon,
                orth,
            )?;
            (
                Marked {
                    segments: None,
                    roman: mark_embedded_clause(grammar, text),
                },
                None,
            )
        }
        // The intransitive frame's object slot: no argument at all. Named
        // explicitly rather than left to the `other` catch-all below, on
        // purpose -- the catch-all would silently absorb it into an empty
        // `Marked`, ordered into the tongue's constituent sequence as an
        // empty complement, which is a WRONG surface (not a gap) for any
        // caller realizing an intransitive clause through this deep
        // realizer, since `predicate_valence` now answers `Intransitive`
        // for a real predicate (`SLEEP`). The value is the same empty
        // `Marked` the catch-all would have produced; what changes is that
        // the variant is named at the site instead of vanishing into a
        // wildcard, so the NEXT variant added to `Argument` cannot vanish
        // the same way. A later task gives the object slot its own
        // ordering-aware `Option` treatment in both orderings (this deep
        // realizer and the floor realizer's own dispatch) rather than an
        // empty placeholder; until then this arm is a correct no-op, not a
        // deferred fix.
        Argument::Absent => (
            Marked {
                segments: None,
                roman: String::new(),
            },
            None,
        ),
        other => (
            Marked {
                segments: None,
                roman: resolve_argument(other, grammar, lexicon, clause.number, pronouns)?,
            },
            None,
        ),
    };
    // The verb slot as a word mid-assembly, so the tense, polarity and
    // evidential layers below join onto the SAME segments in turn rather
    // than each re-reading the drawn copula or re-resolving the lexicon
    // entry. `None` only for a zero-copula tongue predicating nominally.
    // Resolved HERE, between the object and the adjuncts, so a transitive
    // clause whose verb AND object both gap reports the same one either
    // realizer would (the object's), keeping the shallow-identity guarantee
    // exact on the error path as well as the success path.
    let mut verb = tongue_verb(valence, clause, grammar, lexicon)?;
    let adjunct_words =
        realize_adjuncts(&clause.adjuncts, grammar, lexicon, clause.number, pronouns)?;

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

    // Tense marking (The Inquest): past marks the VERB — the lexical verb of
    // a transitive clause, or the copula of a nominal one; with neither (a
    // zero-copula tongue predicating nominally) it falls to the predicate
    // nominal (spec §4.2), the same host the evidential falls to there.
    // Present is the zero member and draws no marker at all (spec §4.1).
    // Applied BEFORE the evidential layer so the evidential stays
    // predicate-final.
    let mut tense_particle: Option<String> = None;
    if let Some(paradigm) = paradigm
        && let Some(tense_value) = marked_tense_value(clause.tense)
        && let Some(marker) = paradigm.tense.get(tense_value)
    {
        let position = paradigm.depths.tense_position;
        match paradigm.depths.tense_depth {
            MorphDepth::None => {}
            MorphDepth::Affix => {
                if let Some(host) = verb.take() {
                    verb = Some(layer_affix(host, marker, position, orth));
                } else if object_concept.is_some() {
                    // No verb slot (a zero-copula NOMINAL clause; a
                    // transitive one always fills it): the predicate nominal
                    // bears it. A non-lexical object (`Name`/`Count`/
                    // `Quantity`) has no segments BY NATURE, so the clause
                    // goes unmarked for tense -- the same outcome a tongue
                    // that drew `MorphDepth::None` already has. The guard is
                    // on the CONCEPT ID, never on `Marked.segments`, because
                    // a `Compound` has no segments either and that is a
                    // lexicon BUG `layer_affix` must keep panicking on
                    // (spec §4.3).
                    complement = layer_affix(complement, marker, position, orth);
                }
            }
            MorphDepth::Particle => tense_particle = Some(marker.roman.clone()),
        }
    }

    // Polarity marking (The Inquest): negation marks the same host tense
    // does -- the verb slot, falling to the predicate nominal when a
    // nominal clause has no copula. Positive is the zero member and draws no marker
    // at all. Applied AFTER the tense layer (so an affixed negative sits
    // outside an affixed tense marker) and BEFORE the evidential one (so
    // the evidential stays predicate-final).
    let mut polarity_particle: Option<String> = None;
    if let Some(paradigm) = paradigm
        && let Some(polarity_value) = marked_polarity_value(clause.polarity)
        && let Some(marker) = paradigm.polarity.get(polarity_value)
    {
        let position = paradigm.depths.polarity_position;
        match paradigm.depths.polarity_depth {
            MorphDepth::None => {}
            MorphDepth::Affix => {
                if let Some(host) = verb.take() {
                    verb = Some(layer_affix(host, marker, position, orth));
                } else if object_concept.is_some() {
                    // No verb slot (a zero-copula NOMINAL clause): the
                    // predicate nominal bears it. The guard is on the
                    // CONCEPT ID, never on
                    // `Marked.segments`, for the same reason the tense layer
                    // above states -- a `Compound` has no segments either and
                    // `layer_affix`'s panic on it is deliberate (spec §4.3).
                    complement = layer_affix(complement, marker, position, orth);
                }
            }
            MorphDepth::Particle => polarity_particle = Some(marker.roman.clone()),
        }
    }

    // Evidential marking: predicate-final — the verb slot's token, or (when
    // it is empty) the predicate nominal, i.e. the (possibly already
    // class-marked) complement.
    let evidential_value = match clause.evidential {
        Evidential::Witnessed => "witnessed",
        Evidential::Taught => "taught",
        Evidential::Inferred => "inferred",
    };
    let mut evidential_particle: Option<String> = None;
    if let Some(marker) = morph.evidential.get(evidential_value) {
        match morph.evidential_depth {
            MorphDepth::None => {}
            MorphDepth::Affix => {
                if let Some(host) = verb.take() {
                    verb = Some(layer_affix(host, marker, ClassPosition::Suffix, orth));
                } else if object_concept.is_some() {
                    // No verb slot: the marker falls to the predicate nominal.
                    // A non-lexical object cannot bear it, so the clause goes
                    // unmarked for evidentiality -- the same outcome a tongue
                    // that drew `MorphDepth::None` already has (spec §4.3).
                    complement = layer_affix(complement, marker, ClassPosition::Suffix, orth);
                }
            }
            MorphDepth::Particle => evidential_particle = Some(marker.roman.clone()),
        }
    }
    let verb_roman = verb.map(|marked| marked.roman);

    // `s` is `None` for an elided subject (`include_subject: false`) — the
    // same "absent constituent simply drops out" shape an absent verb
    // (a zero-copula tongue predicating nominally) already has below.
    let s = subject.as_deref();
    let s_tok = s.map(|s| (Role::Subject, s.to_string()));
    let v = verb_roman.as_deref();
    // "The predicate" for every particle splice below: the verb slot's token
    // when it is filled, otherwise the predicate nominal. Computed ONCE --
    // it used to be re-derived from `grammar.copula` at each of the three
    // splices, which is the same fact three times and would have had to be
    // widened three times now that a transitive clause fills the slot
    // without a drawn copula.
    let predicate_role = if v.is_some() {
        Role::Verb
    } else {
        Role::Complement
    };
    let o = complement.roman.as_str();
    let mut ordered: Vec<(Role, String)> = match grammar.order {
        ConstituentOrder::Sov => [
            s_tok.clone(),
            Some((Role::Complement, o.to_string())),
            v.map(|v| (Role::Verb, v.to_string())),
        ],
        ConstituentOrder::Svo => [
            s_tok.clone(),
            v.map(|v| (Role::Verb, v.to_string())),
            Some((Role::Complement, o.to_string())),
        ],
        ConstituentOrder::Vso => [
            v.map(|v| (Role::Verb, v.to_string())),
            s_tok.clone(),
            Some((Role::Complement, o.to_string())),
        ],
        ConstituentOrder::Vos => [
            v.map(|v| (Role::Verb, v.to_string())),
            Some((Role::Complement, o.to_string())),
            s_tok.clone(),
        ],
        ConstituentOrder::Ovs => [
            Some((Role::Complement, o.to_string())),
            v.map(|v| (Role::Verb, v.to_string())),
            s_tok.clone(),
        ],
        ConstituentOrder::Osv => [
            Some((Role::Complement, o.to_string())),
            s_tok,
            v.map(|v| (Role::Verb, v.to_string())),
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

    // Splice in the tense particle beside "the predicate" (`predicate_role`
    // above: the verb slot's token, or the predicate nominal when that slot
    // is empty) on the drawn side. Spliced BEFORE the evidential particle so
    // the evidential still lands immediately after the predicate and this
    // one sits one further out.
    if let Some(particle) = tense_particle
        && let Some(paradigm) = paradigm
        && let Some(idx) = ordered.iter().position(|(r, _)| *r == predicate_role)
    {
        let insert_at = match paradigm.depths.tense_position {
            ClassPosition::Prefix => idx,
            ClassPosition::Suffix => idx + 1,
        };
        ordered.insert(insert_at, (Role::Marker, particle));
    }

    // Splice in the polarity particle beside "the predicate" on its own
    // drawn side, after the tense particle and before the evidential one —
    // the same ordering rule the tense splice above states, extended by one
    // layer: each later splice lands nearer the predicate, so the evidential
    // stays immediately adjacent to it and this one sits just outside.
    if let Some(particle) = polarity_particle
        && let Some(paradigm) = paradigm
        && let Some(idx) = ordered.iter().position(|(r, _)| *r == predicate_role)
    {
        let insert_at = match paradigm.depths.polarity_position {
            ClassPosition::Prefix => idx,
            ClassPosition::Suffix => idx + 1,
        };
        ordered.insert(insert_at, (Role::Marker, particle));
    }

    // Splice in the evidential particle immediately after "the predicate":
    // the verb slot's token when it is filled, else the complement (the
    // predicate nominal of a zero-copula nominal clause). Tagging spliced
    // tokens `Role::Marker` above means this search still finds the right
    // anchor even after the class-particle splice shifted later indices.
    if let Some(particle) = evidential_particle
        && let Some(idx) = ordered.iter().position(|(r, _)| *r == predicate_role)
    {
        ordered.insert(idx + 1, (Role::Marker, particle));
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

/// Realize a [`Coordination`] under C7's full morphology bundle: each
/// coordinated clause realizes through [`realize_tongue_deep_with_subject`]
/// — a DEEP call threading the SAME `grammar`/`morph`/`paradigm`/
/// `noun_class_of`/`orth` this call was given, never rebuilding them, so
/// each clause's own drawn tense/polarity/evidential/noun-class marking
/// survives exactly as `realize_tongue_deep`'s own `Subject::Clause`/
/// `Argument::Clause` arms already thread it for embedding (The Mortise,
/// Task 5) — then joined via [`join_coordinated`].
///
/// **Tier 2: a clause whose `(subject, number)` equals the LAST STATED
/// subject before it is realized WITHOUT its subject constituent**, via
/// the SAME [`elide_coordinated_subjects`] decision
/// [`realize_tongue_coordination`] uses — one rule, shared by all three
/// coordination realizers, not three copies that could silently disagree
/// (see that function's doc for why "last stated" and not "the first
/// clause": comparing only to the first clause misattributes a 3+-clause
/// coordination like `[X, Y, X]`, fix round 1). Landing elision in the floor entry point
/// alone would re-open the exact floor/deep asymmetry Task 6's fix round
/// closed for coordination itself (that fix round is why this function
/// exists at all): a coordinated sentence would elide its subject only
/// when nobody's narration actually renders it, since `windows/book`/
/// `windows/almanac` call this function, never the floor one, for real
/// output.
///
/// **This is the production path.** `windows/book` and `windows/almanac`
/// call only `realize_tongue_deep`, never bare [`realize_tongue`], for
/// real narration; a coordination realized only through
/// [`realize_tongue_coordination`] (the floor entry point) would render
/// with no morphological marking in the one place a real narration
/// actually renders. This function closes that gap (fix round 1, Task 6).
///
/// **Coordination reads and writes nothing about any one clause's own
/// marking.** Each coordinated clause's tense/polarity/evidential is its
/// own, computed by its own `realize_tongue_deep` call exactly as it would
/// be realized alone; joining the results changes none of them — the same
/// non-interference [`realize_tongue_deep`]'s embedding arms already
/// guarantee for a nested clause. Eliding a subject changes nothing about
/// this either: the elided clause's own tense/polarity/evidential/noun-class
/// marking on its VERB and OBJECT still applies in full, exactly as if its
/// subject had been realized and then discarded — the difference is that
/// discarding happens before the marking work, not after it, so no marking
/// is ever computed and then thrown away.
///
/// Renders fully or gaps entirely (spec §4): a gap realizing any one
/// coordinated clause fails the whole coordination, via `?` on
/// [`realize_tongue_deep_with_subject`]'s own `Result`.
///
/// Panics if `coord.clauses` holds fewer than two clauses, on the
/// identical reasoning [`crate::clause::realize_common_coordination`]
/// states.
/// type-audit: bare-ok(prose)
pub fn realize_tongue_deep_coordination(
    coord: &Coordination,
    grammar: &TongueGrammar,
    morph: &TongueMorphology,
    paradigm: Option<&TongueParadigm>,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &Lexicon,
    orth: Orthography,
) -> Result<String, TongueGap> {
    assert!(
        coord.clauses.len() >= 2,
        "a coordination joins at least two clauses; {} is not a list to \
         coordinate",
        coord.clauses.len()
    );
    let elisions = elide_coordinated_subjects(&coord.clauses);
    let mut texts = Vec::with_capacity(coord.clauses.len());
    for (clause, elide) in coord.clauses.iter().zip(elisions) {
        texts.push(realize_tongue_deep_with_subject(
            clause,
            grammar,
            morph,
            paradigm,
            noun_class_of,
            lexicon,
            orth,
            !elide,
        )?);
    }
    Ok(join_coordinated(texts, grammar.conjunction.as_deref()))
}

#[cfg(test)]
mod tests {
    use super::*;
    // Test-only: `Number` and `Definiteness` reach a tongue realizer and go
    // UNREAD (spec 3.2), so no non-test code in this module names them.
    // Importing them at module level would be an unused import outside
    // `cfg(test)`. `Tense` and `Polarity` are NOT among them any more --
    // The Inquest made the realizer read both, so they are imported at
    // module level via `use super::*`.
    use crate::clause::{Definiteness, Number, PRONOUN_PARADIGM};
    use crate::etymology::CascadeRegime;
    use crate::lexicon::{ExposureClass, LexEntry, build_lexicon};
    use crate::naming::render_views;
    use crate::packs::{EAT, KILL, KNOW};
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use hornvale_kernel::world::IS_A;

    /// An EMPTY pronoun inventory, for the many tests whose clauses have a
    /// `Subject::Name` and no pronoun anywhere: what a tongue's pronouns are
    /// cannot affect them, and passing nothing says so.
    fn no_pronouns() -> BTreeMap<&'static str, MorphForm> {
        BTreeMap::new()
    }

    /// A REAL six-row pronoun inventory, drawn the way `windows/worldgen`
    /// draws one — `pronoun_forms` off a family label and a phonology — so a
    /// test that realizes a pronoun realizes a form the world would actually
    /// produce, never an authored placeholder.
    fn drawn_pronouns() -> BTreeMap<&'static str, MorphForm> {
        let ph = test_phonology();
        crate::morphology::pronoun_forms(
            &Seed(42),
            "goblinoid",
            &ph,
            &crate::etymology::draw_cascade(&Seed(42), "goblin", &ph),
            &ph,
        )
    }

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
        // `grammar` is read only by this function's `Argument::Clause` arm
        // (unreached by any case here), so which grammar is passed is
        // immaterial to this test.
        let grammar = svo_with_copula();
        // A concept goes through the lexicon; everything else is passed through
        // or rendered at the tongue's own grain. This is what `realize_adjuncts`
        // has always done -- naming it is what lets the OBJECT slot do it too.
        assert_eq!(
            resolve_argument(
                &Argument::Name("Nwamvam".to_string()),
                &grammar,
                &lex,
                Number::Sg,
                &no_pronouns()
            )
            .unwrap(),
            "Nwamvam"
        );
        assert_eq!(
            resolve_argument(
                &Argument::Count(8835),
                &grammar,
                &lex,
                Number::Sg,
                &no_pronouns()
            )
            .unwrap(),
            "8835"
        );
        assert_eq!(
            resolve_argument(
                &Argument::Quantity(1.5),
                &grammar,
                &lex,
                Number::Sg,
                &no_pronouns()
            )
            .unwrap(),
            "1.5"
        );
        // A concept with no entry gaps, and the gap names the concept.
        let gap = resolve_argument(
            &Argument::Concept("no-such-concept".to_string()),
            &grammar,
            &lex,
            Number::Sg,
            &no_pronouns(),
        )
        .unwrap_err();
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let svo = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        assert_eq!(
            realize_tongue(&clause, &svo, &lex, &no_pronouns()).unwrap(),
            format!("Vavako gha {word}.")
        );
        let sov = TongueGrammar {
            order: ConstituentOrder::Sov,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        assert_eq!(
            realize_tongue(&clause, &sov, &lex, &no_pronouns()).unwrap(),
            format!("Vavako {word} gha.")
        );
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        assert_eq!(
            realize_tongue(&clause, &zero_copula, &lex, &no_pronouns()).unwrap(),
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        let gap = realize_tongue(&clause, &g, &lex, &no_pronouns()).unwrap_err();
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        let gap = realize_tongue(&clause, &g, &lex, &no_pronouns()).unwrap_err();
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
            subordinator: None,
            conjunction: None,
        };
        let out =
            realize_tongue(&clause, &g, &lex, &no_pronouns()).expect("both concepts are known");
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
            subordinator: None,
            conjunction: None,
        };
        let gap = realize_tongue(&clause, &g, &lex, &no_pronouns()).unwrap_err();
        assert_eq!(gap.concept, "yellow-white-dwarf");
        assert!(!gap.reason.is_empty(), "recountable reason required");
    }

    /// `realize_adjuncts` refuses a clause-carrying adjunct itself, with the
    /// real rule, rather than letting it fall through to
    /// `resolve_argument`'s own `Argument::Clause` arm — which would embed
    /// it silently, correct for the object slot and the WRONG behavior here
    /// (spec §4.1).
    #[test]
    #[should_panic(expected = "adjunct may not carry an embedded clause")]
    fn a_tongue_adjunct_carrying_a_clause_is_refused() {
        let lex = tiny_lexicon_with(&[("planet", ExposureClass::Steeped)]);
        let embedded = Clause {
            predicate: crate::packs::KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Clause(Box::new(embedded)),
            }],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        let _ = realize_tongue(&clause, &g, &lex, &no_pronouns());
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
            subordinator: None,
            conjunction: None,
        };
        let shallow = TongueMorphology {
            pronouns: drawn_pronouns(),
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: BTreeMap::new(),
            class: BTreeMap::new(),
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let floor = realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap();
        let deep = realize_tongue_deep(
            &clause,
            &grammar,
            &shallow,
            None,
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
                subordinator: None,
                conjunction: None,
            };
            assert_eq!(
                realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap(),
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
            pronouns: drawn_pronouns(),
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
                None,
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap(),
            realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap(),
            "MorphDepth::None on both axes must reproduce the C3 floor surface exactly"
        );

        // 2. Affix evidential (overt copula) -> the witnessed marker
        // appears predicate-finally, suffixed onto the copula at the
        // SEGMENT level.
        let affix_evidential = TongueMorphology {
            pronouns: drawn_pronouns(),
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
            None,
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
            pronouns: drawn_pronouns(),
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
            None,
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
            pronouns: drawn_pronouns(),
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
            None,
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
            subordinator: grammar.subordinator.clone(),
            conjunction: grammar.conjunction.clone(),
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
            None,
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

    /// A `TongueParadigm` at `tense_depth`, with a synthetic `"past"`
    /// marker drawn from `ph` — the paradigm sibling of the marker fixtures
    /// `realize_tongue_marks_by_depth` builds, and built the same way (a
    /// real drawn word via `proto_root`, not authored text). Every OTHER
    /// axis is `None` with an empty marker map, so a surface difference can
    /// only come from tense — a fixture that marked a second axis would be
    /// claiming coverage this test has not got.
    fn tense_paradigm(
        ph: &Phonology,
        tense_depth: MorphDepth,
        tense_position: ClassPosition,
    ) -> (TongueParadigm, Vec<Segment>, String) {
        use crate::etymology::proto_root;

        let past_segments = proto_root(&Seed(96), "goblin", "past-marker", ph);
        let past_roman = render_views(&past_segments).roman;
        let mut tense = BTreeMap::new();
        tense.insert(
            "past",
            MorphForm {
                segments: past_segments.clone(),
                roman: past_roman.clone(),
            },
        );
        (
            TongueParadigm {
                depths: ParadigmDepths {
                    number_depth: MorphDepth::None,
                    tense_depth,
                    polarity_depth: MorphDepth::None,
                    number_position: ClassPosition::Suffix,
                    tense_position,
                    polarity_position: ClassPosition::Suffix,
                },
                tense,
                polarity: BTreeMap::new(),
            },
            past_segments,
            past_roman,
        )
    }

    /// The morphology bundle that marks nothing — real marker maps left
    /// empty so a surface difference can only come from the paradigm.
    fn unmarked_morphology() -> TongueMorphology {
        TongueMorphology {
            pronouns: drawn_pronouns(),
            evidential_depth: MorphDepth::None,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: BTreeMap::new(),
            class: BTreeMap::new(),
        }
    }

    /// The Inquest T2: the tongue READS its drawn `tense_depth`.
    ///
    /// The assertion that makes this non-vacuous is DIFFERENTIAL, not an
    /// equality: two paradigms identical except for `tense_depth` must
    /// render the same PAST clause differently. An equality test alone
    /// would pass with the drawn field never read at all.
    #[test]
    fn realize_tongue_reads_its_drawn_tense_depth() {
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let complement_segments = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { derivation, .. } => derivation.modern.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let past = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let present = Clause {
            tense: Tense::Present,
            ..past.clone()
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let morph = unmarked_morphology();
        let grammar = overt_copula_grammar(&ph);

        let (none_paradigm, _, _) = tense_paradigm(&ph, MorphDepth::None, ClassPosition::Suffix);
        let (affix_paradigm, past_segments, past_roman) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);

        let render = |clause: &Clause, grammar: &TongueGrammar, paradigm: &TongueParadigm| {
            realize_tongue_deep(
                clause,
                grammar,
                &morph,
                Some(paradigm),
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap()
        };

        // 1. THE DIFFERENTIAL. Same clause, same tongue, same marker forms —
        // only the drawn depth differs, and the surface must differ with it.
        let unmarked = render(&past, &grammar, &none_paradigm);
        let marked = render(&past, &grammar, &affix_paradigm);
        assert_ne!(
            unmarked, marked,
            "a tongue that draws Affix tense depth must render a past clause \
             differently from one that draws None: {unmarked:?}"
        );

        // 2. `MorphDepth::None` on the tense axis is the shallow surface —
        // identical to passing no paradigm at all.
        assert_eq!(
            unmarked,
            realize_tongue_deep(
                &past,
                &grammar,
                &morph,
                None,
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap(),
            "tense_depth None must reproduce the no-paradigm surface exactly"
        );

        // 3. Affix + overt copula -> the past marker is joined onto the
        // COPULA at the segment level (spec §4.2: tense marks the verb, and
        // in a nominal clause the verb is the copula).
        let copula_segments = grammar
            .copula_segments
            .clone()
            .expect("overt_copula_grammar draws copula_segments alongside copula");
        let expected_copula = affix(
            &copula_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        assert!(
            marked.contains(&expected_copula),
            "Affix tense must suffix the past marker onto the copula: {marked:?} \
             (expected token {expected_copula:?})"
        );

        // 4. PRESENT IS UNMARKED (spec §4.1). The same Affix tongue renders
        // a present clause exactly as a None tongue does — only `past` is
        // drawn, and there is no present marker to invent.
        assert_eq!(
            render(&present, &grammar, &affix_paradigm),
            render(&present, &grammar, &none_paradigm),
            "present is the zero member: an Affix tongue must not mark it"
        );

        // 5. The drawn ATTACHMENT SIDE is read too, not assumed suffixing.
        let (prefix_paradigm, _, _) = tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Prefix);
        let expected_prefixed = affix(
            &copula_segments,
            &past_segments,
            ClassPosition::Prefix,
            Orthography::Digraph,
        )
        .roman;
        let prefixed = render(&past, &grammar, &prefix_paradigm);
        assert!(
            prefixed.contains(&expected_prefixed),
            "the drawn tense_position must decide the side: {prefixed:?} \
             (expected token {expected_prefixed:?})"
        );

        // 6. Particle depth -> a free word adjacent to the predicate (the
        // copula here), on the drawn side.
        let (particle_paradigm, _, _) =
            tense_paradigm(&ph, MorphDepth::Particle, ClassPosition::Suffix);
        let particled = render(&past, &grammar, &particle_paradigm);
        let tokens: Vec<&str> = particled.trim_end_matches('.').split(' ').collect();
        let copula_roman = grammar.copula.as_deref().unwrap();
        let copula_idx = tokens
            .iter()
            .position(|t| *t == copula_roman)
            .expect("the bare copula token must still be present, unmodified");
        assert_eq!(
            tokens.get(copula_idx + 1),
            Some(&past_roman.as_str()),
            "the tense particle must sit beside the predicate: {tokens:?}"
        );

        // 7. ZERO COPULA: no verb, so past falls to the predicate nominal —
        // the same host the evidential falls to in that position (spec §4.2).
        let zero_copula = TongueGrammar {
            order: grammar.order,
            copula: None,
            copula_segments: None,
            articles: grammar.articles,
            subordinator: grammar.subordinator.clone(),
            conjunction: grammar.conjunction.clone(),
        };
        let expected_enclitic = affix(
            &complement_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let zero_marked = render(&past, &zero_copula, &affix_paradigm);
        assert!(
            zero_marked.contains(&expected_enclitic),
            "zero-copula Affix tense must fall to the predicate nominal: \
             {zero_marked:?} (expected token {expected_enclitic:?})"
        );
    }

    /// The Inquest T2, spec §4.3's guard: a NON-LEXICAL object has no
    /// segments and must not be affixed — and the guard is on the concept
    /// id, never on `Marked.segments`, because a `LexEntry::Compound` has
    /// `segments: None` too and its `layer_affix` panic is deliberate
    /// (`layer_affix_panics_on_a_segmentless_word`). A zero-copula tongue
    /// with `Affix` tense has nowhere to put the marker here, so the clause
    /// goes unmarked for tense rather than panicking.
    #[test]
    fn a_non_lexical_object_takes_no_tense_affix_under_a_zero_copula() {
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Count(8835),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let morph = unmarked_morphology();
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        let (affix_paradigm, _, past_roman) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);

        let rendered = realize_tongue_deep(
            &clause,
            &zero_copula,
            &morph,
            Some(&affix_paradigm),
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .expect("a numeral object must still realize");
        assert_eq!(
            rendered, "Vavako 8835.",
            "a non-lexical object bears no tense affix: {rendered:?}"
        );
        assert!(
            !rendered.contains(&past_roman),
            "and the marker must not appear anywhere: {rendered:?}"
        );
    }

    /// A `TongueParadigm` at `polarity_depth`, with a synthetic
    /// `"negative"` marker drawn from `ph` — the polarity sibling of
    /// [`tense_paradigm`], built the same way (a real drawn word via
    /// `proto_root`, not authored text). Every other axis is `None`, so a
    /// surface difference can only come from polarity.
    fn polarity_paradigm(
        ph: &Phonology,
        polarity_depth: MorphDepth,
        polarity_position: ClassPosition,
    ) -> (TongueParadigm, Vec<Segment>, String) {
        use crate::etymology::proto_root;

        let neg_segments = proto_root(&Seed(95), "goblin", "negative-marker", ph);
        let neg_roman = render_views(&neg_segments).roman;
        let mut polarity = BTreeMap::new();
        polarity.insert(
            "negative",
            MorphForm {
                segments: neg_segments.clone(),
                roman: neg_roman.clone(),
            },
        );
        (
            TongueParadigm {
                depths: ParadigmDepths {
                    number_depth: MorphDepth::None,
                    tense_depth: MorphDepth::None,
                    polarity_depth,
                    number_position: ClassPosition::Suffix,
                    tense_position: ClassPosition::Suffix,
                    polarity_position,
                },
                tense: BTreeMap::new(),
                polarity,
            },
            neg_segments,
            neg_roman,
        )
    }

    /// The Inquest T3: the tongue READS its drawn `polarity_depth`.
    ///
    /// The assertion that makes this non-vacuous is DIFFERENTIAL, not an
    /// equality: two paradigms identical except for `polarity_depth` must
    /// render the same NEGATED clause differently. An equality test alone
    /// would pass with the drawn field never read at all.
    #[test]
    fn realize_tongue_reads_its_drawn_polarity_depth() {
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let complement_segments = match lex.entry("goblin-kind").unwrap() {
            LexEntry::Root { derivation, .. } => derivation.modern.clone(),
            other => panic!("goblin-kind should be a root, got {other:?}"),
        };
        let negated = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Neg,
            adjuncts: vec![],
        };
        let positive = Clause {
            polarity: Polarity::Pos,
            ..negated.clone()
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let morph = unmarked_morphology();
        let grammar = overt_copula_grammar(&ph);

        let (none_paradigm, _, _) = polarity_paradigm(&ph, MorphDepth::None, ClassPosition::Suffix);
        let (affix_paradigm, neg_segments, neg_roman) =
            polarity_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);

        let render = |clause: &Clause, grammar: &TongueGrammar, paradigm: &TongueParadigm| {
            realize_tongue_deep(
                clause,
                grammar,
                &morph,
                Some(paradigm),
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap()
        };

        // 1. THE DIFFERENTIAL. Same clause, same tongue, same marker forms —
        // only the drawn depth differs, and the surface must differ with it.
        let unmarked = render(&negated, &grammar, &none_paradigm);
        let marked = render(&negated, &grammar, &affix_paradigm);
        assert_ne!(
            unmarked, marked,
            "a tongue that draws Affix polarity depth must render a negated \
             clause differently from one that draws None: {unmarked:?}"
        );

        // 2. `MorphDepth::None` on the polarity axis is the shallow surface —
        // identical to passing no paradigm at all.
        assert_eq!(
            unmarked,
            realize_tongue_deep(
                &negated,
                &grammar,
                &morph,
                None,
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap(),
            "polarity_depth None must reproduce the no-paradigm surface exactly"
        );

        // 3. Affix + overt copula -> the negative marker is joined onto the
        // COPULA at the segment level (spec §4.2: like tense, polarity marks
        // the verb, and in a nominal clause the verb is the copula).
        let copula_segments = grammar
            .copula_segments
            .clone()
            .expect("overt_copula_grammar draws copula_segments alongside copula");
        let expected_copula = affix(
            &copula_segments,
            &neg_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        assert!(
            marked.contains(&expected_copula),
            "Affix polarity must suffix the negative marker onto the copula: \
             {marked:?} (expected token {expected_copula:?})"
        );

        // 4. POSITIVE IS UNMARKED (spec §4.1's zero-member rule, applied to
        // polarity). The same Affix tongue renders a positive clause exactly
        // as a None tongue does — only `negative` is drawn, and inventing a
        // positive marker would be authoring.
        assert_eq!(
            render(&positive, &grammar, &affix_paradigm),
            render(&positive, &grammar, &none_paradigm),
            "positive is the zero member: an Affix tongue must not mark it"
        );

        // 5. The drawn ATTACHMENT SIDE is read too, not assumed suffixing.
        let (prefix_paradigm, _, _) =
            polarity_paradigm(&ph, MorphDepth::Affix, ClassPosition::Prefix);
        let expected_prefixed = affix(
            &copula_segments,
            &neg_segments,
            ClassPosition::Prefix,
            Orthography::Digraph,
        )
        .roman;
        let prefixed = render(&negated, &grammar, &prefix_paradigm);
        assert!(
            prefixed.contains(&expected_prefixed),
            "the drawn polarity_position must decide the side: {prefixed:?} \
             (expected token {expected_prefixed:?})"
        );

        // 6. Particle depth -> a free word adjacent to the predicate (the
        // copula here), on the drawn side.
        let (particle_paradigm, _, _) =
            polarity_paradigm(&ph, MorphDepth::Particle, ClassPosition::Suffix);
        let particled = render(&negated, &grammar, &particle_paradigm);
        let tokens: Vec<&str> = particled.trim_end_matches('.').split(' ').collect();
        let copula_roman = grammar.copula.as_deref().unwrap();
        let copula_idx = tokens
            .iter()
            .position(|t| *t == copula_roman)
            .expect("the bare copula token must still be present, unmodified");
        assert_eq!(
            tokens.get(copula_idx + 1),
            Some(&neg_roman.as_str()),
            "the polarity particle must sit beside the predicate: {tokens:?}"
        );

        // 7. ZERO COPULA: no verb, so the negative falls to the predicate
        // nominal — the same host tense and the evidential fall to there.
        let zero_copula = TongueGrammar {
            order: grammar.order,
            copula: None,
            copula_segments: None,
            articles: grammar.articles,
            subordinator: grammar.subordinator.clone(),
            conjunction: grammar.conjunction.clone(),
        };
        let expected_enclitic = affix(
            &complement_segments,
            &neg_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let zero_marked = render(&negated, &zero_copula, &affix_paradigm);
        assert!(
            zero_marked.contains(&expected_enclitic),
            "zero-copula Affix polarity must fall to the predicate nominal: \
             {zero_marked:?} (expected token {expected_enclitic:?})"
        );
    }

    /// The Inquest T3, spec §4.3's guard for the polarity axis: a
    /// NON-LEXICAL object has no segments and must not be affixed. The guard
    /// is on the concept id, never on `Marked.segments`, because a
    /// `LexEntry::Compound` has `segments: None` too and its `layer_affix`
    /// panic is deliberate.
    #[test]
    fn a_non_lexical_object_takes_no_polarity_affix_under_a_zero_copula() {
        let ph = test_phonology();
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Count(8835),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Neg,
            adjuncts: vec![],
        };
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let morph = unmarked_morphology();
        let zero_copula = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: None,
        };
        let (affix_paradigm, _, neg_roman) =
            polarity_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);

        let rendered = realize_tongue_deep(
            &clause,
            &zero_copula,
            &morph,
            Some(&affix_paradigm),
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .expect("a numeral object must still realize");
        assert_eq!(
            rendered, "Vavako 8835.",
            "a non-lexical object bears no polarity affix: {rendered:?}"
        );
        assert!(
            !rendered.contains(&neg_roman),
            "and the marker must not appear anywhere: {rendered:?}"
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
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
            pronouns: drawn_pronouns(),
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
            None,
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
            subordinator: None,
            conjunction: None,
        }
    }

    /// A pronoun subject REALIZES in a tongue, and does it with the tongue's
    /// own drawn word.
    ///
    /// **This test replaces one that asserted the exact opposite.** The Scarf
    /// wrote `a_tongue_gaps_on_a_pronoun_subject_and_names_the_missing_inventory`,
    /// which required `realize_tongue` to refuse a `Subject::Pronoun` and to
    /// say in its reason that the tongue drew no pronoun inventory. That was
    /// true when written and green every day until The Inquest, whose Task 7
    /// drew the inventory. The old test was replaced rather than deleted
    /// because the two halves of what it pinned did not both expire: the
    /// refusal did, and the "no English may cross into a tongue" half did
    /// not, so that half is re-asserted below against a realized surface
    /// instead of against a gap.
    ///
    /// **The rule the old test enforced is not reversed** (spec §4.6). It
    /// said a tongue gaps *because* no tongue draws a pronoun inventory;
    /// drawing one falsifies the antecedent, so the rule stays true and
    /// simply stops firing. The unconditional gap arm was deleted from
    /// `tongue_subject`, never left unreachable.
    #[test]
    fn a_tongue_realizes_a_pronoun_subject_from_its_own_drawn_inventory() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let pronouns = drawn_pronouns();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let line = realize_tongue(&clause, &grammar, &lex, &pronouns).expect(
            "a tongue that draws a pronoun inventory realizes a pronoun subject (spec 4.6)",
        );
        let word = pronouns["3sg"].roman.clone();
        assert!(
            line.starts_with(&format!("{word} ")),
            "SVO puts the drawn 3sg pronoun in the subject slot: {line}"
        );
        // The half of the old test that did NOT expire: no English may cross
        // into a tongue. Before The Scarf, the projection stringified
        // Common's own pronoun and handed it over; the drawn word must not be
        // any of Common's.
        for (form, _, _, _) in PRONOUN_PARADIGM {
            assert_ne!(
                word, *form,
                "a tongue's pronoun must be drawn, never Common's English word"
            );
        }
        // And the NUMBER is the clause's: the same person at Pl draws the
        // other row, so the number is demonstrably read rather than defaulted.
        let plural = Clause {
            number: Number::Pl,
            ..clause.clone()
        };
        let plural_line =
            realize_tongue(&plural, &grammar, &lex, &pronouns).expect("3pl realizes too");
        assert!(
            plural_line.starts_with(&format!("{} ", pronouns["3pl"].roman)),
            "the clause's number selects the paradigm row: {plural_line}"
        );
        assert_ne!(
            pronouns["3sg"].roman, pronouns["3pl"].roman,
            "this seed's 3sg and 3pl must differ, or the assertion above is vacuous"
        );
    }

    /// The DEEP realizer reads the inventory off the morphology bundle, not
    /// from a parameter of its own.
    ///
    /// This is the wiring that makes the feature real for production callers
    /// rather than test-only: `windows/worldgen::tongue_morphology_of` fills
    /// `TongueMorphology::pronouns`, and every window that speaks a tongue
    /// assembles its bundle there, so no caller has to learn a new argument
    /// for a pronoun to work. Asserted DIFFERENTIALLY — the same clause
    /// against two bundles whose only difference is the inventory — so the
    /// field is demonstrably read.
    #[test]
    fn the_deep_realizer_takes_its_pronouns_from_the_morphology_bundle() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let render = |morph: &TongueMorphology| {
            realize_tongue_deep(
                &clause,
                &grammar,
                morph,
                None,
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
        };
        let morph = unmarked_morphology();
        let line = render(&morph).expect("a bundle with an inventory realizes");
        assert!(line.starts_with(&format!("{} ", morph.pronouns["3sg"].roman)));

        // Same clause, same tongue, EMPTY inventory: the surface cannot be
        // produced, so the field is what supplied it.
        let empty = TongueMorphology {
            pronouns: no_pronouns(),
            ..morph.clone()
        };
        assert_eq!(render(&empty).unwrap_err().concept, "pronoun/3sg");
    }

    /// An inventory that does not carry the row still refuses, and says which
    /// row. This is the narrower claim that SURVIVED The Scarf's blanket
    /// refusal (spec §4.6): "no tongue draws pronouns" is false now, but "this
    /// inventory has no 3sg" is a true thing to say about an empty bundle.
    #[test]
    fn an_empty_pronoun_inventory_still_refuses_and_names_the_row() {
        let lex = tiny_lexicon_with(&[("goblin-kind", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin-kind".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let gap = realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap_err();
        assert!(
            gap.reason.contains("3sg"),
            "the reason names the missing row, got: {}",
            gap.reason
        );
        assert_eq!(gap.concept, "pronoun/3sg");
    }

    /// A pronoun OBJECT realizes in a tongue — `Argument::Pronoun`, the
    /// variant this campaign added because the transitive frame's object slot
    /// needed it. The corpus line is *"I did not know them"*, so the object
    /// slot is the one that carries the third person there.
    #[test]
    fn a_tongue_realizes_a_pronoun_object() {
        let lex = tiny_lexicon_with(&[(EAT, ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let pronouns = drawn_pronouns();
        let clause = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Name("Vebe".to_string()),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let line = realize_tongue(&clause, &grammar, &lex, &pronouns)
            .expect("a pronoun object realizes (spec 4.6)");
        assert!(
            line.ends_with(&format!("{}.", pronouns["3sg"].roman)),
            "SVO puts the drawn 3sg pronoun last: {line}"
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        // Impossible before The Scarf: tongue_view panicked on any object that
        // was not a Concept, while realize_adjuncts rendered exactly this shape
        // in the adjunct slot. The two slots now agree.
        let out = realize_tongue(&clause, &grammar, &lex, &no_pronouns())
            .expect("a numeral needs no lexicon entry");
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let plural_indef = Clause {
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            ..base.clone()
        };
        assert_eq!(
            realize_tongue(&base, &grammar, &lex, &no_pronouns()).unwrap(),
            realize_tongue(&plural_indef, &grammar, &lex, &no_pronouns()).unwrap(),
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
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let _ = realize_tongue(&clause, &grammar, &lex, &no_pronouns());
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
            pronouns: drawn_pronouns(),
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
            subordinator: None,
            conjunction: None,
        };
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vavako".to_string()),
            object: Argument::Count(8835),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };

        let out = realize_tongue_deep(
            &clause,
            &zero_copula,
            &affixing,
            None,
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

    /// The transitive fixture: a lexicon carrying a word for the ACT as well
    /// as for the patient, which is the whole of what a transitive clause
    /// asks of a tongue that a nominal one did not.
    fn transitive_lexicon() -> Lexicon {
        tiny_lexicon_with(&[
            (EAT, ExposureClass::Steeped),
            ("bread", ExposureClass::Steeped),
        ])
    }

    /// The roman form and modern segments of `concept` in `lex`, for tests
    /// that must state an expected surface exactly. Panics on anything but a
    /// `Root`, so a fixture that quietly became a compound or a gap fails
    /// loudly rather than weakening the assertion.
    fn root_of(lex: &Lexicon, concept: &str) -> (String, Vec<Segment>) {
        match lex.entry(concept).unwrap() {
            LexEntry::Root { views, derivation } => {
                (views.roman.clone(), derivation.modern.clone())
            }
            other => panic!("{concept} should be a root, got {other:?}"),
        }
    }

    /// `Nwamvam <eat> bread` — the transitive demonstration clause, the
    /// tongue-side sibling of `clause.rs`'s `eat_clause`.
    fn transitive_clause(tense: Tense) -> Clause {
        Clause {
            predicate: EAT.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        }
    }

    /// The clause-embedding fixture (The Mortise, Task 5): a lexicon
    /// carrying words for both predicates the headline construction needs —
    /// `know`'s own clause complement and `kill`'s transitive frame — the
    /// tongue-side sibling of `clause.rs`'s embedding fixtures.
    fn embedding_lexicon() -> Lexicon {
        tiny_lexicon_with(&[
            (KNOW, ExposureClass::Steeped),
            (KILL, ExposureClass::Steeped),
        ])
    }

    /// `<3sg> kill <3sg>` — the embedded demonstration clause (*"he killed
    /// her"*, with pronouns rather than names, since a nested clause needs
    /// no fresh referent to make its point), parameterized on the two axes
    /// The Mortise's own tests vary: its own tense and its own evidential.
    fn embedded_kill_clause(tense: Tense, evidential: Evidential) -> Clause {
        Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential,
            tense,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        }
    }

    /// `<1sg> know <embedded>` — the matrix clause every Task 5 test wraps
    /// `embedded` in, riding the transitive frame `know` already had (spec
    /// §4, the campaign's headline construction).
    fn know_matrix_clause(tense: Tense, evidential: Evidential, embedded: Clause) -> Clause {
        Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(embedded)),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential,
            tense,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        }
    }

    /// A hand-fixed grammar for the embedding tests below: SVO, no copula
    /// (irrelevant to a transitive clause, which always fills the verb slot
    /// itself — `realize_tongue_exhaustive_orders_for_a_transitive_clause`
    /// already pins that independence), `subordinator` set by the caller.
    fn embedding_grammar(subordinator: Option<&str>) -> TongueGrammar {
        TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
            subordinator: subordinator.map(str::to_string),
            conjunction: None,
        }
    }

    /// The axis is DRAWN, not hardcoded: a tongue that draws a
    /// complementizer marks the embedded clause's boundary with it. Built
    /// by hand (`embedding_grammar`), never a drawn one — the point of
    /// pairing this test with
    /// [`a_paratactic_tongue_embeds_with_no_marker`] is that a single test
    /// cannot tell "the axis is drawn" from "the axis is hardcoded to the
    /// value I happened to test", and reaching for a drawn grammar would put
    /// the seed's own roll between the assertion and the realizer it is
    /// meant to pin.
    #[test]
    fn a_tongue_with_a_complementizer_marks_the_embedded_boundary() {
        let lex = embedding_lexicon();
        let (know, _) = root_of(&lex, KNOW);
        let (kill, _) = root_of(&lex, KILL);
        let pronouns = drawn_pronouns();
        let subj1 = tongue_pronoun(Person::First, Number::Sg, &pronouns).unwrap();
        let subj3 = tongue_pronoun(Person::Third, Number::Sg, &pronouns).unwrap();
        let grammar = embedding_grammar(Some("zil"));
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Witnessed);
        let matrix = know_matrix_clause(Tense::Past, Evidential::Witnessed, embedded);

        let out = realize_tongue(&matrix, &grammar, &lex, &pronouns).unwrap();
        assert_eq!(out, format!("{subj1} {know} zil {subj3} {kill} {subj3}."));
        // Exactly one full stop: the embedded clause's own trailing "." is
        // trimmed away by `mark_embedded_clause`, the same invariant
        // `clause.rs`'s Common embedding tests pin.
        assert_eq!(out.matches('.').count(), 1);
    }

    /// The other half of the pair: a tongue that draws NO subordinator
    /// embeds by bare juxtaposition, and that is a grammar, not a gap —
    /// many real languages subordinate exactly this way (spec §4.6). Same
    /// clause, same grammar, only `subordinator` differs from the test
    /// above, so any difference in the surface is attributable to the axis
    /// alone.
    #[test]
    fn a_paratactic_tongue_embeds_with_no_marker() {
        let lex = embedding_lexicon();
        let (know, _) = root_of(&lex, KNOW);
        let (kill, _) = root_of(&lex, KILL);
        let pronouns = drawn_pronouns();
        let subj1 = tongue_pronoun(Person::First, Number::Sg, &pronouns).unwrap();
        let subj3 = tongue_pronoun(Person::Third, Number::Sg, &pronouns).unwrap();
        let grammar = embedding_grammar(None);
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Witnessed);
        let matrix = know_matrix_clause(Tense::Past, Evidential::Witnessed, embedded);

        let out = realize_tongue(&matrix, &grammar, &lex, &pronouns).unwrap();
        assert_eq!(out, format!("{subj1} {know} {subj3} {kill} {subj3}."));
        assert_eq!(out.matches('.').count(), 1);
    }

    /// Decision 0296: tense is stated, never derived. The inner clause's
    /// `tense` is absolute and caller-stated, exactly as the matrix's is —
    /// no realizer reads one to adjust the other. The differential is what
    /// makes this non-vacuous: the MATRIX sits at Present (the zero member,
    /// unmarked) while the INNER clause sits at Past (marked), so if a
    /// realizer backshifted — computed the matrix's tense from the inner's,
    /// or vice versa — either the matrix's own verb would wrongly carry the
    /// past affix, or the inner's would wrongly lack it.
    #[test]
    fn an_inner_clause_tense_is_not_backshifted() {
        let ph = test_phonology();
        let lex = embedding_lexicon();
        let (_, know_segments) = root_of(&lex, KNOW);
        let (_, kill_segments) = root_of(&lex, KILL);
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let (paradigm, past_segments, _) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);
        let marked_know = affix(
            &know_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let marked_kill = affix(
            &kill_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        // Fixture sanity: proto_root draws two distinct roots for two
        // distinct concepts, so their affixed forms must differ too, or the
        // `contains`/`!contains` pair below would mean nothing.
        assert_ne!(marked_know, marked_kill);

        let grammar = embedding_grammar(None);
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Witnessed);
        let matrix = know_matrix_clause(Tense::Present, Evidential::Witnessed, embedded);

        let out = realize_tongue_deep(
            &matrix,
            &grammar,
            &unmarked_morphology(),
            Some(&paradigm),
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();
        assert!(
            out.contains(&marked_kill),
            "the inner clause's own Past must mark ITS verb, independent of \
             the matrix's own tense: {out:?}"
        );
        assert!(
            !out.contains(&marked_know),
            "the matrix's Present is the zero member and must not be \
             marked -- backshifting would wrongly impose the inner \
             clause's Past onto it: {out:?}"
        );
    }

    /// Spec §4.5: the matrix does not rewrite the inner clause's own
    /// grounding. `evidential` is where a per-clause value first earns its
    /// keep — a TONGUE-only payoff, since The Scarf's law (0286) has Common
    /// ignoring `evidential` entirely. The matrix carries Witnessed and the
    /// inner clause carries Inferred; both markers must survive into the
    /// rendered surface, neither clobbering the other.
    #[test]
    fn an_inner_clause_keeps_its_own_evidential() {
        use crate::etymology::proto_root;

        let ph = test_phonology();
        let lex = embedding_lexicon();
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let witnessed_segments = proto_root(&Seed(99), "goblin", "witness-marker", &ph);
        let witnessed_roman = render_views(&witnessed_segments).roman;
        let inferred_segments = proto_root(&Seed(95), "goblin", "inferred-marker", &ph);
        let inferred_roman = render_views(&inferred_segments).roman;
        assert_ne!(
            witnessed_roman, inferred_roman,
            "fixture sanity: the two markers must be textually distinct for \
             this assertion to mean anything"
        );

        let mut evidential_map = BTreeMap::new();
        evidential_map.insert(
            "witnessed",
            MorphForm {
                segments: witnessed_segments.clone(),
                roman: witnessed_roman.clone(),
            },
        );
        evidential_map.insert(
            "inferred",
            MorphForm {
                segments: inferred_segments.clone(),
                roman: inferred_roman.clone(),
            },
        );
        let morph = TongueMorphology {
            pronouns: drawn_pronouns(),
            evidential_depth: MorphDepth::Particle,
            noun_class_depth: MorphDepth::None,
            class_position: ClassPosition::Suffix,
            evidential: evidential_map,
            class: BTreeMap::new(),
        };

        let grammar = embedding_grammar(None);
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Inferred);
        let matrix = know_matrix_clause(Tense::Past, Evidential::Witnessed, embedded);

        let out = realize_tongue_deep(
            &matrix,
            &grammar,
            &morph,
            None,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        assert!(
            out.contains(&witnessed_roman),
            "the matrix's OWN evidential (Witnessed) must survive: {out:?}"
        );
        assert!(
            out.contains(&inferred_roman),
            "the inner clause's OWN evidential (Inferred) must survive, not \
             be clobbered by the matrix's Witnessed (spec §4.5): {out:?}"
        );
    }

    /// The Mortise fix-wave: the deep realizer's OBJECT-embedding arm
    /// (`realize_tongue_deep`'s own `Argument::Clause` match arm) must mark
    /// the embedded clause's boundary through [`mark_embedded_clause`],
    /// exactly as the floor realizer's `resolve_argument` arm does. Closes a
    /// hole the whole-branch review found: every existing deep-embedding
    /// test above (`an_inner_clause_tense_is_not_backshifted`,
    /// `an_inner_clause_keeps_its_own_evidential`) builds its grammar with
    /// `embedding_grammar(None)` — no drawn subordinator — so
    /// `mark_embedded_clause` is a no-op whether or not it is ever called,
    /// and neutralising the call at `realize_tongue_deep`'s object arm
    /// passed the entire `hornvale-language` suite. This test is the first
    /// deep-realizer embedding test to draw a subordinator at all.
    #[test]
    fn a_tongue_with_a_complementizer_marks_the_deep_realized_object_boundary() {
        let lex = embedding_lexicon();
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let grammar = embedding_grammar(Some("zil"));
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Witnessed);
        let matrix = know_matrix_clause(Tense::Past, Evidential::Witnessed, embedded);

        let out = realize_tongue_deep(
            &matrix,
            &grammar,
            &unmarked_morphology(),
            None,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        assert!(
            out.contains("zil"),
            "the deep realizer's object-embedding arm must mark the \
             embedded clause's boundary with the tongue's drawn \
             subordinator, exactly as the floor realizer's \
             `a_tongue_with_a_complementizer_marks_the_embedded_boundary` \
             precedent pins for `realize_tongue`: {out:?}"
        );
        assert_eq!(
            out.matches('.').count(),
            1,
            "the embedded clause's own trailing period is trimmed away by \
             `mark_embedded_clause`, the same invariant the floor-realizer \
             precedent pins: {out:?}"
        );
    }

    /// The subject-slot sibling of the test above: the deep realizer's
    /// SUBJECT-embedding arm (`realize_tongue_deep_with_subject`'s own
    /// `Subject::Clause` match arm) must mark the boundary too. Same fixture
    /// shape `clause.rs`'s `a_clause_subject_realizes_through_the_same_
    /// machinery` uses for Common (a `know`-matrix clause whose SUBJECT,
    /// not object, is the embedded `kill` clause), moved to the tongue
    /// layer and given a drawn subordinator so the marker has somewhere to
    /// surface.
    #[test]
    fn a_tongue_with_a_complementizer_marks_the_deep_realized_subject_boundary() {
        let lex = embedding_lexicon();
        let noun_class_of = |_: &str| NounClass::Inanimate;

        let grammar = embedding_grammar(Some("zil"));
        let embedded = embedded_kill_clause(Tense::Past, Evidential::Witnessed);
        let matrix = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Clause(Box::new(embedded)),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };

        let out = realize_tongue_deep(
            &matrix,
            &grammar,
            &unmarked_morphology(),
            None,
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        assert!(
            out.contains("zil"),
            "the deep realizer's subject-embedding arm must mark the \
             embedded clause's boundary with the tongue's drawn \
             subordinator, exactly as the floor realizer's `tongue_subject` \
             arm does: {out:?}"
        );
        assert_eq!(
            out.matches('.').count(),
            1,
            "the embedded clause's own trailing period is trimmed away by \
             `mark_embedded_clause`, the same invariant the object-slot \
             test above pins: {out:?}"
        );
    }

    /// The Inquest T5: a transitive clause honours all six drawn orders, and
    /// the token it puts in the V slot is the tongue's own word for the act.
    ///
    /// **Each order is asserted twice, once against a copula-bearing grammar
    /// and once against a zero-copula one, and the two must be IDENTICAL.**
    /// That pair is the assertion that matters: a transitive clause fills the
    /// verb slot itself, so the tongue's drawn copula plays no part in it.
    /// An exhaustive-order test against one grammar would pass just as well
    /// if the copula were being spliced in alongside the verb.
    #[test]
    fn realize_tongue_exhaustive_orders_for_a_transitive_clause() {
        let lex = transitive_lexicon();
        let (verb, _) = root_of(&lex, EAT);
        let (object, _) = root_of(&lex, "bread");
        let clause = transitive_clause(Tense::Present);
        let cases: [(ConstituentOrder, String); 6] = [
            (ConstituentOrder::Sov, format!("Nwamvam {object} {verb}.")),
            (ConstituentOrder::Svo, format!("Nwamvam {verb} {object}.")),
            (ConstituentOrder::Vso, format!("{verb} Nwamvam {object}.")),
            (ConstituentOrder::Vos, format!("{verb} {object} Nwamvam.")),
            (ConstituentOrder::Ovs, format!("{object} {verb} Nwamvam.")),
            (ConstituentOrder::Osv, format!("{object} Nwamvam {verb}.")),
        ];
        for (order, expected) in cases {
            for copula in [Some("gha".to_string()), None] {
                let grammar = TongueGrammar {
                    order,
                    copula: copula.clone(),
                    copula_segments: None,
                    articles: false,
                    subordinator: None,
                    conjunction: None,
                };
                let out = realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap();
                assert_eq!(out, expected, "order {order:?} copula {copula:?}");
                assert!(
                    !out.contains("gha"),
                    "a transitive clause fills the verb slot itself, so the drawn \
                     copula must not appear: {out}"
                );
            }
        }
    }

    /// The verb lexicalizes through the tongue's OWN lexicon, and gaps the
    /// whole clause when that people has no word for the act (spec §4:
    /// render fully or gap entirely). The object is known here, so a partial
    /// render — a sentence with Common's `eat` in it, or with the verb slot
    /// silently dropped — is the tempting wrong answer.
    #[test]
    fn a_tongue_gaps_when_it_has_no_word_for_the_verb() {
        let lex = tiny_lexicon_with(&[("bread", ExposureClass::Steeped)]);
        let grammar = svo_with_copula();
        let gap = realize_tongue(
            &transitive_clause(Tense::Present),
            &grammar,
            &lex,
            &no_pronouns(),
        )
        .unwrap_err();
        assert_eq!(gap.concept, EAT);
        assert_eq!(gap.reason, "no entry in this lexicon");
    }

    /// **THE ASSERTION THIS CAMPAIGN CARRIED TRANSITIVITY FOR** (spec §4.2).
    ///
    /// Tense marks the verb. Until a transitive clause existed the only verb
    /// available was the copula, and a zero-copula tongue had none at all —
    /// so the past marker fell to the predicate nominal, and nothing
    /// distinguished "tense marks the verb" from "tense marks whatever is
    /// nearest". A transitive clause supplies a real host, and this test
    /// pins that the marker lands on IT and the object stays bare.
    ///
    /// Asserted against a ZERO-COPULA grammar as well, and that arm is the
    /// sharp one: it is exactly the configuration in which a nominal clause
    /// DOES mark the object. Same tongue, same depth, same marker — the only
    /// difference is that the clause now has a verb, and the marker follows
    /// it there.
    #[test]
    fn a_transitive_past_clause_marks_the_verb_not_the_object() {
        let ph = test_phonology();
        let lex = transitive_lexicon();
        let (_, verb_segments) = root_of(&lex, EAT);
        let (object, _) = root_of(&lex, "bread");
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let (paradigm, past_segments, _) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);
        let marked_verb = affix(
            &verb_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;

        for copula in [Some("gha".to_string()), None] {
            let grammar = TongueGrammar {
                order: ConstituentOrder::Svo,
                copula: copula.clone(),
                copula_segments: None,
                articles: false,
                subordinator: None,
                conjunction: None,
            };
            let past = realize_tongue_deep(
                &transitive_clause(Tense::Past),
                &grammar,
                &unmarked_morphology(),
                Some(&paradigm),
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap();
            assert_eq!(
                past,
                format!("Nwamvam {marked_verb} {object}."),
                "past tense must affix onto the VERB and leave the object bare \
                 (copula {copula:?})"
            );

            // Differential, so the assertion cannot pass with the depth
            // unread: the same tongue and the same clause in the PRESENT —
            // the zero member — must not carry the marker anywhere.
            let present = realize_tongue_deep(
                &transitive_clause(Tense::Present),
                &grammar,
                &unmarked_morphology(),
                Some(&paradigm),
                &noun_class_of,
                &lex,
                Orthography::Digraph,
            )
            .unwrap();
            assert_ne!(
                present, past,
                "present is the zero member (copula {copula:?})"
            );
            assert!(
                !present.contains(&marked_verb),
                "an unmarked present must not carry the past affix: {present}"
            );
        }
    }

    /// The shallow-identity guarantee, for the clause shape that did not
    /// exist when it was written: `realize_tongue_deep` at `None` depth on
    /// every axis must equal `realize_tongue` byte for byte. The verb slot is
    /// the new place the two could diverge — the deep realizer builds it as a
    /// `Marked` and the floor realizer takes only its roman — so a transitive
    /// clause needs its own instance of the check.
    #[test]
    fn shallow_identity_holds_for_a_transitive_clause() {
        let ph = test_phonology();
        let lex = transitive_lexicon();
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let (paradigm, _, _) = tense_paradigm(&ph, MorphDepth::None, ClassPosition::Suffix);
        for order in [
            ConstituentOrder::Sov,
            ConstituentOrder::Svo,
            ConstituentOrder::Vso,
            ConstituentOrder::Vos,
            ConstituentOrder::Ovs,
            ConstituentOrder::Osv,
        ] {
            for copula in [Some("gha".to_string()), None] {
                let grammar = TongueGrammar {
                    order,
                    copula,
                    copula_segments: None,
                    articles: false,
                    subordinator: None,
                    conjunction: None,
                };
                for tense in [Tense::Present, Tense::Past] {
                    let clause = transitive_clause(tense);
                    let deep = realize_tongue_deep(
                        &clause,
                        &grammar,
                        &unmarked_morphology(),
                        Some(&paradigm),
                        &noun_class_of,
                        &lex,
                        Orthography::Digraph,
                    )
                    .unwrap();
                    assert_eq!(
                        deep,
                        realize_tongue(&clause, &grammar, &lex, &no_pronouns()).unwrap(),
                        "shallow identity, order {order:?} tense {tense:?}"
                    );
                }
            }
        }
    }

    /// The coordination fixture (The Mortise, Task 6): a lexicon carrying
    /// words for both predicates the two coordinated clauses need — `eat`'s
    /// transitive frame and `kill`'s — the tongue-side sibling of
    /// `clause.rs`'s coordination fixture.
    fn coordination_lexicon() -> Lexicon {
        tiny_lexicon_with(&[
            (EAT, ExposureClass::Steeped),
            (KILL, ExposureClass::Steeped),
            ("bread", ExposureClass::Steeped),
        ])
    }

    /// `<3sg> kill <3sg>` — the second coordinated clause, pronoun-only so
    /// tier 1 (no subject elision — a distinct pronoun subject from the
    /// fixture beside this one — and no right-node raising, which spec
    /// §4.10 CUTS from this campaign entirely rather than deferring)
    /// needs no fresh referent to make its point. Subject elision itself
    /// shipped in Task 7 (`elide_coordinated_subjects`) and is exercised by
    /// the tier-2 tests elsewhere in this module; this particular fixture
    /// simply is not built to trigger it.
    fn coordinated_kill_clause(tense: Tense) -> Clause {
        Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        }
    }

    /// A hand-fixed grammar for the coordination tests below, on the exact
    /// shape [`embedding_grammar`] uses for the subordinator axis: SVO, no
    /// copula (irrelevant — both coordinated clauses here are transitive,
    /// which always fills the verb slot itself), `conjunction` set by the
    /// caller.
    fn coordination_grammar(conjunction: Option<&str>) -> TongueGrammar {
        TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: None,
            copula_segments: None,
            articles: false,
            subordinator: None,
            conjunction: conjunction.map(str::to_string),
        }
    }

    /// Tier 1 coordination (spec §4.10): two FULL clauses joined by the
    /// tongue's own drawn conjunction — "It confused me and it upset me"'s
    /// tongue-side gloss, built from `eat`/`kill` on the identical reasoning
    /// `clause.rs`'s `two_clauses_coordinate_in_common` states. The axis is
    /// DRAWN, not hardcoded, so this test is paired with
    /// [`a_tongue_with_no_conjunction_juxtaposes`] on the identical logic
    /// the subordinator pair uses: a single test cannot tell "the axis is
    /// drawn" from "the axis is hardcoded to the value I happened to test".
    #[test]
    fn a_tongue_with_a_conjunction_joins_two_clauses() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let first = transitive_clause(Tense::Past);
        let second = coordinated_kill_clause(Tense::Past);
        let coord = Coordination {
            clauses: vec![first.clone(), second.clone()],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let mut first_text = realize_tongue(&first, &grammar, &lex, &pronouns).unwrap();
        assert!(first_text.ends_with('.'));
        first_text.pop();
        let mut second_text = realize_tongue(&second, &grammar, &lex, &pronouns).unwrap();
        assert!(second_text.ends_with('.'));
        second_text.pop();
        assert_eq!(out, format!("{first_text} zil {second_text}."));
        // Exactly one full stop: each clause's own trailing "." is trimmed
        // before the join, the same invariant the embedding pair pins.
        assert_eq!(out.matches('.').count(), 1);
    }

    /// The other half of the pair: a tongue that draws NO conjunction
    /// coordinates by bare juxtaposition, and that is a grammar, not a gap —
    /// asyndetic coordination is attested (spec §4.10). Same clauses, same
    /// grammar, only `conjunction` differs from the test above, so any
    /// difference in the surface is attributable to the axis alone.
    #[test]
    fn a_tongue_with_no_conjunction_juxtaposes() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(None);
        let first = transitive_clause(Tense::Past);
        let second = coordinated_kill_clause(Tense::Past);
        let coord = Coordination {
            clauses: vec![first.clone(), second.clone()],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let mut first_text = realize_tongue(&first, &grammar, &lex, &pronouns).unwrap();
        assert!(first_text.ends_with('.'));
        first_text.pop();
        let mut second_text = realize_tongue(&second, &grammar, &lex, &pronouns).unwrap();
        assert!(second_text.ends_with('.'));
        second_text.pop();
        assert_eq!(out, format!("{first_text} {second_text}."));
        assert_eq!(out.matches('.').count(), 1);
    }

    /// A coordination of fewer than two clauses states a contradiction in
    /// its own name, and the tongue realizer refuses it by panic on the
    /// identical reasoning `clause.rs`'s Common realizer test states.
    #[test]
    #[should_panic(expected = "at least two clauses")]
    fn a_tongue_coordination_of_one_clause_panics() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let coord = Coordination {
            clauses: vec![transitive_clause(Tense::Past)],
        };
        let _ = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns);
    }

    /// Tier 2 under the tongue FLOOR realizer (The Mortise, Task 7): a
    /// shared subject is stated once, the tongue-side twin of
    /// `clause::tests::a_shared_subject_is_stated_once`. Both clauses share
    /// `Subject::Pronoun(Third)` at `Number::Sg` — [`coordinated_kill_clause`]'s
    /// own subject, reused as the FIRST clause's subject too so the two are
    /// structurally equal.
    ///
    /// **The expected text is derived, not hardcoded**: `tongue_pronoun` is
    /// the same function [`realize_tongue`] itself calls to resolve
    /// `Subject::Pronoun(Third)`.
    #[test]
    fn a_tongue_elides_a_shared_subject() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        // Built directly rather than reusing `coordinated_kill_clause`
        // (whose OBJECT is `Argument::Pronoun(Third)`): this tongue's floor
        // realizer models no case distinction, so that pronoun would
        // resolve to the SAME word this test's subject pronoun does,
        // making the "exactly once" count below collide on an unrelated
        // token instead of cleanly proving the subject was elided. Sharing
        // the OBJECT concept with `first` instead sidesteps that (the same
        // fix `a_coordination_under_the_deep_realizer_elides_a_shared_subject`
        // makes for the identical reason).
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            first.subject, second.subject,
            "sanity: both clauses must share a subject for tier 2 to fire"
        );
        let coord = Coordination {
            clauses: vec![first.clone(), second.clone()],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let subject_text = tongue_pronoun(Person::Third, Number::Sg, &pronouns).unwrap();
        let mut first_text = realize_tongue(&first, &grammar, &lex, &pronouns).unwrap();
        assert!(first_text.ends_with('.'));
        first_text.pop();

        let mut second_full = realize_tongue(&second, &grammar, &lex, &pronouns).unwrap();
        assert!(second_full.ends_with('.'));
        second_full.pop();
        // `coordination_grammar` draws SVO, so the subject leads the full
        // realization -- the same structural fact the Common test relies on.
        let prefix = format!("{subject_text} ");
        assert!(
            second_full.starts_with(&prefix),
            "sanity: realize_tongue alone states the subject: {second_full:?}"
        );
        let second_without_subject = second_full
            .strip_prefix(&prefix)
            .expect("checked above with starts_with");

        assert_eq!(out, format!("{first_text} zil {second_without_subject}."));
        assert_eq!(
            out.split_whitespace()
                .filter(|word| word.trim_end_matches('.') == subject_text)
                .count(),
            1,
            "a shared subject must surface exactly once: {out:?}"
        );
    }

    /// The tongue-floor twin of `clause::tests::a_shared_object_is_not_raised`:
    /// tier 3 (right-node raising) is CUT (spec §9.1), so a shared OBJECT
    /// must still surface once per clause even though tier 2 now elides a
    /// shared SUBJECT. The two clauses here have DIFFERENT subjects
    /// (`Nwamvam` vs. the drawn pronoun), which keeps tier 2 from firing
    /// and isolates the object-sharing question this test asks.
    #[test]
    fn a_tongue_coordination_does_not_raise_a_shared_object() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let first = transitive_clause(Tense::Past); // Subject::Name("Nwamvam"), object bread
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_ne!(
            first.subject, second.subject,
            "sanity: this test isolates object-sharing, so the subjects \
             must differ (tier 2 must not fire here)"
        );
        let coord = Coordination {
            clauses: vec![first, second],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let object_word = resolve_argument(
            &Argument::Concept("bread".to_string()),
            &grammar,
            &lex,
            Number::Sg,
            &pronouns,
        )
        .unwrap();
        let occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == object_word)
            .count();
        assert_eq!(
            occurrences, 2,
            "tier 3 (right-node raising) is CUT: the shared object must be \
             stated on EACH verb, never raised to a single mention: {out:?}"
        );
    }

    /// Fix round 1, Task 7: the tongue-floor twin of
    /// `clause::tests::a_shared_object_is_not_raised_even_when_the_subject_also_elides`.
    /// The negative test above deliberately isolates object-sharing from
    /// subject-sharing with DIFFERENT subjects, which means a plausible
    /// tier-3 implementation gated on `elide && object equal` never runs
    /// its mutated branch there. This test shares BOTH the subject (tier 2
    /// must elide) and the object concept (tier 3 must still not raise
    /// it) — the exact shape of the brief's own tier-3 example.
    #[test]
    fn a_tongue_shared_object_is_not_raised_even_when_the_subject_also_elides() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third), // SAME subject: tier 2 must elide
            object: Argument::Concept("bread".to_string()), // SAME object: tier 3 must NOT raise
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            first.subject, second.subject,
            "sanity: this test needs elision to fire, unlike the isolated \
             object-sharing test above"
        );
        let coord = Coordination {
            clauses: vec![first, second],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let subject_text = tongue_pronoun(Person::Third, Number::Sg, &pronouns).unwrap();
        let object_word = resolve_argument(
            &Argument::Concept("bread".to_string()),
            &grammar,
            &lex,
            Number::Sg,
            &pronouns,
        )
        .unwrap();
        let subject_occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == subject_text)
            .count();
        let object_occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == object_word)
            .count();

        assert_eq!(
            subject_occurrences, 1,
            "the shared subject must still elide on this pair: {out:?}"
        );
        assert_eq!(
            object_occurrences, 2,
            "tier 3 stays cut even on the exact pair whose subject tier 2 \
             elides: {out:?}"
        );
    }

    /// Fix round 1, Task 7: the tongue-floor twin of
    /// `clause::tests::a_third_clause_matching_only_the_first_clause_states_its_own_subject`
    /// — `[X, Y, X]` run through the public [`realize_tongue_coordination`]
    /// entry point. Clause 2's visible antecedent is clause 1's `Y`, not
    /// clause 0's `X`, so it must restate its own subject rather than
    /// eliding against a clause two positions back.
    #[test]
    fn a_tongue_third_clause_matching_only_the_first_clause_states_its_own_subject() {
        let lex = coordination_lexicon();
        let pronouns = drawn_pronouns();
        let grammar = coordination_grammar(Some("zil"));
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let second = transitive_clause(Tense::Past); // Subject::Name("Nwamvam")
        let third = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third), // matches FIRST, not SECOND
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let coord = Coordination {
            clauses: vec![first, second, third],
        };

        let out = realize_tongue_coordination(&coord, &grammar, &lex, &pronouns).unwrap();

        let subject_text = tongue_pronoun(Person::Third, Number::Sg, &pronouns).unwrap();
        let occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == subject_text)
            .count();
        assert_eq!(
            occurrences, 2,
            "clause 0 and clause 2 must BOTH state the pronoun subject: \
             clause 2's visible antecedent is clause 1's Name(\"Nwamvam\"), \
             not clause 0, so it may not elide: {out:?}"
        );
    }

    /// Fix round 1, Task 6: coordination under the DEEP realizer keeps each
    /// coordinated clause's OWN drawn marking, exactly as embedding does
    /// (`an_inner_clause_tense_is_not_backshifted` is the precedent this
    /// test follows). Before this fix, `realize_tongue_coordination` called
    /// only the floor `realize_tongue`, so a coordinated utterance in any
    /// production narration path (which calls only `realize_tongue_deep`,
    /// never bare `realize_tongue`) rendered with zero morphological
    /// marking -- a coordination that existed but that nothing could
    /// actually use for real narration.
    ///
    /// The differential is what makes this non-vacuous: the FIRST
    /// coordinated clause sits at Past (marked) and the SECOND at Present
    /// (the zero member, unmarked). A fixture where both clauses shared one
    /// tense could pass even if marking were dropped entirely, the same
    /// trap the embedding precedent's own doc names.
    #[test]
    fn a_coordination_under_the_deep_realizer_keeps_each_clauses_own_marking() {
        let ph = test_phonology();
        let lex = coordination_lexicon();
        let (_, eat_segments) = root_of(&lex, EAT);
        let (kill_roman, kill_segments) = root_of(&lex, KILL);
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let (paradigm, past_segments, _) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);
        let marked_eat = affix(
            &eat_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let marked_kill = affix(
            &kill_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        // Fixture sanity, the same check the embedding precedent makes: two
        // distinct roots affix to two distinct marked forms, or the
        // contains/!contains pair below would mean nothing.
        assert_ne!(marked_eat, marked_kill);

        let grammar = coordination_grammar(Some("zil"));
        let first = transitive_clause(Tense::Past); // EAT, marked
        let second = coordinated_kill_clause(Tense::Present); // KILL, unmarked
        let coord = Coordination {
            clauses: vec![first, second],
        };

        let out = realize_tongue_deep_coordination(
            &coord,
            &grammar,
            &unmarked_morphology(),
            Some(&paradigm),
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        assert!(
            out.contains(&marked_eat),
            "the first coordinated clause's own Past must mark ITS verb: {out:?}"
        );
        assert!(
            !out.contains(&marked_kill),
            "the second coordinated clause's Present is the zero member and \
             must not be marked -- a floor-only join would also drop this \
             marking, so this alone would not distinguish the fix from the \
             defect it closes; paired with the `contains(&marked_eat)` \
             assertion above it does: {out:?}"
        );
        assert!(
            out.contains(&kill_roman),
            "the second clause's bare (unmarked) verb must still surface: {out:?}"
        );
        assert!(
            out.contains("zil"),
            "the Mortise fix-wave (I3): the drawn conjunction itself must \
             reach the surface, not only the per-clause marking around it — \
             replacing `join_coordinated`'s conjunction argument with `None` \
             at this call site left every assertion above still green, \
             because none of them checks for the joining word itself: {out:?}"
        );
    }

    /// Tier 2 under the DEEP realizer — the PRODUCTION path — is the case
    /// that actually matters: `windows/book`/`windows/almanac` call only
    /// `realize_tongue_deep`, never the floor realizer, for real narration
    /// (the same fact fix round 1 of Task 6 closed for coordination
    /// itself). Landing elision only under `realize_tongue_coordination`
    /// would have re-opened that exact asymmetry — a coordinated sentence
    /// eliding its subject only where nothing renders it.
    ///
    /// Reuses this module's own differential-marking fixture
    /// (`a_coordination_under_the_deep_realizer_keeps_each_clauses_own_marking`'s
    /// own EAT-Past/KILL-Present pair) so ONE test demonstrates both halves
    /// at once: the shared subject is elided, AND the second clause's own
    /// verb marking (its Present zero-marking, distinguishable from the
    /// first clause's affixed Past) is unaffected by that elision — proof
    /// that omitting the subject happens before any marking work, not as a
    /// string-surgery pass after it that might have disturbed a token
    /// elsewhere in the sentence.
    #[test]
    fn a_coordination_under_the_deep_realizer_elides_a_shared_subject() {
        let ph = test_phonology();
        let lex = coordination_lexicon();
        let (_, eat_segments) = root_of(&lex, EAT);
        let (kill_roman, kill_segments) = root_of(&lex, KILL);
        let noun_class_of = |_: &str| NounClass::Inanimate;
        let (paradigm, past_segments, _) =
            tense_paradigm(&ph, MorphDepth::Affix, ClassPosition::Suffix);
        let marked_eat = affix(
            &eat_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        let marked_kill = affix(
            &kill_segments,
            &past_segments,
            ClassPosition::Suffix,
            Orthography::Digraph,
        )
        .roman;
        assert_ne!(marked_eat, marked_kill);

        let grammar = coordination_grammar(Some("zil"));
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past, // marked
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        // Built directly rather than reusing `coordinated_kill_clause`
        // (whose OBJECT is `Argument::Pronoun(Third)`): the floor/deep
        // tongue realizer models no case distinction, so that pronoun
        // would resolve to the IDENTICAL word this test's subject pronoun
        // does, making a token-count assertion accidentally depend on
        // punctuation (only the sentence-FINAL token carries a trailing
        // period) rather than cleanly proving the subject was elided.
        // Sharing the OBJECT concept with `first` instead sidesteps that.
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present, // unmarked
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            first.subject, second.subject,
            "sanity: both clauses must share a subject for tier 2 to fire"
        );
        let coord = Coordination {
            clauses: vec![first, second],
        };

        let out = realize_tongue_deep_coordination(
            &coord,
            &grammar,
            &unmarked_morphology(),
            Some(&paradigm),
            &noun_class_of,
            &lex,
            Orthography::Digraph,
        )
        .unwrap();

        let subject_text = tongue_pronoun(Person::Third, Number::Sg, &drawn_pronouns()).unwrap();
        assert_eq!(
            out.split_whitespace()
                .filter(|word| word.trim_end_matches('.') == subject_text)
                .count(),
            1,
            "a shared subject must surface exactly once under the deep \
             realizer, the production path: {out:?}"
        );
        assert!(
            out.contains(&marked_eat),
            "the first coordinated clause's own Past must still mark ITS \
             verb after eliding the second clause's subject: {out:?}"
        );
        assert!(
            !out.contains(&marked_kill),
            "the second (subject-elided) clause's Present is the zero \
             member and must not be marked: {out:?}"
        );
        assert!(
            out.contains(&kill_roman),
            "the second clause's bare (unmarked) verb must still surface \
             even though its subject was elided: {out:?}"
        );
    }
}
