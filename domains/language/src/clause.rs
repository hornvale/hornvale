//! The clause layer: a language-neutral Clause and the Common realizer.
//! Generalizes the render_line seam from a bespoke tenet spec to any clause.
//!
//! Both realizers take a **concept id**. [`realize_common`] resolves it
//! through a [`CommonVocabulary`]; `realize_tongue_deep` (see `grammar`)
//! resolves it through a people's `Lexicon`. They differ exactly where they
//! should: Common is total, so this one is infallible; a tongue is partial, so
//! that one returns `Result<_, TongueGap>`. Before The Vernacular they
//! differed in the wrong place — one took a concept and one took a word, so
//! the author's register had no seam where "is this concept sayable?" could
//! even be asked.
#![allow(clippy::module_name_repetitions)]

use crate::common_vocab::CommonVocabulary;
use crate::morphology::Evidential;
use crate::packs::{EAT, KILL, KNOW, OLD, SLEEP, THINK, UNDER};
use hornvale_kernel::world::IS_A;
use std::sync::OnceLock;

/// Grammatical number of the subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Number {
    /// singular
    Sg,
    /// plural
    Pl,
}
/// When the clause's content stands relative to the utterance.
///
/// **Stated, never derived** (spec 3.3). Number, definiteness and evidential
/// are properties OF a clause; tense is a RELATION to a moment outside it —
/// the first feature requiring a deictic centre. A [`Clause`] has no access to
/// speech time and must not acquire one, so the caller, which knows both the
/// fact's `WorldTime` and the utterance's, supplies the relation already
/// computed. A future campaign that wants automatic tense adds a
/// *caller-side* helper, never a clock inside the clause.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tense {
    /// Contemporaneous with the utterance.
    Present,
    /// Prior to the utterance.
    Past,
}

/// Whether the clause asserts or denies.
///
/// Unlike [`Tense`] this is an ordinary property of the clause, recoverable
/// from the surface, needing no deictic centre.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Polarity {
    /// The clause asserts.
    Pos,
    /// The clause denies.
    Neg,
}

/// Whether the complement is introduced with a/the or bare.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Definiteness {
    /// a/an
    Indef,
    /// the
    Def,
}

/// Grammatical person: who the referent is relative to the speech act.
///
/// **Person only — number is not repeated here.** A pronoun's number is the
/// clause's own [`Clause::number`], whose doc already states that it is the
/// subject's number *and* "the number its object slot realizes at". Carrying
/// a second number on the pronoun would let a caller state two contradictory
/// numbers for one referent, and nothing in the world could adjudicate
/// between them. [`Person::paradigm_key`] crosses this with that number to
/// name a personal-pronoun paradigm row.
///
/// **No gender** (The Inquest, spec §4.5): nothing in the ledger assigns
/// grammatical gender, so a gendered third person would be authored rather
/// than derived.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Person {
    /// The speaker.
    First,
    /// The addressee.
    Second,
    /// Neither speaker nor addressee.
    Third,
}

impl Person {
    /// Name the personal-pronoun paradigm row this person occupies at
    /// `number` — `"1sg"`, `"2sg"`, `"3sg"`, `"1pl"`, `"2pl"`, `"3pl"`.
    ///
    /// **These are exactly the keys
    /// [`crate::pronoun_forms`](../morphology/fn.pronoun_forms.html) returns**,
    /// which is a contract between two modules and therefore has a two-way
    /// agreement test on the drawing side
    /// (`the_pronoun_paradigm_keys_are_exactly_person_crossed_with_number`):
    /// neither side may grow a row the other does not have.
    ///
    /// type-audit: bare-ok(identifier-text: return)
    #[must_use]
    pub fn paradigm_key(self, number: Number) -> &'static str {
        match (self, number) {
            (Person::First, Number::Sg) => "1sg",
            (Person::Second, Number::Sg) => "2sg",
            (Person::Third, Number::Sg) => "3sg",
            (Person::First, Number::Pl) => "1pl",
            (Person::Second, Number::Pl) => "2pl",
            (Person::Third, Number::Pl) => "3pl",
        }
    }

    /// Every person, in the order [`PRONOUN_PARADIGM`] lists them.
    pub const ALL: [Person; 3] = [Person::First, Person::Second, Person::Third];
}

/// A clause's subject: a resolved name/noun, or a pronoun for re-mention
/// (e.g. a second sentence about the same referent).
///
/// **`Pronoun` held an English literal until The Inquest** — `"it"` and
/// `"its"` were the only two values any caller passed, which put a
/// pre-rendered English word inside a language-neutral struct, the exact
/// defect The Interlinear removed from `modifiers: Vec<String>`. A tongue
/// could not realize it and gapped (The Scarf), because there was nothing
/// language-neutral in it to realize. It now carries a [`Person`], which
/// crossed with [`Clause::number`] names a paradigm row every tongue draws.
///
/// **`"its"` did not survive the retype, and nothing was lost.** It was a
/// possessive — a different grammatical function that no person/number row
/// names — and it was never *produced*: it appeared only in the two
/// surface-text-to-`Subject` maps on the parse side, inherited from a plan
/// whose day-length fragment embedded the word before adjuncts existed. A
/// bare `"its"` now binds as a [`Subject::Name`], which re-realizes to the
/// identical surface, so the round-trip law is untouched.
/// type-audit: bare-ok(identifier-text: Name.0)
#[derive(Clone, Debug, PartialEq)]
pub enum Subject {
    /// An already-resolved proper name or noun phrase.
    Name(String),
    /// A personal pronoun, at the clause's own number.
    Pronoun(Person),
    /// An embedded clause, realized in place of the subject — the same
    /// machinery [`Argument::Clause`] gives the object slot, in a different
    /// hole (The Mortise). *"That he killed her confused me"* is the
    /// complementizer-marked English gloss of the phenomenon; the
    /// complementizer itself is a tongue-side, DRAWN subordination strategy
    /// (a later campaign's business — see [`Argument::Clause`]'s doc), so
    /// Common realizes the bare embedded clause with no marker, exactly as
    /// it does for a clause bound to the object slot. This is the
    /// complementizer kind of subject clause, never the gerund
    /// (*"Seeing it"*): a gerund is a nominalization, out of scope (spec
    /// §9.1).
    ///
    /// Depth is capped at [`CLAUSE_EMBED_MAX_DEPTH`] — **the same one-level
    /// budget the object slot spends, not a second budget of its own**: a
    /// clause bound to the subject slot counts against the identical cap a
    /// clause bound to the object slot does.
    Clause(Box<Clause>),
}

impl Subject {
    /// The grammatical [`Person`] this subject agrees at.
    ///
    /// **A `Name` and a `Clause` are THIRD person, and that is a fact about
    /// language rather than a fallback.** A proper name, a noun phrase and a
    /// nominalized proposition are all things spoken ABOUT — none of them is
    /// the speaker or the addressee, which is the whole content of
    /// [`Person`]'s own definition ("who the referent is relative to the
    /// speech act"). So this is not "third person is the default when we
    /// cannot tell"; there is nothing to tell. Writing it as a named method
    /// with this doc, rather than a bare `_ => Person::Third` at each of the
    /// two call sites in [`realize_common_with_subject`], is what keeps a
    /// later reader from repairing a "missing case" that is not missing.
    ///
    /// **A `Pronoun` carries its person and nothing else carries a
    /// number** — the number is the clause's own [`Clause::number`], per
    /// [`Person`]'s doc. This method answers only the person half.
    #[must_use]
    pub fn person(&self) -> Person {
        match self {
            Subject::Pronoun(person) => *person,
            Subject::Name(_) | Subject::Clause(_) => Person::Third,
        }
    }
}

/// What an adjunct's role is bound to. Deliberately small: these are the
/// argument shapes the ledger's own `Value` already carries, minus the ones
/// no construction needs yet. A new variant is added when a role needs it,
/// never speculatively.
/// type-audit: bare-ok(identifier-text: Concept.0), bare-ok(prose: Name.0), bare-ok(count: Count.0), bare-ok(diagnostic-value: Quantity.0)
#[derive(Clone, Debug, PartialEq)]
pub enum Argument {
    /// A concept id, resolved through the realizing language's vocabulary.
    Concept(String),
    /// An already-resolved proper name, passed through unresolved.
    Name(String),
    /// A whole count, rendered as the language's cardinal.
    Count(u64),
    /// A continuous quantity, rendered at the language's grain.
    Quantity(f64),
    /// A personal pronoun, at the clause's own number.
    ///
    /// **Added because a role needed it, which is this enum's own stated
    /// rule** (see the doc above: "A new variant is added when a role needs
    /// it, never speculatively"). The role is the object slot of a
    /// transitive clause — *"I did not know them"* — which did not exist
    /// before this campaign gave the clause a transitive frame, and which no
    /// other variant can carry: a pronoun is neither a concept the
    /// vocabulary resolves nor a name that passes through unresolved.
    Pronoun(Person),
    /// An embedded clause, realized in place of a lexical complement.
    ///
    /// **Added because a role needed it, which is this enum's own stated
    /// rule.** The role is the clause complement of `know`/`think` — *"I do
    /// not know he killed her"* — which no other variant can carry.
    ///
    /// Depth is capped at [`CLAUSE_EMBED_MAX_DEPTH`]: the cap states the
    /// depth this campaign builds and can show working, not a stack-safety
    /// belt. `Box` is unique ownership with no `Rc`, so a clause graph
    /// cannot cycle; only depth is unbounded without it.
    Clause(Box<Clause>),
    /// **No argument at all** — the object slot of an intransitive frame,
    /// which relates a subject to nothing.
    ///
    /// **A departure from this enum's own rule, recorded rather than
    /// glossed.** The rule above is that a variant is added when a ROLE
    /// needs it, never speculatively. No role needs this one; the *absence*
    /// of a role does. The alternative was `Clause.object: Option<Argument>`,
    /// which is the more honest type and costs 105 full-literal construction
    /// sites against this variant's five structural match arms.
    ///
    /// **The fact-shape claim (decision 0266) survives it**, because the
    /// kernel already spells an objectless assertion: `Fact.object` is
    /// mandatory too, and `IS_PERSON`, `IS_BELIEF`, `IS_NEIGHBOR` and
    /// `TIDALLY_LOCKED` are all committed as `Value::Flag(true)`. An
    /// utterance is still a fact; this is the object that fact carries.
    ///
    /// **An `Argument::Flag(bool)` variant was rejected on substance, not
    /// passed over.** `Flag(false)` with `Polarity::Pos` and `Flag(true)`
    /// with `Polarity::Neg` would be two spellings of one denial, and the
    /// round trip could not choose between them.
    Absent,
}

/// How many `Argument::Clause`/`Subject::Clause` layers deep an argument
/// nests: `0` for anything else, one more than the deeper of that clause's
/// own object depth ([`clause_embed_depth`] on itself) and its own subject
/// depth ([`subject_embed_depth`]) for a `Clause`. [`realize_common`] panics
/// when this exceeds [`CLAUSE_EMBED_MAX_DEPTH`], and it is what that
/// constant is measured against — a clause complement may not itself
/// contain a clause complement, whichever slot the inner one is bound to.
///
/// **Mutually recursive with [`subject_embed_depth`], on purpose**: the two
/// read the SAME budget from two different holes (The Mortise, Task 4), so a
/// clause bound as a subject counts against it exactly as one bound as an
/// object does, rather than each slot keeping a depth count of its own.
///
/// `pub(crate)` (not `pub`, and not private) since Task 5: `grammar.rs`'s
/// tongue realizer needs the identical depth check `realize_common` already
/// runs, on the identical budget — a second, hand-duplicated copy would be
/// exactly the "duplicated rule with no two-way agreement test" shape a
/// later divergence could rot silently. Widening visibility is the only
/// change; the function's own behaviour is untouched.
pub(crate) fn clause_embed_depth(argument: &Argument) -> usize {
    match argument {
        Argument::Clause(inner) => {
            1 + clause_embed_depth(&inner.object).max(subject_embed_depth(&inner.subject))
        }
        _ => 0,
    }
}

/// [`clause_embed_depth`]'s mirror for the subject slot: `0` for anything but
/// a [`Subject::Clause`], one more than the deeper of that clause's own
/// object depth and subject depth otherwise. See [`clause_embed_depth`]'s
/// doc for why the two are mutually recursive and read one shared budget.
///
/// `pub(crate)` since Task 5, for the same reason [`clause_embed_depth`]
/// widened: `grammar.rs`'s tongue realizer reads the SAME shared budget for
/// a clause bound to the subject slot.
pub(crate) fn subject_embed_depth(subject: &Subject) -> usize {
    match subject {
        Subject::Clause(inner) => {
            1 + clause_embed_depth(&inner.object).max(subject_embed_depth(&inner.subject))
        }
        _ => 0,
    }
}

/// How deep a clause complement may nest before [`realize_common`] refuses
/// it. States demonstrated depth, not a stack-safety belt — see
/// [`Argument::Clause`]'s doc for why `1` is not a placeholder waiting to
/// grow. Every later clause-EMBEDDING site (a clause-carrying object, a
/// clause-carrying subject) reads this same constant rather than stating
/// its own number, so raising the cap later is a one-line change here.
/// **[`Coordination`] does not** — it sits above a clause rather than
/// inside one, so it carries no depth cap of its own; see that type's own
/// doc for why the two operators diverge on this axis.
/// type-audit: bare-ok(count)
pub const CLAUSE_EMBED_MAX_DEPTH: usize = 1;

/// One role binding on a clause: a **registered predicate** bound to an
/// argument. How it surfaces — a preposition, a case affix, a trailing
/// clause, or nothing at all — is the realizing language's business, not the
/// caller's. This is what replaced `modifiers: Vec<String>`, whose English
/// could not cross a language boundary.
/// type-audit: bare-ok(identifier-text: role)
#[derive(Clone, Debug, PartialEq)]
pub struct Adjunct {
    /// The role's predicate id, e.g. `"moon-count"`, `"occ-site"`.
    pub role: String,
    /// What the role is bound to.
    pub argument: Argument,
}

/// A language-neutral clause: predicate-argument structure plus features.
/// The per-language realizer decides how (and whether) each feature surfaces.
///
/// **Fact-shaped, deliberately.** A `Fact` is subject/predicate/object plus
/// its circumstances; a clause is the same shape plus the speaker's
/// features, because an utterance IS a fact. Before The Interlinear this
/// struct had a `frame: Frame` enum standing in for one relation and a
/// `modifiers: Vec<String>` of pre-rendered English, neither of which could
/// cross a language boundary.
///
/// **Each realizer ignores some of these features, and that is the law**
/// (The Scarf, spec 3.2). [`realize_common`] ignores `evidential`; every
/// tongue realizer ignores `number` and `definiteness`. A language-neutral
/// clause states more than any one language surfaces — so a feature going
/// unread is not a gap to be closed by teaching a tongue English number.
/// `number` in particular is what `paradigm.rs`'s drawn `number_depth` will
/// consume when a campaign wires it; this struct is where it arrives.
///
/// Named `ClauseSpec` until The Scarf, where the `Spec` suffix lost its
/// referent: it existed only to distinguish this from `TongueClause`, which
/// no longer exists.
/// type-audit: bare-ok(identifier-text: predicate)
#[derive(Clone, Debug, PartialEq)]
pub struct Clause {
    /// The relation this clause asserts, as a concept id — `"is-a"` for a
    /// classification. The same string a `Fact` would carry, which is the
    /// point: an utterance is a fact, so the clause names its predicate
    /// instead of hiding one relation inside an enum variant.
    /// type-audit: bare-ok(identifier-text: predicate)
    pub predicate: String,
    /// The subject: a resolved name, or a pronoun for re-mention.
    pub subject: Subject,
    /// What the predicate relates the subject to.
    pub object: Argument,
    /// The clause's grammatical number — the SUBJECT's, and, because a
    /// clause states exactly one, also the number its object slot realizes
    /// at.
    ///
    /// In a classification the two are one referent (`the Vavako are
    /// goblins`) so nothing is lost. In a **transitive** clause they are not,
    /// and this field states only one of them: `X eats the breads` shares its
    /// number with the subject. A clause that needs *guards ate a woman* —
    /// plural subject, singular object — is what earns a second field; no
    /// caller and no corpus line needs one today, and inventing it before
    /// then would be authoring a distinction nothing states.
    pub number: Number,
    /// Complement definiteness.
    pub definiteness: Definiteness,
    /// How this clause's content was epistemically grounded.
    ///
    /// **Common ignores this and a tongue may not** — that asymmetry is the
    /// law (spec §3.2), not a gap. A language-neutral clause states more than
    /// any one language surfaces: Common has no evidential construction, and
    /// `number`/`definiteness` run the other way, unread by every tongue.
    /// Before The Scarf this field lived only on `TongueClause`, so a caller
    /// projecting a clause into a tongue had to invent a value out of band.
    pub evidential: Evidential,
    /// When this clause's content stands relative to the utterance.
    ///
    /// **Supplied by the caller, never computed here** (spec §3.3): tense is
    /// the first feature that is a relation to a moment OUTSIDE the clause,
    /// and a `Clause` has no deictic centre to compute it against.
    pub tense: Tense,
    /// Whether this clause asserts or denies.
    pub polarity: Polarity,
    /// Role bindings on this clause. How each surfaces — and whether it
    /// surfaces inline or trailing — is the realizing language's business.
    /// Replaced `modifiers: Vec<String>`, whose pre-rendered English could not
    /// cross a language boundary and had already leaked article selection into
    /// `windows/book`.
    pub adjuncts: Vec<Adjunct>,
}

/// The complement's surface form: the concept's Common word, pluralized for
/// [`Number::Pl`]. The plural rule is deliberately the naive regular English
/// one (append `"s"`) — every kind the corpus pluralizes today (goblin,
/// hobgoblin, kobold, bugbear, gnoll) is regular, and an irregular table is a
/// separate concern from where pluralization *lives*. This is the seam it
/// lives at: `Number` already expressed the feature, so no caller hands the
/// realizer a pre-pluralized string.
fn surface_complement(vocab: &CommonVocabulary, concept: &str, number: Number) -> String {
    let word = vocab.word_for(concept);
    match number {
        Number::Sg => word,
        Number::Pl => format!("{word}s"),
    }
}

fn indefinite_article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

/// One slot or literal in a construction's surface form.
/// type-audit: bare-ok(prose: Literal.0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Part {
    /// The subject slot (a `Subject::Name`, `Subject::Pronoun`, or —
    /// since The Mortise — `Subject::Clause`, a nested clause bound to
    /// this position rather than a noun phrase).
    Subject,
    /// The copula, carrying `Clause.tense`, `Clause.number` and
    /// `Clause.polarity` together (`is`/`are`/`was`/`were`, plus `not`) —
    /// see [`COPULA_PARADIGM`].
    ///
    /// **There is deliberately no `Part::Negator`.** English negates a copula
    /// inside the copular word group (`isn't` is one word), and the parse
    /// direction does not walk `parts` structurally — it is a hand-written
    /// inverse whose first gate searches the surface for a copula. Recovering
    /// `is not` therefore widens that one search from two candidates to
    /// eight; a separate part would not remove that work, only add a second
    /// place stating the same fact. A language whose negator is a free
    /// particle in its own slot is what would earn the variant.
    Copula,
    /// The lexical verb slot: the clause's own **predicate**, resolved
    /// through the realizing vocabulary and inflected for `Clause.tense`,
    /// `Clause.number` and `Clause.polarity` together — see
    /// [`VERB_PARADIGM`].
    ///
    /// A construction carries either this or [`Part::Copula`], never both:
    /// they are the same slot filled two ways, which is why
    /// [`verb_group_forms`] can enumerate a construction's verb surfaces by
    /// asking which of the two it contains. `ConstituentOrder`'s
    /// `Sov/Svo/Vso/…` were always built around a V that the copula stood
    /// in for; this is the first construction that supplies a real one.
    Verb,
    /// The determiner slot (`the `/`a `/`an `/bare), from definiteness + number.
    Determiner,
    /// The complement lexeme.
    Complement,
    /// The clause's own predicate, resolved through the realizing
    /// vocabulary and **not inflected** — unlike [`Part::Verb`], a property
    /// word or a locative adposition takes no tense, number or polarity.
    /// Deliberately general: [`Valence::Property`] and [`Valence::Locative`]
    /// both need exactly this slot, differing only in the object slot, so it
    /// is written once here rather than as two parts with identical
    /// behaviour. **Both readers exist now** — this doc reserved the slot
    /// for a locative valence before there was one, and The Rail's Task 5
    /// built it, so the generality is a fact rather than a forecast.
    PredicateWord,
    /// The adjunct tail: inline adjuncts first (a `' '` before the first,
    /// `", "` between the rest), then each trailing adjunct after `"; "`.
    ModifierTail,
    /// A fixed literal (spacing, terminal punctuation).
    Literal(&'static str),
}

/// One row of [`COPULA_PARADIGM`]: a Common surface form paired with the
/// four features it realizes forward and (as far as the surface allows)
/// recovers backward.
/// type-audit: bare-ok(prose: CopulaRow)
pub type CopulaRow = (&'static str, Tense, Number, Polarity, Person);

/// Common's copula paradigm: `{Present, Past} × {Sg, Pl} × {Pos, Neg} ×
/// {First, Second, Third}` → surface form. **One table, read in both
/// directions** — [`realize_common`] looks a row up by its features, and
/// [`parse_common_with_tail`] searches a sentence for any row's form and
/// reads the features off it. That is the same "bidirectional by
/// construction" discipline [`common_constructions`] states for the clause
/// skeleton, applied one level down: a copula form cannot be realizable but
/// unrecognizable, or the reverse.
///
/// Negation is **appended to the copula** rather than given a slot of its
/// own; see [`Part::Copula`] for why there is no `Part::Negator`.
///
/// **Person arrived with The Rail (`r011`), and it cost this table its
/// injectivity.** Keyed by number alone, all eight rows spelled eight
/// distinct words, so a form named exactly one row and the backward read was
/// a function. English does not spell person that finely: `are` covers 2sg,
/// 1pl, 2pl and 3pl, and `were` covers the same four, so 24 rows now spell
/// **ten** forms. The forward direction is unharmed — every
/// `(tense, number, polarity, person)` still has exactly one surface, which
/// is all [`copula_surface`] ever asks. The backward direction is what
/// changed, and the change is smaller than it looks: see
/// [`verb_group_forms`] for why no row here has to be nominated "canonical",
/// and [`parse_clause_body`] for the signal that actually recovers person.
///
/// **`am` is the only genuinely new WORD**; every other row is a
/// redistribution of forms the table already carried: the DISTINCT-STRING
/// set went from 8 to 10.
///
/// **That is not the parser's search space, and reading it as one understates
/// the cost by 3x** (The Rail, Task 8 — the sentence above used to stop at
/// "went from 8 candidate strings to 10", which invites exactly that
/// reading). [`verb_group_forms`] emits **one entry per ROW**, deliberately
/// (see its own doc for why no row may be nominated canonical), and
/// [`parse_clause_body`] runs one `body.find` per emitted entry. So this
/// table costs 24 `find` calls per copular construction where it once cost
/// 8, not 10 — the 8→10 figure is true of the words, and the work is per
/// row.
/// type-audit: bare-ok(prose: COPULA_PARADIGM)
pub const COPULA_PARADIGM: &[CopulaRow] = &[
    // Present positive. `am` is first person singular and nothing else —
    // the one row in this table English spells uniquely.
    (
        "am",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::First,
    ),
    (
        "are",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "is",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::Third,
    ),
    (
        "are",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::First,
    ),
    (
        "are",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "are",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::Third,
    ),
    // Past positive. `was` is 1sg AND 3sg — English neutralizes person in
    // the singular past, but not across the 2sg row, which takes `were`.
    ("was", Tense::Past, Number::Sg, Polarity::Pos, Person::First),
    (
        "were",
        Tense::Past,
        Number::Sg,
        Polarity::Pos,
        Person::Second,
    ),
    ("was", Tense::Past, Number::Sg, Polarity::Pos, Person::Third),
    (
        "were",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::First,
    ),
    (
        "were",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "were",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::Third,
    ),
    // Present negative: the positive form with `not` appended, throughout.
    (
        "am not",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::First,
    ),
    (
        "are not",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "is not",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::Third,
    ),
    (
        "are not",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::First,
    ),
    (
        "are not",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "are not",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::Third,
    ),
    // Past negative: likewise.
    (
        "was not",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::First,
    ),
    (
        "were not",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "was not",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::Third,
    ),
    (
        "were not",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::First,
    ),
    (
        "were not",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "were not",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::Third,
    ),
];

/// The copula slot's surface for one clause's features — the forward read of
/// [`COPULA_PARADIGM`], and the direction the widened key left **total**.
/// Panics only if the table is missing a row, which the
/// `the_copula_paradigm_is_total_forward_and_syncretic_backward` test makes
/// impossible.
fn copula_surface(
    tense: Tense,
    number: Number,
    polarity: Polarity,
    person: Person,
) -> &'static str {
    COPULA_PARADIGM
        .iter()
        .find(|(_, t, n, p, pe)| *t == tense && *n == number && *p == polarity && *pe == person)
        .map(|(form, _, _, _, _)| *form)
        .expect("the copula paradigm is total over tense x number x polarity x person")
}

/// Which slot a pronoun stands in, and therefore which case Common inflects
/// it for.
///
/// **Case belongs to the SLOT, not to the clause.** A realizer already knows
/// whether it is filling the subject or the object, so nothing has to be
/// stated twice and the language-neutral [`Clause`] acquires no case axis —
/// which matters because the drawn tongue inventory has none either (a
/// tongue's pronouns are person crossed with number, spec §4.5). This is
/// decision 0286's shape one level down: Common surfaces a distinction no
/// tongue does, and that asymmetry is the law rather than a gap.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PronounCase {
    /// The subject slot.
    Nominative,
    /// The object slot.
    Accusative,
}

/// One row of [`PRONOUN_PARADIGM`]: a Common surface form paired with the
/// three features it realizes forward and (for a nominative) recovers
/// backward.
/// type-audit: bare-ok(prose: PronounRow)
pub type PronounRow = (&'static str, Person, Number, PronounCase);

/// Common's personal-pronoun paradigm: `{First, Second, Third} × {Sg, Pl} ×
/// {Nominative, Accusative}` → surface form. **One table, read in both
/// directions**, exactly as [`COPULA_PARADIGM`] is — [`realize_common`] looks
/// a row up by its features and [`nominative_person`] reads a person back off
/// a subject's surface text, so a realizable pronoun cannot be
/// unrecognizable.
///
/// **The nominative forms are pairwise distinguishing on person** (`I`/`we` →
/// first, `you` → second, `they` → third), which is what lets the parse
/// direction recover a [`Person`] from the surface alone and take the number
/// from the clause it is already recovering. That is not an accident of
/// English; it is the property the round trip needs, and
/// `nominative_forms_determine_person` pins it against the table.
///
/// **Two roughnesses were recorded here. The Rail (`r011`) fixed one and
/// left the other, and the split is the interesting part:**
///
/// 1. **Common had no person agreement — FIXED.** This entry used to read
///    that [`COPULA_PARADIGM`] and [`VERB_PARADIGM`] were "keyed by number
///    only, so a first-person subject in the positive present surfaces
///    third-person agreement (*"I knows them"*)", and argued the widening
///    bought nothing the merchant corpus needed. The ladder's `r011`
///    (*"I am a merchant. You are a guard."*) is what it bought: both
///    tables are keyed by [`Person`] now, and
///    `common_agrees_for_person_in_the_verb_and_the_copula` asserts the
///    fix. The prediction that it "also changes the parse-side search" was
///    right — see [`verb_group_forms`] and [`parse_clause_body`] for how,
///    and for why person is recovered from the SUBJECT rather than from the
///    verb group that can no longer state it.
/// 2. **Third-person singular is `they`/`them`, not `it` — UNCHANGED, and
///    not by omission.** Spec §4.5 fixes this: nothing in the ledger
///    assigns gender or animacy to a clause, so Common has one third-person
///    singular and it is the animate-neutral one. The cost is that an
///    inanimate re-mention reads *"they is a planet"* — a genuinely
///    awkward line, and exactly the "controlled register with slightly
///    awkward phrasing" §4.5 names as the accepted trade. Person agreement
///    does not touch it: agreement is keyed on FEATURES, and third-person
///    singular's bundle takes `is`. Real English gives singular *they*
///    plural agreement, which would make the copula depend on the subject's
///    chosen FORM rather than its features — a different mechanism, and one
///    no campaign has built. See
///    `commons_one_third_person_singular_is_the_animate_neutral_one`. It
///    reaches no committed artifact: no volume the book renders ever
///    re-mentions a subject.
///
/// type-audit: bare-ok(prose: PRONOUN_PARADIGM)
pub const PRONOUN_PARADIGM: &[PronounRow] = &[
    ("I", Person::First, Number::Sg, PronounCase::Nominative),
    ("me", Person::First, Number::Sg, PronounCase::Accusative),
    ("you", Person::Second, Number::Sg, PronounCase::Nominative),
    ("you", Person::Second, Number::Sg, PronounCase::Accusative),
    ("they", Person::Third, Number::Sg, PronounCase::Nominative),
    ("them", Person::Third, Number::Sg, PronounCase::Accusative),
    ("we", Person::First, Number::Pl, PronounCase::Nominative),
    ("us", Person::First, Number::Pl, PronounCase::Accusative),
    ("you", Person::Second, Number::Pl, PronounCase::Nominative),
    ("you", Person::Second, Number::Pl, PronounCase::Accusative),
    ("they", Person::Third, Number::Pl, PronounCase::Nominative),
    ("them", Person::Third, Number::Pl, PronounCase::Accusative),
];

/// The pronoun slot's Common surface for one person, number and case — the
/// forward read of [`PRONOUN_PARADIGM`]. Panics only if the table is missing
/// a row, which the `pronoun_paradigm_is_total` test makes impossible.
/// type-audit: bare-ok(prose: return)
#[must_use]
pub fn common_pronoun(person: Person, number: Number, case: PronounCase) -> &'static str {
    PRONOUN_PARADIGM
        .iter()
        .find(|(_, pe, n, c)| *pe == person && *n == number && *c == case)
        .map(|(form, _, _, _)| *form)
        .expect("the pronoun paradigm is total over person x number x case")
}

/// The [`Person`] a subject's surface text names, or `None` when the text is
/// not one of Common's nominative pronouns — the backward read of
/// [`PRONOUN_PARADIGM`], and the one place that inversion is stated. Both
/// parse sites in this workspace (this crate's [`parse_common`] and
/// `windows/book`'s re-realization of a `ParsedLine`) call it rather than
/// keeping their own copy of the mapping.
///
/// **Case-sensitive, by contract.** A capitalized `"It"`/`"They"` is not a
/// pronoun here, because Common's realizer never capitalizes a
/// sentence-initial pronoun; `pronoun_subjects_are_lowercase_by_contract`
/// is the canary that reddens if a construction ever starts to.
/// type-audit: bare-ok(prose: text)
#[must_use]
pub fn nominative_person(text: &str) -> Option<Person> {
    PRONOUN_PARADIGM
        .iter()
        .find(|(form, _, _, case)| *case == PronounCase::Nominative && *form == text)
        .map(|(_, person, _, _)| *person)
}

/// One row of [`VERB_PARADIGM`]: a prefix and a suffix that wrap a verb
/// **stem**, paired with the four features the resulting group realizes
/// forward and (as far as the surface allows) recovers backward.
///
/// A pair rather than a single form because a lexical verb's surface is
/// stem-dependent, which is the one way this table differs from
/// [`COPULA_PARADIGM`]: the copula is suppletive, so its rows carry literal
/// words, while `eat` needs `("", "s")` applied to it. English negation is
/// periphrastic, so the negative rows put the whole auxiliary in the
/// PREFIX and leave the stem bare — which is also why negation needs no
/// `Part::Negator` here any more than it did for the copula.
/// type-audit: bare-ok(prose: VerbRow)
pub type VerbRow = (&'static str, &'static str, Tense, Number, Polarity, Person);

/// Common's lexical-verb paradigm: `{Present, Past} × {Sg, Pl} × {Pos, Neg}
/// × {First, Second, Third}` → the affixes wrapping a verb stem. **One
/// table, read in both directions**, exactly as [`COPULA_PARADIGM`] is —
/// [`realize_common`] looks a row up by its features, and
/// [`parse_common_with_tail`] generates every row's surface for every
/// construction's stem and searches for one.
///
/// **Past tense is the naive regular rule (append `ed`), and third-person
/// singular present the naive `s`** — the same deliberate naivety
/// [`surface_complement`]'s plural carries, and for the same reason: an
/// irregular table is a separate concern from *where inflection lives*, and
/// this is the seam it lives at. `eat` therefore surfaces as `eated` in the
/// positive past, which
/// `a_transitive_verb_inflects_for_tense_number_and_polarity` asserts
/// outright so an irregular table arrives as a red test rather than a
/// silent correction. Note the corpus's own line is unaffected: negation is
/// periphrastic, so *"did not know"* is already right.
///
/// **Widening the key by [`Person`] (The Rail, `r011`) added no form at
/// all**, unlike [`COPULA_PARADIGM`], which gained `am`. English inflects a
/// lexical verb for person in exactly one row — third-person singular
/// present — and the number-only table already spelled that row's `s`; all
/// person did was stop the OTHER two singular rows from borrowing it. So
/// 24 rows spell the same **six** forms the old 8 did, and
/// `verb_paradigm_is_total_and_syncretic_beyond_the_third_singular` asserts
/// that count directly.
///
/// **Far less injective than it was, and in a second dimension.** The past
/// already neutralized number in both polarities (`killed`, `did not
/// kill`); it now neutralizes person as well, and the present positive
/// neutralizes both across the five non-3sg rows that share a bare stem.
/// The parse direction recovers **person from the subject** and then uses
/// it to narrow a candidate SET of numbers, which the object's own plural
/// finally decides — see [`parse_clause_body`].
/// type-audit: bare-ok(prose: VERB_PARADIGM)
pub const VERB_PARADIGM: &[VerbRow] = &[
    // Present positive: the naive third-person-singular `s`, and a bare
    // stem in every other row. THIS ROW IS THE TASK: keyed by number
    // alone, `Sg` took the `s` for all three persons, so *"I eats the
    // bread"* was what Common said. Person splits the singular, and only
    // the third-person row keeps the suffix.
    (
        "",
        "",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::First,
    ),
    (
        "",
        "",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "",
        "s",
        Tense::Present,
        Number::Sg,
        Polarity::Pos,
        Person::Third,
    ),
    (
        "",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::First,
    ),
    (
        "",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Pos,
        Person::Third,
    ),
    // Past positive: `ed` throughout. English neutralizes person AND
    // number in the regular past, so these six rows spell one form.
    (
        "",
        "ed",
        Tense::Past,
        Number::Sg,
        Polarity::Pos,
        Person::First,
    ),
    (
        "",
        "ed",
        Tense::Past,
        Number::Sg,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "",
        "ed",
        Tense::Past,
        Number::Sg,
        Polarity::Pos,
        Person::Third,
    ),
    (
        "",
        "ed",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::First,
    ),
    (
        "",
        "ed",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::Second,
    ),
    (
        "",
        "ed",
        Tense::Past,
        Number::Pl,
        Polarity::Pos,
        Person::Third,
    ),
    // Present negative: periphrastic, so the person distinction lands on
    // the AUXILIARY (`does`/`do`) rather than on the stem — which is why
    // the campaign's own corpus line *"I did not know them"* was already
    // right under the number-only key, and *"I does not know"* was not.
    (
        "do not ",
        "",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::First,
    ),
    (
        "do not ",
        "",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "does not ",
        "",
        Tense::Present,
        Number::Sg,
        Polarity::Neg,
        Person::Third,
    ),
    (
        "do not ",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::First,
    ),
    (
        "do not ",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "do not ",
        "",
        Tense::Present,
        Number::Pl,
        Polarity::Neg,
        Person::Third,
    ),
    // Past negative: `did not` throughout, neutralizing both features
    // exactly as the positive past does.
    (
        "did not ",
        "",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::First,
    ),
    (
        "did not ",
        "",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "did not ",
        "",
        Tense::Past,
        Number::Sg,
        Polarity::Neg,
        Person::Third,
    ),
    (
        "did not ",
        "",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::First,
    ),
    (
        "did not ",
        "",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::Second,
    ),
    (
        "did not ",
        "",
        Tense::Past,
        Number::Pl,
        Polarity::Neg,
        Person::Third,
    ),
];

/// The verb slot's surface for one stem and one clause's features — the
/// forward read of [`VERB_PARADIGM`], and the direction the widened key left
/// **total**. Panics only if the table is missing a row, which the
/// `verb_paradigm_is_total_and_syncretic_beyond_the_third_singular` test
/// makes impossible.
fn verb_surface(
    stem: &str,
    tense: Tense,
    number: Number,
    polarity: Polarity,
    person: Person,
) -> String {
    let (prefix, suffix) = VERB_PARADIGM
        .iter()
        .find(|(_, _, t, n, p, pe)| *t == tense && *n == number && *p == polarity && *pe == person)
        .map(|(prefix, suffix, _, _, _, _)| (*prefix, *suffix))
        .expect("the verb paradigm is total over tense x number x polarity x person");
    format!("{prefix}{stem}{suffix}")
}

/// What a predicate relates — the shape of its argument structure, and the
/// one thing a realizer must know about a predicate before it can put words
/// in an order.
///
/// **A property of the PREDICATE, owned by neither realizer.** Whether a
/// relation holds between a subject and a state it is in, or between an
/// actor and a patient it acts on, is true of `is-a` and of `eat` before any
/// language says either. Common encodes it incidentally, in whether its part
/// list carries [`Part::Copula`] or [`Part::Verb`]; a tongue has no part
/// list at all and needs the fact itself. So the fact is stated once, here,
/// and Common's parts are SELECTED from it (see [`common_constructions`])
/// rather than restated beside it. Decision 0286 makes Common one realizer
/// among the tongues rather than a privileged path, and a tongue reading
/// Common's spelling to learn a predicate's argument structure would undo
/// that quietly.
///
/// **This is not the `Frame` enum The Interlinear deleted, and the
/// difference is many-to-one.** `Frame::Classify` was one variant per
/// RELATION: the construction lookup was keyed by it, so every new predicate
/// meant a new variant. The lookup is keyed by predicate id now and stays
/// that way — nothing here is keyed by `Valence`. This enum instead sorts
/// predicates INTO argument structures many of them share: `eat`, `kill` and
/// `know` are one `Transitive` between them, adding no variant. If a future
/// campaign finds itself adding a variant per predicate, it has rebuilt
/// `Frame` and should stop.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Valence {
    /// A subject and a predicative complement: *X is a Y*. Common fills the
    /// verb slot with a copula; a tongue fills it with its own drawn copula,
    /// or leaves it empty when it drew none (a zero-copula tongue).
    Nominal,
    /// An actor and a patient: *X eats Y*. Both realizers fill the verb slot
    /// with the clause's own predicate, lexicalized — Common through its
    /// [`CommonVocabulary`], a tongue through that people's own `Lexicon`.
    Transitive,
    /// A subject and a verb, and nothing else: *X sleeps*. Stassen (1997)'s
    /// VERBAL intransitive predication strategy. Both realizers fill the
    /// verb slot with the clause's own predicate, lexicalized, exactly as
    /// [`Valence::Transitive`] does; the difference is the absent object,
    /// which Common expresses by a part list with no `Part::Complement` and
    /// a tongue by an ordering slot that is `None`.
    ///
    /// **This taxonomy is CLOSED, per spec §3.2, and that is a live guard in
    /// this campaign, not a historical note.** `Nominal` plus `Property`
    /// and `Locative` (Tasks 4 and 5 add these two beside `Intransitive`)
    /// are exactly Stassen (1997)'s four intransitive predication
    /// strategies, plus `Transitive` for the two-argument case — five
    /// variants total, ever. A fifth INTRANSITIVE-predication strategy
    /// would need Stassen's typology itself to be wrong; a fifth PREDICATE
    /// at any of these five is one row in [`PREDICATE_VALENCE`], never a
    /// new variant here. This is decision 0326's warning, verbatim: "if a
    /// future campaign finds itself adding a variant per predicate, it has
    /// rebuilt `Frame` and should stop."
    Intransitive,
    /// A subject and a property, with no second participant: *X is long*.
    /// Stassen (1997)'s ADJECTIVAL intransitive predication strategy --
    /// Dixon (1982, 2004) isolates a small property-concept core (dimension,
    /// age, value, colour) that forms the semantic heart of the adjective
    /// class wherever a language has one. Both realizers fill the verb slot
    /// with a copula, exactly as [`Valence::Nominal`] does — property
    /// predication is copular, the same way classification is — but the
    /// object slot is [`Argument::Absent`]: a property relates a subject to
    /// a state, not to a second participant, so nothing fills the
    /// determiner/complement slots [`Valence::Nominal`] uses. Common
    /// expresses the property itself through [`Part::PredicateWord`], a
    /// clause's own predicate rendered uninflected through the realizing
    /// vocabulary — the same slot [`Valence::Locative`] reuses for its
    /// adposition, the two differing only in the object slot. (That reuse
    /// was a forecast when this doc was written and is now the built case:
    /// The Rail's Task 5 added the locative valence against this very
    /// slot.)
    ///
    /// **This is the honest fix for the m02 trap** (The Mortise): rendering
    /// *"The road is long"* through [`Valence::Nominal`] produces *"the road
    /// is a long"* — the right meaning stated through the wrong relation
    /// (classification: *road is-a long*). A `Definiteness::Bare` variant
    /// would have produced the right STRING the same wrong way; this variant
    /// asserts what the sentence actually means instead, so `Definiteness`
    /// gains no third value.
    Property,
    /// A subject, a located thing, and the relation between them: *X is
    /// under Y*. Stassen (1997)'s LOCATIONAL intransitive predication
    /// strategy — Freeze (1992) argues that locative, existential and
    /// possessive predication share one underlying construction across a
    /// wide typological range. Common expresses the relation itself
    /// (`under`, `at`, `in`, …) through [`Part::PredicateWord`], the exact
    /// slot [`Valence::Property`]'s doc already reserved for a later
    /// locative valence's adposition, differing from `Property` only in
    /// that the object slot is filled: a locative binds a second
    /// participant (the located thing), a property does not.
    ///
    /// **Freeze's claim is about the CONSTRUCTION, not about this ladder's
    /// edges, and taking the citation at face value overstates what this
    /// valence unlocks.** Spec §1.2 found that the ladder's `presupposes`
    /// graph does not follow Freeze's grouping: `r019` (adnominal
    /// possession) presupposes only `r007` (definiteness), never `r005`
    /// (this valence's own rung), and `r007` gates both `r005` and `r019`
    /// independently rather than either gating the other. The three
    /// constructions may share deep structure in the typological
    /// literature; the ladder's pedagogical ordering does not encode that
    /// relationship, and this variant grants no dispensation for `r019` or
    /// `r104` (the existential rung) on its own.
    Locative,
}

impl Valence {
    /// Whether a clause at this valence binds a second participant at all.
    ///
    /// **One named fact, not an inequality list repeated at every call
    /// site.** `Intransitive` and `Property` both bind [`Argument::Absent`]
    /// — an intransitive clause has one argument by definition, and a
    /// property predication relates a subject to a state, never to a second
    /// participant (see each variant's own doc) — so both answer `false`
    /// here; `Nominal`, `Transitive` and `Locative` all bind a real object
    /// and answer `true` — a locative binds the located thing, which is the
    /// whole difference between it and `Property`. `grammar.rs`'s tongue
    /// realizers (both the floor and the deep one) consult this to decide
    /// whether the object-slot ordering token is present at all, rather
    /// than each restating
    /// `valence != Valence::Intransitive && valence != Valence::Property`
    /// — the same fact stated three times (twice in `grammar.rs`, once in
    /// prose here) before this method existed.
    ///
    /// **States only whether an object is bound, never how a tongue orders
    /// or renders one.** A tongue's own construction for a valence that
    /// binds no object may not exist at all — `Valence::Property`'s tongue
    /// path gaps today rather than ordering anything (see
    /// `grammar.rs::tongue_verb`) — and this method makes no claim about
    /// that; it is a fact about what the clause MEANS, not about what any
    /// realizer currently builds. `Valence::Locative` binds an object AND
    /// still gaps on the tongue side today, for a different reason: see its
    /// own arm in `grammar.rs::tongue_verb`.
    #[must_use]
    pub(crate) fn binds_object(self) -> bool {
        match self {
            Valence::Nominal | Valence::Transitive | Valence::Locative => true,
            Valence::Intransitive | Valence::Property => false,
        }
    }
}

/// **THE predicate inventory**: every predicate this crate can express, with
/// its valence. One row per predicate, read by both realizers — Common
/// through [`common_constructions`], a tongue through [`predicate_valence`].
///
/// A predicate absent from this list is expressible by nobody, which is the
/// condition both realizers refuse on: [`realize_common`] panics and so does
/// the tongue path, because a missing entry is an authoring hole in this
/// repository rather than a fact about a people (spec §3.3).
///
/// [`KILL`] is the promise in [`common_constructions`]'s doc being kept: a
/// second transitive verb is **one row here**, no new construction, no new
/// [`Valence`] variant and no second code path.
///
/// [`KNOW`] (The Mortise) is the same promise kept a third time: it was
/// registered vocabulary with no row here at all, so [`realize_common`]
/// panicked on it, which is exactly the red
/// `sentence_corpus.rs`'s `every_covered_entry_realizes_in_common` witness
/// was built to find.
///
/// [`THINK`] (The Mortise, Task 2) is the same promise kept a fourth time,
/// and by the same argument [`KNOW`]'s row is: one argument structure with a
/// category-flexible object, so it adds a ROW and no new [`Valence`]
/// variant. Unlike `know`, `think` is registered in
/// `packs::universal_stratum` rather than `packs::action_suite_pack` — see
/// [`crate::packs::THINK`]'s doc for why — so it is unconditionally
/// lexicalized where `know` still gaps.
///
/// [`SLEEP`] (The Rail, Task 2) is the first row at a **new** [`Valence`]:
/// [`Valence::Intransitive`], not another transitive row. It is the lever
/// this campaign is named for — `intransitive-frame` sits under seven of
/// the ladder's other eight implemented demand tokens, so this one row
/// moves five rungs from uncovered to covered at once: `r002`, `r006`,
/// `r013` and `r014` directly (each demands `intransitive-frame`), plus
/// `r015` — a reuse/control rung that introduces no token of its own but
/// presupposes both `r013` and `r014`, and so becomes covered as a
/// mechanical side effect of covering both. See
/// `cli/tests/suite/sentence_corpus.rs`'s
/// `the_ladder_score_and_frontier_match_the_campaigns_prediction` for the
/// full account (this crate cannot link to it directly — layering forbids a
/// domain from depending on `cli`).
///
/// [`OLD`] (The Rail, Task 4) is the first row at [`Valence::Property`], the
/// honest fix for the m02 trap: *"the road is old"* is a substitution for
/// the rung's own *"The road is long"* — `long` is not registered in
/// `packs::universal_stratum`, and this campaign registers no new concept
/// (see [`crate::packs::OLD`]'s own doc). Covers `r003` alone.
///
/// [`UNDER`] (The Rail, Task 5) is the row that closes the [`Valence`]
/// taxonomy at five: the first and only [`Valence::Locative`] predicate,
/// realizing *"the merchant is under the tree"* for rung `r005`. It shares
/// [`Part::PredicateWord`] with [`OLD`] and differs only in binding an
/// object (the located thing), which is the whole distinction between the
/// two valences. **Its registry kind is a compromise, and a visible one:**
/// `under` is a `ConceptKind::Quality`, the same kind `old` carries,
/// because this campaign registers no new concept and `Quality` was the
/// nearest existing kind for an adposition — see [`crate::packs::UNDER`]'s
/// own doc. Nothing checks kind against valence; this table is the only
/// thing separating a locative relation from a property word.
const PREDICATE_VALENCE: &[(&str, Valence)] = &[
    (IS_A, Valence::Nominal),
    (EAT, Valence::Transitive),
    (KILL, Valence::Transitive),
    (KNOW, Valence::Transitive),
    (THINK, Valence::Transitive),
    (SLEEP, Valence::Intransitive),
    (OLD, Valence::Property),
    (UNDER, Valence::Locative),
];

/// The valence of `predicate`, or `None` when no realizer covers it.
///
/// **The realizer-neutral question**, and the tongue path's only reason to
/// consult the clause layer's inventory at all: it asks what a predicate
/// relates, never how Common spells it. A tongue orders its constituents by
/// its own drawn [`crate::grammar::ConstituentOrder`] and lexicalizes
/// through its own lexicon; the one thing it cannot draw is whether the
/// verb slot belongs to a copula or to the predicate itself.
/// type-audit: bare-ok(identifier-text)
#[must_use]
pub fn predicate_valence(predicate: &str) -> Option<Valence> {
    PREDICATE_VALENCE
        .iter()
        .find(|(id, _)| *id == predicate)
        .map(|(_, valence)| *valence)
}

/// A form↔meaning pairing: one predicate's surface as an ordered part
/// list. The same entry realizes forward and parses backward — a future
/// predicate is added HERE, and is bidirectional by construction.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug)]
pub struct Construction {
    /// The predicate id this entry realizes/recognizes, e.g. `"is-a"`.
    pub predicate: &'static str,
    /// The ordered surface parts.
    pub parts: &'static [Part],
}

/// The Common construction inventory, keyed by **predicate id**: the
/// classification ([`hornvale_kernel::world::IS_A`]) and the transitive
/// frame ([`EAT`]). Every future predicate adds an entry, never a second
/// code path — this is the first exercise of that promise, and the shared
/// `TRANSITIVE` part list is the shape it takes: a second transitive verb is
/// **one row**, not one construction.
///
/// **The rows come from [`PREDICATE_VALENCE`], and the part list is SELECTED
/// by valence rather than written out per predicate.** That is what makes
/// "a second transitive verb is one row" mechanical instead of a promise:
/// there is one table, so Common's spelling and the fact a tongue reads
/// ([`predicate_valence`]) cannot drift apart, and no agreement test stands
/// between them needing to be kept honest. The cost is that Common gets one
/// part list per valence; a predicate that eventually needs its own Common
/// surface at an existing valence is what would earn a per-row override, and
/// widening this function is the whole change.
///
/// **Why the table stays a closed list of exact predicate ids**, rather than
/// a matcher that would catch any `ConceptKind::Act` predicate: the parse
/// direction needs to ENUMERATE candidate verb surfaces. A lexical verb's
/// surface is stem-dependent, so [`parse_common_with_tail`] recognizes a
/// verb group by generating every row's form for every construction's stem
/// and searching for one. A wildcard matcher leaves that enumeration with no
/// domain — the parser would have to segment an arbitrary sentence, guess
/// which token is the verb, un-inflect it, and only THEN ask whether the
/// result names an act. It could not: [`CommonVocabulary`] carries no
/// concept kinds (it is a declared-overrides map plus a naming convention),
/// and this crate is a domain, so it may not reach sideways to a registry to
/// learn them. The closed table is therefore not a concession to the
/// bidirectionality discipline; it is what makes the inverse computable at
/// all.
///
/// **Each key is its OWNER's constant, not a local literal**, and that is
/// load-bearing rather than tidy. `Frame::Classify` made this lookup
/// statically total: an unhandled variant was a compile error. A string key
/// moves that check to runtime ([`realize_common`] panics on a miss), so the
/// only thing left holding the two ends together is that the producer and
/// this table name the same constant. A kernel epoch bump of `IS_A` must
/// break the render, not recompile cleanly and panic on every gallery page;
/// `EAT` lives in this crate's `packs.rs` because that is what registers the
/// concept.
/// type-audit: bare-ok(identifier-text)
pub fn common_constructions() -> &'static [Construction] {
    const CLASSIFY: &[Part] = &[
        Part::Subject,
        Part::Literal(" "),
        Part::Copula,
        Part::Literal(" "),
        Part::Determiner,
        Part::Complement,
        Part::ModifierTail,
        Part::Literal("."),
    ];
    // The classification with a real verb where the copula stood. Shared by
    // every transitive predicate: adding one is adding a ROW.
    const TRANSITIVE: &[Part] = &[
        Part::Subject,
        Part::Literal(" "),
        Part::Verb,
        Part::Literal(" "),
        Part::Determiner,
        Part::Complement,
        Part::ModifierTail,
        Part::Literal("."),
    ];
    // One argument and a real verb: the transitive frame minus its object.
    const INTRANSITIVE: &[Part] = &[
        Part::Subject,
        Part::Literal(" "),
        Part::Verb,
        Part::ModifierTail,
        Part::Literal("."),
    ];
    // A copula and the property itself, no determiner and no complement:
    // the m02 trap's honest fix. `CLASSIFY` minus its `Determiner`/
    // `Complement` pair, with `Part::PredicateWord` where `Complement` sat —
    // the object slot has nothing to fill, since a property predication
    // binds `Argument::Absent`.
    const PROPERTY: &[Part] = &[
        Part::Subject,
        Part::Literal(" "),
        Part::Copula,
        Part::Literal(" "),
        Part::PredicateWord,
        Part::ModifierTail,
        Part::Literal("."),
    ];
    // A copula, the relation itself (`Part::PredicateWord`, the same slot
    // `PROPERTY` uses), and — unlike `PROPERTY` — a determiner and a
    // complement: a locative has a located thing where a property
    // predication has nothing. `CLASSIFY`'s `Determiner`/`Complement` pair,
    // with `Part::PredicateWord` inserted between the copula and the
    // determiner where the adposition sits.
    const LOCATIVE: &[Part] = &[
        Part::Subject,
        Part::Literal(" "),
        Part::Copula,
        Part::Literal(" "),
        Part::PredicateWord,
        Part::Literal(" "),
        Part::Determiner,
        Part::Complement,
        Part::ModifierTail,
        Part::Literal("."),
    ];
    // Built once and leaked into a `static` so the signature stays
    // `&'static [Construction]` — `parse_common_with_tail` walks this on
    // every parse and callers hold no allocation. The same `OnceLock`
    // memoisation `kernel/src/geosphere.rs` and `domains/climate/src/axes.rs`
    // use for their own derived tables.
    static INVENTORY: OnceLock<Vec<Construction>> = OnceLock::new();
    INVENTORY.get_or_init(|| {
        PREDICATE_VALENCE
            .iter()
            .map(|(predicate, valence)| Construction {
                predicate,
                parts: match valence {
                    Valence::Nominal => CLASSIFY,
                    Valence::Transitive => TRANSITIVE,
                    Valence::Intransitive => INTRANSITIVE,
                    Valence::Property => PROPERTY,
                    Valence::Locative => LOCATIVE,
                },
            })
            .collect()
    })
}

/// Every surface `construction`'s verb group can take, paired with the
/// features each one realizes — the **backward** read of
/// [`COPULA_PARADIGM`] and [`VERB_PARADIGM`], and the enumeration that makes
/// [`parse_common_with_tail`] possible.
///
/// Which table applies is read off the construction's own parts, not off a
/// second field that could disagree with them: a construction carries
/// [`Part::Copula`] or [`Part::Verb`], and one that carries neither has no
/// verb group. Those parts are now SELECTED by the predicate's [`Valence`]
/// (see [`common_constructions`]), so reading them and asking
/// [`predicate_valence`] are the same fact arrived at from two sides — still
/// one table, and still nothing that can disagree. This function keeps
/// reading the parts because it needs Common's surface anyway. A
/// construction carrying neither part has no verb group and contributes no
/// candidate (unreachable today, and returning an empty list rather than
/// panicking keeps the parser's failure a [`ParseError`] rather than a
/// crash).
///
/// **One entry per ROW, not per distinct form — so no row is nominated
/// "canonical" and none has to be.** The widened key (The Rail, `r011`)
/// makes both tables non-injective: `are` names four copula rows and a bare
/// verb stem names five. The obvious repair would be to collapse them here
/// and pick a canonical [`Person`] for each form, which is a rule made by
/// table ORDERING — an implicit decision, and exactly the kind that rots
/// when a row is later inserted. This function refuses to make it. It emits
/// every row, and [`parse_clause_body`] narrows the resulting candidate set
/// using a signal the verb group does not carry: **the subject's own
/// surface**, which spells person unambiguously
/// ([`nominative_person`] over [`PRONOUN_PARADIGM`]'s nominatives, or
/// [`Subject::person`]'s third person for a name).
///
/// So person is not LOST by the backward read; it is recovered from a
/// different part of the sentence than the part that realized it. That is
/// the same division of labour this walk already used for `killed`, whose
/// number the verb group cannot state and the object's plural decides — see
/// [`VERB_PARADIGM`]'s own doc.
fn verb_group_forms(
    construction: &Construction,
    vocab: &CommonVocabulary,
) -> Vec<(String, Tense, Number, Polarity, Person)> {
    if construction.parts.contains(&Part::Copula) {
        COPULA_PARADIGM
            .iter()
            .map(|(form, t, n, p, pe)| ((*form).to_string(), *t, *n, *p, *pe))
            .collect()
    } else if construction.parts.contains(&Part::Verb) {
        let stem = vocab.word_for(construction.predicate);
        VERB_PARADIGM
            .iter()
            .map(|(prefix, suffix, t, n, p, pe)| {
                (format!("{prefix}{stem}{suffix}"), *t, *n, *p, *pe)
            })
            .collect()
    } else {
        Vec::new()
    }
}

/// Realize a Clause as a Common (≈ limited English) sentence, resolving
/// `spec.object` through `vocab` when it names a concept.
///
/// **Infallible, and deliberately so.** Common is the author's register, not
/// a people's tongue: [`CommonVocabulary::word_for`] is total, so there is no
/// `CommonGap` to return. A gap therefore always means something true about
/// the world (this people has no word for the sea) rather than an authoring
/// hole, because only the tongue path can gap at all.
///
/// Every [`Argument`] variant has an answer in the object slot — a
/// `Concept` resolves through the vocabulary (pluralized by `number`), a
/// `Name` passes through verbatim, and a `Count`/`Quantity` renders through
/// [`cardinal`]/[`quantity`]. None panics: a clause whose object is a
/// quantity is a sentence we cannot say *yet*, not a crash.
///
/// The article is chosen from the **resolved word**, not the id — so `an`
/// still fires for `elemental`, and now also for a declared multi-word
/// display.
///
/// Panics only if `spec.predicate` names no construction — the one thing a
/// caller must get right, and the same shape the `Frame` lookup had before
/// The Interlinear made the key a string.
/// type-audit: bare-ok(prose)
pub fn realize_common(spec: &Clause, vocab: &CommonVocabulary) -> String {
    realize_common_with_subject(spec, vocab, true)
}

/// [`realize_common`]'s own body, widened with one caller-only knob:
/// whether to realize `spec.subject` at all. `realize_common` itself always
/// passes `true`, so its behaviour is unchanged byte for byte;
/// [`realize_common_coordination`] is the only caller that ever passes
/// `false`, for a coordinated clause whose subject is identical to the
/// LAST STATED subject before it (see [`elide_coordinated_subjects`] for
/// exactly what that means and why it is not simply "the first clause",
/// The Mortise, Task 7 fix round 1, spec §4.10's tier 2).
///
/// **Elision happens HERE, inside realization, not as text surgery on an
/// already-realized sentence.** A coordinated clause with `include_subject:
/// false` never has its subject text computed or pushed at
/// [`Part::Subject`] at all — the surface constituent is simply absent, the
/// same discipline [`crate::grammar::realize_tongue`]'s own elision knob
/// uses for a tongue whose constituent order is drawn rather than fixed.
/// Common's own construction table happens to place [`Part::Subject`] first
/// in every row (see [`common_constructions`]), immediately followed by a
/// literal space, so omitting the subject leaves exactly one leading space
/// to trim — a mechanical cleanup of a separator this table always emits
/// there, not a search for content.
fn realize_common_with_subject(
    spec: &Clause,
    vocab: &CommonVocabulary,
    include_subject: bool,
) -> String {
    emit_parts(
        common_construction_for(spec).parts,
        spec,
        vocab,
        include_subject,
    )
}

/// The construction [`common_constructions`] holds for this clause's
/// predicate. Split out so the declarative realizer and
/// [`realize_common_polar_question`] resolve it the same way and fail the
/// same way — the question operator is an operator OVER a clause's own
/// construction, never a second table.
///
/// Panics if `spec.predicate` names no construction, exactly as
/// [`realize_common`]'s own doc states.
fn common_construction_for(spec: &Clause) -> &'static Construction {
    common_constructions()
        .iter()
        .find(|c| c.predicate == spec.predicate)
        .unwrap_or_else(|| {
            panic!(
                "Common has no construction for predicate {:?}",
                spec.predicate
            )
        })
}

/// Emit one ordered part list against one clause — the shared surface walk.
///
/// **Takes `parts`, not a [`Construction`], and that is the whole point of
/// the split.** [`realize_common_with_subject`] passes the construction's
/// own list unchanged; [`realize_common_polar_question`] passes a REORDERED
/// copy of the very same list. Neither writes a second surface by hand, so
/// there is nothing to keep in sync with [`common_constructions`].
fn emit_parts(
    parts: &[Part],
    spec: &Clause,
    vocab: &CommonVocabulary,
    include_subject: bool,
) -> String {
    let complement = match &spec.object {
        Argument::Concept(id) => surface_complement(vocab, id, spec.number),
        Argument::Name(text) => text.clone(),
        Argument::Count(n) => cardinal(*n),
        Argument::Quantity(x) => quantity(*x),
        // The object slot, so the ACCUSATIVE — case is the slot's business,
        // never the clause's (see `PronounCase`).
        Argument::Pronoun(person) => {
            common_pronoun(*person, spec.number, PronounCase::Accusative).to_string()
        }
        // An embedded clause realizes as its own full sentence, minus the
        // trailing full stop this clause's own `Part::Literal(".")` will
        // supply — realizing it whole and trimming is simpler than a second
        // "clause body, no terminator" code path, and every construction
        // this table has ends in exactly one `Literal(".")`, so the trim is
        // safe. The depth check runs BEFORE the recursive call so a clause
        // past the cap panics without ever realizing the offending text.
        Argument::Clause(inner) => {
            let depth = clause_embed_depth(&spec.object);
            assert!(
                depth <= CLAUSE_EMBED_MAX_DEPTH,
                "a clause complement nests {depth} deep, past the cap of \
                 {CLAUSE_EMBED_MAX_DEPTH}: a clause complement may not \
                 itself contain a clause complement"
            );
            let mut text = realize_common(inner, vocab);
            if text.ends_with('.') {
                text.pop();
            }
            text
        }
        // No argument at all, so nothing to resolve. Bound to a predicate
        // at a valence that does not bind an object at all
        // ([`Valence::binds_object`] is `false` — today `Intransitive` or
        // `Property`), and each such construction's own part list carries
        // no `Part::Complement` and no `Part::Determiner` (`INTRANSITIVE`,
        // `PROPERTY`, see `common_constructions`), so this value is never
        // read.
        Argument::Absent => String::new(),
    };
    let mut out = String::new();
    for part in parts {
        match part {
            // `include_subject: false` (a coordinated clause whose subject
            // matches the LAST STATED one before it, see
            // `elide_coordinated_subjects`, Task 7) skips this arm
            // entirely — the subject constituent is never computed or
            // pushed, not merely emptied. The one leading space this leaves
            // (this table's own `Part::Literal(" ")` immediately follows
            // every `Part::Subject`, see `common_constructions`) is trimmed
            // once, after the loop below.
            Part::Subject if !include_subject => {}
            Part::Subject => {
                let text = match &spec.subject {
                    Subject::Name(name) => name.clone(),
                    Subject::Pronoun(person) => {
                        common_pronoun(*person, spec.number, PronounCase::Nominative).to_string()
                    }
                    // A clause bound to the subject slot realizes through
                    // the exact same machinery `Argument::Clause` uses in
                    // the object slot: its own full sentence, minus the
                    // trailing period this clause's own `Part::Literal(".")`
                    // supplies. No complementizer is added — the marker
                    // (*"That..."*) is a tongue-side, DRAWN subordination
                    // strategy, a later campaign's business (spec §9.1);
                    // Common's register has no such word to spend. The depth
                    // check runs BEFORE the recursive call, the same
                    // ordering the object slot uses, so a subject past the
                    // cap panics without ever realizing the offending text.
                    Subject::Clause(inner) => {
                        let depth = subject_embed_depth(&spec.subject);
                        assert!(
                            depth <= CLAUSE_EMBED_MAX_DEPTH,
                            "a clause subject nests {depth} deep, past the cap of \
                             {CLAUSE_EMBED_MAX_DEPTH}: a clause bound to the subject \
                             slot may not itself contain a clause complement"
                        );
                        let mut text = realize_common(inner, vocab);
                        if text.ends_with('.') {
                            text.pop();
                        }
                        text
                    }
                };
                out.push_str(&text);
            }
            Part::Copula => {
                // The person comes from the SUBJECT (`Subject::person`),
                // which is the only place a clause states one — `Clause` has
                // no person field of its own, and deliberately: person is a
                // property of the referent in the subject slot, not of the
                // proposition.
                out.push_str(copula_surface(
                    spec.tense,
                    spec.number,
                    spec.polarity,
                    spec.subject.person(),
                ));
            }
            // The clause's own predicate, through the same vocabulary the
            // complement goes through: Common resolves a concept id, it
            // never echoes one.
            Part::Verb => out.push_str(&verb_surface(
                &vocab.word_for(&spec.predicate),
                spec.tense,
                spec.number,
                spec.polarity,
                spec.subject.person(),
            )),
            // The clause's own predicate again, through the same
            // vocabulary lookup `Part::Verb` uses — but **not inflected**:
            // a property word, or the locative valence's adposition, takes
            // no tense, number or polarity, unlike a lexical verb. (This
            // comment said "a FUTURE locative valence's adposition" until
            // The Rail's Task 8; Task 5 built that valence, and this was
            // the third of three forward references in this file left
            // pointing at it as unbuilt.)
            Part::PredicateWord => out.push_str(&vocab.word_for(&spec.predicate)),
            // A PRONOUN fills the determiner slot itself — English has no
            // "*the them", and no `Definiteness` a caller states can change
            // that, so the slot is skipped rather than given a fourth row.
            // The condition is on the object's SHAPE, not on the feature,
            // because definiteness is a property of the clause and this is a
            // property of what the object slot holds. A CLAUSE is skipped
            // for the same reason: "*I do not know a he killed her*" is
            // what NOT suppressing it produces (spec §4.2).
            Part::Determiner
                if matches!(spec.object, Argument::Pronoun(_) | Argument::Clause(_)) => {}
            Part::Determiner => match (spec.definiteness, spec.number) {
                (Definiteness::Def, _) => out.push_str("the "),
                (Definiteness::Indef, Number::Sg) => {
                    out.push_str(indefinite_article(&complement));
                    out.push(' ');
                }
                (Definiteness::Indef, Number::Pl) => {} // bare generic
            },
            Part::Complement => out.push_str(&complement),
            Part::ModifierTail => {
                let mut inline: Vec<String> = Vec::new();
                let mut trailing: Vec<String> = Vec::new();
                for adjunct in &spec.adjuncts {
                    match common_role_surface(adjunct, vocab) {
                        Some((AdjunctPosition::Inline, text)) => inline.push(text),
                        Some((AdjunctPosition::Trailing, text)) => trailing.push(text),
                        None => {}
                    }
                }
                for (i, text) in inline.iter().enumerate() {
                    out.push_str(if i == 0 { " " } else { ", " });
                    out.push_str(text);
                }
                for text in &trailing {
                    out.push_str("; ");
                    out.push_str(text);
                }
            }
            Part::Literal(text) => out.push_str(text),
        }
    }
    if include_subject {
        out
    } else {
        // Every construction places `Part::Subject` first (see
        // `common_constructions`), so an omitted subject leaves exactly the
        // one separator space that always follows it — trimmed here rather
        // than left for `realize_common_coordination` to strip out of an
        // already-joined sentence.
        out.trim_start().to_string()
    }
}

/// A coordinated sequence of clauses — a LIST at a node, never a slot that
/// holds one (The Mortise, Task 6, spec §4.10).
///
/// **This is a different operator from [`Argument::Clause`] /
/// [`Subject::Clause`], and the difference is the whole design.** Embedding
/// is a slot that holds a clause; coordination is a list at a node. They
/// share exactly one idea — both are marked at a boundary, and that marker
/// is what keeps the parser's inverse computable (spec §4.10) — and
/// nothing else. Concretely: `Coordination` carries no depth cap of its
/// own and does not read [`CLAUSE_EMBED_MAX_DEPTH`], because it is not a
/// clause-internal slot at all; it sits ABOVE a clause, wrapping it, the
/// way a sentence wraps a clause rather than a clause wrapping itself.
///
/// **Additive above `Clause`, on purpose.** `Clause` gains no field for
/// this: a coordination is a fact about how two or more *whole* clauses
/// relate to each other, not a fact any one clause carries about itself.
/// Putting a list inside a node that is not one would have cost an edit at
/// every one of `Clause`'s many literal construction sites; arriving above
/// it costs none of them — [`realize_common`] keeps taking a `&Clause` and
/// always will.
///
/// **Tiers 1 and 2 of spec §4.10's three-tier ladder are built; tier 3 is
/// not, and never will be in this campaign.** Tier 1 — every clause states
/// its own subject in full — is what a coordination realizes when its
/// clauses' subjects differ: *"It confused me and the goblin upset me."*
/// Tier 2 — a shared subject is stated once — fires automatically whenever
/// a clause's subject equals the LAST STATED subject before it, not
/// necessarily the first clause's own (see [`elide_coordinated_subjects`],
/// fix round 1: comparing only against the first clause misattributes a
/// 3+-clause coordination like `[X, Y, X]`): *"It confused me and upset
/// me"* rather than *"It confused me and it upset me."* **Tier 3 (right-node raising — sharing
/// the OBJECT too, "It confused and upset me") is CUT from this campaign
/// entirely** (spec §9.1, The Mortise Task 7): what a language may elide is
/// typological, and getting it wrong yields *plausible* garbage, the
/// failure mode that survives review. `a_shared_object_is_not_raised` pins
/// this as a deliberate boundary, not an unexamined gap, so a later
/// campaign that wants tier 3 has to come here and change it on purpose.
///
/// **Subject elision is a uniform surface convention, not a drawn axis.**
/// Whether a language elides a coordinate subject at all is genuinely
/// typological, but this campaign's two stream labels
/// (`subordinator`, `conjunction`) are both spent, and no third is added
/// here — every tongue elides a stated-once subject the same way Common
/// does. A future campaign that wants this to vary per tongue needs its own
/// stream label and its own draw; this one states the limit rather than
/// leaving it to be discovered as a silent default.
///
/// **Parsing a `Coordination` back out of its own realized text is not
/// attempted.** [`parse_common_with_tail`] refuses the instant it sees the
/// top-level `" and "` boundary marker — spec §6's own success criterion
/// asks only that the parser round-trip embedding and DISTINGUISH the two
/// operators by that marker, not that it recover a `Coordination`.
/// Recovering one — including inverting tier 2's elided-subject
/// reattribution, [`elide_coordinated_subjects`]'s own inverse — is out of
/// scope, named here rather than left findable only on the private parser
/// function that enforces it (`parse_clause_body`'s own doc).
#[derive(Clone, Debug, PartialEq)]
pub struct Coordination {
    /// The coordinated clauses, in surface order. At least two — a
    /// "coordination" of fewer than two clauses is not a list, and both
    /// realizers assert this rather than silently degrading.
    pub clauses: Vec<Clause>,
}

/// Which coordinated clauses elide their subject (The Mortise, Task 7 fix
/// round 1): index `i` is `true` when clause `i`'s `(subject, number)`
/// matches whatever subject was last **stated** on the surface — not
/// whichever clause happened to be first.
///
/// **Compared against the last STATED subject, never the first clause.**
/// An earlier version of this rule compared every clause to clause 0. That
/// is wrong for 3+ clauses: for `[X, Y, X]` (subjects `Pronoun(Third)`,
/// `Name("Bemvo")`, `Pronoun(Third)` again), comparing to the first clause
/// elides the third because it matches clause 0 — but a reader parsing
/// left to right has only just read `Y` stated on clause 2, so the missing
/// subject reads as "Bemvo killed... and knowed the goblin", confidently
/// attributed to the WRONG referent. That is *plausible garbage* — the
/// exact failure mode spec §9.1 cites as the reason tier 3 (right-node
/// raising) is cut — occurring inside tier 2, which this campaign does
/// build. Comparing to the last stated subject instead means clause 3
/// compares against `Y` (clause 2's own, since clause 2 was not itself
/// elided), finds no match, and states its own `X` — correct.
///
/// **The successor case, worked by hand:** for `[X, X, Y, X]`, this rule
/// gives X / *elided* / Y / X — clause 2 elides against clause 1's `X`
/// (the last stated subject at that point), clause 3 states `Y` (no
/// match), and clause 4 states `X` again because the visible antecedent
/// immediately before it is `Y`, not `X`. Comparing to the first clause
/// would have elided clause 4 too, reading as `Y`'s subject — wrong. Last
/// stated is strictly more correct than first-clause on every case
/// first-clause got right (two clauses, or 3+ where the shared subject
/// never has an intervening different one) AND on the cases it got wrong.
///
/// **The equality compared is `(subject, number)`, not bare [`Subject`]
/// equality.** [`Subject`] alone derives [`PartialEq`], and comparing only
/// that would treat `Subject::Pronoun(Person::Third)` at [`Number::Sg`]
/// (*"it"*) as the same referent as [`Number::Pl`] (*"they"*) — two
/// different surface pronouns bound to the same enum variant, since a
/// clause's number lives on [`Clause::number`], not on [`Subject`] itself
/// (see that field's own doc). Eliding across a number mismatch would drop
/// the very feature that tells the reader whether one confuser or several
/// are meant, so both must agree before a subject is silently omitted.
///
/// **Shared by all three coordination realizers** (Common,
/// [`crate::grammar::realize_tongue_coordination`], and
/// [`crate::grammar::realize_tongue_deep_coordination`]) so the elision
/// RULE exists in exactly one place, never three copies with no agreement
/// test between them. Each realizer still does its OWN per-clause
/// realization with or without the subject constituent — this function
/// only decides which clauses get which.
///
/// Clause 0 is never elided (`last_stated` starts `None`, so the first
/// comparison always fails), matching every realizer's existing panic-below
/// contract that a coordination needs at least two clauses to mean
/// anything, though this function itself tolerates any length including 0
/// or 1 (it returns an all-`false` vector rather than asserting, since the
/// length check belongs to each public realizer, not to this shared rule).
pub(crate) fn elide_coordinated_subjects(clauses: &[Clause]) -> Vec<bool> {
    let mut elisions = Vec::with_capacity(clauses.len());
    let mut last_stated: Option<(&Subject, Number)> = None;
    for clause in clauses {
        let elide = last_stated
            .is_some_and(|(subject, number)| clause.subject == *subject && clause.number == number);
        elisions.push(elide);
        if !elide {
            last_stated = Some((&clause.subject, clause.number));
        }
    }
    elisions
}

/// Realize a [`Coordination`] as a Common (≈ limited English) sentence: each
/// clause realizes through [`realize_common_with_subject`], trimmed of its
/// own trailing full stop, then joined with `"and"` — Common's own
/// coordinating conjunction, on the same footing every other Common surface
/// choice is (this register's fixed vocabulary, not a drawn value; only a
/// TONGUE's conjunction is drawn, see
/// [`crate::grammar::realize_tongue_coordination`]) — and the whole
/// sentence takes exactly one trailing period, on the identical
/// "realize whole and trim" discipline [`realize_common`]'s own
/// `Argument::Clause`/`Subject::Clause` arms already use for a nested
/// clause.
///
/// **Tier 2: a clause whose subject matches the last STATED subject (see
/// [`elide_coordinated_subjects`] for exactly what "last stated" means and
/// why it is not "the first clause") is realized WITHOUT its subject
/// constituent** (`include_subject: false`, see
/// [`realize_common_with_subject`]) — *"It confused me and upset me"*.
/// Every other clause states its own subject in full (tier 1).
///
/// Panics if `coord.clauses` holds fewer than two clauses: a coordination
/// with nothing to join states a contradiction in its own name, and the
/// panic is the same class as `realize_common`'s "no construction for this
/// predicate" — an authoring hole, not a fact about the world a
/// [`TongueGap`](crate::grammar::TongueGap)-shaped return could state
/// (Common is infallible; see [`realize_common`]'s own doc for why).
/// type-audit: bare-ok(prose)
pub fn realize_common_coordination(coord: &Coordination, vocab: &CommonVocabulary) -> String {
    assert!(
        coord.clauses.len() >= 2,
        "a coordination joins at least two clauses; {} is not a list to \
         coordinate",
        coord.clauses.len()
    );
    let elisions = elide_coordinated_subjects(&coord.clauses);
    let mut parts = coord.clauses.iter().zip(elisions).map(|(clause, elide)| {
        let mut text = realize_common_with_subject(clause, vocab, !elide);
        if text.ends_with('.') {
            text.pop();
        }
        text
    });
    let mut out = parts.next().expect("length checked above: at least one");
    for part in parts {
        out.push_str(" and ");
        out.push_str(&part);
    }
    out.push('.');
    out
}

/// One construction's part list, reordered into a polar question: the verb
/// group moves in front of the subject, and the terminal full stop becomes a
/// question mark.
///
/// **A positional SWAP, not an insertion.** Every copular construction in
/// [`common_constructions`] opens `[Subject, Literal(" "), Copula, …]`, so
/// exchanging the two slots' positions leaves the separator between them
/// exactly where it was and yields `[Copula, Literal(" "), Subject, …]` —
/// *"is the road old?"*, *"is the merchant under the tree?"* — with no
/// literal added, removed, or respaced. Doing it by index rather than by
/// rebuilding a list means a construction that later grows a part between
/// its subject and its copula still gets those two SLOTS exchanged
/// correctly — [`Part::Subject`]'s new position still holds the copula, and
/// [`Part::Copula`]'s new position still holds the subject. **That is
/// narrower than "the resulting surface is correct."** Nothing here or in
/// [`realize_common_polar_question`] asserts anything about what an
/// intervening part *between* the two swapped slots would do to the
/// emitted text once inverted — no construction has one today, so the case
/// is untested, not verified safe (T9 review, carried from Task 8).
///
/// The terminal literal is rewritten rather than appended to, for the same
/// reason: `"."` is a part this table emits, and a question replaces it.
///
/// **Panics, loudly, if the last part is not `Literal(".")`** — every one of
/// `common_constructions`' five part lists ends in exactly that literal
/// today, so the swap above never actually reaches a construction where it
/// would not, but this function used to degrade SILENTLY there instead: an
/// `if let ... && *last == Literal(".")` that simply left a non-`"."`
/// terminal untouched, so a future copular construction ending in anything
/// else would realize a question with a full stop still on it — the exact
/// "plausible garbage" class this operator refuses a lexical verb to avoid
/// (see this function's own caller's doc), just reached by a different door.
/// Unreachable today; reachable the moment a construction is added whose
/// terminal literal is not `"."`, and this panic is what makes that reachable
/// case loud instead of silently wrong (T9 review, carried from Task 8).
fn invert_for_question(parts: &[Part]) -> Vec<Part> {
    let subject_at = parts
        .iter()
        .position(|p| *p == Part::Subject)
        .expect("a construction with a copula also has a subject slot");
    let copula_at = parts
        .iter()
        .position(|p| *p == Part::Copula)
        .expect("checked by the caller before this is called");
    let mut inverted = parts.to_vec();
    inverted.swap(subject_at, copula_at);
    match inverted.last_mut() {
        Some(last @ Part::Literal(".")) => *last = Part::Literal("?"),
        other => panic!(
            "a polar-question construction's terminal part must be \
             Literal(\".\"), so this operator has something to rewrite into \
             \"?\"; found {other:?} instead"
        ),
    }
    inverted
}

/// Realize a [`Clause`] as a Common **polar question** — *"are you a
/// merchant?"* — by inverting its own construction rather than by spelling a
/// second surface.
///
/// **Force is an OPERATOR over a clause, never a field on it.** [`Clause`] is
/// fact-shaped (decision 0266, *an utterance is a fact*) and a question
/// asserts nothing, so a `force` field would falsify the shape claim for
/// every clause in order to serve one. [`Coordination`] is the precedent: a
/// construction sitting above the clause gets its own realizer, not a flag
/// inside it (decision 0327). Unlike `Coordination` this operator needs no
/// new type at all — a polar question is the same proposition asked instead
/// of asserted, so it takes the clause it questions and nothing else.
///
/// # A lexical verb is REFUSED, loudly
///
/// English inverts an auxiliary, and the copula is the only auxiliary Common
/// has. A construction whose verb group is [`Part::Verb`] — the transitive
/// and intransitive frames — does not invert: *"Sleeps the guard?"* and
/// *"Knew you the woman?"* are not Common, and emitting either would produce
/// the plausible garbage this project's realizers refuse on principle. What
/// English actually uses there is **periphrastic *do*-support** (*"Did you
/// know the woman?"*), and that is a later campaign's: the negative half of
/// it already exists as [`VERB_PARADIGM`]'s `"did not "`/`"do not "`/
/// `"does not "` prefixes, but an interrogative *do* needs a MOOD axis on
/// that table's key, and the key is read in both directions
/// ([`verb_group_forms`]), so widening it is a parse-side change as much as
/// a realize-side one. This function panics instead — the same fail-fast
/// posture [`realize_common`] takes for an unconstructed predicate, and for
/// the same reason: an authoring hole, not a fact about the world.
///
/// Panics if `clause.predicate` names no construction, or if the
/// construction it names has no [`Part::Copula`].
/// type-audit: bare-ok(prose)
pub fn realize_common_polar_question(clause: &Clause, vocab: &CommonVocabulary) -> String {
    let construction = common_construction_for(clause);
    assert!(
        construction.parts.contains(&Part::Copula),
        "Common inverts a copula, and predicate {:?} has a lexical verb \
         instead: *\"Sleeps the guard?\"* is not Common. The English \
         question of a lexical verb is periphrastic do-support (*\"Did you \
         know the woman?\"*), which needs a mood axis on VERB_PARADIGM's \
         bidirectional key and is not this campaign's",
        clause.predicate
    );
    emit_parts(
        &invert_for_question(construction.parts),
        clause,
        vocab,
        true,
    )
}

/// Render a small cardinal number as an English word (`0` through `12`);
/// larger numbers render as plain digits.
/// type-audit: bare-ok(prose)
pub fn cardinal(n: u64) -> String {
    const WORDS: [&str; 13] = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        "eleven", "twelve",
    ];
    match WORDS.get(n as usize) {
        Some(word) => (*word).to_string(),
        None => n.to_string(),
    }
}

/// Render an approximate quantity to one decimal place, prefixed `"about "`
/// (e.g. `1.5507 -> "about 1.5"`). Truncates toward zero rather than
/// rounds, so for non-negative inputs the stated tenth is never an
/// overstatement (for negative inputs, toward-zero truncation can
/// overstate: `-1.55 -> "about -1.5"`). Non-finite inputs render
/// literally (`"about NaN"` / `"about inf"`), deterministically; callers
/// should pass finite values.
/// type-audit: bare-ok(prose)
pub fn quantity(x: f64) -> String {
    let truncated = (x * 10.0).trunc() / 10.0;
    format!("about {truncated:.1}")
}

/// Where a realized adjunct attaches. A language decides this, not a caller:
/// Common puts a day-length in a trailing clause and a moon-count inline, and
/// another tongue may do the opposite.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdjunctPosition {
    /// Inside the clause, after the complement.
    Inline,
    /// After the clause, semicolon-joined.
    Trailing,
}

/// Common's role constructions: how each registered role surfaces in the
/// author's register. `None` means Common has no construction for this role
/// yet — the adjunct renders as nothing rather than leaking its key into
/// prose, which is the same discipline `CommonVocabulary::word_for` follows.
///
/// **The first three moved here from `windows/book`'s `fragment_for`.** They
/// lived in a window because `Clause` could not carry structure, which is
/// also why that window had to duplicate `indefinite_article`. A role's
/// surface is a fact about a language and belongs to the language.
///
/// **The four `occ-*` roles are the deep-history occupation predicates**
/// (`domains/history`'s `OCC_PEOPLE`/`OCC_SITE`/`OCC_FOUNDED`/`OCC_ENDED`),
/// and they are spelled as LITERALS here for the same reason the astronomy
/// roles above are: this crate is a domain, and a domain may not depend on a
/// sibling domain, so the constants are out of reach. Contrast
/// [`common_constructions`], whose one key IS the kernel's `IS_A` constant —
/// the kernel is not a sibling. The pairing is therefore held by these
/// strings agreeing with `domains/history`'s, and by the acceptance test that
/// realizes a real occupation through them
/// (`windows/almanac/tests/suite/interlinear.rs`, which reads the ids off
/// the constants).
///
/// Position is chosen per role, which is the whole point of the return type:
/// the people, the site and the founding are all part of the noun phrase
/// being described, so they surface inline; the ending is a separate event
/// and gets its own trailing clause, exactly as `day-length-std` does.
/// A vertex renders as a bare integer rather than through [`cardinal`]
/// because it is an IDENTIFIER, not a count — a year is a count of years and
/// does go through `cardinal`.
///
/// **Refuses an [`Argument::Clause`], by panic, rather than silently
/// rendering nothing.** `Adjunct` holds an `Argument`, so the moment
/// `Argument` gained a `Clause` variant, an adjunct could carry one — and the
/// trailing `_ => None` arm below would have swallowed it, rendering an
/// adjunct that carries a whole embedded sentence as nothing at all, with no
/// error. Spec §4.1 refuses this on purpose: adverbial subordination is a
/// separate construction with its own boundary marking, and letting it
/// arrive as an unexamined side effect of the object slot's own variant is
/// exactly the "capability ships without a decision" failure this function's
/// `None` convention otherwise guards against.
/// type-audit: bare-ok(prose: return)
pub fn common_role_surface(
    adjunct: &Adjunct,
    vocab: &CommonVocabulary,
) -> Option<(AdjunctPosition, String)> {
    match (adjunct.role.as_str(), &adjunct.argument) {
        ("moon-count", Argument::Count(n)) => Some((
            AdjunctPosition::Inline,
            format!(
                "with {} moon{}",
                cardinal(*n),
                if *n == 1 { "" } else { "s" }
            ),
        )),
        ("star-class", Argument::Concept(id)) => {
            let display = vocab.word_for(id);
            Some((
                AdjunctPosition::Inline,
                format!("orbiting {} {display}", indefinite_article(&display)),
            ))
        }
        ("day-length-std", Argument::Quantity(days)) => Some((
            AdjunctPosition::Trailing,
            format!("its day lasts {} standard days", quantity(*days)),
        )),
        // The plural goes through `surface_complement`, so Common has exactly
        // one pluralization rule and this role cannot drift from the
        // complement slot's.
        ("occ-people", Argument::Concept(id)) => Some((
            AdjunctPosition::Inline,
            format!("of the {}", surface_complement(vocab, id, Number::Pl)),
        )),
        ("occ-site", Argument::Count(vertex)) => Some((
            AdjunctPosition::Inline,
            format!("in the clearing at vertex {vertex}"),
        )),
        ("occ-founded", Argument::Count(year)) => Some((
            AdjunctPosition::Inline,
            format!("founded in year {}", cardinal(*year)),
        )),
        ("occ-ended", Argument::Count(year)) => Some((
            AdjunctPosition::Trailing,
            format!("it ended in year {}", cardinal(*year)),
        )),
        (role, Argument::Clause(_)) => panic!(
            "an adjunct may not carry an embedded clause (role {role:?}): \
             adverbial subordination is a separate construction, spec §4.1"
        ),
        _ => None,
    }
}

/// The closed complement set a parse call recognizes — **concept ids**, plus
/// the vocabulary that says how each one surfaces. Parsing is the inverse of
/// [`realize_common`], so it recovers the id the realizer started from, not
/// the word it ended at; both halves therefore need the same vocabulary, and
/// carrying it here is what keeps every caller's signature a single context
/// argument. Longest-match wins on the SURFACE form when one complement's
/// surface is a prefix of another's (`"dwarf"` vs. `"yellow-white dwarf"`).
/// type-audit: bare-ok(identifier-text: complements)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseContext {
    /// The recognized complement concept ids, e.g. `"planet"`,
    /// `"yellow-white-dwarf"`.
    pub complements: std::collections::BTreeSet<String>,
    /// How each id surfaces — the same vocabulary [`realize_common`] used.
    pub vocabulary: CommonVocabulary,
}

/// Why `parse_common` refused to invert a sentence — each variant is a
/// recountable, specific reason rather than a bare "parse failed". These
/// are the parser's three (and only three) failure modes: text after a
/// matched complement is empty or space-prefixed by the complement
/// filter's construction, so no "bad tail" failure exists.
/// type-audit: bare-ok(prose: UnknownComplement.after)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    /// No construction's verb group appears — neither a
    /// [`COPULA_PARADIGM`] form nor any [`VERB_PARADIGM`] form of any
    /// construction's stem — so no subject/verb split exists.
    ///
    /// Named `NoCopula` until Common gained a construction whose verb is
    /// not the copula; the old name would now under-report what was
    /// searched.
    NoVerbGroup,
    /// The text after the determiner doesn't match (a prefix of) any
    /// complement surface in the caller's `ParseContext`, at any of the
    /// numbers the verb group left open — and (The Mortise, Task 8) neither
    /// does a one-level recursive attempt at that same text as an embedded
    /// clause, whether because the recursion itself failed, the shared
    /// depth budget was already spent, or a matched embedded clause's
    /// number could not be resolved without a complement to disambiguate
    /// it (`resolve_embedded_number`). Also carries a top-level `" and "`
    /// in the whole clause body: that marks a
    /// [`Coordination`], which is refused before any split is attempted
    /// rather than misread as an embedding (see
    /// `parse_common_with_tail`'s doc).
    UnknownComplement {
        /// The unrecognized text following the determiner, or (for a
        /// top-level coordination marker) the whole clause body that was
        /// refused before any split.
        after: String,
    },
    /// The text has no terminal `.`, so the construction's final literal
    /// never matched.
    Unterminated,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::NoVerbGroup => {
                write!(f, "no construction's verb group found in the sentence")
            }
            ParseError::UnknownComplement { after } => {
                write!(f, "no registered complement matches '{after}'")
            }
            ParseError::Unterminated => write!(f, "sentence has no terminal '.'"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Invert `realize_common`: parse a Common sentence back into the
/// `Clause` that would realize it. Walks a construction's entry backward —
/// the boundaries come from the construction's shape, and the subject/verb
/// split happens at the EARLIEST occurrence of any construction's verb-group
/// form (so a subject itself never contains the verb word), longest form
/// winning a tie on position. `predicate`, `tense` and `polarity` come back
/// off that form; `number` comes off it too unless the form is syncretic
/// (`VERB_PARADIGM`'s past), in which case the object's own plural decides.
/// Complement surfaces in `ctx` must not begin with a determiner word
/// (`"a "`/`"an "`/`"the "`) — the bare-plural path would misparse them;
/// today's vocabulary (single words and hyphenated compounds) satisfies
/// this.
///
/// Returns a spec whose `object` is `Argument::Concept(<the concept id>)`,
/// recovered by matching the text against each candidate id's realized
/// surface (its Common word, pluralized for a plural clause) — the exact
/// inverse of [`realize_common`]'s complement slot, now that the realizer
/// resolves rather than echoes.
///
/// **The adjunct tail is not recovered: the returned `adjuncts` is always
/// empty.** Common recognizes the clause skeleton, not its role
/// constructions — spec §6 of The Interlinear freezes parsing coverage
/// where it was, and recognizing a role's surface is its own campaign. A
/// caller that still needs the tail's TEXT (today: `windows/book`, which
/// owns an English recognizer of its own) takes it from
/// [`parse_common_with_tail`], which is the same walk with the tail
/// returned instead of dropped.
///
/// **Caller-less by design, and deliberately kept.** Nothing in the
/// workspace calls this: it is the "give me a clause, not English" front
/// door, and The Interlinear's reviewer recommended keeping it against a
/// dead-code sweep that would see an undefended `pub fn`. Its inverse,
/// [`realize_common`], is what makes the pairing bidirectional by
/// construction; deleting this half would make a future `parse_tongue` a
/// new invention rather than a second instance.
/// type-audit: bare-ok(prose)
pub fn parse_common(text: &str, ctx: &ParseContext) -> Result<Clause, ParseError> {
    parse_common_with_tail(text, ctx).map(|(spec, _)| spec)
}

/// [`parse_common`], plus the raw adjunct tail it does not structure: the
/// `", "`-separated phrases that followed the complement, verbatim.
///
/// Two functions rather than one because the tail is a **loss**, not a
/// product: a caller that asks for a `Clause` should not be handed
/// English it then has to recognize, and the one caller that does own a
/// recognizer should have to say so at the call site.
///
/// **Deletion condition, named rather than left in prose alone:** this
/// function and its caller's recognizer (`windows/book::fact_for`) are a
/// matched pair that goes away together, when Common learns to recognize
/// its own role constructions instead of only its clause skeleton — see the
/// idea-registry row `LANG-recognition-seam`.
/// type-audit: bare-ok(prose: text), bare-ok(prose: return)
pub fn parse_common_with_tail(
    text: &str,
    ctx: &ParseContext,
) -> Result<(Clause, Vec<String>), ParseError> {
    // Terminal literal first.
    let body = text.strip_suffix('.').ok_or(ParseError::Unterminated)?;
    parse_clause_body(body, ctx, 0)
}

/// Resolve the `Number` of a clause whose object recursed into an
/// [`Argument::Clause`], where — unlike the non-embedded path just below —
/// there is no complement surface to disambiguate a syncretic verb-group
/// form. Two independent signals are tried, and the number is returned only
/// when one of them is unambiguous:
///
/// 1. **The verb group itself.** `numbers` is the same candidate set
///    [`parse_clause_body`]'s non-embedded path already computes (every
///    `Number` a matched form is consistent with, **after** the person
///    narrowing that path applies) — the present tense is injective on
///    number *in the third person* (`"knows"` vs `"know"`), so this alone
///    resolves a present-tense matrix clause whose subject is a name or
///    `"they"`.
///
///    **That injectivity used to be unconditional and is now person-
///    relative** (The Rail, `r011`). Under the number-only key, `"know"`
///    named the plural and nothing else. It now names 1sg, 2sg, 1pl, 2pl
///    and 3pl, so the third-person filter is what restores the singleton —
///    and a first- or second-person subject falls through to signal 2,
///    where `"I"`/`"we"` resolve and `"you"` does not.
/// 2. **The subject's own pronoun row.** [`PRONOUN_PARADIGM`]'s nominative
///    forms are not uniformly ambiguous: `"I"`/`"we"` name exactly one
///    `Number` each (English spells first person differently by number),
///    while `"they"`/`"you"` do not (spec §4.5 reuses `"they"` for third
///    singular). When the subject text names exactly one row, that row's
///    number is intersected with `numbers` — never used alone, so a
///    genuinely inconsistent pairing (which no realizer produces) still
///    fails closed rather than returning a number the verb group rejects.
///
/// `None` when neither signal is unambiguous — the embedding is refused
/// rather than guessed, matching this task's stop-and-report posture for
/// anything past the small extension it was sanctioned to build.
fn resolve_embedded_number(numbers: &[Number], subject_text: &str) -> Option<Number> {
    if let [n] = numbers {
        return Some(*n);
    }
    let pronoun_rows: Vec<Number> = PRONOUN_PARADIGM
        .iter()
        .filter(|(form, _, _, case)| *case == PronounCase::Nominative && *form == subject_text)
        .map(|(_, _, n, _)| *n)
        .collect();
    match pronoun_rows.as_slice() {
        [n] if numbers.contains(n) => Some(*n),
        _ => None,
    }
}

/// [`parse_common_with_tail`]'s own body (The Mortise, Task 8), now able to
/// recurse at the point the walk used to give up outright, and taught to
/// refuse a coordination sentence before ever attempting that recursion.
///
/// `depth` reads the SAME shared embedding budget the realize side spends
/// ([`clause_embed_depth`]/[`subject_embed_depth`] against
/// [`CLAUSE_EMBED_MAX_DEPTH`]): the outermost call is depth `0`, and one
/// recursive attempt at the unresolved remainder is spent going to depth
/// `1` — past the cap, the walk reports the failure it already had rather
/// than recursing further. Only the OBJECT slot recurses here
/// (`Argument::Clause`): a clause bound to the SUBJECT slot would need the
/// walk to try more than the earliest verb-group occurrence as the
/// subject/verb split, since the subject's own inner verb group is the one
/// that occurs first — a different, backtracking algorithm this task does
/// not build (see `parse_common_with_tail`'s own doc and this campaign's
/// task report for the reasoning; `windows/book`'s `Subject::Clause`
/// `unreachable!()` therefore stays accurate).
///
/// **The boundary marker discriminates, and the check runs BEFORE the
/// verb-group split, not after it (the controller finding this task
/// shipped against).** Common's coordination marker is the fixed word
/// `"and"` (Task 6); its embedding marker is NO WORD AT ALL (Task 5's drawn
/// complementizer is a tongue-side feature Common does not spend). So a
/// body containing a top-level `" and "` names a [`Coordination`], never a
/// single [`Clause`] — and the earliest-verb-group split would otherwise
/// land on the FIRST conjunct's own verb, leaving a remainder that, in
/// general, legitimately CAN contain a real registered complement further
/// in (a second conjunct that happens to be a plain classification), which
/// would then recurse successfully and misreport the second conjunct as a
/// clause embedded under the first conjunct's predicate — plausible
/// garbage, not a parse. Refusing the instant the marker is seen, before
/// any split is attempted, closes that hole at both the outermost call and
/// every recursive one: a [`Coordination`] can never itself be embedded
/// (`Argument::Clause`/`Subject::Clause` wrap a single [`Clause`], never a
/// list of them), so the identical check is correct at every depth, not
/// just depth `0`. **Parsing a [`Coordination`] back out of its own text is
/// not attempted** — spec §6's own success criterion asks only that
/// `parse_common` round-trip embedding and DISTINGUISH the two operators by
/// this marker, not that it recover a `Coordination`; building that
/// (including inverting tier 2's elided-subject reattribution,
/// [`elide_coordinated_subjects`]'s own inverse) is out of this task's
/// scope, named rather than silently absent.
fn parse_clause_body(
    body: &str,
    ctx: &ParseContext,
    depth: usize,
) -> Result<(Clause, Vec<String>), ParseError> {
    if body.contains(" and ") {
        return Err(ParseError::UnknownComplement {
            after: body.to_string(),
        });
    }
    // Subject | verb group: every construction contributes every surface its
    // verb group can take (a copula form, or its own stem run through
    // `VERB_PARADIGM`), and the sentence is searched for all of them at once.
    // This is where the construction table's "bidirectional by construction"
    // promise is actually cashed: the predicate comes back from the row that
    // matched, never from an assumption about which construction this was.
    let mut hits: Vec<(usize, String, &'static str, Tense, Number, Polarity, Person)> = Vec::new();
    for construction in common_constructions() {
        for (form, tense, number, polarity, person) in
            verb_group_forms(construction, &ctx.vocabulary)
        {
            if let Some(at) = body.find(&format!(" {form} ")) {
                hits.push((
                    at,
                    form,
                    construction.predicate,
                    tense,
                    number,
                    polarity,
                    person,
                ));
            }
        }
    }
    // Split at the EARLIEST occurrence (so a subject never contains the verb
    // group), and at a tie on position take the LONGEST form — `" is not "`
    // and `" is "` start at the same index, and the negated reading is the
    // one that consumes the whole group. A tie on both goes to the
    // lexicographically first predicate: deterministic, and unreachable
    // today, since it would need one construction's inflected verb to equal
    // another's at the same offset. The sort is STABLE, so rows that tie on
    // all three stay in paradigm order.
    hits.sort_by(|a, b| {
        a.0.cmp(&b.0)
            .then_with(|| b.1.len().cmp(&a.1.len()))
            .then_with(|| a.2.cmp(b.2))
    });
    // `tense` and `polarity` are read off the winning row and are safe to
    // take from it alone: no form in either table spans two tenses or two
    // polarities, which `no_verb_group_form_spans_two_tenses_or_polarities`
    // asserts against the tables rather than leaving to inspection. `number`
    // and `person` are NOT safe to take that way, and both are dropped here
    // (`_`) and recovered below from signals that do determine them.
    let Some((at, form, predicate, tense, _, polarity, _)) = hits.first().cloned() else {
        return Err(ParseError::NoVerbGroup);
    };
    // The needle was `" {form} "`, so the remainder starts one space past the
    // form, which itself started one space past `at`.
    let (subject_text, rest) = (&body[..at], &body[at + form.len() + 2..]);
    // A nominative pronoun binds to its PERSON; the number is the one the
    // verb group is already recovering, so nothing is read twice. `"its"`
    // used to bind here as a pronoun and no longer does — it is a
    // possessive, which no person/number row names, and no realizer ever
    // produced it (see `Subject`'s doc). It now falls through to `Name`,
    // which re-realizes to the identical surface.
    let subject = match nominative_person(subject_text) {
        Some(person) => Subject::Pronoun(person),
        None => Subject::Name(subject_text.to_string()),
    };
    // **Person is recovered from the SUBJECT, then used to narrow the verb
    // group** — the ordering matters, which is why the subject is bound
    // above rather than after the number search it now feeds.
    //
    // Since The Rail widened both paradigms by [`Person`], a verb-group form
    // can name several rows differing in person as well as in number (`are`
    // names four copula rows; a bare stem names five verb rows). The subject
    // states person unambiguously — Common's nominative pronouns are
    // pairwise distinguishing on person, and anything that is not one of
    // them is a `Subject::Name`, which is third person by
    // [`Subject::person`] — so intersecting on it discards the rows that
    // cannot belong to THIS sentence before number is ever asked about.
    //
    // The intersection can come back EMPTY, and that is a real verdict
    // rather than a case to paper over: it means the text disagrees with
    // itself about agreement (*"I are a planet."*), which no realization
    // produces. An empty `numbers` admits no complement candidate below, so
    // the walk reports the `UnknownComplement` it already had for text it
    // cannot invert — no new failure shape, and
    // `an_agreement_violating_sentence_does_not_parse` pins it so the
    // narrowing is observable rather than a guard nothing exercises.
    let person = subject.person();
    let numbers: Vec<Number> = hits
        .iter()
        .filter(|h| h.0 == at && h.1 == form && h.2 == predicate && h.6 == person)
        .map(|h| h.4)
        .collect();
    // Determiner.
    let (definiteness, after_det) = if let Some(r) = rest.strip_prefix("the ") {
        (Definiteness::Def, r)
    } else if let Some(r) = rest.strip_prefix("an ") {
        (Definiteness::Indef, r)
    } else if let Some(r) = rest.strip_prefix("a ") {
        (Definiteness::Indef, r)
    } else {
        (Definiteness::Indef, rest) // bare plural generic
    };
    // Complement: longest SURFACE match over the closed set of ids CROSSED
    // with the numbers the verb group left open. Ties go to the last
    // (id, number) pair in iteration order — ids in `complements`' (BTreeSet)
    // order, numbers in paradigm order — which reproduces `max_by_key`'s
    // last-wins rule exactly; unreachable today, since no two ids share a
    // surface (`cli/tests/suite/common_is_total.rs` guards that) and a
    // concept's singular and plural surfaces always differ by the naive
    // `'s'`. That last fact is what recovers number from a syncretic past.
    let mut best_complement: Option<(String, Number, String)> = None;
    for concept in &ctx.complements {
        for &candidate in &numbers {
            let s = surface_complement(&ctx.vocabulary, concept, candidate);
            let matched = after_det == s.as_str()
                || after_det
                    .strip_prefix(s.as_str())
                    .is_some_and(|r| r.starts_with(' '));
            if !matched {
                continue;
            }
            if best_complement
                .as_ref()
                .is_none_or(|(_, _, best)| s.len() >= best.len())
            {
                best_complement = Some((concept.clone(), candidate, s));
            }
        }
    }
    let (complement_concept, number, surface) = match best_complement {
        Some(v) => v,
        // The give-up point widens here (The Mortise, Task 8): before
        // reporting failure, spend one level of the shared depth budget on
        // a recursive attempt at the unresolved remainder — the exact
        // inverse of `realize_common`'s own `Argument::Clause` arm, which
        // realizes an embedded clause's own full text in this same spot
        // and trims its trailing period.
        None => {
            if depth < CLAUSE_EMBED_MAX_DEPTH {
                match parse_clause_body(after_det, ctx, depth + 1) {
                    Ok((inner, _inner_tail)) => {
                        // No complement surface exists here to disambiguate
                        // a syncretic verb-group number (past tense shares
                        // one form across Sg/Pl) the way the non-embedded
                        // path does below, so the number must be resolved
                        // some other way or the embedding is refused rather
                        // than guessed.
                        if let Some(number) = resolve_embedded_number(&numbers, subject_text) {
                            // An embedded clause's own text carries no
                            // determiner (`realize_common`'s `Part::Determiner`
                            // arm skips `Argument::Clause`/`Argument::Pronoun`
                            // objects outright), so nothing in the surface
                            // states the matrix clause's own definiteness —
                            // the same kind of loss `evidential` already
                            // documents for every Common clause. `Indef` is
                            // the same default the bare-plural-generic branch
                            // above already falls back to when no determiner
                            // word is found, reused here for the identical
                            // reason.
                            return Ok((
                                Clause {
                                    predicate: predicate.to_string(),
                                    subject,
                                    object: Argument::Clause(Box::new(inner)),
                                    number,
                                    definiteness: Definiteness::Indef,
                                    evidential: Evidential::Witnessed,
                                    tense,
                                    polarity,
                                    adjuncts: Vec::new(),
                                },
                                Vec::new(),
                            ));
                        }
                    }
                    // Propagate the recursive attempt's own `UnknownComplement`
                    // — it names exactly where the walk actually stopped (at
                    // the cap, or on an unrecognized complement one level
                    // down), which is more specific than restating this
                    // level's own remainder. A `NoVerbGroup` from the
                    // recursive attempt is a DIFFERENT signal ("this text
                    // isn't clause-shaped at all") and must not leak upward
                    // dressed as a complement failure — falling through to
                    // this level's own `UnknownComplement` below is what
                    // keeps a plain unresolvable complement (no embedding
                    // possible at all, e.g. "Vebe is a carriage." against a
                    // `ParseContext` that only registers "planet") reporting
                    // the same failure shape it always has.
                    Err(e @ ParseError::UnknownComplement { .. }) => return Err(e),
                    Err(_) => {}
                }
            }
            return Err(ParseError::UnknownComplement {
                after: after_det.to_string(),
            });
        }
    };
    // Adjunct tail: '' or ' m1' or ' m1, m2, …'. The complement filter
    // above only admits candidates whose remainder is empty or starts
    // with ' ', so by construction `tail` is one of exactly those two
    // shapes — no third case exists to report.
    let tail = &after_det[surface.len()..];
    let tail_text: Vec<String> = match tail.strip_prefix(' ') {
        Some(t) => t.split(", ").map(str::to_string).collect(),
        None => Vec::new(),
    };
    Ok((
        Clause {
            // The construction this walk actually inverted, read off the
            // verb group that matched — never a literal and no longer an
            // assumption, so a second construction cannot silently come back
            // as the first.
            predicate: predicate.to_string(),
            subject,
            object: Argument::Concept(complement_concept),
            number,
            definiteness,
            // Common has no evidential construction (spec §3.2), so the
            // surface carries nothing to recover one from: a round trip
            // through Common is lossy in exactly this feature. `Witnessed`
            // is the documented default the inverse direction
            // (`windows/book`'s `rerender`) must also use, so the two ends
            // agree; it is never read out of a parse as a claim about how
            // the original speaker was grounded.
            evidential: Evidential::Witnessed,
            // Both READ OFF the copula, unlike `evidential`: Common has a
            // construction for each, so neither needs a documented default.
            tense,
            polarity,
            adjuncts: Vec::new(),
        },
        tail_text,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Spec 3.2: a language-neutral clause states more than any one language
    /// surfaces. Common has no evidential construction, so the same clause
    /// under all three groundings is the same sentence -- and this is the
    /// law, not a gap to be fixed. A later campaign teaching Common an
    /// evidential surface has to delete an assertion deliberately rather
    /// than drift past it.
    #[test]
    fn common_ignores_the_evidential() {
        let vocab = CommonVocabulary::default();
        let base = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("home".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let taught = Clause {
            evidential: Evidential::Taught,
            ..base.clone()
        };
        let inferred = Clause {
            evidential: Evidential::Inferred,
            ..base.clone()
        };
        assert_eq!(
            realize_common(&base, &vocab),
            realize_common(&taught, &vocab)
        );
        assert_eq!(
            realize_common(&base, &vocab),
            realize_common(&inferred, &vocab)
        );
    }

    /// **The syncretism, and exactly what it costs the backward read.**
    ///
    /// This test replaces `copula_paradigm_is_total_and_unambiguous`, whose
    /// name stated a fact that stopped being true when The Rail widened the
    /// key by [`Person`] (`r011`). That test asserted `forms.len() ==
    /// COPULA_PARADIGM.len()` — eight rows, eight distinct words, a table
    /// that was a bijection. English spells `are` for 2sg, 1pl, 2pl and
    /// 3pl, and `were` for the same four, so 24 rows now spell ten forms
    /// and the bijection is gone in one direction only.
    ///
    /// The FORWARD direction stays total: every `(tense, number, polarity,
    /// person)` has exactly one surface, which is the property that keeps
    /// [`copula_surface`]'s `expect` unreachable and is the only thing
    /// [`realize_common`] asks of the table. It is asserted rather than
    /// assumed because the paradigm is a `const` slice, not a match — the
    /// compiler cannot check its exhaustiveness the way it checked
    /// `Frame`'s.
    ///
    /// The BACKWARD direction cannot recover person from `are`. That loss
    /// is PINNED here by naming the four rows that share the form, rather
    /// than papered over — the same posture the round-trip property takes
    /// toward adjuncts and toward `evidential`. It is not, however, a loss
    /// the PARSER suffers: person is recovered from the subject's own
    /// surface instead (see [`verb_group_forms`] for why no row here is
    /// nominated canonical). A future campaign that made the copula itself
    /// carry person — subject agreement on a suffix, say — would land here
    /// as a red test.
    #[test]
    fn the_copula_paradigm_is_total_forward_and_syncretic_backward() {
        // Forward totality: every combination has exactly one form, and
        // `copula_surface` panics if the table is missing a row.
        let mut combinations = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for number in [Number::Sg, Number::Pl] {
                for polarity in [Polarity::Pos, Polarity::Neg] {
                    for person in Person::ALL {
                        let _ = copula_surface(tense, number, polarity, person);
                        combinations += 1;
                    }
                }
            }
        }
        assert_eq!(combinations, 24);
        assert_eq!(COPULA_PARADIGM.len(), 24);
        // Backward loss, stated by name: these four rows share one surface.
        let shared: Vec<&'static str> = [
            (Number::Sg, Person::Second),
            (Number::Pl, Person::First),
            (Number::Pl, Person::Second),
            (Number::Pl, Person::Third),
        ]
        .iter()
        .map(|(n, p)| copula_surface(Tense::Present, *n, Polarity::Pos, *p))
        .collect();
        assert!(
            shared.iter().all(|f| *f == "are"),
            "the present positive syncretism is exactly these four rows: {shared:?}"
        );
        // The past does the same, on the same four rows.
        let past: Vec<&'static str> = [
            (Number::Sg, Person::Second),
            (Number::Pl, Person::First),
            (Number::Pl, Person::Second),
            (Number::Pl, Person::Third),
        ]
        .iter()
        .map(|(n, p)| copula_surface(Tense::Past, *n, Polarity::Pos, *p))
        .collect();
        assert!(
            past.iter().all(|f| *f == "were"),
            "the past positive syncretism is the same four rows: {past:?}"
        );
        // Ten distinct forms across 24 rows — the exact size of the parse
        // direction's candidate string set, and one more than the eight the
        // number-only table carried (`am` is the only new WORD).
        let forms: std::collections::BTreeSet<&str> =
            COPULA_PARADIGM.iter().map(|(f, _, _, _, _)| *f).collect();
        assert_eq!(forms.len(), 10, "{forms:?}");
        assert!(forms.contains("am"));
    }

    /// The campaign's motivating defect: a settlement whose people left six
    /// hundred years ago was still said to *be* their home, because Common's
    /// copula slot read `number` alone. Tense is STATED (spec 3.3) — the
    /// caller, which knows both the fact's `WorldTime` and the utterance's,
    /// supplies the relation; the realizer surfaces what it is told.
    #[test]
    fn a_past_clause_says_was() {
        let vocab = CommonVocabulary::default();
        let base = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("home".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert!(realize_common(&base, &vocab).contains(" is "));
        let past = Clause {
            tense: Tense::Past,
            ..base.clone()
        };
        let out = realize_common(&past, &vocab);
        assert!(out.contains(" was "), "past tense must say was: {out}");
        assert!(!out.contains(" is "), "and must not also say is: {out}");
    }

    /// Polarity is a property OF the clause (spec 3.3), unlike tense, so it
    /// needs no deictic centre and is recoverable from the surface.
    #[test]
    fn a_negated_clause_says_is_not() {
        let vocab = CommonVocabulary::default();
        let neg = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("home".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let out = realize_common(&neg, &vocab);
        assert!(out.contains("is not"), "got: {out}");
    }

    /// The Construction table realizes forward and parses backward. Both new
    /// features are recoverable from the surface, unlike `evidential`, so the
    /// round trip must return them unchanged across the whole
    /// tense x polarity x number space — the copula paradigm is exactly where
    /// the three interact, so enumerating all eight is the cheap complete
    /// test rather than a sample.
    #[test]
    fn common_round_trips_tense_and_polarity() {
        let mut seen = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for polarity in [Polarity::Pos, Polarity::Neg] {
                for number in [Number::Sg, Number::Pl] {
                    let spec = Clause {
                        predicate: IS_A.to_string(),
                        subject: Subject::Name("Nwamvam".to_string()),
                        object: Argument::Concept("planet".to_string()),
                        number,
                        definiteness: Definiteness::Indef,
                        evidential: Evidential::Witnessed,
                        tense,
                        polarity,
                        adjuncts: Vec::new(),
                    };
                    let ctx = ctx_from(&spec);
                    let text = realize_common(&spec, &ctx.vocabulary);
                    assert_eq!(
                        parse_common(&text, &ctx),
                        Ok(spec.clone()),
                        "round-trip failed for {text:?}"
                    );
                    seen += 1;
                }
            }
        }
        assert_eq!(seen, 8);
    }

    /// An adjunct binds a registered predicate (its role) to an argument.
    #[test]
    fn an_adjunct_binds_a_registered_predicate_to_an_argument() {
        let a = Adjunct {
            role: "moon-count".to_string(),
            argument: Argument::Count(2),
        };
        assert_eq!(a.role, "moon-count");
        assert_eq!(a.argument, Argument::Count(2));
    }

    /// Common resolves its complement through the vocabulary, exactly as the
    /// tongue path resolves through a lexicon. Symmetry is the point: before
    /// this, the caller chose the word and no layer could ask whether the
    /// concept was sayable at all.
    #[test]
    fn common_resolves_its_complement_through_the_vocabulary() {
        let mut vocab = CommonVocabulary::default();
        vocab.declare("yellow-white-dwarf", "yellow-white dwarf (F)");
        let spec = Clause {
            predicate: "is-a".to_string(),
            subject: Subject::Name("Elthandil".to_string()),
            object: Argument::Concept("yellow-white-dwarf".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        assert_eq!(
            realize_common(&spec, &vocab),
            "Elthandil is a yellow-white dwarf (F)."
        );
    }

    /// A hyphenated id never reaches prose wearing its hyphen. This is the
    /// test that would have caught `*celestial-body*` shipping to the gallery.
    #[test]
    fn a_key_never_reaches_prose_as_a_key() {
        let vocab = CommonVocabulary::default();
        let spec = Clause {
            predicate: "is-a".to_string(),
            subject: Subject::Name("X".to_string()),
            object: Argument::Concept("celestial-body".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let line = realize_common(&spec, &vocab);
        assert_eq!(line, "X is a celestial body.");
        assert!(
            !line.contains('-'),
            "a key wore its hyphen into prose: {line}"
        );
    }

    #[test]
    fn classify_singular_indefinite() {
        let s = Clause {
            predicate: "is-a".into(),
            subject: Subject::Name("Elthandil".into()),
            object: Argument::Concept("planet".into()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
            "Elthandil is a planet."
        );
    }
    #[test]
    fn a_becomes_an_before_vowel() {
        let s = Clause {
            predicate: "is-a".into(),
            subject: Subject::Name("Aoth".into()),
            object: Argument::Concept("elemental".into()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
            "Aoth is an elemental."
        );
    }
    /// The collective construction: a plural clause pluralizes the resolved
    /// word itself (`goblin-kind` → `goblin` → `goblins`). Before The
    /// Vernacular the caller pre-pluralized and handed the realizer a string.
    #[test]
    fn classify_generic_plural() {
        let s = Clause {
            predicate: "is-a".into(),
            subject: Subject::Name("The Vavako".into()),
            object: Argument::Concept("goblin-kind".into()),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
            "The Vavako are goblins."
        );
    }

    /// The adjunct tail's surface, unchanged from the pre-Interlinear
    /// `modifiers` join: a space before the first inline adjunct, `", "`
    /// between the rest. The caller now states the ROLES; the realizer
    /// renders them.
    #[test]
    fn classify_with_modifier_tail() {
        let mut vocab = CommonVocabulary::default();
        vocab.declare("yellow-white-dwarf", "yellow-white dwarf");
        let s = Clause {
            predicate: "is-a".into(),
            subject: Subject::Name("Vebe".into()),
            object: Argument::Concept("planet".into()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![
                Adjunct {
                    role: "moon-count".into(),
                    argument: Argument::Count(2),
                },
                Adjunct {
                    role: "star-class".into(),
                    argument: Argument::Concept("yellow-white-dwarf".into()),
                },
            ],
        };
        assert_eq!(
            realize_common(&s, &vocab),
            "Vebe is a planet with two moons, orbiting a yellow-white dwarf."
        );
    }
    #[test]
    fn cardinal_words() {
        assert_eq!(cardinal(2), "two");
        assert_eq!(cardinal(12), "twelve");
        assert_eq!(cardinal(13), "13");
    }
    #[test]
    fn quantity_rounds() {
        assert_eq!(quantity(1.5507), "about 1.5");
    }

    #[test]
    fn the_construction_table_is_keyed_by_predicate() {
        let inv = common_constructions();
        // Against each OWNER's constant, not the literal it happens to
        // equal: the table's key and every producer's key are the same
        // `const` by construction, which is what replaced the static
        // totality `Frame::Classify` used to give this lookup. `IS_A` is the
        // kernel's; `EAT` is this crate's, because `packs.rs` is what
        // registers that concept.
        let classify = inv
            .iter()
            .find(|c| c.predicate == hornvale_kernel::world::IS_A)
            .expect("the classification construction");
        assert_eq!(
            classify.parts,
            &[
                Part::Subject,
                Part::Literal(" "),
                Part::Copula,
                Part::Literal(" "),
                Part::Determiner,
                Part::Complement,
                Part::ModifierTail,
                Part::Literal("."),
            ]
        );
        let transitive = inv
            .iter()
            .find(|c| c.predicate == EAT)
            .expect("the transitive construction");
        assert_eq!(
            transitive.parts,
            &[
                Part::Subject,
                Part::Literal(" "),
                Part::Verb,
                Part::Literal(" "),
                Part::Determiner,
                Part::Complement,
                Part::ModifierTail,
                Part::Literal("."),
            ]
        );
        // Every row's predicate is distinct: `realize_common` takes the
        // FIRST match, so a duplicate key would make one row unreachable
        // and the table's "add a row, never a code path" promise a lie.
        let keys: std::collections::BTreeSet<&str> = inv.iter().map(|c| c.predicate).collect();
        assert_eq!(keys.len(), inv.len());
    }

    /// The predicate inventory is ONE table, and this pins what "selected by
    /// valence" means so the claim cannot quietly become decorative.
    ///
    /// Not an agreement test between two tables — there is only one, which is
    /// the point of the design. It asserts the DERIVATION: every construction
    /// has a valence, every valence has a construction, and each part list is
    /// the one its valence names. A future per-row override (the widening
    /// `common_constructions`' doc anticipates) would redden this and should:
    /// it is the moment the tongue's `predicate_valence` stops being derivable
    /// from Common's parts, which is exactly when someone should look.
    #[test]
    fn common_parts_are_selected_by_the_predicates_valence() {
        let inv = common_constructions();
        for construction in inv {
            let valence = predicate_valence(construction.predicate)
                .expect("every construction's predicate is in the inventory");
            match valence {
                Valence::Nominal => {
                    assert!(
                        construction.parts.contains(&Part::Copula)
                            && !construction.parts.contains(&Part::Verb),
                        "a nominal predication fills the verb slot with a copula: {:?}",
                        construction.predicate
                    );
                }
                Valence::Transitive => {
                    assert!(
                        construction.parts.contains(&Part::Verb)
                            && !construction.parts.contains(&Part::Copula),
                        "a transitive clause fills the verb slot with a lexical verb: {:?}",
                        construction.predicate
                    );
                }
                // The intransitive frame asserts the same pair the
                // transitive arm does — a lexical verb, no copula — since
                // both fill the verb slot with the predicate itself; the
                // difference (no complement) is not this test's business.
                Valence::Intransitive => {
                    assert!(
                        construction.parts.contains(&Part::Verb)
                            && !construction.parts.contains(&Part::Copula),
                        "an intransitive clause fills the verb slot with a lexical verb: {:?}",
                        construction.predicate
                    );
                }
                // A property predication is copular, like `Nominal` — but,
                // unlike `Nominal`, it has no second participant: no
                // `Part::Determiner` and no `Part::Complement`. Those two
                // absences are the whole content of the m02 fix, so both
                // are asserted here rather than left to `Part::PredicateWord`
                // alone to imply.
                Valence::Property => {
                    assert!(
                        construction.parts.contains(&Part::Copula)
                            && !construction.parts.contains(&Part::Verb)
                            && construction.parts.contains(&Part::PredicateWord)
                            && !construction.parts.contains(&Part::Determiner)
                            && !construction.parts.contains(&Part::Complement),
                        "a property predication fills the verb slot with a copula and the \
                         predicate slot with the property word, and binds no determiner or \
                         complement: {:?}",
                        construction.predicate
                    );
                }
                // A locative predication is copular, like `Nominal` — but
                // unlike `Property`, it binds a second participant: both
                // `Part::Determiner` and `Part::Complement` are present, for
                // the located thing, alongside `Part::PredicateWord` for
                // the relation itself.
                Valence::Locative => {
                    assert!(
                        construction.parts.contains(&Part::Copula)
                            && !construction.parts.contains(&Part::Verb)
                            && construction.parts.contains(&Part::PredicateWord)
                            && construction.parts.contains(&Part::Determiner)
                            && construction.parts.contains(&Part::Complement),
                        "a locative predication fills the verb slot with a copula and the \
                         predicate slot with the relation, and binds both a determiner and a \
                         complement: {:?}",
                        construction.predicate
                    );
                }
            }
        }
        // And the other direction: nothing in the inventory is unreachable
        // from Common. `predicate_valence` is what the TONGUE path asks, so
        // a predicate it answers for and Common cannot realize would panic
        // in one realizer and not the other.
        assert_eq!(
            inv.len(),
            [IS_A, EAT, KILL, KNOW, THINK, SLEEP, OLD, UNDER]
                .iter()
                .filter(|p| predicate_valence(p).is_some())
                .count(),
            "every inventory row realizes in Common"
        );
        assert_eq!(predicate_valence(IS_A), Some(Valence::Nominal));
        assert_eq!(predicate_valence(EAT), Some(Valence::Transitive));
        // `kill` is the second transitive verb, and the reason it is only a
        // row: it shares `eat`'s part list rather than earning one.
        assert_eq!(predicate_valence(KILL), Some(Valence::Transitive));
        // `know` (The Mortise, Task 1) is the third: same derivation, same
        // shared part list, no new construction.
        assert_eq!(predicate_valence(KNOW), Some(Valence::Transitive));
        // `think` (The Mortise, Task 2) is the fourth: same derivation,
        // same shared part list, no new construction.
        assert_eq!(predicate_valence(THINK), Some(Valence::Transitive));
        // `sleep` (The Rail, Task 2) is the first row at a NEW valence: the
        // lever this campaign is named for.
        assert_eq!(predicate_valence(SLEEP), Some(Valence::Intransitive));
        // `old` (The Rail, Task 4) is the first row at `Valence::Property`,
        // the m02 trap's honest fix.
        assert_eq!(predicate_valence(OLD), Some(Valence::Property));
        // `under` (The Rail, Task 5) is the first row at `Valence::Locative`,
        // and the last valence this campaign adds.
        assert_eq!(predicate_valence(UNDER), Some(Valence::Locative));
        assert_eq!(predicate_valence("dwells-in"), None);
    }

    /// The transitive demonstration clause: `Nwamvam <eat> the bread`. The
    /// predicate is `eat` because it is `ConceptKind::Act`, `ladder_rank: 0`
    /// and universal (spec §4.3) — the flagship must not be built on a
    /// vocabulary gap.
    fn eat_clause(tense: Tense, number: Number, polarity: Polarity) -> Clause {
        Clause {
            predicate: EAT.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("bread".to_string()),
            number,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense,
            polarity,
            adjuncts: Vec::new(),
        }
    }

    /// The first construction that is not the copula: the clause's own
    /// PREDICATE surfaces as a lexical verb between subject and object,
    /// resolved through the same vocabulary the complement goes through.
    #[test]
    fn a_transitive_clause_surfaces_its_predicate_as_a_verb() {
        let vocab = CommonVocabulary::default();
        assert_eq!(
            realize_common(
                &eat_clause(Tense::Present, Number::Sg, Polarity::Pos),
                &vocab
            ),
            "Nwamvam eats the bread."
        );
    }

    /// The verb group across the whole `tense x number x polarity` space —
    /// the same three features the copula carries, one level up because the
    /// surface is STEM-dependent.
    ///
    /// **`eated` is asserted deliberately, not overlooked.** Common's past
    /// rule is the naive regular one (append `ed`), exactly as
    /// `surface_complement`'s plural rule is the naive regular `s`. Pinning
    /// the wrong output is what makes an irregular table a red test to
    /// update rather than a latent defect: nothing silently depends on the
    /// naive form being right. Note where it does NOT bite — English
    /// negation is periphrastic, so the negative past leaves the stem bare
    /// (`did not eat`), which is the shape the merchant corpus's own line
    /// (*"I didn't know her"*) uses.
    #[test]
    fn a_transitive_verb_inflects_for_tense_number_and_polarity() {
        let vocab = CommonVocabulary::default();
        for (tense, number, polarity, verb) in [
            (Tense::Present, Number::Sg, Polarity::Pos, "eats"),
            (Tense::Present, Number::Pl, Polarity::Pos, "eat"),
            (Tense::Past, Number::Sg, Polarity::Pos, "eated"),
            (Tense::Past, Number::Pl, Polarity::Pos, "eated"),
            (Tense::Present, Number::Sg, Polarity::Neg, "does not eat"),
            (Tense::Present, Number::Pl, Polarity::Neg, "do not eat"),
            (Tense::Past, Number::Sg, Polarity::Neg, "did not eat"),
            (Tense::Past, Number::Pl, Polarity::Neg, "did not eat"),
        ] {
            let plural = match number {
                Number::Sg => "",
                Number::Pl => "s",
            };
            assert_eq!(
                realize_common(&eat_clause(tense, number, polarity), &vocab),
                format!("Nwamvam {verb} the bread{plural}."),
                "verb group wrong for {tense:?}/{number:?}/{polarity:?}"
            );
        }
    }

    /// The intransitive frame: one argument, a lexical verb, no complement.
    ///
    /// **The object slot holds [`Argument::Absent`]**, which is not an argument
    /// at all — see its own doc. The construction's part list simply has no
    /// `Part::Complement` and no `Part::Determiner`, so nothing ever reads it.
    #[test]
    fn an_intransitive_clause_surfaces_its_predicate_as_a_verb_with_no_complement() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: SLEEP.to_string(),
            subject: Subject::Name("the guard".to_string()),
            object: Argument::Absent,
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&clause, &vocab), "the guard sleeps.");
        // The same three features the transitive frame carries, on a frame with
        // no object to carry them into.
        let past = Clause {
            tense: Tense::Past,
            ..clause.clone()
        };
        assert_eq!(realize_common(&past, &vocab), "the guard sleeped.");
        let denied = Clause {
            polarity: Polarity::Neg,
            ..clause.clone()
        };
        assert_eq!(realize_common(&denied, &vocab), "the guard does not sleep.");
    }

    /// Adjectival predication: a copula, the property word, and NO
    /// determiner and NO complement.
    ///
    /// **This is the m02 trap's honest fix.** `Valence::Nominal` renders
    /// `Subject Copula Determiner Complement` and would say *"the road is an
    /// old."* The Mortise declined a `Definiteness::Bare` for this because it
    /// produced the right STRING through the wrong relation — asserting
    /// *road is-a old*, the classification. The property valence asserts
    /// property predication, and `Definiteness` gains no variant.
    ///
    /// **The property is the PREDICATE and the object slot is
    /// [`Argument::Absent`].** A property predication relates a subject to
    /// a state, not to a second participant — the ledger would commit it as
    /// `road old Flag(true)`, objectless, exactly as `IS_PERSON` and
    /// `TIDALLY_LOCKED` are committed. That is the same fact-shape argument
    /// (decision 0266) `Argument::Absent` was introduced under in Task 2.
    #[test]
    fn a_property_predication_takes_no_determiner() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: OLD.to_string(),
            subject: Subject::Name("the road".to_string()),
            object: Argument::Absent,
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&clause, &vocab), "the road is old.");
        // Definiteness is stated and IGNORED here, which is the point: a
        // property predication has no determiner slot for it to fill, so both
        // values produce the same surface.
        let definite = Clause {
            definiteness: Definiteness::Def,
            ..clause.clone()
        };
        assert_eq!(realize_common(&definite, &vocab), "the road is old.");
    }

    /// `Definiteness` still has exactly two variants. The property valence is
    /// what The Mortise's declined `Definiteness::Bare` was standing in for,
    /// and this asserts the stand-in was not quietly added anyway.
    ///
    /// **Made real with an exhaustive match**, not just an array literal: a
    /// third variant would fail to compile here (a non-exhaustive match),
    /// rather than merely going unseen by a `.len()` check nothing forces to
    /// widen.
    #[test]
    fn definiteness_gains_no_bare_variant() {
        for d in [Definiteness::Indef, Definiteness::Def] {
            match d {
                Definiteness::Indef | Definiteness::Def => {}
            }
        }
    }

    /// **A property predication cannot be recovered by `parse_common`, and
    /// this is a STATED LOSS, not an undiscovered gap** (spec §4 already
    /// freezes parsing coverage). Two things are true, and the second is the
    /// more informative one:
    ///
    /// 1. When the property word is not itself registered as a complement
    ///    concept (the realistic case — `old` is a `ConceptKind::Quality`,
    ///    never the kind of id a `ParseContext` registers as a classifiable
    ///    complement), the walk fails exactly as it does on any unrecognized
    ///    complement: [`ParseError::UnknownComplement`].
    /// 2. **If it WERE registered, the walk would not fail at all — it
    ///    would silently MISPARSE the sentence as a classification**,
    ///    `road is-a old`, rather than the property predication it actually
    ///    is. `parse_clause_body`'s verb-group search finds a `" is "` hit
    ///    for BOTH `IS_A`'s `CLASSIFY` construction and `OLD`'s `PROPERTY`
    ///    one at the identical position (they share the same copula
    ///    paradigm), and the documented tie-break ("a tie on both goes to
    ///    the lexicographically first predicate") always prefers `"is-a"`
    ///    over `"old"` (`'i' < 'o'`). The downstream walk is generic over
    ///    which construction actually won — it always tries a
    ///    determiner+complement read of what follows the copula, whether or
    ///    not that construction's own part list carries either slot — so a
    ///    bare property word sitting exactly where a bare-generic complement
    ///    would sit is structurally indistinguishable from one, for THIS
    ///    algorithm, regardless of which predicate wins.
    ///
    /// Restructuring the parser to disambiguate the two is out of this
    /// task's scope (the same "the walk is a hand-written inverse, not a
    /// general one" boundary [`parse_clause_body`]'s own doc already states
    /// for embedding); this test exists so the boundary is recorded rather
    /// than discovered fresh by whoever builds `r003`'s parse direction.
    #[test]
    fn a_property_predication_is_not_recovered_by_parsing() {
        // The realistic case: `old` is never registered as a complement.
        assert!(matches!(
            parse_common("the road is old.", &ctx(&["planet"])),
            Err(ParseError::UnknownComplement { after }) if after == "old"
        ));
        // The more informative case: if it WERE registered, the parse does
        // not fail — it silently returns the WRONG clause, a classification
        // rather than a property predication. Pinned, not endorsed.
        let misparsed = parse_common("the road is old.", &ctx(&["planet", "old"]))
            .expect("the tie-break always favors is-a, so this resolves rather than fails");
        assert_eq!(misparsed.predicate, IS_A);
        assert_eq!(misparsed.object, Argument::Concept("old".to_string()));
    }

    /// Locative predication: a copula, an adposition, and a located complement.
    ///
    /// **The adposition is the PREDICATE**, rendered through
    /// [`Part::PredicateWord`] — the same slot Task 4's property valence uses,
    /// because a property word and an adposition are both the predicate
    /// surfacing uninflected. So `at`, `in` and `under` are three rows sharing
    /// one part list, never three constructions: the same "a second verb is one
    /// row" promise `common_constructions` makes, applied one slot over.
    ///
    /// **The rung's own text is *"The merchant is at the gate."*; `at` and
    /// `gate` are not registered anywhere in this crate** (Task 0 established
    /// this campaign registers no concept), so the witness substitutes `under`
    /// and `tree` — both already in [`universal_stratum`](crate::packs::universal_stratum)
    /// — the same substitution shape `r003`'s witness takes for `old`/`long`
    /// and `r006`'s takes for `kill`/`strike`.
    #[test]
    fn a_locative_predication_places_its_subject() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: UNDER.to_string(),
            subject: Subject::Name("the merchant".to_string()),
            object: Argument::Concept("tree".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common(&clause, &vocab),
            "the merchant is under the tree."
        );
    }

    /// **A locative predication cannot be recovered by `parse_common` either,
    /// and this is a STATED LOSS for the same reason [`Valence::Property`]'s
    /// is** (spec §4 freezes parsing coverage). The shape of the failure is
    /// SHARPER than `Property`'s, not merely a repeat of it, because the
    /// determiner-strip step is what breaks it rather than the complement
    /// lookup:
    ///
    /// 1. **The realistic case fails structurally, regardless of what is
    ///    registered.** `parse_clause_body`'s copula search finds `" is "`
    ///    and (per the same tie-break `Property`'s test documents) always
    ///    prefers `IS_A`'s `CLASSIFY` construction, since `"is-a" < "under"`
    ///    lexicographically. `CLASSIFY`'s own downstream walk then tries to
    ///    strip a determiner (`"the "`/`"a "`/`"an "`) off whatever follows
    ///    the copula — but here that is the adposition (`"under the tree"`),
    ///    which starts with none of the three, so the strip fails, falls back
    ///    to `Definiteness::Indef` and leaves the whole remainder as
    ///    `after_det`. No registered complement's surface is a PREFIX of
    ///    `"under the tree"` (they would have to spell `"under…"`), so the
    ///    walk reports [`ParseError::UnknownComplement`] whether or not
    ///    `tree` itself is registered — unlike `Property`, where the same
    ///    failure required `old` to be UNregistered.
    /// 2. **The informative case still misparses, the same way `Property`'s
    ///    does, when the adposition itself is what gets registered.** If
    ///    `under` is added to the `ParseContext`'s complement set, its own
    ///    surface (`"under"`) IS a prefix of the remainder, so the walk
    ///    matches it as the complement of an `IS_A` classification —
    ///    `"the merchant is-a under"` — rather than reporting failure.
    ///
    /// Pinned by value, exactly as `a_property_predication_is_not_recovered_
    /// by_parsing` pins its own two outcomes, so the misparse hazard is
    /// recorded rather than rediscovered by whoever builds `r005`'s parse
    /// direction.
    #[test]
    fn a_locative_predication_is_not_recovered_by_parsing() {
        // The realistic case: `tree` is registered (it is a real concept
        // elsewhere in the world), `under` is not — and the walk fails
        // anyway, because the adposition sits where the determiner-strip
        // looks, not because no complement matches.
        assert!(matches!(
            parse_common("the merchant is under the tree.", &ctx(&["tree"])),
            Err(ParseError::UnknownComplement { after }) if after == "under the tree"
        ));
        // The informative case: register the adposition itself, and the
        // walk does not fail — it silently returns the WRONG clause, a
        // classification whose object is the adposition. Pinned, not
        // endorsed.
        let misparsed = parse_common("the merchant is under the tree.", &ctx(&["tree", "under"]))
            .expect("the tie-break always favors is-a, so this resolves rather than fails");
        assert_eq!(misparsed.predicate, IS_A);
        assert_eq!(misparsed.object, Argument::Concept("under".to_string()));
    }

    /// `Valence` is CLOSED at Stassen (1997)'s four intransitive predication
    /// strategies plus the transitive frame, and this is where that claim is
    /// mechanical rather than prose.
    ///
    /// **Exhaustive match, not a count.** A count passes whatever five variants
    /// exist; this fails to COMPILE when a sixth is added, which is the point.
    /// Decision 0326 says a campaign adding a variant per predicate has rebuilt
    /// `Frame` and should stop — a sixth variant here is that campaign meeting
    /// a wall it has to argue past deliberately.
    #[test]
    fn the_valence_taxonomy_is_closed_at_five() {
        fn strategy(v: Valence) -> &'static str {
            match v {
                Valence::Nominal => "nominal",       // Stassen: nominal
                Valence::Property => "property",     // Stassen: adjectival
                Valence::Locative => "locative",     // Stassen: locational
                Valence::Intransitive => "verbal",   // Stassen: verbal
                Valence::Transitive => "transitive", // the one two-argument frame
            }
        }
        let all = [
            Valence::Nominal,
            Valence::Property,
            Valence::Locative,
            Valence::Intransitive,
            Valence::Transitive,
        ];
        let named: std::collections::BTreeSet<&str> = all.iter().map(|v| strategy(*v)).collect();
        assert_eq!(named.len(), 5, "five distinct strategies");
    }

    /// The one-row promise, exercised. [`KILL`] was added to
    /// `PREDICATE_VALENCE` and to no other table: it inherits `eat`'s part
    /// list by valence, so it realizes and parses without a line of
    /// construction code of its own. If a future campaign has to touch
    /// `common_constructions` to add a transitive verb, this test is where
    /// that shows up.
    ///
    /// It is also the one place `kill` surfaces REGULARLY where `eat` does
    /// not: the naive `ed` rule is correct for this stem, so the past is
    /// `killed` rather than `eated`.
    #[test]
    fn a_second_transitive_verb_is_one_row_and_no_new_construction() {
        let vocab = CommonVocabulary::default();
        let clause = |tense, polarity| Clause {
            predicate: KILL.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("person".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense,
            polarity,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common(&clause(Tense::Past, Polarity::Pos), &vocab),
            "Nwamvam killed the person."
        );
        assert_eq!(
            realize_common(&clause(Tense::Past, Polarity::Neg), &vocab),
            "Nwamvam did not kill the person."
        );
        // Backward through the same table, and the predicate comes back.
        let parsed = parse_common(
            &realize_common(&clause(Tense::Past, Polarity::Pos), &vocab),
            &ctx(&["person"]),
        )
        .expect("a kill clause parses");
        assert_eq!(parsed.predicate, KILL);
        assert_eq!(parsed.tense, Tense::Past);
    }

    /// `think` is the epistemic-hedge predicate (m09, *"I think her name
    /// was Gilda"*). Transitive by the same argument [`KNOW`] is: one
    /// argument structure with a category-flexible object, so it adds a
    /// ROW and no new [`Valence`] variant — the same one-row promise
    /// [`KILL`]'s test above exercises, kept a third time.
    ///
    /// This pins only that `think` surfaces as a verb in a simple
    /// transitive clause; it does not build m09's full embedded-clause
    /// sentence, which needs clause recursion this task does not add
    /// (a later task's job).
    #[test]
    fn a_hedge_clause_surfaces_think_as_a_verb() {
        let vocab = CommonVocabulary::default();
        let clause = |tense, polarity| Clause {
            predicate: THINK.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("person".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense,
            polarity,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common(&clause(Tense::Present, Polarity::Pos), &vocab),
            "Nwamvam thinks the person."
        );
        assert_eq!(
            realize_common(&clause(Tense::Present, Polarity::Neg), &vocab),
            "Nwamvam does not think the person."
        );
        // Backward through the same table, and the predicate comes back.
        let parsed = parse_common(
            &realize_common(&clause(Tense::Present, Polarity::Pos), &vocab),
            &ctx(&["person"]),
        )
        .expect("a think clause parses");
        assert_eq!(parsed.predicate, THINK);
        assert_eq!(parsed.tense, Tense::Present);
    }

    /// The campaign's headline sentence: *"I did not know they killed
    /// them"* — a clause complement, riding the transitive frame `know`
    /// already had, with NO determiner in front of it. Failing to suppress
    /// `Part::Determiner` for a clause object is what produces *"I did not
    /// know a they killed them"*, which is the defect this test is written
    /// to catch (spec §4.2).
    #[test]
    fn a_clause_object_realizes_with_no_determiner() {
        let vocab = CommonVocabulary::default();
        let embedded = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let matrix = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(embedded)),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let out = realize_common(&matrix, &vocab);
        assert_eq!(out, "I did not know they killed them.");
        assert!(
            !out.contains(" a they") && !out.contains(" the they"),
            "no determiner may precede the embedded clause, got {out:?}"
        );
        // Exactly one full stop: the embedded clause's own trailing "." is
        // trimmed, so the matrix clause's is the only one in the sentence.
        assert_eq!(out.matches('.').count(), 1);
    }

    /// A clause in SUBJECT position — *"That he killed her confused me"*. The
    /// same machinery the object slot uses (`a_clause_object_realizes_with_
    /// no_determiner`, just above), in a different hole: the embedded
    /// clause realizes as its own full sentence, trailing period trimmed,
    /// and slots into `Part::Subject` verbatim — no complementizer, because
    /// that marker is a tongue-side, drawn strategy (Task 5), not Common's
    /// to spend. The gerund (*"Seeing it"*) is a nominalization and stays
    /// out of scope (spec §9.1); this is the complementizer kind.
    #[test]
    fn a_clause_subject_realizes_through_the_same_machinery() {
        let vocab = CommonVocabulary::default();
        let embedded = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let matrix = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Clause(Box::new(embedded)),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let out = realize_common(&matrix, &vocab);
        assert_eq!(out, "they killed them did not know them.");
        // Exactly one full stop, the same invariant the object-slot test
        // pins: the embedded clause's own trailing "." is trimmed away, so
        // the matrix clause's is the only one in the sentence.
        assert_eq!(out.matches('.').count(), 1);
    }

    /// [`subject_embed_depth`] and [`clause_embed_depth`] read one shared
    /// budget (Task 4's doc claim, demonstrated): a clause bound to the
    /// SUBJECT slot, whose own object is itself a clause, goes two deep
    /// exactly as `a_clause_nested_two_deep_is_refused` does through the
    /// object slot — and is refused the same way, before either level
    /// renders.
    #[test]
    #[should_panic(expected = "may not itself contain a clause complement")]
    fn a_clause_subject_nested_two_deep_is_refused() {
        let vocab = CommonVocabulary::default();
        let deepest = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let inner = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Clause(Box::new(deepest)),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let outer = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Clause(Box::new(inner)),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let _ = realize_common(&outer, &vocab);
    }

    /// The depth cap fires as a panic, the same class as the missing-
    /// construction panic [`realize_common`]'s own doc names: an authoring
    /// hole in this repository, never a fact about a people. One level is
    /// the depth this campaign builds and can show working (spec §4.3); a
    /// clause complement whose own object is another clause complement goes
    /// two deep and must be refused before either level renders.
    #[test]
    #[should_panic(expected = "may not itself contain a clause complement")]
    fn a_clause_nested_two_deep_is_refused() {
        let vocab = CommonVocabulary::default();
        let deepest = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let middle = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Clause(Box::new(deepest)),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let outer = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(middle)),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let _ = realize_common(&outer, &vocab);
    }

    /// Spec §4.1's adjunct refusal, fired: `Adjunct` holds an `Argument`, so
    /// an adjunct carrying a clause type-checks, and without an explicit arm
    /// [`common_role_surface`]'s trailing `_ => None` would render it as
    /// nothing, silently. This is the test the CONTROLLER FINDING asked for
    /// — the refusal must be observed, not merely written.
    #[test]
    #[should_panic(expected = "adjunct may not carry an embedded clause")]
    fn an_adjunct_carrying_a_clause_is_refused() {
        let vocab = CommonVocabulary::default();
        let embedded = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let adjunct = Adjunct {
            role: "occ-people".to_string(),
            argument: Argument::Clause(Box::new(embedded)),
        };
        let _ = common_role_surface(&adjunct, &vocab);
    }

    /// [`VERB_PARADIGM`]'s totality, and the places it is deliberately NOT
    /// injective.
    ///
    /// **The headline is that widening by [`Person`] added no form.** 24
    /// rows spell the same **six** words the number-only table's eight did,
    /// because English inflects a lexical verb for person in exactly one
    /// row (third-person singular present) and that row's `s` was already
    /// in the table — person only stopped 1sg and 2sg from borrowing it.
    /// Contrast [`COPULA_PARADIGM`], which is suppletive and gained `am`.
    /// This is worth asserting rather than reasoning about, because it is
    /// the fact that bounds how much the parse direction's search grew.
    ///
    /// The syncretism itself is now two-dimensional: the past neutralizes
    /// both number and person (one `killed`, one `did not kill`), and the
    /// present positive neutralizes both across the five non-3sg rows that
    /// share a bare stem. That is a fact about the target language, not a
    /// hole — and it is why the parse direction narrows by the subject's
    /// person first and then lets the object's own surface break the
    /// remaining number tie.
    #[test]
    fn verb_paradigm_is_total_and_syncretic_beyond_the_third_singular() {
        let mut forms: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        let mut combinations = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for number in [Number::Sg, Number::Pl] {
                for polarity in [Polarity::Pos, Polarity::Neg] {
                    for person in Person::ALL {
                        forms.insert(verb_surface(KILL, tense, number, polarity, person));
                        combinations += 1;
                    }
                }
            }
        }
        assert_eq!(combinations, 24);
        assert_eq!(VERB_PARADIGM.len(), 24);
        assert_eq!(
            forms.len(),
            6,
            "person added no form: the `s` row was already here, and the \
             past and the bare present stem neutralize both features: {forms:?}"
        );
        assert!(forms.contains("killed"));
        assert!(forms.contains("did not kill"));
        // The one row person actually splits, in both polarities.
        assert_eq!(
            verb_surface(
                KILL,
                Tense::Present,
                Number::Sg,
                Polarity::Pos,
                Person::Third
            ),
            "kills"
        );
        assert_eq!(
            verb_surface(
                KILL,
                Tense::Present,
                Number::Sg,
                Polarity::Pos,
                Person::First
            ),
            "kill"
        );
        assert_eq!(
            verb_surface(
                KILL,
                Tense::Present,
                Number::Sg,
                Polarity::Neg,
                Person::Third
            ),
            "does not kill"
        );
        assert_eq!(
            verb_surface(
                KILL,
                Tense::Present,
                Number::Sg,
                Polarity::Neg,
                Person::First
            ),
            "do not kill"
        );
    }

    /// No verb-group form spans two tenses or two polarities — the property
    /// [`parse_clause_body`] relies on when it reads `tense` and `polarity`
    /// straight off the single winning row while dropping that row's
    /// `number` and `person`.
    ///
    /// **Asserted against the tables rather than left to inspection**,
    /// because the widened key made both tables far less injective and the
    /// old reasoning ("eight distinct copula forms") no longer carries it.
    /// If a future row made `were` a present form somewhere, the parser
    /// would silently return the wrong tense; this reddens instead.
    #[test]
    fn no_verb_group_form_spans_two_tenses_or_polarities() {
        let mut seen: std::collections::BTreeMap<String, (Tense, Polarity)> =
            std::collections::BTreeMap::new();
        for (form, t, _, p, _) in COPULA_PARADIGM {
            let entry = seen.entry((*form).to_string()).or_insert((*t, *p));
            assert_eq!(*entry, (*t, *p), "copula form {form:?} spans two rows");
        }
        // Lexical verbs are stem-dependent, so the property is checked on a
        // realized stem the same way the parser generates candidates.
        let vocab = CommonVocabulary::default();
        let stem = vocab.word_for(KILL);
        let mut verbs: std::collections::BTreeMap<String, (Tense, Polarity)> =
            std::collections::BTreeMap::new();
        for (prefix, suffix, t, _, p, _) in VERB_PARADIGM {
            let form = format!("{prefix}{stem}{suffix}");
            let entry = verbs.entry(form.clone()).or_insert((*t, *p));
            assert_eq!(*entry, (*t, *p), "verb form {form:?} spans two rows");
        }
    }

    /// [`Valence::binds_object`], asserted DIRECTLY over all five variants.
    ///
    /// **Written because the `Locative` arm was correct and completely
    /// unobservable.** Both call sites in `grammar.rs` are unreachable for
    /// `Locative` (`tongue_verb` gaps on it first), and no test asked the
    /// method anything, so moving `Locative` into the `false` arm left the
    /// entire suite green. That is a vacuous guard. Covering all five
    /// variants rather than only the one that was unobservable keeps the
    /// next variant-shaped edit — there cannot be one, per the closed
    /// taxonomy, but the arms can still be rewritten — from re-opening the
    /// same hole somewhere else in the match.
    #[test]
    fn every_valence_states_whether_it_binds_an_object() {
        assert!(Valence::Nominal.binds_object());
        assert!(Valence::Transitive.binds_object());
        assert!(!Valence::Intransitive.binds_object());
        assert!(!Valence::Property.binds_object());
        // The locative binds the LOCATED THING — the whole difference
        // between it and `Property`, which shares its `Part::PredicateWord`
        // slot and differs only here.
        assert!(Valence::Locative.binds_object());
    }

    /// The construction table realizes forward and parses backward, and
    /// **that promise now has a second instance**: the parse recovers the
    /// PREDICATE from the verb group it matched, rather than assuming
    /// `is-a`. The past's number syncretism is recovered from the object's
    /// own plural, so the round trip is complete over the whole space.
    #[test]
    fn common_round_trips_a_transitive_clause() {
        let mut seen = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for polarity in [Polarity::Pos, Polarity::Neg] {
                for number in [Number::Sg, Number::Pl] {
                    for definiteness in [Definiteness::Def, Definiteness::Indef] {
                        let spec = Clause {
                            definiteness,
                            ..eat_clause(tense, number, polarity)
                        };
                        let ctx = ctx_from(&spec);
                        let text = realize_common(&spec, &ctx.vocabulary);
                        assert_eq!(
                            parse_common(&text, &ctx),
                            Ok(spec.clone()),
                            "round-trip failed for {text:?}"
                        );
                        seen += 1;
                    }
                }
            }
        }
        assert_eq!(seen, 16);
    }

    /// A parse context over `concepts` (concept ids, not words) with the
    /// bare naming convention as its vocabulary.
    fn ctx(concepts: &[&str]) -> ParseContext {
        ParseContext {
            complements: concepts.iter().map(|c| (*c).to_string()).collect(),
            vocabulary: CommonVocabulary::default(),
        }
    }

    #[test]
    fn parse_inverts_the_c2_target_sentence() {
        let (spec, tail) = parse_common_with_tail(
            "Vebe is a planet with two moons, orbiting a yellow-white dwarf.",
            &ctx(&["planet"]),
        )
        .unwrap();
        assert_eq!(spec.subject, Subject::Name("Vebe".into()));
        assert_eq!(spec.predicate, "is-a");
        assert_eq!(spec.object, Argument::Concept("planet".into()));
        assert_eq!(spec.number, Number::Sg);
        assert_eq!(spec.definiteness, Definiteness::Indef);
        // The tail comes back as TEXT, and `adjuncts` stays empty: Common
        // recognizes the clause skeleton, not its role constructions.
        assert_eq!(spec.adjuncts, Vec::<Adjunct>::new());
        assert_eq!(
            tail,
            vec![
                "with two moons".to_string(),
                "orbiting a yellow-white dwarf".to_string()
            ]
        );
    }

    /// The plural clause recovers the SINGULAR concept id — the parser
    /// matches against each id's realized surface, so the `'s'` the realizer
    /// added is undone by the same rule that added it rather than by a
    /// caller stripping a trailing letter.
    #[test]
    fn parse_inverts_the_plural_generic() {
        let (spec, tail) =
            parse_common_with_tail("The Vavako are goblins.", &ctx(&["goblin"])).unwrap();
        assert_eq!(spec.subject, Subject::Name("The Vavako".into()));
        assert_eq!(spec.object, Argument::Concept("goblin".into()));
        assert_eq!(spec.number, Number::Pl);
        assert_eq!(spec.definiteness, Definiteness::Indef);
        assert_eq!(tail, Vec::<String>::new());
    }

    /// An embedded sentence round-trips: realize, parse, and get an equal
    /// `Clause` back (The Mortise, Task 8). The matrix predicate is
    /// present-tense so its verb-group form is number-UNIQUE (`"does not
    /// know"` is the Sg row only; `"do not know"` is Pl) — the embedded
    /// object leaves no complement surface behind to disambiguate a
    /// syncretic form the way the non-embedded path does, so this test
    /// deliberately avoids relying on that second signal
    /// (`resolve_embedded_number`'s pronoun fallback) to isolate what the
    /// recursion itself proves.
    #[test]
    fn an_embedded_sentence_round_trips() {
        let vocab = CommonVocabulary::default();
        let embedded = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let matrix = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(embedded)),
            number: Number::Sg,
            // Lost information, not a guess: an embedded-object clause's
            // text carries no determiner (`realize_common` skips
            // `Part::Determiner` for `Argument::Clause`), so nothing in the
            // surface states this feature — the exact same kind of loss
            // `evidential` already documents for every Common clause.
            // `Indef` is the parser's documented default, matched here so
            // the round trip holds.
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let text = realize_common(&matrix, &vocab);
        let ctx = ctx(&["planet"]);
        assert_eq!(
            parse_common(&text, &ctx),
            Ok(matrix),
            "round-trip failed for {text:?}"
        );
    }

    /// The boundary marker is the discriminator (The Mortise, Task 8): a
    /// sentence with two verb groups is embedding or coordination, and the
    /// marker says which. A complementizer means a clause hangs BELOW; a
    /// conjunction means one sits BESIDE — and for Common, embedding's
    /// marker is no word at all, so the discriminator is really just the
    /// conjunction's presence.
    ///
    /// The coordination half is not just "some Err comes back" — it proves
    /// the guard is load-bearing. Without checking for a top-level `" and "`
    /// BEFORE the verb-group split, this exact sentence would misparse: the
    /// earliest verb group is "kills" (present tense, Sg-unique, so the
    /// matrix `number` resolves with no ambiguity at all), leaving "them and
    /// Vebe is a planet" as an unrecognized remainder, and a NAIVE recursive
    /// attempt at THAT text would succeed — "is a planet" really is a
    /// registered complement — recovering a bogus embedded clause whose
    /// subject is the nonsense text `"them and Vebe"`. The guard refuses the
    /// whole sentence before any of that runs.
    #[test]
    fn the_marker_discriminates_embedding_from_coordination() {
        let vocab = CommonVocabulary::default();
        let ctx = ctx(&["planet"]);

        // Embedding: no marker, and it parses. The embedded clause's own
        // object is a CONCEPT, not a pronoun — `parse_common` cannot
        // recover `Argument::Pronoun` at all (a separate, out-of-scope
        // limit, registry row `LANG-parse-cannot-recover-a-pronoun-object`),
        // so a pronoun-object inner clause would refuse for that unrelated
        // reason and prove nothing about the marker.
        let embedded = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let matrix = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(embedded)),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        let embedded_text = realize_common(&matrix, &vocab);
        assert!(
            !embedded_text.contains(" and "),
            "Common's embedding marker is no word at all: {embedded_text:?}"
        );
        assert!(
            parse_common(&embedded_text, &ctx).is_ok(),
            "an unmarked embedding must still parse: {embedded_text:?}"
        );

        // Coordination: the marker, and it refuses rather than misparsing.
        let clause_a = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let clause_b = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name("Vebe".to_string()),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let coord = Coordination {
            clauses: vec![clause_a, clause_b],
        };
        let coord_text = realize_common_coordination(&coord, &vocab);
        assert!(
            coord_text.contains(" and "),
            "Common's coordination marker is the word 'and': {coord_text:?}"
        );
        match parse_common(&coord_text, &ctx) {
            Err(ParseError::UnknownComplement { .. }) => {}
            other => panic!(
                "a top-level 'and' must refuse rather than misparse, got {other:?} for {coord_text:?}"
            ),
        }
    }

    /// The depth budget stops the descent rather than recursing forever
    /// (The Mortise, Task 8): this text is hand-assembled (not realized —
    /// `realize_common` itself refuses to build genuinely two-deep text,
    /// same cap, other direction) to need exactly two recursive levels to
    /// fully resolve, and `CLAUSE_EMBED_MAX_DEPTH` is `1`. The returned
    /// error's own `after` field is the decisive assertion: it names
    /// exactly "they is a planet" — the depth-1 call's own unresolved
    /// remainder — proving the walk stopped BEFORE attempting the depth-2
    /// recursion that would otherwise have succeeded (`"planet"` is a real,
    /// registered complement), not that it merely failed for some other
    /// reason.
    #[test]
    fn the_parser_stops_descending_at_the_cap() {
        let ctx = ctx(&["planet"]);
        let text = "I does not know they do not know they is a planet.";
        assert_eq!(
            parse_common(text, &ctx),
            Err(ParseError::UnknownComplement {
                after: "they is a planet".to_string()
            })
        );
    }

    #[test]
    fn parse_reports_a_recountable_failure() {
        // The parser's three failure modes, each directly exercised.
        assert!(matches!(
            parse_common("Vebe is a carriage.", &ctx(&["planet"])),
            Err(ParseError::UnknownComplement { .. })
        ));
        // "wordless" has no terminal '.', hitting Unterminated before the
        // verb-group search ever runs — the terminal check is the FIRST
        // gate. NoVerbGroup needs a terminated sentence that still carries
        // no copula form and no construction's inflected verb.
        assert!(matches!(
            parse_common("wordless.", &ctx(&["planet"])),
            Err(ParseError::NoVerbGroup)
        ));
        assert!(matches!(
            parse_common("wordless", &ctx(&["planet"])),
            Err(ParseError::Unterminated)
        ));
    }

    #[test]
    fn pronoun_subjects_are_lowercase_by_contract() {
        // The re-mention path emits a lowercase nominative; parse binds it as
        // a Pronoun. A capitalized "They" is NOT recognized as a pronoun — if
        // a future construction capitalizes sentence-initial pronouns, this
        // canary reddens and the parse-side binding must learn case together
        // with it (never separately).
        let c = ctx(&["planet"]);
        assert_eq!(
            parse_common("they is a planet.", &c).unwrap().subject,
            Subject::Pronoun(Person::Third)
        );
        assert_eq!(
            parse_common("They is a planet.", &c).unwrap().subject,
            Subject::Name("They".into())
        );
        // "its" was a Pronoun until The Inquest and is a Name now: it is a
        // POSSESSIVE, which no person/number row names, and no realizer ever
        // produced it. Pinned so the retype's one behaviour change is a
        // stated fact rather than a silent one.
        assert_eq!(
            parse_common("its is a planet.", &c).unwrap().subject,
            Subject::Name("its".into())
        );
    }

    /// The pronoun paradigm is total: every person x number x case has
    /// exactly one row, so `common_pronoun`'s `expect` is unreachable.
    #[test]
    fn pronoun_paradigm_is_total() {
        for person in Person::ALL {
            for number in [Number::Sg, Number::Pl] {
                for case in [PronounCase::Nominative, PronounCase::Accusative] {
                    let matches: Vec<_> = PRONOUN_PARADIGM
                        .iter()
                        .filter(|(_, pe, n, c)| *pe == person && *n == number && *c == case)
                        .collect();
                    assert_eq!(
                        matches.len(),
                        1,
                        "exactly one row for {person:?}/{number:?}/{case:?}"
                    );
                }
            }
        }
    }

    /// The property the round trip depends on: a nominative form names one
    /// person and no other, so [`nominative_person`] can invert the subject
    /// slot from the surface alone and take the number from the clause it is
    /// already recovering. A future Common that spelled first and third
    /// person alike in the nominative would redden this BEFORE the round-trip
    /// property failed with a confusing message.
    #[test]
    fn nominative_forms_determine_person() {
        for (form, person, _, case) in PRONOUN_PARADIGM {
            if *case != PronounCase::Nominative {
                continue;
            }
            assert_eq!(
                nominative_person(form),
                Some(*person),
                "{form:?} must invert to {person:?}"
            );
        }
        assert_eq!(nominative_person("Vebe"), None);
        // An ACCUSATIVE-only form is not a subject: "them" never appears in
        // the subject slot, so it must not invert.
        assert_eq!(nominative_person("them"), None);
    }

    /// The person narrowing in [`parse_clause_body`], made observable.
    ///
    /// **Written so the narrowing is not a vacuous guard.** Since The Rail
    /// widened both paradigms by [`Person`], the parse intersects the verb
    /// group's candidate rows with the person it read off the SUBJECT. Every
    /// sentence Common realizes agrees by construction, so on realizer
    /// output that intersection never discards the right answer and its
    /// effect is invisible — exactly the shape of a check nothing exercises.
    /// This test hands it text no realization produces.
    ///
    /// *"I are a planet."* is a real English sentence shape with a real
    /// agreement violation: `are` names four copula rows and not one of them
    /// is first-person singular, so the intersection is empty, no complement
    /// candidate is admitted, and the walk reports the
    /// [`ParseError::UnknownComplement`] it already had for text it cannot
    /// invert. **No new failure shape was added** — that was a design
    /// constraint, not an accident.
    ///
    /// The positive control sits beside it: the same sentence with the
    /// agreeing copula parses, so the refusal above is attributable to
    /// person and not to the fixture.
    #[test]
    fn an_agreement_violating_sentence_does_not_parse() {
        let mut complements = std::collections::BTreeSet::new();
        complements.insert("planet".to_string());
        let ctx = ParseContext {
            complements,
            vocabulary: CommonVocabulary::default(),
        };
        assert!(
            parse_common("I are a planet.", &ctx).is_err(),
            "no copula row is first-person singular `are`"
        );
        // Positive control: the agreeing form parses, and recovers the
        // person from the SUBJECT rather than from the copula.
        let parsed = parse_common("I am a planet.", &ctx).expect("`am` agrees with `I`");
        assert_eq!(parsed.subject, Subject::Pronoun(Person::First));
        assert_eq!(parsed.number, Number::Sg);
        // And the syncretic form is recovered at its own four rows: `are`
        // with a second-person subject is 2sg here, decided by the object's
        // own singular surface.
        let you = parse_common("you are a planet.", &ctx).expect("`are` agrees with `you`");
        assert_eq!(you.subject, Subject::Pronoun(Person::Second));
        assert_eq!(you.number, Number::Sg);
    }

    /// The campaign's own corpus line, in Common: *"I didn't know her"*
    /// realizes as *"I did not know them"* — a first-person subject pronoun
    /// and a third-person object pronoun in one clause, which is the pair
    /// `Argument::Pronoun` was added for.
    ///
    /// **Case comes from the SLOT.** The same `Person::First` renders `I` in
    /// the subject and would render `me` in the object; nothing in the clause
    /// states a case.
    #[test]
    fn common_realizes_a_pronoun_subject_and_a_pronoun_object() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&clause, &vocab), "I did not kill them.");
        // The object slot takes the accusative of whatever person it holds.
        let reflexive = Clause {
            object: Argument::Pronoun(Person::First),
            ..clause.clone()
        };
        assert_eq!(realize_common(&reflexive, &vocab), "I did not kill me.");
        // And the clause's number moves BOTH slots at once, which is what
        // `Clause::number`'s doc says it does.
        let plural = Clause {
            number: Number::Pl,
            ..clause.clone()
        };
        assert_eq!(realize_common(&plural, &vocab), "we did not kill them.");
    }

    /// **The first of the two roughnesses [`PRONOUN_PARADIGM`]'s doc named,
    /// now fixed** — this test is the positive half of the split, and it
    /// asserts the fix rather than the roughness.
    ///
    /// `common_has_no_person_agreement_and_one_third_person_singular`
    /// pinned *"I eats the bread"* so that an agreement fix would "arrive
    /// as a red test rather than a silent correction". The Rail's `r011`
    /// fired it deliberately: [`VERB_PARADIGM`] and [`COPULA_PARADIGM`] are
    /// keyed by [`Person`] now, so a first-person subject takes a bare stem
    /// and the copula is suppletive across all three persons.
    ///
    /// **Only the FIRST half of that test stopped being true.** The second
    /// half — *"they is a planet"* — is unchanged and has its own test
    /// below; see it for why that is a fact about the pronoun inventory
    /// rather than about agreement.
    #[test]
    fn common_agrees_for_person_in_the_verb_and_the_copula() {
        let vocab = CommonVocabulary::default();
        // The line the old test pinned as a roughness, now correct.
        let eats = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&eats, &vocab), "I eat the bread.");
        let second = Clause {
            subject: Subject::Pronoun(Person::Second),
            ..eats.clone()
        };
        assert_eq!(realize_common(&second, &vocab), "you eat the bread.");
        // Third-person singular keeps the `s` — the one row English
        // inflects a lexical verb for person in.
        let third = Clause {
            subject: Subject::Pronoun(Person::Third),
            ..eats.clone()
        };
        assert_eq!(realize_common(&third, &vocab), "they eats the bread.");
        // A NAME is third person (`Subject::person`), so nothing about a
        // named subject's surface moved.
        let named = Clause {
            subject: Subject::Name("Vebe".to_string()),
            ..eats.clone()
        };
        assert_eq!(realize_common(&named, &vocab), "Vebe eats the bread.");
        // The copula is suppletive, so all three persons differ in the
        // singular present — this is the rung's own pair of sentences.
        let am = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Concept("merchant".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&am, &vocab), "I am a merchant.");
        let are = Clause {
            subject: Subject::Pronoun(Person::Second),
            object: Argument::Concept("guard".to_string()),
            ..am.clone()
        };
        assert_eq!(realize_common(&are, &vocab), "you are a guard.");
    }

    /// **The second roughness, unchanged and deliberately so.**
    ///
    /// This is the surviving half of
    /// `common_has_no_person_agreement_and_one_third_person_singular`,
    /// asserting the identical string it always did. The Rail's `r011` gave
    /// Common person agreement and did NOT change this line, which is worth
    /// stating loudly because an earlier draft of the campaign's spec
    /// claimed the surface would become *"they are a planet"*. That was
    /// wrong twice over: it credited this campaign with a fix it does not
    /// make, and it described a mechanism the design does not have.
    ///
    /// **Agreement is keyed on FEATURES.** The subject here is third person
    /// at [`Number::Sg`], and that feature bundle's copula row is `is`. The
    /// awkwardness comes from the pronoun INVENTORY, not from agreement:
    /// Common spells 3sg `they` because nothing in the ledger assigns
    /// gender or animacy (spec §4.5), so there is one third-person singular
    /// and it is the animate-neutral one. Real English gives singular
    /// *they* plural agreement, which would make the copula depend on the
    /// subject's chosen FORM rather than on its features — a different
    /// mechanism, and not one this campaign builds. Deferred, with that
    /// reason.
    ///
    /// It reaches no committed artifact: no volume the book renders ever
    /// re-mentions a subject.
    #[test]
    fn commons_one_third_person_singular_is_the_animate_neutral_one() {
        let vocab = CommonVocabulary::default();
        let remention = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("planet".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&remention, &vocab), "they is a planet.");
    }

    // --- The round-trip property: parse_common(realize_common(s), ctx_from(s))
    // recovers the clause SKELETON, and parse_common_with_tail recovers each
    // adjunct's realized SURFACE. The adjuncts themselves are not recognized
    // (spec §6 freezes parsing coverage), so the equality is stated against a
    // spec with its adjuncts cleared — the loss is pinned, not papered over.

    /// Classify a subject into the coverage axis the property test tracks.
    fn subject_kind(s: &Subject) -> &'static str {
        match s {
            Subject::Pronoun(_) => "pronoun",
            Subject::Name(n) if n.contains(' ') => "multi-word-name",
            Subject::Name(_) => "single-word-name",
            // The round-trip enumeration below never generates one (a
            // clause subject has no parse-side recognizer yet — spec §6
            // freezes parsing coverage), so this arm exists only to keep
            // the match exhaustive against `Subject::Clause` (The Mortise,
            // Task 4).
            Subject::Clause(_) => "clause",
        }
    }

    /// Classify a complement's RESOLVED WORD (not its id) into the coverage
    /// axis the property test tracks. Multi-word wins over vowel-initial so a
    /// phrase like "ancient artifact" (both) still counts toward multi-word
    /// coverage; "elemental" alone covers vowel-initial.
    fn complement_kind(c: &str) -> &'static str {
        if c.contains(' ') {
            "multi-word"
        } else if matches!(c.chars().next(), Some('a' | 'e' | 'i' | 'o' | 'u')) {
            "vowel-initial"
        } else {
            "consonant-initial"
        }
    }

    fn number_str(n: Number) -> &'static str {
        match n {
            Number::Sg => "sg",
            Number::Pl => "pl",
        }
    }

    fn definiteness_str(d: Definiteness) -> &'static str {
        match d {
            Definiteness::Indef => "indef",
            Definiteness::Def => "def",
        }
    }

    /// Build the closed complement set a real caller would hand
    /// `parse_common`: the spec's own complement CONCEPT, plus decoys that
    /// probe longest-match — other legal concepts from the same closed
    /// vocabulary, a concept whose word is the first word of a multi-word
    /// surface (a genuine prefix that must lose to the full phrase), and a
    /// declared concept whose word is the real surface minus one character
    /// (must NOT match at all: the boundary check requires the character
    /// after a matched prefix to be a space).
    fn ctx_from(spec: &Clause) -> ParseContext {
        let Argument::Concept(concept) = &spec.object else {
            panic!("the round-trip property only enumerates concept objects");
        };
        let mut vocabulary = CommonVocabulary::default();
        let mut complements = std::collections::BTreeSet::new();
        complements.insert(concept.clone());
        // Stock decoys: other legal concepts from the closed vocabulary,
        // always present as noise the true complement must outrank.
        for stock in [
            "planet",
            "goblin-kind",
            "elemental",
            "yellow-white-dwarf",
            "ancient-artifact",
            "dwarf",
        ] {
            complements.insert(stock.to_string());
        }
        let word = vocabulary.word_for(concept);
        // Prefix-of-longer probe: a single-word concept whose id is its own
        // word, matching the first word of a multi-word surface.
        if let Some((first, _)) = word.split_once(' ') {
            complements.insert(first.to_string());
        }
        // Must-not-match probe: one character short of the real word. Only a
        // DECLARED word can be a truncation, since the mechanical rules never
        // produce one.
        if word.len() > 1 {
            let mut truncated = word.clone();
            truncated.pop();
            vocabulary.declare("truncation-decoy", &truncated);
            complements.insert("truncation-decoy".to_string());
        }
        ParseContext {
            complements,
            vocabulary,
        }
    }

    #[test]
    fn round_trip_over_the_closed_value_space() {
        // Full-factorial enumeration, NOT a Stream draw: the value space
        // here is small and genuinely closed (5 subjects x 5 complements x
        // 2 numbers x 2 definitenesses x 4 adjunct-counts = 400 cases), so
        // exhaustive enumeration GUARANTEES every combo fires at least
        // once. A drawn sample only gives that probabilistically — and the
        // Concordance campaign shipped a property test whose random
        // generator never once emitted the one value (signed zero) that
        // broke the invariant. Enumeration is strictly stronger here and
        // costs nothing extra since the space is small.
        let subjects: Vec<Subject> = vec![
            Subject::Name("Vebe".into()),
            Subject::Name("Aoth".into()),
            Subject::Name("MacTavish".into()), // mixed-case: interior capital
            Subject::Name("The Vavako".into()), // multi-word
            // **All three persons, WIDENED by The Rail (`r011`) rather than
            // left at third alone.** A pronoun's number is the clause's
            // own, so each of these covers its Sg and Pl legs from the
            // enumeration below rather than needing two entries.
            //
            // The widening is the point. Person agreement made both
            // paradigms non-injective — `are` names four copula rows — so
            // the honest question is whether a round trip through Common
            // survives that. Narrowing the enumeration back to the third
            // person would have HIDDEN the loss rather than handled it, and
            // this property test exists precisely because a generator that
            // never emits the exposing value proves nothing (the Concordance
            // lesson, cited in the comment below).
            //
            // **It survives, and that is a finding worth stating: person is
            // not lost, it is carried by a different part of the
            // sentence.** The verb group cannot state which of four rows
            // `are` came from, but the SUBJECT can — Common's nominative
            // pronouns are pairwise distinguishing on person
            // (`nominative_forms_determine_person`), so the parse recovers
            // person from the subject text and then uses it to narrow the
            // verb group's candidate numbers. See `verb_group_forms` for
            // why that means no paradigm row has to be nominated canonical.
            // The equality below is therefore asserted at FULL width for
            // person, unlike `adjuncts` (cleared, recognizing roles is a
            // later campaign) and `evidential` (defaulted, Common has no
            // evidential surface) — the two losses this property really
            // does carry.
            Subject::Pronoun(Person::First),
            Subject::Pronoun(Person::Second),
            Subject::Pronoun(Person::Third),
        ];
        // Concept IDS, not words — the realizer resolves each through the
        // context's vocabulary, so `yellow-white-dwarf` also exercises the
        // hyphen→space rule inside the round trip.
        let complements = [
            "planet",             // -> "planet", consonant-initial
            "goblin-kind",        // -> "goblin", consonant-initial
            "elemental",          // -> "elemental", vowel-initial
            "yellow-white-dwarf", // -> "yellow white dwarf", multi-word
            "ancient-artifact",   // -> "ancient artifact", multi-word AND vowel-initial
        ];
        // Real ROLES now, not pre-rendered English: the realizer chooses each
        // surface, so the property exercises the role table too. All three are
        // INLINE constructions, keeping the tail a `", "`-joined list the
        // parser's own split can invert.
        let adjunct_pool = [
            Adjunct {
                role: "moon-count".to_string(),
                argument: Argument::Count(2),
            },
            Adjunct {
                role: "star-class".to_string(),
                argument: Argument::Concept("yellow-white-dwarf".to_string()),
            },
            Adjunct {
                role: "moon-count".to_string(),
                argument: Argument::Count(1),
            },
        ];

        let mut covered: std::collections::BTreeSet<(
            &'static str,
            &'static str,
            &'static str,
            &'static str,
            usize,
        )> = std::collections::BTreeSet::new();
        let mut cases = 0usize;

        for subject in &subjects {
            for complement in complements {
                for number in [Number::Sg, Number::Pl] {
                    for definiteness in [Definiteness::Indef, Definiteness::Def] {
                        for adjunct_count in 0..=3usize {
                            let adjuncts: Vec<Adjunct> = adjunct_pool[..adjunct_count].to_vec();
                            let spec = Clause {
                                predicate: "is-a".to_string(),
                                subject: subject.clone(),
                                object: Argument::Concept(complement.to_string()),
                                number,
                                definiteness,
                                // Not varied over: Common has no evidential
                                // surface (spec §3.2), so there is nothing
                                // for the parser to invert. The skeleton
                                // below inherits this value through `..`,
                                // and `parse_common_with_tail` returns the
                                // same documented default.
                                evidential: Evidential::Witnessed,
                                tense: Tense::Present,
                                polarity: Polarity::Pos,
                                adjuncts,
                            };
                            let ctx = ctx_from(&spec);
                            let text = realize_common(&spec, &ctx.vocabulary);
                            let expected_tail: Vec<String> = spec
                                .adjuncts
                                .iter()
                                .map(|a| {
                                    common_role_surface(a, &ctx.vocabulary)
                                        .expect("every pooled role has a Common construction")
                                        .1
                                })
                                .collect();
                            let skeleton = Clause {
                                adjuncts: Vec::new(),
                                ..spec.clone()
                            };
                            assert_eq!(
                                parse_common_with_tail(&text, &ctx),
                                Ok((skeleton, expected_tail)),
                                "round-trip failed for {text:?}"
                            );
                            covered.insert((
                                subject_kind(&spec.subject),
                                complement_kind(&ctx.vocabulary.word_for(complement)),
                                number_str(spec.number),
                                definiteness_str(spec.definiteness),
                                adjunct_count,
                            ));
                            cases += 1;
                        }
                    }
                }
            }
        }

        assert!(cases >= 200, "expected >= 200 cases, got {cases}");

        // The generator's value-space coverage IS the test's strength (the
        // Concordance lesson): assert every (subject-kind x complement-kind
        // x number x definiteness x adjunct-count) combo was actually
        // emitted, not merely that the loop ran. 3 subject kinds x 3
        // complement kinds x 2 numbers x 2 definitenesses x 4 adjunct
        // counts.
        let expected_combos = 3 * 3 * 2 * 2 * 4;
        assert_eq!(
            covered.len(),
            expected_combos,
            "generator did not cover every combo: {covered:?}"
        );
    }

    #[test]
    fn common_renders_a_moon_count_exactly_as_the_book_did() {
        let v = CommonVocabulary::default();
        let one = Adjunct {
            role: "moon-count".into(),
            argument: Argument::Count(1),
        };
        let two = Adjunct {
            role: "moon-count".into(),
            argument: Argument::Count(2),
        };
        assert_eq!(
            common_role_surface(&one, &v),
            Some((AdjunctPosition::Inline, "with one moon".to_string()))
        );
        assert_eq!(
            common_role_surface(&two, &v),
            Some((AdjunctPosition::Inline, "with two moons".to_string()))
        );
    }

    #[test]
    fn common_renders_a_star_class_through_the_vocabulary_with_its_article() {
        let mut v = CommonVocabulary::default();
        v.declare("yellow-white-dwarf", "yellow-white dwarf");
        let a = Adjunct {
            role: "star-class".into(),
            argument: Argument::Concept("yellow-white-dwarf".into()),
        };
        assert_eq!(
            common_role_surface(&a, &v),
            Some((
                AdjunctPosition::Inline,
                "orbiting a yellow-white dwarf".to_string()
            ))
        );
    }

    #[test]
    fn a_day_length_is_trailing_not_inline() {
        let v = CommonVocabulary::default();
        let a = Adjunct {
            role: "day-length-std".into(),
            argument: Argument::Quantity(1.5),
        };
        assert_eq!(
            common_role_surface(&a, &v),
            Some((
                AdjunctPosition::Trailing,
                "its day lasts about 1.5 standard days".to_string()
            ))
        );
    }

    /// The byte-identity contract: the adjunct path must reproduce, to the
    /// byte, what `windows/book`'s pre-rendered `modifiers` tail produced —
    /// the `", "` join between inline adjuncts and the `"; "` join before a
    /// trailing one, terminal `'.'` last.
    #[test]
    fn adjuncts_reproduce_the_modifier_tail_byte_for_byte() {
        let mut v = CommonVocabulary::default();
        v.declare("yellow-white-dwarf", "yellow-white dwarf");
        let spec = Clause {
            predicate: "is-a".into(),
            subject: Subject::Name("Hornvale".into()),
            object: Argument::Concept("planet".into()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![
                Adjunct {
                    role: "moon-count".into(),
                    argument: Argument::Count(2),
                },
                Adjunct {
                    role: "star-class".into(),
                    argument: Argument::Concept("yellow-white-dwarf".into()),
                },
                Adjunct {
                    role: "day-length-std".into(),
                    argument: Argument::Quantity(1.5),
                },
            ],
        };
        assert_eq!(
            realize_common(&spec, &v),
            "Hornvale is a planet with two moons, orbiting a yellow-white dwarf; \
             its day lasts about 1.5 standard days."
        );
    }

    #[test]
    fn common_names_an_occupations_people_in_the_plural() {
        let v = CommonVocabulary::default();
        let a = Adjunct {
            role: "occ-people".into(),
            argument: Argument::Concept("hobgoblin-kind".into()),
        };
        assert_eq!(
            common_role_surface(&a, &v),
            Some((AdjunctPosition::Inline, "of the hobgoblins".to_string()))
        );
    }

    #[test]
    fn common_places_an_occupation_at_its_vertex_as_a_bare_identifier() {
        let v = CommonVocabulary::default();
        let a = Adjunct {
            role: "occ-site".into(),
            argument: Argument::Count(8835),
        };
        assert_eq!(
            common_role_surface(&a, &v),
            Some((
                AdjunctPosition::Inline,
                "in the clearing at vertex 8835".to_string()
            ))
        );
    }

    /// A founding is inline — part of the noun phrase — and its year goes
    /// through [`cardinal`], so a small year reads as a word.
    #[test]
    fn common_dates_a_founding_inline_through_the_cardinal() {
        let v = CommonVocabulary::default();
        let big = Adjunct {
            role: "occ-founded".into(),
            argument: Argument::Count(312),
        };
        let small = Adjunct {
            role: "occ-founded".into(),
            argument: Argument::Count(7),
        };
        assert_eq!(
            common_role_surface(&big, &v),
            Some((AdjunctPosition::Inline, "founded in year 312".to_string()))
        );
        assert_eq!(
            common_role_surface(&small, &v),
            Some((AdjunctPosition::Inline, "founded in year seven".to_string()))
        );
    }

    /// An ending is a separate event, so it gets its own trailing clause —
    /// the position `day-length-std` already established, chosen per role.
    #[test]
    fn an_occupations_ending_is_trailing_not_inline() {
        let v = CommonVocabulary::default();
        let a = Adjunct {
            role: "occ-ended".into(),
            argument: Argument::Count(900),
        };
        assert_eq!(
            common_role_surface(&a, &v),
            Some((
                AdjunctPosition::Trailing,
                "it ended in year 900".to_string()
            ))
        );
    }

    #[test]
    fn an_unknown_role_surfaces_as_nothing_rather_than_as_a_key() {
        let v = CommonVocabulary::default();
        let a = Adjunct {
            role: "not-a-role".into(),
            argument: Argument::Count(1),
        };
        assert_eq!(common_role_surface(&a, &v), None);
    }

    /// Tier 1 coordination (The Mortise, Task 6, spec §4.10): two FULL
    /// clauses joined, nothing shared or elided — the gloss is *"It
    /// confused me and it upset me"*, built here from two predicates the
    /// crate already has words for (`eat`, `kill`) rather than inventing a
    /// `confuse`/`upset` pair this campaign does not register. Each clause
    /// realizes exactly as [`realize_common`] alone would (minus its own
    /// trailing period), joined by Common's own `"and"`, with exactly one
    /// trailing period on the whole coordinated utterance.
    ///
    /// **The two clauses deliberately have DIFFERENT subjects** (`Nwamvam`
    /// vs. `Bemvo`) — Task 6's original fixture gave both the same subject
    /// text, which was harmless before Task 7 landed elision but would now
    /// silently exercise tier 2 instead of the tier 1 this test names and
    /// documents. `two_clauses_coordinate_in_common` and
    /// `a_shared_subject_is_stated_once` are the deliberate pair: same
    /// subject elides, different subjects do not.
    #[test]
    fn two_clauses_coordinate_in_common() {
        let vocab = CommonVocabulary::default();
        let first = eat_clause(Tense::Past, Number::Sg, Polarity::Pos);
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Name("Bemvo".to_string()),
            object: Argument::Concept("goblin".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let coord = Coordination {
            clauses: vec![first.clone(), second.clone()],
        };
        let out = realize_common_coordination(&coord, &vocab);

        let mut first_text = realize_common(&first, &vocab);
        assert!(first_text.ends_with('.'));
        first_text.pop();
        let mut second_text = realize_common(&second, &vocab);
        assert!(second_text.ends_with('.'));
        second_text.pop();
        assert_eq!(out, format!("{first_text} and {second_text}."));
        // Exactly one full stop: each clause's own trailing "." is trimmed
        // before the join, the same discipline the embedded-clause arms of
        // `realize_common` already use.
        assert_eq!(out.matches('.').count(), 1);
    }

    /// Tier 2 (The Mortise, Task 7, spec §4.10): a shared subject is stated
    /// once — *"It confused me and upset me"* rather than *"It confused me
    /// and it upset me"*. Both clauses here share `Subject::Pronoun(Third)`
    /// at `Number::Sg`, so the second clause's own subject constituent must
    /// be entirely absent from the output, not merely rendered and matched
    /// against the first.
    ///
    /// **The expected text is derived, not hardcoded**: `common_pronoun`
    /// is the same function [`realize_common`] itself calls to resolve
    /// `Subject::Pronoun(Third)`, so this test does not restate a "the
    /// pronoun is `it`" fact the crate could later change out from under a
    /// literal string.
    #[test]
    fn a_shared_subject_is_stated_once() {
        let vocab = CommonVocabulary::default();
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
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let coord = Coordination {
            clauses: vec![first.clone(), second.clone()],
        };
        let out = realize_common_coordination(&coord, &vocab);

        let subject_text = common_pronoun(Person::Third, Number::Sg, PronounCase::Nominative);
        let mut first_text = realize_common(&first, &vocab);
        assert!(first_text.ends_with('.'));
        first_text.pop();

        let mut second_full = realize_common(&second, &vocab);
        assert!(second_full.ends_with('.'));
        second_full.pop();
        let prefix = format!("{subject_text} ");
        assert!(
            second_full.starts_with(&prefix),
            "sanity: realize_common alone states the subject: {second_full:?}"
        );
        let second_without_subject = second_full
            .strip_prefix(&prefix)
            .expect("checked above with starts_with");

        assert_eq!(out, format!("{first_text} and {second_without_subject}."));
        // The first clause states the subject once; the second states it
        // zero times — exactly one occurrence total, never two.
        assert_eq!(
            out.split_whitespace()
                .filter(|word| *word == subject_text)
                .count(),
            1,
            "a shared subject must surface exactly once: {out:?}"
        );
    }

    /// Tier 3 — right-node raising, sharing the OBJECT as well as the
    /// subject (*"It confused and upset me"*) — is CUT from this campaign
    /// (spec §9.1, The Mortise Task 7) and this test is the assertion that
    /// keeps that cut from being silently un-cut: a later campaign that
    /// wants tier 3 has to come here and change this test on purpose.
    ///
    /// Two clauses share an object concept (`"goblin"`) but have DIFFERENT
    /// subjects, so tier 2 does not fire and cannot be confused with tier 3
    /// here — this isolates object-sharing from subject-sharing. Both
    /// clauses' own resolved complement word must appear in the output,
    /// once per clause: raising it once, the way tier 3 would, is exactly
    /// what must NOT happen.
    #[test]
    fn a_shared_object_is_not_raised() {
        let vocab = CommonVocabulary::default();
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Name("Bemvo".to_string()),
            object: Argument::Concept("goblin".to_string()),
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
        let out = realize_common_coordination(&coord, &vocab);

        let complement_word = surface_complement(&vocab, "goblin", Number::Sg);
        let occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == complement_word)
            .count();
        assert_eq!(
            occurrences, 2,
            "tier 3 (right-node raising) is CUT: the shared object must be \
             stated on EACH verb, never raised to a single mention: {out:?}"
        );
    }

    /// Fix round 1, Task 7: the reviewer's own discriminator. The negative
    /// test above deliberately isolates object-sharing from subject-sharing
    /// by giving its two clauses DIFFERENT subjects — which means a
    /// plausible tier-3 implementation gated on `elide && object equal`
    /// (the reviewer's own probe) never even runs its mutated branch there,
    /// so that test alone cannot tell tier 3 apart from tier 2 working
    /// correctly. This test closes that gap: the two clauses here share
    /// BOTH the subject (so tier 2 correctly elides) AND the object concept
    /// (so tier 3 must still NOT raise it) — the exact shape of the brief's
    /// own tier-3 example, *"It confused and upset me"*.
    #[test]
    fn a_shared_object_is_not_raised_even_when_the_subject_also_elides() {
        let vocab = CommonVocabulary::default();
        let first = Clause {
            predicate: EAT.to_string(),
            subject: Subject::Pronoun(Person::Third),
            object: Argument::Concept("goblin".to_string()),
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
            object: Argument::Concept("goblin".to_string()), // SAME object: tier 3 must NOT raise
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
        let out = realize_common_coordination(&coord, &vocab);

        let subject_text = common_pronoun(Person::Third, Number::Sg, PronounCase::Nominative);
        let complement_word = surface_complement(&vocab, "goblin", Number::Sg);
        let subject_occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == subject_text)
            .count();
        let object_occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == complement_word)
            .count();

        assert_eq!(
            subject_occurrences, 1,
            "the shared subject must still elide on this pair: {out:?}"
        );
        assert_eq!(
            object_occurrences, 2,
            "tier 3 stays cut even on the exact pair whose subject tier 2 \
             elides -- a plausible tier-3 implementation gated on \
             `elide && object equal` fires HERE and only here: {out:?}"
        );
    }

    /// Fix round 1, Task 7: the reviewer's own probe, run directly against
    /// [`elide_coordinated_subjects`]. `[X, Y, X]` (`Pronoun(Third)`,
    /// `Name("Bemvo")`, `Pronoun(Third)` again) is the shape that
    /// discriminates "compare to the first clause" (which wrongly elides
    /// clause 2, index 2) from "compare to the last STATED subject" (which
    /// correctly does not, since clause 1's `Name("Bemvo")` is the visible
    /// antecedent immediately before it).
    #[test]
    fn elide_coordinated_subjects_compares_to_the_last_stated_not_the_first() {
        let x = Subject::Pronoun(Person::Third);
        let y = Subject::Name("Bemvo".to_string());
        let clause_with = |subject: Subject| Clause {
            predicate: EAT.to_string(),
            subject,
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let clauses = vec![clause_with(x.clone()), clause_with(y), clause_with(x)];
        assert_eq!(
            elide_coordinated_subjects(&clauses),
            vec![false, false, false],
            "clause 0 always states; clause 1 (Y) differs from clause 0's \
             last-stated X, so it states too; clause 2 (X) differs from \
             clause 1's last-stated Y, so it must ALSO state -- eliding it \
             here would misattribute the missing subject to clause 1's Y"
        );
    }

    /// The successor case the reviewer's ruling worked by hand: `[X, X, Y,
    /// X]` should elide clause 1 (matches clause 0's stated X), state
    /// clause 2 (Y, no match), then state clause 3 again (X does not match
    /// the last-stated Y) -- "compare to the first clause" would have
    /// elided clause 3 too, since it matches clause 0.
    #[test]
    fn elide_coordinated_subjects_restates_after_an_intervening_different_subject() {
        let x = Subject::Pronoun(Person::Third);
        let y = Subject::Name("Bemvo".to_string());
        let clause_with = |subject: Subject| Clause {
            predicate: EAT.to_string(),
            subject,
            object: Argument::Concept("bread".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let clauses = vec![
            clause_with(x.clone()),
            clause_with(x.clone()),
            clause_with(y),
            clause_with(x),
        ];
        assert_eq!(
            elide_coordinated_subjects(&clauses),
            vec![false, true, false, false]
        );
    }

    /// The integration-level twin of the two `elide_coordinated_subjects`
    /// unit tests above, run through the public
    /// [`realize_common_coordination`] entry point -- proof the discriminator
    /// is reachable through the type the brief flagged as unguarded
    /// (`Coordination.clauses` is a `Vec` with no cap beyond `len() >= 2`).
    /// `[X, Y, X]`: clause 2 must restate its own subject text, not be
    /// silently absent the way comparing only to clause 0 would produce.
    #[test]
    fn a_third_clause_matching_only_the_first_clause_states_its_own_subject() {
        let vocab = CommonVocabulary::default();
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
            subject: Subject::Name("Bemvo".to_string()),
            object: Argument::Concept("goblin".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let third = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::Third), // matches FIRST, not SECOND
            object: Argument::Concept("goblin".to_string()),
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
        let out = realize_common_coordination(&coord, &vocab);

        let subject_text = common_pronoun(Person::Third, Number::Sg, PronounCase::Nominative);
        let occurrences = out
            .split_whitespace()
            .filter(|word| word.trim_end_matches('.') == subject_text)
            .count();
        assert_eq!(
            occurrences, 2,
            "clause 0 and clause 2 must BOTH state the pronoun subject: \
             clause 2's visible antecedent is clause 1's Name(\"Bemvo\"), \
             not clause 0, so it may not elide: {out:?}"
        );
    }

    /// A coordination of fewer than two clauses states a contradiction in
    /// its own name — nothing to coordinate — and both realizers refuse it
    /// by panic rather than silently degrading to a bare clause.
    #[test]
    #[should_panic(expected = "at least two clauses")]
    fn a_coordination_of_one_clause_panics() {
        let vocab = CommonVocabulary::default();
        let only = eat_clause(Tense::Present, Number::Sg, Polarity::Pos);
        let coord = Coordination {
            clauses: vec![only],
        };
        let _ = realize_common_coordination(&coord, &vocab);
    }

    /// A polar question inverts the copula and takes a question mark — the
    /// ladder's `r083`, *"Are you a merchant?"*.
    ///
    /// **Force is an OPERATOR over a clause, never a field on it.**
    /// [`Clause`] is fact-shaped — decision 0266, *an utterance is a fact* —
    /// and a question asserts nothing, so a `force` field would falsify the
    /// shape claim for every clause in order to serve one. [`Coordination`]
    /// is the precedent: a construction above the clause gets its own
    /// realizer, not a flag inside it (decision 0327).
    ///
    /// The surface is lowercase, like every other witness in this file:
    /// Common's realizer never capitalizes sentence-initially (see
    /// [`nominative_person`]'s case contract), so the rung's own capitalized
    /// *"Are you a merchant?"* differs from the realized surface in exactly
    /// that one way and no other.
    #[test]
    fn a_polar_question_inverts_the_copula_and_takes_a_question_mark() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Second),
            object: Argument::Concept("merchant".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common_polar_question(&clause, &vocab),
            "are you a merchant?"
        );
    }

    /// The declarative is untouched by the operator existing: the same
    /// clause realized through [`realize_common`] is byte-identical to what
    /// it produced before this task, and differs from the question in
    /// exactly the two ways the operator states (the verb group's position,
    /// and the terminal literal).
    #[test]
    fn the_question_operator_does_not_change_the_declarative() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Second),
            object: Argument::Concept("merchant".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(realize_common(&clause, &vocab), "you are a merchant.");
        assert_eq!(
            realize_common_polar_question(&clause, &vocab),
            "are you a merchant?"
        );
    }

    /// The past copula inverts the same way — the witness `m08` stands in
    /// with, and the pair of features that entry's demand tokens actually
    /// name (`polar-question` + `past-tense`).
    #[test]
    fn a_past_polar_question_inverts_the_past_copula() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Pronoun(Person::Second),
            object: Argument::Concept("merchant".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common_polar_question(&clause, &vocab),
            "were you a merchant?"
        );
    }

    /// Inversion is read off the construction, so EVERY copular valence
    /// inverts — not just `IS_A`'s. A property predication and a locative
    /// predication both have a `Part::Copula` and both come out right with
    /// no arm of their own, which is the evidence that the operator is a
    /// reordering of `common_constructions` rather than a second surface.
    #[test]
    fn every_copular_valence_inverts_with_no_arm_of_its_own() {
        let vocab = CommonVocabulary::default();
        let property = Clause {
            predicate: OLD.to_string(),
            subject: Subject::Name("the road".to_string()),
            object: Argument::Absent,
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common_polar_question(&property, &vocab),
            "is the road old?"
        );
        let locative = Clause {
            predicate: UNDER.to_string(),
            subject: Subject::Name("the merchant".to_string()),
            object: Argument::Concept("tree".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        assert_eq!(
            realize_common_polar_question(&locative, &vocab),
            "is the merchant under the tree?"
        );
    }

    /// A construction whose verb group is a LEXICAL VERB is refused, loudly.
    ///
    /// *"Sleeps the guard?"* is not Common. English asks a lexical verb with
    /// periphrastic *do*-support, which needs a mood axis on
    /// [`VERB_PARADIGM`]'s bidirectional key and is a later campaign's — so
    /// this operator panics rather than emitting the plausible garbage, the
    /// same posture [`realize_common`] takes for an unconstructed predicate.
    #[test]
    #[should_panic(expected = "Common inverts a copula")]
    fn a_polar_question_on_a_lexical_verb_panics() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: SLEEP.to_string(),
            subject: Subject::Name("the guard".to_string()),
            object: Argument::Absent,
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let _ = realize_common_polar_question(&clause, &vocab);
    }

    /// The transitive frame is refused for the same reason, and this is the
    /// case that matters to the corpus: `m08` is *"Did you know the woman?"*,
    /// a PAST question on the lexical verb `know`. *"Knew you the woman?"* is
    /// not Common either, so the entry's witness cannot be its own sentence.
    #[test]
    #[should_panic(expected = "Common inverts a copula")]
    fn a_polar_question_on_the_transitive_frame_panics() {
        let vocab = CommonVocabulary::default();
        let clause = Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::Second),
            object: Argument::Concept("person".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        };
        let _ = realize_common_polar_question(&clause, &vocab);
    }

    /// [`resolve_embedded_number`]'s SECOND signal, made observable.
    ///
    /// **The behaviour moved in The Rail's Task 6 and nothing pinned either
    /// direction** (found in Task 6's review, fixed here). Under the old
    /// number-only key `"know"` named the plural row and nothing else, so
    /// `numbers` was a singleton and the first signal answered outright:
    /// a first-person SINGULAR matrix clause came back `number: Pl`,
    /// silently wrong. The widened key spells `"know"` at five rows, so the
    /// singleton is gone and the subject's own pronoun row — `"I"` is 1sg
    /// and nothing else — is what decides it.
    ///
    /// **What this test pins is the fallback, not the person filter**, and
    /// the distinction was measured rather than assumed: deleting
    /// [`parse_clause_body`]'s `h.6 == person` narrowing leaves both this
    /// test and its sibling below GREEN (the fallback still intersects to
    /// `Sg`), while deleting the fallback reddens this one and loosening it
    /// to `pronoun_rows.first()` reddens the sibling. The narrowing has its
    /// own guard in
    /// [`an_agreement_violating_sentence_does_not_parse`]; this pair guards
    /// the signal the widened key made load-bearing.
    ///
    /// (The embedded clause takes a CONCEPT complement rather than a pronoun
    /// object — a pronoun in the object slot is not recoverable by this walk
    /// at all — and its copula is `is`, because Common spells third-person
    /// SINGULAR `they is` (spec §4.5's animate-neutral pronoun, see
    /// `commons_one_third_person_singular_is_the_animate_neutral_one`).
    /// Either substitution avoided would have made the test fail for a
    /// reason that has nothing to do with number.)
    #[test]
    fn an_embedded_clause_reads_its_number_off_a_first_person_subject() {
        let parsed = parse_common("I know they is a planet.", &ctx(&["planet"]))
            .expect("the embedding round-trips");
        assert_eq!(parsed.subject, Subject::Pronoun(Person::First));
        assert_eq!(
            parsed.number,
            Number::Sg,
            "a first-person-singular matrix clause is Sg; before the widened \
             paradigm this returned Pl, because `know` named the plural row \
             alone and the first signal never consulted the subject"
        );
    }

    /// The other half of the same change: a second-person present embedding
    /// now FAILS CLOSED where it used to return `Pl`.
    ///
    /// `"you"` is spelled identically at 2sg and 2pl
    /// ([`PRONOUN_PARADIGM`]), and the present-tense verb group is syncretic
    /// across both, so neither of [`resolve_embedded_number`]'s two signals
    /// is a singleton. Refusing is the correct answer — the old `Pl` was a
    /// guess that happened to be spelled like an answer — and this pins it as
    /// a deliberate loss rather than an undiscovered gap. The positive
    /// control for the same sentence shape is the test directly above, which
    /// differs only in its subject pronoun.
    #[test]
    fn a_second_person_embedding_refuses_rather_than_guessing_its_number() {
        assert!(
            parse_common("you know they is a planet.", &ctx(&["planet"])).is_err(),
            "`you` and the present `know` are both syncretic across number, \
             so nothing in the sentence decides it and the walk must refuse"
        );
    }
}
