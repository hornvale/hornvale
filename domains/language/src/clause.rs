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
use crate::packs::{EAT, KILL, KNOW, THINK};
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
/// grow. Every later embedding site (a clause-carrying subject, a
/// coordinated clause) reads this same constant rather than stating its own
/// number, so raising the cap later is a one-line change here.
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
    /// The subject slot (a `Subject::Name` or `Subject::Pronoun`).
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
    /// The adjunct tail: inline adjuncts first (a `' '` before the first,
    /// `", "` between the rest), then each trailing adjunct after `"; "`.
    ModifierTail,
    /// A fixed literal (spacing, terminal punctuation).
    Literal(&'static str),
}

/// One row of [`COPULA_PARADIGM`]: a Common surface form paired with the
/// three features it realizes forward and recovers backward.
/// type-audit: bare-ok(prose: CopulaRow)
pub type CopulaRow = (&'static str, Tense, Number, Polarity);

/// Common's copula paradigm: `{Present, Past} × {Sg, Pl} × {Pos, Neg}` →
/// surface form. **One table, read in both directions** — [`realize_common`]
/// looks a row up by its features, and [`parse_common_with_tail`] searches a
/// sentence for any row's form and reads the features off it. That is the
/// same "bidirectional by construction" discipline [`common_constructions`]
/// states for the clause skeleton, applied one level down: a copula form
/// cannot be realizable but unrecognizable, or the reverse.
///
/// Negation is **appended to the copula** rather than given a slot of its
/// own; see [`Part::Copula`] for why there is no `Part::Negator`.
/// type-audit: bare-ok(prose: COPULA_PARADIGM)
pub const COPULA_PARADIGM: &[CopulaRow] = &[
    ("is", Tense::Present, Number::Sg, Polarity::Pos),
    ("are", Tense::Present, Number::Pl, Polarity::Pos),
    ("was", Tense::Past, Number::Sg, Polarity::Pos),
    ("were", Tense::Past, Number::Pl, Polarity::Pos),
    ("is not", Tense::Present, Number::Sg, Polarity::Neg),
    ("are not", Tense::Present, Number::Pl, Polarity::Neg),
    ("was not", Tense::Past, Number::Sg, Polarity::Neg),
    ("were not", Tense::Past, Number::Pl, Polarity::Neg),
];

/// The copula slot's surface for one clause's features — the forward read of
/// [`COPULA_PARADIGM`]. Panics only if the table is missing a row, which the
/// `copula_paradigm_is_total` test makes impossible.
fn copula_surface(tense: Tense, number: Number, polarity: Polarity) -> &'static str {
    COPULA_PARADIGM
        .iter()
        .find(|(_, t, n, p)| *t == tense && *n == number && *p == polarity)
        .map(|(form, _, _, _)| *form)
        .expect("the copula paradigm is total over tense x number x polarity")
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
/// **Two roughnesses, both deliberate and both asserted by tests** so they
/// arrive as visible facts rather than surprises, the same posture
/// [`VERB_PARADIGM`]'s `eated` takes:
///
/// 1. **Common has no person agreement.** [`COPULA_PARADIGM`] and
///    [`VERB_PARADIGM`] are keyed by number only, so a first-person subject
///    in the positive present surfaces third-person agreement (*"I knows
///    them"*). Adding a person axis to both tables is a real widening that
///    also changes the parse-side search, and it buys nothing the corpus
///    needs: the campaign's own line is *"I did not know them"*, and English
///    negation is periphrastic, so the number-blind row is already right.
/// 2. **Third-person singular is `they`/`them`, not `it`.** Spec §4.5 fixes
///    this: nothing in the ledger assigns gender or animacy to a clause, so
///    Common has one third-person singular and it is the animate-neutral
///    one. The cost is that an inanimate re-mention reads *"they is a
///    planet"* — a genuinely awkward line, and exactly the "controlled
///    register with slightly awkward phrasing" §4.5 names as the accepted
///    trade. It reaches no committed artifact: no volume the book renders
///    ever re-mentions a subject.
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
/// **stem**, paired with the three features the resulting group realizes
/// forward and recovers backward.
///
/// A pair rather than a single form because a lexical verb's surface is
/// stem-dependent, which is the one way this table differs from
/// [`COPULA_PARADIGM`]: the copula is suppletive, so its rows carry literal
/// words, while `eat` needs `("", "s")` applied to it. English negation is
/// periphrastic, so the negative rows put the whole auxiliary in the
/// PREFIX and leave the stem bare — which is also why negation needs no
/// `Part::Negator` here any more than it did for the copula.
/// type-audit: bare-ok(prose: VerbRow)
pub type VerbRow = (&'static str, &'static str, Tense, Number, Polarity);

/// Common's lexical-verb paradigm: `{Present, Past} × {Sg, Pl} × {Pos, Neg}`
/// → the affixes wrapping a verb stem. **One table, read in both
/// directions**, exactly as [`COPULA_PARADIGM`] is — [`realize_common`]
/// looks a row up by its features, and [`parse_common_with_tail`] generates
/// every row's surface for every construction's stem and searches for one.
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
/// **Not injective, unlike the copula's.** The past neutralizes number in
/// both polarities (`killed`, `did not kill`), so eight rows yield six
/// forms. The parse direction therefore recovers a candidate SET of numbers
/// from the verb group and lets the object's own plural decide — see
/// [`parse_common_with_tail`].
/// type-audit: bare-ok(prose: VERB_PARADIGM)
pub const VERB_PARADIGM: &[VerbRow] = &[
    ("", "s", Tense::Present, Number::Sg, Polarity::Pos),
    ("", "", Tense::Present, Number::Pl, Polarity::Pos),
    ("", "ed", Tense::Past, Number::Sg, Polarity::Pos),
    ("", "ed", Tense::Past, Number::Pl, Polarity::Pos),
    ("does not ", "", Tense::Present, Number::Sg, Polarity::Neg),
    ("do not ", "", Tense::Present, Number::Pl, Polarity::Neg),
    ("did not ", "", Tense::Past, Number::Sg, Polarity::Neg),
    ("did not ", "", Tense::Past, Number::Pl, Polarity::Neg),
];

/// The verb slot's surface for one stem and one clause's features — the
/// forward read of [`VERB_PARADIGM`]. Panics only if the table is missing a
/// row, which the `verb_paradigm_is_total_and_syncretic_in_the_past` test
/// makes impossible.
fn verb_surface(stem: &str, tense: Tense, number: Number, polarity: Polarity) -> String {
    let (prefix, suffix) = VERB_PARADIGM
        .iter()
        .find(|(_, _, t, n, p)| *t == tense && *n == number && *p == polarity)
        .map(|(prefix, suffix, _, _, _)| (*prefix, *suffix))
        .expect("the verb paradigm is total over tense x number x polarity");
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
const PREDICATE_VALENCE: &[(&str, Valence)] = &[
    (IS_A, Valence::Nominal),
    (EAT, Valence::Transitive),
    (KILL, Valence::Transitive),
    (KNOW, Valence::Transitive),
    (THINK, Valence::Transitive),
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
fn verb_group_forms(
    construction: &Construction,
    vocab: &CommonVocabulary,
) -> Vec<(String, Tense, Number, Polarity)> {
    if construction.parts.contains(&Part::Copula) {
        COPULA_PARADIGM
            .iter()
            .map(|(form, t, n, p)| ((*form).to_string(), *t, *n, *p))
            .collect()
    } else if construction.parts.contains(&Part::Verb) {
        let stem = vocab.word_for(construction.predicate);
        VERB_PARADIGM
            .iter()
            .map(|(prefix, suffix, t, n, p)| (format!("{prefix}{stem}{suffix}"), *t, *n, *p))
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
    let construction = common_constructions()
        .iter()
        .find(|c| c.predicate == spec.predicate)
        .unwrap_or_else(|| {
            panic!(
                "Common has no construction for predicate {:?}",
                spec.predicate
            )
        });
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
    };
    let mut out = String::new();
    for part in construction.parts {
        match part {
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
                out.push_str(copula_surface(spec.tense, spec.number, spec.polarity));
            }
            // The clause's own predicate, through the same vocabulary the
            // complement goes through: Common resolves a concept id, it
            // never echoes one.
            Part::Verb => out.push_str(&verb_surface(
                &vocab.word_for(&spec.predicate),
                spec.tense,
                spec.number,
                spec.polarity,
            )),
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
    out
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
/// **Tier 1 only** (spec §4.10's three-tier ladder): each clause realizes
/// in full, with nothing shared or elided between them — *"It confused me
/// and it upset me"* rather than *"It confused and upset me"*. Tier 2
/// (subject elision, so a shared subject is stated once) and tier 3
/// (right-node raising) are later work; tier 3 is cut from this campaign
/// entirely (spec §9.1).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct Coordination {
    /// The coordinated clauses, in surface order. At least two — a
    /// "coordination" of fewer than two clauses is not a list, and both
    /// realizers assert this rather than silently degrading.
    pub clauses: Vec<Clause>,
}

/// Realize a [`Coordination`] as a Common (≈ limited English) sentence: each
/// clause realizes through [`realize_common`] in full (tier 1 — nothing
/// shared or elided), trimmed of its own trailing full stop, then joined
/// with `"and"` — Common's own coordinating conjunction, on the same
/// footing every other Common surface choice is (this register's fixed
/// vocabulary, not a drawn value; only a TONGUE's conjunction is drawn, see
/// [`crate::grammar::realize_tongue_coordination`]) — and the whole
/// sentence takes exactly one trailing period, on the identical
/// "realize whole and trim" discipline [`realize_common`]'s own
/// `Argument::Clause`/`Subject::Clause` arms already use for a nested
/// clause.
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
    let mut parts = coord.clauses.iter().map(|clause| {
        let mut text = realize_common(clause, vocab);
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
    /// numbers the verb group left open.
    UnknownComplement {
        /// The unrecognized text following the determiner.
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
    // Subject | verb group: every construction contributes every surface its
    // verb group can take (a copula form, or its own stem run through
    // `VERB_PARADIGM`), and the sentence is searched for all of them at once.
    // This is where the construction table's "bidirectional by construction"
    // promise is actually cashed: the predicate comes back from the row that
    // matched, never from an assumption about which construction this was.
    let mut hits: Vec<(usize, String, &'static str, Tense, Number, Polarity)> = Vec::new();
    for construction in common_constructions() {
        for (form, tense, number, polarity) in verb_group_forms(construction, &ctx.vocabulary) {
            if let Some(at) = body.find(&format!(" {form} ")) {
                hits.push((at, form, construction.predicate, tense, number, polarity));
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
    let Some((at, form, predicate, tense, _, polarity)) = hits.first().cloned() else {
        return Err(ParseError::NoVerbGroup);
    };
    // Number is the one feature a verb group may leave underdetermined:
    // `VERB_PARADIGM` is syncretic in the past (`killed` is both), so the
    // winning form can name more than one row. Carry every candidate and let
    // the object's own plural decide below — the copula is suppletive, so
    // for a classification this list is always a singleton.
    let numbers: Vec<Number> = hits
        .iter()
        .filter(|h| h.0 == at && h.1 == form && h.2 == predicate)
        .map(|h| h.4)
        .collect();
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
    let (complement_concept, number, surface) =
        best_complement.ok_or_else(|| ParseError::UnknownComplement {
            after: after_det.to_string(),
        })?;
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

    /// `copula_surface` panics on a missing row, so the table's totality over
    /// `tense x number x polarity` is what makes that panic unreachable. It
    /// is asserted rather than assumed because the paradigm is a `const`
    /// slice, not a match — the compiler cannot check its exhaustiveness the
    /// way it checked `Frame`'s.
    #[test]
    fn copula_paradigm_is_total_and_unambiguous() {
        let mut forms: std::collections::BTreeSet<&str> = std::collections::BTreeSet::new();
        let mut combinations = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for number in [Number::Sg, Number::Pl] {
                for polarity in [Polarity::Pos, Polarity::Neg] {
                    forms.insert(copula_surface(tense, number, polarity));
                    combinations += 1;
                }
            }
        }
        assert_eq!(combinations, 8);
        // Distinct forms: a repeated one would make the parse direction
        // ambiguous, which is the failure this table's bidirectionality
        // exists to prevent.
        assert_eq!(forms.len(), COPULA_PARADIGM.len());
        assert_eq!(COPULA_PARADIGM.len(), 8);
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
            }
        }
        // And the other direction: nothing in the inventory is unreachable
        // from Common. `predicate_valence` is what the TONGUE path asks, so
        // a predicate it answers for and Common cannot realize would panic
        // in one realizer and not the other.
        assert_eq!(
            inv.len(),
            [IS_A, EAT, KILL, KNOW, THINK]
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

    /// [`VERB_PARADIGM`]'s totality, and the one place it is deliberately
    /// NOT injective. `copula_paradigm_is_total_and_unambiguous` asserts
    /// eight distinct forms; this table has eight rows and only **six**
    /// distinct forms, because English lexical verbs neutralize number in
    /// the past (`was`/`were` vs. a single `killed`). That is a fact about
    /// the target language, not a hole — and it is why the parse direction
    /// carries a candidate SET of numbers and lets the object's own surface
    /// break the tie.
    #[test]
    fn verb_paradigm_is_total_and_syncretic_in_the_past() {
        let mut forms: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        let mut combinations = 0usize;
        for tense in [Tense::Present, Tense::Past] {
            for number in [Number::Sg, Number::Pl] {
                for polarity in [Polarity::Pos, Polarity::Neg] {
                    forms.insert(verb_surface(KILL, tense, number, polarity));
                    combinations += 1;
                }
            }
        }
        assert_eq!(combinations, 8);
        assert_eq!(VERB_PARADIGM.len(), 8);
        assert_eq!(
            forms.len(),
            6,
            "the past neutralizes number in both polarities: {forms:?}"
        );
        assert!(forms.contains("killed"));
        assert!(forms.contains("did not kill"));
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

    /// The two roughnesses [`PRONOUN_PARADIGM`]'s doc names, asserted so they
    /// are visible facts rather than surprises — the same posture
    /// `a_transitive_verb_inflects_for_tense_number_and_polarity` takes with
    /// `eated`. An irregular fix to either arrives as a red test rather than
    /// a silent correction.
    #[test]
    fn common_has_no_person_agreement_and_one_third_person_singular() {
        let vocab = CommonVocabulary::default();
        // 1. No person agreement: the verb paradigm is keyed by NUMBER, so a
        //    first-person subject takes the third-person singular present.
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
        assert_eq!(realize_common(&eats, &vocab), "I eats the bread.");
        // 2. One third person singular, and it is the animate-neutral one
        //    (spec 4.5: nothing in the ledger assigns gender or animacy to a
        //    clause). An inanimate re-mention therefore reads awkwardly.
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
            // Third person: its number is the clause's own, so this one
            // subject covers "they"/"they" across the Sg and Pl legs of the
            // enumeration below rather than needing two entries.
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
    #[test]
    fn two_clauses_coordinate_in_common() {
        let vocab = CommonVocabulary::default();
        let first = eat_clause(Tense::Past, Number::Sg, Polarity::Pos);
        let second = Clause {
            predicate: KILL.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
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
}
