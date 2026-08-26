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
use hornvale_kernel::world::IS_A;

/// Grammatical number of the subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Number {
    /// singular
    Sg,
    /// plural
    Pl,
}
/// Whether the complement is introduced with a/the or bare.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Definiteness {
    /// a/an
    Indef,
    /// the
    Def,
}

/// A clause's subject: a resolved name/noun, or a fixed pronoun for
/// re-mention (e.g. a second sentence about the same referent).
/// type-audit: bare-ok(identifier-text: Name.0), bare-ok(prose: Pronoun.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Subject {
    /// An already-resolved proper name or noun phrase.
    Name(String),
    /// A fixed pronoun lexeme (e.g. `"it"`, `"its"`).
    Pronoun(&'static str),
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
}

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
    /// Subject number.
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
    /// The copula, agreeing with `Clause.number` (`is`/`are`).
    Copula,
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

/// The Common construction inventory, keyed by **predicate id**. One entry
/// today ([`hornvale_kernel::world::IS_A`], the classification); every future
/// predicate adds an entry, never a second code path.
///
/// **The key is the kernel's constant, not a local literal**, and that is
/// load-bearing rather than tidy. `Frame::Classify` made this lookup
/// statically total: an unhandled variant was a compile error. A string key
/// moves that check to runtime ([`realize_common`] panics on a miss), so the
/// only thing left holding the two ends together is that the producer and
/// this table name the same constant. A kernel epoch bump of `IS_A` must
/// break the render, not recompile cleanly and panic on every gallery page.
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
    &[Construction {
        predicate: IS_A,
        parts: CLASSIFY,
    }]
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
    };
    let mut out = String::new();
    for part in construction.parts {
        match part {
            Part::Subject => out.push_str(match &spec.subject {
                Subject::Name(name) => name.as_str(),
                Subject::Pronoun(pronoun) => pronoun,
            }),
            Part::Copula => out.push_str(match spec.number {
                Number::Sg => "is",
                Number::Pl => "are",
            }),
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
    /// Neither `" is "` nor `" are "` appears, so no subject/copula split
    /// exists.
    NoCopula,
    /// The text after the determiner doesn't match (a prefix of) any
    /// complement surface in the caller's `ParseContext`.
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
            ParseError::NoCopula => write!(f, "no ' is '/' are ' copula found"),
            ParseError::UnknownComplement { after } => {
                write!(f, "no registered complement matches '{after}'")
            }
            ParseError::Unterminated => write!(f, "sentence has no terminal '.'"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Invert `realize_common`: parse a Common sentence back into the
/// `Clause` that would realize it. Walks the `Classify` construction's
/// entry backward — the boundaries come from the construction's shape, and
/// the subject/copula split happens at the EARLIEST `" is "`/`" are "`
/// occurrence (so a subject itself never contains the copula word).
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
    // Subject | Copula: split at the earliest " is " / " are ".
    let is_at = body.find(" is ");
    let are_at = body.find(" are ");
    let (subject_text, number, rest) = match (is_at, are_at) {
        (Some(i), Some(a)) if i < a => (&body[..i], Number::Sg, &body[i + 4..]),
        (Some(i), None) => (&body[..i], Number::Sg, &body[i + 4..]),
        (_, Some(a)) => (&body[..a], Number::Pl, &body[a + 5..]),
        (None, None) => return Err(ParseError::NoCopula),
    };
    let subject = match subject_text {
        "it" => Subject::Pronoun("it"),
        "its" => Subject::Pronoun("its"),
        name => Subject::Name(name.to_string()),
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
    // Complement: longest SURFACE match from the closed set of ids. Ties go
    // to the last id in `complements`' (BTreeSet) order — deterministic, and
    // unreachable today since no two ids share a surface.
    let (complement_concept, surface) = ctx
        .complements
        .iter()
        .map(|concept| {
            (
                concept,
                surface_complement(&ctx.vocabulary, concept, number),
            )
        })
        .filter(|(_, s)| {
            after_det == s.as_str()
                || after_det
                    .strip_prefix(s.as_str())
                    .is_some_and(|r| r.starts_with(' '))
        })
        .max_by_key(|(_, s)| s.len())
        .map(|(concept, s)| (concept.clone(), s))
        .ok_or_else(|| ParseError::UnknownComplement {
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
            // The kernel's constant, matching the construction this walk
            // inverted — never a literal, so the two ends cannot drift.
            predicate: IS_A.to_string(),
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
    fn classify_has_one_declared_construction() {
        let inv = common_constructions();
        assert_eq!(inv.len(), 1);
        // Against the KERNEL's constant, not the literal it happens to
        // equal: the table's key and every producer's key are the same
        // `const` by construction, which is what replaced the static
        // totality `Frame::Classify` used to give this lookup.
        assert_eq!(inv[0].predicate, hornvale_kernel::world::IS_A);
        assert_eq!(
            inv[0].parts,
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
        // copula search ever runs — the terminal check is the FIRST gate.
        // NoCopula needs a terminated sentence that still lacks " is "/" are ".
        assert!(matches!(
            parse_common("wordless.", &ctx(&["planet"])),
            Err(ParseError::NoCopula)
        ));
        assert!(matches!(
            parse_common("wordless", &ctx(&["planet"])),
            Err(ParseError::Unterminated)
        ));
    }

    #[test]
    fn pronoun_subjects_are_lowercase_by_contract() {
        // The re-mention path emits lowercase "it"; parse binds it as a
        // Pronoun. A capitalized "It" is NOT recognized as a pronoun — if a
        // future construction capitalizes sentence-initial pronouns, this
        // canary reddens and the parse-side binding must learn case together
        // with it (never separately).
        let c = ctx(&["planet"]);
        assert_eq!(
            parse_common("it is a planet.", &c).unwrap().subject,
            Subject::Pronoun("it")
        );
        assert_eq!(
            parse_common("It is a planet.", &c).unwrap().subject,
            Subject::Name("It".into())
        );
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
            Subject::Pronoun("it"),
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
}
