//! The clause layer: a language-neutral ClauseSpec and the Common realizer.
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

/// The construction a clause realizes. C1 has one: classification (`isA`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Frame {
    /// X is a Y.
    Classify,
}
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

/// A language-neutral clause: predicate-argument structure plus features.
/// The per-language realizer decides how (and whether) each feature surfaces.
/// type-audit: bare-ok(identifier-text: complement_concept), bare-ok(prose: modifiers)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClauseSpec {
    /// The construction.
    pub frame: Frame,
    /// The subject: a resolved name, or a pronoun for re-mention.
    pub subject: Subject,
    /// The complement **concept id**, resolved through the
    /// [`CommonVocabulary`] at realization (never a word the caller chose).
    pub complement_concept: String,
    /// Subject number.
    pub number: Number,
    /// Complement definiteness.
    pub definiteness: Definiteness,
    /// Additional modifier phrases appended after the complement head, in
    /// order (e.g. `"with two moons"`, `"orbiting a yellow-white dwarf"`).
    /// The first attaches with a space, later ones join with `", "`; any
    /// leading `with`/`orbiting` wording lives in the modifier string
    /// itself — this layer only joins.
    pub modifiers: Vec<String>,
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
    /// The copula, agreeing with `ClauseSpec.number` (`is`/`are`).
    Copula,
    /// The determiner slot (`the `/`a `/`an `/bare), from definiteness + number.
    Determiner,
    /// The complement lexeme.
    Complement,
    /// The modifier tail: first joins with `' '`, later with `", "`.
    ModifierTail,
    /// A fixed literal (spacing, terminal punctuation).
    Literal(&'static str),
}

/// A form↔meaning pairing: one clause frame's surface as an ordered part
/// list. The same entry realizes forward and parses backward — a future
/// frame is added HERE, and is bidirectional by construction.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug)]
pub struct Construction {
    /// The frame this entry realizes/recognizes.
    pub frame: Frame,
    /// The ordered surface parts.
    pub parts: &'static [Part],
}

/// The Common construction inventory. One entry today (`Classify`); every
/// future frame adds an entry, never a second code path.
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
        frame: Frame::Classify,
        parts: CLASSIFY,
    }]
}

/// Realize a ClauseSpec as a Common (≈ limited English) sentence, resolving
/// `spec.complement_concept` through `vocab`.
///
/// **Infallible, and deliberately so.** Common is the author's register, not
/// a people's tongue: [`CommonVocabulary::word_for`] is total, so there is no
/// `CommonGap` to return. A gap therefore always means something true about
/// the world (this people has no word for the sea) rather than an authoring
/// hole, because only the tongue path can gap at all.
///
/// The article is chosen from the **resolved word**, not the id — so `an`
/// still fires for `elemental`, and now also for a declared multi-word
/// display.
/// type-audit: bare-ok(prose)
pub fn realize_common(spec: &ClauseSpec, vocab: &CommonVocabulary) -> String {
    let construction = common_constructions()
        .iter()
        .find(|c| c.frame == spec.frame)
        .expect("every Frame has a construction");
    let complement = surface_complement(vocab, &spec.complement_concept, spec.number);
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
                for (i, modifier) in spec.modifiers.iter().enumerate() {
                    out.push_str(if i == 0 { " " } else { ", " });
                    out.push_str(modifier);
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
/// `ClauseSpec` that would realize it. Walks the `Classify` construction's
/// entry backward — the boundaries come from the construction's shape, and
/// the subject/copula split happens at the EARLIEST `" is "`/`" are "`
/// occurrence (so a subject itself never contains the copula word).
/// Complement surfaces in `ctx` must not begin with a determiner word
/// (`"a "`/`"an "`/`"the "`) — the bare-plural path would misparse them;
/// today's vocabulary (single words and hyphenated compounds) satisfies
/// this.
///
/// Returns a spec whose `complement_concept` is the **concept id**, recovered
/// by matching the text against each candidate id's realized surface (its
/// Common word, pluralized for a plural clause) — the exact inverse of
/// [`realize_common`], which is why `parse_common(realize_common(s)) == s`
/// still holds now that the realizer resolves rather than echoes.
/// type-audit: bare-ok(prose)
pub fn parse_common(text: &str, ctx: &ParseContext) -> Result<ClauseSpec, ParseError> {
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
    // Modifier tail: '' or ' m1' or ' m1, m2, …'. The complement filter
    // above only admits candidates whose remainder is empty or starts
    // with ' ', so by construction `tail` is one of exactly those two
    // shapes — no third case exists to report.
    let tail = &after_det[surface.len()..];
    let modifiers: Vec<String> = match tail.strip_prefix(' ') {
        Some(t) => t.split(", ").map(str::to_string).collect(),
        None => Vec::new(),
    };
    Ok(ClauseSpec {
        frame: Frame::Classify,
        subject,
        complement_concept,
        number,
        definiteness,
        modifiers,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Common resolves its complement through the vocabulary, exactly as the
    /// tongue path resolves through a lexicon. Symmetry is the point: before
    /// this, the caller chose the word and no layer could ask whether the
    /// concept was sayable at all.
    #[test]
    fn common_resolves_its_complement_through_the_vocabulary() {
        let mut vocab = CommonVocabulary::default();
        vocab.declare("yellow-white-dwarf", "yellow-white dwarf (F)");
        let spec = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Elthandil".to_string()),
            complement_concept: "yellow-white-dwarf".to_string(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
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
        let spec = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("X".to_string()),
            complement_concept: "celestial-body".to_string(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
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
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Elthandil".into()),
            complement_concept: "planet".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
            "Elthandil is a planet."
        );
    }
    #[test]
    fn a_becomes_an_before_vowel() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Aoth".into()),
            complement_concept: "elemental".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
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
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("The Vavako".into()),
            complement_concept: "goblin-kind".into(),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
            "The Vavako are goblins."
        );
    }

    #[test]
    fn classify_with_modifier_tail() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Vebe".into()),
            complement_concept: "planet".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![
                "with two moons".into(),
                "orbiting a yellow-white dwarf".into(),
            ],
        };
        assert_eq!(
            realize_common(&s, &CommonVocabulary::default()),
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
        assert_eq!(inv[0].frame, Frame::Classify);
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
        let spec = parse_common(
            "Vebe is a planet with two moons, orbiting a yellow-white dwarf.",
            &ctx(&["planet"]),
        )
        .unwrap();
        assert_eq!(spec.subject, Subject::Name("Vebe".into()));
        assert_eq!(spec.complement_concept, "planet");
        assert_eq!(spec.number, Number::Sg);
        assert_eq!(spec.definiteness, Definiteness::Indef);
        assert_eq!(
            spec.modifiers,
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
        let spec = parse_common("The Vavako are goblins.", &ctx(&["goblin"])).unwrap();
        assert_eq!(spec.subject, Subject::Name("The Vavako".into()));
        assert_eq!(spec.complement_concept, "goblin");
        assert_eq!(spec.number, Number::Pl);
        assert_eq!(spec.definiteness, Definiteness::Indef);
        assert_eq!(spec.modifiers, Vec::<String>::new());
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

    // --- The round-trip property: parse_common(realize_common(s), ctx_from(s)) == Ok(s) ---

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
    fn ctx_from(spec: &ClauseSpec) -> ParseContext {
        let mut vocabulary = CommonVocabulary::default();
        let mut complements = std::collections::BTreeSet::new();
        complements.insert(spec.complement_concept.clone());
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
        let word = vocabulary.word_for(&spec.complement_concept);
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
        // 2 numbers x 2 definitenesses x 4 modifier-counts = 400 cases), so
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
        let modifier_pool = [
            "with two moons",
            "orbiting a yellow-white dwarf",
            "beneath ancient stars",
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
                        for modifier_count in 0..=3usize {
                            let modifiers: Vec<String> = modifier_pool[..modifier_count]
                                .iter()
                                .map(|m| (*m).to_string())
                                .collect();
                            let spec = ClauseSpec {
                                frame: Frame::Classify,
                                subject: subject.clone(),
                                complement_concept: complement.to_string(),
                                number,
                                definiteness,
                                modifiers,
                            };
                            let ctx = ctx_from(&spec);
                            let text = realize_common(&spec, &ctx.vocabulary);
                            assert_eq!(
                                parse_common(&text, &ctx),
                                Ok(spec.clone()),
                                "round-trip failed for {text:?}"
                            );
                            covered.insert((
                                subject_kind(&spec.subject),
                                complement_kind(&ctx.vocabulary.word_for(&spec.complement_concept)),
                                number_str(spec.number),
                                definiteness_str(spec.definiteness),
                                modifier_count,
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
        // x number x definiteness x modifier-count) combo was actually
        // emitted, not merely that the loop ran. 3 subject kinds x 3
        // complement kinds x 2 numbers x 2 definitenesses x 4 modifier
        // counts.
        let expected_combos = 3 * 3 * 2 * 2 * 4;
        assert_eq!(
            covered.len(),
            expected_combos,
            "generator did not cover every combo: {covered:?}"
        );
    }
}
