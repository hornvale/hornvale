//! The clause layer: a language-neutral ClauseSpec and the Common realizer.
//! Generalizes the render_line seam from a bespoke tenet spec to any clause.
#![allow(clippy::module_name_repetitions)]

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
/// type-audit: bare-ok(identifier-text: complement), bare-ok(prose: modifiers)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClauseSpec {
    /// The construction.
    pub frame: Frame,
    /// The subject: a resolved name, or a pronoun for re-mention.
    pub subject: Subject,
    /// The complement concept's Common lexeme.
    pub complement: String,
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

/// Realize a ClauseSpec as a Common (≈ limited English) sentence.
/// type-audit: bare-ok(prose)
pub fn realize_common(spec: &ClauseSpec) -> String {
    let construction = common_constructions()
        .iter()
        .find(|c| c.frame == spec.frame)
        .expect("every Frame has a construction");
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
                    out.push_str(indefinite_article(&spec.complement));
                    out.push(' ');
                }
                (Definiteness::Indef, Number::Pl) => {} // bare generic
            },
            Part::Complement => out.push_str(&spec.complement),
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

/// The closed complement lexeme set a parse call recognizes, e.g. every
/// concept name registered for the calling culture/world. Longest-match
/// wins when one complement is a prefix of another (`"dwarf"` vs.
/// `"yellow-white dwarf"`).
/// type-audit: bare-ok(identifier-text: complements)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseContext {
    /// The recognized complement lexemes, e.g. `"planet"`,
    /// `"yellow-white dwarf"`.
    pub complements: std::collections::BTreeSet<String>,
}

/// Why `parse_common` refused to invert a sentence — each variant is a
/// recountable, specific reason rather than a bare "parse failed".
/// type-audit: bare-ok(prose: UnknownComplement.after), bare-ok(prose: BadTail.tail)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    /// Neither `" is "` nor `" are "` appears, so no subject/copula split
    /// exists.
    NoCopula,
    /// The text after the determiner doesn't match (a prefix of) any
    /// complement in the caller's `ParseContext`.
    UnknownComplement {
        /// The unrecognized text following the determiner.
        after: String,
    },
    /// Text follows the matched complement that is neither empty nor a
    /// `' '`-prefixed modifier tail.
    BadTail {
        /// The offending trailing text.
        tail: String,
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
            ParseError::BadTail { tail } => {
                write!(f, "trailing text '{tail}' is not a valid modifier tail")
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
    // Complement: longest match from the closed set.
    let complement = ctx
        .complements
        .iter()
        .filter(|c| {
            after_det == c.as_str()
                || after_det
                    .strip_prefix(c.as_str())
                    .is_some_and(|r| r.starts_with(' '))
        })
        .max_by_key(|c| c.len())
        .cloned()
        .ok_or_else(|| ParseError::UnknownComplement {
            after: after_det.to_string(),
        })?;
    // Modifier tail: '' or ' m1' or ' m1, m2, …'.
    let tail = &after_det[complement.len()..];
    let modifiers: Vec<String> = if tail.is_empty() {
        Vec::new()
    } else if let Some(t) = tail.strip_prefix(' ') {
        t.split(", ").map(str::to_string).collect()
    } else {
        return Err(ParseError::BadTail {
            tail: tail.to_string(),
        });
    };
    Ok(ClauseSpec {
        frame: Frame::Classify,
        subject,
        complement,
        number,
        definiteness,
        modifiers,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_singular_indefinite() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Elthandil".into()),
            complement: "planet".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
        };
        assert_eq!(realize_common(&s), "Elthandil is a planet.");
    }
    #[test]
    fn a_becomes_an_before_vowel() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Aoth".into()),
            complement: "elemental".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
        };
        assert_eq!(realize_common(&s), "Aoth is an elemental.");
    }
    #[test]
    fn classify_generic_plural() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Goblins".into()),
            complement: "people".into(),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            modifiers: vec![],
        };
        assert_eq!(realize_common(&s), "Goblins are people.");
    }

    #[test]
    fn classify_with_modifier_tail() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: Subject::Name("Vebe".into()),
            complement: "planet".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers: vec![
                "with two moons".into(),
                "orbiting a yellow-white dwarf".into(),
            ],
        };
        assert_eq!(
            realize_common(&s),
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

    fn ctx(words: &[&str]) -> ParseContext {
        ParseContext {
            complements: words.iter().map(|w| (*w).to_string()).collect(),
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
        assert_eq!(spec.complement, "planet");
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

    #[test]
    fn parse_inverts_the_plural_generic() {
        let spec = parse_common("The Vavako are goblins.", &ctx(&["goblins"])).unwrap();
        assert_eq!(spec.subject, Subject::Name("The Vavako".into()));
        assert_eq!(spec.number, Number::Pl);
        assert_eq!(spec.definiteness, Definiteness::Indef);
        assert_eq!(spec.modifiers, Vec::<String>::new());
    }

    #[test]
    fn parse_reports_a_recountable_failure() {
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

    // --- The round-trip property: parse_common(realize_common(s), ctx_from(s)) == Ok(s) ---

    /// Classify a subject into the coverage axis the property test tracks.
    fn subject_kind(s: &Subject) -> &'static str {
        match s {
            Subject::Pronoun(_) => "pronoun",
            Subject::Name(n) if n.contains(' ') => "multi-word-name",
            Subject::Name(_) => "single-word-name",
        }
    }

    /// Classify a complement lexeme into the coverage axis the property
    /// test tracks. Multi-word wins over vowel-initial so a phrase like
    /// "ancient artifact" (both) still counts toward multi-word coverage;
    /// "elemental" alone covers vowel-initial.
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
    /// `parse_common`: the spec's own complement, plus decoys that probe
    /// longest-match — other legal complements from the same closed
    /// vocabulary, the first word of a multi-word complement (a genuine
    /// prefix that must lose to the full phrase), and a same-phrase-minus-
    /// one-character truncation (must NOT match at all: the boundary check
    /// requires the character after a matched prefix to be a space).
    fn ctx_from(spec: &ClauseSpec) -> ParseContext {
        let mut complements = std::collections::BTreeSet::new();
        complements.insert(spec.complement.clone());
        // Stock decoys: other legal complements from the closed vocabulary,
        // always present as noise the true complement must outrank.
        for stock in [
            "planet",
            "goblins",
            "elemental",
            "yellow-white dwarf",
            "ancient artifact",
            "dwarf",
        ] {
            complements.insert(stock.to_string());
        }
        // Prefix-of-longer probe: the first word of a multi-word complement.
        if let Some((first, _)) = spec.complement.split_once(' ') {
            complements.insert(first.to_string());
        }
        // Must-not-match probe: one character short of the real complement.
        if spec.complement.len() > 1 {
            let mut truncated = spec.complement.clone();
            truncated.pop();
            complements.insert(truncated);
        }
        ParseContext { complements }
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
        let complements = [
            "planet",             // consonant-initial
            "goblins",            // consonant-initial
            "elemental",          // vowel-initial
            "yellow-white dwarf", // multi-word, hyphenated first word
            "ancient artifact",   // multi-word AND vowel-initial
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
                                complement: complement.to_string(),
                                number,
                                definiteness,
                                modifiers,
                            };
                            let text = realize_common(&spec);
                            let ctx = ctx_from(&spec);
                            assert_eq!(
                                parse_common(&text, &ctx),
                                Ok(spec.clone()),
                                "round-trip failed for {text:?}"
                            );
                            covered.insert((
                                subject_kind(&spec.subject),
                                complement_kind(&spec.complement),
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
