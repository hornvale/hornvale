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
        assert!(matches!(inv[0].parts.first(), Some(Part::Subject)));
    }
}
