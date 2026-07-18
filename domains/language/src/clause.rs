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

/// Realize a ClauseSpec as a Common (≈ limited English) sentence.
/// type-audit: bare-ok(prose)
pub fn realize_common(spec: &ClauseSpec) -> String {
    match spec.frame {
        Frame::Classify => {
            let subject = match &spec.subject {
                Subject::Name(name) => name.as_str(),
                Subject::Pronoun(pronoun) => pronoun,
            };
            let copula = match spec.number {
                Number::Sg => "is",
                Number::Pl => "are",
            };
            let det = match (spec.definiteness, spec.number) {
                (Definiteness::Def, _) => "the ".to_string(),
                (Definiteness::Indef, Number::Sg) => {
                    format!("{} ", indefinite_article(&spec.complement))
                }
                (Definiteness::Indef, Number::Pl) => String::new(), // bare generic
            };
            let mut head = format!("{det}{}", spec.complement);
            for (i, modifier) in spec.modifiers.iter().enumerate() {
                if i == 0 {
                    head.push(' ');
                } else {
                    head.push_str(", ");
                }
                head.push_str(modifier);
            }
            format!("{subject} {copula} {head}.")
        }
    }
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
}
