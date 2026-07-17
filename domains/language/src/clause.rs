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

/// A language-neutral clause: predicate-argument structure plus features.
/// The per-language realizer decides how (and whether) each feature surfaces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClauseSpec {
    /// The construction.
    pub frame: Frame,
    /// The subject's surface form (already a resolved name/noun).
    pub subject: String,
    /// The complement concept's Common lexeme.
    pub complement: String,
    /// Subject number.
    pub number: Number,
    /// Complement definiteness.
    pub definiteness: Definiteness,
}

fn indefinite_article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

/// Realize a ClauseSpec as a Common (≈ limited English) sentence.
pub fn realize_common(spec: &ClauseSpec) -> String {
    match spec.frame {
        Frame::Classify => {
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
            format!("{} {} {}{}.", spec.subject, copula, det, spec.complement)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_singular_indefinite() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: "Elthandil".into(),
            complement: "planet".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
        };
        assert_eq!(realize_common(&s), "Elthandil is a planet.");
    }
    #[test]
    fn a_becomes_an_before_vowel() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: "Aoth".into(),
            complement: "elemental".into(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
        };
        assert_eq!(realize_common(&s), "Aoth is an elemental.");
    }
    #[test]
    fn classify_generic_plural() {
        let s = ClauseSpec {
            frame: Frame::Classify,
            subject: "Goblins".into(),
            complement: "people".into(),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
        };
        assert_eq!(realize_common(&s), "Goblins are people.");
    }
}
