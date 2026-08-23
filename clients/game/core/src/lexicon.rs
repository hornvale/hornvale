//! The completion vocabulary: candidates, scopes, and their fold.

use crate::schema::Narration;

/// What a candidate is — mirrors the wire tags on session NounEntry.kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    /// A being.
    Creature,
    /// A location.
    Place,
    /// An object.
    Thing,
    /// Anything unrecognized.
    Unknown,
}

/// One completable name and its kind.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Candidate {
    /// The name as the prose mentions it.
    pub name: String,
    /// What kind of thing it is.
    pub category: Category,
}

/// One composable slice of the completion vocabulary. Deterministic, ordered;
/// registered = active.
pub trait CandidateSource {
    /// The candidates this scope offers, in a deterministic order.
    fn candidates(&self) -> Vec<Candidate>;
}

/// v1's only scope: the current turn's examinable catalog.
#[derive(Debug, Default, Clone)]
pub struct CurrentTurnNouns {
    /// The candidates drawn from the latest narration; empty until updated.
    nouns: Vec<Candidate>,
}

impl CurrentTurnNouns {
    /// Replace the contents from `narration.nouns`, leniently mapping each
    /// entry's kind string to a [`Category`].
    pub fn update(&mut self, narration: &Narration) {
        self.nouns = narration
            .nouns
            .iter()
            .map(|entry| Candidate {
                name: entry.noun.clone(),
                category: category_of(&entry.kind),
            })
            .collect();
    }
}

/// Lenient total mapping from a session `kind` string to [`Category`].
fn category_of(kind: &str) -> Category {
    match kind {
        "creature" => Category::Creature,
        "place" => Category::Place,
        "thing" => Category::Thing,
        _ => Category::Unknown,
    }
}

impl CandidateSource for CurrentTurnNouns {
    fn candidates(&self) -> Vec<Candidate> {
        self.nouns.clone()
    }
}

/// Composition root: ordered fold over registered scopes, first-wins dedup by name.
pub struct Lexicon {
    /// The registered scopes, in fold order.
    sources: Vec<Box<dyn CandidateSource>>,
}

impl Lexicon {
    /// Compose a lexicon from `sources`; earlier scopes win on name conflicts.
    pub fn new(sources: Vec<Box<dyn CandidateSource>>) -> Lexicon {
        Lexicon { sources }
    }

    /// Fold every scope in order, keeping the first candidate seen for a name.
    pub fn candidates(&self) -> Vec<Candidate> {
        let mut out: Vec<Candidate> = Vec::new();
        let mut seen: Vec<String> = Vec::new();
        for source in &self.sources {
            for candidate in source.candidates() {
                if !seen.contains(&candidate.name) {
                    seen.push(candidate.name.clone());
                    out.push(candidate);
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::NounEntry;

    /// A test-only fixed scope.
    struct StaticScope(Vec<Candidate>);

    impl CandidateSource for StaticScope {
        fn candidates(&self) -> Vec<Candidate> {
            self.0.clone()
        }
    }

    fn candidate(name: &str, category: Category) -> Candidate {
        Candidate {
            name: name.to_string(),
            category,
        }
    }

    #[test]
    fn the_lexicon_folds_scopes_first_wins() {
        let lexicon = Lexicon::new(vec![
            Box::new(StaticScope(vec![
                candidate("goblin", Category::Creature),
                candidate("key", Category::Thing),
            ])),
            Box::new(StaticScope(vec![
                candidate("goblin", Category::Thing),
                candidate("rug", Category::Thing),
            ])),
        ]);
        assert_eq!(
            lexicon.candidates(),
            vec![
                candidate("goblin", Category::Creature),
                candidate("key", Category::Thing),
                candidate("rug", Category::Thing),
            ]
        );
    }

    #[test]
    fn lexicon_with_no_scopes_is_empty() {
        assert!(Lexicon::new(vec![]).candidates().is_empty());
    }

    #[test]
    fn current_turn_nouns_starts_empty_then_reflects_the_latest_narration() {
        let mut scope = CurrentTurnNouns::default();
        assert!(scope.candidates().is_empty());
        let first = Narration {
            prose: String::new(),
            nouns: vec![NounEntry {
                noun: "goblin".into(),
                datum: "a goblin".into(),
                kind: "creature".into(),
            }],
        };
        scope.update(&first);
        assert_eq!(
            scope.candidates(),
            vec![candidate("goblin", Category::Creature)]
        );
        let second = Narration {
            prose: String::new(),
            nouns: vec![NounEntry {
                noun: "hearth".into(),
                datum: "a hearth".into(),
                kind: "place".into(),
            }],
        };
        scope.update(&second);
        assert_eq!(
            scope.candidates(),
            vec![candidate("hearth", Category::Place)]
        );
    }

    #[test]
    fn unknown_kind_strings_parse_leniently() {
        let narration = Narration {
            prose: String::new(),
            nouns: vec![NounEntry {
                noun: "ore".into(),
                datum: "some ore".into(),
                kind: "mineral".into(),
            }],
        };
        let mut scope = CurrentTurnNouns::default();
        scope.update(&narration);
        assert_eq!(
            scope.candidates(),
            vec![candidate("ore", Category::Unknown)]
        );
    }
}
