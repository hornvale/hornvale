//! The completion vocabulary: candidates, scopes, their fold, and the
//! prefix-matching completion engine.

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

/// The result of one completion attempt against a candidate list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Completion {
    /// Nothing matched (or the prefix was empty): a no-op.
    None,
    /// Exactly one candidate matched: fill it whole. Note that a single match
    /// equal to the prefix exactly is still `Unique` — the engine never
    /// suppresses it; the caller treats that case as a no-op.
    Unique(String),
    /// Several matched: extend to the shared stem and offer the rest.
    Prefix {
        /// The longest common prefix of ALL matches, in the candidates' own
        /// casing, computed char-wise.
        stem: String,
        /// Every matching name, input order preserved.
        matches: Vec<String>,
    },
}

/// Attempt one completion of `prefix` against `candidates`.
///
/// Matching is case-insensitive (`starts_with` on lowercased forms), but
/// every returned name and stem preserves the candidate's own casing. An
/// empty prefix or zero matches yields [`Completion::None`].
pub fn complete(prefix: &str, candidates: &[Candidate]) -> Completion {
    if prefix.is_empty() {
        return Completion::None;
    }
    let lower = prefix.to_lowercase();
    let matches: Vec<&str> = candidates
        .iter()
        .filter(|c| c.name.to_lowercase().starts_with(&lower))
        .map(|c| c.name.as_str())
        .collect();
    match matches.len() {
        0 => Completion::None,
        1 => Completion::Unique(matches[0].to_string()),
        _ => {
            let mut stem: Vec<char> = matches[0].chars().collect();
            for name in &matches[1..] {
                stem = stem
                    .into_iter()
                    .zip(name.chars())
                    .take_while(|(a, b)| a.to_lowercase().eq(b.to_lowercase()))
                    .map(|(a, _)| a)
                    .collect();
            }
            Completion::Prefix {
                stem: stem.into_iter().collect(),
                matches: matches.into_iter().map(str::to_string).collect(),
            }
        }
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

    #[test]
    fn unique_prefix_completes() {
        let candidates = vec![candidate("goblin", Category::Creature)];
        assert_eq!(
            complete("gob", &candidates),
            Completion::Unique("goblin".to_string())
        );
    }

    #[test]
    fn ambiguous_prefix_yields_stem_and_matches() {
        let candidates = vec![
            candidate("Vngashngatva", Category::Creature),
            candidate("Vngashngakelm", Category::Creature),
        ];
        assert_eq!(
            complete("vng", &candidates),
            Completion::Prefix {
                stem: "Vngashnga".to_string(),
                matches: vec!["Vngashngatva".to_string(), "Vngashngakelm".to_string()],
            }
        );
    }

    #[test]
    fn case_insensitive_match_preserves_candidate_casing() {
        let candidates = vec![candidate("Goblin", Category::Creature)];
        assert_eq!(
            complete("GOB", &candidates),
            Completion::Unique("Goblin".to_string())
        );
        let two = vec![
            candidate("Hearth", Category::Place),
            candidate("hearthstone", Category::Thing),
        ];
        assert_eq!(
            complete("HEA", &two),
            Completion::Prefix {
                stem: "Hearth".to_string(),
                matches: vec!["Hearth".to_string(), "hearthstone".to_string()],
            }
        );
    }

    #[test]
    fn no_match_is_none_and_empty_prefix_is_none() {
        let candidates = vec![candidate("goblin", Category::Creature)];
        assert_eq!(complete("zzz", &candidates), Completion::None);
        assert_eq!(complete("", &candidates), Completion::None);
        assert_eq!(complete("gob", &[]), Completion::None);
    }

    #[test]
    fn stem_is_computed_char_wise_not_byte_wise() {
        let candidates = vec![
            candidate("élan", Category::Thing),
            candidate("éclat", Category::Thing),
        ];
        assert_eq!(
            complete("é", &candidates),
            Completion::Prefix {
                stem: "é".to_string(),
                matches: vec!["élan".to_string(), "éclat".to_string()],
            }
        );
    }

    #[test]
    fn exact_single_match_still_reports_unique() {
        // A single match equal to the prefix is still Unique; the CALLER
        // treats it as a no-op — the engine itself never suppresses it.
        let candidates = vec![candidate("goblin", Category::Creature)];
        assert_eq!(
            complete("goblin", &candidates),
            Completion::Unique("goblin".to_string())
        );
    }
}
