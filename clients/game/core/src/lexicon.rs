//! The completion vocabulary: candidates, scopes, their fold, and the
//! prefix-matching completion engine.
//!
//! ## Creature identity (The Legend, Task 9)
//!
//! [`creature_glyph`] is the derivation the coverage audit's 2.1 item asked
//! for: a creature draws its own noun's initial rather than the generic mark
//! every other referent on the walk band and floor plan already draws by
//! ordinal or by terrain. There is no authored species table — none could
//! stay current (The Radiation moved the species roster from nine peoples to
//! fifteen the day before this task was written) — so the rule is derived
//! fresh from whatever noun a render actually carries.
//!
//! ## Stable letters, collisions tolerated (fix round 2)
//!
//! The first version of this rule ranked nouns by RENDER — every
//! `"agent"`-kind mark visible in the same chart or plan competed for the
//! codespace, and a noun losing that competition walked to a different one
//! of its own letters. That made the same species draw differently turn to
//! turn, purely as a function of who else happened to be in view: a goblin
//! alone drew `g`, but a goblin standing next to a gargoyle drew `o`,
//! because the gargoyle sorted first and took `g`. Decision 0389 admits a
//! glyph on the ORDER clause or the IDENTITY clause; a letter that changes
//! per frame is neither — it is a per-frame slot number, the nominal
//! category 0389 forbids, and a slot number needs a legend to read, which
//! defeats the entire point of drawing a creature as its own initial.
//!
//! So [`creature_glyph`] is now a pure function of the noun alone: a goblin
//! is always `g`, and so is a gnoll — the two are ambiguous on the grid, and
//! the cursor (which names the noun outright) is what disambiguates them,
//! not the glyph. This is ADoM's own convention, and it is the division of
//! labour the client already has: the glyph carries identity at a glance,
//! the cursor carries the detail. **Do not restore per-render ranking to
//! chase uniqueness** — that is the exact property this fix round removed on
//! Nathan's explicit instruction, and restoring it reopens the per-frame-slot
//! defect this doc section exists to document.

use crate::schema::{Narration, Spatial};

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

/// v2's second scope (The Newel, Task 4): settlement and cave marks drawn
/// on the walk-band chart, so completion sees names the map shows and the
/// current turn's narration does not (a distant settlement, or an agent
/// several rooms away).
///
/// **Gated by discovery, and the gate lives outside this crate.** Decision
/// 0670: a placed site's glyph draws whether or not it has been
/// discovered, but its proper NAME is withheld until then, and completion
/// is a name surface, so it must take the same gate. This crate carries no
/// `hornvale_kernel` dependency and no discovery ledger at all — `lib.rs`'s
/// own doc: "no hornvale dependency, by design" — so it cannot itself turn
/// a mark's packed room id into a `Vertex` or ask whether that vertex's
/// site has been discovered. [`ChartMarks::update`] asks its caller
/// instead: `is_discovered(kind, room)` is handed each mark's own `kind`
/// string and the packed id of the room it stands on, and the caller
/// (the driver, which owns `NearestVertexIndex`/`Geosphere` and the
/// discovery ledger) answers with whatever machinery it has. This scope
/// only asks and filters.
#[derive(Debug, Default, Clone)]
pub struct ChartMarks {
    /// The candidates drawn from the latest walk-band chart; empty until
    /// updated, and cleared on any non-walk band.
    marks: Vec<Candidate>,
}

impl ChartMarks {
    /// Replace the contents from `spatial`'s walk-band chart (a non-walk
    /// band clears to empty).
    ///
    /// **`"agent"` is the ONE exemption, and everything else consults
    /// `is_discovered`.** Decision 0670 covers a placed site's proper name;
    /// an `"agent"` mark is a live creature, never a placed site, and gating
    /// it would withhold something the decision never asked to withhold — so
    /// `is_discovered` is not called for one, the same way
    /// `resolve_chain_at`'s own discovery closure is only ever asked about a
    /// feature that participates in the chain at all.
    ///
    /// **DEFAULT-DENY, and it used to be the other way round** (fix wave).
    /// The match named `"settlement" | "cave"` as the gated arms and let `_`
    /// through ungated, which leaked nothing at the time — the producer
    /// emits exactly those two kinds plus `"agent"` — but
    /// `windows/scene/src/surrounds.rs` explicitly anticipates further
    /// kinds, and a new placed-site kind added there would have reached
    /// completion ungated with nothing objecting. A gate whose safe
    /// behaviour depends on a producer one crate boundary away staying
    /// still is not a gate. Inverted, this repo's own default-deny posture
    /// (type-audit, placement-audit, plumb) now holds here too: an
    /// unrecognised kind is withheld until discovered, and admitting a new
    /// ungated kind is a deliberate edit to this list.
    pub fn update(&mut self, spatial: &Spatial, is_discovered: impl Fn(&str, u64) -> bool) {
        // A `&` reference is `Copy`, so the closure below can be re-borrowed
        // once per room (`flat_map`'s own `FnMut`) without requiring
        // `is_discovered` itself to implement `Clone`.
        let is_discovered = &is_discovered;
        self.marks = match spatial {
            Spatial::Walk { chart } => chart
                .cells // lexicon: `Chart::cells` is the wire's own frozen field name for the chart's rooms — an area, not a vertex
                .iter()
                .flat_map(|c| {
                    let room = c.room;
                    c.marks.iter().filter_map(move |m| {
                        let discovered = match m.kind.as_str() {
                            "agent" => true,
                            _ => is_discovered(&m.kind, room),
                        };
                        discovered.then(|| Candidate {
                            name: m.noun.clone(),
                            category: category_of_mark(&m.kind),
                        })
                    })
                })
                .collect(),
            Spatial::Chamber { .. } | Spatial::Underground { .. } => Vec::new(),
        };
    }
}

/// Lenient mapping from a chart mark's `kind` string to [`Category`].
/// `"settlement"` and `"cave"` are both places; `"agent"` (a session-owning
/// consumer's own addition — see `schema.rs`'s own doc on [`crate::Mark`])
/// is a creature.
fn category_of_mark(kind: &str) -> Category {
    match kind {
        "settlement" | "cave" => Category::Place,
        "agent" => Category::Creature,
        _ => Category::Unknown,
    }
}

impl CandidateSource for ChartMarks {
    fn candidates(&self) -> Vec<Candidate> {
        self.marks.clone()
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

/// The glyph a creature draws, by IDENTITY: the character spells the
/// thing, which is why it needs no legend (spec's Ruling on `Creature`'s
/// codespace — see `register.rs`'s own doc: "a creature draws its noun's
/// initial, so its codespace is `a-z`/`A-Z` by RULE rather than by row").
///
/// **A pure function of the noun alone (fix round 2).** Always the noun's
/// own first ASCII-alphabetic character, lowercased: a goblin is always
/// `'g'`, a gnoll is always `'g'` too — same-initial species draw
/// identically, and the cursor (which names the noun outright) is what
/// disambiguates them on the grid, not the glyph. This was a two-argument,
/// render-scoped rank function through fix round 1; see this module's doc
/// section "Stable letters, collisions tolerated" for why that was wrong
/// (a letter that changes with who else is in view is a per-frame slot
/// number, not identity) and must not be restored.
///
/// Infallible by design (a render must not panic on a document that
/// parsed, the same discipline `chart.rs`'s `weight_of` already follows):
/// a noun with no ASCII-alphabetic character at all (an unromanized name)
/// still draws something non-whitespace — a fixed `'a'` fallback that, like
/// every other output of this function, depends on nothing but the noun
/// itself.
pub fn creature_glyph(noun: &str) -> char {
    noun.chars()
        .find(char::is_ascii_alphabetic)
        .map(|c| c.to_ascii_lowercase())
        .unwrap_or('a')
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
