//! This file resolves three corpora, not one, and they are not frozen the
//! same way. `the-merchant.corpus.json` and `the-flood-watch.corpus.json`
//! are frozen: each entry count is asserted, so growing or trimming either
//! is a deliberate act rather than a drift. `the-ladder.corpus.json.DRAFT`
//! is deliberately NOT frozen — no count is asserted for it, only
//! structural properties over its `presupposes` graph (see Task 4's block
//! below) — because freezing is the project owner's act and the moment it
//! happens the ladder's rung ids become append-only forever.
//!
//! This file also carries the corpus's **resolver**: for each entry, a demand
//! token is `covered` if the grammar implements a construction for it, `not
//! yet` otherwise. The Interlinear left exactly one token covered
//! (`classify`, the "X is a Y" construction domains/language realizes through
//! the fact-shaped `Clause`); The Inquest added four more — `past-tense`,
//! `negation`, `transitive-frame` and `pronoun-reference` — taking the
//! merchant corpus from 0 of 12 to 2 of 12; The Mortise added three more —
//! `coordination`, `embedded-clause` and `epistemic-hedge` — taking it from
//! 2 of 12 to 5 of 12. The remaining tokens name capabilities no campaign
//! has built: questions, temporal adjuncts, existentials, witness lists,
//! named-entity lists.
//!
//! **[`IMPLEMENTED_DEMANDS`] is a hand-maintained declaration and nothing
//! mechanically proves it.** No test crosses a token in that list against the
//! construction that is supposed to realize it, so a token added on optimism
//! moves the score without moving the grammar — which would make the
//! instrument worse than no instrument, because it would read as evidence.
//! The discipline is therefore social and stated here: a token goes in only
//! alongside a test in `domains/language` that realizes a clause exercising
//! it, in Common and, where the tongue realizer is the point, in a tongue.
//! The eight present tokens are backed by, respectively:
//! `clause.rs::classify_*`; `a_past_clause_says_was` and
//! `grammar.rs::realize_tongue_reads_its_drawn_tense_depth`;
//! `a_negated_clause_says_is_not` and
//! `grammar.rs::realize_tongue_reads_its_drawn_polarity_depth`;
//! `a_transitive_clause_surfaces_its_predicate_as_a_verb` and
//! `grammar.rs::realize_tongue_exhaustive_orders_for_a_transitive_clause`;
//! `common_realizes_a_pronoun_subject_and_a_pronoun_object` and
//! `grammar.rs::a_tongue_realizes_a_pronoun_subject_from_its_own_drawn_inventory`;
//! `a_shared_subject_is_stated_once` and
//! `grammar.rs::a_tongue_elides_a_shared_subject` (`coordination`);
//! `a_clause_object_realizes_with_no_determiner` and
//! `grammar.rs::a_tongue_with_a_complementizer_marks_the_embedded_boundary`
//! (`embedded-clause`); `a_hedge_clause_surfaces_think_as_a_verb`
//! (`epistemic-hedge` — Common-only, the same posture `classify` takes: no
//! tongue realizer is the point of this token, so no tongue test is named).
//!
//! **The score was zero once, and the positive control that answered that is
//! still here on purpose.** A measurement whose only possible answer is zero
//! cannot tell a working resolver from one hardcoded to return "not yet"
//! unconditionally — the same defect class as a guard that has never gone
//! red. That specific blind spot closed when the score moved off zero: a
//! resolver stuck at "not yet" now fails
//! [`merchant_coverage_is_five_of_twelve`] directly. The control keeps its
//! place because it guards a *different* thing from the headline test — it
//! is the only assertion here that survives the corpus being replaced, and
//! the corpus is frozen data a future campaign is expected to swap. See its
//! own doc comment.
//!
//! **This file also carries the ladder/corpus cross-check (The Stile,
//! Task 5).** The ladder (`the-ladder.corpus.json.DRAFT`) and
//! `the-flood-watch.corpus.json` were authored independently, neither
//! author seeing the other's work; the cross-check between them already
//! moved the ladder once (56 tokens flood-watch needed did not exist on
//! its first draft) and is now a standing test —
//! [`the_ladder_covers_flood_watch_vocabulary_except_the_two_refused_input_surface_tokens`]
//! — rather than a one-time analysis. Its ceiling is 147 of 149, not 149:
//! see [`FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER`] for the two tokens
//! that are refused rather than missing.
//! [`the_ladder_covers_the_merchant_corpus_vocabulary_completely`] pins the
//! sibling fact (13 of 13) for the older, recorded corpus.
//! [`removing_a_rungs_introduces_token_makes_the_cross_check_notice`] proves
//! the cross-check is sensitive to a token going missing, not merely to a
//! count.

use hornvale_kernel::ConceptRegistry;
use hornvale_language::packs::{KILL, KNOW, THINK};
use hornvale_language::{
    Argument, Clause, CommonVocabulary, Coordination, Definiteness, Evidential, Number, Person,
    Polarity, Subject, Tense, realize_common, realize_common_coordination,
};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The merchant corpus's frozen entry count. Changing this number is the
/// deliberate act; changing the corpus without it is the drift.
const MERCHANT_ENTRIES: usize = 12;

#[test]
fn the_merchant_corpus_is_frozen_at_its_authored_size() {
    let text = std::fs::read_to_string(repo_root().join("sentences/the-merchant.corpus.json"))
        .expect("the merchant corpus is committed");
    let n = text.matches("\"id\":").count();
    assert_eq!(
        n, MERCHANT_ENTRIES,
        "the corpus moved. If that was deliberate, change MERCHANT_ENTRIES in \
         the same commit and say why in the message; a corpus that drifts \
         under a measurement makes every earlier score incomparable."
    );
}

/// The flood-watch corpus's frozen entry count: 139 utterances across five
/// scenes of investigative dialogue, 68 player lines and 71 NPC lines.
const FLOOD_WATCH_ENTRIES: usize = 139;

/// The flood-watch corpus is frozen the same way the merchant corpus is.
/// **It is not true that nothing else in this file touches it** — four
/// other tests do:
/// [`the_flood_watch_direction_split_is_sixty_eight_parse_seventy_one_produce`],
/// [`the_ladder_covers_flood_watch_vocabulary_except_the_two_refused_input_surface_tokens`],
/// [`removing_a_rungs_introduces_token_makes_the_cross_check_notice`] and
/// [`sentence_coverage_report`]. None of them scores it for coverage. The
/// corpus carries one field the resolver's [`Entry`] does not have —
/// `scene` — plus a per-entry `direction` that [`Entry`] already reads. Its
/// entry shape shipped; wiring a *coverage* resolver over it is not blocked
/// on a schema change (there is none outstanding — see
/// `sentences/README.md`, "Frozen is not the same as measured"). What is
/// actually absent is a coverage resolver over the two new corpora, which
/// is a different and larger question: what "covered" should mean for a
/// corpus the grammar was never built toward, decided before a number
/// exists to chase. So the data is frozen now, before any score exists to
/// be chased (decision 0016), and the measurement is left alone.
///
/// Its own `minted_tokens` block records the 56 demand tokens it needed that
/// `the-ladder.corpus.json.DRAFT` does not name — in the corpus itself rather
/// than in a campaign scratch file, so the vocabulary survives the worktree
/// that authored it.
#[test]
fn the_flood_watch_corpus_is_frozen_at_its_authored_size() {
    let text = std::fs::read_to_string(repo_root().join("sentences/the-flood-watch.corpus.json"))
        .expect("the flood-watch corpus is committed");
    let n = text.matches("\"id\":").count();
    assert_eq!(
        n, FLOOD_WATCH_ENTRIES,
        "the corpus moved. If that was deliberate, change FLOOD_WATCH_ENTRIES \
         in the same commit and say why in the message; a corpus that drifts \
         under a measurement makes every earlier score incomparable."
    );
}

// ---------------------------------------------------------------------
// Task 9: the resolver
// ---------------------------------------------------------------------

/// Demand tokens the grammar implements a construction for. Extend this list
/// only when a token genuinely gains a construction, in the same commit that
/// moves [`MERCHANT_COVERED`] — the same discipline [`MERCHANT_ENTRIES`]
/// enforces on the corpus itself.
///
/// **Nothing mechanically checks a row here against the grammar**; see the
/// module doc, which names the test backing each of the eight.
const IMPLEMENTED_DEMANDS: &[&str] = &[
    // The Interlinear.
    "classify",
    // The Inquest.
    "negation",
    "past-tense",
    "pronoun-reference",
    "transitive-frame",
    // The Mortise.
    "coordination",
    "embedded-clause",
    "epistemic-hedge",
];

/// Whether a corpus entry states what the grammar must **parse** (a player
/// line, read from the world) or **produce** (an NPC line, or any of the
/// ladder's rungs). `None` when the corpus states nothing at all — see
/// [`read_declared`] and [`read_derived`]'s own docs for which corpora leave
/// it unset, and Task 3's discussion for why an absent direction is never
/// inferred from `speaker` or any other field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Direction {
    /// The grammar must parse this text.
    Parse,
    /// The grammar must produce this text.
    Produce,
}

/// One corpus entry, normalized to a single shape regardless of which of
/// the three corpora produced it. A `the-merchant`/`the-flood-watch` entry
/// gets its `demands` [`read_declared`] — straight off the corpus's own
/// `demands` field; a `the-ladder` entry has no such field on disk and gets
/// its `demands` [`read_derived`] instead, as the transitive closure of its
/// `presupposes` chain. Both readers produce this same type so every other
/// function here — `demand_covered`, `entry_covered`, `missing_demands`, the
/// witness, the report — is written once and works for any corpus.
struct Entry {
    /// The entry's corpus-assigned id (`"m05"`, `"fw042"`, `"r006"`, …).
    id: String,
    /// Who utters the entry, when the corpus states one. The ladder's rungs
    /// have no speaker at all — they are a production instrument, not
    /// dialogue — so this is optional even though every entry in the two
    /// declared corpora carries one.
    speaker: Option<String>,
    /// The entry's authored English.
    text: String,
    /// The demand tokens this entry makes on the grammar. Declared
    /// (verbatim, in the corpus's own order) for `the-merchant` and
    /// `the-flood-watch`; derived (the transitive closure of `presupposes`,
    /// collected into sorted order — there is no "authored order" for a
    /// computed set to preserve) for `the-ladder`.
    demands: Vec<String>,
    /// This entry's [`Direction`], or `None` when the corpus states
    /// nothing.
    direction: Option<Direction>,
}

/// The corpus document's shape: only what the resolver reads.
struct Corpus {
    entries: Vec<Entry>,
}

/// The on-disk shape `the-merchant.corpus.json` and
/// `the-flood-watch.corpus.json` share: `demands` and (where stated)
/// `direction` are read exactly as authored. Unrecognized JSON fields
/// (`scene`, `note`, `name`, `provenance` at the document root, …) are
/// ignored by default. See [`LadderEntryJson`] for the opposite shape.
///
/// **`speaker` and `demands` are REQUIRED, not defaulted.** Both frozen
/// corpora carry both on every entry today (re-scanned: 0 of 151 missing
/// either), so a default would buy real data nothing — it would only let a
/// future malformed edit to a frozen corpus deserialize silently as
/// `None`/`[]` instead of panicking loudly at load, which is a regression
/// against this project's fail-fast standard. `direction` is the one field
/// that keeps its default: `the-merchant.corpus.json` genuinely has no
/// `direction` key on any entry, so its absence is a real fact about that
/// corpus (spec §2.3 — never inferred), not a defaulted requirement.
#[derive(serde::Deserialize)]
struct DeclaredEntryJson {
    id: String,
    speaker: String,
    text: String,
    demands: Vec<String>,
    #[serde(default)]
    direction: Option<String>,
}

/// The declared-shape document: only what [`read_declared`] needs.
#[derive(serde::Deserialize)]
struct DeclaredCorpusJson {
    entries: Vec<DeclaredEntryJson>,
}

/// Parse a raw `"direction"` JSON string into a [`Direction`], or `None`
/// when the field was absent (`the-merchant` states nothing — see the
/// module's direction discussion). Panics loudly on any value that is
/// neither `"parse"` nor `"produce"`, the same fail-fast posture every
/// other loader here takes on malformed data.
fn parse_direction(raw: Option<&str>) -> Option<Direction> {
    match raw {
        None => None,
        Some("parse") => Some(Direction::Parse),
        Some("produce") => Some(Direction::Produce),
        Some(other) => panic!("unknown direction {other:?}; expected \"parse\" or \"produce\""),
    }
}

/// Read a declared-shape corpus (`the-merchant`, `the-flood-watch`):
/// `demands` is read as authored, never derived — see [`read_derived`] for
/// the ladder's opposite reader. **A corpus file carries one shape or the
/// other, never both**; writing a derived `demands` list back into the
/// ladder would state the same fact twice, the duplicated-rule shape whose
/// cheapest repair is to delete the check.
fn read_declared(path: &Path) -> Vec<Entry> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("{} is committed: {e}", path.display()));
    let doc: DeclaredCorpusJson = serde_json::from_str(&text)
        .unwrap_or_else(|e| panic!("{} parses as the declared shape: {e}", path.display()));
    doc.entries
        .into_iter()
        .map(|e| Entry {
            id: e.id,
            speaker: Some(e.speaker),
            text: e.text,
            demands: e.demands,
            direction: parse_direction(e.direction.as_deref()),
        })
        .collect()
}

/// The ladder's on-disk shape (`the-ladder.corpus.json.DRAFT`): a rung
/// declares the ONE token it introduces (`null` for none) and the rungs it
/// presupposes; it carries no `demands` field and no `speaker` at all. See
/// [`DeclaredEntryJson`] for the opposite shape.
#[derive(serde::Deserialize)]
struct LadderEntryJson {
    id: String,
    text: String,
    #[serde(default)]
    introduces: Option<String>,
    #[serde(default)]
    presupposes: Vec<String>,
}

/// The ladder document's shape: only what [`read_derived`] needs.
#[derive(serde::Deserialize)]
struct LadderCorpusJson {
    entries: Vec<LadderEntryJson>,
}

/// The transitive closure of `presupposes` for one rung, collecting every
/// reached rung's `introduces` token (a rung whose `introduces` is `null`
/// contributes nothing itself but its presuppositions are still walked).
/// Returned in sorted order via a [`BTreeSet`] — a derived set has no
/// "authored order" to preserve, and the workspace bans `HashSet`.
///
/// **This is the whole feature, not a helper.** A one-level reader — the
/// union of only `id`'s direct presuppositions' own `introduces`, without
/// continuing past them — produces a plausible, smaller set that looks
/// fine; Task 2 pins a rung where the two answers differ.
fn derived_demands(id: &str, by_id: &BTreeMap<&str, &LadderEntryJson>) -> Vec<String> {
    let mut visited: BTreeSet<&str> = BTreeSet::new();
    let mut demands: BTreeSet<String> = BTreeSet::new();
    let mut stack = vec![id];
    while let Some(rung_id) = stack.pop() {
        if !visited.insert(rung_id) {
            continue;
        }
        let rung = by_id
            .get(rung_id)
            .unwrap_or_else(|| panic!("presupposes references unknown rung {rung_id:?}"));
        if let Some(token) = &rung.introduces {
            demands.insert(token.clone());
        }
        stack.extend(rung.presupposes.iter().map(String::as_str));
    }
    demands.into_iter().collect()
}

/// Read the ladder (`the-ladder.corpus.json.DRAFT`): each rung's `demands`
/// is [`derived_demands`] — the transitive closure of `presupposes` — never
/// read from disk, because the ladder carries no `demands` field to read.
/// Every entry's [`Direction`] is `Produce`: the ladder declares itself a
/// production instrument in its own `production_axis` block, and every rung
/// is prose the grammar must generate, never player input it must parse.
fn read_derived(path: &Path) -> Vec<Entry> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("{} is committed: {e}", path.display()));
    let doc: LadderCorpusJson = serde_json::from_str(&text)
        .unwrap_or_else(|e| panic!("{} parses as the ladder shape: {e}", path.display()));

    let by_id: BTreeMap<&str, &LadderEntryJson> =
        doc.entries.iter().map(|e| (e.id.as_str(), e)).collect();

    doc.entries
        .iter()
        .map(|e| Entry {
            id: e.id.clone(),
            speaker: None,
            text: e.text.clone(),
            demands: derived_demands(e.id.as_str(), &by_id),
            direction: Some(Direction::Produce),
        })
        .collect()
}

fn load_merchant_corpus(root: &Path) -> Corpus {
    Corpus {
        entries: read_declared(&root.join("sentences/the-merchant.corpus.json")),
    }
}

/// Load `the-flood-watch.corpus.json` through the same declared reader
/// [`load_merchant_corpus`] uses — it shares the merchant corpus's shape,
/// plus fields (`scene`, `note`, a per-entry `direction`) this resolver
/// either ignores or already reads.
fn load_flood_watch_corpus(root: &Path) -> Corpus {
    Corpus {
        entries: read_declared(&root.join("sentences/the-flood-watch.corpus.json")),
    }
}

/// Load `the-ladder.corpus.json.DRAFT` through [`read_derived`].
fn load_ladder_corpus(root: &Path) -> Corpus {
    Corpus {
        entries: read_derived(&root.join("sentences/the-ladder.corpus.json.DRAFT")),
    }
}

/// Whether a single demand token has a construction in this campaign.
fn demand_covered(demand: &str) -> bool {
    IMPLEMENTED_DEMANDS.contains(&demand)
}

/// An entry is covered only if it makes at least one demand and every demand
/// it makes is covered. A demand-less entry is malformed data, not a free
/// win — it can never appear in the frozen corpus, but the resolver should
/// not silently call it "covered" if one ever slips in.
fn entry_covered(entry: &Entry) -> bool {
    !entry.demands.is_empty() && entry.demands.iter().all(|d| demand_covered(d))
}

/// The demands an entry makes that have no construction yet, in the corpus's
/// own order. This is the **distance** an entry sits at from being covered,
/// and it is why the resolver reports more than a headline count: coverage is
/// CONJUNCTIVE, so it is a lagging indicator. The Inquest implemented four
/// tokens and moved four *other* entries from two missing demands to one
/// without moving the headline number at all — an instrument that cannot show
/// that is hiding its own progress, and would price the next campaign's work
/// as if nothing had happened.
fn missing_demands(entry: &Entry) -> Vec<&str> {
    entry
        .demands
        .iter()
        .map(String::as_str)
        .filter(|d| !demand_covered(d))
        .collect()
}

// ---------------------------------------------------------------------
// Task 3: three buckets, and direction is never inferred
// ---------------------------------------------------------------------

/// A corpus's entries, tallied by [`Direction`]: how many state `parse`,
/// how many state `produce`, and how many state neither. Spec §2.2/§2.3 —
/// coverage is reported **per direction**, and an absent direction is its
/// own bucket rather than something inferred from `speaker` or any other
/// field. `the-flood-watch` splits across the first two; `the-merchant`
/// states no `direction` key at all and lands entirely in `unknown`; the
/// ladder is declared entirely `produce` by [`read_derived`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DirectionCounts {
    /// Entries whose [`Entry::direction`] is [`Direction::Parse`].
    parse: usize,
    /// Entries whose [`Entry::direction`] is [`Direction::Produce`].
    produce: usize,
    /// Entries whose [`Entry::direction`] is `None` — the corpus states
    /// nothing, and nothing here guesses one.
    unknown: usize,
}

/// Tally a corpus's entries into [`DirectionCounts`]. Reads
/// [`Entry::direction`] exactly as [`read_declared`]/[`read_derived`]
/// produced it — **never** derives one from `speaker`, `id`, or any other
/// field. That restraint is the whole point of Task 3: a merchant entry has
/// a `speaker` (`"player"` or `"merchant"`) sitting right there, and mapping
/// it to parse/produce would look reasonable while authoring a fact the
/// corpus does not carry.
fn direction_counts(entries: &[Entry]) -> DirectionCounts {
    let mut counts = DirectionCounts {
        parse: 0,
        produce: 0,
        unknown: 0,
    };
    for entry in entries {
        match entry.direction {
            Some(Direction::Parse) => counts.parse += 1,
            Some(Direction::Produce) => counts.produce += 1,
            None => counts.unknown += 1,
        }
    }
    counts
}

/// The-flood-watch states `direction` on every entry, split 68 player lines
/// (`parse`) to 71 NPC lines (`produce`) — re-derived here from the
/// committed corpus rather than trusted from any campaign document; see
/// [`the_flood_watch_corpus_is_frozen_at_its_authored_size`] for the
/// corpus's frozen total these two numbers must sum to.
#[test]
fn the_flood_watch_direction_split_is_sixty_eight_parse_seventy_one_produce() {
    let corpus = load_flood_watch_corpus(&repo_root());
    assert_eq!(corpus.entries.len(), FLOOD_WATCH_ENTRIES);

    let counts = direction_counts(&corpus.entries);
    assert_eq!(
        counts,
        DirectionCounts {
            parse: 68,
            produce: 71,
            unknown: 0,
        },
        "the flood-watch direction split moved from 68 parse / 71 produce. \
         Every entry in this corpus states a direction, so a nonzero \
         `unknown` here means a `direction` key failed to parse, not that \
         one was legitimately absent."
    );
}

/// **The negative test, and the more important of the two.** `the-merchant`
/// states no `direction` key on any entry (spec §2.3), and it carries a
/// `speaker` field (`"player"` on 4 entries, `"merchant"` on 8) that is
/// exactly the kind of plausible-looking proxy a later campaign might reach
/// for. This pins that every merchant entry resolves as direction-**unknown**
/// regardless — the only thing standing between a future edit and quietly
/// inferring `parse`/`produce` from `speaker`. This test was proven to bite:
/// a temporary `speaker`-based inference was spliced into
/// [`read_declared`], this test went red, the inference was removed, and the
/// file was hashed back to byte-identical with pre-mutation — see the Task 3
/// report for the transcript.
#[test]
fn merchant_entries_resolve_as_direction_unknown() {
    let corpus = load_merchant_corpus(&repo_root());
    assert_eq!(corpus.entries.len(), MERCHANT_ENTRIES);

    let counts = direction_counts(&corpus.entries);
    assert_eq!(
        counts,
        DirectionCounts {
            parse: 0,
            produce: 0,
            unknown: MERCHANT_ENTRIES,
        },
        "a merchant entry resolved to parse or produce. The corpus states no \
         `direction` key at all, so a nonzero count here means something is \
         inferring direction — from `speaker` or another field — which spec \
         §2.3 forbids: a merchant entry's direction must stay unknown."
    );
}

/// The frozen result of resolving the-merchant against the grammar's
/// delivered capability, independent of the prose report below — so a stale
/// report cannot hide a coverage number that moved.
const MERCHANT_COVERED: usize = 5;

/// **Which** entries are covered, not merely how many. A count of 2 could be
/// any two of twelve, and a wrong pair passing a count check is exactly the
/// failure this instrument exists to prevent — the score would read as
/// evidence for a capability the grammar does not have.
///
/// # "Covered" is a claim about GRAMMAR, never about the lexicon
///
/// The corpus's demand tokens name constructions, so this resolver scores
/// constructions. It does not — and must not be read to — claim the world can
/// utter the entry's literal English. The two are genuinely different, and
/// m10 is the case that shows it. Measured on seed 42, 2026-08-26:
///
/// - **m05, *"A guard killed a woman."* realizes on both sides.** Common
///   gives `Nwamvam killed a person.`; every one of seed 42's placed peoples
///   realizes it in its own tongue with no gap (bugbear:
///   `Nwamvam Doobo Dabo.`), because `kill` is `ladder_rank: 0` — universal
///   stratum, so every lexicon holds a root for it.
/// - **m10, *"I didn't know her."*, realizes only as a SHAPE.** The clause
///   `I did not <V> them` is exactly what
///   `clause.rs::common_realizes_a_pronoun_subject_and_a_pronoun_object`
///   asserts, and it realizes in a tongue too (bugbear: `Qao Dao Dabo.`).
///   But `know` itself is unsayable in both registers, for two independent
///   reasons: it has no `PREDICATE_VALENCE` row, so `realize_common` panics
///   with "Common has no construction for predicate"; and it is in the
///   action-suite pack with no exposure rule, so every lexicon returns
///   `Gap { reason: Experiential("<people> has no exposure to 'know'") }`.
///
/// So m10's three demand tokens are all genuinely built, and the sentence
/// still cannot be spoken. That is not a defect in the score — a lexical gap
/// is what `sentences/` was founded to keep separate from a grammatical one —
/// but it is exactly the misreading a bare "2 of 12" invites, which is why
/// it is written down beside the number rather than left to a chronicle.
///
/// **The Mortise adds m06, m07 and m09, and each is a different distance
/// from the corpus's own English than m05/m10 were** — see
/// [`MERCHANT_WITNESS`] for the actual realized surface, pasted from a run,
/// beside each entry's corpus text:
///
/// - **m06, *"I don't know why he killed her."*, drops the "why".** Its
///   demands (`negation`, `embedded-clause`, `pronoun-reference`,
///   `epistemic-hedge`) are all genuinely built, but none of them is an
///   indirect question — *why* he killed her is a wh-word binding into an
///   embedded clause, a construction this campaign does not add (that is
///   `wh-question`'s job, still uncovered). The witness realizes the
///   embedded clause as a bare declarative complement instead: the covered
///   construction is "know that-clause", not "know why-clause".
/// - **m07, *"Seeing it confused and upset me."*, is a [`Coordination`], not
///   a [`Clause`], substitutes the gerund for a complementizer clause, and
///   drops the shared object.** Per spec §9.1's own argument for keeping m07
///   in scope, the gerund *"Seeing it"* is not load-bearing for m07's THREE
///   DEMANDS — substitute the subject form and hold everything else
///   constant, *"That he killed her confused me and upset me"*, and the same
///   tokens are satisfied through the complementizer-free [`Subject::Clause`]
///   machinery `a_clause_subject_realizes_through_the_same_machinery`
///   already pins. So the witness gives EACH coordinated clause a
///   `Subject::Clause` embedding — never a bare pronoun standing in for it,
///   which would have proven coordination and pronoun-reference while never
///   exercising `embedded-clause` at all, the "Clause-shaped stand-in"
///   failure this doc already forbids on the Clause-vs-Coordination axis,
///   one level deeper. `confuse`/`upset` are still not registered predicates
///   in this crate (see `two_clauses_coordinate_in_common`'s own doc), so the
///   witness stands in with two registered transitive predicates for the
///   MATRIX verbs — the point is the SHAPE (two clauses, each with a clause
///   subject, realized through [`realize_common_coordination`], sharing an
///   identical embedded subject that elides on the second clause per spec
///   §4.10 tier 2), not the corpus's literal verbs. Tier 3 (right-node
///   raising — sharing the OBJECT too, which is what actually collapses
///   "confused and upset **me**" onto one mention) is cut from this campaign
///   entirely (spec §9.1), so the witness's two clauses each restate the
///   object in full.
/// - **m09, *"I think her name was Gilda."*, substitutes an embedded
///   clause for the possessed-name complement.** *"Her name was Gilda"* is
///   one clause inside another, not a bare NP: `think`'s object is the
///   whole complement clause *"her name was Gilda"*, and *"her"* is a
///   possessive determiner inside THAT clause's own subject NP ("her
///   name"), not the pronoun `think` takes directly — an earlier version of
///   this doc mis-parsed the sentence as `think` + a bare pronoun object.
///   No construction here builds a possessive NP or a copular "was Gilda"
///   naming clause, so the witness stands in with a different embedded
///   clause the grammar DOES build (`kill`, transitive, pronoun subject and
///   object) rather than the corpus's literal content — the point is that
///   `epistemic-hedge` is exercised as the GRAMMAR the token names (one
///   clause embedded as another's object), never merely as a lexical fact
///   about the matrix verb alone, the same "Clause-shaped stand-in"
///   discipline m07's entry above states.
const MERCHANT_COVERED_IDS: &[&str] = &["m05", "m06", "m07", "m09", "m10"];

/// The headline score.
///
/// **The name states the number, so it moves when the number moves** — and
/// that is a commit-gate change, not a cosmetic one:
/// `docs/timings/subfloor-roster.tsv` selects tests by EXACT name, so a
/// rename that does not also edit that file silently drops this test from
/// `make gate-commit`. Edit both, in the same commit.
#[test]
fn merchant_coverage_is_five_of_twelve() {
    let root = repo_root();
    let corpus = load_merchant_corpus(&root);
    assert_eq!(corpus.entries.len(), MERCHANT_ENTRIES);

    let covered = corpus.entries.iter().filter(|e| entry_covered(e)).count();
    assert_eq!(
        covered, MERCHANT_COVERED,
        "merchant coverage moved from {MERCHANT_COVERED} to {covered}. If a \
         demand token genuinely gained a construction this campaign, update \
         IMPLEMENTED_DEMANDS and MERCHANT_COVERED together and say why in the \
         chronicle; a coverage number that moves silently is worse than a low \
         one."
    );
}

/// The covered entries by id. Complements — never replaces —
/// [`merchant_coverage_is_five_of_twelve`]: the count and the identities can
/// each move without the other, and only holding both pins the claim the
/// campaign actually makes.
#[test]
fn the_covered_entries_are_m05_m06_m07_m09_and_m10() {
    let corpus = load_merchant_corpus(&repo_root());
    let ids: Vec<&str> = corpus
        .entries
        .iter()
        .filter(|e| entry_covered(e))
        .map(|e| e.id.as_str())
        .collect();
    assert_eq!(
        ids, MERCHANT_COVERED_IDS,
        "the covered SET moved, whatever the count did. m05 (\"A guard killed \
         a woman.\") needs transitive-frame + past-tense; m06 (\"I don't know \
         why he killed her.\") needs negation + embedded-clause + \
         pronoun-reference + epistemic-hedge; m07 (\"Seeing it confused and \
         upset me.\") needs coordination + embedded-clause + \
         pronoun-reference; m09 (\"I think her name was Gilda.\") needs \
         epistemic-hedge + past-tense + pronoun-reference; m10 (\"I didn't \
         know her.\") needs negation + past-tense + pronoun-reference."
    );
}

/// Every intra-doc link in this file whose target begins
/// `merchant_coverage_is_` names a function that exists in this file.
///
/// **This guards a rot mechanism this campaign watched produce a defect.** The
/// headline test's name states the score, so it is renamed every time the score
/// moves — zero, then two, then five. The Stile's Task 6 found a doc comment
/// still pointing at `merchant_coverage_is_zero_of_twelve`, a test that had not
/// existed for two campaigns, and fixed it; four live references to the current
/// name remain, and each of them will dangle on the next move by exactly the
/// same route.
///
/// **Prose discipline was the obvious repair and it is the one that already
/// failed.** Nothing mechanical would have caught the stale reference: an
/// unresolved intra-doc link is a *rustdoc* diagnostic, invisible to nextest,
/// to clippy, to the doctest runner and to `gate-commit`
/// (`TOOL-rustdoc-links`). So the reference is checked here instead, by the one
/// instrument that does run — a text scan over this file's own source, in the
/// same suite as the test it points at. A rename that misses a doc comment is
/// already a commit-gate change (`docs/timings/subfloor-roster.tsv` selects by
/// exact name), so the author is editing this file anyway when this reds.
///
/// Scope is narrow on purpose: it resolves references matching
/// `merchant_coverage_is_*` only, and only against `fn` definitions in this
/// file. It is not a general intra-doc link checker and does not stand in for
/// one — `TOOL-rustdoc-links` still names the missing instrument.
#[test]
fn every_reference_to_the_headline_test_names_a_function_in_this_file() {
    let source = std::fs::read_to_string(repo_root().join("cli/tests/suite/sentence_corpus.rs"))
        .expect("this test file is readable from the repo root");

    let defined: BTreeSet<&str> = source
        .lines()
        .filter_map(|l| l.trim().strip_prefix("fn "))
        .filter_map(|rest| rest.split('(').next())
        .collect();

    // The stem is matched on its own and the opening `` [` `` checked from the
    // bytes before it, so this test's own search literal is not itself a
    // bracket-backtick link and cannot match. A candidate that is not a bare
    // Rust identifier is not a link to a function either — a `…` placeholder
    // in prose, say — so it is skipped rather than reported dangling.
    const STEM: &str = "merchant_coverage_is_";
    let bytes = source.as_bytes();
    let mut referenced: BTreeSet<&str> = BTreeSet::new();
    for (offset, _) in source.match_indices(STEM) {
        if offset < 2 || bytes[offset - 2] != b'[' || bytes[offset - 1] != b'`' {
            continue;
        }
        let after = &source[offset..];
        let Some(end) = after.find('`') else { continue };
        let name = &after[..end];
        if name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        {
            referenced.insert(name);
        }
    }

    assert_eq!(
        referenced.len(),
        1,
        "expected exactly one DISTINCT headline-test link target in this \
         file, found {referenced:?}. This asserts distinctness, not arity: \
         however many `[`merchant_coverage_is_*`]` links exist, they must \
         all name the same current test — it does NOT require four of them \
         to exist, and would stay green if three of the four references this \
         test's own doc comment describes were deleted. An empty set means \
         the scan broke (a moved file path, or a changed link syntax), not \
         that the references went away."
    );

    let dangling: Vec<&&str> = referenced.difference(&defined).collect();
    assert!(
        dangling.is_empty(),
        "intra-doc links naming a headline test that does not exist in this \
         file: {dangling:?}. The headline test's name states the score and is \
         renamed whenever the score moves; every doc comment pointing at it \
         has to move in the same commit. Defined here: {:?}",
        defined
            .iter()
            .filter(|f| f.starts_with(STEM))
            .collect::<Vec<_>>()
    );
}

/// The entries sitting at exactly ONE missing demand, each with the token
/// that blocks it — the frozen distance report.
///
/// Every row here is one construction away from covered, so this list is the
/// cheapest available statement of what the next campaign should build:
/// `wh-question`, `temporal-adverbial` and `polar-question` each unlock one
/// entry from here. The Mortise's three new tokens moved m09 OUT of this
/// list (it is covered now, not one-missing) without shrinking it further —
/// m01, m02 and m08 were already exactly one demand short before this
/// campaign and remain so, because none of their blocking tokens
/// (`wh-question`, `temporal-adverbial`, `polar-question`) is one this
/// campaign builds. The wider distance-2-and-beyond picture (not tracked as
/// its own constant): `temporal-adverbial` still blocks 3 entries in total,
/// `wh-question` and `polar-question` 2 each, `witness-set` 2,
/// `existential` and `named-entity-list` 1 each.
const MERCHANT_ONE_MISSING: &[(&str, &str)] = &[
    ("m01", "wh-question"),
    ("m02", "temporal-adverbial"),
    ("m08", "polar-question"),
];

/// Spec criterion 7: the resolver reports distance, not only coverage.
#[test]
fn three_entries_sit_at_one_missing_demand() {
    let corpus = load_merchant_corpus(&repo_root());
    let one_away: Vec<(&str, &str)> = corpus
        .entries
        .iter()
        .filter_map(|e| {
            let missing = missing_demands(e);
            match missing.as_slice() {
                [only] => Some((e.id.as_str(), *only)),
                _ => None,
            }
        })
        .collect();
    assert_eq!(
        one_away, MERCHANT_ONE_MISSING,
        "the distance report moved. This is the leading indicator the headline \
         count cannot show, so a change here is a real result even when \
         MERCHANT_COVERED holds still — record it rather than re-baselining it \
         silently."
    );
}

/// A covered entry is at distance zero, and an entry at distance zero is
/// covered — the two halves of the resolver must agree, or the distance
/// report could show progress the score contradicts.
///
/// **They agree on the corpus, not by construction**, and the one case where
/// they would not is the malformed one `entry_covered`'s doc already names: a
/// demand-LESS entry has nothing missing but is not covered. No such entry is
/// in the frozen corpus, and this test going red is the right response if one
/// ever arrives.
#[test]
fn distance_zero_is_exactly_coverage() {
    let corpus = load_merchant_corpus(&repo_root());
    for entry in &corpus.entries {
        assert_eq!(
            missing_demands(entry).is_empty(),
            entry_covered(entry),
            "{} disagrees between the score and the distance report",
            entry.id
        );
    }
}

/// THE POSITIVE CONTROL. Built inline, never touching the frozen corpus
/// file: a synthetic entry demanding only `classify` must resolve as covered.
///
/// **Its original job is done, and it keeps its place for a second one.** It
/// was written when the score was 0 of 12, where it was the *only* thing
/// separating a working resolver from one hardcoded to return "not yet" —
/// that hardcoding left the headline coverage test green, because zero was
/// the expected answer at that score. (That test's own name has moved with
/// the score since — zero, then two, then five — so it is named here by what
/// it does rather than by a literal name that would go stale on the next
/// move; see [`merchant_coverage_is_five_of_twelve`] for its current form.)
/// The Inquest moved the score to 2, so a hardcoded "not yet" now reds the
/// headline test on its own and this control is no longer load-bearing for
/// THAT reading.
///
/// What it still holds is independence from the corpus. Every other
/// assertion in this file is a claim about `sentences/the-merchant.corpus.json`
/// — frozen data a future campaign is expected to swap for a harder corpus,
/// at which point the score, the covered ids and the distance report all
/// re-baseline together and could re-baseline to anything, including the
/// all-zero row a broken resolver produces. This test and its two siblings
/// below (`an_unimplemented_demand_resolves_as_not_yet`,
/// `a_mixed_entry_needs_every_demand_covered`) survive that swap untouched,
/// because they exercise `entry_covered`'s three branches against inputs the
/// corpus does not supply. Deleting it would trade a permanent guard for a
/// temporary redundancy.
#[test]
fn a_classify_only_entry_resolves_as_covered() {
    let synthetic = Entry {
        id: "synthetic-positive-control".to_string(),
        speaker: Some("player".to_string()),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string()],
        direction: None,
    };
    assert!(
        entry_covered(&synthetic),
        "a demand-only-classify entry must resolve as covered; if this fails, \
         the resolver has regressed to reporting \"not yet\" unconditionally"
    );
}

/// A demand token the negative controls below can rely on being *un*covered.
///
/// **This is a maintenance obligation, and it has already come due twice.**
/// Both controls named `"negation"` until The Inquest implemented negation,
/// at which point they went red — correctly, but with a bare
/// `assertion failed` that said nothing about why. Naming the token once,
/// here, with [`the_negative_control_token_is_genuinely_uncovered`] asserting
/// the property the controls depend on, turns that rot into a failure that
/// explains itself — which is exactly what caught the second occurrence: The
/// Mortise implemented `embedded-clause`, the token this constant named
/// before, and the assertion below reddened with a diagnosis rather than a
/// bare panic. `wh-question` is the new pick: [`Clause`] has no interrogative
/// mood and no question-word slot at all, the same kind of structural gap
/// `embedded-clause` was before a `Clause`/`Coordination` distinction existed
/// — so it should outlast the tokens around it, though "should" is exactly
/// the word `embedded-clause`'s own doc used last time.
const UNCOVERED_TOKEN: &str = "wh-question";

/// The controls' own premise, asserted rather than assumed: whichever token
/// [`UNCOVERED_TOKEN`] names must really be absent from
/// [`IMPLEMENTED_DEMANDS`]. Without this, implementing that token would turn
/// both negative controls into vacuous passes-turned-failures with no
/// diagnosis attached.
#[test]
fn the_negative_control_token_is_genuinely_uncovered() {
    assert!(
        !demand_covered(UNCOVERED_TOKEN),
        "{UNCOVERED_TOKEN} is now implemented, so it can no longer serve as \
         the negative controls' uncovered token. Point UNCOVERED_TOKEN at a \
         demand the grammar still lacks — the distance report names the \
         candidates."
    );
}

/// The negative complement: an entry demanding something the grammar does
/// not build must resolve as not-yet, so the positive control above is
/// checking a real branch and not a resolver that always says "covered".
#[test]
fn an_unimplemented_demand_resolves_as_not_yet() {
    let synthetic = Entry {
        id: "synthetic-negative-control".to_string(),
        speaker: Some("player".to_string()),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec![UNCOVERED_TOKEN.to_string()],
        direction: None,
    };
    assert!(!entry_covered(&synthetic));
}

/// An entry is covered only when EVERY demand it makes is covered — one
/// implemented demand alongside one unimplemented demand must not count.
#[test]
fn a_mixed_entry_needs_every_demand_covered() {
    let synthetic = Entry {
        id: "synthetic-mixed".to_string(),
        speaker: Some("player".to_string()),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string(), UNCOVERED_TOKEN.to_string()],
        direction: None,
    };
    assert!(!entry_covered(&synthetic));
}

// ---------------------------------------------------------------------
// Task 1 (The Stile): one internal entry, two readers
// ---------------------------------------------------------------------

/// **Was the RED probe for Task 1 Step 2.** Before [`read_derived`] existed,
/// this test deserialized `the-ladder.corpus.json.DRAFT` straight into the
/// old `speaker`+`demands`-requiring `Entry`/`Corpus` shape, which panicked
/// on a missing field — proving nothing could read the ladder at all. It now
/// asserts the real thing: r004 ("The long road runs to the shrine.")
/// presupposes r003 ("The road is long."), which presupposes r001 ("The
/// woman is a merchant."). Its own `introduces` is `attributive-adjective`;
/// r003's is `property-predication`; r001's is `classify`. A one-level
/// reader would stop at r003's own `introduces` and never reach r001's
/// `classify` — that is exactly the shallow-closure bug Task 2 pins
/// directly; this test only needs the readers to exist and compute past one
/// level.
#[test]
fn a_ladder_rung_derives_its_transitive_demand_set() {
    let corpus = load_ladder_corpus(&repo_root());
    let r004 = corpus
        .entries
        .iter()
        .find(|e| e.id == "r004")
        .expect("r004 is a ladder rung");
    assert_eq!(
        r004.demands,
        vec![
            "attributive-adjective".to_string(),
            "classify".to_string(),
            "property-predication".to_string(),
        ],
        "r004's derived transitive demand set moved. The ladder is a \
         revisable DRAFT (decision permits this), so a red here is not \
         necessarily a bug — appending a rung, or rewiring `presupposes` \
         mid-graph the way the ladder's last revision did (64 rungs placed \
         throughout, not appended at the end), can legitimately change what \
         r004 transitively presupposes. This assertion pins a CLOSURE, not \
         a count: if the ladder moved deliberately, re-derive r004's demand \
         set from the committed JSON (do not hand-edit this list to make it \
         pass) and update it in the same commit, with a note on why. The \
         ladder stays unfrozen either way — this test does not freeze it."
    );
}

// ---------------------------------------------------------------------
// Task 2 (The Stile): the closure, proven transitive
// ---------------------------------------------------------------------

/// r004 above (2 shallow → 3 transitive) proves the readers compute past one
/// level; it does not prove the closure goes *deep*, and a shallow bug is
/// exactly the kind that produces a plausible, smaller set. r183 ("Who says
/// the guard was paid?") pins the deep case: read straight off the corpus,
/// a one-level reader (the union of r183's own `introduces`, `null` here,
/// plus its direct presuppositions' `introduces`) sees only 3 tokens —
/// `evidential-scope` (r183 itself), `evidential-hearsay` (r181) and
/// `negative-question` (r095) — while the true transitive closure over
/// `presupposes` reaches 22. Re-derived directly from the committed JSON by
/// a throwaway script rather than trusted from the brief, which also named
/// this rung as the widest gap in the ladder (tied with r191, both
/// 3 → 22); r183 is chosen over r191 because its own two direct
/// presuppositions (r181 and r095) independently reach a shared ancestor
/// (r001, `classify`, and r002, `intransitive-frame`), so this single rung
/// also exercises the diamond case Task 2's brief asks about — see this test's
/// own note below for why that made a second, dedicated diamond assertion
/// unnecessary.
///
/// **This rung is a diamond in its own right, which is why one assertion
/// covers both questions the brief raises.** r183 presupposes `[r181,
/// r095]` directly; walking each branch separately (`transitive_ids`
/// rooted at r181, then at r095) reaches `r001` and `r002` from *both*
/// sides. A walk that double-counted a revisited ancestor, or that dropped
/// a branch's contribution once the other branch had already visited a
/// shared node, would show up here: `demands` is a `BTreeSet` collected
/// into sorted `Vec<String>` order, so either fault changes the asserted
/// 22-element list this test pins — a shorter list on a lost branch, or
/// (had `demands` been a bag rather than a set) a token appearing twice.
/// No second, rung-only-for-diamonds test is added: r183 already forces the
/// walk to reconcile two paths into the same ancestor, and hand-picking a
/// second rung for the same structural property without a second bug class
/// to distinguish it from this one would only add corpus-reading cost for
/// no new coverage.
#[test]
fn a_deep_ladder_rung_derives_its_full_transitive_closure() {
    let corpus = load_ladder_corpus(&repo_root());
    let r183 = corpus
        .entries
        .iter()
        .find(|e| e.id == "r183")
        .expect("r183 is a ladder rung");
    assert_eq!(
        r183.demands,
        vec![
            "ability-modal".to_string(),
            "classify".to_string(),
            "clause-coordination".to_string(),
            "complement-utterance".to_string(),
            "coordination".to_string(),
            "definiteness".to_string(),
            "direct-quotation".to_string(),
            "embedded-clause".to_string(),
            "epistemic-hedge".to_string(),
            "evidential-hearsay".to_string(),
            "evidential-scope".to_string(),
            "evidential-witness".to_string(),
            "intransitive-frame".to_string(),
            "negation".to_string(),
            "negative-question".to_string(),
            "number-marking".to_string(),
            "past-tense".to_string(),
            "person-deixis".to_string(),
            "polar-question".to_string(),
            "pronoun-reference".to_string(),
            "reported-speech".to_string(),
            "transitive-frame".to_string(),
        ],
        "r183's derived transitive demand set moved. The ladder is a \
         revisable DRAFT (decision permits this), so a red here is not \
         necessarily a bug — appending a rung, or rewiring `presupposes` \
         mid-graph the way the ladder's last revision did (64 rungs placed \
         throughout, not appended at the end), can legitimately change what \
         r183 transitively presupposes. This assertion pins a CLOSURE, not \
         a count: if the ladder moved deliberately, re-derive r183's demand \
         set from the committed JSON (do not hand-edit this list to make it \
         pass) and update it in the same commit, with a note on why. The \
         ladder stays unfrozen either way — this test does not freeze it."
    );
}

// ---------------------------------------------------------------------
// Task 4 (The Stile): structural assertions over the ladder
// ---------------------------------------------------------------------
//
// **The ladder is `.DRAFT` and this campaign must not freeze it.** A frozen
// entry count is the freeze mechanism ([`MERCHANT_ENTRIES`] does exactly
// that for the merchant corpus above), and freezing is the project owner's
// act — the moment it happens, rung ids become append-only forever (the
// ladder's own `renumbering` block says so). So nothing below asserts a
// count of rungs. What holds instead are structural properties true at any
// size: acyclic, ids unique, no token introduced twice, every rung's
// cumulative closure computable (and consistent with its presuppositions'),
// and exactly two roots.

/// Read `the-ladder.corpus.json.DRAFT`'s raw entries with none of
/// [`read_derived`]'s post-processing — the structural checks below need the
/// graph itself (`presupposes` edges, `introduces` tokens), not the resolved
/// demand sets a rung's [`Entry`] carries.
fn read_ladder_raw(path: &Path) -> Vec<LadderEntryJson> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("{} is committed: {e}", path.display()));
    let doc: LadderCorpusJson = serde_json::from_str(&text)
        .unwrap_or_else(|e| panic!("{} parses as the ladder shape: {e}", path.display()));
    doc.entries
}

/// DFS colouring's three states, applied to the `presupposes` graph
/// (Cormen et al.): a node not yet visited, a node currently on the
/// recursion path (revisiting it is a back edge — a cycle), and a node
/// whose whole subtree is already explored (safe to see again from another
/// branch, which is what makes a diamond — two branches reconverging on a
/// shared ancestor, the shape [`derived_demands`]'s own doc discusses —
/// linear-time instead of exponential).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Colour {
    /// Not yet reached by the walk.
    White,
    /// On the current DFS path — an edge into a `Gray` node is a cycle.
    Gray,
    /// Fully explored; revisiting it from elsewhere is not a cycle.
    Black,
}

/// A real cycle detector over `presupposes`: DFS-coloured, explicit-stack
/// (so depth is bounded by heap, not the call stack, on a ladder far larger
/// than 214 rungs), returning either a full topological order (root-most
/// first) or the minimal cycle — the live ancestors actually on the loop,
/// sliced out of the full DFS path rather than the path itself, so a rung
/// merely visited on the way to the cycle (a root, a shared ancestor) is
/// never named as part of it.
///
/// **This is not a backward-reference check.** A file whose `presupposes`
/// only ever names ids appearing earlier in the array is trivially acyclic
/// by construction — so a test that merely confirms every reference points
/// backward would pass on such a file forever while proving almost nothing:
/// a real cycle introduced by an edit need not respect file order at all,
/// since every id is looked up by content (`by_id`), never by position. Only
/// a real graph walk that tracks the current path — not merely "have I seen
/// this id before" — can tell a shared ancestor (safe; a diamond) from a
/// live ancestor (a cycle).
fn topological_order(entries: &[LadderEntryJson]) -> Result<Vec<&str>, Vec<&str>> {
    let by_id: BTreeMap<&str, &LadderEntryJson> =
        entries.iter().map(|e| (e.id.as_str(), e)).collect();
    let mut colour: BTreeMap<&str, Colour> = entries
        .iter()
        .map(|e| (e.id.as_str(), Colour::White))
        .collect();
    let mut order: Vec<&str> = Vec::new();

    for start in entries.iter().map(|e| e.id.as_str()) {
        if colour[start] != Colour::White {
            continue;
        }
        // Each stack frame is (id, index of the next presupposition of id
        // still to visit) — the frame stays on the stack, index advancing,
        // for as long as `id` is on the current DFS path, and is popped for
        // good (without being pushed back) only once every presupposition
        // has been explored. So at any moment, the ids present in `stack`
        // are exactly the `Gray` ones, root to current, in path order.
        let mut stack: Vec<(&str, usize)> = vec![(start, 0)];
        colour.insert(start, Colour::Gray);
        while let Some((id, next)) = stack.pop() {
            let rung = by_id[id];
            if next < rung.presupposes.len() {
                let child = rung.presupposes[next].as_str();
                stack.push((id, next + 1));
                match colour.get(child) {
                    Some(Colour::White) => {
                        colour.insert(child, Colour::Gray);
                        stack.push((child, 0));
                    }
                    Some(Colour::Gray) => {
                        // `stack` holds the full DFS path from this call's
                        // root to `id`, not the cycle itself — a root
                        // reached before the cycle can sit ahead of it on
                        // the path. `child` is Gray, so by the loop's own
                        // invariant it is somewhere on that path; slicing
                        // from its first (only) occurrence to the end
                        // yields exactly the live ancestors between `child`
                        // and `id`, and re-appending `child` closes the
                        // loop — the MINIMAL cycle, not a superset of it.
                        let path: Vec<&str> = stack.iter().map(|(id, _)| *id).collect();
                        let start = path
                            .iter()
                            .position(|&pid| pid == child)
                            .expect("child is Gray, so it is on the current DFS path");
                        let mut cycle: Vec<&str> = path[start..].to_vec();
                        cycle.push(child);
                        return Err(cycle);
                    }
                    Some(Colour::Black) => {}
                    None => panic!("presupposes references unknown rung {child:?}"),
                }
            } else {
                colour.insert(id, Colour::Black);
                order.push(id);
            }
        }
    }
    Ok(order)
}

/// **Structural assertion: acyclic.** [`topological_order`] must both
/// succeed and account for every rung — a detector that silently dropped
/// rungs (e.g. by treating an already-`Black` node as "done, stop counting")
/// would be as useless here as one that never fires at all.
#[test]
fn the_ladder_is_acyclic() {
    let entries = read_ladder_raw(&repo_root().join("sentences/the-ladder.corpus.json.DRAFT"));
    let n = entries.len();
    match topological_order(&entries) {
        Ok(order) => assert_eq!(
            order.len(),
            n,
            "topological sort produced {} entries from {n} rungs in the file; \
             they must match exactly",
            order.len()
        ),
        Err(cycle) => panic!("the ladder has a cycle: {cycle:?}"),
    }
}

/// **THE POSITIVE CONTROL for [`topological_order`].** A cycle detector that
/// has never fired is not a detector — the same standing rule
/// [`a_classify_only_entry_resolves_as_covered`]'s own doc states for the
/// resolver's positive control, and this campaign has already found one test
/// elsewhere that passed under the exact mutation it existed to catch.
///
/// Builds a two-rung synthetic ladder with a genuine cycle (`x001`
/// presupposes `x002`, which presupposes `x001`) and writes it to a file
/// under [`std::env::temp_dir`] — never under `sentences/`, which this
/// campaign does not edit outside `the-ladder.corpus.json.DRAFT` itself.
/// [`read_ladder_raw`] takes a path, so a temp file is the natural seam
/// (Task 1's fix round established this route works and never touches
/// `sentences/`). Confirms the detector reports the cycle by name, then
/// hashes the real ladder file before and after to confirm it was never
/// touched.
#[test]
fn the_cycle_detector_fires_on_an_injected_cycle() {
    let ladder_path = repo_root().join("sentences/the-ladder.corpus.json.DRAFT");
    let before = std::fs::read(&ladder_path).expect("the ladder is committed and readable before");

    let cyclic_json = r#"{
        "entries": [
            {"id": "x001", "text": "a", "introduces": "tok-a", "presupposes": ["x002"]},
            {"id": "x002", "text": "b", "introduces": "tok-b", "presupposes": ["x001"]}
        ]
    }"#;
    let tmp = std::env::temp_dir().join(format!(
        "hornvale-ladder-cycle-control-{}.json",
        std::process::id()
    ));
    std::fs::write(&tmp, cyclic_json).expect("the temp control file writes");
    let entries = read_ladder_raw(&tmp);
    let result = topological_order(&entries);
    std::fs::remove_file(&tmp).ok();

    match result {
        Ok(order) => panic!(
            "topological_order reported a valid order ({order:?}) over a \
             corpus containing a genuine x001<->x002 cycle; the detector has \
             not fired and cannot be trusted to fire on a real one"
        ),
        Err(cycle) => assert!(
            cycle.contains(&"x001") && cycle.contains(&"x002"),
            "the detector fired, but the reported cycle {cycle:?} does not \
             name both cyclic ids"
        ),
    }

    let after = std::fs::read(&ladder_path).expect("the ladder is committed and readable after");
    assert_eq!(
        before, after,
        "the cycle-detector positive control modified the real ladder file; \
         it must only ever write its synthetic cycle to a temp file"
    );
}

/// **Structural assertion: ids unique.** A duplicate id would make
/// `by_id`'s construction (in [`derived_demands`] and [`topological_order`]
/// alike) silently drop one of the two entries it collides on, which would
/// corrupt every closure and every cycle check above without either ever
/// reporting an error.
#[test]
fn the_ladder_has_no_duplicate_ids() {
    let entries = read_ladder_raw(&repo_root().join("sentences/the-ladder.corpus.json.DRAFT"));
    let mut seen: BTreeSet<&str> = BTreeSet::new();
    let mut duplicates: Vec<&str> = Vec::new();
    for entry in &entries {
        if !seen.insert(entry.id.as_str()) {
            duplicates.push(entry.id.as_str());
        }
    }
    assert!(
        duplicates.is_empty(),
        "duplicate ladder ids: {duplicates:?}"
    );
}

/// **Structural assertion: no token introduced twice.** Two rungs both
/// declaring the same `introduces` token would make "which rung is the
/// canonical source of this capability" ambiguous, which is exactly the
/// question [`derived_demands`] answers by folding every reached
/// `introduces` into one set — a set that cannot see or report the
/// ambiguity once two rungs contribute the same token.
#[test]
fn no_ladder_token_is_introduced_twice() {
    let entries = read_ladder_raw(&repo_root().join("sentences/the-ladder.corpus.json.DRAFT"));
    let mut introduced_by: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for entry in &entries {
        if let Some(token) = entry.introduces.as_deref() {
            introduced_by
                .entry(token)
                .or_default()
                .push(entry.id.as_str());
        }
    }
    let duplicates: BTreeMap<&str, &Vec<&str>> = introduced_by
        .iter()
        .filter(|(_, ids)| ids.len() > 1)
        .map(|(token, ids)| (*token, ids))
        .collect();
    assert!(
        duplicates.is_empty(),
        "tokens introduced by more than one rung: {duplicates:?}"
    );
}

/// **Structural assertion: every rung's cumulative closure is computable,
/// and consistent with its presuppositions.** Every rung's closure must
/// contain its own `introduces` token (if any), and must be a superset of
/// each of its direct presuppositions' own closures — the monotonicity a
/// transitive closure over a DAG is required to have. This both exercises
/// [`derived_demands`] over every rung without panicking (the "computable"
/// half) and pins the closure operator's actual algebra (the "cumulative"
/// half) — a shallow reader that stopped one level early, the exact bug
/// [`a_ladder_rung_derives_its_transitive_demand_set`] pins on r004 alone,
/// would violate this superset relation on some rung somewhere in the
/// ladder even if it happened to pass on r004 and r183 individually.
#[test]
fn every_rungs_closure_is_a_superset_of_its_presuppositions_closures() {
    let entries = read_ladder_raw(&repo_root().join("sentences/the-ladder.corpus.json.DRAFT"));
    let by_id: BTreeMap<&str, &LadderEntryJson> =
        entries.iter().map(|e| (e.id.as_str(), e)).collect();

    for entry in &entries {
        let demands: BTreeSet<String> = derived_demands(entry.id.as_str(), &by_id)
            .into_iter()
            .collect();

        if let Some(token) = &entry.introduces {
            assert!(
                demands.contains(token),
                "{}'s own closure does not contain the token it introduces \
                 ({token})",
                entry.id
            );
        }

        for presupposed_id in &entry.presupposes {
            let parent_demands: BTreeSet<String> = derived_demands(presupposed_id, &by_id)
                .into_iter()
                .collect();
            assert!(
                parent_demands.is_subset(&demands),
                "{}'s closure is missing tokens its presupposition {presupposed_id} \
                 has: {:?}",
                entry.id,
                parent_demands.difference(&demands).collect::<Vec<_>>()
            );
        }
    }
}

/// **Structural assertion: exactly two roots.** A root is a rung with no
/// presuppositions at all — the ladder's own header states there must be
/// exactly two, one per independent predication strategy (nominal and
/// verbal; Stassen 1997), deliberately not nested under each other. This
/// holds at any corpus size: appending a rung anywhere in the file changes
/// the root count only if that rung is itself rootless, which is the
/// deliberate act a third independent strategy would be — not a drift.
#[test]
fn the_ladder_has_exactly_two_roots() {
    let entries = read_ladder_raw(&repo_root().join("sentences/the-ladder.corpus.json.DRAFT"));
    let roots: Vec<&str> = entries
        .iter()
        .filter(|e| e.presupposes.is_empty())
        .map(|e| e.id.as_str())
        .collect();
    assert_eq!(
        roots.len(),
        2,
        "the ladder's root count moved from 2 to {}: {roots:?}. A root is a \
         rung with no presuppositions at all; the ladder's own header \
         states there are exactly two independent predication strategies \
         (nominal and verbal), so a third root is a deliberate structural \
         claim, not a drift.",
        roots.len()
    );
}

// ---------------------------------------------------------------------
// Task 5 (The Stile): the standing cross-check
// ---------------------------------------------------------------------
//
// The ladder (214 rungs, written from linguistic typology) and
// `the-flood-watch.corpus.json` (139 utterances of investigative dialogue,
// written by the project owner) were authored independently on purpose,
// neither author seeing the other's work — the cross-check between them is
// the entire reason for that separation. It already paid once: 56 of the
// flood-watch corpus's demand tokens did not exist on the ladder's first
// draft (its own `minted_tokens` block records them), and the ladder was
// revised. What follows makes that comparison a standing computation rather
// than a one-time analysis, so the NEXT corpus that demands a token with no
// rung gets noticed the same way.

/// Every token any rung in a raw ladder read introduces, deduped into a
/// `BTreeSet` — the ladder's whole vocabulary, not any one rung's transitive
/// closure. [`derived_demands`] answers "what does THIS rung presuppose";
/// this answers "what can the ladder say at all", which is the set a
/// cross-check needs.
fn ladder_introduced_tokens(entries: &[LadderEntryJson]) -> BTreeSet<String> {
    entries
        .iter()
        .filter_map(|e| e.introduces.clone())
        .collect()
}

/// The demand tokens named at least once across a corpus's entries, deduped
/// into a `BTreeSet`. A token five entries all demand still counts once for
/// a vocabulary cross-check — the question is "does the ladder have a rung
/// for this token at all", not "how many entries need it".
fn distinct_demand_tokens(entries: &[Entry]) -> BTreeSet<String> {
    entries
        .iter()
        .flat_map(|e| e.demands.iter().cloned())
        .collect()
}

/// A corpus's distinct demand tokens that the ladder's vocabulary does not
/// contain — the whole cross-check, as one set difference.
fn tokens_absent_from_ladder(
    corpus_tokens: &BTreeSet<String>,
    ladder_tokens: &BTreeSet<String>,
) -> BTreeSet<String> {
    corpus_tokens.difference(ladder_tokens).cloned().collect()
}

/// The flood-watch demand tokens the ladder is **refused** to cover, not
/// missing to cover. Both are input-surface properties that appear only on
/// `parse` entries — every player line in the corpus is lowercase and
/// unterminated, the actual input surface a real player types — while every
/// ladder text is well-formed prose, because the ladder is a *production*
/// instrument (its own `production_axis` block states this). A production
/// instrument has no rung that could ever introduce either token, so their
/// absence is a structural fact about what the ladder is FOR, not a gap
/// nobody has closed yet. Named here so the cross-check's ceiling (147, not
/// 149) ships with its reason rather than reading as an unexplained miss.
const FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER: &[&str] =
    &["contraction-elision", "unpunctuated-input"];

/// The flood-watch corpus's distinct demand-token vocabulary: 149 tokens,
/// re-derived here from the committed corpus rather than trusted from any
/// campaign document.
const FLOOD_WATCH_DISTINCT_DEMAND_TOKENS: usize = 149;

/// **The standing cross-check.** 147 of the flood-watch corpus's 149
/// distinct demand tokens exist on the ladder; the two that do not are
/// exactly [`FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER`], never any other
/// pair — a future edit that drops a *different* token from the ladder while
/// somehow keeping the count at 147 (or 2 absent) must still fail this,
/// because the assertion compares the SET, not merely its size.
#[test]
fn the_ladder_covers_flood_watch_vocabulary_except_the_two_refused_input_surface_tokens() {
    let root = repo_root();

    let flood_watch = load_flood_watch_corpus(&root);
    assert_eq!(flood_watch.entries.len(), FLOOD_WATCH_ENTRIES);
    let flood_watch_tokens = distinct_demand_tokens(&flood_watch.entries);
    assert_eq!(
        flood_watch_tokens.len(),
        FLOOD_WATCH_DISTINCT_DEMAND_TOKENS,
        "the flood-watch corpus's distinct demand vocabulary moved from {FLOOD_WATCH_DISTINCT_DEMAND_TOKENS}; \
         the corpus is frozen (see the_flood_watch_corpus_is_frozen_at_its_authored_size), \
         so this should only move alongside a deliberate, logged change to it"
    );

    let ladder_entries = read_ladder_raw(&root.join("sentences/the-ladder.corpus.json.DRAFT"));
    let ladder_tokens = ladder_introduced_tokens(&ladder_entries);

    let absent = tokens_absent_from_ladder(&flood_watch_tokens, &ladder_tokens);
    let refused: BTreeSet<String> = FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER
        .iter()
        .map(|token| token.to_string())
        .collect();
    assert_eq!(
        absent, refused,
        "the ladder's coverage of the flood-watch vocabulary moved. The only \
         tokens ever expected absent are {FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER:?} \
         — input-surface properties of `parse` entries that a production \
         instrument like the ladder structurally cannot introduce (see \
         FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER's own doc). If a DIFFERENT \
         token is missing, the ladder needs a new rung for it — that is this \
         test's whole job. If a token in this list is no longer absent, the \
         ladder started producing input-surface prose, which is worth its \
         own investigation before updating the constant."
    );

    let covered = flood_watch_tokens.len() - absent.len();
    assert_eq!(
        covered, 147,
        "flood-watch coverage moved from 147 of {FLOOD_WATCH_DISTINCT_DEMAND_TOKENS}. \
         The ceiling is 147, not 149, because contraction-elision and \
         unpunctuated-input are refused rather than missing — see this \
         test's own doc."
    );
}

/// The merchant corpus's vocabulary is fully covered by the ladder — 13 of
/// 13 — worth pinning on its own: the ladder, authored after the merchant
/// corpus (The Interlinear), still names a rung for every demand token the
/// corpus that predates it makes.
#[test]
fn the_ladder_covers_the_merchant_corpus_vocabulary_completely() {
    let root = repo_root();

    let merchant = load_merchant_corpus(&root);
    assert_eq!(merchant.entries.len(), MERCHANT_ENTRIES);
    let merchant_tokens = distinct_demand_tokens(&merchant.entries);
    assert_eq!(
        merchant_tokens.len(),
        13,
        "the merchant corpus's distinct demand vocabulary moved from 13; the \
         corpus is frozen (see the_merchant_corpus_is_frozen_at_its_authored_size), \
         so this should only move alongside a deliberate, logged change to it"
    );

    let ladder_entries = read_ladder_raw(&root.join("sentences/the-ladder.corpus.json.DRAFT"));
    let ladder_tokens = ladder_introduced_tokens(&ladder_entries);

    let absent = tokens_absent_from_ladder(&merchant_tokens, &ladder_tokens);
    assert!(
        absent.is_empty(),
        "the merchant corpus's vocabulary is no longer fully covered by the \
         ladder: {absent:?} of {} demand token(s) now have no rung. The \
         merchant corpus carries no refused tokens the way flood-watch's \
         `parse` entries do (see FLOOD_WATCH_TOKENS_REFUSED_BY_THE_LADDER), \
         so any absence here is a real gap the ladder needs a new rung for.",
        merchant_tokens.len()
    );
}

/// **Proves the cross-check is sensitive to a token going missing from the
/// ladder, not merely to a count.** A test asserting only `147 == 147` would
/// pass while the ladder silently lost one token and gained a different one
/// — precisely the drift this cross-check exists to catch. Loads the real
/// ladder as raw JSON, mutates an IN-MEMORY copy so r001 (which introduces
/// `classify`, a token flood-watch demands) introduces nothing, writes that
/// mutated copy to a temp file — never under `sentences/`, which this
/// campaign does not edit outside `the-ladder.corpus.json.DRAFT` itself —
/// and confirms `classify` newly appears among flood-watch's absent tokens.
/// Then confirms the real ladder file on disk was never touched.
#[test]
fn removing_a_rungs_introduces_token_makes_the_cross_check_notice() {
    let root = repo_root();
    let ladder_path = root.join("sentences/the-ladder.corpus.json.DRAFT");
    let before = std::fs::read(&ladder_path).expect("the ladder is committed and readable before");

    let mut doc: serde_json::Value =
        serde_json::from_slice(&before).expect("the ladder parses as JSON");
    let entries = doc["entries"]
        .as_array_mut()
        .expect("the ladder has an entries array");
    let r001 = entries
        .iter_mut()
        .find(|e| e["id"] == "r001")
        .expect("r001 is a ladder rung");
    assert_eq!(
        r001["introduces"], "classify",
        "r001 no longer introduces classify; this mutation targets the wrong \
         rung and the assertion below would prove nothing"
    );
    r001["introduces"] = serde_json::Value::Null;

    let tmp = std::env::temp_dir().join(format!(
        "hornvale-ladder-token-removal-control-{}.json",
        std::process::id()
    ));
    std::fs::write(
        &tmp,
        serde_json::to_vec(&doc).expect("the mutated ladder serializes"),
    )
    .expect("the temp control file writes");
    let mutated_entries = read_ladder_raw(&tmp);
    std::fs::remove_file(&tmp).ok();

    let mutated_ladder_tokens = ladder_introduced_tokens(&mutated_entries);
    let flood_watch = load_flood_watch_corpus(&root);
    let flood_watch_tokens = distinct_demand_tokens(&flood_watch.entries);
    let absent = tokens_absent_from_ladder(&flood_watch_tokens, &mutated_ladder_tokens);

    assert!(
        absent.contains("classify"),
        "removing r001's introduces token did not make classify newly \
         absent from the cross-check: {absent:?}. Either classify left \
         flood-watch's demand vocabulary, or the in-memory mutation did not \
         take effect — either way this control is no longer proving what it \
         claims to."
    );

    let after = std::fs::read(&ladder_path).expect("the ladder is committed and readable after");
    assert_eq!(
        before, after,
        "the token-removal control modified the real ladder file; it must \
         only ever write its mutated copy to a temp file"
    );
}

// ---------------------------------------------------------------------
// Task 1: the realization witness
// ---------------------------------------------------------------------

/// The grammar shape a covered merchant id realizes through.
///
/// **The Mortise adds this dispatch.** Before this campaign every covered id
/// was a single [`Clause`], so the witness could build one and hand it to
/// [`realize_common`] unconditionally. m07 breaks that: it is a
/// [`Coordination`] — a list of clauses at a node above `Clause`, not a
/// slot a `Clause` holds (see [`Coordination`]'s own doc) — realized through
/// [`realize_common_coordination`] instead. **Giving m07 a `Clause`-shaped
/// stand-in that realizes through `realize_common` would make the witness
/// attest to a capability m07 does not exercise** — the exact failure
/// [`every_covered_entry_realizes_in_common`] exists to catch — so the
/// builder returns this enum and the caller matches on it rather than
/// forcing every id through one realizer.
enum MerchantConstruction {
    /// A single clause, realized through [`realize_common`].
    Clause(Clause),
    /// A coordination of clauses, realized through
    /// [`realize_common_coordination`].
    Coordination(Coordination),
}

/// The [`MerchantConstruction`] this campaign builds for a covered merchant
/// id.
///
/// **Not a general corpus-to-clause translator** — this file has no parser
/// from the corpus's English into a `Clause`/`Coordination`, and building one
/// is out of scope for the witness. Each arm is hand-built from the entry's
/// own text, reading the field list straight off [`Clause`]/[`Coordination`]
/// rather than off any other document, and each is honest about where it
/// falls short of the corpus's literal English (see [`MERCHANT_COVERED_IDS`]'s
/// doc for the three new gaps: m06 drops the indirect *"why"*, m07
/// substitutes a `Subject::Clause` complementizer for the gerund and stands
/// in two unregistered matrix predicates while not raising the shared
/// object, m09 substitutes a different embedded clause for the
/// possessed-name complement). Panics on an id this
/// witness does not cover, the same fail-loud posture [`realize_common`]
/// itself takes on an unconstructed predicate.
fn merchant_construction(id: &str) -> MerchantConstruction {
    match id {
        "m05" => MerchantConstruction::Clause(Clause {
            predicate: KILL.to_string(),
            subject: Subject::Name("Nwamvam".to_string()),
            object: Argument::Concept("person".to_string()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        }),
        // "I don't know why he killed her." The "why" is an indirect
        // question this campaign does not build (that is `wh-question`'s
        // job, still uncovered); the witness embeds the plain declarative
        // "he killed her" as `know`'s clause complement instead — "know
        // that-clause", not "know why-clause".
        "m06" => {
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
            MerchantConstruction::Clause(Clause {
                predicate: KNOW.to_string(),
                subject: Subject::Pronoun(Person::First),
                object: Argument::Clause(Box::new(embedded)),
                number: Number::Sg,
                definiteness: Definiteness::Def,
                evidential: Evidential::Witnessed,
                tense: Tense::Present,
                polarity: Polarity::Neg,
                adjuncts: Vec::new(),
            })
        }
        // "Seeing it confused and upset me." Per spec §9.1's own argument
        // for keeping m07 in scope: the gerund "Seeing it" is not
        // load-bearing for m07's THREE DEMANDS, only for its exact wording
        // — substitute the subject form and hold everything else constant,
        // "That he killed her confused me and upset me", and the same three
        // tokens (coordination, embedded-clause, pronoun-reference) are
        // satisfied through the complementizer-free `Subject::Clause`
        // machinery `a_clause_subject_realizes_through_the_same_machinery`
        // already pins. So EACH coordinated clause's subject is
        // `Subject::Clause`, not a bare pronoun — a bare-pronoun subject
        // would prove coordination and pronoun-reference but never exercise
        // embedded-clause at all, exactly the "Clause-shaped stand-in"
        // failure this file's own witness discipline forbids, one level
        // deeper than the Clause-vs-Coordination axis it was first written
        // against. `confuse`/`upset` are still not registered predicates in
        // this crate (see `two_clauses_coordinate_in_common`'s own doc), so
        // the witness stands in with two registered transitive predicates
        // for the MATRIX verbs only — the embedded clause itself ("he
        // killed her") uses the same `KILL` construction m06's embedding
        // does. Both matrix clauses share the identical embedded
        // `Subject::Clause`, so tier 2 (spec §4.10) elides it on the second
        // clause the same way a bare-pronoun subject elided before; tier 3
        // (right-node raising, which is what would collapse "and upset
        // **me**" onto one mention of the object) is still cut from this
        // campaign entirely, so both clauses restate the object in full.
        "m07" => {
            let embedded_subject = || Clause {
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
            MerchantConstruction::Coordination(Coordination {
                clauses: vec![
                    Clause {
                        predicate: KILL.to_string(),
                        subject: Subject::Clause(Box::new(embedded_subject())),
                        object: Argument::Pronoun(Person::First),
                        number: Number::Sg,
                        definiteness: Definiteness::Def,
                        evidential: Evidential::Witnessed,
                        tense: Tense::Past,
                        polarity: Polarity::Pos,
                        adjuncts: Vec::new(),
                    },
                    Clause {
                        predicate: KNOW.to_string(),
                        subject: Subject::Clause(Box::new(embedded_subject())),
                        object: Argument::Pronoun(Person::First),
                        number: Number::Sg,
                        definiteness: Definiteness::Def,
                        evidential: Evidential::Witnessed,
                        tense: Tense::Past,
                        polarity: Polarity::Pos,
                        adjuncts: Vec::new(),
                    },
                ],
            })
        }
        // "I think her name was Gilda." No construction here builds a
        // possessive noun phrase ("her name"), so the witness's object
        // stands in with an embedded clause rather than a possessed NP —
        // `think`'s complement is the whole proposition "her name was
        // Gilda", not the bare pronoun "her" (see MERCHANT_COVERED_IDS's
        // doc for why a bare pronoun would have been the wrong stand-in:
        // it would exercise epistemic-hedge only as a LEXICAL fact, never
        // the grammar the token actually names).
        "m09" => MerchantConstruction::Clause(Clause {
            predicate: THINK.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Clause(Box::new(Clause {
                predicate: KILL.to_string(),
                subject: Subject::Pronoun(Person::Third),
                object: Argument::Pronoun(Person::Third),
                number: Number::Sg,
                definiteness: Definiteness::Def,
                evidential: Evidential::Witnessed,
                tense: Tense::Past,
                polarity: Polarity::Pos,
                adjuncts: Vec::new(),
            })),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        }),
        "m10" => MerchantConstruction::Clause(Clause {
            predicate: KNOW.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Pronoun(Person::Third),
            number: Number::Sg,
            definiteness: Definiteness::Def,
            evidential: Evidential::Witnessed,
            tense: Tense::Past,
            polarity: Polarity::Neg,
            adjuncts: Vec::new(),
        }),
        other => panic!("merchant_construction has no construction for id {other:?}"),
    }
}

/// Realize whichever [`MerchantConstruction`] shape `id` builds, dispatching
/// to [`realize_common`] or [`realize_common_coordination`] as the shape
/// demands — see [`MerchantConstruction`]'s own doc for why this may not
/// collapse to one realizer.
fn realize_merchant(id: &str, vocab: &CommonVocabulary) -> String {
    match merchant_construction(id) {
        MerchantConstruction::Clause(clause) => realize_common(&clause, vocab),
        MerchantConstruction::Coordination(coord) => realize_common_coordination(&coord, vocab),
    }
}

/// The Common surface each covered entry ACTUALLY realizes, beside the
/// corpus's own English. Not an equality assertion against the corpus:
/// Common is a limited register, so a witness demanding the corpus's literal
/// text could never pass. What it proves is that a covered entry has a
/// constructible clause (or coordination) that realizes at all — the
/// mechanical half `IMPLEMENTED_DEMANDS` has never had.
const MERCHANT_WITNESS: &[(&str, &str)] = &[
    ("m05", "Nwamvam killed a person."),
    ("m06", "I does not know they killed them."),
    ("m07", "they killed them killed me and knowed me."),
    ("m09", "I thinked they killed them."),
    ("m10", "I did not know them."),
];

/// Every covered entry realizes. **This is the guard the module doc says does
/// not exist**, and it went red the first time it ran: m10 was scored covered
/// while `realize_common` panicked on `know`, which had no
/// `PREDICATE_VALENCE` row. A guard that has never been red proves nothing
/// about what it catches.
#[test]
fn every_covered_entry_realizes_in_common() {
    let witness_ids: Vec<&str> = MERCHANT_WITNESS.iter().map(|(id, _)| *id).collect();
    assert_eq!(
        witness_ids, MERCHANT_COVERED_IDS,
        "MERCHANT_WITNESS must name exactly the covered ids, or this test is \
         silently checking a different set than the resolver reports covered"
    );

    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    let vocab = CommonVocabulary::build(&registry).expect("the registry is sayable in Common");

    for (id, expected) in MERCHANT_WITNESS {
        let surface = realize_merchant(id, &vocab);
        assert_eq!(
            &surface, expected,
            "{id} realized a different surface than MERCHANT_WITNESS records"
        );
    }
}

/// The committed report path. Deliberately NOT declared BY NAME in
/// `docs/generated-paths.txt` — `docs/audits/` already declares the whole
/// directory this file is tracked inside, so a by-name entry would only
/// duplicate that coverage (`cli/tests/suite/generated_paths.rs`'s own
/// hazard is a NEW file dropped into an already-declared directory, and this
/// one predates that check, is tracked, and needs no second declaration).
///
/// **This WAS the `docs/audits/lexicon-inventory.tsv` vacuous-drift shape,
/// and no longer is (The Stile).** Until this campaign, nothing in
/// `scripts/regenerate-artifacts.sh` ever set `HV_SENTENCE_REBASELINE=1`, so
/// `git diff --exit-code -- docs/audits/` could only ever compare the
/// committed file against itself — a check with no writer in its own
/// regeneration path cannot fire on staleness, which is the exact shape "a
/// remedy is a claim" names: `make rebaseline` claiming to cover this path
/// asserted a write that never happened. `scripts/regenerate-artifacts.sh`
/// now runs the very test below under that env var (GROUP B+C, alongside the
/// other `docs/audits/` generators), so the drift check is real: an
/// unregenerated edit here, or a code change that should have moved this
/// file and did not, now shows up in `git diff -- docs/audits/` after `make
/// rebaseline` the same as every other artifact under that path. The
/// covered COUNT stays separately guarded, in Rust, against
/// [`MERCHANT_COVERED`] above — the file drifting is now a real, catchable
/// fact, but the number that matters was never trusted to the prose alone.
const REPORT_PATH: &str = "docs/audits/sentence-coverage.md";

/// Rewrite `docs/audits/sentence-coverage.md` under
/// `HV_SENTENCE_REBASELINE=1` — run automatically by
/// `scripts/regenerate-artifacts.sh` (`make rebaseline`) — otherwise a
/// no-op. The covered count itself is guarded independently, in Rust, by
/// [`merchant_coverage_is_five_of_twelve`], so a stale run of *this* test
/// cannot hide a coverage number that moved even before the drift check
/// above existed.
#[test]
fn sentence_coverage_report() {
    if std::env::var("HV_SENTENCE_REBASELINE").is_err() {
        return;
    }

    let root = repo_root();
    let merchant = load_merchant_corpus(&root);
    let flood_watch = load_flood_watch_corpus(&root);
    let ladder = load_ladder_corpus(&root);

    let merchant_directions = direction_counts(&merchant.entries);
    let flood_watch_directions = direction_counts(&flood_watch.entries);
    let ladder_directions = direction_counts(&ladder.entries);

    let ladder_raw = read_ladder_raw(&root.join("sentences/the-ladder.corpus.json.DRAFT"));
    let ladder_tokens = ladder_introduced_tokens(&ladder_raw);
    let merchant_tokens = distinct_demand_tokens(&merchant.entries);
    let merchant_absent = tokens_absent_from_ladder(&merchant_tokens, &ladder_tokens);
    let flood_watch_tokens = distinct_demand_tokens(&flood_watch.entries);
    let flood_watch_absent = tokens_absent_from_ladder(&flood_watch_tokens, &ladder_tokens);

    let mut tally: BTreeMap<String, usize> = BTreeMap::new();
    for entry in &merchant.entries {
        for demand in &entry.demands {
            *tally.entry(demand.clone()).or_insert(0) += 1;
        }
    }
    for demand in IMPLEMENTED_DEMANDS {
        tally.entry(demand.to_string()).or_insert(0);
    }

    let covered = merchant.entries.iter().filter(|e| entry_covered(e)).count();
    let not_yet = merchant.entries.len() - covered;

    let mut out = String::new();
    out.push_str(&format!(
        "# Sentence coverage\n\n\
         Generated by `cli/tests/suite/sentence_corpus.rs` under \
         `HV_SENTENCE_REBASELINE=1`, run automatically by \
         `scripts/regenerate-artifacts.sh` (`make rebaseline`). This file is \
         not declared BY NAME in `docs/generated-paths.txt`: `docs/audits/` \
         already declares the whole directory this report is tracked \
         inside, so `git diff --exit-code -- docs/audits/` catches an \
         unregenerated edit here — or a code change that should have moved \
         this file and did not — the same as anywhere else under that path. \
         The covered COUNT for the-merchant is separately guarded, in Rust, \
         against `MERCHANT_COVERED`.\n\n\
         Three corpora feed this report: `the-merchant` (12 entries, \
         frozen), `the-flood-watch` (139 entries, frozen) and `the-ladder` \
         ({} rungs, an unfrozen DRAFT). Only `the-merchant` is resolved \
         against the grammar below — `the-flood-watch` carries a `scene` \
         field the resolver's `Entry` shape does not need, and neither it \
         nor the ladder has a coverage score. Nothing is waiting on a \
         schema change: the entry shape that reads both corpora shipped; \
         what is absent is a coverage resolver over them, which is a \
         different and larger question (see this file's module doc and \
         `sentences/README.md`, \"Frozen is not the same as measured\"), \
         and the ladder is a production instrument, not dialogue, with no \
         resolver of its own either. What the report gives those two \
         corpora instead is a direction breakdown and a vocabulary \
         cross-check against the ladder, both below.\n\n\
         A demand is `covered` only if the grammar implements a \
         construction for it, and an entry is covered only if EVERY demand \
         it makes is. The Interlinear left one token covered (`classify`, \
         the \"X is a Y\" construction); The Inquest added `past-tense`, \
         `negation`, `transitive-frame` and `pronoun-reference`; The \
         Mortise added `coordination`, `embedded-clause` and \
         `epistemic-hedge`. Everything still uncovered names a grammatical \
         capability no campaign has built — questions, temporal adjuncts, \
         existentials, witness lists, named-entity lists. A **low score is \
         the expected result**, not a defect; the corpus is the program's \
         map, not any one campaign's scorecard.\n\n\
         Read the **distance** table below the merchant tally, not only the \
         headline count. Coverage is conjunctive and therefore lags: a \
         campaign can implement several tokens, move few or no entries to \
         covered, and still move several MORE entries from two missing \
         demands to one — progress the headline number cannot express.\n\n",
        ladder.entries.len(),
    ));

    out.push_str("## Direction breakdown\n\n");
    out.push_str(
        "Whether a corpus states what the grammar must **parse** (a player \
         line) or **produce** (an NPC line, or any ladder rung), per spec \
         §2.2/§2.3 — an absent direction is its own row, never inferred \
         from `speaker` or any other field.\n\n",
    );
    out.push_str("| corpus | parse | produce | unknown | total |\n");
    out.push_str("|---|---|---|---|---|\n");
    out.push_str(&format!(
        "| the-merchant | {} | {} | {} | {} |\n",
        merchant_directions.parse,
        merchant_directions.produce,
        merchant_directions.unknown,
        merchant.entries.len(),
    ));
    out.push_str(&format!(
        "| the-flood-watch | {} | {} | {} | {} |\n",
        flood_watch_directions.parse,
        flood_watch_directions.produce,
        flood_watch_directions.unknown,
        flood_watch.entries.len(),
    ));
    out.push_str(&format!(
        "| the-ladder (draft) | {} | {} | {} | {} |\n\n",
        ladder_directions.parse,
        ladder_directions.produce,
        ladder_directions.unknown,
        ladder.entries.len(),
    ));
    out.push_str(&format!(
        "`the-merchant` states no `direction` key at all, so all {} entries \
         land in `unknown` — that is **correct, not a gap**: the corpus \
         carries a `speaker` field (`\"player\"`/`\"merchant\"`) that looks \
         like a plausible stand-in, and spec §2.3 forbids inferring \
         direction from it. `the-flood-watch` states a direction on every \
         entry ({} player lines, {} NPC lines). `the-ladder` declares \
         itself a production instrument in its own `production_axis` \
         block, so every rung is prose the grammar must generate, never \
         player input it must parse.\n\n",
        merchant_directions.unknown, flood_watch_directions.parse, flood_watch_directions.produce,
    ));

    out.push_str("## the-merchant\n\n");
    out.push_str(&format!("- Total entries: {}\n", merchant.entries.len()));
    out.push_str(&format!("- Covered: {covered}\n"));
    out.push_str(&format!("- Not yet: {not_yet}\n\n"));

    out.push_str("### Per-entry\n\n");
    out.push_str("| id | speaker | text | demands | status |\n");
    out.push_str("|---|---|---|---|---|\n");
    for entry in &merchant.entries {
        let status = if entry_covered(entry) {
            "covered"
        } else {
            "not yet"
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {status} |\n",
            entry.id,
            entry.speaker.as_deref().unwrap_or("—"),
            entry.text.replace('|', "\\|"),
            entry.demands.join(", "),
        ));
    }
    out.push('\n');

    out.push_str("### Per-demand tally\n\n");
    out.push_str("| demand | entries | implemented? |\n");
    out.push_str("|---|---|---|\n");
    for (demand, n) in &tally {
        let implemented = if demand_covered(demand) { "yes" } else { "no" };
        out.push_str(&format!("| {demand} | {n} | {implemented} |\n"));
    }
    out.push('\n');

    // The distance report. Bucketed by how many demands an entry is short,
    // so the leading edge (distance 1) is legible without reading the
    // per-entry table above.
    let mut by_distance: BTreeMap<usize, Vec<&Entry>> = BTreeMap::new();
    for entry in &merchant.entries {
        by_distance
            .entry(missing_demands(entry).len())
            .or_default()
            .push(entry);
    }
    out.push_str("### Distance to covered\n\n");
    out.push_str(
        "How many demands each entry is SHORT. Distance 0 is the covered set \
         above. **Distance 1 is the leading indicator**: each of these entries \
         becomes covered the moment its one remaining token gains a \
         construction, so this is the cheapest statement of what the next \
         campaign should build.\n\n",
    );
    out.push_str("| distance | entries | which |\n");
    out.push_str("|---|---|---|\n");
    for (distance, entries) in &by_distance {
        let which = entries
            .iter()
            .map(|e| {
                let missing = missing_demands(e);
                if missing.is_empty() {
                    e.id.clone()
                } else {
                    format!("{} ({})", e.id, missing.join(" + "))
                }
            })
            .collect::<Vec<_>>()
            .join("; ");
        out.push_str(&format!("| {distance} | {} | {which} |\n", entries.len()));
    }
    out.push('\n');

    out.push_str("## the-flood-watch\n\n");
    out.push_str(&format!("- Total entries: {}\n", flood_watch.entries.len()));
    out.push_str(&format!(
        "- Direction: {} parse / {} produce (see the breakdown above)\n\n",
        flood_watch_directions.parse, flood_watch_directions.produce,
    ));
    out.push_str(&format!(
        "The resolver above does not run over this corpus — not because of \
         a schema change (there is none outstanding), but because no \
         coverage resolver has been written for it yet; see this file's \
         module doc and `sentences/README.md` (\"Frozen is not the same as \
         measured\"). What ties it to the grammar's delivered capability \
         instead is the vocabulary cross-check below: {} of its {} \
         distinct demand tokens name a rung on the ladder.\n\n",
        flood_watch_tokens.len() - flood_watch_absent.len(),
        flood_watch_tokens.len(),
    ));

    out.push_str("## the-ladder (draft)\n\n");
    out.push_str(&format!("- Total rungs: {}\n", ladder.entries.len()));
    out.push_str(&format!(
        "- Direction: {} produce (see the breakdown above)\n\n",
        ladder_directions.produce,
    ));
    out.push_str(
        "`the-ladder.corpus.json.DRAFT` is an unfrozen draft — this \
         campaign does not freeze it, and this report does not pin its \
         rung count the way `MERCHANT_ENTRIES`/`FLOOD_WATCH_ENTRIES` pin \
         the two dialogue corpora's. What holds instead are the structural \
         properties `cli/tests/suite/sentence_corpus.rs` asserts directly \
         over the ladder's `presupposes` graph (acyclic, ids unique, no \
         token introduced twice, exactly two roots) and the vocabulary \
         cross-check below.\n\n",
    );

    out.push_str("## Cross-check: ladder vocabulary against the two dialogue corpora\n\n");
    out.push_str(
        "Whether every distinct demand token a corpus makes names at least \
         one rung on the ladder — the ladder's own `introduces` \
         vocabulary, not any one rung's transitive closure.\n\n",
    );
    out.push_str(&format!(
        "- **the-merchant: {} of {}.** Every demand token the corpus makes \
         has a rung.\n",
        merchant_tokens.len() - merchant_absent.len(),
        merchant_tokens.len(),
    ));
    out.push_str(&format!(
        "- **the-flood-watch: {} of {}.** Two tokens are **refused**, not \
         missing:\n",
        flood_watch_tokens.len() - flood_watch_absent.len(),
        flood_watch_tokens.len(),
    ));
    for token in &flood_watch_absent {
        out.push_str(&format!("  - `{token}`\n"));
    }
    out.push_str(
        "\n  Both are input-surface properties of `parse` entries — every \
         player line in the corpus is lowercase and unterminated, the \
         actual input surface a real player types — while every ladder \
         rung is well-formed prose, because the ladder is a production \
         instrument (its own `production_axis` block states this). A \
         production instrument has no rung that could ever introduce \
         either token, so their absence is a structural fact about what \
         the ladder is FOR, not a gap nobody has closed yet.\n",
    );

    std::fs::write(root.join(REPORT_PATH), out).expect(REPORT_PATH);
}
