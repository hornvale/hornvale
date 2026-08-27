//! The sentence corpus is frozen: its entry count is asserted, so growing or
//! trimming it is a deliberate act rather than a drift.
//!
//! This file also carries the corpus's **resolver**: for each entry, a demand
//! token is `covered` if the grammar implements a construction for it, `not
//! yet` otherwise. The Interlinear left exactly one token covered
//! (`classify`, the "X is a Y" construction domains/language realizes through
//! the fact-shaped `Clause`); The Inquest added four more — `past-tense`,
//! `negation`, `transitive-frame` and `pronoun-reference` — taking the
//! merchant corpus from 0 of 12 to 2 of 12. The remaining tokens name
//! capabilities no campaign has built: questions, embedded clauses,
//! coordination, temporal adjuncts, epistemic hedges, existentials, witness
//! lists, named-entity lists.
//!
//! **[`IMPLEMENTED_DEMANDS`] is a hand-maintained declaration and nothing
//! mechanically proves it.** No test crosses a token in that list against the
//! construction that is supposed to realize it, so a token added on optimism
//! moves the score without moving the grammar — which would make the
//! instrument worse than no instrument, because it would read as evidence.
//! The discipline is therefore social and stated here: a token goes in only
//! alongside a test in `domains/language` that realizes a clause exercising
//! it, in Common and, where the tongue realizer is the point, in a tongue.
//! The five present tokens are backed by, respectively:
//! `clause.rs::classify_*`; `a_past_clause_says_was` and
//! `grammar.rs::realize_tongue_reads_its_drawn_tense_depth`;
//! `a_negated_clause_says_is_not` and
//! `grammar.rs::realize_tongue_reads_its_drawn_polarity_depth`;
//! `a_transitive_clause_surfaces_its_predicate_as_a_verb` and
//! `grammar.rs::realize_tongue_exhaustive_orders_for_a_transitive_clause`;
//! `common_realizes_a_pronoun_subject_and_a_pronoun_object` and
//! `grammar.rs::a_tongue_realizes_a_pronoun_subject_from_its_own_drawn_inventory`.
//!
//! **The score was zero once, and the positive control that answered that is
//! still here on purpose.** A measurement whose only possible answer is zero
//! cannot tell a working resolver from one hardcoded to return "not yet"
//! unconditionally — the same defect class as a guard that has never gone
//! red. That specific blind spot closed when the score moved off zero: a
//! resolver stuck at "not yet" now fails
//! [`merchant_coverage_is_two_of_twelve`] directly. The control keeps its
//! place because it guards a *different* thing from the headline test — it
//! is the only assertion here that survives the corpus being replaced, and
//! the corpus is frozen data a future campaign is expected to swap. See its
//! own doc comment.

use std::collections::BTreeMap;
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

// ---------------------------------------------------------------------
// Task 9: the resolver
// ---------------------------------------------------------------------

/// Demand tokens the grammar implements a construction for. Extend this list
/// only when a token genuinely gains a construction, in the same commit that
/// moves [`MERCHANT_COVERED`] — the same discipline [`MERCHANT_ENTRIES`]
/// enforces on the corpus itself.
///
/// **Nothing mechanically checks a row here against the grammar**; see the
/// module doc, which names the test backing each of the five.
const IMPLEMENTED_DEMANDS: &[&str] = &[
    // The Interlinear.
    "classify",
    // The Inquest.
    "negation",
    "past-tense",
    "pronoun-reference",
    "transitive-frame",
];

/// One corpus entry, as read from `sentences/*.corpus.json`. Only the fields
/// the resolver and the report need; unrecognized JSON fields (`name`,
/// `provenance` at the document root) are ignored by default.
#[derive(serde::Deserialize)]
struct Entry {
    id: String,
    speaker: String,
    text: String,
    demands: Vec<String>,
}

/// The corpus document's shape: only what the resolver reads.
#[derive(serde::Deserialize)]
struct Corpus {
    entries: Vec<Entry>,
}

fn load_merchant_corpus(root: &Path) -> Corpus {
    let text = std::fs::read_to_string(root.join("sentences/the-merchant.corpus.json"))
        .expect("the merchant corpus is committed");
    serde_json::from_str(&text).expect("the merchant corpus parses as the frozen shape")
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

/// The frozen result of resolving the-merchant against the grammar's
/// delivered capability, independent of the prose report below — so a stale
/// report cannot hide a coverage number that moved.
const MERCHANT_COVERED: usize = 2;

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
const MERCHANT_COVERED_IDS: &[&str] = &["m05", "m10"];

/// The headline score.
///
/// **The name states the number, so it moves when the number moves** — and
/// that is a commit-gate change, not a cosmetic one:
/// `docs/timings/subfloor-roster.tsv` selects tests by EXACT name, so a
/// rename that does not also edit that file silently drops this test from
/// `make gate-commit`. Edit both, in the same commit.
#[test]
fn merchant_coverage_is_two_of_twelve() {
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
/// [`merchant_coverage_is_two_of_twelve`]: the count and the identities can
/// each move without the other, and only holding both pins the claim the
/// campaign actually makes.
#[test]
fn the_covered_entries_are_m05_and_m10() {
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
         a woman.\") needs transitive-frame + past-tense; m10 (\"I didn't know \
         her.\") needs negation + past-tense + pronoun-reference."
    );
}

/// The entries sitting at exactly ONE missing demand, each with the token
/// that blocks it — the frozen distance report.
///
/// Every row here is one construction away from covered, so this list is the
/// cheapest available statement of what the next campaign should build:
/// `polar-question` and `temporal-adverbial` each unlock one entry from here
/// and appear twice more elsewhere in the corpus.
const MERCHANT_ONE_MISSING: &[(&str, &str)] = &[
    ("m01", "wh-question"),
    ("m02", "temporal-adverbial"),
    ("m08", "polar-question"),
    ("m09", "epistemic-hedge"),
];

/// Spec criterion 7: the resolver reports distance, not only coverage.
#[test]
fn four_entries_sit_at_one_missing_demand() {
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
/// that hardcoding left `merchant_coverage_is_zero_of_twelve` green, because
/// zero was the expected answer. The Inquest moved the score to 2, so a
/// hardcoded "not yet" now reds the headline test on its own and this control
/// is no longer load-bearing for THAT reading.
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
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string()],
    };
    assert!(
        entry_covered(&synthetic),
        "a demand-only-classify entry must resolve as covered; if this fails, \
         the resolver has regressed to reporting \"not yet\" unconditionally"
    );
}

/// A demand token the negative controls below can rely on being *un*covered.
///
/// **This is a maintenance obligation, and it has already come due once.**
/// Both controls named `"negation"` until The Inquest implemented negation,
/// at which point they went red — correctly, but with a bare
/// `assertion failed` that said nothing about why. Naming the token once,
/// here, with [`the_negative_control_token_is_genuinely_uncovered`] asserting
/// the property the controls depend on, turns that rot into a failure that
/// explains itself. `embedded-clause` is the pick because spec §7 identifies
/// it as the structural next ceiling — a `Clause` has no field pointing at
/// another `Clause` — so it will outlast the tokens around it.
const UNCOVERED_TOKEN: &str = "embedded-clause";

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
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec![UNCOVERED_TOKEN.to_string()],
    };
    assert!(!entry_covered(&synthetic));
}

/// An entry is covered only when EVERY demand it makes is covered — one
/// implemented demand alongside one unimplemented demand must not count.
#[test]
fn a_mixed_entry_needs_every_demand_covered() {
    let synthetic = Entry {
        id: "synthetic-mixed".to_string(),
        speaker: "player".to_string(),
        text: "(synthetic, never in the frozen corpus)".to_string(),
        demands: vec!["classify".to_string(), UNCOVERED_TOKEN.to_string()],
    };
    assert!(!entry_covered(&synthetic));
}

/// The committed report path. Deliberately NOT declared BY NAME in
/// `docs/generated-paths.txt`: nothing in `scripts/regenerate-artifacts.sh`
/// writes it (the "regenerate-artifacts.sh writes it" criterion that would
/// earn a by-name entry), so it follows the two-day-old
/// `docs/audits/lexicon-inventory.tsv` precedent instead — rewritten under
/// an env var, with the number that matters guarded in Rust
/// (`MERCHANT_COVERED` above) rather than by diffing this file. **It is not
/// drift-check-vacuous, though** — `docs/generated-paths.txt` declares the
/// whole `docs/audits/` directory, and this file is tracked, so `git diff
/// --exit-code -- docs/audits/` already covers it: an edit here that is not
/// also committed reddens `make rebaseline`'s drift check like any other
/// file under that directory.
const REPORT_PATH: &str = "docs/audits/sentence-coverage.md";

/// Rewrite `docs/audits/sentence-coverage.md` under
/// `HV_SENTENCE_REBASELINE=1`; otherwise a no-op (the covered count is
/// guarded by `merchant_coverage_is_zero_of_twelve` regardless of whether
/// this prose file is current).
#[test]
fn sentence_coverage_report() {
    if std::env::var("HV_SENTENCE_REBASELINE").is_err() {
        return;
    }

    let root = repo_root();
    let corpus = load_merchant_corpus(&root);

    let mut tally: BTreeMap<String, usize> = BTreeMap::new();
    for entry in &corpus.entries {
        for demand in &entry.demands {
            *tally.entry(demand.clone()).or_insert(0) += 1;
        }
    }
    for demand in IMPLEMENTED_DEMANDS {
        tally.entry(demand.to_string()).or_insert(0);
    }

    let covered = corpus.entries.iter().filter(|e| entry_covered(e)).count();
    let not_yet = corpus.entries.len() - covered;

    let mut out = String::new();
    out.push_str(
        "# Sentence coverage — the-merchant\n\n\
         Generated by `cli/tests/suite/sentence_corpus.rs` under \
         `HV_SENTENCE_REBASELINE=1`. This file is not declared BY NAME in \
         `docs/generated-paths.txt` (the `docs/audits/lexicon-inventory.tsv` \
         precedent — nothing regenerates it on its own) — but it IS \
         drift-checked, because that file also declares the whole `docs/audits/` \
         directory this report is tracked inside: `git diff --exit-code -- \
         docs/audits/` catches an unregenerated edit here the same as \
         anywhere else under that path. The covered COUNT is separately \
         guarded, in Rust, against `MERCHANT_COVERED`.\n\n\
         A demand is `covered` only if the grammar implements a construction \
         for it, and an entry is covered only if EVERY demand it makes is. \
         The Interlinear left one token covered (`classify`, the \"X is a \
         Y\" construction); The Inquest added `past-tense`, `negation`, \
         `transitive-frame` and `pronoun-reference`. Everything still \
         uncovered names a grammatical capability no campaign has built — \
         questions, embedded clauses, coordination, temporal adjuncts, \
         epistemic hedges, existentials, witness lists, named-entity \
         lists. A **low score is the expected result**, not a defect; the \
         corpus is the program's map, not any one campaign's scorecard.\n\n\
         Read the **distance** table below the tally, not only the headline \
         count. Coverage is conjunctive and therefore lags: The Inquest \
         implemented four tokens, moved two entries to covered, and moved \
         four MORE entries from two missing demands to one — progress the \
         headline number cannot express.\n\n",
    );

    out.push_str("## the-merchant\n\n");
    out.push_str(&format!("- Total entries: {}\n", corpus.entries.len()));
    out.push_str(&format!("- Covered: {covered}\n"));
    out.push_str(&format!("- Not yet: {not_yet}\n\n"));

    out.push_str("### Per-entry\n\n");
    out.push_str("| id | speaker | text | demands | status |\n");
    out.push_str("|---|---|---|---|---|\n");
    for entry in &corpus.entries {
        let status = if entry_covered(entry) {
            "covered"
        } else {
            "not yet"
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {status} |\n",
            entry.id,
            entry.speaker,
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
    for entry in &corpus.entries {
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

    std::fs::write(root.join(REPORT_PATH), out).expect(REPORT_PATH);
}
