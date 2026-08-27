//! The sentence corpus is frozen: its entry count is asserted, so growing or
//! trimming it is a deliberate act rather than a drift.
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

use hornvale_kernel::ConceptRegistry;
use hornvale_language::packs::{KILL, KNOW, THINK};
use hornvale_language::{
    Argument, Clause, CommonVocabulary, Coordination, Definiteness, Evidential, Number, Person,
    Polarity, Subject, Tense, realize_common, realize_common_coordination,
};
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
/// - **m09, *"I think her name was Gilda."*, drops the possessive.** Its
///   demands (`epistemic-hedge`, `past-tense`, `pronoun-reference`) build a
///   hedged transitive clause, not a possessed-noun-phrase complement
///   ("her name") — no construction here builds a possessive, so the
///   witness's object is a bare pronoun standing in for "her".
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
/// object, m09 drops the possessive "her name"). Panics on an id this
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
        // possessive noun phrase ("her name"), so the witness's object is a
        // bare pronoun standing in for "her" rather than "her name".
        "m09" => MerchantConstruction::Clause(Clause {
            predicate: THINK.to_string(),
            subject: Subject::Pronoun(Person::First),
            object: Argument::Pronoun(Person::Third),
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
    ("m09", "I thinked them."),
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
