//! The realization witness (decision 0577): `Stageable` requires a
//! committed tableau that actually stages a situation's actants and every
//! relation it stipulates — not merely a corpus token that names a registry
//! entry. See spec §4.2 for the design and `docs/decisions/0330-*.md` for
//! the sibling precedent (`sentence_corpus.rs`'s `MERCHANT_WITNESS`) this
//! follows.
//!
//! **Step 1's captured red is preserved as the first test in this file**
//! (`a_situation_with_no_witness_cannot_be_stageable`), run against
//! `tropes::resolve`'s UNMODIFIED two-argument signature before this
//! campaign touched `cli/src/tropes.rs` at all — see `task-4-report.md` for
//! the transcript. On that surface a situation with an empty `requires`
//! list (so the token check trivially passes) resolved `Stageable`
//! unconditionally: no tableau was ever attempted, no actant was ever
//! placed. That is the defect 0577 closes, and it was a real assertion
//! failure on the live surface, not a compile error standing in for one.

use hornvale::provision::Provision;
use hornvale::tropes::{Corpus, Outcome, Situation, resolve, witness_stages, witnesses};
use hornvale_kernel::{ConceptRegistry, Seed, World};
use hornvale_vessel::{PossessOpts, Session, Tableau};
use std::collections::{BTreeMap, BTreeSet};

/// A world built once per test, the same way `windows/vessel`'s own tableau
/// suite does (`common::build(42)`) — seed 42 is known to carry a
/// settlement, which `Session::start` requires regardless of tableau.
fn a_world() -> World {
    hornvale_worldgen::build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
}

/// A one-situation corpus requiring exactly the tokens the caller passes,
/// with no bundle or exclusion machinery — the same shape
/// `cli/tests/suite/provision.rs::a_corpus` uses, so a reader who already
/// knows that fixture recognizes this one.
fn a_corpus(id: &str, requires: Vec<String>) -> Corpus {
    Corpus {
        corpus: "witness-test".to_string(),
        provenance: "test fixture, not a frozen corpus".to_string(),
        frozen: "n/a".to_string(),
        bundles: BTreeMap::new(),
        situations: vec![Situation {
            id: id.to_string(),
            name: "test situation".to_string(),
            actants: BTreeMap::new(),
            requires,
            excluded_by: Vec::new(),
        }],
    }
}

fn repo_path(rel: &str) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .join(rel)
}

// ---------------------------------------------------------------------
// Requirement 1: a situation with no witness cannot be `Stageable`,
// whatever the provision table says.
// ---------------------------------------------------------------------

/// **Step 1's captured red, preserved as the permanent regression test.** A
/// situation with an empty `requires` list passes the token check by
/// construction (nothing is missing). Today, with the witness gate wired,
/// it must still refuse `Stageable` because no witness names it.
#[test]
fn a_situation_with_no_witness_cannot_be_stageable() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus("s1", vec![]);
    let out = resolve(&corpus, &registry, &world, &witnesses());
    assert_ne!(
        out.get("s1"),
        Some(&Outcome::Stageable),
        "a situation with no registered witness must never resolve Stageable, \
         regardless of how empty its requires list is: {out:?}"
    );
}

/// The same claim, restated over a witness table that is non-empty but
/// simply does not name this situation — so a reader cannot mistake
/// `a_situation_with_no_witness_cannot_be_stageable` for a fluke of an empty
/// table.
#[test]
fn a_witness_table_with_other_entries_still_refuses_an_unlisted_situation() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus("unlisted", vec![]);
    let mut table = BTreeMap::new();
    table.insert("some-other-situation".to_string(), Tableau::new());
    let out = resolve(&corpus, &registry, &world, &table);
    assert_ne!(
        out.get("unlisted"),
        Some(&Outcome::Stageable),
        "a witness table naming other situations must not accidentally admit \
         one it never declared: {out:?}"
    );
}

// ---------------------------------------------------------------------
// Requirement 2: a witness whose tableau fails to stage the actants is
// refused.
// ---------------------------------------------------------------------

/// A `StagedThing` naming a `held_by` index the cast never places is a
/// staging failure independent of `relations` — the prop cannot be placed
/// in anyone's hands, so the whole tableau is refused before any fact is
/// ever committed. `Session::start` surfaces this as `Err`, and `resolve`
/// must treat that refusal the same as an absent witness: `Blocked`, never
/// `Stageable`.
#[test]
fn a_witness_whose_tableau_fails_to_stage_the_actants_is_refused() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus("s2", vec![]);
    let mut table = BTreeMap::new();
    table.insert(
        "s2".to_string(),
        Tableau::new()
            .with_cast(["goblin"])
            // Only one cast member exists (index 0); a thing held by index 7
            // names nobody the tableau ever placed.
            .with_thing("key", 7),
    );
    let out = resolve(&corpus, &registry, &world, &table);
    assert_ne!(
        out.get("s2"),
        Some(&Outcome::Stageable),
        "a tableau that fails to stage (an out-of-range holder) must be \
         refused, not silently accepted: {out:?}"
    );
    assert_eq!(
        witness_stages("s2", &world, &table),
        Err("witness:refused"),
        "the underlying witness check must itself refuse this tableau"
    );
}

// ---------------------------------------------------------------------
// Requirement 3: a witness whose relations do not all resolve is refused.
// ---------------------------------------------------------------------

/// A relation naming a predicate the concept registry does not hold cannot
/// be committed (`Ledger::check`'s `UnknownPredicate`), even though both
/// cast indices it names are real. This is a distinct failure mode from
/// requirement 2's: the cast stages fine, and the relation is the thing
/// that does not resolve.
#[test]
fn a_witness_whose_relations_do_not_all_resolve_is_refused() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus("s3", vec![]);
    let mut table = BTreeMap::new();
    table.insert(
        "s3".to_string(),
        Tableau::new().with_cast(["goblin", "drow"]).with_relation(
            "no-such-predicate-anywhere",
            0,
            1,
        ),
    );
    let out = resolve(&corpus, &registry, &world, &table);
    assert_ne!(
        out.get("s3"),
        Some(&Outcome::Stageable),
        "a relation naming an unregistered predicate must refuse the whole \
         witness, not be silently dropped: {out:?}"
    );
}

/// The same failure, confirmed directly against `Session::start` rather
/// than through `resolve` — this is the mechanism `witness_stages` and
/// `resolve` both lean on, pinned in isolation so a future change to
/// `resolve`'s plumbing cannot quietly stop exercising it.
#[test]
fn session_start_itself_refuses_an_unregistered_relation_predicate() {
    let world = a_world();
    let opts = PossessOpts {
        tableau: Some(Tableau::new().with_cast(["goblin", "drow"]).with_relation(
            "no-such-predicate-anywhere",
            0,
            1,
        )),
        ..PossessOpts::default()
    };
    let err = Session::start(&world, &opts)
        .err()
        .expect("an unregistered relation predicate must refuse to stage");
    let message = format!("{err}");
    assert!(
        message.contains("no-such-predicate-anywhere"),
        "the refusal should name the offending predicate: {message}"
    );
}

// ---------------------------------------------------------------------
// The positive case, and requirement 4: the witness roster and the
// `Stageable` set are equal in membership.
// ---------------------------------------------------------------------

/// A witness that stages successfully AND whose situation's tokens all
/// resolve is exactly what makes a situation `Stageable` — the positive
/// case, so the negative ones above are not the only shape this resolver
/// can produce.
#[test]
fn a_witness_that_stages_and_whose_tokens_resolve_is_stageable() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate("instance-of", true, "test predicate")
        .expect("registers");
    let world = a_world();
    let corpus = a_corpus("s4", vec!["predicate:instance-of".to_string()]);
    let mut table = BTreeMap::new();
    table.insert(
        "s4".to_string(),
        Tableau::new()
            .with_cast(["goblin", "drow"])
            .with_relation("instance-of", 0, 1),
    );
    let out = resolve(&corpus, &registry, &world, &table);
    assert_eq!(
        out.get("s4"),
        Some(&Outcome::Stageable),
        "a witness that stages and whose tokens resolve must be Stageable: {out:?}"
    );
    assert_eq!(witness_stages("s4", &world, &table), Ok(()));
}

/// **The real corpora.** On both frozen catalogues today, the witness
/// roster (`tropes::witnesses`) is empty and the `Stageable` set is empty —
/// see spec §4.2's "migration cost is zero" note. This asserts the two are
/// equal in MEMBERSHIP, not merely both zero in COUNT, so a future campaign
/// that authors a witness for a situation whose tokens do not yet resolve
/// (or vice versa) is caught here rather than discovered by reading the
/// report by eye.
/// claim: structural(both frozen corpora) — false-positive seed-loop flag;
/// `path` ranges over `tropes::CORPORA`, a two-element ALL-CAPS constant
/// naming corpus FILES, not a seed sweep. The loop is over a frozen,
/// exhaustively enumerated set (decision 0016), not a sample of a larger
/// population, so there is no quantifier here for `rate`/`invariant` to
/// bind against.
#[test]
fn the_witness_roster_and_the_stageable_set_agree_on_both_frozen_corpora() {
    let world = a_world();
    let table = witnesses();
    for path in hornvale::tropes::CORPORA {
        let json =
            std::fs::read_to_string(repo_path(path)).unwrap_or_else(|e| panic!("{path}: {e}"));
        let corpus = hornvale::tropes::load(&json).expect("a frozen corpus parses");
        let out = resolve(&corpus, &world.registry, &world, &table);

        let stageable: BTreeSet<&str> = out
            .iter()
            .filter(|(_, o)| **o == Outcome::Stageable)
            .map(|(id, _)| id.as_str())
            .collect();
        let corpus_ids: BTreeSet<&str> = corpus.situations.iter().map(|s| s.id.as_str()).collect();
        let witnessed: BTreeSet<&str> = table
            .keys()
            .map(String::as_str)
            .filter(|id| corpus_ids.contains(id))
            .collect();
        assert_eq!(
            stageable, witnessed,
            "{}: the Stageable set and the witness roster must agree in \
             membership — neither may silently outgrow the other",
            corpus.corpus
        );
    }
}

// ---------------------------------------------------------------------
// `witness_stages` in isolation — the function `resolve` delegates to.
// ---------------------------------------------------------------------

/// A situation not present in the witness table is `Err("witness:absent")`.
#[test]
fn witness_stages_reports_absent_for_an_unlisted_situation() {
    let world = a_world();
    let empty = BTreeMap::new();
    assert_eq!(
        witness_stages("no-such-id", &world, &empty),
        Err("witness:absent")
    );
}

/// A witness whose staging fails is `Err("witness:refused")`, distinct from
/// `"witness:absent"` — a reader of a `Blocked` reason can tell "nobody
/// wrote one" from "one was written and it does not work".
#[test]
fn witness_stages_reports_refused_for_a_witness_that_fails_to_stage() {
    let world = a_world();
    let mut table = BTreeMap::new();
    table.insert(
        "bad".to_string(),
        Tableau::new()
            .with_cast(["goblin"])
            .with_relation("no-such-predicate-anywhere", 0, 0),
    );
    assert_eq!(
        witness_stages("bad", &world, &table),
        Err("witness:refused")
    );
}

/// A little of `Provision` too, to confirm this file's fixture registry
/// convention agrees with the one `provision.rs` already established: a
/// declared-but-empty registry is what makes `s1`'s requires list resolve
/// with nothing missing.
#[test]
fn an_empty_requires_list_needs_no_provision_row() {
    let registry = ConceptRegistry::default();
    let table = Provision::from_registry(&registry);
    assert!(!table.serves("predicate:anything", &registry));
}
