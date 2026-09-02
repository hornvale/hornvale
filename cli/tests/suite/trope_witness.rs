//! The realization witness (decisions 0577/0582): `Stageable` requires a
//! committed tableau that actually stages a situation's actants and every
//! relation it stipulates, AND that tableau's relations are mechanically
//! BOUND to the situation it is filed under — not merely a corpus token
//! that names a registry entry, and not merely a tableau filed under some
//! id or other. See spec §4.2 for the design and `docs/decisions/0330-*.md`
//! for the sibling precedent (`sentence_corpus.rs`'s `MERCHANT_WITNESS`)
//! this follows.
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
//!
//! **Fix round 1 (decision 0581) adds the binding tests** (F1 of the first
//! review): a tableau's staged relations must name predicates the situation
//! ITSELF requires, or it is refused as `"witness:unbound"` even when it
//! would stage successfully. `a_tableau_bound_under_one_situation_is_unbound_
//! under_another_that_lacks_its_predicate` is the property test proving
//! that a witness filed for one situation cannot silently witness another.
//!
//! **Fix round 2 (decision 0582) makes the binding BIDIRECTIONAL.** Round
//! 1's `witness_binds` only ever checked that the tableau's relations were
//! a SUBSET of the situation's required predicates — `all()` over an EMPTY
//! relations list is vacuously `true`, so a review probe built a
//! two-creature, ZERO-relation tableau, filed it under a situation
//! requiring five predicate tokens, and it bound and staged.
//! `a_relation_less_tableau_does_not_bind_to_a_situation_requiring_
//! predicates` reproduces that exact probe; `a_tableau_covering_only_some_
//! required_predicates_is_unbound` covers the same family's non-empty
//! member (a tableau relating by only SOME of what the situation
//! requires). Both were run against the round-1 (one-directional) check
//! via `scripts/mutate.py` and confirmed genuinely red before this fix
//! landed — see `task-4-report.md`.

use hornvale::provision::Provision;
use hornvale::tropes::{
    Corpus, Outcome, Situation, WitnessEntry, resolve, witness_stages, witnesses,
};
use hornvale_kernel::{ConceptRegistry, Seed, World};
use hornvale_vessel::{PossessOpts, Session, Tableau};
use std::collections::{BTreeMap, BTreeSet};

/// A world built once for this test binary and shared — seed 42 is known
/// to carry a settlement, which `Session::start` requires regardless of
/// tableau, and building one per test (9 of the 10 tests here need one)
/// bought nothing over a single shared build.
fn a_world() -> &'static World {
    static WORLD: std::sync::OnceLock<World> = std::sync::OnceLock::new();
    WORLD.get_or_init(|| {
        hornvale_worldgen::build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )
        .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
    })
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
        situations: vec![a_situation(id, requires)],
    }
}

/// A bare `Situation`, for constructing multi-situation corpora by hand.
fn a_situation(id: &str, requires: Vec<String>) -> Situation {
    Situation {
        id: id.to_string(),
        name: "test situation".to_string(),
        actants: BTreeMap::new(),
        requires,
        excluded_by: Vec::new(),
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
    let out = resolve(&corpus, &registry, world, &witnesses());
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
    table.insert(
        "some-other-situation".to_string(),
        WitnessEntry::new(Tableau::new(), "empty tableau, filed under a different id"),
    );
    let out = resolve(&corpus, &registry, world, &table);
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
///
/// This tableau stages NO relations, so [`hornvale::tropes::resolve`]'s
/// binding check (F1) passes vacuously — the failure this test pins is
/// squarely the actant/prop staging failure, not a binding refusal.
#[test]
fn a_witness_whose_tableau_fails_to_stage_the_actants_is_refused() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus("s2", vec![]);
    let mut table = BTreeMap::new();
    table.insert(
        "s2".to_string(),
        WitnessEntry::new(
            Tableau::new()
                .with_cast(["goblin"])
                // Only one cast member exists (index 0); a thing held by
                // index 7 names nobody the tableau ever placed.
                .with_thing("key", 7),
            "deliberately broken: a prop held by a cast index that was never staged",
        ),
    );
    let out = resolve(&corpus, &registry, world, &table);
    assert_ne!(
        out.get("s2"),
        Some(&Outcome::Stageable),
        "a tableau that fails to stage (an out-of-range holder) must be \
         refused, not silently accepted: {out:?}"
    );
    assert_eq!(
        witness_stages(&corpus, &corpus.situations[0], world, &table),
        Err("witness:refused"),
        "the underlying witness check must itself refuse this tableau"
    );
}

// ---------------------------------------------------------------------
// Requirement 3: a witness whose relations do not all resolve is refused.
// ---------------------------------------------------------------------

/// A relation naming a predicate the REAL WORLD's registry does not hold
/// cannot be committed (`Ledger::check`'s `UnknownPredicate`), even though
/// the situation's own token check resolves it (a separate, local
/// registry declares the predicate present — `resolve`'s token check and
/// `witness_stages`'s staging check consult different registries by
/// design, see `cli/src/tropes.rs::resolve`'s own doc) and the relation's
/// predicate matches the situation's sole required token, so the binding
/// check (F1) passes: this isolates the STAGING failure from both a
/// binding refusal and a token-check refusal.
#[test]
fn a_witness_whose_relations_do_not_all_resolve_is_refused() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(
            "no-such-predicate-anywhere",
            false,
            "declared for the token check only",
        )
        .expect("registers");
    let world = a_world();
    let corpus = a_corpus(
        "s3",
        vec!["predicate:no-such-predicate-anywhere".to_string()],
    );
    let mut table = BTreeMap::new();
    table.insert(
        "s3".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin", "drow"]).with_relation(
                "no-such-predicate-anywhere",
                0,
                1,
            ),
            "deliberately broken: relates by a predicate the REAL world never registers, \
             even though a separate token-check registry claims it and the situation \
             requires it — isolates a pure staging failure from a binding refusal",
        ),
    );
    let out = resolve(&corpus, &registry, world, &table);
    assert_ne!(
        out.get("s3"),
        Some(&Outcome::Stageable),
        "a relation naming a predicate the world does not register must refuse the \
         whole witness, not be silently dropped: {out:?}"
    );
    assert_eq!(
        witness_stages(&corpus, &corpus.situations[0], world, &table),
        Err("witness:refused"),
        "this must be a STAGING refusal, not a binding refusal — the relation's \
         predicate does match the situation's required token"
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
    let err = Session::start(world, &opts)
        .err()
        .expect("an unregistered relation predicate must refuse to stage");
    let message = format!("{err}");
    assert!(
        message.contains("no-such-predicate-anywhere"),
        "the refusal should name the offending predicate: {message}"
    );
}

// ---------------------------------------------------------------------
// F1: the witness must be mechanically BOUND to the situation it is
// filed under, not merely present under its id.
// ---------------------------------------------------------------------

/// **The core F1 property.** The identical tableau, filed under two
/// situations that ask for different relation predicates: bound (and
/// Stageable) under the one that requires what it relates by, refused as
/// `"witness:unbound"` under the one that does not — proving a witness
/// filed for situation A cannot silently witness situation B.
#[test]
fn a_tableau_bound_under_one_situation_is_unbound_under_another_that_lacks_its_predicate() {
    let world = a_world();
    let corpus = Corpus {
        corpus: "witness-test".to_string(),
        provenance: "test fixture, not a frozen corpus".to_string(),
        frozen: "n/a".to_string(),
        bundles: BTreeMap::new(),
        situations: vec![
            a_situation("s5", vec!["predicate:instance-of".to_string()]),
            a_situation("s6", vec![]),
        ],
    };
    let tableau = Tableau::new().with_cast(["goblin", "drow"]).with_relation(
        hornvale_kernel::INSTANCE_OF,
        0,
        1,
    );
    let mut table = BTreeMap::new();
    table.insert(
        "s5".to_string(),
        WitnessEntry::new(
            tableau.clone(),
            "exact: relates the two staged cast members by instance-of, matching s5's \
             sole required token one-for-one",
        ),
    );
    table.insert(
        "s6".to_string(),
        WitnessEntry::new(
            tableau,
            "identical tableau content to s5's witness, filed here on purpose to \
             demonstrate the binding refusal: s6 requires no predicate token at all, \
             so the same relation cannot bind to it",
        ),
    );

    let out = resolve(&corpus, &world.registry, world, &table);
    assert_eq!(
        out.get("s5"),
        Some(&Outcome::Stageable),
        "s5 requires instance-of and the witness relates by instance-of: {out:?}"
    );
    assert_eq!(
        out.get("s6"),
        Some(&Outcome::Blocked(vec!["witness:unbound".to_string()])),
        "the SAME tableau filed under s6 must be refused as unbound, since s6's \
         requires never names instance-of: {out:?}"
    );
}

/// **The exact probe the review built to reopen F1.** A cast with no
/// relations at all, filed under a situation requiring FIVE predicate
/// tokens it never touches. Before decision 0582's bidirectional check,
/// `witness_binds` was `tableau.relations.iter().all(...)`, and `all()`
/// over an EMPTY iterator is `true` regardless of what the situation
/// requires — this resolved `Stageable` and `witness_stages` returned
/// `Ok(())`. This is not the degenerate edge of the binding hole; the
/// review's own framing is that it IS the hole.
#[test]
fn a_relation_less_tableau_does_not_bind_to_a_situation_requiring_predicates() {
    let mut registry = ConceptRegistry::default();
    let five = ["alpha", "beta", "gamma", "delta", "epsilon"];
    for p in five {
        registry
            .register_predicate(p, false, "test predicate")
            .expect("registers");
    }
    let world = a_world();
    let requires: Vec<String> = five.iter().map(|p| format!("predicate:{p}")).collect();
    let corpus = a_corpus("probe", requires);
    let mut table = BTreeMap::new();
    table.insert(
        "probe".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin", "drow"]),
            "deliberately broken: two cast members, zero relations -- must not bind to \
             a situation requiring five predicate tokens it never touches",
        ),
    );
    let out = resolve(&corpus, &registry, world, &table);
    assert_eq!(
        out.get("probe"),
        Some(&Outcome::Blocked(vec!["witness:unbound".to_string()])),
        "a relation-less tableau must not bind to a situation that requires predicates: {out:?}"
    );
    assert_eq!(
        witness_stages(&corpus, &corpus.situations[0], world, &table),
        Err("witness:unbound")
    );
}

/// **The subset-family case, the other half of the bidirectional fix.** A
/// tableau relating by only ONE of a situation's TWO required predicates
/// is refused the same as the zero-relation case — one direction of
/// `witness_binds` alone (the tableau's relations all appear in
/// `requires`) was already satisfied here before 0582; only the added
/// direction (every required predicate is realized by some relation)
/// catches it.
#[test]
fn a_tableau_covering_only_some_required_predicates_is_unbound() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate("instance-of", true, "test")
        .expect("registers");
    registry
        .register_predicate("parent-of", true, "test")
        .expect("registers");
    let world = a_world();
    let corpus = a_corpus(
        "partial",
        vec![
            "predicate:instance-of".to_string(),
            "predicate:parent-of".to_string(),
        ],
    );
    let mut table = BTreeMap::new();
    table.insert(
        "partial".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin", "drow"]).with_relation(
                hornvale_kernel::INSTANCE_OF,
                0,
                1,
            ),
            "deliberately partial: relates by only one of the situation's two required \
             predicates -- the subset-family case decision 0582 also closes",
        ),
    );
    let out = resolve(&corpus, &registry, world, &table);
    assert_eq!(
        out.get("partial"),
        Some(&Outcome::Blocked(vec!["witness:unbound".to_string()])),
        "a tableau covering only SOME of a situation's required predicates must not \
         bind: {out:?}"
    );
}

/// `witness_stages` in isolation reports the dedicated `"witness:unbound"`
/// sentinel, distinct from `"witness:absent"` and `"witness:refused"`.
#[test]
fn witness_stages_reports_unbound_for_a_relation_the_situation_never_requires() {
    let world = a_world();
    let situation = a_situation("s7", vec![]);
    let corpus = Corpus {
        corpus: "witness-test".to_string(),
        provenance: "p".to_string(),
        frozen: "f".to_string(),
        bundles: BTreeMap::new(),
        situations: vec![a_situation("s7", vec![])],
    };
    let mut table = BTreeMap::new();
    table.insert(
        "s7".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin", "drow"]).with_relation(
                hornvale_kernel::INSTANCE_OF,
                0,
                1,
            ),
            "relates by instance-of, but s7 requires nothing — must be refused unbound",
        ),
    );
    assert_eq!(
        witness_stages(&corpus, &situation, world, &table),
        Err("witness:unbound")
    );
}

// ---------------------------------------------------------------------
// The positive case, and requirement 4: the witness roster and the
// `Stageable` set are equal in membership.
// ---------------------------------------------------------------------

/// A witness that BINDS (F1) and stages successfully, whose situation's
/// tokens all resolve, is exactly what makes a situation `Stageable` — the
/// positive case, so the negative ones above are not the only shape this
/// resolver can produce.
#[test]
fn a_bound_witness_that_stages_and_whose_tokens_resolve_is_stageable() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate("instance-of", true, "test predicate")
        .expect("registers");
    let world = a_world();
    let corpus = a_corpus("s4", vec!["predicate:instance-of".to_string()]);
    let mut table = BTreeMap::new();
    table.insert(
        "s4".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin", "drow"]).with_relation(
                hornvale_kernel::INSTANCE_OF,
                0,
                1,
            ),
            "exact: relates the two staged cast members by instance-of, matching s4's \
             sole required token one-for-one",
        ),
    );
    let out = resolve(&corpus, &world.registry, world, &table);
    assert_eq!(
        out.get("s4"),
        Some(&Outcome::Stageable),
        "a witness that binds, stages, and whose tokens resolve must be Stageable: {out:?}"
    );
    assert_eq!(
        witness_stages(&corpus, &corpus.situations[0], world, &table),
        Ok(())
    );
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
        let out = resolve(&corpus, &world.registry, world, &table);

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
    let situation = a_situation("no-such-id", vec![]);
    let corpus = a_corpus("no-such-id", vec![]);
    let empty = BTreeMap::new();
    assert_eq!(
        witness_stages(&corpus, &situation, world, &empty),
        Err("witness:absent")
    );
}

/// A witness whose staging fails is `Err("witness:refused")`, distinct from
/// `"witness:absent"` and `"witness:unbound"` — a reader of a `Blocked`
/// reason can tell "nobody wrote one", "one was written for a different
/// claim", and "one was written and it does not work" apart.
#[test]
fn witness_stages_reports_refused_for_a_witness_that_fails_to_stage() {
    let world = a_world();
    let situation = a_situation("bad", vec![]);
    let corpus = a_corpus("bad", vec![]);
    let mut table = BTreeMap::new();
    table.insert(
        "bad".to_string(),
        WitnessEntry::new(
            Tableau::new().with_cast(["goblin"]).with_thing("key", 9),
            "deliberately broken: a prop held by a cast index that was never staged",
        ),
    );
    assert_eq!(
        witness_stages(&corpus, &situation, world, &table),
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
