//! The provision table (decision 0576): `tropes::resolve` now consults
//! [`hornvale::provision::Provision`] instead of asking the concept
//! registry alone. The first block of tests below is Task 5's (ledger home
//! only); the block headed "Task 6" wires the component home
//! (`windows/sentiment`'s per-people snap judgment, decision 0579) and pins
//! Nathan's grain ruling (`docs/superpowers/ledgers/2026-09-01-the-avowal.md`
//! entry #2): `feels-toward` stays unserved, so `bundle:felt-affect` reads
//! 2/3 and stays blocked, deliberately.

use hornvale::provision::{Correspondent, Home, Provision, Unserved};
use hornvale::tropes::{Corpus, Outcome, Situation, resolve, witnesses};
use hornvale_kernel::{ConceptRegistry, Seed};
use std::collections::BTreeMap;
use std::process::Command;

/// A world built the same way `cmd_tropes` builds one — `resolve` now needs
/// one for its witness check even when (as every test in this file does) no
/// situation's tokens ever resolve far enough to reach it.
fn a_world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        Seed(0),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 0 builds: {e}"))
}

/// The workspace root, the same way `trope_coverage.rs` derives it: every
/// binary invocation below resolves corpus and artifact paths relative to
/// the working directory.
fn workspace_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

/// A one-situation corpus requiring exactly the tokens the caller passes,
/// with no bundle or exclusion machinery — enough to exercise `resolve`
/// directly without touching either frozen corpus file.
fn a_corpus(requires: Vec<String>) -> Corpus {
    Corpus {
        corpus: "provision-test".to_string(),
        provenance: "test fixture, not a frozen corpus".to_string(),
        frozen: "n/a".to_string(),
        bundles: BTreeMap::new(),
        situations: vec![Situation {
            id: "s1".to_string(),
            name: "test situation".to_string(),
            actants: BTreeMap::new(),
            requires,
            excluded_by: Vec::new(),
        }],
    }
}

/// **Default-deny is preserved.** A token with no provision row resolves
/// `Blocked`, exactly as before this task — an undeclared token was always
/// missing, and widening the resolver to see two more homes must not
/// change that for a token none of them serve.
#[test]
fn undeclared_token_resolves_blocked() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus(vec!["predicate:no-such-predicate".to_string()]);
    let out = resolve(&corpus, &registry, &world, &witnesses());
    assert_eq!(
        out.get("s1"),
        Some(&Outcome::Blocked(vec![
            "predicate:no-such-predicate".to_string()
        ]))
    );
}

/// **The ledger home still works.** Run both frozen corpora against a real
/// world's registry (built the same way `cmd_tropes` builds one) and
/// confirm the rendered outcome map is byte-identical to the committed
/// artifact. This is the same golden `trope_coverage.rs` pins; restating it
/// here is this task's own evidence that rewiring `resolve` through
/// `Provision` changed no verdict for either corpus.
#[test]
fn ledger_home_still_matches_committed_reports_for_both_corpora() {
    let root = workspace_root();
    for (corpus_path, artifact_stem) in [
        ("tropes/polti.trope.json", "polti-1895"),
        ("tropes/tvtropes-2012.trope.json", "tvtropes-2012"),
    ] {
        let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
            .args(["tropes", "--corpus", corpus_path, "report"])
            .current_dir(&root)
            .output()
            .expect("runs the binary");
        assert!(out.status.success(), "tropes report failed: {out:?}");
        let live = String::from_utf8(out.stdout).expect("utf-8");
        let artifact = root.join(format!("docs/audits/trope-coverage-{artifact_stem}.md"));
        let committed = std::fs::read_to_string(&artifact)
            .unwrap_or_else(|e| panic!("{}: {e}", artifact.display()));
        assert_eq!(
            live, committed,
            "provision rewiring moved the {artifact_stem} report — a verdict changed, which \
             this task must not do"
        );
    }
}

/// **A declared-but-unserved token is refused.** A row whose resolver
/// answers "no" leaves the token missing — a row alone is a claim, not a
/// grant. Without this check, `Provision` would just be a second registry
/// that anything can opt into by being named.
#[test]
fn declared_but_unserved_token_is_not_served() {
    let mut table = Provision::new();
    table.declare("predicate:ghost", Correspondent::Present(Home::Ledger));
    let registry = ConceptRegistry::default();
    assert!(!table.serves("predicate:ghost", &registry));
}

/// **An absence names a reason.** A token declared absent from every home
/// carries a reason string, retrievable from the row itself — and (see
/// `Unserved`'s `compile_fail` doctest in `provision.rs`) a reasonless
/// absence is a construction error the compiler catches, not a runtime one
/// this test could exercise.
#[test]
fn declared_absent_row_carries_its_reason() {
    let mut table = Provision::new();
    table.declare(
        "predicate:affect-kind",
        Correspondent::Absent(Unserved::NotServed("component home not wired (Task 6)")),
    );
    match table.row("predicate:affect-kind") {
        Some(Correspondent::Absent(Unserved::NotServed(reason))) => {
            assert_eq!(*reason, "component home not wired (Task 6)");
        }
        other => panic!("expected an Absent row carrying a reason, got {other:?}"),
    }
}

// ---------------------------------------------------------------------
// Task 6: the component home (decision 0579).
// ---------------------------------------------------------------------

/// **`affect-kind` and `affect-intensity` resolve through the component
/// home.** `Provision::build` (the table `resolve` now actually consults —
/// see that function's own doc) declares both against
/// `Home::Component`, and the component resolver (`windows/sentiment`'s
/// per-people snap judgment) answers "served" without any ledger or world
/// at all.
#[test]
fn affect_kind_and_affect_intensity_resolve_through_component_home() {
    let registry = ConceptRegistry::default();
    let table = Provision::build(&registry);
    assert!(
        table.serves("predicate:affect-kind", &registry),
        "affect-kind should resolve through the component home"
    );
    assert!(
        table.serves("predicate:affect-intensity", &registry),
        "affect-intensity should resolve through the component home"
    );
    for token in ["predicate:affect-kind", "predicate:affect-intensity"] {
        match table.row(token) {
            Some(Correspondent::Present(Home::Component(_))) => {}
            other => panic!("{token}: expected a Present(Home::Component(_)) row, got {other:?}"),
        }
    }
}

/// **No fact is committed** — spec §4.4's whole point. Serialize a real
/// world's ledger before and after every way this task exercises the
/// component home (`Provision::build`, `serves`, and a full `resolve` run
/// over a corpus that requires both affect tokens), and assert byte-identity.
/// `resolve` only ever takes `world: &World`, so Rust's own borrow checker
/// already forbids a mutation through this path — this test is the
/// executable record of that property, not a probe that could plausibly
/// catch a violation the type system missed.
#[test]
fn no_fact_is_committed_serving_affect_tokens() {
    let world = a_world();
    let before =
        serde_json::to_string(&world.ledger).expect("a real world's ledger should serialize");

    let table = Provision::build(&world.registry);
    assert!(table.serves("predicate:affect-kind", &world.registry));
    assert!(table.serves("predicate:affect-intensity", &world.registry));

    let corpus = a_corpus(vec![
        "predicate:affect-kind".to_string(),
        "predicate:affect-intensity".to_string(),
    ]);
    let _ = resolve(&corpus, &world.registry, &world, &witnesses());

    let after =
        serde_json::to_string(&world.ledger).expect("a real world's ledger should serialize");
    assert_eq!(
        before, after,
        "serving affect-kind/affect-intensity must never commit a fact — the ledger moved"
    );
}

/// **`feels-toward` does not resolve — deliberately unregistered.** Nathan's
/// grain ruling (ledger #2): `snap_judgment` is people-to-people,
/// `feels-toward` is person-to-person, and no person-scale producer ships
/// this campaign. The absence carries a reason naming the ruling, not a bare
/// "no row" — a future implementer reading this row should see WHY, not just
/// THAT.
///
/// **This is one of the campaign's honesty-guarantee tests.** If a later
/// change ever makes `feels-toward` resolve without a person-scale producer
/// actually landing, this test is what must catch it.
#[test]
fn feels_toward_does_not_resolve() {
    let registry = ConceptRegistry::default();
    let table = Provision::build(&registry);
    assert!(
        !table.serves("predicate:feels-toward", &registry),
        "feels-toward must stay unserved until a person-scale producer exists"
    );
    match table.row("predicate:feels-toward") {
        Some(Correspondent::Absent(Unserved::NotServed(reason))) => {
            assert!(
                reason.contains("people-to-people") || reason.contains("grain"),
                "the absence reason should name the grain ruling, got {reason:?}"
            );
        }
        other => panic!("expected an explicit Absent row naming the grain ruling, got {other:?}"),
    }
}

/// **`bundle:felt-affect` reads 2/3 and stays blocked.** The other
/// honesty-guarantee test: a situation requiring all three `felt-affect`
/// tokens resolves `Blocked` on exactly `predicate:feels-toward` — the two
/// component-served tokens no longer appear as missing, and the third never
/// does until a person-scale producer lands.
#[test]
fn bundle_felt_affect_reads_two_of_three_and_stays_blocked() {
    let registry = ConceptRegistry::default();
    let world = a_world();
    let corpus = a_corpus(vec![
        "predicate:affect-kind".to_string(),
        "predicate:affect-intensity".to_string(),
        "predicate:feels-toward".to_string(),
    ]);
    let out = resolve(&corpus, &registry, &world, &witnesses());
    assert_eq!(
        out.get("s1"),
        Some(&Outcome::Blocked(vec![
            "predicate:feels-toward".to_string()
        ])),
        "exactly one of felt-affect's three tokens (feels-toward) should still be missing"
    );
}
