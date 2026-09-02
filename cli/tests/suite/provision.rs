//! The provision table (decision 0576): `tropes::resolve` now consults
//! [`hornvale::provision::Provision`] instead of asking the concept
//! registry alone. This task wires the ledger home only; the four tests
//! below are the ones the task brief names.

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
