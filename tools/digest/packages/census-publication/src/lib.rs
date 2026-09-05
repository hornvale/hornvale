//! Digest observations for Hornvale's census publication boundary.
#![warn(missing_docs)]

use digest_protocol::{
    Contribution, Evidence, Instruction, Observation, Outcome, PROTOCOL_VERSION, Requirement,
    validate,
};
use hornvale_lab::census_guard::{
    CANONICAL_CENSUS_HOST, CENSUS_GOLDENS_DIR, require_canonical_host_for,
};
use std::path::{Path, PathBuf};

const NAMESPACE: &str = "hornvale.census-publication";
const GUARD_REQUIREMENT: &str = "hornvale.census-publication:guard-behavior";
const QUEUE_REQUIREMENT: &str = "hornvale.census-publication:queued-authoring";
const SOURCE_OBSERVATION: &str = "hornvale.census-publication:source-host-agreement";
const SCOPES: [&str; 4] = [
    "windows/lab/src/census_guard.rs",
    "windows/lab/src/publish.rs",
    "scripts/census-canonical-host.txt",
    "scripts/sluice-census.sh",
];

type Guard = dyn Fn(&str, &Path, &str) -> Result<(), String>;

#[derive(Clone, Copy)]
enum Expected {
    Allowed,
    Refused,
}

impl Expected {
    fn agrees_with(self, result: &Result<(), String>) -> bool {
        matches!(
            (self, result),
            (Self::Allowed, Ok(())) | (Self::Refused, Err(_))
        )
    }

    fn label(self) -> &'static str {
        match self {
            Self::Allowed => "allowed",
            Self::Refused => "refused",
        }
    }
}

struct GuardCase {
    id: &'static str,
    study: &'static str,
    output: PathBuf,
    output_label: &'static str,
    hostname: String,
    host_label: &'static str,
    expected: Expected,
}

/// Collect the census publication contribution from `repo_root` without
/// publishing artifacts, running a census, or submitting queue work.
pub fn contribution(repo_root: &Path) -> Result<Contribution, String> {
    let source_path = repo_root.join("scripts/census-canonical-host.txt");
    let source_host = std::fs::read_to_string(&source_path)
        .map_err(|error| format!("read {}: {error}", source_path.display()))?;
    let source_host = source_host.trim_ascii();
    if source_host != CANONICAL_CENSUS_HOST {
        return Err(format!(
            "canonical census host source mismatch: scripts/census-canonical-host.txt contains {source_host:?}, but hornvale-lab was compiled with {CANONICAL_CENSUS_HOST:?}; rebuild digest-census-publication from the supplied checkout"
        ));
    }

    let mut observations = evaluate_guard_panel(repo_root, &require_canonical_host_for);
    observations.push(Observation {
        id: SOURCE_OBSERVATION.into(),
        method: "read the supplied root's host source and compare its trimmed value with hornvale-lab's compiled constant".into(),
        subject: "scripts/census-canonical-host.txt and CANONICAL_CENSUS_HOST".into(),
        outcome: Outcome::Satisfied,
        details: format!(
            "The source host and compiled host both name {CANONICAL_CENSUS_HOST:?}. This checks source/build agreement only; it does not identify the machine running the contributor."
        ),
        requirements: vec![GUARD_REQUIREMENT.into()],
    });

    let guard_observations = observations
        .iter()
        .map(|observation| observation.id.clone())
        .collect();
    let contribution = Contribution {
        protocol: PROTOCOL_VERSION,
        namespace: NAMESPACE.into(),
        display_name: "Census publication boundary".into(),
        scopes: SCOPES.iter().map(|scope| (*scope).into()).collect(),
        requirements: vec![
            Requirement {
                id: GUARD_REQUIREMENT.into(),
                statement: "A census study may target the committed census-golden directory suffix only on the canonical host, matched without ASCII case; unrelated studies and unrelated output directories remain permitted by this predicate.".into(),
                sources: vec![
                    "windows/lab/src/census_guard.rs".into(),
                    "windows/lab/src/publish.rs".into(),
                    "scripts/census-canonical-host.txt".into(),
                ],
                evidence: Evidence::Checked {
                    required_observations: guard_observations,
                },
            },
            Requirement {
                id: QUEUE_REQUIREMENT.into(),
                statement: "Request once-per-campaign census authoring through the canonical queue at a full SHA; a delivered branch whose reference artifacts moved lands only through the campaign-close merge approval.".into(),
                sources: vec![
                    "Makefile (sluice-census target)".into(),
                    "scripts/sluice-request.sh (census request kind)".into(),
                    "scripts/sluice-census.sh (branch delivery contract)".into(),
                    "CLAUDE.md (census cadence and close authority)".into(),
                ],
                evidence: Evidence::AuthoredOnly,
            },
        ],
        observations,
        instructions: vec![
            Instruction {
                id: "hornvale.census-publication:interpret-guard-panel".into(),
                markdown: "Treat these observations as a finite check of `require_canonical_host_for` plus current source/compiled-host agreement. The official-output cases use `Path::ends_with` semantics: an absolute checkout prefix is permitted before the full `book/src/laboratory/generated` suffix. They do not execute or establish invocation of `publish`, a census, or any queue path, and they do not identify the current machine.".into(),
                requirements: vec![GUARD_REQUIREMENT.into()],
                observations: vec![
                    "hornvale.census-publication:exact-census-canonical-host".into(),
                    "hornvale.census-publication:prefixed-census-normalized-host".into(),
                    "hornvale.census-publication:exact-census-different-host".into(),
                    "hornvale.census-publication:prefixed-census-different-host".into(),
                    "hornvale.census-publication:exact-census-relative-suffix-different-host".into(),
                    "hornvale.census-publication:unrelated-study".into(),
                    "hornvale.census-publication:unrelated-output".into(),
                    SOURCE_OBSERVATION.into(),
                ],
            },
            Instruction {
                id: "hornvale.census-publication:request-census".into(),
                markdown: "After the candidate has a full commit SHA, request its once-per-campaign census with `make sluice-census BRANCH=<requester> REF=<full-sha>`. The queued job authors on the canonical host and delivers a separate census branch; if reference artifacts moved, submit that delivered branch through the ordinary merge queue at campaign close and obtain the campaign-close approval before landing it.".into(),
                requirements: vec![QUEUE_REQUIREMENT.into()],
                observations: Vec::new(),
            },
        ],
    };
    validate(&contribution).map_err(|error| format!("census contribution contract: {error}"))?;
    Ok(contribution)
}

fn evaluate_guard_panel(repo_root: &Path, guard: &Guard) -> Vec<Observation> {
    let different_host = format!("{CANONICAL_CENSUS_HOST}-different");
    let cases = [
        GuardCase {
            id: "hornvale.census-publication:exact-census-canonical-host",
            study: "the-census",
            output: PathBuf::from(CENSUS_GOLDENS_DIR),
            output_label: "exact committed-goldens suffix",
            hostname: CANONICAL_CENSUS_HOST.into(),
            host_label: "canonical host",
            expected: Expected::Allowed,
        },
        GuardCase {
            id: "hornvale.census-publication:prefixed-census-normalized-host",
            study: "census-of-the-meeting",
            output: repo_root.join(CENSUS_GOLDENS_DIR),
            output_label: "absolute path ending with the committed-goldens suffix",
            hostname: CANONICAL_CENSUS_HOST.to_ascii_uppercase(),
            host_label: "ASCII-case-normalized canonical host",
            expected: Expected::Allowed,
        },
        GuardCase {
            id: "hornvale.census-publication:exact-census-different-host",
            study: "the-census",
            output: repo_root.join(CENSUS_GOLDENS_DIR),
            output_label: "absolute path ending with the committed-goldens suffix",
            hostname: different_host.clone(),
            host_label: "guaranteed different host",
            expected: Expected::Refused,
        },
        GuardCase {
            id: "hornvale.census-publication:prefixed-census-different-host",
            study: "census-of-the-meeting",
            output: repo_root.join(CENSUS_GOLDENS_DIR),
            output_label: "absolute path ending with the committed-goldens suffix",
            hostname: different_host.clone(),
            host_label: "guaranteed different host",
            expected: Expected::Refused,
        },
        GuardCase {
            id: "hornvale.census-publication:exact-census-relative-suffix-different-host",
            study: "the-census",
            output: PathBuf::from(CENSUS_GOLDENS_DIR),
            output_label: "exact relative committed-goldens suffix",
            hostname: different_host.clone(),
            host_label: "guaranteed different host",
            expected: Expected::Refused,
        },
        GuardCase {
            id: "hornvale.census-publication:unrelated-study",
            study: "the-chorus",
            output: repo_root.join(CENSUS_GOLDENS_DIR),
            output_label: "absolute path ending with the committed-goldens suffix",
            hostname: different_host.clone(),
            host_label: "guaranteed different host",
            expected: Expected::Allowed,
        },
        GuardCase {
            id: "hornvale.census-publication:unrelated-output",
            study: "the-census",
            output: repo_root.join("target/digest-census-scratch"),
            output_label: "unrelated scratch output",
            hostname: different_host,
            host_label: "guaranteed different host",
            expected: Expected::Allowed,
        },
    ];

    cases
        .into_iter()
        .map(|case| {
            let result = guard(case.study, &case.output, &case.hostname);
            let actual = if result.is_ok() { "allowed" } else { "refused" };
            Observation {
                id: case.id.into(),
                method: "call the pure census guard and compare its result with an independently authored finite-case expectation".into(),
                subject: format!(
                    "study={:?}; output={}; host={}",
                    case.study, case.output_label, case.host_label
                ),
                outcome: if case.expected.agrees_with(&result) {
                    Outcome::Satisfied
                } else {
                    Outcome::Contradicted
                },
                details: format!(
                    "Expected {} and observed {actual}. This result covers only this literal panel case and does not prove that a caller invokes the guard.",
                    case.expected.label()
                ),
                requirements: vec![GUARD_REQUIREMENT.into()],
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use digest_protocol::{Evidence, PROTOCOL_VERSION, validate};
    use std::{
        fs,
        sync::atomic::{AtomicU64, Ordering},
    };

    static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

    fn repo_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(4)
            .expect("package lives under tools/digest/packages")
            .to_path_buf()
    }

    fn temp_root(source_host: &str) -> PathBuf {
        let root = std::env::temp_dir().join(format!(
            "digest-census-publication-{}-{}",
            std::process::id(),
            NEXT_TEMP.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(root.join("scripts")).unwrap();
        fs::write(
            root.join("scripts/census-canonical-host.txt"),
            format!("{source_host}\n"),
        )
        .unwrap();
        root
    }

    fn outcome<'a>(observations: &'a [Observation], id: &str) -> &'a Outcome {
        &observations
            .iter()
            .find(|observation| observation.id == id)
            .unwrap_or_else(|| panic!("missing discriminating observation {id}"))
            .outcome
    }

    /// Checks required ⊆ observed and every observed panel result is satisfied;
    /// it does not claim the finite panel exhausts all guard inputs.
    #[test]
    fn contribution_is_valid_and_names_every_finite_guard_observation() {
        let contribution = contribution(&repo_root()).expect("current source agrees with lab");
        assert_eq!(contribution.protocol, PROTOCOL_VERSION);
        validate(&contribution).expect("contributor output satisfies the shared protocol");

        let guard_requirement = contribution
            .requirements
            .iter()
            .find(|requirement| requirement.id == GUARD_REQUIREMENT)
            .expect("guard requirement");
        let Evidence::Checked {
            required_observations,
        } = &guard_requirement.evidence
        else {
            panic!("guard behavior must be checked")
        };
        assert_eq!(required_observations.len(), 8);
        assert!(contribution.observations.iter().all(|observation| {
            required_observations.contains(&observation.id)
                && observation.outcome == Outcome::Satisfied
        }));
    }

    /// Checks stale compiled lab data fails closed with recovery context; it
    /// does not decide which hostname should be canonical.
    #[test]
    fn source_host_disagreement_is_an_actionable_collection_error() {
        let different = format!("{CANONICAL_CENSUS_HOST}-different");
        let root = temp_root(&different);
        let error = contribution(&root).expect_err("stale compiled host must stop collection");
        fs::remove_dir_all(&root).unwrap();

        assert!(error.contains("scripts/census-canonical-host.txt"));
        assert!(error.contains(&different));
        assert!(error.contains(CANONICAL_CENSUS_HOST));
        assert!(error.contains("rebuild"));
    }

    /// Checks the panel rejects over-admission; it does not establish behavior
    /// outside the finite cases.
    #[test]
    fn finite_panel_rejects_an_always_accepting_guard() {
        let observations = evaluate_guard_panel(&repo_root(), &|_, _, _| Ok(()));
        assert!(
            observations
                .iter()
                .any(|observation| observation.outcome == Outcome::Contradicted),
            "the different-host refusal case must catch over-admission"
        );
    }

    /// Checks the panel rejects over-refusal; it does not establish behavior
    /// outside the finite cases.
    #[test]
    fn finite_panel_rejects_an_always_refusing_guard() {
        let observations =
            evaluate_guard_panel(&repo_root(), &|_, _, _| Err("mutant refusal".into()));
        assert!(
            observations
                .iter()
                .any(|observation| observation.outcome == Outcome::Contradicted),
            "the permitted cases must catch over-refusal"
        );
    }

    /// Checks the prefixed-study refusal is load-bearing; it does not establish
    /// protection for other possible study-name conventions.
    #[test]
    fn finite_panel_rejects_a_guard_that_omits_census_of_prefix_protection() {
        let observations = evaluate_guard_panel(&repo_root(), &|study, output, hostname| {
            if study.starts_with("census-of-") {
                Ok(())
            } else {
                require_canonical_host_for(study, output, hostname)
            }
        });
        assert_eq!(
            outcome(
                &observations,
                "hornvale.census-publication:prefixed-census-different-host"
            ),
            &Outcome::Contradicted
        );
    }

    /// Checks the exact relative golden suffix is load-bearing; it does not
    /// establish protection for paths that do not end with that suffix.
    #[test]
    fn finite_panel_rejects_a_guard_that_omits_relative_suffix_protection() {
        let observations = evaluate_guard_panel(&repo_root(), &|study, output, hostname| {
            if output.is_relative() {
                Ok(())
            } else {
                require_canonical_host_for(study, output, hostname)
            }
        });
        assert_eq!(
            outcome(
                &observations,
                "hornvale.census-publication:exact-census-relative-suffix-different-host"
            ),
            &Outcome::Contradicted
        );
    }

    /// Checks the current pure lab guard agrees with all literal cases; it does
    /// not execute or establish invocation by publication or queue callers.
    #[test]
    fn live_lab_guard_satisfies_the_independently_authored_panel() {
        let observations = evaluate_guard_panel(&repo_root(), &require_canonical_host_for);
        assert_eq!(observations.len(), 7);
        assert!(
            observations
                .iter()
                .all(|observation| observation.outcome == Outcome::Satisfied)
        );
    }
}
