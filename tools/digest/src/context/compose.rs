use digest_protocol::{ContractError, Contribution, Evidence, Outcome, validate};
use std::collections::{BTreeMap, BTreeSet};

/// Git identity disclosed alongside a freshly composed report.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CheckoutContext {
    /// Revision examined by the contributors.
    pub revision: String,
    /// Whether the examined checkout had tracked or untracked changes.
    pub dirty: bool,
}

/// Deterministic Markdown plus the outcome of required observations.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContextReport {
    /// Human-readable context assembled from validated contributions.
    pub markdown: String,
    /// Whether every required observation was satisfied.
    pub successful: bool,
}

/// Validates and deterministically composes contributor records.
pub fn compose(
    checkout: &CheckoutContext,
    contributions: &[Contribution],
) -> Result<ContextReport, ContractError> {
    if contributions.is_empty() {
        return Err(ContractError("no contributions to compose".into()));
    }

    let mut namespaces = BTreeSet::new();
    let mut record_owners = BTreeMap::new();
    for contribution in contributions {
        validate(contribution).map_err(|error| {
            ContractError(format!(
                "invalid contribution {:?}: {}",
                contribution.namespace, error.0
            ))
        })?;
        if !namespaces.insert(contribution.namespace.as_str()) {
            return Err(ContractError(format!(
                "duplicate contributor namespace {:?}",
                contribution.namespace
            )));
        }
        for id in contribution
            .requirements
            .iter()
            .map(|record| record.id.as_str())
            .chain(
                contribution
                    .observations
                    .iter()
                    .map(|record| record.id.as_str()),
            )
            .chain(
                contribution
                    .instructions
                    .iter()
                    .map(|record| record.id.as_str()),
            )
        {
            if let Some(owner) = record_owners.insert(id, contribution.namespace.as_str()) {
                return Err(ContractError(format!(
                    "duplicate record id {id:?} in contributors {owner:?} and {:?}",
                    contribution.namespace
                )));
            }
        }
    }

    let mut sorted = contributions.iter().collect::<Vec<_>>();
    sorted.sort_by(|left, right| left.namespace.cmp(&right.namespace));

    let successful = sorted.iter().all(|contribution| {
        contribution.requirements.iter().all(|requirement| {
            let Evidence::Checked {
                required_observations,
            } = &requirement.evidence
            else {
                return true;
            };
            required_observations.iter().all(|required_id| {
                contribution
                    .observations
                    .iter()
                    .find(|observation| observation.id == *required_id)
                    .is_some_and(|observation| observation.outcome == Outcome::Satisfied)
            })
        })
    });

    let mut markdown = String::from("# Digest context\n\n");
    markdown.push_str("Revision: `");
    markdown.push_str(&escape_metadata(&checkout.revision));
    markdown.push_str("`\n\nWorking tree: **");
    markdown.push_str(if checkout.dirty { "dirty" } else { "clean" });
    markdown.push_str("**\n");

    for contribution in sorted {
        render_contribution(&mut markdown, contribution);
    }

    Ok(ContextReport {
        markdown,
        successful,
    })
}

fn render_contribution(markdown: &mut String, contribution: &Contribution) {
    markdown.push_str("\n## ");
    markdown.push_str(&escape_metadata(&contribution.display_name));
    markdown.push_str(" (`");
    markdown.push_str(&escape_metadata(&contribution.namespace));
    markdown.push_str("`)\n\nScopes: ");
    render_code_list(markdown, &contribution.scopes);
    markdown.push_str("\n\n### Authored requirements\n");

    let mut requirements = contribution.requirements.iter().collect::<Vec<_>>();
    requirements.sort_by(|left, right| left.id.cmp(&right.id));
    for requirement in requirements {
        markdown.push_str("\n#### `");
        markdown.push_str(&escape_metadata(&requirement.id));
        markdown.push_str("`\n\n");
        markdown.push_str(&requirement.statement);
        markdown.push_str("\n\nAuthorities/sources: ");
        render_code_list(markdown, &requirement.sources);
        markdown.push_str("\n\nEvidence: ");
        match &requirement.evidence {
            Evidence::AuthoredOnly => markdown.push_str("Authored only (unchecked).\n"),
            Evidence::Checked {
                required_observations,
            } => {
                markdown.push_str("Checked. Expected observations: ");
                render_code_list(markdown, required_observations);
                markdown.push_str(". Result: **");
                let all_satisfied = required_observations.iter().all(|required_id| {
                    contribution
                        .observations
                        .iter()
                        .find(|observation| observation.id == *required_id)
                        .is_some_and(|observation| observation.outcome == Outcome::Satisfied)
                });
                markdown.push_str(if all_satisfied {
                    "satisfied"
                } else {
                    "not satisfied"
                });
                markdown.push_str("**.\n");
            }
        }
    }

    markdown.push_str("\n### Authored instructions\n");
    let mut instructions = contribution.instructions.iter().collect::<Vec<_>>();
    instructions.sort_by(|left, right| left.id.cmp(&right.id));
    for instruction in instructions {
        markdown.push_str("\n#### `");
        markdown.push_str(&escape_metadata(&instruction.id));
        markdown.push_str("`\n\n");
        markdown.push_str(&instruction.markdown);
        markdown.push_str("\n\nRequirements: ");
        render_optional_code_list(markdown, &instruction.requirements);
        markdown.push_str("\n\nObservations: ");
        render_optional_code_list(markdown, &instruction.observations);
        markdown.push('\n');
    }

    markdown.push_str("\n### Observations\n");
    if contribution.observations.is_empty() {
        markdown.push_str("\nNo executable observations were declared.\n");
    } else {
        let mut observations = contribution.observations.iter().collect::<Vec<_>>();
        observations.sort_by(|left, right| left.id.cmp(&right.id));
        for observation in observations {
            markdown.push_str("\n#### `");
            markdown.push_str(&escape_metadata(&observation.id));
            markdown.push_str("` — ");
            markdown.push_str(match observation.outcome {
                Outcome::Satisfied => "satisfied",
                Outcome::Contradicted => "contradicted",
                Outcome::Unknown => "unknown",
            });
            markdown.push_str("\n\nMethod: ");
            markdown.push_str(&observation.method);
            markdown.push_str("\n\nSubject: ");
            markdown.push_str(&observation.subject);
            markdown.push_str("\n\nDetails: ");
            markdown.push_str(&observation.details);
            markdown.push_str("\n\nRequirements: ");
            render_optional_code_list(markdown, &observation.requirements);
            markdown.push('\n');
        }
    }

    markdown.push_str(
        "\n### Limits\n\n- Observations cover only the named method and finite subject.\n\
         - Contributor checks are reviewed code and can themselves be wrong.\n\
         - Revision and dirty state do not establish reproducibility or an atomic source snapshot.\n\
         - This context does not authorize gate omission or approve changes to its governing rules.\n",
    );
}

fn render_optional_code_list(markdown: &mut String, values: &[String]) {
    if values.is_empty() {
        markdown.push_str("none");
    } else {
        render_code_list(markdown, values);
    }
}

fn render_code_list(markdown: &mut String, values: &[String]) {
    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            markdown.push_str(", ");
        }
        markdown.push('`');
        markdown.push_str(&escape_metadata(value));
        markdown.push('`');
    }
}

fn escape_metadata(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\r' | '\n' => escaped.push(' '),
            '`' => escaped.push_str("&#96;"),
            '\\' | '#' | '*' | '_' | '[' | ']' | '<' | '>' | '|' => {
                escaped.push('\\');
                escaped.push(character);
            }
            other => escaped.push(other),
        }
    }
    escaped
}

#[cfg(test)]
mod tests {
    use super::*;
    use digest_protocol::{
        Contribution, Evidence, Instruction, Observation, Outcome, PROTOCOL_VERSION, Requirement,
    };

    fn checked_fixture_in(namespace: &str) -> Contribution {
        let requirement_id = format!("{namespace}:coverage");
        let observation_id = format!("{namespace}:coverage-check");
        Contribution {
            protocol: PROTOCOL_VERSION,
            namespace: namespace.into(),
            display_name: "Checked example".into(),
            scopes: vec!["domains/example".into()],
            requirements: vec![Requirement {
                id: requirement_id.clone(),
                statement: "The **example** remains covered.".into(),
                sources: vec!["domains/example/src/lib.rs".into()],
                evidence: Evidence::Checked {
                    required_observations: vec![observation_id.clone()],
                },
            }],
            observations: vec![Observation {
                id: observation_id,
                method: "compare the declared and observed roster".into(),
                subject: "domains/example".into(),
                outcome: Outcome::Satisfied,
                details: "Both rosters contain alpha, then beta.".into(),
                requirements: vec![requirement_id.clone()],
            }],
            instructions: vec![Instruction {
                id: format!("{namespace}:editing"),
                markdown: "Edit the **owning registry** first.".into(),
                requirements: vec![requirement_id],
                observations: vec![],
            }],
        }
    }

    fn authored_fixture_in(namespace: &str) -> Contribution {
        let requirement_id = format!("{namespace}:review");
        Contribution {
            protocol: PROTOCOL_VERSION,
            namespace: namespace.into(),
            display_name: "Authored example".into(),
            scopes: vec!["docs/example".into()],
            requirements: vec![Requirement {
                id: requirement_id.clone(),
                statement: "Review the prose for accuracy.".into(),
                sources: vec!["docs/example.md".into()],
                evidence: Evidence::AuthoredOnly,
            }],
            observations: vec![],
            instructions: vec![Instruction {
                id: format!("{namespace}:editing"),
                markdown: "Preserve the source reference.".into(),
                requirements: vec![requirement_id],
                observations: vec![],
            }],
        }
    }

    #[test]
    fn report_separates_authored_content_observations_and_limits() {
        let contribution = checked_fixture_in("example.checked");
        let checkout = CheckoutContext {
            revision: "fixture".into(),
            dirty: true,
        };

        let report = compose(&checkout, &[contribution]).unwrap();

        assert!(report.successful);
        assert!(report.markdown.contains("Revision: `fixture`"));
        assert!(report.markdown.contains("Working tree: **dirty**"));
        assert!(report.markdown.contains("### Authored requirements"));
        assert!(report.markdown.contains("The **example** remains covered."));
        assert!(report.markdown.contains("### Authored instructions"));
        assert!(
            report
                .markdown
                .contains("Edit the **owning registry** first.")
        );
        assert!(report.markdown.contains("### Observations"));
        assert!(
            report
                .markdown
                .contains("compare the declared and observed roster")
        );
        assert!(report.markdown.contains("### Limits"));
        assert!(report.markdown.contains("does not authorize gate omission"));
        assert!(
            report
                .markdown
                .contains("Expected observations: `example.checked:coverage-check`")
        );
    }

    #[test]
    fn contribution_discovery_order_does_not_change_markdown() {
        let a = checked_fixture_in("example.a");
        let b = authored_fixture_in("example.b");
        let context = CheckoutContext {
            revision: "fixture".into(),
            dirty: false,
        };

        assert_eq!(
            compose(&context, &[a.clone(), b.clone()]).unwrap().markdown,
            compose(&context, &[b, a]).unwrap().markdown
        );
    }

    #[test]
    fn unknown_or_contradicted_required_observation_makes_report_unsuccessful() {
        for outcome in [Outcome::Unknown, Outcome::Contradicted] {
            let mut contribution = checked_fixture_in("example.checked");
            contribution.observations[0].outcome = outcome;

            let report = compose(
                &CheckoutContext {
                    revision: "fixture".into(),
                    dirty: false,
                },
                &[contribution],
            )
            .unwrap();

            assert!(!report.successful, "{outcome:?}");
        }
    }

    #[test]
    fn authored_only_requirement_is_explicitly_unchecked_but_not_a_failure() {
        let report = compose(
            &CheckoutContext {
                revision: "fixture".into(),
                dirty: false,
            },
            &[authored_fixture_in("example.authored")],
        )
        .unwrap();

        assert!(report.successful);
        assert!(report.markdown.contains("Authored only (unchecked)"));
    }

    #[test]
    fn duplicate_contributor_namespace_is_rejected() {
        let contribution = checked_fixture_in("example.checked");

        let error = compose(
            &CheckoutContext {
                revision: "fixture".into(),
                dirty: false,
            },
            &[contribution.clone(), contribution],
        )
        .unwrap_err();

        assert!(
            error.0.contains("duplicate contributor namespace"),
            "{error}"
        );
    }

    #[test]
    fn empty_composition_is_rejected() {
        let error = compose(
            &CheckoutContext {
                revision: "fixture".into(),
                dirty: false,
            },
            &[],
        )
        .unwrap_err();

        assert!(error.0.contains("no contributions"), "{error}");
    }

    #[test]
    fn metadata_in_headings_cannot_create_markdown_structure() {
        let mut contribution = authored_fixture_in("example.authored");
        contribution.display_name = "Example\n# forged heading".into();
        let checkout = CheckoutContext {
            revision: "rev`\n# forged checkout heading".into(),
            dirty: false,
        };

        let markdown = compose(&checkout, &[contribution]).unwrap().markdown;

        assert!(!markdown.lines().any(|line| line == "# forged heading"));
        assert!(
            !markdown
                .lines()
                .any(|line| line == "# forged checkout heading")
        );
        assert!(markdown.contains("Example \\# forged heading"));
        assert!(markdown.contains("Revision: `rev&#96; \\# forged checkout heading`"));
    }

    #[test]
    fn outer_records_are_sorted_without_reordering_semantic_lists() {
        let mut contribution = checked_fixture_in("example.checked");
        let requirement_id = contribution.requirements[0].id.clone();
        contribution.observations.push(Observation {
            id: "example.checked:a-check".into(),
            method: "second method".into(),
            subject: "second subject".into(),
            outcome: Outcome::Satisfied,
            details: "zeta, then alpha".into(),
            requirements: vec![requirement_id.clone()],
        });
        contribution.requirements[0].evidence = Evidence::Checked {
            required_observations: vec![
                "example.checked:coverage-check".into(),
                "example.checked:a-check".into(),
            ],
        };

        let markdown = compose(
            &CheckoutContext {
                revision: "fixture".into(),
                dirty: false,
            },
            &[contribution],
        )
        .unwrap()
        .markdown;

        assert!(
            markdown.find("`example.checked:a-check`").unwrap()
                < markdown.rfind("`example.checked:coverage-check`").unwrap()
        );
        assert!(markdown.contains(
            "Expected observations: `example.checked:coverage-check`, `example.checked:a-check`"
        ));
        assert!(markdown.contains("zeta, then alpha"));
    }
}
