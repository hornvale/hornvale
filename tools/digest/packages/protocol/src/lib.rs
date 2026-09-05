//! Shared, versioned records exchanged between Digest contributors and host.
#![warn(missing_docs)]

use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt};

/// The only protocol version accepted by this release.
pub const PROTOCOL_VERSION: u32 = 1;

/// One contributor's authored policy, observations, and instructions.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Contribution {
    /// Wire-format version.
    pub protocol: u32,
    /// Stable owner prefix for every record in this contribution.
    pub namespace: String,
    /// Human-readable contributor name.
    pub display_name: String,
    /// Repository-relative areas where this contribution is relevant.
    pub scopes: Vec<String>,
    /// Authored obligations declared by the contributor.
    pub requirements: Vec<Requirement>,
    /// Results observed during this collection invocation.
    pub observations: Vec<Observation>,
    /// Authored guidance for work in the declared scopes.
    pub instructions: Vec<Instruction>,
}

/// An authored obligation and the evidence it requires.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Requirement {
    /// Stable namespaced identity.
    pub id: String,
    /// Authored statement of the obligation.
    pub statement: String,
    /// Repository authorities supporting the statement.
    pub sources: Vec<String>,
    /// Whether the obligation is authored-only or expects named checks.
    pub evidence: Evidence,
}

/// Evidence policy for an authored requirement.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields, rename_all = "snake_case")]
pub enum Evidence {
    /// No executable check is claimed for this requirement.
    AuthoredOnly,
    /// Every named observation must exist, refer back, and be satisfied.
    Checked {
        /// Stable observation identities expected on every collection.
        required_observations: Vec<String>,
    },
}

/// Result of a bounded contributor observation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Outcome {
    /// The named method found the requirement satisfied.
    Satisfied,
    /// The named method found evidence against the requirement.
    Contradicted,
    /// The method could not determine the requirement's state.
    Unknown,
}

/// One named check collected from the current checkout.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Observation {
    /// Stable namespaced identity.
    pub id: String,
    /// Method used to obtain the result.
    pub method: String,
    /// Finite subject examined by the method.
    pub subject: String,
    /// Result of applying the method to the subject.
    pub outcome: Outcome,
    /// Contributor-authored explanation of the result and its limits.
    pub details: String,
    /// Local requirements this observation addresses.
    pub requirements: Vec<String>,
}

/// Authored Markdown guidance supplied by a contributor.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Instruction {
    /// Stable namespaced identity.
    pub id: String,
    /// Trusted authored Markdown rendered verbatim in the instruction body.
    pub markdown: String,
    /// Local requirements addressed by this guidance.
    pub requirements: Vec<String>,
    /// Local observations relevant to this guidance.
    pub observations: Vec<String>,
}

/// A malformed or ambiguous contribution contract.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContractError(pub String);

impl fmt::Display for ContractError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for ContractError {}

/// Validates a contribution before it is composed.
pub fn validate(contribution: &Contribution) -> Result<(), ContractError> {
    if contribution.protocol != PROTOCOL_VERSION {
        return Err(ContractError(format!(
            "unsupported protocol version {}; expected {PROTOCOL_VERSION}",
            contribution.protocol
        )));
    }
    validate_name("namespace", &contribution.namespace)?;
    require_text("display name", &contribution.display_name)?;
    if contribution.scopes.is_empty() {
        return Err(ContractError("contributor scopes cannot be empty".into()));
    }
    for scope in &contribution.scopes {
        let normalized = normalize_scope(scope)?;
        if normalized != *scope {
            return Err(ContractError(format!(
                "scope {scope:?} is not normalized; use {normalized:?}"
            )));
        }
    }
    if contribution.requirements.is_empty() {
        return Err(ContractError(
            "contributor must contain at least one requirement".into(),
        ));
    }
    if contribution.instructions.is_empty() {
        return Err(ContractError(
            "contributor must contain at least one instruction".into(),
        ));
    }

    let mut records = BTreeMap::new();
    for requirement in &contribution.requirements {
        register_id(
            &mut records,
            &contribution.namespace,
            &requirement.id,
            RecordKind::Requirement,
        )?;
        require_text("requirement statement", &requirement.statement)?;
        if requirement.sources.is_empty() {
            return Err(ContractError(format!(
                "requirement {} must name at least one authority/source",
                requirement.id
            )));
        }
        for source in &requirement.sources {
            require_text("requirement authority/source", source)?;
        }
        if let Evidence::Checked {
            required_observations,
        } = &requirement.evidence
            && required_observations.is_empty()
        {
            return Err(ContractError(format!(
                "checked requirement {} must declare a nonempty expected observation set",
                requirement.id
            )));
        }
    }
    for observation in &contribution.observations {
        register_id(
            &mut records,
            &contribution.namespace,
            &observation.id,
            RecordKind::Observation,
        )?;
        require_text("observation method", &observation.method)?;
        require_text("observation subject", &observation.subject)?;
    }
    for instruction in &contribution.instructions {
        register_id(
            &mut records,
            &contribution.namespace,
            &instruction.id,
            RecordKind::Instruction,
        )?;
        require_text("instruction markdown", &instruction.markdown)?;
    }

    for requirement in &contribution.requirements {
        if let Evidence::Checked {
            required_observations,
        } = &requirement.evidence
        {
            reject_duplicate_references(&requirement.id, required_observations)?;
            for observation_id in required_observations {
                require_reference(
                    &records,
                    &contribution.namespace,
                    &requirement.id,
                    observation_id,
                    RecordKind::Observation,
                )?;
                let observation = contribution
                    .observations
                    .iter()
                    .find(|observation| observation.id == *observation_id)
                    .expect("validated observation reference must resolve");
                if !observation
                    .requirements
                    .iter()
                    .any(|id| id == &requirement.id)
                {
                    return Err(ContractError(format!(
                        "required observation {observation_id} does not refer back to requirement {}",
                        requirement.id
                    )));
                }
            }
        }
    }
    for observation in &contribution.observations {
        reject_duplicate_references(&observation.id, &observation.requirements)?;
        for requirement_id in &observation.requirements {
            require_reference(
                &records,
                &contribution.namespace,
                &observation.id,
                requirement_id,
                RecordKind::Requirement,
            )?;
        }
    }
    for instruction in &contribution.instructions {
        reject_duplicate_references(&instruction.id, &instruction.requirements)?;
        reject_duplicate_references(&instruction.id, &instruction.observations)?;
        for requirement_id in &instruction.requirements {
            require_reference(
                &records,
                &contribution.namespace,
                &instruction.id,
                requirement_id,
                RecordKind::Requirement,
            )?;
        }
        for observation_id in &instruction.observations {
            require_reference(
                &records,
                &contribution.namespace,
                &instruction.id,
                observation_id,
                RecordKind::Observation,
            )?;
        }
    }

    Ok(())
}

/// Converts an accepted repository scope to its canonical slash form.
pub fn normalize_scope(value: &str) -> Result<String, ContractError> {
    if value.is_empty() {
        return Err(ContractError("scope cannot be empty".into()));
    }
    if value.starts_with('/') {
        return Err(ContractError(format!(
            "scope {value:?} must be repository-relative"
        )));
    }
    if value.contains('\\') {
        return Err(ContractError(format!(
            "scope {value:?} must use forward slashes"
        )));
    }

    let mut components = Vec::new();
    for component in value.split('/') {
        match component {
            "" | "." => {}
            ".." => {
                return Err(ContractError(format!(
                    "scope {value:?} cannot contain parent traversal"
                )));
            }
            other => components.push(other),
        }
    }
    if components.is_empty() {
        Ok(".".into())
    } else {
        Ok(components.join("/"))
    }
}

/// Reports whether requested and declared scopes overlap at path boundaries.
pub fn scope_matches(requested: &str, declared: &str) -> bool {
    let (Ok(requested), Ok(declared)) = (normalize_scope(requested), normalize_scope(declared))
    else {
        return false;
    };
    requested == "."
        || declared == "."
        || requested == declared
        || requested
            .strip_prefix(&declared)
            .is_some_and(|suffix| suffix.starts_with('/'))
        || declared
            .strip_prefix(&requested)
            .is_some_and(|suffix| suffix.starts_with('/'))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RecordKind {
    Requirement,
    Observation,
    Instruction,
}

impl fmt::Display for RecordKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Requirement => "requirement",
            Self::Observation => "observation",
            Self::Instruction => "instruction",
        })
    }
}

fn register_id<'a>(
    records: &mut BTreeMap<&'a str, RecordKind>,
    namespace: &str,
    id: &'a str,
    kind: RecordKind,
) -> Result<(), ContractError> {
    validate_id(namespace, id)?;
    if let Some(existing) = records.insert(id, kind) {
        return Err(ContractError(format!(
            "duplicate record id {id:?} used by {existing} and {kind}"
        )));
    }
    Ok(())
}

fn validate_id(namespace: &str, id: &str) -> Result<(), ContractError> {
    let Some((owner, local_name)) = id.split_once(':') else {
        return Err(ContractError(format!(
            "record id {id:?} must have form <namespace>:<local-name>"
        )));
    };
    if local_name.contains(':') {
        return Err(ContractError(format!(
            "record id {id:?} must contain exactly one namespace separator"
        )));
    }
    validate_name("record namespace", owner)?;
    validate_name("record local name", local_name)?;
    if owner != namespace {
        return Err(ContractError(format!(
            "record id {id:?} must be local to contributor namespace {namespace:?}"
        )));
    }
    Ok(())
}

fn validate_name(label: &str, value: &str) -> Result<(), ContractError> {
    let mut characters = value.chars();
    if !characters
        .next()
        .is_some_and(|character| character.is_ascii_lowercase())
        || !characters.all(|character| {
            character.is_ascii_lowercase()
                || character.is_ascii_digit()
                || matches!(character, '.' | '-')
        })
    {
        return Err(ContractError(format!(
            "{label} {value:?} must start with a lowercase ASCII letter and contain only lowercase ASCII letters, digits, dots, or hyphens"
        )));
    }
    Ok(())
}

fn require_text(label: &str, value: &str) -> Result<(), ContractError> {
    if value.trim().is_empty() {
        Err(ContractError(format!("{label} cannot be empty")))
    } else {
        Ok(())
    }
}

fn reject_duplicate_references(owner: &str, references: &[String]) -> Result<(), ContractError> {
    let mut seen = BTreeMap::new();
    for reference in references {
        if seen.insert(reference.as_str(), ()).is_some() {
            return Err(ContractError(format!(
                "record {owner} contains duplicate reference {reference}"
            )));
        }
    }
    Ok(())
}

fn require_reference(
    records: &BTreeMap<&str, RecordKind>,
    namespace: &str,
    owner: &str,
    reference: &str,
    expected: RecordKind,
) -> Result<(), ContractError> {
    validate_id(namespace, reference).map_err(|_| {
        ContractError(format!(
            "record {owner} reference {reference:?} must be local to contributor namespace {namespace:?}"
        ))
    })?;
    match records.get(reference) {
        Some(actual) if *actual == expected => Ok(()),
        Some(actual) => Err(ContractError(format!(
            "record {owner} references {actual} {reference} where a {expected} is required"
        ))),
        None => Err(ContractError(format!(
            "record {owner} references unresolved {expected} {reference}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked_fixture() -> Contribution {
        checked_fixture_in("example.checked")
    }

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
                statement: "The example remains covered.".into(),
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
                markdown: "Edit the owning registry first.".into(),
                requirements: vec![requirement_id],
                observations: vec![],
            }],
        }
    }

    fn authored_fixture() -> Contribution {
        let namespace = "example.authored";
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
    fn missing_required_observation_is_rejected() {
        let mut contribution = checked_fixture();
        assert!(validate(&contribution).is_ok());
        assert_eq!(contribution.observations.len(), 1);

        contribution.observations.clear();

        assert!(validate(&contribution).is_err());
    }

    #[test]
    fn required_observation_must_reference_its_requirement() {
        let mut contribution = checked_fixture();
        contribution.observations[0].requirements.clear();

        let error = validate(&contribution).unwrap_err();

        assert!(error.0.contains("does not refer back"), "{error}");
    }

    #[test]
    fn reference_to_wrong_record_kind_is_rejected() {
        let mut contribution = checked_fixture();
        let instruction_id = contribution.instructions[0].id.clone();
        contribution.requirements[0].evidence = Evidence::Checked {
            required_observations: vec![instruction_id],
        };

        let error = validate(&contribution).unwrap_err();

        assert!(error.0.contains("instruction"), "{error}");
        assert!(error.0.contains("observation"), "{error}");
    }

    #[test]
    fn duplicate_identity_across_record_kinds_is_rejected() {
        let mut contribution = checked_fixture();
        contribution.instructions[0].id = contribution.observations[0].id.clone();

        let error = validate(&contribution).unwrap_err();

        assert!(error.0.contains("duplicate record id"), "{error}");
    }

    #[test]
    fn invalid_scopes_are_rejected() {
        for invalid in ["", "/domains/thing", "domains/../kernel", "domains\\thing"] {
            let error = normalize_scope(invalid).unwrap_err();
            assert!(error.0.contains("scope"), "{invalid:?}: {error}");
        }
    }

    #[test]
    fn scopes_are_normalized_and_matched_at_component_boundaries() {
        assert_eq!(
            normalize_scope("./domains/thing/").unwrap(),
            "domains/thing"
        );
        assert_eq!(normalize_scope(".").unwrap(), ".");
        assert!(scope_matches("domains", "domains/thing"));
        assert!(scope_matches("domains/thing/src", "domains/thing"));
        assert!(scope_matches(".", "domains/thing"));
        assert!(!scope_matches("domains/thing-other", "domains/thing"));
    }

    #[test]
    fn unknown_protocol_version_is_rejected() {
        let mut contribution = checked_fixture();
        contribution.protocol = PROTOCOL_VERSION + 1;

        let error = validate(&contribution).unwrap_err();

        assert!(error.0.contains("unsupported protocol version"), "{error}");
    }

    #[test]
    fn references_must_stay_within_the_contributor_namespace() {
        let mut contribution = checked_fixture();
        contribution.instructions[0].requirements = vec!["another.owner:coverage".into()];

        let error = validate(&contribution).unwrap_err();

        assert!(error.0.contains("local"), "{error}");
    }

    #[test]
    fn contributors_require_a_requirement_and_instruction() {
        let mut no_requirement = authored_fixture();
        no_requirement.requirements.clear();
        assert!(validate(&no_requirement).is_err());

        let mut no_instruction = authored_fixture();
        no_instruction.instructions.clear();
        assert!(validate(&no_instruction).is_err());

        assert!(validate(&authored_fixture()).is_ok());
    }

    #[test]
    fn checked_requirements_need_a_nonempty_expected_set() {
        let mut contribution = checked_fixture();
        contribution.requirements[0].evidence = Evidence::Checked {
            required_observations: vec![],
        };

        assert!(validate(&contribution).is_err());
    }

    #[test]
    fn required_text_and_authorities_cannot_be_empty() {
        let mut contribution = checked_fixture();
        contribution.requirements[0].statement.clear();
        assert!(validate(&contribution).is_err());

        let mut contribution = checked_fixture();
        contribution.requirements[0].sources.clear();
        assert!(validate(&contribution).is_err());

        let mut contribution = checked_fixture();
        contribution.observations[0].method.clear();
        assert!(validate(&contribution).is_err());

        let mut contribution = checked_fixture();
        contribution.observations[0].subject.clear();
        assert!(validate(&contribution).is_err());
    }

    #[test]
    fn json_uses_snake_case_enums_and_rejects_unknown_fields() {
        let json = serde_json::to_value(checked_fixture()).unwrap();
        assert_eq!(json["observations"][0]["outcome"], "satisfied");
        assert!(json["requirements"][0]["evidence"].get("checked").is_some());

        let mut object = json.as_object().unwrap().clone();
        object.insert("surprise".into(), serde_json::json!(true));
        let error = serde_json::from_value::<Contribution>(object.into()).unwrap_err();
        assert!(error.to_string().contains("unknown field"), "{error}");
    }
}
