//! Checked Digest context for Hornvale's Thing domain.
#![warn(missing_docs)]

use digest_protocol::{
    Contribution, Evidence, Instruction, Observation, Outcome, PROTOCOL_VERSION, Requirement,
};
use hornvale_kernel::{ConceptRegistry, RegistryError};
use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet},
    panic::{AssertUnwindSafe, catch_unwind},
};

const REQUIREMENT_ID: &str = "hornvale.thing:registry-contract";
const REGISTRATION_ID: &str = "hornvale.thing:registration";
const COMPONENT_ROSTER_ID: &str = "hornvale.thing:component-roster";
const CONCEPT_OWNERSHIP_ID: &str = "hornvale.thing:concept-ownership";
const REQUIREMENT_STATEMENT: &str = "The Thing source roster, component registry, and composed concept registry agree in both directions, with each kind owned by Thing unless BORROWED cedes it to its declared owner.";

fn inspect<F>(
    source_roster: &[&str],
    component_roster: Vec<&str>,
    borrowed: &[(&str, &str)],
    mut registry: ConceptRegistry,
    register: F,
) -> Vec<Observation>
where
    F: FnOnce(&mut ConceptRegistry) -> Result<(), RegistryError>,
{
    let (registration_outcome, registration_details, registration_succeeded) =
        match catch_unwind(AssertUnwindSafe(|| register(&mut registry))) {
            Ok(Ok(())) => (
                Outcome::Satisfied,
                "The supplied Settlement-then-Thing registration completed without error or panic."
                    .to_string(),
                true,
            ),
            Ok(Err(error)) => (
                Outcome::Contradicted,
                format!("Registry composition returned an error: {error}"),
                false,
            ),
            Err(payload) => (
                Outcome::Contradicted,
                format!(
                    "Registry composition refused the observed ownership arrangement: {}",
                    panic_message(payload.as_ref())
                ),
                false,
            ),
        };

    let source_set = source_roster.iter().copied().collect::<BTreeSet<_>>();
    let component_set = component_roster.iter().copied().collect::<BTreeSet<_>>();
    let missing_components = source_set
        .difference(&component_set)
        .copied()
        .collect::<Vec<_>>();
    let extra_components = component_set
        .difference(&source_set)
        .copied()
        .collect::<Vec<_>>();
    let source_duplicates = duplicates(source_roster);
    let component_duplicates = duplicates(&component_roster);
    let component_outcome = if missing_components.is_empty()
        && extra_components.is_empty()
        && source_duplicates.is_empty()
        && component_duplicates.is_empty()
    {
        Outcome::Satisfied
    } else {
        Outcome::Contradicted
    };
    let component_details = format!(
        "source roster: {}; component registry: {}; missing from component registry: {}; extra in component registry: {}; duplicate source labels: {}; duplicate component labels: {}. Source roster order is retained as authored.",
        list(source_roster.iter().copied()),
        list(component_roster.iter().copied()),
        list(missing_components.iter().copied()),
        list(extra_components.iter().copied()),
        list(source_duplicates.iter().copied()),
        list(component_duplicates.iter().copied()),
    );

    let (ownership_outcome, ownership_details) = if registration_succeeded {
        compare_ownership(source_roster, borrowed, &registry)
    } else {
        (
            Outcome::Unknown,
            "Registration was refused, so the resulting partial registry was not used to claim concept ownership agreement."
                .to_string(),
        )
    };

    vec![
        Observation {
            id: REGISTRATION_ID.into(),
            method: "Call hornvale_settlement::register_concepts, then hornvale_thing::register_concepts, while catching a registration panic as refusal"
                .into(),
            subject: "The composed ConceptRegistry used to observe Thing concept ownership".into(),
            outcome: registration_outcome,
            details: registration_details,
            requirements: vec![REQUIREMENT_ID.into()],
        },
        Observation {
            id: COMPONENT_ROSTER_ID.into(),
            method: "Compare hornvale_thing::THING_KINDS with hornvale_thing::thing_registry().ids() as sets in both directions"
                .into(),
            subject: "The authored Thing kind roster and canonical Thing component registry".into(),
            outcome: component_outcome,
            details: component_details,
            requirements: vec![REQUIREMENT_ID.into()],
        },
        Observation {
            id: CONCEPT_OWNERSHIP_ID.into(),
            method: "Use ConceptRegistry::concept for forward owner checks and ConceptRegistry::concepts for reverse inclusion of Thing-owned concepts"
                .into(),
            subject: "THING_KINDS, BORROWED, and the composed concept registry owners".into(),
            outcome: ownership_outcome,
            details: ownership_details,
            requirements: vec![REQUIREMENT_ID.into()],
        },
    ]
}

fn build_contribution(statement: &str, observations: Vec<Observation>) -> Contribution {
    Contribution {
        protocol: PROTOCOL_VERSION,
        namespace: "hornvale.thing".into(),
        display_name: "Thing domain registry".into(),
        scopes: vec!["domains/thing".into()],
        requirements: vec![Requirement {
            id: REQUIREMENT_ID.into(),
            statement: statement.into(),
            sources: vec![
                "domains/thing/src/lib.rs (THING_KINDS, BORROWED, thing_registry, register_concepts)"
                    .into(),
                "domains/settlement/src/lib.rs (register_concepts)".into(),
                "domains/CLAUDE.md".into(),
            ],
            evidence: Evidence::Checked {
                required_observations: vec![
                    REGISTRATION_ID.into(),
                    COMPONENT_ROSTER_ID.into(),
                    CONCEPT_OWNERSHIP_ID.into(),
                ],
            },
        }],
        observations,
        instructions: vec![Instruction {
            id: "hornvale.thing:maintainer-guide".into(),
            markdown: "Consult `domains/CLAUDE.md` before editing the domain. `hornvale_thing::THING_KINDS` is the authored ordered roster and `hornvale_thing::thing_registry` supplies its component rows. A concept is owned by Thing unless `hornvale_thing::BORROWED` names another owner; the actual composition calls `hornvale_settlement::register_concepts` before `hornvale_thing::register_concepts` and inspects owners through `ConceptRegistry::concept`. These finite checks establish roster and concept-owner agreement only. They do not construct a world, inspect world-generation wiring, validate save compatibility, or establish portable, openable, lockable, placement, or other item behavior."
                .into(),
            requirements: vec![REQUIREMENT_ID.into()],
            observations: vec![
                REGISTRATION_ID.into(),
                COMPONENT_ROSTER_ID.into(),
                CONCEPT_OWNERSHIP_ID.into(),
            ],
        }],
    }
}

fn compare_ownership(
    source_roster: &[&str],
    borrowed: &[(&str, &str)],
    registry: &ConceptRegistry,
) -> (Outcome, String) {
    let source_set = source_roster.iter().copied().collect::<BTreeSet<_>>();
    let mut borrowed_owners = BTreeMap::new();
    let mut duplicate_borrowing = Vec::new();
    for (label, owner) in borrowed {
        if borrowed_owners.insert(*label, *owner).is_some() {
            duplicate_borrowing.push(*label);
        }
    }

    let undeclared_borrowing = borrowed_owners
        .keys()
        .filter(|label| !source_set.contains(**label))
        .copied()
        .collect::<Vec<_>>();
    let mut missing_concepts = Vec::new();
    let mut wrong_owners = Vec::new();
    for label in source_roster {
        let expected_owner = borrowed_owners.get(label).copied().unwrap_or("thing");
        match registry.concept(label) {
            None => missing_concepts.push(*label),
            Some(concept) if concept.domain != expected_owner => wrong_owners.push(format!(
                "{label} expected {expected_owner}, found {}",
                concept.domain
            )),
            Some(_) => {}
        }
    }
    let extra_thing_concepts = registry
        .concepts()
        .filter(|concept| concept.domain == "thing")
        .map(|concept| concept.name.as_str())
        .filter(|name| !source_set.contains(name))
        .collect::<Vec<_>>();

    let outcome = if missing_concepts.is_empty()
        && wrong_owners.is_empty()
        && extra_thing_concepts.is_empty()
        && undeclared_borrowing.is_empty()
        && duplicate_borrowing.is_empty()
    {
        Outcome::Satisfied
    } else {
        Outcome::Contradicted
    };
    let borrowed_details = borrowed
        .iter()
        .map(|(label, owner)| format!("{label}->{owner}"))
        .collect::<Vec<_>>();
    (
        outcome,
        format!(
            "source roster: {}; declared borrowing: {}; missing concepts: {}; wrong owners: {}; extra Thing-owned concepts: {}; borrowed labels outside source roster: {}; duplicate borrowing declarations: {}. Concepts owned by other domains and absent from THING_KINDS are outside the reverse Thing-owned comparison.",
            list(source_roster.iter().copied()),
            list(borrowed_details.iter().map(String::as_str)),
            list(missing_concepts.iter().copied()),
            list(wrong_owners.iter().map(String::as_str)),
            list(extra_thing_concepts.iter().copied()),
            list(undeclared_borrowing.iter().copied()),
            list(duplicate_borrowing.iter().copied()),
        ),
    )
}

fn duplicates<'a>(values: &[&'a str]) -> Vec<&'a str> {
    let mut seen = BTreeSet::new();
    let mut duplicates = BTreeSet::new();
    for value in values {
        if !seen.insert(*value) {
            duplicates.insert(*value);
        }
    }
    duplicates.into_iter().collect()
}

fn list<'a>(values: impl IntoIterator<Item = &'a str>) -> String {
    let values = values.into_iter().collect::<Vec<_>>();
    if values.is_empty() {
        "(none)".into()
    } else {
        values.join(", ")
    }
}

fn panic_message(payload: &(dyn Any + Send)) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "non-string panic payload".into()
    }
}

/// Collect the Thing contributor's current checked context.
pub fn contribution() -> Result<Contribution, String> {
    let component_registry = hornvale_thing::thing_registry();
    let observations = inspect(
        hornvale_thing::THING_KINDS,
        component_registry.ids().map(|id| id.0).collect(),
        hornvale_thing::BORROWED,
        ConceptRegistry::default(),
        |registry| {
            hornvale_settlement::register_concepts(registry)?;
            hornvale_thing::register_concepts(registry)
        },
    );
    let contribution = build_contribution(REQUIREMENT_STATEMENT, observations);
    digest_protocol::validate(&contribution).map_err(|error| error.to_string())?;
    Ok(contribution)
}

#[cfg(test)]
mod tests {
    use super::*;
    use digest_protocol::Outcome;
    use hornvale_kernel::{
        ConceptDef, ConceptKind, ConceptRegistry, Correspondent, Manifest, Void,
    };

    fn manifest(name: &str, domain: &str) -> Manifest {
        Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: domain.to_string(),
                kind: ConceptKind::Object,
                doc: format!("test concept {name}"),
            },
            lexeme: Correspondent::Absent(Void::Gap("test fixture")),
            percept: Correspondent::Absent(Void::Gap("test fixture")),
            cognition: Correspondent::Absent(Void::Gap("test fixture")),
        }
    }

    fn registry_with(entries: &[(&str, &str)]) -> ConceptRegistry {
        let mut registry = ConceptRegistry::default();
        for (name, domain) in entries {
            registry
                .register_manifest(manifest(name, domain))
                .expect("fixture concepts are distinct");
        }
        registry
    }

    fn outcome(observations: &[digest_protocol::Observation], local_id: &str) -> Outcome {
        observations
            .iter()
            .find(|observation| observation.id == format!("hornvale.thing:{local_id}"))
            .unwrap_or_else(|| panic!("missing {local_id} observation"))
            .outcome
    }

    /// Break caught: dropping or misattributing a production registration must
    /// prevent the contributor from claiming full agreement.
    #[test]
    fn actual_settlement_and_thing_registration_agree() {
        let observations = inspect(
            hornvale_thing::THING_KINDS,
            hornvale_thing::thing_registry()
                .ids()
                .map(|id| id.0)
                .collect(),
            hornvale_thing::BORROWED,
            ConceptRegistry::default(),
            |registry| {
                hornvale_settlement::register_concepts(registry)?;
                hornvale_thing::register_concepts(registry)
            },
        );

        assert_eq!(outcome(&observations, "registration"), Outcome::Satisfied);
        assert_eq!(
            outcome(&observations, "component-roster"),
            Outcome::Satisfied
        );
        assert_eq!(
            outcome(&observations, "concept-ownership"),
            Outcome::Satisfied
        );
    }

    /// Break caught: running Thing before its declared lender must be reported
    /// as a contradiction instead of being turned into successful evidence.
    #[test]
    fn missing_lender_refuses_registration() {
        let observations = inspect(
            &["hearth"],
            vec!["hearth"],
            &[("hearth", "settlement")],
            ConceptRegistry::default(),
            hornvale_thing::register_concepts,
        );

        assert_eq!(
            outcome(&observations, "registration"),
            Outcome::Contradicted
        );
        assert_eq!(
            outcome(&observations, "concept-ownership"),
            Outcome::Unknown
        );
    }

    /// Break caught: a borrowed name attributed to a different owner must be
    /// reported as refused registration.
    #[test]
    fn wrong_lender_refuses_registration() {
        let observations = inspect(
            &["hearth"],
            vec!["hearth"],
            &[("hearth", "settlement")],
            registry_with(&[("hearth", "climate")]),
            hornvale_thing::register_concepts,
        );

        assert_eq!(
            outcome(&observations, "registration"),
            Outcome::Contradicted
        );
    }

    /// Break caught: a successful registrar that omits an owned roster kind
    /// must fail the forward ownership comparison.
    #[test]
    fn missing_owned_kind_fails_ownership_check() {
        let observations = inspect(
            &["altar", "loom"],
            vec!["altar", "loom"],
            &[],
            registry_with(&[("altar", "thing")]),
            |_| Ok(()),
        );

        assert_eq!(
            outcome(&observations, "concept-ownership"),
            Outcome::Contradicted
        );
    }

    /// Break caught: an owned registry entry absent from the source roster
    /// must fail the reverse ownership comparison.
    #[test]
    fn extra_registry_kind_fails_reverse_roster_check() {
        let observations = inspect(
            &["altar"],
            vec!["altar"],
            &[],
            registry_with(&[("altar", "thing"), ("phantom", "thing")]),
            |_| Ok(()),
        );

        assert_eq!(
            outcome(&observations, "concept-ownership"),
            Outcome::Contradicted
        );
    }

    /// Break caught: requirement prose must not feed back into executable
    /// observation outcomes.
    #[test]
    fn changing_authored_statement_does_not_change_observation_outcome() {
        let observations = inspect(
            &["altar"],
            vec!["altar"],
            &[],
            registry_with(&[("altar", "thing")]),
            |_| Ok(()),
        );
        let original = build_contribution("original policy", observations.clone());
        let changed = build_contribution("changed policy", observations);

        assert_eq!(original.observations, changed.observations);
        assert_ne!(
            original.requirements[0].statement,
            changed.requirements[0].statement
        );
    }

    /// Break caught: sorting the authored roster before rendering details
    /// would erase the source order needed for review.
    #[test]
    fn component_roster_details_preserve_source_order() {
        let observations = inspect(
            &["zeta", "alpha"],
            vec!["alpha", "zeta"],
            &[],
            registry_with(&[("alpha", "thing"), ("zeta", "thing")]),
            |_| Ok(()),
        );
        let observation = observations
            .iter()
            .find(|observation| observation.id == "hornvale.thing:component-roster")
            .expect("component-roster observation");

        assert!(observation.details.contains("source roster: zeta, alpha"));
    }
}
