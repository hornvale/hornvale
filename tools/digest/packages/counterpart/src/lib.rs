//! Raw observations for the Counterpart development experiment.

use hornvale_kernel::{ConceptRegistry, RegistryError};
use serde_json::{Value, json};
use std::{
    any::Any,
    panic::{AssertUnwindSafe, catch_unwind},
};

fn panic_detail(payload: &(dyn Any + Send)) -> String {
    if let Some(text) = payload.downcast_ref::<String>() {
        text.clone()
    } else if let Some(text) = payload.downcast_ref::<&str>() {
        (*text).to_string()
    } else {
        "non-string panic payload".to_string()
    }
}

fn registration(result: Result<Result<(), RegistryError>, Box<dyn Any + Send>>) -> Value {
    match result {
        Ok(Ok(())) => json!({"outcome":"accepted", "detail":""}),
        Ok(Err(error)) => json!({"outcome":"refused", "detail":error.to_string()}),
        Err(payload) => json!({"outcome":"refused", "detail":panic_detail(payload.as_ref())}),
    }
}

fn concepts(registry: &ConceptRegistry) -> Vec<Value> {
    let mut rows = registry.concepts().collect::<Vec<_>>();
    rows.sort_by(|left, right| (&left.name, &left.domain).cmp(&(&right.name, &right.domain)));
    rows.into_iter()
        .map(|concept| json!({"name":concept.name, "owner":concept.domain}))
        .collect()
}

/// Observe production facts and separately capture the candidate contribution.
/// Expected registration refusals remain observations; roster faults propagate.
pub fn snapshot() -> Value {
    let source_kinds = hornvale_thing::THING_KINDS;
    let component_registry = hornvale_thing::thing_registry();
    let component_kinds = component_registry.ids().map(|id| id.0).collect::<Vec<_>>();
    let borrowed = hornvale_thing::BORROWED
        .iter()
        .map(|(name, owner)| json!({"name":name, "owner":owner}))
        .collect::<Vec<_>>();
    let mut registry = ConceptRegistry::default();
    let settlement_registration = registration(catch_unwind(AssertUnwindSafe(|| {
        hornvale_settlement::register_concepts(&mut registry)
    })));
    let before_concepts = concepts(&registry);
    let thing_registration = registration(catch_unwind(AssertUnwindSafe(|| {
        hornvale_thing::register_concepts(&mut registry)
    })));
    let after_concepts = concepts(&registry);
    let candidate = match catch_unwind(AssertUnwindSafe(digest_thing::contribution)) {
        Ok(Ok(contribution)) => {
            json!({"outcome":"accepted", "detail":"", "contribution":contribution})
        }
        Ok(Err(error)) => json!({"outcome":"refused", "detail":error, "contribution":null}),
        Err(payload) => {
            json!({"outcome":"refused", "detail":panic_detail(payload.as_ref()), "contribution":null})
        }
    };
    json!({"schema":"counterpart-v1", "facts":{
        "source_kinds":source_kinds, "component_kinds":component_kinds, "borrowed":borrowed,
        "before_concepts":before_concepts, "after_concepts":after_concepts,
        "settlement_registration":settlement_registration, "thing_registration":thing_registration
    }, "candidate":candidate})
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn preserves_returned_error_and_panic_as_refused_observations() {
        let error = registration(Ok(Err(RegistryError::ConflictingDefinition {
            name: "hearth".into(),
        })));
        assert_eq!(
            error,
            json!({"outcome":"refused", "detail":"conflicting redefinition of concept 'hearth'"})
        );
        let panic = registration(catch_unwind(|| panic!("ownership refused")));
        assert_eq!(
            panic,
            json!({"outcome":"refused", "detail":"ownership refused"})
        );
    }

    #[test]
    fn observes_real_lender_and_thing_owners() {
        let value = snapshot();
        let facts = &value["facts"];
        assert_eq!(value["schema"], "counterpart-v1");
        assert_eq!(
            facts["settlement_registration"],
            json!({"outcome":"accepted", "detail":""})
        );
        assert_eq!(
            facts["thing_registration"],
            json!({"outcome":"accepted", "detail":""})
        );
        assert!(
            facts["source_kinds"]
                .as_array()
                .unwrap()
                .contains(&json!("key"))
        );
        assert!(
            facts["component_kinds"]
                .as_array()
                .unwrap()
                .contains(&json!("key"))
        );
        assert_eq!(
            value["candidate"]["contribution"]["namespace"],
            "hornvale.thing"
        );
        assert!(
            facts["before_concepts"]
                .as_array()
                .unwrap()
                .contains(&json!({"name":"hearth", "owner":"settlement"}))
        );
        assert!(
            facts["after_concepts"]
                .as_array()
                .unwrap()
                .contains(&json!({"name":"key", "owner":"thing"}))
        );
        assert!(
            facts["after_concepts"]
                .as_array()
                .unwrap()
                .contains(&json!({"name":"hearth", "owner":"settlement"}))
        );
        assert!(
            facts["borrowed"]
                .as_array()
                .unwrap()
                .contains(&json!({"name":"hearth", "owner":"settlement"}))
        );
    }
}
