//! Historiography, tier 0: recount how any entity came to be, by replaying
//! its committed facts, their provenance, and the registry's predicate docs.
//! Domain-agnostic — it interprets no domain-specific predicate, so a new
//! domain's facts are recounted the day they are committed. This is the seam
//! the Year-2 event ledger and fields-of-history deepen.
#![warn(missing_docs)]

use hornvale_kernel::{EntityId, Value, World};

/// Render a value for a recount line.
fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}

/// Recount an entity from its committed facts: a lead line naming it (by its
/// `name` fact if present), then one bullet per fact — the predicate's
/// registry doc (falling back to the predicate key), the rendered value, and
/// the system that asserted it. `None` if nothing is recorded about `entity`.
/// type-audit: bare-ok(artifact: return)
pub fn recount(world: &World, entity: EntityId) -> Option<String> {
    let facts: Vec<&hornvale_kernel::Fact> = world.ledger.facts_about(entity).collect();
    if facts.is_empty() {
        return None;
    }
    let name = world
        .ledger
        .text_of(entity, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("entity {}", entity.0));
    let mut out = format!("{name}:\n");
    for f in facts {
        let label = world
            .registry
            .predicate(&f.predicate)
            .map(|p| p.doc.clone())
            .unwrap_or_else(|| f.predicate.clone());
        out.push_str(&format!(
            "- {label}: {} (asserted by {})\n",
            render_value(&f.object),
            f.provenance
        ));
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Fact, Seed};

    fn world() -> World {
        let mut w = World::new(Seed(42));
        w.registry
            .register_predicate("is-belief", true, "subject is a belief")
            .unwrap();
        w.registry
            .register_predicate("tenet", true, "the tenet text of a belief")
            .unwrap();
        w
    }

    fn fact(subject: EntityId, predicate: &str, object: Value, provenance: &str) -> Fact {
        Fact {
            subject,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(0.0),
            provenance: provenance.to_string(),
        }
    }

    #[test]
    fn recount_replays_facts_with_docs_and_provenance() {
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                fact(
                    e,
                    hornvale_kernel::NAME,
                    Value::Text("the Ever-Flame".to_string()),
                    "religion",
                ),
                &w.registry,
            )
            .unwrap();
        w.ledger
            .commit(
                fact(e, "is-belief", Value::Flag(true), "religion"),
                &w.registry,
            )
            .unwrap();
        w.ledger
            .commit(
                fact(
                    e,
                    "tenet",
                    Value::Text("it never blinks.".to_string()),
                    "religion",
                ),
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(text.contains("the Ever-Flame"), "lead names the entity");
        assert!(
            text.contains("the tenet text of a belief"),
            "uses the registry doc"
        );
        assert!(text.contains("it never blinks."), "renders the value");
        assert!(text.contains("religion"), "names the provenance");
    }

    #[test]
    fn recount_is_none_for_an_unknown_entity() {
        let w = world();
        assert!(recount(&w, EntityId::new(999).unwrap()).is_none());
    }

    #[test]
    fn recount_is_deterministic() {
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                fact(e, "tenet", Value::Text("x".to_string()), "religion"),
                &w.registry,
            )
            .unwrap();
        assert_eq!(recount(&w, e), recount(&w, e));
    }
}
