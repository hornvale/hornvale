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
/// registry doc (falling back to the predicate key), the rendered value, the
/// system that asserted it, and — when the fact carries one (decision 0014:
/// `Fact.day` is a bare `Option<f64>`) — the sim day it was asserted on. The
/// day is what makes a recount of a *non-functional*, dated predicate (an
/// NPC's `agent-at`, one fact per position change) legible: without it, every
/// position an agent has ever held reads as an undated, unordered pile
/// ("the herder was at the river" — which time?); with it, the recount is a
/// timeline ("the herder was at the river on day 5"). `None` if nothing is
/// recorded about `entity`.
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
        match f.day {
            Some(day) => out.push_str(&format!(
                "- {label}: {} (asserted by {}, day {day})\n",
                render_value(&f.object),
                f.provenance
            )),
            None => out.push_str(&format!(
                "- {label}: {} (asserted by {})\n",
                render_value(&f.object),
                f.provenance
            )),
        }
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
    fn recount_names_the_day_a_dated_fact_was_asserted() {
        // THE PROVENANCE READ (the-quickening T4): a non-functional, dated
        // predicate (like an NPC's `agent-at`) is only legible as a timeline
        // if the recount names the day, not just the value. Mutation-verify:
        // reverting the `f.day` branch above to the undated format string
        // reds this test (it would no longer contain "day 5").
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                Fact {
                    day: Some(5.0),
                    ..fact(
                        e,
                        "tenet",
                        Value::Text("at the river".to_string()),
                        "the-quickening",
                    )
                },
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(
            text.contains("at the river (asserted by the-quickening, day 5)"),
            "recount names the day a dated fact was asserted: {text}"
        );
    }

    #[test]
    fn recount_omits_the_day_when_a_fact_carries_none() {
        // The day suffix must be conditional, not always-on: an undated fact
        // (day: None) should read exactly as it always has, with no
        // dangling "day" text.
        let mut w = world();
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                Fact {
                    day: None,
                    ..fact(e, "tenet", Value::Text("undated".to_string()), "religion")
                },
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(
            text.contains("undated (asserted by religion)\n"),
            "an undated fact renders with no day suffix: {text}"
        );
        assert!(!text.contains("day"), "no dangling day text: {text}");
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
