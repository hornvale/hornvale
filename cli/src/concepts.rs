//! Render the concept registry as the book's generated reference page.

use hornvale_kernel::ConceptRegistry;

/// Render the registry as markdown for the book's reference section.
pub fn render_concepts(registry: &ConceptRegistry) -> String {
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale concepts`. -->\n\n");
    doc.push_str("### Predicates\n\n");
    doc.push_str("| Predicate | Functional | Meaning |\n|---|---|---|\n");
    for p in registry.predicates() {
        let functional = if p.functional { "yes" } else { "no" };
        doc.push_str(&format!("| `{}` | {} | {} |\n", p.name, functional, p.doc));
    }
    doc.push_str("\n### Phenomenon kinds\n\n");
    let mut any = false;
    doc.push_str("| Kind | Meaning |\n|---|---|\n");
    for (kind, kind_doc) in registry.phenomenon_kinds() {
        any = true;
        doc.push_str(&format!("| `{kind}` | {kind_doc} |\n"));
    }
    if !any {
        doc.push_str("| *(none registered)* | |\n");
    }
    doc
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world_builder::register_all;

    #[test]
    fn renders_all_registered_concepts() {
        let mut registry = ConceptRegistry::default();
        registry
            .register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        register_all(&mut registry).unwrap();
        let doc = render_concepts(&registry);
        for expected in [
            "<!-- GENERATED FILE",
            "| `name` | yes |",
            "| `is-place` |",
            "| `has-caste` | no |",
            "| `tenet` |",
            "`celestial-body`",
            "`ambient`",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn render_is_deterministic() {
        let mut registry = ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        assert_eq!(render_concepts(&registry), render_concepts(&registry));
    }
}
