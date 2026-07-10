//! Render the concept registry as the book's generated reference page.

use hornvale_kernel::ConceptRegistry;

/// Render the registry as markdown for the book's reference section.
/// type-audit: bare-ok(artifact: return)
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
    doc.push_str("\n### Concepts\n\n");
    doc.push_str("| Concept | Domain | Kind | Meaning |\n|---|---|---|---|\n");
    let mut any_concepts = false;
    for c in registry.concepts() {
        any_concepts = true;
        doc.push_str(&format!(
            "| `{}` | {} | {} | {} |\n",
            c.name,
            c.domain,
            kind_kebab(c.kind),
            c.doc
        ));
    }
    if !any_concepts {
        doc.push_str("| *(none registered)* | | | |\n");
    }
    doc
}

/// A `ConceptKind`'s kebab-case rendering, matching the generated page's
/// convention for every other identifier column.
fn kind_kebab(kind: hornvale_kernel::ConceptKind) -> &'static str {
    match kind {
        hornvale_kernel::ConceptKind::Substance => "substance",
        hornvale_kernel::ConceptKind::Living => "living",
        hornvale_kernel::ConceptKind::Celestial => "celestial",
        hornvale_kernel::ConceptKind::Terrain => "terrain",
        hornvale_kernel::ConceptKind::Social => "social",
        hornvale_kernel::ConceptKind::Body => "body",
        hornvale_kernel::ConceptKind::Kin => "kin",
        hornvale_kernel::ConceptKind::Quality => "quality",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_worldgen::register_all;

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
            "### Concepts",
            "| `sun` | astronomy | celestial |",
            "| `god` | religion | social |",
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
