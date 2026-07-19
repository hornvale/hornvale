//! Render the concept registry as the book's generated reference page.

use hornvale_kernel::{
    CognitiveHandle, ConceptRegistry, Correspondent, Lexicalization, Manifest, PerceptKind, Void,
};

/// A per-ledger trial balance: how many of the registered concepts are
/// *covered* on this ledger versus voided, split by void class. The point is
/// that `covered + Σ(void classes)` foots to the registered-concept count —
/// every concept lands in exactly one bucket, so a concept silently missing
/// from a ledger cannot hide.
#[derive(Default)]
struct LedgerTally {
    /// Concepts whose edge on this ledger is `Present`.
    covered: usize,
    /// Voided as [`Void::Unnamed`].
    unnamed: usize,
    /// Voided as [`Void::Gap`].
    gap: usize,
    /// Voided as [`Void::Imperceptible`].
    imperceptible: usize,
    /// Voided as [`Void::Uncognized`].
    uncognized: usize,
}

impl LedgerTally {
    /// Bucket one absent edge by its void class.
    fn add_void(&mut self, v: &Void) {
        match v {
            Void::Unnamed(_) => self.unnamed += 1,
            Void::Gap(_) => self.gap += 1,
            Void::Imperceptible(_) => self.imperceptible += 1,
            Void::Uncognized { .. } => self.uncognized += 1,
        }
    }

    /// `covered + Σ(void classes)` — must equal the registered-concept count.
    fn total(&self) -> usize {
        self.covered + self.unnamed + self.gap + self.imperceptible + self.uncognized
    }

    /// One trial-balance line: `<label>: covered N  gap M  …` (only nonzero
    /// void classes shown, in a fixed order so the render is deterministic).
    fn line(&self, label: &str) -> String {
        let mut parts = vec![format!("covered {}", self.covered)];
        if self.unnamed > 0 {
            parts.push(format!("unnamed {}", self.unnamed));
        }
        if self.gap > 0 {
            parts.push(format!("gap {}", self.gap));
        }
        if self.imperceptible > 0 {
            parts.push(format!("imperceptible {}", self.imperceptible));
        }
        if self.uncognized > 0 {
            parts.push(format!("uncognized {}", self.uncognized));
        }
        format!("  {label:10} {}", parts.join("  "))
    }
}

/// Compact, legible cell for the lexeme edge.
fn lexeme_cell(edge: &Correspondent<Lexicalization, Void>) -> String {
    match edge {
        Correspondent::Present(Lexicalization::Expected) => "Expected".to_string(),
        Correspondent::Present(Lexicalization::Word(w)) => format!("`{w}`"),
        Correspondent::Absent(v) => void_cell(v),
    }
}

/// Compact, legible cell for the percept edge (a covered edge names its
/// phenomenon kind).
fn percept_cell(edge: &Correspondent<PerceptKind, Void>) -> String {
    match edge {
        Correspondent::Present(PerceptKind(kind)) => format!("`{kind}`"),
        Correspondent::Absent(v) => void_cell(v),
    }
}

/// Compact, legible cell for the cognition edge.
fn cognition_cell(edge: &Correspondent<CognitiveHandle, Void>) -> String {
    match edge {
        Correspondent::Present(CognitiveHandle) => "handle".to_string(),
        Correspondent::Absent(v) => void_cell(v),
    }
}

/// Compact rendering of a [`Void`] for a matrix cell.
fn void_cell(v: &Void) -> String {
    match v {
        Void::Unnamed(_) => "Unnamed".to_string(),
        Void::Gap(_) => "Gap".to_string(),
        Void::Imperceptible(_) => "Imperceptible".to_string(),
        Void::Uncognized { pending_wave } => format!("pending({pending_wave})"),
    }
}

/// Render the correspondence manifest — the coverage ledger over every
/// registered concept's three edges (lexicon, perception, cognition), in
/// concept-name order (deterministic via [`ConceptRegistry::manifests`]).
///
/// The layout is backlog-summary-first (the negative space and a per-ledger
/// trial balance) then the full matrix. The *concept* anchor and the *compute*
/// ledger are implicit and omitted: every registered concept IS modeled, so
/// those two columns are always covered — the interesting coverage lives on
/// the three edges shown.
/// type-audit: bare-ok(artifact: return)
pub fn render_manifest(registry: &ConceptRegistry) -> String {
    let manifests: Vec<&Manifest> = registry.manifests().collect();
    let total = manifests.len();

    let mut lex = LedgerTally::default();
    let mut per = LedgerTally::default();
    let mut cog = LedgerTally::default();
    let mut unnamed: Vec<&str> = Vec::new();
    let mut unperceived: Vec<&str> = Vec::new();
    let mut cognition_waves: std::collections::BTreeSet<&str> = std::collections::BTreeSet::new();
    // The phenomenon kinds some concept's percept edge actually names. Any
    // registered kind NOT in here is an orphan — a phenomenon the world can
    // emit that no concept accounts for (the inverse correspondence gap).
    let mut named_kinds: std::collections::BTreeSet<&str> = std::collections::BTreeSet::new();

    for m in &manifests {
        match &m.lexeme {
            Correspondent::Present(_) => lex.covered += 1,
            Correspondent::Absent(v) => {
                lex.add_void(v);
                unnamed.push(m.concept.name.as_str());
            }
        }
        match &m.percept {
            Correspondent::Present(pk) => {
                per.covered += 1;
                named_kinds.insert(pk.0.as_str());
            }
            Correspondent::Absent(v) => {
                per.add_void(v);
                unperceived.push(m.concept.name.as_str());
            }
        }
        match &m.cognition {
            Correspondent::Present(_) => cog.covered += 1,
            Correspondent::Absent(v) => {
                cog.add_void(v);
                if let Void::Uncognized { pending_wave } = v {
                    cognition_waves.insert(pending_wave);
                }
            }
        }
    }

    // Orphan phenomena: registered kinds no concept's percept names. The
    // inverse of `unperceived` — there, a concept has no phenomenon; here, a
    // phenomenon has no concept, so the world can emit an observation nothing
    // can name, speak, or reason about.
    let orphan_phenomena: Vec<&str> = registry
        .phenomenon_kinds()
        .map(|(k, _doc)| k)
        .filter(|k| !named_kinds.contains(k))
        .collect();

    let list = |names: &[&str]| {
        if names.is_empty() {
            "none".to_string()
        } else {
            names.join(", ")
        }
    };
    let waves = if cognition_waves.is_empty() {
        String::new()
    } else {
        format!(
            " [{}]",
            cognition_waves
                .iter()
                .copied()
                .collect::<Vec<_>>()
                .join(", ")
        )
    };

    let mut doc = String::new();
    doc.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale concepts --manifest`. -->\n\n",
    );
    doc.push_str("# Concept Manifest — the correspondence ledger\n\n");
    doc.push_str(
        "Every registered concept is carried across three correspondence ledgers — \
         **lexicon** (a word), **perception** (a phenomenon kind), and **cognition** (a \
         handle) — or is explicitly, reason-bearingly *absent* from one, an absence that \
         must name why. The *concept* anchor and the *compute* ledger are implicit and \
         omitted below: every registered concept IS modeled, so those columns are always \
         covered. This page is the negative space made honest — the gaps are typed, not \
         silent. The audit also runs in reverse: **orphan phenomena** are kinds the world \
         can emit that no concept names, so an observation exists with nothing to attach \
         it to.\n\n",
    );

    doc.push_str("## Backlog (the negative space)\n\n");
    doc.push_str("```text\n");
    doc.push_str(&format!("Unnamed (lexeme gap):      {}\n", list(&unnamed)));
    doc.push_str(&format!(
        "Unperceived (percept gap): {}\n",
        list(&unperceived)
    ));
    doc.push_str(&format!(
        "Orphan phenomena (emitted, no concept names): {}\n",
        list(&orphan_phenomena)
    ));
    doc.push_str(&format!(
        "Uncognized (cognition, all pending): {} concepts{}\n",
        cog.uncognized, waves
    ));
    doc.push('\n');
    doc.push_str(&format!(
        "Trial balance (per ledger: covered + voids = {total} concepts):\n"
    ));
    doc.push_str(&lex.line("lexeme:"));
    doc.push('\n');
    doc.push_str(&per.line("percept:"));
    doc.push('\n');
    doc.push_str(&cog.line("cognition:"));
    doc.push('\n');
    doc.push_str("```\n\n");

    doc.push_str("## Matrix\n\n");
    doc.push_str("| concept | lexeme | percept | cognition |\n");
    doc.push_str("|---|---|---|---|\n");
    for m in &manifests {
        doc.push_str(&format!(
            "| `{}` | {} | {} | {} |\n",
            m.concept.name,
            lexeme_cell(&m.lexeme),
            percept_cell(&m.percept),
            cognition_cell(&m.cognition),
        ));
    }

    debug_assert_eq!(lex.total(), total, "lexeme trial balance must foot");
    debug_assert_eq!(per.total(), total, "percept trial balance must foot");
    debug_assert_eq!(cog.total(), total, "cognition trial balance must foot");

    doc
}

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

    #[test]
    fn manifest_render_has_backlog_and_matrix() {
        let mut registry = ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        let doc = render_manifest(&registry);
        for expected in [
            "<!-- GENERATED FILE",
            "# Concept Manifest — the correspondence ledger",
            "## Backlog (the negative space)",
            "Unnamed (lexeme gap):",
            "Unperceived (percept gap):",
            "Orphan phenomena (emitted, no concept names):",
            "Uncognized (cognition, all pending):",
            "Trial balance (per ledger: covered + voids =",
            "[wave-cognition]",
            "## Matrix",
            "| concept | lexeme | percept | cognition |",
            // sun's percept edge is realized as the celestial-body kind.
            "| `sun` | Expected | `celestial-body` | pending(wave-cognition) |",
            // ice's lexeme was flipped to an honest Gap (no pack names it).
            "| `ice` | Gap |",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn manifest_render_lists_orphan_phenomena() {
        let mut registry = ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        let doc = render_manifest(&registry);
        let orphan_line = doc
            .lines()
            .find(|l| l.starts_with("Orphan phenomena"))
            .unwrap_or_else(|| panic!("no orphan-phenomena line in the manifest view"));
        // Kinds the world emits that no concept names today (astronomy's sky
        // events with no concept of their own) must be surfaced.
        for orphan in ["eclipse", "tide", "seasonal-cycle"] {
            assert!(
                orphan_line.contains(orphan),
                "orphan phenomenon '{orphan}' should be surfaced; line was: {orphan_line}"
            );
        }
        // Kinds a concept DOES name (sun/moon -> celestial-body, star ->
        // night-star, wind -> ambient) must NOT be listed as orphans.
        for named in ["ambient", "celestial-body", "night-star"] {
            assert!(
                !orphan_line.contains(named),
                "'{named}' is named by a concept and must not be an orphan; line: {orphan_line}"
            );
        }
    }

    #[test]
    fn manifest_render_is_deterministic() {
        let mut registry = ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        assert_eq!(render_manifest(&registry), render_manifest(&registry));
    }
}
