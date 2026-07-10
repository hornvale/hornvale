//! Render each species' generated lexicon as the book's dictionary
//! reference page: one table per species, one row per registered concept —
//! its gloss, its modern word (roman + IPA), its proto-form, and a one-line
//! derivation; or, for a concept with no word, the gap and its recountable
//! reason. Sibling of `phonology`, but over a world's own lexicon
//! ([`hornvale_worldgen::lexicon_of`]) rather than a fixed reference seed:
//! exposure (and so every gap) depends on what a world actually settled.
#![warn(missing_docs)]

use hornvale_kernel::World;
use hornvale_language::{GapReason, LexEntry, render_views};
use hornvale_worldgen as world_builder;

/// Render every registered species' dictionary as markdown for the book's
/// reference section, over `world`'s own generated lexicon (one row per
/// registered concept, so this stays exhaustive as the concept inventory
/// grows). Errors when `world` can't rebuild a species' phonology or
/// exposure (the same failure modes [`world_builder::lexicon_of`] surfaces).
/// type-audit: bare-ok(artifact: return)
pub fn render_dictionary(world: &World) -> Result<String, String> {
    let mut doc = String::new();
    doc.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale dictionary --world <PATH>`. -->\n\n",
    );
    doc.push_str("# Dictionary\n\n");
    doc.push_str(
        "Per-species vocabulary over the registered concept inventory: each concept's \
         gloss, its modern word (romanization + IPA), the proto-form `evolve` started \
         from, and a one-line derivation naming which sound-change rules actually fired \
         — or, when a species has no word for a concept, the gap and its recountable \
         reason (spec §5–6). Concepts are exposure-gated per species, so two species over \
         the same world can differ in which rows are roots, compounds, or gaps.\n\n",
    );

    for (species, _def) in hornvale_species::registry() {
        let lexicon = world_builder::lexicon_of(world, species).map_err(|e| e.to_string())?;

        doc.push_str(&format!("## {}\n\n", capitalize(species)));
        doc.push_str(
            "| Concept | Gloss | Word | IPA | Proto | Derivation |\n|---|---|---|---|---|---|\n",
        );
        for (concept, entry) in lexicon.entries() {
            let gloss = world
                .registry
                .concept(concept)
                .map(|c| c.doc.as_str())
                .unwrap_or("—");
            doc.push_str(&format!(
                "| `{concept}` | {gloss} | {} |\n",
                entry_row(entry)
            ));
        }
        doc.push('\n');
    }
    Ok(doc)
}

/// The four trailing columns (word, IPA, proto, derivation) of one concept's
/// row, joined by `" | "` — a root and a compound both carry a real word;
/// a gap carries none, and its reason fills the derivation column instead.
fn entry_row(entry: &LexEntry) -> String {
    match entry {
        LexEntry::Root { derivation, views } => {
            let proto = render_views(&derivation.proto).roman;
            format!(
                "{} | /{}/ | {proto} | {}",
                views.roman,
                views.ipa,
                one_liner(&proto, &derivation.steps, &views.roman)
            )
        }
        LexEntry::Compound {
            modifier,
            head,
            views,
        } => {
            format!(
                "{} | /{}/ | — | compound: `{modifier}` + `{head}`",
                views.roman, views.ipa
            )
        }
        LexEntry::Gap { reason } => {
            format!("— | — | — | {}", gap_text(reason))
        }
    }
}

/// The derivation one-liner: proto roman → the changed rule kinds, in
/// application order → modern roman. A cascade whose every rule left the
/// word untouched (its conditioning environment never occurred) renders as
/// "no change" between the arrows, rather than an empty span.
fn one_liner(
    proto_roman: &str,
    steps: &[hornvale_language::AppliedRule],
    modern_roman: &str,
) -> String {
    let changed: Vec<String> = steps
        .iter()
        .filter(|s| s.changed)
        .map(|s| format!("{:?}", s.rule.kind).to_lowercase())
        .collect();
    let rules = if changed.is_empty() {
        "no change".to_string()
    } else {
        changed.join(" → ")
    };
    format!("{proto_roman} → {rules} → {modern_roman}")
}

/// A gap's recountable reason, tagged with its provenance kind.
fn gap_text(reason: &GapReason) -> String {
    match reason {
        GapReason::Experiential(text) => format!("gap (experiential): {text}"),
        GapReason::Perceptual(text) => format!("gap (perceptual): {text}"),
    }
}

/// A single-line, REPL-friendly description of an entry: `<roman> /<ipa>/ —
/// <derivation one-liner or compound composition>` for a root or compound,
/// or the tagged gap reason alone. `pub(crate)` so the REPL's `word` verb
/// reuses the same word/derivation text this page renders, rather than a
/// second formatting that could drift from it.
pub(crate) fn word_line(entry: &LexEntry) -> String {
    match entry {
        LexEntry::Root { derivation, views } => {
            let proto = render_views(&derivation.proto).roman;
            format!(
                "{} /{}/ — {}",
                views.roman,
                views.ipa,
                one_liner(&proto, &derivation.steps, &views.roman)
            )
        }
        LexEntry::Compound {
            modifier,
            head,
            views,
        } => {
            format!(
                "{} /{}/ — compound of '{modifier}' + '{head}'",
                views.roman, views.ipa
            )
        }
        LexEntry::Gap { reason } => gap_text(reason),
    }
}

/// Capitalize a species name's first letter for section headings (mirrors
/// `phonology.rs`'s helper of the same name).
fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use world_builder::{SettlementPins, SkyChoice, build_world};

    fn reference_world() -> World {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap()
    }

    #[test]
    fn renders_a_header_row_and_one_row_per_registered_concept_per_species() {
        let world = reference_world();
        let doc = render_dictionary(&world).unwrap();
        assert!(doc.contains("<!-- GENERATED FILE"));
        assert!(doc.contains("| Concept | Gloss | Word | IPA | Proto | Derivation |"));
        for species in hornvale_species::registry().keys() {
            assert!(doc.contains(&capitalize(species)), "missing {species}");
            let lexicon = world_builder::lexicon_of(&world, species).unwrap();
            for (concept, _) in lexicon.entries() {
                assert!(
                    doc.contains(&format!("`{concept}`")),
                    "missing row for {species}/{concept}"
                );
            }
        }
    }

    #[test]
    fn a_root_row_carries_a_derivation_one_liner_and_a_compound_row_names_its_parts() {
        let world = reference_world();
        let mut saw_root = false;
        let mut saw_compound = false;
        let mut saw_gap = false;
        for species in hornvale_species::registry().keys() {
            let lexicon = world_builder::lexicon_of(&world, species).unwrap();
            for (_, entry) in lexicon.entries() {
                match entry {
                    LexEntry::Root { .. } => saw_root = true,
                    LexEntry::Compound { .. } => saw_compound = true,
                    LexEntry::Gap { .. } => saw_gap = true,
                }
            }
        }
        assert!(saw_root, "seed 42 should have at least one root");
        assert!(saw_gap, "seed 42 should have at least one gap");
        let doc = render_dictionary(&world).unwrap();
        assert!(doc.contains(" → "), "root rows need a derivation one-liner");
        assert!(doc.contains("gap ("), "gap rows need a tagged reason");
        if saw_compound {
            assert!(doc.contains("compound: `"));
        }
    }

    #[test]
    fn render_is_deterministic() {
        let world = reference_world();
        assert_eq!(
            render_dictionary(&world).unwrap(),
            render_dictionary(&world).unwrap()
        );
    }

    #[test]
    fn word_line_reads_the_same_as_the_dictionary_row_for_every_kind_of_entry() {
        let world = reference_world();
        for species in hornvale_species::registry().keys() {
            let lexicon = world_builder::lexicon_of(&world, species).unwrap();
            for (_, entry) in lexicon.entries() {
                let line = word_line(entry);
                match entry {
                    LexEntry::Root { views, .. } => {
                        assert!(line.starts_with(&views.roman));
                        assert!(line.contains(" → "));
                    }
                    LexEntry::Compound {
                        modifier,
                        head,
                        views,
                    } => {
                        assert!(line.starts_with(&views.roman));
                        assert!(line.contains(&format!("'{modifier}'")));
                        assert!(line.contains(&format!("'{head}'")));
                    }
                    LexEntry::Gap { .. } => {
                        assert!(line.starts_with("gap ("));
                    }
                }
            }
        }
    }
}
