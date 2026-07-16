//! Render each species' generated lexicon as the book's dictionary
//! reference page: one table per species, one row per registered concept —
//! its gloss, its modern word (roman + IPA), its proto-form, and a one-line
//! derivation; or, for a concept with no word, the gap and its recountable
//! reason. Sibling of `phonology`, but over a world's own lexicon
//! ([`hornvale_worldgen::lexicon_of`]) rather than a fixed reference seed:
//! exposure (and so every gap) depends on what a world actually settled.
#![warn(missing_docs)]

use hornvale_kernel::World;
use hornvale_language::{GapReason, LexEntry, Lexicon, render_views};
use hornvale_worldgen as world_builder;
use std::collections::BTreeMap;

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

    // Lexicons are a speaking-peoples concept (fauna carry no
    // `PeopledTraits` and never speak); the Task 4 menagerie widened
    // `registry()` to include biosphere-only kinds, so both passes here
    // filter to `peopled.is_some()` before ever reaching `lexicon_of`.
    let mut lexicons: BTreeMap<&str, Lexicon> = BTreeMap::new();
    for (species, def) in hornvale_species::registry() {
        if def.peopled.is_none() {
            continue;
        }
        let lexicon = world_builder::lexicon_of(world, species.0).map_err(|e| e.to_string())?;
        lexicons.insert(species.0, lexicon);
    }

    for (species, def) in hornvale_species::registry() {
        if def.peopled.is_none() {
            continue;
        }
        let lexicon = &lexicons[species.0];

        doc.push_str(&format!("## {}\n\n", capitalize(species.0)));
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

    doc.push_str(&render_cognates(world, &lexicons));
    Ok(doc)
}

/// The "## Cognates" section: for every family shared by more than one
/// registered species ([`hornvale_species::family_registry`]'s entries —
/// currently just goblinoid), one table per family of every concept rooted
/// (spec §3–4, `LexEntry::Root`) in *all* of that family's daughters —
/// their shared proto-form (identical across daughters by construction,
/// since `build_lexicon` draws it once at the family level) beside each
/// daughter's own evolved reflex. A concept a species has forgotten, or
/// never inherited as a root (a gap or compound instead), is silently
/// excluded rather than padded — every surviving row is a genuine,
/// three-way-attested cognate set. Kobold belongs to its own singleton
/// family (absent from `family_registry`, spec §3) and so never enters
/// this loop: excluded by construction, not filtered out, so it can never
/// render here as a false cognate.
fn render_cognates(world: &World, lexicons: &BTreeMap<&str, Lexicon>) -> String {
    let mut doc = String::new();
    doc.push_str("## Cognates\n\n");
    doc.push_str(
        "A family's daughters share one proto-root per concept, drawn once at the family \
         level and evolved through each daughter's own sound-change cascade (spec §3–4): the \
         same ancestral word, nativized three different ways. Below, every concept rooted in \
         *all* of a family's daughters, glossed `*proto → daughter / daughter / daughter` — \
         only concepts every daughter still has a root for, so every row is attested, not \
         padded. A singleton family (e.g. kobold) has no cognates and is not listed here; see \
         its own section above instead.\n\n",
    );

    let registry = hornvale_species::registry();
    for family in hornvale_species::family_registry().keys() {
        // `def.peopled.is_some()` excludes fauna families (draconic, plant)
        // — they carry no lexicon (`lexicons` above was built peopled-only),
        // so this leaves the goblinoid triad the only family that clears
        // the `daughters.len() < 2` guard below.
        let daughters: Vec<&str> = registry
            .iter()
            .filter(|(_, def)| def.family == family.0 && def.peopled.is_some())
            .map(|(name, _)| name.0)
            .collect();
        if daughters.len() < 2 {
            // A family entry with fewer than two peopled daughters (e.g. a
            // fauna family, which has none) is guarded rather than assumed,
            // so it fails safe (no section) instead of rendering a
            // one-column "cognate" table or indexing a missing lexicon.
            continue;
        }

        doc.push_str(&format!("### {}\n\n", capitalize(family.0)));
        doc.push_str("| Concept | Gloss | Proto |");
        for daughter in &daughters {
            doc.push_str(&format!(" {} |", capitalize(daughter)));
        }
        doc.push_str(" Descent |\n|---|---|---|");
        doc.push_str(&"---|".repeat(daughters.len() + 1));
        doc.push('\n');

        let first = &lexicons[daughters[0]];
        for (concept, entry) in first.entries() {
            let LexEntry::Root {
                derivation: proto_derivation,
                ..
            } = entry
            else {
                continue;
            };
            let mut daughter_forms = Vec::with_capacity(daughters.len());
            let mut all_rooted = true;
            for daughter in &daughters {
                match lexicons[daughter].entry(concept) {
                    Some(LexEntry::Root { views, .. }) => daughter_forms.push(views.clone()),
                    _ => {
                        all_rooted = false;
                        break;
                    }
                }
            }
            if !all_rooted {
                continue;
            }

            let gloss = world
                .registry
                .concept(concept)
                .map(|c| c.doc.as_str())
                .unwrap_or("—");
            let proto = render_views(&proto_derivation.proto);
            doc.push_str(&format!(
                "| `{concept}` | {gloss} | *{} /{}/ |",
                proto.roman, proto.ipa
            ));
            for views in &daughter_forms {
                doc.push_str(&format!(" {} /{}/ |", views.roman, views.ipa));
            }
            let descent = daughter_forms
                .iter()
                .map(|v| v.roman.as_str())
                .collect::<Vec<_>>()
                .join(" / ");
            doc.push_str(&format!(" *{} → {descent} |\n", proto.roman));
        }
        doc.push('\n');
    }
    doc
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
        // peopled-only: fauna never speak, so `lexicon_of` is undefined for
        // them (Task 4 widened `registry()` to include biosphere-only kinds).
        for species in hornvale_species::registry()
            .iter()
            .filter(|(_, def)| def.peopled.is_some())
            .map(|(name, _)| name.0)
        {
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
        // peopled-only: fauna never speak, so `lexicon_of` is undefined for
        // them (Task 4 widened `registry()` to include biosphere-only kinds).
        for species in hornvale_species::registry()
            .iter()
            .filter(|(_, def)| def.peopled.is_some())
            .map(|(name, _)| name.0)
        {
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

    /// The cognate section: a goblinoid concept rooted in all three
    /// daughters must render one row carrying the glossed proto-form (a
    /// `*`-marked reconstruction, spec §3) beside every daughter's modern
    /// reflex — the family made visible. Kobold is the unrelated outgroup
    /// (its own singleton family, spec §3): it must never appear inside the
    /// goblinoid cognate table as a false cognate.
    #[test]
    fn cognate_rows_share_a_glossed_proto_form_across_the_goblinoid_daughters() {
        let world = reference_world();
        let doc = render_dictionary(&world).unwrap();
        assert!(doc.contains("## Cognates"), "missing a Cognates section");
        assert!(
            doc.contains("### Goblinoid"),
            "missing the goblinoid family subsection"
        );

        let goblin = world_builder::lexicon_of(&world, "goblin").unwrap();
        let hobgoblin = world_builder::lexicon_of(&world, "hobgoblin").unwrap();
        let bugbear = world_builder::lexicon_of(&world, "bugbear").unwrap();
        let shared_root = goblin
            .entries()
            .find(|(concept, entry)| {
                matches!(entry, LexEntry::Root { .. })
                    && matches!(hobgoblin.entry(concept), Some(LexEntry::Root { .. }))
                    && matches!(bugbear.entry(concept), Some(LexEntry::Root { .. }))
            })
            .map(|(concept, _)| concept.to_string())
            .expect("seed 42 should share at least one root across all three goblinoid daughters");

        let cognates = doc
            .split("## Cognates")
            .nth(1)
            .expect("a Cognates section body");
        assert!(
            cognates.contains(&format!("`{shared_root}`")),
            "missing cognate row for {shared_root}"
        );
        assert!(
            cognates.contains('*'),
            "cognate rows should mark the proto-form as a reconstruction"
        );
        assert!(
            !cognates.contains("Kobold") && !cognates.contains("### Kobold"),
            "kobold is a singleton family (unrelated outgroup) — it must never appear as a \
             false cognate in the goblinoid table"
        );
    }

    #[test]
    fn word_line_reads_the_same_as_the_dictionary_row_for_every_kind_of_entry() {
        let world = reference_world();
        // peopled-only: fauna never speak, so `lexicon_of` is undefined for
        // them (Task 4 widened `registry()` to include biosphere-only kinds).
        for species in hornvale_species::registry()
            .iter()
            .filter(|(_, def)| def.peopled.is_some())
            .map(|(name, _)| name.0)
        {
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
