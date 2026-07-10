//! Render proto-goblinoid as the book's generated reference page: the
//! shared ancestral phoneme inventory and phonotactics goblin, hobgoblin,
//! and bugbear all nativize from (spec §3–4), plus the full proto-root
//! table over the registered concept inventory. Sibling of `phonology`
//! (per-species) and `dictionary` (per-species, over a world's own
//! lexicon), but at the family level: proto-goblinoid has no speakers of
//! its own, only daughters, so this page has no "sample names" section —
//! see the dictionary's Cognates section for how each daughter's cascade
//! nativizes these very roots.
#![warn(missing_docs)]

use hornvale_kernel::{Seed, World};
use hornvale_language::{Manner, Segment, ipa, proto_root, render_views, romanize};
use hornvale_worldgen as world_builder;

/// The reference seed this page's proto-root table is drawn from — the
/// same canonical seed 42 `phonology`/`dictionary`'s reference pages use,
/// so this page's forms are directly comparable against them.
pub(crate) const REFERENCE_SEED: u64 = 42;

/// The one family this page documents: the campaign's only multi-member
/// family today ([`hornvale_species::family_registry`]). A second family
/// would need either a second page or this function generalized to take a
/// family argument — deferred until that need is real.
pub(crate) const FAMILY: &str = "goblinoid";

/// Render proto-goblinoid's phoneme inventory, phonotactics, and full
/// proto-root table as markdown for the book's reference section.
/// Deterministic: a pure function of the reference seed, the (fixed,
/// committed) family-registry envelope, and the concept registry
/// [`world_builder::register_all`] builds fresh — no terrain/settlement
/// genesis is involved, so this never fails.
/// type-audit: bare-ok(artifact: return)
pub fn render_proto() -> Result<String, String> {
    let mut world = World::new(Seed(REFERENCE_SEED));
    world_builder::register_all(&mut world.registry).map_err(|e| e.to_string())?;
    let phonology = world_builder::proto_phonology_of(&world, FAMILY);

    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale proto`. -->\n\n");
    doc.push_str("# Proto-goblinoid\n\n");
    doc.push_str(&format!(
        "The shared ancestral language goblin, hobgoblin, and bugbear all descend from (spec \
         §3–4): a phonology with no speakers of its own, drawn once at the family level from \
         reference seed {REFERENCE_SEED} and the family's authored ancestral articulation \
         vector (`hornvale_species::family_registry`). Every registered concept's proto-root \
         below is drawn directly from this inventory (`hornvale_language::proto_root`), \
         independent of any daughter's actual exposure — the ancestral vocabulary exists \
         whether or not a given daughter still holds it as a root today. The \
         [dictionary](./dictionary-generated.md#cognates)'s Cognates section shows each \
         daughter's own sound-change cascade nativizing these same roots into its modern \
         reflex.\n\n"
    ));

    doc.push_str("## Inventory\n\n");
    doc.push_str("| Segment | Romanization | IPA | Features |\n|---|---|---|---|\n");
    for seg in &phonology.inventory {
        doc.push_str(&format!(
            "| {} | `{}` | `{}` | {} |\n",
            segment_label(seg),
            romanize(seg),
            ipa(seg),
            feature_description(seg),
        ));
    }
    doc.push('\n');

    doc.push_str("## Phonotactics\n\n");
    doc.push_str(&format!(
        "- **Onsets:** {}\n",
        template_list(&phonology.onsets)
    ));
    doc.push_str(&format!(
        "- **Nuclei:** {} slot(s) per syllable\n",
        phonology.nuclei
    ));
    doc.push_str(&format!(
        "- **Codas:** {}\n\n",
        template_list(&phonology.codas)
    ));

    doc.push_str("## Proto-root table\n\n");
    doc.push_str("| Concept | Gloss | Proto | IPA |\n|---|---|---|---|\n");
    for concept in world.registry.concepts() {
        let proto = proto_root(&world.seed, FAMILY, &concept.name, &phonology);
        let views = render_views(&proto);
        doc.push_str(&format!(
            "| `{}` | {} | *{} | /{}/ |\n",
            concept.name, concept.doc, views.roman, views.ipa
        ));
    }

    Ok(doc)
}

/// A segment's raw structural feature-bundle, compactly rendered — mirrors
/// `phonology.rs`'s helper of the same name.
fn segment_label(seg: &Segment) -> String {
    match seg {
        Segment::Consonant {
            place,
            manner,
            voiced,
        } => format!(
            "{place:?}/{manner:?}/{}",
            if *voiced { "voiced" } else { "voiceless" }
        ),
        Segment::Vowel {
            height,
            backness,
            rounded,
        } => format!(
            "{height:?}/{backness:?}/{}",
            if *rounded { "rounded" } else { "unrounded" }
        ),
    }
}

/// A plain-English gloss of a segment's features — mirrors `phonology.rs`'s
/// helper of the same name.
fn feature_description(seg: &Segment) -> String {
    match seg {
        Segment::Consonant {
            place,
            manner,
            voiced,
        } => format!(
            "{} {} {}",
            if *voiced { "voiced" } else { "voiceless" },
            lower(place),
            lower(manner),
        ),
        Segment::Vowel {
            height,
            backness,
            rounded,
        } => format!(
            "{} {} {}vowel",
            lower(height),
            lower(backness),
            if *rounded { "rounded " } else { "" },
        ),
    }
}

/// Lowercase a type's `Debug` rendering — mirrors `phonology.rs`'s helper
/// of the same name.
fn lower<T: std::fmt::Debug>(value: &T) -> String {
    format!("{value:?}").to_lowercase()
}

/// Render a list of manner-slot templates for the phonotactics section —
/// mirrors `phonology.rs`'s helper of the same name.
fn template_list(templates: &[Vec<Manner>]) -> String {
    if templates.is_empty() {
        return "*(none)*".to_string();
    }
    templates
        .iter()
        .map(|template| {
            if template.is_empty() {
                "∅".to_string()
            } else {
                template.iter().map(lower).collect::<Vec<_>>().join("+")
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_inventory_phonotactics_and_a_proto_root_table() {
        let doc = render_proto().unwrap();
        assert!(doc.contains("<!-- GENERATED FILE"));
        assert!(doc.contains("# Proto-goblinoid"));
        assert!(doc.contains("## Inventory"));
        assert!(doc.contains("## Phonotactics"));
        assert!(doc.contains("## Proto-root table"));
        assert!(doc.contains("| Concept | Gloss | Proto | IPA |"));
        // A proto-root reads as a marked reconstruction, e.g. `*Kab`.
        assert!(doc.contains(" | *"), "proto-root rows must be `*`-marked");
    }

    /// This page has no daughters (proto-goblinoid has no speakers of its
    /// own) — it must never claim to be goblin, hobgoblin, or bugbear's own
    /// phonology, and it must never mention kobold (a different, unrelated
    /// singleton family).
    #[test]
    fn never_names_a_daughter_species_or_the_unrelated_kobold_outgroup() {
        let doc = render_proto().unwrap();
        for daughter in ["Goblin", "Hobgoblin", "Bugbear", "Kobold"] {
            assert!(
                !doc.contains(&format!("## {daughter}")),
                "proto page must not carry a per-species section for {daughter}"
            );
        }
    }

    #[test]
    fn render_is_deterministic() {
        assert_eq!(render_proto().unwrap(), render_proto().unwrap());
    }
}
