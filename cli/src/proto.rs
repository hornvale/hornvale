//! Render a language family's proto as the book's generated reference page:
//! the shared ancestral phoneme inventory and phonotactics every daughter
//! nativizes from (spec §3–4), plus the full proto-root table over the
//! registered concept inventory. Sibling of `phonology` (per-species) and
//! `dictionary` (per-species, over a world's own lexicon), but at the family
//! level: a proto has no speakers of its own, only daughters, so this page
//! has no "sample names" section — see the dictionary's Cognates section for
//! how each daughter's cascade nativizes these very roots.
//!
//! One page per multi-member family; the caller names the family. Until The
//! Delvers this module hardcoded `goblinoid` (the roster's only multi-member
//! family at the time) and hardcoded its daughter list in the intro prose.
//! Both are now derived, so a family's page can never name a set of daughters
//! the roster disagrees with.
#![warn(missing_docs)]

use hornvale_kernel::{Correspondent, Seed, Void, World};
use hornvale_language::{Manner, Segment, assign_proto_roots, ipa, render_views, romanize};
use hornvale_worldgen as world_builder;

/// The reference seed this page's proto-root table is drawn from — the
/// same canonical seed 42 `phonology`/`dictionary`'s reference pages use,
/// so this page's forms are directly comparable against them.
pub(crate) const REFERENCE_SEED: u64 = 42;

/// The family `hornvale proto` renders when the caller names none — the
/// roster's first multi-member family, and the one whose page predates the
/// family argument.
pub(crate) const DEFAULT_FAMILY: &str = "goblinoid";

/// Resolve `family` to the `&'static str` label the composition root's
/// family-keyed reads want, failing loudly with the admissible set if this
/// roster has no proto for it. Two rejections, both of which would otherwise
/// render a junk page: a label with no [`hornvale_language::family_proto`]
/// entry (`proto_phonology_of` *panics* on one — a panic is loud but it is
/// not a CLI error), and a label with a proto but no speaking daughter, whose
/// page would open "The shared ancestral language  all descend from".
fn resolve_family(
    wc: &world_builder::WorldComponents,
    family: &str,
) -> Result<&'static str, String> {
    let known: Vec<&'static str> = wc.family_proto.ids().map(|k| k.0).collect();
    let resolved = known
        .iter()
        .copied()
        .find(|k| *k == family)
        .ok_or_else(|| {
            format!(
                "proto: unknown family '{family}' (families with a proto vector: {})",
                known.join(", ")
            )
        })?;
    if world_builder::family_daughter_kinds(wc, resolved).is_empty() {
        return Err(format!(
            "proto: family '{resolved}' has a proto vector but no speaking daughter — nothing to \
             render a proto page for"
        ));
    }
    Ok(resolved)
}

/// The family's daughters as a prose list in the page's own ascending-`KindId`
/// order: `"a"`, `"a and b"`, `"a, b, and c"`.
fn and_list(names: &[&str]) -> String {
    match names {
        [] => String::new(),
        [one] => (*one).to_string(),
        [a, b] => format!("{a} and {b}"),
        [rest @ .., last] => format!("{}, and {last}", rest.join(", ")),
    }
}

/// Render `family`'s proto phoneme inventory, phonotactics, and full
/// proto-root table as markdown for the book's reference section.
/// Deterministic: a pure function of `family`, the reference seed, the
/// (fixed, committed) family-proto envelope, and the concept registry
/// [`world_builder::register_all`] builds fresh — no terrain/settlement
/// genesis is involved, so the only failure is an inadmissible `family`.
/// type-audit: bare-ok(identifier-text: family), bare-ok(artifact: return)
pub fn render_proto(family: &str) -> Result<String, String> {
    let mut world = World::new(Seed(REFERENCE_SEED));
    world_builder::register_all(&mut world.registry).map_err(|e| e.to_string())?;
    let wc = world_builder::WorldComponents::assemble().map_err(|e| e.to_string())?;
    let family = resolve_family(&wc, family)?;
    let phonology = world_builder::proto_phonology_of(&world, family);
    // The intro's daughter list comes from the SAME membership function
    // `family_daughters` (below) derives the merger-aware assignment from, so
    // the sentence and the table can never name different families. It reads
    // in ascending `KindId` order, which is why goblinoid's page says
    // "bugbear, goblin, and hobgoblin" where the hand-authored sentence this
    // replaced said "goblin, hobgoblin, and bugbear".
    let daughter_names: Vec<&str> = world_builder::family_daughter_kinds(&wc, family)
        .iter()
        .map(|k| k.0)
        .collect();

    let mut doc = String::new();
    doc.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale proto {family}`. -->\n\n"
    ));
    doc.push_str(&format!("# Proto-{family}\n\n"));
    doc.push_str(&format!(
        "The shared ancestral language {} all descend from (spec \
         §3–4): a phonology with no speakers of its own, drawn once at the family level from \
         reference seed {REFERENCE_SEED} and the family's authored ancestral articulation \
         vector (`hornvale_language::family_proto`). Every registered concept's proto-root \
         below is assigned injectively over the whole concept universe from this inventory \
         (`hornvale_language::assign_proto_roots`, epoch `root/v3` — merger-aware, so no two \
         core concepts collide even after a daughter's cascade), \
         independent of any daughter's actual exposure — the ancestral vocabulary exists \
         whether or not a given daughter still holds it as a root today. **Excepted:** a \
         concept the registry itself records as objectively unnameable \
         (`Correspondent::Absent(Void::Unnamed(..))`, spec: The Correspondence) reserves no \
         proto-root at all and is omitted from this table entirely — the ancestor cannot have \
         spoken of a referent no culture here has ever had the concept to name (see \
         `hornvale_language::GapReason::Unnameable`). The \
         [dictionary](./dictionary-generated.md#cognates)'s Cognates section shows each \
         daughter's own sound-change cascade nativizing these same roots into its modern \
         reflex.\n\n",
        and_list(&daughter_names)
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
        "- **Nuclei:** {}\n",
        nucleus_list(&phonology.nuclei)
    ));
    doc.push_str(&format!(
        "- **Codas:** {}\n\n",
        template_list(&phonology.codas)
    ));

    doc.push_str("## Proto-root table\n\n");
    doc.push_str("| Concept | Gloss | Proto | IPA |\n|---|---|---|---|\n");
    // Exclude any concept the registry itself records as objectively
    // unnameable (`Correspondent::Absent(Void::Unnamed(..))`) from the
    // proto-root universe entirely — read from the registry, never
    // hardcoded by name, the same exclusion
    // `windows/worldgen`'s exposure classifier applies via
    // `GapReason::Unnameable`. Otherwise this page would keep asserting the
    // ancestor had a word for a referent no culture here can name at all.
    let universe: Vec<&str> = world
        .registry
        .concepts()
        .map(|c| c.name.as_str())
        .filter(|name| !is_unnameable(&world, name))
        .collect();
    // The merger-aware assignment (epoch root/v3): the same daughters the
    // composition root feeds `build_lexicon`, so this page's proto-roots are
    // exactly the ones the dictionary's modern forms descend from.
    let daughters = world_builder::family_daughters(&world, &wc, family);
    let assignment = assign_proto_roots(&world.seed, family, &phonology, &universe, &daughters);
    for concept in world.registry.concepts() {
        if is_unnameable(&world, &concept.name) {
            continue;
        }
        let proto = &assignment[&concept.name];
        let views = render_views(proto);
        doc.push_str(&format!(
            "| `{}` | {} | *{} | /{}/ |\n",
            concept.name, concept.doc, views.roman, views.ipa
        ));
    }

    Ok(doc)
}

/// True when `world`'s registry records `concept`'s lexeme edge as
/// `Correspondent::Absent(Void::Unnamed(..))` — a referent real in the world
/// that no culture here has any concept to name at all, so no proto-root may
/// be reserved for it. Read straight from the registry; never a hardcoded
/// concept-name list, so a future `Void::Unnamed` registration is excluded
/// automatically.
fn is_unnameable(world: &World, concept: &str) -> bool {
    matches!(
        world.registry.manifest(concept).map(|m| &m.lexeme),
        Some(Correspondent::Absent(Void::Unnamed(_)))
    )
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
            ..
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
            ..
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

/// Render the nucleus template set for the phonotactics section: each
/// admissible size as that many `v` slots joined by `+` (`v`, `v+v`),
/// comma-separated. Mirrors `template_list`'s shape, because since The
/// Wearing the nucleus IS a template set rather than a single obligatory
/// count. An empty set (never produced by `draw_phonology`, which always
/// admits the simple vowel) reads as `*(none)*`.
fn nucleus_list(sizes: &[usize]) -> String {
    if sizes.is_empty() {
        return "*(none)*".to_string();
    }
    sizes
        .iter()
        .map(|size| vec!["v"; *size].join("+"))
        .collect::<Vec<_>>()
        .join(", ")
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

    /// Both pages this repo emits today. Every structural assertion below
    /// runs over both, so the dwarf page is covered by the same checks the
    /// goblinoid page has always had rather than by inspection.
    const RENDERED_FAMILIES: [&str; 2] = ["goblinoid", "dwarf"];

    #[test]
    fn renders_inventory_phonotactics_and_a_proto_root_table() {
        for family in RENDERED_FAMILIES {
            let doc = render_proto(family).unwrap();
            assert!(doc.contains("<!-- GENERATED FILE"), "{family}");
            assert!(doc.contains(&format!("# Proto-{family}")), "{family}");
            assert!(doc.contains("## Inventory"), "{family}");
            assert!(doc.contains("## Phonotactics"), "{family}");
            assert!(doc.contains("## Proto-root table"), "{family}");
            assert!(
                doc.contains("| Concept | Gloss | Proto | IPA |"),
                "{family}"
            );
            // A proto-root reads as a marked reconstruction, e.g. `*Kab`.
            assert!(
                doc.contains(" | *"),
                "{family}: proto-root rows must be `*`-marked"
            );
        }
    }

    /// A proto page has no speakers of its own — it must never claim to be
    /// any daughter's own phonology, and it must never carry a section for
    /// a species of an unrelated family. Checked against the union of both
    /// families' daughters plus the kobold outgroup, so neither page may
    /// grow a per-species section.
    #[test]
    fn never_names_a_daughter_species_or_the_unrelated_kobold_outgroup() {
        for family in RENDERED_FAMILIES {
            let doc = render_proto(family).unwrap();
            for daughter in [
                "Goblin",
                "Hobgoblin",
                "Bugbear",
                "Kobold",
                "Desert-dwarf",
                "Gully-dwarf",
                "Hill-dwarf",
            ] {
                assert!(
                    !doc.contains(&format!("## {daughter}")),
                    "proto-{family} page must not carry a per-species section for {daughter}"
                );
            }
        }
    }

    /// The intro sentence names the family's daughters and nobody else's —
    /// the property that replaced a hand-authored list. Each page must name
    /// every one of its own daughters and none of its sibling page's.
    #[test]
    fn the_intro_names_exactly_this_familys_daughters() {
        let wc = world_builder::WorldComponents::assemble().unwrap();
        for family in RENDERED_FAMILIES {
            let doc = render_proto(family).unwrap();
            let intro = doc.split("## Inventory").next().unwrap();
            for kind in world_builder::family_daughter_kinds(&wc, family) {
                assert!(
                    intro.contains(kind.0),
                    "proto-{family}'s intro must name its daughter {}",
                    kind.0
                );
            }
            for other in RENDERED_FAMILIES.iter().filter(|f| **f != family) {
                for kind in world_builder::family_daughter_kinds(&wc, other) {
                    assert!(
                        !intro.contains(kind.0),
                        "proto-{family}'s intro must not name {}, a {other} daughter",
                        kind.0
                    );
                }
            }
        }
    }

    #[test]
    fn render_is_deterministic() {
        for family in RENDERED_FAMILIES {
            assert_eq!(render_proto(family).unwrap(), render_proto(family).unwrap());
        }
    }

    /// An unregistered family must fail with the admissible set, never
    /// render a page. Before this guard `proto_phonology_of` panicked
    /// instead — loud, but not a CLI error a caller can print.
    #[test]
    fn an_unknown_family_is_refused_with_the_admissible_set() {
        let err = render_proto("elf").unwrap_err();
        assert!(err.contains("unknown family 'elf'"), "{err}");
        assert!(err.contains("goblinoid"), "{err}");
        assert!(err.contains("dwarf"), "{err}");
    }

    /// A family label that carries a proto vector but no speaking daughter
    /// (`plant`: treant and twig-blight have no articulation) is refused
    /// too — its page's intro would read "The shared ancestral language  all
    /// descend from".
    #[test]
    fn a_family_with_no_speaking_daughter_is_refused() {
        let err = render_proto("plant").unwrap_err();
        assert!(err.contains("no speaking daughter"), "{err}");
    }

    #[test]
    fn and_list_reads_as_prose() {
        assert_eq!(and_list(&[]), "");
        assert_eq!(and_list(&["a"]), "a");
        assert_eq!(and_list(&["a", "b"]), "a and b");
        assert_eq!(and_list(&["a", "b", "c"]), "a, b, and c");
    }
}
