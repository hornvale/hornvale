//! Render each species' drawn phonology as the book's generated reference
//! page: the phoneme inventory, the phonotactic templates, and a few sample
//! generated names, each in both romanization and IPA.

use hornvale_kernel::{Seed, World};
use hornvale_language::{GeneratedName, Manner, NameKind, Namer, Segment, ipa, romanize};
use hornvale_worldgen as world_builder;

/// The reference seed the page draws every species' inventory and sample
/// names from — the same canonical seed 42 used throughout the book's
/// gallery pages, so this page's output is comparable against them.
pub(crate) const REFERENCE_SEED: u64 = 42;

/// How many settlement-name samples to draw per species (in addition to one
/// deity name), each from a distinct salt so the samples show real variety.
pub(crate) const SETTLEMENT_SAMPLES: u64 = 3;

/// Render every registered species' phonology as markdown for the book's
/// reference section. Deterministic: a pure function of the reference seed
/// plus the (fixed, committed) species and language crates — no world
/// genesis beyond the bare seed is involved, so this never fails.
/// type-audit: bare-ok(artifact: return)
pub fn render_phonology() -> String {
    let world = World::new(Seed(REFERENCE_SEED));
    let mut doc = String::new();
    doc.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale phonology`. -->\n\n",
    );
    doc.push_str("# Phonology\n\n");
    doc.push_str(&format!(
        "Per-species phoneme inventories, phonotactic templates, and sample \
         name transcriptions, drawn from reference seed {REFERENCE_SEED}. The \
         `Segment` feature-bundle is the truth; romanization (the almanac's \
         spelling) and IPA (this page) are both views over it, never stored \
         independently (spec §3–4); the espeak formulation (audio column) is \
         the third view, authored to clips by `hornvale voice`.\n\n"
    ));

    // Speaker-only: the phonology page needs language, so it covers exactly the
    // speaking peoples. Since The Eremite the psyche registry is a superset (the
    // dragons carry a mind but no speech), so skip any non-speaker. `iter()` is
    // `KindId`-ascending, byte-identical to the old registry-then-filter order.
    let speakers = hornvale_language::articulation_registry();
    for (kind, _) in hornvale_species::psyche_registry().iter() {
        if speakers.get(kind).is_none() {
            continue;
        }
        let species = kind.0;
        let phonology = world_builder::language_of(&world, species);
        // The Cloister: `sample_names_for` needs only the society vector.
        // Since this loop covers every minded speaker (dragons included since
        // The Solitary Tongue), a Solitary carries no society row — resolve
        // the goblin baseline for it, mirroring genesis's mixed-consumer rule.
        let society = hornvale_species::society_registry()
            .get(kind)
            .copied()
            .unwrap_or(hornvale_species::SocietyVector::baseline());

        doc.push_str(&format!("## {}\n\n", capitalize(species)));

        doc.push_str("### Inventory\n\n");
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

        doc.push_str("### Phonotactics\n\n");
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

        doc.push_str("### Sample names\n\n");
        doc.push_str("| Kind | Romanization | IPA | Espeak | Audio |\n|---|---|---|---|---|\n");
        for (name_kind, name) in sample_names_for(&world, species, &society) {
            doc.push_str(&format!(
                "| {} | {} | /{}/ | `{}` | <audio controls preload=\"none\" src=\"../audio/{}\"></audio> |\n",
                name_kind,
                name.roman,
                name.ipa,
                name.espeak,
                crate::audio::audio_filename(&name.espeak),
            ));
        }
        doc.push('\n');
    }
    doc
}

/// The sample-name set for one species — three settlement names then one
/// deity name, in the order the page's table renders them. Shared with
/// `hornvale voice`, which authors one audio clip per entry, so the page
/// and the artifact set agree by construction.
pub(crate) fn sample_names_for(
    world: &World,
    species: &str,
    society: &hornvale_species::SocietyVector,
) -> Vec<(&'static str, GeneratedName)> {
    let phonology = world_builder::language_of(world, species);
    let namer = Namer::new(&world.seed, species, &phonology);
    let morph = world_builder::morph_options(society);
    let mut samples = Vec::new();
    for salt in 0..SETTLEMENT_SAMPLES {
        samples.push(("Settlement", namer.name(NameKind::Settlement, salt, &morph)));
    }
    samples.push(("Deity", namer.name(NameKind::Deity, 0, &morph)));
    samples
}

/// A segment's raw structural feature-bundle, compactly rendered — the
/// truth `romanize`/`ipa` are views over, distinct from
/// [`feature_description`]'s plain-English gloss.
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

/// A plain-English gloss of a segment's features (e.g. "voiceless labial
/// stop"), distinct from [`segment_label`]'s raw structural form.
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

/// Lowercase a type's `Debug` rendering (every feature enum here derives
/// `Debug` as its canonical name, e.g. `Labial`, `Stop`).
fn lower<T: std::fmt::Debug>(value: &T) -> String {
    format!("{value:?}").to_lowercase()
}

/// Render a list of manner-slot templates for the phonotactics section:
/// each template as its manners joined by `+` (an empty template is the
/// open-syllable marker `∅`), templates comma-separated. An empty template
/// list (never produced by `draw_phonology` today, but handled here for
/// robustness) reads as `*(none)*`.
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

/// Capitalize a species name's first letter for section headings.
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

    #[test]
    fn renders_every_species_with_an_ipa_column_and_sample_names() {
        let doc = render_phonology();
        // Speaker-only: the page renders a heading per speaking people, so it
        // covers exactly the articulation key-set (the speakers).
        for kind in hornvale_language::articulation_registry().ids() {
            let species = kind.0;
            assert!(
                doc.contains(&capitalize(species)),
                "missing species heading for {species}"
            );
        }
        assert!(doc.contains("IPA"), "missing an IPA column/section");
        assert!(
            doc.contains("Romanization"),
            "missing a romanization column"
        );
        assert!(
            doc.contains("| Settlement |") && doc.contains("| Deity |"),
            "missing settlement/deity sample name rows"
        );
    }

    /// At least one sample name row must carry both a non-empty
    /// romanization and a non-empty IPA transcription — the brief's literal
    /// requirement, checked directly against a drawn name rather than by
    /// string-scraping the rendered table.
    #[test]
    fn a_sample_name_carries_both_romanization_and_ipa() {
        let world = World::new(Seed(REFERENCE_SEED));
        let psyche = hornvale_species::psyche_registry();
        let articulation = hornvale_language::articulation_registry();
        // A SPEAKING people: since The Eremite the psyche registry is a superset
        // (the dragons carry a mind but no speech, and sort ahead of the peoples
        // by KindId), so pick the first psyche-carrier that also speaks.
        let (kind, _) = psyche
            .iter()
            .find(|&(k, _)| articulation.contains(k))
            .expect("at least one speaking people");
        let species = kind.0;
        let society = hornvale_species::society_registry()
            .get(kind)
            .copied()
            .unwrap_or(hornvale_species::SocietyVector::baseline());
        let phonology = world_builder::language_of(&world, species);
        let namer = Namer::new(&world.seed, species, &phonology);
        let morph = world_builder::morph_options(&society);
        let name = namer.name(NameKind::Settlement, 0, &morph);
        assert!(!name.roman.is_empty(), "romanization must not be empty");
        assert!(!name.ipa.is_empty(), "IPA transcription must not be empty");
    }

    #[test]
    fn render_is_deterministic() {
        assert_eq!(render_phonology(), render_phonology());
    }

    /// Every sample-name row must reference a content-addressed audio clip
    /// whose filename is the CRC-32 of that row's espeak formulation — the
    /// page and `hornvale voice` must agree on names by construction.
    #[test]
    fn sample_rows_carry_espeak_and_content_addressed_audio() {
        let doc = render_phonology();
        assert!(doc.contains("Espeak"), "missing the Espeak column");
        let world = World::new(Seed(REFERENCE_SEED));
        let psyche = hornvale_species::psyche_registry();
        let articulation = hornvale_language::articulation_registry();
        // A SPEAKING people: since The Eremite the psyche registry is a superset
        // (the dragons carry a mind but no speech, and sort ahead of the peoples
        // by KindId), so pick the first psyche-carrier that also speaks.
        let (kind, _) = psyche
            .iter()
            .find(|&(k, _)| articulation.contains(k))
            .expect("at least one speaking people");
        let society = hornvale_species::society_registry()
            .get(kind)
            .copied()
            .unwrap_or(hornvale_species::SocietyVector::baseline());
        let samples = sample_names_for(&world, kind.0, &society);
        assert_eq!(samples.len(), SETTLEMENT_SAMPLES as usize + 1);
        for (_, name) in &samples {
            assert!(
                doc.contains(&format!("`{}`", name.espeak)),
                "page must show formulation {}",
                name.espeak
            );
            assert!(
                doc.contains(&format!(
                    "src=\"../audio/{}\"",
                    crate::audio::audio_filename(&name.espeak)
                )),
                "page must reference {}'s clip by its CRC-32 name",
                name.roman
            );
        }
    }
}
