//! The gazetteer page (The Gazetteer campaign): every named landscape
//! feature, capped and ordered by magnitude, with every people's name for
//! it (spec §3.4 — a feature has no primary name, so a surface with no
//! observer joins every name it has, species-label ascending, with no
//! lead).
//!
//! **Never wired into the everyday almanac document.** `render` over
//! `AlmanacContext` (`crate::render`) already produces the three committed
//! `almanac-seed-42*.md` pages, and those must not move when landscape
//! naming ships — a drift-check STOP condition (naming a feature draws
//! nothing new, but appending a section to those pages would still move
//! their bytes). So the gazetteer is its own page, built by its own CLI
//! command (`hornvale gazetteer`), never folded into [`crate::render`].
//!
//! This module cannot assemble its own entries: a feature's identity comes
//! from `hornvale-terrain` (already this crate's dependency) but its *name*
//! comes from `hornvale-worldgen`'s `feature_name`/`gazetteer_features` —
//! and `windows/worldgen` already depends on `windows/almanac` (for
//! `AlmanacContext`), so the edge cannot run the other way without a cycle,
//! the same constraint `crate::connections`'s module doc records for
//! `ConnectionGraph`. The composition root sorts, caps and names each
//! feature into an [`Entry`]; this module only formats what it is handed.
//!
//! **`windows/explain` calls into this module directly** (`hornvale-explain`
//! depends on `hornvale-almanac`) rather than carrying its own copy of
//! [`render_names`]/[`class_words`]: windows may depend on other windows
//! (`windows/CLAUDE.md` — the rank check only forbids depending upward on
//! `cli`; `hornvale-almanac` itself carries zero window-layer dependencies,
//! so the edge is acyclic), and the only edge actually forbidden here is a
//! window reaching `windows/worldgen`'s naming join (the cycle above). An
//! earlier version of this doc comment claimed the two windows could not
//! depend on each other at all — that was checked and found false; see the
//! Gazetteer campaign's Task 8 fix round 1.

use hornvale_kernel::Vertex;
use hornvale_terrain::landscape::FeatureClass;
use std::collections::BTreeMap;

/// One feature's gazetteer row: where it is, how large it is, and every
/// people's name for it — keyed on species label so iterating `names` in
/// key order gives species-label-ascending for free (spec §3.4's
/// "no observer" ordering). There is no primary-name field here or
/// anywhere in this campaign.
/// type-audit: bare-ok(count: magnitude), bare-ok(identifier-text: names)
pub struct Entry {
    /// The vertex a label is drawn at (`hornvale_terrain::landscape::Feature::anchor`).
    pub anchor: Vertex,
    /// The integer scalar ranking this feature within its class.
    pub magnitude: u32,
    /// Every people's name for this feature, keyed by species label.
    pub names: BTreeMap<String, String>,
}

/// A feature's info line: every name it carries, joined —
/// `"Mount McKinley, Denali"` (spec §3.4). `names` iterates in key
/// (species-label) order, so this is deterministic and elects no lead.
/// `hornvale_explain::explain_gazetteer` calls this directly (see this
/// module's doc comment) rather than carrying its own copy.
/// type-audit: bare-ok(identifier-text: names), bare-ok(identifier-text: return)
pub fn render_names(names: &BTreeMap<String, String>) -> String {
    names.values().cloned().collect::<Vec<_>>().join(", ")
}

/// One class's singular and plural label, for the section header and the
/// printed-cap line, and for `windows/explain`'s narration
/// ([`crate::gazetteer`]'s module doc on why that window calls this
/// directly instead of carrying its own copy).
/// type-audit: bare-ok(identifier-text: return)
pub fn class_words(class: FeatureClass) -> (&'static str, &'static str) {
    match class {
        FeatureClass::Volcano => ("volcano", "volcanoes"),
        FeatureClass::Landmass => ("landmass", "landmasses"),
        FeatureClass::Sea => ("sea", "seas"),
        FeatureClass::SaltLake => ("salt lake", "salt lakes"),
        FeatureClass::River => ("river", "rivers"),
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Render the full gazetteer page: one section per class, in `per_class`'s
/// given order, each showing every entry the caller kept — magnitude
/// descending, identity ascending, the same total order `FeatureIndex` and
/// `gazetteer_features` already carry (the caller sorts and caps before
/// calling; this function only formats what it is given).
///
/// **The cap is printed, never silent.** With 15 peoples and up to 208
/// volcanoes on seed 42, showing every name of every feature is 6,075 names
/// on one page, not a document a person would read. Each section instead
/// reads "showing the `{shown}` largest of `{total}` `{class}`" —
/// `entries.len()` (`shown`), never the nominal `cap`, so a class with fewer
/// features than the cap (this world's single sea) never overstates what is
/// shown.
/// type-audit: bare-ok(artifact: return), bare-ok(constructor-edge: seed), bare-ok(count: cap), bare-ok(count: per_class)
pub fn render(seed: u64, cap: usize, per_class: &[(FeatureClass, usize, Vec<Entry>)]) -> String {
    let mut doc = String::new();
    doc.push_str(&format!("# The Gazetteer of Seed {seed}\n\n"));
    doc.push_str(&format!(
        "Every named landscape feature, capped at the {cap} largest per class \
         (magnitude descending, identity ascending). Each listed feature shows \
         every one of the world's peoples' name for it, species-label ascending, \
         none elected as primary — a feature has no primary name (spec §3.4).\n\n"
    ));

    for (class, total, entries) in per_class {
        let (singular, plural) = class_words(*class);
        let class_label = if *total == 1 { singular } else { plural };
        doc.push_str(&format!("## {}\n\n", capitalize(plural)));
        doc.push_str(&format!(
            "showing the {} largest of {total} {class_label}\n\n",
            entries.len(),
        ));
        if entries.is_empty() {
            doc.push_str("(none)\n\n");
            continue;
        }
        // RENDERED PROSE, deliberately still "cell" (The Lexicon of Place).
        // The engine calls this a Vertex now; the almanac must not, because
        // "vertex" is engine vocabulary and this string is read by a person.
        // Changing it also moves the committed gallery almanacs.
        doc.push_str("| # | cell | magnitude | names |\n");
        doc.push_str("|---|---|---|---|\n");
        for (rank, entry) in entries.iter().enumerate() {
            doc.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                rank + 1,
                entry.anchor.0,
                entry.magnitude,
                render_names(&entry.names)
            ));
        }
        doc.push('\n');
    }

    doc
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(pairs: &[(&str, &str)]) -> BTreeMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    /// Spec §3.4: every name joined, species-label ascending (the `BTreeMap`
    /// key order), no lead — "aeldrin" sorts before "khorrun" regardless of
    /// insertion order.
    #[test]
    fn render_names_joins_species_ascending_with_no_lead() {
        let n = names(&[("khorrun", "Denali"), ("aeldrin", "Mount McKinley")]);
        assert_eq!(render_names(&n), "Mount McKinley, Denali");
    }

    #[test]
    fn render_names_of_one_people_is_just_that_name() {
        let n = names(&[("aeldrin", "Denali")]);
        assert_eq!(render_names(&n), "Denali");
    }

    /// The printed cap line reads `entries.len()`, not the nominal `cap` —
    /// a class with fewer features than the cap (this world's one sea) must
    /// never read as though more were truncated than actually exist.
    #[test]
    fn render_prints_shown_count_not_the_nominal_cap_when_fewer_exist() {
        let entry = Entry {
            anchor: Vertex(7),
            magnitude: 40962,
            names: names(&[("aeldrin", "Voa")]),
        };
        let out = render(42, 10, &[(FeatureClass::Sea, 1, vec![entry])]);
        assert!(
            out.contains("showing the 1 largest of 1 sea"),
            "cap line should read the true shown/total counts: {out}"
        );
    }

    /// A capped class states the real total even though only `cap` rows
    /// print — the whole point of printing the cap at all.
    #[test]
    fn render_states_the_true_total_when_capped() {
        let entries: Vec<Entry> = (0..3)
            .map(|i| Entry {
                anchor: Vertex(i),
                magnitude: 10 - i,
                names: names(&[("aeldrin", "Roa")]),
            })
            .collect();
        let out = render(42, 3, &[(FeatureClass::River, 106, entries)]);
        assert!(
            out.contains("showing the 3 largest of 106 rivers"),
            "should state the true class total even when capped: {out}"
        );
    }

    #[test]
    fn render_never_shows_a_bare_class_with_no_entries() {
        let out = render(42, 10, &[(FeatureClass::Sea, 0, vec![])]);
        assert!(out.contains("(none)"));
    }
}
