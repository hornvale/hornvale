//! The naming grammars: build settlement, deity, and epithet names from a
//! drawn [`crate::phonology::Phonology`]. A **stem** is a run of syllables;
//! each syllable's onset/nucleus/coda manner-slots are filled by `pick`ing
//! matching segments FROM the phonology's inventory — this module never
//! constructs a [`crate::phoneme::Segment`] itself. That carry-forward from
//! Task 3/5 is load-bearing: `romanize`/`ipa` are exhaustive only over
//! [`crate::phoneme::canonical_segments`], and `draw_phonology` only ever
//! admits segments from that set, so a name built purely from `pick`s over
//! the inventory can never surface the `"?"` fallback glyph.
//!
//! Morphology is kind-keyed: a settlement name is a bare stem; a deity name
//! is a bare stem drawn with a bias toward closed ("weighty") syllables; an
//! epithet is a descriptive root that may be reduplicated and, when
//! `MorphOptions::honorifics` is set by the composition root (status basis
//! `Rank` → `true`), prefixed with a short bound honorific affix. Every
//! draw is rooted at
//! `seed.derive("language").derive(species).derive("name").derive(kind_label).derive(&salt.to_string()).derive(&redraw.to_string()).stream()`
//! so a name is reproducible from `(seed, species, kind, salt)` alone, and a
//! uniqueness collision advances only the `redraw` leg of the path.

use crate::phoneme::{Manner, Segment, ipa, romanize};
use crate::phonology::Phonology;
use hornvale_kernel::{Seed, Stream};
use std::collections::BTreeSet;

/// What kind of name is being drawn; selects the morphology rules and the
/// `derive` label for the name's seed path.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NameKind {
    /// A settlement name: a bare stem.
    Settlement,
    /// A deity name: a bare stem drawn with a bias toward closed, "weighty"
    /// syllables.
    Deity,
    /// An epithet: a descriptive root, optionally reduplicated and
    /// optionally honorific-prefixed (see [`MorphOptions`]).
    Epithet,
}

impl NameKind {
    /// The stable label folded into the seed's `derive` path for this kind.
    /// Save-format-contract stable: changing it silently reseeds every name
    /// of that kind in every saved world.
    fn label(self) -> &'static str {
        match self {
            NameKind::Settlement => "settlement",
            NameKind::Deity => "deity",
            NameKind::Epithet => "epithet",
        }
    }
}

/// Morphology options the composition root keys from a species' status
/// basis. Plain data — this crate is kernel-only and never imports
/// `hornvale-species`; the mapping (e.g. `Rank` → `honorifics: true`) lives
/// upstream.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MorphOptions {
    /// Whether epithets are prefixed with a drawn honorific affix. Bare
    /// stems (settlement, deity) never consult this field.
    pub honorifics: bool,
}

/// A generated name in its two views: `roman` is what commits as the `name`
/// fact (the almanac's ASCII-ish spelling); `ipa` is the book's phonetic
/// rendering. Neither is stored independently of the segments that produced
/// it — both are views built in the same pass.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneratedName {
    /// The ASCII-ish romanization, capitalized on its first letter. This is
    /// the string that commits as the `name` fact.
    pub roman: String,
    /// The IPA rendering, uncapitalized (IPA has no case convention here).
    pub ipa: String,
}

/// The cap on uniqueness re-draws before `name` gives up and returns the
/// last-drawn candidate even though it collides with `used`. 10,000 is
/// astronomically larger than any plausible in-world namespace for a single
/// `(kind, salt)` pair — the early return inside the loop is expected to
/// fire on the first or second attempt in practice. The cap exists only so
/// `name` is total (never loops forever, never panics).
const MAX_REDRAWS: u64 = 10_000;

/// The chance (per attempt) that a drawn epithet root is reduplicated
/// (one of its syllables doubled) before any honorific prefix is applied.
const REDUPLICATION_CHANCE: f64 = 0.5;

/// One syllable: segments already picked from the phonology's inventory,
/// grouped by onset/nucleus/coda position so morphology (reduplication,
/// prefixing) can operate on whole syllables.
#[derive(Clone, Debug)]
struct Syllable {
    onset: Vec<Segment>,
    nucleus: Vec<Segment>,
    coda: Vec<Segment>,
}

impl Syllable {
    /// The syllable's segments in onset → nucleus → coda order, the order
    /// they render in.
    fn segments(&self) -> impl Iterator<Item = &Segment> {
        self.onset.iter().chain(&self.nucleus).chain(&self.coda)
    }
}

/// Builds names for one species from its drawn [`Phonology`]. Holds a seed
/// handle (copied — `Seed` is `Copy`) and a borrow of the phonology so
/// repeated `name` calls never re-derive or re-draw the phonology itself.
pub struct Namer<'a> {
    seed: Seed,
    species: String,
    ph: &'a Phonology,
}

impl<'a> Namer<'a> {
    /// Start a namer for `species` under `ph`, rooted at `seed`.
    pub fn new(seed: &Seed, species: &str, ph: &'a Phonology) -> Namer<'a> {
        Namer {
            seed: *seed,
            species: species.to_string(),
            ph,
        }
    }

    /// Draw a name of `kind` for `salt` (the caller's per-entity draw
    /// index — e.g. the Nth settlement), applying `morph`'s morphology, and
    /// re-drawing deterministically until the romanized form is not in
    /// `used`. See [`MAX_REDRAWS`] for the (practically unreachable) cap.
    pub fn name(
        &mut self,
        kind: NameKind,
        salt: u64,
        morph: &MorphOptions,
        used: &BTreeSet<String>,
    ) -> GeneratedName {
        let kind_label = kind.label();
        for redraw in 0..MAX_REDRAWS {
            let mut stream = self
                .seed
                .derive("language")
                .derive(&self.species)
                .derive("name")
                .derive(kind_label)
                .derive(&salt.to_string())
                .derive(&redraw.to_string())
                .stream();
            let candidate = self.build_name(kind, morph, &mut stream);
            if redraw == MAX_REDRAWS - 1 || !used.contains(&candidate.roman) {
                return candidate;
            }
        }
        unreachable!("MAX_REDRAWS is nonzero, so the loop always returns")
    }

    /// Build one candidate name from a single stream draw, applying the
    /// kind's morphology.
    fn build_name(
        &self,
        kind: NameKind,
        morph: &MorphOptions,
        stream: &mut Stream,
    ) -> GeneratedName {
        let syllables = match kind {
            NameKind::Settlement => self.draw_syllables(stream, 2, 3, false),
            NameKind::Deity => self.draw_syllables(stream, 2, 3, true),
            NameKind::Epithet => {
                let mut syllables = self.draw_syllables(stream, 1, 2, false);
                Self::maybe_reduplicate(stream, &mut syllables);
                if morph.honorifics {
                    // The honorific affix: a short bound stem, one syllable,
                    // drawn from a dedicated step in this same stream and
                    // prefixed onto the descriptive root.
                    let affix = self.draw_syllable(stream, false);
                    syllables.insert(0, affix);
                }
                syllables
            }
        };
        Self::render(&syllables)
    }

    /// Double a randomly chosen syllable of `syllables` in place, with
    /// probability [`REDUPLICATION_CHANCE`]. A no-op on an empty stem.
    fn maybe_reduplicate(stream: &mut Stream, syllables: &mut Vec<Syllable>) {
        if syllables.is_empty() {
            return;
        }
        if stream.next_f64() < REDUPLICATION_CHANCE {
            let idx = stream.range_u32(0, (syllables.len() - 1) as u32) as usize;
            let doubled = syllables[idx].clone();
            syllables.insert(idx, doubled);
        }
    }

    /// Draw `range_u32(min, max)` syllables in sequence from `stream`.
    /// `weighty` biases each syllable's coda toward a closed (non-empty)
    /// template when one is available (see [`Namer::choose_coda_template`]).
    fn draw_syllables(
        &self,
        stream: &mut Stream,
        min: u32,
        max: u32,
        weighty: bool,
    ) -> Vec<Syllable> {
        let count = stream.range_u32(min, max);
        (0..count)
            .map(|_| self.draw_syllable(stream, weighty))
            .collect()
    }

    /// Draw one syllable: an onset template, `ph.nuclei` vowels, and a coda
    /// template, each filled by `pick`ing matching segments from the
    /// inventory. Draw order (onset, then nucleus, then coda) is part of
    /// the stream-consumption contract.
    fn draw_syllable(&self, stream: &mut Stream, weighty: bool) -> Syllable {
        let onset_template = stream.pick(&self.ph.onsets).cloned().unwrap_or_default();
        let onset = self.fill_manners(stream, &onset_template);

        let nucleus = (0..self.ph.nuclei)
            .filter_map(|_| self.pick_vowel(stream))
            .collect();

        let coda_template = self.choose_coda_template(stream, weighty);
        let coda = self.fill_manners(stream, &coda_template);

        Syllable {
            onset,
            nucleus,
            coda,
        }
    }

    /// Pick a coda template. When `weighty` and at least one of the
    /// phonology's coda templates is non-empty (closed), restrict the pick
    /// to those — "weighty" means deity stems favor closed syllables. Falls
    /// back to picking from every coda template (open or closed) otherwise,
    /// or to the empty (open) template if `ph.codas` is itself empty.
    fn choose_coda_template(&self, stream: &mut Stream, weighty: bool) -> Vec<Manner> {
        if weighty {
            let closed: Vec<Vec<Manner>> = self
                .ph
                .codas
                .iter()
                .filter(|template| !template.is_empty())
                .cloned()
                .collect();
            if let Some(template) = stream.pick(&closed) {
                return template.clone();
            }
        }
        stream.pick(&self.ph.codas).cloned().unwrap_or_default()
    }

    /// Fill each manner slot in `template` by picking a matching consonant
    /// from the inventory. `draw_phonology` guarantees every manner that
    /// appears in a template is present in the inventory, so this only
    /// yields fewer segments than `template.len()` if the inventory is
    /// otherwise degenerate.
    fn fill_manners(&self, stream: &mut Stream, template: &[Manner]) -> Vec<Segment> {
        template
            .iter()
            .filter_map(|manner| self.pick_consonant(stream, *manner))
            .collect()
    }

    /// Pick one consonant of `manner` from the inventory.
    fn pick_consonant(&self, stream: &mut Stream, manner: Manner) -> Option<Segment> {
        let candidates: Vec<Segment> = self
            .ph
            .inventory
            .iter()
            .filter(|seg| matches!(seg, Segment::Consonant { manner: m, .. } if *m == manner))
            .copied()
            .collect();
        stream.pick(&candidates).copied()
    }

    /// Pick one vowel from the inventory.
    fn pick_vowel(&self, stream: &mut Stream) -> Option<Segment> {
        let candidates: Vec<Segment> = self
            .ph
            .inventory
            .iter()
            .filter(|seg| matches!(seg, Segment::Vowel { .. }))
            .copied()
            .collect();
        stream.pick(&candidates).copied()
    }

    /// Render a sequence of syllables to both surface views, capitalizing
    /// the romanization's first letter (the IPA view keeps no case
    /// convention).
    fn render(syllables: &[Syllable]) -> GeneratedName {
        let mut roman = String::new();
        let mut ipa_str = String::new();
        for syllable in syllables {
            for seg in syllable.segments() {
                roman.push_str(romanize(seg));
                ipa_str.push_str(ipa(seg));
            }
        }
        GeneratedName {
            roman: capitalize_first(&roman),
            ipa: ipa_str,
        }
    }
}

/// Capitalize the first character of `s`, leaving the rest untouched.
/// Empty input yields empty output.
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use std::collections::BTreeSet;

    fn kobold_ph() -> crate::phonology::Phonology {
        draw_phonology(
            &Seed(42),
            "kobold",
            &Envelope {
                labiality: 0.1,
                vowel_space: 0.3,
                voicing: 0.6,
                sibilance: 0.9,
                voice_loudness: 0.2,
                exotic: ExoticSeg::Trill,
            },
        )
    }

    #[test]
    fn names_are_deterministic_and_carry_both_views() {
        let ph = kobold_ph();
        let mut n1 = Namer::new(&Seed(1), "kobold", &ph);
        let mut n2 = Namer::new(&Seed(1), "kobold", &ph);
        let a = n1.name(
            NameKind::Settlement,
            10,
            &MorphOptions { honorifics: false },
            &BTreeSet::new(),
        );
        let b = n2.name(
            NameKind::Settlement,
            10,
            &MorphOptions { honorifics: false },
            &BTreeSet::new(),
        );
        assert_eq!(a.roman, b.roman);
        assert!(!a.roman.is_empty() && !a.ipa.is_empty());
    }

    #[test]
    fn uniqueness_redraw_avoids_collision() {
        let ph = kobold_ph();
        let mut namer = Namer::new(&Seed(2), "kobold", &ph);
        let mut used = BTreeSet::new();
        for salt in 0..50u64 {
            let g = namer.name(
                NameKind::Settlement,
                salt,
                &MorphOptions { honorifics: false },
                &used,
            );
            assert!(
                !used.contains(&g.roman),
                "re-draw must avoid an in-world collision"
            );
            used.insert(g.roman);
        }
    }

    #[test]
    fn honorific_morphology_appears_only_when_requested() {
        let ph = kobold_ph();
        let mut namer = Namer::new(&Seed(3), "kobold", &ph);
        // Epithets with honorifics enabled must be able to differ from those without.
        let with = namer.name(
            NameKind::Epithet,
            5,
            &MorphOptions { honorifics: true },
            &BTreeSet::new(),
        );
        let mut namer2 = Namer::new(&Seed(3), "kobold", &ph);
        let without = namer2.name(
            NameKind::Epithet,
            5,
            &MorphOptions { honorifics: false },
            &BTreeSet::new(),
        );
        assert_ne!(
            with.roman, without.roman,
            "status-basis keying must change epithet shape"
        );
    }

    /// End-to-end carry-forward from Tasks 3/5: a name built purely by
    /// `pick`ing segments FROM the phonology's inventory (never synthesizing
    /// a `Segment`) can never surface the `"?"` fallback glyph that
    /// `romanize`/`ipa` return for anything outside `canonical_segments()`.
    #[test]
    fn generated_names_never_contain_the_unrepresentable_glyph() {
        let ph = kobold_ph();
        let mut namer = Namer::new(&Seed(11), "kobold", &ph);
        let mut used = BTreeSet::new();
        for (salt, kind, honorifics) in [
            (0u64, NameKind::Settlement, false),
            (1, NameKind::Deity, false),
            (2, NameKind::Epithet, false),
            (3, NameKind::Epithet, true),
        ] {
            let g = namer.name(kind, salt, &MorphOptions { honorifics }, &used);
            assert!(
                !g.roman.contains('?'),
                "roman {:?} contains the unrepresentable-segment glyph",
                g.roman
            );
            assert!(
                !g.ipa.contains('?'),
                "ipa {:?} contains the unrepresentable-segment glyph",
                g.ipa
            );
            used.insert(g.roman);
        }
    }
}
