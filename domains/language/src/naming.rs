//! The naming grammars: build settlement, deity, and epithet names from a
//! drawn [`crate::phonology::Phonology`]. A **stem** is a run of syllables;
//! each syllable's onset/nucleus/coda manner-slots are filled by `pick`ing
//! matching segments FROM the phonology's inventory — this module never
//! constructs a [`crate::phoneme::Segment`] itself. That carry-forward from
//! Task 3/5 is load-bearing: `romanize`/`ipa`/`espeak_word` are exhaustive
//! only over [`crate::phoneme::canonical_segments`], and `draw_phonology`
//! only ever admits segments from that set, so a name built purely from
//! `pick`s over the inventory can never surface the `"?"` fallback glyph.
//!
//! Morphology is kind-keyed: a settlement name is a bare stem; a deity name
//! is a bare stem drawn with a bias toward closed ("weighty") syllables; an
//! epithet is a descriptive root that may be reduplicated and, when
//! `MorphOptions::honorifics` is set by the composition root (status basis
//! `Rank` → `true`), prefixed with a short bound honorific affix. Every
//! draw is rooted at
//! `seed.derive("language").derive(species).derive("name").derive(kind_label).derive(&salt.to_string()).stream()`
//! so a name is a pure, single deterministic function of `(seed, species,
//! kind, salt)` — no re-draw, no dependence on any other name. Uniqueness is
//! not guaranteed here: the phonology name space is vast enough that
//! collisions are rare in practice (measured as a calibration, spec §9), and
//! the composition root deliberately does NOT thread a shared "used" set
//! through naming. That purity is load-bearing: it makes settlement names
//! pin-isolated by construction (a name depends only on its own cell, never
//! on which other settlements — or species — a world happens to place).
//!
//! [`Namer::glossed_name`] (The Words, Task 9) is a second, later epoch of
//! the same `name` (retired but never deleted — old saves still read it):
//! rooted one leg deeper (`…derive(kind_label).derive("v2").derive(&salt)`),
//! it compounds 1-2 of a [`SiteConcepts`] site's actual lexicon words
//! instead of always drawing a bare stem, so a name becomes a small true
//! story about the entity it names, with a gloss to match. It stays exactly
//! as pure and pin-isolated as `name` — a function of `(seed, species, kind,
//! v2, salt, site, lexicon)`, still no re-draw, still no dependence on any
//! other *name* — though the `lexicon` and `site` a caller supplies may
//! themselves be composed upstream from a species' full settlement scatter
//! (Task 7/8's own per-species, not per-settlement, design); that composition
//! is entirely the composition root's business, never this module's.

use crate::lexicon::{Headedness, LexEntry, Lexicon};
use crate::phoneme::{Manner, Segment, espeak_word, ipa, romanize};
use crate::phonology::Phonology;
use hornvale_kernel::{Seed, Stream};

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

/// The concept ids an entity's own site facts resolve to, composed
/// upstream by the composition root — never by this crate, which is
/// kernel-only and never learns where a concept came from (a settlement:
/// its cell's biome concept plus its people's presiding-belief phenomenon
/// concept; a deity: its phenomenon concept plus its sentiment's quality
/// concept). [`Namer::glossed_name`] draws which 1-2 of these — of those
/// that actually hold a [`crate::lexicon::LexEntry::Root`] or
/// [`crate::lexicon::LexEntry::Compound`] word in the supplied lexicon — to
/// compound; a concept listed here with no word (a
/// [`crate::lexicon::LexEntry::Gap`], or simply absent) is never picked.
#[derive(Clone, Copy, Debug)]
pub struct SiteConcepts<'a> {
    /// The candidate concept ids, in no particular priority order.
    pub concepts: &'a [&'a str],
}

/// A generated name in its three views: `roman` is what commits as the
/// `name` fact (the almanac's ASCII-ish spelling); `ipa` is the book's
/// phonetic rendering; `espeak` is the espeak-ng formulation. None are stored
/// independently of the segments that produced it — all three are views built
/// in the same pass.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneratedName {
    /// The ASCII-ish romanization, capitalized on its first letter. This is
    /// the string that commits as the `name` fact.
    pub roman: String,
    /// The IPA rendering, uncapitalized (IPA has no case convention here).
    pub ipa: String,
    /// The espeak-ng formulation (`[[zv'etnot]]`), the input `hornvale
    /// voice` hands espeak-ng to author the book's audio clip for this name.
    pub espeak: String,
}

/// The chance (per attempt) that a drawn epithet root is reduplicated
/// (one of its syllables doubled) before any honorific prefix is applied.
const REDUPLICATION_CHANCE: f64 = 0.5;

/// One syllable: segments already picked from the phonology's inventory,
/// grouped by onset/nucleus/coda position so morphology (reduplication,
/// prefixing) can operate on whole syllables. `pub(crate)` so etymology's
/// `proto_root` can hold the value returned by [`Namer::draw_syllables`] and
/// pass it straight to [`views_of`] without this module exposing its fields.
#[derive(Clone, Debug)]
pub(crate) struct Syllable {
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
    /// index — e.g. the Nth settlement's cell id), applying `morph`'s
    /// morphology. A single deterministic draw: the name is a pure function
    /// of `(seed, species, kind, salt)` with no re-draw and no dependence on
    /// any other name (see the module docs — this is what makes settlement
    /// names pin-isolated by construction). Uniqueness across a world's
    /// names is de-facto, not guaranteed.
    pub fn name(&self, kind: NameKind, salt: u64, morph: &MorphOptions) -> GeneratedName {
        let mut stream = self
            .seed
            .derive("language")
            .derive(&self.species)
            .derive("name")
            .derive(kind.label())
            .derive(&salt.to_string())
            .stream();
        self.build_name(kind, morph, &mut stream)
    }

    /// Draw a *glossed* name of `kind` for `salt`, at the `/v2` epoch: the
    /// derive path gains a `"v2"` leg right after `kind`'s label, so this
    /// draws from a stream entirely distinct from [`Namer::name`]'s (v1
    /// stays retired, never reused). The stream picks 1-2 of `site`'s
    /// concepts that actually hold a word in `lexicon` (see
    /// [`SiteConcepts`]), compounds their modern-form segments in
    /// `lexicon`'s drawn [`crate::lexicon::Headedness`] order, and applies
    /// `kind`'s existing morphology on top — a bare stem for
    /// [`NameKind::Settlement`]/[`NameKind::Deity`] (compounding over
    /// already-evolved words has nothing left for the weighty-coda bias to
    /// act on), or an honorific prefix for [`NameKind::Epithet`] when
    /// `morph.honorifics` is set (status-basis keying intact; reduplication
    /// is a v1-only embellishment for freshly drawn stems and has no
    /// analog here). Returns the name's three views plus the gloss — the
    /// chosen concepts joined with `"-"` (e.g. `"ice-home"`) — so the gloss
    /// is always truthfully a subset of `site.concepts`.
    ///
    /// If *no* site concept holds a word, falls back to a bare stem drawn
    /// exactly as [`Namer::build_name`] draws v1 names, but still under
    /// this method's own `/v2` stream: the result stays a pure
    /// `(seed, species, kind, v2, salt)` function, distinct from `name`'s
    /// v1 output, with an empty gloss (no true story to tell — callers
    /// should skip the `name-gloss` fact when the gloss is empty).
    pub fn glossed_name(
        &self,
        kind: NameKind,
        salt: u64,
        morph: &MorphOptions,
        site: &SiteConcepts,
        lexicon: &Lexicon,
    ) -> (GeneratedName, String) {
        let mut stream = self
            .seed
            .derive("language")
            .derive(&self.species)
            .derive("name")
            .derive(kind.label())
            .derive("v2")
            .derive(&salt.to_string())
            .stream();

        let candidates: Vec<&str> = site
            .concepts
            .iter()
            .copied()
            .filter(|concept| holds_word(lexicon, concept))
            .collect();

        if candidates.is_empty() {
            let name = self.build_name(kind, morph, &mut stream);
            return (name, String::new());
        }

        let take = if candidates.len() == 1 {
            1
        } else {
            stream.range_u32(1, 2) as usize
        };
        let mut pool = candidates;
        let mut chosen: Vec<&str> = Vec::with_capacity(take);
        for _ in 0..take {
            let idx = stream.range_u32(0, (pool.len() - 1) as u32) as usize;
            chosen.push(pool.remove(idx));
        }

        let mut segments = compound_segments(lexicon, &chosen);
        if kind == NameKind::Epithet && morph.honorifics {
            let affix = self.draw_syllable(&mut stream, false);
            let mut prefixed: Vec<Segment> = affix.segments().copied().collect();
            prefixed.append(&mut segments);
            segments = prefixed;
        }

        let gloss = chosen.join("-");
        (render_views(&segments), gloss)
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
        views_of(&syllables).1
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
    /// `pub(crate)` so etymology's `proto_root` draws proto-roots from the
    /// same stem machinery names use, over its own seed-derivation path,
    /// rather than duplicating the phonotactic-filling logic.
    pub(crate) fn draw_syllables(
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
}

/// Render a bare segment sequence's three surface views in one pass — the
/// segment-level half of the reduction [`views_of`] performs over
/// [`Syllable`]s, factored out so a caller that already holds a flat
/// `Vec<Segment>` (lexicon's roots and recipe compounds, over `evolve`'s
/// modern forms) reuses the same romanization/IPA/espeak logic instead of
/// re-deriving it. `pub(crate)` for that cross-module reuse.
pub(crate) fn render_views(segments: &[Segment]) -> GeneratedName {
    let mut roman = String::new();
    let mut ipa_str = String::new();
    for seg in segments {
        roman.push_str(romanize(seg));
        ipa_str.push_str(ipa(seg));
    }
    GeneratedName {
        roman: capitalize_first(&roman),
        ipa: ipa_str,
        espeak: espeak_word(segments),
    }
}

/// Flatten `syllables` (onset → nucleus → coda, in sequence) into their
/// ordered segments and render all three surface views via
/// [`render_views`]. `Namer::build_name` uses the `GeneratedName` half;
/// etymology's `proto_root` (over syllables drawn from
/// [`Namer::draw_syllables`]) uses the flat `Vec<Segment>` half as its
/// proto-root, reusing this module's stem machinery without duplicating the
/// onset/nucleus/coda flattening or the romanization match arms.
/// `pub(crate)` for that cross-module reuse.
pub(crate) fn views_of(syllables: &[Syllable]) -> (Vec<Segment>, GeneratedName) {
    let segments: Vec<Segment> = syllables
        .iter()
        .flat_map(|syllable| syllable.segments().copied())
        .collect();
    let name = render_views(&segments);
    (segments, name)
}

/// Whether `lexicon` holds an actual word — a [`LexEntry::Root`] or
/// [`LexEntry::Compound`], never a [`LexEntry::Gap`] or an absent entry —
/// for `concept`. The filter [`Namer::glossed_name`] applies to a site's
/// candidate concepts before picking which 1-2 to compound.
fn holds_word(lexicon: &Lexicon, concept: &str) -> bool {
    matches!(
        lexicon.entry(concept),
        Some(LexEntry::Root { .. }) | Some(LexEntry::Compound { .. })
    )
}

/// Join `modifier`'s and `head`'s segments in `headedness` order — the same
/// order [`crate::lexicon::build_lexicon`]'s own compound assembly uses.
fn join_by_headedness(
    headedness: Headedness,
    modifier: Vec<Segment>,
    head: Vec<Segment>,
) -> Vec<Segment> {
    match headedness {
        Headedness::HeadFirst => head.into_iter().chain(modifier).collect(),
        Headedness::HeadLast => modifier.into_iter().chain(head).collect(),
    }
}

/// The full modern-form segments `concept` resolves to in `lexicon`: a
/// root's own evolved segments, or a compound's two component roots
/// rejoined in `lexicon`'s headedness order. [`LexEntry::Compound`] exposes
/// only its already-rendered [`crate::lexicon::WordViews`], never raw
/// segments, so a picked compound's segments are reconstructed here from
/// its `modifier`/`head` roots rather than re-derived from strings.
/// [`Namer::glossed_name`] only ever calls this on a concept [`holds_word`]
/// has already accepted, so the empty fallback below is unreachable in
/// practice — it exists only so this stays total rather than panicking on
/// a lexicon invariant this module doesn't itself enforce.
fn concept_segments(lexicon: &Lexicon, concept: &str) -> Vec<Segment> {
    match lexicon.entry(concept) {
        Some(LexEntry::Root { derivation, .. }) => derivation.modern.clone(),
        Some(LexEntry::Compound { modifier, head, .. }) => {
            let modifier_segs = concept_segments(lexicon, modifier);
            let head_segs = concept_segments(lexicon, head);
            join_by_headedness(lexicon.headedness, modifier_segs, head_segs)
        }
        _ => Vec::new(),
    }
}

/// The segments [`Namer::glossed_name`] compounds for its 1-2 `chosen` site
/// concepts: a single concept's own segments unchanged, or two concepts'
/// segments joined as a fresh modifier/head pair (the first-drawn concept
/// is the modifier, the second-drawn is the head — an arbitrary but
/// deterministic convention, distinct from any recipe compounding already
/// inside `lexicon`) in `lexicon`'s drawn headedness order.
fn compound_segments(lexicon: &Lexicon, chosen: &[&str]) -> Vec<Segment> {
    if chosen.len() == 1 {
        return concept_segments(lexicon, chosen[0]);
    }
    let modifier_segs = concept_segments(lexicon, chosen[0]);
    let head_segs = concept_segments(lexicon, chosen[1]);
    join_by_headedness(lexicon.headedness, modifier_segs, head_segs)
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
    use crate::lexicon::{ExposureClass, build_lexicon};
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use std::collections::BTreeMap;

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

    /// A permissive phonology with plenty of segments to draw proto-roots
    /// and cascades from, matching etymology's/lexicon's own test fixture.
    fn wordy_ph() -> crate::phonology::Phonology {
        draw_phonology(
            &Seed(37),
            "test",
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                exotic: ExoticSeg::None,
            },
        )
    }

    /// A lexicon over `wordy_ph()` where "water" and "fire" are both
    /// Steeped roots and every other concept is left absent (so
    /// `lexicon.entry` returns `None` for it) — enough for `glossed_name`'s
    /// site-concept picking to have real words to compound.
    fn two_word_lexicon(seed: u64) -> Lexicon {
        let ph = wordy_ph();
        let mut exposures = BTreeMap::new();
        exposures.insert("water".to_string(), ExposureClass::Steeped);
        exposures.insert("fire".to_string(), ExposureClass::Steeped);
        build_lexicon(&Seed(seed), "test", &ph, &exposures)
    }

    /// An empty lexicon (no concepts at all) — every site concept
    /// `glossed_name` is offered here is unheld, forcing its fallback
    /// branch.
    fn empty_lexicon(seed: u64) -> Lexicon {
        let ph = wordy_ph();
        build_lexicon(&Seed(seed), "test", &ph, &BTreeMap::new())
    }

    #[test]
    fn glossed_name_is_pure_and_pin_isolated() {
        // Same (seed, species, kind, salt, site, lexicon) must yield the
        // identical name and gloss, independent of any other entity — two
        // freshly built Namers/lexicons over the same inputs, no shared
        // state between them.
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = MorphOptions { honorifics: false };
        let n1 = Namer::new(&Seed(9), "test", &ph);
        let n2 = Namer::new(&Seed(9), "test", &ph);
        let a = n1.glossed_name(NameKind::Settlement, 3, &morph, &site, &lex);
        let b = n2.glossed_name(NameKind::Settlement, 3, &morph, &site, &lex);
        assert_eq!(a, b);
    }

    #[test]
    fn glossed_name_gloss_concepts_are_a_subset_of_site_concepts() {
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = MorphOptions { honorifics: false };
        let namer = Namer::new(&Seed(9), "test", &ph);
        for salt in 0..40u64 {
            let (_, gloss) = namer.glossed_name(NameKind::Settlement, salt, &morph, &site, &lex);
            assert!(
                gloss.is_empty()
                    || gloss == "water"
                    || gloss == "fire"
                    || gloss == "water-fire"
                    || gloss == "fire-water",
                "gloss {gloss:?} is not built purely from site concepts {:?}",
                site.concepts
            );
        }
    }

    #[test]
    fn glossed_name_falls_back_to_a_bare_stem_when_no_site_concept_holds_a_word() {
        let ph = wordy_ph();
        let lex = empty_lexicon(9);
        let site = SiteConcepts {
            concepts: &["nonexistent"],
        };
        let morph = MorphOptions { honorifics: false };
        let namer = Namer::new(&Seed(9), "test", &ph);
        let (name, gloss) = namer.glossed_name(NameKind::Settlement, 3, &morph, &site, &lex);
        assert!(
            gloss.is_empty(),
            "no true story to tell: gloss must be empty"
        );
        assert!(!name.roman.is_empty());
    }

    #[test]
    fn glossed_name_moved_the_epoch_off_v1() {
        // The fallback path draws a bare stem too, but under the /v2 leg —
        // a different stream from v1's `name()`, so the two must diverge
        // even for the same seed/species/kind/salt.
        let ph = wordy_ph();
        let lex = empty_lexicon(9);
        let site = SiteConcepts {
            concepts: &["nonexistent"],
        };
        let morph = MorphOptions { honorifics: false };
        let namer = Namer::new(&Seed(9), "test", &ph);
        let v1 = namer.name(NameKind::Settlement, 3, &morph);
        let (v2, _) = namer.glossed_name(NameKind::Settlement, 3, &morph, &site, &lex);
        assert_ne!(
            v1.roman, v2.roman,
            "v2 must draw from a distinct stream than v1, even on the fallback path"
        );
    }

    #[test]
    fn glossed_epithet_honorific_still_keys_to_status_basis() {
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water"],
        };
        let namer = Namer::new(&Seed(9), "test", &ph);
        let with = namer.glossed_name(
            NameKind::Epithet,
            5,
            &MorphOptions { honorifics: true },
            &site,
            &lex,
        );
        let without = namer.glossed_name(
            NameKind::Epithet,
            5,
            &MorphOptions { honorifics: false },
            &site,
            &lex,
        );
        assert_ne!(
            with.0.roman, without.0.roman,
            "status-basis keying must still change a glossed epithet's shape"
        );
        // The honorific prefix must not change WHAT the name glosses to.
        assert_eq!(with.1, without.1);
    }

    #[test]
    fn names_are_deterministic_and_carry_all_three_views() {
        let ph = kobold_ph();
        let n1 = Namer::new(&Seed(1), "kobold", &ph);
        let n2 = Namer::new(&Seed(1), "kobold", &ph);
        let a = n1.name(
            NameKind::Settlement,
            10,
            &MorphOptions { honorifics: false },
        );
        let b = n2.name(
            NameKind::Settlement,
            10,
            &MorphOptions { honorifics: false },
        );
        assert_eq!(a, b);
        assert!(!a.roman.is_empty() && !a.ipa.is_empty() && !a.espeak.is_empty());
    }

    #[test]
    fn a_name_is_a_pure_function_of_seed_species_kind_and_salt() {
        // No re-draw, no shared "used" set: the same (seed, species, kind,
        // salt) always yields the same name, and distinct salts draw
        // independently. This purity is what makes settlement names
        // pin-isolated by construction (spec §8) — a name never depends on
        // which other settlements a world places.
        let ph = kobold_ph();
        let namer = Namer::new(&Seed(2), "kobold", &ph);
        let mut first: Vec<String> = Vec::new();
        for salt in 0..50u64 {
            let g = namer.name(
                NameKind::Settlement,
                salt,
                &MorphOptions { honorifics: false },
            );
            assert!(!g.roman.is_empty());
            first.push(g.roman);
        }
        // A second pass over the same salts reproduces every name exactly.
        let namer2 = Namer::new(&Seed(2), "kobold", &ph);
        for (salt, expected) in first.iter().enumerate() {
            let g = namer2.name(
                NameKind::Settlement,
                salt as u64,
                &MorphOptions { honorifics: false },
            );
            assert_eq!(&g.roman, expected, "salt {salt} must redraw identically");
        }
    }

    #[test]
    fn honorific_morphology_appears_only_when_requested() {
        let ph = kobold_ph();
        let namer = Namer::new(&Seed(3), "kobold", &ph);
        // Epithets with honorifics enabled must be able to differ from those without.
        let with = namer.name(NameKind::Epithet, 5, &MorphOptions { honorifics: true });
        let namer2 = Namer::new(&Seed(3), "kobold", &ph);
        let without = namer2.name(NameKind::Epithet, 5, &MorphOptions { honorifics: false });
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
        let namer = Namer::new(&Seed(11), "kobold", &ph);
        for (salt, kind, honorifics) in [
            (0u64, NameKind::Settlement, false),
            (1, NameKind::Deity, false),
            (2, NameKind::Epithet, false),
            (3, NameKind::Epithet, true),
        ] {
            let g = namer.name(kind, salt, &MorphOptions { honorifics });
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
            assert!(
                !g.espeak.contains('?'),
                "espeak {:?} contains the unrepresentable-segment glyph",
                g.espeak
            );
        }
    }

    #[test]
    fn a_generated_name_carries_a_wrapped_stressed_espeak_formulation() {
        let ph = kobold_ph();
        let namer = Namer::new(&Seed(1), "kobold", &ph);
        let name = namer.name(NameKind::Settlement, 0, &MorphOptions { honorifics: false });
        assert!(
            name.espeak.starts_with("[[") && name.espeak.ends_with("]]"),
            "formulation {:?} must be wrapped for espeak direct phoneme input",
            name.espeak
        );
        assert!(
            name.espeak.contains('\''),
            "formulation {:?} must carry an explicit stress marker (every name has a vowel)",
            name.espeak
        );
    }
}
