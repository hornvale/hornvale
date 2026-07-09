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
    /// `kind`'s existing morphology on top — an honorific prefix for
    /// [`NameKind::Epithet`] when `morph.honorifics` is set (status-basis
    /// keying intact; reduplication is a v1-only embellishment for freshly
    /// drawn stems and has no analog here). For [`NameKind::Settlement`]
    /// ONLY, the compound also gains a per-salt drawn **stem** — the
    /// toponymic unique element (descriptor + unique element, the
    /// collision fix; see the inline comment) — filling the modifier slot
    /// opposite the site compound's head slot. Returns the name's three
    /// views plus the gloss — the chosen concepts joined with `"-"` (e.g.
    /// `"ice-home"`; the settlement stem names no concept and never enters
    /// it) — so the gloss is always truthfully a subset of
    /// `site.concepts`.
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
        if kind == NameKind::Settlement {
            // The toponymic unique element (the collision fix — Task 12's
            // census measured an ~86% in-world collision rate for pure
            // site-concept compounds, spec §9's low-collision criterion):
            // settlement names compound the site-concept word(s) with a
            // per-salt DRAWN stem (1-2 template syllables from this same
            // v2 stream, after the site-concept picks — real-world
            // toponymy's descriptor + unique element, "Ice-home-by-the-
            // ford"). The stem fills the slot OPPOSITE the head: the site
            // compound plays the head, the stem the modifier, joined in
            // the lexicon's drawn headedness order. It is a proper-name
            // element naming no concept, so it never enters the gloss —
            // glosses stay truthful compositions of site concepts alone.
            // Deity and Epithet names carry no stem (their name spaces
            // are one-per-belief, not pigeonholed by settlement counts).
            let stem_syllables = self.draw_syllables(&mut stream, 1, 2, false);
            let (stem, _) = views_of(&stem_syllables);
            segments = join_by_headedness(lexicon.headedness, stem, segments);
        }
        // Repair AFTER compounding, BEFORE morphology (the permanent order):
        // evolved roots only guarantee inventory membership, not template
        // conformance, so the compound is adapted to the synchronic
        // phonotactics first (see [`repair_phonotactics`] — the spec §8
        // structural invariant "every name well-formed for its language").
        // The honorific prefix below is a freshly drawn template syllable —
        // conformant by construction — so prefixing it onto a repaired word
        // keeps the whole name conformant. Repair changes sound, never
        // meaning: the gloss is computed from `chosen` alone.
        let mut segments = repair_phonotactics(segments, self.ph);
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
/// re-deriving it. `pub`, not `pub(crate)`: a [`crate::etymology::Derivation`]'s
/// `proto` field is a bare `Vec<Segment>` too, and the dictionary surface
/// (Task 11) needs this exact reduction to render a proto-form's roman
/// spelling — the same view [`crate::lexicon::WordViews`] already gets for
/// modern forms, so a proto-form's rendering can never drift from it.
pub fn render_views(segments: &[Segment]) -> GeneratedName {
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

/// Consume one exact phonotactic template from `segments` at `pos`: each
/// slot must be filled by a consonant of exactly that manner, in order.
/// `Some(position after the template)` on a full match, `None` otherwise.
/// The segment-level twin of the almanac-side romanization validator's
/// template matching, shared by [`conforms`] and [`repair_phonotactics`].
fn match_manner_seq(segments: &[Segment], pos: usize, template: &[Manner]) -> Option<usize> {
    let mut p = pos;
    for &required in template {
        match segments.get(p) {
            Some(Segment::Consonant { manner, .. }) if *manner == required => p += 1,
            _ => return None,
        }
    }
    Some(p)
}

/// Whether `segments` parses as a sequence of syllables under `ph`'s drawn
/// phonotactic templates: each syllable an onset matching one of
/// `ph.onsets` exactly (by manner sequence), then exactly `ph.nuclei`
/// vowels, then a coda matching one of `ph.codas`. A backtracking parse
/// (every onset/coda split is explored), so any parseable sequence is
/// accepted; the empty sequence is not a word. This is the invariant
/// [`repair_phonotactics`] restores and the property the Lab's
/// romanization-level validator re-checks from the committed string.
fn conforms(segments: &[Segment], ph: &Phonology) -> bool {
    fn from(segments: &[Segment], pos: usize, ph: &Phonology) -> bool {
        if pos == segments.len() {
            return true;
        }
        for onset in &ph.onsets {
            let Some(after_onset) = match_manner_seq(segments, pos, onset) else {
                continue;
            };
            let after_nucleus = after_onset + ph.nuclei;
            if after_nucleus > segments.len()
                || !segments[after_onset..after_nucleus]
                    .iter()
                    .all(|s| matches!(s, Segment::Vowel { .. }))
            {
                continue;
            }
            for coda in &ph.codas {
                if let Some(after_coda) = match_manner_seq(segments, after_nucleus, coda)
                    && from(segments, after_coda, ph)
                {
                    return true;
                }
            }
        }
        false
    }
    !segments.is_empty() && from(segments, 0, ph)
}

/// Epenthesis cost per inserted vowel in [`repair_phonotactics`]'s
/// minimal-edit resyllabification. Strictly cheaper than
/// [`DELETION_COST`]: insertion is the primary repair, deletion the
/// second resort. Both constants are part of the permanent repair formula
/// (changing either reseeds every repaired name in every saved world).
const EPENTHESIS_COST: u32 = 1;

/// Deletion cost per dropped segment in [`repair_phonotactics`] — the
/// second resort, for a segment no template can host in its position.
const DELETION_COST: u32 = 2;

/// One step of a reconstructed repair plan: either this input segment is
/// dropped, or a syllable starts here using the indexed onset/coda
/// templates, consuming `vowels` input vowels and inserting `pads`
/// epenthetic ones.
enum RepairStep {
    /// Drop the segment at this position (no template can host it).
    Delete,
    /// Emit one syllable starting at this position.
    Syllable {
        /// Index into `ph.onsets` of the matched onset template.
        onset: usize,
        /// Index into `ph.codas` of the matched coda template.
        coda: usize,
        /// How many input vowels the nucleus consumed (≤ `ph.nuclei`).
        vowels: usize,
        /// How many epenthetic vowels complete the nucleus
        /// (`ph.nuclei - vowels`).
        pads: usize,
    },
}

/// **The phonotactic repair rule** (permanent formula, The Words Task 9):
/// make an arbitrary segment sequence — a compound of evolved lexicon
/// roots, whose sound changes are only guaranteed to land in the phoneme
/// *inventory*, never inside the synchronic syllable *templates* — parse
/// under `ph`'s drawn phonotactics, the way real languages adapt loanwords
/// and compounds. Pure and deterministic: a function of the segments and
/// the phonology alone, no stream draws.
///
/// 1. **Already-legal input is untouched** ([`conforms`] short-circuit) —
///    repair of a valid sequence is the identity, which also makes repair
///    idempotent (its output always conforms).
/// 2. **Resyllabify with minimal edits.** The sequence is re-parsed into
///    syllables against the drawn onset/nucleus/coda templates; where a
///    stretch cannot parse, two edits are available: **epenthesis** —
///    insert the language's canonical epenthetic vowel (the first vowel in
///    the drawn inventory's order — deterministic, not drawn) to complete
///    a nucleus and thereby break an illegal cluster — and **deletion** of
///    a segment no template can host in its position. Epenthesis costs
///    [`EPENTHESIS_COST`] per inserted vowel, deletion [`DELETION_COST`]
///    per dropped segment, so insertion is always preferred where either
///    would do (deletion is the second resort). The minimal-cost plan is
///    found by dynamic programming over input positions; ties break
///    deterministically toward the earlier-listed template pair (onsets
///    then codas, in their drawn order), with deletion considered last.
/// 3. **Degenerate-input fallback**: if the minimal plan deletes
///    everything (an input with no vowel and no template-hostable
///    consonant — unreachable from real lexicon roots, which always carry
///    nuclei), emit one minimal legal syllable instead: the first onset
///    template filled with the first inventory consonant of each required
///    manner, an all-epenthetic nucleus, and the first coda template
///    filled the same way. A name is never empty.
fn repair_phonotactics(segments: Vec<Segment>, ph: &Phonology) -> Vec<Segment> {
    if conforms(&segments, ph) {
        return segments;
    }
    let Some(epenthetic) = ph
        .inventory
        .iter()
        .find(|s| matches!(s, Segment::Vowel { .. }))
        .copied()
    else {
        // A vowelless inventory cannot host any nucleus; nothing can be
        // repaired against it. Unreachable for drawn phonologies (the
        // vowel band always admits at least one vowel) — refuse to edit
        // rather than destroy the name.
        return segments;
    };

    // best[i]: the cheapest way to legalize segments[i..], filled back to
    // front. best[n] is the implicit empty suffix at cost 0.
    let n = segments.len();
    let mut best: Vec<Option<(u32, RepairStep)>> = Vec::with_capacity(n);
    best.resize_with(n, || None);
    let cost_at = |best: &[Option<(u32, RepairStep)>], i: usize| -> u32 {
        if i >= best.len() {
            0
        } else {
            best[i].as_ref().expect("filled back to front").0
        }
    };
    for i in (0..n).rev() {
        let mut chosen: Option<(u32, RepairStep)> = None;
        for (onset_idx, onset) in ph.onsets.iter().enumerate() {
            let Some(after_onset) = match_manner_seq(&segments, i, onset) else {
                continue;
            };
            let available = segments[after_onset.min(n)..]
                .iter()
                .take_while(|s| matches!(s, Segment::Vowel { .. }))
                .count();
            let vowels = available.min(ph.nuclei);
            let pads = ph.nuclei - vowels;
            let after_nucleus = after_onset + vowels;
            for (coda_idx, coda) in ph.codas.iter().enumerate() {
                let Some(after_coda) = match_manner_seq(&segments, after_nucleus, coda) else {
                    continue;
                };
                if after_coda == i {
                    continue; // a syllable must consume at least one segment
                }
                let cost = pads as u32 * EPENTHESIS_COST + cost_at(&best, after_coda);
                if chosen.as_ref().is_none_or(|(c, _)| cost < *c) {
                    chosen = Some((
                        cost,
                        RepairStep::Syllable {
                            onset: onset_idx,
                            coda: coda_idx,
                            vowels,
                            pads,
                        },
                    ));
                }
            }
        }
        let deletion = DELETION_COST + cost_at(&best, i + 1);
        if chosen.as_ref().is_none_or(|(c, _)| deletion < *c) {
            chosen = Some((deletion, RepairStep::Delete));
        }
        best[i] = chosen;
    }

    // Replay the plan front to back.
    let mut out: Vec<Segment> = Vec::with_capacity(n + ph.nuclei);
    let mut i = 0;
    while i < n {
        match &best[i].as_ref().expect("every position has a plan").1 {
            RepairStep::Delete => i += 1,
            RepairStep::Syllable {
                onset,
                coda,
                vowels,
                pads,
            } => {
                let onset_len = ph.onsets[*onset].len();
                out.extend_from_slice(&segments[i..i + onset_len]);
                i += onset_len;
                out.extend_from_slice(&segments[i..i + vowels]);
                i += vowels;
                out.extend(std::iter::repeat_n(epenthetic, *pads));
                let coda_len = ph.codas[*coda].len();
                out.extend_from_slice(&segments[i..i + coda_len]);
                i += coda_len;
            }
        }
    }
    if out.is_empty() {
        out = minimal_syllable(ph, epenthetic);
    }
    out
}

/// The degenerate-input fallback syllable for [`repair_phonotactics`]:
/// the first onset template filled with the first inventory consonant of
/// each required manner, `ph.nuclei` epenthetic vowels, and the first coda
/// template filled the same way. Deterministic and always legal — every
/// drawn template's manners come from the inventory's own consonants.
fn minimal_syllable(ph: &Phonology, epenthetic: Segment) -> Vec<Segment> {
    let first_of = |required: Manner| {
        ph.inventory
            .iter()
            .find(|s| matches!(s, Segment::Consonant { manner, .. } if *manner == required))
            .copied()
    };
    let mut out = Vec::new();
    if let Some(onset) = ph.onsets.first() {
        out.extend(onset.iter().filter_map(|&m| first_of(m)));
    }
    out.extend(std::iter::repeat_n(epenthetic, ph.nuclei));
    if let Some(coda) = ph.codas.first() {
        out.extend(coda.iter().filter_map(|&m| first_of(m)));
    }
    out
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

    /// A hand-built phonology small enough to reason about repair exactly:
    /// one vowel (a), a voiceless alveolar stop (t), an alveolar nasal (n);
    /// onsets admit exactly one stop, nuclei are single vowels, codas admit
    /// one nasal or nothing. So `tan`, `ta`, and `tanta` are legal; a
    /// doubled stop (`tt`) is an illegal cluster no template can host in
    /// one syllable, and a nasal can never begin a syllable.
    fn toy_repair_ph() -> crate::phonology::Phonology {
        use crate::phoneme::{Backness, Height, Place};
        let a = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
        };
        let t = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        };
        let n = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Nasal,
            voiced: true,
        };
        crate::phonology::Phonology {
            inventory: vec![a, t, n],
            onsets: vec![vec![Manner::Stop]],
            nuclei: 1,
            codas: vec![vec![Manner::Nasal], vec![]],
        }
    }

    /// The toy phonology's three segments, for building test inputs.
    fn toy_segments() -> (Segment, Segment, Segment) {
        let ph = toy_repair_ph();
        (ph.inventory[0], ph.inventory[1], ph.inventory[2])
    }

    #[test]
    fn repair_breaks_an_illegal_seam_cluster_by_epenthesis() {
        // "tan" + "tta" (an evolved word whose fortition doubled its onset
        // stop) compound to t-a-n-t-t-a: the tt cluster fits no
        // coda+onset split ([Stop] is not a coda template; [Stop, Stop] is
        // not an onset template), so repair must insert the epenthetic
        // vowel — the first (and only) vowel in inventory order, `a` —
        // after the stranded stop, yielding tan.ta.ta exactly.
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let seam = vec![t, a, n, t, t, a];
        assert!(!conforms(&seam, &ph), "test premise: the seam is illegal");
        let repaired = repair_phonotactics(seam, &ph);
        assert_eq!(
            repaired,
            vec![t, a, n, t, a, t, a],
            "epenthesis must break the tt cluster into tan.ta.ta"
        );
        assert!(conforms(&repaired, &ph));
    }

    #[test]
    fn repair_of_a_valid_sequence_is_identity_and_repair_is_idempotent() {
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        // Valid input: repair is the identity.
        let valid = vec![t, a, n, t, a];
        assert!(conforms(&valid, &ph), "test premise: tanta is legal");
        assert_eq!(repair_phonotactics(valid.clone(), &ph), valid);
        // Idempotence on an invalid input: repairing a repaired sequence
        // changes nothing (repair output always conforms, so the second
        // pass takes the identity branch).
        let seam = vec![t, a, n, t, t, a];
        let once = repair_phonotactics(seam, &ph);
        let twice = repair_phonotactics(once.clone(), &ph);
        assert_eq!(once, twice);
    }

    #[test]
    fn repair_deletes_a_segment_no_template_can_host_as_second_resort() {
        // "tan" + "na" (an evolved word whose cluster-simplify dropped its
        // onset stop, leaving a bare nasal): the toy phonology's nasal can
        // never begin a syllable ([Nasal] is not an onset template) and the
        // trailing vowel then has no onset either — no vowel insertion can
        // host them, so repair falls to deletion and keeps exactly "tan".
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let seam = vec![t, a, n, n, a];
        assert!(!conforms(&seam, &ph), "test premise: the seam is illegal");
        let repaired = repair_phonotactics(seam, &ph);
        assert_eq!(
            repaired,
            vec![t, a, n],
            "deletion (second resort) must drop the unhostable nasal and its \
             stranded vowel, keeping the legal prefix"
        );
        assert!(conforms(&repaired, &ph));
    }

    #[test]
    fn settlement_names_carry_a_per_salt_stem_beyond_the_site_words() {
        // The collision fix (Task 12's census exposed an 86% in-world
        // collision rate): a settlement's glossed name compounds its site
        // word(s) with a per-salt DRAWN stem — the toponymic descriptor +
        // unique element pattern — so the same site yields distinct names
        // across salts. Deity names carry no stem: a single-candidate site
        // yields exactly the repaired site word.
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water"],
        };
        let morph = MorphOptions { honorifics: false };
        let namer = Namer::new(&Seed(9), "test", &ph);

        let plain = render_views(&repair_phonotactics(concept_segments(&lex, "water"), &ph)).roman;

        // Deity: exactly the site word — the stem is Settlement-only.
        let (deity, deity_gloss) = namer.glossed_name(NameKind::Deity, 3, &morph, &site, &lex);
        assert_eq!(deity.roman, plain, "a deity name gains no stem element");
        assert_eq!(deity_gloss, "water");

        // Settlement: distinct names across salts, all glossing to the same
        // concept — the stem is a proper-name element with no concept.
        let mut names = std::collections::BTreeSet::new();
        for salt in 0..12u64 {
            let (name, gloss) = namer.glossed_name(NameKind::Settlement, salt, &morph, &site, &lex);
            assert_eq!(
                gloss, "water",
                "salt {salt}: the stem must not enter the gloss"
            );
            assert_ne!(
                name.roman, plain,
                "salt {salt}: a settlement name must carry a stem beyond the site word"
            );
            names.insert(name.roman);
        }
        assert!(
            names.len() >= 10,
            "per-salt stems must spread settlement names across salts, got {names:?}"
        );
    }

    #[test]
    fn repair_makes_real_evolved_compounds_conform() {
        // The structural invariant repair exists to uphold (spec §8), probed
        // over REAL drawn machinery rather than the toy phonology: every
        // 1- and 2-concept compound of evolved lexicon roots, repaired,
        // parses against the same phonology's drawn templates — across
        // several seeds, since evolve's template-breaking rules (lenition,
        // fortition, cluster simplify, final loss) fire seed-dependently.
        for seed in 0..12u64 {
            let ph = wordy_ph();
            let lex = two_word_lexicon(seed);
            for chosen in [
                vec!["water"],
                vec!["fire"],
                vec!["water", "fire"],
                vec!["fire", "water"],
            ] {
                let raw = compound_segments(&lex, &chosen);
                assert!(!raw.is_empty(), "roots must produce segments");
                let repaired = repair_phonotactics(raw, &ph);
                assert!(
                    conforms(&repaired, &ph),
                    "seed {seed}: repaired compound {chosen:?} must parse \
                     against its own phonotactic templates"
                );
            }
        }
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
