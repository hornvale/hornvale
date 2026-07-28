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
//! `seed.derive(streams::ROOT).derive(StreamLabel::dynamic(species)).derive(streams::NAME).derive(StreamLabel::dynamic(kind_label)).derive(StreamLabel::dynamic(&salt.to_string())).stream()`
//! so a name is a pure, single deterministic function of `(seed, species,
//! kind, salt)` — no re-draw, no dependence on any other name. Uniqueness is
//! not guaranteed here, and since The Wearing retired the drawn settlement
//! stem it is not even rare: two settlements over the same site concepts
//! legitimately share a name (decision 0024 — uniqueness is a property of a
//! *reference*, and no work fixes the collision rate by adding entropy).
//!
//! The composition root deliberately does NOT thread a shared, mutable
//! "used" set through naming, and that purity is load-bearing. But note
//! what it does and does not buy since The Wearing. **`Namer::name`'s v1
//! names remain pin-isolated in the strong sense** — a name depends only on
//! its own cell. **A glossed settlement name no longer is.** Its morphemes
//! are worn against a [`NameCorpus`], a per-culture statistic the
//! composition root counts over that species' whole settlement scatter, so
//! **a pin that moves a species' scatter can move every name of that
//! species** — even for settlements whose own cell did not move.
//!
//! Measured by holding a world's scatter fixed and varying only the corpus
//! (**re-measured for the drawn [`NameShape`], which changes which
//! morphemes a name is built from and therefore which of them the corpus
//! can wear**): 9 of 207 names change at seed 99, 2 of 195 at seed 1, and
//! **none at all** at seeds 42, 777 and 404 — every wear those cultures
//! attempt is surrendered to the survival rule (see
//! [`Namer::worn_compound`]), so those worlds' glossed names happen to be
//! corpus-invariant. The dependence is real but *sparser* than it was
//! before the shape draw, and seed 42 is not a seed to cite it from. This
//! module stays pure (the corpus is an explicit, read-only argument), but
//! the world-level isolation property is gone by design, and a
//! pin-isolation test must not assume it.
//!
//! A *glossed* name additionally has a **shape** ([`NameShape`]) — how many
//! site concepts it compounds — drawn per entity from its culture's own
//! weighted, β-sharpened distribution (`MorphOptions::shape_weights` /
//! `shape_beta`, both keyed upstream from the species' psychology vectors).
//! One shape dominates per people, with a real tail; the profile is what
//! makes a people's toponymy recognizably theirs instead of every name in
//! the world sharing one construction.
//!
//! [`Namer::glossed_name`] (The Words, Task 9) is a later epoch of the same
//! `name` (v1 retired but never deleted — old saves still read it): rooted
//! one leg deeper (`…derive(StreamLabel::dynamic(kind_label)).derive(streams::V3).derive(StreamLabel::dynamic(&salt))`),
//! it compounds 1-3 of a [`SiteConcepts`] site's actual lexicon words
//! instead of always drawing a bare stem, so a name becomes a small true
//! story about the entity it names, with a gloss to match. `/v2` was that
//! epoch's first leg; The Wearing (2026-07-27) supersedes it with `/v3`,
//! which retires the drawn settlement stem and inserts toponymic wear (see
//! [`Namer::wear`]) — both changes to what the method consumes, so both owe
//! an epoch suffix rather than a rename. It is exactly as **pure** as
//! `name` — a function of `(seed, species, kind, v3, salt, site, lexicon,
//! corpus)`, no re-draw, no hidden state — but it is **not** as
//! pin-isolated, and the two must not be conflated. `name` reads only its
//! own salt; `glossed_name` also reads a `lexicon`, a `site` and a
//! [`NameCorpus`], all of which the composition root may compose from a
//! species' full settlement scatter. Absence of a shared mutable "used" set
//! is what keeps this module *pure*; it is emphatically not what would keep
//! a name pin-isolated, and the corpus breaks that property by a different
//! route — read-only, explicit, and upstream. See the note above for what
//! survives and what does not.

use crate::etymology::{draw_wear_cascade, evolve};
use crate::lexicon::{Headedness, LexEntry, Lexicon};
use crate::phoneme::{
    Manner, Segment, espeak_word, ipa, romanize, tone_mark_ipa, tone_mark_roman, tone_of,
};
use crate::phonology::Phonology;
use crate::streams;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, Stream};
use std::collections::BTreeMap;

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

/// How many morphemes a glossed name is built from — the *shape* of the
/// name, drawn per entity from its culture's own weighted distribution
/// (see [`MorphOptions::shape_weights`]).
///
/// Real toponymic systems are not uniform in shape: English is
/// overwhelmingly specific+generic (`Oxford`, `Newcastle`) but keeps a
/// simplex stratum (`York`, `Bath`) and a thin qualified one
/// (`Newcastle-upon-Tyne`, `Great Yarmouth`). What reads as generated is
/// not any individual name but a corpus in which *every* name has the same
/// construction, so the distribution — one dominant shape with a real tail
/// — is the thing being modelled, and it is per-culture: a people's
/// toponymy stays recognizably theirs.
///
/// The variants are ordered by morpheme count, and that order is the
/// [`MorphOptions::shape_weights`] index order. It is therefore a
/// **save-format contract**: permuting these variants silently reassigns
/// every culture's weights.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NameShape {
    /// One morpheme: the specific alone (`York`).
    Simplex,
    /// Two morphemes: the specific and a generic (`Oxford`) — the shape
    /// most real systems lean on.
    SpecificGeneric,
    /// Three morphemes: a specific+generic core with a further qualifier
    /// attached to the whole (`Newcastle-upon-Tyne`). The rarest shape in
    /// every profile the composition root derives.
    Qualified,
}

impl NameShape {
    /// The variants in [`NameShape::morphemes`] order — the index order
    /// [`MorphOptions::shape_weights`] is read in.
    pub const ALL: [NameShape; 3] = [
        NameShape::Simplex,
        NameShape::SpecificGeneric,
        NameShape::Qualified,
    ];

    /// How many site-concept morphemes this shape compounds. The name may
    /// still come out shorter: a site offering fewer candidate concepts
    /// than the drawn shape asks for clamps to what it has (see
    /// [`Namer::choose_concepts`]).
    /// type-audit: bare-ok(count)
    pub fn morphemes(self) -> usize {
        match self {
            NameShape::Simplex => 1,
            NameShape::SpecificGeneric => 2,
            NameShape::Qualified => 3,
        }
    }
}

/// Morphology options the composition root keys from a species' psychology
/// vectors. Plain data — this crate is kernel-only and never imports
/// `hornvale-species`; the mapping (e.g. `Rank` → `honorifics: true`, and
/// the whole shape profile below) lives upstream, in `hornvale-worldgen`.
///
/// Not `Eq`: `shape_weights` and `shape_beta` are floats. They are compared
/// nowhere in this crate; `PartialEq` exists only for test assertions.
/// type-audit: bare-ok(flag: honorifics), bare-ok(ratio: shape_weights), bare-ok(ratio: shape_beta)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MorphOptions {
    /// Whether epithets are prefixed with a drawn honorific affix. Bare
    /// stems (settlement, deity) never consult this field.
    pub honorifics: bool,
    /// This culture's relative preference for each [`NameShape`], indexed
    /// by [`NameShape::ALL`]. Relative, not normalized — only the ratios
    /// are read (`weighted_index` normalizes internally, and the β
    /// sharpening below is scale-invariant: `(c·w)^β = c^β · w^β`).
    /// Non-positive entries are excluded from the draw.
    ///
    /// Read only by [`Namer::glossed_name`] and [`Namer::glossed_concepts`];
    /// the v1 [`Namer::name`] draws a bare stem and never consults it.
    pub shape_weights: [f64; 3],
    /// How stereotyped this people's toponymy is: the exponent each
    /// positive weight is raised to before the draw. `1.0` leaves the
    /// weights as written; `> 1.0` sharpens toward the heaviest (one
    /// pattern dominates and the tail thins); `< 1.0` flattens toward
    /// uniform (a heterogeneous, ad-hoc naming practice).
    pub shape_beta: f64,
}

/// The concept ids an entity's own site facts resolve to, composed
/// upstream by the composition root — never by this crate, which is
/// kernel-only and never learns where a concept came from (a settlement:
/// its cell's biome concept plus its people's presiding-belief phenomenon
/// concept; a deity: its phenomenon concept plus its sentiment's quality
/// concept). [`Namer::glossed_name`] draws which 1-3 of these — of those
/// that actually hold a [`crate::lexicon::LexEntry::Root`] or
/// [`crate::lexicon::LexEntry::Compound`] word in the supplied lexicon — to
/// compound; a concept listed here with no word (a
/// [`crate::lexicon::LexEntry::Gap`], or simply absent) is never picked.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug)]
pub struct SiteConcepts<'a> {
    /// The candidate concept ids, in no particular priority order.
    pub concepts: &'a [&'a str],
}

/// The share of a culture's own names each concept's morpheme appears in —
/// the corpus statistic [`Namer::wear`] grinds against.
///
/// Composed upstream by the composition root, which is the only layer that
/// can see a culture's whole settlement scatter; this crate never counts
/// anything itself, so [`Namer::glossed_name`] stays a **pure** function of
/// its arguments — no shared mutable "used" set, no hidden state, same
/// arguments always the same name.
///
/// **This type is also what ends world-level pin isolation for settlement
/// names, and it should be read as the place that happens.** A name built
/// against a corpus depends on *which other settlements that species has*,
/// because the corpus counts them. Purity is preserved by making that
/// dependence an explicit read-only argument rather than ambient state; it
/// is not removed. Measured by varying only the corpus, on the shipped code
/// (the drawn [`NameShape`] moved these numbers and they were re-measured
/// for it): 9 of 207 names change at seed 99, 2 of 195 at seed 1, 0 of 169
/// at seed 42, 0 of 79 at seed 777, 0 of 76 at seed 404 (see the module
/// docs for why seed 42 is the degenerate case).
///
/// A concept absent from `frequencies` reads as `0.0`: unattested in the
/// corpus, therefore unworn. [`NameCorpus::none`] is the empty corpus every
/// caller that has no name corpus at all (deity and epithet naming, whose
/// name spaces are one-per-belief rather than a scatter of settlements)
/// passes.
/// type-audit: bare-ok(ratio: frequencies)
#[derive(Clone, Copy, Debug)]
pub struct NameCorpus<'a> {
    /// Concept id → the share of this culture's names whose gloss contains
    /// that concept, in `[0, 1]`.
    pub frequencies: &'a BTreeMap<String, f64>,
}

/// The empty corpus [`NameCorpus::none`] hands out. A `static` rather than a
/// per-call allocation: `BTreeMap::new` is `const`, so the empty map costs
/// nothing and every no-corpus caller borrows the same one.
static NO_FREQUENCIES: BTreeMap<String, f64> = BTreeMap::new();

impl NameCorpus<'static> {
    /// The empty corpus: every concept reads `0.0`, so nothing wears.
    pub fn none() -> NameCorpus<'static> {
        NameCorpus {
            frequencies: &NO_FREQUENCIES,
        }
    }
}

impl NameCorpus<'_> {
    /// The share of this culture's names `concept`'s morpheme appears in,
    /// or `0.0` when the corpus has never seen it.
    /// type-audit: bare-ok(identifier-text: concept), bare-ok(ratio: return)
    pub fn frequency_of(&self, concept: &str) -> f64 {
        self.frequencies.get(concept).copied().unwrap_or(0.0)
    }
}

/// The corpus share at or above which a morpheme starts to wear.
///
/// Below it a form is returned untouched — a rare generic stays whole,
/// which is `-thwaite` beside `-ham`, and is what makes the resulting
/// transparency a DISTRIBUTION rather than a new constant. A quarter of a
/// culture's names is the stipulated line between a word that happens to
/// recur and one doing generic duty; it is a calibration dial, not a
/// derived quantity, and the Lab's name-length metrics are what move it.
const WEAR_FLOOR: f64 = 0.25;

/// A generated name in its three views: `roman` is what commits as the
/// `name` fact (the almanac's ASCII-ish spelling); `ipa` is the book's
/// phonetic rendering; `espeak` is the espeak-ng formulation. None are stored
/// independently of the segments that produced it — all three are views built
/// in the same pass.
/// type-audit: bare-ok(identifier-text)
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
/// pass it straight to [`segments_of`] without this module exposing its
/// fields.
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
    /// type-audit: bare-ok(identifier-text: species)
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
    /// any other name — this, the **v1** draw, is pin-isolated in the strong
    /// sense: it reads nothing but its own salt. [`Namer::glossed_name`] is
    /// not (see the module docs). Uniqueness across a world's names is
    /// de-facto, not guaranteed.
    /// type-audit: pending(wave-3: salt)
    pub fn name(&self, kind: NameKind, salt: u64, morph: &MorphOptions) -> GeneratedName {
        let mut stream = self
            .seed
            .derive(streams::ROOT)
            .derive(StreamLabel::dynamic(&self.species))
            .derive(streams::NAME)
            .derive(StreamLabel::dynamic(kind.label()))
            .derive(StreamLabel::dynamic(&salt.to_string()))
            .stream();
        self.build_name(kind, morph, &mut stream)
    }

    /// The stream every `/v3`-epoch glossed draw runs off: `name`'s path
    /// with a `"v3"` leg right after `kind`'s label, so it is entirely
    /// distinct from [`Namer::name`]'s v1 stream and from the retired
    /// [`streams::V2`] epoch. Built here, in one place, so
    /// [`Namer::glossed_name`] and [`Namer::glossed_concepts`] cannot drift
    /// apart: both replay the identical draw sequence from the identical
    /// derivation.
    fn glossed_stream(&self, kind: NameKind, salt: u64) -> Stream {
        self.seed
            .derive(streams::ROOT)
            .derive(StreamLabel::dynamic(&self.species))
            .derive(streams::NAME)
            .derive(StreamLabel::dynamic(kind.label()))
            .derive(streams::V3)
            .derive(StreamLabel::dynamic(&salt.to_string()))
            .stream()
    }

    /// Draw this name's [`NameShape`] from `morph`'s per-culture weighted
    /// distribution, β-sharpened.
    ///
    /// The sharpening is the same idiom [`crate::schemas::select_schema`]
    /// uses, and it copies both of that function's safety rules verbatim
    /// because both are load-bearing here too:
    ///
    /// - `powf` is [`hornvale_kernel::math::powf`], the portable libm
    ///   route, never the inherent `f64::powf` — the platforms disagree in
    ///   the last ULP and this value feeds a draw, so an inherent `powf`
    ///   would make the *name* platform-dependent, not merely the last
    ///   digit of a serialized float.
    /// - A non-positive weight sharpens to `0.0` rather than being handed
    ///   to `powf`, because an even β would flip a deliberately-excluded
    ///   negative weight back to a positive one (`(-1.0).powf(2.0) == 1.0`)
    ///   and revive a shape the culture meant to have none of.
    ///
    /// Consumes exactly one `next_f64` from `stream` (inside
    /// [`Stream::weighted_index`]) **whenever any sharpened weight is
    /// positive**, which is every profile the composition root derives.
    /// Two paths consume nothing at all, and both matter to the
    /// stream-consumption contract:
    ///
    /// - the caller returns before reaching this method when the site
    ///   offers no candidate concept;
    /// - a fully degenerate profile consumes nothing *here*, because
    ///   [`Stream::weighted_index`] tests its total and returns `None`
    ///   **before** it draws. The fallback below is therefore not just a
    ///   different answer, it is a different amount of stream — exercised
    ///   by `a_degenerate_shape_profile_falls_back_to_the_unclampable_shape`.
    ///
    /// Falls back to [`NameShape::Simplex`] when every weight is
    /// non-positive, which is the only shape realizable from *any*
    /// non-empty candidate list and so can never itself be clamped. No
    /// profile the composition root derives is degenerate; this is the
    /// answer for a hand-built one that is.
    fn draw_shape(stream: &mut Stream, morph: &MorphOptions) -> NameShape {
        let sharpened: Vec<f64> = morph
            .shape_weights
            .iter()
            .map(|w| {
                if *w > 0.0 {
                    hornvale_kernel::math::powf(*w, morph.shape_beta)
                } else {
                    0.0
                }
            })
            .collect();
        match stream.weighted_index(&sharpened) {
            Some(i) => NameShape::ALL[i],
            None => NameShape::Simplex,
        }
    }

    /// Pick which of `site`'s concepts this name is built from: those that
    /// actually hold a word in `lexicon` (see [`SiteConcepts`]), narrowed by
    /// a drawn [`NameShape`] to that shape's morpheme count, in the order
    /// drawn. Empty when no site concept holds a word — the fallback case,
    /// in which no draw is made at all, so the stream is left exactly where
    /// the caller found it.
    ///
    /// **The shape is drawn before the pool is consulted, and unconditionally
    /// once there is a pool at all.** That is deliberate and it is what makes
    /// the drawn shape a per-culture statistic rather than a per-site one: a
    /// site with only two candidate concepts still draws `Qualified` at its
    /// culture's rate and merely *realizes* it as a two-morpheme name. (The
    /// superseded `/v2` code drew nothing at all when the pool held one
    /// candidate, so pool size and consumption were entangled; that is one
    /// of the two consumption changes `/v3` covers.)
    fn choose_concepts<'c>(
        &self,
        stream: &mut Stream,
        morph: &MorphOptions,
        site: &SiteConcepts<'c>,
        lexicon: &Lexicon,
    ) -> Vec<&'c str> {
        let candidates: Vec<&'c str> = site
            .concepts
            .iter()
            .copied()
            .filter(|concept| holds_word(lexicon, concept))
            .collect();
        if candidates.is_empty() {
            return Vec::new();
        }
        let take = Self::draw_shape(stream, morph)
            .morphemes()
            .min(candidates.len());
        let mut pool = candidates;
        let mut chosen: Vec<&'c str> = Vec::with_capacity(take);
        for _ in 0..take {
            let idx = stream.range_u32(0, (pool.len() - 1) as u32) as usize;
            chosen.push(pool.remove(idx));
        }
        chosen
    }

    /// The site concepts [`Namer::glossed_name`] will compound for this
    /// `(kind, salt, site, lexicon)` — exactly the ones its gloss will name,
    /// in gloss order, computed without rendering anything.
    ///
    /// `morph` is read for its shape weights alone (the honorific flag is a
    /// render-time embellishment applied after the concepts are picked, so
    /// it cannot change them) — but the weights DO change them, so a caller
    /// re-deriving a name's concepts must pass the same options the naming
    /// pass will.
    ///
    /// This exists for the composition root's benefit: a culture's
    /// [`NameCorpus`] is *the share of its names each morpheme appears in*,
    /// which cannot be known until the names' concepts are known, and
    /// worldgen is the only layer that sees the whole scatter. So worldgen
    /// asks this first, counts, and then names. That two-pass composition is
    /// self-consistent because **wear consumes nothing from the name
    /// stream** — [`Namer::wear`] draws its cascade from the lexicon's own
    /// derivation — so a name's chosen concepts are independent of the
    /// corpus, and the second pass picks exactly what the first one
    /// reported. `glossed_name` shares this method verbatim, so the two can
    /// never disagree.
    /// type-audit: pending(wave-3: salt), bare-ok(identifier-text: return)
    pub fn glossed_concepts<'c>(
        &self,
        kind: NameKind,
        salt: u64,
        morph: &MorphOptions,
        site: &SiteConcepts<'c>,
        lexicon: &Lexicon,
    ) -> Vec<&'c str> {
        let mut stream = self.glossed_stream(kind, salt);
        self.choose_concepts(&mut stream, morph, site, lexicon)
    }

    /// Wear `segments` down when `frequency` — the share of this culture's
    /// names the morpheme appears in — reaches [`WEAR_FLOOR`], by running
    /// the culture's own [`crate::etymology::draw_wear_cascade`] cascade
    /// over it — [`crate::etymology::CascadeRegime::WEAR`]'s 1-2 rules, drawn
    /// off a leg of their own so they are rules the words have not already
    /// undergone (see [`streams::WEAR`]).
    ///
    /// This is the mechanism behind `-ham`, `-ton` and `-by`: the generic is
    /// the highest-frequency morpheme in the whole name corpus, so Zipf's
    /// law of abbreviation grinds it hardest. Keying on frequency rather
    /// than on the compound's HEAD slot is deliberate (ledger #3): it
    /// derives the generic/specific asymmetry rather than authoring it, and
    /// it correctly wears a *specific* that happens to be ubiquitous in one
    /// culture.
    ///
    /// Below [`WEAR_FLOOR`] the form is returned untouched — a rare generic
    /// stays whole, which is `-thwaite` beside `-ham`.
    ///
    /// **What the wear actually is**: the language's own drawn sound
    /// changes, run one epoch further. Etymology's drawn rule kinds already
    /// include [`crate::etymology::RuleKind::ClusterSimplify`] and
    /// [`crate::etymology::RuleKind::FinalLoss`], the two changes that
    /// perform real toponymic reduction, so wear needs no bespoke clipping
    /// pass and comes out Neogrammarian-regular for free, with a printable
    /// derivation like every other form. It is drawn, not stipulated,
    /// which means it is **not guaranteed to shorten**: a culture whose
    /// wear cascade happens to draw only length-preserving rules (lenition,
    /// vowel shift, tonogenesis) wears its frequent morphemes in quality
    /// without wearing them in length. That is the honest consequence of
    /// deriving the wear from the language rather than authoring it, and it
    /// is why toponymic reduction is a distribution across cultures rather
    /// than a uniform shortening.
    ///
    /// Pure: a function of `(seed, species, ph, segments, frequency)` and
    /// nothing else.
    /// type-audit: bare-ok(ratio: frequency)
    pub fn wear(&self, segments: &[Segment], frequency: f64) -> Vec<Segment> {
        if frequency < WEAR_FLOOR {
            return segments.to_vec();
        }
        let cascade = draw_wear_cascade(&self.seed, &self.species);
        evolve(segments, &cascade, self.ph).modern
    }

    /// Join the already-resolved `parts` (one per chosen site concept, in
    /// draw order) into a compound, in `lexicon`'s drawn [`Headedness`]
    /// order.
    ///
    /// - One part ([`NameShape::Simplex`]): returned unchanged.
    /// - Two ([`NameShape::SpecificGeneric`]): a fresh modifier/head pair —
    ///   the first-drawn concept is the modifier, the second-drawn is the
    ///   head. An arbitrary but deterministic convention, distinct from any
    ///   recipe compounding already inside `lexicon`.
    /// - Three ([`NameShape::Qualified`]): the first two form that same
    ///   core, and each further part is a **qualifier of the whole name
    ///   already built** — joined as the modifier with the accumulated
    ///   compound as the head. That bracketing is the one real qualified
    ///   toponyms have (`[[New castle] upon Tyne]`), and it renders as
    ///   either attested English pattern depending on the culture's drawn
    ///   headedness: qualifier-trailing under `HeadFirst`
    ///   (`Newcastle upon Tyne`), qualifier-leading under `HeadLast`
    ///   (`Great Yarmouth`).
    ///
    /// The fold is written for any part count, but [`NameShape`] tops out
    /// at three and the caller clamps to the candidate pool, so three is
    /// the most it is ever handed.
    fn join_parts(lexicon: &Lexicon, parts: Vec<Vec<Segment>>) -> Vec<Segment> {
        let mut parts = parts.into_iter();
        // Fail fast, in release as well as debug. An empty part list would
        // otherwise fold to an empty segment vector and surface as a
        // settlement with an EMPTY NAME — a silent, world-committed
        // falsehood rather than a crash. The caller filters the empty case
        // (it is the bare-stem fallback) before compounding, so reaching
        // here means that filter broke.
        let mut whole = parts.next().expect(
            "the caller filters the empty case (the bare-stem fallback) before compounding",
        );
        if let Some(head) = parts.next() {
            whole = join_by_headedness(lexicon.headedness, whole, head);
        }
        for qualifier in parts {
            whole = join_by_headedness(lexicon.headedness, qualifier, whole);
        }
        whole
    }

    /// The repaired segments [`Namer::glossed_name`] surfaces for its 1-3
    /// `chosen` site concepts, each **worn to its own corpus frequency
    /// first** — wearing as many of them as can survive repair, and
    /// reporting how many had to give their wear up.
    ///
    /// Wear is applied **per morpheme, before the join**, which is what
    /// keeps it frequency-keyed rather than slot-keyed. Wearing the
    /// assembled compound instead would be slot-keyed by accident:
    /// `FinalLoss` only ever touches a word's last segment, so it would
    /// grind whichever morpheme the headedness happened to put last, no
    /// matter how rare that morpheme was. Per-morpheme, a ubiquitous
    /// modifier wears and a rare head beside it does not.
    ///
    /// # The survival rule, and why it is not optional
    ///
    /// Wear and repair interact destructively if left alone, and the first
    /// implementation of this method shipped that bug. Repair is the
    /// identity for a culture's **attested** words (The Speakable: the
    /// lexicon's own forms are admitted verbatim at zero cost) — but a worn
    /// form is by construction *no longer* one of them, so it falls through
    /// to the syllabifier, and when no template can host it the cheapest
    /// remaining plan is [`RepairStep::Delete`]. Measured on seed 42: nine
    /// settlements committed a `name-gloss` fact naming a morpheme their
    /// name did not contain — `marsh` worn to `Soov` and then deleted
    /// outright, so `marsh-tropical-rainforest` and `tropical-rainforest`
    /// rendered byte-identically. That is a falsehood in the ledger.
    ///
    /// It is also the wrong *linguistics*. `-ham` is a reduced **survival**;
    /// erasure is not opacification, and a campaign whose thesis is that a
    /// worn morpheme stays recognizable cannot ship a wear that deletes one.
    ///
    /// So the rule: **a worn morpheme must leave a contiguous reflex in the
    /// repaired surface form.** This method wears every eligible morpheme,
    /// repairs, and checks; if any part was annihilated it gives up the wear
    /// on the **least frequent** still-worn morpheme and tries again, down
    /// to no wear at all (which is the pre-wear behavior and repairs to the
    /// identity). Giving up in ascending frequency order keeps even the
    /// fallback frequency-keyed rather than slot-keyed.
    ///
    /// Two deliberate coarsenesses, both erring toward keeping the name
    /// honest rather than toward keeping the wear:
    ///
    /// - **Contiguity is stricter than "recognizable".** An epenthetic vowel
    ///   inserted *inside* a worn morpheme leaves it perfectly audible but
    ///   fails [`contains_run`], and the wear is forfeited. A looser
    ///   subsequence test would keep more wear at the cost of a check that
    ///   can no longer tell reduction from scattering, so the strict form
    ///   stands.
    /// - **The check covers unworn parts too.** A surrender is only ever
    ///   *applied* to a genuinely worn morpheme (the give-up order is built
    ///   from the worn ones), but it can be *triggered* by an unworn part's
    ///   annihilation — a pre-existing possibility with nothing to do with
    ///   wear — in which case a sibling loses perfectly good wear and the
    ///   count over-reports. Not observed: all **39** surrenders measured
    ///   on the shipped seed-42 pipeline had at least one genuinely worn
    ///   part among the annihilated ones. (Was 30 before the drawn
    ///   [`NameShape`] gave some compounds a third morpheme; re-measured
    ///   with the same instrumentation, not carried forward.)
    ///
    /// The alternative remedy — admitting worn forms into the attested tier
    /// so repair leaves them alone — was rejected: the Lab's own
    /// romanization validator reconstructs that tier from the lexicon alone
    /// (`attested_roman_forms`, `windows/lab`), so a name containing a form
    /// the lexicon does not hold would stop validating. Falling back keeps
    /// repair's guarantee exactly as it was: every name still conforms
    /// under `(phonology, attested-from-lexicon)`.
    ///
    /// Returns the repaired segments and **how many morphemes surrendered
    /// their wear** — the fallback count, which nothing in a committed world
    /// depends on but which is what the fallback rate is measured from.
    ///
    /// Consumes no stream draws — wear draws its cascade off the lexicon
    /// derivation and repair is pure — so none of this touches the naming
    /// epoch's stream-consumption contract.
    fn worn_compound(
        &self,
        lexicon: &Lexicon,
        chosen: &[&str],
        corpus: &NameCorpus,
        attested: &[Vec<Segment>],
    ) -> (Vec<Segment>, usize) {
        let raw: Vec<Vec<Segment>> = chosen
            .iter()
            .map(|concept| concept_segments(lexicon, concept))
            .collect();
        // Which morphemes wear at all, and how strongly — the give-up order
        // below is ascending frequency, so the morpheme said least often is
        // the first to keep its whole form.
        let mut worn: Vec<Option<Vec<Segment>>> = chosen
            .iter()
            .zip(raw.iter())
            .map(|(concept, form)| {
                let out = self.wear(form, corpus.frequency_of(concept));
                if out == *form { None } else { Some(out) }
            })
            .collect();
        let mut give_up_order: Vec<usize> =
            (0..chosen.len()).filter(|i| worn[*i].is_some()).collect();
        // Ascending frequency; ties by concept id, then index — a total,
        // float-free order (the frequencies are compared with `total_cmp`,
        // never `<`).
        give_up_order.sort_by(|a, b| {
            corpus
                .frequency_of(chosen[*a])
                .total_cmp(&corpus.frequency_of(chosen[*b]))
                .then_with(|| chosen[*a].cmp(chosen[*b]))
                .then_with(|| a.cmp(b))
        });

        let mut surrendered = 0usize;
        let attempts = give_up_order.len() + 1;
        for attempt in 0..attempts {
            let parts: Vec<Vec<Segment>> = raw
                .iter()
                .enumerate()
                .map(|(i, form)| worn[i].clone().unwrap_or_else(|| form.clone()))
                .collect();
            let repaired =
                repair_phonotactics(Self::join_parts(lexicon, parts.clone()), self.ph, attested);
            if parts.iter().all(|part| contains_run(&repaired, part)) {
                return (repaired, surrendered);
            }
            // Annihilated: give up the least-frequent surviving wear and
            // retry. The last iteration has every `worn[i] == None`, so it
            // tries the fully unworn compound — which is what this code
            // built before wear existed, and which repair leaves alone
            // because every part is attested.
            if attempt < give_up_order.len() {
                worn[give_up_order[attempt]] = None;
                surrendered += 1;
            }
        }
        // Unreachable in practice: the loop's last iteration already tried
        // the fully unworn compound, so falling out means even THAT failed
        // containment — repair is not the identity for this culture's own
        // attested words, which is a pre-existing property with nothing to
        // do with wear. This recomputes that same unworn repair rather than
        // caching it, because the branch is cold and the duplication is
        // one line; it is a RECOMPUTATION, not a different result.
        (
            repair_phonotactics(Self::join_parts(lexicon, raw), self.ph, attested),
            surrendered,
        )
    }

    /// Draw a *glossed* name of `kind` for `salt`, at the `/v3` epoch: the
    /// derive path gains a `"v3"` leg right after `kind`'s label, so this
    /// draws from a stream entirely distinct from [`Namer::name`]'s (v1
    /// stays retired, never reused) and from the superseded `/v2` epoch. The
    /// stream draws a [`NameShape`] from `morph`'s per-culture weighted
    /// distribution and picks that many of `site`'s concepts that actually hold a word in
    /// `lexicon` (see [`SiteConcepts`]), **wears each of them against
    /// `corpus`** (see [`Namer::wear`]), compounds their worn modern-form
    /// segments in `lexicon`'s drawn [`Headedness`] order, and applies
    /// `kind`'s existing morphology on top — an honorific prefix for
    /// [`NameKind::Epithet`] when `morph.honorifics` is set (status-basis
    /// keying intact; reduplication is a v1-only embellishment for freshly
    /// drawn stems and has no analog here). Returns the name's three views
    /// plus the gloss — the chosen concepts joined with `"-"` (e.g.
    /// `"ice-home"`) — so the gloss is always truthfully a subset of
    /// `site.concepts`. Wear changes sound, never meaning: a worn morpheme
    /// still glosses to its concept, which is precisely the opacification
    /// `-ham` underwent.
    ///
    /// The `/v3` epoch is what The Wearing (2026-07-27) owes for three
    /// changes to what this method consumes: the wear itself, the drawn
    /// [`NameShape`] (which replaced a bare `range_u32(1, 2)` that was
    /// skipped entirely for a one-candidate site), and the retirement of
    /// the per-salt 2-3 syllable **drawn settlement stem** the `/v2` epoch
    /// appended to every settlement name. All three landed inside the same
    /// campaign and `/v3` has never been in a released world — the census
    /// and every committed fixture are regenerated once, at the campaign's
    /// close — so they share one epoch rather than minting `/v4` and `/v5`
    /// for save formats no save ever held. That stem existed as a collision
    /// fix; decision 0024 ratified that uniqueness is a reference-time
    /// property and that no future work fixes the collision rate by adding
    /// entropy, "which lengthens names without addressing the structural
    /// fact that meaning collides". It was the single largest length
    /// contributor in a settlement name and the only part of it that named
    /// nothing. Settlement collisions consequently rise; that is expected,
    /// and the relief 0024 names is render-time qualification, never more
    /// drawn entropy.
    ///
    /// If *no* site concept holds a word, falls back to a bare stem drawn
    /// exactly as [`Namer::build_name`] draws v1 names, but still under
    /// this method's own `/v3` stream: the result stays a pure
    /// `(seed, species, kind, v3, salt)` function, distinct from `name`'s
    /// v1 output, with an empty gloss (no true story to tell — callers
    /// should skip the `name-gloss` fact when the gloss is empty).
    /// type-audit: pending(wave-3: salt), bare-ok(identifier-text: return)
    pub fn glossed_name(
        &self,
        kind: NameKind,
        salt: u64,
        morph: &MorphOptions,
        site: &SiteConcepts,
        lexicon: &Lexicon,
        corpus: &NameCorpus,
    ) -> (GeneratedName, String) {
        let mut stream = self.glossed_stream(kind, salt);
        let chosen = self.choose_concepts(&mut stream, morph, site, lexicon);

        if chosen.is_empty() {
            let name = self.build_name(kind, morph, &mut stream);
            return (name, String::new());
        }

        // Repair AFTER compounding and wear, BEFORE morphology (the
        // permanent order — wear before repair, because wear may produce a
        // form the synchronic templates reject and repair is what adapts
        // it):
        // evolved roots only guarantee inventory membership, not template
        // conformance, so the compound is adapted to the synchronic
        // phonotactics first (see [`repair_phonotactics`] — the spec §8
        // structural invariant "every name well-formed for its language").
        // The attested tier (The Speakable) is how that gap closes for
        // material that is itself one of the language's own words — no
        // template edit needed, just verbatim admission: repair runs over
        // the canon templates plus the lexicon's attested tier, so it is
        // the identity for native compounds by construction. A WORN
        // morpheme is not attested, though, so `worn_compound` owns the
        // compound-and-repair pair together: it wears, repairs, and gives
        // wear back up on any morpheme repair would have annihilated (see
        // its docs — the alternative was a name that lies about its own
        // gloss). The honorific prefix below is a freshly drawn template
        // syllable — conformant by construction — so prefixing it onto a
        // repaired word keeps the whole name conformant. Neither wear nor
        // repair changes MEANING: the gloss is computed from `chosen`
        // alone.
        let attested = attested_forms(lexicon);
        let (mut segments, _surrendered) = self.worn_compound(lexicon, &chosen, corpus, &attested);
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
        // Segment quality first, then its tone mark (spec §6): a combining
        // diacritic on the roman vowel, a Chao tone letter after the IPA
        // vowel. Both are empty for `Tone::Neutral`, so an atonal word renders
        // exactly as before the tone tier. espeak stays tone-blind — lexical
        // tone is espeak-weak, a known audio limit (spec §9), so the segmental
        // formulation stands and the pitch is simply not voiced.
        roman.push_str(romanize(seg));
        roman.push_str(tone_mark_roman(tone_of(seg)));
        ipa_str.push_str(ipa(seg));
        ipa_str.push_str(tone_mark_ipa(tone_of(seg)));
    }
    GeneratedName {
        roman: capitalize_first(&roman),
        ipa: ipa_str,
        espeak: espeak_word(segments),
    }
}

/// Flatten `syllables` (onset → nucleus → coda, in sequence) into their
/// ordered segments, without rendering any surface view — the draw-free,
/// string-free half of [`views_of`]. For callers that need only the
/// segments: etymology's `proto_root` (one call per species × concept) and
/// `glossed_name`'s settlement stem, which would otherwise build and
/// discard three rendered strings per draw. Rendering is a pure function of
/// the segments ([`render_views`]), so which half a caller takes can never
/// change what was drawn. `pub(crate)` for the cross-module reuse — the
/// carry-forward invariant stands: no caller constructs a [`Segment`]
/// outside this module's machinery.
pub(crate) fn segments_of(syllables: &[Syllable]) -> Vec<Segment> {
    syllables
        .iter()
        .flat_map(|syllable| syllable.segments().copied())
        .collect()
}

/// Flatten `syllables` via [`segments_of`] and render all three surface
/// views via [`render_views`]. `Namer::build_name` uses the
/// `GeneratedName` half; callers that would discard it use [`segments_of`]
/// directly. `pub(crate)` for that cross-module reuse.
pub(crate) fn views_of(syllables: &[Syllable]) -> (Vec<Segment>, GeneratedName) {
    let segments = segments_of(syllables);
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

/// The attested tier (The Speakable): every modern root form the lexicon
/// actually holds, admitted verbatim as parse units beside the drawn
/// canon templates. Descriptive phonotactics — the templates are the
/// morphology's grammar, the lexicon is its own evidence — so a name
/// compounded from the language's own words never needs repair. Deduped
/// and sorted longest-first (ties by `Segment`'s total order) so the
/// repair DP's first-match tie-break is deterministic. Draw-free and
/// pure; Gaps and Compounds contribute nothing (a compound's segments
/// are its two roots in sequence, each already attested).
/// Wired into [`Namer::glossed_name`] (The Speakable Task 3), which
/// computes this once per name and passes it to
/// [`repair_phonotactics`]/[`conforms`] as the attested tier.
pub(crate) fn attested_forms(lexicon: &Lexicon) -> Vec<Vec<Segment>> {
    let mut forms: Vec<Vec<Segment>> = lexicon
        .entries()
        .filter_map(|(_, entry)| match entry {
            LexEntry::Root { derivation, .. } if !derivation.modern.is_empty() => {
                Some(derivation.modern.clone())
            }
            _ => None,
        })
        .collect();
    forms.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    forms.dedup();
    forms
}

/// Whether `segments` parses as a sequence of syllables under `ph`'s drawn
/// phonotactic templates, or as attested lexicon words admitted verbatim:
/// each syllable an onset matching one of `ph.onsets` exactly (by manner
/// sequence), then exactly `ph.nuclei` vowels, then a coda matching one of
/// `ph.codas` — OR, at any position, one whole word from `attested`
/// (The Speakable's attested tier). A backtracking parse (every onset/coda
/// split AND every attested match is explored), so any parseable sequence
/// is accepted; the empty sequence is not a word. This is the invariant
/// [`repair_phonotactics`] restores and the property the Lab's
/// romanization-level validator re-checks from the committed string.
fn conforms(segments: &[Segment], ph: &Phonology, attested: &[Vec<Segment>]) -> bool {
    fn from(segments: &[Segment], pos: usize, ph: &Phonology, attested: &[Vec<Segment>]) -> bool {
        if pos == segments.len() {
            return true;
        }
        for word in attested {
            debug_assert!(!word.is_empty(), "attested forms must be non-empty");
            if segments[pos..].starts_with(word.as_slice())
                && from(segments, pos + word.len(), ph, attested)
            {
                return true;
            }
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
                    && from(segments, after_coda, ph, attested)
                {
                    return true;
                }
            }
        }
        false
    }
    !segments.is_empty() && from(segments, 0, ph, attested)
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
    /// Emit one attested lexicon word verbatim starting at this position
    /// (`len` segments, zero cost — the attested tier).
    Attested {
        /// How many input segments the attested word spans.
        len: usize,
    },
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

/// **The phonotactic repair rule** (permanent formula, The Words Task 9;
/// the attested tier is The Speakable, Task 1): make an arbitrary segment
/// sequence — a compound of evolved lexicon roots, whose sound changes are
/// only guaranteed to land in the phoneme *inventory*, never inside the
/// synchronic syllable *templates* — parse under `ph`'s drawn phonotactics,
/// the way real languages adapt loanwords and compounds. Two tiers of
/// legal unit: the drawn canon templates (the morphology's grammar) and
/// `attested` — whole modern root forms the lexicon actually holds,
/// admitted verbatim (the lexicon's own evidence). Consequence: a name
/// compounded purely from the language's own attested words parses as a
/// concatenation of attested units and needs no repair at all — repair
/// stays the identity on it, same as already-canon-legal input. Pure and
/// deterministic: a function of the segments, the phonology, and the
/// attested set alone, no stream draws.
///
/// 1. **Already-legal input is untouched** ([`conforms`] short-circuit) —
///    repair of a valid sequence is the identity, which also makes repair
///    idempotent (its output always conforms).
/// 2. **Resyllabify with minimal edits.** The sequence is re-parsed into
///    attested words and syllables against `attested` and the drawn
///    onset/nucleus/coda templates; where a stretch cannot parse, two edits
///    are available: **epenthesis** — insert the language's canonical
///    epenthetic vowel (the first vowel in the drawn inventory's order —
///    deterministic, not drawn) to complete a nucleus and thereby break an
///    illegal cluster — and **deletion** of a segment no template can host
///    in its position. An attested match costs nothing (zero, cheaper than
///    either edit). Epenthesis costs [`EPENTHESIS_COST`] per inserted
///    vowel, deletion [`DELETION_COST`] per dropped segment, so insertion
///    is preferred one-for-one over deletion — but the plan is chosen by
///    total cost, not by a per-edit preference, so a single deletion can
///    still beat a run of three or more epentheses the same stretch would
///    otherwise need. The minimal-cost plan is found by dynamic
///    programming over input positions; ties break deterministically
///    toward the earlier-considered option — attested words (longest
///    first, per [`attested_forms`]'s ordering) before the earlier-listed
///    template pair (onsets then codas, in their drawn order), with
///    deletion considered last.
/// 3. **Degenerate-input fallback**: if the minimal plan deletes
///    everything (an input with no vowel and no template-hostable
///    consonant — unreachable from real lexicon roots, which always carry
///    nuclei), emit one minimal legal syllable instead: the first onset
///    template filled with the first inventory consonant of each required
///    manner, an all-epenthetic nucleus, and the first coda template
///    filled the same way. A name is never empty.
fn repair_phonotactics(
    segments: Vec<Segment>,
    ph: &Phonology,
    attested: &[Vec<Segment>],
) -> Vec<Segment> {
    if conforms(&segments, ph, attested) {
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
        for word in attested {
            debug_assert!(!word.is_empty(), "attested forms must be non-empty");
            if segments[i..].starts_with(word.as_slice()) {
                let cost = cost_at(&best, i + word.len());
                if chosen.as_ref().is_none_or(|(c, _)| cost < *c) {
                    chosen = Some((cost, RepairStep::Attested { len: word.len() }));
                }
            }
        }
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
            RepairStep::Attested { len } => {
                out.extend_from_slice(&segments[i..i + len]);
                i += len;
            }
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

/// Whether `haystack` contains `needle` as a **contiguous** run of
/// segments. The precise reading of "a worn morpheme must leave a
/// recognizable reflex": the surface form still says the morpheme, in one
/// piece, in order. An empty needle is trivially contained (a concept with
/// no segments cannot be annihilated); this stays total rather than
/// panicking on a lexicon invariant this module does not enforce.
fn contains_run(haystack: &[Segment], needle: &[Segment]) -> bool {
    if needle.is_empty() {
        return true;
    }
    haystack.len() >= needle.len() && haystack.windows(needle.len()).any(|w| w == needle)
}

/// Whether `lexicon` holds an actual word — a [`LexEntry::Root`] or
/// [`LexEntry::Compound`], never a [`LexEntry::Gap`] or an absent entry —
/// for `concept`. The filter [`Namer::glossed_name`] applies to a site's
/// candidate concepts before picking which 1-3 to compound.
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
    use crate::etymology::CascadeRegime;
    use crate::lexicon::{ExposureClass, build_lexicon};
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};
    use hornvale_kernel::Seed;
    use std::collections::BTreeMap;

    /// A neutral shape profile for tests that are not about shape: an even
    /// three-way preference at β = 1, so `glossed_name` draws each
    /// [`NameShape`] with probability 1/3. No world uses it — the
    /// composition root derives a per-culture profile — and every test that
    /// IS about shape states its own weights inline.
    fn morph(honorifics: bool) -> MorphOptions {
        MorphOptions {
            honorifics,
            shape_weights: [1.0, 1.0, 1.0],
            shape_beta: 1.0,
        }
    }

    /// The empirical [`NameShape`] distribution `draw_shape` produces for
    /// `(weights, beta)`, one draw per salt off a distinct stream — the
    /// same one-draw-per-entity shape production uses, not a single stream
    /// drawn from repeatedly.
    fn shape_distribution(weights: [f64; 3], beta: f64) -> [f64; 3] {
        const DRAWS: u64 = 20_000;
        let morph = MorphOptions {
            honorifics: false,
            shape_weights: weights,
            shape_beta: beta,
        };
        let mut counts = [0u64; 3];
        for salt in 0..DRAWS {
            let mut stream = Seed(7)
                .derive(StreamLabel::dynamic(&salt.to_string()))
                .stream();
            let shape = Namer::draw_shape(&mut stream, &morph);
            let i = NameShape::ALL
                .iter()
                .position(|s| *s == shape)
                .expect("draw_shape returns a NameShape::ALL member");
            counts[i] += 1;
        }
        [
            counts[0] as f64 / DRAWS as f64,
            counts[1] as f64 / DRAWS as f64,
            counts[2] as f64 / DRAWS as f64,
        ]
    }

    /// The closed-form share `draw_shape` should produce: `wᵢ^β / Σ wⱼ^β`.
    /// Written out independently of the implementation rather than calling
    /// into it, so agreement between the two is evidence and not a
    /// tautology.
    fn expected_shares(weights: [f64; 3], beta: f64) -> [f64; 3] {
        let p: Vec<f64> = weights
            .iter()
            .map(|w| hornvale_kernel::math::powf(*w, beta))
            .collect();
        let total: f64 = p.iter().sum();
        [p[0] / total, p[1] / total, p[2] / total]
    }

    #[test]
    fn the_shape_draw_follows_its_weights_and_beta_actually_sharpens_them() {
        // Two claims, and the second is the one worth testing: that
        // `shape_beta` is a live dial rather than a field that is read and
        // discarded. A dial that does nothing is worse than no dial,
        // because the composition root's per-culture story would be a
        // comment asserting a property the code does not have.
        //
        // Tolerance: 20,000 draws puts the standard error on any share
        // below 0.004, so 0.02 is ~5σ — tight enough to catch a wrong
        // exponent, loose enough never to flake. The draw is deterministic
        // anyway (a fixed seed, fixed salts), so "flake" here means "breaks
        // on an unrelated change to `Seed::derive`", which is exactly what
        // a save-format contract test elsewhere already owns.
        let weights = [3.0, 2.0, 1.0];
        let close = |got: [f64; 3], want: [f64; 3], beta: f64| {
            for i in 0..3 {
                assert!(
                    (got[i] - want[i]).abs() < 0.02,
                    "beta {beta}, shape {:?}: drew {:.4}, weights predict {:.4}",
                    NameShape::ALL[i],
                    got[i],
                    want[i]
                );
            }
        };

        let flat = shape_distribution(weights, 0.4);
        let neutral = shape_distribution(weights, 1.0);
        let sharp = shape_distribution(weights, 2.5);
        close(flat, expected_shares(weights, 0.4), 0.4);
        close(neutral, expected_shares(weights, 1.0), 1.0);
        close(sharp, expected_shares(weights, 2.5), 2.5);

        // β is monotone in exactly the direction its doc comment claims:
        // above 1 the heaviest weight takes more and the lightest takes
        // less; below 1 both move back toward uniform. Asserted on the
        // DRAWN shares, so a `powf` that is computed and then dropped fails
        // here even though the `close` checks above would still pass at
        // β = 1.
        assert!(
            flat[0] < neutral[0] && neutral[0] < sharp[0],
            "the heaviest shape's share must rise with beta: {:.3} / {:.3} / {:.3}",
            flat[0],
            neutral[0],
            sharp[0]
        );
        assert!(
            flat[2] > neutral[2] && neutral[2] > sharp[2],
            "the lightest shape's share must fall with beta: {:.3} / {:.3} / {:.3}",
            flat[2],
            neutral[2],
            sharp[2]
        );
    }

    #[test]
    fn a_degenerate_shape_profile_falls_back_to_the_unclampable_shape() {
        // No profile the composition root derives is degenerate (both
        // workhorse weights are strictly positive over the whole
        // `in_group_radius` domain), but a hand-built one can be, and the
        // answer must be a shape that no candidate pool can clamp.
        //
        // It must also be honest about the STREAM, which is the half of
        // this that touches the save-format contract:
        // `Stream::weighted_index` tests its total and returns `None`
        // BEFORE it draws, so the degenerate path consumes nothing while
        // the ordinary path consumes exactly one `next_f64`. Measured by
        // reading the next value off each stream and comparing it against
        // a fresh stream's first value.
        let degenerate = MorphOptions {
            honorifics: false,
            shape_weights: [0.0, -1.0, 0.0],
            shape_beta: 2.0,
        };
        let control = MorphOptions {
            shape_weights: [3.0, 2.0, 1.0],
            ..degenerate
        };
        let first = Seed(7).stream().next_f64();

        let mut stream = Seed(7).stream();
        assert_eq!(
            Namer::draw_shape(&mut stream, &degenerate),
            NameShape::Simplex,
            "a profile with no positive weight must fall back to the one shape no candidate \
             pool can clamp"
        );
        assert_eq!(
            stream.next_f64(),
            first,
            "the degenerate path must consume NOTHING — weighted_index rejects the total \
             before drawing"
        );

        let mut stream = Seed(7).stream();
        let _ = Namer::draw_shape(&mut stream, &control);
        assert_ne!(
            stream.next_f64(),
            first,
            "the ordinary path must consume exactly one next_f64, so the stream must have \
             moved off its first value"
        );
    }

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
                tonality: 0.0,
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
                tonality: 0.0,
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
        build_lexicon(
            &Seed(seed),
            "test",
            "test",
            &ph,
            &ph,
            &exposures,
            &[],
            CascadeRegime::SETTLED,
        )
    }

    /// An empty lexicon (no concepts at all) — every site concept
    /// `glossed_name` is offered here is unheld, forcing its fallback
    /// branch.
    fn empty_lexicon(seed: u64) -> Lexicon {
        let ph = wordy_ph();
        build_lexicon(
            &Seed(seed),
            "test",
            "test",
            &ph,
            &ph,
            &BTreeMap::new(),
            &[],
            CascadeRegime::SETTLED,
        )
    }

    #[test]
    fn render_views_marks_tone_on_the_vowel_and_leaves_neutral_bare() {
        use crate::phoneme::{Backness, Height, Place, Tone};
        let t = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        };
        let a = |tone| Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone,
        };
        // Atonal (Neutral) word renders exactly as the pre-tone views: "ta".
        let neutral = render_views(&[t, a(Tone::Neutral)]);
        assert_eq!(neutral.roman, "Ta");
        assert_eq!(neutral.ipa, "ta");
        // A High-toned nucleus gains an acute (roman) and ˥ (IPA); a Low one a
        // grave and ˩. The consonant is untouched.
        let high = render_views(&[t, a(Tone::High)]);
        assert_eq!(high.roman, "Ta\u{0301}");
        assert_eq!(high.ipa, "ta˥");
        let low = render_views(&[t, a(Tone::Low)]);
        assert_eq!(low.ipa, "ta˩");
        // Distinct tones make distinct surface strings — tonogenesis's repair
        // is visible, not just structural.
        assert_ne!(high.roman, low.roman);
        assert_ne!(high.roman, neutral.roman);
        // espeak stays tone-blind (the documented audio limit): all three share
        // the same segmental formulation.
        assert_eq!(high.espeak, neutral.espeak);
        assert_eq!(low.espeak, neutral.espeak);
    }

    /// The Wearing (LANG-11, opacification): a morpheme that recurs across
    /// many of a culture's names wears down; a rare one survives whole. This
    /// is Zipf's law of abbreviation and it is the mechanism behind OE
    /// *hām* → `-ham`, ON *býr* → `-by`.
    ///
    /// The fixture's probe form is drawn from the phonology's OWN inventory
    /// through the namer's own syllable machinery — never hand-constructed
    /// `Segment` variants, which would prove the wear only against a form
    /// the phonology could not have produced.
    #[test]
    fn frequent_morphemes_wear_and_rare_ones_do_not() {
        // "kobold" at Seed(42) is chosen because its WEAR cascade actually
        // contains length-reducing rules; the precondition below asserts
        // that rather than assuming it, so a reseed fails loudly and
        // diagnosably instead of silently proving nothing (the Task 2
        // lesson: name the precondition the test rests on).
        let ph = wordy_ph();
        let seed = Seed(42);
        let namer = Namer::new(&seed, "kobold", &ph);
        let cascade = crate::etymology::draw_wear_cascade(&seed, "kobold");
        assert!(
            cascade.rules.iter().any(|r| matches!(
                r.kind,
                crate::etymology::RuleKind::ClusterSimplify | crate::etymology::RuleKind::FinalLoss
            )),
            "fixture precondition: this culture's wear cascade must contain a \
             length-reducing rule, got {:?}",
            cascade.rules
        );

        let mut stream = seed.derive(streams::ROOT).stream();
        let stem = segments_of(&namer.draw_syllables(&mut stream, 3, 3, false));
        assert!(!stem.is_empty(), "the probe form must have segments");

        let worn = namer.wear(&stem, 0.95);
        let whole = namer.wear(&stem, 0.02);

        assert!(
            worn.len() < stem.len(),
            "a morpheme in 95% of this culture's names did not wear at all"
        );
        assert_eq!(
            whole, stem,
            "a morpheme in 2% of names wore down; rare forms must survive whole"
        );
    }

    #[test]
    fn glossed_name_is_a_pure_function_of_its_arguments() {
        // ARGUMENT PURITY — one of the two properties the spec's amended
        // pin-isolation clause (ledger #10, 2026-07-28) says this battery
        // must assert, now that world-level scatter-invariance is
        // deliberately given up. The other, determinism under repeated
        // builds, is `hornvale-worldgen`'s
        // `glossed_names_are_stable_across_two_builds`.
        //
        // This test previously called itself `..._is_pure_and_pin_isolated`,
        // which over-claimed in its own name: it never exercised scatter
        // invariance, and since the wear that property is false anyway.
        //
        // Same (seed, species, kind, salt, site, lexicon, corpus) must yield
        // the identical name and gloss — two freshly built Namers/lexicons
        // over the same inputs, no shared state between them, and two
        // SEPARATELY CONSTRUCTED corpora of equal value, so the name is
        // proved to depend on the corpus's contents rather than on its
        // identity or on any state it might carry.
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = morph(false);
        let n1 = Namer::new(&Seed(9), "test", &ph);
        let n2 = Namer::new(&Seed(9), "test", &ph);
        let a = n1.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus::none(),
        );
        let b = n2.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus::none(),
        );
        assert_eq!(a, b);

        // The corpus argument obeys the same rule: equal VALUE, equal name.
        let mut f1: BTreeMap<String, f64> = BTreeMap::new();
        f1.insert("water".to_string(), 0.8);
        f1.insert("fire".to_string(), 0.1);
        let f2 = f1.clone();
        let c = n1.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus { frequencies: &f1 },
        );
        let d = n2.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus { frequencies: &f2 },
        );
        assert_eq!(c, d, "the name must depend on the corpus's VALUE alone");
    }

    #[test]
    fn glossed_name_gloss_concepts_are_a_subset_of_site_concepts() {
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = morph(false);
        let namer = Namer::new(&Seed(9), "test", &ph);
        for salt in 0..40u64 {
            let (_, gloss) = namer.glossed_name(
                NameKind::Settlement,
                salt,
                &morph,
                &site,
                &lex,
                &NameCorpus::none(),
            );
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
        let morph = morph(false);
        let namer = Namer::new(&Seed(9), "test", &ph);
        let (name, gloss) = namer.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus::none(),
        );
        assert!(
            gloss.is_empty(),
            "no true story to tell: gloss must be empty"
        );
        assert!(!name.roman.is_empty());
    }

    #[test]
    fn glossed_name_moved_the_epoch_off_v1() {
        // The fallback path draws a bare stem too, but under the /v3 leg —
        // a different stream from v1's `name()`, so the two must diverge
        // even for the same seed/species/kind/salt.
        let ph = wordy_ph();
        let lex = empty_lexicon(9);
        let site = SiteConcepts {
            concepts: &["nonexistent"],
        };
        let morph = morph(false);
        let namer = Namer::new(&Seed(9), "test", &ph);
        let v1 = namer.name(NameKind::Settlement, 3, &morph);
        let (v3, _) = namer.glossed_name(
            NameKind::Settlement,
            3,
            &morph,
            &site,
            &lex,
            &NameCorpus::none(),
        );
        assert_ne!(
            v1.roman, v3.roman,
            "v3 must draw from a distinct stream than v1, even on the fallback path"
        );
    }

    /// A hand-built phonology small enough to reason about repair exactly:
    /// one vowel (a), a voiceless alveolar stop (t), an alveolar nasal (n);
    /// onsets admit exactly one stop, nuclei are single vowels, codas admit
    /// one nasal or nothing. So `tan`, `ta`, and `tanta` are legal; a
    /// doubled stop (`tt`) is an illegal cluster no template can host in
    /// one syllable, and a nasal can never begin a syllable.
    fn toy_repair_ph() -> crate::phonology::Phonology {
        use crate::phoneme::{Backness, Height, Place, Tone};
        let a = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::Neutral,
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
        assert!(
            !conforms(&seam, &ph, &[]),
            "test premise: the seam is illegal"
        );
        let repaired = repair_phonotactics(seam, &ph, &[]);
        assert_eq!(
            repaired,
            vec![t, a, n, t, a, t, a],
            "epenthesis must break the tt cluster into tan.ta.ta"
        );
        assert!(conforms(&repaired, &ph, &[]));
    }

    #[test]
    fn repair_of_a_valid_sequence_is_identity_and_repair_is_idempotent() {
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        // Valid input: repair is the identity.
        let valid = vec![t, a, n, t, a];
        assert!(conforms(&valid, &ph, &[]), "test premise: tanta is legal");
        assert_eq!(repair_phonotactics(valid.clone(), &ph, &[]), valid);
        // Idempotence on an invalid input: repairing a repaired sequence
        // changes nothing (repair output always conforms, so the second
        // pass takes the identity branch).
        let seam = vec![t, a, n, t, t, a];
        let once = repair_phonotactics(seam, &ph, &[]);
        let twice = repair_phonotactics(once.clone(), &ph, &[]);
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
        assert!(
            !conforms(&seam, &ph, &[]),
            "test premise: the seam is illegal"
        );
        let repaired = repair_phonotactics(seam, &ph, &[]);
        assert_eq!(
            repaired,
            vec![t, a, n],
            "deletion (second resort) must drop the unhostable nasal and its \
             stranded vowel, keeping the legal prefix"
        );
        assert!(conforms(&repaired, &ph, &[]));
    }

    #[test]
    fn an_attested_word_conforms_verbatim_even_where_canon_rejects_it() {
        // "nat" is canon-illegal in the toy phonology (a nasal can never
        // begin a syllable) but attested — the tier admits it whole.
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let word = vec![n, a, t];
        assert!(
            !conforms(&word, &ph, &[]),
            "test premise: canon rejects nat"
        );
        let attested = vec![word.clone()];
        assert!(conforms(&word, &ph, &attested));
        // Repair of attested material is the identity.
        assert_eq!(repair_phonotactics(word.clone(), &ph, &attested), word);
    }

    #[test]
    fn a_compound_of_attested_words_and_canon_syllables_conforms() {
        // attested "nat" + canon "ta" + attested "nat": parses as
        // [attested][canon syllable][attested] with zero edits.
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let word = vec![n, a, t];
        let attested = vec![word.clone()];
        let compound = vec![n, a, t, t, a, n, a, t];
        assert!(conforms(&compound, &ph, &attested));
        assert_eq!(
            repair_phonotactics(compound.clone(), &ph, &attested),
            compound
        );
    }

    #[test]
    fn foreign_material_still_repairs_exactly_as_before() {
        // The Task-1 regression guard: with an attested tier PRESENT but not
        // matching, the old epenthesis behavior is unchanged (the tan.ta.ta
        // case from repair_breaks_an_illegal_seam_cluster_by_epenthesis).
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let attested = vec![vec![n, a, t]]; // does not occur in the seam below
        let seam = vec![t, a, n, t, t, a];
        assert_eq!(
            repair_phonotactics(seam, &ph, &attested),
            vec![t, a, n, t, a, t, a],
        );
    }

    #[test]
    fn an_attested_span_survives_verbatim_inside_a_sequence_needing_repair() {
        // The DP branch proper (not the conforms short-circuit): "nat" is
        // attested but canon-illegal; "nat" + "tta" does NOT conform even
        // with the tier (tt is an illegal cluster), so the DP must run and
        // its plan must keep the attested span verbatim while epenthesis
        // breaks the residue: nat.ta.ta exactly.
        let ph = toy_repair_ph();
        let (a, t, n) = toy_segments();
        let attested = vec![vec![n, a, t]];
        let seam = vec![n, a, t, t, t, a];
        assert!(
            !conforms(&seam, &ph, &attested),
            "test premise: the sequence must not conform even with the tier"
        );
        let repaired = repair_phonotactics(seam, &ph, &attested);
        assert_eq!(
            repaired,
            vec![n, a, t, t, a, t, a],
            "the attested span must survive verbatim and the residue gain one epenthetic vowel"
        );
        assert!(conforms(&repaired, &ph, &attested));
    }

    #[test]
    fn attested_forms_are_roots_only_deduped_longest_first() {
        // two_word_lexicon(9) holds Steeped roots for water and fire and
        // nothing else; attested_forms yields exactly those modern forms,
        // longest first, no Gap/absent concepts.
        let lex = two_word_lexicon(9);
        let forms = attested_forms(&lex);
        assert_eq!(forms.len(), 2, "exactly the two roots");
        assert!(forms[0].len() >= forms[1].len(), "longest first");
        let water = match lex.entry("water") {
            Some(LexEntry::Root { derivation, .. }) => derivation.modern.clone(),
            other => panic!("water must be a root, got {other:?}"),
        };
        assert!(forms.contains(&water));
    }

    #[test]
    fn glossed_names_surface_their_site_words_verbatim() {
        // The Speakable's core invariant: a glossed name CONTAINS each
        // glossed concept's modern form as a contiguous segment run —
        // audible words, not repair residue. Checked at the roman level
        // via the same render path the committed fact uses.
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = morph(false);
        let namer = Namer::new(&Seed(9), "test", &ph);
        for salt in 0..20u64 {
            for kind in [NameKind::Settlement, NameKind::Deity] {
                let (name, gloss) =
                    namer.glossed_name(kind, salt, &morph, &site, &lex, &NameCorpus::none());
                for concept in gloss.split('-').filter(|c| !c.is_empty()) {
                    let word = match lex.entry(concept) {
                        Some(LexEntry::Root { derivation, .. }) => {
                            render_views(&derivation.modern).roman.to_lowercase()
                        }
                        other => panic!("gloss concept {concept} must be a root, got {other:?}"),
                    };
                    assert!(
                        name.roman.to_lowercase().contains(&word),
                        "salt {salt} {kind:?}: name {:?} must audibly contain {concept} = {word:?}",
                        name.roman
                    );
                }
            }
        }
    }

    #[test]
    fn the_drawn_settlement_stem_has_retired() {
        // The Wearing: a settlement name is now its site word(s) and
        // NOTHING else — the per-salt 2-3 syllable drawn stem (the /v2
        // epoch's collision fix) is gone, because decision 0024 ratified
        // that uniqueness is a reference-time property and that no future
        // work fixes collisions by adding entropy. A settlement over a
        // single-concept site therefore renders exactly the repaired site
        // word, just as a deity over the same site always did.
        let ph = wordy_ph();
        let lex = two_word_lexicon(9);
        let site = SiteConcepts {
            concepts: &["water"],
        };
        let morph = morph(false);
        let namer = Namer::new(&Seed(9), "test", &ph);

        let attested = attested_forms(&lex);
        let plain = render_views(&repair_phonotactics(
            concept_segments(&lex, "water"),
            &ph,
            &attested,
        ))
        .roman;

        let (deity, deity_gloss) =
            namer.glossed_name(NameKind::Deity, 3, &morph, &site, &lex, &NameCorpus::none());
        assert_eq!(deity.roman, plain, "a deity name gains no stem element");
        assert_eq!(deity_gloss, "water");

        // Every salt yields the SAME name — the drawn per-salt element is
        // what used to spread them, and it is gone. This is the collision
        // rise 0024 sanctions, asserted rather than hidden.
        let mut names = std::collections::BTreeSet::new();
        for salt in 0..12u64 {
            let (name, gloss) = namer.glossed_name(
                NameKind::Settlement,
                salt,
                &morph,
                &site,
                &lex,
                &NameCorpus::none(),
            );
            assert_eq!(gloss, "water", "salt {salt}: the gloss is the site word");
            assert_eq!(
                name.roman, plain,
                "salt {salt}: a settlement name must now be exactly its site word"
            );
            names.insert(name.roman);
        }
        assert_eq!(
            names.len(),
            1,
            "with the stem retired a one-concept site names every settlement alike, got {names:?}"
        );
    }

    #[test]
    fn wear_is_keyed_to_frequency_not_to_the_compound_slot() {
        // Ledger #3's actual content, and the property that distinguishes
        // this implementation from the one it rejects. In a two-morpheme
        // compound, whichever morpheme is FREQUENT wears and the rare one
        // beside it does not — regardless of which slot each occupies. A
        // slot-keyed wear (or a wear applied to the assembled compound,
        // where `FinalLoss` can only ever touch the word's last segment)
        // would grind the same slot both times.
        let ph = wordy_ph();
        // Lexicon seed 19 over `wordy_ph`: both roots are consonant-final,
        // so kobold@42's wear cascade has an environment to fire in for
        // each, AND both worn forms survive repair (asserted below as a
        // precondition — the survival rule would otherwise silently give
        // the wear back and this test would be measuring the fallback).
        // `ClusterSimplify` only fires on a word-initial CC and `FinalLoss`
        // only on a word-final consonant, so a lexicon of open CV roots
        // would make this test vacuously green.
        let lex = two_word_lexicon(19);
        // "kobold" at Seed(42): a wear cascade with real length-reducing
        // rules, asserted as a precondition so a reseed fails loudly.
        let namer = Namer::new(&Seed(42), "kobold", &ph);
        let cascade = crate::etymology::draw_wear_cascade(&Seed(42), "kobold");
        assert!(
            cascade.rules.iter().any(|r| matches!(
                r.kind,
                crate::etymology::RuleKind::ClusterSimplify | crate::etymology::RuleKind::FinalLoss
            )),
            "fixture precondition: the wear cascade must reduce length, got {:?}",
            cascade.rules
        );

        let chosen = ["water", "fire"];
        let attested = attested_forms(&lex);
        let (bare, bare_gave_up) =
            namer.worn_compound(&lex, &chosen, &NameCorpus::none(), &attested);
        assert_eq!(bare_gave_up, 0, "an unworn compound surrenders nothing");

        // Non-vacuity: the untouched compound is the two words' segments,
        // and each word on its own really does wear when frequent — so a
        // "nothing changed" result below could not be a no-op wear.
        for concept in chosen {
            let raw = concept_segments(&lex, concept);
            assert!(
                namer.wear(&raw, 0.95).len() < raw.len(),
                "{concept} must be wearable at all for this test to mean anything"
            );
        }

        let mut only_first: BTreeMap<String, f64> = BTreeMap::new();
        only_first.insert("water".to_string(), 0.95);
        only_first.insert("fire".to_string(), 0.02);
        let mut only_second: BTreeMap<String, f64> = BTreeMap::new();
        only_second.insert("water".to_string(), 0.02);
        only_second.insert("fire".to_string(), 0.95);

        let (first_worn, first_gave_up) = namer.worn_compound(
            &lex,
            &chosen,
            &NameCorpus {
                frequencies: &only_first,
            },
            &attested,
        );
        let (second_worn, second_gave_up) = namer.worn_compound(
            &lex,
            &chosen,
            &NameCorpus {
                frequencies: &only_second,
            },
            &attested,
        );
        assert_eq!(
            (first_gave_up, second_gave_up),
            (0, 0),
            "fixture precondition: both worn forms must SURVIVE repair here, or the \
             asymmetry below would be measuring the fallback rather than the wear"
        );

        assert!(
            first_worn.len() < bare.len(),
            "a ubiquitous FIRST morpheme must wear"
        );
        assert!(
            second_worn.len() < bare.len(),
            "a ubiquitous SECOND morpheme must wear"
        );
        assert_ne!(
            first_worn, second_worn,
            "wearing the frequent morpheme must depend on WHICH one is frequent — \
             identical results here mean the wear is keyed to a slot, not to frequency"
        );
    }

    #[test]
    fn wear_that_repair_would_annihilate_is_given_up() {
        // Critical: wear breaks attestedness, and `repair_phonotactics` is
        // the identity only for a culture's ATTESTED words — a worn form
        // falls through to the syllabifier and, when no template can host
        // it, to `RepairStep::Delete`. On seed 42 that erased whole
        // morphemes and left nine settlements committing a `name-gloss`
        // naming a word their name did not contain.
        //
        // Lexicon seed 49 is exactly that case: kobold@42's wear cascade
        // fires on both roots, and neither worn form survives repair. The
        // survival rule must therefore give the wear back rather than let
        // the morpheme vanish.
        let ph = wordy_ph();
        let lex = two_word_lexicon(49);
        let namer = Namer::new(&Seed(42), "kobold", &ph);
        let attested = attested_forms(&lex);
        let chosen = ["water", "fire"];

        // Precondition: this fixture really does wear, or the test proves
        // nothing about the fallback.
        for concept in chosen {
            let raw = concept_segments(&lex, concept);
            assert!(
                namer.wear(&raw, 0.95).len() < raw.len(),
                "fixture precondition: {concept} must wear at all"
            );
        }

        let mut saturated: BTreeMap<String, f64> = BTreeMap::new();
        saturated.insert("water".to_string(), 0.95);
        saturated.insert("fire".to_string(), 0.95);
        let (segments, surrendered) = namer.worn_compound(
            &lex,
            &chosen,
            &NameCorpus {
                frequencies: &saturated,
            },
            &attested,
        );

        assert!(
            surrendered > 0,
            "fixture precondition: repair must annihilate a worn form here, \
             or the fallback is never exercised"
        );
        // The invariant the fallback exists for: every glossed morpheme
        // still says itself, in one contiguous piece, in the surface form.
        for concept in chosen {
            assert!(
                contains_run(&segments, &concept_segments(&lex, concept)),
                "{concept} was annihilated: {:?} does not contain it",
                render_views(&segments).roman
            );
        }
    }

    #[test]
    fn glossed_concepts_is_exactly_what_glossed_name_glosses_and_ignores_the_corpus() {
        // The contract worldgen's two-pass composition rests on: the
        // concepts reported before the corpus exists are the concepts the
        // name is actually built from, because wear consumes nothing from
        // the name stream.
        let ph = wordy_ph();
        let lex = two_word_lexicon(19);
        let site = SiteConcepts {
            concepts: &["water", "fire"],
        };
        let morph = morph(false);
        let namer = Namer::new(&Seed(42), "kobold", &ph);
        let mut saturated: BTreeMap<String, f64> = BTreeMap::new();
        saturated.insert("water".to_string(), 1.0);
        saturated.insert("fire".to_string(), 1.0);
        let corpus = NameCorpus {
            frequencies: &saturated,
        };

        let mut any_worn = false;
        for salt in 0..40u64 {
            for kind in [NameKind::Settlement, NameKind::Deity] {
                let reported = namer.glossed_concepts(kind, salt, &morph, &site, &lex);
                let (worn, gloss) = namer.glossed_name(kind, salt, &morph, &site, &lex, &corpus);
                assert_eq!(
                    reported.join("-"),
                    gloss,
                    "salt {salt} {kind:?}: glossed_concepts must report exactly what \
                     glossed_name glosses, under any corpus"
                );
                let (unworn, _) =
                    namer.glossed_name(kind, salt, &morph, &site, &lex, &NameCorpus::none());
                any_worn |= worn.roman != unworn.roman;
            }
        }
        assert!(
            any_worn,
            "non-vacuity: the saturated corpus must actually have changed some name, \
             or the agreement above proves nothing about wear"
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
                // The UNREPAIRED join — this test supplies its own
                // (empty) attested tier below, so it must not go through
                // `worn_compound`, which repairs against the real one.
                let raw = Namer::join_parts(
                    &lex,
                    chosen.iter().map(|c| concept_segments(&lex, c)).collect(),
                );
                assert!(!raw.is_empty(), "roots must produce segments");
                let repaired = repair_phonotactics(raw, &ph, &[]);
                assert!(
                    conforms(&repaired, &ph, &[]),
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
            &morph(true),
            &site,
            &lex,
            &NameCorpus::none(),
        );
        let without = namer.glossed_name(
            NameKind::Epithet,
            5,
            &morph(false),
            &site,
            &lex,
            &NameCorpus::none(),
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
        let a = n1.name(NameKind::Settlement, 10, &morph(false));
        let b = n2.name(NameKind::Settlement, 10, &morph(false));
        assert_eq!(a, b);
        assert!(!a.roman.is_empty() && !a.ipa.is_empty() && !a.espeak.is_empty());
    }

    #[test]
    fn a_name_is_a_pure_function_of_seed_species_kind_and_salt() {
        // No re-draw, no shared "used" set: the same (seed, species, kind,
        // salt) always yields the same name, and distinct salts draw
        // independently. This covers `Namer::name`'s V1 names ONLY, and for
        // those the strong property holds — a v1 name never depends on
        // which other settlements a world places (spec §8). It says nothing
        // about `glossed_name`, whose corpus argument does depend on a
        // species' whole scatter; see the module docs.
        let ph = kobold_ph();
        let namer = Namer::new(&Seed(2), "kobold", &ph);
        let mut first: Vec<String> = Vec::new();
        for salt in 0..50u64 {
            let g = namer.name(NameKind::Settlement, salt, &morph(false));
            assert!(!g.roman.is_empty());
            first.push(g.roman);
        }
        // A second pass over the same salts reproduces every name exactly.
        let namer2 = Namer::new(&Seed(2), "kobold", &ph);
        for (salt, expected) in first.iter().enumerate() {
            let g = namer2.name(NameKind::Settlement, salt as u64, &morph(false));
            assert_eq!(&g.roman, expected, "salt {salt} must redraw identically");
        }
    }

    #[test]
    fn honorific_morphology_appears_only_when_requested() {
        let ph = kobold_ph();
        let namer = Namer::new(&Seed(3), "kobold", &ph);
        // Epithets with honorifics enabled must be able to differ from those without.
        let with = namer.name(NameKind::Epithet, 5, &morph(true));
        let namer2 = Namer::new(&Seed(3), "kobold", &ph);
        let without = namer2.name(NameKind::Epithet, 5, &morph(false));
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
            let g = namer.name(kind, salt, &morph(honorifics));
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
        let name = namer.name(NameKind::Settlement, 0, &morph(false));
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

    /// An envelope swept from seed bits so the 64-seed battery crosses the
    /// full phonotactic regime space — including the cluster-heavy draws
    /// that caused the collapse (spec §6).
    fn swept_envelope(seed: u64) -> Envelope {
        let f = |k: u64| ((seed >> k) & 3) as f64 / 3.0;
        Envelope {
            labiality: f(0),
            vowel_space: (f(2)).max(0.2),
            voicing: f(4),
            // Bits 6/8 are always zero for seed < 64, so the last two
            // dims reuse overlapping windows — correlated with the
            // others, but genuinely varying (a coverage sweep needs
            // variation, not independence).
            sibilance: f(1),
            voice_loudness: f(3),
            tonality: 0.0,
            exotic: ExoticSeg::None,
        }
    }

    #[test]
    fn attested_compounds_repair_to_identity_across_the_seed_sweep() {
        // Spec §6: for 64 seeds, with the lexicon descending from a DIFFERENT
        // (permissive) proto phonology than the daughter's own drawn one —
        // the exact mismatch that caused the collapse — every root conforms
        // under its own (phonology, attested) pair and every 1-2-concept
        // compound repairs to itself.
        for seed in 0..64u64 {
            let proto = wordy_ph();
            let ph = draw_phonology(&Seed(seed), "swept", &swept_envelope(seed));
            let mut exposures = BTreeMap::new();
            for c in ["water", "fire", "moon", "shadow"] {
                exposures.insert(c.to_string(), ExposureClass::Steeped);
            }
            // ph is the daughter's own drawn phonology (evolution target),
            // proto is the DIFFERENT permissive family-level proto phonology
            // (the draw source) — see build_lexicon's doc comment on
            // (ph, proto_ph) at lexicon.rs:237, and NOT the fixture's usual
            // ph == proto_ph collapse.
            let lex = build_lexicon(
                &Seed(seed),
                "fam",
                "swept",
                &ph,
                &proto,
                &exposures,
                &[],
                CascadeRegime::SETTLED,
            );
            let attested = attested_forms(&lex);
            for chosen in [
                vec!["water"],
                vec!["moon"],
                vec!["water", "fire"],
                vec!["shadow", "moon"],
            ] {
                if !chosen.iter().all(|c| holds_word(&lex, c)) {
                    continue; // exposure may still gap a concept; skip, don't fake
                }
                // The unworn compound: repair is the identity for a
                // culture's own attested words, and wear is what may
                // break that (a worn form is no longer attested).
                // The UNREPAIRED join — this test supplies its own
                // (empty) attested tier below, so it must not go through
                // `worn_compound`, which repairs against the real one.
                let raw = Namer::join_parts(
                    &lex,
                    chosen.iter().map(|c| concept_segments(&lex, c)).collect(),
                );
                assert!(
                    conforms(&raw, &ph, &attested),
                    "seed {seed}: compound {chosen:?} must conform under its own attested tier"
                );
                assert_eq!(
                    repair_phonotactics(raw.clone(), &ph, &attested),
                    raw,
                    "seed {seed}: repair of native compound {chosen:?} must be the identity"
                );
            }
        }
    }
}
