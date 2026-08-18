//! The phonology engine: a per-species phoneme inventory and syllable
//! phonotactics drawn under the species' articulation envelope. `Envelope`
//! is language's OWN copy of the articulation dimensions — populated later
//! by the composition root from the species `ArticulationVector`; this
//! domain never imports `hornvale-species`. `draw_phonology` never
//! constructs a `Segment` outside [`crate::phoneme::canonical_segments`]:
//! `romanize`/`ipa` are exhaustive only over that curated set, so an
//! off-menu feature combination would surface as `"?"` in every later name.

use crate::phoneme::{
    Manner, Place, Segment, Tone, canonical_segments, sonority, sonority_of_manner,
};
use crate::streams;
use crate::typology::{CodaLaw, Harmony, OnsetLaw, Typology};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, Stream};

/// An exotic manner of articulation a species may or may not be capable of.
/// `permits` only admits a `Trill`/`Click`/`Ejective` segment when it
/// matches the envelope's own capability.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExoticSeg {
    /// The species has no exotic manner beyond the common set.
    None,
    /// The species can trill (a rapid tap sequence, e.g. an alveolar r).
    Trill,
    /// The species can click (a non-pulmonic ingressive stop).
    Click,
    /// The species can produce ejectives (non-pulmonic glottalic stops).
    Ejective,
}

/// The articulation envelope as language consumes it: a species' capacity
/// to produce classes of sound, on a 0–1 scale per dimension. This is
/// language's own copy of the dimensions — populated by the composition
/// root from the species `ArticulationVector`, never read from
/// `hornvale-species` directly (language is kernel-only).
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Envelope {
    /// Capacity to form labial (lip) sounds. Below [`LABIALITY_THRESHOLD`],
    /// every labial segment is forbidden.
    pub labiality: f64,
    /// How much of the vowel space the species' vocal tract spans. Scales
    /// the band of canonical vowels `permits` admits, centered on the most
    /// common vowel (`a`).
    pub vowel_space: f64,
    /// Capacity to voice consonants. Above [`VOICING_THRESHOLD`], voiced
    /// segments are permitted; at or below it every voiced segment is
    /// forbidden.
    pub voicing: f64,
    /// Sibilance shifts the keep-probability of sibilant consonants
    /// upward during the inventory draw; it never gates `permits` itself.
    pub sibilance: f64,
    /// How loud/resonant the species' voice is. Low `voice_loudness`
    /// down-weights high-sonority consonants (trills, approximants) during
    /// the inventory draw — "can trill, but the names hiss" — without ever
    /// forbidding them outright.
    pub voice_loudness: f64,
    /// Tonal propensity: atonal 0 (humanoid default) ↔ fully tonal 1. Maps to
    /// the drawn tone-inventory size in [`draw_phonology`] (1 = atonal,
    /// Neutral-only; 2–3 tone-capable), which admits toned vowels into the
    /// inventory and makes [`crate::etymology::RuleKind::Tonogenesis`]
    /// effective.
    pub tonality: f64,
    /// The one exotic manner (if any) this species' vocal tract can
    /// produce.
    pub exotic: ExoticSeg,
}

/// A per-species phonology: a drawn phoneme inventory and syllable
/// phonotactics as manner-slot templates. A template is filled from the
/// inventory at name time (a later task), not here — this struct stores
/// only the shape.
/// type-audit: bare-ok(count: nuclei)
#[derive(Clone, Debug, PartialEq)]
pub struct Phonology {
    /// The drawn segment inventory, a subset of
    /// [`crate::phoneme::canonical_segments`] permitted by the envelope.
    pub inventory: Vec<Segment>,
    /// Onset templates: each is a sequence of manner slots a syllable-
    /// initial cluster may fill, in order.
    pub onsets: Vec<Vec<Manner>>,
    /// Nucleus templates: the **set** of nucleus sizes a syllable may
    /// take, ascending and duplicate-free (1 = a simple vowel, 2 = a
    /// diphthong). A syllable picks one per syllable, so a language that
    /// admits diphthongs *permits* them rather than *requires* them.
    ///
    /// **Ascending order is load-bearing, and this field is `pub`.**
    /// `repair_phonotactics` visits nucleus sizes in slice order under a
    /// strict cost comparison, so an ascending set is what makes a tie
    /// settle toward the SHORTER nucleus. [`draw_phonotactics`] guarantees
    /// it and the drawn-phonology tests assert it; a hand-built
    /// `Phonology` (test fixtures, and any future caller) is on the honour
    /// system — an unsorted set stays correct, but silently loses the
    /// shortening tie-break.
    ///
    /// [`draw_phonotactics`] guarantees `1 ∈ nuclei`: the simple vowel is
    /// universal, and no natural language puts an obligatory diphthong in
    /// every syllable. This field held a single obligatory `usize` count
    /// until The Wearing (2026-07-27), which is what made names read
    /// `Qvooshtvoagootao`; the shape now matches `onsets` and `codas` —
    /// a set of templates, picked from at name time.
    pub nuclei: Vec<usize>,
    /// Coda templates: each is a sequence of manner slots a syllable-final
    /// cluster may fill, in order (an empty template is an open syllable).
    pub codas: Vec<Vec<Manner>>,
    /// Whether vowels within a word must agree, and on what — copied from
    /// the [`Typology`] this phonology was drawn under, so a later name-time
    /// call (`naming::build_name`) has something to read `Harmony` from
    /// without threading `Typology` itself down that call path. Not a save-
    /// format field: `Phonology` carries no `serde` derive and is re-derived
    /// from the seed on every load, never persisted.
    pub harmony: Harmony,
}

/// Below this labiality, every labial segment is forbidden outright.
const LABIALITY_THRESHOLD: f64 = 0.3;

/// Above this voicing, voiced segments are permitted; at or below it every
/// voiced segment is forbidden.
const VOICING_THRESHOLD: f64 = 0.2;

/// The base keep-probability an envelope-permitted consonant starts from
/// before the loudness penalty and sibilance bonus are applied.
const BASE_KEEP: f64 = 0.7;

/// How strongly low `voice_loudness` down-weights high-sonority consonants:
/// keep-probability falls by `LOUDNESS_PENALTY * sonority * (1 -
/// voice_loudness)`.
const LOUDNESS_PENALTY: f64 = 0.22;

/// How strongly `sibilance` raises the keep-probability of sibilant
/// consonants specifically.
const SIBILANCE_BONUS: f64 = 0.3;

/// Keep-probability is clamped to this floor so no envelope-permitted
/// segment is unreachable, and to a ceiling below 1 so the draw stays a
/// draw.
const KEEP_PROBABILITY_RANGE: (f64, f64) = (0.05, 0.98);

/// The minimum number of consonants a drawn inventory always retains, so
/// names stay constructible even under an unlucky draw. Padding is filled
/// from non-exotic, non-approximant manners only (stop/fricative/sibilant/
/// nasal) — it must never hand a species a trill, click, or ejective it
/// didn't independently draw, or the loudness bias in `draw_phonology`
/// would no longer hold by construction.
const MIN_CONSONANTS: usize = 2;

/// The maximum tone-inventory size the phonology epoch draws: `Neutral` plus
/// the two contrastive level tones `High` and `Low`. `Tone::Mid` is banked (no
/// rule writes it), so it is never drawn into an inventory.
const MAX_TONE_COUNT: usize = 3;

/// The contrastive level tones a tone-capable species may draw, in canonical
/// order. `Neutral` is always present and is not among these.
const CONTRASTIVE_TONES: [Tone; 2] = [Tone::High, Tone::Low];

/// Map a species' `tonality` (0 atonal … 1 fully tonal) to its tone-inventory
/// size: 1 (Neutral only, atonal) … [`MAX_TONE_COUNT`].
fn tone_count(tonality: f64) -> usize {
    (1 + (tonality.clamp(0.0, 1.0) * (MAX_TONE_COUNT - 1) as f64).round() as usize)
        .clamp(1, MAX_TONE_COUNT)
}

/// Draw a species' tone inventory from its `tonality`, over the phonology
/// seed's own `"tones"` leg — isolated from the `"inventory"` and
/// `"phonotactics"` streams, so introducing it never perturbs them (an atonal
/// species, count 1, draws nothing new and stays byte-identical). `Neutral` is
/// always present (the untoned default); a count of 2 draws which single
/// contrastive tone joins it, a count of 3 takes both.
fn draw_tone_inventory(phonology_seed: &Seed, tonality: f64) -> Vec<Tone> {
    let mut tones = vec![Tone::Neutral];
    match tone_count(tonality) {
        1 => {}
        n if n >= MAX_TONE_COUNT => tones.extend(CONTRASTIVE_TONES),
        _ => {
            let mut s = phonology_seed.derive(streams::TONES).stream();
            if let Some(t) = s.pick(&CONTRASTIVE_TONES) {
                tones.push(*t);
            }
        }
    }
    tones
}

/// The distinct tones present among a phonology's vowels, in canonical (`Tone`
/// `Ord`) order — the realized tone inventory, for the Lab's `tone-count`
/// metric and the capacity floor. Always includes `Tone::Neutral`; a
/// tone-capable species adds `High` and/or `Low`.
/// type-audit: bare-ok(count)
pub fn tone_inventory(ph: &Phonology) -> Vec<Tone> {
    let mut tones: Vec<Tone> = ph
        .inventory
        .iter()
        .filter_map(|s| match s {
            Segment::Vowel { tone, .. } => Some(*tone),
            Segment::Consonant { .. } => None,
        })
        .collect();
    tones.sort();
    tones.dedup();
    tones
}

/// The number of inventory consonants of a given `manner`.
fn consonants_of_manner(inventory: &[Segment], manner: Manner) -> u64 {
    inventory
        .iter()
        .filter(|s| matches!(s, Segment::Consonant { manner: m, .. } if *m == manner))
        .count() as u64
}

/// The number of distinct fillings a set of onset/coda templates admits from
/// `inventory`: summed over templates, the product over each template's manner
/// slots of the count of inventory consonants of that manner (an empty
/// template — an open onset/coda — contributes one filling). Clamped to at
/// least 1 so capacity is never zeroed by a degenerate template set.
fn template_choices(templates: &[Vec<Manner>], inventory: &[Segment]) -> u64 {
    templates
        .iter()
        .map(|t| {
            t.iter()
                .map(|m| consonants_of_manner(inventory, *m))
                .product::<u64>()
        })
        .sum::<u64>()
        .max(1)
}

/// The **distinguishable-syllable capacity** of a phonology (spec §2.3): a
/// lower bound on the number of distinct syllables it can form, `onset
/// fillings × nucleus fillings × coda fillings`, where the nucleus factor
/// folds tone in (its vowel count already spans every drawn tone). This is
/// the channel capacity the floor guarantees a minimum of, reachable via
/// segments OR tone — the number the Lab's `distinguishable-capacity` metric
/// reports.
/// type-audit: bare-ok(count)
pub fn distinguishable_capacity(ph: &Phonology) -> u64 {
    let onset = template_choices(&ph.onsets, &ph.inventory);
    let coda = template_choices(&ph.codas, &ph.inventory);
    let vowels = ph
        .inventory
        .iter()
        .filter(|s| matches!(s, Segment::Vowel { .. }))
        .count() as u64;
    // One nucleus filling per admissible size, summed — the same "sum over
    // templates of the product over slots" shape `template_choices` uses for
    // onsets and codas. `nuclei` is duplicate-free, so no filling is counted
    // twice.
    //
    // Second-order consequence on decision 0035's axis, recorded rather than
    // left silent. Compare a language against its own pre-Wearing self at the
    // same nucleus SIZES: an obligatory-2 phonology scored `v^2`, its
    // `[1, 2]` counterpart scores `v + v^2`; an obligatory-1 phonology and a
    // `[1]` one both score `v`. So capacity is monotonically ≥ what the old
    // formula gave, never lower, and `ensure_capacity_floor` therefore widens
    // a tone-capable species' tone inventory slightly less often. That is
    // correct — the language really can form those simple-nucleus syllables
    // now, and the floor is a claim about syllables it can form — but it does
    // move the tone tier's trigger rate, which is decision 0035's dial.
    let nucleus = ph
        .nuclei
        .iter()
        .map(|&size| vowels.saturating_pow(size as u32))
        .fold(0u64, u64::saturating_add)
        .max(1);
    onset.saturating_mul(nucleus).saturating_mul(coda)
}

/// The distinguishable-syllable capacity floor (spec §2.3, §5): the minimum
/// number of distinct syllables a language should be able to form. A species
/// short of it is raised by widening the **tone** inventory — pitch, not
/// un-characteristic new consonants, so a few-place species "meets the bar
/// with pitch and keeps its character." An atonal species is never widened
/// here: its floor is the already-applied narrow minimum-inventory guarantee
/// ([`MIN_CONSONANTS`]), and its residual low capacity is the accepted
/// realistic tail (spec §10 Q3), measured — not repaired — by the Lab's
/// confusable-vs-free split.
const CAPACITY_FLOOR: u64 = 24;

/// Append, for every Neutral vowel quality already in `inventory`, its
/// `tone`-toned variant (skipping any already present) — the segmental effect
/// of admitting one more tone level.
fn add_toned_vowels(inventory: &mut Vec<Segment>, tone: Tone) {
    let toned: Vec<Segment> = inventory
        .iter()
        .filter_map(|s| match s {
            Segment::Vowel {
                height,
                backness,
                rounded,
                tone: Tone::Neutral,
            } => Some(Segment::Vowel {
                height: *height,
                backness: *backness,
                rounded: *rounded,
                tone,
            }),
            _ => None,
        })
        .collect();
    for v in toned {
        if !inventory.contains(&v) {
            inventory.push(v);
        }
    }
}

/// Raise a tone-capable species to the [`CAPACITY_FLOOR`] by widening its tone
/// inventory — never by adding un-characteristic consonants. While capacity is
/// short and a contrastive tone is still unused, admit that tone's vowels.
/// Atonal species (`env.tonality == 0`) are left untouched, so their draw stays
/// byte-identical (their floor is [`MIN_CONSONANTS`], already applied).
/// Best-effort: a species whose tones are already maxed keeps whatever capacity
/// its segments and pitch afford.
fn ensure_capacity_floor(env: &Envelope, ph: &mut Phonology) {
    if env.tonality <= 0.0 {
        return;
    }
    for &tone in &CONTRASTIVE_TONES {
        if distinguishable_capacity(ph) >= CAPACITY_FLOOR {
            return;
        }
        if !tone_inventory(ph).contains(&tone) {
            add_toned_vowels(&mut ph.inventory, tone);
        }
    }
}

/// The canonical vowel order `permits`/`draw_phonology` reason about: i, e,
/// a, o, u. Index 2 (`a`) is the center every vowel-space band grows from.
fn vowel_order() -> [Segment; 5] {
    use crate::phoneme::Backness::*;
    use crate::phoneme::Height::*;
    [
        Segment::Vowel {
            height: High,
            backness: Front,
            rounded: false,
            tone: Tone::Neutral,
        }, // i
        Segment::Vowel {
            height: Mid,
            backness: Front,
            rounded: false,
            tone: Tone::Neutral,
        }, // e
        Segment::Vowel {
            height: Low,
            backness: Central,
            rounded: false,
            tone: Tone::Neutral,
        }, // a
        Segment::Vowel {
            height: Mid,
            backness: Back,
            rounded: true,
            tone: Tone::Neutral,
        }, // o
        Segment::Vowel {
            height: High,
            backness: Back,
            rounded: true,
            tone: Tone::Neutral,
        }, // u
    ]
}

/// The index in [`vowel_order`] every vowel-space band is centered on.
const VOWEL_CENTER: usize = 2;

/// How many of the 5 canonical vowels a given `vowel_space` admits, always
/// at least 1 so a species can always name things.
fn vowel_band_count(vowel_space: f64) -> usize {
    let n = (vowel_space.clamp(0.0, 1.0) * vowel_order().len() as f64).round() as usize;
    n.clamp(1, vowel_order().len())
}

/// Whether two vowel segments share the same quality (height, backness,
/// rounding), ignoring tone — `permits`/`vowel_permitted` gate the vocal-tract
/// quality, not the suprasegmental pitch, so a toned vowel is permitted
/// exactly when its Neutral quality is.
fn same_vowel_quality(a: &Segment, b: &Segment) -> bool {
    matches!(
        (a, b),
        (
            Segment::Vowel { height: h1, backness: bk1, rounded: r1, .. },
            Segment::Vowel { height: h2, backness: bk2, rounded: r2, .. },
        ) if h1 == h2 && bk1 == bk2 && r1 == r2
    )
}

/// Whether `seg` (assumed a vowel) falls inside the `vowel_space`-sized
/// band centered on `a`. Tone-agnostic: a toned vowel is permitted exactly
/// when its Neutral-quality counterpart is (see [`same_vowel_quality`]).
fn vowel_permitted(vowel_space: f64, seg: &Segment) -> bool {
    let order = vowel_order();
    let Some(idx) = order.iter().position(|v| same_vowel_quality(v, seg)) else {
        // Not one of the 5 canonical vowel qualities: outside the curated set
        // entirely, so it is never permitted (draw_phonology never offers
        // one, but permits() stays correct for any caller).
        return false;
    };
    let count = vowel_band_count(vowel_space);
    let radius_lo = (count - 1) / 2;
    let radius_hi = count - 1 - radius_lo;
    let lo = VOWEL_CENTER.saturating_sub(radius_lo);
    let hi = (VOWEL_CENTER + radius_hi).min(order.len() - 1);
    idx >= lo && idx <= hi
}

/// Which exotic capability a manner requires, or `None` if any vocal tract
/// can produce it.
///
/// **A trill is deliberately not here** (a decision recorded at this
/// campaign's close, The Burr — `docs/decisions/` has no record yet as of
/// this commit). Clicks and ejectives
/// are non-pulmonic and genuinely marked — a species either has the mechanism
/// or does not. An alveolar trill is an ordinary pulmonic consonant present in
/// a large majority of the world's languages, and gating it behind the same
/// capability is what left 17 of 18 shipped tongues with zero liquid-bearing
/// words. `ExoticSeg::Trill` survives as a capability a species may still
/// declare; it simply no longer *gates* the segment.
/// type-audit: bare-ok(flag)
fn exotic_manner(manner: Manner) -> Option<ExoticSeg> {
    match manner {
        Manner::Click => Some(ExoticSeg::Click),
        Manner::Ejective => Some(ExoticSeg::Ejective),
        _ => None,
    }
}

/// The envelope filter: whether `seg` is producible at all by a species
/// with this `Envelope`. Does not weight likelihood — see
/// [`draw_phonology`] for the probabilistic keep step.
/// type-audit: bare-ok(flag)
pub fn permits(env: &Envelope, seg: &Segment) -> bool {
    match seg {
        Segment::Consonant {
            place,
            manner,
            voiced,
        } => {
            if *place == Place::Labial && env.labiality < LABIALITY_THRESHOLD {
                return false;
            }
            if let Some(required) = exotic_manner(*manner)
                && env.exotic != required
            {
                return false;
            }
            if *voiced && env.voicing <= VOICING_THRESHOLD {
                return false;
            }
            true
        }
        Segment::Vowel { .. } => vowel_permitted(env.vowel_space, seg),
    }
}

/// The probability an envelope-permitted consonant is kept during the
/// inventory draw: falls with sonority as `voice_loudness` drops (a quiet
/// species under-represents its most sonorous manners), rises with
/// `sibilance` for sibilants specifically.
fn keep_probability(env: &Envelope, seg: &Segment) -> f64 {
    let son = f64::from(sonority(seg));
    let mut p = BASE_KEEP - LOUDNESS_PENALTY * son * (1.0 - env.voice_loudness);
    if matches!(
        seg,
        Segment::Consonant {
            manner: Manner::Sibilant,
            ..
        }
    ) {
        p += SIBILANCE_BONUS * env.sibilance;
    }
    p.clamp(KEEP_PROBABILITY_RANGE.0, KEEP_PROBABILITY_RANGE.1)
}

/// Whether `manner` is safe padding for the guaranteed minimum consonant
/// set: never an exotic manner (those must only appear because the species
/// independently drew them) and never an approximant (kept probabilistic,
/// like sibilants and trills, to keep the loudness bias meaningful).
fn is_padding_manner(manner: Manner) -> bool {
    matches!(
        manner,
        Manner::Stop | Manner::Fricative | Manner::Sibilant | Manner::Nasal
    )
}

/// Top up `inventory` to [`MIN_CONSONANTS`] consonants, drawing only from
/// `candidates` (already envelope-permitted) and only non-exotic,
/// non-approximant manners, in canonical order, so the addition is
/// deterministic and never hands a species an exotic segment it didn't
/// earn through the probabilistic draw.
fn ensure_minimum_consonants(candidates: &[Segment], inventory: &mut Vec<Segment>) {
    let consonant_count = |inv: &[Segment]| {
        inv.iter()
            .filter(|s| matches!(s, Segment::Consonant { .. }))
            .count()
    };
    if consonant_count(inventory) >= MIN_CONSONANTS {
        return;
    }
    for seg in candidates {
        if consonant_count(inventory) >= MIN_CONSONANTS {
            break;
        }
        if let Segment::Consonant { manner, .. } = seg
            && is_padding_manner(*manner)
            && !inventory.contains(seg)
        {
            inventory.push(*seg);
        }
    }
}

/// The minimum number of sonorant consonants (trill or approximant) an
/// inventory retains when the language's typology asks for one.
const MIN_SONORANTS: usize = 1;

/// Top up `inventory` to [`MIN_SONORANTS`] sonorants, drawing only from
/// `candidates` (already envelope-permitted), in canonical order.
///
/// Deterministic and **draw-free**, exactly like [`ensure_minimum_consonants`]
/// beside it: it consumes no stream, so adding it perturbs no sibling stream
/// and the phonotactics draw downstream is untouched.
///
/// The direction this enforces, stated so it cannot be mistaken for a
/// guarantee it does not make: it puts a **floor** under sonorant count. It
/// does not cap it, does not order the inventory, and says nothing about
/// whether the phonotactic templates will ever *use* the sonorant it adds —
/// that is Task 10's job.
fn ensure_minimum_sonorants(candidates: &[Segment], inventory: &mut Vec<Segment>) {
    let is_sonorant = |s: &Segment| {
        matches!(
            s,
            Segment::Consonant {
                manner: Manner::Trill | Manner::Approximant,
                ..
            }
        )
    };
    let count = |inv: &[Segment]| inv.iter().filter(|s| is_sonorant(s)).count();
    for seg in candidates {
        if count(inventory) >= MIN_SONORANTS {
            return;
        }
        if is_sonorant(seg) && !inventory.contains(seg) {
            inventory.push(*seg);
        }
    }
}

/// The distinct manners present among `inventory`'s consonants, in
/// canonical (`Manner`'s declared) order, for phonotactic template draws.
fn consonant_manners(inventory: &[Segment]) -> Vec<Manner> {
    let mut manners: Vec<Manner> = inventory
        .iter()
        .filter_map(|s| match s {
            Segment::Consonant { manner, .. } => Some(*manner),
            Segment::Vowel { .. } => None,
        })
        .collect();
    manners.sort();
    manners.dedup();
    manners
}

/// Draw a single manner-slot template of length in `[min_len, max_len]`
/// from `manners`. An empty `manners` yields an empty template (an open
/// syllable) rather than panicking.
fn draw_manner_slots(
    stream: &mut Stream,
    manners: &[Manner],
    min_len: usize,
    max_len: usize,
    rising: bool,
) -> Vec<Manner> {
    if manners.is_empty() {
        return Vec::new();
    }
    let len = stream.range_u32(min_len as u32, max_len as u32) as usize;
    let drawn: Vec<Manner> = (0..len)
        .filter_map(|_| stream.pick(manners).copied())
        .collect();
    order_by_sonority(drawn, rising)
}

/// Impose the **Sonority Sequencing Principle** on a drawn manner sequence:
/// sonority rises toward the nucleus in an onset and falls away from it in a
/// coda, and no two adjacent slots sit at the same height.
///
/// Without this the templates were drawn independently, so `[Nasal, Nasal]`
/// was a legal onset and produced names opening `ngng-`; `[Nasal, Stop]` gave
/// the reverse-sonority clusters no language uses. Sorting rather than
/// rejecting keeps the draw count identical, so the constraint costs no extra
/// entropy — it only decides what the same draws mean.
fn order_by_sonority(mut drawn: Vec<Manner>, rising: bool) -> Vec<Manner> {
    drawn.sort_by_key(|m| {
        if rising {
            sonority_of_manner(*m)
        } else {
            4 - sonority_of_manner(*m)
        }
    });
    // Equal-sonority neighbours are the geminate-like clusters (ngng, shsh);
    // the plateau is what makes them unsayable, so collapse it.
    drawn.dedup_by_key(|m| sonority_of_manner(*m));
    drawn
}

/// The largest nucleus a drawn template set may admit: two vowel slots, a
/// diphthong. The tier above (triphthongs, long-vowel-plus-glide) is not
/// modelled. Part of the phonotactics consumption contract — raising it
/// changes what [`draw_phonotactics`] draws for every species.
const MAX_NUCLEUS: usize = 2;

/// Draw the **nucleus template set**: which nucleus sizes a syllable of this
/// language may take, as a sorted duplicate-free set.
///
/// Drawn in the same two-step shape as `onsets` and `codas` — a count, then
/// that many templates — rather than as a third mechanism. The one thing it
/// does that they do not is **push `1` unconditionally**: the simple vowel is
/// universal. This is the whole point of the draw. `nuclei` was a single
/// `usize` count until The Wearing, and when it landed on 2 *every* syllable
/// in that language carried two vowels — an obligatory diphthong no natural
/// language has, and roughly a third of every name's characters.
///
/// Consumes `1 + count` draws (2 or 3), where the old count consumed 1.
/// Reachable sets are therefore exactly `[1]` and `[1, 2]`, at
/// `P([1]) = 3/8`.
fn draw_nuclei(stream: &mut Stream) -> Vec<usize> {
    let count = stream.range_u32(1, 2) as usize;
    let mut nuclei: Vec<usize> = (0..count)
        .map(|_| stream.range_u32(1, MAX_NUCLEUS as u32) as usize)
        .collect();
    nuclei.push(1);
    nuclei.sort_unstable();
    nuclei.dedup();
    nuclei
}

/// Draw the onset/nucleus/coda phonotactic templates from `inventory`'s
/// available consonant manners. Draw order (onsets, then nuclei, then codas)
/// is part of the stream-consumption contract.
///
/// **The Wearing changed what this consumes and deliberately did NOT bump an
/// epoch on the `phonotactics` leg.** The rule an epoch answers is "a
/// *released* consumption contract whose output a saved ledger holds". A
/// [`Phonology`] is never serialized — no world file contains one — so this
/// leg's only ledger-visible outputs are names, lexicon forms, and the two
/// other `draw_syllables` consumers — `grammar::draw_copula_form`'s copula
/// stem and `morphology::draw_morph_proto`'s affix protos, both of which are
/// likewise re-derived from the seed rather than persisted. Every one of
/// those is already reseeded by this same campaign under the unreleased
/// `name/v3` epoch and Task 1's re-founded root cohort. A
/// `phonotactics/v2` would guard nothing those do not already guard, and it
/// would additionally reseed every species' **onsets**, which this change
/// deliberately leaves alone (onset cluster density is a separate question)
/// and which would confound the measurement of the nucleus fix. The onsets
/// of every world main has ever generated therefore survive this change
/// unchanged; the codas, which follow the nucleus draw, do not.
///
/// **That has now flipped, and this paragraph records it rather than leaving
/// the condition in the future tense.** The Wearing merged at `0b65be20`, so a
/// world saved off main carries `language/<species>/name/settlement: v3` in its
/// `derived_under` stamp; `name/v3` is frozen. There is no unreleased epoch
/// left to ride here, and **the next change to what this function consumes owes
/// `phonotactics/v2`** — the reasoning above is the record of why *this*
/// change did not, not a standing licence for the next one. The freeze rule
/// itself is decision 0089 (`docs/decisions/`), which is where a reader should
/// go before riding any epoch.
fn draw_phonotactics(
    stream: &mut Stream,
    inventory: &[Segment],
    typ: &Typology,
) -> (Vec<Vec<Manner>>, Vec<usize>, Vec<Vec<Manner>>) {
    let manners = consonant_manners(inventory);

    let onset_count = stream.range_u32(2, 3) as usize;
    // Every arm draws the same template first (`draw_manner_slots`), so no
    // arm can shift the stream relative to another; only the post-processing
    // differs. `sonorant-open` guarantees the property on template 0 only —
    // every other template is left free to be single-consonant, and is only
    // corrected if the *draw itself* produced a non-sonorant-second cluster.
    let onsets: Vec<Vec<Manner>> = (0..onset_count)
        .map(|i| {
            let drawn = draw_manner_slots(stream, &manners, 1, 2, true);
            match typ.onset_law {
                // Today's behaviour, byte-for-byte.
                OnsetLaw::Drawn => drawn,
                // Template 0 is forced into a sonorant-second cluster
                // unconditionally, which is what guarantees the property
                // *by construction* rather than by draw (spec §3.7). Every
                // other template keeps its drawn length, single-consonant
                // onsets included, and is only rewritten if the draw itself
                // produced a cluster whose second slot is not a sonorant —
                // never widened into a cluster it wasn't already.
                OnsetLaw::SonorantSecond if i == 0 => force_sonorant_second(drawn, &manners),
                OnsetLaw::SonorantSecond => sonorize_onset_cluster(drawn, &manners),
                // One slot only.
                OnsetLaw::Single => drawn.into_iter().take(1).collect(),
            }
        })
        .collect();

    let nuclei = draw_nuclei(stream);

    let coda_count = stream.range_u32(1, 2) as usize;
    let codas: Vec<Vec<Manner>> = (0..coda_count)
        .map(|_| {
            let drawn = draw_manner_slots(stream, &manners, 0, 1, false);
            match typ.coda_law {
                // Today's behaviour, byte-for-byte.
                CodaLaw::Drawn => drawn,
                // A coda is required and obstruent: an empty or non-
                // obstruent draw is back-filled from the inventory's
                // obstruents in canonical order, consuming no extra draw.
                CodaLaw::ObstruentObligatory => force_obstruent_coda(drawn, &manners),
                // Optional, and restricted to the closed sonorant set.
                CodaLaw::SonorantClosed => restrict_to_sonorant(drawn, &manners),
                // Open, or a single nasal.
                CodaLaw::OpenOrNasal => restrict_to_nasal(drawn, &manners),
            }
        })
        .collect();

    (onsets, nuclei, codas)
}

/// Whether `m` is one of the three sonorant manners a coda law treats as a
/// liquid/nasal for phonotactic purposes. Used by the coda helpers below,
/// which are a separate design question from the onset guarantee.
fn is_sonorant_manner(m: &Manner) -> bool {
    matches!(m, Manner::Trill | Manner::Approximant | Manner::Nasal)
}

/// Whether `m` is a **liquid** specifically — `Trill` or `Approximant`, the
/// exact set [`ensure_minimum_sonorants`] floors and spec §3.7 means by "a
/// liquid". Deliberately narrower than [`is_sonorant_manner`]: `Manner`'s
/// canonical order places `Nasal` before `Trill`, and every shipped envelope
/// already carries a nasal (it is never gated), so a "first sonorant in
/// canonical order" search over the wider set finds the nasal every time and
/// never reaches the liquid the floor exists to guarantee. Using the
/// narrower set here is what makes the onset law actually seat the liquid
/// the floor put in the inventory, rather than seating a nasal that was
/// already reachable regardless.
fn is_liquid_manner(m: &Manner) -> bool {
    matches!(m, Manner::Trill | Manner::Approximant)
}

/// A two-slot onset whose second slot is a liquid, unconditionally. Used
/// only for the one onset template `sonorant-open` designates to carry the
/// guarantee (see [`draw_phonotactics`]) — draw-free: the liquid comes from
/// `manners` in canonical order, so no stream is consumed and the control
/// bundle's draw count is unchanged. Falls back to the drawn cluster when
/// the language has no liquid at all (a bundle other than `sonorant-open`,
/// whose floor does not run), rather than handing it a segment it cannot
/// produce.
///
/// The direction this enforces: it guarantees a liquid is *reachable in an
/// onset*. It does not guarantee every onset is a cluster, does not choose
/// which liquid, and says nothing about codas.
fn force_sonorant_second(drawn: Vec<Manner>, manners: &[Manner]) -> Vec<Manner> {
    let Some(&son) = manners.iter().find(|m| is_liquid_manner(m)) else {
        return drawn;
    };
    let head = drawn
        .iter()
        .copied()
        .find(|m| !is_liquid_manner(m))
        .unwrap_or(Manner::Stop);
    vec![head, son]
}

/// Correct an onset template *only if the draw already made it a cluster*
/// and that cluster's second slot is not a liquid. A single-consonant (or
/// open) template is returned unchanged — this is what keeps `sonorant-open`
/// from forcing every onset into a two-consonant cluster (the caricature the
/// campaign controller flagged in the naive sketch), while still holding the
/// law over every cluster the draw does produce. Draw-free, like
/// [`force_sonorant_second`].
fn sonorize_onset_cluster(drawn: Vec<Manner>, manners: &[Manner]) -> Vec<Manner> {
    if drawn.len() <= 1 || is_liquid_manner(&drawn[1]) {
        return drawn;
    }
    force_sonorant_second(drawn, manners)
}

/// A coda that must not be empty and must be an obstruent. Draw-free: an
/// empty or non-obstruent draw is replaced from `manners` in canonical
/// order.
fn force_obstruent_coda(drawn: Vec<Manner>, manners: &[Manner]) -> Vec<Manner> {
    let obstruent = |m: &Manner| matches!(m, Manner::Stop | Manner::Fricative | Manner::Sibilant);
    if !drawn.is_empty() && drawn.iter().all(obstruent) {
        return drawn;
    }
    match manners.iter().find(|m| obstruent(m)) {
        Some(m) => vec![*m],
        // A language with no obstruent at all keeps its drawn coda rather
        // than being handed a segment it cannot produce.
        None => drawn,
    }
}

/// A coda restricted to the closed sonorant set, or empty. Draw-free.
fn restrict_to_sonorant(drawn: Vec<Manner>, manners: &[Manner]) -> Vec<Manner> {
    if drawn.iter().all(is_sonorant_manner) {
        return drawn;
    }
    match manners.iter().find(|m| is_sonorant_manner(m)) {
        Some(m) => vec![*m],
        None => Vec::new(),
    }
}

/// A coda that is open or a single nasal. Draw-free.
fn restrict_to_nasal(drawn: Vec<Manner>, manners: &[Manner]) -> Vec<Manner> {
    if drawn.is_empty() {
        return drawn;
    }
    match manners.iter().find(|m| matches!(m, Manner::Nasal)) {
        Some(m) => vec![*m],
        None => Vec::new(),
    }
}

/// Draw a per-species phonology: a phoneme inventory (a subset of
/// [`crate::phoneme::canonical_segments`] permitted by `env`, with
/// high-sonority consonants down-weighted when `env.voice_loudness` is
/// low) and syllable phonotactic templates. Every draw comes from
/// `seed.derive(streams::ROOT)
/// .derive(StreamLabel::dynamic(species)).derive(streams::PHONOLOGY)`,
/// split into an `"inventory"` sub-stream and a `"phonotactics"` sub-stream
/// so adding a new draw to one never perturbs the other. `typ` names the
/// family's [`Typology`] bundle; every path it reaches today still
/// delegates to the pre-typology draw, unchanged in effect.
/// type-audit: bare-ok(identifier-text: species)
pub fn draw_phonology(seed: &Seed, species: &str, env: &Envelope, typ: &Typology) -> Phonology {
    let phonology_seed = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::PHONOLOGY);

    // The drawn tone inventory (spec §5). Atonal species get `{Neutral}`, so
    // only Neutral vowels are admitted below and the phonology is byte-identical
    // to the pre-tone draw.
    let tones = draw_tone_inventory(&phonology_seed, env.tonality);

    let candidates: Vec<Segment> = canonical_segments()
        .into_iter()
        .filter(|s| permits(env, s))
        .collect();

    let mut inventory_stream = phonology_seed.derive(streams::INVENTORY).stream();
    let mut inventory: Vec<Segment> = Vec::new();
    for seg in &candidates {
        let keep = match seg {
            // A vowel is admitted iff its tone is in the drawn tone inventory.
            // Vowel admission consumes no stream draw ("the vowels the
            // vowel_space allows, at the tones the tonality allows" is not a
            // probabilistic draw), so the interleaved consonant draws below are
            // untouched by the tone dimension.
            Segment::Vowel { tone, .. } => tones.contains(tone),
            Segment::Consonant { .. } => inventory_stream.next_f64() < keep_probability(env, seg),
        };
        if keep {
            inventory.push(*seg);
        }
    }
    ensure_minimum_consonants(&candidates, &mut inventory);
    if typ.requires_sonorant() {
        ensure_minimum_sonorants(&candidates, &mut inventory);
    }

    let mut phonotactics_stream = phonology_seed.derive(streams::PHONOTACTICS).stream();
    let (onsets, nuclei, codas) = draw_phonotactics(&mut phonotactics_stream, &inventory, typ);

    let mut ph = Phonology {
        inventory,
        onsets,
        nuclei,
        codas,
        harmony: typ.harmony,
    };
    // Capacity floor (spec §5): widen a tone-capable species' tone inventory
    // until it clears the floor. A no-op for atonal species (byte-identical
    // draw) and draw-free — it only appends toned vowels the tone tier already
    // authorizes, never a new consonant or a stream draw, so phonotactics
    // (consonant-manner based) stay valid.
    ensure_capacity_floor(env, &mut ph);
    ph
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    /// The manikin's articulation envelope: the reference values these tests
    /// are framed against — five scalars at the neutral midpoint, `tonality`
    /// at the designated default 0.0 (atonal), no exotic manner. Goblin's
    /// authored row happens to equal it, which is why the goblin-labelled
    /// draws below pass it.
    fn manikin_env() -> Envelope {
        Envelope {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        }
    }
    fn kobold_env() -> Envelope {
        Envelope {
            labiality: 0.1,
            vowel_space: 0.3,
            voicing: 0.6,
            sibilance: 0.9,
            voice_loudness: 0.2,
            tonality: 0.0,
            exotic: ExoticSeg::Trill,
        }
    }

    #[test]
    fn envelope_forbids_labials_for_a_low_labiality_species() {
        let bilabial = Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Stop,
            voiced: false,
        };
        assert!(!permits(&kobold_env(), &bilabial));
        assert!(permits(&manikin_env(), &bilabial));
    }

    /// claim: structural(seed: 42) — false-positive seed-loop flag; `s` binds a
    /// Segment, two fixed hand-built scenarios (quiet vs loud)
    #[test]
    fn a_quiet_species_admits_its_trill_rarely_or_not_at_all() {
        // Kobold is Trill-capable but low-loudness: the drawn inventory should
        // contain few/no trills relative to a loud species with the same manner.
        let quiet = draw_phonology(
            &Seed(42),
            "kobold",
            &kobold_env(),
            &crate::typology::concatenative(),
        );
        let trills = quiet
            .inventory
            .iter()
            .filter(|s| {
                matches!(
                    s,
                    Segment::Consonant {
                        manner: Manner::Trill,
                        ..
                    }
                )
            })
            .count();
        let mut loud = kobold_env();
        loud.voice_loudness = 0.9;
        let loud_ph = draw_phonology(
            &Seed(42),
            "kobold",
            &loud,
            &crate::typology::concatenative(),
        );
        let loud_trills = loud_ph
            .inventory
            .iter()
            .filter(|s| {
                matches!(
                    s,
                    Segment::Consonant {
                        manner: Manner::Trill,
                        ..
                    }
                )
            })
            .count();
        assert!(
            trills <= loud_trills,
            "low loudness must not admit MORE trills than high loudness"
        );
    }

    #[test]
    fn draw_is_deterministic() {
        let a = draw_phonology(
            &Seed(7),
            "kobold",
            &kobold_env(),
            &crate::typology::concatenative(),
        );
        let b = draw_phonology(
            &Seed(7),
            "kobold",
            &kobold_env(),
            &crate::typology::concatenative(),
        );
        assert_eq!(a.inventory, b.inventory);
        assert_eq!(a.onsets, b.onsets);
    }

    /// claim: structural(seed: 3) — false-positive seed-loop flag; `s` binds a
    /// Segment, single fixed seed
    #[test]
    fn inventory_respects_the_envelope() {
        let ph = draw_phonology(
            &Seed(3),
            "kobold",
            &kobold_env(),
            &crate::typology::concatenative(),
        );
        assert!(ph.inventory.iter().all(|s| permits(&kobold_env(), s)));
        assert!(!ph.inventory.is_empty());
    }

    /// Carry-forward from Task 3's review: `romanize`/`ipa` return `"?"`
    /// for any `Segment` outside `canonical_segments()`. If `draw_phonology`
    /// ever synthesized a segment instead of filtering the canonical set,
    /// "?" would surface in every later name. Assert the invariant holds
    /// across multiple species/envelopes.
    /// claim: invariant(forall-seed) — tuple pattern `(seed, species, env)`
    /// over [1,42,99] (Fix round 1, Class 1)
    #[test]
    fn drawn_inventory_is_always_a_subset_of_canonical_segments() {
        let canonical = canonical_segments();
        for (seed, species, env) in [
            (Seed(1), "goblin", manikin_env()),
            (Seed(42), "kobold", kobold_env()),
            (Seed(99), "kobold", kobold_env()),
        ] {
            let ph = draw_phonology(&seed, species, &env, &crate::typology::concatenative());
            for seg in &ph.inventory {
                assert!(
                    canonical.contains(seg),
                    "drawn segment {seg:?} is not in canonical_segments(); \
                     romanize/ipa would render it as \"?\""
                );
            }
        }
    }

    // ---- The tone dimension (Stage 4).

    /// A fully tone-capable envelope (the manikin's values elsewhere,
    /// tonality 1.0).
    fn tonal_env() -> Envelope {
        Envelope {
            tonality: 1.0,
            ..manikin_env()
        }
    }

    fn is_toned_vowel(s: &Segment) -> bool {
        matches!(s, Segment::Vowel { tone, .. } if *tone != Tone::Neutral)
    }

    /// claim: invariant(forall-seed) — atonal species admits no toned vowel
    #[test]
    fn an_atonal_species_admits_only_neutral_vowels() {
        // tonality 0.0 ⇒ tone inventory {Neutral} ⇒ no toned vowel is admitted,
        // so the vowel set is exactly the pre-tone (Neutral-only) set.
        for seed in 0..12u64 {
            let ph = draw_phonology(
                &Seed(seed),
                "goblin",
                &manikin_env(),
                &crate::typology::concatenative(),
            );
            assert!(
                !ph.inventory.iter().any(is_toned_vowel),
                "seed {seed}: an atonal species must carry no toned vowel"
            );
            assert_eq!(tone_inventory(&ph), vec![Tone::Neutral]);
        }
    }

    #[test]
    fn a_tone_capable_species_admits_toned_vowels() {
        // tonality 1.0 ⇒ tone inventory {Neutral, High, Low} ⇒ the inventory
        // carries toned vowels, and every vowel quality present appears in each
        // drawn tone.
        let ph = draw_phonology(
            &Seed(1),
            "serpent",
            &tonal_env(),
            &crate::typology::concatenative(),
        );
        assert!(
            ph.inventory.iter().any(is_toned_vowel),
            "a fully tonal species must admit toned vowels"
        );
        assert_eq!(
            tone_inventory(&ph),
            vec![Tone::Neutral, Tone::High, Tone::Low],
            "a fully tonal species realizes Neutral + both contrastive tones"
        );
    }

    #[test]
    fn the_tone_inventory_grows_monotonically_with_tonality() {
        let atonal = tone_count(0.0);
        let mid = tone_count(0.5);
        let full = tone_count(1.0);
        assert_eq!(atonal, 1, "tonality 0 is atonal");
        assert!(atonal < mid && mid < full, "more tonality ⇒ more tones");
        assert_eq!(full, MAX_TONE_COUNT);
    }

    // ---- The distinguishable-capacity floor (Stage 4).

    use crate::phoneme::{Backness, Height};

    fn cons(place: Place, manner: Manner, voiced: bool) -> Segment {
        Segment::Consonant {
            place,
            manner,
            voiced,
        }
    }
    fn vow(height: Height, backness: Backness) -> Segment {
        Segment::Vowel {
            height,
            backness,
            rounded: false,
            tone: Tone::Neutral,
        }
    }

    /// Whether a segment is a sonorant consonant (trill or approximant).
    fn is_sonorant_seg(s: &Segment) -> bool {
        matches!(
            s,
            Segment::Consonant {
                manner: Manner::Trill | Manner::Approximant,
                ..
            }
        )
    }

    /// The quietest envelope the roster carries (elf proto, voice_loudness
    /// 0.35) must still be able to hold a liquid. Before the floor, the
    /// approximant keep-probability there is 0.128, and the region
    /// "quiet AND sonorant-rich" — most of what a Quenya-like tongue is —
    /// was unreachable by construction (spec §3.2).
    ///
    /// **Seed 1, not 42.** Seed 42 draws a sonorant here even before the
    /// floor exists — Task 4's ungating alone happens to suffice at that
    /// seed, which would make this test prove nothing. Seed 1 was found by
    /// scanning `0..64` for a seed that genuinely fails pre-floor (34 of the
    /// 64 do); it is the lowest of them.
    #[test]
    fn a_quiet_envelope_still_draws_a_sonorant() {
        let env = Envelope {
            voice_loudness: 0.35,
            vowel_space: 0.70,
            ..manikin_env()
        };
        let ph = draw_phonology(
            &Seed(1),
            "quiet-probe",
            &env,
            &crate::typology::sonorant_open(),
        );
        assert!(
            ph.inventory.iter().any(is_sonorant_seg),
            "a quiet envelope drew no sonorant at all; inventory: {:?}",
            ph.inventory
        );
    }

    /// The floor is a claim about every seed, not the one in the test above.
    /// claim: invariant(forall-seed) — the sonorant floor holds for every
    /// seed in 0..64 under the quietest envelope the roster carries.
    #[test]
    fn the_sonorant_floor_holds_across_seeds() {
        let env = Envelope {
            voice_loudness: 0.35,
            vowel_space: 0.70,
            ..manikin_env()
        };
        for seed in 0..64u64 {
            let ph = draw_phonology(
                &Seed(seed),
                "quiet-probe",
                &env,
                &crate::typology::sonorant_open(),
            );
            assert!(
                ph.inventory.iter().any(is_sonorant_seg),
                "seed {seed} drew no sonorant"
            );
        }
    }

    /// The floor is a property of the BUNDLE, not of every language. Only
    /// `sonorant-open` requires a liquid; the control bundle must be free to
    /// draw a liquid-free inventory, or the control stops being a control.
    ///
    /// claim: invariant(forall-seed) — sonorant-open holds a liquid on all 64
    /// probe seeds; the same sweep also needs an existential witness that the
    /// control bundle does NOT, which is the half that actually falsifies an
    /// unconditional floor.
    #[test]
    fn only_the_sonorant_open_bundle_requires_a_liquid() {
        let env = Envelope {
            voice_loudness: 0.35,
            ..manikin_env()
        };
        let sonorant_bundle = crate::typology::sonorant_open();
        let control = crate::typology::concatenative();
        let mut control_had_a_liquid_free_draw = false;
        for seed in 0..64u64 {
            let open = draw_phonology(&Seed(seed), "probe", &env, &sonorant_bundle);
            assert!(
                open.inventory.iter().any(is_sonorant_seg),
                "sonorant-open drew no liquid at seed {seed}"
            );
            let ctl = draw_phonology(&Seed(seed), "probe", &env, &control);
            if !ctl.inventory.iter().any(is_sonorant_seg) {
                control_had_a_liquid_free_draw = true;
            }
        }
        assert!(
            control_had_a_liquid_free_draw,
            "the control bundle got a liquid on all 64 seeds — the floor is \
             being applied unconditionally, so the control is not a control"
        );
    }

    /// A trill is not an exotic manner. An alveolar trill is present in a
    /// large majority of the world's languages; a click and an ejective are
    /// genuinely marked. Grouping the three is what left 17 of the 18 shipped
    /// tongues with zero liquid-bearing words (spec §3.1, §3.3).
    #[test]
    fn a_trill_is_permitted_without_an_exotic_capability() {
        let env = Envelope {
            exotic: ExoticSeg::None,
            ..manikin_env()
        };
        assert!(
            permits(&env, &cons(Place::Alveolar, Manner::Trill, true)),
            "an alveolar trill must be drawable without an exotic capability"
        );
    }

    /// The other half, and what keeps this from being a blanket ungating:
    /// clicks and ejectives stay behind the capability gate.
    #[test]
    fn clicks_and_ejectives_are_still_gated() {
        let env = Envelope {
            exotic: ExoticSeg::None,
            ..manikin_env()
        };
        for manner in [Manner::Click, Manner::Ejective] {
            assert!(
                !permits(&env, &cons(Place::Alveolar, manner, false)),
                "{manner:?} must remain gated behind an exotic capability"
            );
        }
    }

    /// A cramped-but-plausible phonology: three stops, one nasal, three vowel
    /// qualities; onsets a single stop, codas a nasal or nothing. Base capacity
    /// is `3 (onset) × 3 (vowels) × 2 (coda) = 18`, below the floor.
    fn cramped_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                cons(Place::Alveolar, Manner::Stop, false), // t
                cons(Place::Alveolar, Manner::Stop, true),  // d
                cons(Place::Velar, Manner::Stop, false),    // k
                cons(Place::Alveolar, Manner::Nasal, true), // n
                vow(Height::High, Backness::Front),         // i
                vow(Height::Low, Backness::Central),        // a
                vow(Height::High, Backness::Back),          // u
            ],
            onsets: vec![vec![Manner::Stop]],
            nuclei: vec![1],
            codas: vec![vec![Manner::Nasal], vec![]],
            harmony: Harmony::None,
        }
    }

    #[test]
    fn distinguishable_capacity_multiplies_onset_nucleus_and_coda_fillings() {
        assert_eq!(distinguishable_capacity(&cramped_phonology()), 18);
    }

    /// The Wearing: no natural language puts an obligatory diphthong in
    /// EVERY syllable, but `nuclei: usize` did exactly that for half of all
    /// drawn phonologies — which is most of what read as obnoxious in
    /// `Qvooshtvoagootao`.
    ///
    /// Two envelopes, because the pre-change draw was envelope-independent
    /// (`range_u32(1, 2)` reads nothing about the species) and so is this
    /// one; a single envelope would leave that unstated.
    /// claim: invariant(forall-seed) — nuclei-set shape over 200 seeds x 2
    /// species, with an embedded non-vacuity guard (complex_seen)
    #[test]
    fn no_language_requires_a_diphthong_in_every_syllable() {
        let mut complex_seen = 0usize;
        for (label, env) in [("goblin", manikin_env()), ("kobold", kobold_env())] {
            for seed in 0..200u64 {
                let ph =
                    draw_phonology(&Seed(seed), label, &env, &crate::typology::concatenative());
                assert!(
                    ph.nuclei.contains(&1),
                    "{label} seed {seed}: every syllable is obligatorily complex ({:?})",
                    ph.nuclei
                );
                assert!(
                    ph.nuclei.windows(2).all(|w| w[0] < w[1]),
                    "{label} seed {seed}: nuclei must be a sorted set ({:?})",
                    ph.nuclei
                );
                if ph.nuclei.iter().any(|&n| n > 1) {
                    complex_seen += 1;
                }
            }
        }
        // Non-vacuity. `nuclei = vec![1]` for every seed would satisfy every
        // assertion above while quietly deleting diphthongs from the world
        // rather than making them optional — the "passes for the wrong
        // reason" failure this campaign keeps hitting. The claim is that a
        // diphthong is PERMITTED, not that it is gone.
        assert!(
            complex_seen > 40,
            "the template set has degenerated to a constant simple nucleus: only \
             {complex_seen}/400 draws admit a complex nucleus at all"
        );
    }

    /// claim: structural(seed: none) — false-positive seed-loop flag; `s` binds a
    /// Segment, single hand-built cramped_phonology() scenario
    #[test]
    fn the_capacity_floor_widens_a_tone_capable_species_by_pitch() {
        let env = tonal_env();
        let mut ph = cramped_phonology();
        assert!(
            distinguishable_capacity(&ph) < CAPACITY_FLOOR,
            "test premise"
        );
        ensure_capacity_floor(&env, &mut ph);
        assert!(
            distinguishable_capacity(&ph) >= CAPACITY_FLOOR,
            "a tone-capable species must reach the floor via pitch"
        );
        assert!(
            ph.inventory.iter().any(is_toned_vowel),
            "the remedy must be tone (pitch), not new consonants"
        );
        // No new consonant was introduced — character preserved.
        assert_eq!(
            ph.inventory
                .iter()
                .filter(|s| matches!(s, Segment::Consonant { .. }))
                .count(),
            4,
            "the floor must not add un-characteristic consonants"
        );
    }

    #[test]
    fn the_capacity_floor_never_widens_an_atonal_species() {
        // Atonal species keep their (accepted) low capacity — no tone is added,
        // so the draw stays byte-identical (spec §10 Q3: the tail is measured,
        // not repaired).
        let atonal = Envelope {
            tonality: 0.0,
            ..manikin_env()
        };
        let mut ph = cramped_phonology();
        let before = ph.clone();
        ensure_capacity_floor(&atonal, &mut ph);
        assert_eq!(ph, before, "an atonal species must not be widened");
    }

    /// claim: structural(seed: 5) — false-positive seed-loop flag; `s` binds a
    /// Segment, single fixed seed, two scenarios (atonal vs tonal)
    #[test]
    fn the_tone_leg_is_isolated_the_consonant_draw_is_tonality_independent() {
        // The tone draw lives on its own `phonology/tones` leg, so raising
        // tonality must not perturb the consonant inventory (drawn on the
        // separate `inventory` leg) — only add toned vowels.
        let atonal = draw_phonology(
            &Seed(5),
            "x",
            &manikin_env(),
            &crate::typology::concatenative(),
        );
        let tonal = draw_phonology(
            &Seed(5),
            "x",
            &tonal_env(),
            &crate::typology::concatenative(),
        );
        let consonants = |ph: &Phonology| -> Vec<Segment> {
            ph.inventory
                .iter()
                .copied()
                .filter(|s| matches!(s, Segment::Consonant { .. }))
                .collect()
        };
        assert_eq!(
            consonants(&atonal),
            consonants(&tonal),
            "the tone dimension must not change the drawn consonants"
        );
    }

    /// Pinned from the pre-Stage-3 tree at Task 8's byte-inert commit —
    /// `the_control_bundle_draws_what_it_always_drew` measures these are
    /// still what `concatenative` draws for `("goblin", Seed(42))` once the
    /// per-bundle law is applied.
    const CONTROL_CODA_COUNT: usize = 2;
    /// See [`CONTROL_CODA_COUNT`].
    const CONTROL_NUCLEI: [usize; 1] = [1];

    /// Each bundle's coda law is visible in the drawn templates. These are
    /// the four rules the campaign exists to introduce, so each gets its own
    /// assertion rather than one loop — a loop would let three pass on the
    /// strength of the fourth.
    /// claim: invariant(forall-seed) — templatic never draws an open coda,
    /// for every seed in 0..32.
    #[test]
    fn the_obstruent_obligatory_law_never_draws_an_open_coda() {
        let env = manikin_env();
        let typ = crate::typology::templatic();
        for seed in 0..32u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            assert!(
                !ph.codas.iter().any(|c| c.is_empty()),
                "seed {seed}: templatic drew an open coda: {:?}",
                ph.codas
            );
        }
    }

    /// claim: invariant(forall-seed) — isolating-tonal never draws a coda
    /// cluster, for every seed in 0..32; whether it also admits an open
    /// syllable somewhere in the sweep is the separate `saw_open` readout
    /// below.
    #[test]
    fn the_open_or_nasal_law_admits_an_open_coda() {
        let env = manikin_env();
        let typ = crate::typology::isolating_tonal();
        let mut saw_open = false;
        for seed in 0..32u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            for c in &ph.codas {
                assert!(
                    c.len() <= 1,
                    "seed {seed}: isolating drew a coda cluster: {c:?}"
                );
                if c.is_empty() {
                    saw_open = true;
                }
            }
        }
        assert!(saw_open, "isolating-tonal never drew an open syllable");
    }

    /// The onset law is what actually puts a liquid into a WORD, and Stage 2
    /// proved the floor alone does not: proto-elf held `/r/` in its inventory
    /// while no drawn onset slot could host it, so zero roots carried one
    /// (spec §3.7). `sonorant-open` must therefore produce a sonorant-second
    /// onset by construction, not by draw.
    /// claim: invariant(forall-seed) — every onset cluster sonorant-open
    /// draws has a sonorant second slot, for every seed in 0..32.
    #[test]
    fn the_sonorant_second_law_puts_a_sonorant_in_every_onset_cluster() {
        let env = manikin_env();
        let typ = crate::typology::sonorant_open();
        for seed in 0..32u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            let clusters: Vec<&Vec<Manner>> = ph.onsets.iter().filter(|o| o.len() > 1).collect();
            assert!(
                !clusters.is_empty(),
                "seed {seed}: sonorant-open drew no onset cluster at all"
            );
            for c in clusters {
                assert!(
                    matches!(c[1], Manner::Trill | Manner::Approximant | Manner::Nasal),
                    "seed {seed}: cluster {c:?} second slot is not a sonorant"
                );
            }
        }
    }

    /// Regression guard for the near-miss The Burr's controller caught by
    /// mutation: the test above accepts `Nasal` as satisfying "sonorant
    /// second slot", so reverting the onset helpers' predicate from
    /// [`is_liquid_manner`] back to the wider [`is_sonorant_manner`] passes
    /// every test in this file (nasal is always available and never
    /// gated) while silently returning `proto elf`'s liquid-root count to
    /// its pre-campaign 0 (spec §3.7's whole motivation). This test pins
    /// the FORCED template specifically (index 0 — see
    /// `draw_phonotactics`'s `i == 0` arm) to a liquid, not merely a
    /// sonorant, second slot. It must NOT be widened into a blanket claim
    /// over every onset: `sonorize_onset_cluster` leaves other templates'
    /// naturally-drawn clusters alone unless they are already non-liquid,
    /// and a nasal-second cluster elsewhere is legitimate — only the
    /// designated template's guarantee is a liquid by construction.
    /// claim: invariant(forall-seed) — sonorant-open's forced onset
    /// template (index 0) has a liquid, not merely a sonorant, second
    /// slot, for every seed in 0..32.
    #[test]
    fn the_forced_onset_template_carries_a_liquid_not_merely_a_sonorant() {
        let env = manikin_env();
        let typ = crate::typology::sonorant_open();
        for seed in 0..32u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            let forced = &ph.onsets[0];
            assert_eq!(
                forced.len(),
                2,
                "seed {seed}: the forced onset template {forced:?} is not a two-slot cluster"
            );
            assert!(
                matches!(forced[1], Manner::Trill | Manner::Approximant),
                "seed {seed}: the forced onset template's second slot {:?} is not a liquid                  (Trill or Approximant) — a Nasal there is the exact regression this test                  guards against",
                forced[1]
            );
        }
    }

    /// The outcome half of the regression guard above: not just that the
    /// mechanism seats a liquid in a template, but that assigning real
    /// proto roots over that phonology actually surfaces one — the direct
    /// end-to-end claim spec §3.7 makes and the campaign's own readout
    /// measures via `proto elf`. Harder to satisfy vacuously than the
    /// mechanism test: a synthetic concept universe large enough that at
    /// least one concept's per-concept draw lands on the forced template
    /// (`draw_candidate` re-derives its own stream per concept, so this is
    /// not guaranteed by any single draw the way the mechanism test is).
    /// claim: invariant(forall-seed) — assigning 200 synthetic proto roots
    /// over a sonorant-open phonology yields at least one root containing
    /// a liquid, for every seed in 0..8.
    #[test]
    fn sonorant_open_proto_roots_actually_contain_a_liquid() {
        let env = manikin_env();
        let typ = crate::typology::sonorant_open();
        let concepts: Vec<String> = (0..200).map(|i| format!("probe-concept-{i}")).collect();
        let concept_refs: Vec<&str> = concepts.iter().map(String::as_str).collect();
        for seed in 0..8u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            let roots = crate::assign_proto_roots(&Seed(seed), "probe", &ph, &concept_refs, &[]);
            let has_liquid = roots.values().any(|segments| {
                segments.iter().any(|s| {
                    matches!(
                        s,
                        Segment::Consonant {
                            manner: Manner::Trill | Manner::Approximant,
                            ..
                        }
                    )
                })
            });
            assert!(
                has_liquid,
                "seed {seed}: none of {} assigned proto roots contain a liquid segment",
                roots.len()
            );
        }
    }

    /// The single-slot law admits no clusters at all.
    /// claim: invariant(forall-seed) — isolating-tonal never draws an onset
    /// cluster, for every seed in 0..32.
    #[test]
    fn the_single_onset_law_draws_no_clusters() {
        let env = manikin_env();
        let typ = crate::typology::isolating_tonal();
        for seed in 0..32u64 {
            let ph = draw_phonology(&Seed(seed), "probe", &env, &typ);
            for o in &ph.onsets {
                assert!(
                    o.len() <= 1,
                    "seed {seed}: isolating-tonal drew an onset cluster {o:?}"
                );
            }
        }
    }

    /// The control's templates must be reachable exactly as before, or P4's
    /// control is compromised.
    #[test]
    fn the_control_bundle_draws_what_it_always_drew() {
        let env = manikin_env();
        let typ = crate::typology::concatenative();
        let ph = draw_phonology(&Seed(42), "goblin", &env, &typ);
        // Pinned from the pre-Stage-3 tree at Task 8's byte-inert commit.
        assert_eq!(ph.codas.len(), CONTROL_CODA_COUNT);
        assert_eq!(ph.nuclei, CONTROL_NUCLEI);
    }
}
