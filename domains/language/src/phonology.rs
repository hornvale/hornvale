//! The phonology engine: a per-species phoneme inventory and syllable
//! phonotactics drawn under the species' articulation envelope. `Envelope`
//! is language's OWN copy of the articulation dimensions — populated later
//! by the composition root from the species `ArticulationVector`; this
//! domain never imports `hornvale-species`. `draw_phonology` never
//! constructs a `Segment` outside [`crate::phoneme::canonical_segments`]:
//! `romanize`/`ipa` are exhaustive only over that curated set, so an
//! off-menu feature combination would surface as `"?"` in every later name.

use crate::phoneme::{Manner, Place, Segment, canonical_segments, sonority};
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
    /// How many nucleus slots a syllable may have (1 = simple vowel, 2 =
    /// the species' names may show diphthongs).
    pub nuclei: usize,
    /// Coda templates: each is a sequence of manner slots a syllable-final
    /// cluster may fill, in order (an empty template is an open syllable).
    pub codas: Vec<Vec<Manner>>,
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
        }, // i
        Segment::Vowel {
            height: Mid,
            backness: Front,
            rounded: false,
        }, // e
        Segment::Vowel {
            height: Low,
            backness: Central,
            rounded: false,
        }, // a
        Segment::Vowel {
            height: Mid,
            backness: Back,
            rounded: true,
        }, // o
        Segment::Vowel {
            height: High,
            backness: Back,
            rounded: true,
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

/// Whether `seg` (assumed a vowel) falls inside the `vowel_space`-sized
/// band centered on `a`.
fn vowel_permitted(vowel_space: f64, seg: &Segment) -> bool {
    let order = vowel_order();
    let Some(idx) = order.iter().position(|v| v == seg) else {
        // Not one of the 5 canonical vowels: outside the curated set
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

/// Whether `manner` is one of the three exotic (non-pulmonic or trilled)
/// manners `Envelope::exotic` gates.
fn exotic_manner(manner: Manner) -> Option<ExoticSeg> {
    match manner {
        Manner::Trill => Some(ExoticSeg::Trill),
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
) -> Vec<Manner> {
    if manners.is_empty() {
        return Vec::new();
    }
    let len = stream.range_u32(min_len as u32, max_len as u32) as usize;
    (0..len)
        .filter_map(|_| stream.pick(manners).copied())
        .collect()
}

/// Draw the onset/nucleus/coda phonotactic templates from `inventory`'s
/// available consonant manners.
fn draw_phonotactics(
    stream: &mut Stream,
    inventory: &[Segment],
) -> (Vec<Vec<Manner>>, usize, Vec<Vec<Manner>>) {
    let manners = consonant_manners(inventory);

    let onset_count = stream.range_u32(2, 3) as usize;
    let onsets = (0..onset_count)
        .map(|_| draw_manner_slots(stream, &manners, 1, 2))
        .collect();

    let nuclei = stream.range_u32(1, 2) as usize;

    let coda_count = stream.range_u32(1, 2) as usize;
    let codas = (0..coda_count)
        .map(|_| draw_manner_slots(stream, &manners, 0, 1))
        .collect();

    (onsets, nuclei, codas)
}

/// Draw a per-species phonology: a phoneme inventory (a subset of
/// [`crate::phoneme::canonical_segments`] permitted by `env`, with
/// high-sonority consonants down-weighted when `env.voice_loudness` is
/// low) and syllable phonotactic templates. Every draw comes from
/// `seed.derive("language").derive(species).derive("phonology")`, split
/// into an `"inventory"` sub-stream and a `"phonotactics"` sub-stream so
/// adding a new draw to one never perturbs the other.
/// type-audit: bare-ok(identifier-text: species)
pub fn draw_phonology(seed: &Seed, species: &str, env: &Envelope) -> Phonology {
    let phonology_seed = seed.derive("language").derive(species).derive("phonology");

    let candidates: Vec<Segment> = canonical_segments()
        .into_iter()
        .filter(|s| permits(env, s))
        .collect();

    let mut inventory_stream = phonology_seed.derive("inventory").stream();
    let mut inventory: Vec<Segment> = Vec::new();
    for seg in &candidates {
        let keep = match seg {
            // Every envelope-permitted vowel is always kept: "the vowels
            // the vowel_space allows" is not itself a probabilistic draw.
            Segment::Vowel { .. } => true,
            Segment::Consonant { .. } => inventory_stream.next_f64() < keep_probability(env, seg),
        };
        if keep {
            inventory.push(*seg);
        }
    }
    ensure_minimum_consonants(&candidates, &mut inventory);

    let mut phonotactics_stream = phonology_seed.derive("phonotactics").stream();
    let (onsets, nuclei, codas) = draw_phonotactics(&mut phonotactics_stream, &inventory);

    Phonology {
        inventory,
        onsets,
        nuclei,
        codas,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn goblin_env() -> Envelope {
        Envelope {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.5,
            sibilance: 0.5,
            voice_loudness: 0.5,
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
        assert!(permits(&goblin_env(), &bilabial));
    }

    #[test]
    fn a_quiet_species_admits_its_trill_rarely_or_not_at_all() {
        // Kobold is Trill-capable but low-loudness: the drawn inventory should
        // contain few/no trills relative to a loud species with the same manner.
        let quiet = draw_phonology(&Seed(42), "kobold", &kobold_env());
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
        let loud_ph = draw_phonology(&Seed(42), "kobold", &loud);
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
        let a = draw_phonology(&Seed(7), "kobold", &kobold_env());
        let b = draw_phonology(&Seed(7), "kobold", &kobold_env());
        assert_eq!(a.inventory, b.inventory);
        assert_eq!(a.onsets, b.onsets);
    }

    #[test]
    fn inventory_respects_the_envelope() {
        let ph = draw_phonology(&Seed(3), "kobold", &kobold_env());
        assert!(ph.inventory.iter().all(|s| permits(&kobold_env(), s)));
        assert!(!ph.inventory.is_empty());
    }

    /// Carry-forward from Task 3's review: `romanize`/`ipa` return `"?"`
    /// for any `Segment` outside `canonical_segments()`. If `draw_phonology`
    /// ever synthesized a segment instead of filtering the canonical set,
    /// "?" would surface in every later name. Assert the invariant holds
    /// across multiple species/envelopes.
    #[test]
    fn drawn_inventory_is_always_a_subset_of_canonical_segments() {
        let canonical = canonical_segments();
        for (seed, species, env) in [
            (Seed(1), "goblin", goblin_env()),
            (Seed(42), "kobold", kobold_env()),
            (Seed(99), "kobold", kobold_env()),
        ] {
            let ph = draw_phonology(&seed, species, &env);
            for seg in &ph.inventory {
                assert!(
                    canonical.contains(seg),
                    "drawn segment {seg:?} is not in canonical_segments(); \
                     romanize/ipa would render it as \"?\""
                );
            }
        }
    }
}
