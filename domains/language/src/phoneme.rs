//! The phoneme model: `Segment` is an articulatory feature-bundle — the
//! truth from which every surface form derives. `romanize` (ASCII-ish, for
//! the almanac), `ipa` (for the book), and `espeak` (espeak-ng mnemonics, for
//! authored audio) are VIEWS over a segment; none is ever stored. `sonority`
//! gives the 0–5 rank a later task's loudness bias and phonotactics depend on.
//!
//! The curated inventory below is deliberately practical, not exhaustive of
//! human phonology: stops p/b t/d k/g q, fricatives f/v x, sibilants
//! s/z ʃ/ʒ, nasals m n ŋ, a trill r, a click ǃ, an ejective kʼ, approximants
//! l j w, and vowels i/e/a/o/u with rounding. `romanize`/`ipa` are exhaustive
//! matches over exactly this set; a feature combination outside it has no
//! glyph and is not represented here.

/// Place of articulation: where in the vocal tract a consonant is formed.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Place {
    /// The lips (p, b, f, v, w).
    Labial,
    /// The alveolar ridge (t, d, s, z, n, l, r).
    Alveolar,
    /// Just behind the alveolar ridge (sh, zh).
    Postalveolar,
    /// The soft palate (k, g, ŋ).
    Velar,
    /// The back of the soft palate (q).
    Uvular,
    /// The glottis (the click's release point, authored here as its place).
    Glottal,
}

/// Manner of articulation: how the airstream is shaped.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Manner {
    /// A full closure then release (p, t, k).
    Stop,
    /// A narrowed channel, continuous turbulence (f, x).
    Fricative,
    /// A high-pitched, sharply channeled fricative (s, sh).
    Sibilant,
    /// Air routed through the nose (m, n, ŋ).
    Nasal,
    /// A rapid tap sequence against the articulator (r).
    Trill,
    /// A non-pulmonic ingressive stop (ǃ).
    Click,
    /// A non-pulmonic glottalic stop (kʼ).
    Ejective,
    /// A narrowing that falls short of frication (l, j, w).
    Approximant,
}

/// Vowel height: how open the mouth is.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Height {
    /// The tongue sits close to the roof of the mouth (i, u).
    High,
    /// A middling tongue position (e, o).
    Mid,
    /// The mouth is maximally open (a).
    Low,
}

/// Vowel backness: where along the mouth the tongue bunches.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Backness {
    /// Tongue bunched toward the teeth (i, e).
    Front,
    /// Tongue bunched in the middle (a).
    Central,
    /// Tongue bunched toward the throat (u, o).
    Back,
}

/// A syllable nucleus's pitch — the suprasegmental tier, orthogonal to the
/// segmental place/manner/height inventory (spec §2.1). `Neutral` is the
/// atonal default: it carries no contrast and renders bare, so every vowel
/// bearing it reproduces the pre-tone output exactly. A tone-capable language
/// draws its inventory from the level tones. `Neutral` is declared first so
/// it is the `Ord` minimum — the tone key sorts an atonal (all-`Neutral`)
/// world identically to a world with no tone dimension at all.
///
/// `Mid` is authored but **banked**: the tonogenesis rule (spec §4) conditions
/// on a single binary feature (the dropped segment's voicing → `High`/`Low`),
/// so `Mid` awaits a future three-way conditioning source (aspiration, a
/// three-register voicing split) and is never written by this epoch's rule.
/// type-audit: bare-ok(flag)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tone {
    /// The atonal default: no pitch contrast, renders bare. Every daughter's
    /// vowels carry this unless its drawn tone inventory admits a contrast.
    Neutral,
    /// A high level tone (˥). Written by tonogenesis when the merger dropped a
    /// voiceless segment.
    High,
    /// A mid level tone (˧). Banked — no rule writes it this epoch.
    Mid,
    /// A low level tone (˩). Written by tonogenesis when the merger dropped a
    /// voiced segment (nasals, being voiced, fall in this bucket).
    Low,
}

/// A segment: an articulatory feature-bundle, the truth from which every
/// surface form (romanization, IPA) derives as a view.
/// type-audit: bare-ok(flag)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Segment {
    /// A consonant: place, manner, and voicing.
    Consonant {
        /// Where in the vocal tract it is formed.
        place: Place,
        /// How the airstream is shaped.
        manner: Manner,
        /// Whether the vocal folds vibrate.
        voiced: bool,
    },
    /// A vowel: height, backness, lip rounding, and its suprasegmental tone.
    /// `tone` is the LAST field so it is the last `Ord` key — the stable
    /// tie-break the determinism contract depends on (an all-`Tone::Neutral`
    /// world sorts identically to a pre-tone one).
    Vowel {
        /// How open the mouth is.
        height: Height,
        /// Where along the mouth the tongue bunches.
        backness: Backness,
        /// Whether the lips are rounded.
        rounded: bool,
        /// The nucleus's suprasegmental pitch (`Tone::Neutral` for an atonal
        /// language).
        tone: Tone,
    },
}

/// The curated segment inventory the engine can draw from. The phonology
/// draw (`phonology::draw_phonology`) filters this set under a species'
/// articulation envelope; `romanize`/`ipa` are exhaustive only over
/// exactly this set.
pub(crate) fn canonical_segments() -> Vec<Segment> {
    use Backness::*;
    use Height::*;
    use Manner::*;
    use Place::*;
    let mut segments = vec![
        // Stops.
        Segment::Consonant {
            place: Labial,
            manner: Stop,
            voiced: false,
        }, // p
        Segment::Consonant {
            place: Labial,
            manner: Stop,
            voiced: true,
        }, // b
        Segment::Consonant {
            place: Alveolar,
            manner: Stop,
            voiced: false,
        }, // t
        Segment::Consonant {
            place: Alveolar,
            manner: Stop,
            voiced: true,
        }, // d
        Segment::Consonant {
            place: Velar,
            manner: Stop,
            voiced: false,
        }, // k
        Segment::Consonant {
            place: Velar,
            manner: Stop,
            voiced: true,
        }, // g
        Segment::Consonant {
            place: Uvular,
            manner: Stop,
            voiced: false,
        }, // q
        // Fricatives.
        Segment::Consonant {
            place: Labial,
            manner: Fricative,
            voiced: false,
        }, // f
        Segment::Consonant {
            place: Labial,
            manner: Fricative,
            voiced: true,
        }, // v
        Segment::Consonant {
            place: Velar,
            manner: Fricative,
            voiced: false,
        }, // x
        // Sibilants.
        Segment::Consonant {
            place: Alveolar,
            manner: Sibilant,
            voiced: false,
        }, // s
        Segment::Consonant {
            place: Alveolar,
            manner: Sibilant,
            voiced: true,
        }, // z
        Segment::Consonant {
            place: Postalveolar,
            manner: Sibilant,
            voiced: false,
        }, // sh
        Segment::Consonant {
            place: Postalveolar,
            manner: Sibilant,
            voiced: true,
        }, // zh
        // Nasals.
        Segment::Consonant {
            place: Labial,
            manner: Nasal,
            voiced: true,
        }, // m
        Segment::Consonant {
            place: Alveolar,
            manner: Nasal,
            voiced: true,
        }, // n
        Segment::Consonant {
            place: Velar,
            manner: Nasal,
            voiced: true,
        }, // ŋ
        // Trill.
        Segment::Consonant {
            place: Alveolar,
            manner: Trill,
            voiced: true,
        }, // r
        // Click.
        Segment::Consonant {
            place: Glottal,
            manner: Click,
            voiced: false,
        }, // ǃ
        // Ejective.
        Segment::Consonant {
            place: Velar,
            manner: Ejective,
            voiced: false,
        }, // kʼ
        // Approximants.
        Segment::Consonant {
            place: Alveolar,
            manner: Approximant,
            voiced: true,
        }, // l
        Segment::Consonant {
            place: Postalveolar,
            manner: Approximant,
            voiced: true,
        }, // j
        Segment::Consonant {
            place: Labial,
            manner: Approximant,
            voiced: true,
        }, // w
        // Vowels.
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
    ];
    // The toned vowel variants (spec §3: the curated set grows by the tone
    // dimension). Only High and Low are offered — `Tone::Mid` is banked, so no
    // rule writes it and the inventory draw never admits it. The `Neutral`
    // block above stays first (so an atonal draw, which keeps only Neutral
    // vowels, is byte-identical to the pre-tone set); the toned variants are
    // appended in a fixed tone-major order. `draw_phonology` admits a toned
    // variant only when the species' `tonality` put its tone in the drawn tone
    // inventory, so `romanize`/`ipa` — which ignore tone and render the base
    // glyph — never surface a `"?"` for any of them.
    let qualities: Vec<Segment> = segments
        .iter()
        .copied()
        .filter(|s| matches!(s, Segment::Vowel { .. }))
        .collect();
    for tone in [Tone::High, Tone::Low] {
        for q in &qualities {
            if let Segment::Vowel {
                height,
                backness,
                rounded,
                ..
            } = *q
            {
                segments.push(Segment::Vowel {
                    height,
                    backness,
                    rounded,
                    tone,
                });
            }
        }
    }
    segments
}

/// Render a segment as an ASCII-ish romanization, for the almanac.
///
/// Exhaustive over the curated inventory (see module docs). Segments with
/// no single ASCII letter romanize to a digraph: `sh`/`zh` for the
/// postalveolar sibilants, `ng` for the velar nasal, `kx` for the ejective
/// (its glottalic release has no Latin letter), and `ts` for the glottal
/// click (there is no ASCII click glyph; `ts` approximates its sharp
/// release).
/// type-audit: bare-ok(identifier-text)
pub fn romanize(seg: &Segment) -> &'static str {
    match seg {
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Stop,
            voiced: false,
        } => "p",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Stop,
            voiced: true,
        } => "b",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        } => "t",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: true,
        } => "d",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Stop,
            voiced: false,
        } => "k",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Stop,
            voiced: true,
        } => "g",
        Segment::Consonant {
            place: Place::Uvular,
            manner: Manner::Stop,
            voiced: false,
        } => "q",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Fricative,
            voiced: false,
        } => "f",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Fricative,
            voiced: true,
        } => "v",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Fricative,
            voiced: false,
        } => "x",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "s",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "z",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "sh",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "zh",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Nasal,
            voiced: true,
        } => "m",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Nasal,
            voiced: true,
        } => "n",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        } => "ng",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Trill,
            voiced: true,
        } => "r",
        Segment::Consonant {
            place: Place::Glottal,
            manner: Manner::Click,
            voiced: false,
        } => "ts",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Ejective,
            voiced: false,
        } => "kx",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Approximant,
            voiced: true,
        } => "l",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Approximant,
            voiced: true,
        } => "j",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Approximant,
            voiced: true,
        } => "w",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Front,
            rounded: false,
            ..
        } => "i",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
            ..
        } => "e",
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            ..
        } => "a",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Back,
            rounded: true,
            ..
        } => "o",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Back,
            rounded: true,
            ..
        } => "u",
        // Outside the curated inventory: no romanization is authored.
        _ => "?",
    }
}

/// Render a segment as its nearest IPA glyph, for the book.
///
/// Exhaustive over the curated inventory (see module docs). `kʼ` (the
/// ejective) and `ǃ` (the click) are the only multi-codepoint/non-Latin
/// glyphs; every other mapping is a single standard IPA letter.
/// type-audit: bare-ok(identifier-text)
pub fn ipa(seg: &Segment) -> &'static str {
    match seg {
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Stop,
            voiced: false,
        } => "p",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Stop,
            voiced: true,
        } => "b",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        } => "t",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: true,
        } => "d",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Stop,
            voiced: false,
        } => "k",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Stop,
            voiced: true,
        } => "g",
        Segment::Consonant {
            place: Place::Uvular,
            manner: Manner::Stop,
            voiced: false,
        } => "q",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Fricative,
            voiced: false,
        } => "f",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Fricative,
            voiced: true,
        } => "v",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Fricative,
            voiced: false,
        } => "x",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "s",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "z",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "ʃ",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "ʒ",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Nasal,
            voiced: true,
        } => "m",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Nasal,
            voiced: true,
        } => "n",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        } => "ŋ",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Trill,
            voiced: true,
        } => "r",
        Segment::Consonant {
            place: Place::Glottal,
            manner: Manner::Click,
            voiced: false,
        } => "ǃ",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Ejective,
            voiced: false,
        } => "kʼ",
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Approximant,
            voiced: true,
        } => "l",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Approximant,
            voiced: true,
        } => "j",
        Segment::Consonant {
            place: Place::Labial,
            manner: Manner::Approximant,
            voiced: true,
        } => "w",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Front,
            rounded: false,
            ..
        } => "i",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
            ..
        } => "e",
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            ..
        } => "a",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Back,
            rounded: true,
            ..
        } => "o",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Back,
            rounded: true,
            ..
        } => "u",
        // Outside the curated inventory: no IPA glyph is authored.
        _ => "?",
    }
}

/// The romanization mark a `tone` appends to its vowel's glyph (spec §6): a
/// trailing combining diacritic — High an acute (á), Low a grave (à), Mid a
/// macron (ā) — or the empty string for `Tone::Neutral`, which renders bare.
/// A view over the segment, never stored; a word-level renderer
/// ([`crate::naming::render_views`]) appends it right after the vowel it tones.
/// type-audit: bare-ok(identifier-text)
pub fn tone_mark_roman(tone: Tone) -> &'static str {
    match tone {
        Tone::Neutral => "",
        Tone::High => "\u{0301}", // combining acute accent
        Tone::Mid => "\u{0304}",  // combining macron
        Tone::Low => "\u{0300}",  // combining grave accent
    }
}

/// The IPA mark a `tone` appends to its vowel (spec §6): a Chao tone letter —
/// High `˥`, Mid `˧`, Low `˩` — or the empty string for `Tone::Neutral`. A
/// view, appended after the vowel by the word-level renderer.
/// type-audit: bare-ok(identifier-text)
pub fn tone_mark_ipa(tone: Tone) -> &'static str {
    match tone {
        Tone::Neutral => "",
        Tone::High => "˥",
        Tone::Mid => "˧",
        Tone::Low => "˩",
    }
}

/// The tone borne by `seg` if it is a vowel, else `Tone::Neutral` — the hook a
/// word-level renderer uses to decide whether to append a tone mark.
pub fn tone_of(seg: &Segment) -> Tone {
    match seg {
        Segment::Vowel { tone, .. } => *tone,
        Segment::Consonant { .. } => Tone::Neutral,
    }
}

/// Render a segment as its espeak-ng phoneme mnemonic (Kirshenbaum-style
/// ASCII), the notation `hornvale voice` feeds espeak-ng's direct phoneme
/// input to author the book's audio clips.
///
/// Verified against espeak-ng 1.52.0, voice `en`. Five segments differ
/// from their IPA glyph: `ʃ`/`ʒ` → `S`/`Z`, `ŋ` → `N`, and two documented
/// approximations in the spirit of the romanization digraphs — the click
/// (voice `en` silently drops Kirshenbaum's `!`) renders as `tS`, and the
/// ejective as plain `k` (`'` is espeak's stress marker, so `k'` cannot be
/// written). Every other mnemonic coincides with the IPA glyph, so those
/// arms delegate to [`ipa`]; the ASCII-guard test keeps that coupling safe.
/// type-audit: bare-ok(identifier-text)
pub fn espeak(seg: &Segment) -> &'static str {
    match seg {
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "S",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "Z",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        } => "N",
        Segment::Consonant {
            place: Place::Glottal,
            manner: Manner::Click,
            voiced: false,
        } => "tS",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Ejective,
            voiced: false,
        } => "k",
        other => ipa(other),
    }
}

/// Assemble a whole word's espeak-ng formulation: each segment's mnemonic
/// in order, a `'` stress marker before the first vowel (explicit stress
/// keeps the formulation self-contained rather than leaning on espeak's
/// automatic assignment), wrapped in `[[…]]` for direct phoneme input.
/// type-audit: bare-ok(identifier-text)
pub fn espeak_word(segments: &[Segment]) -> String {
    let mut body = String::new();
    let mut stressed = false;
    for seg in segments {
        if !stressed && matches!(seg, Segment::Vowel { .. }) {
            body.push('\'');
            stressed = true;
        }
        body.push_str(espeak(seg));
    }
    format!("[[{body}]]")
}

/// The sonority rank of a segment, 0 (least sonorous) to 5 (most): the
/// scale a later task's loudness bias and phonotactics depend on. Ranked by
/// manner alone — place and voicing do not affect sonority.
///
/// `Stop`/`Ejective`/`Click` 0 < `Fricative`/`Sibilant` 1 < `Nasal` 2 <
/// `Trill` 3 < `Approximant` 4 < `Vowel` 5.
/// type-audit: bare-ok(count)
pub fn sonority(seg: &Segment) -> u8 {
    match seg {
        Segment::Consonant {
            manner: Manner::Stop,
            ..
        }
        | Segment::Consonant {
            manner: Manner::Ejective,
            ..
        }
        | Segment::Consonant {
            manner: Manner::Click,
            ..
        } => 0,
        Segment::Consonant {
            manner: Manner::Fricative,
            ..
        }
        | Segment::Consonant {
            manner: Manner::Sibilant,
            ..
        } => 1,
        Segment::Consonant {
            manner: Manner::Nasal,
            ..
        } => 2,
        Segment::Consonant {
            manner: Manner::Trill,
            ..
        } => 3,
        Segment::Consonant {
            manner: Manner::Approximant,
            ..
        } => 4,
        Segment::Vowel { .. } => 5,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn romanization_and_ipa_are_distinct_views_of_one_segment() {
        let s = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        assert_eq!(romanize(&s), "s");
        assert_eq!(ipa(&s), "s");
        let sh = Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        assert_eq!(romanize(&sh), "sh");
        assert_eq!(ipa(&sh), "ʃ");
    }

    #[test]
    fn tone_is_the_last_ord_key_of_a_vowel() {
        // Determinism (spec §8): `tone` is the final `Ord` key, so an
        // all-`Neutral` world sorts exactly as a pre-tone one, toned variants
        // of one vowel order among themselves by tone, and any difference in
        // vowel QUALITY is decided before tone is ever consulted — the stable
        // tie-break `nativize` relies on.
        let a = |tone| Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone,
        };
        // Same quality: tones order in their declared sequence.
        assert!(a(Tone::Neutral) < a(Tone::High));
        assert!(a(Tone::High) < a(Tone::Mid));
        assert!(a(Tone::Mid) < a(Tone::Low));
        // Quality dominates tone: a High-height vowel with the maximal tone
        // still sorts before a Low-height vowel with the minimal tone.
        let i_low_tone = Segment::Vowel {
            height: Height::High,
            backness: Backness::Front,
            rounded: false,
            tone: Tone::Low,
        };
        assert!(
            i_low_tone < a(Tone::Neutral),
            "a difference in height must decide before tone is consulted"
        );
    }

    #[test]
    fn tone_marks_render_for_toned_vowels_and_are_empty_for_neutral() {
        // Neutral renders bare (byte-identity with the pre-tone views); the
        // contrastive tones render their mark. Mid is banked but still has an
        // authored mark for a future rule.
        assert_eq!(tone_mark_roman(Tone::Neutral), "");
        assert_eq!(tone_mark_ipa(Tone::Neutral), "");
        assert_eq!(tone_mark_roman(Tone::High), "\u{0301}");
        assert_eq!(tone_mark_ipa(Tone::High), "˥");
        assert_eq!(tone_mark_roman(Tone::Low), "\u{0300}");
        assert_eq!(tone_mark_ipa(Tone::Low), "˩");
        // `tone_of` reads a vowel's tone and is Neutral for any consonant.
        let a_high = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::High,
        };
        assert_eq!(tone_of(&a_high), Tone::High);
        let t = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        };
        assert_eq!(tone_of(&t), Tone::Neutral);
    }

    #[test]
    fn sonority_orders_stop_below_vowel() {
        let stop = Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Stop,
            voiced: false,
        };
        let vowel = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::Neutral,
        };
        assert!(sonority(&stop) < sonority(&vowel));
    }

    #[test]
    fn a_trill_is_more_sonorous_than_a_sibilant() {
        let trill = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Trill,
            voiced: true,
        };
        let sib = Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        assert!(
            sonority(&trill) > sonority(&sib),
            "the loudness bias depends on this ordering"
        );
    }

    #[test]
    fn espeak_is_a_third_distinct_view() {
        let sh = Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        assert_eq!(espeak(&sh), "S");
        let ng = Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        };
        assert_eq!(espeak(&ng), "N");
        let click = Segment::Consonant {
            place: Place::Glottal,
            manner: Manner::Click,
            voiced: false,
        };
        assert_eq!(espeak(&click), "tS");
        let ejective = Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Ejective,
            voiced: false,
        };
        assert_eq!(espeak(&ejective), "k");
    }

    /// espeak-ng direct phoneme input only accepts its ASCII mnemonics, and
    /// `'` is its stress marker. Guard every canonical segment's mnemonic —
    /// this is what makes the delegation to `ipa()` for the shared arms safe:
    /// if an IPA glyph ever drifts non-ASCII, this fails instead of espeak
    /// silently dropping the phoneme.
    #[test]
    fn every_canonical_espeak_mnemonic_is_bare_ascii_alphanumeric() {
        for seg in canonical_segments() {
            let m = espeak(&seg);
            assert!(
                !m.is_empty() && m.chars().all(|c| c.is_ascii_alphanumeric()),
                "{seg:?} has espeak mnemonic {m:?}"
            );
        }
    }

    #[test]
    fn espeak_word_stresses_the_first_vowel_and_wraps_for_phoneme_input() {
        use Backness::*;
        use Height::*;
        use Manner::*;
        use Place::*;
        // z-v-e-t-n-o-t — the seed-42 goblin sample "Zvetnot".
        let segs = [
            Segment::Consonant {
                place: Alveolar,
                manner: Sibilant,
                voiced: true,
            },
            Segment::Consonant {
                place: Labial,
                manner: Fricative,
                voiced: true,
            },
            Segment::Vowel {
                height: Mid,
                backness: Front,
                rounded: false,
                tone: Tone::Neutral,
            },
            Segment::Consonant {
                place: Alveolar,
                manner: Stop,
                voiced: false,
            },
            Segment::Consonant {
                place: Alveolar,
                manner: Nasal,
                voiced: true,
            },
            Segment::Vowel {
                height: Mid,
                backness: Back,
                rounded: true,
                tone: Tone::Neutral,
            },
            Segment::Consonant {
                place: Alveolar,
                manner: Stop,
                voiced: false,
            },
        ];
        assert_eq!(espeak_word(&segs), "[[zv'etnot]]");
    }

    #[test]
    fn espeak_word_of_a_vowelless_or_empty_run_carries_no_stress_marker() {
        let segs = [Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Sibilant,
            voiced: false,
        }];
        assert_eq!(espeak_word(&segs), "[[s]]");
        assert_eq!(espeak_word(&[]), "[[]]");
    }
}
