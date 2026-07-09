//! The phoneme model: `Segment` is an articulatory feature-bundle — the
//! truth from which every surface form derives. `romanize` (ASCII-ish, for
//! the almanac), `ipa` (for the book), and `espeak` (espeak-ng mnemonics, for
//! authored audio) are VIEWS over a segment; neither is ever stored. `sonority`
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

/// A segment: an articulatory feature-bundle, the truth from which every
/// surface form (romanization, IPA) derives as a view.
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
    /// A vowel: height, backness, and lip rounding.
    Vowel {
        /// How open the mouth is.
        height: Height,
        /// Where along the mouth the tongue bunches.
        backness: Backness,
        /// Whether the lips are rounded.
        rounded: bool,
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
    vec![
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

/// Render a segment as an ASCII-ish romanization, for the almanac.
///
/// Exhaustive over the curated inventory (see module docs). Segments with
/// no single ASCII letter romanize to a digraph: `sh`/`zh` for the
/// postalveolar sibilants, `ng` for the velar nasal, `kx` for the ejective
/// (its glottalic release has no Latin letter), and `ts` for the glottal
/// click (there is no ASCII click glyph; `ts` approximates its sharp
/// release).
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
        } => "i",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
        } => "e",
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
        } => "a",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Back,
            rounded: true,
        } => "o",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Back,
            rounded: true,
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
        } => "i",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
        } => "e",
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
        } => "a",
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Back,
            rounded: true,
        } => "o",
        Segment::Vowel {
            height: Height::High,
            backness: Backness::Back,
            rounded: true,
        } => "u",
        // Outside the curated inventory: no IPA glyph is authored.
        _ => "?",
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

/// The sonority rank of a segment, 0 (least sonorous) to 5 (most): the
/// scale a later task's loudness bias and phonotactics depend on. Ranked by
/// manner alone — place and voicing do not affect sonority.
///
/// `Stop`/`Ejective`/`Click` 0 < `Fricative`/`Sibilant` 1 < `Nasal` 2 <
/// `Trill` 3 < `Approximant` 4 < `Vowel` 5.
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
}
