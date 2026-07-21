//! LANG-44: numeracy as a per-listener quantity register — the first
//! listener-side epistemic filter in this crate (every existing filter,
//! `crate::account`'s four-stage stack among them, is speaker-side). See
//! `docs/superpowers/specs/2026-07-20-few-and-many-design.md`.
#![warn(missing_docs)]

use crate::streams;
use hornvale_kernel::Seed;
use hornvale_kernel::seed::StreamLabel;

/// A tongue's numeral-system rung: how far its counting words go past the
/// near-universal subitizing floor. Ordered least to most precise
/// (`Subitizing` < `FullCounting` < `Decimals`) — drawn per species,
/// independent of any psychology/culture vector, the same anti-astrology
/// posture [`crate::morphology::MorphDepth`] already keeps for
/// grammaticalization depth: numeral-system elaborateness is
/// historically/environmentally contingent (trade, agriculture,
/// measurement culture), never a claim about cognitive capability.
/// `Subitizing` means this language's numeral system builds no counting
/// words beyond the near-universal pre-linguistic one/two/few/many floor
/// (rapid exact perception of roughly one to four items is itself a
/// human — and animal — universal, not a rung some cultures lack) — a
/// linguistic-typology claim, never a capacity one.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumeracyRung {
    /// No counting words beyond one/two/few/many.
    Subitizing,
    /// Exact integer counting, rendered via `cardinal`-style words.
    FullCounting,
    /// Measured decimal approximation, rendered via `quantity`-style
    /// "about X.Y".
    Decimals,
}

/// Preregistered numeracy-rung weights over `[Subitizing, FullCounting,
/// Decimals]`: full counting is the near-universal typological default;
/// subitizing-only and decimal-measuring systems are both real but
/// minority cases.
const NUMERACY_RUNG_WEIGHTS: [f64; 3] = [10.0, 75.0, 15.0];

/// The `weighted_index` bucket order the draw uses: 0 = `Subitizing`,
/// 1 = `FullCounting`, 2 = `Decimals` (frozen by the weight table above).
fn rung_from_bucket(bucket: usize) -> NumeracyRung {
    match bucket {
        0 => NumeracyRung::Subitizing,
        1 => NumeracyRung::FullCounting,
        _ => NumeracyRung::Decimals,
    }
}

/// Draw `species`' numeracy rung — one permanent stream:
/// `language/<species>/grammar/numeracy-rung`. Drawn, independent of any
/// psychology/culture vector (see [`NumeracyRung`]'s own doc — the
/// anti-astrology line).
/// type-audit: bare-ok(identifier-text)
pub fn numeracy_rung(seed: &Seed, species: &str) -> NumeracyRung {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::GRAMMAR)
        .derive(streams::NUMERACY_RUNG)
        .stream();
    rung_from_bucket(
        stream
            .weighted_index(&NUMERACY_RUNG_WEIGHTS)
            .expect("NUMERACY_RUNG_WEIGHTS is fixed and positive"),
    )
}

/// Render `x` at `rung`'s own precision — the shared codec both a
/// speaker's own rendering and a listener's comprehension degradation
/// reuse (spec §3.2): the same function serves both directions, never two
/// independently-authored mechanisms. `Subitizing` renders exact "one"/
/// "two" only for those exact values; any other value in `(1, 3)` renders
/// the qualitative "more than one" (a fraction like `1.5` degrades to
/// this, matching the spec's own worked example) rather than rounding to
/// a false-precision exact word; `[3, 5)` renders "few"; `5` and above
/// renders "many" (`x < 1.0` is a spec-sketch extension: a sub-unit
/// quantity renders "less than one" rather than being left unhandled).
/// `FullCounting` rounds to the nearest integer and
/// renders via [`crate::clause::cardinal`]. `Decimals` delegates to
/// [`crate::clause::quantity`], unchanged from today's only decimal path.
/// type-audit: bare-ok(prose)
pub fn render_quantity_at_rung(x: f64, rung: NumeracyRung) -> String {
    match rung {
        NumeracyRung::Subitizing => {
            if x == 1.0 {
                "one".to_string()
            } else if x == 2.0 {
                "two".to_string()
            } else if x < 1.0 {
                "less than one".to_string()
            } else if x < 3.0 {
                "more than one".to_string()
            } else if x < 5.0 {
                "few".to_string()
            } else {
                "many".to_string()
            }
        }
        NumeracyRung::FullCounting => crate::clause::cardinal(x.round() as u64),
        NumeracyRung::Decimals => crate::clause::quantity(x),
    }
}

/// Whether `rung`'s vocabulary can name a `numerator:denominator` ratio
/// precisely (LANG-48 spec §3.4). A `1:1` ratio is naming one quantity
/// twice, not composing a relationship, so every rung expresses it
/// (`render_quantity_at_rung(1.0, Subitizing)` is the exact word
/// `"one"`). A genuine relationship between two DIFFERENT integers
/// (`2:1`, `3:2`, ...) requires composing two named quantities into a
/// comparison — `Subitizing`'s four-category vocabulary (one/two/few/
/// many) cannot do this even when both individual sides happen to be
/// individually nameable (`render_quantity_at_rung` renders `1.0` and
/// `2.0` exactly at `Subitizing` too, but naming EACH side is not the
/// same as naming their RELATIONSHIP). `FullCounting` and `Decimals`
/// both can.
/// type-audit: bare-ok(count: numerator), bare-ok(count: denominator), bare-ok(flag: return)
pub fn expressible_at_rung(numerator: u32, denominator: u32, rung: NumeracyRung) -> bool {
    if numerator == denominator {
        // A 1:1 "ratio" is just naming one quantity twice, not composing
        // a relationship — every rung, including Subitizing, can do this
        // (render_quantity_at_rung(1.0, Subitizing) == "one" exactly).
        return true;
    }
    match rung {
        NumeracyRung::Subitizing => false,
        NumeracyRung::FullCounting | NumeracyRung::Decimals => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeracy_rung_is_pure() {
        let a = numeracy_rung(&Seed(1), "test");
        let b = numeracy_rung(&Seed(1), "test");
        assert_eq!(a, b);
    }

    #[test]
    fn numeracy_rung_covers_all_three_buckets_across_many_seeds() {
        let mut saw_subitizing = false;
        let mut saw_full_counting = false;
        let mut saw_decimals = false;
        for i in 0..200u64 {
            match numeracy_rung(&Seed(i), "test") {
                NumeracyRung::Subitizing => saw_subitizing = true,
                NumeracyRung::FullCounting => saw_full_counting = true,
                NumeracyRung::Decimals => saw_decimals = true,
            }
        }
        assert!(saw_subitizing && saw_full_counting && saw_decimals);
    }

    #[test]
    fn the_ladder_renders_genuinely_different_text() {
        // Law 2 (spec §4): the three rungs must not collapse into fewer
        // effective rungs — a fractional value renders three DIFFERENT
        // strings, one per rung.
        let x = 1.5;
        let subitizing = render_quantity_at_rung(x, NumeracyRung::Subitizing);
        let full_counting = render_quantity_at_rung(x, NumeracyRung::FullCounting);
        let decimals = render_quantity_at_rung(x, NumeracyRung::Decimals);
        assert_eq!(subitizing, "more than one");
        assert_eq!(full_counting, "two");
        assert_eq!(decimals, "about 1.5");
        assert_ne!(subitizing, full_counting);
        assert_ne!(full_counting, decimals);
        assert_ne!(subitizing, decimals);

        // The non-obvious pair at an integer: FullCounting's exact "two"
        // vs Decimals' "about 2.0" must still genuinely differ, even
        // though Subitizing and FullCounting correctly agree here.
        let x2 = 2.0;
        let subitizing2 = render_quantity_at_rung(x2, NumeracyRung::Subitizing);
        let full_counting2 = render_quantity_at_rung(x2, NumeracyRung::FullCounting);
        let decimals2 = render_quantity_at_rung(x2, NumeracyRung::Decimals);
        assert_eq!(subitizing2, "two");
        assert_eq!(full_counting2, "two");
        assert_eq!(decimals2, "about 2.0");
        assert_ne!(subitizing2, decimals2);
        assert_ne!(full_counting2, decimals2);
    }

    #[test]
    fn a_two_to_one_ratio_is_expressible_at_full_counting_but_not_subitizing() {
        assert!(!expressible_at_rung(2, 1, NumeracyRung::Subitizing));
        assert!(expressible_at_rung(2, 1, NumeracyRung::FullCounting));
        assert!(expressible_at_rung(2, 1, NumeracyRung::Decimals));
    }

    #[test]
    fn a_one_to_one_ratio_is_expressible_even_at_subitizing() {
        // "one" and "one" are both exact words at every rung, including
        // the floor — Subitizing's own exact-word cases (spec: x == 1.0
        // renders "one" exactly, not a qualitative degradation).
        assert!(expressible_at_rung(1, 1, NumeracyRung::Subitizing));
    }

    #[test]
    fn expressibility_is_monotonic_in_rung() {
        // A coarser rung is never MORE expressive than a finer one — Law
        // 5 (spec §4): if Subitizing can express a ratio, FullCounting
        // and Decimals must too (never the reverse).
        for (numerator, denominator) in [(1u32, 1u32), (2, 1), (3, 1), (3, 2), (5, 4)] {
            if expressible_at_rung(numerator, denominator, NumeracyRung::Subitizing) {
                assert!(expressible_at_rung(
                    numerator,
                    denominator,
                    NumeracyRung::FullCounting
                ));
                assert!(expressible_at_rung(
                    numerator,
                    denominator,
                    NumeracyRung::Decimals
                ));
            }
            if expressible_at_rung(numerator, denominator, NumeracyRung::FullCounting) {
                assert!(expressible_at_rung(
                    numerator,
                    denominator,
                    NumeracyRung::Decimals
                ));
            }
        }
    }
}
