//! The presentation lens (The Lantern, spec §7) — a filter over the emitted
//! colour, downstream of everything.
//!
//! §5.2 chose **accuracy in the model**: the blackbody became a band integral
//! rather than a midpoint sample, because a midpoint rule is 34 % wrong at
//! 1100 K and a hearth cannot afford that. The *look* is then recovered here,
//! where it belongs — the project's own spine (decision 0022: the sim emits
//! data, clients render), the same seam The Beholding's CLI colour lens and The
//! Idioms' Orrery render-style layer already occupy.
//!
//! # The four constraints, and where each is held
//!
//! 1. **One-way and downstream of `sense()`.** Nothing here takes a world, a
//!    ledger, a fact, an illuminant or a reflectance. [`apply`] takes three
//!    bytes and returns three bytes; there is no other entry point, so there is
//!    nothing for a feedback path to be written through.
//!    (`lantern_lens.rs::the_lens_never_touches_the_illuminant_or_the_reflectance`)
//! 2. **It transforms the emitted triple.** Brightening an illuminant changes
//!    the world; brightening an output changes the picture.
//! 3. **Disclosable and defeatable** (`RENDER-9`). [`Lens::Off`] is the exact
//!    identity, and it is what the CLI's `--lens off` selects; the drawn plan
//!    names the lens it drew through, so a reader is never shown a filtered
//!    picture without being told.
//! 4. **Lensed colour never lands in a committed artifact.** If it did, these
//!    constants would become a save-format-class contract — versioned and frozen
//!    like a seed label — for the sake of a look. So [`crate::plan::plan_of`]
//!    knows nothing about this module; only the terminal draw does.
//!
//! # Why this curve, and not a prettier one
//!
//! The obvious lens is a saturation or contrast boost. Both were refused by a
//! measurement the campaign already had in hand.
//!
//! **H1's tail.** Sweeping 1505 settlements across eight seeds, the median pair
//! of stone walls differs by 41 `u8` steps — but the **tenth percentile is 1**,
//! because settlements cluster on shared rock classes (all four of Task 6's
//! sampled flagships stand on Alluvium). Any transform whose slope drops below
//! 1 erases that decile outright, and the median goes on looking fine while it
//! happens. So the curve holds **slope ≥ 1 across the entire range the model
//! produces** — H1's real stone tops out at 217, the chamber's brightest floor
//! at 136 — and gives up separation only above [`SEPARATION_CEILING`], where
//! nothing the model emits has ever been seen to land.
//!
//! **The chamber's own band.** Task 6 measured walls from `[13, 13, 1]` to
//! `[88, 88, 73]`, floors to `[136, 136, 91]`, and a dimmest *visible* cell of
//! `[2, 2, 0]`. That is dark and narrow — and H4a's finding is that the chamber
//! never actually reaches black, so the room's legibility problem is not
//! contrast at the top but crushed detail at the bottom. The fix is therefore a
//! **shadow expansion**: below [`SHADOW_KNEE`] the curve is a gamma with slope
//! greater than 1, so a near-black wall at 13 is drawn at 24 rather than
//! staying invisible.
//!
//! **The two segments meet at slope 1**, which is what makes the whole thing one
//! curve rather than a kink: the shadow segment lifts the knee by
//! [`HIGHLIGHT_LIFT`], so its slope there is `(1 + LIFT/KNEE) * GAMMA`, and
//! setting that equal to the highlight segment's slope of 1 pins
//! [`SHADOW_GAMMA`] to `KNEE / (KNEE + LIFT)`. It is derived, not chosen, and a
//! guard asserts the relation so retuning one constant cannot silently break it.
//!
//! **No hue term, deliberately.** Seed 42's possessed bugbear is a *dichromat*:
//! its projection writes the same value into the red and green slots of every
//! triple. A warm tint — the obvious way to add "mood" — would hand that eye a
//! red/green distinction its own physiology never produced, which is a lie about
//! the observer rather than a filter over the picture. So this is **one scalar
//! curve applied identically to all three slots**, and the warmth stays where it
//! is already physical: in the 1900 K torch and the 1200 K hearth the model
//! actually lights the room with.

use hornvale_kernel::math;

/// Where the shadow expansion ends and the constant lift begins, in `u8`
/// steps.
///
/// Chosen to sit above the chamber's own dark band (walls run 13 to 88) so the
/// expansion covers the range that is actually crushed, and below H1's real
/// stone (107 and up) so the material distinctions the model produces travel
/// through the unity-slope segment untouched.
/// type-audit: bare-ok(render-internal)
pub const SHADOW_KNEE: u8 = 96;

/// How far the curve lifts everything above [`SHADOW_KNEE`], in `u8` steps.
///
/// This is the whole budget: every step spent lifting is a step of headroom
/// lost at the top, and the headroom is what keeps [`SEPARATION_CEILING`] clear
/// of the brightest colour the model emits (H1's 217). 24 steps leaves the
/// ceiling at 231 — 14 steps of margin over the brightest real stone measured,
/// and 95 over the brightest chamber floor.
/// type-audit: bare-ok(render-internal)
pub const HIGHLIGHT_LIFT: u8 = 24;

/// The exponent of the shadow segment.
///
/// **Derived, not tuned**: it is exactly `SHADOW_KNEE / (SHADOW_KNEE +
/// HIGHLIGHT_LIFT)`, the only value at which the shadow segment arrives at the
/// knee with slope 1 and therefore meets the constant lift without a kink — and
/// without a compressing stretch in the middle of the model's range. See this
/// module's own doc for the derivation, and
/// `lantern_lens.rs::the_two_segments_of_the_curve_meet_with_matching_slope`
/// for the guard that keeps the three constants consistent.
/// type-audit: bare-ok(ratio)
pub const SHADOW_GAMMA: f64 = 0.8;

/// The highest input value whose successor the lens can still be relied on to
/// draw differently.
///
/// Above this the constant lift has run out of headroom and the curve
/// saturates at 255, so distinctions there are lost. **This is the one place
/// the lens destroys information, and it is stated rather than hidden**: it sits
/// at 231, above every value the model has been measured to emit (H1's real
/// stone reaches 217; the chamber's brightest floor is 136). A future light
/// bright enough to push past it would need this constant re-derived, not the
/// guard relaxed.
/// type-audit: bare-ok(render-internal)
pub const SEPARATION_CEILING: u8 = 255 - HIGHLIGHT_LIFT;

/// A presentation filter over the colour the model emitted.
///
/// `Default` is [`Lens::Lantern`] — asking for a lens and getting the identity
/// would be a strange thing to hand back. Note that
/// [`crate::PossessOpts::lens`] defaults the other way, to [`Lens::Off`], and
/// its doc says why: a possession's output may be captured to an artifact, and
/// a filter must be opted into rather than out of at that boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Lens {
    /// No filter. The picture carries the model's own bytes — what every
    /// preregistered claim in spec §6 was measured on. This is the "unlensed
    /// mode" `RENDER-9` requires, and it is what makes the other variant a lens
    /// rather than a lie.
    Off,
    /// The lantern curve: shadows expanded, the model's own range left
    /// separable, warmth left to the physics. See this module's doc.
    #[default]
    Lantern,
}

impl Lens {
    /// The name the CLI takes and the drawn plan discloses.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(&self) -> &'static str {
        match self {
            Lens::Off => "off",
            Lens::Lantern => "lantern",
        }
    }

    /// Every lens a caller may name, in the order a refusal should list them.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn roster() -> [&'static str; 2] {
        [Lens::Off.label(), Lens::Lantern.label()]
    }

    /// The lens `name` selects, or `None` if there is no such lens.
    ///
    /// `None` rather than a fallback: generation never guesses (spec §4.6 of
    /// The Beholding, and the same posture `observer_named` takes), and quietly
    /// filtering a picture through a lens the caller did not ask for is the
    /// worst possible answer to a typo.
    /// type-audit: bare-ok(identifier-text: name)
    pub fn parse(name: &str) -> Option<Lens> {
        match name {
            "off" => Some(Lens::Off),
            "lantern" => Some(Lens::Lantern),
            _ => None,
        }
    }
}

/// One channel through the lantern curve.
///
/// Two segments, meeting at [`SHADOW_KNEE`] with matching slope:
///
/// - below the knee, a gamma that maps `[0, KNEE]` onto `[0, KNEE + LIFT]` —
///   slope strictly greater than 1 everywhere, so the chamber's crushed dark
///   end expands and nothing in it collapses;
/// - above it, a constant `+LIFT` — slope exactly 1, so every distinction the
///   model produced in its own range survives verbatim — saturating at 255
///   above [`SEPARATION_CEILING`].
///
/// `f(0) == 0` exactly: the black point is preserved, because "no light reaches
/// this cell" must stay drawable as black. Lifting the floor would make black
/// unrepresentable and quietly contradict H4.
fn curve(value: u8) -> u8 {
    let knee = f64::from(SHADOW_KNEE);
    let lifted = knee + f64::from(HIGHLIGHT_LIFT);
    let v = f64::from(value);
    let out = if value <= SHADOW_KNEE {
        lifted * math::powf(v / knee, SHADOW_GAMMA)
    } else {
        v + f64::from(HIGHLIGHT_LIFT)
    };
    out.round().clamp(0.0, 255.0) as u8
}

/// `rgb` seen through `lens`.
///
/// The whole of the lens's contact with the rest of the project: three bytes in,
/// three bytes out, no world-state on either side. [`Lens::Off`] is the exact
/// identity.
///
/// **One scalar curve, applied to all three slots identically.** That is what
/// keeps the lens honest about the observer: a dichromat emits triples whose red
/// and green slots are equal, and a per-channel term would give it a hue its eye
/// never made. See this module's doc.
/// type-audit: bare-ok(render-internal: rgb), bare-ok(render-internal: return)
pub fn apply(lens: &Lens, rgb: [u8; 3]) -> [u8; 3] {
    match lens {
        Lens::Off => rgb,
        Lens::Lantern => [curve(rgb[0]), curve(rgb[1]), curve(rgb[2])],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The black point is preserved exactly. An unlit cell draws black, and a
    /// lens that lifted the floor would make "no light reaches here"
    /// unrepresentable — quietly contradicting the model H4 was stated at.
    ///
    /// FIRES WHEN: the shadow segment gains an offset.
    #[test]
    fn the_lens_leaves_black_black() {
        assert_eq!(apply(&Lens::Lantern, [0, 0, 0]), [0, 0, 0]);
    }

    /// The curve rises everywhere it is not already at the ceiling — a lens
    /// that darkened part of the range while lifting the rest would reorder
    /// two cells' apparent brightness, which is a different picture, not a
    /// filtered one.
    ///
    /// FIRES WHEN: the two segments cross, or the gamma inverts.
    #[test]
    fn the_lens_never_darkens_a_cell() {
        for v in 0u8..=255 {
            let out = apply(&Lens::Lantern, [v, v, v])[0];
            assert!(
                out >= v,
                "the lens drew {v} as {out}, darker than the model emitted"
            );
        }
    }

    /// The knee is where the two segments meet, and they must agree there to
    /// the byte — a discontinuity would draw a visible band across a smoothly
    /// lit wall.
    ///
    /// FIRES WHEN: the constants stop satisfying `f(KNEE) == KNEE + LIFT`.
    #[test]
    fn the_segments_agree_at_the_knee() {
        assert_eq!(
            apply(&Lens::Lantern, [SHADOW_KNEE; 3])[0],
            SHADOW_KNEE + HIGHLIGHT_LIFT
        );
        assert_eq!(
            apply(&Lens::Lantern, [SHADOW_KNEE + 1; 3])[0],
            SHADOW_KNEE + 1 + HIGHLIGHT_LIFT
        );
    }

    /// A name the roster does not hold is refused, not guessed at.
    ///
    /// FIRES WHEN: `parse` gains a fallback.
    #[test]
    fn an_unknown_lens_name_is_refused() {
        assert_eq!(Lens::parse("lantern"), Some(Lens::Lantern));
        assert_eq!(Lens::parse("off"), Some(Lens::Off));
        assert_eq!(Lens::parse("warm"), None);
        assert_eq!(Lens::parse(""), None);
        for name in Lens::roster() {
            assert!(
                Lens::parse(name).is_some(),
                "the roster names {name}, which does not parse"
            );
        }
    }
}
