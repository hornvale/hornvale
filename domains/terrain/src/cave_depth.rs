//! A cave's depth budget, in metres (The Underworld, spec §4.0).
//!
//! **Why this module exists.** Until The Underworld a cave's depth was a
//! [`crate::strata::BandKind`] — a *name from the stratigraphic ladder*, not a
//! quantity — and the function that chose it read the same `proneness` scalar
//! the presence gate reads. Two failures followed from that one shape:
//!
//! - **The depth coordinate was two-valued.** `top_depth_m(deepest_band)` is
//!   `dtb = soil + sediment` (~0 m over most of the world) or `roots_top ≈
//!   moho/2` (~14 km). Task 1 measured the consequence: ΔT above the surface
//!   datum, which is that depth times a gradient varying only 1.27×, put 62–75%
//!   of caves in `[0, 2) K` and 24–38% in `[50, ∞) K`, with 1–25 caves out of
//!   874–1681 in between. Any monotone function of a two-valued input is
//!   two-valued; re-spacing the rungs could not populate a hole no chamber's
//!   depth ever landed in.
//! - **Presence and depth were welded** (`MAP-cave-depth-weld`). They want
//!   opposite calibrations: raising `Fracture`'s proneness to give it a fair
//!   share of caves also drove every one of them a band deeper. The weld is cut
//!   here by the shape of the signature — [`cave_depth_reach_m`] does not take
//!   a proneness and so cannot re-form it.
//!
//! **What replaces it.** A budget in metres, derived purely from fields terrain
//! already owns (`MaterialBuffer`, `StratigraphicColumn`) in the manner of
//! [`crate::strata::geothermal_gradient`]. Pure means no stream draw, so this
//! module cannot perturb stream consumption order — a save-format contract.
//! [`crate::features::cave_depth`] survives, and becomes a *lookup* of the
//! metre reach against the column's own band tops, so the archival question
//! ("which bands does this void penetrate") keeps a correct answer while no
//! longer being the depth coordinate itself.
//!
//! **The unifying physics is void closure under lithostatic load.** A void
//! survives to the depth where the weight of the overburden exceeds what the
//! surrounding rock mass can carry around an opening; below that it creeps,
//! spalls and shuts. That gives `z = S / (ρ g)` from a single rock-strength
//! axis, and rock strength is the quantity in this whole problem that genuinely
//! spans orders of magnitude — which is why the derivation spreads where a band
//! index could not.
//!
//! **`z = S / (ρ g)` is a first-order UPPER BOUND on void survival, not the
//! closure depth**, and the module says so rather than letting the arithmetic
//! imply otherwise. It compares rock strength against the *far-field*
//! lithostatic stress, and stress does not stay far-field at the wall of an
//! opening: the Kirsch solution puts the tangential stress around a circular
//! hole at roughly **2× the far field** (3× at the springline for a hydrostatic
//! field). A void therefore fails at some fraction of the depth this expression
//! names. Applying that factor is a deliberate deferral, not an oversight — it
//! would roughly halve every reach, move the whole distribution, and needs its
//! own calibration review — so what the budget rests on today is the **relative
//! ordering** of competences, which the factor does not change, plus the Earth
//! anchors below acting as a check that the absolute scale is not absurd.
//!
//! Each kind then states what *limits* it below that mechanical ceiling:
//! dissolution needs soluble rock, a tube is a near-surface primary void, and a
//! fault void is the reference case. The Earth anchors in spec §4.0 (lava tubes
//! tens to a few hundred metres; karst to ~2.2 km; fracture voids closing
//! within a few km) are used as sanity ceilings on the result, never as the
//! derivation.

use crate::features::CaveKind;
use crate::lithology::MaterialBuffer;
use crate::strata::StratigraphicColumn;
use hornvale_kernel::math;

/// Rock-mass strength (MPa) at `induration = 0` — the weakest rock the axis
/// describes. ISRM grade R1, "very weak rock": crumbles under firm blows of a
/// geological hammer, peelable by a knife. Poorly consolidated tuff, mudstone
/// and chalk sit here.
const STRENGTH_MIN_MPA: f64 = 1.0;

/// Rock-mass strength (MPa) at `induration = 1`. The ISRM R4/R5 boundary,
/// "strong rock". Deliberately **not** the intact-core maximum (quartzite and
/// gneiss reach 250 MPa in a laboratory): `induration` is a rock-*mass* field
/// by construction — [`crate::lithology::induration_at`] builds it from crust
/// age and orogenic overprint at a ~110 km cell — and a jointed mass carries a
/// fraction of intact strength. Applying a Hoek–Brown rock-mass reduction on
/// top of this scale would double-count that fraction, so the scale absorbs it
/// once, here.
const STRENGTH_MAX_MPA: f64 = 100.0;

/// Why the strength scale is **logarithmic** in `induration` rather than
/// linear: rock strength classifications are themselves log-spaced. The ISRM
/// field grades run 0.25–1, 1–5, 5–25, 25–50, 50–100, 100–250 MPa — a constant
/// ratio per grade, because that is how strength actually distributes across
/// rock types. A linear map from `induration` onto [`STRENGTH_MIN_MPA`] …
/// [`STRENGTH_MAX_MPA`] would put 90% of the range above 10 MPa and lose the
/// weak half of the scale entirely. This constant is that span as a ratio, kept
/// as a named value so the two endpoints and the interpolation cannot drift
/// apart.
const STRENGTH_SPAN: f64 = STRENGTH_MAX_MPA / STRENGTH_MIN_MPA;

/// Lithostatic stress gradient, MPa per metre of depth: `ρ g` for a mean upper
/// crustal density of 2700 kg/m³ at 9.81 m/s² = 26.5 kPa/m. The standard
/// crustal average; not tuned.
const LITHOSTATIC_MPA_PER_M: f64 = 0.026_5;

/// Absolute ceiling (m) on any cave's depth budget — a rail, not a target.
/// Spec §4.0 states the window the delve ladder covers as 0–3 km, and Earth's
/// deepest known cave system (Veryovkina, 2212 m) sits inside it.
///
/// **It binds on 37 of 48,316 caves (0.077%) over 30 worlds**
/// (`hollow_readout`'s `at-ceiling` figure), which makes it a guarantee about
/// the function's range rather than something that shapes the distribution.
/// The three probe seeds alone showed a maximum of 2732 m and no binding at
/// all; the 30-world figure is the one to quote.
///
/// **Which cases bind, since the obvious guess is wrong.** Not the closure
/// term running away — that reaches 3774 m only at `induration = 1`, and the
/// highest induration in any measured world is 0.9292, giving 2732 m. The
/// 37 are **paleokarst**: [`PALEOKARST_GAIN`] applied to a well-indurated
/// carbonate, which clamps once `closure * 0.7 * 1.6 > 3000`, i.e. above
/// `induration ≈ 0.924` — just inside the karst maximum of 0.9268. So the
/// ceiling's only live customer is the one multiplier in this module that is
/// authored rather than derived.
///
/// **Public because it is the function's declared range**, and a consumer that
/// wants to partition the budget must read the range from here rather than
/// duplicate the literal — `hollow_readout`'s restated H2 bins against it.
/// type-audit: bare-ok(diagnostic-value)
pub const CAVE_REACH_CEILING_M: f64 = 3000.0;

/// Ceiling (m) on a lava tube's depth below the surface. A tube is a *primary*
/// void — the drained interior of a single flow unit, and flow units are metres
/// to tens of metres thick — so its depth below ground is however deeply later
/// flows and sediment have buried it, not how deep it could have grown.
/// Terrestrial tubes are a near-surface phenomenon: Kazumura, the longest
/// mapped, lies within tens of metres of the ground over 65 km. 200 m is the
/// generous end of spec §4.0's "tens to a few hundred metres" anchor.
const LAVATUBE_CEILING_M: f64 = 200.0;

/// Multiplier on a karst system's reach where the column records an
/// unconformity. A nonconformity in this model is thin young cover directly on
/// ancient basement — on Earth, the classic *paleokarst* setting: a dissolution
/// system that formed, was buried, and is now back in circulation. It has had
/// two erosional cycles to deepen rather than one. **A fidelity choice, stated
/// as one:** two full cycles would imply 2.0, and this is deliberately below
/// that because a buried karst is partly infilled with sediment and collapse
/// breccia, so only part of the inherited void is recoverable. It carries
/// forward the `column.unconformity` branch of the retired band-returning
/// `cave_depth`, which made the same claim with a band instead of a number.
const PALEOKARST_GAIN: f64 = 1.6;

/// A first-order **upper bound** (m) on the depth at which lithostatic load
/// closes a void in rock of this competence: `z = S / (ρ g)`, with `S`
/// interpolated log-linearly across [`STRENGTH_SPAN`] (see that constant for
/// why the scale is logarithmic).
///
/// **Not the closure depth, and the name is shorthand.** The expression sets
/// rock strength against the *far-field* lithostatic stress, omitting the
/// stress concentration at the wall of the opening — the Kirsch solution gives
/// roughly 2× the far field around a circular hole, so a real void closes
/// shallower than this by something of that order. The factor is deliberately
/// not applied yet (it would halve every reach and needs its own calibration
/// review); the module doc records that deferral. What survives the omission,
/// and what the budget actually rests on, is the **ordering**: a rock twice as
/// competent still holds a void deeper, by the same ratio, factor or no factor.
///
/// This is the shared half of every kind's budget — what the *rock* permits,
/// before the process's own limit applies.
fn closure_depth_m(induration: f64) -> f64 {
    let competence = induration.clamp(0.0, 1.0);
    let strength_mpa = STRENGTH_MIN_MPA * math::powf(STRENGTH_SPAN, competence);
    strength_mpa / LITHOSTATIC_MPA_PER_M
}

/// A cave's depth budget in metres below the surface: how far down the void
/// this process opened actually reaches at this cell.
///
/// **It does not take a proneness, and that absence is the point** (spec §4.0,
/// `MAP-cave-depth-weld`). Presence is gated on
/// [`crate::lithology::cave_proneness`] × belt × noise; depth is this. The one
/// field both read is `carbonate`, which is unavoidable — a karst cave needs
/// soluble rock both to exist and to deepen — and `porosity` is left to the
/// presence gate alone rather than counted twice.
///
/// A pure function of the two fields terrain already owns, so it draws nothing
/// and cannot perturb stream consumption order.
///
/// Per kind, on top of [`closure_depth_m`]:
///
/// - **Karst.** Dissolution can only remove the soluble fraction of the rock,
///   so the mechanically-available depth is scaled by `carbonate`. **The
///   direction is physical; the proportionality is authored.** That reach
///   should rise with solubility follows from the process; that it should rise
///   *linearly*, so a rock half carbonate reaches exactly half as deep, is a
///   modelling choice with no more backing than [`PALEOKARST_GAIN`] has — no
///   published scale sets it, and a sub-linear law would be equally
///   defensible. It is the second authored number in this module, and the
///   first one that does not say so at its own definition, because it has no
///   definition to say it at.
/// - **LavaTube.** The tube is a near-surface void inside the flow that drained
///   out of it, capped at [`LAVATUBE_CEILING_M`]. Note the cap is *not* the
///   column's `depth_to_basement_m`: that measures soil plus **sedimentary**
///   cover, and a basalt flow is neither.
/// - **Fracture.** The reference case, with no reduction. A fault void is
///   nothing but an aperture held open against confining stress, so
///   [`closure_depth_m`] *is* its budget — the fault supplies the opening for
///   free. (Which means the fracture arm inherits that function's bound
///   directly, undamped by any other term: it is where the omitted
///   stress-concentration factor would bite hardest.)
///
/// type-audit: bare-ok(diagnostic-value: return)
pub fn cave_depth_reach_m(
    kind: CaveKind,
    buf: &MaterialBuffer,
    column: &StratigraphicColumn,
) -> f64 {
    let closure = closure_depth_m(buf.induration);
    let reach = match kind {
        CaveKind::Karst => {
            let inherited = if column.unconformity {
                PALEOKARST_GAIN
            } else {
                1.0
            };
            closure * buf.carbonate.clamp(0.0, 1.0) * inherited
        }
        CaveKind::LavaTube => closure.min(LAVATUBE_CEILING_M),
        CaveKind::Fracture => closure,
    };
    reach.clamp(0.0, CAVE_REACH_CEILING_M)
}

#[cfg(test)]
mod tests {
    use crate::features::CaveKind;
    use crate::lithology::{Basement, MarginPolarity, MaterialBuffer, SoilDepth};

    /// A carbonate-rich, porous, moderately indurated buffer.
    fn karstic() -> MaterialBuffer {
        MaterialBuffer {
            silica: 0.5,
            grain: 0.75,
            induration: 0.6,
            carbonate: 0.7,
            metamorphic_grade: 0.0,
            porosity: 0.8,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(1.0),
            basement: Basement::Continental,
            thaumic: 0.0,
        }
    }

    /// A mafic, fine-grained, young-flow buffer.
    fn basaltic() -> MaterialBuffer {
        MaterialBuffer {
            silica: 0.15,
            grain: 0.2,
            induration: 0.39,
            carbonate: 0.05,
            metamorphic_grade: 0.0,
            porosity: 0.374,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(0.0),
            basement: Basement::Oceanic,
            thaumic: 0.0,
        }
    }

    #[test]
    fn a_lava_tube_is_shallower_than_a_karst_system() {
        let col = crate::strata::column(
            40.0,
            0.5,
            true,
            300.0,
            5.0,
            crate::lithology::RockClass::ReefLimestone,
            crate::lithology::Basement::Continental,
        );
        let tube = super::cave_depth_reach_m(CaveKind::LavaTube, &basaltic(), &col);
        let karst = super::cave_depth_reach_m(CaveKind::Karst, &karstic(), &col);
        assert!(tube < karst, "tube={tube} karst={karst}");
    }

    #[test]
    fn karst_reach_grows_with_carbonate() {
        let col = crate::strata::column(
            40.0,
            0.5,
            true,
            300.0,
            5.0,
            crate::lithology::RockClass::ReefLimestone,
            crate::lithology::Basement::Continental,
        );
        let mut poor = karstic();
        poor.carbonate = 0.1;
        let mut rich = karstic();
        rich.carbonate = 0.9;
        let a = super::cave_depth_reach_m(CaveKind::Karst, &poor, &col);
        let b = super::cave_depth_reach_m(CaveKind::Karst, &rich, &col);
        assert!(b > a, "carbonate 0.9 gave {b}, carbonate 0.1 gave {a}");
    }

    #[test]
    fn every_reach_is_finite_non_negative_and_under_the_ceiling() {
        // Total over the input domain, and inside the window the delve ladder
        // covers. 3500 m is the assertion's absurd-HIGH bound, not a target.
        let col = crate::strata::column(
            40.0,
            0.5,
            true,
            300.0,
            5.0,
            crate::lithology::RockClass::ReefLimestone,
            crate::lithology::Basement::Continental,
        );
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            for buf in [karstic(), basaltic()] {
                let d = super::cave_depth_reach_m(kind, &buf, &col);
                assert!(d.is_finite(), "{kind:?} gave a non-finite reach");
                assert!((0.0..=3500.0).contains(&d), "{kind:?} gave {d} m");
            }
        }
    }

    #[test]
    fn the_derived_band_agrees_with_the_columns_own_boundaries() {
        // `cave_depth` must now be a LOOKUP of the metre reach against the
        // column, so the two can never disagree.
        let col = crate::strata::column(
            40.0,
            0.5,
            true,
            300.0,
            5.0,
            crate::lithology::RockClass::ReefLimestone,
            crate::lithology::Basement::Continental,
        );
        for kind in [CaveKind::Karst, CaveKind::LavaTube, CaveKind::Fracture] {
            let reach = super::cave_depth_reach_m(kind, &karstic(), &col);
            let band = crate::features::cave_depth(kind, &col, &karstic());
            let idx = match band {
                crate::strata::BandKind::Regolith => 0,
                crate::strata::BandKind::Cover => 1,
                crate::strata::BandKind::Basement => 2,
                crate::strata::BandKind::Roots => 3,
                crate::strata::BandKind::Underneath => 4,
            };
            assert!(
                col.bands[idx].top_depth_m <= reach,
                "{kind:?}: band {band:?} starts at {} m but the reach is {reach} m",
                col.bands[idx].top_depth_m
            );
        }
    }
}
