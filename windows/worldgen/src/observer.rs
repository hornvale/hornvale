//! The observer a species' perception implies (spec "The Beholding" §4.4).
//!
//! Lives beside [`crate::pack_depths`], which derives a perceptual gate from
//! the same [`PerceptionVector`] at the same seam and already carries the
//! authored model card this one extends: channel *count* is read off
//! `pack_depths`' own hue ladder, so the eye and the lexicon cannot disagree
//! by construction — a species with no word for green lacks the channel that
//! would distinguish it. The *degree* of merging within a tier is continuous
//! in `night_vision` (spec M1): a tiered eye derived from a tiered gate
//! cannot tell its own species apart, which is what the first candidate
//! model did for hobgoblin, bugbear and kobold before the merge fraction was
//! made continuous.
//!
//! **Determinism.** Building an observer is multiply-and-add over fixed-size
//! arrays plus one division per normalizer (kernel `sense`/`chromaticity`),
//! so it is bit-identical by IEEE-754 without routing through
//! [`hornvale_kernel::math`] — the same posture `kernel::color` itself takes.

use hornvale_kernel::color::{
    BANDS, ChannelRole, Illuminant, Observer, Projection, Reflectance, Spectrum, standard_observer,
};
use hornvale_species::{PerceptionVector, perception_registry};

/// The standard observer's four authored curves (short, medium, long,
/// scotopic), copied from `hornvale_kernel::color::standard_observer` —
/// which does not expose its channels — so this module can build merged
/// variants from the same source data. An edit to the kernel's curves must
/// be mirrored here; `the_human_row_derives_exactly_the_standard_observer`
/// (`windows/worldgen/tests/beholding_probe.rs`) pins the two against each
/// other end to end.
fn standard_curves() -> [[f64; BANDS]; 4] {
    [
        [0.00, 0.25, 1.00, 0.62, 0.10, 0.01, 0.00, 0.00, 0.00, 0.00], // short
        [0.00, 0.01, 0.10, 0.45, 0.90, 1.00, 0.72, 0.28, 0.05, 0.00], // medium
        [0.00, 0.01, 0.06, 0.25, 0.60, 0.92, 1.00, 0.75, 0.30, 0.06], // long
        [0.00, 0.15, 0.55, 0.95, 1.00, 0.68, 0.25, 0.05, 0.00, 0.00], // scotopic
    ]
}

/// Validate and wrap a band array as a [`Spectrum`]. Every curve this module
/// builds — copied, merged, or gain-scaled — stays finite by construction,
/// so the only way this panics is a logic error upstream.
fn curve(bands: [f64; BANDS]) -> Spectrum {
    Spectrum::new(bands).expect("a derived channel curve is always finite")
}

/// The scotopic channel's gain: `1.0 + 0.25 * (luminance - 1.0)`, spec
/// §4.4. `luminance` is `pack_depths`' coarse two-step switch (1 or 3), so
/// this is exactly 1.0 (no-op) for every species below the night-vision
/// midpoint and 1.5 above it. It exists so a later naming campaign has the
/// axis under low light; it can never move a colour because the scotopic
/// channel is always `Achromatic` — read by no projection and counted by no
/// chromaticity metric ([`hornvale_kernel::color::Observer::chromaticity`]).
fn scotopic_gain(p: &PerceptionVector) -> f64 {
    let depths = crate::pack_depths(p);
    1.0 + 0.25 * (depths.luminance as f64 - 1.0)
}

/// Assemble a derived (non-hue-5) observer: the last channel is always the
/// gain-scaled scotopic curve and is always [`ChannelRole::Achromatic`];
/// every other channel is [`ChannelRole::Chromatic`]. The projection's
/// `norms` are computed **per observer at construction** — spec §4.4 — by
/// sensing a unit reflectance under a unit illuminant on this exact channel
/// set and reading the channels `rgb` names, never carried literals. (Only
/// the standard observer's own `native` projection is exempt from that rule,
/// and the hue-5 arm of [`observer_for`] never reaches this helper.)
fn build(
    channels: Vec<Spectrum>,
    rgb: [usize; 3],
    name: &'static str,
    preserves: &'static str,
) -> Observer {
    let mut roles = vec![ChannelRole::Chromatic; channels.len()];
    let last = roles.len() - 1;
    roles[last] = ChannelRole::Achromatic;

    let unprojected = Observer::with_roles(channels.clone(), roles.clone(), None)
        .expect("roles are one-per-channel with at least one chromatic channel by construction");
    let white = Reflectance::new([1.0; BANDS]).expect("a unit reflectance is within [0, 1]");
    let flat = Illuminant::new([1.0; BANDS]).expect("a unit illuminant is non-negative");
    let signal = unprojected.sense(&white, &flat);
    let norms = [
        signal.get()[rgb[0]],
        signal.get()[rgb[1]],
        signal.get()[rgb[2]],
    ];
    let projection = Projection::new(name, preserves, rgb, norms)
        .expect("a live channel's unit-surface response is finite and non-zero");

    Observer::with_roles(channels, roles, Some(projection))
        .expect("the same construction validated above, now carrying its projection")
}

/// The observer a species' perception vector implies (spec §4.4's model
/// card, transcribed exactly):
///
/// ```text
/// hue 5  ->  channels [S, M, L, R]                     (identical to standard)
/// hue 4  ->  M' = (M + (M+L)/2) / 2                    both pulled halfway
///            L' = (L + (M+L)/2) / 2                    toward their mean
///            channels [S, M', L', R]
/// hue<=3 ->  t = clamp((night_vision - 0.5) / 0.5, 0, 1)
///            C = (1 - t)*L + t*(M + L)/2               one merged channel
///            channels [S, C, R]
/// ```
///
/// `observer_for` applied to human's row reproduces
/// [`hornvale_kernel::color::standard_observer`] **exactly**, byte for byte
/// (spec H2): the standard observer is a derived row, not a privileged base
/// case.
pub fn observer_for(p: &PerceptionVector) -> Observer {
    let depths = crate::pack_depths(p);
    let [short, medium, long, scotopic] = standard_curves();
    let gain = scotopic_gain(p);
    let mut rod = scotopic;
    for band in rod.iter_mut() {
        *band *= gain;
    }

    match depths.hue {
        5 => {
            // The full trichromat. Gain is always exactly 1.0 here — hue 5
            // requires night_vision below the luminance switch — so cloning
            // the kernel's own observer (curves, roles, and its `native`
            // projection, unchanged) holds H2 byte-for-byte rather than
            // merely numerically: recomputing the projection's norms live
            // would land one ULP off the carried constants (Task 1).
            standard_observer()
        }
        4 => {
            // Anomalous trichromat: medium and long each pulled halfway
            // toward their mean. Three chromatic channels remain; the
            // red-green axis narrows but is not removed.
            let mut m2 = [0.0; BANDS];
            let mut l2 = [0.0; BANDS];
            for b in 0..BANDS {
                let mean = (medium[b] + long[b]) / 2.0;
                m2[b] = (medium[b] + mean) / 2.0;
                l2[b] = (long[b] + mean) / 2.0;
            }
            build(
                vec![curve(short), curve(m2), curve(l2), curve(rod)],
                [2, 1, 0],
                "native-anomalous",
                "three chromatic channels; the red-green axis is narrowed, not removed",
            )
        }
        _ => {
            // Dichromat: medium and long merged into one channel. The merge
            // is CONTINUOUS in night_vision (spec M1) rather than keyed to
            // the hue tier, so two species that share a tier need not share
            // an eye — the fix for the first candidate model's collapse of
            // hobgoblin/bugbear/kobold onto one swatch set.
            let t = ((p.night_vision - 0.5) / 0.5).clamp(0.0, 1.0);
            let mut merged = [0.0; BANDS];
            for b in 0..BANDS {
                let mean = (medium[b] + long[b]) / 2.0;
                merged[b] = (1.0 - t) * long[b] + t * mean;
            }
            build(
                vec![curve(short), curve(merged), curve(rod)],
                [1, 1, 0],
                "yellow-blue",
                "the short-to-long opposition; the red-green axis is not carried",
            )
        }
    }
}

/// Word why this species sees as it does — the companion `perceptual_reason`
/// (beside [`crate::pack_depths`]) already does this for the lexicon.
/// type-audit: bare-ok(prose: return)
pub fn ocular_reason(p: &PerceptionVector) -> String {
    let depths = crate::pack_depths(p);
    let nv = p.night_vision;
    match depths.hue {
        5 => format!(
            "night-vision {nv} gives hue depth 5: the full trichromat, unmerged, \
             so every hue exemplar stays distinct"
        ),
        4 => format!(
            "night-vision {nv} gives hue depth 4: the medium and long channels are \
             pulled halfway together, so red and green narrow but do not vanish"
        ),
        depth => {
            // `(nv - 0.5) / 0.5` is subtraction-then-division on an
            // arbitrary authored `night_vision`, so it is not guaranteed to
            // land on a value with a short decimal expansion even when the
            // spec's model card states it as one (bugbear's 0.7 gives
            // 0.3999999999999999, not 0.4) — this is a player-visible
            // string (`eyes_report`, Session::set_eyes), so the fraction is
            // rounded for display; the merge itself still runs at full
            // precision above.
            let t = ((nv - 0.5) / 0.5).clamp(0.0, 1.0);
            format!(
                "night-vision {nv} gives hue depth {depth}: the medium and long \
                 channels are merged {t:.2} of the way, so red and green fall on one axis"
            )
        }
    }
}

/// Resolve `name` to the observer it names: `"standard"` for the kernel's
/// own [`standard_observer`], or a [`hornvale_species::KindId`] row of
/// [`perception_registry`]. `None` for anything else — Task 5's `eyes
/// <name>` verb calls this and lists [`observer_roster`] on a `None`, rather
/// than guessing (spec §4.6: "generation never guesses").
/// type-audit: bare-ok(identifier-text: name)
pub fn observer_named(name: &str) -> Option<Observer> {
    if name == "standard" {
        return Some(standard_observer());
    }
    perception_registry().get_by_label(name).map(observer_for)
}

/// Every name [`observer_named`] accepts, ascending — the roster Task 5's
/// `eyes` verb lists in its unknown-name error.
/// type-audit: bare-ok(identifier-text: return)
pub fn observer_roster() -> Vec<String> {
    let mut names: Vec<String> = vec!["standard".to_string()];
    names.extend(perception_registry().ids().map(|k| k.0.to_string()));
    names.sort();
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scotopic_gain_is_unity_below_the_luminance_switch() {
        // Every species below the night_vision midpoint has pack_depths
        // luminance == 1, so the gain must be an exact 1.0 no-op — this is
        // what lets the hue-5 arm of `observer_for` clone `standard_observer`
        // unchanged rather than needing to apply the gain itself.
        let p = PerceptionVector {
            night_vision: 0.15,
            ..PerceptionVector::MANIKIN
        };
        assert_eq!(scotopic_gain(&p), 1.0);
    }

    #[test]
    fn ocular_reason_rounds_the_merge_fraction_for_display() {
        // Bugbear's night_vision (0.7) is exactly the case that surfaced
        // this: `(0.7 - 0.5) / 0.5` is 0.3999999999999999 in f64, not the
        // spec's clean 0.4 — and `ocular_reason` is player-visible
        // (`eyes_report`, reachable through the `eyes` verb), so raw noise
        // must never reach the reader.
        let p = PerceptionVector {
            night_vision: 0.7,
            ..PerceptionVector::MANIKIN
        };
        let reason = ocular_reason(&p);
        assert!(
            reason.contains("0.40"),
            "expected the rounded fraction 0.40 in {reason:?}"
        );
        assert!(
            !reason.contains("0.3999999999999999"),
            "raw float noise reached a player-visible string: {reason:?}"
        );
    }

    #[test]
    fn observer_roster_is_sorted_ascending() {
        let roster = observer_roster();
        let mut sorted = roster.clone();
        sorted.sort();
        assert_eq!(roster, sorted);
    }
}
