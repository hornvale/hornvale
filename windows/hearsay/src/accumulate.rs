//! How damage accumulates, and how a width becomes a rung.
//!
//! THREE RULES, NO PRIMARY. Spec §6.4: substrate for all three was measured
//! before the rule was chosen, so nominating one would be selection on data
//! already seen. All three are implemented, all three are reported, and
//! adopting one is a separate dated decision citing this campaign's numbers.
//! Deliberately no `Default` impl — a default would nominate one silently.

use crate::ladder::PrecisionLadder;
use hornvale_kernel::Precision;

/// How a retelling's damage combines with the damage already carried.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Accumulation {
    /// Damage sums. The naive baseline; assumes perfectly correlated errors.
    Additive,
    /// Damage sums in quadrature — the standard rule for INDEPENDENT
    /// contributions.
    Quadrature,
    /// Width grows by a factor, which is additive in log space and therefore
    /// the rule commensurate with an approximately geometric ladder.
    Multiplicative,
}

impl Accumulation {
    /// Every rule, in a fixed order so a readout's columns are stable.
    pub const ALL: [Accumulation; 3] = [
        Accumulation::Additive,
        Accumulation::Quadrature,
        Accumulation::Multiplicative,
    ];

    /// This rule's short name, used as a metric-name suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Accumulation::Additive => "additive",
            Accumulation::Quadrature => "quadrature",
            Accumulation::Multiplicative => "multiplicative",
        }
    }

    /// The width after one retelling of generational span `span`.
    ///
    /// Non-decreasing for every rule and every non-negative `span`, which is
    /// what preserves campaign 2's precision-rank monotonicity: width only
    /// grows, so the rung index only rises.
    /// type-audit: bare-ok(count: width), bare-ok(count: span), bare-ok(count: return)
    pub fn step(self, width: f64, span: f64) -> f64 {
        if !span.is_finite() || span <= 0.0 {
            return width;
        }
        match self {
            Accumulation::Additive => width + span,
            Accumulation::Quadrature => (width * width + span * span).sqrt(),
            Accumulation::Multiplicative => width * (1.0 + span),
        }
    }
}

/// The coarsest rung whose span does not exceed `width`.
///
/// This is the project's quantize-at-emit-only discipline applied to a
/// non-float quantity: the width is carried at full resolution and the rung
/// is resolved when the claim is READ. Saturates at the ladder's coarsest
/// rung rather than running off the end, and returns `FINEST` for an empty
/// ladder, matching `PrecisionLadder::coarser`'s posture.
/// type-audit: bare-ok(count: width)
pub fn precision_at(ladder: &PrecisionLadder, width: f64) -> Precision {
    if ladder.is_empty() {
        return Precision::FINEST;
    }
    let mut out = Precision::FINEST;
    for i in 0..ladder.len() {
        let p = Precision(i as u8);
        match ladder.span(p) {
            Some(span) if span.get() <= width => out = p,
            _ => break,
        }
    }
    out
}
