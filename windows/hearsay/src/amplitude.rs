//! How many of the teller's own generations one retelling spans.
//!
//! A story handed down across three generations blurs more than one handed
//! across half a generation. The unit is the TELLER's people's generation
//! length, which varies 6.75x across seed 42's peoples — so the same gap in
//! days is a different amplitude for an elven lineage than a gnoll one, and
//! two peoples end up remembering the same event at different resolutions
//! with no contact between them.
//!
//! The teller's people rather than the hearer's is a free choice: fission
//! never crosses a people boundary (campaign 2 §3.1, zero of 658 typed edges
//! and zero of 780 after main moved), so they are always the same.

use crate::durations::PeopleDurations;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};

fn number(led: &Ledger, occ: EntityId, pred: &str) -> Option<f64> {
    match led.value_of(occ, pred) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// The generational span of the retelling `teller -> hearer`.
///
/// Zero — not infinity, not a panic — when the founding days or the teller's
/// generation length cannot be derived. Total by construction, the same
/// posture `hornvale_history::descent::remove` takes toward a non-positive
/// generation length.
/// type-audit: bare-ok(count: return)
pub fn gen_span(
    led: &Ledger,
    durations: &PeopleDurations,
    teller: EntityId,
    hearer: EntityId,
) -> f64 {
    let Some(Value::Text(people)) = led.value_of(teller, hornvale_history::OCC_PEOPLE) else {
        return 0.0;
    };
    let (Some(generation), _) = durations.get(people) else {
        return 0.0;
    };
    let g = generation.get();
    if !g.is_finite() || g <= 0.0 {
        return 0.0;
    }
    let (Some(ft), Some(fh)) = (
        number(led, teller, hornvale_history::OCC_FOUNDED),
        number(led, hearer, hornvale_history::OCC_FOUNDED),
    ) else {
        return 0.0;
    };
    (fh - ft).abs() / g
}
