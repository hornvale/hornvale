//! Whether a host will answer, and how honestly.
//!
//! The two inputs are kept SEPARATE and are never pre-summed into one
//! disposition number (spec 3.5): the doctrine prior is what a people believes
//! a rider is, the fold is what this rider has actually done, and the case
//! worth reaching is the one where they disagree — a host that still calls you
//! by the warm word and still will not say where the water is.

use crate::doctrine::Openness;

/// What the host will do when asked about a drive. Ordered worst-last so a
/// test can assert monotonicity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stance {
    /// Answers from its own state.
    Forthcoming,
    /// Answers accurately, and volunteers the residue it would normally keep —
    /// truth chosen BECAUSE truth is what costs you.
    Costly,
    /// Names a state it is not in.
    Dissembling,
    /// Declines to name its state at all.
    Withholding,
}

/// How many overrides each prior tolerates before the stance worsens one step.
///
/// A people with apparatus spends its patience fastest. These are the only
/// three numbers in the campaign that are not read off a fact, and they are
/// thresholds on a derived count rather than authored dispositions — the
/// quantity itself is earned. Preregistered: the campaign reports whether the
/// prior moves observable testimony at all (spec section 5, H4).
///
/// Public since The Reticence, Task 6: the per-people report
/// (`hornvale_lab::render_reticence_report`) shows this threshold beside the
/// [`Openness`] it is keyed on, so a reader sees the number `stance_for`
/// actually uses rather than a second, hand-kept copy of it.
/// type-audit: bare-ok(count: return)
pub fn patience(prior: Openness) -> u32 {
    match prior {
        Openness::Guarded => 2,
        Openness::Wary => 4,
        Openness::Open => 8,
    }
}

/// The stance a host of this prior takes toward a drive it has been overridden
/// on `overrides` times. Zero overriding is always [`Stance::Forthcoming`]:
/// nothing about a culture's beliefs makes a host unhelpful before the rider
/// has done anything.
/// type-audit: bare-ok(count: overrides)
pub fn stance_for(prior: Openness, overrides: u32) -> Stance {
    let step = patience(prior);
    match overrides {
        0 => Stance::Forthcoming,
        n if n <= step => Stance::Costly,
        n if n <= step * 2 => Stance::Dissembling,
        _ => Stance::Withholding,
    }
}
