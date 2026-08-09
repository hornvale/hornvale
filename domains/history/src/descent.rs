//! The pure descent arithmetic (The Namesake, spec §3.1).
//!
//! The ledger commits a community tree — `occ-founded-from` links a daughter
//! occupation to the mother it was settled from. It does **not** commit a
//! genealogy, and the two are not the same: seed 42's founding gaps run to a
//! median of 50 years and a maximum of 975, which no lifespan in the roster
//! supports as a parent-child link. What the edge encodes is *descent at an
//! unknown remove*, and this module derives the remove.
//!
//! Everything here is a total function of its arguments — no world, no
//! ledger, no `Stream` draw. The generation length arrives as a plain `f64`
//! because this crate is kernel-only and cannot read `hornvale-species`; the
//! composition root resolves it and passes it in, the same discipline
//! `MorphOptions` follows in `domains/language`.

use crate::flesh::RoleHandle;
use hornvale_kernel::Seed;

/// How two founders on either end of one `occ-founded-from` edge are related.
/// type-audit: bare-ok(count: Ancestor.0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kinship {
    /// The same generation: the daughter was founded within half a
    /// generation of its mother, so its founder is a sibling rather than a
    /// descendant. Measured at 13% of seed-42's edges.
    Sibling,
    /// A descendant at this many generations' remove (always `>= 1`).
    Ancestor(u32),
}

/// The number of generations between two founders, given the years between
/// their communities' foundings and the people's generation length.
///
/// Rounds to nearest, so a gap under half a generation is zero. Total by
/// construction: a non-positive or non-finite `generation_length_years`
/// (an `Ametabolic` kind has no generation length at all) and a negative
/// `gap_years` both yield `0` rather than a panic, a NaN, or a `u32`
/// underflow.
/// type-audit: bare-ok(count: gap_years), bare-ok(count: generation_length_years), bare-ok(count: return)
pub fn remove(gap_years: f64, generation_length_years: f64) -> u32 {
    if !gap_years.is_finite()
        || !generation_length_years.is_finite()
        || generation_length_years <= 0.0
        || gap_years <= 0.0
    {
        return 0;
    }
    // `floor` stays intrinsic per the kernel's math discipline; this value
    // never feeds a draw, only a count.
    let generations = (gap_years / generation_length_years + 0.5).floor();
    if generations <= 0.0 {
        0
    } else if generations >= f64::from(u32::MAX) {
        u32::MAX
    } else {
        generations as u32
    }
}

/// [`remove`], read as a relationship.
/// type-audit: bare-ok(count: gap_years), bare-ok(count: generation_length_years)
pub fn kinship(gap_years: f64, generation_length_years: f64) -> Kinship {
    match remove(gap_years, generation_length_years) {
        0 => Kinship::Sibling,
        n => Kinship::Ancestor(n),
    }
}

/// The handle of the figure `steps` generations before `of`.
///
/// **Reserved and currently unconsumed.** Nothing in the workspace calls this
/// outside its own tests: The Namesake specified and hardened the lazy walk
/// but never wired it, so no shipped name resolves through it yet. Recorded
/// here rather than left to be inferred from a caller search — an unconsumed
/// public function is indistinguishable from a forgotten one six months on.
///
/// A lazy walk: the intermediate ancestors a long remove implies are never
/// materialised as records, only as handles, exactly as [`RoleHandle`]'s own
/// documentation intends ("a record can reference many unnamed roles without
/// ever materializing them until something actually observes one"). Seed 42's
/// median remove is 2 and its maximum 32, so the walk is short in practice.
///
/// `steps == 0` returns `of` unchanged — a figure is their own zeroth
/// ancestor — which is what makes [`Kinship::Sibling`] resolve to a shared
/// forebear without a special case at the call site.
///
/// The mix is the same splitmix-style arithmetic [`crate::flesh::persona_of`]
/// uses, iterated, with the step counter folded into each round so the walk
/// is not the same fixed permutation applied `steps` times: a fixed
/// permutation iterated has fixed points, and `(RoleHandle(0), Seed(0))` was
/// one — every step collapsed onto the same handle, and Seed(0) is a
/// reachable world seed. In practice, distinctness across a chain is
/// empirically verified for the seeds and lengths this crate's tests probe
/// (including the all-zero degenerate case and the deepest measured chain,
/// 32 steps), not proven for every input; no `Stream` is drawn, so it
/// consumes no draws and touches no stream-consumption-order contract.
/// type-audit: bare-ok(count: steps)
pub fn ancestor(of: RoleHandle, steps: u32, seed: Seed) -> RoleHandle {
    let mut h = of.0;
    for k in 0..steps {
        // `k + 1` (never zero, even at the first round) times an odd
        // constant is the perturbation. A plain `u64::from(k)` still leaves
        // the k = 0 round's term at zero, and `mix(0) == 0` (multiplying and
        // xor-shifting zero is a no-op), so the very first round would still
        // fix `(RoleHandle(0), Seed(0))` at zero — the defect this exists to
        // close.
        let mut x = h ^ seed.0 ^ (u64::from(k) + 1).wrapping_mul(0x9E37_79B9_7F4A_7C15);
        x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
        x ^= x >> 29;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 32;
        h = x;
    }
    RoleHandle(h)
}
