//! Deterministic, platform-independent float quantization. Rust's `f64`
//! transcendentals route to the platform libm (Apple's vs glibc's), which
//! differ in the last ULP; those differences reach committed golden files
//! and break byte-exact drift checks across platforms. `quantize` rounds a
//! float to a fixed number of significant decimal digits using Rust's
//! libm-free float formatting/parsing (identical on every platform), so two
//! values differing only by sub-quantum libm noise collapse to the same
//! `f64`. This is a save-format contract: changing [`QUANTIZE_SIG_DIGITS`]
//! silently re-bases every committed artifact.
//!
//! Quantization is applied only at serialization boundaries (the ledger on
//! commit, the lab CSV, scene/ephemeris JSON) — never in the compute path.
//! The noise fields, sculpting, and orbital mechanics all run at full
//! precision; nothing samples a quantized value back into an algorithm. The
//! lossless `u64` seed, not these floats, is the recoverable initial
//! condition (reload re-derives providers from the seed), so a lossy save
//! format never becomes chaotic initial conditions.

/// Significant decimal digits retained by [`quantize`]. Eight leaves ~7–8
/// digits of margin over 15–16-digit ULP noise (widening the safety margin
/// against future boundary-flip re-pins) while preserving far more physical
/// precision than a text sim observes — and quantization touches only
/// serialized outputs, never the compute path, so the digit count has zero
/// effect on simulation fidelity. Save-format constant.
/// type-audit: bare-ok(count)
pub const QUANTIZE_SIG_DIGITS: u32 = 8;

/// Round `x` to [`QUANTIZE_SIG_DIGITS`] significant digits, deterministically
/// and identically on every platform. Non-finite inputs pass through
/// unchanged (the ledger rejects them elsewhere).
/// type-audit: bare-ok(artifact)
pub fn quantize(x: f64) -> f64 {
    if !x.is_finite() {
        return x;
    }
    // `{:.*e}` uses core::fmt's Grisu/Dragon formatter (pure Rust, no libm);
    // parse uses core::num::dec2flt (Eisel-Lemire, also pure Rust). Both are
    // bit-for-bit identical across platforms for identical input bits.
    let precision = (QUANTIZE_SIG_DIGITS - 1) as usize;
    let s = format!("{x:.precision$e}");
    s.parse::<f64>()
        .expect("quantize: re-parse of a formatted finite f64 cannot fail")
}

/// serde `serialize_with` adapters that quantize on the way out, so artifact
/// structs stay full-precision in memory but serialize canonically.
pub mod quantize_serde {
    use super::quantize;
    use serde::{Serialize, Serializer};

    /// Quantize an `f64` field.
    /// type-audit: bare-ok(artifact)
    pub fn f64_field<S: Serializer>(x: &f64, s: S) -> Result<S::Ok, S::Error> {
        quantize(*x).serialize(s)
    }

    /// Quantize an `Option<f64>` field.
    /// type-audit: bare-ok(artifact)
    pub fn opt_f64_field<S: Serializer>(x: &Option<f64>, s: S) -> Result<S::Ok, S::Error> {
        x.map(quantize).serialize(s)
    }

    /// Quantize a `Vec<f64>` field.
    /// type-audit: bare-ok(artifact)
    pub fn vec_f64_field<S: Serializer>(x: &[f64], s: S) -> Result<S::Ok, S::Error> {
        x.iter()
            .copied()
            .map(quantize)
            .collect::<Vec<_>>()
            .serialize(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quantize_is_idempotent() {
        let v = 368.05357093462703_f64;
        assert_eq!(quantize(v), quantize(quantize(v)));
    }

    #[test]
    fn one_ulp_neighbors_collapse_to_the_same_value() {
        // The cross-platform property in miniature: two values differing by a
        // single ULP (as libm implementations do) quantize equal.
        let v = 210.2242156495795_f64;
        let up = f64::from_bits(v.to_bits() + 1);
        assert_ne!(v, up, "precondition: distinct bit patterns");
        assert_eq!(quantize(v), quantize(up));
    }

    #[test]
    fn retains_eight_significant_digits_across_magnitudes() {
        assert_eq!(quantize(0.041161436763_f64), 0.041161437);
        assert_eq!(quantize(4328.497184910964_f64), 4328.4972);
    }

    #[test]
    fn non_finite_and_zero_pass_through() {
        assert_eq!(quantize(0.0), 0.0);
        assert!(quantize(f64::NAN).is_nan());
        assert_eq!(quantize(f64::INFINITY), f64::INFINITY);
    }
}
