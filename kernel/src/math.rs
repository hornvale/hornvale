//! Portable transcendental math: every `f64` transcendental in the
//! workspace routes through here (decision 0028).
//!
//! Rust's inherent `f64` transcendentals (`x.sin()`, `x.powf(y)`, …)
//! dispatch to the *platform* libm — Apple's on macOS, glibc's on Linux —
//! which differ in the last ULP. Routing every call through the pure-Rust
//! `libm` crate instead makes the compute path itself bit-identical on
//! every platform, not just the quantized serialization boundary.
//!
//! Only transcendentals live here. IEEE 754 requires exact results for
//! `sqrt`, `abs`, `floor`/`ceil`/`round`, `mul_add`, and arithmetic, so
//! those inherent methods remain allowed everywhere.

/// Portable sine (radians).
/// type-audit: bare-ok(ratio)
#[inline]
pub fn sin(x: f64) -> f64 {
    libm::sin(x)
}

/// Portable cosine (radians).
/// type-audit: bare-ok(ratio)
#[inline]
pub fn cos(x: f64) -> f64 {
    libm::cos(x)
}

/// Portable tangent (radians).
/// type-audit: bare-ok(ratio)
#[inline]
pub fn tan(x: f64) -> f64 {
    libm::tan(x)
}

/// Portable arcsine: result in [-pi/2, pi/2].
/// type-audit: bare-ok(ratio)
#[inline]
pub fn asin(x: f64) -> f64 {
    libm::asin(x)
}

/// Portable arccosine: result in [0, pi].
/// type-audit: bare-ok(ratio)
#[inline]
pub fn acos(x: f64) -> f64 {
    libm::acos(x)
}

/// Portable arctangent: result in [-pi/2, pi/2].
/// type-audit: bare-ok(ratio)
#[inline]
pub fn atan(x: f64) -> f64 {
    libm::atan(x)
}

/// Portable four-quadrant arctangent of `y/x`: result in [-pi, pi].
/// type-audit: bare-ok(ratio)
#[inline]
pub fn atan2(y: f64, x: f64) -> f64 {
    libm::atan2(y, x)
}

/// Portable natural exponential.
/// type-audit: bare-ok(ratio)
#[inline]
pub fn exp(x: f64) -> f64 {
    libm::exp(x)
}

/// Portable natural logarithm.
/// type-audit: bare-ok(ratio)
#[inline]
pub fn ln(x: f64) -> f64 {
    libm::log(x)
}

/// Portable base-10 logarithm.
/// type-audit: bare-ok(ratio)
#[inline]
pub fn log10(x: f64) -> f64 {
    libm::log10(x)
}

/// Portable floating-point power `x^y`.
/// type-audit: bare-ok(ratio)
#[inline]
pub fn powf(x: f64, y: f64) -> f64 {
    libm::pow(x, y)
}

/// Portable hyperbolic tangent.
/// type-audit: bare-ok(ratio)
#[inline]
pub fn tanh(x: f64) -> f64 {
    libm::tanh(x)
}

#[cfg(test)]
// The comparison target here IS the platform libm — the one place std's
// transcendentals are legitimately called.
#[allow(clippy::disallowed_methods)]
mod tests {
    use super::*;

    /// libm agrees with the platform to within 1 ULP on a smoke sample
    /// (the whole point is that "within 1 ULP" is not "equal").
    #[test]
    fn smoke_close_to_std() {
        for i in 0..100 {
            let x = f64::from(i) * 0.13 - 6.5;
            assert!((sin(x) - x.sin()).abs() <= x.sin().abs().max(1e-300) * 1e-15);
            assert!((exp(x) - x.exp()).abs() <= x.exp() * 1e-15);
        }
    }

    /// Exact identities that must hold bit-for-bit on every platform.
    #[test]
    fn exact_anchor_values() {
        assert_eq!(sin(0.0), 0.0);
        assert_eq!(cos(0.0), 1.0);
        assert_eq!(exp(0.0), 1.0);
        assert_eq!(ln(1.0), 0.0);
        assert_eq!(powf(2.0, 10.0), 1024.0);
        assert_eq!(atan2(0.0, 1.0), 0.0);
    }
}
