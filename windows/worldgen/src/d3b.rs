//! Pure source and projection signatures for the Staple D3B diagnostic.
//!
//! These helpers classify already-computed continuous observations. They do
//! not quantize, derive roles, or decide whether a set of observations is
//! saturated. Callers retain the raw coverage/shortfall vectors and the
//! per-resource exchange counters from `DiagnosticSubsistenceWitness`; those
//! counters are the separate access companion, not part of a source or
//! projection signature.

/// A three-way source band whose endpoint meaning is supplied by the axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum D3bTernaryBand {
    /// The axis's lower band or lower endpoint.
    Low,
    /// The axis's open interior or middle interval.
    Middle,
    /// The axis's upper band or upper endpoint.
    High,
}

/// Existing settlement-scale bands applied to local carrying capacity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum D3bCapacityBand {
    /// Capacity below the existing hamlet scale of 150.
    BelowHamlet,
    /// Capacity from 150 inclusive to the longhouse scale of 200 exclusive.
    HamletToLonghouse,
    /// Capacity at or above the existing longhouse scale of 200.
    AtLeastLonghouse,
}

/// Structural bands on the existing clamped coverage domain.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum D3bCoverageBand {
    /// Exactly zero demand coverage.
    None,
    /// Coverage strictly between zero and one.
    Partial,
    /// Exactly full demand coverage.
    Full,
}

/// Exact ordering of phase-integrated typed coverage A and B.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum D3bOrdering {
    /// A coverage is strictly less than B coverage.
    ALessThanB,
    /// A coverage equals B coverage exactly.
    Equal,
    /// A coverage is strictly greater than B coverage.
    AGreaterThanB,
}

/// The joint signature of the three eligible D3B source axes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct D3bSourceSignature {
    /// Local surplus under the existing culture structure gates.
    pub surplus: D3bTernaryBand,
    /// River access under the existing proximity endpoint semantics.
    pub river_access: D3bTernaryBand,
    /// Per-people local carrying capacity at settlement scales.
    pub capacity: D3bCapacityBand,
}

/// The structural part of one phase-integrated typed projection observation.
///
/// Raw coverage and shortfall remain on the diagnostic witness so within-band
/// spread remains measurable. This signature deliberately contains no
/// saturation verdict and no exchange-access counters.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct D3bProjectionSignature {
    /// Structural coverage bands ordered `[A, B]`.
    pub coverage: [D3bCoverageBand; 2],
    /// Exact ordering of the raw phase-integrated A/B coverage components.
    pub ordering: D3bOrdering,
}

fn unit_interval(value: f64) -> bool {
    value.is_finite() && (0.0..=1.0).contains(&value)
}

/// Classify local surplus using the culture structure gates `<= 0.4`,
/// `(0.4, 0.6]`, and `> 0.6`.
///
/// Returns `None` when the input is non-finite or outside the surplus ratio's
/// unit interval; an invalid observation is not silently assigned a band.
/// type-audit: bare-ok(ratio: surplus)
pub fn d3b_surplus_band(surplus: f64) -> Option<D3bTernaryBand> {
    if !unit_interval(surplus) {
        return None;
    }
    Some(if surplus <= 0.4 {
        D3bTernaryBand::Low
    } else if surplus <= 0.6 {
        D3bTernaryBand::Middle
    } else {
        D3bTernaryBand::High
    })
}

/// Classify river proximity as exact zero, open interior, or exact one.
///
/// Returns `None` when the input is non-finite or outside the proximity
/// domain `[0, 1]`.
/// type-audit: bare-ok(ratio: river_access)
pub fn d3b_river_band(river_access: f64) -> Option<D3bTernaryBand> {
    if !unit_interval(river_access) {
        return None;
    }
    Some(if river_access == 0.0 {
        D3bTernaryBand::Low
    } else if river_access == 1.0 {
        D3bTernaryBand::High
    } else {
        D3bTernaryBand::Middle
    })
}

/// Classify non-negative local capacity as `< 150`, `[150, 200)`, or
/// `>= 200` using the existing settlement scales.
///
/// Returns `None` for negative or non-finite capacity.
/// type-audit: bare-ok(count: capacity)
pub fn d3b_capacity_band(capacity: f64) -> Option<D3bCapacityBand> {
    if !capacity.is_finite() || capacity < 0.0 {
        return None;
    }
    Some(if capacity < 150.0 {
        D3bCapacityBand::BelowHamlet
    } else if capacity < 200.0 {
        D3bCapacityBand::HamletToLonghouse
    } else {
        D3bCapacityBand::AtLeastLonghouse
    })
}

/// Build one source signature from continuous surplus, river proximity, and
/// per-people local capacity.
///
/// Returns `None` if any source axis is invalid. No biome, subsistence,
/// function, or specialization label participates.
/// type-audit: bare-ok(ratio: surplus), bare-ok(ratio: river_access), bare-ok(count: capacity)
pub fn d3b_source_signature(
    surplus: f64,
    river_access: f64,
    capacity: f64,
) -> Option<D3bSourceSignature> {
    Some(D3bSourceSignature {
        surplus: d3b_surplus_band(surplus)?,
        river_access: d3b_river_band(river_access)?,
        capacity: d3b_capacity_band(capacity)?,
    })
}

fn coverage_band(coverage: f64) -> Option<D3bCoverageBand> {
    if !unit_interval(coverage) {
        return None;
    }
    Some(if coverage == 0.0 {
        D3bCoverageBand::None
    } else if coverage == 1.0 {
        D3bCoverageBand::Full
    } else {
        D3bCoverageBand::Partial
    })
}

/// Build the structural signature for raw phase-integrated coverage `[A, B]`.
///
/// The helper performs exact endpoint and ordering comparisons: it introduces
/// neither quantization nor a tolerance bucket. It returns `None` if either
/// component is non-finite or outside the clamped coverage domain. Callers
/// retain this raw coverage vector, its complementary shortfall vector, and
/// the witness's typed access counters for measurement and vacuity checks.
/// type-audit: bare-ok(ratio: coverage)
pub fn d3b_projection_signature(coverage: [f64; 2]) -> Option<D3bProjectionSignature> {
    let bands = [coverage_band(coverage[0])?, coverage_band(coverage[1])?];
    let ordering = if coverage[0] < coverage[1] {
        D3bOrdering::ALessThanB
    } else if coverage[0] > coverage[1] {
        D3bOrdering::AGreaterThanB
    } else {
        D3bOrdering::Equal
    };
    Some(D3bProjectionSignature {
        coverage: bands,
        ordering,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn surplus_bands_keep_both_existing_upper_endpoints_in_the_lower_band() {
        assert_eq!(d3b_surplus_band(0.0), Some(D3bTernaryBand::Low));
        assert_eq!(d3b_surplus_band(0.4), Some(D3bTernaryBand::Low));
        assert_eq!(
            d3b_surplus_band(f64::from_bits(0.4f64.to_bits() + 1)),
            Some(D3bTernaryBand::Middle)
        );
        assert_eq!(d3b_surplus_band(0.6), Some(D3bTernaryBand::Middle));
        assert_eq!(
            d3b_surplus_band(f64::from_bits(0.6f64.to_bits() + 1)),
            Some(D3bTernaryBand::High)
        );
        assert_eq!(d3b_surplus_band(1.0), Some(D3bTernaryBand::High));
    }

    #[test]
    fn river_bands_distinguish_exact_endpoints_from_every_interior_value() {
        assert_eq!(d3b_river_band(0.0), Some(D3bTernaryBand::Low));
        assert_eq!(
            d3b_river_band(f64::MIN_POSITIVE),
            Some(D3bTernaryBand::Middle)
        );
        assert_eq!(d3b_river_band(0.5), Some(D3bTernaryBand::Middle));
        assert_eq!(
            d3b_river_band(f64::from_bits(1.0f64.to_bits() - 1)),
            Some(D3bTernaryBand::Middle)
        );
        assert_eq!(d3b_river_band(1.0), Some(D3bTernaryBand::High));
    }

    #[test]
    fn capacity_bands_keep_150_and_200_on_their_named_lower_endpoints() {
        assert_eq!(
            d3b_capacity_band(149.999_999),
            Some(D3bCapacityBand::BelowHamlet)
        );
        assert_eq!(
            d3b_capacity_band(150.0),
            Some(D3bCapacityBand::HamletToLonghouse)
        );
        assert_eq!(
            d3b_capacity_band(199.999_999),
            Some(D3bCapacityBand::HamletToLonghouse)
        );
        assert_eq!(
            d3b_capacity_band(200.0),
            Some(D3bCapacityBand::AtLeastLonghouse)
        );
    }

    #[test]
    fn source_signature_combines_only_the_three_continuous_source_axes() {
        assert_eq!(
            d3b_source_signature(0.4, 0.5, 200.0),
            Some(D3bSourceSignature {
                surplus: D3bTernaryBand::Low,
                river_access: D3bTernaryBand::Middle,
                capacity: D3bCapacityBand::AtLeastLonghouse,
            })
        );
    }

    #[test]
    fn projection_signature_uses_coverage_endpoints_and_exact_typed_order() {
        assert_eq!(
            d3b_projection_signature([0.0, 1.0]),
            Some(D3bProjectionSignature {
                coverage: [D3bCoverageBand::None, D3bCoverageBand::Full],
                ordering: D3bOrdering::ALessThanB,
            })
        );
        assert_eq!(
            d3b_projection_signature([0.5, 0.5]),
            Some(D3bProjectionSignature {
                coverage: [D3bCoverageBand::Partial, D3bCoverageBand::Partial],
                ordering: D3bOrdering::Equal,
            })
        );
        assert_eq!(
            d3b_projection_signature([1.0, 0.0]),
            Some(D3bProjectionSignature {
                coverage: [D3bCoverageBand::Full, D3bCoverageBand::None],
                ordering: D3bOrdering::AGreaterThanB,
            })
        );
    }

    #[test]
    fn projection_ordering_has_no_tolerance_bucket() {
        let next_after_half = f64::from_bits(0.5f64.to_bits() + 1);
        let signature = d3b_projection_signature([0.5, next_after_half])
            .expect("both adjacent values are valid coverage ratios");

        assert_eq!(
            signature.coverage,
            [D3bCoverageBand::Partial, D3bCoverageBand::Partial]
        );
        assert_eq!(signature.ordering, D3bOrdering::ALessThanB);
    }

    #[test]
    fn helpers_reject_non_finite_and_out_of_domain_values() {
        for invalid in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            assert_eq!(d3b_surplus_band(invalid), None);
            assert_eq!(d3b_river_band(invalid), None);
            assert_eq!(d3b_capacity_band(invalid), None);
            assert_eq!(d3b_projection_signature([invalid, 0.5]), None);
            assert_eq!(d3b_projection_signature([0.5, invalid]), None);
        }

        assert_eq!(d3b_surplus_band(-0.1), None);
        assert_eq!(d3b_surplus_band(1.1), None);
        assert_eq!(d3b_river_band(-0.1), None);
        assert_eq!(d3b_river_band(1.1), None);
        assert_eq!(d3b_capacity_band(-0.1), None);
        assert_eq!(d3b_projection_signature([-0.1, 0.5]), None);
        assert_eq!(d3b_projection_signature([0.5, 1.1]), None);
    }
}
