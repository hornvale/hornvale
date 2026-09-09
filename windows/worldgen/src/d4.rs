//! Pure portfolio-regime vocabulary for the Staple D4 diagnostic.
//!
//! This module classifies observations supplied by callers. It neither reads
//! world state nor assigns occupations or specialization labels.

/// Two-type portfolio values, kept separate by observed mechanism.
/// type-audit: bare-ok(diagnostic-value: realized_output), bare-ok(diagnostic-value: voluntary_exchange), bare-ok(diagnostic-value: imports), bare-ok(diagnostic-value: shortfall), bare-ok(diagnostic-value: coercive_transfer), bare-ok(diagnostic-value: protection_access)
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct D4PortfolioVector {
    /// Locally realized production or extraction.
    pub realized_output: [f64; 2],
    /// Voluntary outward exchange.
    pub voluntary_exchange: [f64; 2],
    /// Recurring inward exchange or dependency.
    pub imports: [f64; 2],
    /// Demand left unmet.
    pub shortfall: [f64; 2],
    /// Tribute or another imposed transfer.
    pub coercive_transfer: [f64; 2],
    /// Access mediated by protection or patronage.
    pub protection_access: [f64; 2],
}

impl D4PortfolioVector {
    /// An observed vector containing no activity.
    pub const fn zero() -> Self {
        Self {
            realized_output: [0.0; 2],
            voluntary_exchange: [0.0; 2],
            imports: [0.0; 2],
            shortfall: [0.0; 2],
            coercive_transfer: [0.0; 2],
            protection_access: [0.0; 2],
        }
    }

    fn values(self) -> [f64; 12] {
        [
            self.realized_output[0],
            self.realized_output[1],
            self.voluntary_exchange[0],
            self.voluntary_exchange[1],
            self.imports[0],
            self.imports[1],
            self.shortfall[0],
            self.shortfall[1],
            self.coercive_transfer[0],
            self.coercive_transfer[1],
            self.protection_access[0],
            self.protection_access[1],
        ]
    }
}

/// Why an explanatory or mechanism axis cannot currently be observed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4AxisDebt {
    /// No joined opportunity field exists.
    Source,
    /// No settlement path consumes the opportunity.
    Consumer,
    /// Communities cannot differ in exploitation capability.
    Capability,
    /// Movement or relations cannot transmit the opportunity.
    Access,
    /// Comparable recurrence cannot be observed.
    Temporal,
    /// Only labels or aggregate totals are available.
    Mechanism,
    /// Voluntary and coercive realization cannot be separated.
    Causal,
}

/// Whether a channel was observed, distinct from an observed zero.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4Availability {
    /// The source channel was observed.
    Available,
    /// The source channel was unavailable for the stated reason.
    Debt(D4AxisDebt),
}

/// Availability of each independently typed portfolio mechanism.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct D4MechanismAvailability {
    /// Availability of realized output.
    pub realized_output: D4Availability,
    /// Availability of voluntary exchange.
    pub voluntary_exchange: D4Availability,
    /// Availability of imports.
    pub imports: D4Availability,
    /// Availability of shortfall.
    pub shortfall: D4Availability,
    /// Availability of coercive transfer.
    pub coercive_transfer: D4Availability,
    /// Availability of protection access.
    pub protection_access: D4Availability,
}

impl D4MechanismAvailability {
    /// Availability for a fully observed profile.
    pub const fn all_available() -> Self {
        Self {
            realized_output: D4Availability::Available,
            voluntary_exchange: D4Availability::Available,
            imports: D4Availability::Available,
            shortfall: D4Availability::Available,
            coercive_transfer: D4Availability::Available,
            protection_access: D4Availability::Available,
        }
    }

    fn is_complete(self) -> bool {
        [
            self.realized_output,
            self.voluntary_exchange,
            self.imports,
            self.shortfall,
            self.coercive_transfer,
            self.protection_access,
        ]
        .iter()
        .all(|availability| *availability == D4Availability::Available)
    }
}

/// Whether a profile has a complete, evidentiary phase window.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4Completeness {
    /// The window or one of its required channels is incomplete.
    Incomplete,
    /// Every required channel was observed in a complete phase window.
    Complete,
}

/// Invalid numeric input to a D4 profile helper.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4ProfileError {
    /// At least one value was NaN or infinite.
    NonFiniteQuantity,
    /// A non-negative source channel contained a negative value.
    NegativeQuantity,
}

/// A phase-specific portfolio reading; never a specialization label.
/// type-audit: bare-ok(index: phase)
#[derive(Clone, Debug, PartialEq)]
pub struct D4PortfolioProfile {
    /// Unmodified observed values.
    pub raw: D4PortfolioVector,
    /// Composition normalized across all typed channels, absent for zero or
    /// unavailable evidence.
    pub normalized: Option<D4PortfolioVector>,
    /// Caller-supplied phase identity.
    pub phase: u16,
    /// Whether this window can contribute evidence.
    pub completeness: D4Completeness,
    /// Explicit channel availability and debt.
    pub mechanism_availability: D4MechanismAvailability,
}

impl D4PortfolioProfile {
    /// Construct a profile from a caller-identified complete phase window.
    ///
    /// Missing mechanisms downgrade the reading to incomplete rather than
    /// treating their numeric slots as observed zeroes.
    /// type-audit: bare-ok(index: phase)
    pub fn complete(
        phase: u16,
        raw: D4PortfolioVector,
        mechanism_availability: D4MechanismAvailability,
    ) -> Result<Self, D4ProfileError> {
        let normalized = d4_normalize_profile(&raw)?;
        let completeness = if normalized.is_some() && mechanism_availability.is_complete() {
            D4Completeness::Complete
        } else {
            D4Completeness::Incomplete
        };
        Ok(Self {
            raw,
            normalized,
            phase,
            completeness,
            mechanism_availability,
        })
    }
}

/// Normalized typed composition used for exact pure comparisons.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct D4ProfileSignature {
    /// Normalized channels in their original typed structure.
    pub composition: D4PortfolioVector,
}

/// Temporal recurrence of an ordered sequence of complete profiles.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4RecurrenceClass {
    /// No complete evidentiary window exists.
    Incomplete,
    /// Only one complete window exists.
    Transient,
    /// A profile recurs in the same phase.
    Seasonal,
    /// Profiles return to an earlier structure after changing.
    Rotating,
    /// Profiles change without returning to the initial structure.
    Drifting,
    /// Stable structure recurs across distinct phases.
    PersistentCandidate,
}

/// Mechanism interpretation kept separate from recurrence and role labels.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4MechanismClass {
    /// Realization includes voluntary exchange and no coercive transfer.
    Voluntary,
    /// Voluntary and coercive mechanisms both contribute.
    Mixed,
    /// Coercive transfer contributes without voluntary exchange.
    CoerciveOnly,
    /// Causal mechanism separation is unavailable.
    Unavailable,
}

/// Already-computed per-seed facts consumed by the verdict helper.
/// type-audit: bare-ok(count: adequate_communities), bare-ok(count: distinct_regimes), bare-ok(flag: source_or_access_varies), bare-ok(flag: differentiation_is_vacuous), bare-ok(flag: some_units_underpowered)
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct D4RegimeEvidence {
    /// Count of adequately observed communities.
    pub adequate_communities: usize,
    /// Count of materially distinct recurring regimes.
    pub distinct_regimes: usize,
    /// Whether joined source or access observations vary.
    pub source_or_access_varies: bool,
    /// Whether differentiation is explained only by a preregistered vacuity.
    pub differentiation_is_vacuous: bool,
    /// Temporal classification of the observed profiles.
    pub recurrence: D4RecurrenceClass,
    /// Whether the seed mixes adequate evidence with incomplete units.
    pub some_units_underpowered: bool,
}

/// Per-seed D4 falsifier branch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum D4RegimeVerdict {
    /// Adequate profiles are materially flat.
    NoRealizedDifferentiation,
    /// Opportunity varies but normalized portfolio structure does not.
    ProjectionCollapse,
    /// Differences arise only from a preregistered vacuity.
    VacuousDifferentiation,
    /// Profiles differ but do not recur.
    TransientContrast,
    /// Distinct profiles recur in the same phase.
    SeasonalRegime,
    /// Distinct profiles recur across phases; specialization remains deferred.
    PersistentCandidate,
    /// Evidence is mixed, incomplete, or insufficiently plural.
    MixedOrUnderpowered,
}

/// Validate and normalize all typed channels by their shared total.
///
/// `Ok(None)` identifies an observed all-zero vector as non-evidentiary.
pub fn d4_normalize_profile(
    raw: &D4PortfolioVector,
) -> Result<Option<D4PortfolioVector>, D4ProfileError> {
    let values = raw.values();
    if values.iter().any(|value| !value.is_finite()) {
        return Err(D4ProfileError::NonFiniteQuantity);
    }
    if values.iter().any(|value| *value < 0.0) {
        return Err(D4ProfileError::NegativeQuantity);
    }
    let total: f64 = values.iter().sum();
    if !total.is_finite() {
        return Err(D4ProfileError::NonFiniteQuantity);
    }
    if total == 0.0 {
        return Ok(None);
    }
    let scale = |pair: [f64; 2]| [pair[0] / total, pair[1] / total];
    Ok(Some(D4PortfolioVector {
        realized_output: scale(raw.realized_output),
        voluntary_exchange: scale(raw.voluntary_exchange),
        imports: scale(raw.imports),
        shortfall: scale(raw.shortfall),
        coercive_transfer: scale(raw.coercive_transfer),
        protection_access: scale(raw.protection_access),
    }))
}

/// Return the typed composition signature of complete evidence.
pub fn d4_profile_signature(profile: &D4PortfolioProfile) -> Option<D4ProfileSignature> {
    (profile.completeness == D4Completeness::Complete)
        .then_some(profile.normalized)
        .flatten()
        .map(|composition| D4ProfileSignature { composition })
}

/// Classify recurrence in an ordered sequence without averaging phases away.
pub fn d4_recurrence_class(profiles: &[D4PortfolioProfile]) -> D4RecurrenceClass {
    let signatures: Option<Vec<_>> = profiles.iter().map(d4_profile_signature).collect();
    let Some(signatures) = signatures else {
        return D4RecurrenceClass::Incomplete;
    };
    if signatures.is_empty() {
        return D4RecurrenceClass::Incomplete;
    }
    if signatures.len() == 1 {
        return D4RecurrenceClass::Transient;
    }
    // A repeated phase cycle is seasonal even when the cycle contains more
    // than one distinct profile. Phase identity remains part of the cycle;
    // this is not an all-window average or a cross-phase relabeling.
    for period in 1..=(signatures.len() / 2) {
        if signatures.len() >= period * 2
            && (period * 2..signatures.len()).all(|index| {
                signatures[index] == signatures[index % period]
                    && profiles[index].phase == profiles[index % period].phase
            })
            && (period..(period * 2)).all(|index| {
                signatures[index] == signatures[index % period]
                    && profiles[index].phase == profiles[index % period].phase
            })
        {
            return D4RecurrenceClass::Seasonal;
        }
    }
    if signatures
        .iter()
        .all(|signature| *signature == signatures[0])
    {
        return if profiles
            .iter()
            .all(|profile| profile.phase == profiles[0].phase)
        {
            D4RecurrenceClass::Seasonal
        } else {
            D4RecurrenceClass::PersistentCandidate
        };
    }
    if signatures.len() > 2 && signatures.first() == signatures.last() {
        D4RecurrenceClass::Rotating
    } else {
        D4RecurrenceClass::Drifting
    }
}

/// Select the preregistered per-seed verdict from already-computed evidence.
pub fn d4_regime_verdict(evidence: D4RegimeEvidence) -> D4RegimeVerdict {
    if evidence.adequate_communities < 2 || evidence.some_units_underpowered {
        return D4RegimeVerdict::MixedOrUnderpowered;
    }
    if evidence.distinct_regimes == 0 {
        return if evidence.source_or_access_varies {
            D4RegimeVerdict::ProjectionCollapse
        } else {
            D4RegimeVerdict::NoRealizedDifferentiation
        };
    }
    if evidence.differentiation_is_vacuous {
        return D4RegimeVerdict::VacuousDifferentiation;
    }
    if evidence.distinct_regimes < 2 {
        return D4RegimeVerdict::MixedOrUnderpowered;
    }
    match evidence.recurrence {
        D4RecurrenceClass::Transient | D4RecurrenceClass::Drifting => {
            D4RegimeVerdict::TransientContrast
        }
        D4RecurrenceClass::Seasonal => D4RegimeVerdict::SeasonalRegime,
        D4RecurrenceClass::PersistentCandidate => D4RegimeVerdict::PersistentCandidate,
        D4RecurrenceClass::Incomplete | D4RecurrenceClass::Rotating => {
            D4RegimeVerdict::MixedOrUnderpowered
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vector(realized_output: [f64; 2]) -> D4PortfolioVector {
        D4PortfolioVector {
            realized_output,
            voluntary_exchange: [0.0; 2],
            imports: [0.0; 2],
            shortfall: [0.0; 2],
            coercive_transfer: [0.0; 2],
            protection_access: [0.0; 2],
        }
    }

    fn profile(phase: u16, raw: D4PortfolioVector) -> D4PortfolioProfile {
        D4PortfolioProfile::complete(phase, raw, D4MechanismAvailability::all_available()).unwrap()
    }

    #[test]
    fn signatures_preserve_dependencies_and_typed_channels() {
        // Break caught: folding imports, shortfalls, or coercion into another
        // channel makes structurally different portfolios compare equal.
        let baseline = profile(0, vector([4.0, 2.0]));

        let mut imported = vector([4.0, 2.0]);
        imported.imports = [0.0, 3.0];
        let imported = profile(0, imported);

        let mut short = vector([4.0, 2.0]);
        short.shortfall = [0.0, 3.0];
        let short = profile(0, short);

        let mut coerced = vector([4.0, 2.0]);
        coerced.coercive_transfer = [3.0, 0.0];
        let coerced = profile(0, coerced);

        assert_ne!(
            d4_profile_signature(&baseline),
            d4_profile_signature(&imported)
        );
        assert_ne!(
            d4_profile_signature(&imported),
            d4_profile_signature(&short)
        );
        assert_ne!(
            d4_profile_signature(&coerced),
            d4_profile_signature(&baseline)
        );
        assert_eq!(coerced.raw.voluntary_exchange, [0.0; 2]);
    }

    #[test]
    fn normalization_is_non_evidentiary_for_zero_and_preserves_raw_values() {
        // Break caught: normalization mutates evidence or turns an all-zero
        // observation into a usable profile.
        let raw = vector([3.0, 1.0]);
        let normalized = d4_normalize_profile(&raw).unwrap().unwrap();
        assert_eq!(raw.realized_output, [3.0, 1.0]);
        assert_eq!(normalized.realized_output, [0.75, 0.25]);
        assert_eq!(
            d4_normalize_profile(&D4PortfolioVector::zero()).unwrap(),
            None
        );
    }

    #[test]
    fn normalization_rejects_non_finite_inputs_and_finite_sum_overflow() {
        // Break caught: non-finite values, or a non-finite total accumulated
        // from finite values, reach division and fabricate a zero composition.
        for invalid in [f64::NAN, f64::INFINITY] {
            assert_eq!(
                d4_normalize_profile(&vector([invalid, 0.0])),
                Err(D4ProfileError::NonFiniteQuantity)
            );
        }

        assert_eq!(
            d4_normalize_profile(&vector([f64::MAX, f64::MAX])),
            Err(D4ProfileError::NonFiniteQuantity)
        );
    }

    #[test]
    fn invalid_and_unavailable_observations_are_explicit() {
        // Break caught: malformed quantities or unavailable mechanisms are
        // silently represented as observed zeroes.
        let mut negative = vector([1.0, 0.0]);
        negative.imports[0] = -1.0;
        assert_eq!(
            d4_normalize_profile(&negative),
            Err(D4ProfileError::NegativeQuantity)
        );

        let availability = D4MechanismAvailability {
            realized_output: D4Availability::Debt(D4AxisDebt::Mechanism),
            ..D4MechanismAvailability::all_available()
        };
        let unavailable =
            D4PortfolioProfile::complete(0, vector([1.0, 0.0]), availability).unwrap();
        assert_eq!(unavailable.completeness, D4Completeness::Incomplete);
        assert_eq!(d4_profile_signature(&unavailable), None);
    }

    #[test]
    fn recurrence_retains_phase_and_distinguishes_rotation_from_drift() {
        // Break caught: one window or an all-window average is allowed to
        // masquerade as recurrence, erasing phase structure.
        let a0 = profile(0, vector([4.0, 1.0]));
        let a1 = profile(1, vector([4.0, 1.0]));
        let a0_again = profile(0, vector([4.0, 1.0]));
        let b = profile(1, vector([1.0, 4.0]));
        let middle = profile(1, vector([2.0, 3.0]));

        assert_eq!(d4_recurrence_class(&[]), D4RecurrenceClass::Incomplete);
        assert_eq!(
            d4_recurrence_class(std::slice::from_ref(&a0)),
            D4RecurrenceClass::Transient
        );
        assert_eq!(
            d4_recurrence_class(&[a0.clone(), a0_again]),
            D4RecurrenceClass::Seasonal
        );
        assert_eq!(
            d4_recurrence_class(&[a0.clone(), a1]),
            D4RecurrenceClass::PersistentCandidate
        );
        assert_eq!(
            d4_recurrence_class(&[a0.clone(), b.clone(), a0.clone(), b.clone()]),
            D4RecurrenceClass::Seasonal
        );
        assert_eq!(
            d4_recurrence_class(&[a0.clone(), b.clone(), a0.clone(), b.clone(), a0.clone()]),
            D4RecurrenceClass::Seasonal
        );
        assert_eq!(
            d4_recurrence_class(&[a0.clone(), b.clone(), a0.clone()]),
            D4RecurrenceClass::Rotating
        );
        assert_eq!(
            d4_recurrence_class(&[a0, middle, b]),
            D4RecurrenceClass::Drifting
        );
    }

    #[test]
    fn verdict_covers_falsifiers_and_requires_multiple_regimes() {
        // Break caught: flat, collapsed, vacuous, or singleton evidence can
        // enter a positive regime branch.
        let base = D4RegimeEvidence {
            adequate_communities: 3,
            distinct_regimes: 0,
            source_or_access_varies: false,
            differentiation_is_vacuous: false,
            recurrence: D4RecurrenceClass::Transient,
            some_units_underpowered: false,
        };
        assert_eq!(
            d4_regime_verdict(base),
            D4RegimeVerdict::NoRealizedDifferentiation
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                source_or_access_varies: true,
                ..base
            }),
            D4RegimeVerdict::ProjectionCollapse
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                distinct_regimes: 2,
                differentiation_is_vacuous: true,
                ..base
            }),
            D4RegimeVerdict::VacuousDifferentiation
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                distinct_regimes: 2,
                ..base
            }),
            D4RegimeVerdict::TransientContrast
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                distinct_regimes: 2,
                recurrence: D4RecurrenceClass::Seasonal,
                ..base
            }),
            D4RegimeVerdict::SeasonalRegime
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                distinct_regimes: 2,
                recurrence: D4RecurrenceClass::PersistentCandidate,
                ..base
            }),
            D4RegimeVerdict::PersistentCandidate
        );
        assert_eq!(
            d4_regime_verdict(D4RegimeEvidence {
                adequate_communities: 2,
                distinct_regimes: 1,
                recurrence: D4RecurrenceClass::PersistentCandidate,
                ..base
            }),
            D4RegimeVerdict::MixedOrUnderpowered
        );
    }
}
