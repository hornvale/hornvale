//! The open resource-axis basis and [`ResourceVector`]: the authored
//! ecological primitive the coexistence packer derives from.
//!
//! A [`ResourceVector`] is a sparse utilization profile over a registered
//! set of [`ResourceAxis`] members — the "what a population eats, and how
//! much it leans on each axis" niche description. Pianka symmetric niche
//! overlap ([`ResourceVector::overlap`]) turns two such profiles into a
//! single competition coefficient in `[0, 1]`.
//!
//! The basis is open: [`v1_basis`] enumerates the axes registered so far,
//! but a [`ResourceVector`] does not validate its axis ids against it —
//! callers build vectors from named [`ResourceAxis`] constants, so an
//! unregistered id can only appear by deliberately constructing one.

use std::collections::{BTreeMap, BTreeSet};

use crate::units::UnitError;

/// Whether a resource axis is ambient (undepleted by consumption) or a
/// depletable stock (consumption draws the pool down and drives the
/// trophic cap).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceKind {
    /// Ambient, undepleted resource — e.g. photosynthate or mana. Many
    /// consumers can draw on the same axis without exhausting it.
    Field,
    /// Depletable resource — e.g. standing prey or detritus. Consumption
    /// draws down a finite stock, which is what drives the trophic cap.
    Stock,
}

/// A registered member of the open resource-axis basis: one dimension a
/// [`ResourceVector`] can carry a weight on.
/// type-audit: bare-ok(index: id), bare-ok(identifier-text: label)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResourceAxis {
    /// Stable numeric id — the key a [`ResourceVector`] stores this axis's
    /// weight under. Stable across a save's lifetime (a save-format
    /// contract, per the kernel's determinism rules): renumbering an axis
    /// id would silently reinterpret every world's `ResourceVector`s.
    pub id: u16,
    /// Human-readable label for diagnostics and the almanac.
    pub label: &'static str,
    /// Whether this axis is ambient or depletable.
    pub kind: ResourceKind,
}

/// Ambient solar (or magical) energy fixing — the base of the food web,
/// never depleted by consumption.
pub const PHOTOSYNTHATE: ResourceAxis = ResourceAxis {
    id: 0,
    label: "photosynthate",
    kind: ResourceKind::Field,
};

/// Standing plant biomass available as forage.
pub const PLANT_FORAGE: ResourceAxis = ResourceAxis {
    id: 1,
    label: "plant forage",
    kind: ResourceKind::Stock,
};

/// Animal prey biomass.
pub const ANIMAL_PREY: ResourceAxis = ResourceAxis {
    id: 2,
    label: "animal prey",
    kind: ResourceKind::Stock,
};

/// Dead organic matter available to decomposers and scavengers.
pub const DETRITUS: ResourceAxis = ResourceAxis {
    id: 3,
    label: "detritus",
    kind: ResourceKind::Stock,
};

/// Mineral reserves (soil/rock nutrients; the aquatic mineral axis is
/// reserved for a later basis extension).
pub const MINERAL: ResourceAxis = ResourceAxis {
    id: 4,
    label: "mineral",
    kind: ResourceKind::Stock,
};

/// The v1 resource-axis basis, in ascending id order. The basis is open —
/// later campaigns may register further axes with higher ids — so this
/// slice is a snapshot of what's registered today, not a closed enum.
pub fn v1_basis() -> &'static [ResourceAxis] {
    &[PHOTOSYNTHATE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS, MINERAL]
}

/// A sparse resource-utilization vector: axis id to non-negative weight.
/// The zero vector (no axes, or every weight zero) is legal — it means "no
/// recorded niche," not an error — and overlaps nothing (see
/// [`ResourceVector::overlap`]).
#[derive(Debug, Clone, PartialEq)]
pub struct ResourceVector(BTreeMap<u16, f64>);

impl ResourceVector {
    /// Validating constructor: rejects any non-finite or negative weight.
    /// An empty slice is legal and produces the zero vector. Repeated axis
    /// ids overwrite rather than sum (last write wins), matching the
    /// map-like semantics of the sparse representation.
    /// type-audit: bare-ok(constructor-edge: weights)
    pub fn new(weights: &[(ResourceAxis, f64)]) -> Result<Self, UnitError> {
        let mut map = BTreeMap::new();
        for (axis, weight) in weights {
            if !weight.is_finite() {
                return Err(UnitError {
                    unit: "resource weight",
                    value: *weight,
                    reason: "must be finite",
                });
            }
            if *weight < 0.0 {
                return Err(UnitError {
                    unit: "resource weight",
                    value: *weight,
                    reason: "must not be negative",
                });
            }
            map.insert(axis.id, *weight);
        }
        Ok(Self(map))
    }

    /// The weight recorded on `axis`, or `0.0` if this vector doesn't carry
    /// one.
    /// type-audit: bare-ok(ratio: return)
    pub fn weight(&self, axis: ResourceAxis) -> f64 {
        self.0.get(&axis.id).copied().unwrap_or(0.0)
    }

    /// True if every recorded weight is zero (including the empty vector).
    /// type-audit: bare-ok(flag: return)
    pub fn is_zero(&self) -> bool {
        self.0.values().all(|weight| *weight == 0.0)
    }

    /// Pianka symmetric niche overlap against `other`, in `[0, 1]`:
    /// `Σ pᵢqᵢ / √(Σ pᵢ² · Σ qᵢ²)`, summed over the union of axis ids
    /// present in either vector. `0.0` when either vector is the zero
    /// vector (either squared-sum factor is `0.0`), which also makes the
    /// division safe — the denominator is checked non-zero before it is
    /// ever computed. `sqrt` is IEEE-754 exact and platform-portable, so it
    /// is used directly rather than through `hornvale_kernel::math` (which
    /// exists only for transcendentals whose platform libm implementations
    /// diverge in the last ULP — see that module's doc comment).
    /// type-audit: bare-ok(ratio: return)
    pub fn overlap(&self, other: &ResourceVector) -> f64 {
        let self_sq: f64 = self.0.values().map(|weight| weight * weight).sum();
        let other_sq: f64 = other.0.values().map(|weight| weight * weight).sum();
        if self_sq == 0.0 || other_sq == 0.0 {
            return 0.0;
        }
        let axis_ids: BTreeSet<u16> = self.0.keys().chain(other.0.keys()).copied().collect();
        let numerator: f64 = axis_ids
            .into_iter()
            .map(|id| {
                let p = self.0.get(&id).copied().unwrap_or(0.0);
                let q = other.0.get(&id).copied().unwrap_or(0.0);
                p * q
            })
            .sum();
        numerator / (self_sq * other_sq).sqrt()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_vector_is_legal_and_overlaps_nothing() {
        let z = ResourceVector::new(&[]).unwrap();
        assert!(z.is_zero());
        let herb = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        assert_eq!(z.overlap(&herb), 0.0);
    }

    #[test]
    fn overlap_is_symmetric_unit_diagonal_and_disjoint_zero() {
        let a = ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap();
        let b = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        assert!((a.overlap(&a) - 1.0).abs() < 1e-9, "self-overlap is 1");
        assert!((a.overlap(&b) - b.overlap(&a)).abs() < 1e-12, "symmetric");
        let plants = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        let meat = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        assert_eq!(plants.overlap(&meat), 0.0, "disjoint niches don't compete");
    }

    #[test]
    fn rejects_negative_weight() {
        assert!(ResourceVector::new(&[(MINERAL, -0.1)]).is_err());
    }
}
