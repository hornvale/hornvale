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

use crate::units::{Mass, UnitError};

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

/// Marine primary production and the prey web it supports — the sea's single
/// trophic axis at this fidelity.
///
/// Deliberately conflates what the land resolves into three axes
/// (`PHOTOSYNTHATE` → `PLANT_FORAGE` → `ANIMAL_PREY`), because one axis needs
/// one calibration knob and three would need three. The consequence is real
/// and worth knowing: a reef grazer and a pelagic apex predator are
/// differentiated only by their condition-response curves, not by what they
/// eat, so marine food-chain *length* is not yet an emergent property. Splitting
/// it is BIO-marine-trophic-split, and costs only new ids — never a reinterpretation of this one.
///
/// `Stock` rather than `Field`: what a consumer eats here is standing biomass,
/// even though its supply is derived from production.
pub const MARINE_FORAGE: ResourceAxis = ResourceAxis {
    id: 5,
    label: "marine forage",
    kind: ResourceKind::Stock,
};

/// The registered resource-axis basis, in ascending id order. The basis is
/// open — later campaigns may register further axes with higher ids — so this
/// slice is a snapshot of what's registered today, not a closed enum. The
/// name is historical (`v1` predates the sea) and is kept because renaming it
/// would churn four call sites for no behavioural gain.
///
/// **Append only — and the reason is positional, not arithmetic.**
///
/// The tempting rationale is float non-associativity, and for a zero-weight
/// axis it is simply wrong: `x + 0.0 == x` exactly, at every position, so a
/// mid-slice insert of an unweighted axis leaves every partial sum
/// bit-identical. Checking ULPs will find nothing and prove nothing.
///
/// What actually breaks is **tie-breaking by basis position**. A niche's
/// dominant axis is resolved by anchoring at `v1_basis()[0]` and keeping the
/// current leader unless a later axis is *strictly* greater, so position
/// decides every tie — including the total tie of the zero vector, which
/// resolves to whatever sits at index 0. Insert an axis at or before an
/// existing one and you change which axis wins those ties: prepending
/// `MARINE_FORAGE` would make a zero-weight niche resolve marine-dominant
/// instead of photosynthate-dominant, silently changing the trophic
/// classification that feeds off-chain detection.
///
/// Pinned by `the_basis_ids_are_append_only` below, which is what makes this
/// a rule rather than a hope.
pub fn v1_basis() -> &'static [ResourceAxis] {
    &[
        PHOTOSYNTHATE,
        PLANT_FORAGE,
        ANIMAL_PREY,
        DETRITUS,
        MINERAL,
        MARINE_FORAGE,
    ]
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

/// A per-axis condition-tolerance curve: how suitable one environmental
/// condition axis (temperature, moisture, insolation, elevation, ...) is at
/// a given field value, for one species. A Gaussian bump around
/// [`ConditionResponse::optimum`], floored by the caller's supplied
/// sovereignty so a well-defended species is never fully excluded even far
/// from its preferred value.
/// type-audit: bare-ok(diagnostic-value: optimum), bare-ok(diagnostic-value: width), bare-ok(ratio: devotion)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConditionResponse {
    /// The axis value the species most prefers.
    pub optimum: f64,
    /// Tolerance breadth (Gaussian sigma). Must be `> 0`.
    pub width: f64,
    /// Preference amplitude given freedom from any sovereignty floor;
    /// typically in `[0, 1]`.
    pub devotion: f64,
}

impl ConditionResponse {
    /// Suitability of `field` under this curve, floored by the caller-
    /// supplied `floor` (the species' sovereignty on this axis, computed
    /// elsewhere). `floor` is expected in `[0, 1]`: `0.0` is a hard
    /// constraint (excluded far from the optimum), `1.0` would mean fully
    /// unconstrained. This method does not validate `floor` — that is the
    /// caller's contract. Result is clamped to `[0.0, 1.0]` since `devotion`
    /// may push the unclamped peak above `1.0`.
    /// type-audit: bare-ok(diagnostic-value: field), bare-ok(ratio: floor), bare-ok(ratio: return)
    pub fn eval(&self, field: f64, floor: f64) -> f64 {
        let z = (field - self.optimum) / self.width;
        let bump = crate::math::exp(-0.5 * z * z);
        let value = floor + (1.0 - floor) * self.devotion * bump;
        value.clamp(0.0, 1.0)
    }
}

/// Ceiling on [`sovereignty_floor`]'s output — a mortal is never fully
/// omnipresent, so the saturating curve approaches but never reaches `1.0`.
/// AUTHORED biological prior (not census-calibrated).
const SOVEREIGNTY_FLOOR_MAX: f64 = 0.95;

/// How fast mass buys homeostatic buffering (the `a` coefficient on
/// `ln(mass_kg)`). AUTHORED biological prior (not census-calibrated).
const SOVEREIGNTY_MASS_COEFF: f64 = 0.15;

/// How fast magical potency buys homeostatic buffering (the `b` coefficient
/// on `potency`). AUTHORED biological prior (not census-calibrated).
const SOVEREIGNTY_POTENCY_COEFF: f64 = 1.0;

/// The sovereignty floor a species' mass and magical potency buy it: the
/// homeostatic buffering capacity that feeds [`ConditionResponse::eval`]'s
/// `floor` argument. "Preference is the luxury of the unconstrained" — a
/// tiny material creature is environment-placed (`floor` near `0.0`, a hard
/// constraint), while a dragon or a god is self-determined (`floor` near the
/// ceiling, a soft preference that never excludes).
///
/// `mass` is the species' typical body mass. `potency` is dimensionless
/// magical potency, `>= 0`; `0.0` is a purely material creature, whose floor
/// comes from mass alone.
///
/// The buffering budget `e = a * ln(mass_kg) + b * potency` is clamped to
/// `>= 0.0` before feeding a saturating exponential — this guards the
/// sub-1-kg case, where `ln(mass_kg) < 0.0` would otherwise push the budget
/// negative: a tiny material creature (`potency == 0.0`) yields `e <= 0.0`
/// and thus `floor == 0.0` (fully constrained), while a tiny but magical
/// creature still gets a high floor because `b * potency` lifts the budget
/// positive.
///
/// Returns a value in `[0.0, SOVEREIGNTY_FLOOR_MAX]` (so always `< 1.0`),
/// monotone non-decreasing in both `mass` and `potency`.
/// type-audit: bare-ok(ratio: potency), bare-ok(ratio: return)
pub fn sovereignty_floor(mass: Mass, potency: f64) -> f64 {
    let e = SOVEREIGNTY_MASS_COEFF * crate::math::ln(mass.kilograms())
        + SOVEREIGNTY_POTENCY_COEFF * potency;
    SOVEREIGNTY_FLOOR_MAX * (1.0 - crate::math::exp(-e.max(0.0)))
}

/// A per-cell **dimensionless suitability** in `[0, 1]`: how well conditions
/// suit a population, carrying no units and no magnitude.
///
/// Distinct from [`CapacityMap`] by decision 0103, which exists because a
/// campaign spec was written on the belief that a suitability field was a
/// capacity field — both were `CellMap<f64>`, so neither the compiler, the
/// reviewer, nor the type-audit objected to a 20–100× silent rescale. The
/// only legal way to combine the two is [`CapacityMap::modulated_by`].
///
/// Named `…Map` rather than `…Field` deliberately: `crate::field::Field<T>`
/// already means a function over (space × time), and reusing the word would
/// reproduce the very blur this type exists to prevent.
/// type-audit: bare-ok(ratio: element)
#[derive(Debug, Clone, PartialEq)]
pub struct SuitabilityMap(crate::CellMap<f64>);

impl SuitabilityMap {
    /// Validating constructor: every element must be finite and in `[0, 1]`.
    /// type-audit: bare-ok(ratio: values)
    pub fn new(values: crate::CellMap<f64>) -> Result<Self, UnitError> {
        for (_, v) in values.iter() {
            if !v.is_finite() || *v < 0.0 || *v > 1.0 {
                return Err(UnitError {
                    unit: "suitability",
                    value: *v,
                    reason: "must be finite and within [0, 1]",
                });
            }
        }
        Ok(Self(values))
    }

    /// Suitability at one cell.
    /// type-audit: bare-ok(ratio: return)
    pub fn at(&self, id: crate::CellId) -> f64 {
        *self.0.get(id)
    }

    /// The number of cells.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the map is empty.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// A per-cell **headcount capacity**: how many individuals a cell supports.
/// Has units — it is a population, not a ratio. See [`SuitabilityMap`] for the
/// distinction and decision 0103 for why it is enforced in the type system.
/// type-audit: bare-ok(count: element)
#[derive(Debug, Clone, PartialEq)]
pub struct CapacityMap(crate::CellMap<f64>);

impl CapacityMap {
    /// Validating constructor: every element must be finite and non-negative.
    /// type-audit: bare-ok(count: values)
    pub fn new(values: crate::CellMap<f64>) -> Result<Self, UnitError> {
        for (_, v) in values.iter() {
            if !v.is_finite() || *v < 0.0 {
                return Err(UnitError {
                    unit: "capacity",
                    value: *v,
                    reason: "must be finite and non-negative",
                });
            }
        }
        Ok(Self(values))
    }

    /// Headcount capacity at one cell.
    /// type-audit: bare-ok(count: return)
    pub fn at(&self, id: crate::CellId) -> f64 {
        *self.0.get(id)
    }

    /// The number of cells.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the map is empty.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Borrow the untyped field. An **explicit** escape hatch: consumers that
    /// still take a bare `CellMap<f64>` need one, and making the unwrap visible
    /// at the call site is the point — an implicit `Deref` would restore exactly
    /// the interchangeability decision 0103 removes.
    /// type-audit: bare-ok(count: return)
    pub fn as_cell_map(&self) -> &crate::CellMap<f64> {
        &self.0
    }

    /// Consume into the untyped field. See [`CapacityMap::as_cell_map`].
    /// type-audit: bare-ok(count: return)
    pub fn into_cell_map(self) -> crate::CellMap<f64> {
        self.0
    }

    /// Scale every cell by a dimensionless factor, staying a capacity. This is
    /// the shape of `carrying_capacity × SETTLERS_PER_CAPACITY`.
    /// type-audit: bare-ok(ratio: factor)
    pub fn scaled(&self, factor: f64) -> CapacityMap {
        CapacityMap(self.0.map_indexed(|_, v| v * factor))
    }

    /// **The only legal product**: headcount × suitability → headcount. This is
    /// what `eff_capacity` is, and having it as the one combining operation is
    /// how decision 0103 makes `capacity := suitability` unwritable.
    ///
    /// # Panics
    /// If the two maps cover different cell counts — they must come from the
    /// same geosphere.
    pub fn modulated_by(&self, suitability: &SuitabilityMap) -> CapacityMap {
        assert_eq!(
            self.0.len(),
            suitability.len(),
            "a capacity and a suitability must span the same geosphere"
        );
        CapacityMap(self.0.map_indexed(|id, v| v * suitability.at(id)))
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

    #[test]
    fn a_trailing_zero_weight_axis_does_not_perturb_a_terrestrial_niche() {
        // The stage-2 keystone: every existing kind's niche must be numerically
        // untouched by the basis extension. Both properties below are what make
        // that true, and both are checked rather than assumed.
        let terrestrial =
            ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap();

        // 1. The new axis contributes an exact zero.
        assert_eq!(terrestrial.weight(MARINE_FORAGE), 0.0);

        // 2. Summing over the extended basis is bit-identical to summing over the
        //    five-axis prefix — the property that keeps `coexist.rs` and
        //    `niche.rs` byte-identical.
        let over_full: f64 = v1_basis().iter().map(|a| terrestrial.weight(*a)).sum();
        let over_prefix: f64 = v1_basis()[..5].iter().map(|a| terrestrial.weight(*a)).sum();
        assert_eq!(over_full.to_bits(), over_prefix.to_bits());

        // 3. Overlap cannot see the basis at all: it iterates the two vectors'
        //    own recorded keys and normalizes by their own values. Measure that
        //    directly rather than narrating it — carrying an EXPLICIT zero
        //    weight on the new axis must give bit-identical overlap to omitting
        //    the axis entirely. A range check (`> 0.0 && <= 1.0`) would pass
        //    even if the extension moved the value, and a hardcoded bit pattern
        //    would pin the number without testing the property.
        let other = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        let with_explicit_zero = ResourceVector::new(&[
            (PLANT_FORAGE, 0.65),
            (ANIMAL_PREY, 0.35),
            (MARINE_FORAGE, 0.0),
        ])
        .unwrap();
        assert_eq!(
            terrestrial.overlap(&other).to_bits(),
            with_explicit_zero.overlap(&other).to_bits(),
            "a zero weight on the new axis must not move Pianka overlap"
        );
    }

    #[test]
    fn the_basis_ids_are_append_only() {
        // The real guard on the append-only rule, and the reason it is a rule
        // rather than a doc comment.
        //
        // The obvious pin — that a zero-weight axis contributes an exact zero —
        // catches nothing: a mid-slice insert and even a prepend both leave
        // every sum bit-identical, because `x + 0.0 == x` at any position
        // (verified by mutation). What a reorder DOES change is tie-breaking,
        // which resolves by basis position: the zero vector resolves to
        // whatever sits at index 0, so prepending an axis silently changes a
        // niche's dominant axis and with it the off-chain trophic
        // classification.
        //
        // Pinning the id sequence catches every reorder, insert, and renumber
        // in one assertion — and doubles as the guard on `ResourceAxis::id`
        // being a save-format contract (an id that changes meaning
        // reinterprets every world's stored `ResourceVector`s).
        let ids: Vec<u16> = v1_basis().iter().map(|a| a.id).collect();
        assert_eq!(
            ids,
            vec![0, 1, 2, 3, 4, 5],
            "the basis is append-only: ids must be dense and ascending from 0, \
             and a new axis takes the next free id at the END"
        );
    }

    #[test]
    fn condition_response_floors_and_peaks() {
        let r = ConditionResponse {
            optimum: 20.0,
            width: 10.0,
            devotion: 1.0,
        };
        // hard (floor 0): excluded far from optimum, ~1 at optimum
        assert!((r.eval(20.0, 0.0) - 1.0).abs() < 1e-9);
        assert!(r.eval(80.0, 0.0) < 0.01, "hard tolerance excludes far away");
        // soft (floor 0.7): never excluded, still peaked
        assert!(r.eval(80.0, 0.7) >= 0.7, "sovereign floor never excludes");
        assert!(
            r.eval(20.0, 0.7) > r.eval(80.0, 0.7),
            "still prefers its optimum"
        );
    }

    #[test]
    fn sovereignty_floor_constrains_the_tiny_and_frees_the_mighty() {
        let mouse = Mass::new(0.02).unwrap();
        let bugbear = Mass::new(132.0).unwrap();
        let dragon = Mass::new(50_000.0).unwrap();
        // a tiny material creature is environment-placed: floor ~ 0
        assert!(
            sovereignty_floor(mouse, 0.0) < 0.05,
            "tiny material creature is constrained"
        );
        // floor stays within [0, ceiling)
        assert!(sovereignty_floor(dragon, 0.0) >= 0.0 && sovereignty_floor(dragon, 5.0) < 1.0);
        // a very magical creature is nearly unconstrained regardless of mass
        assert!(
            sovereignty_floor(mouse, 5.0) > 0.8,
            "high potency frees even the small"
        );
        // strictly monotone in mass (potency fixed) and in potency (mass fixed)
        assert!(sovereignty_floor(dragon, 0.0) > sovereignty_floor(bugbear, 0.0));
        assert!(sovereignty_floor(bugbear, 0.0) > sovereignty_floor(mouse, 0.0));
        assert!(sovereignty_floor(bugbear, 1.0) > sovereignty_floor(bugbear, 0.0));
    }

    fn tiny_geo() -> crate::Geosphere {
        crate::Geosphere::new(0)
    }

    #[test]
    fn a_suitability_map_accepts_the_unit_interval_and_rejects_outside_it() {
        let geo = tiny_geo();
        let ok = crate::CellMap::from_fn(&geo, |c| f64::from(c.0 % 2));
        assert!(SuitabilityMap::new(ok).is_ok());

        for bad in [-0.01, 1.01, f64::NAN, f64::INFINITY] {
            let m = crate::CellMap::from_fn(&geo, |_| bad);
            assert!(
                SuitabilityMap::new(m).is_err(),
                "suitability must reject {bad}"
            );
        }
    }

    #[test]
    fn a_capacity_map_accepts_any_non_negative_magnitude_and_rejects_negatives() {
        let geo = tiny_geo();
        let ok = crate::CellMap::from_fn(&geo, |c| f64::from(c.0) * 37.5);
        assert!(CapacityMap::new(ok).is_ok());

        for bad in [-1.0, f64::NAN, f64::NEG_INFINITY] {
            let m = crate::CellMap::from_fn(&geo, |_| bad);
            assert!(CapacityMap::new(m).is_err(), "capacity must reject {bad}");
        }
    }

    #[test]
    fn modulating_a_capacity_by_a_suitability_yields_a_capacity() {
        let geo = tiny_geo();
        let cap = CapacityMap::new(crate::CellMap::from_fn(&geo, |_| 40.0)).unwrap();
        let suit = SuitabilityMap::new(crate::CellMap::from_fn(&geo, |_| 0.25)).unwrap();
        let eff = cap.modulated_by(&suit);
        for c in geo.cells() {
            assert_eq!(eff.at(c), 10.0);
        }
        // Scaling stays a capacity and composes the other way round identically.
        assert_eq!(
            cap.scaled(0.25).at(crate::CellId(0)),
            eff.at(crate::CellId(0))
        );
    }

    #[test]
    fn a_suitability_never_raises_a_capacity() {
        // The property that makes the product safe: modulation is a contraction,
        // because a suitability cannot exceed 1.
        let geo = tiny_geo();
        let cap =
            CapacityMap::new(crate::CellMap::from_fn(&geo, |c| f64::from(c.0) + 1.0)).unwrap();
        let suit = SuitabilityMap::new(crate::CellMap::from_fn(&geo, |c| f64::from(c.0 % 3) / 2.0))
            .unwrap();
        let eff = cap.modulated_by(&suit);
        for c in geo.cells() {
            assert!(
                eff.at(c) <= cap.at(c),
                "modulation must never raise capacity at {c:?}"
            );
        }
    }
}
