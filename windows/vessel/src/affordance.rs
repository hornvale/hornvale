//! The object property vocabulary (The Offer, spec §3.1/§3.3): what an
//! object OFFERS, as a kind-level component rather than a verb×object table.
//!
//! `affordances(object, body, observer) = { verb : required_properties(verb)
//! ⊆ properties(object) } ∩ ...` (spec §3.2) is the derived query this
//! vocabulary feeds; deriving the query itself is a later task. This module
//! is only the vocabulary and the table: [`ObjectProperty`], the five
//! variants the spec earns against real or imminent verbs, and
//! [`object_registry`], a `ComponentStore<AnchorKind, ObjectTraits>` held
//! vessel-locally — build-state, not world-state (spec §3.1), so it needs no
//! kernel, domain, or worldgen change and nothing here is serialized.
//!
//! `MaterialTraits` (`domains/terrain/src/lib.rs`) is the model for the
//! *shape*: a thin, honest, kind-keyed trait table built with
//! `ComponentStore`'s `FromIterator`.

use crate::interior::AnchorKind;
use hornvale_kernel::ComponentStore;
use std::collections::BTreeSet;

/// A property an object may carry — what it OFFERS, independent of any verb
/// that reads it. Minimal and earned (spec §3.3): each variant exists
/// because a shipped or imminent verb gates on it, and a property no verb
/// would ever consult was cut rather than kept (the removed
/// `bears-weight`/`climb` row is the spec's own example of the cut).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObjectProperty {
    /// A place a body may lie down and sleep — gates `Rest` (spec §3.4:
    /// body-relative, and additive to the existing at-home precondition).
    SupportsRest,
    /// A place a body may drink from — gates `Drink`.
    HoldsLiquid,
    /// A seam between two rooms — gates entering/leaving.
    AffordsPassage,
    /// An anchor whose `within` relation is semantic containment (a
    /// strongbox keeps things) rather than merely spatial (spec §3.6) —
    /// gates a recursive `examine`.
    Encloses,
    /// An anchor that emits warmth, read via `warmth_at`'s graph-distance
    /// decay — gates `warm` (spec §3.3: a new verb).
    RadiatesHeat,
}

impl ObjectProperty {
    /// Every property, one representative each, in declaration order — the
    /// roster [`object_registry`]'s own coverage test sweeps.
    pub fn all() -> Vec<ObjectProperty> {
        vec![
            ObjectProperty::SupportsRest,
            ObjectProperty::HoldsLiquid,
            ObjectProperty::AffordsPassage,
            ObjectProperty::Encloses,
            ObjectProperty::RadiatesHeat,
        ]
    }

    /// This property's registry-name mapping, and — by construction — the
    /// compile-time tripwire for the vocabulary: an exhaustive match with
    /// **no wildcard arm**, so a new [`ObjectProperty`] variant fails to
    /// compile here until it is named, forcing [`ObjectProperty::all`] and
    /// the eventual language-side pack to be revisited. One function, not
    /// two: an earlier draft paired this with a separate
    /// `object_property_variants_must_all_be_rostered` match returning the
    /// same five strings, which is a duplicated table whose cheapest repair
    /// deletes one side. Never remove, never add a `_` arm.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn concept_name(self) -> &'static str {
        match self {
            ObjectProperty::SupportsRest => "supports-rest",
            ObjectProperty::HoldsLiquid => "holds-liquid",
            ObjectProperty::AffordsPassage => "affords-passage",
            ObjectProperty::Encloses => "encloses",
            ObjectProperty::RadiatesHeat => "radiates-heat",
        }
    }
}

/// What an anchor kind offers: the properties it carries. Thin and honest
/// (the `MaterialTraits` model) — a set, not a bitmask or a table of bools,
/// because most kinds carry zero or one property and a set says so directly.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ObjectTraits {
    /// The properties this kind carries.
    pub properties: BTreeSet<ObjectProperty>,
}

/// The canonical object-kind registry: which [`AnchorKind`] carries which
/// [`ObjectProperty`]. Assigns the six certain carriers spec §3.3 names by
/// name (`Bed`→`SupportsRest`, `Pool`/`Vessel`→`HoldsLiquid`,
/// `Threshold`→`AffordsPassage`, `Strongbox`→`Encloses`,
/// `Hearth`→`RadiatesHeat`); a kind absent from this table carries no
/// property.
///
/// **Every other kind was checked against the code and found to carry
/// nothing**, not merely left unconsidered:
/// - `Screen`'s own doc says outright "affords nothing, shapes sightlines"
///   (`interior/anchor.rs`).
/// - `Alcove` is excluded from `Encloses` by the spec's own distinction
///   (§3.6): the fire's `Attach::Within(AnchorKind::Alcove)`
///   (`interior/pattern.rs`) makes an alcove a container only *spatially*,
///   which is the case the spec names as the one `Encloses` does NOT mark.
/// - `warmth_at` (`interior/field.rs`) sums only over anchors whose
///   `kind == AnchorKind::Hearth`; no other kind ever contributes to the
///   warmth field, so `RadiatesHeat` has exactly one mechanically-supported
///   carrier.
/// - `AnchorKind::Bed` is the only kind ever pushed in a rest/fatigue
///   context anywhere in this crate (`session.rs`'s `SLEPT_PROVENANCE`,
///   every `Rest`-adjacent test); `HighSeat` ("a carved chair... sees the
///   door first") and `Alcove` ("deep enough to sit in") both afford
///   sitting, not the fatigue-resetting rest `Action::Rest` models, so
///   neither earns `SupportsRest`.
/// - `Threshold` is the only kind ever described as "ALSO a room-graph
///   edge" (`interior/anchor.rs`); every other seam concept
///   (`interior/seam.rs`) is a property of the room-graph EDGE, not of an
///   anchor kind.
/// - `Log` appears in no pattern in `interior/pattern.rs`'s `INVENTORY` at
///   all — it is drawn by nothing today — so nothing in the code licenses
///   assigning it a property.
pub fn object_registry() -> ComponentStore<AnchorKind, ObjectTraits> {
    fn one(property: ObjectProperty) -> ObjectTraits {
        ObjectTraits {
            properties: [property].into_iter().collect(),
        }
    }
    [
        (AnchorKind::Bed, one(ObjectProperty::SupportsRest)),
        (AnchorKind::Pool, one(ObjectProperty::HoldsLiquid)),
        (AnchorKind::Vessel, one(ObjectProperty::HoldsLiquid)),
        (AnchorKind::Threshold, one(ObjectProperty::AffordsPassage)),
        (AnchorKind::Strongbox, one(ObjectProperty::Encloses)),
        (AnchorKind::Hearth, one(ObjectProperty::RadiatesHeat)),
    ]
    .into_iter()
    .collect()
}

/// A verb an object may advertise to a body — the counterpart to
/// [`ObjectProperty`]: where a property is what an object HAS, an
/// `OfferedVerb` is what a body may DO with it, gated by which properties it
/// requires (spec §3.2/§3.3). Four of five retrofit verbs that already ship
/// (`Sleep`/`Rest`, `Drink`, `Enter`, `Examine`); `Warm` is the one genuinely
/// new verb this campaign adds, and spec §6 names it the witness for
/// acceptance test (2) — it must appear on `Hearth` with no edit to
/// [`object_registry`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OfferedVerb {
    /// Lie down and sleep — gates on `SupportsRest` (spec §3.4:
    /// body-relative, additive to the existing at-home precondition).
    Sleep,
    /// Drink from a source — gates on `HoldsLiquid`.
    Drink,
    /// Pass through a seam between rooms — gates on `AffordsPassage`.
    Enter,
    /// Look at the object — gates on no property (spec §3.3: universal).
    Examine,
    /// Warm oneself at a heat source — gates on `RadiatesHeat`.
    Warm,
}

impl OfferedVerb {
    /// Every verb, one representative each, in declaration order — the
    /// roster [`offered_by`] sweeps for each kind.
    pub fn all() -> Vec<OfferedVerb> {
        vec![
            OfferedVerb::Sleep,
            OfferedVerb::Drink,
            OfferedVerb::Enter,
            OfferedVerb::Examine,
            OfferedVerb::Warm,
        ]
    }

    /// This verb's surface word, for the four surfaces spec §4 unifies
    /// (deriving them is a later task; this is the shared source).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn word(self) -> &'static str {
        match self {
            OfferedVerb::Sleep => "sleep",
            OfferedVerb::Drink => "drink",
            OfferedVerb::Enter => "enter",
            OfferedVerb::Examine => "examine",
            OfferedVerb::Warm => "warm",
        }
    }
}

/// The properties `v` requires before it is offered. The derived query
/// (spec §3.2) tests this as a SUBSET of an object's properties, never
/// equality — an object carrying more properties than `v` needs still
/// affords it. `Examine` requires the empty set: it is universal (spec
/// §3.3), and the empty set is a subset of every set, so universality falls
/// out of the subset relation in [`offered_by`] rather than needing its own
/// `if`.
pub fn required_properties(v: OfferedVerb) -> BTreeSet<ObjectProperty> {
    match v {
        OfferedVerb::Sleep => [ObjectProperty::SupportsRest].into_iter().collect(),
        OfferedVerb::Drink => [ObjectProperty::HoldsLiquid].into_iter().collect(),
        OfferedVerb::Enter => [ObjectProperty::AffordsPassage].into_iter().collect(),
        OfferedVerb::Examine => BTreeSet::new(),
        OfferedVerb::Warm => [ObjectProperty::RadiatesHeat].into_iter().collect(),
    }
}

/// The verbs `kind` advertises: every verb whose required properties are a
/// SUBSET of the kind's (spec §3.2). Subset, never equality — see
/// `extra_properties_do_not_withdraw_an_offer` in the test suite.
///
/// A `kind` absent from [`object_registry`] is treated as carrying the empty
/// property set (`ObjectTraits::default()`), not as offering nothing: eight
/// of the fourteen `AnchorKind` variants carry no property at all, and
/// `Examine`'s universality (empty required set ⊆ empty property set) must
/// hold for them too, or "universal" would silently mean "universal among
/// the six kinds Task 1 happened to register."
pub fn offered_by(kind: AnchorKind) -> BTreeSet<OfferedVerb> {
    let reg = object_registry();
    let traits = reg.get(&kind).cloned().unwrap_or_default();
    OfferedVerb::all()
        .into_iter()
        .filter(|v| required_properties(*v).is_subset(&traits.properties))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn each_property_is_carried_by_at_least_one_anchor_kind() {
        let reg = object_registry();
        for p in ObjectProperty::all() {
            assert!(
                reg.iter().any(|(_, t)| t.properties.contains(&p)),
                "{p:?} is carried by no anchor kind: a property no object has \
                 cannot gate a verb, and is dead vocabulary"
            );
        }
    }

    #[test]
    fn the_certain_carriers_named_by_the_spec_carry_their_property() {
        let reg = object_registry();
        let certain = [
            (AnchorKind::Bed, ObjectProperty::SupportsRest),
            (AnchorKind::Pool, ObjectProperty::HoldsLiquid),
            (AnchorKind::Vessel, ObjectProperty::HoldsLiquid),
            (AnchorKind::Threshold, ObjectProperty::AffordsPassage),
            (AnchorKind::Strongbox, ObjectProperty::Encloses),
            (AnchorKind::Hearth, ObjectProperty::RadiatesHeat),
        ];
        for (kind, prop) in certain {
            let traits = reg
                .get(&kind)
                .unwrap_or_else(|| panic!("{kind:?} has no ObjectTraits"));
            assert!(
                traits.properties.contains(&prop),
                "{kind:?} must carry {prop:?} (spec §3.3)"
            );
        }
    }
}
