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

use crate::body::Body;
use crate::clock::REFERENCE_MASS_KG;
use crate::interior::AnchorKind;
use crate::knowledge::{Knowledge, LOCALE_KEY_PREFIX};
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
/// out of the subset relation in [`offered`] rather than needing its own
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

/// The verbs a set of `traits` affords: every verb whose required properties
/// are a SUBSET of `traits.properties` (spec §3.2). Subset, never equality —
/// see `extra_properties_expand_the_offer_never_withdraw_it` in the test
/// suite, which asserts this against a *constructed* `ObjectTraits` rather
/// than one read from [`object_registry`]. That distinction is load-bearing:
/// every kind [`object_registry`] currently assigns carries exactly one
/// property (Task 1's finding), so no query run only against the registry
/// can tell a correct subset filter apart from an incorrect equality
/// check — both agree on every single-property carrier. This is the actual
/// query; [`offered_by`] is a thin wrapper reading the global registry, kept
/// separate precisely so a test can hand it traits the registry does not
/// (and never will, while every carrier stays single-property) produce on
/// its own.
pub fn offered(traits: &ObjectTraits) -> BTreeSet<OfferedVerb> {
    OfferedVerb::all()
        .into_iter()
        .filter(|v| required_properties(*v).is_subset(&traits.properties))
        .collect()
}

/// The verbs `kind` advertises — [`offered`] applied to `kind`'s registered
/// traits (spec §3.2).
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
    offered(&traits)
}

/// The ceiling on `mass_kg / REFERENCE_MASS_KG` a bed still supports —
/// authored so a body several times the reference mass still fits, but a
/// body two orders of magnitude over it (a mammoth on a bed) does not.
/// Consumed as a ratio, matching `body.rs`'s own stated convention for
/// `mass_kg` (never raw kilograms).
/// type-audit: bare-ok(ratio)
const BED_MASS_RATIO_CEILING: f64 = 5.0;

/// Whether `body` can actually make use of `property`, given its own
/// capacities — the body-relative half of the offer (spec §3.4, Gibson: "a
/// supporter to a sprite is not one to a giant"). [`offered_to`] applies
/// this per required property, on top of (never instead of) the kind-level
/// [`offered`] query.
///
/// **Only [`ObjectProperty::SupportsRest`] is body-relative in IV.a.** Every
/// other property returns `true` unconditionally — in particular
/// [`ObjectProperty::AffordsPassage`] is deliberately excluded (spec §3.4):
/// making passage body-relative would newly BLOCK traversal, which is
/// restrictive rather than additive and belongs to Arc IV.b. This is the one
/// named function the next body-relative property gets an obvious arm in,
/// per the task brief.
///
/// **Private, not `pub` (fix round 2 reverts fix round 1's C1 patch).**
/// Fix round 1 made this `pub` so an external test could assert against the
/// predicate directly rather than through the `offered_by -> offered_to`
/// filter chain (which is a subset of its own baseline for any predicate,
/// so a test that only reads `offered_to`'s output cannot see a restrictive
/// mutation here). That test now lives in this module's own `tests` block
/// below instead, reaching this function through the ordinary `use
/// super::*;` a same-module test already gets — no `pub` required. Keeping
/// this private also closes a real bypass: [`offered_to`] is not the only
/// planned caller of this predicate forever, and a `pub` `body_can_use`
/// would let a future caller (Task 4's `offered_to_observer`, or any of
/// `cli`/`windows/lab`/the wasm clients) ask "can this body use this
/// property" while stepping around the knowledge gate spec §3.5 requires an
/// offer to pass through.
fn body_can_use(property: ObjectProperty, body: &Body) -> bool {
    match property {
        ObjectProperty::SupportsRest => body.mass_kg / REFERENCE_MASS_KG <= BED_MASS_RATIO_CEILING,
        ObjectProperty::HoldsLiquid
        | ObjectProperty::AffordsPassage
        | ObjectProperty::Encloses
        | ObjectProperty::RadiatesHeat => true,
    }
}

/// The verbs `kind` offers to this particular `body` — [`offered_by`]
/// narrowed by [`body_can_use`] (spec §3.4: an affordance is a relation
/// between object AND body, not an intrinsic property of the object alone).
///
/// **Additive, never restrictive, at the system level (spec §3.4).** Within
/// this module the result is always a *subset* of [`offered_by`]`(kind)` —
/// body-relativity can only decide whether THIS body reaches a verb the
/// kind's properties already gate, never invent one `offered_by` did not
/// already grant. That subset relation is what stays true no matter which
/// body is asked; what makes the overall rule additive is that
/// `supports-rest`'s body-gated Sleep is a channel this campaign adds
/// *alongside* the pre-existing at-home rest precondition (outside this
/// module) — a large body still rests exactly as it could before this
/// campaign, just not via a bed too small for it.
pub fn offered_to(kind: AnchorKind, body: &Body) -> BTreeSet<OfferedVerb> {
    offered_by(kind)
        .into_iter()
        .filter(|v| {
            required_properties(*v)
                .iter()
                .all(|p| body_can_use(*p, body))
        })
        .collect()
}

/// Whether `known` shows the observer has encountered ANY room — the
/// coarsest reading the [`Knowledge`] type can honestly support of "has
/// encountered the anchor's room" (spec §3.5), and the one
/// [`offered_to_observer`] actually gates on. See that function's doc for
/// why room granularity, rather than anchor granularity or a specific
/// room, is what this interface can express at all.
fn has_encountered_any_room(known: &Knowledge) -> bool {
    known.0.keys().any(|k| k.starts_with(LOCALE_KEY_PREFIX))
}

/// The verbs `kind` offers to `body`, gated by whether the observer's
/// `known` [`Knowledge`] shows it has encountered the anchor's room (spec
/// §3.5: "the offer passes through the observer's knowledge before it is
/// rendered"). **This is the seam a lying object will later use** — IV.a
/// ships truthful advertisement through it and nothing else; do not read
/// this function as a place to invent deception, and do not delete this
/// doc comment when a later campaign wires a false `Knowledge` through it.
///
/// **Gated at ROOM granularity, not anchor granularity — a narrowing the
/// TYPE forces, not a simplification of convenience.** [`Knowledge`]'s only
/// key shapes are `room/<packed FacetId>` (`knowledge.rs`'s
/// `LOCALE_KEY_PREFIX`) and `settlement/<id>/<field>`; there is no anchor
/// key, and adding one would fight decision 0069 — `AnchorId` is
/// positional and never serialized (`interior/anchor.rs`), so any anchor
/// key would be an unstable index. Nor can this check scope to *one
/// specific* room: this signature carries no room/`Facet` argument at all
/// (by the task interface, matching `offered_to`'s own shape), so the only
/// honest reading available is "has `known` recorded ANY room," via
/// [`has_encountered_any_room`] — not "has it recorded THIS anchor's
/// specific room."
///
/// **Measured, not assumed: as of this campaign, no live `Session` can ever
/// present this function with a `known` that fails the gate.**
/// `Session::new` unconditionally absorbs the CURRENT room's knowledge
/// before returning (`session.rs`'s `session.absorb_here()?;`, run before
/// the first turn is ever processed), and `IdentityProjection::project`
/// writes that knowledge regardless of light or perception — it takes a
/// `_perception: &PerceptionVector` argument it never reads.
/// `Session::enter` is reachable only from the outdoor locale the body is
/// already standing at, and standing there already absorbed that locale
/// (`self.position()` does not move while descending into a structure, so
/// every chamber of one structure shares the one `room/<id>` key the
/// outdoor step already wrote). So by the time any chamber's anchors could
/// be rendered, `known` already contains that room, and the empty branch
/// below is unreached in practice today (verified by reading
/// `Session::new`, `Session::go`/`Session::back`, and `Session::enter` —
/// none of the four callers of `absorb_here` sits behind chamber entry,
/// and all four sit in front of it).
///
/// **That makes this a seam ahead of its consumer, not yet a live gate** —
/// the same status `Warm`/`RadiatesHeat` shipped in ahead of a `warm`
/// dispatcher entry. It starts denying the day knowledge absorption stops
/// being unconditional (gating on light or perception is `knowledge.rs`'s
/// own named future direction: "Fog, inference, false belief: The
/// Vessel's"), or the day a lying object writes a `Knowledge` that omits a
/// room truthfully known to exist. A future task that wires this directly
/// against a live `Session::knowledge()` without changing one of those two
/// things would ship a check that reads as live and is permanently
/// satisfied — this doc comment is the tripwire for that.
///
/// Consumed with synthetic `Knowledge` values (as the tests beside
/// `offered_to`/`offered_by` already do with synthetic `Body` values),
/// because that is the only way to observe the deny branch at all today —
/// not because the deny branch is make-believe: [`Knowledge::default`] is
/// a real, reachable state of the type, and this function's contract must
/// hold for it regardless of whether any current caller happens to produce
/// it.
pub fn offered_to_observer(
    kind: AnchorKind,
    body: &Body,
    known: &Knowledge,
) -> BTreeSet<OfferedVerb> {
    if has_encountered_any_room(known) {
        offered_to(kind, body)
    } else {
        BTreeSet::new()
    }
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

    /// A `Body` fixture varying only `species`/`mass_kg`, for
    /// [`body_can_use`]'s own direct test below. A plain struct literal
    /// built from public constructors (`EntityId::new`, `Facet::containing`,
    /// `ResourceVector::new`, `ThreatNiche`'s pub fields,
    /// `PerceptionVector::MANIKIN`) — no world or `Ledger`, and no
    /// suite-only machinery, so it is the same shape as the identically
    /// named helper in `tests/suite/affordance.rs` (kept separate rather
    /// than shared, since that file exercises the crate's public API and
    /// this one exercises a private function no external crate can reach).
    fn body_with_mass(species: &str, mass_kg: f64) -> Body {
        let home = hornvale_kernel::Facet::containing([0.0, 0.0, 0.0], 6);
        Body {
            entity: hornvale_kernel::EntityId::new(1).expect("1 is a valid entity id"),
            home: home.clone(),
            resource: home,
            species: species.into(),
            activity: hornvale_species::ActivityCycle::Diurnal,
            temperature_niche: hornvale_kernel::ConditionResponse {
                optimum: 15.0,
                width: 10.0,
                devotion: 0.5,
            },
            deliberation_latency: 0.5,
            time_horizon: 0.0,
            thermal_strategy: hornvale_species::ThermalStrategy::Endothermic,
            niche: hornvale_kernel::ResourceVector::new(&[]).expect("the empty niche is valid"),
            boldness: 0.5,
            threat_niche: crate::liveness::ThreatNiche {
                uncanny: 1.0,
                heat: 0.0,
                cold: 0.0,
                predator: 0.5,
            },
            mass_kg,
            label: "test-body".into(),
            perception: hornvale_species::PerceptionVector::MANIKIN,
            village: None,
        }
    }

    /// Fix round 1 C1: `body_relativity_never_withdraws_an_existing_capability`
    /// (`tests/suite/affordance.rs`) asserts
    /// `offered_to(kind, body).is_subset(&offered_by(kind))`, but
    /// `offered_to` is *defined* as `offered_by(kind).into_iter().filter(..)`
    /// — a filter over a baseline is a subset of that baseline for ANY
    /// predicate whatsoever, including a restrictive one. That test cannot
    /// fail no matter what [`body_can_use`] does, so it proved nothing
    /// about §3.4's additive-only rule. Verified by mutation
    /// (task-3-report.md, fix round 1): mass-gating `AffordsPassage` — the
    /// one change §3.4 forbids by name, since it would newly block
    /// traversal — passed the whole suite undetected.
    ///
    /// This test asserts against [`body_can_use`] directly, reached the
    /// ordinary same-module-test way (`use super::*;`) rather than by
    /// making the function `pub` (fix round 2 reverts fix round 1's `pub`
    /// widening once this test proved a same-module test was enough): every
    /// [`ObjectProperty`] except [`ObjectProperty::SupportsRest`] must
    /// return `true` unconditionally, for every body regardless of mass.
    #[test]
    fn only_supports_rest_is_body_relative_in_iv_a() {
        let biosphere = hornvale_species::biosphere_registry();
        let masses = [
            crate::clock::mass_for_species("kobold", Some(&biosphere)),
            crate::clock::mass_for_species("human", Some(&biosphere)),
            crate::clock::mass_for_species("woolly-mammoth", Some(&biosphere)),
        ];

        for mass_kg in masses {
            let body = body_with_mass("probe", mass_kg);
            for property in ObjectProperty::all() {
                if property == ObjectProperty::SupportsRest {
                    continue;
                }
                assert!(
                    body_can_use(property, &body),
                    "{property:?} must be unconditional in IV.a (spec §3.4): \
                     body_can_use returned false for a body of mass {mass_kg} kg, \
                     which means a property other than SupportsRest has become \
                     body-relative — AffordsPassage becoming body-relative is the \
                     one change §3.4 forbids by name"
                );
            }
        }
    }
}
