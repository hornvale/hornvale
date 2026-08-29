//! The object property vocabulary (The Offer, spec §3.1/§3.3): what an
//! object OFFERS, as a kind-level component rather than a verb×object table.
//!
//! `affordances(object, body, observer) = { verb : required_properties(verb)
//! ⊆ properties(object) } ∩ ...` (spec §3.2) is the derived query this
//! vocabulary feeds; deriving the query itself is a later task. This module
//! is only the vocabulary and the table: [`ObjectProperty`], the eight
//! variants the spec earns against real or imminent verbs, and
//! [`object_registry`], a `ComponentStore<KindId, ObjectTraits>` held
//! vessel-locally — build-state, not world-state (spec §3.1), so it needs no
//! kernel, domain, or worldgen change and nothing here is serialized.
//!
//! **The table is keyed on thing-kind, not on anchor kind (The Chattel,
//! Task 7, spec §3.6).** The Offer keyed it on `AnchorKind`, an
//! interior-object enum; IV.c promotes an anchor into a *thing*, so the same
//! strongbox exists at two lifecycle stages and a second, thing-keyed table
//! would let its properties disagree between them. There is therefore ONE
//! table, keyed on [`hornvale_kernel::KindId`], and [`thing_kind_of`] carries
//! every [`AnchorKind`] into it — total by an exhaustive match with no
//! wildcard arm. The key space widening from a closed enum to an arbitrary
//! string is the one thing the re-key gives up, and it is bought back by
//! `cli/tests/suite/anchor_thing_correspondence.rs`, which pins every key
//! this module mints against `hornvale_thing::THING_KINDS`.
//!
//! `MaterialTraits` (`domains/terrain/src/lib.rs`) is the model for the
//! *shape*: a thin, honest, kind-keyed trait table built with
//! `ComponentStore`'s `FromIterator`.

use crate::body::Body;
use crate::clock::REFERENCE_MASS_KG;
use crate::interior::AnchorKind;
use crate::knowledge::{Knowledge, LOCALE_KEY_PREFIX};
use hornvale_kernel::{ComponentStore, KindId};
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
    /// An anchor that reveals what lies `within` it on examine — the
    /// interactive-fiction rule (spec §3.6, amended): contents show when a
    /// container is open or transparent. Both `Strongbox` and `Alcove`
    /// carry it; IV.a has no closed/open state to gate on, so every
    /// carrier reveals unconditionally — a carrier with nothing `within`
    /// it stays silent for want of contents, not for want of the property.
    Encloses,
    /// An anchor that emits warmth, read via `warmth_at`'s graph-distance
    /// decay — gates `warm` (spec §3.3: a new verb).
    RadiatesHeat,
    /// A thing a body may take up and carry — gates `take`/`drop` (The
    /// Chattel, spec §3.8). This is the SINGLE source of truth for "may a
    /// body carry this kind": `hornvale_thing::ThingTraits` carried a
    /// `portable: bool` keyed on the same [`KindId`] until Task 7 deleted
    /// it, because two `KindId`-keyed tables answering one question is the
    /// disagreement spec §3.6 re-keys this table to prevent.
    Portable,
    /// A thing with a closed state and an open one — gates `open`/`close`
    /// (spec §3.7/§3.8). Carrying it says the kind HAS the two states, never
    /// which one it is in: the state itself is a fold over committed
    /// `openness` facts (Task 5), not a property.
    Openable,
    /// A thing whose `open` additionally requires a key in the body's
    /// custody (spec §3.8). Still M+N: the lock declares what it requires
    /// and the key declares what it carries, and neither names the other.
    /// Implies nothing on its own — a `Lockable` kind that did not also
    /// carry [`ObjectProperty::Openable`] would have a lock and no lid, and
    /// `lockable_kinds_are_also_openable` refuses that combination rather
    /// than letting the two drift apart.
    Lockable,
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
            ObjectProperty::Portable,
            ObjectProperty::Openable,
            ObjectProperty::Lockable,
        ]
    }

    /// This property's registry-name mapping, and — by construction — the
    /// compile-time tripwire for the vocabulary: an exhaustive match with
    /// **no wildcard arm**, so a new [`ObjectProperty`] variant fails to
    /// compile here until it is named, forcing [`ObjectProperty::all`] and
    /// the eventual language-side pack to be revisited. One function, not
    /// two: an earlier draft paired this with a separate
    /// `object_property_variants_must_all_be_rostered` match returning the
    /// same strings, which is a duplicated table whose cheapest repair
    /// deletes one side. Never remove, never add a `_` arm. It fired as
    /// designed for The Chattel's three additions (Task 7): the compiler
    /// refused the new variants here before anything else could be run.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn concept_name(self) -> &'static str {
        match self {
            ObjectProperty::SupportsRest => "supports-rest",
            ObjectProperty::HoldsLiquid => "holds-liquid",
            ObjectProperty::AffordsPassage => "affords-passage",
            ObjectProperty::Encloses => "encloses",
            ObjectProperty::RadiatesHeat => "radiates-heat",
            ObjectProperty::Portable => "portable",
            ObjectProperty::Openable => "openable",
            ObjectProperty::Lockable => "lockable",
        }
    }
}

/// What a thing-kind offers: the properties it carries. Thin and honest
/// (the `MaterialTraits` model) — a set, not a bitmask or a table of bools,
/// because most kinds carry zero or one property and a set says so directly.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ObjectTraits {
    /// The properties this kind carries.
    pub properties: BTreeSet<ObjectProperty>,
}

/// The thing-kind an [`AnchorKind`] IS (The Chattel, Task 7, spec §3.6).
///
/// **Total, by an exhaustive match with no wildcard arm** — the same
/// compile-time tripwire [`ObjectProperty::concept_name`] uses, and for the
/// same reason: a new `AnchorKind` variant (the enum's own doc calls
/// appending routine) must fail to *compile* here rather than fall through a
/// `_` arm into some default thing-kind and quietly inherit its properties.
/// **Never add a `_` arm**; `thing_kind_of_has_no_wildcard_arm`
/// (`tests/suite/affordance.rs`) scans this function's own source and fails
/// if one appears, because a wildcard is invisible to every behavioural test
/// until the day someone appends a variant.
///
/// Every label returned here is a row in `hornvale_thing::THING_KINDS`
/// (Task 2 authored the roster wide enough for exactly this), which
/// `cli/tests/suite/anchor_thing_correspondence.rs` asserts against the real
/// roster — `windows/vessel` cannot import `hornvale-thing` to check it here
/// without a dependency it needs for nothing else, and `cli/` is the one
/// crate that already depends on both.
///
/// The mapping is INJECTIVE today: fourteen anchor kinds, fourteen distinct
/// labels. That is not stated as a law — two anchor kinds could legitimately
/// be one thing-kind some day — but it is the property that makes the
/// re-key behaviour-preserving, so
/// `the_re_key_preserves_every_anchor_kinds_offer` pins the resulting verb
/// set for all fourteen rather than trusting the mapping to stay sane.
pub fn thing_kind_of(kind: AnchorKind) -> KindId {
    match kind {
        AnchorKind::Hearth => KindId("hearth"),
        AnchorKind::Threshold => KindId("threshold"),
        AnchorKind::Bed => KindId("bed"),
        AnchorKind::Vessel => KindId("vessel"),
        AnchorKind::Screen => KindId("screen"),
        AnchorKind::Pool => KindId("pool"),
        AnchorKind::Log => KindId("log"),
        AnchorKind::Ground => KindId("ground"),
        AnchorKind::Alcove => KindId("alcove"),
        AnchorKind::Strongbox => KindId("strongbox"),
        AnchorKind::HighSeat => KindId("high-seat"),
        AnchorKind::Loom => KindId("loom"),
        AnchorKind::Anvil => KindId("anvil"),
        AnchorKind::Altar => KindId("altar"),
    }
}

/// The canonical object-kind registry: which thing-kind carries which
/// [`ObjectProperty`]. **Keyed on [`KindId`], not on [`AnchorKind`] (The
/// Chattel, Task 7, spec §3.6):** IV.c promotes an anchor into a thing, so
/// the strongbox a player opens and the strongbox derived into a room are
/// one object at two lifecycle stages. Two tables would let their properties
/// disagree, which §3.1's promotion makes reachable by construction, so
/// there is one — and `hornvale_thing::ThingTraits` lost its `portable: bool`
/// in the same commit rather than becoming the second (see
/// [`ObjectProperty::Portable`]).
///
/// The Offer's seven carriers, carried across unchanged by
/// [`thing_kind_of`]: `bed`→`SupportsRest`, `pool`/`vessel`→`HoldsLiquid`,
/// `threshold`→`AffordsPassage`, `strongbox`/`alcove`→`Encloses`,
/// `hearth`→`RadiatesHeat`. Task 7 adds the three properties spec §3.8
/// earns, on the carriers §3.8 names: `key`→`Portable`,
/// `strongbox`/`cave-mouth`→`Openable`, `strongbox`→`Lockable`. A kind
/// absent from this table carries no property.
///
/// **`key` and `cave-mouth` are the first rows with no `AnchorKind` behind
/// them at all**, and that is the point of the re-key rather than an
/// oversight: a cave mouth is a `Vertex`/`ChamberAddr` and a key is a thing
/// a body carries, neither expressible in the enum The Offer keyed on
/// (decision 0369's "the obstacle is addressing, not durability").
///
/// **`cave-mouth` carries `Openable` and NOT `AffordsPassage`, deliberately
/// and only for now.** Spec §3.7 gives the finished cave-mouth thing-kind
/// both, but `AffordsPassage` gates `Enter`, and nothing routes chamber
/// entry through this table until Task 8 retires `passage-cleared`. Adding
/// it here would make `offered_by(KindId("cave-mouth"))` advertise a verb no
/// dispatcher honours — the shape spec §3.3 calls dead vocabulary. Task 8
/// adds it with the consumer that reads it.
///
/// **Every kind NOT listed was checked against the code and found to carry
/// nothing**, not merely left unconsidered:
/// - `screen`'s own doc says outright "affords nothing, shapes sightlines"
///   (`interior/anchor.rs`).
/// - `alcove` carries `Encloses` too, and did not always: an earlier draft
///   of §3.6 drew a semantic/spatial line that excluded it, reserving
///   `Encloses` for containment that is semantic (a strongbox keeps
///   things) rather than merely spatial (an alcove is a recess in a wall).
///   The Offer's Task 6 measured the consequence — the grammar's only
///   `within` relation anywhere is `{(Hearth, Alcove)}` (a full census over
///   all 60 production gate combinations) — and that line put the property
///   on the one anchor (`strongbox`) that never holds anything, so the
///   feature would have reported nothing, forever. The interactive-fiction
///   rule that replaced it (contents show when a container is open or
///   transparent, Inform/TADS's own convention) marks BOTH.
/// - `warmth_at` (`interior/field.rs`) sums only over anchors whose
///   `kind == AnchorKind::Hearth`; no other kind ever contributes to the
///   warmth field, so `RadiatesHeat` has exactly one mechanically-supported
///   carrier.
/// - `AnchorKind::Bed` is the only kind ever pushed in a rest/fatigue
///   context anywhere in this crate (`session.rs`'s `SLEPT_PROVENANCE`,
///   every `Rest`-adjacent test); `high-seat` ("a carved chair... sees the
///   door first") and `alcove` ("deep enough to sit in") both afford
///   sitting, not the fatigue-resetting rest `Action::Rest` models, so
///   neither earns `SupportsRest`.
/// - `threshold` is the only kind ever described as "ALSO a room-graph
///   edge" (`interior/anchor.rs`); every other seam concept
///   (`interior/seam.rs`) is a property of the room-graph EDGE, not of an
///   anchor kind.
/// - `log` appears in no pattern in `interior/pattern.rs`'s `INVENTORY` at
///   all — it is drawn by nothing today — so nothing in the code licenses
///   assigning it a property.
/// - **`Portable` went to `key` and nowhere else, and that set was READ
///   rather than chosen**: it is exactly the set
///   `hornvale_thing::thing_registry` marked `portable: true` before Task 7
///   deleted the field, so the deletion moves the fact without changing it.
///   A `log` is the one plausible further candidate and nothing in the tree
///   ever picks one up; inventing a carrier here would be the Cyc bound
///   spec §3.8 names.
pub fn object_registry() -> ComponentStore<KindId, ObjectTraits> {
    fn traits(properties: &[ObjectProperty]) -> ObjectTraits {
        ObjectTraits {
            properties: properties.iter().copied().collect(),
        }
    }
    [
        (KindId("bed"), traits(&[ObjectProperty::SupportsRest])),
        (KindId("pool"), traits(&[ObjectProperty::HoldsLiquid])),
        (KindId("vessel"), traits(&[ObjectProperty::HoldsLiquid])),
        (
            KindId("threshold"),
            traits(&[ObjectProperty::AffordsPassage]),
        ),
        (
            KindId("strongbox"),
            traits(&[
                ObjectProperty::Encloses,
                ObjectProperty::Openable,
                ObjectProperty::Lockable,
            ]),
        ),
        (KindId("alcove"), traits(&[ObjectProperty::Encloses])),
        (KindId("hearth"), traits(&[ObjectProperty::RadiatesHeat])),
        (KindId("key"), traits(&[ObjectProperty::Portable])),
        (KindId("cave-mouth"), traits(&[ObjectProperty::Openable])),
    ]
    .into_iter()
    .collect()
}

/// Whether `kind` carries [`ObjectProperty::Encloses`] — the gate `examine`
/// reads before revealing what an anchor holds `within` it (spec §3.6,
/// amended). A kind absent from [`object_registry`] carries no property, so
/// it never encloses, matching [`offered_by`]'s own "absent = empty set"
/// convention.
///
/// Takes a [`KindId`] since Task 7's re-key; its one production caller
/// (`chamber_prose::examine_detail`) holds an [`AnchorKind`] and converts
/// with [`thing_kind_of`] at the call site, rather than this function
/// converting for it — the conversion belongs where the anchor is, not
/// inside a query over the thing table.
///
/// `pub(crate)`, not `pub`: the only production caller is `chamber_prose.rs`,
/// a sibling module in this crate. (No `type-audit:` tag: the extractor only
/// reads bare-`pub` items, same reason `chamber_prose::noun`/`detail` carry
/// none.)
pub(crate) fn encloses(kind: KindId) -> bool {
    object_registry()
        .get(&kind)
        .is_some_and(|traits| traits.properties.contains(&ObjectProperty::Encloses))
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
    ///
    /// **Private, narrowed from `pub` (final review minor M-g).** Its only
    /// caller anywhere in the workspace is [`offered`], two lines below, in
    /// this same module. Two prior fix rounds this campaign (`body_can_use`,
    /// `encloses`) narrowed exactly this kind of surface on exactly this
    /// argument — a `pub` item nothing outside the crate calls is a bypass
    /// waiting for a future caller, not a convenience for one that exists
    /// today.
    fn all() -> Vec<OfferedVerb> {
        vec![
            OfferedVerb::Sleep,
            OfferedVerb::Drink,
            OfferedVerb::Enter,
            OfferedVerb::Examine,
            OfferedVerb::Warm,
        ]
    }

    /// This verb's surface word, for the four surfaces spec §4 unifies.
    /// Task 7 wired three of those four surfaces (`examine`'s datum, the
    /// `HELP` tie, `IN_CHARACTER_VERBS`) directly against the literal
    /// strings each surface already owned, never against this method — so
    /// "deriving them is a later task", this doc's former claim, was stale
    /// the moment Task 7 shipped and did not use it (final review, M-g).
    /// The one caller today is `session.rs`'s own test module
    /// (`OfferedVerb::Warm.word()`, pinning `HELP`'s line against the same
    /// word this module derives `warm` from).
    ///
    /// **Stays `pub`, not narrowed to `pub(crate)`, and this is checked
    /// rather than assumed.** `body_can_use`/`encloses` (both narrowed by
    /// earlier fix rounds this campaign) each keep a real PRODUCTION
    /// caller after narrowing, so the compiler still sees them used outside
    /// `#[cfg(test)]`. `word`'s only caller lives inside another module's
    /// `#[cfg(test)]` block, which a plain (non-test) build cannot see at
    /// all — narrowing this to `pub(crate)` was tried and reddens
    /// `cargo clippy -p hornvale-vessel --lib -- -D warnings` with
    /// `error: method `word` is never used` (`-D dead-code`, part of
    /// `gate-commit`'s own `-D warnings`), because a `pub(crate)` method
    /// with no caller in the lib's own non-test compilation unit is
    /// genuinely dead code from that unit's point of view. Only a bare
    /// `pub` item is exempt from that lint (the compiler treats it as
    /// public API, potentially used elsewhere), which is why it stays.
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
///
/// **Private, narrowed from `pub` (final review minor M-g).** No caller
/// outside this module exists — `offered` and `offered_to`, both in this
/// same file, are the only two call sites in the workspace; the mention in
/// `tests/suite/affordance.rs`'s own doc comment is prose, not a call.
fn required_properties(v: OfferedVerb) -> BTreeSet<ObjectProperty> {
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
/// than one read from [`object_registry`].
///
/// **That distinction WAS load-bearing and no longer is, and the claim this
/// paragraph replaces is now false in production** (The Chattel, Task 7 fix
/// round 1). It read: "every kind [`object_registry`] currently assigns
/// carries exactly one property (Task 1's finding), so no query run only
/// against the registry can tell a correct subset filter apart from an
/// incorrect equality check — both agree on every single-property carrier
/// … a test can hand it traits the registry does not (and never will, while
/// every carrier stays single-property) produce on its own." Task 7 gave
/// `strongbox` three properties (`Encloses`/`Openable`/`Lockable`), so the
/// registry now discriminates subset from equality by itself — asserted by
/// `a_registered_multi_property_kind_discriminates_subset_from_equality` in
/// the test suite, which says the opposite of the parenthesis above.
///
/// It is corrected here rather than deleted because the parenthesis is
/// exactly the kind of sentence a later reader reasons FROM: "the registry
/// can never produce multi-property traits" is a licence to write a query
/// that assumes it. Two copies of this same claim in
/// `tests/suite/affordance.rs` were corrected during Task 7 and this one —
/// the copy a reader of `offered` actually meets — was missed, because the
/// correction was made by matching wording rather than by grepping the
/// claim.
///
/// What survives unchanged is the reason the two functions are separate:
/// [`offered`] takes traits so a test can construct a property set
/// independent of whatever the registry happens to hold today, and
/// [`offered_by`] is the thin wrapper that reads the global registry. That
/// separation is now a convenience rather than the only route to the
/// discriminating case, and `extra_properties_expand_the_offer_never_
/// withdraw_it` keeps its constructed traits for the reason its own doc
/// gives.
pub fn offered(traits: &ObjectTraits) -> BTreeSet<OfferedVerb> {
    OfferedVerb::all()
        .into_iter()
        .filter(|v| required_properties(*v).is_subset(&traits.properties))
        .collect()
}

/// The verbs `kind` advertises — [`offered`] applied to `kind`'s registered
/// traits (spec §3.2). Keyed on thing-kind since Task 7's re-key; an
/// anchor-side caller converts with [`thing_kind_of`].
///
/// A `kind` absent from [`object_registry`] is treated as carrying the empty
/// property set (`ObjectTraits::default()`), not as offering nothing: seven
/// of the fourteen `AnchorKind` variants map to a thing-kind carrying no
/// property at all, and `Examine`'s universality (empty required set ⊆ empty
/// property set) must hold for them too, or "universal" would silently mean
/// "universal among the kinds The Offer happened to register."
///
/// **The re-key widened the key space from a closed enum to an arbitrary
/// string, and that is a real loss this doc states rather than hides.**
/// `offered_by(KindId("srongbox"))` is a well-typed call returning the
/// empty-set answer, where `offered_by(AnchorKind::Srongbox)` could not
/// compile. Nothing in this module can close that; what closes it is
/// `cli/tests/suite/anchor_thing_correspondence.rs`, which pins every key
/// [`object_registry`] and [`thing_kind_of`] mint against
/// `hornvale_thing::THING_KINDS`.
pub fn offered_by(kind: KindId) -> BTreeSet<OfferedVerb> {
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
        | ObjectProperty::RadiatesHeat
        | ObjectProperty::Portable
        | ObjectProperty::Openable
        | ObjectProperty::Lockable => true,
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
pub fn offered_to(kind: KindId, body: &Body) -> BTreeSet<OfferedVerb> {
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
/// **Still keyed on [`AnchorKind`] after Task 7's re-key, and that is the
/// task boundary rather than an inconsistency.** The table, [`offered_by`]
/// and [`offered_to`] all speak [`KindId`] now; this one converts with
/// [`thing_kind_of`] on the way in, so the *gate's* currency is unchanged.
/// Decision 0369 says the reason the gate has never denied anything is
/// addressing — `AnchorKind` has no cave-mouth variant — and changing the
/// currency here is what finally gives it something to deny. That needs a
/// cave-mouth thing to exist (Task 8) before it means anything, so the plan
/// gives it its own task (Task 9) and its own decision record answering
/// 0369, and this signature is deliberately left for it.
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
        offered_to(thing_kind_of(kind), body)
    } else {
        BTreeSet::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn each_property_is_carried_by_at_least_one_thing_kind() {
        let reg = object_registry();
        for p in ObjectProperty::all() {
            assert!(
                reg.iter().any(|(_, t)| t.properties.contains(&p)),
                "{p:?} is carried by no thing-kind: a property no object has \
                 cannot gate a verb, and is dead vocabulary"
            );
        }
    }

    /// The carriers spec §3.3 (The Offer) and spec §3.8 (The Chattel) each
    /// name outright — the half of [`object_registry`] that is not the
    /// implementer's call. Every row is asserted through [`thing_kind_of`]
    /// where an anchor kind exists for it, so a mis-keyed mapping arm fails
    /// here as well as a missing property.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: drop `ObjectProperty::Lockable` from
    /// `strongbox`'s row in `object_registry`. Red observed:
    ///
    /// ```text
    /// thread 'affordance::tests::the_certain_carriers_named_by_the_spec_carry_their_property'
    /// panicked at windows/vessel/src/affordance.rs:
    /// KindId("strongbox") must carry Lockable (spec §3.3/§3.8)
    /// ```
    #[test]
    fn the_certain_carriers_named_by_the_spec_carry_their_property() {
        let reg = object_registry();
        let certain = [
            (thing_kind_of(AnchorKind::Bed), ObjectProperty::SupportsRest),
            (thing_kind_of(AnchorKind::Pool), ObjectProperty::HoldsLiquid),
            (
                thing_kind_of(AnchorKind::Vessel),
                ObjectProperty::HoldsLiquid,
            ),
            (
                thing_kind_of(AnchorKind::Threshold),
                ObjectProperty::AffordsPassage,
            ),
            (
                thing_kind_of(AnchorKind::Strongbox),
                ObjectProperty::Encloses,
            ),
            (
                thing_kind_of(AnchorKind::Hearth),
                ObjectProperty::RadiatesHeat,
            ),
            // The Chattel, spec §3.8's own table.
            (KindId("key"), ObjectProperty::Portable),
            (
                thing_kind_of(AnchorKind::Strongbox),
                ObjectProperty::Openable,
            ),
            (KindId("cave-mouth"), ObjectProperty::Openable),
            (
                thing_kind_of(AnchorKind::Strongbox),
                ObjectProperty::Lockable,
            ),
        ];
        for (kind, prop) in certain {
            let traits = reg
                .get(&kind)
                .unwrap_or_else(|| panic!("{kind:?} has no ObjectTraits"));
            assert!(
                traits.properties.contains(&prop),
                "{kind:?} must carry {prop:?} (spec §3.3/§3.8)"
            );
        }
    }

    /// A lock with no lid is not a thing: [`ObjectProperty::Lockable`]
    /// extends `open`'s precondition (spec §3.8, "a lockable thing's `open`
    /// requires a key in the body's custody"), so a kind carrying it without
    /// [`ObjectProperty::Openable`] would declare a requirement on a verb it
    /// never offers. Asserted over the whole registry rather than against
    /// `strongbox` by name, so a future carrier inherits the rule.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: drop `ObjectProperty::Openable` from
    /// `strongbox`'s row (leaving `Encloses` and `Lockable`). Red observed:
    ///
    /// ```text
    /// thread 'affordance::tests::lockable_kinds_are_also_openable' panicked at
    /// windows/vessel/src/affordance.rs:
    /// KindId("strongbox") carries Lockable without Openable: a lock with no lid
    /// ```
    #[test]
    fn lockable_kinds_are_also_openable() {
        for (kind, traits) in object_registry().iter() {
            if traits.properties.contains(&ObjectProperty::Lockable) {
                assert!(
                    traits.properties.contains(&ObjectProperty::Openable),
                    "{kind:?} carries Lockable without Openable: a lock with no lid"
                );
            }
        }
    }

    /// The Offer, Task 6 (spec §3.6, amended): the earlier draft carried
    /// `Encloses` on `Strongbox` alone, drawing a semantic/spatial line that
    /// excluded `Alcove`. Task 6's census found the consequence — the
    /// grammar's only `within` relation anywhere is `{(Hearth, Alcove)}`, so
    /// that line put the property on the one anchor that never holds
    /// anything. The interactive-fiction rule that replaced it (contents
    /// show when a container is open or transparent) marks BOTH. `encloses`
    /// is the query `examine_chamber` reads before revealing an anchor's
    /// contents (`chamber_prose::examine_detail`); this pins it against the
    /// registry it wraps rather than trusting the two never drift apart.
    #[test]
    fn both_strongbox_and_alcove_carry_encloses() {
        for kind in [AnchorKind::Strongbox, AnchorKind::Alcove] {
            assert!(
                encloses(thing_kind_of(kind)),
                "{kind:?} must carry ObjectProperty::Encloses (spec §3.6, amended)"
            );
        }
        assert!(
            !encloses(thing_kind_of(AnchorKind::Bed)),
            "a kind whose row carries no Encloses must not enclose"
        );
        assert!(
            !encloses(KindId("no-such-thing-kind")),
            "a KindId absent from object_registry must not enclose — the \
             widened key space still answers the absent case the way \
             offered_by's `absent = empty set` convention does"
        );
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
