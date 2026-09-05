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
//! **The table is keyed on thing-kind (The Chattel, Task 7, spec §3.6; The
//! Wicket, Task 2).** The Offer keyed it on a closed interior-object enum;
//! IV.c promotes an anchor into a *thing*, so the same strongbox exists at
//! two lifecycle stages and a second, thing-keyed table would let its
//! properties disagree between them. The Chattel therefore made ONE table,
//! keyed on [`hornvale_kernel::KindId`], with a total enum-to-label mapping
//! feeding it. The Wicket deleted the enum outright, so an anchor now CARRIES
//! a `KindId` and there is no mapping left to be total about. The key space is
//! an arbitrary string either way, and what buys that back is the totality
//! gate (spec §5.1's G-e), which pins every key this module mints against
//! `hornvale_thing::THING_KINDS`.
//!
//! `MaterialTraits` (`domains/terrain/src/lib.rs`) is the model for the
//! *shape*: a thin, honest, kind-keyed trait table built with
//! `ComponentStore`'s `FromIterator`.

use crate::body::Body;
use crate::clock::REFERENCE_MASS_KG;
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
    /// carry it. A carrier with nothing `within` it stays silent for want of
    /// contents, not for want of the property.
    ///
    /// **The "or transparent" half went live in Task 11, and this doc used to
    /// say the opposite.** It read: *"IV.a has no closed/open state to gate
    /// on, so every carrier reveals unconditionally."* True then; there is a
    /// state now. `chamber_prose::examine_detail` gates a carrier that ALSO
    /// carries [`ObjectProperty::Openable`] on its `openness` fold, and leaves
    /// a carrier without one (the alcove — a recess has no lid) revealing
    /// unconditionally, which is what the two-arm rule always said.
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

/// What a rest surface is made of, from the sleeping body's point of view.
///
/// **Two variants, not a single hardness scalar, because a made surface has
/// no hardness of its own that matters.** 0697's own words: "a made bed is
/// made by, and for, the body that made it." A `Made` surface is fitted to
/// whoever built it, so the sleeper's substrate preference does not
/// discriminate against it and `fit` is `1.0` — the species' own
/// `sleep_grade_registry` row already encodes whether that species can
/// collect the fit half at all (`INSULATION_ONLY` 1.35 is exactly "endotherm,
/// cannot collect fit"), so applying a second fit penalty here would charge
/// it twice.
///
/// (The `type-audit:` tag sits on the TYPE, not on the variant: the extractor
/// reads an item's own doc and names the primitive by position, so a tag on
/// `Natural`'s doc line is invisible to it.)
/// type-audit: bare-ok(ratio: Natural.0)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Substrate {
    /// Built by, and sized to, whoever built it.
    Made,
    /// Found rather than made, at this hardness — `0.0` fully yielding,
    /// `1.0` rock.
    Natural(f64),
}

/// What a kind offers a body that lies down on it (The Tenon).
///
/// **Held in the same row as [`ObjectProperty::SupportsRest`] rather than in
/// a sibling table, and that is a correctness choice.** See
/// [`object_registry`] and `supports_rest_and_a_rest_surface_imply_each_other`.
/// type-audit: bare-ok(ratio: offer)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RestSurface {
    /// The fraction of a fully-offering made surface's benefit this kind
    /// gives, in `[0, 1]`. `1.0` is a bed.
    pub offer: f64,
    /// What the surface is, for the sleeper's substrate preference.
    pub substrate: Substrate,
}

/// What a thing-kind offers: the properties it carries, and — for a kind that
/// carries [`ObjectProperty::SupportsRest`] — what lying down on it is worth.
/// Thin and honest
/// (the `MaterialTraits` model) — a set, not a bitmask or a table of bools,
/// because most kinds carry zero or one property and a set says so directly.
///
/// **No `Eq` derive, and its absence is load-bearing rather than an
/// oversight**: [`RestSurface`] carries an `f64`, so `Eq` cannot survive the
/// field. Nothing in the workspace required it (measured before the field was
/// added: dropping `Eq` produced no errors under
/// `cargo check --workspace --all-targets`).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ObjectTraits {
    /// The properties this kind carries.
    pub properties: BTreeSet<ObjectProperty>,
    /// What this kind offers a body that lies down on it, or `None` for a
    /// kind with nowhere to lie down — which is every kind not carrying
    /// [`ObjectProperty::SupportsRest`], asserted in both directions by
    /// `supports_rest_and_a_rest_surface_imply_each_other`.
    pub rest: Option<RestSurface>,
}

/// A bed is the reference surface: the one this campaign's grade is
/// calibrated against, and the value that makes `grade(species, bed)`
/// reproduce `sleep_grade_registry`'s row for every species byte for byte
/// (spec §5.2). It is `1.0` by definition of the scale, not by measurement —
/// every other surface is stated as a fraction of it.
/// plumb: universal(the unit of the offer scale itself, against which every per-kind offer is expressed -- a definition, not a quantity that varies)
const BED_OFFER: f64 = 1.0;

/// How much a loose bed of rushes offers against the made-bed reference: most
/// of a bed's support, while withholding the fitted construction it lacks.
/// plumb: universal(the fixed share of a made bed's benefit offered by the rushes thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const RUSHES_OFFER: f64 = 0.7;
/// Where rushes lie on the hardness axis: close to fully yielding, with a
/// little resistance left in bundled stems.
/// plumb: universal(the fixed hardness of the rushes thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const RUSHES_HARDNESS: f64 = 0.1;
/// How much a ledge offers against the made-bed reference. It matches the
/// other found surfaces so the relation's ordering comes from substrate fit,
/// not a hidden generosity advantage authored into one kind.
/// plumb: universal(the fixed share of a made bed's benefit offered by the ledge thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const LEDGE_OFFER: f64 = 0.7;
/// Where a weathered stone ledge lies on the hardness axis: nearly rock, but
/// short of the scale's bare, unyielding endpoint.
/// plumb: universal(the fixed hardness of the ledge thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const LEDGE_HARDNESS: f64 = 0.85;
/// How much wild bracken offers against the made-bed reference: the same
/// found-surface share as rushes and ledge, before a sleeper's fit is applied.
/// plumb: universal(the fixed share of a made bed's benefit offered by the bracken thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const BRACKEN_OFFER: f64 = 0.7;
/// Where a springy stand of bracken lies on the hardness axis: the same
/// yielding point as loose rushes.
/// plumb: universal(the fixed hardness of the bracken thing-kind in every world -- species variation belongs to substrate_response and world variation to composition)
const BRACKEN_HARDNESS: f64 = 0.1;

/// How much a natural overhang offers against the made-bed reference. It is
/// the hard-natural sibling of a ledge: both use the preregistered natural-
/// surface offer, while the distinct key keeps the calibration explicit on
/// the derived-feature row Nathan assigned it to at close reconciliation.
/// plumb: universal(the fixed share of a made bed's benefit offered by the overhang weft-kind in every world -- species variation belongs to substrate_response and world variation to derived occurrence)
const OVERHANG_OFFER: f64 = 0.7;
/// Where a stone overhang lies on the hardness axis. Nathan placed it beside
/// the ledge as a hard natural surface, short of the unyielding endpoint.
/// plumb: universal(the fixed hardness of the overhang weft-kind in every world -- species variation belongs to substrate_response and world variation to derived occurrence)
const OVERHANG_HARDNESS: f64 = 0.85;

/// The canonical object-kind registry: which thing-kind carries which
/// [`ObjectProperty`]. **Keyed on [`KindId`], not on an anchor-kind enum
/// (The Chattel, Task 7, spec §3.6; the enum itself is gone since The
/// Wicket):** IV.c promotes an anchor into a thing, so
/// the strongbox a player opens and the strongbox derived into a room are
/// one object at two lifecycle stages. Two tables would let their properties
/// disagree, which §3.1's promotion makes reachable by construction, so
/// there is one — and `hornvale_thing::ThingTraits` lost its `portable: bool`
/// in the same commit rather than becoming the second (see
/// [`ObjectProperty::Portable`]).
///
/// The Offer's seven carriers, carried across unchanged when the table was
/// re-keyed: `bed`→`SupportsRest`, `pool`/`vessel`→`HoldsLiquid`,
/// `threshold`→`AffordsPassage`, `strongbox`/`alcove`→`Encloses`,
/// `hearth`→`RadiatesHeat`. Task 7 adds the three properties spec §3.8
/// earns, on the carriers §3.8 names: `key`→`Portable`,
/// `strongbox`/`cave-mouth`→`Openable`, `strongbox`→`Lockable`; Task 8 adds
/// `cave-mouth`→`AffordsPassage` (spec §3.7); and Task 5 adds
/// `brazier`→`RadiatesHeat`, the proof kind that arrived as data rows and no
/// dispatcher edit. A kind absent from this table carries no property.
/// The Tenon adds `rushes`, `ledge`, and `bracken` as three more
/// `SupportsRest` carriers, each with the [`RestSurface`] payload the recovery
/// fold and sleep-site chooser read.
///
/// **This paragraph used to claim "`key` and `cave-mouth` were the first
/// rows with no anchor-kind variant behind them at all", past-tensed as a
/// record of why the re-key happened rather than a live claim — and the
/// claim was false the whole time it stood, not merely superseded.** `key`
/// got an `AnchorKind::Key` variant in The Chattel's Task 11, four tasks
/// after this sentence was first written, so it was never true by the time
/// the enum this campaign deleted actually went away. Past-tensing a false
/// claim launders it into false history: a reader audits a claim about NOW
/// and accepts one about THEN, so the tense change made the sentence harder
/// to catch, not easier — and two reviewers read it after that rewrite and
/// neither flagged it. What the sentence was reaching for is true of
/// `cave-mouth` alone: a cave mouth is a `Vertex`/`ChamberAddr`, never
/// expressible in the closed enum The Offer keyed on (decision 0369's "the
/// obstacle is addressing, not durability"), so it is the row that was
/// never an anchor — the openness this campaign generalised. The Wicket
/// deleted that enum outright, so the distinction either row once drew no
/// longer separates anything and every row here is a label now.
///
/// **`cave-mouth` carries BOTH `Openable` and `AffordsPassage` as of Task 8**
/// (spec §3.7), and the paragraph this replaces deferred the second one with
/// a precise condition, so the condition is worth settling rather than
/// quietly dropping. It read: *"`AffordsPassage` gates `Enter`, and nothing
/// routes chamber entry through this table until Task 8 retires
/// `passage-cleared` … Task 8 adds it with the consumer that reads it."*
///
/// Task 8 retired the predicate: chamber entry (`Session::delve_at`) is now
/// gated on the cave mouth's own `openness` fold, so the cave mouth is a
/// promoted thing with a derived `EntityId` and an authored kind, which is
/// what the deferral was waiting on.
///
/// **What Task 9 changed here, and what it did not.** This paragraph used
/// to read "no production caller holds a cave-mouth [`KindId`] yet … Task 9
/// re-keys [`offered_to_observer`] to `KindId` …, which is the call site."
/// The re-key landed; the second clause was optimistic and is corrected
/// rather than deleted, because it is the kind of sentence a later reader
/// reasons FROM. What the re-key bought is that a cave mouth can now be
/// NAMED in the query's currency at all — `offered_to_observer(KindId(
/// "cave-mouth"), …)` is a well-typed call, where a call taking an
/// anchor-kind variant could never have reached this row: that mapping was
/// injective over fourteen variants and none of them was `cave-mouth`.
/// What it did NOT buy is a production caller: chamber entry
/// (`Session::delve_at`) gates on the cave mouth's own `openness` fold, not
/// on this table, so `offered_by(KindId("cave-mouth")) == {Enter, Examine}`
/// is still a fact only the test suite reads. That distinction is decision
/// 0397's, and it is the honest half of answering 0369 — addressing lifted,
/// live reachability untouched. The property is granted HERE rather than at
/// a call site because §3.7 assigns it to Task 8 and because the thing it
/// describes — a mouth a body passes through, whose passability is a ledger
/// fold — exists as of that task and not before it.
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
///   `within` relation anywhere was `{(Hearth, Alcove)}` (a full census over
///   all 60 production gate combinations) — and that line put the property
///   on the one anchor (`strongbox`) that never held anything, so the
///   feature would have reported nothing, forever. The interactive-fiction
///   rule that replaced it (contents show when a container is open or
///   transparent, Inform/TADS's own convention) marks BOTH.
///   **The strongbox holds something now** (The Chattel, Task 11): the
///   authored `the-key-in-the-strongbox` pattern makes the census read
///   `{(Alcove, Hearth), (Strongbox, Key)}`, and
///   `interior::pattern::tests::the_grammar_puts_exactly_these_things_
///   inside_other_things` is that measurement made permanent. The tenses
///   above are past on purpose: the sentence is a record of why the line
///   moved, not a live claim about today's grammar.
/// - **`RadiatesHeat` has TWO carriers, and heat has TWO dispatchers that
///   do not agree — this bullet claimed "exactly one mechanically-supported
///   carrier" until the final review, and Task 5 falsified it 45 lines
///   below.** The `warm` VERB reads this table
///   ([`offered_by`] over [`required_properties`]), so `hearth` and `brazier` both
///   afford it and a third carrier needs only a row. The warmth FIELD does
///   not: `warmth_at` (`interior/field.rs`) is a literal
///   `kind != kinds::HEARTH` skip, so no other kind contributes a degree of
///   warmth however many rows this table grows — the exact kind-comparison
///   the campaign replaced in `Session::warm`, surviving one file away.
///   **The brazier does not expose the disagreement, and that is an accident
///   of the BAND, not a property of the code.** Every production `warmth_at`
///   caller (`Fatigue`/comfort urgency in `liveness.rs`) reads an `Interior`
///   built by `interior_of`, whose `selection` admits only `at_locale: true`
///   patterns, and `the-brazier` is `at_locale: false` — so today no body
///   ever stands beside one while a thermal drive is scored. Promote that
///   pattern to the locale band and the offer says "you may warm yourself
///   here" while the field says 0.0 °C. Fixing it means reading
///   `object_registry` from `warmth_at`; nothing in this campaign's scope
///   asked for it, so it is named rather than taken.
/// - `bed`, `rushes`, `ledge`, and `bracken` are the four rest surfaces. The
///   last three are the natural surfaces The Tenon's locale patterns make
///   reachable; `high-seat` ("a carved chair... sees the door first") and
///   `alcove` ("deep enough to sit in") both afford sitting, not the
///   fatigue-resetting rest `Action::Rest` models, so neither earns
///   `SupportsRest`.
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
    fn traits(properties: &[ObjectProperty], rest: Option<RestSurface>) -> ObjectTraits {
        ObjectTraits {
            properties: properties.iter().copied().collect(),
            rest,
        }
    }
    [
        (
            KindId("bed"),
            traits(
                &[ObjectProperty::SupportsRest],
                Some(RestSurface {
                    offer: BED_OFFER,
                    substrate: Substrate::Made,
                }),
            ),
        ),
        (
            KindId("rushes"),
            traits(
                &[ObjectProperty::SupportsRest],
                Some(RestSurface {
                    offer: RUSHES_OFFER,
                    substrate: Substrate::Natural(RUSHES_HARDNESS),
                }),
            ),
        ),
        (
            KindId("ledge"),
            traits(
                &[ObjectProperty::SupportsRest],
                Some(RestSurface {
                    offer: LEDGE_OFFER,
                    substrate: Substrate::Natural(LEDGE_HARDNESS),
                }),
            ),
        ),
        (
            KindId("bracken"),
            traits(
                &[ObjectProperty::SupportsRest],
                Some(RestSurface {
                    offer: BRACKEN_OFFER,
                    substrate: Substrate::Natural(BRACKEN_HARDNESS),
                }),
            ),
        ),
        (KindId("pool"), traits(&[ObjectProperty::HoldsLiquid], None)),
        (
            KindId("vessel"),
            traits(&[ObjectProperty::HoldsLiquid], None),
        ),
        (
            KindId("threshold"),
            traits(&[ObjectProperty::AffordsPassage], None),
        ),
        (
            KindId("strongbox"),
            traits(
                &[
                    ObjectProperty::Encloses,
                    ObjectProperty::Openable,
                    ObjectProperty::Lockable,
                ],
                None,
            ),
        ),
        (KindId("alcove"), traits(&[ObjectProperty::Encloses], None)),
        (
            KindId("hearth"),
            traits(&[ObjectProperty::RadiatesHeat], None),
        ),
        (
            KindId("brazier"),
            traits(&[ObjectProperty::RadiatesHeat], None),
        ),
        (KindId("key"), traits(&[ObjectProperty::Portable], None)),
        (
            KindId("cave-mouth"),
            traits(
                &[ObjectProperty::AffordsPassage, ObjectProperty::Openable],
                None,
            ),
        ),
        // The Brattice, spec §3.7: the cave mouth's properties plus the
        // strongbox's lock — a passage a body walks through, with a lid and a
        // lock on it. Not `Portable`, deliberately: a door is hung, and
        // `the_lock_wants_a_property_and_exactly_one_kind_supplies_it`
        // (`session.rs`) is the test that would redden if it were.
        (
            KindId("door"),
            traits(
                &[
                    ObjectProperty::AffordsPassage,
                    ObjectProperty::Openable,
                    ObjectProperty::Lockable,
                ],
                None,
            ),
        ),
    ]
    .into_iter()
    .collect()
}

/// The derived-feature affordance table (The Weft, Task 8, carried from Task
/// 7): what a weft KIND affords, in the SAME vocabulary [`object_registry`]
/// uses — [`ObjectProperty`], [`ObjectTraits`], the [`offered`] query — but
/// keyed on [`hornvale_worldgen::WeftKind`], **never** on [`KindId`].
///
/// **Why a second table, when the ruling said "use `object_registry`."**
/// `object_registry`'s own keys are gated closed: `kind_totality.rs`'s
/// `every_propertied_kind_is_a_roster_row` (G-e, spec §5.1) asserts every one
/// of its rows is a member of `hornvale_thing::THING_KINDS`, the roster
/// `windows/vessel`'s interior pattern grammar places CHAMBER anchors from —
/// bed, hearth, door, and the rest. A weft kind is not one of those: it is
/// generated on the WALK band, from noise and macro state, never placed by
/// that grammar, so a `KindId("overhang")` row in `object_registry` would
/// either fail G-e outright or force `overhang` into `THING_KINDS` itself —
/// which conflates the WALK-band derived tier with the CHAMBER placed tier,
/// the exact mistake controller ruling R2 forbids for `SiteKind` ("the tier
/// is *when* a feature is generated, the kind is *what it is*"; a `KindId`
/// keyed on `THING_KINDS` is a placed-thing IDENTITY, not a feature kind).
/// `ComponentStore<K, C>` is documented as "one typed store per shape" for
/// exactly this reason (the plan's Global Constraints): a new key SHAPE gets
/// its own store rather than a second population squeezed into one already
/// governed by a closed roster. Reusing [`ObjectProperty`]/[`ObjectTraits`]/
/// [`offered`] — the actual "existing vocabulary" the ruling names — is what
/// this table does; reusing `object_registry`'s own gated KEY SPACE is not
/// the same thing, and the ruling's wording ("`object_registry` as a
/// `ComponentStore<KindId, ObjectTraits>`") reads as naming what that
/// function already IS, for orientation, not as instructing a second
/// population into its one gated table.
///
/// **Only [`hornvale_worldgen::WeftKind::Overhang`] carries anything.** Spec
/// §5.6 assigns overhang alone the job of "the affordance path end to end" —
/// spring is enterable through its own site machinery (a DIFFERENT path,
/// R2), thicket and erratic are texture and a negative control with no
/// affordance claim at all. `SupportsRest` stands in for "a place to get out
/// of the rain" and `RadiatesHeat` for "a place to build a fire" — the exact
/// two properties [`hornvale_worldgen::WeftKind::Overhang`]'s own doc names
/// ("SupportsRest-adjacent shelter plus a warmth variant"). At The Tenon's
/// close reconciliation Nathan made that adjacency exact: the overhang is
/// the hard-natural sibling of a ledge, with the preregistered `0.7` offer
/// and `0.85` hardness. The payload lives in this same row so decision 0728's
/// two-way `SupportsRest`/`RestSurface` invariant holds across both object-
/// trait stores. `RadiatesHeat` here is a CAPABILITY, not a claim that a fire
/// is already lit — no verb in this campaign turns it into one (see
/// [`weft_offers`]'s own doc for what that stops short of).
pub fn weft_object_registry() -> ComponentStore<hornvale_worldgen::WeftKind, ObjectTraits> {
    [(
        hornvale_worldgen::WeftKind::Overhang,
        ObjectTraits {
            properties: [ObjectProperty::SupportsRest, ObjectProperty::RadiatesHeat]
                .into_iter()
                .collect(),
            rest: Some(RestSurface {
                offer: OVERHANG_OFFER,
                substrate: Substrate::Natural(OVERHANG_HARDNESS),
            }),
        },
    )]
    .into_iter()
    .collect()
}

/// The verbs a weft `kind` offers — [`weft_object_registry`]'s traits routed
/// through the SAME [`offered`] query [`offered_by`] uses over
/// `object_registry`, so a derived kind and a placed thing prove the
/// affordance path through one shared function, never two. A kind absent
/// from [`weft_object_registry`] (three of the four today) offers nothing,
/// matching [`offered_by`]'s own "absent = empty set" convention.
///
/// **What this proves, and what it does not.** It proves the query
/// machinery — [`ObjectProperty`]'s required-property sets, the subset
/// filter [`offered`] runs — answers correctly for a KIND that was never a
/// placed [`KindId`], which is the "affordance path end to end" spec §5.6
/// asks for. It does NOT wire a live `warm`/`sleep` verb to fire at an
/// outdoor overhang facet: `Session::warm`'s own gate reads
/// `chamber_interior_here`, a chamber-only anchor catalogue, on purpose (its
/// own doc: "standing at a hearth out of doors is impossible in the first
/// place"), and this campaign builds no fire-lighting mechanic for the
/// derived surface to hand that gate a lit fire to warm at. Wiring a verb
/// that always narrates success at ANY overhang, unconditionally, would
/// invent exactly the gameplay claim spec §5.6 does not make; the
/// affordance stays a proven CAPABILITY (this function, and the walk-band
/// prose that already names it) rather than an implemented ACT.
pub fn weft_offers(kind: hornvale_worldgen::WeftKind) -> BTreeSet<OfferedVerb> {
    let reg = weft_object_registry();
    let traits = reg.get(&kind).cloned().unwrap_or_default();
    offered(&traits)
}

/// [`weft_offers`], from a REALIZED [`hornvale_worldgen::WeftFeature`]
/// rather than a bare kind (fix round 1, F6). Spec §5.6 assigns the overhang
/// the job of proving "the affordance path end to end" — before this
/// function, `weft_offers` ran from a hardcoded `WeftKind`, never from an
/// occurrence a derivation actually produced (`prevalence`/`occurs`, or the
/// residency window that caches them), so the path stopped one step short
/// of the claim. This is that step: the query answer for a facet the world
/// actually generated a feature at, not a kind named in isolation.
pub fn weft_offers_for(feature: &hornvale_worldgen::WeftFeature) -> BTreeSet<OfferedVerb> {
    weft_offers(feature.kind)
}

/// Whether `kind` carries [`ObjectProperty::Encloses`] — the gate `examine`
/// reads before revealing what an anchor holds `within` it (spec §3.6,
/// amended). A kind absent from [`object_registry`] carries no property, so
/// it never encloses, matching [`offered_by`]'s own "absent = empty set"
/// convention.
///
/// Takes a [`KindId`] since Task 7's re-key. Its one production caller
/// (`chamber_prose::examine_detail`) used to hold an anchor-kind variant and
/// convert at the call site, rather than this function converting for it —
/// the conversion belonged where the anchor was, not inside a query over the
/// thing table. Since The Wicket the anchor carries a [`KindId`] outright
/// and there is no conversion at either end.
///
/// `pub(crate)`, not `pub`: the only production caller is `chamber_prose.rs`,
/// a sibling module in this crate. (No `type-audit:` tag: the extractor only
/// reads bare-`pub` items, same reason `chamber_prose::noun`/`detail` carry
/// none.)
pub(crate) fn encloses(kind: KindId) -> bool {
    carries(kind, ObjectProperty::Encloses)
}

/// Whether `kind` carries `property` — [`encloses`] generalised, because The
/// Chattel's Task 11 needs the same question asked of three properties rather
/// than one, and three near-identical wrappers is the duplicated-table shape
/// decision 0261 warns about.
///
/// `encloses` is kept as its own name rather than folded in: it is the
/// vocabulary `chamber_prose` reads, and its doc carries the interactive-
/// fiction rule that gates it. It now routes through here, so there is one
/// registry read and not two that agree.
pub(crate) fn carries(kind: KindId, property: ObjectProperty) -> bool {
    object_registry()
        .get(&kind)
        .is_some_and(|traits| traits.properties.contains(&property))
}

/// Whether the kind spelled `label` carries `property`.
///
/// **The label-keyed door into the property table, and it exists because the
/// LEDGER speaks labels.** A promoted thing's kind reaches a reader as
/// `Ledger::kind_of`'s `&str` — an `instance-of` object, a `Value::Text` —
/// and [`KindId`] wraps a `&'static str`, so a runtime string cannot be
/// turned into one without leaking it. Scanning the registry for a matching
/// spelling is the honest conversion: it answers `false` for a label no row
/// carries, which is the same "absent = no property" convention
/// [`object_registry`]'s own doc states.
///
/// The one caller today is `open`'s lock precondition, which asks whether
/// anything in a body's custody carries [`ObjectProperty::Portable`] — the
/// first question in Hornvale asked of a thing the ledger knows about rather
/// than of an anchor the grammar just drew.
/// type-audit: bare-ok(identifier-text: label)
pub(crate) fn label_carries(label: &str, property: ObjectProperty) -> bool {
    object_registry()
        .iter()
        .any(|(kind, traits)| kind.0 == label && traits.properties.contains(&property))
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
    /// Lie down and sleep. **This doc used to say "gates on `SupportsRest`
    /// (spec §3.4: body-relative, additive to the existing at-home
    /// precondition)", and both halves were false**: no at-home precondition
    /// has ever been enforced (see [`crate::action::Action::Rest`]'s doc),
    /// and `SupportsRest` reaches only the *advertisement* layer —
    /// [`required_properties`]`(Sleep)` decides which objects LIST `sleep`
    /// as something they offer, but nothing reads that property to decide
    /// whether sleeping is ALLOWED, because sleeping is always allowed
    /// (Nathan's ruling, 2026-09-01: a creature must be able to pass out in
    /// the road, and prefer a bed, a fur or bracken where it can). The
    /// property survives here as a GRADE filed among gates, not a gate
    /// itself — `PSY-rest-quality-is-a-grade-not-a-gate` in the idea
    /// registry is where that distinction, and its eventual use, live.
    ///
    /// **"The eventual use" arrived in Task 10, and the paragraph above is
    /// left standing because every word of it is still true.** Sleeping is
    /// still ungated: `Session::sleep` still asks nothing about the room, and
    /// a creature still beds down where it stands. What changed is that the
    /// property is no longer advertisement-only —
    /// `liveness::room_affords_rest` asks this verb of every anchor in the
    /// room a recovery bout was taken in, and a bout in a room that offers it
    /// repays MORE than one on bare ground. That is the GRADE half of the
    /// grade/gate split going live, not the gate half arriving late: the same
    /// act is available everywhere and is worth more in some places.
    ///
    /// **HOW MUCH more is a property of the SLEEPER, not of this verb** (The
    /// Pallet, Task 4). This sentence used to name a constant,
    /// `AFFORDED_REST_GAIN`, and say a bout "repays `AFFORDED_REST_GAIN`
    /// times" — one uniform multiplier for every creature alive. That
    /// constant no longer exists under that name, so a reader who grepped it
    /// from here landed on nothing, and a reader who did not grep learned
    /// something false. The multiplier is now
    /// `hornvale_species::sleep_grade_registry`, one row per kind: a settled
    /// people gets the authored ceiling, a fully marine kind almost nothing,
    /// and an ametabolic one exactly `1.0` — no bonus, so a xorn on a bed
    /// folds the road's arithmetic. `liveness::sleep_grade_for` is the read.
    ///
    /// So of spec §6a, the OBJECT side and the SPECIES side are both built.
    /// The people side (a `(species, thing)` edge — which thing a people
    /// tends to sleep on, needing kind-to-kind edges) and the individual side
    /// (a `Lineage`-derived per-instance preference) are still where that
    /// registry row leaves them, and are declared as named seams on
    /// `liveness::SiteGrade`'s own doc, which is the type that would carry
    /// either one.
    Sleep,
    /// Drink from a source — gates on `HoldsLiquid`.
    Drink,
    /// Pass through a seam between rooms — gates on `AffordsPassage`.
    Enter,
    /// Look at the object — gates on no property (spec §3.3: universal).
    Examine,
    /// Warm oneself at a heat source — gates on `RadiatesHeat`.
    Warm,
    /// Open a thing that has a closed state — gates on `Openable` (The
    /// Chattel, spec §3.7/§3.8).
    Open,
    /// Take a thing up into the body's custody — gates on `Portable` (The
    /// Chattel, Task 12, spec §3.8). The property was granted in Task 7 and
    /// gated nothing until now; `the_re_key_preserves_every_anchor_kinds_offer`
    /// froze `Key`'s offer at `[Examine]` precisely so that this arrival had
    /// to move a line deliberately.
    Take,
    /// Set a carried thing down in the room — gates on `Portable` too, and
    /// the shared requirement is the same argument [`OfferedVerb::Close`]
    /// makes about its own pair: a body may always set down what it could
    /// pick up, and a second property would let a kind declare a thing it can
    /// take and never release. Spec §3.8 bounds the vocabulary at three
    /// additions and all three are spent, so a fourth would have to be earned
    /// against that bound rather than assumed.
    Drop,
    /// Stow a carried thing inside a container — gates on `Portable`, for the
    /// same reason again: the property belongs to the thing being MOVED. What
    /// the container must be (`Encloses`, and open if it has a lid) is a
    /// precondition on the ACT, read against a second object exactly as
    /// [`OfferedVerb::Open`]'s lock is, and so is not expressible in this
    /// query — which holds one object and no other.
    Put,
    /// Close it again — gates on `Openable` too, and the shared requirement
    /// is the point rather than a shortcut. Spec §3.7's deliverable is that
    /// re-closing exists at all: decision 0367 deferred a closing act because
    /// "doors, lids, and containers … are the same mechanism seen from three
    /// angles", and a `Closeable` property distinct from `Openable` would be
    /// a fourth vocabulary item no verb needs and would let a kind declare a
    /// lid it can open and not shut. Spec §3.8 bounds the property vocabulary
    /// at three additions; this is the reading that stays inside it.
    Close,
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
            OfferedVerb::Open,
            OfferedVerb::Close,
            OfferedVerb::Take,
            OfferedVerb::Drop,
            OfferedVerb::Put,
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
            OfferedVerb::Open => "open",
            OfferedVerb::Close => "close",
            OfferedVerb::Take => "take",
            OfferedVerb::Drop => "drop",
            OfferedVerb::Put => "put",
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
        // `Lockable` is deliberately NOT required here, and the omission is
        // the design rather than a gap. A lock is a precondition on the ACT
        // — it reads the body's custody, which is not a property of the
        // object at all — so requiring it would say "only lockable things
        // open", inverting the meaning. `lockable_kinds_are_also_openable`
        // holds the one direction that IS a property relation.
        OfferedVerb::Open | OfferedVerb::Close => [ObjectProperty::Openable].into_iter().collect(),
        // The three verbs The Chattel's Task 12 ships, all gated on the one
        // property spec §3.8 assigns them. `Put`'s CONTAINER requirement
        // (`Encloses`, plus an open lid where the kind has one) is deliberately
        // absent for the reason `Lockable`'s omission above gives: it is a
        // precondition on the act, read against a SECOND object, and this
        // query holds exactly one.
        OfferedVerb::Take | OfferedVerb::Drop | OfferedVerb::Put => {
            [ObjectProperty::Portable].into_iter().collect()
        }
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
/// anchor-side caller passes the anchor's own kind straight through.
///
/// A `kind` absent from [`object_registry`] is treated as carrying the empty
/// property set (`ObjectTraits::default()`), not as offering nothing: **seven
/// of the seventeen kinds on `hornvale_thing::THING_KINDS` carry no property
/// at all** (`altar`, `anvil`, `ground`, `high-seat`, `log`, `loom`,
/// `screen` — the roster minus [`object_registry`]'s ten keys), and
/// `Examine`'s universality (empty required set ⊆ empty
/// property set) must hold for them too, or "universal" would silently mean
/// "universal among the kinds The Offer happened to register."
///
/// **The denominator, not the numerator, was wrong here until the final
/// review, and this sentence was REWRITTEN in Task 2 with the stale figure
/// carried straight through the rewrite.** It said "seven of the *fourteen*
/// kinds a room's grammar can place" — fourteen was the deleted `AnchorKind`
/// enum's variant count, and neither of the two quantities it could have
/// meant is fourteen: the roster is seventeen and the kinds
/// `interior::pattern::INVENTORY` actually places are fifteen (`cave-mouth`
/// and `log` are placed by nothing). Seven is only true against the ROSTER,
/// so the roster is what it now names; against the placeable set the count
/// would be six, since `cave-mouth` is a registry key the grammar never
/// places.
///
/// **The re-key widened the key space from a closed enum to an arbitrary
/// string, and that is a real loss this doc states rather than hides.**
/// `offered_by(KindId("srongbox"))` is a well-typed call returning the
/// empty-set answer, where a misspelt enum variant could not compile.
/// Nothing in this module can close that; what closes it is the totality
/// gate (spec §5.1's G-e), which pins every key [`object_registry`] mints
/// against `hornvale_thing::THING_KINDS`.
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
/// plumb: universal(a fixed ratio-ceiling against the universal REFERENCE_MASS_KG anchor — the ratio already generalizes across species mass)
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
    let reg = object_registry();
    offered_to_traits(&reg.get(&kind).cloned().unwrap_or_default(), body)
}

/// [`offered_to`] reading traits the caller already holds — the body-relative
/// member of the [`offered`] / [`offered_by`] pair above, standing to
/// `offered_to` exactly as `offered` stands to `offered_by` (The Tenon, Task
/// 6).
///
/// **It exists for cost, and the cost it removes is real rather than
/// theoretical.** [`offered_by`] builds a whole [`object_registry`] per call,
/// so asking `offered_to` of every anchor in a room built one
/// `ComponentStore` PER ANCHOR — on `liveness::room_affords_rest`'s path,
/// which runs inside the fatigue fold. A caller that already holds the roster
/// (`liveness::object_roster`, built once per fold) can now ask the same
/// question against it and build nothing.
///
/// **Same answer, by construction.** `offered_to` is now this function
/// applied to the registry's own row for `kind`, with the same
/// `unwrap_or_default()` treatment of an unregistered kind
/// [`offered_by`]'s doc explains — so the two cannot drift, and the public
/// entry point's behaviour is byte-for-byte what it was.
///
/// `pub(crate)`, not `pub`, for the reason [`body_can_use`]'s own narrowing
/// records: a caller supplying its own traits is a caller stating what an
/// object is, and that is a statement this crate should keep inside itself.
pub(crate) fn offered_to_traits(traits: &ObjectTraits, body: &Body) -> BTreeSet<OfferedVerb> {
    offered(traits)
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
/// **Keyed on [`KindId`] since Task 9, and the re-key IS the deliverable
/// rather than a tidy-up (spec §3.6, decision 0397 answering 0369).** The
/// table, [`offered_by`] and [`offered_to`] were re-keyed by Task 7 while
/// this one kept converting an anchor-kind variant on the way in, which
/// left the *gate's* currency at that closed enum — and that currency was
/// exactly 0369's obstacle. It was an interior-object enum with no
/// cave-mouth variant, the mapping out of it was injective over its fourteen
/// variants, and none of them landed on `cave-mouth`, so **before this change
/// there was no argument that named a passage and the gate could not be
/// asked about one at all.** Moving the conversion OUT of this function and
/// to its two anchor-side call sites (`Session::warm`,
/// `Session::examine_chamber`) is the whole mechanism by which the obstacle
/// lifts: an argument that is a thing-kind admits `KindId("cave-mouth")`,
/// which `passage.rs` mints for a `Vertex`/`ChamberAddr` and no anchor
/// enum can express. `an_unencountered_passage_offers_nothing`
/// (`tests/suite/affordance.rs`) is the denial that was unwritable before.
///
/// **What did NOT change, said plainly so the re-key is not over-read.** The
/// gate's *reachability through a live `Session`* is untouched — the
/// paragraph above about unconditional absorption still holds, and no
/// production caller passes `KindId("cave-mouth")` here today
/// (chamber entry gates on the cave mouth's own `openness` fold in
/// `Session::delve_at`, not on this query). What Task 9 bought is
/// ADDRESSABILITY: the gate can now be asked about a passage and answers by
/// denying, which is precisely the half 0369 identified as the wall. A
/// campaign that wants the *other* half must change one of the two things
/// the tripwire paragraph above names, not the key.
///
/// Consumed with synthetic `Knowledge` values (as the tests beside
/// `offered_to`/`offered_by` already do with synthetic `Body` values),
/// because that is the only way to observe the deny branch at all today —
/// not because the deny branch is make-believe: [`Knowledge::default`] is
/// a real, reachable state of the type, and this function's contract must
/// hold for it regardless of whether any current caller happens to produce
/// it.
pub fn offered_to_observer(kind: KindId, body: &Body, known: &Knowledge) -> BTreeSet<OfferedVerb> {
    if has_encountered_any_room(known) {
        offered_to(kind, body)
    } else {
        BTreeSet::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_thing::kinds;

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
    /// implementer's call. Every row names the kind's own handle, so a
    /// mis-spelled handle fails here as well as a missing property.
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
            (kinds::BED, ObjectProperty::SupportsRest),
            (kinds::POOL, ObjectProperty::HoldsLiquid),
            (kinds::VESSEL, ObjectProperty::HoldsLiquid),
            (kinds::THRESHOLD, ObjectProperty::AffordsPassage),
            (kinds::STRONGBOX, ObjectProperty::Encloses),
            (kinds::HEARTH, ObjectProperty::RadiatesHeat),
            // The Chattel, spec §3.8's own table.
            (KindId("key"), ObjectProperty::Portable),
            (kinds::STRONGBOX, ObjectProperty::Openable),
            (KindId("cave-mouth"), ObjectProperty::Openable),
            (KindId("cave-mouth"), ObjectProperty::AffordsPassage),
            (kinds::STRONGBOX, ObjectProperty::Lockable),
            // The Brattice, spec §3.7's own table.
            (kinds::DOOR, ObjectProperty::AffordsPassage),
            (kinds::DOOR, ObjectProperty::Openable),
            (kinds::DOOR, ObjectProperty::Lockable),
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
        for kind in [kinds::STRONGBOX, kinds::ALCOVE] {
            assert!(
                encloses(kind),
                "{kind:?} must carry ObjectProperty::Encloses (spec §3.6, amended)"
            );
        }
        assert!(
            !encloses(kinds::BED),
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
