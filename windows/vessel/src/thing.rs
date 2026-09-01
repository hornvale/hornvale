//! Thing identity and promotion (The Chattel, arc IV.c) — the module that
//! turns a derived anchor into an entity the ledger can hold facts about.
//!
//! **Identity costs nothing until it changes.** A thing's `EntityId` is a pure
//! function of its lineage ([`hornvale_kernel::derive_entity_id`]) — no
//! counter, no allocation, no ledger read — so [`thing_id`] answers "which
//! entity is the strongbox of this room" for a room nothing has ever touched,
//! and [`promote`] only writes when something actually happens. That is what
//! lets a later session find the strongbox an earlier one promoted, the same
//! property `Ledger::reuse_or_mint_entity` already gives a settlement's herder.
//!
//! **The lineage is keyed on `(room facet, thing-kind, ordinal)` and never on
//! the anchor index** (spec §3.2). This is the module's whole compliance with
//! decision 0069, and the reason is mechanical rather than stylistic: an
//! anchor index points into the *fine* layer, which 0069 licenses to
//! regenerate differently forever, so an id derived from one would orphan
//! every fact about the thing the next time an interior epoch moved. A facet
//! is the coarse layer and a kind is authored, so nothing stored here points
//! into the fine layer. A promoted thing's position is its ROOM; which anchor
//! it rests at is re-derived and re-bound on every entry, exactly as
//! `interior_of` already re-derives the whole interior.
//!
//! **The ordinal is the one place the fine layer could still leak in, and
//! today it is always 0.** The ordinal exists to distinguish two strongboxes
//! standing in one room, and its only obvious source would be the interior's
//! own derivation order — a `Vec` order, which is fine-layer and which 0069
//! licenses to change. The Chattel's Task 1 census swept all 60 production
//! gate combinations — `selection(built, cold)` over four, and
//! `selection_for(role, built, cold, populous)` over seven roles × eight
//! boolean triples — and found the **stronger** of the spec's two green
//! results: not merely "no duplicate of a *promotable* kind" (§3.2's branch
//! row 2) but row 1, `CENSUS_ANY_DUPLICATE false` — every `AnchorKind` that
//! appears in a composed interior appears exactly once, promotable or not.
//! So no ordering rule is needed, and `0` is the only ordinal any caller has
//! cause to pass.
//!
//! **That census is now a permanent test, not a scratch measurement**:
//! `interior::pattern::tests::no_production_room_composes_two_anchors_of_one_kind`
//! re-runs the same 60 combinations on every gate. It is the thing standing
//! between a future pattern addition and a silently colliding entity id — see
//! its doc comment for what a red from it means.
//!
//! **Nothing outside this module calls [`thing_id`] or [`promote`] yet.** That
//! is an obligation on the tasks that will (the latent-slot read and the
//! promotion seam), not a property of today's tree: a production caller must
//! pass `0`, and it may do so *only* because the census above holds. A caller
//! that wants a nonzero ordinal is asserting the census has moved, and owes
//! the ordering rule below before it may exist.
//!
//! The parameter is kept rather than hardcoded away
//! because that is an invariant of today's layouts, not of the design: a
//! future interior epoch may compose two of a kind, and the day it does, the
//! ordering rule it needs must be keyed on something a layout epoch cannot
//! change — never on derivation order. A silently unstable ordinal is a wrong
//! entity id, and no gate in this tree can see one.

use hornvale_kernel::{
    ConceptRegistry, EntityId, Facet, FacetError, Fact, INSTANCE_OF, Ledger, LedgerError, Lineage,
    Value, WorldTime,
};

/// Why a promotion could not be carried out.
///
/// **The spec's §3.2 signature for [`promote`] was `Result<EntityId,
/// LedgerError>`, and it cannot be.** Promotion must derive the thing's id
/// before it can commit anything, and deriving it packs a [`Facet`], which is
/// fallible past `MAX_DEPTH` or on a malformed path — a failure
/// [`LedgerError`] has no variant for. Widening the return type is the only
/// way to propagate it; the alternative was to `unwrap` or swallow it, which
/// would turn a malformed room into a panic or a silently wrong entity.
///
/// Neither `Clone` nor `PartialEq` is derived: [`LedgerError`] implements
/// neither, and wrapping it in a `String` to buy them would throw away the
/// variant a caller would want to match on.
#[derive(Debug)]
pub enum ThingError {
    /// The room's facet could not be packed into a `FacetId`.
    Facet(FacetError),
    /// The `instance-of` fact could not be committed.
    Ledger(LedgerError),
}

impl std::fmt::Display for ThingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThingError::Facet(e) => write!(f, "a thing's room does not pack: {e:?}"),
            ThingError::Ledger(e) => write!(f, "committing a thing's instance-of: {e}"),
        }
    }
}

impl From<FacetError> for ThingError {
    fn from(e: FacetError) -> Self {
        ThingError::Facet(e)
    }
}

impl From<LedgerError> for ThingError {
    fn from(e: LedgerError) -> Self {
        ThingError::Ledger(e)
    }
}

/// The role leg of a thing's lineage: the ROOM and the KIND, never the anchor
/// index. That choice is this module's whole compliance with decision 0069 —
/// a facet is the coarse layer and a kind is authored, so nothing stored
/// points into the fine layer, which 0069 licenses to regenerate forever.
///
/// A save-format contract: this string is an input to a derived `EntityId`
/// (see [`hornvale_kernel::Lineage::role`]), so changing its spelling
/// renumbers every thing in every saved world. A change is an epoch, not an
/// edit. `the_thing_role_spelling_is_the_permanent_lineage_key` writes the
/// exact string out as a literal and so **cannot be rebaselined**, which is
/// the entire point of writing it that way: every other test in this module
/// asserts a *relative* property (two derivations agree, three derivations
/// differ), and any injective spelling whatsoever satisfies all of them. The
/// literal is the only assertion that a spelling change can fail.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(identifier-text: return)
pub fn thing_role(facet: &Facet, kind: &str) -> Result<String, FacetError> {
    Ok(format!("thing@{}/{}", facet.pack()?.0, kind))
}

/// The ONE construction of a thing's [`Lineage`], and the reason it is a
/// function rather than two struct literals. Both [`id_for_role`] (which
/// derives) and [`promote_role`] (which mints under the same derivation) route
/// through it — and so, transitively, do [`thing_id`], [`promote`] and
/// `passage::cave_mouth_id` — so the id a caller *predicts* for an unpromoted
/// thing and the id the ledger *mints* for a promoted one cannot disagree:
/// there is nothing left for them
/// to disagree about. The duplicate literal this replaced was a live seam:
/// mutating `promote`'s copy of `ordinal` to `0` left the whole vessel suite
/// green, because every assertion in the module ran through `thing_id`'s copy.
fn thing_lineage<'a>(role: &'a str, ordinal: u16) -> Lineage<'a> {
    Lineage {
        parent: None,
        role,
        ordinal,
    }
}

/// The entity a thing of `kind` standing in `facet` has — whether or not
/// anything has ever promoted it. A pure derivation: nothing is minted, no
/// ledger is read, and two ledgers that have never met agree.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(count: ordinal)
pub fn thing_id(facet: &Facet, kind: &str, ordinal: u16) -> Result<EntityId, FacetError> {
    Ok(id_for_role(&thing_role(facet, kind)?, ordinal))
}

/// The entity a thing whose role leg is already spelled has — [`thing_id`]
/// with the room-keyed half factored out, so a thing whose address is NOT a
/// room can reach the same derivation.
///
/// **`pub(crate)`, and the narrowness is the whole design.** A cave mouth is a
/// [`hornvale_worldgen::chamber::ChamberAddr`], not a [`Facet`], so
/// [`thing_role`] cannot spell it — but a second `Lineage` literal in
/// `passage.rs` would re-open exactly the seam [`thing_lineage`]'s own doc
/// records as having been live once already (mutating one copy's `ordinal`
/// left the whole vessel suite green, because every assertion ran through the
/// other). So `passage.rs` owns its role SPELLING and this module keeps its
/// monopoly on the DERIVATION. A `pub` version would let any caller invent a
/// role string, which is the encoding gap [`located_fact`]'s privacy closes
/// on the other side of this module.
/// type-audit: bare-ok(identifier-text: role), bare-ok(count: ordinal)
pub(crate) fn id_for_role(role: &str, ordinal: u16) -> EntityId {
    hornvale_kernel::derive_entity_id(thing_lineage(role, ordinal))
}

/// Promote the thing of `kind` in `facet` to a ledger entity, committing its
/// `instance-of` fact.
///
/// **Idempotent by construction.** It reaches for
/// [`Ledger::reuse_or_mint_entity`], never `mint_entity` (whose collision
/// assert panics on a repeat) and never `mint_instance` (which calls
/// `mint_entity`): promoting the same `(facet, kind, ordinal)` twice yields
/// one entity and one fact, because the second call finds the id the first
/// derived and `Ledger::commit` dedups the identical fact.
///
/// Committing [`INSTANCE_OF`] by hand is licensed even though
/// `mint_instance`'s doc calls itself "the sole writer of the predicate":
/// `INSTANCE_OF` sits in [`hornvale_kernel::KERNEL_CORE_PREDICATES`], which is
/// exactly the exemption list worldgen's single-writer check is passed, and
/// `Ledger::change_kind` is already a second in-kernel writer of it.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(count: ordinal)
pub fn promote(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    facet: &Facet,
    kind: &str,
    ordinal: u16,
    day: WorldTime,
) -> Result<EntityId, ThingError> {
    promote_role(
        ledger,
        registry,
        &thing_role(facet, kind)?,
        kind,
        ordinal,
        day,
    )
}

/// [`promote`] for a thing whose role leg is already spelled — the same
/// mint-and-commit, reached by a caller whose address is not a [`Facet`].
/// `pub(crate)` for the reason [`id_for_role`]'s doc gives.
///
/// The two must stay one function rather than two that agree, because
/// [`promote`]'s idempotence is a property of the `reuse_or_mint_entity` call
/// and its `INSTANCE_OF` commit sitting together: a second copy could reuse
/// the entity and forget the fact, or mint and dedup differently, and nothing
/// in either caller's own tests would see it.
/// type-audit: bare-ok(identifier-text: role), bare-ok(identifier-text: kind), bare-ok(count: ordinal)
pub(crate) fn promote_role(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    role: &str,
    kind: &str,
    ordinal: u16,
    day: WorldTime,
) -> Result<EntityId, ThingError> {
    let id = ledger.reuse_or_mint_entity(thing_lineage(role, ordinal));
    ledger.commit(
        Fact {
            subject: id,
            predicate: INSTANCE_OF.to_string(),
            object: Value::Text(kind.to_string()),
            place: None,
            day: Some(day),
            provenance: "the-chattel: an anchor was promoted to a thing".to_string(),
        },
        registry,
    )?;
    Ok(id)
}

/// A ROOM as save-format text: the room's packed `FacetId` (decision 0006)
/// rendered as a decimal `u64` string. **The crate's one room-key encoder**,
/// and the only string a [`LOCATED_IN`] fact may carry as a room.
///
/// **There is exactly one of these, not two that agree.**
/// `liveness::room_to_text` — which spells `agent-at`'s object, and whose
/// `room_from_text` is this function's inverse — now calls straight through
/// to here, so `agent-at` and `located-in` cannot drift into two spellings of
/// one room. That mattered because a second encoding would be
/// self-consistent, would round-trip through its own decoder, and would red
/// nothing: the two predicates would simply stop describing the same place,
/// on disk, forever.
///
/// **It propagates [`FacetError`] where `room_to_text` `.expect()`s, and the
/// fallible half has to be the shared one** — a panicking core cannot be
/// widened into a fallible wrapper, only the reverse. It is also the
/// signature this module already uses: [`thing_role`] and [`thing_id`] refuse
/// a room past `MAX_DEPTH` rather than unwrapping it, and an encoder that
/// panicked on the very facet `thing_id` politely refuses would be an
/// inconsistency inside one module. `room_to_text` keeps its panic because
/// the liveness walk has no error channel and its own doc states the
/// invariant it relies on (a scheduled room is always within `MAX_DEPTH`).
///
/// The spelling is a save-format contract on the same terms as
/// [`LOCATED_IN`]'s — it is written into every committed fact — and
/// `a_rooms_key_is_the_permanent_on_disk_spelling` freezes it as a literal.
/// type-audit: bare-ok(identifier-text: return)
pub fn room_key(room: &Facet) -> Result<String, FacetError> {
    Ok(room.pack()?.0.to_string())
}

/// The predicate naming WHERE a thing is — **one** predicate whose object can
/// REPRESENT all three location types (spec §3.3): in a room, in a container,
/// in a hand.
///
/// **Representable is not resolvable, and only two of the three resolve.**
/// [`room_of`] follows a [`Value::Text`] room key and a [`Value::Entity`]
/// holder that is itself located, so "in a room" and "in a container" both
/// answer. "In a hand" does not: a body's own position is committed under
/// [`crate::liveness::AGENT_AT`], a predicate this fold never consults, so a
/// thing whose location is a BODY resolves to `None` — the same answer a
/// thing nobody ever placed gets. The gap is PINNED by a test
/// (`a_thing_held_by_a_body_has_no_room_today`).
///
/// **THIS PARAGRAPH USED TO END "Nothing puts a thing in a hand until Task
/// 12", AND TASK 12 HAS SHIPPED.** `Session::take` commits exactly this
/// shape now. The fallback was still not written, and the reason CHANGED
/// rather than lapsed — see [`room_of`]'s own doc, which carries the
/// measurement.
///
/// **The location rides in the fact's `object`, never in [`Fact::place`].**
/// `Fact::place` is an `Option<EntityId>` ("the entity where this fact was
/// observed") and could not hold a room key at all; `object` is a [`Value`],
/// so a room rides as [`Value::Text`] and a holder — a chest, a hand — rides
/// as [`Value::Entity`]. Two shapes of ONE field is exactly what makes
/// transitivity ("a key in a chest in a room is in the room") expressible in
/// [`room_of`] rather than re-asserted at every call site. This follows
/// `agent_at_fact`, which puts the place in the object. (`passage`'s own
/// `cleared_fact` was the second precedent cited here; Task 8 retired it —
/// a cave mouth's address is now a lineage role rather than a fact object,
/// so `agent_at_fact` is the one live example left.)
///
/// Non-functional and append-only: a thing moves, and each move is one dated
/// fact. The read is [`location_of`] — as of a day — never
/// [`hornvale_kernel::Ledger::latest_value_of`], which answers "where is it
/// now" and would let a replayed past see a move that had not happened yet.
///
/// Registered PER-SESSION (never at genesis), beside `AGENT_AT` in
/// `Session::start`. An out-of-session reader registers
/// it into its own registry the way `windows/lab` does for `AGENT_AT`;
/// `ConceptRegistry::register_predicate` is idempotent for an identical
/// definition, so the second registration is a no-op. A world saved by
/// `possess --out` carries the registration with the facts it licenses
/// (`Session::into_played_world`, decision 0368).
///
/// **The spelling is a save-format contract**, for the same reason
/// `passage::addr_key`'s is: it is written into every committed [`Fact`] and
/// into a saved world's registry, so a rename leaves every old fact in the
/// file and invisible to every fold that reads it — a thing silently loses
/// its location and nothing goes red, because tests use the constant on both
/// sides. `the_location_predicate_spellings_are_permanent_on_disk_keys`
/// writes the literal out for that reason; a change is an epoch
/// (`located-in/v2`), never an edit.
/// type-audit: bare-ok(identifier-text)
pub const LOCATED_IN: &str = "located-in";

/// The doc string [`LOCATED_IN`] is registered with — **a constant because a
/// predicate's doc is save-format state, not a comment.**
///
/// [`hornvale_kernel::PredicateDef`] derives `PartialEq` over
/// `{name, functional, doc}` and `ConceptRegistry::register_predicate` is
/// idempotent only for an *identical* definition, so two registrations of
/// `located-in` whose docs differ by one word are a
/// `RegistryError::ConflictingDefinition` — which `Session::start` meets
/// behind an `.expect`, i.e. as a panic. A world saved by `possess --out`
/// carries the registration (decision 0368), so the two registrations that
/// must agree are not even in one process: they are a saved world's and a
/// later session's.
///
/// Task 5 shipped that divergence inside its own file on day one —
/// `session.rs` registered "…: a room, a container, or a hand" while this
/// module's test helper registered "where a thing is on a day". Neither red,
/// because the two registries never met. One constant removes the
/// possibility rather than testing for it, and
/// `tests/suite/session.rs`'s `a_sessions_registry_carries_the_shared_predicate_docs`
/// pins that `Session::start` actually reaches for it.
/// type-audit: bare-ok(prose)
pub const LOCATED_IN_DOC: &str = "where a thing is on a day: a room, a container, or a hand";

/// The predicate recording that a thing was opened or closed.
///
/// **ABSENCE MEANS "whatever the seed drew"** (spec §3.3), which is why
/// [`is_open`] returns `Option<bool>` and not `bool`: a thing nobody has
/// touched has no openness fact, and the answer belongs to the generator, not
/// to this fold. Collapsing the `None` into `false` here would state that
/// every untouched door in the world is shut.
///
/// Non-functional and append-only — a chest may be opened, closed and opened
/// again — the latest posting at or before the day is the answer, never a
/// short-circuit on any posting ever made. That is what made this predicate
/// able to absorb restricted passage in Task 8, where The Latch's monotone
/// `passage-cleared` could not: decision 0396 supersedes 0367 by folding the
/// cave mouth through THIS rule.
///
/// `openness` is a LEDGER FACT about one thing. It is not `Openable`, the
/// affordance property saying a KIND can be opened at all; the two are
/// different objects and a plan draft that read "openness (Task 7's
/// `Openable`)" conflated them.
///
/// Its spelling is a save-format contract on the same terms as
/// [`LOCATED_IN`]'s.
/// type-audit: bare-ok(identifier-text)
pub const OPENNESS: &str = "openness";

/// The doc string [`OPENNESS`] is registered with, a constant for exactly the
/// reason [`LOCATED_IN_DOC`] is — and it had diverged too: `session.rs`
/// registered "whether a thing was open on a day" against the test helper's
/// "whether a thing is open".
/// type-audit: bare-ok(prose)
pub const OPENNESS_DOC: &str = "whether a thing was open on a day";

/// The predicate recording that a thing was locked or unlocked — **a
/// SEPARATE state from [`OPENNESS`], which is the whole of decision 0399.**
///
/// The Chattel shipped with one boolean doing both jobs: a strongbox was
/// "locked" iff `open_or_close` could not find a key in the body's custody at
/// the moment of asking, so `close` — which shuts a lid and nothing else —
/// re-locked the chest. Put the key inside, shut the lid, and the world was
/// unrecoverable: `open` refused for want of a key, and `take a key` refused
/// because the key was shut away. Two states with one variable, and the dead
/// end was where they disagreed.
///
/// **Absence means "whatever the seed drew"**, on exactly [`OPENNESS`]'s
/// terms and for the same reason: [`is_locked`] returns `Option<bool>` and
/// the authored default lives in the reader that knows what KIND it is
/// asking about (`Session::container_is_locked`, where a
/// [`crate::affordance::ObjectProperty::Lockable`] kind defaults to locked
/// and everything else has no lock at all).
///
/// Non-functional, like the two predicates above it: a lock is a thing that
/// changes, and the answer is the latest posting at or before the day. **No
/// verb in this campaign writes `true`** — a lock needs the key IN THE LOCK,
/// which is a location no verb can put a key in (decision 0399, clause 4), so
/// the only posting anything commits is the `false` that `open` writes when a
/// key in custody turns a seeded lock. The `true` direction exists in this
/// fold so a future `lock` verb has one writer to reach for rather than a
/// second predicate.
///
/// Its spelling is a save-format contract on the same terms as
/// [`LOCATED_IN`]'s.
/// type-audit: bare-ok(identifier-text)
pub const LOCKEDNESS: &str = "lockedness";

/// The doc string [`LOCKEDNESS`] is registered with, a constant for exactly
/// the reason [`LOCATED_IN_DOC`] and [`OPENNESS_DOC`] are: a predicate's doc
/// is save-format state, and a saved world's registry must agree with a later
/// session's or `register_predicate` is a `ConflictingDefinition`.
/// type-audit: bare-ok(prose)
pub const LOCKEDNESS_DOC: &str = "whether a thing was locked on a day";

/// The fact committed when `thing` comes to rest in `room` on `day`.
///
/// **The room is encoded HERE, from a typed [`Facet`], and that is the whole
/// point of this function existing beside [`located_in_holder_fact`].** The
/// shipped API took an already-encoded `Value`, which left NOBODY owning the
/// spelling of a room in a `located-in` fact: `agent_at_fact` takes a `&Facet`
/// and encodes inside (as did `passage::cleared_fact`, until Task 8 retired
/// it), and this one asked its caller to have done it. A caller
/// that spelled a room its own way would have produced facts that are
/// self-consistent, that its own reader resolves, and that no gate can
/// distinguish from the real thing. There is no public constructor taking a
/// bare `Value` any more, so that shape is not merely discouraged — it is
/// unreachable from outside this module.
pub fn located_in_room_fact(
    thing: EntityId,
    room: &Facet,
    day: WorldTime,
) -> Result<Fact, FacetError> {
    Ok(located_fact(thing, Value::Text(room_key(room)?), day))
}

/// The fact committed when `thing` comes to rest in `holder` — a chest, and
/// since Task 12 a body — on `day`. The holder rides as a [`Value::Entity`],
/// which is what [`room_of`] follows transitively.
pub fn located_in_holder_fact(thing: EntityId, holder: EntityId, day: WorldTime) -> Fact {
    located_fact(thing, Value::Entity(holder), day)
}

/// The shared envelope both public constructors above are made of, and the
/// reason it is PRIVATE: the two `Value` shapes a location may take are the
/// campaign's whole "three location types, one predicate" claim, and a public
/// function taking an unconstrained `Value` would re-open the encoding gap
/// they close. The in-module tests still reach it, which is how the malformed
/// arm of [`room_of`] is exercised at all.
fn located_fact(thing: EntityId, location: Value, day: WorldTime) -> Fact {
    Fact {
        subject: thing,
        predicate: LOCATED_IN.to_string(),
        object: location,
        place: None,
        day: Some(day),
        provenance: "the-chattel: a thing came to rest somewhere".to_string(),
    }
}

/// The fact committed when `thing` is opened (`open == true`) or closed.
/// type-audit: bare-ok(flag: open)
pub fn openness_fact(thing: EntityId, open: bool, day: WorldTime) -> Fact {
    Fact {
        subject: thing,
        predicate: OPENNESS.to_string(),
        object: Value::Flag(open),
        place: None,
        day: Some(day),
        provenance: "the-chattel: a thing was opened or closed".to_string(),
    }
}

/// The fact committed when `thing` is locked (`locked == true`) or unlocked.
/// type-audit: bare-ok(flag: locked)
pub fn lockedness_fact(thing: EntityId, locked: bool, day: WorldTime) -> Fact {
    Fact {
        subject: thing,
        predicate: LOCKEDNESS.to_string(),
        object: Value::Flag(locked),
        place: None,
        day: Some(day),
        provenance: "the-chattel: a thing was locked or unlocked".to_string(),
    }
}

/// Record that the thing of `kind` in `facet` was opened (`open == true`) or
/// closed, on `day` — promoting it first, so the thing exists in the ledger
/// with its `instance-of` kind and not merely as an id some fold derives.
///
/// **The one writer, in both directions, for both angles of §3.7's "same
/// mechanism seen from three angles".** [`crate::passage::set_openness`] is
/// the cave mouth's entry point and calls [`set_openness_role`] — this
/// function's own body, minus the room-keyed spelling — so a passage and a
/// container are opened by one promote-and-commit pair rather than two that
/// agree. A second copy could reuse the entity and forget the fact, or
/// promote against an id it derived itself, and nothing in either caller's
/// tests would see it: the ledger would read correctly to its own author and
/// to nothing else.
///
/// **Idempotent within a day and not across days**, exactly as
/// `passage::set_openness`'s own doc records: [`promote`]'s idempotence is
/// `Ledger::commit`'s dedup of an IDENTICAL fact and [`Fact`] compares its
/// `day`, so two calls on different days leave one entity and two
/// `instance-of` facts saying the same thing twice. `Session::open_or_close`
/// refuses a no-op ("it is already open") before reaching here, so the
/// redundant pair is not reachable through the verb; the caveat is recorded
/// because a future caller that does not check first would accumulate one
/// per call.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(count: ordinal), bare-ok(flag: open)
pub fn set_openness(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    facet: &Facet,
    kind: &str,
    ordinal: u16,
    open: bool,
    day: WorldTime,
) -> Result<EntityId, ThingError> {
    set_openness_role(
        ledger,
        registry,
        &thing_role(facet, kind)?,
        kind,
        ordinal,
        open,
        day,
    )
}

/// [`set_openness`] for a thing whose role leg is already spelled —
/// `pub(crate)` for the reason [`id_for_role`]'s doc gives, and reached by
/// `passage.rs`, whose address is a `ChamberAddr` rather than a [`Facet`].
/// type-audit: bare-ok(identifier-text: role), bare-ok(identifier-text: kind), bare-ok(count: ordinal), bare-ok(flag: open)
pub(crate) fn set_openness_role(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    role: &str,
    kind: &str,
    ordinal: u16,
    open: bool,
    day: WorldTime,
) -> Result<EntityId, ThingError> {
    let id = promote_role(ledger, registry, role, kind, ordinal, day)?;
    ledger.commit(openness_fact(id, open, day), registry)?;
    Ok(id)
}

/// Record that the thing of `kind` in `facet` was locked (`locked == true`) or
/// unlocked, on `day` — [`set_openness`]'s sibling, promoting first for the
/// same reason and carrying the same across-days caveat.
///
/// **There is no `_role` variant and there should not be one until something
/// needs it.** [`set_openness_role`] exists because a passage's address is a
/// `ChamberAddr` rather than a [`Facet`]; no passage has a lock, and inventing
/// the seam before its caller would be a second entry point nothing keeps
/// honest.
///
/// **`Ledger::commit` dedups an identical fact, so a caller that writes the
/// same value twice on one day writes once** — which is why
/// `Session::open_or_close` asks [`is_locked`] before reaching here rather
/// than unlocking unconditionally. That check is not merely thrift: the reply
/// is computed before the commit, so a caller that stopped asking would be
/// relying on a write it never verified.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(count: ordinal), bare-ok(flag: locked)
pub fn set_lockedness(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    facet: &Facet,
    kind: &str,
    ordinal: u16,
    locked: bool,
    day: WorldTime,
) -> Result<EntityId, ThingError> {
    let id = promote(ledger, registry, facet, kind, ordinal, day)?;
    ledger.commit(lockedness_fact(id, locked, day), registry)?;
    Ok(id)
}

/// The last object committed for (`subject`, `predicate`) **at or before**
/// `day` — the one fold both [`location_of`] and [`is_open`] are made of.
///
/// **The `<= day` filter is the whole point**, and it is what distinguishes a
/// fold from a mutable flag: a read over the entire history would look
/// chronologically PAST the instant being asked about, so a replayed past
/// would see a move that had not happened yet. Same discipline as
/// `passage::effective_state` and as `last_fact_day_at_or_before` in the
/// liveness walk (which is private, so it is a discipline to copy and not a
/// function to call), and the rule decision 0366 already states.
///
/// **The tie-break, which neither the spec nor the plan stated: two facts at
/// ONE instant resolve by commit order, last posting wins.** That is
/// `Ledger::latest_value_of`'s rule, and keeping it here means a same-instant
/// pair reads the same way whichever of the two reads a caller reaches for.
/// It is bought by `seen <= d` rather than `seen < d` in the fold below — a
/// one-character property, so it is pinned by its own test
/// (`the_last_posting_at_one_instant_wins`).
///
/// A fact with no `day` is skipped rather than treated as ancient: an undated
/// location is not a location as of any day.
fn latest_object_at_or_before<'a>(
    ledger: &'a Ledger,
    subject: EntityId,
    predicate: &str,
    day: WorldTime,
) -> Option<&'a Value> {
    let mut best: Option<(WorldTime, &'a Value)> = None;
    for fact in ledger.facts_of(subject, predicate) {
        let Some(d) = fact.day else { continue };
        if d > day {
            continue;
        }
        if best.is_none_or(|(seen, _)| seen <= d) {
            best = Some((d, &fact.object));
        }
    }
    best.map(|(_, object)| object)
}

/// Where `thing` was as of `day`: the latest [`LOCATED_IN`] object at or
/// before that instant, or `None` if nothing has ever placed it.
///
/// A [`Value::Text`] answer is a room key; a [`Value::Entity`] answer is a
/// holder, and [`room_of`] is the read that follows those to the room.
pub fn location_of(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<Value> {
    latest_object_at_or_before(ledger, thing, LOCATED_IN, day).cloned()
}

/// The room `thing` is in as of `day`, following containment transitively: a
/// key in a chest in a room is in the room.
///
/// Transitivity is a property of the `in` relation itself (RCC-8, declared in
/// The Hearth §5), so it is resolved HERE and never re-asserted by each
/// caller. The walk stops at the first [`Value::Text`]; a
/// [`Value::Entity`] is a holder to follow; anything else is a malformed
/// location and yields `None` rather than a panic.
///
/// **A chest resolves; a HAND does not, and the difference is which predicate
/// carries the holder's own position.** A chest is located by a
/// [`LOCATED_IN`] fact, so the next hop is a fact this walk reads. A body is
/// positioned by [`crate::liveness::AGENT_AT`], which this walk never
/// consults, so a thing held by a body walks one hop and then finds no
/// location at all — `None`, indistinguishable from a thing nobody placed.
///
/// # Task 12 arrived, did not write the fallback, and the reason is not the old one
///
/// This paragraph used to say the omission was "deliberate for now: nothing
/// commits a held thing until Task 12", and instructed whoever wrote that
/// task to revisit it. `Session::take` commits a held thing today, so that
/// reason has lapsed. Two others replaced it, and the second is the one that
/// matters:
///
/// 1. **Nothing calls this function in production.** Grepped across the
///    workspace at Task 12: every call site is a test in this module. The
///    custody verbs read [`location_of`], [`held_by`] and [`lying_in`]
///    directly, because each asks about a DIRECT location and none of them
///    wants a transitive one.
/// 2. **The fallback would answer in a different key space, and on the
///    commonest walk it would not answer at all.** A body's `AGENT_AT`
///    object is its WALK-BAND locale facet: `Session::commit_agent_at` is
///    reached only from `go` and `back`, and `Session::enter` commits no
///    position at all, so the fact does not move while a possession walks
///    from chamber to chamber. A thing set down indoors is keyed on
///    `Session::chamber_facet_here` — a chamber facet, twenty-one path
///    digits deep. Measured on seed 1, one turn apart in one played walk:
///    the body's `agent-at` room key was `540999680` while the chamber it
///    stood in packed to `141819825979456`. Both are [`Value::Text`] and
///    [`room_key`] spells both, so a caller comparing "where is the key I am
///    holding" against "where is the key I just put down" would read two
///    incomparable numbers and conclude the key had moved rooms.
///
///    **The weaker half of that is what used to be written here, and the
///    stronger one is measured** (fix round 1). A mismatched key is the
///    BEST case — it needs the possession to have used `go` or `back` at
///    all. A walk that only goes indoors commits **zero** `agent-at` facts
///    for the body, so the fallback would return `None`: not a wrong room, no
///    room. Measured through the shipped CLI on seed 1 — `enter`, three
///    `enter further in`s, `take a key`, `possess --out` — the saved world
///    holds one `located-in` fact naming the body as holder and
///    `agent-at` facts about that body: **0**. A `room_of` that fell back to
///    `AGENT_AT` would answer `None` for a key a player is visibly holding,
///    which is the same answer it gives for a key nobody ever placed.
///
/// Closing it therefore needs a decision about what a body's room IS while
/// it is indoors — which is a position-model question, not a fold question.
/// `a_thing_held_by_a_body_has_no_room_today` still fails the moment a
/// fallback lands, so the obligation is intact; only its owner has moved.
///
/// **A cycle terminates.** `visited` is a [`std::collections::BTreeSet`] (no
/// `HashSet` — decision 0005's deterministic-collections ban; 0004 is the
/// *dependency* allowlist, and citing it here sent a reader to the wrong
/// record), and a holder already seen ends the walk with `None`. Nothing in
/// this campaign creates a containment cycle, which is exactly why nothing
/// else would catch one.
/// type-audit: bare-ok(identifier-text: return)
pub fn room_of(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<String> {
    let mut visited = std::collections::BTreeSet::new();
    let mut current = thing;
    loop {
        if !visited.insert(current) {
            return None;
        }
        match location_of(ledger, current, day)? {
            Value::Text(room) => return Some(room),
            Value::Entity(holder) => current = holder,
            _ => return None,
        }
    }
}

/// Every thing whose location is `holder` itself as of `day` — what a chest
/// contains, and (since Task 12) what a body carries.
///
/// **The O-shaped question, and the only one in this module that is.** Every
/// other fold here starts from a thing and asks where it is; this starts from
/// a place and asks what is in it, which the flat ledger could not answer at
/// all before `Ledger::query_by_object`'s OSP index. It is `O(log n + k)` in
/// the number of facts naming this holder, not a scan.
///
/// **Two filters, and the second is the one that makes it a fold rather than
/// a search.** `query_by_object` finds every fact that ever named `holder`,
/// including one a later fact superseded — a key put into a chest and taken
/// out again still has its putting-in fact forever. So each candidate is
/// re-asked through [`location_of`], whose answer is the LATEST posting at or
/// before `day`, and kept only if that answer is still this holder. Trusting
/// the O-index alone would report everything ever placed here, which is a
/// different question and a wrong one.
///
/// Deduplicated by construction: the result is collected through a
/// [`std::collections::BTreeSet`], so a thing placed here twice appears once,
/// in `EntityId` order — deterministic, and never ledger order, which would
/// make the answer depend on how the history happened to be written.
///
/// DIRECT containment only, never [`room_of`]'s transitive reading: a key in
/// a chest in a room is *in the chest*, and a caller asking what a room holds
/// wants the chest, not its contents spilled onto the floor. That is the same
/// asymmetry [`is_latent`] states from the other side.
pub fn held_by(ledger: &Ledger, holder: EntityId, day: WorldTime) -> Vec<EntityId> {
    let candidates: std::collections::BTreeSet<EntityId> = ledger
        .query_by_object(&Value::Entity(holder))
        .filter(|fact| fact.predicate == LOCATED_IN)
        .filter(|fact| fact.day.is_some_and(|d| d <= day))
        .map(|fact| fact.subject)
        .collect();
    candidates
        .into_iter()
        .filter(|&thing| location_of(ledger, thing, day) == Some(Value::Entity(holder)))
        .collect()
}

/// What `holder` is holding, as the NOUNS prose says them — [`held_by`] with
/// each thing's kind resolved to its word.
///
/// **One fold, three readers.** `Session::carried_by` (the wire and the
/// verbs), `examine`'s creature reply, and the chart's agent marks all call
/// this rather than each resolving kinds themselves, because three
/// independent copies is exactly how a pane and a verb end up disagreeing
/// about whose hands hold what.
/// type-audit: bare-ok(identifier-text: return)
pub fn carried_nouns(
    ledger: &Ledger,
    holder: EntityId,
    day: WorldTime,
) -> Vec<(EntityId, &'static str)> {
    held_by(ledger, holder, day)
        .into_iter()
        .filter_map(|thing| {
            let noun = crate::chamber_prose::noun_for_label(ledger.kind_of(thing)?)?;
            Some((thing, noun))
        })
        .collect()
}

/// Every thing whose location is `room` ITSELF as of `day` — what a player
/// left lying on a chamber's floor (The Chattel, Task 12).
///
/// **[`held_by`]'s sibling, and the second O-shaped question in this module.**
/// It differs in exactly one thing: a room rides as a [`Value::Text`] key
/// where a holder rides as a [`Value::Entity`], so the [`Ledger::
/// query_by_object`] probe is built from [`room_key`] and the fold is
/// otherwise identical — the same two filters, the same re-ask through
/// [`location_of`] so a superseded posting cannot answer, the same
/// [`std::collections::BTreeSet`] dedup into `EntityId` order.
///
/// **It exists because a DROPPED thing has no anchor.** A room's offer list
/// is the grammar's — `interior_of` composes anchors — and a key carried in
/// from two rooms away is composed by nothing here. Without this fold a
/// player could set a thing down in a room whose grammar never held one and
/// never pick it up again, which is the "and is offered there on the next
/// entry" half of the campaign's own acceptance claim.
///
/// DIRECT residence only, on [`held_by`]'s own terms: a key inside a chest
/// standing in this room is *in the chest*, and this answers about the floor.
///
/// Fallible for the reason every room-keyed function in this module is: a
/// facet past `MAX_DEPTH` does not pack, and refusing beats unwrapping.
pub fn lying_in(
    ledger: &Ledger,
    room: &Facet,
    day: WorldTime,
) -> Result<Vec<EntityId>, FacetError> {
    let here = Value::Text(room_key(room)?);
    let candidates: std::collections::BTreeSet<EntityId> = ledger
        .query_by_object(&here)
        .filter(|fact| fact.predicate == LOCATED_IN)
        .filter(|fact| fact.day.is_some_and(|d| d <= day))
        .map(|fact| fact.subject)
        .collect();
    Ok(candidates
        .into_iter()
        .filter(|&thing| location_of(ledger, thing, day).as_ref() == Some(&here))
        .collect())
}

/// Whether `thing` was open as of `day`, or `None` if no [`OPENNESS`] fact
/// exists at or before it — which means "whatever the seed drew", never
/// "shut". See [`OPENNESS`].
/// type-audit: bare-ok(flag: return)
pub fn is_open(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<bool> {
    match latest_object_at_or_before(ledger, thing, OPENNESS, day) {
        Some(Value::Flag(open)) => Some(*open),
        _ => None,
    }
}

/// Whether `thing` was locked as of `day`, or `None` if no [`LOCKEDNESS`] fact
/// exists at or before it — "whatever the seed drew", never "unlocked". See
/// [`LOCKEDNESS`], and `Session::container_is_locked` for where the authored
/// default that resolves the `None` actually lives.
///
/// **A locked thing may be open and an unlocked one may be shut**, which is
/// the point of this fold being separate from [`is_open`]: decision 0399
/// separates the two states precisely so that shutting a lid cannot turn a
/// key.
/// type-audit: bare-ok(flag: return)
pub fn is_locked(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<bool> {
    match latest_object_at_or_before(ledger, thing, LOCKEDNESS, day) {
        Some(Value::Flag(locked)) => Some(*locked),
        _ => None,
    }
}

/// Whether the thing of `kind` and `ordinal` belonging to `facet` **is still
/// where the grammar puts it**, as of `day` — the campaign's first NEGATIVE
/// fold, and HALF of the latency rule.
///
/// # This answers one conjunct of two, and the other one is the caller's
///
/// Spec §3.4 states latency as a conjunction:
///
/// ```text
///   latent(facet, kind, n)  ==  the grammar offers it        <- THE CALLER'S HALF
///                           AND no committed located-in fact places
///                               thing_id(facet, kind, n) anywhere but this
///                               room                          <- THIS FUNCTION
/// ```
///
/// **Nothing in this signature can evaluate the first conjunct.** "Does the
/// grammar offer a strongbox here" is answered only by
/// [`crate::interior::interior_of`] or `chamber_interior_of`, both of which
/// need a `&dyn Terrain`; this function holds no terrain, no seed and no
/// world. Widening the signature to take one was considered and rejected:
/// the caller **already** holds the [`crate::interior::Interior`], because
/// enumerating it is where the offer list comes from in the first place. So
/// the shape the design describes — and the shape that makes the cost below
/// the right cost — is *enumerate the interior, then ask this per slot*:
///
/// ```text
///   for id in interior.ids() {
///       let kind = kind_of(interior.anchor(id).kind);      // the grammar's half
///       if is_latent(ledger, &room, kind, 0, day)? { offer(id) }
///   }
/// ```
///
/// A caller that skips the enumeration and asks this alone gets `true` for a
/// kind the room never offered, which is not a bug in this fold — it is the
/// missing conjunct.
///
/// **The `0` in that sketch is licensed by a measurement, not by taste.**
/// Every production room composes at most one anchor of any kind, so the
/// ordinal is always 0 — and that invariant is held by
/// `no_production_room_composes_two_anchors_of_one_kind`
/// (`crate::interior::pattern`'s `mod tests`), whose doc explains that a red
/// there means two things in one room now derive the SAME entity id, and
/// says to return to the spec rather than choose an ordering rule in a task.
/// Copy the pointer along with the sketch.
///
/// # "Anywhere but this room", not "has ever been touched"
///
/// A thing that was **promoted and put back** is still here, and so is a
/// thing that was promoted and never moved at all. Only a [`LOCATED_IN`]
/// fact naming somewhere ELSE takes it out of the room's offer list.
/// Answering "does any fact about it exist" instead would make every
/// promoted fixture vanish from its own room the instant a player opened it,
/// and it would pass a test that only ever carries things away —
/// `a_thing_put_back_is_still_here` and
/// `a_promoted_thing_that_never_moved_is_still_here` exist to separate the
/// two implementations.
///
/// # It reads the DIRECT location, never [`room_of`]
///
/// A key dropped into a chest that stands in this room is *in the room* by
/// [`room_of`]'s transitive reading, and is NOT latent here: it is in the
/// chest, and a room that went on offering it loose on the floor would be
/// showing the same key twice. So the comparison is against
/// [`location_of`]'s own answer — the thing's own [`LOCATED_IN`] posting —
/// and a [`Value::Entity`] holder of any sort ends latency. A malformed
/// location (neither a room key nor a holder) ends it too: a fact placing
/// the thing somewhere unreadable is still a fact that it was moved, and
/// re-offering it would mint a second copy.
///
/// **That argument is asymmetric, and Task 12 inherited the gap knowingly.**
/// The grammar ALSO expresses containment — `the-fire` is
/// `Attach::Within(AnchorKind::Alcove)` and `compose` sets `Anchor.within`
/// from it — and `offers_of` correctly offers both the alcove and the hearth
/// within it. But this fold suppresses a thing whose containment is stated
/// in the LEDGER, so committing a holder fact that merely restates what the
/// grammar already says would stop the room offering an anchor the grammar
/// still places there.
///
/// **`Session::put_in` is now a verb that commits exactly such a fact, and
/// the wrong answer it could produce is closed one layer up rather than
/// here.** `put a key in a strongbox` posts `located-in(key, strongbox)` — a
/// key whose grammar already puts it `Within(Strongbox)` — so the key stops
/// being latent in that room, which is correct (it is in the chest, not on
/// the floor) but would make the room refuse to give it back. `Session::take`
/// therefore reads a second arm: a thing whose direct location is a container
/// ANCHORED IN THIS ROOM is takeable if that container admits. This fold is
/// unchanged; the conjunct that was always the caller's stayed the caller's.
///
/// # Cost
///
/// **On an INDEXED ledger:** one SPO-indexed lookup per slot per room entry
/// — `Ledger::facts_of` is `O(log n + k)` in `k`, the number of `located-in`
/// facts about *this one thing*, which is 0 for every untouched slot. The id
/// is derived *before* the read ([`thing_id`] consults no ledger), which is
/// what makes it a lookup rather than a search. This is the path Task 1
/// measured at ~90 ns, on a ledger obtained through `into_played_world` —
/// that is, after commits.
///
/// **On an UNINDEXED one it is a whole-ledger scan, and this paragraph said
/// otherwise.** It read "nothing costs per fact in the ledger", which is
/// false in a state a live session can actually be in. `Ledger::facts_of`
/// takes `&self` and therefore *cannot* build the index; `index` is
/// `#[serde(skip)]` and is built only by the three `&mut self` paths
/// (`mint_entity`, `reuse_or_mint_entity`, `commit`), so a freshly
/// deserialized ledger falls through to `naive_facts_of` — a filter over
/// every fact it holds. The kernel documents this state outright in
/// `index_is_absent_until_first_use_then_complete`.
///
/// **The forward consequence is Task 12's and it now has a live caller:**
/// `Session::take` asks this once per attempt, not once per slot, so a
/// `possess --world` session that has committed nothing yet pays ONE
/// whole-ledger scan on the first take and none afterwards — the first
/// `Ledger::commit` builds the index. Task 1's own played session held
/// 22,880 facts. The paragraph this replaces said "there is no live caller
/// yet"; there is, and the cost it pays is bounded by the number of takes
/// rather than by the number of slots, because nothing in production
/// enumerates an interior through this fold.
///
/// Both [`Facet::pack`] calls behind it are fallible, so an unpackable room
/// is refused rather than unwrapped — the same contract [`thing_id`] and
/// [`room_key`] already carry. `pack` has TWO failure modes, not the one
/// `latency_in_a_room_that_does_not_pack_is_refused` exercises:
/// `DepthExceedsCap` past `MAX_DEPTH`, and `Invalid` for a `face >= 20` or a
/// path digit `>= 4`.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(count: ordinal), bare-ok(flag: return)
pub fn is_latent(
    ledger: &Ledger,
    facet: &Facet,
    kind: &str,
    ordinal: u16,
    day: WorldTime,
) -> Result<bool, FacetError> {
    let thing = thing_id(facet, kind, ordinal)?;
    let here = room_key(facet)?;
    Ok(match location_of(ledger, thing, day) {
        // Nothing ever placed it: the untouched slot, and the common case.
        None => true,
        // Placed, and placed HERE — promoted and put back, or promoted and
        // never moved.
        Some(Value::Text(room)) => room == here,
        // Another room, a holder, or a malformed location: gone from here.
        Some(_) => false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn facet(face: u8, path: &[u8]) -> Facet {
        Facet {
            face,
            path: path.to_vec(),
        }
    }

    /// The same (room, kind, ordinal) derives the same id in two ledgers that
    /// have never met — which is what lets a session find the strongbox a
    /// previous session promoted, and is the same property
    /// `reuse_or_mint_entity`'s own doc claims for a settlement's NPC.
    #[test]
    fn a_things_id_is_a_pure_function_of_room_kind_and_ordinal() {
        let f = facet(3, &[1, 2]);
        let a = thing_id(&f, "strongbox", 0).expect("a shallow facet packs");
        let b = thing_id(&f, "strongbox", 0).expect("a shallow facet packs");
        assert_eq!(a, b);
    }

    /// Distinct rooms, kinds and ordinals never collide. A collision would
    /// make two things one thing, and every fact about either would key to
    /// the other. Each of the three trailing entries differs from the first
    /// in exactly ONE lineage leg, so this fails if any leg stops reaching
    /// the derivation.
    #[test]
    fn distinct_addresses_never_share_an_id() {
        let f1 = facet(3, &[1, 2]);
        let f2 = facet(3, &[1, 3]);
        let ids = [
            thing_id(&f1, "strongbox", 0).unwrap(),
            thing_id(&f2, "strongbox", 0).unwrap(),
            thing_id(&f1, "key", 0).unwrap(),
            thing_id(&f1, "strongbox", 1).unwrap(),
        ];
        let uniq: std::collections::BTreeSet<_> = ids.iter().collect();
        assert_eq!(uniq.len(), ids.len(), "two addresses share an id: {ids:?}");
    }

    /// A room too deep to pack is refused, not unwrapped. `MAX_DEPTH` is the
    /// kernel's cap; a path past it is the one input that makes identity
    /// underivable, and the whole reason [`ThingError`] exists.
    #[test]
    fn a_room_that_does_not_pack_is_refused() {
        let too_deep = facet(3, &[1u8; 64]);
        assert_eq!(
            thing_id(&too_deep, "strongbox", 0),
            Err(FacetError::DepthExceedsCap)
        );
    }

    /// Promotion is idempotent: promoting twice yields one entity, not two.
    /// This is the whole reason `reuse_or_mint_entity` exists rather than
    /// `mint_entity`, whose collision assert would panic on the second call.
    #[test]
    fn promoting_twice_yields_one_entity() {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        let mut ledger = Ledger::default();

        let f = facet(3, &[1, 2]);
        let day = WorldTime::from_std_days(5.0).expect("5 days is in range");

        let first = promote(&mut ledger, &reg, &f, "strongbox", 0, day).unwrap();
        let second = promote(&mut ledger, &reg, &f, "strongbox", 0, day).unwrap();

        assert_eq!(
            first, second,
            "a second promotion must find the first thing"
        );
        assert_eq!(
            first,
            thing_id(&f, "strongbox", 0).unwrap(),
            "a promoted thing must carry the id `thing_id` derives for it"
        );
        assert_eq!(ledger.entity_count(), 1, "two entities were minted");
        assert_eq!(
            ledger.find(INSTANCE_OF).count(),
            1,
            "a second instance-of fact was committed"
        );
    }

    /// The role string's exact on-disk spelling, written out as a literal.
    ///
    /// **This test cannot be rebaselined, which is the entire point of writing
    /// it out.** Every other assertion about identity in this module is
    /// *relative* — `a_things_id_is_a_pure_function_of_room_kind_and_ordinal`
    /// asks whether two derivations agree,
    /// `distinct_addresses_never_share_an_id` asks whether three derivations
    /// differ — and ANY injective spelling satisfies both. Changing
    /// `"thing@{}/{}"` to `"thingX{}|{}"` left all four of them green while
    /// silently renumbering every thing in every saved world.
    ///
    /// The same shape as `passage::addr_key`'s frozen key and
    /// `knowledge::LOCALE_KEY_PREFIX`: the literal IS the contract, so the only
    /// legitimate way to change it is an epoch (a new prefix alongside the old,
    /// never a rename), and this assertion is what makes that a deliberate act
    /// rather than a green diff.
    ///
    /// `707` is not a magic number: `Facet { face: 3, path: [1, 2] }` packs to
    /// `(((1 << 2 | 1) << 2 | 2) << 5) | 3`, and the sentinel-prefixed path
    /// word is the kernel's `FacetId` contract, frozen independently.
    #[test]
    fn the_thing_role_spelling_is_the_permanent_lineage_key() {
        let f = facet(3, &[1, 2]);
        assert_eq!(
            f.pack().expect("a shallow facet packs").0,
            707,
            "the FacetId packing moved; the role literal below is stated in \
             terms of it"
        );
        assert_eq!(
            thing_role(&f, "strongbox").expect("a shallow facet packs"),
            "thing@707/strongbox",
            "thing_role's spelling is a SAVE-FORMAT CONTRACT: it is an input to \
             a derived EntityId, so changing it renumbers every thing in every \
             saved world. Do not rebaseline this literal — see the doc comment \
             on `thing_role`."
        );
    }

    /// `promote`'s `ordinal` argument must reach the id it returns.
    ///
    /// The module used to construct the `Lineage` twice — once in `thing_id`,
    /// once in `promote` — and only `thing_id`'s copy was pinned, so mutating
    /// `promote`'s `ordinal` to a literal `0` left the whole vessel suite
    /// green. The duplicate is gone (both route through `thing_lineage`), but
    /// that deletion alone cannot pin the ARGUMENT `promote` passes: the ledger
    /// takes a `Lineage`, not an `EntityId`, so `promote` must still hand the
    /// ordinal on, and only a test that promotes a NONZERO ordinal watches that
    /// hand-off. This is that test.
    #[test]
    fn promotes_ordinal_reaches_the_id_it_returns() {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        let mut ledger = Ledger::default();

        let f = facet(3, &[1, 2]);
        let day = WorldTime::from_std_days(5.0).expect("5 days is in range");

        let zeroth = promote(&mut ledger, &reg, &f, "strongbox", 0, day).unwrap();
        let first = promote(&mut ledger, &reg, &f, "strongbox", 1, day).unwrap();

        assert_ne!(
            zeroth, first,
            "two ordinals in one room promoted to one entity"
        );
        assert_eq!(
            first,
            thing_id(&f, "strongbox", 1).unwrap(),
            "promote and thing_id disagree about ordinal 1"
        );
    }

    /// The committed `instance-of` fact's FIELDS, not merely its count.
    ///
    /// `promoting_twice_yields_one_entity` counts facts, which is the wrong
    /// half: mutating the fact's `object` to `Value::Text("mutant")` or its
    /// `day` to `None` left that count at 1 and the suite green.
    ///
    /// `day` is the load-bearing one. A thing with no day is invisible to every
    /// `day <= now` fold downstream — the same time-correctness discipline
    /// `passage.rs` documents for a clearing fact — so a `None` here would not
    /// fail anything today and would silently hide every promoted thing from
    /// the reads that come next.
    #[test]
    fn the_committed_instance_of_fact_carries_the_kind_and_the_day() {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        let mut ledger = Ledger::default();

        let f = facet(3, &[1, 2]);
        let day = WorldTime::from_std_days(5.0).expect("5 days is in range");
        let id = promote(&mut ledger, &reg, &f, "strongbox", 0, day).unwrap();

        assert_eq!(
            ledger.kind_of(id),
            Some("strongbox"),
            "the instance-of object must be the thing's KIND"
        );
        let fact = ledger
            .find(INSTANCE_OF)
            .find(|fact| fact.subject == id)
            .expect("promotion committed an instance-of about the thing");
        assert_eq!(
            fact.day,
            Some(day),
            "a promoted thing with no day is invisible to every `day <= now` \
             fold that reads it"
        );
    }

    /// `promote`'s own error path, not just [`thing_id`]'s. The two reach
    /// [`Facet::pack`] by different routes, and [`ThingError`] exists precisely
    /// because this one has to widen `LedgerError` to carry the failure — an
    /// arm nothing exercised until now.
    #[test]
    fn a_promotion_into_a_room_that_does_not_pack_is_refused() {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        let mut ledger = Ledger::default();

        let too_deep = facet(3, &[1u8; 64]);
        let day = WorldTime::from_std_days(5.0).expect("5 days is in range");

        let err = promote(&mut ledger, &reg, &too_deep, "strongbox", 0, day)
            .expect_err("a room past MAX_DEPTH cannot carry a thing");
        assert!(
            matches!(err, ThingError::Facet(FacetError::DepthExceedsCap)),
            "expected the Facet arm, got {err:?}"
        );
        assert_eq!(
            ledger.entity_count(),
            0,
            "a refused promotion must mint nothing"
        );
        assert_eq!(
            ledger.find(INSTANCE_OF).count(),
            0,
            "a refused promotion must commit nothing"
        );
    }

    /// A registry carrying the predicates this module's tests commit: the
    /// kernel's `instance-of`, the two live-play predicates `Session::start`
    /// registers, and `AGENT_AT` (which no fold here reads — that is the
    /// point of `a_thing_held_by_a_body_has_no_room_today`).
    ///
    /// **The two docs come from the shared constants, not from literals.** A
    /// helper spelling them its own way is exactly the divergence Task 5
    /// shipped: `PredicateDef` compares `{name, functional, doc}`, so a
    /// one-word difference between this registry and `Session::start`'s is a
    /// `ConflictingDefinition` the moment one registry meets the other's
    /// facts — and neither side reds until then.
    fn play_registry() -> ConceptRegistry {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        reg.register_predicate(LOCATED_IN, false, LOCATED_IN_DOC)
            .unwrap();
        reg.register_predicate(OPENNESS, false, OPENNESS_DOC)
            .unwrap();
        reg.register_predicate(crate::liveness::AGENT_AT, false, "t")
            .unwrap();
        reg
    }

    fn eid(raw: u64) -> EntityId {
        EntityId::new(raw).expect("a nonzero raw id")
    }

    fn at(days: f64) -> WorldTime {
        WorldTime::from_std_days(days).expect("a small day count is in range")
    }

    /// A thing's location is the LATEST fact AT OR BEFORE the day asked
    /// about, not the latest fact outright. This is what distinguishes a fold
    /// from a mutable flag: a replayed past must not see a move that had not
    /// happened yet (decision 0366; the same discipline
    /// `passage::effective_state` inlines).
    ///
    /// MUTATION THIS FAILS AGAINST: the `if d > day { continue; }` guard in
    /// `latest_object_at_or_before`. Deleting it — the shape a "latest value"
    /// read would naturally have — reds this test and exactly one other,
    /// `openness_is_absent_until_a_fact_says_otherwise`, which asks about a
    /// day before its own first fact through the SAME shared fold. Every
    /// other test in this module stays green, because every one of them asks
    /// about a day at or after the last fact it committed. (The claim here
    /// used to be "leaves every other test in this module green", which the
    /// suite itself contradicts — over-coverage, but a stated fact about the
    /// suite has to be true or the next reader trusts the next one.) The
    /// third assertion below, a day BEFORE the first fact, pins the other end
    /// of the same guard.
    #[test]
    fn location_is_read_as_of_the_day_asked_about() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let key = eid(1);

        ledger
            .commit(
                located_fact(key, Value::Text("hall".to_string()), at(2.0)),
                &reg,
            )
            .unwrap();
        ledger
            .commit(
                located_fact(key, Value::Text("crypt".to_string()), at(6.0)),
                &reg,
            )
            .unwrap();

        assert_eq!(
            location_of(&ledger, key, at(4.0)),
            Some(Value::Text("hall".to_string())),
            "a read as of day 4 must not see the move that happened on day 6"
        );
        assert_eq!(
            location_of(&ledger, key, at(6.0)),
            Some(Value::Text("crypt".to_string())),
            "the filter is `<= day`, not `< day`: a move is visible on the \
             instant it happens"
        );
        assert_eq!(
            location_of(&ledger, key, at(1.0)),
            None,
            "before its first location fact, a thing has no location"
        );
    }

    /// Two location facts at ONE instant resolve by COMMIT ORDER — the last
    /// posting wins, the same rule `Ledger::latest_value_of` uses.
    ///
    /// MUTATION THIS FAILS AGAINST: `seen <= d` to `seen < d` in
    /// `latest_object_at_or_before`. That is a one-character change which
    /// keeps every dated-ordering property intact and silently inverts the
    /// tie-break, and no other test in this module commits two facts at one
    /// instant.
    #[test]
    fn the_last_posting_at_one_instant_wins() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let key = eid(1);
        let noon = at(3.0);

        ledger
            .commit(
                located_fact(key, Value::Text("hall".to_string()), noon),
                &reg,
            )
            .unwrap();
        ledger
            .commit(
                located_fact(key, Value::Text("crypt".to_string()), noon),
                &reg,
            )
            .unwrap();

        assert_eq!(
            location_of(&ledger, key, noon),
            Some(Value::Text("crypt".to_string())),
            "two facts at one instant resolve by commit order: the LAST \
             posting wins"
        );
    }

    /// The positive control for the transitive walk: a thing whose location
    /// is already a room needs no hop at all. Without this, the mutation
    /// `room_of` is pinned against below could be mistaken for a break in the
    /// whole read rather than in its containment step.
    #[test]
    fn a_thing_resting_in_a_room_is_in_that_room() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let chest = eid(2);

        ledger
            .commit(
                located_fact(chest, Value::Text("vault".to_string()), at(1.0)),
                &reg,
            )
            .unwrap();

        assert_eq!(room_of(&ledger, chest, at(5.0)), Some("vault".to_string()));
    }

    /// A key in a chest in a room is in the room. Transitivity is a property
    /// of the `in` relation (RCC-8, The Hearth §5), so it belongs to the
    /// resolver and not to each caller.
    ///
    /// MUTATION THIS FAILS AGAINST: `Value::Entity(holder) => current =
    /// holder` to `Value::Entity(_) => return None` in `room_of` — the
    /// one-hop reading of the same function. Every other assertion about
    /// `room_of` stays green under it, including the positive control above,
    /// because they place things directly in rooms.
    #[test]
    fn containment_resolves_transitively_to_a_room() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let key = eid(1);
        let chest = eid(2);

        ledger
            .commit(located_fact(key, Value::Entity(chest), at(1.0)), &reg)
            .unwrap();
        ledger
            .commit(
                located_fact(chest, Value::Text("vault".to_string()), at(1.0)),
                &reg,
            )
            .unwrap();

        assert_eq!(
            room_of(&ledger, key, at(5.0)),
            Some("vault".to_string()),
            "a key in a chest in a room is in the room"
        );
    }

    /// A containment cycle TERMINATES rather than hanging. Nothing in this
    /// campaign creates one, which is exactly why nothing else would catch
    /// it.
    ///
    /// MUTATION THIS FAILS AGAINST: `if !visited.insert(current) { return
    /// None; }` in `room_of`. Its red is a HANG, not an assertion failure —
    /// the walk revisits `a` forever — so it is observed as a timeout on this
    /// test alone rather than as a printed `assert` diff. That is the honest
    /// shape of the failure this guard prevents.
    #[test]
    fn a_containment_cycle_terminates() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let a = eid(1);
        let b = eid(2);

        ledger
            .commit(located_fact(a, Value::Entity(b), at(1.0)), &reg)
            .unwrap();
        ledger
            .commit(located_fact(b, Value::Entity(a), at(1.0)), &reg)
            .unwrap();

        assert_eq!(
            room_of(&ledger, a, at(5.0)),
            None,
            "a cycle has no room, and must be answered rather than walked \
             forever"
        );
    }

    /// A location that is neither a room key nor a holder is refused, not
    /// unwrapped. Nothing in the tree commits one today; the arm exists so
    /// that a future writer of a malformed fact gets `None` instead of a
    /// panic, and this is what pins it.
    #[test]
    fn a_malformed_location_is_not_a_room() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let key = eid(1);

        ledger
            .commit(located_fact(key, Value::Number(7.0), at(1.0)), &reg)
            .unwrap();

        assert_eq!(room_of(&ledger, key, at(5.0)), None);
    }

    /// Openness is ABSENT until a fact says otherwise, and absence means
    /// "whatever the seed drew" — never "shut". The same as-of-day fold
    /// `location_of` uses, which is why both route through one helper.
    #[test]
    fn openness_is_absent_until_a_fact_says_otherwise() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let chest = eid(2);

        assert_eq!(
            is_open(&ledger, chest, at(5.0)),
            None,
            "an untouched thing's openness belongs to the generator, not to \
             this fold"
        );

        ledger
            .commit(openness_fact(chest, true, at(2.0)), &reg)
            .unwrap();
        ledger
            .commit(openness_fact(chest, false, at(6.0)), &reg)
            .unwrap();

        assert_eq!(
            is_open(&ledger, chest, at(1.0)),
            None,
            "before the first openness fact there is still no answer here"
        );
        assert_eq!(
            is_open(&ledger, chest, at(4.0)),
            Some(true),
            "openness is read as of the day asked about, like every other fold"
        );
        assert_eq!(
            is_open(&ledger, chest, at(6.0)),
            Some(false),
            "openness is NOT monotone: unlike a thrown latch, a chest closes \
             again"
        );
    }

    /// The two facts' whole envelope, not merely the value a fold reads back.
    ///
    /// Task 4's review found the committed `instance-of` fact's `object` and
    /// `day` both unpinned — a `day: None` would have hidden every promoted
    /// thing from every `day <= now` fold with the suite green. `place` is
    /// the field with that shape here: nothing downstream reads it, so a
    /// mutation setting it would go unnoticed, and it must stay `None`
    /// because the location rides in `object` (see [`LOCATED_IN`]).
    #[test]
    fn the_location_and_openness_facts_carry_their_whole_envelope() {
        let key = eid(1);
        let chest = eid(2);

        let located = located_in_holder_fact(key, chest, at(3.0));
        assert_eq!(located.subject, key, "the THING is the subject");
        assert_eq!(located.predicate, LOCATED_IN);
        assert_eq!(
            located.object,
            Value::Entity(chest),
            "the LOCATION is the object"
        );
        assert_eq!(
            located.place, None,
            "`Fact::place` is where a fact was observed; the location rides \
             in `object`, and a holder could not fit in `place` at all"
        );
        assert_eq!(
            located.day,
            Some(at(3.0)),
            "an undated location is invisible to every `day <= now` fold"
        );

        let opened = openness_fact(chest, true, at(3.0));
        assert_eq!(opened.subject, chest);
        assert_eq!(opened.predicate, OPENNESS);
        assert_eq!(opened.object, Value::Flag(true));
        assert_eq!(opened.place, None);
        assert_eq!(opened.day, Some(at(3.0)));

        // BOTH flags, because the message below names a guarantee one of them
        // cannot give: a body of `Value::Flag(true)` satisfies the `true`
        // case, so asserting only that arm cannot see the constant it warns
        // against. (`openness_is_absent_until_a_fact_says_otherwise` does
        // catch it, through the fold — this is the envelope-level half.)
        let shut = openness_fact(chest, false, at(3.0));
        assert_eq!(
            shut.object,
            Value::Flag(false),
            "openness must carry the flag it was told, not a constant"
        );
    }

    /// A ROOM's on-disk key, written out as a literal — and the whole reason
    /// [`room_key`] exists.
    ///
    /// **This test cannot be rebaselined**, the same shape as
    /// `the_thing_role_spelling_is_the_permanent_lineage_key` above and
    /// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key` in
    /// `tests/suite/passage.rs`. Every other assertion about locations in
    /// this module uses a legible stand-in (`"hall"`, `"vault"`) because
    /// `room_of` returns whatever text it finds — so any encoding whatsoever
    /// keeps them green, while a `located-in` room key that stopped agreeing
    /// with `agent-at`'s would put two spellings of one room on disk, each
    /// self-consistent, each round-tripping through its own decoder.
    ///
    /// The second assertion is the one that pins the WIRING: an encoder
    /// nothing routes through is not an owner, and `located_in_room_fact`
    /// hand-formatting the same digits would satisfy the first assertion
    /// alone.
    ///
    /// `707` is the same packing the role-spelling test above states:
    /// `Facet { face: 3, path: [1, 2] }`.
    #[test]
    fn a_rooms_key_is_the_permanent_on_disk_spelling() {
        let f = facet(3, &[1, 2]);
        assert_eq!(
            room_key(&f).expect("a shallow facet packs"),
            "707",
            "room_key's spelling is a SAVE-FORMAT CONTRACT: it is written \
             into every located-in fact naming a room, and agent-at spells \
             the same rooms through this same function. Do not rebaseline \
             this literal — take an epoch."
        );
        assert_eq!(
            located_in_room_fact(eid(1), &f, at(3.0))
                .expect("a shallow facet packs")
                .object,
            Value::Text("707".to_string()),
            "the room fact must carry the key `room_key` produces, not a \
             spelling of its own"
        );
    }

    /// A room too deep to pack is refused by the fact constructor, not
    /// unwrapped — the reason [`room_key`] propagates [`FacetError`] instead
    /// of `.expect()`ing the way `liveness::room_to_text` does. `thing_id`
    /// refuses the same facet (`a_room_that_does_not_pack_is_refused`), and
    /// an encoder that panicked where identity politely refuses would be an
    /// inconsistency inside one module.
    #[test]
    fn a_room_that_does_not_pack_cannot_carry_a_thing_either() {
        let too_deep = facet(3, &[1u8; 64]);
        assert_eq!(room_key(&too_deep), Err(FacetError::DepthExceedsCap));
        assert!(matches!(
            located_in_room_fact(eid(1), &too_deep, at(3.0)),
            Err(FacetError::DepthExceedsCap)
        ));
    }

    /// **A thing held by a BODY has no room today, and this test exists to
    /// FAIL when that changes.**
    ///
    /// The module's docs used to claim `located-in` covered "in a room, in a
    /// container, in a hand" and that `room_of` resolved containment
    /// transitively — both true of what the object can REPRESENT, and the
    /// second false of what resolves. A chest is located by a `located-in`
    /// fact, which this walk reads; a body is positioned by `AGENT_AT`, which
    /// it does not. So a key in a hand walks one hop and finds nothing —
    /// `None`, the same answer a key nobody ever placed gets. The body below
    /// genuinely HAS a position (the `agent-at` fact is committed and names a
    /// room), so this is not the absence of data: it is the absence of a
    /// bridge between two predicates.
    ///
    /// **TASK 12 SHIPPED AND THIS TEST DID NOT MOVE, WHICH IS A RESULT AND
    /// NOT AN OVERSIGHT.** This doc used to read "TASK 12
    /// (`take`/`drop`/`put`/`carrying`) MUST CHANGE THIS", on the reasoning
    /// that a fallback could not be exercised until something committed a
    /// held thing. Something does now — `Session::take` — and the fallback
    /// was still not written, because implementing it surfaced a second
    /// obstacle the first one had been hiding: a body's `AGENT_AT` position
    /// is its WALK-BAND LOCALE and a thing set down indoors is keyed on the
    /// CHAMBER, so the fallback would answer in a different key space from
    /// every other answer this function gives, with nothing in the
    /// [`Value::Text`] to mark which. [`room_of`]'s own doc carries the
    /// measurement (`540999680` against `141819825979456`, one turn apart in
    /// one walk) and the consequence.
    ///
    /// So the tripwire is KEPT rather than satisfied or deleted, and it now
    /// guards a sharper thing: not "somebody forgot", but "somebody closed
    /// this without deciding what a body's room is while it is indoors". A
    /// deferred obligation written only as prose is one nobody meets;
    /// written as a red, it cannot be inherited silently — and the red is
    /// still armed.
    #[test]
    fn a_thing_held_by_a_body_has_no_room_today() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let key = eid(1);
        let body = eid(2);
        let room = facet(3, &[1, 2]);

        ledger
            .commit(
                crate::liveness::agent_at_fact(body, &room, at(1.0), "test placement"),
                &reg,
            )
            .unwrap();
        ledger
            .commit(located_in_holder_fact(key, body, at(1.0)), &reg)
            .unwrap();

        assert_eq!(
            room_of(&ledger, key, at(5.0)),
            None,
            "a thing in a hand does not resolve to a room today. If you just \
             implemented the AGENT_AT fallback, this red is the tripwire \
             working — and before you rebaseline it, read `room_of`'s doc: a \
             body's agent-at position is its WALK-BAND LOCALE, while a thing \
             set down indoors is keyed on the CHAMBER, so a naive fallback \
             answers in a key space no caller can tell apart from the other \
             one. Update this test, `LOCATED_IN`'s doc and `room_of`'s doc \
             together, and say what a body's room is while it is indoors."
        );
        assert_eq!(
            location_of(&ledger, key, at(5.0)),
            Some(Value::Entity(body)),
            "the holding IS represented — only the walk to the body's own \
             room is missing, and this assertion is what keeps the red above \
             from being read as a missing fact"
        );
    }

    /// The two predicates' exact on-disk spellings, written out as literals.
    ///
    /// **This test cannot be rebaselined, which is the entire point of
    /// writing it out** — the same reasoning as
    /// `the_thing_role_spelling_is_the_permanent_lineage_key` above and
    /// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key` in
    /// `tests/suite/passage.rs`. Every other assertion in this module reaches
    /// the predicate through the CONSTANT on both the write and the read
    /// side, so any spelling whatsoever keeps them all green — while a
    /// rename leaves every fact in an already-saved world (`possess --out`
    /// carries them, decision 0368) present in the file and invisible to
    /// every fold that reads it. A thing would silently lose its location and
    /// a chest silently forget it was opened.
    ///
    /// A predicate name is therefore load-bearing on the same terms as
    /// `addr_key`'s format string, and the only legitimate change is an epoch
    /// (`located-in/v2`), never an edit.
    #[test]
    fn the_location_predicate_spellings_are_permanent_on_disk_keys() {
        assert_eq!(
            LOCATED_IN, "located-in",
            "LOCATED_IN's spelling is a SAVE-FORMAT CONTRACT: it is written \
             into every committed fact and into a saved world's registry, so \
             a rename makes every existing located-in fact unreadable without \
             failing anything. Do not rebaseline this literal — take an epoch."
        );
        assert_eq!(
            OPENNESS, "openness",
            "OPENNESS's spelling is a SAVE-FORMAT CONTRACT on the same terms \
             as LOCATED_IN's."
        );
    }

    // ---- latency: the negative fold (Task 6) ----------------------------
    //
    // Every test below places things through `located_in_room_fact` /
    // `located_in_holder_fact`, never by hand-formatting a room key: the
    // encoder is the one `is_latent` compares against, and a test that spelled
    // its own would be asserting agreement with itself.

    /// A thing carried away is NOT offered again where it came from — the
    /// negative fold, across two entries into the same room.
    ///
    /// The first assertion is the positive control and it is load-bearing:
    /// without it, a body of `Ok(false)` would satisfy the second assertion
    /// alone, and "the location check is consulted at all" would be unpinned
    /// in the direction that matters.
    ///
    /// TWO MUTATIONS, ONE PER ASSERTION, because no single one reaches both
    /// halves of a positive control plus its negative case.
    ///
    /// 1. `Some(Value::Text(room)) => room == here` to `... room != here` in
    ///    `is_latent` — the SENSE of the one comparison the whole negative
    ///    fold is made of. It reds the second assertion (5 tests red in all,
    ///    the sense being load-bearing nearly everywhere):
    ///
    /// ```text
    /// thread 'thing::tests::a_thing_carried_away_is_not_re_offered_where_it_came_from'
    ///   panicked at windows/vessel/src/thing.rs:1326:9:
    /// a strongbox committed into another room must stop being offered in the
    /// room it came from
    /// ```
    ///
    /// 2. `None => true` to `None => false` — the whole read collapsed to
    ///    "nothing is ever offered", which is what a body of `Ok(false)`
    ///    would amount to. It reds the FIRST assertion (6 tests red in all):
    ///
    /// ```text
    /// thread 'thing::tests::a_thing_carried_away_is_not_re_offered_where_it_came_from'
    ///   panicked at windows/vessel/src/thing.rs:1313:9:
    /// a room with no facts about its strongbox must still offer it — this is
    /// the state every room starts in
    /// ```
    #[test]
    fn a_thing_carried_away_is_not_re_offered_where_it_came_from() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let elsewhere = facet(3, &[1, 3]);
        let strongbox = thing_id(&here, "strongbox", 0).unwrap();

        assert!(
            is_latent(&ledger, &here, "strongbox", 0, at(5.0)).unwrap(),
            "a room with no facts about its strongbox must still offer it — \
             this is the state every room starts in"
        );

        ledger
            .commit(
                located_in_room_fact(strongbox, &elsewhere, at(3.0)).unwrap(),
                &reg,
            )
            .unwrap();

        assert!(
            !is_latent(&ledger, &here, "strongbox", 0, at(5.0)).unwrap(),
            "a strongbox committed into another room must stop being offered \
             in the room it came from"
        );
    }

    /// A thing promoted and PUT BACK is still offered here.
    ///
    /// This is the case an implementation keyed on "has any `located-in`
    /// fact" gets wrong, and it is why this test stands beside
    /// `a_thing_carried_away_is_not_re_offered_where_it_came_from` rather
    /// than instead of it: a suite that only ever carries things away cannot
    /// distinguish the two readings.
    ///
    /// The middle assertion — not latent between the taking and the return —
    /// is what keeps the last one from being read as "the fold never fires".
    ///
    /// MUTATION THIS FAILS AGAINST: `Some(Value::Text(room)) => room == here`
    /// to `Some(Value::Text(_)) => false`, i.e. exactly the "any location
    /// fact means gone" reading. It reds two tests in the whole vessel crate
    /// — this one's last assertion and its behavioural twin
    /// `suite::thing::a_room_offers_again_what_was_brought_back` — and leaves
    /// the carried-away test above green, which is the discrimination this
    /// test exists for:
    ///
    /// ```text
    /// thread 'thing::tests::a_thing_put_back_is_still_here' panicked at
    ///   windows/vessel/src/thing.rs:1384:9:
    /// a thing put back in the room it came from is HERE — an implementation
    /// keyed on "has any located-in fact" reads this as gone
    /// ```
    #[test]
    fn a_thing_put_back_is_still_here() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let elsewhere = facet(3, &[1, 3]);
        let strongbox = thing_id(&here, "strongbox", 0).unwrap();

        ledger
            .commit(
                located_in_room_fact(strongbox, &elsewhere, at(3.0)).unwrap(),
                &reg,
            )
            .unwrap();
        assert!(
            !is_latent(&ledger, &here, "strongbox", 0, at(3.5)).unwrap(),
            "while it is away it is away"
        );

        ledger
            .commit(
                located_in_room_fact(strongbox, &here, at(4.0)).unwrap(),
                &reg,
            )
            .unwrap();

        assert!(
            is_latent(&ledger, &here, "strongbox", 0, at(5.0)).unwrap(),
            "a thing put back in the room it came from is HERE — an \
             implementation keyed on \"has any located-in fact\" reads this \
             as gone"
        );
    }

    /// A thing PROMOTED and never moved is still here.
    ///
    /// Promotion writes an `instance-of` fact and no location at all (spec
    /// §3.4's phase 4), so this is the second way a thing acquires facts
    /// without leaving: opening a chest must not make the chest disappear
    /// from the room it stands in.
    ///
    /// MUTATION THIS FAILS AGAINST: `None => true` to `None => false` in
    /// `is_latent` (6 tests red in all):
    ///
    /// ```text
    /// thread 'thing::tests::a_promoted_thing_that_never_moved_is_still_here'
    ///   panicked at windows/vessel/src/thing.rs:1429:9:
    /// promotion is not departure: a chest opened in place is still in the room
    /// ```
    ///
    /// **NO MUTATION REDS THIS TEST ALONE, and that is worth stating rather
    /// than papering over with a mutation chosen for its blast radius.** The
    /// promoted-but-unmoved thing reaches `is_latent`'s `None` arm, the same
    /// arm the carried-away test's positive control reaches, so every
    /// mutation that kills one kills both. (`LOCATED_IN` to `INSTANCE_OF`
    /// inside `location_of` also reds it — along with twelve other tests,
    /// because it is a mutation of the SHARED fold and says nothing about
    /// this function.) What this test holds that no other does is a claim
    /// spanning two functions: that [`promote`] writes no location at all,
    /// so promotion cannot take a thing out of its own room. **Task 12 made
    /// that falsifiable and it held**: `Session::take` promotes AND places,
    /// in that order, in one turn — and it places the thing on the BODY, not
    /// in the room, so `promote`'s own silence about location is what keeps
    /// this assertion true rather than an accident of nothing having tried.
    /// A `promote` that wrote a room posting of its own would red here.
    #[test]
    fn a_promoted_thing_that_never_moved_is_still_here() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);

        promote(&mut ledger, &reg, &here, "strongbox", 0, at(3.0)).unwrap();

        assert!(
            is_latent(&ledger, &here, "strongbox", 0, at(5.0)).unwrap(),
            "promotion is not departure: a chest opened in place is still in \
             the room"
        );
    }

    /// Latency is read AS OF the day asked about, like every other fold here:
    /// at an instant before the carrying fact, the thing is still offered.
    ///
    /// This pins that `is_latent`'s own `day` argument reaches the fold —
    /// which `location_is_read_as_of_the_day_asked_about` cannot, because it
    /// calls `location_of` directly and never passes through this function's
    /// argument list.
    ///
    /// MUTATION THIS FAILS AGAINST: `location_of(ledger, thing, day)` to
    /// `location_of(ledger, thing, WorldTime::from_ticks(i64::MAX))` — "where
    /// is it NOW", the read a caller not thinking about replay would write,
    /// and the one that severs this function's `day` argument. It reds
    /// exactly two tests in the vessel crate, this one's FIRST assertion and
    /// the corresponding as-of-day assertion in
    /// `suite::thing::a_room_offers_again_what_was_brought_back`; every other
    /// latency test asks about a day at or after the last fact it committed,
    /// so a read from the far future agrees with them:
    ///
    /// ```text
    /// thread 'thing::tests::latency_is_time_correct' panicked at
    ///   windows/vessel/src/thing.rs:1479:9:
    /// a replayed day 4 must not see a move that happens on day 6
    /// ```
    ///
    /// Substituting `from_ticks(0)` instead — the other end of the same
    /// severing — reds seven, because it makes everything latent forever. It
    /// is the less useful of the two for exactly that reason: a mutation that
    /// reds half the file locates nothing.
    #[test]
    fn latency_is_time_correct() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let elsewhere = facet(3, &[1, 3]);
        let strongbox = thing_id(&here, "strongbox", 0).unwrap();

        ledger
            .commit(
                located_in_room_fact(strongbox, &elsewhere, at(6.0)).unwrap(),
                &reg,
            )
            .unwrap();

        assert!(
            is_latent(&ledger, &here, "strongbox", 0, at(4.0)).unwrap(),
            "a replayed day 4 must not see a move that happens on day 6"
        );
        assert!(
            !is_latent(&ledger, &here, "strongbox", 0, at(6.0)).unwrap(),
            "on the day of the move and after it, the room must stop \
             offering it"
        );
    }

    /// Latency is keyed on the WHOLE lineage — the room, the kind and the
    /// ordinal — because that is what `thing_id` derives from. Carrying one
    /// room's strongbox away must not empty the slot beside it, nor the same
    /// slot in the next room.
    ///
    /// The last assertion is the one worth stating twice: `elsewhere`'s own
    /// strongbox is a DIFFERENT entity from the one now standing in
    /// `elsewhere`, so a room does not stop offering its strongbox because
    /// someone carried another room's strongbox in.
    ///
    /// MUTATION THIS FAILS AGAINST: `thing_id(facet, kind, ordinal)` to
    /// `thing_id(facet, kind, 0)`, which drops the ordinal leg — the same
    /// class of severed argument `promotes_ordinal_reaches_the_id_it_returns`
    /// pins for `promote`. It reds this test and NOTHING ELSE in the vessel
    /// crate, because no other latency test uses a nonzero ordinal:
    ///
    /// ```text
    /// thread 'thing::tests::latency_is_keyed_on_the_whole_lineage' panicked
    ///   at windows/vessel/src/thing.rs:1530:9:
    /// the SECOND strongbox of this room is a different thing and is still here
    /// ```
    #[test]
    fn latency_is_keyed_on_the_whole_lineage() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let elsewhere = facet(3, &[1, 3]);
        let strongbox = thing_id(&here, "strongbox", 0).unwrap();

        ledger
            .commit(
                located_in_room_fact(strongbox, &elsewhere, at(3.0)).unwrap(),
                &reg,
            )
            .unwrap();

        assert!(
            is_latent(&ledger, &here, "key", 0, at(5.0)).unwrap(),
            "carrying the strongbox off says nothing about the key"
        );
        assert!(
            is_latent(&ledger, &here, "strongbox", 1, at(5.0)).unwrap(),
            "the SECOND strongbox of this room is a different thing and is \
             still here"
        );
        assert!(
            is_latent(&ledger, &elsewhere, "strongbox", 0, at(5.0)).unwrap(),
            "the room the strongbox was carried INTO still offers its own \
             latent strongbox: two rooms' strongboxes are two entities"
        );
    }

    /// A thing put INSIDE a container standing in this room is no longer
    /// loose here — the reason `is_latent` compares [`location_of`]'s answer
    /// and never [`room_of`]'s.
    ///
    /// The first assertion is the trap stated outright: `room_of` resolves
    /// the key transitively to this very room, so an implementation that
    /// asked "is it in this room" instead of "is it AT this room" would call
    /// the key latent and the room would offer it loose on the floor while
    /// it sits in the chest — one key rendered twice.
    ///
    /// MUTATION THIS FAILS AGAINST: `location_of(ledger, thing, day)` to
    /// `room_of(ledger, thing, day).map(Value::Text)`, the transitive
    /// reading. It reds two tests in the vessel crate: this one, and
    /// `a_malformed_location_still_ends_latency` — which is the same arm seen
    /// from its other input, since `room_of` answers `None` for a malformed
    /// location too. Every latency test that places things DIRECTLY in rooms
    /// stays green, which is why neither of those two could be dropped in
    /// favour of the other:
    ///
    /// ```text
    /// thread 'thing::tests::a_thing_inside_a_container_here_is_not_loose_here'
    ///   panicked at windows/vessel/src/thing.rs:1588:9:
    /// a key in the chest is not a key on the floor — offering it again would
    /// render one key twice
    /// ```
    #[test]
    fn a_thing_inside_a_container_here_is_not_loose_here() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let key = thing_id(&here, "key", 0).unwrap();
        let chest = thing_id(&here, "strongbox", 0).unwrap();

        ledger
            .commit(located_in_room_fact(chest, &here, at(1.0)).unwrap(), &reg)
            .unwrap();
        ledger
            .commit(located_in_holder_fact(key, chest, at(1.0)), &reg)
            .unwrap();

        assert_eq!(
            room_of(&ledger, key, at(5.0)),
            Some(room_key(&here).unwrap()),
            "the transitive read DOES place the key in this room — which is \
             exactly why latency must not be asked through it"
        );
        assert!(
            !is_latent(&ledger, &here, "key", 0, at(5.0)).unwrap(),
            "a key in the chest is not a key on the floor — offering it \
             again would render one key twice"
        );
    }

    /// A malformed location ends latency rather than being ignored. Nothing
    /// in the tree commits one today; the arm exists so that a fact placing a
    /// thing somewhere unreadable is still read as "it was moved", never as
    /// "no location at all" — the latter would re-offer the thing and mint a
    /// second copy of it.
    ///
    /// MUTATION THIS FAILS AGAINST: `Some(_) => false` to `Some(_) => true`,
    /// which reds exactly this test and the container test above — the arm's
    /// two inputs. Keeping both is what makes the pair informative: the day
    /// `Value::Entity` is given its own branch, this
    /// one still holds the malformed case alone:
    ///
    /// ```text
    /// thread 'thing::tests::a_malformed_location_still_ends_latency' panicked
    ///   at windows/vessel/src/thing.rs:1624:9:
    /// an unreadable location is still a location: re-offering the thing
    /// would mint a second copy of it
    /// ```
    #[test]
    fn a_malformed_location_still_ends_latency() {
        let reg = play_registry();
        let mut ledger = Ledger::default();
        let here = facet(3, &[1, 2]);
        let strongbox = thing_id(&here, "strongbox", 0).unwrap();

        ledger
            .commit(located_fact(strongbox, Value::Number(7.0), at(1.0)), &reg)
            .unwrap();

        assert!(
            !is_latent(&ledger, &here, "strongbox", 0, at(5.0)).unwrap(),
            "an unreadable location is still a location: re-offering the \
             thing would mint a second copy of it"
        );
    }

    /// A room too deep to pack is REFUSED, not unwrapped — the same contract
    /// `thing_id` and `room_key` already carry, and the reason `is_latent`
    /// returns a `Result` rather than a bare `bool`. Both of its `pack` calls
    /// are fallible and either would do; the point is that neither panics.
    #[test]
    fn latency_in_a_room_that_does_not_pack_is_refused() {
        let ledger = Ledger::default();
        let too_deep = facet(3, &[1u8; 64]);

        assert_eq!(
            is_latent(&ledger, &too_deep, "strongbox", 0, at(5.0)),
            Err(FacetError::DepthExceedsCap)
        );
    }
}
