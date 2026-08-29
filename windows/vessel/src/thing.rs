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
/// function rather than two struct literals. Both [`thing_id`] (which derives)
/// and [`promote`] (which mints under the same derivation) route through it, so
/// the id a caller *predicts* for an unpromoted thing and the id the ledger
/// *mints* for a promoted one cannot disagree — there is nothing left for them
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
    let role = thing_role(facet, kind)?;
    Ok(hornvale_kernel::derive_entity_id(thing_lineage(
        &role, ordinal,
    )))
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
    let role = thing_role(facet, kind)?;
    let id = ledger.reuse_or_mint_entity(thing_lineage(&role, ordinal));
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

/// The predicate naming WHERE a thing is — **one** predicate covering all
/// three location types (spec §3.3): in a room, in a container, in a hand.
///
/// **The location rides in the fact's `object`, never in [`Fact::place`].**
/// `Fact::place` is an `Option<EntityId>` ("the entity where this fact was
/// observed") and could not hold a room key at all; `object` is a [`Value`],
/// so a room rides as [`Value::Text`] and a holder — a chest, a hand — rides
/// as [`Value::Entity`]. Two shapes of ONE field is exactly what makes
/// transitivity ("a key in a chest in a room is in the room") expressible in
/// [`room_of`] rather than re-asserted at every call site. This follows
/// `agent_at_fact` and `passage::cleared_fact`, which both put the place in
/// the object.
///
/// Non-functional and append-only: a thing moves, and each move is one dated
/// fact. The read is [`location_of`] — as of a day — never
/// [`hornvale_kernel::Ledger::latest_value_of`], which answers "where is it
/// now" and would let a replayed past see a move that had not happened yet.
///
/// Registered PER-SESSION (never at genesis), beside `AGENT_AT` and
/// `PASSAGE_CLEARED` in `Session::start`. An out-of-session reader registers
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

/// The predicate recording that a thing was opened or closed.
///
/// **ABSENCE MEANS "whatever the seed drew"** (spec §3.3), which is why
/// [`is_open`] returns `Option<bool>` and not `bool`: a thing nobody has
/// touched has no openness fact, and the answer belongs to the generator, not
/// to this fold. Collapsing the `None` into `false` here would state that
/// every untouched door in the world is shut.
///
/// Non-functional and append-only — a chest may be opened, closed and opened
/// again — so, unlike The Latch's monotone `PASSAGE_CLEARED`, the latest
/// posting at or before the day is the answer.
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

/// The fact committed when `thing` comes to rest at `location` on `day`.
///
/// The thing is the SUBJECT and the location the OBJECT — a
/// [`Value::Text`] room key or a [`Value::Entity`] holder. Nothing validates
/// which of the two a caller passes, because both are legal: that choice is
/// the campaign's whole "three location types, one predicate" claim.
pub fn located_fact(thing: EntityId, location: Value, day: WorldTime) -> Fact {
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
/// **A cycle terminates.** `visited` is a [`std::collections::BTreeSet`] (no
/// `HashSet` — decision 0004's determinism ban), and a holder already seen
/// ends the walk with `None`. Nothing in this campaign creates a containment
/// cycle, which is exactly why nothing else would catch one.
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

    /// A registry carrying the three predicates this module commits: the
    /// kernel's `instance-of` plus the two live-play predicates
    /// `Session::start` registers.
    fn play_registry() -> ConceptRegistry {
        let mut reg = ConceptRegistry::default();
        reg.register_predicate(INSTANCE_OF, false, "t").unwrap();
        reg.register_predicate(LOCATED_IN, false, "where a thing is on a day")
            .unwrap();
        reg.register_predicate(OPENNESS, false, "whether a thing is open")
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
    /// read would naturally have — leaves every other test in this module
    /// green, because every one of them asks about a day at or after the last
    /// fact. The third assertion below (a day BEFORE the first fact) pins the
    /// other end of the same guard.
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

        let located = located_fact(key, Value::Entity(chest), at(3.0));
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
        assert_eq!(
            opened.object,
            Value::Flag(true),
            "openness must carry the flag it was told, not a constant"
        );
        assert_eq!(opened.place, None);
        assert_eq!(opened.day, Some(at(3.0)));
    }

    /// The two predicates' exact on-disk spellings, written out as literals.
    ///
    /// **This test cannot be rebaselined, which is the entire point of
    /// writing it out** — the same reasoning as
    /// `the_thing_role_spelling_is_the_permanent_lineage_key` above and
    /// `addr_key_spelling_is_the_permanent_on_disk_key` in
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
}
