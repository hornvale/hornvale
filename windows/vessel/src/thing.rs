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
}
