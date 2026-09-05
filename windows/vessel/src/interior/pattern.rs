//! The pattern inventory — authored primitives, DERIVED selection, and a
//! validator. The architecture is `domains/language/src/phonology.rs`
//! transposed: one authored inventory shared by the world, a per-culture draw
//! from it conditioned on what that culture already is, and an admissibility
//! predicate that rejects ill-formed results.
//!
//! THE UNIT OF AUTHORSHIP IS A PATTERN, NOT A ROOM. A pattern is a relational
//! fragment; a room is a composition of them. Authoring whole rooms would make
//! this a catalogue of solutions rather than a generative language, which is
//! the failure mode that killed software's borrowing of Alexander.
//!
//! THE COMPOSITION RULES ARE THE SUBSTANCE, not the inventory's size. Two rules
//! carry the weight here: a pattern declares WHERE it attaches, and a pattern
//! may declare another it COMPLETES and without which it is inadmissible.
//! Together they give a room depth — a hub composition, where everything hangs
//! off the centre, is the degenerate case and is what the anti-hub test forbids.

use super::anchor::{AnchorId, Interior};
use crate::housemark::{AuthorityMark, Housemark, ThresholdPosture};
use hornvale_kernel::KindId;
use hornvale_thing::kinds;

/// The cultural condition under which a chamber pattern is admitted.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HousemarkGate {
    /// Every housemark admits this pattern.
    Universal,
    /// Only a matching authority reading admits this pattern.
    Authority(AuthorityMark),
    /// Only a matching threshold posture admits this pattern.
    Threshold(ThresholdPosture),
}

impl HousemarkGate {
    fn admits(self, housemark: Option<Housemark>) -> bool {
        match self {
            Self::Universal => true,
            Self::Authority(authority) => {
                matches!(housemark, Some(mark) if mark.authority == authority)
            }
            Self::Threshold(threshold) => {
                matches!(housemark, Some(mark) if mark.threshold == threshold)
            }
        }
    }
}

/// Where a pattern's anchor attaches to what is already composed.
pub enum Attach {
    /// To the room's [`kinds::GROUND`] — the open middle.
    Hub,
    /// Adjacent to the first anchor of this kind (`Ec`).
    Beside(KindId),
    /// Strictly inside the first anchor of this kind (`Ntpp`).
    Within(KindId),
}

pub use crate::structure::role::{EVERY_ROLE, Role};

/// The roles a store's vocabulary is shared with: every role that keeps things,
/// which is every role except the two that furnish the front of a dwelling.
const STORING_ROLES: &[Role] = &[
    Role::Store,
    Role::Hall,
    Role::Loomroom,
    Role::Smithy,
    Role::Shrine,
];

/// One authored pattern: a named relational fragment contributing one anchor,
/// where it attaches, and what it completes.
/// type-audit: bare-ok(identifier-text: name), bare-ok(flag: needs_cold), bare-ok(flag: built), bare-ok(flag: at_locale), bare-ok(flag: needs_populous)
pub struct Pattern {
    /// The pattern's name — the selection key (never its index; see
    /// [`selection`]).
    pub name: &'static str,
    /// The anchor this pattern contributes.
    pub kind: KindId,
    /// Where that anchor attaches.
    pub attach: Attach,
    /// A kind that must ALREADY be present for this pattern to be admissible —
    /// Alexander's "patterns complete other patterns", made checkable.
    pub requires: Option<KindId>,
    /// Whether this pattern is drawn only where warmth matters.
    pub needs_cold: bool,
    /// Whether this pattern belongs to BUILT rooms (false = wilderness).
    pub built: bool,
    /// Which chamber roles draw this pattern. [`EVERY_ROLE`] for a pattern no
    /// role withholds — including the two the GRAMMAR confines anyway
    /// (`the-fire`, `the-fireside-bed`), which is the point.
    pub roles: &'static [Role],
    /// Whether the LOCALE band draws this pattern — the committed-derivation
    /// gate, and the one field in this struct that exists for a determinism
    /// reason rather than a world reason.
    ///
    /// [`selection`] admits only patterns with `at_locale: true`. A creature
    /// stands at a LOCALE, and its thermal drive reads the warmth of the
    /// interior composed there, which is committed history. A pattern with
    /// `at_locale: false` stays outside that derivation: chamber composition
    /// consumes no stream, writes no [`hornvale_kernel::Fact`], and decision
    /// 0069 keeps `Interior` unserialized. Setting one to `true` changes the
    /// locale derivation and requires an epoch classification;
    /// `a_locale_composition_is_untouched_by_the_role_layer` is the check that
    /// makes that deliberate rather than accidental.
    ///
    /// This flag does not promise world-file byte identity. If an appended
    /// pattern names a newly registered kind, that kind may extend the
    /// serialized [`hornvale_kernel::ConceptRegistry`] independently of room
    /// composition; the accession and its epoch cost must be classified and
    /// approved. An `at_locale: false` pattern can also move a rendered
    /// artifact: `the-brazier` renders in
    /// `book/src/gallery/possession-carry-seed-14.md`. Rule 3 below states both
    /// boundaries in full; whoever edits either contract must keep them in
    /// step.
    pub at_locale: bool,
    /// Whether this pattern is drawn only where the place held more people than
    /// a hamlet ([`crate::brief::Brief::is_populous`]). The `needs_cold` of
    /// social scale.
    ///
    /// **NOTHING IN [`INVENTORY`] SETS THIS TODAY (decision 0398).** The
    /// Blocking gave it to `the-strongbox` ("a hamlet has nothing worth locking
    /// up") and The Chattel mirrored it onto `the-key-in-the-strongbox`. That
    /// was a defensible claim about social scale and a false one about
    /// reachability: measured across three worlds and a 48-seed sweep, not one
    /// living occupation clears
    /// `hornvale_history::flesh::HAMLET_POPULATION_CEILING`, so the gate did not
    /// make the strongbox rare — it made it impossible, and a capability nothing
    /// can reach is not a capability. Both patterns were relaxed to `false`.
    ///
    /// The FIELD stays, and so does the filter arm in [`draw`], because the
    /// grammar's ability to gate on social scale is real and the next pattern
    /// may want it. What keeps that arm honest while no authored pattern
    /// exercises it is [`draw_from`], the seam a synthetic inventory is fed
    /// through in `the_populous_gate_still_works_though_no_authored_pattern_
    /// uses_it`. Setting this to `true` on a real pattern is therefore a
    /// deliberate act with a working filter under it, not an untested one.
    pub needs_populous: bool,
    /// The housemark condition for a CHAMBER to admit this pattern. Locale
    /// selection deliberately ignores it: the walk band has no housemark input.
    pub housemark_gate: HousemarkGate,
}

/// The authored inventory.
///
/// **What costs an epoch, stated exactly.** This comment used to say flatly that
/// adding or reordering a pattern *is* an epoch. That was true while every
/// pattern reached every band, and The Blocking made it over-strict — an
/// over-strict warning is one that gets ignored, which is how an *undeclared*
/// epoch ships. The true condition has three parts:
///
/// 1. **Reordering or inserting is ALWAYS an epoch.** [`draw`] admits a pattern
///    only once its `requires` kind is present, so the order IS the grammar's
///    dependency order. Moving a pattern before its requirement silently drops
///    it; moving one after a pattern that requires it silently drops that one.
/// 2. **Appending a pattern with `at_locale: true` is an epoch.** A locale
///    composition feeds [`crate::interior::warmth_at`], which feeds a creature's
///    thermal drive, which is committed history.
/// 3. **Appending a pattern with `at_locale: false` is LATENT to seeded
///    derivation — and "commits" here means one specific thing, stated
///    precisely because the word used to do double duty.** No live read
///    reaches its chamber composition through the LEDGER:
///    [`selection`] filters it out, and the only other consumer is
///    [`selection_for`], whose output feeds chamber composition without
///    consuming a stream or writing a [`hornvale_kernel::Fact`]. A chamber's
///    composed content is never serialized into a `World`'s ledger (decision
///    0069 keeps `Interior` derived per room, bubble-scoped, and discarded with
///    the bubble), so no ledger fact or seeded derivation changes. That half is
///    permanent and does not decay.
///
///    It is not a promise that no world file moves. If the pattern references
///    a newly registered kind, registering that kind may extend the serialized
///    [`hornvale_kernel::ConceptRegistry`]. That accession is independent of
///    chamber composition and requires its own epoch classification and
///    approval, as The Housemark's `BENCH` accession did at epoch 20.
///
///    **The other half of "commits" already decayed by the time this
///    sentence first named a date, and a first correction got the date right
///    and the mechanism wrong.** The sentence used to read "read by the
///    chamber renderer and by nothing that commits", meaning nothing
///    GIT-committed either — and a first fix (The Wicket's Task 5) dated
///    that claim's death to **2026-08-30**, attributing it to `b8fc0cd02`/
///    `26ebaf7e4` "landing `the-loom` and `the-key-by-the-loom`". Checked
///    and wrong: `the-loom` landed **2026-07-28** (`f2cfb0974`, The
///    Blocking) — a month earlier. `b8fc0cd02` did not land a pattern; it
///    CREATED `book/src/gallery/possession-carry-seed-1.md`, and that
///    transcript's very first version already rendered `a loom`, `a
///    strongbox` and `a key` — three `at_locale: false` anchors that had
///    existed since July.
///
///    **So the reviewable act is not "a pattern is appended" — that is the
///    very act this clause declares LATENT, and saying it opened the gate
///    contradicts the clause it sits in.** The act that opens the gate is *a
///    new committed artifact walking a chamber deep enough to render one*:
///    the day `book/src/gallery/possession-carry-seed-1.md` was created and
///    scripted to delve past the threshold, every `at_locale: false` pattern
///    already in `INVENTORY` became visible in a committed file at once,
///    `the-loom` included, with no edit to `INVENTORY` itself. A successor
///    reviewing "did this change open the gate" should watch for a new (or
///    newly deepened) committed transcript/fixture, not for an append here.
///
///    So: appending an `at_locale: false` pattern is LATENT with respect to
///    the LEDGER and seeded derivation — no Fact or stream changes through
///    chamber composition — but a newly referenced registered kind is a
///    separate accession question, and a RENDERED artifact like a gallery
///    transcript may show anything the world now contains and is expected to
///    move. A census column or `book/src/domesday/` ledger field moving would
///    still be the signal that chamber composition crossed its promised
///    boundary; serialized `ConceptRegistry` growth is classified separately.
///
///    **This clause has a twin, and keeping them in step is the point.**
///    [`Pattern::at_locale`]'s own field doc states the same latency, and
///    until The Wicket's close it stated it in the retired over-broad form
///    (*"cannot move a world: no live read can reach it"*) — thirty lines
///    above a rule that had already been corrected twice. Whoever edits
///    either one edits both, or the pair drifts again exactly as it did.
///
/// Sized near its intended scale deliberately, all the same: growth is cheap
/// today and will not stay cheap.
pub const INVENTORY: [Pattern; 20] = [
    // --- built, drawn at BOTH bands ---
    Pattern {
        name: "the-ground",
        kind: kinds::GROUND,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-threshold",
        kind: kinds::THRESHOLD,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        // Every chamber has at least one link, so a doorway cannot be the
        // threshold role's private property (spec §4.1).
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-alcove",
        kind: kinds::ALCOVE,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: true,
        // THE ONE ROLE GATE THE WHOLE GRAMMAR HANGS OFF. See `the-fire`.
        roles: &[Role::Hearthroom],
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-fire",
        kind: kinds::HEARTH,
        attach: Attach::Within(kinds::ALCOVE),
        requires: Some(kinds::ALCOVE),
        needs_cold: true,
        built: true,
        // NO ROLE WITHHOLDS THE FIRE, and it still burns in exactly one room.
        // It requires an alcove and only `Hearthroom` admits one, so the fire is
        // confined to the hearthroom by the GRAMMAR rather than by a rule anyone
        // wrote. And since a wall is a cell (Task 4b), a fire within an alcove is
        // a recess in a wall with a fire in it: a FIREPLACE.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-fireside-bed",
        kind: kinds::BED,
        attach: Attach::Beside(kinds::HEARTH),
        requires: Some(kinds::HEARTH),
        needs_cold: true,
        built: true,
        // Confined the same way, one link further along the chain: a bed by the
        // fire needs a fire, which needs an alcove, which only one role admits.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-water-jar",
        kind: kinds::VESSEL,
        attach: Attach::Beside(kinds::GROUND),
        requires: None,
        needs_cold: false,
        built: true,
        roles: STORING_ROLES,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-screen",
        kind: kinds::SCREEN,
        attach: Attach::Beside(kinds::THRESHOLD),
        requires: Some(kinds::THRESHOLD),
        needs_cold: false,
        built: true,
        // A screen affords nothing and shapes sightlines, which is a thing worth
        // doing beside exactly one doorway: the one strangers come through.
        roles: &[Role::Threshold],
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Threshold(ThresholdPosture::Inward),
    },
    // --- wild ---
    Pattern {
        name: "the-clearing",
        kind: kinds::GROUND,
        attach: Attach::Hub,
        requires: None,
        needs_cold: false,
        built: false,
        // A hollow has a floor and a pool whatever anyone would use it for; the
        // role layer has nothing to say about unbuilt ground.
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-pool",
        kind: kinds::POOL,
        attach: Attach::Beside(kinds::GROUND),
        requires: None,
        needs_cold: false,
        built: false,
        roles: EVERY_ROLE,
        at_locale: true,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    // --- built, CHAMBER BAND ONLY (`at_locale: false`) ---
    //
    // Appended, never inserted: each requires a kind an EARLIER pattern
    // contributes, so the append position is also the dependency-correct one.
    // `needs_populous` WAS `true` here, and is `false` since decision 0398. The
    // Blocking's reason ("a hamlet has nothing worth locking up") was a good
    // claim about social scale and a wrong one about this world: no living
    // occupation in any measured world clears `HAMLET_POPULATION_CEILING`, so
    // the gate made the strongbox unreachable rather than rare. `roles:
    // &[Role::Store]` and `requires: Some(Vessel)` still confine it — a
    // strongbox stands in a room for keeping things, beside the water jar.
    Pattern {
        name: "the-strongbox",
        kind: kinds::STRONGBOX,
        attach: Attach::Beside(kinds::VESSEL),
        requires: Some(kinds::VESSEL),
        needs_cold: false,
        built: true,
        roles: &[Role::Store],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-high-seat",
        kind: kinds::HIGH_SEAT,
        attach: Attach::Beside(kinds::THRESHOLD),
        requires: Some(kinds::THRESHOLD),
        needs_cold: false,
        built: true,
        // A high seat is set where whoever sits in it sees who comes in. That is
        // what the seat is FOR, so `Beside(Threshold)` is the pattern, not decor.
        roles: &[Role::Hall],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-loom",
        kind: kinds::LOOM,
        attach: Attach::Beside(kinds::THRESHOLD),
        requires: Some(kinds::THRESHOLD),
        needs_cold: false,
        built: true,
        // Weaving wants light, and in a building with no windows the doorway is
        // where the light is.
        roles: &[Role::Loomroom],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-anvil",
        kind: kinds::ANVIL,
        attach: Attach::Beside(kinds::VESSEL),
        requires: Some(kinds::VESSEL),
        needs_cold: false,
        built: true,
        // The quench. An anvil without water within arm's reach is a smithy
        // nobody could work in, so the water jar is the anvil's requirement and
        // not merely its neighbour.
        roles: &[Role::Smithy],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    Pattern {
        name: "the-altar",
        kind: kinds::ALTAR,
        attach: Attach::Beside(kinds::VESSEL),
        requires: Some(kinds::VESSEL),
        needs_cold: false,
        built: true,
        // The washing the rite asks for before it begins.
        roles: &[Role::Shrine],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    // --- The Chattel (Task 11): the first authored CONTENTS ---
    //
    // Appended, and appended AFTER `the-strongbox`, which is the only position
    // the admissibility walk permits: `draw` admits a pattern only once its
    // `requires` kind is present, so a key placed before the strongbox that
    // holds it would be silently dropped from every composition.
    //
    // Its whole reason for existing is that nothing was ever inside anything.
    // The grammar's only `within` relation anywhere was `{(Alcove, Hearth)}`
    // (a census over all 60 production gate combinations, The Offer's Task 6,
    // re-measured on this tree by
    // `the_grammar_puts_exactly_these_things_inside_other_things` — which
    // reported exactly that pair the moment before this entry was written).
    // So `Openable` had nothing to reveal and `Lockable` nothing to lock, and
    // `open`/`close` would have shipped reporting nothing forever (spec §3.8).
    //
    // `at_locale: false` is load-bearing rather than copied: a locale-band
    // pattern feeds `warmth_at`, which feeds a creature's thermal drive, which
    // is committed history — appending one there is an EPOCH (see
    // [`Pattern::at_locale`] and `the_locale_band_draws_exactly_what_it_drew`).
    // A chamber-only append moves no saved world.
    //
    // `needs_populous` was authored `true` here to state the gate the strongbox
    // already implied, "so that a reader who later relaxes the strongbox's own
    // scale gate should have to see this one too". That reader arrived
    // (decision 0398) and the mirror worked exactly as intended: relaxing the
    // strongbox alone would have left the key gated behind a flag nothing else
    // set, and the key would have been silently dropped from every strongbox
    // that composed. Both are `false` now. The key stays confined by
    // `requires: Some(Strongbox)`, which is the honest gate: a key is inside a
    // strongbox or it is nowhere.
    Pattern {
        name: "the-key-in-the-strongbox",
        kind: kinds::KEY,
        attach: Attach::Within(kinds::STRONGBOX),
        requires: Some(kinds::STRONGBOX),
        needs_cold: false,
        built: true,
        roles: &[Role::Store],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    // --- The Chattel (Task 13, fix round 1): the SECOND key ---
    // --- The Custodian: moved off the doorway ---
    //
    // **The pattern above is the whole reason `take` could reach through a
    // locked lid, and this one is why closing that hole does not brick the
    // strongbox.** `the-key-in-the-strongbox` places the only `Portable` kind
    // in the game inside the only `Lockable` one, so the single key a played
    // world contained was the key to the box it was sealed in. A `take` that
    // refused a shut lid — which is what a lid MEANS — would then have made
    // the strongbox unopenable in every world, reverting decision 0398's
    // reachability ruling by a different route. The lock is only a lock if
    // there is a key somewhere else; the two changes are one change.
    //
    // **IT WAS `roles: &[Role::Threshold]` AND `requires: Some(Screen)`, AND
    // THE COST OF THAT WAS THE LOCK ITSELF.** Every built structure has a
    // threshold chamber — chamber index 0 is unconditionally `Role::Threshold`
    // — so a key stood in the entrance of every dwelling in
    // every world, and finding one was a formality rather than an event. The
    // Chattel chose that placement for a structural guarantee (a strongbox
    // implies a threshold in front of it, so "a reachable lock implies a
    // reachable key" needed no seed sweep to believe), and the guarantee was
    // real. What it bought was ubiquity.
    //
    // **The loom, and the role gate is the same idiom the fire uses.**
    // `the-loom` is `roles: &[Role::Loomroom]`, so `requires: Some(Loom)`
    // confines this key to the loomroom by the GRAMMAR rather than by a rule
    // anyone wrote — exactly the way `requires: Some(Alcove)` confines the
    // fire to the hearthroom. `roles` here and on `the-key-in-the-strongbox`
    // stay disjoint, which is what keeps
    // `no_production_room_composes_two_anchors_of_one_kind` true: no composed
    // interior can hold two `Key` anchors and collide their derived
    // `EntityId`s at ordinal 0.
    //
    // **WHY NOT THE SMITHY, WHICH IS WHERE A KEY IS ACTUALLY MADE.** Measured
    // on the 48-seed sweep, through `possess --seed N --script`, the role at
    // chamber index 2 is `Role::Loomroom` in **24 of 24** structures that have
    // an index 2 at all. `Role::Smithy`, `Role::Hall` and `Role::Shrine` occur
    // **zero** times: chamber index 2's role derivation reaches them only through
    // `Function::Mine | Function::Fort`, `Notability::Seat` and
    // `Function::Cult`, and no flagship a possession starts at carries one. A
    // key in the smithy would therefore be a key in no world — which is
    // decision 0398's own finding repeated on a different field: *a gate whose
    // predicate is false everywhere is not a gate, it is a deletion.* The
    // thematic reading was the better story and the wrong placement.
    //
    // **WHAT THE MOVE COSTS, STATED RATHER THAN GLOSSED.** The Threshold
    // placement made the reachability implication a property of chamber index
    // 0's role derivation; the Loomroom placement does not. Chamber index 2 is
    // `Role::Store` for a brief with no `Function`, so a three-chamber structure of that
    // shape would compose a strongbox with no key anywhere in the building.
    // No swept seed is that shape — all ten strongbox seeds read
    // `[Threshold, Hearthroom, Loomroom, Store]`, and `opened_with_that_key`
    // is unchanged at 10 of 48 across the move — but the guarantee is now a
    // measurement rather than a theorem, and `a_key_is_drawn_where_no_
    // strongbox_is` asserts the weaker claim it can actually prove.
    //
    // **The hearthroom and the store were both excluded, and neither on
    // taste.** The alcove is the only lidless container the grammar composes
    // and it is `roles: &[Role::Hearthroom]`; a key there would leave NO room
    // in the world that stands a container and composes no key, which is the
    // exact shape
    // `a_thing_put_into_a_container_the_grammar_never_composes_comes_back_out`
    // needs to reach `take_from_the_ledger`'s second source. The store is
    // where the strongbox stands, and a key in the room with the lock it
    // opens is the paradox this pattern exists to fix.
    //
    // **A key on the floor is a placeholder for a PERSON.** The right model is
    // that a resident holds this key or stashes it somewhere only they know,
    // and the custody mechanism for that already exists and is body-agnostic:
    // `thing::located_in_holder_fact` and `thing::held_by` take any
    // `EntityId`, not a player. What is missing is the resident —
    // `SOC-one-creature-per-settlement`. So `roles` is a **prop-management
    // knob, not a difficulty knob**: it says which rooms furnish a key while
    // nobody is home to carry one. See `PLAY-key-placement-stands-in-for-a-
    // resident` in the idea registry.
    //
    // `Attach::Beside`, not `Within`: a loom is not a container, so this
    // composes no new (container, contained) pair and
    // `the_grammar_puts_exactly_these_things_inside_other_things` is
    // untouched. `at_locale: false` for the reason its sibling states — a
    // locale-band append feeds `warmth_at` and is an EPOCH. `needs_cold:
    // false`: a household keeps its key whether or not it keeps a fire.
    // Appended rather than inserted, and the append position is still the
    // only admissible one: `draw` admits a pattern only once its `requires`
    // kind is present, and `the-loom` is index 11.
    Pattern {
        name: "the-key-by-the-loom",
        kind: kinds::KEY,
        attach: Attach::Beside(kinds::LOOM),
        requires: Some(kinds::LOOM),
        needs_cold: false,
        built: true,
        roles: &[Role::Loomroom],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    // --- The Wicket, Task 5: the brazier ---
    //
    // The proof kind: a kind that could not have existed under the closed
    // `AnchorKind` enum this campaign deleted, arriving as five data rows
    // and no dispatcher edit. `RadiatesHeat` gates `warm` the same way
    // `hearth`'s row does (`affordance::object_registry`); this is simply a
    // second carrier, outside a hearthroom.
    //
    // **Placed in the loomroom, not the shrine.** An earlier draft of this
    // task placed it in a shrine; `Role::Shrine` measures **zero** across the
    // 48-seed sweep this file's own comments record (chamber index 2's role
    // derivation reaches it only through `Function::Cult`, which no flagship a possession starts
    // at carries), so that placement would have been a brazier in no world —
    // decision 0398's own finding, repeated on a third field after the two
    // key patterns above it already state it once each. `Role::Loomroom` is
    // the one role the sweep found at chamber index 2 in 24 of 24 structures
    // that have an index 2 at all, and Step 1's own probe (seeds 42, 7, 1234)
    // confirms both existence (all three) and reachability (seed 42's
    // flagship walks straight to it).
    //
    // `attach: Attach::Beside(kinds::LOOM)` and `requires: Some(kinds::LOOM)`
    // for the same reason `the-key-by-the-loom` above it takes both: the
    // grammar confines this to the loomroom without a rule anyone wrote, the
    // way `requires: Some(Alcove)` confines the fire to the hearthroom.
    // `Attach::Beside`, not `Within`: a brazier is not a container, so this
    // composes no new (container, contained) pair.
    //
    // **Append, never insert.** `draw` admits a pattern only once its
    // `requires` kind is present, so `the-loom` must precede this row or the
    // brazier is silently dropped from every composition it would otherwise
    // join. `at_locale: false` keeps the append LATENT under `INVENTORY`'s
    // own three-part epoch rule (the chamber renderer reads it; no derivation
    // that writes a Fact or consumes a stream does).
    Pattern {
        name: "the-brazier",
        kind: kinds::BRAZIER,
        attach: Attach::Beside(kinds::LOOM),
        requires: Some(kinds::LOOM),
        needs_cold: false,
        built: true,
        roles: &[Role::Loomroom],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Universal,
    },
    // --- The Housemark: culturally diagnostic threshold relations ---
    //
    // These are chamber-only appendages. Their gate is read only by
    // `selection_for`, so the locale composition remains the frozen one that
    // feeds the walk band's thermal history.
    Pattern {
        name: "the-command-seat-at-the-threshold",
        kind: kinds::HIGH_SEAT,
        attach: Attach::Beside(kinds::THRESHOLD),
        requires: Some(kinds::THRESHOLD),
        needs_cold: false,
        built: true,
        roles: &[Role::Threshold],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Authority(AuthorityMark::Command),
    },
    Pattern {
        name: "the-common-bench-by-the-ground",
        kind: kinds::BENCH,
        attach: Attach::Beside(kinds::GROUND),
        requires: Some(kinds::GROUND),
        needs_cold: false,
        built: true,
        roles: &[Role::Threshold],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Authority(AuthorityMark::Common),
    },
    Pattern {
        name: "the-guest-water-at-the-threshold",
        kind: kinds::VESSEL,
        attach: Attach::Beside(kinds::THRESHOLD),
        requires: Some(kinds::THRESHOLD),
        needs_cold: false,
        built: true,
        roles: &[Role::Threshold],
        at_locale: false,
        needs_populous: false,
        housemark_gate: HousemarkGate::Threshold(ThresholdPosture::Outward),
    },
];

/// The patterns a room draws, DERIVED from what it already is — never authored
/// per culture. Admissibility is order-sensitive: a pattern that COMPLETES
/// another is admitted only once that other has been admitted, so the inventory
/// order encodes the grammar's dependency order.
///
/// **Keyed by NAME, never by position.** A future seeded draw must select on
/// `p.name`; keying on an index would silently re-roll every room the moment a
/// pattern is inserted (the same bug class as an id-as-offset, one scale up).
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold)
pub fn selection(built: bool, cold: bool) -> Vec<&'static Pattern> {
    // The LOCALE band: no role (a creature stands in a place, not in a room with
    // a purpose), never populous (no locale-band pattern is population-gated),
    // and only patterns this band draws at all.
    draw(built, cold, false, |p| p.at_locale)
}

/// The patterns a CHAMBER of `role` draws. [`selection`]'s sibling, sharing its
/// composer and its admissibility walk and differing only in the declared
/// vocabulary — which is spec §4.1's claim ("the same composer, a different
/// declared vocabulary") reduced to one predicate argument.
///
/// `populous` is [`crate::brief::Brief::is_populous`]: whether the place ever
/// held more people than a hamlet. `housemark` is absent until a caller has a
/// living society to derive one from, and then admits only matching typed gates.
///
/// It does NOT filter on `at_locale`: a chamber draws the shared vocabulary
/// *and* the chamber-only patterns. That asymmetry is the whole gate — see
/// [`Pattern::at_locale`].
/// type-audit: bare-ok(flag: built), bare-ok(flag: cold), bare-ok(flag: populous)
pub fn selection_for(
    role: Role,
    built: bool,
    cold: bool,
    populous: bool,
    housemark: Option<Housemark>,
) -> Vec<&'static Pattern> {
    draw(built, cold, populous, |p| {
        p.roles.contains(&role) && p.housemark_gate.admits(housemark)
    })
}

/// The one admissibility walk over the authored [`INVENTORY`]. `admits` is the
/// band's declared vocabulary; every other gate is a property of the place.
///
/// Order-sensitive by design, and shared so that the two bands cannot drift into
/// two different readings of what "completes" means.
fn draw(
    built: bool,
    cold: bool,
    populous: bool,
    admits: impl Fn(&'static Pattern) -> bool,
) -> Vec<&'static Pattern> {
    draw_from(&INVENTORY, built, cold, populous, admits)
}

/// [`draw`] over an arbitrary inventory — the seam that keeps a filter arm no
/// authored pattern exercises from becoming a claim nothing checks.
///
/// It exists for exactly one reason (decision 0398): since the strongbox and
/// its key were relaxed, no [`INVENTORY`] entry sets
/// [`Pattern::needs_populous`], so the `needs_populous && !populous` arm below
/// is unreachable from production. Deleting the arm would delete a real grammar
/// capability; leaving it unexercised would hand the next author who writes
/// `needs_populous: true` a filter nothing has run since the day it went idle.
/// Feeding a SYNTHETIC inventory through the same walk keeps the arm live
/// without putting a pattern in the world to serve a test.
///
/// Private, and takes `&'static [Pattern]` rather than a lifetime parameter,
/// because the two production callers hand it the promoted `&INVENTORY` and the
/// test hands it a `static` — no third shape exists, and inventing one would be
/// the abstraction this seam is trying not to become.
fn draw_from(
    inventory: &'static [Pattern],
    built: bool,
    cold: bool,
    populous: bool,
    admits: impl Fn(&'static Pattern) -> bool,
) -> Vec<&'static Pattern> {
    let mut out: Vec<&'static Pattern> = Vec::new();
    let mut present: std::collections::BTreeSet<KindId> = std::collections::BTreeSet::new();
    for p in inventory.iter() {
        if p.built != built {
            continue;
        }
        if p.needs_cold && !cold {
            continue;
        }
        if p.needs_populous && !populous {
            continue;
        }
        if !admits(p) {
            continue;
        }
        if let Some(req) = p.requires
            && !present.contains(&req)
        {
            continue;
        }
        present.insert(p.kind);
        out.push(p);
    }
    out
}

/// Compose the selected patterns into one interior, honouring each pattern's
/// attachment. The first `Ground` anchor is the hub; everything else attaches to
/// the hub, beside a named kind, or within one. Depth comes from the chain
/// (`Within` then `Beside`), which is what keeps the result from collapsing to
/// a star.
pub fn compose(selected: &[&Pattern]) -> Interior {
    let mut interior = Interior::new();
    let mut hub: Option<AnchorId> = None;
    // First placed anchor of each kind — the attachment target.
    let mut first_of: std::collections::BTreeMap<KindId, AnchorId> =
        std::collections::BTreeMap::new();

    for p in selected {
        let target = match p.attach {
            Attach::Hub => hub,
            Attach::Beside(k) | Attach::Within(k) => first_of.get(&k).copied().or(hub),
        };
        let within = match (&p.attach, target) {
            (Attach::Within(_), Some(t)) => Some(t),
            _ => None,
        };
        let id = interior.push(p.kind, within);
        // A contained anchor is already linked by containment; anything else
        // needs an explicit edge (unless it IS the hub).
        if within.is_none()
            && let Some(t) = target
        {
            interior.connect(t, id);
        }
        if hub.is_none() && p.kind == kinds::GROUND {
            hub = Some(id);
        }
        first_of.entry(p.kind).or_insert(id);
    }
    interior
}

/// Whether a composition is well-formed. The first rule: the anchor graph must
/// be CONNECTED, or part of the room is unreachable and a creature could be
/// asked to walk somewhere it cannot get to.
/// type-audit: bare-ok(flag: return)
pub fn permits(interior: &Interior) -> bool {
    interior.is_connected()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::site::{Site, SiteKind};

    /// A settlement site, mirroring `built` — the same computation
    /// `brief_of` performs, kept here so every fixture below stays a real
    /// (built, site) pairing rather than an untested combination.
    fn settlement_site() -> Option<Site> {
        Some(Site::placed(SiteKind::Settlement, None))
    }

    #[test]
    fn selection_is_derived_from_conditions_not_authored_per_culture() {
        // The SAME inventory yields different sets under different climates —
        // the culture signal is derived, exactly as a phoneme inventory is.
        let cold = selection(true, true);
        let warm = selection(true, false);
        assert_ne!(
            cold.iter().map(|p| p.name).collect::<Vec<_>>(),
            warm.iter().map(|p| p.name).collect::<Vec<_>>(),
            "climate must change which patterns a people uses"
        );
        assert!(
            cold.iter().any(|p| p.kind == kinds::HEARTH),
            "a cold people builds around a fire"
        );
    }

    #[test]
    fn a_pattern_whose_requirement_is_absent_is_not_admissible() {
        // THE COMPOSITION RULE, and the whole test of whether this is a language
        // or a catalogue: `the-fireside-bed` completes `the-fire`, so it may not
        // be drawn where no fire was. This is Alexander's "patterns complete
        // other patterns" made checkable.
        let warm = selection(true, false);
        assert!(
            !warm.iter().any(|p| p.kind == kinds::HEARTH),
            "no fire in a warm room (fixture precondition)"
        );
        assert!(
            !warm.iter().any(|p| p.name == "the-fireside-bed"),
            "a bed BY THE FIRE cannot be drawn where there is no fire"
        );
        let cold = selection(true, true);
        assert!(
            cold.iter().any(|p| p.name == "the-fireside-bed"),
            "with a fire present, the pattern that completes it becomes admissible"
        );
    }

    #[test]
    fn wilderness_draws_natural_patterns_and_no_built_ones() {
        // The fine layer must exist where most agents live (spec §13 item 2).
        // A wilderness interior legitimately has NO threshold: seams belong to
        // room-graph edges, not to a room's interior (found by The Threshold).
        let wild = selection(false, false);
        assert!(!wild.is_empty(), "wilderness rooms get anchors too");
        assert!(
            wild.iter().all(|p| !p.built),
            "an unbuilt room contains no built patterns"
        );
        assert!(
            !wild.iter().any(|p| p.kind == kinds::THRESHOLD),
            "wilderness needs no doorway"
        );
    }

    #[test]
    fn composition_is_not_degenerate() {
        // THE ANTI-HUB TEST. A hub composition puts everything one hop from the
        // centre, so graph distance is 1-2 and field decay has nothing to decay
        // over. A real grammar produces DEPTH: some pair of anchors must be at
        // least three steps apart.
        let interior = compose(&selection(true, true));
        let ids = interior.ids();
        let mut deepest = 0usize;
        for a in &ids {
            for b in &ids {
                if let Some(path) = crate::interior::route_within(&interior, *a, *b, 256) {
                    deepest = deepest.max(path.len());
                }
            }
        }
        assert!(
            deepest >= 3,
            "the composed interior is degenerate (deepest route {deepest} hops); \
             a hub composition is a catalogue, not a language"
        );
    }

    #[test]
    fn a_composition_is_connected_and_the_validator_says_so() {
        for (built, cold) in [(true, true), (true, false), (false, false)] {
            let interior = compose(&selection(built, cold));
            assert!(
                interior.is_connected(),
                "composition (built={built}, cold={cold}) is walkable"
            );
            assert!(permits(&interior), "the validator accepts it");
        }
    }

    #[test]
    fn a_permitted_interior_is_routable_between_every_pair() {
        // CONNECTIVITY AND ROUTABILITY MUST AGREE. `permits` walks containment;
        // routing must too, or the validator green-lights a room a creature
        // cannot cross. This is the invariant a hearth-inside-an-alcove broke
        // silently — the anti-hub test still passed, via a different arm.
        for (built, cold) in [(true, true), (true, false), (false, false)] {
            let interior = compose(&selection(built, cold));
            assert!(permits(&interior));
            for a in interior.ids() {
                for b in interior.ids() {
                    assert!(
                        crate::interior::route_within(&interior, a, b, 256).is_some(),
                        "permitted interior (built={built}, cold={cold}) has no route \
                         {a:?} -> {b:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn the_intended_chain_is_the_deep_one() {
        // Not merely SOME 3-hop route: the route the grammar was designed to
        // produce. threshold -> ground -> alcove -> hearth -> bed.
        let interior = compose(&selection(true, true));
        let find = |k: KindId| {
            interior
                .ids()
                .into_iter()
                .find(|id| interior.anchor(*id).kind == k)
                .unwrap_or_else(|| panic!("a cold built room has a {k:?}"))
        };
        let door = find(kinds::THRESHOLD);
        let bed = find(kinds::BED);
        let plan = crate::interior::route_within(&interior, door, bed, 256)
            .expect("the bed is reachable from the door");
        assert!(
            plan.len() >= 4,
            "the intended chain is at least four steps, got {}: {plan:?}",
            plan.len()
        );
    }

    #[test]
    fn the_locale_band_draws_exactly_what_it_drew() {
        // THE EPOCH GATE, asserted as a list rather than as a property. Task 6
        // appended five patterns; if any of them reaches this list, the warmth a
        // creature's committed thermal drive read has changed, and that is an
        // epoch. Written out so a future append cannot pass by being "the same
        // shape".
        let warm: Vec<&str> = selection(true, false).iter().map(|p| p.name).collect();
        assert_eq!(
            warm,
            [
                "the-ground",
                "the-threshold",
                "the-alcove",
                "the-water-jar",
                "the-screen"
            ],
            "the LOCALE band's warm built composition moved: this is an epoch"
        );
        let cold: Vec<&str> = selection(true, true).iter().map(|p| p.name).collect();
        assert_eq!(
            cold,
            [
                "the-ground",
                "the-threshold",
                "the-alcove",
                "the-fire",
                "the-fireside-bed",
                "the-water-jar",
                "the-screen"
            ],
            "the LOCALE band's cold built composition moved: this is an epoch"
        );
        let wild: Vec<&str> = selection(false, false).iter().map(|p| p.name).collect();
        assert_eq!(wild, ["the-clearing", "the-pool"]);
    }

    #[test]
    fn no_pattern_the_locale_band_declines_can_reach_it_by_any_route() {
        // The structural counterpart to the list above: `selection` filters on
        // `at_locale`, so a chamber-only pattern is unreachable from the locale
        // band whatever gates it also carries. This is what makes the LATENT
        // outcome auditable — disarm it by flipping one `at_locale` to `true` and
        // the test above fires too.
        let locale_names: Vec<&str> = [(true, true), (true, false), (false, true), (false, false)]
            .into_iter()
            .flat_map(|(b, c)| selection(b, c))
            .map(|p| p.name)
            .collect();
        for p in INVENTORY.iter().filter(|p| !p.at_locale) {
            assert!(
                !locale_names.contains(&p.name),
                "{} is chamber-only and reached a locale composition",
                p.name
            );
        }
        assert!(
            INVENTORY.iter().any(|p| !p.at_locale),
            "no pattern is chamber-only, so this test asserts nothing"
        );
    }

    /// **No production room composes two anchors of one kind** — the census
    /// The Chattel's Task 1 ran once, made permanent.
    ///
    /// WHAT A RED HERE MEANS, and it is not "a pattern was added". It means the
    /// thing that made a pattern addition safe has stopped holding.
    /// `windows/vessel/src/thing.rs` keys a thing's `EntityId` on
    /// `(room facet, kind, ordinal)` and every caller hardcodes ordinal `0`,
    /// which is legitimate ONLY while each kind occurs at most once per
    /// composed interior. The moment one occurs twice, two distinct things in
    /// one room derive the SAME entity id: every fact about either keys to the
    /// other, and no gate in this tree can see it — a derived id has no
    /// collision assert behind `reuse_or_mint_entity`, and the world still
    /// saves, loads and renders.
    ///
    /// So the response is not to widen this test. It is the STOP row of the
    /// spec's §3.2 branch table (`docs/superpowers/plans/`'s Task 1, Step 2):
    /// an ordering rule is needed, and it must be keyed on something a layout
    /// epoch cannot change — **never** on the interior's `Vec` derivation
    /// order, which decision 0069 licenses to regenerate differently forever.
    /// Return to the spec; do not choose a rule here.
    ///
    /// Task 1 measured the STRONGER of the table's two green rows: not row 2
    /// ("duplicates exist, but only of unpromotable kinds") but row 1 —
    /// `CENSUS_ANY_DUPLICATE false`, every kind's count exactly 1. This test
    /// asserts row 1, so it fires on a duplicate of ANY kind, promotable or
    /// not, which is deliberately stricter than `thing.rs` strictly needs: a
    /// duplicate `Ground` would be a real change to what a room IS, and
    /// discovering it here beats discovering it when someone makes that kind
    /// promotable.
    #[test]
    fn no_production_room_composes_two_anchors_of_one_kind() {
        let mut combinations = 0usize;

        let mut census = |label: String, selected: Vec<&'static Pattern>| {
            combinations += 1;
            let interior: Interior = compose(&selected);
            let mut counts: std::collections::BTreeMap<KindId, usize> =
                std::collections::BTreeMap::new();
            for id in interior.ids() {
                *counts.entry(interior.anchor(id).kind).or_insert(0) += 1;
            }
            for (kind, n) in &counts {
                assert_eq!(
                    *n, 1,
                    "{label} composes {n} anchors of {kind:?}. thing.rs's \
                     hardcoded ordinal 0 now collides two things into one \
                     entity id: an ordering rule is needed, and it must not key \
                     on derivation order. See spec SS3.2's branch table (STOP \
                     row) and this test's doc comment."
                );
            }
        };

        // The LOCALE band: no role, never populous.
        for (built, cold) in [(true, true), (true, false), (false, true), (false, false)] {
            census(
                format!("selection(built={built}, cold={cold})"),
                selection(built, cold),
            );
        }

        // The CHAMBER band. `populous` is swept as well as `built`/`cold`
        // because `the-strongbox` is population-gated, so a sweep that omitted
        // it would under-cover exactly the pattern this invariant protects.
        for role in EVERY_ROLE {
            for built in [true, false] {
                for cold in [true, false] {
                    for populous in [true, false] {
                        census(
                            format!(
                                "selection_for({role:?}, built={built}, cold={cold}, \
                                 populous={populous})"
                            ),
                            selection_for(*role, built, cold, populous, None),
                        );
                    }
                }
            }
        }

        // The census's own accounting. Without this, a future edit that dropped
        // a loop would still pass every assertion above by measuring less —
        // which is the quietest way an invariant test stops being one.
        assert_eq!(
            combinations,
            4 + EVERY_ROLE.len() * 8,
            "the census no longer sweeps every production gate combination"
        );
        assert_eq!(combinations, 60, "the census swept {combinations}, not 60");
    }

    /// **What the grammar ever puts INSIDE anything** — The Offer's Task 6
    /// census re-run as a permanent test, over the same 60 production gate
    /// combinations `no_production_room_composes_two_anchors_of_one_kind`
    /// sweeps.
    ///
    /// It exists because the campaign's container half is only as real as
    /// this set is non-empty in the right place. The Offer measured the
    /// answer as `{(Alcove, Hearth)}` — a fire in a recess, and **nothing
    /// anywhere inside a strongbox**, which is why The Chattel's spec §3.8
    /// says outright that "contents must be authored, because nothing is ever
    /// inside anything today": `Openable`/`Lockable` would have had nothing
    /// to reveal and `open` would have reported nothing, forever, exactly as
    /// `Encloses` nearly did one campaign earlier.
    ///
    /// So this is an ANTI-VACUITY check, not a freeze for its own sake. A
    /// pattern authored with `Attach::Within` that no production combination
    /// selects would leave the feature reporting nothing while looking
    /// authored; the pair has to show up HERE, in a composition the gates
    /// actually produce, or it is not in the world.
    ///
    /// Frozen as an exact set, so a future `Attach::Within` addition is a
    /// deliberate edit here rather than a silent widening — the same
    /// discipline `the_locale_band_draws_exactly_what_it_drew` carries one
    /// band over.
    #[test]
    fn the_grammar_puts_exactly_these_things_inside_other_things() {
        let mut combinations = 0usize;
        let mut pairs: std::collections::BTreeSet<(KindId, KindId)> =
            std::collections::BTreeSet::new();

        let mut census = |selected: Vec<&'static Pattern>| {
            combinations += 1;
            let interior: Interior = compose(&selected);
            for id in interior.ids() {
                if let Some(container) = interior.anchor(id).within {
                    pairs.insert((interior.anchor(container).kind, interior.anchor(id).kind));
                }
            }
        };

        for (built, cold) in [(true, true), (true, false), (false, true), (false, false)] {
            census(selection(built, cold));
        }
        for role in EVERY_ROLE {
            for built in [true, false] {
                for cold in [true, false] {
                    for populous in [true, false] {
                        census(selection_for(*role, built, cold, populous, None));
                    }
                }
            }
        }

        // The same accounting the sibling census carries, and for the same
        // reason: a dropped loop would otherwise satisfy the set assertion
        // below by measuring less.
        assert_eq!(
            combinations,
            4 + EVERY_ROLE.len() * 8,
            "the census no longer sweeps every production gate combination"
        );
        assert_eq!(combinations, 60, "the census swept {combinations}, not 60");

        let expected: std::collections::BTreeSet<(KindId, KindId)> = [
            // The Offer's Task 6 finding, unchanged: a fire within an alcove.
            (kinds::ALCOVE, kinds::HEARTH),
            // The Chattel's Task 11 addition, and the whole point of it: a
            // production room that actually holds something inside a
            // container `open` can open.
            (kinds::STRONGBOX, kinds::KEY),
        ]
        .into_iter()
        .collect();
        assert_eq!(
            pairs, expected,
            "the set of (container, contained) pairs any production gate \
             combination composes has moved. An addition here is what makes \
             the container half of The Chattel non-vacuous; a REMOVAL takes \
             it back to reporting nothing"
        );
    }

    #[test]
    fn only_the_hearthroom_can_hold_a_fire_and_no_rule_says_so() {
        // THE CLAIM THAT MAKES THIS A LANGUAGE RATHER THAN A CATALOGUE. Nothing
        // forbids the fire to any role — `the-fire` declares `EVERY_ROLE` — and
        // it still burns in exactly one, because it REQUIRES an alcove and only
        // the hearthroom admits one. The confinement is a consequence of the
        // grammar, not a rule anyone wrote, and this test asserts both halves so
        // that a future edit which "helpfully" adds a role gate to the fire is
        // recognized as a loss rather than a tidy-up.
        let fire = INVENTORY
            .iter()
            .find(|p| p.name == "the-fire")
            .expect("the-fire is authored");
        assert_eq!(
            fire.roles.len(),
            EVERY_ROLE.len(),
            "the fire must be withheld from NO role; its confinement is grammatical"
        );
        for &role in EVERY_ROLE {
            let has_fire = selection_for(role, true, true, true, None)
                .iter()
                .any(|p| p.kind == kinds::HEARTH);
            assert_eq!(
                has_fire,
                role == Role::Hearthroom,
                "{role:?} and the fire disagree"
            );
        }
    }

    #[test]
    fn every_role_composes_something_the_validator_accepts() {
        // Swept over roles AND over the place-gates, because
        // `chamber_interior_of` is the only composer the session calls and a
        // role whose composition is unwalkable would strand a possession.
        for &role in EVERY_ROLE {
            for (built, cold, populous) in [
                (true, true, true),
                (true, true, false),
                (true, false, true),
                (true, false, false),
                (false, false, false),
            ] {
                let interior = compose(&selection_for(role, built, cold, populous, None));
                assert!(
                    permits(&interior),
                    "{role:?} (built={built}, cold={cold}, populous={populous}) \
                     composes an interior the validator rejects"
                );
                for a in interior.ids() {
                    for b in interior.ids() {
                        assert!(
                            crate::interior::route_within(&interior, a, b, 256).is_some(),
                            "{role:?}: no route {a:?} -> {b:?}"
                        );
                    }
                }
            }
        }
    }

    /// **The role-derivation half of this test moved to `structure::grammar`,
    /// over `frame_for`** (The Cruck, Task 5a) — `role_for` deleted, its
    /// index-and-brief derivation now belongs to `structure_at`
    /// (production) and the structure grammar (a built site, once Task 3
    /// wires it in). What stays here is the half that is genuinely about
    /// PATTERN SELECTION: a fort and a farm must not draw the same
    /// vocabulary for the roles the two respectively hold, checked directly
    /// on the roles themselves rather than by re-deriving them from a brief.
    #[test]
    fn a_fort_and_a_farm_draw_different_things_not_more_things() {
        let names = |role: Role| {
            selection_for(role, true, false, false, None)
                .iter()
                .map(|p| p.name)
                .collect::<Vec<_>>()
        };
        let fort = names(Role::Smithy);
        let farm = names(Role::Loomroom);
        assert!(
            fort.iter().any(|n| !farm.contains(n)) && farm.iter().any(|n| !fort.contains(n)),
            "one place's third room is a superset of the other's, which is a tier \
             list rather than a vocabulary: {fort:?} vs {farm:?}"
        );
    }

    /// **A hamlet's storeroom holds a strongbox, and the strongbox holds a
    /// key** — decision 0398, and the inversion of the test this replaces.
    ///
    /// The old test asserted the opposite (`!names(&hamlet).contains(
    /// &"the-strongbox")`) and was correct about the code and wrong about the
    /// world: no living occupation in any measured world clears the ceiling, so
    /// what it froze was not "rare in a hamlet" but "absent everywhere". A
    /// scale gate is a good idea in a world that has towns; this one does not
    /// yet, and the capability was the thing being spent.
    ///
    /// The hamlet is spelled at the CEILING rather than at zero on purpose: the
    /// old test's own boundary case, kept, so that this is a statement about
    /// the relaxation rather than about an empty brief.
    #[test]
    fn a_hamlet_composes_a_strongbox_with_a_key_inside_it() {
        let ceiling = hornvale_history::flesh::HAMLET_POPULATION_CEILING;
        let hamlet = crate::brief::Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            ceiling,
            true,
            false,
            settlement_site(),
            None,
        );
        assert!(!hamlet.is_populous(), "at the ceiling is still a hamlet");
        let names = selection_for(Role::Store, true, false, hamlet.is_populous(), None)
            .iter()
            .map(|p| p.name)
            .collect::<Vec<_>>();
        assert!(
            names.contains(&"the-strongbox"),
            "a hamlet's storeroom draws no strongbox, so the capability is \
             unreachable again: {names:?}"
        );
        assert!(
            names.contains(&"the-key-in-the-strongbox"),
            "the strongbox composes with nothing in it, so `open` reports \
             nothing: {names:?}"
        );
        // Scale is no longer a gate on this vocabulary AT ALL — asserted in
        // both directions so that relaxing one pattern and not the other
        // cannot pass here.
        let populous = selection_for(Role::Store, true, false, true, None)
            .iter()
            .map(|p| p.name)
            .collect::<Vec<_>>();
        assert_eq!(
            names, populous,
            "a town's storeroom and a hamlet's differ, so something is still \
             population-gated"
        );
    }

    /// **A key stands somewhere a strongbox cannot** — the grammar half of
    /// the lock's repair (Task 13, fix round 1).
    ///
    /// The lid gate `Session::take` grew in the same change is only safe
    /// because of this: while `the-key-in-the-strongbox` was the ONLY key
    /// pattern, refusing to reach through a shut lid made the box unopenable
    /// in every world. So what has to hold is not merely "a second key
    /// exists" but the two structural facts that make it reachable from the
    /// box:
    ///
    /// 1. The two key patterns' `roles` are DISJOINT, which is what keeps
    ///    `no_production_room_composes_two_anchors_of_one_kind` true — two
    ///    `Key` anchors in one interior would derive one `EntityId` at
    ///    ordinal 0 and silently fuse two things.
    /// 2. In an AGRARIAN place — the only third-room function any measured
    ///    flagship draws — chamber index 2's role is `Role::Loomroom` and
    ///    every `Role::Store` is at index 3 or deeper, so a structure holding a
    ///    strongbox holds this key in a shallower room of the same building
    ///    and a possession walks THROUGH it to reach the lock.
    /// 3. The loomroom stands no container at all, which is why the key did
    ///    not go to the hearthroom: the alcove is the only lidless `Encloses`
    ///    kind the grammar composes, and a key beside it would leave no room
    ///    in the world that stands a container and composes no key — the
    ///    shape
    ///    `a_thing_put_into_a_container_the_grammar_never_composes_comes_back_out`
    ///    needs.
    ///
    /// **CLAUSE 2 IS WEAKER THAN THE ONE IT REPLACES, AND THAT IS THE PRICE
    /// THE CUSTODIAN PAID FOR RARITY.** While the key was
    /// `roles: &[Role::Threshold]` the implication held for EVERY brief, since
    /// chamber index 0 is `Role::Threshold` unconditionally: the grammar
    /// carried "a reachable lock implies a reachable key" as a theorem. It no
    /// longer does. Chamber index 2 is `Role::Store` for a brief with no
    /// `Function`, so a three-chamber structure of that shape composes a
    /// strongbox and no key at all. The clause below is therefore asserted
    /// against an AGRARIAN brief and states only what is provable; the
    /// universal claim is gone and is not quietly retained in prose. What
    /// stands in for it is a measurement — 10 of 48 swept seeds reach a
    /// strongbox and all 10 open it, unchanged across the move — recorded in
    /// The Custodian's chronicle, and a measurement is a claim with a date.
    ///
    /// **DO NOT STRENGTHEN THIS CLAUSE BACK WITHOUT REOPENING DECISION 0516.**
    /// The universal claim reads like a defect to fix, and restoring it is the
    /// specific act 0516 exists to prevent: the gap is left open deliberately,
    /// because a key on a floor is a placeholder for the person who would hold
    /// it, and the custody mechanism it stands in for already takes any entity
    /// (`thing::held_by`). What is missing is the resident
    /// (`SOC-one-creature-per-settlement`). Once a building has one, "where is
    /// the key" becomes "who has it", and the question this clause cannot
    /// answer stops being asked.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: change `the-key-by-the-loom`'s
    /// `roles` to `&[Role::Store]` — the plausible copy of its sibling, which
    /// compiles and leaves an `INVENTORY` of the same length. Confirmed
    /// 2026-08-30, unfiltered over the whole crate — `870 tests run: 858
    /// passed, 12 failed, 3 skipped`: this test plus eleven custody tests
    /// that walk to the loomroom for a key, including
    /// `a_shut_lid_refuses_take_and_an_open_one_does_not`,
    /// `custody_survives_a_save_and_a_re_possession` and
    /// `the_snapshot_carries_what_the_body_holds`.
    ///
    /// ```text
    /// assertion failed: the two key patterns share a role, so one room
    /// composes two Key anchors and ordinal 0 fuses them: [Store] vs [Store]
    /// ```
    ///
    /// **`no_production_room_composes_two_anchors_of_one_kind` stays GREEN
    /// under it — measured in that same run, not predicted — for the same
    /// reason it did before the move, and the reason is worth keeping.** A
    /// Store-roled key does not collide with `the-key-in-the-strongbox` at
    /// ordinal 0: `requires: Some(Loom)` is unsatisfiable in a `Role::Store`
    /// chamber — a loom is `roles: &[Role::Loomroom]` — so `draw` drops the
    /// pattern altogether and the second key vanishes from the world instead
    /// of doubling in one room. Both outcomes are bad and only one of them is
    /// what the collision census watches for, which is exactly why clause 1
    /// is asserted HERE, on the roles themselves, rather than left to a
    /// downstream test that would only fire for a different reason.
    #[test]
    fn a_key_is_drawn_where_no_strongbox_is() {
        let key_roles = |name: &str| {
            INVENTORY
                .iter()
                .find(|p| p.name == name)
                .unwrap_or_else(|| panic!("{name} is an authored pattern"))
                .roles
        };
        let in_box = key_roles("the-key-in-the-strongbox");
        let by_the_loom = key_roles("the-key-by-the-loom");
        assert!(
            !in_box.iter().any(|r| by_the_loom.contains(r)),
            "the two key patterns share a role, so one room composes two Key \
             anchors and ordinal 0 fuses them: {in_box:?} vs {by_the_loom:?}"
        );

        // The second key is really drawn, in the band that draws it, with no
        // container beside it. `built=true, cold=false` is the plainest
        // production gate; `no_production_room_composes_two_anchors_of_one_kind`
        // sweeps the rest.
        let loomroom: Vec<&str> = selection_for(Role::Loomroom, true, false, false, None)
            .iter()
            .map(|p| p.name)
            .collect();
        assert!(
            loomroom.contains(&"the-key-by-the-loom"),
            "a loomroom draws no key, so closing `take`'s lid bypass leaves \
             the strongbox unopenable: {loomroom:?}"
        );
        let stands_a_container = selection_for(Role::Loomroom, true, false, false, None)
            .iter()
            .any(|p| {
                crate::affordance::carries(p.kind, crate::affordance::ObjectProperty::Encloses)
            });
        assert!(
            !stands_a_container,
            "the loomroom now stands a container, so the room shape \
             `a_thing_put_into_a_container_the_grammar_never_composes_comes_back_out` \
             needs may no longer exist: {loomroom:?}"
        );

        // THE KEY IS NOT IN EVERY BUILT ROOM ANY MORE — the whole point of
        // the move, asserted rather than described. The threshold chamber is
        // the one every structure has, and it must no longer furnish a key.
        let door: Vec<&str> = selection_for(Role::Threshold, true, false, false, None)
            .iter()
            .map(|p| p.name)
            .collect();
        assert!(
            !door.iter().any(|n| n.starts_with("the-key")),
            "the threshold chamber composes a key again, so a key stands in \
             the entrance of every built structure in every world: {door:?}"
        );

        // THE DEPTH ARGUMENT MOVED, NOT DISAPPEARED (The Cruck, Task 5a).
        // `role_for` used to let this test SEARCH for "the shallowest chamber
        // index whose role is Store" against an agrarian brief and compare it
        // to the loomroom's own index — a real check, once, because the
        // mapping from index to role lived here. It does not any more:
        // `chamber_interior_of` now takes a role directly, and pattern.rs has
        // no notion of "index" left to search over at all, so restating the
        // search with a hand-picked brief would search nothing but a literal
        // `2 < 3` — a tautology dressed as a measurement. The real claim (an
        // agrarian brief's business sits at chamber index 2, one shallower
        // than the first index that is unconditionally a Store) is pinned
        // where the index-to-role mapping actually lives now:
        // `structure::tests::chamber_two_differentiates_on_the_briefs_business_at_the_index_role_for_used`.
    }

    /// **The population filter still works, and no authored pattern proves
    /// it** — the guard decision 0398 owes the field it left behind.
    ///
    /// After the relaxation nothing in [`INVENTORY`] sets
    /// [`Pattern::needs_populous`], so `draw`'s `needs_populous && !populous`
    /// arm is unreachable from production. That is the quiet failure this
    /// test exists to prevent: the arm keeps compiling, the field keeps
    /// reading as a live capability, and the first author to write
    /// `needs_populous: true` inherits a filter nothing has run since the day
    /// it went idle.
    ///
    /// So the arm is driven directly, through [`draw_from`], against a
    /// SYNTHETIC inventory — a pattern authored for a test rather than for the
    /// world, which is the whole point: proving the mechanism must not cost a
    /// pattern in the world.
    #[test]
    fn the_populous_gate_still_works_though_no_authored_pattern_uses_it() {
        // The premise, asserted rather than assumed. If this fires, someone
        // authored a population-gated pattern: good — say so here, and check
        // that the production censuses in this file sweep both values of
        // `populous` for the role it belongs to (they do today).
        let authored = INVENTORY.iter().filter(|p| p.needs_populous).count();
        assert_eq!(
            authored, 0,
            "an INVENTORY pattern is population-gated again, so this test's \
             synthetic stand-in is no longer the only witness the filter has"
        );

        static SYNTHETIC: [Pattern; 2] = [
            Pattern {
                name: "test-ground",
                kind: kinds::GROUND,
                attach: Attach::Hub,
                requires: None,
                needs_cold: false,
                built: true,
                roles: EVERY_ROLE,
                at_locale: true,
                needs_populous: false,
                housemark_gate: HousemarkGate::Universal,
            },
            Pattern {
                name: "test-town-only",
                kind: kinds::STRONGBOX,
                attach: Attach::Beside(kinds::GROUND),
                requires: None,
                needs_cold: false,
                built: true,
                roles: EVERY_ROLE,
                at_locale: true,
                needs_populous: true,
                housemark_gate: HousemarkGate::Universal,
            },
        ];
        let names = |populous: bool| {
            draw_from(&SYNTHETIC, true, false, populous, |_| true)
                .iter()
                .map(|p| p.name)
                .collect::<Vec<_>>()
        };
        assert_eq!(
            names(false),
            vec!["test-ground"],
            "the populous filter admitted a population-gated pattern into a \
             hamlet, so `needs_populous: true` would silently do nothing"
        );
        assert_eq!(
            names(true),
            vec!["test-ground", "test-town-only"],
            "the populous filter withheld a population-gated pattern from a \
             town, so `needs_populous: true` would silently gate everything"
        );
    }

    /// `Brief::is_populous` still reads the HOISTED ceiling rather than a
    /// re-typed `150` — one number, one meaning, shared with the ruin model.
    ///
    /// Kept after decision 0398 relaxed the strongbox, and worth saying why:
    /// the predicate is still WIRED (`chamber_interior_of` passes it into
    /// `selection_for` on every chamber derivation) and is simply selecting
    /// nothing today, so the threshold it reads is still the thing a future
    /// `needs_populous: true` pattern would be gated on.
    #[test]
    fn is_populous_reads_the_shared_hamlet_ceiling() {
        let ceiling = hornvale_history::flesh::HAMLET_POPULATION_CEILING;
        let at = |n: u32| {
            crate::brief::Brief::from_parts(
                None,
                None,
                None,
                None,
                None,
                n,
                true,
                false,
                settlement_site(),
                None,
            )
        };
        assert!(
            !at(ceiling).is_populous(),
            "at the ceiling is still a hamlet"
        );
        assert!(at(ceiling + 1).is_populous(), "one over the ceiling is not");
    }

    #[test]
    fn the_validator_rejects_a_disconnected_composition() {
        // The first well-formedness rule (spec §6): an unreachable anchor means
        // part of the room cannot be used, so the composition is ill-formed.
        let mut broken = Interior::new();
        broken.push(kinds::HEARTH, None);
        broken.push(kinds::BED, None); // no edge — orphaned
        assert!(
            !permits(&broken),
            "the validator rejects an unreachable anchor"
        );
    }
}
