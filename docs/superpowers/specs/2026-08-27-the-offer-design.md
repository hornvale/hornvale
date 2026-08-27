# The Offer — objects advertise their verbs

**Campaign:** The Offer · **Branch:** `campaign/the-offer` ·
**Decision block:** 0346–0355 (main ceiling 0338 at reservation) ·
**Drafted:** 2026-08-27 · **Status:** G3 package pending.

**Arc IV.a of The Bridle** (`docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md:310`).
Predecessors: The Tackle (I.a), The Deed (I.b), The Hand (II), The Coercion (III).

*An offer is a thing advertised, not a thing consummated. That distinction is
the campaign's scope line, not a figure of speech.*

---

## 1. What occasioned it

The metaplan gives this arc one row: *objects advertise verbs (MAP-19 +
MAP-27)*, acceptance test *"a key says unlock-me-with-this; no verb×object
table exists anywhere."*

A survey of the tree found the row is hiding two separate inventions, and that
the second one is a much larger campaign than the first.

**Hornvale has no objects.** Not a thin object model — none. There is no
item/thing/container type in the workspace. There is no inventory, and its
absence is *enforced*: `clients/game/core/src/endpaper.rs:62` fails if the
string `inventory` ever appears in the rendered strip. No manipulation verb
(`take`, `drop`, `open`, `use`, `put`) exists in any dispatcher. There is no
name→entity lookup; a typed word resolves against a per-turn string catalog
rebuilt on every call (`windows/vessel/src/focalize.rs:79`) and yields a
`datum` string, never an entity.

What reads like an object is an **anchor**: a derived region inside a room's
graph, carrying exactly two fields.

```rust
// windows/vessel/src/interior/anchor.rs:60
pub struct Anchor {
    pub kind: AnchorKind,          // 14 fieldless variants
    pub within: Option<AnchorId>,  // the anchor it lies strictly inside (Ntpp)
}
```

A strongbox is that discriminant plus two authored strings in
`chamber_prose.rs`. It cannot be opened, locked, moved, or contain anything.

## 2. The scale, and where the arc is cut

The useful frame is not "objects vs no objects" but **how much state an
affordance's precondition reads**. That is an ordinal axis, and the collision
the metaplan's row walks into is one position on it.

```
  reads     position                          example                       status
  ---------------------------------------------------------------------------------
   0%   nothing                          look, help                      shipped
  10%   position only                    drink at water, enter           shipped -- ALL of it
  30%   derived object properties        a strongbox affords `open`      EMPTY  <- IV.a
  50%   the observer's knowledge         an unseen key is silent         built, unused here
  70%   the playthrough's daybook        a door YOU opened this run      daybook shipped (I.b)
  90%   committed world facts            a door someone else locked      BREAKS catch-up  <- IV.b
 100%   another agent's committed mind   a door that opens if believed   far future
```

Every precondition in the codebase today sits at 10%, and that is **structurally
enforced, not incidental** — `windows/vessel/src/action.rs:135`:

```rust
pub fn precondition_reads_committed_state(a: &Action) -> bool {
    match a { /* every variant, no wildcard arm */ }
}
```

Its own doc names this campaign's example as the thing that would end it:

> "catch-up (The Threshold) … replays a creature's movement while suppressing
> the actions that commit facts, which reconstructs a past that could actually
> have happened only while no movement is gated by a committed effect. **A
> barred door needing unbarring would end it.**"

**So the arc is cut at 50%.** IV.a occupies the empty 30–50% band; IV.b takes
90% together with the catch-up replay redesign that must accompany it. This
follows Arc I's own precedent, which split into I.a (The Tackle,
byte-identical extraction) and I.b (The Deed, the semantic change).

**Both clauses of the metaplan's acceptance test survive the cut, read
literally.** "A key *says* unlock-me-with-this" is advertisement — 30%. "No
verb×object table exists anywhere" is structural. What IV.a defers is the
unlocking that *stays* unlocked.

### 2.1 The mechanism was already named here

This is not a new invention. `windows/worldgen/src/components.rs:165`:

> "A selector over the per-domain component registries, for reflection — 'which
> kinds carry this component' (UNI-21's capability query; **the GOAP
> available-action set**)."

And UNI-21's registry row specifies that the capability set is *"DERIVED at
load … never in the world save (build-state, not world-state; no cross-version
hazard)."* That is §2's ≤50% constraint, arrived at independently by a frontier
row before this scale was drawn. IV.a is the first real user of a mechanism
that has been named, tagged, and left empty.

## 3. The mechanism

### 3.1 Properties are a kind-level component

Properties live in a `ComponentStore<AnchorKind, ObjectTraits>` — the kernel's
own store type (`kernel/src/component.rs:14`), held **vessel-locally**.
`AnchorKind` already derives `Ord`, so this needs no kernel, domain, or
worldgen change. `MaterialTraits` (`domains/terrain/src/lib.rs:274` — two rows,
granite and limestone, its own doc calling the field set "thin and honest") is
the model for the *shape*: a thin, honest, kind-keyed trait table.

This is **build-state, not world-state**. It satisfies §2's constraint
structurally rather than by discipline: a kind-level registry has nowhere to
put per-instance state, so IV.a *cannot* accidentally climb to 70%.

**A correction to an earlier draft, stated because it narrows a claim this
spec used to argue its own recommendation.** That draft said the store is
"joined into `WorldComponents` exactly as `MaterialTraits` is." It is not, and
should not be for IV.a. Joining requires a `KindId` key and a domain that owns
object kinds — and no object domain exists, so joining means *creating* one.
The consequence for §2.1's argument: UNI-21's "build-state, not world-state,
derived at load" holds exactly, and IV.a's query has the same shape as a
capability query, but `ComponentTag`/`kinds_with` is worldgen-side machinery
that IV.a does not reach. Joining is IV.b's move, when objects become mintable
entities validated against `WorldComponents::kinds()`.

The argument that actually decided A2 over A1 is untouched by this: a
`ComponentStore` is a data table, whereas a `fn(AnchorKind) -> &[Verb]` is a
match arm returning verbs — one refactor from the forbidden table.

**Rejected: a static `fn affordances(AnchorKind) -> &[Verb]`.** It is smaller
and it is disqualifying — a table keyed by kind returning verbs is one refactor
from being the verb×object table the acceptance test forbids. It would pass the
grep while being the thing the grep exists to catch.

**Rejected: ledger facts per object instance.** That is the 90% rung; it is
IV.b.

### 3.2 The offer is a derived query

```
affordances(object, body, observer)
    = { verb : required_properties(verb) ⊆ properties(object) }   -- M + N
      ∩ what this BODY can do        (Gibson; §3.4)
      ∩ what this OBSERVER knows     (§3.5)
```

Authoring cost is M verbs + N objects, never M×N outcomes. A new object kind
declares properties and gains every qualifying verb; a new verb declares
required properties and appears on every qualifying object. Neither edits the
other, and neither edits a dispatcher.

### 3.3 The property vocabulary

Minimal and **earned** — each property exists because a verb needs it, and a
property no shipped verb gates was cut rather than kept. Five, against the 14
existing `AnchorKind` variants. MAP-19's own cautionary bound is
Cyc: *enumerate nothing; keep the ruleset small and generative.*

```
  property         certainly carried by   verb it gates            status of verb
  ---------------------------------------------------------------------------------
  supports-rest    Bed                    sleep / Rest             EXISTS, body-relative
  holds-liquid     Pool, Vessel           drink / Drink            EXISTS
  affords-passage  Threshold              enter / out              EXISTS
  encloses         Strongbox              examine (recursive)      EXISTS
  radiates-heat    Hearth                 warm                     NEW
```

Four of the five retrofit **verbs that already ship**. That is deliberate: it
makes IV.a a unification of behaviour that already exists rather than a
speculative new subsystem, and it means the mechanism is exercised by real
traffic on day one.

**"Certainly carried by" is not the full assignment.** This spec names one
anchor kind per property that must carry it; which of the other 13 also do is
the implementer's call, made from the code. A property carried by exactly one
kind is a weak M+N demonstration, so the implementer should expect to assign
more — but a spec author choosing them from outside has been wrong here every
time it has been tried.

**A row this spec removed, and why it matters.** An earlier draft had
`bears-weight → climb`, marked "EXISTS, body-relative". `climb` is not a
general climbing verb: it is underground egress (`session.rs:2780`, *"You are
not underground; there is nothing to climb out of"*). Retrofitting it onto a
log would have been a semantic collision presented as a unification. The
property was cut rather than repaired, because no shipped verb earned it.

**`encloses` earns its place through `examine`, not through a new verb.**
Examining an enclosing anchor reports what lies within it, derived from the
`Interior`'s existing `within` (`Ntpp`) relation. That is a real, testable,
derived use of the RCC-8 already built — and it is containment without any
durable state, which is what keeps it inside IV.a.

Otherwise `examine` takes no property — it is universal, and stays so.

### 3.4 Affordances are body-relative (Gibson)

MAP-19 states this directly: *"a supporter to a sprite is not one to a giant."*
An affordance is a **relation** between object and body, not an intrinsic
property of either. `supports-rest` is where this becomes observable: whether a
bed affords rest depends on the body that would lie in it.

The Tackle (I.a) gave the possessed body `mass_kg`, for the clock's
`tempo(mass_kg)`. IV.a is its second consumer, and the first that makes the
distinction between an object's properties and a body's capacities *visible in
play*.

**Body-relativity must be ADDITIVE in IV.a, never restrictive.** `supports-rest`
carries it because a bed offering rest is a *new* place to rest alongside the
existing at-home precondition — nothing a body could do yesterday becomes
refused today. The tempting alternative, making `affords-passage` body-relative
(Gibson's canonical aperture experiment, and `interior/seam.rs` already ships
`SeamKind::{Narrow, Broad}`), would newly BLOCK traversal — changing where
creatures can go, moving gallery transcripts substantially, and perturbing NPC
pathing. That is a real campaign and it is not this one; it belongs with IV.b,
where restricted passage already lives.

**The implementer chooses the discriminating case.** This spec names the
property whose body-relativity must be demonstrated; it does not prescribe
which species, mass, or anchor demonstrates it. A plan author writing that from
outside the code has been wrong every time it has been tried here.

### 3.5 The offer is knowledge-gated, and this is the seam a lie would use

The offer passes through the observer's `Knowledge` store
(`windows/vessel/src/knowledge.rs:24`) before it is rendered. A body that has
not encountered a thing is offered nothing by it.

**The gate cannot deny anything in IV.a, and that is stated rather than
discovered.** Task 4 investigated and the review verified every citation:
`Session::new` calls `absorb_here` unconditionally before returning
(`session.rs:1167`), and `IdentityProjection::project` takes `_perception` and
never reads it (`knowledge.rs:131`), so darkness cannot suppress absorption.
`enter` descends from a locale already absorbed. There is therefore **no live
path in IV.a on which a body is offered an object whose room it does not
know.**

The gate ships anyway, wired (Nathan, at the §3.5 stop): Task 7 routes the four
advertisement surfaces *through* `offered_to_observer`, so it is live code with
an unreachable branch rather than dead code, and IV.b's durable objects give it
a firing case with no rewiring. The alternative considered and declined was
reverting it and moving §3.5 to IV.b entirely.

**What this costs, named so it cannot be mistaken:** §3.5 reads as delivered
and cannot be observed working. The machinery is correct and its tests are
mutation-proven — making `offered_to_observer` ignore its `known` argument
reddens `an_unencountered_object_offers_nothing` — but no seed, no session, and
no transcript in IV.a will ever exercise the denying branch. The chronicle must
say so too.

**IV.a ships truthful advertisement through this seam.** It does not ship the
lie. But the seam is where a lie plugs in later, and naming it now is what
keeps that a one-file change rather than a redesign — see followup 4. The
project has now shipped the same finding twice in two registers (The
Reticence: a host unreliable in ways it cannot help; The Coercion: a hold
invisible in the ledger and visible in testimony). An object whose offer is
wrong is the third.

### 3.6 Containment reuses the RCC-8 already built

`Interior` already computes RCC-8 relations over anchors — `within`
(`Ntpp`), `relation()`, `walkable_neighbors()`. `encloses` marks the anchors
that hold something within them.

**An earlier draft of this section drew the line in the wrong place, and the
correction is the better rule.** It said `encloses` marks containment that is
*semantic* (a strongbox contains) rather than merely *spatial* (an alcove is a
recess in a wall). Task 6 measured the consequence: a census over all 60
production gate combinations found the grammar's only `within` relation
anywhere is `{(Hearth, Alcove): 3}`, and **nothing is ever placed inside a
strongbox** — `the-strongbox` is `Attach::Beside(Vessel)`, a sibling, not a
container. So the semantic/spatial line put the property on the one anchor that
never holds anything, and the feature would have reported nothing, forever.

**The rule that replaces it is the interactive-fiction one** (Nathan, at the
§3.6 stop): *contents are revealed when a container is OPEN or TRANSPARENT.*
Inform and TADS both work this way. A wall nook is open and transparent, so its
hearth shows; a chest is closed, so its contents do not. Both `Alcove` and
`Strongbox` therefore carry `encloses` — the alcove reports, the strongbox is
silent, and **the strongbox is silent because it is CLOSED, not because
containment is unimplemented.**

**That reasoning also lands the arc cut in exactly the same place, for a better
reason than the one this spec originally gave.** Open/closed is *durable object
state* — a chest you open and that stays open is the 90% rung §2 defers. So
IV.a ships the containment read without the open/closed/transparent state
machine, and IV.b gets a firing case for free rather than a redesign. The
absence is stated, not discovered: **IV.a has no way to open anything.**

The conformance question this raises — whether the object model obeys IF world
rules generally — is captured as `MAP-if-world-conformance` in the idea
registry, a proposed fourth sibling to `tropes/`, `systems/` and `sentences/`.

**Known consequence, accepted:** `Interior` is derived per room and never
serialized (decision 0069 — verified structurally: `anchor.rs` carries no
serde derive at all), so anything contained evaporates with the bubble. That is
correct for IV.a, which has no durable state by construction, and is IV.b's
problem by design rather than by accident.

## 4. The four surfaces this unifies

Arc IV is not the first affordance-advertisement channel in the tree. It is the
fifth, and the first principled one. The four that exist are ad hoc and
mutually unaware:

| surface | what it is | where |
|---|---|---|
| `HELP` | a static verb list | `session.rs`, third of three copies kept in agreement by a two-way test |
| `examine`'s `datum` | one authored English sentence per kind | `chamber_prose.rs:52` |
| `NounEntry.kind` | a completion hint on the wire, consulted by no dispatcher | `snapshot.rs:252` |
| the chart legend | a second matcher inside `examine`, synthesizing throwaway `Noun`s | `session.rs:4611` |

IV.a derives all four from the single query in §3.2. This is the campaign's
largest *diff* and its smallest *risk*: the behaviour is already shipped and
already tested; what changes is that one derivation feeds it.

## 5. The wire

`NounEntry` gains an `affordances` field. The precedent for how is the field
beside it — `NounEntry.kind`'s own doc (`snapshot.rs:255`):

> "Optional on the wire: older mirrors load unchanged (serde default), newer
> fixtures carry it. Additive on `vessel/session/v2` per the schema discipline."

So: `#[serde(default)]`, additive, **no version bump**. Scene schemas are
cross-repo contracts (the external Orrery client), additive-or-versioned only;
this is the additive half, and it has a worked precedent in the same struct.

## 6. Acceptance test

The metaplan's one-liner, sharpened, and **two-way on purpose** — decision 0261
(a rule duplicated on purpose carries a two-way agreement test) is three days
old and this is its first application outside the campaign that minted it.

1. **A new object kind ships with properties only** — no dispatcher change —
   and the right verbs appear on it.
2. **A new verb ships declaring required properties only** — no object change —
   and it appears on every object that qualifies.
3. The four surfaces of §4 provably derive from one source.
4. No verb×object table exists, by grep.

**(1) and (2) together are the M+N proof; either one alone is satisfiable by an
M×N table.** (1) passes trivially if verbs are hardcoded per kind and you add a
kind that reuses an existing row. (2) passes trivially if objects are hardcoded
per verb. Only both directions, held at once, exclude the table.

`warm` (§3.3) is (2)'s witness: it is the one new verb, it declares
`radiates-heat`, and it must appear on the hearth without the hearth being
edited.

## 7. What drifts, and why that is expected

Committed artifacts that move, with the reason each moves:

- `book/src/gallery/possession-live.md`, `possession-seed-42.md` — the two
  gallery transcripts that contain `examine` output.
- `clients/game/core/tests/fixtures/session-seed-42-*.json` — verified to carry
  `NounEntry.kind` today (27 `floor`, 3 `place`, 3 `thing`, 2 `threshold`,
  7 `wall`, 1 `settlement`); they gain the additive `affordances` field.
- `docs/audits/type-audit-report.md` — any pub-boundary change drifts it; it is
  an aggregate of counts, so it must never be text-merged.
- `book/src/reference/concept-registry-generated.md` and the language dumps —
  **only if §8's concept registration is kept.**

All are declared in `docs/generated-paths.txt` and regenerable via `make
rebaseline`. This is expected drift, not breakage — but it lands in the same
commit as the change, and a reviewer reads the diff rather than accepting it.

**Not a prediction, a branch table.** The implementer does not "confirm an
empty diff":

- only the paths above moved → regenerate, commit in the same commit;
- `book/src/domesday/` moved → STOP; that reads the census, which this campaign
  does not touch;
- a path not listed here moved → STOP and report before regenerating.

## 8. Concept registration — the cut Nathan may make

The Confidant's epoch 12 is the exact precedent: `AffectLabel`, a **window**
type, has a matching concept pack in `domains/language`, kept in step **by a
test in `windows/vessel`, not an import, because a domain cannot depend on a
window.** Its stated motivation — *"registered so a culture can have — or lack
— a word for one"* — applies unchanged to object properties, and connects to
The Reticence's deficiency machinery.

The recommendation is to register the five properties as `ConceptKind::Quality`
(its definition is literally "an abstract property or attribute") in a new
accession epoch, appended and never merged into an earlier cohort, per
`accession.rs`'s absolute rule.

**A claim that was checked and withdrawn:** an earlier draft argued "register
now, because doing it later costs an extra epoch." That is false — epochs are
appended, so epoch 13 and epoch 14 cost the same. Nothing about deferring this
is more expensive later.

So the honest case is narrower: shipping a property vocabulary the world cannot
name recreates, one layer down, exactly the problem The Actants fixed for verbs.
The cost is a drift in three generated language artifacts. **It is severable —
IV.a's acceptance test does not need it** — and severing it is a clean cut.

## 9. Deliberately NOT in this campaign

- **Durable object state.** No `open` that stays open. §2, IV.b.
- **Object entities in the ledger.** `mint_instance_of_kind` works and is
  unused in production for anything but species collectives; IV.b would be its
  second real user.
- **Inventory, carrying, taking.** No verb moves an object. The wire's
  no-inventory assertion stays true and its test stays green.
- **The lying object.** The seam ships (§3.5); the lie does not.
- **MAP-19's naïve physics** — liquids, sense-passing, support chains,
  Baillargeon's acquisition order. Cyc's bound.
- **`BarrierState`.** It ships drawn, seeded, pinnable dials with zero effects
  (`worldgen/src/character.rs:33`) and nothing reads `barrier_of`. It is IV.b's
  obvious input and IV.a must not quietly become its first consumer.
- **Command parsing.** Unchanged from the metaplan's §5: the `!` sigil is
  notation, not committed grammar.

## 10. Risks

1. **§4 is a wide diff over shipped behaviour.** Four surfaces re-pointed at one
   derivation. The mitigation is that each has existing tests; the danger is a
   surface that silently starts deriving *differently*. Every surface needs a
   before/after equality assertion, not just a green suite.
2. **`session.rs` is 8,399 lines and `liveness.rs` is 15,476.** The metaplan
   already flagged this for Arc I. IV.a adds a module rather than growing
   either, but the four-surface unification touches `session.rs` in several
   places.
3. **A property set chosen from the 14 existing anchor kinds may not generalize.**
   The vocabulary is fitted to what exists. Arc V will author verbs the current
   five properties cannot gate. That is acceptable — the properties are data and
   extending them is the intended motion — but the spec should not pretend the
   six are complete.
4. **`domains/alchemy` is a second, disconnected property system** — a real
   substance/quality/production grammar with mass balance whose only consumer is
   `worldgen/src/alchemy.rs`, flagged by `domesday/detect.rs:488` as reachable
   with no live metric. IV.a must state explicitly that it does not connect,
   rather than leaving two property vocabularies to drift into each other.

## 11. Decisions this campaign proposes (block 0346–0355)

- **An affordance is derived, never committed.** (§2, §3.1)
- **An affordance is a relation between object and body, not a property of
  either.** (§3.4)
- **Properties are kind-level build-state; instance state is Arc IV.b.** (§3.1)
- **The offer passes through the observer's knowledge before it is rendered.**
  (§3.5)
- **M+N is proved two-way or not at all.** (§6)
- **A property is a `Quality`, and is registered before it is gated on.** (§8,
  G3) — the vocabulary a verb gates on must be nameable by the world, the same
  obligation The Actants imposed on verbs, one layer down.

## 12. G3 outcome (2026-08-27, Nathan)

**Resolved at the stop:**

- **Concept registration (§8) is KEPT.** The five properties are registered,
  accepting the drift in three generated language artifacts. The severable cut
  was offered and declined.
- **`ConceptKind::Quality`, not a new kind.** Its definition already is "an
  abstract property or attribute", and the one-word-per-concept rule points the
  same way. The Actants' `Act` and The Confidant's `Affect` were both minted for
  concepts that were *categorically* new — a thing done, a thing undergone.
  A property a thing HAS is what `Quality` already means, so this is the case
  those two precedents do not cover.
- **The wire field, the arc collision, and the campaign name ride as specced**
  (items 1, 4, 5 below), reviewed and not contested.

**Consequences now binding on the plan:** §8 is in scope, which means a new
accession epoch appended to `domains/language/src/accession.rs` — never merged
into an earlier cohort, per that module's absolute rule — plus a two-way
agreement test in `windows/vessel`, following The Confidant's epoch 12 exactly
(a domain cannot depend on a window, so the test lives window-side and asserts
in both directions).

## 13. What was flagged at G3, for the record

*Items 2 and 3 are resolved above; they are retained here as the record of what
was asked, since a G3 package is part of the campaign's provenance.*

1. **Schema-adjacent: the wire gains a field.** `NounEntry.affordances`,
   additive on `vessel/session/v2` with `#[serde(default)]`, no version bump.
   Precedent is exact (`NounEntry.kind`, same struct), but it is a cross-repo
   contract and the owner should see it named.
2. **[RESOLVED — KEPT]** Schema-adjacent: concept registration (§8) is severable. Recommended in,
   costs drift in three generated language artifacts, and the acceptance test
   does not require it. A clean cut if the owner wants a smaller campaign.
3. **[RESOLVED — `Quality`]** `ConceptKind::Quality` vs a new kind. If §8 is kept: The Actants minted
   `Act` and The Confidant minted `Affect` rather than overloading `Quality`, so
   there are two recent precedents for minting. This spec recommends `Quality`
   instead — its definition already *is* "an abstract property or attribute,"
   and the memory rule against an engine-vs-world vocabulary carve-out points
   the same way. But two campaigns in a row went the other way, and that
   deserves the owner's eye.
4. **The arc-numbering collision is inherited and still open.** The Reticence
   G3-flagged it (§10 item 1) and **no decision record resolved it** — verified
   by grep over `docs/decisions/`. The Confidant claims "Arc III" for content
   the metaplan's arc table does not list. This spec proceeds as "Arc IV.a"
   citing the metaplan's table explicitly, which is unambiguous and touches
   none of the collision. The three routes The Reticence named are unchanged:
   amend the metaplan and renumber; treat The Confidant's claim as an error to
   correct in its own spec; or stop numbering arcs.
5. **"The Offer" names IV.a, not the arc.** Three committed specs already say
   "Arc IV (The Offer)" for advertisement content; the metaplan's table gives
   sub-arcs the names ("I.a — The Tackle"), not arcs. IV.b will need its own
   name. Note `book/src/chronicle/the-standing-offer.md` exists and is
   unrelated (generated-artifact merge conflicts).
