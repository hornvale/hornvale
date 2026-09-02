# The Wicket — one kind model, not two

**Date:** 2026-09-01 · **Registry row:** `MAP-one-kind-model`
(`elaborated`, high) · **Essay:**
[frontier §the orange problem](../../../book/src/frontier/frontier.md#the-orange-problem--one-kind-model-not-two)
· **Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md`
· **Decision block:** 0556-0565 (main ceiling 0516 at reservation) — used:
0556, 0557, 0558

A wicket is a small gate set in front of open ground. This campaign takes one
down — and then finds the same fence built out of a number instead of an enum,
and takes that down too.

**The thesis, widened 2026-09-01 on Nathan's ruling.** A closed vocabulary and
an authored scalar are the same defect wearing different clothes: both are
world-facts fixed at compile time that should be rows keyed by kind.
`AnchorKind` says *these fifteen things exist*. `FATIGUE_RISE = 0.3` says *every
species in every world accrues sleep debt at this rate*. Neither is a fact about
Hornvale; both are facts about what nobody has keyed yet. So the campaign ships
both halves: the vocabulary becomes rows (§4-§6), and the first constant that
should never have been one follows it (§6b).

## 1. The problem, measured

Hornvale ships two kind models. A **species** is `KindId("goblin")` plus
whatever component tables happen to carry a row at that key — `biosphere_registry`,
`psyche_registry`, `perception_registry`, `family_of`, `habitat_realm`. Nothing
holds "the definition of a goblin"; a goblin is the join, and a species no table
mentions is a key with no consequences.

An **object** reaches the same shape through a fence. `affordance::object_registry()`
is a `ComponentStore<KindId, ObjectTraits>` — open, string-keyed, exactly the
species shape — and `hornvale_thing::thing_registry()` is another, already joined
into `WorldComponents`. But the vocabulary a room's grammar can name is
`AnchorKind`, a closed enum declared by the `anchor_kinds!` macro. A kind the
enum does not name cannot be placed in a room, however open the stores behind it
are.

### 1.1 The essay's numbers are wrong, and the correction is part of the work

The essay and the registry row both say "thirty-four variants, thirty-six
exhaustive match sites, seventeen files." Measured on this branch, and again at
`208efe4f1` (the essay's own commit, 2026-08-31):

```text
                                  claimed        measured today   measured at 208efe4f1
  AnchorKind variants             thirty-four    15               15
  exhaustive match sites          thirty-six      3               --
  files mentioning AnchorKind     seventeen      18 src / 22 all  --
```

The three exhaustive sites are `chamber_prose::noun`, `chamber_prose::detail`
and `affordance::thing_kind_of`. The fence is roughly 2.3x smaller than
advertised, and it was already 15 variants on the day the sentence was written —
so this is a drafting error, not drift. Both the essay and the registry row are
corrected as part of this campaign's DoD (§11).

### 1.2 What the fence actually costs

Fifteen variants, three exhaustive matches, and about 340 mentions across 18
source files — but the production predicates are almost all one shape:

```text
  interior/field.rs:70    kind != AnchorKind::Hearth        (warmth falls off from fires)
  interior/seam.rs:61,66  kind == AnchorKind::Threshold / Ground
  interior/derive.rs      kind == AnchorKind::Hearth / Threshold
  interior/pattern.rs     Pattern { kind, attach, requires } — the grammar's vocabulary
  chamber_prose.rs        noun / detail — the two exhaustive matches
  affordance.rs           thing_kind_of — the third, the adapter to KindId
```

Everything else is tests. `liveness.rs`'s 53 mentions and `session.rs`'s 34 are
test-module constructions of hand-built interiors.

## 2. What must survive

Four properties are load-bearing and the design is shaped by them.

**Totality.** Every kind that can be placed in a room has a noun (or a declared
absence of one — `Ground` is the floor, not a thing standing in it), a detail
line, and a thing-kind row. Today the compiler enforces this by exhaustiveness.
That guarantee is the point of the enum; the closedness is only the means.

**Byte-identity.** Same seed and pins, byte-identical worlds. The re-key must
not reorder an interior.

**Layering.** `kernel` → `domains/*` → `windows/*` → `cli`. A domain may not
depend on a sibling; a window may depend on a domain.

**Failing loudly.** A kind with no prose row must be a refusal with a physical
reason, never a silent default. `unwrap_or(default)` is the failure mode a map
has that a match does not.

### 2.1 The re-key is ordering-safe, verified

Three `BTree*` collections in this workspace are keyed on `AnchorKind`, and
every one is lookup-only:

```text
  pattern.rs:562   present: BTreeSet<AnchorKind>          contains / insert only
  pattern.rs:631   first_of: BTreeMap<AnchorKind, AnchorId>  get / entry only
  pattern.rs:909   counts: BTreeMap<AnchorKind, usize>     (a test)
```

Interior order comes from `selected`, a `Vec` in `INVENTORY` declaration order,
and `compose` pushes in that order. `AnchorKind::ALL` — the generated roster —
is read only by tests. So changing the key type from an enum ordered by
declaration to a `KindId` ordered lexically changes no iteration anywhere, and
the re-key alone moves no bytes. This is stated as a verified fact rather than
an expectation, and Task 2's acceptance criterion is the empty diff that
demonstrates it.

## 3. Non-goals

- **Kind-to-kind edges.** `family_of` is a single edge type flattened to a
  column; the general form is a graph over `KindId`. That is the second of
  MAP-one-kind-model's three additions and it is not in this campaign (§8
  records why the sequencing argument survived an ideonomy pass that pushed
  against it).
- **Per-instance components derived from `Lineage`.** The third addition.
- **Systems, schedulers, archetype storage.** The essay's "where the ECS
  analogy stops" is settled and this campaign does not reopen it: a scheduler
  would be a second cross-domain communication mechanism competing with the
  trace protocol, and iteration order would become an undeclared save-format
  contract.
- **The people-side and individual-side halves of sleep quality.** The
  campaign builds the OBJECT side — rest recovers more where the room affords
  more, using property rows that already exist (§6b). It does not build *what a
  people tends to sleep on* (a kind-to-kind edge, addition two) or *this one
  likes a sleeping bag* (a `Lineage`-derived per-instance value, addition
  three). §6a states the split and why the space for both is free.
- **Runtime-generated kinds.** `KindId(pub &'static str)` — kinds are authored
  and committed, which is the "models author, dice roll" constraint. "Kinds as
  data" here means *authored data*, never *derived at runtime*.

## 4. Design: a kind is a row

### 4.1 The roster already exists

`hornvale_thing::THING_KINDS` is a `&[&str]` roster of authored labels, and
`roster_and_registry_agree_in_both_directions` already pins it against
`thing_registry()` in both directions. It carries **16** rows: one for each of
the 15 `AnchorKind` variants, plus `cave-mouth` — a kind no variant has ever
backed, because a cave mouth is a `Vertex`/`ChamberAddr` and was never
expressible in the enum. That one row is the openness this campaign
generalises: the store behind the fence already holds a kind the fence cannot
name. The destination is built; the work is pointing the grammar at it.

(`affordance.rs`'s own doc says `key` and `cave-mouth` are both rows with no
`AnchorKind` behind them. That was true when Task 7 wrote it and stopped being
true four tasks later, when Task 11 added the `Key` variant. Verified against
the enum, not the comment.)

### 4.2 Named handles, not variants

Production predicates must not become stringly typed. `kind == KindId("hearht")`
compiles. So `domains/thing` publishes named constants beside the roster:

```rust
pub const HEARTH: KindId = KindId("hearth");
pub const THRESHOLD: KindId = KindId("threshold");
pub const GROUND: KindId = KindId("ground");
// ... one per kind that code names
```

and a test asserts every published constant's label is a `THING_KINDS` row.

**The asymmetry between a constant and a variant is the whole campaign.** A
variant is *mandatory* — a kind with no variant cannot exist. A constant is a
*convenience for code that must name the kind* — a kind with no constant is a
first-class kind that simply has no predicate written against it. Adding a kind
is a data edit: a `THING_KINDS` row, a `thing_registry` row, a prose row, and
optionally an `object_registry` row. No enum, no match arm, no macro.

### 4.3 The interior grammar re-keyed

`Anchor::kind`, `Interior::push`, `Pattern::kind`, `Pattern::requires` and
`Attach::{Beside, Within}` all take `KindId`. `anchor_kinds!`, `AnchorKind`,
`AnchorKind::ALL` and `affordance::thing_kind_of` are deleted — `thing_kind_of`
is an adapter between two vocabularies, and after this there is one.

`windows/vessel` gains a `hornvale-thing` dependency. This is layering-legal (a
window may depend on a domain; vessel already depends on eight of them) and it
lets `cli/tests/suite/anchor_thing_correspondence.rs` — which lives in `cli/`
only because `cli` was the one crate depending on both — become a vessel test or
disappear into the totality gate.

### 4.4 Prose becomes a component table

`chamber_prose::noun` and `chamber_prose::detail` become one vessel-local store:

```rust
pub struct ChamberProse {
    /// How prose names this kind, or None for a kind that is the room itself
    /// rather than a thing standing in it (the floor).
    pub noun: Option<&'static str>,
    /// The line examine gives, total where noun is not.
    pub detail: &'static str,
}

pub fn chamber_prose_registry() -> ComponentStore<KindId, ChamberProse>
```

Prose is presentation, so it stays in the window; `display` stays in
`domains/thing` where it already is. The lookup returns `Option` and callers
refuse rather than default (§5.2).

## 5. The totality gate

This is the deliverable. The compiler stops enforcing totality; a default-deny
test takes over, and it must be *better* than what it replaces, not merely
present.

### 5.1 The direction each check enforces, named in its own doc comment

A gate asserting *declared ⊆ resolvable* is structurally blind to
over-admission and still reads as total to the next reader. So each check says
which way it runs:

```text
  G-a  every KindId an INVENTORY Pattern names — kind, requires,
       Attach::Beside/Within target — is a THING_KINDS row      (declared ⊆ rostered)
  G-b  every THING_KINDS row has a chamber-prose row            (rostered ⊆ prosed)
  G-c  every chamber-prose row is a THING_KINDS row             (prosed ⊆ rostered)
  G-d  every published KindId constant is a THING_KINDS row     (named ⊆ rostered)
  G-e  every object_registry key is a THING_KINDS row           (propertied ⊆ rostered)
  G-f  THING_KINDS is frozen as an ORDERED SET, not a count
```

G-f exists because a size ratchet passes any compensating swap: a count is not a
membership. The frozen list is the ratchet, so adding a kind is a visible,
deliberate edit to a committed roster — the same discipline `AnchorKind::ALL`
bought by being generated, kept without the closedness.

There is deliberately **no** check that every rostered kind is placeable by some
pattern. `cave-mouth` is a rostered kind that is a `Vertex`/`ChamberAddr` and
never an anchor, and inventing an exemption list to keep a reverse check green
would be a list nobody maintains.

### 5.2 Refusal, not default

`compose` and the prose surfaces take a kind's prose by lookup. A miss is a
refusal naming the kind, not a fallback string. G-b makes a miss unreachable
from the authored inventory; the refusal is what makes the unreachable case
loud if G-b is ever weakened.

### 5.3 The mutations these tests must fail against

Per decision 0353, each gate is specified by the mutation it must catch. These
are named as *properties*, and the implementer finds the concrete mutation by
reading — a plan author does not know which mutation is discriminating.

- A pattern naming a kind with no roster row must redden G-a. (Today the
  compiler catches this; after the re-key nothing else does.)
- Deleting one prose row must redden G-b, and adding a prose row for a
  non-existent kind must redden G-c. Both directions, because the cheapest
  repair to a one-way check deletes the check.
- Appending a kind to `THING_KINDS` without updating the frozen ordered set must
  redden G-f; swapping two rows without changing the count must also redden it.
- Replacing a prose lookup's refusal with a default string must redden a test
  that stands a kind with no prose row in a room.

## 6. The proof: a kind the enum could not have named

Decision 0398 — *a capability nothing can reach is not a capability* — was
minted on 2026-08-29 against exactly this shape: The Chattel shipped a
container, a lock, a key and an `open`/`close` pair no session in any world
could stand in front of. An open kind vocabulary that places nothing new is the
same failure. So the campaign places one kind, and the diff that adds it is the
evidence.

**What it actually cost, measured rather than predicted.** This paragraph said
"a `THING_KINDS` row, a `thing_registry` row, a prose row, an `object_registry`
row, and one appended `Pattern`. No enum edit, no match arm, no macro, no
dispatcher edit." The first half held. The second half was too strong, and Task
5 found the difference by running:

```text
  the grammar path        roster row, registry row, prose row,     AS PREDICTED
                          object_registry row, appended Pattern
                          -- no enum, no match arm, no dispatcher
  the concept path        concept_doc() exhaustive match arm       NOT PREDICTED
                          EPOCH_COHORTS accession cohort (ep. 18)  NOT PREDICTED
  the artifacts           keystone golden + 2 byte-goldens,        NOT PREDICTED
                          purely additive (6 insertions, 0 deletions)
```

So the honest claim is narrower and still worth making: **the room grammar's
kind vocabulary is open, and adding a kind touches no dispatcher, no enum and
no match arm in that path.** But a kind is also a *concept*, and the concept
side still has two closed lists of its own — an exhaustive `concept_doc` match
and a hand-maintained accession cohort. `MAP-one-kind-model`'s fence came down
in `windows/vessel`; an equivalent one is still standing in `domains/thing` and
`domains/language`, one layer over. That is a finding, not a failure: it is the
same shape the campaign was built to remove, and it is now named and located.

**The kind: `brazier`** — a standing pan of coals, carrying `RadiatesHeat`,
**in the loomroom**.

> **Corrected 2026-09-01, before Task 5, and the correction is the point.** This
> section originally placed the brazier in a shrine (`Role::Shrine`). That role
> occurs **zero times** in any world a possession can reach, and the project had
> already measured it: a comment in `interior/pattern.rs` records a 48-seed
> sweep through `possess --seed N --script` finding `Role::Loomroom` at chamber
> index 2 in **24 of 24** structures that have an index 2 at all, with
> `Role::Smithy`, `Role::Hall` and `Role::Shrine` at zero — because `role_for`
> reaches them only through `Function::Mine | Function::Fort`,
> `Notability::Seat` and `Function::Cult`, and no flagship a possession starts
> at carries one. That comment draws the conclusion in so many words: *"a key in
> the smithy would therefore be a key in no world — which is decision 0398's own
> finding repeated on a different field: a gate whose predicate is false
> everywhere is not a gate, it is a deletion."*
>
> A brazier in a shrine is a brazier in no world. Choosing a role this campaign
> cites 0398 to justify, without checking a 48-seed measurement sitting in the
> file the campaign edits, would have shipped the exact defect 0398 names. The
> shrine reasoning was the better story and the wrong placement — the same
> sentence that campaign wrote about its key.

`warm` is genuinely enforced. `Session::warm` (`session.rs:2624`) asks
`offered_to_observer` of every anchor in the room and refuses with *"There is no
fire here to warm yourself at"* if none offers `Warm`. `hearth` is the only
`RadiatesHeat` carrier, and a hearth is confined by a three-link chain —
`the-fire` requires `Alcove`, and `the-alcove` declares
`roles: &[Role::Hearthroom]` — so **no room but a hearthroom can be warmed at**,
and that includes the loomroom every reachable three-chamber structure has. A
brazier beside the loom makes a refused act succeed, in 24 of 24 swept
structures rather than none.

It is also the better placement on its own terms, not merely the reachable one:
fine work at a loom wants light and warmth, so the anchor is earned by the
activity it affords — `anchor.rs`'s standing rule — rather than by decoration.

The choice is not merely available; the code asks for it by name. `warm`'s own
doc comment, written when the method was fixed to read the offer instead of a
hardcoded `AnchorKind::Hearth` literal:

> *A future `RadiatesHeat` carrier (a cauldron of coals on `AnchorKind::Vessel`,
> say) would have needed an edit HERE as well as an `object_registry` entry —
> exactly the M×N dispatcher-edit this campaign exists to abolish.*

The brazier is that carrier. It is the anticipated case, arriving to find the
edit already unnecessary — which is a stronger proof than a kind chosen to suit
the campaign.

```text
  the-brazier   kind: brazier   roles: [Loomroom]   built: true
                attach: Beside(loom)   requires: loom
                at_locale: false        properties: [RadiatesHeat]
                noun: "a brazier"
```

**Appended, never inserted, and after `the-loom`.** `INVENTORY` is a
fixed-size `[Pattern; 16]` and `draw` admits a pattern only once its `requires`
kind is already present, so the order IS the dependency order: inserting or
reordering is an epoch, and a brazier placed before the loom it requires would
be silently dropped from every composition. The array's length becomes 17 in the
same edit.

**`at_locale: false` is the reason this is affordable.** `INVENTORY`'s own doc
states the three-part epoch rule: reordering is always an epoch; appending with
`at_locale: true` is an epoch, because a locale composition feeds `warmth_at`,
which feeds a creature's thermal drive, which is committed history; appending
with `at_locale: false` is **latent** — `selection` filters it out and the only
other consumer, `selection_for`, is read by the chamber renderer and by nothing
that commits. A chamber-band brazier therefore changes what a player finds on
delving and commits nothing. §7 states this as a branch table rather than a
prediction.

**Alternatives discarded.** A `kneeler` carrying `SupportsRest` — rejected
because `SupportsRest` gates nothing (§6a), so the kind would have advertised a
capability rather than granting one, and the "a shrine offers nowhere to rest"
justification an earlier draft of this spec gave for it was false. A wilderness
rest or heat object — a larger gap (`MAP-wilderness-affords-no-rest`) but it
redraws every outdoor interior in every world, a content change too large to
ride on a refactor. A test-only kind — rejected under 0398.

**The blast radius is measured, not predicted.** The task that adds the brazier
reports the count of shrine chambers at the three census seeds *before* it adds
the pattern, and the artifact diff *after*, under §7's branch table. If the
measurement shows shrines are unreachable at those seeds — the 0398 failure
repeating one level down, which is exactly what `needs_populous` turned out to
be — the brazier moves to a role that is reachable, and the measurement is the
finding either way. The 48-seed sweep already recorded in `pattern.rs` says
`Role::Loomroom` is 24 of 24, so this measurement is expected to CONFIRM rather
than discover — which is exactly why it must still be run. A measurement you
expect to pass is the one you are most likely to skip, and skipping the
equivalent check is what put the brazier in a shrine in the first draft.

## 6a. The space left for sleep quality

Nathan's ruling, 2026-09-01: *a creature should be able to pass out in the
middle of the road, but prefer a bed, or a fur, or bracken — whatever their
people tends to use. They should normally make fairly sane choices about this,
and a creature sleeping somewhere unsafe or unrestful is a useful indicator that
something needs tuning.*

This campaign builds **one of the three halves that ruling names**, and leaves
the other two a space that is not a placeholder but the campaign's own thesis.

```text
  object side      rest recovers more where the room affords more    BUILT (§6b)
  people side      what a people tends to sleep on                   kind-to-kind EDGE (addition 2)
  individual side  this one just likes a sleeping bag               Lineage-DERIVED (addition 3)
```

The object side is buildable now because `ObjectProperty` rows keyed by `KindId`
already exist and this campaign is what makes them reachable from a room. The
other two are additions two and three of `MAP-one-kind-model` arriving with a
named consumer, which is a stronger position than the orange left them in. After The Wicket a kind is a
`KindId` with open component tables behind it, so a future
`ComponentStore<KindId, RestQuality>` is a new table and nothing else: no enum,
no match, no dispatcher. Leaving room is therefore free; what is *not* free is
entrenching the wrong shape on the way past, and three things guard against
that.

**(1) Three doc comments are false and are corrected here.** They describe a
gate that does not exist, and they are precisely the sentences a later reader
reasons from — an earlier draft of this very spec justified a `kneeler` out of
them:

```text
  OfferedVerb::Sleep   "gates on SupportsRest"       Session::sleep never asks
  Action::Rest         "precondition: at home"       nothing enforces it
  affordance.rs        key and cave-mouth are the    Task 11 gave Key a variant
                       rows with no AnchorKind       four tasks later
```

What actually runs: `Session::sleep` (`session.rs:2536`) refuses a non-empty
argument, charges the clock, commits `rested`, sets `wake_at`. There is no bed
check and no home check. The creature layer agrees and says so —
`liveness.rs:2293`, the fatigue drive: *"sleeps where it is — its proposal is
always `Rest`."* `SupportsRest` reaches only the **advertisement** layer:
`required_properties(Sleep)` decides which objects list `sleep`, never whether
sleeping is allowed. The corrections say that, and name the grade/gate split as
the reason the property is misfiled rather than deleting it.

**(2) A regression test pins the ungated behaviour.** `sleep` succeeds in a room
with no rest-affording object in it. Cheap now, and it is the tripwire for the
specific future mistake this section exists to prevent: someone tidying the
inconsistency by making the verb honour its own comment, at which point a
magically-slept target walks off to find a bed. The two routes into sleep — the
voluntary act and an imposed effect — must not share a gate, because only one of
them is chosen.

**(3) No new `SupportsRest` carrier is added.** Adding one would deepen the
advertisement model on the eve of replacing it. This is why the proof kind is a
brazier rather than a kneeler; the property it carries, `RadiatesHeat`, gates a
verb that is genuinely enforced.

**What the later campaign inherits, so it need not re-derive it.** The grade is
relational twice over. Once in the sense decision 0347 already settled — an
affordance is a relation over (object, body, observer), *a supporter to a sprite
is not one to a giant*. And once in a sense 0347 did not need: **what a people
tends to sleep on**, which makes the preference a `(species KindId, thing
KindId)` pair. That is a kind-to-kind edge, and it is the first non-hypothetical
use case for MAP-one-kind-model's *second* addition, whose only prior example
was the orange tree. It also sharpens §8's sequencing argument: edges now have a
named consumer waiting on them.

The readout is captured separately (`PSY-rest-site-is-a-tuning-indicator`),
including the sign convention it needs settled before measurement — sleeping
rough is a defect only where a better site was **reachable**, so the metric is a
gap against the best site in range, never the absolute rung.

## 6b. Wait, rest and sleep are three acts, and today they are one and a half

Nathan's ruling, 2026-09-01: *waiting merely passes time and does not inherently
change anything about the character that would not be changed merely through the
passage of time. Resting should allow the character to remain conscious and
watchful but allow some benefits of increased healing, mana recovery, etc.
Sleeping should render the character unconscious and allow more dramatic
benefits, and is (for most species) mandatory.* Absorbed into this campaign
rather than sequenced after it, on his call, with a mid-campaign stage gate to
flush out unexpected effects.

### What is actually there

**`wait` is already correct and needs no work.** `Session::wait`
(`session.rs:6270`) parses a span, advances the clock, runs the NPC tick.
Nothing intrinsic to the body changes, and it has no `Action` variant at all —
which is the right shape for an act that transforms nothing.

**`rest` and `sleep` are one act with two words.** `Action::Rest` is the only
variant; its `concept_name()` is `"rest"`; the `sleep` verb routes to it and
bolts `wake_at` on afterwards, so unconsciousness is a property of the player
path rather than of the act. NPCs propose the same `Action::Rest` and never go
under by that route — their sleeping is the wake-gate, a separate process. The
language domain registers `"sleep"` and `"rest"` as two concepts
(`accession.rs`, `packs.rs`), so the vocabulary already draws a line the
simulation does not. Even the provenance collapses: `SLEPT_PROVENANCE` is
`"lay down and slept (fatigue eased)"` and its own comment says it is *"the same
register `liveness.rs` uses for a creature's own Rest"*.

**`rested` is a reset, not a recovery — and this is the load-bearing one.**

```rust
(FATIGUE_RISE * (t - last_rested).as_std_days()).clamp(0.0, 1.0)
```

Fatigue is time since the most recent `rested` fact, so *any* rest zeroes it.
There is no representation of how long a body slept, which means "rest gives
some benefit, sleep gives more" cannot be said at all: the only value in the
system is debt-cleared-to-zero. **The act split is cosmetic until this changes.**
Sleep debt has to become a stock paid down at a rate, not a flag cleared.

**The rate is the right number on the wrong clock.** `FATIGUE_RISE = 0.3` is
within rounding of Nathan's ~1/3, and `fatigue_at` multiplies it by
`.as_std_days()` — standard days, where the ruling says planetary. `LocalDays`
and `in_local(day_length)` have been in `domains/astronomy/src/units.rs` the
whole time. On any world whose rotation is not the standard day, sleep debt
accrues at the wrong rate today, and nothing notices because the constant is
authored and never compared against a local clock.

**And it is one constant for every species.** "Humans would likely have..."
names a per-species rate; "(for most species) mandatory" names species that opt
out entirely. A constant can express neither; a `ComponentStore<KindId, _>` row
expresses both, including the opt-out, as the absence of a row.

### What this campaign builds

1. **`Action::Sleep` beside `Action::Rest`.** `windows/vessel/src/action.rs`
   carries no `_` arm in any of its matches (verified), so the compiler
   enumerates every site the new variant must be classified at — `mood`,
   `precondition_reads_committed_state`, `concept_name`, and `all()` under
   `action_variants_must_all_be_rostered`. Unconsciousness moves off the player
   path and onto the act, so an NPC that sleeps goes under for the same reason a
   possessed body does.
2. **Recovery replaces reset.** Fatigue becomes a stock discharged by an amount,
   so `Rest` restores less than `Sleep` per unit time and neither zeroes the
   debt by fiat. The rung for healing and mana is left named and unbuilt (§6b's
   closing note).
3. **The rate becomes a row and the clock becomes local.** `FATIGUE_RISE` moves
   to a per-species component; `as_std_days()` becomes the planetary-day
   conversion `units.rs` already provides. A species with no row does not sleep.
4. **Rest effectiveness reads the affordances present** — the first real
   consumer of §6a's grade, and the reason the grade is designed there rather
   than deferred wholesale.

### What it does not build

No health, no mana. One mention of mana exists in the workspace
(`kernel/src/ecology.rs`, naming it as an example of an ambient undepleted
resource), and no health, wound or vitality state exists on a body at all. So
"more dramatic benefits" has nothing to attach to yet, and inventing a health
system to justify the split would be the tail wagging the dog. What ships is the
**shape**: two acts with different recovery magnitudes, so that when a
restorable stock arrives it plugs into a model that already distinguishes them.
Fatigue and consciousness are the two stocks that exist, and both are observable
today — fatigue drives NPC behaviour, consciousness is player-visible — so the
split clears decision 0398's reachability bar on its own.

### The cost, stated plainly

Fatigue folds over committed facts, so changing the fold changes when creatures
rest, which changes committed history: same seed, different world. That is not a
determinism violation — the standard is commit-to-commit — but it is a full
golden and census refresh, and every census column touching creature behaviour
will move. Nathan ruled this acceptable ("pre-alpha, golden and census refreshes
are the norm") and asked for a **stage gate on the sluice at this boundary** to
surface unexpected effects before the campaign goes further. `RESTED` is a
vessel-local predicate registered per session (`session.rs:1197`), not a domain
predicate, so a second predicate is expected not to move the world-level concept
dump — expected, and read off the branch table rather than assumed.

## 7. Artifacts and determinism

- §2.1's verified argument says the re-key alone moves no bytes. Task 2's
  acceptance criterion is a `git diff --exit-code` over the declared generated
  paths coming back **empty** after `make rebaseline`, with the branch table
  written out rather than the expectation: *nothing moved → proceed;
  `docs/audits/` alone moved → the type-audit report drifted on a pub-boundary
  change, regenerate and commit in the same commit; anything under
  `book/src/gallery/` or a census CSV moved → STOP, the re-key is not
  behaviour-preserving and that is a finding.*
- The brazier's artifact surface is **measured at Task 4, not predicted**, and
  the branch table is the deliverable: *nothing moved → proceed and say so;
  `clients/game/core/tests/fixtures/` moved → the chamber-band session snapshot
  picked up the new anchor, refresh and commit in the same commit; a census CSV
  or `book/src/domesday/` moved → STOP, something that commits now reads a
  chamber, `INVENTORY`'s latency rule has lapsed and the append was an
  undeclared epoch — that is a finding and the campaign's headline, not a
  rebaseline.*
- No save-format contract changes. `AnchorKind` derives no `Serialize` or
  `Deserialize` (checked on the type, not inferred from a doc comment), so no
  variant ever reaches a byte. The `threshold`/`floor`/`wall`/`furnishing`
  strings in the committed chamber fixture are `lattice::render` **plan mark**
  kinds, not `AnchorKind` values, and are unaffected by the re-key. No stream
  label changes; no seed-derivation change; the world file is untouched.
- The clients are unaffected: `clients/game/core` draws one `FURNISHING_GLYPH`
  for every furnishing kind by deliberate design (`CLIENT-glyphs-22-rejected`),
  so widening the vocabulary is invisible across the wasm ABI.

## 8. What an ideonomy pass changed

Operators: abstraction-lift → cross-domain re-instantiation. Prompts: purpose,
symmetry, materiality.

**Lifted form.** *An open commons reachable only through a closed admission
list, sized for an earlier and smaller world.* A permit regime, not a capacity
limit.

**Drug scheduling.** A controlled-substance schedule is a closed enumeration,
and a compound not on it does not legally exist; the analogue problem forced a
*structural* rule — substantial similarity — to replace the enumeration. Carried
back: without inheritance, every new kind must author every row by hand, so
kind-to-kind edges are the *rescue* for an open vocabulary, not merely the next
item on a list. This is a genuine argument for bundling addition two into this
campaign, and it loses on scale: 15 kinds do not make hand-authoring expensive,
and the essay's order-of-least-regret argument (three working mechanisms
risked for two missing ones) still governs. It is a reason to sequence edges
next, and it is recorded as such.

**Immunology.** Germline-encoded pattern recognition is closed and small;
adaptive immunity is open and enormous, and survivable only because of thymic
negative selection. Carried back: an open vocabulary is safe exactly to the
degree its default-deny gate is real — §5, and the reason it is the deliverable
rather than a supporting task.

**Faceted classification.** Ranganathan composes a subject from facets where
Dewey locates it in an enumeration, and the faceted catalogue still needs a
caption per composed class and still has to shelve the book. Carried back: the
prose row is the caption (§4.4), and an open vocabulary changes nothing
observable unless something can actually draw the new kinds — §6.

**Purpose (the overturn).** `AnchorKind` carries two inherited purposes and only
one is load-bearing: it is the grammar's vocabulary, and it is the guarantee
that every placeable kind is total in prose and thing-kind. Closedness serves
the second and is a poor instrument for it at any scale. So the campaign's
deliverable is the totality mechanism, and deleting the enum is the consequence.
This reframing decided §5.

**Materiality.** The enum conflates a region (`Ground`, `Alcove`), a graph seam
(`Threshold`) and an object (`Key`, `Altar`). Under composition that conflation
dissolves for free — a kind is a region exactly by carrying no `ObjectTraits`
row, which is already true of `screen`, `ground` and `log`. No `Role` field is
added to `Pattern`; this paragraph exists so nobody adds one later thinking it
was overlooked.

## 9. Risks

- **A stringly-typed predicate.** Mitigated by §4.2's named constants plus G-d.
  The residual risk is a `KindId("...")` literal written inline in production
  code; a grep-based check for `KindId(` outside the roster module is cheap and
  is a task decision, not a spec mandate.
- **Prose lookup cost.** Fifteen short-string comparisons per anchor rather than
  enum discriminants, on a per-chamber derivation. Expected negligible; the
  duration baseline is the instrument and a regression there is the signal.
- **The totality gate is weaker than the compiler in one direction.** The
  compiler refused to build; a test refuses to pass. That is a real reduction
  and it is the price of the openness, which is why G-a through G-f are
  specified by their mutations rather than by their existence.
- **Scope creep into edges.** §3 and §8 both name it. The registry row is
  amended at close rather than the campaign widening.
- **The one thing the compiler still catches.** `INVENTORY` is a fixed-size
  array, so appending a pattern forces its length to change and cannot be done
  silently. That protection is unaffected by this campaign and is worth naming,
  because the rest of it argues for replacing compiler guarantees with tests and
  a reader could reasonably conclude none survive.

## 10. Stages

1. **Handles and roster.** `domains/thing` publishes named `KindId` constants;
   G-d and G-f land against the existing roster. No vessel change. Green on its
   own.
2. **The re-key.** `Anchor`, `Interior`, `Pattern`, `Attach`, the four interior
   predicates, `affordance`; `anchor_kinds!` and `thing_kind_of` deleted; vessel
   gains the `hornvale-thing` dependency. G-a lands. **Acceptance: the generated
   artifacts do not move** (§7's branch table).
3. **Prose as a table.** `chamber_prose` becomes `ComponentStore<KindId,
   ChamberProse>`; G-b, G-c and the refusal land. The last exhaustive match is
   gone.
4. **The brazier**, and the three comment corrections and the ungated-sleep
   regression test that go with it (§6a). One kind, five data rows, one appended
   `Pattern`, no control flow. Loomroom reachability measured before, artifact
   diff read after under §7's branch table.
   **Stage gate on the sluice here** — the last boundary at which the tree is
   still artifact-clean, so anything the queue reddens is attributable to the
   re-key alone.
5. **The acts** (§6b): `Action::Sleep`; recovery replaces reset; the rate becomes
   a per-species row on the planetary clock; rest reads the affordances present.
   This is where committed history moves.
   **Stage gate on the sluice here too**, on Nathan's instruction — this is the
   boundary that changes world output, and the queue is what surfaces the
   unexpected half.
6. **DoD.** §11, including the golden and census refresh.

## 11. Definition of done

- Chronicle entry (`book/src/chronicle/the-wicket.md`) and a freshness sweep.
- **The essay and the registry row are corrected** — the thirty-four/thirty-six/
  seventeen sentence is replaced with the measured numbers and a note that it
  was wrong at authoring, not stale. This is the loud-correction discipline: a
  published measured fact that is wrong produces wrong cost estimates from
  readers acting in good faith.
- `MAP-one-kind-model` moves to reflect that addition one has shipped, with
  addition two (edges) carrying §8's inheritance argument.
- Four idea-registry rows are already written rather than deferred to close:
  `PSY-rest-quality-is-a-grade-not-a-gate`,
  `PSY-rest-site-is-a-tuning-indicator`,
  `MAP-wilderness-affords-no-rest` (the discarded alternative from §6), and
  `TOOL-authored-scalar-should-be-a-component` (§6b's generalisation, and the
  sweep it implies).
- Retrospective (`docs/retrospectives/the-wicket.md`).
- Decision records for: the totality-by-registry rule (§5, and the direction
  each check enforces); the constant-is-a-convenience/variant-is-mandatory
  asymmetry (§4.2); and **sleep is never gated — the place grades it** (§6a),
  which is Nathan's ruling and binds every future campaign that touches rest,
  the sleep spell, or the property vocabulary. All three bind future campaigns
  and so need records rather than ledger entries.
- Census refreshed once at pre-merge close on lefford; artifacts regenerated;
  merged through the sluice.
