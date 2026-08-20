# The Tackle

Tackle is the gear you fit before anything is driven — the harness hardware
that goes on the animal while it is still standing still. The Bridle, the
program this campaign opens, is about one action system serving player and
creature alike, with the controller as a parameter and mind control as a
consequence. None of that is here. What is here is the rigging: an action
layer with its own module, a word returned to its rightful owner, and one
shared derivation of what a body weighs. Every committed artifact in the
repository is byte-identical afterwards.

## Why a campaign that changes nothing was worth merging alone

The Bridle's first arc was originally one piece of work. Splitting it was a
judgment about the *kind* of risk each half carries rather than the amount.

The second half — The Deed — unifies the player's verbs with the creature's,
introduces the in-character/out-of-character distinction, and starts charging
the player time for what they do. Its acceptance criterion is necessarily that
artifacts **move**, and that the diff is readable as an intended consequence.
The first half is a mechanical relocation and a 22-site rename whose acceptance
criterion is the exact opposite: that nothing moves at all.

Run together, those two criteria destroy each other. The artifact drift that
The Deed is supposed to produce would arrive tangled with several hundred lines
of pure relocation and a rename touching every drive in the file, and no
reviewer — human or otherwise — could say which change moved which byte. The
byte-identity claim is only worth making when it is the *only* claim on the
table. So the campaign that asserts "nothing moved" merges on its own, and the
campaign that asserts "this moved, for this reason" inherits a clean baseline
to say it against.

## An action layer at 2% of the file it lived in

`windows/vessel/src/liveness.rs` was 14,768 lines. Inside it, undifferentiated
from the occupancy bookkeeping and the drive arbitration around them, sat the
pieces that describe what a body can *do*: the `Action` enum and its concept
roster, the plan state, the two search spaces (`GoapSpace` for goal-oriented
action planning, `NavSpace` for navigation), the two planners over them, and
the remembered-danger edge cost.

Extracted whole into `action.rs`, that layer is **352 lines** — a little under
2.4% of the file it was buried in. The number is the argument. The next arc's
entire semantic content is a rewrite of those 352 lines, and performing it in
situ means every reading, every review, and every conflict resolution pays the
full 14,768-line tax for a change that touches a fortieth of the file. The
extraction is not a tidying preference; it is the difference between the
semantic work being visible and the semantic work being lost.

The relocation is pure. `Occupancy`, `Drive`, and the drive implementations
stay behind, because they answer a different question — not *what can be done*
but *which of the doable things does this creature want*. That boundary is the
same one the next arc will cut along when it inserts a controller above the
drives, so drawing it now costs nothing and saves the cut later.

## Two arrows, one word

`Drive::affordance` was the method every drive implemented to answer "what do I
want to do next". The name is wrong, and it is wrong in a way that only became
expensive once a later arc needed the word for its actual meaning.

The distinction is one of **arrow direction**. `Drive::affordance` points
actor → action: a thermal drive is asked what it proposes, and it answers with
a candidate. Gibson's *affordance*, the term of art the word belongs to, points
object → actor: a ledge affords sitting, a door affords opening, and the object
advertises this to whatever is capable of perceiving it. These are not two
shades of one concept. They are inverse relations over the same pair of types,
which is precisely the configuration where a shared name stops being ambiguous
and starts being actively misleading — a reader who knows one sense will
confidently misread the other.

Arc IV needs Gibson's sense, and Gibson has the stronger claim. So the method
became `Drive::proposal`, which says what it does: a drive *proposes*, and
something above it disposes. One trait declaration, six implementations,
twenty-two call sites, eight test function names, and every doc comment that
referenced it — all inside the one file. Doing it now, while only one sense is
live, is a mechanical sweep. Doing it in Arc IV, with both senses in active use
on both sides of the same module boundary, would mean renaming under conditions
where every occurrence has to be individually adjudicated.

Counting the implementations turned out to be the hard part, and it is
recorded in the retrospective rather than here: the obvious search returned
three where the truth was six.

## The tariff the body pays and the driver does not

The keystone of the whole Bridle program is a statement about records: when an
aboleth drives a fisherman, the world's ledger must be indistinguishable from
the fisherman acting alone. If it is not — if a dominated body moves at a
different speed, or leaves a differently-shaped trace — then domination is
detectable as a physics anomaly, and the entire dramatic proposition collapses
into a debug flag with a costume on.

That was stated as a design goal to be built toward. It was already true.

`windows/vessel/src/clock.rs` charges time for an action through

```rust
pub fn cost_ticks(action: &Action, mass_kg: f64, terrain_factor: f64) -> Ticks
```

and the signature settles the question by omission. There is **no driver
parameter**. There is no place to put one. Time is charged as a function of the
act, the mass of the thing performing it, and the ground it is performed on —
and every one of those three is a property of the *body*. Whatever is deciding
what the body does cannot reach this function, and so cannot influence what the
decision costs. The architecture had agreed with the program's keystone before
anyone stated the keystone, and the campaign's contribution was to notice.

What was genuinely missing was smaller and more mundane: a possessed body had
no mass at all. The creature layer derived one inline — twice, in two
byte-identical copies, one for domestic and one for wild creatures — while the
player had nothing, because no player had ever been routed through the clock.
The repair is a single shared `mass_for_species`, called by both former copies
and available to the player when The Deed connects it, falling back to a
reference mass when the biosphere is absent or the species unknown. Creature
masses are unmoved; the tests now assert the fallback that was previously only
implied.

This is the campaign's one piece of real construction, and it is deliberately
small. The point is not that the player now has a mass — the player still has
no route to the clock. The point is that when that route is built, it will
provably terminate at the same derivation a creature uses, rather than at a
parallel one that is equal today and drifts next year.

## What byte-identity did and did not prove

The acceptance criterion was that a full artifact regeneration moves nothing,
and it held: `drift-exit=0` across every path in the generated-paths list, at
every task boundary and at close.

A check that passes trivially is worth nothing, so the criterion was tested
against a deliberate perturbation before it was trusted. Raising the reference
mass from 70.0 kg to 71.0 kg reddens the check, which establishes that a live
path exists from vessel source through the tempo calculation into the emitted
day timestamps of a committed transcript. That is the fact the control proves.
It is also the *only* fact the control proves, and the honest statement of its
strength is thin: one file, two lines, at the fifth decimal place. The day-zero
possession transcript did not move at all.

So byte-identity here is a tripwire for one class of accident — the kind that
changes an emitted number — and not a proof that nothing behavioural changed.
The behavioural weight rests where it always did, on the vessel crate's own
test suite. The distinction is written down because the plan originally implied
otherwise, and a necessary condition read as a sufficient one is how a green
check comes to mean less than its reader thinks.

The one generated artifact that did move is instructive in the same direction.
`docs/audits/type-audit-report.md` carries aggregate counts — by verdict class,
by crate, by pending wave — and no per-symbol rows at all. For a pure rename or
a pure relocation it therefore must **not** move, and it did not: the extraction
lifted a dozen public items into a new module without shifting a byte of it, and
the rename likewise. It moved exactly once, by exactly two in the vessel crate,
in the one commit that introduced two new public-boundary primitives. Read this
way the report is not a signature check but a **tag-conservation** check, whose
silence is the signal and whose movement during a rename would mean a verdict
tag had been dropped or duplicated.

## What is not done

Nothing here is playable and nothing here is player-facing. The player still
does not act in character, still is not charged time, still leaves no record.
There is no in-character/out-of-character distinction, no daybook, no gate on
body state, and no controller stack — those are The Deed and the arcs after it.
`Action::Rest` still models a multi-day sleep as a single tick that teleports
the clock forward, which means the codebase contains one long action and no
concept of one, and therefore no act that can be interrupted; mind control will
force that question and this campaign does not answer it. The action layer has a
module of its own, a name that means what it says, and one definition of what a
body weighs. That is harness, fitted while the animal is standing still.
