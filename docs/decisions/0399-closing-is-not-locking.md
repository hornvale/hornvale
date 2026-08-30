# 0399. Closing is not locking — a lid and a lock are different states

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (ruled directly on a
soft-lock a reviewer reached in three moves; four remedies were put to him and
he rejected all four for a model correction, so not an autopilot gate) ·
**Amends:** [0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md)
and Task 11's `open`/`close`, which shipped one boolean doing two jobs ·
**Relates:** [0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md)
(the `openness` fold this leaves untouched),
[0367](0367-the-latch-is-monotone.md) (the monotone state 0396 retired, and
the shape a lock genuinely is),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md)
(how the split is evidenced) ·
[The Chattel](../superpowers/plans/2026-08-28-the-chattel.md)

In the context of a permanent, unrecoverable soft-lock reachable with three of
The Chattel's own verbs, we decided that **closed and locked are separate
states of a thing, `close` shuts a lid and never turns a key, and a lock is
turned only by an `open` that had the key in hand** — accepting that no verb
in this campaign can ever re-lock anything, because locking requires the key
in the lock and "in the lock" is a location no verb can reach.

## Context

`open`/`close` shipped with `openness` as a container's only state. The lock
was not a state at all: it was re-derived from the PLAYER'S POCKETS at the
moment of asking — `carries(kind, Lockable) && !carrying_something_that(
Portable)` — so a chest was locked whenever the body happened not to be
holding something portable, whatever the chest's own history. Shutting a lid
therefore re-locked it. Measured through the shipped CLI on seed 1, in the
storeroom four chambers in:

```text
> take a key                -> You take the key.
> open a strongbox          -> You open the strongbox. Within it: a key.
> put a key in a strongbox  -> You put the key in the strongbox.
> close a strongbox         -> You close the strongbox.
> open a strongbox          -> It is locked, and you are carrying nothing that would open it.
> take a key                -> The key is shut away in something closed.
```

Unrecoverable at any day, by any verb. The key is behind a lid that will not
lift without the key. Four remedies were put to Nathan — refuse the `close`
when the container holds the only opener; refuse the `put` instead; let `take`
reach through a shut lid for a thing that opens it; leave it and document it —
and he rejected all four:

> "You can close it, but you shouldn't be able to lock it without the key in
> the lock. And if the key is in the lock, it's not in the container."

Every rejected remedy was a special case bolted onto a wrong model. The ruling
is that the model is wrong: **one variable was carrying two states, and the
dead end is exactly where they disagreed.**

## The rule

**1. `openness` is untouched.** It stays Task 5's open/shut fold — the same
predicate, the same absent-fact default, the same reader
(`Session::container_is_open`), the same passage angle 0396 folded through it.
Nothing about doors, cave mouths or lids changes.

**2. Lockedness is its own predicate.** `thing::LOCKEDNESS` ("lockedness"),
non-functional, per-session-registered beside `LOCATED_IN` and `OPENNESS`,
read by `thing::is_locked` as an `Option<bool>` on exactly `is_open`'s terms.
Absence means "whatever the seed drew", and the authored default lives in the
reader that knows the kind: `Session::container_is_locked` answers **locked**
for an `ObjectProperty::Lockable` kind with no fact, and **never locked** for
a kind with no lock at all. That default is what makes a seeded strongbox
worth finding a key for, and it is the state every world starts in.

**3. `close` shuts and never locks.** The lock is consulted on the `open`
branch only — `let locked = open && self.container_is_locked(…)` — so the
conflation is not merely fixed but unrepresentable: `close` has no expression
to reach it through. The sequence above now ends with a shut, unlocked chest,
`open` succeeds with empty hands, and the key comes back out.

**4. Nothing re-locks, and the reason is clause 4 of the ruling rather than an
omission.** Locking needs the key IN THE LOCK — a location distinct from both
"in the container" and "in a hand" — and no verb in this campaign puts a key
there. So `LOCKEDNESS`'s `true` direction has no production writer. The
writer exists (`thing::set_lockedness` takes a `bool`) so that a future `lock`
verb has one function to reach for rather than a second predicate; the
constraint it inherits is that it must first model a key in a lock.

**5. THE DEAD END DISAPPEARS WITH NO SPECIAL CASE, AND THAT IS THE TEST OF THE
MODEL.** No precondition anywhere reads a container's contents. No verb asks
"is the only thing that opens me inside me". Every one of the four rejected
remedies needed such a question, and each would have been a rule with one
instance — the shape that is correct until the second lockable kind, or the
second opener, or a key a player deliberately locks away. A model correction
costs a predicate and removes the question.

## Consequences

- **The split is evidenced by two mutations, taken separately because they
  hold different halves.** `closing_a_container_does_not_lock_it` fails when
  the pre-0399 expression is put back
  (`let locked = open && carries(thing_kind, Lockable)`), which compiles and
  leaves every first-open test green — the whole reason the defect shipped:
  `864 tests run: 863 passed, 1 failed`. `a_container_nobody_unlocked_is_
  still_locked_after_a_close` fails when the unlock is hoisted above the
  refusal, so a refused `open` turns the lock on its way to saying it did
  not — `864 tests run: 863 passed, 1 failed`, red on its third assertion.
  A blunter mutation (`container_is_locked`'s `.unwrap_or(true)` →
  `.unwrap_or(false)`) reds three tests and all three object at "a seeded
  strongbox starts locked", which was already held; it proves the default,
  not the split, and is recorded in the test as evidence for the wrong claim.

- **No committed artifact moves, and the reason is structural rather than
  lucky.** `LOCKEDNESS` is registered per-session in `Session::start`, never
  at genesis, so `world-seed-42.json` does not carry it and no
  concept-registry dump changes. The only artifacts that could move are ones
  produced by a walk that types `open` — and no gallery script does:
  `scripts/possession-walk.txt` and `scripts/possession-over-time-walk.txt`
  are the only inputs `book/src/gallery/possession-*.md` is generated from,
  and neither types `open`, `close`, `take`, `drop`, `put` or `carrying` at
  all. That absence was already load-bearing before this record and was
  believed to be the opposite: `Session::take`'s own doc deferred the
  `open`/`close` charging defect on the grounds that "their transcripts are
  in the galleries." They are not, which is how the defect survived a task
  boundary.

- **A saved world written before this record loads unchanged.** A new
  per-session predicate is additive: `register_predicate` is idempotent for
  an identical definition and an old world's registry simply lacks this one
  until a session adds it. Every strongbox in such a world reads its authored
  default — locked — which is the state it was already in. Contrast 0189 and
  0396, which took deliberate breaks; this one does not need to.

- **`LOCKED_WITHOUT_A_KEY_REFUSAL`'s hazard is unchanged and unmoved.** The
  lock still wants `ObjectProperty::Portable` as a literal at one call site,
  `Lockable` still carries no payload naming its opener, and
  `the_lock_wants_a_property_and_exactly_one_kind_supplies_it` still pins the
  roster at `["key"]`. This record separates WHEN the lock is consulted from
  WHAT it wants; the second question is still open and still that test's.
