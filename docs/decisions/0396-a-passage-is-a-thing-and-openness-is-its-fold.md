# 0396. A passage is a thing, and `openness` is its fold — the monotone latch retires

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Supersedes:** [0367](0367-the-latch-is-monotone.md) (the latch is monotone)
· **Relates:** [0366](0366-passage-state-is-derived-never-stored.md) (the fold
this keeps whole), [0368](0368-a-played-world-is-a-fork.md) (why a session
fact reaches disk at all),
[0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md) (the
precedent for a deliberate save-format break) ·
[The Chattel](../superpowers/plans/2026-08-28-the-chattel.md)

In the context of containers, portables and passages all becoming visible in
one campaign, we decided that **a cave mouth is a thing of kind `cave-mouth`
whose openness is an ordinary `openness` fact, and the `passage-cleared`
predicate is retired**, accepting that a world saved before this flip loses
whatever passages it had recorded as cleared.

## Context

0367 deferred a closing act and said exactly why:

> "Re-closing is not hard to write; it is hard to write *once*. A closing act
> belongs with doors, lids, and containers, because they are the same
> mechanism seen from three angles, and a `passage-closed` predicate shipped
> now would be designed against one of the three."

Arc IV.c is the first campaign in which all three angles are visible, so this
supersedes 0367 with the thing 0367 asked for rather than extending it. The
mechanism that arrived is not a `passage-closed` predicate at all — it is
`openness`, the same non-functional, append-only predicate a strongbox
already uses, folded as-of-day by `thing::is_open`. A passage that needed its
own predicate was a passage that had not joined the object model.

## The rule

**1. What folds when an open and a close fact both exist: LEDGER ORDER, latest
posting wins.** `effective_state` reads `thing::is_open`, which is
`thing::latest_object_at_or_before` — the last `openness` fact dated at or
before the day asked about, ties at one instant broken by commit order. 0367
forecast this and named the precedent to follow ("almost certainly
ledger-order, the way The Coercion's `possessed-by`/`possession-ended` pair
already resolves"), and that is the rule taken. Nothing in the fold consults
`BarrierState`'s derived `Ord` any more.

The shape spec §3.7 pins is unchanged:

```
  effective_state(thing, day) =
      Open                     if the latest openness fact at-or-before day is true
      barrier_of(seed, addr)   if it is false, or if there is none
```

**The asymmetry in the second line is deliberate and is the half most likely
to be misread.** A close does not invent a barrier; it withdraws an opening.
Closing a mouth the seed drew as `Open` still reads `Open`, because the
fallback is `barrier_of` and not a hardcoded `Sealed`. A trap needs a seeded
barrier to fall back *to*. Both directions are pinned
(`a_cave_mouth_closed_after_it_was_opened_bars_again`,
`a_closed_cave_mouth_falls_back_to_the_barrier_the_seed_drew`).

**2. A monotone latch could not express a trap, and this can.** 0367's own
closing consequence — *"A passage that closes behind you is a real design
object this forecloses for now, and it is worth naming because it is the
first thing anyone will want"* — is discharged rather than restated: a `Thin`
mouth opened on day 3 and closed on day 6 is `Open` at day 4 and `Thin` again
at day 7. **What is discharged is EXPRESSIBILITY, not a shipped trap.**
Nothing in the verb surface closes a passage yet; `open`/`close` are a later
task, and until they land the only writer is `clear`, which only ever opens.
The distinction matters because "the trap is possible" and "the trap exists"
are different claims and only the first is made here.

**3. The save-format break was MEASURED, not estimated.**
`grep -rl 'passage-cleared' --include='*.json' .` returns nothing (exit 1) —
run with a positive control, the same command shape against `supports-rest`,
which correctly finds `cli/tests/fixtures/world-seed-42.json`, so the null is
a null and not a broken instrument. No committed fixture carries the
predicate, so the break reaches only hand-made `possess --out` saves written
since The Latch landed, and those worlds still LOAD — they simply carry facts
nothing reads, so a passage one recorded as cleared is barred again. 0189 is
the precedent for taking a break like this deliberately; worlds are
version-locked (0099), so a stale key cannot corrupt a world that still
loads.

## Consequences

- **The address string got a wider contract, not a narrower one, and this is
  the trap for a future reader.** `passage::addr_key` used to be the
  `Value::Text` object of a fact: a changed spelling made one fold miss its
  facts. It is now the address leg of `passage::cave_mouth_role`, which is a
  `Lineage` role, which is an input to a derived `EntityId` — so a changed
  spelling renumbers every cave mouth in every saved world and orphans every
  fact of every predicate about it at once. The retiring predicate's guard
  (`addr_key_spelling_is_the_permanent_on_disk_key`) was REPLACED by a
  strictly wider one on the successor key
  (`the_cave_mouth_role_spelling_is_the_permanent_lineage_key`), whose literal
  contains the old one. Deleting a guard along with the thing it guarded was
  the cheapest repair available here and would have been the wrong one.
- **The clearing BODY is no longer recorded.** The Latch's fact had the
  clearing body as its subject and the address as its object; the subject is
  now the cave mouth itself. That is a real loss of information and it is
  deliberate: 0366's fold never consulted the subject ("any body's clearing
  fact opens the passage for everyone"), so nothing *interpreted* the field
  and no assertion in the tree *held* it, and a predicate about a thing whose
  subject is a *different* thing is exactly what joining the object model
  removes. Whoever wants "who opened this" back wants an agentive predicate,
  not this one's subject slot.
  - **This paragraph said "the field was write-only" when it was ratified, and
    that was stronger than the evidence** (fix round 1, m1). The subject was
    unreadable by nothing: `windows/historiography`'s `recount` iterates
    `Ledger::facts_about(entity)` with NO predicate filter — domain-agnostic
    by construction, which `windows/CLAUDE.md` states as a feature — and
    `cli/src/repl.rs`'s `why <id>` calls it, so a pre-flip `possess --out`
    world reloaded into `repl` rendered the clearing under `passage-cleared`'s
    registered doc, against the clearing body. The correction is to the
    premise only; the conclusion above does not move, and "nothing interpreted
    it and no assertion held it" is the true and sufficient statement of what
    is being given up.
- **`cave-mouth` now carries `AffordsPassage` as well as `Openable`** (spec
  §3.7). Chamber entry is gated on the cave mouth's own fold, which is the
  routing the property was waiting for — but no production caller holds a
  cave-mouth `KindId` yet, because every live call site of
  `offered_by`/`offered_to`/`offered_to_observer` still converts from an
  `AnchorKind`. Task 9's re-key of `offered_to_observer` to `KindId` is that
  caller. The property is granted here because §3.7 assigns it here and
  because the thing it describes exists as of this task; the one-task gap is
  recorded rather than hidden.
- **The predicate count on a played world goes down by one.**
  `Session::start` no longer registers `passage-cleared`, so a `--out` world
  written after this flip does not carry it in its registry. Genesis is
  untouched either way: neither predicate was ever registered there, and
  `world-seed-42.json` does not move.
