# 0089. An epoch freezes when a world saved from `main` can carry it

**Status:** Accepted (2026-07-29) · **Decider:** Nathan · **Refines:**
[0006](0006-seed-labels-are-permanent-contracts.md),
[0073](0073-epoch-granularity-is-declared.md)

In the context of a staged campaign that changes what one seed-derivation leg
consumes more than once before it merges, facing decision 0006's silence on when
an epoch suffix stops being available to ride, we decided that **an epoch is
owed per *released* consumption contract, not per code change: a `/vN` leg is
frozen from the moment a world saved off `main` can carry `vN` in its
`derived_under` stamp, and every consumption change after that owes `/vN+1`** —
accepting that "released" is a property of `main` alone, so the question is
answered by reading `origin/main`, never a branch's own history.

**Context.** 0006 makes a seed-derivation label a permanent save-format contract
and requires that deliberate regeneration use an epoch suffix, never a rename.
0073 fixes a label's *granularity* at declaration. 0084 fixes when an epoch is
*owed* (only when a committed derivation actually moved). None of the three says
when an already-minted epoch stops being rideable — and that is the question a
staged campaign hits, because a campaign that mints `/vN` in task 6 and changes
consumption again in task 7 must decide whether task 7 owes `/vN+1`.

The answer follows from what an epoch boundary is *for*. A boundary between
`/v2` and `/v3` is a claim that some save held `/v2` and a later save held `/v3`.
Minting `/v4` for a `/v3` that no save ever held writes a discontinuity into the
manifest that never occurred — the same defect 0084 named as an **empty epoch**,
arriving from the other direction. Conversely, once a save *can* hold `/vN`, the
next consumption change genuinely separates two formats that both exist in the
world, and it owes its own suffix.

**The test, and it is mechanical.** `cli::streams::versioned_labels()` is "the
stamp a world carries", and `cli::streams::stamp` writes it into
`World::derived_under` at save time. So the freeze condition is checkable by
building one world:

```
$ hornvale new --seed 42 --out w.json     # on origin/main
$ jq .derived_under w.json
  "language/<species>/name/settlement": "v3",   # ← frozen
```

Measured on `origin/main` at `0b65be20`, that stamp reads `v3` for all three
naming stems. `/v3` is therefore released, and the next campaign that changes
what `Namer::glossed_name` consumes owes `/v4`.

**The concrete case, both halves of it.** *The Wearing* (merged `0b65be20`)
exercised this gap twice in the same campaign, once in each direction.

1. **`/v3` was ridden, correctly.** Task 6 minted
   `language/<species>/name/{settlement,deity,epithet}/v3`. Task 7 then changed
   consumption again (a drawn `NameShape` replacing a conditional
   `range_u32(1, 2)`), and Task 8 changed the phonotactics leg underneath.
   Neither minted a new epoch. Verified rather than assumed: the merge commit's
   `main`-side parent `38d6c3ed` has **zero** occurrences of
   `name/settlement/v3` in `domains/language/src/lib.rs`, against two on the
   branch side. The epoch was minted, consumed three ways and merged without any
   world outside the branch ever holding it, which is exactly the case in which
   one suffix is the honest count.
2. **`ROOT_EPOCH` v3→v4 was minted and then withdrawn.** `d028ebac` (2026-07-27)
   bumped it to legalise re-founding the accession cohort baseline;
   `784c2cb6` (2026-07-29) put it back to `v3` after the premise was measured and
   found false, and after the record's own reasoning established that the
   campaign had changed *the phonology the assignment algorithm draws from*, not
   the algorithm — which reseeds every root at any label and has never owed a
   bump. The withdrawal cost a regeneration and nothing else, **because the same
   unreleased property that made `/v3` rideable made `/v4` retractable.** After
   the freeze, neither move is available: an epoch on `main` can only be
   superseded, never ridden and never taken back.

**A correction to the rule as first stated.** Task 7's report wrote the freeze
condition as "the first thing that lands on `main` with `/v3` regenerated
fixtures freezes it". The fixture clause is wrong, and this campaign is what
proved it wrong: The Wearing landed the `/v3` *code* while deliberately deferring
its census regeneration (followup F11), so `main` today generates `/v3` worlds
against a census golden that knows nothing about them. The two can separate, and
when they do it is the code that decides. Fixtures are evidence *about* worlds;
`derived_under` is what a world *is*. A rule keyed to the fixture would have left
`/v3` nominally rideable by the next campaign while real saves already carried
it — the corrupting move 0006 exists to prevent.

**Consequences.**

- **The question is asked of `origin/main`, not of the branch.** A campaign
  riding an epoch must show the label is absent from `main`, and show it by
  reading `main` (a `git show origin/main:<file> | grep` is sufficient and is
  what the case above did), not by remembering when the branch minted it.
- **An unreleased epoch absorbs any number of consumption changes.** They land
  together, they regenerate together, and one suffix is the truthful count.
  Riding is not a discount; it is the accurate number.
- **Absorbing `main` does not release a branch's epoch,** and merging the branch
  does. This matters for campaigns that absorb repeatedly: the freeze is a
  property of what is *on* `main`, so it is only the branch's own merge that can
  trip it.
- **Where the reasoning is written is at the draw site,** not only in a task
  report. `Namer::glossed_name`'s doc comment names all three changes `/v3`
  covers and why they share one suffix; `draw_phonotactics` carries the parallel
  note for the unsuffixed leg. This record now carries the *expiry*, which a doc
  comment stating only the application cannot — a later reader of
  `glossed_name` alone would find "`/v3` has never been in a released world" and
  no statement of when that stopped being true.

**What this does not license.** Riding an unreleased epoch is never a substitute
for declaring one. 0084's **UNDECLARED** — a derivation that moved with no bump —
remains the one unforgivable outcome, and it is unaffected: the rule here only
answers *which* suffix a change owes once it is established that it owes one.
Nor does it license minting an epoch early to have something to ride: 0083
already forbids declaring a label in advance of the algorithm that draws from it.

**See also.** [0006](0006-seed-labels-are-permanent-contracts.md) (the contract
this refines); [0073](0073-epoch-granularity-is-declared.md) (granularity fixed
at declaration); [0083](0083-a-label-per-algorithm-and-never-in-advance.md) (a
label is declared only for an algorithm that exists);
[0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md) (when an epoch
is *owed*, and the empty-epoch failure this rule's other end avoids);
[0039](0039-epochs-replace-tiers-refine.md) (why an epoch replaces rather than
coexists); [The Wearing chronicle](../../book/src/chronicle/the-wearing.md) and
[retrospective](../retrospectives/the-wearing.md) (followup F8, which owed this
record).
