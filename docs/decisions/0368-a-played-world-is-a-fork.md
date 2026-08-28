# 0368. A played world is a fork: live-play facts persist only when asked for, and the latch's durability is claimed only for the session

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot; the
original finding was ruled at the spec stop, and **corrected at the
Definition-of-Done sweep after the premise was tested rather than read**) ·
**Relates:** [0366](0366-passage-state-is-derived-never-stored.md) (the
mechanism whose lifetime this bounds),
[0171](0171-a-players-acts-are-not-filtered-out-of-a-saved-world.md) (The
First Mark's rule, which is the thing this campaign failed to find),
[0007](0007-seed-is-identity.md),
[0349](0349-the-offer-passes-through-the-observers-knowledge.md) (the axis
this re-scores) ·
[The Latch](../../book/src/chronicle/the-latch.md)

In the context of shipping the possession axis's 90% rung — *a door someone
else locked* — we decided that **the latch's durability is claimed for the
session and deliberately left unverified across a save**, accepting that the
campaign ships a mechanism whose most interesting property is untested.

## The finding this record was going to make, and why it was wrong

The campaign's spec §3.1 asserted: *"Nothing a possession session commits is
ever persisted… There is no world-writing path after genesis anywhere in the
CLI."* It was written into the spec, into `passage.rs`'s module doc, into an
idea-registry row, and into an earlier draft of this decision.

**It is false, and one command retired it.** `possess` takes a documented
`--out <PATH>` flag. `Session::into_played_world` folds the session's evolved
ledger **and its per-session registry** into a `World`, and `--out` saves it.
This is not obscure: it is The First Mark's Task 4, whose own decision (0171)
rules that a player's acts are *not* filtered on the way out, and whose test
`a_players_facts_survive_into_the_saved_world` asserts exactly that.

Measured at the Definition-of-Done sweep, seed 42:

| run | result |
| --- | --- |
| `possess --script 'go n; go n' --out walked.json` | played world written, **2 `agent-at` facts** in the saved ledger |
| the same saved world's registry | **`agent-at` present** — the per-session predicate travelled |
| `possess --world walked.json` | **rc=0** — a played world can be possessed again |

**How the error was made is the part worth keeping.** The `ledger` field's own
doc comment says "a clone of the frozen world's ledger… **Never written
back**." That sentence is *true*, and it answers its author's question: does a
session mutate the world it borrowed? No — `--world` is read-only, and
`cli/src/main.rs` says so in a comment. The spec read it as answering a
different question — can these facts ever be saved at all? — and the two
questions have opposite answers. A doc comment answers its author's question,
not the one a later reader brings to it.

## What is actually true

**A played world is a fork, not an update.** Live play never mutates the world
it possessed. Its facts are carried into a *new* `World` and written only if
the player asks with `--out`. So:

- **Default (no `--out`): session lifetime.** Nothing survives. This is the
  common case and the original finding's surviving half.
- **With `--out`: the facts persist into a new world file**, along with the
  per-session predicates that validate them, and that file can be possessed
  again.

## The rule

The latch's durability is claimed **for the session** — that is what acceptance
criterion 2 states and what
`a_cleared_passage_stays_open_for_the_rest_of_the_session` proves, across many
turns including a `wait` tick and the NPC activity it drives.

**Its behaviour across a save is deliberately not claimed, because this
campaign did not test it.** The mechanism that would carry it demonstrably
works for `agent-at`, a sibling predicate committed through the same
`Ledger::commit` call on the same session ledger, and nothing in
`passage-cleared`'s handling differs from it. That is a strong inference and it
is still an inference: no test drives clear → `--out` → re-possess → delve, so
this record does not assert the round trip works.

Writing that test is the cheapest piece of real work this campaign leaves
behind, and it is left as work rather than as a claim — because an
unimplemented followup is an unverified claim, and this record exists because
one of those was believed for a whole campaign.

## Consequences

- **The axis's rung names are about what a precondition READS, never about how
  long that state lives.** Conflating the two is what produced the original
  error, and the conflation is not local to this campaign: any future rung can
  make it.
- **Three questions the earlier draft called open are already answered**, and
  by The First Mark rather than by anyone here: a saved playthrough contains
  the whole evolved ledger unfiltered (0171); the per-session registry travels
  with it and needs no promotion to genesis; and reload reads the saved ledger
  directly, which is what a fold over committed facts needs. The
  `MAP-playthrough-persistence` registry row is corrected accordingly — the
  open question is narrower than "does persistence exist" and is really "is the
  fork the shape we want, and what happens when a saved world's derivation has
  moved under its facts."
- **`make rebaseline`, the drift check, and every gate were green throughout.**
  Nothing mechanical could have caught this: it is a false sentence in a spec,
  and the code it describes was correct the whole time.
