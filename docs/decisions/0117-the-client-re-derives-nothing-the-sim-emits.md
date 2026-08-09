# 0117. The client re-derives nothing the simulation already decides

**Status:** Accepted (2026-08-09, G6) · **Decider:** Nathan · **Relates:**
[0022](0022-sim-emits-data-clients-render.md),
[0114](0114-a-native-client-drives-across-the-linker-and-reads-across-the-serializer.md),
[0115](0115-a-clients-mirror-may-omit-a-channel.md)

In the context of a client that had grown an always-visible row listing the
exits from the current place — and rendered it wrong underground, where the
prose said `Ways on: out.` and the row said `Ways on: NE, NW, S.` — we decided
that **a client renders what the simulation emits and re-derives none of the
simulation's decisions**, and that the correct repair for such a row is to
**delete it**, not to feed it a better channel.

## What happened

The row was built, then rebuilt from a better source, then reviewed. The review
found that the session's spatial channel splits `walk` from `chamber`, and that
`walk` means *not inside a built structure* — so `submerged` and `underground`
both fold into it. Underground, the row was rendering the surface locale's
exits: three bearings answering a question nobody asked from below. A reviewer
generated a genuine underground snapshot and rendered it unmodified to prove it.

The client could not fix it. Nothing on the wire distinguishes the two states
except the literal word inside the prose, and parsing prose for meaning is the
one boundary the render crate exists not to cross.

The proposed fix was to have `vessel/session/v1` emit the exits as a structured,
additive field. It was argued for at length — including a correct argument that
a *described* opening ("a wooden door which appears nailed shut") need not be a
*traversable* one, so prose and structure answer different questions.

Then the question that dissolved it: **why does the client need to know which
directions the player can go?**

It does not. `Session::handle(&str)` already tokenizes and parses; the key
mapping sends `go <direction>` unconditionally and consults nothing; an invalid
move returns "No way n from here." The only consumer of the exits was a row that
displayed them — and they were already on screen, because the simulation puts
`Ways on: …` in the room prose, which the client renders verbatim.

## The decision

The row is deleted. With it went **three** client reimplementations of
simulation knowledge: a filter over compass bearings, an invariant about how
chambers connect, and a dispatch on which band the possession is in. No channel
was added.

The general rule this record ratifies:

> **A client may render a datum the simulation emits. It may not re-derive a
> decision the simulation makes.** If a display needs an answer the wire does not
> carry, the question to settle first is whether the client needs the answer at
> all — not which channel should carry it.

## Why deletion beat a better channel

The prose was **right in exactly the case the row was wrong**, because the
simulation knows its own band and the client never can. Every reimplementation
the client carried was a second, worse copy of a rule whose authoritative
version already ran upstream — and each one was a fresh opportunity to diverge
under a condition nobody had a fixture for.

The abandoned argument is worth keeping, because it was a good argument for a
requirement that did not exist. Its generalisable form: **before designing a
channel to serve a consumer, check that the consumer acts on it.**

## Consequences

- **The band fold stays open, and this campaign is evidence for it, not a
  resolution of it.** `band == "walk"` still carries no signal distinguishing
  submerged from underground from surface, and a display that needed that
  distinction still could not get it. The client simply stopped needing it.
- **A truncation guarantee was given up knowingly.** With the exits back inside
  the prose, a sufficiently long entry could push them behind the entry pane's
  `… more, not shown …` marker. That is a *rendering* problem deserving a
  rendering answer (a scrollable pane), not a duplicated datum, and it is
  hypothetical today — neither committed fixture comes close to overflowing.
  This reasoning is recorded in `clients/game/core/src/entry.rs`'s module doc so
  that nobody "fixes" it by re-adding a row.
- **It bounds decision 0115's licence.** A mirror may omit a channel; that
  omission is never evidence about what the producer emits. The intermediate
  ruling here — "no ways-on datum exists on the wire" — was derived from the
  client's own mirror and was false. Read the producer.
- **The rule has teeth in one direction only.** A future noun-entry mode for
  `examine` sits under it: the client must not read `narration.nouns` to choose
  an object on the player's behalf, for the same reason it must not validate a
  move against exits.

## See also

[The Quire chronicle](../../book/src/chronicle/the-quire.md);
[the retrospective](../retrospectives/the-quire.md);
`clients/game/core/src/entry.rs`.
