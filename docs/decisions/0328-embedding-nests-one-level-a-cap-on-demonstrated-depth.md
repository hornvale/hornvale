# 0328. Embedding nests one level — a cap on demonstrated depth, not a safety belt

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Relates:**
[0326](0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md);
[The Mortise](../../book/src/chronicle/the-mortise.md)

In the context of `Argument`/`Subject` gaining a `Clause(Box<Clause>)` variant,
we decided nesting is capped at exactly one level —
`CLAUSE_EMBED_MAX_DEPTH: usize = 1` — enforced in both the realizer and the
parser, and refused by an assertion that fails without the cap.

## Why a cap at all, when nothing can crash

`Clause` derives `Clone, Debug, PartialEq` and not `Serialize`, so recursion
carries no save-format risk. And `Box<Clause>` is unique ownership with no
`Rc` and no shared borrow, so a clause graph **cannot cycle** — infinite
regress is not a failure mode this type admits. The stack-safety framing for
a depth cap does not survive that fact: every caller in this tree is repo
code, so a stack overflow would have to be built deliberately, by hand,
thousands of `Box::new` deep. That is not a hazard the cap is protecting
against, and this record's first draft argued it was before being corrected.

## What the cap actually states

**One level is the depth this campaign builds, tests, and can show working.**
An uncapped `Box<Clause>` ships reach that nothing constructs and nothing
covers — the same shape as The Deed's seven inert concepts
(`LANG-in-character-acts-are-unspeakable`), arriving through a type instead of
a registry row. Refusing past the tested depth keeps the capability honest
about its own size: no corpus entry needs two levels, nothing in the world
constructs one, and a cap set above demonstrated need would be authoring a
distinction nothing states.

## What ships

A single named constant, read by both directions: the realizer refuses to
render a clause complement that itself contains a clause complement, and the
parser's recursive descent stops at the same depth and reports the failure it
already had rather than recursing further. Both `clause_embed_depth` (object
slot) and `subject_embed_depth` (subject slot) compare against the same
constant, so the two embedding sites share one budget rather than two
independently-tuned ones.

## Consequences we accept

**Raising the cap later is additive and costs no epoch.** `CLAUSE_EMBED_MAX_DEPTH`
is a plain `usize`, not a save-format contract — nothing about it is
serialized. A future campaign that demonstrates a real two-level need changes
one constant and extends the two call sites that already read it.
