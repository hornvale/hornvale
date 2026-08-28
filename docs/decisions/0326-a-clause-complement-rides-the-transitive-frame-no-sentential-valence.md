# 0326. A clause complement rides the transitive frame — no `Valence::Sentential`

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Relates:**
[0297](0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md);
[The Mortise](../../book/src/chronicle/the-mortise.md)

In the context of giving `know` and `think` a clause-shaped object — *"I don't
know **why he killed her**"* — we decided that a clause complement rides the
existing `Valence::Transitive` frame rather than a new `Valence::Sentential`
variant.

## Why the obvious design is wrong

`know` needs **both** an NP object (*"I didn't know her"*, decision 0297's
`m10`) and a clause object (*"I don't know why he killed her"*, `m06`).
`PREDICATE_VALENCE` is `&[(&str, Valence)]`, resolved through `.find()` —
first row wins — so one predicate cannot carry two valences without re-keying
the table on `(predicate, object-shape)`, which is a table nobody proposed and
would duplicate 0297's single-source-of-truth structure for no gain.

`clause.rs` states the tripwire this decision follows explicitly: *"If a
future campaign finds itself adding a variant per predicate, it has rebuilt
`Frame` and should stop."* A predicate taking an NP-or-clause object is one
argument structure with a category-flexible object — exactly what `Valence`
already sorts predicates into.

## What ships instead

`Argument` gains a `Clause(Box<Clause>)` variant, and `object` may hold one
under `Valence::Transitive` like any other argument. The only special case is
`Part::Determiner`, which becomes a no-op when the object is a clause — the
one place that already switches on the object's shape. Without it, Common
would say *"I do not know **a** he killed her."*

## Consequences we accept

**One table survives; no agreement test is created that could rot.** The same
argument 0297 made for valence itself applies unchanged: `Valence` is
many-to-one, and adding a variant per predicate is the failure mode it exists
to prevent.

**`Argument::Clause` is depth-capped, not `Valence`-capped.** Nothing about
this decision bounds how deep a clause complement can nest; that bound is a
separate, later decision (0328).
