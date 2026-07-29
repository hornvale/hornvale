# The Formations — retrospective

Process lessons, not product. A nine-crate taxonomy change that moved zero
bytes.

## Projection-first is worth reusing

The technique that made this safe: **keep the old type and make it a projection
of the new one.** `Biome` stayed exactly where it was and became what a
`BiomeExpr` looks like flattened. Every consumer kept its call site; the
disentangling happened underneath a surface that never changed.

The alternative — migrate the nine crates that reference `Biome` to the new
types — would have been a large diff with a large blast radius, reviewed all at
once, with byte-identity provable only at the end. Projection-first inverted
that: the risky part (Task 3, where classification could move) was one function,
guarded before anything downstream was touched at all.

**Reusable shape:** when disentangling a type that many crates consume, ask
whether the old type can become a *view* of the new one rather than a thing to
be replaced. If it can, the migration stops being a migration.

## A transcribed oracle is not duplication

The equivalence sweep compares the new classifier against a verbatim copy of
the old one, kept in the test module. That copy looks like exactly the
duplication tests are supposed to avoid, and deleting it would silently make
the test worthless: delegating to the shipped function would compare it with
itself, and pass forever.

**Lesson:** when the claim is "the new path reproduces the old one", the old
path has to survive somewhere the new one cannot reach. Write it down and say
in a comment why it is there, or the next tidying pass will remove it.

## Preserve the arms that look like bugs

Two arms of the classifier read like defects — a trench outranking a ridge, and
a shallow temperature band matching neither reef nor kelp. Both were kept
exactly, and the plan called them out *before* implementation, with the reason.

That mattered. Either would have been tempting to "fix" while touching the
code, and either would have changed which biome thousands of cells report,
cascading into settlement placement and names. The campaign's whole claim would
have been false.

**Lesson:** a refactor plan should enumerate the surprising behaviour it is
deliberately preserving. Absent that list, a well-meaning implementer improves
something and nobody notices until an artifact moves — or worse, until one
doesn't move that should have.

## The self-review earned its keep again

Writing the plan's Task 5 against `domains/culture` was wrong: the function it
targeted does not exist there, and cannot — culture may not depend on climate.
The real mapping is `worldgen::biome_class`, at the composition root. The
plan's self-review caught it by checking a name against the source rather than
trusting the sketch.

This is the second consecutive campaign where the `writing-plans` self-review
found a defect that would otherwise have surfaced mid-execution. It is cheap
and it keeps paying.

## Three checks, deliberately not one

Byte-identity was verified three independent ways: the >600-case sweep, the
seed-42 world fixture already standing in the gate, and a `cmp` against a
binary built before the campaign. That is redundant on purpose — The
Occlusion's lesson was that a determinism claim checked along *one* route is
not verified, and the cheapest total check (build both, compare) is the one
that catches what route-specific reasoning misses.

Regenerating every artifact in the book produced no diff at all, which is the
strongest single statement the campaign can make about itself.

## Follow-ups

- **A third copy of the biome→class taxonomy** lives in
  `windows/lab/tests/calibration.rs:121`, keyed off kebab-case *names* rather
  than the enum. It is unaffected by this campaign and was deliberately left
  alone — unifying it touches a preregistered battery and deserves its own
  change. Noted so it is not forgotten a third time.
- **`Formation` has no concepts yet.** Deliberate: concepts and name glosses
  belong to campaign 2 (The Variants), which owns O1's epoch. Campaign 1 stayed
  concept-neutral so it could stay byte-identical.
- **`Realm` has two values and one consumer.** That is the intended end state
  for this campaign — the triple exists so later realms are values — but it
  will look like over-engineering to a reader who has not read The Stratum §3.4.
  The chronicle carries the reason; keep the link alive.
