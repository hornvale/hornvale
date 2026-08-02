# The Shuttle

A Full world builds in about three seconds. The suite's slowest test took
250 — and a flamegraph put 83.7 % of its cycles inside
`hornvale_terrain::globe::generate`, the plate-assignment and elevation
pipeline that sculpts a planet. The test built two worlds and rendered two
book volumes; the sculpting ran roughly 170 times. Every chorus readout —
`account_params_of`, `doctrine_of`, `day_schema_of`, even
`noun_class_of("earth")` asking whether a culture's sky is animate —
reconstructed the entire globe from the seed, answered its question, and
threw the globe away.

None of this was a bug in the ordinary sense. The chorus module's
derived-never-stored posture is a real constitutional virtue: nothing is
cached, nothing serialized, every answer re-derivable from committed state.
The defect was implementing that posture as *re-derive everything on every
call* when the derivation's expensive prefix — terrain and climate — is a
pure function of the seed that the caller usually already held. The lab had
hit the identical disease a month earlier and named the cure: The Single
Sculpt threaded a view's already-built terrain through the census metrics
and documented the result byte-identical. The artifact-taking `_from` idiom
was sitting in the tree, ten functions strong. The book, vessel, and
worldgen test paths had simply never adopted it.

## What the campaign did

Stage 1 finished the idiom. The chorus readouts gained (or had promoted to
`pub`) `_from` twins taking built terrain and climate; the book's entry
points — `render_volume`, `reckoning_at`, `esoteric_lines`,
`parse_context` — build the artifacts once and thread them; the vessel
`Session` sculpts once at `start` and reuses the pair for every turn; the
CLI's `book` command reuses the build's own `BuildArtifacts` rather than
reconstructing what it just made. Every existing `(world)`-shaped function
kept its exact signature as a delegating wrapper. A rendered volume now
sculpts **once** — measured by instrumented counter, not asserted.

The implementer's counter mattered. The plan's threading, applied
literally, left the tongue test at 54 s: `crisis_of` had no twin the plan
knew about, and `doctrine_from` carried a doc comment claiming a threading
its body didn't perform — `doctrine_explain` quietly re-derived climate one
level down. Both were found by counting sculpts rather than reading claims,
which is becoming this project's oldest lesson in new clothes.

Stage 2 dense-indexed `demography::coexist::pack`: the per-cell loop's
`(u32,u32)`-keyed overlap lookups became integer indexes into a matrix
built once per pack, under a constraint that no float operation move — the
values reach committed health pins.

## The numbers

On lefford, solo, dev profile: the tongue test 147 s → **9.5 s**; the
deep-grammar coherence battery 279 s → **23 s** (after its test callers
migrated to the `_from` forms — the product path alone got it to 147 s,
and the remainder was the test itself paying the wrapper's by-design
sculpt per probe); the vessel stitch law 162 s → **64 s**; the health
battery 91.6 s → **81.2 s**. The commit gate's nextest phase ran 565 s at
close on forty cores.

Byte-identity was proven on the total route, twice: the seed-42 world and
all three `book` lenses `cmp`/`diff`ed against outputs staged from the
pre-campaign binary before any code moved, and again after each stage. The
in-tree equivalence tests are honestly documented as what they are —
future-drift guards that reduce to `f(x) == f(x)` the moment a wrapper
delegates — because the review caught them being offered as evidence and
made the cross-binary artifacts the evidence instead.

## What was predicted wrong

Two predictions falsified, both kept as findings. The doctrine battery
barely moved (~184 s → ~170 s): measurement showed ~159 s of its
negative-arm sweep is sixty full world *builds*, ~13 s the readouts — its
cost was never the wrapper sculpts. And Stage 2's profiled "13 % flat
`cell_share` share" turned out to be mostly the irreducible O(n²)
power-law arithmetic the order constraint forbids touching; the avoidable
overhead was worth ~11 % of the health battery only after a review round
replaced the surviving per-pair map lookup the first fix had merely
relabeled. A profiler's flat self-time names where cycles are, not which
of them are optional.

One spec sentence was dropped in execution and is recorded as a deviation
rather than silently absorbed: `WorldComponents::assemble()` still runs
per readout, not once per entry. It sculpts nothing and the targets were
met without it; threading it (or formally accepting the cost) is in the
follow-up register.
