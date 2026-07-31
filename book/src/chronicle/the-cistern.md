# The Cistern

[The Sextant](./the-sextant.md) fixed the ship's position and did not move it.
This campaign moves it. A region patch — the document the Orrery requests once
per level-of-detail tile, the one that made a camera move cost seventeen
seconds — went from about nine hundred milliseconds to about eighty. The
change is not clever. The scene window now derives the planet once per world
instead of once per call, and the cistern that holds it is a struct with six
fields.

## The number, and how it was taken

The measurement is a ratio, and the way it was taken matters more than usual.
The Sextant left a committed profiler behind, shaped like the Orrery's session:
build a world, ask for the six scene documents, then ask for a fan of region
patches at the client's own level-of-detail constants. That profiler now runs
the workload **twice in one process** — once through the `&World` entry points,
which each build their own context internally, and once through the `_in`
variants against a context built beforehand — and prints both columns.

```text
scene profile (seed 42, 8 region tiles):
  hw_new                        2957.9 ms

  -- &World path: one planet derived per call --
  hw_scene_tiles(512) build     1448.1 ms  (17313 KB)
  hw_scene_tiles json           1053.6 ms
  system+moons+neigh+ecl           0.4 ms  (12712 B)
  hw_scene_tiles_region x8      7218.6 ms  (2335 KB)
    ... per tile                 902.3 ms
  TOTAL                        12678.6 ms

  -- SceneContext path: one planet derived per world --
  SceneContext::build            812.2 ms
  hw_scene_tiles(512) build      600.3 ms  (17313 KB)
  hw_scene_tiles json           1062.9 ms
  system+moons+neigh+ecl           0.4 ms  (12712 B)
  hw_scene_tiles_region x8       651.2 ms  (2335 KB)
    ... per tile                  81.4 ms
  TOTAL                         6084.8 ms

  per tile   902.3 ms -> 81.4 ms   (11.1x)
```

**11.1×** on the per-tile figure, and 10.8× on an immediately preceding run —
against the specification's hypothesised ~11×, which was arithmetic done in
advance from The Sextant's flamegraph and its 638-millisecond fixed overhead.
The hypothesis was preregistered and it held.

Both columns were taken on a forty-core box carrying roughly eighty runnable
threads of somebody else's test suite, which is why the absolute milliseconds
are inflated — 902.3 against The Sextant's 702 for the identical unfixed code.
That is precisely the confound a two-pass instrument removes. A before and an
after measured in separate runs differ by the code *and* by whatever the
machine was doing in between; measured in one process, against one world, on
one build, the only surviving difference is the code. The ratio is the
statistic; the milliseconds are context.

The residual is worth naming honestly. The specification predicted about
sixty-four milliseconds of real sampling per patch, a figure it had obtained
by subtraction (702 − 638) and flagged as such. What the direct measurement
shows is eighty-one, on a loaded box. The comparison that means anything is
the *fraction* of the unfixed call the residual represents, since the two
figures were taken under different loads: 81.4 of 902.3 is 9.0%, against the
prediction's 64 of 702, which is 9.1% — the same basis on both sides. The
subtraction was sound, and the measurement came in a hair under it rather
than over. The ~11× is where the agreement is loudest, but the residual
agrees too.

## The ratchet, moved down for the first time

The Sextant's other bequest was a falsification ceiling per client-visible
operation, held in the full gate, each constant recording its measured value,
its date, and the host it was measured on — with a rule attached: **lowering a
ceiling is free, raising one is an explicit reviewed act.** That asymmetry was
written because the settlement-graph battery's budget had been re-baselined
upward several times as the world grew, each raise correct, with nothing in
the mechanism marking a raise as unusual.

The rule had never been exercised downward. It has now.

| ceiling | Sextant | Cistern | measured |
|---|---|---|---|
| region patch, per tile | 3100 ms | **420 ms** | 206.1 ms |
| `tiles_scene(512)` + JSON | 11000 ms | **8700 ms** | 4319.9 ms |
| `SceneContext::build` | — | **2700 ms** | 1308.0 ms |
| four small documents | 5.2 ms | 5.2 ms | 2.7 ms |
| genesis | 13000 ms | 13000 ms | 6318.6 ms |

Two of those rows are the interesting ones, and neither is a ratchet.

The **small documents** did not move, and that is the ceiling's whole purpose.
The star system, the moons, the neighbouring stars and the eclipse windows read
the sky and derive no terrain, which is why the four of them together cost
under three milliseconds. Their ceiling exists to catch one of them silently
acquiring a terrain derivation. It reads 2.7 against 2.6 — noise — and
together with genesis reading 6318.6 against 6442.8 it does a second job
nobody designed it for: it is the **control**. The two quantities this
campaign did not touch reproduced the earlier basis under the same load band,
which is what makes the other two rows attributable to the code rather than to
a quieter machine.

`SceneContext::build` is a **new** ceiling, and adding one during a ratchet
deserves an argument. The 638 milliseconds did not vanish; they moved. Taking
the derivation out of the per-call ceilings without putting a ceiling over
where it went would have left the single most expensive operation on this
surface unguarded — a regression inside terrain or climate derivation would
have tripped nothing here, and the campaign would have improved the numbers
while degrading the coverage. A ratchet that loosens the thing it stops
measuring is not a ratchet.

## Byte-identity, and a checksum that failed correctly

This is a pure performance change touching the code path that produces
committed artifacts, so byte-identity was the acceptance criterion rather than
a hoped-for side effect. Eleven documents for seed 42 — the four astronomical
scenes, the globe tile document at width 512, three region patches, a
full-globe temperature grid, a regional temperature grid, and the world's own
serialized ledger — were checksummed before a line of the refactor was
written.

At the close, ten of the eleven verified byte-for-byte. The eleventh,
`world.json`, failed.

It failed because main had meanwhile landed a change that stamps every world
with the epoch labels it was derived under, and the diff between the two files
is that stamp and nothing else: twelve added lines at the end of a
five-and-a-half-megabyte document, with all 26,309 facts untouched. The
attribution was cheap to establish and it is the reason the split is worth
recording. A verification suite whose every line always passes is
indistinguishable from one that cannot fail. Here one line went red, the
reason was knowable in a single `diff`, and the ten lines that mattered stayed
green — which is a stronger result than eleven silent OKs would have been.

The scene documents' independence was checked twice more: the full artifact
regeneration produced no drift in any committed gallery, reference or
laboratory file, and the crate's committed goldens — including a region golden
added mid-campaign precisely so the absolute evidence would live in the
repository rather than in `/tmp` — held.

That last precaution turned out to matter, because the campaign's most obvious
test quietly stopped being evidence. A byte-equivalence test comparing the
`&World` path against the context path was written first, before the refactor,
and it was real evidence at that moment. By the end every `&World` form
literally delegates to its `_in` form, so the test compares a function against
itself and now asserts only that context construction is deterministic. It
still earns its place — it would catch a future divergence between the two
paths — but it is no longer the net it was written to be. A test can be made
tautological by the very change it was written to guard, without failing, and
without anyone editing it.

## What it cost, and a measurement that was wrong twice

The client is a size-critical WebAssembly binary compiled at `opt-level = "z"`,
where ties go to the smaller build. Holding a second live structure costs
bytes: **1,569 of them, +0.171%**, on a 915,830-byte baseline.

Getting that figure right took three attempts, and the reason is the
interesting part. Early measurements disagreed with each other by a factor of
four. The binary embeds source file paths — `#[track_caller]` panic locations
survive `strip = true` — so its size depends on where on disk it was built.
Two builds of byte-identical source, from directories at different depths, are
not the same number of bytes. Every comparison has to hold the build location
constant, and the corrected figure above does.

## What survives

The specification named one thing it would not fix, and it was right to. The
globe's tile document is seventeen megabytes, and serializing it costs about a
second — the one line in the profiler that is *identical* in both columns
(1053.6 against 1062.9 milliseconds). Building that document got 2.4× cheaper
(1448.1 down to 600.3 milliseconds); emitting it did not move at all, and is
now the larger half of its cost. The redundancy problem is closed and the volume problem is
next — and it was next: [The Winnowing](./the-winnowing.md) took it up
immediately, and found that a client asking only for the layers it reads pays
46.3% of the bytes and 44.1% of that serialization second.

The structural guard The Sextant specified and could not write is now written,
in two layers that check different things: a source scan asserting that every
scene entry point in the catalog reaches an `_in` variant, and a behavioural
test that the two paths agree. Either alone is nearly worthless — the first
passes if the `_in` variant re-derives internally, the second passes if the
catalog never calls it — which is the standing lesson about multi-layer
guards, applied on purpose rather than rediscovered.

And the shape of the whole thing is worth stating plainly, because it is an
argument for building instruments before fixes. The Sextant produced no
speedup and was, at the time, a campaign that measured something and stopped.
What it actually produced was a preregistered claim, a profiler that could
check it, and a ceiling that would move when it was true. This campaign wrote
a struct with six fields and passed a reference around. The reason anyone can
say it worked — a number, a ratio, a control, and a ceiling that fell — is
entirely the earlier campaign's doing.
