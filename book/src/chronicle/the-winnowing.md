# The Winnowing

[The Cistern](./the-cistern.md) closed the redundancy problem and named the
next one in the same breath: the globe's tile document is seventeen megabytes,
serializing it costs about a second, and that second was the one line in the
profiler that did not move. This campaign moves it, by the least clever means
available. The client now says which of the nineteen per-tile layers it will
read, and the producer emits those.

The document is a rare shape for this: nineteen independent parallel arrays
over one index space, with no cross-field structure. Any subset of them is a
coherent document. That is what makes a projection a filter rather than a
second serializer, and it is what keeps the testing story from exploding into
2¹⁹ documents.

## The measurement

The projection section of `cargo run --release -p hornvale-scene --example
profile_scene -- 8`, verbatim:

```text
  -- The Winnowing: projection at width 512 --
  metadata only (no layers)         32333 B
  composition, one layer at a time (bytes in any document carrying it):
    elevation_m             1417346 B    8.0%  read
    ocean                    694821 B    3.9%  read
    biome                    356964 B    2.0%  read
    plate                    306911 B    1.7%  read
    unrest                  1672216 B    9.4%  read
    t_mean_c                1380269 B    7.8%  read
    t_swing_c               1561341 B    8.8%  read
    t_diurnal_amp_c          962507 B    5.4%  UNREAD
    current_east            1223237 B    6.9%  UNREAD
    current_north           1213916 B    6.8%  UNREAD
    moisture                 789448 B    4.5%  read
    precip_mm_yr            1297220 B    7.3%  UNREAD
    snow_fraction           1469970 B    8.3%  UNREAD
    precip_regime            262162 B    1.5%  UNREAD
    cloud_fraction          1148668 B    6.5%  UNREAD
    weather_propensity       886083 B    5.0%  UNREAD
    cloud_type               262159 B    1.5%  UNREAD
    water                    262154 B    1.5%  UNREAD
    drainage                 529579 B    3.0%  UNREAD
    sum + metadata         17729304 B  (residual against the full document: 0 B)

  full document           17729304 B   serialize 1011.9 / 1029.1 / 1037.9 ms  (median 1029.1)
  Orrery's 8 layers        8211649 B   serialize 442.9 / 453.7 / 453.9 ms  (median 453.7)
  bytes    46.3% of full (-53.7%)   serialize 44.1% of full (-55.9%)
  proportionality: time/byte ratio = 0.952 (1.000 = serialize fell exactly with bytes)
```

**9,517,655 bytes, 53.7% of the document, are layers the client never looks
at.** Asking for the eight it reads costs 453.7 milliseconds against 1029.1 —
a 55.9% reduction against a 53.7% reduction in bytes.

That last comparison is the point of printing both. The specification's
predicted serialize time was arithmetic: it assumed cost falls in proportion
to bytes, said so explicitly, and said that if it did not, the discrepancy was
the finding. It did — slightly better than proportionally, a time/byte ratio
of 0.952. The reason is that what remains after the arrays are removed is
about 32 KB of metadata whose serialization cost is not float formatting and
does not shrink; a projection removes float-formatting work, which is the
expensive kind, and leaves the cheap kind behind. Proportionality was the
right first-order model and it was very slightly conservative.

### The composition table is a measurement, not an accounting

Each layer's figure above is the difference between a document containing only
that layer and a document containing none — which is that layer's contribution
to *any* document carrying it, if and only if layers are genuinely
independent. So the printed residual is not decoration. Nineteen measured
contributions plus the metadata sum to 17,729,304 bytes, and the full document
is 17,729,304 bytes: **a residual of zero at width 512**. The design's load-
bearing assumption is checked by the same run that uses it.

## Eight layers, not ten

The specification said the Orrery reads ten per-tile arrays and named them.
The task brief instructed the implementer to verify that list against
`parseTiles` in the client's own source rather than trust it, and the list was
wrong in two ways.

`features` is not a per-tile layer at all — it is document metadata, always
emitted, never selectable, and it appeared in the spec's list because it *is*
something the client reads. And `water` is not referenced anywhere in the
Orrery's `src/`: it was counted as read, and it is not. The client extracts
exactly eight per-tile arrays — `elevation_m`, `ocean`, `biome`, `plate`,
`unrest`, `t_mean_c`, `t_swing_c`, `moisture` — and eleven layers go unread,
not nine.

The error was in the campaign's favour, which is the uncomfortable half: the
predicted saving was 52% and the measured one is 53.7%, so nothing looked
wrong. A spec's transcription of another repository's source is a claim about
that repository, and it is checkable in one command.

## What is measured and what is inferred

The **byte** reduction above is measured, on this box, at width 512, and it is
a property of the producer alone.

The **parse** reduction is not. `JSON.parse` on the two documents in node,
warm, medians of three runs after a discarded cold pass: **289.5 ms → 124.5
ms**. That is a proxy. It is the right *order* of evidence — V8 is V8, and the
document is the same bytes either way — but the honest browser measurement
lives in the Orrery's own harness, a scripted headless-Chromium run sampled
over the debug protocol, which [The Frame Budget](./the-frame-budget.md) built
and which is in a different repository. This campaign claims a byte reduction
it measured and a parse reduction it inferred, and the two should not be read
as the same kind of statement.

Worth noting anyway: parse fell 57.0% against the bytes' 53.7%, so the same
mild super-proportionality shows up on the consumer's side, for the same
structural reason — array-building dominates and metadata does not shrink.

### An unexplained baseline, named rather than smoothed over

The specification quoted a serialize baseline of ~553 ms, traceable to [The
Sextant](./the-sextant.md)'s spec (567.0 ms for a 17,313 KB document). The
comparable number from this campaign's run is the profiler's `hw_scene_tiles
json` line — **1021.3 ms** — which is serde's derive on the full document,
literally the same path and the same line The Sextant and The Cistern
measured. (The projection harness's 1029.1 ms above goes through the new
manual serializer and corroborates it within 0.8%, but it is not the
unchanged path, so it is not the number to compare.) So: the same document
size, on a quiet box, along an unchanged serialization path — `git log` shows
nothing touching `kernel/src/quantize.rs` or the scene crate's emit path
between the two beyond this campaign's own byte-identical work. The Cistern's committed chronicle
records 1053.6 and 1062.9 ms for the same line. Two of three independent
measurements agree, and The Sextant's is the outlier.

The campaign's claim is a ratio taken within one process on one build against
one world, so this does not touch it. But it is a roughly 1.8× discrepancy in
an absolute figure that a committed profiler produced, and the resolution is
not known. It is recorded here and in the retrospective's follow-ups rather
than quietly replaced with the number that came out.

## What shipped

`TileFields` in `windows/scene` — a set of layer names, not a bitmask, because
the wire already speaks these names and an unknown one must be a loud error
rather than a silently ignored bit. `scene_json_selected(&scene, &fields)`
beside the existing `scene_json`, and `hw_scene_tiles_selected(width, len)` in
the wasm catalog, reading a JSON array of names from the input buffer.
`hw_scene_tiles(width)` is unchanged; a caller that says nothing gets exactly
today's document. The catalog grew 15,617 bytes (+1.68%) — a before-and-after
of the same build, which is the only way that delta means anything. The
absolute size moved for an unrelated reason: rebuilt at the end, the catalog
is **986,525 bytes**, leaving **62,051 bytes (5.9%)** of headroom under the
1 MiB gate. Most of that consumption is not this campaign's. The margin is
nonetheless narrower than the delta alone suggests, and the next
catalog-growing change should measure the size rather than subtract from a
remembered one.

`scene_json` deliberately keeps using serde's derive while the projection goes
through a hand-written `Serialize` on a private wrapper. That is the whole
reason the equality test between them is evidence: if both paths ran through
the manual implementation, the test would compare a thing to itself. The
default document is byte-identical by construction, verified at width 512.

## The test that passed for the wrong reason

The design's testing story had three pins, and the plan shipped two of them —
an equality test between the full projection and the derive, and a per-layer
independence test asserting that a layer's bytes inside a single-field
document match its bytes inside the full one. Both were real. Both were also
blind to the one failure that matters most.

The Task 1 reviewer gutted `TileFields::contains` to return `true`
unconditionally — defeating the campaign's entire purpose, emitting every
layer regardless of what was asked for — and **both tests stayed green**. The
equality test only ever exercises `all()`, where over-emission is correct
behaviour. The independence test only inspects emitted bytes, and the helper
that locates a layer *panics* when the layer is absent, so it could never
observe a layer that should not have been there. Nothing at the Rust level
tested that the projection projects.

The missing pin was the third one the spec had asked for and the plan dropped:
a committed golden of a representative subset. It exists now —
`tiles-seed-1-w16-projected.json`, seventeen of thirty-one keys, with holes at
the head, in the middle, adjacent to metadata on both sides, and at the tail —
and it is red under the reviewer's exact mutant.

The general shape is worth keeping. A property test that checks *what is
present* cannot see *what should not be*, and absence is the harder half to
assert precisely because a helper written to find things naturally fails loudly
when it cannot. The wasm-side smoke test had the same weakness in miniature
and it is closed the same way: it asserted one withheld layer of sixteen, and
now filters the full name list and checks all of them.

## An outlier found by writing a doc comment

The new export sets an error envelope on *every* non-zero return, including
the two that refuse before reading a byte of input. That needed a paragraph
explaining why, and writing it surfaced something about the export next door.

On a refusal, `OUT` still holds whatever the last successful call left there —
for this export, a multi-megabyte tiles document. A caller that ignores the
return code and reads `OUT` gets a stale success payload. That much is
ordinary. But `hw_new_pinned` sets `WORLD = None` *before* its `-1` and `-2`
returns, so after a refused pinned call the buffer holds a document belonging
to a world that no longer exists — not a stale payload, a stale payload for a
**dead** world. The buffer and the live state disagree about which world is
current, and only the return code says so.

Nothing here is a defect: every caller in the repository checks its return
code, and the drive script asserts these paths. But it makes `hw_new_pinned`
the outlier rather than this export the exception, which is the opposite of
how it read while the doc comment was being written. Recorded as a followup,
not fixed, because changing that export's clearing order is a separate change
with its own byte-identity story.

## What this is not

The producer still computes all nineteen layers. This campaign buys wire
bytes, serialization time, and client parse time; it buys no sampling time at
all, and `tiles_scene`'s ~600 ms build is untouched. Skipping the *build* of
unrequested layers is a larger and more invasive change, and the numbers above
are what would justify it.

Nor is it a re-encoding. Base64 typed arrays would beat JSON's ~10 bytes per
float badly — roughly 4 against 10 on *every* field, not 54% off the unread
ones — but that is `scene/tiles/v2` and a coordinated client change.
Projection composes with a later encoding change rather than competing with
it, which is why it went first: it is where the measurement lives. The
`ocean` array remains 5.3 bytes per element to carry one bit, about 430 KB of
pure `true,`/`false,` syntax, and that too is an encoding problem rather than
a projection one.

And it is the smallest win of the three campaigns in this sequence — roughly
half a second of serialization and an inferred 165 ms of parse, against The
Cistern's fifteen seconds. That is the honest framing rather than a
disappointment: the volume problem is now half-solved by the cheap half, and
what remains of it is legible.
