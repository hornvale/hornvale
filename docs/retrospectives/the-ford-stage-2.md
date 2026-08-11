# Retrospective — The Ford (stage 2)

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-ford-stage-2.md): a room stores
the signed distance to the channel and the band edges for reading it, keeps
its `locale/room/v2` tag, declares which of its fields were decided by which
model, and answers fordability about a *step* rather than about a place.

**This is a stage retrospective, not a campaign one**, like
[stage 1's](the-ford.md). Stages 3 (riparian conditioning), 4 (scene
emission) and 5 remain, and `MAP-ford-subcell-water` stays `spec'd`.

**One reversal to record up front.** Stage 1's spec §9 — and stage 1's own
retrospective — said stage 2 would redefine `Locale.fields.water` and mint
`locale/room/v3`. It did neither. Decisions 0121–0124 landed while The Ford
was in flight, and 0124 requires a change *presented as a refinement* to
preregister a conservation criterion this geometry cannot meet (39 of 700
seed-42 river cells carry no polyline). The design moved to appended trailing
keys with no epoch, and §4 of the stage-2 spec lists the invariants that make
that framing checkable rather than merely convenient. Anyone reading the older
documents should read this paragraph beside them.

## The readout

| hypothesis | preregistered | measured | verdict |
|---|---|---|---|
| H2-1 sign is a pure function of the seed | 100% | 100% | green — **but see §1** |
| H2-2 appending is byte-clean | 100% | 19 inserts, 0 deletes, 0 replaces over 7 artifacts | green, independently verified |
| H2-3 ordinal reproducible from the stored quantity | 100% | 100% from the *serialized, quantized* document | green, deliberately narrowed |
| H2-4 fordable fraction of the network | [0.10, 0.70] — a **late freeze** | 0.0308 → 0.2170 → 0.7059 → **0.3372** | **not resolved** |

## 1. A green hypothesis can be blind to the failure its section exists to prevent

H2-1 read "two builds of the same seed agree on the sign for 100% of sampled
rooms", and an earlier draft added that anything below 100% would prove the
referent was still build-ordered. That inference is not entailed. Both arms
build the same seed at the same commit and `build` is already asserted
deterministic, so the two networks are structurally identical and *any* pure
function of them agrees bit-for-bit. Mutation settles it: reversing every run
in the world (`run.reverse()` in `build`) leaves H2-1 **green**.

What H2-1 establishes is **purity** — no global state, no wall-clock, no
iteration-order nondeterminism. Durability is carried by one different
assertion, `the_polyline_vertex_order_is_downstream_order`
(`domains/terrain/tests/channel_properties.rs:290`), which is the only one
whose reference comes from *outside* the object under test
(`TectonicGlobe.downhill`) and the only one that reddens under the reversal.

The structural fact that deserves stating plainly, because a close is where it
would otherwise be assumed away: **the durable referent is guaranteed by
assertion, not by construction.** Nothing prevents a future change to run
construction from reversing a run; a test catches it. That is the right
engineering call — no runtime cost, no duplicated geometry — but it is a
different guarantee from one the type system or the data layout would make,
and the difference should be visible to whoever next edits `build`.

**The general form:** *two arms that share a construction cannot test the
construction.* Ask what the test's reference is, and whether it lives inside
the thing being checked.

## 2. Six vacuity findings in one campaign, all of that same shape

This is the campaign's dominant defect class, and every instance is a guard
whose reference lives inside the thing it checks:

1. **H2-1** (above) — both arms build the same object.
2. **H2-4's denominator**, caught before any code: "adjacent room pairs whose
   sign differs" is not a candidate population, it *is* the ford set — the
   left-bank → right-bank transition without an intervening channel room is
   possible only when the channel is narrower than one step, which is the
   crossability criterion itself. It would have read ~1.0 by construction,
   exactly as `channel-connectivity` became a constant in stage 1.
3. **H2-3**, caught while resolving where a room's `channel_bands` come from:
   taking the edges from the same call that computed the `Transverse` makes
   `Transverse::from_band(band(d, &edges))` agree *by construction*.
4. **The implementer's own same-bank test**, caught by their own mutation
   testing rather than by review: it passed with the sign clause deleted,
   because the band clause was excluding its pairs.
5. **`water_reading.rs`'s `grid_level == ctx.globe_level()`** — the field
   compared to its own source (deferred, below).
6. **The width positive control's expected value**, which restates the clause
   from the same inputs: it proves the clause is *live*, not that it is
   *right* (deferred, below).

Two repairs generalize. For (3), **move the reference outside the object**:
H2-3 now recomputes from the serialized, quantized JSON, so what survives is a
real claim (the document is self-sufficient and quantization does not flip a
band) rather than a tautology. For (4), **choose a population where every
other clause is pre-satisfied by construction**, so the clause under test is
the only thing that can decide the outcome — the reviewer named that the
correct general repair shape, and it is worth reusing by name.

## 3. A crate-scoped green is not a branch-green

Task 1 left the branch **red for an entire task and nobody noticed**. The
durability assertion in §1 — the single most load-bearing test this stage
produced — shipped without a `/// claim:` tag, so `cli/tests/claim_shape.rs`
failed from `16b05a1f` until `e951261d`. Task 1's own evidence was
`cargo test -p hornvale-terrain`: correct for the code it wrote, and blind by
construction, because the workspace enforcement tests live in `cli/` and no
`-p <domain>` invocation can ever reach them.

The rule this implies is narrow and cheap: **a task that adds a test or moves
a public boundary owes at least one `cli/`-inclusive run before it reports
complete**, even if the full gate waits for the close.

## 4. A mutation that silently fails to apply produces evidence

One discharge mutation appeared green — the clause looked unguarded when it
was not. The patch had matched nothing: `cargo fmt` had rewrapped the target
expression, so the search text no longer existed in the file. A mutation that
does not apply is indistinguishable, in its output, from a mutation the suite
survives, and it argues for the *opposite* conclusion from the truth.

Every mutation afterwards was **grep-verified as applied before its result was
believed**, and the pasted grep is part of the evidence rather than an
assertion that it was run.

## 5. Two mutations can present as identical, and one of them can be stale

A review challenged a mutation row reporting 0.3607 — exactly 123/341, which
is the *discharge*-deletion signature — appearing in the band-gate row. The
challenge was right: the row had been carried over from the pre-rebuild
instrument rather than re-measured. Re-running it against the rebuilt
instrument gave the same fraction with a **different composition**: unmutated
341 = 115 F + 8 I + 218 N; band gate widened to not-`Dry` = 123 F + 8 I +
210 N; discharge deleted = 123 F + 0 I + 218 N.

**Report compositions, not just the headline fraction.** Two different
mutations produced the same number, and only the breakdown distinguishes them.

## 6. Writing a measurement as a formula exposes its unnamed terms

The single highest-value line of prose in this stage was notation. Writing the
H2-4 measurement as

```text
frac = |{v : Fordable(transect(v))}| / |V'|
```

made it immediately visible that `V'` was not `V` and that **nothing said what
the difference was**. That is exactly where the stage's largest defect was
hiding: a mesh-adjacency check later found **307 of 341 pairs were not
adjacent**, so the instrument's "the pair is a walker's step" justification was
false for 90% of its population, and the reading that rested on the 34
survivors carried an unexamined selection mechanism.

Prose can hold an unnamed term indefinitely. Notation cannot.

## 7. H2-4: what an instrument rebuild costs a hypothesis

Four readings exist — **0.0308, 0.2170, 0.7059, 0.3372** — and **three of the
four instrument changes were made after seeing a number**. The criterion
itself was already a *late freeze* (chosen with stage-1 data in hand), so two
discounts stack.

| # | instrument | reading | what forced the change |
|---|---|---|---|
| 1 | one room edge *per probe* | 0.0308 | outside the interval; the probe unit disagreed with the criterion's unit |
| 2 | one room edge *between the pair* | 0.2170 | a review found nothing asserted the pair was a step |
| 3 | pairs filtered to mesh-adjacent | **0.7059 — falsifying** | 307 of 341 pairs were not adjacent |
| 4 | transect *constructed* from three mesh steps | **0.3372** | adjacency made structural; both drop causes impossible; \|V'\| = \|V\| = 341 |

Change (1) is defensible as a **correction rather than a rescue** — it *adds*
a constraint (the pair must be an actual step with the channel-bearing room
between them) and makes the probe unit agree with the criterion's own unit —
and change (4) was forced by a genuine defect, not by a number. The
implementer also declared the rebuilt rule's one free parameter (the
any-of-three-steps quantifier) and an expected 0.20–0.50 band **in a file
written before the run**, verified by timestamp between the two commits.

None of that restores epistemic status. The adopted ruling, carried in the
chronicle and at the assertion site:

> H2-4 was not tested under preregistration and is not resolved. 0.3372 at one
> room edge at walk depth is reportable as a measurement of the world at a
> stated step length, not as a confirmation of the [0.10, 0.70] interval.

The range check that remains in the suite is labelled **a witness, not a
hypothesis test**, in its doc comment and again at the assertion site, so a
reader of only the headline cannot come away believing the interval was
confirmed. The corresponding discipline, which held here: when the instrument
moves under unblinded observation, the honest move is to *spend the
hypothesis*, not to widen the interval or to quietly report the last reading.

The implementer also removed a softening **no review had named** — a paragraph
claiming the interval "is robust across a factor of four in step length" when
three of five sweep rows are inside it and two are not. Robustness is a
property of the instrument, never evidence for the hypothesis.

## 8. A precedent cited in prose can contradict the same document's next sentence

Task 2's brief described `Resolution` as "a small `Serialize` struct of
`&'static str` field-name → resolution-name pairs" and then, two sentences
later, told the implementer to read decision 0123 and "match the precedent it
establishes rather than inventing a second shape". The prose sentence **was**
the invented second shape. The instruction governed; the shipped struct
matches `windows/scene/src/surrounds.rs`'s three keys and adds a fourth list
for the channel-resolution fields.

Where a brief specifies by precedent, the precedent is the authority and any
paraphrase of it in the brief is a hazard, not a convenience. Specifying by
precedent is still right — 0123's shape had landed days earlier in another
campaign and guessing at it from outside would have been worse — but the
paraphrase should be dropped rather than written carefully.

## 9. The defects were in the controller's own text again, and re-running found them

As in stage 1: most defects this stage were in spec, plan and brief text
rather than in implementers' code, and **nearly all were caught by re-running
a check rather than by re-reading prose** — the reversal mutation, the
adjacency check, the recomputed mutation table, the timestamp verification.
Re-reading found the two prose contradictions in §8 and §1; everything
quantitative came from making the machine say it again.

## 10. What did and did not have to be regenerated

The close found **zero artifact drift**: `make rebaseline` produced no change
across `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`,
`docs/audits/`, `docs/digest/`, `book/src/domesday/` and
`clients/game/core/tests/fixtures/`, because Tasks 2 and 3 regenerated the
seven locale-embedding artifacts and the type-audit report *in the commits
that drifted them* (`cb452ef9`, `bea0aadd`). That is the intended shape and
worth naming as a success: a close that has nothing to regenerate is a close
where every drifting commit paid its own bill.

The drift check was confirmed non-vacuous rather than assumed: the new keys
are present in the committed `book/src/reference/locale-seed-42.json`, so the
paths have index entries and `git diff --exit-code` against them can fail.

## Follow-ups (promoted from the campaign's scratch, which dies with it)

Open items from both stages. Nothing below is a stage-2 defect.

- **Walk-scale reachability is a third predicate.** "Is there water within a
  few minutes' walk" is neither point-presence (this campaign) nor 110 km cell
  availability (`river_proximity`). Not built here.
- **Split lakes out of `WaterKind::River`.** Registered as
  `MAP-river-lake-conflation`; predates The Ford, deliberately not repaired
  by it.
- **The generic primitive has other callers waiting.** Coastline, scarp,
  treeline and roads all band a signed distance field identically
  (`MAP-distance-banded-features`). The Ford builds the primitive; nothing
  else calls it yet.
- **`lab_is_fordable_cell` becomes a second opinion, not the answer.** Once the
  transverse model reaches consumers, the cell-scale drainage proxy and the
  real crossable-profile reading coexist. Do not unify them — the lab's
  duplicate detectors are deliberately independent re-derivations
  (`windows/lab/src/metrics.rs:6425-6433`).
- **Seasonality** (`MAP-seasonal-band-stage`) is unlocked but unbuilt; keep
  discharge an argument to the band functions, never baked into a stored
  per-cell width.
- **A room can now render a contradiction, and four client fixtures ship one.**
  Seed 42 room `750518284` reads `fields.water == "river"` while
  `channel_distance` is `-0.0509` rad — ~2.7 canonical cell edges, ~20× its
  own outermost band edge (`0.00259`). The grid water field and the channel
  network already disagreed; stage 2 is what makes the disagreement visible in
  one document, and it is now committed in `snapshot-seed-42-walk.json` and
  three siblings. Suspected cause: `build` dropping single-cell runs, so a
  river cell gets no polyline (stage 1 measured 39 of 700 seed-42 river cells
  carrying no polyline). **Stage 3 owes a consumer rule, or the network owes
  those cells a line.**
- **Nothing pins the vertex-selection rule against an independent reference.**
  Making the document and the classifier share one selection path is right,
  and it means a wrong vertex would be wrong consistently everywhere. The
  reference has to come from outside: a hand-computed nearest vertex for one
  known room.
- **`crossing_between` can call two different rivers one crossing.** It
  compares the signs of two readings that `nearest_line` may have taken from
  *different* polylines. Untouched by stage 2, but the rebuilt instrument
  leans on it harder: the pair is now a full room edge apart instead of two
  half-edge probes. `nearest_line` publishes the line index precisely so a
  consumer can tell these apart; the gate does not use it.
- **Two definitions of "the reach" coexist.** `wadeable` reads each room's own
  `band_edges[0]`/`cell`; every attribution column in the H2-4 measurement
  reads the transected vertex's. They agree today; nothing asserts they must.
- **The spurious-flip locus is documented, and the fraction is dominated by
  it.** Of 341 transects, §8's conjunction holds on 324 (95.0%) while only 123
  have any crossing at all. What separates them is the crossing gate — the
  sign change plus the requirement that a room stand inside its own bank edge
  — not the crossability criterion. A later stage quoting "the fordable
  fraction" is mostly quoting the gate.

### Deferred minors, with their sites

Each was raised in review, judged not to block, and would otherwise die with
the scratch:

- `windows/locale/tests/water_reading.rs` — `grid_level == ctx.globe_level()`
  compares the field to its own source (tautological; §2 item 5).
- The width positive control's expected value restates the clause from the
  same inputs (§2 item 6), and it filters on `room_edge(&a.home)` while
  `crossing_between` uses `room_edge(a).min(room_edge(b))` and each room's own
  `band_edges[0]`: 98 of 98 today, one-sided-fragile under terrain drift.
- The `usable >= 200` floor no longer guards what its comment says — the only
  remaining drop cause is documented unreachable, so it now guards network
  shrinkage rather than selection bias.
- `room_edge(&transects[0].home)` indexes element 0 before the emptiness
  check, so an empty population panics in the print rather than failing the
  informative floor.
- `describe` now pays roughly a thousand transcendentals per call (144
  polylines × `acos` per segment) **inside the cached path** — about
  0.2–0.5 ms per room, tens of milliseconds per surrounds build. Bounded, not
  a blocker; `make ci` on a quiet box arbitrates, and the seam if it moves is
  `RoomMeshMemo`, not `ChannelNetwork`.
- `windows/locale/tests/water_reading.rs:861` assumes a fixture line ends in
  `}` (`&old[..old.len()-1]`) — would fail loudly rather than wrongly;
  `strip_suffix` is the fix.
- The disclosure roster repeats per room: `session-seed-42.json` grew
  65,649 → 67,817 bytes (+3.3%), about 271 B per embedded room. Inherent to
  the mandated shape (scene emits one per document, locale one per room).
  Recorded because nobody had the number.
