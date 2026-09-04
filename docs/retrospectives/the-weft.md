# Campaign The Weft — retrospective

**Merged:** pending (queued 2026-09-04) · **Ledger:**
`docs/superpowers/ledgers/2026-09-03-the-weft.md` (16 entries) · **Chronicle:**
[the-weft](../../book/src/chronicle/the-weft.md) · **Decisions:** 0686, 0687

## The headline: ten defects in the controller's spec and plan text, and approximately zero in implementer code

Every one of the ten was a **confident claim about code nobody had opened**.
Not a judgement call that went the other way, not a design that aged badly — a
sentence asserting something about an enum, an API, a crate graph or a
distribution, written from reasoning, travelling downstream as an instruction.

1. **A named enum variant that does not exist.** The design's branch table said
   to emit a region extent. The enum has exactly one variant, `Point`. Worse,
   the file already carried **four** doc corrections saying so, each cleaning up
   an earlier draft that had assumed otherwise; this spec was the fifth
   instance of a documented, named trap.
2. **A wrong `VertexMap` API.** Named the wrong file, never named the
   constructor, and wrote a range check against a value the accessor returns by
   reference — a type error as specified.
3. **A type-audit class that does not exist.** The brief instructed a tag whose
   class the tool does not define. The implementer deviated; the reviewer
   independently confirmed the deviation was right.
4. **A domain/window layering error.** The prose-rendering method was specified
   on a type in a *domain* crate. Domains draw world state; windows render it.
   No domain in the repo has a rendering method, and this would have been the
   first.
5. **A circular crate dependency.** Task 5's signature took a context type from
   `windows/locale` inside `windows/worldgen` — but locale already depends on
   worldgen, so the reverse edge cannot exist. This would have blocked the task
   outright.
6. **A doubled derivation.** Two pool builders each constructed the same index
   and field pack independently.
7. **A two-dimensional noise function where the sphere needed a
   three-dimensional one.** The plan named the kernel's 2D field for a
   position that is a 3D point, which forces a latitude/longitude projection —
   a seam at the antimeridian and distortion at the poles. That is not an
   aesthetic problem: it would have surfaced as a spatial anomaly in this
   campaign's own autocorrelation statistic and been attributed to the
   mechanism rather than to the projection, poisoning the measurement the whole
   campaign exists to take.
8. **Inverted contextuality endpoints.** §5.2 said "0 = wallpaper, 1 =
   speckle"; the implementation mixes `c·macro + (1−c)·noise`, so it is exactly
   backwards, and §5.6 of the same document used the correct sense. The spec
   contradicted itself.
9. **A cost estimate that was a perimeter, not a delta.** The movement tax was
   estimated at ~84 facets per step. Measured: **21** straight-line, 41
   diagonal, at three radii. The estimate had computed the whole window
   boundary rather than the change across one step — and did so with corners
   double-counted, so it was not even the perimeter it meant to be.
10. **A discharge claiming one call site where seven exist.** A ruling recorded
    that widening a kind enum would touch one exhaustive match. A reviewer
    found seven, including one in the *same file* the original scan read, three
    lines below the line that was read.

**Every one was caught by grepping the code immediately before dispatch —
never by re-reading the document.** That is the whole finding. Re-reading
checks a claim against the model that produced it; the model is what was
wrong. The two defects that reached furthest (the non-existent variant, the
inverted endpoints) both survived multiple readings of documents that were
internally consistent.

The counter-observation is worth stating too, because it is the reason this
matters: **implementer code was near-clean throughout**, and where implementers
deviated from a brief they were right, and said so.

## A sweep reported complete was wrong three rounds running

Three separate times, a correction sweep was reported as finished and a further
site had survived:

| round | the sweep | what survived |
| --- | --- | --- |
| Task 1 | a stale count of reserved fields in a module header | the *re-derived* count contradicted the list in the same header |
| Task 7 | replacing the speckle/wallpaper vocabulary after the endpoints were found inverted | a further section still carried the old sense |
| Task 9 | correcting an overstatement of what the coherence statistic shows | the campaign's own ledger entry, which then had a *second* entry claiming it had been fixed |

The third is the worst, and it is a distinct defect rather than a repeat: the
fix-round record asserted it had edited a paragraph when the diff was purely
additive — the correction was appended *below* the sentence it claimed to have
rewritten. A reader checking whether the campaign had caught the overstatement
would have found a claim that it had.

**What caught it each time was an instruction to *re-read*, not an instruction
to fix the named sites.** Those are different instructions and they produce
different actions: "fix these three" is discharged by fixing three, and reports
success honestly. "Re-read everything that says X" is discharged only by
looking. If the sweep matters, the instruction must name the *property*, and
the report must list what it examined rather than what it changed.

## Preregistration froze the statistic but not its weights matrix

The frozen measurement named Moran's I. It did not name the **adjacency** the
statistic is computed over, and for a spatial statistic the weights matrix *is*
part of the statistic — the same indicator over two different adjacencies is
two different measurements.

That gap was not hypothetical. The first implementation weighted by adjacency
on the coarse point lattice, where one step spans roughly 106–127 facets: two
to twenty-three correlation lengths, depending on the kind. At that lag, **both
hypotheses the statistic exists to discriminate between predict the same
thing** (near zero), which is derivable from the published correlation-length
constants before any world is built. A statistic whose competing hypotheses
agree is not a test.

The correction was made on that power argument, before any number was
interpreted, and the discarded readings were published in three places rather
than deleted — which is what made the correction checkable at all. Two
corroborating facts made it clearly a re-scale rather than a rescue: the
discarded and corrected statistics rank the four kinds in the *same* order, and
the mismatch was measured (correlation length in facets against lattice
spacing) rather than inferred from the numbers being unwelcome.

**Next time: a preregistration that names a spatial statistic must name its
adjacency, its population, and its lag in the same breath.** The population
half of this was learned separately and correctly (see below); the adjacency
half was not.

## An empty diff is a claim about a command's reach

`make rebaseline` reported that only the timings row had moved. Two
golden-pinned tests were failing at that moment.

The pinned prose lives in a byte-golden fixture, which `make rebaseline`
excludes **by design** — a golden is an assertion, not an output, and
regenerating it silently is exactly what the exclusion prevents. So the report
was true and meaningless: the command cannot reach that file, and its silence
was a statement about its own reach, not about the tree.

**Silence needs a positive control.** Before reading an empty diff as "nothing
moved", establish that the command can write the thing you are asking about.

## A fix can invalidate the measurement that justified it

One constant in the overhang recipe had been chosen by analogy and never
checked against real ground; measured, it saturated its term above 0.95 across
most of the land, so half the recipe was doing nothing. Replacing it with the
world's own ruggedness ceiling was clearly right — and it moved the overhang's
own occurrence count by **−24.5%** in the same change that measured the defect.

Three committed tables then held numbers taken before the fix. They were caught,
but the general shape is worth carrying: **when a correction moves the quantity
the correction was diagnosed from, every number derived from that quantity is
stale, including the ones in the same commit.**

## Two reviewer numbers lost to measurements, and the implementer was right both times

- A review estimated a duplicated per-world build at **60–160 ms** and
  attributed 10–20% of an added cost to it. The implementer measured it at
  **~2.3–2.8 ms**; an independent re-measurement got ~2.5–3.0 ms. The estimate
  was off by 20–60×, and the attribution had to be struck. The fix was made
  either way, because it was correct independent of the number.
- A review called a doc cell stale; it was correct as written.

In both cases the implementer declined to defer to the reviewer's authority and
measured instead. That is the behaviour to keep. A review finding is a
hypothesis with a source, not a fact, and the cheapest response to a *number*
in a review is to reproduce it.

## A vacuous guard, commissioned by the controller

The guard meant to prove that derived features never reach the record read a
**fixture off disk**. No derived-feature code ran in it at all, so it could not
have failed from the surface it was guarding. It was replaced with a live
300-step walk through dense derived ground.

Proving the replacement could fail took two attempts: the first mutation was
silently absorbed by the record's own duplicate detection and passed falsely.
**A mutation the system deduplicates is not a mutation** — the proof needs a
perturbation the system is obliged to notice.

## Where the effort actually went

Task 1 — the smallest task in the campaign, a single additive field on a type
that is never serialized — **landed correct on the first attempt and then spent
three commits and two fix rounds on prose**, all of them on a module header's
count of reserved fields — wrong in two consecutive commits, by two different
authors, with nothing checking it. The ruling was to stop asserting a cardinal
count and name the fields instead: a count is a claim that decays silently,
while a list is checkable against the struct standing beside it.

That ratio — code right the first time, the header wrong twice — matches the
headline finding rather than contradicting it. The defects in this campaign
were overwhelmingly in *sentences*, wherever they were written.

## What worked, and is worth keeping

- **The negative control earned its place.** It fired exactly once, on the
  whole-sphere population, outscoring the sign case — and that firing is what
  exposed the eligibility confound and produced the population amendment. A
  control that never fires teaches nothing; this one paid for itself in a
  single reading.
- **The population amendment landed before the measurement**, in the frozen
  document, with the confounding figures stated inline. The ordering is the
  whole legitimacy: the same edit after unblinding would have been a rescue.
- **The falsification shipped as the headline.** No constant was moved on the
  strength of the reading, and the test that pins the result asserts the
  relations that survived while staying silent about the one that did not — so
  a future change cannot find a green assertion protecting a falsified claim.
- **A hypothesis with no population was withdrawn before it could pass
  vacuously.** The rumor invariant was frozen alongside the others, and the
  self-review found that the campaign builds no rumor producer, so a test would
  have passed by finding nothing to check. It was deferred *with* its producer,
  and the frozen document was amended immediately rather than at close, so the
  two documents agreed for the campaign's whole life.
- **Implementers improved on briefs repeatedly** — a scale-independent
  autocorrelation check for the control kind, a measured slope constant in
  place of an analogized one, a corrected movement-tax formula verified at
  three radii.

## Estimate deltas

| | planned | actual |
| --- | --- | --- |
| tasks | 10 | 10, with 13 fix rounds across them |
| preregistered hypotheses | 4 | **3** — one withdrawn before measurement, with its producer |
| hypotheses upheld | 3 of 3 predicted | **2 of 3** (density holds; coherence passes as a construction-validation; legibility falsified) |
| movement tax per step at radius 10 | ~84 facets | **21** straight-line, 41 diagonal |
| epochs | 0 | 0 |

## Do differently next time

1. **Grep the definition before writing the sentence.** Every one of the ten
   controller defects was one command away from being caught, and the command
   is always the same shape: open the enum, the signature, the manifest, the
   distribution. Do it while writing, not while reviewing.
2. **A sweep instruction names the property, not the sites.** "Fix these three"
   is honestly dischargeable by fixing three. "Re-read every place that claims
   X, and list what you examined" is not.
3. **Preregister a spatial statistic's adjacency, population and lag, not just
   its name.** The weights matrix is part of the statistic.
4. **Read an empty diff as a claim about reach.** Establish that the command
   can write the artifact before treating its silence as evidence.
5. **After a correction, re-derive every number downstream of what it moved** —
   including numbers committed in the same change.
6. **Reproduce a reviewer's number before acting on its magnitude.** Two lost
   to direct measurement this campaign; the fixes were right anyway, the
   attributions were not.
7. **A guard must be able to fail from the thing it guards.** If it reads a
   fixture, it is testing the fixture. Prove the failure with a mutation the
   system cannot deduplicate away.
8. **Prefer a list to a count in a doc header.** A count decays silently and
   nothing checks it.

## Deferred minors, and where each landed

| item | disposition |
| --- | --- |
| no committed fixture is pinned at a facet carrying a derived feature, so the rendered sentence has no fixture-anchored guard | registry row filed; live-session tests do cover the mechanism |
| the brief's occupation fields at a settlement's facet name the *containing vertex's* occupation, not the settlement | registry row filed; inherited, and nothing renders those fields today |
| the rumor producer, and the region invariant withdrawn with it | registry row filed; restoring the invariant is that campaign's first task |
| a ruin still reports a point, not ground | registry row filed by Task 3; the reason is written beside the enum |
| `OVERHANG_ABUNDANCE` was calibrated against the pre-I2 slope recipe, which then moved that kind's occurrence count **−24.5%** (1,117 → 843) in the same commit — the shipped frequency is not the frequency the constant was tuned for | recorded in the ledger; a future task should re-derive `OVERHANG_ABUNDANCE` against the corrected `GORGE_SLOPE`-based recipe |
| the coherence guard's own threshold was chosen after unblinding and is not a frozen floor | corrected in the metric's documentation, which now says so rather than claiming a preregistration that did not happen |
| the walking band and one architecture chapter still quote a pre-lattice-change facet size (~1.7 km rather than 1.126 km) | pre-existing, unrelated to this campaign; noted, not fixed here |
