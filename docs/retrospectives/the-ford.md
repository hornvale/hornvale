# Retrospective — The Ford (stage 1)

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-ford.md): rivers stop being
110 km wide because they stop being faces, and a point's relation to a river
becomes the banding of a signed distance to a polyline network.

**This is a stage retrospective, not a campaign one.** The Ford's spec §9 lays
out five stages; only the first has shipped. Stages 2 (`locale/room/v3`),
3 (riparian conditioning) and 4 (scene emission) are separate plans, and
`MAP-ford-subcell-water` stays `spec'd` until they land. Two of the five
preregistered hypotheses — H3 (the ford exists) and H5 (the conditioning null)
— were not measured here because they need consumers stage 1 deliberately did
not move.

## The readout, and the one repair that followed it

Preregistered in spec §10 before any code, measured in Task 6, then moved once
by an authorized post-unblinding repair in Task 6b.

| axis | preregistered | Task 6 | Task 6b |
|---|---|---|---|
| H1 channel-land-fraction (seed 42) | [0.005%, 0.5%] | 0.025931% **CONFIRMED** | 0.025938% |
| H2 channel-connectivity | ≥ 0.95 | **0.9236 / 0.8983 FALSIFIED**, 4/64 clear | 1.0, 64/64 |
| H4 band-monotonicity | ≥ 0.99 | 1.0000 **CONFIRMED** | 1.0000 |
| H4 un-truncated companion | (not preregistered) | 17/64 ≥ 0.99 | 64/64 |

The repair was a design change **after unblinding**. It was surfaced by the
measuring task as a candidate and explicitly *not applied there*; it was
Nathan's call; it landed in its own commit (`e938edc6`) with its own
before/after; and the original falsification is kept — in the chronicle, here,
and in the metric's own doc. That sequence is the only reason the number is
readable. Task 6's own report contains the sentence that made it possible:
*"it is a design change to a measured feature after unblinding, so it is the
controller's call and belongs in its own commit with its own before/after —
not smuggled into the readout that found it."*

## 1. A preregistered axis can become a constant

`channel-connectivity` now reads 1.0 on all 64 probe worlds, and the honest
statement is that **it is no longer a measurement**. Both of its failure
branches are unreachable:

- The join-crossing branch is degenerate **by construction** after the repair:
  the tributary's terminal vertex is *assigned* the trunk's vertex, so the two
  polylines are exactly coincident there and no gap exists to fall through.
- The owner-is-`None` branch **never could fire, even before the repair**.
  `build` pushes a target cell before the `claimed.insert` check, so a cell
  with a river downhill is always a non-final vertex of some kept run. The
  measured `continues_but_unowned` count is 0 in **both** arms.

So the axis that falsified is now satisfied by construction, and 1.0 carries
almost no information. This is not an argument for deleting the metric — it
still discriminates a regression that stopped anchoring — but it is an
argument against reading the post-repair number as a confirmation of anything.
A preregistered floor cleared by making the failure mode structurally
impossible is a different result from one cleared by the world behaving well,
and the two look identical in a table.

**The generalisation worth keeping:** when a repair is designed *against the
metric's own failure mode*, check afterwards whether the metric still has a
failure mode at all. Nothing in the preregistration discipline asks that
question, because preregistration protects against retuning to rescue a
prediction and this is a different move — not retuning the constant, but
removing the phenomenon.

## 2. Right measurement, wrong attribution — twice, on the same phenomenon

The un-truncated H4 companion had 14 violations across three worlds. Task 6
read them as a transect ray entering a **neighbouring** river's water, and
said so with evidence: it instrumented every failure and showed the *winning
polyline changes* at exactly the reversal point. **Task 6's independent
reviewer reproduced that instrumentation and independently confirmed the same
reading.**

All 14 of 14 actually attribute to the originating line's own **confluence
partner** — the trunk it should have joined, not an unrelated river.

The measurement was never wrong. Every digit reported was correct: a different
line does win, and it is never the originating line. The interpretation
conflated *"a different line wins"* with *"an unrelated river"*, and at a
confluence those are precisely the two things that are not the same. It went
undetected because the evidence for the wrong reading is also evidence for the
right one — an instrument that reports "which line won" cannot distinguish
"some other river" from "the trunk this one flows into" without being asked to.

It was caught only because the repair moved the number from 17/64 to 64/64. If
the repair had been rejected, both readings would still stand today, both
signed off by two independent parties.

And the repair's own stated *reason* was a third error in the same family:
Task 6b justified the improvement by continuity **at** the switch, which says
nothing about `|d|` **after** it — and in 11 of the 14 cases the winning line
had already changed before the reversal. Right conclusion, wrong premise,
three times in a row about one phenomenon.

**The lesson:** a confirmed attribution is not confirmed by a reviewer running
the same instrument. Independent reproduction establishes the *number*, and
inherits the *reading*. The only thing that separated the two here was an
intervention that changed the world.

## 3. An anti-vacuity guard can itself be vacuous

The channel golden pins per-vertex meander displacements, and it carries an
anti-vacuity companion asserting that not all displacements are zero — the
guard against a fixture that would stay green if the meander field were
removed entirely.

That guard read **31 of 46 rows moved**. Only **21** carry a real
displacement. The other ten are *anchored* vertices, whose displacement is
exactly zero by construction — but the computed value is
`acos(dot(p, p)) ≈ 1.5e-8` rather than `0.0`, with arbitrary sign, because
`dot(p, p)` for a normalized `p` is not exactly 1. The guard's float
comparison counted that residue as movement. **32% of the anti-vacuity guard
was satisfied by noise**, and the comment beside it claimed the opposite about
anchored rows.

Three things about how it was found:

- It was found **only by fixing an adjacent defect** — closing the golden's
  sign gap surfaced it. Nothing was looking for it.
- The fix commit for the *previous* round introduced a fresh instance of the
  same class: a committed comment asserting "36 of 46" where the guard's own
  parse gives 31 (36 counts five `-0` rows the `f64` parse does not). The
  round convened to fix a counting error produced a counting error.
- The implementer's own mutation proof of the golden was **not
  distance-preserving** — `normalize(2b - t)` perturbs magnitude in the 8th
  digit, so the *old* fixture reddened on it too and the proof established
  nothing about the gap it claimed to close. The re-reviewer built a genuinely
  isometric mirror and showed the old fixture was blind (green) while the new
  one flips sign exactly. The gap was real; the recorded evidence for it was
  not.

This is *fixing a defect grants no immunity to its neighbour* at three
storeys — the shape [The Hollow](./the-hollow.md) named when its own repair
committed both defects. The transferable rule is narrower and mechanical: **a guard
that compares a computed float against zero to prove "something moved" will
count float residue as movement.** Short-circuit on exact input equality
(`base == placed`) before computing the derived quantity, rather than
tolerancing the output.

## 4. The defects were in the controller's own text, again

Seven defects found across seven tasks and four fix rounds trace to the spec,
the plan, or a task brief — text this session wrote and dispatched to agents
who executed against it:

| the text asserted | reality |
|---|---|
| mean L6 cell edge is `0.0276` rad | measured `0.018886` — 46% off, and the tripwire built on it had 2× headroom where the real figure gives ~15× |
| a `bands_are_monotone` test, written out | did not compile, **and** was vacuous once it did |
| a pin-isolation test, written out | unfalsifiable — and its first replacement inherited the flaw |
| type-audit verdict classes, named | the named classes do not exist |
| "the downhill graph is already retained" | a dropped local in `globe.rs:421`; Task 1 owed the retention |
| "No census regen is authorized. Nothing here requires one." | three new lab metrics change the census schema; 43 lab tests fail on that one cause and `make gate` cannot be green without a refresh |
| channel width in metres | no length scale exists in this codebase — no planet radius anywhere |

Two of these are worth separating from the rest, because they are not
carelessness.

**The `0.0276` constant and the "already retained" claim are the same
failure.** Both are *readings of the codebase* stated as fact in prose, made
from outside the code, by a party that could not run it. The spec caught one of
them itself and said so in the text — §5.2 carries the correction and the
sentence *"An earlier draft of this spec said 'already retained', which was
wrong, and the correction is not free"* — which is the right behaviour and is
also evidence that the class was known and still produced six more instances.

**"No census regen is authorized" is worse than wrong; it is a prohibition.**
It appears in the spec §7 and again in the decision ledger's carve-out watch
as *"Do not run one."* Adding a lab metric changes the census schema; that is
a known, documented consequence in this project's own memory. The spec
asserted the negative without checking it, and the assertion had teeth — it
would have blocked the only action that could turn the gate green, had Task 6
not escalated it as a carve-out rather than obeying it.

The mitigation this campaign actually demonstrates is the one it used: **every
one of these was found by an implementer or reviewer measuring the premise
rather than executing on it.** The measured cell edge, the unfalsifiable test,
the missing census authorization — all surfaced as *reports back*, not as
silent compliance. The instruction that produced that behaviour is worth
keeping in briefs verbatim: state the measurement the brief is built on, and
say it is to be re-derived rather than trusted.

## 5. The close task was the first full-workspace gate

Tasks 1–6 ran scoped tests by explicit instruction (`cargo test -p
hornvale-terrain`, `--test <name>`), with `make gate` deferred to Task 7. That
is the right cost trade and it has one predictable hole: **workspace-wide
source-scanning lints are invisible to every scoped run.**

The gate came up red at Task 7 on exactly that — Task 6b's
`a_tributary_mouth_sits_exactly_on_the_trunk_vertex_it_joins` loops seeds
`[42, 7]` and carried no `claim:` shape tag (decision 0093,
`cli/tests/claim_shape.rs`). It is a one-line doc-comment, it is in the
terrain crate, and no amount of `-p hornvale-terrain` could have found it,
because the lint lives in `cli/tests/`.

Nothing here argues for running the full gate every task. It argues for
knowing which checks are *only* reachable from the workspace root — the
claim-shape lint, the layering and dependency-allowlist tests, the doc-drift
check, the heavy-tier token guard — and expecting the close task to pay for
them. A task that adds a `#[test]` with a seed loop should expect that bill.

## 6. What did and did not have to be regenerated

`make rebaseline` at close produced **no drift at all**: gallery, reference,
laboratory, audits, digest, domesday and the client fixtures were all already
current. That is the right outcome and it was not automatic —
`book/src/reference/` was stale by design from Task 3 (the stream manifest
gained `terrain/channel-meander`) and `docs/audits/` from the new `pub`
boundary items, and both were regenerated in the commits that staled them.

The census was the exception and it was expensive: the three new metrics moved
the schema, so a refresh on lefford was required, had to run on the **merged**
tree (main had already refreshed 80 of 194 shared columns), and had to land
**after** the metric-doc edits and the 36→31 count correction, or it would
have baked stale prose into the tracked `schema.json`. The sequencing was
recorded in the ledger and followed; it is the kind of ordering constraint
that is invisible to any check and costs a full 15-minute canonical run to get
wrong.

## Follow-ups (promoted from the worktree scratch, which dies with it)

- **Walk-scale reachability is a third predicate.** "Is there water within a
  few minutes' walk" is neither point-presence (this campaign) nor 110 km cell
  availability (`river_proximity`). Surfaced by the predicate × consumer
  cross-product at ledger #3. Not built.
- **Split lakes out of `WaterKind::River`** — registered as
  `MAP-river-lake-conflation`, predates The Ford, deliberately not repaired by
  it (spec §8).
- **The generic primitive has other callers waiting.** Coastline, scarp,
  treeline and roads all band a signed distance field identically
  (`MAP-distance-banded-features`). The Ford builds the primitive; nothing else
  calls it yet.
- **`lab_is_fordable_cell` becomes a second opinion, not the answer.** The
  cell-scale drainage proxy and the real crossable-profile reading coexist. Do
  not unify them — the lab's duplicate detectors are deliberately independent
  re-derivations (`windows/lab/src/metrics.rs`).
- **Seasonality** (`MAP-seasonal-band-stage`) is unlocked but unbuilt; keep
  discharge an argument to the band functions, never baked into a stored
  per-cell width.
- **Confluences join at valley resolution, not channel resolution** — the
  finding that falsified H2, now repaired at the vertex. The *bands* around a
  join are still built per-line; a point equidistant from two lines at a
  confluence takes the nearer line's bands rather than a merged profile.
- **`water_kind == River` and `transverse_at == Channel` disagree for over
  half of river centres, by design.** Measured L6 seed 42: 364/700 river cells
  read `Channel` at their own centre, 39 read `Dry`, and 39 (5.6%) have no
  polyline at all (dropped singletons). Any consumer that assumes the two
  agree is wrong; this is H1's denominator and it is stated in the metric doc.
- **`channel-land-fraction` returns `Number(0.0)` on a channel-free world**
  while its two siblings return `Absent`. Defensible but asymmetric; census
  consumers treat the two differently.
- **Two `pub` parallel `Vec`s** (`polylines` / `band_edges`) carry an
  unenforced length invariant, and `band_edges[line][nearest]` is indexed
  unguarded. A hazard once stage 2 hands this to a window.
- **The `lib.rs` re-export omits** `TERRACE_WIDTH_RATIO`, `MEANDER_FREQUENCY`,
  `MEANDER_OCTAVES` and `MEANDER_AMPLITUDE_RATIO`.
- **`regenerate-artifacts.sh` was only ever verified to its prefix** during the
  campaign (it failed at the census schema step until the refresh landed). It
  now runs to completion — verified at close, rc=0 — but the Domesday survey,
  the trope report and both digest renders had never executed before Task 7.
- **The network is now built at every `GeneratedTerrain::new`**, workspace-wide,
  including both `windows/worldgen` call sites. It looks negligible (L6 = 144
  lines / 681 vertices) and the `make ci` duration alarm is where it would
  show; nothing has flagged it yet.
</content>
