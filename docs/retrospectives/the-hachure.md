# Retrospective — The Hachure (2026-09-02 / 2026-09-03)

Process lessons only; the product story is
[the chronicle](../../book/src/chronicle/the-hachure.md).

## 1. Decisions-first belongs beside registry-first

The campaign read `plate.rs` and its implementation comments, inferred the
design's intent from them, and wrote a spec section calling a **ratified**
behaviour "a regression with a paper trail." The comments were accurate. They
describe what a fix round *did*; they do not say what was *ratified*. Decision
0287 had made the flat slabs structural on purpose, and decision 0196 held the
policy behind it.

`docs/CLAUDE.md` already names registry-first as the one habit that matters:
grep the idea registry before proposing or reopening anything. This campaign
is the argument for its sibling. **Before calling an existing mechanism a
mistake, grep `docs/decisions/` for it.** A mechanism that looks like an
oversight is the likeliest thing to have been decided deliberately, because
deliberate choices are the ones that survive looking wrong.

**What caught it was not a review.** It was `lexicon_guard`'s ratchet on the
word *cell*, refusing two probe files; chasing that refusal reached the lexicon
of place, which cites 0287 in its second paragraph. A vocabulary lint surfaced
a constitutional error in a design. Nothing in the design review would have —
the design was internally consistent, and its premise was the thing that was
false.

## 2. A measured figure derived from the consumer's arithmetic is not a measurement

The spec stated the terrain mesh's resolution as 256 samples around a great
circle. Nathan challenged the number and was right: 256 is the *chart's* width,
derived from the cube-sphere facet lattice. Terrain lives on the icosphere
vertex lattice, `10 · 4^L + 2`, which at level 6 is 40,962 vertices — **363**
around a great circle.

The correct figure was two function calls away for the whole campaign
(`Geosphere::new(6).vertex_count()`). What was done instead was reading the
consumer's derivation and mistaking it for the producer's resolution. This is
`evaluate-the-curve-not-the-constant` in its exact form, and it is worth
pairing with a sharper statement: **a resolution read off the code that
consumes data is a hypothesis about that data.** Run the producer.

The same error had a second life. The lexicon of place had been describing
facets as *triangular* and as *duals* of vertices since The Pavement made them
quads — the same conflation of two incommensurate lattices, sitting in the
book's reference section, published, for two days. Nobody had grepped the
producer there either.

## 3. Every band-shaped assertion measured the quantizer

Four test drafts passed against unfixed code before one discriminated. Each
asserted something about the rendered relief **band**, and the band is a lossy
quantization of the very quantity being refined — its rungs are hundreds of
metres wide, so within one ~110 km sample a real height ramp almost never
crosses one. The refinement was real (distinct heights per plate went from 1–4
to 612–3,860) and completely invisible in the observable every draft chose.

The lesson is not "write better tests." It is: **when the thing under test is a
refinement, check whether the observable is downstream of a quantizer coarse
enough to erase it — before writing the assertion.** The fix was to stop
guessing and measure first, then assert on the observable the measurement
named. This is a fifth instance of `tests-whose-input-collapses-to-one-value`
with a new cause.

## 4. Conserving against a defective baseline preserves the defect

Two Stage-2 candidate rules were measured against the existing rung-6 raster
and reported as −96.1% and +1383%. Both were meaningless, and the second was
written up as a falsification that it was not.

The rule that exposed it: today's river rendering covers ~0.98% of tiles at
**every** rung — 0.98 / 0.99 / 0.99 / 0.96, dead flat. A rasterized line must
cover `O(N)` of an `N × N` chart, so its cell fraction has to halve per rung.
The flatness was the signature of the defect, so the baseline *was* the bug.

Generalized as [decision 0677](../decisions/0677-a-line-carried-feature-conserves-its-length-not-its-rasterized-area.md).
The transferable half: **before conserving against a baseline, check whether
the baseline has the shape its own type demands.** A quantity that should scale
and does not is not a stable reference; it is a broken one.

## 5. A cited phrase is a claim about what that record compared

The conservation test's first form asserted decision 0121's phrase that "a
blend moves a value at most one band," and measured a move of two. 0121's
ruling was never wrong and is untouched — what did not survive was reading its
phrase as a bound on *blend versus snap*, which is not the comparison 0121
describes. The bound that is actually provable, and is what conservation needs,
is the convex hull one.

**When citing a record's phrase as a bound, state what two things it compares.**
A phrase lifted out of its comparison reads as more general than it is, and it
had already passed a spec review in that form.

## 6. An existing invariant found what the new tests structurally could not

Stage 2's wrap bug survived both of its own purpose-built tests and was caught
by the tile cache's byte-identity check — an unrelated invariant comparing a
composed plate against an uncached one. Both new tests drew a full-width plate
at origin zero, where the wrap never arises. They shared a blind spot; the
older test did not have it.

Pair with `reduced-fixtures-delete-defect-preconditions`. A test written
*alongside* a feature inherits the feature author's model of it, including the
gaps. Invariants written for something else are the cheapest source of
independence available.

## 7. Deferring a stage narrows the decisions it was going to justify

Nathan's close ruling deferred Stage 3. The spec asked for one amendment to
0196 licensing both interpolation (Stage 1, shipped) and invention (Stage 3,
now unbuilt). Writing it as specified would have ratified a capability nothing
implements and nobody reviewed — a live clause with no implementation to
constrain it, which is `a-clause-vacuously-satisfied` pointed the other way.
0676 licenses interpolation only and says in terms that it does not license the
deferred half.

**A deferral is not only a scope change; it is a re-scoping of every decision
the deferred work was going to carry.** Walk the ratification list against what
actually shipped, item by item, rather than writing the list the spec asked
for.

The inverse also applied: decision 0678 was written *despite* implementing
nothing, because it is Nathan's ruling rather than the campaign's choice, and
an unrecorded ruling gets rediscovered or relitigated. The test is not "did we
build it" — it is "whose call was this, and would its absence cost the next
campaign the same conversation."

## Deferred minors — where each one landed

The closing walk asks for a named committed location per deferred item, not
"it's in the ledger." All four were routed to registry rows during the
campaign rather than at close, which is the intended cadence:

| ledger entry | the minor | landed at |
|---|---|---|
| #15 | zoom is cursor-anchored and drifts the observer off screen over several rungs; Stage 0 exposed it and deliberately does not fix it | `CLIENT-zoom-drift-loses-the-observer` |
| #16 | the height refinement is real and the band ladder throws it away; the answer is colour within a band, not a finer ladder | `CLIENT-colour-carries-sub-band-relief` |
| #22 | the sim owes a named magnitude ladder (Strahler order) so consumers stop inventing their own | `MAP-stream-order-is-sim-truth` |
| #25 | Stage 3 itself, with the epoch it needs and the measurement that it costs no census golden | `MAP-coherent-detail-field` |

## A note on what the close could not read

This campaign's scratch (`.superpowers/sdd/`) did not survive the session
boundary — the branch was resumed into a fresh worktree, so the closing walk's
step 2A found no directory at all. **Nothing was lost**, and that is The
Cartulary's design working exactly as intended: every ruling, falsified
prediction, measured table and deferred minor above was read back out of the
*committed* ledger at `docs/superpowers/ledgers/2026-09-02-the-hachure.md`,
which a fresh worktree carries because git does.

Worth stating plainly because this is the first close in this campaign's
records where the pre-Cartulary practice would simply have failed: promotion at
close cannot promote from a directory that no longer exists, and the failure
would have been silent — an empty `ls` reads identically to a clean sweep.
