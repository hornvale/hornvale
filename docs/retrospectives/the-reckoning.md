# The Reckoning — retrospective

**Closed:** 2026-07-17 (pending merge). Process lessons, not product.

## Four tests shipped that asserted nothing — and the gate was green for all four

Every one was caught by **review, not by the gate**:

1. A byte-identity test with an empty `expected` list, run on a seed that draws
   **zero** moons — `assert_eq!(0, 0)`.
2. A Luna calibration test that **recomputed the formula inline** instead of
   calling production, so reverting production to the linear map left it
   passing. It pinned the arithmetic, not the model.
3. A safety test that could not fail for the reason it was cited for.
4. A vacuous albedo bound.

The pattern is one thing, and it is not carelessness: each assertion was
written next to the **belief** rather than against the **code**. A test author
who knows what the answer should be can write a test that agrees with them
without ever touching the thing under test. The gate cannot see the difference
— a tautology passes exactly like a proof.

The fix that worked, and the one to institutionalize: **mutation-verify every
test. Break the thing it pins, watch it fail, revert.** It is thirty seconds
per test and it is the only evidence that a passing test means anything. The
campaign's own containment battery — the most load-bearing test it has — was
mutation-tested by a reviewer (force `age` into `luminosity`, confirm the guard
fails), and that is the reason to trust it. The correlation across the whole
campaign is blunt: **reviewers who *measured* found real defects; reviewers who
*reasoned* did not.**

## The controller's own estimate was falsified by the measurement it commissioned

The sharpest lesson here is about me, and it should be recorded that way. I
argued that the eclipse-geometry correction's `O(i³)` perturbation was `<1e-9°`
and would likely vanish under 8-digit quantization, making the "total" epoch
free in practice. It is **0.0185°** — four orders of magnitude above the floor —
and it moved **260 of 627** all-impact worlds.

The error was specific and worth naming rather than filing under
"overconfidence": I generalised from the curve's **peak** (`u = 90°`, where the
exact and small-angle forms agree *identically*) to the whole curve. That is a
reasoning error a moment of arithmetic would have caught, and no amount of
further reasoning would have.

**Task 6 existed precisely because nobody had the number — and it earned its
place by overturning the person who wrote the brief.** A measurement task whose
only defensible outcome is confirming the controller is not a measurement task.
This one was allowed to say no, and did.

## Measuring the value is not measuring the mechanism

An implementer measured the inner-moon impact rate correctly — 0.6813 under the
then-current linear map — and then narrated a **falsified** cause in a
confident, permanent code comment: "mutual spacing pushes the innermost moon
up." An A/B showed spacing pushes it *down*, the tide cap pushes it *up*, and
the two cancel to ~0.0001. The real cause is an order statistic with no physics
in it: the minimum of `k` uniform draws averages `1/(k+1)` of the range, so the
innermost moon's distance fraction averages ~0.32 with every constraint
switched off — which a linear map reads straight off as its capture chance.

The number was right. The explanation shipped next to it in the source was
wrong, and would have been inherited as fact by everyone who read it after. The
project's standing lesson — *falsified predictions are mechanism discoveries,
and reviewers must measure* — bites in a new place here: **having measured the
value confers no license to assert the cause.** Those are two measurements, and
only one of them was taken.

## Related: a plan's unmeasured gloss became an unreachable assertion

"The innermost moon is almost certainly an impact child" was the **plan's**
prose, not the spec's, and it assumed the innermost moon sits near the distance
floor. The threshold derived from it — `inner_rate > 0.7` — was arithmetically
**unreachable**, because at the floor `p_capture` clamps. A distribution claim
nobody measured propagated into a test nobody could pass. **Plans should not
state distribution claims they have not measured**; the spec, notably, did not
make this mistake — it said only "weighted by distance."

## What to carry forward

- **When you fix a class of bug, grep for the class, not the instance.** The
  eclipse small-angle defect was fixed in two functions; the third,
  `series_returns`, sat 350 lines away, live in the almanac, affecting ~10% of
  mooned worlds — and was found only because a reviewer swept for siblings. It
  survived because seed 42's *innermost* moon is prograde, so no committed
  golden ever exposed it. Goldens pin the paths the demo seed happens to walk.
- **A drift check cannot catch an omission upstream of the generator.** A
  `streams.rs` `pub const` that never reaches `stream_labels()` is invisible to
  CI: the artifact regenerates from the same incomplete function, so nothing
  disagrees. It bit **three times** this campaign before a cross-check test
  (`every_stream_label_constant_is_published_in_stream_labels`) closed it. This
  is a general shape — a generated artifact verifies its generator's *output*,
  never its *coverage*.
- **The absorb cadence slipped, and got away with it.** Main moved **68
  commits** during execution (rift-and-fit, the-terminator, an ECS migration,
  an AWS regen). The doctrine is absorption at every plan-stage boundary; this
  campaign absorbed once at the start and once at close. It merged cleanly —
  but that was **luck of the axes** (terrain vs astronomy), not diligence, and
  the one collision that did land was semantic, not textual (below).
- **`make preflight` cannot score the collision that matters.** A parallel
  session's campaign, *The Faces*, shipped `scene/moons/v1` deriving moon radius
  at an **assumed constant Luna density** — the exact assumption this campaign
  existed to retire — and closed hv#4, whose title is the *parked sibling's*
  scope. The merge was textually clean and preflight was green. It surfaced only
  because someone read the incoming spec. Preflight scores filenames and row
  IDs; **it cannot score "main just shipped the assumption you are here to
  kill."** No mechanization proposed — this is a reason to read parallel
  campaigns' specs at absorption, not a tooling gap with an obvious fix.

## Follow-ups

Registered, none blocking, none correctness bugs in shipped behaviour:
**FU1** publish `pub fn formation_name` from astronomy — `windows/scene`'s
`formation_word` duplicates the `Formation`→text mapping (the `is_icy`
precedent). **FU2** extend the 32-seed pin-isolation sweep to
formation/density/age (spec §8 asked; only seed 42's full-system equality
covers them today). **FU3** decide the ZAMS back-derivation — implement or
drop; it is a formula, not an API. **FU4** the captured-moon age bound
`[0.05, 0.95]·planet_age` is an approximation, not a derivation (a captured body
can be *older* than its planet — Triton likely predates Neptune's final
assembly). **FU5** `maria_fraction` is composition-blind — an icy moon can carry
basaltic maria; damped at 0.3, not solved. **FU6** the census is stale for
**633/1000** seeds — accepted debt, for a future campaign to batch.

**FU7 — needs Nathan's call, and it is not this campaign's debt.**
`book/src/gallery/the-gods-seed-42.md`, the Year-1 exit demo, quotes almanacs
"verbatim" that no longer exist. This campaign staled its eclipse periods
(423.36 → 3111.86) and refreshed nothing else, because the page was **already
badly stale at this campaign's branch point**: it describes 182/172 settlements
across four peoples with a kobold flagship, while the committed almanacs held
62 (now 60/65) across **two** — no kobold or bugbear pantheon forms at seed 42
any more. Its stated comparison has lost half its subject. A dated notice now
says so on the page. Applying this campaign's own lesson — *grep for the class,
not the instance* — found a second: `the-meeting-seed-42.md`, the Year-2
capstone, quotes a kobold settlement of 522 souls and gods named in the
pre-*Speakable* style. It is **not** this campaign's staleness (it quotes only
celestial-body gods, which did not move) and is left untouched, but it is the
same rot.

**The lesson is the finding**: two flagship gallery pages have lagged merged
reality across several campaigns' closes and no check caught either — the
*generated* almanacs regenerate and drift-check green while the hand-written
pages quoting them rot silently beside them. **A drift-checked artifact confers
no freshness on the prose that cites it**, and "quoted verbatim from the
committed almanac" is exactly the sentence that stops being true without
anything turning red. This is the Confidence Gradient's own sharpening (*the
drift-check is not the checkable part — the anchor is*) reappearing one level
up, in the book about the world rather than the world. A cheap check exists if
someone wants it: assert that a page's fenced quote is a substring of the
artifact it names. The repair of the pages themselves is a re-founding, not a
sweep.

## Confidence Gradient

**One bet moved.** The phenomena/scene-seam bet gained a mirror image of the
caveat *The Lens* added to it. The Lens found data crossing the seam that no
consumer ever drew; this campaign found the inverse — a consumer drawing a
distinction the producer never made (`bright-icy` firing off a hash-derived
albedo, for a model with no concept of ice), and briefly **two answers for one
quantity** (moon radius) on either side of the seam. Re-scored in
`book/src/open-questions.md`. No terrain, population, economics, or
historiography bet was touched.

## Deferred at close (Nathan's call)

- **The AWS census regen was declined.** The campaign closes with the census
  fixtures knowingly stale for 633/1000 seeds and main red on the census tests
  until a future campaign batches the spend. This is the authorized state under
  the project's tolerated-lag policy, applied — unusually, and better — to a
  **measured** number rather than an estimated one.
