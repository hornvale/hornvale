# Retrospective — The Generalist

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-generalist.md): a sixth people,
authored as a broad-but-weak competitor, that held on two of three
preregistered predictions and exposed the third's own instrument as
under-powered before it could mislead anyone.

## The plan's task boundaries were wrong, and the owner absorbed that cost deliberately

The plan split authoring across three tasks along registry lines — biosphere
first, then psyche/society/perception, then articulation/lexicon. That split
cannot produce a green tree at any intermediate commit: `windows/worldgen`
enforces, as a load-bearing invariant, that every settling kind carries all
four registries *simultaneously* or none. The moment the first task adds a
biosphere row for a kind with no psyche row yet, the invariant is violated by
construction, and it stays violated until the last of the three tasks lands.

This was found before it caused confusion, not after: the owner was presented
with three options (merge the tasks into one commit, front-load a placeholder
psyche row, or accept two knowingly-red commits and move the review gate to
the task that restores green) and chose the third, deliberately bending the
project's own every-commit-is-green convention for that range. The choice was
right — merging the tasks would have destroyed the attribution the split
existed to preserve, and a placeholder row would have been a second kind of
lie — but the record of *why* two commits were red on purpose needs to live
somewhere a later `git bisect` will actually read, not only in a plan
document. It was written into the campaign ledger; it would have been cheaper
to write it into the two commit messages themselves, since that is where a
bisecting reader actually looks first.

**The generalizable version:** when a plan's task boundaries cut across an
atomic invariant, name the invariant and the boundary crossing *at planning
time*, not when the first task's implementer discovers it by watching a test
fail. The invariant here was easy to state in one sentence and was not stated
anywhere in the plan.

## The count-baked-into-an-assertion-string defect recurred five times, each caught by the grep that followed the last one

A hardcoded roster count — "four peoples," "five peoples," `psyche.len() == 8`,
a five-element `vec!` of names — living inside a test assertion, a doc
comment, or a prose sentence is invisible to the compiler and often invisible
to a targeted grep, because each instance uses different words for the same
number. This campaign found the pattern five separate times before this task,
each discovery prompting a slightly wider grep that found the next:

1. The plan named one assertion in `dissolve_equivalence.rs`. Review before
   the task even started found a second, three lines away, that the plan's
   own author had missed while looking directly at the first.
2. Task 3 found two more in a different file, missed by both the brief and
   the pre-task correction that had just widened the search.
3. Task 3 deferred a fourth instance — an eight-name roster plus a prose
   comment reading "five peoples plus three dragons, eight" — to a later
   task, on the reasoning that it belonged to a registry that task did not
   own.
4. The Tasks 2–4 gate review found a fifth instance in a *test comment*
   whose own text read "if a sixth people arrives, it goes here" — written by
   an earlier campaign as a considerate signpost for exactly this moment, and
   still missed by three successive greps because none of them searched for
   the word "sixth."
5. This task's own freshness sweep — grepping specifically for "four
   peoples" and "five peoples" across the whole workspace, not only the
   crates already touched — found a sixth wave: doc comments in
   `windows/worldgen` whose *specific numeric claims* (which settling people
   is longest-lived, how many time-horizon values are patron-reachable) had
   been silently false since an *earlier* campaign added the roster's fifth
   member, not this one. Nobody had looked at those comments since.

**What actually worked, eventually:** a search for the *shape* of the defect
(a bare integer or number-word sitting near "peoples," "settl-," or a
species-name list) rather than for a specific stale value, run against the
whole workspace rather than the crates a task believes it touched. A grep
scoped to "did my change break anything" will only ever find the instances
that predate the change by one campaign; it will not find the ones that
predate it by three.

## Several defects originated in the plan's own text, not in any implementation

Every one of the following was written into the plan or an early brief by the
same hand that later had to fix it, which is worth recording because it
contradicts the comfortable assumption that plan text is a safe reference and
implementation is where bugs live:

- The plan called a constructor `Seed::new(x)` that does not exist — the real
  type is a tuple struct constructed as `Seed(x)`. The campaign's own ledger
  notes this exact mistake had already ridden into three earlier tasks in
  this repository, unrelated to this campaign, which means it is a standing
  trap in how the type reads at a glance rather than a one-off typo.
- An early brief specified a heavy-test ignore reason that did not match the
  one canonical string a workspace-wide enforcement test requires verbatim.
- A niche's doc comment asserted a specific numeric contrast — "the widest,
  least-devoted curves in the roster" — that was only half true against its
  own authored values, on two of four axes, until a mid-campaign measurement
  caught it and the owner ordered a re-authoring.
- A comparison band definition in a later brief used elevation as a proxy for
  a specialist's stronghold axis, when that specialist's own doc comment
  named a different axis (moisture) as the actual hard-excluding one — an
  error that produced a real but misleading finding ("the specialist doesn't
  even win its own band") that had to be caught and re-attributed rather than
  reported at face value.

None of these were found by re-reading the plan. All four were found by
someone implementing against it and noticing the plan's claim did not survive
contact with the actual code or the actual measured data. **The lesson is not
"review plans harder"** — plans are written under the same uncertainty as
everything else, and catching all four of these at planning time would have
required doing the measurement the plan was deferring to implementation
anyway. The lesson is that a plan's technical claims are hypotheses with the
same standing as an implementer's, not a specification to trust by default,
and the campaign's discipline of measuring rather than assuming is the thing
that actually caught them — applied to the plan's own text, not only to the
code being written from it.

## The vacuity gate that was supposed to prevent a meaningless measurement was itself nearly meaningless

The check meant to prove the new niche was not merely the existing
generalist's niche wearing a new name was first specified as a rank-order
comparison. Before any code was written against that specification, the
implementer found it algebraically incapable of ever failing on two of the
niche's four axes, because the response function is a strictly monotone
function of distance from an optimum and the two niches share two axis
optima by deliberate design — so rank order between them is fixed by
construction regardless of what either curve's width or strength is
authored to. This was caught before implementation, which is the version of
this failure worth having: the algebra was checked against the actual
formula before any test was written to it, not discovered by a test passing
for the wrong reason.

The statistic that replaced it — a comparison of spread rather than rank —
survived one round of review that found a second, subtler gap: the
formula's strength parameter is a flat multiplier on the whole response
curve, so a species' *devotion* contributes exactly zero to a
coefficient-of-variation measurement, which is scale-invariant. The gate
could pass entirely on the *width* difference between two niches while
claiming to have measured the *devotion* difference — its own named failure
mode, present in the replacement check the first vacuity finding produced.
Catching this took a second review pass with a width-only attribution
variant built specifically to isolate which half of the authored contrast
was actually driving the passing number. **Building a check that cannot be
fooled by the exact axis you are not trying to test takes deliberately
constructing the fooling case, not just reasoning about the formula in the
abstract** — the first review pass reasoned correctly about the formula and
still missed this, because the failure mode only becomes visible once you
build the specific input that exploits it.

## The preregistered readout's headline needed a control it was not specified to have

The third prediction's test — whether the new generalist would take a
majority share of settleable land — returned a clean null on the first run:
zero cells out of a hundred forty-two thousand. Reported at face value, that
null reads as "the coexistence tuning survived a true generalist," which is
the campaign's most attractive possible headline. A review pass asked the
question the original brief never posed: does *anything* in this roster ever
cross that threshold, against anything, anywhere? It does not — even the
roster's strongest, most locally dominant specialist tops out at roughly a
third of the threshold in its own best ground. The null was real. Its
interpretation was not what the first draft claimed, and the difference
between those two readings is the difference between "this campaign measured
something" and "this campaign measured nothing informative and did not
notice." **A threshold-crossing test needs a positive control proving the
threshold is reachable by *something*, specified alongside the test, not
added after a suspiciously clean result invites the question.**

## What went right

- **The owner's mid-campaign design intervention was scoped correctly.** A
  review finding about the raid gate misreading a defensive axis as a
  proactive one prompted new design work, spun out as its own document rather
  than folded into this campaign's diff — which kept this campaign's own
  attribution clean (nothing about the finding's fix touches this campaign's
  measured numbers) while still capturing the discovery before it was lost.
- **A falsified internal hypothesis was written into the shipped file, not
  discarded.** The implementer who built the vacuity statistic predicted a
  specific direction for the result, watched the first run falsify that
  direction, and recorded the mechanism rather than quietly picking a
  statistic that would have confirmed the prediction.
- **The council of "measure, don't narrate" caught its own report's errors.**
  A hand-computed ratio and a backwards-stated rank both survived into the
  first draft of the final readout and were caught only because review
  insisted on re-deriving every number from the committed test output rather
  than trusting the prose that summarized it.
