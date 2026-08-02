# Retrospective — The Namesake

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-namesake.md): a person-descent
graph that costs no epoch, a naming grammar derived from a society vector, and
a preregistered criterion falsified with a named mechanism behind it.

## The central lesson

**Every review found a real defect, and every defect originated in the plan's
own text. Every number taken from a live measurement survived.**

Six of the seven implementation tasks came back from review with at least one
Important finding. Not one of them was an implementer misreading a clear
instruction; each traced to a sentence the plan or a task brief had written
without measuring what it asserted:

- Task 1's metric deduplicated rule firings by cascade *step index* while its
  documentation promised distinct *rule kinds* — an ambiguity inherited
  verbatim from the brief's own wording. Two of four hundred measured cases
  differ between the readings, and one of those two was the sole occupant of
  the published histogram's top bucket.
- Task 2's ancestor walk was an iterated fixed permutation, so `(Seed(0),
  RoleHandle(0))` was a fixed point and a whole lineage shared one name. Seed 0
  is a reachable world seed; the brief's own deepest-chain test used seed 42
  and never touched it.
- Task 3's forebear derivation fell back to a zero generation length when the
  species had none, which made the kinship function return *sibling* — a
  confident claim — where the honest answer is "not derivable". The brief's own
  comment asserted this case "stays honest". It does not.
- Task 7's brief gave one half of a two-sided frozen criterion a metric and
  left the other half to be "asserted in the study readout", which a readout
  structurally cannot do.

Meanwhile the three numbers this campaign *measured* before designing on them
— the founding-gap distribution, the drift-rate uniformity across settled
peoples, and the rule-firing battery — all survived contact with the
implementation unchanged. The asymmetry is the lesson, and it is the same one
the standing note "measure the edge before designing on it" states: prose in a
plan is an untested assertion wearing the costume of a specification.

## Three specifics worth carrying forward

**A controller-supplied patch is a hypothesis, not a fix.** Reviewing Task 2's
fixed point, the controller wrote the repair inline — fold the step counter
into the mix as `k * C1`. That patch is wrong: the term is zero at `k == 0`
and the mixing function maps zero to zero, so the collision survives at one
step. The implementer caught it and used `(k + 1) * C1`; the re-reviewer
reproduced both variants independently. A patch that arrives with review
authority attached still needs the same mutation proof implementer code gets,
and it is *more* likely to skip it, because the review frame invites it to be
applied rather than tested.

**The plan ran the type-audit lint in every task and the committed report in
none.** The audit is two things wearing one name: `check` is a default-deny
lint inside `make gate`, and `report` regenerates a drift-checked artifact.
Every task ran the lint and passed. Tasks 2 and 3 were therefore reviewed and
committed against a stale report, and Task 4 swept up the accumulated drift
(history +7, worldgen +2, language +3 — all of it this campaign's own, though
the first report of it implied otherwise). This is precisely the failure mode
the standing note names: folding the type audit into the gate closed the lint
gap and left the *committed report* freshness gap open. A plan that touches a
public boundary in more than one task needs the report regeneration in each of
them, or explicitly in exactly one, named as such.

**A frozen two-sided criterion needs a metric per side.** The criterion read
"the median resolves in ≥ 2 elements **and** fewer than 50% require the full
stack". The brief allotted it one metric. A lab study readout reports
registered metrics and nothing else, so as briefed, half of a preregistered
criterion would have gone unmeasured over the whole battery — and it is the
half that would have been quietly dropped, since the other half failed loudly.
The implementer caught it and registered a fifth metric. Preregistration is
only as strong as the instrument list it is checked against; count the
conjuncts and count the metrics.

## What worked

**Measuring before designing, twice, killed two designs cheaply.** The
campaign's opening proposal read the committed community-lineage edge as a
parent-child link. One distribution — founding gaps of median 50 years and
maximum 975 — ended it before any code existed, and the replacement (a descent
edge at a derived remove) fell out of the same measurement, including sibling
relations for free at 13% of edges. The campaign's *original headline* died
the same way: inherited names as phonological fossils was withdrawn on two
measurements taken before preregistration, not on taste. Both deaths cost a
day of measurement rather than a task of implementation and a null nobody
could interpret.

**One seed was treated as an anecdote rather than a finding.** Seed 42's zero
sound rules firing for three peoples was the evidence that killed the fossil
claim — and it would have been an *engine* finding if general. Rather than
report it as one, the campaign's first task built the metric the lab lacked
and ran 200 worlds. The result inverts the reading: goblin fires a mean of
1.305 rules and reads zero on 19% of worlds, so seed 42 sat in a one-in-five
tail of a live distribution, not on a dead mechanism. A suspected engine-level
defect was retired for the cost of one additive metric. The withdrawal of the
fossil claim still stands, on its other and structural reason.

**The failure was pushed on rather than reported.** Task 7's falsified
criterion could have shipped as a bare null. Two artifact hypotheses were
tested instead — that dropped unresolvable elements shortened names, and that
the scope was wrong — and both were shown to fail, the first via a machine-
checked equivalence (for any founder with ≥ 2 elements, spending exactly one
is equivalent to its stem being world-unique, verified over 54 seeds with zero
mismatches) that makes the maximum-fill counterfactual computable rather than
estimable. That is what converted a null into a falsification with a named
mechanism upstream of the thing being measured.

**Nothing was retuned after unblinding, and it was verified rather than
asserted.** The review checked `git diff --name-only` across Tasks 2–6's
source over the measurement commits and found it empty.

## Follow-ups

| | |
|---|---|
| **F1** | **`occ-founded` is in years and doc-commented as "standard days."** Same for `Occupation::tenure` and `HISTORY_NOW`. The one place it could bite — `vestige.rs` weathering against `PERISHABLE_MAX_AGE`, documented in years — was traced and the units agree, so this is a naming inconsistency, not a live arithmetic bug. Cheap to fix; worth doing before someone reads the doc comment and divides by a day length. |
| **F2** | **Person-name stems draw near-uniformly, and real given names do not.** The mechanism behind this campaign's falsification: an effective stem space of 5k–17k makes 62–99% of a world's given names world-unique, against a measured 0.567 collision rate for settlement and deity names in the same worlds. Real given-name distributions are steeply Zipfian, which is *why* patronymics and bynames exist. Giving the person draw a realistic frequency distribution would make the shortest-prefix rule earn its keep without changing a line of it — and it is a naming-function change, so it lands on the derived side and still costs no epoch until a toponym cites it. |
| **F3** | **The region-scope criterion may never have been satisfiable.** §5.2(2)'s "region" resolved to every founder in the world across 2000 years of history — figures separated by centuries who would never contend for the same reference. The design's own ladder (household → settlement → region) meant something spatial and smaller. This does *not* rescue the criterion (a smaller competitor set can only push the median down), but it means the frozen criterion was measuring a different question than the one it was written for. Worth a defined spatial scope before any successor campaign re-freezes it. |
| **F4** | **A latent trap in the descent walk's element-budget arithmetic.** The push and pop guards are mutually exclusive and therefore correct today — but only because both arms compare the same `0.5` literal with strict operators. Widening either to `>=`/`<=` would silently pop the wrong element with no compiler signal. Reviewer recommends an `if`/`else` or a three-way match; triaged and deliberately deferred rather than folded into a task whose measurement was already running. |
| **F5** | **`the_clan_walk_terminates_for_every_occupation` runs ~62 s**, over nextest's 60 s slow threshold. Pre-existing from the task that introduced the walk; not a heavy-tier candidate on cost alone, but it is now the longest test in its crate and will show up in the duration alarm's baseline. |
| **F6** | **`NameElement.conferred` is `None` everywhere and has no writer.** Every element the current derivation produces is conferred at birth, so the field is documented as the seam a later deed-name or coming-of-age campaign fills. Recorded rather than shipped silently: an always-`None` public field is indistinguishable from a forgotten one six months on. |

## Confidence Gradient

**No bet moved.** `book/src/open-questions.md` was grepped against this
campaign's territory — `name`, `naming`, `toponym`, `person`, `lineage`,
`kin`, `shortest`, `disambiguat`, `collision`, `anthropon`, `genealog`,
`founder`, `descent`, `cascade`, `sound change`, `etymolog`. Every hit was
unrelated: the two `cascade` matches describe a conflict-collapse mechanism,
not sound change, and the `descent` match is a chamber-derivation sentence
from a different campaign. Checked, no re-score owed.

## A note on the branch's red gate

This campaign's branch closes with a known-red baseline of **34** failures in
`hornvale-lab`, all one cause: adding metrics to the lab registry makes the
committed census fixtures' headers disagree with the study schema, and those
fixtures are authored on the canonical host at the campaign's close rather
than locally. The plan did not carry a census-refresh step, which is a real
gap in it — the count and shape were established after the first task and
carried into every subsequent review brief precisely so that 34 reds would not
be mistaken for the task under review. One correction was needed along the
way: the reds are **two** stale study fixtures, not one (31 from the main
census, 3 from a second `"metrics": "all"` study), so a refresh covering only
the first would leave three red. Any campaign that adds a lab metric inherits
this, and should establish the baseline's exact count and shape at its first
task rather than at its last.
