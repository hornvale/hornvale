# The Type Audit — retrospective

**Completed:** 2026-07-10 (name-only designation per decision 0026; the
campaign spec had claimed "Campaign 27" before the numbering freeze landed)

**Recurring findings.** One finding recurred across every tagging pass and is
worth naming as a pattern for future judgment work: *a newly-ratified class is
applied where the owner named it, but not generalized to structurally
identical sites.* The `prose` class was ratified on astronomy/kernel; the
reclassification then applied it only to the exact fields the ratification
named, leaving six identical genesis-note and description strings tagged
`identifier-text` — caught in review, fixed by re-running the same pattern.
The lesson held for the rest of the campaign: every subsequent crate's review
was primed to check "is any description mislabeled a key?", and the later
crates came back clean because the taggers were told the lesson up front. When
a rubric grows mid-task, the growth has to be swept backward over already-done
work, not just forward.

**The ratification loop worked, and paid for itself early.** The plan routed
contested and novel-class items to the owner. Batching them after the *first*
pass (astronomy + kernel) rather than per-crate was the right call: those two
crates surfaced all four foundational questions at once — the artifact class,
the prose class, the handle-vs-index split, the already-typed-should-defer
question — and ratifying them before the other thirteen crates meant those
crates had a settled eleven-class rubric and produced almost no new contested
items (climate: zero; species, culture, worldgen: zero). Front-loading the
hard classification questions onto the reference crates is the pattern to
repeat.

**A durable convention changed mid-flight.** The campaign began numbered
"Campaign 27" and writing decisions `0024`/`0025`. Between its start and its
close, decision 0026 ("slugs, not numbers") landed on main and explicitly
named this campaign as the first to adopt name-only designation. The decisions
were renamed to slugs and the campaign to a name before merge. No harm done,
but it is a clean instance of the exact problem 0026 solves — parallel
sessions colliding on sequential IDs — observed one more time from the inside.
The branch stayed on its original base and used the collision-free conventions
(slug decisions, name-only campaign), so integration with the advanced main is
a clean merge rather than a renumbering.

**Estimate deltas.** No stage-level time estimates were made, so there is
nothing to compare against — recorded here only to say so rather than pad.

**Spec vs. reality.** The plan's sixteen tasks landed close to as written. The
tool (tasks 1–8) needed three small corrections the plan's example code did
not anticipate, each caught by its task review: a proc-macro2 feature the plan
had added pre-emptively (correct), a `cfg(test)` detector that a substring
match would have made silently skip production `cfg(not(test))` modules
(fixed to a precise parse before the walk shipped — the spec's own "no silent
skip" rule, enforced against the plan's sketch), and a path-exclusion rule
whose literal form would have excluded the tool's own fixtures. All three are
the ordinary distance between a plan's illustrative code and a compiler's
insistence, and the review-per-task loop closed each before it propagated. The
tagging tasks (9–13) deviated from the plan only in that the rubric grew by
three classes — a growth the plan explicitly anticipated as the audit's
ratification loop, not a miss. The one genuinely unresolvable case is a tool
limitation worth carrying forward: a `Vec<(label, description)>` return is a
single audited position, so a `stream_labels()` function cannot tag its label
half and its description half differently — it takes the verdict of its
load-bearing element. A future tool with per-tuple-position granularity would
resolve it; the campaign chose the load-bearing verdict and documented the
limit.
