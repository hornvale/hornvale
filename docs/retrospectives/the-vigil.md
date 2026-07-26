# The Vigil — retrospective

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-vigil.md).

## The headline: a drift check is not a test of correctness

The campaign's keystone claim — that the draconic clade eye lexicalizes
exactly `dark`, `light`, and `red` — was preregistered before any regeneration,
verified against the regenerated dictionary, and reported as held. All of that
was true and none of it was sufficient. The independent whole-branch review
found that the added exposure test asserted only that `blue` is a perceptual
gap and `starlit` is `Steeped`, and **both of those hold at 0.75 as well as at
the shipped 0.9**. The concepts that actually discriminate the two depths —
`green` and `yellow` — were asserted nowhere in code. The headline rested
entirely on `dictionary-generated.md` being drift-checked.

A drift check pins output against *change*, not against being *right*. Had the
constant been wrong, the dictionary would have been regenerated to match the
wrong value, the drift check would have passed forever after, and the campaign
would have shipped a confidently-stated falsehood with a green suite. The fix
added a direct `pack_depths` assertion plus the discriminating concepts, and
**mutation-verified** it: set the constant to 0.75, watch both tests go red,
restore, watch them go green.

The generalizable rule: **when a campaign's headline is a claim about a
generated artifact, the artifact's drift check is not the test — a test that
fails when the claim is false is.** Preregistration protects against
motivated reading; it does not protect against a value being wrong. The
spec had actually asked for exactly this (§8.3: "asserted against the concept
ids that enter and leave the ladder"); the plan's self-review then claimed §8
items 1–7 were covered when item 3 was not written. A self-review that checks
coverage by *listing* rather than by *looking* will pass a plan that omits the
one test that matters.

## Consumer enumeration needs a second axis

Spec-time consumer enumeration, complete by class, has been this program's
reliable defence since The Eremite's whack-a-mole, and it worked again — the
§6 blast-radius table predicted every code consumer correctly, including all
five "unreached" rows, which the final review re-verified independently.

It still missed something: `cli/tests/fixtures/world-seed-42.json`, a committed
byte-golden world fixture. The enumeration was organised by **consumer class**
(who reads this component?) and never asked the orthogonal question, **what
committed artifacts encode this output?** Test fixtures are not consumers and
not artifacts-under-the-CI-drift-check, so they fell between the two headings.
Its tripwire caught it at the gate and it was re-pinned in the drifting commit,
so no harm — but the near-miss is the lesson: **enumerate consumers by class
AND committed artifacts by class; they are different partitions of the blast
radius.** Prediction P7 ("the dictionary is the only committed artifact that
changes") was scoped to the CI drift check and technically held, which is
exactly how a too-narrow scope survives review — it reads as broader than it is.

## An ideonomy pass earned its keep by overturning the brief

The brief's stated lean was a "sky-attentive" dragon. A tree-finding pass over
"a creature's world-access" surfaced an empty branch — **vantage** — and with
it the distinction the lean had collapsed: `sky_attention` means *celestial
versus terrestrial*, not *airborne*, and it trades directly against noticing
the ground. Flight is a vantage fact with no slot in the vector. Encoding it as
celestial attention would have put a true thing in a false slot, and would have
worked against the very next campaign, whose subject is a ground-scanning
predator.

The same pass produced the campaign's authoring rule by noticing that the three
dimensions sit at different levels — organ, schedule, allocation — and that only
the organ reaches language. That is what made "clade eye, ecological schedule"
a derivation rather than a preference.

## The Cloister's own lesson, applied and then found insufficient

The Cloister's parting advice was: when a campaign decouples one axis, audit
any new gate it introduces for a silent re-coupling to a third. That audit ran
and found the real thing — fact emission gated on `Settled`, i.e. sedentism
standing in for capacity, the same confusion decision 0068 had corrected for
society and left standing in three other families.

What the audit did *not* initially produce was the right scope. The owner was
asked a question about perception facts alone; fixing only those would have
reproduced 0068's defect one field over, in three families at once. **A
re-coupling audit should ask not just "is this gate wrong for my axis?" but
"how many other axes does this same gate carry?"** — the answer decides the
scope, and it is usually larger than the question that surfaced it.

## The absorption cadence was missed

This branch's first meeting with main came at close, by which point main had
moved **76 commits** and recorded decisions 0069–0073 — so the spec's own §10,
which names decision 0069, was stale before it was written. CLAUDE.md requires
absorbing main at every plan-stage boundary; this campaign absorbed once, at
the end. The merge was clean and the gate green on the merged tree (2049 tests,
and the world-identity tripwire passed unchanged, so main's commits had not
moved world identity), so the cost here was only a wrong decision number. The
cost is not always that small.

Concretely: **compute the decision number at close, never at spec time** —
the same rule the registry IDs already follow, and for the same reason.

## Subagent economics, and what a spend limit costs mid-campaign

An API spend limit terminated the Task 2 implementer after it had written every
source edit but before it gated or committed — 65k tokens and ~83 minutes spent,
no commit, no report. Recovery was cheap only because the preamble discipline
meant its work sat in a known worktree on a known branch, verifiable against
the task brief.

Two consequences worth recording:

- Tasks 2–4 ran inline without an independent per-task reviewer. The final
  whole-branch review became the sole independent check, and it found three
  Important issues — including the headline one above. **When per-task review
  coverage is lost, the whole-branch review is not a formality, it is the
  entire gate**, and it should be dispatched on the most capable model with the
  gap named explicitly in its prompt (it was, and it responded by verifying
  emission-order preservation more rigorously than the branch's own tests did —
  parsing both fixtures and proving the old fact sequence is an in-order
  subsequence of the new).
- A single fix subagent handling all eleven findings in one pass cost far less
  than the per-finding fixers the skill warns about, and produced a coherent
  three-commit grouping (tests, comments, prose).

## Smaller notes

- **`make gate-full` rewrites committed benchmark artifacts as a side effect.**
  The heavy tier regenerates `book/src/laboratory/generated/the-sounding/`,
  whose contents are timing-derived scaling exponents; running it dirties the
  tree with machine-load noise that `regenerate-artifacts.sh` never produces.
  Reverting is correct. A committed artifact whose regeneration is not
  deterministic is a latent trap for exactly the close-time "is the tree clean?"
  check — worth its own fix someday.
- **The two-block emission shape is load-bearing and looks like a DRY
  violation.** A future "simplify" pass that merges the duplicated mind blocks
  would move `in-group-radius` after `time-horizon` for four peoples in every
  world — a save-format change with no epoch. It is now commented as
  load-bearing; the pre-flight plan scan predicted a reviewer would flag it,
  and the reviewer instead defended it more strongly than the plan did.
- **A comment can be a forward reference.** Task 2 shipped a comment asserting
  an invariant that Task 3 had not yet added — false at that commit, true one
  commit later. It was flagged at the time and confirmed true at branch HEAD by
  the final review. Acceptable on a branch that merges as a unit; worth naming
  so it stays a deliberate choice rather than a habit.
