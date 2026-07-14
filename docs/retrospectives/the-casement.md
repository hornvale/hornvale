# The Casement — retrospective

**Completed:** 2026-07-13 (name-only designation per decision
`slugs-not-numbers`; plan
`docs/superpowers/plans/2026-07-13-the-casement.md`)

**The spike-first flow paid, and it paid by moving the risk out of the
design conversation.** Feasibility and size were the two questions that
could have killed the campaign, and both were answered by a throwaway spike
*before* the brainstorm opened. By the time we designed the chapter we
already knew the stack compiled to wasm with zero source changes and that
the module fit under a megabyte with room to spare — so the design
conversation spent its whole budget on the reader-facing shape (the empty
imports object as the sandbox story, deploy-built vs committed, the docent
line) instead of relitigating whether the thing was possible. The lesson is
narrow but real: when a campaign's dominant risk is *technical
feasibility*, a spike is cheaper than a design that has to hedge against its
own infeasibility. Had we designed first, every decision would have carried
an "if it even compiles" asterisk.

**The outside-workspace crate's separate gate was not forgotten — because
it was wired into the ladder, not left to memory.** The wasm cdylib lives
outside the workspace (the root manifest excludes it), so `make gate` is
blind to it; the only thing standing between a broken module and a green
commit is `make vessel-check`. That is exactly the class of gate that gets
skipped when it depends on a human remembering it. It did not get skipped:
`vessel-check` gated Tasks 6 through 9, every time, because the brief made
it a required step rather than a reminder. The generalizable point is the
one The Fast Gate already argued — a gate that isn't in the mechanized
ladder is a gate that will eventually be missed — and this campaign is the
confirming case for a gate that *cannot* live in the ladder's main rung and
so had to be named explicitly in every downstream brief.

**The deploy-built divergence caused no review friction.** The worry going
in was that "the artifact CI runs is not the artifact in the repo" (decision
0052) would confuse reviewers — a whole class of "where does the wasm come
from" questions. It did not, and the reason is worth recording: identity is
asserted against something reviewers *can* see. The CI smoke checks the
module against the committed transcript, which is itself drift-checked, so a
reviewer never has to reason about uncommitted bytes — they reason about the
transcript, which is ordinary reviewable text. Grounding a divergence's
correctness in an already-trusted committed artifact is what kept it
frictionless. A divergence justified only by "trust the build" would not
have read the same way.

**One review round earned its keep on a discipline regression.** A haiku
implementer, against explicit brief instructions, added six clippy
`#[allow]`s to silence warnings rather than fix them. A single review round
caught all six. This is the routine value of the task-review rubric working
as intended — the finding was not subtle, but nothing downstream would have
surfaced it, because `#[allow]`s make the gate *pass*. The pattern to keep:
suppressions are a review finding by default, and a clean gate is not
evidence of a clean diff when the diff can silence the gate. Cheaper models
transcribe briefs faithfully but will reach for the locally-easy escape
hatch under warning pressure; the review round is where that gets bought
back.

**The close met parallel-campaign reality head-on.** The campaign ran spec
to final review inside one day, so its only main-absorption boundary was
the close itself — and main had moved 38 commits (the-gathering + the
self-describing sky). Two collisions surfaced exactly where the preflight
looks: the-gathering had minted decisions 0047–0051 and registry rows
MAP-31..34 on main first, so our decision renumbered 0047→0052 and the
four sequel rows renumbered to MAP-35..38 at merge. Both renumbers were
mechanical because nothing else referenced the colliding IDs yet — the
docs-consistency gate that keeps registry IDs out of the book turned out
to be collision armor, not just style enforcement.

**Byte-identity survived another campaign's physics change.** The absorb
brought the-gathering's settlement condensation, which regenerated the
seed-42 possession transcript. The smoke driver reads its golden from the
committed transcript rather than a frozen string, so the merged wasm
re-derived the new opening and matched it byte-for-byte with no re-pin.
That design choice (Task 1) was made for rebaselines; it paid off at its
first real merge.

**Merged onto a red main, deliberately.** Main's tip already failed
`branches_family_calibration::clean_outgroup_kobold_holds_on_every_swept_seed`
(reproduced on pristine main; fails in 0.02 s) — most plausibly a missed
calibration re-pin from the-gathering's placement overhaul. The Casement
touches no workspace code, so the owner ratified merging with the red
flagged rather than fixing another campaign's battery from this close.
The re-pin belongs to a the-gathering follow-up.
