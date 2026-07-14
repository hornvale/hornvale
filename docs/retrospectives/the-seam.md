# The Seam — retrospective

**Completed:** 2026-07-13 (name-only designation per decision
`slugs-not-numbers`; plan `docs/superpowers/plans/2026-07-13-the-seam.md`)

**Two campaigns closed under us in one afternoon, and the stage-boundary
absorption cadence held.** The Uncommon Ground and The Datum both merged to
main mid-execution. The preflight-driven absorption at the Task 5/6
boundary caught both: the first was a genuine semantic collision (the
locale's `SubCellTexture` became the `Regime` overlay under
`locale/room/v2`, breaking the focalizer's ground noun at compile time),
reconciled in the merge commit itself with the full gate as proof; the
second was behavior-free for the vessel. The lesson is the one the
process doctrine already claims, now with evidence at game-layer scale:
small absorptions put the semantic drift next to its cause. Had the branch
met main only at close, the texture→regime rewrite would have surfaced
inside a 22-commit merge instead of a two-line focalizer edit — and the
committed transcript would have baked the stale surface. Absorb before
artifacts, not just before merge.

**Ideonomy passes earned their keep as spec-hardeners, not idea
generators.** Four passes produced seven adoptions, but the two that
mattered most — the examine contract and session-accumulating knowledge —
came from organons exposing *unfilled slots* in a design everyone thought
was complete: the notation pass could not fill `examine ???`'s slots, and
the state-machine pass found `day` was a parameter no transition could
change (which became `wait`). The pattern to repeat: run the machinery on
the *written spec*, not the idea — precision exposes ambiguity only after
there is something precise to expose.

**Plan example code is not reviewed code.** Two review findings traced to
code the plan itself supplied: the sky noun's prose containment held by
coincidence of two string literals (against the plan's own stated
"by construction" principle), and two session tests probed directions via
a substring match with a known collision. Both were caught by task
reviewers, neither by implementers — implementers transcribe briefs
faithfully, which is exactly why the plan's own code needs the same
scrutiny as the implementer's. The task-review rubric's "plan-mandated is
still a finding" clause did the work; keep it.

**The dispatch preamble's per-commit re-check caught a live wrong-checkout
drift.** Task 1's implementer, after a correct opening `cd`, drifted back
to the main checkout mid-task and began editing there; the pre-commit
`pwd && git branch --show-current` re-check caught it before any commit
landed astray, and the sweep confirmed the main checkout clean. That is
the second production save for the read-do checklist (after Crust Task 7)
— the recovery recipe shipping *with* the prohibition is what makes it
hold.

**The final whole-branch review sees what nine task reviews cannot.** The
one genuine spec-vs-shipped divergence (the spec's "render diegetically"
for nesting exits vs the shipped hidden door) survived every task-scoped
gate because each task honestly matched its brief — the divergence lived
*between* the spec and the plan, and only the cross-task altitude caught
it. It became an owner decision at close (hidden door ratified, "nesting"
replacing the misleading "vertical" in prose). Budget for the expensive
final review; it is not a formality.

**Carried terminology decision:** `ExitKind::Vertical` (locale's shipped
code name) misleads — these exits nest scales rather than climb. Renaming
is a locale-owned, schema-adjacent change (the variant serializes into
`locale/room/v2` artifacts), so it was deliberately not done at this
close; the prose now says "nesting" and the rename waits for a locale
campaign with a schema epoch to spend.
