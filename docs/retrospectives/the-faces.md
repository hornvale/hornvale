# The Faces — retrospective

Process lessons from hornvale#4 (`scene/moons/v1`). Product is in the
[chronicle](../../book/src/chronicle/the-faces.md); this page is how the work
went, not what it built.

## What went well

- **The fidelity call held end-to-end.** The G1 decision (derived physics +
  seeded descriptors, no invented elevation) was made once, at brainstorm, and
  every downstream artifact — schema, page, reviews — kept the derived-vs-seeded
  line sharp. Both the A4 and final reviewers specifically checked for
  fidelity-overstating language and found none. A single honest boundary,
  stated up front, propagated without erosion.
- **The binary-is-fixture test is the right cross-repo seam.** The orrery test
  that instantiates the real `world-wasm-v4` binary, runs genesis, and parses
  what `hw_scene_moons` actually emits makes producer↔consumer drift impossible
  to miss — a stronger guarantee than any committed JSON copy, and it caught
  nothing only because the contract was right.
- **A review loop improved the design, not just fixed a bug.** The A1 review
  flagged a 46-minute test; the fix (exercise the pure `seeded_descriptors` fn
  over synthetic masses instead of building 199 worlds) was both ~10⁴× faster
  and more targeted — it tests the bias mechanism directly. The best fixes make
  the thing better, not merely green.

## What to carry forward

- **A plan's sample code must itself pass the project's gates.** Both A1
  review findings traced to the plan's own sample code, not implementer error:
  it omitted the CI-enforced `type-audit` verdict tags, and it shipped an
  untagged multi-minute live-worldgen test into the commit gate. When a plan
  embeds complete code, that code needs the same scrutiny as committed code —
  type-audit tags and `heavy:` tiering are exactly the easy-to-forget bits.
  Next time: a plan-authoring check that any pub-boundary primitive in sample
  code carries a tag, and any world-building test carries a tier decision.
- **The parked-subagent recipe earned its keep.** The A1 implementer parked on
  background watchers instead of finishing — the documented failure mode — even
  with the preamble in place. The challenge-response caught it (no verdict line,
  narrated intent to "wait"), and the SendMessage un-park (foreground poll, then
  continue) recovered it in one turn. The preamble alone did not prevent
  parking; the recovery recipe is what made it a non-event. Keep both.
- **After resolving a merge conflict, grep for ALL three marker types before
  committing.** Resolving the idea-registry conflict, the first edit removed the
  `<<<<<<<`/`=======` markers but left the trailing `>>>>>>> main`, and the
  `cargo fmt --check` pre-commit hook does not scan markdown — so it committed a
  stray marker, caught only by a follow-up grep and amend. The gate does not
  guard prose conflict markers; a `grep -n '^<<<<<<<\|^>>>>>>>\|^=======$'`
  after every conflict resolution does.
- **Verify origin/release state before external actions — memory goes stale.**
  Standing memory claimed a 24-commit unpushed backlog and a blocked v3 release;
  reality at close was origin == main and v3 released. The pre-push checks
  (`rev-list --count`, `gh release list`) took seconds and corrected the record.

## Cadence note (owed by the close discipline)

This campaign's first meeting with `main` was at close, not at a stage
boundary — the absorption cadence was skipped. The cost was low (an additive,
single-session campaign; the absorb produced one trivial registry conflict and,
usefully, pulled in the census re-pin that turned the gate green), but it was a
skip. For a longer campaign the same skip would have surfaced as a large,
late, semantically-tangled merge. Absorb at stage boundaries even when the
campaign feels short.

## Follow-ups

Carried from `.superpowers/sdd/followups.md`: FU1 the moon-mass draw range is
duplicated between `seeded_descriptors` and astronomy (low coupling risk); FU2
the now-dead `moonRadiusUnits(sizeRel)` in the orrery; FU3 explicit texture
`colorSpace`; FU4 a length-mismatch guard on the moon arrays; FU5 the MAP-50
moon-elevation tier (idea registry). None block; none are correctness bugs.

## Confidence Gradient

No bet in `open-questions.md` moved: the open bets touch terrain-LOD fidelity
and the semantic-query surface, not moon presentation or scene-schema breadth.
Checked, no re-score owed.
