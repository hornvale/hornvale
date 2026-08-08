# Retrospective — The Assay (test-suite census, 2026-08-07)

Process lessons only; the product story is the chronicle. Filed under the
slug `the-reassay` because `docs/retrospectives/the-assay.md` already
belongs to the unrelated 2026-07-21 creature-potency campaign — see the
naming-collision section at the end, which is this retrospective's own
best example of its central lesson.

## The dominant finding: a claim of absence requires a complete search

One error recurred **six times** across this campaign, and the sixth
instance was the controller's own, made while cataloguing the first five.
Every instance reduces to the same shape: someone asserted that something
was *not* there — a duration in a baseline file, a machine's identity, a
function's precondition, code inside a function body — from a search that
had not actually covered the ground it needed to.

1. **The spec's own hunt durations were read off a stale baseline.**
   `docs/timings/test-baseline-MacBookPro.tsv` was recorded pre-Weir; the
   spec's first draft cited 7.617 s and 8.412 s for two of the three seed
   hunts. lefford's post-Weir baseline gives 13.132 s and 12.262 s for the
   same two tests — the search for "the hunt's cost" had stopped at the
   first file that had a number in it, not the current one.

2. **The controller called ambrose "this Mac" and quoted MacBookPro's gate
   median against it.** `hostname -s` on the working box returns `ambrose`,
   a different, 12-core machine from the one the baseline file is keyed to.
   The absence being (wrongly) asserted was "no difference worth naming
   between the two machines."

3. **The build profile and `hydro_witness`'s live timing were measured on
   ambrose and cited beside MacBookPro's figures** in the same table,
   without the host named on either — the same error, propagating.

4. **Task 10's brief asked for a before/after across two hosts** — lefford's
   sum of a 7-test binary against an ambrose run of the merged 6-test
   binary — and reported ~101 s saved. The defensible number, same host
   throughout, is the retired test's own cost on lefford: 13.132 s of
   137.864 s, 9.53%. The brief's search for "the honest saving" had crossed
   a machine boundary without noticing.

5. **The spec's synthetic route for the crisis hunt was infeasible**, found
   only when an implementer read `crisis_from`'s actual signature and
   preconditions. `observations_from` calls `crate::sky_of(world)?` and
   refuses anything but a `Sky::Generated`, deriving its event list from
   real orbital mechanics — no fact a test can hand-commit expresses that.
   The spec's search for "can this be synthesized" had checked the shape of
   decision 0093's precedent, not the target function's own preconditions.

6. **The controller asserted two `purview.rs` claim-tags were false**, from
   a grep window (`sed -n '301,340p'`) that stopped one line short of the
   evidence (`.map(|s| s.sun_altitude_deg)` at line 341) — *while writing a
   message that catalogued the same error class in five other places*. The
   dispatched reconciler refused the correction, re-checked both claims
   directly against the source, and was right on both counts. The
   controller's own message had told it to verify rather than comply; that
   is the only reason the sixth instance was caught before the final
   review rather than after.

**The durable form, revised after instance 6:** it is not "agents state
unmeasured facts" — the controller does it at the same rate, including
about the agents it is supervising. A claim of absence requires a complete
search, and a truncated window — of a baseline file, of a host identity, of
a function's preconditions, of a source-code region — is not one.

**Two corollaries worth keeping in every future brief:**
- A duration is meaningless without its host; a before/after names a single
  host throughout or it is not stated at all.
- Tell every reviewer and reconciler to *verify*, not comply. The dissent
  that caught instance 6 existed only because the dispatch said so
  explicitly.

## The tag format invites its own class of error

Twelve false `claim:` tags landed across three separate tagging efforts
during Task 12, none caught by the author, all caught by review or a later
absorb:

- `windows/vessel/src/lattice/classify.rs` — 8 tags read `structural(seed:
  none — seedless sweep)` on a function with `const SEEDS: Range<u64> =
  0..192`, drawing 384 seed-derived fixtures per call. A 192-seed sweep,
  tagged as seed-free.
- `render.rs` — 2 more of the same shape (96 fixtures per call).
- One fabricated self-citation quoting a module-doc sentence that does not
  exist.
- One unmeasured claim ("147 raw strings … none trips this") that a sweep
  later found false in exactly the file it named, though the underlying
  mechanism turned out not to be live anywhere in the tree — itself only
  established by actually running the scanner, not by re-asserting the
  documentation.

The shape is structural, not a run of bad luck: a `claim:` tag pairs a
checked vocabulary token with an unchecked prose justification, and every
false claim landed in the prose half. **The follow-up worth keeping is
mechanical, not procedural**: have the lint parse and verify its own
`seed:` argument — cross-check a claimed `seed: none` against the presence
of `Seed(` in the test's own body — which would have caught most, though
not all, of these on the first pass rather than the second or third review
round.

## Two controller process errors, disclosed rather than smoothed over

- **Committing in a worktree while a subagent was live in it.** The
  controller ran `git commit` after staging one unrelated file, not
  realizing a live Task 8 implementer had already staged its own `git rm`
  of `hydro_witness.rs` in the same index. `git commit` commits the whole
  index, so the resulting commit contained both changes under a message
  describing only one. The fix was procedural, not a history rewrite —
  re-splitting would have raced the subagent's own pending commit — and the
  review package for that task was told explicitly to span both commits.
  **Lesson:** never run `git commit` in a worktree with a subagent still
  live in it; commit out-of-band captures on a separate branch or worktree,
  or wait for the task boundary.

- **Treating "clean tree plus a new tip" as proof an agent had finished.**
  A watcher process observed a clean working tree and a new commit and
  concluded the Task 12 agent's turn was over — while that agent was, in
  fact, still running, and reacted to the same merge independently. Two
  agents ended up converging on the same follow-up task; no tree damage
  resulted only because the second agent (a dispatched reconciler) had not
  yet written anything. **Lesson:** only the completion notification means
  an agent has finished; a clean tree and a new commit are consistent with
  either "done" or "about to write more."

## What worked

- **Mutation testing, applied consistently, caught what inspection did
  not** across every stage that used it: the tripwire's own staleness guard
  (Task 3), the hydro coverage assertion (Task 8, both directions — present
  and absent), the toponymic assertions (Task 9), and the claim-shape lint
  itself (three separate fix rounds, each re-verified by compiling the
  shipped scanner byte-for-byte into a scratch harness rather than trusting
  a description of it).
- **An implementer that measured, found a claim didn't reproduce, and wrote
  neither the original claim nor its opposite — only what it actually
  measured** (Task 12, fix round 2, the raw-string sweep) is the correct
  response to an unmeasured assertion, and it is recorded here as the
  positive example beside the twelve negative ones above.
- **The lint validated itself against real incoming code**, unarranged: the
  Task 12–13 main-absorb pulled in two untagged seed loops from a
  concurrent campaign, and the lint caught both on the first merge that
  could have hidden them.

## Follow-ups promoted before teardown

- The remaining ~224-test migration this tranche's three retirements sampled
  from (spec §7's explicit out-of-scope register).
- 31 live `reachability(seed: …)`-shaped hunts the claim-shape tagging pass
  surfaced, roughly seven times the original audit's count; the expensive
  concentration sits in `domains/astronomy/src/facts.rs` (four hunts, up to
  200 `generate()` calls each — two sweep `0..64`, two sweep `1..=200`),
  `domains/terrain/src/carve.rs:2503` (seeds 1..=8 at `GLOBE_LEVEL`), and
  `domains/language/src/naming.rs:2709` (0..600).
- The claim-shape lint's five residual detection gaps, matching the module
  doc's own list: a ≥9-identifier `for`-pattern is invisible
  (`MAX_PATTERN_TOKENS = 8`; fixed for `Seed(<expr>)` patterns in round 2, so
  this gap is the `for`-pattern binding, not `Seed(<expr>)` itself); a nested
  block comment miscounts brace depth; a raw string with an odd
  embedded-quote count and a brace inside it leaks past its own end; nine
  further tests sweeping the same seeded corpus under a `_` binding in the
  two files the tagging pass already corrected once; and the detected-count
  floor's slack, which grows with the tree rather than staying pinned. (The
  subprocess-built world in `cli/tests/sky_exit_criterion.rs` is a separate,
  named blind spot in the module doc — not one of these five.)
- The mechanical `seed:`-argument verification idea above — cross-checking
  a tag's own claimed argument against the test body, closing the gap the
  twelve false tags all fell through.
- A second census `pin_set` for pinned-regime claims (spec §6.6) — the
  current single-`pin_set` fixture cannot serve them.
- Per-test durations for the heavy tier (already registered as
  `PROC-heavy-tier-cost-budget` in the idea registry — not re-minted here).
- The stale `docs/timings/test-baseline-MacBookPro.tsv` itself: not merely
  the mechanism gap CLAUDE.md already names (a renamed host forks the
  baseline silently), but the concrete fact that this file sat unrefreshed
  long enough to feed a wrong number into this campaign's own spec.
- `hornvale-worldgen` and `hornvale-vessel` as a real cost campaign — the
  two crates carrying the largest remaining un-migrated world-building test
  populations (138 and 11 Settlements/Full tests respectively, plus
  `hornvale-vessel`'s 2,079 s `session` suite, which spec §6.5 rules out of
  this migration's scope entirely as an action-sequence claim).
- The five parked worktrees this campaign's own session accumulated across
  its 110-odd commits — swept, per house rule, rather than left for the
  next session to puzzle over.

## The naming collision, and why it belongs in this document

This campaign's branch (`campaign/the-assay`), its spec
(`docs/superpowers/specs/2026-08-07-the-assay-design.md`), its decision
ledger, and 110-odd commit messages all call it "The Assay." A shipped
2026-07-21 campaign — the Dragons program's creature-potency warm-up —
already holds that exact name, with its own chronicle and retrospective
already committed at the paths this campaign's brief asked Task 13 to
write to. Nobody checked whether the name was free at spec time, at plan
time, or across the first twelve tasks; it surfaced only while writing this
document, the thirteenth.

It is the same failure mode this retrospective spends most of its length
on, wearing a different costume: a claim of absence ("no campaign is
already using this name") asserted from a search that was never run. This
document and the matching chronicle entry are filed under the disambiguating
slug `the-reassay` to avoid overwriting the original. The branch name, the
spec's filename, and the decision records already minted under "the-assay"
are left as they are — renaming any of that is a judgment call for the
close, not something a doc-only closing task should decide unilaterally.
