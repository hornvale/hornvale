# The Gazetteer — retrospective

**Merged:** 2026-08-19

## Every defect this campaign found originated in the controller's own text, and none in implementer code

Twelve instances, all traceable to the spec or the plan rather than to what an
implementer wrote against them, and all caught by someone *measuring*
something rather than re-reading it:

- **Four stale or inert `type-audit` tags**, copied from the plan's own
  code snippets into three different tasks — `components()`'s
  `bare-ok(count: return)`, `FeatureId`'s `bare-ok(index: cell)`, a third at
  Task 3, a fourth dropped at Task 4. Every one was inert for the same
  reason: `tools/type-audit/src/primitives.rs` does not track
  `CellId`-nested or otherwise untracked-primitive types, so the tag had no
  matching audited position to attach to. Harmless, but four separate
  implementers had to notice it independently because the plan's own
  snippets kept re-copying it forward.
- **Three degenerate synthetic-predicate test fixtures.** Task 2's brief
  predicates yielded one component (a partition test that cannot fail for
  the reason it was written) and two components (an ordering test that
  passes about half the time under a shuffled-order bug); Task 3's inherited
  `c.0 % 7 < 4` yielded five components, one 360-cell blob and four debris.
  None of the three was hoped past — every implementer measured its fixture
  before trusting it, which is what caught all three, and the plan was
  amended mid-campaign with a non-degenerate-fixture rule in Global
  Constraints once the shape repeated (5ee8cfe5) rather than patched site by
  site.
- **One unbuildable registry premise.** `RENDER-fogged-world-map-rung`
  claimed a fogged planetary view would carry "the same three epistemic
  states the local chart does" — false by construction, since a planet
  needs a fourth state covering nearly all of its surface that a local
  chart's bounded radius never needs. Caught while drafting this campaign's
  own spec §9, by checking the claim against the code rather than
  transcribing it forward again.
- **One tautological hypothesis.** H2 predicted per-culture name divergence
  and was measured at world scale as 42,525/42,525 = 1.0000 — a number that
  could not have come out otherwise, because `species` is a leg in
  `Namer::name`'s own derive path. The exact 100% reading is what flagged
  it: an exact rate is close to always a default, not a finding, and this
  is the fifth time in this repo's history that shape has bitten. The
  measurement that actually carries H2 is a narrower unit control nobody had
  specified until the tautology forced the question of what would actually
  vary.
- **One falsely-justified code duplication.** Task 8 duplicated a naming
  join rule into two window crates with a doc comment asserting the
  duplication was architecturally forced. It was not — `windows/CLAUDE.md`
  already permits window-to-window dependencies, and `hornvale-almanac` has
  no window-layer dependencies of its own, so `explain -> almanac` was
  acyclic and legal from the start. The reviewer's finding was sharper than
  "duplication exists": a documented-but-wrong justification is worse than
  an undocumented duplicate, because it reads as a settled decision and
  stops the next reader from re-examining it.

The pattern holds across all ten: nobody's code was wrong. What was wrong,
repeatedly, was a claim written in a spec, a plan, or a doc comment that
nobody had checked against the thing it described before writing it down.
The fix in every case was the same motion — measure the actual fixture size,
grep the actual constraint file, run the actual derive chain — never a
closer reading of the prose that made the claim.

## Two git mistakes, one root cause, inside one hour

`git add <path> && git commit` without `-- <paths>` swept a subagent's
already-staged probe into a registry commit (62f13d85) — the documented
"`git commit` commits the whole index" trap, already in the controller's own
memory index, and it still happened. Recovering from the first mistake
caused the second: a `git commit --amend`, issued on the belief that HEAD
was still the registry commit just made, actually amended a *different*
commit — the Task 4 agent had landed `5325ea8b` in the interval between the
two commands, so the amend rewrote the TIMINGS commit with the registry
message. Restored as `c28b98ee`, with the original commit's content verified
intact by diff.

**In subagent-driven development, HEAD moves under you.** A dispatched
agent can commit at any point the controller is not looking, so any command
that acts on "the current HEAD" without first re-reading `git log -1` in the
same breath is acting on a stale assumption — and `--amend` is exactly that
command, since it has no confirmation step and no dry run. The general rule
this campaign re-learns from a new angle: always path-scope a commit
(`git commit -- <paths>`, never a bare `add -A`/`commit` pair), and never
`--amend` without a `git log -1` immediately before it, in the same tool
call if the harness allows it. 62f13d85's commit message still understates
its own diff — it omits the probe half of what it actually carries — and it
was left as-is rather than rewritten, because a non-interactive rebase
was unavailable and the disproportion between a stale commit message and a
history rewrite was not close.

## Two measurements arrived that nobody had preregistered, and both were the real finding

Twice in this campaign, a number that *was* preregistered turned out to be
the wrong question, and the right one only surfaced because a suspicious
result got interrogated rather than accepted.

Task 1 froze the floors before writing traversal code, but its actual open
question was never "what are the counts" — it was "does the shipped census
extractor agree with a fresh implementation of the same walk", after a
183-vs-23 discrepancy against a committed census artifact appeared. It
closed by exact multiset agreement between two independently-written
implementations, which is a stronger check than a plan or a reviewer could
give it directly, and which incidentally exposed that the committed census
figures were stale by 66 terrain commits — a repo-health finding this
campaign is not positioned to fix, banked to the registry instead.

Task 7's H2 was preregistered as cross-people divergence. The tautology
that measurement turned out to be forced the real question into the open:
whether one people's own names collide with themselves. That number —
9.15% pooled, 62.47% for kobold alone — was not asked for anywhere in the
spec, and it is the more useful of the two by a wide margin. Both instances
share a shape: the informative measurement was adjacent to the frozen one,
not identical to it, and the freeze did not prevent finding it — it just did
not ask for it.

## Two the retrospective itself missed, because it was written too early

This document was authored at Task 10, before the final whole-branch review
and before the absorption. Both found defects, and the count above was
originally "ten" for that reason alone — a retrospective written before the
last checks run is a retrospective that under-reports.

- **A floor that truncated where the rule it claimed to reuse rounds up.**
  The campaign chose `continent-count`'s proportional 0.5%-of-land floor
  *because* reusing it would give one definition of "is this a continent"
  rather than two that drift apart. The census compares in float
  (`s as f64 >= 0.005 * land as f64`); the plan specified
  `(0.005 * total_land as f64) as usize`, which truncates and therefore
  admits any component in `[trunc(floor), floor)`. It was the second
  definition. Measured over seeds 1-400 the two disagree on **four worlds**
  (111, 112, 221, 287); seed 111 has a 56-cell component against a floor of
  56.205. The stated rationale was the precise property the code lacked, and
  eight per-task reviews passed over it — it took a whole-branch reviewer
  holding both call sites at once. Fixed with `.ceil()`, a shared exported
  constant, and a `.max(2)` closing a related hole where a heavy-ocean world
  could reach a floor of zero and name every single-cell rock.
- **A generated artifact never regenerated.** The close commit added
  `docs/decisions/0147` and did not regenerate
  `docs/digest/decisions-in-force.md`, so the committed index omitted its own
  newest record.

## Absorb main at every stage boundary — the cost of skipping is stale premises, not conflicts

This campaign ran **ten tasks with zero absorptions, seventy-one commits
behind main**. Nothing caught it: not the local gate, not eight task reviews,
not the whole-branch review. It surfaced because Nathan asked whether the
branch had merged from main recently.

**A green stage gate is not a substitute, and the distinction is easy to
miss.** The stage gate merges main and the branch *in the chamber* and gates
that product, so it answers "would this survive contact with main today?"
mechanically. It does not tell you your **premises** held. Every number this
campaign measured — 183 land components, the floors yielding 10/1/80/106/208,
and the five committed H1 regression bands — was taken against the branch's
terrain, never against merged main's.

What the absorption found, in ascending order of what it would have cost:

1. **The premises survived by luck.** `git log HEAD..origin/main` over
   `domains/terrain/`, `domains/language/`, `windows/worldgen/` and `kernel/`
   returned **zero commits**: main's seventy-one commits never touched
   anything this campaign measures. Had they, five committed regression bands
   would have been wrong and nothing in the process would have said so.
2. **One conflict, in a generated file** — the decision digest, where main
   held 0146 while this branch held 0147. Resolved by *regenerating*, never
   hand-editing.
3. **The one that matters: `docs/audits/type-audit-report.md` auto-merged
   cleanly and was wrong.** `make gate-commit` refused the merge commit
   outright (`rc=2`, "type-audit-report is stale") and `make rebaseline` moved
   five lines in it. No-conflict and correctly-merged are unrelated properties
   for a generated file.

**The irony is worth keeping.** A *conflicted* merge runs `pre-commit`; a
clean one runs **no hook at all**. The digest conflict is the only reason the
bad type-audit report surfaced before merge time rather than after.

The practical form: at each stage boundary, `git merge origin/main`, then
`make rebaseline` **unconditionally**, then the drift check — and never infer
freshness from a clean merge. Ten minutes at each of ten boundaries is the
same ten minutes this cost at the end, minus the risk that an entire
campaign's measurements rest on a premise that has moved.

## Process notes

- **A good instinct, worth repeating.** Task 1's implementer distrusted a
  naive n=8 `time`-based cost comparison whose signal sat inside the
  quantization step's own noise, and re-measured with a repeat-count
  regression instead of reporting the naive number. That is the right
  reflex when a measurement's signal-to-noise ratio is not obviously large
  relative to what it is trying to detect.
- **A review was waived deliberately, not skipped silently.** Task 1's probe
  review was waived on the grounds that its only deliverable — a set of
  numbers — already carried a stronger check (exact multiset agreement with
  an independently-written shipped function, plus a production-path
  replay) than a reviewer reading the diff could add, and the probe code
  itself is deleted at campaign close. Recording the waiver and its reason
  is what keeps this different from an unreviewed task quietly passing.
- **An implementer correction of a plan's own weaker assertion.** Task 4's
  brief specified `big.len() <= all.len()` as a fixture assertion; the
  implementer tightened it to strict `<`, on the grounds that `<=` is
  satisfied by equality and proves nothing about the fixture actually
  splitting. Caught before review, by the person closest to running it.
- **A cross-campaign collision was posted, not resolved.** The Glasshouse's
  live `hold-off` on `domains/terrain/` names exactly the field this
  campaign's five committed magnitudes derive from. No gate can see a
  semantic collision like this one; the board post is the only mechanism
  available, and the correct repair if Glasshouse lands and these
  regression bands go red is to re-measure and move them, never to widen
  them to fit — a distinction that has to reach whoever does the repair by
  a note, since nothing mechanical will carry it.
