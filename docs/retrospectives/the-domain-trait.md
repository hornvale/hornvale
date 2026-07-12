# The Domain Trait — retrospective

**Completed:** 2026-07-12 — written at branch completion; merge pending owner review.

**Recurring findings.** The fmt/clippy gate held clean this campaign (the
canonical recurring skip did not recur). The one recurring-in-spirit finding
was a **gate that did not mirror CI**: the plan's final gate listed test, fmt,
clippy, docs-consistency, and artifact-drift but omitted the type-audit
check, which is default-deny in CI. The gap was invisible until an implementer's
own clippy/type-audit sweep flagged two untagged trait returns left by the
first task. Lesson banked in the plan itself (type-audit added to the gate);
the standing lesson is that a plan's "final gate" step must be diffed against
`.github/workflows/ci.yml`, not written from memory.

**Estimate deltas.** Four of five tasks were textbook transcription and ran
short. The third task — the roster — ran long and had to be **re-scoped
mid-flight** (see below), which cost one ideonomy pass, one owner decision, a
spec/plan rewrite, and a re-dispatch. That single task carried all the real
risk in the campaign; the other four were mechanical, as a registration-substrate
refactor should be.

**Spec vs. reality.** Two spec assumptions were wrong and both were caught
before they shipped:
- *"The manifest omits empty-stream domains."* Reading the committed manifest
  (not the spec's claim) showed it already renders streamless domains with a
  "no streams" line and omits only the deep-time layer — arbitrarily. The rule
  was inverted; the fix restored the missing section rather than adding an
  opt-out flag. **Read the ground-truth artifact, not the spec's memory of it.**
- *"Registration order is not observable."* The load-bearing byte-identity
  assumption was false: the language layer claims borrowed concepts under its
  own name when it registers before their owners. It surfaced as a hard
  registration conflict the moment the roster was alphabetized. The implementer
  **BLOCKED rather than papering over it** — exactly because the plan carried a
  "STOP if the diff is not empty" guard on the byte-identity check. That guard
  earned its keep; every determinism-sensitive task should carry one.

**What ideonomy bought.** Three separate passes each changed a decision, not
just decoration: the notation pass produced `env!(CARGO_PKG_NAME)` for the
manifest key and the anti-god-interface non-goal; the ground-truth read
overturned the omit-empty rule; and the combination/procedure pass on the
registration conflict reframed it as a *conflated verb* (`register_concept`
doing both "claim ownership" and "ensure present") and yielded a principled
follow-up — a distinct reference primitive — that was captured as a durable
registry row and deferred rather than lost or over-built into this campaign.

**Do differently next time.** (1) Diff the plan's final gate against CI's job
list before execution, so a missing check is found at planning time, not by an
implementer's off-scope sweep. (2) When a spec asserts an artifact's shape,
open the artifact during planning and confirm — the two cheapest corrections
this campaign both came from reading the file the spec described. (3) A refactor
whose correctness rests on "X is unobservable" should treat that claim as the
first thing to test, not the last: here it was the entire risk, concentrated in
one task, and the byte-identity guard is what turned a latent catastrophe into
a clean mid-flight re-scope.
