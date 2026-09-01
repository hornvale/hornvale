---
name: campaign-autopilot
description: Use when starting or continuing Hornvale brainstorm, spec, plan, or campaign-execution work that runs Superpowers process skills — invoke before the first clarifying question or approval gate. Do not apply after Nathan has said "manual mode" this session.
---

# Campaign Autopilot

## Overview

Overlay, not replacement: the Superpowers skills still run and still own
process; this skill changes only how their decision points resolve. It
encodes Nathan's standing policy (spec:
`docs/superpowers/specs/2026-07-14-campaign-autopilot-design.md`) so his
oversight is asynchronous — a decision ledger reviewed at two hard stops —
instead of gate-by-gate ratification.

## The gate policy

| Gate | Point in pipeline | Policy |
|------|-------------------|--------|
| G1 | Approach selection | Auto-adopt the recommendation after ideonomy convergence; ledger entry |
| G2 | Design section approvals | Self-review against spec consistency; proceed; ledger entry |
| G3 | **Spec review before planning** | **HARD STOP — present the G3 package, wait for Nathan** |
| G4 | Plan review before execution | Self-review against the approved spec; proceed; ledger entry |
| G5 | Per-task execution checkpoints | Existing subagent review machinery; auto-continue on green; the 3-attempt rule still stops on red |
| G6 | **Merge / campaign close** | **HARD STOP — present the post-G3 ledger digest, wait for Nathan; then `closing-a-campaign`, unchanged** |

## Clarifying questions: answer from precedent

Do not ask Nathan. Search, in order: decision log (`docs/decisions/`) →
specs (`docs/superpowers/specs/`) → idea registry
(`book/src/frontier/idea-registry.md`) → memory → book → repo convention.
Adopt the precedented answer and ledger it (entry type `Q`).

Surface a question to Nathan only when BOTH hold: no precedent anywhere in
that chain, AND the candidate answers diverge materially in outcome.
Expected volume: zero to two per brainstorm.

## Ideonomy convergence

At each G1 decision and each nontrivial `Q`, run `ideonomy-plain` passes
(expansion, inversion, implication-mining, symmetry-hunting) until the
latest pass produces no material improvement — no new options, overturned
assumptions, or spec-worthy implications. Convergence is your judgment; at
least one pass, no hard cap. If a pass overturns the initial
recommendation, adopt the improved version — the policy is "no waiting for
ratification," not "first answer wins." Note pass count and overturns in
the ledger entry. Promising discards feed the capture discipline.

**Why run it on the obvious answers.** A pass is for *disruption*, not a
second opinion, so it earns the most exactly when the answer feels like a
no-brainer: it comes back "yes, of course — *and also* the X, Y, Z you
weren't weighing," not a reversal. Overturns are rare; enrichment is the
norm and the point. So the settledness of an answer is a reason to run a
pass, never a reason to skip one.

**"Each nontrivial `Q`" means each — not just G1, and a precedent- or
science-grounded answer is NOT exempt.** The precedented answer is the
*input* to a pass, not a substitute for one; run at least one. **The
check:** a nontrivial `Q` whose ledger entry shows zero passes means you
skipped this step — the `ideonomy passes / overturns` field is required and
non-zero, not optional.

## Verify generated-artifact and tool-behavior claims

Before writing "this generated output won't change (much)" or "this tool
behaves this way on failure" into a spec, plan, or ledger entry: run the
actual command and cite its real output, not a claim inferred from
reasoning about what should happen. This recurred four times across
three campaigns in one session (PROC-18, PROC-19, and PROC-12 twice — the
second time inside PROC-12's own *correction* of the first instance)
before being named here: each claim was plausible, stated with
confidence, and wrong, caught only when a subagent later reproduced it
empirically.

**The check:** any sentence of the shape "X will/won't happen" or "the
diff is only Y" about a generated file, a tool's documented behavior, or
git's own merge/conflict handling needs a command-and-output pair next to
it before it goes in the spec — at drafting time, not review time.
"Regenerated it and diffed: `<result>`" is a verification. "This should
only touch two rows" is not, however confident the reasoning sounds.

**Why drafting time, not just G2/G3 self-review:** every one of the four
instances was caught by a *reviewer*, never the drafting session — the
self-review step already existed and still missed all four, because it
checked the spec's internal consistency, not whether its claims about
external systems had been verified at all. Fold this into G2's own
self-review pass explicitly; don't rely on it being implied by "spec
consistency."

### Imperative mood hides assertions — the reason the check above kept missing

The Quire (2026-08-08) put **five** defects into plan text underneath this
very section. None was caught by it, and the reason is mechanical rather
than a lapse: the check hunts for sentences "of the shape *X will/won't
happen*," and **not one of the five had that shape.** Every one was an
imperative with an outcome smuggled inside:

| what it looked like | what it actually asserted | how it was wrong |
|---|---|---|
| "Run `make rebaseline` and confirm an empty diff" | the diff *will* be empty | the task added `pub` items, so the type-audit report always drifts |
| "Swap two adjacent derivations and confirm the test fails" | the swap *is* observable | both are pure and share no stream; 490 tests stayed green |
| "`git add windows/vessel cli/src/main.rs`" | that *is* the complete file set | adding a struct field breaks every full-literal construction site |
| "`possess --seed 42 --snapshot X`" | the command *terminates* | no `--script`, so it blocks on stdin |
| "root cause `c25bb1d2`" | that *is* the provenance | `c25bb1d2` changed only a comment; the cause was a later merge |

A step reads as a thing to *do*, so the claim inside it is never audited.
So the check is not "scan for predictions" — it is **scan every imperative
for the outcome hiding inside it.** Two rules follow, and they are cheaper
than more scanning:

- **Write decision rules, not predictions.** Instead of "Expected: empty
  diff," enumerate the branches: *`book/src/gallery/` moved → STOP, epoch
  event; only `docs/audits/` moved → regenerate and commit in the same
  commit.* A prediction can be wrong; a branch table covering the responses
  cannot, and it is more useful to the implementer. The Quire's first
  defect is pure self-inflicted proof: that exact branch table was written
  correctly into Task 2 and omitted from Task 1.
- **Never prescribe a specific mutation from outside the code.** Name the
  *property* the mutation must demonstrate and let the implementer find
  one. A plan author does not know which derivations share a stream; the
  implementer does, after reading. Both prescribed mutations in The Quire
  were nulls, and in both cases the implementer found a discriminating one
  by hunting — the outside guess was strictly worse than the inside search,
  every time.

**What not to do about it.** A plan pre-flight that runs every command
before the plan commits is the obvious heavyweight fix and does not earn
its cost: it would have caught three of the five, missed the Critical one
entirely (see the stdin note below), and missed the provenance one. What
did catch all five was every plan step demanding executable proof, plus
implementers empowered to override the plan and say so in their report.
Keep that sharp in preference to adding a gate in front of it.

**The drafting environment is not the execution environment.** An agent's
stdin is at EOF, it has no TTY, and the box may be loaded. So a command
that reads stdin *cannot* be validated by running it here — it will pass
for you and hang for the human at a terminal. That is what made The
Quire's stdin defect Critical and structurally invisible to the session
that wrote it. Same family: timings taken on a contended box (that
campaign discarded a 5-run pair taken at load average 50, which was 3.3x
wrong).

### Four more claim-classes, each with a command that settles it

The Benchmark and The Handle added seven more plan-text defects (2026-08-06/07),
**all in the author's own text, none in implementers' code**, and each was
caught by something *executable* rather than by re-reading. Plan review compares
a document to a document; these were claims about the world. Extend the rule:

| claim in a spec or plan | what settles it, at drafting time |
|---|---|
| "retyping this / changing this signature is safe" | stub the change and `cargo check --workspace --all-targets`. The Benchmark's operator design died on 21 errors, two of which were quantities the type would have *lied* about. |
| "this invariant holds" | measure it against real data before writing it down. A spec asserted a room on dry land never reports a negative height; 18.5% of rooms did, and the probe that would have shown it had already been written. |
| "the N sites are …" | grep for the **observable** the behaviour produces — a shared output string, an error message — not for the function you happened to open. `examine` had two matchers; teaching one never taught the other, twice. |
| "call `foo()` / `foo` returns X" | read the signature. Plans named a method clippy bans by policy and asserted the wrong return prefix for another. |

**And two rules about the verification itself, both learned the hard way:**

- **A mutation test must prove it mutated.** Assert the target text exists
  before substituting it (`assert old in s, "TARGET NOT FOUND"`). A `cargo fmt`
  rewrap once made a single-line replacement match nothing, and the resulting
  green looked exactly like a robust implementation. A no-op mutation is worse
  than no mutation, because it produces evidence.
- **A RED from a compile error proves nothing about an assertion.** Where the
  type under test does not exist yet, capture the *behavioural* red from the
  live surface first, before touching code. "Fails to compile" is not "would
  have caught the defect".

**Name the direction a check enforces**, in its own doc comment. A gate
asserting *declared ⊆ resolvable* is structurally blind to over-admission and
still reads as total to the next person. A check that states its direction
cannot be silently mistaken for a guarantee.

## Capture discipline

**Invariant: no idea dies in conversation.** Before any gate auto-passes,
route everything raised — including promising ideonomy discards:

- Speculative directions → idea-registry rows
  (`book/src/frontier/idea-registry.md`).
- Actionable followups → the campaign followup register
  (`.superpowers/sdd/followups.md` in the worktree; promoted into the
  campaign retrospective's follow-up section at close).
- Process lessons → retro / memory.
- Rejected branches → ledger, with the reason.

## The decision ledger

Location: `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md` — a committed,
per-campaign document (The Cartulary, decision 0486), not the vendored
plugin's per-worktree `.superpowers/sdd/<campaign>/progress.md` (which keeps
its own separate job — task state, fix rounds, resume-after-compaction
material — and stays scratch; see spec §4a). Create it at the first
auto-resolved decision, committing as each entry is written rather than
batching. **"Never the shared main checkout" still governs**, in its
original form: the ledger is written on the campaign's own branch, in the
campaign's own worktree, and lands on `main` only through the normal merge
path — the same discipline that already applies to every other file a
campaign writes. What changed is that the file is no longer git-ignored
scratch; the rule about *which checkout* was never about that. Entry
format:

```
#N [G1|G2|G4|G5|Q] — question · decision · why (precedent cited) ·
alternatives discarded · ideonomy passes / overturns · capture actions
```

**A backfilled entry** — one recording a ruling made before the ledger
existed to capture it contemporaneously (pre-flight scan findings, or
decisions made during spec/plan authoring before Task 1 of a campaign like
The Cartulary created its own ledger) — may omit `ideonomy passes /
overturns` if no pass was ever run for it, provided the entry says so
explicitly rather than leaving the field silently blank.

**Two shapes of live entry, not one, and both are still required to state
question · decision · why · alternatives discarded · capture actions —
only the LAYOUT differs.** A cross-task ruling made outside any single
task's own review loop (a pre-flight conflict between two tasks, a
mid-campaign Q consultation with Nathan, an ad hoc controller call) uses
the numbered `#N [G1|G2|G4|G5|Q] — ...` line above, because nothing else
is going to organize it. A ruling that IS the subject of a task's own
"Task N — complete" or "Task N — fix round" section inherits that
section's own narrative structure instead — the review that produced the
section already interrogated the question, the decision, the why and what
was rejected, so re-flattening it into the one-line tagged form would
duplicate content already present in a fuller form, not add rigor. This is
a correction, not a new rule: The Cartulary's own exemplar ledger
(`docs/superpowers/ledgers/2026-08-30-the-cartulary.md`) wrote every live
ruling this second way from Task 1 onward and none the first way, which an
earlier draft of this paragraph called a violation of "no exemption" — a
false reading of the campaign's own practice, caught by its final review
(finding M2). `ideonomy passes / overturns` is still a required, non-zero
field either way — a task-boundary ruling states it in the task section's
own prose (e.g. "no ideonomy pass was run for this" or naming the pass
taken) rather than as a labelled slot.

## The G3 package

**Before assembling the package, walk the ledger and verify every type-`Q`
entry shows ≥1 ideonomy pass.** A zero-pass `Q` is a skipped step — go run
the pass and update the entry before the package goes out, not after Nathan
asks. This is the verification point for the required-slot rule above.

At the spec-review stop, present one message containing, in order:

1. **Flagged items** — schema-adjacent or save-format/epoch calls,
   low-confidence assumptions (no precedent either way), anything near a
   carve-out.
2. **Ledger digest** — one line per entry, link to the full ledger.
3. **Capture manifest** — what got recorded where; rejected branches with
   reasons.
4. The spec path.

If Nathan vetoes an entry, revise the spec and re-present. Promote material
ledger entries into the spec's decisions section regardless — the ledger
is committed and durable now, but it is a per-campaign narrative, not the
cross-campaign decision log; anything meant to bind future campaigns still
needs a decision record of its own.

## Carve-outs — hard-stop regardless of gate

- **Fidelity cuts / accuracy tradeoffs** — always unpacked and brought to
  Nathan.
- **Census regen** — NO LONGER A CARVE-OUT (decision 0514, 2026-09-01).
  `make sluice-census BRANCH=<branch> REF=<full-sha>` is ordinary queued work;
  dispatch it without asking. Every hazard the old carve-out named has been
  closed by a later decision — the host guard (0079), the one serial claim and
  the census's own FIFO row (0133), the anchored worktree default (0146), and
  the fact that a census pushes a `census/<ref>-<stamp>` branch and never
  `main` (0139). Cost is ~15 min (read `docs/timings.md`, not prose). What is
  still gated: LANDING the moved goldens, which goes through the merge queue
  and G6 like any candidate.
- **AWS spend** — RETIRED, not relaxed. Decision 0063 deleted the thing this
  governed; `scripts/aws-gate/` and `make regen-remote` are abandoned. Listing
  a hazard that cannot occur trains readers to skim the list that also holds
  the live ones.
- **Destructive or externally visible actions** — force-pushes, deletions,
  publishing.

Save-format / epoch / determinism-contract decisions do not hard-stop
mid-brainstorm but always lead the G3 flagged section. Entries ledgered
after G3 (during plan or execution) are presented as a digest at G6, with
save-format / epoch / determinism-contract entries leading it.

## Manual mode

If Nathan says "manual mode" (or equivalent) at any point, disengage this
overlay for the rest of the session — every gate returns to gate-by-gate
ratification. Re-engage only when he says so.

## Red flags — you are rationalizing

| Thought | Reality |
|---------|---------|
| "This one is too important to answer myself" | Check the carve-out list. Not on it? Answer from precedent and ledger it. Note the list SHRANK on 2026-09-01 (decision 0514) — a census refresh is no longer on it. |
| "I'll ledger these at the end" | Ledger BEFORE the gate passes. Batching loses entries. |
| "The tangent isn't worth recording" | The invariant is unconditional: capture it or ledger its rejection with a reason. |
| "Nathan probably wants to see this section" | That is what the G3 package is for. Proceed. |
| "It converged after zero passes" | Convergence requires at least one ideonomy pass that came back empty. |
| "This `Q` is science-grounded / precedent-answered, so ideonomy adds nothing" | The precedented answer is the INPUT to a pass, not a substitute for it. Run at least one. The most confident-looking `Q`s are where a pass catches the stability bug or the wrong model — skipping ideonomy on the "obvious" ones is this overlay's documented first-campaign failure. |
| "I ran a pass at G1, so ideonomy is handled for this campaign" | Ideonomy is per-decision, not per-campaign. G1 does not bank passes for the later `Q`s. |
| "Automating the merge too would be consistent" | G3, G6, and the carve-outs are constitutionally manual. Never automate them. |
| "I just recommended, but Nathan hasn't weighed in — the next move isn't mine" | Under autopilot the recommendation IS the decision: run ideonomy to convergence, adopt, ledger, keep moving. Nathan reviews at G3. |
| "Writing to the idea registry mid-brainstorm without asking feels presumptive" | Capture is pre-authorized — recording ideas without asking is this skill's core mandate. |
| "This diff/behavior claim is obviously right, no need to check" | PROC-18, PROC-19, and PROC-12 (twice) were all exactly this confident and all wrong. Run the command before writing the claim. |
