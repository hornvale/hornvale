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

Location: the campaign worktree's `.superpowers/sdd/decision-ledger.md`
(scratch-in-worktree rule — never the shared main checkout). Create it at
the first auto-resolved decision. Entry format:

```
#N [G1|G2|G4|G5|Q] — question · decision · why (precedent cited) ·
alternatives discarded · ideonomy passes / overturns · capture actions
```

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
ledger entries into the spec's decisions section — the scratch ledger is
not the durable record.

## Carve-outs — hard-stop regardless of gate

- **Fidelity cuts / accuracy tradeoffs** — always unpacked and brought to
  Nathan.
- **Census regen / AWS spend** — explicit authorization only.
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
| "This one is too important to answer myself" | Check the carve-out list. Not on it? Answer from precedent and ledger it. |
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
