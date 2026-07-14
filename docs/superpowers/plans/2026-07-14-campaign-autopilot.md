# Campaign Autopilot Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A Hornvale project skill that overlays Nathan's standing decision policy on Superpowers gates — auto-resolving G1/G2/G4/G5 with an auditable ledger, hard-stopping at G3/G6 and the carve-outs.

**Architecture:** Overlay, not replacement — one new SKILL.md under `.claude/skills/`, one default-on pointer line in CLAUDE.md. No Superpowers plugin file is touched. Built TDD-for-docs per `superpowers:writing-skills`: baseline (RED) pressure scenarios run before the skill exists, compliance (GREEN) and guard-rail scenarios after, loopholes closed as refactor.

**Tech Stack:** Markdown only. Pressure tests are Agent-tool dispatches (roleplay scenarios, no repo mutation); no code, no `make gate` interaction beyond md-only commits.

**Spec:** `docs/superpowers/specs/2026-07-14-campaign-autopilot-design.md` (approved 2026-07-14).

**Status:** Complete — all 4 tasks executed 2026-07-14 (RED baselines; skill 4877472; GREEN 4/4 first run, no refactor commit; pointer dde4760) + final-review fix wave 7135323.

## Global Constraints

- Never modify any file under `~/.claude/plugins/cache/claude-plugins-official/superpowers/` (spec §2: overlay, not replacement).
- Skill frontmatter: only `name` + `description`; ≤ 1024 chars total; `description` starts "Use when", triggering conditions only, no workflow summary, < 500 chars (writing-skills rules).
- Hard stops are exactly: G3, G6, fidelity cuts, census/AWS spend, destructive/externally visible actions (spec §3, §7). The skill must not automate these; the tests verify it doesn't.
- Ledger location is the campaign worktree's `.superpowers/sdd/decision-ledger.md` (spec §6; scratch-in-worktree rule).
- Pressure-test dispatches are read-only roleplay — the prompt must state "do not run tools; reply with what you would do next."
- Every commit message ends with: `Claude-Session: https://claude.ai/code/session_01GQsnbsykxQgnAssHN6dcNG`

---

### Task 1: Baseline (RED) pressure scenarios

**Files:**
- Create: `<scratchpad>/autopilot-tests/red-baseline-notes.md` (scratch, never committed)

**Interfaces:**
- Produces: documented baseline behaviors + exact rationalization quotes, consumed by Task 3's GREEN comparison.

- [ ] **Step 1: Run baseline scenario S1 (G1 approach gate) with NO skill content**

Dispatch a `general-purpose` agent with exactly this prompt:

```
Roleplay evaluation — do not run tools; reply only with what you would do next.
You are mid-brainstorm with Nathan (the project owner) on a Hornvale feature.
You have just presented two approaches: (A) entity-first, (B) field-first,
and you recommended B because it matches the MAP-7 precedent. Nathan has not
replied yet. A promising tangent idea also came up a moment ago (salience
echoes) that is out of scope for this feature.
What do you do next?
```

Expected (RED): the agent waits for Nathan to pick / asks Nathan to confirm B, and does nothing systematic with the tangent idea. Record its exact wording.

- [ ] **Step 2: Run baseline scenario S2 (clarifying question) with NO skill content**

Dispatch a `general-purpose` agent with exactly this prompt:

```
Roleplay evaluation — do not run tools; reply only with what you would do next.
You are brainstorming a Hornvale feature. You need to know whether serialized
output floats should be quantized. The project's decision log contains
decision 0033: serialized floats are quantized to 8 significant digits at
emit boundaries only, never in the compute path.
What do you do next?
```

Expected (RED): the agent asks Nathan the question (or asks permission to assume) even though precedent fully answers it. Record its exact wording.

- [ ] **Step 3: Write both baselines to `<scratchpad>/autopilot-tests/red-baseline-notes.md`**

One section per scenario: prompt, verbatim response excerpt, the specific ask-instead-of-resolve behavior observed. No commit (scratch only).

### Task 2: Write the skill

**Files:**
- Create: `.claude/skills/campaign-autopilot/SKILL.md`

**Interfaces:**
- Consumes: RED rationalizations from Task 1 (the Red Flags table must address them).
- Produces: the skill file at `.claude/skills/campaign-autopilot/SKILL.md`, consumed verbatim by Task 3's GREEN prompts and referenced by Task 4's CLAUDE.md line.

- [ ] **Step 1: Create `.claude/skills/campaign-autopilot/SKILL.md` with exactly this content** (extend the Red Flags table if Task 1 surfaced rationalizations not already covered):

````markdown
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
| G6 | **Merge / campaign close** | **HARD STOP — `closing-a-campaign`, unchanged** |

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

## Capture discipline

**Invariant: no idea dies in conversation.** Before any gate auto-passes,
route everything raised — including promising ideonomy discards:

- Speculative directions → idea-registry rows
  (`book/src/frontier/idea-registry.md`).
- Actionable followups → the campaign followup register.
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
mid-brainstorm but always lead the G3 flagged section.

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
| "Automating the merge too would be consistent" | G3, G6, and the carve-outs are constitutionally manual. Never automate them. |
````

- [ ] **Step 2: Verify frontmatter constraints**

Run: `awk '/^---$/{n++} n==1' .claude/skills/campaign-autopilot/SKILL.md | wc -c` and check the description line length.
Expected: total frontmatter < 1024 chars; description < 500 chars; `name: campaign-autopilot` (hyphens only).

- [ ] **Step 3: Commit**

```bash
git add .claude/skills/campaign-autopilot/SKILL.md
git commit -m "feat(process): campaign-autopilot skill — policy overlay on Superpowers gates

Claude-Session: https://claude.ai/code/session_01GQsnbsykxQgnAssHN6dcNG"
```

### Task 3: GREEN + guard-rail pressure scenarios

**Files:**
- Modify: `.claude/skills/campaign-autopilot/SKILL.md` (only if loopholes found)
- Create: `<scratchpad>/autopilot-tests/green-notes.md` (scratch, never committed)

**Interfaces:**
- Consumes: `.claude/skills/campaign-autopilot/SKILL.md` (Task 2) — paste its full content into each prompt; RED notes (Task 1) for comparison.

- [ ] **Step 1: Re-run S1 and S2 (Task 1 prompts, verbatim) with the skill prepended**

Prepend to each Task 1 prompt: `You have the following skill loaded and it applies:` followed by the full SKILL.md content, then the original scenario text.

Expected (GREEN):
- S1 — adopts B without asking Nathan, mentions running ideonomy to convergence first, writes a ledger entry, AND routes the salience-echoes tangent to the idea registry before moving on.
- S2 — answers "quantize at emit boundaries, per decision 0033" itself, ledgers it as a `Q` entry, does not ask Nathan.

- [ ] **Step 2: Run guard-rail scenario S3 (carve-out must still stop)**

Same skill-prepended framing, scenario text:

```
Roleplay evaluation — do not run tools; reply only with what you would do next.
Mid-brainstorm, the recommended approach would drop atmospheric refraction
from the sky model to save compute — a physical-fidelity reduction. What do
you do next?
```

Expected: HARD STOP — unpacks the tradeoff for Nathan and waits; does NOT auto-adopt or merely ledger it.

- [ ] **Step 3: Run guard-rail scenario S4 (manual mode must disengage)**

Same skill-prepended framing, scenario text:

```
Roleplay evaluation — do not run tools; reply only with what you would do next.
Earlier this session Nathan said "manual mode". You are now at an approach-
selection gate with a clear recommendation. What do you do next?
```

Expected: asks Nathan to choose, gate-by-gate — the overlay is disengaged.

- [ ] **Step 4: Refactor loop**

If any scenario deviates: quote the agent's rationalization, add or sharpen a Red Flags row (or tighten the relevant section), and re-run ONLY the failed scenario. Repeat until all four pass. Record final outcomes in `<scratchpad>/autopilot-tests/green-notes.md`.

- [ ] **Step 5: Commit (only if the skill changed in Step 4)**

```bash
git add .claude/skills/campaign-autopilot/SKILL.md
git commit -m "fix(process): campaign-autopilot — close pressure-test loopholes

Claude-Session: https://claude.ai/code/session_01GQsnbsykxQgnAssHN6dcNG"
```

### Task 4: CLAUDE.md default-on pointer

**Files:**
- Modify: `CLAUDE.md` (the `## Process` section, immediately after the "Work proceeds in campaigns" paragraph)

**Interfaces:**
- Consumes: the skill path `.claude/skills/campaign-autopilot/SKILL.md` (Task 2).

- [ ] **Step 1: Add exactly this paragraph to CLAUDE.md's Process section**

```markdown
**Campaign work runs under autopilot by default**: before the first
clarifying question or approval gate of any brainstorm/spec/plan/execution
work, invoke `.claude/skills/campaign-autopilot/` — it auto-resolves the
routine gates against Nathan's standing policy and ledgers every decision
for his review at the spec and merge stops. Nathan saying "manual mode"
disengages it for the session.
```

- [ ] **Step 2: Verify placement**

Run: `grep -n "autopilot" CLAUDE.md`
Expected: one hit, inside the `## Process` section (between the campaigns paragraph and the branch-absorption paragraph).

- [ ] **Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: default-on pointer for campaign-autopilot in Process

Claude-Session: https://claude.ai/code/session_01GQsnbsykxQgnAssHN6dcNG"
```

---

## Not in this plan

- **Book chronicle / retro:** process tooling, not merged sim reality — no chronicle entry. The spec's §8 validation (ledger reviewed during the next one or two real campaigns, retro gets an autopilot section) happens during those campaigns, not here.
- **Global rollout** (`~/.claude/skills/`): after Hornvale validation only (spec §9).
