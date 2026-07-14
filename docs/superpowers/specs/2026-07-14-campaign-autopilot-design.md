# Campaign Autopilot — design

**Date:** 2026-07-14
**Status:** Approved (brainstorm complete)
**Scope:** Hornvale only, deliberately — graduation to a global skill is a
copy-and-generalize after it proves itself here.

## 1. Problem

The Superpowers pipeline pauses for Nathan at many decision points. In
practice his responses are highly predictable: accept the recommended
approach, ask for followups to be recorded, ask for an ideonomy pass to
shake out latent possibilities. His rare interventions have one shape —
making sure a good idea was captured before the conversation moved on.

Those gates bundle two jobs: **extracting preferences** (solved — the
preferences are stable and known) and **catching errors** (must survive, but
can be asynchronous). This skill encodes the preferences as standing policy
and replaces synchronous ratification with a capture discipline plus an
auditable decision ledger.

## 2. Shape and invocation

- A project skill: `.claude/skills/campaign-autopilot/SKILL.md`, sibling to
  `closing-a-campaign` and `dispatching-hornvale-subagents`.
- **Overlay, not replacement.** Superpowers skills still run and still own
  process; the overlay changes only how their decision points resolve.
  Nothing in the Superpowers plugin is forked or edited, so plugin updates
  cannot break it.
- **Default-on in this repo** via one line in CLAUDE.md's Process section:
  brainstorm/campaign work invokes campaign-autopilot first. (Skill
  trigger descriptions alone fire unreliably; the CLAUDE.md pointer is what
  makes default-on real.)
- **Escape hatch:** Nathan saying "manual mode" (or equivalent) disengages
  the overlay for the session, including mid-campaign. Manual is the marked
  exception; autopilot is the default.

## 3. Decision policy (the gates)

| Gate | Point in pipeline | Policy |
|------|-------------------|--------|
| G1 | Approach selection (2–3 options + recommendation) | Auto-adopt after ideonomy convergence (§4); ledger entry |
| G2 | Design section approvals | Self-review against spec consistency; proceed; ledger entry |
| G3 | **Spec review before planning** | **HARD STOP — Nathan reviews the G3 package (§6)** |
| G4 | Plan review before execution | Self-review against approved spec; proceed; ledger entry |
| G5 | Per-task execution checkpoints | Existing subagent review machinery; auto-continue on green; the 3-attempt rule still stops on red |
| G6 | **Merge / campaign close** | **HARD STOP — post-G3 ledger digest, then `closing-a-campaign` unchanged, Nathan-gated** |

**Clarifying questions** are answered from precedent instead of asked, in
priority order: decision log (`docs/decisions/`) → specs → idea registry →
memory → book → repo convention. The assumed answer is ledgered. A question
reaches Nathan only when it is genuinely novel (no precedent anywhere) AND
the candidate answers diverge materially. Expected volume: zero to two per
brainstorm, down from eight to twelve.

**Ideonomy runs before adoption, not on request.** If a pass overturns the
initial recommendation, the improved version is what gets adopted — the
policy is "no waiting for ratification," not "first answer wins."

## 4. Ideonomy convergence loop

At each G1 decision and each nontrivial answered question, run
`ideonomy-plain` passes (expansion, inversion, implication-mining,
symmetry-hunting) until a pass produces no material improvement — new
options, overturned assumptions, or spec-worthy implications. Convergence is
a judgment call; no hard pass cap. Each pass's promising discards feed the
capture discipline (§5); the pass count and any overturns are noted in the
ledger entry.

## 5. Capture discipline

**Invariant: no idea dies in conversation.** Before any gate auto-passes,
everything raised — including promising ideonomy discards — is routed:

- Speculative directions → idea-registry rows
  (`book/src/frontier/idea-registry.md`).
- Actionable followups → the campaign followup register.
- Process lessons → retro / memory.
- Rejected branches → logged in the ledger with the reason.

This mechanizes Nathan's one intervention pattern and is what makes
unattended gates safe.

## 6. Decision ledger and the G3 package

Every auto-resolved decision gets a ledger entry in the campaign worktree's
`.superpowers/sdd/decision-ledger.md` (per the scratch-in-worktree rule):

```
#N [G1|G2|G4|G5|Q] — question · decision · why (precedent cited) ·
alternatives discarded · ideonomy passes / overturns · capture actions
```

At G3, Nathan receives one package:

1. **Flagged items first** — schema-adjacent or save-format/epoch calls,
   low-confidence assumptions (no precedent either way), anything near a
   carve-out.
2. **Ledger digest** — one line per auto-resolved decision, link to the full
   ledger.
3. **Capture manifest** — what got recorded where; rejected branches with
   reasons.
4. The spec itself.

Vetoing any entry revises the spec and re-presents. Material ledger entries
are promoted into the spec's decisions section; the scratch ledger is not
the durable record.

## 7. Carve-outs (hard-stop regardless of gate)

- **Fidelity cuts / accuracy tradeoffs** — always unpacked and brought to
  Nathan (standing rule).
- **Census regen / AWS spend** — explicit authorization only (standing
  rule).
- **Destructive or externally visible actions** — force-pushes, deletions,
  publishing.

Save-format / epoch / determinism-contract decisions do not hard-stop
mid-brainstorm but always appear at the top of the G3 flagged section.
Entries ledgered after G3 (during plan or execution) are presented as a
digest at G6, with save-format / epoch / determinism-contract entries
leading it.

## 8. Validation

The first one or two campaigns run under autopilot treat the ledger as the
test artifact: at G3, Nathan checks whether any auto-decision would have
been vetoed. Zero vetoes means it is working; systematic vetoes mean the
policy or the precedent search gets fixed, not worked around. Those
campaigns' retros include an autopilot section. Implementation follows
`superpowers:writing-skills` (skill tested before deployment).

## 9. Out of scope

- Global (`~/.claude/skills`) rollout — after Hornvale validation only.
- Any modification to Superpowers plugin skills.
- Automating G3, G6, or the carve-outs.
