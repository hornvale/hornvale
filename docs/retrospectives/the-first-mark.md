# Retrospective — The First Mark

Process lessons, not product. The product is chronicled; this is what the
*way we worked* taught us.

## What worked

- **An honest BLOCK on an invariant is signal, not failure.** Task 2's
  implementer discovered that seed 42 at rest has no latent ambient tension
  and refused to tune a constant that would have manufactured one — which
  the "bias not manufacture" invariant forbids. It reported BLOCKED with the
  measured drive values rather than faking the effect. That block was the
  single most valuable event in the campaign: it surfaced a real design fact
  (a world at rest has no near-threshold drive) that the spec had assumed
  away, and the dispatch/review machinery correctly escalated it to an owner
  decision instead of letting a subagent paper over it. The instruction to
  *escalate, don't guess* — with explicit permission that a BLOCKED here was
  the right outcome — is what made the honest refusal safe to give.

- **Grounding the plan against real APIs, before writing it, made the
  scariest flag evaporate.** The spec led its G3 flags with a save-format-
  contract change (a provenance marker on the `Fact` envelope). Reading the
  actual envelope during planning showed it *already* carried a `provenance`
  string — so the "change" became a no-op, no epoch, no new field. The
  lesson restates a standing one: ground against the code, not the spec's
  mental model of the code.

- **The mid-execution pivot rippled cleanly because the ledger/spec/plan
  were the single source of truth.** When the mechanic changed from
  ambient-bias to direct-social-consequence, threading it through one ledger
  entry, targeted spec edits, and a plan re-scope of Tasks 2–3 let a *fresh*
  re-dispatched implementer work from the corrected brief with zero stale
  context. Re-scoping the artifact, not just re-instructing the agent, is
  what kept the fleet aligned.

- **Absorbing main at close resolved the inherited red for free.** The gate
  was red on a census-calibration test — inherited from `origin/main`, where
  *The Lode* had added census metrics without a regen. Absorbing main at
  close (before spending the authorized ~7-minute census regen) pulled in
  *The Solitary Tongue*'s re-pin, which had already chased that drift back.
  The regen was never needed. **Absorb main before running any expensive
  regen at close — a parallel campaign may have already fixed it.**

## What to watch

- **The "all-metrics census owes a regen per added metric" trap bit again —
  upstream.** The Lode added lab metrics and closed without the census
  regen, reddening `branches_family_calibration` on main for everyone who
  branched after it. It cost us a real investigation to prove it wasn't
  ours. The standing memory note held; the gap is that nothing in a
  campaign's own gate catches a *metric-add-without-regen* at the adding
  campaign's close.

- **First meeting with main was at close.** This branch did not absorb main
  mid-flight (main advanced `07fd15a7 → 210e85ec` during execution). It was
  harmless here — the execution ran in a single fast session and the landed
  campaign (*The Solitary Tongue*) touched none of our code (only two
  doc/artifact files conflicted, both trivially resolved). But the cadence
  in CLAUDE.md is stage-boundary absorption, and a longer campaign would not
  have been so lucky. Worth an explicit mid-execution `make preflight` next
  time even when the session feels short.

## Autopilot notes

Campaign-autopilot carried two hard stops (G3 spec approval, G6 close) and
surfaced exactly what it should: one owner decision mid-execution (the
mechanic pivot, via AskUserQuestion when a subagent's honest block had no
precedent) and one carve-out (the census regen, explicitly authorized —
then found unnecessary). Zero routine gates reached the owner. The overlay
did its job: asynchronous oversight, not gate-by-gate ratification.
