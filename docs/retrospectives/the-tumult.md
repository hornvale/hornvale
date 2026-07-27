# Retrospective — The Tumult (Predation)

*Living-community program, campaign 3, slice 1. Process lessons, not product.*

## What went right

- **Building the wrong thing and measuring it was cheaper than arguing about
  it.** The crowding sandpile was specified, approved at G3, built across three
  tasks, and falsified — and the falsification is what produced the reframe. The
  measurement said two things no design review would have: that seed 42 never
  crowds at all, and that where it does the avalanches are a depth-cap
  truncation artifact rather than a tail. Six ideonomy passes then converged on
  drive-plus-dissipation, and the salvage was near-total (the ring-walking BFS,
  the relocation recursion, the cascade histogram, the measurement entry point,
  the gate scaffolding all carried forward). The plan edited *forward* from the
  falsified branch rather than reverting it, which is why a full re-spec cost
  one task, not a campaign.

- **The preregistered gates caught deviations, not just regressions.** The
  first shipped raid rule depopulated the world (alive-at-now 138 → 30) and
  three sane-band gates went red. The correct read was not "calibrate the
  constants" but "the implementation deviated from the spec" — and it had. Every
  gate that fired traced to the deviation, and fixing the deviation fixed the
  gates with no constant tuned and no band moved. A gate that reddens is
  evidence about *which* thing is wrong; treat the diagnosis as the first step,
  not the tuning.

- **The falsification shipped as a falsification.** No constant was moved toward
  a power law at any point. The headline is σ ≈ 0.051 against a critical value
  of 1, replicated across a 3.3× change of sample, and the campaign closes by
  re-scoring the bet down rather than softening the prose. The floors that were
  re-pinned were pointed at the phenomenon *existing* (conflict fires, cascades
  occur) and deliberately assert no ceiling on the shape — pinning the shape
  would freeze the falsification against the very slice designed to break it.

## What to do differently

- **The implementation plan contradicted the approved spec twice, in the same
  direction.** Both times the plan softened predation into something gentler
  than the spec's §4.3: first by making a raid pure plunder (seize population,
  never take the coveted cell), then by scanning every reachable cell for a
  displaced people rather than the nearest admissible ring. The spec governed
  both times, and both deviations were caught only at task review, after
  measurement. The failure mode has a shape worth naming: a plan written from a
  spec's *summary* drifts toward the reviewer's intuition about what the
  mechanism "should" do, and intuition softens conflict. **At G4, diff the
  plan's mechanism paragraphs against the spec's clause by clause** — not for
  agreement in spirit, for agreement in outcome.

- **Four non-binding assertions in one campaign.** Three were found by
  mutation-testing during task review; the fourth was found by the whole-branch
  review, which deleted a viability guard outright and watched seed 42's entire
  census come back **byte-identical**. That guard's *comment* was wrong — it
  claimed to prevent a condition that is closed upstream — which is why it went
  unarmed for the whole campaign, and it is the reusable lesson: **a guard
  justified by a stale comment is a guard nobody mutation-tested.** Mutation
  verification is now clearly worth its cost at every review gate, not only when
  a test looks suspicious.

- **The branch absorbed main once, at close, at 161 commits.** The stated
  cadence is absorption at every plan-stage boundary. This campaign had a
  legitimate partial excuse — the measurement exception (never absorb between a
  preregistered baseline and its readout) covers T3 — but not for the stretch
  before it, and the merge was large enough that its collateral damage went
  unnoticed for a while (see below). The excuse covers one stage boundary, not
  four.

- **A merge silently clobbered this campaign's decision ledger.** Another
  campaign committed `.superpowers/sdd/decision-ledger.md` — a path this repo
  git-**ignores**. Because the file is tracked *there* and ignored *here*,
  absorbing main overwrote the untracked local ledger with theirs, with **no
  conflict raised and nothing red**. Twenty decisions were recovered from the
  session record and the path re-untracked (`git rm --cached`). The rule this
  needs is flat and mechanical: **scratch that is ignored by policy must never
  be committed** — one campaign doing it once clobbers every parallel campaign's
  ledger on absorption, silently. This is the cheapest possible class of data
  loss to prevent and among the most expensive to detect.

## Tooling notes

- **Running the heavy tier rewrites a committed artifact with nanosecond
  timings.** `make gate-full` regenerates
  `book/src/laboratory/generated/the-sounding/{rows.csv,summary.md}`, whose rows
  carry live wall-clock measurements, so every heavy-tier session produces
  spurious drift in `git status` that has nothing to do with the work. Check out
  those two paths before reading the diff; a future campaign should consider
  whether a benchmark page belongs in the drift-checked set at all.

- **A measurement task can be a real deliverable.** The coastal-inversion
  investigation changed no simulation source and produced the campaign's second
  and third findings: the inversion is a flagship **re-selection** artifact
  rather than anyone relocating, and — incidentally — an elevation datum that
  had made one people's authored stronghold unoccupiable on most worlds. Budget
  for the investigation when a preregistered hypothesis inverts. The cheap move
  (flip the assertion, re-pin, move on) would have frozen the bug for another
  year, exactly as the drift-check lesson predicts.
