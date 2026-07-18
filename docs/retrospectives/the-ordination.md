# Retrospective — The Ordination (ECS Campaign 6: systems & schedule)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The name-gloss finding is the campaign's measure-don't-narrate win, made
  concrete.** The single-writer-per-functional-predicate contract (metaplan
  §7) could have been written, spot-checked by eye against the eight genesis
  declarations, and declared satisfied — an assumed-green check, which is a
  check with no evidence behind it. Instead it was built to run for real at
  load, against the actual `CapabilitySchema`, and it found something: a
  functional predicate (`name-gloss`, 72 facts in a seed-42 world) declared
  as a write by both `settlement` and `religion`. The check that was *run*
  found what a check that was *assumed* would have hidden — the campaign's
  own decision ledger (#86) had already named this outcome as the most
  valuable one available before the check was even built, and it happened.
- **The anti-vacuity keystone caught its own vacuity risk by construction.**
  SCHEDULE ≡ HAND-ORDER is not literal sequence equality (the label
  tie-break provably disagrees with the hand-order on the independent
  stages) — it is "the hand-order is a valid topological linearization of
  the declared DAG." A topological-consistency check like that is trivially
  satisfiable by *under*-declaring: a system with a missing `reads` entry
  just looks independent, and the positive-half test would pass regardless
  of whether the declaration was honest. The plan named this risk at G4
  (decision #88) before task 3 wrote a single declaration, and task 3
  shipped the negative alongside the positive from the first commit: swap
  `terrain` and `settlement` in the hand-order (a genuine dependency —
  settlement reads terrain's elevation) and `is_valid_order` must reject it.
  This is the same construction-not-recognition lesson the PROC-11 line of
  work established for compile-fail tests, applied here to a graph
  property instead of a type error, and it was designed in rather than
  discovered by a reviewer.
- **Five clean, independently-reviewable stages, each byte-identical.** The
  spec's six crux items (#82–#87, plus #89's mid-campaign carve-out)
  settled the scope question — genesis-as-tick-0, shadow-not-cutover,
  declaration-not-`Domain`-member, measure-the-check, byte-identity
  posture — before task 1 started, and execution never re-litigated any of
  them. Every task landed with the artifact drift-check empty; the
  byte-identity claim was verified at every task boundary, not asserted
  once at the end.

## What the campaign taught mid-execution

- **A settled ruling can still be pushed on productively.** #89 was already
  a resolved decision — Nathan's ruling (kernel-core exemption,
  byte-identical) — when it went through two more ideonomy passes at his
  explicit request to "shake everything loose." Neither pass overturned the
  ruling; both deepened *why* it is safe rather than merely expedient. The
  cross-domain reinstantiation pass (SQL UNIQUE-column / per-row) surfaced
  that the schedule's per-predicate contract is a coarse static proxy for a
  per-(subject, predicate) invariant the ledger already enforces,
  unconditionally, at commit time — which is the actual reason `name-gloss`
  was never a real conflict, not just a convenient story. The periodic-grid
  pass surfaced the contract's *other* degeneracy, a predicate read by
  nobody's write (the orphan-read check, never built), and a connectivity
  framing (kernel-core predicates are hubs; the single-writer contract is
  shaped for domain-owned leaves) that explains why hubs need the exemption
  by nature rather than by carve-out. Ideonomy run on a question already
  answered is not redundant work if the question is load-bearing enough —
  it found two new followups (6, 7) a single confirming pass would not
  have.
- **Two dispatches parked on a background job mid-task.** T2 (the
  single-writer check) and T3 (the worldgen declarations + keystone, the
  campaign's substantive task) both parked on a backgrounded test run
  instead of waiting on it in the foreground, per the dispatch preamble's
  explicit instruction not to. Both were resumed via `SendMessage`
  un-parking rather than a fresh dispatch, and both completed clean with no
  work lost — but it cost a round trip each time, and it is the second
  campaign in this program's short history to note the same failure mode
  (`progress.md`'s note after T2: "watch for parking on later dispatches"
  did not, on its own, prevent it recurring at T3). The dispatch preamble
  already says "run in the FOREGROUND... DO NOT background or park" in
  as many words; the gap is between the instruction being written and a
  subagent actually following it under a long-running command, not between
  the instruction existing and not existing.

## Followups (register, promoted verbatim)

1. **The running liveness tick (ticks 1..N)** — the future arc: GOAP
   (PSY-6), belief (UNI-16), and social (SOC-9) systems generating new
   sim-time facts, run by the BSP mechanism this campaign made concrete.
   Needs those systems built first and a consumer that renders sim-time
   evolution. World-changing — its own epoch/census call.
2. **Genesis executes through the derived schedule** — the cutover held in
   shadow (#84). Needs a uniform runnable `step()` so genesis stages run in
   the derived order rather than the hand-written `build_to` sequence;
   rides the running-tick arc.
3. **Same-tick immediate effects** (metaplan §3.6's opt-in) — needs the
   topological schedule (shipped here) to order the same-tick chain; a
   running-tick concern.
4. **The capability schema as a queryable reflection surface** (UNI-21) —
   a `systems` CLI/book reference page (like `concepts`/`streams`) dumping
   the derived schedule and per-system reads/writes. Additive, deferred.
5. **Single-writer reconciliation** — RESOLVED within this campaign: #86
   found `name-gloss`, #89 resolved it (kernel-core exemption,
   byte-identical). No longer open.
6. **The orphan-read check** — the single-writer check's symmetric twin,
   surfaced by the ideonomy periodic-grid pass on #89. A predicate read by
   a system no declared system writes is a dangling read (it always reads
   empty); the metaplan §7 name-drops this check, and this campaign built
   only the write-side half. Cheap over the same declarations, but needs
   the kernel-core and externally-sourced (pin) exemptions to avoid false
   positives. Additive, deferred.
7. **Per-(subject, tick) conflict check for kernel-core functional
   predicates** — surfaced by the ideonomy Unix-sticky-bit pass on #89. The
   kernel-core exemption is safe at genesis because subjects are disjoint
   by construction; a running-tick system re-naming the same subject
   another system also writes within one tick could collide in a way the
   static per-predicate exemption cannot catch (the ledger's commit-time
   Contradiction check still would, but only after the tick runs). Rides
   the running-tick arc (followup 1).

## Confidence Gradient

No open-questions bet was re-scored. Grepped `book/src/open-questions.md`
for schedule/systems/ECS/capability-schema terms; the only hit is an
unrelated use of "systems" in prose about historiography, not this
campaign's mechanism. Same shape as the c4 and c5 closes: this campaign is
substrate *beneath* the generation/taste bets — it gives the world's own
build pipeline a checkable order and a proven tick mechanism, in shadow,
with zero worlds changed — not a resolution of a taste-gated bet. A
substrate milestone tracked by the UNI-22 registry row is not, by itself, a
movement in a generation/taste bet. Campaign 7 (spatial partition) is the
program's last substrate campaign; after it, the taste-gated liveness arc
(followup 1) is what would first move a Confidence Gradient bet.
