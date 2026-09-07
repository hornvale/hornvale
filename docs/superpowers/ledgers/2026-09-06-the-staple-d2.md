# The Staple D2 — decision ledger

Campaign: **The Staple D2** — voluntary local exchange beside tribute.
Branch: `campaign/the-staple-d2`. Decision block: **0886–0895**.
Spec: `docs/superpowers/specs/2026-09-06-the-staple-d2-design.md`.
Status: G3 approved; implementation active (Task 1 complete).

## Entries

#1 [G1] — **What is D2's first mechanism?** · **Decision: two typed,
non-convertible subsistence commodities exchanged bilaterally over local
reachability.** One aggregate stock would model redistribution or aid, not
reciprocal exchange. Ideonomy: 1 pass, no overturn; the pass exposed the
one-stock dead mechanism. Capture: spec §§3–4.

#2 [Q] — **What is the stock family?** · **Decision: population, two typed
subsistence stocks, and existing non-edible `stores`; units are fungible only
within type.** Cross-type conversion is a later campaign. Ideonomy: 1 pass,
no overturn. Capture: spec §3.

#3 [Q] — **How are exchange promises represented?** · **Decision: D2
exercises only immediate delivery; typed obligations remain the extensible
future family.** Failed and partial attempts have explicit outcomes but do
not create debt. Ideonomy: 1 pass, no overturn. Capture: spec §§3–4.

#4 [Q] — **How is scarce stock allocated?** · **Decision: reserve local
projected need, then simultaneous deterministic pro-rata clearing. Funded
acyclic chains may continue; unfunded cycles cannot settle.** Ideonomy: 1
pass, no overturn. Capture: spec §3.

#5 [Q] — **What is the probe?** · **Decision: paired control/treatment over
200 seeds, with separate world and attempt denominators. Dead poles are zero
activation and more than half the treatment worlds breaching each existing
demographic bar.** Outcome rates are descriptive. Ideonomy: 1 pass, no
overturn. Capture: spec §4; decision 0826.

#6 [Q] — **What is committed versus derived?** · **Decision: exchange
outcomes are derived study traces and aggregate census candidates; only
irreducible stock inputs and pins become durable simulation state.** Ideonomy:
1 pass, no overturn. Capture: spec §§3–4.

#7 [G3] — **Which existing bars define instability?** · **Decision: use the
history gate's settlement-count sane band `40..=400`, collapse-share ceiling
`0.05`, and alive-at-now floor `50`; count only treatment-only breaches against
the paired control.** These are existing guardrails, not new population
targets. G3 approval received 2026-09-06. Ideonomy: 1 pass, no overturn.
Capture: spec §4; `windows/worldgen/tests/suite/history_tumult.rs`.

#8 [G4] — **Plan review before execution.** · **Decision: proceed with the
five-stage plan at `docs/superpowers/plans/2026-09-06-the-staple-d2.md`.**
Self-review found and corrected the Task 1 dependency inversion: the probe
fixture freezes the report contract before production exchange code; Task 4
is the integrated treatment. It also replaced the vague one-hop reference
with the verified `traversable_neighbors` helper and named the exact existing
bars. Ideonomy: 0 passes — this is a review gate, not a design choice.
Capture: `IMPLEMENTATION_PLAN.md`; the plan.

## Task 1 — probe contract frozen

The executable fixture now treats the 200 worlds and the attempt population as
different denominators. An empty outcome set reports `NoExchangeAttempts`;
failed attempts with no settlement produce a valid report with
`activation_worlds == 0`. Every reported status carries its own non-zero
attempt denominator, and only a bar passed by the paired control can count as
a treatment-only breach. No ideonomy pass was run: this implements the
G4-approved report contract and records measured behavior rather than making a
new design choice.

The fixed roster `1..=200` was built as 200 same-seed control/disabled-treatment
pairs through separate fixture boundaries. The explicit probe passed
`200 / 200` byte-identical ledger comparisons in `949.31s`; because production
exchange does not exist yet, it then reached the separately represented zero-
attempt result. Stock residual shape and finiteness checks execute before that
result, so the empty outcome cannot bypass the conservation surface.

### Task 1 review — fix round 1

The reviewer found two high-severity defects: the conservation check rejected
non-finite residuals but accepted finite nonzero residuals, and the disabled
treatment arm called the same builder as control rather than exercising an
explicit disabled-treatment boundary. Ruling: fix both before Task 1 can be
marked complete. Cost if wrong: the probe could certify stock creation or a
disconnected control switch while all assertions remained green.

Fix result: the reducer now rejects every non-finite or finite nonzero
per-resource residual before outcome reporting. Control and disabled treatment
own separate world-builder boundaries, and the observation seam serializes and
compares their ledger bytes. A focused test mutates only the disabled ledger
and proves the mismatch reaches `DisabledControlChanged`; the fixed 200-seed
test remains unchanged and was not rerun because its prior 949.31s evidence is
already recorded above. Ideonomy: 0 passes — this fix executes the review's
specified contract and introduces no design choice.

Task 1 is complete at `3bf26e0dc`. The scoped reviewer re-ran the focused
contract tests (9 passed, 0 failed, 1 ignored) and found no remaining
findings. The expensive 200-seed fixture is covered by the earlier recorded
949.31s run; it was deliberately not repeated during review. The probe is
now an accepted executable contract for the production state and clearing
tasks that follow.

### Task 2 review — fix round 1

The scoped reviewer found one high-severity defect: computing resource B as
`total - A` does not guarantee that `A + B` is bit-exactly `total` for every
valid floating-point total and authored share. The existing single-case test
was favorable and could not protect the exact-zero conservation contract that
Task 4 will consume. Ruling: repair the partition or weaken the contract only
with an explicit design decision; do not mark Task 2 complete while the
reviewer's counterexample remains possible. Cost if wrong: the integrated
probe could report stock creation from roundoff under the preregistered exact
conservation criterion.

Fix round 1 repaired the reviewer counterexample by multiplying the majority
component first and using Sterbenz-exact subtraction for the remainder; the
regression covers the original value, representative biomes, subnormal
boundaries, `f64::MAX`, and a two-million-value finite sweep. Re-review found
one medium edge: the debug contract admitted negative zero as non-negative,
but bit-exact reconstruction canonicalized it to positive zero. Ruling: make
the valid non-negative domain explicit by canonicalizing zero at the helper
boundary and add a signed-zero regression; do not weaken the bit-exact
conservation property. Cost if wrong: a future zero-valued production path
could trip its own exact conservation assertion despite carrying no stock.

Fix round 2 canonicalized signed zero at the partition boundary and added a
non-vacuous `-0.0` regression. The final scoped reviewer reports CLEAN: the
original counterexample, representative positive values, subnormal and large
finite values, and signed zero all satisfy the exact-zero conservation
property; no Task 2 requirement regressed. Task 2 is complete at
`8da7001c8`. Implementer evidence: focused 8-test suite passed, clippy passed,
and gate-commit passed 1300/1300 with no Task 1 probe or census rerun.

## Deferred minors and follow-ups

- Verify the specialization input and shortfall insertion point against the
  live tree before dispatching any brief; every identifier in the brief must
  be grepped first.
- Correct the bar-source citation during Task 5 documentation reconciliation:
  `history_tumult.rs` carries the settlement band `40..=400` and alive floor
  `50`, but the approved collapse ceiling `0.05` is defined and asserted in
  `history_sundering.rs`. Task 1 kept the approved value and recorded the live
  source instead of silently inventing a replacement identifier.
- Add the two typed resource names to the idea registry if implementation
  makes them stable concepts rather than probe-local names.
- Promote aggregate exchange metrics to the larger census only after paired
  results show they are durable query needs.
- Re-baseline the census after the D2 epoch and convert history-adjacent pins
  to invariants; do not treat the D2 study trace as a permanent ledger.

## G3 review questions

1. Is D2's two-commodity production split acceptable as the smallest genuine
   exchange mechanism? **Approved at G3.**
2. Does the climate-to-city ownership sentence belong in the metaplan now,
   or remain an open D2 design consequence until the probe is measured?
