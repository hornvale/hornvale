# The Pyx — Design

**Campaign:** The Pyx · **Date:** 2026-07-30 · **Status:** spec, awaiting G3
· **Backlog row:** TOOL-24

Named for the Trial of the Pyx, the Royal Mint's assay of a random sample of
struck coins against a reference standard — held annually since 1282,
because a mint that never checks its own output against a standard will not
notice the day it stops matching.

## 1. The problem

Hornvale's census goldens are authored on exactly one machine. Decision
[0063](../../decisions/0063-census-regen-is-local-again.md) ratified that
("this machine is the single canonical platform"), and
[0079](../../decisions/0079-census-goldens-are-authored-on-one-enforced-host.md)
mechanized it — a declared hostname, enforced at every write path, failing
closed.

The ruling rests on one measurement: on 2026-07-19, lefford and an AWS box
disagreed by one unit on ~0.1% of census values — `divergence-magnitude`-class
discrete counts, decided by a comparison in the *compute* path where
quantize-at-emit ([0033](../../decisions/0033-serialized-floats-are-quantized-for-cross-platform-determinism.md))
cannot reach them.

**The mechanism of that divergence was never diagnosed.** 0063 records the
observation and the ruling, and correctly identifies the *amplifier* (a
threshold comparison turns a last-ULP float difference into a visible integer
difference). It does not identify the *source* of the ULP difference, and
nothing since has looked for it.

Two consequences follow, and the second is the campaign.

**(a) The measurement's conditions no longer hold.** See §2.

**(b) The project has never measured reproducibility at all.** ISO 5725 splits
*repeatability* — same lab, same operator, short interval — from
*reproducibility* — a different lab reaching the same value. Every determinism
guarantee Hornvale enforces is a repeatability guarantee: the drift check
compares a fresh run on lefford against a golden authored on lefford. The
question "can this corpus be reproduced anywhere, by anything, ever again?"
has not been asked, and today the honest answer is that nobody knows. Lefford
is Le Grand K: a physical artifact serving as a standard, never assayed
against a copy.

This is a live single-point-of-failure. If lefford's disk died tomorrow, the
committed goldens would remain — and there would be no evidence any successor
machine could reproduce them, nor any way to tell a successor's correct output
from a wrong one.

## 2. What is already pinned, and what the audit must not confound

Every entry below was verified by command, not inferred. The dates matter
because 0063's measurement is dated 2026-07-19.

```
  pinned?  what                          since        evidence
  -------  ----------------------------  -----------  -------------------------
  YES      rustc 1.96.1                  2026-07-07   rust-toolchain.toml
                                                      (7ec62eb4)
  YES      libm crate 0.2.16             tracked      Cargo.lock (committed)
  YES      f64 transcendentals routed    2026-07-12   clippy.toml
             through kernel::math                     disallowed-methods
                                                      (80d78bc2), -D warnings
  N/A      thread-order dependence       n/a          runner.rs:210-258 --
             (FALSIFIED, see below)                   verified absent
  YES      codegen baseline              2026-07-27   .cargo/config.toml
             target-cpu=x86-64-v2                     (3a7092c3)  <-- AFTER
  no       glibc version                 --           dynamic, per-host
  no       kernel version                --           per-host
  no       build path / env in binary    --           no --remap-path-prefix
```

**Thread count is not a divergence source.** This was the leading hypothesis
and reading the code falsified it. `run_pin_set`
(`windows/lab/src/runner.rs:210-258`) computes `threads` from
`available_parallelism()` and chunks seeds contiguously — so lefford's 40 cores
and velaryon's 24 do produce different partitions — but each worker writes one
result slot per seed offset, the main thread reads slots back in offset order,
and each row is computed independently from its own seed. Core count changes
who computes a row, never the row. Recorded because it is the obvious
hypothesis and it is wrong.

**The codegen baseline is the one pin that postdates the measurement, and it
moved the hot floating-point path.** `.cargo/config.toml` landed eight days
after 0063. Its own comment states that on the default `x86-64` baseline LLVM
cannot emit `roundsd`, so **every `f64::floor()` became a library call** — a
bare `floor` symbol that `perf` attributed 4.62% of census self-time — in
`Fbm::sample`, the innermost primitive of all world generation, floored twice
per sample. Raising the baseline turned those calls into instructions.

So between 0063's measurement and today, the innermost FP primitive stopped
calling into a per-host shared library and started executing a fixed
instruction. **That is the strongest surviving hypothesis for a
never-diagnosed divergence, and it is a hypothesis with a testable locus
rather than a diagnosis.** The honest caveat, stated here so the spec does not
overclaim: IEEE-754 `floor` is exactly representable and any conforming libm
returns the same value, so a glibc `floor` *should not* diverge. Either the
mechanism is elsewhere, or one of these boxes was doing something
non-conforming. The experiment does not need to resolve this to be worth
running — it needs to establish whether divergence reproduces *at all* under
today's pins.

## 3. Decisions

**D1. The audit measures the canonical host, not a candidate second host.**
Velaryon (the amd64 node in the goldentooth cluster) motivated this work and
is deferred entirely to a follow-on campaign. 0063 already blesses the
endpoint — "another development machine … must NOT commit goldens unless it is
**proven byte-identical**" — so the blocking artifact is the proof, and the
proof is cheaper and more valuable than the hardware.

**D2. Compare binaries before comparing outputs.** If two builds of the same
source produce the same binary, identical output is implied and no world need
be generated to establish it. If they differ, the binary diff *localizes* the
mechanism, which is precisely what 0063 lacked. Output comparison is the
fallback measurement, not the primary one.

**D3. Velaryon is out of scope, and the second arm is the Mac.** This changes
what was presented at approach selection, where L1 was "build on lefford and
velaryon." Reason: any velaryon arm requires the container/Job/registry work
that D1 defers, whereas the Mac is a *different platform* (aarch64/Darwin vs
x86_64/Linux) available with zero infrastructure — and 0033 already claims
cross-platform byte-identity, so the Mac is a legitimate and unusually
demanding arm. A Mac-vs-lefford probe disagreement would be a finding about a
ratified constitutional guarantee.

**D4. The probe is a new study, and its hypothesis is frozen in this spec.**
Studies are data (0011), and a study freezes its hypothesis and success
criteria before the code that could move them (0016). **Where that freeze
lives matters, and it is not the study file:** a study JSON carries only
`name`, `description`, `seeds`, `pin_sets`, `metrics` — there is no
`hypothesis` field. And `windows/lab/tests/preregistration_guard.rs`, despite
its name, does not check hypotheses at all; it is PROC-6's *result-quieting*
guard, a default-deny scan asserting that any `#[ignore]` in a lab calibration
test carries a sanctioned reason (one naming a cost, or citing a decision
number). So the preregistration for this campaign is **§5 of this document**,
committed before any layer runs, in the same shape calibration tests already
use when they cite "spec §9.2". The guard binds only if the campaign later
adds an `#[ignore]`d calibration test — in which case its reason must name a
cost or cite a decision.

**D5. The probe is deliberately named outside the census-study convention.**
0079's Rust guard identifies census studies by name pattern (`the-census`,
`census-of-*`) and refuses to publish them off the canonical host; 0079 itself
records the residual gap that a census-class study named outside the
convention slips past. `the-pyx-probe` uses that gap **on purpose and
narrowly**: the probe must run on every machine, and it is safe to do so
because it publishes nothing into `book/src/laboratory/generated/` — it writes
to a scratch path and its output is compared, never committed. This is stated
explicitly so a future reader does not mistake it for an end-run around 0079.
If the probe is ever wired to publish, it must take the guard.

## 4. The design

Three layers, cheapest first. Each can kill the next; each is independently
informative.

### L0 — Repeatability in time (lefford vs its own past self)

Re-run the census on lefford at the SHA whose committed golden it authored,
and diff the result against that committed golden.

- Instrument: `scripts/census-run.sh` with `HV_CENSUS_REF=<SHA>` (verified at
  `scripts/census-run.sh:79-93`: the ref feeds `checkout --force` then
  `reset --hard`, so it must be a SHA — a branch name can land on a stale
  local branch on the canonical box). Pass a resolved `HV_CENSUS_WORKTREE` and
  verify its HEAD; the regeneration worktree on lefford is shared.
- **Target SHA: `9855048da84ca7c3b050b88f8b9160a02e75db92`** ("build(lab): pay
  off The Wearing's deferred census regen (lefford, 0063/0079)", 2026-07-29) —
  the most recent commit to author `the-census/rows.csv`. Verified
  artifacts-only: `git show --stat` lists 40 files, all under
  `book/src/laboratory/generated/` (rows, SVGs, `summary.md`, and the
  generated `schema.json`) plus `docs/timings.md`. **No `.rs`, no `.toml`, no
  `Cargo.lock`, no scripts** — so re-running at this commit reproduces the
  exact tree that authored its goldens.
- **Exclude `docs/timings.md` from the comparison.** `census-run.sh` appends a
  ledger row on every run, so that file is *expected* to differ and is not
  part of the measurement.
- **Non-authoring.** The comparison is a `git diff`; nothing is committed.
- Cost: one census. The five most recent lefford census rows in
  `docs/timings.md` read 828-1284 s wall (~14-21 min).

This is the control the project has never run. It is also the only layer whose
failure would be more important than the campaign.

### L1 — Build reproducibility (lefford vs lefford, clean rebuild)

Build the `hornvale` binary twice on lefford from the same SHA, in two
different directories, from a cleaned `target/`, and compare `sha256`.

- If equal: the binary is a pure function of the source on this host, and the
  L0/L2 results inherit that.
- If unequal: capture *where*. Rust embeds absolute paths absent
  `--remap-path-prefix`, so a benign difference is expected and its
  confirmation is the finding — it tells us binary hashing cannot be the
  cross-host oracle, and D2's cheap path closes.

### L2 — Cross-platform probe (lefford vs the Mac)

A small study, run on both machines, comparing output values directly.

- **Contents:** seed 681 (0063's named tripwire) plus a seed spread, reading
  the `divergence-magnitude-*` family — `divergence-magnitude-hobgoblin` is
  still live at `windows/lab/src/metrics.rs:2829` — and the other
  threshold-decided discrete-count metrics.
- **Why these:** they are the metrics 0063 measured diverging. A metric whose
  value is a quantized float cannot show a cross-host difference (0033 absorbs
  it); only a count decided upstream of the emit boundary can. The probe
  targets the amplifier deliberately.
- **Scale:** seconds, not minutes. Small enough to run anywhere, which is what
  makes the follow-on campaign's 17-node matrix cheap.
- Output goes to a scratch path; the two runs are diffed.

## 5. Preregistered predictions

Frozen before any layer runs (0016). Recorded so that a surprise is a finding
and not a retune.

| Layer | Prediction | If it holds | If it fails |
|---|---|---|---|
| L0 | **Green** — lefford reproduces its own golden exactly | Repeatability confirmed; the corpus is stable on its authoring host | **Escalate above this campaign.** The golden corpus is not reproducible even by the machine that made it; every drift check is comparing against an unreproducible artifact |
| L1 | **Red, benignly** — binaries differ on embedded paths | Binary hashing is not a viable cross-host oracle; D2's cheap path closes and L2 becomes the primary measurement | If green, a much stronger tool is available: binary identity as a standing cross-host conformance check |
| L2 | **Green** — Mac and lefford agree on every probe value | 0063's divergence does not reproduce under today's pins; 0063's single-platform ruling becomes a candidate for supersession and velaryon recruitment is de-risked | Cross-platform byte-identity (0033) is violated **today**, on a ratified constitutional guarantee — a first-order finding that redirects the campaign entirely |

**What the null proves.** An all-green result does *not* prove velaryon can
author goldens; it proves the divergence 0063 measured does not reproduce
between two current, pinned hosts spanning two architectures and two operating
systems. That is the strongest available evidence short of running velaryon
itself, and it is what licenses the follow-on campaign — not a substitute for
it.

**What would falsify the §2 hypothesis.** If L2 is green, the codegen-baseline
story is *consistent* but unproven; proving it requires rebuilding without the
`x86-64-v2` flag and showing divergence returns. That is a named optional
extension (§7), not a requirement.

## 6. Non-goals

- **Recruiting velaryon.** No container image, no Job manifest, no registry
  push, no PVC. That is the follow-on campaign, and it starts only if this one
  comes back green.
- **Changing 0079's guard.** Migrating canonicity from a hostname to a
  toolchain fingerprint is the obvious lift-back from metrology (a hostname
  check cannot catch lefford drifting from itself; a fingerprint can). It is
  recorded as a followup, not built here — it needs L0/L1 results to be
  designed against.
- **Superseding 0063.** This campaign produces the *evidence*. Whether to
  supersede is Nathan's call at G6, on the numbers.
- **Making the probe a gate.** A standing cross-host conformance check is the
  natural product, but it needs a second host to check against, which this
  campaign does not have.

## 7. Verification

- L0: `git diff --exit-code` on `book/src/laboratory/generated/*/rows.csv`
  after the re-run. Zero diff is the pass.
- L1: two `sha256sum` values, recorded verbatim in the chronicle whether they
  match or not. If they differ, `cmp -l | wc -l` and a `strings` diff to
  localize.
- L2: byte comparison of the two probe outputs, plus an explicit readout of
  seed 681's `divergence-magnitude-hobgoblin` value on each host against the
  5-vs-6 disagreement 0063 recorded.
- Every run's evidence is written to a file, not piped to `tail` — an
  expensive run must never need repeating to see a line that scrolled past.
- Optional extension, only if L2 is green and the mechanism is wanted:
  rebuild without `target-cpu=x86-64-v2` and re-run L2. Divergence returning
  would confirm §2's hypothesis outright.

## 8. Definition of Done

- [ ] L0, L1, L2 run; all evidence files retained.
- [ ] Predictions in §5 compared against results, with any miss labelled as a
      miss in both the chronicle and the retrospective.
- [ ] A decision record: either refining 0063/0079 with the new evidence, or
      recording that the divergence still reproduces and single-platform
      stands.
- [ ] Chronicle entry (`book/src/chronicle/the-pyx.md`) + SUMMARY.md.
- [ ] Book freshness sweep; re-score the Confidence Gradient
      (`book/src/open-questions.md`) if this moves a bet on determinism.
- [ ] Retrospective (`docs/retrospectives/the-pyx.md`), including the
      followup table (the durable record — the scratch ledger dies with the
      worktree).
- [ ] TOOL-24 row in `WORKFLOW_IMPROVEMENTS_PLAN.md`; the
      hostname-to-fingerprint followup captured there.
- [ ] `make gate` green on the Mac before merge.
