# 0087. A benchmark's timings are a record, not a golden

**Status:** Accepted (2026-07-29) · **Decider:** Nathan · **Relates to:** [0086](0086-the-heavy-tier-runs-on-the-canonical-box.md)

Found while measuring whether the heavy tier's committed artifacts diverge
across hosts (The Siding, assumption A2). Regenerating the two sweeps on the
canonical box at the shared ref `65581f18` produced:

- `the-history` — **zero diff**.
- `the-sounding` — `rows.csv` 62 lines changed, `summary.md` 16.

Inspecting *which* columns moved settled it. Every `peak_bytes,events` pair
appeared exactly twice in the diff, once as `-` and once as `+`: the
deterministic columns were **byte-identical**. Only `bake_ns`,
`read_ns_per_op`, and `replay_ns` moved, and `summary.md`'s entire diff was
statistics fitted from them — the scaling exponents (2.14 → 1.95) and the
scan-vs-index ratio (13× → 14×).

So the artifact was never diverging across *hosts*. It diverges across
**runs**, because it records wall-clock time. The generator has always said so
("timings are wall-clock and machine-dependent"), but the files still sat
inside the strictly drift-checked `book/src/laboratory/` tree.

**Why this never fired.** `make rebaseline` does not run the heavy tier, and
the heavy tier is not followed by a drift check. `the-sounding` is written only
by a heavy-tier *test*, so the one command that rewrites it and the one command
that would notice have never run together. The check was latent, not absent —
the same shape as artifacts the regeneration script cannot refresh, one layer
deeper.

## The ruling

**A benchmark's absolute timings are an informational record. They are excluded
from the drift check, and the artifact says so about itself.**

`book/src/laboratory/generated/the-sounding/rows.csv` and `summary.md` join the
existing pathspec exclusions in `.github/workflows/ci.yml`, each of which
already carries its own written rationale. The generator's preamble now states
the exclusion, so a reader — or an agent regenerating artifacts — is told the
file is not a golden by the file itself, rather than having to infer it.

**`sample-biographies.txt` is NOT excluded.** The generator states the
biographies are byte-deterministic, so it stays under the strict check. It is
the file that would catch a real regression in what The Sounding computes, and
keeping it checked is what stops this decision from being a blanket amnesty for
the whole study.

**Read the exponents, not the nanoseconds.** The benchmark's result is the
scan-vs-index coupling — quadratic versus near-linear — and that conclusion is
robust to which box ran it. The absolute nanoseconds are a record of one run on
one box and were never the finding.

## Consequences

- **Coverage is lost, and it is not nothing.** `rows.csv`'s deterministic
  `peak_bytes`/`events` columns and `summary.md`'s workload-census counts are
  deterministic but now unchecked, because they share a file with the timings.
  Splitting the timing columns into their own file would regain both; recorded
  as a follow-up rather than done here, to keep it out of The Siding's scope.
- A verification step of the form `git diff --exit-code
  book/src/laboratory/generated` **cannot pass** after a heavy-tier run. The
  Siding's own plan specified exactly that check and would have failed on its
  first use; it now compares the deterministic columns and tolerates timing
  drift explicitly.
- This does not weaken determinism anywhere. Nothing in the sim core changed:
  the excluded values are measurements *of* the implementation, not outputs of
  it. Every genuinely seeded artifact stays strictly checked.
- The committed timings were faster than the canonical box's, consistent with
  having been authored on the Mac. Under 0086 the tier now runs on the
  canonical box, so future records come from one machine — which makes the
  numbers comparable run-to-run even though they are not byte-stable.
