# 0042. GitHub Actions CI is manual-only

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of a one-person project whose primary dev machine (an M1 Max,
10 cores) is markedly faster than GitHub's free 2-core runners, facing a
per-push CI that took ~35 min–2 h (the workspace suite alone is ~17 min on 2
cores; the full-census determinism check ran for ~98 min before we cancelled
it) while the same gate runs in ~4 min locally, we decided that **the GitHub
Actions CI workflow (`ci.yml`) does not run automatically** — its trigger is
`workflow_dispatch` only. The book publish (`book.yml`) still runs on push.

This deliberately inverts the usual expectation that CI runners match or beat
dev machines. Here they don't, and there are no other contributors whose
pushes need gating, so automatic CI added latency without value.

**The gates that replace it.**
- **Local commit gate** (`make gate` — nextest + doctests, ~4 min): every
  commit passes it before it reaches `main`. A push to `main` is trusted to
  have passed it.
- **AWS Linux runs** for the one axis local macOS *cannot* verify —
  cross-platform byte-identity. `scripts/aws-gate/regen-git.sh` (full regen +
  return over SSH) and `gate-remote.sh` run the gate on a fast Linux spot box,
  on demand. libm (decision 0041) makes the compute cross-platform
  deterministic, but this only stays honest if the Linux check is actually run
  for census/naming/terrain-touching changes — that discipline is the cost of
  this decision.

**Consequence.** Cross-platform regressions are caught by the AWS runs (when
run), not per-push. `ci.yml` remains in the tree, runnable by hand
(Actions → Run workflow) as an escape hatch, and still carries the fast census
spot-check so a manual run is quick. Local macOS green does not, by itself,
prove Linux correctness.

**See also.** decision 0041 (libm — the cross-platform compute fix);
`scripts/aws-gate/` (the AWS toolkit); `.github/workflows/book.yml` (still
auto-publishes on push).
