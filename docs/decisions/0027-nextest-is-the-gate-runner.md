# 0027. cargo-nextest is the gate's test runner

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of keeping the local commit gate fast enough to run on every
commit, facing a workspace test suite that had grown to ~43.5 min under
`cargo test` on an M1 Max, we decided that **`make gate` runs the tests via
`cargo nextest run` (plus `cargo test --doc` for doctests)** rather than
`cargo test`.

**Context.** `cargo test` runs test *binaries* sequentially (one at a time),
so a suite of ~45 binaries pays the sum of their wall times even on an idle
many-core machine. nextest runs every test as a process scheduled across all
cores, filling the idle gaps. Measured on an M1 Max under load, the fast tier
(heavy batteries `#[ignore]`d — see the fast-gate-tiers spec) went **379s →
234s**; combined with the `#[ignore]` tiering it brings the commit gate under
4 min. Neither lever alone suffices: gating-only leaves cargo's sequential
6.3 min; nextest-only is bounded by the ~450s calibration-census tail.

**This does not violate [0004](0004-no-new-dependencies.md).** nextest is a
*developer tool*, installed with `cargo install cargo-nextest` or `brew
install cargo-nextest` — it is not a crate the workspace links against and it
does not appear in any `Cargo.toml`. It joins `scc`, `shellcheck`, `yq`, and
the out-of-workspace `tools/type-audit/` runner as tooling that is distinct
from the workspace dependency allowlist. The shipped artifacts, their
determinism, and the byte-for-byte contracts are unchanged.

**Consequence.**
- `make gate` / `make gate-fast` / `make gate-full` require `cargo-nextest`;
  each fails with an install hint if it is missing (`nextest-check`).
- **nextest does not run doctests** — the gate targets run `cargo test --doc`
  alongside it, so doctest coverage is retained. This split is load-bearing:
  dropping the `--doc` step silently removes doctest coverage.
- The `#[ignore]` heavy tier still governs *which* tests run; nextest respects
  `#[ignore]` (skipped by default). The two mechanisms are complementary, not
  redundant.
- **`#[ignore]` is overloaded in this tree** — it marks both cost-deferred
  heavy batteries (greppable via the `heavy:` token) AND genuinely-deferred
  tests (WIP, flaky, superseded, or a documented physics limitation such as
  the single-craton hypsometry). So `gate-full` must NOT use `--run-ignored
  all`, which would run the deferred-red ones and never be green. Instead
  `scripts/gate-full-heavy.sh` discovers the `heavy:` roster from source and
  runs exactly those via nextest `--run-ignored only -E 'test(/name$/)'`, so
  gate-full stays a meaningful green/red signal.
- Determinism is unaffected: nextest's process-per-test parallelism runs each
  test in isolation; the heavy tier passes byte-clean.

**See also.** The fast-gate-tiers spec
(`docs/superpowers/specs/2026-07-13-fast-gate-tiers-design.md`);
`CLAUDE.md` "Commands".
