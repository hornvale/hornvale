# 0001. Determinism is constitutional

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of building an infinite, multiscalar world that must be
saveable, testable, and usable for controlled social-science experiments,
facing the choice between convenience and reproducibility, we decided that
**the same seed plus the same pins produces byte-identical output forever, at
every fidelity**, accepting that no wall-clock time or other ambient
nondeterminism may ever enter the kernel or domain code.

**Context.** Determinism is simultaneously the infinite-world mechanism, the
save format (a seed plus a ledger), the test strategy, and the
computational-social-science story ("same world, but the moon is tidally
locked"). It is not an optimization; it is the substrate everything else
stands on.

**Consequence.** No `std::time` in kernel or domain code; time is
`WorldTime { day: f64 }`. Most bugs in this area are catastrophic (they
silently corrupt every world). CI enforces it via artifact drift checks. See
[0005](0005-deterministic-collections-and-sorts.md),
[0006](0006-seed-labels-are-permanent-contracts.md).

**See also.** Constitution Principle 3 (long-term-plan spec §2); `CLAUDE.md`
"Determinism" section.
