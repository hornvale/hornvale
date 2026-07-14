# 0045. One canonical census; frozen studies are evidence

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of the census family having grown two separate all-metrics
studies with overlapping purpose (`census-lands-drift` as CI's determinism
guard, `branches-family` as the goblinoid-phylogeny battery) and no shared,
typed schema across any published study, we decided that **the live census
record is ONE all-metrics study (`the-census`, 1,000 seeds, default pins)
plus the solo-roster null control (`census-of-the-meeting`)** — `the-census`
is `census-lands-drift` promoted and renamed, its seed count raised
500→1,000 — and that studies superseded by it are frozen as evidence,
never regenerated, rather than deleted.

**Context.** `branches-family` froze at the consolidation: its run
retired (its data is a strict column projection of the canonical run —
verified byte-identical at migration), its committed artifacts kept as
preregistered evidence, its calibrations re-pointed at the canonical
fixture. Frozen studies are never regenerated — regenerating under moved
physics would falsify the record — but each records its producing commit,
so any of them is reproducible via checkout (`git checkout <commit> && lab
run`). Every published study now carries a co-generated `schema.json`
(spec §2: typed, documented columns, quantization convention, row count,
FNV-1a64 content hash). The analysis harness lives OUTSIDE the workspace
(`tools/census/`, duckdb + python3), like the type audit (decisions
0027/0028's pattern).

**Consequence.** Considered and refused: append-only epoch directories in
the working tree (git IS the append-only store); a committed long-format
CSV (derived, not stored — the long view is built at query time from the
wide `rows.csv` files); Rust-native query surfaces (std-only SQL
reinvention, when DuckDB already exists as a dev-only tool outside the
workspace boundary). The cost accepted: a frozen study's numbers can never
be refreshed in place — a physics change that would move them is visible
only as a divergence from `the-census`, not as an updated frozen row.

**See also.** Spec: `docs/superpowers/specs/2026-07-13-census-as-data-design.md`;
decision 0016 (studies preregister their hypotheses — `branches-family`'s
frozen role is still that evidence); decisions 0027/0028 (the non-workspace
dev-tool pattern `tools/census/` follows); decision 0029 (CI checks
500-seed censuses — `the-census`'s 1,000-seed live tier supersedes the
smaller drift study that decision described).
