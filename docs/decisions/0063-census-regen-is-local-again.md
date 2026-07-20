# 0063. Census regeneration is local again

**Status:** Accepted (2026-07-19) · **Decider:** Nathan · **Supersedes:** 0046

In the context of [The Local Census](../../book/src/chronicle/the-local-census.md)
having cut the all-metric census's per-world cost from ~285 to ~8 CPU-seconds
— byte-identically, by stopping the metric and genesis-naming paths from
re-sculpting terrain they already held — we decided that **the full census
regenerates LOCALLY at the pre-merge campaign close**, not on the AWS spot box.
`HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` runs `the-census` (1,000
seeds) and `census-of-the-meeting` (500×2) in ~7 minutes on the 40-core Linux
box; the committed census goldens (`book/src/laboratory/generated/*/rows.csv`)
are kept **current with main**, not left to lag.

**Why 0046 no longer holds.** Decision 0046 routed census regen through AWS for
two reasons, and this campaign dissolves both:

1. *Load.* 0046's premise was an M1 Max whose ~1–2 h census run starved
   sibling sessions. At ~7 minutes on a 40-core box the run no longer
   monopolizes the machine; the "shared-machine stability" 0046 bought with a
   remote round-trip is now bought with seven minutes once per campaign.
2. *Cross-platform coverage.* 0046 also valued the AWS box as the project's
   one Linux cross-platform reference (decision 0042). Adopting local goldens
   surfaced that **this machine is NOT byte-identical to AWS**: on ~0.1% of
   census values — discrete-count metrics like `divergence-magnitude`, where a
   count is decided by a comparison in the *compute* path, before the
   quantize-at-emit boundary (decision 0033) can round it — the two Linux
   boxes disagree by one unit (verified: seed 681 `divergence-hobgoblin` is 5
   here, 6 in the committed AWS golden; origin/main's own code produces 5 on
   this box too). Quantization absorbs last-ULP float differences at
   serialization; it cannot un-flip a discrete count already decided upstream.
   So AWS cannot be a silent cross-check — its goldens and this machine's
   would flip-flop ~1 value in 1000. **By owner decision (2026-07-19): this
   machine is the single canonical platform. AWS is abandoned, not kept as a
   parallel reference.**

**Consequence.** The sanctioned refresh is local (`HV_CENSUS=1`), run once per
campaign at the close, keeping the census fixtures current instead of trailing
main by tens of commits. `make regen-remote` / `scripts/aws-gate/` is
**abandoned** — not a fallback, because a fallback that disagrees on ~0.1% of
values is worse than none. **The canonical-machine constraint:** exactly one
machine regenerates and commits the census goldens (this box). Another
development machine may run censuses for its own analysis, but must NOT commit
goldens unless it is proven byte-identical to the canonical box (same libm,
same rounding) — otherwise the goldens flip-flop by regenerating host. A
cross-machine census *queue* (serialize runs to avoid contention) is fine for
throughput, but the committed goldens still come from the one canonical host.

Adopting local goldens also required **rebaselining the whole calibration
battery**: because the calibration tests *load* the fixture (golden == pin,
both stale together), origin/main's ~26 commits of un-pinned physics (the
the-rains moisture epoch et al.) had drifted invisibly; the first fresh local
regen surfaced it, and every pin was re-measured to the local value (see the
rebaseline commit and `windows/lab/tests/{calibration,branches_family_calibration,gathering_calibration}.rs`).
The "census fixtures deliberately lag" trade (0046's consequence) is retired.

**Scope.** This supersedes **only** 0046 (the AWS-only *mechanism*). Decision
0045 (one canonical census; frozen studies are evidence) is untouched — the
census *structure* stands. Decision 0042 (GitHub CI is manual-only) stands,
but its Linux cross-platform reference is now this canonical box, and the
project no longer maintains a *second* platform to check against — a
deliberate narrowing: single-platform-canonical, byte-reproducible on that
platform, is the ratified trade.

**See also.** [The Local Census](../../book/src/chronicle/the-local-census.md)
chronicle; decision 0046 (superseded); decision 0033 (quantize-at-emit for
cross-platform byte-identity); `scripts/regenerate-artifacts.sh`'s
`HV_CENSUS` guard.
