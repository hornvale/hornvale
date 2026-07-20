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
   one Linux cross-platform check (decision 0042). The regen box here **is**
   Linux (glibc) — so regenerating the goldens locally exercises the same
   platform axis, and the cross-platform byte-identity contract (quantize at
   the emit boundary, decision 0033) means a macOS session's census-probe
   still validates against these Linux-generated goldens. The Linux check is
   preserved, not lost.

**Consequence.** The sanctioned refresh is local (`HV_CENSUS=1`), run once per
campaign at the close, keeping the census fixtures current instead of trailing
main by tens of commits. `make regen-remote` / `scripts/aws-gate/` is
**retired as the mandatory path** — kept only as an optional fallback for a
sweep too large for the local box, and never required for a normal campaign
close. The "census fixtures deliberately lag" trade (0046's consequence) is
retired with it.

**Scope.** This supersedes **only** 0046 (the AWS-only *mechanism*). Decision
0045 (one canonical census; frozen studies are evidence) is untouched — the
census *structure* stands. Decision 0042 (GitHub CI is manual-only) also
stands; this decision merely observes that the Linux axis it valued is now
covered by the local regen box rather than exclusively by AWS.

**See also.** [The Local Census](../../book/src/chronicle/the-local-census.md)
chronicle; decision 0046 (superseded); decision 0033 (quantize-at-emit for
cross-platform byte-identity); `scripts/regenerate-artifacts.sh`'s
`HV_CENSUS` guard.
