# 0046. Census regeneration is AWS-only

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of parallel campaign sessions sharing one M1 Max and full
census regeneration (`the-census`, `census-of-the-meeting`, 1,000 and 500
seeds of every registered metric) repeatedly driving that machine's load to
the point of starving sibling sessions, we decided that **full censuses
never regenerate on a local box unless Nathan explicitly says so** —
`scripts/regenerate-artifacts.sh` skips the census `lab run` calls by
default, running them only under `HV_CENSUS=1`, and `make regen-remote`
(`scripts/aws-gate/regen-git.sh`) is the only sanctioned path that sets it.

**Context.** Mechanized on main at commit `ecd1b96`. `make rebaseline` /
`make artifacts` still regenerate every other committed artifact locally;
only the two census `lab run` invocations are gated. `SKIP_CENSUS=1` (CI's
fast-probe path) also skips, independent of `HV_CENSUS`. The AWS spot box
doubles as the project's only Linux cross-platform verification (decision
0042), so routing census regen through it is not pure cost-avoidance — the
same run that protects the local machine's load also exercises the one
platform axis macOS can't check.

**Consequence.** A session that wants fresh census numbers locally must
either wait for `make regen-remote` or get explicit, in-the-moment
authorization to set `HV_CENSUS=1` by hand — there is no local default that
silently pays the cost. This trades a small amount of friction (an extra
remote round-trip for census-touching work) for shared-machine stability
across parallel sessions and for keeping the Linux check exercised on every
regen rather than skipped when convenient.

**See also.** Decision 0042 (GitHub CI is manual-only — the AWS runs are
the cross-platform gate this decision routes census regen through);
`scripts/aws-gate/regen-git.sh`; `scripts/regenerate-artifacts.sh`'s
`HV_CENSUS` guard.
