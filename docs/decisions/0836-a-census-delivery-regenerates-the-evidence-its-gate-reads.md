# 0836. A census delivery regenerates the evidence its gate reads, and defers only what needs a human re-statement

**Status:** Accepted (2026-09-06) · **Campaign:** The Spillway · **Decider:**
Nathan · **Relates:**
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md),
[0139](0139-main-advances-only-through-the-lock.md),
[0514](0514-a-census-refresh-needs-no-authorization.md)

In the context of a queued census refresh that registers a metric being
refused by its own delivery commit — the commit gate compares the Gnomon
injection arms' column set to the census's, the arms' authoring script
refused the delivery's staged goldens as dirt, and a second gate test carries
the column count in its own name — we decided that **the delivery satisfies
every check whose remedy is a regeneration and defers only a check whose
remedy is a human re-statement**, accepting roughly eight more minutes of the
canonical box on a moving census, a lock held without a claim file for those
minutes, and a delivery branch that carries a deferred witness until the
campaign that merges it re-states it.

**Context.** The Warp's census at `4a419e996ef7` moved 136 goldens and could
not be committed (ledger #12): the goldens were landed through an ungated
intermediate object and a by-hand re-authoring on lefford. `HV_CENSUS_DELIVERY`
already stood down two checks for a delivery commit, each justified
separately; this record states the rule they were instances of and places
two more checks by it.

**Decision.**
1. `scripts/sluice-census.sh` re-authors the Gnomon injection arms at the
   censused ref, with that ref's own `gnomon-injection.sh`, under the shared
   flock (0133), timed into `docs/timings.md` as `gnomon-injection`, and
   commits them with the goldens — whenever the world moved (a census golden
   changed) or an arm's column set differs from the census's. The manifest's
   `sha` is therefore the census's ref and its host the canonical one (0079).
2. `gnomon-injection.sh`'s clean-tree guard covers everything a build or its
   mutation can see and excludes `book/`, `docs/` and its own fixtures.
3. The column-count witness
   (`domesday::anomaly::tests::evaluable_columns_measured_surface_on_the_<N>_column_census`)
   is stood down for the delivery commit only, by omitting its roster term.
   The merge of the delivery branch runs it and demands the re-pin — safe
   because a delivery never pushes `main` (0139).
4. The next census-shaped check is placed by the rule, not by precedent: if a
   machine on the canonical box can regenerate what it reads, the delivery
   does so; if a human must re-state it, the delivery defers it.

**Consequence.** A refresh that grows the registry delivers on its own; the
by-hand path (`ssh lefford … scripts/gnomon-injection.sh`) survives as the
exception. A ref predating The Spillway carries the old guard and is refused
at the delivery's pre-flight with the goldens left staged. No claim file is
written for the arms, so status readers see nothing for those minutes while
every flock-taker waits.

**See also.** `docs/superpowers/specs/2026-09-06-the-spillway-design.md` §3;
`scripts/sluice-census.sh` header; `scripts/hooks/pre-commit` (the third
`HV_CENSUS_DELIVERY` branch); `scripts/test-census-guard.sh` (pins the
stand-down count at three).
