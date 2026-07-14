# The Laboratory

A study is a small JSON file naming a range of seeds, a set of sky pins, and
a selection of metrics — data selecting code-side measurements, not code
selecting data. `hornvale lab run <study.json>` builds one world per seed,
extracts the selected metrics from each, and publishes a CSV, a per-metric
distribution summary, and a bar-chart SVG per metric into the book.

To author a study, list its seed range, pin sets (or `[]` for the unpinned
tier-0 path), and either `"metrics": "all"` or a named subset. Run
`hornvale lab list-metrics` to see every metric the registry currently
supports before choosing.

```bash
cargo run -p hornvale -- lab list-metrics
cargo run -p hornvale -- lab run studies/census-of-skies.study.json
```

### The author-time census family

Alongside the drift-checked study below, eight larger studies walk the same
10,000 seeds (0 through 9,999) at author time — run by hand, never reran
by CI. `census-of-skies` commits its published summary and charts (Study
001 includes them directly); `census-of-coasts` (Study 010's six
continental-shape metrics, the Campaign 25 addition) does the same, its
summary and charts embedded directly in its chapter; the other six —
`census-of-lands`, `census-of-peoples`, `census-of-faiths`, `census-of-eyes`
(Study 007's two-pantheon baseline, the Campaign Y2-2 addition),
`census-of-tongues` (Study 008's naming/voice baseline, the Campaign Y2-3
addition), and `census-of-words` (Study 011's lexicon/etymology baseline,
the Campaign 27
addition) — are never committed as raw output. Each backs one or more
chapters in this section; regenerating one is a one-line `lab run` and a
manual re-read of its headline numbers, not a CI obligation.

`census-of-coasts` re-runs the same study a second time within Campaign
25's Crust epoch (immediately after Study 011 in the book's ordering): a
**v1@L6 interim** re-measurement of the six shape metrics on the raised
grid alone, then a **v2@L6 after-photograph** once the Crust generator
lands, judged against the spec §7 acceptance bands. Both land in [The
Census of Coasts II](./census-of-coasts-ii.md) rather than a new numbered
study, since it is the same study file and the same ten thousand seeds —
only the pins under test change.

### The census family: live and frozen

One study is canonical. `the-census` walks every metric the unified
registry knows — sky and land alike — over 1,000 unselected worlds (seeds
0 through 999, default pins): the project's queryable dataset and CI's
determinism guard in one instrument. It is `census-lands-drift` promoted
and renamed (named `census-drift` through Campaign 3b, `census-lands-drift`
from Campaign 3c's land-metrics addition, `the-census` from the
census-as-data campaign — the same continuously-run instrument throughout,
its seed count raised 500 to 1,000 at the rename) — so prior history
recorded under those two names still describes today's `the-census`. A
second committed study shares the live tier: `census-of-the-meeting` (500
seeds), the **null control**. It walks two *solo* rosters — `[goblin]` and
`[goblin-twin]`, a species carrying the goblin's exact vectors — over one
shared seed range, and its calibrations assert that the twin is
structurally indistinguishable from the goblin: at chance on blind
attribution, within the sampling bound on every distribution. It backs
[Study 009, the Census of the Meeting](./study-009.md), the Year-2
capstone. Both live studies are regenerated on the remote box (`make
regen-remote`, [decision
0046](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0046-census-regen-is-remote-only.md)), then drift-checked and CI-probed on every build.

Everything else the census family has produced is frozen, not deleted.
`branches-family` (1,000 seeds, the goblinoid-phylogeny battery —
Neogrammarian regularity, monophyly, inventory closure, real divergence,
the loudness ordering, the homophony ledger) froze at the consolidation:
`the-census` subsumes its columns exactly, so its own run retired, but its
committed rows remain the evidence [decision 0016](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0016-studies-preregister-hypotheses.md)'s
preregistrations point at. `census-of-coasts` and `census-of-coasts-tuning`
— the preregistered terrain-overhaul baselines behind [The Census of Coasts
II](./census-of-coasts-ii.md) — are frozen the same way. None of the three
is ever regenerated: rerunning a frozen study under moved physics would
falsify the record it exists to preserve, since a frozen study is evidence,
not an instrument. What keeps each one a *reproducible* citation rather
than an inert snapshot is that `tools/census/manifest.json` records the
exact commit each was produced at — `branches-family` at `90fd7cc`,
`census-of-coasts` at `15ef667`, `census-of-coasts-tuning` at `2d82a6a` —
so `git checkout <commit> && lab run` reproduces any of them byte for byte.
See [decision 0045](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0045-one-canonical-census.md)
for the full ratification.

### The instrument's self-check

Between commits, a cheap always-on probe (`windows/lab/tests/fixture_staleness.rs`)
regenerates the live tier's first three seeds — plus a rotating three-seed
window whose position derives from the committed fixture's own bytes, so
successive regenerations sweep different slices of the seed range — and
compares them against the committed rows. It was authored as an always-on,
few-seconds check on every `cargo test`, but as the worldgen pipeline
deepened its cost grew to minutes, so it now runs in the heavy tier
(`make gate-full`) rather than in the commit gate: a worldgen change that
moves a census surfaces there and in CI's regenerate-and-diff, not on the
developer's next local test run. The full census fixtures themselves are refreshed once
per campaign on the remote regeneration box (`make regen-remote`),
just before the campaign merges to `main` — never on the local machine, whose
gate stays under five minutes by design. Between those refreshes a moved
census is *known* (the probe fails) but its committed rows deliberately lag
until the pre-merge regeneration.

When a census *does* move, the reviewable surface is `make lab-diff
STUDY=<name>` (wrapping `hornvale lab diff`): a per-metric report of which
distribution moved and by how much — count deltas per value bucket and the
numeric mean shift — rather than a raw CSV diff.

### The Census as Data

Every published study — live or frozen — carries a co-generated
`schema.json` alongside its `rows.csv`: typed, documented columns, the
quantization convention its floats were serialized under, a row count, and
an FNV-1a64 content hash binding the two files together, so a study's data
and its schema can never silently drift apart. `tools/census/` (outside
the workspace, like the type audit — decisions 0027/0028's pattern) turns
that commitment into a queryable instrument: `make census` builds a
throwaway DuckDB database from every committed study — typed wide views
per study plus a unified `census_long` long view — and opens a session on
it; `make census-query Q="…"` is the one-shot, non-interactive form, for a
script or a session that wants an answer rather than a REPL; `make
census-history STUDY=…` walks that study's `rows.csv` back through git
history and loads a longitudinal table, so "what did this metric look like
as of the Crust close" is a `WHERE` clause. The doctrine governing all of
it is graduation: an ad-hoc query that finds something worth keeping
graduates to a committed canned query under `tools/census/queries/explore/`,
and, if it keeps earning its keep, to a metric in the registry or a new
preregistered study — and a question you find yourself *unable* to write as
a query marks a missing metric, not a dead end. `golden-pins.sql` is the
harness's pin-provenance report: every pinned calibration constant in the
codebase is reproducible as a query against the committed fixture, so a
calibration is never just a number someone once computed and typed in.

{{#include generated/the-census/the-census-summary.md}}
