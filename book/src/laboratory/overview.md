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

### The instrument's self-check

`census-lands-drift` is a small, fast study (500 seeds, every metric the
unified registry knows — sky and land alike) that CI reruns on every build.
Its outputs are committed artifacts: if the generator's behavior ever
drifts from what's checked in, the rerun produces different numbers and the
diff fails the build. The instrument is honest because it re-proves itself
every time. (It was named `census-drift` through Campaign 3b, before the
land metrics arrived; the Campaign 3c rename is a label change only — the
same one study, still the CI-checked half of every census.)

A second committed, CI-drift-checked study joined it in The Meeting:
`census-of-the-meeting` (500 seeds), the year's **null control**. It walks
two *solo* rosters — `[goblin]` and `[goblin-twin]`, a species carrying the
goblin's exact vectors — over one shared seed range, and its calibrations
assert that the twin is structurally indistinguishable from the goblin: at
chance on blind attribution, within the sampling bound on every distribution.
It backs [Study 009, the Census of the Meeting](./study-009.md), the Year-2
capstone, and like `census-lands-drift` it is re-run and drift-checked on
every build.

The Branches added a third: `branches-family` (1,000 seeds), the family
battery behind the goblinoid phylogeny — Neogrammarian regularity,
monophyly, inventory closure, real divergence, the loudness ordering, and
the homophony ledger. Its calibration suite loads the committed rows the
same way the census calibrations do, keeping a roughly 200-second sweep
off every local `cargo test`, and it is re-run and drift-checked on every
build like the other two.

Between commits, a cheap always-on probe (`windows/lab/tests/fixture_staleness.rs`)
regenerates each census's first three seeds — plus a rotating three-seed
window whose position derives from the committed fixture's own bytes, so
successive regenerations sweep different slices of the seed range — live on
every `cargo test` and compares them against the committed rows — so a worldgen change that moves
the census fails locally with the regeneration instruction (`make
rebaseline`) instead of surfacing an hour later in CI's full
regenerate-and-diff.

When a census *does* move, the reviewable surface is `make lab-diff
STUDY=<name>` (wrapping `hornvale lab diff`): a per-metric report of which
distribution moved and by how much — count deltas per value bucket and the
numeric mean shift — rather than a raw CSV diff.

{{#include generated/census-lands-drift/census-lands-drift-summary.md}}
