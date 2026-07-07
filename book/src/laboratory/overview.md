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

### The instrument's self-check

`census-lands-drift` is a small, fast study (500 seeds, every metric the
unified registry knows — sky and land alike) that CI reruns on every build.
Its outputs are committed artifacts: if the generator's behavior ever
drifts from what's checked in, the rerun produces different numbers and the
diff fails the build. The instrument is honest because it re-proves itself
every time. (It was named `census-drift` through Campaign 3b, before the
land metrics arrived; the Campaign 3c rename is a label change only — the
same one study, still the CI-checked half of every census.)

{{#include generated/census-lands-drift/census-lands-drift-summary.md}}
