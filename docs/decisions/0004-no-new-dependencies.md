# 0004. No new dependencies beyond serde

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of a long-lived deterministic simulator, facing the usual pull
toward convenience crates (rand, chrono, clap, thiserror, a plotting library,
a YAML parser), we decided that **the workspace depends on `serde` and
`serde_json` only**, accepting that randomness, time, CLI parsing, error
types, charts, and config formats are all hand-rolled against the standard
library.

**Context.** Every dependency is a determinism risk, a supply-chain surface,
and a source of hidden state. Randomness must come from the kernel's
`Seed`/`Stream` so it is reproducible; a `rand` crate would undermine the
constitution.

**Consequence.** SVG charts, WAV audio, and bitmap output are hand-rolled and
drift-checked. CLI parsing is std-only. This constraint is the stated reason
behind [0012](0012-config-is-json-not-yaml.md). Revisit only with strong
justification; the bar is deliberately high.

**See also.** `CLAUDE.md` "Constraints and conventions".
