# 0011. Studies are data, metrics are code

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of the Hornvale Lab's batch experiment harness, facing the
perennial pull for configuration files to grow into a programming language, we
decided that **a study file only *selects* named extractors from a code-side
registry; it never *defines* computation**, accepting that adding a new
measurement means writing Rust and shipping a new metric, not editing config.

**Context.** Config-as-DSL is a well-known trap: it ends with an
underpowered, untestable interpreter embedded in data. Keeping metrics in code
means they are type-checked, unit-tested, and reviewed like everything else;
keeping studies in data means experiments are cheap to author and diff.

**Consequence.** A study names a seed range, pin sets, and either `"all"` or a
list of registry metric names. The runner never changes when the registry
grows. Config-defined metrics are explicitly and permanently deferred.

**See also.** Campaign L0 spec §2.1; Laboratory book chapter; chronicle L0.
