# 0027. Non-workspace dev tools may use parser libraries

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of enforcing the typed-quantities convention (decision 0008)
that nothing currently polices, facing the need to parse Rust source to find
primitives at public boundaries, we decided that **offline developer tools
that live outside the Cargo workspace may depend on parser libraries (`syn`,
`proc-macro2`); their outputs are committed and drift-checked**, accepting a
second dependency stack that the sim's build graph never sees.

**Context.** Decision 0019 bans procedural macros because `syn`/`quote` would
enter the *sim's* build graph, and because the codegen candidates there were
save-format contracts whose value is being plain-text. Neither force applies
to a checker that never ships in a world: it is `tools/type-audit/`, excluded
from the workspace (root `Cargo.toml` `exclude = ["tools/type-audit"]`), with
its own lockfile, never built by `cargo … --workspace`. This is the same
posture as decisions 0009 ("models author, dice roll") and 0023 (clients carry
their own toolchains): build offline, commit the output, drift-check its
freshness.

**Consequence.** `tools/type-audit/` is the first citizen; its CI presence is
two lines in the "Artifacts are current" step (`check`, then `report`
regenerating `docs/audits/type-audit-report.md`), never inside
`cargo test --workspace`. The workspace's serde-only allowlist (decision 0004)
and the no-proc-macro rule (0019, which still governs *workspace* crates) are
untouched. Future offline dev tools follow the same shape: outside the
workspace, own lockfile, committed and drift-checked outputs.

**See also.** Decisions 0019, 0009, 0023, 0004, 0008; the type-audit spec
(`docs/superpowers/specs/2026-07-09-the-type-audit-design.md`); decision
0028 (the rubric the tool enforces). Slug filename, not a
number, per decision 0026 (slugs, not numbers).
