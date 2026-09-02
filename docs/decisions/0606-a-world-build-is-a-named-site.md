# 0606. A world build is a named site on a bidirectional roster

**Status:** Proposed (2026-09-02) · **Decider:** Nathan · **Relates to:**
[0092](0092-derivation-at-named-sites.md),
[0041](0041-libm-for-portable-transcendentals.md),
`cli/tests/suite/test_binary_ratchet.rs`

In the context of the workspace's five world-build entry points
(`build_world`, `build_world_to`, `build_world_to_with_artifacts`,
`build_world_observed`, `history_for`) being called from hundreds of sites
across the test suite — a full seed-42 build costs ~3.0s in a debug build, and
nextest's process-per-test model means a per-process memo recovers nothing
(decision 0032 recorded the identical conclusion for the census) — we decided
that **every call site to one of these entry points is a named row on a
roster, checked in both directions**, rather than left to accrete silently or
banned outright by a lint.

**Context.** The mechanism ruling: a source-scan enforcement test
(`cli/tests/suite/world_build_sites.rs`, roster in
`cli/tests/fixtures/world-build-sites.tsv`) rather than a `clippy.toml`
`disallowed-methods` entry. Decision 0092 already recorded the trap in that
lint: it is one on/off switch per scope, and a single crate-level `#[allow]`
there silenced all 24 platform-libm bans (decision 0041) across worldgen as a
side effect. Grandfathering hundreds of build sites into the same mechanism
would repeat that collateral at a far larger scale. `test_binary_ratchet.rs`
is the shipped idiom for this shape — freeze a roster, check both directions —
and CLAUDE.md names `cli/` as the home of the workspace-wide enforcement
tests.

**The mechanism.** Each roster row is `<repo-relative path>\t<count>\t<reason:N
...>`, keyed by an occurrence count per file rather than by line number, so an
unrelated edit above a site cannot redden the guard. Five reason codes:
`build-path` (asserts on the build itself — byte-identity, stream consumption
order, pin isolation), `artifacts` (needs `GeneratedTerrain`/
`GeneratedClimate`, which are `Clone` but not `Serialize`), `identity` (a seed
or pin set with no committed fixture), `production` (a real build on a real
code path), and `unmigrated` — grandfathered, the only reason that may not
grow, bounded by `UNMIGRATED_CEILING` in `world_build_sites.rs`. The guard
asserts *live ⊆ roster* and *roster ⊆ live*; it does not assert that a row's
reason is the correct one, only that every site is accounted for. It is a
textual scan, so an entry-point name inside a comment or string literal counts
as a site — the same accepted imprecision `build_path_embedding.rs` states of
itself.

**Consequence.** At ratification the roster was generated in full, every row
`unmigrated` by construction: 350 sites across 200 files, `UNMIGRATED_CEILING`
set to 350. A generated file that size invites rubber-stamping, so no row may
carry a permanent reason code except by a human reclassifying it — the ratchet
starts at its maximum and only ever shrinks. The closing ceiling, after this
campaign's migrations land, is recorded here when this record is finalized at
campaign close.

**See also.** Spec `docs/superpowers/specs/2026-09-02-the-reservoir-design.md`
§3.1-§3.2 and §8; decision 0032 (the census's identical nextest-process-model
conclusion); decisions 0092 and 0041 (the `disallowed-methods` collateral this
avoids repeating). Slug filename per decision 0026.
