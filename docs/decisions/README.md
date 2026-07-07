# Decision Log

This directory is the durable, grep-able home for Hornvale's **ratified
decisions** — choices that are settled and must not be relitigated without new
information. Each file records one decision: the context that forced it, what
was decided, and what we accept as its cost. It consolidates the "do not
relitigate" convention that was previously scattered across the spec's
Constitution, `CLAUDE.md`, the plans' Self-Review Notes, and the chronicles.

## Conventions

- **One decision per file**, named `NNNN-kebab-title.md`, numbered in order of
  ratification.
- **Never edit a decision's substance once it is `Accepted`.** To change one,
  add a *new* record that supersedes it, and set the old record's status to
  `Superseded by NNNN`. Git carries the history; the log never rewrites it.
- **Statuses:** `Accepted`, `Superseded by NNNN`, or (rarely) `Proposed`.
- Where a decision is stated authoritatively elsewhere (the Constitution in
  the long-term-plan spec, or `CLAUDE.md`), the record **cross-links** the
  canonical source rather than duplicating its wording.
- Keep each record short — a Y-statement plus a few lines. If it needs a page,
  it is probably a spec, not a decision record.

## Template

```markdown
# NNNN. Title

**Status:** Accepted (YYYY-MM-DD) · **Decider:** Nathan

In the context of <situation>, facing <problem/forces>, we decided
<the choice>, accepting <the trade-off>.

**Context.** Why this came up.
**Consequence.** What follows, and what we knowingly give up.
**See also.** Links to spec sections, CLAUDE.md, chronicles.
```

## Index

| # | Decision | Status |
|---|----------|--------|
| [0001](0001-determinism-is-constitutional.md) | Determinism is constitutional | Accepted |
| [0002](0002-domains-depend-only-on-kernel.md) | Domains depend only on the kernel | Accepted |
| [0003](0003-trace-protocol-is-the-only-cross-domain-channel.md) | The trace protocol is the only cross-domain channel | Accepted |
| [0004](0004-no-new-dependencies.md) | No new dependencies beyond serde | Accepted |
| [0005](0005-deterministic-collections-and-sorts.md) | Deterministic collections and sorts | Accepted |
| [0006](0006-seed-labels-are-permanent-contracts.md) | Seed-derivation labels are permanent contracts | Accepted |
| [0007](0007-seed-is-identity.md) | The seed is a world's identity | Accepted |
| [0008](0008-typed-quantities.md) | Typed quantities at API boundaries | Accepted |
| [0009](0009-models-author-dice-roll.md) | Models author, dice roll | Accepted |
| [0010](0010-predicate-schema-value-kind-enforced.md) | Predicate schema: value-kind enforced, subject-kind refused | Accepted |
| [0011](0011-studies-are-data-metrics-are-code.md) | Studies are data, metrics are code | Accepted |
| [0012](0012-config-is-json-not-yaml.md) | Config files are JSON, not YAML | Accepted |
| [0013](0013-definition-of-done-includes-the-book.md) | Definition of Done includes the project book | Accepted |
| [0014](0014-fact-day-stays-bare-option.md) | `Fact.day` stays a bare `Option<f64>` | Accepted |
| [0015](0015-predicatedef-name-duplicates-key.md) | `PredicateDef.name` duplicates its registry key | Accepted |
| [0016](0016-studies-preregister-hypotheses.md) | Studies preregister their hypotheses | Accepted |
| [0017](0017-campaigns-drop-year-naming.md) | Campaigns drop the "Year" naming | Accepted |
| [0018](0018-gallery-images-are-hand-rolled-png.md) | Gallery images are hand-rolled PNG | Accepted |
| [0019](0019-no-procedural-macros.md) | No procedural macros | Accepted |
| [0020](0020-campaigns-write-retrospectives.md) | Campaigns write retrospectives | Accepted |
