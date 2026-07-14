# The Hornvale documentation map

This is the **doc-of-docs**: what kind of knowledge lives where, and how an
idea flows from a first mention to merged reality. It exists so that neither
a new collaborator nor a fresh session has to rediscover the structure — and
so that ideas are neither *lost* (a capture failure) nor *relitigated* (a
retrieval failure). When in doubt about where something belongs or where to
look for it, start here.

Two governing documents sit above everything and win any conflict: the
**Constitution** (in the long-term-plan spec, mirrored at
[`book/src/constitution.md`](../book/src/constitution.md)) and the project
**book** ([`book/`](../book/), the published reality). Where a lesser
document disagrees with the spec, the spec governs.

## The idea pipeline

An idea moves left to right; the store that owns it changes as it matures.

```
  spoken idea
      │  captured as one line
      ▼
  idea-registry.md ──┐         (the scannable index of every idea + status)
      │              │
      │  elaborated  │  each store cross-links, none duplicates
      ▼              │
  frontier.md        │         (the essays: interconnected argument, confidence-tagged)
      │              │
      │  a campaign pulls for it
      ▼              │
  specs/*  ──────────┤         (binding design for one campaign)
      │              │
      ▼              │
  plans/*            │         (execution: staged, subagent-driven)
      │              │
      ▼              │
  code + book/src/chronicle/   (merged reality + its product story)
      │              │
      ▼              ▼
  retrospectives/*   decisions/*   (process lessons ·· settled choices, cross-cutting)
```

(The registry and frontier live in the book — `book/src/frontier/`, the
published Frontier part — since decision
0031; the pipeline itself is unchanged.)

The registry entry never dies — as an idea drains rightward, its **row flips
status and its pointer follows** (`elaborated` → `spec'd` → `shipped`). The
elaboration relocates; the breadcrumb stays. That is how the frontier sheds
mass without amnesia.

## Where each kind of knowledge lives

| You are looking for… | Go to | Notes |
|----------------------|-------|-------|
| **Is this idea already thought of? decided against?** | [`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md) | The retrieval surface. Check here *first*, always. |
| **The argument behind a speculative idea** | [`book/src/frontier/frontier.md`](../book/src/frontier/frontier.md) | Non-binding essays, confidence-tagged. Governs nothing. |
| **A settled choice — "why is it this way?"** | [`decisions/`](decisions/) | Ratified, grep-able, supersede-don't-edit. Do not relitigate. |
| **The binding design for a campaign** | [`superpowers/specs/`](superpowers/specs/) | One spec per campaign; binds over the frontier. |
| **The execution plan for a campaign** | [`superpowers/plans/`](superpowers/plans/) | Staged, with success criteria and tests. |
| **What was built, as a story** | [`../book/src/chronicle/`](../book/src/chronicle/) | Product history; published. |
| **What the process taught** | [`retrospectives/`](retrospectives/) | Process lessons, one page per campaign. |
| **The law** | [`../book/src/constitution.md`](../book/src/constitution.md) | Constitutional; the spec is authoritative. |
| **The world as it currently is** | [`../book/`](../book/) | Published reality; never lags a merge. |
| **The wider, non-binding vision** | [`book/src/frontier/`](../book/src/frontier/) | The Frontier part of the book; `vision/` holds redirect stubs only. |

## The two failure modes this structure defends against

- **Losing an idea (capture).** Every idea spoken in a strategy session gets
  at least a one-line registry row *before* the conversation ends. A row is
  cheap; a lost idea is gone. Capture beats elaboration — a `raw` stub is a
  success.
- **Relitigating an idea (retrieval).** Before proposing or reopening
  anything, scan the registry. A `rejected` or `ratified` row is a closed
  question; reopening it requires *new information*, not a fresh opinion.
  The registry's rejected-rows table and the decision log are the two homes
  for "we already answered that."

## Conventions at a glance

- **Decisions** are numbered `NNNN-kebab.md`, never edited once `Accepted`,
  superseded by new records. See [`decisions/README.md`](decisions/README.md).
- **Registry IDs** are permanent category-prefixed handles; status flips,
  IDs do not. See
  [`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md).
- **The frontier drains into specs**, not into sibling frontier files; it is
  a holding pen, not an archive.
- **The book's merged-reality contract is per-part.** The frontier and
  registry are published as the book's marked Frontier part (decision
  0031); every other part of the book is
  merged reality. Specs, plans, and decisions stay in `docs/`.
- **Definition of Done** for a merged campaign includes a chronicle entry, a
  book freshness sweep, and a retrospective (decisions 0013, 0020).
- **Non-workspace dev tools** analyze committed artifacts without joining
  the crate graph (decisions 0027/0028): `tools/type-audit/` checks every
  `pub`-boundary primitive carries a unit-typing verdict; `tools/census/` —
  the census analysis harness — mounts DuckDB views over every committed
  `rows.csv`+`schema.json` pair, git-history longitudinal extraction, and
  canned explore/calibrate queries (`make census`, `make census-query`,
  `make census-history`, `make census-check`).
