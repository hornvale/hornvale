# CLAUDE.md — working in `book/src/frontier/`

This directory is **The Frontier** — the book's one speculative part: the
frontier essays (`frontier.md`) and the idea registry (`idea-registry.md`).
It is published, but it is NOT merged reality; it governs nothing, and
specs bind over it (decision `the-frontier-is-published-in-the-book`).

## The registry and the frontier are two halves of one thing

The **frontier** holds the *essays* — the interconnected argument, why it
stays one file (splitting it was considered and rejected: `REJ-3`). The
**registry** holds the *index* — one greppable line per idea. Editing one
usually means touching the other:

- **New idea** → add a registry row (permanent category-prefixed ID, status,
  confidence, pointer). Elaborating it later → flip the row to `elaborated`
  and point at the new frontier section.
- **New frontier `## ` section** → add a bullet to the frontier's
  `## Contents` ToC, and (usually) a registry row pointing at it.
- **Renamed frontier heading** → its anchor changes, so fix the ToC bullet
  and every registry row that links to it.
- **Idea drains into a spec** → flip the row's status (`spec'd` / `shipped`)
  and repoint **Where** at the spec. Never delete a row.
- **A vision idea becomes a decision** → cross-link the decision from the
  row (`ratified (NNNN)` or the slug); do not restate the decision here.

IDs are permanent. A superseded idea keeps its ID and flips its status; new
ideas take the next free number in their category; never renumber.

## Link discipline

Links between these two files stay relative. Links to anything under
`docs/` (decisions, specs, the docs map) are GitHub blob URLs —
`https://github.com/hornvale/hornvale/blob/main/docs/…` — because those
files are not published in the book. The drift-check
(`cargo test -p hornvale --test docs_consistency`) validates both kinds and
enforces ToC completeness and ID uniqueness. Fix the doc, not the test.

## Boundary

Registry IDs (`MAP-7`, `LANG-3`, …) may appear ONLY in this directory —
every other part of the book describes merged reality, and the drift-check
enforces the ban. Do not cite registry IDs from chronicle, domain, or any
other chapter.
