# CLAUDE.md — working in `book/src/frontier/`

This directory is **The Frontier** — the book's one speculative part: the
frontier essays (`frontier.md`) and the idea registry (`idea-registry.md`).
It is published, but it is NOT merged reality; it governs nothing, and
specs bind over it (decision 0031).

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
- **Repointing Where REPLACES the row's prose — it does not append to it.**
  A campaign's narrative belongs in its chronicle, which the row links; the
  row says what the idea *is*, in one line. Appending "**Shipped X**: …" to a
  row on every campaign that touches it is what grew this file to 673 KB.
- **A vision idea becomes a decision** → cross-link the decision from the
  row (`ratified (NNNN)` or the slug); do not restate the decision here.

IDs are permanent. A superseded idea keeps its ID and flips its status; new
ideas take a fresh category+slug; never renumber.

## A row is a shelf-mark

The Idea cell is capped at **600 characters**, enforced by
`cli/tests/docs_consistency.rs`. The cap is on the Idea cell only — the Where
column carries full GitHub blob URLs by design (see Link discipline below) and
is never counted against the budget.

A row over the cap is nearly always *mislabeled* rather than merely long.
`raw` means "a stub, not an argument"; `elaborated` means "has a full essay in
`frontier.md`". A row carrying 900 characters of argument is not `raw`. The
three legal remedies:

1. **Compact** — the prose duplicates a chronicle or spec the Where cell
   already links. Delete it; keep the pointer.
2. **Relocate** — it is real argument. Move it to a `frontier.md` section, add
   the ToC bullet, flip `raw` → `elaborated`, point **Where** at the anchor.
3. **Trim** — it is a stub that got wordy. Cut to one clause.

Rows that were over the cap when it landed are listed in
`cli/tests/fixtures/registry-length-waivers.txt`. That list is
**append-never**: compacting a row means deleting its line from the file, and
a test reddens if you compact without pruning. Never add to it — a new row
over the cap is a failure to fix, not a fixture to edit.

## Row form

- **Five columns**, always: `| ID | Idea | Status | Conf | Where |`.
- **Escape bare pipes in prose as `\|`** — including inside `` `code spans` ``,
  where GFM splits the cell anyway. An unescaped pipe shifts every later column
  left and silently drops the Where cell from the published page. Three rows
  sat that way undetected until the column check was written.
- **Status is a closed vocabulary**: `raw`, `elaborated`, `spec'd`, `shipped`,
  `ratified (NNNN)`, `rejected`, `refuted (evidence)`. Category prefixes are
  open — coin a new one freely — but statuses are not. Do not invent one.
- **New IDs are category+slug** (`LANG-exonyms`, not `LANG-6`), per decision
  `0026-slugs-not-numbers`. The numbered era is frozen at the 403 IDs in
  `cli/tests/fixtures/registry-numbered-ids.txt`; those keep their names
  forever.
- **Where is never empty.** A row with nothing to point at is not a row.

## A row's status is self-reported, and its prose is unchecked

Nothing derives the Status column. It is written by whoever last touched the
row, from their own sense of how settled the idea felt, and the drift-check
validates only that the value is in the closed vocabulary — never that it is
*true*. So:

- **`spec'd` can mean "a brainstorm decided to do it."** `BIO-three-probes`
  carried `spec'd` with no spec in existence; its **Where** cell cited a
  conversation, not a file. A Where cell pointing at something unopenable is
  the tell.
- **A row's prose can encode design guesses never checked against the code.**
  Two of that row's three technical claims were false: the elf's "long-lived"
  cell was inexpressible (`lifespan` is a pure function of mass, with no
  per-kind override), and "a people on the MINERAL axis" would have made
  dwarves lithovores, because `MINERAL` is a trophic `Stock`.

Before building on a row, verify its claims against the code the same way you
would verify any other doc. A registry row is a *shelf-mark*, not evidence.

## Scan before you mint — a targeted grep is not a scan

`docs/README.md` says to scan the registry before proposing. Scanning means
reading the category, not grepping your own phrasing: the same idea is
routinely already banked in an **adjacent category under a different name**. A
2026-07-16 magic brainstorm grepped `"graph grammar"`, got zero hits, concluded
the idea was new, and ran nine ideonomy passes before reading the registry
properly — by which point several "findings" turned out to be `MEM-7`, `UNI-8`
and `UNI-9`, one of them stated more sharply than the new version reached.

The numbered-ID collision this used to also cause is now closed by
construction: decision `0026-slugs-not-numbers` froze the numbered era, so new
IDs are category+slug and cannot be minted by taking "the next integer" from a
staging doc. (That is how `TOOL-24` was minted twice, reaching a spec, a plan,
a study JSON and a decision record before the campaign close caught it.) The
*semantic* collision above is the half that survives, and nothing mechanical
catches it.

## Link discipline

Links between these two files stay relative. Links to anything under
`docs/` (decisions, specs, the docs map) are GitHub blob URLs —
`https://github.com/hornvale/hornvale/blob/main/docs/…` — because those
files are not published in the book. The drift-check
(`cargo test -p hornvale --test suite -- docs_consistency`) validates both kinds and
enforces ToC completeness and ID uniqueness, alongside the row-form rules
above. Fix the doc, not the test.

## Boundary

Registry IDs (`MAP-7`, `LANG-3`, …) may appear ONLY in this directory —
every other part of the book describes merged reality, and the drift-check
enforces the ban. Do not cite registry IDs from chronicle, domain, or any
other chapter.
