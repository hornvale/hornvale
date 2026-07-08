# CLAUDE.md — working in `docs/`

Guidance for a Claude session editing anything under `docs/`. The
human-facing map of this tree is [`README.md`](README.md); read it first for
*what lives where*. This file adds the *operational rules* for keeping the
knowledge architecture from drifting.

## The one habit that matters: registry-first

Before you propose a new idea, reopen an old one, or argue an architectural
or process question, **grep [`vision/idea-registry.md`](vision/idea-registry.md)**.
It is the scannable index of every speculative idea the project holds.

- A row marked `rejected` or `ratified (NNNN)` is a **closed question**.
  Reopening it needs *new information*, not a fresh opinion — say what is new,
  or drop it. The rejected-rows table exists precisely so good ideas that
  were considered and set aside are not re-proposed from scratch.
- A row marked `elaborated` already has an essay in
  [`vision/frontier.md`](vision/frontier.md); extend that, don't restart it.
- If an idea genuinely isn't there, it is new — add a one-line row (see
  below) before the conversation ends. Capturing beats elaborating; a `raw`
  stub is a success. A lost idea is gone.

## The registry and the frontier are two halves of one thing

The **frontier** holds the *essays* — the interconnected argument, why the
project stays one file (splitting it was considered and rejected: `REJ-3`).
The **registry** holds the *index* — one greppable line per idea. Editing one
usually means touching the other:

- **New idea** → add a registry row (permanent category-prefixed ID, status,
  confidence, pointer). Elaborating it later → flip the row to `elaborated`
  and point at the new frontier section.
- **New frontier `## ` section** → add a bullet to the frontier's `## Contents`
  ToC, and (usually) a registry row pointing at it.
- **Renamed frontier heading** → its anchor changes, so fix the ToC bullet
  and every registry row that links to it.
- **Idea drains into a spec** → flip the row's status (`spec'd` / `shipped`)
  and repoint **Where** at the spec. The elaboration relocates; the
  breadcrumb stays. This is how the frontier sheds mass without amnesia —
  never delete a row.
- **A vision idea becomes a decision** → cross-link the decision from the
  row (`ratified (NNNN)`); do not restate the decision's content here.

IDs are permanent. A superseded idea keeps its ID and flips its status; new
ideas take the next free number in their category; never renumber.

## The drift-check enforces the above

`cli/tests/docs_consistency.rs` runs inside the normal `cargo test --workspace`
gate and asserts three invariants:

1. every `## ` section in the frontier is listed in the Contents ToC;
2. registry IDs are unique;
3. every cross-link in the frontier, the registry, and `README.md` resolves —
   both the file and, for `#fragment` links, the target heading.

Run it directly while editing docs:

```bash
cargo test -p hornvale --test docs_consistency
```

If it fails, the message names the broken link, the missing ToC bullet, or
the duplicate ID. Fix the doc, not the test — the test encodes the
discipline, so a red result means the architecture drifted, not that the
check is wrong. (If the *rule itself* should change, change the test
deliberately and say why, exactly as with any other invariant.)

## Boundaries

- **`docs/` is the private end of the pipe; `book/` is the public end.** The
  frontier and registry deliberately stay out of the book — the book is
  merged reality only. Do not surface speculative material into `book/`.
- **Decisions are append-only.** Never edit a ratified decision's substance;
  supersede it with a new record. See
  [`decisions/README.md`](decisions/README.md).
- **The spec governs.** Where any doc here disagrees with the long-term-plan
  spec or the Constitution, the spec wins; fix the lesser doc.
