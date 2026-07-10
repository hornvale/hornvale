# CLAUDE.md — working in `docs/`

Guidance for a Claude session editing anything under `docs/`. The
human-facing map of this tree is [`README.md`](README.md); read it first for
*what lives where*. This file adds the *operational rules* for keeping the
knowledge architecture from drifting.

## The one habit that matters: registry-first

Before you propose a new idea, reopen an old one, or argue an architectural
or process question, **grep
[`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md)**.
It is the scannable index of every speculative idea the project holds.

- A row marked `rejected` or `ratified (NNNN)` is a **closed question**.
  Reopening it needs *new information*, not a fresh opinion — say what is new,
  or drop it. The rejected-rows table exists precisely so good ideas that
  were considered and set aside are not re-proposed from scratch.
- A row marked `elaborated` already has an essay in
  [`book/src/frontier/frontier.md`](../book/src/frontier/frontier.md);
  extend that, don't restart it.
- If an idea genuinely isn't there, it is new — add a one-line row (see
  below) before the conversation ends. Capturing beats elaborating; a `raw`
  stub is a success. A lost idea is gone.

## The registry and the frontier moved into the book

The frontier essays and the idea registry live at `book/src/frontier/` —
the book's one clearly-marked speculative part (decision
`the-frontier-is-published-in-the-book`). Their editing discipline (IDs are
permanent, ToC completeness, status flips, link conventions) lives beside
them in [`book/src/frontier/CLAUDE.md`](../book/src/frontier/CLAUDE.md).
The old `docs/vision/` paths hold redirect stubs so historical links
resolve; do not add new content under `docs/vision/`.

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

- **The book's merged-reality contract is per-part.** The frontier and
  registry are published as the book's clearly-marked Frontier part
  (decision `the-frontier-is-published-in-the-book`); every other part of
  the book describes merged reality — do not surface speculative material
  anywhere else in `book/`, and do not cite registry IDs outside
  `book/src/frontier/` (the drift-check enforces this). Specs, plans, and
  decisions stay in `docs/`.
- **Decisions are append-only.** Never edit a ratified decision's substance;
  supersede it with a new record. See
  [`decisions/README.md`](decisions/README.md).
- **The spec governs.** Where any doc here disagrees with the long-term-plan
  spec or the Constitution, the spec wins; fix the lesser doc.
