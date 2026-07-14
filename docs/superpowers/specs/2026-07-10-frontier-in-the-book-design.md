# The Frontier in the Book — design

**Date:** 2026-07-10
**Status:** Approved design, awaiting implementation plan
**Supersedes:** the documented convention that the frontier and idea
registry stay out of the book (`docs/CLAUDE.md` §Boundaries, the frontier
preamble). This spec is accompanied by a decision record ratifying the
change (see §6).

## 1. Motivation and the new information

The frontier essays (`docs/vision/frontier.md`, ~1400 lines) and the idea
registry (`docs/vision/idea-registry.md`, ~340 lines) are the project's
speculative end of the pipe. The boundary "the book is merged reality only;
the frontier stays out" was drawn deliberately and is enforced executably
(`cli/tests/docs_consistency.rs::the_book_carries_no_registry_ids_or_process_vocabulary`).

The new information that reopens it: **the raw markdown is effectively
unreadable unrendered.** The registry is a table whose idea cells run to
hundreds of words (MAP-19, LANG-3); without HTML rendering it cannot be
scanned, which defeats its purpose as the retrieval surface. The book is the
project's one rendered reading surface. Publishing the frontier there — as a
clearly-marked speculative part — solves readability without weakening the
contract, which becomes per-part instead of per-book: *every part of the
book except The Frontier describes merged reality; The Frontier is
speculative and governs nothing.*

## 2. Book structure

A new final part in `book/src/SUMMARY.md`, after Open Questions:

```markdown
# The Frontier

- [The Frontier](./frontier/frontier.md)
- [The Idea Registry](./frontier/idea-registry.md)
```

- The essays stay **one chapter, one file**. REJ-3 (splitting the frontier
  into topic files) is a settled rejection; nothing here is new information
  against it. mdbook renders a 1400-line chapter fine.
- Essays first (the argument), registry second (the reference surface).
- Both files keep their strong "SPECULATIVE — governs nothing" preambles.
  The sentence "stays out of the book" is replaced with the new contract:
  published in the book as a clearly-marked speculative part; the rest of
  the book describes merged reality; specs still bind over the frontier.

## 3. Migration mechanics

1. `git mv docs/vision/frontier.md book/src/frontier/frontier.md`
2. `git mv docs/vision/idea-registry.md book/src/frontier/idea-registry.md`
3. The intra-pair links (`frontier.md#anchor` ↔ `idea-registry.md`) survive
   unchanged — the files remain siblings.
4. **Redirect stubs at the old paths.** `docs/vision/frontier.md` and
   `docs/vision/idea-registry.md` are recreated as one-paragraph stubs
   pointing at the new home (repo path and published URL). Rationale:
   ratified decisions (0009, 0016, 0021, …) and historical plans/specs link
   into `docs/vision/`; decisions are append-only and historical documents
   are frozen, so the old paths must keep resolving rather than every
   inbound link being edited. Fragment links into the stubs will no longer
   hit a heading; that is acceptable for frozen historical documents and is
   outside the drift-check's scope (it link-checks only the live knowledge
   docs).
5. **Live docs are repointed directly** (not via the stubs):
   `docs/README.md`, `docs/CLAUDE.md`, root `CLAUDE.md`.

## 4. Link policy for the moved files

- **Outbound links to unpublished repo files** (`docs/decisions/`,
  `docs/superpowers/specs/`, `docs/README.md`) are rewritten to GitHub blob
  URLs: `https://github.com/hornvale/hornvale/blob/main/docs/...`. These
  work on the published site, on GitHub, and in editors.
- Links between the two moved files stay relative.
- No links from the frontier part into the rest of the book are required by
  this migration; if any exist they become normal relative book links.

## 5. The drift-check follows the files

`cli/tests/docs_consistency.rs` changes:

1. **Paths update** to `book/src/frontier/…` in every test that reads the
   frontier or registry.
2. **GitHub blob URLs are validated, not skipped.** `check_links` currently
   skips all `http(s)` URLs. It gains a mapping: a URL prefixed
   `https://github.com/hornvale/hornvale/blob/main/` is converted to a
   repo-relative path and validated exactly like a relative link (existence
   plus `#fragment` heading check). Link rot in the rewritten links stays
   loud. Other external URLs remain skipped.
3. **The registry-ID ban is rescoped.**
   `the_book_carries_no_registry_ids_or_process_vocabulary` excludes
   `book/src/frontier/` from the registry-ID scan (the registry may cite
   itself); the rest of the book stays banned, which preserves the test's
   original purpose — chronicle and domain chapters must not cite registry
   IDs. The process-vocabulary check is untouched (it already scopes to
   chronicle/domains only). The test's doc comment and failure message are
   reworded to state the per-part contract.
4. The two vision-file redirect stubs are **not** added to the link-checked
   set; the live knowledge docs checked are `docs/README.md` and the two
   moved files at their new paths.

The three invariants (ToC completeness, ID uniqueness, links resolve) are
otherwise unchanged.

## 6. Ratifying the boundary change

A new decision record, slug-named per decision 0026:
`docs/decisions/0031-the-frontier-is-published-in-the-book.md`. It records:

- the prior convention and where it was stated;
- the new information (unrenderable registry tables; the book is the one
  rendered surface);
- the new contract (merged-reality is per-part; The Frontier part is
  speculative and governs nothing; specs bind over it);
- what is deliberately unchanged (REJ-3 one-file frontier; registry-first
  discipline; the drain-to-specs lifecycle; decisions stay out of the book).

`docs/CLAUDE.md` §Boundaries is rewritten to match (the operational
registry-first rules keep working with paths updated). `docs/README.md`'s
map and pipe diagram are updated (the vision docs' home is now
`book/src/frontier/`; the pipe now ends *visibly* in the book at both
ends). The book's `introduction.md` is checked for "merged reality only"
phrasing and adjusted to the per-part contract if present.

## 7. Out of scope

- Reshaping the registry's mega-table into a friendlier rendered form (a
  separate readability question; the IDs/statuses/anchors all stay as they
  are).
- Publishing `docs/decisions/` or the specs in the book.
- Any change to registry content, IDs, statuses, or the frontier essays
  beyond the preamble sentences and link rewrites described above.

## 8. Testing and verification

- `cargo test -p hornvale --test docs_consistency` — all invariants green at
  the new paths; a deliberately broken GitHub blob URL fails in a local
  check (then reverted) to prove the new mapping bites.
- `mdbook build book` — builds clean; the new part renders.
- The full gate: `cargo test --workspace`, `cargo fmt --check`,
  `cargo clippy --workspace --all-targets -- -D warnings`.
- Manual: open the rendered frontier and registry chapters; confirm the
  registry table is readable and outbound decision/spec links resolve on
  GitHub.
