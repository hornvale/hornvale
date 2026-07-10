# The Frontier in the Book — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move `docs/vision/frontier.md` and `docs/vision/idea-registry.md` into the book as a clearly-marked speculative final part, with the drift-check following the files.

**Architecture:** A `git mv` into `book/src/frontier/`, outbound links to unpublished repo files rewritten as GitHub blob URLs, redirect stubs at the old paths (append-only decisions and frozen plans keep resolving), and `cli/tests/docs_consistency.rs` updated red-first: new paths, GitHub-URL validation, and the book-wide registry-ID ban rescoped to exclude the new part.

**Tech Stack:** Markdown, mdBook, one Rust test file. No workspace code changes beyond `cli/tests/docs_consistency.rs`.

**Spec:** `docs/superpowers/specs/2026-07-10-frontier-in-the-book-design.md`

## Global Constraints

- Every commit passes the full gate: `cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`.
- No new dependencies (decision 0004). No `HashMap`/`HashSet`. Rust edition 2024 (let-chains are available and already used in this test file).
- Decisions are append-only — do **not** edit any existing file under `docs/decisions/` except appending one row to its `README.md` index.
- Registry content, IDs, statuses, and the frontier essays' bodies are untouched except the preamble sentences and link rewrites specified below.
- New decision records are slug-named, not numbered (decision 0026).
- Repo remote: `https://github.com/hornvale/hornvale`. Book publishes to `https://hornvale.github.io/hornvale/`.
- End every commit message with:
  `Claude-Session: https://claude.ai/code/session_01AVcNDBVsLpjnS19xdreZei`

---

### Task 1: Move the files; the drift-check moves first (red → green, one commit)

**Files:**
- Modify: `cli/tests/docs_consistency.rs`
- Move: `docs/vision/frontier.md` → `book/src/frontier/frontier.md`
- Move: `docs/vision/idea-registry.md` → `book/src/frontier/idea-registry.md`
- Modify: `book/src/SUMMARY.md`
- Create: `docs/vision/frontier.md` (redirect stub), `docs/vision/idea-registry.md` (redirect stub)
- Test: `cli/tests/docs_consistency.rs` (this IS the test)

**Interfaces:**
- Produces: the two chapters at `book/src/frontier/{frontier,idea-registry}.md` (Task 2's live docs point here); the const `REPO_BLOB_PREFIX = "https://github.com/hornvale/hornvale/blob/main/"` inside the test; the link convention that every frontier/registry link into `docs/` is a GitHub blob URL under that prefix.

- [ ] **Step 1: Update the drift-check to the target state (the failing test)**

In `cli/tests/docs_consistency.rs`, make four changes.

**(a)** Add a const above `check_links` and replace the whole `check_links` function (the change: GitHub blob URLs into this repo are mapped to local paths and validated; other external URLs stay skipped):

```rust
/// Links into this repo written as GitHub blob URLs (the published frontier
/// part links to decisions and specs, which live outside the book) are
/// mapped back to repo-relative paths and validated like relative links.
const REPO_BLOB_PREFIX: &str = "https://github.com/hornvale/hornvale/blob/main/";

/// Validate every link in `file`, pushing a message per broken one into
/// `errors`. A link resolves if its path exists (relative to the file's
/// directory; an empty path means the file itself; a `REPO_BLOB_PREFIX` URL
/// is relative to the repo root), and, when it carries a `#fragment` into a
/// Markdown file, if that fragment names a real heading.
fn check_links(file: &Path, errors: &mut Vec<String>) {
    let content = read(file);
    let dir = file.parent().expect("doc file should have a parent");
    for url in link_urls(&content) {
        let (rel, base) = if let Some(repo_rel) = url.strip_prefix(REPO_BLOB_PREFIX) {
            (repo_rel.to_string(), repo_root())
        } else if url.starts_with("http://") || url.starts_with("https://") {
            continue;
        } else {
            (url.clone(), dir.to_path_buf())
        };
        let (path_part, fragment) = match rel.split_once('#') {
            Some((p, f)) => (p, Some(f.to_string())),
            None => (rel.as_str(), None),
        };
        let target = if path_part.is_empty() {
            file.to_path_buf()
        } else {
            base.join(path_part)
        };
        if !target.exists() {
            errors.push(format!(
                "{}: link `{url}` points at a missing path",
                file.display()
            ));
            continue;
        }
        if let Some(fragment) = fragment
            && target.extension().and_then(|e| e.to_str()) == Some("md")
            && !anchors(&read(&target)).contains(&fragment)
        {
            errors.push(format!(
                "{}: link `{url}` names an anchor that no heading produces",
                file.display()
            ));
        }
    }
}
```

**(b)** Swap the four hard-coded paths:
- in `every_frontier_section_is_listed_in_the_contents`: `"docs/vision/frontier.md"` → `"book/src/frontier/frontier.md"`
- in `registry_ids_are_unique`: `"docs/vision/idea-registry.md"` → `"book/src/frontier/idea-registry.md"`
- in `registry_id_prefixes`: `"docs/vision/idea-registry.md"` → `"book/src/frontier/idea-registry.md"`
- in `all_knowledge_doc_links_resolve`, the checked set becomes:

```rust
    for rel in [
        "docs/README.md",
        "book/src/frontier/frontier.md",
        "book/src/frontier/idea-registry.md",
    ] {
```

(The redirect stubs at the old paths are deliberately NOT in this set.)

**(c)** Rescope the registry-ID ban. Replace the doc comment of `the_book_carries_no_registry_ids_or_process_vocabulary` (keep its second paragraph about the deliberately small banned-terms set) and the ID-scan portion of its loop:

New first paragraph of the doc comment:

```rust
/// The merged-reality parts of the book must not cite the idea registry:
/// The Frontier part is the registry's published home (decision
/// `the-frontier-is-published-in-the-book`), and every other part describes
/// merged reality — the recurring failure where a chronicle or domain
/// chapter cited a registry ID (`EXP-3`) or leaked engineering-process
/// vocabulary was caught by review twice before this check existed.
```

In the loop body, the ID scan gains a frontier-part exemption:

```rust
        let rel = path.strip_prefix(&root).unwrap_or(path);
        // The Frontier part IS the registry; the ban guards everything else.
        let in_frontier_part = rel.starts_with("book/src/frontier");
        if !in_frontier_part && let Some(id) = find_registry_id(&text, &prefixes) {
            errors.push(format!(
                "{}: registry ID `{id}` — only The Frontier part may cite the registry",
                rel.display()
            ));
        }
```

The `in_world_prose` process-vocabulary check below it is untouched. The final assert message becomes:

```rust
    assert!(
        errors.is_empty(),
        "a merged-reality part of the book cited the idea registry or leaked \
         engineering-process vocabulary (only the marked Frontier part is \
         speculative — docs/CLAUDE.md):\n  {}",
        errors.join("\n  ")
    );
```

**(d)** Replace the module doc comment's first sentence (lines 1–3) so it reads:

```rust
//! Drift checks binding the knowledge architecture together: the idea
//! registry and frontier essays (published as the book's Frontier part),
//! and the docs map must not silently diverge. Enforces that every
//! cross-link resolves, every frontier section is indexed in the Contents
//! ToC, and registry IDs stay unique. The architecture these checks defend
```

Keep the remainder of the comment (the `docs/README.md` / `docs/CLAUDE.md` sentence and the `architecture.rs` comparison) unchanged.

- [ ] **Step 2: Run the drift-check — expect RED**

```bash
cargo test -p hornvale --test docs_consistency
```

Expected: FAIL — panics reading `book/src/frontier/frontier.md` ("No such file or directory").

- [ ] **Step 3: Move the files and rewrite their outbound links**

```bash
mkdir -p book/src/frontier
git mv docs/vision/frontier.md book/src/frontier/frontier.md
git mv docs/vision/idea-registry.md book/src/frontier/idea-registry.md
sed -i '' 's|](\.\./|](https://github.com/hornvale/hornvale/blob/main/docs/|g' \
  book/src/frontier/frontier.md book/src/frontier/idea-registry.md
```

Every `](../…)` link in both files points into `docs/` (decisions, specs, `../README.md`), so this one rewrite covers all of them. Sibling links (`frontier.md#…`, `idea-registry.md`) and in-page `#anchor` links are untouched. (The registry's one directory link, `../decisions/`, becomes a `blob/main/docs/decisions/` URL — GitHub redirects blob→tree for directories, and the local check validates the directory exists.)

- [ ] **Step 4: Reword the two preambles**

In `book/src/frontier/frontier.md`, replace (original lines 8–11; this passage contains no links, so Step 3's sed left it untouched):

```
it earns a proper spec in `docs/superpowers/specs/`, and that spec — never
this map — is what binds. Where this document and a spec disagree, the spec
wins by definition. The public book (`book/`) describes only merged reality;
this file is the opposite end of the pipe, and stays out of the book.
```

with:

```
it earns a proper spec in `docs/superpowers/specs/`, and that spec — never
this map — is what binds. Where this document and a spec disagree, the spec
wins by definition. This chapter and the [idea registry](idea-registry.md)
form The Frontier — the book's one clearly-marked speculative part. Every
other part of the book describes merged reality; nothing in this part
governs any of it.
```

In `book/src/frontier/idea-registry.md`, at the end of the opening paragraph, after the sentence "Then read the linked essay to understand it.", append (same paragraph):

```
Like the essays it indexes, this chapter is part of The Frontier — the
book's clearly-marked speculative part; it records ambitions and verdicts,
not merged reality, and governs nothing.
```

- [ ] **Step 5: Add the part to `book/src/SUMMARY.md`**

Append at the end of the file (after the Open Questions part):

```markdown

# The Frontier

- [The Frontier](./frontier/frontier.md)
- [The Idea Registry](./frontier/idea-registry.md)
```

- [ ] **Step 6: Create the redirect stubs**

Create `docs/vision/frontier.md`:

```markdown
# The frontier has moved

The frontier essays are now a published part of the project book:
[`book/src/frontier/frontier.md`](../../book/src/frontier/frontier.md),
rendered at <https://hornvale.github.io/hornvale/frontier/frontier.html>.

This stub keeps historical links resolving — ratified decisions and frozen
plans/specs are never edited. See decision
[`the-frontier-is-published-in-the-book`](../decisions/the-frontier-is-published-in-the-book.md).
```

Create `docs/vision/idea-registry.md`:

```markdown
# The idea registry has moved

The idea registry is now a published part of the project book:
[`book/src/frontier/idea-registry.md`](../../book/src/frontier/idea-registry.md),
rendered at <https://hornvale.github.io/hornvale/frontier/idea-registry.html>.

The registry-first habit is unchanged: grep the new path before proposing
or reopening any idea. This stub keeps historical links resolving — ratified
decisions and frozen plans/specs are never edited. See decision
[`the-frontier-is-published-in-the-book`](../decisions/the-frontier-is-published-in-the-book.md).
```

(These stubs link to a decision record created in Task 2; the stubs are outside the drift-check's link-checked set, so this ordering is safe, and Task 2 lands before anything is pushed.)

- [ ] **Step 7: Run the drift-check — expect GREEN**

```bash
cargo test -p hornvale --test docs_consistency
```

Expected: PASS (5 tests).

- [ ] **Step 8: Prove the GitHub-URL mapping bites (negative check, then revert)**

```bash
sed -i '' 's|docs/decisions/0009-models-author-dice-roll.md|docs/decisions/0009-models-author-dice-rollz.md|' book/src/frontier/idea-registry.md
cargo test -p hornvale --test docs_consistency
```

Expected: FAIL — `all_knowledge_doc_links_resolve` names the `https://github.com/hornvale/hornvale/blob/main/docs/decisions/0009-models-author-dice-rollz.md` link as a missing path. Then revert and re-run:

```bash
sed -i '' 's|docs/decisions/0009-models-author-dice-rollz.md|docs/decisions/0009-models-author-dice-roll.md|' book/src/frontier/idea-registry.md
cargo test -p hornvale --test docs_consistency
```

Expected: PASS.

- [ ] **Step 9: Build the book**

```bash
mdbook build book
```

Expected: exit 0; `book/book/frontier/frontier.html` and `book/book/frontier/idea-registry.html` exist.

- [ ] **Step 10: The full gate**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
```

Expected: all green. (Run `cargo fmt` first if the test-file edits need it.)

- [ ] **Step 11: Commit**

```bash
git add -A cli/tests/docs_consistency.rs book/src docs/vision
git commit -m "docs(book): the frontier and idea registry move into the book

The book gains a final, clearly-marked speculative part — The Frontier —
holding the frontier essays and the idea registry, per the 2026-07-10
frontier-in-the-book spec. Outbound links to decisions/specs become GitHub
blob URLs, which the drift-check now maps back to local paths and
validates; the no-registry-IDs-in-the-book ban is rescoped to exempt the
Frontier part while still guarding every merged-reality chapter. Redirect
stubs hold the old docs/vision/ paths so append-only decisions and frozen
plans keep resolving.

Claude-Session: https://claude.ai/code/session_01AVcNDBVsLpjnS19xdreZei"
```

---

### Task 2: Repoint the live docs, ratify the decision (one commit)

**Files:**
- Create: `docs/decisions/the-frontier-is-published-in-the-book.md`
- Create: `book/src/frontier/CLAUDE.md`
- Modify: `docs/decisions/README.md` (append one index row)
- Modify: `docs/README.md`, `docs/CLAUDE.md`, `CLAUDE.md` (repo root), `book/src/introduction.md`

**Interfaces:**
- Consumes: the chapters at `book/src/frontier/` from Task 1.
- Produces: the decision slug `the-frontier-is-published-in-the-book` (already linked by Task 1's stubs).

- [ ] **Step 1: Write the decision record**

Create `docs/decisions/the-frontier-is-published-in-the-book.md`:

```markdown
# The frontier is published in the book

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the frontier essays and the idea registry being
effectively unreadable as raw markdown (the registry's idea cells run to
hundreds of words; only a rendered table can be scanned, and the registry
exists to be scanned), facing the documented convention that the
speculative docs stay out of the book ("the book is merged reality only"),
we decided to publish both files as a clearly-marked final part of the book
— **The Frontier** — making the merged-reality contract per-part rather
than per-book, accepting that the published book now carries speculative
material a reader must not mistake for built reality.

**Context.** The book is the project's one rendered reading surface. The
old boundary predates the registry's growth into essay-length rows; the
readability failure is the new information that reopens it.

**Consequence.** The contract is now: every part of the book except The
Frontier describes merged reality; The Frontier is speculative and governs
nothing — specs still bind over it. The drift-check's registry-ID ban
exempts `book/src/frontier/` and still guards every other chapter. Old
`docs/vision/` paths hold redirect stubs so append-only decisions and
frozen plans resolve forever. Deliberately unchanged: REJ-3 (the essays
stay one file), the registry-first discipline, the drain-to-specs
lifecycle, and decisions/specs staying out of the book.

**See also.** The frontier-in-the-book spec
(`../superpowers/specs/2026-07-10-frontier-in-the-book-design.md`);
`docs/CLAUDE.md` §Boundaries; `cli/tests/docs_consistency.rs`.
```

Append to the index table in `docs/decisions/README.md`:

```markdown
| [`the-frontier-is-published…`](the-frontier-is-published-in-the-book.md) | The frontier is published in the book | Accepted |
```

- [ ] **Step 2: Create `book/src/frontier/CLAUDE.md` (the editing rules follow the files)**

CLAUDE.md files load by directory, so the registry/frontier editing discipline must live beside the files it governs or sessions editing them will never see it. Create `book/src/frontier/CLAUDE.md`:

```markdown
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
```

- [ ] **Step 3: Repoint `docs/CLAUDE.md`**

Three edits:

**(a)** The registry-first habit paragraph — replace:

```markdown
Before you propose a new idea, reopen an old one, or argue an architectural
or process question, **grep [`vision/idea-registry.md`](vision/idea-registry.md)**.
```

with:

```markdown
Before you propose a new idea, reopen an old one, or argue an architectural
or process question, **grep
[`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md)**.
```

**(b)** In the same section, replace:

```markdown
- A row marked `elaborated` already has an essay in
  [`vision/frontier.md`](vision/frontier.md); extend that, don't restart it.
```

with:

```markdown
- A row marked `elaborated` already has an essay in
  [`book/src/frontier/frontier.md`](../book/src/frontier/frontier.md);
  extend that, don't restart it.
```

**(c)** Replace the entire section `## The registry and the frontier are two halves of one thing` (its rules moved to `book/src/frontier/CLAUDE.md` in Step 2) with a pointer:

```markdown
## The registry and the frontier moved into the book

The frontier essays and the idea registry live at `book/src/frontier/` —
the book's one clearly-marked speculative part (decision
`the-frontier-is-published-in-the-book`). Their editing discipline (IDs are
permanent, ToC completeness, status flips, link conventions) lives beside
them in [`book/src/frontier/CLAUDE.md`](../book/src/frontier/CLAUDE.md).
The old `docs/vision/` paths hold redirect stubs so historical links
resolve; do not add new content under `docs/vision/`.
```

**(d)** In `## Boundaries`, replace:

```markdown
- **`docs/` is the private end of the pipe; `book/` is the public end.** The
  frontier and registry deliberately stay out of the book — the book is
  merged reality only. Do not surface speculative material into `book/`.
```

with:

```markdown
- **The book's merged-reality contract is per-part.** The frontier and
  registry are published as the book's clearly-marked Frontier part
  (decision `the-frontier-is-published-in-the-book`); every other part of
  the book describes merged reality — do not surface speculative material
  anywhere else in `book/`, and do not cite registry IDs outside
  `book/src/frontier/` (the drift-check enforces this). Specs, plans, and
  decisions stay in `docs/`.
```

- [ ] **Step 4: Repoint `docs/README.md`**

Five edits:

**(a)** After the pipe diagram's closing ``` fence (before "The registry entry never dies"), add:

```markdown
(The registry and frontier live in the book — `book/src/frontier/`, the
published Frontier part — since decision
`the-frontier-is-published-in-the-book`; the pipeline itself is unchanged.)
```

**(b)** Table row — replace:

```markdown
| **Is this idea already thought of? decided against?** | [`vision/idea-registry.md`](vision/idea-registry.md) | The retrieval surface. Check here *first*, always. |
| **The argument behind a speculative idea** | [`vision/frontier.md`](vision/frontier.md) | Non-binding essays, confidence-tagged. Governs nothing. |
```

with:

```markdown
| **Is this idea already thought of? decided against?** | [`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md) | The retrieval surface. Check here *first*, always. |
| **The argument behind a speculative idea** | [`book/src/frontier/frontier.md`](../book/src/frontier/frontier.md) | Non-binding essays, confidence-tagged. Governs nothing. |
```

**(c)** Table row — replace:

```markdown
| **The wider, non-binding vision** | [`vision/`](vision/) | The frontier and its registry live here. |
```

with:

```markdown
| **The wider, non-binding vision** | [`../book/src/frontier/`](../book/src/frontier/) | The Frontier part of the book; `vision/` holds redirect stubs only. |
```

**(d)** Conventions bullet — replace:

```markdown
- **Registry IDs** are permanent category-prefixed handles; status flips,
  IDs do not. See [`vision/idea-registry.md`](vision/idea-registry.md).
```

with:

```markdown
- **Registry IDs** are permanent category-prefixed handles; status flips,
  IDs do not. See
  [`book/src/frontier/idea-registry.md`](../book/src/frontier/idea-registry.md).
```

**(e)** Conventions bullet — replace:

```markdown
- **`docs/` is the private end of the pipe; `book/` is the public end.** The
  frontier and registry deliberately stay out of the book — the book is
  merged reality only.
```

with:

```markdown
- **The book's merged-reality contract is per-part.** The frontier and
  registry are published as the book's marked Frontier part (decision
  `the-frontier-is-published-in-the-book`); every other part of the book is
  merged reality. Specs, plans, and decisions stay in `docs/`.
```

- [ ] **Step 5: Repoint the root `CLAUDE.md`**

Two edits:

**(a)** Replace:

```markdown
  drift-checked. See `docs/vision/frontier.md` for the wider (non-binding)
  vision map.
```

with:

```markdown
  drift-checked. See `book/src/frontier/frontier.md` (the book's Frontier
  part) for the wider (non-binding) vision map.
```

**(b)** Replace:

```markdown
  directions, `docs/vision/idea-registry.md` is the scannable index (check it
  before proposing or reopening any idea; a `rejected`/`ratified` row is a
  closed question), and `docs/vision/frontier.md` holds the essays behind it.
```

with:

```markdown
  directions, `book/src/frontier/idea-registry.md` is the scannable index
  (check it before proposing or reopening any idea; a `rejected`/`ratified`
  row is a closed question), and `book/src/frontier/frontier.md` holds the
  essays behind it — both published as the book's marked Frontier part.
```

- [ ] **Step 6: Mark the exception in `book/src/introduction.md`**

After the first paragraph (which ends "…and what remains open."), insert a new paragraph:

```markdown
One part is explicitly exempt from that claim: the closing part, **The
Frontier**, publishes the project's speculative vision map — ambitions and
their confidence tags, not the system as built. It is marked as such
throughout, and it governs nothing.
```

- [ ] **Step 7: Verify and commit**

```bash
cargo test -p hornvale --test docs_consistency
mdbook build book
```

Expected: both green (`docs/README.md`'s rewritten links now resolve into `book/src/frontier/`; the drift-check checks them).

```bash
git add docs/decisions/the-frontier-is-published-in-the-book.md docs/decisions/README.md \
  book/src/frontier/CLAUDE.md docs/CLAUDE.md docs/README.md CLAUDE.md book/src/introduction.md
git commit -m "docs: ratify the frontier's publication; repoint the live docs

Decision the-frontier-is-published-in-the-book records the boundary change
(per-part merged-reality contract) and what it deliberately leaves intact
(REJ-3, registry-first, drain-to-specs). The registry/frontier editing
rules move beside the files (book/src/frontier/CLAUDE.md); docs/CLAUDE.md,
docs/README.md, the root CLAUDE.md, and the book introduction are repointed
to the new home.

Claude-Session: https://claude.ai/code/session_01AVcNDBVsLpjnS19xdreZei"
```

---

### Task 3: Full-gate verification and rendered inspection (no new commit expected)

**Files:**
- None modified. Verification only.

**Interfaces:**
- Consumes: everything from Tasks 1–2.

- [ ] **Step 1: The full gate**

```bash
cargo test --workspace
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: all green.

- [ ] **Step 2: Artifact freshness is untouched**

```bash
git status --porcelain book/src/gallery book/src/reference book/src/laboratory
```

Expected: empty output (this migration regenerates nothing; note `book/src/open-questions.md` was already locally modified before this work began — leave it as found).

- [ ] **Step 3: Rendered inspection**

```bash
mdbook build book
ls book/book/frontier/
grep -c "MAP-19" book/book/frontier/idea-registry.html
grep -o 'href="https://github.com/hornvale/hornvale/blob/main/docs/decisions/0009-models-author-dice-roll.md"' book/book/frontier/idea-registry.html | head -1
```

Expected: both `.html` files present; the MAP-19 grep ≥ 1 (the registry table rendered); the href grep prints the URL (outbound links point at GitHub). Then open `book/book/frontier/idea-registry.html` in a browser and confirm the table is readable — this is the motivating deliverable.

- [ ] **Step 4: Report**

Summarize for the user: what moved, the new contract, the decision slug, and that pushing (which publishes the book) is their call.

---

## Deliberately out of scope (from the spec)

- Reshaping the registry table's rendered form.
- Publishing `docs/decisions/` or specs in the book.
- Any chronicle entry — this is documentation housekeeping, not a campaign (no code behavior changed; the book chapters that moved ARE the deliverable).
- Any content change to registry rows or frontier essays beyond the preamble sentences and link rewrites above.
