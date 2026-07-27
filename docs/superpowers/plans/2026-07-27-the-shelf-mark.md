# The Shelf-Mark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `book/src/frontier/idea-registry.md` an index again — five
default-deny form checks in `cli/tests/docs_consistency.rs`, a guidance rewrite
that names where displaced prose goes, and a first compaction wave.

**Architecture:** All five checks share one row parser added to the existing
`docs_consistency.rs` (which today duplicates row detection across two
functions). The length cap lands as a ratchet: a committed waiver fixture holds
the IDs over cap on the day it ships, the check is default-deny for everything
else, and the fixture is append-never so it can only shrink.

**Tech Stack:** Rust (std only, edition 2024), `cargo nextest`, mdbook.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-07-27-the-shelf-mark-design.md`. When
  plan and spec disagree, the spec governs.
- **No new dependencies** (decision 0004). std only; `serde`/`serde_json` are
  the workspace's entire allowlist and this plan needs neither.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy
  `disallowed-types` enforces workspace-wide).
- **Never delete a registry row**, and never rename or renumber an existing ID
  (`book/src/frontier/CLAUDE.md:23`; decision `0026-slugs-not-numbers`).
- **Registry IDs may appear only under `book/src/frontier/`** — enforced by
  `the_book_carries_no_registry_ids_or_process_vocabulary`. Chronicle prose
  names the concept, never the ID.
- **The Idea-cell cap is 600 characters.** Applies to the Idea cell only,
  never the whole row.
- **The closed status vocabulary is exactly:** `raw`, `elaborated`, `spec'd`,
  `shipped`, `ratified`, `rejected`. `ratified` may carry a parenthetical
  (`ratified (0009)`); any status may carry a trailing `→ <status>` transition
  and surrounding `**` emphasis.
- `cargo fmt` is the final step before every commit.
- Run `cargo test -p hornvale --test docs_consistency` after each task; the
  full `make gate` before the close.

---

## File Structure

| File | Responsibility |
|---|---|
| `cli/tests/docs_consistency.rs` | All five checks + the shared row parser. Modified by Tasks 1–4. |
| `cli/tests/fixtures/registry-numbered-ids.txt` | The frozen numbered-ID era. Created Task 3; never changes again. |
| `cli/tests/fixtures/registry-length-waivers.txt` | The append-never grandfather list. Created Task 4, shrunk Task 6. |
| `book/src/frontier/idea-registry.md` | 3 structural fixes (T1), 4 status fixes (T2), preamble (T5), compaction wave (T6), capture rows (T7). |
| `book/src/frontier/CLAUDE.md` | Authoring guidance — the rules that produce compliant rows. Task 5. |
| `book/src/chronicle/the-shelf-mark.md` | Chronicle entry. Task 8. |
| `docs/retrospectives/the-shelf-mark.md` | Campaign retrospective. Task 8. |

---

### Task 1: The shared row parser and the column-count check

Three rows in the registry contain an unescaped `|` inside an inline code
span. GFM splits the cell on it anyway, mdbook truncates the row to the
header's five columns, and **the Where cell is silently dropped from the
published page**. This task adds the parser every later check reuses, the
column check, and fixes the three rows.

**Files:**
- Modify: `cli/tests/docs_consistency.rs` (add parser + test; refactor
  `registry_ids_are_unique:169` and `registry_id_prefixes:245` to use it)
- Modify: `book/src/frontier/idea-registry.md` (3 rows)

**Interfaces:**
- Consumes: nothing.
- Produces: `struct RegistryRow { line: usize, id: String, idea: String,
  status: String, where_cell: String, cells: usize }` and
  `fn registry_rows() -> Vec<RegistryRow>`. Every later task parses rows
  through these and adds no parsing of its own.

- [ ] **Step 1: Write the failing test**

Add to `cli/tests/docs_consistency.rs`:

```rust
/// One parsed row of the idea registry's tables. `cells` counts the pieces
/// the line splits into on unescaped pipes — a well-formed five-column row
/// splits into seven (an empty piece before the leading `|` and after the
/// trailing one).
struct RegistryRow {
    /// 1-based line number in `idea-registry.md`, for error messages.
    line: usize,
    /// The ID cell (`MAP-7`, `SKY-eclipse-seasons`).
    id: String,
    /// The Idea cell — the prose the length cap applies to.
    idea: String,
    /// The Status cell.
    status: String,
    /// The Where cell.
    where_cell: String,
    /// Pieces the line splits into on *unescaped* pipes; 7 when well-formed.
    cells: usize,
}

/// A sentinel standing in for `\|` while splitting, so an escaped pipe (which
/// GFM renders as a literal `|` inside a cell) never counts as a separator.
/// Restored before any cell is returned, so lengths and text are faithful.
const ESCAPED_PIPE: char = '\u{1}';

/// True when `cell` is a registry ID: a category prefix, a hyphen, and either
/// a number with an optional sub-letter (`MAP-9`, `MAP-9a` — the frozen
/// numbered era) or a lowercase slug (`SKY-eclipse-seasons`, decision
/// `0026-slugs-not-numbers`).
fn looks_like_registry_id(cell: &str) -> bool {
    cell.split_once('-').is_some_and(|(pre, post)| {
        let numbered = post.starts_with(|c: char| c.is_ascii_digit())
            && post
                .trim_end_matches(|c: char| c.is_ascii_lowercase())
                .chars()
                .all(|c| c.is_ascii_digit());
        let slug = post.starts_with(|c: char| c.is_ascii_lowercase())
            && post
                .chars()
                .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
        !pre.is_empty()
            && pre.chars().all(|c| c.is_ascii_uppercase())
            && !post.is_empty()
            && (numbered || slug)
    })
}

/// Parse `text` as the idea registry, returning one entry per ID-bearing
/// table row. Header and separator rows are skipped.
fn parse_registry(text: &str) -> Vec<RegistryRow> {
    let mut rows = Vec::new();
    for (idx, line) in text.lines().enumerate() {
        if !line.starts_with("| ") {
            continue;
        }
        let masked = line.replace("\\|", &ESCAPED_PIPE.to_string());
        let pieces: Vec<String> = masked
            .split('|')
            .map(|p| p.replace(ESCAPED_PIPE, "\\|").trim().to_string())
            .collect();
        // pieces[0] is the empty text before the leading `|`.
        let Some(id) = pieces.get(1) else { continue };
        if !looks_like_registry_id(id) {
            continue;
        }
        let at = |i: usize| pieces.get(i).cloned().unwrap_or_default();
        rows.push(RegistryRow {
            line: idx + 1,
            id: id.clone(),
            idea: at(2),
            status: at(3),
            where_cell: at(5),
            cells: pieces.len(),
        });
    }
    rows
}

/// Every ID-bearing row of the idea registry.
fn registry_rows() -> Vec<RegistryRow> {
    parse_registry(&read(
        &repo_root().join("book/src/frontier/idea-registry.md"),
    ))
}

#[test]
fn an_escaped_pipe_is_not_a_column_separator() {
    // The trap this check exists to avoid: a naive split on '|' counts the
    // escaped pipes inside the code span as separators and reports a
    // well-formed row as broken. Both rows below are five-column rows; only
    // the second is malformed.
    let ok = "| MAP-1 | uses `a \\| b` in prose | raw | med | [x](y.md) |";
    let broken = "| MAP-2 | uses `a | b` unescaped | raw | med | [x](y.md) |";
    let rows = parse_registry(&format!("{ok}\n{broken}\n"));
    assert_eq!(rows.len(), 2, "both rows should parse as registry rows");
    assert_eq!(rows[0].cells, 7, "escaped pipes must not split the cell");
    assert_eq!(
        rows[0].idea, "uses `a \\| b` in prose",
        "the escape must survive parsing intact"
    );
    assert_eq!(rows[1].cells, 8, "a bare pipe must split the cell");
}

#[test]
fn registry_rows_have_five_columns() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.cells != 7)
        .map(|r| {
            format!(
                "{}:{} ({} columns, expected 5) — escape bare `|` in prose as `\\|`",
                r.id,
                r.line,
                r.cells - 2
            )
        })
        .collect();
    assert!(
        offenders.is_empty(),
        "malformed registry rows — mdbook truncates these to five cells, \
         shifting the columns left and DROPPING the Where pointer from the \
         published page:\n  {}",
        offenders.join("\n  ")
    );
}
```

- [ ] **Step 2: Run the tests to verify the unit test passes and the registry check fails**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`

Expected: `an_escaped_pipe_is_not_a_column_separator` PASSES (it tests the
parser, which this step wrote). `registry_rows_have_five_columns` FAILS
listing exactly three offenders: `ALCH-1`, `SKY-circumpolar`, `PROC-16`.

If any row other than those three appears, the escaped-pipe handling is
wrong — do not "fix" the extra row; fix the parser.

- [ ] **Step 3: Fix the three malformed rows**

For each of `ALCH-1`, `SKY-circumpolar`, `PROC-16`, find the bare `|` inside an
inline code span and escape it as `\|`. Locate them with:

```bash
grep -n '^| \(ALCH-1\|SKY-circumpolar\|PROC-16\) ' book/src/frontier/idea-registry.md
```

`PROC-16`'s is `` `Correspondent | Void(reason)` `` → `` `Correspondent \| Void(reason)` ``.
Change only the escaping. Do not reword, shorten, or restructure these rows —
compaction is Task 7, and mixing the two makes this fix unreviewable.

- [ ] **Step 4: Verify the fix in the rendered HTML, not the source**

The source looking right is not the acceptance criterion; the published page
having its Where cell back is.

```bash
mdbook build book
cd book/book/frontier
for id in PROC-16 ALCH-1 SKY-circumpolar; do
  echo -n "$id last cell: "
  grep -o "<tr><td>$id</td>.*\?</tr>" idea-registry.html | head -1 \
    | sed 's/<\/td>/\n/g' | tail -2 | head -1 | cut -c1-80
done
```

Expected: each prints a **link** (the Where cell — `<a href=...`), not
`med (workflow)` or another Conf value. Before the fix `PROC-16` printed
`med (workflow)`, because the Where cell had been dropped.

- [ ] **Step 5: Refactor the two existing duplicate parsers**

`registry_ids_are_unique:169` and `registry_id_prefixes:245` each inline their
own copy of the ID detection. Rewrite both to use `registry_rows()` and
`looks_like_registry_id`, deleting the duplicated logic. `registry_id_prefixes`
keeps deriving prefixes *from the file* — that behaviour is deliberate
(category vocabulary is open) and must not become a hard-coded list.

```rust
#[test]
fn registry_ids_are_unique() {
    let mut seen = BTreeSet::new();
    let mut dupes = Vec::new();
    for row in registry_rows() {
        if !seen.insert(row.id.clone()) {
            dupes.push(format!("{}:{}", row.id, row.line));
        }
    }
    assert!(
        dupes.is_empty(),
        "duplicate registry IDs (IDs are permanent and unique):\n  {}",
        dupes.join("\n  ")
    );
}

fn registry_id_prefixes() -> BTreeSet<String> {
    registry_rows()
        .iter()
        .filter_map(|r| r.id.split_once('-'))
        .filter(|(_, post)| post.starts_with(|c: char| c.is_ascii_digit()))
        .map(|(pre, _)| pre.to_string())
        .collect()
}
```

- [ ] **Step 6: Run the full docs_consistency suite**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 10 passed, 0 failed. The refactor must not change the behaviour of
the 8 pre-existing tests.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add cli/tests/docs_consistency.rs book/src/frontier/idea-registry.md
git commit -m "fix(frontier): restore the Where pointer to three registry rows

An unescaped | inside an inline code span splits the cell in GFM. mdbook
truncates the row to the header's five columns, so the columns shift left
and the Where cell is dropped from the published page entirely — verified
in the rendered HTML, not the source.

Adds the shared row parser the remaining form checks build on, and the
column-count check that would have caught this. The escaped-pipe case gets
its own unit test: a naive split reports five offenders, two of them rows
that escape correctly."
```

---

### Task 2: The status vocabulary check

`idea-registry.md:26-34` defines a closed six-token status vocabulary and
nothing enforces it. Four rows have drifted.

**Files:**
- Modify: `cli/tests/docs_consistency.rs`
- Modify: `book/src/frontier/idea-registry.md` (4 rows)

**Interfaces:**
- Consumes: `registry_rows()`, `RegistryRow::status` from Task 1.
- Produces: `fn normalize_status(&str) -> String`.

- [ ] **Step 1: Write the failing test**

```rust
/// The closed status vocabulary, per `idea-registry.md`'s "How to read a row".
/// Unlike the category prefixes — which `registry_id_prefixes` derives from
/// the file so a newly coined category adapts automatically — this list is
/// deliberately hard-coded. The category vocabulary is open; the status
/// vocabulary is closed, and leaving it open by omission is what let
/// `registered` and three prose-filled Status cells into the file.
const REGISTRY_STATUSES: [&str; 6] = [
    "raw",
    "elaborated",
    "spec'd",
    "shipped",
    "ratified",
    "rejected",
];

/// Reduce a Status cell to its bare token: strip `**` emphasis, a trailing
/// `→ <status>` transition, and any trailing parenthetical (`ratified (0009)`,
/// `shipped (field half)`).
fn normalize_status(cell: &str) -> String {
    let mut s = cell.replace('*', "");
    if let Some((head, _)) = s.split_once('→') {
        s = head.to_string();
    }
    if let Some((head, _)) = s.split_once('(') {
        s = head.to_string();
    }
    s.trim().to_string()
}

#[test]
fn registry_statuses_use_the_closed_vocabulary() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| !REGISTRY_STATUSES.contains(&normalize_status(&r.status).as_str()))
        .map(|r| {
            format!(
                "{}:{} — status {:?}",
                r.id,
                r.line,
                r.status.chars().take(60).collect::<String>()
            )
        })
        .collect();
    assert!(
        offenders.is_empty(),
        "registry rows whose Status is outside the closed vocabulary {:?}:\n  {}",
        REGISTRY_STATUSES,
        offenders.join("\n  ")
    );
}

#[test]
fn status_normalization_handles_the_documented_forms() {
    assert_eq!(normalize_status("shipped"), "shipped");
    assert_eq!(normalize_status("**shipped**"), "shipped");
    assert_eq!(normalize_status("ratified (0009)"), "ratified");
    assert_eq!(normalize_status("shipped (field half)"), "shipped");
    assert_eq!(normalize_status("rejected → ratified"), "rejected");
    assert_eq!(normalize_status("registered"), "registered"); // not a status
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale --test docs_consistency registry_statuses 2>&1 | tail -20`
Expected: FAIL listing 4 offenders — `LANG-55` (`registered`), and `ALCH-1`,
`SKY-circumpolar`, `PROC-16` if Task 1's fixes have not landed. **If Task 1
landed, only `LANG-55` should remain**; the other three were prose-in-status
caused by the column shift, and fixing the pipes fixed the status too. Confirm
this — it is the cheapest evidence that Task 1's diagnosis was right.

- [ ] **Step 3: Fix the remaining offenders**

`LANG-55`'s `registered` is not in the vocabulary. Read the row and pick the
token that matches its actual state — it describes something built, so
`shipped` unless the row's own prose says otherwise. Do not invent a seventh
status; if no existing token fits, stop and raise it (that is a spec question,
not an implementation one).

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 12 passed, 0 failed.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add cli/tests/docs_consistency.rs book/src/frontier/idea-registry.md
git commit -m "test(frontier): enforce the registry's closed status vocabulary

The registry documents six statuses and nothing checked them. Category
prefixes stay derived-from-the-file (open vocabulary, deliberately); status
becomes default-deny (closed vocabulary). That asymmetry was missing, which
is how 'registered' got in."
```

---

### Task 3: The numbered-ID freeze and the Where tripwire

Decision `0026-slugs-not-numbers` ratified category+slug for new registry rows
and promised a `docs_consistency` freeze check — but scoped it to
decision/chronicle/study *files*, never registry rows. This completes it. The
403 existing numbered IDs are legitimate history and are frozen, not renamed.

**Files:**
- Modify: `cli/tests/docs_consistency.rs`
- Create: `cli/tests/fixtures/registry-numbered-ids.txt`

**Interfaces:**
- Consumes: `registry_rows()` from Task 1.
- Produces: nothing later tasks depend on.

- [ ] **Step 1: Generate the freeze list**

```bash
awk '/^\| [A-Z]+-[0-9]/ {split($0,a,"|"); id=a[2]; gsub(/^ +| +$/,"",id); print id}' \
  book/src/frontier/idea-registry.md | sort -u > cli/tests/fixtures/registry-numbered-ids.txt
wc -l < cli/tests/fixtures/registry-numbered-ids.txt
```

Expected: `403`. If the count differs, stop — the registry changed under the
plan and the number in the spec needs re-deriving before freezing.

- [ ] **Step 2: Write the failing test**

```rust
/// The numbered registry IDs that existed when decision
/// `0026-slugs-not-numbers`'s freeze was finally applied to registry rows.
/// Append-never: an ID may leave this list only by leaving the registry,
/// which never happens (rows are permanent). A *new* numbered ID fails.
fn frozen_numbered_ids() -> BTreeSet<&'static str> {
    include_str!("fixtures/registry-numbered-ids.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect()
}

#[test]
fn no_new_numbered_registry_ids() {
    let frozen = frozen_numbered_ids();
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| {
            r.id.split_once('-')
                .is_some_and(|(_, post)| post.starts_with(|c: char| c.is_ascii_digit()))
        })
        .filter(|r| !frozen.contains(r.id.as_str()))
        .map(|r| format!("{}:{}", r.id, r.line))
        .collect();
    assert!(
        offenders.is_empty(),
        "new numbered registry IDs — decision `0026-slugs-not-numbers` \
         requires category+slug for new rows (`LANG-exonyms`, not `LANG-6`); \
         the numbered era is frozen, not extended:\n  {}",
        offenders.join("\n  ")
    );
}

#[test]
fn every_registry_row_carries_a_pointer() {
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.where_cell.is_empty() || r.where_cell == "—")
        .map(|r| format!("{}:{}", r.id, r.line))
        .collect();
    assert!(
        offenders.is_empty(),
        "registry rows with an empty Where cell — a row is a pointer; \
         without one there is nothing to point at:\n  {}",
        offenders.join("\n  ")
    );
}
```

- [ ] **Step 3: Run to verify both pass, then verify the freeze actually bites**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 14 passed. Both are green on landing — the freeze list was generated
from the current file, and zero rows have an empty Where cell.

A check that passes on arrival proves nothing yet. Verify it fails when it
should:

```bash
printf '| MAP-999 | a new numbered idea | raw | med | [x](frontier.md#x) |\n' \
  >> book/src/frontier/idea-registry.md
cargo test -p hornvale --test docs_consistency no_new_numbered 2>&1 | tail -12
git checkout book/src/frontier/idea-registry.md
```

Expected: FAIL naming `MAP-999`. Then the checkout restores the file — confirm
with `git status --short` that `idea-registry.md` is clean before committing.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add cli/tests/docs_consistency.rs cli/tests/fixtures/registry-numbered-ids.txt
git commit -m "test(frontier): freeze the numbered registry-ID era

Decision 0026 ratified category+slug for new registry rows and promised
this check, but only ever scoped it to decision/chronicle/study files —
docs_consistency has been commenting 'the frozen numbered era' while
accepting both forms forever. The 403 existing numbered IDs freeze as
history; new ones fail. Adds the empty-Where tripwire alongside."
```

---

### Task 4: The Idea-cell length cap

**Files:**
- Modify: `cli/tests/docs_consistency.rs`
- Create: `cli/tests/fixtures/registry-length-waivers.txt`

**Interfaces:**
- Consumes: `registry_rows()` from Task 1.
- Produces: `const REGISTRY_IDEA_CAP: usize = 600;` and the waiver fixture
  Task 7 shrinks.

- [ ] **Step 1: Generate the waiver list**

The cap applies to the **Idea cell**, never the whole row — the Where column
carries mandated full GitHub blob URLs (~100 chars each) and taxing a row for
carrying pointers is backwards.

```bash
awk '{ line=$0; gsub(/\\\|/,"\001",line);
       if (line !~ /^\| [A-Z]+-/) next;
       n=split(line,a,"|"); if (n<7) next;
       id=a[2]; gsub(/^ +| +$/,"",id);
       idea=a[3]; gsub(/^ +| +$/,"",idea);
       if (length(idea) > 600) print id }' \
  book/src/frontier/idea-registry.md | sort -u > cli/tests/fixtures/registry-length-waivers.txt
wc -l < cli/tests/fixtures/registry-length-waivers.txt
```

Expected: `278`. A materially different number means Tasks 1–3 changed row
lengths; re-read before proceeding.

- [ ] **Step 2: Write the failing test**

```rust
/// The Idea-cell budget, in characters. A row is a shelf-mark: what the idea
/// is, and a pointer to where it is argued. The argument lives in
/// `frontier.md`; the campaign narrative lives in the chronicle the Where
/// cell links. Sibling norm: `docs/decisions/README.md` — "keep each record
/// short … if it needs a page, it is probably a spec".
const REGISTRY_IDEA_CAP: usize = 600;

/// Rows over `REGISTRY_IDEA_CAP` on the day the cap landed. **Append-never:**
/// entries may be removed as rows are compacted, never added. A new row over
/// the cap is a failure, not a fixture edit — that ratchet is the whole
/// mechanism (the pattern is the type audit's `pending(wave-N)`, decision
/// 0028).
fn registry_length_waivers() -> BTreeSet<&'static str> {
    include_str!("fixtures/registry-length-waivers.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect()
}

#[test]
fn registry_idea_cells_are_within_budget() {
    let waived = registry_length_waivers();
    let offenders: Vec<String> = registry_rows()
        .iter()
        .filter(|r| r.idea.chars().count() > REGISTRY_IDEA_CAP)
        .filter(|r| !waived.contains(r.id.as_str()))
        .map(|r| format!("{}:{} — {} chars", r.id, r.line, r.idea.chars().count()))
        .collect();
    assert!(
        offenders.is_empty(),
        "registry Idea cells over {REGISTRY_IDEA_CAP} chars. A row is an \
         index entry, not an essay — compact it (the prose is redundant with \
         the chronicle the Where cell links), relocate it (move the argument \
         to a frontier.md section and flip `raw` → `elaborated`), or trim it:\
         \n  {}",
        offenders.join("\n  ")
    );
}

#[test]
fn the_waiver_list_only_shrinks() {
    let waived = registry_length_waivers();
    let ids: BTreeSet<String> = registry_rows().iter().map(|r| r.id.clone()).collect();
    let unknown: Vec<&str> = waived
        .iter()
        .filter(|w| !ids.contains(**w))
        .copied()
        .collect();
    assert!(
        unknown.is_empty(),
        "waived IDs absent from the registry — the waiver list is \
         append-never and rows are permanent, so this means a typo or a \
         renamed ID:\n  {}",
        unknown.join("\n  ")
    );

    let compacted: Vec<&str> = waived
        .iter()
        .filter(|w| {
            registry_rows()
                .iter()
                .find(|r| r.id.as_str() == **w)
                .is_some_and(|r| r.idea.chars().count() <= REGISTRY_IDEA_CAP)
        })
        .copied()
        .collect();
    assert!(
        compacted.is_empty(),
        "these rows are now within budget — remove them from \
         fixtures/registry-length-waivers.txt so the ratchet holds:\n  {}",
        compacted.join("\n  ")
    );
}
```

- [ ] **Step 3: Run to verify both pass**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 16 passed. `the_waiver_list_only_shrinks` is what makes the ratchet
one-way: compact a row without pruning its waiver and the gate reddens.

- [ ] **Step 4: Verify the cap bites on a new row**

```bash
python3 - <<'PY' >> book/src/frontier/idea-registry.md
print("| MAP-overlong | " + "x" * 700 + " | raw | med | [x](frontier.md#x) |")
PY
cargo test -p hornvale --test docs_consistency registry_idea_cells 2>&1 | tail -12
git checkout book/src/frontier/idea-registry.md
```

Expected: FAIL naming `MAP-overlong` at 700 chars. Confirm `git status --short`
shows the registry clean afterward.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add cli/tests/docs_consistency.rs cli/tests/fixtures/registry-length-waivers.txt
git commit -m "test(frontier): cap registry Idea cells at 600 chars, as a ratchet

The registry calls itself 'one line each, scannable and greppable' and is
570 rows / 632 KB. The cap targets the Idea cell, not the whole row: the
Where column carries mandated full blob URLs, and taxing a row for carrying
pointers is backwards.

278 rows over cap ship waived (the type audit's pending(wave-N) pattern);
the list is append-never and a companion test reddens if a compacted row
keeps its waiver, so it can only shrink."
```

---

### Task 5: The authoring guidance

The checks reject bad rows; this task tells an author how to write a good one.
The observed failure mode is that campaigns *append* their result to a row
instead of repointing it, and the current guidance never says to delete the
prose being replaced.

**Files:**
- Modify: `book/src/frontier/CLAUDE.md`
- Modify: `book/src/frontier/idea-registry.md` (the "How to read a row"
  preamble — the copy an agent reads first)

- [ ] **Step 1: Rewrite the `CLAUDE.md` row-lifecycle rules**

In `book/src/frontier/CLAUDE.md`, the bullet at line 22–23 currently reads:

```markdown
- **Idea drains into a spec** → flip the row's status (`spec'd` / `shipped`)
  and repoint **Where** at the spec. Never delete a row.
```

Replace with:

```markdown
- **Idea drains into a spec** → flip the row's status (`spec'd` / `shipped`)
  and repoint **Where** at the spec. Never delete a row.
- **Repointing Where REPLACES the row's prose — it does not append to it.**
  A campaign's narrative belongs in its chronicle, which the row links; the
  row says what the idea *is*, in one line. Appending "**Shipped X**: …" to
  a row on every campaign is what grew the registry to 632 KB.
```

Then add a new section after "The registry and the frontier are two halves of
one thing":

```markdown
## A row is a shelf-mark

The Idea cell is capped at **600 characters**, enforced by
`cli/tests/docs_consistency.rs`. The cap is on the Idea cell only — the Where
column carries full GitHub blob URLs by design and is never counted.

A row over the cap is nearly always *mislabeled* rather than merely long.
`raw` means "a stub, not an argument"; `elaborated` means "has a full essay in
`frontier.md`". A row carrying 900 characters of argument is not `raw`. The
three legal remedies:

1. **Compact** — the prose duplicates a chronicle or spec the Where cell
   already links. Delete it; keep the pointer.
2. **Relocate** — it is real argument. Move it to a `frontier.md` section, add
   the ToC bullet, flip `raw` → `elaborated`, point **Where** at the anchor.
3. **Trim** — it is a stub that got wordy. Cut to one clause.

Rows over the cap when it landed are listed in
`cli/tests/fixtures/registry-length-waivers.txt`. That list is **append-never**:
compacting a row means deleting its line from the file, and a test reddens if
you compact without pruning. Never add to it.

## Row form

- **Five columns**, always: `| ID | Idea | Status | Conf | Where |`.
- **Escape bare pipes in prose as `\|`** — including inside `` `code
  spans` ``, where GFM splits the cell anyway. An unescaped pipe shifts every
  later column left and silently drops the Where cell from the published page.
- **Status is a closed vocabulary**: `raw`, `elaborated`, `spec'd`, `shipped`,
  `ratified (NNNN)`, `rejected`. Category prefixes are open — coin a new one
  freely — but statuses are not. Do not invent one.
- **New IDs are category+slug** (`LANG-exonyms`, not `LANG-6`), per decision
  `0026-slugs-not-numbers`. The numbered era is frozen; existing numbered IDs
  keep their names forever.
- **Where is never empty.** A row with nothing to point at is not a row.
```

- [ ] **Step 2: Update the registry's own preamble**

In `book/src/frontier/idea-registry.md`, extend the "How to read a row"
section's `Status` bullet with the closed-vocabulary note, and add after the
final "When an idea drains into a spec…" paragraph:

```markdown
**Rows are capped at 600 characters of Idea prose** and the cap is enforced in
CI. A row is a shelf-mark: what the idea is, plus a pointer to where it is
argued. If a row needs a page, it needs a `frontier.md` essay and a status of
`elaborated` — not a longer row. Authoring rules live in this directory's
`CLAUDE.md`.
```

- [ ] **Step 3: Verify the docs still pass their own checks**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 16 passed. In particular `all_knowledge_doc_links_resolve` must stay
green — the new prose cites `cli/tests/fixtures/registry-length-waivers.txt`
and `docs/decisions/0026-slugs-not-numbers.md`, both of which must resolve.

- [ ] **Step 4: Commit**

```bash
git add book/src/frontier/CLAUDE.md book/src/frontier/idea-registry.md
git commit -m "docs(frontier): state the row-form rules the checks enforce

Names the destination, not just the limit: repointing Where REPLACES the
row's prose rather than appending to it, and an over-cap row is nearly
always mislabeled (a 900-char 'raw' row is an elaborated idea whose essay
was written in the index). Documents the three legal remedies, the pipe
escaping, the closed status vocabulary, and the slug rule."
```

---

### Task 6: The first compaction wave — the `shipped` rows

87 of the 127 `shipped` rows are over cap (68%, the worst-offending status)
and their prose is the most mechanical to remove: it duplicates chronicles the
Where cell already links.

**Files:**
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `cli/tests/fixtures/registry-length-waivers.txt`

- [ ] **Step 1: List the wave**

```bash
awk '{ line=$0; gsub(/\\\|/,"\001",line);
       if (line !~ /^\| [A-Z]+-/) next;
       n=split(line,a,"|"); if (n<7) next;
       id=a[2]; gsub(/^ +| +$/,"",id);
       idea=a[3]; gsub(/^ +| +$/,"",idea);
       st=a[4]; gsub(/^ +| +$/,"",st); gsub(/\*/,"",st); sub(/ *\(.*/,"",st); sub(/ *→.*/,"",st);
       if (st=="shipped" && length(idea)>600) printf "%-28s %5d\n", id, length(idea) }' \
  book/src/frontier/idea-registry.md | sort -k2 -rn
```

Expected: 87 rows, largest first. Work down the list.

- [ ] **Step 2: Compact each row**

For each, apply remedy 1 (compact) from the guidance. The target shape:

> one clause naming what the idea *is* · what shipped, in one clause · the
> unbuilt remainder if any, in one clause

Rules while compacting:
- **Never delete the row, rename the ID, or change the Status.**
- **Preserve every `[[wiki-link]]`** — they are the registry's cross-index.
- **Preserve every distinct link in Where.** If the Idea prose names a spec or
  chronicle the Where cell does not yet link, move that link into Where rather
  than dropping it.
- **Keep unbuilt remainders.** "Deliberately NOT built: X, Y, Z" is live
  backlog, not history — compress it, do not delete it.
- Delete: campaign narrative, measured numbers already in the chronicle,
  restated mechanisms, and appended "**Shipped in campaign Q**" paragraphs.

- [ ] **Step 3: Prune each compacted ID from the waiver list**

The `the_waiver_list_only_shrinks` test from Task 4 fails until you do. That
is the ratchet working — let it drive the loop rather than batching at the end.

```bash
grep -c . cli/tests/fixtures/registry-length-waivers.txt
```

Expected after the wave: `191` (278 − 87). A different number means some rows
were not brought under cap; the test names them.

- [ ] **Step 4: Verify no cross-links broke**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -20`
Expected: 16 passed. `all_knowledge_doc_links_resolve` catches a link deleted
during compaction; `registry_id_prefixes` derives from the file, so confirm no
category lost its last row:

```bash
awk '/^\| [A-Z]+-/ {split($0,a,"|"); id=a[2]; gsub(/^ +| +$/,"",id); split(id,p,"-"); print p[1]}' \
  book/src/frontier/idea-registry.md | sort -u | wc -l
```

Expected: `22`. Fewer means a category was emptied — impossible without
deleting a row, so investigate before continuing.

- [ ] **Step 5: Confirm the win**

```bash
wc -c book/src/frontier/idea-registry.md
```

Record the before (673243 bytes) and after in the commit message.

- [ ] **Step 6: Build the book and read a sample**

```bash
mdbook build book
```

Then read three compacted rows in `book/book/frontier/idea-registry.html` —
one `MAP`, one `LANG`, one `SKY`. The acceptance question is not "is it
shorter" but **"could a reader who has never seen this idea tell what it is and
where to read more?"** If not, the compaction went too far; restore from
`git diff`.

- [ ] **Step 7: Commit**

```bash
git add book/src/frontier/idea-registry.md cli/tests/fixtures/registry-length-waivers.txt
git commit -m "docs(frontier): compact the shipped rows (wave 1 of the burn-down)

87 shipped rows carried campaign narrative duplicating the chronicles their
Where cells already link. Compacted to one line each; every wiki-link and
every unbuilt remainder preserved, no row deleted, no ID renamed.

Waiver list 278 -> 191."
```

---

### Task 7: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-shelf-mark.md`
- Create: `docs/retrospectives/the-shelf-mark.md`
- Modify: `book/src/SUMMARY.md`
- Modify: `book/src/frontier/idea-registry.md` (rows for the campaign)

- [ ] **Step 1: Run the full gate**

Run: `make gate`
Expected: green, ~4 min. This is the first full-workspace run of the campaign;
everything before was scoped to `docs_consistency`.

- [ ] **Step 2: Absorb main and re-gate**

```bash
make preflight
```

On an ancestry NO-GO, merge main **into** this branch and re-run. The registry
is a high-collision file — a parallel campaign may have added rows, which will
conflict inside the compaction wave. Resolve by keeping **both** rows
(registry rows are permanent) and compacting any newly-arrived row that lands
over cap; then regenerate both fixtures from Step 1 of Tasks 3 and 4 and
confirm the counts.

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-shelf-mark.md`, at the book's usual altitude —
technical, comprehensible without the code. Cover: the index that grew into
the thing it indexed; the three rows silently losing their Where pointer in the
published book (with the mdbook-truncation mechanism, which is the entry's
best material); the open-vs-closed vocabulary asymmetry; the ratchet.

**Name concepts, never registry IDs** — `the_book_carries_no_registry_ids_or_process_vocabulary`
bans `MAP-7`-style IDs outside `book/src/frontier/`, and the chronicle is
outside it. This is a recurring trap; write "the underworld row", not the ID.

Add the entry to `book/src/SUMMARY.md` in chronicle order.

- [ ] **Step 4: Write the retrospective**

`docs/retrospectives/the-shelf-mark.md` — process lessons, not product
(decision 0020). At minimum:
- The naive `awk -F'|'` split reported 5 column violations, 2 of them false
  positives, because awk does not understand `\|`. The check's own unit test
  now pins that case. *A measurement tool that does not model the escaping of
  the thing it measures will overcount.*
- The first-drafted defect ("renders with extra columns") was wrong; building
  the book and counting `<td>` showed mdbook truncates and drops the Where
  cell. *Verified in the rendered artifact, not the source.*
- The cap target changed from whole-row to Idea-cell only after measuring both.

- [ ] **Step 5: Add the campaign's registry rows**

Capture what this campaign raised, per the capture discipline. New rows take
**category+slug** IDs (the freeze from Task 3 enforces it) and must be under
600 chars:

- A `PROC-` row for the raw-backlog triage question (spec §8.3) — 322 `raw`
  rows whose liveness no test can judge.
- A `PROC-` row for the burn-down remainder — 191 waived rows across
  `raw`/`elaborated`, where the work is judgment (relocate vs trim) rather
  than deletion.

- [ ] **Step 6: Final gate and book build**

```bash
make gate && mdbook build book
```
Expected: both green.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add book/src/chronicle/the-shelf-mark.md docs/retrospectives/the-shelf-mark.md \
        book/src/SUMMARY.md book/src/frontier/idea-registry.md
git commit -m "docs(the-shelf-mark): close — chronicle, retrospective, capture rows"
```

---

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| §1a prose bloat / §4a cap target | 4 |
| §1b status drift / §4c status check | 2 |
| §1c column breaks | 1 |
| §3 keystone (three remedies) | 5 (guidance), 6 (applied) |
| §4b ratchet + grandfather list | 4 |
| §4c numbered-ID freeze | 3 |
| §4c Where non-empty | 3 |
| §4d guidance rewrite | 5 |
| §5 blast radius (prefix derivation, ID ban) | 6 Step 4, 7 Step 3 |
| §6 acceptance 1–6 | 1–7 |
| §7 first wave = shipped rows | 6 |
| §8.3 raw triage captured, not answered | 7 Step 5 |

No gaps.

**Type consistency:** `registry_rows()`, `RegistryRow`, `looks_like_registry_id`,
`normalize_status`, `REGISTRY_IDEA_CAP`, `registry_length_waivers()`,
`frozen_numbered_ids()` — each defined once (Task 1, 2, 3, 4) and referenced
under the same name thereafter. `RegistryRow.where_cell` avoids the `where`
keyword. Field `cells` counts split pieces (7 for a well-formed row), and every
consumer uses that convention.

**Placeholder scan:** no TBD/TODO; every code step carries complete code; every
command carries its expected output. The one deliberately unspecified value is
`LANG-55`'s replacement status (Task 2 Step 3), which requires reading the row
— with an explicit stop-and-raise instruction rather than a guess.
