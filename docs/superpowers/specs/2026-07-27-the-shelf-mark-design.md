# The Shelf-Mark — a registry row is a pointer, not a copy

**Campaign:** The Shelf-Mark
**Date:** 2026-07-27
**Status:** Draft — awaiting G3
**Decisions in force:** `0026-slugs-not-numbers` (registry rows are
category+slug; its registry provisions survive supersession by 0043), 0028
(the `pending(wave-N)` grandfather pattern), 0013 (DoD includes the book),
0031 (the Frontier governs nothing; specs bind over it).
**Occasioned by:** a review of whether completed rows should be deleted from
the idea registry. They should not — see §2 — but the question surfaced that
the registry has stopped being an index.

## 1. The finding

`book/src/frontier/idea-registry.md` opens by describing itself:

> **This is the retrieval surface for Hornvale's speculative ideas — one line
> each, scannable and greppable.**

It is **570 rows and 632 KB**. The mean row is 1109 characters. The largest
single row (`PSY-11`) is 13.8 KB — longer than most chapters of the book it
is indexed inside.

The file has three distinct defects, in increasing order of severity.

### 1a. Rows carry prose that belongs in the essays

The frontier is a deliberate two-file split: `frontier.md` holds the argument
(37 sections, 188 KB), the registry holds one greppable line each. The bloat
is not the format running out of room — it is argument written into the index
half of a pair that already has an argument half.

Measured against a 600-character **Idea cell** (§4a explains why the Idea
cell and not the whole row):

```
status        rows   over-600   pct
raw            322        130   40%
shipped        127         87   68%
elaborated      97         52   53%
spec'd           6          5   83%
ratified         7          2   28%
rejected         7          0    0%
malformed        4          2   50%
                     ---------
                            278  (48% of all rows)
```

**`rejected` is the status that already works** — zero violations, mean 521
chars. It is also, per the registry's own preamble, "the anti-relitigation
payload — the registry's most important status." The rows doing the most
valuable work are already the shortest. That is the existence proof that the
cap is achievable, not merely desirable.

### 1b. The status vocabulary drifted into free text

`idea-registry.md:26-34` defines a closed six-token status vocabulary (`raw`,
`elaborated`, `spec'd`, `shipped`, `ratified (NNNN)`, `rejected`). Nothing
enforces it. Four rows now carry something else, including `LANG-55`'s
invented `registered` and three whose Status cell holds prose (§1c).

This drift has a specific cause, and it is an asymmetry in the existing
tooling. `docs_consistency.rs:245` (`registry_id_prefixes`) deliberately
*derives* the category prefixes from the file rather than hard-coding them,
with the comment "so the book lint auto-adapts when a new prefix is coined
rather than hard-coding a list that rots." That is correct: the category
vocabulary is **open**. Status was left equally open by omission — and the
status vocabulary is **closed**. Nothing in the test distinguishes the two.

### 1c. Three rows silently lose their pointer in the published book

`ALCH-1`, `SKY-circumpolar`, and `PROC-16` contain an unescaped `|` inside an
inline code span — `` `Correspondent | Void(reason)` ``. GFM splits the cell
on it regardless of the code span; `\|` is required.

This was verified by building the book and inspecting the rendered HTML, not
inferred. `mdbook build book`, then:

```
$ for id in PROC-16 ALCH-1 SKY-circumpolar MAP-1; do ... count <td> ...
PROC-16 rendered cells: 5
ALCH-1 rendered cells: 5
SKY-circumpolar rendered cells: 5
MAP-1 rendered cells: 5
```

The extra columns do **not** render as extra cells. mdbook truncates each row
to the header's five columns, which means the overflow shifts every
subsequent cell left and **drops the last one**. `PROC-16`'s rendered row is:

```
<td>PROC-16</td>
<td>The correspondence invariant — … (Idea, truncated at the bare pipe)</td>
<td>Void(reason)`, each carrying the [[PROC-13]] … (Idea prose, in the
    Status column, with mangled code spans)</td>
<td>shipped</td>
<td>med (workflow)</td>
```

Five cells, so no validator notices — but the columns are shifted by one and
**the Where cell is gone**. The pointer to the chronicle, the one thing this
campaign argues a row exists to carry, is silently deleted from the published
page. It has been that way undetected because every existing check is about
reference integrity and none is about row *form*.

## 2. What is NOT wrong, and is not in scope

**Completed rows are not dead weight and are not deleted.** `shipped` and
`rejected` rows are the registry's highest-value content: they are what stops
a future session re-proposing a built thing or relitigating a settled one.
`book/src/frontier/CLAUDE.md:23` states the rule ("Never delete a row") and it
stands unchanged. Deleting every completed row would recover 30% of the file
and destroy the reason the file exists.

**Migrating to a purpose-built system is rejected.** The registry's primary
consumer is an agent reading it in-context before proposing an idea; its
secondary consumer is the published book. An external tracker (Issues, Linear)
breaks both, desyncs from the commit that ships the work, and is barred by
0004. A generated page from structured data (the `*-generated.md` pattern) is
the wrong shape: those pages exist because their source of truth is *code*,
and here the source of truth is the prose itself. One-file-per-idea relocates
where the discipline must hold rather than enforcing it, and makes bloat
invisible instead of fixed. **The structure is already correct; the
enforcement was never built.**

## 3. The keystone: a long `raw` row is a category error, not a size problem

The registry already defines its own admission control and never enforced it.
`raw` means "captured but not yet elaborated; **a stub, not an argument**"
(`idea-registry.md:31`). `elaborated` means "has a full essay in
`frontier.md`."

322 rows are `raw`, and they average 831 characters of argument.

So the cap does not need a new inbox, a new file, or a new status. **If a row
has grown past the cap, the row is mislabeled**: an idea with 900 characters
of argument in it is not `raw`, it is `elaborated`, and by the registry's own
rules its argument belongs in `frontier.md` with the row pointing at it. The
cap's remedy is always one of exactly three moves, and each is already legal:

1. **Compact** — the prose is redundant with a chronicle/spec the Where cell
   already links. Delete it; keep the pointer. (Most `shipped` rows.)
2. **Relocate** — the prose is real argument. Move it to a `frontier.md`
   section, flip `raw` → `elaborated`, point Where at the new anchor.
3. **Trim** — the prose is genuinely a stub that got wordy. Cut to one clause.

This is why the cap is principled rather than cosmetic: the row is a
*shelf-mark*, and the foreign key it points through (the Where column) already
exists on every row — verified, zero rows have an empty Where cell.

## 4. The design

### 4a. Cap the prose, not the pointers

The cap applies to the **Idea cell only**, not the whole row. The Where column
carries GitHub blob URLs, which `book/src/frontier/CLAUDE.md:32-35` *mandates*
in full form (~100 chars each; a row citing spec + chronicle + essay carries
300+ characters of pure pointer). Capping the whole line would penalize a row
for carrying the pointers this design wants more of.

Measured both ways at 600 chars: whole-row → 366 violations (64%), Idea cell →
278 (48%). The 88-row difference is rows that are *already compliant prose*
with good pointers. Capping the whole row would send those to be rewritten
in the wrong direction.

**Cap: 600 characters on the Idea cell.** Precedent: `docs/decisions/README.md`
sets the sibling norm — "Keep each record short — a Y-statement plus a few
lines. If it needs a page, it is probably a spec, not a decision record." The
registry's analogue: if it needs a page, it is an essay, not a row. The
empirical anchor is §1a — 100% of `rejected` rows already comply.

### 4b. Land as a ratchet, not a cliff

278 violations cannot land in one commit. Precedent: the type audit shipped a
default-deny check against a large non-compliant corpus using `pending(wave-N)`
(decision 0028). Same mechanism here.

The check ships with a **grandfather list** — the IDs over the cap on the day
it lands, as a sorted constant in the test. The rule is asymmetric and that
asymmetry is the whole value:

- A row **not** on the list must be under the cap. New bloat fails the gate.
- A row **on** the list may exceed it, but the list is append-never: removing
  an ID is always allowed, adding one fails the test.

This makes the gate green on day one, makes every future row compliant by
construction, and turns the 278-row compaction into a burn-down that any
campaign can chip at instead of a blocking 278-row rewrite.

### 4c. The five checks

All in `cli/tests/docs_consistency.rs`, which already parses every row
(`registry_ids_are_unique:169` has the row detector to reuse).

| Check | Rule | Today |
|---|---|---|
| **Column count** | exactly 5 cells after normalizing `\|` | 3 violations |
| **Idea-cell length** | ≤ 600 chars unless grandfathered | 278 grandfathered |
| **Status vocabulary** | closed set, default-deny | 4 violations |
| **No new numbered IDs** | frozen allowlist of today's 403 | 0 (freeze at HEAD) |
| **Where non-empty** | every row carries a pointer | 0 — tripwire only |

**The column check must normalize `\|` before splitting.** This is a live
trap, not a hypothetical: a naive split on `|` reports five violations, two of
which (`SKY-22`, `MAP-connectors-as-apertures`) escape correctly and are fine.
The first pass of this analysis made exactly that error. The test gets a unit
case for an escaped pipe.

**The numbered-ID freeze completes an unfinished decision.** `0026` ratified
category+slug for new rows and its Consequence section promised
"`docs_consistency` gains a check that no new numbered decision/chronicle/study
file appears once the freeze lands" — scoped to *files*, never extended to
registry rows. `docs_consistency.rs:179` even comments "the frozen numbered
era" and then accepts both forms forever. 403 numbered rows are legitimate
history and are frozen, not renamed; 167 slug rows already exist.

### 4d. The guidance rewrite

`book/src/frontier/CLAUDE.md` currently says an idea draining into a spec
should "flip the row's status and repoint **Where**." It does not say to
*delete the prose being replaced*, and the observed failure mode is that
campaigns append their result to the row instead. Every long `shipped` row is
a stack of these appends.

Changes:
- State that repointing Where **replaces** the row's prose; the campaign's
  narrative lives in the chronicle, which the row links.
- State the cap, and the three legal remedies from §3.
- State the closed status vocabulary and that new rows take category+slug.
- State the `\|` escaping requirement for pipes in row prose.

`idea-registry.md`'s own "How to read a row" preamble gets the same, since it
is the copy an agent reads first.

## 5. Blast radius

Documentation only. No domain, kernel, or window code; no save-format, no
epoch, no stream label, no determinism contract. Worlds are byte-identical by
construction — nothing in the compute path is touched.

`book/src/frontier/` is **not** in CI's generated-artifact drift check (that
covers `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`), so
no regeneration is triggered and no census is involved.

Two consumers to keep whole:
- `docs_consistency.rs` derives its category-prefix list *from* the registry,
  so compaction must not drop the last row of any category. 22 categories; the
  smallest is `PERF` at 2 rows.
- `the_book_carries_no_registry_ids_or_process_vocabulary:320` bans registry
  IDs outside `book/src/frontier/`. Moving prose from a row into `frontier.md`
  stays inside that boundary; moving it into a *chronicle* does not — chronicle
  prose must name the concept, not the ID. This is a known recurring trap.

## 6. Acceptance

1. `cargo test -p hornvale --test docs_consistency` passes with all five
   checks live.
2. Each check has a unit test that **fails** when fed a violating row —
   including the escaped-pipe case, which is the one a naive implementation
   gets wrong.
3. The 3 column-structure violations are fixed and `mdbook build book` renders
   all three rows with the Where cell present. Verified by inspecting the
   rendered HTML, not by the markdown source.
4. The 4 status violations are resolved to the closed vocabulary.
5. The grandfather list contains exactly the IDs over cap at the compaction
   commit, and is strictly smaller than 278.
6. `make gate` green; `book/src/frontier/` renders without warnings.

## 7. Scope

**In:** the five checks, the guidance rewrite in both files, the 3 structural
fixes, the 4 status fixes, and a first compaction wave.

**Out, explicitly:**
- Deleting any row (§2).
- Renaming or renumbering any existing ID (0026: "no renames, no
  renumbering").
- Migrating the registry to any other storage (§2).
- Compacting all 278 over-cap rows in this campaign — that is the burn-down
  §4b exists to enable.
- Splitting `frontier.md` — considered and rejected as `REJ-3`.

**The first wave's size is the one open scoping question** and is flagged for
G3 (§8).

## 8. Flagged for G3

1. **Cap value: 600 chars on the Idea cell.** Adopted from the decision-log
   norm plus the `rejected`-rows anchor. It is a taste call and it is cheap to
   change *now* and expensive later — the grandfather list is computed from it.
2. **First-wave scope.** Recommendation: compact the **127 `shipped` rows**
   (87 over cap, 68% — the worst-offending status and the most mechanical,
   since their prose is redundant with chronicles they already link). This
   leaves `raw`/`elaborated` for later waves, where the work is judgment
   (relocate vs trim), not deletion. Alternative: all 278 in one campaign —
   rejected as too large to review well.
3. **The 322 `raw` rows are a triage question this campaign does not answer.**
   §3 reclassifies long ones as mislabeled, but whether an idea captured
   months ago is still live is a judgment no test can make. Recommend a
   separate pass; flagging so it is a decision, not a drift.
4. **No carve-outs triggered** — no fidelity cut, no census regen, no AWS
   spend, no destructive or externally visible action.
