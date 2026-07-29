# The Running Head — Design

**Status:** draft, awaiting G3 · **Date:** 2026-07-29 · **Campaign:** The Running Head

A retitling and reorganization pass over the project book. No mechanism, no
new facts, no draws. Every change is a chapter title, a heading string, or a
line of `SUMMARY.md`.

## 1. The problem

The book carries three naming eras in one table of contents.

- **Chronicle.** 24 of ~170 entries keep a designation prefix that ratified
  decisions retired: `Campaign 1a:` … `Campaign 5:` (alphanumeric),
  `Campaign Y2-0/Y2-1` and `Campaign L0:` (retired by decision 0017),
  `Campaign 15:` … `Campaign 27:` (retired by decision 0026, which made
  campaigns name-only). The remaining ~146 are already bare names.
- **Laboratory.** Numbering stops mid-list: `Study 001:` … `Study 012:` are
  numbered, then `The Census of Coasts II/III/IV` and `The Terminator Probe`
  are bare — decision 0026 keys studies by name, upheld by 0043.
- **Gallery.** Four competing seed suffixes: `X of Seed 42` (11 pages),
  `X — Seed 42` (6), `First Light (seed 42)` (1), and no marker at all (2).
- **Drift.** Two different chapters, `A Possession — Seed 42` and
  `A Possession, Over Time`, render the *same* H1: `A Possession — seed 42,
  day 0`. `The Concept Manifest` in `SUMMARY.md` renders as `Concept
  Manifest — the correspondence ledger`.
- **Organization.** `# The Book` and `# The Laboratory` are parts whose lead
  chapter repeats the part header verbatim. Architecture and Domains do not
  do this — their parts lead with a distinct chapter (`The Four Layers`,
  `The Cascade, Tier 0 to Tier 1`) and nest children beneath it. Laboratory
  is flat where it should nest.

## 2. What is generated, and what that costs

Most Gallery and Reference H1s are not hand-authored. This is the fact that
sets the shape of the work.

| H1 source | Pages | Change cost |
|---|---|---|
| `cli/src/main.rs` (7 writers) | Land, Biomes, Peoples, Deep Time, Vestige, Night Sky, Possession | Rust edit + regen + gate |
| `windows/almanac/src/lib.rs:270` | Almanac ×3 | Rust edit + regen + **2 test assertions** |
| `cli/src/{concepts,phonology,dictionary,proto}.rs` | Reference pages | Rust edit + regen + **1–2 assertions each** |
| `scripts/regenerate-artifacts.sh` (4 printf) | Abandoned Clearing, Transport Topology, Look of the World, Strange Sites | Script edit + regen |
| Hand-authored `.md` | 8 Gallery pages, all Chronicle, all Laboratory | Direct edit, no regen |
| `book/src/SUMMARY.md` | Every sidebar entry | Direct edit, no regen |

The majority Gallery form, `X of Seed {n}`, is the **templated, seed-agnostic**
one emitted by Rust. The minority `— Seed 42` form is hardcoded `42` in
shell. This asymmetry decides the target convention: standardize on the form
the code already produces, and the Rust writers need no edit at all.

## 3. Decisions

**D1 — Target Gallery convention: `<Definite noun phrase> of Seed 42`.**
One stated exception: where the phrase *preceding* the suffix already
contains "of", the page keeps the em-dash form, to avoid `The Look of the
World of Seed 42`. One rule, one exception — `room-sample-seed-42.md` is the
only page that trips it.

**D2 — Strip `Campaign NN:` and `Study NNN:` from titles; re-home the
campaign designation into the page subtitle.** Decision 0026's "existing
numbered artifacts freeze as history: no renames" governs *filenames and
IDs*, which this pass does not touch — every filename and every existing
cross-reference keeps resolving. But prose across `domains/*.md`,
`introduction.md`, and `open-questions.md` cites campaigns as "Campaign 15
(The Eyes)" dozens of times, so a reader holding "Campaign 15" needs on-page
confirmation. Every numbered entry already carries a `**date · commits ·
outcome**` subtitle; the designation is prepended there:

```
**Campaign 16 · July 2026 · 14 commits · outcome: complete, merged — …**
```

This applies to all 24 stripped entries, using each one's own designation as
it stands today — `Campaign 1a ·`, `Campaign Y2-0 ·`, `Campaign L0 ·`,
`Campaign 15 ·`, and so on. No renumbering, no normalization across eras:
the subtitle records the designation the campaign actually carried.

Studies get **no** such re-homing. A campaign happened at a time, so its
number encodes a real temporal fact. A study is re-runnable and timeless —
its number was always arbitrary, which is why 0026 keyed studies by name.
Forcing symmetry would invent metadata that means nothing.

**D3 — `The Tongues` collision → the later entry becomes `The Tongues II`.**
Stripping `Campaign 16: The Tongues` collides with the later, different
`The Tongues` (`the-tongues.md`). Repo precedent puts the numeral on the
later work: `Firm Ground` / `Firm Ground II`, `The Census of Coasts II/III/IV`.

**D4 — `The Laboratory` collision → rename the *overview chapter*, not the
chronicle entry.** Stripping `Campaign L0: The Laboratory` collides with the
Laboratory part's lead chapter. Following the Architecture/Domains model
(part header distinct from lead chapter), `laboratory/overview.md` becomes
**`Studies Are Data, Metrics Are Code`** — decision 0011's ratified phrase,
and the chapter's actual thesis.

**D5 — `# The Book` → `# The Book of Seed {n}`.** Resolves the part/chapter
echo and brings the page under the D1 convention. No test or golden pins the
current string (verified: `grep -rn '# The Book'` over `cli/tests/`,
`clients/`, `book/`, `docs/` returns only the page itself, the unrelated
`The Book Polish` chronicle entry, and a historical plan document).

**D6 — Reference: fix the one mismatch, leave the cosmetics.** `# Concept
Manifest — the correspondence ledger` → `# The Concept Manifest`, matching
what `SUMMARY.md` already says. `Phonology`, `Dictionary`, and
`Proto-goblinoid` keep their bare titles: they match their sidebar entries
exactly, so there is no drift to fix, and touching them would edit Rust and
two assertions for pure house-style.

**D7 — Possession collision.** `cli/src/main.rs:466` becomes `# A Possession
of Seed {n} — day {day}`, which fixes the day-0 page and aligns the CLI. The
over-time page cannot be distinguished that way (it also starts at day 0), so
its regen block emits `# A Possession of Seed 42 — over time` explicitly in
place of its current `head -n 1`.

## 4. The title map

### Chronicle — strip prefix (24), both `SUMMARY.md` and page H1

| File | New title |
|---|---|
| `campaign-1a.md` | The Kernel |
| `campaign-1b.md` | The Tier-0 Cascade |
| `campaign-2a.md` | System Genesis |
| `campaign-2b.md` | The Sky's Debut |
| `campaign-l0-lab.md` | The Laboratory |
| `campaign-3b.md` | The Tectonic Globe |
| `campaign-3c.md` | Climate & Biomes |
| `campaign-4a.md` | Placement & Drainage |
| `campaign-4b.md` | Emergent Society |
| `campaign-5.md` | The Gods |
| `campaign-y2-0.md` | Firm Ground |
| `campaign-y2-1.md` | The Peoples |
| `15-the-eyes.md` | The Eyes |
| `16-the-tongues.md` | The Tongues |
| `17-audible-phonology.md` | Audible Phonology |
| `18-the-meeting.md` | The Meeting |
| `19-the-star-chart.md` | The Star Chart |
| `20-firm-ground-ii.md` | Firm Ground II |
| `21-the-scene-window.md` | The Scene Window |
| `22-the-atlas.md` | The Atlas |
| `23-the-orrery.md` | The Orrery |
| `25-the-measured-coast.md` | The Measured Coast |
| `26-the-live-orrery.md` | The Live Orrery |
| `27-the-words.md` | The Words |
| `the-tongues.md` | The Tongues **II** (D3) |

Collision sweep against the other ~146 chronicle titles: only `The Tongues`
(D3) and `The Laboratory` (D4) collide. `The Gods`, `The Peoples`,
`The Atlas`, `The Meeting`, `The Eyes`, `The Words`, `The Orrery` are each
unique within the Chronicle and are distinguished from their Gallery
namesakes by the `of Seed 42` suffix.

### Laboratory — strip prefix (12) + rename overview + nest

`study-001.md` … `study-012.md` drop `Study NNN: `. `study-006.md`'s
`The Census of Peoples II: Two Peoples` becomes `The Census of Peoples II`
(the trailing gloss is a second colon in a title; the page body carries it).
`overview.md` becomes `Studies Are Data, Metrics Are Code` (D4). All 16
studies nest one level under it in `SUMMARY.md`, matching Domains.

### Gallery — unify the suffix

| Page | Old | New | Where |
|---|---|---|---|
| `first-light.md` | First Light (seed 42) | First Light of Seed 42 | hand |
| `surrounds-seed-42.md` | The Purview, Off a Possession — Seed 42 | The Purview of Seed 42 | hand |
| `history-seed-42.md` | An Abandoned Clearing — Seed 42 | The Abandoned Clearing of Seed 42 | script L154 |
| `connections-seed-42.md` | The Transport Topology — Seed 42 | The Transport Topology of Seed 42 | script L192 |
| `strange-sites-seed-42.md` | Strange Sites — Seed 42 | The Strange Sites of Seed 42 | script L305 |
| `possession-seed-42.md` | A Possession — seed 42, day 0 | A Possession of Seed 42 — day 0 | `main.rs:466` |
| `possession-over-time-seed-42.md` | A Possession — seed 42, day 0 | A Possession of Seed 42 — over time | script L137 block |
| `the-book.md` | The Book | The Book of Seed 42 | `main.rs:856` |
| `room-sample-seed-42.md` | The Look of the World — Seed 42 | *unchanged* | D1 exception |

The 11 pages already on `of Seed 42` are untouched, so
`exit_criterion.rs:67` and `windows/almanac/src/lib.rs:666` stay green.

### Reference

`cli/src/concepts.rs:213` → `# The Concept Manifest`, with the paired
assertion at `concepts.rs:391` updated.

### Organization

- `# The Book` part: lead chapter becomes `The Book of Seed 42` (D5),
  ending the header/chapter echo.
- `# The Laboratory` part: lead chapter becomes `Studies Are Data, Metrics
  Are Code` (D4); the 16 studies nest beneath it.
- `# The Constitution` / `Six Principles` and `# Open Questions` / `The
  Confidence Gradient` already differ from their part headers — unchanged.
- `# The Gallery` stays flat: it has no overview page, and creating one is
  authoring, not renaming.

## 5. Non-goals

- **No file renames or moves.** Decision 0026 freezes existing filenames;
  every cross-reference keeps resolving.
- **No prose rewrites.** Body text citing "Campaign 15 (The Eyes)" stays —
  D2's subtitle re-homing is what keeps those cites resolvable.
- **No Chronicle era-grouping.** The Chronicle is a flat ~170-item list, and
  the real navigability lever is grouping it into eras or dating it — a
  separate change, filed as a followup rather than smuggled in here.
- **No `The`-prefixing of `Phonology` / `Dictionary` / `Proto-goblinoid`** (D6).

## 6. Verification

1. `mdbook build book` — every `SUMMARY.md` link resolves.
2. `cargo test -p hornvale` — the `concepts.rs` and possession assertions.
3. `make rebaseline`, then `git diff --exit-code book/src/gallery/
   book/src/reference/ book/src/laboratory/ docs/audits/` — confirms the
   regenerated titles match the committed ones and nothing else moved.
4. `make gate`.
5. Manual: the regen diff must be **titles only**. Any body-text change in a
   generated page means a title edit reached further than intended.

## 7. Definition of Done

Per decision 0013: a chronicle entry (`book/src/chronicle/the-running-head.md`)
and a retrospective (`docs/retrospectives/`). Flagged for Nathan at G3: this
is ceremony for a pass with no mechanism change, and is his call to keep or
drop.
