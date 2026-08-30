# Generated-path authorship (Task 1, The Attestation)

**Not itself a declared generated path.** This file lives at `docs/`
root rather than `docs/audits/` deliberately: `docs/audits/` is itself
declared in `docs/generated-paths.txt`, and a hand-authored classification
of generated paths must not live inside one of them (that would manufacture
a fresh instance of the defect this campaign exists to remove).

## Method

From a clean tree at `14b27322a` (branch `campaign/the-attestation`, three
documentation commits ahead of the `26db4df99` the spec measured at):

```
marker=$(mktemp); sleep 1
make rebaseline
for p in $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); do
  # count tracked files newer than the marker, per declared path
done
```

Then, for every tracked file the run left untouched, its actual author was
established by **reading the file or finding its writer** — never inferred
from its name or its directory. Evidence trail: `git log -1 -- <path>`
(does the file's last change coincide with a script/test run, or with a
one-off campaign commit?), `grep` across `scripts/`, `cli/tests/`,
`windows/lab/`, `tools/` for the literal path, and opening the file itself
when no writer turned up in code.

## Step 1: the measurement reproduces exactly

```
  path                                        written / tracked
  book/src/gallery/                              45 / 56
  book/src/reference/                             9 / 21
  book/src/laboratory/                           11 / 825
  docs/audits/                                   11 / 16
  docs/digest/                                    2 / 3
  book/src/domesday/                             14 / 14
  clients/game/core/tests/fixtures/               3 / 3
  docs/audits/underworld-lattice-seed-panel.md    1 / 1
  docs/audits/the-confidant-report.md             1 / 1
  docs/audits/the-reticence-report.md             1 / 1
  ------------------------------------------------------
  TOTAL                                          98 / 941
```

**98 of 941 — identical to the figure measured 2026-08-29 at `26db4df99`.**
No re-derivation needed; the branch has moved only by documentation commits
since, and the drift-check inputs did not change.

## Per-path authorship

| declared path | tracked | written this run | author(s) of the rest |
|---|---:|---:|---|
| `book/src/gallery/` | 56 | 45 | **mixed** — see below |
| `book/src/reference/` | 21 | 9 | **mixed** — see below |
| `book/src/laboratory/` | 825 | 11 | **mixed, five authors** — see below |
| `docs/audits/` | 16 | 11 | **mixed** — see below |
| `docs/digest/` | 3 | 2 | hand-edited (`facts.jsonl`, see below) |
| `book/src/domesday/` | 14 | 14 | — (fully current; nothing to classify) |
| `clients/game/core/tests/fixtures/` | 3 | 3 | — (fully current; nothing to classify) |
| `docs/audits/underworld-lattice-seed-panel.md` | 1 | 1 | rebaseline (subset of the row above) |
| `docs/audits/the-confidant-report.md` | 1 | 1 | rebaseline (subset of the row above) |
| `docs/audits/the-reticence-report.md` | 1 | 1 | rebaseline (subset of the row above) |

**Finding, before the detail: a per-path author column cannot be a single
value.** Every path with any unwritten files at all turned out to hold at
least two authors, and `book/src/laboratory/` holds five. A schema that
assigns one author per declared path (as opposed to per file, or per
sub-path) will be wrong the day it is written for four of the seven
directory-shaped rows above.

### `book/src/gallery/` — 11 unwritten, three authors

- **8 hand-written prose pages**, verified by opening each (narrative
  voice, "Campaign N's exit artifact" framing, no generated-file banner):
  `almanac.md`, `atlas.md`, `first-light.md`, `possession-live.md`,
  `surrounds-seed-42.md`, `the-gods-seed-42.md`, `the-meeting-seed-42.md`,
  `the-sky.md`. `surrounds-seed-42.md` says so explicitly in
  `scripts/regenerate-artifacts.sh:763`: "hand-authored prose, NOT
  generated here — edit it directly."
- **2 authored by the Casement client build**, not by `regenerate-
  artifacts.sh`: `vessel.js`, `vessel-worker.js`. `clients/vessel/
  deno.json:6` names the exact command
  (`deno bundle --platform browser --minify -o ../../book/src/gallery/
  vessel.js src/main.ts && … vessel-worker.js src/worker.ts`), run by
  `make vessel-check`, a different lane entirely.
- **1 file with NO current author** — `lithology-seed-42.png`.
  `cli/src/main.rs` still accepts `--field lithology` on `hornvale map`,
  but `scripts/regenerate-artifacts.sh` invokes `--field` only for
  `sediment`, `column`, and `features` (lines 916-921); nothing calls
  `--field lithology` anywhere in the tree. Last touched 2026-07-14
  (`bc7da8cad`/`32973d3f8`), fifteen days before the sediment/column/
  features lenses were added in their current form. This is the
  preregistered null showing up in miniature: one tracked file under a
  declared generated path whose generator was retired and nobody noticed,
  because the drift check compares the file only to itself.

### `book/src/reference/` — 12 unwritten, two authors

- **11 hand-written prose pages** (verified by opening each): seven
  scene-schema docs (`scene-eclipses-v2.md`, `scene-moons-v1.md`,
  `scene-neighbors-v1.md`, `scene-surrounds-v2.md`, `scene-system-v1.md`,
  `scene-tiles-region-v1.md`, `scene-tiles-v1.md`), plus
  `concept-registry.md`, `layering.md`, `lexicon-of-place.md`,
  `stream-manifest.md`. These are the hand-authored companions to the
  *-generated.md files rebaseline does write (`concept-registry-
  generated.md`, `stream-manifest-generated.md`, and others accounted in
  the 9 written).
- **1 file authored by `cargo test`, not by `make rebaseline` at all** —
  `layering-generated.md`. `cli/tests/suite/architecture.rs:231-241`
  (`the_layering_page_matches_the_enforced_graph`) calls
  `hornvale_kernel::golden::assert_golden(...)` against it — **the same
  byte-golden mechanism CLAUDE.md says "must never be listed" in
  `docs/generated-paths.txt`**, because `make rebaseline` does not and
  must not write goldens; only `REBASELINE=1` / `make rebaseline-goldens`
  does, after a human reviews the diff. This file is nested inside
  `book/src/reference/`, which *is* declared, so the directory-level
  declaration is currently papering over one file that follows the
  opposite rule from every other file beside it. This is a live instance
  of exactly the hazard the campaign's thesis names, not a hypothetical
  one.

  (Full 11: `concept-registry.md`, `layering.md`, `lexicon-of-place.md`,
  `stream-manifest.md`, `scene-eclipses-v2.md`, `scene-moons-v1.md`,
  `scene-neighbors-v1.md`, `scene-surrounds-v2.md`, `scene-system-v1.md`,
  `scene-tiles-region-v1.md`, `scene-tiles-v1.md`.)

### `book/src/laboratory/` — 814 unwritten, five authors

This is the directory the spec's "585 unaccounted" figure is about, and
the finding here is a **correction to that figure**, not just a
classification of it (see the H1 section below).

Structure: 17 files sit directly in `book/src/laboratory/`; the remaining
808 sit under `book/src/laboratory/generated/<study-name>/`, one
subdirectory per lab study. 11 of the 825 were written this run (all 9
files of `the-chorus/`, plus the two census `schema.json` re-derivations
that `regenerate-artifacts.sh`'s unconditional backfill loop, lines
993-1002, writes on every run regardless of `HV_CENSUS`).

**1. Census-authored — 682 files, in TWO directories, not one.**
`book/src/laboratory/generated/the-census/` (229 total, 1 written —
its `schema.json`) and `book/src/laboratory/generated/census-of-the-
meeting/` (455 total, 1 written — same reason). Both are run **only**
under `HV_CENSUS=1` (`scripts/regenerate-artifacts.sh:964-970`,
confirmed live at this run: the log printed `censuses SKIPPED`), on the
canonical host only (decision 0063/0079, enforced by
`windows/lab/src/census_guard.rs`). 228 + 454 = 682 unwritten files here
whose author is "the census" and nothing else.

**2. Heavy-tier-authored, currently — 2 files.**
`generated/the-history/` (2 files). `cli/tests/suite/history_battery.rs`
writes it, tagged `heavy:`, and CLAUDE.md and `scripts/census-
canonical-host.sh:106` both confirm it as the one heavy-tier test that
still authors a committed artifact.

**3. Heavy-tier-authored historically, demoted 2026-08-28 — 3 files.**
`generated/the-sounding/` (3 files). `windows/chronicle/tests/suite/
sounding_sweep.rs`'s `run_the_sounding_and_write_the_report` is the
writer, but its `#[ignore]` reason as of this run reads: `"probe: … run
by hand (The Sounding answered its question; demoted by The Governor
2026-08-28)"` — it carries no `heavy:` tag today. The directory's last
commit (`34c6ff03b`, 2026-08-19, "chore(artifacts): regenerate after
heavy") predates the demotion, so the *committed bytes* were last
written by the heavy tier, but the *current* author-on-record is "run by
hand, ad hoc" — a manual invocation, not any automated phase.
**`scripts/sluice-phases.sh:51`'s comment ("book/src/laboratory/ (heavy
authors the-history and the-sounding)") is stale as of this correction**
and should not be trusted as a authorship source without cross-checking
the test's own `#[ignore]` reason — this is exactly the kind of prose
that rotted silently once, per this project's own history.

**4. Nine frozen, one-off studies — 110 files, all authored by a manual
`cargo run -p hornvale -- lab run studies/<name>.study.json` (or, for one,
a standalone tool), run once during a specific past campaign and never
re-run by anything automated since.** Verified two ways per directory:
(a) no test or script in the tree writes to `generated/<name>/`, and
(b) the directory's entire git history is a single commit, matching a
named campaign:

| directory | files | last commit | origin |
|---|---:|---|---|
| `branches-family/` | 35 | `cc031f422` 2026-07-13 | frozen deliberately — census-as-data spec §1: "moves to the frozen tier — not deleted"; `fixture_staleness.rs` explicitly excludes it from staleness-checking for this reason |
| `census-of-coasts/` | 9 | `cc031f422` 2026-07-13 | same commit as branches-family's freeze |
| `census-of-coasts-tuning/` | 10 | `cc031f422` 2026-07-13 | same commit |
| `census-of-skies/` | 30 | `cfabfd361` 2026-07-11 | one-time study run, carried in on a merge |
| `earth-mask-l6/` | 1 | `82c0c70a3` 2026-07-16 | `tools/earth-mask` — a standalone tool outside the workspace, run by hand once ("Fetched once by the campaign controller"), never re-invoked |
| `the-cascade/` | 5 | `b053fe533` 2026-08-02 | one-time study run alongside a lab fix |
| `the-contour/` | 6 | `0df4070a8` 2026-07-30 | one-time study run, campaign "The Contour" |
| `the-granary/` | 5 | `a7cd7fc5e` 2026-08-24 | one-time study run, campaign "The Granary" (T8) |
| `the-namesake/` | 9 | `47b41b183` 2026-08-02 | one-time study run, campaign "The Namesake" |

Note on `is_census_study()` (`windows/lab/src/census_guard.rs:37-45`):
three of these — `census-of-coasts`, `census-of-coasts-tuning`,
`census-of-skies` — match the *naming* convention that also matches "the
census," and the canonical-host guard treats them as census-scale for
**where they may be re-run** (lefford only, if ever). That governs a
future re-run; it does not change who authored the committed bytes today,
which is the one-off run named above. Naming convention and current
authorship diverge here, which is exactly why the brief warns against
classifying by filename.

**5. Hand-written prose — 17 files.** Every file directly under
`book/src/laboratory/` (not `generated/`): `overview.md`, `study-001.md`
through `study-012.md`, `the-terminator-probe.md`,
`census-of-coasts-ii.md`, `census-of-coasts-iii.md`,
`census-of-coasts-iv.md`. Verified by opening each: narrative voice,
first-person campaign framing ("Preregistered before any generator code
lands"), no generated-file banner, none rewritten by this run.
`census-of-coasts-ii.md` was the brief's given example; `-iii` and `-iv`
read identically.

Sum check: 682 + 2 + 3 + 110 + 17 = **814**, matching 825 − 11 exactly.

### `docs/audits/` — 5 unwritten, three authors

- **4 hand-written, one-off campaign audit reports** (each opens with a
  "Status: committed finding" / "committed as data" header naming its
  campaign): `heavy-tier-adjudication.md` (The Governor),
  `land-elevation-attribution.md` (The Glasshouse),
  `the-assay-build-volume-audit.md` (The Assay),
  `the-escapement-census-attribution.md` (The Escapement).
- **1 file authored by `cargo test` with an opt-in rebaseline flag, not by
  `make rebaseline`** — `lexicon-inventory.tsv`. `cli/tests/suite/
  lexicon_guard.rs:49` names it as `INVENTORY`; its own module doc says
  "re-run with `HV_LEXICON_REBASELINE=1` to rewrite the inventory." A
  **third** distinct authorship mechanism sharing this one declared path,
  alongside the 11 files `regenerate-artifacts.sh` writes directly (`lab
  confidant`, `lab reticence`, `gen_underworld_lattice`, the type-audit
  report, the digest's two rendered pages) and these 5 hand/test-authored
  ones.

### `docs/digest/` — 1 unwritten

- **`facts.jsonl`** — hand-edited, or edited through `tools/digest`'s MCP
  interface (`tools/digest/src/mcp.rs`). Its own module doc: "remain
  possible by editing `docs/digest/facts.jsonl` by hand." Not written by
  `regenerate-artifacts.sh`, which only ever reads it (to render the two
  files that ARE written: `decisions-in-force.md`, `intent-vs-reality.md`).

### `book/src/domesday/` and `clients/game/core/tests/fixtures/`

Fully current (14/14 and 3/3). Nothing to classify.

## Step 4: H1, and a correction the spec needs

**H1 — "the [unaccounted files] fall into a small number of authors" —
survives, but the preregistered COUNT it was framed against is wrong, and
Tasks 3-5 should use the corrected one.**

The spec's §1.3 computed 585 as `825 (book/src/laboratory/ total) − 11
(written) − 229 (census)`, treating `the-census/` as the whole census.
**`census-of-the-meeting/` (455 files) is authored by the exact same
`HV_CENSUS=1` gate** (`scripts/regenerate-artifacts.sh:964-970` runs both
studies together, under one `if`), so it belongs in the subtracted term
too. The correct residual after removing every rebaseline- and
census-authored file is:

```
825 − 11 (rebaseline) − 682 (both censuses' unwritten remainder) = 132
```

Those 132 do fall into a small number of authors — four, cleanly: 2 heavy
(`the-history`), 3 formerly-heavy-now-manual (`the-sounding`), 110 frozen
one-off studies across nine directories, and 17 hand-written prose pages.
**So H1's substance is confirmed** (no bulk "no author" case, no
proliferation into dozens of distinct authors) **but its headline number,
585, should not be carried into Task 3's schema design or Task 5's
deletion list as if it were the count of files needing a real per-file
author value.** 585 conflates 454 genuinely census-authored files (which
already have a clean, single-value answer: `census`) with the 132 that
needed the work this file just did. Whichever task designs the author
column's possible values should use the four-author + hand-written
breakdown above, not re-derive 585 as its scope.

**A second, smaller correction, not part of H1 but found in the course of
answering it:** `scripts/sluice-phases.sh:51`'s comment naming `the-
sounding` as heavy-authored is stale since 2026-08-28's demotion (see
above) — worth a fix wherever that comment is next touched, since a
reader taking it at face value would misclassify the file the same way
the spec's own math did for the census.

## Concerns for review

1. **`lithology-seed-42.png` has no current author.** Not "many authors,"
   not "no author in bulk" — one file, but it is the concrete instance of
   exactly the failure class this campaign is about: a tracked file under
   a declared generated path that nothing currently writes, invisible to
   the drift check because the check only ever compares the file to
   itself.
2. **`layering-generated.md` is a byte-golden living inside a declared
   `make rebaseline`-authored directory.** This is not a hypothetical
   collision with the "byte-goldens must never be listed" rule in
   `docs/generated-paths.txt`'s own header — it is a real, present
   instance of one file inside a listed directory following the opposite
   contract from its neighbours.
3. **`lexicon-inventory.tsv` inside `docs/audits/` is the same shape**:
   test-authored, opt-in rebaseline flag, not `make rebaseline`.
4. **The spec's 585 needs correcting to 132** before Task 3 designs the
   author-column schema against it, per the H1 section above.
