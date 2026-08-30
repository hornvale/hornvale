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
for p in $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1); do
  # count tracked files newer than the marker, per declared path
done
```

(`cut -f1` takes the path column. At the time this measurement ran the file
had no second column yet — Task 3 added the author column afterward — so
the loop as actually executed needed no `cut`; it is shown here with `cut -f1`
so the recipe still reproduces against the file's current, two-column form.)

Then, for every tracked file the run left untouched, its actual author was
established by **reading the file or finding its writer** — never inferred
from its name or its directory. Evidence trail: `git log -1 -- <path>`
(does the file's last change coincide with a script/test run, or with a
one-off campaign commit?), `grep` across `scripts/`, `cli/tests/`,
`windows/lab/`, `tools/` for the literal path, and opening the file itself
when no writer turned up in code.

## What this method can and cannot establish

Every author in this file was assigned by one test: **does anything in the
tree, today, write this path?** That test can establish that nothing
currently does. It cannot establish that nothing *ever* did — a directory
can carry a real history of past automated writers that were later retired,
and git preserves that history even when the current tree does not. It
also cannot establish that nothing *should* write a path going forward;
that is a design question this file does not answer. Read every "no
current writer" and "one-off" claim below as bounded to the tree as it
stands at this commit, not as a claim about the path's whole history or
its correct future. Section 4 below (the nine study directories) is where
this distinction actually bites: one of the nine was a genuine automated
writer once, and the difference between "never automated" and "automation
retired" is this campaign's own subject.

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

**4. Nine directories with no current writer — 110 files.** All were
written, at various points in the past, by a manual `cargo run -p
hornvale -- lab run studies/<name>.study.json` (or, for one, a standalone
tool). What is verified for **all nine, equally, in the current tree**:
no test or script anywhere under `scripts/`, `cli/tests/`, `windows/lab/`
or `tools/` writes to `generated/<name>/` — checked by literal-path grep
plus reading every test that references each study by name. That is the
claim this section actually rests on.

**An earlier version of this file also claimed "the directory's entire
git history is a single commit" as second, corroborating evidence, for
all nine. That claim is false for six of them** — `branches-family` (8
commits), `census-of-coasts` (5), `census-of-coasts-tuning` (3),
`census-of-skies` (14), `the-cascade` (2), `the-namesake` (2); only
`earth-mask-l6`, `the-contour`, and `the-granary` are genuinely
single-commit. The multi-commit six are ordinary iterative development
history — a study re-run and re-committed several times while its owning
campaign was still active, then never touched again once that campaign
closed — not evidence of an ongoing automated writer, and it does no work
the no-current-writer check above does not already do on its own. It is
reported here corrected rather than deleted, because silently dropping a
disproven claim would itself be an absence with no row.

**`census-of-skies` is not the same shape as the other eight, and the
distinction matters more than the shared "no writer today" conclusion.**
Its history includes `7b7bec3ef` (2026-07-09, "chore(lab): re-baseline
census-of-skies + add it to CI's regen list") — at that point it *was*
wired into an automated regenerator, `.github/workflows/ci.yml`'s regen
list. That workflow file no longer exists at all (decision 0125 retired
CI entirely), so this directory's automation did not merely go unused —
its host was deleted out from under it. "No current writer" is still the
correct, verified answer for `census-of-skies` today, but its origin is
*an automated author that existed and was removed*, not *a study that was
only ever run once by hand* like `the-granary` or `earth-mask-l6`.
Collapsing those two into one "frozen one-off" bucket, as the earlier
version of this table did, erases exactly the difference this campaign
exists to preserve: an author that stops running leaves no trace by
default, and here git happened to preserve one anyway.

| directory | files | commits | origin |
|---|---:|---:|---|
| `branches-family/` | 35 | 8 | iterated through phonology work 2026-07-09→07-13, then frozen deliberately at `cc031f422` — census-as-data spec §1: "moves to the frozen tier — not deleted"; `fixture_staleness.rs` explicitly excludes it from staleness-checking for this reason |
| `census-of-coasts/` | 9 | 5 | iterated through the Crust terrain campaign 2026-07-09→07-13, frozen at the same `cc031f422` commit as branches-family |
| `census-of-coasts-tuning/` | 10 | 3 | iterated 2026-07-10→07-11 alongside census-of-coasts, frozen at `cc031f422` |
| `census-of-skies/` | 30 | 14 | **was wired into the now-deleted CI regen list** (`7b7bec3ef`, 2026-07-09); no writer today because that automation was retired (decision 0125), not because the study was only ever manual — see above |
| `earth-mask-l6/` | 1 | 1 | `tools/earth-mask` — a standalone tool outside the workspace, run by hand once ("Fetched once by the campaign controller"), never re-invoked |
| `the-cascade/` | 5 | 2 | two commits during one campaign, 2026-08-02, untouched since |
| `the-contour/` | 6 | 1 | one commit, campaign "The Contour", 2026-07-30 |
| `the-granary/` | 5 | 1 | one commit, campaign "The Granary" (T8), 2026-08-24 |
| `the-namesake/` | 9 | 2 | two commits during one campaign, 2026-08-02, untouched since |

Note on `is_census_study()` (`windows/lab/src/census_guard.rs:37-45`):
three of these — `census-of-coasts`, `census-of-coasts-tuning`,
`census-of-skies` — match the *naming* convention that also matches "the
census," and the canonical-host guard treats them as census-scale for
**where they may be re-run** (lefford only, if ever). That governs a
future re-run; it does not change who authored the committed bytes today.
Naming convention and current authorship diverge here, which is exactly
why the brief warns against classifying by filename.

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
5. **`census-of-skies` was once CI-authored, and its author was deleted,
   not merely stopped.** Six of the nine "no current writer" study
   directories carry multi-commit histories rather than the single-commit
   shape an earlier version of this file claimed (corrected in Section 4
   above); `census-of-skies` is the one where that history changes the
   classification's meaning — see Section 4 for the full account.
