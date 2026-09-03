# The Nettle — design

> **COMPLETE — merged as campaign *the-nettle*.** All six tasks landed and
> reviewed; the whole-branch review returned safe-to-merge with no Critical
> findings and no residuals. Item 5 was dropped per Nathan's G3 option (d) and
> is carried as a corrected registry row. Chronicle:
> `book/src/chronicle/the-nettle.md`. Retrospective:
> `docs/retrospectives/the-nettle.md`. Rulings: 19 entries in
> `docs/superpowers/ledgers/2026-09-02-the-nettle.md`.


**Campaign:** the-nettle · **Branch:** `campaign/the-nettle` ·
**Ledger:** `docs/superpowers/ledgers/2026-09-02-the-nettle.md`

Five bounded annoyances measured during The Reservoir and handed over as a
brief. Four are built here; the fifth is flagged for Nathan at G3 by explicit
instruction.

---

## §0. What this campaign is for, after verification

The brief's five items were re-derived before being planned. **Four of the
five needed correction, and two were materially wrong.** The table is in
ledger #2 and is not restated here; the consequences for scope are:

- **Item 1 is 5× its stated size** (14 sites / 8 files, not 3 files), and two
  sites `remove_dir_all` a fixed directory, which is destructive rather than
  merely colliding.
- **Item 3's stated defect was already fixed** before the brief was written.
  A live sibling sits one section up in the same file.
- **Item 4's proposed cost figure was the test-execution time**, not the wall.
  The wall is 96× larger in the mode that matters.
- **Item 5's premise is false.** Thirteen drift-checked seed-42 PNGs render
  from the very structs it says nothing pins.

The cross-cutting cause is recorded as a lesson, **not built here**: a parked
finding that asserts a property of a *file's bytes* stays true until someone
edits that file, while one that asserts a property of the *repo's whole state*
("nothing does X") decays silently the moment any campaign closes the gap —
and nothing distinguishes the two at the moment of filing. Measured support:
**the registry's five columns are ID, Idea, Status, Conf and Where — there is no observed-at column at all, across 1,026 `raw` rows.**
This campaign fixes its five items and captures that as one registry row plus
a retrospective lesson. Building a parking-discipline mechanism is out of
scope (§7).

---

## §1. Fixed temp paths in tests (item 1)

### The defect

A test that writes to `std::env::temp_dir().join("<fixed name>")` shares that
path with every concurrent run of the same test on the box. Observed
2026-09-02 (The Reservoir, parked finding P1): `scene_surrounds_colour_cli.rs`
asserted `new` succeeded, then panicked at `World::load` with
`NotFound`, taking down a fail-fast run at test 274 of 4869.

### The set, re-derived

Classified mechanically over every `temp_dir()` site in the repo (excluding
`.claude/worktrees/` copies), by whether the joined name carries a
uniquifier — `process::id()`, a counter, or a per-test tag.

```
FIXED, and a writer -- to be fixed (14 sites / 8 files)
  cli/tests/suite/locale_cli.rs                19, 34
  cli/tests/suite/scene_moons_cli.rs           19
  cli/tests/suite/scene_surrounds_colour_cli.rs 42
  tools/digest/src/mcp.rs                      68, 90, 105, 116, 138
  tools/placement-audit/src/walk.rs            138   <- remove_dir_all
  tools/type-audit/src/walk.rs                 181   <- remove_dir_all
  windows/lab/src/domesday/comparators.rs      289, 310, 328

FIXED, but NOT a writer -- documented, not changed
  windows/lab/tests/suite/anomaly_holdout.rs   160
    Pure path arithmetic: asserts the scratch path does not start with the
    committed goldens dir. Never creates anything. Changing it would add
    noise; it goes in the ratchet's roster with that reason.

NOT FIXED AFTER ALL -- a false alarm the brief named as a candidate
  cli/tests/suite/repertory_corpus.rs          242
    `scratch_name()` (line 232) is
    `format!("hv-repertory-{}-{}-{}", std::process::id(), seed, n)`.
    Already uniquified; the inner "script.txt" sits inside that unique dir.
```

The brief's second named candidate, `comparators.rs`'s `armature-dir-test`,
**is** real — and it is the predicted shape exactly: two other sites in that
same file (lines 239, 264) already embed `process::id()`, so a per-file survey
reads the file as safe.

### The fix

Follow the neighbours, as the brief directs. Measured over the repo (worktree
copies excluded): **66 `temp_dir()` sites, 50 already uniquified, 16
flagged** — the 14 writers above, plus the one non-writer and the one false
alarm. The dominant convention is a `process::id()` in the name;
`CARGO_TARGET_TMPDIR` (4 sites) is used where a file must *persist* past the
run, which is a different job. Each site takes the shape its own file already
uses.

`tools/` sites are outside the cargo workspace and outside every gate; they
are fixed anyway, and their suites run by hand
(`cargo test --manifest-path tools/<t>/Cargo.toml`).

### The ratchet

A fixed temp path is greppable, so it is the class where a standing guard is
cheap and permanent. `cli/tests/suite/test_binary_ratchet.rs` is the exact
precedent to follow, including the properties that make it work:

- **Three-valued, not a wall.** A frozen roster of accepted sites, so the
  guard is not red on day one and trained away by day two.
- **Both directions.** A new unuiquified site reddens; a roster entry whose
  site no longer exists also reddens, so the list cannot rot.
- **Explicit crate enumeration, never a blind walk** — `test_binary_ratchet`
  enumerates crate directories rather than walking from the root, which is
  what keeps `.claude/worktrees/` out. This guard must scan `tools/` too, so
  it enumerates that as well.

The roster starts with exactly one entry: `anomaly_holdout.rs:160`, with its
reason.

**Do not hand-edit `docs/timings/subfloor-roster.tsv`** to add the new test.
The chamber's `gate` phase rewrites that file on a green run and commits it
like any other tracked drift.

### Verification

- Every fixed site above rewritten; re-run the classifier and confirm the
  FIXED-and-a-writer list is empty.
- The ratchet must be shown to **discriminate**, not merely to pass:
  introduce a fixed-path site, confirm RED; remove it, confirm GREEN; delete
  the roster's one entry, confirm RED in the other direction.

---

## §2. The Bash guard matches command text, not the command (item 2)

### The defect, reproduced

`scripts/hv-guard-bash.sh` runs its rules against the raw command string, so a
quoted argument or a heredoc *body* that merely mentions a command is refused
as though it were that command. Verified by driving the script directly:

```
A  heredoc whose body is prose about the gate        deny   <- false positive
B  grep searching FOR the flag, quoted               deny   <- false positive
C  heredoc whose body mentions the flag              deny   <- false positive
D  CONTROL  a real whole-workspace run               deny   <- correct
E  CONTROL  a real hook bypass                       deny   <- correct
F  CONTROL  a narrow, scoped run                     allow  <- correct
```

D/E/F are the positive controls, and they matter: an earlier version of this
harness returned `allow` for all six, which looked like a clean result and was
a broken probe.

Three **further live instances** were generated while writing this spec — each
attempt to create the probe file was itself refused, because the heredoc
carrying the test data mentioned the commands. That is six instances across
two sessions, all on legitimate work, and it is the failure mode the guard's
own header warns about: a guard that blocks legitimate work "gets
`HV_TEST_OK=1` exported into a shell profile and dies."

### The fix

Add a **projection** applied to the command text before any rule runs, and run
the existing rules unchanged against the projection:

1. **Strip heredoc bodies** (`<<` and `<<-`, quoted or unquoted delimiter). A
   heredoc body is data on a program's stdin; this shell never executes it.
2. **Strip single- and double-quoted string literals.**

This is a narrowing of the *input*, not of the rules, so each rule's reasoning
and message survive untouched.

### The hole this accepts, named rather than discovered later

`bash -c "cargo nextest run --workspace"` and `bash <<EOF … EOF` become
invisible to the guard. Both are real; both are rare; and the guard is
explicitly designed to **fail open** ("any internal error allows the
command"), so a missed detection is the failure mode it already accepts, while
a false refusal is the one it says kills the guard. Recorded here so a future
reader does not rediscover it as a defect.

### Verification

The self-test is the deliverable, not an afterthought. It already carries a
deny case for every rule; the change adds allow cases for A, B and C and
**must leave every existing deny case denying**. A narrowing that flips any
existing deny to allow has removed the rule rather than narrowed it — that is
the stop condition, stated as a branch table:

- all existing denies still deny, A/B/C now allow → **done**
- any existing deny now allows → **STOP**, the projection is too aggressive
- A/B/C still deny → the projection is not reaching them; do not "fix" it by
  weakening a rule

---

## §3. `campaign-autopilot`'s live half, and a stale row (item 3)

### The stated defect is already closed

`.claude/skills/campaign-autopilot/SKILL.md:190` already names
`docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, fixed at `3b9a026aa`
(2026-08-30, The Cartulary) — an ancestor of The Reservoir's own merge. A grep
across `.claude/skills/` and the vendored superpowers 6.3.0 tree finds no
reference to `.superpowers/sdd/decision-ledger.md` in any loaded skill.

### The live defect, one section up

Lines 181-183, in *Capture discipline*, route actionable followups to
`.superpowers/sdd/followups.md` — "promoted into the campaign retrospective's
follow-up section at close". That carries **both** defects The Cartulary
fixed for rulings:

- **Flat and shared.** Decision 0493 names this exact shape; two such files
  exist in this checkout today, one of them in the shared main checkout.
- **Promoted at close** — the practice that failed five recorded times
  (The Ell, The Quoin, The Gallery/Lodestar, The Overture, The Attestation).

It is the mechanism behind The Reservoir's ledger #20: four deferred minors
that lived only in git-ignored scratch and were recovered by a closer's
backstop walk rather than by the process.

### The fix

1. Route actionable followups to the committed per-campaign ledger, written as
   they occur, consistent with rulings.
2. Add the sentence Nathan asked for, naming **why** the two files differ —
   "task state, fix rounds and resume-after-compaction material → the plugin's
   `progress.md`, which is scratch and dies with the worktree; rulings,
   deferred minors and followups → the committed ledger" — because *nothing
   makes that distinction visible at the moment of writing*, which is ledger
   #20's own generalisable lesson.
3. **Refute** `PROC-autopilot-names-the-superseded-ledger-path`, citing the
   commit that closed it. `refuted` is the admissible status for a claim a
   measurement overturned (as distinct from `rejected`), and
   `docs_consistency::every_refuted_row_cites_its_evidence` requires the
   citation.
4. Open a replacement row for the live half.

### Verification

A grep for `.superpowers/sdd/followups.md` across all loaded skills returns
nothing; `docs_consistency` stays green (it gates the registry's statuses,
uniqueness and citations).

---

## §4. Docs-only commits skip a gate that checks docs (item 4)

### The defect

`scripts/hooks/pre-commit` fast-paths past `make gate-commit` when no
Rust-relevant path is staged. The premise is "Rust paths staged → Rust checks
matter"; the counterexample is **a Rust test whose subject is prose**.
`docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary` is
in the sub-floor roster, would have caught a `MAP-25` citation in a chronicle,
and never ran — across roughly twelve docs-only commits in one campaign. The
merge queue finds it instead, on the canonical box, after taking the shared
claim.

### The measurement the registry row asked for

`cargo nextest run -p hornvale --test suite -E 'test(docs_consistency)'`,
28 tests, this Mac, warm 36 GB `target/`:

```
condition                                    wall        test execution
-------------------------------------------  ----------  --------------
after a 137-commit absorption (suite cold)    234.6 s          2.094 s
immediately after, binary warm                  2.443 s        1.324 s
```

**The registry row's "~1 s" is the execution figure.** The wall is dominated
by building the `hornvale` suite binary — the same finding CLAUDE.md records
for `gate-commit` as a whole (compilation units, not tests). This matters
because a docs-only commit right after absorbing main — the chronicle and
retrospective commits at campaign close — is exactly when the tree is cold.

**Correction (fix wave, 2026-09-03): the 2.443 s warm figure above is the
28-test `test(docs_consistency)` filter, not the shipped filter.** The
shipped `docs-tests` target runs eight predicates, 60 tests (62 once
`temp_path_ratchet` was added in the same fix wave), and warm re-measurements
of that wider filter came in at **5.664 s, 10.249 s, 16.0 s, 17.2 s** (mine
and the reviewer's, same box, different load) and 20.2 s (`real`, this fix
wave) — a **5.7–24.0 s** warm range. The pole is `repose_byte_identity`'s
three worldgen probes (seed-42 almanac/world-json/scene, each several seconds
on their own), which are inherently that costly and not primarily a symptom
of box contention. The cold build figure (234.6 s) is unaffected: `nextest`
compiles the whole `suite` binary regardless of which predicate selects
tests from it, so build cost does not vary with the filter.

### Why it is still worth paying

A campaign's docs-only commits cluster after one absorption, so the realistic
cost is the cold build **once** plus ~5.7–24.0 s each thereafter: roughly
`234 + 11 × 5.7 ≈ 297 s` to `234 + 11 × 20.2 ≈ 456 s` per absorption cycle
for a twelve-commit campaign. The alternative is a merge phase reddening on
the canonical box after taking the serial claim, on a run that costs
~1129–1595 s. Even at the top of the corrected range this beats the merge
phase by more than 2x. The trade is clear.

### The fix

Invert the fast path rather than deleting it: when no Rust-relevant path is
staged, run the **prose-subject tests** instead of skipping outright. The
three unconditional guards above the fast path keep running unconditionally,
which is already load-bearing and must not change.

### Deriving the test set — a branch table, not a prediction

The set is **not** asserted here. `docs_consistency` is the known member; a
grep for tests mentioning `docs/` or `book/src` matches **24 of the 50 files**
in `cli/tests/suite/`, but mentioning a path in a comment is not reading it. The task derives the set from what each
test actually **reads**, and reports:

- a test reads only `docs/` or `book/` inputs → **in the set**
- a test reads a mix of prose and Rust sources → **in the set**, since a
  docs-only commit can still move its verdict
- a test reads no prose input → **out**, and say which grep hit put it on the
  candidate list
- the derived set is larger than ~40 tests, or its warm wall exceeds ~10 s →
  **STOP and report**; the cheap shape has stopped being cheap and the scope
  call returns to Nathan

### Verification

Positive control, and it is the whole point: stage a docs-only commit
containing a defect one of these tests catches (a registry ID cited from
`book/src/chronicle/` reproduces the original), confirm the hook **refuses**,
then remove it and confirm the hook passes. A hook change that has only been
seen to pass is not known to discriminate.

---

## §5. FLAGGED FOR G3 — nothing pins the derived artifacts' bytes (item 5)

**Not decided here.** The brief instructs that this come to Nathan with
options rather than a pick. The verification below changes the question, so
the options are restated against the corrected premise.

### The premise as stated is false

> "`GeneratedTerrain` and `GeneratedClimate` are not `Serialize`, and no
> committed artifact pins the terrain sculpt byte-for-byte."

The first clause is true. The second is not. `book/src/gallery/` is a declared
`artifacts` path in `docs/generated-paths.txt`, is regenerated by
`make rebaseline`, and is byte-compared by the drift check. Of its 13 tracked
PNGs, **10 have a live writer and render from worlds built through those very
structs** — elevation, biome (×2), sediment, column, features, paleo, vestige,
and settlement (×2, downstream of both). The other three are not evidence
here and are excluded deliberately: `star-chart-seed-42.png` is astronomy, and
`first-light-seed-42.png` and `lithology-seed-42.png` have no current writer
in `regenerate-artifacts.sh` at all. The census additionally pins ~203 metrics
derived from the same worlds.

### What the real residue is

The PNGs pin **projections**, not state: elevation mapped to 8-bit colour
through a renderer. Two things can slip past them — a change smaller than the
colour quantum, and a change to a field no map renders. That is a genuine gap,
and it is far narrower than "nothing pins the bytes".

### The options

| | option | cost | what it buys | what it costs conceptually |
|---|---|---|---|---|
| a | derive `Serialize` on both structs | medium | a direct pin | **contradicted by the code**: `GeneratedClimate`'s own doc says "Recomputed on demand, never serialized", and it holds an `Fbm` sampler and two derive-once indices — caches, not state. Save-format-adjacent; needs its own decision |
| b | commit a byte-golden | high | a determinism reference | a multi-MB committed artifact, and it becomes a reference under the same discipline as `cli/tests/fixtures/world-seed-42.json` — a deliberate act |
| c | pin a digest of the field values | low | full-precision coverage of every field, above what the PNGs reach; no `Serialize`, no large artifact | pins a number, so a diff says "something moved" without saying what |
| d | drop it here; correct the premise and file the row | ~zero | an accurate record | leaves the narrow residue open |

**Recommendation: (d), with (c) as the alternative if you want coverage now.**
The marginal coverage over 13 drift-checked PNGs plus ~203 census metrics is
small; the *valuable* act is correcting a parked finding that overstated the
gap, since no `TOOL-*` row exists for it yet and the wrong version would be
what the next reader inherits. (a) is a save-format-adjacent change that argues
against the code's own stated design and should not ride along in a small-fixes
campaign. If you want the residue closed now, (c) is the only cheap option.

---

## §6. Testing

Every item above carries its own verification, and each is a **discrimination
test** rather than a pass: §1's ratchet must be shown to redden in both
directions, §2's self-test must keep every existing deny denying, §4's hook
must be shown to refuse a real prose defect. This is the campaign's own
subject applied to itself — an unexercised guard in a healthy-looking artifact
is what item 3 turned out to be.

## §7. Non-goals

- **A parking-discipline mechanism.** The observation/judgment distinction and
  the missing observed-at SHA are captured as a registry row and a
  retrospective lesson. Building it is a campaign of its own.
- **Depth-scoping, `MAP-25`.** Named only as the source of the citation defect
  in §4; not reopened.
- **Item 5's implementation**, pending §5's G3 call.
- **Changing what `gate-commit` runs.** §4 changes only which commits reach it.
