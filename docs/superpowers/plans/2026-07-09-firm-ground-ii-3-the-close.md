# Firm Ground II, Plan 3: The Close — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cash the campaign's check — do the **single re-baseline** of every committed artifact (both halves' churn at once), write the astronomy domain chapter to **tier 2** with the extended model card, and close the book (chronicle entry + freshness sweep).

**Architecture:** No new code and no logic changes. This plan regenerates the committed artifacts that Plans 1–2 deliberately left stale (the drift check has been red since Plan 1's Lab metric and Plan 2's placed observer), verifies the regeneration is clean and idempotent, then writes the book: the astronomy chapter's tier-2 sections (the moving sky, the placed observer) and model-card rows, the Campaign 19 chronicle entry, and a freshness sweep of chapters the campaign made stale.

**Tech Stack:** The CLI verbs (`new`, `almanac`, `concepts`, `streams`, `phonology`, `map`, `biome-map`, `settlement-map`, `lab run`), `mdbook`, and prose. Rust edition 2024. No crate is edited.

## Global Constraints

- **The re-baseline happens exactly once, here** (spec §7). Plans 1 and 2 each ended with the committed artifacts stale by design; this plan is where the churn is absorbed. Do not split the re-baseline across commits per-artifact — regenerate the whole set together so the diff tells one coherent story.
- **The CI "Artifacts are current" step is the authoritative regeneration list** (CLAUDE.md; `.github/workflows/ci.yml`). Copy the command list from that step at execution time — do not hand-transcribe from this plan, which is a snapshot. `census-of-skies` is a **frozen historical (Y2) artifact**: CI does not regenerate it, so this plan does not either; leaving it byte-untouched keeps the drift check green.
- **`main` is volatile — the shared seam is here** (standing rule 2; decision 0022). The parallel PNG-migration thread is landing on `main` and touches the *same* committed gallery artifacts and the *same* CI artifact-command list. **Before regenerating (Task 1), run `git merge-base HEAD main` and reconcile** with whatever the rendering thread has landed: adopt the current CI command list, and if a map/PNG format changed, regenerate against the merged tooling — never revert the rendering thread's format.
- **The gate for this plan includes the artifact drift check.** After Task 1, `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` must be **clean** (the whole CI artifact step must pass), in addition to `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. This is the plan that turns the campaign's drift check green.
- **Determinism (constitutional):** the re-baseline must be **idempotent** — regenerating a second time leaves every artifact byte-identical (spec §10). Task 1 verifies this explicitly.
- **Book altitude** (CLAUDE.md): technical and mathematical, comprehensible without reading the code it may show. The book is **merged reality only** — no speculative material (no PSY-4, no deferred-idea prose); the frontier/registry stay out of `book/`.
- **DoD** (decision 0013, 0020): a chronicle entry and a freshness sweep are required; the campaign retrospective (`docs/retrospectives/`) is a separate close-out, not part of this plan.
- **Conventional commits** with the trailer `Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9`; `cargo fmt` is a no-op here (no Rust edited) but run the full gate before every commit; never `--no-verify`.

---

## File Structure

| File | Action | Responsibility |
| --- | --- | --- |
| `book/src/gallery/almanac-seed-42-sky.md` | Regenerate | Default (spinning) almanac — calendar daylight line now at the flagship's latitude; Plan-1 calendar changes. |
| `book/src/gallery/almanac-seed-42-locked.md` | Regenerate | Locked almanac — hemisphere-culled sky, place-dependent pantheon. |
| `book/src/gallery/almanac-seed-42.md` | Regenerate | Constant-sky almanac (regenerated for completeness; expected unchanged). |
| `book/src/reference/stream-manifest-generated.md` | Regenerate | Gains Plan 1's `forcing` + `phase-offsets` stream labels. |
| `book/src/reference/concept-registry-generated.md` | Regenerate | Gains Plan 1's `eccentricity-mean` + `obliquity-amplitude` predicates. |
| `book/src/reference/phonology.md`, `book/src/gallery/{elevation,biome,settlement}-seed-42*.{md,png}` | Regenerate | Per the CI list (expected unchanged; regenerate to prove it). |
| `book/src/laboratory/generated/census-lands-drift/` | Regenerate | Gains Plan 1's `obliquity-range` metric; absorbs Plan-1/2 changes. |
| `book/src/laboratory/generated/census-of-the-meeting/` | Regenerate | Absorbs any Plan-1/2 metric drift. |
| `book/src/domains/astronomy.md` | Rewrite | Tier-2 chapter (the moving sky, the placed observer) + extended model card. |
| `book/src/chronicle/19-firm-ground-ii.md` | Create | The campaign chronicle entry. |
| `book/src/SUMMARY.md` | Modify | Link the new chronicle entry. |
| (freshness sweep targets) | Modify | Chapters describing the frozen sky / position-blind observer, per a grep sweep. |

---

### Task 1: The single re-baseline

**Files:** every committed artifact under `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/generated/` that the CI step regenerates.

- [ ] **Step 1: Reconcile with `main` (the shared seam).** Confirm the worktree's real base and adopt the rendering thread's current tooling:

```bash
git merge-base HEAD main
git log --oneline main -5    # has the PNG-migration thread landed since this worktree was cut?
```

If `main` has advanced with rendering-thread work, merge it into this worktree (`git merge main`) and resolve any conflict in `.github/workflows/ci.yml`'s artifact step or the gallery PNGs in the rendering thread's favor for *format*, this campaign's favor for *content*. The map/PNG commands below must match whatever the merged CI step says.

- [ ] **Step 2: Read the authoritative command list.** Do not transcribe from this plan; read it live:

```bash
sed -n '/Artifacts are current/,/git diff --exit-code/p' .github/workflows/ci.yml
```

For reference, at plan-writing time that list is (verify against the file — the rendering thread may have changed the map/PNG lines):

```bash
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-ci-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-ci-locked.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-locked.json > book/src/gallery/almanac-seed-42-locked.md
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
cargo run -p hornvale -- phonology > book/src/reference/phonology.md
cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.png > book/src/gallery/elevation-seed-42.md
cargo run -p hornvale -- biome-map --world /tmp/hv-ci-sky.json --out book/src/gallery/biome-seed-42.png > book/src/gallery/biome-seed-42.md
cargo run -p hornvale -- biome-map --world /tmp/hv-ci-locked.json --out book/src/gallery/biome-seed-42-locked.png > book/src/gallery/biome-seed-42-locked.md
cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-sky.json --out book/src/gallery/settlement-seed-42.png > book/src/gallery/settlement-seed-42.md
cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-locked.json --out book/src/gallery/settlement-seed-42-locked.png > book/src/gallery/settlement-seed-42-locked.md
cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
cargo run -p hornvale -- lab run studies/census-of-the-meeting.study.json
```

Run the (verified) list from the repo root.

- [ ] **Step 3: Inspect the diff — is it the story the campaign should tell?** `git status` and `git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/`. Confirm the changes are exactly the expected ones, and nothing else moved:
  - `stream-manifest-generated.md`: adds `forcing` and `phase-offsets` labels (Plan 1). If absent, Plan 1's `stream_labels()` wiring did not land — stop.
  - `concept-registry-generated.md`: adds `eccentricity-mean` and `obliquity-amplitude` (Plan 1). If absent, stop.
  - `almanac-seed-42-sky.md`: the calendar's "Daylight swells to …%" line changed to the flagship's latitude (Plan 2), plus any Plan-1 calendar wording (day 0 is now an ordinary day — moon phases/season shifted off the grand alignment).
  - `almanac-seed-42-locked.md`: the salient-phenomena and Gods sections reflect a single hemisphere (Plan 2's culling), not both.
  - `almanac-seed-42.md` (constant sky): **expected byte-unchanged** (constant worlds have no generated calendar and a position-blind `ConstantSun`). If it changed, investigate before committing.
  - `census-lands-drift/`: a new `…-obliquity-range.svg` appears (Plan 1's metric) and `census-lands-drift-summary.md` lists it.
  - Maps (`elevation`/`biome`/`settlement` PNG+md): **expected byte-unchanged** (climate reads the `t = 0` obliquity, which equals the pre-campaign value). If a PNG changed and the rendering thread did *not* change its format, investigate.

- [ ] **Step 4: Verify idempotence (determinism).** Re-run the entire Step-2 list a second time, then:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: **no diff** on the second run (the first run already wrote the final bytes; a second regeneration changes nothing). A non-empty diff here means non-determinism — a catastrophic bug; stop and diagnose (do not commit).

- [ ] **Step 5: Full gate + commit.**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
# and the artifact drift check itself, now green:
git add -A book/src/gallery/ book/src/reference/ book/src/laboratory/
git status   # confirm only regenerated artifacts are staged
git commit -m "chore(book): re-baseline every artifact for Firm Ground II (the one re-baseline)

Regenerates the three seed-42 almanacs, the reference dumps (new forcing +
phase stream labels, new eccentricity/obliquity-amplitude predicates), and the
census-lands-drift + census-of-the-meeting studies (new obliquity-range metric)
— absorbing both halves' churn at once (spec §7).

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 2: The astronomy chapter to tier 2 + the model card

**Files:**
- Modify: `book/src/domains/astronomy.md`

**Interfaces:** none (prose). The chapter must read at tier 2 and the model card must state every new quantity in exactly one column (spec §9).

- [ ] **Step 1: Add the tier-2 narrative.** After the "Tiers 1–2 are live (Campaign 2)" paragraph, add two sections. Write at the book's altitude (mathematical, self-contained). Required content:

  **`## The moving sky (Firm Ground II)`** — the Milankovitch triad as slow functions of `WorldTime`:
  - Obliquity, eccentricity, and precession each become an oscillation over absolute standard days, anchored so that **every pre-existing element at `t = 0` equals its genesis draw** — the present sky is byte-unchanged; deep time is where the drift shows. Give the obliquity law `ε(t) = ε₀ + A_ε·(sin(2π·t/P_ε + φ_ε) − sin φ_ε)` and name the periods (~41 kyr obliquity, ~100 kyr eccentricity, ~21 kyr precession, expressed in standard days).
  - **Eccentricity is genuinely new** — a tilt-independent seasonal driver, so a zero-obliquity world now has apsidal seasons and therefore a year (SKY-2).
  - **Moon-coupled obliquity (SKY-21):** a large stabilizing moon damps the obliquity wobble; a moonless world's tilt swings wider over deep time — one moon draw with two coupled consequences.
  - **Per-body phase offsets (SKY-4):** genesis day 0 is no longer a grand alignment (every moon new, year and day beginning together); each body carries a drawn phase so day 0 is an ordinary day.
  - **Bright scope line:** the sky *drifts* the elements the domain already holds; it gains **no new bodies**. Nothing consumes the deep-time behavior yet — paleoclimate reads it in Campaign 20.

  **`## The placed observer (Firm Ground II)`** — the sky of a real place:
  - `ObserverContext` now carries a position on the globe; the default almanac observes from the **flagship cell**, so it reports the sky of a real place rather than of an omniscient nowhere.
  - **Latitude daylight (SKY-8):** daylight length follows the sunrise equation `cos H₀ = −tan φ · tan δ` with declination `δ(t) = ε(t)·sin(2π·year-phase)` — a flat half-day at the equator, running to polar day/night toward the poles. The almanac reports the flagship's own daylight range.
  - **Ever-visible hemisphere culling (SEQ-5):** a placed observer sees only what ever rises at their location. On a **spinning** world the sky turns, so every body rises and sets from every longitude — the whole sky is visible. On a **tidally locked** world the sky is fixed: the day hemisphere sees only the sun, the night hemisphere only the moons and stars, so a locked world's pantheon depends on which hemisphere its people settled.
  - **Bright scope line:** the observer sees a **hemisphere, not an altitude-azimuth sky** — a body is up or down with the hemisphere, not placed at a coordinate. Per-body altitude/azimuth and twilight (SKY-7), and how meaning-making *reacts* to the vantage, are later work.

- [ ] **Step 2: Extend the model card.** Update the three columns in `## The model card`:
  - **Derived (real formulas):** append — *the moon-coupling of the obliquity amplitude (a stabilizing moon damps the wobble); latitude daylight from the sunrise equation.* Adjust the existing "day/night geometry" clause to note it is now the observer's hemisphere.
  - **Approximated (declared):** append — *the Milankovitch drift laws (slow sinusoids at fixed near-real periods; no coupled climate feedback); ever-visible hemisphere culling (a body is up/down with the hemisphere, not placed at an altitude); the substellar point fixed on the prime meridian on locked worlds.* Note that eccentricity retires the "circular orbits" clause **as a genesis-only approximation** — orbits are still circular at `t = 0`, but eccentricity now oscillates over deep time.
  - **Drawn from the seed (or pinned):** append — *each forcing element's mean, amplitude, and phase (obliquity amplitude/phase, eccentricity mean/amplitude/phase, precession phase); each body's genesis phase offset (year, day, per moon).*
  - Add a fourth note after the three columns: **Derived from placement:** *the observer's position — the flagship's cell coordinate, deterministic because placement is.*

- [ ] **Step 3: Update the tier ladder.** In "The tier ladder ahead," fold the now-shipped items (obliquity drift, eccentricity, phase offsets, the placed observer) out of the "ahead" list; keep genuinely-future items (binaries, resonances, eclipses, wanderers, constellations, per-body altitude/azimuth SKY-7, per-neighbor coordinates SKY-12).

- [ ] **Step 4: Build + gate.**

```bash
mdbook build book
cargo test -p hornvale --test docs_consistency
```

Expected: the book builds; docs-consistency passes (no broken links introduced).

- [ ] **Step 5: Commit.**

```bash
git add book/src/domains/astronomy.md
git commit -m "docs(book): astronomy to tier 2 — the moving sky and the placed observer (spec §9)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 3: The chronicle entry, the summary link, and the freshness sweep

**Files:**
- Create: `book/src/chronicle/19-firm-ground-ii.md`
- Modify: `book/src/SUMMARY.md`
- Modify: freshness-sweep targets (grep-driven)

- [ ] **Step 1: Confirm the campaign number.** The chronicle is the authoritative order (decision 0017, forward-only):

```bash
ls book/src/chronicle/ | grep -E '^[0-9]+-' | sort -n | tail -3
grep -n "chronicle/1[0-9]-" book/src/SUMMARY.md | tail -3
```

The last entry is `18-the-meeting.md`, so this is **Campaign 19**. If the parallel rendering thread has already landed a `19-*.md`, use the next free number instead (20) — the chronicle numbering never rewinds.

- [ ] **Step 2: Write the chronicle entry.** Create `book/src/chronicle/19-firm-ground-ii.md`, matching the voice and shape of `18-the-meeting.md` (read it first). It must recount: the two deliverables (a sky that acquired a past; an observer that acquired a place); the drift-anchored `t = 0` identity (the present sky unchanged, deep time drifting); moon-coupled obliquity; day 0 made ordinary; eccentricity giving a zero-tilt world a year; the placed observer and the ever-visible culling (a locked world's pantheon now depends on its hemisphere); and the single re-baseline. State plainly that this campaign built **no consumer** — paleoclimate (Campaign 20) reads the forcing, the epistemic layer (Year 4) reads the vantage. Reference the specs and the three plans.

- [ ] **Step 3: Link it in the summary.** In `book/src/SUMMARY.md`, after the `Campaign 18: The Meeting` line (`:48`), add:

```markdown
- [Campaign 19: Firm Ground II](./chronicle/19-firm-ground-ii.md)
```

- [ ] **Step 4: Freshness sweep.** Find chapters the campaign made stale and correct them (the book may never lag merged reality — CLAUDE.md Process). Run:

```bash
grep -rniE "frozen|fixed obliquity|never seen to move|position-blind|nowhere in particular|places\[0\]|the old planet-wide|single number|both hemispheres" book/src --include=*.md | grep -v gallery/ | grep -v chronicle/
```

For each hit, decide: is the statement still true (e.g. tier-0's "fixed at zenith" is *correct* for the constant sun) or now stale (e.g. a claim that the sky is frozen for all worlds, or that the almanac observes from nowhere)? Correct the stale ones. Likely targets: any kernel/phenomena chapter describing `ObserverContext` as placeless; any overview claiming the sky is static; the sky gallery narrative (`book/src/gallery/the-sky.md`) if it asserts a frozen or position-blind sky. Leave tier-0 descriptions that are true of the constant sun alone.

- [ ] **Step 5: Build + full gate + the artifact drift check.**

```bash
mdbook build book
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
cargo test -p hornvale --test docs_consistency
# The campaign's drift check, now green end to end:
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: all green, and the drift check reports **no** diff (Task 1 already committed the regenerated artifacts; Tasks 2–3 touched only hand-written chapters, not generated ones).

- [ ] **Step 6: Commit.**

```bash
git add book/src/chronicle/19-firm-ground-ii.md book/src/SUMMARY.md
git add -u book/src   # any freshness-sweep edits
git commit -m "docs(book): Firm Ground II chronicle + freshness sweep (Campaign 19 close)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

## Self-Review Notes

**Spec coverage (against `2026-07-09-firm-ground-ii-design.md` §7, §9, §10):**
- §7 re-baseline exactly once (every committed artifact refreshed together) → Task 1, driven by the authoritative CI list, verified idempotent. §9 the book opens the astronomy chapter to tier 2 with the extended model card (each element's mean/amplitude/phase drawn; drift laws approximated; moon coupling derived; observer position derived-from-placement; the bright scope lines) → Task 2. §10 the re-baseline is clean and a second regeneration is byte-identical → Task 1 Step 4. DoD chronicle + freshness sweep → Task 3.

**Read-the-engine / read-the-repo findings that shaped the plan:**
- The CI "Artifacts are current" step regenerates the three almanacs, the reference dumps, the maps, and only two studies (`census-lands-drift`, `census-of-the-meeting`); `census-of-skies` is committed but **not** regenerated by CI — a frozen Y2 artifact. Regenerating it would introduce drift CI never checks; the plan leaves it untouched.
- The calibration battery (`windows/lab/tests/calibration.rs`) runs each study **fresh in-process**, so `cargo test --workspace` was green throughout Plans 1–2 despite the stale committed SVGs; only the CI `git diff` artifact check was red. This plan is what turns that check green — which is why the artifact drift check is part of *this* plan's gate but not Plans 1–2's.
- The astronomy chapter and its model card share one file (`book/src/domains/astronomy.md`), so the tier-2 rewrite and the model-card rows land in a single edit (Task 2).

**Volatility watch (standing rule 2, decision 0022):** the rendering thread shares this plan's two seams — the gallery PNGs and the CI artifact-command list. Task 1 Step 1 reconciles with `main` and adopts the merged tooling *before* regenerating, so the re-baseline is computed against the rendering thread's current map format, not a stale one. This is the campaign's designated merge-coordination point.

**No-placeholder check:** the regeneration commands are concrete (copied from CI, to be re-verified live); the model-card rows are enumerated by column with exact content; the chronicle and freshness-sweep steps name their targets and the grep that finds them. The one deliberate deferral to execution time is the exact map/PNG command text, which must come from the merged `ci.yml` rather than this snapshot — stated as such.
