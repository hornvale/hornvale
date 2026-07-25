# The Retainer — implementation plan

**Goal:** derive a world's terrain/climate providers once per scope instead of
repeatedly. Fix A (universal within-world dedup) + Fix B (possess session reuse)
+ Fix C (sky memo). Byte-identical throughout.

**Execution:** controller-led (byte-identity-critical; Fix A's safety is
by-construction and the exact `climate_from` call must be preserved), with the
committed **possession transcripts regenerating byte-for-byte** as the gate and
an independent whole-branch review before merge.

## Global Constraints (from the spec)

- **Byte-identical.** `climate_of(world) ≡ climate_from(world, &terrain_of(world))`
  — Fix A preserves it by construction. Every fix returns identical providers.
- Not an epoch (providers derived, never serialized). No draw/label/format change.
- Gates: `lens_purity`; possession transcripts + all artifacts byte-unmoved;
  full `make gate`; census drift (out-of-band).
- No `HashMap`/`HashSet`; `#![warn(missing_docs)]`; type-audit; `cargo fmt`.

---

### Task 1: Fix A — terrain+climate dedup at every site

**Files:** `windows/locale/src/lib.rs` (`LocaleContext::build`), `windows/lab/src/metrics.rs`
(`WorldView::build`), + any other `terrain_of`+`climate_of` pair (audit).

- [ ] **Step 1 — audit:** grep the workspace (non-test) for sites that call both
  `terrain_of` and `climate_of` on the same world. List them.
- [ ] **Step 2 — swap:** at each, `let terrain = terrain_of(world)?; let climate =
  climate_from(world, &terrain)?;` (reuse the single terrain). Where the site
  already holds a terrain, pass it to `climate_from` and drop the `climate_of`.
- [ ] **Step 3 — verify byte-identity:** `cargo test -p hornvale --test lens_purity`;
  `cargo test -p hornvale-lab` (WorldView metrics) and `-p hornvale-locale`;
  regenerate the possession transcripts + census-adjacent artifacts and `git diff`
  (must be empty).
- [ ] **Step 4 — commit:** `perf(worldgen-consumers): derive terrain once + climate_from (The Retainer Fix A)`.

### Task 2: Fix B — possess session-scoped provider reuse

**Files:** `windows/vessel/src/session.rs` (+ `windows/locale` if the residual is there).

- [ ] **Step 1 — diagnose:** with the profiling binary, profile `possess` after Fix A
  and find the residual `terrain_of` calls (the profile showed ~2.7 s; Fix A removes
  the `LocaleContext` double — locate the rest: the world-load path, per-`vantage`
  derivation, or NPC derivation). Confirm with a caller analysis of `terrain_of`.
- [ ] **Step 2 — reuse:** ensure the session derives the frozen world's providers
  **once** and threads them (the session already holds `LocaleContext` with
  terrain+climate — eliminate any *other* derivation of the same world's providers).
- [ ] **Step 3 — verify:** possession transcripts byte-unmoved; re-measure `possess`
  wall-clock (target: well under the 5.4 s baseline).
- [ ] **Step 4 — commit:** `perf(vessel): reuse the session's providers, don't re-derive (The Retainer Fix B)`.

### Task 3: Fix C — sky_report memo for the frozen day

**Files:** `windows/vessel/src/session.rs` / `vantage.rs`.

- [ ] **Step 1 — memo:** the possession day is frozen (`self.day`); compute
  `sky_report(world, day)` once for the session and reuse in `observable` instead
  of per-observation. Byte-identical (same day → same sky).
- [ ] **Step 2 — verify:** transcripts byte-unmoved; possess re-measured.
- [ ] **Step 3 — commit:** `perf(vessel): memoize sky_report for the frozen possession day (The Retainer Fix C)`.

### Task 4: Close

- [ ] Chronicle (`book/src/chronicle/the-retainer.md` + SUMMARY): the interactive
  seam was the slowest path; providers re-derived because `climate_of` rebuilds
  terrain and nothing is scoped; the fix is derive-once-per-scope, not a cache;
  byte-identical; the possess speedup.
- [ ] Retrospective: profile-what-you-run found it (possess unprofiled); the
  matrix/cyclicity insight (a cross-world cache helps the one-shot census nothing);
  Fix-A-byte-safe-by-construction (the API already had the seam). Promote followups.
- [ ] Whole-branch review; absorb origin/main; full `make gate` + artifact drift;
  census drift is out-of-band. G6 hard-stop.

## Self-Review

**Spec coverage:** Fix A → T1; Fix B → T2; Fix C → T3; DoD → T4. Complete.
**Placeholders:** T2 Step 1 is a genuine diagnosis step (the residual isn't yet
located) — flagged as investigation, not a hidden gap. **Type consistency:**
`terrain_of`/`climate_from`/`sky_report`/`LocaleContext`/`WorldView` match the spec.
