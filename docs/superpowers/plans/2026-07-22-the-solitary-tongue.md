# The Solitary Tongue Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Make language drift-rate a function of `SocialForm` × `lifespan` (a settled people drifts; a long-lived solitary is frozen), and give the three chromatic dragons a Draconic tongue that comes out as a conservative isolate.

**Architecture:** A language-owned `CascadeRegime` (a cascade-length range) threaded into `draw_cascade`/`build_lexicon`; the composition root (`windows/worldgen`) owns the map `(&BiosphereTraits) -> CascadeRegime` (reads `social_form` + `allometry::lifespan`) and a `cascade_of(world, species)` helper every direct cascade-caller adopts. Dragons gain an authored Draconic `ArticulationVector` + stopgap lexicon and join the speaker registries. Additive to the CLI reference pages; byte-identical for the peoples; not an epoch.

**Tech Stack:** Rust 2024, `cargo nextest`, `make gate`.

## Global Constraints

- **Byte-identity for the peoples; additive for the dragons; NOT an epoch.** Every kind that speaks today is `Settled → CascadeRegime (2,4)` — the current constant — so its draws are unchanged. Dragons draw from new labeled streams (`language/<dragon>/…`), perturbing no existing stream. The visible change is **new Draconic sections** in the CLI dictionary/phonology/audio reference pages + a Draconic cognate section — a **pure addition**, re-pinned by regeneration. No world (almanac/possession/census) changes; dragons are unplaced.
- **Layering:** `domains/language` depends only on the kernel and MUST NOT read `SocialForm`. The regime is passed *in*; `windows/worldgen` (composition root) computes it.
- **The regime map (authored, not fit):** `Settled → (2,4)` [UNCHANGED]; `Solitary` + `lifespan ≥ LIFESPAN_THRESHOLD → (0,1)` frozen; `Solitary` + short → `(2,4)` (banked); `Gregarious → (1,2)` (banked, no speaker). `Sessile` never speaks.
- **`draw_cascade(seed, species)` keeps its signature** as `draw_cascade_with_regime(seed, species, CascadeRegime::SETTLED)` so no existing caller changes behavior. Every caller that could see a dragon must instead route through worldgen's `cascade_of`.
- `serde`/`serde_json` only; no new crates; no `HashMap`. `#![warn(missing_docs)]`; `cargo fmt` before each commit. End commit messages with the `Claude-Session:` trailer.
- Task ordering: **1 → 2 → 3 → 4** is load-bearing (dragons must not speak until the regime + threading exist, or they draw a non-frozen cascade).

---

### Task 0: Re-verify the consumer table (no code)

- [ ] **Step 1:** From the merged base, run:
`grep -rn "articulation_registry()\|lexicon_registry()\|language_of\b\|lexicon_of\b\|draw_cascade\|sample_names_for\|placed_peoples" windows/ cli/ domains/ --include="*.rs"`
Confirm each speaker/cascade consumer's gate against this table; report any NEW consumer not listed (do not code yet — report to the controller):

```
consumer                                    gate today        after: dragons?
-----------------------------------------   ---------------   -----------------
cli dictionary / phonology / audio / repl   articulation      YES (additive Draconic)
render_cognates / language_diverges_in      articulation/fam  YES (draconic cognates)
windows/book (people pages)                 placed_peoples    NO (dragons unplaced)
worldgen settlement roster / naming         Settled           NO
lab metrics (language)                       specific kinds    NO ("goblin"/"kobold")
branches_coverage (cascade replay)          articulation      YES — must use cascade_of
chorus / language_diverges_in draw_cascade   direct            must use cascade_of
lexicon_of / build_lexicon                   —                 threads the regime
```

---

### Task 1: `CascadeRegime` + regime-aware cascade & lexicon (language)

**Files:** Modify `domains/language/src/etymology.rs` (regime type + `draw_cascade_with_regime`), `domains/language/src/lexicon.rs` (`build_lexicon` regime param). Add tests to each.

**Interfaces produced:** `hornvale_language::CascadeRegime { min: u32, max: u32 }` with `const SETTLED = {2,4}` and a constructor; `draw_cascade_with_regime(seed, species, regime) -> Cascade`; `draw_cascade(seed, species)` delegates with `SETTLED`; `build_lexicon(..., regime: CascadeRegime)`.

- [ ] **Step 1 (TDD):** In `etymology.rs` tests, write `draw_cascade_default_equals_settled_regime`: for several seeds/species, `draw_cascade(seed, sp) == draw_cascade_with_regime(seed, sp, CascadeRegime::SETTLED)`. Run — fails to compile (types absent).
- [ ] **Step 2:** Read the current `draw_cascade` + `CASCADE_LEN_RANGE`. Add `CascadeRegime` (a `{min,max}` range, `#[derive(Clone,Copy,Debug,PartialEq,Eq)]`, doc'd, with `const SETTLED: CascadeRegime = CascadeRegime { min: 2, max: 4 }`). Add `draw_cascade_with_regime(seed, species, regime)` = the current body but drawing `range_u32(regime.min, regime.max)`. Rewrite `draw_cascade` as `draw_cascade_with_regime(seed, species, CascadeRegime::SETTLED)`. **The stream derivation is unchanged**, so `SETTLED` is byte-identical.
- [ ] **Step 3:** Run the new test + the full `-p hornvale-language` suite — green (byte-identical default).
- [ ] **Step 4 (TDD):** In `lexicon.rs` tests, write `build_lexicon_default_regime_is_unchanged` (a `Settled`-regime lexicon equals a pre-change golden for one species). Then read `build_lexicon` (it calls `draw_cascade(seed, species)` at ~line 265). Add a `regime: CascadeRegime` parameter and pass it to `draw_cascade_with_regime` there. Update `build_lexicon`'s in-crate callers/tests to pass `CascadeRegime::SETTLED` (byte-identical).
- [ ] **Step 5:** `cargo test -p hornvale-language` green. **Commit** (`feat(language): CascadeRegime — drift-rate as a passed-in cascade-length range (the-solitary-tongue)`).

---

### Task 2: The regime map + `cascade_of`, threaded through worldgen

**Files:** Modify `windows/worldgen/src/lib.rs` (a `cascade_regime_of(&BiosphereTraits) -> CascadeRegime`; a `cascade_of(world, species)`; thread the regime into `lexicon_of_in`/`build_lexicon` calls).

**Interfaces consumed:** Task 1's `CascadeRegime`, `draw_cascade_with_regime`, `build_lexicon(..., regime)`; `hornvale_species::allometry::lifespan(mass, class)`; `wc.biosphere`.

- [ ] **Step 1:** Add `const LIFESPAN_THRESHOLD_YEARS` (authored; a value comfortably below a dragon's allometric lifespan and above the peoples' — read `allometry::lifespan` for the four peoples and the dragons at their authored masses to place it; document the two bracketing values in the comment).
- [ ] **Step 2 (TDD):** Write `cascade_regime_of` unit tests: each of the four peoples (`Settled`) → `CascadeRegime::SETTLED`; each dragon (`Solitary`, long-lived) → the frozen `{0,1}`; the map is total. Run — fails (fn absent).
- [ ] **Step 3:** Implement `cascade_regime_of(bio: &BiosphereTraits) -> CascadeRegime`: match `bio.social_form` — `Settled → SETTLED`; `Gregarious → {1,2}`; `Solitary →` (`lifespan(bio.mass, bio.metabolic_class) >= LIFESPAN_THRESHOLD` ? `{0,1}` : `SETTLED`); `Sessile → SETTLED` (never speaks; inert). Add `cascade_of(world, species)`: resolve the kind, read its biosphere from the assembled `wc`, return `draw_cascade_with_regime(&world.seed, species, cascade_regime_of(bio))`.
- [ ] **Step 4:** Thread the regime into lexicon derivation: in `lexicon_of_in`/wherever `build_lexicon` is called, compute `cascade_regime_of(bio_of_the_species)` and pass it. The four peoples resolve to `SETTLED` → byte-identical.
- [ ] **Step 5:** `cargo test -p hornvale-worldgen` green; the peoples' lexicons unchanged. **Commit** (`feat(worldgen): cascade_regime_of + cascade_of — drift = f(SocialForm, lifespan) (the-solitary-tongue)`).

---

### Task 3: The dragons gain a Draconic tongue

**Files:** Modify `domains/language/src/lib.rs` (`articulation_registry` + `lexicon_registry` — three dragon rows each). The `"draconic"` `family_proto` already exists (Eremite's forward-proto requirement) — verify, don't re-add.

- [ ] **Step 1 (TDD):** Write a species/worldgen test: the three dragons carry an `articulation` + `lexicon` row; `WorldComponents::assemble()` still passes `check_integrity` (speech ⊆ mind holds — dragons have psyche from The Eremite; `articulation.ids == lexicon.ids` holds with the dragons added to both). Run — fails (dragons absent from articulation).
- [ ] **Step 2:** Author a shared **Draconic `ArticulationVector`** for `white/red/black-dragon`: harsh and hissing — high `sibilance` (~0.9), high `voice_loudness` (~0.9), voiced (`voicing` ~0.7), `vowel_space` ~0.4, `labiality` ~0.2, `tonality` 0.0, `exotic` an `ExoticManner` fitting a hiss/growl if the enum offers one (read `ExoticManner`'s variants; else `None`). Same vector for all three (per-chromatic differentiation is deferred).
- [ ] **Step 3:** Author a **stopgap `Lexicon`** row for each dragon in `lexicon_registry` (mirror the four peoples' authored stopgap shape — read an existing entry; a minimal noun+rung set), so `articulation.ids == lexicon.ids`.
- [ ] **Step 4:** `cargo test -p hornvale-species -p hornvale-worldgen` green; `check_integrity` passes. **Commit** (`feat(language): the dragons speak — a Draconic articulation + lexicon (the-solitary-tongue)`).

---

### Task 4: Route every direct cascade-caller through `cascade_of` (the sweep)

**Files:** Modify `windows/worldgen/src/chorus.rs` (~1464), `windows/worldgen/src/lib.rs` (`language_diverges_in` ~3268), `cli/tests/branches_coverage.rs` (~144), `windows/lab/src/metrics.rs` (~3596 — only if it can see a dragon; if it iterates specific kinds, leave it).

**Interfaces consumed:** worldgen `cascade_of(world, species)` (Task 2).

- [ ] **Step 1:** For each direct `hornvale_language::draw_cascade(&…seed, species)` call, determine whether the caller can see a dragon (iterates articulation/family, vs. a fixed people). For those that can, replace the direct call with worldgen `cascade_of(world, species)` so a dragon gets its frozen regime consistent with its lexicon. In-worldgen callers (`chorus`, `language_diverges_in`) can call `cascade_of` directly; `branches_coverage` (cli) and `lab` call the worldgen re-export.
- [ ] **Step 2:** Run `grep -rn "hornvale_language::draw_cascade(" windows/ cli/` and confirm every remaining direct call is either a fixed-people/test path (byte-identical) or intentionally default; no dragon-reachable path uses the default regime.
- [ ] **Step 3:** `cargo test -p hornvale-worldgen && cargo test -p hornvale -p hornvale-lab` green (dragons build frozen cascades consistently). **Commit** (`refactor: route dragon-reachable cascade draws through cascade_of (the-solitary-tongue)`).

---

### Task 5: Frozen-Draconic + isolate-vs-family tests

**Files:** Add a worldgen/lab test (e.g. `windows/worldgen/tests/solitary_tongue.rs`).

- [ ] **Step 1 (TDD):** Assert on a real derived world (seed 42): (a) each dragon's evolved core forms differ from the shared `"draconic"` proto in **few** segments (frozen — the modern ≈ proto); (b) the three chromatics' mean inter-daughter distance is **below** the goblinoid family's (the isolate-vs-family contrast); (c) a byte-identity guard — a people's lexicon (e.g. goblin) is unchanged vs. a pre-campaign value. Use the existing divergence/homophony helpers where possible (read `windows/lab/src/metrics.rs` / the branches study for the distance function).
- [ ] **Step 2:** Run green. **Commit** (`test: the Draconic isolate is frozen, and diverges less than the goblinoid family (the-solitary-tongue)`).

---

### Task 6: Additive-artifact re-pin + final gate

- [ ] **Step 1:** `make gate` (timeout 3600000) — green.
- [ ] **Step 2:** `bash scripts/regenerate-artifacts.sh` then `git status`/`git diff`. **Verify the diff is a PURE ADDITION**: the CLI dictionary/phonology/audio pages + the cognate section gain Draconic; **every existing people's row/section is byte-identical** (inspect the diff — no people's content changes). No world/almanac/possession/census artifact changes. If any *existing* people content moved, STOP — the `Settled → (2,4)` byte-identity was broken; investigate.
- [ ] **Step 3:** Commit the re-pinned artifacts (`docs(book): re-pin — the dictionary/phonology/audio pages gain Draconic (the-solitary-tongue)`).

---

## Definition of Done (close — via `closing-a-campaign`)

- Absorb main; **re-run gate + regen** on the merged tree (byte-identity for peoples, additive for dragons).
- Chronicle `book/src/chronicle/the-solitary-tongue.md` + SUMMARY; retrospective; decision (spec §8, ID at close).
- **Flip BIO-37 → shipped**, Where → this chronicle/spec.
- Confidence Gradient check (likely no bet moved). Push main.

## Self-Review notes

- **Spec coverage:** §3.1 regime map → Task 2; §3.2 layering/threading → Tasks 1–2; §3.3 dragons speak → Task 3; §3.4 payoff → Task 5; §4 additive/consumer table → Task 0 + Task 6; §6 tests → Tasks 1/2/3/5. §7 non-goals → DoD/deferred.
- **Ordering:** 1 (regime) → 2 (map/thread) → 3 (dragons speak) → 4 (route callers) → 5 (tests) → 6 (regen). Dragons cannot speak (Task 3) before the frozen regime + threading (Tasks 1–2) exist.
- **The lesson:** Task 0 re-verifies the consumer table on the merged base *before* any code; Task 4 routes every dragon-reachable cascade draw through `cascade_of`; Task 6 proves the artifact diff is pure addition.
