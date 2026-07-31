# The Weir — derivation at named sites only

**Campaign:** the-weir
**Date:** 2026-07-31
**Status:** Draft — awaiting G3 review
**Thesis:** The Shuttle fixed the callers; the pulpit still stands. The
`_of` convenience readouts that embed a full derivation invited the next
sin before the last one was merged — `Session::start` runs the demography
fit **three times**. A weir forces the whole flow through one measured
channel: delete the derivation-embedding readouts, lint the remaining
derivation entry points down to named construction sites, and write the
principle into the decision log so the slow path stops being representable.

## 1. Evidence (measured 2026-07-31 on lefford, quiet, frame-pointer dev builds)

Post-Shuttle baseline (`test-baseline-lefford.tsv` at b09f408d): suite
total 10 807.9 s; vessel is now the largest crate (4 514 s / 129 tests),
then worldgen (2 421 s), lab (1 832 s), book (771 s). A Full generated
build costs **3.25 s** (re-measured, `profile_build`, 2 seeds / 6.496 s).

| Test profiled | perf result |
|---|---|
| vessel `possession_moves::grievance_accumulates…` (95.7 s) | **`Session::start` 84.3 %**; inside it `demography_report_with_beta_from` **62.5 %** — reached THREE times: `predator_pressure` 25.0 %, `prey_pressure` 24.7 %, `wild_concentrations` 25.0 %, each running its own full fit (`coexist::pack` 25.5 %, `niche_per_species_k` 18.0 %, condense ~19 %); the un-threaded start sites re-sculpt (`terrain_of` 17.6 %, `globe::generate` 20.1 %). The world build itself is 11.4 %. |
| lab `health_calibration … no_chronic_distress` (81–97 s) | sim loop 84.3 %: A* 32.1 % + room addressing (`NearestCellIndex::scan_at` 7.2 % self, `child_at_scale`/`orient`/`decode` ~9 % self) — legitimate navigation work; the same triple fit ~28 % (prey 11.2 % + predator 11.1 % + wild 11.3 %); `kernel::tick`'s per-tick ledger clone measured **~2 %** (memmove) — NOT a monster, kernel stays untouched. |
| worldgen `doctrine::the_soc1_gate…` (203.0 s) | **86.99 % inside the test's own `generated()` helper** — the test's negative arm builds **61 Full worlds** (seeds 1..=60 + seed 1) at 3.25 s each to find ~11 folk flagships; the readouts are ~13 s. The test's own panic message already prescribes the alternative: *"Add a synthetic-society unit test driving doctrine_from directly against a hand-built world whose flagship's committed cult-form is 'folk' instead of relying on this sweep."* |

The pattern The Shuttle predicted: fixing call sites without removing the
convenience forms leaves every future feature one innocent-looking call
away from re-committing the sin. `predator_pressure(world)`,
`prey_pressure(world)`, and `wild_concentrations(world)` are exactly
`terrain_of`-shaped — each embeds a fit that costs more than a world
build — and all three were written *after* the artifact-taking idiom
existed.

## 2. Design

### Stage 1 — the weir itself (unrepresentability)

1. **Delete the sculpting/fitting `_of` readout wrappers** in
   `hornvale-worldgen`: the chorus family's `_of` forms
   (`account_params_of`, `cyclic_beliefs_of`, `doctrine_of`,
   `doctrines_of`, `day_schema_of`, `noun_class_of`, `observations_of`,
   `ladder_of`, `crisis_of`, `accounts_of`), `lexicon_of`, `exposure_of`,
   and the demography-embedding forms (`predator_pressure`,
   `prey_pressure`, `wild_concentrations`, `demography_report`,
   `demography_report_with_beta` — each gains/keeps an artifact-taking
   form; the bare-`(world)` forms go). Absence is the strongest
   unrepresentability. The `_from` signatures stay as they are (loose
   parts; no rename — the `_from` suffix becomes vestigial and a later
   quiescent-window sweep may strip it, parked in the ledger).
2. **Migrate every remaining caller** (grep-driven): `cli/src/dictionary.rs`
   (~13 sites), `windows/lab/src/metrics.rs` (~10), `windows/lab/tests/`,
   the remaining `windows/worldgen/tests/` callers, vessel/CLI fallback
   arms (the `(None) => wrapper` splits die with the wrappers — a
   `Session` whose artifacts failed to build cannot render the chorus
   surfaces, which is truthful: those surfaces ARE derivation).
3. **Lint the constructors.** The sanctioned derivation entry points —
   `terrain_of`, `climate_from`, `build_world*`,
   `demography_report_from`, `WorldComponents::assemble` — get
   `clippy.toml` `disallowed-methods` entries (the HashMap-ban precedent),
   with scoped `#[allow(clippy::disallowed_methods)]` + one-line
   justification at each named construction site: the composition root's
   build path, `Session::start`, lab's `FullView::build`, CLI command
   entries, and test fixtures. The `#[allow]` attribute at the site IS the
   sanctioned-site list — one source of truth, greppable.
4. **The decision record** (`docs/decisions/0092-…`): "derivation happens
   at named construction sites; readouts take artifacts" — the durable
   statement, with the lint named as its mechanical arm and this spec's
   evidence as its grounds.

### Stage 2 — one fit per start (the motivating sin)

`Session::start` (and lab `simulate_world`'s setup) computes ONE
demography report and threads it: `predator_pressure_from(report, …)`,
`prey_pressure_from(report, …)`, `wild_concentrations_from(report, …)`
become derivations over a shared fit (their `(world)` forms die in
Stage 1). Thread `Session::start`'s remaining un-threaded sites
(`LocaleContext::build` gains an artifact-taking form in
`windows/locale`; the derive-NPC path reuses the session's artifacts).
Expected from the profile: the triple fit's ~62 % becomes ~21 %, the
start-path sculpts (~18 %) vanish; a vessel session test drops roughly by
half, and vessel is 42 % of the suite.

**Byte-identity caveat (leads G3):** the three pressure functions today
each run `demography_report_from` with identical inputs — sharing one
report is byte-identical *iff* the fit is deterministic in its inputs
(it is: pure derivation over world+wc+terrain+climate, no draws — to be
verified by the cross-binary sweep, not assumed). No stream consumption
changes anywhere.

### Stage 3 — the soc1 sweep (61 builds → 3)

Implement exactly what the test's own panic message prescribes: a
synthetic-society unit test driving `doctrine_from` against a hand-built
world whose flagship's committed cult-form is `"folk"` (deterministic
negative-arm coverage, zero builds), plus keep the live positive arm
(seed 1, both gate arms) and ONE live folk smoke (seed 56, the known folk
flagship, documented as epoch-sensitive). The 60-seed hunt goes. This is
a test-breadth reduction and is flagged for Nathan at G3: the sweep's
property coverage (doctrine iff organized, across seeds) narrows to
{seed 1, seed 56, synthetic}; the wide sweep existed to *find* a folk
instance, which the synthetic world now supplies by construction.

### Explicitly accepted (closes a Shuttle followup)

`WorldComponents::assemble()` stays per-readout: it is registry
construction measured in milliseconds (the whole culture+religion+species
stage is 0.032 s), and threading `wc` through every `_from` signature is
churn without a measured win. The Shuttle's recorded deviation is hereby
resolved as **accepted-with-measurement** in the decision record.

### Out of scope (followup register)

A*/room-addressing precompute (~32 % of health — legitimate sim work,
own campaign); scene-window threading (the Casement line);
`kernel::tick`'s clone (measured ~2 %); the broader build-volume audit
(which other tests over-build — soc1 is the surgical instance); the
`_from` suffix rename sweep.

## 3. Determinism and save-format analysis

Zero new draws, zero stream-order changes, zero serialization changes.
Deleting pub functions is API surface, not save format. The shared-fit
refactor reorders no floats (one fit's outputs feed three consumers that
each ran an identical fit). The synthetic-society world in Stage 3
commits facts through the normal registry path in a test; it writes
nothing. Acceptance bar unchanged from The Shuttle: stage cross-binary
references FIRST (world-42 cmp, the three `book` lenses, plus one vessel
transcript — a scripted `possess` session diffed across binaries, since
vessel is where Stage 2 lands), `make rebaseline` NO-DRIFT, health pins
byte-identical.

## 4. Success criteria (testable)

1. **Unrepresentability:** `grep -rn` for the deleted names returns zero
   product hits; `clippy.toml` denies the constructors outside sanctioned
   `#[allow]` sites; the workspace compiles with `-D warnings`; the
   decision record exists. A NEW readout embedding a derivation cannot
   compile without adding a greppable `#[allow]` — demonstrated in the
   spec review by a scratch attempt that clippy rejects (then reverted).
2. **Per-test (lefford, solo, dev):** `grievance_accumulates…` 95.7 s →
   **< 45 s**; `the_sky_follows_the_walker` 85.4 s → **< 45 s**;
   `health … no_chronic_distress` ~81 s → **< 65 s**; `the_soc1_gate…`
   203 s → **< 15 s**.
3. **Suite total:** ≥ 20 % below the post-Shuttle 10 807.9 s baseline on
   the same measurement (quiet-lefford `make ci`, intersection sum),
   ledgered.
4. Byte-identity: §3's sweep, all IDENTICAL / NO-DRIFT.
5. A falsified prediction is a finding; misses ship un-retuned with the
   post-fix flamegraph in the chronicle.

## 5. Testing strategy

The existing suite pins bytes everywhere and is the harness. New tests:
the synthetic-society negative-arm unit test (Stage 3, prescribed by the
test it replaces); a clippy-rejection demonstration (Stage 1, transient,
documented in the task report rather than committed). The equivalence
tests from The Shuttle keep pinning `_from` behavior; the wrappers they
compared against die, so those tests simplify to direct `_from` pins —
assertions preserved, wrapper arms dropped.

## 6. Risks

- **Widest-churn campaign yet** (deletions touch cli, lab, worldgen tests,
  vessel, book): mitigations — absorb per task; the-pigment/the-watershed/
  the-shibboleth are live and worldgen-adjacent, so read their chronicles
  before absorbing and expect semantic conflicts, not just textual.
- **The fallback arms die with the wrappers** (vessel `consult`/`write`,
  CLI): the degraded path becomes "no chorus surface on a world whose
  artifacts fail to reconstruct" — behavior change ONLY on worlds with
  malformed pin facts (unreachable for built worlds; the-shuttle's review
  established the same posture). Stated here so G3 sees it.
- **Stage 3 narrows test breadth** — flagged above, Nathan's call.
- **clippy.toml `disallowed-methods` granularity**: it bans by path
  workspace-wide; the sanctioned sites carry scoped allows. If clippy's
  resolution of re-exported paths proves flaky (worldgen re-exports chorus
  fns), fall back to the enforcement-test pattern
  (`cli/tests/architecture.rs`-style source scan) — same single-list
  discipline, decided at plan time with a verified probe, not assumed.

## 7. Decisions (promoted from the autopilot ledger)

- **G1 — layered weir over lint-only or deprecation:** delete the
  wrappers (absence beats any lint), lint the constructors, record the
  principle. 2 ideonomy passes, 0 overturns; the lab's
  `FullView`/`SettlementView` ladder is the acknowledged view-type
  precedent; the `_from` rename is parked.
- **Q — Session triple-fit fix is Stage 2** of this campaign, not a
  separate one: it is the weir's motivating evidence. 1 pass, 0 overturns.
- **Q — kernel `tick` clone deferred WITH its measurement** (~2 %), so the
  suspicion doesn't recur unmeasured. 1 pass, 0 overturns.
- **Q — siting: lefford**, by the-shuttle's precedent.
