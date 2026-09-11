# The Tidemark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A people can live in the sea, and the sea can stop supporting them.

**Architecture:** A third `HabitatRealm` variant gated exactly as the
subterranean realm is, seated across the five pelagic strata the way the
subterranean arm is seated across delve rungs; the Waterworld overlay built in
the `Settlements` rung so it has a consumer; six marine peoples distinguished on
shipped trait axes rather than by depth; a subsistence roster beneath them; and
vent succession reaching seating, so a failing vent ends an occupation.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only, `cargo nextest`,
`BTreeMap`/`BTreeSet`/`Vec` (no hash containers), kernel `Seed`/`Stream`.

**Spec:** `docs/superpowers/specs/2026-09-11-the-tidemark-design.md`

## Global Constraints

- Determinism is constitutional: same seed + pins → byte-identical output.
- No new stream label. `WATERWORLD_VENT = "waterworld/vent/v1"` is already
  registered and already in the committed manifest; vent draws are per-vertex
  sub-streams, so nothing may reorder an existing stream's consumption.
- No new external dependency. The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`.
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. `domains/species`
  may not read `domains/climate`; the two realm vocabularies meet only in
  `windows/worldgen`.
- No `HashMap`/`HashSet`; no wall-clock time. Enforced by `clippy.toml`.
- Every crate is `#![warn(missing_docs)]`; every public item, field and variant
  gets a one-line doc comment.
- `cargo fmt` is the final step before every commit. `make gate-commit` must be
  green before every commit; fmt-gate skips are the most common review finding.
- **No two marine kinds may differ only by stratum** (spec §3.4). Every pair must
  be separable on an axis the model already carries.
- **The underworld's trophic half is not ours.** Only the marine chemotroph is in
  scope; no `Surface`-realm kind gains a `CHEMOSYNTHATE` weight and no underworld
  kind changes.

## File map

- `domains/species/src/lib.rs` — `HabitatRealm` (the enum, ~2741; the registry,
  ~2766), `substrate_response` (~4740), `environment_niche_registry` (~6397),
  `biosphere_registry` (~3666), the locomotion store (~2856).
- `windows/worldgen/src/lib.rs` — `per_species_suitability_masked` (~1869) and
  its realm `match` arms (~2009, ~2418, ~8203); `build_to`'s `Settlements` rung
  (the gate at ~9145); `marine_chemosynthate_supply_field` (~1408).
- `windows/worldgen/src/waterworld.rs` — `waterworld_from` (~405), `WaterWorld`,
  `WaterWorldSnapshot`, `VentState`, vent admission (~490).
- `windows/worldgen/tests/suite/` — the realm agreement test (new file), the
  marine seating and placement tests, the measurement harnesses.
- `domains/species/tests/suite/` — the distinctness and classification tests.
- `docs/superpowers/ledgers/2026-09-11-the-tidemark.md` — rulings as they occur.

---

### Task 1: Stage 1 — The realm variant, and what the compiler says about it

**Goal:** `HabitatRealm::Marine` exists, every site that must handle it does, and
no world moves.

**Success criteria:** the workspace compiles with the new variant; `sea-elf` and
`giant-crocodile` carry explicitly stated realms; M4 measures zero movement;
seed-42 artifacts are byte-identical.

- [ ] Write M4 as a **two-arm test in one run**, not a before/after across the
  code change. `availability` is internal to `per_species_suitability_masked` and
  is never returned; the observable is `per_species_suitability`'s
  `Vec<(u32, VertexMap<f64>)>`, and `species_realm` is a **caller-supplied
  slice**, so one run can score `sea-elf` and `giant-crocodile` under the realm
  vector the registry yields and under a forced vector, and compare. Follow
  `windows/worldgen/tests/suite/deep_realm_rehome.rs`, which does exactly this
  (`k_live` vs `k_surface_forced`) — it is the shipped idiom for this question.
  Count vertices whose returned suitability is non-zero. Record both counts in
  the campaign ledger.
- [ ] Add `HabitatRealm::Marine` with its doc comment. Do **not** hunt for call
  sites by grep: `substrate_response`'s own doc says the `match` is exhaustive
  with no wildcard, so build the workspace and let the compiler enumerate them.
  Record the site list the compiler produced in the ledger; it is the task's real
  scope and nobody knows it in advance.
- [ ] For each site the compiler named, decide the marine arm. `substrate_response`
  gets the third curve: author `MARINE_OPTIMUM`/`WIDTH`/`DEVOTION` with `plumb:`
  prose arguing from the existing two — the subterranean devotion is 0.8 because
  "the habitat is the void and not the floor", and for a water-column kind the
  seabed is less accountable still, so devotion falls again. State the argument,
  not just the number.
- [ ] Write the two-way agreement test in a new `windows/worldgen/tests/suite/`
  file: `HabitatRealm` ↔ `climate::facets::Realm` is a **bijection**, asserted in
  both directions. Its doc comment must name the direction each assertion
  enforces — a one-directional gate is blind to over-admission and still reads as
  total. Verify it fails if a variant is added to either side without the other.
- [ ] Add `sea-elf` and `giant-crocodile` to `habitat_realm_registry` as explicit
  `HabitatRealm::Surface` rows, each with a comment carrying its own reason —
  sea-elf's is its shipped row ("a settled coastal people does not live entirely
  in the water… Not `ALREADY_BUOYED`"), giant-crocodile's is that it is
  land-dominant at 0.6 `ANIMAL_PREY`. The registry is sparse and absence already
  means `Surface`, so state in the doc why these two are nevertheless listed.
- [ ] Require M4's two arms equal. Equality is the prediction; any difference
  means the variant silently reclassified a shipped kind and must be resolved
  before the task closes, not noted. Because both arms run against the same tree,
  this test keeps working after the task — it is a permanent guard, not a
  one-off measurement.
- [ ] Run `make rebaseline` and `git diff --exit-code` over the paths in
  `docs/generated-paths.txt`. Branch table, because the response differs by what
  moved: **only `docs/audits/` moved** → the type-audit report drifted on the new
  pub variant; regenerate and commit in the same commit. **`book/src/gallery/`
  moved** → STOP, a world changed and this task was supposed to move nothing.
  **Nothing moved** → proceed.
- [ ] Run `make gate-commit`; record its exit code and duration in the ledger.
- [ ] Commit: `feat(the-tidemark): add the marine habitat realm`.

---

### Task 2: Stage 2 — Wire the overlay, gate availability, seat on the ladder

**Goal:** the Waterworld overlay is built by a real build path and a marine kind
can be scored against it.

**Success criteria:** `waterworld_from` has a non-test caller; marine
availability is a `{0.0, 1.0}` presence mask; seating scores the five pelagic
strata; M1 is answered with counts.

- [ ] **M1 first, because seating cannot be written until it is answered.**
  Count the vertices carrying `climate::Biome::HydrothermalVent` and the vertices
  carrying a `WaterVent`, and the size of their intersection, at seeds 42, 7 and
  3. Report all three counts per seed. The spec predicts the intersection is
  under half the smaller set; if instead the overlap is large, stop and report
  that as the headline — two representations of one phenomenon is a larger
  finding than this campaign, and seating should not be built on top of an
  unresolved duplication.
- [ ] Record M1's answer and the chosen authoritative representation in the
  ledger before writing any seating code.
- [ ] Construct the overlay inside `build_to`'s `Settlements` rung, before
  placement reads it. `waterworld_from(world, terrain, climate, config)` needs
  exactly what that rung already holds. Decide against the rung's existing shape
  whether `WaterWorldConfig { enabled }` survives as a knob or the overlay simply
  builds when the world has marine vertices, and record which and why.
- [ ] **Name the instant.** Seating reads vent phase, which is a function of
  `WorldTime`. Write down which tick placement reads and why, in the ledger and
  in a doc comment at the call. Then verify — do not assume — that the read
  consumes no draws: assert that building a world twice at the same seed and pins
  produces byte-identical output, and that the vent sub-stream's consumption is
  unchanged from before this task.
- [ ] Build the marine substrate field the way `subterranean_substrate_field` is
  built: hoisted unconditionally, read only for a `Marine` kind, pure, no draws.
  Its doc comment should say so, as the subterranean one does.
- [ ] Add the `Marine` arm to `per_species_suitability_masked`: `availability` is
  `1.0` where the vertex holds a water column and `0.0` otherwise, and it stays
  **outside** the Liebig minimum, exactly as the cave mask does — it is a presence
  mask, not a tolerance.
- [ ] Seat across the five strata of `Realm::WATERWORLD.strata()`, taking the
  best, mirroring the subterranean arm's loop over `Band::habitation()`.
- [ ] Prove the seating is not vacuous before believing it: perturb a real marine
  input and require a downstream difference, while freezing substrate and
  identity. Do not prescribe which input — read the code and find one that
  actually discriminates; a mutation that cannot move the result proves nothing.
- [ ] Run the existing Waterworld suite once with `--no-fail-fast` and read the
  whole failure list in one pass.
- [ ] Run `make gate-commit`; record exit code and duration.
- [ ] Commit: `feat(the-tidemark): build the waterworld overlay in the settlements rung`.

---

### Task 3: Stage 3 — The six peoples

**Goal:** triton, merfolk, abyssal elf, vent commensal, kelp tender and reef
mason exist, are distinct on axes the model carries, and place as predicted.

**Success criteria:** M5's minimum pairwise axis-difference is ≥ 1 excluding
stratum; M2 is two-sided green; M7 shows no map domination.

- [ ] Author the six kinds against the spec §3.4 table, each with its
  `biosphere_registry` row, `habitat_realm_registry` row (`Marine`),
  `environment_niche_registry` row scored against the marine names already in
  `domains/climate/src/axes.rs` (`reef`, `kelp-forest`, `vent`, `coral-head`,
  `kelp-canopy`, `smoker-field`, `vent-plume`, `abyssal-plain`, the open-water
  set), and whatever the compiler's coverage ratchets demand — several registries
  are TOTAL maps with their own coverage tests, and those tests are the
  enumeration of what a new kind owes.
- [ ] The **abyssal elf** follows drow's discipline exactly: its only authored
  separation from the elf family is the realm gate and its deep-band niche. Where
  drow's `elevation` response is wood-elf's byte for byte, the abyssal elf's
  inherited responses should likewise be its family's, and the doc comment must
  say that this is deliberate.
- [ ] The **vent commensal** weights `CHEMOSYNTHATE` and reads **the same**
  `marine_chemosynthate_supply_field` the `Surface` arm reads — not a marine
  twin. Nothing forces a second field (contrast `HabitatRealm` vs
  `climate::Realm`, which layering does force), and that field's doc is the thing
  reserving the consumer.
- [ ] **In the same commit that lands the vent commensal**, update two prose
  sites, because after this campaign they become true sentences producing a false
  conclusion: `marine_chemosynthate_supply_field`'s doc comment, and the
  `BIO-chemotrophy` row in `book/src/frontier/idea-registry.md`. Both currently
  say no `Surface`-realm kind weights `CHEMOSYNTHATE` so the supply reaches no
  consumer — which stays **literally true** once our consumer is `Marine`, so a
  reader checking whether rung 4 is open gets a true sentence and the wrong
  answer. Rewrite each to say the **marine half is closed by this campaign** and
  the **underworld half is open and belongs to THE TENANT**. Do not delete the
  sentence — a successor needs to find it. (Raised by campaign/the-ceiling, which
  is taking larder rung 3.)
- [ ] **Before authoring any kind, re-derive the arriving-axis table in spec
  §3.4 against the tree as you find it** — it was measured at Task 2's landing and
  is the constraint everything else in this task rests on. Confirm in particular
  that per-band `insolation` still reaches nothing and that `height_asl_m` still
  carries `-depth_m`. If either has moved, stop and report: the slate's
  differentiation depends on it.
- [ ] Differentiate the six on temperature, depth and `CHEMOSYNTHATE` only.
  **The kelp tender is the trap**: a phototrophic people of the photic zone is
  naturally light-differentiated, and light does not arrive. Separate it by
  shallow depth and temperature, and carry its phototrophy through `TrophicMode`
  and the `PHOTOSYNTHATE` weight — then verify that weight actually moves its
  score, rather than assuming it does.
- [ ] Write M5 as a **test**, not a one-off measurement: for all 15 pairs, count
  the model-carried axes on which the pair differs, excluding stratum, and assert
  the minimum is ≥ 1. It names the offending pair when it fails. This is the
  guard against the Delvers' defect and it must outlive the campaign.
- [ ] Run M2 two-sided: settlements **per marine kind** at seed 42 at
  `BuildDepth::Full`. Each of the five `Settled` kinds ≥ 1 and under the surface
  total; `merfolk` exactly 0. A non-zero for merfolk means `SocialForm` is not
  reaching placement — a defect a one-sided floor could not see.
- [ ] Run M7 at seeds 42, 7 and 1234 — the same three the sea-elf confinement was
  measured over — and compare the abyssal elf's held vertices against the family's
  order (wood-elf ~800, shelf-confined sea-elf ~1,425). If it is an order larger,
  apply the remedy that worked for sea-elf: band confinement with its own pinning
  test. Do not retune niche weights to hide it.
- [ ] **Revisit I4's wiring guard, which is currently a source-text scan.**
  `the_bake_hoists_the_vent_bearing_marine_habitat` reads `lib.rs` as text and
  matches the literal `EraInvariantSupply::build_at(`. It reddens on the real
  reversion (verified by mutation), but a change that keeps the string in a
  comment or dead branch while calling `build(` would pass it. The reason an
  outcome test was impossible was that **no real marine kind existed** — after
  this task, six do. Check whether a behavioural guard is now possible: a marine
  kind whose capacity differs measurably with and without the vent layer would
  redden on the reversion semantically. If it is possible, replace the scan and
  say so; if it still is not, record why in one sentence.
- [ ] Record M5, M2 and M7's actual numbers in the ledger. A measurement whose
  result is described rather than stated is not recorded.
- [ ] Run `make gate-commit`; then submit a stage gate:
  `make sluice-stage BRANCH=campaign/the-tidemark REF=<full-sha>`.
- [ ] Commit: `feat(the-tidemark): author the six marine peoples`.

---

### Task 4: Stage 4 — The subsistence roster

**Goal:** the six peoples eat something the world actually has.

**Success criteria:** M6 reports zero dangling requirements.

- [ ] Author the roster of spec §3.7: a kelp and a reef-building coral (both
  `SocialForm::Sessile`, which is "rooted; placed on the map, never agentified"),
  a bivalve bed and an urchin-analogue grazer, a schooling forage fish, a
  tube-worm analogue (Sessile, Chemotrophic) and a vent scavenger, and a
  detritivore. Follow `treant`, `shrieker` and `twig-blight` as the terrestrial
  precedent for flora as kinds.
- [ ] Hold the line between a named kind and an aggregate: `WaterStocks`'
  `plankton`, `chemosynthetic_bloom`, `nutrients` and `kelp_reef` stay fields.
  A kind is authored only where a people interacts with it as a thing. If a
  proposed kind has no such interaction, it does not get authored — say so in the
  ledger rather than adding it for completeness.
- [ ] Check whether `urchin-barren`, already a name in the environment basis,
  becomes a reachable state once the grazer exists. If it does not, say why; a
  name in the basis that no mechanism can produce is worth recording either way.
- [ ] Write M6 as a test: for each of the six peoples, resolve its subsistence to
  a named kind or an aggregate field, and assert zero dangling. Its failure
  message must name which kind owes what.
- [ ] Run the species coverage ratchets — several registries are TOTAL maps and
  will refuse an unauthored row. Fix the registries, never the ratchets.
- [ ] Run `make gate-commit`; record exit code and duration.
- [ ] Commit: `feat(the-tidemark): author the marine subsistence roster`.

---

### Task 5: Stage 5 — The habitat expires, the sea elves dive, and close

**Goal:** the campaign's headline is observable, and the campaign is landable.

**Success criteria:** M3 non-zero with its negative control at zero; sea-elf
reach extended without its residence moving; book, ledger and artifacts current.

- [ ] Write M3a's **negative control first**: the same world-time sweep with vent
  phase held constant must produce a count of zero. Written second it is a
  rationalisation; written first it is a control. Confirm it is zero before the
  live sweep is trusted.
- [ ] Run **M3a** (the mechanism, capacity side): over a world-time sweep at seed
  42, count vertices whose marine availability or capacity is non-zero at one
  instant and zero at a later one. A zero count falsifies spec §4 and is reported
  as the headline.
- [ ] Run **M3b** (the reachability, occupation side): count occupied vertices
  hosting a vent that transitions into `Failed` across the bake. **Task 2
  measured that this may legitimately be zero** — the vent layer reaches capacity
  but not siting, and two probe kinds placed four occupations each without one
  landing on a vent-improved vertex. A zero here is a reachability finding about
  the bake's siting, NOT a falsification of §4, and must be written in those
  words. Do not retune anything to make it non-zero.
- [ ] **Test the ending mechanism directly, regardless of M3b.** Construct an
  occupied vertex whose vent enters `Failed` and assert the occupation ends,
  rather than waiting for the bake to produce that situation naturally. A
  mechanism that is correct and rarely exercised still needs a test that
  exercises it; M3b measures how often the world does, which is a different
  question.
- [ ] Wire a failing vent to an ending: a vent entering `VentState::Failed`
  under an occupied vertex ends that occupation with `Ended::Nature` **and** a
  drawn cause at `OccupationRecord.core.cause` (note the hop through `core` —
  the field is on `Occupation`, not on the record). Map outright failure to
  `CauseOfEnd::Famine` and a people following a migrating vent to
  `CauseOfEnd::Migrated`; `flesh.rs` already pairs `Migrated` with a
  `Departure`, so read that before authoring.
- [ ] Do **not** author a new `CauseOfEnd` variant, and do **not** emit
  `Breached` — its doc restricts it to `Function::Mine`, "because only a working
  cuts rock". If you write any `match` on `CauseOfEnd`, it has **six** arms and
  no wildcard: a wildcard would silently classify a breached delving as an
  ordinary ending.
- [ ] Extend `sea-elf`'s **reach** without touching its residence: it already
  carries `SWIM`; express deep reach through locomotion and `Access::Dive`, not
  through vertices held. Then run
  `radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` and require it
  **green and unmodified** — if extending reach reddens it, reach has leaked into
  residence and the design is wrong, not the test.
- [ ] Add observation: ordinary output reports what the water is doing; diagnostic
  output names the inferred vent phase and marks it uncertain. Follow The Living
  Vent's split between present consequence and inferred cause.
- [ ] Regenerate artifacts (`make rebaseline`) and commit the drift with its
  cause. Expect movement here — placement changed, so worlds changed.
- [ ] Write the chronicle (`book/src/chronicle/the-tidemark.md`) and the
  retrospective (`docs/retrospectives/the-tidemark.md`), and do the book
  freshness sweep. Re-score any Confidence Gradient bet this campaign moved —
  that is a grep over `book/src/open-questions.md`, not a judgement about which
  chapters sound relevant.
- [ ] Update `docs/audits/campaign-reconciliation.tsv`: the row is `active` today
  and becomes `shipped` with its chronicle and retrospective paths at close.
- [ ] Update `WAT-sea-peoples` in the idea registry to `shipped`, pointing at the
  chronicle and spec. Leave `WAT-reef-fragmentation`, `WAT-signal-distortion` and
  the sea-peoples raider reading `raw`.
- [ ] Present the G6 package (post-G3 ledger digest, save-format entries first)
  and stop for Nathan.
- [ ] On approval: `make sluice BRANCH=campaign/the-tidemark REF=<full-sha>`.
