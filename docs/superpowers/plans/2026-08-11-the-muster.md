# The Muster Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the biome-affinity level a guard that can actually see it, and
widen `founder_handle` so a founder's identity cannot silently be someone
else's — the second as a `/v2` epoch, with the drop backstop retained beneath
it.

**Architecture:** Two independent halves, sequenced. **Part A** repairs
`windows/worldgen/tests/beta_calibration_freeze.rs`, whose component set holds
zero affinity rows and therefore cannot redden on any affinity-level change,
and makes it *name the roster it counts* — because the band's verdict is
decided by the roster, not the physics. **Part B** widens
`domains/history/src/flesh.rs::founder_handle` to fold its `founded_from` and
`ended_by` referents by their **material** keys (never their ids), which
renames every founder in every world and is therefore an epoch. **Part C** —
recording that the level has three consumers, not one — is not a task; its
evidence is produced by Part A's positive control and is written down in the
task that produces it. Part A runs first so the guard work is measured against
a stable identity space (spec §8).

**Tech Stack:** Rust 2024. `hornvale-history` (domain), `hornvale-species`
(domain), `hornvale-worldgen` (composition root), `hornvale-demography`. No new
dependencies.

Spec: `docs/superpowers/specs/2026-08-11-the-muster-design.md` (binding on
intent; **not** on line numbers — see "What drafting verified" below).
Ledger: `.superpowers/sdd/decision-ledger.md` (git-ignored; promote into the
retrospective before teardown).

---

## Global Constraints

Every task's requirements implicitly include this section.

### Project-wide

- **Dependencies:** `serde`, `serde_json`, `libm` only, workspace-wide. No new
  crates. The allowlist is `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only, enforced
  by `clippy.toml` `disallowed-types`. Float sorting uses `total_cmp`.
- **No wall-clock time anywhere, including in test code.**
  `std::time::Instant` is banned workspace-wide.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment.
- **Rust edition 2024. `cargo fmt` is the final step before every commit.**
  Fmt-gate skips are this project's most common review finding.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate
  depends on `hornvale-kernel` and nothing else. This is why founder promotion
  lives in `windows/worldgen/src/person_promote.rs` and not in
  `domains/person` (that file's own module doc, lines 1–6).
- **A seed loop needs a `/// claim:` doc comment.** Decision 0093, enforced
  default-deny by `cli/tests/claim_shape.rs`: any `#[test]` whose body iterates
  seeds (a `for` over a seed-shaped binding, a seed-shaped closure parameter,
  an ALL-CAPS `SEEDS`-like constant, or a `map_seeds` call) must carry a
  `claim:` tag in the doc block directly above the function. Copy the shape
  from `windows/worldgen/tests/founder_collision.rs:64-65` and `:83-86`.
- **The heavy-tier ignore reason is matched VERBATIM.** If any task adds an
  `#[ignore]`, the only reason string that keeps it inside `make gate-full` is,
  character for character (`cli/tests/heavy_tier.rs:63-64`):
  ```
  heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full
  ```
  Any other reason string must be added to `EXPECTED_UNTOKENISED` in the same
  file, which is a review decision, not a convenience.
- **`git commit` commits the WHOLE index.** Every commit in this plan uses
  `git commit -F <msgfile> -- <explicit paths>`.
- **Never run a census.** Never set `HV_CENSUS=1`. `scripts/census-run.sh`
  fails closed on the hostname; the one refresh is at the close, on lefford,
  authorization-gated (decisions 0063 / 0079 / 0081 / 0086). Budget ~15 min.

### This campaign's own

- **Part B is an EPOCH.** Deliberate regeneration uses an epoch suffix, never a
  rename (decision 0006). Two consequences that bind:
  1. **A superseded derivation stays declared.** The precedent is
     `domains/language/src/streams.rs`'s `V2`: *"**Retired** by `V3` (The
     Wearing, 2026-07-27) but never deleted — an epoch is a save-format
     contract, so a superseded leg stays declared."* The v1 `founder_handle`
     is retained, marked superseded, and is what Task 4's comparison arm reads.
  2. **Every committed artifact carrying a founder identity regenerates in the
     commit that changes the key** — not at the close. `make rebaseline` plus
     the seven-path `git diff --exit-code` runs inside Task 3.
- **The drop backstop STAYS.** Even the best-scoring key leaves **2 residual
  whole-record pairs in 3000 worlds** (spec §4.1). Any change that deletes the
  `kept_by_handle` drop in `person_promote.rs:161-169`, or restores the
  `assert!` it replaced, is wrong. The post-condition assert at
  `person_promote.rs:188-198` also stays — it is a post-condition, not the
  backstop.
- **Non-goals (spec §2), all four decided in advance.** No affinity-level
  split (refuted). No β re-sweep — if a result implies β must move, that is a
  **finding**, not an action. No repair of `BIO-40`'s diversity band — this
  campaign makes the guard *able to see*; it does not make the band pass. No
  preference row is re-authored; affinity **shapes** are untouched throughout.
- **`make rebaseline` does not touch byte-goldens.** `make rebaseline-goldens`
  does (it sets `REBASELINE=1`). Any task that moves a world runs **both**,
  then the seven-path diff:
  ```
  book/src/gallery/  book/src/reference/  book/src/laboratory/  docs/audits/
  docs/digest/  book/src/domesday/  clients/game/core/tests/fixtures/
  ```
- **`make rebaseline` exits 2 while a schema mismatch is outstanding**, and
  `set -e` then kills the five generators after it (The Radiation
  retrospective §5). Read the exit code *and* the log; a non-zero exit means
  "some generators did not run", not "nothing drifted".
- **`make gate` fail-fasts.** To see the whole red list in one pass, the
  campaign's suite command is:
  ```
  HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-muster-<task>.txt
  ```
  Run once, grep the file. Never re-run the suite to read a second line.
- **Post to the board before a long write and after a hard-won fact:**
  `make board-post KIND=technique NOTE='…' PATHS='domains/history/ windows/worldgen/'`.

---

## How this plan is written, and what that obliges you to do

The Radiation shipped **twenty-four defects, every single one in
controller-authored plan or brief text**, and none in implementer code
(`docs/retrospectives/the-radiation.md` §"The spine"). What caught them was
every plan step demanding executable proof, plus implementers who overrode the
plan and said so. Three rules follow, and they bind both sides:

1. **This plan states decision rules, not predictions.** Where a step measures
   something, it gives a branch table over the possible results. If you find a
   step that says "expected: X" without a branch for "not X", treat that as a
   plan defect and say so in your commit message.
2. **This plan never prescribes a specific mutation.** Where a positive control
   is required, the plan names the **property** the mutation must demonstrate.
   You find the mutation, by reading the code. Every mutation The Quire's plan
   prescribed from outside the code was a null; the implementer found a
   discriminating one by hunting, every time.
3. **You may override this plan.** If reading the tree shows a better shape
   than the one written here, take it and record the deviation in the commit
   message under a `PLAN DEVIATION:` line. A blocking implementer is what this
   process is for (Radiation retrospective §7).

---

## What drafting verified against the tree, and one spec claim that is wrong

Checked at `e78614c9` (branch `the-muster`), because main has moved since the
spec's evidence was gathered.

**Verified true:**

- `windows/worldgen/tests/beta_calibration_freeze.rs:180-211`'s
  `peopled_components()` calls `WorldComponents::from_stores(...)` with **five**
  trailing `ComponentStore::new()` (lines 204–208). Against
  `windows/worldgen/src/components.rs:121-134`, parameters 9–13 are
  `deity`, `culture`, `material`, `habitat_realm`, `biome_affinity`. So the
  guard holds **zero affinity rows** — the spec's §1.2 claim is exact — **and
  zero realm rows as well**, which the spec does not mention and Task 1 must
  handle.
- The affinity reach: `wc.biome_affinity` is read at
  `windows/worldgen/src/lib.rs:1747-1753` inside
  `demography_report_with_beta_from` (`:1710`, `pub(crate)`), feeding
  `per_species_suitability` → `per_species_k` → `coexist::pack` (`:1778`) →
  `byproducts` (`:1785`). `demography_report_from` (`:2050`) is that function
  pinned to the frozen `BETA`/`FLOOR`, and is what the guard calls. So a live
  store does reach `byproducts.strife`, which is what `claimed_diversity`
  averages.
- `domains/history/src/flesh.rs:137` `pub fn founder_handle(occ:
  &OccupationRecord) -> RoleHandle`, mixing `people` bytes, `site`,
  `founded.to_bits()`, `ended` (or `u64::MAX`), `peak_population`, and the
  discriminant `FOUNDER_ROLE` (`:69`). Its docstring carries The Radiation's
  dated correction — that decision 0051 forbids keying on an id **as a value**,
  not on a referent's own material facts — struck through rather than deleted.
- `windows/worldgen/src/person_promote.rs`: `select_founders` at `:130`;
  the handle as the fourth ranking key at `:156`; the drop backstop
  (`kept_by_handle` → `unremembered`) at `:141` and `:161-169`; the
  post-condition assert at `:188-198`; `FounderCast { remembered, unremembered }`
  at `:66-73`.
- `domains/history/src/record.rs`: `layer_key` `:255`, `FoundingCoords<'a>`
  `:308`, `founding_coords(&Occupation) -> FoundingCoords<'static>` `:318`,
  `material_key(&Occupation) -> u64` `:362`,
  `founding_key_from(own: FoundingCoords<'_>, parent: Option<FoundingCoords<'_>>) -> u64`
  `:398`, `founding_key(&Occupation, Option<FoundingCoords<'_>>) -> u64` `:424`.
  These are the material-fold precedents Part B builds on.
- `cli/tests/heavy_tier.rs:63-64` — the verbatim token, quoted above.
- `book/src/frontier/idea-registry.md:572` already reads
  `BIO-affinity-level-is-two-quantities … | rejected |`. The DoD's "first
  commit" item is **already discharged** by the spec commit `e78614c9`; Task 6
  verifies rather than repeats it.

**One spec claim is wrong against the current tree, and it matters:**

> Spec §1.3 cites
> `windows/worldgen/tests/descent_graph.rs::founder_handles_are_free_of_the_entity_id`
> as the test pinning that `founder_handle` excludes the entity id.

That test does not touch `flesh::founder_handle` at all. Read at
`windows/worldgen/tests/descent_graph.rs:242-289`: it calls
`founder_of(&w, o.id)` and keys its groups with
`hornvale_history::record::founding_key`. `founder_of`
(`windows/worldgen/src/descent.rs:37`) is a **second, different** founder
handle — `RoleHandle(founding_key_from(own, parent) ^ world.seed.0.rotate_left(17))`
— consumed by `windows/lab/src/metrics.rs:6187,6202` (The Namesake's
name-prefix census metrics) and by `descent.rs:179`'s kinship walk.

The test that actually pins `flesh::founder_handle`'s id-freedom is
`domains/history/tests/flesh.rs:192`
`a_founder_handle_ignores_entity_ids_and_notices_semantics`.

**Consequence for Part B, and it is scope-defining:** the world holds **two**
founder identities derived from different keys. This campaign's epoch moves
`flesh::founder_handle` (the promotion key, hence person **names** in the
ledger) and **not** `descent::founder_of` (the descent-graph key, hence the
census's name-prefix metrics). Task 3 documents that distinction beside the
key; Task 3's artifact branch table treats any movement in
`book/src/laboratory/` as a STOP, precisely because `founder_of` is what those
rows read.

**Not verified, and left to the implementer to establish by measurement:**

- Whether `Ended::By(EntityId)` always references another *occupation*
  reconstructable from the same records slice. `record.rs:60-66` says only "at
  the hand of another entity (a raiding people, a rival community, …)". Task 3
  step 1 measures the resolution rate before the key is designed around it.
- Which of the five empty stores in `peopled_components()` the band's subject
  actually needs. Task 1 step 2 measures rather than assumes.

---

## File Structure

| file | responsibility | tasks |
|---|---|---|
| `windows/worldgen/tests/beta_calibration_freeze.rs` | **modify** — the Part A guard: live component set, named roster, its own module-doc record of the roster flip | 1, 2 |
| `domains/species/src/lib.rs` | **modify** — `biome_affinity_registry`'s doc gains Part C: the level's three consumers and the evidence for each | 2 |
| `domains/history/src/flesh.rs` | **modify** — `founder_handle` retained as the superseded v1; `founder_handle_v2` added, folding parent and ender material keys | 3 |
| `domains/history/src/record.rs` | **modify** (only if the resolver helper lands here) — an id→`&Occupation` resolution helper shared by promotion and its tests | 3 |
| `domains/history/tests/flesh.rs` | **modify** — the v2 key's unit properties: id-free, semantics-sensitive, referent-sensitive, twins still collide | 3 |
| `windows/worldgen/src/person_promote.rs` | **modify** — `select_founders` keys on v2; the drop backstop and post-condition unchanged | 3 |
| `windows/worldgen/tests/founder_collision.rs` | **modify** — the five named seeds re-read under v2; module doc re-stated | 3, 5 |
| `windows/worldgen/tests/founder_epoch.rs` | **create** — B2: v1 and v2 casts compared in one process, with the arm that would have shown movement | 4 |
| `book/src/gallery/`, `docs/audits/`, `docs/digest/`, … | **regenerate** — the epoch's artifact blast radius, in the commit that causes it | 3 |
| `book/src/chronicle/the-muster.md` | **create** — the campaign's chronicle, carrying the promoted sweep (spec §6) | 6 |
| `docs/retrospectives/the-muster.md` | **create** — process lessons (decision 0020) | 6 |
| `book/src/frontier/idea-registry.md` | **modify** — `MEM-founder-handle-epoch` → `shipped`; `BIO-40` repointed | 6 |
| `docs/decisions/0121-*.md` | **create** — the founder-handle epoch, ratified | 6 |

---

## Task 1: The guard reads the live registry, and names the roster it counts

**Files:**
- Modify: `windows/worldgen/tests/beta_calibration_freeze.rs:180-211`
  (`peopled_components`), `:213-263` (the assertion), and the module doc
  (`:1-128`)

**Interfaces:**
- Consumes: `hornvale_worldgen::WorldComponents::from_stores(biosphere, psyche,
  society, perception, articulation, lexicon, family_proto, family_of, deity,
  culture, material, habitat_realm, biome_affinity) -> Result<Self, BuildError>`
  (`windows/worldgen/src/components.rs:121`);
  `hornvale_species::{psyche_registry, biosphere_registry, society_registry,
  perception_registry, family_of, biome_affinity_registry, habitat_realm_registry}`;
  `hornvale_worldgen::demography_report_from(&World, &WorldComponents,
  &GeneratedTerrain, &GeneratedClimate) -> Result<DemographyReport, BuildError>`
  (`windows/worldgen/src/lib.rs:2050`).
- Produces: a repaired `fn peopled_components() -> WorldComponents` whose
  affinity store is non-empty, and a named roster the assertion message states.
  Task 2 consumes both.

- [ ] **Step 1: Measure the blindness before repairing it — and get the count on the record**

Add a temporary `#[test]` beside the guard (deleted in step 6) that prints, for
the component set the guard builds **today**, the size of every store it
passes:

```rust
#[test]
fn t1_probe_which_stores_the_guard_holds() {
    let wc = peopled_components();
    println!(
        "T1-STORES psyche={} biosphere={} habitat_realm={} biome_affinity={} \
         deity={} culture={} material={}",
        wc.psyche.len(),
        wc.biosphere.len(),
        wc.habitat_realm.len(),
        wc.biome_affinity.len(),
        wc.deity.len(),
        wc.culture.len(),
        wc.material.len(),
    );
    println!(
        "T1-CANONICAL affinity_rows={} realm_rows={}",
        hornvale_species::biome_affinity_registry().len(),
        hornvale_species::habitat_realm_registry().len(),
    );
}
```

If a registry accessor named above does not exist under that name, grep
`domains/species/src/lib.rs` for the `pub fn *_registry` it does export and use
that; record the correction in the commit message.

Run:
```bash
cargo test -p hornvale-worldgen --test beta_calibration_freeze t1_probe -- --nocapture
```

**Branch on `biome_affinity` in the `T1-STORES` line:**

| result | do this |
|---|---|
| `biome_affinity=0` | The spec's §1.2 claim holds. Proceed to step 2. |
| `biome_affinity>0` | The defect has already been repaired by another campaign since the spec was written. **STOP** and report — Part A's premise is gone and the campaign's shape changes. Do not invent a second defect to justify the task. |

Record the whole `T1-STORES` / `T1-CANONICAL` output in the task's commit
message; step 2's decision reads it.

- [ ] **Step 2: Decide which stores go live, by what the band's subject needs — not by tidiness**

The guard's subject is `byproducts.strife` over claimed cells
(`beta_calibration_freeze.rs:146-174`), which reaches the affinity store
through `demography_report_with_beta_from`
(`windows/worldgen/src/lib.rs:1747-1753`). Read that function's body from
`:1710` to `:1790` and answer, in the commit message, **for each of the five
empty stores**: does `demography_report_with_beta_from` read it on the path to
`byproducts.strife`?

The rule to apply:

| store | rule |
|---|---|
| `biome_affinity` | goes live unconditionally — it is the quantity the guard exists to be able to see |
| any other store the traced path reads | goes live, scoped to the peopled key-set the same way `biosphere` and `family_of` already are (`:184-194`) |
| any store the traced path does not read | stays `ComponentStore::new()`, and the code carries a one-line comment naming *why it is empty* rather than leaving it to be read as an oversight |

`habitat_realm` is the one the drafting pass flagged: it is read three lines
above the affinity store (`:1733-1743`), and the sweep's peopled instrument
retained it. Trace it and decide; do not take this sentence as the answer.

- [ ] **Step 3: Repair `peopled_components`, scoped to the peopled key-set**

Follow the existing scoping idiom in the same function (`:181-194`) — filter
the canonical registry by the `psyche` key-set, do not hand-write a roster:

```rust
let affinity: ComponentStore<KindId, hornvale_species::BiomeAffinity> =
    hornvale_species::biome_affinity_registry()
        .iter()
        .filter(|(k, _)| peopled.contains(k))
        .map(|(k, v)| (*k, v.clone()))
        .collect();
```

and pass it in `from_stores`' thirteenth position. Repeat for each store step 2
put live.

- [ ] **Step 4: Make the instrument name itself**

The band's verdict is decided by which roster is counted (spec §3.2), so the
guard must state its own population inside its own assertion. Extend the
existing message at `:253-262` so a reader of a red gate learns, without
opening the file: the roster's size, how the roster was derived, how many of
those kinds carry an affinity row, and that the band was written about this
population and not the biosphere.

The property to satisfy: **a reader who sees only the failure message can tell
which of the two rosters in spec §3.2's table was counted.** For example
(adapt, do not paste blind):

```rust
let affinity_rows = wc.biome_affinity.len();
assert!(
    (MONOCULTURE_FLOOR..=ceiling).contains(&mean),
    "mean per-claimed-cell diversity at beta={} across seeds {per_seed:?} = {mean}, \
     expected in [{MONOCULTURE_FLOOR}, {ceiling}].\n\
     ROSTER: the {oatmeal} PEOPLED kinds (the `psyche` key-set), of which \
     {affinity_rows} carry a biome-affinity row. This is NOT the {} -row \
     biosphere: the same worlds at the same beta measure ~1.42-1.46 over the \
     biosphere and fail this band at every affinity level, and ~2.19-3.00 over \
     this roster and pass at every affinity level (The Muster, spec 3.2). The \
     band was preregistered about the peopled roster; scoring it over the \
     biosphere changes the verdict without changing the physics.\n\
     The floor is absolute (monoculture is 1 whatever the roster size); the \
     ceiling is {OATMEAL_FRACTION} x oatmeal. Do not replace the derived \
     ceiling with a literal.",
    hornvale_demography::BETA,
    hornvale_species::biosphere_registry().len(),
);
```

- [ ] **Step 5: Produce Prediction A2's evidence — the roster flip**

Spec §3.4: holding worlds and β fixed, the guard's pass/fail flips on the
roster counted alone. Add a second temporary probe (deleted in step 6) that
measures `claimed_diversity` over the same `SEEDS` with **two** component sets:
the repaired peopled one, and one built from the full canonical registries
(`WorldComponents::assemble()`). Print both means and both verdicts against the
`[1.5, 0.75 × oatmeal]` band each roster derives for itself.

Run it, capture the output, and branch:

| result | do this |
|---|---|
| the two rosters give **opposite** verdicts | A2 confirmed. Paste the two means into the guard's module doc as the record of *why* the roster is named, and into the commit message. |
| the two rosters give the **same** verdict | A2 is falsified. That is a **finding about the sweep, not about the guard** (spec §3.4). Record the measured numbers, say plainly that spec §3.2's table does not reproduce on this tree, keep the repair (it is justified by A1 alone), and carry the falsification into the chronicle. Do **not** adjust either roster to recover the flip. |
| the full-roster arm fails to build or panics | Report it; do not work around it. A component set that `assemble()` produces and `demography_report_from` cannot consume is its own defect. |

- [ ] **Step 6: Delete both probes and run the guard**

Remove `t1_probe_which_stores_the_guard_holds` and the step-5 probe. Their
output lives in the module doc and the commit message, which is where a
measurement belongs; a probe left in the tree becomes a test nobody owns.

```bash
cargo test -p hornvale-worldgen --test beta_calibration_freeze 2>&1 | tee /tmp/hv-muster-t1.txt
```

**Branch on the guard's result:**

| result | do this |
|---|---|
| green | Proceed. Note that green here proves nothing yet — Task 2's positive control is what makes this task's claim real. |
| red, mean **below** 1.5 | The live affinity store drove the peopled roster monocultural. That is a real finding about the level and it is **not** licence to widen the band (non-goal 3). Report the mean, stop, and escalate. |
| red, mean **above** the derived ceiling | Same posture: report, stop, escalate. Do not replace the derived ceiling with a literal — the message at `:260` forbids it and says why. |

- [ ] **Step 7: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
```

```bash
cat > /tmp/hv-msg-t1.txt <<'EOF'
test(muster): the coexistence guard reads the live registry, and says what it counted

`beta_yields_realistic_coexistence` built its component set with five empty
stores, `biome_affinity` among them, so no affinity-level change could reach
it. It now holds the live rows, scoped to the peopled key-set the same way
`biosphere` and `family_of` already were.

The repair owes more than a live store: the band's verdict is decided by which
roster is counted, not by the physics. The assertion now states its roster, its
size, and the contrasting reading over the biosphere, so a red gate names the
instrument as well as the number.

<paste T1-STORES, T1-CANONICAL and the A2 two-roster means here>
EOF
git commit -F /tmp/hv-msg-t1.txt -- windows/worldgen/tests/beta_calibration_freeze.rs
```

---

## Task 2: The positive control (A1), and the level's three consumers (Part C)

**Files:**
- Modify: `windows/worldgen/tests/beta_calibration_freeze.rs` (module doc only,
  to record the control)
- Modify: `domains/species/src/lib.rs` — `biome_affinity_registry`'s doc block
  (the derivation narrative runs from roughly `:2469` to `:2688`; the new
  material goes beside the "level is gauge" paragraph at ~`:2641-2646`)

**Interfaces:**
- Consumes: Task 1's repaired `peopled_components()`.
- Produces: a recorded, reproducible mutation that reddens the guard, and the
  three-consumer record beside the derivation. Nothing later depends on either
  as an API.

- [ ] **Step 1: Establish what the guard's own headroom is**

From Task 1 step 6 you have the guard's mean and its band. Write down the
distance to each edge. You need it in step 2: a mutation smaller than the
sweep's whole-grid swing (measured at **0.039, 2.7%**, spec §2 non-goal 3)
cannot be expected to move a verdict, and choosing one is how a positive
control turns into a false null.

- [ ] **Step 2: Find a mutation that makes the repaired guard go RED**

**This is mandatory. A guard that is repaired and still green proves nothing**
(spec §3.3).

The property the mutation must demonstrate, stated as a property because the
plan author does not know which perturbation this tree will find
discriminating:

> A change confined to the biome-affinity **level** — the quantity
> `BiomeAffinity::default`, which `from_preferences`
> (`domains/species/src/lib.rs:2440`) sets from `sovereignty_floor` — applied
> only inside the test's own component set, with every authored **shape**
> (the `by_biome` preference vector) carried through unchanged, moves
> `beta_yields_realistic_coexistence`'s measured mean far enough to cross a
> band edge.

Constraints on your search, and each is there because of a measured failure:

- **The shape must be preserved.** Invert and rebuild through the same round
  trip the sweep used: `pref = (factor − floor) / (1 − floor)`, then
  `BiomeAffinity::from_preferences(new_level, prefs)`. That round trip is known
  to be bit-exact at an unchanged level (sweep 2 control (a), 35 factors on
  `to_bits()`), so it will not itself contribute movement.
- **A uniform level is a trap.** Sweep 1 collapsed the per-kind spread to one
  number and returned a verdict that was an artifact of the parameterisation.
  If you use a uniform level, you are measuring the destruction of per-kind
  ordering, not the level.
- **Reach is the likely obstacle, and it is measured.** The sweep recorded
  `affinity_rows=8`, `peopled_with_affinity=7`,
  `fraction_of_pack_columns=0.2051` — the level reaches about a fifth of the
  columns `pack` normalizes over. If a plausible level change does not move the
  mean, widening the *reach* (more kinds carrying a row, as the sweep's
  `diversity_positive_control_broad_reach` did) is a legitimate second arm and
  must be reported as such.
- **`test-fixture-only`.** The mutation lives inside the test's component set.
  Nothing under `domains/species/src/` is edited to produce it — that would be
  re-authoring a preference row (non-goal 4).

Record the mutation you tried, the mean it produced, and the band edge it
crossed, in `MUTATION:` lines in the commit message. Record the ones that
**failed** too — a null arm is evidence about reach.

**Branch on the outcome:**

| result | do this |
|---|---|
| a level-only mutation reddens the guard | A1 confirmed. Go to step 3. |
| no level-only mutation reddens it, but a level-plus-reach mutation does | A1 confirmed **with a stated qualification**: the guard discriminates the level only across the columns the level reaches, and today that is ~20% of them. Write the qualification into the module doc; it is the honest reading and it is a finding. |
| nothing reddens it | **A1 is falsified, and that is the campaign's headline for Part A** (spec §3.3). The guard is blind for a second reason beyond the empty store, and *that reason is the finding*. Do not repair anything further in this task. Bisect the path — component set → `species_affinity` slice (`lib.rs:1747`) → `per_species_suitability` → `pack` → `byproducts.strife` — find where the signal dies, and report the location. Escalate before writing any fix. |

- [ ] **Step 3: Record the control in the module doc, reproducibly**

The mutation is deleted; its record is not. Add a section to
`beta_calibration_freeze.rs`'s module doc giving the exact mutation, the mean
before, the mean after, and the band edge crossed — enough that a reader can
re-run it in five minutes. Model it on the module doc's existing re-baseline
tables (`:28-36`, `:62-68`), which is the file's own idiom for a recorded
measurement.

- [ ] **Step 4: Part C — record the level's three consumers beside the derivation**

`domains/species/src/lib.rs::biome_affinity_registry`'s doc already carries the
derivation and, at ~`:2641-2646`, the sentence *"'Level is gauge' … is true
only of RANKING"*. That sentence is correct and **incomplete**: it names one
consumer and one exemption. Extend it to name all three, with what each is
sensitive to and the evidence for it:

1. **`per_species_capacity`** — the level multiplies headcount, hence
   population, hence the history bake's volume. Evidence already in the tree:
   the abandoned `0.25` took seed 42 from 552 occupation records to 193 and
   breached four fidelity floors (recorded at ~`:2626-2639`).
2. **`coexist::pack`, per-kind share** — each kind's share of a cell is `K^β`
   normalized **across** kinds, so a per-kind rescale moves every share.
   Evidence: `windows/worldgen/src/lib.rs:1778`, and this task's own step-2
   control, which moved the guard by moving the level alone.
3. **`coexist::pack`, cell capacity** — the cell's total is a plain **sum**
   across kinds, so the level moves the total even where it does not move the
   ordering.

Then state the closing point, which is the whole reason this is being written
down: **only within-kind ranking is level-invariant.** "The level is gauge" is
a true statement about exactly one consumer that was applied to all three, and
that is why it survived two campaigns (Radiation retrospective §3).

Attach the sweep's verdict as the evidence that the level is nonetheless **one**
quantity: `level_k = λ · floor_k` satisfies every preregistered band at
λ ∈ {0.25, 0.50, 1.00, 1.20} with the shipped configuration interior, and
λ = 1.0 reproduces the shipped world byte-identically across five seeds. Cite
the chronicle (Task 6 writes it) rather than the throwaway scratch.

- [ ] **Step 5: Doctests, fmt, clippy**

`biome_affinity_registry`'s doc is large and doc-tested by the workspace
doctest run. Adding a fenced block without `text`/`ignore` will try to compile.

```bash
cargo test -p hornvale-species --doc
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
```

- [ ] **Step 6: Regenerate artifacts and read the diff as a branch table**

A doc-comment change to a `pub fn` can move the type-audit report.

```bash
make rebaseline; echo "rebaseline exit=$?"
make rebaseline-goldens; echo "goldens exit=$?"
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

| what moved | do this |
|---|---|
| nothing | commit the source change alone |
| only `docs/audits/type-audit-report.md` | stage and commit it **in this same commit** |
| `book/src/gallery/` or `book/src/laboratory/` | **STOP.** A doc-comment change cannot move a world. Something else is in your working tree, or an earlier task's regeneration was skipped. Find it before committing. |
| a non-zero exit from either `make` | the run was truncated by `set -e`; read the log, fix the cause, re-run. A clean seven-path diff after a failed rebaseline is not evidence. |

- [ ] **Step 7: Commit**

```bash
cat > /tmp/hv-msg-t2.txt <<'EOF'
test(muster): the repaired guard can go red, and the level has three consumers

The positive control Part A owes: <mutation>, applied to the level alone with
every authored shape carried through the invert/rebuild round trip, moves the
guard's mean from <before> to <after> and crosses the <edge> edge. A guard that
is repaired and still green proves nothing; this is what makes Task 1's claim
real.

MUTATION (fired): ...
MUTATION (null):  ...

`biome_affinity_registry`'s doc now names all three consumers of the level —
capacity/population/history volume, pack's per-kind share, pack's cell-capacity
sum — and states that only within-kind ranking is level-invariant. "The level is
gauge" was a true statement about one of three consumers applied to all of them,
which is why it survived two campaigns.
EOF
git commit -F /tmp/hv-msg-t2.txt -- windows/worldgen/tests/beta_calibration_freeze.rs domains/species/src/lib.rs docs/audits/
```

(Drop `docs/audits/` from the path list if step 6 showed it did not move.
`git diff --exit-code` against a path with no staged change is silent, so an
unnecessary path is harmless — a *missing* one is not.)

---

## Task 3: The widened key, as a `/v2` epoch

**Files:**
- Modify: `domains/history/src/flesh.rs:69` (`FOUNDER_ROLE`), `:79-137` (the
  doc block), `:137-153` (`founder_handle`)
- Modify: `domains/history/src/record.rs` — only if the resolver helper lands
  here (see step 2)
- Modify: `domains/history/tests/flesh.rs:192-224`
- Modify: `windows/worldgen/src/person_promote.rs:130-203`
- Modify: `windows/worldgen/tests/founder_collision.rs`
- Regenerate: the seven artifact paths

**Interfaces:**
- Consumes: `hornvale_history::record::{Occupation, OccupationRecord, Ended,
  Founding, FoundingCoords, founding_coords, material_key}`
  (`domains/history/src/record.rs:101, 149, 60, 74, 308, 318, 362`);
  `hornvale_history::flesh::{RoleHandle, founder_handle}`
  (`domains/history/src/flesh.rs:26, 137`).
- Produces, and Task 4 depends on every name here:
  - `pub fn founder_handle(occ: &OccupationRecord) -> RoleHandle` — **unchanged
    body, unchanged value**, marked superseded. Retained because an epoch is a
    save-format contract and a superseded derivation stays declared
    (`domains/language/src/streams.rs::V2` is the precedent).
  - `pub fn founder_handle_v2(occ: &OccupationRecord, parent: Option<&Occupation>, ender: Option<&Occupation>) -> RoleHandle`
  - `pub fn select_founders(records: &[OccupationRecord]) -> FounderCast` —
    signature unchanged, now keyed on v2 internally.
  - Whatever id→record resolution helper step 2 lands, with its exact
    signature written into the commit message.

- [ ] **Step 1: Measure what the referents actually resolve to, before designing around them**

`Founding::From(EntityId)` and `Ended::By(EntityId)` are documented only as
"another entity" (`record.rs:60-79`). The widened key folds their **material**
keys, which requires resolving them to `Occupation`s in the same records slice.
Whether that resolution succeeds is an empirical question.

Write a temporary `#[test]` in `windows/worldgen/tests/founder_collision.rs`
that, for a named seed panel including the five colliding seeds
(283, 705, 2403, 2634, 2898) plus 42 and 2793, builds to
`BuildDepth::Settlements`, builds `BTreeMap<EntityId, &OccupationRecord>` over
`occupation_records`, and prints per seed:

```
T3-RESOLVE seed=<s> occs=<n> from_genesis=<a> from_resolved=<b> from_unresolved=<c> \
           ended_nature=<d> ended_resolved=<e> ended_unresolved=<f>
```

It loops seeds, so it needs a `/// claim:` line — use
`/// claim: structural(seed: [42, 283, 705, 2403, 2634, 2793, 2898]) — seven
named worlds; the enumeration is a completed sweep's positive set plus two
controls, not a search.` (the same form as `:83-86`).

**Branch on the unresolved counts:**

| result | do this |
|---|---|
| `from_unresolved` and `ended_unresolved` are both 0 everywhere | Both referents always resolve within the slice. Design the key on that, and still handle `None` (a caller may hold a partial slice — this is exactly the case `layer_key:277-280` documents). |
| either is non-zero | The key must fold an explicit **unresolved marker**, distinct from both "no referent" and "resolved referent", exactly as `layer_key` does with its `(2u8, 0, 0, 0)` arm. Record the rates. |
| a seed fails to build | Report it. `founder_collision.rs:66-77` already claims 283 and 705 build to `Full`; a build failure here is a liveness regression from an absorbed commit, not this campaign's. |

- [ ] **Step 2: Land the resolver helper where both promotion and its tests can reach it**

`select_founders` holds the whole slice, and three tests already hand-roll the
same `BTreeMap<EntityId, …>` construction
(`cli/tests/id_shift_invariance.rs:216-219`,
`windows/worldgen/tests/descent_graph.rs:249-251`,
`windows/worldgen/tests/history_emit.rs:476-479`). Add it once, in
`domains/history/src/record.rs`, next to the other material-fold helpers:

```rust
/// Every occupation in `records`, indexed by its own entity — the lookup a
/// caller needs to resolve `founded_from` / `ended_by` to their referents'
/// MATERIAL facts. The `EntityId` is a lookup key only and is never read for
/// its value (decision 0051, The Salt); nothing derived from this map folds
/// the id itself.
pub fn occupations_by_id(records: &[OccupationRecord]) -> BTreeMap<EntityId, &Occupation> {
    records.iter().map(|r| (r.id, &r.core)).collect()
}
```

If the borrow shape fights you (`&Occupation` borrowed from a slice the caller
also mutates), return `BTreeMap<EntityId, usize>` instead and index the slice —
record which you chose and why in the commit message. Either is fine; a
`HashMap` is not.

- [ ] **Step 3: Write the v2 key's failing unit tests first**

In `domains/history/tests/flesh.rs`, beside the existing
`a_founder_handle_ignores_entity_ids_and_notices_semantics` (`:192`). Four
properties, four tests, hand-built records (no world build):

```rust
#[test]
fn the_v2_handle_is_still_free_of_entity_ids() {
    // Same shape as the v1 test at :192 — two records identical in every
    // material fact, differing only in `id`, with identical referents.
    // Their v2 handles must be equal.
}

#[test]
fn the_v2_handle_notices_a_referents_material_difference() {
    // Two records identical in all five v1 fields and identical in their own
    // ids' *positions*, whose PARENTS differ in one material fact, must get
    // DIFFERENT v2 handles. Repeat for a difference in the ENDER.
    // This is the property the whole epoch exists for: v1 gives these two the
    // same handle, v2 must not.
}

#[test]
fn the_v2_handle_distinguishes_no_referent_from_an_unresolved_one() {
    // `Founding::Genesis`, `Founding::From(e)` with `e` resolvable, and
    // `Founding::From(e)` with `e` unresolvable must yield three distinct
    // handles for otherwise-identical records. Same for `Ended::Nature` vs
    // `Ended::By(resolved)` vs `Ended::By(unresolved)`.
}

#[test]
fn two_wholly_identical_occupations_still_share_a_v2_handle() {
    // The residual the backstop exists for. `+ parent and ender` still leaves
    // 2 pairs in 3000 worlds, so this is a PROPERTY OF THE DESIGN, not a bug:
    // a record cloned entire, with cloned referents, must collide.
}
```

Fill in the bodies with real records — copy the literal `OccupationRecord`
construction from `:193-210` and from `person_promote.rs:308-327`. No
placeholders.

Run them; they fail to compile because `founder_handle_v2` does not exist. That
is the red.

- [ ] **Step 4: Write `founder_handle_v2`, and mark v1 superseded**

In `domains/history/src/flesh.rs`. Three things, in one edit:

1. A new discriminant beside `FOUNDER_ROLE` (`:69`), so the epoch is greppable
   and the v1 salt keeps its meaning:

```rust
/// A discriminant mixed into every founder handle, so that a future second
/// role at the same occupation cannot collide with the founder.
///
/// **Superseded by [`FOUNDER_ROLE_V2`]** and retained, not deleted: an epoch
/// is a save-format contract (decision 0006), so a superseded derivation
/// stays declared — the same posture `domains/language/src/streams.rs`'s
/// `V2` leg takes after `V3` retired it.
const FOUNDER_ROLE: u64 = 0x466F_756E_6465_7200;

/// The founder role's epoch-2 discriminant. The Muster widened the key to
/// fold the `founded_from` and `ended_by` referents by their MATERIAL facts;
/// a distinct discriminant is what makes the two epochs findable rather than
/// silently one derivation with two behaviours.
const FOUNDER_ROLE_V2: u64 = 0x466F_756E_6465_7202;
```

2. `founder_handle_v2`, folding the referents through `record::material_key`
   (`record.rs:362`) — never through their ids. Mirror v1's `mix` arithmetic so
   both handles are drawn from one space (`record.rs:349-359` says why that
   matters), and give the three referent states distinct arms the way
   `layer_key:265-281` does.

3. Rewrite v1's doc block (`:79-137`). The struck-through paragraph and The
   Radiation's dated correction **stay** — they are the record of the reasoning
   that closed off this repair for a campaign and a half, and the file itself
   says they are "worth more visible than tidy". Append a dated Muster section
   saying: the correction has been acted on; `founder_handle_v2` is the shipped
   key; this function is retained as the superseded epoch and is read by
   `windows/worldgen/tests/founder_epoch.rs`; and — the fact drafting found and
   the spec does not carry — **`descent::founder_of` is a different founder
   handle, keyed on `founding_key`, and this epoch does not move it.**

- [ ] **Step 5: Run the unit tests to green**

```bash
cargo test -p hornvale-history --test flesh 2>&1 | tee /tmp/hv-muster-t3-unit.txt
```

All four new tests pass; the existing v1 test at `:192` still passes untouched.
If the v1 test now fails, you changed v1's value — that is a defect, not an
epoch. Revert and re-do.

- [ ] **Step 6: Wire promotion onto v2, without touching the backstop**

In `windows/worldgen/src/person_promote.rs`:

- Build the referent map once at the top of `select_founders` (step 2's
  helper), before the `by_people` grouping.
- Replace both `founder_handle(...)` calls — the ranking leg at `:156` and the
  promotion call at `:160` — with `founder_handle_v2(...)` and the resolved
  referents.
- **Do not touch** `kept_by_handle` (`:141`), the drop at `:161-169`, the
  post-condition assert at `:188-198`, or the ranking's first three keys
  (`:145-149`).
- The comment at `:150-155` explains why the handle is the *last* ranking key
  and why adding a fifth would move worlds that never collide. That reasoning
  survives the epoch verbatim — the handle is still the last key, it is just a
  different handle. Update the comment's reference from
  `founder_handle` to `founder_handle_v2` and leave the argument standing.
- Update the `# Indistinguishable occupations are dropped, not fatal` section
  (`:83-114`) to state the v2 key and to keep the sentence that no key is
  total. Delete the "deferred as an epoch" paragraph at `:112-114` and replace
  it with what actually shipped.

The four in-module unit tests at `:329-455` should all still pass: each builds
records whose referents are identical (`Founding::Genesis(site)`,
`Ended::Nature`) or cloned entire, so the twins are still twins under v2. If
one goes red, read it before changing it — a red there means the v2 key
distinguishes something the test intended to be identical, which is
information.

- [ ] **Step 7: Re-read `founder_collision.rs` under the new key**

Delete the step-1 probe. Then run the file and treat every red as a
measurement, not a chore:

```bash
cargo test -p hornvale-worldgen --test founder_collision 2>&1 | tee /tmp/hv-muster-t3-coll.txt
```

`the_dropped_founders_are_pinned_per_seed` (`:88`) pins
`(283,1) (705,1) (2403,1) (2634,1) (2898,1)` with 42 and 2793 at 0. Under the
widened key those five are the whole of Prediction B1's subject.

| result | do this |
|---|---|
| all seven seeds now drop **0** | B1 confirmed on its named subject. Re-pin all seven to 0, and rewrite the array's comment to say these are now **controls** — seeds that used to drop and no longer do — so a future reader does not read seven zeros as a vacuous test. |
| some seed still drops | B1 partially falsified on its named subject. **Report the rate; do not widen the key further to chase it** (spec §4.4). Re-pin to the measured values and carry the residual into the chronicle as the finding. |
| the *count of drops* fell to 0 everywhere | Then `a_dropped_founder_is_not_backfilled` (`:138`, seed 283) has **no live subject** and its `.expect("seed 283 drops exactly one founder")` panics. This is expected, and the response is **not** to delete the property: the no-backfill behaviour is already proved synthetically by `person_promote.rs::a_drop_costs_one_founder_and_is_not_backfilled` (`:429`). Remove the live-world test, and put a sentence in `founder_collision.rs`'s module doc saying the live subject is gone, that the synthetic one is now the only proof, and where it is. A property with no witness left in the tree is how a backstop gets deleted two campaigns later. |
| `a_colliding_seed_builds_to_full_depth_instead_of_panicking` (`:66`) reddens | **STOP.** A liveness regression. The backstop was removed or bypassed. |

Rewrite the module doc (`:1-33`) to describe the v2 key, the retained backstop,
and what each remaining test now pins.

- [ ] **Step 8: Run the whole suite once, and classify every red**

```bash
HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-muster-t3-suite.txt
```

Every red is one of exactly three kinds. Classify each in the commit message:

| kind | signature | response |
|---|---|---|
| **a founder name moved** | the test asserts a literal person name, or a golden containing one | expected; re-pin the witness, and say in the commit message which name moved to which |
| **a founder count or cast shape moved** | drop counts, cast sizes, `person-*` fact counts | **not** expected from a pure rename. Investigate before re-pinning: this is Prediction B2's falsifier. |
| **anything upstream of promotion moved** | settlement counts, populations, occupation records, terrain, climate, census rows | **STOP.** B2 is falsified and the campaign is not safe. `founder_handle` would have to reach something upstream of promotion; `promote` runs last (`windows/worldgen/src/lib.rs:7403`). Escalate. |

- [ ] **Step 9: Regenerate every artifact, and read the diff as a branch table**

This is the commit that changes the key, so this is the commit that regenerates
(Global Constraints).

```bash
make rebaseline; echo "rebaseline exit=$?"
make rebaseline-goldens; echo "goldens exit=$?"
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

| what moved | reading | do this |
|---|---|---|
| `book/src/gallery/history-seed-42.md`, only in `was founded by <name>` tokens | the rename, exactly as B2 predicts | stage it |
| `docs/audits/type-audit-report.md` | `founder_handle_v2` and `occupations_by_id` are new `pub` boundaries | stage it in this same commit |
| `docs/digest/` | expected only if a decision record or the registry moved — neither has yet | if it moved for another reason, read it before staging |
| another `book/src/gallery/` almanac, in a **settlement, population or date** field | B2's falsifier | **STOP.** Escalate. |
| anything under `book/src/laboratory/` | those rows read `descent::founder_of`, which this epoch does not touch | **STOP.** Either the epoch reached further than designed, or the committed census is stale for an unrelated reason. Establish which before committing; `make lab-diff STUDY=the-census` names the metrics. |
| anything under `book/src/domesday/` | a pure read over the committed census, which has not been re-run | **STOP**, same reasoning |
| anything under `clients/game/core/tests/fixtures/` | drafting found no `hornvale_person` consumer under `windows/vessel` or `windows/scene` | **STOP** and report — it means person state reaches a session snapshot by a path nobody has mapped |
| a non-zero exit from either `make` | truncated by `set -e` | read the log, fix, re-run; a clean diff after a failed rebaseline is not evidence |

- [ ] **Step 10: fmt, clippy, gate, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate 2>&1 | tee /tmp/hv-muster-t3-gate.txt
```

```bash
cat > /tmp/hv-msg-t3.txt <<'EOF'
feat(muster): the founder-handle epoch — v2 folds the referents' material keys

`founder_handle` keyed on (people, site, founded, ended, peak_population) and
collided by construction: 1904 handle-sharing pairs over seeds 0-2999, five
reaching the promoted cast. `founder_handle_v2` folds the `founded_from` and
`ended_by` referents by their MATERIAL keys — never their ids, which is what
The Radiation's correction to decision 0051 made admissible and what
`FoundingCoords`, `founding_key_from` and `layer_key` had already been doing.

v1 is retained and marked superseded, not deleted: an epoch is a save-format
contract, so a superseded derivation stays declared. `descent::founder_of` is a
DIFFERENT founder handle, keyed on `founding_key`, and this epoch does not move
it.

The drop backstop stays. `+ parent and ender` still leaves 2 residual pairs in
3000 worlds, so no key is total and a world generator must not panic on a legal
seed.

Re-pins: <per-seed drop counts>
Artifacts regenerated in this commit: <list>
EOF
git commit -F /tmp/hv-msg-t3.txt -- domains/history/src/flesh.rs domains/history/src/record.rs domains/history/tests/flesh.rs windows/worldgen/src/person_promote.rs windows/worldgen/tests/founder_collision.rs book/src/gallery/ docs/audits/ <plus any other path step 9 moved>
```

---

## Task 4: Prediction B2 — the epoch is a rename, and nothing else

**Files:**
- Create: `windows/worldgen/tests/founder_epoch.rs`

**Interfaces:**
- Consumes: `hornvale_history::flesh::{founder_handle, founder_handle_v2, RoleHandle}`;
  `hornvale_worldgen::person_promote::{select_founders, Founder, FounderCast, MEMORY_DEPTH}`;
  `hornvale_worldgen::{build_world_to, occupation_records, BuildDepth,
  SettlementPins, SkyChoice, WorldComponents}`;
  `hornvale_history::record::occupations_by_id` (Task 3 step 2).
- Produces: nothing consumed later. This is the campaign's safety measurement.

**Why this shape.** B2 says the widened key changes every founder handle and
changes nothing else. The comparison is expressible **in one process**, because
Task 3 retained v1: build a seed once, compute both casts over the same
records, and compare. The precedent for a test-local legacy replica of a
production ordering is `windows/worldgen/tests/history_emit.rs:653`
`legacy_layer_key`, which exists for exactly this purpose.

**And it needs the arm that would have shown movement if there were any**
(spec §4.5). A test that asserts "the non-name fields are equal" passes
trivially if both casts are empty, or if the two keys happen to be the same
function. So the same test asserts, on the same data, that the handle sets are
**disjoint** — the arm that must move.

- [ ] **Step 1: Write the test file, red first**

```rust
//! Prediction B2 (The Muster): the founder-handle epoch is a RENAME and
//! nothing else.
//!
//! The v1 key is retained (an epoch is a save-format contract, so a superseded
//! derivation stays declared), which makes this comparison expressible in one
//! process: build a seed once, promote it twice — once under each key — and
//! ask what differs.
//!
//! **Both arms are required.** "The non-name fields are equal" passes
//! vacuously against an empty cast or an accidentally-identical key; "every
//! handle moved" is the arm that proves the instrument can see a difference at
//! all. Neither alone is evidence.
//!
//! The legacy promotion loop below is a test-local replica of the production
//! one, the same posture `history_emit.rs`'s `legacy_layer_key` takes.

use hornvale_astronomy::SkyPins;
use hornvale_history::flesh::{RoleHandle, founder_handle};
use hornvale_history::record::OccupationRecord;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::{MEMORY_DEPTH, select_founders};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, occupation_records,
};
use std::collections::{BTreeMap, BTreeSet};

/// The seed panel. Seed 42 is the project's witness; 283 and 705 are the two
/// census-range worlds that used to drop a founder, so the panel contains the
/// case the epoch exists to fix as well as the ordinary case.
const SEEDS: [u64; 3] = [42, 283, 705];

/// The v1 promotion loop, replicated. Mirrors
/// `windows/worldgen/src/person_promote.rs:130-203` at the commit that
/// introduced `founder_handle_v2`, differing ONLY in which key it calls, so a
/// difference between the two casts can be attributed to the key and to
/// nothing else.
fn legacy_cast(records: &[OccupationRecord]) -> Vec<(RoleHandle, usize, &'static str, f64)> {
    // ... replicate `select_founders`' body: group by people, sort by
    // (peak_population DESC, site ASC, founded ASC, founder_handle ASC),
    // take MEMORY_DEPTH, drop handle-equal duplicates into a side list.
    // Return (handle, occupation index, people label, founded).
    todo!("replicate; see person_promote.rs:130-203")
}

/// claim: structural(seed: [42, 283, 705]) — three named worlds, built once
/// each. Not a search: the panel is the project's witness plus the two
/// census-range worlds the epoch's subject was found in.
#[test]
fn the_epoch_renames_every_founder_and_moves_nothing_else() {
    for seed in SEEDS {
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
        let world = build_world_to(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .unwrap_or_else(|e| panic!("seed {seed} failed to build: {e:?}"));
        let occs = occupation_records(&world);
        let v2 = select_founders(&occs);
        let v1 = legacy_cast(&occs);

        // The arm that must MOVE. Without it the equalities below are
        // satisfiable by a v2 key that equals v1.
        let v1_handles: BTreeSet<u64> = v1.iter().map(|(h, ..)| h.0).collect();
        let v2_handles: BTreeSet<u64> = v2.remembered.iter().map(|f| f.handle.0).collect();
        assert!(
            v1_handles.intersection(&v2_handles).next().is_none(),
            "seed {seed}: the two epochs share a handle — the epoch did not \
             rename, and every equality below is therefore vacuous"
        );

        // The arm that must HOLD: which occupations were promoted, for which
        // people, on which day. Compared by the position-free (people,
        // occupation index, founded) triple, since a handle-ordered tie-break
        // may legitimately reorder EQUAL-RANKING records.
        let key1: BTreeSet<(&'static str, usize, u64)> = v1
            .iter()
            .map(|(_, occ, people, founded)| (*people, *occ, founded.to_bits()))
            .collect();
        let key2: BTreeSet<(&'static str, usize, u64)> = v2
            .remembered
            .iter()
            .map(|f| (f.people.0, f.occupation, f.founded.to_bits()))
            .collect();
        assert_eq!(
            key1, key2,
            "seed {seed}: the epoch changed WHICH occupations are remembered, \
             not only what they are called. That is B2's falsifier: it means \
             `founder_handle` reaches promotion selection and not only \
             promotion naming."
        );
    }
}
```

Fill in `legacy_cast`'s body — no `todo!()` survives into the commit.

- [ ] **Step 2: Run it, and branch on the result**

```bash
cargo test -p hornvale-worldgen --test founder_epoch -- --nocapture 2>&1 | tee /tmp/hv-muster-t4.txt
```

| result | do this |
|---|---|
| both arms pass on all three seeds | **B2 confirmed with its positive control.** The epoch is a rename. Record the cast sizes per seed in the commit message. |
| the disjointness arm fails | The two keys agree somewhere. Either `legacy_cast` is calling the wrong key, or `founder_handle_v2` collapses onto v1 for some record shape. Find out which; a partial overlap is a real finding about the key. |
| the equality arm fails | **B2 is falsified. STOP and escalate.** Print the symmetric difference before you stop — which people, which occupation indices — and do not proceed to Task 5. This is the prediction that decides whether the campaign is safe (spec §4.5). |
| the cast is empty on any seed | The test proves nothing on that seed. Assert `!v2.remembered.is_empty()` before the comparisons so the vacuity cannot be silent, and investigate why. |

- [ ] **Step 3: Add the second arm — the drop backstop's own before/after**

Same file, second test: on 283 and 705, `legacy_cast` must record **one**
dropped founder each (the v1 behaviour `founder_collision.rs` pinned before
Task 3) and `select_founders` must record what Task 3 step 7 measured.

This is B1's named-subject evidence stated as a *difference* rather than as two
independently re-pinned constants, which is what makes it read as a claim about
the epoch instead of a pair of magic numbers. Write it so its failure message
says which epoch dropped what.

- [ ] **Step 4: fmt, clippy, targeted run, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo test -p hornvale-worldgen --test founder_epoch
```

```bash
cat > /tmp/hv-msg-t4.txt <<'EOF'
test(muster): B2 — the epoch renames every founder and moves nothing else

Both arms in one process, because v1 is retained. The arm that must move: the
v1 and v2 handle sets are disjoint on every seed in the panel. The arm that
must hold: the set of (people, occupation, founded) triples promoted is
identical. Without the first, the second is satisfiable by a key that did not
change at all.

Per-seed cast sizes: <...>
Drops, v1 -> v2: <...>
EOF
git commit -F /tmp/hv-msg-t4.txt -- windows/worldgen/tests/founder_epoch.rs
```

---

## Task 5: Prediction B1 — the residual over the census range

**Files:**
- Modify: `windows/worldgen/tests/founder_collision.rs` (module doc only, to
  carry the measured residual)
- Scratch only: the sweep probe

**Interfaces:**
- Consumes: everything Task 3 produced.
- Produces: the measured residual rate, for the chronicle and the registry row.

**Why this is a scratch probe and not a committed battery.** B1 is a
distribution claim over 3000 worlds — a census question by decision 0093's own
taxonomy. The reusable scatter helper (`map_seeds`) lives in
`windows/lab/tests/seed_sweep/mod.rs` and is not reachable from a
`hornvale-worldgen` test; a hand-rolled `std::thread::scope` sweep is, by
`cli/tests/heavy_tier.rs`'s own documented blind spot, invisible to the
serialization guard that keeps such batteries from oversubscribing the box.
Adding one properly is a larger piece of work than this campaign's claim needs.
So: measure once, promote the numbers into the chronicle and the module doc,
and keep the cheap named-seed pins in the commit gate. This is the same posture
The Radiation's own founder-collision diagnosis took.

- [ ] **Step 1: Write the sweep probe (uncommitted)**

Create `windows/worldgen/tests/muster_b1_sweep.rs`. **Do not `git add` it.**
For each seed in `0..3000`, build to `BuildDepth::Settlements`, take
`occupation_records`, and count (a) handle-sharing pairs across the whole
record set under v2, and (b) `select_founders(&occs).unremembered.len()`.
Print one line per non-zero seed plus a totals line:

```
B1-SEED seed=<s> whole_record_pairs=<n> cast_drops=<m>
B1-TOTAL seeds=3000 worlds_with_pairs=<a> total_pairs=<b> worlds_with_drops=<c> total_drops=<d>
```

Give it the verbatim heavy-tier ignore reason so it cannot join the commit gate
even by accident while it exists in your tree:

```rust
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
```

It loops seeds, so it needs a `/// claim:` line — this one is genuinely a
distribution claim: `/// claim: distribution(seeds 0..3000) — the residual
collision rate of the v2 key, measured once and promoted into the chronicle;
not a committed battery (see the task's note on `map_seeds` reach).`

- [ ] **Step 2: Run it, in the foreground, with an explicit long timeout**

```bash
cargo test -p hornvale-worldgen --test muster_b1_sweep -- --ignored --nocapture \
  > /private/tmp/claude-501/.../scratchpad/muster-b1.txt 2>&1; echo "exit=$?"
```

Budget: 3000 `Settlements` builds. The Radiation measured ~2 s each on the
optimized dev profile, so **expect ~100 minutes single-threaded.** If that is
beyond the session's budget, narrow the range deliberately — `0..1000` is the
census range and is the one that matters for the once-per-campaign census —
and **record the narrowing as a narrowing**, in the chronicle, with the range
you actually ran. A silently narrowed sweep reported as 3000 seeds is the
defect this campaign is named against.

- [ ] **Step 3: Branch on the totals**

| result | do this |
|---|---|
| `worlds_with_drops = 0` over the range run | **B1 confirmed.** Record the whole-record `total_pairs` too — the spec's scoring predicted 2 residual pairs in 3000 worlds, and whether that reproduces is the interesting number. |
| `worlds_with_drops > 0` | **B1 partially falsified.** Report the rate and the seeds (spec §4.4: *report the rate; do not widen further to chase it without saying so*). If any of them is inside `0..1000`, say so explicitly — that is the range the census sweeps, and it is the difference between "a rare fidelity cut" and "the census drops a founder". |
| `total_pairs` is far from the scored 2 | The Radiation's diagnosis was scored on a tree that has since moved. Report both numbers side by side; a changed residual is a fact about the world's occupation population, not about the key. |
| the run does not finish in budget | narrow per step 2 and say so |

- [ ] **Step 4: Promote the numbers, delete the probe**

Write the measured residual into `founder_collision.rs`'s module doc, replacing
the v1 figures it carries at `:1-33`. Then:

```bash
rm windows/worldgen/tests/muster_b1_sweep.rs
git status --porcelain    # must show no untracked file under windows/worldgen/tests/
```

- [ ] **Step 5: fmt, targeted run, commit**

```bash
cargo fmt
cargo test -p hornvale-worldgen --test founder_collision
```

```bash
cat > /tmp/hv-msg-t5.txt <<'EOF'
docs(muster): B1 — the widened key's residual over <range>

<B1-TOTAL line>

<Confirmed / partially falsified, with the rate.>

Measured by a throwaway sweep, not a committed battery: B1 is a distribution
claim, and the scatter helper a heavy battery would need lives in
windows/lab/tests and is not reachable from a worldgen test. The named-seed
pins in the commit gate stay; the distribution lives here and in the chronicle.
EOF
git commit -F /tmp/hv-msg-t5.txt -- windows/worldgen/tests/founder_collision.rs
```

---

## Task 6: Close — the sweep promoted, the record written, one census

**Files:**
- Create: `book/src/chronicle/the-muster.md`
- Create: `docs/retrospectives/the-muster.md`
- Create: `docs/decisions/0121-<slug>.md`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `book/src/SUMMARY.md`, `book/src/open-questions.md` (as the sweep
  requires)

- [ ] **Step 1: Promote the sweep out of scratch (spec §6) — before anything else**

The freeze, Amendment 1 and both result sets live in a throwaway scratch
directory that dies with the session. Carry into `book/src/chronicle/the-muster.md`:

- **The decision rule, verbatim**, and the fact that it was frozen before any
  measurement.
- **That the rule was answered on the wrong arm first**, and why: sweep 1
  varied λ globally, substituting along the **kind** axis while the hypothesis
  was a substitution along the **consumer** axis. Its SPLIT verdict was an
  artifact.
- **The control that made this legible, and it generalises:** for any
  one-scalar sweep over a per-kind quantity, *does the shipped configuration
  reproduce byte-identically somewhere on the grid?* Sweep 1 could not express
  that check; sweep 2 passed it three ways (35 component factors on
  `to_bits()`, the serialized `World` at `Full` depth on five seeds, and every
  readout's `SHIPPED` row equal digit-for-digit to its `λ=1.00` twin). **If a
  sweep cannot reproduce the shipped world, it is not interpolating it.**
- **The verdict:** all-bands-satisfied at λ ∈ {0.25, 0.50, 1.00, 1.20} with the
  shipped configuration interior → LEAVE; the registry row refuted by its own
  instrument.
- **Diversity is not an opposed party** — refuting the failure mode the
  preregistration named in advance, on both arms.
- **The known fragility, carried forward:** the majority-of-seeds reading
  condemns the shipped configuration via **sea-elf**, which places 2–3
  settlements a seed and ties its baseline exactly on 3 of 5 seeds. Widen the
  panel before spending any decision on a kind that thin.

Then the campaign's own results: Part A's roster flip and positive control,
Part B's B1 and B2 readings, and whichever predictions were falsified — carried
as headlines, not as footnotes.

- [ ] **Step 2: The retrospective (decision 0020)**

`docs/retrospectives/the-muster.md`. Process lessons only. The decision ledger
(`.superpowers/sdd/decision-ledger.md`) is git-ignored and dies with the
worktree — promote from it now, especially ledger entry **#4**, which is a
controller defect found by an ideonomy pass *before* it reached a spec, and is
the same family as The Radiation's twenty-four.

- [ ] **Step 3: The decision record**

`docs/decisions/0121-<slug>.md`, following the format of `0120`. The subject is
the founder-handle epoch: what changed, that v1 stays declared, that the drop
backstop stays because no key is total, and that `descent::founder_of` is a
separate handle this epoch does not move. Append-only; do not edit 0051, 0006
or 0120 — cite them.

- [ ] **Step 4: Registry rows**

```bash
grep -n "MEM-founder-handle-epoch\|BIO-affinity-level-is-two-quantities\|BIO-40" book/src/frontier/idea-registry.md
```

| row | action |
|---|---|
| `BIO-affinity-level-is-two-quantities` | **verify** it already reads `rejected` (it does at `:572`, set by the spec commit). If so, the DoD item is discharged; do not re-do it. |
| `MEM-founder-handle-epoch` | `raw` → `shipped`, with the measured residual from Task 5 and the **Where** cell pointing at `flesh.rs::founder_handle_v2`, `founder_epoch.rs`, and the chronicle |
| `BIO-40` | repoint: state that the diversity debt is **not** an affinity-level problem — diversity moves 0.039 (2.7%) across the entire λ range on both arms and never crosses `[1.5, 3.0]` at any λ. The debt stays where it is. |

Rows are five columns with `\|` escaped, Status from the closed vocabulary, no
new numbered IDs, no empty **Where** cell — `cli/tests/docs_consistency.rs`
enforces all of it inside `make gate`. **Do not cite a registry ID outside
`book/src/frontier/`**; that check is what The Gyre tripped on.

```bash
cargo test -p hornvale --test docs_consistency
```

- [ ] **Step 5: Book freshness sweep (decision 0013) and the Confidence Gradient (0030)**

Chapters that may now lag merged reality: the concept-registry chapter (new
`pub` surface in `domains/history`), any chapter describing founder identity or
the affinity ladder's level. Add the chronicle entry to `book/src/SUMMARY.md`.
If this campaign resolved or moved a bet in `book/src/open-questions.md`,
re-score that chapter as part of the sweep.

```bash
mdbook build book
```

- [ ] **Step 6: Regenerate, gate, commit the documentation**

```bash
make rebaseline; echo "exit=$?"
make rebaseline-goldens; echo "exit=$?"
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
make gate 2>&1 | tee /tmp/hv-muster-t6-gate.txt
```

`docs/digest/decisions-in-force.md` drifts whenever a decision record is added
— expect it here, and stage it in this commit.

```bash
git commit -F /tmp/hv-msg-t6.txt -- book/ docs/decisions/ docs/retrospectives/ docs/digest/
```

- [ ] **Step 7: Absorb main, then the census — in that order, authorization-gated**

```bash
make preflight
```

On an ancestry NO-GO, merge main **into** the branch and re-run the gate here.
Then regenerate artifacts again unconditionally: a generated artifact has no
merge, and The Radiation had two auto-merge cleanly and **wrong**, one dropping
four freshly-added registry predicates that nothing but regeneration caught.

Then, and only with Nathan's authorization, the one census refresh:

```bash
git push
bash scripts/census-run.sh status      # is a heavy run already holding the box?
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

A **full SHA**, never a branch name — `HV_CENSUS_REF` feeds `reset --hard` and
can land on a stale local branch of that name over there. Commit the
regenerated goldens **on lefford**, then push and fast-forward locally.

The census's re-pin sweep is the expensive half, not the census (spec §8; The
Radiation's cleared 47 reds and took a full agent). Budget it. Note the
expectation this campaign carries into it: the census's name-prefix metrics
read `descent::founder_of`, which this epoch does **not** move — so if census
rows move, read them before re-pinning.

- [ ] **Step 8: Definition of Done**

Check each against evidence you can point at, not against memory:

- [ ] Part A's guard is repaired, **names its roster**, and is mutation-proven
      to redden on an affinity-level change (Task 2 step 2's recorded mutation)
- [ ] Part B's key is widened, the drop backstop is retained
      (`person_promote.rs:161-169` still present), and B2's rename-only
      property is measured **with its positive control** (Task 4)
- [ ] Part C's three consumers are documented beside the derivation
- [ ] Registry: `BIO-affinity-level-is-two-quantities` `rejected` (verified);
      `MEM-founder-handle-epoch` `shipped`; `BIO-40` repointed
- [ ] Chronicle, retrospective, decision record, book freshness sweep,
      Confidence Gradient re-score if a bet moved
- [ ] One census refresh, on lefford, authorization-gated
- [ ] The sweep's freeze and both results promoted out of scratch (spec §6)
- [ ] `.superpowers/sdd/` promoted and **not** committed
- [ ] `git status --porcelain` clean — no orphan probe files, no zero-byte
      `.tmp` left by a truncated rebaseline

---

## Self-review (run at drafting, recorded here)

**1. Spec coverage.**

| spec section | task |
|---|---|
| §1.1 the open question and its answer | Task 6 step 1 (promotion), Task 6 step 4 (row already `rejected`) |
| §1.2 debt one — the level is guarded by nothing | Tasks 1, 2 |
| §1.3 debt two — a founder's identity | Tasks 3, 4, 5 |
| §2 non-goals ×4 | Global Constraints; Task 1 step 6 and Task 2 step 2 both branch on them |
| §3.1 the repair | Task 1 steps 2–3 |
| §3.2 the named roster instrument | Task 1 step 4 |
| §3.3 A1, mandatory positive control | Task 2 step 2 |
| §3.4 A2, the roster decides the verdict | Task 1 step 5 |
| §4.1 the widened key | Task 3 steps 3–4 |
| §4.2 the docstring's correction acted on | Task 3 step 4 item 3 |
| §4.3 the backstop stays | Global Constraints; Task 3 steps 4, 6; Task 3 step 7's branch table |
| §4.4 B1 | Task 3 step 7 (named seeds), Task 5 (distribution) |
| §4.5 B2 | Task 4 |
| §4.6 epoch discipline | Global Constraints; Task 3 steps 4, 9 |
| §5 Part C | Task 2 step 4 |
| §6 the sweep promoted | Task 6 step 1 |
| §7 DoD | Task 6 step 8 |
| §8 risks: sequencing, census cost, rebaseline exit 2 | Global Constraints; Task 6 step 7 |

**2. Placeholder scan.** One deliberate `todo!()` survives, in Task 4 step 1's
`legacy_cast`, and the step immediately below it says to fill it in. Every
other code block is complete or is explicitly a shape to adapt with the
adaptation named. No "add appropriate error handling", no "similar to Task N".

**3. Type consistency.** `founder_handle_v2(&OccupationRecord,
Option<&Occupation>, Option<&Occupation>) -> RoleHandle` is used with the same
signature in Tasks 3 and 4. `occupations_by_id(&[OccupationRecord]) ->
BTreeMap<EntityId, &Occupation>` is introduced in Task 3 step 2 with an
explicit alternative return type and an instruction to record which was chosen;
Task 4 consumes it by name. `select_founders(&[OccupationRecord]) ->
FounderCast` keeps its signature throughout. `FounderCast.remembered` /
`.unremembered` and `Founder.{handle, occupation, people, community, founded}`
are used exactly as declared at `person_promote.rs:28-73`.
