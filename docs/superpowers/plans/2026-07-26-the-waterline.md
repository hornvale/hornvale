# The Waterline Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the habitat model a concept of medium — a per-kind `HabitatDomain` gated against terrain's `is_ocean` as support restriction — so that no land kind holds ocean cells.

**Architecture:** One enum and one field on `BiosphereTraits` (a universal axis, beside `social_form`), authored `Terrestrial` for all sixteen shipped kinds, read by a single early-return in `niche_per_species_k`. No new field is derived: `terrain.is_ocean(cell)` already exists.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, `cargo nextest`, `mdbook`. Spec: `docs/superpowers/specs/2026-07-26-the-waterline-design.md`. Evidence: `windows/worldgen/tests/waterline_probe.rs` (`0041069f`).

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **No new dependencies.** `serde` + `serde_json` only, workspace-wide.
- **`#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **Type-audit tag grammar** — a primitive at a `pub` boundary carries its verdict as the LAST doc line, exactly `/// type-audit: bare-ok(<class>)`. For a struct field the verdict goes on the **struct's** doc comment. A malformed tag fails `make gate`. `HabitatDomain` is an enum, not a primitive, so it needs **no** tag — do not invent one.
- **`cargo fmt` is the final step before every commit.**
- **Run `make gate` before every commit.** Iterate with `cargo test -p <crate>`.
- **Run once, inspect many** — `2>&1 | tee /tmp/hv-wl.txt`, then grep the file.
- **Never run `HV_CENSUS=1` or `make gate-full`** without instruction: the census is an authorization-gated carve-out (Task 6), and `gate-full` rewrites committed benchmark artifacts as a side effect.
- **A falsified prediction is a finding to report, never a number to quietly re-pin.**

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `windows/worldgen/tests/waterline_probe.rs` | measurement only; extended in Task 1 | 1 |
| `domains/species/src/lib.rs` | `HabitatDomain` enum + `BiosphereTraits` field + 16 authored rows | 2 |
| `windows/worldgen/src/lib.rs` (`niche_per_species_k`) | the gate | 3 |
| `windows/worldgen/src/lib.rs` (`mod tests`) | dominance-fallout tests, the `#[ignore]`d target | 4 |
| `cli/tests/fixtures/world-seed-42.json` | **unchanged** — measured byte-identical | — |
| census goldens | Task 6, verify-then-authorize | 6 |

---

### Task 1: Measure P4 before building anything

**Goal:** answer "does settlement placement move?" with a throwaway gate. If it does, the campaign's shape changes and the owner must know at task 1.

**Files:**
- Modify (TEMPORARILY, reverted in this task): `windows/worldgen/src/lib.rs`
- Modify (kept): `windows/worldgen/tests/waterline_probe.rs`

- [ ] **Step 1: Record the baseline**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/wl-before.json
```

Expected (this branch's base includes The Vigil): `world of seed 42 written to /tmp/wl-before.json (3553 facts; village: Qvooshtvoagootao)`. **Record what it actually prints** — do not assume this line; if it differs, that is the baseline and you say so.

- [ ] **Step 2: Apply a throwaway gate**

In `windows/worldgen/src/lib.rs`, inside `niche_per_species_k`'s `CellMap::from_fn` closure, as the very first statement:

```rust
                // TEMPORARY (The Waterline Task 1, reverted in this task):
                // measure whether support restriction moves settlement
                // placement. Not the shipped form — no authored field yet.
                if terrain.is_ocean(cell) {
                    return 0.0;
                }
```

- [ ] **Step 3: Measure**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/wl-after.json
cargo test -p hornvale --test lens_purity 2>&1 | tail -5
```

Record: the fact count, the village name, and whether `seed_42_world_json_matches_the_committed_fixture` passes.

- [ ] **Step 4: Diff the worlds**

```bash
python3 - <<'PY'
import json
a=json.load(open('/tmp/wl-before.json')); b=json.load(open('/tmp/wl-after.json'))
fa=a['ledger']['facts'] if 'facts' in a.get('ledger',{}) else a['ledger']
fb=b['ledger']['facts'] if 'facts' in b.get('ledger',{}) else b['ledger']
print('facts before/after:', len(fa), len(fb))
sa={(f['subject'],f['predicate'],json.dumps(f['object'])) for f in fa}
sb={(f['subject'],f['predicate'],json.dumps(f['object'])) for f in fb}
print('only-before:', len(sa-sb), ' only-after:', len(sb-sa))
for row in list(sa-sb)[:10]: print('  -', row)
for row in list(sb-sa)[:10]: print('  +', row)
PY
```

If the JSON shape differs from what this script assumes, adapt it — the goal is the count of facts that changed, and a sample of them.

- [ ] **Step 5: Record the answer in the probe**

Add a comment block at the top of `windows/worldgen/tests/waterline_probe.rs` recording the measured result verbatim (baseline line, gated line, fixture pass/fail, changed-fact count). This is the campaign's evidence and must survive the throwaway edit.

- [ ] **Step 6: Revert the throwaway gate**

```bash
git checkout -- windows/worldgen/src/lib.rs
git diff --stat   # must show ONLY the probe file
```

- [ ] **Step 7: Commit and REPORT**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add -A && git commit -m "probe(the-waterline): measure P4 — does support restriction move settlement placement"
```

**Then stop and report the P4 answer before starting Task 2.** If placement moved, the remaining tasks need re-scoping (world-identity drift, fixture re-pin, possibly an epoch conversation) and that is the owner's call.

---

### Task 2: `HabitatDomain`, authored and inert

**Files:**
- Modify: `domains/species/src/lib.rs`

**Interfaces:**
- Produces: `hornvale_species::HabitatDomain` and `BiosphereTraits::habitat_domain`, read by Task 3. **NOTE: Task 2 shipped three variants and authored all sixteen kinds `Terrestrial`. Task 3 adds the fourth variant (`Lithic`) and re-authors xorn — see Task 3 Step 0.**

- [ ] **Step 1: Write the failing test**

Add to `domains/species/src/lib.rs`'s `mod tests`:

```rust
    #[test]
    fn every_shipped_kind_declares_a_habitat_domain_and_all_are_terrestrial() {
        // The Waterline v1: the mechanism is general, the content degenerate.
        // No shipped kind is Aquatic yet — the aquatic roster is the sequel —
        // so this asserts the ROSTER FACT, and a future marine kind is meant
        // to fail here and be added deliberately, not slip in unnoticed.
        let bio = biosphere_registry();
        assert_eq!(bio.len(), 16, "sixteen kinds compete for space");
        for (kind, traits) in bio.iter() {
            assert_eq!(
                traits.habitat_domain,
                HabitatDomain::Terrestrial,
                "{kind:?} is terrestrial in v1"
            );
        }
    }
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-species every_shipped_kind_declares 2>&1 | tail -10
```

Expected: FAIL — `cannot find type HabitatDomain`.

- [ ] **Step 3: Add the enum**

In `domains/species/src/lib.rs`, immediately before `BiosphereTraits`:

```rust
/// The medium a kind's body lives in — a universal biosphere axis, like
/// [`SocialForm`] (decision 0065). Every kind with a body is in some medium,
/// so this is a field on [`BiosphereTraits`] rather than an optional
/// component: it is not a capacity only some kinds carry.
///
/// Read by worldgen's carrying-capacity layer as **support restriction**: a
/// kind outside its medium has zero carrying capacity there, full stop, rather
/// than a small one. Softening this to a low score would not work — dominance
/// is an argmax, and a cell with only small values still has a largest one
/// (spec: The Waterline §3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HabitatDomain {
    /// Lives above the waterline. Every shipped kind, in v1.
    Terrestrial,
    /// Lives below it. No shipped kind yet — the aquatic roster is this
    /// campaign's sequel, and the variant ships ahead of its first holder so
    /// that adding one is authoring rather than a code change.
    Aquatic,
    /// At home in both — a shore-dweller, an otter, a crocodilian.
    Amphibious,
}
```

- [ ] **Step 4: Add the field**

In `BiosphereTraits`, immediately after `social_form`:

```rust
    /// The medium this kind's body lives in (The Waterline). Gated at the
    /// carrying-capacity layer: a kind outside its medium cannot be there.
    pub habitat_domain: HabitatDomain,
```

- [ ] **Step 5: Author all sixteen rows**

Add `habitat_domain: HabitatDomain::Terrestrial,` to every `BiosphereTraits { .. }` literal in `biosphere_registry()`. There are sixteen. Place it directly after each row's `social_form` line so the diff reads as one column added.

The two synthetic literals in `windows/worldgen/src/lib.rs` (~6649, ~8264) use `..goblin_bio` struct-update syntax and inherit the field — **do not edit them**. Lab's rosters in `windows/lab/src/roster.rs` clone from `biosphere_registry()` and likewise inherit.

- [ ] **Step 6: Run — the test must pass and nothing else may move**

```bash
cargo test -p hornvale-species 2>&1 | tail -10
cargo run -q -p hornvale -- new --seed 42 --out /tmp/wl-t2.json
```

Expected: species tests green, and the seed-42 line **identical to Task 1 Step 1's baseline** — the field is authored but unread, so this task is byte-identical.

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add -A
git commit -m "feat(the-waterline): HabitatDomain — the medium a kind lives in

A universal biosphere axis beside social_form (0065): every kind with a body is
in some medium. All sixteen shipped kinds are Terrestrial, so this commit is
byte-identical — the field is authored and not yet read. Task 3 gates on it.

The Aquatic variant ships ahead of its first holder so that adding a marine
kind later is authoring, not a code change."
```

---

### Task 3: The gate (plus the `Lithic` variant)

- [ ] **Step 0: Add `Lithic` and re-author xorn**

Task 2 shipped `Terrestrial | Aquatic | Amphibious` and authored all sixteen
kinds `Terrestrial`. Add the fourth variant to `HabitatDomain` in
`domains/species/src/lib.rs`:

```rust
    /// Lives *in the substrate*, which underlies both land and sea floor, and
    /// is therefore indifferent to the waterline above it. A xorn swims
    /// through stone; the ocean over its head is not its medium. Shares
    /// `Amphibious`'s permit-everywhere gate in v1 but makes a different
    /// claim — `Amphibious` is at home in both media and moves between them;
    /// `Lithic` is in neither, in a third medium that underlies both. When a
    /// future campaign gives the substrate its own extent, this gains a real
    /// gate and `Amphibious` does not.
    Lithic,
```

Re-author **xorn only** to `HabitatDomain::Lithic` (`Ametabolic`, pure-`MINERAL`,
burrows through stone). `rust-monster` stays `Terrestrial` — same pure-`MINERAL`
niche, but an `Ectotherm` that walks around eating metal objects, so it lives
*on* the surface, not *in* it.

Update Task 2's roster test, which asserts every kind is `Terrestrial`: it must
now assert fifteen `Terrestrial` and xorn `Lithic`, **by name**, so a future
re-authoring is a deliberate visible change.

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`niche_per_species_k`)
- Test: `windows/worldgen/src/lib.rs` (`mod tests`)
- Possibly re-pin: `cli/tests/fixtures/world-seed-42.json` (only if Task 1 showed drift)

**Interfaces:**
- Consumes: Task 2's `habitat_domain`.

- [ ] **Step 1: Write the failing tests**

Add to `windows/worldgen/src/lib.rs`'s `mod tests`:

```rust
    #[test]
    fn no_terrestrial_kind_holds_carrying_capacity_below_the_waterline() {
        // The Waterline's whole claim (P1). Before the gate the goblin held
        // 930 ocean cells and the xorn 25,982; the mechanism was that
        // `habitability` asks whether the CLIMATE is livable (open ocean
        // passes easily) and that MINERAL/DETRITUS bypass it entirely.
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _y, _yp) = stellar_inputs(&sky);
        let wc = WorldComponents::assemble().unwrap();
        let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
        let bio: Vec<&hornvale_species::BiosphereTraits> =
            wc.biosphere.iter().map(|(_, b)| b).collect();
        let ks = niche_per_species_k(
            geo, &terrain, &climate, obliquity_deg, insolation_scalar, &regime, &bio,
        );
        let mut offenders: Vec<(&str, usize)> = Vec::new();
        for (tag, k) in &ks {
            let wet = geo
                .cells()
                .filter(|&c| terrain.is_ocean(c) && *k.get(c) > 0.0)
                .count();
            if wet > 0 {
                offenders.push((names[*tag as usize], wet));
            }
        }
        assert!(
            offenders.is_empty(),
            "terrestrial kinds hold capacity below the waterline: {offenders:?}"
        );
    }

    #[test]
    fn the_gate_removes_cells_without_reweighting_the_survivors() {
        // P5: support restriction takes cells OUT of contention; it must not
        // change the relative standing of kinds on land. Compare each pair's
        // K ratio on a land cell against the same ratio computed from the
        // ungated formula (supply x conditions), which the gate does not touch.
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _y, _yp) = stellar_inputs(&sky);
        let wc = WorldComponents::assemble().unwrap();
        let bio: Vec<&hornvale_species::BiosphereTraits> =
            wc.biosphere.iter().map(|(_, b)| b).collect();
        let ks = niche_per_species_k(
            geo, &terrain, &climate, obliquity_deg, insolation_scalar, &regime, &bio,
        );
        let land: Vec<_> = geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();
        assert!(!land.is_empty(), "seed 42 has land");
        let mut any_positive = false;
        for &c in land.iter().take(200) {
            for (_, k) in &ks {
                if *k.get(c) > 0.0 {
                    any_positive = true;
                }
            }
        }
        assert!(
            any_positive,
            "the gate must leave land capacity intact, not zero the world"
        );
    }
```

- [ ] **Step 2: Run and watch the first fail**

```bash
cargo test -p hornvale-worldgen --lib -- no_terrestrial_kind_holds the_gate_removes 2>&1 | tail -20
```

Expected: `no_terrestrial_kind_holds_...` FAILS naming offenders (xorn, rust-monster, twig-blight, goblin at minimum). The second test passes already — it is the guard against over-correction.

- [ ] **Step 3: Add the gate**

In `niche_per_species_k`, as the first statement inside `CellMap::from_fn(geo, |cell| { ... })`:

```rust
                // THE WATERLINE: support restriction. A kind outside its
                // medium has no carrying capacity here at all — not a small
                // one. Gated at K rather than per supply axis because the
                // defect had two independent sources (climate's
                // `habitability` is not a land test, and MINERAL/DETRITUS
                // bypass it), and because the claim is "this kind cannot be
                // here", not "this kind finds no food here". A small value
                // would not do: dominance is an argmax, so a cell containing
                // only small values still has a largest one.
                let wet = terrain.is_ocean(cell);
                let permitted = match bio.habitat_domain {
                    hornvale_species::HabitatDomain::Terrestrial => !wet,
                    hornvale_species::HabitatDomain::Aquatic => wet,
                    hornvale_species::HabitatDomain::Amphibious => true,
                };
                if !permitted {
                    return 0.0;
                }
```

- [ ] **Step 4: Run — both must pass**

```bash
cargo test -p hornvale-worldgen --lib -- no_terrestrial_kind_holds the_gate_removes 2>&1 | tail -10
```

- [ ] **Step 5: Check world identity against Task 1's measured answer**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/wl-t3.json
cargo test -p hornvale --test lens_purity 2>&1 | tail -5
```

The result must match what Task 1 measured. If Task 1 said placement does not move, `lens_purity` must pass untouched; if Task 1 said it does, re-pin the fixture **in this commit** (`REBASELINE=1 cargo test -p hornvale --test lens_purity`) and state the drift in the commit message. **A result that disagrees with Task 1 is a finding — stop and report.**

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt && make gate 2>&1 | tail -20
git add -A
git commit -m "feat(the-waterline): gate carrying capacity on the waterline"
```

Expect `make gate` to surface dominance-related failures here — those are Task 4's, and it is correct that they appear now. If the gate is red, commit nothing and proceed to Task 4 on the working tree, committing both together.

---

### Task 4: The fallout, re-pinned in the commit that drifts it

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`mod tests`), plus any test the gate reddens.

- [ ] **Step 1: Enumerate the damage in one pass**

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/wl-fallout.txt | tail -30
grep -E "^\s+FAIL" /tmp/wl-fallout.txt
```

- [ ] **Step 2: Re-measure the dominance table**

```bash
cargo test -p hornvale-worldgen --test waterline_probe -- --nocapture --ignored 2>&1 | sed -n '/dominance by land/,/world has/p'
```

Record the new table. **P1** requires rust-monster's ocean column to be zero. **P2** requires xorn to be unaffected directly — its own K field must be identical before/after (it is `Lithic`, permit-everywhere); its *dominance* count is expected to move upward instead, as it inherits the ocean cells rust-monster vacates. **P3** is whatever it is — if the goblin still dominates nothing, report that as a finding about the peoples' competitive position; do not tune.

- [ ] **Step 3: Update each reddened assertion to the measured value**

For every failing test, change the pinned number to the newly measured one **and** update its comment to say why it moved (the waterline removed ocean cells from contention). Do not weaken an assertion into a range to make it pass.

For the `#[ignore]`d `≥6`-distinct-dominants target, update its ignore-reason to state the post-waterline count. It stays `#[ignore]`d.

- [ ] **Step 4: Full gate**

```bash
cargo fmt && make gate 2>&1 | tail -20
```

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "test(the-waterline): re-pin the dominance fallout"
```

---

### Task 4b: Sovereignty stops being a floor — DEFERRED

**This task was attempted, built, measured, and then reverted; it does not
ship in this campaign.** Full account: spec §4.4. Summary: the revision is
correct on its own terms (it replaces an unconditional floor with real,
distance-decaying gap-closing, and reproduces today's formula exactly at
`s = 0`), but it invalidates the β = 2.0 coexistence-diversity calibration —
that constant was frozen under the old response shape, and under the new one
mean diversity falls to 1.333 against a preregistered band of [1.5, 3.0] on
every seed. Fixing that properly needs a re-sweep of β, which is a
calibration study and its own campaign, not a line of arithmetic bolted onto
a medium gate.

The implementation is preserved at commit `4f852fd2` (recoverable by SHA) for
that sequel campaign to cherry-pick rather than re-derive from scratch.

---

### Task 5: Re-pin the dominance fallout

**Files:** `windows/worldgen/src/lib.rs` (`mod tests`), `windows/worldgen/tests/demesne.rs`, and any other test the two changes redden.

- [ ] **Step 1: Enumerate the damage in one pass**

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/wl-fallout.txt | tail -30
grep -E "^\s+FAIL" /tmp/wl-fallout.txt
```

- [ ] **Step 2: Update each reddened assertion to its measured value**

Change the pinned number to the newly measured one **and** update its comment to say why it moved (which of the two changes, and in which direction). Do not weaken an assertion into a range to make it pass. A falsified preregistered prediction is reported, not tuned away.

- [ ] **Step 3: Gate and commit**

```bash
cargo fmt && make gate 2>&1 | tail -20
git add -A && git commit -m "test(the-waterline): re-pin the dominance fallout"
```

---

### Task 6: Census — verify first, then request authorization

- [ ] **Step 1: Determine whether any census metric actually moves**

World identity is byte-identical (P4), so any metric reading committed facts is unchanged. Only metrics reading density or dominance can move. Identify them by inspection before running anything:

```bash
grep -rn "density\|dominan" windows/lab/src/metrics.rs | head -30
```

- [ ] **Step 2: If none move, say so and skip the regen.** Record the reasoning.

- [ ] **Step 3: If any move, STOP and request authorization.** The census is a standing carve-out; do not run `HV_CENSUS=1` before receiving it.

---

### Task 7: Whole-branch verification

- [ ] **Step 1: Full gate, captured once**

```bash
make gate 2>&1 | tee /tmp/wl-gate.txt | tail -30
```

- [ ] **Step 2: Heavy tier**

```bash
make gate-full 2>&1 | tee /tmp/wl-gatefull.txt | tail -30
```

Revert the `book/src/laboratory/generated/the-sounding/` churn afterwards — it is timing-derived benchmark noise, not a result.

- [ ] **Step 3: Artifact freshness**

```bash
make rebaseline
git status --porcelain
```

- [ ] **Step 4: Walk the spec against the diff** — P1–P7 and both blast-radius tables against `git diff main...HEAD`.

- [ ] **Step 5: Report** every prediction with its verified outcome.

## Self-Review

**Spec coverage:** §4.1 → Task 2. §4.2 → Task 3. §4.3 (habitability untouched) → nothing edits it; verified in Task 6 Step 4. §5 P1/P2/P3 → Task 4 Step 2; P4 → Task 1; P5 → Task 3 Step 1. §6 consumers → Tasks 2–4; committed artifacts → Tasks 3 (fixture), 5 (censuses), 6 (freshness). §7 → Task 1's placement first in the order. §8 → Task 5's gate. §9 (The Demesne's chronicle) → **not a task**: it is a close-time judgment reserved to the owner, and belongs in the close walk rather than the plan.

**Placeholders:** none — every code step carries its code, every command its expected output.

**Type consistency:** `HabitatDomain` (Task 2) is referenced by Task 3's match arms with the same three variant names. `BiosphereTraits::habitat_domain` is the field name in both. `terrain.is_ocean(cell) -> bool` matches the existing signature at `domains/terrain/src/provider.rs:101`.

**Known risk this plan accepts:** Task 3 may leave the gate red until Task 4 re-pins. That is deliberate — the alternative is re-pinning numbers before the change that moves them, which inverts cause and effect.

---

**Status: SUPERSEDED IN PART, CLOSED** (2026-07-26). Tasks 1–3 were executed and then **removed** at `559a1510`: The Tumult shipped a better-argued fix for the same defect while this plan was being executed (see the spec's §12). Task 4b (sovereignty) was built, measured, and deferred to its own campaign — the implementation is preserved at `4f852fd2` and the finding is registry row BIO-40. Task 6's census verification and regeneration did run, on the canonical box, and taught its own lesson (UNI-39).

For anyone reading this plan as a template, the two things worth taking from it: **Task 1's measure-before-you-build step worked** — it answered the world-identity question in one task rather than five, and its answer held every subsequent time it was rechecked. And **the plan specified a test that asserted nothing** (`the_gate_removes_cells_without_reweighting_the_survivors`), which the final review caught by mutation. A plan can hand an implementer a vacuous test just as easily as an implementer can write one.
