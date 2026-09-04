# The Zenith Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Delete Hornvale's astronomy provider-tier system — `ConstantSun`,
`Sky::Constant`, `SkyChoice` — so that every world has a sky with a calendar
and a star system.

**Architecture:** Four mechanical stages in a forced order. The 23
constant-sky sites are flipped **while `SkyChoice` still exists**, so the
semantic diff is 23 reviewable hunks; only then is the parameter deleted
across all 405 sites, compiler-driven. Then the `Option`-ness collapses, and
every guard that existed only for the acyclic case is deleted rather than
stubbed.

**Tech Stack:** Rust 2024, `cargo nextest`, `make gate-commit`,
`scripts/regenerate-artifacts.sh`.

**Spec:** [`2026-09-04-the-zenith-design.md`](../specs/2026-09-04-the-zenith-design.md)
**Ledger:** [`2026-09-04-the-zenith.md`](../ledgers/2026-09-04-the-zenith.md)
**Decision block:** 0736–0745 (reserved).

---

## Global Constraints

- **Dependencies are frozen**: `serde`, `serde_json`, `libm` only. This
  campaign adds none.
- **No `HashMap`/`HashSet`** anywhere (`clippy.toml` `disallowed-types`).
- **`cargo fmt` is the last step before every commit.** Fmt-gate skips are
  this project's most common review finding.
- **Every commit runs `make gate-commit` via the pre-commit hook** and must
  pass. Never `--no-verify`.
- **Every public item, field and variant carries a one-line doc comment**
  (`#![warn(missing_docs)]` workspace-wide).
- **Determinism is constitutional.** Same seed + same pins → byte-identical
  worlds. This campaign changes which world seed 42 denotes for the
  *keystone almanac only* (§4.6 of the spec); it must not change
  `cli/tests/fixtures/world-seed-42.json`, which is already a generated-sky
  world.
- **Push the branch at every task boundary.**
- **Absorb `main` at every stage boundary** via
  `make sluice-stage BRANCH=campaign/the-zenith REF=<full-sha>`.

---

## Three traps, verified in the code, that this plan exists to route around

Read these before Task 1. Each was found by reading the tree, not by
reasoning, and each one silently produces a plausible wrong result.

### Trap 1 — stacked guards: one is the tier, one is a locked world

The `Option` guards do **not** all belong to tier-0. A locked world is a real
physical regime with **no day**, and its guards must survive. Three verified
instances where a tier-0 guard and a locked-world guard sit adjacent:

```rust
// windows/worldgen/src/lib.rs:5192 — the first goes, the second STAYS
let Some(calendar) = sky.calendar() else { return Ok(0.0); };            // tier-0  -> DELETE
let Some(day_len) = calendar.day_length() else { return Ok(0.0); };      // locked  -> KEEP
```

```rust
// windows/vessel/tests/suite/the_detent.rs:153 — same shape, different syntax
.and_then(|sky| sky.calendar().cloned())   // tier-0 -> becomes .map(|sky| sky.calendar().clone())
.and_then(|c| c.day_ticks())               // locked -> KEEP as .and_then
```

```rust
// windows/worldgen/src/lib.rs:14387
fn observation_time_is_zero_for_constant_and_locked_skies()  // loses HALF its subject, not all of it
```

**The rule:** `Sky::calendar()` / `Sky::system()` returning `Option` is the
tier. `Calendar::day_length()` returning `Option` is **tidal locking** and is
untouched by this campaign. Before deleting any guard, name which of the two
it is.

### Trap 2 — "tier" in this repo does not always mean an astronomy tier

`windows/scene/examples/tier_comparison_spike.rs` uses "tier 0 / tier 1 /
tier 2" to mean **rendering vocabularies** (plain ASCII / 16-colour ANSI /
CP437 + 24-bit colour). It has nothing to do with astronomy providers. A
`grep -r tier` sweep hits it. **Do not touch that file.** Its own module doc
says it is a throwaway spike; leave it exactly as it is.

### Trap 3 — a committed artifact carries tier-0 prose that regeneration will not fix

`kernel/examples/first_light.rs` is run by
`scripts/regenerate-artifacts.sh:1002` and writes
`book/src/gallery/world-seed-42.md`. Line 152 **hardcodes** the sentence:

> Under a golden sun fixed at zenith, in an unnamed vale, stands the goblin
> village of **Bolnar**.

The example imports **only `hornvale_kernel`** — it cannot reach
`ConstantSun` and does not derive that sentence from anything. So deleting
the tier leaves the sentence in place, regeneration reproduces it
byte-identically, and the drift check compares the file against itself and
reports **no drift, forever**. It is a silently-false committed artifact that
no gate can see. Task 6 fixes it by hand.

---

## File Structure

| file | responsibility after this campaign |
| --- | --- |
| `domains/astronomy/src/lib.rs` | loses `ConstantSun` and its two unit tests; `GeneratedSky` is the only provider |
| `domains/astronomy/tests/suite/sky_conformance.rs` | **renamed from** `tier_refinement.rs`; asserts what a Hornvale sky *is*, with frozen literals |
| `windows/worldgen/src/lib.rs` | `Sky` becomes a one-provider newtype; `calendar()`/`system()` non-`Option`; `sky_of` errors on an absent fact; `SkyChoice` gone from `build_world`/`build_world_to` |
| `cli/src/main.rs` | `--sky` flag and its usage line gone; `parse_sky_args` returns `SkyPins` only |
| `clients/world-wasm/src/lib.rs` | `"sky"` pin key rejected as unknown |
| `scripts/regenerate-artifacts.sh` | builds two seed-42 almanacs, not three worlds |
| `book/src/gallery/almanac.md`, `the-sky.md` | one spinning almanac, one locked; no duplication |
| `CLAUDE.md`, `domains/CLAUDE.md`, the Constitution | state the doctrine as retired, not live |
| `docs/decisions/0736`, `0737`, `0738` | the ratified record |

---

### Task 1: The artifact — collapse the two seed-42 worlds

**Files:**
- Modify: `scripts/regenerate-artifacts.sh:192-193, 228-229, 1050-1051`
- Modify: `book/src/gallery/almanac.md`
- Modify: `book/src/gallery/the-sky.md`
- Modify: `book/src/gallery/the-gods-seed-42.md:39,110`
- Modify: `book/src/gallery/the-meeting-seed-42.md:39`
- Modify: `book/src/chronicle/campaign-y2-0.md:134`
- Delete: `book/src/gallery/almanac-seed-42-sky.md`

**Interfaces:**
- Consumes: nothing.
- Produces: `book/src/gallery/almanac-seed-42.md` now holds the **generated
  spinning** seed-42 almanac (previously the tier-0 one). Later tasks assume
  no `-sky` suffixed artifact exists.

- [ ] **Step 1: Confirm the two worlds differ only by the flag**

Run:
```bash
sed -n '192,193p;228,229p' scripts/regenerate-artifacts.sh
```
Expected: `$w42` is commented "seed 42, tier-0 constant sun", `$wsky` is
"generated sky (default)"; the two `new` lines differ only in
`--sky constant`. If they differ in any other argument, **STOP** — the
collapse in Step 2 is not valid and the plan needs revising.

- [ ] **Step 2: Collapse `$w42` into `$wsky`**

Delete the `$w42` variable, its `new` invocation, and repoint its one reader.
`$w42` has exactly one reader (`almanac-seed-42.md`); verify before editing:

```bash
grep -n 'w42' scripts/regenerate-artifacts.sh
```

Then: delete the `w42=` line (192) and the `spawn run … --sky constant` line
(228); change line 1050 to read from `$wsky`; delete line 1051 (the
`-sky` almanac) entirely. Update the Group A/B comments at lines 45, 48, 54,
66, 75, 791, 988, 1035, 1040, 1049 that name `$w42` so they name only
`$wsky`/`$wlocked`.

- [ ] **Step 3: Repoint the book**

`book/src/gallery/almanac.md` — replace the framing paragraph. It currently
calls the artifact "Campaign 1b's exit artifact", which was the tier-0 world:

```markdown
# The Almanac of Seed 42

The same document `hornvale almanac` prints, generated from seed 42's world
under default pins and regenerated by `make rebaseline`, which fails if this
committed copy ever goes stale. When this page changes, the world changed.

Campaign 1b's original exit artifact was this page under a *stipulated*
sky — a sun fixed at zenith, no calendar, no cycles. That provider was
retired by The Zenith (decision 0736); the page below is the same seed under
the generated sky it has had since Campaign 2b.

---

{{#include almanac-seed-42.md}}
```

`book/src/gallery/the-sky.md` — change the two includes at the bottom to one.
The spinning almanac now lives in `almanac.md`; including it here as well
would print the same page twice in one book. Replace the trailing
`{{#include almanac-seed-42-sky.md}}` / `---` / `{{#include
almanac-seed-42-locked.md}}` block with:

```markdown
The spinning almanac is [The Almanac of Seed 42](./almanac.md); the locked
one follows here, so the pair can be read against each other.

---

{{#include almanac-seed-42-locked.md}}
```

Also update that chapter's own sentence "Regenerated in CI alongside the
constant-sky almanac" — there is no constant-sky almanac now, and **there is
no CI** (decision 0125). Replace with "Regenerated by `make rebaseline`; if
either page goes stale, the drift check fails."

- [ ] **Step 4: Repoint the three inbound links**

```bash
grep -rn 'almanac-seed-42-sky' book/src/
```
Each hit becomes `almanac-seed-42.md` (and `./almanac.md` where the link is
to the *chapter* rather than the artifact). Expected hits:
`the-gods-seed-42.md:39,110`, `the-meeting-seed-42.md:39`,
`chronicle/campaign-y2-0.md:134`. `chronicle/the-scaffold.md:84` mentions
`almanac-seed-42*.md` as a glob in historical prose — **leave it**, it is a
record of what a past campaign predicted.

- [ ] **Step 5: Regenerate and read the branch table**

Run:
```bash
make rebaseline 2>&1 | tail -20
git status --short
```

**Branch table — do not treat any of these as "expected drift":**

| what moved | do this |
| --- | --- |
| only `book/src/gallery/` and `docs/audits/` | correct — commit together in Step 7 |
| `cli/tests/fixtures/` | **STOP.** Those goldens are already generated-sky (spec §3). Nothing in this task may move them. Report and halt. |
| `book/src/domesday/` or `docs/digest/` | **STOP.** Neither reads a seed-42 almanac. Report and halt. |
| anything else | **STOP** and report before committing. |

- [ ] **Step 6: Verify the collapse actually happened**

Run:
```bash
git diff --stat book/src/gallery/almanac-seed-42.md
ls book/src/gallery/almanac-seed-42-sky.md 2>&1
mdbook build book 2>&1 | tail -5
```
Expected: `almanac-seed-42.md` shows a large diff (it changed worlds — ~1741
diff lines against the old content); `almanac-seed-42-sky.md` does not exist;
the book builds with no broken-link warnings.

**A near-empty diff on `almanac-seed-42.md` means the flag was not actually
dropped** — the regeneration rebuilt the same tier-0 world. Go back to Step 2.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "artifact(the-zenith): one seed-42 almanac, generated sky

Drops --sky constant from the keystone almanac's regeneration line and
collapses \$w42 into \$wsky. The two worlds differed only by that flag, so
almanac-seed-42.md and almanac-seed-42-sky.md were about to become
byte-identical; the -sky suffix only ever meant 'not the tier-0 one' and
names nothing now.

gallery/almanac.md keeps the plain name and says what happened; the-sky.md
stops including the spinning almanac inline (it would print the same page
twice) and becomes the spinning-vs-locked contrast it always meant to be.

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 2: The 23 sites — classify, then flip or retire

This is the campaign's only semantically risky task. `SkyChoice` still
exists throughout; every hunk is a deliberate decision.

**Files:**
- Modify: `cli/src/repl.rs:512-520, 557-565, 708-712, 742-, 781-789, 862-870`
- Modify: `cli/src/main.rs:2778-2790`
- Modify: `cli/tests/suite/repose_byte_identity.rs:79-96, 156`
- Modify: `cli/tests/suite/exit_criterion.rs:78-102`
- Modify: `windows/worldgen/src/lib.rs:12094-12100, 12529-12535, 13114-13136, 13423-13432, 13465-13471, 13853-, 14385-14393`
- Modify: `windows/worldgen/tests/suite/pin_enumeration.rs:45-70`
- Modify: `windows/book/src/lib.rs:3161-3205`
- Modify: `windows/scene/src/lib.rs:1512-1525, 2008-2020, 2262-, 2338-`
- Modify: `windows/scene/tests/suite/golden.rs:15-22`
- Modify: `windows/scene/examples/illumination_probe.rs:575-583`
- Modify: `clients/game/bin/tests/wash.rs:444-465`
- Modify: `windows/locale/tests/suite/surface_mixture.rs:180` (comment only)

**Interfaces:**
- Consumes: Task 1's single seed-42 almanac.
- Produces: zero `SkyChoice::Constant` sites outside the surface that Task 3
  deletes (`cli/src/main.rs:240`, `cli/src/main.rs:2554`,
  `clients/world-wasm/src/lib.rs:95`, `windows/worldgen/src/lib.rs:8136`,
  `pin_enumeration.rs:66`).

- [ ] **Step 1: Enumerate the population and record it**

Run:
```bash
grep -rn "SkyChoice::Constant" --include=*.rs . | grep -v '^./target' | tee /tmp/zenith-23.txt
wc -l /tmp/zenith-23.txt
```
Expected: **23** lines. If the count differs, `main` moved under this branch
— absorb first (`make sluice-stage`) and re-run.

- [ ] **Step 2: Classify each of the 23 by reading it — do not guess from the filename**

For each site, decide **A** or **B** by this rule, and write the verdict into
the task report:

- **A — the tier is the SUBJECT.** The test's assertions are *about* tier-0
  behaviour: it asserts the word "zenith", asserts `calendar().is_none()`,
  asserts an error path whose only producer is tier-0, or asserts the
  `sky_of` fallback. **These tests retire.** Their evidentiary job completed
  at their own campaign's merge — the same reasoning decision 0039 gives for
  retiring a keystone at an epoch.
- **B — the tier is SCAFFOLDING.** The test wants *a world*; the tier was how
  it got one cheaply. **These flip**, per Step 3.

**Verified kind-A candidates** (confirm each by reading before retiring —
this list is a starting point, not an instruction):

| test | why it is kind A |
| --- | --- |
| `astronomy::the_sky_never_changes` | asserts `"zenith"` in `ConstantSun`'s own output |
| `astronomy::phenomena_are_constant_and_maximally_salient` | asserts `ConstantSun`'s phenomena directly |
| `worldgen::constant_choice_yields_constant_sky_and_unchanged_almanac_context` | asserts the choice yields the tier |
| `worldgen::absent_sky_provider_fact_falls_back_to_constant` | asserts the fallback Task 4 deletes |
| `worldgen::constant_world_has_no_calendar_or_night_sky_or_notes` | asserts the tier's absences |
| `worldgen::sky_calendar_accessor_present_for_generated_absent_for_constant` | asserts the `Option` Task 4 removes |
| `worldgen::constant_sky_world_still_has_a_climate` | asserts the constant-sky climate fallback |
| `scene::system_scene_errors_on_a_constant_sun` | error path, tier-0 the only producer |
| `scene::moons_scene_errors_on_a_constant_sun` | same |
| `scene::neighbors_scene_errors_on_a_constant_sun` | same |
| `main::star_chart_on_a_constant_sun_world_is_a_loud_error` | same |
| `repl::sky_reports_the_constant_sun` | asserts `"zenith"` |
| `repl::calendar_on_constant_world_says_no_generated_sky` | asserts the tier's absence |
| `exit_criterion::repl_answers_sky_village_and_belief` | campaign 1b's exit criterion; asserts `"zenith"` |

**`worldgen::observation_time_is_zero_for_constant_and_locked_skies` is
NEITHER — it is Trap 1.** It loses *half* its subject. Keep the test, drop
the constant arm, rename to name only the locked sky, and confirm the locked
assertion still holds.

**Kind A carries an unconditional obligation.** If retiring a kind-A test
leaves a production branch with no reachable producer, that branch is
**deleted in Task 4**, never left as `unreachable!()` and never left as a
`None` arm nothing can return. Record every such branch in the task report so
Task 4 can act on it. A guard that can never fire, sitting in a healthy-looking
tree, is worse than an absent one.

- [ ] **Step 3: Flip each kind-B site by this decision rule**

In order — the first matching branch wins:

1. **Does the site assert about the build path itself?** (It compares a built
   world against `cli/tests/fixtures/world-seed-42.json`, or exercises
   `build_world`'s own behaviour.) → **build**, `SkyChoice::Generated`.
   Substituting the fixture here compares the fixture against itself.
   `repose_byte_identity::constant_sun_world` is the known instance.
2. **Does it need seed 42 under default pins?** → use the existing loader:

```rust
let world = hornvale_worldgen::fixture::seed_42_world();
```

   This is *cheaper than the constant-sky build it replaces* — a file read
   against 0.86 s — so these sites get faster. It is already used at 56 sites
   across 27 files; it is not a new seam.
3. **Any other seed, or non-default pins?** → **build**,
   `SkyChoice::Generated`, and pay the +1.76 s. Sites using `Seed(0)`,
   `Seed(1)` or a loop variable land here.

- [ ] **Step 4: Run each flipped test and read what it says**

Run, per crate as you go:
```bash
cargo nextest run -p hornvale-worldgen 2>&1 | tail -30
cargo nextest run -p hornvale-scene -p hornvale-book -p hornvale 2>&1 | tail -30
```

**A red here is the intended signal, not an obstacle.** A constant-sky world
holds 16 beliefs; a generated one holds 145. An assertion tuned to the left
column fails loudly on the right, which is exactly why this task runs before
the parameter is deleted.

**When one fails:** fix the assertion against the generated world and say so
in the task report, naming the old and new expected values. **Do not reach
for `fixture::seed_42_world()` to make a red go away** — if the site is
building deliberately (rule 1 or 3), substituting the fixture changes what
the test measures rather than fixing it.

- [ ] **Step 5: Halve the pin enumeration**

`windows/worldgen/tests/suite/pin_enumeration.rs` — remove `SkyChoice` from
the enumerated product:

```rust
fn sky_choices() -> [SkyChoice; 2] {
    [SkyChoice::Constant, SkyChoice::Generated]
}
```
becomes nothing; delete the function and drop its loop level, so the product
is `Rotation × Neighbor` = 24.

Its module doc at lines 50-56 makes a claim **about the half being deleted**:

> Half the product (`SkyChoice::Constant`, 24 combinations) is refusal-free
> by construction: rotation and neighbor pins are still recorded on those
> worlds, but the generated-sky path that could act on them never runs, so
> any future refusal in this product can only arise on the generated-sky
> half.

Rewrite it, do not renumber it — the sentence's entire content is about a
half that no longer exists. State instead that the product is now 24
combinations, all generated-sky, and that the refusal-free-by-construction
carve-out is gone with the tier, so **every** combination can now refuse.

Also update the "Measured at authoring (2026-07-11, seed 42): 48 built, 0
refused" line — re-measure and state the new figure with today's date,
reported not asserted, exactly as the existing comment does.

- [ ] **Step 6: Measure the pin-enumeration cost rather than predicting it**

Run:
```bash
cargo nextest run -p hornvale-worldgen --run-ignored all -E 'test(pin_enumeration)' 2>&1 | tail -8
```
Record the wall time in the task report against the module doc's existing
figure (~16 s at `BuildDepth::Terrain`). 24 generated builds replace 24
generated + 24 constant, so the expectation is *down or flat* — but report
the number, do not assert it.

- [ ] **Step 6b: Write the `sky_of` error test now, and record it RED**

Task 4 makes `sky_of` error on a world with no `sky-provider` fact (decision
0737). The test is written **here**, while `sky_of` still returns `Ok`, so its
red is BEHAVIOURAL — an `expect_err` panic — rather than a compile error. By
Task 4 there is no pre-change behaviour left to fail against, and a red from a
compile error proves nothing about whether an assertion would have caught the
behaviour (ruling R3).

Add to `windows/worldgen/src/lib.rs`'s test module, alongside the retiring
`absent_sky_provider_fact_falls_back_to_constant`:

```rust
#[test]
fn a_world_with_no_sky_provider_fact_is_an_error_not_a_fallback() {
    // A bare world, never built — the only way to reach this arm, since
    // every build commits the fact unconditionally (decision 0737).
    let mut world = World::new(Seed(1));
    register_all(&mut world.registry).unwrap();
    let err = sky_of(&world).expect_err("a never-built world has no sky");
    assert!(
        format!("{err:?}").contains("no sky-provider fact"),
        "the error must name the missing predicate: {err:?}"
    );
}
```

Run it and **paste the failure into the task report**:
```bash
cargo test -p hornvale-worldgen a_world_with_no_sky_provider_fact -- --nocapture
```
Expected: **FAIL** — `sky_of` returns `Ok`, so `expect_err` panics. That
panic is the evidence the test discriminates.

Then mark it `#[ignore = "green from Task 4: sky_of errors on an absent \
sky-provider fact (decision 0737); red here is the recorded pre-change \
behaviour"]` so the branch stays green between tasks. Task 4 removes the
attribute.

- [ ] **Step 7: Confirm the population is empty and commit**

Run:
```bash
grep -rn "SkyChoice::Constant" --include=*.rs . | grep -v '^./target'
```
Expected: exactly the 5 surface sites Task 3 deletes
(`cli/src/main.rs:240`, `cli/src/main.rs:2554`,
`clients/world-wasm/src/lib.rs:95`, `windows/worldgen/src/lib.rs:8136`,
`pin_enumeration.rs:66` if any residue remains). Anything else means a site
was missed.

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "test(the-zenith): flip the constant-sky sites, retire the tier's own tests

The 23 SkyChoice::Constant sites, sorted by what the tier was doing at
each. Kind A (the tier IS the subject) retire; kind B (incidental cheap
world) flip to fixture::seed_42_world() where seed 42 + default pins
serves, or to SkyChoice::Generated where the site builds deliberately.

Deliberately before the parameter is deleted: a constant-sky world holds
16 beliefs and a generated one holds 145, so a flip is a change of
subject, and that is invisible inside a 405-site diff.

pin_enumeration's product halves to 24, all generated-sky; its module doc
claimed the deleted half was refusal-free by construction, so the claim is
rewritten rather than renumbered.

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 3: Promote the battery

**Files:**
- Rename: `domains/astronomy/tests/suite/tier_refinement.rs` → `sky_conformance.rs`
- Modify: `domains/astronomy/tests/suite.rs` (the module list)

**Interfaces:**
- Consumes: Task 2's flipped sites. **Runs BEFORE Task 4 deletes `ConstantSun`** — the battery imports and calls it (lines 10, 51, 119), so the deletion cannot compile until this task has removed those uses (ruling R1).
- Produces: nothing later tasks depend on.

- [ ] **Step 1: Rename the file and its module declaration**

```bash
git mv domains/astronomy/tests/suite/tier_refinement.rs domains/astronomy/tests/suite/sky_conformance.rs
grep -n "tier_refinement" domains/astronomy/tests/suite.rs
```
Update the `mod` line. **A rename is a commit-gate change** — the sub-floor
roster is keyed on test names, so `docs/timings/subfloor-roster.tsv` will
carry stale entries until the next green stage gate rewrites it. Note this in
the task report; do not hand-edit the roster.

- [ ] **Step 2: Freeze the two literals the tests used to source from `ConstantSun`**

Two of the four tests derived their expectations by calling production code
inside the crate under test. Replace those derivations with frozen constants
— this is *stronger* than what it replaces, not merely equivalent:

```rust
//! The sky-conformance battery: what a Hornvale sky *is*. Every generated
//! sky, on every seed, in every rotation regime, at every hour, keeps four
//! claims true — there is exactly one day-sky sun, nothing outranks it, it
//! is never retracted from the visible bodies, and any period it carries is
//! the calendar's own day.
//!
//! These were the claims the retired tier-0 provider stipulated and the
//! cross-tier refinement battery checked the generated sky against
//! ("coarse constrains fine"). The tier is gone (decision 0736); the claims
//! were always the part doing the work, so they are asserted directly.
//!
//! The two constants below were previously read out of `ConstantSun` at
//! test time — production code inside the crate under test. Frozen here,
//! they can no longer drift with the implementation they check.

/// The registered concept a sun is: every sky's day-sky body reports it.
const SUN_KIND: &str = CELESTIAL_BODY;
/// The sun's salience. It is the unique maximum: nothing outranks it, and
/// nothing ties it.
const SUN_SALIENCE: f64 = 1.0;
/// The sun's entry in a rendered sky's `bodies` list, at every hour.
const SUN_BODY: &str = "the sun";
```

Then in `every_generated_sky_keeps_the_one_sun_tier_0_promises` — renamed to
`every_sky_has_exactly_one_day_sky_sun` — replace:

```rust
let tier0 = ConstantSun.phenomena(&ctx(0.0));
assert_eq!(tier0.len(), 1, "tier 0 claims exactly one thing");
let coarse_sun = &tier0[0];
```
with nothing; and the two assertions that referenced `coarse_sun` become:

```rust
assert_eq!(suns[0].kind, SUN_KIND);
assert_eq!(
    suns[0].salience, SUN_SALIENCE,
    "seed {seed} t {t}: the sun must stay the top-salience day-sky body"
);
```

In `the_sun_never_leaves_the_visible_bodies_list`, replace:
```rust
let coarse = ConstantSun.sky_at(WorldTime::GENESIS);
assert_eq!(coarse.bodies, vec!["the sun".to_string()]);
```
with nothing, and use `SUN_BODY` in the `contains` assertion below it.

- [ ] **Step 3: Rename the two tests whose names cite the tier**

`every_generated_sky_keeps_the_one_sun_tier_0_promises` →
`every_sky_has_exactly_one_day_sky_sun`.
`refinement_adds_structure_only_beneath_the_sun` →
`nothing_in_a_sky_outranks_its_sun`.
The other two need only their prose updated — they never mentioned
`ConstantSun` and are already direct assertions.

- [ ] **Step 4: Preserve the four `claim:` tags verbatim**

Each test carries:
```rust
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
```
**Do not edit these.** The sweep's cost and its census-homelessness are
unchanged by a rename, and the tag is machine-read.

- [ ] **Step 5: Run and commit**

```bash
cargo test -p hornvale-astronomy --test suite -- sky_conformance 2>&1 | tail -20
```
Expected: 4 passed.

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "test(the-zenith): promote the cross-tier battery to a sky-conformance battery

tier_refinement.rs becomes sky_conformance.rs. Two of its four tests never
mentioned ConstantSun at all — they were already direct assertions over the
generated sky. The other two used it only to source two literals, which
means the battery derived its expectation by calling production code inside
the crate under test. Frozen as constants, they can no longer drift with the
implementation they check, so this is stronger than what it replaces.

The claim: sanctioned-sweep tags are preserved verbatim; the sweep's cost
and census-homelessness are unchanged by a rename.

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 4: Delete the type, the surface, and the parameter

Mechanical, and large. The compiler does the enumeration.

**Files:**
- Modify: `domains/astronomy/src/lib.rs:504-553` (delete `ConstantSun` + impls)
- Modify: `windows/worldgen/src/lib.rs:228-236, 310-316, 8135-8138, 8154-8159` and every `build_world*` signature
- Modify: `cli/src/main.rs:14, 237-245`
- Modify: `clients/world-wasm/src/lib.rs:92-99`
- Modify: ~335 `build_world*` call sites workspace-wide

**Interfaces:**
- Consumes: Task 3's de-`ConstantSun`-ed battery, and Task 2's empty `SkyChoice::Constant` population.
- Produces: `pub fn build_world(seed: Seed, sky: &SkyPins, terrain:
  &TerrainPins, settlements: &SettlementPins) -> Result<World, BuildError>`
  — the third positional parameter is gone. `build_world_to` loses it in the
  same position. Every later task uses the four-argument form.

- [ ] **Step 1: Verify the compiler will enumerate — the precondition**

Run:
```bash
grep -n "_ =>" windows/worldgen/src/lib.rs | head -40
grep -rn "match .*sky\b" -A8 --include=*.rs windows/worldgen/src/lib.rs | grep "_ =>"
```
Expected: **no wildcard arm on any `Sky` or `SkyChoice` match.** Checked on
`main`: there are none. A `_ =>` arm would silently absorb the deletion
instead of erroring, and the enumeration would be incomplete without saying
so. If one has appeared, replace it with explicit arms *before* proceeding.

- [ ] **Step 2: Delete `ConstantSun`**

In `domains/astronomy/src/lib.rs`, delete `pub struct ConstantSun` (504-505),
`impl ConstantSun` (522-541), `impl PhenomenaSource for ConstantSun`
(543-553), and its two unit tests
(`the_sky_never_changes` at 568, `phenomena_are_constant_and_maximally_salient`
at 578 — they live inside the block being deleted and cannot outlive it;
ruling R4 keeps them here rather than splitting them into Task 2).
Leave `SkyReport` — it is the shared report type every provider returns.

- [ ] **Step 3: Collapse `Sky` and delete `SkyChoice`**

`windows/worldgen/src/lib.rs` — `Sky` becomes a single-provider wrapper:

```rust
/// The live astronomy provider a world uses, reconstructed from its ledger.
///
/// One provider, since The Zenith (decision 0736): every Hornvale world has
/// a generated sky. The enum this replaced carried a `Constant` variant for
/// the retired tier-0 stub.
pub struct Sky(Box<GeneratedSky>);
```

with `sky_at_visibility`, `notes` and the `PhenomenaSource` impl delegating
straight through.

**`calendar()` and `system()` keep returning `Option` in this task** — wrap in
`Some(...)`. They collapse in Task 5 (ruling R2). Collapsing them here would
force this task to also fix all ten `let … else` guard sites, which is exactly
the coupling the stage split exists to prevent: this task is mechanical, and
Task 5 needs Trap 1 judgment at every site. Delete `pub enum
SkyChoice` (229-236) and the `choice_text` match (8135-8138); the
`sky-provider` fact is now always `"generated"`:

```rust
world.ledger.commit(
    scenario_fact(
        world_entity,
        facts::SKY_PROVIDER,
        Value::Text("generated".to_string()),
    ),
    &world.registry,
)?;
```

**Keep committing the fact.** It is a save-format contract and a world's
self-description; a reader must still be able to ask what sky a world has.
Removing it would change every world's ledger and is out of scope.

Make the astronomy stage unconditional — this is the `if let
SkyChoice::Generated = sky` at 8154:

```rust
stage("astronomy", || -> Result<(), BuildError> {
    let outcome = generate(seed, pins).map_err(BuildError::Genesis)?;
    facts::genesis(&mut world, world_entity, &outcome)?;
    Ok(())
})?;
```

- [ ] **Step 4: Drop the parameter and let the compiler find the call sites**

Remove the `sky: SkyChoice` parameter from `build_world`, `build_world_to`
and any `build_world_observed` variant. Then:

```bash
cargo check --workspace --all-targets 2>&1 | tee /tmp/zenith-errors.txt
grep -c "^error" /tmp/zenith-errors.txt
```

Work the list to zero. **Do not use `sed`, `rg -r`, or any scripted rewrite.**
Measured: 372 of the 382 `SkyChoice::Generated` sites are the plain
trailing-comma argument form, but **10 are not** — three `assert_eq!`
comparisons (`cli/src/main.rs:2509, 2560, 2651`), an array literal
(`pin_enumeration.rs:66`), a match arm (`lib.rs:8137`), an `if let`
(`lib.rs:8155`), a doc-comment example (`fixture.rs:62`), and three prose
comments. A regex sweep corrupts each of those and leaves a plausible tree.

Update `windows/worldgen/src/fixture.rs:60-64`'s doc comment, which spells
out the old five-argument call.

- [ ] **Step 5: Delete the CLI and wasm surface**

`cli/src/main.rs` — delete the `[--sky constant|generated]` usage line (14)
and the `--sky` arm of `parse_sky_args` (237-245); the function returns
`Result<SkyPins, String>` now. Delete `sky_flag_selects_constant` (2552) and
fix the three `assert_eq!(sky, SkyChoice::Generated)` sites (2509, 2560,
2651), which no longer have a second element to compare.

`clients/world-wasm/src/lib.rs:92-99` — delete the `if key == "sky"` branch
so `sky` falls through to the unknown-key error. **Note in the task report
whether the wasm catalog needs a version bump**: decision 0356 retired the
external consumers, so no cross-repo contract binds it, but the released
catalog is cut by hand and the removal is a real ABI narrowing.

- [ ] **Step 5b: Make `sky_of` error on an absent fact**

Decision 0737. A world is a seed plus a ledger; no compatibility shim.

```rust
/// Reconstruct the live astronomy provider from this world's ledger: fold
/// every `scenario-pin` fact back through `parse_pin` and regenerate
/// deterministically from the world's own seed.
///
/// A world with no `sky-provider` fact is an error, not a fallback
/// (decision 0737). Every build commits the fact unconditionally, so its
/// absence means the world was never built — and a world is a seed plus a
/// ledger, re-derivable with `hornvale new --seed <seed>`, so there is
/// nothing a shim could recover that regeneration cannot.
pub fn sky_of(world: &World) -> Result<Sky, BuildError> {
    let Some(provider_fact) = world.ledger.find(facts::SKY_PROVIDER).next() else {
        return Err(BuildError::Pins(
            "world has no sky-provider fact: it was never built; \
             regenerate it from its seed and pins".to_string(),
        ));
    };
    // ... unchanged: parse pins, regenerate, wrap in Sky
}
```

This is not optional in this task: `sky_of`'s fallback at `lib.rs:571`
constructs `Sky::Constant(ConstantSun)`, so deleting the variant makes the
function uncompilable. Ruling R3 moved it here from Task 5 for that reason.
Task 2 already wrote the test and recorded it RED against the old behaviour;
it should go green here.

Run:
```bash
cargo test -p hornvale-worldgen a_world_with_no_sky_provider_fact -- --nocapture
```
Expected: **PASS**, having been recorded FAIL in Task 2.

- [ ] **Step 6: Verify and commit**

```bash
cargo check --workspace --all-targets 2>&1 | grep -c "^error"   # expect 0
grep -rn "ConstantSun\|SkyChoice\|Sky::Constant" --include=*.rs . | grep -v '^./target'
```
Expected: zero errors; the second command returns nothing except prose
comments you have deliberately left.

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "refactor(the-zenith)!: delete ConstantSun, SkyChoice, and the sky parameter

Sky becomes a one-provider wrapper; build_world and build_world_to lose
their SkyChoice argument at ~335 call sites. The --sky flag and the wasm
'sky' pin key go with them.

Compiler-driven, never a scripted rewrite: 372 of 382 SkyChoice::Generated
sites are the plain argument form and 10 are not (match arms, assert_eq!
comparisons, an array literal, a doc example), so a regex sweep would have
left a plausible wrong tree. Verified beforehand that no '_ =>' wildcard
arm exists on any Sky match, so the compiler enumerates the work completely.

The sky-provider fact is still committed, always 'generated'. It is a
save-format contract and a world's self-description; dropping it would
move every world's ledger.

sky_of now errors on a world carrying no sky-provider fact instead of
silently falling back (decision 0737, on 0189's precedent: a world is a
seed plus a ledger, and no compatibility shim). The change is forced here
rather than chosen — the fallback constructed Sky::Constant, which this
commit deletes. Its test was written in Task 2 against the old behaviour
and recorded failing there, so the red is behavioural, not a compile error.

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 5: Collapse the Option-ness, and delete the guards it justified

**Files:**
- Modify: `windows/worldgen/src/lib.rs:334-350, 564-598, 2605-2655, 3646, 3902, 5192, 8691, 10007, 10273`
- Modify: `windows/lab/src/metrics.rs:78-90, 180-190`
- Modify: `windows/lab/src/health.rs:403-407`
- Modify: `windows/book/src/lib.rs:783-806`
- Modify: `windows/vessel/tests/suite/the_detent.rs:153-157`, `windows/vessel/examples/agent_scaling.rs:489`, `windows/vessel/examples/session_length_scaling.rs:757`, `windows/vessel/src/liveness_tests/emitter_scan.rs:489`

**Interfaces:**
- Consumes: Task 4's one-provider `Sky`.
- Produces: `pub fn calendar(&self) -> &hornvale_astronomy::Calendar` and
  `pub fn system(&self) -> &hornvale_astronomy::StarSystem` — both
  non-`Option`. `sky_of` returns `Err(BuildError::Pins(_))` for a world with
  no `sky-provider` fact.

- [ ] **Step 1: Re-read Trap 1 before touching a single guard**

`Sky::calendar()` / `Sky::system()` returning `Option` is **the tier** and
goes. `Calendar::day_length()` returning `Option` is **tidal locking** and
stays. Verified stacked instances: `lib.rs:5192`, `the_detent.rs:153`. Name
which kind each guard is before deleting it.

- [ ] **Step 2: Make the accessors non-`Option`**

```rust
/// The derived calendar. Every world has one, since The Zenith
/// (decision 0736) — a `Calendar` whose `day_length()` is `None` is a
/// tidally locked world, which is a different thing and still expressible.
pub fn calendar(&self) -> &hornvale_astronomy::Calendar {
    self.0.calendar()
}

/// The generated star system. Every world has one; the star-chart command
/// reads this.
pub fn system(&self) -> &hornvale_astronomy::StarSystem {
    self.0.system()
}
```

- [ ] **Step 3: Delete every guard whose producer is gone**

Work `cargo check --workspace --all-targets` to zero. For each site, the
treatment:

| site | today | after |
| --- | --- | --- |
| `lib.rs:3646` | `let Some(system) = sky.system() else { return empty paleoclimate }` | delete the guard; the early return goes with it |
| `lib.rs:3902` | `let Some(system) = sky.system() else { one present-era mask }` | delete the guard and its whole `else` block |
| `lib.rs:5192` | two stacked guards | delete the **calendar** one; **keep** `day_length()` |
| `lib.rs:8691` | `let Some(calendar) = sky.calendar() else { return Ok(()) }` | delete the guard |
| `lib.rs:10007` | `Sky::Constant(_) => Vec::new()` in `genesis_notes` | delete the arm |
| `lib.rs:10273` | `if let Sky::Generated(sky) = sky_of(world)?` | unconditional |
| `lib.rs:2609` | `stellar_inputs`' `Sky::Constant` Earth-baseline arm | delete the arm and its five-tuple of Earth defaults |
| `lib.rs:2648` | `greenhouse_forcing_k`'s `Sky::Constant => 0.0` | delete the arm; update the doc comment, which explains the constant-sky case at length |
| `lab/metrics.rs:80` | `let Sky::Generated(sky) = sky else { return Err("expected Generated sky, got Constant") }` | delete the guard. Also fix `WorldView::system`'s doc, which reads *"The star system, reconstructed or constant"* — already false today, since this guard refuses constant worlds |
| `lab/health.rs:405` | `.and_then(\|sky\| sky.calendar().cloned())` | `.map(\|sky\| sky.calendar().clone())` |
| `book/lib.rs:802` | `Sky::Constant(_) => 0` in `true_event_count` | delete the arm; rewrite the 12-line doc comment, which is entirely about the tier-0 case |
| `the_detent.rs:153` | two stacked `and_then`s | first becomes `.map`; **keep** the second (`day_ticks`) |

**The obligation from Task 2 Step 2 lands here.** Every production branch
that task recorded as having lost its only producer is deleted in this step.
None is left as `unreachable!()`; none is left as a `None` arm nothing can
return.

- [ ] **Step 4: Verify no branch survives on absence**

Run:
```bash
grep -rn "constant sky\|constant-sky\|tier-0\|tier 0" --include=*.rs . | grep -v '^./target' | grep -v 'tier_comparison_spike'
cargo nextest run --workspace 2>&1 | tail -20
```
Expected: the grep returns only deliberate historical prose; the suite is
green. **`tier_comparison_spike` is excluded on purpose — Trap 2.**

- [ ] **Step 5: Commit**

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "refactor(the-zenith)!: Sky::calendar and Sky::system are no longer Option

Every world has a calendar and a star system, so the ten production guards
that existed only for the acyclic case are deleted rather than stubbed. A
guard that can never fire, sitting in a healthy-looking tree, is worse than
an absent one.

Kept, deliberately: Calendar::day_length()'s Option. That is tidal locking
— a real regime the generated sky still produces — not the tier. Two sites
stacked the two guards adjacently (lib.rs:5192, the_detent.rs:153) and only
the tier half is gone.


Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 6: The prose sweep — including one artifact no gate can see

**Files:**
- Modify: `CLAUDE.md:808-810` and its generated-artifact block (~line 665)
- Modify: `domains/CLAUDE.md:52-58`
- Modify: `docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md:165-177`
- Modify: `kernel/examples/first_light.rs:152`
- Modify: `book/src/gallery/world-seed-42.md` (via regeneration)
- Modify: `book/src/domains/religion.md:11-20`

- [ ] **Step 1: Fix Trap 3 first — the artifact nothing will catch**

`kernel/examples/first_light.rs:152` hardcodes:

```rust
"Under a golden sun fixed at zenith, in an unnamed vale, stands the \
 goblin village of **{village_name}**. Its people revere the most \
 salient phenomenon their sky offers them.\n\n"
```

The example imports **only `hornvale_kernel`** — it cannot reach
`ConstantSun` and derives nothing from it. So the tier's deletion leaves this
sentence in place, `make rebaseline` reproduces it byte-identically, and the
drift check compares the regenerated file against the committed one and
reports **no drift, forever**. It is a false committed artifact that no gate
can see.

The example builds its own kernel-level mini-genesis with no astronomy at
all, so the honest fix is to stop asserting a sky it does not model:

```rust
"In an unnamed vale stands the goblin village of **{village_name}**. Its \
 people revere the most salient phenomenon their sky offers them.\n\n"
```

- [ ] **Step 2: Rewrite the two `CLAUDE.md` doctrine statements**

Root `CLAUDE.md:808-810` currently reads:

> **Provider tiers coexist:** the tier-0 `ConstantSun` and the generated star
> system are both valid; worlds choose. Higher fidelity refines, never
> contradicts, lower ("coarse constrains fine").

**Rewrite, do not delete.** A reader who finds the sentence gone learns
nothing; one who finds it corrected learns the history:

```markdown
**Provider tiers are retired** (decision 0736, The Zenith). The doctrine —
multiple tiers of one truth coexisting, "coarse constrains fine" — held that
`ConstantSun` and the generated star system were both valid and worlds chose.
Decision 0039 half-retired it, ruling that a generator which *contradicts* its
predecessor is an epoch rather than a tier, and named astronomy as the one
surviving licence for coexistence. The Zenith closed that licence: every world
has a generated sky with a calendar and a star system.

Nothing expressible was lost. Tier-0's whole content was *acyclicity*, and a
tidally locked world (`--rotation locked`) is acyclic as derived physics
rather than stipulation — `Calendar::day_length()` still returns `None` for
one, and `book/src/gallery/almanac-seed-42-locked.md` is the committed proof.
```

`domains/CLAUDE.md:52-58` — same treatment, **but its closing sentence is a
terrain rule that must survive the paragraph housing it**:

> Keep tier-0 paths byte-identical when you touch shared code — the
> `strongest`/`None`-branch comments in `terrain/crust.rs` are a live example
> of code kept "instruction-for-instruction" unperturbed to protect a
> byte-identity contract.

That sentence is about **terrain**, has nothing to do with astronomy tiers,
and is still live. Relocate it — do not drop it. Suggested shape: retitle the
section `## One provider per domain` and keep the byte-identity sentence under
it, with the tier framing removed.

- [ ] **Step 3: Verify the terrain sentence survived**

Run:
```bash
grep -rn "instruction-for-instruction" domains/CLAUDE.md
```
Expected: **one hit.** Zero hits means Step 2 dropped a live terrain
discipline while rewriting an astronomy paragraph. This check exists because
that is the easy mistake here.

- [ ] **Step 4: Fix the stale almanac count**

Root `CLAUDE.md`'s generated-artifact block says the script builds *"three
seed-42 almanacs"*; after Task 1 it builds two. **`grep 'three seed-42'` will
not find it** — the phrase wraps across a line break in that comment block.
Find it with:

```bash
grep -n "seed-42 almanacs" CLAUDE.md
```

- [ ] **Step 5: Reconcile the Constitution**

`docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md:165-177`
carries the four-rung tier ladder and the sentence *"A trivial provider's
sparse answer is itself meaningful input downstream (what religion develops
under an eternal noon?)."*

The spec governs where it and `CLAUDE.md` disagree, so leaving it stating a
doctrine `CLAUDE.md` has retired would invert the precedence rule. Amend it
in place with a dated note recording three things: that tiers 1–3 all shipped
inside one provider so the ladder was never a ladder; that 0736 retired the
doctrine; and that the eternal-noon question is **a census question** —
`pantheon-cyclic-share` over a `rotation=locked` pin set — not a tier
question, which is why the tier's removal costs nothing that mattered.

- [ ] **Step 6: Update `book/src/domains/religion.md`**

Lines 11-20 carry a `**Tier 0 — one belief from the top phenomenon.**`
heading, the tier-0 deity example, and the phrase *"waiting for a sky that
actually cycles"* — prose written before the generated sky shipped and never
refreshed. It is hand-written (not in `docs/generated-paths.txt`), so this is
an ordinary book-freshness edit under the standard DoD. Rewrite the heading
to describe the mechanism rather than a tier, and drop "waiting for" — that
sky arrived in Campaign 2b.

- [ ] **Step 7: Regenerate, verify, commit**

```bash
make rebaseline 2>&1 | tail -10
git diff --stat book/src/gallery/world-seed-42.md
grep -rn "zenith" book/src/gallery/world-seed-42.md
```
Expected: `world-seed-42.md` shows a one-line diff; the grep returns nothing.
**An empty diff means Step 1's edit did not reach the artifact** — the
example was not re-run. Check that `regenerate-artifacts.sh:1002` ran.

```bash
cargo fmt && make gate-commit
git add -A
git commit -m "docs(the-zenith): retire the tier doctrine in prose, and one artifact no gate could see

kernel/examples/first_light.rs hardcoded 'Under a golden sun fixed at
zenith' into book/src/gallery/world-seed-42.md. The example imports only
hornvale_kernel and derives nothing from ConstantSun, so deleting the tier
would have left the sentence in place, regeneration would reproduce it
byte-identically, and the drift check would compare the file against itself
and report no drift forever.

CLAUDE.md, domains/CLAUDE.md and the Constitution are rewritten rather than
cleared: a reader who finds the sentence gone learns nothing. Care taken in
domains/CLAUDE.md, whose tier section closes with a TERRAIN byte-identity
rule that has nothing to do with astronomy and had to survive the paragraph
housing it.

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 7: The decision records

**Files:**
- Create: `docs/decisions/0736-the-provider-tier-doctrine-is-retired.md`
- Create: `docs/decisions/0737-a-world-with-no-sky-provider-fact-does-not-load.md`
- Create: `docs/decisions/0738-a-retired-tiers-battery-is-promoted-not-deleted.md`

- [ ] **Step 1: Write 0736**

Follow `docs/decisions/0039`'s shape — context, decision, the distinction,
the load-bearing consequence, scope. It must record: that astronomy was the
doctrine's last instance and 0039's carve-out is now empty; that tiers 1–3
shipped inside one provider so the ladder was never a ladder; that acyclicity
survives as `--rotation locked`; and that the Constitution's eternal-noon
question is a census question (`pantheon-cyclic-share`, a `rotation=locked`
pin set over `census-of-faiths`' 10,000 worlds), with the finding that **no
committed study uses a non-empty pin set at all**.

- [ ] **Step 2: Write 0737**

Cite 0189 explicitly: a world is a seed plus a ledger; no compatibility shim.
Record that `sky-provider` is committed unconditionally
(`windows/worldgen/src/lib.rs`, the `scenario_fact` commit in
`build_world_to`), so absence means the world was never built.

- [ ] **Step 3: Write 0738**

The generalizable rule, stated past astronomy: *when a coarse tier is
retired, its cross-tier battery is promoted to a conformance battery over the
surviving provider, with its expectations frozen as literals rather than
sourced from the code under test.* Record the specific improvement — the old
battery read its expectation out of `ConstantSun`, production code inside the
crate under test.

- [ ] **Step 4: Verify the records are well-formed and commit**

```bash
cargo test -p hornvale --test suite -- docs_consistency 2>&1 | tail -12
```
Expected: green. It asserts each record's title matches its filename,
decision numbers are unique, and every `cite` resolves.

```bash
git add -A && git commit -m "decisions(the-zenith): 0736, 0737, 0738

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
```

---

### Task 8: Close

**Files:**
- Create: `book/src/chronicle/the-zenith.md`
- Create: `docs/retrospectives/the-zenith.md`
- Modify: `book/src/SUMMARY.md`
- Modify: `book/src/frontier/idea-registry.md` (the `SKY-retire-the-tier-system` row)
- Modify: `docs/superpowers/ledgers/2026-09-04-the-zenith.md`

- [ ] **Step 1: Verify the DoD by measurement**

```bash
grep -rn "ConstantSun\|SkyChoice\|Sky::Constant" --include=*.rs . | grep -v '^./target'
grep -rn "Sky::Constant" --include=*.sh --include=*.toml . | grep -v '^./target'
ls book/src/gallery/almanac-seed-42*.md
```
Expected: no `.rs` hits; two almanacs (`almanac-seed-42.md`,
`almanac-seed-42-locked.md`).

**Do NOT add a committed source-scan test asserting these are zero.** The
spec's DoD said "asserted, not eyeballed", and that phrasing invited exactly
the wrong instrument. Once the types are deleted, nothing can reintroduce
`SkyChoice` without also failing to compile — so such a test could never go
red. That is a check that can never fire, which this project holds to be
worse than an absent one. The compiler *is* the guard here; the greps above
are a one-time close verification, not a ratchet. Record this reasoning in
the ledger.

- [ ] **Step 2: Update the registry row**

`SKY-retire-the-tier-system` moves to `shipped`. Its **Where** cell must
record that the row's own estimate was wrong — the row said "six production
files" and the measured surface was 405 `SkyChoice` sites across 335
`build_world` call sites. Correct it rather than deleting it: a shipped row's
Where cell is where a future reader learns what the scoping error was.

Also add the follow-up row from ledger #9:
a `{"label": "locked", "pins": ["rotation=locked"]}` pin set for
`census-of-faiths`, which would answer the Constitution's eternal-noon
question properly for the first time. Status `raw`.

Run:
```bash
cargo test -p hornvale --test suite -- docs_consistency 2>&1 | tail -12
```
Expected: green — five columns, closed status vocabulary, non-empty Where,
no new numbered ID.

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-zenith.md`, added to `book/src/SUMMARY.md`. Written
at the book's altitude — technical, comprehensible without the code. The
material worth carrying: that tier-0's content was acyclicity and a locked
world already had it; that the ladder was never a ladder; and the measured
16-vs-145 belief difference, which is the one thing the tier ever showed and
is now recorded where it survives its subject.

- [ ] **Step 4: Book freshness sweep**

Re-read every chapter that describes the sky or the providers, not only the
ones this campaign edited. Check `book/src/open-questions.md`'s **Confidence
Gradient**: if this campaign resolved or moved a bet, re-score that chapter
(decision 0030). If it moved none, say so explicitly in the retrospective —
an unstated sweep is indistinguishable from a skipped one.

- [ ] **Step 5: Write the retrospective**

`docs/retrospectives/the-zenith.md` — process lessons, not product. The
candidates, all of which happened:
- The registry row's own scope estimate was wrong by ~70x, and it was the
  campaign's scoping input.
- Two citation errors in the ledger's first draft, caught by re-reading the
  cited *lines* rather than the cited *claim*.
- A count column in the spec's first draft that mixed two populations and
  summed to 32 against a population of 23 — plausible, and wrong.
- The flagged G3 fidelity concern was dissolved by naming the right
  instrument, not by weighing the loss.
- Trap 3: a hardcoded sentence in a generated artifact that the drift check
  structurally cannot see.

- [ ] **Step 6: Commit and hand to the merge queue**

```bash
cargo fmt && make gate-commit
git add -A && git commit -m "close(the-zenith): chronicle, retrospective, registry row

Claude-Session: https://claude.ai/code/session_01QKhCP8Pr8wWuqejxeKEAGs"
git push origin campaign/the-zenith
```

Then the pre-merge close, in order: absorb `main`, regenerate artifacts,
re-gate, and submit
`make sluice BRANCH=campaign/the-zenith REF=<full-sha>`.
A census refresh is ordinary queued work (decision 0514) and needs no
authorization; this campaign moves no census golden, so submit one only if
`make lab-diff STUDY=the-census` shows movement.

