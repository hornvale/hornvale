# The Prospect Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the walk band say what is here, let the player walk into it, and widen what counts as a place from "somebody built it" to "there is something here."

**Architecture:** `built: bool` — currently the gate every enterable place hangs off — is replaced by `site: Option<Site>`, carrying a kind (Settlement / Cave / Exotic), an optional name, and an `Extent` that is modelled now and only ever `Point` in this campaign. Settlements keep working unchanged; caves derive from the continuous `cave_proneness` field with no seeded draw; exotic sites are re-sited from their level-6 vertex to a specific facet by a new seeded draw, which is an epoch. Prose and map both surface sites.

**Tech Stack:** Rust 2024, no crates beyond `serde`/`serde_json`/`libm`. `windows/vessel`, `windows/locale`, `domains/terrain`, `clients/game/bin`.

**Spec:** `docs/superpowers/specs/2026-09-01-the-prospect-design.md`

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only.
- No wall-clock time. Floats sort with `total_cmp`.
- Layering is constitutional: `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain never depends on a sibling.
- Quantize at emit only, never in a compute path.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and variant gets a one-line doc comment.
- `cargo fmt` is the final step before every commit.
- Bypassing the pre-commit hook is forbidden without exception. If the hook refuses, that refusal IS the finding. It sometimes false-positives on the TEXT of a command — reword, never override, and report it.
- A new seed label is a permanent save-format contract; a changed one is an epoch (`/v2` suffix), never a rename.
- `clients/` is outside the cargo workspace and has its own gate (`make game-check`), which ABORTS at its first failing recipe line — only a green result is a complete result.
- `deno` may need `export PATH="$HOME/.deno/bin:$PATH"`.

## H1's exact scope, because it is easy to get wrong

H1 says *surfacing* changes nothing enterable. **Tasks 4 and 5 deliberately DO change enterability** — that is the widening. So H1 is asserted across Tasks 1–3 and 6–8, and Task 4/5 each record the enterability delta they intend. A task that changes enterability without saying so is the failure H1 exists to catch.

## File structure

| file | responsibility |
| --- | --- |
| `windows/vessel/src/site.rs` (new) | the `Site`, `SiteKind`, `Extent` types and their invariants. Pure data, no world access. |
| `windows/vessel/src/brief.rs` | gains `site: Option<Site>`; keeps `built: bool` as a settlement property |
| `windows/vessel/src/structure.rs` | gates on `brief.site` instead of `brief.built` |
| `windows/vessel/src/session.rs` | the `enter` refusal message; chamber identity |
| `windows/locale/src/lib.rs` | the prose clause naming sites |
| `domains/terrain/src/features.rs` | `cave_site_at` — the derived cave predicate |
| `windows/worldgen/src/streams.rs` | the new `SITE_PLACEMENT` label (epoch) |
| `clients/game/bin/src/plate.rs` | a site roster beside the settlement roster |

---

### Task 1: The `Site` type

**Files:**
- Create: `windows/vessel/src/site.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod site;`)

**Interfaces:**
- Produces: `Site { kind: SiteKind, name: Option<String>, extent: Extent }`, `SiteKind::{Settlement, Cave, Exotic}`, `Extent::{Point}`, `Site::salience(&self) -> u8`.

- [ ] **Step 1: Write the failing test**

In `windows/vessel/src/site.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// Salience orders what gets NAMED when a facet holds more than one site
    /// (spec §6). It is presentation only and never world-state.
    #[test]
    fn salience_ranks_settlement_over_exotic_over_cave() {
        let s = Site::new(SiteKind::Settlement, Some("Doaba".into()));
        let x = Site::new(SiteKind::Exotic, None);
        let c = Site::new(SiteKind::Cave, None);
        assert!(s.salience() > x.salience());
        assert!(x.salience() > c.salience());
    }

    /// This campaign emits `Point` only (spec §7). The variant exists so
    /// multi-facet sites are a fill-in rather than a migration.
    #[test]
    fn a_new_site_is_a_point() {
        assert_eq!(Site::new(SiteKind::Cave, None).extent, Extent::Point);
    }
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo test -p hornvale-vessel --lib site::`
Expected: FAIL — `site` module does not exist.

- [ ] **Step 3: Write the type**

```rust
//! A *site* is something at a facet with an interior worth entering.
//!
//! This replaces `Brief::built` as the gate every enterable place hangs off.
//! `built` meant "a structure stands here" (`brief.rs`), which is true of a
//! settlement and false of a cave (dissolved by water) or an exotic site
//! (grown) — so widening `built` would have put a lie in the predicate.
//! Decision 0536.

/// What kind of place a site is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiteKind {
    /// A cave mouth, derived from `cave_proneness` — no seeded draw.
    Cave,
    /// A placed exotic site: strange biota, mineral crystal, a fungal canopy.
    Exotic,
    /// A settlement — the only kind for which `Terrain::is_built` is true.
    Settlement,
}

/// How much ground a site covers.
///
/// **Only [`Extent::Point`] is emitted by The Prospect.** The enum exists
/// because exotic sites are not uniform in scale — a cursed land is miles
/// across with components inside it — and modelling extent later would be a
/// migration of every consumer rather than a fill-in. Decision 0538.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Extent {
    /// One facet.
    Point,
}

/// Something at a facet with an interior worth entering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Site {
    /// What kind of place this is.
    pub kind: SiteKind,
    /// The site's own name, where it has one. Caves and exotic sites do not.
    pub name: Option<String>,
    /// How much ground it covers. Always [`Extent::Point`] in this campaign.
    pub extent: Extent,
}

impl Site {
    /// A point site of the given kind.
    pub fn new(kind: SiteKind, name: Option<String>) -> Self {
        Self { kind, name, extent: Extent::Point }
    }

    /// Presentation rank for choosing which sites a locale NAMES when it holds
    /// more than one (spec §6). Higher is more salient. Never world-state.
    /// type-audit: bare-ok(count: return)
    pub fn salience(&self) -> u8 {
        match self.kind {
            SiteKind::Settlement => 3,
            SiteKind::Exotic => 2,
            SiteKind::Cave => 1,
        }
    }
}
```

- [ ] **Step 4: Run it and watch it pass**

Run: `cargo test -p hornvale-vessel --lib site::`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/src/site.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): the Site type, with Extent modelled and Point-only"
```

---

### Task 2: `Brief` carries a site, and settlements produce one

**Files:**
- Modify: `windows/vessel/src/brief.rs` (the `Brief` struct at :40-58, and `from_parts`)

**Interfaces:**
- Consumes: `Site`, `SiteKind` from Task 1.
- Produces: `Brief::site: Option<Site>`; `Brief::built` unchanged.

**This task must not change what is enterable.** It adds a field that mirrors
`built` for settlements. Task 3 makes it load-bearing.

- [ ] **Step 1: Write the failing test**

In `windows/vessel/src/brief.rs`'s test module:

```rust
/// H1's anchor at this task: a brief with `built` true carries a Settlement
/// site, and one without carries none. The two agree exactly, so swapping the
/// gate in Task 3 cannot change enterability.
#[test]
fn a_built_brief_carries_a_settlement_site_and_an_unbuilt_one_carries_none() {
    let built = Brief::from_parts(None, None, None, None, 0, true, false);
    let wild = Brief::from_parts(None, None, None, None, 0, false, false);
    assert_eq!(built.site.as_ref().map(|s| s.kind), Some(SiteKind::Settlement));
    assert_eq!(wild.site, None);
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo test -p hornvale-vessel --lib brief::`
Expected: FAIL — no field `site` on `Brief`.

- [ ] **Step 3: Add the field**

Add to `Brief` after `built`:

```rust
    /// The site here, if any — the gate every enterable place hangs off since
    /// decision 0536. For a settlement this mirrors [`Self::built`]; caves and
    /// exotic sites arrive in Tasks 4 and 5.
    pub site: Option<Site>,
```

and in `from_parts`, after computing the other fields:

```rust
        let site = built.then(|| Site::new(SiteKind::Settlement, None));
```

adding `site` to the returned struct literal. Name stays `None` here; Task 7
attaches it.

- [ ] **Step 4: Run it and watch it pass**

Run: `cargo test -p hornvale-vessel --lib brief::`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/src/brief.rs
git commit -m "feat(vessel): Brief carries a Site, mirroring built for settlements"
```

---

### Task 3: `structure_at` gates on the site, not on `built`

**Files:**
- Modify: `windows/vessel/src/structure.rs:60-` (`structure_at`)
- Modify: `windows/vessel/src/session.rs:5166` (the refusal message)

**Interfaces:**
- Consumes: `Brief::site` from Task 2.

- [ ] **Step 1: Write the failing test**

In `windows/vessel/tests/suite/the_prospect.rs` (new file, registered in `tests/suite.rs`):

```rust
//! H1: the rename changes nothing enterable.

use hornvale_vessel::{PossessOpts, Session};

/// The seed-42 flagship is enterable before and after the gate swap. This is
/// the whole of H1 at this task: a real world, the real verb, the real answer.
#[test]
fn the_flagship_is_still_enterable_after_the_gate_swap() {
    let w = crate::common::seed_42_world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let reply = s.handle("enter").text();
    assert!(
        !reply.starts_with("There is nothing here to enter"),
        "the flagship must stay enterable across the gate swap: {reply}"
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-vessel -E 'test(the_prospect)'`
Expected: FAIL — the new refusal wording does not exist yet, so the assertion
is testing the old string. This failure proves the test is wired to the code.

- [ ] **Step 3: Swap the gate**

In `structure_at`, replace the `brief.built` check with:

```rust
    // Decision 0536: the gate is the SITE, not `built`. `built` still means
    // "a structure stands here" and is one property of a settlement; a cave
    // and an exotic site are enterable and were never built.
    brief.site.as_ref()?;
```

In `session.rs:5166`, replace the message:

```rust
            return Turn::Out("There is nothing here to enter.".to_string());
```

The old wording named `built`, which after 0536 is no longer the reason.

- [ ] **Step 4: Run it and watch it pass**

Run: `cargo nextest run -p hornvale-vessel -E 'test(the_prospect)'`
Expected: PASS.

Then the whole package: `cargo nextest run -p hornvale-vessel`
Expected: some tests asserting the OLD refusal string will fail. Update each to
the new wording — this is a message change, not a behaviour change, and each
edit should be a one-line string swap. If any test fails for a reason that is
NOT the message, stop and report it: that would be H1 falsified.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(vessel): the site is the gate, and the refusal stops naming built"
```

---

### Task 4: Caves become sites, derived and calibrated (H2)

**Files:**
- Modify: `domains/terrain/src/features.rs` (add `cave_site_at`)
- Modify: `windows/vessel/src/brief.rs` (`brief_of` consults it)
- Test: `windows/lab/tests/suite/cave_rate_calibration.rs` (new)

**Interfaces:**
- Consumes: `cave_proneness(&MaterialBuffer, f64) -> f64` (`lithology.rs:452`).
- Produces: `cave_site_at(proneness: f64, elevation_m: f64) -> bool`.

**This task DOES change enterability, deliberately.** Record the delta.

- [ ] **Step 1: Write the calibration test (H2)**

```rust
//! H2, preregistered: over 2,000 land facets on 5 seeds, the fraction holding
//! a cave is 1%-8%. Below 1% the widening buys nothing; above 8% caves stop
//! being remarkable. A result outside the band is a FINDING — re-derive the
//! threshold once, in the open, and say so.

/// The preregistered band. Spec §9.
const CAVE_RATE_MIN: f64 = 0.01;
const CAVE_RATE_MAX: f64 = 0.08;

#[test]
fn the_cave_rate_sits_in_its_preregistered_band() {
    let rate = measure_cave_rate(&[42, 13, 7, 1, 100], 2_000);
    assert!(
        (CAVE_RATE_MIN..=CAVE_RATE_MAX).contains(&rate),
        "cave rate {rate:.4} is outside the preregistered band \
         {CAVE_RATE_MIN}..={CAVE_RATE_MAX}. This is a FINDING, not a failure: \
         re-derive CAVE_PRONENESS_THRESHOLD once, record the measured rate in \
         its doc, and say in the chronicle that the band was missed and why."
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-lab -E 'test(cave_rate)'`
Expected: FAIL — `measure_cave_rate` and the threshold do not exist.

- [ ] **Step 3: Implement the predicate**

In `domains/terrain/src/features.rs`:

```rust
/// Whether a cave mouth opens at a facet.
///
/// **Derived, never drawn.** [`crate::lithology::cave_proneness`] is a pure
/// function of the material buffer and drainage, so this needs no roster and
/// no seed label — a cave has the same answer at any resolution the fields can
/// be sampled at. Decision 0537 covers the exotic sites, which are NOT derived
/// this way and do need a draw.
///
/// The elevation floor keeps mouths out of the sea: a cave under water is not
/// a place a walker can enter, and the walk band has no swimming.
/// type-audit: bare-ok(ratio: proneness), waiver(elevation-convention: elevation_m), bare-ok(flag: return)
pub fn cave_site_at(proneness: f64, elevation_m: f64) -> bool {
    proneness >= CAVE_PRONENESS_THRESHOLD && elevation_m > CAVE_MIN_ELEVATION_M
}

/// The proneness a facet must clear to open a mouth.
///
/// **Calibrated, not chosen.** Measured against H2's preregistered 1%-8% band
/// over 2,000 land facets on seeds 42, 13, 7, 1 and 100. Moving this number
/// moves the rate; re-measure rather than reasoning about it.
/// type-audit: bare-ok(ratio)
const CAVE_PRONENESS_THRESHOLD: f64 = 0.35;

/// Sea level plus a margin — a mouth below this is unreachable on foot.
/// type-audit: waiver(elevation-convention)
const CAVE_MIN_ELEVATION_M: f64 = 5.0;
```

Wire it in `brief_of`: where the brief resolves `built`, also resolve
proneness and elevation at the walk facet, and set
`site = Some(Site::new(SiteKind::Cave, None))` when `built` is false and
`cave_site_at` is true. Settlements win when both hold (salience, Task 1).

- [ ] **Step 4: Run, calibrate, run again**

Run: `cargo nextest run -p hornvale-lab -E 'test(cave_rate)'`
If it fails, adjust `CAVE_PRONENESS_THRESHOLD` **once**, record the measured
rate in its doc comment, and re-run. If a second adjustment is needed, stop:
that means the predicate's shape is wrong, not its constant.

- [ ] **Step 5: Record the enterability delta and commit**

Measure and state in the commit message: how many of the 2,000 sampled facets
became enterable. That number is what H1 is scoped around.

```bash
cargo fmt
git add -A
git commit -m "feat(terrain): a cave mouth is a site, derived from proneness with no draw"
```

---

### Task 5: Exotic sites are re-sited to a facet (the epoch)

**Files:**
- Modify: `windows/worldgen/src/streams.rs` (new label)
- Modify: `windows/vessel/src/brief.rs` (`brief_of` consults the placement)
- Test: `windows/vessel/tests/suite/the_prospect.rs`

**Interfaces:**
- Consumes: `LocaleContext::strange_sites() -> Vec<StrangeSite>` (`budget.rs:23`; `StrangeSite.vertex: u32`).
- Produces: `site_facet_for(vertex: u32, seed: Seed, walk_depth: u32) -> Facet`.

**This mints an epoch.** Every world's exotic-site placement moves.

- [ ] **Step 1: Declare the label**

In `windows/worldgen/src/streams.rs`, inside the existing `stream_labels!`:

```rust
    /// Where a placed exotic site lands within its level-6 vertex's territory.
    ///
    /// **Why a draw and not a derivation.** A site is placed at a geosphere
    /// vertex, and those are 110-132 km apart — the same mesh that produced
    /// `CLIM-water-label-resolution-vs-walk-band`, where a whole 6.5 km band
    /// read as river because it inherited one distant point's verdict. Asking
    /// "is a site near me" off that mesh reproduces the defect exactly. So the
    /// site gets a real address, and an address nobody derived has to be drawn.
    /// Decision 0537.
    SITE_PLACEMENT => "site/placement/v1",
```

- [ ] **Step 2: Write the failing test**

```rust
/// A site's facet is stable for a seed and distinct between neighbouring
/// vertices — the two properties that make it an ADDRESS rather than a
/// re-rolled guess.
#[test]
fn a_sites_facet_is_stable_and_vertex_distinct() {
    let seed = Seed(42);
    let a1 = site_facet_for(1953, seed, 13);
    let a2 = site_facet_for(1953, seed, 13);
    let b = site_facet_for(1954, seed, 13);
    assert_eq!(a1, a2, "the same vertex must always place at the same facet");
    assert_ne!(a1, b, "neighbouring vertices must not share a facet");
}
```

- [ ] **Step 3: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-vessel -E 'test(the_prospect)'`
Expected: FAIL — `site_facet_for` does not exist.

- [ ] **Step 4: Implement, keyed on the vertex**

Key the draw on the **vertex index**, never on a generation ordinal — decision
0102. The vertex is a fixed lattice position; the order sites happen to be
enumerated in is not.

- [ ] **Step 5: Run the pin-isolation tests**

Run: `cargo test -p hornvale-astronomy --test suite -- genesis_properties`
Run: `cargo test -p hornvale-terrain --test suite -- tectonic_properties`
Expected: PASS. A new label consumes no draws from an existing stream, so
consumption order must be untouched. **If either fails, stop** — that means the
new label perturbed an existing stream, which is the one thing a new label must
never do.

- [ ] **Step 6: Regenerate, declare, commit**

Run `make rebaseline` and `make rebaseline-goldens`. Artifacts WILL move — this
is the epoch. State in the commit which moved and why. Do not absorb the churn
silently.

```bash
cargo fmt
git add -A
git commit -m "feat(worldgen): exotic sites get a real address — site/placement/v1 (EPOCH)"
```

---

### Task 6: The prose names what is here

**Files:**
- Modify: `windows/locale/src/lib.rs` (the locale description)
- Test: `windows/locale/tests/suite/site_prose.rs` (new)

- [ ] **Step 1: Write the failing test**

```rust
/// Spec §4: a facet with a site names it; a facet without says NOTHING.
/// Silence is honest and it is what makes the density gap visible.
#[test]
fn a_sited_locale_names_it_and_an_empty_one_stays_silent() {
    let ctx = seed_42();
    let flagship = ctx.describe(&flagship_facet(), WorldTime::GENESIS).unwrap();
    assert!(flagship.text().contains("Doaba"), "the flagship must name Doaba");

    let empty = ctx.describe(&three_tiles_east(), WorldTime::GENESIS).unwrap();
    assert!(
        !empty.text().contains("You can enter"),
        "a facet with no site must not offer one: {}", empty.text()
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo nextest run -p hornvale-locale -E 'test(site_prose)'`
Expected: FAIL — no site clause is emitted.

- [ ] **Step 3: Emit the clause**

At most **two** sites, ranked by `Site::salience()`, ties broken by
`strangeness` with `total_cmp`. Name the kind and the name, never the contents.

- [ ] **Step 4: Run it and watch it pass**

Run: `cargo nextest run -p hornvale-locale -E 'test(site_prose)'`

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(locale): a locale names the sites it holds, and stays silent when it holds none"
```

---

### Task 7: What you enter has a name

**Files:**
- Modify: `windows/vessel/src/session.rs` (chamber description)
- Modify: `windows/vessel/src/brief.rs` (attach the settlement's name to its Site)

- [ ] **Step 1: Write the failing test**

```rust
/// Entering Doaba should say Doaba. Before this task it said "A small room,
/// holding a doorway and a screen" — true, and anonymous.
#[test]
fn entering_a_named_site_names_it() {
    let w = crate::common::seed_42_world();
    let (mut s, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let reply = s.handle("enter").text();
    assert!(reply.contains("Doaba"), "entering the village must name it: {reply}");
}
```

- [ ] **Step 2: Run it and watch it fail**

Expected: FAIL — the reply is anonymous.

- [ ] **Step 3: Attach the name and use it**

Resolve the settlement's name in `brief_of` into `Site::name`, and have the
chamber description name the site it belongs to.

- [ ] **Step 4: Run it and watch it pass**

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(vessel): entering a named site names it"
```

---

### Task 8: The map draws sites, not only settlements

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (`settlements_of` at :106 and the draw path)

- [ ] **Step 1: Write the failing test**

Assert a frame containing a known cave draws a glyph for it. Preserve the two
behaviours that already exist: the discovery gate (an undiscovered site is not
drawn at all) and the by-rank major/minor split for settlements. Non-settlement
sites draw at a single weight in this campaign.

- [ ] **Step 2: Run it and watch it fail**

Run: `cd clients/game/bin && cargo test`

- [ ] **Step 3: Widen the roster to sites**

- [ ] **Step 4: Verify**

Run: `make game-check`
Expected: rc=0. It ABORTS at its first failing recipe line, so a red result is
a truncated list — only green is complete.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(game): the map draws every site, not only settlements"
```

---

### Task 9: H3, the density baseline — and close

**Files:**
- Test: `windows/lab/tests/suite/site_density.rs` (new)
- Create: `book/src/chronicle/the-prospect.md`, `docs/retrospectives/the-prospect.md`
- Modify: `book/src/SUMMARY.md`, `docs/decisions/` (0536-0538), `docs/decisions/README.md`

- [ ] **Step 1: Measure H3**

```rust
//! H3, preregistered with NO predicted value (spec §9): the fraction of land
//! facets holding at least one site of any kind. Nobody has this number, and
//! it is what tells the follow-on campaign whether the gap is rendering or
//! generation. Recording it is the success criterion, whatever it says.

#[test]
fn the_site_density_baseline_is_recorded() {
    let rate = measure_site_density(&[42, 13, 7, 1, 100], 2_000);
    println!("H3: site density = {rate:.4} of land facets");
    assert!(rate.is_finite(), "H3 must produce a number");
}
```

- [ ] **Step 2: Run it and record the number**

Run: `cargo nextest run -p hornvale-lab -E 'test(site_density)' --no-capture`
Put the measured value in the chronicle. Do not editorialise it — a low number
is the finding that aims the next campaign.

- [ ] **Step 3: Write decisions 0536, 0537, 0538**

0536 sites replace `built`; 0537 sites are re-sited by a seeded draw (epoch);
0538 `Extent` is modelled and Point-only. Index each in
`docs/decisions/README.md` — **look the filenames up, do not guess them from
titles** (five guessed links were wrong in the last campaign).

- [ ] **Step 4: Chronicle and retrospective**

The chronicle records what landed including anything that went wrong. The
retrospective is process, not product (decision 0020).

- [ ] **Step 5: Regenerate and verify**

Run `make rebaseline`, then diff the declared paths:
`git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(the-prospect): H3's density baseline, three decisions, chronicle and retrospective"
```

---

## Self-review

**Spec coverage.** §3 rename → Tasks 1–3. §4 prose → Task 6. §5 map → Task 8.
§6 widening → Tasks 4 and 5. §7 re-siting and extent → Tasks 5 and 1. §8
non-goals → no task, correctly. §9 H1 → Tasks 1–3 and 6–8; H2 → Task 4; H3 →
Task 9. §11 success criteria 1–5 → Tasks 6, 7, 4+5+8, 4+9, 9.

**Type consistency.** `Site::new(kind, name)` is used identically in Tasks 1,
2 and 4. `SiteKind` variants are `Settlement`/`Cave`/`Exotic` throughout.
`Brief::site` is `Option<Site>` in Tasks 2, 3, 4, 5 and 7. `cave_site_at` takes
`(f64, f64)` in Task 4 and is not referenced elsewhere.

**Known gap, stated rather than hidden.** Tasks 6, 7 and 8's Step 3 describe
the change without pasting final code, because each depends on prose and
drawing surfaces whose exact shape the implementer must read first. Every one
of them has a failing test written out in full, so the test pins the behaviour
even where the implementation is described rather than dictated — which is the
plan's own rule about naming the property and letting the implementer find the
mechanism.
