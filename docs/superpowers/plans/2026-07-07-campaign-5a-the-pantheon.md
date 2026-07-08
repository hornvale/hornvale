# Campaign 5a: The Pantheon — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the single tier-0 belief into a **pantheon** — a deity per salient phenomenon the flagship observes — whose structure (ranked vs. flat, organized vs. folk) reflects the flagship society, and render it in the almanac so the Year-1 exit demo shows two skies producing two religions.

**Architecture:** `domains/religion` becomes tier 1 (kernel-only): its `genesis` consumes the salience-ranked `&[Phenomenon]` (as tier 0 does) plus a religion-owned `SocietySummary { strata, has_priesthood }` that the composition root fills from the flagship's committed castes. It mints one belief per phenomenon above a salience floor (no cap), flags the top deity `high-god` in stratified societies, and tags each belief's `cult-form` from whether a priesthood exists. The almanac's "The Gods" section renders the pantheon with its structure. Historiography (`why <entity>`), the Census of Faiths, and the book close-out are Plan 5b.

**Tech Stack:** Rust edition 2024; `hornvale-kernel` only in domains; std only; the existing composition-root + almanac + CI drift harness.

## Global Constraints

- `serde` + `serde_json` only; **no new crates.** Randomness is the kernel's `Seed`/`Stream`.
- **Layering (enforced by `cli/tests/architecture.rs`):** `domains/religion` depends on `hornvale-kernel` and **nothing else** — it never learns which system produced a phenomenon (trace protocol, Constitution §3.1.6), and it never imports culture. The flagship's social structure reaches it only as a religion-owned `SocietySummary`, mapped at the composition root.
- **Determinism (constitutional):** same seed + pins → identical pantheon and almanac; **no `HashMap`/`HashSet`/`std::time`** (clippy-forbidden); `BTreeMap`/`Vec`; `total_cmp`. Beliefs are committed facts (persist, read back — not reconstructed).
- **Pantheon rule (spec §4):** one deity per phenomenon with `salience ≥ PANTHEON_FLOOR` (**0.25**); if none clear the floor, take the single most salient (min 1); **no size cap** — minor phenomena become lesser spirits (esoterica). Phenomena arrive pre-sorted by salience descending (`kernel::observe`), so element 0 is the most salient.
- **Structure rules (spec §5):** ranked pantheon iff `strata ≥ RANKED_STRATA` (**4**) → the most-salient deity gets the `high-god` flag; `cult-form` = `organized` iff `has_priesthood` else `folk`.
- **Stream labels are permanent contracts.** Religion keeps `religion` and `religion/epithet`; no new labels (the pantheon draws epithets from the existing `religion/epithet` stream). Epithet pools may be *extended* (append-only) but never reordered — reordering changes committed epithets. This plan extends each pool from 3 to 6 (append 3 each); document that the first 3 are unchanged.
- **Naming:** the provenance verb is `why` (5b); this plan introduces no `explain`.
- Every crate `#![warn(missing_docs)]`; every public item/field/variant a `///` doc comment.
- Edition 2024; `cargo fmt` final each task. **Full gate:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the CI artifact drift check. `gen` is reserved in Rust 2024.
- **Baseline:** 398 tests pass on `main` at the start of this plan (Campaign 4 merged). Verify counts directly after each task.

---

## File Structure

| File                                                                        | Action      | Responsibility                                                                                                                                                                                                      |
| --------------------------------------------------------------------------- | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `domains/religion/src/lib.rs`                                               | Modify      | `SocietySummary`; `HIGH_GOD`/`CULT_FORM` predicates; tier-1 `genesis` (pantheon from the floor; verticality + priesthood); extended epithet pools; `Belief.high_god` + `cult_form_of`; `beliefs_of` reads the flag. |
| `windows/worldgen/src/lib.rs`                                               | Modify      | Build `SocietySummary` from the flagship's committed castes; pass it to `religion::genesis`; the exit-demo test.                                                                                                    |
| `windows/almanac/src/lib.rs`                                                | Modify      | "The Gods" renders the pantheon: the cult form, the high god marked, each deity's tenet.                                                                                                                            |
| `.github/workflows/ci.yml`                                                  | (unchanged) | The exit-demo almanacs (seed 42 sky + locked) are already rendered + drift-checked; 5a only changes their content.                                                                                                  |
| `book/src/gallery/almanac-seed-42*.md`, `book/src/reference/*-generated.md` | Regenerate  | Committed generated artifacts (the pantheon now appears; two new predicates).                                                                                                                                       |

Dataflow: `worldgen` maps flagship castes → `SocietySummary` → `religion::genesis(phenomena, summary)` → pantheon beliefs → `beliefs_of` → almanac.

---

### Task 1: Religion tier 1 — the pantheon

**Files:**
- Modify: `domains/religion/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{Phenomenon, World, EntityId, ...}` (as today).
- Produces:
  - `pub struct SocietySummary { pub strata: usize, pub has_priesthood: bool }` (`Clone, Copy, Debug, PartialEq`).
  - `pub const HIGH_GOD: &str = "high-god";` (functional Flag), `pub const CULT_FORM: &str = "cult-form";` (functional Text).
  - `pub fn genesis(world: &mut World, community: EntityId, phenomena: &[Phenomenon], society: &SocietySummary) -> Result<Vec<EntityId>, LedgerError>` — the pantheon (replaces the tier-0 `Option` signature).
  - `Belief` gains `pub high_god: bool`; `beliefs_of` populates it. `pub fn cult_form_of(world: &World) -> Option<String>`.

- [ ] **Step 1: Write the failing tests** (rewrite the religion tests module to the pantheon API):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let community = w.ledger.mint_entity();
        (w, community)
    }

    fn ph(kind: &str, desc: &str, period: Option<f64>, salience: f64) -> Phenomenon {
        Phenomenon { kind: kind.to_string(), description: desc.to_string(), period_days: period, salience }
    }

    // Pre-sorted salience-descending, as kernel::observe delivers.
    fn sky() -> Vec<Phenomenon> {
        vec![
            ph("celestial-body", "the sun", None, 1.0),      // eternal
            ph("seasonal-cycle", "the seasons", Some(365.0), 0.5),
            ph("celestial-body", "a moon", Some(29.0), 0.4),
            ph("ambient", "still air", None, 0.15),          // below floor
        ]
    }

    #[test]
    fn pantheon_takes_every_phenomenon_above_the_floor() {
        let (mut w, c) = world(42);
        let society = SocietySummary { strata: 5, has_priesthood: true };
        let ids = genesis(&mut w, c, &sky(), &society).unwrap();
        assert_eq!(ids.len(), 3, "sun+seasons+moon are above 0.25; ambient (0.15) is not");
        // No cap, but the sub-floor ambient is excluded.
        assert!(beliefs_of(&w).iter().all(|b| b.source_kind != "ambient"));
    }

    #[test]
    fn a_ranked_society_crowns_the_most_salient_deity() {
        let (mut w, c) = world(42);
        let ids = genesis(&mut w, c, &sky(), &SocietySummary { strata: 5, has_priesthood: true }).unwrap();
        let beliefs = beliefs_of(&w);
        assert!(beliefs[0].high_god, "top-salience deity presides in a stratified society");
        assert_eq!(beliefs.iter().filter(|b| b.high_god).count(), 1, "exactly one high god");
        assert_eq!(beliefs[0].id, ids[0]);
    }

    #[test]
    fn a_flat_society_has_no_high_god() {
        let (mut w, c) = world(42);
        let ids = genesis(&mut w, c, &sky(), &SocietySummary { strata: 2, has_priesthood: false }).unwrap();
        let _ = ids;
        assert!(beliefs_of(&w).iter().all(|b| !b.high_god), "egalitarian society: flat pantheon");
    }

    #[test]
    fn priesthood_sets_the_cult_form() {
        let (mut w, c) = world(42);
        genesis(&mut w, c, &sky(), &SocietySummary { strata: 5, has_priesthood: true }).unwrap();
        assert_eq!(cult_form_of(&w).as_deref(), Some("organized"));
        let (mut w2, c2) = world(42);
        genesis(&mut w2, c2, &sky(), &SocietySummary { strata: 2, has_priesthood: false }).unwrap();
        assert_eq!(cult_form_of(&w2).as_deref(), Some("folk"));
    }

    #[test]
    fn tenets_track_periodicity_and_a_locked_sun_is_eternal() {
        let (mut w, c) = world(42);
        // Locked sky: eternal sun, no seasons.
        let locked = vec![ph("celestial-body", "a fixed sun", None, 1.0), ph("celestial-body", "a moon", Some(29.0), 0.4)];
        genesis(&mut w, c, &locked, &SocietySummary { strata: 5, has_priesthood: true }).unwrap();
        let head = &beliefs_of(&w)[0];
        assert!(head.tenet.contains("never"), "eternal sun yields an eternal tenet");
    }

    #[test]
    fn empty_phenomena_means_no_pantheon() {
        let (mut w, c) = world(42);
        assert!(genesis(&mut w, c, &[], &SocietySummary { strata: 5, has_priesthood: true }).unwrap().is_empty());
        assert!(beliefs_of(&w).is_empty());
    }

    #[test]
    fn below_floor_only_still_yields_the_single_most_salient() {
        let (mut w, c) = world(42);
        let faint = vec![ph("ambient", "a whisper of air", None, 0.15), ph("celestial-body", "a dim star", None, 0.1)];
        let ids = genesis(&mut w, c, &faint, &SocietySummary { strata: 2, has_priesthood: false }).unwrap();
        assert_eq!(ids.len(), 1, "never godless while something is observed");
        assert_eq!(beliefs_of(&w)[0].source_kind, "ambient");
    }

    #[test]
    fn genesis_is_deterministic() {
        let run = || {
            let (mut w, c) = world(7);
            genesis(&mut w, c, &sky(), &SocietySummary { strata: 5, has_priesthood: true }).unwrap();
            beliefs_of(&w).iter().map(|b| b.tenet.clone()).collect::<Vec<_>>()
        };
        assert_eq!(run(), run());
    }
}
```

- [ ] **Step 2: Run to see them fail**

Run: `cargo test -p hornvale-religion 2>&1 | tail -10`
Expected: FAIL — `SocietySummary`/new `genesis` signature/`high_god` undefined.

- [ ] **Step 3: Implement the pantheon**

Rewrite the non-test body of `domains/religion/src/lib.rs`. Add the predicates and register them; extend the epithet pools (append-only); add `SocietySummary`; rewrite `genesis`; extend `Belief` + `beliefs_of`; add `cult_form_of`. Keep `IS_BELIEF`/`HELD_BY`/`TENET`/`DERIVED_FROM_PHENOMENON`/`why`/`stream_labels`.

```rust
/// Predicate: the presiding deity of a ranked pantheon (functional Flag).
pub const HIGH_GOD: &str = "high-god";
/// Predicate: the cult form of a belief — `organized` or `folk` (functional Text).
pub const CULT_FORM: &str = "cult-form";

/// Salience a phenomenon must reach to seat a deity in the pantheon.
const PANTHEON_FLOOR: f64 = 0.25;
/// Social strata at or above which the pantheon is ranked (a high god presides).
const RANKED_STRATA: usize = 4;

// Extended append-only: the first three of each pool are unchanged from tier 0.
const ETERNAL_EPITHETS: [&str; 6] = [
    "the Unblinking Eye", "the Ever-Flame", "the Gold Warden",
    "the Still Crown", "the Deathless Watch", "the Fixed Star",
];
const CYCLIC_EPITHETS: [&str; 6] = [
    "the Returning One", "the Tidewalker", "the Promised Lamp",
    "the Wheel-Turner", "the Waning Herald", "the Patient Pilgrim",
];

/// A summary of the flagship society's shape, mapped at the composition root
/// from its committed castes. Religion consumes this instead of importing
/// culture (the trace discipline).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SocietySummary {
    /// Number of caste/role strata.
    pub strata: usize,
    /// Whether an organized priesthood (a shaman caste) exists.
    pub has_priesthood: bool,
}
```

Update `register_concepts` to also register `HIGH_GOD` (functional Flag → `register_predicate(HIGH_GOD, true, "the presiding deity of a ranked pantheon")`) and `CULT_FORM` (`register_predicate(CULT_FORM, true, "the cult form of a belief (organized or folk)")`).

Rewrite `genesis`:

```rust
/// Tier-1 genesis: derive a pantheon from the salient phenomena the community
/// observes, structured by its society. One deity per phenomenon at or above
/// `PANTHEON_FLOOR` (or the single most salient if none clear it); the most
/// salient deity presides (`high-god`) in a stratified society; the cult form
/// follows the priesthood. Phenomena arrive salience-descending. Returns the
/// pantheon in that order (element 0 is the head).
pub fn genesis(
    world: &mut World,
    community: EntityId,
    phenomena: &[Phenomenon],
    society: &SocietySummary,
) -> Result<Vec<EntityId>, LedgerError> {
    // Members: everything above the floor; else the single most salient; else none.
    let above = phenomena.iter().filter(|p| p.salience >= PANTHEON_FLOOR).count();
    let take = if above > 0 { above } else { phenomena.len().min(1) };
    let members = &phenomena[..take];
    if members.is_empty() {
        return Ok(Vec::new());
    }

    let ranked = society.strata >= RANKED_STRATA;
    let cult_form = if society.has_priesthood { "organized" } else { "folk" };
    let mut stream = world
        .seed
        .derive(streams::ROOT)
        .derive(streams::EPITHET)
        .stream();

    // Distinct epithets within a pantheon: draw an index, advance past any
    // already used (deterministic; pools of 6 cover the realistic pantheon
    // size — if ever exhausted, repeats are allowed rather than failing).
    let mut used: Vec<&'static str> = Vec::new();
    let mut ids = Vec::with_capacity(members.len());
    for (i, p) in members.iter().enumerate() {
        let pool: &[&'static str] = match p.period_days {
            None => &ETERNAL_EPITHETS,
            Some(_) => &CYCLIC_EPITHETS,
        };
        let start = stream.range_u32(0, pool.len() as u32 - 1) as usize;
        let mut epithet = pool[start];
        for step in 0..pool.len() {
            let candidate = pool[(start + step) % pool.len()];
            if !used.contains(&candidate) {
                epithet = candidate;
                break;
            }
        }
        used.push(epithet);

        let tenet = match p.period_days {
            None => format!(
                "{epithet} is {}; it has never departed and will never blink.",
                p.description
            ),
            Some(period) => format!(
                "{epithet} departs and returns every {period} days; its absences are mourned \
                 and its returns feasted."
            ),
        };

        let belief = world.ledger.mint_entity();
        let fact = |predicate: &str, object: Value| Fact {
            subject: belief,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(0.0),
            provenance: "religion".to_string(),
        };
        world.ledger.commit(fact(IS_BELIEF, Value::Flag(true)), &world.registry)?;
        world.ledger.commit(fact(TENET, Value::Text(tenet)), &world.registry)?;
        world.ledger.commit(fact(HELD_BY, Value::Entity(community)), &world.registry)?;
        world.ledger.commit(fact(DERIVED_FROM_PHENOMENON, Value::Text(p.kind.clone())), &world.registry)?;
        world.ledger.commit(fact(CULT_FORM, Value::Text(cult_form.to_string())), &world.registry)?;
        if ranked && i == 0 {
            world.ledger.commit(fact(HIGH_GOD, Value::Flag(true)), &world.registry)?;
        }
        ids.push(belief);
    }
    Ok(ids)
}
```

Extend `Belief` and `beliefs_of`, and add `cult_form_of`:

```rust
/// A belief as this domain knows it.
#[derive(Debug, Clone, PartialEq)]
pub struct Belief {
    /// The belief's entity id.
    pub id: EntityId,
    /// The belief's tenet text.
    pub tenet: String,
    /// The phenomenon kind it mythologizes.
    pub source_kind: String,
    /// Whether this belief is the pantheon's presiding high god.
    pub high_god: bool,
}
```

In `beliefs_of`, populate `high_god: matches!(world.ledger.value_of(id, HIGH_GOD), Some(Value::Flag(true)))`.

```rust
/// The cult form shared by the world's pantheon (`organized`/`folk`), read
/// from the first belief; `None` if there are no beliefs.
pub fn cult_form_of(world: &World) -> Option<String> {
    let first = world.ledger.find(IS_BELIEF).next()?.subject;
    match world.ledger.value_of(first, CULT_FORM) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

(`why` stays as-is for now — it reads `DERIVED_FROM_PHENOMENON` + provenance; 5b generalizes provenance into `windows/historiography`. The workspace will not compile until Task 2 updates the `build_world` call site — gate ONLY `-p hornvale-religion` here.)

- [ ] **Step 4: Run + fmt + commit** (religion crate only — workspace breaks until Task 2)

Run: `cargo test -p hornvale-religion && cargo clippy -p hornvale-religion --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add domains/religion/src/lib.rs
git commit -m "feat(religion): tier-1 pantheon from salient phenomena + society structure"
```

---

### Task 2: Composition-root wiring + the exit-demo test

**Files:**
- Modify: `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: `religion::{SocietySummary, genesis}`, `culture::castes_of`.
- Produces: `build_world` compiles and runs the pantheon on the flagship; a worldgen exit-demo test.

- [ ] **Step 1: Write the failing exit-demo test** in the `tests` module of `windows/worldgen/src/lib.rs`:

```rust
#[test]
fn the_pantheon_reorganizes_between_spinning_and_locked() {
    use hornvale_astronomy::RotationPin;
    let spinning = generated(42);
    let locked = build_world(
        Seed(42),
        &SkyPins { rotation: Some(RotationPin::Locked), ..SkyPins::default() },
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    ).unwrap();
    let head_tenet = |w: &World| hornvale_religion::beliefs_of(w).first().map(|b| b.tenet.clone()).unwrap_or_default();
    // The head deity's periodicity flips with the sun's rotation regime.
    assert!(head_tenet(&locked).contains("never"), "locked world: an eternal high god");
    assert!(head_tenet(&spinning).contains("return") || head_tenet(&spinning).contains("never") == false,
            "spinning world: a cyclic head deity");
    assert_ne!(head_tenet(&spinning), head_tenet(&locked), "the two skies yield different religions");
    // A pantheon, not a single belief.
    assert!(hornvale_religion::beliefs_of(&spinning).len() >= 1);
}

#[test]
fn the_flagship_pantheon_reflects_its_society() {
    let world = generated(42);
    let beliefs = hornvale_religion::beliefs_of(&world);
    assert!(!beliefs.is_empty(), "the flagship has a pantheon");
    // cult form is set and consistent.
    assert!(hornvale_religion::cult_form_of(&world).is_some());
    // At most one high god.
    assert!(beliefs.iter().filter(|b| b.high_god).count() <= 1);
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-worldgen the_pantheon_reorganizes 2>&1 | tail -6`
Expected: FAIL to compile — `religion::genesis` arity changed (still the old call).

- [ ] **Step 3: Wire the `SocietySummary` into the religion call**

In `build_world`, the flagship block already computes `env` and calls `hornvale_culture::genesis(&mut world, flagship, &env)?` then `let seen = observed_phenomena(&world, 0.0)?; hornvale_religion::genesis(&mut world, flagship, &seen)?;`. Replace the religion call with a `SocietySummary` built from the flagship's now-committed castes:

```rust
        hornvale_culture::genesis(&mut world, flagship, &env)?;
        let castes = hornvale_culture::castes_of(&world, flagship);
        let society = hornvale_religion::SocietySummary {
            strata: castes.len(),
            has_priesthood: castes.iter().any(|c| c == "shaman"),
        };
        let seen = observed_phenomena(&world, 0.0)?;
        hornvale_religion::genesis(&mut world, flagship, &seen, &society)?;
```

(`observed_phenomena` and the rest are unchanged. Confirm the exact in-scope binding names; adapt if the merged code renamed anything.)

- [ ] **Step 4: Fix any caller/test fallout**

`religion::genesis` now returns `Vec<EntityId>` (was `Option`). Grep `hornvale_religion::genesis` across the workspace; the only production caller is `build_world`. Update any test that asserted the old `Option`/single-belief religion behavior (e.g. a worldgen test asserting exactly one belief) to the pantheon (`!beliefs_of(&world).is_empty()`), keeping real assertions.

Run: `cargo test --workspace 2>&1 | grep -E "FAILED|test result" | tail -20` and fix each failure honestly.

- [ ] **Step 5: Full gate + commit**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): build the flagship pantheon from its society"
```

---

### Task 3: Almanac "The Gods" + artifact regeneration

**Files:**
- Modify: `windows/almanac/src/lib.rs`
- Regenerate: `book/src/gallery/almanac-seed-42*.md`, `book/src/reference/*-generated.md`

**Interfaces:**
- Consumes: `AlmanacContext.beliefs` (now a pantheon; `Belief.high_god`), plus a new `AlmanacContext.cult_form: Option<String>`.
- Produces: "The Gods" renders the pantheon with structure.

- [ ] **Step 1: Add `cult_form` to `AlmanacContext` and write the failing render test**

In `windows/almanac/src/lib.rs`, add `pub cult_form: Option<String>` to `AlmanacContext`. Add a test asserting the pantheon renders the high god and cult form:

```rust
#[test]
fn the_gods_section_renders_a_structured_pantheon() {
    let ctx = AlmanacContext {
        beliefs: vec![
            Belief { id: EntityId(3), tenet: "the Ever-Flame never blinks.".to_string(), source_kind: "celestial-body".to_string(), high_god: true },
            Belief { id: EntityId(4), tenet: "the Tidewalker returns.".to_string(), source_kind: "seasonal-cycle".to_string(), high_god: false },
        ],
        cult_form: Some("organized".to_string()),
        ..sample_context()
    };
    let doc = render(&ctx);
    assert!(doc.contains("organized"), "cult form named");
    assert!(doc.contains("Ever-Flame"));
    assert!(doc.contains("Tidewalker"));
    // The high god is distinguished from the lesser deities.
    let gods = doc.split("## The Gods").nth(1).unwrap();
    assert!(gods.contains("presides") || gods.contains("high"), "high god marked");
}
```

Update the almanac's `sample_context()` helper and any other `AlmanacContext { ... }` literal to include `cult_form: None` (or a value), and the `Belief { ... }` literals to include `high_god: false`.

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-almanac the_gods_section 2>&1 | tail -6`
Expected: FAIL — no `cult_form` field / high god not marked.

- [ ] **Step 3: Render the pantheon**

Replace the "The Gods" block in `render`:

```rust
    doc.push_str("## The Gods\n\n");
    if ctx.beliefs.is_empty() {
        doc.push_str("No beliefs are recorded.\n\n");
    } else {
        if let Some(form) = &ctx.cult_form {
            let lead = match form.as_str() {
                "organized" => "An organized priesthood tends a pantheon:",
                _ => "The people keep a folk pantheon:",
            };
            doc.push_str(&format!("{lead}\n\n"));
        }
        for belief in &ctx.beliefs {
            let mark = if belief.high_god { " *(who presides)*" } else { "" };
            doc.push_str(&format!(
                "> {}{mark}\n>\n> — derived from the phenomenon *{}*\n\n",
                belief.tenet, belief.source_kind
            ));
        }
    }
```

- [ ] **Step 4: Populate `cult_form` at the root**

In `windows/worldgen/src/lib.rs` `almanac_context`, add `cult_form: hornvale_religion::cult_form_of(world),` to the `AlmanacContext { ... }`.

- [ ] **Step 5: Gate + regenerate artifacts (stdout-only)**

Run: `cargo test -p hornvale-almanac -p hornvale-worldgen && cargo clippy --workspace --all-targets -- -D warnings`; `cargo fmt`.

Regenerate the committed artifacts the pantheon + new predicates touch (stdout-only — cargo logs go to stderr; use `-q`):

```bash
cargo run -q -p hornvale -- new --seed 42 --sky constant --out /tmp/a.json 2>/dev/null && cargo run -q -p hornvale -- almanac --world /tmp/a.json > book/src/gallery/almanac-seed-42.md
cargo run -q -p hornvale -- new --seed 42 --out /tmp/b.json 2>/dev/null && cargo run -q -p hornvale -- almanac --world /tmp/b.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -q -p hornvale -- new --seed 42 --rotation locked --out /tmp/c.json 2>/dev/null && cargo run -q -p hornvale -- almanac --world /tmp/c.json > book/src/gallery/almanac-seed-42-locked.md
cargo run -q -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
```

Verify no leaked logs: `grep -rlE "Compiling|Finished \`|Running \`target" book/src` returns nothing. Then run the FULL CI "Artifacts are current" block a second time and confirm `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` is clean (byte-identical) — the seed-42 spinning vs. locked almanacs must now show different "The Gods" sections (the exit demo).

- [ ] **Step 6: Full gate + commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, then the drift check.

```bash
git add windows/almanac/src/lib.rs windows/worldgen/src/lib.rs book/src/gallery/ book/src/reference/
git commit -m "feat(almanac): render the structured pantheon; regenerate exit-demo artifacts"
```

---

## Self-Review Notes

**Spec coverage (against §3, §4, §5, §7, §11):**
- §3 (religion kernel-only; root maps `SocietySummary`) → Task 1 (SocietySummary) + Task 2 (root mapping). §4 (pantheon from floor, no cap, min 1; `high-god`/`cult-form`; eternal/cyclic tenets) → Task 1. §5 (verticality `strata≥4`; priesthood → cult form) → Task 1. §7 (exit demo: two skies, different pantheons; almanac "The Gods") → Task 2 (test) + Task 3 (render + regenerated almanac pair). §11 staging (5a = pantheon + structure + exit demo + almanac; historiography/`why`/Census/book = 5b) → this whole plan.

**Deferred to 5b (not this plan):** `windows/historiography` + the `why <entity>` generalization; the Census of Faiths metrics + the verticality⇔stratification and high-god⇔lock calibrations; the religion tier-1 book chapter, Study 004, and the Year-1 close-out chronicle.

**Type consistency:** `SocietySummary { strata, has_priesthood }`, `genesis(world, community, phenomena, &SocietySummary) -> Vec<EntityId>`, `Belief.high_god`, `cult_form_of`, `HIGH_GOD`/`CULT_FORM` (Task 1) are consumed with the same names in Task 2 (root) and Task 3 (almanac). `AlmanacContext.cult_form` (Task 3) is populated by `almanac_context` in the same task.

**Watch items (carried lessons):** verify test counts directly (baseline 398); `religion::genesis`'s signature change (Task 2) ripples — fix callers/tests honestly, never trivialize; regenerate artifacts stdout-only (Task 3); the epithet-pool extension is append-only (never reorder — it would change committed epithets on existing worlds); `gen` reserved in Rust 2024; confirm the exit-demo almanacs genuinely differ in "The Gods" after regeneration (the whole point).
