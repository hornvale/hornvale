# The Scaffold Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the history bake's internal handles their own type, split the occupation record into honest bake-side and ledger-side forms, and replace three mint-order sort tie-breaks with a material-fact comparator.

**Architecture:** `domains/history` gains a shared `Occupation` core and makes `Founding`/`Ended` generic over a handle type. `windows/worldgen` instantiates them with a new bake-local `BakeId`; the ledger side instantiates them with `EntityId`. Three comparators — two in worldgen, one in the almanac's deliberately-duplicated decoder — stop consulting mint order.

**Tech Stack:** Rust edition 2024. No new dependencies; the workspace allowlist is `serde`, `serde_json`, `libm`.

## Global Constraints

- **No committed fact changes.** `cli/tests/fixtures/world-seed-42.json` must stay byte-identical. The comparators live in read helpers; `vestige.rs` states of itself "no live mutation, no committed facts."
- A domain crate depends on `hornvale-kernel` and **nothing else** (`cli/tests/architecture.rs` asserts `normal_deps == ["hornvale-kernel"]` exactly).
- No `HashMap`/`HashSet`. No wall-clock. `#![warn(missing_docs)]`. Every pub-boundary primitive carries a `type-audit:` tag — the audit is **default-deny**, and a tag in the *wrong class* passes silently, so check decision 0028's rubric rather than copying a neighbour.
- Float ordering uses `f64::total_cmp`. Bare float equality is banned.
- **`BakeId` must NOT derive `Serialize`/`Deserialize`.** `EntityId` does, because it is saved. A handle that never touches the ledger deriving serde would invite exactly the confusion this campaign removes.
- `cargo fmt` as the final step before every commit. Commit messages go to a file and through `git commit -F` — never a heredoc with backticks, which has previously executed as command substitution.

---

## File Structure

| File | Responsibility |
|---|---|
| `domains/history/src/record.rs` | Modify. `Occupation` core; `Founding<I>`/`Ended<I>` generic; `OccupationRecord` re-expressed with `id: EntityId`. |
| `windows/worldgen/src/history_bake.rs` | Modify. `BakeId` newtype; `Bake::mint` returns it; `Community` and the bake's records use `BakeOccupation`. |
| `windows/worldgen/src/history_emit.rs` | Modify. `bake_to_ledger: BTreeMap<BakeId, EntityId>`; `reconstruct_occupation` sets `id`; two comparators. |
| `windows/almanac/src/history.rs` | Modify. `record_of` sets `id`; `layers_at`'s comparator — the third, and the one a reader sees. |

**Six test helpers construct an `OccupationRecord`** and all need their shape updated: `domains/history/tests/record.rs:16` (plus a struct-update spread at `:36`), `domains/history/tests/flesh.rs:21`, `cli/tests/history_render.rs:34`, `windows/worldgen/tests/history_emit.rs:30`, `windows/worldgen/src/vestige.rs:235`.

**Artifacts that move:** `book/src/gallery/vestige-seed-42.png` and `.md` (via `occupations_by_cell` → `vestiges_field` → `residue_pixels`), and `almanac-seed-42*.md` / `history-seed-42.md` (via the almanac's own `layers_at`). **`world-seed-42.json` must NOT move.**

---

### Task 1: The type split

**Files:**
- Modify: `domains/history/src/record.rs`
- Modify: `windows/worldgen/src/history_bake.rs`, `windows/worldgen/src/history_emit.rs`
- Modify: `windows/almanac/src/history.rs`
- Modify: the six test helpers listed above

**Interfaces:**
- Produces: `hornvale_history::record::{Occupation, OccupationRecord, Founding, Ended}` where `Founding<I>` and `Ended<I>` are generic; `OccupationRecord { core: Occupation, id: EntityId, founded_from: Founding<EntityId>, ended_by: Ended<EntityId> }`. And `hornvale_worldgen::history_bake::{BakeId, BakeOccupation}`.

- [ ] **Step 1: Make `Founding` and `Ended` generic**

In `domains/history/src/record.rs`, replace both enums. Keep the doc comments and derives exactly — `Clone, Copy, Debug, PartialEq, Eq`:

```rust
/// How an occupation ended: on its own terms, or at another entity's hand.
///
/// Generic over the handle type so the bake can reference its own private
/// handles and the ledger side can reference committed entities, without the
/// two being interchangeable.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ended<I> {
    /// No antagonist entity — famine, plague, or an orderly departure.
    Nature,
    /// Ended at the hand of another entity (a raiding people, a rival
    /// community, ...).
    By(I),
}

/// How an occupation began: raised from nothing at a site, or founded by
/// settlers from another community.
///
/// Generic over the handle type, for the same reason as [`Ended`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Founding<I> {
    /// The first occupation at a site — no predecessor community.
    Genesis(CellId),
    /// Founded by settlers dispatched from an existing community.
    From(I),
}
```

- [ ] **Step 2: Extract the shared core**

Add above `OccupationRecord`:

```rust
/// What both sides of the emit boundary agree an occupation is: a people, a
/// place, a span, and how it fared. Everything here is a committed fact or
/// derivable from one.
///
/// The handle-bearing fields — which community, which lineage, who founded it,
/// who ended it — live on the bake-side and ledger-side types instead, because
/// they mean different things there.
/// type-audit: bare-ok(count: founded), bare-ok(count: ended), bare-ok(count: peak_population)
#[derive(Clone, Debug, PartialEq)]
pub struct Occupation {
    /// The people occupying the site.
    pub people: KindId,
    /// The Geosphere cell the occupation sits on.
    pub site: CellId,
    /// The standard day the occupation began.
    pub founded: f64,
    /// The standard day the occupation ended, `None` if still alive.
    pub ended: Option<f64>,
    /// The highest population this occupation ever reached.
    pub peak_population: u32,
    /// The technological horizon of this occupation.
    pub tech: TechHorizon,
    /// What this occupation was for.
    pub function: Function,
    /// The deity this occupation's people held foremost, if any.
    pub deity: Option<KindId>,
    /// The tongue this occupation's people spoke, if any.
    pub tongue: Option<KindId>,
    /// Why the occupation ended, if it has.
    pub cause: Option<CauseOfEnd>,
    /// How notable the occupation was.
    pub notability: Notability,
}

impl Occupation {
    /// How long the occupation has lasted (or lasted), in standard days, as
    /// of `now`. Ended occupations ignore `now` entirely.
    /// type-audit: bare-ok(count: now), bare-ok(count: return)
    pub fn tenure(&self, now: f64) -> f64 {
        self.ended.unwrap_or(now) - self.founded
    }

    /// Whether the occupation is still ongoing.
    /// type-audit: bare-ok(flag: return)
    pub fn is_alive(&self) -> bool {
        self.ended.is_none()
    }
}
```

- [ ] **Step 3: Re-express `OccupationRecord` as the ledger-side type**

Replace the existing struct and its `impl`:

```rust
/// One span of a people occupying a site, as **reconstructed from committed
/// facts**. The ledger-side half of the pair.
///
/// It carries no `community` and no `lineage`: neither is ever emitted as a
/// fact, so a reconstructed record genuinely does not know them. What it does
/// know is its own identity, which earlier versions of this type smuggled into
/// the `community` field and called a placeholder.
#[derive(Clone, Debug, PartialEq)]
pub struct OccupationRecord {
    /// The facts both sides agree on.
    pub core: Occupation,
    /// This occupation's own entity — the subject of every fact above.
    pub id: EntityId,
    /// How the occupation began.
    pub founded_from: Founding<EntityId>,
    /// How the occupation ended.
    pub ended_by: Ended<EntityId>,
}

impl OccupationRecord {
    /// How long the occupation lasted, as of `now`. Delegates to [`Occupation`].
    /// type-audit: bare-ok(count: now), bare-ok(count: return)
    pub fn tenure(&self, now: f64) -> f64 {
        self.core.tenure(now)
    }

    /// Whether the occupation is still ongoing. Delegates to [`Occupation`].
    /// type-audit: bare-ok(flag: return)
    pub fn is_alive(&self) -> bool {
        self.core.is_alive()
    }
}
```

- [ ] **Step 4: Compile and read the errors as your worklist**

Run: `cargo check --workspace 2>&1 | tee /tmp/hv-split.log`
Expected: **many** errors. That is the point — every one is a site that was relying on the confusion. Work through them; do not suppress any with `allow`.

- [ ] **Step 5: Add `BakeId` and the bake-side record**

In `windows/worldgen/src/history_bake.rs`, near the top:

```rust
/// A handle to a community *inside the bake*, and nowhere else.
///
/// Deliberately not an `EntityId`: these live for the duration of one
/// simulation and are translated to real entities at emit. Deliberately not
/// `Serialize`/`Deserialize` either — a handle that never reaches the ledger
/// has no business being saveable, and `EntityId` deriving serde is exactly
/// what made the two easy to confuse.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BakeId(pub u64);

/// One span of a people occupying a site, as the **bake** holds it. The
/// bake-side half of the pair.
///
/// Unlike [`OccupationRecord`] it knows its community and its lineage, because
/// the simulation tracks both; neither survives emit, because neither is
/// committed as a fact.
#[derive(Clone, Debug, PartialEq)]
pub struct BakeOccupation {
    /// The facts both sides agree on.
    pub core: hornvale_history::record::Occupation,
    /// The community this occupation belongs to.
    pub community: BakeId,
    /// The lineage this occupation continues.
    pub lineage: BakeId,
    /// How the occupation began.
    pub founded_from: hornvale_history::record::Founding<BakeId>,
    /// How the occupation ended.
    pub ended_by: hornvale_history::record::Ended<BakeId>,
}
```

Change `Bake::mint` to return `BakeId`, and `Bake::next_id` stays a `u64`:

```rust
    /// Mint a fresh, never-reused bake-local handle.
    fn mint(&mut self) -> BakeId {
        let id = BakeId(self.next_id);
        self.next_id += 1;
        id
    }
```

`Community`'s `id` and `lineage` fields become `BakeId`. `Bake::records` becomes `Vec<BakeOccupation>`. Note `Community` currently derives **nothing** — leave it that way unless a compile error demands otherwise, and say which error if so.

- [ ] **Step 6: Update the two decoders**

`reconstruct_occupation` (`history_emit.rs:371`) and `record_of` (`almanac/history.rs:254`) both currently set `community: entity, lineage: entity`. Both become `id: entity`, with the eleven shared fields moving inside `core: Occupation { … }`. Nothing else about either function changes.

**These two are a deliberate un-DRY'd pair** — the almanac keeps its own copy to avoid a dependency cycle, and its doc states they are "kept in lockstep... by copying verbatim in both directions." Change both identically. If you find yourself writing something different in one, stop and report.

- [ ] **Step 7: Update `bake_to_ledger`**

In `emit_history` (`history_emit.rs:119`), the map becomes `BTreeMap<BakeId, EntityId>`, built from `r.community` (now a `BakeId`) to the minted entity. The lookups that translate `Founding::From` and `Ended::By` now convert `Founding<BakeId>` → `Founding<EntityId>` explicitly, which is the conversion that used to be invisible.

- [ ] **Step 8: Update the six test helpers**

Each constructs an `OccupationRecord` literal. Each becomes a `core: Occupation { … }` plus the handle fields. Where a helper takes a `community` parameter, decide per file whether the caller means a bake handle or a ledger entity — the bake-side tests (`windows/worldgen/tests/history_bake.rs`, `history_tithe.rs`) mean `BakeId`; the ledger-side ones mean `EntityId`. **If a helper is used by tests on both sides, that is a finding — report it rather than papering over it with a conversion.**

- [ ] **Step 9: Verify the split is real**

Add to `domains/history/tests/record.rs`:

```rust
#[test]
fn the_core_carries_the_shared_facts_and_the_record_carries_identity() {
    use hornvale_history::record::{Ended, Founding, Occupation, OccupationRecord};
    use hornvale_kernel::{CellId, EntityId, KindId};

    let core = Occupation {
        people: KindId("goblin"),
        site: CellId(7),
        founded: 100.0,
        ended: Some(200.0),
        peak_population: 42,
        tech: hornvale_history::record::TechHorizon::Iron,
        function: hornvale_history::record::Function::Agrarian,
        deity: None,
        tongue: None,
        cause: Some(hornvale_history::record::CauseOfEnd::Fled),
        notability: hornvale_history::record::Notability::Common,
    };
    assert_eq!(core.tenure(500.0), 100.0, "an ended occupation ignores `now`");
    assert!(!core.is_alive());

    let r = OccupationRecord {
        core: core.clone(),
        id: EntityId::new(9).expect("nonzero"),
        founded_from: Founding::Genesis(CellId(7)),
        ended_by: Ended::Nature,
    };
    assert_eq!(r.tenure(500.0), core.tenure(500.0), "the record delegates");
    assert_eq!(r.id.get(), 9, "a record knows its own identity, not a placeholder");
}
```

- [ ] **Step 10: Run the suites**

```bash
cargo test -q -p hornvale-history > /tmp/hv-t1a.log 2>&1; echo "history=$?"
cargo test -q -p hornvale-worldgen > /tmp/hv-t1b.log 2>&1; echo "worldgen=$?"
cargo test -q -p hornvale-almanac > /tmp/hv-t1c.log 2>&1; echo "almanac=$?"
grep -E "^test result|FAILED" /tmp/hv-t1a.log /tmp/hv-t1b.log /tmp/hv-t1c.log
```

Expected: all green. **This task changes no behaviour** — it is a type split. If any test's *assertions* had to change (as opposed to its construction syntax), that is a behaviour change and a finding; report which and why.

- [ ] **Step 11: `world-seed-42.json` must not have moved**

```bash
cargo test -q -p hornvale --test lens_purity > /tmp/hv-t1d.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-t1d.log
```

Expected: green. A red result means the refactor leaked into `emit_history` or `bake()` and changed commit order — **stop and report; do not rebaseline.**

- [ ] **Step 12: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add -A
git commit -F <a message file>
```

---

### Task 2: The material comparator

**Files:**
- Modify: `windows/worldgen/src/history_emit.rs` (`occupations_at` ~:333, `occupations_by_cell` ~:357)
- Modify: `windows/almanac/src/history.rs` (`layers_at` ~:201)

**Interfaces:**
- Consumes: `OccupationRecord { core, id, founded_from, ended_by }` from Task 1.

- [ ] **Step 1: Write the failing test**

No existing test exercises a genuine same-day tie — `occupation_records_round_trip_every_committed_field` says so of itself ("this fixture never restacks a site"). Add to `windows/worldgen/tests/history_emit.rs`:

```rust
#[test]
fn same_day_layers_order_by_material_facts_not_mint_order() {
    // Two occupations of one cell founded the same day. The one that ended
    // FIRST lies deeper — that is what a stratigraphy is. Mint order is
    // deliberately the reverse, so a mint-order comparator fails this.
    let mut w = world_with_registry();
    let early_end = commit_occupation(&mut w, CellId(4), 100.0, Some(150.0), 20);
    let late_end = commit_occupation(&mut w, CellId(4), 100.0, Some(900.0), 20);
    assert!(
        early_end.get() > late_end.get(),
        "fixture must mint the early-ending record LAST, or the test proves nothing"
    );

    let layers = occupations_at(&w, CellId(4));
    assert_eq!(layers.len(), 2);
    assert_eq!(
        layers[0].id, early_end,
        "the layer that closed first lies deeper, whatever order it was minted in"
    );
}
```

`world_with_registry` and `commit_occupation` are helpers you must write beside it, following the construction pattern the existing tests in that file already use for committing `occ-*` facts. Read one first; do not invent a new committing style.

- [ ] **Step 2: Run it and watch it fail**

Run: `cargo test -q -p hornvale-worldgen --test history_emit same_day_layers`
Expected: FAIL — the mint-order tie-break puts `late_end` first.

- [ ] **Step 3: Replace all three comparators**

The comparator, identical in all three places:

```rust
        // Material facts only. A layer that closed earlier lies deeper, which
        // is what a stratigraphy is; a still-living occupation is the top
        // layer; peak breaks the remainder. `founded_from` closes the last
        // ties — ancestry is genuinely what distinguishes two occupations that
        // share a site, an epoch, a fate and a size (measured: 6 such records
        // in seed 42, 4 in seed 7, 0 in seed 1000, separable by nothing else).
        a.core
            .founded
            .total_cmp(&b.core.founded)
            .then(match (a.core.ended, b.core.ended) {
                (None, None) => std::cmp::Ordering::Equal,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (Some(_), None) => std::cmp::Ordering::Less,
                (Some(x), Some(y)) => x.total_cmp(&y),
            })
            .then(b.core.peak_population.cmp(&a.core.peak_population))
            .then(founding_key(&a.founded_from).cmp(&founding_key(&b.founded_from)))
```

with a shared helper beside it in each file:

```rust
/// A total, deterministic ordering key for a founding. Genesis foundings sort
/// before descended ones; within each, by the cell or the predecessor.
fn founding_key(f: &Founding<EntityId>) -> (u8, u64) {
    match f {
        Founding::Genesis(c) => (0, u64::from(c.0)),
        Founding::From(e) => (1, e.get()),
    }
}
```

**`None` sorts last** — a living occupation is the top layer, not the bottom. Get this backwards and the palimpsest inverts for every site with a survivor.

Apply to `occupations_at`, `occupations_by_cell`, and the almanac's `layers_at`. The almanac's version compares `a.record.core.founded` against `b.record.core.founded` and uses `a.record.founded_from` — adapt the field path, not the logic. Update all three doc comments, which currently describe the mint-order tie-break.

- [ ] **Step 4: Run the test**

Run: `cargo test -q -p hornvale-worldgen --test history_emit same_day_layers`
Expected: PASS.

- [ ] **Step 5: Assert the comparator is total (V3)**

Add to `windows/worldgen/tests/history_emit.rs`:

```rust
#[test]
fn the_layer_comparator_is_total_on_the_live_corpus() {
    // No two distinct occupations of one site may compare Equal. If they do,
    // the sort falls back to input order — the exact failure this campaign
    // removes. Measured before implementation: (founded, ended, peak) alone
    // ties on 6 records in seed 42, which `founded_from` is there to break.
    for seed in [42u64, 7, 1000] {
        let w = build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("builds");
        for (cell, occs) in occupations_by_cell(&w) {
            for i in 0..occs.len() {
                for j in (i + 1)..occs.len() {
                    assert_ne!(
                        layer_key(&occs[i]),
                        layer_key(&occs[j]),
                        "seed {seed}, cell {cell:?}: two layers compare equal"
                    );
                }
            }
        }
    }
}
```

This needs `layer_key`, and its home is **`domains/history`, not `history_emit`**
— confirmed from the manifests: `windows/almanac/Cargo.toml` depends on
`hornvale-history` and **not** on `hornvale-worldgen`, which is exactly why the
decoder was duplicated in the first place. But both crates depend on
`domains/history`, so the *comparator* need not be duplicated at all.

Move Step 3's comparator into `domains/history/src/record.rs` beside
`OccupationRecord`:

```rust
/// The order a site's layers stack in: material facts only, oldest-founded
/// first, and total.
///
/// Lives here rather than beside either caller because `windows/worldgen` and
/// `windows/almanac` both need it and neither depends on the other — the same
/// reason their decoders are duplicated. The decoders still are; this is one
/// less thing that has to be kept in lockstep by hand.
///
/// A layer that closed earlier lies deeper, which is what a stratigraphy is; a
/// still-living occupation is the top layer, so `None` sorts LAST; peak breaks
/// the remainder. `founded_from` closes the final ties — ancestry is genuinely
/// what distinguishes two occupations sharing a site, an epoch, a fate and a
/// size (measured: 6 such records in seed 42, 4 in seed 7, 0 in seed 1000,
/// separable by nothing else).
/// type-audit: bare-ok(count: return)
pub fn layer_key(r: &OccupationRecord) -> (u64, u8, u64, std::cmp::Reverse<u32>, u8, u64) {
    let founded = r.core.founded.to_bits();
    let (ended_rank, ended) = match r.core.ended {
        Some(d) => (0u8, d.to_bits()),
        None => (1u8, 0),
    };
    let (from_rank, from) = match r.founded_from {
        Founding::Genesis(c) => (0u8, u64::from(c.0)),
        Founding::From(e) => (1u8, e.get()),
    };
    (
        founded,
        ended_rank,
        ended,
        std::cmp::Reverse(r.core.peak_population),
        from_rank,
        from,
    )
}
```

`to_bits` is a total, deterministic ordering for these days: they come back from
the ledger already quantised, are never `NaN`, and are never negative here — so
the bit pattern orders the same way the value does. Using it lets the key be a
plain `Ord` tuple rather than a hand-written comparator, which is what makes the
totality test in this step expressible.

All three call sites become `v.sort_by_key(layer_key)` — or
`sort_by(|a, b| layer_key(a).cmp(&layer_key(b)))` if borrow rules complain. Step 3
above described three copies of an inline comparator; **this supersedes it**: write
the comparator once, here, and have all three sort by it.

- [ ] **Step 6: Run it**

Run: `cargo test -q -p hornvale-worldgen --test history_emit the_layer_comparator_is_total`
Expected: PASS. A failure names the seed and cell — investigate that pair rather than adding a key blindly.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -F <a message file>
```

---

### Task 3: Measure, regenerate, gate

**Files:**
- Modify: `book/src/gallery/vestige-seed-42.png`, `vestige-seed-42.md`, `almanac-seed-42*.md`, `history-seed-42.md` (regenerated)

- [ ] **Step 1: Report M1, both paths**

Write a throwaway probe (delete it before committing) that, for seeds 42, 7 and 1000, computes each multi-occupation site's layer order twice — once by `(founded, id)`, the old rule, and once by the new comparator — and reports the fraction of sites whose order differs. Do it for **both** `occupations_by_cell` and the almanac's `layers_at`.

Expected, measured from the spec's definition before implementation:

| seed | multi-occupation sites | order changes |
|---|---|---|
| 42 | 299 | 19 (6.4%) |
| 7 | 341 | 6 (1.8%) |
| 1000 | 302 | 13 (4.3%) |

**Reproduce these.** A materially different number means the comparator built is not the comparator specced. The two paths should agree with each other; if they do not, the two decoders were already out of lockstep before this campaign, which is a finding worth its own report.

- [ ] **Step 2: Regenerate artifacts**

```bash
make rebaseline
git status --short book/src/gallery docs/audits cli/tests/fixtures
```

Expect `vestige-seed-42.png`, `vestige-seed-42.md`, and the almanac/history gallery pages to move. **`cli/tests/fixtures/world-seed-42.json` must NOT move** — if it does, stop and report.

- [ ] **Step 3: Read the prose diff**

```bash
git diff book/src/gallery/history-seed-42.md | head -80
```

Read it as a reader. Layer orderings should change at a minority of sites, and where they do, the earlier-closing layer should now appear first. If the prose reads worse, say so — the point of a material order is that it means something.

- [ ] **Step 4: Full gate**

```bash
bash scripts/census-run.sh status
make gate
```

Pass an explicit Bash `timeout: 3600000`. Check for a competing heavy job first and wait rather than contending.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -F <a message file>
```

- [ ] **Step 6: Close the campaign**

Chronicle entry at `book/src/chronicle/the-scaffold.md`, added to `book/src/SUMMARY.md` in close order. A retrospective at `docs/retrospectives/the-scaffold.md`. Promote every followup from `.superpowers/sdd/` — that directory is git-ignored and dies with the worktree. Re-score any Confidence Gradient bet this moves, or say explicitly that none moved. **Then STOP: G6 is a hard stop for Nathan.**

---

## Self-Review Notes

**Spec coverage.** D1 → Task 1 Step 5. D2 (type changes, derivation does not) → `BakeId` stays counter-derived; no task alters `Bake::mint`'s arithmetic. D3 → Task 1 Steps 1–3. D4 → Task 2 Step 3. D5 → Task 1 Step 11 and Task 3 Step 2. D6's corrected surface → Task 1 Step 6 and Task 2 Step 3 both cover the almanac. V1 → Task 1 Step 11. V2 → Task 1 Step 9. V3 → Task 2 Step 5. M1 → Task 3 Step 1.

**The one thing my self-review could not settle, since settled.** Task 2 Step 5
originally branched on whether `windows/almanac` depends on `hornvale-worldgen`.
It does not — its manifest lists `hornvale-history` and no worldgen — which is why
the decoder is duplicated. But that same fact makes the *comparator* shareable:
both crates depend on `domains/history`, so `layer_key` lives there and all three
call sites use one implementation. The branch is gone, and the campaign removes a
hand-maintained duplication it was originally going to preserve.

**Type consistency.** `Occupation`, `OccupationRecord { core, id, founded_from, ended_by }`, `BakeId`, `BakeOccupation`, `Founding<I>`, `Ended<I>`, `founding_key`, `layer_key` are used identically across all three tasks.
