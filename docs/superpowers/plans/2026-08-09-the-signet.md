# The Signet Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive `EntityId` from an entity's lineage instead of its position in mint order, so adding entities never renumbers unrelated ones.

**Architecture:** An id becomes `(48-bit path hash << 16) | 16-bit sibling ordinal`, where the path hash is `Seed::derive` over the parent id and the role label. `Ledger::next_entity` survives as an accession count but stops being identity. The vessel's printed `[537]` becomes a session-local display handle, since a wide id is unusable as the typed input it currently is.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only (decision 0004). Kernel primitives only — no new hash, no `uuid` crate.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` **only**. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml` `disallowed-types`).
- No wall-clock time. No `std::time::Instant`, including in test code.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
- `cargo fmt` as the final step before every commit.
- **`cli/tests/id_shift_invariance.rs` is the keystone and must NOT be edited by any task in this plan.** If it goes red, the derivation is wrong. Its anti-vacuity assert is the guard that the new derivation has not hollowed it out.

---

### Task 1: The derivation — a pure function and its `Lineage` input

**Files:**
- Modify: `kernel/src/ledger.rs` (add `Lineage`, `derive_entity_id`, the root const)
- Modify: `kernel/src/streams.rs` (declare the role-derivation label)
- Test: `kernel/src/ledger.rs` (in-module `#[cfg(test)]`)

**Interfaces:**
- Consumes: `Seed`, `StreamLabel` from `kernel/src/seed.rs`; `EntityId` from `kernel/src/ledger.rs`.
- Produces: `pub struct Lineage<'a> { parent: Option<EntityId>, role: &'a str, ordinal: u16 }` and `pub fn derive_entity_id(lineage: Lineage<'_>) -> EntityId`. Tasks 2 and 3 both call these by exactly these names.

- [ ] **Step 1: Write the failing tests**

Add to `kernel/src/ledger.rs`'s test module:

```rust
#[test]
fn an_id_is_a_function_of_lineage_not_of_call_order() {
    let parent = EntityId::new(7).expect("nonzero");
    let a = derive_entity_id(Lineage { parent: Some(parent), role: "occupation", ordinal: 0 });
    let b = derive_entity_id(Lineage { parent: Some(parent), role: "occupation", ordinal: 0 });
    assert_eq!(a, b, "the same lineage must always yield the same id");
}

#[test]
fn siblings_differ_only_in_the_low_sixteen_bits() {
    let parent = EntityId::new(7).expect("nonzero");
    let a = derive_entity_id(Lineage { parent: Some(parent), role: "occupation", ordinal: 0 });
    let b = derive_entity_id(Lineage { parent: Some(parent), role: "occupation", ordinal: 1 });
    assert_ne!(a, b, "distinct siblings must not collide");
    assert_eq!(
        a.get() >> 16,
        b.get() >> 16,
        "siblings share their path hash, so a lineage is legible in a hex dump"
    );
    assert_eq!(a.get() & 0xFFFF, 0);
    assert_eq!(b.get() & 0xFFFF, 1);
}

#[test]
fn a_different_parent_or_role_moves_the_path_hash() {
    let p7 = EntityId::new(7).expect("nonzero");
    let p8 = EntityId::new(8).expect("nonzero");
    let base = derive_entity_id(Lineage { parent: Some(p7), role: "occupation", ordinal: 0 });
    let other_parent = derive_entity_id(Lineage { parent: Some(p8), role: "occupation", ordinal: 0 });
    let other_role = derive_entity_id(Lineage { parent: Some(p7), role: "person", ordinal: 0 });
    assert_ne!(base.get() >> 16, other_parent.get() >> 16);
    assert_ne!(base.get() >> 16, other_role.get() >> 16);
}

#[test]
fn a_root_needs_no_parent_and_no_world_seed() {
    let a = derive_entity_id(Lineage { parent: None, role: "star", ordinal: 0 });
    let b = derive_entity_id(Lineage { parent: None, role: "star", ordinal: 0 });
    assert_eq!(a, b);
    let other = derive_entity_id(Lineage { parent: None, role: "plate", ordinal: 0 });
    assert_ne!(a.get() >> 16, other.get() >> 16);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib ledger::tests 2>&1 | tail -20`
Expected: FAIL to compile — `cannot find function derive_entity_id` / `cannot find struct Lineage`.

Note: a compile failure is an acceptable RED **here only** because the type under test does not exist yet and these tests assert on a pure function with no live surface to probe first.

- [ ] **Step 3: Declare the derivation label**

In `kernel/src/streams.rs`, add to the existing label declarations:

```rust
    /// The leg an entity's identity derives on. Save-format-contract stable:
    /// changing it silently renumbers every entity in every saved world.
    entity_identity => "entity/identity/v1",
```

Match the surrounding macro's exact syntax — read the neighbouring entries before editing rather than assuming the arm shape.

- [ ] **Step 4: Implement the derivation**

In `kernel/src/ledger.rs`:

```rust
/// Where an entity comes from — the whole input to its derived identity.
/// An id is a function of this and nothing else; deliberately NOT of any
/// material fact, so two materially identical entities still differ.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lineage<'a> {
    /// The parent entity, or `None` for a root.
    pub parent: Option<EntityId>,
    /// The role this entity fills for its parent. A save-format contract:
    /// changing a role label renumbers that whole lineage.
    /// type-audit: bare-ok(identifier-text)
    pub role: &'a str,
    /// Which sibling this is among that parent's children in that role.
    /// type-audit: bare-ok(count)
    pub ordinal: u16,
}

/// The fixed root every parentless entity derives from. Not the world seed:
/// ids are a pure function of structure, exactly as today's 1, 2, 3... are.
const ENTITY_ROOT: u64 = 0x5369_676E_6574_0001;

/// Derive an entity's identity from its lineage: a 48-bit path hash over
/// (parent, role) in the high bits, the sibling ordinal in the low 16.
/// Siblings therefore share their high bits, which makes a lineage legible
/// in a hex dump.
/// type-audit: bare-ok(constructor-edge: return)
pub fn derive_entity_id(lineage: Lineage<'_>) -> EntityId {
    let base = crate::seed::Seed(lineage.parent.map_or(ENTITY_ROOT, EntityId::get));
    let hashed = base
        .derive(crate::seed::StreamLabel::dynamic(lineage.role))
        .0;
    let raw = ((hashed >> 16) << 16) | u64::from(lineage.ordinal);
    // `raw` is zero only when the top 48 bits AND the ordinal are all zero
    // (p = 2^-48). Map that one case to 1 rather than panicking: 1 is a
    // legal id and the collision assert in Task 2 catches any clash it causes.
    EntityId::new(raw).unwrap_or(EntityId::MIN)
}
```

Verify `EntityId::MIN` exists (`fact_index.rs:23` references it); if it does not, use `EntityId::new(1).expect("1 is nonzero")`.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib ledger::tests 2>&1 | tail -20`
Expected: PASS, 4 new tests.

- [ ] **Step 6: Regenerate the stream manifest**

A new label in `streams.rs` reaches the generated manifest through `stream_labels()`, so the committed artifact drifts:

```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git diff --stat book/src/reference/
```

Expected: exactly one added row, `entity/identity/v1`. If the diff is empty, the label was not wired into the macro — go back to Step 3.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add kernel/src/ledger.rs kernel/src/streams.rs book/src/reference/stream-manifest-generated.md
git commit -m "feat(the-signet): an id derives from lineage, not from mint order"
```

---

### Task 2: Minting takes a lineage, and a collision fails loudly

**Files:**
- Modify: `kernel/src/ledger.rs:145-150` (`mint_entity`), `:357-359` (`minting_is_valid`), `:371-391` (`mint_instance`)
- Test: `kernel/src/ledger.rs` in-module tests

**Interfaces:**
- Consumes: `Lineage`, `derive_entity_id` from Task 1.
- Produces: `mint_entity(&mut self, lineage: Lineage<'_>) -> EntityId` and `mint_instance(&mut self, lineage: Lineage<'_>, kind_label: &str, day: Option<f64>, provenance: &str, registry: &ConceptRegistry) -> Result<EntityId, LedgerError>`. **`lineage` is the FIRST parameter in both.** Task 3 updates every caller to this shape.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn minting_the_same_lineage_twice_is_a_hard_error() {
    let mut l = Ledger::default();
    let lin = Lineage { parent: None, role: "star", ordinal: 0 };
    let _first = l.mint_entity(lin);
    let again = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| l.mint_entity(lin)));
    assert!(
        again.is_err(),
        "minting one lineage twice must panic rather than hand out a duplicate identity"
    );
}

#[test]
fn an_inserted_mint_does_not_move_an_unrelated_id() {
    // The campaign's whole point, at kernel scale.
    let mut before = Ledger::default();
    let a_before = before.mint_entity(Lineage { parent: None, role: "star", ordinal: 0 });

    let mut after = Ledger::default();
    let _inserted = after.mint_entity(Lineage { parent: None, role: "interloper", ordinal: 0 });
    let a_after = after.mint_entity(Lineage { parent: None, role: "star", ordinal: 0 });

    assert_eq!(
        a_before, a_after,
        "minting an unrelated entity first must not move the star's id"
    );
}

#[test]
fn the_accession_count_still_counts() {
    let mut l = Ledger::default();
    l.mint_entity(Lineage { parent: None, role: "star", ordinal: 0 });
    l.mint_entity(Lineage { parent: None, role: "plate", ordinal: 0 });
    assert_eq!(l.entity_count(), 2, "next_entity survives as an accession count");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel --lib ledger::tests 2>&1 | tail -20`
Expected: FAIL — `mint_entity` takes 0 arguments; `entity_count` not found.

- [ ] **Step 3: Implement**

Replace `mint_entity` (currently `kernel/src/ledger.rs:145-150`):

```rust
    /// Mint an entity whose identity derives from `lineage`.
    ///
    /// # Panics
    ///
    /// If `lineage` derives an id already minted in this ledger. That means
    /// either the same lineage was minted twice — a caller bug — or a
    /// 48-bit path-hash collision (p ~ 1.8e-9 per world at this entity
    /// population). Failing loudly turns silent identity corruption into a
    /// reproducible panic; never suppress this.
    pub fn mint_entity(&mut self, lineage: Lineage<'_>) -> EntityId {
        let id = derive_entity_id(lineage);
        assert!(
            self.minted.insert(id),
            "entity id {:#x} already minted — lineage (parent {:?}, role {:?}, \
             ordinal {}) collides. Same lineage minted twice, or a path-hash \
             collision. Do not suppress: widen the lineage instead.",
            id.get(),
            lineage.parent,
            lineage.role,
            lineage.ordinal
        );
        self.next_entity += 1;
        id
    }

    /// How many entities this ledger has minted — an accession count, never
    /// an identity. Ids are derived (see [`derive_entity_id`]); this only
    /// answers "how many".
    /// type-audit: bare-ok(count: return)
    pub fn entity_count(&self) -> u64 {
        self.next_entity
    }
```

Add the `minted` field to `Ledger` beside `next_entity`:

```rust
    /// Every id minted so far — the collision guard's memory. Rebuilt from
    /// the facts on load, so it is `#[serde(skip)]` and never widens the
    /// save format.
    #[serde(skip)]
    minted: BTreeSet<EntityId>,
```

Replace `minting_is_valid` (currently `:357-359`) — the old `next_entity >= max_entity_id()` is meaningless once ids are derived:

```rust
    /// Valid when every id referenced in a fact is one this ledger minted.
    /// Replaces the old counter-vs-maximum check, which assumed ids were
    /// positions in a sequence.
    /// type-audit: bare-ok(flag)
    pub fn minting_is_valid(&self) -> bool {
        self.facts.iter().all(|f| self.minted.contains(&f.subject))
    }
```

`minted` must be repopulated wherever the index is rebuilt after load — find `ensure_index` (`kernel/src/ledger.rs:152`) and populate `minted` from `facts` in the same place, so a loaded world can still mint without false collisions.

Then thread `lineage` into `mint_instance` as its **first** parameter, passing it straight to `mint_entity`.

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-kernel --lib 2>&1 | tail -20`
Expected: the 3 new tests PASS. Other in-file tests still fail to compile — Task 3 fixes callers.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add kernel/src/ledger.rs
git commit -m "feat(the-signet): minting takes a lineage, and a collision panics"
```

---

### Task 3: Thread lineage through every minting call site

**Files:**
- Modify: `kernel/src/ledger.rs` (its own ~23 test call sites), `kernel/src/refine.rs`, `kernel/src/world.rs`, `kernel/src/schedule.rs`, `kernel/examples/first_light.rs`
- Modify: `domains/astronomy/src/facts.rs`, `domains/paleoclimate/src/facts.rs`, `domains/settlement/src/genesis.rs`, `domains/species/src/lib.rs`, `domains/religion/src/lib.rs`, `domains/terrain/src/facts.rs`, `domains/culture/src/lib.rs`
- Modify: `windows/worldgen/src/lib.rs`, `windows/worldgen/src/history_emit.rs`, `windows/worldgen/src/disposition.rs`, `windows/explain/src/lib.rs`
- Modify: the test files that mint (`domains/species/tests/instance_lens.rs`, `windows/worldgen/tests/{descent_graph,tolerance_draw,doctrine,species_worlds}.rs`)

**Interfaces:**
- Consumes: `Lineage`, `mint_entity`, `mint_instance` from Task 2.
- Produces: no new API. Every production mint passes a real lineage.

- [ ] **Step 1: Enumerate the true call-site list from the compiler, not from grep**

```bash
cargo check --workspace --all-targets 2>&1 | tee /tmp/signet-sites.txt | grep -c "^error"
grep -E "^error" -A 3 /tmp/signet-sites.txt | grep "\-\->" | sort -u
```

The compiler is the enumeration. A grep-derived list of call sites has been incomplete on this repo more than once — work the compiler's list to zero.

- [ ] **Step 2: Choose each lineage from what the code already knows**

Rules, in order:

1. **The parent is the entity this one belongs to.** An occupation's parent is its settlement; a person's parent is the occupation they founded; a moon's parent is its planet.
2. **The role is the concept name already in use**, matching the `instance-of` kind label or the predicate family (`"occupation"`, `"person"`, `"moon"`). Reuse an existing label rather than minting a new vocabulary.
3. **The ordinal is the loop index** where the caller already loops (`for (i, x) in xs.iter().enumerate()` → `ordinal: i as u16`), and `0` where exactly one child of that role exists per parent.
4. **A root** (`parent: None`) is for entities that genuinely belong to the world rather than to another entity: the star, the plates, the top-level kinds.

**Do not invent a lineage to make a call site compile.** If the right parent is not in scope, stop and record it in `.superpowers/sdd/followups.md` rather than passing `None` to move on — a wrong parent silently mis-keys every fact about that entity, which is the exact defect class The Scaffold existed to remove.

- [ ] **Step 3: Convert test call sites with a helper**

Most of the ~190 call sites are tests that do not care about lineage. Add one helper to the kernel's test module and use it everywhere a test just needs a distinct entity:

```rust
#[cfg(test)]
/// A distinct throwaway lineage for tests that only need "some entity".
pub fn test_lineage(n: u16) -> Lineage<'static> {
    Lineage { parent: None, role: "test", ordinal: n }
}
```

Each call in a given test needs a **different** `n`, or the Task 2 collision assert fires — which is the guard working, not a problem to suppress.

- [ ] **Step 4: Run the full check to zero**

Run: `cargo check --workspace --all-targets 2>&1 | grep -c "^error"`
Expected: `0`.

- [ ] **Step 5: Run the suite and expect fixture reds, not logic reds**

Run: `HV_TEST_OK=1 cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/signet-t3.txt | grep -E "^\s+FAIL|Summary"`

Expected: every failure is a **committed-fixture byte mismatch** (world JSON, session/snapshot fixtures, gallery prose). Task 6 re-pins those, as the epoch. **Any failure that is not a fixture mismatch is a logic error in this task — fix it here, do not carry it forward.** In particular `cli/tests/id_shift_invariance.rs` must be GREEN: it reads no fixture.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor(the-signet): every mint passes a real lineage"
```

---

### Task 4: The vessel's display handle

**Files:**
- Modify: `windows/vessel/src/session.rs:3151` (the print), `:1104` and `:3170` and `:3289` (the parsers)
- Test: `windows/vessel/tests/` — add `display_handle.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: no public API change; the `npcs` listing prints a 1-based position instead of a raw id.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_npc_listing_prints_a_short_handle_not_a_raw_entity_id() {
    let mut session = /* build the seed-42 session the other vessel tests use */;
    let listing = session.handle("npcs");
    assert!(
        listing.contains("[1]"),
        "the listing must offer a short, typeable handle: {listing}"
    );
    for line in listing.lines().filter(|l| l.contains('[')) {
        let n: u64 = /* parse between '[' and ']' */;
        assert!(
            n < 1000,
            "a printed handle must be typeable, not a derived entity id: {line}"
        );
    }
}

#[test]
fn a_short_handle_addresses_the_same_npc_the_label_does() {
    let mut session = /* same fixture */;
    let by_handle = session.handle("why 1");
    let first_label = /* first label from `npcs` */;
    let by_label = session.handle(&format!("why {first_label}"));
    assert_eq!(by_handle, by_label, "handle and label must address one NPC");
}
```

Read `windows/vessel/tests/` for the existing session-construction helper and reuse it verbatim rather than writing a new one.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test display_handle 2>&1 | tail -20`
Expected: FAIL — the listing prints the full derived id.

- [ ] **Step 3: Implement**

At `session.rs:3151`, print the 1-based position in the listing:

```rust
            lines.push(format!("  [{}] {}", i + 1, npc.label));
```

(where `i` is the enumeration index over `self.npcs` — add `.enumerate()` if absent).

At each of the three parse sites (`:1104`, `:3170`, `:3289`), resolve a numeric input as a **1-based position into `self.npcs`**, not as an entity id:

```rust
        who.parse::<usize>()
            .ok()
            .filter(|n| *n >= 1)
            .and_then(|n| self.npcs.get(n - 1))
```

keeping the existing label-substring fallback exactly as it is. Update the prompt text at `:3167` — it already says "label or id"; make it "label or number".

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -20`
Expected: the 2 new tests PASS. Fixture-backed vessel tests may still be red pending Task 6.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/
git commit -m "feat(the-signet): the vessel prints a typeable handle, not an identity"
```

---

### Task 5: String-encode ids where they cross to JavaScript

**Files:**
- Modify: whichever of `windows/scene/src/*.rs` and `windows/vessel/src/snapshot.rs` serialize an `EntityId` into emitted JSON
- Test: `windows/vessel/tests/` or `windows/scene/tests/` — a round-trip test

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: no Rust API change; the emitted JSON carries ids as strings.

- [ ] **Step 1: Find every emitted id, from the artifact rather than from the source**

```bash
grep -oE '"[a-z_]+":[0-9]{6,}' book/src/gallery/*.json clients/game/core/tests/fixtures/*.json \
  windows/vessel/tests/fixtures/*.json 2>/dev/null | sort -u
```

Every bare number wider than 2^53 in an emitted artifact is a precision bug in a JS client. Copy the encoding `AgentId` already uses — `"agent":"9947299063136102849"` in `vessel/session/v1` is the working precedent in this exact schema. Do not invent a second convention.

- [ ] **Step 2: Write the failing test**

```rust
#[test]
fn an_emitted_entity_id_survives_a_javascript_number() {
    let json = /* emit the seed-42 session snapshot */;
    for cap in /* every "entity": <number> occurrence */ {
        panic!("entity id emitted as a bare number, which loses precision above 2^53: {cap}");
    }
    assert!(json.contains(r#""entity":""#), "ids must be string-encoded");
}
```

- [ ] **Step 3: Run to verify it fails**

Expected: FAIL — ids are emitted as bare numbers.

- [ ] **Step 4: Implement**

Add `#[serde(with = ...)]` string encoding on the `EntityId`-typed fields of the emitted structs, matching how `AgentId` is already handled. **These are `scene/*` and `vessel/session/v1` schemas — cross-repo contracts under decision 0055.** This is a breaking wire change, so bump the schema version rather than mutating it in place, and note the bump in the commit message.

- [ ] **Step 5: Run to verify it passes, then check the external client**

```bash
cargo test -p hornvale-vessel -p hornvale-scene 2>&1 | tail -20
make vessel-check
make world-check
```

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(the-signet): ids cross to JavaScript as strings, not as lossy numbers"
```

---

### Task 6: The epoch — re-pin every artifact, and guard the property

**Files:**
- Modify: every committed world/session/snapshot fixture and generated artifact
- Create: `cli/tests/id_stability_under_insertion.rs`

**Interfaces:**
- Consumes: everything above.
- Produces: the campaign's acceptance test.

- [ ] **Step 1: Write the acceptance test (spec P1)**

```rust
//! The campaign's acceptance test: inserting a minting stage must not move
//! any id outside that stage's lineage. Before The Signet this failed by
//! construction on six committed fixtures.

#[test]
fn inserting_a_minting_stage_moves_no_unrelated_id() {
    let plain = /* build seed 42 to Full depth */;
    let with_extra = /* build seed 42 to Full depth, minting one extra
                        root-lineage entity before the settlement stage */;

    let plain_ids: BTreeSet<u64> = /* every entity id in `plain` */;
    let extra_ids: BTreeSet<u64> = /* every entity id in `with_extra` */;

    let missing: Vec<_> = plain_ids.difference(&extra_ids).collect();
    assert!(
        missing.is_empty(),
        "inserting a stage moved {} pre-existing ids — the positional identity \
         this campaign removed has come back: {:?}",
        missing.len(),
        missing.iter().take(10).collect::<Vec<_>>()
    );
}
```

- [ ] **Step 2: Run it and require RED before the re-pin**

Run: `cargo test -p hornvale --test id_stability_under_insertion 2>&1 | tail -20`

This test must be seen **GREEN on the new derivation**. To prove it is not vacuous, temporarily revert `derive_entity_id` to `EntityId::new(self.next_entity)` and confirm the test goes RED, then restore. A test that has never failed proves nothing; record both outcomes in the commit message.

- [ ] **Step 3: Regenerate everything**

```bash
make rebaseline
make rebaseline-goldens
```

- [ ] **Step 4: Read the prose diff as prose (spec P3)**

```bash
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ \
  docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
git diff book/src/gallery/possession-seed-42.md
```

The Salt's handoff predicts the prose files that move are **exactly** `possession-seed-42.md` and `possession-over-time-seed-42.md`. **Anything else that moves is a channel The Salt missed** — record it in `.superpowers/sdd/followups.md` as a finding. Do not retune anything to make the prediction come true.

- [ ] **Step 5: Verify the six fixtures now hold still**

Re-run the probe that motivated the campaign — after re-pinning, a *second* insertion must move nothing:

```bash
for f in windows/vessel/tests/fixtures/session-seed-42.json \
         clients/game/core/tests/fixtures/session-seed-42-turn-0.json; do
  python3 - "$f" <<'PY'
import re, sys
s = open(sys.argv[1]).read()
print(sys.argv[1], sorted(set(re.findall(r'"entity":"?(\d+)"?', s)))[:6])
PY
done
```

- [ ] **Step 6: Full gate, including what the gate cannot see**

```bash
make gate
make game-check
make vessel-check
```

Pass an explicit Bash `timeout: 3600000`. `clients/` is outside the cargo workspace, so `make gate` alone is not sufficient evidence.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "epoch(the-signet): re-pin every world-derived artifact, once"
```

---

### Task 7: Score the predictions and close

**Files:**
- Create: `book/src/chronicle/the-signet.md`, `docs/retrospectives/the-signet.md`
- Modify: `book/src/SUMMARY.md`, `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Score P1–P4 from spec §6**

P1 is the acceptance test in Task 6. P2 is `id_shift_invariance.rs` still reporting ≥1 colliding material group — report the actual count, expected unchanged at 3 material / 29 founding-key. P3 is the prose-diff finding from Task 6 Step 4. P4 is the honest limit: state plainly that within-lineage reordering still churns.

- [ ] **Step 2: Write the chronicle**

`book/src/chronicle/the-signet.md`, added to `book/src/SUMMARY.md` in close order. Cover: why a position is not an identity; the archival accession/call-number distinction; why the derivation reads no material fact; the six-fixture measurement before and after. **No process content** (decision 0020). Book titles are code-generated — check before hand-writing one.

- [ ] **Step 3: Write the retrospective**

`docs/retrospectives/the-signet.md`. Promote **every** item from `.superpowers/sdd/followups.md` and the decision ledger — both are git-ignored and die with the worktree.

- [ ] **Step 4: Flip registry rows**

`SIG-agentid-entityid-confusion` → shipped (closed by Task 4). `SIG-layer-key-ties-rest-on-a-stable-sort` → re-check and update. Add a row for **self-describing ids** (domain tag / epoch / schema version in spare bits) — the deferred case for 128-bit ids, spec §8. Measure every Idea cell against the 600-character cap; do not estimate.

- [ ] **Step 5: Book freshness sweep**

Re-score any Confidence Gradient bet in `book/src/open-questions.md` this campaign moves (decision 0030). If none moves, say so explicitly rather than skipping silently — both The Scaffold and The Salt checked and recorded "none".

- [ ] **Step 6: Full gate, then STOP**

```bash
make gate && make game-check && make vessel-check
```

**G6 is a hard stop.** Do not merge, do not push, do not delete the worktree. Present the post-G3 ledger digest and wait for Nathan.

---

## Self-Review Notes

**Spec coverage.** §2 derivation → Task 1. §2 "why not material facts" → Task 1 tests + the Global Constraint forbidding edits to `id_shift_invariance.rs`. §3 two fields / collision assert → Task 2. §4 display handle → Task 4. §5 epoch → Task 6; §5 JSON strings → Task 5. §6 P1 → Task 6 Step 1; P2 → Task 7 Step 1; P3 → Task 6 Step 4; P4 → Task 7 Step 1. §7 testing → Tasks 1–6. §9 DoD → Task 7.

**One spec revision made while planning**, recorded here rather than silently: §2's root rule originally derived from the world seed, which would have forced a `Seed` through every mint path. Dropped, because today's ids are *already* world-independent (every world numbers from 1), so a structural-only derivation preserves that property rather than changing it. The spec was updated before this plan was written.

**Two places I could not fully close from the armchair**, flagged in place rather than papered over:

1. **Task 3's lineage choices are the campaign's real risk.** The mechanical part (adding a parameter) is trivial; choosing the *right* parent for ~17 production call sites is judgment, and a wrong parent compiles cleanly while silently mis-keying every fact about that entity. That is why Step 2 carries an explicit "do not invent a lineage to make it compile" rule with a followup escape hatch.
2. **Task 4 and 5's test scaffolding is described, not written.** Both need the existing session-construction helper from `windows/vessel/tests/`, which I did not read. The implementer must read it and reuse it verbatim rather than writing a parallel one — a second world-building helper in the test suite is how fixtures drift apart.
