# The Particular Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale individual persons that can be the subject of a fact — promoting the founders of remembered occupations into ledger entities.

**Architecture:** A new `domains/person` crate owns four predicates and a thin `genesis` over already-resolved plain data. All cross-domain reading (occupation records, species lifespans) happens in `windows/worldgen`, the composition root, because a domain crate may depend on `hornvale-kernel` and nothing else; the handle derivation itself lands in `domains/history` beside `persona_of`, so promotion and rendering cannot disagree about who a founder is. Promotion runs as the **last** stage of `build_to`, so no existing `EntityId` shifts.

**Tech Stack:** Rust edition 2024, `hornvale-kernel` only in the domain; `serde`/`serde_json`/`libm` are the workspace's entire external allowlist.

## Global Constraints

- **A domain crate depends on `hornvale-kernel` and NOTHING else.** `cli/tests/architecture.rs:110-131` asserts `pkg.normal_deps == ["hornvale-kernel"]` exactly. `domains/person` may **not** import `hornvale-history` or `hornvale-species`.
- **Exactly one new draw** (spec D5): a founder's name, via a new `NameKind::Person` whose derive path is disjoint from every existing one, so no existing name moves. Nothing else in the promotion path draws — `persona_of` and `life_history` are both pure. A new stream label is declared; no epoch is owed.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Enforced by `clippy.toml`.
- No wall-clock time. No `Instant::now`, no `SystemTime`.
- `#![warn(missing_docs)]` in the crate root; every public item, field and variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag. The audit is **default-deny** — an untagged primitive fails the gate.
- `Fact.day` stays `Option<f64>`, never `WorldTime` (decision 0014).
- **A name is never salted from an `EntityId` or from mint order** (decision 0051).
- Promotion runs **last** in `build_to` (spec D3).
- `cargo fmt` as the final step before every commit.

---

## File Structure

| File | Responsibility |
|---|---|
| `domains/person/Cargo.toml` | Create. Kernel-only dependency. |
| `domains/person/src/lib.rs` | Create. Four predicates, `PersonSeed`, `genesis`, `Domain` impl. |
| `domains/history/src/flesh.rs` | Modify. `founder_handle`, public, beside `persona_of` — the derivation both worldgen and the almanac must agree on. |
| `domains/language/src/naming.rs` | Modify. A fourth `NameKind` (`Person`) and its label — additive, disjoint derive path. |
| `domains/history/tests/flesh.rs` | Modify. The handle's mint-order-independence test. |
| `windows/worldgen/src/person_promote.rs` | Create. Per-people selection, the cast-uniqueness assertion, and `promote`. Pure but for the ledger write. |
| `windows/worldgen/src/lib.rs` | Modify. `DOMAINS` roster, the roster-count assertion, `mod person_promote`, the `"person"` stage. |
| `windows/worldgen/Cargo.toml` | Modify. Add `hornvale-person`. |
| `windows/almanac/src/history.rs` | Modify. `founding_sentence` gains a founder clause. |
| `windows/almanac/Cargo.toml` | Modify. Add `hornvale-person`. |

**Regenerated artifacts, by the task that causes the drift:**

| Artifact | Caused by | Command |
|---|---|---|
| `book/src/reference/layering-generated.md` | Task 1 (a new crate = a new row) | `REBASELINE=1 cargo test -q -p hornvale --test architecture` |
| `book/src/reference/concept-registry-generated.md` | Task 1 (four new predicates) | `make rebaseline` |
| `docs/audits/trope-coverage.md` | Task 1 (**registration alone satisfies the bundle**) | `make rebaseline` |
| `docs/audits/type-audit-report.md` | Task 1 + 2 (new pub boundaries) | `make rebaseline` |
| `cli/tests/fixtures/world-seed-42.json` | Task 3 (new facts) | `REBASELINE=1 cargo test -q -p hornvale --test lens_purity` |
| `book/src/gallery/almanac-seed-42*.md`, `history-seed-42.md` | Task 4 (new prose) | `make rebaseline` |

**Only `lens_purity` and `architecture` may move among the six byte-goldens.** If `scene/golden` or `session_snapshot` also drift, persons have leaked into a client-facing scene document — scene schemas are cross-repo contracts, additive-or-versioned only. **Stop and report; do not rebaseline.**

---

### Task 1: The `domains/person` crate and its four predicates

**Files:**
- Create: `domains/person/Cargo.toml`
- Create: `domains/person/src/lib.rs`
- Modify: `windows/worldgen/Cargo.toml`
- Modify: `windows/worldgen/src/lib.rs:286-301` (the `DOMAINS` roster) and `:10212-10220` (the roster-count assertion)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `hornvale_person::{Person, PersonSeed, genesis, IS_PERSON, PERSON_FOUNDED, PERSON_BORN, PERSON_DIED, register_concepts}`. `PersonSeed { community: EntityId, name: String, birth_day: f64, death_day: Option<f64> }`. `genesis(world: &mut World, seeds: &[PersonSeed]) -> Result<Vec<EntityId>, LedgerError>`.

- [ ] **Step 1: Create the manifest**

`domains/person/Cargo.toml`:

```toml
[package]
name = "hornvale-person"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale person domain: the individuals a people remembers."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

No workspace edit is needed — the root `Cargo.toml` has `members = ["domains/*"]`.

- [ ] **Step 2: Write the failing registration test**

Create `domains/person/src/lib.rs` containing only this test module for now, plus the crate doc and `#![warn(missing_docs)]`:

```rust
#![warn(missing_docs)]
//! The person domain: the individuals a people remembers.
//!
//! A person here is the founder of an occupation notable enough that the
//! people who founded it still remembers who did. This crate owns the
//! predicates and commits the facts; it never decides *which* founders are
//! remembered, because that decision needs occupation records and species
//! lifespans, and a domain may reach only the kernel (decision 0002).
//! `windows/worldgen` resolves those and hands over [`PersonSeed`] values.

#[cfg(test)]
mod tests {
    #[test]
    fn concepts_registered() {
        let mut r = hornvale_kernel::ConceptRegistry::default();
        crate::register_concepts(&mut r).expect("registers without conflict");
        crate::register_concepts(&mut r).expect("registration is idempotent");
        let names: Vec<&str> = r.predicates().map(|p| p.name.as_str()).collect();
        for p in [
            crate::IS_PERSON,
            crate::PERSON_FOUNDED,
            crate::PERSON_BORN,
            crate::PERSON_DIED,
        ] {
            assert!(names.contains(&p), "{p} should be registered");
        }
    }
}
```

- [ ] **Step 3: Run it and confirm it fails**

Run: `cargo test -p hornvale-person`
Expected: FAIL to compile — `register_concepts`, `IS_PERSON` etc. not found.

- [ ] **Step 4: Add the predicates and registration**

Insert above the test module:

```rust
use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Marks an entity as an individual person.
/// type-audit: bare-ok(identifier-text)
pub const IS_PERSON: &str = "is-person";
/// The community whose occupation this person founded.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_FOUNDED: &str = "person-founded";
/// The day this person was born, in absolute standard days.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_BORN: &str = "person-born";
/// The day this person died. Absent while they are still alive.
/// type-audit: bare-ok(identifier-text)
pub const PERSON_DIED: &str = "person-died";

/// Register this domain's predicates.
///
/// No concepts are registered: `person` already exists as a *lexical* concept
/// owned by `domains/language` (the autonym root), and re-registering it with a
/// different definition would be a `RegistryError::ConflictingDefinition`.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PERSON, true, "this entity is an individual person")?;
    registry.register_predicate(
        PERSON_FOUNDED,
        true,
        "the community whose occupation this person founded",
    )?;
    registry.register_predicate(PERSON_BORN, true, "the day this person was born")?;
    registry.register_predicate(PERSON_DIED, true, "the day this person died")?;
    Ok(())
}
```

All four are functional (`true`): a person has one birth, one death, one founding, and is or is not a person.

- [ ] **Step 5: Run the test to verify it passes**

Run: `cargo test -p hornvale-person`
Expected: PASS, 1 test.

- [ ] **Step 6: Write the failing genesis test**

Add to the test module:

```rust
    #[test]
    fn a_living_founder_gets_no_death_fact() {
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        crate::register_concepts(&mut world.registry).expect("registers");
        let community = world.ledger.mint_entity();
        let ids = crate::genesis(
            &mut world,
            &[
                crate::PersonSeed {
                    community,
                    name: "Grokk".to_string(),
                    birth_day: 10.0,
                    death_day: Some(60.0),
                },
                crate::PersonSeed {
                    community,
                    name: "Vashti".to_string(),
                    birth_day: 20.0,
                    death_day: None,
                },
            ],
        )
        .expect("commits");
        assert_eq!(ids.len(), 2, "one entity per seed");

        let died: Vec<&hornvale_kernel::Fact> =
            world.ledger.find(crate::PERSON_DIED).collect();
        assert_eq!(died.len(), 1, "only the dead founder carries a death fact");
        assert_eq!(died[0].subject, ids[0]);

        let born: Vec<&hornvale_kernel::Fact> =
            world.ledger.find(crate::PERSON_BORN).collect();
        assert_eq!(born.len(), 2, "every founder carries a birth fact");
    }
```

- [ ] **Step 7: Run it and confirm it fails**

Run: `cargo test -p hornvale-person a_living_founder`
Expected: FAIL — `PersonSeed` and `genesis` not found.

- [ ] **Step 8: Implement `PersonSeed` and `genesis`**

```rust
/// One resolved founder, ready to commit.
///
/// Built by the composition root, which alone can see occupation records and
/// species lifespans. Every field is a kernel type so this crate needs no
/// sibling domain.
/// type-audit: bare-ok(count: birth_day), bare-ok(count: death_day)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PersonSeed {
    /// The community whose occupation this person founded.
    pub community: EntityId,
    /// The name this person is remembered by, drawn at genesis where the
    /// language machinery lives. Committed, not derived at render time.
    pub name: String,
    /// Birth, in absolute standard days.
    pub birth_day: f64,
    /// Death, in absolute standard days. `None` means still alive at `now`.
    pub death_day: Option<f64>,
}

/// A person's day-stamped fact. `place` is the community, so a reader can find
/// a founder from the settlement.
fn fact(subject: EntityId, predicate: &str, object: Value, community: EntityId, day: f64) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(community),
        day: Some(day),
        provenance: "person".to_string(),
    }
}

/// Commit one person per seed, in the order given.
///
/// Four facts always — `is-person`, `name`, `person-founded`, `person-born` —
/// plus a fifth when the person has already died. `name` is kernel-core and
/// exempt from the single-writer check, so committing it here is not a
/// violation; several domains already do.
/// A living
/// person is represented by the *absence* of `person-died`: birth is known and
/// death may not have happened, which is the asymmetry the occupation data
/// already carries.
pub fn genesis(world: &mut World, seeds: &[PersonSeed]) -> Result<Vec<EntityId>, LedgerError> {
    let mut ids = Vec::with_capacity(seeds.len());
    for s in seeds {
        let id = world.ledger.mint_entity();
        world.ledger.commit(
            fact(id, IS_PERSON, Value::Flag(true), s.community, s.birth_day),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                hornvale_kernel::NAME,
                Value::Text(s.name.clone()),
                s.community,
                s.birth_day,
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, PERSON_FOUNDED, Value::Entity(s.community), s.community, s.birth_day),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, PERSON_BORN, Value::Number(s.birth_day), s.community, s.birth_day),
            &world.registry,
        )?;
        if let Some(d) = s.death_day {
            world.ledger.commit(
                fact(id, PERSON_DIED, Value::Number(d), s.community, d),
                &world.registry,
            )?;
        }
        ids.push(id);
    }
    Ok(ids)
}

/// The person domain, for the composition root's roster.
#[derive(Debug, Default)]
pub struct Person;

impl hornvale_kernel::Domain for Person {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
}
```

`stream_labels` is deliberately **not** overridden — this crate draws nothing, and the `Domain` trait's default returns an empty `Vec`. Publishing a phantom label would mislead a reader of the generated stream manifest.

- [ ] **Step 9: Run the tests to verify they pass**

Run: `cargo test -p hornvale-person`
Expected: PASS, 2 tests.

- [ ] **Step 10: Put the domain on the roster**

In `windows/worldgen/Cargo.toml`, add beside the other domain dependencies:

```toml
hornvale-person = { path = "../../domains/person" }
```

In `windows/worldgen/src/lib.rs`, in the `DOMAINS` roster near line 286-301, add after the `hornvale_history::History` entry:

```rust
    &hornvale_person::Person,
```

Then find `domains_roster_crate_names_are_unique_and_nonempty` near line 10212 and bump its hardcoded count:

```rust
        assert_eq!(names.len(), 11, "expected eleven domains in the roster");
```

**This bump is mandatory and easy to miss** — the assertion hardcodes the previous count, and leaving it fails a test whose message looks unrelated to this change.

- [ ] **Step 11: Verify the roster and the layering rule**

Run: `cargo test -q -p hornvale-worldgen domains_roster && cargo test -q -p hornvale --test architecture`
Expected: the roster test passes; `architecture` **fails** on the layering golden, because a new crate adds a row. `domains_depend_only_on_the_kernel` must PASS — if it fails, `domains/person` acquired a dependency it may not have.

- [ ] **Step 12: Accept the drifted artifacts**

```bash
REBASELINE=1 cargo test -q -p hornvale --test architecture
make rebaseline
git diff --stat book/src/reference/ docs/audits/
```

Expect exactly four files to move: `layering-generated.md` (a new row), `concept-registry-generated.md` (four predicates), `type-audit-report.md` (new pub boundaries), and **`trope-coverage.md`** — registering the predicates alone satisfies `bundle:individual-persons`, so the coverage report changes here, before any person exists. Read that diff: `individual-persons` should leave the Leverage table and the row count should drop 31 → 30.

- [ ] **Step 13: Commit**

```bash
cargo fmt
git add domains/person windows/worldgen/Cargo.toml windows/worldgen/src/lib.rs book/src/reference docs/audits
git commit -F <a message file>
```

Write the message to a file and use `git commit -F`; a heredoc containing backticks has previously executed as command substitution in this repo.

---

### Task 2: Handle derivation and per-people selection

**Files:**
- Modify: `domains/history/src/flesh.rs` (add `founder_handle`, public, beside `persona_of`)
- Modify: `domains/history/tests/flesh.rs` (the handle's tests)
- Create: `windows/worldgen/src/person_promote.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `mod person_promote;` beside the other module declarations)

**Why `founder_handle` lives in `domains/history`, not here.** Deriving a handle
for an occupation's founder is the missing half of the mechanism `flesh.rs`
already provides — its own doc says a handle stands for "a founder, the chieftain
who led a flight," and it supplies `persona_of` to expand one but nothing to
*make* one. Putting it there also means **both** consumers reach the same
derivation: `windows/worldgen` for promotion and `windows/almanac` for rendering
(the almanac depends on `hornvale-history`, not on worldgen). If the handle lived
in worldgen the almanac could not recompute it, and the campaign would have to
commit it as a fact — putting a derived value in the save format for no reason.

**Interfaces:**
- Consumes: `hornvale_person::PersonSeed` from Task 1.
- Produces, in `domains/history`: `pub fn founder_handle(occ: &OccupationRecord) -> RoleHandle`.
- Produces: `pub const MEMORY_DEPTH: usize = 20;` and
  `pub fn select_founders(records: &[OccupationRecord]) -> Vec<Founder>`, where
  `pub struct Founder { pub handle: RoleHandle, pub occupation: usize, pub people: KindId, pub community: EntityId, pub founded: f64 }`.
  `occupation` is the index into the `records` slice the founder came from.

- [ ] **Step 1: Write the failing selection tests**

Create `windows/worldgen/src/person_promote.rs` with the module doc and this test module:

```rust
//! Promotion: which founders a people remembers, and how their identity is
//! derived.
//!
//! This lives in the composition root rather than in `domains/person` because
//! it reads `domains/history`'s occupation records, and a domain crate may
//! reach only the kernel (decision 0002).

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{
        CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

    fn rec(people: &'static str, site: u32, founded: f64, peak: u32) -> OccupationRecord {
        let e = EntityId::new(1).expect("nonzero");
        OccupationRecord {
            people: KindId(people),
            community: e,
            lineage: e,
            site: CellId(site),
            founded,
            ended: None,
            peak_population: peak,
            tech: TechHorizon::Neolithic,
            function: Function::Agrarian,
            deity: None,
            tongue: None,
            cause: None,
            ended_by: Ended::Nature,
            founded_from: Founding::Genesis(CellId(site)),
            notability: Notability::Common,
        }
    }

    #[test]
    fn each_people_is_capped_at_memory_depth() {
        let mut records = Vec::new();
        for i in 0..(MEMORY_DEPTH as u32 + 5) {
            records.push(rec("goblin", i, f64::from(i), 100 - i));
        }
        records.push(rec("kobold", 900, 0.0, 7));
        let cast = select_founders(&records);
        let goblins = cast.iter().filter(|f| f.people.0 == "goblin").count();
        let kobolds = cast.iter().filter(|f| f.people.0 == "kobold").count();
        assert_eq!(goblins, MEMORY_DEPTH, "a populous people is capped at the depth");
        assert_eq!(kobolds, 1, "a people with one occupation gets one founder");
    }

    #[test]
    fn selection_takes_the_largest_and_is_order_independent() {
        let forward = vec![rec("goblin", 1, 0.0, 5), rec("goblin", 2, 0.0, 99)];
        let mut backward = forward.clone();
        backward.reverse();
        let a = select_founders(&forward);
        let b = select_founders(&backward);
        assert_eq!(a.len(), 2);
        assert_eq!(
            a.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            b.iter().map(|f| f.handle.0).collect::<Vec<_>>(),
            "the cast does not depend on input order"
        );
        assert_eq!(a[0].community, forward[1].community);
    }

}
```

- [ ] **Step 2: Run them and confirm they fail**

Run: `cargo test -q -p hornvale-worldgen person_promote`
Expected: FAIL to compile — `MEMORY_DEPTH`, `select_founders`, `Founder` not found.

- [ ] **Step 3: Add `founder_handle` to `domains/history`, test first**

Add to `domains/history/tests/flesh.rs`, which already exercises `persona_of`:

```rust
#[test]
fn a_founder_handle_ignores_entity_ids_and_notices_semantics() {
    use hornvale_history::flesh::founder_handle;
    use hornvale_history::record::{
        Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
    };
    use hornvale_kernel::{CellId, EntityId, KindId};

    let e = EntityId::new(1).expect("nonzero");
    let mut a = OccupationRecord {
        people: KindId("goblin"),
        community: e,
        lineage: e,
        site: CellId(4),
        founded: 25.0,
        ended: None,
        peak_population: 40,
        tech: TechHorizon::Neolithic,
        function: Function::Agrarian,
        deity: None,
        tongue: None,
        cause: None,
        ended_by: Ended::Nature,
        founded_from: Founding::Genesis(CellId(4)),
        notability: Notability::Common,
    };
    let mut b = a.clone();
    b.community = EntityId::new(9_999).expect("nonzero");
    b.lineage = b.community;
    assert_eq!(
        founder_handle(&a).0,
        founder_handle(&b).0,
        "mint order must not change a founder's identity (decision 0051)"
    );
    a.peak_population = 41;
    assert_ne!(
        founder_handle(&a).0,
        founder_handle(&b).0,
        "a semantic difference must change the handle"
    );
}
```

Run it, watch it fail to compile, then add this to `domains/history/src/flesh.rs`
beside `persona_of` — `pub`, with the doc comment verbatim:

Then run `cargo test -q -p hornvale-history flesh` (2 tests pass) and continue in
`windows/worldgen/src/person_promote.rs` with the selection, importing the handle
rather than redefining it:

```rust
use hornvale_history::flesh::{founder_handle, RoleHandle};
use hornvale_history::record::OccupationRecord;
use hornvale_kernel::{EntityId, KindId};
use std::collections::BTreeMap;

/// How many founders one people remembers.
///
/// A constant per *holder*, not per world and not a ratio: oral genealogies
/// hold roughly constant depth however much time has passed, because the
/// binding constraint is transmission rather than history length. The world's
/// cast is therefore the sum over peoples of `min(MEMORY_DEPTH, occupations)`,
/// which grows when the species roster grows and needs no retuning.
pub const MEMORY_DEPTH: usize = 20;

/// A discriminant mixed into every founder handle, so that a future second
/// role at the same occupation cannot collide with the founder.
const FOUNDER_ROLE: u64 = 0x466F_756E_6465_7200;

/// One remembered founder: an identity plus where it came from.
/// type-audit: bare-ok(count: occupation), bare-ok(count: founded)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Founder {
    /// The stable identity, expandable by `persona_of`.
    pub handle: RoleHandle,
    /// Index into the records slice this founder was selected from.
    pub occupation: usize,
    /// The people who remember this founder.
    pub people: KindId,
    /// The community whose occupation they founded.
    pub community: EntityId,
    /// The occupation's founding day — this founder's birth.
    pub founded: f64,
}

/// Derive a founder's stable identity from an occupation's **semantic** fields.
///
/// Never from its `EntityId` and never from its position in a collection.
/// Decision 0051 forbids salting a procedural name from mint order, and a
/// handle is exactly such a salt — `persona_of` turns it into a name. Keying on
/// the occupation entity would rename every founder in the world the first time
/// an unrelated domain minted earlier in genesis.
///
/// `(people, site, founded, ended, peak_population)` is unique across the
/// selected cast on every measured seed (90/90, 82/82, 100/100 for seeds 42, 7
/// and 1000) and collides on 3 of 1776 occupations world-wide — records that are
/// genuinely indistinguishable in every emitted field. `select_founders`
/// asserts cast-uniqueness rather than trusting it.
fn founder_handle(occ: &OccupationRecord) -> RoleHandle {
    let mut x: u64 = 0xA076_1D64_78BD_642F;
    let mut mix = |v: u64, x: &mut u64| {
        *x ^= v;
        *x = x.wrapping_mul(0x9E37_79B9_7F4A_7C15);
        *x ^= *x >> 29;
    };
    for b in occ.people.0.bytes() {
        mix(u64::from(b), &mut x);
    }
    mix(u64::from(occ.site.0), &mut x);
    mix(occ.founded.to_bits(), &mut x);
    mix(occ.ended.map_or(u64::MAX, f64::to_bits), &mut x);
    mix(u64::from(occ.peak_population), &mut x);
    mix(FOUNDER_ROLE, &mut x);
    RoleHandle(x)
}
```

`f64::to_bits` is exact and platform-independent for a given value, and these days come back from the ledger already quantized, so the bits are stable. Founding days are never `NaN` and never negative zero.

- [ ] **Step 3b: Add `NameKind::Person` to `domains/language`**

A founder's name needs a fourth kind. In `domains/language/src/naming.rs`, add the
variant and its label:

```rust
    /// A person's name: a bare stem, like a settlement's.
    Person,
```

and in `NameKind::label`:

```rust
            NameKind::Person => "person",
```

**Why this is additive and safe.** `label`'s own doc warns that *changing* a label
"silently reseeds every name of that kind in every saved world." Adding one does
not: `Namer` holds no mutable stream (`naming.rs:327-331`) and `name` derives a
fresh stream per call from `ROOT → species → NAME → kind.label() → salt`, so the
new path is disjoint and consumes nothing from any existing stream. Declare the
label wherever this crate publishes its stream labels, following the neighbouring
entries.

Run `cargo test -q -p hornvale-language` and confirm green — no existing name test
may move. If any does, **stop and report**: it would mean the paths are not
disjoint after all, and that is an epoch, not a rebaseline.

- [ ] **Step 4: Implement the selection in `person_promote.rs`**

```rust
/// The founders a world remembers: per people, the `MEMORY_DEPTH` occupations
/// with the largest peak population.
///
/// Ranking is `(peak_population DESC, site ASC, founded ASC)` — a total order
/// over a `u32` and two structural keys, with no float comparison. Iteration is
/// over a `BTreeMap`, so the result does not depend on input order.
///
/// # Panics
///
/// If two selected founders share a handle. That would mean two occupations
/// indistinguishable in every semantic field both reached the cast, and they
/// would silently become one identity with one name. Failing loudly is the
/// point: this is the campaign's determinism guard, not a formality.
pub fn select_founders(records: &[OccupationRecord]) -> Vec<Founder> {
    let mut by_people: BTreeMap<&'static str, Vec<usize>> = BTreeMap::new();
    for (i, r) in records.iter().enumerate() {
        by_people.entry(r.people.0).or_default().push(i);
    }

    let mut cast = Vec::new();
    for idxs in by_people.values_mut() {
        idxs.sort_by(|&a, &b| {
            let (x, y) = (&records[a], &records[b]);
            y.peak_population
                .cmp(&x.peak_population)
                .then(x.site.0.cmp(&y.site.0))
                .then(x.founded.total_cmp(&y.founded))
        });
        for &i in idxs.iter().take(MEMORY_DEPTH) {
            let r = &records[i];
            cast.push(Founder {
                handle: founder_handle(r),
                occupation: i,
                people: r.people,
                community: r.community,
                founded: r.founded,
            });
        }
    }

    let mut seen = std::collections::BTreeSet::new();
    for f in &cast {
        assert!(
            seen.insert(f.handle.0),
            "two selected founders share handle {:#x} — occupations \
             indistinguishable in every semantic field both reached the cast, \
             so they would become one person with one name. Widen the key in \
             founder_handle rather than suppressing this.",
            f.handle.0
        );
    }
    cast
}
```

- [ ] **Step 5: Register the module and run the tests**

Add `mod person_promote;` beside the other module declarations in `windows/worldgen/src/lib.rs`, then:

Run: `cargo test -q -p hornvale-worldgen person_promote`
Expected: PASS, 3 tests.

- [ ] **Step 6: Prove the cast-uniqueness assertion discriminates — and keep the test**

```rust
    #[test]
    #[should_panic(expected = "share handle")]
    fn indistinguishable_occupations_in_the_cast_are_a_hard_error() {
        // Two records identical in every field the handle keys on. The live
        // corpus must never produce this; the guard must fire when it does.
        let a = rec("goblin", 7, 50.0, 60);
        let b = a.clone();
        let _ = select_founders(&[a, b]);
    }
```

This is a **permanent** test, not a scaffold. It asserts that the *guard* fires on
synthetic input — which is a property worth keeping — rather than asserting that
the real corpus collides, which would enshrine a collision as expected. Those are
different claims and only the second would be wrong to keep.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/src/person_promote.rs windows/worldgen/src/lib.rs
git commit -F <a message file>
```

---

### Task 3: The `"person"` stage, last in `build_to`

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (insert a stage between the close of `stage("peoples", ...)` near line 6143 and `Ok(BuildArtifacts {` near line 6145)
- Modify: `windows/worldgen/src/person_promote.rs` (add `promote`)

**Interfaces:**
- Consumes: `select_founders`, `Founder`, `MEMORY_DEPTH` from Task 2; `hornvale_person::{PersonSeed, genesis}` from Task 1.
- Produces: `pub fn promote(world: &mut World, wc: &WorldComponents) -> Result<Vec<EntityId>, BuildError>`.

- [ ] **Step 1: Write the failing integration test**

Create `windows/worldgen/tests/person_promotion.rs`:

```rust
//! Promotion is a pure function of the world it reads: two builds of one seed
//! produce the same cast, and every promoted person is internally coherent.

use hornvale_kernel::Seed;
use hornvale_worldgen::{build_world, SettlementPins, SkyChoice};

fn world() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

#[test]
fn every_person_is_born_before_they_die_and_after_their_community() {
    let w = world();
    let people: Vec<&hornvale_kernel::Fact> =
        w.ledger.find(hornvale_person::IS_PERSON).collect();
    assert!(!people.is_empty(), "seed 42 should remember some founders");

    for p in &people {
        let born = w
            .ledger
            .facts_about(p.subject)
            .find(|f| f.predicate == hornvale_person::PERSON_BORN)
            .and_then(|f| match f.object {
                hornvale_kernel::Value::Number(n) => Some(n),
                _ => None,
            })
            .expect("every person has a birth day");
        if let Some(died) = w
            .ledger
            .facts_about(p.subject)
            .find(|f| f.predicate == hornvale_person::PERSON_DIED)
            .and_then(|f| match f.object {
                hornvale_kernel::Value::Number(n) => Some(n),
                _ => None,
            })
        {
            assert!(died > born, "death must follow birth: {died} vs {born}");
        }
        assert!(
            w.ledger
                .facts_about(p.subject)
                .any(|f| f.predicate == hornvale_person::PERSON_FOUNDED),
            "every person founded something"
        );
    }
}

#[test]
fn the_cast_is_byte_identical_across_two_builds() {
    let a = world();
    let b = world();
    let cast = |w: &hornvale_kernel::World| -> Vec<String> {
        w.ledger
            .find(hornvale_person::PERSON_BORN)
            .map(|f| format!("{:?}|{:?}", f.subject, f.object))
            .collect()
    };
    assert_eq!(cast(&a), cast(&b), "the same seed remembers the same founders");
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -q -p hornvale-worldgen --test person_promotion`
Expected: FAIL — `seed 42 should remember some founders` (no promotion stage yet).

- [ ] **Step 3: Implement `promote`**

Append to `windows/worldgen/src/person_promote.rs`:

```rust
/// Promote every remembered founder into a ledger person.
///
/// Reads occupation records back out of the committed ledger — the `History`
/// value is local to an earlier stage's closure and out of scope here.
///
/// Death is `founded + lifespan(species)`, and the `person-died` fact is
/// committed only when that day has already passed at `now`. A living person is
/// the absence of a death fact. A species with no lifespan (`Ametabolic`) yields
/// no death fact either, which reads as "not known to have died".
pub fn promote(
    world: &mut hornvale_kernel::World,
    wc: &crate::components::WorldComponents,
) -> Result<Vec<EntityId>, crate::BuildError> {
    let records = crate::occupation_records(world);
    let now = world
        .ledger
        .find("history-now")
        .filter_map(|f| match f.object {
            hornvale_kernel::Value::Number(n) => Some(n),
            _ => None,
        })
        .next_back()
        .unwrap_or(0.0);

    let cast = select_founders(&records);
    let mut seeds = Vec::with_capacity(cast.len());
    for f in &cast {
        let lifespan_days = wc
            .biosphere
            .get(&f.people)
            .and_then(|b| {
                hornvale_species::allometry::life_history(b.mass, b.metabolic_class).lifespan
            })
            .map(|y| y.days());
        let death = lifespan_days
            .map(|d| f.founded + d)
            .filter(|d| *d <= now);
        // Named here, where the language machinery already stands. `Namer` holds
        // no mutable stream and derives fresh per call, so this draw is on a
        // path disjoint from every other name in the world.
        let ph = crate::language_of_wc(world, wc, f.people.0);
        let namer = hornvale_language::Namer::new(&world.seed, f.people.0, &ph);
        let name = namer
            .name(
                hornvale_language::NameKind::Person,
                f.handle.0,
                &crate::morph_options(world, wc),
            )
            .to_string();
        seeds.push(hornvale_person::PersonSeed {
            community: f.community,
            name,
            birth_day: f.founded,
            death_day: death,
        });
    }
    hornvale_person::genesis(world, &seeds).map_err(crate::BuildError::from)
}
```

If `crate::BuildError` has no `From<LedgerError>`, map the error the way the neighbouring stages do — read one and match it; do not invent a new variant.

- [ ] **Step 4: Insert the stage last**

In `windows/worldgen/src/lib.rs`, between the `})?;` that closes `stage("peoples", ...)` and the `Ok(BuildArtifacts {`:

```rust
    // LAST, deliberately. `mint_entity` is a monotonic counter, so appending
    // mints cannot shift an existing `EntityId` only while nothing mints after
    // — the same reason species entities are appended rather than interleaved.
    // Moving this earlier silently rewrites every world.
    stage("person", || -> Result<(), BuildError> {
        person_promote::promote(&mut world, wc)?;
        Ok(())
    })?;
```

Do **not** add `"person"` to `GENESIS_HAND_ORDER` or `genesis_systems()`. The schedule module's own doc scopes that schema to the eight core stages and excludes the classification tail, because a late `name`-writer is falsely forced before an early `name`-reader by predicate-granular, subject-blind edges. A `"person"` stage after `"peoples"` is architecturally the same case. Add a one-line comment saying so, citing that doc, so the omission reads as deliberate.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -q -p hornvale-worldgen --test person_promotion`
Expected: PASS, 2 tests.

- [ ] **Step 6: Confirm the cast matches the spec's measured figures**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-p42.json
python3 -c "
import json,collections
f=json.load(open('/tmp/hv-p42.json'))['ledger']['facts']
c=collections.Counter(x['predicate'] for x in f)
print('persons', c['is-person'], 'born', c['person-born'], 'died', c['person-died'])
print('total facts', len(f))
"
```

Expected: `persons 90`, `born 90`, and `total facts` about 26,309 + 270–360. If the person count is not 90 for seed 42, **stop and report** — the spec's prediction P7 and its cast measurement both say 90, and a different number means the selection diverged.

- [ ] **Step 7: Accept the world fixture and check nothing else moved**

```bash
REBASELINE=1 cargo test -q -p hornvale --test lens_purity
make rebaseline
git status --short book/src docs/audits cli/tests/fixtures
```

`cli/tests/fixtures/world-seed-42.json` must move. `scene/golden` and `session_snapshot` must **not** — run them and confirm green:

```bash
cargo test -q -p hornvale-scene --test golden
cargo test -q -p hornvale-vessel --test session_snapshot
```

If either is red, persons have reached a client-facing scene document. **Stop and report** — scene schemas are cross-repo contracts.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/worldgen cli/tests/fixtures book/src docs/audits
git commit -F <a message file>
```

---

### Task 4: The almanac names a remembered founder

**Files:**
- Modify: `windows/almanac/Cargo.toml`
- Modify: `windows/almanac/src/history.rs:443-457` (`founding_sentence`)

**Interfaces:**
- Consumes: `hornvale_person::{IS_PERSON, PERSON_FOUNDED}` from Task 1; `hornvale_history::flesh::persona_of` (already available).
- Produces: nothing downstream.

- [ ] **Step 1: Write the failing test**

Add to `windows/almanac/src/history.rs`'s test module:

```rust
    #[test]
    fn a_remembered_founder_is_named_and_an_unremembered_one_is_silent() {
        let world = crate::tests::seed_42_world();
        let prose = crate::history::render(&world);
        let named = prose.matches(" was founded by ").count();
        let persons = world.ledger.find(hornvale_person::IS_PERSON).count();
        assert!(named > 0, "some settlement should name its founder");
        assert!(
            named <= persons,
            "no more founder sentences ({named}) than founders ({persons})"
        );
        assert!(
            !prose.contains("founded by an unremembered"),
            "silence, not a placeholder, is the rendering for no remembered founder"
        );
    }
```

If `crate::tests::seed_42_world()` does not exist, build a seed-42 world inline the way the neighbouring tests in that file do — read one first and copy its construction.

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -q -p hornvale-almanac a_remembered_founder`
Expected: FAIL — `named > 0` is false, because no founder sentence exists yet.

- [ ] **Step 3: Add the dependency**

In `windows/almanac/Cargo.toml`, beside the other domain dependencies:

```toml
hornvale-person = { path = "../../domains/person" }
```

- [ ] **Step 4: Add the founder clause**

In `founding_sentence`, after computing the existing sentence, append a founder clause when one is remembered. Insert a helper above it:

```rust
/// The name of the founder this community remembers, if it remembers one.
///
/// Silence is the correct rendering of a founder nobody remembers. A
/// placeholder phrase would reproduce the defect where every settlement in
/// every world narrated the same sentence.
fn remembered_founder(world: &World, r: &OccupationRecord) -> Option<String> {
    // Promotion commits `person-founded` only for the selected cast, so its
    // absence is the answer to "is a founder remembered here?".
    let person = world
        .ledger
        .find(hornvale_person::PERSON_FOUNDED)
        .find(|f| f.object == Value::Entity(r.community))?
        .subject;
    // The name is committed, so read it — the same two-fact lookup the almanac
    // already does for settlement names. No language machinery in a window.
    world.ledger.text_of(person, hornvale_kernel::NAME).map(str::to_string)
}
```

Check `text_of`'s exact signature before pasting — `windows/almanac/src/history.rs`
already calls it for settlement names, so copy that call's shape. If it returns
`Option<&str>`, the `map(str::to_string)` above is right; if it returns
`Option<String>`, drop the `map`.

- [ ] **Step 5: Run the test to verify it passes**

Run: `cargo test -q -p hornvale-almanac a_remembered_founder`
Expected: PASS.

- [ ] **Step 6: Regenerate the gallery and read the prose**

```bash
make rebaseline
git diff book/src/gallery/history-seed-42.md | head -60
```

Read the diff as a reader, not a compiler. A founder sentence should appear for some settlements and not others, and should not read as boilerplate.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/almanac book/src/gallery
git commit -F <a message file>
```

---

### Task 5: Score the predictions and close the campaign

**Files:**
- Create: `book/src/chronicle/the-particular.md`
- Create: `docs/retrospectives/the-particular.md`
- Modify: `book/src/SUMMARY.md`, `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Score all seven preregistered items**

From `docs/superpowers/specs/2026-08-01-the-particular-design.md` §5. **P1, P3 and P4 are verification** — they cannot fail unless the implementation is wrong. **P2, P6 and P7 are the predictions.** P5 is an identity.

Score each against `docs/audits/trope-coverage.md` and the built worlds. For **P6** — at least one same-people pair of remembered founders with overlapping lifespans — write a throwaway script over the seed-42 world, report the answer, and **do not adjust anything if it fails**. A falsified P6 means the cast has no contemporaries, which is the most useful thing this campaign can learn.

- [ ] **Step 2: Write the chronicle**

`book/src/chronicle/the-particular.md`, and add it to `book/src/SUMMARY.md` in close order (after The Repertoire). Book prose is technical and mathematical, comprehensible without reading the code. Cover: what a particular is and why a distribution cannot yield one; the promotion of an already-shipped mechanism that had no consumer; memory belonging to a people rather than a world; the seven items scored with any falsification stated plainly. **No process content** — the shell-quoting and fixture mechanics belong in the retrospective (decision 0020).

- [ ] **Step 3: Write the retrospective**

`docs/retrospectives/the-particular.md`. Promote **every** item from `.superpowers/sdd/followups.md` — F1 through F8 — because that file is git-ignored and dies with the worktree. F1 (constant fields stranding unreachable residue and unreadable prose), F2 (no ledger-size ratchet), F3 (history constants an order of magnitude above demography's output), F4 (a lexicon word satisfying a capability token), F5 (`EntityId` as positional identity, both tiers), F6 (a fixture proves correctness, never reachability), F7 (three instances of capability without a consumer), F8 (resolved — folded in).

- [ ] **Step 4: Flip the registry rows**

Flip what shipped to `shipped` with **Where** pointing at the chronicle. Before editing, read `book/src/frontier/CLAUDE.md`. **Measure every Idea cell — do not estimate.** The cap is 600 characters and three rows in this session were rejected at 646, 634 and 605 on the first attempt.

- [ ] **Step 5: Book freshness sweep**

Re-score any Confidence Gradient bet in `book/src/open-questions.md` that this campaign moves (decision 0030). If none moves, say so explicitly rather than skipping silently.

- [ ] **Step 6: Full gate**

```bash
bash scripts/census-run.sh status
make gate
```

Pass an explicit Bash `timeout: 3600000`. Expect green, with the test count risen by the tests this plan added.

- [ ] **Step 7: Commit, then STOP**

```bash
cargo fmt
git add book docs
git commit -F <a message file>
```

**G6 is a hard stop.** Do not merge, do not push, do not delete the worktree. Present the post-G3 ledger digest and wait for Nathan.

---

## Self-Review Notes

**Spec coverage.** D1 → Task 2's `founder_handle`. D2 → Task 2's `MEMORY_DEPTH`/`select_founders`. D3 → Task 3 Step 4. D4 → Task 1 Steps 6-8. D4a → Task 1 Step 4 (no concepts registered). D5 → Global Constraints, and no task introduces a draw. D6 → Task 1 (kernel-only manifest). D7 → Task 3's coherence test. D8 → not implemented, correctly. D9 → Task 4. §5 P1-P7 → Task 5 Step 1. §10 DoD → Task 5.

**One gap I could not close from the armchair**, flagged in place rather than papered over: the almanac's existing name-rendering helper (Task 4 Step 4). Everything else resolved during self-review — including a real design hole in my own spec. The almanac needs a founder's handle and the ledger does not carry it, which first read as an implementer decision between committing the handle as a fact or recomputing it. Neither was needed: `founder_handle` belongs in `domains/history::flesh` beside `persona_of`, where both worldgen and the almanac reach one derivation, and nothing derived enters the save format.

**Type consistency.** `PersonSeed { community, birth_day, death_day }` is used identically in Tasks 1 and 3. `Founder { handle, occupation, people, community, founded }` is produced in Task 2 and consumed in Task 3. `MEMORY_DEPTH` is `usize` in both. `Years::days()` is the only lifespan conversion, and Task 3 uses it.
