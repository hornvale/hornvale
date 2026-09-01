# The Wicket Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Retire the closed `AnchorKind` enum so an object kind is a row rather
than a variant, replace the compiler's totality guarantee with a default-deny
registry gate, and then do the same to the first authored scalar that should
never have been one — the sleep-debt rate — splitting `wait`/`rest`/`sleep` into
three acts on the way.

**Architecture:** A kind becomes `KindId` plus rows in open component tables,
which is what a species already is. `domains/thing` publishes the roster and
named `KindId` handles; `windows/vessel`'s interior grammar, prose and
affordance query all key on `KindId`; a family of default-deny tests replaces
the three exhaustive matches. Then fatigue stops being a flag cleared by any
rest and becomes a stock discharged by an amount, with the rate a per-species
component read against the planetary day.

**Tech Stack:** Rust 2024, std-only plus `serde`/`serde_json`/`libm`.
`cargo nextest`, in-module `#[cfg(test)]` for domains, `tests/suite/*.rs`
behind one `tests/suite.rs` binary for windows.

**Spec:** `docs/superpowers/specs/2026-09-01-the-wicket-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md` — append a
ruling as it is made, do not batch to the end.

## Global Constraints

- **Layering** (`cli/tests/architecture.rs`): `kernel` → `domains/*` →
  `windows/*` → `cli`. A domain may not depend on a sibling domain. A window
  may depend on a domain. Adding `hornvale-thing` to `windows/vessel` is legal;
  adding any domain dep to `domains/thing` is not.
- **Dependencies**: `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by
  `clippy.toml` `disallowed-types`).
- **No wall-clock time.** Time is `WorldTime { ticks: i64 }`, 100,000 ticks per
  **standard** day. `TickSpan(i64)` is the signed difference.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field and
  variant gets a one-line doc comment.
- **`tools/type-audit` is default-deny**, and it is the commit gate's third
  step — stricter than `missing_docs` and the one that actually reddens. Every
  primitive at a `pub` boundary carries a verdict tag (`bare-ok(<class>)` /
  `waiver(<reason>)` / `pending(wave-N)`) as the last line of its doc comment.
  A new `pub const` holding a `&str`, a `bool`, an index or a count needs one;
  `bare-ok(identifier-text)` is the class for a label.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the
  most common review finding.
- **Commit gate**: `make gate-commit` runs on every commit via the pre-commit
  hook for Rust-relevant paths. Never bypass a commit hook; if fmt or clippy is
  red, that IS the finding.
- **Every test carries a `MUTATION THIS MUST FAIL AGAINST:` line in its doc
  comment**, naming a concrete edit and pasting the observed red. This is the
  house convention (`domains/thing/src/lib.rs`'s existing tests are the model)
  and decision 0353. A mutation that does not compile is not evidence; find one
  that does.
- **Push at every task boundary.** An unpushed branch is invisible to the
  sluice's mouth.
- Branch: `campaign/the-wicket`. Worktree: `.claude/worktrees/the-wicket`.

---

## File Structure

| File | Responsibility | Tasks |
| --- | --- | --- |
| `domains/thing/src/lib.rs` | The kind roster, the registry rows, and (new) the named `KindId` handles + the ordered-set ratchet | 1, 5 |
| `domains/species/src/lib.rs` | The per-species sleep-debt rate, beside the registries already there | 9 |
| `windows/vessel/Cargo.toml` | Gains `hornvale-thing` | 2 |
| `windows/vessel/src/interior/anchor.rs` | `Anchor`/`Interior` re-keyed to `KindId`; `anchor_kinds!` and `AnchorKind` deleted | 2 |
| `windows/vessel/src/interior/pattern.rs` | `Pattern`/`Attach` re-keyed; the brazier appended | 2, 5 |
| `windows/vessel/src/interior/{field,seam,derive,route}.rs` | Predicates re-keyed to named handles | 2 |
| `windows/vessel/src/affordance.rs` | `thing_kind_of` deleted; `object_registry` gains the brazier row | 2, 5 |
| `windows/vessel/src/chamber_prose.rs` | `noun`/`detail` become one `ComponentStore<KindId, ChamberProse>`; `noun_for_label` collapses into `noun` | 4 |
| `windows/vessel/src/session.rs` | Call sites re-keyed; `sleep` split from `rest`; the three false comments corrected | 2, 6, 8 |
| `windows/vessel/src/action.rs` | `Action::Sleep` added beside `Action::Rest` | 8 |
| `windows/vessel/src/liveness.rs` | Fatigue becomes a recovery stock; rate becomes a component read on the planetary day | 7, 9, 10 |
| `windows/vessel/tests/suite/kind_totality.rs` | **New.** The default-deny gate family G-a..G-f | 3, 4 |
| `cli/tests/suite/anchor_thing_correspondence.rs` | Retired into the totality gate | 3 |

---

## Stage 1 — the roster grows handles

### Task 1: Named `KindId` handles and the ordered-set ratchet

`domains/thing` already owns the roster (`THING_KINDS`, 16 rows) and the
registry (`thing_registry`), and already pins them against each other in both
directions. What it does not have is a way for *code* to name a kind without
spelling a string literal, which is the one thing the enum was good at. This
task adds that, and freezes the roster so growth is deliberate.

**Files:**
- Modify: `domains/thing/src/lib.rs`
- Test: `domains/thing/src/lib.rs` (in-module `#[cfg(test)] mod tests`)
- **Not** `windows/vessel/src/passage.rs` — Step 4 explains where that edit
  goes and why it does not go here.

**Interfaces:**
- Consumes: `hornvale_kernel::KindId`, `THING_KINDS`, `thing_registry()`.
- Produces: `hornvale_thing::kinds::{ALCOVE, ALTAR, ANVIL, BED, CAVE_MOUTH,
  GROUND, HEARTH, HIGH_SEAT, KEY, LOG, LOOM, POOL, SCREEN, STRONGBOX,
  THRESHOLD, VESSEL}`, each `pub const … : KindId`, plus
  `kinds::EVERY_HANDLE: &[(&str, KindId)]`. Every later task names a kind
  through this module and never with a bare `KindId("…")` literal.

- [ ] **Step 1: Write the failing tests**

Add to `domains/thing/src/lib.rs`'s `mod tests`:

```rust
    /// Every named handle resolves to a roster row (G-d, spec §5.1). The
    /// direction this enforces is **named ⊆ rostered**: it cannot see a
    /// rostered kind that has no handle, and deliberately so — a kind no code
    /// names needs no handle, which is the whole difference between a handle
    /// and a variant.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: change `kinds::HIGH_SEAT` to
    /// `KindId("high_seat")` (underscore for hyphen — the spelling a reader
    /// guesses). It compiles, and every consumer keeps compiling, which is
    /// exactly the failure a bare literal invites.
    #[test]
    fn every_named_handle_is_a_roster_row() {
        for (name, id) in kinds::EVERY_HANDLE {
            assert!(
                THING_KINDS.contains(&id.0),
                "handle {name} is {:?}, which the roster does not carry",
                id.0
            );
        }
    }

    /// The roster is frozen as an ORDERED SET, not a count (G-f, spec §5.1).
    ///
    /// A length assertion passes any compensating swap — drop one kind, add
    /// another, and a count-based ratchet reports nothing. Freezing the
    /// sequence makes every addition, removal and reordering a visible edit to
    /// this list, which is the discipline `AnchorKind::ALL` bought by being
    /// generated from the enum's own declaration. Update this list in the same
    /// commit that changes the roster, never afterwards.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: swap the `"log"` and `"loom"` entries
    /// in `THING_KINDS`. The length is unchanged and the set is unchanged;
    /// only the order moves, and a count-based check would stay green.
    #[test]
    fn the_roster_is_frozen_as_an_ordered_set() {
        const FROZEN: &[&str] = &[
            "alcove",
            "altar",
            "anvil",
            "bed",
            "cave-mouth",
            "ground",
            "hearth",
            "high-seat",
            "key",
            "log",
            "loom",
            "pool",
            "screen",
            "strongbox",
            "threshold",
            "vessel",
        ];
        assert_eq!(
            THING_KINDS, FROZEN,
            "the thing-kind roster moved; update FROZEN in the same commit"
        );
    }
```

- [ ] **Step 2: Run them and watch them fail**

Run: `cargo test -p hornvale-thing`

Expected: FAIL to compile — the `kinds` module does not exist. That is a
compile error, not a behavioural red, so **it proves nothing about the
assertions**. After Step 3 lands the module, Step 5 applies each mutation and
captures the real red. A red from a missing module is not evidence.

- [ ] **Step 3: Add the handles module**

Add to `domains/thing/src/lib.rs`, after `THING_KINDS`:

```rust
/// Named handles for the kinds that code names.
///
/// **A handle is a convenience; a variant was a requirement.** This is the
/// asymmetry the whole campaign turns on. `AnchorKind` made a variant
/// mandatory: a kind with no variant could not be placed in a room, however
/// open the stores behind it were. A handle is the opposite — it exists so a
/// predicate can say `kind == kinds::HEARTH` instead of `kind ==
/// KindId("hearth")` and have the compiler catch the typo. A kind with **no**
/// handle is a first-class kind that simply has no predicate written against
/// it, and adding a kind never requires adding one here.
///
/// So: add a handle when you write code that names the kind. Do not add one
/// "for completeness" — an unused handle is a name with no reader.
pub mod kinds {
    use hornvale_kernel::KindId;

    /// A recess off the main space.
    pub const ALCOVE: KindId = KindId("alcove");
    /// An altar.
    pub const ALTAR: KindId = KindId("altar");
    /// A smith's anvil.
    pub const ANVIL: KindId = KindId("anvil");
    /// A place to sleep.
    pub const BED: KindId = KindId("bed");
    /// The mouth of a cave — a `Vertex`/`ChamberAddr`, never an anchor.
    pub const CAVE_MOUTH: KindId = KindId("cave-mouth");
    /// The room's open middle: the floor itself, not a thing standing on it.
    pub const GROUND: KindId = KindId("ground");
    /// A fire.
    pub const HEARTH: KindId = KindId("hearth");
    /// The seat that commands the entrance.
    pub const HIGH_SEAT: KindId = KindId("high-seat");
    /// A small key.
    pub const KEY: KindId = KindId("key");
    /// A fallen log.
    pub const LOG: KindId = KindId("log");
    /// An upright loom.
    pub const LOOM: KindId = KindId("loom");
    /// A natural pool.
    pub const POOL: KindId = KindId("pool");
    /// A screen or pillar: affords nothing, shapes sightlines.
    pub const SCREEN: KindId = KindId("screen");
    /// A locked chest.
    pub const STRONGBOX: KindId = KindId("strongbox");
    /// A doorway — an anchor that is also a room-graph edge.
    pub const THRESHOLD: KindId = KindId("threshold");
    /// A water vessel or basin.
    pub const VESSEL: KindId = KindId("vessel");

    /// Every handle with its own name, for the roster check.
    ///
    /// Hand-written, and that is a deliberate cost rather than an oversight:
    /// there is no macro here because a macro generating both the constants
    /// and this list would make the list unable to disagree with them. That
    /// is what makes `AnchorKind::ALL` safe and it is exactly what is NOT
    /// wanted here — this list is checked against the ROSTER, a third party,
    /// so it must be able to go wrong.
    ///
    /// type-audit: bare-ok(identifier-text)
    pub const EVERY_HANDLE: &[(&str, KindId)] = &[
        ("ALCOVE", ALCOVE),
        ("ALTAR", ALTAR),
        ("ANVIL", ANVIL),
        ("BED", BED),
        ("CAVE_MOUTH", CAVE_MOUTH),
        ("GROUND", GROUND),
        ("HEARTH", HEARTH),
        ("HIGH_SEAT", HIGH_SEAT),
        ("KEY", KEY),
        ("LOG", LOG),
        ("LOOM", LOOM),
        ("POOL", POOL),
        ("SCREEN", SCREEN),
        ("STRONGBOX", STRONGBOX),
        ("THRESHOLD", THRESHOLD),
        ("VESSEL", VESSEL),
    ];
}
```

- [ ] **Step 4: Note where `passage::CAVE_MOUTH` goes, and do not do it here**

`windows/vessel/src/passage.rs:71` declares `pub const CAVE_MOUTH: &str =
"cave-mouth";` — a second spelling of a label the roster already owns. It
should become a re-export of the handle:

```rust
/// The thing-kind a cave mouth is. Re-exported from the roster's own handle
/// rather than spelled again here: a second literal is a second thing to keep
/// in step, and the check that used to keep them in step
/// (`anchor_thing_correspondence`) is retired by Task 3.
pub const CAVE_MOUTH: &str = hornvale_thing::kinds::CAVE_MOUTH.0;
```

That needs the `hornvale-thing` dependency, which Task 2 Step 1 adds. **Do it
at the top of Task 2, not here** — Task 2's acceptance criterion is an artifact
diff, and a stray dependency edit landing in this task's commit would muddy
which change produced it.

- [ ] **Step 5: Verify each mutation reddens, and paste the reds**

For each of the two tests, apply the mutation its doc comment names, run
`cargo test -p hornvale-thing`, copy the failure text into the doc comment under
`Red observed:`, then revert.

Use `scripts/mutate.py` rather than a `sed` expression: it substitutes only when
the target text is found and is unique, so a mutation that silently matched
nothing cannot masquerade as a robust green. On macOS `sed`'s `\b` matches
nothing at all and exits 0, which is the specific way this goes wrong quietly.

After reverting, re-run and confirm green before moving on — a restored
mutation can leave cargo holding the mutated build.

- [ ] **Step 6: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add domains/thing/src/lib.rs
git commit -m "feat(thing): named KindId handles and an ordered-set roster ratchet"
git push
```

---

## Stage 2 — the re-key

### Task 2: `AnchorKind` becomes `KindId`

The atomic one. Changing `Anchor::kind`'s type breaks every consumer at once,
so this cannot be staged — the compiler is the worklist. It is mechanical, and
the acceptance criterion is that **the generated artifacts do not move**.

**Files:**
- Modify: `windows/vessel/Cargo.toml`, `windows/vessel/src/passage.rs`
- Modify: `windows/vessel/src/interior/anchor.rs` (delete `anchor_kinds!`,
  `AnchorKind`, `AnchorKind::ALL`; re-key `Anchor`/`Interior`)
- Modify: `windows/vessel/src/interior/{pattern,field,seam,derive,route,mod}.rs`
- Modify: `windows/vessel/src/affordance.rs` (delete `thing_kind_of`)
- Modify: `windows/vessel/src/{session,chamber_prose,snapshot,light,thing}.rs`
- Modify: `windows/vessel/src/lattice/render.rs`
- Modify: `windows/vessel/tests/suite/{affordance,thing,noun_entity}.rs`
- Modify: `cli/tests/suite/anchor_thing_correspondence.rs`

**Interfaces:**
- Consumes: Task 1's `hornvale_thing::kinds::*`.
- Produces: `Anchor { kind: KindId, within: Option<AnchorId> }`;
  `Interior::push(&mut self, kind: KindId, within: Option<AnchorId>) ->
  AnchorId`; `Pattern { name: &'static str, kind: KindId, attach: Attach,
  requires: Option<KindId>, needs_cold: bool, built: bool, roles: &'static
  [Role], at_locale: bool, needs_populous: bool }`; `Attach::{Hub,
  Beside(KindId), Within(KindId)}`. `affordance::thing_kind_of` no longer
  exists — call sites that wrapped a kind in it pass the kind straight through.
  `chamber_prose::noun_for_label` no longer exists; `noun` takes its place.

- [ ] **Step 1: Add the dependency and land the `CAVE_MOUTH` re-export**

`windows/vessel/Cargo.toml`, in `[dependencies]`, beside the other domains:

```toml
hornvale-thing = { path = "../../domains/thing" }
```

Then apply Task 1 Step 4's `passage::CAVE_MOUTH` re-export. Run
`cargo check -p hornvale-vessel`; it must still build. A window depending on a
domain is layering-legal, and vessel already depends on eight of them.

- [ ] **Step 2: Record the pre-change artifact state**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Expected: clean. If it is NOT clean, stop and report — something landed dirty,
and Step 6's acceptance criterion would be meaningless against a dirty baseline.

- [ ] **Step 3: Delete the enum and re-key `anchor.rs`**

Delete the whole `anchor_kinds!` macro and its invocation. `Anchor` becomes:

```rust
/// One anchor: what it is, and the anchor it lies strictly within, if any.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Anchor {
    /// What this anchor is — a thing-kind, and a row rather than a variant
    /// (The Wicket). A kind the roster does not carry cannot reach here,
    /// because `Pattern` is the only producer and Task 3 gates it.
    pub kind: KindId,
    /// The anchor this one lies strictly inside (`Ntpp`), if any.
    pub within: Option<AnchorId>,
}
```

and `push` takes `kind: KindId`. Everything else in the file — `AnchorId`,
`connect`, `neighbors`, `walkable_neighbors`, `within_chain`, `relation`,
`is_connected` — is untouched: none of them reads a kind.

- [ ] **Step 4: Follow the compiler**

Run `cargo check -p hornvale-vessel --all-targets` and fix what it names. The
substitutions are mechanical:

```text
  AnchorKind::Hearth             ->  hornvale_thing::kinds::HEARTH
  AnchorKind::Threshold          ->  hornvale_thing::kinds::THRESHOLD
  Attach::Beside(AnchorKind::X)  ->  Attach::Beside(kinds::X)
  requires: Some(AnchorKind::X)  ->  requires: Some(kinds::X)
  thing_kind_of(k)               ->  k
```

**Do not trust one check run to enumerate the work.** A `cargo check` that
fails early has enumerated nothing past where it stopped — the error list is a
floor, not a total. Re-run until clean, then run `--all-targets` again, because
each integration-test file is its own crate.

`chamber_prose::noun_for_label` collapses in this step. Its body scanned
`AnchorKind::ALL` through `thing_kind_of` looking for a label, which after the
re-key is exactly what `noun` itself takes. Delete it and re-point its callers
at `noun`. Its own doc already anticipated this: it says a second hand-written
label→noun map would be the duplicated-table shape decision 0261 warns about,
and the re-key removes the need for either.

- [ ] **Step 4b: Sweep what the compiler cannot see**

**The compiler will not enumerate this task's whole worklist**, and that is a
measured fact rather than a caution. Three files mention `AnchorKind` only in
prose — `snapshot.rs` (3 lines), `light.rs` (1), `thing.rs` (2) — so they
compile clean after the type is gone and keep describing a type that no longer
exists. Worse, at least ten of the mentions are **intra-doc links** of the form
``[`AnchorKind::ALL`]`` (in `chamber_prose.rs`, `session.rs`, `affordance.rs`,
and `tests/suite/affordance.rs`). A broken intra-doc link is a *rustdoc* lint,
not a rustc one, so `cargo clippy --workspace --all-targets -- -D warnings`
stays green on every one of them.

After the compiler is quiet, run:

```bash
git grep -n "AnchorKind" -- '*.rs' | wc -l
git grep -n "AnchorKind" -- '*.rs'
```

Expected end state: **zero**. Each remaining mention is one of three cases, and
each is resolved, not left:

- an intra-doc link → re-point at `hornvale_thing::kinds` or the roster, or
  delete the clause if the sentence was only ever about the enum;
- a sentence explaining why something is the way it is *because* of the enum →
  rewrite it to say what is now true. Several of these are load-bearing
  explanations (the `anchor_kinds!` macro's own rationale about rosters going
  short) and their content belongs with the roster ratchet in
  `domains/thing`, not deleted;
- a historical note in a test doc → keep the history, past-tense it.

A stale doc comment describing a deleted mechanism is the exact failure this
campaign is correcting in the frontier essay and in three of vessel's own
comments (Task 6). Do not create ten more while removing three.

- [ ] **Step 5: Run the vessel suite once, inspect many**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-wicket-t2.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-wicket-t2.log
```

Every test must pass **with its expectations unchanged** — the re-key is
behaviour-preserving, so a test needing its expected value edited is a signal,
not a chore. If one does, stop and report which and why.

- [ ] **Step 6: The acceptance criterion — read the branch table, do not predict**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Respond by branch:

- **Nothing moved** → proceed, and say so explicitly in the commit message.
  This is the outcome spec §2.1 argues for: the three `BTree*` collections
  keyed on the old enum were all lookup-only, and interior order comes from a
  `Vec` in `INVENTORY` declaration order, so lexical `KindId` ordering reaches
  no iteration.
- **Only `docs/audits/` moved** → the type-audit report drifted on a
  pub-boundary change, which this task certainly makes. Regenerate and commit
  it in the SAME commit:
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- **Anything under `book/src/gallery/`, `book/src/domesday/`, or a census CSV
  moved** → STOP. The re-key is then not behaviour-preserving, spec §2.1's
  argument has a hole, and finding the hole is the deliverable. Report it; do
  not rebaseline past it.

- [ ] **Step 7: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "refactor(vessel): re-key the interior grammar from AnchorKind to KindId"
git push
```

---

### Task 3: The pattern-resolves gate (G-a), replacing the correspondence test

`cli/tests/suite/anchor_thing_correspondence.rs` exists because `cli` was the
only crate depending on both `hornvale-vessel` and `hornvale-thing`. Task 2
gives vessel that dependency directly, so the check moves to where its subject
lives — and widens, because after Task 2 the compiler no longer guarantees that
a pattern names a real kind.

**Files:**
- Create: `windows/vessel/tests/suite/kind_totality.rs`
- Modify: `windows/vessel/tests/suite.rs` (add `mod kind_totality;`)
- Delete: `cli/tests/suite/anchor_thing_correspondence.rs`
- Modify: `cli/tests/suite.rs` (drop its `mod` line)

**Interfaces:**
- Consumes: `hornvale_vessel::interior::pattern::{INVENTORY, Attach}`,
  `hornvale_thing::THING_KINDS`.
- Produces: nothing other tasks consume; Task 4 appends to the same file.

- [ ] **Step 1: Write the test**

`windows/vessel/tests/suite/kind_totality.rs`:

```rust
//! The default-deny gate family that replaces the compiler's exhaustiveness
//! (The Wicket, spec §5). Each test states the DIRECTION it enforces in its
//! own doc comment, because a gate asserting `declared ⊆ resolvable` is
//! structurally blind to over-admission and still reads as total to the next
//! reader.

use hornvale_thing::THING_KINDS;
use hornvale_vessel::interior::pattern::{Attach, INVENTORY};

/// **Direction: declared ⊆ rostered.** Every kind the authored pattern
/// inventory names — the anchor it contributes, the kind it requires, and the
/// kind it attaches beside or within — has a row in `THING_KINDS`.
///
/// This is the guarantee the enum gave for free: before The Wicket a pattern
/// could only name a variant, and a variant could only exist by being
/// declared. Now a pattern names a string, and a typo compiles.
///
/// It does NOT check the converse — that every rostered kind is placeable by
/// some pattern — and that omission is deliberate. `cave-mouth` is a rostered
/// kind that is a `Vertex`/`ChamberAddr` and never an anchor, so the converse
/// would need an exemption list, and an exemption list is a thing nobody
/// maintains.
///
/// MUTATION THIS MUST FAIL AGAINST: change `the-high-seat`'s `kind` in
/// `INVENTORY` to `KindId("high_seat")` — underscore for hyphen. It compiles,
/// the room composes, and the anchor silently has no prose and no properties.
#[test]
fn every_kind_the_grammar_names_is_a_roster_row() {
    let mut checked = 0usize;
    for p in INVENTORY.iter() {
        let mut named = vec![("kind", p.kind)];
        if let Some(r) = p.requires {
            named.push(("requires", r));
        }
        match p.attach {
            Attach::Beside(k) | Attach::Within(k) => named.push(("attach", k)),
            Attach::Hub => {}
        }
        for (slot, id) in named {
            assert!(
                THING_KINDS.contains(&id.0),
                "pattern {:?} names {:?} in its {slot}, which the roster does not carry",
                p.name,
                id.0
            );
            checked += 1;
        }
    }
    // A vacuous pass is the failure mode of any loop-over-a-table test: an
    // empty INVENTORY satisfies every assertion above. One kind per pattern is
    // the floor, so this cannot pass while the loop is not running.
    assert!(
        checked >= INVENTORY.len(),
        "checked {checked} kinds across {} patterns — the loop is not running",
        INVENTORY.len()
    );
}
```

- [ ] **Step 1b: The property-table gate (G-e), in the same file**

```rust
/// **Direction: propertied ⊆ rostered.** Every key in `object_registry()` is a
/// roster row, so a property can never be granted to a kind that does not
/// exist.
///
/// This one has bitten in miniature already: `object_registry`'s own doc
/// carried a claim about which rows had no `AnchorKind` behind them that went
/// stale four tasks after it was written, and nothing checked it. A table of
/// string keys wants a check that the strings mean something.
///
/// MUTATION THIS MUST FAIL AGAINST: change the `strongbox` key in
/// `object_registry()` to `KindId("strong-box")`. It compiles; `offered_by`
/// silently returns an empty set for the real strongbox, and every lock in
/// every world stops offering `open`.
#[test]
fn every_propertied_kind_is_a_roster_row() {
    let reg = hornvale_vessel::affordance::object_registry();
    assert!(!reg.is_empty(), "the property table is empty — this test is vacuous");
    for id in reg.ids() {
        assert!(
            THING_KINDS.contains(&id.0),
            "object_registry has {:?}, the roster does not",
            id.0
        );
    }
}
```

- [ ] **Step 1d: Point the frozen verb table at the roster, not at the handles**

Task 2 left `every_named_kind()` (`windows/vessel/tests/suite/affordance.rs`)
reading `hornvale_thing::kinds::EVERY_HANDLE`, and flagged in its own doc that
this is **one step weaker than what it replaced**: `AnchorKind::ALL` was
generated from the enum's declaration and could not go short, while
`EVERY_HANDLE` is hand-written and can. Task 1's
`every_named_handle_is_a_roster_row` checks only *named ⊆ rostered*, so a handle
quietly dropped from `EVERY_HANDLE` narrows the frozen verb table's sweep and
nothing objects.

Sweep the set that cannot go short instead:

```rust
fn every_named_kind() -> Vec<KindId> {
    hornvale_thing::THING_KINDS.iter().map(|l| KindId(l)).collect()
}
```

`THING_KINDS` is frozen as an ordered set by
`the_roster_is_frozen_as_an_ordered_set`, so a kind cannot leave it silently.
Strictly stronger, and it needs no new mechanism — handles are for *code that
names a kind*, and a sweep is not that.

Confirm the frozen verb table passes unchanged after the swap. If it does not,
the two sets disagree today and **that disagreement is the finding** — report
it rather than editing the table to match.

- [ ] **Step 1c: Decide the bare-literal question, and record the decision**

Spec §9 leaves this to the task: after Task 2, a production predicate could
write `KindId("hearht")` and compile. Task 1's handles are the mitigation; the
residual risk is an inline literal written anyway.

Run the census first, then decide:

```bash
git grep -n 'KindId("' -- windows/vessel/src domains/thing/src | grep -v 'kinds::' | wc -l
```

- **Zero or a handful, all in tests** → add a guard test asserting production
  sources carry no bare `KindId("` outside `domains/thing/src/lib.rs`'s handles
  module, on the model of `thing_kind_of_has_no_wildcard_arm`, which scans its
  own subject's source text.
- **Many, spread through production** → do NOT add the guard in this task.
  Report the count; a guard that must be born with a long allow-list is a guard
  whose list nobody will maintain, and the honest move is a follow-up that
  converts the sites first.

Either way, **write the decision and its count into the ledger** — a guard
declined for a measured reason is a different artifact from a guard nobody
thought of.

- [ ] **Step 2: Run it — green is expected, and is not the evidence**

```bash
cargo nextest run -p hornvale-vessel -E 'test(every_kind_the_grammar_names)'
```

Expected: PASS. A guard written after the code it guards starts green; Step 3
is where it earns its place.

- [ ] **Step 3: Apply the mutation and paste the red**

```bash
python3 scripts/mutate.py windows/vessel/src/interior/pattern.rs \
  'kind: kinds::HIGH_SEAT' 'kind: KindId("high_seat")'
cargo nextest run -p hornvale-vessel -E 'test(every_kind_the_grammar_names)'
```

Copy the failure into the doc comment under `Red observed:`, then
`git checkout -- windows/vessel/src/interior/pattern.rs` and re-run to confirm
green — a restored mutation can leave cargo holding the mutated build.

- [ ] **Step 4: Retire the correspondence test**

Delete `cli/tests/suite/anchor_thing_correspondence.rs` and its `mod` line.
Everything it asserted is now either impossible (the `AnchorKind` → `KindId`
mapping it checked no longer exists) or covered by Step 1's gate plus Task 1's
`every_named_handle_is_a_roster_row`. Its one remaining claim — that
`passage::CAVE_MOUTH` is a roster row — holds by construction once the
re-export in Task 2 Step 1 lands.

**Deleting a test changes the commit gate's roster.**
`docs/timings/subfloor-roster.tsv` selects by exact test name, so a deleted or
renamed test leaves a stale row behind. Do not hand-edit it — the chamber's
`gate` phase rewrites it on the next green run.

- [ ] **Step 5: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "test(vessel): gate that every kind the grammar names is a roster row"
git push
```

---

## Stage 3 — prose becomes a table

### Task 4: `ChamberProse` and the two-way totality gate (G-b, G-c)

The last exhaustive match pair. `noun` and `detail` are two 15-arm matches over
a type that no longer exists after Task 2, so they currently key on `KindId`
with a `match` on string literals — which compiles, and is worse than either
the enum or a table. This task makes them one table and puts the guarantee they
used to get from the compiler into two tests that run in both directions.

**Files:**
- Modify: `windows/vessel/src/chamber_prose.rs`
- Modify: `windows/vessel/tests/suite/kind_totality.rs`

**Interfaces:**
- Consumes: Task 1's handles, Task 2's re-key.
- Produces: `pub struct ChamberProse { pub noun: Option<&'static str>, pub
  detail: &'static str }`; `pub fn chamber_prose_registry() ->
  ComponentStore<KindId, ChamberProse>`; `pub(crate) fn noun(kind: &str) ->
  Option<&'static str>`; `pub(crate) fn detail(kind: KindId) ->
  &'static str`. The accessors keep the signatures Task 2 left them with, so no
  caller outside this file changes.

**The two accessors take DIFFERENT key types, and that is forced, not sloppy.**
An earlier draft of this plan specified `noun(kind: KindId)` and was wrong.
`KindId` holds a `&'static str`, while three of `noun`'s callers read the
ledger — `Ledger::kind_of` returns `Option<&str>` borrowed from a `String` in
the fact store, not `'static`, so it cannot be wrapped in a `KindId` at all.
That is precisely why the retired `noun_for_label` existed. `detail`'s callers
are all interior-side and hold a real `KindId`, so it keeps the typed parameter
and the typo-safety that comes with it. Document the asymmetry **at the table**,
naming both caller sets: a reader who finds two accessors on one table with two
key types will otherwise unify them, and unifying downward loses `detail`'s
safety while unifying upward is impossible.

- [ ] **Step 1: Write the two gates**

Append to `windows/vessel/tests/suite/kind_totality.rs`:

```rust
/// **Direction: rostered ⊆ prosed.** Every kind in `THING_KINDS` has a
/// chamber-prose row, so a kind the grammar may place always has something to
/// be called and something to say when examined.
///
/// This is one half of what the two exhaustive `match`es used to guarantee. It
/// is stated separately from its converse because the cheapest repair to a
/// one-way check is to delete the check, and a pair that must agree in both
/// directions cannot be repaired that way.
///
/// MUTATION THIS MUST FAIL AGAINST: delete the `"log"` row from
/// `chamber_prose_registry()`.
#[test]
fn every_roster_kind_has_chamber_prose() {
    let prose = hornvale_vessel::chamber_prose::chamber_prose_registry();
    for label in THING_KINDS {
        assert!(
            prose.get(&hornvale_kernel::KindId(label)).is_some(),
            "roster names {label:?}, chamber prose does not"
        );
    }
}

/// **Direction: prosed ⊆ rostered.** No chamber-prose row names a kind the
/// roster does not carry — the converse of the check above, and the one that
/// catches a row added for a kind that was renamed or never existed.
///
/// MUTATION THIS MUST FAIL AGAINST: add a `KindId("brasier")` row to
/// `chamber_prose_registry()` (the plausible misspelling of the kind Task 5
/// adds).
#[test]
fn every_chamber_prose_row_is_a_roster_kind() {
    let prose = hornvale_vessel::chamber_prose::chamber_prose_registry();
    for id in prose.ids() {
        assert!(
            THING_KINDS.contains(&id.0),
            "chamber prose has {:?}, the roster does not",
            id.0
        );
    }
}
```

- [ ] **Step 1a: Make Task 3's vacuity guards absolute, and fix a comment that overclaims**

Task 3's review found both vacuity guards in `kind_totality.rs` are **relative,
not absolute**, and that one of them carries a comment claiming more than the
assertion delivers:

```text
  kind_totality.rs:62   assert!(checked >= INVENTORY.len(), "... the loop is not running")
                        -> 0 >= 0 holds if INVENTORY is empty. The comment says this
                           "cannot pass while the loop is not running". It can.
  kind_totality.rs:96   assert!(!reg.is_empty())
                        -> passes with one row where there are ~19
```

Both trace to this plan's own literal code, not to an implementer deviation.
The retired correspondence test did it correctly with an absolute floor
(`object_registry().ids().count() >= 9`), and Task 2 set the better precedent
inside this campaign: `every_kind_the_grammar_places_has_a_detail` closes on
`assert_eq!(checked, 38)`.

Do the same here — pin exact counts, and **correct the comment to describe what
the assertion actually guarantees**. An exact count is a ratchet: Task 5 appends
a pattern and will update it deliberately, exactly as it updates `FROZEN`. That
is the intended cost.

A guard whose comment overstates it is this campaign's own recurring defect
wearing a fourth costume — the reason `SupportsRest` looked like a gate, the
reason two source-scanning guards looked like coverage. Do not add a fifth.

- [ ] **Step 2: Run them and watch them fail to compile**

Run: `cargo nextest run -p hornvale-vessel -E 'test(chamber_prose)'`

Expected: compile error — `chamber_prose_registry` does not exist. Not
evidence; Step 5 captures the real reds.

- [ ] **Step 2b: Delete `chamber_prose`'s test-module `EVERY_KIND`**

Task 2 left a hand-written 15-kind roster in `chamber_prose`'s test module and
documented it as a hazard: it is the exact anti-pattern the deleted
`anchor_kinds!` macro existed to prevent — a list that can go short beside a
sweep that looks total. It could not be replaced in Task 2 because `cave-mouth`
had no `detail` line, which is the gap G-b closes here.

Once every roster kind has prose, delete `EVERY_KIND` and let the tests that
used it sweep `hornvale_thing::THING_KINDS` directly.

- [ ] **Step 3: Build the table**

In `chamber_prose.rs`, replace the two matches with one store. **Move every
noun and detail string across verbatim** — this task changes where the strings
live, never what they say, and a reworded line would be an undeclared prose
change that `a_chamber_never_speaks_of_terrain` and its siblings are not
watching for.

```rust
/// What prose calls a kind, and what `examine` says about it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChamberProse {
    /// How prose names this kind, or `None` for a kind that IS the room rather
    /// than a thing standing in it — the floor. `detail` is total where this
    /// is not, because the plan's legend requires every noun it depicts to
    /// answer and the floor is depicted.
    pub noun: Option<&'static str>,
    /// The line `examine` gives. Short, concrete, and free of terrain words —
    /// `no_detail_speaks_of_terrain` is the guard.
    pub detail: &'static str,
}

/// The chamber-prose rows: one per thing-kind the grammar may place.
///
/// Prose is presentation, so it lives in the window; `hornvale_thing`'s
/// `ThingTraits::display` is the domain's own label and stays there. Two
/// tables answering two questions, not one table answering both — the same
/// split The Chattel's Task 7 made when it deleted `ThingTraits::portable`
/// rather than letting two tables answer "may a body carry this".
pub fn chamber_prose_registry() -> ComponentStore<KindId, ChamberProse> {
    // ... one row per kind, strings moved verbatim from the retired matches
}
```

The two accessors become lookups over it, and **`detail` must refuse rather
than default**:

```rust
pub(crate) fn detail(kind: KindId) -> &'static str {
    chamber_prose_registry()
        .get(&kind)
        .map(|p| p.detail)
        .unwrap_or_else(|| panic!("no chamber prose for kind {:?}", kind.0))
}
```

The panic is the design, not laziness: `every_roster_kind_has_chamber_prose`
makes it unreachable from the authored inventory, and the refusal is what makes
the unreachable case loud if that gate is ever weakened. A `unwrap_or("A
featureless thing.")` would put a plausible sentence in a real room forever.

- [ ] **Step 4: Consider the allocation, and measure before optimising**

`chamber_prose_registry()` builds a `BTreeMap` per call, and `detail` is called
per anchor per render. If the duration baseline moves, hoist the store to a
`OnceLock` or thread it through the caller — but **read the baseline first**.
Do not restructure on suspicion; the sub-floor tier's own execution is seconds
and 15 short-string comparisons are unlikely to show.

- [ ] **Step 5: Verify both mutations redden, paste the reds, revert**

Use `scripts/mutate.py`. Confirm green after each revert.

- [ ] **Step 6: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "refactor(vessel): chamber prose becomes a KindId-keyed component table"
git push
```

---

## Stage 4 — the proof, and the corrections

### Task 5: The brazier

One kind, five data rows, one appended pattern, no control flow. The diff IS
the deliverable: if it touches an enum, a match arm or a dispatcher, the
campaign has not done what it claims.

**Files:**
- Modify: `domains/thing/src/lib.rs` (roster row, registry row, handle, FROZEN)
- Modify: `windows/vessel/src/chamber_prose.rs` (prose row)
- Modify: `windows/vessel/src/affordance.rs` (`object_registry` row)
- Modify: `windows/vessel/src/interior/pattern.rs` (appended `Pattern`, array
  length 16 → 17)
- Test: `windows/vessel/tests/suite/affordance.rs`

**Interfaces:**
- Consumes: everything above.
- Produces: `hornvale_thing::kinds::BRAZIER`.

- [ ] **Step 1: Measure loomroom reachability BEFORE adding anything**

Write a throwaway probe (do not commit it) that builds worlds at the three
census seeds and counts chambers whose `role_for` is `Role::Loomroom`. Report
the counts.

**This is expected to CONFIRM, not discover.** `interior/pattern.rs` already
records a 48-seed sweep finding `Role::Loomroom` at chamber index 2 in 24 of 24
structures that have an index 2, with `Smithy`, `Hall` and `Shrine` at zero. An
earlier draft of this task put the brazier in a shrine, which that sweep had
already measured as empty. Run the probe anyway: a measurement you expect to
pass is the one you skip, and skipping this one is what produced the error.

**The prior evidence, and why the role moved.** An earlier draft of this task
put the brazier in a shrine. `interior/pattern.rs` already carried a 48-seed
sweep, run through `possess --seed N --script` by an earlier campaign, finding
the role at chamber index 2 is `Role::Loomroom` in **24 of 24** structures that
have an index 2 at all, and that `Role::Smithy`, `Role::Hall` and `Role::Shrine`
occur **zero** times — `role_for` reaches those three only through
`Function::Mine | Function::Fort`, `Notability::Seat` and `Function::Cult`, and
no flagship a possession starts at carries one. That comment states the
consequence outright: *a gate whose predicate is false everywhere is not a gate,
it is a deletion.* A brazier in a shrine would have been a brazier in no world —
decision 0398's failure, shipped by the campaign that cites 0398.

So measure **two** things, because 0398 is about reachability and not existence:

- **(a) Existence:** how many `Role::Loomroom` chambers exist at the three
  census seeds.
- **(b) Reachability:** whether a possession can actually get to one and delve
  it.

Branch on the pair:
- **Both non-zero** → proceed as written. This is the expected outcome; the
  48-seed sweep predicts it.
- **(a) non-zero, (b) zero** → the brazier exists and no player meets it. Report
  it and stop; the campaign needs a different proof kind, and the fact that the
  loomroom is unreachable would contradict a committed measurement, which is a
  finding in its own right.
- **(a) zero** → STOP and report. That contradicts the 48-seed sweep directly,
  so either the sweep has rotted or the probe is wrong — settle which before
  going further.

Whatever the numbers, **write them into the report**. A null here is a result,
and it would be a bigger one than the brazier: it would mean a committed
48-seed measurement no longer holds.

`Role::Shrine` is drawn at chamber index 2 when the place's history function is
`Function::Cult` and its notability is not `Seat`, so the count is a real
question, not a formality.

- [ ] **Step 2: Write the failing test**

In `windows/vessel/tests/suite/affordance.rs`:

```rust
/// A brazier offers `warm`, and it is the first carrier of `RadiatesHeat`
/// outside a hearthroom. Before The Wicket this kind could not exist: `warm`
/// reads whichever anchor is present through `offered_to_observer`, but the
/// only kinds an anchor could BE were the enum's fifteen.
///
/// MUTATION THIS MUST FAIL AGAINST: drop `ObjectProperty::RadiatesHeat` from
/// the `brazier` row in `object_registry()`.
#[test]
fn a_brazier_offers_warm_with_no_dispatcher_edit() {
    use hornvale_thing::kinds;
    assert!(offered_by(kinds::BRAZIER).contains(&OfferedVerb::Warm));
    assert!(!offered_by(kinds::ALTAR).contains(&OfferedVerb::Warm));
}
```

- [ ] **Step 3: Add the five rows**

1. `domains/thing`: `"brazier"` into `THING_KINDS` (alphabetical position, after
   `"bed"`), into `FROZEN` in the same edit, a `thing_registry` row with
   `display: "brazier"`, and `kinds::BRAZIER` plus its `EVERY_HANDLE` entry.
2. `chamber_prose_registry`: `noun: Some("a brazier")`, and a detail line in
   the register of the existing ones — short, concrete, no terrain words.
3. `object_registry`: `(kinds::BRAZIER, traits(&[ObjectProperty::RadiatesHeat]))`.
4. `INVENTORY`: appended **after `the-loom`**, and the array length becomes
   `[Pattern; 17]`.

```rust
    Pattern {
        name: "the-brazier",
        kind: kinds::BRAZIER,
        attach: Attach::Beside(kinds::LOOM),
        requires: Some(kinds::LOOM),
        needs_cold: false,
        built: true,
        roles: &[Role::Loomroom],
        at_locale: false,
        needs_populous: false,
    },
```

**Append, never insert.** `draw` admits a pattern only once its `requires` kind
is present, so the order IS the dependency order: a brazier placed before the
loom it requires would be silently dropped from every composition, and
reordering is an epoch outright. `at_locale: false` keeps the append LATENT per
`INVENTORY`'s own three-part rule — the chamber renderer reads it and nothing
that commits does.

- [ ] **Step 4: Confirm the diff's shape**

```bash
git diff --stat
```

Five files, data rows only. If the diff contains a new `match` arm, an `if`
on a kind, or a dispatcher edit, **the campaign's central claim is false** and
that is the finding to report — not something to work around.

- [ ] **Step 5: Read the artifact branch table**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

- **Nothing moved** → proceed; say so.
- **`clients/game/core/tests/fixtures/` moved** → the chamber-band session
  snapshot picked up the new anchor. Refresh and commit in the same commit.
- **`book/src/gallery/` moved** → **PROCEED, and say so loudly.** This branch
  read STOP in an earlier draft and that was a defect: the rule was imported
  from Task 2, whose premise is that *nothing* should change, into a task whose
  premise is that *one thing* should. A possession transcript gaining the new
  kind is the campaign's success criterion made visible — decision 0398
  satisfied in a committed artifact rather than argued in a spec. Refresh and
  commit in the same commit.
- **A census CSV or `book/src/domesday/` moved** → STOP. Those are folds over
  the LEDGER, and a chamber's contents must never reach it (decision 0069 keeps
  `Interior` unserialized). Movement there means a chamber is being committed,
  which is a real epoch and the campaign's headline, not a rebaseline.

**The distinction those two branches turn on, stated once:** a *rendered
artifact* is not the *ledger*. A gallery transcript is regenerated from a live
walk and may show anything the world now contains; a census column is a fold
over committed facts and must not move because a room gained furniture.
- **A generated file moved that is on none of these branches** → the
  enumeration is not exhaustive, so ask the question that decides it: *is this
  file derived from the WORLD, or from the SOURCE TREE?* A source-derived page
  (the layering diagram, the type-audit report, a manifest dump) moving for a
  reason you can state in one sentence is expected — regenerate, commit it in
  the same commit, and name the reason. A world-derived artifact moving is the
  STOP branch whether or not it appears in a list. Task 2 hit exactly this with
  `book/src/reference/layering-generated.md`, which gained vessel's new
  dependency row.

- [ ] **Step 6: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(vessel): a brazier warms a shrine -- five data rows, no dispatcher edit"
git push
```

---

### Task 6: The three corrections and the ungated-sleep tripwire

Spec §6a. Small, and it is the guard that keeps stage 5 honest.

**Files:**
- Modify: `windows/vessel/src/affordance.rs` (the `OfferedVerb::Sleep` doc)
- Modify: `windows/vessel/src/action.rs` (the `Action::Rest` doc)
- Modify: `windows/vessel/src/affordance.rs` (the `object_registry` doc's stale
  claim about `key` and `cave-mouth`)
- Test: `windows/vessel/tests/suite/affordance.rs`

- [ ] **Step 1: Write the tripwire**

```rust
/// Sleeping is NOT gated on an object, and this test exists so it stays that
/// way. `Session::sleep` refuses a non-empty argument, charges the clock,
/// commits `rested` and sets `wake_at`; it asks nothing about the room.
///
/// The tripwire is for a specific future mistake: someone reads
/// `OfferedVerb::Sleep`'s doc, sees "gates on SupportsRest", and makes the
/// verb honour it. At that point a magically-slept target walks off to find a
/// bed, because the two routes into sleep — the voluntary act and an imposed
/// effect — would share a gate that only one of them chose.
///
/// MUTATION THIS MUST FAIL AGAINST: add an early return to `Session::sleep`
/// refusing when no anchor in the current interior offers `OfferedVerb::Sleep`
/// (the "fix" this test exists to reject).
#[test]
fn sleeping_needs_no_bed() {
    // Stand a session in a room with no rest-affording anchor, `sleep`, and
    // assert the reply is SLEEP_REPLY rather than a refusal. Build the room
    // through the same seam the other session tests in this file use.
}
```

The body is left to the implementer **on purpose**: the session-construction
seam these tests use is a detail of this file, and prescribing it from outside
the code is how a plan ships a test that cannot run. Read a neighbouring
session test in the same file and follow it.

- [ ] **Step 2: Correct the three comments**

Each correction states what the sentence used to say, that it was false, and
why the property is still there — not a silent edit. The pattern is CLAUDE.md's
own loud-correction discipline: a record that outlives its subject produces
wrong answers from readers acting in good faith.

- `OfferedVerb::Sleep` — "gates on `SupportsRest`" becomes: `SupportsRest`
  reaches the **advertisement** layer only. `required_properties(Sleep)`
  decides which objects list `sleep`; nothing decides whether sleeping is
  allowed, because sleeping is always allowed. The property is a GRADE filed
  among gates, and `PSY-rest-quality-is-a-grade-not-a-gate` is where that goes.
- `Action::Rest` — "precondition: at home" becomes: no at-home precondition is
  enforced anywhere. `precondition_reads_committed_state` already answers
  `false` for `Rest`, and the creature layer's fatigue drive says outright that
  a creature *sleeps where it is*.
- `object_registry`'s doc — "`key` and `cave-mouth` are the first rows with no
  `AnchorKind` behind them at all" was true at The Chattel's Task 7 and stopped
  being true at its Task 11, which gave `Key` a variant. After The Wicket there
  is no `AnchorKind` at all, so the sentence is replaced by what it was
  reaching for: `cave-mouth` is the row that was never an anchor, and it is the
  openness the campaign generalised.

- [ ] **Step 3: Verify the mutation reddens, paste the red, revert**

- [ ] **Step 4: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "docs(vessel): correct three comments describing a gate nothing enforces"
git push
```

- [ ] **Step 5: STAGE GATE — submit to the sluice**

```bash
git rev-parse HEAD
make sluice-stage BRANCH=campaign/the-wicket REF=<the full sha just printed>
```

Verify the SHA with `git cat-file -e <sha>^{commit}` rather than transcribing
it — `rev-parse` echoes any 40-hex string back at you.

This is the last boundary at which the tree is artifact-clean, so anything the
queue reddens is attributable to the re-key alone. Wait for the result
(`make sluice-status`, `make sluice-log`) before starting Task 7. **A stage gate
never pushes main**; it merges main into the branch in the chamber and gates
that product.

---

## Stage 5 — the acts

Spec §6b. This is where committed history moves. Nothing before this task
changes what a world contains; everything from here does.

### Task 7: Fatigue becomes a recovery stock, not a flag

The load-bearing one. `fatigue_at` is `FATIGUE_RISE * (t - last_rested)`
clamped — time since the most recent `rested` fact — so **any** rest zeroes the
debt and there is no representation of how much sleep a body got. Until that
changes, "rest gives some benefit, sleep gives more" cannot be said, and the
act split in Task 8 would be cosmetic.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`fatigue_at`, `Fatigue`,
  `decide_step`'s own fatigue arithmetic)
- Modify: `windows/vessel/src/session.rs` (`rested_fact`)
- Test: `windows/vessel/tests/suite/` (a new file, `fatigue_stock.rs`)

**Interfaces:**
- Produces: `fatigue_at(ledger, entity, t) -> f64` keeps its signature and its
  `[0, 1]` range. What changes is the fold behind it.

- [ ] **Step 1: Read before designing — the plan does not prescribe the form**

Read, in this order, and report what you found before writing code:

1. `fatigue_at` and the FOLD doctrine comment above it, which explains why the
   read and the mover must compute fatigue the same way and how they once
   diverged by an ULP on 56.7% of tick pairs.
2. `decide_step`'s own fatigue arithmetic — the mover half of that pair.
3. Whatever advances the clock for a creature's `Action::Rest`, which is where
   a rest's DURATION has to come from.
4. `rested_fact` and what the `rested` fact carries today.

The properties the new model must have are fixed (Step 2). **The form is
yours**, because it depends on facts about the creature tick that this plan's
author could not read off the code with confidence, and a prescribed shape
would be a guess dressed as a requirement.

- [ ] **Step 2: The properties, as tests, written first**

Create `windows/vessel/tests/suite/fatigue_stock.rs`. Each of these is a
property the model must have; write them as assertions before touching
`liveness.rs`:

```text
  P1  fatigue is still a pure FOLD over committed facts — recomputing it at t
      from the ledger gives the same answer twice, and gives the same answer
      after a reload, with no state held anywhere
  P2  fatigue still ranges [0, 1] and still rises with time awake
  P3  a SHORT rest does NOT zero the debt — this is the property today's model
      cannot express, and the one that makes the split real
  P4  a LONGER rest restores strictly more than a shorter one of the same kind
  P5  the read (`fatigue_at`) and the mover (`decide_step`) agree exactly on
      inputs measured to disagree under a naive float shape — the existing
      pairing test `a_fatigue_read_matches_the_walks_own_fatigue_arithmetic` is
      the model; extend it rather than writing a second one
  P6  fatigue never goes negative however much a body sleeps
```

P3 is the discriminating one. State its mutation explicitly: **revert
`fatigue_at` to the old `RISE * (t - last_rested)` shape** and P3 must redden.
If it does not, P3 is not testing what it claims and the model has not changed.

- [ ] **Step 3: Implement, run, iterate**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-wicket-t7.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-wicket-t7.log
```

Expect unrelated tests to move: this changes when creatures rest, which changes
committed history. **A test whose expected value must change is a finding to
report, not a chore** — say which, and why the new value is right, in the task
report. Do not batch-update expectations.

- [ ] **Step 4: Read the artifact branch table**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

Unlike every earlier task, **a moved census column here is expected, not a
stop**. Nathan ruled the refresh a normal pre-alpha cost. What still stops the
task: a moved column with no plausible mechanism, or a moved column in a domain
this change cannot reach (astronomy, terrain, language). Report the list of
what moved with a one-line mechanism for each; a column you cannot explain is
the finding.

- [ ] **Step 5: Format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(vessel): fatigue becomes a recovery stock rather than a flag cleared by any rest"
git push
```

---

### Task 8: `Action::Sleep` beside `Action::Rest`

**Files:**
- Modify: `windows/vessel/src/action.rs`
- Modify: `windows/vessel/src/session.rs` (`sleep`; `wake_at` moves onto the act)
- Modify: `windows/vessel/src/liveness.rs` (the fatigue drive's proposal)
- Test: `windows/vessel/tests/suite/action_module.rs`,
  `windows/vessel/tests/suite/action_mood.rs`

**Interfaces:**
- Produces: `Action::Sleep`, `Mood::InCharacter`, `concept_name() == "sleep"`.

- [ ] **Step 1: Add the variant and let the compiler enumerate**

`action.rs` carries **no `_` arm in any of its matches** — verified on this
tree with `grep -n '^\s*_ =>' windows/vessel/src/action.rs`, which returns
nothing. So adding the variant makes `mood`, `precondition_reads_committed_state`
and `concept_name` fail to compile until each is answered, and
`action_variants_must_all_be_rostered` fails until `all()` lists it.

**Re-run that grep before relying on this.** A pre-existing catch-all arm voids
the compiler's enumeration silently, and this plan's claim is a measurement with
a date on it.

```rust
    /// Sleep (precondition: none — a body sleeps where it stands; effect:
    /// unconsciousness until its own cycle wakes it, and the larger of the two
    /// fatigue recoveries). Distinct from [`Action::Rest`], which stays
    /// conscious and watchful and restores less. The Wicket, spec §6b.
    Sleep,
```

- [ ] **Step 2: Move unconsciousness off the player path and onto the act**

Today `Session::sleep` sets `wake_at` itself, so unconsciousness is a property
of the player's route rather than of the act — which is why an NPC proposing
`Action::Rest` never goes under by that route. After this task, `Action::Sleep`
carries it, and both routes reach the same state.

`concept_name()` returns `"sleep"`, which is already a registered language
concept (`domains/language/src/accession.rs`, `packs.rs`), so no concept, cohort
or accession entry moves. **Verify that rather than assuming it** — run the
orphan-acts audit and confirm it is quiet.

- [ ] **Step 3: The three acts, asserted apart**

In `action_module.rs`, one test per distinction, so a failure names which
distinction broke:

```text
  waiting changes nothing about the body that time alone would not
  resting leaves the body conscious and restores some fatigue
  sleeping renders the body unconscious and restores strictly more than
    resting for the same span
```

The third is the one that would silently pass if Task 7 had not landed;
assert the strict inequality, not merely that both are non-zero.

- [ ] **Step 4: Confirm `wait` was left alone**

`Session::wait` needs no change — it parses a span, advances the clock, runs
the NPC tick, and has no `Action` variant at all, which is the right shape for
an act that transforms nothing. Assert that rather than trusting it: a test that
`wait` moves the clock and moves no fatigue beyond what the elapsed time itself
accrues.

- [ ] **Step 5: Artifact branch table (as Task 7), then format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(vessel): Action::Sleep splits from Action::Rest"
git push
```

---

### Task 9: The rate becomes a per-species row on the planetary clock

Two defects in one constant. `FATIGUE_RISE = 0.3` is one sleep-debt rate for
every species in every world, and `fatigue_at` multiplies it by
`.as_std_days()` where the quantity is per **planetary** day.

**Files:**
- Modify: `domains/species/src/lib.rs` (a new kind-keyed registry)
- Modify: `windows/vessel/src/liveness.rs`
- Test: `windows/vessel/tests/suite/fatigue_stock.rs`

**Interfaces:**
- Produces: a `ComponentStore<KindId, _>` in `domains/species` carrying the
  per-species sleep-debt rate. Follow the shape of the registries already
  there (`biosphere_registry`, `psyche_registry`, `perception_registry`) —
  read one before writing this, and match its idiom rather than inventing one.

- [ ] **Step 1: The rate is a row**

A species with **no row does not sleep**. That is the "(for most species)
mandatory" half of the ruling falling out of the table for free — a constant
could not express the exception at all, and an `Option` field would have made
"does not sleep" and "not yet authored" the same value. Absence is the
statement.

Humans keep ~1/3 per planetary day, which is what `0.3` already approximately
was; do not silently re-tune other species while moving the number. **Any
species whose rate is not 1/3 is a fidelity decision and goes to Nathan**, not
into this task.

- [ ] **Step 2: The clock becomes local**

`domains/astronomy/src/units.rs` already carries `LocalDays`, `StdDays`,
`in_local(day_length: StdDays)` and `in_std(day_length)`. The debt accrues per
planetary day, so the conversion needs the world's day length; read where the
session and the creature tick can each reach it before choosing where to do the
conversion, and do it in **one** place — the read and the mover must still agree
exactly (Task 7's P5).

- [ ] **Step 3: The discriminating test**

```text
  two worlds whose day lengths differ, same species, same elapsed STANDARD
  time -> different accrued sleep debt, and the ratio is the ratio of their
  day lengths
```

This is the test the old model could not pass and would not have been written
for, because with `as_std_days()` the two worlds agree exactly. Name that as the
mutation: **revert the conversion to `as_std_days()`** and this must redden.

- [ ] **Step 4: Artifact branch table, format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(species): the sleep-debt rate is a per-species row on the planetary day"
git push
```

---

### Task 10: Rest reads the affordances present

The object half of spec §6a's grade, and the first consumer of the property
rows this campaign made reachable from a room.

**Files:**
- Modify: `windows/vessel/src/liveness.rs`
- Test: `windows/vessel/tests/suite/fatigue_stock.rs`

- [ ] **Step 1: Recovery scales with what the room affords**

A rest or sleep taken where an anchor carries `SupportsRest` restores more than
one taken on bare ground. Read the room through the same query every other
surface reads — `affordance::offered_to_observer` — and not by comparing kinds:
`Session::warm`'s own history is the precedent, where a hardcoded
`AnchorKind::Hearth` literal was replaced by the offer for exactly this reason,
and reintroducing the pattern here would be the M×N dispatcher coupling the
campaign exists to abolish.

- [ ] **Step 2: What this task does NOT build**

Not *what a people tends to sleep on* — that is a `(species, thing)` edge,
addition two, and it is out of scope by Nathan's stopping line. Not *this one
likes a sleeping bag* — that is a `Lineage`-derived per-instance value,
addition three. Both are recorded in
`PSY-rest-quality-is-a-grade-not-a-gate`. If either starts to feel necessary to
finish this task, stop and report: that is the stopping line moving, which is
Nathan's call and not the implementer's.

- [ ] **Step 3: The test**

```text
  the same species, the same span, two rooms -> the one with a SupportsRest
  anchor present restores strictly more
```

Mutation: make the recovery ignore the room. It must redden.

- [ ] **Step 4: Artifact branch table, format, gate, commit, push**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(vessel): rest recovers more where the room affords rest"
git push
```

- [ ] **Step 5: STAGE GATE — submit to the sluice**

```bash
git rev-parse HEAD
make sluice-stage BRANCH=campaign/the-wicket REF=<the full sha just printed>
```

The boundary Nathan asked for. Stage 4's gate established a clean baseline, so
anything this one reddens has one candidate cause — the acts — rather than two.
Wait for the result before starting Task 11.

---

## Stage 6 — close

### Task 11: Definition of done

- [ ] Chronicle entry `book/src/chronicle/the-wicket.md`, plus a freshness
      sweep of chapters this campaign moved.
- [ ] **Correct the frontier essay and the registry row.** `frontier.md`'s
      "thirty-four variants, thirty-six exhaustive match sites, seventeen
      files" becomes the measured 15 / 3 / 18, with a note that it was wrong at
      authoring rather than stale — the loud-correction discipline, because a
      published measured fact that is wrong produces wrong cost estimates from
      readers acting in good faith. `MAP-one-kind-model` carries the same
      correction and moves to reflect that addition one has shipped, with
      addition two carrying the inheritance argument and its new named
      consumer (sleep quality).
- [ ] Re-score any Confidence Gradient bet this campaign moved
      (`book/src/open-questions.md`), per decision 0030.
- [ ] Retrospective `docs/retrospectives/the-wicket.md`, and its row in
      `docs/retrospectives/README.md`.
- [ ] Decision records: **(a)** totality by registry, and the direction each
      check enforces; **(b)** a handle is a convenience where a variant was a
      requirement; **(c)** sleep is never gated — the place grades it (Nathan's
      ruling, and the one that binds future campaigns touching rest, the sleep
      spell, or the property vocabulary). Reserve numbers with
      `make decision-block` — max+1 is wrong and collides silently.
- [ ] The four idea-registry rows are already written; confirm they still read
      true against what shipped, and amend rather than leave them aspirational.
- [ ] `make rebaseline`, and the census refreshed once on lefford.
- [ ] Merge: `make sluice BRANCH=campaign/the-wicket REF=<full sha>`. The
      merge refuses without a `Sluice-Headline:` trailer in the same block as
      `Claude-Session:`, no blank line between them.
- [ ] Release the worktree — never remove it; removal destroys a warm
      `target/` that costs ~771 s to rebuild.
