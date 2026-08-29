# The Chattel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale objects that can be held — entities whose identity
travels independently of their position — and unify passages, containers and
portables under one open/close mechanism.

**Architecture:** An anchor is a derived region of a room and stays free until
something touches it, at which point it **promotes** to an entity whose
position is its *room*. Identity is a pure function of `Lineage(facet, kind,
ordinal)`, so an object can be named, compared and matched before any fact
about it exists; only a change pays. Durable state — where a thing is, whether
it is open — is a fold over committed facts evaluated at the instant asked
about, inheriting decision 0366's rule whole.

**Tech Stack:** Rust 2024, std only. `serde`, `serde_json`, `libm` are the
entire external allowlist. No `HashMap`/`HashSet`. `cargo nextest` for tests.

**Spec:** `docs/superpowers/specs/2026-08-28-the-chattel-design.md` — the plan
argues from the spec; executors read both.

## Global Constraints

- **Dependencies:** `ALLOWED_EXTERNAL` is `["libm", "serde", "serde_json"]`
  (`cli/tests/suite/architecture.rs:11`). A new domain crate may depend on
  `hornvale-kernel` and nothing else — never a sibling domain.
- **No wall-clock time.** Time is `WorldTime { ticks: i64 }`. Main moved 23
  commits before this campaign started; The Precedence retyped every walk
  instant in `windows/vessel` to `WorldTime`, so any `f64`-day signature
  inherited from an older spec is stale.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced
  workspace-wide by `clippy.toml`.
- **`#![warn(missing_docs)]`** in every crate; every public item, field and
  variant gets a one-line doc comment.
- **`cargo fmt` is the last step before every commit.** fmt-gate skips are
  this project's most common review finding.
- **Clippy runs with `--all-targets -D warnings`** in `gate-commit`, so an
  unused import is a hard failure. Import only what the task's own code uses.
- **Every task regenerates and commits its own artifacts in its own commit.**
  There is no terminal sweep that fixes earlier tasks' drift; the final task
  asserts the diff is empty, which is a *finding* about earlier tasks.
- **Every regression test names the mutation it must fail against and pastes
  the red in its doc comment** (decision 0353). **No mutation is prescribed by
  this plan** — each task names the *property* a mutation must demonstrate and
  the implementer finds one from inside the code. Use `scripts/mutate.py`,
  which substitutes only if the target is found and unique.
- **A test that needs a private `session.rs` seam lives in `session.rs`'s own
  `mod tests`.** `windows/vessel/tests/suite/` is an integration crate and
  cannot reach private module items. The Latch lost a task to a brief that
  demanded both at once.
- **Run a suite ONCE and grep the captured file.** Never re-run to ask a
  second question; this workspace's wall time is dominated by test execution.
- **Absorb main at every stage boundary** with
  `make sluice-stage BRANCH=campaign/the-chattel REF=<full-sha>`. Push at
  every task boundary — an unpushed branch is invisible to the mouth, the
  chamber and every peer.
- **Commit with `git commit -F <file>`,** never `-m "$(cat <<'EOF' ...)"`. A
  heredoc breaks on apostrophes even with a quoted delimiter; this project has
  hit it twice in one session. Never put backticks in a commit message body
  written through shell interpolation.

## File Structure

**Created:**
- `domains/thing/Cargo.toml`, `domains/thing/src/lib.rs` — the object domain:
  `ThingTraits` and the authored kind roster. Depends on `hornvale-kernel`
  only. Draws nothing (the `domains/alchemy` model: no `streams.rs`, no
  `StreamLabel`, no `Seed` parameter).
- `windows/vessel/src/thing.rs` — identity, promotion, the `located-in` and
  `openness` folds, latency. The vessel-side half, sibling to `passage.rs`.
- `windows/vessel/tests/suite/thing.rs` — integration tests over the public
  surface.

**Modified:**
- `kernel/src/registry.rs` — one new `ConceptKind` variant.
- `windows/worldgen/src/components.rs` — `WorldComponents` gains the thing
  store; `kinds()` unions it.
- `windows/vessel/src/affordance.rs` — `ObjectTraits` re-keys from
  `AnchorKind` to `KindId`; three new `ObjectProperty` variants;
  `offered_to_observer` re-keyed.
- `windows/vessel/src/session.rs` — six verbs (dispatch arm +
  `IN_CHARACTER_VERBS` + `HELP` each), and the genesis registration.
- `windows/vessel/src/passage.rs` — `passage-cleared` retires into `openness`.
- `windows/vessel/src/focalize.rs` — `Noun` gains `entity: Option<EntityId>`.
- `windows/vessel/src/snapshot.rs` — `NounEntry` gains `affordances`.
- `windows/vessel/src/interior/pattern.rs` — a pattern that places a thing
  `Within` a container.

Root `Cargo.toml` is **not** in this list: `members` is a glob (`domains/*`),
so the new crate joins the workspace with no edit.

**Byte-goldens and generated artifacts that move:** spec §8 is the list. Two
need naming here because nothing routine writes them:
`cli/tests/fixtures/world-seed-42.json` (**`make rebaseline-goldens` only** —
`make rebaseline` never writes it, and neither guarding test is in the
subfloor roster, so `gate-commit` compiles them and never runs them) and
`book/src/reference/layering-generated.md` (written by
`cli/tests/suite/architecture.rs`, which discovers crates from `cargo
metadata`, so a new domain moves it automatically).

---

### Task 1: Measure the two things the design rests on, before building on them

The spec's §3.2 and §3.4 each rest on an unmeasured claim. This task settles
both and may move the cut. **It is expected to produce a report, not code**,
and it is dispatched as a task that might correctly conclude the design needs
changing.

**Files:**
- Create: `.superpowers/sdd/task-1-report.md` (git-ignored scratch)
- Read: `windows/vessel/src/interior/pattern.rs`,
  `windows/vessel/src/interior/derive.rs`, `kernel/src/ledger.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: two answers Tasks 4 and 6 are written against — the ordinal rule,
  and the measured cost of one latent-slot position read.

- [ ] **Step 1: Answer the ordinal question by census, not by reading**

The spec's §3.2 branch table turns on: *does any production room place two
anchors of one kind?* The lineage's `ordinal` distinguishes them, and its only
obvious source is the interior's `Vec` derivation order — which is fine-layer
and which decision 0069 licenses to change forever.

What is already known and is **not** the answer: `INVENTORY`
(`windows/vessel/src/interior/pattern.rs:144`) holds 14 patterns, and exactly
one `AnchorKind` appears twice — `Ground`, via `the-ground` (`built: true`)
and `the-clearing` (`built: false`). `compose` (`pattern.rs:438`) does **not**
dedupe: it pushes every selected pattern and keeps only the *first* of each
kind as an attachment target. So two same-kind anchors in one room are
representable, and whether any production combination produces them is the
open question.

Census every production combination the way The Offer's Task 6 did — both
`selection(built, cold)` and `selection_for(...)` across every `Role` in
`EVERY_ROLE` — and report, per combination, the multiset of `AnchorKind` in
the composed `Interior`.

- [ ] **Step 2: Apply the branch table**

```
  measurement                       response
  -------------------------------   -------------------------------------------
  no combination composes two       the ordinal is always 0. Write the invariant
  anchors of one kind               as a test over the same census, and record
                                    that Task 4 may hardcode 0.
  some combination does, but only   the ordinal is 0 for every PROMOTABLE kind.
  for kinds nothing can promote     Name them, and say in the report why
  (Ground, the hub, is the likely   promotability is the right filter.
  case)
  some combination composes two     STOP. A stable ordering rule is needed and
  of a PROMOTABLE kind              must not key on derivation order. Report the
                                    finding and return to the spec's SS3.2 -- do
                                    NOT choose a rule inside this task. A
                                    silently unstable ordinal is a wrong entity
                                    id, and no gate in this tree can see one.
```

- [ ] **Step 3: Measure the read the negative fold will do**

§3.4's latency check does one indexed position read per latent slot per room
entry. There are no `located-in` facts yet, so measure the two factors
separately and report both:

1. **Slots per room** — from Step 1's census, the anchor count per composed
   interior (min / median / max).
2. **Indexed read cost on a real ledger** — `Ledger::latest_value_of`
   (`kernel/src/ledger.rs:511`) against a played session's ledger, using
   `AGENT_AT` as the stand-in high-traffic predicate. Report facts-per-
   predicate and wall time per read.

**Do not inherit 0366's "a read costs a scan" in either direction.**
`Ledger::find` (`:388`) consults `positions_for_predicate` when the index is
present, so the cost is one predicate's own traffic rather than world history
— which makes the warning smaller than its prose, and still real, because this
campaign reads far more often than The Latch did. The report states the
measured number, not either adjective.

- [ ] **Step 4: Report**

Write `.superpowers/sdd/task-1-report.md` with both answers, the exact commands
run, and their real output pasted. If Step 2 hit the STOP row, that is the
report's first line and the task ends there.

- [ ] **Step 5: Nothing to commit**

This task writes only to git-ignored scratch. Report back instead of
committing.

---

### Task 2: The `domains/thing` crate and its kind roster

**Files:**
- Create: `domains/thing/Cargo.toml`, `domains/thing/src/lib.rs`
- Test: `domains/thing/src/lib.rs`'s own `mod tests`
- Regenerate: `book/src/reference/layering-generated.md`,
  `docs/audits/type-audit-report.md`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `pub struct ThingTraits` — the per-kind trait row.
  - `pub fn thing_registry() -> ComponentStore<KindId, ThingTraits>`
  - `pub const THING_KINDS: &[&str]` — the authored kind labels.
  - `WorldComponents.thing`, and `kinds()` unioning it — see Step 7a.

- [ ] **Step 1: Understand what this task does NOT do**

The property *vocabulary* stays in `windows/vessel/src/affordance.rs` until
Task 7, which re-keys `ObjectTraits` from `AnchorKind` to `KindId` and merges
the two tables. Splitting it this way keeps this task's diff to "a new crate
exists and the workspace still builds" — a gate a reviewer can actually hold,
which a simultaneous re-keying would swamp.

So `ThingTraits` carries only what a later task in this plan reads: whether
the kind is portable, and its display label. `MaterialTraits`
(`domains/terrain/src/lib.rs`) is the model for the shape, and its own doc
calls its field set "thin and honest."

- [ ] **Step 2: Write the failing test**

In `domains/thing/src/lib.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// The roster is non-empty and every label is unique — a duplicate label
    /// would make two kinds share one `KindId`, and every fact about either
    /// would key to the same row.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: (name the duplicate you introduced,
    /// and paste the red here once you have run it)
    #[test]
    fn thing_kind_labels_are_unique() {
        let mut seen = std::collections::BTreeSet::new();
        for label in THING_KINDS {
            assert!(seen.insert(*label), "duplicate thing-kind label {label:?}");
        }
        assert!(!THING_KINDS.is_empty(), "the roster may not be empty");
    }

    /// Every label in the roster has a row in the registry, and the registry
    /// has no row the roster does not name — the two-way agreement decision
    /// 0261 requires of a rule written down twice.
    #[test]
    fn roster_and_registry_agree_in_both_directions() {
        let reg = thing_registry();
        for label in THING_KINDS {
            assert!(reg.get(&KindId(*label)).is_some(), "roster names {label:?}, registry does not");
        }
        for id in reg.ids() {
            assert!(THING_KINDS.contains(&id.0), "registry has {:?}, roster does not", id.0);
        }
        // `ComponentStore` (kernel/src/component.rs) exposes `get(&K)`,
        // `contains(&K)`, `ids()`, `iter()`, `len()`, `is_empty()` and
        // `get_by_label(&str)`. Use those; do not add an accessor for a test.
    }
}
```

- [ ] **Step 3: Run it and watch it fail**

Run: `cargo test -p hornvale-thing`
Expected: FAIL — the crate does not exist yet, so this is a *build* failure.
That is acceptable here **only** because the type under test does not exist;
where behaviour is being asserted, a red from a compile error proves nothing.

- [ ] **Step 4: Create the crate**

`domains/thing/Cargo.toml` copies `domains/alchemy/Cargo.toml`, with
`hornvale-kernel` as the only dependency:

```toml
[package]
name = "hornvale-thing"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale thing domain: object kinds and their traits."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

**Do NOT edit root `Cargo.toml`.** Its `members` is a glob —
`["kernel", "domains/*", "windows/*", "cli"]` — so a new directory under
`domains/` joins the workspace automatically. An earlier draft of this plan
told you to add the path by hand and listed root `Cargo.toml` as a modified
file; both were wrong, and adding a redundant explicit member alongside the
glob is the kind of edit that reads as intentional forever after.

The module doc states, in the `domains/alchemy` register, that this domain
**draws nothing** — no `streams.rs`, no `StreamLabel`, no `Seed` parameter —
because what a *kind* is does not vary by world; which kinds a world places
does, and that is derived at the composition root.

- [ ] **Step 5: Author the roster**

The minimum this plan's later tasks need is `cave-mouth`, `strongbox`, `key`.
Spec §3.8 earns each against a verb.

Task 7 needs the `AnchorKind` → thing-kind mapping to be **total**, so a row
per anchor kind is expected. **Which further kinds carry which property is the
implementer's call, made from the code** — spec §3.8 states why: a spec author
choosing them from outside has been wrong here every time it has been tried.

- [ ] **Step 6: Run the tests and confirm green**

Run: `cargo test -p hornvale-thing > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Then: `grep -E '^test result|FAILED|panicked' /tmp/hv-t2.log`
Expected: `exit=0`, both tests passing.

- [ ] **Step 7a: Join the store into `WorldComponents`**

**Added at pre-flight; the plan's first draft named `components.rs` in its
File Structure and gave the edit to no task.** Without it `promote` (Task 4)
would validate a thing-kind against a roster its kinds are absent from, and
`mint_instance_of_kind` would reject every one.

`WorldComponents` (`windows/worldgen/src/components.rs:26`) gains a
`thing: ComponentStore<KindId, ThingTraits>` field, and `kinds()` (`:216`)
extends with its ids alongside the other eleven stores. `deity`, `culture` and
`material` are the model — kind stores with no biosphere row.

`WorldComponents::assemble` must populate it. **An earlier draft of this step
told you to read an assertion that `assemble`'s output is "byte-equal to the
default roster's composed set". No such test exists** — that phrase is from
`build_world`'s doc comment in `windows/worldgen/src/lib.rs`, and a plan
sending you to find a test that was never written is this project's
best-documented defect shape.

What actually guards `assemble`, verified, is three tests in
`components.rs`'s own `mod tests`:

- `the_kind_roster_is_the_union_of_all_stores` — asserts named labels are
  present and that `kinds()` is sorted. A new store should keep both true.
- `kinds_with_covers_the_new_component_tags` — asserts **exact vectors** per
  tag. This is the one a new `ComponentTag` variant would break.
- `kinds_with_biosphere_is_the_full_roster_and_psyche_is_the_peopled_subset`.

Run those three and read what they assert before you change anything. If
adding the store reddens one, that is a finding to report, not a number to
update.

**Do NOT add a `ComponentTag` variant unless something reads it.** `kinds_with`
(`:196`) is the capability query; nothing in this plan calls it for things, and
an unread variant is a surface with no consumer.

- [ ] **Step 7: Prove the layering rule holds against the new crate**

Run: `cargo nextest run -p hornvale -E 'test(architecture)' > /tmp/hv-arch.log 2>&1; echo "exit=$?"`
Expected: `exit=0`. This test discovers crates from `cargo metadata`, so the
new domain is checked automatically against "a domain depends on
`hornvale-kernel` and nothing else." A red here means the Cargo.toml took a
dependency it may not have.

- [ ] **Step 8: Regenerate the artifacts this task moves**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**A branch table, not a prediction:**

```
  what moved                             response
  ------------------------------------   ------------------------------------
  book/src/reference/layering-generated   expected -- a new crate. Commit it.
  docs/audits/type-audit-report.md        expected -- new pub items. Commit it,
                                          and NEVER text-merge it later: it is
                                          an aggregate of counts.
  cli/tests/fixtures/world-seed-42.json    UNEXPECTED -- this task registers no
                                          concept and mints nothing. Stop and
                                          find out why before committing.
  anything else                           stop and read the diff before
                                          committing.
```

- [ ] **Step 9: Commit and push**

```bash
cargo fmt
git add domains/thing windows/worldgen/src/components.rs book/src/reference/layering-generated.md docs/audits/type-audit-report.md
printf '%s\n' \
  'feat(thing): the object domain and its kind roster' '' \
  'A domain that draws nothing, on the domains/alchemy model: what a kind IS' \
  'does not vary by world. Which kinds a world places is derived at the' \
  'composition root.' '' \
  'Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg' > /tmp/msg
git commit -F /tmp/msg
git push
```

---

### Task 3: The `ConceptKind` variant, the genesis registration, and the golden

**This task moves `cli/tests/fixtures/world-seed-42.json`.** It has its own
task for that reason: `make rebaseline` never writes that file, and neither
guarding test is in the subfloor roster, so `gate-commit` compiles them and
runs neither. A plan that folded this into another task would produce a green
local gate and a red chamber. This bounced The Offer once.

**Files:**
- Modify: `kernel/src/registry.rs` (one new `ConceptKind` variant),
  `domains/thing/src/lib.rs` (a `register_concepts` entry point)
- Modify: `windows/worldgen/src/lib.rs` (call it during genesis)
- Test: `domains/thing/src/lib.rs`'s `mod tests`
- Regenerate: `cli/tests/fixtures/world-seed-42.json` (**`make
  rebaseline-goldens`**), `book/src/reference/concept-registry-generated.md`,
  `docs/audits/type-audit-report.md`, `docs/digest/`

**Interfaces:**
- Consumes: `THING_KINDS` from Task 2.
- Produces: `pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError>`
  in `domains/thing`, called once at genesis.

- [ ] **Step 1: Add the `ConceptKind` variant**

`ConceptKind` (`kernel/src/registry.rs:26`) has nine variants — Substance,
Living, Celestial, Terrain, Social, Body, Kin, Quality, Act — and none fits a
made, portable object. Add one, with a doc comment in the register
`ConceptKind::Act`'s uses: say what it separates itself from and why, since
`Act`'s own doc is the model for how much a variant owes its reader.

**The compiler is the enumeration.** Every exhaustive match on `ConceptKind`
will fail to compile until the new variant is handled; that list is the work,
and it is complete by construction rather than by grep.

- [ ] **Step 2: Write the failing test**

In `domains/thing/src/lib.rs`:

```rust
/// Every roster kind is a registered concept after `register_concepts`, and
/// each carries this domain's own name — so the registry dump attributes
/// them here and not to whichever crate happened to call the function.
///
/// MUTATION THIS MUST FAIL AGAINST: name the property this must demonstrate —
/// that registration is not vacuous — and find a mutation for it from inside
/// the code. Paste the red here.
#[test]
fn every_roster_kind_registers_as_a_concept() {
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    register_concepts(&mut reg).expect("registration is total and idempotent");
    for label in THING_KINDS {
        let c = reg.concept(label).unwrap_or_else(|| panic!("{label:?} not registered"));
        assert_eq!(c.domain, "thing", "{label:?} attributed to the wrong domain");
    }
}
```

- [ ] **Step 3: Run it and watch it fail**

Run: `cargo test -p hornvale-thing > /tmp/hv-t3.log 2>&1; echo "exit=$?"`
Expected: `exit` non-zero — `register_concepts` does not exist.

- [ ] **Step 4: Implement `register_concepts`**

Follow `domains/settlement/src/lib.rs`'s own registration verbatim in shape —
`registry.register_manifest(Manifest { concept: ConceptDef { name, domain,
kind, doc }, lexeme, percept, cognition })`. Every domain that registers
concepts does it this way, so `domains/thing` needs no cross-domain
dependency: it registers its own.

Use `Correspondent::Absent(Void::Gap(...))` with an honest reason for
`lexeme`/`percept` unless a language pack genuinely names the kind — settlement
does exactly this and its comment explains why an honest `Gap` beats an
over-optimistic `Expected`.

- [ ] **Step 5: Join the composition root's domain roster**

**An earlier draft of this step was wrong twice and is corrected here.** It
said to "wire `register_concepts` into world construction … beside the other
domains' registrations", and justified the ordering with *"the derivation
order inside `WorldContext::build` is copied verbatim from the old
`Session::start`; a reorder changes which seed draws are taken."* That cites
`windows/vessel/src/session.rs` — a SESSION concern — as the reason
registration order matters during worldgen GENESIS. Two different mechanisms,
and the wrong one would send you to the wrong file and imply a seed-draw
hazard that does not apply.

**What is actually true**, from `windows/worldgen/src/lib.rs:306-339`:

1. There is a roster, `pub const DOMAINS: &[&dyn Domain]` (`:322`), and
   `register_all` (`:348`) iterates it. **You do not add a call anywhere** —
   you add one line to the roster and implement the trait.
2. The trait is `hornvale_kernel::Domain`. `domains/person/src/lib.rs:187` is
   the minimal model: a unit struct, `crate_name()` returning
   `env!("CARGO_PKG_NAME")`, and `register_concepts` delegating to the crate
   function you wrote in Step 4.
3. **Order on that roster constrains concept LENDERS and BORROWERS only.** The
   roster's own doc says why: `language::register_concepts` references
   concepts it does not own (terrain's `stone`, religion's `god`) and must run
   after their owners, "or it claims them under domain `language` and
   conflicts." `thing` owns every concept it registers and borrows none, so it
   has no ordering constraint — the roster already carries `person` last with
   a comment saying exactly that. Put `thing` last, and say why in a comment
   rather than leaving the next reader to re-derive it.
4. Membership is declarative, not the directory: `hornvale-demography` is a
   domain crate that registers nothing and is deliberately OFF the roster. So
   "every crate under `domains/` is on `DOMAINS`" is false and must not be
   asserted.

- [ ] **Step 5a: Assert registration is idempotent**

`domains/person/src/lib.rs:199-205` registers twice in one test and expects
both to succeed. The registry rejects a *conflicting* redefinition, not an
identical one, and every domain relies on that. Follow it.

- [ ] **Step 6: Run the tests, then refresh the golden**

```bash
cargo nextest run --workspace > /tmp/hv-t3-full.log 2>&1; echo "exit=$?"
grep -E 'FAILED|panicked|Summary' /tmp/hv-t3-full.log
```

`world-seed-42.json`'s guarding tests are expected to be **red** here — that
is the registration landing, not a bug. Confirm the failure names that fixture
and nothing else, then:

```bash
make rebaseline-goldens
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

```
  what moved                                response
  ---------------------------------------   ---------------------------------
  cli/tests/fixtures/world-seed-42.json      EXPECTED -- this is the task.
                                             Read the diff: it should add the
                                             new concepts and nothing else.
  concept-registry-generated.md, digest/     expected. Commit.
  a fixture this task has no reason to       stop. A moved golden nobody
  touch                                      predicted is the loudest signal
                                             in this campaign.
```

- [ ] **Step 7: Re-run the full suite green, then commit and push**

```bash
cargo nextest run --workspace > /tmp/hv-t3-after.log 2>&1; echo "exit=$?"
grep -E 'FAILED|panicked|Summary' /tmp/hv-t3-after.log
cargo fmt
git add -A
printf '%s\n' \
  'feat(thing): thing-kinds are concepts, registered at genesis' '' \
  'A thing must be nameable before it can be taken -- The Actants precedent,' \
  'which minted ConceptKind::Act so a verb could be a reaction gated by' \
  'properties. None of the nine existing variants fits a made, portable' \
  'object.' '' \
  'This moves the keystone byte-golden, which make rebaseline never writes.' '' \
  'Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg' > /tmp/msg
git commit -F /tmp/msg
git push
```

---

### Task 4: Identity and promotion

**Files:**
- Create: `windows/vessel/src/thing.rs`
- Modify: `windows/vessel/src/lib.rs` (declare the module)
- Test: `windows/vessel/src/thing.rs`'s own `mod tests`

**Interfaces:**
- Consumes: `THING_KINDS` (Task 2).
- Produces:
  - `pub fn thing_role(facet: &Facet, kind: &str) -> Result<String, FacetError>`
  - `pub fn thing_id(facet: &Facet, kind: &str, ordinal: u16) -> Result<EntityId, FacetError>`
  - `pub fn promote(ledger: &mut Ledger, registry: &ConceptRegistry, facet: &Facet, kind: &str, ordinal: u16, day: WorldTime) -> Result<EntityId, LedgerError>`

- [ ] **Step 1: Note the fallibility the spec did not mention**

`Facet::pack()` (`kernel/src/room.rs:326`) returns `Result<FacetId,
FacetError>` — it fails past `MAX_DEPTH` and on a malformed path. So
`thing_role` and `thing_id` are fallible, and the spec's §3.2 wrote them as
though they were not. Propagate the error; do not `unwrap`. If a caller in a
later task cannot propagate, say so in that task's report rather than
swallowing it.

- [ ] **Step 2: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn facet(face: u8, path: &[u8]) -> Facet {
        Facet { face, path: path.to_vec() }
    }

    /// The same (room, kind, ordinal) derives the same id in two ledgers that
    /// have never met — which is what lets a session find the strongbox a
    /// previous session promoted, and is the same property
    /// `reuse_or_mint_entity`'s own doc claims for a settlement's NPC.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: name the property — that the id
    /// depends on all three lineage legs — and find a mutation for it from
    /// inside `thing_role`/`thing_id`. Paste the red.
    #[test]
    fn a_things_id_is_a_pure_function_of_room_kind_and_ordinal() {
        let f = facet(3, &[1, 2]);
        let a = thing_id(&f, "strongbox", 0).expect("a shallow facet packs");
        let b = thing_id(&f, "strongbox", 0).expect("a shallow facet packs");
        assert_eq!(a, b);
    }

    /// Distinct rooms, kinds and ordinals never collide. A collision would
    /// make two things one thing, and every fact about either would key to
    /// the other.
    #[test]
    fn distinct_addresses_never_share_an_id() {
        let f1 = facet(3, &[1, 2]);
        let f2 = facet(3, &[1, 3]);
        let ids = [
            thing_id(&f1, "strongbox", 0).unwrap(),
            thing_id(&f2, "strongbox", 0).unwrap(),
            thing_id(&f1, "key", 0).unwrap(),
            thing_id(&f1, "strongbox", 1).unwrap(),
        ];
        let uniq: std::collections::BTreeSet<_> = ids.iter().collect();
        assert_eq!(uniq.len(), ids.len(), "two addresses share an id: {ids:?}");
    }

    /// Promotion is idempotent: promoting twice yields one entity, not two.
    /// This is the whole reason `reuse_or_mint_entity` exists rather than
    /// `mint_entity`, whose collision assert would panic on the second call.
    #[test]
    fn promoting_twice_yields_one_entity() {
        // Body deliberately unwritten -- see the note below this block.
    }
}
```

**RESOLVED AT PRE-DISPATCH — there is no helper, and the idiom is inline
construction.** `windows/vessel/tests/suite/passage.rs:207-211` is the sibling
module's own pattern, and it is the one to copy:

```rust
let mut reg = ConceptRegistry::default();
reg.register_predicate(PASSAGE_CLEARED, false, "t").unwrap();
let mut ledger = Ledger::default();
let who = ledger.mint_entity(hornvale_kernel::test_lineage(0));
```

**Two things that will otherwise cost you a debugging cycle**, both verified:

1. **`ConceptRegistry::default()` is EMPTY.** It derives `Default` over
   `BTreeMap`s, so it pre-registers nothing — not even the kernel-core
   predicates. `KERNEL_CORE_PREDICATES` exists for a single-writer *check*, not
   for registration. So `promote` commits an `INSTANCE_OF` fact that a default
   registry will REJECT until the test registers that predicate itself, exactly
   as `passage.rs` registers `PASSAGE_CLEARED`.
2. `FacetError` is exported from the kernel root (`kernel/src/lib.rs:72`), and
   `Facet`'s `face`/`path` fields are `pub`, so the test's `facet()` helper
   needs no accessor.

Declare the module as `pub mod thing;` beside `pub mod passage;`
(`windows/vessel/src/lib.rs:27`).

**The original note stands for the rest:** This plan does not know which fixture helper
`windows/vessel` exposes for a bare ledger plus registry, and **naming a
helper that does not exist is the single most common defect in this project's
plan text** — The Latch's first pre-flight defect was exactly that
(`crate::common::seed_42_world()`, which has never existed). A placeholder
call would be the same defect wearing a comment.

Find the nearest existing helper by reading the crate's other tests, and use
it. The claim to assert is: promote the same (facet, kind, ordinal) twice
against one ledger and get one entity and one `INSTANCE_OF` fact, not two.
`mint_entity`'s collision assert is what would panic if the implementation
reached for it instead of `reuse_or_mint_entity`, so a panic here is a
diagnosis, not a mystery.

- [ ] **Step 3: Run the tests and watch them fail**

Run: `cargo nextest run -p hornvale-vessel -E 'test(thing)' > /tmp/hv-t4.log 2>&1; echo "exit=$?"`
Expected: non-zero, on missing items.

- [ ] **Step 4: Implement**

```rust
/// The role leg of a thing's lineage: the ROOM and the KIND, never the anchor
/// index. That choice is this module's whole compliance with decision 0069 —
/// a facet is the coarse layer and a kind is authored, so nothing stored
/// points into the fine layer, which 0069 licenses to regenerate forever.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(identifier-text: return)
pub fn thing_role(facet: &Facet, kind: &str) -> Result<String, FacetError> {
    Ok(format!("thing@{}/{}", facet.pack()?.0, kind))
}
```

`thing_id` derives from `Lineage { parent: None, role: &thing_role(..)?,
ordinal }`. `promote` calls `Ledger::reuse_or_mint_entity` and commits one
`INSTANCE_OF` fact — **not** `mint_entity`, whose collision assert panics on a
repeat, and which is correct only for a genesis path that mints a fresh
lineage exactly once.

Apply Task 1's ordinal finding: if the census found no production room
composes two anchors of one promotable kind, the callers in later tasks pass
`0` and this module carries the invariant in its doc.

- [ ] **Step 5: Confirm green, then commit and push**

Run the same filtered command; expect `exit=0`. Then `cargo fmt`, add
`windows/vessel/src/thing.rs` and `windows/vessel/src/lib.rs`, commit with
`git commit -F`, push.

No generated artifact moves in this task; run the drift check anyway and stop
if one did.

---

### Task 5: `located-in` — one predicate, three location types

**Files:**
- Modify: `windows/vessel/src/thing.rs`, `windows/vessel/src/session.rs`
  (register the predicate)
- Test: `windows/vessel/src/thing.rs`'s `mod tests`

**Interfaces:**
- Consumes: `thing_id`, `promote` (Task 4).
- Produces:
  - `pub const LOCATED_IN: &str = "located-in";`
  - `pub fn located_fact(thing: EntityId, place: Value, day: WorldTime) -> Fact`
  - `pub fn location_of(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<Value>`
    — the **as-of-day** read, which is what a replay must use.
  - `pub fn room_of(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<String>`
    — `location_of` followed transitively through `Value::Entity` holders.
  - `pub const OPENNESS: &str = "openness";`
  - `pub fn openness_fact(thing: EntityId, open: bool, day: WorldTime) -> Fact`
  - `pub fn is_open(ledger: &Ledger, thing: EntityId, day: WorldTime) -> Option<bool>`
    — `None` when no fact exists, which means "whatever the seed drew".

**`openness` is produced HERE, and the plan's first draft produced it
nowhere.** Tasks 8 and 11 both said they consumed "`openness` (Task 7's
`Openable`)", which conflates two different objects: `Openable` is an
affordance property saying a kind CAN be opened; `openness` is the ledger
predicate recording that one WAS. Both folds live in this module, share the
`day' <= day` discipline, and share the registration question in Step 1 —
answering it once for both is strictly better than twice.

- [ ] **Step 1: Register the predicate, and decide WHERE from the code**

`AGENT_AT` and `PASSAGE_CLEARED` are both registered per-session
(`session.rs`, near line 1023), never at genesis, and `world-seed-42.json` did
not move for either. Task 3 already registered thing-kind *concepts* at
genesis; a *predicate* is a different object.

```
  question                                  answer this task must establish
  ---------------------------------------   -------------------------------
  does per-session registration keep the     if yes, follow AGENT_AT exactly.
  golden still?                              Verify by running the golden's
                                             guarding tests, not by reasoning.
  does anything outside a session need to    if yes, genesis registration is
  read located-in?                           required and the golden moves
                                             again -- report before doing it.
```

- [ ] **Step 2: Write the failing tests**

```rust
/// A thing's location is the LATEST fact at or before the day asked about,
/// not the latest fact outright. This is what distinguishes a fold from a
/// mutable flag: a replayed past must not see a move that had not happened
/// yet. Same discipline as `last_fact_day_at_or_before` in the liveness walk,
/// and the rule decision 0366 states.
///
/// MUTATION THIS MUST FAIL AGAINST: name the property — that the `<= day`
/// filter is load-bearing — and find a mutation for it inside `location_of`.
/// Paste the red.
#[test]
fn location_is_read_as_of_the_day_asked_about() {
    // commit two located-in facts for one thing on different days;
    // assert the read at the earlier day yields the earlier place.
}

/// A key in a chest in a room is in the room. Transitivity is a property of
/// the `in` relation (RCC-8, declared in The Hearth SS5), so it belongs in the
/// resolver and not in each caller.
#[test]
fn containment_resolves_transitively_to_a_room() {
    // key located-in Entity(chest); chest located-in Text(room);
    // room_of(key) == that room.
}

/// A cycle in containment terminates rather than hanging. Nothing in this
/// campaign creates one, which is exactly why nothing would catch it.
#[test]
fn a_containment_cycle_terminates() {
    // a located-in Entity(b); b located-in Entity(a); room_of(a) is None.
}
```

**Write the bodies from inside the code.** The three claims are specified;
the fixtures are not, because this plan does not know the crate's helpers.

- [ ] **Step 3: Run them, watch them fail, implement, confirm green**

`location_of` reads the subject's own postings and applies `day' <= day`.
`room_of` walks `Value::Entity` links with a visited set so the cycle test
terminates. Use `BTreeSet`, never `HashSet`.

- [ ] **Step 4: Commit and push**

`cargo fmt`, `git commit -F`, `git push`. Run the drift check; stop if
anything moved.

---

### Task 6: Latency — the first negative fold in the tree

**This is the task the spec expects to be hardest** (§3.4, §4.1). The Latch
never needed a fold in this direction, because a cave mouth cannot leave the
room it is in.

**Files:**
- Modify: `windows/vessel/src/thing.rs`
- Test: `windows/vessel/src/thing.rs`'s `mod tests`, and
  `windows/vessel/tests/suite/thing.rs` for the two-entry behavioural case

**Interfaces:**
- Consumes: `thing_id` (Task 4), `location_of` (Task 5).
- Produces: `pub fn is_latent(ledger: &Ledger, facet: &Facet, kind: &str, ordinal: u16, day: WorldTime) -> Result<bool, FacetError>`

- [ ] **Step 1: Write the rule down before writing the code**

```
  latent(facet, kind, n)  ==  the grammar offers it
                          AND no committed located-in fact places
                              thing_id(facet, kind, n) anywhere but this room
```

The id is derivable *before* the ledger read, which is what makes this one
indexed lookup per slot rather than a search. Task 1 measured the cost; if it
came back worse than the design assumed, that finding governs and this task
reports rather than proceeds.

**Note the `anywhere but this room` clause.** A thing promoted and *put back*
is still here. An implementation that treats "has any `located-in` fact" as
"has left" would make every promoted fixture vanish from its own room, and it
would pass a test that only ever carries things away.

- [ ] **Step 2: Write the failing tests**

```rust
/// A thing carried away is not offered again where it came from -- the
/// negative fold, asserted across two entries into the same room.
///
/// MUTATION THIS MUST FAIL AGAINST: name the property -- that the location
/// check is consulted at all -- and find a mutation for it. Paste the red.
#[test]
fn a_thing_carried_away_is_not_re_offered_where_it_came_from() {}

/// A thing promoted and PUT BACK is still offered here. This is the case an
/// implementation keyed on "has any located-in fact" gets wrong, and it is
/// the reason this test exists beside the one above rather than instead of it.
#[test]
fn a_thing_put_back_is_still_here() {}

/// Latency is read as-of-day like every other fold: at a day before the
/// carrying fact, the thing is still here.
#[test]
fn latency_is_time_correct() {}
```

- [ ] **Step 3: Run, fail, implement, confirm green**

Run: `cargo nextest run -p hornvale-vessel -E 'test(thing)' > /tmp/hv-t6.log 2>&1; echo "exit=$?"`

- [ ] **Step 4: Commit and push**

`cargo fmt`, `git commit -F`, `git push`, drift check.

- [ ] **Step 5: Stage-gate boundary — absorb main**

This is the end of the substrate. Submit
`make sluice-stage BRANCH=campaign/the-chattel REF=<full-sha>` with a real
SHA, never a branch name, and wait for the verdict before starting Task 7.

---

### Task 7: Re-key the property table to `KindId`, and add three properties

**Files:**
- Modify: `windows/vessel/src/affordance.rs`, `domains/thing/src/lib.rs`
- Test: `windows/vessel/tests/suite/affordance.rs` (exists), plus
  `affordance.rs`'s own `mod tests`
- Regenerate: `docs/audits/type-audit-report.md`,
  `book/src/reference/concept-registry-generated.md`

**Interfaces:**
- Consumes: `THING_KINDS`, `thing_registry` (Task 2).
- Produces: `ObjectTraits` keyed by `KindId`; `ObjectProperty::{Portable,
  Openable, Lockable}`; a total `AnchorKind -> KindId` mapping.

- [ ] **Step 1: Understand why this is one table and not two**

Spec §3.6: a strongbox's properties must not be able to disagree between its
latent form (an anchor) and its promoted form (a thing) — the same object at
two lifecycle stages with two sources of truth is a silent bug class, and
§3.1's promotion makes it reachable by construction. So the table re-keys to
`KindId` and every `AnchorKind` maps to a thing-kind.

The Offer forecast exactly this: *"Joining is IV.b's move, when objects become
mintable entities validated against `WorldComponents::kinds()`."* IV.b did not
join; this task does.

- [ ] **Step 2: Add the three properties, each earned by a verb**

```
  property      carried by (at minimum)   verb it gates (Tasks 10-11)
  ---------------------------------------------------------------
  Portable      key                       take / drop
  Openable      strongbox, cave-mouth     open / close
  Lockable      strongbox                 open, requiring a key in custody
```

`ObjectProperty::concept_name` (`affordance.rs:73`) is an exhaustive match
with no wildcard arm, and its doc says never to add one: a new variant fails
to compile there until it is named. That is the tripwire — let it fire, and
update `ObjectProperty::all` with it.

**Which further kinds carry which property is the implementer's call, made
from the code.** Spec §3.8 and The Offer's §3.3 both say why.

- [ ] **Step 3: Preserve The Offer's two-way M+N proof through the re-key**

`windows/vessel/tests/suite/affordance.rs` already holds The Offer's
acceptance tests, including the structural scan for a verb×object table
(decision 0350). **The risk in this task is re-keying in a way that still
passes those tests while changing what they mean.** Before changing anything,
read what each asserts and write down — in the task report — what each one
would still catch after the re-key. Any test whose answer is "nothing" is a
finding, not a formality.

- [ ] **Step 4: Run the affordance suite, confirm green, commit, push**

```bash
cargo nextest run -p hornvale-vessel -E 'test(affordance)' > /tmp/hv-t7.log 2>&1; echo "exit=$?"
grep -E 'FAILED|panicked|Summary' /tmp/hv-t7.log
```

Then `make rebaseline`, drift check, `cargo fmt`, `git commit -F`, `git push`.

---

### Task 8: Passages join the object model — `passage-cleared` retires

**Files:**
- Modify: `windows/vessel/src/passage.rs`, `windows/vessel/src/session.rs`,
  `windows/vessel/src/thing.rs`
- Test: `windows/vessel/tests/suite/passage.rs` (exists), `session.rs`'s
  `mod tests`
- Create: `docs/decisions/0396-<slug>.md` superseding 0367

**Interfaces:**
- Consumes: `openness`/`is_open` (Task 5), `Openable` (Task 7), `thing_id`, `promote` (Task 4).
- Produces: `effective_state` re-expressed over `openness`; `cave-mouth` as a
  thing-kind.

- [ ] **Step 1: Read what the retiring predicate promised on disk**

`addr_key` (`passage.rs:60`) carries a contract beyond injectivity, and its
own doc states it: the band rides as `{:?}`, `possess --out` puts these keys
in a saved world, and `addr_key_spelling_is_the_permanent_on_disk_key`
(`tests/suite/passage.rs`) pins the literal spelling for that reason — *"a
rename is an epoch, not an edit."*

Retiring the predicate retires that test's subject. **Do not delete the test
without replacing what it guarded**: whatever key a cave-mouth thing derives
from now owes the same on-disk permanence, and needs its own pinned-literal
test. A guard deleted along with the thing it guarded is the cheapest repair
and the wrong one.

- [ ] **Step 2: Confirm the blast radius is still what the spec measured**

```bash
grep -rl 'passage-cleared' --include='*.json' . ; echo "exit=$?"
```

```
  result                    response
  -----------------------   ---------------------------------------------
  no files (exit 1)         as the spec measured. Proceed.
  any file                  STOP. A committed fixture now carries the
                            predicate, the spec's SS3.7 measurement is stale,
                            and the break is no longer free. Report.
```

- [ ] **Step 3: Re-express `effective_state` over `openness`**

The shape is unchanged and the spec pins it:

```
  effective_state(thing, day) =
      Open                     if the latest openness fact at-or-before day is true
      barrier_of(seed, addr)   if it is false, or if there is none
```

Monotonicity is what goes away. `BarrierState`'s derived `Ord` (`Sealed <
Warded < Thin < Open`) no longer implies the fold cannot go down, so **any
comment or doc claiming monotonicity by construction is now false and must be
rewritten, not left.** Decision 0367's own §"The rule" is the text to check
against.

- [ ] **Step 4: Keep the three-outcome tripwire honest**

`delve` has three distinguishable outcomes and a tripwire says so. Its doc
demands a scan of every cave-bearing vertex; keep that shape. The Latch's own
§4 risk 3 names the failure mode: a renamed tripwire that passes on a fixture
accident. The four refusal strings were verified pairwise distinct once; a
new closing refusal makes five, and the same check must cover it.

- [ ] **Step 5: Write decision 0396**

Supersede 0367 with the thing 0367 asked for. State: what folds when an open
and a close fact both exist (ledger-order — 0367 forecast this and named The
Coercion's `possessed-by`/`possession-ended` pair as the precedent to follow);
that a monotone latch could not express a trap and this can; and that the
save-format break was measured, not estimated.

- [ ] **Step 6: Full suite, rebaseline, drift check, commit, push**

The gallery transcripts contain `delve` output and are expected to move.

---

### Task 9: The knowledge gate denies something, for the first time

**Files:**
- Modify: `windows/vessel/src/affordance.rs`
- Test: `windows/vessel/tests/suite/affordance.rs`
- Create: `docs/decisions/0397-<slug>.md` answering 0369

**Interfaces:**
- Consumes: Task 7's `KindId` keying, Task 8's `cave-mouth` thing-kind.
- Produces: `offered_to_observer` keyed on thing-kind.

- [ ] **Step 1: Re-key `offered_to_observer`**

`offered_to_observer` is at `windows/vessel/src/affordance.rs:450` — **the
definition, not the first grep match.** A bare `grep -n offered_to_observer`
returns 344, a doc-comment mention. This plan's author hit that and it is the
kind of citation error a reviewer cannot see.

- [ ] **Step 2: Build the firing case**

Decision 0369 is precise about why The Latch could not: the gate took an
`AnchorKind`, `AnchorKind` has no cave-mouth variant, and `passage.rs` held
zero references to `Knowledge`. Tasks 7 and 8 removed both obstacles.

```rust
/// The gate DENIES something -- the first time in this project's history.
/// The Latch's acceptance criterion 5 was recorded unmet with its reason
/// (decision 0369) because the obstacle was ADDRESSING, not durability.
///
/// MUTATION THIS MUST FAIL AGAINST: make `offered_to_observer` ignore its
/// `known` argument. The Offer's own
/// `an_unencountered_object_offers_nothing` is already mutation-proven this
/// way, so follow it. Paste the red.
#[test]
fn an_unencountered_passage_offers_nothing() {}
```

- [ ] **Step 3: If it cannot be made to fire, that is a result**

This task is dispatched as one that may correctly produce nothing. If the gate
still cannot deny, **record it unmet with its reason** — that is what 0369
itself did, and the spec's §6.4 commits to it in advance. A criterion quietly
dropped tells a successor nothing; an unreachable one tells them where the
wall is.

- [ ] **Step 4: Write decision 0397 — ANSWER 0369, do not supersede it**

0369's rule ("a gate's reachability is a property of what it is keyed on, not
of how durable the state behind it is") is correct and stays in force. Only
its enumeration of remedies was short: it listed two and this campaign took a
third. Say that plainly, and update `PLAY-passage-has-no-anchor` in the idea
registry.

- [ ] **Step 5: Suite, rebaseline, drift, commit, push**

- [ ] **Step 6: Stage-gate boundary — absorb main**

`make sluice-stage BRANCH=campaign/the-chattel REF=<full-sha>`.

---

### Task 10: The name → entity lookup

**Files:**
- Modify: `windows/vessel/src/focalize.rs`, and the `Noun` construction sites
- Test: `windows/vessel/tests/suite/` (the focalize tests' existing home)

**Interfaces:**
- Consumes: `thing_id` (Task 4).
- Produces: `Noun.entity: Option<EntityId>`.

- [ ] **Step 1: Add the field additively**

`Noun` (`windows/vessel/src/focalize.rs:79`) carries `display`, `datum`,
`words`, `kind`. Add `entity: Option<EntityId>`, defaulting to `None`, with a
`with_entity` builder beside the existing `with_kind` — that is the
established shape for "construction sites that can claim one."

**`Noun::new` has callers that construct it by full literal.** Adding a field
breaks every one of them; that list is the compiler's to produce, not this
plan's. Use the builder so `new` keeps its signature.

- [ ] **Step 2: Write the failing test**

```rust
/// A typed word resolves to the entity, not merely to a string. Before this
/// campaign the catalog yielded a `datum` and stopped, which is the whole of
/// the "no name -> entity lookup" gap The Offer named.
///
/// MUTATION THIS MUST FAIL AGAINST: make `with_entity` discard its argument.
#[test]
fn a_typed_word_resolves_to_a_things_entity() {}
```

- [ ] **Step 3: Run, fail, implement, confirm green, commit, push**

`words` is process-internal and never serialized — `Noun`'s own doc says
putting aliases on the wire would spray them into the client legend. `entity`
is likewise **not** serialized by this task; the wire is Task 13.

---

### Task 11: `open` and `close`

**Files:**
- Modify: `windows/vessel/src/session.rs` (dispatch arm, `IN_CHARACTER_VERBS`,
  `HELP`), `windows/vessel/src/thing.rs`
- Test: `session.rs`'s own `mod tests` (the dispatch and body-state tests need
  private seams), `windows/vessel/tests/suite/thing.rs`
- Regenerate: `book/src/gallery/possession-*.md`

**Interfaces:**
- Consumes: `openness`/`is_open` (Task 5), `Openable`/`Lockable` (Task 7), `promote` (Task 4), `is_latent` (Task 6), `Noun.entity` (Task 10).
- Produces: two verbs, a pattern that places contents, and the `Lockable`
  precondition that reads a second object.

- [ ] **Step 0: Author the contents, because today nothing is inside anything**

**Without this step every test below passes vacuously, and the campaign's
headline feature reports nothing forever.** The Offer's Task 6 censused all 60
production gate combinations and found the grammar's only `within` relation
anywhere is `{(Hearth, Alcove)}` — *nothing is ever placed inside a
strongbox*. `the-strongbox` is `Attach::Beside(Vessel)`, a sibling, not a
container. So `Lockable` has nothing to lock and `open` has nothing to reveal
until a pattern puts something there.

The grammar already supports it: `Attach::Within(AnchorKind)` exists
(`windows/vessel/src/interior/pattern.rs:26`, "strictly inside the first
anchor of this kind (`Ntpp`)") and `compose` (`:438`) sets `within` from it.
`INVENTORY` (`:144`) is a `[Pattern; 14]` whose length is part of the type and
must grow.

Add a pattern placing a `key` thing-kind `Within(AnchorKind::Strongbox)`,
with `requires: Some(AnchorKind::Strongbox)` so the admissibility filter —
Alexander's "patterns complete other patterns", made checkable — keeps it out
of rooms with no strongbox.

**Then re-run The Offer's own census** and confirm the `within` multiset now
contains the new pair. An authored pattern that no production combination
selects is the same vacuity in a new place.

**This moves the interior of every room that draws it**, so the gallery
transcripts and any warmth/route test reading a composed interior may move
with it. Read that drift; do not accept it.

- [ ] **Step 1: The three-things rule, per verb, without exception**

**A verb added to the dispatcher and to neither roster is invisible to every
test.** `every_bare_verb_help_lists_is_classified` checks `HELP`→roster and
roster→`HELP`; a verb in *neither* is iterated by neither loop, so both
directions are satisfied by its absence. The Latch shipped `clear` exactly
that way — the only verb in the free band that writes to the ledger — so a
sleeping body could commit a fact, with 715 tests green. `IN_CHARACTER_VERBS`
is `[&str; 20]` at `session.rs:126`; its length is part of the type and must
grow with it.

For **each** of `open` and `close`:

```
  [ ] a dispatch arm in Session::handle's in-character match
  [ ] an entry in IN_CHARACTER_VERBS (and its array length bumped)
  [ ] a line in HELP
  [ ] a body-state refusal test -- <verb>_is_refused_while_asleep
```

The template is `warm`. `warm_is_refused_while_asleep` and
`clear_is_refused_while_asleep` are the two shipped examples; read both.

**These refusal tests will not run in `gate-commit`.** Their siblings measure
13.4 s and 12.5 s, over `subfloor-roster.tsv`'s `BASELINE_FLOOR_SECS = 1.0`,
so they are permanently excluded by design — coverage is the stage gate's job.
Do not "fix" that by making the test cheaper in a way that stops exercising
the gate.

- [ ] **Step 2: Write the failing tests**

```rust
/// A body asleep cannot open anything. The body-state gate stands in front
/// of every in-character verb, and a verb absent from IN_CHARACTER_VERBS
/// silently bypasses it.
///
/// MUTATION THIS MUST FAIL AGAINST: remove "open" from IN_CHARACTER_VERBS.
/// The verb keeps working, the dispatch is unchanged, and only this test
/// objects -- which is the whole point. Paste the red.
#[test]
fn open_is_refused_while_asleep() {}

/// Same, for close.
#[test]
fn close_is_refused_while_asleep() {}

/// A lockable thing refuses without the key and yields with it -- the arc's
/// own acceptance test, and the first precondition in Hornvale that reads a
/// SECOND object.
///
/// MUTATION THIS MUST FAIL AGAINST: name the property -- that custody is
/// consulted -- and find a mutation for it from inside the code.
#[test]
fn a_lockable_thing_opens_only_with_the_key_in_custody() {}

/// Opening is durable for the session, and closing undoes it -- the
/// re-closability 0367 deferred, now shipped.
#[test]
fn a_container_opens_closes_and_re_opens() {}
```

- [ ] **Step 3: Run, fail, implement, confirm green**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-t10.log 2>&1; echo "exit=$?"
grep -E 'FAILED|panicked|Summary' /tmp/hv-t10.log
```

- [ ] **Step 4: Verify the roster agreement test still means something**

Run `every_bare_verb_help_lists_is_classified` and confirm it passes — then
confirm it would have *failed* had you added the verb to only one of the two
rosters. That second half is the check; the first half passes vacuously for a
verb in neither.

- [ ] **Step 5: Rebaseline, drift check, commit, push**

The gallery transcripts move if any `examine` or `look` output changed.

---

### Task 12: `take`, `drop`, `put`, and `carrying`

**Files:**
- Modify: `windows/vessel/src/session.rs`, `windows/vessel/src/thing.rs`
- Test: `session.rs`'s `mod tests`, `windows/vessel/tests/suite/thing.rs`
- Regenerate: `book/src/gallery/possession-*.md`

**Interfaces:**
- Consumes: `located_fact`, `location_of`, `room_of` (Task 5), `is_latent`
  (Task 6), `Portable` (Task 7), `Noun.entity` (Task 10 — `take <thing>`
  resolves a typed noun to an entity, and cannot without it).
- Produces: four verbs; custody as a `located-in` fact naming the body.

- [ ] **Step 1: The three-things rule, per verb, without exception**

Same rule as Task 11, repeated because a reader may arrive here first. A verb
in the dispatcher and in neither roster is invisible to every test; the paired
agreement check cannot see it, because both its directions are satisfied by
absence. The Latch shipped one that way.

For **each** of `take`, `drop`, `put`, `carrying`:

```
  [ ] a dispatch arm in Session::handle's in-character match
  [ ] an entry in IN_CHARACTER_VERBS (and its array length bumped)
  [ ] a line in HELP
  [ ] a body-state refusal test -- <verb>_is_refused_while_asleep
```

**`carrying` is the one to think about rather than copy.** It reads state and
writes none, so it resembles `knows` and `needs` more than it resembles
`take`. Decide from the code whether it is in-character (and so gated by the
body) or an operator instrument, and say why in its doc — `SESSION_CONTROL` is
disjoint from `IN_CHARACTER_VERBS` by an asserted test, so a wrong choice is
caught, but the *reason* is the part worth writing down.

- [ ] **Step 2: Write the failing tests**

```rust
/// A thing taken in one room is carried to another and is still held there
/// -- the identity-that-travels claim, which is the whole campaign.
///
/// MUTATION THIS MUST FAIL AGAINST: name the property -- that custody
/// survives a room transition -- and find a mutation from inside the code.
#[test]
fn a_thing_taken_is_carried_between_rooms() {}

/// A dropped thing is in the room, and is offered there on the next entry.
#[test]
fn a_dropped_thing_joins_the_room_it_was_dropped_in() {}

/// take is refused while asleep. Repeat for drop, put, and carrying if
/// carrying is in-character.
#[test]
fn take_is_refused_while_asleep() {}

/// A saved played world carries custody. `possess --out` folds the evolved
/// ledger AND the per-session registry into a new World (decision 0368), and
/// 0171 rules a player's acts are not filtered on the way out. The Latch left
/// this round trip untested for its own predicate and said so; this campaign
/// claims it, so it tests it.
///
/// Drive: take a thing, `--out` a world, re-possess it, and assert the thing
/// is still held.
#[test]
fn custody_survives_a_save_and_a_re_possession() {}
```

The last test is the cheapest real work The Latch left behind, and it is the
evidence for spec §6.1. **It must drive the actual round trip**, not assert
that a sibling predicate works.

- [ ] **Step 3: Run, fail, implement, confirm green**

- [ ] **Step 4: Rebaseline, drift check, commit, push**

- [ ] **Step 5: Stage-gate boundary — absorb main**

`make sluice-stage BRANCH=campaign/the-chattel REF=<full-sha>`.

---

### Task 13: The wire

**Files:**
- Modify: `windows/vessel/src/snapshot.rs`
- Test: `windows/vessel/tests/suite/`, `clients/game/core/`
- Regenerate: `clients/game/core/tests/fixtures/session-seed-42-*.json`

**Interfaces:**
- Consumes: Task 7's properties, Task 12's custody.
- Produces: `NounEntry.affordances`, and carried things on the snapshot.

- [ ] **Step 1: Add `affordances` the way `kind` was added**

The Offer specified this field and deferred it, and its §5.1 states the
precedent verbatim from `NounEntry.kind`'s own doc: *"Optional on the wire:
older mirrors load unchanged (serde default), newer fixtures carry it.
Additive on `vessel/session/v2` per the schema discipline."* So
`#[serde(default)]`, additive, **no version bump**.

- [ ] **Step 2: Check the reason The Offer deferred it is actually gone**

The Offer deferred the field because a defect it found made it useless:
`session-seed-42-chamber.json`'s `narration.nouns` held only walk-band nouns —
no anchor reached the wire even in the chamber band — so the field would have
been empty for every entry.

```
  measurement                              response
  --------------------------------------   ------------------------------------
  anchors now reach narration.nouns        proceed; the field carries something.
  they still do not                        STOP and report. Shipping an empty
                                           additive field is the third artifact
                                           reading as delivered while doing
                                           nothing, after SS3.5's gate. The Offer
                                           refused to do it once already.
```

- [ ] **Step 3: Carried things on the snapshot — and the guard to check first**

`clients/game/core/src/endpaper.rs:62` fails if `inventory` appears in the
rendered strip. **That guard is narrower than it looks and this plan says so
because the handoff read it the other way:** it asserts the *identity strip*
draws no vitals, and the strip reads four `SelfChannel` fields. A `carrying`
verb never touches it.

What it actually guards is a `Snapshot`-level statement in its own module doc
— *"`Snapshot` carries no player vitals at all — no hit points, stamina,
hunger, or inventory — by design (The Quire spec §6)"*. Putting carried things
on the snapshot engages **that**, not the strip's forbidden-word list.

So: read The Quire's §6 before changing anything here, and if the change
contradicts it, that is a decision record, not an edit.

- [ ] **Step 4: Run the client checks — they are not in any Rust gate**

```bash
make game-check > /tmp/hv-game.log 2>&1; echo "exit=$?"
```

`clients/` is outside the cargo workspace with its own toolchain. `make
gate-commit` does not build it.

- [ ] **Step 5: Rebaseline, drift check, commit, push**

The committed client fixtures are declared in `docs/generated-paths.txt` and
move here.

---

### Task 14: Definition of Done — the sweep that asserts, rather than fixes

**This task fixes nothing.** Every earlier task regenerated and committed its
own artifacts. If this task finds drift, that is a **finding about an earlier
task**, and it goes in the retrospective.

**Files:**
- Create: `book/src/chronicle/the-chattel.md`,
  `docs/retrospectives/the-chattel.md`
- Modify: `book/src/frontier/idea-registry.md`, `book/src/open-questions.md`,
  `docs/superpowers/specs/2026-08-28-the-chattel-design.md` (§8 decisions
  taken during execution)

- [ ] **Step 1: Assert the diff is empty**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$'); echo "exit=$?"
```

`exit=0` is the expected result. Anything else names the task that should have
committed it.

- [ ] **Step 2: Assert the non-goal, because an unasserted invariant is a wish**

Spec §5 claims *every object that will ever exist is already latent* — nothing
is created and nothing is destroyed. That is checkable and nothing in Tasks
1–13 checks it. Write the test: over the production gate combinations, every
`INSTANCE_OF` fact a played session commits names a thing-kind the grammar
latently offers at that room, and no code path removes one.

If it cannot be written as stated, the non-goal is weaker than §5 claims and
the retrospective says so. **A non-goal nobody can test is a hope, and this
project has shipped five vacuous guards in one campaign before.**

- [ ] **Step 3: Registry and open-questions sweep**

- `MAP-if-world-conformance` — this campaign is its named first customer. Say
  what moved and what did not.
- `PLAY-passage-has-no-anchor` — closed or not, per Task 9's real result.
- `MAP-19`, `MAP-27` — the ontology and chemistry rows this advances.
- `MAP-playthrough-persistence` — Task 12 tested the round trip 0368 left as
  work; update the row to say so.
- `book/src/open-questions.md` — decision 0030 requires re-scoring any
  Confidence Gradient bet a campaign moves. Promotion-on-touch is named in
  that chapter as "exactly as unbuilt as they were." It is now built. **A
  witness firing is not a witness changing status** — report the number and
  let Nathan move the status.

- [ ] **Step 4: Chronicle and retrospective**

Both are written **before** the merge submission, not after. The chronicle is
product; the retrospective is process. The retrospective must count defects by
**origin** (controller prose vs implementer code) — four campaigns running have
reported the same distribution, and the interesting number is the split, not
the total.

- [ ] **Step 5: Promote the SDD ledger into the spec**

`.superpowers/sdd/` is git-ignored and dies with the worktree. Promote every
material decision into the spec's §8. Never `git add -f` the ledger.

- [ ] **Step 6: Submit the merge**

```bash
make sluice BRANCH=campaign/the-chattel REF=<full-sha>
```

The merge **refuses** without a `Sluice-Headline:` trailer, in the same
trailer block as `Claude-Session:` and with no blank line between them.
