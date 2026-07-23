# The First Mark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A player possessing an agent can commit a consequential act that biases a latent NPC tension past its threshold, watch the consequence fire a tick later, persist that mark to a saved world, and trace the consequence back to their own hand.

**Architecture:** Extend `windows/vessel` so the possess session commits the *first player-authored facts* (an opposed `provoke`/`soothe` disposition vocabulary) into its already-owned ledger clone. Those facts feed an **additive, threshold-gated** term into one existing NPC drive (`liveness.rs`, the `>= act` seam), so an unplayed world is byte-identical by construction. On the next `wait` tick the tipped drive commits its discrete consequence. `possess --out` saves the evolved `World`; `why`/`recount` (which already renders provenance) is the butterfly-reader.

**Tech Stack:** Rust 2024, std-only (serde + serde_json). Randomness via the kernel `Seed`/`Stream`. No new crates.

## Global Constraints

Copied verbatim from the spec and the crate contracts; every task's requirements implicitly include these:

- **A player mark is an ordinary fact in the world's own vocabulary.** No player-only *storage*; player facts are `Fact`s distinguished only by their existing `provenance: String` field (e.g. `"player: provoke"`). No change to the `Fact` envelope.
- **Presence biases latent tensions; never manufactures them.** The bias is an **additive** term on an existing drive, **gated at the existing `act` threshold**. No player fact → zero added term → **byte-identical** arbitration.
- **Byte-identity contract:** seed 42 with no act verb used must leave every committed artifact byte-identical to `main` (drift check). New predicates are additive (safe, like a new stream label); registered in the session's registry extension exactly as `AGENT_AT` already is (`session.rs:90`).
- **Determinism / Lorenz guard-rail:** the forward step commits a **discrete** fact (quantized at emit via `Ledger::commit`); reload re-derives from the fact trace, never from a quantized float checkpoint. Never seed a forward-integrator from persisted floats.
- **No `HashMap`/`HashSet`** (`BTreeMap`/`BTreeSet`/`Vec` only); **no wall-clock time**. Every `pub` item carries a one-line doc comment and, if it exposes a primitive at a boundary, a `type-audit:` tag.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate`; a save-format-adjacent change runs `make gate-full` before merge.

---

## File Structure

- `windows/vessel/src/session.rs` — the verb loop. Add `provoke`/`soothe` verbs, register the disposition predicate, expose the final ledger+registry for persistence. *(modify)*
- `windows/vessel/src/liveness.rs` — the drive layer. Add the additive, threshold-gated bias term sourced from player disposition facts. *(modify)*
- `windows/vessel/src/lib.rs` — the run driver; may need to surface the played `World`. *(modify)*
- `cli/src/main.rs` — `cmd_possess` gains `--out` to save the played world. *(modify)*
- `windows/vessel/tests/possession_moves.rs` — existing possession test file; add act/bias/consequence/persistence/why tests. *(modify)*
- `windows/vessel/tests/the_first_mark.rs` — new integration test file for the end-to-end loop. *(create)*

---

## Task 1: The first player-authored fact (`provoke` / `soothe` verbs)

**Files:**
- Modify: `windows/vessel/src/session.rs` (verb dispatch near `:229`–`:257`; registry extension near `:90`)
- Test: `windows/vessel/tests/possession_moves.rs`

**Interfaces:**
- Consumes: `Session::handle(&mut self, line: &str) -> Turn`; `Turn::Out(String)`; the session-owned `ledger: Ledger` and `registry: ConceptRegistry` (`session.rs:60`, `:61`); `hornvale_kernel::{Fact, Value, EntityId}`; `Ledger::commit(fact, &registry)`.
- Produces: two verbs `provoke <who>` / `soothe <who>`; a registered non-functional predicate `disposition-shift`; a public accessor `pub fn committed_disposition_count(&self) -> usize` mirroring `committed_agent_at_count` (`session.rs:173`).

- [ ] **Step 1: Write the failing test**

```rust
// in windows/vessel/tests/possession_moves.rs
#[test]
fn provoke_commits_one_player_authored_disposition_fact() {
    let world = test_world(42); // existing helper in this file; else build_world(Seed(42), ..)
    let mut session = Session::possess(&world, 0.0);
    // move onto a co-located NPC's room if needed; the helper world places NPCs at home.
    let before = session.committed_disposition_count();
    let turn = session.handle("provoke");
    let after = session.committed_disposition_count();
    match turn {
        Turn::Out(s) => assert!(s.to_lowercase().contains("provoke") || s.contains("bristle"),
            "diegetic acknowledgement, got: {s}"),
        other => panic!("expected Out, got {other:?}"),
    }
    assert_eq!(after, before + 1, "exactly one disposition fact committed");
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-vessel --test possession_moves provoke_commits -- --nocapture`
Expected: FAIL — no `provoke` verb / no `committed_disposition_count`.

- [ ] **Step 3: Register the predicate and add the verbs**

In `Session::possess` where `AGENT_AT` is registered (`session.rs:~90`), add:

```rust
// The player's disposition mark — the first player-authored predicate.
// Non-functional (a subject may be provoked and later soothed; each is one
// dated fact). Additive: registering a new predicate perturbs nothing.
registry.register_predicate(
    DISPOSITION_SHIFT,
    false,
    "an agent's disposition was shifted by the possessing player",
);
```

Add the constant near the other predicate consts:

```rust
/// The player-authored disposition-shift predicate (The First Mark).
pub const DISPOSITION_SHIFT: &str = "disposition-shift";
```

In `handle`'s verb match (`session.rs:~229`), add:

```rust
"provoke" => self.act_on_disposition(rest, +1),
"soothe"  => self.act_on_disposition(rest, -1),
```

Add the method:

```rust
/// Commit the first player-authored fact: a signed disposition shift on a
/// co-located NPC. `sign` is +1 (provoke) / -1 (soothe). The fact carries a
/// `player:` provenance so the butterfly-reader (and contradiction-checking)
/// can tell it from the world's own facts.
fn act_on_disposition(&mut self, who: &str, sign: i8) -> Turn {
    let Some(npc) = self.colocated_npc(who) else {
        return Turn::Out("there is no one here to move.".into());
    };
    let verb = if sign >= 0 { "provoke" } else { "soothe" };
    let fact = Fact {
        subject: npc.id,
        predicate: DISPOSITION_SHIFT.to_string(),
        object: Value::Number(sign as f64),
        place: Some(self.here_id()),
        day: Some(self.day),
        provenance: format!("player: {verb}"),
    };
    self.ledger
        .commit(fact, &self.registry)
        .expect("disposition-shift is registered and finite");
    let felt = if sign >= 0 { "bristles" } else { "eases" };
    Turn::Out(format!("You {verb} {}. They {felt}.", npc.name))
}

/// How many player disposition-shift facts the session ledger holds.
pub fn committed_disposition_count(&self) -> usize {
    self.ledger.find(DISPOSITION_SHIFT).count()
}
```

Use the existing co-located-NPC lookup that `needs`/`why` already use (`session.rs` — the `needs` handler resolves a co-located NPC; extract or reuse it as `colocated_npc`). `here_id()`/`self.day` mirror existing accessors; if absent, read the current room id and `self.day` field directly.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-vessel --test possession_moves provoke_commits`
Expected: PASS.

- [ ] **Step 5: Byte-identity guard — no verb used, nothing changes**

```rust
#[test]
fn possession_with_no_act_leaves_session_ledger_unchanged() {
    let world = test_world(42);
    let a = Session::possess(&world, 0.0).session_ledger_json();
    let mut s = Session::possess(&world, 0.0);
    let _ = s.handle("look"); // a read-only verb
    assert_eq!(a, s.session_ledger_json(), "read-only verbs commit nothing");
}
```

Run: `cargo test -p hornvale-vessel --test possession_moves`
Expected: PASS (both tests).

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/session.rs windows/vessel/tests/possession_moves.rs
git commit -m "feat(vessel): the first player-authored fact (provoke/soothe disposition marks)"
```

---

## Task 2: The additive, threshold-gated bias

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the drive value used against the `>= act` seek threshold, `:136`–`:159`)
- Test: `windows/vessel/tests/possession_moves.rs`

**Interfaces:**
- Consumes: the drive value in `[0, 1]` and its `act` threshold (`liveness.rs:154`); the session ledger (to read `DISPOSITION_SHIFT` facts about an NPC); `Ledger::find` / `facts_about`.
- Produces: a pure helper `fn disposition_bias(ledger: &Ledger, npc: EntityId) -> f64` returning the net signed player pressure (Σ of `disposition-shift` objects, scaled), and its addition into the arbitrated drive **before** the `>= act` comparison.

- [ ] **Step 1: Write the failing test (bias tips a latent drive, not a cold one)**

```rust
// windows/vessel/tests/possession_moves.rs
#[test]
fn provoke_tips_a_latent_drive_but_not_a_cold_one() {
    // A latent NPC: its danger/aggression drive sits just below `act`.
    let world = test_world(42);
    // cold case: provoke an NPC whose drive is far from threshold -> no firing
    let mut cold = Session::possess(&world, 0.0);
    cold.handle("provoke stranger"); // choose a cold NPC by name/helper
    assert!(!cold.drive_would_fire("stranger"), "a cold drive stays below act");

    // latent case: provoke the near-threshold NPC -> fires
    let mut hot = Session::possess(&world, 0.0);
    hot.handle("provoke sentry"); // the seeded near-threshold NPC
    assert!(hot.drive_would_fire("sentry"), "provoke tips a latent drive past act");
}
```

`drive_would_fire(name)` is a **test-only** accessor (behind `#[cfg(test)]` or a `pub(crate)` used by the test) that returns whether the arbitrated drive (with bias) is `>= act` for the named co-located NPC. It exists to make the threshold observable; production code reads the same value.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test possession_moves provoke_tips`
Expected: FAIL — no bias term, no `drive_would_fire`.

- [ ] **Step 3: Add the additive bias helper**

```rust
/// The net player pressure on `npc`, folded from committed disposition-shift
/// facts (The First Mark). Additive and bounded: each provoke/soothe nudges
/// the target drive by `DISPOSITION_GAIN`; with no player facts this is 0.0,
/// so arbitration is byte-identical to an unplayed world by construction.
pub(crate) fn disposition_bias(ledger: &Ledger, npc: EntityId) -> f64 {
    let net: f64 = ledger
        .facts_about(npc)
        .filter(|f| f.predicate == crate::session::DISPOSITION_SHIFT)
        .map(|f| match f.object { Value::Number(n) => n, _ => 0.0 })
        .sum();
    net * DISPOSITION_GAIN
}

/// How hard one provoke/soothe leans on a drive. Tuned so a *single* act
/// tips a drive already within one gain-step of `act` (a latent tension) and
/// leaves a cold drive (far below `act`) below it — bias, not manufacture.
pub(crate) const DISPOSITION_GAIN: f64 = /* set in Step 4 against the seed-42 seam */ 0.15;
```

At the seek-threshold comparison (`liveness.rs:154`, `drive >= act`), add the bias to the drive value being compared for the aggression/danger axis the consequence rides:

```rust
let biased = (drive + disposition_bias(ledger, npc.id)).clamp(0.0, 1.0);
let fires = biased >= act;
```

- [ ] **Step 4: Calibrate `DISPOSITION_GAIN` against seed 42 and run**

Inspect the danger/aggression drive values of the seeded NPCs at day 0 (add a temporary `eprintln!` or a throwaway test) to find one NPC within one gain-step of `act` (the "sentry", latent) and one far below (the "stranger", cold). Set `DISPOSITION_GAIN` (and, if needed, pick which named NPCs the test targets) so the latent one crosses on a single `provoke` and the cold one does not. This is the "soft clay" of the slice; document the chosen values in the const's doc comment.

Run: `cargo test -p hornvale-vessel --test possession_moves provoke_tips`
Expected: PASS.

- [ ] **Step 5: Byte-identity property — no player fact, identical arbitration**

```rust
#[test]
fn no_disposition_fact_means_zero_bias() {
    let world = test_world(42);
    let s = Session::possess(&world, 0.0);
    for npc in s.npc_ids() { // pub(crate)/test accessor listing NPC entity ids
        assert_eq!(hornvale_vessel::liveness::disposition_bias(s.ledger_ref(), npc), 0.0);
    }
}
```

Run: `cargo test -p hornvale-vessel --test possession_moves`
Expected: PASS.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/liveness.rs windows/vessel/tests/possession_moves.rs
git commit -m "feat(vessel): additive, threshold-gated disposition bias (bias not manufacture)"
```

---

## Task 3: The consequence fires (one-hop forward integration)

**Files:**
- Modify: `windows/vessel/src/session.rs` (the `wait` tick path, `:336`–`:389`), `windows/vessel/src/liveness.rs` (the tick that commits the fired consequence)
- Test: `windows/vessel/tests/the_first_mark.rs` *(create)*

**Interfaces:**
- Consumes: `wait`'s NPC tick (`DriveMovements`, `session.rs:338`); the biased `fires` flag from Task 2.
- Produces: when a biased drive fires, the tick commits its **discrete** consequence fact (the drive's existing action — e.g. an `agent-at` move toward the player, or the drive's `act` outcome) with a provenance that names the player cause, e.g. `"drive:danger (player-provoked)"`.

- [ ] **Step 1: Write the failing end-to-end test**

```rust
// windows/vessel/tests/the_first_mark.rs  (new file)
use hornvale_vessel::{Session, Turn};
mod common; // reuse test_world if factored; else inline build_world(Seed(42), ..)

#[test]
fn provoke_then_wait_fires_a_consequence_that_would_not_fire_unprovoked() {
    let world = common::test_world(42);

    // control: wait without provoking -> the latent drive does NOT fire
    let mut control = Session::possess(&world, 0.0);
    control.handle("wait");
    let control_consequences = control.committed_consequence_count();

    // treatment: provoke the latent NPC, then wait -> it DOES fire
    let mut treat = Session::possess(&world, 0.0);
    treat.handle("provoke sentry");
    treat.handle("wait");
    let treat_consequences = treat.committed_consequence_count();

    assert!(treat_consequences > control_consequences,
        "the provoked latent tension fired a consequence the control did not");
}
```

`committed_consequence_count()` counts the consequence facts (e.g. `agent-at` facts tagged with the player-provoked provenance, or the drive's action predicate). Add it beside `committed_agent_at_count` (`session.rs:173`).

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test the_first_mark`
Expected: FAIL — the consequence path isn't wired / accessor missing.

- [ ] **Step 3: Wire the firing into the tick**

In the drive tick, when `fires` is true *because of* the bias (i.e. `biased >= act` while `drive < act`), commit the drive's existing action outcome and stamp its provenance to record the player cause:

```rust
let provoked = drive < act && biased >= act;
if fires {
    // ... existing action commit (the agent-at move / act outcome) ...
    let provenance = if provoked { "drive:danger (player-provoked)" } else { "drive:danger" };
    // use `provenance` on the committed consequence Fact
}
```

Keep the consequence **discrete** (one committed fact), quantized at emit by `Ledger::commit`. No continuous chaotic state is checkpointed.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-vessel --test the_first_mark`
Expected: PASS.

- [ ] **Step 5: Determinism — same trace, byte-identical ledger**

```rust
#[test]
fn same_action_trace_is_byte_identical() {
    let world = common::test_world(42);
    let run = |script: &[&str]| {
        let mut s = Session::possess(&world, 0.0);
        for line in script { s.handle(line); }
        s.session_ledger_json()
    };
    let script = ["provoke sentry", "wait", "wait"];
    assert_eq!(run(&script), run(&script), "same seed + same trace -> identical ledger");
}
```

Run: `cargo test -p hornvale-vessel --test the_first_mark`
Expected: PASS.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/session.rs windows/vessel/src/liveness.rs windows/vessel/tests/the_first_mark.rs
git commit -m "feat(vessel): a provoked latent tension fires a consequence (one-hop integration)"
```

---

## Task 4: Persistence — `possess --out` writes the played world

**Files:**
- Modify: `windows/vessel/src/lib.rs` (surface the played `World`), `windows/vessel/src/session.rs` (expose final `ledger` + `registry`), `cli/src/main.rs` (`cmd_possess`, `:418`)
- Test: `windows/vessel/tests/the_first_mark.rs`, plus a CLI-level check

**Interfaces:**
- Consumes: `Session`'s final `ledger: Ledger` and `registry: ConceptRegistry`; `World { seed, registry, ledger }`; `World::save(&Path)`; the possess run driver (`lib.rs:90`).
- Produces: `Session::into_played_world(self, seed: Seed) -> World` (or `run(..) -> (io_result, Option<World>)`); a `--out <PATH>` flag on `possess` that saves the played world; input world untouched.

- [ ] **Step 1: Write the failing round-trip test**

```rust
// windows/vessel/tests/the_first_mark.rs
#[test]
fn played_world_persists_the_mark_across_reload() {
    let world = common::test_world(42);
    let mut s = Session::possess(&world, 0.0);
    s.handle("provoke sentry");
    s.handle("wait");
    let played = s.into_played_world(world.seed);

    // round-trip through JSON exactly as save/load would
    let json = serde_json::to_string(&played).unwrap();
    let reloaded: hornvale_kernel::World = serde_json::from_str(&json).unwrap();

    let player_facts = reloaded.ledger.find("disposition-shift").count();
    assert_eq!(player_facts, 1, "the player's mark survives reload");
    // the played world carries MORE facts than the pristine one
    assert!(reloaded.ledger.len() > world.ledger.len(), "the world remembers");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test the_first_mark played_world_persists`
Expected: FAIL — no `into_played_world`.

- [ ] **Step 3: Implement `into_played_world`**

```rust
/// Consume the session and fold its evolved ledger + registry into a saveable
/// `World`. The evolved ledger is the bubble's forward integration made
/// history (player facts + the consequences they triggered + NPC ticks). The
/// caller supplies the seed; the input world is never mutated in place.
pub fn into_played_world(self, seed: Seed) -> World {
    World { seed, registry: self.registry, ledger: self.ledger }
}
```

(Confirm `World`'s field visibility; if fields are private, add a `World::from_parts(seed, registry, ledger)` constructor in the kernel with a doc comment and `type-audit` tag as needed.)

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-vessel --test the_first_mark played_world_persists`
Expected: PASS.

- [ ] **Step 5: Wire `--out` into the CLI**

In `cmd_possess` (`cli/src/main.rs:418`), after the interactive run returns the played world, if `--out` is present, save it:

```rust
if let Some(out) = flag_value(args, "--out") {
    played.save(std::path::Path::new(out))
        .map_err(|e| format!("saving {out}: {e}"))?;
    eprintln!("played world written to {out} ({} facts)", played.ledger.len());
}
```

The interactive `run` must return the played `World` (thread `Session::into_played_world` through `run`'s return, or expose the session so the CLI folds it). The input `--world` file is read-only; only `--out` is written.

- [ ] **Step 6: Manual CLI smoke + fmt + commit**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/w.json
printf 'provoke sentry\nwait\nrelease\n' | cargo run -p hornvale -- possess --world /tmp/w.json --out /tmp/w2.json
# expect: /tmp/w2.json exists, has more facts than /tmp/w.json, and /tmp/w.json is unchanged
cargo fmt
git add windows/vessel/src/lib.rs windows/vessel/src/session.rs cli/src/main.rs windows/vessel/tests/the_first_mark.rs
git commit -m "feat(vessel,cli): possess --out persists the played world (the world remembers)"
```

---

## Task 5: The butterfly-reader — `why`, aimed at the player's hand

**Files:**
- Modify: `windows/vessel/src/session.rs` (the `why <who>` verb) — ensure it recounts the provoked NPC including the player-provenance fact and the fired consequence
- Test: `windows/vessel/tests/the_first_mark.rs`

**Interfaces:**
- Consumes: `hornvale_historiography::recount(world, entity)` (already renders provenance, historiography/src/lib.rs:32); the played world or the session ledger.
- Produces: `why <npc>` output that surfaces the player's fingerprint (`player: provoke`) and the consequence it seeded — the traceable chain.

- [ ] **Step 1: Write the failing trace test**

```rust
// windows/vessel/tests/the_first_mark.rs
#[test]
fn why_traces_the_consequence_back_to_the_players_hand() {
    let world = common::test_world(42);
    let mut s = Session::possess(&world, 0.0);
    s.handle("provoke sentry");
    s.handle("wait");
    let out = match s.handle("why sentry") {
        Turn::Out(text) => text,
        other => panic!("expected Out, got {other:?}"),
    };
    assert!(out.contains("player: provoke"), "the wake names the player's own act");
    assert!(out.to_lowercase().contains("provoked"),
        "the consequence is attributed to the provocation");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel --test the_first_mark why_traces`
Expected: FAIL — `why` doesn't yet surface player provenance for the target, or the consequence isn't attributed.

- [ ] **Step 3: Ensure `why` recounts the played state including player facts**

The `why <who>` handler already recounts an NPC's history. Point it at the *session's evolving* ledger (not the frozen input world) so the just-committed player fact and consequence appear. If `why` currently builds a `World` from the frozen input, build the recount view from `into_played_world`-equivalent parts (the session ledger + registry) or call `recount` against a `World` assembled from the session's live ledger. `recount` already prints each fact's `provenance` (historiography/src/lib.rs:53), so no historiography change is needed — only that the recounted ledger is the evolved one.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-vessel --test the_first_mark why_traces`
Expected: PASS.

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/vessel/src/session.rs windows/vessel/tests/the_first_mark.rs
git commit -m "feat(vessel): why traces the fired consequence back to the player's own act"
```

---

## Task 6: Byte-identity gate, artifact freshness, and campaign close

**Files:**
- Verify only (no new production code); then docs per Definition of Done.
- Create: `book/src/chronicle/the-first-mark.md`, `docs/retrospectives/the-first-mark.md`

**Interfaces:** none — this is the evidence + docs task.

- [ ] **Step 1: The unplayed world is byte-identical**

Run:
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > /tmp/almanac.md
git stash list # ensure clean; then compare against a main-built almanac
```
Expected: a seed-42 `new` world and its almanac are **identical** to `main` (the additive-latent guarantee). If not, a player predicate leaked into the unplayed path — fix before proceeding.

- [ ] **Step 2: Type-audit clean**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: PASS (any new pub-boundary primitive — e.g. a `World::from_parts` return, `disposition_bias` — carries a verdict tag).

- [ ] **Step 3: Full gate (save-format-adjacent → gate-full before merge)**

Run: `make gate` then, before merge, `make gate-full` (Bash `timeout: 3600000`).
Expected: green. Per memory, a boundary change runs the full gate.

- [ ] **Step 4: Regenerate committed artifacts + drift check**

Run the artifact commands from `.github/workflows/ci.yml` ("Artifacts are current" step) and:
```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: no drift (unplayed artifacts unchanged). If a new gallery transcript for The First Mark is wanted, generate it with `possess --script` and commit it deliberately.

- [ ] **Step 5: Chronicle, registry flips, retro (Definition of Done, 0020/0030)**

- Write `book/src/chronicle/the-first-mark.md` (the campaign narrative: the first player-authored fact, bias-not-manufacture, the world remembers).
- Flip idea-registry rows: UNI-32 `spec'd → shipped`, UNI-34 `spec'd → shipped`; leave UNI-33/35/36/37 `raw` (deferred), each `Where` still valid. Re-run `cargo test -p hornvale --test docs_consistency`.
- If this resolves/moves an open-questions bet, re-score that chapter (decision 0030).
- Write the one-page retrospective `docs/retrospectives/the-first-mark.md` (process lessons; promote the `followups.md` items).

- [ ] **Step 6: Final fmt + commit**

```bash
cargo fmt
git add -A
git commit -m "docs(the-first-mark): chronicle, retro, registry flips, artifact freshness sweep"
```
