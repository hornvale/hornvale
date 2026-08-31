# The Repertory Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Found `repertory/`, a fourth corpus family whose verdict comes from an actual run, and leave `the-orange` standing red at its first missing beat.

**Architecture:** A corpus JSON file (data) holds scenes; a resolver under `cli/tests/suite/` (code) drives `hornvale possess --script --snapshot` against each scene's recorded witness world and evaluates beat-level assertions against the `vessel/session/v2` snapshot. No item may be satisfiable by a declaration. Five-valued verdicts with a floor ratchet held in code.

**Tech Stack:** Rust 2024, `serde_json` (already a direct `cli` dependency), `std::process::Command` against `env!("CARGO_BIN_EXE_hornvale")`. No new crates — the workspace allowlist is `serde`, `serde_json`, `libm` (decision 0004/0041).

**Spec:** `docs/superpowers/specs/2026-08-30-the-repertory-design.md`

## Global Constraints

- **No new external crates.** `ALLOWED_EXTERNAL` in `cli/tests/architecture.rs` is the allowlist; adding one fails the layering test.
- **No `HashMap`/`HashSet`** anywhere — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** `Instant` is banned in tests.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the most common review finding.
- **The corpus is data; the resolver is code** (decision 0011). Nothing in `windows/vessel` or `domains/*` may read a corpus file.
- **No item may be satisfiable by a declaration** (spec section 3). If a scene could go green by adding a registry row, it belongs in `tropes/` or `systems/`.
- **Assertions are structural, never golden strings** (spec section 7). v1 has no prose assertion at all; adding one is a followup, not an improvisation.

## Measured facts this plan rests on

Each was established by running the command, not by reading code. Re-run any of them if a step behaves unexpectedly.

| fact | value |
| --- | --- |
| `possess --script <P> --snapshot <S>` | exit 0; markdown transcript to stdout; snapshot written |
| snapshot `schema` | `"vessel/session/v2"`, 20,372 bytes at the witness below |
| snapshot top-level keys | `day, known, narration, schema, self, sensed, social, spatial, turn` |
| cost per run, warm binary | ~5.4 s |
| **The witness used by every seed scene** | `--seed 42 --target most-populous-settlement` (no `--day`) |
| at that witness, opening | `/self/room = 890961927`, `/day = 0.0` |
| after `go e` | `/self/room = 890962023`, `/day = 0.10167` |
| after `wait 1` | `/self/room = 890961927`, `/day = 1.0` |
| `/social/0/label` at opening | `"hobgoblin of Naabeena"` (present) |

**Note the second row of that pair:** `go e` advances the day as well as the room. A beat asserting that walking leaves `/day` unchanged would fail; do not write one.

---

### Task 1: The corpus file, the record types, and the freeze guard

**Files:**
- Create: `repertory/the-founding.scene.json`
- Create: `cli/tests/suite/repertory_corpus.rs`
- Modify: `cli/tests/suite.rs` (add the module declaration, alphabetical among its neighbours)

**Interfaces:**
- Consumes: nothing.
- Produces: `fn repo_root() -> PathBuf`; `fn load(corpus: &str) -> Vec<Scene>`; `struct Scene { id: String, title: String, control: u64, witness: Witness, beta: bool, declared: Option<String>, beats: Vec<Beat> }`; `struct Witness { seed: u64, target: String, day: Option<String> }`; `struct Beat { id: String, description: String, script: Vec<String>, assertion: Assertion }`; `enum Assertion { SnapshotEquals { pointer: String, value: serde_json::Value }, SnapshotPresent { pointer: String }, SnapshotChanges { pointer: String }, SnapshotUnchanged { pointer: String } }`.

- [ ] **Step 1: Write the corpus file**

Create `repertory/the-founding.scene.json`. The four scenes are the positive control (spec section 11) — every one of their assertions was verified to hold at the witness before this plan was written.

```json
{
  "name": "the-founding",
  "provenance": "Authored 2026-08-30 in the brainstorm that opened The Repertory. These four are the instrument's positive control: each was run and verified to pass before the resolver existed, so the resolver's first AUTHORED is one it has actually earned.",
  "scenes": [
    {
      "id": "walk-changes-the-room",
      "title": "A body walks a compass exit and the room it senses changes",
      "provenance": "authored",
      "control": 80,
      "beta": true,
      "witness": { "seed": 42, "target": "most-populous-settlement" },
      "beats": [
        {
          "id": "b1",
          "description": "the body senses a room before it moves",
          "script": ["look"],
          "assertion": { "kind": "snapshot_present", "pointer": "/self/room" }
        },
        {
          "id": "b2",
          "description": "after walking east, the room is a different room",
          "script": ["go e"],
          "assertion": { "kind": "snapshot_changes", "pointer": "/self/room" }
        }
      ]
    },
    {
      "id": "waiting-moves-the-day",
      "title": "A body waits and the day above it moves",
      "provenance": "authored",
      "control": 80,
      "beta": true,
      "witness": { "seed": 42, "target": "most-populous-settlement" },
      "beats": [
        {
          "id": "b1",
          "description": "waiting a day advances the world's clock",
          "script": ["wait 1"],
          "assertion": { "kind": "snapshot_changes", "pointer": "/day" }
        }
      ]
    },
    {
      "id": "waiting-does-not-move-the-body",
      "title": "A body waits and does not thereby travel",
      "provenance": "authored; a negative-space scene (spec section 7) — it asserts what must NOT happen",
      "control": 80,
      "beta": true,
      "witness": { "seed": 42, "target": "most-populous-settlement" },
      "beats": [
        {
          "id": "b1",
          "description": "time passes and the body stays where it stood",
          "script": ["wait 1"],
          "assertion": { "kind": "snapshot_unchanged", "pointer": "/self/room" }
        }
      ]
    },
    {
      "id": "co-location-is-observable",
      "title": "A body can tell that it shares its room with someone",
      "provenance": "authored",
      "control": 80,
      "beta": true,
      "witness": { "seed": 42, "target": "most-populous-settlement" },
      "beats": [
        {
          "id": "b1",
          "description": "a co-located creature is named to the possessing will",
          "script": ["look"],
          "assertion": { "kind": "snapshot_present", "pointer": "/social/0/label" }
        }
      ]
    }
  ]
}
```

- [ ] **Step 2: Write the resolver and the freeze guard**

Create `cli/tests/suite/repertory_corpus.rs`:

```rust
//! Resolves the `repertory/` corpus: scenes the world must be able to play.
//!
//! The corpus is DATA (`repertory/*.scene.json`) and this file is its
//! RESOLVER (decision 0011). Nothing in `windows/vessel` or `domains/*`
//! reads a corpus file.
//!
//! **No scene may be satisfiable by a declaration.** This family exists
//! because `sentence_corpus.rs` says of its own `IMPLEMENTED_DEMANDS` that
//! it "is a hand-maintained declaration and nothing mechanically proves
//! it... a token added on optimism moves the score without moving the
//! grammar, which would make the instrument worse than no instrument,
//! because it would read as evidence." A scene's verdict here comes from
//! running `hornvale possess` and reading the snapshot it writes.

use serde_json::Value;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The world a scene was found in (spec section 6). Scenes are FOUND, never
/// staged: a witness names a real `(seed, target, day)` a human or a search
/// actually located, so an AUTHORED verdict is evidence about Hornvale
/// rather than about the staging.
#[derive(Debug, Clone)]
struct Witness {
    seed: u64,
    target: String,
    day: Option<String>,
}

/// What a beat asserts. Structural only — a golden string would redden on
/// any prose change, so v1 carries no prose assertion at all.
#[derive(Debug, Clone)]
enum Assertion {
    /// The JSON pointer resolves to exactly this value.
    SnapshotEquals { pointer: String, value: Value },
    /// The JSON pointer resolves to anything at all.
    SnapshotPresent { pointer: String },
    /// The pointer's value after the beat differs from the opening.
    SnapshotChanges { pointer: String },
    /// The pointer's value after the beat equals the opening. The
    /// negative-space form (spec section 7): a corpus that only ever asserts
    /// what CAN happen ratchets toward permissiveness.
    SnapshotUnchanged { pointer: String },
}

/// One step of a scene. Beats are mandatory rather than decorative: a
/// monolithic red tells an implementer nothing, and a beat-level red names
/// the missing capability, which is what makes the corpus generate campaigns
/// instead of merely scoring them.
#[derive(Debug, Clone)]
struct Beat {
    id: String,
    description: String,
    script: Vec<String>,
    assertion: Assertion,
}

/// A scene the world is supposed to be able to play.
#[derive(Debug, Clone)]
struct Scene {
    id: String,
    title: String,
    control: u64,
    beta: bool,
    declared: Option<String>,
    witness: Witness,
    beats: Vec<Beat>,
}

fn as_str(v: &Value, key: &str) -> String {
    v.get(key)
        .and_then(Value::as_str)
        .unwrap_or_else(|| panic!("scene field `{key}` is a string"))
        .to_string()
}

fn parse_assertion(v: &Value) -> Assertion {
    let pointer = as_str(v, "pointer");
    match as_str(v, "kind").as_str() {
        "snapshot_equals" => Assertion::SnapshotEquals {
            pointer,
            value: v.get("value").expect("snapshot_equals has a value").clone(),
        },
        "snapshot_present" => Assertion::SnapshotPresent { pointer },
        "snapshot_changes" => Assertion::SnapshotChanges { pointer },
        "snapshot_unchanged" => Assertion::SnapshotUnchanged { pointer },
        other => panic!(
            "unknown assertion kind `{other}`. The vocabulary is closed on \
             purpose: a scene that needs a new kind is a deliberate edit to \
             this resolver, not a field a corpus may invent."
        ),
    }
}

fn parse_scene(v: &Value) -> Scene {
    let w = v.get("witness").expect("every scene carries a witness");
    Scene {
        id: as_str(v, "id"),
        title: as_str(v, "title"),
        control: v
            .get("control")
            .and_then(Value::as_u64)
            .expect("control is 0..=100"),
        beta: v.get("beta").and_then(Value::as_bool).unwrap_or(false),
        declared: v
            .get("declared")
            .and_then(Value::as_str)
            .map(str::to_string),
        witness: Witness {
            seed: w.get("seed").and_then(Value::as_u64).expect("witness seed"),
            target: as_str(w, "target"),
            day: w.get("day").and_then(Value::as_str).map(str::to_string),
        },
        beats: v
            .get("beats")
            .and_then(Value::as_array)
            .expect("every scene has beats")
            .iter()
            .map(|b| Beat {
                id: as_str(b, "id"),
                description: as_str(b, "description"),
                script: b
                    .get("script")
                    .and_then(Value::as_array)
                    .expect("every beat has a script")
                    .iter()
                    .map(|s| s.as_str().expect("script lines are strings").to_string())
                    .collect(),
                assertion: parse_assertion(b.get("assertion").expect("every beat asserts")),
            })
            .collect(),
    }
}

fn load(corpus: &str) -> Vec<Scene> {
    let path = repo_root().join("repertory").join(corpus);
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{} is committed: {e}", path.display()));
    let v: Value = serde_json::from_str(&text).expect("the corpus is valid JSON");
    v.get("scenes")
        .and_then(Value::as_array)
        .expect("a corpus has a scenes array")
        .iter()
        .map(parse_scene)
        .collect()
}

/// The founding corpus's frozen scene count. Changing this number is the
/// deliberate act; changing the corpus without it is the drift (decision
/// 0016).
const FOUNDING_SCENES: usize = 4;

#[test]
fn the_founding_corpus_is_frozen_at_its_authored_size() {
    let scenes = load("the-founding.scene.json");
    assert_eq!(
        scenes.len(),
        FOUNDING_SCENES,
        "the corpus moved. If that was deliberate, change FOUNDING_SCENES in \
         the same commit and say why in the message; a corpus that drifts \
         under a measurement makes every earlier score incomparable."
    );
}

#[test]
fn every_scene_carries_a_witness_and_at_least_one_beat() {
    for s in load("the-founding.scene.json") {
        assert!(
            !s.beats.is_empty(),
            "scene `{}` has no beats; a scene with nothing to assert cannot \
             produce a verdict from a run",
            s.id
        );
        assert!(
            !s.witness.target.is_empty(),
            "scene `{}` has an empty witness target",
            s.id
        );
    }
}
```

- [ ] **Step 3: Register the module**

In `cli/tests/suite.rs`, add the declaration between `mod release_determinism;` and `mod repose_byte_identity;` so the list stays alphabetical:

```rust
#[path = "suite/repertory_corpus.rs"]
mod repertory_corpus;
```

- [ ] **Step 4: Run the tests**

Run: `cargo nextest run -p hornvale --test suite -E 'test(repertory_corpus)'`

Expected: 2 passed.

Some fields (`title`, `control`, `beta`, `declared`, `description`, `script`, `assertion`) are parsed but not yet read by any test, so clippy will report dead code. **Decision rule, not a prediction:**
- clippy reports dead code on fields Tasks 2-5 consume -> add `#[allow(dead_code)]` to the struct with the comment `// Tasks 2-5 read these.` and remove the allow in the task that reads them.
- clippy reports anything else -> stop and read it; it is not this.

- [ ] **Step 5: Track the new directory, then commit**

`git diff --exit-code` is silently vacuous against a path with no index entry, so `repertory/` must be added before anything can ever check it. Do **not** add it to `docs/generated-paths.txt` — the corpus is hand-authored data, not a generated artifact (spec section 15.1).

Run, in order:
- `cargo fmt`
- `cargo clippy --workspace --all-targets -- -D warnings`
- `git add repertory/the-founding.scene.json cli/tests/suite/repertory_corpus.rs cli/tests/suite.rs`
- `git status --porcelain repertory/` — expect no output, meaning nothing untracked is left
- `git commit -m "feat(repertory): the scene record, the loader, and the freeze guard"`

---

### Task 2: The runner

**Files:**
- Modify: `cli/tests/suite/repertory_corpus.rs`

**Interfaces:**
- Consumes: `Witness`, `repo_root()` from Task 1.
- Produces: `fn run_at(witness: &Witness, script: &[String]) -> Value` — drives the CLI once and returns the parsed `vessel/session/v2` snapshot. Panics with the CLI's stderr on a non-zero exit.

- [ ] **Step 1: Write the failing test**

Append to `cli/tests/suite/repertory_corpus.rs`:

```rust
#[test]
fn the_runner_returns_a_v2_snapshot_from_a_real_possession() {
    let w = Witness {
        seed: 42,
        target: "most-populous-settlement".to_string(),
        day: None,
    };
    let snap = run_at(&w, &["look".to_string()]);
    assert_eq!(
        snap.pointer("/schema").and_then(Value::as_str),
        Some("vessel/session/v2"),
        "the runner must return the session snapshot the resolver asserts \
         against; anything else means --snapshot changed shape"
    );
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo nextest run -p hornvale --test suite -E 'test(the_runner_returns_a_v2_snapshot)'`

Expected: FAIL to compile, `cannot find function run_at`.

- [ ] **Step 3: Implement the runner**

Add above the tests:

```rust
/// Drive one possession at `witness`, feeding `script` to `--script`, and
/// return the `vessel/session/v2` snapshot it writes.
///
/// The scene's verdict comes from THIS — an actual run — rather than from
/// any declaration in the corpus file. Measured cost is ~5.4 s per call
/// against a warm binary.
fn run_at(witness: &Witness, script: &[String]) -> Value {
    let dir = std::env::temp_dir().join(format!(
        "hv-repertory-{}-{}",
        std::process::id(),
        witness.seed
    ));
    std::fs::create_dir_all(&dir).expect("scratch dir");
    let script_path = dir.join("script.txt");
    let snap_path = dir.join("snapshot.json");
    let mut body = script.join("\n");
    body.push('\n');
    std::fs::write(&script_path, body).expect("write script");

    let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"));
    cmd.arg("possess")
        .arg("--seed")
        .arg(witness.seed.to_string())
        .arg("--target")
        .arg(&witness.target)
        .arg("--script")
        .arg(&script_path)
        .arg("--snapshot")
        .arg(&snap_path);
    if let Some(day) = &witness.day {
        cmd.arg("--day").arg(day);
    }
    let out = cmd.output().expect("the hornvale binary runs");
    assert!(
        out.status.success(),
        "possess failed at seed {} target {}: {}",
        witness.seed,
        witness.target,
        String::from_utf8_lossy(&out.stderr)
    );
    let text = std::fs::read_to_string(&snap_path).expect("--snapshot wrote a file");
    serde_json::from_str(&text).expect("the snapshot is valid JSON")
}
```

- [ ] **Step 4: Run it to verify it passes**

Run: `cargo nextest run -p hornvale --test suite -E 'test(the_runner_returns_a_v2_snapshot)'`

Expected: PASS, in roughly 5-10 s.

- [ ] **Step 5: Commit**

Run, in order: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `git add cli/tests/suite/repertory_corpus.rs`; `git commit -m "feat(repertory): drive a real possession and return its snapshot"`

---

### Task 3: Beat evaluation

**Files:**
- Modify: `cli/tests/suite/repertory_corpus.rs`

**Interfaces:**
- Consumes: `Scene`, `Beat`, `Assertion`, `Witness`, `run_at` from Tasks 1-2.
- Produces: `fn evaluate(scene: &Scene) -> Result<(), String>` — `Ok(())` when every beat holds; `Err(beat_id)` naming the FIRST failing beat.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn every_founding_scene_passes_every_beat() {
    for scene in load("the-founding.scene.json") {
        assert_eq!(
            evaluate(&scene),
            Ok(()),
            "founding scene `{}` ({}) failed. These four are the instrument's \
             POSITIVE CONTROL: each was run and verified before the resolver \
             existed. A red here means the resolver is broken, or the world \
             changed under a scene that used to play — not that the scene was \
             ever aspirational.",
            scene.id,
            scene.title
        );
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo nextest run -p hornvale --test suite -E 'test(every_founding_scene_passes)'`

Expected: FAIL to compile, `cannot find function evaluate`.

- [ ] **Step 3: Implement evaluation**

`SnapshotChanges` and `SnapshotUnchanged` compare against the scene's OPENING, so each needs a baseline run. The four founding scenes share one witness, so this is one extra run per scene, not per beat.

```rust
/// The opening state at a witness — the baseline `changes`/`unchanged`
/// compare against. `look` is the cheapest script that leaves the world
/// where it found it.
fn opening_at(witness: &Witness) -> Value {
    run_at(witness, &["look".to_string()])
}

/// Run every beat of `scene` in order and return the id of the FIRST that
/// fails, or `Ok(())` if all hold.
fn evaluate(scene: &Scene) -> Result<(), String> {
    let opening = opening_at(&scene.witness);
    for beat in &scene.beats {
        let after = run_at(&scene.witness, &beat.script);
        let held = match &beat.assertion {
            Assertion::SnapshotEquals { pointer, value } => after.pointer(pointer) == Some(value),
            Assertion::SnapshotPresent { pointer } => after.pointer(pointer).is_some(),
            Assertion::SnapshotChanges { pointer } => {
                let (a, b) = (opening.pointer(pointer), after.pointer(pointer));
                a.is_some() && b.is_some() && a != b
            }
            Assertion::SnapshotUnchanged { pointer } => {
                let (a, b) = (opening.pointer(pointer), after.pointer(pointer));
                a.is_some() && a == b
            }
        };
        if !held {
            return Err(beat.id.clone());
        }
    }
    Ok(())
}
```

Both `SnapshotChanges` and `SnapshotUnchanged` require the pointer to RESOLVE on both sides. A pointer that resolves nowhere would otherwise satisfy `unchanged` vacuously (`None == None`), which is the shape of a guard that can never fail.

- [ ] **Step 4: Run it to verify it passes**

Run: `cargo nextest run -p hornvale --test suite -E 'test(every_founding_scene_passes)'`

Expected: PASS. Five beats plus four baselines, so roughly 45-60 s.

- [ ] **Step 5: Prove the evaluator can FAIL, not just pass**

A green from an evaluator that has never returned `Err` is a green nothing earned. This test pins the failing direction using a scene built in the test, not one added to the corpus:

```rust
#[test]
fn a_beat_whose_assertion_does_not_hold_names_that_beat() {
    let scene = Scene {
        id: "control-must-fail".to_string(),
        title: "A pointer that cannot resolve fails, and says which beat".to_string(),
        control: 100,
        beta: false,
        declared: None,
        witness: Witness {
            seed: 42,
            target: "most-populous-settlement".to_string(),
            day: None,
        },
        beats: vec![Beat {
            id: "b-impossible".to_string(),
            description: "a pointer no snapshot carries".to_string(),
            script: vec!["look".to_string()],
            assertion: Assertion::SnapshotPresent {
                pointer: "/no/such/pointer".to_string(),
            },
        }],
    };
    assert_eq!(evaluate(&scene), Err("b-impossible".to_string()));
}
```

- [ ] **Step 6: Run both, then commit**

Run: `cargo nextest run -p hornvale --test suite -E 'test(repertory_corpus)'`

Expected: 5 passed.

Then, in order: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `git add cli/tests/suite/repertory_corpus.rs`; `git commit -m "feat(repertory): beat-level evaluation, with a control that proves it can fail"`

---

### Task 4: The five-valued verdict and the floor ratchet

**Files:**
- Modify: `cli/tests/suite/repertory_corpus.rs`

**Interfaces:**
- Consumes: `Scene`, `evaluate` from Tasks 1-3.
- Produces: `enum Verdict { Absent(String), Authored, Declared, StaleDecl }` with `fn name(&self) -> &'static str`; `fn verdict(scene: &Scene) -> Verdict`; `const FLOORS: &[(&str, &str)]`.

`PARTIAL` from spec section 9 is **not** implemented here and its variant is not created. `evaluate` stops at the first failing beat, so this resolver cannot yet distinguish "the scene never started" from "the scene ran and beat 4 failed" — and a variant no code can produce is a declaration of the exact kind spec section 3 forbids. Record it as a followup; add it with the code that can tell the two apart.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn no_scene_has_fallen_below_its_recorded_floor() {
    for scene in load("the-founding.scene.json") {
        let floor = FLOORS
            .iter()
            .find(|(id, _)| *id == scene.id)
            .unwrap_or_else(|| {
                panic!(
                    "scene `{}` has no floor. Every scene needs one in the \
                     same commit that adds it, or the ratchet has a hole \
                     exactly the shape of the newest scene.",
                    scene.id
                )
            })
            .1;
        let got = verdict(&scene);
        assert_eq!(
            got.name(),
            floor,
            "scene `{}` is {} but its floor is {}. A scene may not regress \
             without a recorded reason: if this drop is deliberate, change \
             the floor in the same commit and say why in the message.",
            scene.id,
            got.name(),
            floor
        );
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo nextest run -p hornvale --test suite -E 'test(no_scene_has_fallen_below)'`

Expected: FAIL to compile, `cannot find value FLOORS`.

- [ ] **Step 3: Implement the verdict and the floors**

```rust
/// A scene's standing. Five-valued for the reason `tropes check`,
/// seam-guard and the type audit's `waiver(...)` are: a binary gate over a
/// growing roster goes red on day one and stays red, which trains everyone
/// to ignore it.
#[derive(Debug, PartialEq, Eq)]
enum Verdict {
    /// The scene does not play; the string names the first failing beat.
    Absent(String),
    /// Every beat holds against the scene's witness.
    Authored,
    /// A known-absent scene, declared WITH a reason. Green, printed loudly.
    Declared,
    /// Declared absent, but it now passes — delete the declaration.
    StaleDecl,
}

impl Verdict {
    fn name(&self) -> &'static str {
        match self {
            Verdict::Absent(_) => "ABSENT",
            Verdict::Authored => "AUTHORED",
            Verdict::Declared => "DECLARED",
            Verdict::StaleDecl => "STALE-DECL",
        }
    }
}

fn verdict(scene: &Scene) -> Verdict {
    match (evaluate(scene), scene.declared.is_some()) {
        (Ok(()), false) => Verdict::Authored,
        (Ok(()), true) => Verdict::StaleDecl,
        (Err(beat), false) => Verdict::Absent(beat),
        (Err(_), true) => Verdict::Declared,
    }
}

/// Each scene's floor. A scene may not fall below its floor without a
/// deliberate edit here in the same commit.
const FLOORS: &[(&str, &str)] = &[
    ("walk-changes-the-room", "AUTHORED"),
    ("waiting-moves-the-day", "AUTHORED"),
    ("waiting-does-not-move-the-body", "AUTHORED"),
    ("co-location-is-observable", "AUTHORED"),
];
```

`StaleDecl` is what keeps a declaration honest: a one-directional acknowledgement can only ever be satisfied, so it rots. This one fails the moment the scene starts working.

- [ ] **Step 4: Run it to verify it passes**

Run: `cargo nextest run -p hornvale --test suite -E 'test(repertory_corpus)'`

Expected: 6 passed.

- [ ] **Step 5: Commit**

In order: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `git add cli/tests/suite/repertory_corpus.rs`; `git commit -m "feat(repertory): five-valued verdicts and the per-scene floor ratchet"`

---

### Task 5: the-orange, the standing red

**Files:**
- Create: `repertory/the-orange.scene.json`
- Modify: `cli/tests/suite/repertory_corpus.rs`

**Interfaces:**
- Consumes: everything from Tasks 1-4.
- Produces: no new functions. Adds `const ORANGE_SCENES: usize` and extends `FLOORS`.

This is the scene the project owner chose, and it is expected to be `DECLARED` for several campaigns (spec section 12). Its beats are the first arc's task list.

- [ ] **Step 1: Write the corpus file**

The witness is the same seed-42 settlement world every founding scene uses.

```json
{
  "name": "the-orange",
  "provenance": "Chosen by Nathan, 2026-08-30, as the founding scene of the repertory: 'a goblin and a drow in a room and the drow is trying to convince the goblin to give him a particularly ripe orange.' Expected DECLARED for several campaigns; its beats are the arc's task list.",
  "scenes": [
    {
      "id": "the-orange",
      "title": "A drow wants a ripe orange that a goblin has",
      "provenance": "authored",
      "control": 60,
      "beta": true,
      "declared": "Beats beyond the first need capabilities no campaign has built. Objects have a property vocabulary but no instances: windows/vessel/src/affordance.rs is 'only the vocabulary and the table', held 'vessel-locally -- build-state, not world-state... nothing here is serialized', and the affordance query is 'a later task'. The vessel/session/v2 snapshot has no inventory channel. No verb in the 25-entry in-character list addresses another creature: ask asks the body you wear how IT feels, write speaks a line of Common into your OWN margin, and the operator instruments provoke and soothe carry no utterance. Delete this declaration the moment the scene passes -- STALE-DECL will insist.",
      "witness": { "seed": 42, "target": "most-populous-settlement" },
      "beats": [
        {
          "id": "b1-co-located",
          "description": "two creatures share a room, and the possessing will can tell",
          "script": ["look"],
          "assertion": { "kind": "snapshot_present", "pointer": "/social/0/label" }
        },
        {
          "id": "b2-holds-an-object",
          "description": "one of them holds a thing that can change hands",
          "script": ["look"],
          "assertion": { "kind": "snapshot_present", "pointer": "/social/0/holding" }
        }
      ]
    }
  ]
}
```

Only beats 1 and 2 are encoded. Beats 3-7 from spec section 12 are **not** written as beats, because a beat whose assertion vocabulary does not exist yet cannot be evaluated — writing them now would put five unevaluatable records in a corpus whose whole premise is that a verdict comes from a run. They live in spec section 12's table and in the followups until each has a snapshot surface to assert against.

- [ ] **Step 2: Write the tests**

```rust
/// The orange corpus's frozen scene count.
const ORANGE_SCENES: usize = 1;

#[test]
fn the_orange_is_frozen_at_its_authored_size() {
    assert_eq!(load("the-orange.scene.json").len(), ORANGE_SCENES);
}

#[test]
fn the_orange_stands_declared_at_its_first_missing_beat() {
    let scene = load("the-orange.scene.json")
        .into_iter()
        .find(|s| s.id == "the-orange")
        .expect("the-orange is committed");
    assert!(
        scene.declared.is_some(),
        "the-orange must carry a reason; a reasonless declaration is a parse \
         error in every sibling instrument and is one here too"
    );
    let got = verdict(&scene);
    assert_eq!(
        got,
        Verdict::Declared,
        "the-orange is {}. If it is now AUTHORED, that is the campaign \
         landing: delete the `declared` field, raise its floor, and say so \
         in the chronicle.",
        got.name()
    );
}
```

- [ ] **Step 3: Run them**

Run: `cargo nextest run -p hornvale --test suite -E 'test(the_orange)'`

This task's test-first order is inverted by necessity — the corpus file IS the implementation, so both tests pass on the first run after Step 1. **Decision rule:**
- both pass -> expected; go to Step 4.
- `the_orange_stands_declared` reports `AUTHORED` -> beat 2's pointer `/social/0/holding` resolved, meaning the snapshot gained a holding channel since this plan was written. STOP and tell the campaign owner: the scene's premise changed and the declaration is stale on arrival.
- it reports `ABSENT` -> `verdict` is not routing a declared scene correctly; re-read Task 4 Step 3.

- [ ] **Step 4: Extend the floors and confirm the file is green**

Add to `FLOORS`:

```rust
    ("the-orange", "DECLARED"),
```

The floor test iterates `the-founding.scene.json` only, so extend it to cover both corpora by iterating the concatenation of `load("the-founding.scene.json")` and `load("the-orange.scene.json")`.

Run: `cargo nextest run -p hornvale --test suite -E 'test(repertory_corpus)'`

Expected: 8 passed.

- [ ] **Step 5: Track the new file, then commit**

A new file dropped into an already-tracked directory inherits the vacuous-diff hazard in full: `repertory/`'s other tracked file keeps every tracked-ness check green while `git diff` cannot see this one at all.

In order: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `git add repertory/the-orange.scene.json cli/tests/suite/repertory_corpus.rs`; `git status --porcelain repertory/` (expect no output); `git commit -m "feat(repertory): the-orange, declared absent at its first missing beat"`

---

### Task 6: Gate placement, then close

**Files:**
- Create: `book/src/chronicle/the-repertory.md`
- Create: `docs/retrospectives/the-repertory.md`
- Modify: `book/src/SUMMARY.md`

**Interfaces:**
- Consumes: everything.
- Produces: no code.

- [ ] **Step 1: Measure what the resolver costs**

Run: `cargo nextest run -p hornvale --test suite -E 'test(repertory_corpus)'` and record the reported wall time. Spec section 15.5 leaves gate placement open, to be settled against a measurement rather than an estimate.

- [ ] **Step 2: Place the tests by the measured cost**

**Decision rule, not a prediction:**
- total under 60 s -> leave the tests OUT of `docs/timings/subfloor-roster.tsv`. The commit gate's own test execution is ~6 s today; adding a minute to every commit to run a corpus that moves at campaign cadence is the mispricing decision 0132 exists to prevent. The stage gate runs the full workspace suite, so they are covered there with no edit at all.
- total 60 s or more -> the same conclusion, more strongly.
- **In neither branch do these tests enter the sub-floor tier.** This step exists to record the number in the chronicle and make the exclusion a decision with a measurement behind it rather than an omission.

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/the-repertory.md` and add it to `book/src/SUMMARY.md` alongside its neighbours. Cover: what the corpus family is and why its verdict comes from a run; the AUTHORED/REACHED gap and why only AUTHORED shipped; why the found-versus-authored fork was false; that `the-orange` stands declared and what its beats ask for next; the measured cost from Step 1. Book prose is technical and mathematical, comprehensible without reading the code it may show.

- [ ] **Step 4: Write the retrospective**

Create `docs/retrospectives/the-repertory.md` — one page, process lessons, not product (decision 0020). The followups to carry: `PARTIAL` needs code that can distinguish "never started" from "beat 4 failed"; the automated witness search (spec section 6) is unbuilt and witnesses are hand-found; beats 3-7 of `the-orange` have no snapshot surface to assert against; prose assertions are deliberately absent from v1; `participants` was cut from the record because nothing read it.

- [ ] **Step 5: Freshness sweep and Confidence Gradient**

Re-read `book/src/open-questions.md`. This campaign resolves no bet, but spec section 13 routes "is the world alive" there as a bet only a human playing can move. **Decision rule:**
- a bet on that question already exists -> leave it; note in the chronicle that the repertory deliberately does not measure it.
- no such bet exists -> add one, scored low, and say in the chronicle that the instrument built here cannot move it.

- [ ] **Step 6: Regenerate artifacts, then commit**

Run `make rebaseline`, then the drift check:
`git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`

**Decision rule:**
- empty diff -> nothing generated moved; commit the prose alone.
- `docs/digest/` moved -> expected, the idea registry gained two rows this campaign; commit it in the same commit.
- `book/src/domesday/` or a census path moved -> STOP. Neither should move for a prose-and-tests campaign; do not commit it, and report it.

Then: `cargo fmt`; `git add -A`; `git commit -m "docs(the-repertory): chronicle, retrospective, and the DoD sweep"`; `git push`

- [ ] **Step 7: Stage gate**

Run: `make sluice-stage BRANCH=campaign/the-repertory REF=$(git rev-parse HEAD)`

This never pushes. A conflict is refused at the mouth in milliseconds; that is the signal to absorb main locally and resubmit.

---

## Self-review

**Spec coverage.** Section 3 sibling relationship -> Task 1 module doc. Section 4 authorship scale -> `control` field, Task 1. Section 5 AUTHORED only, no reach field -> Task 1's schema carries none; the deferral is recorded in Task 6 Step 4. Section 6 found-not-staged -> `Witness`, Task 1; the automated search is explicitly unbuilt and recorded as a followup. Section 7 record, beats, structural assertions, negative space -> Task 1 (`waiting-does-not-move-the-body` is the negative-space item). Section 8 resolver -> Tasks 2-3. Section 9 five-valued plus ratchet -> Task 4, with `PARTIAL` explicitly not implemented and the reason given. Section 10 beta cut -> `beta` field, Task 1. Section 11 positive control -> Task 1's four scenes, all four verified by running them. Section 12 the-orange -> Task 5. Section 13 non-goals -> Task 6 Step 5 routes aliveness to the Gradient. Section 15.1 generated-paths -> Task 1 Step 5. Section 15.5 gate placement -> Task 6 Steps 1-2.

**Gap found and closed.** Spec section 7 lists a `participants` array and no task reads it. A field the resolver ignores is a declaration of exactly the kind section 3 forbids, so `participants` is absent from Task 1's JSON and from the record. It earns its place when the witness search consumes it, and not before. Task 6 Step 4 carries the note.

**Second gap found and closed.** Task 4's floor test iterates `the-founding.scene.json` only. Task 5 adds a second corpus file, so `the-orange` would have had no floor enforced despite appearing in `FLOORS`. Task 5 Step 4 extends the iteration to both corpora.

**Type consistency.** `Witness { seed: u64, target: String, day: Option<String> }` is constructed identically in Tasks 2, 3 and 5. `evaluate` returns `Result<(), String>` in Task 3 and is matched on that shape in Task 4. `Verdict::name()` returns `&'static str`, compared against `FLOORS`' `&str` in Task 4 and printed in Task 5. `load(&str) -> Vec<Scene>` is called with a file name in Tasks 1, 4 and 5.
