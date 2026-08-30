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

/// What a beat asserts. Structural only -- a golden string would redden on
/// any prose change, so v1 carries no prose assertion at all.
#[derive(Debug, Clone)]
#[allow(dead_code)]
// Tasks 2-5 read these.
// The shared `Snapshot` prefix is deliberate and is kept against
// `clippy::enum_variant_names`: v1 asserts against the session snapshot
// ONLY, and prose assertions are a named followup. When a `Transcript*`
// variant lands the prefix becomes the thing that distinguishes them, so
// dropping it now would have to be undone then.
#[allow(clippy::enum_variant_names)]
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
#[allow(dead_code)] // Tasks 2-5 read these.
struct Beat {
    id: String,
    description: String,
    script: Vec<String>,
    assertion: Assertion,
}

/// A scene the world is supposed to be able to play.
#[derive(Debug, Clone)]
#[allow(dead_code)] // Tasks 2-5 read these.
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

/// Drive one possession at `witness`, feeding `script` to `--script`, and
/// return the `vessel/session/v2` snapshot it writes.
///
/// The scene's verdict comes from THIS — an actual run — rather than from any
/// declaration in the corpus file. Measured cost is ~5.4 s per call against a
/// warm binary.
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

/// claim: structural(corpus scenes) — false-positive seed-loop flag
/// (decision 0093): this iterates the committed corpus's scenes, not seeds.
/// The claim is that every scene is well-formed enough to produce a verdict
/// from a run at all.
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
