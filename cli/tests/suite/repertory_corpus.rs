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

use hornvale_worldgen::seed_sweep;

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
    target: Target,
    day: Option<String>,
}

/// Whose body a witness drives. `possess` selects by NAME or by entity id and
/// the two are different flags, so one string cannot carry both.
#[derive(Debug, Clone)]
enum Target {
    /// A named selector `--target` understands (`flagship`, ...).
    Named(String),
    /// A specific derived roster member, by `--creature`.
    Creature(u64),
}

/// How a scene names the world it needs (spec section 4).
///
/// **A witness is a QUERY, not a PIN, by default.** A pinned entity id is
/// lineage-derived, so it moves the first time derivation changes — silently,
/// resolving to some other creature or to a refusal rather than to an error
/// that names the cause. The corpus already refuses golden strings in
/// ASSERTIONS on exactly those grounds; a pinned id is a golden string
/// wearing a witness's clothes.
///
/// [`Selector::Pin`] survives for the case it is right for: a scene whose
/// point IS a particular world, which is a regression check rather than a
/// capability one.
#[derive(Debug, Clone)]
enum Selector {
    /// This exact world. Grounded: "does this world still do X".
    Pin {
        seed: u64,
        target: Target,
        day: Option<String>,
    },
    /// Any world, from `from_seed`, within `scan` seeds, in which some
    /// derived body shares its room with another. Existential: "does SOME
    /// world do X".
    ///
    /// The species filters narrow WHO must share the room. Both `None` asks
    /// only for the mechanism; naming them asks for a particular pair, which
    /// is a far stronger request — see `SOC-one-creature-per-settlement`,
    /// under which a settlement holds exactly one derived creature, so the
    /// pairs that DO occur are wild concentrations rather than peoples.
    CoLocated {
        from_seed: u64,
        scan: u64,
        self_species: Option<String>,
        other_species: Option<String>,
    },
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
    selector: Selector,
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
        selector: parse_selector(w),
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

/// A scratch directory name unique to ONE [`run_at`] call.
///
/// **This was keyed `(pid, seed)`, and that was a race.** Two tests running
/// concurrently inside one process — `every_founding_scene_passes_every_beat`
/// and `no_scene_has_fallen_below_its_recorded_floor`, which both iterate
/// `the-founding.scene.json` — share a pid, so at a shared seed they wrote the
/// same `script.txt` and read back the same `snapshot.json`, each receiving the
/// other's run. The scene then failed a beat it passes in isolation and scored
/// [`Verdict::Absent`], which the floor test reports as a regression that never
/// happened.
///
/// It was invisible to every gate for a structural reason worth keeping: this
/// project gates with **nextest, which is process-per-test**, so every test
/// holds its own pid and these paths cannot collide there. Only libtest's
/// default — threads inside a single process — reproduces it. Measured at
/// commit `4400e3081`, one tree, the concurrency the only variable: serial
/// gives 11 passed in 138.3 s, parallel gives 9 passed and 2 failed in 12.9 s.
/// The failure is therefore not a world regression and never was, which is how
/// it survived: it looked exactly like one.
///
/// The counter makes the name unique per CALL rather than per test or per
/// scene, so no caller needs to know which other caller might share its seed.
fn scratch_name(seed: u64) -> String {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("hv-repertory-{}-{}-{}", std::process::id(), seed, n)
}

/// Drive one possession at `witness`, feeding `script` to `--script`, and
/// return the `vessel/session/v2` snapshot it writes.
///
/// The scene's verdict comes from THIS — an actual run — rather than from any
/// declaration in the corpus file. Measured cost is ~5.4 s per call against a
/// warm binary.
fn run_at(witness: &Witness, script: &[String]) -> Value {
    let dir = std::env::temp_dir().join(scratch_name(witness.seed));
    std::fs::create_dir_all(&dir).expect("scratch dir");
    let script_path = dir.join("script.txt");
    let snap_path = dir.join("snapshot.json");
    let mut body = script.join("\n");
    body.push('\n');
    std::fs::write(&script_path, body).expect("write script");

    let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"));
    cmd.arg("possess")
        .arg("--seed")
        .arg(witness.seed.to_string());
    match &witness.target {
        Target::Named(name) => cmd.arg("--target").arg(name),
        Target::Creature(id) => cmd.arg("--creature").arg(id.to_string()),
    };
    cmd.arg("--script")
        .arg(&script_path)
        .arg("--snapshot")
        .arg(&snap_path);
    if let Some(day) = &witness.day {
        cmd.arg("--day").arg(day);
    }
    let out = cmd.output().expect("the hornvale binary runs");
    assert!(
        out.status.success(),
        "possess failed at seed {} target {:?}: {}",
        witness.seed,
        witness.target,
        String::from_utf8_lossy(&out.stderr)
    );
    let text = std::fs::read_to_string(&snap_path).expect("--snapshot wrote a file");
    serde_json::from_str(&text).expect("the snapshot is valid JSON")
}

/// The opening state at a witness — the baseline `changes`/`unchanged`
/// compare against. `look` is the cheapest script that leaves the world where
/// it found it.
fn opening_at(witness: &Witness) -> Value {
    run_at(witness, &["look".to_string()])
}

/// Run every beat of `scene` in order and return the id of the FIRST that
/// fails, or `Ok(())` if all hold.
///
/// Both `SnapshotChanges` and `SnapshotUnchanged` require the pointer to
/// RESOLVE on both sides. A pointer that resolves nowhere would otherwise
/// satisfy `unchanged` vacuously (`None == None`), which is the shape of a
/// guard that can never fail.
fn evaluate(scene: &Scene, witness: &Witness) -> Result<(), String> {
    let opening = opening_at(witness);
    for beat in &scene.beats {
        let after = run_at(witness, &beat.script);
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

/// A scene's standing. Five-valued for the reason `tropes check`, seam-guard
/// and the type audit's `waiver(...)` are: a binary gate over a growing
/// roster goes red on day one and stays red, which trains everyone to ignore
/// it.
///
/// Spec section 9's PARTIAL is deliberately absent. [`evaluate`] stops at the
/// first failing beat, so nothing here can distinguish "the scene never
/// started" from "the scene ran and beat 4 failed" — and a variant no code
/// can produce is the same defect this whole family exists to avoid. Add it
/// with the code that can tell the two apart.
#[derive(Debug, PartialEq, Eq)]
enum Verdict {
    /// The scene does not play; the string names the first failing beat.
    Absent(String),
    /// Every beat holds against the scene's witness.
    Authored,
    /// A known-absent scene, declared WITH a reason. Green, printed loudly.
    Declared,
    /// No world in the selector's range assembles the scene's STAGE. Not the
    /// same finding as `Absent`: absent says a capability is missing, this
    /// says the world never puts the participants together, which points at
    /// different work entirely. The string records the bound searched, so the
    /// claim is "not within N seeds" rather than an unbounded negative.
    Unwitnessed(String),
    /// Declared absent, but it now passes — delete the declaration. This is
    /// what keeps a declaration honest: a one-directional acknowledgement can
    /// only ever be satisfied, so it rots; this one fails the moment the
    /// scene starts working.
    StaleDecl,
}

impl Verdict {
    /// The verdict's name, as the floor table spells it.
    fn name(&self) -> &'static str {
        match self {
            Verdict::Absent(_) => "ABSENT",
            Verdict::Unwitnessed(_) => "UNWITNESSED",
            Verdict::Authored => "AUTHORED",
            Verdict::Declared => "DECLARED",
            Verdict::StaleDecl => "STALE-DECL",
        }
    }
}

/// Resolve one scene to its verdict by running it.
fn verdict_of(scene: &Scene) -> Verdict {
    let Some(witness) = resolve(&scene.selector) else {
        return Verdict::Unwitnessed(match &scene.selector {
            Selector::CoLocated {
                from_seed, scan, ..
            } => {
                format!("no world in seeds {from_seed}..{}", from_seed + scan)
            }
            Selector::Pin { seed, .. } => format!("seed {seed} did not build"),
        });
    };
    match (evaluate(scene, &witness), scene.declared.is_some()) {
        (Ok(()), false) => Verdict::Authored,
        (Ok(()), true) => Verdict::StaleDecl,
        (Err(beat), false) => Verdict::Absent(beat),
        (Err(_), true) => Verdict::Declared,
    }
}

/// Two `run_at` calls at the SAME seed must not share a scratch directory.
///
/// This is the regression pin for the race [`scratch_name`] documents: the old
/// `(pid, seed)` name made this assertion false for any two concurrent callers
/// at one seed, and two committed tests really do iterate the same corpus at the
/// same seeds. Asserting on the NAME rather than on an observed interleaving is
/// deliberate — a test that races to prove a race is itself flaky, while this one
/// fails deterministically against the old scheme and cannot pass by luck.
#[test]
fn a_scratch_name_is_unique_per_call_even_at_one_seed() {
    let names: std::collections::BTreeSet<String> = (0..64).map(|_| scratch_name(42)).collect();
    assert_eq!(
        names.len(),
        64,
        "scratch names collided at one seed; run_at callers would clobber \
         each other's script.txt and snapshot.json, and a scene would score \
         Absent for a beat it actually passes"
    );
}

/// Every scene the repertory commits, across all corpora. The floor test
/// iterates THIS rather than one corpus: a new corpus file whose scenes the
/// ratchet never visits would leave a hole exactly the shape of the newest
/// work.
fn every_committed_scene() -> Vec<Scene> {
    let mut all = load("the-founding.scene.json");
    all.extend(load("two-in-a-room.scene.json"));
    all.extend(load("the-orange.scene.json"));
    all
}

/// Each scene's floor. A scene may not fall below its floor without a
/// deliberate edit here in the same commit.
const FLOORS: &[(&str, &str)] = &[
    ("walk-changes-the-room", "AUTHORED"),
    ("waiting-moves-the-day", "AUTHORED"),
    ("waiting-does-not-move-the-body", "AUTHORED"),
    ("co-location-is-observable", "AUTHORED"),
    ("two-in-a-room", "AUTHORED"),
    ("the-orange", "UNWITNESSED"),
];

fn parse_target(v: &Value) -> Target {
    match v.get("creature").and_then(Value::as_u64) {
        Some(id) => Target::Creature(id),
        None => Target::Named(as_str(v, "target")),
    }
}

fn parse_selector(w: &Value) -> Selector {
    match as_str(w, "kind").as_str() {
        "pin" => Selector::Pin {
            seed: w
                .get("seed")
                .and_then(Value::as_u64)
                .expect("a pin has a seed"),
            target: parse_target(w),
            day: w.get("day").and_then(Value::as_str).map(str::to_string),
        },
        "co-located" => Selector::CoLocated {
            from_seed: w.get("from_seed").and_then(Value::as_u64).unwrap_or(0),
            scan: w.get("scan").and_then(Value::as_u64).unwrap_or(1),
            self_species: w
                .get("self_species")
                .and_then(Value::as_str)
                .map(str::to_string),
            other_species: w
                .get("other_species")
                .and_then(Value::as_str)
                .map(str::to_string),
        },
        other => panic!(
            "unknown selector kind `{other}`. The vocabulary is closed on \
             purpose, exactly as the assertion vocabulary is."
        ),
    }
}

fn seed_world(seed: u64) -> Option<hornvale_kernel::World> {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .ok()
}

/// Resolve a selector to the world it names, or `None` if no world in range
/// satisfies it — which is a FINDING (spec section 6's `UNWITNESSED`), not an
/// error.
///
/// The search runs IN-PROCESS: a world build dominates the cost (~3.5 s) and
/// a roster is small, so each world is built ONCE and every roster member is
/// checked against it without paying a process apiece. The beats still run
/// out-of-process through [`run_at`], which is the real driving surface.
///
/// **A roster member `possess` refuses is not a witness.** It is a miss, and
/// the search continues past it; propagating it as an error would let one
/// unrelated refusal mask every world after it.
fn resolve(selector: &Selector) -> Option<Witness> {
    match selector {
        Selector::Pin { seed, target, day } => Some(Witness {
            seed: *seed,
            target: target.clone(),
            day: day.clone(),
        }),
        Selector::CoLocated {
            from_seed,
            scan,
            self_species,
            other_species,
        } => {
            for seed in *from_seed..from_seed.saturating_add(*scan) {
                let Some(world) = seed_world(seed) else {
                    continue;
                };
                let opts = hornvale_vessel::PossessOpts::default();
                let Ok((roster, _)) = hornvale_vessel::Session::start(&world, &opts) else {
                    continue;
                };
                let bodies: Vec<(hornvale_kernel::EntityId, String)> = roster
                    .bodies()
                    .iter()
                    .map(|b| (b.entity, b.species.clone()))
                    .collect();
                drop(roster);
                for (entity, species) in &bodies {
                    if self_species.as_ref().is_some_and(|want| want != species) {
                        continue;
                    }
                    let entity = *entity;
                    let opts = hornvale_vessel::PossessOpts {
                        target: hornvale_vessel::PossessTarget::Creature(entity),
                        ..hornvale_vessel::PossessOpts::default()
                    };
                    let Ok((session, _)) = hornvale_vessel::Session::start(&world, &opts) else {
                        continue;
                    };
                    let here = session.colocated_entities();
                    let matches = match other_species {
                        None => !here.is_empty(),
                        Some(want) => here
                            .iter()
                            .any(|e| bodies.iter().any(|(id, sp)| id == e && sp == want)),
                    };
                    if matches {
                        return Some(Witness {
                            seed,
                            target: Target::Creature(entity.0.get()),
                            day: None,
                        });
                    }
                }
            }
            None
        }
    }
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
    for s in every_committed_scene() {
        assert!(
            !s.beats.is_empty(),
            "scene `{}` has no beats; a scene with nothing to assert cannot \
             produce a verdict from a run",
            s.id
        );
        assert!(
            !s.id.is_empty(),
            "a scene needs an id: it is what the floor table keys on"
        );
        // NOT `resolve(...).is_some()`. That assertion lived here while
        // `UNWITNESSED` did not exist, and its own message said so. A scene
        // whose selector resolves to nothing is now a FINDING the verdict
        // reports, not a malformed corpus entry, and asserting otherwise
        // would make the corpus unable to hold the very state this campaign
        // added.
    }
}

#[test]
fn the_runner_returns_a_v2_snapshot_from_a_real_possession() {
    let w = Witness {
        seed: 42,
        target: Target::Named("most-populous-settlement".to_string()),
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

/// claim: structural(corpus scenes) — false-positive seed-loop flag
/// (decision 0093): this iterates the committed corpus's scenes, not seeds.
#[test]
fn every_founding_scene_passes_every_beat() {
    let scenes = load("the-founding.scene.json");
    let results = seed_sweep::map_seeds(0..scenes.len() as u64, |index| {
        let scene = &scenes[index as usize];
        let witness = resolve(&scene.selector);
        let evaluation = witness
            .as_ref()
            .map_or_else(|| Ok(()), |witness| evaluate(scene, witness));
        (witness, evaluation)
    });

    for (scene, (witness, evaluation)) in scenes.iter().zip(results) {
        let _witness = witness.expect("a founding scene pins its world");
        assert_eq!(
            evaluation,
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

/// A green from an evaluator that has never returned `Err` is a green nothing
/// earned. This pins the failing direction, using a scene built here rather
/// than one added to the corpus — the corpus states what the world should do,
/// not what the resolver should do.
#[test]
fn a_beat_whose_assertion_does_not_hold_names_that_beat() {
    let scene = Scene {
        id: "control-must-fail".to_string(),
        title: "A pointer that cannot resolve fails, and says which beat".to_string(),
        control: 100,
        beta: false,
        declared: None,
        selector: Selector::Pin {
            seed: 42,
            target: Target::Named("most-populous-settlement".to_string()),
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
    let witness = resolve(&scene.selector).expect("a pin always resolves");
    assert_eq!(evaluate(&scene, &witness), Err("b-impossible".to_string()));
}

/// claim: structural(corpus scenes) — false-positive seed-loop flag
/// (decision 0093): this iterates the committed corpus's scenes, not seeds.
#[test]
fn no_scene_has_fallen_below_its_recorded_floor() {
    let scenes = every_committed_scene();
    let verdicts = seed_sweep::map_seeds(0..scenes.len() as u64, |index| {
        verdict_of(&scenes[index as usize])
    });

    for (scene, got) in scenes.iter().zip(verdicts) {
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

/// The orange corpus's frozen scene count.
const ORANGE_SCENES: usize = 1;

#[test]
fn the_orange_is_frozen_at_its_authored_size() {
    assert_eq!(load("the-orange.scene.json").len(), ORANGE_SCENES);
}

/// The scene the project owner chose, and the reason this instrument exists.
/// It is expected to stand DECLARED for several campaigns; its beats are the
/// arc's task list (spec section 12).
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
    let got = verdict_of(&scene);
    assert_eq!(
        got.name(),
        "UNWITNESSED",
        "the-orange is {}. UNWITNESSED is the expected standing: the world \
         never assembles a drow-and-goblin room, which is a different finding \
         from a missing capability and points at different work. If it is now \
         AUTHORED, that is the campaign landing -- delete the `declared` \
         field, raise its floor, and lead the chronicle with it.",
        got.name()
    );
}

#[test]
fn a_co_located_selector_resolves_to_a_world_where_someone_is_present() {
    let sel = Selector::CoLocated {
        from_seed: 42,
        scan: 1,
        self_species: None,
        other_species: None,
    };
    let w = resolve(&sel).expect(
        "seed 42 carries a co-located pair (measured: the otyugh and the \
         carrion-crawler share room 633110509). If this is now None, the \
         world stopped assembling a shared room at seed 42 -- which is a \
         finding about the world, not a broken test.",
    );
    let snap = run_at(&w, &["look".to_string()]);
    let present = snap
        .pointer("/sensed/present")
        .and_then(Value::as_array)
        .expect("a resolved witness snapshots");
    assert!(
        !present.is_empty(),
        "a `co-located` selector must resolve to a world where someone is \
         actually present; it resolved to one where nobody is"
    );
}

/// The founding corpus's frozen scene count for the mechanism scene.
const TWO_IN_A_ROOM_SCENES: usize = 1;

#[test]
fn two_in_a_room_is_frozen_at_its_authored_size() {
    assert_eq!(load("two-in-a-room.scene.json").len(), TWO_IN_A_ROOM_SCENES);
}

/// A scene whose stage no world assembles resolves UNWITNESSED, not ABSENT.
///
/// Built here rather than committed to the corpus, and BOUNDED: proving a
/// negative over the whole seed space is not a test, it is a hang. The claim
/// is "not within this range", and the verdict carries the range it searched.
#[test]
fn a_scene_no_world_can_stage_is_unwitnessed_rather_than_absent() {
    let scene = Scene {
        id: "control-unstageable".to_string(),
        title: "A pair no world assembles".to_string(),
        control: 40,
        beta: false,
        declared: None,
        selector: Selector::CoLocated {
            from_seed: 0,
            scan: 2,
            self_species: Some("no-such-species".to_string()),
            other_species: Some("no-such-species".to_string()),
        },
        beats: vec![Beat {
            id: "b1".to_string(),
            description: "never reached — there is no stage to run it on".to_string(),
            script: vec!["look".to_string()],
            assertion: Assertion::SnapshotPresent {
                pointer: "/self/room".to_string(),
            },
        }],
    };
    let got = verdict_of(&scene);
    assert_eq!(
        got.name(),
        "UNWITNESSED",
        "a scene with no stage is {}, and the two verdicts mean different \
         things: ABSENT says a capability is missing, UNWITNESSED says the \
         world never puts the participants together",
        got.name()
    );
}
