//! Campaign 1b exit criterion, verbatim from the spec: `hornvale new
//! --seed 42`, interrogate sky/village/belief in the REPL, and `hornvale
//! almanac` emits a one-page world document — deterministically.

use std::path::PathBuf;
use std::process::{Command, Stdio};

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("hornvale-1b-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn make_world(dir: &std::path::Path, seed: u64) -> PathBuf {
    make_world_with(dir, seed, &[])
}

fn make_world_with(dir: &std::path::Path, seed: u64, extra_args: &[&str]) -> PathBuf {
    let path = dir.join(format!("world-{seed}.json"));
    let out = bin()
        .args([
            "new",
            "--seed",
            &seed.to_string(),
            "--out",
            path.to_str().unwrap(),
        ])
        .args(extra_args)
        .output()
        .unwrap();
    assert!(out.status.success(), "new failed: {:?}", out);
    path
}

fn almanac_of(path: &std::path::Path) -> String {
    let out = bin()
        .args(["almanac", "--world", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success());
    String::from_utf8(out.stdout).unwrap()
}

#[test]
fn new_creates_a_world_and_reports_the_village() {
    let dir = temp_dir("new");
    let path = make_world(&dir, 42);
    assert!(path.exists());
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn almanac_is_byte_deterministic_and_seed_sensitive() {
    let dir = temp_dir("almanac");
    let w42 = make_world(&dir, 42);
    let w43 = make_world(&dir, 43);
    let a = almanac_of(&w42);
    let b = almanac_of(&w42);
    let c = almanac_of(&w43);
    assert_eq!(a, b, "same seed must yield byte-identical almanacs");
    assert_ne!(a, c, "different seeds must yield different almanacs");
    for section in [
        "# The Almanac of Seed 42",
        "## The Sky",
        "## The Land",
        "## The People",
        "## The Gods",
    ] {
        assert!(a.contains(section), "missing section: {section}");
    }
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn repl_answers_sky_village_and_belief() {
    // This is the campaign-1b (tier-0) exit criterion: it asserts the
    // constant sun's "zenith" wording specifically, so it pins --sky
    // constant explicitly now that `new`'s default has flipped to
    // generated (spec §8, Task 7).
    let dir = temp_dir("repl");
    let world = make_world_with(&dir, 42, &["--sky", "constant"]);
    let mut child = bin()
        .args(["repl", "--world", world.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write as _;
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"sky\nvillage\nbeliefs\nquit\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("zenith"), "sky answered");
    assert!(stdout.contains("population"), "village answered");
    assert!(stdout.contains("1."), "belief listed");
    // Extract the first belief's entity id from "1. [id]" format.
    let first_belief_id = stdout
        .lines()
        .find(|line| line.starts_with("1. ["))
        .and_then(|line| {
            line.split('[')
                .nth(1)
                .and_then(|s| s.split(']').next())
                .and_then(|s| s.parse::<u64>().ok())
        });
    assert!(
        first_belief_id.is_some(),
        "belief listing has [id] format: {}",
        stdout
    );
    // Query the why information for the first belief via a second REPL run.
    let mut child2 = bin()
        .args(["repl", "--world", world.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let id = first_belief_id.unwrap();
    child2
        .stdin
        .take()
        .unwrap()
        .write_all(format!("why {id}\nquit\n").as_bytes())
        .unwrap();
    let out2 = child2.wait_with_output().unwrap();
    assert!(out2.status.success());
    let stdout2 = String::from_utf8(out2.stdout).unwrap();
    assert!(stdout2.contains("celestial-body"), "why answered");
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn concepts_dump_contains_every_domain() {
    let out = bin().arg("concepts").output().unwrap();
    assert!(out.status.success());
    let doc = String::from_utf8(out.stdout).unwrap();
    for concept in [
        "is-place",
        "is-settlement",
        "has-caste",
        "tenet",
        "celestial-body",
        "ambient",
    ] {
        assert!(doc.contains(concept), "missing concept: {concept}");
    }
}
