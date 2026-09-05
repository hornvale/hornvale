//! Real-Cargo fixtures guard runtime checkout selection, not simulation behavior.
#![cfg(unix)]
use serde_json::{Value, json};
use std::{
    fs,
    path::PathBuf,
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Repo(PathBuf);
fn checked(cmd: &mut Command) -> Output {
    let out = cmd.output().unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    out
}
impl Repo {
    fn new(marker: &str) -> Self {
        let root = std::env::temp_dir().join(format!(
            "digest cli {} {}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&root).unwrap();
        let repo = Self(root.canonicalize().unwrap());
        repo.write(".gitignore", "target/\n");
        // Fixtures leave Hornvale's directory override. Carry the prepared toolchain
        // so rustup never selects an older host default or fetches a new compiler.
        repo.write(
            "rust-toolchain.toml",
            include_str!("../../../rust-toolchain.toml"),
        );
        repo.write(
            "tools/digest/Cargo.toml",
            "[workspace]\nmembers = [\"packages/*\"]\nresolver = \"3\"\n",
        );
        repo.write("tools/digest/packages/mock/Cargo.toml", &manifest());
        repo.source(envelope(marker).to_string());
        repo.prepare();
        checked(repo.git().args(["init", "-q"]));
        checked(repo.git().args(["add", "."]));
        checked(repo.git().args([
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "-qm",
            "fixture",
        ]));
        repo
    }
    fn git(&self) -> Command {
        let mut command = Command::new("git");
        command.current_dir(&self.0);
        // Fixture setup and inspection own their repository before the host
        // is invoked. Inherited hook paths must never redirect these writes.
        for name in [
            "GIT_DIR",
            "GIT_WORK_TREE",
            "GIT_COMMON_DIR",
            "GIT_INDEX_FILE",
            "GIT_OBJECT_DIRECTORY",
            "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        ] {
            command.env_remove(name);
        }
        command
    }
    fn write(&self, path: &str, value: &str) {
        let path = self.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, value).unwrap();
    }
    fn prepare(&self) {
        checked(Command::new("cargo").current_dir(&self.0).args([
            "generate-lockfile",
            "--offline",
            "--manifest-path",
            "tools/digest/Cargo.toml",
        ]));
    }
    fn source(&self, json: String) {
        self.write("tools/digest/packages/mock/src/main.rs", &format!("fn main() {{ let a: Vec<_> = std::env::args().collect(); assert_eq!(&a[1..3], &[\"collect\", \"--repo-root\"]); assert_eq!(std::path::Path::new(&a[3]), std::env::current_dir().unwrap()); println!(\"{{}}\", {json:?}); }}"));
    }
    fn run(&self, scope: &str) -> Output {
        self.command(scope).output().unwrap()
    }
    fn command(&self, scope: &str) -> Command {
        let mut c = Command::new(env!("CARGO_BIN_EXE_digest"));
        c.current_dir(&self.0).args(["context", scope]);
        c
    }
    fn snapshot(&self) -> Vec<(PathBuf, Vec<u8>)> {
        let out = checked(self.git().args(["ls-files", "-z"]));
        out.stdout
            .split(|b| *b == 0)
            .filter(|p| !p.is_empty())
            .map(|p| {
                let p = PathBuf::from(std::str::from_utf8(p).unwrap());
                let bytes = fs::read(self.0.join(&p)).unwrap();
                (p, bytes)
            })
            .collect()
    }
}
impl Drop for Repo {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}
fn manifest() -> String {
    "[package]\nname = \"mock\"\nversion = \"0.1.0\"\nedition = \"2024\"\n[package.metadata.digest]\nrole = \"contributor\"\nnamespace = \"fixture.mock\"\nprotocol = 1\nbinary = \"mock\"\nscopes = [\"domains/thing\"]\n".into()
}
fn envelope(marker: &str) -> Value {
    json!({"protocol":1,"namespace":"fixture.mock","display_name":"Fixture","scopes":["domains/thing"],"requirements":[{"id":"fixture.mock:rule","statement":marker,"sources":["fixture policy"],"evidence":{"checked":{"required_observations":["fixture.mock:check"]}}}],"observations":[{"id":"fixture.mock:check","method":"finite fixture check","subject":"fixture input","outcome":"satisfied","details":marker,"requirements":["fixture.mock:rule"]}],"instructions":[{"id":"fixture.mock:guide","markdown":"Read fixture policy.","requirements":["fixture.mock:rule"],"observations":["fixture.mock:check"]}]})
}
fn fails(output: Output, needle: &str) {
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(needle), "wanted {needle}: {stderr}");
    assert!(output.stdout.is_empty(), "partial report leaked");
}
#[test]
fn reuses_host_across_current_roots_ignoring_git_and_target_overrides() {
    let a = Repo::new("fixture-a-marker");
    let b = Repo::new("fixture-b-marker");
    let before_a = a.snapshot();
    let before_b = b.snapshot();
    for (repo, marker, absent) in [
        (&a, "fixture-a-marker", "fixture-b-marker"),
        (&b, "fixture-b-marker", "fixture-a-marker"),
    ] {
        let output = repo
            .command("domains/thing")
            .env("GIT_DIR", a.0.join(".git"))
            .env("GIT_WORK_TREE", &a.0)
            .env("GIT_COMMON_DIR", a.0.join(".git"))
            .env("GIT_INDEX_FILE", a.0.join(".git/index"))
            .env("GIT_OBJECT_DIRECTORY", a.0.join(".git/objects"))
            .env("GIT_ALTERNATE_OBJECT_DIRECTORIES", a.0.join(".git/objects"))
            .env("CARGO_TARGET_DIR", a.0.join("shared-target"))
            .env("CARGO_BUILD_TARGET", "invalid-target")
            .env("CARGO_ENCODED_RUSTFLAGS", "--bad-fixture-flag")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let text = String::from_utf8_lossy(&output.stdout);
        assert!(text.contains(marker));
        assert!(!text.contains(absent));
        assert!(repo.0.join("tools/digest/target/debug/mock").exists());
    }
    assert!(!a.0.join("shared-target").exists());
    assert_eq!(before_a, a.snapshot());
    assert_eq!(before_b, b.snapshot());
}
#[test]
fn root_selects_all_and_fresh_build_observes_source_changes() {
    let repo = Repo::new("before-change");
    let old = repo.run(".");
    assert!(
        old.status.success(),
        "{}",
        String::from_utf8_lossy(&old.stderr)
    );
    repo.write("saved-report.md", &String::from_utf8_lossy(&old.stdout));
    repo.source(envelope("after-change").to_string());
    let output = repo.run("domains");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let text = String::from_utf8_lossy(&output.stdout);
    assert!(text.contains("after-change"));
    assert!(!text.contains("before-change"));
    assert!(text.contains("dirty"));
    fails(
        repo.command(".")
            .args(["--report", "saved-report.md"])
            .output()
            .unwrap(),
        "usage",
    );
}
#[test]
fn no_match_and_invalid_scope_fail_before_build() {
    let repo = Repo::new("fixture");
    fails(repo.run("domains/thing-other"), "no contributors");
    fails(repo.run("../thing"), "scope");
    fails(repo.run(""), "scope");
    assert!(!repo.0.join("tools/digest/target").exists());
}
#[test]
fn rejects_invalid_enrollment_before_build() {
    let repo = Repo::new("fixture");
    for (from, to, needle) in [
        ("protocol = 1", "protocol = 2", "protocol"),
        ("binary = \"mock\"", "binary = \"absent\"", "binary"),
        ("role = \"contributor\"", "role = \"other\"", "role"),
        (
            "namespace = \"fixture.mock\"",
            "namespace = \"INVALID\"",
            "namespace",
        ),
        (
            "scopes = [\"domains/thing\"]",
            "scopes = [\"../thing\"]",
            "scope",
        ),
        ("scopes = [\"domains/thing\"]", "scopes = []", "scope"),
        (
            "scopes = [\"domains/thing\"]",
            "scopes = [\"domains/thing/\"]",
            "normalized",
        ),
    ] {
        repo.write(
            "tools/digest/packages/mock/Cargo.toml",
            &manifest().replace(from, to),
        );
        fails(repo.run("."), needle);
    }
    assert!(!repo.0.join("tools/digest/target").exists());
}
#[test]
fn malformed_missing_and_disagreeing_envelopes_fail_without_partial_stdout() {
    let repo = Repo::new("fixture");
    for (json, needle) in [("{".into(), "JSON"), ("".into(), "JSON")] {
        repo.source(json);
        fails(repo.run("."), needle);
    }
    for (field, value, needle) in [
        ("protocol", json!(2), "protocol"),
        ("namespace", json!("another.owner"), "agreement"),
        ("scopes", json!(["other"]), "agreement"),
        ("observations", json!([]), "fixture.mock:check"),
    ] {
        let mut value_json = envelope("fixture");
        value_json[field] = value;
        repo.source(value_json.to_string());
        fails(repo.run("."), needle);
    }
}
#[test]
fn unknown_and_contradicted_required_results_render_honest_failed_reports() {
    let repo = Repo::new("fixture");
    for outcome in ["unknown", "contradicted"] {
        let mut json = envelope("fixture");
        json["observations"][0]["outcome"] = json!(outcome);
        repo.source(json.to_string());
        let output = repo.run(".");
        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stdout).contains(outcome),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}
#[test]
fn execution_and_build_failures_identify_phase() {
    let repo = Repo::new("fixture");
    repo.write(
        "tools/digest/packages/mock/src/main.rs",
        "fn main() {eprintln!(\"fixture diagnostic\"); std::process::exit(7);}",
    );
    fails(repo.run("."), "execution fixture.mock");
    repo.write("tools/digest/packages/mock/src/main.rs", "not Rust");
    fails(repo.run("."), "build fixture.mock");
}
#[test]
fn symlinked_member_escape_is_refused() {
    let repo = Repo::new("fixture");
    let outside = Repo::new("outside");
    fs::remove_dir_all(repo.0.join("tools/digest/packages/mock")).unwrap();
    std::os::unix::fs::symlink(
        outside.0.join("tools/digest/packages/mock"),
        repo.0.join("tools/digest/packages/mock"),
    )
    .unwrap();
    fails(repo.run("."), "escape");
}
#[test]
fn stale_lock_is_read_only_and_names_preparation() {
    let repo = Repo::new("fixture");
    repo.write(
        "tools/digest/packages/mock/Cargo.toml",
        &manifest().replace("0.1.0", "0.2.0"),
    );
    let before = fs::read(repo.0.join("tools/digest/Cargo.lock")).unwrap();
    fails(repo.run("."), "context-prepare");
    assert_eq!(
        before,
        fs::read(repo.0.join("tools/digest/Cargo.lock")).unwrap()
    );
}

#[test]
fn metadata_selects_before_build_and_root_collects_every_enrolled_member() {
    let repo = Repo::new("first-marker");
    repo.write(
        "tools/digest/packages/second/Cargo.toml",
        &manifest()
            .replace("name = \"mock\"", "name = \"second\"")
            .replace("fixture.mock", "fixture.second")
            .replace("binary = \"mock\"", "binary = \"second\"")
            .replace("domains/thing", "windows/lab"),
    );
    repo.write("tools/digest/packages/second/src/main.rs", "not Rust");
    repo.prepare();
    let scoped = repo.run("domains/thing");
    assert!(
        scoped.status.success(),
        "{}",
        String::from_utf8_lossy(&scoped.stderr)
    );
    fails(repo.run("."), "build fixture.second");
    let json = envelope("second-marker")
        .to_string()
        .replace("fixture.mock", "fixture.second")
        .replace("domains/thing", "windows/lab");
    repo.write(
        "tools/digest/packages/second/src/main.rs",
        &format!("fn main() {{ println!(\"{{}}\", {json:?}); }}"),
    );
    let root = repo.run(".");
    assert!(
        root.status.success(),
        "{}",
        String::from_utf8_lossy(&root.stderr)
    );
    let text = String::from_utf8_lossy(&root.stdout);
    assert!(text.contains("first-marker"));
    assert!(text.contains("second-marker"));
    // A valid first result cannot leak when the next selected contributor fails.
    repo.write(
        "tools/digest/packages/second/src/main.rs",
        "fn main() {println!(\"malformed\");}",
    );
    fails(repo.run("."), "execution fixture.second JSON");
}

#[test]
fn duplicate_namespace_and_unenrolled_members_do_not_become_success() {
    let repo = Repo::new("fixture");
    repo.write(
        "tools/digest/packages/second/Cargo.toml",
        &manifest()
            .replace("name = \"mock\"", "name = \"second\"")
            .replace("binary = \"mock\"", "binary = \"second\""),
    );
    repo.write("tools/digest/packages/second/src/main.rs", "fn main() {}");
    repo.prepare();
    fails(repo.run("."), "duplicate namespace");
    repo.write(
        "tools/digest/packages/second/Cargo.toml",
        "[package]\nname = \"second\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
    );
    let output = repo.run(".");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(!repo.0.join("tools/digest/target/debug/second").exists());
}

#[test]
fn target_directory_symlink_cannot_redirect_builds() {
    let repo = Repo::new("fixture");
    let outside = Repo::new("outside");
    std::os::unix::fs::symlink(&outside.0, repo.0.join("tools/digest/target")).unwrap();
    fails(repo.run("."), "target symlink escape");
    assert!(!outside.0.join("debug").exists());
}

#[test]
fn fixture_setup_and_snapshot_ignore_inherited_git_paths() {
    let outer = Repo::new("outer-fixture-marker");
    // A distinct staged path makes selecting the outer index observable even
    // when the fixture snapshots otherwise share the same source-file roster.
    outer.write(
        "outer-only-tracked.txt",
        "belongs only to the outer fixture",
    );
    checked(outer.git().args(["add", "outer-only-tracked.txt"]));
    let head_before = checked(outer.git().args(["rev-parse", "HEAD"])).stdout;
    let index_before = fs::read(outer.0.join(".git/index")).unwrap();
    // Run the real fixture setup and snapshots in another process. Every
    // inherited override belongs to this disposable outer repo; no global
    // environment is changed and no path points at the developer's checkout.
    let output = Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            "reuses_host_across_current_roots_ignoring_git_and_target_overrides",
            "--nocapture",
        ])
        .env("GIT_DIR", outer.0.join(".git"))
        .env("GIT_WORK_TREE", &outer.0)
        .env("GIT_COMMON_DIR", outer.0.join(".git"))
        .env("GIT_INDEX_FILE", outer.0.join(".git/index"))
        .env("GIT_OBJECT_DIRECTORY", outer.0.join(".git/objects"))
        .env(
            "GIT_ALTERNATE_OBJECT_DIRECTORIES",
            outer.0.join(".git/objects"),
        )
        .output()
        .unwrap();
    let head_after = checked(outer.git().args(["rev-parse", "HEAD"])).stdout;
    assert_eq!(head_before, head_after, "outer fixture HEAD changed");
    assert_eq!(
        index_before,
        fs::read(outer.0.join(".git/index")).unwrap(),
        "outer fixture index changed"
    );
    assert!(
        output.status.success(),
        "inherited-path fixture subprocess failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
