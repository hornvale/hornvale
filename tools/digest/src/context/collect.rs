//! Fresh collection from the caller's checkout, with exact Cargo artifacts.
use super::{
    CheckoutContext, ContextReport, compose,
    discovery::{self, Contributor},
    process::{self, Limits},
};
use digest_protocol::{Contribution, normalize_scope, validate};
use serde::Deserialize;
use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::{Arc, atomic::AtomicBool},
    time::Duration,
};

const GIT_PATH_VARS: [&str; 6] = [
    "GIT_DIR",
    "GIT_WORK_TREE",
    "GIT_COMMON_DIR",
    "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES",
];
fn controlled(command: &mut Command) -> &mut Command {
    for name in GIT_PATH_VARS {
        command.env_remove(name);
    }
    for name in [
        "CARGO_TARGET_DIR",
        "CARGO_BUILD_TARGET_DIR",
        "CARGO_BUILD_TARGET",
        "CARGO_ENCODED_RUSTFLAGS",
        "RUSTFLAGS",
        "RUSTC_WRAPPER",
        "RUSTC_WORKSPACE_WRAPPER",
    ] {
        command.env_remove(name);
    }
    command.env("GIT_OPTIONAL_LOCKS", "0")
}
fn machine_limits() -> Limits {
    Limits {
        stdout: 16 * 1024 * 1024,
        stderr: 16 * 1024 * 1024,
        timeout: Duration::from_secs(600),
        grace: Duration::from_millis(250),
    }
}
fn contributor_limits() -> Limits {
    Limits {
        stdout: 1024 * 1024,
        stderr: 64 * 1024,
        timeout: Duration::from_secs(5),
        grace: Duration::from_millis(250),
    }
}
fn phase(
    command: &mut Command,
    label: &str,
    limits: Limits,
    interrupted: &Arc<AtomicBool>,
) -> Result<Vec<u8>, String> {
    let output = process::run(controlled(command), limits, interrupted.clone())
        .map_err(|e| format!("{label}: {e}"))?;
    eprintln!(
        "context timing: {label} {:.3}s (whole phase; lock-wait attribution unavailable)",
        output.elapsed.as_secs_f64()
    );
    if !output.status.success() {
        return Err(format!(
            "{label}: exit {}; stderr: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    if !output.stderr.is_empty() {
        eprintln!("{label}: {}", String::from_utf8_lossy(&output.stderr));
    }
    Ok(output.stdout)
}
fn git(root: &Path, args: &[&str], interrupted: &Arc<AtomicBool>) -> Result<String, String> {
    let output = phase(
        Command::new("git").current_dir(root).args(args),
        "git checkout",
        machine_limits(),
        interrupted,
    )?;
    String::from_utf8(output).map_err(|e| format!("git checkout UTF-8: {e}"))
}
fn cargo(root: &Path) -> Command {
    let mut command = Command::new("cargo");
    command.current_dir(root);
    command
}
fn preparation(error: String) -> String {
    format!(
        "{error}\nIf dependencies or Cargo.lock need preparation, run `make context-prepare` in this checkout, then retry."
    )
}

/// Build and collect all contributors selected by a repository-relative scope.
/// Checkout revision/dirty state are context, not a source snapshot or complete provenance.
#[cfg(unix)]
pub fn context_from_current_checkout(scope: &str) -> Result<ContextReport, String> {
    let scope = normalize_scope(scope).map_err(|e| format!("scope: {e}"))?;
    let interrupted = Arc::new(AtomicBool::new(false));
    let _signals = Signals::register(interrupted.clone())?;
    collect(&scope, &interrupted)
}
/// Report the unsupported context process boundary without affecting legacy renderers.
#[cfg(not(unix))]
pub fn context_from_current_checkout(_: &str) -> Result<ContextReport, String> {
    Err("context is supported only on Unix".into())
}

fn collect(scope: &str, interrupted: &Arc<AtomicBool>) -> Result<ContextReport, String> {
    let cwd = std::env::current_dir().map_err(|e| format!("caller cwd: {e}"))?;
    let root = discovery::canonical(Path::new(
        git(&cwd, &["rev-parse", "--show-toplevel"], interrupted)?.trim_end_matches('\n'),
    ))?;
    let checkout = CheckoutContext {
        revision: git(&root, &["rev-parse", "HEAD"], interrupted)?
            .trim()
            .into(),
        dirty: !git(
            &root,
            &["status", "--porcelain", "--untracked-files=normal"],
            interrupted,
        )?
        .is_empty(),
    };
    let manifest = root.join("tools/digest/Cargo.toml");
    let metadata = phase(
        cargo(&root)
            .args(["metadata", "--manifest-path"])
            .arg(&manifest)
            .args([
                "--format-version",
                "1",
                "--no-deps",
                "--locked",
                "--offline",
            ]),
        "metadata",
        machine_limits(),
        interrupted,
    )
    .map_err(preparation)?;
    let contributors =
        discovery::discover(&metadata, &root, scope).map_err(|e| format!("discovery: {e}"))?;
    let target = root.join("tools/digest/target");
    std::fs::create_dir_all(&target).map_err(|e| format!("build target: {e}"))?;
    if discovery::canonical(&target)? != target {
        return Err("build target symlink escape".into());
    }
    let mut contributions = Vec::new();
    for contributor in contributors {
        let declaration = &contributor.declaration;
        let build = phase(
            cargo(&root)
                .args(["build", "--manifest-path"])
                .arg(&manifest)
                .arg("--package")
                .arg(&contributor.package_id)
                .arg("--bin")
                .arg(&declaration.binary)
                .arg("--target-dir")
                .arg(&target)
                .args([
                    "--locked",
                    "--offline",
                    "--message-format=json-render-diagnostics",
                ]),
            &format!("build {}", declaration.namespace),
            machine_limits(),
            interrupted,
        )
        .map_err(preparation)?;
        let executable = artifact(&build, &contributor, &target)
            .map_err(|e| format!("build {}: {e}", declaration.namespace))?;
        let bytes = phase(
            Command::new(executable)
                .current_dir(&root)
                .args(["collect", "--repo-root"])
                .arg(&root),
            &format!("execution {}", declaration.namespace),
            contributor_limits(),
            interrupted,
        )?;
        let envelope: Contribution = serde_json::from_slice(&bytes)
            .map_err(|e| format!("execution {} JSON: {e}", declaration.namespace))?;
        if envelope.protocol != declaration.protocol
            || envelope.namespace != declaration.namespace
            || envelope.scopes != declaration.scopes
        {
            return Err(format!(
                "execution {}: metadata/envelope agreement failed (protocol, namespace or scopes)",
                declaration.namespace
            ));
        }
        validate(&envelope)
            .map_err(|e| format!("execution {} validation: {e}", declaration.namespace))?;
        contributions.push(envelope);
    }
    compose(&checkout, &contributions).map_err(|e| format!("composition: {e}"))
}

#[derive(Deserialize)]
struct CargoMessage {
    reason: String,
    package_id: Option<String>,
    target: Option<ArtifactTarget>,
    executable: Option<PathBuf>,
}
#[derive(Deserialize)]
struct ArtifactTarget {
    name: String,
    kind: Vec<String>,
}
fn artifact(bytes: &[u8], contributor: &Contributor, target: &Path) -> Result<PathBuf, String> {
    let mut matches = Vec::new();
    for line in bytes.split(|b| *b == b'\n').filter(|l| !l.is_empty()) {
        let message: CargoMessage =
            serde_json::from_slice(line).map_err(|e| format!("Cargo artifact JSON: {e}"))?;
        if message.reason == "compiler-artifact"
            && message.package_id.as_deref() == Some(&contributor.package_id)
            && message
                .target
                .as_ref()
                .is_some_and(|t| t.name == contributor.declaration.binary && t.kind == ["bin"])
            && let Some(path) = message.executable
        {
            matches.push(path);
        }
    }
    if matches.len() != 1 {
        return Err(format!(
            "expected exactly one matching executable artifact, found {}",
            matches.len()
        ));
    }
    let path = discovery::canonical(&matches[0])?;
    if !path.starts_with(target) || !path.is_file() {
        return Err(format!(
            "executable artifact outside selected target directory: {}",
            path.display()
        ));
    }
    Ok(path)
}

#[cfg(unix)]
struct Signals(Vec<signal_hook::SigId>);
#[cfg(unix)]
impl Signals {
    fn register(flag: Arc<AtomicBool>) -> Result<Self, String> {
        let mut signals = Self(Vec::new());
        for signal in [signal_hook::consts::SIGINT, signal_hook::consts::SIGTERM] {
            signals.0.push(
                signal_hook::flag::register(signal, flag.clone())
                    .map_err(|e| format!("context interruption registration: {e}"))?,
            );
        }
        Ok(signals)
    }
}
#[cfg(unix)]
impl Drop for Signals {
    fn drop(&mut self) {
        for id in self.0.drain(..) {
            signal_hook::low_level::unregister(id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    #[test]
    fn artifact_requires_exact_package_target_cardinality_and_containment() {
        let exe = std::env::current_exe().unwrap().canonicalize().unwrap();
        let target = exe.parent().unwrap();
        let contributor = Contributor {
            package_id: "opaque-selected-id".into(),
            declaration: super::super::discovery::Declaration {
                role: "contributor".into(),
                namespace: "fixture.owner".into(),
                protocol: 1,
                binary: "collector".into(),
                scopes: vec![".".into()],
            },
        };
        let message = json!({"reason":"compiler-artifact","package_id":"opaque-selected-id","target":{"name":"collector","kind":["bin"]},"executable":exe});
        let bytes = serde_json::to_vec(&message).unwrap();
        assert_eq!(artifact(&bytes, &contributor, target).unwrap(), exe);
        let mut duplicate = bytes.clone();
        duplicate.push(b'\n');
        duplicate.extend_from_slice(&bytes);
        assert!(
            artifact(&duplicate, &contributor, target)
                .unwrap_err()
                .contains("found 2")
        );
        for (field, value) in [
            ("package_id", json!("another-package")),
            ("target", json!({"name":"another-binary","kind":["bin"]})),
            ("executable", json!(null)),
        ] {
            let mut wrong = message.clone();
            wrong[field] = value;
            assert!(
                artifact(&serde_json::to_vec(&wrong).unwrap(), &contributor, target)
                    .unwrap_err()
                    .contains("found 0")
            );
        }
        assert!(
            artifact(&bytes, &contributor, &target.join("other-target"))
                .unwrap_err()
                .contains("outside selected target")
        );
    }
}
