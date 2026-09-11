//! Build-time attestation and root-anchored runtime source inventory.
use crate::package::{PackageError, hash};
use serde::{Deserialize, Serialize};
use std::{path::Path, process::Command};
#[derive(Debug, Serialize, Deserialize)]
pub struct SourceFile {
    pub path: String,
    pub sha256: String,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct SourceState {
    pub head: String,
    pub status: String,
    pub patch: String,
    pub files: Vec<SourceFile>,
}
pub fn source_state(from: &Path) -> Result<SourceState, PackageError> {
    let git = |dir: &Path, args: &[&str]| -> Result<String, PackageError> {
        let output = Command::new("git").arg("-C").arg(dir).args(args).output()?;
        if !output.status.success() {
            return Err(PackageError::Manifest("git provenance failed".into()));
        }
        String::from_utf8(output.stdout).map_err(|e| PackageError::Manifest(e.to_string()))
    };
    let root = git(from, &["rev-parse", "--show-toplevel"])?;
    let root = Path::new(root.trim());
    let listed = git(
        root,
        &[
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "-z",
            "--",
            "clients/visual",
        ],
    )?;
    let files = listed
        .split('\0')
        .filter(|p| !p.is_empty())
        .map(|p| {
            Ok(SourceFile {
                path: p.into(),
                sha256: hash(&std::fs::read(root.join(p))?),
            })
        })
        .collect::<Result<Vec<_>, PackageError>>()?;
    if !files
        .iter()
        .any(|f| f.path == "clients/visual/planetarium/src/main.rs")
    {
        return Err(PackageError::Manifest(
            "empty or unexpected rendering source inventory".into(),
        ));
    }
    Ok(SourceState {
        head: git(root, &["rev-parse", "HEAD"])?.trim().into(),
        status: git(root, &["status", "--porcelain"])?,
        patch: git(root, &["diff", "HEAD", "--", "clients/visual"])?,
        files,
    })
}
pub const BUILD_REVISION: &str = env!("PLANETARIUM_BUILD_REVISION");
pub const BUILD_CLEAN: bool = matches!(env!("PLANETARIUM_BUILD_CLEAN").as_bytes(), b"true");
pub const RUSTC: &str = env!("PLANETARIUM_RUSTC");
