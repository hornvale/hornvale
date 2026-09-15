use std::{path::Path, process::Command};

// These are inputs only for an explicitly packaged/read-only build. Ordinary
// builds do not set them and continue to attest against Git directly.
const PROVENANCE_REVISION_ENV: &str = "PLANETARIUM_BUILD_PROVENANCE_REVISION";
const PROVENANCE_CLEAN_ENV: &str = "PLANETARIUM_BUILD_PROVENANCE_CLEAN";

fn explicit_provenance() -> Option<(String, bool)> {
    let revision = std::env::var(PROVENANCE_REVISION_ENV).ok();
    let clean = std::env::var(PROVENANCE_CLEAN_ENV).ok();
    match (revision, clean) {
        (None, None) => None,
        (Some(_), None) | (None, Some(_)) => {
            panic!("{PROVENANCE_REVISION_ENV} and {PROVENANCE_CLEAN_ENV} must be set together")
        }
        (Some(revision), Some(clean)) => {
            assert!(
                revision.len() == 40 && revision.bytes().all(|byte| byte.is_ascii_hexdigit()),
                "{PROVENANCE_REVISION_ENV} must be a 40-character hexadecimal Git revision"
            );
            let clean = match clean.as_str() {
                "true" => true,
                "false" => false,
                _ => panic!("{PROVENANCE_CLEAN_ENV} must be exactly true or false"),
            };
            Some((revision, clean))
        }
    }
}

fn main() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..");
    // An intentionally absent input reruns attestation for EVERY build, including
    // the clean build after a commit with otherwise unchanged Rust sources.
    println!("cargo:rerun-if-changed=build-attestation-always-absent");
    let git = |args: &[&str]| {
        let out = Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(args)
            .output()
            .expect("git build provenance");
        assert!(out.status.success(), "git build provenance failed");
        String::from_utf8(out.stdout).expect("UTF-8 git output")
    };
    let (revision, clean) = explicit_provenance().unwrap_or_else(|| {
        (
            git(&["rev-parse", "HEAD"]).trim().to_owned(),
            git(&["status", "--porcelain"]).is_empty(),
        )
    });
    println!("cargo:rustc-env=PLANETARIUM_BUILD_REVISION={revision}");
    println!("cargo:rustc-env=PLANETARIUM_BUILD_CLEAN={clean}");
    let rustc = Command::new(std::env::var("RUSTC").unwrap())
        .arg("--version")
        .output()
        .unwrap();
    assert!(rustc.status.success());
    println!(
        "cargo:rustc-env=PLANETARIUM_RUSTC={}",
        String::from_utf8(rustc.stdout).unwrap().trim()
    );
}
