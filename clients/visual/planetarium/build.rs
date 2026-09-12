use std::{path::Path, process::Command};
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
    println!(
        "cargo:rustc-env=PLANETARIUM_BUILD_REVISION={}",
        git(&["rev-parse", "HEAD"]).trim()
    );
    println!(
        "cargo:rustc-env=PLANETARIUM_BUILD_CLEAN={}",
        git(&["status", "--porcelain"]).is_empty()
    );
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
