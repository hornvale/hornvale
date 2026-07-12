//! The layering rules and the dependency allowlist (decisions 0002 and 0004)
//! as executable assertions over `cargo metadata`. CLAUDE.md's architecture
//! section describes these rules; this file enforces them.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use std::process::Command;

/// The only crates permitted from outside the workspace (decision 0004).
const ALLOWED_EXTERNAL: &[&str] = &["serde", "serde_json"];

/// One workspace crate: its name, its layer (the first path component under
/// the workspace root), and its dependency names by kind.
struct Package {
    name: String,
    layer: String,
    normal_deps: Vec<String>,
    all_deps: Vec<String>,
}

/// Read the workspace's crates and declared dependencies via `cargo metadata`.
fn workspace() -> Vec<Package> {
    let output = Command::new(env!("CARGO"))
        .args(["metadata", "--format-version", "1", "--no-deps"])
        .output()
        .expect("cargo metadata should run");
    assert!(
        output.status.success(),
        "cargo metadata failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let meta: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("cargo metadata should emit JSON");
    let root = meta["workspace_root"]
        .as_str()
        .expect("metadata should name the workspace root");
    meta["packages"]
        .as_array()
        .expect("metadata should list packages")
        .iter()
        .map(|pkg| {
            let manifest = pkg["manifest_path"]
                .as_str()
                .expect("package should have a manifest path");
            let layer = manifest
                .strip_prefix(root)
                .and_then(|rel| rel.strip_prefix('/'))
                .and_then(|rel| rel.split('/').next())
                .expect("manifest should sit under the workspace root")
                .to_string();
            let deps = pkg["dependencies"]
                .as_array()
                .expect("package should list dependencies");
            let dep_name = |d: &serde_json::Value| {
                d["name"]
                    .as_str()
                    .expect("dependency should have a name")
                    .to_string()
            };
            Package {
                name: pkg["name"]
                    .as_str()
                    .expect("package should have a name")
                    .to_string(),
                layer,
                normal_deps: deps
                    .iter()
                    .filter(|d| d["kind"].is_null())
                    .map(dep_name)
                    .collect(),
                all_deps: deps.iter().map(dep_name).collect(),
            }
        })
        .collect()
}

#[test]
fn external_dependencies_are_allowlisted() {
    let packages = workspace();
    let internal: BTreeSet<&str> = packages.iter().map(|p| p.name.as_str()).collect();
    for pkg in &packages {
        for dep in &pkg.all_deps {
            assert!(
                internal.contains(dep.as_str()) || ALLOWED_EXTERNAL.contains(&dep.as_str()),
                "{} depends on {dep}, which is neither a workspace crate nor \
                 an allowlisted external (decision 0004)",
                pkg.name
            );
        }
    }
}

#[test]
fn the_kernel_depends_on_no_workspace_crate() {
    let packages = workspace();
    let internal: BTreeSet<&str> = packages.iter().map(|p| p.name.as_str()).collect();
    let kernel = packages
        .iter()
        .find(|p| p.name == "hornvale-kernel")
        .expect("the workspace should contain the kernel");
    for dep in &kernel.all_deps {
        assert!(
            !internal.contains(dep.as_str()),
            "the kernel must sit below every other crate, but depends on {dep}"
        );
    }
}

#[test]
fn domains_depend_only_on_the_kernel() {
    let packages = workspace();
    let domains: Vec<&Package> = packages.iter().filter(|p| p.layer == "domains").collect();
    assert!(!domains.is_empty(), "the workspace should contain domains");
    for pkg in domains {
        assert_eq!(
            pkg.normal_deps,
            ["hornvale-kernel"],
            "{} must depend on hornvale-kernel and nothing else (decision 0002)",
            pkg.name
        );
        for dep in &pkg.all_deps {
            assert!(
                dep == "hornvale-kernel" || ALLOWED_EXTERNAL.contains(&dep.as_str()),
                "{} dev/build-depends on {dep}; a domain may reach only the \
                 kernel and allowlisted externals (decision 0002)",
                pkg.name
            );
        }
    }
}

#[test]
fn windows_depend_only_on_kernel_domains_and_windows() {
    let packages = workspace();
    let layer_of: BTreeMap<&str, &str> = packages
        .iter()
        .map(|p| (p.name.as_str(), p.layer.as_str()))
        .collect();
    for pkg in packages.iter().filter(|p| p.layer == "windows") {
        for dep in &pkg.all_deps {
            let in_lower_layer = matches!(
                layer_of.get(dep.as_str()),
                Some(&"kernel") | Some(&"domains") | Some(&"windows")
            );
            assert!(
                in_lower_layer || ALLOWED_EXTERNAL.contains(&dep.as_str()),
                "window {} depends on {dep}, which sits above the window layer",
                pkg.name
            );
        }
    }
}

/// Sort rank of a layer along the constitutional chain.
fn layer_rank(layer: &str) -> usize {
    match layer {
        "kernel" => 0,
        "domains" => 1,
        "windows" => 2,
        "cli" => 3,
        other => panic!("unknown layer '{other}' — extend the chain deliberately"),
    }
}

/// Render the enforced dependency graph as the book's generated layering
/// page: the layer chain plus one row per crate, from the same
/// `cargo metadata` truth the assertions above enforce. Deterministic:
/// crates ordered by (layer, name), dependency lists sorted.
fn render_layering(packages: &[Package]) -> String {
    let internal: BTreeSet<&str> = packages.iter().map(|p| p.name.as_str()).collect();
    let mut sorted: Vec<&Package> = packages.iter().collect();
    sorted.sort_by(|a, b| {
        layer_rank(&a.layer)
            .cmp(&layer_rank(&b.layer))
            .then_with(|| a.name.cmp(&b.name))
    });
    let fmt_deps = |deps: &[&str]| {
        if deps.is_empty() {
            "—".to_string()
        } else {
            deps.join(", ")
        }
    };
    let mut out = String::from(
        "<!-- GENERATED FILE — do not edit. Emitted and drift-checked by \
         cli/tests/architecture.rs (the layering enforcer); accept a deliberate \
         graph change with REBASELINE=1 (make rebaseline-goldens). -->\n\n",
    );
    out.push_str("```text\nkernel  →  domains/*  →  windows/*  →  cli\n```\n\n");
    out.push_str("| crate | layer | workspace dependencies | dev/build-only extras |\n");
    out.push_str("|---|---|---|---|\n");
    for pkg in sorted {
        let mut normal: Vec<&str> = pkg
            .normal_deps
            .iter()
            .map(String::as_str)
            .filter(|d| internal.contains(*d))
            .collect();
        normal.sort_unstable();
        normal.dedup();
        let mut extras: Vec<&str> = pkg
            .all_deps
            .iter()
            .map(String::as_str)
            .filter(|d| internal.contains(*d) && !normal.contains(d))
            .collect();
        extras.sort_unstable();
        extras.dedup();
        out.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            pkg.name,
            pkg.layer,
            fmt_deps(&normal),
            fmt_deps(&extras)
        ));
    }
    out
}

#[test]
fn the_layering_render_is_deterministic_and_grounded() {
    let packages = workspace();
    let a = render_layering(&packages);
    assert_eq!(a, render_layering(&packages));
    assert!(
        a.contains("| hornvale-kernel | kernel | — | — |"),
        "the kernel row must show no workspace dependencies:\n{a}"
    );
}

#[test]
fn the_layering_page_matches_the_enforced_graph() {
    hornvale_kernel::golden::assert_golden(
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../book/src/reference/layering-generated.md"
        )),
        &render_layering(&workspace()),
        "the book's layering page drifted from the enforced dependency graph — if the \
         workspace graph changed deliberately, accept with REBASELINE=1 (or `make \
         rebaseline-goldens`) and review the diff as an architecture change; the picture \
         is authored by the enforcer, never by hand",
    );
}
