//! Walking the workspace's source tree and grouping audited items by crate.

use crate::extract::{AuditItem, positions_in_file};
use std::path::{Path, PathBuf};

/// The workspace roots scanned when `check` is given no explicit paths.
pub const WORKSPACE_ROOTS: &[&str] = &["kernel", "domains", "windows", "cli"];

/// Audited items grouped under the crate they came from.
#[derive(Debug)]
pub struct CrateItems {
    /// Crate label (`kernel`, `terrain`, `scene`, `cli`, …).
    pub crate_name: String,
    /// Every audited item found in the crate's non-test source.
    pub items: Vec<AuditItem>,
}

/// Derive a crate label from a source-file path.
pub fn crate_name_of(path: &Path) -> String {
    let comps: Vec<String> = path
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect();
    // domains/<name>/src/…  or  windows/<name>/src/…  → <name>
    for anchor in ["domains", "windows"] {
        if let Some(i) = comps.iter().position(|c| c == anchor)
            && let Some(name) = comps.get(i + 1)
        {
            return name.clone();
        }
    }
    if comps.iter().any(|c| c == "kernel") {
        return "kernel".to_string();
    }
    "cli".to_string()
}

/// Scan `roots` for audited items; empty `roots` means [`WORKSPACE_ROOTS`].
pub fn scan(roots: &[PathBuf]) -> Result<Vec<CrateItems>, String> {
    let default: Vec<PathBuf>;
    let roots = if roots.is_empty() {
        default = WORKSPACE_ROOTS.iter().map(PathBuf::from).collect();
        &default
    } else {
        roots
    };

    let mut files = Vec::new();
    for root in roots {
        collect_rs_files(root, &mut files)?;
    }
    files.sort();

    // Group by crate, preserving discovery order per crate.
    let mut out: Vec<CrateItems> = Vec::new();
    for path in files {
        let src = std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        let file = syn::parse_file(&src).map_err(|e| format!("{}: {e}", path.display()))?;
        let items = positions_in_file(&file);
        if items.is_empty() {
            continue;
        }
        let crate_name = crate_name_of(&path);
        match out.iter_mut().find(|c| c.crate_name == crate_name) {
            Some(c) => c.items.extend(items),
            None => out.push(CrateItems { crate_name, items }),
        }
    }
    out.sort_by(|a, b| a.crate_name.cmp(&b.crate_name));
    Ok(out)
}

fn collect_rs_files(root: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if root.is_file() {
        // An explicitly-named file root is trusted as-is: the tests/examples/
        // benches exclusion below only prunes directories discovered while
        // recursing a directory root (spec §2's intent is to keep crate
        // test/example/bench code out of a workspace *sweep*, not to reject
        // a file the caller pointed at directly — which matters for this
        // tool's own fixtures, which live under `tests/` by Cargo convention).
        if root.extension().is_some_and(|e| e == "rs") {
            out.push(root.to_path_buf());
        }
        return Ok(());
    }
    if !root.exists() {
        return Ok(());
    }
    let entries = std::fs::read_dir(root).map_err(|e| format!("{}: {e}", root.display()))?;
    let mut paths: Vec<PathBuf> = entries
        .map(|e| e.map(|e| e.path()).map_err(|e| e.to_string()))
        .collect::<Result<_, _>>()?;
    paths.sort();
    for path in paths {
        if is_excluded_dir(&path) {
            continue;
        }
        collect_rs_files(&path, out)?;
    }
    Ok(())
}

/// True for a directory named `tests`, `examples`, or `benches` (spec §2) —
/// pruned entirely so its contents are never walked.
fn is_excluded_dir(path: &Path) -> bool {
    path.is_dir()
        && matches!(
            path.file_name().and_then(|n| n.to_str()),
            Some("tests") | Some("examples") | Some("benches")
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crate_name_of_maps_domains_windows_kernel_and_cli() {
        assert_eq!(
            crate_name_of(Path::new("domains/terrain/src/lib.rs")),
            "terrain"
        );
        assert_eq!(
            crate_name_of(Path::new("windows/scene/src/lib.rs")),
            "scene"
        );
        assert_eq!(crate_name_of(Path::new("kernel/src/lib.rs")), "kernel");
        assert_eq!(crate_name_of(Path::new("cli/src/main.rs")), "cli");
    }

    /// Locks down the deviation from the brief's literal `is_excluded`: a
    /// directory sweep prunes `tests/` (and siblings) entirely, but an
    /// explicit file root under `tests/` is still trusted and scanned —
    /// the behavior this tool's own fixtures under `tests/fixtures/` rely on.
    #[test]
    fn scan_prunes_tests_dirs_on_sweep_but_trusts_an_explicit_file_root() {
        let dir = std::env::temp_dir().join("type_audit_walk_exclusion_test");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::create_dir_all(dir.join("tests")).unwrap();
        std::fs::write(
            dir.join("src/lib.rs"),
            "/// A fn.\npub fn real(x: f64) -> f64 { x }\n",
        )
        .unwrap();
        std::fs::write(
            dir.join("tests/helper.rs"),
            "/// A fn.\npub fn pruned(y: f64) -> f64 { y }\n",
        )
        .unwrap();

        let swept = scan(std::slice::from_ref(&dir)).unwrap();
        let swept_names: Vec<_> = swept
            .iter()
            .flat_map(|c| c.items.iter().map(|i| i.name.as_str()))
            .collect();
        assert_eq!(swept_names, vec!["real"]);

        let direct = scan(&[dir.join("tests/helper.rs")]).unwrap();
        let direct_names: Vec<_> = direct
            .iter()
            .flat_map(|c| c.items.iter().map(|i| i.name.as_str()))
            .collect();
        assert_eq!(direct_names, vec!["pruned"]);

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
