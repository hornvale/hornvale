//! Walking `kernel/` and `domains/` for pub enum/struct shapes, grouped by
//! the crate they came from.

use crate::extract::{TypeShape, extract_types};
use std::path::{Path, PathBuf};

/// The roots scanned when `check` is given no explicit paths. Kernel and
/// domains only: a window may import a domain, so a window-side duplicate
/// is an ordinary refactor, never a forced one (spec §3, The Hallmark).
pub const SCAN_ROOTS: &[&str] = &["kernel", "domains"];

/// Every [`TypeShape`] found in one crate's non-test source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateTypes {
    /// Crate label (`kernel`, `<domain-name>`, `other`).
    pub crate_name: String,
    /// Every pub enum/named-field-struct shape found in the crate, sorted
    /// by `(name, line)` for byte-stable output.
    pub types: Vec<TypeShape>,
}

/// Derive a crate label from a source-file path: `domains/<name>/…` → name;
/// `kernel/…` → `"kernel"`; anything else → `"other"` (an explicitly-passed
/// fixture path, e.g. this tool's own tests, which is exactly what lets a
/// fixture under `tests/fixtures/twins/domains/<x>/…` derive crate `<x>`).
pub fn crate_name_of(path: &Path) -> String {
    let comps: Vec<String> = path
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect();
    if let Some(i) = comps.iter().position(|c| c == "domains")
        && let Some(name) = comps.get(i + 1)
    {
        return name.clone();
    }
    if comps.iter().any(|c| c == "kernel") {
        return "kernel".to_string();
    }
    "other".to_string()
}

/// Scan `roots` for pub enum/struct shapes; empty `roots` means [`SCAN_ROOTS`].
pub fn scan(roots: &[PathBuf]) -> Result<Vec<CrateTypes>, String> {
    let default: Vec<PathBuf>;
    let roots = if roots.is_empty() {
        default = SCAN_ROOTS.iter().map(PathBuf::from).collect();
        &default
    } else {
        roots
    };

    let mut files = Vec::new();
    for root in roots {
        collect_rs_files(root, &mut files)?;
    }
    files.sort();

    // Group by crate, preserving discovery order per crate (re-sorted below).
    let mut out: Vec<CrateTypes> = Vec::new();
    for path in files {
        let src = std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        let file = syn::parse_file(&src).map_err(|e| format!("{}: {e}", path.display()))?;
        let crate_name = crate_name_of(&path);
        let mut types = extract_types(&file, &crate_name, &path);
        if types.is_empty() {
            continue;
        }
        match out.iter_mut().find(|c| c.crate_name == crate_name) {
            Some(c) => c.types.append(&mut types),
            None => out.push(CrateTypes { crate_name, types }),
        }
    }
    for c in &mut out {
        c.types
            .sort_by(|a, b| (a.name.as_str(), a.line).cmp(&(b.name.as_str(), b.line)));
    }
    out.sort_by(|a, b| a.crate_name.cmp(&b.crate_name));
    Ok(out)
}

fn collect_rs_files(root: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if root.is_file() {
        // An explicitly-named file root is trusted as-is: the tests/examples/
        // benches exclusion below only prunes directories discovered while
        // recursing a directory root, which matters for this tool's own
        // fixtures, which live under `tests/` by Cargo convention.
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

/// True for a directory named `tests`, `examples`, or `benches` — pruned
/// entirely so its contents are never swept.
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
    fn crate_name_of_maps_domains_kernel_and_other() {
        assert_eq!(
            crate_name_of(Path::new("domains/terrain/src/lib.rs")),
            "terrain"
        );
        assert_eq!(crate_name_of(Path::new("kernel/src/lib.rs")), "kernel");
        assert_eq!(crate_name_of(Path::new("cli/src/main.rs")), "other");
    }

    /// A directory sweep prunes `tests/` (and siblings) entirely, but an
    /// explicit file root under `tests/` is still trusted and scanned — the
    /// behavior this tool's own fixtures under `tests/fixtures/` rely on.
    #[test]
    fn scan_prunes_tests_dirs_on_sweep_but_trusts_an_explicit_file_root() {
        let dir = std::env::temp_dir().join("placement_audit_walk_exclusion_test");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("domains/a/src")).unwrap();
        std::fs::create_dir_all(dir.join("domains/a/tests")).unwrap();
        std::fs::write(
            dir.join("domains/a/src/lib.rs"),
            "/// A shape.\npub enum Real { X, Y }\n",
        )
        .unwrap();
        std::fs::write(
            dir.join("domains/a/tests/helper.rs"),
            "/// A shape.\npub enum Pruned { X, Y }\n",
        )
        .unwrap();

        let swept = scan(std::slice::from_ref(&dir)).unwrap();
        let swept_names: Vec<_> = swept
            .iter()
            .flat_map(|c| c.types.iter().map(|t| t.name.as_str()))
            .collect();
        assert_eq!(swept_names, vec!["Real"]);

        let direct = scan(&[dir.join("domains/a/tests/helper.rs")]).unwrap();
        let direct_names: Vec<_> = direct
            .iter()
            .flat_map(|c| c.types.iter().map(|t| t.name.as_str()))
            .collect();
        assert_eq!(direct_names, vec!["Pruned"]);

        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn scan_returns_empty_for_a_missing_root() {
        let out = scan(&[PathBuf::from("/nonexistent/placement-audit-missing-root")]).unwrap();
        assert!(out.is_empty());
    }
}
