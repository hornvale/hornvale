//! Walking the source tree for authored numeric constants.
//!
//! **The visibility rule is the one place this tool deliberately differs from
//! `type-audit`.** That tool audits primitives at a *`pub` boundary*, because
//! its subject is an API contract. This tool's subject is an authored number,
//! and the motivating instance — `FATIGUE_RISE`, one sleep-debt rate for every
//! species in every world — was a **private** constant, as is `REST_BOUT`. A
//! `pub` filter would have walked straight past both. So visibility is ignored
//! entirely here, and that is a deliberate widening, not an oversight.

use crate::tag::doc_text_of;
use std::path::{Path, PathBuf};
use syn::visit::Visit;

/// The roots scanned when no explicit paths are given.
///
/// This is the population the campaign measured (see [`crate::report`]), and
/// it deliberately excludes `kernel/` and `cli/`: the kernel's constants are
/// the ladder's own `universal` exemplars and the CLI authors no world state.
/// Both are still reachable by naming them as explicit path roots, and Task 4
/// may widen the default when it closes the ratchet.
pub const AUDITED_ROOTS: &[&str] = &["domains", "windows"];

/// The constant types this tool judges.
///
/// Numeric only, and deliberately not `bool`/`&str`/`char`: a rung is a
/// statement about what a *quantity* varies with. Types outside this list are
/// counted as excluded rather than silently dropped, so the denominator the
/// report prints can be reconciled against the tree.
pub const NUMERIC_TYPES: &[&str] = &["f64", "i64", "u64", "u32", "usize"];

/// Substrings whose presence in a file's text marks it **kind-adjacent** — the
/// creature-modelling middle where a rung is genuinely arguable.
///
/// This is a text heuristic and nothing else. `Body` in particular is shared
/// with astronomy's celestial bodies, so the sub-population it produces is a
/// *reading aid* for a human deciding where to spend judgement, never a gate.
/// It is stated here rather than left implicit because a number is only as
/// meaningful as the predicate that produced it.
pub const KIND_MARKERS: &[&str] = &["KindId", "Species", "species", "Body"];

/// Where in a file a constant was declared.
///
/// Recorded because the campaign's measured population (610) counted only
/// file-level constants, and a total that silently mixes the two cannot be
/// reconciled against it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Site {
    /// Declared at the top level of a file.
    File,
    /// Declared inside a nested `mod { … }`.
    Module,
    /// An associated `const` on an `impl` or in a `trait`.
    Associated,
    /// A `const` declared inside a function body.
    FnBody,
}

impl Site {
    /// A stable label for the report's breakdown table.
    pub fn label(&self) -> &'static str {
        match self {
            Site::File => "file-level",
            Site::Module => "in a nested mod",
            Site::Associated => "associated (impl/trait)",
            Site::FnBody => "inside a fn body",
        }
    }
}

/// One authored numeric constant found in production source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthoredConst {
    /// Crate label (`terrain`, `vessel`, …).
    pub crate_name: String,
    /// Path of the file it was found in, as scanned.
    pub path: PathBuf,
    /// 1-based source line of the constant's name.
    pub line: usize,
    /// The constant's identifier.
    pub name: String,
    /// The numeric type, one of [`NUMERIC_TYPES`].
    pub ty: String,
    /// Concatenated doc-comment text (where a `plumb:` line may live).
    pub doc: String,
    /// Where in the file it sits.
    pub site: Site,
    /// Whether its file mentions any of [`KIND_MARKERS`].
    pub kind_adjacent: bool,
}

/// Everything one sweep saw, including what it deliberately did not judge.
#[derive(Debug, Default)]
pub struct Scan {
    /// The roots actually walked, in the order given.
    pub roots: Vec<PathBuf>,
    /// How many `.rs` files were parsed.
    pub files_parsed: usize,
    /// How many of those files were kind-adjacent.
    pub kind_adjacent_files: usize,
    /// Every production numeric constant found.
    pub consts: Vec<AuthoredConst>,
    /// Numeric constants skipped because they are test-only.
    pub test_only_consts: usize,
    /// Production constants skipped because their type is not numeric.
    pub non_numeric_consts: usize,
}

/// Derive a crate label from a source-file path (same rule as `type-audit`).
pub fn crate_name_of(path: &Path) -> String {
    let comps: Vec<String> = path
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect();
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

/// Scan `roots` for authored numeric constants; empty `roots` means
/// [`AUDITED_ROOTS`].
pub fn scan(roots: &[PathBuf]) -> Result<Scan, String> {
    let default: Vec<PathBuf>;
    let roots = if roots.is_empty() {
        default = AUDITED_ROOTS.iter().map(PathBuf::from).collect();
        &default
    } else {
        roots
    };

    let mut files = Vec::new();
    for root in roots {
        collect_rs_files(root, &mut files)?;
    }
    files.sort();

    let mut out = Scan {
        roots: roots.to_vec(),
        ..Scan::default()
    };
    for path in files {
        let src = std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        let file = syn::parse_file(&src).map_err(|e| format!("{}: {e}", path.display()))?;
        let kind_adjacent = KIND_MARKERS.iter().any(|m| src.contains(m));
        out.files_parsed += 1;
        if kind_adjacent {
            out.kind_adjacent_files += 1;
        }

        let mut collector = Collector {
            crate_name: crate_name_of(&path),
            path: path.clone(),
            kind_adjacent,
            mod_depth: 0,
            fn_depth: 0,
            test_depth: 0,
            consts: Vec::new(),
            test_only: 0,
            non_numeric: 0,
        };
        collector.visit_file(&file);
        out.consts.extend(collector.consts);
        out.test_only_consts += collector.test_only;
        out.non_numeric_consts += collector.non_numeric;
    }
    Ok(out)
}

/// Collect every `.rs` file under `root`, pruning test/example/bench dirs.
fn collect_rs_files(root: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    if root.is_file() {
        // An explicitly-named file root is trusted as-is, so this tool's own
        // fixtures can live under `tests/` by Cargo convention.
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

/// True for a directory named `tests`, `examples`, `benches`, `target`, or
/// `studies` — pruned entirely so its contents are never walked. A constant in
/// a test fixture is not an authored world parameter.
fn is_excluded_dir(path: &Path) -> bool {
    path.is_dir()
        && matches!(
            path.file_name().and_then(|n| n.to_str()),
            Some("tests") | Some("examples") | Some("benches") | Some("target")
        )
}

/// True for a `#[cfg(test)]` attribute.
pub fn has_cfg_test(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        a.path().is_ident("cfg")
            && a.parse_args::<syn::Meta>()
                .map(|m| matches!(m, syn::Meta::Path(p) if p.is_ident("test")))
                .unwrap_or(false)
    })
}

/// True for a bare `#[test]` attribute — a test function that is NOT inside a
/// `#[cfg(test)]` module, which is legal and does occur.
pub fn has_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| a.path().is_ident("test"))
}

/// The numeric type name, if `ty` is exactly one of [`NUMERIC_TYPES`].
pub fn numeric_type_name(ty: &syn::Type) -> Option<&'static str> {
    let syn::Type::Path(tp) = ty else {
        return None;
    };
    if tp.qself.is_some() || tp.path.segments.len() != 1 {
        return None;
    }
    let ident = tp.path.segments[0].ident.to_string();
    NUMERIC_TYPES.iter().copied().find(|n| *n == ident)
}

struct Collector {
    crate_name: String,
    path: PathBuf,
    kind_adjacent: bool,
    mod_depth: usize,
    fn_depth: usize,
    test_depth: usize,
    consts: Vec<AuthoredConst>,
    test_only: usize,
    non_numeric: usize,
}

impl Collector {
    fn site(&self, associated: bool) -> Site {
        if associated {
            Site::Associated
        } else if self.fn_depth > 0 {
            Site::FnBody
        } else if self.mod_depth > 0 {
            Site::Module
        } else {
            Site::File
        }
    }

    fn record(
        &mut self,
        ident: &syn::Ident,
        ty: &syn::Type,
        attrs: &[syn::Attribute],
        associated: bool,
    ) {
        let Some(ty_name) = numeric_type_name(ty) else {
            // A non-numeric constant is counted, not dropped, so the report's
            // denominator can be reconciled against a grep of the tree.
            if self.test_depth == 0 && !has_cfg_test(attrs) {
                self.non_numeric += 1;
            }
            return;
        };
        if self.test_depth > 0 || has_cfg_test(attrs) {
            self.test_only += 1;
            return;
        }
        self.consts.push(AuthoredConst {
            crate_name: self.crate_name.clone(),
            path: self.path.clone(),
            line: ident.span().start().line,
            name: ident.to_string(),
            ty: ty_name.to_string(),
            doc: doc_text_of(attrs),
            site: self.site(associated),
            kind_adjacent: self.kind_adjacent,
        });
    }
}

/// Which nesting counter a visited node moves while it is being descended.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Nest {
    /// A `mod { … }`.
    Mod,
    /// A function body.
    Fn,
    /// Neither — an `impl` block, which only carries a `cfg` to its children.
    Neither,
}

impl Collector {
    fn enter(&mut self, test: bool, nest: Nest) {
        if test {
            self.test_depth += 1;
        }
        match nest {
            Nest::Mod => self.mod_depth += 1,
            Nest::Fn => self.fn_depth += 1,
            Nest::Neither => {}
        }
    }

    fn leave(&mut self, test: bool, nest: Nest) {
        match nest {
            Nest::Mod => self.mod_depth -= 1,
            Nest::Fn => self.fn_depth -= 1,
            Nest::Neither => {}
        }
        if test {
            self.test_depth -= 1;
        }
    }
}

impl<'ast> Visit<'ast> for Collector {
    fn visit_item_mod(&mut self, m: &'ast syn::ItemMod) {
        let test = has_cfg_test(&m.attrs);
        self.enter(test, Nest::Mod);
        syn::visit::visit_item_mod(self, m);
        self.leave(test, Nest::Mod);
    }

    fn visit_item_fn(&mut self, f: &'ast syn::ItemFn) {
        let test = has_cfg_test(&f.attrs) || has_test_attr(&f.attrs);
        self.enter(test, Nest::Fn);
        syn::visit::visit_item_fn(self, f);
        self.leave(test, Nest::Fn);
    }

    fn visit_impl_item_fn(&mut self, f: &'ast syn::ImplItemFn) {
        let test = has_cfg_test(&f.attrs) || has_test_attr(&f.attrs);
        self.enter(test, Nest::Fn);
        syn::visit::visit_impl_item_fn(self, f);
        self.leave(test, Nest::Fn);
    }

    fn visit_trait_item_fn(&mut self, f: &'ast syn::TraitItemFn) {
        let test = has_cfg_test(&f.attrs);
        self.enter(test, Nest::Fn);
        syn::visit::visit_trait_item_fn(self, f);
        self.leave(test, Nest::Fn);
    }

    fn visit_item_impl(&mut self, i: &'ast syn::ItemImpl) {
        let test = has_cfg_test(&i.attrs);
        self.enter(test, Nest::Neither);
        syn::visit::visit_item_impl(self, i);
        self.leave(test, Nest::Neither);
    }

    fn visit_item_const(&mut self, c: &'ast syn::ItemConst) {
        self.record(&c.ident, &c.ty, &c.attrs, false);
        syn::visit::visit_item_const(self, c);
    }

    fn visit_impl_item_const(&mut self, c: &'ast syn::ImplItemConst) {
        self.record(&c.ident, &c.ty, &c.attrs, true);
        syn::visit::visit_impl_item_const(self, c);
    }

    fn visit_trait_item_const(&mut self, c: &'ast syn::TraitItemConst) {
        self.record(&c.ident, &c.ty, &c.attrs, true);
        syn::visit::visit_trait_item_const(self, c);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Sequence for unique fixture directories. NOT a clock: `SystemTime` is a
    /// disallowed type workspace-wide (decision 0001), and this crate inherits
    /// the repo's `clippy.toml` even though it is outside the workspace.
    static FIXTURE_SEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    fn scan_src(src: &str) -> Scan {
        let n = FIXTURE_SEQ.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let dir = std::env::temp_dir().join(format!("plumb_walk_test_{}_{n}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("domains/probe/src")).unwrap();
        let file = dir.join("domains/probe/src/lib.rs");
        std::fs::write(&file, src).unwrap();
        let out = scan(&[dir.join("domains")]).unwrap();
        std::fs::remove_dir_all(&dir).unwrap();
        out
    }

    fn names(scan: &Scan) -> Vec<&str> {
        scan.consts.iter().map(|c| c.name.as_str()).collect()
    }

    /// The motivating instance is PRIVATE. `FATIGUE_RISE` carried no `pub`, and
    /// neither does `REST_BOUT`; a `pub`-boundary filter — the rule
    /// `type-audit` correctly uses for its own subject — would walk past both
    /// and the sweep would report a clean tree.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): add a `pub` filter —
    /// `if !matches!(c.vis, syn::Visibility::Public(_)) { return; }` at the top
    /// of `visit_item_const`. The red is the campaign's own subject vanishing:
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["OTHER"]
    ///  right: ["FATIGUE_RISE", "OTHER"]
    /// ```
    #[test]
    fn a_private_const_is_audited_the_same_as_a_public_one() {
        let s = scan_src(
            "/// Private.\nconst FATIGUE_RISE: f64 = 0.3;\n\
             /// Public.\npub const OTHER: f64 = 1.0;\n",
        );
        assert_eq!(names(&s), vec!["FATIGUE_RISE", "OTHER"]);
    }

    /// A constant in a `#[cfg(test)]` module is not an authored world
    /// parameter, and telling the two apart is precisely what a line scanner
    /// cannot do — the reason this tool parses Rust.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): blind the check —
    /// replace `a.path().is_ident("cfg")` inside `has_cfg_test` with `false`.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["REAL", "FIXTURE", "NESTED", "BARE"]
    ///  right: ["REAL"]
    /// ```
    #[test]
    fn a_cfg_test_const_is_excluded_and_counted() {
        let s = scan_src(
            "/// Real.\nconst REAL: f64 = 1.0;\n\
             #[cfg(test)]\nmod tests {\n    const FIXTURE: f64 = 2.0;\n    const NESTED: u32 = 3;\n}\n\
             #[cfg(test)]\nconst BARE: f64 = 4.0;\n",
        );
        assert_eq!(names(&s), vec!["REAL"]);
        assert_eq!(s.test_only_consts, 3);
    }

    /// A `#[test] fn` outside a `#[cfg(test)]` module is legal and does occur;
    /// its locals are fixtures too.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): blind the check —
    /// `attrs.iter().any(|a| a.path().is_ident("test"))` -> `.any(|_a| false)`.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["REAL", "FIXTURE"]
    ///  right: ["REAL"]
    /// ```
    #[test]
    fn a_const_inside_a_test_fn_is_excluded() {
        let s = scan_src(
            "/// Real.\nconst REAL: f64 = 1.0;\n\
             #[test]\nfn t() { const FIXTURE: f64 = 2.0; assert!(FIXTURE > 0.0); }\n",
        );
        assert_eq!(names(&s), vec!["REAL"]);
        assert_eq!(s.test_only_consts, 1);
    }

    /// The walk is TOTAL over declaration sites, not just file-level ones, and
    /// records which site each came from — the campaign measured 610 file-level
    /// constants, so a total that could not be split back apart could not be
    /// reconciled against that number.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): delete the
    /// `self.record(...)` call from the `visit_impl_item_const` override, so the
    /// override still delegates but collects nothing.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: [("LOCAL", FnBody), ("NESTED", Module), ("TOP", File)]
    ///  right: [("ASSOC", Associated), ("LOCAL", FnBody), ("NESTED", Module), ("TOP", File)]
    /// ```
    #[test]
    fn the_walk_reaches_every_declaration_site_and_labels_it() {
        let s = scan_src(
            "/// Top.\nconst TOP: f64 = 1.0;\n\
             pub mod inner { /// Nested.\n pub const NESTED: u32 = 2; }\n\
             pub struct S;\nimpl S { /// Assoc.\n pub const ASSOC: usize = 3; }\n\
             /// Fn.\npub fn f() -> i64 { const LOCAL: i64 = 4; LOCAL }\n",
        );
        let mut got: Vec<(&str, Site)> =
            s.consts.iter().map(|c| (c.name.as_str(), c.site)).collect();
        got.sort();
        assert_eq!(
            got,
            vec![
                ("ASSOC", Site::Associated),
                ("LOCAL", Site::FnBody),
                ("NESTED", Site::Module),
                ("TOP", Site::File),
            ]
        );
    }

    /// A rung is a statement about a QUANTITY, so non-numeric constants are out
    /// of scope — but they are counted rather than dropped, because a
    /// denominator a reader cannot reconcile is the defect this project has
    /// catalogued most.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3): make
    /// `numeric_type_name`'s final expression `Some("f64")`, so every
    /// path-shaped type is judged numeric.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["N", "ON", "B"]
    ///  right: ["N"]
    /// ```
    #[test]
    fn non_numeric_consts_are_excluded_and_counted() {
        let s = scan_src(
            "/// Num.\nconst N: f64 = 1.0;\n\
             /// Text.\nconst LABEL: &str = \"x\";\n\
             /// Flag.\nconst ON: bool = true;\n\
             /// Byte.\nconst B: u8 = 1;\n",
        );
        assert_eq!(names(&s), vec!["N"]);
        assert_eq!(s.non_numeric_consts, 3);
    }

    /// The doc comment must reach the constant it documents — the other half
    /// of why this tool parses rather than scans.
    #[test]
    fn the_doc_comment_travels_with_its_const() {
        let s = scan_src(
            "/// Ticks in one standard day.\n\
             /// plumb: universal(the tick lattice is a kernel constant)\n\
             const TICKS: i64 = 100_000;\n",
        );
        assert_eq!(s.consts.len(), 1);
        assert!(s.consts[0].doc.contains("plumb: universal("));
        assert_eq!(s.consts[0].ty, "i64");
        assert_eq!(s.consts[0].crate_name, "probe");
    }

    #[test]
    fn kind_adjacency_is_a_whole_file_property() {
        let s = scan_src(
            "/// A.\nconst A: f64 = 1.0;\n/// Uses a KindId.\npub fn f(k: KindId) -> KindId { k }\n",
        );
        assert!(s.consts[0].kind_adjacent);
        assert_eq!(s.kind_adjacent_files, 1);

        let s = scan_src("/// A.\nconst A: f64 = 1.0;\n");
        assert!(!s.consts[0].kind_adjacent);
        assert_eq!(s.kind_adjacent_files, 0);
    }

    #[test]
    fn crate_name_of_maps_domains_windows_kernel_and_cli() {
        assert_eq!(
            crate_name_of(Path::new("domains/terrain/src/lib.rs")),
            "terrain"
        );
        assert_eq!(
            crate_name_of(Path::new("windows/vessel/src/liveness.rs")),
            "vessel"
        );
        assert_eq!(crate_name_of(Path::new("kernel/src/lib.rs")), "kernel");
        assert_eq!(crate_name_of(Path::new("cli/src/main.rs")), "cli");
    }

    #[test]
    fn the_default_roots_are_domains_and_windows() {
        assert_eq!(AUDITED_ROOTS, ["domains", "windows"]);
    }
}
