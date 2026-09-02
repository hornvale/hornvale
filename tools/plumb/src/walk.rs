//! Walking the source tree for authored constants.
//!
//! **Two filters `type-audit` applies are deliberately absent here, and they
//! are absent for two different reasons.** Conflating them is what an earlier
//! draft of this paragraph did — it claimed the visibility argument covered
//! both, which was true of visibility and false of type.
//!
//! **1. Visibility is ignored.** `type-audit` audits primitives at a *`pub`
//! boundary* because its subject is an API contract. This tool's subject is an
//! authored number wherever it sits. The campaign's motivating instance —
//! `FATIGUE_RISE`, one sleep-debt rate for every species in every world — was
//! a **private** constant; it was deleted in The Slumber's Task 9 and its live
//! successor, `windows/vessel/src/liveness.rs`'s `DEFAULT_FATIGUE_RISE`, is
//! likewise not `pub`. So is `REST_BOUT`. A `pub` filter would walk past all
//! three, which is what mutation M6 demonstrates.
//!
//! **2. The type filter is a DENYLIST, not an allowlist** (decision ledger
//! #19). The first draft admitted five numeric primitives, and that was wrong
//! in a way that would have got worse over time:
//!
//! - It could not see `REST_BOUT`, a `TickSpan` — the constant the campaign's
//!   own spec titles *"the worked example"*.
//! - **The blind zone would grow as the code improved.** This repo pushes bare
//!   numbers toward typed newtypes; `type-audit` exists to do exactly that. An
//!   allowlist of primitives therefore goes blind to every crate that gets
//!   better, and an audit whose silence tracks code quality reads as a clean
//!   bill when it is a missing measurement.
//!
//! So every `const` is judged except one whose type is *declared* a
//! non-quantity — text, truth values, containers, markers. A newtype over a
//! number is the most quantity-like thing in the tree: `Gyr` is a duration and
//! `TickSpan` is a span, and the rung question applies to both unchanged.

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

/// Named types that are **not** quantities, and so carry no rung.
///
/// This is a denylist and the direction is load-bearing (ledger #19): a rung is
/// a statement about what a *quantity* varies with, and everything that is not
/// on this list — `f64`, `i32`, `u8`, `u128`, and every newtype over a number —
/// is a quantity. Naming the exceptions rather than the admissions is what
/// keeps the tool from going blind as bare primitives become newtypes.
///
/// A path type is matched on its **last** segment, so a qualified
/// `kernel::TickSpan` is judged exactly as a bare `TickSpan` is.
pub const NON_QUANTITY_TYPES: &[&str] = &["str", "String", "bool", "char"];

/// The five primitive names the campaign spec's line grep could match.
///
/// **This is not a filter and nothing in the walk consults it.** It exists so
/// [`crate::report`] can reconstruct the one figure that is actually comparable
/// to the spec's 610 — the walk's population is now defined by
/// [`NON_QUANTITY_TYPES`], which is a different population, and comparing a
/// denylist total against an allowlist grep would be comparing two things that
/// merely share a unit.
pub const SPEC_GREP_TYPES: &[&str] = &["f64", "i64", "u64", "u32", "usize"];

/// A type's short label, used both to report a judged constant's type and to
/// tally an excluded one. Deterministic and shape-based; never the full source
/// text, which would make the report churn on whitespace.
pub fn type_label(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(tp) => match tp.path.segments.last() {
            Some(seg) => {
                let name = seg.ident.to_string();
                if matches!(seg.arguments, syn::PathArguments::None) {
                    name
                } else {
                    format!("{name}<…>")
                }
            }
            None => "path".to_string(),
        },
        syn::Type::Reference(r) => format!("&{}", type_label(&r.elem)),
        syn::Type::Array(a) => format!("[{}; …]", type_label(&a.elem)),
        syn::Type::Slice(s) => format!("[{}]", type_label(&s.elem)),
        syn::Type::Tuple(t) if t.elems.is_empty() => "()".to_string(),
        syn::Type::Tuple(_) => "(…)".to_string(),
        syn::Type::Paren(p) => type_label(&p.elem),
        syn::Type::Group(g) => type_label(&g.elem),
        syn::Type::Ptr(_) => "*…".to_string(),
        syn::Type::BareFn(_) => "fn(…)".to_string(),
        syn::Type::TraitObject(_) => "dyn …".to_string(),
        syn::Type::ImplTrait(_) => "impl …".to_string(),
        syn::Type::Never(_) => "!".to_string(),
        _ => "other".to_string(),
    }
}

/// True when `ty` is a quantity — everything except the declared non-quantities
/// and the container/marker shapes.
///
/// A **generic** path (`Option<f64>`, `BTreeMap<_, _>`, `[f64; 3]`, a tuple, a
/// reference) is a container: it may hold quantities but is not one, and the
/// rung question would have no single answer for it.
pub fn is_quantity(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => match tp.path.segments.last() {
            Some(seg) => {
                matches!(seg.arguments, syn::PathArguments::None)
                    && !NON_QUANTITY_TYPES.contains(&seg.ident.to_string().as_str())
            }
            None => false,
        },
        syn::Type::Paren(p) => is_quantity(&p.elem),
        syn::Type::Group(g) => is_quantity(&g.elem),
        _ => false,
    }
}

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

/// One authored quantity-typed constant found in production source.
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
    /// The type's short label (see [`type_label`]) — `f64`, `TickSpan`, `Gyr`.
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
    /// Every production quantity-typed constant found.
    pub consts: Vec<AuthoredConst>,
    /// Constants skipped because they are test-only.
    pub test_only_consts: usize,
    /// Production constants skipped because their type is a declared
    /// non-quantity, tallied **by type label** rather than as a bare number.
    ///
    /// A bare count would let the exclusion bucket hide a quantity someone had
    /// wrongly denied, which is the failure the denylist ruling (#19) exists to
    /// prevent; a per-type tally makes every exclusion legible in the report.
    pub excluded_types: std::collections::BTreeMap<String, usize>,
}

impl Scan {
    /// How many production constants were excluded as non-quantities.
    pub fn non_quantity_consts(&self) -> usize {
        self.excluded_types.values().sum()
    }
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
            excluded_types: std::collections::BTreeMap::new(),
        };
        collector.visit_file(&file);
        out.consts.extend(collector.consts);
        out.test_only_consts += collector.test_only;
        for (label, n) in collector.excluded_types {
            *out.excluded_types.entry(label).or_default() += n;
        }
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

/// True for a directory named `tests`, `examples`, `benches` or `target` —
/// pruned entirely so its contents are never walked. A constant in a test
/// fixture is not an authored world parameter.
///
/// The four names here and the four in this sentence are the whole list; an
/// earlier draft of this doc named a fifth (`studies`) that the `matches!` arm
/// below never had. Inert, since no such directory exists under a crate's
/// `src/` — but a doc asserting behaviour the code lacks is how a reader comes
/// to believe a sweep excluded something it walked.
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

struct Collector {
    crate_name: String,
    path: PathBuf,
    kind_adjacent: bool,
    mod_depth: usize,
    fn_depth: usize,
    test_depth: usize,
    consts: Vec<AuthoredConst>,
    test_only: usize,
    excluded_types: std::collections::BTreeMap<String, usize>,
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
        // Test-only is decided FIRST, so a fixture never lands in the
        // non-quantity tally the report publishes as a reviewable list.
        if self.test_depth > 0 || has_cfg_test(attrs) {
            self.test_only += 1;
            return;
        }
        if !is_quantity(ty) {
            // A non-quantity is counted under its own type label, not dropped,
            // so the report's denominator reconciles against the tree and the
            // denylist's own judgement stays reviewable.
            *self.excluded_types.entry(type_label(ty)).or_default() += 1;
            return;
        }
        self.consts.push(AuthoredConst {
            crate_name: self.crate_name.clone(),
            path: self.path.clone(),
            line: ident.span().start().line,
            name: ident.to_string(),
            ty: type_label(ty),
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

    /// The motivating instance was PRIVATE. `FATIGUE_RISE` carried no `pub`;
    /// it was deleted in The Slumber's Task 9 and its live successor,
    /// `windows/vessel/src/liveness.rs:3282`'s `DEFAULT_FATIGUE_RISE`, is
    /// likewise not `pub` — and IS swept, verified against the real tree. Nor
    /// is `REST_BOUT`. A `pub`-boundary filter — the rule `type-audit`
    /// correctly uses for its own subject — would walk past all three and the
    /// sweep would report a clean tree. The fixture below keeps the historical
    /// name because that is the constant the campaign exists because of.
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

    /// **The type filter is a denylist** (ledger #19): every `const` is judged
    /// unless its type is a declared non-quantity. `u8` and `i32` are
    /// quantities the first draft's five-name allowlist silently dropped.
    /// Exclusions are tallied BY TYPE rather than summed, because the denylist
    /// is a judgement and a bare count would hide a quantity someone had
    /// wrongly denied.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): invert the denylist — make `is_quantity`'s path arm
    /// `NON_QUANTITY_TYPES.contains(...)` instead of `!...contains(...)`.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["ON", "NAME", "C"]
    ///  right: ["N", "B", "SMALL"]
    /// ```
    #[test]
    fn the_type_filter_is_a_denylist_and_its_exclusions_are_tallied_by_type() {
        let s = scan_src(
            "/// Num.\nconst N: f64 = 1.0;\n\
             /// Byte.\nconst B: u8 = 1;\n\
             /// Small.\nconst SMALL: i32 = -1;\n\
             /// Text.\nconst LABEL: &str = \"x\";\n\
             /// Flag.\nconst ON: bool = true;\n\
             /// Owned.\nconst NAME: String = String::new();\n\
             /// Letter.\nconst C: char = 'x';\n",
        );
        assert_eq!(names(&s), vec!["N", "B", "SMALL"]);
        assert_eq!(
            s.excluded_types
                .iter()
                .map(|(k, v)| (k.as_str(), *v))
                .collect::<Vec<_>>(),
            vec![("&str", 1), ("String", 1), ("bool", 1), ("char", 1)]
        );
        assert_eq!(s.non_quantity_consts(), 4);
    }

    /// **A newtype over a number is a quantity**, and this is the half the
    /// first draft got wrong: `REST_BOUT: TickSpan` is the constant the
    /// campaign's own spec titles "the worked example", and an allowlist of
    /// primitives could not see it. The blind zone would have grown as bare
    /// numbers became newtypes — which is what this repo's `type-audit`
    /// exists to make happen.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): restore the allowlist — make `is_quantity`'s path arm
    /// `["f64", "i64", "u64", "u32", "usize"].contains(&seg.ident.to_string().as_str())`.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: []
    ///  right: ["REST_BOUT", "AGE", "WETNESS", "SPAN"]
    /// ```
    #[test]
    fn a_newtype_over_a_number_is_a_quantity() {
        let s = scan_src(
            "/// A rest bout.\nconst REST_BOUT: TickSpan = TickSpan::from_ticks(25_000);\n\
             /// An age.\nconst AGE: Gyr = Gyr(4.5);\n\
             /// Wetness.\nconst WETNESS: SurfaceWetness = SurfaceWetness(0.5);\n\
             /// Qualified.\nconst SPAN: kernel::TickSpan = kernel::TickSpan(1);\n",
        );
        assert_eq!(names(&s), vec!["REST_BOUT", "AGE", "WETNESS", "SPAN"]);
        // The qualified path is judged on its LAST segment, so it reports the
        // same type label a bare one would.
        assert_eq!(s.consts[3].ty, "TickSpan");
        assert!(s.excluded_types.is_empty());
    }

    /// A container may HOLD quantities but is not one, so the rung question has
    /// no single answer for it. Excluded — and, like every exclusion, listed
    /// under its own shape rather than summed away.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3,
    /// fix round 1): drop the `PathArguments::None` requirement from
    /// `is_quantity`, admitting every generic container.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: ["N", "MAYBE"]
    ///  right: ["N"]
    /// ```
    #[test]
    fn a_container_is_not_a_quantity() {
        let s = scan_src(
            "/// Num.\nconst N: f64 = 1.0;\n\
             /// Maybe.\nconst MAYBE: Option<f64> = None;\n\
             /// Table.\nconst TABLE: [f64; 3] = [0.0, 1.0, 2.0];\n\
             /// Pair.\nconst PAIR: (f64, f64) = (0.0, 1.0);\n\
             /// Unit.\nconst NOTHING: () = ();\n",
        );
        assert_eq!(names(&s), vec!["N"]);
        assert_eq!(
            s.excluded_types
                .keys()
                .map(|k| k.as_str())
                .collect::<Vec<_>>(),
            vec!["()", "(…)", "Option<…>", "[f64; …]"]
        );
    }

    /// A test-only constant is decided BEFORE the type filter, so a fixture
    /// never lands in the non-quantity list the report publishes for review.
    ///
    /// **The first draft had this ordering backwards, and it silently lost
    /// constants from the denominator entirely**: it tested the type first and
    /// returned without counting a test-only non-quantity in EITHER bucket, so
    /// the row headed "every `const` the walk touched" was 44 short of the
    /// truth on the real tree.
    ///
    /// MUTATION THIS MUST FAIL AGAINST (confirmed 2026-09-02, The Plumb Task 3, fix round 1): restore that
    /// ordering — delete the `test_depth`/`has_cfg_test` early return that
    /// precedes the `is_quantity` check in `Collector::record`.
    ///
    /// ```text
    /// assertion failed: s.consts.is_empty()
    /// ```
    #[test]
    fn a_test_only_const_never_enters_the_non_quantity_tally() {
        let s = scan_src(
            "#[cfg(test)]\nmod tests {\n    const LABEL: &str = \"x\";\n    const N: f64 = 1.0;\n}\n",
        );
        assert!(s.consts.is_empty());
        assert_eq!(s.test_only_consts, 2);
        assert!(s.excluded_types.is_empty());
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
