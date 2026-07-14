# The Type Audit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `tools/type-audit/` — a syn-based, non-workspace checker that makes every primitive crossing a public API boundary a judged, tagged, enforced decision — then tag every such crossing across the workspace, commit the drift-checked report, and ratify decisions 0027/0028. **No production code changes:** the only workspace-visible edits are doc-comment tag lines, CI wiring, decisions, and the book.

**Architecture:** A standalone Cargo project outside the workspace (excluded from the root manifest, own lockfile, `syn` dependency) parses each crate's `.rs` files, finds primitives at `pub` boundaries, and requires each to carry a `type-audit:` verdict in its doc comment. Two commands: `check` (default-deny — untagged, stale, or malformed tags are errors) and `report` (regenerates the committed `docs/audits/type-audit-report.md`). The sim's build graph never sees syn (decision 0027 extends 0019's boundary to offline dev tools, same posture as "models author, dice roll"). Tags are docs-only diffs; the workspace gate, world bytes, and every committed artifact are untouched.

**Tech Stack:** Rust (edition 2024) for the tool, with `syn` (full + visit/extra-traits) + `proc-macro2`; the workspace stays serde-only. mdBook for the book. The audit itself is a judgment pass driven by the tool's error list.

## Global Constraints

- **No production code changes.** The only new/changed files are: the tool (`tools/type-audit/`), doc-comment tag lines, the report (`docs/audits/`), decisions 0027/0028, CI wiring, CLAUDE.md, and the book. `cargo test --workspace` output, world bytes, and all committed sim artifacts are byte-identical before and after. (spec §1 Contract, §10)
- **The tool is not a workspace member.** Root `Cargo.toml` gains `exclude = ["tools/type-audit"]`; the tool has its own `Cargo.lock`. The workspace's serde-only allowlist (decision 0004/0019) is untouched — syn lives only in the excluded tool. (spec §2)
- **The tool is not in `cargo test --workspace`.** It is exercised by its own `cargo test` (run from `tools/type-audit/`) and by the CI drift step. The per-commit workspace gate stays fast. (spec §5)
- **Determinism of the report.** Same tree → byte-identical `docs/audits/type-audit-report.md`: sorted output, no timestamps, no wall-clock. CI drift-checks it. (spec §8)
- **Bare-ok classes (rubric, spec §4):** `ratio`, `count`, `index`, `constructor-edge`, `envelope`, `identifier-text`, `render-internal`, `flag`. Novel classes go to Nathan; the ratified set becomes decision 0028. Waiver reasons cite a source (`decision-0014`, `elevation-convention`).
- **No relitigation of ratified bare choices** — 0014 (`Fact.day`) and the elevation convention become `waiver` tags citing their sources; not reopened. (spec §10)
- **Rust hygiene in the tool:** the tool sets `#![warn(missing_docs)]` on its public items to match house style, runs `cargo fmt` as the final step before each commit, and is clippy-clean (`cargo clippy -- -D warnings` from `tools/type-audit/`). No `--no-verify`.
- **Tool run form (verbatim, used in CI and locally):**
  - `cargo run --manifest-path tools/type-audit/Cargo.toml -- check [paths…]`
  - `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

---

## File Structure

**The tool (`tools/type-audit/`, excluded from the workspace):**
- `Cargo.toml` — standalone package; `syn`, `proc-macro2` deps; lib + bin.
- `Cargo.lock` — committed (reproducible tool build).
- `src/lib.rs` — crate root; re-exports; `run(args) -> exit code`; module wiring.
- `src/args.rs` — `Command` enum + `parse_args`.
- `src/primitives.rs` — `contains_tracked_primitive(&syn::Type) -> bool`.
- `src/extract.rs` — `AuditItem`, `positions_in_file(&syn::File) -> Vec<AuditItem>` (pub-boundary walk, exclusions, doc capture, position names + line numbers).
- `src/tag.rs` — `Verdict`, `parse_tag(doc) -> Result<Vec<Verdict>, TagError>`.
- `src/audit.rs` — `Finding`, `audit_items(&[AuditItem]) -> Vec<Finding>` (bidirectional coverage).
- `src/walk.rs` — filesystem walk of workspace roots → `Vec<AuditItem>` tagged with crate name.
- `src/report.rs` — `render_report(&[AuditItem]) -> String` (deterministic tallies).
- `tests/fixtures/mini/` — a tiny fixture tree for the report-determinism + bidirectional integration tests.
- `tests/report_determinism.rs`, `tests/bidirectional.rs` — integration tests over the fixture tree.
- (Extraction, tag, audit, primitive unit tests live inline as `#[cfg(test)] mod tests` in their modules — matches repo style.)

**Workspace-visible (docs-only + wiring):**
- Modify: root `Cargo.toml` (`exclude`), `.github/workflows/ci.yml` (two lines + diff scope), `CLAUDE.md` (command block + typed-quantities pointer).
- Create: `docs/audits/type-audit-report.md`, `docs/decisions/0027-*.md`, `docs/decisions/0028-*.md`, `book/src/chronicle/NN-the-type-audit.md`, `docs/retrospectives/campaign-27.md`.
- Modify: doc comments across `kernel/`, `domains/*`, `windows/*`, `cli/` (tag lines only); `docs/decisions/README.md` index; `docs/vision/idea-registry.md` (REJ-1 cross-link to 0027); `book/src/SUMMARY.md` (chronicle entry) + any stale chapter the freshness sweep finds.

**Task ordering.** Tasks 1–8 build and self-test the tool (pure TDD, all inside `tools/type-audit/`, zero workspace impact). Tasks 9–13 are the audit: tag the workspace crate by crate, `check` green per crate. Task 14 wires CI and commits the report (lands only once the whole tree is green). Tasks 15–16 close: decisions, CLAUDE.md, registry, then the book DoD.

---

### Task 1: Scaffold the standalone tool + argument dispatch

**Files:**
- Create: `tools/type-audit/Cargo.toml`
- Create: `tools/type-audit/src/lib.rs`
- Create: `tools/type-audit/src/main.rs`
- Create: `tools/type-audit/src/args.rs`
- Modify: `Cargo.toml` (root — add `exclude`)

**Interfaces:**
- Produces: `type_audit::args::{Command, parse_args}` where `Command` is `enum Command { Check { paths: Vec<std::path::PathBuf> }, Report }` and `parse_args(&[String]) -> Result<Command, String>`; `type_audit::run(&[String]) -> i32`.

- [ ] **Step 1: Create the package manifest.** `tools/type-audit/Cargo.toml`:

```toml
[package]
name = "type-audit"
version = "0.1.0"
edition = "2024"
license = "MIT"
publish = false

[lib]
name = "type_audit"
path = "src/lib.rs"

[[bin]]
name = "type-audit"
path = "src/main.rs"

[dependencies]
syn = { version = "2", features = ["full", "visit", "extra-traits"] }
# `span-locations` makes `Span::start().line` return real line numbers when
# parsing files (outside a proc-macro); without it, diagnostics report `:0:`.
proc-macro2 = { version = "1", features = ["span-locations"] }
```

(`quote = "1"` is added in Task 4, where `ToTokens` is first needed.)

- [ ] **Step 2: Exclude the tool from the workspace.** Edit root `Cargo.toml`, adding an `exclude` key to the `[workspace]` table:

```toml
[workspace]
resolver = "3"
members = ["kernel", "domains/*", "windows/*", "cli"]
exclude = ["tools/type-audit"]
```

- [ ] **Step 3: Write the failing arg-parser test.** Create `tools/type-audit/src/args.rs`:

```rust
//! Command-line argument parsing for the type-audit tool.

use std::path::PathBuf;

/// The subcommand selected on the command line.
#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    /// Default-deny check; optional path roots scope the scan.
    Check {
        /// Directory roots to scan; empty means every workspace crate.
        paths: Vec<PathBuf>,
    },
    /// Regenerate the audit report on stdout.
    Report,
}

/// Parse `argv` (without the program name) into a [`Command`].
pub fn parse_args(args: &[String]) -> Result<Command, String> {
    match args.split_first() {
        Some((cmd, rest)) if cmd == "check" => Ok(Command::Check {
            paths: rest.iter().map(PathBuf::from).collect(),
        }),
        Some((cmd, rest)) if cmd == "report" => {
            if rest.is_empty() {
                Ok(Command::Report)
            } else {
                Err("report takes no arguments".to_string())
            }
        }
        Some((cmd, _)) => Err(format!("unknown command: {cmd}")),
        None => Err("usage: type-audit <check [paths…]|report>".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_check_report_and_rejects_unknown() {
        assert_eq!(
            parse_args(&["check".to_string()]),
            Ok(Command::Check { paths: vec![] })
        );
        assert_eq!(
            parse_args(&["check".to_string(), "kernel".to_string()]),
            Ok(Command::Check { paths: vec![PathBuf::from("kernel")] })
        );
        assert_eq!(parse_args(&["report".to_string()]), Ok(Command::Report));
        assert!(parse_args(&["frobnicate".to_string()]).is_err());
        assert!(parse_args(&[]).is_err());
    }
}
```

- [ ] **Step 4: Create the library root and binary.** `tools/type-audit/src/lib.rs`:

```rust
//! Type-audit: a syn-based checker that requires every primitive crossing a
//! public API boundary to carry a `type-audit:` verdict in its doc comment.
#![warn(missing_docs)]

pub mod args;

use args::{Command, parse_args};

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = success, non-zero = usage error or audit failure).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { .. }) => {
            // Wired to the real walk + audit in Task 7.
            0
        }
        Ok(Command::Report) => {
            // Wired to the report generator in Task 8.
            0
        }
        Err(msg) => {
            eprintln!("{msg}");
            2
        }
    }
}
```

`tools/type-audit/src/main.rs`:

```rust
//! Binary entry point; delegates to [`type_audit::run`].

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    std::process::exit(type_audit::run(&args));
}
```

- [ ] **Step 5: Run the test — expect PASS, and confirm the workspace still builds.**

Run: `cd tools/type-audit && cargo test`
Expected: PASS (1 test).

Run (from repo root): `cargo metadata --no-deps --format-version 1 | head -c 1`
Expected: prints `{` with no "current package believes it's in a workspace" error — the `exclude` is correct.

- [ ] **Step 6: Commit.**

```bash
git add tools/type-audit/Cargo.toml tools/type-audit/Cargo.lock tools/type-audit/src Cargo.toml
git commit -m "feat(type-audit): scaffold standalone tool + arg dispatch"
```

---

### Task 2: The tracked-primitive classifier

**Files:**
- Create: `tools/type-audit/src/primitives.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod primitives;`)

**Interfaces:**
- Consumes: `syn::Type`.
- Produces: `type_audit::primitives::contains_tracked_primitive(&syn::Type) -> bool` — true iff a tracked primitive appears anywhere in the type (peeling `Option`/`Vec`/slice/array/tuple/reference/paren/group). Tracked set: `f64 f32 u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 usize isize bool char String str` (`&str` is `Reference → Path "str"`).

- [ ] **Step 1: Write the failing test.** Create `tools/type-audit/src/primitives.rs`:

```rust
//! Recognising the primitives the audit tracks, wherever they nest.

/// The primitive type names the audit tracks. `&str` is caught as its inner
/// path segment `str`; `String` is a path segment.
const TRACKED: &[&str] = &[
    "f64", "f32", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64",
    "i128", "usize", "isize", "bool", "char", "String", "str",
];

/// True iff `ty` contains a tracked primitive at any nesting depth.
pub fn contains_tracked_primitive(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(p) => {
            // A tracked leaf, or a generic wrapper whose args contain one.
            if let Some(seg) = p.path.segments.last() {
                if TRACKED.contains(&seg.ident.to_string().as_str()) {
                    return true;
                }
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    return args.args.iter().any(|a| match a {
                        syn::GenericArgument::Type(t) => contains_tracked_primitive(t),
                        _ => false,
                    });
                }
            }
            false
        }
        syn::Type::Reference(r) => contains_tracked_primitive(&r.elem),
        syn::Type::Slice(s) => contains_tracked_primitive(&s.elem),
        syn::Type::Array(a) => contains_tracked_primitive(&a.elem),
        syn::Type::Tuple(t) => t.elems.iter().any(contains_tracked_primitive),
        syn::Type::Paren(p) => contains_tracked_primitive(&p.elem),
        syn::Type::Group(g) => contains_tracked_primitive(&g.elem),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ty(src: &str) -> syn::Type {
        syn::parse_str(src).unwrap()
    }

    #[test]
    fn tracks_primitives_wherever_they_nest() {
        assert!(contains_tracked_primitive(&ty("f64")));
        assert!(contains_tracked_primitive(&ty("Option<f64>")));
        assert!(contains_tracked_primitive(&ty("Vec<(u32, String)>")));
        assert!(contains_tracked_primitive(&ty("&str")));
        assert!(contains_tracked_primitive(&ty("&[f64]")));
        assert!(contains_tracked_primitive(&ty("[u8; 4]")));
        // Non-tracked: opaque domain types and bare generics.
        assert!(!contains_tracked_primitive(&ty("CellId")));
        assert!(!contains_tracked_primitive(&ty("Vec<Cell>")));
        assert!(!contains_tracked_primitive(&ty("T")));
        assert!(!contains_tracked_primitive(&ty("Au")));
    }
}
```

- [ ] **Step 2: Wire the module.** Add to `tools/type-audit/src/lib.rs` after `pub mod args;`:

```rust
pub mod primitives;
```

- [ ] **Step 3: Run the test.**

Run: `cd tools/type-audit && cargo test primitives`
Expected: PASS.

- [ ] **Step 4: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/primitives.rs tools/type-audit/src/lib.rs tools/type-audit/Cargo.lock
git commit -m "feat(type-audit): tracked-primitive classifier"
```

---

### Task 3: Extraction — functions and struct fields

**Files:**
- Create: `tools/type-audit/src/extract.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod extract;`)

**Interfaces:**
- Consumes: `syn::File` (from `syn::parse_file`), `primitives::contains_tracked_primitive`.
- Produces:
  - `type_audit::extract::Position { name: String, line: usize }` — one audited primitive-bearing position; `name` is the parameter/field name (`"return"` for return types).
  - `type_audit::extract::AuditItem { name: String, doc: String, line: usize, positions: Vec<Position> }` — one audited `pub` item and every primitive-bearing position on it; `doc` is the concatenated doc-comment text.
  - `type_audit::extract::positions_in_file(&syn::File) -> Vec<AuditItem>` — only items with ≥1 audited position are returned.
  - Helper `is_bare_pub(&syn::Visibility) -> bool` (true only for `Visibility::Public`).
  - Helper `doc_text(&[syn::Attribute]) -> String`.

- [ ] **Step 1: Write the failing test.** Create `tools/type-audit/src/extract.rs` with the module skeleton and this test (implementation follows in Step 2):

```rust
//! Walking a parsed file for primitives at `pub` boundaries.

use crate::primitives::contains_tracked_primitive;
use syn::spanned::Spanned;

/// One audited primitive-bearing position on an item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    /// Parameter or field name; `"return"` for a return type.
    pub name: String,
    /// 1-based source line of the position, for diagnostics.
    pub line: usize,
}

/// One audited `pub` item and all its primitive-bearing positions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuditItem {
    /// Item name (function/struct/enum/const/type identifier).
    pub name: String,
    /// Concatenated doc-comment text (where a `type-audit:` line may live).
    pub doc: String,
    /// 1-based source line of the item, for diagnostics.
    pub line: usize,
    /// Every primitive-bearing audited position on the item.
    pub positions: Vec<Position>,
}

/// True only for a bare `pub` (not `pub(crate)`/`pub(super)`/`pub(in …)`).
pub fn is_bare_pub(vis: &syn::Visibility) -> bool {
    matches!(vis, syn::Visibility::Public(_))
}

/// Concatenate the text of `#[doc = "…"]` attributes into one string.
pub fn doc_text(attrs: &[syn::Attribute]) -> String {
    let mut out = String::new();
    for attr in attrs {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(nv) = &attr.meta {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
                {
                    out.push_str(&s.value());
                    out.push('\n');
                }
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(src: &str) -> syn::File {
        syn::parse_file(src).unwrap()
    }

    #[test]
    fn extracts_pub_fn_params_and_returns() {
        let f = file(
            r#"
            /// A function.
            /// type-audit: bare-ok(count)
            pub fn seed_count(octaves: u32, label: &str) -> f64 { 0.0 }
            fn private(x: f64) {}
            pub fn opaque(cell: CellId) -> CellId { cell }
            "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1); // private + opaque excluded
        let item = &items[0];
        assert_eq!(item.name, "seed_count");
        assert!(item.doc.contains("type-audit: bare-ok(count)"));
        let names: Vec<_> = item.positions.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["octaves", "label", "return"]);
    }

    #[test]
    fn extracts_pub_fields_only_and_respects_restricted_vis() {
        let f = file(
            r#"
            /// A struct.
            pub struct Config {
                /// Ocean fraction.
                pub ocean_fraction: f64,
                pub(crate) hidden: f64,
                secret: u32,
            }
            "#,
        );
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1);
        let names: Vec<_> = items[0].positions.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["ocean_fraction"]);
    }

    #[test]
    fn extracts_tuple_struct_pub_fields_by_index() {
        let f = file(r#"
            /// A cell id.
            pub struct CellId(pub u32);
        "#);
        let items = positions_in_file(&f);
        assert_eq!(items[0].positions[0].name, "0");
    }
}
```

- [ ] **Step 2: Implement `positions_in_file` for functions and structs.** Append to `tools/type-audit/src/extract.rs` (above the `#[cfg(test)]` block):

```rust
/// Collect every audited `pub` item with ≥1 primitive-bearing position.
pub fn positions_in_file(file: &syn::File) -> Vec<AuditItem> {
    let mut items = Vec::new();
    collect_items(&file.items, &mut items);
    items
}

fn collect_items(syn_items: &[syn::Item], out: &mut Vec<AuditItem>) {
    for item in syn_items {
        match item {
            syn::Item::Fn(f) if is_bare_pub(&f.vis) => {
                push_fn(&f.sig, &f.attrs, out);
            }
            syn::Item::Struct(s) if is_bare_pub(&s.vis) => {
                push_struct(s, out);
            }
            // Enums, const/static, type aliases, traits, inherent impls, and
            // nested modules are handled in Task 4.
            _ => {}
        }
    }
}

fn push_fn(sig: &syn::Signature, attrs: &[syn::Attribute], out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for input in &sig.inputs {
        if let syn::FnArg::Typed(pt) = input {
            if contains_tracked_primitive(&pt.ty) {
                positions.push(Position {
                    name: pat_name(&pt.pat),
                    line: pt.span().start().line,
                });
            }
        }
    }
    if let syn::ReturnType::Type(_, ty) = &sig.output {
        if contains_tracked_primitive(ty) {
            positions.push(Position { name: "return".to_string(), line: ty.span().start().line });
        }
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: sig.ident.to_string(),
            doc: doc_text(attrs),
            line: sig.ident.span().start().line,
            positions,
        });
    }
}

fn push_struct(s: &syn::ItemStruct, out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for (idx, field) in s.fields.iter().enumerate() {
        if !is_bare_pub(&field.vis) {
            continue;
        }
        if contains_tracked_primitive(&field.ty) {
            let name = field
                .ident
                .as_ref()
                .map(|i| i.to_string())
                .unwrap_or_else(|| idx.to_string());
            positions.push(Position { name, line: field.ty.span().start().line });
        }
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: s.ident.to_string(),
            doc: doc_text(&s.attrs),
            line: s.ident.span().start().line,
            positions,
        });
    }
}

fn pat_name(pat: &syn::Pat) -> String {
    match pat {
        syn::Pat::Ident(pi) => pi.ident.to_string(),
        _ => "_".to_string(),
    }
}
```

- [ ] **Step 3: Wire the module.** Add to `tools/type-audit/src/lib.rs`:

```rust
pub mod extract;
```

- [ ] **Step 4: Run the tests.**

Run: `cd tools/type-audit && cargo test extract`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/extract.rs tools/type-audit/src/lib.rs
git commit -m "feat(type-audit): extract pub fn + struct positions"
```

---

### Task 4: Extraction — enums, const/static, type aliases, traits, impls, exclusions

**Files:**
- Modify: `tools/type-audit/src/extract.rs`

**Interfaces:**
- Extends `collect_items` to also visit: `pub enum` variant fields (all fields — variants inherit the enum's visibility); `pub const`/`pub static` types; `pub type` alias RHS; `pub trait` method signatures; inherent `impl` (`trait_ == None`) `pub fn`s. Recurses into non-`#[cfg(test)]` nested modules. `impl Trait for T` blocks and `#[cfg(test)]` items are skipped.

- [ ] **Step 1: Write the failing tests.** Add to the `tests` module in `extract.rs`:

```rust
    #[test]
    fn extracts_enum_variant_fields() {
        let f = file(r#"
            /// A rotation.
            pub enum Rotation {
                /// Spinning with a day length.
                Spinning { day: f64 },
                /// Tidally locked.
                Locked,
            }
        "#);
        let items = positions_in_file(&f);
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].positions[0].name, "day");
    }

    #[test]
    fn extracts_const_static_and_type_alias() {
        let f = file(r#"
            /// A label.
            pub const SEED_LABEL: &str = "x";
            /// An alias that launders a primitive.
            pub type Meters = f64;
        "#);
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"SEED_LABEL"));
        assert!(names.contains(&"Meters"));
    }

    #[test]
    fn audits_trait_defs_and_inherent_impls_but_not_trait_impls() {
        let f = file(r#"
            /// A provider.
            pub trait Provider {
                /// Sample at a coordinate.
                fn sample(&self, x: f64) -> f64;
            }
            pub struct P;
            impl P {
                /// Inherent, audited.
                pub fn scale(&self, k: f64) -> f64 { k }
            }
            impl Provider for P {
                fn sample(&self, x: f64) -> f64 { x } // trait impl — NOT audited
            }
        "#);
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert!(names.contains(&"sample")); // trait def
        assert!(names.contains(&"scale")); // inherent impl
        assert_eq!(names.iter().filter(|n| **n == "sample").count(), 1); // impl not double-counted
    }

    #[test]
    fn skips_cfg_test_modules() {
        let f = file(r#"
            /// Real.
            pub fn real(x: f64) -> f64 { x }
            #[cfg(test)]
            mod tests {
                pub fn helper(y: f64) -> f64 { y }
            }
        "#);
        let items = positions_in_file(&f);
        let names: Vec<_> = items.iter().map(|i| i.name.as_str()).collect();
        assert_eq!(names, vec!["real"]);
    }
```

- [ ] **Step 2: Extend `collect_items`.** Replace the `match item` arms in `collect_items` with:

```rust
        match item {
            syn::Item::Fn(f) if is_bare_pub(&f.vis) => push_fn(&f.sig, &f.attrs, out),
            syn::Item::Struct(s) if is_bare_pub(&s.vis) => push_struct(s, out),
            syn::Item::Enum(e) if is_bare_pub(&e.vis) => push_enum(e, out),
            syn::Item::Const(c) if is_bare_pub(&c.vis) => {
                push_type_item(&c.ident, &c.ty, &c.attrs, out)
            }
            syn::Item::Static(s) if is_bare_pub(&s.vis) => {
                push_type_item(&s.ident, &s.ty, &s.attrs, out)
            }
            syn::Item::Type(t) if is_bare_pub(&t.vis) => {
                push_type_item(&t.ident, &t.ty, &t.attrs, out)
            }
            syn::Item::Trait(t) if is_bare_pub(&t.vis) => {
                for ti in &t.items {
                    if let syn::TraitItem::Fn(m) = ti {
                        push_fn(&m.sig, &m.attrs, out);
                    }
                }
            }
            syn::Item::Impl(i) if i.trait_.is_none() => {
                for ii in &i.items {
                    if let syn::ImplItem::Fn(m) = ii {
                        if is_bare_pub(&m.vis) {
                            push_fn(&m.sig, &m.attrs, out);
                        }
                    }
                }
            }
            syn::Item::Mod(m) if !has_cfg_test(&m.attrs) => {
                if let Some((_, items)) = &m.content {
                    collect_items(items, out);
                }
            }
            _ => {}
        }
```

- [ ] **Step 3: Add the new helpers.** Append near the other `push_*` helpers:

```rust
fn push_enum(e: &syn::ItemEnum, out: &mut Vec<AuditItem>) {
    let mut positions = Vec::new();
    for variant in &e.variants {
        for (idx, field) in variant.fields.iter().enumerate() {
            if contains_tracked_primitive(&field.ty) {
                let base = field
                    .ident
                    .as_ref()
                    .map(|i| i.to_string())
                    .unwrap_or_else(|| idx.to_string());
                positions.push(Position {
                    name: format!("{}.{}", variant.ident, base),
                    line: field.ty.span().start().line,
                });
            }
        }
    }
    if !positions.is_empty() {
        out.push(AuditItem {
            name: e.ident.to_string(),
            doc: doc_text(&e.attrs),
            line: e.ident.span().start().line,
            positions,
        });
    }
}

fn push_type_item(
    ident: &syn::Ident,
    ty: &syn::Type,
    attrs: &[syn::Attribute],
    out: &mut Vec<AuditItem>,
) {
    if contains_tracked_primitive(ty) {
        out.push(AuditItem {
            name: ident.to_string(),
            doc: doc_text(attrs),
            line: ident.span().start().line,
            positions: vec![Position { name: ident.to_string(), line: ty.span().start().line }],
        });
    }
}

fn has_cfg_test(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        a.path().is_ident("cfg") && a.to_token_stream().to_string().contains("test")
    })
}
```

Add the import for `to_token_stream` at the top of the module, beside the existing `use syn::spanned::Spanned;`:

```rust
use quote::ToTokens;
```

and add `quote = "1"` to `[dependencies]` in `tools/type-audit/Cargo.toml`.

- [ ] **Step 4: Run the tests.**

Run: `cd tools/type-audit && cargo test extract`
Expected: PASS (7 tests total).

- [ ] **Step 5: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/extract.rs tools/type-audit/Cargo.toml tools/type-audit/Cargo.lock
git commit -m "feat(type-audit): extract enums, consts, aliases, traits, impls; skip cfg(test)"
```

---

### Task 5: The tag grammar parser

**Files:**
- Create: `tools/type-audit/src/tag.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod tag;`)

**Interfaces:**
- Consumes: a doc string (`AuditItem::doc`).
- Produces:
  - `type_audit::tag::Verdict` — `enum Verdict { BareOk { class: String, position: Option<String> }, Waiver { reason: String, position: Option<String> }, Pending { wave: u32, position: Option<String> } }`.
  - `type_audit::tag::BARE_OK_CLASSES: &[&str]` = `["ratio","count","index","constructor-edge","envelope","identifier-text","render-internal","flag"]`.
  - `type_audit::tag::find_tag_line(doc) -> Result<Option<&str>, TagError>` — the single `type-audit:` payload, or `None` if absent; error if >1 line.
  - `type_audit::tag::parse_tag(doc) -> Result<Vec<Verdict>, TagError>` — empty vec when no tag line.
  - `type_audit::tag::TagError { pub message: String }`.

- [ ] **Step 1: Write the failing test.** Create `tools/type-audit/src/tag.rs`:

```rust
//! Parsing the `type-audit:` verdict line inside an item's doc comment.

/// The ratified `bare-ok` classes (spec §4 / decision 0028).
pub const BARE_OK_CLASSES: &[&str] = &[
    "ratio", "count", "index", "constructor-edge", "envelope",
    "identifier-text", "render-internal", "flag",
];

/// One parsed verdict from a tag line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// Permanently fine as a bare primitive, in the named rubric class.
    BareOk {
        /// Rubric class from [`BARE_OK_CLASSES`].
        class: String,
        /// Optional position qualifier (parameter/field name).
        position: Option<String>,
    },
    /// Should be a newtype; deliberately deferred with a stated reason.
    Waiver {
        /// Traceable reason (e.g. `decision-0014`).
        reason: String,
        /// Optional position qualifier.
        position: Option<String>,
    },
    /// Will be converted in remediation wave `wave`.
    Pending {
        /// Remediation wave number.
        wave: u32,
        /// Optional position qualifier.
        position: Option<String>,
    },
}

/// A malformed or ambiguous tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagError {
    /// Human-readable reason.
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_every_verdict_form() {
        assert_eq!(
            parse_tag("type-audit: bare-ok(count)").unwrap(),
            vec![Verdict::BareOk { class: "count".into(), position: None }]
        );
        assert_eq!(
            parse_tag("prose\ntype-audit: bare-ok(ratio: ocean_fraction), pending(wave-2: seed)").unwrap(),
            vec![
                Verdict::BareOk { class: "ratio".into(), position: Some("ocean_fraction".into()) },
                Verdict::Pending { wave: 2, position: Some("seed".into()) },
            ]
        );
        assert_eq!(
            parse_tag("type-audit: waiver(decision-0014)").unwrap(),
            vec![Verdict::Waiver { reason: "decision-0014".into(), position: None }]
        );
        assert!(parse_tag("no tag here").unwrap().is_empty());
    }

    #[test]
    fn rejects_malformed_and_unknown() {
        assert!(parse_tag("type-audit: bare-ok(nonsense)").is_err()); // unknown class
        assert!(parse_tag("type-audit: frobnicate(x)").is_err()); // unknown verdict
        assert!(parse_tag("type-audit: pending(wave-x)").is_err()); // non-numeric wave
        assert!(parse_tag("type-audit: bare-ok(count)\ntype-audit: waiver(y)").is_err()); // two lines
    }
}
```

- [ ] **Step 2: Implement the parser.** Insert above the `#[cfg(test)]` block:

```rust
/// Return the single `type-audit:` payload (text after the colon), or `None`.
/// Errors if more than one tag line is present.
pub fn find_tag_line(doc: &str) -> Result<Option<&str>, TagError> {
    let mut found: Option<&str> = None;
    for line in doc.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("type-audit:") {
            if found.is_some() {
                return Err(TagError { message: "more than one type-audit line".into() });
            }
            found = Some(rest.trim());
        }
    }
    Ok(found)
}

/// Parse every verdict on the item's tag line (empty when none present).
pub fn parse_tag(doc: &str) -> Result<Vec<Verdict>, TagError> {
    let Some(payload) = find_tag_line(doc)? else {
        return Ok(Vec::new());
    };
    payload.split(',').map(|v| parse_verdict(v.trim())).collect()
}

fn parse_verdict(s: &str) -> Result<Verdict, TagError> {
    let err = || TagError { message: format!("malformed verdict: {s:?}") };
    let open = s.find('(').ok_or_else(err)?;
    if !s.ends_with(')') {
        return Err(err());
    }
    let head = &s[..open];
    let inner = &s[open + 1..s.len() - 1];
    let (arg, position) = match inner.split_once(':') {
        Some((a, p)) => (a.trim(), Some(p.trim().to_string())),
        None => (inner.trim(), None),
    };
    match head {
        "bare-ok" => {
            if !BARE_OK_CLASSES.contains(&arg) {
                return Err(TagError { message: format!("unknown bare-ok class: {arg:?}") });
            }
            Ok(Verdict::BareOk { class: arg.to_string(), position })
        }
        "waiver" => {
            if arg.is_empty() {
                return Err(TagError { message: "waiver needs a reason".into() });
            }
            Ok(Verdict::Waiver { reason: arg.to_string(), position })
        }
        "pending" => {
            let wave = arg
                .strip_prefix("wave-")
                .and_then(|n| n.parse::<u32>().ok())
                .ok_or_else(|| TagError { message: format!("bad wave: {arg:?}") })?;
            Ok(Verdict::Pending { wave, position })
        }
        other => Err(TagError { message: format!("unknown verdict: {other:?}") }),
    }
}
```

- [ ] **Step 3: Wire the module.** Add to `tools/type-audit/src/lib.rs`:

```rust
pub mod tag;
```

- [ ] **Step 4: Run the tests.**

Run: `cd tools/type-audit && cargo test tag`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/tag.rs tools/type-audit/src/lib.rs
git commit -m "feat(type-audit): tag grammar parser"
```

---

### Task 6: The audit engine — bidirectional coverage

**Files:**
- Create: `tools/type-audit/src/audit.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod audit;`)

**Interfaces:**
- Consumes: `extract::AuditItem`, `tag::{Verdict, parse_tag}`.
- Produces:
  - `type_audit::audit::Finding { pub item: String, pub line: usize, pub message: String }`.
  - `type_audit::audit::audit_item(&AuditItem) -> Vec<Finding>` — the coverage rules for one item.
  - `type_audit::audit::audit_items(&[AuditItem]) -> Vec<Finding>` — flattened over a slice.
- **Coverage rules (bidirectional, spec §3):**
  1. malformed/unknown tag → one finding (the `TagError` message);
  2. every audited position needs a covering verdict — a position-less verdict covers all positions; a position-qualified verdict covers only the position it names; an uncovered position → "untagged primitive at <name>";
  3. a position-qualified verdict naming no audited position → "stale tag position <name>";
  4. an item carrying a tag line but with **zero** audited positions is impossible here (`positions_in_file` only returns items with ≥1 position) — the stale-whole-item case is caught by rule 3 when a position qualifier misses, and by the walker never handing such items to the audit. (A tag on a genuinely primitive-free item is invisible to extraction and therefore harmless; documented as the syntactic-honesty boundary, spec §2.)

- [ ] **Step 1: Write the failing test.** Create `tools/type-audit/src/audit.rs`:

```rust
//! Bidirectional coverage: audited positions and tag verdicts must match
//! exactly — no untagged primitive, no stale tag.

use crate::extract::{AuditItem, Position};
use crate::tag::{Verdict, parse_tag};

/// One audit failure, anchored to an item and line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    /// Item the finding is about.
    pub item: String,
    /// 1-based source line.
    pub line: usize,
    /// Human-readable failure.
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn item(doc: &str, positions: &[&str]) -> AuditItem {
        AuditItem {
            name: "thing".into(),
            doc: doc.into(),
            line: 1,
            positions: positions
                .iter()
                .map(|n| Position { name: n.to_string(), line: 2 })
                .collect(),
        }
    }

    #[test]
    fn position_less_verdict_covers_all() {
        let i = item("type-audit: bare-ok(count)", &["a", "b"]);
        assert!(audit_item(&i).is_empty());
    }

    #[test]
    fn uncovered_position_is_a_finding() {
        let i = item("type-audit: bare-ok(count: a)", &["a", "b"]);
        let f = audit_item(&i);
        assert_eq!(f.len(), 1);
        assert!(f[0].message.contains("untagged primitive at b"));
    }

    #[test]
    fn stale_position_qualifier_is_a_finding() {
        let i = item("type-audit: bare-ok(count: ghost)", &["a"]);
        let f = audit_item(&i);
        // "a" uncovered AND "ghost" stale → two findings.
        assert!(f.iter().any(|x| x.message.contains("stale tag position ghost")));
        assert!(f.iter().any(|x| x.message.contains("untagged primitive at a")));
    }

    #[test]
    fn malformed_tag_is_a_finding() {
        let i = item("type-audit: bare-ok(nonsense)", &["a"]);
        assert_eq!(audit_item(&i).len(), 1);
    }

    #[test]
    fn missing_tag_is_a_finding() {
        let i = item("just prose", &["a"]);
        let f = audit_item(&i);
        assert_eq!(f.len(), 1);
        assert!(f[0].message.contains("untagged primitive at a"));
    }
}
```

- [ ] **Step 2: Implement the coverage rules.** Insert above the `#[cfg(test)]` block:

```rust
/// Audit one item against its own tag line.
pub fn audit_item(item: &AuditItem) -> Vec<Finding> {
    let verdicts = match parse_tag(&item.doc) {
        Ok(v) => v,
        Err(e) => {
            return vec![Finding { item: item.name.clone(), line: item.line, message: e.message }];
        }
    };

    let mut findings = Vec::new();
    let position_names: Vec<&str> = item.positions.iter().map(|p| p.name.as_str()).collect();

    // Rule 3: a position qualifier that names no audited position is stale.
    for v in &verdicts {
        if let Some(pos) = verdict_position(v) {
            if !position_names.contains(&pos) {
                findings.push(Finding {
                    item: item.name.clone(),
                    line: item.line,
                    message: format!("stale tag position {pos}"),
                });
            }
        }
    }

    // Rule 2: every audited position needs a covering verdict.
    let has_blanket = verdicts.iter().any(|v| verdict_position(v).is_none());
    for p in &item.positions {
        let covered = has_blanket
            || verdicts.iter().any(|v| verdict_position(v) == Some(p.name.as_str()));
        if !covered {
            findings.push(Finding {
                item: item.name.clone(),
                line: p.line,
                message: format!("untagged primitive at {}", p.name),
            });
        }
    }

    findings
}

/// Audit every item, flattening the findings.
pub fn audit_items(items: &[AuditItem]) -> Vec<Finding> {
    items.iter().flat_map(audit_item).collect()
}

fn verdict_position(v: &Verdict) -> Option<&str> {
    match v {
        Verdict::BareOk { position, .. }
        | Verdict::Waiver { position, .. }
        | Verdict::Pending { position, .. } => position.as_deref(),
    }
}
```

- [ ] **Step 3: Wire the module.** Add to `tools/type-audit/src/lib.rs`:

```rust
pub mod audit;
```

- [ ] **Step 4: Run the tests.**

Run: `cd tools/type-audit && cargo test audit`
Expected: PASS (5 tests).

- [ ] **Step 5: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/audit.rs tools/type-audit/src/lib.rs
git commit -m "feat(type-audit): bidirectional coverage audit engine"
```

---

### Task 7: Filesystem walk + wire the `check` command

**Files:**
- Create: `tools/type-audit/src/walk.rs`
- Create: `tools/type-audit/tests/fixtures/mini/good.rs`
- Create: `tools/type-audit/tests/fixtures/mini/bad.rs`
- Create: `tools/type-audit/tests/bidirectional.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod walk;`; wire `run`)

**Interfaces:**
- Consumes: `extract::positions_in_file`, `audit::{Finding, audit_items}`.
- Produces:
  - `type_audit::walk::CrateItems { pub crate_name: String, pub items: Vec<AuditItem> }`.
  - `type_audit::walk::WORKSPACE_ROOTS: &[&str]` = `["kernel", "domains", "windows", "cli"]` (scanned from CWD = repo root).
  - `type_audit::walk::scan(roots: &[std::path::PathBuf]) -> Result<Vec<CrateItems>, String>` — walk `.rs` files under each root, skipping any path containing a `tests`, `examples`, or `benches` directory component; parse and extract; group by crate. `roots` empty → `WORKSPACE_ROOTS`.
  - `type_audit::walk::crate_name_of(path) -> String` — the crate label from a file path (`kernel`, `domains/terrain` → `terrain`, `windows/scene` → `scene`, `cli`).
  - `check` in `run` prints findings as `path:line: message`, sorted; exit 1 if any, else 0.

- [ ] **Step 1: Write the failing integration test.** Create the fixture tree. `tools/type-audit/tests/fixtures/mini/good.rs`:

```rust
/// A fully-tagged function.
/// type-audit: bare-ok(count)
pub fn octaves(n: u32) -> u32 { n }
```

`tools/type-audit/tests/fixtures/mini/bad.rs`:

```rust
/// An untagged function.
pub fn gravity(mass: f64) -> f64 { mass }
```

Create `tools/type-audit/tests/bidirectional.rs`:

```rust
//! Integration: on a fixture tree, `check` passes iff tags exactly cover
//! audited positions.

use std::path::PathBuf;
use type_audit::audit::audit_items;
use type_audit::walk::scan;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/mini").join(name)
}

#[test]
fn good_file_has_no_findings_bad_file_does() {
    let good = scan(&[fixture("good.rs")]).unwrap();
    let good_findings: Vec<_> = good.iter().flat_map(|c| audit_items(&c.items)).collect();
    assert!(good_findings.is_empty(), "unexpected: {good_findings:?}");

    let bad = scan(&[fixture("bad.rs")]).unwrap();
    let bad_findings: Vec<_> = bad.iter().flat_map(|c| audit_items(&c.items)).collect();
    assert_eq!(bad_findings.len(), 2); // mass + return, both untagged
}
```

- [ ] **Step 2: Run the test to confirm it fails.**

Run: `cd tools/type-audit && cargo test --test bidirectional`
Expected: FAIL — `walk` module does not exist.

- [ ] **Step 3: Implement the walker.** Create `tools/type-audit/src/walk.rs`:

```rust
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
    let comps: Vec<String> =
        path.components().map(|c| c.as_os_str().to_string_lossy().to_string()).collect();
    // domains/<name>/src/…  or  windows/<name>/src/…  → <name>
    for anchor in ["domains", "windows"] {
        if let Some(i) = comps.iter().position(|c| c == anchor) {
            if let Some(name) = comps.get(i + 1) {
                return name.clone();
            }
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
        if root.extension().is_some_and(|e| e == "rs") && !is_excluded(root) {
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
        collect_rs_files(&path, out)?;
    }
    Ok(())
}

/// Skip `tests/`, `examples/`, `benches/` directory components (spec §2).
fn is_excluded(path: &Path) -> bool {
    path.components().any(|c| {
        matches!(c.as_os_str().to_string_lossy().as_ref(), "tests" | "examples" | "benches")
    })
}
```

- [ ] **Step 4: Wire the module and the `check` command.** Add to `tools/type-audit/src/lib.rs`:

```rust
pub mod walk;
```

Replace the `Ok(Command::Check { .. })` arm in `run` with:

```rust
        Ok(Command::Check { paths }) => {
            match walk::scan(&paths) {
                Ok(crates) => {
                    let mut lines: Vec<String> = Vec::new();
                    for c in &crates {
                        for f in audit::audit_items(&c.items) {
                            lines.push(format!("{}:{}: {} ({})", c.crate_name, f.line, f.message, f.item));
                        }
                    }
                    lines.sort();
                    for l in &lines {
                        println!("{l}");
                    }
                    if lines.is_empty() {
                        0
                    } else {
                        eprintln!("{} untagged/stale/malformed primitive(s)", lines.len());
                        1
                    }
                }
                Err(e) => {
                    eprintln!("scan error: {e}");
                    2
                }
            }
        }
```

- [ ] **Step 5: Run the fixture test and the full tool suite.**

Run: `cd tools/type-audit && cargo test`
Expected: PASS (all unit + integration tests).

- [ ] **Step 6: Run `check` against the real workspace — the work queue.**

Run (from repo root): `cargo run --manifest-path tools/type-audit/Cargo.toml -- check | head -20 ; echo "exit: ${PIPESTATUS[0]}"`
Expected: a list of `crate:line: untagged primitive at …` lines and **exit 1**. **This non-zero exit is correct and expected** — no tags exist yet. This list is the audit work queue (spec §6.1). CI does not run `check` until Task 14, so the workspace gate is unaffected.

- [ ] **Step 7: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/walk.rs tools/type-audit/src/lib.rs tools/type-audit/tests
git commit -m "feat(type-audit): filesystem walk + check command"
```

---

### Task 8: The `report` command

**Files:**
- Create: `tools/type-audit/src/report.rs`
- Create: `tools/type-audit/tests/report_determinism.rs`
- Modify: `tools/type-audit/src/lib.rs` (add `pub mod report;`; wire `run`)

**Interfaces:**
- Consumes: `walk::{CrateItems, scan}`, `tag::{Verdict, parse_tag}`.
- Produces:
  - `type_audit::report::render_report(&[CrateItems]) -> String` — a deterministic Markdown report: a header, a **by-class** table (bare-ok classes sorted; waiver; pending), a **by-crate** table (crates sorted; columns bare-ok/waiver/pending/total), and a **pending-by-wave** table (waves ascending). No timestamps.
  - `report` in `run` prints `render_report(&scan(&[])?)` to stdout, exit 0.
- **Counting rule:** each *verdict* on a covered position counts once toward its class; a blanket (position-less) verdict counts once per audited position it covers, classified by the blanket verdict's class. (This makes the report's total equal the number of audited positions, which is the burn-down denominator.)

- [ ] **Step 1: Write the failing determinism test.** Create `tools/type-audit/tests/report_determinism.rs`:

```rust
//! Integration: same tree → byte-identical report; content sanity.

use std::path::PathBuf;
use type_audit::report::render_report;
use type_audit::walk::scan;

fn mini() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/mini/good.rs")
}

#[test]
fn report_is_deterministic_and_counts_the_good_fixture() {
    let a = render_report(&scan(&[mini()]).unwrap());
    let b = render_report(&scan(&[mini()]).unwrap());
    assert_eq!(a, b, "report must be byte-identical across runs");
    assert!(a.contains("bare-ok(count)"));
    assert!(!a.contains("2026")); // no timestamps
}
```

- [ ] **Step 2: Run to confirm failure.**

Run: `cd tools/type-audit && cargo test --test report_determinism`
Expected: FAIL — `report` module does not exist.

- [ ] **Step 3: Implement the report.** Create `tools/type-audit/src/report.rs`:

```rust
//! The committed audit report: deterministic tallies by class, crate, wave.

use crate::tag::{Verdict, parse_tag};
use crate::walk::CrateItems;
use std::collections::BTreeMap;

/// Render the full Markdown report for `crates` (deterministic, no timestamps).
pub fn render_report(crates: &[CrateItems]) -> String {
    let mut by_class: BTreeMap<String, u64> = BTreeMap::new();
    let mut by_wave: BTreeMap<u32, u64> = BTreeMap::new();
    // crate → (bare_ok, waiver, pending)
    let mut by_crate: BTreeMap<String, (u64, u64, u64)> = BTreeMap::new();

    for c in crates {
        let entry = by_crate.entry(c.crate_name.clone()).or_default();
        for item in &c.items {
            let verdicts = parse_tag(&item.doc).unwrap_or_default();
            for p in &item.positions {
                // The verdict covering this position: its own qualifier, else blanket.
                let v = verdicts
                    .iter()
                    .find(|v| position_of(v) == Some(p.name.as_str()))
                    .or_else(|| verdicts.iter().find(|v| position_of(v).is_none()));
                match v {
                    Some(Verdict::BareOk { class, .. }) => {
                        *by_class.entry(format!("bare-ok({class})")).or_default() += 1;
                        entry.0 += 1;
                    }
                    Some(Verdict::Waiver { .. }) => {
                        *by_class.entry("waiver".to_string()).or_default() += 1;
                        entry.1 += 1;
                    }
                    Some(Verdict::Pending { wave, .. }) => {
                        *by_class.entry("pending".to_string()).or_default() += 1;
                        *by_wave.entry(*wave).or_default() += 1;
                        entry.2 += 1;
                    }
                    None => {} // uncovered — a `check` error, not counted here
                }
            }
        }
    }

    let mut s = String::new();
    s.push_str("# Type Audit Report\n\n");
    s.push_str(
        "_Generated by `tools/type-audit`. Do not edit by hand; regenerate with\n\
         `cargo run --manifest-path tools/type-audit/Cargo.toml -- report`._\n\n",
    );

    s.push_str("## Verdicts by class\n\n| Class | Count |\n|-------|------:|\n");
    for (class, n) in &by_class {
        s.push_str(&format!("| {class} | {n} |\n"));
    }

    s.push_str("\n## By crate\n\n| Crate | bare-ok | waiver | pending | total |\n");
    s.push_str("|-------|--------:|-------:|--------:|------:|\n");
    for (name, (b, w, p)) in &by_crate {
        s.push_str(&format!("| {name} | {b} | {w} | {p} | {} |\n", b + w + p));
    }

    s.push_str("\n## Pending by wave\n\n| Wave | Count |\n|------|------:|\n");
    for (wave, n) in &by_wave {
        s.push_str(&format!("| wave-{wave} | {n} |\n"));
    }

    s
}

fn position_of(v: &Verdict) -> Option<&str> {
    match v {
        Verdict::BareOk { position, .. }
        | Verdict::Waiver { position, .. }
        | Verdict::Pending { position, .. } => position.as_deref(),
    }
}
```

Note: `BTreeMap` here is the tool's own choice (deterministic ordering) — the workspace's no-`HashMap` rule is honored in spirit even though the tool is outside the workspace.

- [ ] **Step 4: Wire the module and the `report` command.** Add to `tools/type-audit/src/lib.rs`:

```rust
pub mod report;
```

Replace the `Ok(Command::Report)` arm in `run` with:

```rust
        Ok(Command::Report) => match walk::scan(&[]) {
            Ok(crates) => {
                print!("{}", report::render_report(&crates));
                0
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
```

- [ ] **Step 5: Run the report test and the whole suite.**

Run: `cd tools/type-audit && cargo test`
Expected: PASS (all tests).

- [ ] **Step 6: Confirm clippy-clean.**

Run: `cd tools/type-audit && cargo clippy --all-targets -- -D warnings`
Expected: no warnings.

- [ ] **Step 7: Commit.**

```bash
cd tools/type-audit && cargo fmt
git add tools/type-audit/src/report.rs tools/type-audit/src/lib.rs tools/type-audit/tests/report_determinism.rs
git commit -m "feat(type-audit): deterministic report command"
```

**The tool is now complete and self-tested. Tasks 9–13 apply it to the workspace.**

---

### The audit procedure (shared by Tasks 9–13)

Tasks 9–13 are **judgment passes**, not code. Each tags one or more crates. The tool's error list is the work queue; there is no test code to write — the acceptance test for each crate is `check <crate>` exiting 0. Repeat this procedure per crate, in the task's listed order:

1. **List the queue:** `cargo run --manifest-path tools/type-audit/Cargo.toml -- check <crate-root>` (e.g. `domains/terrain`). Each line is `crate:line: untagged primitive at <position> (<item>)`.
2. **Classify each item** against the rubric (spec §4), adding one `/// type-audit: …` line to the item's existing doc comment (`missing_docs` guarantees the comment exists). Use the decision procedure below. For an item mixing verdicts across positions, use position qualifiers on one comma-separated line.
3. **Record a save-format note** for every `pending` item in the task's commit message: does the value reach `Value`/world JSON serialization? (the constraint every remediation wave inherits, spec §6.2).
4. **Batch contested/novel-class items to Nathan** — one message per crate listing the item, the position, and the proposed-but-uncertain verdict. Do not invent a new `bare-ok` class; ratified outcomes extend the rubric and are recorded for decision 0028. **Stop and wait** for the batch's resolution before committing that crate.
5. **Green the crate:** `check <crate-root>` exits 0.
6. **Commit** (docs-only diff): `git commit -m "docs(<crate>): type-audit tags"` with the save-format notes in the body.

**Decision procedure (spec §4 rubric):**
- Dimensionless ratio/fraction → `bare-ok(ratio)`. Honest cardinality (counts, octaves) → `bare-ok(count)`. Position into a structure whose *type* carries meaning → `bare-ok(index)`. A newtype's sanctioned raw edge (`new(value: f64)`, `get() -> f64`) → `bare-ok(constructor-edge)`. Kernel `Value`/`Fact` trace-protocol fields → `bare-ok(envelope)`. A name/label/key contracted as plain text → `bare-ok(identifier-text)`. Screen-space pixel/glyph math in render modules → `bare-ok(render-internal)`. A `bool` unambiguous at *every* call site → `bare-ok(flag)`.
- `Fact.day` → `waiver(decision-0014)`. The elevation field's documented bare-f64 convention → `waiver(elevation-convention)`. Any other waiver cites a traceable source or it is a review failure.
- A coherent physical quantity or a confusable identity that is **not** a bare-ok class (meters, days, Kelvin, degrees/radians, AU, masses, probability-as-threshold, a `u32` that is secretly an id) → `pending(wave-N)`, N per the §7 wave map: **wave-1** kernel; **wave-2** terrain + climate; **wave-3** settlement, species, language, religion, windows.

---

### Task 9: Tag `astronomy` (worked example) + `kernel`

**Files:**
- Modify: doc comments in `domains/astronomy/src/**` and `kernel/src/**` (tag lines only).

**Interfaces:** none — docs-only.

- [ ] **Step 1: Tag `astronomy`** following the audit procedure. Astronomy is the reference implementation of the typed-quantities convention, so expect mostly `bare-ok(constructor-edge)` (the `quantity!` `new`/`get` edges), `bare-ok(ratio)` (dimensionless ratios like `angular_diameter_rel`), and `bare-ok(count)`. This crate proves the loop end-to-end and surfaces the first contested batch.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/astronomy`
Work until: exit 0.

- [ ] **Step 2: Commit astronomy.**

```bash
git add domains/astronomy/src
git commit -m "docs(astronomy): type-audit tags"
```

- [ ] **Step 3: Tag `kernel`.** Expect `bare-ok(envelope)` on `Value`/`Fact` fields (spec §3.1.6), `waiver(decision-0014)` on `Fact.day`, and `pending(wave-1)` on geosphere latitude/longitude degrees, noise coordinates, and `WorldTime` consistency (spec §7 wave-1). Record for each `pending` whether it reaches world JSON (most kernel serialized types do).

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check kernel`
Work until: exit 0.

- [ ] **Step 4: Commit kernel** with save-format notes in the body.

```bash
git add kernel/src
git commit -m "docs(kernel): type-audit tags

Save-format notes: <per pending item — reaches Value/world JSON? y/n>"
```

---

### Task 10: Tag `terrain` + `climate` (the two largest surfaces)

**Files:**
- Modify: doc comments in `domains/terrain/src/**` and `domains/climate/src/**`.

**Interfaces:** none — docs-only.

- [ ] **Step 1: Tag `terrain`** (~41 bare lines, spec provenance). The elevation field's documented convention → `waiver(elevation-convention)` (not reopened here — spec §10). Physical quantities (elevation deltas as meters, plate rates) not otherwise waived → `pending(wave-2)`. Ratios (`ocean_fraction`, `supercontinent`) → `bare-ok(ratio)`; plate/cell counts → `bare-ok(count)`; `CellId`-style indices → `bare-ok(index)`.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/terrain`
Work until: exit 0. Batch contested items to Nathan before committing.

- [ ] **Step 2: Commit terrain** with save-format notes.

```bash
git add domains/terrain/src
git commit -m "docs(terrain): type-audit tags

Save-format notes: <per pending item>"
```

- [ ] **Step 3: Tag `climate`** (~49 bare lines — the largest surface). Temperatures (Kelvin/°C), pressures, insolation → `pending(wave-2)`; dimensionless indices/ratios → `bare-ok(ratio)`; band/cell counts → `bare-ok(count)`.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/climate`
Work until: exit 0. Batch contested items to Nathan before committing.

- [ ] **Step 4: Commit climate** with save-format notes.

```bash
git add domains/climate/src
git commit -m "docs(climate): type-audit tags

Save-format notes: <per pending item>"
```

---

### Task 11: Tag `settlement`, `species`, `culture`

**Files:**
- Modify: doc comments in `domains/settlement/src/**`, `domains/species/src/**`, `domains/culture/src/**`.

**Interfaces:** none — docs-only.

- [ ] **Step 1: Tag `settlement`** (~22 bare lines). Names/keys → `bare-ok(identifier-text)`; population/site counts → `bare-ok(count)`; suitability ratios → `bare-ok(ratio)`; genuine physical/identity quantities → `pending(wave-3)`.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/settlement`
Work until: exit 0.

- [ ] **Step 2: Commit settlement.**

```bash
git add domains/settlement/src
git commit -m "docs(settlement): type-audit tags

Save-format notes: <per pending item>"
```

- [ ] **Step 3: Tag `species`**, then **`culture`**, same procedure; `pending → wave-3`.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/species`
then `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/culture`
Work until: both exit 0.

- [ ] **Step 4: Commit species and culture** (one commit each).

```bash
git add domains/species/src && git commit -m "docs(species): type-audit tags"
git add domains/culture/src && git commit -m "docs(culture): type-audit tags"
```

---

### Task 12: Tag `language`, `religion`

**Files:**
- Modify: doc comments in `domains/language/src/**`, `domains/religion/src/**`.

**Interfaces:** none — docs-only.

- [ ] **Step 1: Tag `language`.** Phoneme/label strings → `bare-ok(identifier-text)`; inventory counts → `bare-ok(count)`; weights/probabilities used as thresholds → `pending(wave-3)`; unambiguous `bool`s → `bare-ok(flag)` (per-site check — a `bool` that reads `foo(true,false)` at call sites is `pending`, not `flag`).

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/language`
Work until: exit 0.

- [ ] **Step 2: Tag `religion`**, same procedure. Religion must never learn which system produced a phenomenon — no phenomenon-source leaks into any tagged signature (spec provenance / CLAUDE.md).

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check domains/religion`
Work until: exit 0.

- [ ] **Step 3: Commit both** (one commit each).

```bash
git add domains/language/src && git commit -m "docs(language): type-audit tags"
git add domains/religion/src && git commit -m "docs(religion): type-audit tags"
```

---

### Task 13: Tag `windows/*` and `cli`

**Files:**
- Modify: doc comments in `windows/almanac/src/**`, `windows/historiography/src/**`, `windows/lab/src/**`, `windows/scene/src/**`, `windows/worldgen/src/**`, `cli/src/**`.

**Interfaces:** none — docs-only.

- [ ] **Step 1: Tag each window crate.** Windows *present* domains, so expect `bare-ok(render-internal)` (almanac/scene screen-space and glyph math), `bare-ok(count)`, `bare-ok(index)`, `bare-ok(identifier-text)` for scene keys, and `pending(wave-3)` for any world-space physical quantity that surfaces unwrapped. `windows/scene` emits committed scene JSON — any `pending` there carries a save-format note (its output is drift-checked).

Run each until exit 0:
```
cargo run --manifest-path tools/type-audit/Cargo.toml -- check windows/almanac
cargo run --manifest-path tools/type-audit/Cargo.toml -- check windows/historiography
cargo run --manifest-path tools/type-audit/Cargo.toml -- check windows/lab
cargo run --manifest-path tools/type-audit/Cargo.toml -- check windows/scene
cargo run --manifest-path tools/type-audit/Cargo.toml -- check windows/worldgen
```

- [ ] **Step 2: Commit each window crate** (one commit per crate, e.g. `docs(scene): type-audit tags`).

- [ ] **Step 3: Tag `cli`.** CLI parsing is std-only; flag values and arg counts dominate — `bare-ok(flag)`, `bare-ok(count)`, `bare-ok(identifier-text)` for paths/labels; `render-internal` for map/glyph geometry.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check cli`
Work until: exit 0.

- [ ] **Step 4: Commit cli, then confirm the whole tree is green.**

```bash
git add cli/src && git commit -m "docs(cli): type-audit tags"
cargo run --manifest-path tools/type-audit/Cargo.toml -- check ; echo "exit: $?"
```
Expected: **no output, exit 0** — the whole workspace is tagged.

- [ ] **Step 5: Confirm the workspace gate is still byte-clean** (proves no production code changed — only doc comments).

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git diff --stat HEAD~<N>  # every changed file is a src doc comment; no artifact/world-byte change
```
Expected: gate green; the drift-checked artifacts under `book/` and `clients/` are untouched.

---

### Task 14: CI wiring + commit the report

**Files:**
- Create: `docs/audits/type-audit-report.md`
- Modify: `.github/workflows/ci.yml`

**Interfaces:** none.

- [ ] **Step 1: Generate the report.**

```bash
mkdir -p docs/audits
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [ ] **Step 2: Add the two tool lines to the CI "Artifacts are current" step.** In `.github/workflows/ci.yml`, inside the `run: |` block of that step, before the final `git diff --exit-code`, add:

```yaml
          cargo run --manifest-path tools/type-audit/Cargo.toml -- check
          cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

and extend the existing `git diff --exit-code` line to include `docs/audits/`:

```yaml
          git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ clients/orrery/testdata/ docs/audits/
```

- [ ] **Step 3: Verify the CI commands locally** (the exact lines CI runs).

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check ; echo "check exit: $?"
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --exit-code docs/audits/ ; echo "diff exit: $?"
```
Expected: check exit 0; diff exit 0 (report already current). If the diff is non-zero, the report was stale — commit the regenerated file.

- [ ] **Step 4: Commit.**

```bash
git add docs/audits/type-audit-report.md .github/workflows/ci.yml
git commit -m "ci(type-audit): wire check + report into the drift step"
```

---

### Task 15: Decisions 0027 & 0028, CLAUDE.md, registry cross-link

**Files:**
- Create: `docs/decisions/0027-non-workspace-dev-tools-may-use-parser-libraries.md`
- Create: `docs/decisions/0028-the-bare-ok-rubric.md`
- Modify: `docs/decisions/README.md` (index rows)
- Modify: `CLAUDE.md` (command block + typed-quantities pointer)
- Modify: `docs/vision/idea-registry.md` (REJ-1 cross-link)

**Interfaces:** none.

- [ ] **Step 1: Write decision 0027.** Create `docs/decisions/0027-non-workspace-dev-tools-may-use-parser-libraries.md` using the README template:

```markdown
# 0027. Non-workspace dev tools may use parser libraries

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of enforcing the typed-quantities convention (decision 0008)
that nothing currently polices, facing the need to parse Rust source to find
primitives at public boundaries, we decided that **offline developer tools
that live outside the Cargo workspace may depend on parser libraries (`syn`,
`proc-macro2`); their outputs are committed and drift-checked**, accepting a
second dependency stack that the sim's build graph never sees.

**Context.** Decision 0019 bans procedural macros because `syn`/`quote` would
enter the *sim's* build graph and because the codegen candidates were
save-format contracts whose value is being plain-text. Neither force applies
to a checker that never ships in a world: it is `tools/type-audit/`, excluded
from the workspace (root `Cargo.toml` `exclude`), with its own lockfile. This
is the same posture as decisions 0009 ("models author, dice roll") and 0023
(clients carry their own toolchains): build offline, commit the output,
drift-check its freshness.

**Consequence.** `tools/type-audit/` is the first citizen; its CI presence is
two lines in the "Artifacts are current" step (`check`, `report`), never
inside `cargo test --workspace`. The workspace's serde-only allowlist
(decision 0004) and the no-proc-macro rule (0019, for *workspace* crates) are
untouched. Future offline dev tools follow the same shape.

**See also.** Decisions 0019, 0009, 0023, 0004, 0008; the type-audit spec
(`docs/superpowers/specs/2026-07-09-the-type-audit-design.md`); decision 0028
(the rubric the tool enforces).
```

- [ ] **Step 2: Write decision 0028** from the ratified rubric (fold in whatever Nathan ratified during Tasks 9–13). Create `docs/decisions/0028-the-bare-ok-rubric.md`:

```markdown
# 0028. The bare-ok rubric for primitives at API boundaries

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of the type audit (spec, decision 0027) requiring a verdict on
every primitive crossing a public boundary, facing the question of which bare
primitives are *permanently* fine versus deferred newtypes, we decided the
**bare-ok rubric**: a primitive at a public boundary is permanently acceptable
only if it falls in one of `ratio`, `count`, `index`, `constructor-edge`,
`envelope`, `identifier-text`, `render-internal`, or `flag`; anything else that
is a coherent physical quantity or a confusable identity is a deferred newtype
(`pending(wave-N)`) or a sourced `waiver`.

**Context.** Decision 0008 said dimensionless ratios *stay* bare but drew no
wider line. The audit forced the rest of the boundary to be named. The classes
are the contested cases ratified during the audit; `constructor-edge` sanctions
the `quantity!` `new`/`get` raw edges, `envelope` the kernel trace protocol's
deliberately-dumb `Value`/`Fact` fields (spec §3.1.6), `flag` an unambiguous
`bool` judged per call site.

**Consequence.** The rubric is enforced mechanically by `tools/type-audit/`
via `type-audit:` doc-comment tags; new classes are added only by ratifying a
contested case here (supersede, never edit). Waivers cite a source
(`decision-0014`, `elevation-convention`); an untraceable waiver is a review
failure. The remediation waves (spec §7) convert every `pending` tag.

**See also.** Decisions 0008, 0027, 0014; the type-audit spec §4; the elevation
convention (Campaign 3 plan).
```

- [ ] **Step 3: Add the index rows** to `docs/decisions/README.md`'s table (after the 0023 row):

```markdown
| [0027](0027-non-workspace-dev-tools-may-use-parser-libraries.md) | Non-workspace dev tools may use parser libraries | Accepted |
| [0028](0028-the-bare-ok-rubric.md) | The bare-ok rubric for primitives at API boundaries | Accepted |
```

- [ ] **Step 4: Update `CLAUDE.md`.** In the Commands block, add under the CLI examples:

```markdown
# The type audit (standalone tool outside the workspace; see decisions 0027/0028):
cargo run --manifest-path tools/type-audit/Cargo.toml -- check          # default-deny
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

In the "Typed quantities" convention bullet, append a pointer:

```markdown
  Enforced by `tools/type-audit/` (decisions 0027/0028): every primitive at a
  `pub` boundary carries a `type-audit:` verdict tag (`bare-ok`/`waiver`/`pending`).
```

- [ ] **Step 5: Cross-link the registry.** In `docs/vision/idea-registry.md`, extend the REJ-1 row's rationale/pointer cell to note that non-workspace dev tools are carved out by decision 0027 (0019 still governs workspace crates). Keep it one line; do not renumber.

- [ ] **Step 6: Run the docs drift check** (validates every cross-link resolves — spec `docs/CLAUDE.md`).

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS.

- [ ] **Step 7: Commit.**

```bash
git add docs/decisions/0027-*.md docs/decisions/0028-*.md docs/decisions/README.md CLAUDE.md docs/vision/idea-registry.md
git commit -m "docs(decisions): ratify 0027 (dev-tool parsers) + 0028 (bare-ok rubric)"
```

---

### Task 16: Book DoD — chronicle, freshness sweep, retrospective

**Files:**
- Create: `book/src/chronicle/NN-the-type-audit.md` (NN = next chronicle number)
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Create: `docs/retrospectives/campaign-27.md`
- Modify: any stale book chapter the freshness sweep finds (e.g. a typed-quantities passage that should now mention enforcement)

**Interfaces:** none.

- [ ] **Step 1: Confirm the campaign number.** Per the spec header and CLAUDE.md's parallel-campaign caution, verify the next free campaign number on `main` before writing (24 Deep Time / 26 Live Orrery context). Adjust "27" throughout the chronicle/retrospective if numbering shifted at merge.

Run: `ls docs/retrospectives/ book/src/chronicle/ | sort`
Confirm: `campaign-27` is free (else use the next free number consistently).

- [ ] **Step 2: Write the chronicle entry** `book/src/chronicle/NN-the-type-audit.md` at the book's deliberate altitude (technical, comprehensible without the code): what the audit is, the tag grammar, the rubric, the burn-down that Waves 1–3 will drive, and the deliberate `syn`-outside-the-workspace posture. Do **not** surface speculative material (book = merged reality only).

- [ ] **Step 3: Add the chronicle to `book/src/SUMMARY.md`** in the chronicle section, following the existing entry pattern.

- [ ] **Step 4: Freshness sweep.** Grep the book for typed-quantities / bare-f64 prose that is now enforced, and update it to point at the audit (do not duplicate the decisions):

```bash
grep -rniE "typed quant|bare f64|bare-f64|newtype" book/src
```
Update any chapter that now lags reality.

- [ ] **Step 5: Write the retrospective** `docs/retrospectives/campaign-27.md` (decision 0020) — process lessons only: recurring findings vs. prior retrospectives, estimate deltas (say so if none were made), spec-vs-reality. Follow `campaign-26.md`'s shape.

- [ ] **Step 6: Build the book and run the full gate.**

```bash
mdbook build book
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check ; echo "check exit: $?"
```
Expected: book builds; gate green; check exit 0.

- [ ] **Step 7: Commit.**

```bash
git add book/src/chronicle book/src/SUMMARY.md docs/retrospectives/campaign-27.md book/src
git commit -m "docs(book): chronicle, freshness sweep, retrospective for the type audit"
```

---

## Self-Review Notes

**Spec coverage (§ by §):**
- §1 goal/contract → Tasks 1–16; the no-production-code contract is a Global Constraint and re-verified in Task 13 Step 5.
- §2 the tool (non-workspace, syn, boundary definition, tracked primitives, syntactic-honesty rule) → Tasks 1–7 (exclusion in Task 1; primitives Task 2; boundary/exclusions Tasks 3–4; type-alias-launders-primitive caught at the alias RHS in Task 4; walker exclusions Task 7).
- §3 tag grammar (verdict forms, position qualifiers, multi-verdict, bidirectional errors, rustdoc visibility) → Task 5 (parser) + Task 6 (bidirectional coverage).
- §4 rubric → encoded as `BARE_OK_CLASSES` (Task 5); applied in Tasks 9–13; ratified as decision 0028 (Task 15).
- §5 enforcement/CI → Task 14 (two lines + diff scope; not in `cargo test --workspace`).
- §6 audit process → the shared audit procedure + Tasks 9–13 (queue-as-worklist, per-crate classify, contested-to-Nathan batch, per-crate commit, report/decisions last).
- §7 remediation waves → **out of scope by design** (separate campaigns); wave assignment is captured in `pending(wave-N)` tags during Tasks 9–13 and summarized by the report's pending-by-wave table.
- §8 tests → Tasks 2–8 (extraction, tag parser, bidirectional property in Task 7's integration test, report determinism in Task 8).
- §9 DoD → Tasks 14 (report/CI), 15 (decisions/CLAUDE.md), 16 (chronicle/freshness/retrospective).
- §10 non-goals → honored: no newtype conversions (Tasks 9–13 add tags only), no allowlist change (tool excluded), no relitigation (0014/elevation → waivers), no custom-attribute machinery (tags are doc-comment lines).

**Placeholder scan:** the only intentionally non-literal content is the *tag content* in Tasks 9–13 and the chronicle/retrospective prose in Task 16 — both are genuinely discovered by running the tool / reflecting on the campaign, not inventable in advance (inventing hundreds of specific tags would be fiction). Every code step in Tasks 1–8 carries complete, compiling code and an exact command with expected output. The audit procedure and rubric are stated once and referenced, per the plan skill's DRY guidance, because the *procedure* is identical across crates even though the *content* differs.

**Type consistency:** `AuditItem`/`Position` (Task 3) are consumed unchanged by `audit` (Task 6), `walk` (Task 7), and `report` (Task 8). `Verdict`/`parse_tag` (Task 5) are consumed by `audit` (Task 6) and `report` (Task 8) — both use the same `position`-accessor shape. `Command`/`parse_args` (Task 1) drive `run`'s two arms, wired in Tasks 7 (`check`) and 8 (`report`). `scan`/`CrateItems` (Task 7) feed both `check` and `report`.

**One open coordination point:** the campaign number (27) and next chronicle number (NN) are confirmed against `main` at merge (Task 16 Step 1), per CLAUDE.md's parallel-campaign caution — Deep Time (24) and Live Orrery (26) were in flight when the spec was written.
