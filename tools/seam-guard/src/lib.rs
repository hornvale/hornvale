//! Seam-guard — find the code whose contribution nothing tests.
//!
//! # The defect class
//!
//! A *seam* is a function whose output feeds a rendered or committed
//! artifact, but whose contribution no assertion pins. Delete it and the
//! suite stays green while the world quietly renders differently. The
//! motivating case: `windows/almanac`'s `conquest_victim` decides whether a
//! settlement's ending reads as conquest or as climate migration. Neutralise
//! it and all 78 almanac tests pass — while 4 of 69 rendered lines change
//! meaning. Its only real guard is a gallery drift ritual that `make gate`
//! does not run.
//!
//! That is mechanisable, and this tool mechanises it: register the seam,
//! neutralise it at each call site, run the scoped tests, and report every
//! site where everything stayed green.
//!
//! # Why registration rather than blanket mutation
//!
//! A general mutation testing pass over this workspace would take hours and
//! bury a real finding in thousands of equivalent mutants. Registration
//! keeps the roster small and deliberate, and — the part a generic tool
//! cannot do — lets the author state a mutation that still *type-checks*.
//! See `tools/type-audit` for the same shape.
//!
//! # The outcome that is easy to get wrong
//!
//! A mutation that fails to compile is reported as [`Outcome::Invalid`], not
//! as a kill. A red from a compile error proves nothing about whether any
//! assertion would have caught the behaviour, and counting it as a kill
//! would make this tool the very thing it exists to find.

pub mod mutate;
pub mod scan;
pub mod tag;

use scan::{CallSite, WORKSPACE_ROOTS};
use std::path::Path;
use std::process::Command;
use tag::Seam;

/// What running the scoped tests under one mutation showed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The scoped tests went red. The seam is guarded.
    Killed,
    /// The scoped tests stayed green. **Nothing guards this seam.**
    Survived,
    /// The mutation did not compile, so the run says nothing either way.
    Invalid(String),
}

/// One seam paired with the call sites found for it.
#[derive(Debug, Clone)]
pub struct Registered {
    /// The registration read from the source.
    pub seam: Seam,
    /// Every non-test call site found across the scanned roots.
    pub sites: Vec<CallSite>,
}

/// The result of mutating one call site.
#[derive(Debug, Clone)]
pub struct Report {
    /// Seam name.
    pub name: String,
    /// File the mutated call lives in.
    pub file: String,
    /// 1-based line of the mutated call.
    pub line: usize,
    /// What happened.
    pub outcome: Outcome,
}

/// Scan the roots and pair every registered seam with its call sites.
///
/// Tag parse failures are returned rather than skipped: a malformed
/// registration means a seam silently stops being checked, which is exactly
/// the failure mode this tool exists to prevent.
pub fn gather(roots: &[&str]) -> Result<Vec<Registered>, String> {
    let files = scan::rust_files(roots);
    let mut sources = Vec::new();
    for path in files {
        let Ok(src) = std::fs::read_to_string(&path) else {
            continue;
        };
        sources.push((path, src));
    }

    let mut seams = Vec::new();
    let mut errors = Vec::new();
    for (path, src) in &sources {
        let (found, errs) = scan::seams_in(path, src);
        seams.extend(found);
        for (line, e) in errs {
            errors.push(format!(
                "{}:{}: malformed seam-guard tag: {}",
                path.display(),
                line,
                e
            ));
        }
    }
    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    seams.sort_by(|a, b| (&a.file, a.line).cmp(&(&b.file, b.line)));

    let mut out = Vec::new();
    for seam in seams {
        let mut sites = Vec::new();
        for (path, src) in &sources {
            sites.extend(scan::calls_in(path, src, &seam.name));
        }
        sites.sort_by_key(|s| (s.file.clone(), s.line()));
        out.push(Registered { seam, sites });
    }
    Ok(out)
}

/// Default roots, as `&str` slices.
pub fn default_roots() -> &'static [&'static str] {
    WORKSPACE_ROOTS
}

/// True iff the working tree has no uncommitted changes.
///
/// The run rewrites real source files and restores them afterwards. Refusing
/// to start on a dirty tree means recovery from any interruption is always
/// the same single command — `git checkout -- <file>` — rather than a
/// reconstruction from memory.
pub fn tree_is_clean() -> bool {
    Command::new("git")
        .args(["status", "--porcelain"])
        .output()
        .map(|o| o.stdout.is_empty())
        .unwrap_or(false)
}

/// Run the scoped test set for one package.
fn run_scope(scope: &str) -> (bool, String) {
    let out = Command::new("cargo")
        .args(["nextest", "run", "-p", scope, "--no-fail-fast"])
        .output();
    match out {
        Ok(o) => {
            let text = format!(
                "{}{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            );
            (o.status.success(), text)
        }
        Err(e) => (false, format!("failed to run cargo nextest: {e}")),
    }
}

/// Mutate one call site, run its scoped tests, and restore the file.
///
/// The original text is written back on every path, including when the test
/// command itself fails to start.
pub fn probe(seam: &Seam, site: &CallSite) -> Report {
    let file = site.file.display().to_string();
    let line = site.line();
    let mk = |outcome| Report {
        name: seam.name.clone(),
        file: file.clone(),
        line,
        outcome,
    };

    let Ok(original) = std::fs::read_to_string(&site.file) else {
        return mk(Outcome::Invalid(format!("cannot read {file}")));
    };
    let mutated = match mutate::apply(&original, site, &seam.op) {
        Ok(m) => m,
        Err(e) => return mk(Outcome::Invalid(e.to_string())),
    };
    if std::fs::write(&site.file, &mutated).is_err() {
        return mk(Outcome::Invalid(format!("cannot write {file}")));
    }

    let (passed, text) = run_scope(&seam.scope);

    // Restore before interpreting anything, so an early return cannot leave
    // the tree mutated.
    let _ = std::fs::write(&site.file, &original);

    if text.contains("could not compile") || text.contains("error[E") {
        let first = text
            .lines()
            .find(|l| l.starts_with("error"))
            .unwrap_or("compile error")
            .to_string();
        return mk(Outcome::Invalid(first));
    }
    mk(if passed {
        Outcome::Survived
    } else {
        Outcome::Killed
    })
}

/// Format the roster for `list`.
pub fn format_list(reg: &[Registered]) -> String {
    let mut out = String::new();
    if reg.is_empty() {
        out.push_str("no seams registered\n");
        return out;
    }
    for r in reg {
        out.push_str(&format!(
            "{} [{}] scope({}) — {}:{}\n",
            r.seam.name,
            scan::describe(&r.seam.op),
            r.seam.scope,
            r.seam.file,
            r.seam.line
        ));
        if r.sites.is_empty() {
            out.push_str("    (no call sites found)\n");
        }
        for s in &r.sites {
            out.push_str(&format!("    {}:{}\n", s.file.display(), s.line()));
        }
    }
    out
}

/// Relative path helper for reporting.
pub fn short(path: &Path) -> String {
    path.display().to_string()
}
