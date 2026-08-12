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

/// What the run concludes once an [`Outcome`] is read against the seam's own
/// declaration.
///
/// The verdict is three-valued on purpose. A binary gate that fails on the
/// mere *existence* of an unguarded seam goes red on day one and stays red,
/// which trains everyone to stop reading it — and a report-only check that
/// never fails is ignored just as fast. The escape is to fail on **novelty**
/// rather than existence, the same ratchet `tropes check`, the timings
/// baseline and `type-audit`'s `waiver(...)` all use.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// A test caught the mutation, and nothing claimed otherwise. Green.
    Guarded,
    /// Nothing guards this seam and nothing said so. **Red** — the finding.
    Unguarded,
    /// Declared unguarded, with a reason, and still unguarded. Green, loudly.
    KnownUnguarded(String),
    /// Declared unguarded — but a test caught it. **Red.**
    ///
    /// The direction that makes the declaration honest. Someone added the
    /// missing assertion; the acknowledgement is now a lie and must be
    /// deleted in the same change. Without this, a declaration can only ever
    /// be satisfied, and the roster silently fills with stale excuses.
    DeclarationStale(String),
    /// The mutation did not compile. **Red** — this registration is checking
    /// nothing at all, which is this tool's own failure mode.
    Invalid(String),
}

impl Verdict {
    /// True iff this verdict should fail the gate.
    pub fn is_red(&self) -> bool {
        matches!(
            self,
            Self::Unguarded | Self::DeclarationStale(_) | Self::Invalid(_)
        )
    }

    /// Short label for the report column.
    pub fn label(&self) -> &'static str {
        match self {
            Self::Guarded => "GUARDED",
            Self::Unguarded => "UNGUARDED",
            Self::KnownUnguarded(_) => "KNOWN",
            Self::DeclarationStale(_) => "STALE-DECL",
            Self::Invalid(_) => "INVALID",
        }
    }
}

/// Read an outcome against the seam's declaration.
pub fn verdict_of(seam: &Seam, outcome: &Outcome) -> Verdict {
    match (outcome, seam.expect_survives.as_deref()) {
        (Outcome::Invalid(w), _) => Verdict::Invalid(w.clone()),
        (Outcome::Killed, None) => Verdict::Guarded,
        (Outcome::Killed, Some(r)) => Verdict::DeclarationStale(r.to_string()),
        (Outcome::Survived, None) => Verdict::Unguarded,
        (Outcome::Survived, Some(r)) => Verdict::KnownUnguarded(r.to_string()),
    }
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
        if let Some(reason) = &r.seam.expect_survives {
            out.push_str(&format!("    declared unguarded: {reason}\n"));
        }
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

/// Render the roster as a committed, drift-checkable artifact.
///
/// Deliberately **static**: registrations, declarations and call sites only,
/// never verdicts. Verdicts cost a scoped test run per site, so an artifact
/// carrying them could not be regenerated cheaply and would rot. What this
/// makes visible is the thing that actually needs review pressure — the list
/// of seams somebody has declared unguarded, and their stated reasons. A
/// declaration that has to survive a diff is much harder to leave lying
/// around than one buried in a doc comment.
pub fn render_report(reg: &[Registered]) -> String {
    let mut out = String::new();
    out.push_str("# Seam-guard roster\n\n");
    out.push_str(
        "Generated by `scripts/regenerate-artifacts.sh` — do not edit by hand.\n\n\
         A *seam* is a function whose output reaches a rendered or committed artifact\n\
         but whose contribution no assertion pins. `make seam-guard` neutralises each\n\
         call site below and reports whether the scoped tests notice.\n\n\
         This page is static: it lists what is registered and what has been declared,\n\
         never the verdicts (those cost a test run per site). Its job is to keep the\n\
         **declarations** under review pressure — an `expect(survives: …)` that has to\n\
         survive a diff is harder to leave lying around than one buried in a doc\n\
         comment.\n\n",
    );

    let declared: Vec<&Registered> = reg
        .iter()
        .filter(|r| r.seam.expect_survives.is_some())
        .collect();
    out.push_str(&format!(
        "{} seam(s) registered; {} declared unguarded.\n\n",
        reg.len(),
        declared.len()
    ));

    if reg.is_empty() {
        out.push_str("_No seams registered._\n");
        return out;
    }

    for r in reg {
        out.push_str(&format!("## `{}`\n\n", r.seam.name));
        out.push_str(&format!("- Defined at `{}:{}`\n", r.seam.file, r.seam.line));
        out.push_str(&format!("- Operator: `{}`\n", scan::describe(&r.seam.op)));
        out.push_str(&format!("- Test scope: `{}`\n", r.seam.scope));
        match &r.seam.expect_survives {
            Some(reason) => out.push_str(&format!(
                "- **Declared unguarded:** {reason}\n\
                 - This seam is expected to SURVIVE. If a test ever catches it, the\n  \
                 run goes red and this declaration must be deleted in the same change.\n"
            )),
            None => out.push_str("- No declaration: a survivor here fails the gate.\n"),
        }
        out.push_str("- Call sites:\n");
        if r.sites.is_empty() {
            out.push_str("  - _none found_\n");
        }
        for s in &r.sites {
            out.push_str(&format!("  - `{}:{}`\n", s.file.display(), s.line()));
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use tag::Op;

    fn seam(expect: Option<&str>) -> Seam {
        Seam {
            name: "f".into(),
            op: Op::Returns("None".into()),
            scope: "crate".into(),
            expect_survives: expect.map(str::to_string),
            file: "a.rs".into(),
            line: 1,
        }
    }

    #[test]
    fn an_undeclared_seam_that_is_caught_is_green() {
        let v = verdict_of(&seam(None), &Outcome::Killed);
        assert_eq!(v, Verdict::Guarded);
        assert!(!v.is_red());
    }

    #[test]
    fn an_undeclared_survivor_is_red() {
        let v = verdict_of(&seam(None), &Outcome::Survived);
        assert_eq!(v, Verdict::Unguarded);
        assert!(v.is_red());
    }

    #[test]
    fn a_declared_survivor_is_known_and_green() {
        let v = verdict_of(&seam(Some("being fixed")), &Outcome::Survived);
        assert_eq!(v, Verdict::KnownUnguarded("being fixed".into()));
        assert!(!v.is_red());
    }

    #[test]
    fn a_declared_seam_that_gets_caught_is_red() {
        // The direction that keeps declarations honest: the guard arrived, so
        // the acknowledgement is now false and must go.
        let v = verdict_of(&seam(Some("being fixed")), &Outcome::Killed);
        assert_eq!(v, Verdict::DeclarationStale("being fixed".into()));
        assert!(v.is_red());
    }

    #[test]
    fn an_invalid_mutation_is_red_whatever_was_declared() {
        // A registration whose mutation cannot compile is checking nothing —
        // declaring it survives must not launder that into green.
        for decl in [None, Some("being fixed")] {
            let v = verdict_of(&seam(decl), &Outcome::Invalid("E0282".into()));
            assert!(v.is_red(), "invalid must stay red with decl={decl:?}");
        }
    }

    #[test]
    fn the_report_names_a_declaration_and_its_reason() {
        let reg = vec![Registered {
            seam: seam(Some("gallery-drift-only")),
            sites: Vec::new(),
        }];
        let out = render_report(&reg);
        assert!(out.contains("1 declared unguarded"), "{out}");
        assert!(out.contains("gallery-drift-only"), "{out}");
    }

    #[test]
    fn the_report_is_deterministic() {
        let reg = vec![Registered {
            seam: seam(None),
            sites: Vec::new(),
        }];
        assert_eq!(render_report(&reg), render_report(&reg));
    }
}
