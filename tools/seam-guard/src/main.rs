//! CLI for seam-guard. See the library docs for what a seam is, why the
//! roster is registered rather than discovered, and why the verdict is
//! three-valued rather than a pass/fail bit.

use seam_guard::{Verdict, format_list, gather, probe, render_report, tree_is_clean, verdict_of};

const USAGE: &str = "\
usage: seam-guard <command>

  list    Print every registered seam and its call sites. Cheap; no build.
  report  Write the roster as markdown to stdout (a committed artifact).
  run     Neutralise each call site, run its scoped tests, report verdicts.
          Requires a clean working tree.

`run` exits non-zero on UNGUARDED (a survivor nobody declared), STALE-DECL
(a declared survivor a test now catches — delete the declaration) and
INVALID (a mutation that would not compile, so nothing was checked).
";

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cmd = args.first().map(String::as_str).unwrap_or("");

    let registered = match gather(seam_guard::default_roots()) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(2);
        }
    };

    match cmd {
        "list" => print!("{}", format_list(&registered)),
        "report" => print!("{}", render_report(&registered)),
        "run" => {
            if !tree_is_clean() {
                eprintln!(
                    "seam-guard: refusing to run on a dirty working tree.\n\
                     This rewrites source files in place and restores them; starting clean\n\
                     means recovery from any interruption is always `git checkout -- <file>`."
                );
                std::process::exit(2);
            }

            let total: usize = registered.iter().map(|r| r.sites.len()).sum();
            if total == 0 {
                println!("seam-guard: no registered seams with call sites — nothing to probe.");
                return;
            }
            eprintln!("seam-guard: probing {total} call site(s); each runs a scoped test set.");

            let mut reds = Vec::new();
            let mut greens = 0usize;

            for r in &registered {
                for site in &r.sites {
                    let report = probe(&r.seam, site);
                    let verdict = verdict_of(&r.seam, &report.outcome);
                    let detail = match &verdict {
                        Verdict::KnownUnguarded(why) => format!("  ({why})"),
                        Verdict::DeclarationStale(why) => format!("  (declared: {why})"),
                        Verdict::Invalid(why) => format!("  ({why})"),
                        _ => String::new(),
                    };
                    println!(
                        "{:<11}{}  {}:{}{}",
                        verdict.label(),
                        report.name,
                        report.file,
                        report.line,
                        detail
                    );
                    if verdict.is_red() {
                        reds.push(verdict);
                    } else {
                        greens += 1;
                    }
                }
            }

            println!("\n{greens} ok, {} needing action", reds.len());

            if reds.iter().any(|v| matches!(v, Verdict::Unguarded)) {
                println!(
                    "\nUNGUARDED: the seam's entire contribution can be removed with the scoped\n\
                     tests still green. Add an assertion that pins it — or, if it is knowingly\n\
                     unguarded for now, declare it with a reason:\n\
                     \n    /// seam-guard: <op> scope(<crate>) expect(survives: <reason>)\n\
                     \n  A drift check is not a test. Declaring it keeps the finding visible in\n\
                     the committed roster instead of silencing it."
                );
            }
            if reds
                .iter()
                .any(|v| matches!(v, Verdict::DeclarationStale(_)))
            {
                println!(
                    "\nSTALE-DECL: a seam declared unguarded is now caught by a test. Good news —\n\
                     delete the `expect(survives: …)` clause in the same change that added the\n\
                     guard, so the roster keeps telling the truth."
                );
            }
            if reds.iter().any(|v| matches!(v, Verdict::Invalid(_))) {
                println!(
                    "\nINVALID: the mutation did not compile. It is NOT a kill — nothing was\n\
                     learned about whether a test would catch the behaviour. Fix the operator\n\
                     in the tag (a type-compatible identity index, or a returns() expression\n\
                     the call site accepts)."
                );
            }

            if !reds.is_empty() {
                std::process::exit(1);
            }
        }
        _ => {
            eprint!("{USAGE}");
            std::process::exit(2);
        }
    }
}
