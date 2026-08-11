//! CLI for seam-guard. See the library docs for what a seam is and why the
//! roster is registered rather than discovered.

use seam_guard::{Outcome, format_list, gather, probe, tree_is_clean};

const USAGE: &str = "\
usage: seam-guard <command>

  list    Print every registered seam and its call sites. Cheap; no build.
  run     Neutralise each call site, run its scoped tests, report survivors.
          Requires a clean working tree. Exits non-zero if any seam SURVIVED.
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

            let mut survivors = Vec::new();
            let mut invalid = Vec::new();
            let mut killed = 0usize;

            for r in &registered {
                for site in &r.sites {
                    let report = probe(&r.seam, site);
                    match &report.outcome {
                        Outcome::Killed => {
                            killed += 1;
                            println!("KILLED    {}  {}:{}", report.name, report.file, report.line);
                        }
                        Outcome::Survived => {
                            println!("SURVIVED  {}  {}:{}", report.name, report.file, report.line);
                            survivors.push(report);
                        }
                        Outcome::Invalid(why) => {
                            println!(
                                "INVALID   {}  {}:{}  ({why})",
                                report.name, report.file, report.line
                            );
                            invalid.push(report.clone());
                        }
                    }
                }
            }

            println!(
                "\n{killed} killed, {} survived, {} invalid",
                survivors.len(),
                invalid.len()
            );

            if !invalid.is_empty() {
                println!(
                    "\nINVALID means the mutation did not compile. It is NOT a kill: nothing\n\
                     was learned about whether a test would catch the behaviour. Fix the\n\
                     operator in the seam's tag (a type-compatible identity index, or a\n\
                     returns() expression the call site accepts)."
                );
            }

            if !survivors.is_empty() {
                println!(
                    "\nSURVIVED means the seam's entire contribution can be removed with the\n\
                     scoped tests still green. Either add an assertion that pins it, or widen\n\
                     the scope if the real guard lives elsewhere (a drift check is not a test)."
                );
                std::process::exit(1);
            }
        }
        _ => {
            eprint!("{USAGE}");
            std::process::exit(2);
        }
    }
}
