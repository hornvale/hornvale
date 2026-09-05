//! The queue's CLI. `HV_SLUICE_DIR` is parsed here and nowhere else — every
//! library function takes its store explicitly.

use sluice::store::Store;
use sluice::verbs::{ClaimError, SetStateError, claim, set_state};
use std::path::PathBuf;

fn state_dir() -> PathBuf {
    if let Ok(d) = std::env::var("HV_SLUICE_DIR") {
        return PathBuf::from(d);
    }
    let home = std::env::var("HOME").expect("HOME is set");
    PathBuf::from(home).join(".local/state/hornvale/sluice")
}

/// Print a usage line and exit 2 — the brief's own documented exit code for
/// a malformed invocation. A missing argv used to reach `.expect()` and
/// panic with rc=101 (fix round 2, Important F3): indistinguishable from
/// every other panic, and one short of what the interface promised.
fn usage_error(msg: &str) -> ! {
    eprintln!("sluice: usage: {msg}");
    std::process::exit(2);
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let store = Store::new(state_dir()).expect("state dir");
    match args.first().map(String::as_str) {
        Some("list") => {
            for r in store.read_rows().expect("read") {
                println!("{}", r.render());
            }
        }
        Some("set-state") => {
            let Some(id) = args.get(1) else {
                usage_error("set-state <id> <state> [note]");
            };
            let Some(st) = args.get(2) else {
                usage_error("set-state <id> <state> [note]");
            };
            match set_state(&store, id, st, args.get(3).map(String::as_str)) {
                Ok(()) => {}
                Err(SetStateError::NoSuchRow) => {
                    eprintln!("sluice: set-state: no row with id '{id}' — NOTHING WAS CHANGED.");
                    std::process::exit(1);
                }
                Err(SetStateError::BadState) => {
                    eprintln!("sluice: set-state: '{st}' is not a known state");
                    std::process::exit(1);
                }
                Err(SetStateError::Io(e)) => {
                    eprintln!("sluice: set-state: {e}");
                    std::process::exit(1);
                }
            }
        }
        Some("claim") => {
            // `--sha` REQUIRES its value (fix round 2, Important F2). Reading
            // a missing value as `None` silently produced the DISPATCHER form
            // (claim the head of the queue) instead of a usage error — a
            // malformed argv would have WRITTEN TO THE STATE MACHINE. Bash
            // refused this with `${2:?usage: …}`; the port must too.
            let (sha, note) = if args.get(1).map(String::as_str) == Some("--sha") {
                let Some(sha) = args.get(2) else {
                    usage_error("claim --sha <sha> [note]");
                };
                (Some(sha.as_str()), args.get(3).map(String::as_str))
            } else {
                (None, args.get(1).map(String::as_str))
            };
            match claim(&store, sha, note) {
                Ok(Some(r)) => println!("{}", r.render()),
                Ok(None) => {}
                Err(ClaimError::HeldByAnother) => {
                    eprintln!(
                        "sluice: claim: a row for that ref exists but is NOT queued — somebody else has it. NOTHING WAS CHANGED."
                    );
                    std::process::exit(4);
                }
                Err(ClaimError::NoSuchRow) => {
                    eprintln!("sluice: claim: no row at all for that ref.");
                    std::process::exit(5);
                }
                Err(ClaimError::Io(e)) => {
                    eprintln!("sluice: claim: {e}");
                    std::process::exit(1);
                }
            }
        }
        other => {
            eprintln!("sluice: unknown command {other:?}");
            std::process::exit(2);
        }
    }
}
