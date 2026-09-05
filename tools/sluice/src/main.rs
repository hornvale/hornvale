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
            let id = args.get(1).expect("usage: set-state <id> <state> [note]");
            let st = args.get(2).expect("usage: set-state <id> <state> [note]");
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
            let (sha, note) = if args.get(1).map(String::as_str) == Some("--sha") {
                (
                    args.get(2).map(String::as_str),
                    args.get(3).map(String::as_str),
                )
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
