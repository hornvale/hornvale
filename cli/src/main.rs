//! The hornvale CLI: create worlds, render almanacs, interrogate via REPL.
#![warn(missing_docs)]

mod world_builder;

use hornvale_kernel::{Seed, World};
use std::process::ExitCode;

const USAGE: &str = "\
usage:
  hornvale new --seed <N> [--out <PATH>]   create a world (default out: world.json)
  hornvale almanac [--world <PATH>]        render the almanac (default: world.json)
  hornvale repl [--world <PATH>]           interrogate a world interactively
  hornvale concepts                        dump the concept registry as markdown
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("new") => cmd_new(&args),
        Some("help") | None => {
            print!("{USAGE}");
            Ok(())
        }
        Some(other) => Err(format!("unknown command '{other}'\n{USAGE}")),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

/// Value of `--flag` in args, if present.
fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

fn cmd_new(args: &[String]) -> Result<(), String> {
    let seed: u64 = flag_value(args, "--seed")
        .ok_or("new requires --seed <N>")?
        .parse()
        .map_err(|e| format!("--seed must be a u64: {e}"))?;
    let out = flag_value(args, "--out").unwrap_or("world.json");
    let world = world_builder::build_world(Seed(seed)).map_err(|e| e.to_string())?;
    world
        .save(std::path::Path::new(out))
        .map_err(|e| format!("saving {out}: {e}"))?;
    let village = hornvale_settlement::village_info(&world)
        .map(|v| v.name)
        .unwrap_or_else(|| "no settlement".to_string());
    println!(
        "world of seed {seed} written to {out} ({} facts; village: {village})",
        world.ledger.len()
    );
    Ok(())
}

/// Load a world from `--world` (default world.json).
// used from Task 10 (almanac/repl commands)
#[allow(dead_code)]
fn load_world(args: &[String]) -> Result<World, String> {
    let path = flag_value(args, "--world").unwrap_or("world.json");
    World::load(std::path::Path::new(path)).map_err(|e| format!("loading {path}: {e}"))
}
