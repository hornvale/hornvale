//! The hornvale CLI: create worlds, render almanacs, interrogate via REPL.
#![warn(missing_docs)]

mod concepts;
mod repl;
mod streams;

use hornvale_astronomy::{SkyPins, parse_pin};
use hornvale_kernel::{Seed, World};
use hornvale_worldgen as world_builder;
use std::process::ExitCode;

const SKY_FLAGS: &str =
    "  [--sky constant|generated]               sky provider (default: generated)
  [--moons N|MIN+K]                        pin the moon count, exact or graded
  [--rotation normal|locked]               pin the rotation regime
  [--day-hours F]                          pin the solar day length, in standard hours
  [--obliquity none|F]                     pin axial tilt in degrees
  [--year-days F]                          pin the year length, in local days
  [--neighbor blue-giant|red-giant|white-dwarf|orange-giant|red-dwarf|sun-like]
                                            force one showpiece neighbor star
";

const USAGE: &str = "\
usage:
  hornvale new --seed <N> [--out <PATH>] [sky flags]
                                            create a world (default out: world.json)
  hornvale scout [sky flags] [--from-seed N] [--limit K] [--max-scan M]
                                            scan seeds for ones satisfying the pins
  hornvale almanac [--world <PATH>]        render the almanac (default: world.json)
  hornvale repl [--world <PATH>]           interrogate a world interactively
  hornvale concepts                        dump the concept registry as markdown
  hornvale streams                         dump the stream manifest as markdown

sky flags (shared by new and scout):
";

fn usage() -> String {
    format!("{USAGE}{SKY_FLAGS}")
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("new") => cmd_new(&args),
        Some("scout") => cmd_scout(&args),
        Some("almanac") => cmd_almanac(&args),
        Some("repl") => cmd_repl(&args),
        Some("concepts") => cmd_concepts(),
        Some("streams") => cmd_streams(),
        Some("help") | None => {
            print!("{}", usage());
            Ok(())
        }
        Some(other) => Err(format!("unknown command '{other}'\n{}", usage())),
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

/// Parse the sky-related flags shared by `new` and `scout` into pins plus a
/// sky choice. One parser: every flag becomes a `key=value` pin string and
/// folds through `astronomy::parse_pin`, so pin-string syntax never drifts
/// from flag syntax. Default sky is `Generated` (spec §8).
fn parse_sky_args(args: &[String]) -> Result<(SkyPins, world_builder::SkyChoice), String> {
    let sky = match flag_value(args, "--sky") {
        None | Some("generated") => world_builder::SkyChoice::Generated,
        Some("constant") => world_builder::SkyChoice::Constant,
        Some(other) => return Err(format!("--sky: unknown value '{other}'")),
    };

    let mut pins = SkyPins::default();
    for (flag, key) in [
        ("--moons", "moons"),
        ("--rotation", "rotation"),
        ("--day-hours", "day-hours"),
        ("--obliquity", "obliquity"),
        ("--year-days", "year-days"),
        ("--neighbor", "neighbor"),
    ] {
        if let Some(value) = flag_value(args, flag) {
            parse_pin(&format!("{key}={value}"), &mut pins)?;
        }
    }
    Ok((pins, sky))
}

fn cmd_new(args: &[String]) -> Result<(), String> {
    let seed: u64 = flag_value(args, "--seed")
        .ok_or("new requires --seed <N>")?
        .parse()
        .map_err(|e| format!("--seed must be a u64: {e}"))?;
    let out = flag_value(args, "--out").unwrap_or("world.json");
    let (pins, sky) = parse_sky_args(args)?;
    let world = world_builder::build_world(Seed(seed), &pins, sky).map_err(|e| e.to_string())?;
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

/// Deterministically scan seeds upward from `--from-seed` (default 0) for
/// ones whose generated sky satisfies the pins, printing up to `--limit`
/// (default 5) matches and scanning at most `--max-scan` (default 10000)
/// seeds. Read-only: never writes a world.
fn cmd_scout(args: &[String]) -> Result<(), String> {
    let (pins, _sky) = parse_sky_args(args)?;
    let from: u64 = flag_value(args, "--from-seed")
        .unwrap_or("0")
        .parse()
        .map_err(|e| format!("--from-seed: {e}"))?;
    let limit: u64 = flag_value(args, "--limit")
        .unwrap_or("5")
        .parse()
        .map_err(|e| format!("--limit: {e}"))?;
    let max_scan: u64 = flag_value(args, "--max-scan")
        .unwrap_or("10000")
        .parse()
        .map_err(|e| format!("--max-scan: {e}"))?;
    let mut found = 0u64;
    let mut scanned = 0u64;
    for seed in from..from.saturating_add(max_scan) {
        scanned += 1;
        if let Ok(outcome) = hornvale_astronomy::generate(hornvale_kernel::Seed(seed), &pins) {
            let system = &outcome.system;
            let day = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day } => {
                    format!("{:.1}h day", day.get() * 24.0)
                }
                hornvale_astronomy::Rotation::Locked => "tidally locked".to_string(),
            };
            println!(
                "seed {seed}: {} moons, {day}, year {:.1} std days, {} neighbors",
                system.moons.len(),
                system.anchor.year.get(),
                system.neighbors.len()
            );
            found += 1;
            if found == limit {
                break;
            }
        }
    }
    println!("scanned {scanned} seeds, found {found}");
    Ok(())
}

/// Load a world from `--world` (default world.json).
fn load_world(args: &[String]) -> Result<World, String> {
    let path = flag_value(args, "--world").unwrap_or("world.json");
    World::load(std::path::Path::new(path)).map_err(|e| format!("loading {path}: {e}"))
}

fn cmd_almanac(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let ctx = world_builder::almanac_context(&world).map_err(|e| e.to_string())?;
    print!("{}", hornvale_almanac::render(&ctx));
    Ok(())
}

fn cmd_repl(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    repl::run(&world, stdin.lock(), stdout.lock()).map_err(|e| e.to_string())
}

fn cmd_concepts() -> Result<(), String> {
    // Generated, not constant: the registry is identical either way (every
    // predicate is registered up front), but this exercises the fuller
    // pipeline as a bonus smoke test of the sky genesis wiring.
    let world = world_builder::build_world(
        Seed(0),
        &SkyPins::default(),
        world_builder::SkyChoice::Generated,
    )
    .map_err(|e| e.to_string())?;
    print!("{}", concepts::render_concepts(&world.registry));
    Ok(())
}

fn cmd_streams() -> Result<(), String> {
    print!("{}", streams::render_streams());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::{MoonsPin, NeighborClass, RotationPin};

    fn args(flags: &[&str]) -> Vec<String> {
        flags.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn default_sky_is_generated_with_no_pins() {
        let (pins, sky) = parse_sky_args(&args(&[])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
        assert_eq!(pins, SkyPins::default());
    }

    #[test]
    fn sky_flag_selects_constant() {
        let (_, sky) = parse_sky_args(&args(&["--sky", "constant"])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Constant);
    }

    #[test]
    fn sky_flag_selects_generated_explicitly() {
        let (_, sky) = parse_sky_args(&args(&["--sky", "generated"])).unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
    }

    #[test]
    fn sky_flag_rejects_unknown_value() {
        let err = parse_sky_args(&args(&["--sky", "bogus"])).unwrap_err();
        assert!(err.contains("--sky"), "unexpected error text: {err}");
    }

    #[test]
    fn moons_flag_parses_exact_count() {
        let (pins, _) = parse_sky_args(&args(&["--moons", "2"])).unwrap();
        assert_eq!(pins.moons, Some(MoonsPin::exact(2).unwrap()));
    }

    #[test]
    fn moons_flag_parses_graded_pin() {
        let (pins, _) = parse_sky_args(&args(&["--moons", "2+1"])).unwrap();
        assert_eq!(pins.moons, Some(MoonsPin::graded(2, 1).unwrap()));
    }

    #[test]
    fn rotation_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--rotation", "locked"])).unwrap();
        assert_eq!(pins.rotation, Some(RotationPin::Locked));
    }

    #[test]
    fn day_hours_flag_sets_a_period_hours_rotation() {
        let (pins, _) = parse_sky_args(&args(&["--day-hours", "30"])).unwrap();
        assert_eq!(pins.rotation, Some(RotationPin::PeriodHours(30.0)));
    }

    #[test]
    fn obliquity_flag_parses_a_number() {
        let (pins, _) = parse_sky_args(&args(&["--obliquity", "7.5"])).unwrap();
        assert_eq!(pins.obliquity.unwrap().get(), 7.5);
    }

    #[test]
    fn obliquity_flag_parses_none() {
        let (pins, _) = parse_sky_args(&args(&["--obliquity", "none"])).unwrap();
        assert_eq!(pins.obliquity.unwrap().get(), 0.0);
    }

    #[test]
    fn year_days_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--year-days", "300"])).unwrap();
        assert_eq!(pins.year_local_days.unwrap().get(), 300.0);
    }

    #[test]
    fn neighbor_flag_parses() {
        let (pins, _) = parse_sky_args(&args(&["--neighbor", "blue-giant"])).unwrap();
        assert_eq!(pins.neighbor, Some(NeighborClass::BlueGiant));
    }

    #[test]
    fn all_flags_combine_into_one_pin_set() {
        let (pins, sky) = parse_sky_args(&args(&[
            "--sky",
            "generated",
            "--moons",
            "1+2",
            "--obliquity",
            "12.5",
            "--neighbor",
            "red-giant",
        ]))
        .unwrap();
        assert_eq!(sky, world_builder::SkyChoice::Generated);
        assert_eq!(pins.moons, Some(MoonsPin::graded(1, 2).unwrap()));
        assert_eq!(pins.obliquity.unwrap().get(), 12.5);
        assert_eq!(pins.neighbor, Some(NeighborClass::RedGiant));
    }

    #[test]
    fn bad_moons_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--moons", "abc"])).unwrap_err();
        assert!(err.contains("moons"), "unexpected error text: {err}");
    }

    #[test]
    fn moons_exceeding_the_legal_maximum_yields_constructor_error_text() {
        let err = parse_sky_args(&args(&["--moons", "4"])).unwrap_err();
        assert!(
            err.contains("legal maximum"),
            "unexpected error text: {err}"
        );
    }

    #[test]
    fn bad_rotation_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--rotation", "sideways"])).unwrap_err();
        assert!(err.contains("rotation"), "unexpected error text: {err}");
    }

    #[test]
    fn bad_obliquity_value_yields_constructor_error_text() {
        let err = parse_sky_args(&args(&["--obliquity", "-1"])).unwrap_err();
        assert!(!err.is_empty());
    }

    #[test]
    fn bad_neighbor_value_yields_parse_error_text() {
        let err = parse_sky_args(&args(&["--neighbor", "green-dwarf"])).unwrap_err();
        assert!(err.contains("neighbor"), "unexpected error text: {err}");
    }

    #[test]
    fn usage_mentions_scout() {
        assert!(USAGE.contains("scout"));
    }

    #[test]
    fn usage_mentions_sky_flags() {
        let full = usage();
        assert!(full.contains("--sky"));
        assert!(full.contains("--moons"));
        assert!(full.contains("--neighbor"));
    }
}
