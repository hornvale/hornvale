//! The REPL window: interrogate a world line by line. Generic over
//! input/output so tests drive it with buffers.

use hornvale_kernel::{EntityId, Value, World, WorldTime};
use std::io::{BufRead, Write};

const HELP: &str = "\
commands:
  sky [day]        what the sky looks like (default day 0)
  climate          local climate
  places           known places
  village          the settlement
  castes           the settlement's castes
  beliefs          recorded beliefs
  why <n>          why belief n (from `beliefs`) is held
  phenomena [day]  salient phenomena (default day 0)
  facts <id>       every fact about entity id
  quit             leave
";

/// Run the REPL over a world until `quit` or EOF.
pub fn run(world: &World, input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
    writeln!(
        output,
        "hornvale repl — world of seed {} ('help' for commands)",
        world.seed.0
    )?;
    for line in input.lines() {
        let line = line?;
        let mut parts = line.split_whitespace();
        let command = parts.next().unwrap_or("");
        let argument = parts.next();
        match command {
            "" => {}
            "quit" | "exit" => break,
            "help" => write!(output, "{HELP}")?,
            "sky" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                let report = crate::world_builder::sky_report(world, WorldTime { day });
                writeln!(output, "{}", report.description)?;
            }
            "climate" => {
                let report = crate::world_builder::climate_report(world);
                writeln!(
                    output,
                    "{} ({:.0}°C)",
                    report.description, report.temperature_c
                )?;
            }
            "places" => {
                for place in hornvale_terrain::places(world) {
                    writeln!(
                        output,
                        "{} — {} (entity {})",
                        place.name, place.biome, place.id.0
                    )?;
                }
            }
            "village" => match hornvale_settlement::village_info(world) {
                Some(v) => writeln!(
                    output,
                    "{} — population {} (entity {})",
                    v.name, v.population, v.id.0
                )?,
                None => writeln!(output, "no settlement is known")?,
            },
            "castes" => match hornvale_settlement::village_info(world) {
                Some(v) => {
                    let castes = hornvale_culture::castes_of(world, v.id);
                    writeln!(output, "{}", castes.join(", "))?;
                }
                None => writeln!(output, "no settlement is known")?,
            },
            "beliefs" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                if beliefs.is_empty() {
                    writeln!(output, "no beliefs are recorded")?;
                }
                for (i, belief) in beliefs.iter().enumerate() {
                    writeln!(output, "{}. {}", i + 1, belief.tenet)?;
                }
            }
            "why" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                let picked = argument
                    .and_then(|a| a.parse::<usize>().ok())
                    .and_then(|n| beliefs.get(n.checked_sub(1)?));
                match picked {
                    Some(belief) => match hornvale_religion::why(world, belief.id) {
                        Some(reason) => writeln!(output, "{reason}")?,
                        None => writeln!(output, "no recorded derivation")?,
                    },
                    None => writeln!(output, "usage: why <n> — n from `beliefs`")?,
                }
            }
            "phenomena" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                for p in crate::world_builder::observed_phenomena(world, day) {
                    writeln!(output, "[{:.2}] {} — {}", p.salience, p.kind, p.description)?;
                }
            }
            "facts" => {
                let id = argument.and_then(|a| a.parse::<u64>().ok());
                match id {
                    Some(id) => {
                        for f in world.ledger.facts_about(EntityId(id)) {
                            writeln!(
                                output,
                                "{} = {} ({})",
                                f.predicate,
                                render_value(&f.object),
                                f.provenance
                            )?;
                        }
                    }
                    None => writeln!(output, "usage: facts <entity-id>")?,
                }
            }
            other => writeln!(output, "unknown command '{other}' — try 'help'")?,
        }
    }
    Ok(())
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world_builder::{SkyChoice, build_world};
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;

    fn drive(commands: &str) -> String {
        let world = build_world(Seed(42), &SkyPins::default(), SkyChoice::Constant).unwrap();
        let mut out = Vec::new();
        run(&world, commands.as_bytes(), &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn sky_reports_the_constant_sun() {
        assert!(drive("sky\nquit\n").contains("zenith"));
    }

    #[test]
    fn village_and_castes_and_beliefs_report() {
        let out = drive("village\ncastes\nbeliefs\nquit\n");
        assert!(out.contains("population"));
        assert!(out.contains("shaman"));
        assert!(out.contains("1."));
    }

    #[test]
    fn why_explains_belief_one() {
        let out = drive("why 1\nquit\n");
        assert!(out.contains("celestial-body"));
    }

    #[test]
    fn facts_lists_an_entity() {
        let out = drive("facts 2\nquit\n");
        assert!(out.contains("(settlement)") || out.contains("(terrain)"));
    }

    #[test]
    fn unknown_commands_are_reported_not_fatal() {
        let out = drive("dance\nsky\nquit\n");
        assert!(out.contains("unknown command"));
        assert!(out.contains("zenith"));
    }

    #[test]
    fn eof_ends_the_loop_without_quit() {
        assert!(drive("sky\n").contains("zenith"));
    }
}
