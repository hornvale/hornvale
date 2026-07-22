#![warn(missing_docs)]
//! The vessel window: possess an agent minted from the world and walk the
//! frozen locale mesh through a read-only verb loop (The Seam, Chunk 0 of
//! The Walk).

mod agent;
mod focalize;
mod knowledge;
pub mod liveness;
mod session;
pub mod streams;
mod vantage;
pub use agent::{Agent, AgentId, mint_flagship, walk_depth};
pub use focalize::*;
pub use knowledge::*;
pub use session::Session;
pub use streams::stream_labels;
pub use vantage::*;

use std::io::{BufRead, Write};

/// Why a possession could not begin or proceed.
/// type-audit: bare-ok(prose: NoSpecies.0), bare-ok(prose: NoPosition.0), bare-ok(prose: Build.0)
#[derive(Debug, Clone, PartialEq)]
pub enum VesselError {
    /// The world has no settlements to mint from.
    NoSettlement,
    /// The settlement's species is unknown to the registry.
    NoSpecies(String),
    /// The settlement has no committed position fact.
    NoPosition(String),
    /// The locale window could not describe a room.
    Locale(hornvale_locale::LocaleError),
    /// Building a coarse-world view failed (worldgen).
    Build(String),
}

impl std::fmt::Display for VesselError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VesselError::NoSettlement => write!(f, "no settlement to mint an agent from"),
            VesselError::NoSpecies(m) => write!(f, "no species known for {m}"),
            VesselError::NoPosition(m) => write!(f, "no position: {m}"),
            VesselError::Locale(e) => write!(f, "locale: {e}"),
            VesselError::Build(m) => write!(f, "building the coarse world: {m}"),
        }
    }
}

/// Options for a possession.
/// type-audit: bare-ok(flag: echo), bare-ok(flag: wild_agents)
pub struct PossessOpts {
    /// The frozen day the possession observes.
    pub day: hornvale_kernel::WorldTime,
    /// Echo each command line (script/transcript mode).
    pub echo: bool,
    /// Whether to append the world's wild beast agents to the derived NPCs
    /// (The Wilding). On by default — the game and its galleries show the
    /// fauna walking alongside the peoples. A settled-population unit test
    /// that isolates the peopled narration path sets this off.
    pub wild_agents: bool,
}

impl Default for PossessOpts {
    /// Noon, no echo — the plain possession a test drives. Noon (day
    /// fraction 0.5) means a single default `wait 1` lands at the next
    /// noon too (fraction 0.5, still inside the diurnal active band), so a
    /// default script actually crosses an active phase rather than landing
    /// on the midnight boundary every integer day would.
    fn default() -> Self {
        PossessOpts {
            day: hornvale_kernel::WorldTime { day: 0.5 },
            echo: false,
            wild_agents: true,
        }
    }
}

/// One verb's outcome.
/// type-audit: bare-ok(prose: Out.0), bare-ok(prose: Released.0)
pub enum Turn {
    /// Text to print; the possession continues.
    Out(String),
    /// Final text; the possession ends.
    Released(String),
}

/// Drive a session over line-based I/O until release or EOF — the same
/// shape as the repl's `run`, so tests drive it with buffers.
pub fn run(
    world: &hornvale_kernel::World,
    opts: PossessOpts,
    input: impl BufRead,
    mut output: impl Write,
) -> std::io::Result<()> {
    let (mut session, opening) = match Session::start(world, &opts) {
        Ok(x) => x,
        Err(e) => {
            writeln!(output, "error: {e}")?;
            return Err(std::io::Error::other(e.to_string()));
        }
    };
    writeln!(output, "{opening}")?;
    for line in input.lines() {
        let line = line?;
        if opts.echo {
            writeln!(output, "> {line}")?;
        }
        match session.handle(&line) {
            Turn::Out(s) => {
                if !s.is_empty() {
                    writeln!(output, "{s}")?;
                }
            }
            Turn::Released(s) => {
                writeln!(output, "{s}")?;
                break;
            }
        }
    }
    Ok(())
}
