#![warn(missing_docs)]
//! The vessel window: possess an agent minted from the world and walk the
//! frozen locale mesh through a read-only verb loop (The Seam, Chunk 0 of
//! The Walk).

pub mod act;
pub mod action;
pub mod affordance;
mod agent;
pub mod body;
pub mod brief;
pub mod chamber_prose;
pub mod clock;
pub mod controller;
pub mod depth;
mod descent_thing;
pub mod doctrine;
pub mod eyes;
pub mod fabric;
mod focalize;
pub mod gate;
pub mod ground;
pub mod housemark;
pub mod interior;
mod knowledge;
pub mod lattice;
pub mod lens;
pub mod level_doc;
pub mod light;
pub mod liveness;
pub mod passage;
pub mod plan;
mod plat_prose;
mod purview;
pub mod resident;
pub mod residents;
pub mod roll;
pub mod roster;
mod session;
pub mod site;
mod sleep_site;
pub mod snapshot;
pub mod stance;
pub mod streams;
pub mod structure;
pub mod tableau;
pub mod testimony;
pub mod thing;
pub mod turn_work;
mod underground;
pub mod underworld_level;
mod vantage;
pub use agent::{most_populous_settlement, walk_depth};
pub use brief::{Brief, brief_of};
// `brief_of`'s room-keyed occupation-register value is `OccupationRecord`
// (`hornvale_history::record`); re-exported so a caller can name that map
// without a direct dependency on `hornvale-history`.
pub use chamber_prose::describe_chamber;
pub use controller::{Controller, DefaultController, ImposedController, PlayerController};
pub use depth::{CHAMBER_DEPTH_OFFSET, chamber_depth, truncate_to_walk};
pub use focalize::*;
pub use hornvale_history::record::OccupationRecord;
pub use knowledge::*;
pub use lattice::{Cell, CellKind, Lattice, Plan, Rect, allocate, embed_with, extent_for, render};
pub use level_doc::{
    LEVEL_SCHEMA, LevelCell, LevelExtent, LevelPaletteEntry, LevelPoint, SessionLevel, level_of,
};
pub use plan::{
    PLAN_SCHEMA, PaletteEntry, PlanExtent, PlanMark, PlanPoint, SessionPlan, Shading, plan_of,
};
pub use purview::*;
pub use session::{Session, WorldContext};
pub use snapshot::{
    CarriedEntry, KnownChannel, KnownEntry, Narration, NounEntry, PresentEntry, SESSION_SCHEMA,
    SelfChannel, SensedChannel, SessionSnapshot, SocialEntry, SpatialChannel, snapshot_json,
};
pub use streams::stream_labels;
pub use structure::{MAX_CHAMBERS, Structure, structure_at};
pub use tableau::{StagedBody, Tableau};
pub use underworld_level::{
    Level, LevelCellKind, generate_descent, generate_descent_for_character, generate_level,
    generate_level_extent, generate_level_with_origin, generate_level_with_water,
};
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
    /// A living occupation could not be reduced to a production brief.
    Brief(crate::brief::BriefError),
    /// Building a coarse-world view failed (worldgen).
    Build(String),
    /// [`PossessTarget::Creature`] named an entity this session's derived
    /// roster does not contain (The Hand, Task 4). Generation never
    /// guesses: an absent id fails loudly here rather than silently
    /// falling back to the flagship.
    NoSuchCreature(hornvale_kernel::EntityId),
}

impl std::fmt::Display for VesselError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VesselError::NoSettlement => write!(f, "no settlement to mint an agent from"),
            VesselError::NoSpecies(m) => write!(f, "no species known for {m}"),
            VesselError::NoPosition(m) => write!(f, "no position: {m}"),
            VesselError::Locale(e) => write!(f, "locale: {e}"),
            VesselError::Brief(e) => write!(f, "brief: {e}"),
            VesselError::Build(m) => write!(f, "building the coarse world: {m}"),
            VesselError::NoSuchCreature(id) => write!(
                f,
                "no creature with entity {} in the derived roster",
                id.get()
            ),
        }
    }
}

impl From<crate::brief::BriefError> for VesselError {
    fn from(error: crate::brief::BriefError) -> Self {
        Self::Brief(error)
    }
}

/// Which settlement the commanded body is driven at.
///
/// The `commanded` half of the possession grid (The Quire spec §7). The
/// `focalized` half is not yet a parameter, and `commanded = NONE` — which
/// yields the world viewer and attract mode — is not yet expressible.
///
/// **Neither variant mints any more (The Hand, Task 3).** `Session::start`
/// derives its roster once (`liveness::derive_npcs`) and both arms SELECT
/// which already-derived body is driven — the home settlement's own entry,
/// which `ordered_for_derivation` always hoists to the roster's front — they
/// differ only in *which* settlement supplies that home. This is what closes
/// the doctrine gap decision 0116 recorded as open ("you possess a creature
/// already living in the world" — not one invented for the occasion):
/// selecting an agent the world already derived is now exactly what both
/// variants do. `RENDER-possession-still-mints` in the idea registry names
/// that gap; The Hand closed it (decision 0227) and re-scored the row —
/// possession now selects a roster index and `mint_at` is gone.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PossessTarget {
    /// The roster body at the flagship settlement — the first `is-settlement`
    /// fact in the ledger, which is what `village_info` returns. **Not the
    /// largest**: on seed 42 the flagship is Doaba (pop. 68) while the most
    /// populous is Geoboge (pop. 82), which is what [`PossessTarget::
    /// MostPopulousSettlement`] selects. Those two names and numbers are a
    /// *reading of one world*, not an invariant — The Range (2026-08-09)
    /// re-decided the settlement contest and moved the most-populous
    /// settlement from Toa (pop. 84) to the town The Burr (ROOT_EPOCH v4)
    /// later renamed Geoboge, without touching the flagship. Re-read them
    /// after any campaign that moves placement; the
    /// property this variant relies on is only that the two can differ, and
    /// that is what the driver test asserts rather than these values.
    /// `book/src/reference/scene-tiles-v1.md`
    /// calls `kind: "flagship"` "the world's capital, the single
    /// highest-population settlement", which disagrees with that observation;
    /// the contradiction predates this campaign and is recorded in the registry
    /// rather than resolved here.
    /// The default, and byte-identical to the behaviour that predates this
    /// enum.
    #[default]
    Flagship,
    /// The roster body at the world's most-populous settlement, ranked
    /// population-descending then id-ascending. Selected, never minted (The
    /// Hand, decision 0227).
    MostPopulousSettlement,
    /// A specific, already-derived roster member, named by its ledger
    /// entity (The Hand, Task 4: "a creature on player-input" needs no
    /// new mechanism beyond naming which one). The roster itself is still
    /// seeded exactly as [`PossessTarget::Flagship`] seeds it — this variant
    /// only SELECTS which already-derived body [`crate::Session::driven_body`]
    /// names, the same "select, never mint" discipline Task 3 established
    /// for the other two variants: resolution sets the session's `driven`
    /// index to the named entity's roster position (spec §3.2: "possessing
    /// any creature is `driven = i`"), a real index rather than a
    /// front-slot swap, so no OTHER body's `!npcs`/`!why` handle number
    /// moves depending on which creature is chosen. An entity outside the
    /// derived roster (including a wild creature's, when
    /// [`PossessOpts::wild_agents`] is on) fails loudly with
    /// [`VesselError::NoSuchCreature`] rather than falling back to the
    /// flagship.
    Creature(hornvale_kernel::EntityId),
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
    /// Whose eyes the possession's chart is coloured through (The Beholding,
    /// Task 4). Defaults to [`eyes::Eyes::Own`] — colour on, through the
    /// possessed agent's own species.
    pub eyes: eyes::Eyes,
    /// Which presentation lens the *drawn* chamber plan is filtered through
    /// (The Lantern, Task 8, spec §7).
    ///
    /// **Defaults to [`lens::Lens::Off`], which is deliberately the opposite of
    /// [`lens::Lens`]'s own `Default`.** The two defaults answer different
    /// questions. `Lens::default()` answers "if a caller asks for a lens and
    /// does not say which, what do they get" — the lantern, obviously; handing
    /// back the identity there would be perverse. `PossessOpts::default()`
    /// answers "what does a possession do when nobody has said anything", and a
    /// possession's output is routinely *captured*: the book's gallery
    /// transcripts are `possess --script` output, and the client fixtures are
    /// snapshots of a default session. Spec §7's fourth constraint is that
    /// lensed colour must never land in a committed artifact, so the safe
    /// default at that boundary is off, and the CLI's interactive path opts in.
    ///
    /// This field reaches **only the terminal draw**. `plan_of` and the
    /// snapshot never see it — `lantern_lens.rs` proves both halves.
    pub lens: lens::Lens,
    /// Whose body the possession commands (The Quire, Task 2). Defaults to
    /// [`PossessTarget::Flagship`], byte-identical to the pre-existing
    /// behaviour.
    pub target: PossessTarget,
    /// A staged situation, or `None` for an ordinary derived session (The
    /// Tableau).
    ///
    /// Additive with a `Default`, so every existing construction site is
    /// unchanged. When present the tableau's cast REPLACES the derived
    /// roster — including when it is empty, which stages nobody rather than
    /// inheriting the world's own inhabitants.
    pub tableau: Option<crate::tableau::Tableau>,
}

impl Default for PossessOpts {
    /// Noon, no echo — the plain possession a test drives. Noon (day
    /// fraction 0.5) means a single default `wait 1` lands at the next
    /// noon too (fraction 0.5, still inside the diurnal active band), so a
    /// default script actually crosses an active phase rather than landing
    /// on the midnight boundary every integer day would.
    fn default() -> Self {
        PossessOpts {
            day: hornvale_kernel::WorldTime::from_std_days(0.5).expect("a day value is finite"),
            echo: false,
            wild_agents: true,
            eyes: eyes::Eyes::Own,
            lens: lens::Lens::Off,
            target: PossessTarget::Flagship,
            tableau: None,
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
/// shape as the repl's `run`, so tests drive it with buffers. Returns the
/// played world (the session's evolved ledger + registry, folded onto the
/// input world's seed — The First Mark, Task 4): "the world remembers"
/// applies to every caller of `run`, whether or not it saves the result.
///
/// The CLI no longer routes through here — `--out` calls `drive_session` and
/// then `into_played_world` directly, so `run`'s only remaining callers are in
/// `windows/vessel/tests/session.rs`. It stays public as the line-oriented
/// entry point a future non-CLI driver would use.
pub fn run(
    world: &hornvale_kernel::World,
    opts: PossessOpts,
    input: impl BufRead,
    mut output: impl Write,
) -> std::io::Result<hornvale_kernel::World> {
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
    Ok(session.into_played_world(world.seed))
}
