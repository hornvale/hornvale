//! The possession session: a pure step function over a frozen world. Every
//! verb is read-only; possessing a world never changes it.

use crate::{
    Agent, Focalized, Focalizer, IdentityProjection, Knowledge, PossessOpts, Projection,
    TemplateFocalizer, Turn, VesselError, mint_flagship, observable,
};
use hornvale_kernel::{RoomAddr, World, WorldTime};
use hornvale_locale::{Compass, Direction, ExitKind, LocaleContext};

const HELP: &str = "\
verbs:
  look             where you stand, focalized
  go <dir>         walk a compass exit (n ne e se s sw w nw)
  examine <thing>  anything look mentions
  back             retrace your last step
  wait [N]         let N days pass overhead (default 1); the world stays still
  whoami           the one you possess
  knows            everything they have seen
  release          let go (quit works too)
";

/// A live possession over a frozen world.
pub struct Session<'w> {
    world: &'w World,
    ctx: LocaleContext,
    agent: Agent,
    knowledge: Knowledge,
    trail: Vec<RoomAddr>,
    day: WorldTime,
    focalizer: TemplateFocalizer,
    projection: IdentityProjection,
}

impl<'w> Session<'w> {
    /// Begin a possession: build the locale context, mint the flagship
    /// agent, absorb the first projection, and return the opening text.
    /// type-audit: bare-ok(prose: return)
    pub fn start(
        world: &'w World,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        let ctx = LocaleContext::build(world).map_err(VesselError::Locale)?;
        let agent = mint_flagship(world, &ctx)?;
        let mut session = Session {
            world,
            ctx,
            agent,
            knowledge: Knowledge::default(),
            trail: Vec::new(),
            day: opts.day,
            focalizer: TemplateFocalizer,
            projection: IdentityProjection,
        };
        session.absorb_here()?;
        let opening = session.describe_here()?;
        Ok((session, opening))
    }

    /// The possessed agent (read-only).
    pub fn agent(&self) -> &Agent {
        &self.agent
    }

    /// The accumulated knowledge (read-only).
    pub fn knowledge(&self) -> &Knowledge {
        &self.knowledge
    }

    /// The locale context this session walks (for the battery's checks).
    pub fn context(&self) -> &LocaleContext {
        &self.ctx
    }

    /// The current room, focalized (for the battery's checks).
    pub fn focalized(&self) -> Result<Focalized, VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        Ok(self.focalizer.render(&v))
    }

    /// One verb, one response. `Turn::Released` ends the possession.
    /// type-audit: bare-ok(prose: line)
    pub fn handle(&mut self, line: &str) -> Turn {
        let line = line.trim();
        let (verb, rest) = match line.split_once(' ') {
            Some((v, r)) => (v, r.trim()),
            None => (line, ""),
        };
        match verb {
            "" => Turn::Out(String::new()),
            "look" => self.out(self.describe_here()),
            "go" => self.go(rest),
            "examine" => self.examine(rest),
            "back" => self.back(),
            "wait" => self.wait(rest),
            "whoami" => Turn::Out(self.whoami()),
            "knows" => Turn::Out(self.knows()),
            "enter" | "exit" => Turn::Out(
                "The grain of the world resists; that way lies another scale of things."
                    .to_string(),
            ),
            "help" => Turn::Out(HELP.to_string()),
            "release" | "quit" => Turn::Released("You let go.".to_string()),
            other => Turn::Out(format!("No verb '{other}' ('help' lists them).")),
        }
    }

    /// Absorb the current room's projection into knowledge.
    fn absorb_here(&mut self) -> Result<(), VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        self.knowledge
            .absorb(self.projection.project(&v, &self.agent.perception));
        Ok(())
    }

    /// The full room rendering: room id, prose, ways on.
    fn describe_here(&self) -> Result<String, VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        let f = self.focalizer.render(&v);
        let ways: Vec<String> = v
            .locale
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
                _ => None,
            })
            .collect();
        Ok(format!(
            "[room {}, day {}]\n{}\nWays on: {}.",
            v.locale.id,
            self.day.day,
            f.prose,
            ways.join(", ")
        ))
    }

    fn go(&mut self, dir: &str) -> Turn {
        let Some(wanted) = parse_compass(dir) else {
            return Turn::Out(format!("Go where? '{dir}' is no direction I know."));
        };
        let v = match observable(self.world, &self.ctx, &self.agent, self.day) {
            Ok(v) => v,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let exit = v
            .locale
            .exits
            .iter()
            .find(|e| e.kind == ExitKind::Edge && e.direction == Direction::Compass(wanted));
        let Some(exit) = exit else {
            return Turn::Out(format!("No way {} from here.", dir.to_lowercase()));
        };
        // Lateral exits stay at walk depth: the destination is the
        // neighbor whose packed id the exit names.
        let dest = self
            .agent
            .position
            .neighbors()
            .into_iter()
            .find(|n| n.pack().map(|r| r.0) == Ok(exit.to));
        let Some(dest) = dest else {
            return Turn::Out("error: exit names no neighbor".to_string());
        };
        let from = std::mem::replace(&mut self.agent.position, dest);
        self.trail.push(from);
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    fn back(&mut self) -> Turn {
        let Some(prev) = self.trail.pop() else {
            return Turn::Out("You have not walked anywhere yet.".to_string());
        };
        self.agent.position = prev;
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    fn wait(&mut self, arg: &str) -> Turn {
        // Re-parameterize the freeze, never thaw it: nothing steps, the
        // ledger is untouched — only the observation day moves.
        let days: f64 = if arg.is_empty() {
            1.0
        } else {
            match arg.parse() {
                Ok(d) if d > 0.0 => d,
                _ => return Turn::Out(format!("Wait how long? '{arg}' is no span of days.")),
            }
        };
        self.day = WorldTime {
            day: self.day.day + days,
        };
        self.out(self.describe_here())
    }

    fn examine(&self, noun: &str) -> Turn {
        if noun.is_empty() {
            return Turn::Out("Examine what?".to_string());
        }
        let f = match self.focalized() {
            Ok(f) => f,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let wanted = noun.to_lowercase();
        match f.nouns.iter().find(|(n, _)| n.to_lowercase() == wanted) {
            Some((_, detail)) => Turn::Out(detail.clone()),
            None => Turn::Out(format!("You see no {noun} here.")),
        }
    }

    fn whoami(&self) -> String {
        format!(
            "A {} of {} (agent {}), day {}, room {}.",
            self.agent.species,
            self.agent.village.name,
            self.agent.id.0,
            self.day.day,
            self.agent
                .position
                .pack()
                .map(|r| r.0.to_string())
                .unwrap_or_else(|_| "?".to_string()),
        )
    }

    fn knows(&self) -> String {
        let mut lines = vec![format!("{} things seen:", self.knowledge.0.len())];
        for (key, value) in &self.knowledge.0 {
            // char-safe truncation: byte slicing can split a UTF-8 boundary.
            let shown: String = if value.chars().count() > 60 {
                let head: String = value.chars().take(57).collect();
                format!("{head}…")
            } else {
                value.clone()
            };
            lines.push(format!("  {key} = {shown}"));
        }
        lines.join("\n")
    }

    fn out(&self, r: Result<String, VesselError>) -> Turn {
        match r {
            Ok(s) => Turn::Out(s),
            Err(e) => Turn::Out(format!("error: {e}")),
        }
    }
}

/// Parse a compass token (case-insensitive, long names allowed).
fn parse_compass(s: &str) -> Option<Compass> {
    match s.to_lowercase().as_str() {
        "n" | "north" => Some(Compass::N),
        "ne" | "northeast" => Some(Compass::Ne),
        "e" | "east" => Some(Compass::E),
        "se" | "southeast" => Some(Compass::Se),
        "s" | "south" => Some(Compass::S),
        "sw" | "southwest" => Some(Compass::Sw),
        "w" | "west" => Some(Compass::W),
        "nw" | "northwest" => Some(Compass::Nw),
        _ => None,
    }
}
