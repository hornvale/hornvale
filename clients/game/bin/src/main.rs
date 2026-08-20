//! `hornvale-game`: the terminal binary. Parses `--seed`/`--target`, opens
//! the terminal, and drives [`hornvale_game::driver::Driver`] and
//! [`hornvale_game::input`] in a loop until release.
//!
//! Argument parsing is hand-rolled (std-only) rather than reaching for
//! `clap`, matching `cli/`'s own convention even though `clients/` is not
//! bound by the workspace's no-new-crates rule (decision 0055) — one new
//! dependency (crossterm) is enough for this campaign.

use hornvale_game::driver::Driver;
use hornvale_game::{input, term};
use hornvale_game_core::{MIN_HEIGHT, MIN_WIDTH};
use hornvale_vessel::PossessTarget;

const USAGE: &str = "usage: hornvale-game --seed <N> [--target flagship|most-populous-settlement]";

fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

/// Parse `--target`, defaulting to [`PossessTarget::Flagship`]. Fails loudly
/// on an unknown value, matching `cli/src/main.rs`'s `cmd_possess`.
fn parse_target(args: &[String]) -> Result<PossessTarget, String> {
    match flag_value(args, "--target") {
        None | Some("flagship") => Ok(PossessTarget::Flagship),
        Some("most-populous-settlement") => Ok(PossessTarget::MostPopulousSettlement),
        Some(other) => Err(format!(
            "--target: unknown target '{other}'; known targets: flagship, \
             most-populous-settlement"
        )),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let Err(e) = run(&args) {
        eprintln!("error: {e}\n{USAGE}");
        std::process::exit(1);
    }
}

fn run(args: &[String]) -> Result<(), String> {
    let seed: u64 = flag_value(args, "--seed")
        .ok_or("--seed <N> is required")?
        .parse()
        .map_err(|e| format!("--seed must be a u64: {e}"))?;
    let target = parse_target(args)?;

    let mut driver = Driver::start(seed, target).map_err(|e| e.to_string())?;

    // The terminal is opened only once genesis and the possession have
    // already succeeded — a failure above prints a normal error to a normal
    // shell rather than needing the raw-mode screen restored first.
    let term = term::Term::open().map_err(|e| e.to_string())?;
    let outcome = play(&mut driver, &term);
    // Explicit drop before reporting any error: whatever `play` returns, the
    // user's terminal must be sane before they read the message.
    drop(term);
    outcome.map_err(|e| e.to_string())
}

/// The terminal's current size, clamped up to the monochrome floor
/// `hornvale-game-core` refuses to render below. The single place both
/// `redraw` and `play`'s driver-resize calls read this from, so the height
/// the driver resolves cursor positions against is always the SAME height
/// `render_with`/`compose` draw the plate at — never a second, possibly
/// stale copy of "what size is the terminal" (fix round 2: a fixed
/// floor-height constant used to stand in for this and silently diverged
/// from the real plate on any terminal taller than 24 rows).
fn terminal_size() -> std::io::Result<(u16, u16)> {
    let (cols, rows) = crossterm::terminal::size()?;
    Ok((cols.max(MIN_WIDTH), rows.max(MIN_HEIGHT)))
}

/// Draw the driver's current state at the terminal's current size.
///
/// Reads `driver.snapshot()`/`cursor()`/`strip_text()` fresh each call
/// rather than being handed them, so every call site redraws the driver's
/// true current state rather than whatever it happened to return from the
/// action that triggered the redraw (`Event::Resize` has no action at all).
fn redraw(term: &term::Term, driver: &Driver) -> std::io::Result<()> {
    let (w, h) = terminal_size()?;
    let json = driver.snapshot();
    match hornvale_game_core::render_with(&json, w, h, driver.cursor(), driver.strip_text()) {
        Ok((grid, cursor)) => term.draw(&grid, cursor),
        Err(e) => term.draw_text(&format!("render error: {e}")),
    }
}

/// The main loop: sync the driver's plate height, draw the opening, then
/// read one key at a time. Each key is mapped to an [`input::Action`] under
/// the driver's current [`hornvale_game_core::Focus`] — `Action::None` does
/// nothing at all, costing no turn and no redraw; every other action is
/// applied and its reply redrawn.
///
/// **Release detection is not wired here yet.** Before this campaign,
/// `release` was detected by matching the sent verb line
/// (`Action::Verb("release")`) and ending the loop so the user could see
/// the sim's own parting line before the terminal was restored. `Action` no
/// longer carries a verb line at all — a keypress now reaches the line
/// buffer, not the driver, until it is submitted (Task 3's job) — so there
/// is nothing here yet for a release check to match against. Task 3 must
/// reintroduce this once `Action::Submit` actually sends the buffer's text.
///
/// `driver.resize` is called here (startup) and on every `Event::Resize` —
/// never on a plain key press, since a terminal's size does not change
/// between resize events, and `resize` itself is cheap (one subtraction
/// plus a cursor re-clamp).
fn play(driver: &mut Driver, term: &term::Term) -> std::io::Result<()> {
    use crossterm::event::{Event, read};

    let (_, h) = terminal_size()?;
    driver.resize(h);
    redraw(term, driver)?;
    loop {
        match read()? {
            Event::Key(key) => {
                let action = input::action_for(key, driver.focus());
                if matches!(action, input::Action::None) {
                    continue;
                }
                driver.apply(action);
                redraw(term, driver)?;
            }
            Event::Resize(_, _) => {
                let (_, h) = terminal_size()?;
                driver.resize(h);
                redraw(term, driver)?;
            }
            _ => {}
        }
    }
}
