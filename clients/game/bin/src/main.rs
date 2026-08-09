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

const USAGE: &str = "usage: hornvale-game --seed <N> [--target flagship|first-settlement]";

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
        Some("first-settlement") => Ok(PossessTarget::FirstSettlement),
        Some(other) => Err(format!(
            "--target: unknown target '{other}'; known targets: flagship, first-settlement"
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

/// Draw `json` at the terminal's current size, clamped up to the monochrome
/// floor `hornvale-game-core` refuses to render below.
fn redraw(term: &term::Term, json: &str) -> std::io::Result<()> {
    let (cols, rows) = crossterm::terminal::size()?;
    let w = cols.max(MIN_WIDTH);
    let h = rows.max(MIN_HEIGHT);
    match hornvale_game_core::render(json, w, h) {
        Ok(grid) => term.draw(&grid),
        Err(e) => term.draw_text(&format!("render error: {e}")),
    }
}

/// The main loop: draw the opening, then read one key at a time. A mapped
/// key is sent to the driver unconditionally (the client never validates —
/// see `input`'s module doc) and the reply is redrawn; an unmapped key does
/// nothing at all, costing no turn and no redraw. `release` (bound to
/// capital `Q`) ends the loop after its own reply is drawn, so the user sees
/// the sim's own parting line before the terminal is restored.
fn play(driver: &mut Driver, term: &term::Term) -> std::io::Result<()> {
    use crossterm::event::{Event, read};

    redraw(term, &driver.snapshot())?;
    loop {
        match read()? {
            Event::Key(key) => {
                let Some(verb) = input::verb_for(key) else {
                    continue;
                };
                // Detected by matching the SENT verb, not the sim's answer —
                // correct today only because `input::verb_for` is the sole
                // source of outgoing verbs and its only release-shaped line
                // is the literal string `"release"` (it never emits
                // `"quit"`, the sim's other synonym for the same thing). If
                // a future free-text input mode lets a player type `quit`
                // directly, this check needs to grow with it or move to
                // reading the driver's answer instead.
                let released = verb == "release";
                let json = driver.handle(&verb);
                redraw(term, &json)?;
                if released {
                    return Ok(());
                }
            }
            Event::Resize(_, _) => redraw(term, &driver.snapshot())?,
            _ => {}
        }
    }
}
