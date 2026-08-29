//! `hornvale-game`: the terminal binary. Parses `--seed`/`--target`, opens
//! the terminal, and drives [`hornvale_game::driver::Driver`] and
//! [`hornvale_game::input`] in a loop until release.
//!
//! Argument parsing is hand-rolled (std-only) rather than reaching for
//! `clap`, matching `cli/`'s own convention even though `clients/` is not
//! bound by the workspace's no-new-crates rule (decision 0055) — one new
//! dependency (crossterm) is enough for this campaign.

use hornvale_game::driver::Driver;
use hornvale_game::overture::genesis::{self, Gesture, Keys, Screen};
use hornvale_game::overture::{AlmanacView, AtlasView, Frame, SkyView, TongueView, View};
use hornvale_game::{boot, input, term};
use hornvale_game_core::{CommandLine, MIN_HEIGHT, MIN_WIDTH};
use hornvale_kernel::Seed;
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

    // The terminal opens BEFORE genesis (The Overture, Task 1): every
    // later view needs the screen live from the first frame, not blank
    // until genesis finishes. That inversion means a genesis or possession
    // failure below now happens with the terminal already in raw mode on
    // the alternate screen, so `boot::start_and_report` explicitly
    // restores it before this function's `?` ever hands the error back to
    // `main`'s `eprintln!` — see that module's doc for why this is a
    // tested seam rather than a hope resting on `Term`'s `Drop` backstop.
    let term = term::Term::open().map_err(|e| e.to_string())?;

    // THE OVERTURE (Task 3, ruling R5). Genesis runs on a worker thread and
    // this thread draws around it — see `overture::genesis`'s module doc for
    // why a thread, what the observer clones and what that costs (measured:
    // 21.9-24.5 ms, under 1% of the build), and which phase cannot move off
    // this thread and why. Both fallible steps go through
    // `boot::start_and_report`, so the restore-before-report ordering Task 1
    // established covers the frame's own failures too, not just the driver's.
    let mut frame = Frame::new(views()).titled(format!("seed {seed}"));
    let mut driver = boot::start_and_report(&term, || -> Result<Driver, String> {
        let progress = genesis::spawn(Seed(seed));
        let world = genesis::run(
            &mut frame,
            &progress,
            &TermScreen(&term),
            &mut CrosstermKeys,
        )
        .map_err(|e| e.to_string())?;
        // `start_from_world`, not `start`: the world the worker just spent
        // ~2.2 s building is handed straight over rather than built again.
        let driver = Driver::start_from_world(world, target)
            .map_err(|e: hornvale_game::driver::DriverError| e.to_string())?;
        // The post-genesis phase reports through no rung, so its own owner
        // closes it (see `Frame::finish_phase`). Closing it HERE, after
        // `start_from_world` returns, is what makes `living` measurable at all.
        frame.finish_phase(genesis::POST_GENESIS_PHASE);
        Ok(driver)
    })?;
    // Best effort by design: a missing or unwritable state directory must cost
    // the next run its bar and nothing else (see `state_dir`'s module doc).
    let _ = frame.save_timings();

    let outcome = play(&mut driver, &term);
    // Explicit drop before reporting any error: whatever `play` returns, the
    // user's terminal must be sane before they read the message.
    drop(term);
    outcome.map_err(|e| e.to_string())
}

/// The views the overture cycles, in cycle order.
///
/// **`sky`, `atlas`, `almanac`, then `tongue` — one line added per task, and
/// nothing else changed.** Task 3 settles the `View` contract; Tasks 4-7
/// write the four views (`sky`, `atlas`, `almanac`, `tongue`) and each adds
/// one line here. `sky` speaks from the first rung; `atlas` (Task 5) cannot
/// speak until terrain exists (`BuildDepth::Terrain`); `almanac` (Task 6)
/// speaks from the first rung too (its own `OrbitComponent` needs nothing
/// deeper), and grows its own content internally as later rungs land,
/// through the component registry rather than through anything visible
/// here. `tongue` (Task 7) speaks only once the ledger has actually
/// committed a settlement's species — `BuildDepth::Full`, the ladder's last
/// rung, not `Settlements` as originally sketched (see `overture::tongue`'s
/// module doc for why). The frame skips what cannot speak, so a roster of
/// four behaves correctly from the first rung the same way a roster of two
/// did (`hornvale_game::overture`).
fn views() -> Vec<Box<dyn View>> {
    vec![
        Box::new(SkyView),
        Box::new(AtlasView::default()),
        Box::new(AlmanacView),
        Box::new(TongueView),
    ]
}

/// [`Screen`] over the real terminal.
///
/// A newtype rather than an `impl` on `Term` itself, because the trait belongs
/// to the overture and `term.rs` should not have to know it exists — the same
/// direction of dependency `boot::TermHandle` takes.
struct TermScreen<'a>(&'a term::Term);

impl Screen for TermScreen<'_> {
    fn size(&self) -> std::io::Result<(u16, u16)> {
        terminal_size()
    }

    fn draw(&self, grid: &hornvale_game_core::Grid) -> std::io::Result<()> {
        // No cursor during the overture: there is nothing to type at yet, and a
        // parked block cursor on a chrome cell reads as an input prompt.
        self.0.draw(grid, None)
    }
}

/// [`Keys`] over crossterm's event queue.
///
/// Maps only what the overture understands. `space` cycles; a resize is
/// forwarded so the frame can re-lay its chrome; everything else is
/// [`Gesture::Ignored`], because the world does not exist yet and there is
/// nothing else to ask it. Release is deliberately NOT handled here: the
/// signal handler (`term::watch_signals`) already owns interruption, and a
/// half-built world has no session to release.
struct CrosstermKeys;

impl Keys for CrosstermKeys {
    fn next(&mut self, timeout: std::time::Duration) -> std::io::Result<Option<Gesture>> {
        use crossterm::event::{Event, KeyCode, KeyEventKind, poll, read};
        if !poll(timeout)? {
            return Ok(None);
        }
        Ok(Some(match read()? {
            // `Press` only: a terminal reporting key-release events would
            // otherwise cycle twice per keystroke.
            Event::Key(key) if key.kind == KeyEventKind::Press => match key.code {
                KeyCode::Char(' ') => Gesture::Cycle,
                _ => Gesture::Ignored,
            },
            Event::Resize(_, _) => Gesture::Resize,
            _ => Gesture::Ignored,
        }))
    }
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
/// Reads `driver.snapshot()`/`focus()`/`cursor()`/`strip_text()`/
/// `strip_offset()`/`line_text()`/`caret()`/`echo()` fresh each call rather
/// than being handed them, so every call site redraws the driver's true
/// current state rather than whatever it happened to return from the
/// action that triggered the redraw (`Event::Resize` has no action at
/// all). **This function IS the redraw F3 drives `strip_offset` from**:
/// `Driver::refresh_strip` (called from every action that could change
/// what the strip shows) advances the strip's own scroll counter, and this
/// function reads it back out here, fresh — no timer anywhere in the loop.
/// The command
/// buffer moved into `Driver` itself with Task 3 (The Stylus) — it used to
/// be a separate `bin::line::Line` this loop owned alongside the driver,
/// but `Driver::apply` now needs to mutate it directly to answer `Submit`,
/// so `Driver` owns it and this function reads it back through the small
/// accessors rather than threading a second mutable buffer through the
/// loop.
fn redraw(term: &term::Term, driver: &mut Driver) -> std::io::Result<()> {
    let (w, h) = terminal_size()?;
    let json = driver.snapshot();
    let text = driver.line_text();
    let cmd_line = CommandLine {
        text: &text,
        caret: driver.caret(),
    };
    // Rebuild the renderer's borrowed `Hint` view from the driver's owned
    // pair; both bindings live to the end of this function, so the borrows
    // outlive the `render_with` call below.
    let hint_parts = driver.hint_parts();
    let matches: Vec<&str> = hint_parts
        .as_ref()
        .map(|(_, m)| m.iter().map(String::as_str).collect())
        .unwrap_or_default();
    let hint = hint_parts
        .as_ref()
        .map(|(s, _)| hornvale_game_core::entry::Hint {
            stem: s.as_str(),
            matches: &matches,
        });
    // The Portolan part II, Task 3a: the world view's activation is
    // `Driver`'s own decision, not re-derived here — `Driver::
    // world_plate_for_redraw`'s doc explains why (fix round 1: an earlier
    // revision gated this on `Focus::Map` alone, which silently retired the
    // walk-band cursor/strip feature that ALSO lives behind `Focus::Map`).
    // `Focus::Map`'s own door is still just submitting a bare `map`
    // (`Driver::enter_map`); nothing here adds a new verb, and nothing here
    // can yet turn the world view itself on — Task 3b owns that gesture.
    let world_plate = driver.world_plate_for_redraw(w, h);
    match hornvale_game_core::render_with(
        &json,
        w,
        h,
        driver.focus(),
        driver.cursor(),
        cmd_line,
        driver.strip_text(),
        driver.echo(),
        world_plate.as_ref(),
        driver.strip_offset(),
        hint.as_ref(),
    ) {
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
/// **Release now reads the driver's own answer.** Before this campaign,
/// `release` was detected by matching the sent verb line
/// (`Action::Verb("release")`) — a check that could only ever see the
/// `"release"` spelling, even though the sim also honours `"quit"` (ledger
/// #8). `Driver::apply` now returns whether the possession RELEASED, so
/// this loop asks the driver directly rather than re-deriving the answer
/// from what it happened to send. The final `redraw` still runs before the
/// loop returns, so the player sees the sim's own parting line before the
/// terminal is restored (`main`'s `run` explicitly drops the terminal only
/// after `play` returns).
///
/// `driver.resize` is called here (startup) and on every `Event::Resize` —
/// never on a plain key press, since a terminal's size does not change
/// between resize events, and `resize` itself is cheap (one subtraction
/// plus a cursor re-clamp).
/// How long one marquee column lasts. Authored, not derived: fast enough to
/// read as motion, slow enough to read as text. The ONLY wall-clock value in
/// this client, and it is a render cadence rather than world time — the sim's
/// ban on `Instant` (decision 0001) is about `WorldTime`, and does not reach
/// `clients/game` (its own workspace, outside the determinism boundary,
/// decision 0055).
const MARQUEE_TICK: std::time::Duration = std::time::Duration::from_millis(300);

fn play(driver: &mut Driver, term: &term::Term) -> std::io::Result<()> {
    use crossterm::event::{Event, poll, read};

    let (w, h) = terminal_size()?;
    driver.resize(w, h);
    redraw(term, &mut *driver)?;
    loop {
        // BLOCK unless the strip actually has somewhere to scroll. An idle
        // client whose strip fits its plate wakes for nothing; only a
        // genuinely overflowing strip costs a timed poll.
        if driver.strip_is_scrolling() && !poll(MARQUEE_TICK)? {
            driver.tick_marquee();
            redraw(term, &mut *driver)?;
            continue;
        }
        match read()? {
            Event::Key(key) => {
                let action = input::action_for(key, driver.focus());
                if matches!(action, input::Action::None) {
                    continue;
                }
                let released = driver.apply(action);
                redraw(term, &mut *driver)?;
                if released {
                    return Ok(());
                }
            }
            Event::Resize(_, _) => {
                let (w, h) = terminal_size()?;
                driver.resize(w, h);
                redraw(term, &mut *driver)?;
            }
            _ => {}
        }
    }
}
