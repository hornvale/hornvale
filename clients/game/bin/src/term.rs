//! Terminal setup, teardown, and drawing a [`Grid`].
//!
//! **Restored on drop, on panic, AND on an external SIGINT/SIGTERM/SIGHUP.**
//! A client that leaves the user's terminal in raw mode after a crash — or
//! after a `kill` from another terminal — is a bug the user pays for, not
//! the program. [`Term::open`] therefore covers three exit paths: the
//! ordinary [`Drop`] impl (the ordinary case), a panic hook that restores
//! before the default hook prints, and [`watch_signals`], a background
//! thread that restores and exits when the process receives SIGINT, SIGTERM,
//! or SIGHUP from *outside* the keyboard.
//!
//! **Why "outside the keyboard" is the operative phrase.** Raw mode clears
//! `ISIG`, so a keyboard Ctrl-C never becomes a `SIGINT` at all while this
//! terminal is open — it arrives as an ordinary byte, which `input.rs`
//! already treats as an unmapped key (inert, costs no turn). The real path
//! this module defends is a player who does not know `Q` releases and
//! reaches for `kill`/Ctrl-C *from a different terminal*, or whose
//! controlling terminal simply closes (SIGHUP). Confirmed with a pty
//! harness sending a real `SIGINT` from outside — see the task report.

use crossterm::cursor::{Hide, MoveTo, SetCursorStyle, Show};
use crossterm::execute;
use crossterm::queue;
use crossterm::style::{Attribute, Print, SetAttribute};
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use hornvale_game_core::{Grid, Weight};
use std::io::{self, Write};
use std::sync::atomic::{AtomicBool, Ordering};

/// Whether a [`Term`] is currently open. A `static`, not a field on `Term`
/// itself, because the hazard this guards against is two *instances*
/// existing at once (each independently entering/restoring the one real
/// terminal) — a zero-sized `Term` carries no state a second instance could
/// be told apart from, so the only way to make "at most one" true is to
/// track it outside any instance. [`Term::open`] refuses to open a second
/// terminal while one is already live, rather than silently layering a
/// second raw-mode/alternate-screen/panic-hook stack that would fight the
/// first on drop.
static OPEN: AtomicBool = AtomicBool::new(false);

/// An open terminal: raw mode, the alternate screen, and the cursor hidden.
/// Dropping it (or a panic, or an external SIGINT/SIGTERM/SIGHUP, unwinding
/// or exiting through it) restores all three. At most one may be open at a
/// time — see [`OPEN`].
pub struct Term;

impl Term {
    /// Enter raw mode and the alternate screen, show the cursor styled as a
    /// blinking underscore, install a panic hook that restores the terminal
    /// before the previous hook (Rust's default backtrace printer,
    /// ordinarily) runs, and start [`watch_signals`]. Fails if a [`Term`]
    /// is already open.
    ///
    /// **The cursor is shown, not hidden, at setup.** The Portolan gives the
    /// terminal's own hardware cursor a job: it reports the free-roaming
    /// look-mode cursor's position (see [`Grid`]'s crate,
    /// `hornvale_game_core::Cursor`) by moving the *real* cursor there
    /// rather than drawing ink onto the grid. [`Term::draw`] hides it again
    /// on any redraw that has no position to report.
    pub fn open() -> io::Result<Term> {
        if OPEN.swap(true, Ordering::SeqCst) {
            return Err(io::Error::other(
                "a Term is already open; at most one may be open at a time",
            ));
        }
        enable_raw_mode()?;
        execute!(
            io::stdout(),
            EnterAlternateScreen,
            Show,
            SetCursorStyle::BlinkingUnderScore
        )?;
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let _ = Term::restore();
            previous(info);
        }));
        watch_signals();
        Ok(Term)
    }

    /// Undo `open`'s terminal changes. Idempotent and infallible-in-effect
    /// (errors are swallowed by every caller): called from ordinary
    /// [`Drop`], from the panic hook, and from [`watch_signals`]'s
    /// background thread — all three must be able to run it without a
    /// second panic during unwind or a signal-handling deadlock.
    fn restore() -> io::Result<()> {
        OPEN.store(false, Ordering::SeqCst);
        disable_raw_mode()?;
        execute!(io::stdout(), Show, LeaveAlternateScreen)?;
        Ok(())
    }

    /// Draw `grid` to the alternate screen from the top-left, honouring
    /// [`Weight`]: `Bold` sets the bold attribute, `Dim` sets dim, `Normal`
    /// resets — a single buffered write per redraw, flushed once at the end.
    ///
    /// `cursor` is the screen position `hornvale_game_core::render_with`
    /// reported (never a grid cell's ink — see [`Term::open`]'s doc): when
    /// `Some`, the real terminal cursor is shown and moved there after
    /// painting; when `None`, it is hidden, parked out of the way of the
    /// freshly painted page.
    pub fn draw(&self, grid: &Grid, cursor: Option<(u16, u16)>) -> io::Result<()> {
        let mut out = io::stdout();
        queue!(out, MoveTo(0, 0), SetAttribute(Attribute::Reset))?;
        let mut current = Weight::Normal;
        for y in 0..grid.height() {
            queue!(out, MoveTo(0, y))?;
            for x in 0..grid.width() {
                let cell = grid.get(x, y);
                let weight = cell.map(|c| c.weight).unwrap_or_default();
                if weight != current {
                    let attr = match weight {
                        Weight::Bold => Attribute::Bold,
                        Weight::Normal => Attribute::Reset,
                        Weight::Dim => Attribute::Dim,
                    };
                    queue!(out, SetAttribute(attr))?;
                    current = weight;
                }
                let ch = cell.and_then(|c| c.glyph).unwrap_or(' ');
                queue!(out, Print(ch))?;
            }
        }
        queue!(out, SetAttribute(Attribute::Reset))?;
        match cursor {
            Some((x, y)) => queue!(out, Show, MoveTo(x, y))?,
            None => queue!(out, Hide)?,
        }
        out.flush()
    }

    /// Draw a plain diagnostic line at the top-left — the fallback when
    /// [`hornvale_game_core::render`] itself refuses a snapshot (should not
    /// happen in practice: the driver always clamps the requested grid to
    /// the monochrome floor and always emits a schema-conformant document),
    /// so the user sees why the screen is blank rather than a silent hang.
    pub fn draw_text(&self, text: &str) -> io::Result<()> {
        let mut out = io::stdout();
        queue!(
            out,
            MoveTo(0, 0),
            SetAttribute(Attribute::Reset),
            Print(text)
        )?;
        out.flush()
    }
}

impl Drop for Term {
    fn drop(&mut self) {
        let _ = Term::restore();
    }
}

/// Watch SIGINT/SIGTERM/SIGHUP on a background thread and restore the
/// terminal, then exit, when one arrives.
///
/// Without this, Rust's default disposition for these three signals —
/// terminate the process immediately, no unwind, no [`Drop`] — leaves the
/// terminal in raw mode on the alternate screen. See the module doc for why
/// this is a real path (an unfamiliar player reaching for `kill`) rather
/// than a keyboard one (raw mode already clears `ISIG`, so a keyboard
/// Ctrl-C is inert, not a signal).
///
/// Registration itself failing (an exotic resource-limit case) leaves the
/// game running without this safety net rather than failing `Term::open`
/// over it — the ordinary [`Drop`] and panic-hook paths are unaffected.
///
/// Installed at most once per process (guarded by [`OPEN`]'s
/// compare-and-swap in `open`, which already refuses a second concurrent
/// `Term`), using `signal-hook`'s self-pipe-backed
/// [`signal_hook::iterator::Signals`] rather than a hand-rolled `extern "C"`
/// handler: the actual OS-level handler it registers is the minimal,
/// async-signal-safe piece (writing one byte to a pipe); everything this
/// function does in response — [`Term::restore`], [`std::process::exit`] —
/// runs on an ordinary thread woken by reading that pipe, never inside
/// real signal-handler context, so none of it needs to be async-signal-safe
/// itself.
fn watch_signals() {
    let mut signals = match signal_hook::iterator::Signals::new([
        signal_hook::consts::SIGINT,
        signal_hook::consts::SIGTERM,
        signal_hook::consts::SIGHUP,
    ]) {
        Ok(signals) => signals,
        Err(_) => return,
    };
    std::thread::spawn(move || {
        if let Some(sig) = signals.forever().next() {
            let _ = Term::restore();
            std::process::exit(128 + sig);
        }
    });
}
