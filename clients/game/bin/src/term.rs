//! Terminal setup, teardown, and drawing a [`Grid`].
//!
//! **Restored on drop AND on panic.** A client that leaves the user's
//! terminal in raw mode after a crash is a bug the user pays for, not the
//! program — so [`Term::open`] installs a panic hook that restores the
//! terminal before the default hook prints, in addition to the ordinary
//! [`Drop`] impl covering the non-panicking exit.

use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::execute;
use crossterm::queue;
use crossterm::style::{Attribute, Print, SetAttribute};
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use hornvale_game_core::{Grid, Weight};
use std::io::{self, Write};

/// An open terminal: raw mode, the alternate screen, and the cursor hidden.
/// Dropping it (or a panic unwinding through it) restores all three.
pub struct Term;

impl Term {
    /// Enter raw mode and the alternate screen, hide the cursor, and install
    /// a panic hook that restores the terminal before the previous hook
    /// (Rust's default backtrace printer, ordinarily) runs — so a panic's
    /// message lands on a normal, scrollable screen instead of a raw-mode
    /// alternate one the shell can neither read nor exit cleanly from.
    pub fn open() -> io::Result<Term> {
        enable_raw_mode()?;
        execute!(io::stdout(), EnterAlternateScreen, Hide)?;
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let _ = Term::restore();
            previous(info);
        }));
        Ok(Term)
    }

    /// Undo `open`'s terminal changes. Idempotent and infallible-in-effect
    /// (errors are swallowed by both callers): called from ordinary [`Drop`]
    /// and, separately, from the panic hook — both must be able to run it
    /// without a second panic during unwind.
    fn restore() -> io::Result<()> {
        disable_raw_mode()?;
        execute!(io::stdout(), Show, LeaveAlternateScreen)?;
        Ok(())
    }

    /// Draw `grid` to the alternate screen from the top-left, honouring
    /// [`Weight`]: `Bold` sets the bold attribute, `Dim` sets dim, `Normal`
    /// resets — a single buffered write per redraw, flushed once at the end.
    pub fn draw(&self, grid: &Grid) -> io::Result<()> {
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
