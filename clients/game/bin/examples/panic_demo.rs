//! Diagnostic-only, not shipped as part of the game: open the terminal and
//! panic immediately, so an external harness (a pty, since `enable_raw_mode`
//! needs a real terminal) can observe that `Term`'s panic hook restores the
//! terminal *before* the process exits — the other half of "restored on
//! drop AND on panic" that `tests/driver.rs` cannot exercise (it has no
//! terminal to restore).
//!
//! Run under a pty and check the terminal's `termios` state after the
//! process exits; see `.superpowers/sdd/2026-08-08-the-quire/task-10-
//! report.md` for the harness and its output.

fn main() {
    let _term = hornvale_game::term::Term::open().expect("open a real terminal");
    panic!("intentional panic for the terminal-restore demonstration");
}
