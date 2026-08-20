//! Recalled command lines.
//!
//! A plain `Vec<String>` in submission order — never a map, matching the
//! workspace-wide `HashMap`/`HashSet` ban this crate follows by convention
//! even though it sits outside the cargo workspace (see `clients/CLAUDE.md`:
//! the workspace rules do not bind here, but there is no reason to reach for
//! a structure this crate's sibling code never uses).
//!
//! **The walk position is an index counted from the END of `entries`**, not
//! a plain array index: `None` means "not currently walking" (the state
//! immediately after construction, and immediately after every [`History::
//! push`] or [`History::reset`]), and `Some(n)` for `n >= 1` means "`n`
//! entries back from the most recent", so `Some(1)` names the newest entry
//! and `Some(entries.len())` names the oldest. Counting from the end rather
//! than from the start means the walk position stays meaningful across a
//! `push` without needing to shift — though in practice `push` resets it
//! outright (see below), so that property is not currently exercised, only
//! kept simple.
//!
//! **`push` resets the walk.** Otherwise a second `Up` after submitting a
//! command would resume wherever the previous walk left off, rather than
//! starting fresh from the newest entry — surprising, since the player just
//! added a newest entry.

/// Recalled command lines, walked with [`History::prev`]/[`History::next`].
#[derive(Debug, Default)]
pub struct History {
    /// Submitted lines, oldest first.
    entries: Vec<String>,
    /// The walk position, counted back from the newest entry — see the
    /// module doc. `None` means the walk has not started (or was reset).
    pos: Option<usize>,
}

impl History {
    /// An empty history, walk not started.
    pub fn new() -> History {
        History::default()
    }

    /// Record a submitted line and reset the walk — see the module doc for
    /// why a fresh submission must not leave the walk wherever it was.
    pub fn push(&mut self, line: String) {
        self.entries.push(line);
        self.pos = None;
    }

    /// Walk one step further into the past. Starts at the newest entry;
    /// repeated calls step backward and stop at the oldest rather than
    /// wrapping around to the newest again. `None` only when there is
    /// nothing recorded at all.
    pub fn prev(&mut self) -> Option<&str> {
        if self.entries.is_empty() {
            return None;
        }
        let next_pos = match self.pos {
            None => 1,
            Some(p) => (p + 1).min(self.entries.len()),
        };
        self.pos = Some(next_pos);
        let idx = self.entries.len() - next_pos;
        self.entries.get(idx).map(String::as_str)
    }

    /// Walk one step back toward the present. Past the newest entry this
    /// returns `None` and the walk stops (a caller reads that as "the empty
    /// buffer", per the buffer's own `set`/`take` contract) — it does not
    /// wrap and does not recall anything if the walk was never started.
    // `next`/`prev` is the interface the task brief specifies verbatim
    // (mirroring the shell/readline convention for history recall), not an
    // attempt at `Iterator` — `History` has no useful `Item` and no `next`
    // that returns `None` permanently once exhausted the way `Iterator`'s
    // contract expects, so implementing the trait here would be the wrong
    // fit dressed up as the right name.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&str> {
        match self.pos {
            None => None,
            Some(1) => {
                self.pos = None;
                None
            }
            Some(p) => {
                let next_pos = p - 1;
                self.pos = Some(next_pos);
                let idx = self.entries.len() - next_pos;
                self.entries.get(idx).map(String::as_str)
            }
        }
    }

    /// Restart the walk from the newest entry, without touching what has
    /// been recorded.
    pub fn reset(&mut self) {
        self.pos = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prev_walks_backwards_from_the_most_recent() {
        let mut h = History::new();
        h.push("look".to_string());
        h.push("go n".to_string());
        assert_eq!(h.prev(), Some("go n"));
        assert_eq!(h.prev(), Some("look"));
    }

    #[test]
    fn prev_stops_at_the_oldest_rather_than_wrapping() {
        let mut h = History::new();
        h.push("look".to_string());
        assert_eq!(h.prev(), Some("look"));
        assert_eq!(
            h.prev(),
            Some("look"),
            "walking past the oldest must not wrap"
        );
    }

    #[test]
    fn next_walks_forwards_and_returns_none_past_the_newest() {
        let mut h = History::new();
        h.push("look".to_string());
        h.push("go n".to_string());
        h.prev();
        h.prev();
        assert_eq!(h.next(), Some("go n"));
        assert_eq!(h.next(), None, "past the newest is the empty buffer");
    }

    #[test]
    fn an_empty_history_recalls_nothing() {
        let mut h = History::new();
        assert_eq!(h.prev(), None);
        assert_eq!(h.next(), None);
    }

    /// A fresh submission restarts the walk — otherwise the second Up after
    /// a command would resume from wherever the previous walk stopped.
    #[test]
    fn pushing_resets_the_walk() {
        let mut h = History::new();
        h.push("look".to_string());
        h.push("go n".to_string());
        h.prev();
        h.prev();
        h.push("wait".to_string());
        assert_eq!(h.prev(), Some("wait"));
    }
}
