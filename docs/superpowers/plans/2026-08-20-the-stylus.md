# The Stylus Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The terminal client accepts typed commands — a real command line
with a buffer, editing, history and `Enter` — with focus, toggled by `Esc`,
as the single concept that decides what any key means.

**Architecture:** Entirely client-side, in `clients/game`. `bin` owns the
command buffer and the focus state; `core` renders them and computes where
the one hardware cursor goes, because only `core` knows the layout. The sim
is untouched: `Driver::handle(&str)` already forwards to `Session::handle`.
Part I's `Mode { Normal, Look }` is replaced in place by `Focus { Cli, Map }`
— the same function, a different question.

**Tech Stack:** Rust (edition 2024), crossterm 0.29, two crates outside the
cargo workspace: `hornvale-game-core` (no hornvale dependency at all) and
`hornvale-game` (links `hornvale-vessel`, in `bin/src/driver.rs` only).

**Spec:** `docs/superpowers/specs/2026-08-20-the-stylus-design.md` — read it
first, especially §2 (the routing table), §4 (what this supersedes) and §7
(what is unverified). The plan argues from the spec; both travel together.

## Global Constraints

- **No modifier keys anywhere.** Every binding is a plain key. The existing
  chord discipline stays: any `KeyModifiers` beyond a bare `SHIFT` maps to
  nothing, before the key code is inspected. `Ctrl-L` must never walk east.
- **`Tab` is bound to nothing** and must stay that way (spec §3.3). It is
  reserved for completion, which noun matching makes valuable: `Noun::matches`
  in `windows/vessel/src/focalize.rs` is `self.words.contains(&w)` — exact
  word, not prefix, so a player must type `Vngashngatva` exactly.
- **No wall-clock time.** `Instant` is banned workspace-wide, including in
  tests. `Esc` costs no timer — crossterm resolves a lone `0x1B` by asking
  whether input is already available (spec §3.1).
- **No schema change and no sim change.** Nothing in `kernel/`, `domains/`
  or `windows/` is edited. If a task seems to need `Snapshot` to grow a
  field, that is a STOP (spec §7 F3).
- **The 80×24 floor is inherited and may not be weakened.** `render_with`
  refuses anything smaller, strip present or not.
- **`hornvale-game-core` must never gain a hornvale dependency.**
  `scripts/game-no-vessel-dep.sh` asserts this and runs in `make game-check`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field
  and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are
  this project's most common review finding.

## How to test — read this before Task 1

**`make gate-commit` compiles NONE of this campaign's code.** `clients/` is
outside the cargo workspace. The gate that matters here is:

```bash
make game-check     # fmt + clippy + tests on BOTH crates + the containment guard
```

Its recipe is at `Makefile:802`; read it rather than inventing commands. To
iterate faster, run one manifest at a time (`--manifest-path
clients/game/core/Cargo.toml` or `.../bin/Cargo.toml`).

Run `make gate-commit` as well before each commit. It catches nothing in
`clients/`, but after Task 0 this branch also carries part I's workspace-side
commits, and a green workspace is still the commit contract.

**Capture once, inspect many.** Never re-run a suite to grep a second line:

```bash
cargo test --manifest-path clients/game/bin/Cargo.toml > /tmp/hv-stylus.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-stylus.log
```

---

## Stage 0 — the ground the spec was written on

### Task 0: Absorb The Portolan part I, and main

**Why this task exists.** The spec was written against a tree this branch
does not have. `campaign/the-stylus` branched from `main`; part I is not in
its history. Four things the spec asserts are absent here:

| the spec says | what is actually on this branch |
|---|---|
| §0 quotes `input::action_for(key, driver.mode())` in `main.rs` | `main.rs` calls `input::verb_for(key)`; there is no mode |
| §4 supersedes look mode | there is no `Mode`, no `Look`, no `Action` |
| Task 4 re-points part I's keyspace sweep | that sweep is not in this tree |
| F2 proposes the hardware cursor | `Cursor`, `render_with` and `strip.rs` are not in this tree |

Absorbing part I first makes the spec runnable exactly as written. Decision
ledger #6 records the alternatives and why each was rejected.

**Files:**
- Modify: the whole tree, by merge. No hand edits in this task.

**Interfaces:**
- Consumes: nothing.
- Produces: the tree every later task edits — `clients/game/bin/src/input.rs`
  with `Mode`/`Action`/`action_for`; `clients/game/core/src/lib.rs` with
  `Cursor` and `render_with(json, w, h, cursor, strip)`;
  `clients/game/core/src/strip.rs`; `clients/game/bin/src/term.rs` with
  `draw(&Grid, Option<(u16, u16)>)`; `Driver::mode()`, `Driver::cursor()`,
  `Driver::strip_text()`, `Driver::apply(Action)`, `Driver::resize(h)`.

- [ ] **Step 1: Confirm where you are before touching anything**

The worktree pool is shared and recycles; a path that resolved earlier may
belong to another campaign now. Guard every command in a chain with `&&`.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && git status --short
```

Expected: that path, branch `campaign/the-stylus`, and a clean tree apart
from untracked `.superpowers/`.
**If the branch is anything else, STOP and report** — do not proceed.

- [ ] **Step 2: Absorb main into part I's branch first**

Part I is **50 commits behind main** (measured 2026-08-20). Merging it into
this branch directly would import a stale tree.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-portolan && \
  pwd && git branch --show-current && \
  git fetch origin && git merge origin/main
```

Expected: branch `campaign/the-portolan`, and a merge that either completes
or reports conflicts.

**Decision rule — do not predict which:**
- *Completes clean* → go to Step 3.
- *Conflicts in `clients/game/`* → resolve them, preferring part I's side for
  input/cursor code and main's side for everything else. Report every file.
- *Conflicts elsewhere* → resolve to main's side unless part I clearly
  authored the change. Report every file.
- *A generated artifact conflicts* (`docs/audits/`, `docs/digest/`,
  `book/src/domesday/`, `clients/game/core/tests/fixtures/`) → do NOT
  hand-resolve. Take either side; Step 5's `make rebaseline` authors the
  correct content.

- [ ] **Step 3: Verify part I is green against today's main, before merging it here**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-portolan && \
  make game-check
```

Expected: PASS. A clean textual merge is not evidence the premise survived —
50 commits of main have moved underneath this code and nothing has compiled
it against them until now.

**Decision rule:**
- *Green* → go to Step 4.
- *Red* → fix it there, on `campaign/the-portolan`, and commit the fix there.
  It is part I's regression, not The Stylus's. Report what broke and why.

- [ ] **Step 4: Merge part I into The Stylus**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  git merge campaign/the-portolan
```

Expected: branch `campaign/the-stylus`, merge completes. (`git merge-tree`
reported this combination clean on 2026-08-20, *before* Step 2's absorption
of main — re-verify rather than assume it still is.)

- [ ] **Step 5: Regenerate artifacts and commit any drift**

A generated artifact can auto-merge cleanly and be wrong.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make rebaseline && \
  git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Decision rule:**
- *No drift* → nothing to add.
- *`docs/audits/` or `docs/digest/` moved* → expected after a 50-commit
  absorption; `git add` them into this task's commit.
- *`clients/game/core/tests/fixtures/` moved* → the committed seed-42 session
  snapshots changed, meaning the sim's output moved. Report the diff before
  committing it; this is the campaign's byte-identity surface.
- *`book/src/domesday/` moved* → the census CSV changed under you. Report it;
  do not regenerate a census.

- [ ] **Step 6: Verify both gates, then commit**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check && make gate-commit
```

Expected: both PASS.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  git commit --allow-empty -m "merge(the-stylus): absorb The Portolan part I and main

The spec quotes part I's input layer as current and names removing look mode
as a task; neither was in this branch's tree. Part I absorbed main (50
commits) first, so what lands here is part I against today's main rather
than against its own stale base. Decision ledger #6."
```

- [ ] **Step 7: Report what you observed**

State: whether either merge conflicted and where; whether part I was green at
Step 3 and what you fixed if not; exactly which generated paths drifted.

---

## Stage 1 — the buffer

### Task 1: Focus, a total routing table, and the line buffer

**Files:**
- Create: `clients/game/bin/src/line.rs`
- Modify: `clients/game/bin/src/input.rs` (replace `Mode`/`Action`/`action_for`)
- Modify: `clients/game/bin/src/lib.rs` (declare `pub mod line;`)
- Modify: `clients/game/core/src/lib.rs` (add `Focus`)
- Modify: `clients/game/bin/src/driver.rs`, `main.rs` (keep them compiling)
- Test: in-module `#[cfg(test)]` in both `line.rs` and `input.rs`

**Interfaces:**
- Consumes: Task 0's tree — `verb_for`, `Mode`, `Action`, `action_for`.
- Produces, and later tasks depend on these exact names and types:
  - `hornvale_game_core::Focus` — `enum Focus { Cli, Map }`, `Copy`, `Eq`,
    `Default` = `Cli`.
  - `hornvale_game::line::Line` — `new()`, `insert(char)`, `backspace()`,
    `caret_left()`, `caret_right()`, `text() -> String`, `caret() -> usize`,
    `take() -> String`, `set(String)`, `is_empty() -> bool`.
  - `hornvale_game::input::Action` — the variants defined in Step 9.
  - `hornvale_game::input::action_for(key: KeyEvent, focus: Focus) -> Action`.
  - `Driver::focus() -> Focus`, `Driver::toggle_focus()`.

**`Focus` lives in `core`, not `bin`,** because Task 2's `render_with` takes
it as a parameter and `bin` already imports `Cursor` from `core`. One
definition, imported by both.

`text()` returns an owned `String`, not `&str` — the characters are stored
as `Vec<char>` so the caret indexes characters rather than bytes. Later
tasks must expect an owned value.

- [ ] **Step 1: Write the failing test for the line buffer**

Create `clients/game/bin/src/line.rs` with only this test module — no
implementation yet.

```rust
//! The command line's editable buffer.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typing_appends_at_the_caret_and_advances_it() {
        let mut l = Line::new();
        for c in "look".chars() {
            l.insert(c);
        }
        assert_eq!(l.text(), "look");
        assert_eq!(l.caret(), 4);
    }

    #[test]
    fn the_caret_moves_and_insertion_follows_it() {
        let mut l = Line::new();
        for c in "lok".chars() {
            l.insert(c);
        }
        l.caret_left();
        l.insert('o');
        assert_eq!(l.text(), "look");
        assert_eq!(l.caret(), 3);
    }

    #[test]
    fn backspace_deletes_before_the_caret_and_is_a_no_op_at_the_start() {
        let mut l = Line::new();
        for c in "ab".chars() {
            l.insert(c);
        }
        l.caret_left();
        l.backspace();
        assert_eq!(l.text(), "b");
        assert_eq!(l.caret(), 0);
        l.backspace();
        assert_eq!(l.text(), "b", "backspace at the start must not delete forward");
        assert_eq!(l.caret(), 0);
    }

    #[test]
    fn the_caret_never_leaves_the_buffer() {
        let mut l = Line::new();
        l.caret_left();
        assert_eq!(l.caret(), 0);
        l.insert('a');
        l.caret_right();
        l.caret_right();
        assert_eq!(l.caret(), 1, "the caret stops one past the last character");
    }

    #[test]
    fn take_empties_the_buffer_and_resets_the_caret() {
        let mut l = Line::new();
        for c in "look".chars() {
            l.insert(c);
        }
        assert_eq!(l.take(), "look");
        assert_eq!(l.text(), "");
        assert_eq!(l.caret(), 0);
        assert!(l.is_empty());
    }

    /// The buffer is indexed by CHARACTER, not by byte. A multi-byte glyph
    /// must not panic the caret arithmetic or split a character in half.
    /// The sim's own prose carries non-ASCII (`entry.rs`'s truncation
    /// marker is `\u{2026}`), so this is reachable, not theoretical.
    #[test]
    fn a_multi_byte_character_does_not_break_the_caret() {
        let mut l = Line::new();
        l.insert('é');
        l.insert('x');
        assert_eq!(l.caret(), 2);
        l.caret_left();
        l.backspace();
        assert_eq!(l.text(), "x");
    }
}
```

- [ ] **Step 2: Run it and confirm it fails for the right reason**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml line:: > /tmp/hv-line.log 2>&1; echo "exit=$?"
grep -E "^error|cannot find" /tmp/hv-line.log | head
```

Expected: FAILS TO COMPILE — `cannot find type Line in this scope`. A
compile red proves nothing about behaviour; it only shows the type is
absent. The behavioural red for the campaign's actual defect is captured in
Step 8.

- [ ] **Step 3: Implement `Line`**

Add above the test module in `line.rs`:

```rust
//! The command line's editable buffer — the one reversible thing on screen.
//!
//! Everything else the client shows is append-only: the turn counter, the
//! accreted prose, the world itself. A half-typed command can be erased,
//! which is why `Enter` on an empty buffer must cost nothing (spec §6) and
//! why this type is unit-tested in isolation from the terminal.
//!
//! **Indexed by character, never by byte.** The caret is a `char` offset
//! into `chars`, so a multi-byte glyph moves it by one and can never be
//! split. `text()` re-materialises a `String` for rendering.

/// An editable single-line buffer with a caret.
#[derive(Debug, Default, Clone)]
pub struct Line {
    /// The characters typed so far, in order.
    chars: Vec<char>,
    /// The insertion point, as an index into `chars`. Ranges over
    /// `0..=chars.len()`; one past the end is where typing normally happens.
    caret: usize,
}

impl Line {
    /// An empty buffer with the caret at the start.
    pub fn new() -> Line {
        Line::default()
    }

    /// Insert `c` at the caret and advance past it.
    pub fn insert(&mut self, c: char) {
        self.chars.insert(self.caret, c);
        self.caret += 1;
    }

    /// Delete the character before the caret. A no-op at the start — it
    /// never deletes forward, which would be a surprise on the one
    /// reversible surface the client has.
    pub fn backspace(&mut self) {
        if self.caret > 0 {
            self.caret -= 1;
            self.chars.remove(self.caret);
        }
    }

    /// Move the caret one character left, stopping at the start.
    pub fn caret_left(&mut self) {
        self.caret = self.caret.saturating_sub(1);
    }

    /// Move the caret one character right, stopping one past the last
    /// character.
    pub fn caret_right(&mut self) {
        if self.caret < self.chars.len() {
            self.caret += 1;
        }
    }

    /// The buffer's contents.
    pub fn text(&self) -> String {
        self.chars.iter().collect()
    }

    /// The caret's position, as a character offset.
    pub fn caret(&self) -> usize {
        self.caret
    }

    /// Whether the buffer holds nothing.
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    /// Empty the buffer and return what it held, resetting the caret.
    pub fn take(&mut self) -> String {
        self.caret = 0;
        std::mem::take(&mut self.chars).into_iter().collect()
    }

    /// Replace the contents, putting the caret at the end — how history
    /// recall lands a remembered line (Task 3).
    pub fn set(&mut self, text: String) {
        self.chars = text.chars().collect();
        self.caret = self.chars.len();
    }
}
```

Declare it in `clients/game/bin/src/lib.rs`:

```rust
pub mod line;
```

- [ ] **Step 4: Run the tests and confirm they pass**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml line:: > /tmp/hv-line.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-line.log
```

Expected: `test result: ok. 6 passed`.

- [ ] **Step 5: Commit the buffer on its own**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  cargo fmt --manifest-path clients/game/bin/Cargo.toml && \
  git add clients/game/bin/src/line.rs clients/game/bin/src/lib.rs && \
  git commit -m "feat(game): a character-indexed line buffer with a caret"
```

- [ ] **Step 6: Add `Focus` to core**

In `clients/game/core/src/lib.rs`, beside `Cursor`:

```rust
/// Which pane a key press is addressed to.
///
/// This is the client's one input mode, and it replaces The Portolan part
/// I's `Mode { Normal, Look }`: there is no longer a mode to enter in order
/// to point at something, only a question of which pane is listening.
/// Toggled by `Esc`; a printable key pressed while the map is focused
/// returns focus here and types itself, so the common path costs no
/// keypress at all (spec §2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Focus {
    /// The command line is listening. Text is the default destination.
    #[default]
    Cli,
    /// The map is listening: arrows drive the map cursor, `-`/`+`/`=` zoom.
    Map,
}
```

- [ ] **Step 7: Write the failing routing tests**

Replace `input.rs`'s `Mode`/`Action` test coverage with these. Leave every
existing `verb_for` test untouched — `verb_for` itself does not change in
this task.

```rust
    /// **H1, and the whole point of the campaign.** With the CLI focused,
    /// every printable ASCII character types ITSELF. Not "most keys" and
    /// not a sampled list: the assertion is that the count of printable
    /// characters doing anything other than typing themselves is ZERO.
    ///
    /// Part I's sweep asserted the opposite property against `verb_for` (a
    /// binding count of 27) and is superseded here rather than deleted: the
    /// sweep was always the right test, and it now asserts that almost
    /// every key is text (spec §4).
    #[test]
    fn every_printable_character_types_itself_when_the_cli_is_focused() {
        let mut not_text = Vec::new();
        for b in 0x20u8..=0x7Eu8 {
            let c = b as char;
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            match action_for(key, Focus::Cli) {
                Action::Type(got) if got == c => {}
                other => not_text.push((c, format!("{other:?}"))),
            }
        }
        assert!(
            not_text.is_empty(),
            "these printable characters did not type themselves with the CLI \
             focused: {not_text:?}"
        );
    }

    /// The other half of totality: the sweep above says what text does, and
    /// this says every non-printable key has a DEFINED destination too. H3
    /// is falsified by a key whose destination cannot be predicted.
    #[test]
    fn the_named_keys_route_predictably_with_the_cli_focused() {
        let cases = [
            (KeyCode::Left, Action::CaretBy(-1)),
            (KeyCode::Right, Action::CaretBy(1)),
            (KeyCode::Up, Action::HistoryPrev),
            (KeyCode::Down, Action::HistoryNext),
            (KeyCode::Enter, Action::Submit),
            (KeyCode::Backspace, Action::DeleteBack),
            (KeyCode::Esc, Action::ToggleFocus),
            (KeyCode::Tab, Action::None),
        ];
        for (code, want) in cases {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Cli), want, "{code:?}");
        }
    }

    /// With the map focused, a printable character returns focus to the CLI
    /// AND types itself — one keypress, not two (spec §2). The zoom keys
    /// are the deliberate exception, checked separately below.
    #[test]
    fn a_printable_character_bounces_focus_back_to_the_cli_and_types() {
        let mut wrong = Vec::new();
        for b in 0x20u8..=0x7Eu8 {
            let c = b as char;
            if matches!(c, '-' | '+' | '=') {
                continue;
            }
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            match action_for(key, Focus::Map) {
                Action::FocusAndType(got) if got == c => {}
                other => wrong.push((c, format!("{other:?}"))),
            }
        }
        assert!(wrong.is_empty(), "did not bounce-and-type: {wrong:?}");
    }

    /// The zoom keys must NOT bounce and must NOT type. This is the
    /// assertion discriminating a routed zoom binding from a character that
    /// merely falls through to the buffer.
    #[test]
    fn the_zoom_keys_zoom_on_the_map_and_type_on_the_cli() {
        let cases = [
            ('-', Action::Zoom(-1)),
            ('+', Action::Zoom(1)),
            ('=', Action::Zoom(1)),
        ];
        for (c, want) in cases {
            let key = KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Map), want, "{c} on the map");
            assert_eq!(
                action_for(key, Focus::Cli),
                Action::Type(c),
                "{c} must be ordinary text on the CLI"
            );
        }
    }

    /// The named keys with the map focused. `Enter` and `Backspace` are
    /// deliberately inert here: both act on a buffer whose caret is not
    /// being shown, and a destructive or turn-costing key must not fire
    /// against a surface the player cannot see.
    #[test]
    fn the_named_keys_route_predictably_with_the_map_focused() {
        let cases = [
            (KeyCode::Left, Action::CursorBy(-1, 0)),
            (KeyCode::Right, Action::CursorBy(1, 0)),
            (KeyCode::Up, Action::CursorBy(0, -1)),
            (KeyCode::Down, Action::CursorBy(0, 1)),
            (KeyCode::Esc, Action::ToggleFocus),
            (KeyCode::Tab, Action::None),
            (KeyCode::Enter, Action::None),
            (KeyCode::Backspace, Action::None),
        ];
        for (code, want) in cases {
            let key = KeyEvent::new(code, KeyModifiers::NONE);
            assert_eq!(action_for(key, Focus::Map), want, "{code:?}");
        }
    }

    /// `Tab` is reserved for completion and bound to NOTHING, in both focus
    /// states (spec §3.3). Spending it is the mistake this test makes loud:
    /// it fails the moment anyone gives `Tab` a meaning.
    #[test]
    fn tab_is_bound_to_nothing_in_either_focus() {
        let tab = KeyEvent::new(KeyCode::Tab, KeyModifiers::NONE);
        assert_eq!(action_for(tab, Focus::Cli), Action::None);
        assert_eq!(action_for(tab, Focus::Map), Action::None);
    }

    /// The chord discipline survives the rewrite: `Ctrl-L` must not type an
    /// `l` any more than it used to walk the player east. Checked in BOTH
    /// focus states, because the routing table is now two tables.
    #[test]
    fn a_chord_is_inert_in_either_focus() {
        let codes = [
            KeyCode::Char('l'),
            KeyCode::Char('c'),
            KeyCode::Enter,
            KeyCode::Left,
        ];
        for code in codes {
            for m in [KeyModifiers::CONTROL, KeyModifiers::ALT] {
                for focus in [Focus::Cli, Focus::Map] {
                    assert_eq!(
                        action_for(KeyEvent::new(code, m), focus),
                        Action::None,
                        "{code:?} with {m:?} in {focus:?}"
                    );
                }
            }
        }
    }

    /// A bare SHIFT is not a chord — it is how a capital letter arrives at
    /// all, and `Q` must type a `Q` now rather than releasing.
    #[test]
    fn shift_still_types_a_capital_and_q_no_longer_releases() {
        let q = KeyEvent::new(KeyCode::Char('Q'), KeyModifiers::SHIFT);
        assert_eq!(action_for(q, Focus::Cli), Action::Type('Q'));
    }

    /// A key-release event must never do anything — one physical keystroke
    /// must not type two characters any more than it could cost two turns.
    #[test]
    fn a_release_event_is_inert_in_either_focus() {
        let mut key = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE);
        key.kind = KeyEventKind::Release;
        assert_eq!(action_for(key, Focus::Cli), Action::None);
        assert_eq!(action_for(key, Focus::Map), Action::None);
    }
```

- [ ] **Step 8: Capture the behavioural red before implementing**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml input:: > /tmp/hv-input.log 2>&1; echo "exit=$?"
grep -E "^error" /tmp/hv-input.log | head
```

Expected: FAILS TO COMPILE (`Focus` is not a parameter of `action_for`).

**A compile error says nothing about whether the client is broken, so
capture the real defect as an observation first.** Stash your test edits and
run part I's own sweep against the unmodified code:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  git stash && \
  cargo test --manifest-path clients/game/bin/Cargo.toml \
    normal_mode_agrees_with_verb_for > /tmp/hv-old.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-old.log
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && git stash pop
```

Expected: it PASSES — which is the point. The old sweep is green while
typing `look` sends `go e` and drops `o`, `o`, `k`. Green against the wrong
property is exactly what this campaign supersedes. Paste both results into
your report.

- [ ] **Step 9: Implement the routing table**

Replace `Mode`, `Action` and `action_for` in `input.rs`. `verb_for` stays
exactly as it is — Task 4 removes it, once nothing calls it.

```rust
use hornvale_game_core::Focus;

/// What one key press means, once [`Focus`] is taken into account.
///
/// **The table is TOTAL**: every `KeyCode` maps to exactly one variant in
/// each focus state, and [`Action::None`] is a destination like any other.
/// H3 is falsified by a key whose destination cannot be predicted from what
/// is on screen, so "unhandled" is not an option this enum offers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Insert this character at the caret — the CLI's answer to almost
    /// every printable key.
    Type(char),
    /// Return focus to the CLI and insert this character. One keypress,
    /// produced only when the map is focused (spec §2).
    FocusAndType(char),
    /// Delete the character before the caret.
    DeleteBack,
    /// Move the text caret by this many characters.
    CaretBy(i16),
    /// Recall the previous line from history.
    HistoryPrev,
    /// Recall the next line from history.
    HistoryNext,
    /// Submit the buffer. On an empty buffer this must cost no turn
    /// (spec §6) — that is the driver's call, not the router's.
    Submit,
    /// Move the map cursor by `(dx, dy)` grid cells.
    CursorBy(i16, i16),
    /// Zoom the map in (`1`) or out (`-1`). **Routed, not implemented** —
    /// zoom itself is The Portolan part II's. The driver accepts and
    /// ignores it; what matters now is that `-` on the map does not fall
    /// through to the buffer and type a `-`.
    Zoom(i8),
    /// Move focus to the other pane.
    ToggleFocus,
    /// The key does nothing in this focus state. Costs no turn, draws
    /// nothing, and is a deliberate destination — `Tab` is the clearest
    /// case (spec §3.3).
    None,
}

/// Map one key press to an [`Action`], given the current [`Focus`].
///
/// **The direction this function enforces:** total in both focus states.
/// Every key has a defined destination; none falls through unanswered.
///
/// The chord discipline is unchanged from [`verb_for`] and applies before
/// the key code is inspected: only a bare [`KeyEventKind::Press`] with no
/// modifier beyond [`KeyModifiers::SHIFT`] does anything at all, so
/// `Ctrl-L` types nothing just as it used to walk nowhere.
pub fn action_for(key: KeyEvent, focus: Focus) -> Action {
    if key.kind != KeyEventKind::Press {
        return Action::None;
    }
    if !key.modifiers.difference(KeyModifiers::SHIFT).is_empty() {
        return Action::None;
    }
    match focus {
        Focus::Cli => match key.code {
            KeyCode::Char(c) => Action::Type(c),
            KeyCode::Left => Action::CaretBy(-1),
            KeyCode::Right => Action::CaretBy(1),
            KeyCode::Up => Action::HistoryPrev,
            KeyCode::Down => Action::HistoryNext,
            KeyCode::Enter => Action::Submit,
            KeyCode::Backspace => Action::DeleteBack,
            KeyCode::Esc => Action::ToggleFocus,
            _ => Action::None,
        },
        Focus::Map => match key.code {
            KeyCode::Char('-') => Action::Zoom(-1),
            KeyCode::Char('+') | KeyCode::Char('=') => Action::Zoom(1),
            KeyCode::Char(c) => Action::FocusAndType(c),
            KeyCode::Left => Action::CursorBy(-1, 0),
            KeyCode::Right => Action::CursorBy(1, 0),
            KeyCode::Up => Action::CursorBy(0, -1),
            KeyCode::Down => Action::CursorBy(0, 1),
            KeyCode::Esc => Action::ToggleFocus,
            _ => Action::None,
        },
    }
}
```

`Tab` reaches `_ => Action::None` in both arms. That is correct, and it is
what `tab_is_bound_to_nothing_in_either_focus` pins — but do NOT add an
explicit `KeyCode::Tab` arm to make it look deliberate: an explicit arm
returning `None` is indistinguishable from the fallthrough and invites
someone to fill it in later.

- [ ] **Step 10: Fix the fallout and run the whole crate**

`Driver::mode()`, `Driver::apply(Action)` and `main.rs` still speak the old
`Mode`/`Action`. Get the crate compiling with the smallest change that
preserves behaviour: give `Driver` a `focus: Focus` field with `focus()` and
`toggle_focus()`, and have `apply` handle the new variants.

**Wiring the buffer into `apply` is Task 3's job.** For now `Type`,
`FocusAndType`, `DeleteBack`, `CaretBy`, `HistoryPrev`, `HistoryNext`,
`Submit` and `Zoom` may be accepted and ignored, each with a
`// Task 3 wires this to the buffer.` comment. `ToggleFocus` and `CursorBy`
work now.

Delete part I's diagonal cursor bindings along with the `yubn` arms — the
map cursor is arrows-only now (ledger #12), and leaving the assertions in
place would pin a capability the client no longer has.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check
```

Expected: PASS.

- [ ] **Step 11: Settle F1 and F3 in your report, then commit**

**F1 — is the map reachable?** `Esc` is the SOLE route to map focus (a
printable key bounces back automatically, but nothing bounces the other
way), so an `Esc` that does not arrive is a total lockout of half the
client, not a degradation. State what you observed about `KeyCode::Esc`
handling. The fallback, if it is ever needed, is one binding: `` ` `` is
free — it currently routes to ``Action::Type('`')``.

**F3 — where does the buffer live?** State it plainly: `bin`, in `line.rs`,
owned there and handed over for rendering (Task 2), following the precedent
`render_with`'s `strip` parameter already set. Confirm nothing you wrote
reaches into `Snapshot`.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  cargo fmt --manifest-path clients/game/bin/Cargo.toml && \
  cargo fmt --manifest-path clients/game/core/Cargo.toml && \
  git add clients/game && \
  git commit -m "feat(game): focus replaces look mode, and the routing table is total

Mode{Normal,Look} becomes Focus{Cli,Map}: x types an x, hjkl type letters,
and every printable character reaches the buffer. The sweep part I proved
catches a keyspace hole now asserts the opposite property — that the count
of printable characters doing anything other than typing themselves is
zero. Tab stays bound to nothing. Spec sections 2, 3.3 and 4."
```

---

### Task 2: Draw the line, and show focus with the one hardware cursor

**Files:**
- Modify: `clients/game/core/src/entry.rs` (draw the buffer, report the caret)
- Modify: `clients/game/core/src/spread.rs` (thread the line through)
- Modify: `clients/game/core/src/lib.rs` (`CommandLine`; `render_with` grows)
- Modify: `clients/game/bin/src/main.rs` (`redraw` passes them)
- Test: in-module tests in `entry.rs`

**Interfaces:**
- Consumes: `Focus` from Task 1; `Line` for its `text()`/`caret()`.
- Produces:
  - `hornvale_game_core::CommandLine<'a> { text: &'a str, caret: usize }`,
    `Copy` + `Default`.
  - `render_with(json: &str, w: u16, h: u16, focus: Focus, map_cursor: Option<Cursor>, line: CommandLine<'_>, strip: Option<&str>) -> Result<(Grid, Option<(u16, u16)>), Error>`
  - `entry::draw(...) -> Option<(u16, u16)>` — the caret's screen position,
    `Some` only when the CLI is focused.

**The design decision this task makes (F2), and why it is forced.** A
terminal has exactly ONE hardware cursor. §2.1 forbids ornament occupying a
cell that carries information, which rules out drawing a focus marker onto
the grid. So the one cursor's LOCATION is the signal:

| focus | where the hardware cursor sits |
|---|---|
| `Cli` | the caret, in the entry pane, after the `>` prompt |
| `Map` | the map cursor's cell, on the plate |

**"The cursor is unconditional" (spec §4) means its POSITION is always held
and always pointable — not that it is always displayed.** The naive reading
would leave focus unshown and falsify H3 while consuming the only mechanism
§2.1 permits. Ledger #10.

**Why `core` computes the caret's screen position and not `bin`:** the entry
pane's origin and the prompt's width live in `spread`/`entry`. `bin` does
not know them and must not learn them.

- [ ] **Step 1: Write the failing tests for drawing the line**

In `entry.rs`'s test module:

```rust
    /// The buffer is drawn after the prompt on the command row, and the
    /// caret's screen position is reported for the terminal to place its
    /// own cursor at — never drawn as ink.
    #[test]
    fn the_buffer_is_drawn_after_the_prompt_and_the_caret_is_reported() {
        let n = Narration { prose: "hi".to_string(), nouns: vec![] };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Cli,
            crate::CommandLine { text: "look", caret: 4 },
        );
        let row: String = (0..6)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(row, "> look");
        assert_eq!(caret, Some((6, 2)), "the caret sits one past the last character");
    }

    /// With the map focused the entry pane reports NO cursor position — the
    /// one hardware cursor is over on the plate, and that is how focus is
    /// shown (spec §2.1). The line's TEXT is still drawn: the buffer does
    /// not disappear because the player looked away.
    #[test]
    fn the_caret_is_not_reported_when_the_map_is_focused() {
        let n = Narration { prose: "hi".to_string(), nouns: vec![] };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Map,
            crate::CommandLine { text: "look", caret: 4 },
        );
        assert_eq!(caret, None);
        let row: String = (0..6)
            .map(|x| g.get(x, 2).unwrap().glyph.unwrap_or(' '))
            .collect();
        assert_eq!(row, "> look", "the buffer stays visible while the map has focus");
    }

    /// The caret follows the caret INDEX, not the end of the text.
    #[test]
    fn the_caret_reports_the_index_not_the_end() {
        let n = Narration { prose: "hi".to_string(), nouns: vec![] };
        let mut g = crate::Grid::new(20, 3);
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            20,
            3,
            crate::Focus::Cli,
            crate::CommandLine { text: "look", caret: 1 },
        );
        assert_eq!(caret, Some((3, 2)));
    }

    /// **A line longer than the pane must keep the caret visible.** The
    /// entry pane is 40 columns at the 80x24 floor; a command can be
    /// longer. Whatever windowing you choose, the invariant is that the
    /// reported caret is inside the pane — a caret drawn off-screen is a
    /// cursor the player cannot find.
    #[test]
    fn a_line_wider_than_the_pane_keeps_the_caret_on_screen() {
        let n = Narration { prose: "hi".to_string(), nouns: vec![] };
        let width = 20u16;
        let mut g = crate::Grid::new(width, 3);
        let long: String = std::iter::repeat_n('a', 60).collect();
        let caret = draw(
            &n,
            &mut g,
            (0, 0),
            width,
            3,
            crate::Focus::Cli,
            crate::CommandLine { text: &long, caret: 60 },
        );
        let (cx, _) = caret.expect("the CLI is focused, so a caret is reported");
        assert!(cx < width, "caret at column {cx} is outside a {width}-column pane");
    }
```

- [ ] **Step 2: Run and confirm the compile red**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/core/Cargo.toml entry:: > /tmp/hv-entry.log 2>&1; echo "exit=$?"
grep -E "^error" /tmp/hv-entry.log | head
```

Expected: FAILS TO COMPILE — `draw` takes 5 arguments, not 7.

- [ ] **Step 3: Add `CommandLine` to core**

In `clients/game/core/src/lib.rs`, beside `Focus`:

```rust
/// The command line's contents, as the renderer sees them.
///
/// A borrowed view, never ownership: the buffer lives in the binary
/// (`hornvale_game::line::Line`) and is handed over for drawing, the same
/// way [`render_with`]'s `strip` already is. This crate has no dependency
/// on any hornvale crate and gains none here — a `&str` and an index are
/// the whole contract.
#[derive(Debug, Clone, Copy, Default)]
pub struct CommandLine<'a> {
    /// What has been typed so far.
    pub text: &'a str,
    /// The insertion point, as a CHARACTER offset into `text` — never a
    /// byte offset, so a multi-byte glyph cannot split it.
    pub caret: usize,
}
```

- [ ] **Step 4: Draw the line and report the caret**

Change `entry::draw` to take `focus: crate::Focus` and
`line: crate::CommandLine<'_>`, and to return `Option<(u16, u16)>`.

After the existing prompt `set`, draw the line's text starting at
`origin.0 + 2` (prompt, then a space), and compute the caret's column.
Available text columns are `width - 2`.

**Attribute the typed text to `Source::Chrome`, not `Source::Prose`** — it
is not the sim's narration, it is the player's own unsent keystrokes.
`Source::Prose` would be a false provenance claim of exactly the kind
`entry.rs`'s own doc already warns about for the prompt glyph.

**Update the module doc**, whose opening claim now reads "a prompt, **not a
text box**". It is a text box now; say so, and keep the sentence about why
it was not one before.

**Windowing:** when the caret would fall outside the available columns,
scroll the visible window so the caret stays inside. Choose the simplest
rule that satisfies the test and document it in the function's doc.

- [ ] **Step 5: Thread it through `spread` and `render_with`**

`spread::compose` gains `focus` and `line`, passes them to `entry::draw`,
and returns the caret alongside the grid. `render_with` then picks which
position to report:

```rust
    let (grid, caret) = spread::compose(&snapshot, w, h, strip, focus, line);
    // ONE hardware cursor, so its location IS the focus indicator: the
    // caret in the entry pane, or the map cursor on the plate. Never both,
    // never neither — see `Focus`.
    let cursor = match focus {
        Focus::Cli => caret,
        Focus::Map => map_cursor.map(|c| (c.x, c.y)),
    };
    Ok((grid, cursor))
```

Keep `render(json, w, h)` delegating with `Focus::Cli`, `None`,
`CommandLine::default()` and `None`. It drops the cursor anyway, and
`core`'s own fixture tests depend on that exact signature.

- [ ] **Step 6: Pass them from `main.rs`**

`redraw` reads `driver.focus()`, `driver.cursor()` and the buffer's
`text()`/`caret()`, and hands them to `render_with`. `term.draw(&grid,
cursor)` is unchanged — it already shows and moves the cursor on `Some` and
hides it on `None`.

- [ ] **Step 7: Run everything**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check
```

Expected: PASS.

- [ ] **Step 8: Prove the focus indicator can actually fail**

A passing test is not evidence the indicator works; it must be able to go
red. Use `scripts/mutate.py` — it substitutes only if the target is found
and unique, where a `sed` matching nothing produces a green indistinguishable
from a robust implementation.

Neutralise the focus branch in `render_with` so it reports the caret in both
focus states. Run the core tests and confirm
`the_caret_is_not_reported_when_the_map_is_focused` FAILS. Restore, re-run,
confirm green. Paste both outcomes into your report.

**If the mutation turns nothing red, that is the finding** — report it
rather than proceeding. It would mean focus is not actually observable.

- [ ] **Step 9: Settle F2 in your report, then commit**

State which mechanism you chose and whether you added anything beyond cursor
location. If you did, justify it against §2.1's rule that ornament may never
occupy a cell that carries information.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  cargo fmt --manifest-path clients/game/core/Cargo.toml && \
  cargo fmt --manifest-path clients/game/bin/Cargo.toml && \
  git add clients/game && \
  git commit -m "feat(game): draw the command line, and show focus with the one cursor

A terminal has exactly one hardware cursor, and section 2.1 forbids ornament
taking an informative cell — so its location is the focus signal: the caret
in the entry pane, or the map cursor on the plate. The map cursor's position
stays unconditional; what is conditional is which pane displays it."
```

**STAGE 1 BOUNDARY — absorb main and gate before continuing.**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  git fetch origin && git merge origin/main && \
  make rebaseline && \
  git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

Commit any drift, push, then submit the stage gate with a FULL SHA:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  git push -u origin campaign/the-stylus && \
  make sluice-stage BRANCH=campaign/the-stylus REF=$(git rev-parse HEAD)
```

---

## Stage 2 — the loop

### Task 3: `Enter`, echo, history, and an exit that survives free text

**Files:**
- Create: `clients/game/bin/src/history.rs`
- Modify: `clients/game/bin/src/driver.rs` (buffer, history, release reporting)
- Modify: `clients/game/bin/src/main.rs` (release on the driver's answer)
- Modify: `clients/game/core/src/entry.rs` (echo the submitted line)
- Modify: `clients/game/bin/src/lib.rs` (declare `pub mod history;`)
- Test: in-module tests in `history.rs`; `clients/game/bin/tests/driver.rs`

**Interfaces:**
- Consumes: `Line`, `Action`, `Focus` from Tasks 1–2.
- Produces:
  - `hornvale_game::history::History` — `new()`, `push(String)`,
    `prev() -> Option<&str>`, `next() -> Option<&str>`, `reset()`.
  - `Driver::handle(&mut self, line: &str) -> bool` — returns whether the
    session RELEASED, replacing the caller's string comparison.
  - `Driver::apply(&mut self, action: Action) -> bool` — same meaning.
  - `entry::draw(...)` gains an `echo: Option<&str>` parameter.

**The bug this task fixes, verified rather than predicted (ledger #8).**
`windows/vessel/src/session.rs:1367` is:

```rust
"release" | "quit" => Turn::Released("You let go.".to_string()),
```

Two synonyms. `main.rs` decides on the SENT string
(`let released = matches!(&action, Action::Verb(v) if v == "release")`), and
its own comment predicts this exact break: *"If a future free-text input
mode lets a player type `quit` directly, this check needs to grow with it or
move to reading the driver's answer instead."* `Driver::handle` currently
discards the outcome (`let _turn = self.session.handle(line);`), so today the
driver cannot report it.

**This matters more than it looks.** Once `Q` types a `Q` (Task 1), typing
`release` or `quit` is the ONLY way out of the client — `Ctrl+C` is inert
and §5 refuses modifier keys, so it must stay inert. The exit path now runs
through the buffer, `Enter`, and this reporting change. Ledger #11.

- [ ] **Step 1: Write the failing history tests**

Create `clients/game/bin/src/history.rs` with only this test module:

```rust
//! Recalled command lines.

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
        assert_eq!(h.prev(), Some("look"), "walking past the oldest must not wrap");
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
```

- [ ] **Step 2: Run and confirm the compile red**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml history:: > /tmp/hv-hist.log 2>&1; echo "exit=$?"
grep -E "^error|cannot find" /tmp/hv-hist.log | head
```

Expected: `cannot find type History in this scope`.

- [ ] **Step 3: Implement `History`**

A `Vec<String>` plus a walk position. Use `Vec`, never a map — the workspace
bans `HashMap`/`HashSet` and this crate follows the same convention.
Document that the walk position is an index from the END, and that `push`
resets it.

- [ ] **Step 4: Run the history tests**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml history:: > /tmp/hv-hist.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-hist.log
```

Expected: `ok. 5 passed`.

- [ ] **Step 5: Write the failing whole-line and exit tests**

In `clients/game/bin/tests/driver.rs`. **H2 and the §8 whole-line test live
here**, because H1's sweep tests single keys and by construction cannot see
a buffer that drops every third character.

```rust
    /// **H2.** A whole line typed key by key reaches the sim and its answer
    /// comes back. The sweep in `input.rs` tests single keys and cannot see
    /// a defect in SEQUENCES — a buffer that drops every third character
    /// would pass it and fail here (spec §8).
    #[test]
    fn a_whole_typed_line_reaches_the_sim_and_its_answer_returns() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        for c in "look".chars() {
            d.apply(Action::Type(c));
        }
        assert_eq!(d.line_text(), "look", "the buffer must hold every character typed");
        d.apply(Action::Submit);

        let typed = d.snapshot();
        let mut direct = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        direct.handle("look");
        assert_eq!(
            typed,
            direct.snapshot(),
            "typing `look` must produce exactly what handle(\"look\") produces"
        );
    }

    /// **Spec §6.** `Enter` on an empty buffer must not advance the world.
    /// The buffer is the last point of reversibility before an irreversible
    /// act, and a stray keypress must not cost a turn.
    #[test]
    fn enter_on_an_empty_line_costs_no_turn() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        let before = d.snapshot();
        let released = d.apply(Action::Submit);
        assert!(!released);
        assert_eq!(d.snapshot(), before, "an empty submit must change nothing at all");
    }

    /// **Ledger #8, and the campaign's exit guarantee.** Both synonyms the
    /// sim honours must end the loop. `quit` is the one the old sent-string
    /// check could not see, and once `Q` types a `Q` this is the only way
    /// out of the client.
    #[test]
    fn both_release_synonyms_end_the_possession() {
        for line in ["release", "quit"] {
            let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
            assert!(d.handle(line), "`{line}` must report a release");
        }
    }

    /// An ordinary verb must NOT report a release — otherwise the loop ends
    /// on the first command and the test above passes vacuously.
    #[test]
    fn an_ordinary_verb_does_not_report_a_release() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        assert!(!d.handle("look"));
    }

    /// **Spec §6.** The submitted line is echoed so the record shows what
    /// was ASKED, not only what was answered. A journal that records only
    /// replies is not one.
    #[test]
    fn the_submitted_line_is_echoed_into_the_entry() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        for c in "look".chars() {
            d.apply(Action::Type(c));
        }
        d.apply(Action::Submit);
        assert_eq!(d.echo(), Some("look"));
    }

    /// The buffer empties on submit — a command must not be left behind to
    /// be sent twice.
    #[test]
    fn submitting_empties_the_buffer() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        for c in "look".chars() {
            d.apply(Action::Type(c));
        }
        d.apply(Action::Submit);
        assert_eq!(d.line_text(), "");
    }

    /// A printable key pressed while the map is focused returns focus to
    /// the CLI *and* types — one keypress, not two (spec §2).
    #[test]
    fn a_printable_key_on_the_map_bounces_focus_and_types() {
        let mut d = Driver::start(42, PossessTarget::Flagship).expect("genesis");
        d.apply(Action::ToggleFocus);
        assert_eq!(d.focus(), Focus::Map);
        d.apply(Action::FocusAndType('l'));
        assert_eq!(d.focus(), Focus::Cli);
        assert_eq!(d.line_text(), "l");
    }
```

You will need small accessors on `Driver` for these: `line_text() -> String`
and `echo() -> Option<&str>`. Add them with doc comments.

- [ ] **Step 6: Run them and record the red**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  cargo test --manifest-path clients/game/bin/Cargo.toml --test driver > /tmp/hv-drv.log 2>&1; echo "exit=$?"
grep -E "^error|^test result|FAILED" /tmp/hv-drv.log | head -20
```

Expected: compile errors for the new accessors and the changed `handle`
return type. Note them; the behavioural red follows once it compiles.

- [ ] **Step 7: Implement**

1. **`Driver::handle` returns `bool`.** Read the `Turn` it currently
   discards: `matches!(self.session.handle(line), Turn::Released(_))`.
   Refresh the cache as before, then return that.
2. **`Driver` grows `line: Line`, `history: History`, `echo: Option<String>`.**
3. **`Driver::apply` returns `bool`** and wires every variant:
   - `Type(c)` → `line.insert(c)`
   - `FocusAndType(c)` → `focus = Cli; line.insert(c)`
   - `DeleteBack` → `line.backspace()`
   - `CaretBy(-1)` → `line.caret_left()`; `CaretBy(1)` → `line.caret_right()`
   - `HistoryPrev`/`HistoryNext` → `line.set(...)` from the recalled entry;
     `HistoryNext` past the newest clears the buffer
   - `Submit` → if `line.is_empty()`, return `false` having done NOTHING
     (spec §6). Otherwise take the line, push it to history, store it as
     `echo`, and return `self.handle(&taken)`.
   - `CursorBy` → unchanged from part I
   - `Zoom(_)` → accepted and ignored, with a doc comment saying zoom is The
     Portolan part II's and this arm is where it lands
   - `ToggleFocus` → flip `focus`
   - `None` → nothing
4. **`main.rs`** stops comparing strings: `let released = driver.apply(action);`
   Delete the stale comment block that predicted this change and replace it
   with one saying the driver's own answer is now what decides.
5. **`entry::draw` gains `echo: Option<&str>`**, drawn immediately above the
   command row so the record reads ask-then-answer.

- [ ] **Step 8: Run everything**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check
```

Expected: PASS.

- [ ] **Step 9: Prove the whole-line test can see a sequence defect**

H2's value is that it catches what the single-key sweep cannot. Demonstrate
that it does. Using `scripts/mutate.py`, make `Line::insert` drop every third
character, then run the bin tests.

Expected:
`a_whole_typed_line_reaches_the_sim_and_its_answer_returns` FAILS while
`every_printable_character_types_itself_when_the_cli_is_focused` still
PASSES — which is precisely the blind spot §8 names. Restore and confirm
green. Paste both outcomes into your report.

**If the sweep also fails, say so** — it would mean the two tests are less
independent than the spec claims, which is worth knowing.

- [ ] **Step 10: Settle F4, then commit**

**F4 — does history survive a `release`?** State what it does. `release`
ends `play`'s loop and the process exits, so history dies with it; nothing
persists it and nothing is expected to (spec §9). Say so plainly rather than
leaving it accidental.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  cargo fmt --manifest-path clients/game/bin/Cargo.toml && \
  cargo fmt --manifest-path clients/game/core/Cargo.toml && \
  git add clients/game && \
  git commit -m "feat(game): Enter, echo, history, and release read from the answer

The possession now ends because the driver says it did, not because the sent
string was spelled 'release' — the sim honours 'quit' too, and main.rs's own
comment predicted this break. Enter on an empty buffer costs no turn: the
buffer is the last reversible thing before an irreversible act. Ledger #8."
```

---

### Task 4: Remove look mode's residue

**Files:**
- Modify: `clients/game/bin/src/input.rs` (delete `verb_for` if unused)
- Modify: `clients/game/bin/src/driver.rs` (strip caching)
- Modify: `clients/game/core/src/strip.rs` (or delete it)
- Modify: doc comments across `clients/game/`
- Test: whatever the deletions break

**Interfaces:**
- Consumes: everything Tasks 1–3 produced.
- Produces: no new API. This task only removes.

**What is left to remove.** Task 1 replaced the *routing*; this removes what
that orphaned. Find it rather than trusting this list — grep for the
observable, not for the function you happen to open:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  grep -rn "look mode\|Look\|verb_for\|EnterLook\|LeaveLook\|Mode::" \
    clients/game --include=*.rs | grep -v target
```

- [ ] **Step 1: Decide `verb_for`'s fate, from what calls it**

After Task 1, nothing routes through `verb_for` — every key is text. Check:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  grep -rn "verb_for" clients/game --include=*.rs | grep -v target
```

**Decision rule:**
- *Only its own tests reference it* → delete `verb_for` and every test that
  covers it. Its bindings are gone; the tests would pin a keymap the client
  no longer has, which is worse than no test.
- *Something still calls it* → report what, and leave it. It means a key
  path was missed in Task 1, which is a finding.

Deleting `verb_for` also deletes the `Ctrl-L` regression test. **Do not lose
that property** — `a_chord_is_inert_in_either_focus` (Task 1) already covers
it against `action_for`. Confirm that before deleting, and say so.

- [ ] **Step 2: Decide the strip's fate**

`core/src/strip.rs` is part I's look-mode strip, and `Driver::strip_text()`
caches it on `EnterLook`, which no longer exists. But the cursor is
unconditional now, so the strip's *content* — what the cursor is pointing at
— is arguably always meaningful.

**Decision rule:**
- *The strip still renders correctly with focus on the map* → keep it, and
  recompute on `CursorBy` rather than on a mode transition. Say so.
- *It depends on look mode in a way that does not survive* → remove it and
  its reserved row, and state what the plate's content height becomes.
  `render_with`'s doc records that reserving the row cost the plate one row
  at 80×24 (21 → 20); if you give it back, update that doc.

Either way the 80×24 floor is unweakened.

- [ ] **Step 3: Sweep the prose**

Doc comments across both crates still describe look mode, `x` entering it,
and `hjkl` driving a cursor. The module docs on `input.rs`, `entry.rs`,
`driver.rs` and `term.rs` all say things that are no longer true.

**Fix the claims, do not just delete them.** Where a doc explains *why*
something is the way it is — `entry.rs`'s truncation decision, `driver.rs`'s
containment rule, `input.rs`'s chord discipline — that reasoning is still
load-bearing and must survive. What changes is the input model around it.

- [ ] **Step 4: Run everything**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check
```

Expected: PASS.

- [ ] **Step 5: Confirm no look-mode residue remains**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  grep -rn "look mode\|EnterLook\|LeaveLook\|Mode::Normal\|Mode::Look" \
    clients/game --include=*.rs | grep -v target
```

Expected: no output, or only deliberate historical references in prose that
explain what was superseded. **An empty grep needs a positive control** —
confirm the pattern can match by running it against
`campaign/the-portolan`'s tree first.

- [ ] **Step 6: Commit**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  pwd && git branch --show-current && \
  cargo fmt --manifest-path clients/game/bin/Cargo.toml && \
  cargo fmt --manifest-path clients/game/core/Cargo.toml && \
  git add clients/game && \
  git commit -m "refactor(game): remove look mode's residue

Task 1 replaced the routing; this removes what that orphaned. The cursor is
unconditional and there is no mode to enter, so the vocabulary goes with the
bindings. Spec section 4."
```

**STAGE 2 BOUNDARY — absorb main, regenerate, and gate as at Stage 1.**

---

## Stage 3 — close

### Task 5: Chronicle, retrospective, registry, decision, freshness sweep

**Files:**
- Create: `book/src/chronicle/<n>-the-stylus.md`
- Create: `docs/retrospectives/the-stylus.md`
- Create: `docs/decisions/<NNNN>-focus-is-the-clients-one-input-mode.md`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `book/src/SUMMARY.md`
- Modify: stale book chapters found by the sweep

**This task carries a debt Task 0 created.** The merge lands part I's 15
commits as well as The Stylus's, so **part I needs its own chronicle entry
and retrospective too** — otherwise reviewed work lands on main
undocumented. Ledger #6 records this. Write both.

- [ ] **Step 1: Read how the last few campaigns did it**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  ls book/src/chronicle/ | tail -5 && ls docs/retrospectives/ | tail -5
```

Match their altitude and structure. Book prose is technical and
mathematical, comprehensible without reading the code it may show. **Book
chapter titles are code-generated** — check how `SUMMARY.md` and the title
relate before hand-writing either.

- [ ] **Step 2: Write the decision record**

Promote spec §11. The decision to ratify:

> **Focus is the client's one input mode, and it is shown.** A key's meaning
> depends on which pane has focus and on nothing else — no modifiers, no
> chords, no timed sequences — so the routing table is total, predictable
> from the screen, and free of any per-terminal reporting. Text is the
> default destination: a client that cannot be typed into has an ornament
> where its command line should be.

Number it against the **merged tree**, not your branch:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  git fetch origin && ls docs/decisions/ | tail -3 && \
  git ls-tree --name-only origin/main docs/decisions/ | tail -3
```

Holes are legal (the contiguity gate was dropped `81809fe5`); first to merge
keeps the number. Record what it supersedes: part I's `Mode { Normal, Look }`.

- [ ] **Step 3: Write both chronicle entries**

One for The Stylus, one for The Portolan part I. Part I's should say plainly
that its look mode was superseded by the campaign that landed alongside it,
and that part II is spec'd, planned and not started.

- [ ] **Step 4: Write both retrospectives — LAST, and honestly**

Process lessons, not product. The Stylus's has real material:

- **The spec was written against a tree its branch did not have.** Three
  spec claims were unrunnable as written, and the plan found it by running
  `git merge-base --is-ancestor`, not by re-reading. Ledger #6.
- **The old sweep was green while the client could not be typed into.** A
  test asserting the wrong property is not a weak test; it is a confident
  one. Task 1 Step 8 captured that green deliberately.
- **`make gate-commit` compiles none of this campaign.** `clients/` is
  outside the workspace; `make game-check` is the gate that matters.
- Whatever the mutation probes in Tasks 2 and 3 actually showed.

- [ ] **Step 5: Update the idea registry**

`$@` cursor substitution, tab completion, and persisted history are spec §9
carry-forwards. Add or update rows for each. Registry IDs named in DoD prose
fail `cli/tests/docs_consistency.rs` if they do not resolve — check before
committing.

- [ ] **Step 6: Freshness sweep**

The book may never lag merged reality. Find chapters describing the client's
input model:

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  grep -rln "look mode\|verb_for\|command line\|prompt" book/src/ | head -20
```

If this campaign moved a bet in `book/src/open-questions.md`'s Confidence
Gradient, re-score that chapter (decision 0030).

- [ ] **Step 7: Regenerate, gate, commit**

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make rebaseline && \
  git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

`docs/digest/` WILL move — the in-force decision index drifts whenever a
decision record is added. `git add` it in the same commit.

```bash
cd /Users/nathan/Projects/hornvale/hornvale/.claude/worktrees/the-stylus && \
  make game-check && make gate-commit && \
  mdbook build book
```

Expected: all PASS.

- [ ] **Step 8: STOP — G6 is Nathan's**

Do not submit `make sluice`. Report to the controller, who presents the G6
package. The merge is a hard stop under campaign-autopilot, and this one
lands two campaigns' work rather than one.

---

## Self-review against the spec

Run before dispatching Task 1.

**Spec coverage:**

| spec | task |
|---|---|
| §1 buffer, insertion, deletion, `Enter`, history, line reaches `Session::handle` | 1, 3 |
| §1 focus toggled by `Esc` | 1 |
| §1 the routing table, no modifiers | 1 |
| §1 removal of look mode | 1 (routing), 4 (residue) |
| §2 the table itself | 1, extended to totality per ledger #7 |
| §2.1 focus is visible | 2 |
| §3.3 `Tab` bound to nothing | 1, its own test |
| §6 `Enter` on empty costs no turn | 3 |
| §6 the submitted line is echoed | 3 |
| §7 F1 `Esc` reads reliably / map reachable | 1 Step 11 |
| §7 F2 how focus is shown | 2, decided in the task header |
| §7 F3 where the buffer lives | 1 Step 11 |
| §7 F4 history across release | 3 Step 10 |
| §8 H1 keyspace sweep | 1 Step 7 |
| §8 H2 typed command reaches the sim | 3 Step 5 |
| §8 H3 focus total and visible | 1 (total), 2 (visible) |
| §8 the whole-line test the sweep cannot replace | 3 Step 5 + Step 9 mutation |
| §9 carry-forwards | 5 Step 5 |
| §11 decision to promote | 5 Step 2 |
| §12 stage boundaries: absorb + regenerate | after Tasks 2 and 4 |

**Gaps closed beyond the spec, each ledgered:**
- Task 0 exists because the spec's base was not this branch (#6).
- The routing table is total, not six columns (#7).
- Release detection moves off the sent string (#8) — verified against
  `session.rs:1367`, not predicted.
- F1 is about map *reachability*, not `Esc` parsing (#9).
- "Unconditional cursor" vs "focus is shown" is resolved (#10).
- `Q` no longer releases; the exit path is now load-bearing (#11).
- The map cursor loses its diagonals (#12).

**Type consistency:** `Focus` and `CommandLine` are defined in `core` and
imported by `bin`. `Line::text()` returns an owned `String` throughout.
`Driver::handle` and `Driver::apply` both return `bool` meaning "released",
introduced together in Task 3. `entry::draw` takes `focus` and `line` from
Task 2 and gains `echo` in Task 3 — Task 3's signature is the final one.

**Placeholders:** none. Every code step carries the code; every step whose
outcome is not knowable in advance carries a decision rule instead of a
prediction.
