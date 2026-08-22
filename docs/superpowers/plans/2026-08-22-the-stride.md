# The Stride — Walk Input Mode Implementation Plan

> **REQUIRED SUB-SKILL:** Use the executing-plans skill to implement this plan task-by-task.

**Goal:** Add a third input mode ("Walk", the new startup default) in which
arrow keys and `<`/`>` send movement commands (`north`/`south`/…/`up`/`down`)
straight to the session; `Esc` cycles Walk↔CLI; `Map` is entered by
submitting `map` and exited by `Esc` back to Walk.

**Architecture:** `Focus` (in `hornvale-game-core`) grows a `Walk` variant
and becomes the default. The bin's routing table (`input.rs`) gains a
three-way match and one new action, `Action::Move(&'static str)`. The driver
(`driver.rs`, still the only session-aware module) executes moves through
the same path `Submit` uses, flips focus on an exact first-token `map`
submission, and replaces the boolean focus flip with an explicit transition
table. Rendering needs no visual change: Walk shows neither caret nor map
cursor.

**Tech Stack:** Rust 2024, crossterm; two crates outside the cargo
workspace (`clients/game/core`, `clients/game/bin`). Tests are plain
`cargo test` property sweeps already living in-module.

Design: `docs/superpowers/specs/2026-08-22-the-stride-design.md`

---

### Task 1: `Focus::Walk` in hornvale-game-core

**TDD scenario:** New feature — full TDD cycle. Small surface, but the
exhaustive matches this change breaks are the point.

**Files:**
- Modify: `clients/game/core/src/lib.rs` (the `Focus` enum ~line 76, and
  `render_with`'s cursor `match` ~line 166)

**Step 1: Write the failing test**

Add to the existing `mod tests` at the bottom of `clients/game/core/src/lib.rs`
(merge with whatever import style is there):

```rust
/// Walk is the DEFAULT focus — the game starts walking, not typing
/// (The Stride design, "Mode model").
#[test]
fn walk_is_the_default_focus() {
    assert_eq!(Focus::default(), Focus::Walk);
}
```

**Step 2: Run test to verify it fails**

```bash
cargo test --manifest-path clients/game/core/Cargo.toml walk_is_the_default
```
Expected: FAIL — no variant `Walk`.

**Step 3: Implement**

In the `Focus` enum: add the variant, move `#[default]` to it, rewrite the
doc comment (the current one says "the client's one input mode" and "two
panes" — update honestly):

```rust
/// Which pane a key press is addressed to.
///
/// Three modes. [`Focus::Walk`] is the default and the player's primary:
/// arrows and `<`/`>` are movement commands sent to the session, any other
/// printable key bounces to the CLI and types itself. [`Focus::Cli`] is
/// the command line; [`Focus::Map`] drives the map cursor. `Esc` cycles
/// Walk↔Cli and returns Map→Walk (the transition lives in the binary's
/// driver — this enum only names the states).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Focus {
    /// Movement commands go straight to the session; printable keys bounce
    /// to the CLI and type themselves.
    #[default]
    Walk,
    /// The command line is listening. Text is the default destination.
    Cli,
    /// The map is listening: arrows drive the map cursor, `-`/`+`/`=` zoom.
    Map,
}
```

`render_with`'s exhaustive cursor match (~line 166) will now FAIL TO
COMPILE — that is the guard working. Give Walk its own arm. Walk owns
neither hardware-cursor destination: the buffer is not being edited
(printable keys bounce away), and the map cursor is not driving:

```rust
let cursor = match focus {
    Focus::Cli => caret,
    Focus::Map => map_cursor.map(|c| (c.x, c.y)),
    // Walk drives neither pane's cursor: the buffer is not being edited
    // (printable keys bounce to the CLI), so the caret is not reported;
    // the map cursor is not moving either. Deliberately NEITHER — the
    // "never both, never neither" rule above is about ambiguity between
    // the two pane cursors, and Walk claims no cursor at all.
    Focus::Walk => None,
};
```

Update the `render_with` doc paragraph that says "Exactly one of the two is
ever consulted per call" to name Walk as the exception that consults
neither.

**Step 4: Run tests**

```bash
cargo test --manifest-path clients/game/core/Cargo.toml
```
Expected: PASS. If other core tests pattern-match on `Focus`, fix their
matches the same way — never with `_`.

**Step 5: Commit**

```bash
git add clients/game/core/src/lib.rs
git commit --no-verify -m "feat(game-core): Focus::Walk, the new default"
```

---

### Task 2: `Action::Move` and the three-way routing table

**TDD scenario:** Modifying tested code — write the new tests first, watch
them fail against the unchanged table.

**Files:**
- Modify: `clients/game/bin/src/input.rs`

**Step 1: Write the failing tests**

In `input.rs`'s test module, add:

```rust
/// The six movement bindings under Walk (The Stride design, key table).
/// `<`/`>` arrive SHIFTed — they are the only shifted characters that do
/// anything but type.
#[test]
fn the_movement_keys_send_move_actions_when_walking() {
    let cases = [
        (KeyCode::Up, Action::Move("north")),
        (KeyCode::Down, Action::Move("south")),
        (KeyCode::Left, Action::Move("west")),
        (KeyCode::Right, Action::Move("east")),
        (
            KeyEvent::new(KeyCode::Char('<'), KeyModifiers::SHIFT),
            Action::Move("up"),
        ),
        (
            KeyEvent::new(KeyCode::Char('>'), KeyModifiers::SHIFT),
            Action::Move("down"),
        ),
    ];
    for (key, want) in cases {
        assert_eq!(action_for(key, Focus::Walk), want, "{key:?}");
    }
}

/// Every OTHER printable character bounces to the CLI and types itself
/// under Walk — the same one-keypress convention Map uses, extended to the
/// third focus. `<`/`>` are the deliberate exceptions (tested above).
#[test]
fn a_printable_character_bounces_from_walk_too() {
    let mut wrong = Vec::new();
    for b in 0x20u8..=0x7Eu8 {
        let c = b as char;
        if matches!(c, '<' | '>') {
            continue;
        }
        for m in [KeyModifiers::NONE, KeyModifiers::SHIFT] {
            let key = KeyEvent::new(KeyCode::Char(c), m);
            if !matches!(action_for(key, Focus::Walk), Action::FocusAndType(got) if got == c)
            {
                wrong.push((c, format!("{m:?}")));
            }
        }
    }
    assert!(wrong.is_empty(), "did not bounce-and-type: {wrong:?}");
}

/// Totality's Walk column: every non-printable key has a defined
/// destination, and Esc toggles.
#[test]
fn the_named_keys_route_predictably_when_walking() {
    let cases = [
        (KeyCode::Esc, Action::ToggleFocus),
        (KeyCode::Tab, Action::None),
        (KeyCode::Enter, Action::None),
        (KeyCode::Backspace, Action::None),
    ];
    for (code, want) in cases {
        let key = KeyEvent::new(code, KeyModifiers::NONE);
        assert_eq!(action_for(key, Focus::Walk), want, "{code:?}");
    }
}

/// Chords stay inert in the third state too, and a release event never
/// acts under Walk.
#[test]
fn chords_and_releases_stay_inert_when_walking() {
    for code in [KeyCode::Char('l'), KeyCode::Left] {
        for m in [KeyModifiers::CONTROL, KeyModifiers::ALT] {
            assert_eq!(
                action_for(KeyEvent::new(code, m), Focus::Walk),
                Action::None,
                "{code:?} with {m:?}"
            );
        }
    }
    let mut key = KeyEvent::new(KeyCode::Up, KeyModifiers::NONE);
    key.kind = KeyEventKind::Release;
    assert_eq!(action_for(key, Focus::Walk), Action::None);
}
```

Also EXTEND two existing sweeps to cover all three foci (they currently
loop `[Focus::Cli, Focus::Map]`; add `Focus::Walk` to the loop):
`a_chord_is_inert_in_either_focus` (rename: `..._in_any_focus`) and
`a_release_event_is_inert_in_either_focus` (same rename).

**Step 2: Run tests to verify they fail**

```bash
cargo test --manifest-path clients/game/bin/Cargo.toml input::
```
Expected: FAIL — no variant `Walk`, no variant `Move`.

**Step 3: Implement**

Add to `Action` (with doc comment):

```rust
/// Send this movement word (`"north"`, `"up"`, …) to the session as if
/// submitted. Produced only under [`Focus::Walk`]; executed by the
/// driver's `apply`, which routes it through the same path `Submit` uses.
Move(&'static str),
```

Rewrite `action_for`'s body: keep the kind/modifier guards verbatim, then
match on three arms. The `Cli` arm is byte-for-byte today's. The `Map` arm
is byte-for-byte today's. The new arm:

```rust
Focus::Walk => match key.code {
    KeyCode::Up => Action::Move("north"),
    KeyCode::Down => Action::Move("south"),
    KeyCode::Left => Action::Move("west"),
    KeyCode::Right => Action::Move("east"),
    KeyCode::Char('<') => Action::Move("up"),
    KeyCode::Char('>') => Action::Move("down"),
    KeyCode::Esc => Action::ToggleFocus,
    KeyCode::Char(c) => Action::FocusAndType(c),
    _ => Action::None,
},
```

Update module docs that describe "two focus states"/"each focus state" to
say three, naming Walk as primary.

**Step 4: Run tests**

```bash
cargo test --manifest-path clients/game/bin/Cargo.toml input::
```
Expected: PASS.

**Step 5: Commit**

```bash
git add clients/game/bin/src/input.rs
git commit --no-verify -m "feat(game): Action::Move and the Walk routing column"
```

---

### Task 3: Driver — transitions, movement execution, `map` entry

**TDD scenario:** Modifying tested code — tests first.

**Files:**
- Modify: `clients/game/bin/src/driver.rs`

Driver unit tests construct a `Driver` via `Driver::start(seed, target)` —
follow the existing patterns in the file's test modules for how sessions
are built in tests (grep `Driver::start` / existing `mod.*tests` in the
file first; mirror whichever fixture idiom is there).

**Step 1: Write the failing tests**

```rust
/// Walk is the startup focus (The Stride).
#[test]
fn the_game_starts_walking() {
    let d = /* existing Driver fixture idiom */;
    assert_eq!(d.focus(), Focus::Walk);
}

/// Esc transitions: Walk→Cli→Walk, and Map lands on Walk — "back to
/// playing".
#[test]
fn esc_cycles_walk_and_cli_and_map_returns_to_walk() {
    let mut d = fixture();
    d.toggle_focus();
    assert_eq!(d.focus(), Focus::Cli);
    d.toggle_focus();
    assert_eq!(d.focus(), Focus::Walk);
    // Enter Map by submitting `map`, then Esc home.
    assert!(!d.apply(Action::Submit.with_line("map"))); // see Step 3 note
    assert_eq!(d.focus(), Focus::Map);
    d.toggle_focus();
    assert_eq!(d.focus(), Focus::Walk);
}

/// A Move action sends the direction word through history/echo/handle —
/// the same path Submit takes — without touching the buffer.
#[test]
fn a_move_action_submits_the_direction_word() {
    let mut d = fixture();
    d.apply(Action::Move("north"));
    assert_eq!(d.echo(), Some("north"));
    assert_eq!(d.history_last(), Some("north")); // add a #[cfg(test)] accessor if none exists
    assert!(d.line_text().is_empty());
}
```

If no test fixture exists yet, build one: `Driver::start(42,
PossessTarget::default())` unwrapped however the crate's other tests do (or
skip driver-level tests that need a world only if building one is
prohibitive — but prefer real: genesis on seed 42 is seconds-scale here).

**Step 2: Run to verify failure**, then implement:

1. **Startup:** `focus: Focus::Cli` → `Focus::Walk` in `start`'s struct
   literal.
2. **Transitions:** replace `toggle_focus`'s boolean flip with the explicit
   table; entering Map resolves the strip, leaving Map clears it (keep the
   existing `refresh_strip`/`strip = None` logic keyed off the NEW focus):

```rust
pub fn toggle_focus(&mut self) {
    self.focus = match self.focus {
        Focus::Walk => Focus::Cli,
        Focus::Cli => Focus::Walk,
        Focus::Map => Focus::Walk,
    };
    if self.focus == Focus::Map {
        self.refresh_strip();
    } else {
        self.strip = None;
    }
}
```

   Note: `FocusAndType`'s `apply` arm routes through `toggle_focus()` and
   relies on "toggle always lands on Cli when produced from Map". That is
   now FALSE twice over: from Map, toggle lands on **Walk**. Fix
   `FocusAndType` to land on `Cli` explicitly (set `self.focus =
   Focus::Cli; self.strip = None;` — or factor a private
   `leave_map_for_cli()`), and update its doc comment. This is the subtlest
   change in the plan; the existing invariant comment in that arm explains
   why direct assignment was rejected before — carry that reasoning
   forward rather than deleting it.

3. **Movement:** in `apply`, add the arm BEFORE `Action::Submit`:

```rust
Action::Move(dir) => {
    let taken = dir.to_string();
    self.history.push(taken.clone());
    self.echo = Some(taken.clone());
    self.handle(&taken)
}
```

4. **`map` entry:** in `apply`'s `Submit` arm, after `self.handle(&taken)`
   returns `released`, check the verb:

```rust
Action::Submit => {
    if self.line.is_empty() {
        return false;
    }
    let taken = self.line.take();
    self.history.push(taken.clone());
    self.echo = Some(taken.clone());
    let released = self.handle(&taken);
    // Mirror of Session::handle's own first-token convention: submitting
    // exactly `map` opens the map pane. Exact-after-trim: `map x` and
    // `examine map` do not.
    if taken.trim().split_once(' ').map_or(taken.trim(), |(v, _)| v) == "map" {
        if self.focus != Focus::Map {
            self.toggle_focus(); // unreachable-from-Cli/Walk today; kept total anyway
        }
    }
    released
}
```

   Wait — `toggle_focus` from Cli lands on Walk, not Map. Do NOT reuse it:
   set focus directly with the strip discipline factored out:

```rust
fn enter_map(&mut self) {
    self.focus = Focus::Map;
    self.refresh_strip();
}
```

   and call `enter_map()` from the Submit arm. Keep `toggle_focus` for the
   Esc keys only.

5. `cursor()`/`strip_text()` already gate on `== Focus::Map` — verify, no
   change. Update `apply`'s doc comment (it enumerates actions) and the
   module doc's two-mode language.

**Step 3: Run tests**

```bash
cargo test --manifest-path clients/game/bin/Cargo.toml
```
Expected: PASS. Existing driver tests asserting `focus == Cli` after start
or after toggle must be updated to the new contract — update them toward
the design, never weaken.

**Step 4: Commit**

```bash
git add clients/game/bin/src/driver.rs
git commit --no-verify -m "feat(game): driver executes moves, walks at start, enters map on \`map\`"
```

---

### Task 4: Sweep, fmt, gate

**Files:** whatever the compiler points at (likely none beyond Tasks 1–3).

**Step 1:** Grep for stale two-mode language:
```bash
rg -n "two panes|either focus|both focus|ToggleFocus" clients/game/bin/src clients/game/core/src
```
Fix prose, not code semantics.

**Step 2:** fmt + full local suites (clients are OUTSIDE the workspace —
use manifest paths):
```bash
cargo fmt --manifest-path clients/game/bin/Cargo.toml
cargo fmt --manifest-path clients/game/core/Cargo.toml
cargo test --manifest-path clients/game/core/Cargo.toml
cargo test --manifest-path clients/game/bin/Cargo.toml
cargo clippy --manifest-path clients/game/bin/Cargo.toml --all-targets -- -D warnings
```
Expected: all green.

**Step 3:** Manual smoke (needs a terminal): `cargo run --manifest-path
clients/game/bin/Cargo.toml -- possess --seed 42` — confirm: starts able to
walk with arrows; `<`/`>` climb/descend where exits exist; typing a letter
lands in the command line; `map` focuses the pane; `Esc` returns to
walking; illegal moves come back as sim prose.

**Step 4: Commit anything fmt touched, then hand off to the campaign's
normal close (stage gate via the sluice — NOT a direct push).**
