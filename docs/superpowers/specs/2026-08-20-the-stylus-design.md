# The Stylus — design

**Status:** DRAFT — awaiting G3 review. **Campaign:** The Stylus.
**Branch:** `campaign/the-stylus`.

The terminal client learns to accept typed commands. Focus moves between the
map and the command line with `Esc`; everything else follows from that.

---

## 0. The prompt is a picture of a command line

`clients/game` draws a `>` and cannot be typed into. This is not a bug — it is
a feature that was never built, and the code says so plainly.

`entry.rs`'s own doc: *"The glyph the command line opens with — a prompt, **not
a text box**."* `PROMPT_GLYPH` is attributed `Source::Chrome`: **declared
inert**. It is ornament shaped like an affordance.

And the key loop has no text path at all (`main.rs`):

```rust
let action = input::action_for(key, driver.mode());
if matches!(action, input::Action::None) { continue; }   // typed text dies here
```

Every key is either a verb or discarded. No buffer, no `Enter`, no
`Backspace`. Typing `look` sends `go e` (from `l`) and drops `o`, `o`, `k`.

**The sim is already ready.** `Driver::handle(&mut self, line: &str) -> String`
exists and forwards to `Session::handle`, the same free-text entry point the
REPL uses. **This campaign is entirely client-side**: nothing in `windows/`,
`domains/` or `kernel/` changes, and no schema moves.

---

## 1. What this produces

- **A command line that works**: a buffer, insertion, deletion, `Enter`,
  history, and the typed line reaching `Session::handle`.
- **Focus**, toggled by `Esc`, as the single organising concept.
- **The routing table** below, with no modifier keys anywhere.
- **The removal of look mode**, which this supersedes (§4).

### What it does not produce

- **Tab completion.** `Tab` is *reserved* for it and bound to nothing (§3.3).
- **`$@` cursor substitution.** It needs the CLI and the cursor both working;
  this campaign builds the first. §9.
- **Zoom.** The `-`/`+`/`=` bindings are routed here but zoom itself is The
  Portolan part II's, which is paused pending this campaign.

---

## 2. Focus is the whole design

| focus | `←` `→` | `↑` `↓` | `-` `+` `=` | letters | `Esc` | `Tab` |
|---|---|---|---|---|---|---|
| **CLI** *(default)* | text cursor | **history** | typed | typed | → map | *reserved* |
| **map** | map cursor | map cursor | zoom | **→ CLI, then typed** | → CLI | *reserved* |

Everything falls out of it:

- **`←`/`→` edit the line for free** when focus is on the CLI, so `Home`/`End`
  and `Ctrl+W` become optional polish rather than compensation for a lost
  affordance.
- **`↑`/`↓` have no meaning in a single-line field**, so history lands on the
  shell convention, unmodified, exactly where a user expects it.
- **Typing a letter on the map returns focus to the CLI and types it**, so the
  common path costs no keypress at all. `Esc` is needed only to go *to* the
  map.
- **No modifier keys anywhere.** No `Ctrl`, no `Alt`, no `Shift`
  discrimination — and therefore nothing whose terminal reporting has to be
  verified per-emulator.

### 2.1 Focus must be visible

A mode that does not show itself is a trap — the standing complaint against
vi's insert mode. Focus is different in kind only if it is *shown*, and it is
conventional in every windowing system to show it. The plate indicates focus;
§7 F2 settles how, within the constraint that **ornament may never occupy a
cell that carries information**.

---

## 3. Why these keys

### 3.1 Esc, and the hazard it does not have here

Every terminal escape sequence begins with `0x1B`, so a lone `Esc` is
ambiguous with the *start* of an arrow key. Most readers resolve this with a
**timeout** — which this project cannot have, since wall-clock time is banned
workspace-wide.

**crossterm does not use one.** `event/sys/unix/parse.rs:34-42`:

```rust
b'\x1B' => {
    if buffer.len() == 1 {
        if input_available { Ok(None) }                  // more bytes pending
        else { Ok(Some(... KeyCode::Esc ...)) }          // nothing follows
    }
```

It asks whether more input is *already waiting*, not how long to wait. No
clock, no constitutional conflict.

**The trade-off, recorded rather than hidden:** on a slow or high-latency
terminal a genuine arrow key could have its `0x1B` arrive in one read and its
`[A` in the next, and be misread as a bare `Esc`. That is inherent to the
no-timeout approach, affects every `Esc`-using crossterm application equally,
and is the price of having no timer. §7 F1 says how it is watched.

### 3.2 Arrows, and why not the numpad

Roguelike tradition assumes a numpad; laptops do not have one, and neither do
many modern desktop keyboards. Four arrow keys are sufficient to drive a
cursor and are what a present-day user reaches for. `hjkl` therefore **types
letters**, like every other letter.

### 3.3 Tab is reserved, and this section exists so nobody spends it

`Tab` is bound to **nothing** and must stay that way. It is the universal
completion key, and completion is unusually valuable in this game: noun
matching is **exact-word**, not prefix — `focalize.rs:87-90` is
`self.words.contains(&w)` — so `examine vnga` resolves nothing and a player
must type `Vngashngatva` exactly.

Focus was very nearly bound to `Tab` during design, which would have spent it.
`Esc` was chosen instead **specifically to keep `Tab` free.**

Note the tension this leaves, honestly: the cursor plus `$@` is the *intended*
answer to unpronounceable names — point rather than type — so completion may
never be needed. Reserving `Tab` costs nothing if so, and costs everything to
undo if not.

---

## 4. This supersedes look mode

The Portolan part I shipped `Mode { Normal, Look }`, with `x` entering look
mode and `hjkl`/arrows driving the cursor. Under this design:

- `x` types an `x`. So does `h`.
- **The cursor is unconditional** — always live, always pointable. "Look mode"
  as a concept dissolves; there is a cursor, and there is focus.
- `Mode` becomes **focus**: `Cli` / `Map`.

**This is a revision of shipped, reviewed work, and the spec says so rather
than letting a later reader think it was never considered.** Part I's
mutation-proved keyspace sweep stays the right test — it simply asserts a
different thing now: that almost every key is text.

**Part I's branch is paused, not abandoned** (`campaign/the-portolan`, 15
commits, unmerged). Whoever resumes it reads this spec first.

---

## 5. Refusals

- **No modifier keys.** Not for focus, not for history, not for editing. Every
  binding is a plain key, so nothing depends on per-terminal modifier
  reporting.
- **No timer, no animation loop, no wall-clock.** `Instant` is banned
  workspace-wide. A cursor that blinks does so because the *terminal* blinks
  it (§7 F2's constraint on the hardware cursor), never because this client
  measures time.
- **No schema change and no sim change.** `Driver::handle` already exists.
- **`Tab` stays unbound** (§3.3).
- **The 80×24 floor is inherited and may not be weakened.**
- **No completion, no `$@`** — both named, both deferred (§9).

---

## 6. What a command line owes that a shell's does not

The buffer is **the only reversible thing on screen.** A half-typed command
can be erased; everything else — the turn counter, the accreted prose, the
world — is append-only. The buffer is the last point of reversibility before
an irreversible act.

That raises the stakes on two things a shell would treat casually:

- **`Enter` on an empty line must do nothing**, not advance a turn. A stray
  keypress must not cost the player a turn of the world.
- **The submitted line is echoed into the entry pane** before its answer, so
  the record shows what was *asked*, not only what was answered. The entry
  pane is a journal; a journal that records only replies is not one.

---

## 7. What is unverified, and how each is settled

**F1 — does `Esc` read reliably in practice?** §3.1's analysis says yes with a
known latency trade-off. **Settled by:** Task 1 tests `KeyCode::Esc` handling
directly and states what it observed. **Decision rule:** if a bare `Esc` is
found to be swallowed or delayed on a normal terminal, report it — the
fallback is a plain unshifted key (`` ` `` is free), and switching costs one
binding, not a redesign.

**F2 — how is focus shown, without ornament taking an informative cell?** The
spread's rule is absolute: gutters carry no ink and reserving them means *not
drawing there*. **Settled by:** Task 2 chooses and justifies. The terminal's
hardware cursor is already the campaign's answer to "mark a position without
spending a cell" — showing it in the pane that has focus, and hiding it in the
one that does not, may be the whole mechanism.

**F3 — where does the command buffer live?** `hornvale-game-core` depends on
no hornvale crate and must not start; a buffer is client state, not wire
state. **Settled by:** Task 1 states whether the buffer lives in `core`
(rendered, but owned) or `bin` (owned, handed over for rendering), with the
reason. **If it reaches into `Snapshot`, that is a STOP.**

**F4 — does history survive a `release`?** Out of scope to persist, but the
behaviour should be stated rather than accidental. **Settled by:** Task 3 says
what it does.

---

## 8. Preregistered measurement

**H1 — every non-reserved key reaches the buffer.** Sweep the printable-ASCII
keyspace: with focus on the CLI, every printable character appends itself, and
**no key silently does something else.** *Falsified by* any key that is
neither typed nor explicitly reserved — which is the current bug's exact
shape, and the sweep is the test part I already proved catches it.

**H2 — a typed command reaches the sim and its answer returns.** Type `look`,
press `Enter`, and the entry pane shows the same prose `Session::handle("look")`
produces. *Falsified by* divergence, which would mean the client is
interpreting rather than transmitting.

**H3 — focus is total and visible.** At every moment exactly one pane has
focus, the screen shows which, and no key is ambiguous between them.
*Falsified by* a state where a key's destination cannot be predicted from what
is on screen.

**H1 is the one that matters**, because it is the current defect stated as a
property. **And note what it could NOT catch:** it sweeps single keys, so it
cannot see a defect in *sequences* — a buffer that drops every third
character, or mishandles a full line. Task 3 tests a whole typed line for
exactly that reason, and this sentence exists so the sweep's green is not
read as more than it is.

---

## 9. Out of scope, carried forward

- **Tab completion** (§3.3) — the key is reserved for it.
- **`$@` cursor substitution.** Needs the CLI and the cursor both working.
  This campaign builds the CLI; The Portolan built the cursor; the campaign
  that joins them gets `$@`.
- **The Portolan part II** — paused, and resumes on this foundation.
- **Persisted history across sessions.**
- **`Home`/`End`/`Ctrl+W`** — optional polish, unnecessary now that `←`/`→`
  edit the line (§2).

---

## 10. Flagged for Nathan at G3

- **This revises shipped, reviewed work** (§4). Part I's look mode is
  superseded rather than extended, and `x` stops being a command.
- **The `Esc` latency trade-off** (§3.1) is accepted, not solved. The
  alternative is a timer, which is banned.
- **`Tab` is spent on nothing**, deliberately, for a feature that may never be
  needed (§3.3).

---

## 11. Decision to promote

> **Focus is the client's one input mode, and it is shown.** A key's meaning
> depends on which pane has focus and on nothing else — no modifiers, no
> chords, no timed sequences — so the routing table is total, predictable from
> the screen, and free of any per-terminal reporting. Text is the default
> destination: a client that cannot be typed into has an ornament where its
> command line should be.

---

## 12. Task outline

**Stage 1 — the buffer**
1. Focus, the routing table, and the buffer; F1, F3, H1.
2. Rendering the line and showing focus; F2, H3.

**Stage 2 — the loop**
3. `Enter`, echo, history; F4, H2, and the whole-line test §8 names.
4. Remove look mode; re-point part I's keyspace sweep (§4).

**Stage 3 — close**
5. Chronicle, retrospective, registry rows, decision, freshness sweep.
   **Absorb main and regenerate at every stage boundary.** Write the
   retrospective LAST.
