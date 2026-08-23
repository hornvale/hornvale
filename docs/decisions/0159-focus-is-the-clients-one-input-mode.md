# 0159. Focus is the client's one input mode, and it is shown

**Status:** Accepted (2026-08-20) · **Decider:** Nathan

In the context of a terminal client whose key loop discarded every keystroke
that was not a bound verb — so typing `look` sent `go e` and dropped `ook`,
and only 7 of `Session::handle`'s ~25 behaviours had a key at all — we decided
that **focus is the client's one input mode, and it is shown**: a key's
meaning depends on which pane has focus and on nothing else — no modifiers,
no chords, no timed sequences — so the routing table is total, predictable
from the screen, and free of any per-terminal reporting. Text is the default
destination: a client that cannot be typed into has an ornament where its
command line should be.

## Context

`clients/game`'s `entry.rs` drew a `>` glyph documented in its own source as
"a prompt, not a text box," attributed to `Source::Chrome` (declared inert).
The key loop in `main.rs` ran every keypress through `action_for(key)` and
discarded anything that was not a mapped verb — there was no buffer, no
`Enter`, no `Backspace`, and no way to type free text at all. `Driver::handle`
already forwarded arbitrary text to `Session::handle`, the same entry point
the REPL uses, so the gap was entirely client-side.

This decision **supersedes The Portolan part I's `Mode { Normal, Look }`**,
which was never itself promoted to a decision record. Part I gave the client
a free-roaming map cursor toggled by `x`, with `hjkl` and the diagonals
driving it while in Look mode. The Stylus's routing table makes every letter
default to text, which leaves no keys free for map-cursor motion inside a
mode — so `Mode` collapses into `Focus { Cli, Map }`, `x` becomes an ordinary
typed character, and the map cursor moves on the arrow keys only. Both
campaigns landed in the same merge (Task 0 of The Stylus absorbed part I's
paused branch before any input work, because The Stylus's own spec was
written assuming part I's cursor already existed).

Three discriminators for what should toggle focus were considered and
rejected before `Esc`: a `Ctrl`/`Alt` modifier (terminal reporting of these is
unreliable, and `Alt` is frequently eaten by the emulator); `Tab` (it works,
but spends the universal completion key against a client whose noun matching
is exact-word, so a future `examine <partial-noun>` would need it); and
plain arrows vs. shift+arrows as the discriminator with no explicit mode at
all (fails silently on a terminal that does not report Shift+Arrow
distinctly — the map cursor moves while the player believes they are
editing). `Esc` is plain ASCII, carries no per-terminal reporting risk, and
keeps `Tab` free.

## Consequences

- The routing table is **total**: every `KeyCode` has a defined destination
  in both focus states, not merely a sample of bound verbs. A printable key
  under map focus bounces to CLI focus and types itself, so the common path
  (typing) costs no keypress to reach; `Esc` is the sole route the other way,
  which makes it a single point of failure for reaching the map at all — the
  spec names a fallback binding (`` ` ``) rather than leaving one to be
  designed under failure.
- **The forced consequence, because it is the non-obvious half:** a terminal
  has exactly one hardware cursor. The spread's row-reservation rule forbids
  ornament from occupying an informative cell, so focus cannot be shown by a
  drawn marker — it is shown by the cursor's *location*: the caret in the
  entry pane under CLI focus, the map cursor's cell under map focus. "The
  cursor is unconditional" means its *position* is always held and always
  computable, never that it is always *displayed* in the same place — the
  hardware cursor is never hidden, only somewhere.
- `Q` no longer ends a session (letters are text now); the only exit is
  typing `release` or `quit` and pressing `Enter`, so the driver's own
  answer — not the sent string — must report `Turn::Released` for the exit
  path to work at all. `Ctrl+C` remains inert, unaffected and unrescued: no
  binding in this client depends on any modifier beyond `Shift`.
- Part I's mutation-proved keyspace sweep is kept rather than deleted; it now
  asserts a different property (almost every key is text, not a verb), and
  its diagonal-cursor assertions (`yubn`) are removed along with the bindings
  they tested — a client with no diagonal keys cannot claim diagonal motion.
- `Tab` is spent on nothing, deliberately, reserved for completion (idea
  registry: tab completion). `$@` cursor substitution and persisted history
  are likewise carried forward rather than built here.

## See also

`docs/superpowers/specs/2026-08-20-the-stylus-design.md` §§2, 4, 11 (the
promoted text above is verbatim from §11); `book/src/chronicle/the-stylus.md`;
`book/src/chronicle/the-portolan.md` (part I, landing in the same merge).
