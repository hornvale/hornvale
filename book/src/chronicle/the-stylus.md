# The Stylus

A stylus is what you write with once a surface stops being merely marked and
starts being *inscribed on* — the tool a wax tablet needs before it is
anything but a slab. `clients/game`'s command line had exactly this problem:
it drew a `>` glyph documented in its own source as "a prompt, not a text
box," and the key loop threw away every keystroke that was not one of a
handful of bound verbs. Typing `look` sent `go e`, from the `l`, and dropped
`o`, `o`, `k` on the floor. This campaign gives the client a stylus.

## The gap was structural, not missing polish

`Driver::handle(&mut self, line: &str) -> String` already existed and already
forwarded to `Session::handle` — the same free-text entry point the REPL
uses. Nothing in `windows/`, `domains/`, or `kernel/` needed to change; the
gap was entirely client-side, and entirely about what happened to a keypress
before it ever reached that function. Of `Session::handle`'s roughly 25
behaviours, only 7 had a key at all (`go`, `enter`, `out`, `wait`, `map`,
`help`, `release`); `eyes`, `whoami`, `knows`, `npcs`, `why`, `needs`,
`provoke`, `soothe`, `consult`, `dive`, `surface` and the rest had no route
to the player whatsoever, because there is no key-per-verb keyboard wide
enough to reach them.

## One discriminator, and the reasons the other three lost

The design question was not "should the client accept text" — obviously it
should — but what should tell a keypress it is text rather than a command to
the map. Three candidates were tried and rejected before landing on `Esc`
toggling **focus**, `Focus { Cli, Map }`:

- A `Ctrl`/`Alt` modifier fails because terminal reporting of these chords is
  unreliable — `Ctrl+-` notoriously so, and `Alt` is frequently swallowed by
  the emulator before the program ever sees it.
- `Tab` works and was very nearly chosen, but it spends the one key every
  terminal program reserves for completion, and this client's noun matching
  is exact-word (`Noun::matches` is `self.words.contains(&w)`) — `examine
  vnga` resolves nothing against `Vngashngatva`. Spending `Tab` today would
  have to be un-spent later.
- Plain arrows discriminated by a `Shift` modifier has no mode at all, which
  sounds like the simplest option until you notice it depends on the
  terminal reporting `Shift+Arrow` distinctly — and fails *silently* when it
  does not: an unsupported terminal moves the map cursor while the player
  believes they are editing text.

`Esc` is plain ASCII, reports identically on every terminal this project has
tested against, and leaves `Tab` free for a future that wants it. Verified
against `crossterm` 0.29's own source (`event/sys/unix/parse.rs:34-42`) that a
lone `0x1B` resolves by checking whether more input is *already available*,
not by waiting on a timeout — so `Esc` costs no wall-clock read, which the
kernel's determinism rules forbid outright.

## The routing table is total, and totality is the property that matters

Under `Focus::Cli`, `←`/`→` move the caret and edit the line for free,
`↑`/`↓` recall history (the unmodified shell convention), and every printable
character is typed. Under `Focus::Map`, `←`/`→`/`↑`/`↓` move the map cursor,
`-`/`+`/`=` are reserved for The Portolan part II's zoom, and any printable
character bounces focus back to `Cli` *and types itself* — so the common
path, typing, costs no extra keypress to reach even from the map. `Esc` is
the sole route from `Cli` to `Map`, which makes it a single point of failure
for half the client; the spec names a fallback binding (`` ` ``) rather than
leaving one to be improvised under failure, though nothing has needed it.

**Focus is shown, and showing it collides with a real constraint.** A
terminal has exactly one hardware cursor, and the row-reservation rule
already governing this client's spread forbids ornament from occupying an
informative cell — so a drawn focus marker was never an option. The signal is
the cursor's *location*: the caret sits in the entry pane under `Cli` focus,
the map cursor's cell under `Map` focus. "The cursor is unconditional" means
its position is always held and always computable — never that it is always
drawn in the same place.

## What this costs Part I

The Portolan part I (landing in this same merge — see its own chronicle
entry) built a free-roaming map cursor toggled by pressing `x`, driven by
arrows, `hjkl`, and the diagonals `yubn`. A total routing table where every
letter defaults to text leaves no letters free for map-cursor motion inside
a mode, so `Mode { Normal, Look }` collapses into `Focus`, `x` becomes an
ordinary typed character, and the map cursor loses its diagonals — reaching
a diagonal cell now costs two keypresses instead of one. Part I's own
mutation-proved keyspace sweep survives the change; it asserts a different
property now (almost every key is text), and its diagonal-`CursorBy`
assertions were deleted along with the bindings they tested, rather than
left asserting a capability the client no longer has.

The player's one way out changes with it. Capital `Q` used to end a session;
under a total routing table `Q` types a `Q`, so the only exit is typing
`release` or `quit` and pressing `Enter`. That moved the exit decision off
the *sent string* main.rs used to compare against, and onto the driver's own
answer — `Driver::handle` now reports whether the turn released, derived
from `Turn::Released` rather than string-matched, which is what makes typing
either synonym reliably end the loop.

## What shipped, concretely

A `Line` buffer with insertion, deletion, and a caret; `History` (a plain
`Vec<String>` plus a walk position counted back from the newest entry);
`Enter` submits a non-empty line, pushes it to history, echoes it on the row
above the command line, and sends it to `Session::handle`; an empty `Enter`
does nothing and costs no turn. The player's live, unsent keystrokes needed
their own place in `hornvale-game-core`'s `Source` provenance enum — neither
"derived from world state" nor "declared inert," the two categories the enum
had claimed were exhaustive — so `Source::Typed` and `Source::Echo` are new,
third-category variants rather than a third reuse of `Source::Chrome`.

## What is not built

**Tab completion** — the key is reserved, unbound. **`$@` cursor
substitution** — it needs the CLI and the cursor both working; this campaign
built the CLI, part I built the cursor, and the campaign that joins them
gets `$@`. **Persisted history across sessions** — history dies with the
process today. All three are carried forward as idea-registry rows rather
than half-built here.
