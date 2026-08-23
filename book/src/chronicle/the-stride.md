# The Stride

A stride is the unit a body moves in when it is not thinking about moving.
`clients/game` could be typed at, thanks to [The Stylus](./the-stylus.md),
and pointed at, thanks to [The Portolan](./the-portolan.md), but it could
not be *walked* in: taking a step north meant typing `n`, `o`, `r`, `t`, `h`
and Enter, or `g`, `o`, space, `n` — six keypresses and a submission for the
single most common act in the game. This campaign gives it a stride.

## The gap was made by a fix, not left by an omission

At [The Quire](./the-quire.md) the client had movement on arrows, numpad and
vi-keys, and no free-text entry at all — the keyboard reached seven of the
roughly twenty-five behaviours `Session::handle` answers, and that chronicle
named the shortfall as the branch's most player-visible gap. The Stylus
closed it by making the routing table type a letter by default, which is the
only way a keyboard reaches twenty-five verbs. But a table where almost
every key types a letter has no letters left over, and the arrows had to go
somewhere: they became caret and history motion. The Portolan's own
"Superseded before it merged" section records the same squeeze from the map
cursor's side, where the diagonals `yubn` were lost for exactly this reason.

So the capability was not missing. It was *spent*, deliberately, to buy
something worth more, and the bill came due as soon as anyone tried to play.
The fix is not to unbind letters — the keyboard really is full — but to add
a third state in which the arrows mean what they meant at The Quire.

## Three modes, and the one that is the game

`Focus` grows from `{ Cli, Map }` to `{ Walk, Cli, Map }`, and `Walk` is the
default: the client now starts able to play rather than able to type. Arrows
send `north`/`south`/`west`/`east` and `<`/`>` send `up`/`down`, each routed
through the same path a submitted line takes — pushed onto history so it
recalls, echoed so the transcript still reads as ask-then-answer, and handed
to `Session::handle` unvalidated, so an illegal step comes back as the sim's
own prose rather than a client-side refusal.

Any other printable key bounces to the CLI and types itself, which is the
one-keypress convention the map focus already used, extended to the third
state rather than invented for it. The result is that a player never chooses
a mode in order to type: they type, and the mode follows. `Esc` cycles
Walk↔Cli; from the map it returns to Walk, because the map is a place you
look from and walking is the place you return to.

Nothing renders differently. Walk claims neither the caret nor the map
cursor — the terminal has exactly one hardware cursor and Walk consults
neither pane's, which is a third answer to a question `render_with` had only
ever needed two answers for.

## A predicate with two readings, and the fact that chose between them

The map focus is entered by submitting `map`. The design said so twice, in
consecutive sentences, and the two sentences did not agree: one described
splitting off the **first token** and comparing it to `map`, the other an
**exact whole-line match after trim**. They differ on `map out 2`, which has
`map` as its first token and is not the whole line. The implementation took
the first reading and the test written beside it took the second, in the
same commit, and the suite went red — the cheapest possible outcome, and one
worth recording precisely because a prose ambiguity of this shape usually
ships instead.

The tie is not broken by preference. `Session::map` takes `&self` and
returns prose, and the client's map plate is redrawn from the snapshot's
`Spatial` channel every turn regardless of what was typed. So **no argument
form of `map` can move the plate at all** — the intuition behind the
first-token reading ("it drew a chart, so focus it") describes something
that does not happen. Submitting `map` in this client is a mode gesture, not
a fetch, and focusing after `map out 2` would hand the player a cursor on an
unzoomed plate they never asked about. Bare-only is correct, and the sim
already draws that exact distinction: `Session::handle` guards its own `map`
and `eyes` arms on `rest.is_empty()`, so the client mirrors an existing
convention rather than minting one.

That `map out N` zooms the sim's text and not the client's plate is not a
defect this campaign introduced: it is a
[registered frontier idea](../frontier/idea-registry.md) carried since The
Grain — the snapshot channel builds its chart at a hardcoded zoom while the
`map` verb honours the argument — seen from the client's side here for the
first time.

The mode model is ratified as decision 0160, which **amends** decision 0159
rather than superseding it: 0159's rule — a key's meaning depends on focus
and nothing else, the table is total, text is the default destination — is
untouched, and only its two-state enumeration needed restating. The
substantive change buried in that restatement is that **`Esc` is no longer a
route to the map**. 0159 had named `Esc` the sole route in and reserved a
fallback binding against the risk of a single point of failure; entry now
depends on the command line instead, which is the client's most robust
surface rather than its most fragile, so the reserved binding stays unspent.

## What shipped, concretely

`Focus::Walk` as the default in `hornvale-game-core`, with `render_with`'s
cursor match given a third arm that reports no cursor. `Action::Move(&'static
str)` and a three-way routing table in the binary, with totality preserved:
every key has a defined destination in all three states, chords stay inert,
release events stay inert. A driver that walks at start, executes moves
through the submit path, and has exactly one door into the map focus —
`enter_map`, called only from a bare `map` submission, which is why `Esc`'s
own transition can clear the map strip unconditionally instead of testing
for a state it can no longer reach.

## What is not built

**Diagonals and a wait key** — the game is designed for laptops without
numpads, and no content requires diagonal movement. **Any refinement of the
map focus itself**, zoom included: `Action::Zoom` is bound and remains a
no-op in the driver, exactly as it was. **A visual affordance for Walk** —
the client draws identically in Walk and Cli, so the only signal that the
arrows will move you is that they move you.
