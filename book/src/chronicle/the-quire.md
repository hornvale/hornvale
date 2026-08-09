# The Quire

A quire is the gathering of folded leaves from which a book is bound — the
smallest unit a binder actually handles. This campaign bound the first
gathering of a visual brief that had been written and never built: a native
character-grid client you can walk a Hornvale world around in, at eighty
columns by twenty-four rows, in monochrome.

That sentence is the least interesting thing about the campaign. What is worth
recording is that two of its opening questions turned out to have no answer
because they had no fork, that its headline measurement is a capability rather
than a saving, that its most important safety property is the absence of a
symbol rather than the presence of a rule, and that the one feature it deleted
was deleted because a question dissolved it.

## Two forks that were never forks

The campaign opened with two decisions to make, and neither survived being
looked at.

**"A terminal program, or a real native game?"** The reference point was
chapter four of a well-known Rust roguelike tutorial, offered as the parity
target. The tutorial is not a terminal program. The library under it draws an
eighty-by-fifty character grid into an OpenGL desktop window, and ships
backends for the terminal, for curses, for the GPU, and for the browser. So
"old-school character grid" and "native desktop application" were never
opposites; they are the same option wearing two names. The decision that
actually remained was *which backend to attach first*, and that is small and
reversible precisely because the cell buffer belongs to this project rather
than to the backend library.

**"The game client, or the world viewer?"** There was a long-standing idea for
a separate map-browsing viewer, and it read like a competing product. It is a
cell of the game client's own parameter space. Possession has two independent
parameters — whose senses filter the world (*focalized*), and whose body
executes verbs (*commanded*) — and the viewer is simply the case where
`commanded` is nothing at all. Laid out saturated, the grid names something
real in every cell: `commanded = none` gives the viewer, attract mode, and the
ethnographer's vantage; commanding an existing creature gives free possession;
the off-diagonal gives playing an NPC through its own limited knowledge. This
campaign filled two cells of the middle column and built the *parameter*, which
is what stops the viewer from being built twice.

Recording both here is deliberate. A dissolved fork looks identical to an
unexamined one six months later, and the only difference is whether someone
writes down that it dissolved.

## The cost of a possession, and what the measurement is worth

Starting a possession cost about 0.73 seconds, every time — and that is not
world creation, which is a separate 1.4 seconds. It is the per-possession
price of deriving terrain, climate, the locale context, the coexistence-stack
fit, the demography report and the neighbouring NPCs. Every one of those is
scoped to the *world*, not to the agent being possessed, and all of them were
being recomputed inside `Session::start`.

The requirement was that possession and release should take essentially no
time. That is a refactor, not an optimization: hoist the world-scoped block
into a context that outlives a session, and let a session borrow it.

Measured on a quiet box, release build, five runs, medians, and independently
re-measured afterwards by a second party across three further runs:

| | cold — re-derives the context | warm — borrows a shared context |
|---|---|---|
| median | 694.9 – 755.6 ms | 14.94 – 15.18 ms |

**46.5× to 49.8×.** Three independent runs agreed within eight per cent on the
cold arm and within two per cent on the warm one, which is itself evidence of
a quiet machine — contention produces scatter, not agreement.

The honest caveat travels with the number. **The shipped game never
repossesses.** Its driver starts one session per process and holds it, so this
is a measurement of a capability the architecture now has, not of a cost a
player currently avoids. It becomes a saving the first time something switches
bodies without leaving — free possession, attract mode, a spectator watching a
different creature — and every one of those is a cell of the grid above. It is
a real number about a real mechanism, and it is not yet a number anybody feels.

The mechanism was checked rather than inferred: the cold path calls
`WorldContext::build` on every invocation and owns the result; the warm path
takes a borrowed reference and pays none of it; the benchmark builds its
context once, outside the timed loop; and the complete list of
`WorldContext::build` call sites across the simulation and both clients is four
lines. There is no hidden second derivation propping up the ratio.

## Containment is a missing symbol, not a rule

The client drives across the linker and reads across the serializer. It links
the possession layer to *start* a session and to *hand it a line of text*, and
then every datum it displays comes back as `vessel/session/v1` JSON. This is
not a compromise between the two obvious designs; it is what the browser client
already does, adopted deliberately instead of by accident of the wasm boundary.

It matters because the possession layer's public surface offers a native client
a great deal it must not have — whether an NPC would turn hostile, an NPC's
grievance against you, the agent handle, the knowledge store. A client reading
the typed struct would have all of it one method call away, and the first time
the snapshot lacked a field, the client would quietly reach past the schema
instead of the schema growing to meet it.

So the containment is structural in three ways, none of them a lint:

1. **The crate split.** The rendering crate has no dependency on the simulation
   at all. Those methods are not merely discouraged; there is no symbol to
   reach. A `cargo tree` check in the gate asserts this, and — after review
   demonstrated the guard was blind to a dependency declared optional — it now
   asserts it with all features enabled.
2. **The mirror omits `social`.** The client defines its own deserialization
   types against the wire schema rather than the simulation gaining a derive.
   The schema's `social` channel documents, in its own doc comment, that any
   pane rendering it unfiltered ships a cheat pane — a discipline the schema has
   no way to perform. The mirror simply does not declare the field. Serde
   ignores unknown keys, so the channel never enters the client's address space,
   and the cheat pane cannot be built by accident. **The omission is the
   enforcement.**
3. **The render core is I/O-free.** It takes a string of JSON and returns a grid
   of cells. It does not open a terminal, does not know the terminal library
   exists, and never runs world generation — so every visual assertion in its
   suite is a pure function over a committed fixture, costing microseconds
   rather than the seconds a possession costs.

The third property has a consequence nobody designed for and everybody used: a
rendering change rebuilds two small crates and relinks. Playing the thing
became cheap enough to be a feedback loop rather than a demonstration.

## The chart was geometrically wrong under seventeen green tests

The map plate draws the surrounding lattice, which is not square — it is a
sheared hexagonal embedding with a parity term. The implementation chose a
projection, wrote seventeen tests, and mutation-proved four of them. All green.

It was wrong. Rendered side by side against the simulation's own `map` verb
over the identical thirty-one cells, the simulation drew five dense rows
(5 + 7 + 9 + 7 + 3 = 31) and the client drew nine sparse sheared rows with
holes through them. Both were internally consistent. Only one was what Hornvale
draws.

Nothing executable could have caught it. Every test asserted a property that
holds whether or not the picture is right: the correct number of cells, the
right glyphs, the right weights, `you` in the middle. It was caught because a
report pasted the rendering and somebody compared it against live output. The
repair pinned the client's projection against the simulation's own ASCII render
byte-for-byte — and even that golden had a trap in it, since a backslash
continuation in the Rust source silently eats leading whitespace, which was
found by dumping the string with `od -c` rather than reading it.

The indoor plate got the same treatment pre-emptively and passed it: its golden
matches the simulation's own indoor `map` output byte-for-byte, not merely in
shape.

A second defect of the same family: the key `x` sent a bare `examine` with no
object, so it always answered "Examine what?" — and burned a turn doing it.
That is correct behaviour for an unconditional key-to-verb mapping, and it is
still a real gap. Ten tests were green over it. It was found by playing.

## The ways-on row, and the question that deleted it

The client grew an always-visible row listing the exits from the current place.
It seemed obviously right, and it produced the campaign's longest and most
instructive detour.

First it was built from the wrong source, then from the right one. Then a
review found the real defect: the session's spatial channel splits `walk` from
`chamber`, and `walk` means *not inside a built structure* — so `submerged` and
`underground` both fold into it. Underground, the row rendered the surface
locale's exits. The prose said `Ways on: out.` and the row said
`Ways on: NE, NW, S.`, three bearings that answer a question nobody asked from
down there. A reviewer generated a genuine underground snapshot and rendered it
unmodified to prove it.

The client could not fix this. Nothing on the wire distinguishes the two states
except the literal word "underground" inside the prose, and parsing the prose
for meaning is exactly the boundary this crate exists not to cross. That is the
campaign's clearest finding about the *simulation*: the emitted contract does
not carry the distinction, and a client that renders only the emitted contract
has now proved it insufficient for this element. It remains an open question,
and this campaign is evidence for it rather than a resolution of it.

The proposed fix was to make the simulation emit the exits as a structured
field — additive, cheap, and it would have removed the defect by construction.
It was argued for at length, including an argument that a described opening
("a wooden door which appears nailed shut") is not necessarily a traversable
one, so prose and structure answer different questions and both are wanted.

Then the actual question: *why does the client need to know which directions
the player can go?*

It does not. Command parsing lives in the simulation. The client sends text and
renders the reply; an invalid move comes back as "No way n from here." The key
mapping sends `go <direction>` unconditionally and consults nothing. The only
consumer of the exits was a row that displayed them — and the exits were
already on screen, because the simulation puts `Ways on: …` in the room prose,
which the client renders verbatim.

So the fix was **deletion**, and it took three reimplementations of simulation
knowledge out of the client with it: a filter over compass bearings, an
invariant about how chambers connect, and a dispatch on which band you are in.
The prose had been right in exactly the case the row was wrong, because the
simulation knows its own band and the client never can.

The general form is worth keeping, because the argument that had to be
abandoned was a good argument for a requirement that did not exist: **before
designing a channel to serve a consumer, check that the consumer acts on it.**

One cost was accepted knowingly. The entry pane is a fixed height and drops
overflowing prose behind a `… more, not shown …` marker; with the exits back
inside the prose, a long enough entry could in principle push them off. Neither
committed fixture comes close, and the answer to a rendering problem is a
rendering fix — a scrollable pane — not a duplicated datum. The same truncation
has one live consequence today: `release` is the last line of the simulation's
own help text, so pressing `?` at eighty by twenty-four cannot currently teach a
player how to quit.

## What shipped

`clients/game/`, outside the cargo workspace with its own toolchain and its own
gate, in two crates: a rendering core with no simulation dependency, and a
binary that links the possession layer and owns exactly the terminal. The
spread is a plate on the left, a written entry on the right, an identity strip
below. The outdoor plate draws the sheared lattice; the indoor plate draws the
floor plan; the band switches by itself. Movement is on arrows, numpad and
vi-keys, plus the verbs the session already answers. The three epistemic ink
weights are the whole visual channel in monochrome — bold for *here*, normal
for *sensed*, dim for *remembered*, and never-known ground is not written at
all, which is unmarked paper rather than black fog.

Every visible cell carries a provenance: a test walks the composed grid and
asserts that each non-blank cell traces to a named field of the snapshot. Six
honest source categories survive; a seventh was deleted for naming a datum it
did not describe. There is no `social` category, and there is no way to add
one.

On the simulation side: the world-scoped derivation is hoisted into a shareable
context, and possession takes a target parameter with two values — the minted
flagship, which is the unchanged default, and the first settlement's existing
resident. Both stages were held to byte-identity against every committed
gallery transcript and client fixture, and both met it.

The terminal restores itself on normal exit, on panic, and on `SIGINT`,
`SIGTERM` and `SIGHUP` — proven under a real pseudo-terminal harness, which had
to be fixed first, because the naive version signalled ready before the child
had entered raw mode and would have passed vacuously.

The campaign also repaired a red gate it inherited rather than caused: a
browser-client test whose hard-coded coordinates had gone stale when an earlier
absorption regenerated the fixture underneath it. The distinction mattered
enough to check: re-pinning a *witness* to where the evidence actually is, not
re-pinning a *claim* to match broken behaviour.
