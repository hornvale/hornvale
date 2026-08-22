# The Portolan

A portolan is the kind of chart a navigator worked over with a finger — no
labels painted on the water, a name known by pointing. This campaign gave
`clients/game` a cursor to point with: a free-roaming selector over the
terrain plate, a query from cursor position to the sim's own answer for what
is there, and a status strip beneath the map carrying the most specific
feature's name. Part I is what shipped; part II — fog of discovery, learned
names — is spec'd, planned, and has not started.

**This chronicle is written after the fact, at the close of a different
campaign** ([The Stylus](./the-stylus.md)), which absorbed this branch and
landed it in the same merge. Without this entry, fifteen reviewed commits
would land on `main` with no chapter of their own.

## The first spike measured a design nobody wants

Before any cursor existed, a spike rendered every seed-42 feature label
directly onto an ASCII Mercator projection of the terrain, ungated, at three
widths. It was illegible at all of them — 31.5% of character cells consumed
by text at 72 columns, with 26 labels contending for a single cell, and still
43.5% of labels colliding at 288 columns, already wider than a terminal.
Names ran into each other (`RjarRlerjorji`, `SidxerZherQvadvoshao`). The
spike's own conclusion was that the binding constraint was spatial
clustering — volcanoes follow island arcs, rivers follow drainage — and that
no salience gate alone would fix it.

Every word of that conclusion was true and none of it mattered, because the
premise was wrong. Nathan's correction: labels are not painted on the map at
all. A cursor selects, and the name appears elsewhere. A second spike then
showed the first spike's catastrophe was an artifact of the question it
asked — painting anchors onto a coarse grid piles 26 labels into one cell,
but *asking what is at this cell* returns at most 3 features, and 85% of
cells return exactly 1:

```
0 features:    377 cells (0.92%)
1 feature:  34,851 cells (85.08%)
2 features:  5,697 cells (13.91%)
3 features:     37 cells (0.09%)
```

The lesson recorded at the time, and worth repeating here: a measurement
inherits the question's framing, and a catastrophic number is not
self-interpreting.

## What shipped

A free-roaming cursor, rendered as the terminal's own blinking hardware
underline, moved by key at every band the plate draws. A resolution query
from cursor position to the sim's answer for what occupies that cell. A
status strip of plate width beneath the map, carrying the most specific
feature's resolved name, recomputed as the cursor moves. `CellId →
Vec<FeatureId>`, a load-time index built once when the world loads rather
than searched per keystroke. Declared per-class salience on `FeatureClass`,
so the strip prefers the most specific match rather than the first one found.
The map pane itself draws terrain only — no labels, no gate, no abbreviation
— which is what makes the strip necessary rather than decorative.

Cursor motion originally answered to arrows, `hjkl`, and the diagonal keys
`yubn` (`Action::CursorBy(±1, ±1)`), toggled into a `Mode { Normal, Look }`
by pressing `x`.

## Superseded before it merged

That `Mode` and that keyset did not survive contact with the campaign that
absorbed this branch. [The Stylus](./the-stylus.md)'s routing table makes
almost every key type a letter by default, which leaves no letters free for
map-cursor motion inside a mode — so `Mode { Normal, Look }` collapsed into
`Focus { Cli, Map }`, toggled by `Esc` rather than `x`, and the cursor lost
its diagonals: reaching a diagonal cell now costs two keypresses on the
arrow keys instead of one press of `y`/`u`/`b`/`n`. The cursor's *position*
and the strip's *resolution* — the substance of this campaign — are
unaffected; what changed is only how a player tells the client "I mean the
map, not the command line."

**Superseded again, after it merged.** [The Stride](./the-stride.md) grew
that `Focus` a third state — `{ Walk, Cli, Map }`, with `Walk` the default —
so `Esc` no longer toggles between two panes: it cycles Walk↔Cli, and from
the map it returns to Walk. The map is now entered by submitting the command
`map` rather than by any key, which means this section's `Esc` is a
departure from the map and never an arrival at it. The cursor's *position*
and the strip's *resolution* are again unaffected.

This campaign's own mutation-proved keyspace sweep survived the change and
now proves a different property (the routing table's totality, not this
mode's own keyset); its diagonal-motion assertions were deleted along with
the bindings, rather than left asserting a capability the client no longer
has.

## A paused branch accrues rule-drift, not just code-drift

This branch sat 50 commits behind `main` while paused, and nothing compiled
it against `main`'s current state until The Stylus's Task 0 absorbed it.
Once absorbed, it was green on the client's own gate (`make game-check`) —
but it tripped a test-binary consolidation ratchet
(`cli/tests/suite/test_binary_ratchet.rs`) that had landed on `main` while
this branch slept, and which this branch's own tests had never been checked
against. The code was fine; the surrounding rule about how tests are allowed
to be organized had moved underneath it. A campaign that pauses mid-flight
owes its resumption an explicit check against whatever `main` grew while it
was gone — being green against the `main` it branched from proves
increasingly little the longer the pause runs.

## What is not built

Part II — fog of discovery (a feature's name withheld until the cursor, or
some in-fiction proxy for it, has actually found it) and the zoom bindings
this campaign's routing table already reserved cells for (`-`/`+`/`=`) — is
spec'd and planned, paused pending The Stylus, and resumes on the foundation
this part leaves behind.
