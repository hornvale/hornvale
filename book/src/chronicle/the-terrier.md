# The Terrier

*A terrier is an estate's register of who holds which land — walked once,
consulted many times. Hornvale keeps one too: every occupation the history
bake ever committed, grouped by the vertex it stands on. The vessel had been
re-surveying the whole estate every time it asked who held one plot.*

## The wrong noun

[The Rack](./the-rack.md) had made a chamber turn's per-turn sighting memo
legible enough to measure, and what it measured was a difference: a chamber
snapshot cost 8.4 ms after `look`, where the sighting memo already had an
answer to share, and 16.3–16.8 ms after `map` or a chamber `go`, where it did
not and had to derive one. Eight milliseconds, attributed to the thing that
difference seemed to name — one shadowcast — and written into a chronicle, a
retrospective, and a registry row as exactly that.

It was the whole derivation, not one step of it, and the step it was named
after was not the expensive one.

Ten scratch `Instant` prints inside `derive_sighting`, `chamber_plan`,
`describe_chamber_here`, `enter` and `brief_of`, taken on the same release
build, the same seed-42 flagship, and a visibly contended box (`uptime` load
averages 28.07/25.58/22.66 on the first run, 51.01/34.28/26.41 on the second
— every figure below is an upper bound, and the *split* between them is the
finding, not their absolute size), decomposed the difference completely:

```text
step inside derive_sighting                       per call
  chamber_interior_here  (= one brief_here)     8.7 – 12.6 ms
  anchor_cells                                  0.086 – 0.091 ms
  shadowcast (SIGHT_RADIUS = 4, ≤ 81 cells)     0.011 – 0.013 ms
  occupancy seat + interior_of                  0.005 – 0.006 ms
  placement loop + furnishings                  0.009 – 0.013 ms

step inside chamber_plan
  fabric_here                                   0.003 – 0.004 ms
  chamber_sources        (= one brief_here)     8.8 – 17.6 ms
  light_field                                   0.060 – 0.896 ms
  plan_of (+ ambient)                           0.119 – 1.023 ms

inside brief_of                     n = 42 calls in the run
  is_built                                      mean 0.000 ms
  is_cold                                       mean 0.003 ms
  containing_vertex                             mean 0.003 ms
  occupations_by_vertex (452 vertices)          mean 11.421 ms, max 26.048 ms
```

The shadowcast is a hundredth of a millisecond. Every other row worth naming
is `brief_here`, and every `brief_here` bottoms out in
`hornvale_worldgen::occupations_by_vertex(world)` — a function that
reconstructs **every occupation the history bake ever committed**, from a
`find(IS_OCCUPATION)` scan of the ledger plus a `value_of` read and a string
parse per record, then throws away every entry but the one vertex the caller
asked about. `Session::brief_here` has five static call sites, one of them
(`chamber_interior_here`) with seventeen callers of its own; dynamically, an
`enter` from out of doors makes four calls to it in `handle` and one more in
`snapshot`, a chamber `look` makes two and one, and a chamber `map` or `go`
makes zero and two. The function's own comment had prescribed the fix since
the day it was written, 2026-07-27, `4569d883d`: *"If a profile shows it
mattering, hoist the map to the caller… do NOT memoize inside this
function."* A profile finally showed it mattering, five weeks later, wearing
the wrong name.

## The register, kept once

The fix is the hoist the comment already asked for, placed where every other
world-scoped, derived-once, session-shared read already lives:
`WorldContext` (The Quire) gains one field — the occupation register,
`BTreeMap<Vertex, Vec<OccupationRecord>>` — built once at the end of
`WorldContext::build`, after the five seeded derivations that precede it, so
a reader who does not already know cannot mistake a ledger read for a sixth
stream-consuming one. `brief::brief_of` stops taking `&World` and starts
taking `&BTreeMap<Vertex, Vec<OccupationRecord>>`; its one production caller
passes `&session.wctx.occupations`. The whole-world scan moves from
*every call* to *once per world* — roughly nine to twenty-six milliseconds
against a multi-second build, paid exactly once.

Nothing about the *shape* of the fix was in doubt once the decomposition
existed. The find was that the campaign the brief opened — attack the
shadowcast — would have shipped a real, green, and almost entirely useless
0.01 ms improvement. See [decision 0636](../../../docs/decisions/0636-a-world-scoped-derivation-lives-on-worldcontext.md)
for the rule this generalizes to, and [the frontier's idea
registry](../frontier/idea-registry.md) — corrected here rather than
replaced — for the number that misled and the one that replaced it.

## The numbers

Read on a quiet MacBookPro at `8b4f7f49065eb69852a9e2cded0d88e50b352a75`,
`uptime` load averages `2.12 2.06 2.07` before and `2.67 2.18 2.11` after —
all three under the project's quiet-box threshold of 4, so none of these
readings is an upper bound the way the decomposition above was:

| line | budget | before | after | verdict |
| --- | ---: | ---: | ---: | --- |
| chamber `snapshot()+json` after `map`/`go n/e/s/w` | ≤ 3 ms | 16.3–16.8 ms | **0.457–0.525 ms** | MET |
| `enter` handle | ≤ 3 ms | 33.747 ms | **0.175–0.178 ms** | MET |
| chamber `look` handle | ≤ 1 ms | 16.540 / 16.125 ms | **0.095–0.114 ms** | MET |
| `Session::start` | ≤ +30 ms | 845 ms | **868 ms (+23 ms)** | MET |

Outdoor rows are the control: 4.06–4.98 ms, against the same ~4.1–4.9 ms
they read before this campaign — unmoved, within noise, because nothing in
the walk band ever called `brief_of` at all. Every chamber row now reads
within a tenth of a millisecond of its post-`look` sibling, which is the
shape predicted once the brief was understood to be the residue rather than
the shadowcast.

The client feels the same fix at the scale a player actually launches:
`clients/game/bin/examples/move_cost.rs`, default (debug) profile, quiet
(`uptime` `2.67 2.18 2.11` before, `2.48 2.15 2.10` after). Every indoor
movement turn — `enter`, chamber `look`, `map`, `go n/e/s/w` — went
**18.7–47.4 ms to 0.61–0.82 ms**, against the 15 ms budget this campaign was
opened to close. Outdoor rows, the control, read 9.27–13.07 ms — the
JSON-and-spatial-channel floor [The Rack](./the-rack.md) measured, unmoved
beyond noise.

## The ratchet, and its direction

A source scan in `windows/vessel` (decision 0636) asserts that production
code under `windows/vessel/src` names `occupations_by_vertex`,
`occupations_at` or `occupation_records` nowhere but inside the body of
`WorldContext::build`. It was witnessed red twice before it shipped green:
once against the pre-hoist tree, where `brief.rs` names the call directly,
and once more by mutating the finished tree's `brief_here` to reintroduce
the call — where the scan caught it at `session.rs:7232`, the observation
the plan's own evidence clause had originally asked for the wrong red
instead of.

**What the scan forbids, and what it cannot show.** It forbids a
whole-world occupation read anywhere on a session path, present tense,
forever. It does not prove the hoisted map is *complete* or *current* — that
a vertex absent from `wctx.occupations` really carries no occupation, or
that one present really is still live. That is a different property, and a
different instrument proves it: for every vertex, and for every locale a
script visits at seed 42 and at seed 7, `brief_of` read over the hoisted
map is asserted equal to `brief_of` read over a freshly built one — asserted
non-vacuously, so at least one visited locale carries a living occupation
and at least one carries none, and a hoist that silently dropped the map or
returned a stale "alive" would fail one side or the other.

**The scan's own reach had to be witnessed, not assumed.** Its first draft
split "production" from "test" code at the first `#[cfg(test)]` attribute
in a file — which, in two files, gates a test-only *helper* sitting in the
middle of otherwise-production code, not the test module itself:
`liveness.rs`'s first `#[cfg(test)]` sits at line 4353 while its real test
module begins at 8491, so roughly 4,100 production lines — `species_activity`
among them — were never looked at; `roster.rs` had the identical shape from
line 237 to 446. A guard that reads green over code it never scanned is
exactly the failure class it exists to close. The fix splits at the test
*module* instead (an attribute followed, across attribute lines, by
`mod `), blanks comment lines rather than dropping them so a cited line
number stays the file's own, and adds a per-file coverage control plus a
synthetic-shape test so the boundary detector's own reach is asserted rather
than assumed.

## What the correction touched, and why in place

The wrong attribution — "the 8 ms is one shadowcast" — had travelled into
seven places before this campaign measured it, and each is corrected loudly
and dated, in place, rather than by quiet edit: the two idea-registry rows it
named (`TOOL-chamber-snapshot-prices-a-shadowcast`, body rewritten and
shipped; `TOOL-tick-profile-2026-08`'s last sentence), two sentences in [The
Rack's chronicle](./the-rack.md) ("The numbers" and "Honest limits"), one
bullet in [The Rack's retrospective](../../../docs/retrospectives/the-rack.md),
and the interpretation half of a Measured block in each of this project's
two `move_cost.rs` benches — the numbers in every one of the seven stand
unedited; only the noun attached to them does not. The reason is the one
this project's own guidance gives for a loud correction over a quiet one: a
record that outlives its subject does not sit inert, it produces wrong
answers from readers who trust it in good faith — which is exactly how this
record was produced in the first place.

## Honest limits

The walk-band's 4.2 ms JSON-and-spatial-channel floor is untouched; nothing
in this campaign is a fold, and there was no fold left to remove here.
`session_cost.rs`'s Mac-keyed wall-clock ceilings are untouched — they were
already upper bounds and stay valid under every one of these readings.
`enter` still makes four separate brief derivations in `handle` and a
chamber turn still makes two; the campaign leaves the count exactly as
found and only removes what each one cost, from single-digit milliseconds
to single-digit microseconds. The register is built once for every
`WorldContext`, which is once per world, at a measured cost of +23 ms
against an 845 ms `Session::start` — paid whether or not any session built
from that context ever calls `brief_of` at all. And a possession that
outlived the `World` it was built against would need the register rebuilt
from a fresh ledger; nothing in this codebase does that today, so the case
is untested rather than handled.
