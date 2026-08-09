# The Quire — the native character-grid client, first gathering

**Status**: spec, awaiting review (G3)
**Campaign**: The Quire
**Predecessor**: [The Journal](2026-08-07-the-journal-design.md) — the visual
brief this campaign begins implementing. That campaign produced a brief and no
interface; no design-system output ever came back from it, so **the brief is the
design input**, and its nine acceptance tests are this campaign's visual
criteria.

A quire is the gathering of leaves from which a book is bound. This campaign
binds the first gathering of the Journal's spread.

## 1. What this campaign produces

The first native client you can actually play Hornvale in: a character-grid
roguelike display, driven by a real possession, rendering only what
`vessel/session/v1` emits.

Rough feature parity with chapter 4 of the *Roguelike Tutorial — In Rust* is the
target Nathan set. Worth stating plainly what that means here, because it is
lopsided: chapter 4 builds rooms, corridors, a player placed in the first room,
and movement on arrow/numpad/vi keys. Hornvale's simulation is already many
years past that. **The entire deliverable of this campaign is the display and
the input loop** — plus one structural change on the sim side that the display
makes urgent.

## 2. Scope

> **CORRECTION (added at merge).** Two **In** items shipped smaller than stated.
> The ink weights ship on the grid but **not in the prose** — every prose glyph
> is `Normal`, because the wire carries no salience signal for prose and
> inferring one means reading prose for meaning (decision 0117 forbids it).
> And the keyboard reaches **seven** verbs, not twelve: `go`, `enter`, `out`,
> `wait`, `map`, `help`, `release`. `Session::handle` answers about twenty-five
> behaviours, so "the twelve verbs the session already answers" matched nothing
> even when written. Both shortfalls are recorded in
> [the chronicle](../../../book/src/chronicle/the-quire.md)'s "What shipped".

**In**

- `clients/game/` — a new client, outside the cargo workspace, in two crates.
- The Spread, the Chart Plate, the Plan Plate, the Entry, the Endpaper (brief
  components 7, 8, 9, 14, 15).
- The three epistemic ink weights plus unmarked paper (brief foundation 3), on
  the grid and in the prose.
- Monochrome at 80×24 as the shipping floor (brief legibility law).
- Movement on arrows, numpad and vi-keys, plus the twelve verbs the session
  already answers.
- Hoisting the world-scoped derivation out of `Session::start`, so possession
  and release stop costing a world derivation each.
- A possession *target* parameter on `Session::start`, filling exactly two cells
  of the grid in §7.

**Out** — deferred to named follow-on campaigns, not dropped

- Colour, and with it the temperature × moisture palette surface, the marine
  depth ramp and ordinal relief (brief foundation 2).
- The 22-biome glyph vocabulary (brief foundation 5). The brief calls two glyphs
  for twenty-two biomes "the largest unused capacity in the whole interface" and
  it deserves a campaign, not a corner of this one.
- The Study Plate, the Caption, the Leaf, the Index, the Margin (brief
  components 10, 11, 12, 13, 16).
- The reserved correction vocabulary. The brief is explicit that these marks
  must appear in a specimen sheet and in **no** composed screen; with no
  specimen sheet in scope, they appear nowhere.
- Tiles and the sprite counterpart. The cell law is *honoured* here — layout
  never assumes characters — but no tile backend is built.
- Free-roaming creature selection, and the `commanded = NONE` cells of §7's
  grid.
- Any windowed or remote backend.

## 3. Two forks that turned out to be false

Both were live questions at the start of this brainstorm and both dissolved
under examination. Recording them so they are not re-opened as though open.

**"A terminal UI, or a real native game?"** The reference point Nathan named —
`bfnightly.bracketproductions.com/chapter_4.html` — is **not a terminal
program**. bracket-lib draws an 80×50 character grid in an OpenGL desktop
window, with a wasm build available and feature flags for `crossterm`, `curses`,
`opengl`, `webgpu` and `bevy`. "Old-school character grid" and "native desktop
app" are the same option. The decision that remains is not *terminal vs. game*
but *which backend first*, and that is a small, reversible decision because the
cell buffer is ours (§8).

**"The game client, or RENDER-3's world viewer?"** The world viewer is a *cell*
of the possession grid (§7), not a separate product: it is possession with
`commanded = NONE`. `RENDER-focalized-not-commanded` already says so — "attract
mode is not a feature to build, it is a parameter set to none." Building the
target parameter in this campaign is what makes the viewer fall out later
instead of being built twice.

## 4. What was verified, not assumed

Per the autopilot verification rule, every claim below about live behaviour or
cost was settled by running something, not by reasoning about it.

| claim | how it was settled | result |
|---|---|---|
| whether the tutorial is a terminal program | fetched chapter 4 | **no** — RLTK/bracket-lib renders an 80×50 grid in an OpenGL window; a wasm build exists |
| bracket-lib's backends and liveness | crates.io API | 0.8.7, released **October 2022**; flags `crossterm`, `curses`, `opengl`, `webgpu`, `bevy` |
| ratatui's liveness | crates.io API | 0.30.2, June 2026, ~15.9M downloads/90d |
| what a native client could reach past the snapshot | read `windows/vessel/src/session.rs` pub surface | `would_turn_hostile`, `npc_grievance`, `committed_hostility_count`, `agent()`, `knowledge()`, `session_ledger_json()` — all reachable, all forbidden by the brief |
| whether the snapshot types can be deserialized as-is | read `windows/vessel/src/snapshot.rs` | **no** — `SessionSnapshot` and its siblings derive `Serialize` only |
| how the agent id crosses the wire | read `SelfChannel::agent` | a decimal **string**, not a number — it is a uniform 64-bit draw that exceeds 2^53 |
| whether `social` is knowledge-gated | read `SocialEntry`'s doc | **explicitly not**: "rendering this channel unfiltered ships a cheat pane" |
| what the map channel actually carries | read `SpatialChannel`, `SurroundsCell`, `Mark` | a tagged union on `band`; cells carry `state`, legend indices, lattice `u/v/w/up/seam`, and marks carry a `salience` **rank** |
| where possession's cost lives | timed `new` vs `possess --world` | §5 |
| whether possession is drawn from the world or created | read `mint_flagship` | **created** — it mints an agent at the one settlement; §7 |

## 5. The cost of a possession

Measured on the canonical Mac, release build, five runs each, medians, on a
**quiet** box — the first attempt at these numbers was taken at load average 50
while this campaign's own `make prewarm` was running and was discarded as
contended, which is the hazard `docs/timings.md` and the root `CLAUDE.md` both
warn about.

```
                                              median   runs
genesis only    hornvale new --seed 42         1.43 s   1.43 1.43 1.43 1.45 1.46
Session::start  hornvale possess --world ...   0.73 s   0.72 0.73 0.73 0.73 0.76
```

The second row is the important one. It is not world creation — it is the cost
of **every** possession, and `Session::start`'s own comment says why: terrain,
climate, `LocaleContext::build_from`, the coexistence-stack fit, the demography
report and `derive_npcs` are all derived there, and every one of them is
**world-scoped, not agent-scoped**. Nathan's requirement that "possession and
release should take essentially no time at all" is therefore a refactor, not an
optimisation: hoist the world-scoped block into a context that outlives a
session, and let a session borrow it.

## 6. The seam: drive across the linker, read across the serializer

The central architectural question was whether a native client should link
`hornvale-vessel` and read the typed `SessionSnapshot`, or consume the
serialized `vessel/session/v1` JSON as the browser client does.

**Neither. It drives across the linker and reads across the serializer** — which
is what `clients/vessel/wasm` already does. `hv_start`/`hv_handle` link
`hornvale-vessel` and call `Session::start`/`Session::handle`; then every datum
the client *displays* comes back through `snapshot_json`. The wasm shim is a
native program that happens to target wasm, and this client is its desktop peer.

Three reasons this is the right seam rather than a compromise:

**It preserves a sufficiency proof the project already values.** `windows/
explain` narrates a world by reading only committed facts, never the in-memory
system, and the architecture notes say that restriction "is how it validates
that the ledger is sufficient." A client that renders only `vessel/session/v1`
is that pattern pointed at the session snapshot: it becomes standing evidence
that the emitted contract is renderable. A client reading the typed struct
proves nothing, and the first time the snapshot lacks a field, the native client
quietly reaches past it instead of the schema growing to meet it.

**It converts an accident into a design.** Nothing ever *chose* the JSON
boundary in the browser client as a guarantee — wasm forced it, and the
containment is a side effect. Adopting it deliberately costs nothing here.

**The alternative is measurably dead.** A subprocess-per-query design — RENDER-3
as originally written, driving `hornvale scene` as a subprocess — cannot host a
game: it pays genesis on every invocation (§5) and has nowhere to keep session
state.

The cost is a JSON round-trip per turn. Against a measured 1.071 ms `handle` and
1.249 ms snapshot emit, with rendering *episodic* — redrawn on a committed turn,
never on a frame, which `CLIENT-four-clocks` identifies as "nearly free" — this
is not a budget concern at any plausible terminal size.

### Three structural containments, no lints

Each of these is a boundary something *cannot cross*, rather than a rule
something is asked not to break. This is deliberate: a denial test is worth only
its mutation proof, and a doc comment is worth only the next implementer's
attention.

1. **The crate split.** `game-core` does not depend on `hornvale-vessel` at
   all. `Session` is not in its dependency graph, so no render path can reach
   `npc_grievance` — there is no symbol to reach. `game`, the binary, links
   vessel and owns exactly two functions: start a session, hand a line to it,
   return JSON.
2. **The mirror types are a redaction boundary.** `game-core` defines its own
   `Deserialize` types against the schema — as `clients/vessel/src/snapshot.ts`
   already does in TypeScript — rather than `windows/vessel` gaining a derive.
   A mirror written by an outsider is a stronger sufficiency proof than a
   derive, and it leaves `windows/vessel`'s types untouched by this campaign.
   Then: **the mirror omits `social` entirely.** serde ignores unknown fields, so
   the channel the schema itself warns is a cheat pane never enters the client's
   address space, and the cheat pane cannot be built by accident.
   This discharges, for this client, the failure The Journal's retrospective
   named — a schema documenting a discipline that nothing can perform.
3. **The render core is I/O-free.** `game-core` takes a `&str` of JSON and
   returns a cell grid. It does not open a terminal, does not know crossterm
   exists, and does not run genesis — so every visual assertion in the suite is
   a pure function test costing microseconds, not the seconds §5 measures.

The mirror's real risk is silent lag: a field can be added upstream and the
client never notice. The answer is a **committed snapshot fixture**, shared with
the browser client's own fixtures, regenerated by `regenerate-artifacts.sh`, so
drift surfaces as a diff rather than as an absence.

## 7. What possession is, and which two cells this campaign fills

> **CORRECTION (added at merge; the section body below is left as the
> historical record it is).** This section is wrong about what the target
> parameter does, and the error travelled from here into decision 0116, the
> chronicle, and the variant's own doc comment before the whole-branch review
> caught it.
>
> **Both shipped targets MINT.** `PossessTarget::Flagship` and
> `PossessTarget::MostPopulousSettlement` (named `FirstSettlement` until the
> merge — it selected the *most-populous* settlement, while "the first
> settlement" is literally what `Flagship` uses) both call `mint_at`, which
> derives a fresh `AgentId` from a seed stream. They differ only in **which
> settlement** the agent is minted at. Neither selects an agent `derive_npcs`
> already produced.
>
> So the campaign filled **one** cell of the grid below — `commanded = a minted
> agent`, parameterised twice — not two. The `commanded = an EXISTING creature`
> row is unbuilt.
>
> Consequently the paragraph headed *"A doctrine gap this exposes, and closes"*
> is right about the gap and wrong about closing it. The gap is **identified
> and open**: possession is still a mint, the doctrine still says otherwise,
> and closing it needs a derived-NPC identity that survives the session
> boundary plus a session that starts from one. Carried forward as
> `RENDER-possession-still-mints` in the idea registry; see the corrected
> [decision 0116](../../decisions/0116-possession-is-a-parameter-not-a-fixture.md).

`RENDER-focalized-not-commanded` already holds that *focalized* (whose senses
filter the world) and *commanded* (whose body executes verbs) are independent
parameters. Laid out as a saturated grid, every cell names something real:

```
                  | focalized = none    | focalized = the      | focalized = a
                  | (cartographic)      | commanded agent      | DIFFERENT agent
------------------+---------------------+----------------------+---------------------
commanded = NONE  | RENDER-3, the       | attract mode / the   | the scholar or
                  | world viewer        | autonomous observer  | ethnographer vantage
------------------+---------------------+----------------------+---------------------
commanded =       | -- (a body with     | TODAY: mint_flagship | --
minted flagship   | no eyes)            | + derive_npcs        |
------------------+---------------------+----------------------+---------------------
commanded = an    | --                  | THIS CAMPAIGN's      | RENDER-12: an NPC's
EXISTING creature |                     | second cell          | own purview, played
```

This campaign fills the two cells in the middle column and builds the parameter
that makes the rest reachable. It builds none of the others.

**A doctrine gap this exposes, and closes.** The Journal's brief states flatly:
*"You possess a creature already living in the world — you do not create a
character."* But `Session::start` calls `mint_flagship`, which **mints** — it
finds the single settlement, resolves its species, and creates an agent there at
session start; `derive_npcs` then populates neighbours around that creation. The
possessed creature is invented, not discovered, and the doctrine says otherwise.
Nathan's request to "possess the first NPC it finds anywhere in the world" is
therefore not a new feature. It is closing the gap between what Hornvale says it
does and what it does.

**The flagship path stays.** It is the default, it is unchanged, and every
gallery transcript and client fixture derived from it must stay byte-identical
(§10). The new target is opt-in.

## 8. Architecture

```
clients/game/               # outside the cargo workspace, own toolchain
├── Cargo.toml              # a standalone workspace, like clients/vessel/wasm
├── core/                   # pkg `hornvale-game-core` — NO hornvale dependency
│   ├── schema.rs           #   mirror types, Deserialize, `social` omitted
│   ├── cell.rs             #   Cell { glyph, weight, ink } and Grid
│   ├── spread.rs           #   layout: plate | entry / endpaper
│   ├── chart.rs            #   walk band  -> cells
│   ├── plan.rs             #   chamber band -> cells
│   ├── entry.rs            #   prose, ways-on, command line
│   └── endpaper.rs         #   identity strip
└── bin/                    # pkg `hornvale-game` — links hornvale-vessel
    ├── driver.rs           #   start(seed, target) -> String; handle(&str) -> String
    ├── input.rs            #   keys -> verb lines
    └── term.rs             #   crossterm: raw mode, alt screen, styled writes
```

**Naming.** The client is named for the game, not for the game's visual
register. `journal` was the first proposal and Nathan rejected it: it would name
the binary after a mechanic, when the game is simply *Hornvale*. That is the
same defect The Journal's own retrospective recorded — "a component named after
a subject is a claim about that subject's schema" — applied one level up. The
Field Journal register may be revised; the game will still be the game. The
crates therefore follow `clients/`'s existing package convention
(`hornvale-vessel-wasm`) rather than the bare directory names.

**The backend library is crossterm alone, not ratatui.** Ratatui's value is
widgets and layout; this design owns its layout — the spread is bespoke, and the
cell law forbids a widget model owning cell positions. Its `Buffer` would
compete with ours for ownership of the grid. Crossterm is what ratatui itself
sits on and supplies exactly what is wanted: raw mode, alternate screen, key
events, styled writes.

**The cell buffer is the campaign's real artifact.** A `Cell` is
`{ glyph: char, weight: Weight, ink: Ink }` where `Weight` is
`Dim | Normal | Bold` and `Ink` is, for now, a single monochrome value with a
colour variant reserved. A `Grid` is a `Vec<Cell>` with a width — dense indexing,
`Vec` not a map, matching the kernel's storage discipline even though this tree
is not bound by it. Everything above renders *into* the grid; only `term.rs`
turns a grid into bytes. That is the seam a windowed or remote backend attaches
to later, and it is why choosing terminal-first forecloses nothing.

**Weight carries attention; that is the brief's law and it survives here
intact.** On the plate, `here` → Bold, `sensed` → Normal, `remembered` → Dim,
never-known → **the cell is not written at all**. In monochrome that is the whole
channel, which is exactly the brief's acceptance test 8.

**Marks are ordered by `salience`, never dimmed by it.** The brief is explicit
that rank is not magnitude: rank three of four and rank three of forty are
different situations, so a rank may decide *which mark draws on top* and *which
the legend names first*, and may never become a weight.

## 9. Stages

> **CORRECTION (added at merge).** Two acceptance criteria below are wrong as
> written; the substance of both was met.
>
> - **Stage 2** says the second cell is "the first derived NPC found in the
>   world". It is not: it is an agent **minted** at the most-populous
>   settlement. See §7's correction note.
> - **Stage 4**'s command `cargo run -p hornvale-game -- --seed 42` **fails** —
>   `clients/game/` is its own workspace, so `-p` cannot reach it. The working
>   form, which the plan's Task 10 uses correctly, is
>   `cargo run --manifest-path clients/game/bin/Cargo.toml -- --seed 42`.

**Stage 1 — the derivation hoist.** Extract the world-scoped block out of
`Session::start` into a reusable context borrowed by a session.
*Success*: the §5 second-row median falls substantially; every existing vessel
test, gallery transcript and client fixture is byte-identical.

**Stage 2 — the possession target.** `Session::start` takes whom to possess.
Two cells: the minted flagship (default, unchanged) and the first derived NPC
found in the world.
*Success*: possessing the flagship produces byte-identical output to today;
possessing the first NPC produces a different, deterministic, seed-stable
session.

**Stage 3 — `game-core`.** Mirror types, the cell buffer, the spread, both
plates, entry and endpaper. No terminal, no genesis, pure functions over a
committed fixture.
*Success*: the brief's acceptance tests 1, 3, 4, 8 and 9 assert as unit tests
against the fixture.

**Stage 4 — `game`, the binary.** Driver, input mapping, crossterm backend.
*Success*: `cargo run -p hornvale-game -- --seed 42` reaches a rendered spread you can
walk around in; a scripted key sequence produces a byte-identical grid dump.

**Stage 5 — the gate and the book.** `make game-check` joins the gate ladder
alongside `make vessel-check`; chronicle entry; retrospective; freshness sweep;
registry rows.

## 10. Byte-identity risks, named

Stages 1 and 2 both touch the sim's hottest session path, and the artifacts
downstream of it are committed and drift-checked. Two specific hazards:

- **The gallery transcripts are `possess --script` output** and the client
  fixtures are snapshots of a default session (`PossessOpts`' own doc says so).
  Any reordering of derivation that changes a stream's consumption order changes
  every one of them. The stage-1 and stage-2 success criteria are therefore
  stated as byte-identity, and the drift check over `book/src/gallery/` is the
  evidence — not a passing test suite.
- **Stream consumption order is a save-format contract.** Hoisting derivation
  must not change *which* draws are taken or in what order. If it does, that is
  an epoch event (`vessel/agent/v2`), not a refactor, and it stops this campaign
  for a decision.

## 11. Testing

- `game-core` is tested as pure functions over a committed
  `vessel/session/v1` fixture. No terminal, no genesis, microseconds.
- The brief's acceptance tests 1, 3, 4, 8, 9 become assertions. Test 3 — "every
  visible datum, and which channel it came from" — is the one worth building
  properly: a provenance assertion that every non-blank cell traces to a named
  snapshot field. It is also what makes the `social` omission checkable rather
  than merely true.
- Stage 1 and 2 are guarded by byte-identity against committed artifacts, per
  §10.
- Every visual assertion must be able to fail. A grid-dump golden that would
  pass against an empty grid is vacuous; each golden gets a mutation proof that
  it discriminates, and the mutation must assert its target text was found
  before substituting.

## 12. Open questions and follow-ups

- **Which NPC is "the first found anywhere in the world"?** `derive_npcs` mints
  NPCs relative to an already-existing agent, so "first" needs a definition that
  does not presuppose the flagship. Deterministic and seed-stable is the
  requirement; the ordering rule is a stage-2 design decision and is flagged at
  G3 because it is a new seed-ordering surface.
- **Does the chart's `remembered` state reach a fresh session?** Fog is
  session-accumulated; a newly started possession may have no remembered cells
  at all, which would make the brief's "signature gesture" unobservable in
  campaign one. Worth measuring in stage 3 before designing the transition.
- **Telnet-ability is a real affordance, not nostalgia.** Decision 0022 is
  already a client/server split and `vessel/session/v1` is already a wire
  protocol; a character-grid client makes remote play, spectating, and
  simultaneous possession of different creatures in one world nearly free later.
  Nothing here builds it; nothing here should design against it. → registry row.
- The `commanded = NONE` cells (RENDER-3's viewer, attract mode, the
  ethnographer vantage) → registry, pointing at §7's grid.

## 13. Decisions this campaign should ratify

1. A native client drives across the linker and reads across the serializer;
   containment is structural (crate split + mirror types), never a lint.
2. A client's mirror of a schema may deliberately **omit** a channel, and that
   omission is the enforcement mechanism for a redaction the schema can only
   document.
3. Possession is a parameter, not a fixture: `focalized` and `commanded` are
   independent, and the world viewer is a cell of that grid rather than a
   separate product.
