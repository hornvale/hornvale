# CLAUDE.md — working in `windows/`

A **window** is a way of looking at a world. Domains model; windows present.
The layering (`kernel → domains/* → windows/* → cli`) is constitutional and
enforced by `cli/tests/architecture.rs`, which also emits the book's
generated layering page — so the dependency graph and its documentation
cannot drift apart. Read the root `CLAUDE.md` "Architecture" and
`domains/CLAUDE.md` first.

## What a window may do that a domain may not

- **Depend on many domains.** That is the point — a window presents them.
- **Depend on another window.** `windows/lab` builds worlds through
  `windows/worldgen`; `windows/book` realizes prose via `domains/language`.
  The rank check only forbids depending *upward* (on `cli`).

## What a window should not do

- **Draw new world-state.** Seeded draws belong in the domain that owns the
  concept, with a label in *that* crate's `streams.rs`. A window that draws
  has quietly become a domain with no registry entry and no pin-isolation
  test.

  **The composition root is the standing exception, and it is a real one —
  read `windows/worldgen/src/streams.rs` before applying the rule above.**
  That file declares ten labels of its own, and only two of them are recent:
  `SCHEMA`/`SKY`/`LEXEME`/`DOCTRINE_SCHEMA`/`DOCTRINE_LEXEME` (the chorus),
  `RELIGION_DEITY_V2`, `SETTLEMENT_DISPOSITION` (The Tolerance), `CHAMBER`
  (The Deep Realm), and `VOLCANO`/`HAZARD_EVENT` (The Repose). A rule
  contradicted eight times before this campaign and ten after it is not a
  rule, so state what actually governs:

  A draw belongs in `windows/worldgen` when **no single domain can host it**
  — because the draw's inputs come from two domains at once, and a domain
  crate may not depend on a sibling. `SETTLEMENT_DISPOSITION` is the clean
  case: it needs `hornvale_species::Dispersion` *and* the occupation's own
  site, so neither `species` nor any siting domain can own it, and there is
  nowhere else for the label to live. `VOLCANO` and `HAZARD_EVENT` are the
  same shape over terrain plus world-time.

  What does NOT change: such a label is still a save-format contract, still
  goes through `stream_labels!` so it reaches the generated manifest, and
  still owes the key discipline every other stream owes — key on a **place in
  a fixed lattice**, never a generation ordinal (decision 0102). If the draw
  *could* sit in one domain, it must; "the wiring is already here" is not a
  reason. And at the composition root as everywhere else, **new draws are the
  only epoch-triggering additions** — a derived read over an existing field
  (`has_edifice`, `hazard_at`) consumes nothing and moves no saved world.
- **Read the in-memory system instead of the ledger.** The strongest windows
  read only committed facts — `windows/explain` narrates a world's derivation
  from the ledger alone, which is precisely how it *validates* that the
  ledger is sufficient to explain the world. A window that reaches into live
  system state can pass every test while the saved world is missing the facts
  a reload would need.
- **Interpret domain-specific predicates when it doesn't have to.**
  `windows/historiography` is domain-agnostic by construction: it replays any
  entity's facts against the registry's predicate docs, so a new domain gets
  historiography for free. Prefer that shape.

## The roster

| window | what it presents |
|---|---|
| `worldgen` | **the composition root** — where all domains meet; not a view |
| `almanac` | a world as a one-page document |
| `explain` | a world's derivation, read back out of the ledger |
| `historiography` | how any entity came to be (domain-agnostic) |
| `book` | committed classification facts as Common sentences |
| `chronicle` | the derived-history engine |
| `scene` | semantic-only scene descriptions over the query surface (Ring 2) |
| `locale` | a `RoomAddr` as an observable place |
| `vessel` | possession: walking the locale mesh through a verb loop |
| `lab` | the measurement instrument — studies over generated worlds |

Subdirectories with extra guidance: `worldgen/` (the composition root and the
`BuildDepth` ladder) and `lab/` (studies vs metrics, census regeneration).

## Build to the shallowest sufficient depth

`BuildDepth` rungs nest: `Astronomy ⊂ Terrain ⊂ Settlements ⊂ Full`. A window
that needs only astronomy must not force a `Full` build — that pays for
terrain sculpting it never reads, on every world, in every test. See
`worldgen/CLAUDE.md`; `lab`'s `depth_ladder` test asserts the nesting holds.

## Rendering is a save-format-adjacent surface

Window output is where most committed artifacts come from (the almanacs, the
scene JSON, the census CSVs, the book's generated pages). Two consequences:

- **Quantize at emit.** Floats leaving a window in a serialized artifact go
  through `hornvale_kernel::quantize` — the scene/ephemeris JSON and the lab's
  `render_csv` are the existing boundaries. Never quantize in the compute
  path.
- **Colour is ON by default in the playable path — and four readers in one
  campaign concluded otherwise, each from something true.** `cli/src/main.rs:586`
  selects `Lens::Lantern` for the interactive session; only `--script` takes
  `Lens::Off`, deliberately, so committed transcripts stay byte-stable. Measure
  before you claim: `possess --seed 42` then `enter; map` emits **414 escape
  bytes**, while the same session outdoors emits **0** — and the chart's own
  disclosure line says why, `0 tinted, 31 withheld (water, a mark, or you)`,
  because seed 42's flagship stands on water. Seed 13 outdoors reports `30
  tinted, 1 withheld`. **A single seed is not evidence about the renderer**, and
  neither is `windows/scene/src/surrounds_ascii.rs`'s `\x1b[38;2;` path on its
  own: a code path is not a default, a default is not a screen, and a screen at
  one seed is not the program. Say **which surface, which seed, and whether the
  player opted in.** The wrong answers in order were: colour is a client-side
  deferral; colour is absent; colour is on (from the code path, without the
  default); colour is off by default (from the default, without the interactive
  arm — and evidenced by counting escape bytes in `scripts/possession-*.txt`,
  which are **input command scripts** and could not have contained any). The
  code was never wrong. Every claim about it was, and every correction came
  from running the program.
- **Separately, and this one IS unconditional: there is no per-entity
  distinction in the walk band.** `clients/game/core/src/chart.rs` draws `@` for
  the possession and `+` for everything else, "terrain and marks alike" in its
  own words, and `Mark` carries `noun`/`kind`/`datum`/`salience` with no glyph
  and no colour — so a creature and a boulder are the same character on every
  seed, and no lens changes it.
- **A rendering change is an artifact change.** Regenerate and review:
  `make rebaseline`, then diff the paths `docs/generated-paths.txt` declares —
  `git diff -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')`. That
  file is the single source of truth and no guide restates it
  (`cli/tests/generated_paths.rs` enforces it). One of its entries,
  `clients/game/core/tests/fixtures/`, is a window's artifact
  too, despite living outside the workspace: a change to `windows/vessel`'s
  `snapshot_json` drifts the committed `vessel/session/v2` fixtures. **It is
  not the only surface that can.** The snapshot embeds
  `hornvale_scene::SurroundsScene` (`snapshot.rs`), and `windows/scene/src/
  region.rs` carries its own `quantize_serde` attributes — so a float-emitting
  change anywhere under `windows/scene` reaches those fixtures too. Check the
  whole embedded path, not just the vessel end of it. Scene schemas
  additionally cross the repo boundary to external clients (decision 0055)
  — additive-or-versioned only.
