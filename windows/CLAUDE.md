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
  test. `windows/worldgen` is the one place cross-domain *wiring* lives, and
  even there new draws are the only epoch-triggering additions.
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
- **A rendering change is an artifact change.** Regenerate and review:
  `make rebaseline`, then diff `book/src/gallery/ book/src/reference/
  book/src/laboratory/ docs/audits/`. Scene schemas additionally cross the
  repo boundary to external clients (decision 0055) — additive-or-versioned
  only.
