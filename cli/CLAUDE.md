# CLAUDE.md — working in `cli/`

Two very different things live in this crate, and the second is the one that
surprises people:

1. **`src/`** — the command surface (`hornvale new|scout|repl|possess|almanac|
   map|scene|concepts|streams|lab|explain|…`). Thin by design: it parses
   std-only (no clap) and delegates to `hornvale-worldgen`, which it
   re-exports. Adding a command should not add logic here that a window
   could own.
2. **`tests/`** — the **workspace-wide enforcement suite**. Because `cli/` is
   the only crate that depends on everything, the invariants that must hold
   *across* crates are asserted from here. A failure in `cli/tests/` is
   usually not a CLI bug; it is a report about the whole repo.

Read the root `CLAUDE.md` first; this file is about the second half.

## The enforcement tests, and what a red one is telling you

- **`architecture.rs`** — layering and the dependency allowlist as executable
  assertions over `cargo metadata` (decisions 0002/0004, amended by 0041 for
  `libm`). `ALLOWED_EXTERNAL` here is the *real* allowlist; the prose
  elsewhere is a copy of it. Red = a domain reached sideways, or a new
  external crate arrived.
- **`docs_consistency.rs`** — the knowledge-architecture drift check: frontier
  ToC completeness, registry ID uniqueness and row form, link resolution
  (file *and* `#fragment`), and the ban on citing registry IDs outside
  `book/src/frontier/`. Red = the docs drifted; fix the doc, not the test.
  See `docs/CLAUDE.md` and `book/src/frontier/CLAUDE.md`.
- **`heavy_tier.rs`** — asserts every `#[ignore]`d live-worldgen battery
  carries the one canonical reason string verbatim, so `make gate` (skips)
  and `make gate-full` (runs) can never fall out of sync. Red = someone
  `#[ignore]`d a heavy test with an ad-hoc reason, which would make it
  invisible to *both* tiers. Ignore reasons that are deliberately **not**
  `heavy:` (the census/calibration batteries) are excluded from even
  `gate-full` — that is the intent, not an oversight.
- **`lens_purity.rs`** — the **world-identity guard**: the seed-42 world's
  JSON is a committed fixture. Red = world identity drifted. That is either
  a genuine epoch (terrain/sky) or an accidental one — including the
  non-obvious case where a species-roster change re-baselines settlement
  placement and every generated name world-wide.
- **`release_determinism.rs`** — debug/release byte-identity. This is the
  ratification evidence for running censuses under `--release`; a red here
  means the optimizer moved a drawn value, which is a save-format-class
  event.
- **`accession.rs`** — parity between the concept registry and the authored
  `EPOCH_COHORTS` table. An authored table has one failure mode: a forgotten
  row, which silently defaults to epoch 0. Red = you registered a concept and
  didn't file it.
- **`branches_identity.rs`**, `graph_cost.rs`, `*_exit_criterion.rs` — the
  structural guard for the post-Branches world, the connection-graph size
  gate, and campaign exit criteria transcribed verbatim from their specs.

## Adding a command

Flags are parsed by hand against the `*_FLAGS` help constants in `main.rs`;
those constants **are** the help output, so a flag added without a line there
is undocumented by construction. Keep the pin-parsing delegated to the owning
domain (`hornvale_astronomy::parse_pin` and friends) rather than
reimplementing it — pins fail loudly with a physical reason, and that error
should surface unchanged.

`main.rs` is ~1.9k lines and, like worldgen's `lib.rs`, edited from parallel
sessions. Prefer a new submodule (`concepts.rs`, `streams.rs`, `repl.rs`,
`dictionary.rs` are the precedent) over growing it.

## A CLI change is usually an artifact change

Most commands here have committed output in `book/src/gallery/` or
`book/src/reference/` (the almanacs, the elevation map, the `concepts` and
`streams` dumps). Changing a command's *rendering* — even a whitespace
tweak — drifts those. After any output change:

```bash
make rebaseline
git diff book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

and review the diff rather than committing it blind: the point of the drift
check is that it makes you look.
