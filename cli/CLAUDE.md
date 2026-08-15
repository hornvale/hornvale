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
  carries the one canonical reason string verbatim, so every gate short of
  `gate-campaign` skips it and `gate-campaign` (via `scripts/gate-full-heavy.sh`)
  runs it — the two can never fall out of sync. Red = someone `#[ignore]`d a
  heavy test with an ad-hoc reason, which would make it invisible to *every*
  tier. Ignore reasons that are deliberately **not** `heavy:` (the
  census/calibration batteries) are excluded even from `gate-campaign` —
  that is the intent, not an oversight.
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

## A crate-scoped green is not a branch-green

The root `CLAUDE.md` tells you to iterate cost-ordered — `cargo test -p <crate>`
while working, full `--workspace` coverage at the stage gate's lane dispatch —
and that advice is right. But
it has one consequence worth stating where the tests actually live: **every
invariant listed above is asserted from `cli/`, and none of them is about the
crate you edited.** A `cargo test -p hornvale-terrain` cannot see layering, the
dependency allowlist, doc drift, `claim_shape`, `heavy_tier`, or
`docs_consistency`, no matter how green it is.

This is not hypothetical. The Ford's stage 2 shipped the one assertion carrying
the campaign's durability guarantee without its `/// claim:` tag;
`cli/tests/claim_shape.rs` failed from that commit onward and **nobody noticed
for a whole task**, because the task's gate evidence was a crate-scoped run
that was doing exactly what the guidance recommends. It surfaced only because
the next implementer happened to run something wider.

So: a task may *iterate* crate-scoped, but the evidence it reports as "green"
must be workspace-wide, or it is reporting on a different question than the one
being asked.

## The heavy tier is invisible to every gate but `gate-campaign`, including on `main`

`gate-commit` and the stage gate's own suite both skip the `heavy:` tier by
design, so anything only that tier can see is unobserved on every ordinary
commit — and `main` is no exception. Two shapes this has actually taken:

- **A heavy cost gate sat RED on `main` and nothing reported it.** The Tithe's
  close found `connection_graph_cost_is_bounded_on_seed_42` failing at 31.1 s
  against a 15 s budget, red since the physics moved — the campaign's own
  headline (tribute keeps communities alive) had taken seed 42 from 203
  settlements to 344. It surfaced only when a task happened to run the tier.
- **A heavy test can OWN a committed artifact**, which makes the tier an
  *authoring* path and not merely an expensive one. That is why `heavy-run.sh`
  carries the same canonical-host guard a census does (see `scripts/CLAUDE.md`).

**The ignore reason is compared VERBATIM, not by prefix.** `heavy_tier.rs`
asserts the string is exactly:

```
heavy: live-worldgen battery; deferred from the commit gate to make gate-campaign (decision 0132)
```

A bespoke reason naming its own cost — which is what
`windows/lab/tests/preregistration_guard.rs` asks for elsewhere — fails this.
The canonical string satisfies both guards, so use it unchanged on every
`heavy:`-tagged battery. Guessing cost The Fare a full gate cycle across four
batteries that each had a sensible, descriptive, rejected reason.

The string used to say "(minutes)" and "make gate-full" — both wrong, since
the retired-target name predated decision 0132's rename and the duration was
never re-measured after it was written. The Retelling (Task 7) measured a
sample of the actual heavy batteries at 1.96-6.40 s, two orders of magnitude
under "minutes", and retired the duration claim outright rather than
replacing it with a fresher one: a duration baked into a verbatim-compared
ratchet goes stale the moment the batteries' cost changes again, so
`heavy_tier.rs` now also asserts the string states no duration at all
(`the_canonical_heavy_reason_states_no_duration`). Updating every
`#[ignore]`d heavy test in the tree to match was the change, not a follow-up
avoided for being cosmetic.

**Dropping the duration broke the OTHER guard, silently, until `gate-commit`
said so.** `preregistration_guard.rs`'s `reason_is_sanctioned` treats any
reason containing "minutes" as naming a cost — that is how the old string
satisfied it. The new string names no cost, so every `heavy:`-tagged
`#[ignore]` inside a `tests/*calibration*.rs` file (seven sites, across
`disposition_calibration.rs`, `health_calibration.rs`,
`the_fare_calibration.rs` and `the_mire_calibration.rs`) started failing that
guard the moment the duration left. The `(decision 0132)` suffix is not
decorative: `reason_is_sanctioned` also accepts any reason that names a
decision, so citing the decision that renamed the target this string refers
to satisfies both guards again, honestly. If either guard's matching logic
changes, re-derive the canonical string against both rather than one.

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
# The path list is `docs/generated-paths.txt` — the single source of truth, so
# no guide restates it (`cli/tests/generated_paths.rs` enforces that).
git diff -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

`clients/game/core/tests/fixtures/` belongs on that list even though it lives
outside the cargo workspace: `possess --snapshot` writes it, so a change to
the possession command's *output* drifts it exactly the way a rendering change
drifts an almanac.

and review the diff rather than committing it blind: the point of the drift
check is that it makes you look.
