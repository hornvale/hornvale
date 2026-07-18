# CLAUDE.md — working in `windows/worldgen/`

`hornvale-worldgen` is the **composition root** — the only library where all
domains meet and where providers (astronomy/climate/terrain implementations)
are constructed. The CLI and every other window build worlds through it
(`cli/` re-exports it; `windows/lab` builds through it). Read the root
`CLAUDE.md` "Architecture" and `domains/CLAUDE.md` first.

## The rule that defines this crate

**Adding a domain must never require editing an existing domain — and the
wiring that makes that true lives here, not in the domains.** `lib.rs` holds
the `DOMAINS` roster (the single membership list) and `register_all`. A new
domain is added by extending the roster and its provider construction here;
the domain crate itself stays ignorant of its siblings.

Because everything converges, this is the one place cross-domain glue is
allowed. Keep the glue thin: a per-domain adapter that reads a domain crate
plus `WorldComponents`, never two domains reaching into each other.

## The build-depth ladder

Worlds build to a depth, and the rungs **nest**:
`Astronomy ⊂ Terrain ⊂ Settlements ⊂ Full` (`BuildDepth`). Each rung builds
the world to its target depth and reconstructs its own view fields atop the
shallower rung. Consequences to know:

- A metric/consumer that needs only astronomy must not force a `Full` build —
  that pays for terrain sculpting it never reads. (Language/culture metrics
  genuinely need `Full`; they sit atop the whole stack.)
- `lab`'s `depth_ladder` test asserts depth-scoped metrics equal the
  full-build values — don't break the nesting.

## `lib.rs` is large and splittable — but merge-hot

At ~6k lines it is a god-file by size (≈16 per-domain adapter clusters + a big
test module), with clean seams (`errors`, `paleoclimate`, `language`,
`phenomena`, `reports`, `build`). Extraction is already idiomatic here
(`components`/`schedule`/`settlement_pins` submodules exist). A split is
worthwhile **but** this file is edited constantly from parallel sessions —
attempt a split only when it is quiescent, in one focused pass, or expect a
painful rebase.

Watch for the repeated boilerplate if you extend it: the
`WorldComponents::assemble()?` wrapper-pair prelude (a public `foo(world)`
delegating to `foo_in(world, wc)`) recurs ~12×, and the `observed_phenomena`
fan-out has six near-identical variants.

## Save-format note

Worldgen composes; it should not introduce *new* seeded draws lightly. New
draws are the only epoch-triggering additions — prefer deriving parameters
from existing world-state (the self-writing-book program's standing
principle). If you must draw, add a label in the owning domain's `streams.rs`,
never here.
