# Retrospective — The Tilth / The Tense

**Covers both campaigns.** The Tense ran as further stages of
`campaign/the-tilth` rather than on a branch of its own (its spec says why: it
needs `per_species_capacity`, which existed only there, so a separate branch
would have forked from an unmerged branch). One branch, one rebaseline, one
execution — splitting the process lessons across two files would fabricate a
boundary the work did not have.

## What went right

**Branching off an unmerged branch was recognised as a hazard and avoided.**
An earlier framing ("The Sovereign") was started off `main` on the belief its
subject matter had landed there, and was caught only by an unresolved import at
the pre-commit hook. The Tense was deliberately placed on the branch that
actually carried its dependency. This is the cheap version of a lesson that is
otherwise paid for at merge time.

**Two implementations were reverted rather than defended.** The Tilth's stages
6 and 7 were both landed with measurements, both found to trade one defect for
another, and both taken back out in `511d1fa9` — with their numbers preserved
in the doc comment of the function they touched. The reverts are what made the
underlying diagnosis (the constraint structure is flat) reachable; had either
been kept, the campaign would have shipped a floor arrangement and never asked
why floors were the question.

**The null was allowed to be the headline.** The Fallow's premise turned out to
be falsified — its H1 baseline of "1 layer" was never real, and 16 were measured
at the time. That was recorded as a finding rather than rescued.

## What went wrong, and the rule each one buys

**1. `--test X` is not the lib tests.** Two unit tests inside
`windows/worldgen/src/history_bake.rs` were broken for an entire session while
`cargo nextest run -p hornvale-worldgen --test history_bake` — the *integration*
binary of the same name — ran green, and "all rule tests pass" was claimed on
that basis. A same-named integration target is a trap specifically because it
looks like the thing you meant.
**Rule:** run `--lib` explicitly, or run `make gate`. Never infer lib coverage
from a `--test` invocation.

**2. Retiring an oracle makes fixtures vacuous, not red.** The same two tests
had built their conditions out of `EraClimate.habitable`, which the bake stopped
reading. They did not fail — they passed, having quietly stopped exercising
anything. The handoff's own hypothesis about them (that they were correct
consequences of the eviction change) was wrong; the mechanism was the dead mask,
and eviction had nothing to do with either.
**Rule:** when a field stops being read, grep the fixtures that construct
through it before assuming the suite will notice. And diagnose the mechanism
before accepting a plausible story about it — this one was plausible and wrong.

**3. Measuring the parts you happen to have listed, and projecting as if they
were all the parts.** A cost projection of "1.1×" measured 3.7×, because the
per-species scoring loop was not in the list of components measured.
**Rule:** a projection from components must first show that the components sum
to the whole. Measure the total, then decompose — not the reverse.

**4. Unchecked multiplication, on the wrong machine.** "+70 minutes" for a
census, from multiplying a per-world cost by a world count. The census is
embarrassingly parallel (`runner.rs:210` spawns `available_parallelism()`
threads), and the per-world figure had been timed on a different box than the
one censuses run on. Two independent errors in one number.
**Rule:** state a ratio, not a second-count, until the measurement is taken on
the host that will pay it. The fleet is not uniform — lefford 384 GB/40 cores,
MacBookPro 64 GB/~10, ambrose 38.7 GB/12.

**5. A gauge that re-measures its own anchor.** `tilth_probe` recomputed its
target on every run, so it could not report drift — it would have re-gauged
`V_max` from 140.2 to 118.9 and reported agreement. Now frozen, with drift shown
beside it.
**Rule:** an instrument's zero must not move with the thing it measures. This is
decision 0104's shape arriving from a different direction: a check that cannot
fail is not a check.

**6. A test that hunts for a fixture instead of constructing one.** The vessel's
settlement-free test scouted seeds for a world that happened to have no
settlements; The Tense made empty worlds rare, and the scout ground for 50
minutes before being widened to a range that would have built 357 full worlds.
`BuildDepth::Terrain` produces the fixture directly in 0.90 s. **This class
recurred:** `id_shift_invariance`'s witness seed has now been re-hunted three
times in two days (42 → 7 → 1, and 1 has now gone vacuous too), and the REPL's
settlement-listing test lost its precondition when names stopped repeating.
**Rule:** construct the property; do not scan for a world that exhibits it. Where
a hunt is genuinely necessary, the anti-vacuity guard is what makes it survivable
— all three of those tests failed loudly instead of passing empty, which is the
design working.

**7. A single-seed comparison measures noise.** A `GENESIS_TOP_CELLS` sweep on
seed 42 returned 433 / 483 / 558 / **281** for 8 / 16 / 32 / 64 —
non-monotonic, because changing the constant re-rolls the world. There is no
"same world with a different constant" for a constant that feeds generation.
**Rule:** any constant that participates in generation gets a multi-seed spread,
never a single-seed before/after.

**8. A confidently wrong handoff costs hours.** The handoff this session
inherited was wrong in three places. The one written at its end adopted a
convention worth keeping: **every claim is either accompanied by the command that
measured it, or explicitly marked as inferred.** That convention is the single
highest-leverage artifact this arc produced, and it caught its own author —
`era.ice`'s emptiness and the "make factor ice-only" proposal were both recorded
as settled precisely so the next session would not re-derive them.

## Open follow-ups

- **`id_shift_invariance` needs constructing, not re-hunting.** Three witness
  invalidations in two days is a rate that argues the hunt is unsustainable. The
  file's header records that a *synthetic id shift* was considered and rejected
  as near-vacuous — correctly — but that is a different thing from constructing
  two genuinely distinct records with equal material cores. Worth a proper look
  rather than a fourth seed.
- **The re-pin wave is downstream of three open calls** (kobold's distribution,
  the migration floor, the owed census). Re-pinning roughly forty seed-content
  assertions before those are settled risks doing the work twice, since each of
  them moves placement.
- **Ocean exclusion now rides supply rather than the era mask** and is the most
  likely site of a silent regression; `era_substrate.rs` guards it.
- **`tolerance_tiered` is landed and called by nothing.** It should either be
  wired or removed; dead successors rot.
