# The Fathom

A fathom is a unit of depth, and originally a unit of *reach* — both arms
spread, roughly six feet, the span a sailor could pull in one stroke of a
lead-line before reading the next mark. It measures nothing about the
surface. It exists because a chart that only says *water here* is not yet
a chart: a ship needs to know how much water.

Hornvale's world had exactly that gap. Every cell already carried a type
built to answer it — `BiomeExpr { realm, formation, stratum }`, one field of
which is a literal depth rung — and the one function every consumer actually
called, `biome_at(cell)`, read the type and threw the rung away before
returning. The accessor was behind the type it was built from. This campaign
is the fathom-line: an addition, not a redesign, that lets the world answer
a question its own data structure had been able to answer all along.

## What the accessor discarded

`GeneratedClimate::biome_at` returns one `Biome` per cell — a flat
place-name, `TemperateForest` or `Reef`. It derives that name from
`biome_expr_at(cell) -> BiomeExpr`, whose `stratum` field records *which
rung of a realm's vertical ladder* the cell's community sits on: `Surface`
for land, one of five pelagic bands for the sea, one of five rock bands for
the underworld. `biome_at` calls `biome_expr_at`, reads `.formation`, and
discards `.stratum` on the way out. The information was computed and thrown
away at the same call site, every time.

Two accessors close the gap, both pure reads over data the cell's stored
expression already carries — no new draws, no new storage, no change to
construction order:

```
strata_at(cell) -> Vec<Stratum>                          // every rung present, shallowest first
biome_expr_at_stratum(cell, stratum) -> Option<BiomeExpr> // the community at that rung, or None
```

The headline test is a consistency identity that has to hold at every cell
in every world for the new accessors to be trustworthy at all: querying the
column at a cell's own stored rung must reproduce exactly what the old
accessor already said there.

```
biome_expr_at_stratum(cell, biome_expr_at(cell).stratum) == Some(biome_expr_at(cell))
```

That is the whole shape of the campaign: give the column a name, prove the
old single-value read is one slice of it, and change nothing else.

## The sea's column, for free

The ocean already computes everything a column needs. `classify_marine_expr`
derives a cell's stratum from its **floor** depth and its community from a
separate precedence chain — the module's own reasoning is that "a vent is a
community *at* a depth," not a depth that got overwritten by a community.
Once stratum and community are already independent, the column above a
seafloor cell is not new information, only a new way of reading what is
there:

```
for an ocean cell whose floor sits at stratum F:

  strata_at(cell)                      = [Epipelagic ..= F]
  biome_expr_at_stratum(cell, F)        = the cell's stored expression   (the seafloor community, unchanged)
  biome_expr_at_stratum(cell, s < F)    = { WATERWORLD, OpenWater, s }   (open water above the floor)
  biome_expr_at_stratum(cell, s > F)    = None                          (below the floor: absent)
```

A reef sits in a one-rung column. The deepest floor recorded anywhere in
seed 42 is `Bathypelagic` — a three-rung column; no cell in that world
reaches `Abyssal` or `Hadal`, so a four- or five-rung column doesn't occur
there, though the accessor is general enough to report one wherever a floor
reaches that deep.
Land cells, and every cell in the world before this campaign, keep a
one-rung column of `[Surface]` — unchanged in every observable respect.

## Two predictions, measured on seed 42

Two hypotheses were frozen before the column was queried against a real
world (`BuildDepth::Terrain`, 29,896 `WATERWORLD` cells):

**H-1 — the column is non-degenerate.** Four clauses, all required: at least
three distinct column heights occur; the median column height is at least
three; fewer than 5% of ocean cells are single-rung; no single height holds
more than 90% of cells. Measured heights: `{1: 1,749, 2: 6,669, 3: 21,478}`.

| clause | requirement | measured | verdict |
|---|---|---|---|
| distinct heights | ≥ 3 | 3 | confirmed |
| median height | ≥ 3 | 3 | confirmed |
| single-rung share | < 5% | 5.85% (1,749 / 29,896) | **preregistered, not met** |
| tallest-bucket ceiling | ≤ 90% | 71.8% | confirmed |

Three of four clauses hold cleanly — the column is not degenerate, most
cells are multi-rung, and no single height dominates. The third measured
5.85% against a 5% ceiling, narrowly over, and it is carried on the record
as falsified rather than rescued: no threshold was moved after the fact. The
diagnosis is that a single-rung column is a floor shallower than 200 m — a
continental-shelf cell — and Earth's own continental shelf covers roughly
7–8% of ocean area. The world was not wrong; the ceiling was authored
before anyone measured a real shelf fraction against it. That is now its own
open question, filed for a successor to re-derive the ceiling from a
measured seed set rather than a guess.

The same three numbers hold a second, unremarked finding: `1,749 + 6,669 +
21,478 = 29,896` exactly, the full ocean-cell count, so no seed-42 cell
reaches a fourth or fifth column height at all. No cell in the flagship
world has an `Abyssal` or `Hadal` floor — two of the sea's five pelagic
strata never occur as a floor anywhere seed 42 built, though the accessor
would report them wherever a floor reached that deep.

**H-2 — sea ice occurs below the surface it forms at.**
`classify_marine_expr` selects `Formation::SeaIce` in its first arm, keyed
on sea-surface temperature alone, with no depth condition — while `stratum`
is derived from the floor. A polar cell over an abyssal trench should
therefore yield ice filed four kilometres down. Measured: of 9,695 sea-ice
cells, 8,916 — **91.96%** — carry a stratum below the epipelagic. This is
not a marginal artifact of one unlucky cell; it is nearly the whole
population. The mismatch existed before this campaign — `classify_marine_
expr` has always read surface temperature independently of floor depth — but
nothing before the column could see it, because a one-value-per-cell
accessor cannot expose a contradiction between a cell's formation and its
depth. Reported and left unrepaired, per this campaign's own scope: fixing
it changes what a cell returns, and that is the byte-identity budget this
campaign exists to protect. It is now a named artifact for the campaign that
next redefines what a marine community means.

## The column that already existed, one crate over

The clearest finding this campaign produced was about itself. Partway
through implementation, a routine check of a neighboring file —
`windows/locale/src/lib.rs` — found `water_column_at` and `expr_at_stratum`,
functions doing exactly what this campaign's Task 1 had just built:
same idiom, same "the community lives on the floor" rule, a doc comment
making the identical argument this campaign's own spec makes, already
shipped, already in the tree.

Two independent implementations of the same derivation is precisely the
hazard the headline consistency test above exists to catch — and it could
not catch this one, because the duplicate lived in a file the campaign's own
diff never touched. A review of a diff can prove that new code agrees with
itself; it cannot prove that new code agrees with an old function it never
looked at.

The two were not identical. The new column is more general — it answers a
query at any stratum, on any realm, and returns `None` below a cell's floor.
The older one only ever answered water queries, and its below-floor case
falls through to `OpenWater` rather than absence, so it will answer "open
water" for a rung that is solid rock. The resolution was delegation rather
than deletion of either: `windows/locale`'s functions now call through to
`GeneratedClimate::strata_at`/`biome_expr_at_stratum`, preserving the older
function's exact contract — including its incorrect fallback — because
changing that fallback's *answer* would move client-facing bytes, which is
this campaign's one forbidden move. A captured before-arm fixture and an
exhaustive agreement check across all 40,962 cells of the seed-42 world
confirm nothing moved in the collapse itself.

## The one latent re-key

Five sites in the codebase assumed the world has exactly two realms — land
and sea. Reading them against the actual structure of the underworld (a
stratum *beneath* a cell, never a cell's own biome) showed that four of the
five stay correct forever, because they reason about a cell's surface
projection, which never changes. The fifth, `vantage.rs`'s `submerged`
field, asked *"is this stratum something other than `Surface`?"* — a
predicate that looked live, and was not: `Session` threads the water column
and the cave lattice through two separate fields, `submerged:
Option<Stratum>` (populated only from `water_column_at`, which never returns
a rock stratum) and `underground: Option<Chamber>`, so no rock stratum
reaches `describe_at` by any path that exists today. Campaign 2, the first
campaign to give the underworld a community, is expected to extend
`underground`, not repurpose `submerged`.

The fix is worth having anyway. `Stratum` is one enum spanning both the
pelagic and rock ladders, so nothing in the *type* stops a future session
refactor — unifying the two descent fields, or widening the dive verb — from
handing `submerged` a rock rung; the predicate would then answer wrong on
the day that happens, silently, since a rock stratum is not `Surface` and is
not wet either. The fix asks the question `submerged` was always meant to
ask — is this stratum's realm a water realm — rather than the proxy
question that used to stand in for it. Values are identical today, by
construction, which the committed client-facing snapshot fixtures make a
gate condition rather than an assumption.

The other four sites keep their exact behaviour and gain a doc line each,
naming the question they actually answer — a cell's *surface* medium, never
a question about the column beneath it. That is cheaper than a re-key and
is what actually stops the next campaign from tripping over them: a
predicate that is correct but narrowly named is a trap for a reader who
assumes the name is the whole truth.

## What this leaves standing for the next campaign

One finding is carried forward as a concrete, traced prediction rather than
a vague warning: a rock stratum reaches `windows/locale`'s preserved
below-floor fallback and produces `BiomeExpr { formation: OpenWater, .. }`
at a rock rung. `BiomeExpr::biome()` declares a cave formation
`unreachable!()` by design — the underworld has no communities yet — so
that value panics the instant anything constructs it. Nothing in the
codebase does today. The campaign that gives the underworld its first
community (Campaign 2 of The Chorography, unblocked by this one) will be
the first thing that can, and this chronicle exists partly to make sure it
does not discover the panic by hitting it.

## The proof

Every committed artifact — the seed-42 world, its almanacs, the gallery
renders, the reference dumps, the census schemas, the domesday survey, the
digest, and the client-facing session fixtures — was regenerated at the
campaign's close and diffed against what stood before it started. Nothing
moved, not even `docs/audits/type-audit-report.md`: this campaign adds two
`pub` functions, and the type-audit report drifts on any pub-boundary
change, but it was already regenerated in the same commit that introduced
them, so no drift remained to find at close. **The world is byte-identical.
The accessor was behind the type, and now it is not, and not one value
anyone can observe from outside the crate moved by a single byte.**
