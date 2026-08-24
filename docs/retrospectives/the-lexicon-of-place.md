# The Lexicon of Place — retrospective

**In flight** (merge pending). Process lessons only. The product story is in
[the chronicle](../../book/src/chronicle/the-lexicon-of-place.md); the
vocabulary itself is
[the glossary](../../book/src/reference/lexicon-of-place.md); the rules are
decisions
[0246](../decisions/0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md)
and
[0247](../decisions/0247-a-mesh-vertex-is-a-vertex-and-a-face-is-a-facet.md).

## The spec was written by reasoning about the code, and the code disagreed four times

This is the headline, and it is not a criticism of the spec's author — it was a
careful handoff document, and every one of the four misses was one query away.
That is exactly why it is worth recording: **carefulness is not the variable
that separates these outcomes; whether you asked the code is.**

| the spec said | the code says | how it was found |
|---|---|---|
| rename `CellId` to `Node` | "node" already means four different things here | one grep, before writing any code |
| `level` means three things | four — it missed `RegionScene.level`, a serialized field of a cross-repo scene schema | reading the struct for its `derive` |
| two vertical ladders, `Band` and `Stratum` | three — `Horizon` in terrain is the rock column | `git grep 'pub enum'` while writing the glossary |
| the `"room/"` prefix has no test cover | it has three sites and six objecting tests | `scripts/mutate.py` |

The fourth is the one that mattered most, and it is treated separately below.
The first would have shipped a fifth meaning of "node" into a campaign whose
entire purpose was removing a naming collision.

**The transferable rule** is the one `campaign-autopilot` already states and
which this campaign is a fresh instance of: a sentence of the form "X is the
case" about the repository needs a command-and-output pair beside it *at
drafting time*. Extend it with what this campaign adds: **a rename's candidate
name is itself such a claim.** "Call it `Node`" asserts that `Node` is free.

## A red with a one-command answer is worse than no cover at all

The spec made the `"room/"` knowledge-key coupling its flagship deliverable,
described as a total gap: *"two literals, in two files, with nothing asserting
they agree ... rename one and the fog of war silently stops working: no test
fails."*

Mutation testing found the opposite, and then something worse than the
original claim:

| mutation | tests red |
|---|---|
| the writer alone | 6, including the fog-of-war test the spec says goes silent |
| writer + the `strip_prefix` reader | 5 |
| **all three sites, consistently** | **2** |

The spec counted two sites; there are three — it missed the default-deny match
arm in `knowledge_is_subset`, which is the strongest cover of the three.

**A rename campaign is tidy, so it produces the third row.** And the only two
objectors there are byte-goldens, whose documented remedy is one
`make rebaseline-goldens`. The suite goes red, one command makes it green, and
a changed wire format ships to `clients/game` behind a green gate.

The generalisation, now decision 0246: **a byte-golden is an assertion about
current output, not about a contract.** Its purpose is to be re-accepted when
output moves deliberately. So a wire-value freeze may never rest on one — it
needs a test that writes the literal out and therefore cannot be rebaselined.

**And note which half of this the process caught.** The plan step said "the
spec predicts nothing fails; if something DOES fail, say so and name it." That
branch — writing the *decision rule* instead of the *prediction* — is what
turned a wrong premise into a finding instead of a silently-skipped task. The
prediction would have been discharged by a green.

## Renaming through a `git grep -E` under-reported the blast radius by 1.8x

The first scope question put to Nathan carried "~9,000 sites". The real figure
was **16,201 occurrences across 447 distinct identifiers**. The cause was
mechanical: `git grep -E` does not support `\b`, so the pattern silently
matched less than intended. `git grep -P` gives the right answer.

This is already in memory as a tooling trap and it still cost a wrong number
in a question that shaped the campaign's size. The number was corrected to
Nathan before any work depended on it, but the lesson is that a *measurement
used to ask for a decision* deserves the same verification as a measurement
used to make one.

## A type rename reached committed data through documentation

After the mechanical rename, exactly three committed artifacts drifted and all
three carried the same sentence: a laboratory metric's doc string names
`NearestCellIndex`, and that prose is published into the census `schema.json`
and read out again by the Domesday survey.

No metric name moved, no metric value moved, and no `rows.csv` moved. But the
plan's branch table said any movement under `book/src/laboratory/` means STOP,
because that is where census goldens live — and it was right to make me look.
The fourth branch is now known and recorded: **a metric doc string citing a
renamed type is expected drift.**

Worth generalising: the freeze inventory covered labels, epoch keys, wire
keys, predicate names and CSV columns. It did not occur to anyone that *prose*
is a serialization boundary too.

## Every verification grep in this campaign was blind to `CELL`

The plan's definition-of-done, and every one of the six dispatch briefs, told
the sweeper to verify with `git grep -nP '\b\w*[Cc]ell\w*\b'`. That pattern
matches `cell` and `Cell` and misses **`CELL`** — so ALL-CAPS constants and
shouty prose were invisible to the check that was supposed to prove the sweep
complete. A sweeper could finish, run the required grep, get a clean residue,
and be wrong.

It was caught by a subagent noticing the asymmetry itself, not by the check.
Found afterwards: `ISLAND_CELL_CAP`, `LAB_ISLAND_CELL_CAP`,
`MIN_SETTLEABLE_CELLS`, `LANDMASS_MIN_CELLS`, `GENESIS_TOP_CELLS`,
`SHALLOW_YOUNG_CELL`, `PANEL_CELLS`, `LOUD_REACH_CELLS_FLOOR`,
`CONSERVATION_CELLS`, `ARC_EDIFICE_DECAY_CELLS`, plus prose in four files —
about 80 occurrences across five crates.

**Two things follow, and the second is the one worth keeping.**

The narrow fix is `[Cc]` → `(?i)`. The general one: **a case-sensitive
character class is a silent scope limit, and a limit you wrote yourself is the
hardest kind to see** — the pattern looks exhaustive because you enumerated the
cases, and enumeration reads as completeness. The guard that shipped
lowercases before comparing, so it catches what the greps could not; the check
that *proves* the work should not share a blind spot with the check that
*did* the work.

## The rename found a collision that predated it

`windows/locale`'s `Transect` struct already had a field named `vertex` — a
`(polyline, index)` pair naming a point on a channel centreline. Renaming its
`cell: Vertex` field produced two fields called `vertex` in one struct, and the
compiler refused it.

That is the campaign's own thesis firing at it. Two different vertices in one
struct, one of them a mesh vertex and one a polyline vertex, is precisely the
collision class the rename existed to remove — and it was already there, under
a word nobody had audited because nobody was renaming it. The fix names the
polyline one for what it is (`polyline_at`) rather than for its type.

**The transferable point:** a rename does not only rename. It *forces a
uniqueness check* on every name it lands next to, and the compiler performs
that check for free. Some of what a rename campaign finds is not damage it
caused.

## The mechanical sweep was wrong exactly where the two senses share a file

Three areas were swept by script and two of them had to be reverted:

- `windows/scene/src/surrounds.rs` builds a CHART of squares. A blanket
  substitution turned `scene.cells` into `scene.vertices` — chart squares
  renamed to mesh vertices, which is the campaign's thesis applied backwards.
- `windows/scene`'s ASCII renderer, goldens and probes mix the senses *inside
  single functions*: a terrain lookup at a mesh vertex, three lines from an
  iteration over chart cells.

`clients/game/core` is the clean opposite: 228 uses, all of them chart squares,
nothing to rename at all.

**So the unit of classification is not the crate and not the word — it is the
expression.** The decisive test that works is "what is this typed as": a
`hornvale_kernel::Vertex`, or something out of a `VertexMap` /
`geo.vertices()` / `nearest_vertex`, is the mesh sense; a `SurroundsCell` or an
index into `scene.cells` is a square. Scripts cannot apply that test. That is
the line between what was scripted and what was delegated, and getting it wrong
in `windows/scene` cost one revert.

## Parallel sweepers in one worktree: what worked and what to fix

Three subagents swept disjoint crate trees in the same worktree
simultaneously. It worked, and two frictions are worth recording.

- **Instructing "run `cargo fmt`" ran it workspace-wide**, reformatting files
  the other two agents had open. Harmless here (style-only, idempotent) and
  the agent flagged it unprompted, which is the behaviour the dispatch brief
  asks for. Next time say `cargo fmt -p <crate>`.
- **A `pub` item renamed inside one agent's tree breaks callers in another's.**
  `run_cells` and `variant_at_cell` are consumed by `windows/lab` and
  `windows/locale`, so the workspace did not compile between waves. Expected
  and cheap, but it means **the wave boundary is a real barrier** — a
  downstream crate cannot be swept concurrently with the crate it depends on.
  One agent noticed a concurrent sweeper's edit flip-flopping in
  `windows/worldgen/src/lib.rs` and used it to infer the intended target name,
  which is resourceful and also a sign the boundary was drawn one crate too
  loosely.

**The instruction that paid for itself** was asking each agent to report
"anything you left alone because you were not certain". That list is where the
frozen byte-golden CSV header, the retired `subcell-outlet` label, and the
cross-crate leak all came from — none of which a clean-sweep report would have
mentioned.

## Follow-ups

- **`docs/timings/subfloor-roster.tsv` selects tests by exact name**, and this
  campaign renamed many test functions. Reconciling the roster against the
  live `cargo nextest list` is part of the campaign, not a follow-up — but the
  underlying fragility is: a rename anywhere silently drops tests from the
  commit gate until someone notices, and nothing warns.
  `docs/timings/test-baseline-<host>.tsv` has the same shape.
- The guard added here bans vertex-sense "cell" in `*.rs`. It says nothing
  about the book, the specs, or the plans, which is deliberate — those are
  historical records — but it means a *new* book chapter may reintroduce the
  word with nothing objecting.
