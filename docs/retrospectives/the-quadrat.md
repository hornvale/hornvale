# Retrospective — The Quadrat

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-quadrat.md): a zoom ladder rebuilt
onto the mesh's own depths, a three-layer map with three cache keys, and two
measured registry rows dissolved by the rebuild rather than by attacking them.

## The headline: eleven claims that could not discriminate

Not eleven bugs. **Eleven pieces of evidence that proved nothing while appearing
to** — and the reason this is the campaign's transferable finding is that every
one of them was invisible to reading, invisible to a green suite, and visible
only to mutation.

**Eight were tests whose input space collapsed to one value.**

- Task 1's subrect test compared two all-**ocean** grids. It passed with the
  implementation gutted.
- Task 3's facet test ran at one rung, and that rung was the constant a mutation
  hard-coded, so the mutation survived green.
- Task 3's grid-level read was untestable because three separate constants all
  equal 6 — `hornvale_terrain::GLOBE_LEVEL`, the client's `GLOBE_RUNG`, and the
  literal. **The obvious fix for that would have been vacuous too**, because a
  depth-6 facet's level-5 ancestor is the same triangle. The fix that works
  builds a world at a *different* globe level, so one test can tell the three
  apart.
- Task 5's rung test used columns 96 and 192, which are different tiles
  regardless of the rung, so a `depth: 0` mutation stayed green.
- Task 7's cursor invariant would have been asserted at a tolerance **equal to
  half a tile at the coarsest rung** — blind to a full one-tile error, which is
  the only error it exists to catch.

**Two were suites that would HANG rather than fail** on the exact regression
their tests existed to catch: an unbounded loop in a helper about 25 tests route
through, and a dropped modulo that stalled a composition loop. A hang is worse
than a red, because a red is a result and a hang is a machine to go debug.

**One was a COMMENT asserting coverage a test did not provide.** `driver.rs`
claimed a named test pinned an anchor's placement. The reviewer applied the
exact regression the comment described — moved the anchor block outside its
guard — and all eleven relevant tests passed. The comment's own premise defeated
it: an idempotent anchor is invisible to that test. The test is not a tautology
and was kept; only the claim about it was wrong.

**The pattern's shape, stated so it is recognisable next time:** a test's
*input* space collapses to one value, so the test cannot distinguish what was
computed from what was assumed. Reading the test does not reveal it, because
each test looks reasonable. Running the suite does not reveal it, because
everything is green. Only mutating the code the test claims to cover reveals it.

**What worked against it:** every task brief from Task 2 onward carried the trap
named in advance, with the prior instance quoted. The last three instances were
caught **by the implementer** rather than by review — which is the only outcome
that scales, since a reviewer catching them one at a time is a linear cost and a
brief that teaches the shape is paid once.

## The controller-side half, which is the more useful one

**Five times the campaign controller put a claim into a durable document that
ran ahead of its evidence.** Each was caught by someone else. Listed without
softening, because the pattern is more instructive than any individual instance:

1. *"Identical memo miss counts show the computation was identical."* The code
   **had** changed between the two sweeps; the miss count counts a memo call
   that is invariant to that refactor, so it cannot detect a code change at all.
   Identical counts license only "the same ground and the same memo-visible
   work".
2. *"The one-minute load average had not decayed."* Only *instantaneous* idle
   was ever logged. This was an after-the-fact reconstruction stated as a
   measurement.
3. *"~19 bytes per glyph."* Repeated from an implementer's report without
   checking; the real range is 17–23, and 19 is below the floor for any
   realistic tint.
4. *"The bar holds at rungs ≥ 7."* An interpolation across a rung that had not
   been measured — the sweep was 6, 8 and 12. The implementer measured rung 7 at
   66.4 ms and corrected its own controller.
5. *"The perception overlay already paints the observer and marks, so `@` and
   creatures survive."* True **only at band B**, asserted without checking the
   other rungs. **This one would have shipped.** Six keystrokes from the opening
   screen left the walking view drawing terrain with no player marker, no
   creatures and no cursor, while the prose pane beside it narrated the player's
   immediate surroundings.

**A sixth instance was committed inside this very document** and is recorded
under *Confidence Gradient* below, where it happened.

**A seventh is the most consequential of the set, and it is why this document
was revised after the whole-branch review.** The controller wrote *"the wire
does not carry the sub-tile phase"* — a real measurement (a 200-phase sweep of
what happens when a consumer reprojects *without* reconstructing absolute
coordinates) generalised into a claim about the contract itself. The wire does
carry it: the observer block states the centre's own centroid latitude and
longitude, bearing and distance are centroid-to-centroid great-circle
quantities, so the spherical direct problem recovers every facet's absolute
position exactly, and eight-significant-digit quantization is centimetre-scale
against a 1.87 km tile. The true statement is narrower and crate-local —
`clients/game/core`'s parsed mirror of the document drops the observer block,
and the kernel has no inverse of `bearing_to`/`distance_rad_to`. That framing
was already written correctly, in `driver.rs`'s own module doc, by an
implementer; the controller's version travelled instead, into six committed
documents.

One of the six is `book/src/reference/scene-surrounds-v2.md`, a **cross-repo
contract page**, which it left contradicting itself — that page's own address
section states that the observer carries its centroid. The other instances put
wrong numbers, or one wrong claim about this repository's own code, into this
repository's own documents; this one told external client authors that something
straightforward was impossible. Decision 0290's ruling survives untouched, on
grounds that never depended on the false claim — one projection called from both
sides beats two independent arithmetics obliged to agree — and the record now
rests on those instead.

Four of the five numbered above are the same error in different clothes: a true
statement generalised past the ground it was measured on. The fifth is the same
error about code rather than about numbers. The seventh is the same error again
at the largest blast radius available to it: past this repository, onto a page
other repositories read as a contract. The remedy that actually worked was not
more care — it was that implementers and reviewers were briefed to judge on the
merits and did, twice contradicting the controller directly and being right both
times (rung 7's boundary; removing observer-centring from the zoom path).

## What worked

- **Three brief items were measured UNBUILDABLE rather than approximated**, each
  by the implementer that received it, and each with a number. Task 4's third
  layer function had no perception input to take. Task 6's reprojection could
  not be written in the crate that was to hold it — that crate's mirror of the
  wire drops the observer block, and no inverse of the bearing-and-distance
  construction exists to call; reprojecting without one, swept across 200
  sub-tile phases, misplaces at worst 24 of 31 marks, mean 11.5, only 2 of 200
  exact. Task 8's per-call
  lens request failed four independent ways. In every case the answer was a
  measurement and an alternative, not a shrug, and in Task 6's case the
  alternative was better than the design it replaced.
- **A hypothesis was reported as DISSOLVED rather than banked as a pass.** H2's
  subject was removed by Task 6's replan, so its pin is green because nothing
  touched it. The implementer followed that ruling rather than taking the free
  green, and the chronicle says so.
- **A measurement discipline applied unprompted.** An implementer's first
  performance sweep read about 2× high at every rung; `ps` showed another
  campaign's debug suite at 254% CPU. It discarded the run, polled until quiet,
  re-measured, **and recorded the discard in `docs/timings.md`** so the
  difference could never later read as a mystery 2× win.
- **Stopping rather than producing a fifth number.** Three attempts at the
  uncached rung boundary produced three answers. The campaign recorded it as
  load-sensitive and unresolved, and separated out what does not depend on load,
  rather than committing a figure the next reader would anchor on. This
  repository's own notes document two prior campaigns extrapolating wrong from
  exactly such a figure.

## Process facts worth carrying

- **An absorb is stale almost immediately here.** Main moved 27 commits during a
  single task's dispatch. The campaign absorbed four times and `gate-commit`
  went 597 → 679 tests over its life. The check that means anything is
  `git merge-base --is-ancestor`, not "I absorbed recently".
- **The stage gate earned its keep**: green in 1030 s, gating the real merge
  product rather than a branch tip.
- **`make rebaseline` can leave the tree WORSE than it found it.** A job died
  OOM mid-redirect and truncated a committed artifact to empty *before* the
  script reported its non-zero exit — every redirect-written artifact is
  truncated at `open()` time. A truncated-then-committed artifact then reads
  **clean** under the drift check, which compares tree to index rather than tree
  to what the generator should produce. Check `uptime` first; check
  `git status` immediately after; a failed rebaseline is not a no-op to retry
  from. Posted to the board as a `technique`.
- **`make board-post NOTE=` is shell-interpolated, and backticks execute.** One
  post came out mangled with `git status` output injected mid-sentence and had
  to be redacted and reposted.
- **The decision-block reservation scheme is advisory and it collided.** This
  campaign's spec reserved 0286–0295; a sibling campaign's spec reserved the
  same range, landed first, and took 0286. Nothing detected the double
  reservation at either drafting, and nothing would have — the specs were
  written a day apart and neither read the other's header. This campaign shifted
  to 0287–0295 at close. Separately, `docs/decisions/README.md`'s index is
  hand-maintained and nothing enforces it: it carried 190 rows against 210
  decision files before this campaign added its nine, so roughly twenty ratified
  decisions are currently absent from their own index.

## Confidence Gradient

**One bet moved, and the first pass at this section said none did.** That is
worth recording as its own process note: the retrospective's Gradient paragraph
was drafted asserting "no bet moved" — on the reasoning that decision 0022 puts
rendering outside the ledger, so a client-side presentation rebuild cannot
reach a bet about what the *world* can represent — **before**
`book/src/open-questions.md` was actually grepped. It was the sixth instance of
this campaign's own controller-side pattern (a claim written ahead of its
evidence), committed inside the retrospective whose headline is that pattern,
and it was caught by running the grep the standing rule requires rather than by
anyone noticing the reasoning was thin.

The bet that moved is **"the phenomena interface generalizes"** — the emit-seam
row whose instrument The Quire converted from taste into a byte comparison
against the sim's own renderer of the same thirty-one facets. This campaign set
out to move that pin and did not: the wire carries the packet as a *relative
polar offset* and a raster addresses *absolute* tiles, and the render crate that
was to convert between them holds neither the observer block its mirror drops
nor any inverse of the bearing-and-distance construction. The row is re-scored
with what that boundary actually is — a document may be complete and still ask a
consumer for real work before it can be placed in the world's frame, either
spherical trigonometry it writes itself or a dependency on the simulation — and
with the resolution, which is stronger than the instrument it replaces: one
projection called from both sides, so the two pictures agree by construction
rather than by comparison. The boundary as first written here was the wider,
false one (the seventh instance above).

The reasoning that produced the wrong first answer was not stupid, which is why
it is worth naming. It was a *category* argument ("rendering is outside the
ledger, therefore outside the bets") applied to a bet that is specifically about
the boundary between the two. A category argument is exactly the kind of claim
that feels like it does not need checking.

## Follow-ups

Carried from `.superpowers/sdd/followups.md` and the campaign's own ledger,
promoted before the worktree that held them dies. Registry rows were opened for
the substantial ones; the residue is listed here so nothing is lost.

| | |
|---|---|
| **F1** | **A latent equator-band bug in the kernel's windowed nearest-vertex scan.** On the shipped ladder the mesh-addressed lookup and `NearestVertexIndex::nearest` agree on ~2.2M samples with 0 mismatches. Below the ladder they disagree 37 times, and brute force over the full vertex set says the mesh-addressed method is right 37/37 — every one at latitude exactly 0.0000. That contradicts `nearest`'s own doc claim of bit-identity with the full band scan "pinned by an all-levels equality test"; that test evidently does not sample the equator. No shipped path hits it. Repro: uniform random sampling below the grid level. |
| **F2** | **The startup experience.** Genesis is silent; `main.rs` opens the terminal only after it returns. The seam is `BuildDepth`, whose own doc records that earlier rungs are a **byte-identical prefix** of later ones — which is why a staged startup is safe rather than a determinism hazard. Three options: a status line naming the rung; drawing the world at `BuildDepth::Terrain` while the rest builds (this campaign built the renderer that would draw it, but whether the pipeline can *resume* from a partial build is unverified); animating the sculpting. **Blocking unknown: nobody has measured how long genesis takes**, and that decides which option is worth it. |
| **F3** | **A per-call chart lens on the sim side.** Additive, moves no byte, but touches 17 exhaustive `PossessOpts` constructions — which is why Task 8 delivered the guarantee client-side with `strip_sgr` instead. |
| **F4** | **No observer marker at coarse rungs.** The reader has a way home without being able to see where home is. Distinct from the perception layer's deliberate refusal off band B, and that distinction is the row's whole content. |
| **F5** | **An outdoor band A.** There is a ~1000× gap between band A (feet) and band B (1.87 km) with nothing between. This is the Dragon Warrior / 7th Saga model and is deliberate — but nothing recorded it as deliberate, so the next reader would read it as an omission. |
| **F6** | `compose`/`draw_terrain_layer` diverge past `virtual_h` in a degenerate case, reachable at the coarsest rung on a ~800-column terminal. Nothing pins it. |
| **F7** | `active_plate_dims` and the redraw read the terminal size from **two different syscalls**, so a resize landing between them desynchronises the cursor clamp from the drawn plate for one frame. Pre-existing — but Task 9 moved it onto the **default** view. |
| **F8** | The `.min(w)` guard at `spread.rs:247` is now unreachable and reads as live; `core::chart::cell_at` is now test-only; a new colour-consistency window opens on a cache hit (inert under a running process). Three one-line residues of the rebuild. A fourth residue of a different kind: Task 6's placement predicate is frame-specific, which matters before anyone extends it to locked worlds (H4). |
| **F9** | The reader's rung no longer survives a map consultation (deliberate, decision 0293). The escape hatch, if ever wanted, is `enter_map` remembering the previous consultation's rung. |
| **F10** | `follow_the_walker` re-centres every turn unconditionally, so the walk plate has no scroll of its own. A design position, not an accident — recorded so it is not re-derived as a bug. |
| **F11** | **The walk loop costs 24× more per redraw** — 0.089 → 2.124 ms, size-independent, so not the plate. It is `walk_band_scene()` doing a full `Snapshot::parse` plus `purview(0)` on **every** redraw, including keypresses that are just typing in the command pane. Imperceptible today; the wrong shape. |
| **F12** | `CLIENT-world-map-visitedness-is-unwired` — `Visited` is written every turn and read by nothing but a test. This campaign's layer split gives it a natural home in the features layer but does not wire it. |
| **F13** | `TOOL-delve-literal-has-no-cross-repo-gate` is adjacent: this campaign leaves a second client/sim byte-identity coupling in place (the projection pin at `chart.rs:180`), which strengthens the case for a scripted cross-repo check. |
