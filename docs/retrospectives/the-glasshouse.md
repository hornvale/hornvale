# The Glasshouse — retrospective

**Status: complete.** Written in three sittings across two machines — sections
1–7 at the machine handoff of 2026-08-14, sections 8–11 at the resumed
session's close on 2026-08-15, and sections 12–14 plus the readout at the
campaign close the same day. Decision 0020 asks for this at close; it was
*started* early because `.superpowers/sdd/` is git-ignored and dies with its
worktree, and that decision is the reason sections 1–11 exist at all: two of
the three sittings ran in worktrees that no longer hold their scratch.

**The product readout is section 12.** Everything else is process, per 0020.

## 1. The premise in the brief was the defect

H4's failure was handed to the session as *"402 caves against 262 expected is
~8.6σ — not sampling noise."* Every lead followed from that sigma, and the
sigma was fictitious: it assumed the bucket's cells were independent Bernoulli
draws over a field that H5, four lines below it in the same battery, **asserts
is spatially clustered ≥90%**. Corrected for measured overdispersion the excess
is 1.09σ.

The generalisable part is not "check the statistics." It is that **a criterion
and its neighbour can encode contradictory models of the same field and both
stay green for a year**, because nothing compares one assertion to another. H4
assumed independence; H5 asserted dependence; they sat eight lines apart. What
surfaced it was not review but a population change that finally made the two
disagree out loud.

Second-order: the failing buckets were ~93% `LavaTube`, and the campaign
*already knew* LavaTube had collapsed 9837 → 2379. The −76% bucket fall and the
−76% LavaTube fall were the same number, and nobody connected them until the
kind mix was printed per bucket. **When two measurements move by the same
percentage in the same week, try assuming they are the same event.**

## 2. Fixing an over-firing criterion is not done until the injected defect goes red again

The corrected H4 was written, shipped green, and looked right. A mutation test —
`GATE_NOISE_MEAN` 0.5003 → 0.5100, firing the gate 11.85% hot — **passed it**,
while the *original* H4 would have caught it. The repair had traded a false
positive for a false negative, and nothing in the green run said so.

This is now decision 0135's durable half. Two sub-lessons that cost real time:

- **A mutation must be shown to have taken effect before its verdict means
  anything.** Here: 48,316 → 55,080 caves. A no-op mutation produces exactly the
  same green as a well-caught one.
- **Weakening is invisible in the direction you are looking.** The session was
  watching "does the false positive stop?" — it did. Nobody was watching "does
  the true positive still fire?" until the mutation forced it.

## 3. Two plan defects, both found by the same three minutes of grep

`dispatching-hornvale-subagents` step 1 (verify the brief against the code,
immediately before each dispatch) found a defect before *both* dispatches:

- **Task 3's test sketch drew from a leg no world uses.** It passed the bare
  `Seed(seed)` to `generate_star`/`generate_anchor`; production derives first
  (`system.rs:44-46`, `world_seed.derive(streams::ROOT)`). Every assertion in the
  sketch — finite, in range, deterministic — is satisfiable on that wrong world.
  It would have shipped green while testing nothing, and "the draw is correctly
  wired" was the entire claim of the task.
- **Task 4's file list was incomplete** (7 `ClimateInputs` construction sites in
  3 files; the plan named 2 of the files), and it listed `FREEZE_C` beside three
  `domains/climate` constants when `FREEZE_C` lives in `windows/worldgen` with
  **two private test mirrors** — while `domains/climate` separately defines a
  confusable `FREEZING_C = 0.0`.

The step's own guidance says its accuracy comes from being *late and narrow* —
one task ahead, with the tree in the state the implementer will find it. That
held. Both defects were in claims about code, not in reasoning, and neither was
visible from the plan text alone.

## 4. A wall of identically-shaped reds is where a differently-owned one hides

Registering one metric reddened 45 lab tests, all with the same
`rows.csv header does not match study … schema` panic. The implementer reported
them honestly and concluded all would clear at the census refresh. **44 would;
one would not.** `anomaly_injection` names study `gnomon-injection`, whose
fixtures live under `windows/lab/tests/fixtures/injection/`, are absent from
`docs/generated-paths.txt` (so no drift check sees them), are untouched by
`make rebaseline`, and are never mentioned by `census-run.sh`.

This is `windows/lab/CLAUDE.md` §2b happening **verbatim a second time** — that
section exists because The Hearsay did the same thing and it was caught only
after its census refresh had already run. The generalisation: *shape is not
ownership.* The error was a reasonable inference from a uniform symptom, which
is why it needs a mechanical check (the §2b grep) rather than more care.

## 5. Duplicate decision numbers are minted by parallel campaigns and nothing catches them

Two branches independently minted `0131`. `make preflight` and
`docs_consistency` are both blind to it; it surfaced only as a merge conflict in
the *generated* digest. Renumbering cost 57 references across 29 files, and the
renumber itself nearly created a second collision — `hollow_readout.rs` had
already reserved `0134` for a decision not yet written. **A forward reference to
an unwritten decision is invisible to every check and to the person renumbering.**

## 6. "Pick the constant from Earth" was impossible as written

The plan's Task 4 Step 2 says pick `k` and the anchor *from Earth, not from the
census*. The anchor obeyed this correctly and robustly: `THERMOSTAT_ANCHOR_K`
fixes the area-weighted mean at `S = 1`, and the land mean falls out of that plus
hypsometry rather than being a second free parameter — so terrain moving does not
require re-fitting it.

`k` could not obey it, for a structural reason nobody noticed at plan time: the
model is `effective_S = 1 + k·(S − 1)`, so **at `S = 1` the `k` term vanishes
identically**. Earth's anchor is *at* `S = 1`. A single anchor point cannot
constrain a slope through it. With no Earth-based way to fix `k`, the implementer
fell back on a population criterion and chose the **boundary value of an
arbitrary sweep** ({0.4, 0.5, 0.6, 0.7} → 0.4, "the most-compensating value in
the evaluated range").

Two lessons, one specific and one general:

- **Identifiability is a plan-time question.** "Fix this constant from datum D"
  should be checked by asking whether D actually varies the constant. Here it
  provably does not, and the check is one line of algebra.
- **A chosen value at the edge of its sweep means the sweep did not bracket the
  answer.** That is a signal to widen the sweep or change the criterion, never to
  take the endpoint.

The identified fix is a second constraint at `S ≠ 1` — the faint-young-Sun
condition (`S ≈ 0.75` early, surface water persisting) is the natural physical
one, and is population-independent, which is the property that matters.

## 7. Process notes worth carrying

- **A subagent's `git add -A` sweeps the controller's uncommitted work.** The
  controller must commit or stash before dispatching into a shared worktree.
  `.superpowers/sdd/` is safe only because it is git-ignored.
- **`--no-fail-fast` on the first run, not the second.** A fail-fast workspace run
  reported 2 failures where there were 3, and the wrong number was reported
  upward before being corrected. `CLAUDE.md` already says this; the session
  ignored it and paid a full suite re-run.
- **An absorption is not automatically physics-neutral.** The first (90 commits)
  was, and was *proven* so by re-running the `hollow_readout` probe and getting
  byte-identical figures. The second (49 commits, The Repose) deletes lines from
  the island-arc elevation path, so the same proof must be run rather than
  assumed.

## 8. Three tests predicted their own futures, and all three were right

The most transferable thing this campaign produced is not a finding about
climate. It is that **a test carrying its own decision rule survived a physics
change that broke everything pinned to a bare number.**

- `range_readout`'s assertion message said "this may mean P1″ holds again" —
  and P1″ did, once the world was warm enough for arid land to exist for an
  arid-affine people to relocate *to*.
- `gathering_calibration`'s doc said its narrowing margin "would become a
  finding rather than a re-pin if it kept going". It kept going, a fourth
  consecutive time, and was recorded as a finding rather than re-pinned
  silently.
- `founder_collision`'s liveness test asserts that its own seeds still collide,
  so when 1741 stopped colliding the test said "this now proves only that an
  uncontested world builds" instead of passing vacuously. It has caught a
  cleared seed on two consecutive re-pins.

Against that, the tests that cost the most were the ones pinned to an exact
tuple with no rule attached. `range_readout` was pinned to `(10, 0, 2, 0)` and
interrupted the campaign **twice** for reasons unrelated to its claim, because
a *directional* prediction had been frozen as four integers. It is now back to
the direction it preregistered.

**Write the branch table, not the expected value.** A pinned number tells the
next reader what was true once; a rule tells them what to do when it stops
being true.

## 9. Aggregate shape is not per-item ownership — twice, in both directions

Section 4 records reading a wall of 45 identically-shaped reds as one cause
when 44 shared it and one did not. The resumed session made the *same* error
inverted: it read a shape histogram ("9 golden mismatches") as if it
partitioned 25 failures, reported "~19 mechanical rebaselines" to Nathan, and
was wrong — parsing the panic **per test** gave 8 golden-file and 17
hand-written assertions.

Both times the mistake was reasoning about a *population* of failures from a
summary of their shapes. The fix is the same in both directions and is
mechanical rather than attentional: attribute every failure individually
before classifying any of them.

## 10. Four ways to hide a failure from yourself, all found here

Every one of these produced a confident wrong statement before being caught,
and all four are the same underlying error — **optimising the output of a step
whose success had not yet been established.**

- `| tail -60` on a backgrounded run truncated the **output file itself**, so
  three failures' details were unrecoverable and the run had to be repeated.
- `cmd > f 2>&1; echo "EXIT=$?"` at the end of a chain made the task
  notification report the *echo's* status. A gate that exited 2 was reported as
  exit 0, twice.
- `git stash apply <sha> | tail -6` hid an apply that had **refused** on a
  dirty file. The surviving summary looked like success, and a content grep was
  what actually caught it.
- `git rev-parse <full-sha>` was used to ask "does the canonical box have this
  commit?". It echoes any 40-hex string back **without touching the object
  store**, so it answered yes about a commit that had never existed — because
  the SHA had been *transcribed from a 12-character prefix and the remaining 28
  invented*. `git cat-file -e <sha>^{commit}` is the check that asks.

The last one deserves its own line: **derive a SHA into a variable, never type
one.** `HV_CENSUS_REF` feeds `reset --hard`, which is exactly the hazard root
`CLAUDE.md` names.

## 11. The gate ladder changed mid-campaign, and the collision was structural

The Staff landed while this campaign was mid-flight and rewrote the gate ladder
(`make gate` and friends became refusing signposts). It also minted decisions
0132 and 0133 — **both of which this campaign had already minted**, for
entirely different things. Section 5 predicted this class and said nothing
catches it; it then happened again, doubly, to the same campaign.

Two things made the recovery cheap, and both are worth copying:

- **The renumber was keyed on provenance, not on the number.** A citation line
  was rewritten only if it was *absent from main's copy of the same file*. 63
  lines across 31 files moved and 5 were correctly left as The Staff's. A bare
  grep would have rewritten those five, and section 5's own warning (a search
  for `012`+digit once matched `0.0126`, a standard deviation) says why.
- **The new gate created a circular block**, and naming it was most of the fix:
  `gate-commit` runs a sub-floor tier including the lab calibrations, so the
  stale census blocked *every* commit — including the merge that would have
  brought the new gate in. Broken by refreshing the census at the **pre-merge**
  tip, justified by verifying The Staff touches only `kernel/CLAUDE.md` under
  `kernel/`+`domains/` and nothing under `windows/lab/src/metrics.rs` or
  `studies/`.

---

## 12. The readout — six preregistered criteria, four met

Measured on the refreshed 1000-world census (`c0211b18`, canonical box) against
Stage A's frozen baselines. **The before-arm was recomputed from the
pre-refresh CSV as a positive control and reproduced all five frozen figures
exactly** — median −11.9886, spread 44.5907 K, ice 65.10%, r = +0.9227, and
the spinning subset's +0.9814 against the spec's stated +0.980. The instrument
is verified against the preregistration, not merely consistent with it.

| # | criterion | baseline | after | verdict |
|---|---|---|---|---|
| 1 | median `mean-land-temperature-c` within 5 K of +8.6 °C | −11.99 | **−3.649** | **FAIL** — 12.25 K short |
| 2 | spread p95−p5 ≥ 31.2 K (70% of baseline) | 44.5907 | **34.9175** | **PASS** (78.3% retained) |
| 3 | no biome class > 50% | ice 65.10% | **taiga 25.00%** | **PASS** |
| 4 | `dominant-soil-order` no longer frozen | leptosol 100% | **leptosol 100%** | **FAIL** |
| 5 | Earth's `S=1` lands in the 25th–75th pct of temperature | 88.1th | **58.9th** | **PASS** |
| 6 | r(`insolation-rel`, T) < 0.75 | +0.9227 / +0.9814 spinning | **+0.3267 / +0.3642** | **PASS** |

**The spec's named falsification did not occur.** §4.1 said that if (2) and (3)
proved incompatible — if every parameterization that spread temperature kept a
class dominant — that was the finding and the biome specials were an
independent defect. Both passed simultaneously. The specials were a symptom.

**No constant moved after unblinding.** `k` was settled at 0.30 before the
refresh; nothing was retuned to rescue criterion 1.

### Risk 4 — which threshold constants actually moved behaviour

Per-world proxy (`mean-land-temperature-c`; the constants are per-*cell*
thresholds, so these counts bound the behaviour change rather than measuring it
directly, and are labelled as such deliberately):

| constant | value | worlds below, before | after |
|---|---|---|---|
| `ICE_C` | −20.0 | 305 | **67** |
| `FREEZE_C` | −10.0 | 529 | **278** |
| `HABITABLE_MIN_C` | −5.0 | 623 | **456** |
| `TEMPERATE_BASELINE_C` | 14.0 | 951 | **954** |

Three moved hard; the fourth did not, and **the inherited explanation for why
was wrong in a way worth recording.** The handoff said `TEMPERATE_BASELINE_C`
"is a baseline for a deviation and cannot switch". It *can*: felt weather emits
heat or cold once the deviation passes ±2 °C, and deification crosses at ~15 °C.
The true reason is quantitative, not structural — the population is still ~9.6 °C
below the emission margin at the median, so an 8.3 K warming moved only 3 of
1000 worlds across it (941 → 946 emitting; 699 → 576 crossing the pantheon
floor, which *is* a real behavioural change the structural story would have
missed entirely). An inherited diagnosis is a hypothesis, and this one was
right by accident.

## 13. The soil column: a scope error wearing a null's clothes

Criterion 4 returned zero movement under the largest intervention available.
Read as a measurement that is an emphatic null; it was not one.

`classify_soil` opens with `depth < 0.25 || slope > 300.0 → Leptosol`, and every
branch below it reads temperature or moisture. Measured over 20 worlds and
307,588 land cells: **72.1% of land never reaches the climate ladder** —
61.09% by depth, 11.04% by slope — and where the ladder does run it is healthy
(8 orders, max 30.19%).

Three things generalise, and the third is the one that cost time:

- **Spec §2.3's premise was falsified, not merely unmet.** It said "`leptosol`
  follows the same elevation" and therefore would move when elevation did. It is
  overwhelmingly the *depth* arm, and depth is not elevation.
- **The plan's gate table was not exhaustive.** Its two rows were "no class >50%
  AND soil unfrozen → nothing needed" and "a class still >50% → independent
  defect". The actual result — biome half clean, soil half frozen — matched
  neither, and a table with an `AND` in one row and a different predicate in the
  other cannot be exhaustive by construction. **When a branch table's rows are
  not negations of each other, it has a hole.**
- **A statistic that refuses to move is indistinguishable from a weak effect by
  inspection.** The only thing that separated them was attributing the deciding
  branch instead of inferring it — the same lesson as §4 (*shape is not
  ownership*), one layer down: *magnitude is not mechanism*.

## 14. The one rule that had to be frozen blind, and how to make that checkable

`toponymic_shape` failed on 2 of 29 pairs and its `forall` rule justified itself
by claiming a 0.15 predicted gap was "several sampling standard errors". At the
smallest sample the test admits (20 names apiece), the SE on a difference of two
proportions is 0.158. The gap is **0.95 SE**. The justification was wrong by
3–5×, and the test had been failing on sampling noise.

The rule had to be replaced *before* re-measuring (0016), and the session
already knew both inverted margins — which is exactly the situation where
"frozen before measuring" degrades into a promise. The move that makes it
checkable instead of trusted:

> **Choose a rule whose verdict is invariant across every conventional value of
> its free parameter, then say so.** The two inversions sit at 0.28 SE and
> 0.79 SE — both under a *single* standard error — so `k` = 1, 2 or 3 all
> forgive them. A threshold that cannot have been fitted, because no admissible
> choice would have changed the outcome, needs no trust.

Two supporting notes:

- **A significance filter beats a rate criterion here, and the reason is
  directional.** `range_readout`'s P2 precedent (a frozen majority rate) would
  also have passed, but a rate stays equally lax forever. A threshold denominated
  in the sample's own error *tightens automatically* as worlds gain names: the
  same rule that forgives 0.11 at n=21 refuses it at n=200. A correction should
  get stricter with better data; a loosening does not.
- **The negative control was run and mattered.** Setting `k` to 0.5 turned the
  0.79 SE pair red with a *behavioural* failure, proving the assertion path is
  live rather than vacuous. §2 of this retrospective is the same lesson, and it
  needed applying twice in one campaign.

## 16. Nineteen reds nothing was watching, and two the campaign chose to keep

**`gate-commit` was green at 2745/2745 for this entire campaign while the
branch carried nineteen full-workspace failures.** Both facts are true and
neither is a bug: the commit gate runs only the sub-floor tier, and not one of
the nineteen is in it. Nothing above `gate-commit` had been run since the
census refresh, so the physics landed and its downstream witnesses were never
re-read.

Three lessons, in increasing order of how much they cost.

**A handoff that reports a gate must name which gate.** "gate-commit green at
2745/2745" was accurate and read as "the branch is clean". After decision
0132 split one gate into three, a green from the cheapest is evidence about a
tier, not about a branch — and the tier it covers is *by design* the one that
excludes anything slow enough to be interesting.

**`--no-fail-fast` on the FIRST run, and this campaign paid for that twice.**
Section 7 already records a fail-fast run reporting 2 failures where there
were 3. At the close a fail-fast lane gate reported **2 where there were 19** —
it cancelled with 39 tests still running — and the number was carried into a
status report before a local `--no-fail-fast` pass corrected it. The lesson did
not fail to be learned; it failed to be applied to a *dispatched* run, where
the fail-fast default lives in someone else's script.

**Attribute individually; the shape histogram lies in both directions.**
Section 9 records reading 9 golden mismatches as if they partitioned 25
failures. Parsing all nineteen panics individually gave four classes that no
count of shapes would have separated: 3 byte goldens, 4 numeric pins, 8
liveness pins where the world moved out from under a named NPC or concept,
2 deliberate reds, and — hiding among them — one witness whose re-read
overturned another campaign's published finding.

### The two that are staying red, deliberately

`water_reading`'s discharge floors and `wetness_reading`'s R-8 are **not**
witnesses and were not re-pinned. Both are instruments that noticed something
true:

- seed 42's loud-reach cells fell 34 → 16 and strong crossings 8 → 2, because
  a higher sea level shortens drainage paths and shrinks catchments. The
  threshold they are measured against was calibrated on pre-epoch catchments.
- one riparian room in thirty-five now reads `dry`, because the riparian noun
  and the dry clause are downstream of two different functions of moisture,
  which the invariant's "by construction" wording assumed away.

Nudging either floor would have deleted the only instrument that noticed, so
both are left firing with their diagnosis, their mechanism and their two
candidate repairs written at the failing constant, and both are filed as
registry rows (`MAP-waterfall-threshold-mis-scaled`,
`LOC-riparian-dry-overlap`). **A campaign that merges with named, explained
reds is more honest than one that merges green because it moved two numbers.**
The cost is real and should be stated: the stage and campaign gates are red on
this branch, and will stay red until those rows are picked up.

## 17. A published finding was overturned by a campaign that never touched it

The Gnomon measured its anomaly report at recall@10 = 0.5667 over 120
injection pairs against a preregistered 0.60 bar, published the shortfall as
its headline, and marked its registry row `refuted`. It also did something
better than that: it pinned the tally as an explicit **witness**, with a
comment demanding that any move be re-read rather than updated, in the same
commit, across the chronicle, the registry row and the heavy-tier roster.

This campaign moved it — to **73/120 = 0.6083**, above the bar — without
touching `REPORT_SIZE`, `TAIL_DEPTH_BAR`, the scorer or the evaluable surface.
Only the worlds changed.

**The pin did its job and caught a mover it was not watching for.** It was
written to defend against a change to the *report*; what arrived was a change
to the *world*, from a campaign six weeks later with an unrelated subject. That
is the transferable part: a witness guards a number against everything
upstream of it, and the set of things upstream of a number is larger than the
set its author was thinking about.

The re-read did not confirm the report either. The crossing is one hit out of
120; at the bar the standard error is 0.0447, so the old figure sat 0.75 SE
below and the new one sits 0.19 SE above, and the distance between them is
0.66 SE. **A 120-pair battery was never able to adjudicate a 0.60 line**, and
the original refutation looked clean only because it landed on the low side of
a distribution wide enough to reach both sides of the bar. Recorded as "cannot
tell" — not refuted, not confirmed — with the row returned to open.

That makes three separate places this campaign found a preregistered threshold
sitting inside its own sampling noise: `toponymic_shape`'s `forall` (0.15 =
0.95 SE), H4's inherited 8.6σ (really 1.09σ, section 1), and this. The pattern
is worth naming as a check rather than three anecdotes: **a preregistered bar
needs a power calculation, not just a number frozen early.** Freezing is the
right discipline and all three did it. A threshold is only a decision rule if
the instrument can resolve it, and one line of arithmetic at freeze time
settles that.

## 15. Follow-ups promoted out of scratch

- `MAP-soil-depth-freeze` (new registry row) — 61% of land under 25 cm of soil.
  Needs its own preregistration and census refresh; deliberately not fixed here.
- `CLIM-insolation-draw-measure` (new registry row) — the residual 12 K to Earth
  is in the orbital draw's measure, not the climate model. This is the successor
  campaign's target, not another thermostat constant.
- `tree_line_m`'s 40 m/degree slope is not Earth's (~57 needed; the function
  reaches 0 m only at 100° latitude). Provenance recorded in the doc comment,
  re-fit deferred as census-moving.
- The faint-young-Sun constraint at `S ≈ 0.75` remains the identified way to
  make `k` identifiable from Earth (§6), and is still unbuilt.

---

## HANDOFF — state at 2026-08-15, second close session

**Branch** `campaign/the-glasshouse` at `c8f9c6a6`, worktree
`.claude/worktrees/the-glasshouse`. `gate-commit` green at **2745/2745**.
Absorbed `main` through The Staff (`152f278c`).

**The campaign's own work is COMPLETE.** Tasks 1–7, Risk 4, the classifier
gate, the six-criteria readout, both decisions (0134/0135), the chronicle, the
Confidence Gradient re-score, the registry updates and this retrospective are
all done and committed. What remains is **not campaign work** — it is the
post-refresh witness sweep that nothing had run.

### Both host-pinned refreshes are complete — DO NOT RE-RUN EITHER

Census `c0211b18`, gnomon-injection `c252f9a8`, both on the canonical box.
Verified at this session's start and unchanged since: the only edits under
`kernel/`, `domains/`, `windows/worldgen/src`, `windows/lab/src/metrics.rs`,
`studies/` are comment-only `0132`→`0134` renumbers plus test-module-only
changes. **A census costs ~16 min of lefford and is once per campaign.**

**`make gate-campaign` DISPATCHES A CENSUS REFRESH.** `scripts/lane-sets.tsv`
maps the `census` set to `census-run.sh` — the real refresh, not
`census-check` — and it fires on the standard pre-merge command. This session
dispatched one by accident and killed it while still queued (no compute ever
started). Either dispatch the sets individually, or kill the census job
immediately: `ssh lefford 'pgrep -af "lane-run.sh census"'` then `kill -9`.

### What is left: 11 failures and one blocked set

Counted with `--no-fail-fast`. **The lane's `gate` set is fail-fast and
reported 2 of 19** — never trust a dispatched gate's failure count.

| # | where | status |
|---|---|---|
| 2 | `water_reading` floors, `wetness_reading` R-8 | **deliberate reds**, settled — see §16 |
| 8 | the `heavy` tier | open |
| 1 | `clients` — `the_shape_matches_the_sims_own_ascii_render` | open |
| — | `seam-guard` | **blocked by a lane bug**, never actually ran |

**The 8 heavy failures**, all census-refresh witnesses, none caused by this
session's commits:

```
hornvale::session_cost                a_possessed_turn_stays_within_its_ceilings
hornvale-lab::disposition_calibration non_raiding_peoples_hold_their_genesis_flagship_far_longer_than_raiders
hornvale-lab::the_fare_calibration    weathering::the_fares_preregistered_readout
hornvale-lab::the_mire_calibration    the_mires_preregistered_readout
hornvale-worldgen::generalist_distinctness substituting_goblins_niche_for_humans_is_detected
hornvale-worldgen::generalist_distinctness human_is_not_goblin_recentred
hornvale-worldgen::repose_exposure    repose_exposure_readout_matches_the_committed_fixture
hornvale-worldgen::occupancy_readout  occupancy_readout_is_current
```

They split three ways and the split decides the method:

- **`repose_exposure` and `occupancy_readout` OWN COMMITTED ARTIFACTS.** The
  heavy tier is an authoring path, so these must be regenerated **on lefford**
  (`heavy-run.sh` carries the canonical-host guard). Mechanical.
- **Three are PREREGISTERED CALIBRATION READOUTS** (`disposition`, `the_fare`,
  `the_mire`). These are the same class as The Gnomon's witness, and §17 is
  the warning: **re-read them, do not re-pin them.** Any one could be another
  campaign's published finding quietly reversing. Each needs its own judgement
  and, if a verdict moves, its chronicle + registry row + roster string
  re-stated in the same commit.
- **`session_cost` and the two `generalist_distinctness`** are ordinary
  witnesses; re-read and re-pin with the mechanism named.

### The seam-guard lane bug — file this, it is not ours

`seam-guard` refused: *"refusing to run on a dirty working tree."* The
`artifacts` set runs `regenerate-artifacts.sh` in the **same** lane worktree
earlier in the serial queue and leaves it modified; `seam-guard` runs later and
refuses. **So seam-guard has been silently not-running on every campaign gate
that also dispatched artifacts** — it reports rc=2 in 28 s with no verdicts,
which reads like a failure and is actually a no-op. Independent of this branch.

### `outboard` is red on lefford only

3 `tools/board` `sync` tests fail there (`None` where `Some(1000)` expected —
a recorded sync invisible to a second `Repo` handle, smelling like worktree
common-dir resolution). The same suite is **177/177 green on the Mac at this
branch**, and `tools/board` is byte-identical to `main` here. Pre-existing on
other refs too. Not this campaign's.

### Decisions already taken — do not relitigate

- **The Gnomon is "cannot tell"**, not confirmed (Nathan, this session). Its
  witness now pins `(73, 120, 0, 0)`; registry row off `refuted`; chronicle
  postscript written; roster string updated. §17 has the reasoning.
- **`water_reading` and `wetness_reading` stay red**, documented at the
  failing constant and filed as rows (Nathan, this session). §16.
- **`toponymic_shape`'s rule is frozen** at 2 SE and must not be retuned.
- **`k = 0.30`**, settled. Criterion 1 missing Earth by 12.25 K is the
  published finding, not a thing to fix.

### Two lane-waiting traps, both cost this session time

**`pgrep -f "<pattern>"` MATCHES ITS OWN WRAPPER.** A poll loop built as
`while ssh lefford 'pgrep -f "lane-run.sh .* $REF"'; do sleep 120; done` never
exits: the remote `bash -c` wrapper's command line contains the pattern
string, so `pgrep` finds itself and the condition is permanently true. The
same self-match is visible in a bare `pgrep -af "lane-run.sh census"`, which
lists its own `bash -c pgrep …` row. Match on something the wrapper cannot
contain, or use the tool built for this: **`make lane-wait JOB=<id>`**, which
is opt-in blocking and is the sanctioned way to wait on a lane job. This
session's watcher looped for hours and the lane results were obtained by
polling `make lane-status` by hand instead — no conclusion depended on it, but
the wait was wasted.

**The lane is SHARED and strictly serial across campaigns.** At this handoff it
was occupied by two other refs (`a386b784` artifacts; `ed8a717b` seam-guard and
heavy). A dispatch does not start when you make it — it takes a queue position,
and `heavy` alone runs ~30 min while a census runs ~16. Read `make lane-status`
before assuming a dispatch is running, and budget queue time, not just run time.
That is an accepted cost of decision 0133, not a fault.

### The tension to resolve before merging

A **fully green** stage/campaign gate is incompatible with §16's two
deliberate reds as they stand. Either the merge accepts two named, explained
reds, or those two rows get picked up and genuinely fixed first — which is
campaign-sized work (a 0106 provenance re-fit and a new census metric for one;
a shared-predicate repair for the other). That is a scope call, not a
technical one.
