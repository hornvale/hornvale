# The Glasshouse — retrospective (IN PROGRESS)

**Status: written mid-campaign, extended at a second handoff.** Sections 1-7
were written at the machine handoff of 2026-08-14; sections 8-11 and the
HANDOFF block were added at the close of the resumed session on 2026-08-15,
which absorbed The Staff, settled `k`, and ran both owed refreshes. Decision 0020 asks for
this at close; it is being started early because `.superpowers/sdd/` is
git-ignored and dies with its worktree, and this campaign's Stage B reasoning
(ledger entries #10–#28) would otherwise not survive the move. The product
state lives in the plan's checkboxes and in `HANDOFF` below; what follows is
process, per 0020.

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

## HANDOFF — state at 2026-08-15

**Branch:** `campaign/the-glasshouse` at `f8014156`, pushed, tree clean,
`gate-commit` green at 2745/2745. **Absorbed:** `main` through The Staff
(152f278c).

| item | state |
|---|---|
| Tasks 1–5 | complete |
| `k` | **settled at 0.30**, `THERMOSTAT_RESIDUAL_FRACTION` (decision: Nathan) |
| census refresh | **done** — `c0211b18`, canonical box, 979.5 s |
| gnomon-injection refresh | **done** — `c252f9a8`, canonical box |
| Risk 4 | open |
| Tasks 6, 7 | not started |
| `toponymic_shape` | **deliberately red**, see below |

**Both host-pinned refreshes are complete.** Verify that nothing
physics-moving has landed since `c0211b18` before relying on that — the check
is whether anything under `kernel/`, `domains/`, `windows/worldgen/src` or
`windows/lab/src/metrics.rs` has changed — and if nothing has, **do not re-run
either**. A census is once per campaign and costs ~16 min of the canonical box.

**Next actions, in order:**

1. **Risk 4** — which of `FREEZE_C` / `HABITABLE_MIN_C` / `ICE_C` /
   `TEMPERATE_BASELINE_C` actually moved behaviour. Cheaper than planned: the
   refreshed census carries the after-arm directly. The structural half is
   already established — `TEMPERATE_BASELINE_C` is a baseline for a *deviation*
   and cannot switch; the other three are gates, and `FREEZE_C` at −10.0 sat on
   the pre-campaign median of −10.49.
2. **Task 6, the classifier gate.** Two independent lines already favour "no
   code change" — P1″ un-falsified, and settlement rising 620 → 826
   occupations — but the gate *is* the re-measurement, so run it.
3. **Task 7** — all six criteria against Stage A's frozen baselines, then close.
4. **`toponymic_shape`.** 27 of 29 pairs confirm; 2 invert by 0.033 and 0.111,
   both inside one sampling standard error (~0.14 at n≈25, against a 0.15
   separation). Its `forall` rule does not match its own sample sizes. **Freeze
   a decision rule before measuring again** — choosing one after seeing which
   pairs inverted is the same error as lowering `SHAPE_SAMPLE_FLOOR`, which was
   already refused once. It is *not* in the sub-floor roster, so `gate-commit`
   will not show it; it surfaces at `gate-stage`.
5. **Definition of Done** — chronicle entry, this retrospective, a Confidence
   Gradient re-score, and spec §10's registry updates (correct `SKY-19` and
   `CLIM-cold-attractor`, resolve `CLIM-astronomy-unmeasured`, move
   `CLIM-greenhouse` and `CLIM-biome-classifier-mixing` to in-progress, add a
   hypsometry row, note this campaign as `CLIM-ice-albedo`'s unblocker).

**The headline, for the chronicle:** median land temperature **−11.99 →
−3.649 °C**; ice-dominant worlds **651/1000 → 187/1000**; settlement at seed 42
**620 occupations across 217 sites → 826 across 302**. Twenty-nine sky columns
are byte-unchanged across a refresh that added a new seeded draw, which is the
stream-isolation contract holding over 1000 worlds.

**On a fresh session:**

- **The gate ladder is not what this campaign started with.** `make gate`,
  `ci`, `gate-fast`, `gate-full` refuse. Use `gate-commit` locally;
  `gate-stage`/`gate-campaign` dispatch to one strictly serial lane on the
  canonical box and **fail closed** if it is unreachable.
- **This campaign's decisions are 0134 and 0135**, not 0132/0133 — The Staff
  took those. Any prose citing the old numbers for the craton clamp or the
  criterion restatement is stale.
- Two stashes (`a04ffb26`, `a9e470f9`) are **superseded and already landed**.
  Do not re-apply them; the stash stack is shared with other sessions.
- `docs/timings/test-baseline-<host>.tsv` is keyed on `hostname -s`. This
  session ran on `MacBookPro`; the earlier half ran on `ambrose`.
