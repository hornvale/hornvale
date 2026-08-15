# The Glasshouse — retrospective (IN PROGRESS)

**Status: written mid-campaign, at a machine handoff.** Decision 0020 asks for
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

This is now decision 0133's durable half. Two sub-lessons that cost real time:

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
already reserved `0132` for a decision not yet written. **A forward reference to
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

---

## HANDOFF — state at the pause (2026-08-14)

**Branch:** `campaign/the-glasshouse`. **Absorbed:** `origin/main` at `1f3589e2`.

| item | state |
|---|---|
| Task 1, 2 | complete (decision 0132, terrain epoch) |
| H4 | diagnosed and restated; decision 0133; green |
| Task 3 | complete — `91f19f2f` |
| Task 4 | **committed** — `4c806ece`, but see below |
| Task 5 | **WIP, UNVERIFIED** — `a5f1a373`, 4 known reds |
| Tasks 6, 7 | not started |

**Immediate next actions, in order:**

1. **Triage Task 5's four reds** (`history_emit` ×2, `range_readout`,
   `solitary_tongue`). None triaged. The lexicon golden is the interesting one.
2. **Evaluate spec §3.2's three bounds** — never produced, because the agent
   died before reporting. Area-weighted mean within 1 K of +14 °C, equatorial
   within 3 K of +26 °C, polar within 5 K of −25 °C. Remember `⟨sin²lat⟩ = 1/3`.
3. **Answer Risk 4** — which of `FREEZE_C` / `HABITABLE_MIN_C` / `ICE_C` /
   `TEMPERATE_BASELINE_C` actually moved behaviour. Unanswered.
4. **Decide `k`** (§6 above). Carve-out: fidelity call, Nathan's.
5. **Absorb `origin/main`** (49 commits, The Repose, tip `c9fb7701`) — deferred
   deliberately, not forgotten. Then re-run the `hollow_readout` probe as the
   geometry oracle.
6. Tasks 6, 7.

**Expected red baseline — do not read as new breakage:** `hornvale-lab` is
**45 failed / 421 passed**. 42 census-schema, 2 from Task 2, 1
(`anomaly_injection`) owed to `gnomon-injection.sh`. A failure of a *different
shape* is a real finding.

**Two host-pinned refreshes are owed at close, not one:** the census
(`scripts/census-run.sh`) **and** `scripts/gnomon-injection.sh`. Both on lefford.

**Open items:** H4a is blind to aggregate decalibration below ~10% at 30 seeds
(more seeds is the only honest sharpening); The Hollow's spec §4 does not yet
record H1's and H4's restatement the way §4.0 records this campaign's own
supersession.

**On a fresh machine:** `docs/timings/test-baseline-<host>.tsv` is keyed on
`hostname -s`, so a new host **forks the baseline** — the first gate there
records silently and cannot alarm. Rebuild the board binary
(`cargo build --release --manifest-path tools/board/Cargo.toml`); nothing does it
for you. Start `make prewarm` immediately after `make worktree-take`.
