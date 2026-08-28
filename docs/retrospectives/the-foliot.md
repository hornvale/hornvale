# The Foliot — retrospective

All four stages complete. Written incrementally rather than at the end,
because `.superpowers/sdd/` is git-ignored and dies with the worktree — the
stage-1 half was committed before stage 2 began.

Process lessons, not product. The product is decision
[0316](../decisions/0316-a-local-day-is-a-whole-number-of-ticks.md).

## The campaign's own claims were wrong three times, and measurement caught all three

This is the headline, and it is uncomfortable in a useful way. The spec, the
plan, and the first probe were each confidently wrong about the same defect.

1. **The spec** said a vessel tick *is* a kernel tick, so the `f64` bridge was
   "pure loss." False. A local day was an exact integer of *vessel* ticks but
   `d·B` *kernel* ticks, which is not an integer, so `days_of`'s conversion was
   doing real work. **Implementing the planned fix would have introduced an
   error while the commit message claimed it removed one.**
2. **The first probe** sampled action costs up to `base_cost`'s 10,000, found
   **zero** witnesses, and read as "no defect at all." The error is
   `t·ε / round(d·B)`, so a differing tick needs `t` on the order of a whole
   local day — the probe sat ~25× below where the effect can appear.
3. **The second probe** over-corrected to `MASS_BAND_KG`'s 100,000 kg ceiling
   and overstated the reach. No authored species exceeds 6,000 kg, so the true
   reachable maximum is 121,709 ticks, not 246,000.

**What caught it was a guard written into the plan on purpose.** Task 1.1 was
"prove the premise before Task 1.2 may act on it," with an explicit *if the
witness list is empty, STOP*. It fired. Nothing else would have: the fix
compiled, the tests it would have broken were goldens that a rebaseline would
have quietly accepted, and the commit message would have read plausibly.

**Generalisable rule:** when a plan's task exists to *remove* something, give
the preceding task the job of proving the thing is there, and give it a stop
condition. A fix task cannot audit its own premise.

## A registry row overstated a defect, and that is what aimed the campaign wrong

`TOOL-vessel-clock-duplicates-the-kernel-tick-lattice` called it "one
`Session::charge` rounding from observable." The Escapement's retrospective had
said "not wrong, just collidingly named." I adopted the registry over the
retrospective on the reasoning that the row was later and evidence-cited.

Measured: 213 losses against 211 gains over 5,764 samples, net **−2 ticks**.
Symmetric noise. **The retrospective was closer than the row that corrected
it.** The row is now corrected in place.

Being later and citing evidence is not the same as being right. Both documents
were describing something real — a second lattice does exist — and the one with
more apparent rigour had the worse characterisation of its consequence.

## The real defect was found by verifying the fake one

`Session::charge` converts once and adds integers — correct. `liveness.rs`
accumulates `f64` days in a loop (`:4644`, `:5233`) while the live walk it
reconstructs advances on the integer lattice; the site's own comment says that
divergence "would be a failure by construction." Nobody was looking there.

It is filed as `TOOL-liveness-accumulates-f64-days` and **deferred after three
attempts**, which is the 3-attempt rule working rather than failing. Converting
`WalkState.day` to `WorldTime` passes 442 of 444 vessel unit tests and then
breaks the shared-clock monotonicity invariant by 75 ticks — an ordering
inversion, not rounding. That layer's currency is `f64` days end to end
(`agent_at_fact(.., day: f64)`, three `last_*` fields, `catch_up`'s own
`entry_day`, `hold_step`, `next_awake_day`), so converting one field creates a
boundary the interleaving invariant is sensitive to.

One independent defect fell out of the attempt and survives it:
`TOOL-hold-step-progress-lost-to-round-to-nearest`.

## Removing a reason beats managing a consequence

The shallow fix — define vessel's day as `round(d·B)` kernel ticks — was
offered and declined. It relocates the discrepancy (0.5 tick/day between
vessel's day boundary and astronomy's) rather than removing it.

Quantizing at the draw instead made three things *collapse*, and each had been
real machinery: `local_ticks_of` branched on whether a rotation pin was set,
because a pinned world genuinely put the lattices at different rates; the
queue's `scale` factor `ticks_per_local_day / day_length_std` disappeared
entirely; and `catch_up`'s `day_ticks` parameter went **dead**, which is the
cleanest evidence available that a replay's charge no longer consults the
planet. One test got strictly stronger — "a move costs the same duration on
every world" was asserted to 0.1% and is now exact equality.

**The scoping fact that made an epoch cheap:** the *draw* did not change. The
same two `next_f64()` calls in the same order, with quantization applied after.
So no seed label took an epoch suffix and every pin-isolation test passed
unmodified. Total committed movement: one number in the seed-42 world, eclipse
ground-track longitudes growing 0.0008°→0.017° with elapsed time, one leaf of a
vessel snapshot, and six affect-trace values at exactly 1e-8.

## Four tooling defects, three found by tooling failing rather than by looking

- `make worktree-take` built `campaign/the-gnomon` over an existing remote
  branch of a **completed** campaign and warned about neither. Caught only
  because the board's per-author digest showed 10 posts under that name.
- `make rebaseline`'s parallel fan-out left `proto-elf-generated.md` 249 lines
  short when a generator lost the cargo build lock, reported only as "a parallel
  job failed." Caught by the drift check afterwards.
- A seed-looping test must declare a claim shape (decision 0093) — the lint
  named it; the plan had not.
- A domain's tests may not reach `hornvale_worldgen`; the probe had to build
  through astronomy's own `generate`.

## The same failure shape, five times, in my own verification

Stage 1's headline was that the campaign's claims were wrong three times. The
rest of the campaign kept producing the *same* shape, and it is worth naming
as one pattern rather than five incidents: **every time, I checked a cheaper
neighbour of the real question, and the check looked obviously correct.**

| what I ran | what it actually answered |
|---|---|
| `paste - -` on diff output | zipped a two-line hunk's `<` lines together, pairing *before* with *before* — reported an arousal value moving 16% when both ticks had moved by 1e-8 |
| `grep "$REQ"` on the queue | matched the *superseded* row, whose status text contains the new request's id — reported a false terminal state |
| `grep CONFLICT` on `git merge` | swallowed an "Aborting" error, so a merge that never ran looked like a merge with no conflicts |
| re-deriving climate's phase formula | three attempts from outside, each nearly right; reading the source took one |
| the type-audit report drift | ran `gate-commit` *before* `rebaseline`, twice, and read the resulting red as new information |

None of these was a hard problem. Each was a filter or a shortcut standing in
for the thing I wanted to know. The cost was small every time and the fix was
always the same: **run the thing that answers the actual question**, which was
usually one command away and often cheaper than the substitute.

## A guard in the plan is worth more than diligence at the keyboard

The single most valuable line in this campaign was written before any code:
Task 1.1 existed to prove Task 1.2's premise, and carried an explicit *if the
witness list is empty, STOP*.

It fired. Nothing else would have — the wrong fix compiled, and the tests it
would have broken were goldens that a rebaseline would have quietly accepted.

The same shape paid twice more. Task 2.5's branch table ("no caller reaches
it / a caller can / the trace is inconclusive") meant the clamp decision was
made against a traced answer instead of a guess, which matters because 0190
exists precisely where a confident trace of that question was wrong before.
And Task 3.1 was deliberately sequenced *before* the clamp removal, which is
the only reason the `year_phase` defect was found before shipping rather
than after.

**Generalise it:** when a task exists to REMOVE something, give the preceding
task the job of proving the thing is there, and give it a stop condition. A
fix task cannot audit its own premise.

## Success criteria are worth re-reading at the end, not just at the start

Stage 4 was scoped to the two climate methods the spec's follow-up named. The
plan's success criterion was broader — *no* climate or scene entry point takes
a bare float day — and checking it by counting rather than by recollection
found two more, `weather_at` and `cloud_type_at`.

That was not pedantry. `weather_at` is the exact function whose raw unclamped
day produced the sky/weather incoherence found in Task 2.5, and retyping it
turned that fix from incidental into structural: the two halves of a sky
report now share a type, so a future clamp on either side cannot be silent.

A criterion phrased as an absolute ("no X remains") is checkable by counting
and should be counted. A criterion phrased as a list of edits is not.

## What the type system was hiding

Worth recording as a category, because it is not obvious in advance: the
`StdDays` conflation was not only producing wrong answers, it was **making a
question unaskable**.

Seventeen `Calendar` methods had never been exercised at negative time from
outside the crate, and could not be — the constructor refused the input. The
one existing test reached past the constructor from within. So the status quo
was neither tested-and-correct nor known-broken; it was *unknown*, and the
type was what made it so. Asked for the first time, it answered in seconds:
`year_phase(-100_000)` = −0.49.

A type that forecloses a legal input hides whatever lies behind it, and the
hiding is invisible: nothing fails, no coverage tool reports a gap, and the
absent test looks like an absent need.

## Two judgement calls a reader should be able to find

Both are places where something got weaker and I would rather they were
visible than buried in a diff.

- **Scene's zero-phase test moved from bit-equality to a 1e-6 tolerance.** It
  asserted `grid == t_mean_c + diurnal` exactly, which held only because the
  probe could sit precisely on the zero-phase day. On the tick lattice that
  instant is not representable; the phase is ~1e-9 and the seasonal term it
  produces is ~1.6e-9. The structural claim survives; the exactness does not.
- **One climate call site now rounds** — `surface_mixture`'s probe day is an
  arbitrary fraction of a year, so landing it on the lattice moves the sample
  by up to half a tick. Named at the site rather than absorbed. Every other
  climate call site in the tree was byte-neutral.

## Deferred, each with a home

- `TOOL-liveness-accumulates-f64-days` — the real defect, with the reverted
  attempt and the 75-tick ordering inversion that stopped it.
- `TOOL-hold-step-progress-lost-to-round-to-nearest` — strict progress does
  not survive `from_std_days`'s round-to-nearest.
- `TOOL-worktree-take-does-not-guard-a-used-campaign-name` — built a branch
  over a completed campaign's remote and warned about neither.
- `TOOL-rebaseline-parallel-job-can-truncate-an-artifact` — a generator
  losing the cargo build lock left a generated page 249 lines short, reported
  only as "a parallel job failed".
- `SubstrateField::at` still takes a bare float day: the same defect one layer
  along, named in the code at the call site rather than silently converted.

Follow-ups 1, 3, 4 and 5 of The Escapement needed no rows: they shipped.
Follow-up 3 in particular was *resolved rather than answered* — it asked what
wave-2 verdict the scene floats should get, and v2 deleted the floats.

## Merge friction, measured

Four absorptions of main across the campaign, roughly a hundred commits.
`docs/audits/type-audit-report.md` conflicted on most of them, structurally:
it is a whole-repo aggregate and this branch changed public signatures, so it
drifts on our side whenever it drifts on anyone's. Two mouth refusals, both
cheap by design (milliseconds, box never taken) — the cost is round trips,
and it compounds with branch age.

Landing stage 1 mid-campaign was the right call for exactly that reason. The
argument was not tidiness: it was that a self-contained, gated unit carried
through three more stages pays the absorption toll three more times.

One quiet case worth keeping: a byte golden **auto-merged cleanly** on the
second absorb, which the board warns can be silently wrong. It was correct —
verified rather than assumed. But on the same absorb, The Coercion's two new
call sites used a field this campaign had renamed, git merged them without
conflict, and the result did not compile. A clean merge is not a correct one,
in compiling code as much as in generated artifacts.

## The census deadlock, and a fix that arrived from outside

The census could not land, and the reason was structural rather than a
mistake in the run: `pre-commit`'s golden-pins guard triggers on the census
fixture itself, so a refresh that moves a pinned value can never self-commit.
Worse, the failure was invisible — the delivery script did not check `git
commit`'s exit code, so it reported `rc=0` and "DELIVERED" over an empty
branch.

Another session fixed it while this campaign was diagnosing it, and its
commit message opens by citing this run. Two things worth carrying:

- The third defect it names — the census worktree ran the *censused ref's*
  hooks rather than the queue's, because `core.hooksPath` is relative — would
  have defeated the manual workaround I was about to attempt. Diagnosing a
  deadlock is not the same as knowing all of its causes.
- The operator later **held** a re-run rather than spending 17 minutes
  reproducing byte-identical goldens, and was right. I verified the claim
  (no census-determining code had changed) rather than taking it on trust,
  which took one command.

## Seven calibration pins, and only three announced themselves

The census refresh moved seven pins. `census-check` reported four; running
the lab suite surfaced three more; and of the homophony test's four
sequential assertions **only the first was reported**, because `assert_eq!`
aborts and masks the rest — the shape that cost the-granary one chamber run
per hidden assertion.

Reading all four out of the new census with a single query took one command
and replaced four ~17-minute round trips. When a test carries several pins
against one derived quantity, re-measure all of them in one pass; the
reporter can only ever name the first.

## A green commit gate is not a green crate

The merge went red in the chamber on a test I had never run. `gate-commit`
executes the **sub-floor tier only** — a deliberately cheap filter — and
`an_eclipse_carries_an_exact_tick_alongside_its_quantized_day` is not in that
roster. I ran `hornvale-scene`'s full suite after Task 4.1, then made a
*larger* change (the v2 schema) and verified it with `gate-commit` and
`make world-check` alone.

The failure is not that I skipped a check; it is that I let a passing check
stand in for a different one — the same shape as this campaign's other five,
one level up. A sub-floor pass says the obvious breakage is absent. It says
nothing about the crate whose public schema I had just rewritten.

The test itself was correct to fail: it is The Escapement's, and it pins the
property v2 reverses (schema still v1, the float still present, a tick added
*beside* it). It had to invert with the schema, and its replacement now
asserts the **absence** of the old fields — the load-bearing half, since a
test that only checked the tick fields exist would pass just as happily with
the floats left behind.

**Rule:** after changing a crate, run that crate's own suite before the gate,
not instead of it. Re-run across all eight touched crates afterwards: 26
suites, 0 failures — which is the check that should have preceded the first
submission.

## Cost and process notes

- `census_sentinel` has **zero** sub-floor roster entries, so a branch that
  moves census values goes green under `gate-commit` and only reddens at the
  stage gate. That is documented in CLAUDE.md and still surprised this campaign.
- Decision numbers: I picked `0287` by `max+1`, which the convention forbids.
  Main's ceiling was already `0308`. `make decision-block` reserved 0316–0325.
- `docs_consistency` resolves a GitHub URL against the **local** tree, so citing
  an unwritten retrospective is a red gate, not a forward reference.
