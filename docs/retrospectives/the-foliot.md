# The Foliot — retrospective

**In progress.** Stage 1 is complete; stages 2–4 (the `StdDays` instant/duration
split, the negative-time sweep, climate and scene) are not started. Written
incrementally because `.superpowers/sdd/` is git-ignored and dies with the
worktree.

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

## Two of my own verification habits failed in the same way

Both were checking a cheaper neighbour of the real question, and both looked
obviously correct:

- **Pairing diff output with `paste - -`** zips a two-line hunk's `<` lines
  together, pairing *before* with *before*. It reported an arousal value moving
  0.638→0.739 (16%) when both ticks had moved by 1e-8 on their own lines. Nearly
  rejected a legitimate golden. **Compare per-line, not per-hunk.**
- **A monitor filtering the queue with `grep "$REQ"`** matched the *superseded*
  row, because that row's status text contains the new request's ID. Reported a
  false terminal state. Exact field match (`awk '$2 == r'`) fixed it.

## Merge friction, stated because it is structural rather than bad luck

Two mouth refusals, both on `docs/audits/type-audit-report.md`. It is a
whole-repo aggregate and this branch changes public signatures, so it drifts on
our side whenever it drifts on main's — which is every campaign touching a
`pub` boundary. Main moved 38 commits, then 27 more, within hours.

The refusals are cheap by design (milliseconds, box never taken); the cost is
round trips, and it compounds with branch age. Generated aggregates are
resolved by **regeneration, never text-merge**. Separately, and more quietly: a
byte golden auto-merged *cleanly* on the second absorb, which is the case the
board warns can be silently wrong. It was correct — verified rather than
assumed.

## Cost and process notes

- `census_sentinel` has **zero** sub-floor roster entries, so a branch that
  moves census values goes green under `gate-commit` and only reddens at the
  stage gate. That is documented in CLAUDE.md and still surprised this campaign.
- Decision numbers: I picked `0287` by `max+1`, which the convention forbids.
  Main's ceiling was already `0308`. `make decision-block` reserved 0316–0325.
- `docs_consistency` resolves a GitHub URL against the **local** tree, so citing
  an unwritten retrospective is a red gate, not a forward reference.
