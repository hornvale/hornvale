# The Tailrace: The Read Side of Log Bounding — A Campaign Design

**Program:** The Penstock (`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`), stage 7.
**Branch:** `campaign/the-tailrace`, from `origin/main` @ `c39444ba6`.
**Decision block:** 0236–0245.
**Status:** G3 approved 2026-08-24. Planning.

---

## 0. What this campaign is, and the thing it is not

**It does not remove a single fact from the ledger.** Committed bytes at the
end of a session are byte-identical before and after this campaign, by
construction and by the drift check. A reader who takes "log bounding" to mean
"the log gets smaller" will find this campaign delivered nothing.

What it delivers is the **precondition** that both existing halves of stage 7
silently assume and neither has: that *nothing depends on the raw history
being there.* Today six production folds in `windows/vessel/src/liveness.rs`
walk an agent's entire committed `agent-at` trail on every evaluation, so
removing facts — by abstention at the commit site or by compaction after it —
changes behaviour. That is not a storage question, and it is why stage 7 has
been carveable in two incompatible ways for as long as it has existed.

So this campaign proposes a re-carve, and it is the first thing to accept or
reject at G3:

| | delivers | when |
|---|---|---|
| **7a — The Tailrace** | the read side: nothing folds raw history any more | this campaign |
| 7b — `UNI-intention-is-structured` | the typed, compositional intention, so a per-errand commit preserves the `why?` chain the per-step provenance carries today | enterable; costs an epoch |
| 7c — `TOOL-log-bounding-epoch-fact-lifetime` | fact lifetime: what may leave, and how the seed plus the surviving prefix re-derives the world | enterable; `MEM-1`'s melt is its diegetic sibling |

The ordering is forced, not chosen. 7c cannot be safe while the folds read raw
history (this campaign). And 7b sits between them because **the trail is
content, not bookkeeping**: each step's `provenance` is authored prose — *"went
down to the river it knew (thirst)"*, *"fled the uncanny ground (fear)"* — and
it is rendered, by `windows/historiography/src/lib.rs:53` and
`cli/src/repl.rs:407`. Dropping steps without first making the intention carry
its own `why?` would delete readable content, which is a fidelity cut and a
carve-out, not an optimisation.

## 1. The finding that motivates it, and why no existing instrument saw it

§6.1 established that the commit *rate* is flat rather than falling, and
concluded correctly that an append-only log with a non-summable rate grows
without bound in RAM. That argument is about **storage**, and it is the only
argument the metaplan makes for stage 7.

There is a second one, and unlike the first it is measurable today rather
than projected. The folds are O(history) and run per agent per tick, so **tick
cost grows with session length** — the log costs
CPU while it sits there, before it ever costs a byte too many. Neither sibling
instrument can see this:

- `windows/vessel/examples/agent_scaling.rs` sweeps *agent count* at a fixed
  20 ticks, and attributes its superlinear residual to the population-wide
  per-creature roster reads — an O(agents²) shape. Holding ticks fixed makes a
  history term invisible to it by construction.
- `windows/vessel/tests/suite/tick_commit_budget.rs` sweeps ticks but measures
  facts committed, never time.

So the campaign opens with the missing instrument (§3, stage 1), and §4 reports
what it measured.

## 2. The six folds, and why each is bounded

Every one of these reduces an unbounded history to bounded state. Read on
`origin/main`; line numbers are `windows/vessel/src/liveness.rs`.

| fold | line | what it walks | what it reduces to | bounded by |
|---|---|---|---|---|
| `agent_sightings` → `integrate_thirst`, via `drive_at` | 806, 835, 884 | every `agent-at` ≤ `t` | the accumulated integral, plus the last sighting's day and room | the interval since the last `drank` |
| `hunger_at` | 2405 | the same trail, `HUNGER` params | likewise | the interval since the last `eaten` |
| `fatigue_at` | 2233 | every `rested` fact | the latest `rested` day | O(1) — a max |
| `believed_water` | 905 | every `agent-at` ≤ `t`, ∩ water | a `BTreeSet<RoomAddr>` | reachable water rooms |
| `hazard_memory_memo` | 1176 | every `agent-at` ≤ `t` | a `BTreeMap<RoomAddr, f64>`, latest-visit-wins | cells visited |
| `build_emitter_scan` | 985 | every roster member's `agent-at` ≤ `t` | a `BTreeSet<RoomAddr>` of alarm cells, plus per-emitter timelines | cells visited × emitters |

Three multipliers make this worse than the table suggests, and all three are
in the same call path:

- `shared_believed_water` (`:1323`) calls `believed_water` **once per
  co-located peer**, so the history walk is multiplied by band size.
- `build_emitter_scan` is threaded the **full roster**, so it is O(agents ×
  history) inside a per-agent call — the O(agents²) term the sibling bench
  suspected and the history term, compounded.
- `step_with_occupancy`'s `begin` (`:4945`) folds `DRANK`, `RESTED` and
  `EATEN` from scratch per creature per tick.

**The thirst integral is exactly incrementalisable, and this is the load-bearing
claim of the design.** Reading `integrate_thirst`'s own segmentation
(`:848–856`): the bounds are `last_drank`, each sighting strictly inside
`(last_drank, t)`, then `t`. So

```
thirst(t) = A(d_n) + rate(temp(p_n, d_n)) * (t - d_n)
```

where `A(d_n)` is the accumulation through the last sighting `(d_n, p_n)`, and a
`drank` fact sets `A := 0`. Three scalars and one room, queryable at any `t`
ahead of the frontier in O(1).

**Four traps found while reading, recorded here so the plan inherits them
rather than rediscovering them.**

1. **Clamp at read, never incrementally.** `integrate_thirst` clamps to
   `[0, 1]` on the **total**. An accumulator that clamps as it goes diverges
   the moment a creature exceeds the ceiling and not before, which is the worst
   possible failure schedule for noticing.
2. **The segment's temperature is sampled at its START.** So the accumulator
   carries the segment-start position and day, not the current ones.
3. **The reset's own position comes from before the reset.** The first segment
   begins at `last_drank`, and its governing position is "the latest sighting
   with `d <= last_drank`" — a sighting *earlier* than the reset. So a `drank`
   fact must capture the position in force at that instant, not merely zero the
   sum.
4. **`agent_sightings` SORTS, and the sort is not commit order.** It sorts by
   `(day, RoomAddr)` — `a.0.total_cmp(&b.0).then_with(|| a.1.cmp(&b.1))`
   (`:820`, and again at `:4325` where the tick folds `frozen` plus its own emitted moves). A fold advancing in commit order would break same-day ties
   differently, and the tie-break selects the *position* that governs the next
   segment, hence its temperature, hence the integral. **This is a live
   divergence, not a theoretical one**, and it is precisely what the FOLD ≡
   SCAN property exists to catch: the incremental fold must reproduce the
   sorted order's tie-break, or the batch function must be changed to commit
   order and the change justified as its own decision. Same-day sightings are
   not exotic — the action clock can charge less than a day for a step.

**`agent_sightings` is the hub, and the work is to DELETE it, not cache it.**
Five of the six folds route through that one function (thirst, hunger, hazard,
belief, the emitter scan), which makes it look like the obvious thing to
maintain incrementally. It is not: its output is a `Vec<(f64, RoomAddr)>`
timeline, O(history) in *size* however cheaply it is kept up to date, so caching
it bounds nothing at all. The bounded objects are the *reductions over* it — an
accumulator, a set, a map — so each consumer gets its own and the shared
timeline goes away. That is why stage 3 is named "delete the hub" rather than
"migrate six call sites": it is a smaller piece of work than the call-site count
suggests.

**Past-`t` queries are real and are already documented in the tree.**
`last_fact_day_at_or_before` (`:4451`) exists precisely because catch-up's
replay loop evaluates many instants across a span, and its doc says why a
whole-history fold cannot serve it: the folded value "could be looking
chronologically PAST the day it is being asked about." So the primitive must be
restorable at an earlier position, not merely advanceable. For the three
reset-partitioned drives this is cheap and exact — the reset facts *are*
checkpoints, so restoring at position `p` means starting from the last reset at
or before `p` — which is the bound the fold needs anyway.

## 3. The mechanism, and why the store The Forebay shipped is not it

`kernel/src/derived.rs`'s `Derived<K, V>` with `Validity::Ledger` is the
obvious candidate and it is the wrong one. `Validity::is_stale` returns true
once a fact touching a watched `DepKey` commits after the entry's recorded
position, and a stale entry is evicted on read and recomputed from scratch. For
these folds the watched dependency is `(subject, agent-at)`, which commits
**every tick for that very subject**. So the entry is stale every tick, every
read is a miss, and the recomputation is the O(history) walk being eliminated:
a cache with a structurally guaranteed 100% miss rate.

Invalidation and accumulation are different operations, not two policies over
one. What these folds need is **extend-on-commit** — a value that *advances*
with the log rather than being invalidated by it:

```
advance: (state, &Fact) -> state          O(1), exactly once, in commit order
restore: position          -> state       from the nearest checkpoint at or before it
read:    (state, t)        -> value       closed form ahead of the frontier
```

`Derived` is left untouched. The new primitive sits beside it in the kernel
(generality is §6.6's standing direction, and `Derived` set the precedent that
the *mechanism* is kernel-side while the *tenants* are not); the six folds are
vessel's, and stay there. `Ledger::commit` gains no hook — the tick already
holds the facts it is about to commit (`step_with_occupancy` returns them
before they are appended), so the caller advances the fold explicitly and the
kernel's write path is unchanged.

**Nothing this campaign builds enters the save** (§8). The fold is session
state, re-derivable from the ledger, exactly as `HomeNavCache` and
`RoomMeshMemo` are.

### The correctness ladder, strongest first

Metaplan §7's ladder, with the subject changed from views to folds:

1. **Type-level** — the fold hands out an immutable borrow; a read cannot
   advance the state it reads.
2. **Property — FOLD ≡ SCAN.** Over random ledgers, the incremental result is
   byte-identical to the batch result. This is the campaign's central property
   and every tenant gets it.
3. **Property — advance-exactly-once, in commit order.** Double-advancing and
   skipping are the two bugs this primitive can have; both are silent.
4. **Adversarial — chaos-rebuild.** §7's chaos-eviction, adapted: discard the
   accumulator at *every legal opportunity* and rebuild from the ledger, and
   assert byte-identical output. If the fold is genuinely a fold, no schedule of
   discards is observable.
5. **Master oracle** — the census drift check, and the committed-artifact drift
   check for the byte-identity claim in §0.

## 4. Measured baseline

Instrument: `windows/vessel/examples/session_length_scaling.rs`. Seed 42, **50
agents held fixed**, 200 ticks in bands of 20, `--release`, on the Mac (the
host this program's other probes use — metaplan §4:108, §6.5:737). **Four runs**, box
load average 11.8–15.3 throughout.

The deterministic columns are the control and they do not move across bands:
facts committed per band 1,117–1,150; `HomeNavCache` searches 550–556. **The
workload is identical band to band; only the history grows.** Band 1 is
excluded from every statistic (2,279 searches against ~553 steady — a cold
cache, not a sample of the same process), and the instrument declares the
exclusion rather than applying it silently.

### The decisive measurement: `drive_at` alone

`drive_at` is timed directly, 200 back-to-back calls on one fixed probe agent,
at every band. Nothing inside that span scales with anything but the history it
walks — no A\*, no roster read, no occupancy, no commit.

| | run 1 | run 2 | run 3 | run 4 |
|---|---|---|---|---|
| µs/call, band 2 → band 10 | 337 → 737 | 243 → 751 | 388 → 1019 | 414 → 917 |
| `k` (µs/call per fact) | 1.89 | 2.50 | 3.33 | 2.90 |
| `r²` | 0.820 | 0.975 | 0.972 | 0.948 |
| **elasticity** | **0.86** | **1.24** | **1.07** | **1.02** |
| monotone rises | 6/8 | 7/8 | 7/8 | 6/8 |

The probe agent's own history grew 130 → 322 facts (**2.48×**) across those
bands, and cost tracked it: **elasticity 0.86 / 1.24 / 1.07 / 1.02, median
1.045**, with `r²` ≥ 0.82 in every run and ≥ 0.94 in three of four. `drive_at`
is, to measurement precision, *proportional to the history it walks* — a pure
walk with no meaningful fixed part at these depths. `k` spans 1.89–3.33 µs/call
per fact across runs; the elasticity is the stable statistic and `k` the
load-sensitive one, which is why H2 below is written against the elasticity.

**`C` is deliberately not quoted as a floor.** The sampled range starts at 130
facts, so the intercept is an extrapolation far outside the data and comes out
negative on two of three runs. An earlier draft of the instrument printed "the
history term is 106.5% of the total" from exactly that; the share is now
suppressed when `C < 0` and the elasticity leads, because it needs no
intercept.

**In absolute terms: one `drive_at` call costs 0.74–1.02 ms at 322 facts of
history**, and `drive_at` is called more than once per creature per tick.

### The whole tick, for context — and a correction to this campaign's own earlier reasoning

| | run 1 | run 2 | run 3 | run 4 |
|---|---|---|---|---|
| `k` (ms/tick per fact of mean per-agent history) | 2.72 | 2.70 | 3.25 | 3.67 |
| `r²` | 0.842 | 0.843 | 0.839 | 0.791 |
| history term's share at band 10 | 71.2% | 78.2% | 70.2% | 79.8% |

**~70–80% of per-tick cost at 144 facts/agent of mean history is the history
term**, stable across four runs (`r²` 0.79–0.84).

That stability is a correction worth recording, because three earlier runs of
the same column gave +2.05×, 0.70× and 0.64× — disagreeing about the *sign* —
and this campaign's first diagnosis of that was box contention. Contention was
real (one band ran 3.5× out of line at load average 18 while the calibration
yardstick moved 19%, which is how we learned a single-threaded integer yardstick
does not capture memory-bandwidth contention). But it was **not the main cause.**
The main cause was fitting against `ledger_len`, which moves only 1.55× across
this run because genesis commits ~12,500 facts before the walk begins, while
`facts_of`'s `(subject, predicate)` index means a fold walks only its own
agent's postings. Regressing on a near-constant produced the unstable sign. With
the right x-axis the whole-tick column is *good* evidence, not junk — and the
lesson is that "the box is noisy" was the more flattering explanation and the
wrong one.

### What this establishes, and what it does not

**Establishes H1.** `k > 0` on the decisive column in all four runs with
`r²` ≥ 0.82, and an elasticity indistinguishable from 1.0 (median 1.045). Per-tick cost is
linear in history, so **total session cost is quadratic in session length.**

**Does not establish** which of the six folds carry the term beyond `drive_at`
itself — that is stage 1's attribution profile. Nor does it connect to §6.4's
8.76 ms/agent-tick, which is a level claim about a different bench at a
different agent count; joining the two would repeat the level-vs-shape
conflation §6.4 itself identified in §6.2.

**A known limitation with a known remedy, stated rather than apologised for.**
This instrument *observes* history rather than *controlling* it: the sim grows
the history and the bench watches cost follow. Two consequences. The sampled
range is only 2.48×, which is why `C` is not identifiable and why the elasticity
leads. And history depth is perfectly correlated with wall-clock order, because
history only grows — so a band's condition cannot be revisited, and any drift in
machine availability arrives disguised as a history effect. The `calibrate()`
yardstick divides out CPU speed and demonstrably not memory-bandwidth
contention.

The remedy is a **synthetic depth sweep**: build ledgers at chosen depths and
time `drive_at` against each, **interleaved** (10, 10,000, 10, 10,000, …) so
depth and elapsed time are uncorrelated by construction. That widens the range
from 2.48× to ~1000×, makes `C` identifiable, and turns H2 into a paired
before/after comparison at identical depths. `kernel/examples/query_scaling.rs`
is the same construction for the same reason, so the pattern is already in the
tree. It is stage 1's third deliverable.

It does **not** replace this instrument. A synthetic sweep has no real tick, so
it cannot produce the 70–80% share — and that share is the number that makes the
campaign worth doing. One answers *is this real and does it matter*; the other
answers *what exactly is the law*.

### Attribution across the six folds

Stage 1's remaining deliverable, and stage 4's entry gate. §4's decisive
measurement establishes only that `drive_at` itself is history-proportional; it
does not say which of the other five folds carry the rest of the whole tick's
70–80% history-proportional share. This section closes that.

**Method: direct timing, not a `samply` profile — a change from this
campaign's own earlier plan, and worth stating why.** A sampling profiler
attributes wall time to whatever symbol the program counter was in when a
sample landed, which fails exactly where this question is hardest: a fold
small enough to be inlined into its caller (plausible for `fatigue_at`, the
cheapest of the six) disappears from the profile's symbol table entirely, and
a missing symbol reads as "zero cost" when it may only mean "not separately
addressable." Direct timing — `session_length_scaling.rs`'s existing
`probe_fold_us` pattern, extended to the other five folds — has a known call
count (`FOLD_REPS` back-to-back calls, timed as one span) and no
symbolication step, so it cannot mistake absence-from-a-sample for absence-of-
cost.

Each of the five siblings gets its own probe function
(`probe_hunger_us`/`probe_fatigue_us`/`probe_believed_water_us`/
`probe_shared_believed_water_us`/`probe_hazard_memory_memo_us`), timed on the
SAME fixed probe agent `drive_at`'s own decisive measurement uses, and
regressed against the SAME x-axis (`probe_history`, that agent's own
`agent-at` count) so all six elasticities are directly comparable.
`shared_believed_water` and `hazard_memory_memo` are threaded the full
50-agent roster, matching what `step_with_occupancy` actually passes — not a
cheaper single-agent proxy. `hazard_memory_memo` constructs a FRESH
`PrimaryAfraidMemo` inside the loop on every one of the `FOLD_REPS`
repetitions: the type's own doc says "one per tick" for exactly this reason,
and sharing one across repetitions at the same `(ledger, t)` would serve every
call after the first from cache, measuring the memo's hit rate rather than the
fold — reading, wrongly, as this fold being nearly free.

**Results — four runs (the fourth added in fix round 1, to re-confirm the
pattern survives the roster-distribution code added for that round), box load
average 7.0–16.3, reported without picking the flattering one.** The
instrument's own module doc already documents a build-up of history from
wall-clock order making load contention indistinguishable from a genuine
effect at high load; that recurred here. Run 1 sat at load average 14.8–16.3
throughout and every elasticity below is suppressed relative to runs 2–4
(load 7.0–15.0) — same code, same seed, same probe agent, lower signal at
higher load, exactly as §4's own history with `drive_at` predicts.

| fold | elasticity (run1/run2/run3/run4) | r² (run1/run2/run3/run4) | final-band µs/call (run1/run2/run3/run4) |
|---|---|---|---|
| `drive_at` (decisive, restated) | 0.18/1.14/0.81/1.33 | 0.010/0.824/0.544/0.861 | 1772.14/764.55/809.35/1011.08 |
| `hunger_at` | 0.21/1.09/1.22/0.79 | 0.012/0.824/0.913/0.372 | 2375.58/770.26/770.28/1018.29 |
| `fatigue_at` | -0.86/0.53/-0.18/-0.20 | 0.019/0.050/0.031/0.025 | 0.98/0.13/0.14/0.19 |
| `believed_water` | 0.37/1.01/1.10/1.39 | 0.124/0.763/0.881/0.882 | 8878.72/4416.68/4344.16/6324.77 |
| `shared_believed_water` | 0.40/1.10/1.18/1.27 | 0.092/0.856/0.971/0.887 | 10199.48/4983.09/5349.93/6804.43 |
| `hazard_memory_memo` | 0.47/1.06/1.21/1.19 | 0.132/0.865/0.975/0.896 | 127122.78/73460.38/75564.65/96630.13 |

Reading the three higher-signal runs (2–4, load 7.0–15.0 at launch — still
noisier than a quiet box, but each shows the DECISIVE column's own `r²` above
0.5, unlike run 1's 0.010): **four of the five siblings — `hunger_at`,
`believed_water`, `shared_believed_water`, `hazard_memory_memo` — show an
elasticity in the same broad band `drive_at`'s own decisive measurement
occupies (0.8–1.4 against `drive_at`'s 0.8–1.3): each is, to measurement
precision, proportional to the history it walks, the same shape §4
established for `drive_at` alone.** `hunger_at`'s run-4 `r²` (0.372) is the
one exception inside that trio of runs — a reminder that noise does not fall
uniformly across folds within a single run, not evidence against the shape
(its run-2/run-3 `r²` of 0.824/0.913 and elasticity of 1.09/1.22 are as clean
as `drive_at`'s own). `fatigue_at` is the one fold that is cleanly NOT
history-proportional: its elasticity has no stable sign across any of the
four runs (0.53, -0.18, -0.20 in runs 2–4) and its absolute cost is three
orders of magnitude below every other fold's (0.13–0.19 µs/call at the final
band, against 765–96,630 µs/call for the rest) — consistent with a fold
whose OWN history (committed `rested` events) stayed near-empty across this
run's 200 ticks, uncorrelated with the `agent-at` count it is plotted against.
It carries no material share of anything.

**Absolute magnitude, not just shape, is what stage 4's gate asks about — and
here the five history-proportional folds separate sharply. All ratios below
are the same method throughout: the ratio of each fold's own final-band raw
µs/call to `drive_at`'s own final-band raw µs/call, run by run across the
four runs — stated explicitly so a reader can recompute it from the table
above rather than trust it.** At the final band, `hunger_at` costs almost
exactly what `drive_at` costs across every run (within ~1–30% of it — the two
are structural twins over the same `integrate_thirst` machinery, differing
only in which predicate resets the fold, so this is the expected result, not
a surprise).

`believed_water` costs **5.0–6.3× `drive_at`'s own per-call figure**
(8878.72/1772.14=5.01, 4416.68/764.55=5.78, 4344.16/809.35=5.37,
6324.77/1011.08=6.26) and `shared_believed_water` costs **5.8–6.7×**
(10199.48/1772.14=5.76, 4983.09/764.55=6.52, 5349.93/809.35=6.61,
6804.43/1011.08=6.73). **Two candidate mechanisms could drive this, and this
task can only rule one IN and the other OUT for the specific probe agent
measured, not rank them in general:**

1. `believed_water`'s outer fold calls the terrain's water-truth check
   (`is_water`) once per raw `agent-at` posting, where `integrate_thirst`
   only evaluates terrain at *segment* boundaries (`bounds.dedup()`-
   collapsed) — strictly fewer than the raw posting count. This candidate
   applies to every call, regardless of what the fold finds.
2. `believed_water` also runs a bounded `plan_to_room` A* search **per
   distinct water room found** (`liveness.rs:908-931`, the `seen.into_iter
   ().filter_map(|r| plan_to_room(...))` line) — a real cost `integrate_thirst`
   has no equivalent of, and one a reviewer flagged as plausibly the larger
   driver.

**For THIS probe agent, candidate 2 is ruled out, not merely unmeasured.** A
temporary diagnostic (added to a local working copy only, reverted before
this commit — not part of the measured instrument) reconstructed
`believed_water`'s own water-membership fold using the crate's public
`is_fresh_water`/`RoomId::unpack` (the same decode technique
`fold_depth_sweep.rs` already uses for the private `room_from_text`) and
counted the probe agent's DISTINCT water rooms at the final band: **zero**.
That is consistent with the "probe agent has no known water" diagnostic this
task's own probe functions already print at every band, in every run — the
`seen` set `believed_water`/`shared_believed_water` fold over is empty
throughout this entire run, so `plan_to_room` is called **zero times** for
this probe. Candidate 1 (the per-posting vs. per-segment terrain-check
count) is therefore the sole applicable explanation for the 5–7× figures
measured here. Candidate 2 remains a real, unmeasured cost for a
DIFFERENT — better-water-located — agent: this task did not measure a probe
that ever finds water, so it cannot say how much `plan_to_room` would add for
one, only that it adds nothing to the specific numbers reported above.

`hazard_memory_memo` is the extreme case: **72–96× `drive_at`'s own per-call
figure** (127122.78/1772.14=71.7, 73460.38/764.55=96.1, 75564.65/809.35=93.4,
96630.13/1011.08=95.6), because `build_emitter_scan` — reached only through
this fold, and timed as part of it per the interface note above — scans
every one of the 50 roster members' own full histories on every single call.

**A caveat on that last number that changes how it should be read, not
whether it matters.** Production shares ONE `PrimaryAfraidMemo` per tick
across the whole 50-agent roster (`DriveMovements::step_with_occupancy`,
`windows/vessel/src/liveness.rs:4701`: `afraid_memo` is built once and passed
by `&mut` into every creature's `hazard_memory_memo` call for that tick), so
`build_emitter_scan`'s O(roster × history) cost is paid **once per tick**,
amortized over 50 creatures. This probe's fresh-memo-per-call design — required
so the measurement is not the memo's hit rate — instead pays that scan cost on
**every** repetition, which is the correct thing to do to expose the fold's
true re-derivation cost and its history elasticity, but it means the raw
73,460–96,630 µs/call figures (runs 2–4) are an upper bound on this fold's
marginal per-creature production cost, not a literal per-tick-per-creature
charge. Even dividing generously by the roster size (50) to approximate the
amortized share — a rough bound, not a measurement — leaves roughly
1,470–1,930 µs of per-creature cost, still comparable to or larger than
`drive_at`'s own 765–1,011 µs/call. The elasticity (1.06–1.21 in runs 2–4) is
unaffected by this caveat: the scan's own cost grows with the SAME roster
history that grows `probe_history`, so the fold's shape claim stands
regardless of how its level is amortized. A probe splitting
`build_emitter_scan`'s cost from the per-creature latest-visit fold it feeds
would sharpen this further; that is a followup, not a gap in this task's own
conclusion.

**Stage 4's entry gate.** `believed_water`, `shared_believed_water` and
`hazard_memory_memo` all show `k > 0`, high `r²` at low load, and an
elasticity in the same broad band `drive_at`'s own decisive measurement
occupies across all four runs — and each costs *more* per call than
`drive_at` itself, not less.
**Stage 4 is entered.** Migrating belief and hazard to the incremental-fold
primitive is motivated by measurement, not merely plausible from reading the
code: all three carry a material share of `k`, and `hazard_memory_memo` in
particular is very likely the single largest per-call cost among the six
folds even after the memo-amortization caveat above is applied.

### Step 3's grounding: `fold_depth_sweep.rs`'s `RESET_EVERY`, and an unbounded `S`

`fold_depth_sweep.rs`'s periodic-reset regime picks `RESET_EVERY = 20` as an
authored guess at `S`, production's postings-per-drink, and says explicitly
that a synthetic bench cannot ground that guess because it knows its own reset
cadence by construction. `session_length_scaling.rs` can, because it drives a
real session: it now reports the probe agent's own cumulative `drank` count
alongside its `agent-at` count, at every band.

**The result is not "the ratio is near 20" and not "materially different" — it
is the third, more significant case the task brief named explicitly: the
probe agent committed ZERO `drank` facts across all 200 ticks, in all four
runs (the original three, plus the fourth added in fix round 1).** Its own `agent-at` history grew to 322 postings with no reset at all.
`S` — postings since the last drink — is therefore **unbounded** for this
agent over the run's whole span, not merely large: the SINGLE-RESET regime
`fold_depth_sweep.rs` isolates as a deliberate edge case ("a 'never drinks'
regime production does not reach", in that file's own words) is, for this
particular agent, simply the regime it is actually in. The probe agent is
the roster member `session_length_scaling.rs` picks by construction — the one
with the MOST `agent-at` postings after the first band — which is one
plausible reason it never reaches water: it may be the one member whose
derived home/resource geometry puts water out of comfortable reach, which
would also make it more likely to keep moving (and thus accumulate the most
postings) rather than settling near a water source the way a better-placed
member would. That is a hypothesis, not a finding this task measured; the
finding is the zero count itself.

**What this does and does not unsettle.** `fold_depth_sweep.rs`'s SHAPE claim
— that periodic resets at a bounded `S` isolate the O(h) term from the
O(s·h) term the single-reset regime exposes — survives this finding
unchanged; it is a claim about the mechanism, not about any one agent's
cadence. What it does unsettle is treating `RESET_EVERY = 20` as *typical*:
at least one real derived agent's own production cadence is not "resets every
~20 postings", it is "never resets in 200 ticks", which sits at the opposite
extreme from the periodic regime and close to the single-reset regime's own
`S == H`.

**Fix round 1 — the selection effect, closed.** The probe agent is not a
random roster member: it is chosen, BY CONSTRUCTION, as the roster member
with the MOST `agent-at` postings after the first band — exactly the member
most likely to still be walking rather than settled near water. So "the
probe never drinks" risked being an artifact of that choice rather than a
fact about the population, and the fix this round closes that by reporting
the WHOLE roster's own `drank` distribution alongside the probe's figure
(`session_length_scaling.rs` now computes `drank_counts` over the full
50-agent roster at every band, the same indexed `facts_of` read
`folded_counts` already uses).

**At the final band, the roster's own distribution is: min 0, median 9.0, max
47 `drank` facts; 23 of 50 agents (46.0%) have drunk ZERO times** — a value
identical across repeated runs, as expected: it comes from the deterministic
columns (facts committed, who drank how often), not the wall-clock-timed
ones, so it does not carry the timing noise the six-fold table does.

That number sits close to, but on the "most agents drink" side of, an even
split: a slim majority (54%) of the roster has drunk at least once, against
the 46% — nearly half — that, like the probe, never has. Read against the
task brief's three-way branch: this is neither cleanly "most agents drink and
only the tail does not" (46% is too large a share to call a tail) nor
cleanly "few or no agents drink" (a majority *has* drunk). **The honest
reading is a population that splits close to evenly between the two
regimes**, with the probe landing — by construction — on the larger-history,
never-yet-drunk side. That still means the single-reset (quadratic) regime is
closer to a worst case than to strictly typical (a bare majority avoids it),
but it is a much larger worst case than "one outlier agent": on this roster,
essentially half the population is in it. `RESET_EVERY = 20` describes
neither the never-drinks half (for whom `S` is unbounded within this run's
200-tick horizon) nor, exactly, the drinking half either — the drinking
half's own median of 9 `drank` events over 200 ticks (≈1 every 22 ticks) is
at least in the right neighborhood of a `drank` roughly every 20-some ticks,
though that is ticks, not the postings `RESET_EVERY` actually counts, and the
two are not the same unit without knowing postings-per-tick for that half
specifically — which this task did not separately measure.

Whether the roster's OTHER members drink more regularly (making this probe
agent unrepresentative) or a large share shares its cadence (making
`RESET_EVERY = 20` optimistic for close to half the population) is now
measured, not unmeasured: **it is closer to the latter than most readers
would expect.** This task measured one probe agent's own ratio as scoped, and
reports the near-even split loudly rather than treating 46% as a rounding
error toward "most agents drink."

### Reconciling with `fold_depth_sweep.rs`: the crossover between a linear and a quadratic regime

Task 2's own decisive measurement of `drive_at` (§4, immediately above)
reads an elasticity of 0.18 / 1.14 / 0.81 / 1.33 across four runs (median
≈0.98) at the probe agent's history range of 130 → 322 facts. That looks, at
first read, like it disagrees with `fold_depth_sweep.rs`'s single-reset
sweep, whose own elasticity climbs toward 2.0 at high depth (Task 1's
report). **It does not disagree — both are readings of the same underlying
cost, at different points on the same curve, and the probe agent measured
here is, per the finding immediately above, IN the single-reset (`S == H`)
regime the whole time, not the periodic one.**

Write cost as `a·H + b·H²` for an agent whose `S` tracks its whole history
(the single-reset case: `S == H`, so the general `a·H + b·S·H` term
becomes `a·H + b·H²`). Elasticity is `d(log cost)/d(log H) = (a + 2bH)/(a +
bH)`: it tends to **1.0** for `H` well below the crossover `H₀ = a/b` (the
linear term dominates), reads exactly **1.5** at the crossover itself, and
tends to **2.0** well above it (the quadratic term dominates). A periodic
agent (`S` bounded near `RESET_EVERY`) never leaves the linear regime at all,
because its own cost is `a·H + b·(RESET_EVERY)·H` — linear in `H` for any
`H`, with no crossover.

**Bounding the crossover from `fold_depth_sweep.rs`'s own single-reset table
(Task 1's report), without fitting a precise value the data does not pin —
fix round 2 correction: Task 1's report holds TWO such tables, from its own
two fix rounds, described there as equally legitimate noisy re-measurements
of the same sweep, neither superseding the other. The original submission of
this section used only round 1's table without naming it as one of two; both
are reported here.** Local elasticity between adjacent swept depths
(`ln(y₂/y₁)/ln(x₂/x₁)`, a model-free finite difference, not a fit), computed
from each table independently:

| interval | round 1 local elasticity | round 2 local elasticity |
|---|---|---|
| 10 → 32 | 0.52 | 0.55 |
| 32 → 100 | 1.19 | 1.25 |
| 100 → 320 | 1.57 | 1.49 |
| 320 → 1,000 | 1.80 | 1.80 |
| 1,000 → 3,200 | 1.96 | 1.96 |
| 3,200 → 10,000 | 2.09 | 2.09 |

Both sequences climb monotonically from below 1.0 toward 2.0. **Applying the
"first interval whose local elasticity exceeds 1.5" rule literally to each
table separately gives DIFFERENT brackets** — round 1 crosses inside 100→320
(1.19 → 1.57); round 2 crosses one interval later, inside 320→1,000 (1.49 →
1.80, since round 2's 100→320 step reads 1.49, just under the 1.5 line). That
disagreement is exactly why presenting only one of the two tables, without
naming it, understated how sensitive the literal bracket rule is to
which noisy re-measurement happens to be on hand — it is not that either
table is wrong; §4's own module doc already documents this class of run-to-
run scatter at microsecond scale.

**A rule that reads a mechanical "first interval past 1.5" is more brittle
than the underlying question needs, so the robust statement is a
central-difference interpolation across BOTH tables instead of a single
bracket.** Interpolating (in log-depth, between each interval's own
geometric-mean representative point) for where local elasticity crosses
exactly 1.5 lands near **H ≈ 190 under both tables** — inside the 100–320
window either way, regardless of which table supplies the bracket. **The
claim this task can actually support is therefore "the crossover sits on the
order of a couple of hundred facts of single-reset history"** — not a
specific bracket, and not the more precise-sounding "H=322 sits almost
exactly at the upper edge of the 100–320 window" framing the original
submission used, which was an artifact of applying the bracket rule to one
of the two available tables rather than a robust reading of both.

This bound is what reconciles the two instruments, and the campaign-level
conclusion survives however the crossover is stated precisely: **a crossover
around a couple of hundred facts per agent means a production agent enters
the quadratic regime early in an ordinary session, not late.**
`fold_depth_sweep.rs`'s `320` depth already reads 31.7–32.0 µs/call
(single-reset, both tables) against the periodic sweep's 8.1–10.6 µs/call at
the same depth — a real and growing gap — and Task 2's own probe agent, over
a real 200-tick session, reached `H = 322`, inside that same couple-hundred-
fact window. Its own measured elasticity (median ≈0.98, ranging 0.18–1.33
across four runs) reading close to but not cleanly at 1.0 is consistent with
sitting near a bend in the curve rather than safely below it, without this
task asserting a more specific position on that curve than the data
supports. Both `fold_depth_sweep.rs`'s ~2.0 at depth ≥1,000 and
`session_length_scaling.rs`'s figures at depth ≤322 are correct readings of
the SAME `a·H + b·H²` mechanism — they differ because they sample different
windows of the same curve. Neither instrument is wrong, and this is precisely
the shape §4's own model (`ms/tick = C + k·h`, an affine fit with no single
power-law exponent) already warned a naive log-log read would misrepresent.

This also connects to the selection-effect finding directly above: the
ORIGINAL decisive-measurement runs quoted earlier in this section, like this
task's own, used the same max-history probe selection — so it is likely that
they, too, were reading a single-reset-regime agent at `H` in the low
hundreds, i.e. at or near the same couple-hundred-fact crossover region,
rather than a periodic-regime agent safely inside the linear-only zone. The
~1.0 elasticity those runs report is therefore consistent with — not
independent evidence against — the crossover bound above, not a
demonstration that this fold's cost is linear at every depth a production
session could reach.

## 5. Preregistration

Frozen here, before the code that would move it (decision 0016). §6.1's own
rule applies: the conclusion rests on the **shape** of a curve, not a
magnitude.

**The model.** Per-tick cost is affine in the per-agent folded history `h`, not
a power law:

```
ms/tick = C + k * h
```

`C` is the history-independent floor (roster reads, A* searches, occupancy
bookkeeping). `k` is milliseconds of tick time per additional committed fact
per agent. An affine relation with a large intercept has **no** single
power-law exponent, which is why this spec fits `C + k·h` and the instrument
carries no log-log helper — see §4.

**H1 (stage 1, the premise) — MET, see §4.** `k > 0` on the decisive column in
four runs (`k` 1.89 / 2.50 / 3.33 / 2.90 µs/call per fact, `r²` 0.82 / 0.98 /
0.97 / 0.95), with an elasticity of 0.86–1.24 (median 1.045) against a 2.48×
history growth. Stage 2 is therefore
enterable.

**H2 (stage 5, the result).** Stated as a threshold rather than a direction,
because "falls toward zero" is unfalsifiable. On the same instrument, same seed,
same 50 agents, same 200 ticks, ≥3 runs:

- **`drive_at`'s elasticity drops below 0.20**, against the 0.86–1.24 measured
  now. That is the primary criterion: it says the fold stopped being a walk over
  history.
- **`C` becomes identifiable and positive** on the decisive column — currently
  negative on two of three runs precisely because there is almost no fixed part
  to find. A real O(1) fold has a real floor.
- **The whole-tick history share falls below 20%**, against 70–80% now.

Any one of those failing while the others pass is a finding to report, not a
result to average away.

The saving is therefore a **change of order** — per-tick cost linear in history
means total session cost quadratic in session length — not a constant factor.

**H3 (the negative control, and it must be stated).** Committed bytes do not
move. No fact is removed. A campaign that reduced the fact count would have
changed behaviour and failed §0.

**Falsifier for the fix, not just the premise.** If `k` falls but `C` rises by
more than the `k` saving at realistic session lengths, the fix is a
pessimisation for short sessions. That is a finding to state, not to bury by
choosing a long session for the headline number.

**Not predicted, deliberately.** Any share of §6.4's 8.76 ms/agent-tick that
the history term explains. §6.4 is a *level* claim about a different bench at a
different agent count; connecting the two would be the level-vs-shape
conflation §6.4 itself caught §6.2 making.

## 6. The stage carve

Strangler-fig, each stage measurement-gated on the one before, per the
metaplan's own discipline.

| # | stage | delivers | gate to enter | blocked by |
|---|---|---|---|---|
| 1 | The instruments and the attribution | `session_length_scaling.rs` — **done, §4 reports it**. Remaining: a `samply` profile attributing `k` across the six folds in cost order, and an **interleaved synthetic depth sweep** to identify `C` over a ~1000× range (§4's limitations note) | — | nothing |
| 2 | The primitive | the incremental ledger fold, kernel-side; FOLD ≡ SCAN, advance-exactly-once, chaos-rebuild | **met for `drive_at`** (§4) | nothing — see §10 |
| 3 | Delete the hub | remove `agent_sightings` and give thirst, hunger and fatigue their own bounded accumulators; `last_fact_day_at_or_before` becomes O(1) | stage 2's properties green | the Escapement |
| 4 | Belief and hazard | `believed_water` (× peers), `hazard_memory_memo`, `build_emitter_scan` | stage 1's profile says these carry a material share of `k` | the Escapement |
| 5 | The readout | re-run the instrument; H2/H3; state what 7b and 7c may now assume | stages 3–4 | stages 3–4 |

Stage 4 is the one that may not be entered, and that is deliberate: if the
profile says the three drives carry `k` and belief/hazard do not, migrating
them is unmotivated memory for no measured gain — the same judgement §6.5 made
against stage 2 of the parent program.

## 7. Determinism contracts

- **No serialized surface.** Nothing here enters the save. Serializing a fold
  state would be a new decision, not an implementation detail (§8).
- **Discarding the fold is a no-op, provably.** Any observable difference
  between a rebuilt and a resident fold is a defect of the highest severity,
  not a tolerance. This is what the chaos-rebuild harness buys.
- **The fold is a pure function of the ledger prefix.** Memory availability
  may never reach it; there is no eviction policy, because there is no budget.
- **No new stream label, no new predicate, no epoch.** The campaign adds no
  seed-derivation label and changes no save-format contract. That is what makes
  it 7a rather than 7b.

## 8. In / out

**In:** the read side — the incremental-fold primitive, the six folds in
`liveness.rs`, the instrument, and the attribution profile.
**Out:** removing any fact (7c); the typed intention (7b, and an epoch);
`MEM-1`'s melt; the O(agents²) roster term the sibling bench suspects, which is
a different mechanism in the same call path and should not be silently absorbed
into this campaign's headline.

## 9. Decisions this campaign will need

Numbered from the reserved block at ratification.

1. **Stage 7 is three stages, and their order is forced** — 7a the read side,
   7b the typed intention, 7c fact lifetime. Ratified at G3, 2026-08-24. The
   ordering is not a preference: 7c is unsafe while anything folds raw history,
   and 7b precedes 7c because the per-step `provenance` is rendered prose, so
   dropping steps first is a fidelity cut. Amends the metaplan's §6 stage table.
2. **A fold advances; it is not invalidated** — the distinction between the
   incremental-fold primitive and `Derived`'s memo, and the rule that a
   dependency touched every tick makes a memo useless by construction.
3. **The reset event is the checkpoint** — a past-position read is served from
   the last reset at or before it, which is what bounds the three drives.
4. Possibly: **the trail's provenance is content**, recording why 7c is gated
   behind 7b rather than being reachable directly. This may be better as a
   `see-also` on `MEM-1` than a decision of its own.

## 10. Operational notes for the implementer

- **`campaign/the-escapement` holds a live hold-off on `windows/vessel/` and
  `kernel/src/ledger.rs`** — an unmerged `WorldTime` epoch (f64 fractional days
  → i64 ticks, its decision 0186), 176 of 333 sites in `windows/vessel`,
  committed the same day this spec was written and with uncommitted edits to
  `liveness.rs` in its worktree. Its diff touches `agent_sightings`,
  `integrate_thirst` and `latest_committed_position` directly. **Stages 3–5
  sequence behind it; stages 1 and 2 do not.** Stage 1 adds only a new file.
  Stage 2 is a new kernel module plus two one-line registrations, and this
  design gives `Ledger::commit` no hook — so it never touches
  `kernel/src/ledger.rs`, the one kernel path the hold-off names. Checked
  against the actual diff: The Escapement's `kernel/src/lib.rs` change is
  confined to the `pub use units::{…}` block, so the worst case is a
  three-line-context conflict on two registration lines.
  Expect every `f64` day in §2's arithmetic to become an integer tick count,
  which makes the accumulator's arithmetic *exact* and is a simplification, not
  a cost.
- Read costs from `docs/timings.md` per host (`wall_s` is field 4), never from
  `CLAUDE.md`'s prose. `gate-commit`'s documented 10–16 s does not hold on this
  Mac: the warm floor is ~110–155 s and a cold run after an absorb is 450–700 s.
- Predict a sluice hold with `git merge-tree` against the peer ahead in `make
  sluice-status`, not against `main`.
- **`make worktree-take` printed `scripts/test-worktree-freshness.sh: line
  181: syntax error near unexpected token '('` — and it is NOT a bug on
  `main`.** Traced rather than reported: `make worktree-take` resolves the
  worktree pool from the **main checkout**, so it runs *that* checkout's copy
  of the script, and this machine's main checkout sits on
  `chore/heavy-tier-reclassify` @ `ea08a2ed5`, where an embedded `awk` program's
  `END { ... }` block is unquoted and bash parses it. `bash -n` on `ea08a2ed5`
  exits 2; on `origin/main` it exits 0 (fixed by `d8fbabd71`). So the failure is
  a **stale-checkout artifact**, and the general lesson is worth more than the
  instance: a `make` target that reaches into the main checkout runs the main
  checkout's *code*, at whatever revision it happens to be parked on — so an
  error it prints says nothing about the branch you are working in.
