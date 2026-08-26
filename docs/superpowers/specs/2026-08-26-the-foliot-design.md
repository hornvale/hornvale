# The Foliot — finishing the redenomination The Escapement began

**Campaign:** The Foliot · **Date:** 2026-08-26 · **Status:** approved 2026-08-26;
stage 1 revised in execution (see its note)

The Escapement retyped the kernel's instant to an exact signed tick count
(decisions 0186, 0188). Five surfaces below the kernel never followed. This
campaign moves them, and closes The Escapement's follow-ups 1–5.

The name carries the lineage deliberately: a foliot is the oscillating bar
that *regulates* a verge escapement. The Escapement set the tick; this
campaign regulates what hangs off it.

## 1. The diagnosis

The five follow-ups read as five unrelated chores. They are one defect seen
five times: **the kernel changed its unit of account and its dependents still
quote the old currency, converting at the boundary.** Each conversion point
fails differently.

(Row 2 is stated as execution corrected it. The original spec said vessel
"rounds every transaction"; measurement showed that rounding is symmetric and
harmless, and that the real fault is a second lattice replayed in `f64` — see
stage 1.)

| # | surface | how the conversion fails |
| --- | --- | --- |
| 2 | `windows/vessel` | keeps a **second lattice**, and replays it in `f64` |
| 5 | `StdDays` | **refuses legal values** (non-negative) and conflates two meanings |
| 1 | `domains/climate` | **declares no unit at all** (bare `f64`) |
| 3 | `windows/scene` | **posts both prices**, verdict unsettled |
| 4 | `Calendar` | **cannot be audited** — the old currency can't express the test value |

The kernel already models the distinction correctly: `WorldTime` (instant,
signed) and `TickSpan` (duration, signed). Astronomy does not. `StdDays`'s own
doc comment is the confession, verbatim:

> `"Absolute time or duration in standard days."`

Both meanings, one type, one `non_negative` rule that is correct for a
duration and wrong for an instant.

The sharpest single illustration, two adjacent parameters of one function:

```rust
pub fn node_longitude_at(moon: &Moon, year: StdDays, t: StdDays) -> f64
//                                    ^^^^ duration  ^^^^ instant
```

## 2. The type design

**`StdDays` narrows to a duration.** Non-negative stays, because it is correct
for a duration. It keeps its name and the large majority of its 256 astronomy
sites: every `period`, `year`, `day_length()`, `year_length()`,
`synodic_month()`, `draconic_month`, `eclipse_year`.

**A new `StdInstant`** — signed, continuous standard days since genesis —
takes the instant positions. Measured inventory of the public boundary
(counted by script, not by eye — an eyeball pass got every one of these
figures wrong):

- **27 public functions** take an instant parameter (`t`, `t0`, `t1`,
  `t_now`, `from`, `until`): 17 on `Calendar`, 6 in `eclipses.rs`, 2 in
  `star.rs`, 1 in `heliacal.rs`, 1 in `night_sky.rs`.
- **1 public function returns an instant**: `Calendar::alignment_epoch_of`,
  which both takes one (`t_now`) and returns one (an epoch) — the mixed case
  that most needs the two types to be distinguishable.
- **1 public field** is an instant: `EclipseEvent::day`.
- **8 public functions are duration-only** and are left alone, as are the
  duration struct fields (`year`, `period`, `synodic_period`).

A caution for the implementer, learned while producing these numbers: a
grep for `t: StdDays` misses multi-line signatures (`eclipse_events`,
`heliacal_events`) and unconventional parameter names (`t_now`). Enumerate
by parsing the signature blocks, not by line-grep.

**`WorldTime` remains the world/ledger instant**, and `GeneratedSky::t`
remains the single crossing. That funnel's signature is *already*
`WorldTime -> StdDays`; only its output type changes.

### Why not `WorldTime` at astronomy's public boundary

Considered and rejected. One instant type project-wide is attractive, but
`WorldTime` is tick-quantized, so every caller currently passing a
non-tick-aligned `f64` would have its input silently rounded — including
`windows/scene/examples/ephemeris_golden.rs`, which samples a
`while d < 365.0` loop. That breaks byte-identity by construction, for no
physical gain: astronomy's mathematics is genuinely continuous.

`StdInstant` introduces no rounding anywhere. The kernel keeps the exact
instant; astronomy keeps the continuous one; the named hatch (0188) is the
only crossing. This is the design's load-bearing choice.

## 3. Stages

Each stage ends at a sluice stage gate. Stage 1 is independent and lands value
even if later stages slip.

### Stage 1 — one tick, defined once (follow-up 2)

**Revised in execution, 2026-08-26.** This stage was specced as "delete the
`f64` bridge in `Session::charge`, which is pure loss." Its own Task 1.1
probe — written to prove the premise before the fix could act on it —
**refuted that**, and the correction is why the stage grew.

**What the spec got wrong.** It claimed a vessel tick and a kernel tick are
the same unit. They are not. A local day is an exact integer of *vessel*
ticks — that is what `ticks_per_local_day` exists to guarantee, so dawn does
not beat against the day cycle — but it is `d * B` *kernel* ticks, which is
not an integer. So one vessel tick is `d*B / round(d*B)` kernel ticks, and
`days_of`'s conversion is **correct**. Implementing the planned identity
would have introduced an error while claiming to remove one.

**What the round trip actually costs**, measured over 5,764 samples: 213
losses, 211 gains, net **-2 ticks**; `round(d*B)` sits above `d*B` 651 times
and below it 650. Symmetric rounding noise, not a leak, no accumulation. The
registry row `TOOL-vessel-clock-duplicates-the-kernel-tick-lattice` ("one
`Session::charge` rounding from observable") overstates it; The Escapement
retrospective's "not wrong, just collidingly named" was closer to the truth.

**The defect that IS real, found while checking the one that was not.**
`Session::charge` converts once and adds integers — correct. The liveness
catch-up replay does not: `liveness.rs:4644` and `:5233` run
`day += days_of(...)` in a loop over a bare `f64`, never touching the
lattice, and compare against a horizon. The site's own comment says a replay
"that advanced time at a different rate than the walk it is reconstructing
would drift from it by construction" — which is exactly what it does, since
the live walk is integer-exact and the replay is not. Genuine accumulating
drift.

**The fix, at Nathan's direction: unify at the draw.** Two depths were put to
him with their costs. The shallow one (vessel-only, defining vessel's day as
`round(d*B)` kernel ticks) leaves vessel's day boundary drifting from
astronomy's by 0.5 tick per day. He chose the deep one, explicitly accepting
that every world regenerates:

`Rotation::Spinning` stores `day: TickSpan`. A world's local day becomes an
exact integer of kernel ticks, so there is genuinely **one lattice** and the
conversion question disappears rather than being managed.

- **The draw is unchanged.** `anchor.rs` still takes `stream.next_f64()`
  twice in the same order; only the derived value is snapped to the lattice.
  So **no seed label takes an epoch suffix** and the pin-isolation tests hold
  unmodified. This is the most important scoping fact in the stage.
- **`Calendar::day_length()` keeps returning `Option<StdDays>`**, now an exact
  derived conversion, so its 31 call sites do not move.
- vessel unifies for free: `ticks_per_local_day` reads the world's real tick
  count, `Ticks` and `days_of` leave the charge path, `cost_ticks` returns
  `TickSpan`, and liveness's `f64` accumulators become integer addition —
  closing the genuine drift above.

**What moves.** The derived day length shifts by up to **0.432 s**, and that
propagates through every calendar computation into the sky, eclipses and
climate's diurnal term. So: the five vessel byte goldens, the almanacs, and
**census columns** (`day_length()` reaches `windows/lab/src/metrics.rs`).

> **Epoch.** A save-format-adjacent change needing its own decision record.
> Every world regenerates. Authorized by Nathan at the two-depth fork.

> **Census carve-out, authorized.** One refresh on lefford at the pre-merge
> close, standard discipline. Cost read from `docs/timings.md` rather than
> CLAUDE.md's prose, per the project's own rule: the last six runs were
> 895-918 s.

### Stage 2 — the split (follow-up 5)

Introduce `StdInstant`; retype the 22 functions and 1 field; leave duration
positions alone.

Then reopen decision 0187. Its clamp was chosen *because* the type foreclosed
the honest answer — the record says so directly: "the choice is between
clamping and defending a case the type system already forecloses." With a
signed instant that argument no longer holds, so the clamp becomes a free
choice requiring its own decision. Decision 0190 has already **retracted**
0187's claim that no caller reaches the negative path.

Note also that `GeneratedSky::t`'s code comment still carries 0187's
*retracted* rationale ("the sky before the world exists is not a physical
question"). It must not survive this stage in that form.

**A decision rule, not a prediction.** Whether removing the clamp changes
output depends on whether a pre-genesis `WorldTime` can reach the funnel
today. 0190 exists precisely because a confident trace of this question was
wrong once. So the stage traces it and branches:

- **No caller can reach the negative path** → removing the clamp is
  behaviour-preserving. Supersede 0187 with a record that says so and cites
  the trace.
- **A caller can reach it** → output moves for that caller. Stop, characterize
  the change, and bring the diff to Nathan before accepting bytes.
- **The trace is inconclusive** → keep the clamp, and supersede 0187 only to
  correct its retracted rationale. An honest clamp with a true reason beats a
  removal justified by a trace we could not close.

### Stage 3 — the negative-time sweep (follow-up 4)

A property sweep over the **17 `Calendar` methods that take an instant**, at
negative instants. `year_phase`, `season_phase` and `moon_phase` all use
`.fract()`, which is negative for negative input, and none is tested there.

The status quo is not "known broken" but **unknown**, and the reason is
structural: today the only negative-time test lives *inside* `calendar.rs` and
its own comment records that it had to bypass `StdDays::new`. After stage 2
the sweep is expressible from the integration suite through the public API,
which is the point of doing it after and not before.

### Stage 4 — climate and scene (follow-ups 1, 3)

**climate.** `is_frozen_at(vertex, f64)` and `temperature_at(vertex, f64)`
become `WorldTime` — the kernel's type, not `StdDays`, because a domain may
never depend on a sibling and `StdDays` lives in astronomy. ~19 external call
sites; most pass a literal `0.0`, which becomes `WorldTime::GENESIS`.

Per-site rule rather than a prediction: a site whose `day` originates from a
`WorldTime` is byte-neutral; a site whose `day` is an independent `f64` must
have its provenance recorded in the task, and any rounding introduced is
reported, not absorbed. `windows/worldgen/src/graph_derive.rs:226` is known to
pass a loop variable and needs its origin traced.

Fold in the same defect in a third place: `eclipses_scene(world, from: f64,
until: f64)`.

**scene.** Delete the `f64` `day` / `from_day` / `until_day` fields; keep the
exact `*_ticks`. The fields exist for one stated reason —

> "Kept for the external Orrery's existing consumers"

— and Nathan has ruled both external clients (the Orrery and goldengrove,
`hornvale/goldengrove`) out of scope. Verified: no in-repo client reads these
fields.

Bump `scene/eclipses/v1` → **v2** rather than editing v1 in place. The schema
string is emitted in the document, so a consumer that ever reappears fails
loudly on an unknown schema instead of silently reading a missing field — the
reasoning the file itself already advances for a sibling case ("Fails loudly
at the boundary beats three fewer tags").

Regenerates two committed, drift-checked artifacts via ordinary
`make rebaseline`: `book/src/gallery/scene-eclipses-seed-42.json` and
`book/src/reference/scene-eclipses-v1.md` (renamed for v2). No census
involvement — the census is lab metrics and is untouched.

## 4. Out of scope

- **The wider `pending(wave-2)` backlog.** 70 tags exist workspace-wide. This
  campaign settles only the time-typed ones it moves.
- **Retyping `LocalDays`.** Out of scope and deliberately so: local days are a
  presentation conversion, and a world may have none at all
  (`Calendar::day_length()` returns `Option`, `Rotation::Locked` has no day).
  The world-independent unit must remain the backbone.
- **The remaining nine open Minors** from The Escapement's review list.

## 5. Success criteria

1. No public astronomy signature takes a type that means both an instant and a
   duration; the `non_negative` rule binds only durations.
2. `Session::charge` performs no `Ticks -> f64 -> WorldTime` round trip, and
   any surviving conversion names its rounding rule at the call.
3. `Calendar`'s negative-time behaviour is asserted from the integration suite
   through the public API, with no constructor bypass.
4. No `domains/climate` or `windows/scene` public entry point takes a bare
   `f64` day.
5. Every byte-golden movement is deliberate, reviewed, and attributed to the
   stage that caused it. A stage that expected none and produced one stops.
6. Decision 0187 is either superseded or re-justified, and no code comment
   cites its retracted rationale.

## 6. Flagged for review

1. **An EPOCH, and it leads this list.** Stage 1 makes a world's local day an
   exact integer of kernel ticks, moving the derived day length by up to
   0.432 s. Every world regenerates. The *draw* is untouched — same
   `stream.next_f64()` calls in the same order — so no seed label takes an
   epoch suffix and pin-isolation holds. Needs its own decision record.
   Authorized by Nathan at the two-depth fork, with the cost stated.
2. **Census regeneration, authorized** (an autopilot carve-out). The quantized
   day length reaches `windows/lab/src/metrics.rs`. One refresh on lefford at
   the pre-merge close; `docs/timings.md` puts the last six runs at 895-918 s.
3. **Two byte-golden acceptances** — stage 1 (vessel, 5 fixtures) and stage 4
   (scene artifacts). Both legitimate; both need Nathan's eyes on the diff.
4. **A ratified decision is superseded** — 0187, whose rationale 0190 has
   already partly withdrawn.
5. **A published schema breaks** — `scene/eclipses/v1` → v2, permitted only
   because both external consumers are out of scope by Nathan's ruling. This
   is a deliberate departure from the additive-or-versioned-only rule in
   CLAUDE.md, and the rule itself should be re-scoped to say "in-repo
   contracts" once the external clients are formally declared dead.
6. **Capture** — four of the five follow-ups currently exist only in a
   retrospective. Registry rows are written for all of them regardless of
   which stages land.
