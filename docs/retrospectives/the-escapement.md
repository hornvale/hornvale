# The Escapement — retrospective

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-escapement.md).

## The through-line

A verified claim and a verified *consequence* of that claim are two different
things, and across this campaign the second kept getting inherited from the
first without being checked on its own terms. It recurred in six distinct
costumes, spread across an implementer, a reviewer, and the controller
session writing this file — the error was not any one person's, and naming
whose turn it was each time is less useful than naming the shape. **The
first costume below is the sharpest instance of the whole pattern, because it
did not stop at one bad inference — it survived a second pass that believed
it was correcting the first, got inherited into a spec, a decision record,
and this file's own first draft, and was only caught three weeks later by an
unrelated measurement that had no idea it was checking anything.**

### Six costumes

1. **A verified defect read as a verified blast radius, corrected once, and
   the correction was the same error one level up.** `local_day`'s
   `local as u64` cast was shown, with a command, to saturate every negative
   input to zero. That is real. What followed it was an unchecked jump to
   "live bug." A reachability trace then checked whether `GeneratedSky::t` —
   the domain's one `WorldTime -> StdDays` funnel — could emit a negative
   value (no, it clamps) and whether the funnel-bypassing call sites named in
   `windows/worldgen/src/lib.rs` could construct one (no, they pass a
   hardcoded zero), and from those two checks concluded the negative path was
   **unreachable altogether** — a claim about every path into the function,
   drawn from evidence about two of them. That conclusion read as a
   correction of the first over-claim, and it was published as one: in this
   spec (§1), in decision 0187's rationale, and in this file's own first
   draft of this very costume. It stood, believed, through the rest of the
   campaign. It was wrong: `domains/astronomy/src/heliacal.rs` calls
   `local_day` directly, beneath the funnel, with values it constructs
   internally from an already-clamped time — and those values are negative on
   almost every world (1,293,003 divergent calls measured in a single
   seed-267 build; decision 0190 records the correction). Nothing forced the
   question until a census refresh, weeks and several stages later, moved
   three settlement names that trace straight back to this exact fix. The
   defect and its reach are two separate claims; this campaign verified the
   first, asserted the second from a check that covered only some of the
   paths that mattered, and then treated that narrower check as if it closed
   the question — twice, the second time while writing a document about
   exactly this failure mode.

2. **A number correct in one context, reused in another where it was
   false.** "Nine orders of magnitude" was written once, correctly, about how
   close a call site sat to a tick-range threshold. It was reused, later, to
   describe the gap between the tick range and `f64::MAX` — a different pair
   of quantities entirely, where the real gap is roughly 294 orders. Nobody
   caught it because the sentence it arrived in was plausible on its own. A
   number is a claim with a context; carrying the number across without
   carrying the context is the same error as costume 1, one level more
   abstract.

3. **"Out of scope" in a review read as "owned by someone else."** A reviewer
   reported survivors of the flip "only in `domains/astronomy` — out of
   scope," and that was accepted without asking out of scope of what, owned
   by whom. There was no elsewhere: no task had ever been assigned
   `provider.rs`'s 22 unported `WorldTime::new` sites, and the migration's own
   shims hid the gap for four green commits. A reviewer reporting a survivor
   is reporting a fact, not a disposition.

4. **A fixture value accepted without checking it had the property it was
   chosen for.** A test was specified as needing "a negative, non-tick-aligned
   `f64` day," with a value supplied but not verified: `-2.75 * 100,000 =
   -275,000` exactly, which *is* tick-aligned. The test passed trivially and
   could not have caught the real bug living one line away (`whole_days()`
   and `tick_of_day()` deriving from different domains — `f64::floor` versus
   integer `rem_euclid` — disagreeing at exactly the boundary the fixture was
   supposed to probe). The fix was to name the *property* a witness must have
   and let the implementer find one, rather than prescribing a value from
   outside the code — which is the rule the campaign was already supposed to
   be following.

5. **A causal mechanism volunteered rather than established, then refuted
   twice.** A moved fact count (189 → 190 in a committed fixture) was
   attributed, confidently, to a specific window-boundary shift the spec had
   already ratified as an acceptable consequence. Arithmetic refuted it — the
   two window bounds rounded to the identical tick across the full
   uncertainty band, so the named shift was zero. A second guess (a
   read-back mismatch) was raised in its place and also refuted, this time
   by an instrumented run dumping every fact a homeostatic drive committed on
   each side of the change: the real cause was a feedback loop in
   read-modify-write drive facts, invisible to either guess. Volunteering a
   plausible cause is not the same act as establishing one, and this was one
   commit away from being quoted forward into the chronicle and a decision
   record before anyone checked it.

6. **A unit-convention error that appeared twice in the same document.** The
   spec's opening precision table used half-step spacing (the distance to the
   nearest rounding boundary); a table added below it later, arguing the
   opposite nuance, used full-step spacing. The two conventions differ by
   exactly 2×, and the error was invisible until someone asked for a
   plain-English restatement of both tables in one breath — the kind of
   pressure a reader supplies and a re-read of one's own prose does not.

The pattern under all six: *checking a claim is not the same act as checking
what depends on it*, and the two are cheap to conflate precisely because a
verified fact makes everything built on top of it feel verified too.

## The two-phase migration was forced by tooling, and the shims that made it possible hid costume 3

`WorldTime`'s flip from `f64` to `i64` could not land as a single sweep across
333 call sites, and the reason was not judgment — it was that
`scripts/hooks/pre-commit` runs `make gate-commit`, whose lint step is an
*unscoped* workspace `clippy -D warnings`. The whole workspace has to compile
for any commit to land, full stop, so a migration that leaves the tree red for
nine tasks' worth of work cannot exist as a sequence of commits at all. The
plan split into a rename phase (add the tick-shaped surface as accessors over
the still-`f64` field, so nothing rounds and nothing breaks) and a flip phase
(retype the field itself, once, behind everything the rename phase built).

Both halves of the consequence are true and worth holding at once. The split
is what made nine tasks' worth of a breaking change committable at all under
a real constraint, discovered — not assumed — at Task 1. And the same shims
that made that possible are exactly what let costume 3 survive for four
commits: a migration shim's entire purpose is to make old and new code both
compile, which means it also makes an *unported* call site compile, silently,
until the shim itself is deleted. The shim-deletion gate at the end of the
flip is what finally surfaced the 22 sites nobody owned. A tooling constraint
that forces a staged migration is not free of the risk that staging exists to
manage — it relocates the risk to the boundary where the staging ends, and
that boundary needs its own explicit check (a grep returning zero, in this
case), not an assumption that the staged parts were complete because they
compiled.

## What was measured against a real instrument, not asserted

Two things in this campaign that could easily have shipped as prose claims
were instead checked against the code: whether the flip actually held
generation invariant (a controller re-parse of a world on both sides of the
flip, 12,534 facts each, zero non-day differences), and whether the vessel
rename's guard test still reached the code path it was written to guard
(traced through to confirm the accumulation branch, not the earlier parse
guard, is what the fixed test exercises). Both paid off precisely because the
instinct to assert instead was available and declined.

## Follow-ups

Promoted from the campaign's working ledger; not addressed in this campaign,
each for a stated reason.

1. **`domains/climate`'s time parameter is an untyped `f64` day**
   (`is_frozen_at(cell, f64)` / `temperature_at(c, f64)`, called from
   `windows/locale/src/{surface,lib}.rs`). This campaign routes it through the
   kernel hatch but does not type it. Retyping a domain's public sampling API
   is its own campaign.
2. **`windows/vessel::clock::Ticks(u64)` now shares a name with the kernel's
   own tick concept.** It is an action *cost* — a duration, legitimately
   unsigned — so it is not wrong, just collidingly named. Rename to `Cost` or
   `ActionTicks` when vessel is next open; deliberately deferred because
   vessel was the gated stage.
3. **`type-audit: pending(wave-2: day)` on the scene structs is unresolved.**
   This campaign adds `*_ticks: i64` beside the `f64` day fields but does not
   settle the wave-2 verdict on the `f64` halves. Whoever runs wave-2 should
   know the `f64` is now a deliberate presentation unit, not an unexamined
   primitive.
4. **Only one of ~20 `Calendar` methods has negative-time coverage.** Stage 2
   added a test for `local_day`; `year_phase`/`season_phase`/`moon_phase` all
   use `.fract()`, which is negative for negative input, and remain untested
   there. Worth a property test sweeping negative time across the whole
   `Calendar` surface.
5. **`StdDays` conflates an instant with a duration, and validates
   non-negative** (`domains/astronomy/src/units.rs`, `quantity!(StdDays, ...,
   non_negative, ...)`). Decision 0126 says a time point may be negative
   because it is a point on an axis, not a duration; `StdDays`'s own
   constructor structurally cannot hold one. This is the same defect class
   0126 exists to fix, surviving one layer down — it is the structural reason
   decision 0187's clamp is *forced*, not freely chosen, and in-crate code
   already bypasses the validation via the `pub(crate)` tuple field
   (`eclipses.rs`, `heliacal.rs`) to construct negative values anyway, so the
   invariant is not even held internally. It is the structural reason decision
   0187 chose a clamp over an `Option` — stated that way deliberately, because
   0187 goes out of its way to disclaim the word *forced*: the clamp is not
   forced at `GeneratedSky::t`'s own call site, which builds its `StdDays`
   through the `pub(crate)` tuple field and bypasses `new` entirely. Splitting
   `StdDays` into an instant type and a duration type is its own campaign;
   worth an idea-registry row.
6. **The wire message to a `the-hand` session expired unapproved and was
   never delivered — and the question it carried has since been answered by
   the merge itself.** `campaign/the-hand` landed on `main` at `fb1f39127`
   while this campaign was in flight, and this branch absorbed it. The answer
   to "does its `Body` refactor move where time lives in `windows/vessel`" is
   **yes**: `latest_committed_position` and `agent_position` now take `&Body`
   rather than `&Npc`, and `room_entry_day` is new. The absorb also settled
   the paired workaround spec §1 flagged — The Hand merged first, so this
   branch deleted its half, `let t = hornvale_kernel::quantize(t.day())` in
   `latest_committed_position`, which had become a no-op over an exact tick
   count. Decision 0191 supersedes 0230, the record that installed it. Closed;
   nothing to carry forward except follow-up 2's vessel work.
