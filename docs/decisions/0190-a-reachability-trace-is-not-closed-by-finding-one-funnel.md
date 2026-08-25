# 0190. A reachability trace is not closed by finding one funnel

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:** nothing · **Amends:**
[0187](0187-a-pre-genesis-sky-query-is-clamped-to-genesis.md)'s "What was
traced" rationale — 0187's *decision* (a pre-genesis sky query clamps to
genesis) is unaffected and stands

In the context of decision 0187 justifying `GeneratedSky::t`'s clamp partly
on the claim that "no caller can hand `local_day` a negative value today,"
and the spec this campaign shipped (`docs/superpowers/specs/2026-08-23-
the-escapement-design.md` §1) independently making the same claim to justify
treating `Calendar::local_day`'s negative-time defect as latent rather than
live, and a census refresh three weeks later moving three cells that trace
directly to that defect on real (non-hardcoded, non-clamped) worlds, we
record that **both claims were wrong, they were wrong for the same reason,
and the reason is a trace that stopped at the first funnel it found instead
of checking every path that reaches the function under review.**

**What was actually traced, and where it stopped.** The original reachability
work (this campaign's Task 4/0187, and independently this spec's §1) checked
two things: whether `GeneratedSky::t` — the domain's `WorldTime -> StdDays`
funnel — could emit a negative value (no; it clamps with `.max(0.0)`), and
whether the funnel-bypassing call sites named in `windows/worldgen/src/lib.rs`
could construct one directly (no; both pass a hardcoded `StdDays::new(0.0)`).
Both checks were run correctly and both answers were correct. The trace then
concluded *unreachability* from those two checks, which is a different,
stronger claim than either check supports: it requires that **every** path
into `Calendar::local_day` — not just the two paths checked — be
non-negative.

**The path that was not checked.** `domains/astronomy/src/heliacal.rs` calls
`Calendar::local_day` directly, and does not go through `GeneratedSky::t` at
all. `heliacal_events` receives an already-clamped, non-negative `t`, then
computes `year_start = t.0 - calendar.year_phase(t) * year` (`heliacal.rs:112`)
and scans 400 points forward from `year_start` (`:137`), each of which reaches
`local_day` through `at_local_fraction` (`:81`). `year_start` is negative
whenever `t < year_phase(t) * year` — true at genesis for essentially every
world with a nonzero drawn `year_phase_offset`. The clamp at the funnel does
nothing here, because the negative value is constructed **downstream** of the
clamp, inside astronomy itself, not handed in from outside it. 0187's own
"What was traced" section came within one sentence of finding this — its
closing paragraph on the Task-5 tension observes that "nothing at the Rust
level stops `GeneratedSky::t` itself from constructing a negative `StdDays`
internally, the way `eclipses.rs`'s node/phase arithmetic already does" — and
did not extend that observation to `heliacal.rs`, which does exactly that.

**Measured, not argued.** Instrumenting `local_day` to compare the old and
fixed fraction formulas while building one seed-267 world found 1,293,003
fraction divergences out of 2,776,344 probe calls, every one at `local < 0`,
zero at `local >= 0` — the negative path was the common case for the early
part of every year scan, not a corner case behind an unreachable guard. The
corrected fraction changes which heliacal risings and settings the scan
finds for 2 of 354 seed-267 settlements (1 of 274 in seed 831), which changes
`presiding` for those cells, which changes the settlement's drawn name — the
three cells the campaign's census refresh moved. Full instrumentation,
backtrace, and the arithmetic closure over the census's `fnv1a64` hash are in
`docs/audits/the-escapement-census-attribution.md`.

**What this does and does not change.**

- 0187's decision — clamp `GeneratedSky::t` to genesis rather than returning
  `Option` or an error — is unaffected. The structural argument for it (a
  negative `StdDays` cannot survive `StdDays::new` at astronomy's public
  boundary, so the choice is between a total function and defending a case
  the type system already forecloses) does not depend on whether some other
  function elsewhere in the domain can be reached with a negative value; it
  depends only on what `GeneratedSky::t`'s own callers can construct at the
  public boundary. That part of 0187 was correct and stands unedited, per
  decisions being append-only.
- 0187's "What was traced" section's *summary claim* — "no caller can reach
  the negative path today" — is false, and this record is the correction.
  The narrower, true statement is: **no caller can hand `GeneratedSky::t`'s
  own output a negative value, but `heliacal.rs` constructs one internally,
  beneath that funnel, and reaches `local_day` with it.** The clamp governs
  one call path, not the domain.
- The spec's §1, which drew the same "latent, not live" conclusion from the
  same incomplete trace, is corrected directly in the document
  (`docs/superpowers/specs/2026-08-23-the-escapement-design.md` §1 and §6's
  stage 2 exit criterion) rather than through this record, since a spec is
  not append-only the way a decision is.

**The general lesson, worth stating once so it outlives this campaign.** A
reachability trace closes when every call path into a function has been
enumerated and checked, not when the first funnel found happens to be safe.
"The only conversion is `X`, and `X` clamps" is a true and useful fact about
`X`; it is evidence toward unreachability, not proof of it, until every other
path into the target function has been ruled out by the same standard. The
stronger the resulting claim (here: "not live," used to justify skipping test
coverage and shipping the fix as low-priority cleanup rather than a bugfix),
the more that gap matters — this campaign shipped the fix regardless, for an
unrelated reason (stage 2 was porting the function anyway), so the practical
outcome was the same either way; what changed is the *record*, which claimed
a stronger fact than the evidence supported and was believed for weeks
because nothing forced the question until an unrelated measurement (a census
refresh) surfaced it downstream.

**See also.** Decision 0187 (the decision this record amends the rationale
of, not the substance); decision 0186 (the tick-lattice migration whose Task
5 fixed the underlying `local_day` defect); `docs/superpowers/specs/
2026-08-23-the-escapement-design.md` §1 (the corrected spec text) and §6
(the corrected stage 2 exit criterion);
`docs/audits/the-escapement-census-attribution.md` (the forensic
investigation this record summarizes, promoted out of the campaign's
git-ignored scratch so this citation outlives the worktree); `docs/retrospectives/the-escapement.md` (the process
lesson — a verified claim and a verified consequence of that claim are two
different things).
