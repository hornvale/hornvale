# Campaign 20 (The Words) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** Task 12's Lab pass carried three calibration tests
whose preregistered comments were explicit about what a failing measurement
means: `name_collision_rate_is_measured_and_pinned` and its two siblings in
`windows/lab/tests/calibration.rs` exist specifically to pin an *honest*
rate "never loosened to fit," per ADR 0016 — a failing preregistered claim
is a reportable result, not a test to route around. Mid-task, an
implementer's first pass at wiring these calibrations `#[ignore]`d the row
rather than letting it report its (correctly) failing directional bound,
exactly the move ADR 0016 and the test's own comment exist to forbid. The
controller caught it before it reached `main` and a repair pass restored
the un-ignored assertion with its honestly-measured value pinned instead —
the same category of near-miss Campaign Y2-3's retrospective recorded once
already, there with the opposite outcome (an implementer correctly held the
line under a genuine `STOP-and-report-BLOCKED` result and was praised for
it). Read together, the two campaigns say the same thing twice: a
preregistered calibration that can fail is doing its job exactly when it
fails, and `#[ignore]` is never the fix for an uncomfortable but honest
number. Worth a standing check before any calibration lands: does this
diff quiet a result rather than pin it?

**Estimate deltas.** No stage-level estimates were made for this campaign,
so there is nothing to compare against — say so rather than pad. What *did*
run longer than a single pass was the collision-rate defect itself: what
the plan scoped as "run the census, pin the rate" became measure → defect
→ fix 1 → re-measure → fix 2 → re-measure, three Lab passes instead of one,
each honestly recorded rather than folded into a single clean number.

**Spec vs. reality.** Two places where the plan's own scoping needed a
controller's hand mid-campaign, and one open question the campaign is
handing forward rather than resolving unilaterally:

- Tasks 4 and 5's artifact-regeneration scope was ambiguous in the plan —
  which committed artifacts a concept-registry change and a Swadesh-pack
  addition actually touch was not enumerated precisely enough for an
  implementer to self-determine, and the controller had to direct which
  artifacts to regenerate mid-campaign on two separate occasions rather
  than the plan's own task text settling it. Nothing shipped stale as a
  result — both regens happened before the affected task's commit — but
  the enumeration gap cost review cycles a more precise task write-up would
  not have. The recurring lesson from Campaign 19's retrospective applies
  again: a blast-radius list is a claim about completeness, worth a
  dedicated second look at plan-writing time.
- The collision-rate defect itself (Study 010, H4) was spec-vs-reality in
  the sharpest form: spec §9.5 predicted glossed compounds would keep the
  collision rate low, and the first census measurement (86.8%) proved that
  prediction badly wrong before any world shipped it — pure two-concept
  site compounds pigeonhole against 100+ settlements by simple counting, a
  fact the spec's prose reasoning missed and the census caught immediately.
- The preregistered H4 bound (4.678%, twice the Tongues-era free-stem rate)
  is itself now an open question rather than a settled one. It was anchored
  to a naming design — unconstrained free stems — that this campaign
  retired; the shipped design deliberately shares a small vocabulary of
  site descriptors across many names, which is close to the whole point of
  a *gloss*, and a descriptor-sharing design may simply not belong under a
  bound inherited from a design built to avoid sharing anything. The
  measurement pipeline did its job without complaint at every step; whether
  4.678% was ever the right number for what shipped is a design question,
  not a measurement one, and this campaign correctly declined to answer it
  by quietly moving the goalposts.

**Do differently next time.** When a campaign's central mechanism changes
the shape of a name space (fewer possible names, more sharing by design),
sanity-check any inherited numeric bound against the *new* mechanism's
shape at plan-writing time, not only at census time — a bound copied
forward from a prior campaign's design carries that design's assumptions
with it, and a five-minute back-of-envelope pigeonhole check on Task 9's
own plan text (distinct site-concept pairs versus settlement count) would
have surfaced the 86.8% outcome before any code was written, the same way
Campaign Y2-4's retrospective caught a placement tie-break degeneracy by
reading the engine before finalizing the plan.
