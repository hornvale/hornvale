# Retrospective — The Mire

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-mire.md): weather gained memory
through a state-dependent recurrence, and the preregistered study built on
top of it found a double falsification — no systemic swing, and the swing
that does exist runs backward from the predicted latitude gradient.

## The dominant lesson: every defect this campaign found started in my own text

Ten defects were found across this campaign's eight tasks. Every one of
them originated in the plan or spec text handed to an implementer, not in an
implementation choice made freely on top of correct instructions. None of
this is a claim that the implementers were flawless — it is the opposite and
more useful claim: **the campaign's error budget lived entirely upstream of
the code**, in the sentences that told each task what to build and what to
assert.

The shape repeats closely enough to name: measured, computed *content*
survived essentially unscathed all campaign (formulas, thresholds inferred
from real data, the recurrence itself). What failed, over and over, was an
**asserted relationship between two components written down before either
was checked against the other** — "the tool reads a tag from here," "this
gate is wired into that target," "this test's mutation proves the property
it claims to prove," "this sampled value is the mean it is named for." A
sentence describing a fact about the codebase is a claim, and ten of this
campaign's claims turned out to be false on inspection:

1. **The artifact-regen sequencing was backwards.** The plan deferred all
   `docs/audits/type-audit-report.md` regeneration to the close task, which
   review flagged as guaranteed staleness the moment any earlier task
   touched a `pub` boundary. Escalated and resolved by inserting a new task
   (1b) and a standing rule: every task touching a `pub` boundary
   regenerates the report in its own commit.
2. **The gate-wiring claim was false.** The inserted task's own brief
   assumed wiring a check into `make quick` would reach `make gate`; the
   implementer found `gate` does not depend on `quick`, it merely lists
   `quick`'s components, so the check needed wiring into both directly or it
   would silently never run at commit time.
3. **Per-field type-audit tags are invisible to the extractor.** The plan
   assumed a struct's field-level `type-audit:` tags could be read positionally
   by the tool that checks them; they cannot, and three later tasks had to be
   amended in the plan itself before three more implementers rediscovered the
   same wrong assumption independently.
4. **A vacuous keystone test.** The test written specifically to guard this
   campaign's central mechanism — that the snowpack sink is state-dependent —
   passed even when a reviewer mutated the driver to feed it a constant `0.0`
   instead of the real accumulated state. The substrate became an unbounded
   accumulator under the mutation and still satisfied the assertion, because
   the assertion checked only a lower bound the mutated version also cleared.
5. **A one-day lag in the overflow cap.** The brief's sink computed overflow
   against the driver's pre-gain `present` value, so a day's own rainfall
   was not counted against the same day's cap — a 500 mm/day deluge
   stabilized at 543.7 mm against a stated 50 mm ceiling, one day of lag
   compounding indefinitely.
6. **A convergence budget that could never be satisfied.** The brief's
   `years_run <= 2` assertion is structurally unreachable: spin-up compares
   whole-year trajectories, and year one is always a cold start from zero, so
   any nonzero fixed point needs a minimum of three years to be recognized as
   converged, not two.
7. **A cap design proven pathological, and a test that could not tell.** The
   brief's pre-gain ablation cap converges to a spurious permanent residual
   under sustained cold, sub-freezing precipitation (a real physics bug, not
   a style choice); the test written to guard against exactly this used zero
   precipitation as its scenario, which cannot distinguish a correct design
   from the pathological one.
8. **A sampled instant mislabeled a daily mean.** The brief fed a per-cell
   mean temperature from a function whose diurnal term is phased on the
   fractional day and is therefore always exactly zero at integer-day
   samples — sampling one fixed instant of each day's cycle, biased by each
   cell's latitude and longitude, and calling the result a daily mean. Every
   freeze gate and every degree-day melt calculation downstream would have
   carried a fixed, silent per-cell offset.
9. **A self-comparison test that could not fail.** The brief's byte-identity
   guard for a day-gated configuration compared two independently-built
   `day: None` calls to each other rather than to a reference free of the
   feature entirely; a reviewer found the two configs being compared were
   literally equal structs, so no mutation of the gating logic could ever
   have turned the test red.
10. **An unspecified threshold that made the instrument blind.** The
    preregistration said "at the default `min_conductance`" without stating
    what that default was. The first value chosen from an existing constant
    (0.05) turned out to exceed the *maximum* real edge conductance observed
    in a pilot (0.0417) — every edge was already below threshold before any
    weather scaling ran, so the instrument could not have registered a swing
    of any size, real or spurious. Recalibrated to the pilot's own pooled
    median (0.002) and frozen before the full run.

None of these ten were caught by running the code and seeing wrong numbers —
the code, run as written, mostly produced plausible-looking output at first
glance. Every one surfaced by mutating the thing the text claimed to prove,
or by re-deriving the number the text asserted rather than trusting it.

## What that implies for writing the next plan

A sentence of the shape "component A does X to component B" is a testable
claim about the codebase, not a design decision, and it is exactly the kind
of sentence this campaign's plan got wrong ten times running. The mitigation
already in practice — mutate the guarding test before trusting it, and
re-derive a cited number rather than pasting it — is what caught all ten;
none would have been caught by review-by-reading alone. Nothing about this
campaign suggests slowing down; it suggests treating every relational claim
in a brief the same way a reviewer already treats every constant: verified,
not assumed.

## Operational findings

- **The Mac was contended during the preregistered run.** Load average 24.4
  from a parallel campaign's own test run was measured before the 200-seed
  study executed — directly the condition the two-machine operating model
  warns about. It did not block the run outright, but it is the likely cause
  of the next finding.
- **A first debug-mode attempt at the full run died at the 60-minute tool
  ceiling with zero visible progress.** No per-seed logging existed at that
  point, so an hour of wall-clock time produced no evidence of whether the
  run was progressing, stuck, or already finished — it was killed by the
  timeout with nothing recoverable. The fix was a temporary per-seed
  progress line, and running the actual population in `--release` rather
  than debug (a determinism-safe choice: same seed and pins already
  guarantee byte-identical output across build profiles, and the project's
  own canonical heavy-artifact paths already run this way).
- **`std::time::Instant` is banned workspace-wide, including inside test
  code, and it is enforced by the same lint that bans it everywhere else.**
  A temporary timing breakdown added to diagnose the phase costs above had
  to be removed before anything could ship — `clippy.toml`'s
  `disallowed-types` lint rejected it under `-D warnings` even though it
  never touched production code, confirmed by a real failing build rather
  than assumed from the rule's stated scope. Diagnostic timing code needs a
  different instrument than the one this workspace forbids everywhere.

## Confidence Gradient

**No bet moved.** `book/src/open-questions.md` was checked directly against
this campaign's territory (weather, substrate, conductance, passability,
seasonality); nothing there stakes a claim this campaign's result bears on.
A freshness sweep of the topology-adjacent chapters (`book/src/domains/
settlement.md`'s connection-graph narrative) found nothing this campaign
made stale — the existing prose describes the graph's existence and its
time-varying structure, neither of which changed; only what gates an edge's
conductance did.

## Follow-ups

Endorsed at close and written to the idea registry rather than left in the
gitignored ledger:

- **Cost, not passability.** H1 measured whether a route is open at all; the
  far likelier home for weather's real effect is how much slower or harder
  an open route becomes. A large cost swing could sit entirely under this
  campaign's measured 0.95% passability swing and be invisible to it — the
  highest-value next measurement.
- **The polar zero is a land-only result.** Water edges were deliberately
  left ungated this campaign; sea ice is plausibly exactly where
  high-latitude seasonality would show up instead, on coastlines whose land
  never varies because it is permanently frozen.
- **Seasonal variation lives where conditions alternate, not where they are
  extreme.** The generalizable lesson behind H2's reversal, stated as its
  own idea so a future campaign asking "where should seasonality show up"
  does not have to re-derive it.
- **Weather's drama is at the scale of a trip, not a kingdom** — and lives
  in the seasonally-wet tropics rather than the frozen north, the reverse of
  genre instinct. Feeds the game layer's registers for how weather should be
  experienced, survived, avoided, exploited, and induced.
