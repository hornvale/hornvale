# The Staple D3 — decision ledger

Campaign: **The Staple D3** — the flow returns downhill.
Predecessor: The Staple D2 (`campaign/the-staple-d2`), merged before this
campaign opened. Branch: `campaign/the-staple-d3`. Decision block:
**0916–0925**, reserved 2026-09-07.

The requested D2 filenames were checked and are not present in the current
tree. The canonical records are `docs/superpowers/ledgers/2026-09-04-the-staple.md`,
`docs/retrospectives/the-staple.md`, and `book/src/chronicle/the-staple.md`;
those are the records read here.

## Task 0 — preregistered falsifier

## #1 [G1] — Where should D3's derived function live?

**Question:** should the return flow classify each standing relation, its
patron after aggregation, or the world from a stores snapshot? **Decision:**
adopt the edge-local return as the design direction. A standing relation is
the unit named by D3, and the derived function is the consequence of that
relation's downhill return; patron aggregation is a later readout, not the
primary witness.

**Why:** the live tree confirms `History::tribute` exposes only subordinate,
patron, and `since`, while the bake's private `Tribute` carries the patron,
assessment, and health state. The current public history therefore cannot be
used to claim that a return magnitude already exists. The probe must observe
the private relation state during the bake or a deliberately designed emitted
return record. This keeps the witness causal and prevents D2's existing
`stores` inflow from masquerading as D3's outflow.

**Alternatives discarded:** patron aggregation can let one high-degree hub
make every edge look successful, collapsing the gradient into `N/N`; a global
stores snapshot passes whenever old tribute accumulates and does not prove any
return. Neither is an adequate Task 0 falsifier.

**Ideonomy passes / overturns:** one randomized pass (seed 90703), using
abstraction-lift, cross-domain re-instantiation, and lattice/graph
organons; no overturn. Drainage, traffic, and accounting re-instantiations
all reinforced edge-level measurement and added the nonempty denominator,
relation-count rather than patron-count, and conservation companion checks.

**Capture actions:** criterion and vacuity checks are recorded above;
`History::tribute`'s limited public shape is recorded in Verified premises;
the public-emission choice is deferred to the D3 design brief.

### Criterion

For the baked world's standing tribute relations at `now`, let `N` be the
count of relations in `History::tribute` (the existing
`tribute_relations_at_now` stock), and let `C` be the count of those same
relations whose downhill return produces one of D3's three derived functions:
`Function::Trade`, `Function::Cult`, or `Function::Fort`. The probe must
assert `N > 0` before reporting a ratio. `Function::Mine` is excluded: it is
already the separate, existing working-daughter path and would let mining
alone make D3 appear active. The D3 activation bar is `C/N` strictly between
the two dead poles:

```text
0/N  = no relation returns enough protection, goods, or legitimacy to
       produce a D3 function (the flow is still a sink)
N/N  = every relation produces a D3 function (uniform relabeling, not a
       gradient)
0 < C < N = the relation graph differentiates at least two outcomes
```

This is a count against a bar already present in code: `Function::Agrarian` is
the authored founding default in `Bake::open`, while `Function::Trade`,
`Function::Cult`, and `Function::Fort` are existing downstream enum values and
existing structure/render branches. The denominator is the already measured
standing-relation stock, not a hand-picked subset of successful exchanges.

The probe also records the per-relation return magnitude/order used to derive
the function, so the mixed result can be classified as a gradient (multiple
occupied bands with no single cut) or a cliff (one threshold separates nearly
all relations). That classification is descriptive; it cannot rescue either
dead pole.

### Vacuity checks

- `N == 0` is a probe failure, not a green result; D2 already established a
  valid 200-seed activation surface, but D3 must reassert its own live
  denominator.
- Counting patrons instead of relations would allow one patron with many
  edges to manufacture a gradient; the unit is explicitly the standing
  relation.
- Counting authored function labels would pass by construction; the probe
  must derive the return and decode the resulting function.
- A single nonzero relation is not sufficient evidence of a gradient; the
  criterion requires both poles to be excluded and reports the full ratio.

### Ideonomy against the criterion

One randomized `ideonomy-plain --more --seed 90703` pass used
cross-domain re-instantiation, abstraction-lift, and organon-construction,
with lattice and graph organons and cardinality, modularity, animacy,
direction, and complexity prompts.

The lift is: **a directed network transfers a stock downhill and may return a
signal or resource uphill/downhill; classify each edge by the returned
consequence**. Re-instantiations supplied the following attacks:

- Drainage: one outlet receiving every tributary is not a differentiated
  basin; this supports the `N/N` dead pole and relation-level counting.
- Traffic: a route network with one open road is not a gradient if all routes
  carry the same service; measure edge outcomes, not merely connectivity.
- Accounting: a ledger with a nonzero balance is not circulation if every
  credit is the same transfer; retain a conservation/return readout beside
  the function count.

The lattice places `N > 0` below “observable relation population”, `0 < C < N`
below “differentiated return”, and the gradient/cliff classification as a
separate incomparable description rather than a second success gate. The
graph exposes the hidden hub: the return payload-to-function derivation is the
load-bearing interface, not `stores` alone.

No overturn occurred. The pass added the nonempty-denominator guard, rejected
patron-counting as a false gradient, and made conservation a companion
readout. A second pass is required if a later design decision changes the
criterion materially.

## Verified premises and identifiers

- `Community` currently carries `population`, non-edible `stores`, and typed
  `subsistence`; `stores` decays by `STORE_DECAY` and currently has no
  meaningful downhill outflow.
- `Tribute` currently carries `patron`, `assessment`, `since`, and
  `last_seen_population`; `collect_tribute` subtracts remittance from the
  subordinate population and adds it to the patron's `stores`.
- `Bake::open` currently authors `Function::Agrarian` and
  `Notability::Common` for every new occupation.
- D2's `windows/worldgen/tests/suite/staple_d2_probe.rs` and
  `ExchangeCensus` provide the existing activation/conservation probe surface;
  D3 must not silently reuse its treatment-only denominator.
- The live branch chronicles checked before absorption include The Housemark,
  The Cruck, The Hidage, The Fetch, The Murrain, and The Newel. Their shared
  lesson is to state negative results as measured counts and to distinguish a
  live witness from a label or a stale artifact.

## Rejected and deferred

## #2 [G2] — Draft design section self-review

**Question:** does the D3 draft preserve the falsifier's direction and avoid
assertions that can pass vacuously? **Decision:** proceed to G3 with the draft
spec at `docs/superpowers/specs/2026-09-07-the-staple-d3-design.md`.

**Why:** the self-review verified that the draft keeps the unit as a standing
relation edge, excludes the already-live `Mine` path, rejects an empty
denominator, names both `0/N` and `N/N`, and makes simultaneous clearing and
continuous-cause derivation proof obligations. It also includes the required
epoch, sanctioned census re-baseline, and history-pin conversion cost.

**Alternatives discarded:** adding implementation detail before Task 0;
making the gradient/cliff description itself the success gate; and treating
the missing browser visual companion as evidence for or against the mechanism.

**Ideonomy passes / overturns:** the prior non-zero G1 pass was re-read
against the complete criterion; no new overturn. Its cross-domain findings
remain attached to Task 0. The browser companion was attempted and returned
“No browser is available”, so the planned visual follow-up is deferred rather
than fabricated.

**Capture actions:** committed the draft spec and retained all unresolved
conversion/precedence choices as explicit deferred scope.

- **Rejected:** use catchment size as D3's falsifier. The Hidage measured the
  proposed D1 at `1.00` clearing the hamlet ceiling on every seed; it is a
  rescale question, not the return-flow question.
- **Rejected:** use `max_stores_at_now > 0` as the success bar. D2 already
  makes stores accumulate, so this would test the old inflow and pass with no
  outflow or derived function.
- **Rejected:** denominator = exchange attempts or successful deliveries.
  Those are D2's local treatment surface, not D3's standing relations.
- **Deferred:** exact protection/goods/legitimacy conversion, function
  precedence when several return components qualify, and whether a relation
  may produce more than one downstream function. These belong in the D3
  design after Task 0 is accepted as the measurement instrument.
- **Deferred:** debt, obligations, interest, labor claims, currencies, price
  discovery, negotiation, storage loss, transport, and priority institutions,
  all explicitly deferred by D2.

## Cost ledger

D3 is a dynamics rung. Any implementation that changes the bake's committed
history pays an epoch, a sanctioned census re-baseline through the sluice, and
conversion of history-adjacent pins into invariants. The cost compounds with
D2's changed populations and typed stocks; it is not a later cleanup task.

## #5 [G4] — Task 1 witness-boundary ruling

**Question:** did the fixed Task 1 scaffold produce Task 0 evidence? **Ruling:**
no. The public integration-test boundary exposes standing relation endpoints
and their count, but no per-relation downhill return, return magnitude, or
conservation residual. The first scaffold briefly counted endpoint
`Function` labels; review rejected that as a vacuous substitute for the
approved causal criterion. The repair now reports `d3_function_relations` and
`d3_ratio` as explicitly unavailable and prints the same for the ignored
readout.

**Evidence:** fixed roster `1..=200`, nonzero relation denominator guard, and
the non-claiming focused test passed. `make gate-commit` passed with 1,429
sub-floor tests and 60 report checks. The ignored 200-seed readout timed out at
120 seconds while compiling and emitted no measurement; it is not evidence.
Repair commit: `1a21a7ecfca3f0619150f7510c8943fc4c702c34`.

**Decision:** Task 1 is complete as a boundary-finding scaffold, but Task 0 is
not yet measured. Do not enter the `C=0`, `C=N`, or mixed branch, and do not
implement production return flow. The current plan's phrase “first executable
slice” is insufficient: a return witness must be approved and made observable
before the falsifier can run. This is a plan/spec boundary, not a measured
sink or uniform relabeling.

**Rejected:** endpoint-label classification (review finding; it can pass from
authored/derived labels without any return); treating the timeout as a census
result; adding an unapproved public return field merely to satisfy the probe.

**Follow-ups:** amend the plan with a bounded witness-design step that chooses
between an internal bake test witness and an approved emitted diagnostic
record, then rerun the preregistered Task 0 criterion. Preserve the same
relation denominator, D3 function set, dead poles, and no-local-census rule.

## #6 [G3] — Emitted diagnostic witness approved

**Ruling:** Nathan approved the emitted diagnostic return witness on
2026-09-07. The plan and design now permit a zero-impact diagnostic sidecar on
the emitted `History` result, separate from save-facing `History::tribute`
records. It may carry relation identity, continuous protection/goods/legitimacy
components, magnitude bands, and an explicit diagnostic classification.

The sidecar must not mutate stores, population, functions, epoch output, or D2
exchange behavior. Its thresholds and precedence must be written into the
ledger before implementation, and its classification must read the continuous
vector rather than endpoint labels. Task 0 remains the first production
mechanism gate: only a measured mixed `0 < C < N` result can authorize the
production return flow.

**Sequencing change:** the plan adds a bounded witness task before adjudication;
the old Task 2/3/4 names shift by one. This approval does not approve a
production outflow, a public save-format expansion, or a census.

## #7 [G5] — Diagnostic witness calibration before implementation

**Question:** what explicit, relation-local continuous readout can make Task
0 observable without implying a production return flow? **Ruling:** each
standing relation emits three dimensionless diagnostic components from its
closing bake state. Let `S` be the patron's stores, `H` its number of standing
relations, `P` the subordinate population, `A` the relation assessment, and
`L` its last-seen subordinate population. The witness records
`share = S / H`, `protection = share / max(P, 1)`,
`goods = share / max(A, 1)`, and `legitimacy = P / max(L, 1)`. The `1` floors
only make an otherwise-defined ratio total for an empty or newborn measure;
they are named constants in code and do not convert or move stock.

Classification is purely diagnostic and reads that vector only:

```text
Fort  if protection >= 0.25
Trade if not Fort and goods >= 1.00
Cult  if neither above and legitimacy >= 1.00
None  otherwise
```

The precedence is `Fort`, then `Trade`, then `Cult`: protection is the most
specific return (per-person cover), goods demonstrates an entire assessment's
cover, and legitimacy is the residual health signal. This gives every
simultaneous qualification one deterministic result while keeping categorical
`Function` values entirely outside the inputs. The diagnostic magnitude band
is the maximum component: `Low < 0.25`, `Medium < 1.00`, `High >= 1.00`.
Every numeric cutoff is named beside this evaluation and in its test fixtures.

**Why:** portfolio-normalized patron stock avoids giving each edge a hub's
entire hoard; normalizing it against the subordinate and assessment makes the
two material causes comparable ratios; the health ratio preserves the
relation's observed state without reading an endpoint label. The only emitted
identity is the same durable subordinate/patron pair already emitted by
`History::tribute`.

**Alternatives discarded:** raw stores (hub-size confound), endpoint
`Function` labels (vacuous output-as-input), and a production outflow (beyond
Task 0). A sum of heterogeneous raw quantities was also rejected; `max` of
the three normalized components makes the band name the strongest continuous
cause without inventing a conversion rate.

**Ideonomy passes / overturns:** one `ideonomy-plain --more --seed 90707`
pass using cross-domain re-instantiation, negation, and dimension
identification; no overturn. Portfolio share survives the financial and
traffic analogies, while the negated global-hub / categorical-label variants
confirmed that both would erase the required relation-level gradient. The
component vector remains decomposable and diagnostic-only, so its lifetime is
the emitted history result rather than an epoch state.

**Capture actions:** implement the named constants, direct vector-to-class
tests, relation coverage/determinism tests, and an unchanged-history
comparison; record the fixed-200 result below before any production decision.

## #8 [G5] — Task 0 fixed-roster result: failed denominator

**Ruling:** the Task 0 readout is blocked, not a sink, uniform relabeling, or
mixed result. The once-run fixed roster test preserved its `N > 0` guard and
failed before it could aggregate a report:

```text
HV_TEST_OK=1 timeout 1200s cargo test -p hornvale-worldgen --test suite \
  staple_d3_probe::fixed_200_seed_d3_relation_return_report \
  -- --ignored --exact --nocapture

FAILED: D3 ratio requires a non-zero relation denominator
finished in 493.47s; explicit 1200s timeout did not fire
```

The test's failure path intentionally emits no aggregate `N`, `C`, `C/N`,
band totals, residual aggregate, `dead_pole_sink`, or `dead_pole_uniform`.
Those values are therefore **unavailable**, not zero: treating any of them as
evidence would convert the required vacuity guard into a false negative. The
focused seed-42 coverage fixture did have standing relations, so this result
also establishes that the fixed roster has a seed-level denominator failure
rather than that the witness cannot emit at all.

**Why:** Task 0's denominator is every fixed-roster seed's standing
`History::tribute` population. One empty member invalidates the prescribed
relation-level population; omitting it or changing the denominator would be a
post-hoc selection.

**Alternatives rejected:** inferring `C=0` from the early assertion, treating
the failure as a timeout, rerunning or narrowing the preregistered fixed-200
readout, and starting production return flow. None supplies the required
valid denominator.

**Ideonomy passes / overturns:** one prior calibration pass (#7) applies; no
new conversion decision is licensed by a failed measurement. No overturn.

**Capture actions:** retain the zero-impact diagnostic sidecar and its focused
tests; stop before Task 3/4 production work. A future denominator-fixture
repair needs its own approved task and a fresh probe run with a seed-naming
failure message before it may classify the roster.

## #9 [G4] — Denominator repair proposal

**Finding:** the fixed-roster probe added a stronger per-seed assertion than
the preregistered criterion requires. The criterion's `N` is the count of
standing relations in the measured baked result; `1..=200` is the fixed
measurement roster, not a requirement that every seed independently contain a
standing relation. Seed 1 has no relation, while the seed-42 fixture and the
sidecar coverage prove the witness can emit.

**Proposed repair:** pool all emitted standing relation witnesses across the
unchanged fixed roster, assert only the aggregate `N > 0`, and report each
seed's relation count in the diagnostic readout. Do not omit seed 1, replace
its result, or change the 200-seed roster. The aggregate remains relation-local
and the denominator remains the existing standing-relation stock; an empty
individual seed contributes zero relations rather than invalidating the pooled
measurement.

**Rejected:** narrowing the roster, selecting only seeds with relations,
turning seed 1 into `C=0`, or rerunning the failed probe without changing the
over-strong per-seed guard. Each would either post-select the denominator or
misstate unavailable evidence.

**Status:** this is a bounded probe-fixture repair only. It does not authorize
production return flow or function conversion. A fresh fixed-roster run is
required after the repair before Task 0 can enter any dead-pole branch.

## #10 [G3] — Pooled denominator repair approved

Nathan approved the pooled relation denominator on 2026-09-07. The fixed
roster remains exactly seeds `1..=200`; an individual seed with no standing
relation contributes zero records, while Task 0 asserts the pooled relation
count `N > 0`. This matches the preregistered definition of `N` as the count
of standing relations in the measured baked result and does not post-select
successful seeds.

The repair is probe-fixture scope only. It authorizes one fresh fixed-roster
diagnostic run, not production return flow, function conversion, epoch work,
or a census.

## Follow-ups

- Write the D3 brief/spec around this Task 0 criterion before proposing code.
- Re-check the exact `History` relation exposure and the function consumers in
  `domains/history/src/flesh.rs` and `windows/almanac/src/history.rs` when the
  brief chooses the return representation.
- If the visual companion becomes available, render the relation-level
  `0/N -> mixed -> N/N` bar and the gradient/cliff distinction before G2.
- Keep Nathan's review points at G3 (spec review) and G6 (merge/close).

## Process capture

The prose gate initially failed because the new spec was absent from
`docs/audits/campaign-reconciliation.tsv`. After adding the spec row, it
correctly failed again when the ledger path was placed in the TSV: ledgers are
not part of that audit population. Removing only that citation fixed the
contract; `make docs-tests` then passed 75/75. The ledger remains the committed
campaign record, just not a reconciliation record.

## #3 [G3] — Spec review

Nathan reviewed `docs/superpowers/specs/2026-09-07-the-staple-d3-design.md` and
approved it with “LGTM” on 2026-09-07. The G3 hard stop is cleared. The exact
return conversion and function precedence remain intentionally deferred until
Task 0 produces evidence; the implementation plan must not fill those gaps by
assumption.

## #4 [G4] — Plan review

The implementation plan at `docs/superpowers/plans/2026-09-07-the-staple-d3.md`
passes self-review. It covers the approved spec, makes Task 0 the first
executable slice, branches explicitly on `N == 0`, `C == 0`, `C == N`, and
`0 < C < N`, carries the epoch/census/history-pin cost, and contains no
production implementation step before the measurement ruling. The plan's
reconciliation row was added and `make docs-tests` passed 75/75. One ideonomy
re-read of the plan against the prior G1 criterion produced no overturn.
