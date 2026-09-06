# The Warrant — decision ledger

Campaign: **The Warrant** (Penstock stage 7b — the typed, compositional
intention). Branch `campaign/the-warrant`. Autopilot engaged
(`campaign-autopilot`), so every gate below G3 is auto-resolved against
standing policy and recorded here.

A *warrant* is both the authority for an errand and the justification for it.
That is exactly what this campaign gives a creature's walk: one committed
thing that says where it is going and why, in place of a prose sentence
restated at every step.

---

#1 [G1] — **What is the committed unit?** · Decision: the **errand** — a
maximal run of constant `Mode` + target — becomes one committed fact; the
per-step `agent-at` facts stay, and their `provenance` stops carrying authored
prose. · Why: the Penstock metaplan §5.6 sanctions this object by name ("the
discrete divergence — *this agent resolved to go there, on this day* —
commits, and the step sequence stays derived"), and `Mode`'s own doc
(`windows/vessel/src/liveness.rs:2096`) already calls itself "the errand an NPC
is on", so the unit is not invented here, only promoted from tick-local to
committed. · Alternatives discarded: (a) *per-drive-episode* commits (coarser
than an errand — would merge the outbound and homing legs and lose "walking
home (sated)" outright: strictly less content, rejected); (b) *keep prose and
add the typed fact alongside* (two sources for one meaning, free to disagree,
and it defers the fidelity call into 7c, which is precisely the ordering
decision 0238 forbids). · ideonomy passes / overturns: 1 pass (organon-
construction + combination; scale organon on longevity × cardinality ×
polarity) / 0 overturns of the core, 3 enrichments — see #5. · Capture:
this ledger; spec §3.

#2 [Q] — **Does 7b remove the per-step `agent-at` facts?** · Decision: **no.**
7b retypes the *why* and adds the errand fact; removing facts is 7c. · Why
(precedent): decision 0238's table assigns "fact lifetime: what may leave, and
how the prefix re-derives" to **7c**, not 7b; and `agent_position`
(`liveness.rs:45`) folds `agent-at` as the position store, with the roster
invariant `roster.positions()[slot] == agent_position(...)` restated at five
sites in `session.rs`. Dropping per-step facts in 7b would break every
position reader and pre-empt 7c's own design. · Alternatives discarded:
dropping the steps now (breaks `agent_position`; also breaks
`the_kerf.rs:270`, `the_roll.rs:1083`, `resident_folds.rs:3820`, each of which
asserts a positive `agent-at` count). · ideonomy passes / overturns: 1 / 0 —
the pass confirmed the split and surfaced that the *idea-registry row*
`UNI-intention-is-structured` describes the end state ("the per-errand fact
that **replaces** per-step commits") rather than 7b's own delivery, which is a
row that needs amending. · Capture: spec §2 and §9; registry-row amendment
listed in the capture manifest.

#3 [Q] — **Where does the errand's reason live, given `Value` is only
`{Entity, Text, Number, Flag}`?** · Decision: in the **predicate**, drawn from
a closed registered family (`errand/*`), with the target in the object as
`Value::Text(room_to_text(target))` — the same encoding `agent-at` already
uses — and `provenance` returned to naming its producer. · Why: `Fact`'s own
doc calls the envelope "dumb … semantics live in the concept registry"
(`kernel/src/ledger.rs:67`); `register_predicate(name, functional, doc)` gives
each predicate a doc string, and that doc is *already* the only prose
`windows/historiography` renders for a predicate ("an agent's position on a
day"). Putting the reason in the predicate therefore moves the reader-facing
words from the ledger into the registry — which is `kernel/src/phenomena.rs`'s
producer rule ("a producer cannot know who is looking … a stored string could
only ever be culture-neutral or wrong") applied to facts, the exact convention
gap `UNI-intention-is-structured` names. The per-drive predicate family is
established convention here, not a novelty: `drank`/`eaten`/`rested`/`slept`
are already four predicates for one relation. · Alternatives discarded:
(a) *one predicate `intends`, packed object* `Text("thirst-known@229504")` —
one key instead of eight, but no per-reason registry doc, so the prose returns
to code and the language layer has nothing to realize; (b) *reason in
`provenance`* — re-creates the very gap being closed, since `provenance` is
tagged `bare-ok(prose: provenance)` and every other producer in the repo puts
a *system* name there (`astronomy`, `species`, `the-roll`); (c) *widen `Value`
with a structured variant* — correct in the abstract and a kernel save-format
change touching every match arm, every serialized surface and the clients, for
a benefit this campaign does not need. · ideonomy passes / overturns: 1 / 0.
· Capture: spec §4; alternative (c) → idea-registry row (capture manifest).

#4 [G2] — **Is the fidelity premise true as rendered?** · Decision: measure it
before asserting it, and lead G3 with the measurement. · Why: decision 0238
rests 7b's whole existence on "the trail is *content*, not bookkeeping",
asserted from reading the code and never measured against a rendering. The
autopilot rule "verify generated-artifact and tool-behavior claims" applies
to a *fidelity* premise at least as strongly. · Result: measured, and the
premise is **half true** — see spec §1. The distinct-string content of an
errand is fully preserved by a per-errand fact by construction; what the
per-step commits add is repetition, 2.36x to 77x depending on regime. ·
ideonomy passes / overturns: 0 — this is a measurement ruling, not a design
choice; no pass was run for it. · Capture: spec §1; the probe commands are in
the spec so they are re-runnable.

#5 [G2] — **Three enrichments from the #1 ideonomy pass**, adopted. ·
(a) *Polarity*: `Intention::Go(target, reason)` as the registry row spells it
**under-types two of the eight cases** — `Mode::Homing`/`Idle` is not a
pursuit at all, and `Pursuing(Danger)` is repulsion from a threat rather than
attraction to a target. The typed reason must be `Mode`-shaped (approach /
flight / sated-return), not `DriveKind`-shaped. (b) *Longevity*: the errand
is a point on a scale (step · errand · drive-episode · mission · standing
disposition), and naming it as such keeps the door open for a longer-lived
intention without re-litigating the unit — the metaplan §5.8's "one gap … a
multi-tick plan" lives one rung up this scale. (c) *Cardinality*: an errand
that is **abandoned** — preempted before its target is reached — is invisible
today (the trail simply stops mentioning that drive) and becomes visible
content the moment the intention is committed. That is the raw material for
§5.7's preemption-to-invalidation ratio and §5.9's sunk-cost study, delivered
as a side effect. · ideonomy passes / overturns: recorded under #1's single
pass. · Capture: spec §3.2, §4.1, §10; (b) and (c) → idea-registry rows.

---

## Follow-ups

- `book/src/frontier/idea-registry.md` row `UNI-intention-is-structured` says
  the per-errand fact "replaces per-step commits". Amend to say it *carries the
  why* while 7c does the replacing (see #2).
- `windows/vessel/tests/suite/tick_commit_budget.rs:527-530` reads
  `FEAR_OR_BELONGING_CEILING` by grepping `provenance` for `"(fear)"` and
  `"(belonging)"`. After this epoch that instrument is **vacuous, not
  failing** — it reads 0 and passes. It must be re-pointed at the errand
  predicate in the same commit that moves the provenance.
- Nine-plus hand-written `book/src/chronicle/` pages quote the exact
  provenance prose and are *not* declared in `docs/generated-paths.txt`, so
  nothing will tell you they went stale. Enumerated in spec §7.3.

---

#6 [G4] — **The errand fact's object is its ORIGIN, not its target.** ·
Found while writing the plan, in the spec's own §4, after G3 approval. · The
first draft specified `object: Value::Text(room_to_text(target))`. The
arbitration seam exposes `Intent::Do(Action)` and nothing else
(`liveness.rs:1920`); `Action::MoveTo(n)` is the *next step* and
`Drive::proposal` is documented as "the next executable step". **No
destination is materialized at the commit site**, so the field as written had
no possible caller. · Decision: the object carries `st.pos` at the instant the
errand's first step is charged; the errand's **endpoint is derived** from the
`Trail` at the next errand boundary. · Why: surfacing a goal through
`Drive`/`Resolution` is an arbitration-seam change that spec §9 forbids and
would make an epoch a behaviour change too; `Flag(true)` discards a free and
useful fact. The origin is always available and never wrong. · Consequence,
and it is an improvement: the recount now says where a creature *got to*
rather than where it *meant* to go, so an **abandoned** errand (§3.3) does not
make the ledger assert an intention the code never formed. 7c inherits one
asymmetry, recorded in §8.1. · Alternatives discarded: seam change; bare flag;
special-casing `errand/water-known`, where `st.believed` does hold a real
target — rejected to keep one shape for all eight keys. · ideonomy passes /
overturns: 0 — a correction forced by the code, not a design choice; no pass
was run for it. · Capture: spec §4.0 (a new subsection, written as a
correction rather than a silent edit), §5.1, §5.3, §8.1.

**Process note this campaign should carry to its retrospective:** the
autopilot rule that catches this class ("verify tool-behavior claims with a
command before writing them") was applied at G3 to the *fidelity* premise and
not to the *mechanism* claim beside it. The mechanism claim was an imperative
("the target in the object") with an assertion hiding inside it — the exact
shape the skill's own "imperative mood hides assertions" table describes. It
survived a G3 package and was caught only by reading the seam to write Task 1.

---

#7 [G4] — **Pre-flight scan rulings (3 plan defects, ruled before Task 1).**
The scan table is in the plan's SDD workspace ledger; the rulings are here
because rulings are durable and task state is not.

**R1 — the eight glosses exist twice between Task 1 and Task 3, with nothing
pinning them equal.** Task 1 puts them in `errand_predicates()`; the live
prose match at `liveness.rs:8137` still emits them until Task 3 deletes it. A
rule duplicated on purpose needs a **two-way agreement test**, so Task 1 gains
one — the registry docs are exactly the strings the prose match emits, asserted
from inside `liveness.rs` where both are visible — and Task 3 deletes it in the
same commit that deletes the match. Cost if wrong: the two copies drift inside
a two-task window and the flip silently changes a rendered string.

**R2 — Task 4's `Errands` tenant has no production consumer in this campaign,
and spec §6 is wrong about why it would.** §6 says `why?` needs a
binary-searchable index; §5.4's own design has `recount` group by a single
pass over the fact list it has *already* collected, and `recount` takes
`&World` — it has no `ResidentFolds` and cannot get one without a
`windows/historiography` → `windows/vessel` dependency the layering test
forbids. So the index would ship unused. **Ruling: Task 4 is dropped from The
Warrant** and the tenant is recorded as owed by whichever campaign first has a
consumer — 7c's compaction, or the metaplan §5.7 counters, both of which are
this campaign's declared non-goals. This is the second "no possible caller"
defect in my own text this campaign (see #6); both were caught by reading the
code rather than re-reading the prose. Cost if wrong: 7c pays for the tenant
instead, which is where its consumer actually lives. **Nathan sees this at G6
as a deliberate scope reduction, not a silent one.**

**R3 — the registration-site list is incomplete and its line numbers have
drifted.** The plan named five sites from memory of a subagent's report. Grep
finds `windows/vessel/src/session.rs:1595` (which the plan described but
mislocated), `windows/lab/src/synthetic.rs:152`,
`windows/lab/src/health.rs:336`, `windows/lab/examples/rest_site_census.rs:360`
(not 361), `windows/lab/tests/suite/hearth_population_calibration.rs:442` (not
443), and one the plan missed entirely,
`windows/vessel/examples/fold_depth_sweep.rs:712`. Ruling: the brief stops
enumerating and instructs the implementer to **derive the set from the
observable** — `grep -rn 'register_predicate(AGENT_AT'` — registering the
errand keys at every production and example site, and at a test site only
where that test reads an errand back. A brief that enumerates call sites is
asserting completeness, and completeness is what enumeration gets wrong. Cost
if wrong: a session registry lacking the keys renders errands as bare
predicate strings instead of glosses.

· ideonomy passes / overturns: 0 — three defect corrections forced by grep,
not design choices. · Capture: plan text amended for R1 and R3; Task 4 struck;
spec §6 amended to record that its stated consumer does not exist.

---

#8 [G5] — **Task 1 review, Critical finding: my own brief's example code did
not implement my own brief's stated requirement.** · The requirement was "a
test that fails if **either** copy drifts". The example code I supplied
compared two `BTreeSet<&str>`s, which is **membership** in both directions,
not **correspondence**. Two sets of the same eight strings are equal when two
have been SWAPPED between keys — and a swap is the likelier authoring mistake
precisely because it leaves no orphan string for a set comparison to notice.
The implementer transcribed the snippet faithfully; the defect is the brief's.
· Decision: the finding is upheld in full. The test becomes a per-mode pairwise
assertion through `errand_key`, and the mutation proof must exchange two
glosses rather than corrupt one — a mutation that introduces a string appearing
nowhere else proves only the weaker property. Plan text corrected in the same
commit as the ruling, because Task 3 reads that file too. · Why it matters
beyond the test: Task 3 deletes `prose_for`, leaving `errand_predicates()` as
the only copy. A swap latent at that moment becomes permanent and undetectable
— the wrong gloss would render forever, on the right key, with every test
green. · ideonomy passes / overturns: 0 — a review finding upheld, not a design
choice.

**This is the fourth defect in my own text this campaign** (#6 origin/target,
#7 R2 unused tenant, #7 R3 enumerated sites, #8 here), and the first three
were caught by reading code while this one needed a reviewer. Its shape is
distinct and worth the retrospective: I wrote the requirement **correctly in
prose** and then supplied example code that did not implement it. A brief
carries two claims — what is required, and that the code shown satisfies it —
and only the first was audited. Prose and its own exemplar must be checked
against each other, not just against the world.

#9 [G5] — **Task 1 review, Important finding: three registration sites use a
loop-table idiom the brief's grep cannot match.** ·
`windows/vessel/examples/session_length_scaling.rs`,
`windows/vessel/tests/suite/the_detent.rs` (`bench_shape`) and
`windows/vessel/src/liveness_tests/emitter_scan.rs` register `AGENT_AT` as
`for (pred, doc) in [(AGENT_AT, "..."), ...]`, so `grep 'register_predicate(AGENT_AT'`
misses them **by construction**. All three run real 60-tick walks with a
panicking `.expect` on every committed fact, so each panics the moment Task 2
emits an `errand/*` fact under an unregistered predicate. · Decision: upheld;
register all three. · The lesson is the one R3 was already about, one turn
deeper: R3 replaced an enumerated list with a grep, and the grep was itself an
enumeration in disguise — it enumerated *one calling idiom*. The observable to
derive from was never the call shape; it was **which code paths run a real
walk**. `session_length_scaling.rs` is the sharpest case: its own comment says
it was copied from `agent_scaling.rs`, which this diff DID register. · ideonomy
passes / overturns: 0.

---

#10 [G5] — **Task 1 complete; two pre-existing defects found by looking, and
parked with reasons.** · Both findings from #8 and #9 verified ADDRESSED by a
scoped re-review that traced the pairwise test through the code rather than
trusting the report, and confirmed each of the three new registrations sits in
the path that actually runs a walk rather than a decoy constructor. · The
re-reviewer went one step past `cargo build --examples` and actually RAN
`windows/vessel/examples/session_length_scaling.rs`. **It panics** —
`UnknownPredicate { predicate: "slept-on" }`, several ticks into the loop.
Pre-existing: confirmed present at base `5098fc054`, before this campaign
touched the file. `windows/vessel/src/liveness_tests/emitter_scan.rs` has the
same gap and its tests pass only because their scenarios never emit a
`slept-on` fact. Both parked, not fixed: out of scope for an epoch about
errands, and fixing an unrelated example mid-campaign widens the diff a
reviewer has to hold. · **The part worth keeping:** the panic is itself the
evidence that Task 1's registration there is correctly wired — the run got
*past* registration and into the tick loop before failing on something else.
A build check would have proved neither. That is the difference between
compiling a site and exercising it, and it cost twelve seconds. · Follow-up
rows owed at close for both. · ideonomy passes / overturns: 0.

---

#30 [G5] — **Task 2 complete: the errand-boundary commit, and a cross-tick
continuity defect the brief's own test caught.** · Implemented exactly as the
brief's Step 3 specified — `WalkState.errand: Option<&'static str>`,
`errand_fact` beside `agent_at_fact`, the boundary check immediately before
`out.push(agent_at_fact(...))` in the `MoveTo` arm — then ran the brief's own
`one_errand_fact_per_run_of_constant_step_provenance` and it went RED: one
walked entity showed 7 errand facts against 5 provenance runs. · Root cause:
`step_with_occupancy` opens a fresh `WalkState` once per `wait` ("one walk per
wait", `session.rs`'s own comment on the call site), so seeding
`errand: None` in `begin` — literally what Step 3 says — recommits an errand
fact at the START of every `wait` whose reason merely CONTINUES from the
previous one, one per TICK on that boundary rather than one per ERRAND. ·
Decision: seed `errand` from `frozen` in `begin` instead, via a new
`latest_committed_errand` helper (scans all eight `errand_predicates()`
streams for the entity's own latest, mirroring `last_drank`/`last_ate`'s
existing shape for the identical problem). Both of the brief's tests pass
after the fix, and `an_errand_commits_once_and_its_steps_commit_under_it`'s
own coverage assertion (every step has a covering errand at or before its
day) also depends on this — it would have passed even under the naive `None`
seed, so it did not catch this on its own; only the run-count test did. · This
is a deviation from the brief's literal Step 3 text, not from its intent —
the brief's own two tests state the intent precisely, and the naive
implementation fails the second one. · ideonomy passes / overturns: 0.

#31 [G5] — **Fourteen liveness.rs unit tests moved, all legitimately.** ·
Full-crate `cargo nextest run -p hornvale-vessel` after the boundary landed:
14 failures, all self-contained in `liveness.rs`'s own `mod tests` (none in
`tests/suite/`, none outside this crate). Three shapes, not one: (a) 11
`UnknownPredicate { predicate: "errand/..." }` panics — narrow hand-built
`ConceptRegistry`s that predate this campaign's new commit path and were
correctly left unregistered by Task 1 (Task 1 added no new committed
predicate; Task 2 does) — fixed by registering `errand_predicates()` beside
each site's existing `AGENT_AT`/`DRANK`/`RESTED`/`SLEPT`/`EATEN` block; (b)
`h4_the_distinct_fact_shapes_imposed_and_free_can_reach_are_identical`'s
closed 5-predicate roster assertion, now 13 (updated the assertion, not
silenced it); (c) `drinking_and_eating_now_cost_time`'s strict
day-must-strictly-advance loop, which an errand fact and its accompanying
`agent-at` deliberately violate (same tick, same day, zero elapsed cost
between naming a reason and taking its first step — spec §4.0) — narrowed
the loop to exclude `errand/*` facts, with the reasoning stated inline rather
than silently dropped. · The 80→108→132-fact `the_hoisted_walk_emits_
exactly_what_the_loop_emitted` golden moved too: mechanically diffed the
regenerated 132-fact sequence against the committed 108 with every
`errand/*` row filtered back out — byte-identical, same order, same days,
same ids — before replacing the literal, so the update is a verified
addition rather than a re-recorded failure. · All 1169 `hornvale-vessel`
tests, all 513 `hornvale-lab` tests, and all 460 `hornvale` (cli) tests pass
after. · ideonomy passes / overturns: 0.

#32 [G5] — **H3 (spec §10) measured; no STOP triggered.**
`tick_commit_budget::facts_committed_per_agent_per_tick_stays_bounded`:
last-half rate **1.785075** facts/agent/tick against `STEADY_STATE_CEILING`
= 2.5 (first-half 1.855970 — non-growing, well inside `NON_GROWTH_MARGIN` =
1.10). `the_commit_rate_is_carried_by_the_settled_rosters_even_churn`: 67
residents contributing in the last half (>= `MIN_CONTRIBUTING_RESIDENTS` =
60), 0 (fear)/(belonging)-tagged facts. Both green; none of the three guard
rails moved against this task, matching the brief's own prediction (this
task only adds facts). Seed 42's own agent roster at this point in the
codebase's history is 67 agents, not the 6-7 an older comment block in that
file's own doc records — a pre-existing staleness in that file, unrelated to
this campaign, left unfixed (widening this diff to a documentation sweep of
an unrelated file was not this task). · ideonomy passes / overturns: 0.

#33 [G5] — **Step 5 fixture: `tests/fixtures/the-warrant-glosses.json`,
frozen and undeclared.** · Captured, by hand, the ordered `(day, provenance)`
run-start pairs for every entity that committed at least one `agent-at` under
the same harness the new tests use (seed 11, 12 waits) — 26 entities, empty
walkers omitted as noise. Generated via a scratch `#[test]` added to
`the_warrant.rs`, run once with `--nocapture`, its stdout piped to the fixture
file, then the generator test deleted before this commit — the fixture is a
historical snapshot of the PRE-flip prose, and nothing should ever regenerate
it from live code once Task 3 lands (regenerating would just re-derive the
NEW provenance strings, defeating its purpose as a positive control). Left
undeclared in `docs/generated-paths.txt` for exactly that reason: it is a
frozen pin, not a generated artifact. · ideonomy passes / overturns: 0.

---

#11 [G5] — **H3 is CONFIRMED, not falsified, and the implementer's report says
otherwise because it compared against a stale committed baseline.** · Task 2
reported "H3 measured at 1.785075 facts/agent/tick against a 2.5 ceiling",
implicitly against the 1.06 that `liveness.rs`'s own comment records — a jump
that would mean seed 42 had started committing errand facts, contradicting the
spec's §1 measurement. · **Measured both sides rather than reasoning about
them.** Two independent probes (`possess --seed 42` with one `wait 40`, and
again with forty single `wait`s, sweeping all 67 residents by `!why`) find
**zero** `agent-at` renders and **zero** errand glosses. Then the decisive one:
checked out `3aa975838` — the commit immediately before Task 2 — and ran the
instrument there. The per-tick series is **byte-identical** and the rates are
identical to six decimal places: first-half 1.855970, last-half 1.785075. Task
2 moved nothing on seed 42. **H3's "and by exactly 0% on seed 42" holds
exactly.** · **The real finding is a repo defect this nearly turned into a
false alarm.** `tick_commit_budget.rs`'s module doc records the rate as
"roughly flat at ~0.92-0.96", and `liveness.rs`'s hoist-golden comment records
"1.06 … (0.96 before Task 7, 1.24 after it, 1.01 before this fix round)". The
instrument reads **1.79-1.86**. Both prose baselines are stale by roughly 2x,
drifted there by campaigns that raised the ceiling (1.5 -> 2.5) without
restating the measured value beside it. A reader arriving at this number finds
two committed explanations waiting for it and both are wrong — which is the
exact failure that file's OWN module doc, at line 51, warns about in a
different register ("this doc had already pre-committed to the benign
reading"). · Decision: correct both prose baselines as part of this campaign's
freshness sweep, with the date and the commit they were measured at, and add a
follow-up row. Do **not** touch the ceiling. · ideonomy passes / overturns: 0.

---

#12 [G5] — **Task 2 approved (spec ✅, 0 Critical, 0 Important, 5 Minor).** ·
The review re-derived the task's central claims from a live ledger built in a
throwaway probe outside the repo rather than reading the report: 290 steps,
138 errand facts, 138 provenance run-starts, zero per-entity mismatch, zero
double-commits, and every errand fact's object equal to the PREVIOUS
`agent-at` object — the origin, confirmed rather than argued. A→B→A was
observed live (`comfort, home, water-blind, comfort, home, comfort,
water-known` on one entity), so an interrupted-and-resumed errand does
re-commit. · **The Step-3 override is upheld and is load-bearing.** Within a
tick, `st.errand` carries; across ticks, `latest_committed_errand` re-derives
from the frozen ledger, and `Session::wait` commits the walk's facts into that
ledger before the next tick passes it back — no gap between the two. The case
occurs: one entity's `comfort` run spans days 1.43 to 2.98, straddling two
wait boundaries, and would have re-committed once per tick under the brief's
`None` seed. Two probe runs byte-identical. · The 108→132 golden is additive,
verified from the diff mechanically (`grep -c '^-'` over the golden hunks
returns 0; the whole `liveness.rs` diff removes nine lines, all accounted
for), and corroborated independently by `plumb-roster.md` moving `&str`
265→266 — exactly one new const. · Two Minors are carried into Task 3 rather
than deferred, because Task 3 edits the same file: the stale "the five" roster
message at `:13600`, and `latest_committed_errand` lacking the `day <= t`
filter its sibling carries (safe today only through an invariant documented in
a *different* function, which is the kind of safety that stops being safe
quietly). · ideonomy passes / overturns: 0.

---

#34 [Task 3] — **H2 IS FALSIFIED, and the null is the headline.** Preregistered
(spec §10): *provenance bytes committed per agent per tick fall by at least 50%
in the walking regime, and by exactly 0% on seed 42.* The instrument is the §1
probe's shape (`Session::start` + 12 `wait`s), measured as the provenance-byte
sum over the facts committed during the wait loop, over `roll_len() - 1`
agents × 12 ticks. Before-side taken by building the **merge base**
`20c0cd375` in a throwaway worktree and running the same probe there — not
derived, not extrapolated:

```
seed  agents  main (bytes/agent/tick)  after   reduction   agent-at prov bytes
  7     101         141.037954        71.878713   49.04%    129886 ->  44520 (65.72%)
 14      58         196.903736        87.912356   55.35%    117093 ->  40020 (65.82%)
 23     109         109.123089        84.918960   22.18%     83319 ->  37860 (54.56%)
 42      67          73.250000        73.250000    0.00%          0 ->      0
```

**Clause 2 holds exactly**: seed 42 moves by zero, to the byte — same fact
count (1407), same provenance total (58893), same rate. **Clause 1 fails on
two of the three walking seeds.** Nothing is retuned. · **Why it fails is not
a defect in the flip, and stating it precisely matters more than the verdict.**
The preregistered quantity is *all* committed provenance, and only `agent-at`'s
share of it shrinks. On seed 23 the drive tick commits 920 errands to 2524
steps — an errand every 2.7 steps, the worst-case regime §1 itself
identified — so the errand facts' own `vessel/liveness` bytes (13800) buy back a
sixth of what the steps give up, and `agent-at` was only 58% of the total to
begin with. The right reading is the fourth column: **the `agent-at` provenance
reduction is 65.72% / 65.82% / 54.56%, over 50% on all three**. That is the
quantity §1 argued about ("the repetition is 98.5% of the rendered lines") and
the quantity the flip actually governs; §10 wrote the denominator wider than the
claim. Recorded as a falsification of what was frozen, with the narrower
measurement stated beside it — not as a redefinition of the hypothesis after
unblinding. · **H1 holds**, as exact equality: `every_gloss_and_its_first_day_
survives_the_flip` compares each entity's `errand/*` glosses and days against
`windows/vessel/tests/fixtures/the-warrant-glosses.json`, the before-image Task 2
froze while the prose was live — 26 entities, 138 run-starts, equal in both
directions (no entity in the fixture missing from the run, none in the run
absent from the fixture). · **This task removed no fact.** Pre-flip and post-flip
fact counts are identical on every seed (4035 / 3213 / 4870 / 1407), as are the
`agent-at` counts (2968 / 2668 / 2524 / 0). · ideonomy passes / overturns: 0.

---

#35 [Task 3] — **The brief's own suggested RED proof for the re-pointed
fear/belonging witness is a no-op, and finding that out is the point of Step 1.**
`tick_commit_budget`'s `FEAR_OR_BELONGING_CEILING` assertion counted facts whose
provenance contained `"(fear)"`/`"(belonging)"`; after the flip that matches
nothing, reads 0 and passes — an instrument that stops measuring inside a green
gate. It is re-pointed at `ERRAND_FLIGHT`/`ERRAND_COMPANY` **before** the flip,
as sequenced. The brief names "lowering the ceiling to 0" as the obvious
mutation: seed 42 commits **zero** fear/belonging errands, so `0 <= 0` still
passes and that mutation witnesses nothing. A ceiling mutation cannot fire
against a measured zero. · Substituted the FILTER instead — the two errand keys
for `"drank"`/`"slept"`, predicates this run does commit in quantity. Red at
2256, which proves the thing a permanently-zero witness otherwise cannot: that
the filter really reads the JSON `predicate` field and matches on exact key
equality. Both mutations recorded in the constant's own doc, target text
asserted present before substituting, file `diff`-confirmed byte-identical
after. · ideonomy passes / overturns: 0.

---

#36 [Task 3] — **A fourth in-crate provenance reader the brief did not list,
and its dangerous half was the NEGATIVE control.**
`the_herd_bolts_borrowed_alarm_makes_a_calm_creature_flee_then_settle` reads
`f.provenance.contains("fear")` twice: once as a positive floor (`b_fear_moves
>= 1`) and once as a control asserting a lone creature never flees (`c_fear ==
0`). The flip reddens the positive arm honestly — which is how it was found —
but the control would have gone on passing **for exactly the wrong reason**, a
zero that means "the predicate can never match" wearing the clothes of a zero
that means "the behaviour is absent". Both re-pointed at `ERRAND_FLIGHT`. The
lesson generalises past this test: when a flip retires a field, the arms that go
red announce themselves and the arms that assert ABSENCE over that field do
not. · **Method note, recorded because it cost a full 533 s suite run.** The
mutation-restore in one bash block ran as `set -e` + `cargo nextest … | grep …`;
the shell has `pipefail`, the failing test aborted the block before the `cp`
restore, and the "RESTORED byte-identical" line that would have said so was
simply never printed. Two subsequent edits landed on a mutated file and six
tests failed for a reason that looked like the flip. The tell was the absent
confirmation line, not the failures. Restore-and-`diff` belongs in its own
invocation, never downstream of a command that is expected to fail. · ideonomy
passes / overturns: 0.

---

#37 [Task 3] — **The two carried Minors, both closed.** (a) The H4 assertion
message said "one of the five this file can ever commit" against a roster of 13;
it now interpolates `known_predicates.len()`, so the denominator cannot go stale
again rather than being corrected to a new literal that can. (b)
`latest_committed_errand` gains the `day <= t` filter its sibling
`latest_committed_position` carries, rather than a doc note — **the filter, not
the documented invariant, and here is why.** The unfiltered form was safe only
through an invariant stated in `agent_position`'s doc, a *different* function: a
correctness argument a reader of this function cannot see and a future caller
cannot be expected to preserve. The sibling carries the filter for precisely the
case that would break it (The Phantom's transient-danger memory re-derives a
PAST instant, where an unfiltered read answers with the future). It is a no-op
for today's one caller — `WalkState::begin` passes the walk's own `from` against
the frozen pre-tick ledger — and the doc says so, so nobody deletes it as dead
code. Full vessel suite (1169 tests) green after, confirming the no-op. ·
ideonomy passes / overturns: 0.

---

#17 [G5] — **H2 IS FALSIFIED, AND IT STAYS FALSIFIED.** · Measured against a
real build of merge base `20c0cd375`: provenance bytes per agent per tick fall
**49.04% / 55.35% / 22.18%** on seeds 7 / 14 / 23 against a preregistered floor
of 50%, and by **exactly zero** on seed 42 (1407 facts, 58893 bytes,
unchanged). Two of three walking seeds miss the floor. · **Ruling: the
hypothesis is reported falsified, unamended.** Spec §10 says a null on H2 is
the headline and is not retuned away; that binds when the null is inconvenient
or it binds never. The chronicle leads with the falsification. · **The cause is
a defect in my own §10, and naming it must not become a retroactive rescue.**
§1 argued about the **`agent-at` trail's** prose; §10 then froze the
denominator as **all committed provenance**, a quantity §1 never discussed.
Only `agent-at`'s share can shrink, and errand facts add provenance back — on
seed 23 an errand commits every 2.7 steps, which is why it lands at 22%. On the
quantity the flip actually governs, the reduction is **65.72% / 65.82% /
54.56%** — comfortably past 50% on all three. **Both numbers are reported. The
second is context for why the first came out as it did, not a substitute
result**, and the difference between those two sentences is the whole
discipline. · This is the ledger's own memory twice over: *a null needs its
denominator*, and *filter the numerator too*. I wrote a floor over one
population having argued the case for a different one, which is the same
family of error as #6 and #8 — prose and its own formalisation disagreeing,
audited only on the prose side. · **The spec is NOT edited to move the
goalpost.** §10 gains a dated note recording that its denominator does not
match §1's argument, that the prediction failed as written, and what the
correctly-scoped figure was. A preregistration that can be re-scoped after
unblinding is not one. · ideonomy passes / overturns: 0.

#18 [G5] — **Task 3's three incidental findings, all upheld.** ·
(a) **The brief's suggested RED proof for the re-pointed fear/belonging
witness was a no-op.** I wrote "lowering the ceiling to 0 against a run that
produces one is the obvious candidate"; seed 42 measures zero fear/belonging
errands, so ceiling→0 passes. The implementer mutated the *filter* instead
(errand keys → `"drank"`/`"slept"`, red at 2256), which additionally proves the
check reads the `predicate` field rather than merely that a number is under a
ceiling. Strictly better than what I asked for, and it is the fourth time this
campaign that naming the property and letting the implementer find the mutation
beat prescribing one from outside the code. · (b) **A fourth provenance reader
the brief did not list, whose dangerous half was a NEGATIVE CONTROL.**
`the_herd_bolts_…`'s `c_fear == 0` assertion would have kept passing after the
flip *for exactly the wrong reason* — the control asserts an absence, and the
flip makes the absence unconditional. A vacuous positive check reads as
coverage; a vacuous negative control reads as *proof*, which is worse.
Re-pointed at `ERRAND_FLIGHT`. · (c) **`one_errand_fact_per_run_of_constant_step_provenance`
is retired**, not deleted quietly: it compared errand counts against live
`agent-at` prose runs, which the flip destroys by construction. Its successor
is the fixture-backed H1 test, which the implementer states is strictly
stronger. **That claim is the review's job to check** — a delete-plus-add reads
as a replacement while covering a different branch, and the honest test is
whether the successor catches what the retired one caught. · ideonomy passes /
overturns: 0.

---

#19 [G5] — **Task 3 review, Important: deleting the transitional test left the
key→gloss pairing unpinned for four of the eight errands, exactly as that
test's own doc predicted.** · Deleting
`the_registry_glosses_and_the_live_prose_match_agree_both_ways` was correct —
`prose_for` is gone, so it cannot exist in that form. But it was the pin, and
what survives it is thinner than it looks: `every_mode_maps_to_exactly_one_errand_key`
pins Mode→key for all eight, the spelling test pins the eight key strings, and
`every_errand_predicate_carries_a_distinct_non_empty_doc` checks only
distinctness and non-emptiness — **which a swap satisfies**. The only thing
pinning key→gloss is the H1 fixture, and seed 11 produces just four of the
eight glosses. `errand/forage`, `errand/rest`, `errand/flight` and
`errand/company` are pinned by nothing: swap two of their glosses today and
the whole suite stays green, permanently, with the wrong words rendering on
the right key. · **This is Task 1's Critical (#8) resurfacing one task later
in a new shape.** There the defect was a test too weak to catch a swap; here
it is the correct deletion of the only test that could, with no successor
taking over its half of the job. A transitional guard's retirement is a
coverage event and must be accounted for like one — "delete it with the thing
it pins" is only half an instruction. · Decision: upheld and fixed in this
task's fix round — an eight-row literal key→gloss table in `the_warrant.rs`,
the same shape as the spelling test, making the pairing a save-format-grade pin
rather than a seed-11 accident. · ideonomy passes / overturns: 0.

#20 [G5] — **Task 3 review, plan-mandated Minor: H1 was measured on a
different population than the one preregistered, and I recorded it as
"HOLDS".** · Spec §10 freezes H1 as "for every resident on **seeds 7, 14,
23** over 12 days". The implemented test runs on **seed 11**, because Task 2's
before-image fixture was frozen on seed 11 and the brief pointed H1 at that
fixture. The test is a real exact-equality before/after comparison and it does
hold — on seed 11. · **The reviewer's framing is the part I want kept:** both
preregistered hypotheses were evaluated against a population other than the
frozen one; H2's mismatch was ruled a falsification and reported unamended
(#17), and H1's was not surfaced at all. Reporting one and not the other is not
a defensible asymmetry, and the direction of the omission — the one that made
the campaign look better — is exactly the direction that needs a rule rather
than a judgement. · Decision: **discharge it properly rather than confess it.**
The reason H1 landed on one seed is structural — a before-image can only exist
for a seed captured while the prose was live — but that is a reason to go build
the missing before-images, not to narrow the hypothesis. The merge-base build
the implementer already stood up for H2 can capture seeds 7, 14 and 23 the same
way. H1 is then discharged as written. If any of the three disagrees, that is a
finding and it is reported like #17 was. · Interim status, until the fix round
returns: **H1 is discharged on seed 11 and untested on 7/14/23** — not
"HOLDS". · ideonomy passes / overturns: 0.

#21 [G5] — **Two further Task 3 Minors, both to the freshness sweep.** ·
(a) `player_acts_commit.rs`'s module doc claims a player's trail is
"indistinguishable from a creature's … same provenance kind". After the flip
that is false in substance — a creature's step carries `vessel/liveness`, a
player's carries prose — while the test still passes, because it asserts only
non-emptiness and the absence of five driver tells. Not a defect in the diff
(spec §7.4 makes the asymmetry deliberate and the brief forbids touching the
player constants), but a doc that now reads as coverage it no longer has.
(b) The H1 test's panic says "do not regenerate the fixture", which is right
for a provenance regression and **wrong for the case that will actually
occur** — a future terrain or drive change moves seed 11's walk and this test
reds with a message forbidding the one legitimate remedy. Name that cause and
its corroborating evidence (the `hoist_walk_shape` golden moving in the same
commit) so the guard does not become a trap. · ideonomy passes / overturns: 0.

---

#22 [G5] — **Task 3 complete; H1 discharged as preregistered, on three preregistered seeds plus one.**
· All four findings verified ADDRESSED by a re-review that reproduced the
mutation itself rather than reading the report. The evidence worth keeping is
what stayed GREEN under the swap: distinctness (both copies), the spelling
test, `every_mode_maps_to_exactly_one_errand_key`, the
no-authored-prose test, the errand-boundary test — **and H1 itself**, which
passes under a swapped literal because it resolves glosses through the real
registry rather than through the test's own table. Six tests satisfied by the
mutation, one catching it: that is the coverage gap of #19 demonstrated rather
than argued, and it is also an independence proof for the new table (it is not
merely a second copy of something already checked). · **H1 as frozen in spec
§10 now holds.** Merge base `20c0cd375` confirmed a genuine pre-flip commit by
two independent tests — its `liveness.rs` still carries the `match st.mode`
prose block, and it is an ancestor of the flip commit — so the before-images
cannot have been derived after the fact. 252 entities, 1242 run-starts across
seeds 7/11/14/23, exact equality both directions, zero disagreement. Seed 11's
fixture is byte-identical across the fix range. A hardcoded `(252, 1242)` shape
pin guards against a fixture table silently shrinking or a seed going quiet. ·
Cost disclosed and accepted: H1 is ~150 s over four world builds, correctly
outside the commit gate. · Correction to my own #19-era record: the seed-23
fixture is **64,846 bytes (~65 KB)**, not the ~120 KB the fix report and my
summary of it stated. · ideonomy passes / overturns: 0.

---

#23 [G5] — **Task 5 review: the grouping-rule departure is upheld, and the
reason is better than the one the implementer gave.** · The implementer changed
the rule because a measured rendering regressed. The reviewer ruled it correct
from the *producer* instead, which is the stronger argument: **nothing anywhere
assigns `st.errand = None`.** There is no close fact, no expiry, no end marker.
An errand ends only implicitly, when the next `MoveTo` computes a different
key. So "open until the next errand fact" is not a rendering choice at all — it
is a readback of the model the ledger already implements, and it is the same
words Task 2's own coverage test uses (`rfind(|e| e.day <= step.day)`). My
brief's "any other predicate flushes the group" described a rule the ledger does
not implement. · The mis-attribution I asked about — a step after one errand
ended and before the next began — **cannot occur, because the premise cannot**:
under the producer, such a step *is* covered by that errand. · ideonomy passes /
overturns: 0.

#24 [G5] — **Task 5 review, two Important findings, both upheld: correct code
with no guard on it.** · (a) **`group()`'s provenance guard survives
mutation.** Replacing `f.provenance == facts[e.opened].provenance` with `true`
leaves all 13 tests green — yet that guard is exactly what stops a
hand-planted `harness-placement` step being swallowed as a step of an errand it
had nothing to do with. The existing uncovered-step test only plants the
harness step *before* the errand, where the join rule never runs. Remedy: one
test of the mid-errand shape (errand → step → harness step → step). · (b) **The
implementer's own concern 2 is worse than it documented.** A second
`vessel/liveness` predicate arriving first after an errand does not merely get
*counted* as a step — it **fixes the group's step predicate**, so every genuine
`agent-at` after it is orphaned and renders `(asserted by vessel/liveness, …)`.
That is this campaign's own regression reappearing silently, and the roll-up
line reads `ending at true`. Unreachable today (`ERRAND_PRODUCER` occurs in
exactly two constructor positions) but nothing would redden the day a third is
added. · **The reviewer's placement ruling is adopted and is the interesting
part:** the renderer cannot express the constraint without naming `agent-at`,
which layering forbids — so the ratchet goes where the invariant actually
lives, in `windows/vessel`: the only predicates ever committed under
`ERRAND_PRODUCER` are `AGENT_AT` and the eight `errand/*` keys. A guard belongs
in the crate that can state it, not the crate that suffers it. · ideonomy
passes / overturns: 0.

#25 [G5] — **An errand is never closed, and that is a design property with a
visible consequence 7c inherits.** · Because nothing emits an end marker, a
creature that walks for thirst, drinks, sleeps for a month and then walks for
thirst again commits **no new errand fact** — the two runs fold into one line
reading `3 steps, days 5 to 40`. That is a faithful reading of the ledger, not
a renderer bug, but it is the one shape where the roll-up's compression is
**misleading rather than merely lossy**, and it is the mirror image of the
campaign's headline: §1 measured that per-step commits carry one string's worth
of content N times; this is the case where one errand fact carries two
episodes' worth of walk. · Consequences: it goes in the chronicle as a stated
limit rather than being discovered by a reader; and **7c inherits it** on top of
the endpoint asymmetry already recorded in spec §8.1 — a compaction that drops
steps cannot recover the gap between two folded runs, because nothing marks it.
· An idea-registry row is owed for the unnamed option this surfaces: an errand
*close* fact, which nobody has argued for and which would make abandonment
(§3.3) and resumption distinguishable at the ledger rather than by inference. ·
ideonomy passes / overturns: 0.

#26 [G5] — **Three further Task 5 Minors.** · (a) The repl tolerates a typo'd
flag (`why 12345 --step` silently renders the rolled-up view) where the session
refuses loudly. Extra tokens were tolerated before this task, so tightening the
repl is a behaviour change to pre-existing latitude and is **not** taken here;
recorded so the divergence is deliberate rather than unnoticed. · (b) The repl's
nested "Seen through {species} eyes" branch always calls `recount`, never
`recount_steps`, so `--steps` does not reach it. Harmless today (species
entities commit no errands) and inconsistent with the flag's stated meaning —
fixed in the fix round. · (c) `recount_steps`'s doc calls the stepless errand a
case where "nothing is silently dropped", reading as if the walk emits them; it
cannot — the errand fact and its first step are pushed in the same arm.
Defensive handling is right, the prose is not. · ideonomy passes / overturns: 0.

---

#27 [G5] — **CORRECTION to #25: the misleading-span shape is real, but the
mechanism I recorded for it does not occur.** · #25 said a creature that
"walks for thirst, drinks, sleeps for a month, then walks for thirst again
commits no new errand fact — the two runs fold into one line". **Measured:
zero.** Scanning every errand line on seeds 7, 11, 14 and 23 for one whose span
brackets a `drank` returns nothing, and the reason is mechanical: a drink flips
the mode to sated, so the next step computes a different errand key and commits
a new fact. The two-runs-fold-into-one story was plausible, was relayed by me
from the review without being measured, and is false. · **The real shape is
narrower and better named: an errand outlives its own activity.** Seed 11,
resident 1: `walking home (sated) — 5 steps, days 113.04798 to 116.05578`,
bracketing **five sleeps and a graze** — a step every ~14 hours where a walking
step is ~4 hours apart. The errand is genuinely one errand; what the span hides
is that the creature was mostly not walking during it. The widest *continuous*
spans are ordinary: seed 7's 65 steps over 10.266 days, seed 14's 77 over
9.816. · So the chronicle states the limit as **"an errand's span measures
elapsed time, not time spent walking"**, not as "two episodes fold into one".
The consequence for 7c is unchanged in force and changed in shape: a compaction
that drops steps loses the *activity profile* inside an errand, which nothing
marks — but it does not lose an episode boundary, because a drive discharge
already creates one. · **The process point, which is the reason this entry
exists rather than a silent edit:** I relayed a reviewer's example into the
ledger as fact and told Nathan about it in the same breath. It was one probe
away from being checked, and the implementer ran that probe. A reviewer's
sentence can overstate its data, and mine inherited the overstatement without
adding any. · ideonomy passes / overturns: 0.

---

#28 [G5] — **Task 5 complete; #27's correction independently confirmed.** ·
All four findings ADDRESSED, each mutation reproduced by the re-reviewer rather
than read: the provenance guard reds exactly one test (the new mid-errand one),
and the `ERRAND_PRODUCER` ratchet reds on a `drank` fact given the producer
token. The ratchet derives its predicate set from a **real session walk**, not
a list of call sites, with two vacuity floors. · **Direction, stated because a
check that does not state it reads as total:** the ratchet enforces `observed ⊆
sanctioned` and is blind to a sanctioned key silently disappearing. Judged
correct — the hazard is a *foreign* predicate arriving under `ERRAND_PRODUCER`
and hijacking `group()`'s join rule; a sanctioned key going quiet does not
create it, and the two floors already catch the walk going globally silent. ·
**#27's correction re-measured independently at a wider sweep** — 249 errand
lines across seeds 7/11/14/23, residents 1-3, at 120 waits: **zero** whose span
brackets a `drank`. The seed-11 example reproduced verbatim, five sleeps and a
graze inside `walking home (sated) — 5 steps, days 113.04798 to 116.05578`. The
chronicle's stated limit now rests on two independent measurements rather than
on a reviewer's sentence. · ideonomy passes / overturns: 0.

---

#29 [G5] — **CORRECTION: H1's preregistered population is THREE seeds, not
four, and I said four twice.** · Spec §10 freezes H1 on "seeds 7, 14, 23" —
three. Seed 11 is the *fixture* seed, added by Task 2 because a before-image
can only exist for a seed captured while the prose was live, and it is
additional to the preregistration rather than part of it. My ledger #22 and two
statements to Nathan called it "all four preregistered seeds". · The result is
unchanged and if anything stronger than I described: H1 holds on **all three
preregistered seeds plus a fourth**. But "four preregistered" is a claim about
what was frozen, and the whole point of #17's ruling — that a preregistration
which can be re-scoped after unblinding is not one — is that the frozen text is
the authority on its own scope. Miscounting it in the direction of "we tested
more than we promised" is the same error as re-scoping it, wearing better
clothes. · Found by the Task 6 implementer reading §10 while writing the
chronicle, not by me re-reading my own summaries. · Second correction from the
same read: the seed-7 before-image is **42** bullet lines, not the 41 I stated
— 3 identity facts + 1 errand line + 38 steps. Counted, not estimated. ·
ideonomy passes / overturns: 0.

---

#38 [G5] — **This ledger had eight duplicate entry numbers, because it has
more than one writer and nobody allocated the numbers.** · Task 2's and Task
3's implementers each wrote their own rulings here — correctly; the
`campaign-autopilot` skill says rulings go in the committed ledger as they
occur — and each numbered from what it could see, while I was numbering from
what I could see. Result: two `#11`s, two `#12`s, two `#13`s, two `#14`s, and a
`#7` reused. Nothing cited the colliding numbers in any body, so the collision
was invisible until a reviewer counted the entries and got 33 against my stated
28. · Fixed by renumbering the implementers' blocks to **#30-#33** (Task 2) and
**#34-#37** (Task 3), leaving physical order and every cross-reference intact.
The numbers are therefore non-monotonic in file order, which is ugly and is the
honest repair: renumbering to restore monotonicity would have broken the five
in-body cites (`#2`, `#5`, `#6` ×2, `#8`, `#22`) that a reader actually
follows. · **The process defect is mine, not the implementers'.** A shared
append-only document with concurrent writers needs its numbers allocated, and I
dispatched five implementers at that document without ever saying which range
was theirs. It is the same failure shape as `.superpowers/sdd/followups.md`
(decision 0493) at a smaller scale — one path, several writers, silent
collision — and the remedy is the same: hand each writer a range, or key the
entries by something that cannot collide. Recorded for the retrospective. ·
Also corrected in passing: **#22's headline said "on four seeds"**, which #29
already corrected in substance but not there. · ideonomy passes / overturns: 0.

---

#39 [G5] — **Two corrections and one finding sharper than the one I gave it.**
· (a) I told the Task 6 implementer this ledger "now has 38". It has **34**;
38 is the highest *number*, and my own repair left #13-#16 vacant. A bare
count taken from the maximum is the same species of error as the "28" it was
replacing, one turn later, in a message correcting that error. The implementer
counted instead of accepting, and says thirty-four while naming the gap. · (b)
It found two further unreconciled counts in the retrospective that were in
nobody's finding list: "five of the seven … only two needed a reviewer" is
**four and three** when enumerated against this ledger (code or probe: #6, #7
R2, #7 R3, #17; reviewer: #8, #9, #23), and a "that is now four times" claim
contradicted itself and could not be enumerated from what it held, so it now
cites #18a rather than restating a count as its own. · **(c) The finding worth
keeping is its third concern, and it goes beyond this campaign.** CLAUDE.md
says the committed ledger's path is "keyed by campaign slug, touched by exactly
one campaign, ever, **so there is no collision to have**". That is true across
campaigns and false within one: The Cartulary moved the ledger out of
per-worktree scratch precisely so it could be *shared*, and sharing among a
campaign's own agents is exactly what produced #38's eight duplicate numbers.
The claim is not wrong so much as scoped to the collision it was designed
against, and silent about the one it introduced. That is a note owed to
`docs/CLAUDE.md` or a decision record, not just to this campaign's retro — it
will recur in every campaign that lets implementers write rulings, which the
autopilot skill instructs them to do. · ideonomy passes / overturns: 0.

---

#40 [G5] — **#39 contained two defects of the exact class it was recording, and
a reviewer counting rather than reading found both.** · (a) I wrote that the
retrospective "now cites #35". It cites **#18a**, which is what the
implementer's own report said. I appear to have reached for a number in the
range I had just renumbered rather than reading the one in front of me. An
entry whose subject is *counts stated without being counted* asserted a
citation without checking it. · (b) **#39 falsified the retrospective's
freshly-corrected entry count by existing.** The retrospective said
"thirty-four entries, numbered to #38"; true when written, false the instant
#39 appended. · **The remedy is structural, not another correction.** A prose
total over an append-only document that the prose lives inside is
self-falsifying: every entry that states it is wrong by the next entry, and
this is now the *third* value that sentence has held (28 → 34 → 35). So the
retrospective stops stating a total at all and names the command that counts
it — the same move the census-cost block in CLAUDE.md makes for a figure that
moves faster than its prose ("read it from `docs/timings.md`, never from this
block"), and for the identical reason. A pointer cannot go stale; a number in
the wrong document always will. · **This is the campaign's own thesis closing
on itself**, and it belongs in the retrospective in exactly those terms: I
argued in §1 that per-step prose repeats a fact N times and should be stated
once where it can be resolved, then spent four rounds restating a count in
prose that had a command behind it the whole time. · ideonomy passes /
overturns: 0.

---

#41 [G5] — **My prediction about the self-falsifying count was itself
outrun before the fix could be written.** · I told the implementer the ledger
was at "35 now, 36 after this round". It ran the counting command first and
found **36 already** — my number was stale between composing the message and
the implementer reading it. That is the fourth value in this thread (28 → 34 →
35 → 36) and the cleanest possible demonstration that the fix had to be
structural: I could not state this integer correctly even in the act of
explaining why it could not be stated. · Three further things it did that are
worth keeping: (a) it **verified the counting command before adopting it**,
because a command that over-counts relocates the defect rather than closing it
— the anchored `grep -cE '^#[0-9]+ \['` returns 36 while the naive
`^#[0-9]+` returns 38, matching two cross-references inside entry bodies, and
the retrospective now names that trap so a reader substituting the obvious
regex does not get a wrong number; (b) its own replacement paragraph contained
the same error class and it caught it pre-commit — it had written "three
different totals" while naming two; (c) adding the first-half rate falsified
the paragraph *below* it ("a reader arriving at **that number**", singular),
which it also fixed. · **Every one of those is the campaign's thesis
recurring**: a number stated in prose acquires dependents, and each dependent
is a place the correction has to reach. That is precisely the argument spec §1
makes about a provenance string repeated at every step, and it is why the
remedy in both cases is to state the thing once, where it can be resolved,
rather than to state it accurately more often. · ideonomy passes / overturns: 0.
