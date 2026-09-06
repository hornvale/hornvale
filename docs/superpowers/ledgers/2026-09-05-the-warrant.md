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

#11 [G5] — **Task 2 complete: the errand-boundary commit, and a cross-tick
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

#12 [G5] — **Fourteen liveness.rs unit tests moved, all legitimately.** ·
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

#13 [G5] — **H3 (spec §10) measured; no STOP triggered.**
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

#14 [G5] — **Step 5 fixture: `tests/fixtures/the-warrant-glosses.json`,
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
