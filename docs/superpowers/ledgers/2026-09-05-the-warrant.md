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
