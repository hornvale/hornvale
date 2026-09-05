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
