# The Avowal — decision ledger

Campaign: **The Avowal** — giving the ledger vocabulary for capabilities the
world already computes. Branch `campaign/the-avowal`, based on `18f63ebfa`.

Autopilot is engaged (G3 and G6 are the hard stops).

---

#1 [G1] — **Which campaign, and on what evidence.**

*Question.* What is the highest-impact next campaign?

*Decision.* The Avowal: close the gap between what the world computes and what
its ledger can say, measured by trope stageability.

*Why.* Measured, not inferred. `wolverson-2021` reads 27 present / 5 refused /
26 deferred / 11 absent of 74 — the *program* is built. `polti-1895` reads
**0 of 36 stageable** and `tvtropes-2012` reads **0 of 409**. Greedy leverage
over the live 395-token registry: 38 predicates across 15 bundles takes
tvtropes from 0 to 140/409 and Polti from 0 to 6/36; the curve is convex, with
the first Polti situation arriving only at bundle 9.

*Alternatives discarded.* (a) Continue the language thread (`the-ladder` reads
1 of 214) — real, but The Rail froze the instrument for it and the frontier is
5 rungs wide; it is well-served and does not need this campaign. (b) Push the
systems corpus from 27/74 — the 26 `deferred` items are deferred on cost
rulings already taken, so the headroom is smaller than the number suggests.

*Ideonomy passes / overturns.* 1 pass (operators: combination,
organon-construction; organon: notation; dimension-prompts: connectivity,
direction, naturalness). **No reversal; one material sharpening.** Expressing
the resolver as a notation exposed three slots it has no term for —
`token |- producer`, `token |- witness`, `situation |- instance`. The
`direction` prompt found the reason: registry membership is append-only, so
the metric is **monotone by construction** and cannot detect a registered
predicate that nothing produces. That moved the campaign from "register the
bundles" to "witness first, vocabulary second." The `naturalness` prompt
surfaced the authored-vs-derived fidelity axis, escalated as #2 below. Crossing
bundles against domains found the campaign's actual prize: bundles whose
**producer already exists and whose ledger vocabulary does not**.

*Capture actions.* Findings that grounded this are recorded in #3 below rather
than left in conversation.

---

#2 [Q] — **The grain of an affect fact.** (Carve-out: fidelity — hard stop,
answered by Nathan directly.)

*Question.* `windows/sentiment::snap_judgment` takes two `PeopleTraits` and is
therefore **people-to-people**. Polti's `feels-toward` is **person-to-person**.
Commit at the grain that exists, or the grain the corpus asks for?

*Decision (Nathan).* **Both, as two predicates.** A people-scale predicate
ships this campaign; a person-scale `feels-toward` is defined but the corpus
resolves only against it, so **the trope number does not move until the real
grain lands**.

*Why.* Committing people-scale affect under the corpus's own token would move
the headline number on a grain the corpus did not ask for — the quiet
mis-scoring decision 0136 exists to prevent. Two predicates keep the shipped
capability honest and the instrument unflattered.

*Consequence, and it is load-bearing.* This ruling **only holds if the
realization witness ships**. Under today's resolver
(`cli/src/tropes.rs::resolve`, which tests registry membership and nothing
else) merely registering a person-scale `feels-toward` moves the number with
no producer behind it. The witness is therefore a prerequisite of this ruling,
not a tidiness item.

*Alternatives discarded.* People-scale under the corpus's own token (moves the
number dishonestly); person-scale built this campaign (needs a per-person
appraisal fold that does not exist; displaces a bundle from scope); defer
affect entirely (leaves Polti's #2 bundle, 16/36, untouched when its producer
already exists).

*Ideonomy passes / overturns.* Covered by #1's pass, whose `naturalness`
prompt raised the authored-vs-derived axis this question sits on. No separate
pass; Nathan ruled directly, as a fidelity carve-out requires.

*Capture actions.* Person-scale `feels-toward` to be registered as an idea-
registry row at spec time.

---

#3 [G2] — **Four facts established by command, not inference.**

Recorded here because each is load-bearing for the spec and each was verified
by running something. Backfilled at brainstorm time; no ideonomy pass applies.

1. **The resolver tests membership only.** `cli/src/tropes.rs::resolve` builds
   `held` from `registry_tokens()` and reports a situation stageable when its
   expanded tokens are all present. No producer, witness or instance is
   consulted. Decision 0330 ruled this exact hazard on the *sentence* corpus
   ("a token added on optimism moves the score without moving the grammar")
   and the trope corpus has no equivalent witness.
2. **Kinship is computed and unspoken.** `domains/history/src/descent.rs`
   exposes `Kinship`, `remove()`, `kinship()`, `ancestor()`, with tests. It
   commits **zero** facts (no `predicate` reference in the file). The registry
   holds `concept:parent`, `concept:child`, `concept:sibling` and **no kin
   predicate**, so `bundle:consanguineal-kin` scores blocked across 12 Polti
   and 50 tvtropes situations on machinery that already runs.
3. **Affect is computed and unspoken.** `windows/sentiment` exposes
   `Judgment { warmth, competence, emotion }` and `snap_judgment(judger,
   target)`. `grep -rn 'Fact\|predicate\|commit' windows/sentiment/src/*.rs`
   returns nothing. This corrects the brainstorm's own first estimate, which
   had `felt-affect` as speculative and needing an appraisal model.
4. **The envelope is binary; two of the demanded bundles are not.**
   `kernel/src/ledger.rs::Fact` is `subject / predicate / object(Value) /
   place / day / provenance`, and `Value` is `Entity | Text | Number | Flag`.
   A qualified relation (affect: a kind *and* an intensity on one ordered
   pair) and an addressable act (`witnessed`, `act-precedes`, `deed-of`) have
   no direct expression. Precedent for both is reification: an occupation is
   an entity tagged `is-occupation` carrying ~11 `occ-*` attribute facts.
   Measured in `cli/tests/fixtures/world-seed-42.json`: 21,635 facts, 118
   distinct predicates, **1,212 occupation entities** whose facts are ~60% of
   the whole ledger. Reification is already this ledger's dominant shape.

---

#4 [Q] — **Does an act become addressable?** (Carve-out: scale/fidelity —
MAP-20's "drawn once, deliberately, as a spec question". Answered by Nathan.)

*Question.* `witnessed`, `act-precedes`, `deed-of` and `act-occurred-on` all
presuppose an act has identity, and no `EventId` exists in the tree. Reify
every act, reify only remembered ones, or leave acts implicit?

*Decision (Nathan).* **Reify every act — and do not commit them.** "We
shouldn't need to commit the facts though, except in snapshotting/incremental
persistence."

*Why, and the error it corrected.* The controller had costed reification as if
addressability implied storage, and recommended against it on that basis. It
does not: 0366 makes passage state "a pure function of the seed and the
committed ledger, and only the CHANGE is written", 0346 makes an affordance
derived and never committed, and 0368 makes live-play facts persist only when
asked for. An act therefore gets a derived identity — an `ActHandle`, the same
shape as `RoleHandle` and `barrier_of` — at no ledger cost. The recommendation
against C2 rested on a cost that was never real.

*Alternatives discarded.* Leave acts implicit (a permanent ceiling, not a
delay — two bundles blocked forever); reify only remembered acts (the
promotion precedent, but an unnecessary restriction once storage is not the
cost).

*Ideonomy passes / overturns.* Covered by #1's pass. **This is the campaign's
first genuine overturn** — the controller's own recommendation was reversed by
Nathan on a decision-log fact the controller had not checked.

*Capture actions.* Spec §4.5; decision 0580.

---

#5 [G2] — **The audit reads one of three homes, and that is the campaign's
real subject.**

*Question.* Chasing #4, the controller over-corrected — concluding that
kinship and affect should also be derived rather than committed, which would
have emptied the campaign. Where is the line?

*Decision.* Three homes, three answers, established by reading the code rather
than by reasoning from the principle:

- **the ledger** (`EntityId`-keyed, saved) — kinship. 0366 and 0368 govern
  *play-time* change; the genesis bake's job is to commit its derivations,
  which is why `is-person`/`person-born` are facts at all.
- **the component layer** (`KindId`-keyed, build-state, never saved) — affect.
  `snap_judgment` is kind x kind and world-invariant; committing it would
  store ~840 facts identical in every world ever generated.
- **session state** (`EntityId`-keyed, persists only when asked) — acts.

*Why this is the finding.* `registry_tokens()` reads the `ConceptRegistry`
alone, so **a capability scores absent whenever it lives anywhere but the
ledger** — which under 0001/0346/0366 is most of this architecture. Left
unfixed the instrument rewards committing what those decisions say to derive.
The fix is a capability manifest spanning all three homes, not a new place to
put data.

*Verified, not inferred.* `World` is `{ seed, registry, ledger,
stream_versions }` — `WorldComponents` is not in it. Every `WorldComponents`
field is `ComponentStore<KindId, _>`; `grep -rn 'ComponentStore<EntityId'`
returns nothing. The registry already names one non-fact category
(`phenomenon_kinds`, used by `bundle:celestial-portent`), so a registry token
need not correspond to something stored — but no precedent exists for a
predicate naming a purely derived relation.

*Alternatives discarded.* A new registry storage category (invents a home when
three exist); commit affect at genesis anyway (the duplication 0366 objects
to); leave affect unnamed (leaves the blindness unaddressed).

*Ideonomy passes / overturns.* Covered by #1's pass, whose notation organon
named the missing `token |- producer` slot this entry fills. One
self-correction, recorded above rather than quietly dropped.

*Capture actions.* Spec §2.1, §4.1; decisions 0576, 0579.
