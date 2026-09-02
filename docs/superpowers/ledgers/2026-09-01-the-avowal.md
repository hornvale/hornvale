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

---

#6 [G5] — **Task 1: the promoted-forebear yield probe — kill criterion for
spec §4.3.**

*Question.* Spec §5: "if the median [promoted-forebear] yield is under 10%,
§4.3 does not ship." Measured on one seed only (42, from the committed
fixture): 45.6%. Does a 25-seed panel confirm the world, not the one seed, is
where that margin lives?

*Instrument.* `windows/worldgen/tests/suite/promoted_forebear_yield.rs`
(`#[ignore = "calibration: run by hand, prints the promoted-forebear yield
panel"]`), tagged `claim: sanctioned-sweep(...)` per `cli/tests/suite/
claim_shape.rs`'s default-deny lint. Built through `hornvale_worldgen::
{select_founders, forebear_of, founder_of}` — the same public API the spec
names for §4.3 — never through `Founder::handle`, which lives in a different,
seed-independent handle space (`founder_handle`'s discrimination fold vs.
`founder_of`'s seed XOR; see the test file's module doc for the full trap).
`yield = (promoted founders whose forebear is ALSO promoted) / (all promoted
founders)`; roots count against the denominator, per the brief.

*Positive control (spec §5 step 3).* Before trusting a high reading, the
probe was proven able to report a low one:
`positive_control_a_capped_people_yields_near_zero` (not `#[ignore]`d — cheap,
no world build) constructs one people, 25 hand-committed occupations: 5
low-`peak_population` "parents" and 20 higher-`peak_population` "children"
each naming a parent as `occ-founded-from`. `MEMORY_DEPTH` (20) promotes
exactly the 20 children and drops all 5 parents, so every promoted founder's
forebear exists but is never promoted — yield is **exactly 0%** by
construction, and the test asserts it. Confirms the instrument moves.

*Panel result (25 seeds: 42 + 1..=24; `#[ignore]`d run, `--release`,
`--nocapture`, 63.9 s wall):*

```
  seed  promoted  fb_promoted  unpromoted_fb  roots  yield_all%  yield_excl_roots%
  1        234       122            73          39      52.1        62.6
  2        230        89           109          32      38.7        44.9
  3        187        82            67          38      43.9        55.0
  4        194        82            73          39      42.3        52.9
  5        130        52            32          46      40.0        61.9
  6        198        89            62          47      44.9        58.9
  7        203       114            46          43      56.2        71.2
  8        111        64             8          39      57.7        88.9
  9        275       212            12          51      77.1        94.6
  10       203       106            55          42      52.2        65.8
  11       229       100            95          34      43.7        51.3
  12       244       118            99          27      48.4        54.4
  13       161        64            54          43      39.8        54.2
  14       238       105            96          37      44.1        52.2
  15       120        53            24          43      44.2        68.8
  16       240       132            57          51      55.0        69.8
  17       265       149            71          45      56.2        67.7
  18       241       103           110          28      42.7        48.4
  19       196       144             9          43      73.5        94.1
  20       234       181             9          44      77.4        95.3
  21       147        65            45          37      44.2        59.1
  22       266       156            69          41      58.6        69.3
  23       192        86            64          42      44.8        57.3
  24       184        74            72          38      40.2        50.7
  42       204        95            74          35      46.6        56.2

  pooled promoted-ancestor chain depth: {0: 2489, 1: 1130, 2: 656, 3: 367,
    4: 238, 5: 121, 6: 61, 7: 24, 8: 18, 9: 14, 10: 2, 11: 1, 12: 1, 13: 1,
    14: 1, 15: 1, 16: 1}

  median yield (all promoted founders, THE KILL CRITERION): 44.9%
  median yield (excluding roots, not the kill criterion):   59.1%
```

Every seed in the panel individually clears 38.7%; the lowest reading is
nearly 4x the 10% floor. Seed 42's own live reading (46.6%) is close to, but
not identical to, the brief's fixture-derived reference (45.6%) — see the
finding below for why, and why it does not matter here.

*Decision.* **PASS. Median yield (all promoted founders) = 44.9%, well above
the 10% kill criterion. §4.3 (`parent-of`/`kin-of`) ships.** Seed 42 is not
unrepresentative — every other panel seed reads in the same 39-77% band.

*Finding: `founder_of`'s handle space collides, ~3.5% of occupations at seed
42 (not a defect in this probe, a property of the descent module).* Cross-
checked ad hoc (not committed): `founder_of` maps seed 42's 1,212 occupations
onto only 1,169 distinct `RoleHandle`s — 42 colliding groups. `founder_of` is
exactly `founding_key_from(...) ^ seed`, with none of `founder_handle`'s extra
discrimination fold (`ended`, `peak_population`, a role tag) — the very fold
`select_founders` needed because the identity key alone is known to collide
(The Salt's own 8.4%/3.3%/3.6% *stem*-collision figures at seeds 42/7/1000 are
this same phenomenon's name-rendering symptom). A second, entity-identity-only
cross-check (reading `occ-founded-from` directly and testing occupation-
`EntityId` membership, bypassing `forebear_of`/`RoleHandle` entirely) measured
seed 42 at 93/76/35/204 — **exactly** the spec §4.3 reference figure — against
this probe's handle-based 95/74/35/204: two promoted founders whose forebear's
handle happens to collide with an unrelated promoted occupation's handle, read
as "forebear promoted" when the entity forebear was not. Both readings clear
the kill criterion by a wide margin — irrelevant to *this* verdict — but it
bears directly on Task 5: spec §4.3 requires `parent-of` be `functional` "if
and only if a founder can have at most one recorded forebear," and a handle
collision can make one `RoleHandle` resolve to more than one promoted person.
If Task 5 commits `parent-of` by matching `forebear_of`'s returned handle
against promoted founders (the only forebear-identification path the public
API offers), it inherits this ~1-in-100 misattribution risk as-is, or must add
a discrimination fold to `founder_of`/`forebear_of` itself (a `descent.rs`
change, out of Task 1's scope). **Flagged for Task 5's implementer to decide,
not resolved here.**

*Alternatives discarded.* Fixing the `founder_of` collision inside Task 1 —
out of scope (measurement only, no production code; the brief and spec both
scope Task 1 to a probe). Silently reporting only the handle-based number
without the entity-based cross-check — would have hidden a real,
forward-relevant defect behind a coincidentally tiny effect size.

*Ideonomy passes / overturns.* None — a measurement task, not a design
question.

*Capture actions.* This entry; the probe file's own module doc carries the
same finding for a reader who never opens the ledger. Task 5's implementer
should read the "finding" paragraph above before choosing how `parent-of` is
committed.
