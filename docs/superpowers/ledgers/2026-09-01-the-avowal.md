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

---

#7 [G5] — **Task 3: reuse `Correspondent<T, V>` or define a sibling for the
provision table?**

*Question.* Spec §4.1 says `kernel/src/manifest.rs`'s `Correspondent<T, V>` —
`Present(payload) | Absent(reason)`, where an absence must name why — is
"exactly the discipline this table needs," and asks the implementer to decide
whether to reuse it or re-derive a sibling type, and to record the choice.

*Decision.* **Reuse `Correspondent<T, V>` generically; define a project-local
`V`.** `Provision`'s rows are `BTreeMap<String, Correspondent<Home, Unserved>>`
— `hornvale_kernel::Correspondent` used as-is (it is already re-exported from
the kernel root, so reuse costs nothing across the `kernel/` → `cli/`
layering), with a new `cli::provision::Unserved` enum standing in for
`Manifest`'s `Void`.

*Why not `Void` itself.* Confirmed by reading `manifest.rs` in full: `Void`'s
four variants (`Unnamed`, `Gap`, `Imperceptible`, `Uncognized { pending_wave
}`) all name lexicon/perception/cognition reasons — none describes "no
storage home serves this token," which is what a `Provision` absence means.
Reusing `Void` would either force a token's reason into a vocabulary it does
not fit, or add provision-shaped variants to a type `manifest.rs` owns for a
different correspondence (concept ↔ lexicon/perception/cognition), coupling
two unrelated tables' vocabularies. `Correspondent<T, V>` itself carries no
such coupling — it is already generic over the reason type, which is the
whole point of parameterizing it — so reusing the *shape* while defining a
fresh *reason type* is not a compromise between the two options; it is what
the generic was built to let a second caller do.

*What `Unserved` had to guarantee, and how.* Spec's Step 2 test 4 requires
"a reasonless absence is a construction error." `Unserved` has exactly one
variant, `NotServed(&'static str)`, whose field is mandatory — `Correspondent
::Absent(Unserved::NotServed())` does not compile (missing argument), the
same guarantee `Void`'s mandatory-data variants give `Manifest`. A
`compile_fail` doctest on `Unserved` (`cli/src/provision.rs`) pins this the
same way `manifest.rs`'s PROC-13 exhibit pins `Void`.

*What "unreachable by construction" for the component/session homes turned
out to mean, concretely.* Not "no rows exist for them" (a fact about this
task's data, easy to violate later by accident) but a type-level guarantee:
`Home::Component`/`Home::Session` each carry an `Unwired` payload, and
`Unwired` is an empty enum (`pub enum Unwired {}`) — no value of it can ever
exist, so neither variant can be constructed by any code, this task's or a
future one's, until Task 6/7 replace `Unwired` with a real payload type at
the call site. `Provision::serves`'s match arms for those two variants are
therefore genuinely unreachable (`match *unwired {}`), not merely undialled.

*Alternatives discarded.* Extending `Void` with a fifth variant naming a
storage-home reason (couples `manifest.rs`'s concept-correspondence
vocabulary to `provision.rs`'s capability-home vocabulary — two different
questions that happen to share a shape); a bespoke `Present`/`Absent` enum
re-deriving `Correspondent`'s two variants from scratch (pure duplication of
a type built to be reused this way, and the reviewer would have to re-verify
by hand that it carries the same discipline).

*Ideonomy passes / overturns.* None — a reuse-or-re-derive call answered by
reading the one file the spec pointed at, not a design question.

*Capture actions.* `cli/src/provision.rs` (`Home`, `Unwired`, `Unserved`,
`Provision`); `cli/src/tropes.rs::resolve` now consults `Provision` instead
of `registry_tokens` directly. `docs/audits/trope-*.md` confirmed unchanged
after `make rebaseline` — this task rewires how `resolve` looks a token up
without moving any verdict (see `cli/tests/suite/provision.rs`'s
`ledger_home_still_matches_committed_reports_for_both_corpora`, which fails
loudly on either committed report if one moved).

---

#8 [G5] — **Task 4: what counts as "the actants fail to stage" vs "a
relation fails to resolve," and how a witness failure renders in `Outcome`.**

*Question.* The brief's step 2 asks for two DISTINCT failing-witness tests
("a witness whose tableau fails to stage the actants is refused" and "a
witness whose relations do not all resolve is refused") without prescribing
what makes them distinct, and does not say whether a witness failure is a
new `Outcome` variant or folds into the existing `Blocked` shape.

*Decision.* **Two failure modes, one shape.** "Fails to stage the actants"
is a `StagedThing` naming a `held_by` cast index nobody placed — the prop
cannot be put in anyone's hands, so the whole tableau is refused before any
fact is committed (`Session::start`'s `"tableau stages a {kind} held by cast
member {N}, but the cast has {M} member(s)"` path). "Relations do not all
resolve" is a `StagedRelation` naming a predicate the concept registry does
not hold — the cast stages fine, and `Ledger::check`'s `UnknownPredicate` is
what refuses (`Session::start`'s `"staging a {predicate} relation: {e:?}"`
path). Both are genuinely different code paths inside `Session::start`,
confirmed by `cli/tests/suite/trope_witness.rs`'s
`a_witness_whose_tableau_fails_to_stage_the_actants_is_refused` (an
out-of-range `with_thing` holder) and
`a_witness_whose_relations_do_not_all_resolve_is_refused` (an unregistered
relation predicate) exercising them separately. A witness failure folds into
the EXISTING `Outcome::Blocked(Vec<String>)` — no new variant — using two
sentinel strings, `"witness:absent"` (no row in the table) and
`"witness:refused"` (a row exists but `Session::start` errored), rather than
propagating the raw `VesselError` text: every `Outcome` consumer already
treats `Blocked`'s vector as "here are the reasons," and a witness failure
is exactly that kind of reason, not a new kind of verdict — see decision
0577.

*Why sentinels rather than the raw error message.* The report is a
byte-ratcheted artifact. `VesselError`'s `Display` text is not itself
part of any save-format or determinism contract, so pinning it verbatim into
a committed report would make a future, behavior-preserving refactor of
`windows/vessel`'s error wording a silent report diff. A fixed two-value
sentinel is deliberately coarser and stable across such refactors; since the
witness branch is unreached by both frozen corpora today (§4.2's zero
migration cost), this cost nothing to get right immediately rather than
patching it after a report drift someone had to explain.

*Order of the AND.* The witness check runs only once the token check's
`missing` list is empty — never unconditionally. Confirmed by inspection
(`resolve`'s `if !missing.is_empty() { Blocked(missing) } else {
witness_stages(...) }`) and by the fact that `make rebaseline` produced a
byte-identical demand table for both corpora: if the witness check ran
first, or independently, a corpus whose tokens are missing WOULD ALSO gain
a `"witness:absent"` entry in its `missing` list the moment this task
landed, which the report ratchet would have caught as a verdict change and
did not.

*Step 1's captured red.* Ran `cli/tests/suite/trope_witness.rs`'s
`a_situation_with_no_witness_cannot_be_stageable` against `tropes::resolve`'s
UNMODIFIED two-argument signature (before `cli/src/tropes.rs` was touched at
all): a one-situation corpus with an empty `requires` list resolved
`Stageable` unconditionally, since no witness concept existed yet to refuse
it. `assert_ne!(out.get("s1"), Some(&Outcome::Stageable), ...)` failed with
`left: Some(Stageable), right: Some(Stageable)` — a genuine behavioural red
on the live surface, not a compile error standing in for one.

*Step 5's mutation.* Hypothesized a naive-but-plausible implementation bug:
`witness_stages` checking only that the witness TABLE contains an entry for
the situation id, never actually attempting `Session::start` — "an
implementation that satisfies every assertion while not exercising the
staging machinery a witnessed situation is credited for." Applied via
`scripts/mutate.py` (replacing the function body with
`witnesses.get(id).map(|_| ()).ok_or("witness:absent")`), ran the suite:
three tests went genuinely red (`a_witness_whose_tableau_fails_to_stage_the_
actants_is_refused`, `a_witness_whose_relations_do_not_all_resolve_is_
refused`, `witness_stages_reports_refused_for_a_witness_that_fails_to_
stage`), each a real assertion failure (`left: Ok(()), right: Err
("witness:refused")` and the corresponding `Stageable` in place of
`Blocked`), not a compile error. Restored with `git checkout -- cli/src/
tropes.rs` and confirmed by md5 (`a1dc2ed0c70225f7a031d23032d98778` both
before and after) — **note for a future reader of this ledger: `git
checkout` restores to the last COMMIT, which discarded this task's entire
uncommitted implementation the first time this was tried, not merely the
mutation; the implementation had to be re-applied from scratch before the
checksum could be re-verified. A scratch-copy-and-restore (or `mutate.py
--to`) is the safer sequence when the file under mutation carries
uncommitted work, and this ledger entry exists partly so the next session
does not repeat it.**

*Alternatives discarded.* A new `Outcome::WitnessMissing`/`WitnessRefused`
variant — rejected because every consumer (`render`, `render_matrix`,
`tally`, the per-situation report row) already branches on `Blocked` as "a
list of reasons," and a fourth variant would need its own match arm
everywhere `Outcome` is matched for a distinction no consumer currently
needs to make differently. Embedding the raw `VesselError` text in the
`Blocked` reason — rejected per the sentinel-vs-raw-text reasoning above.
Checking the witness unconditionally (before or independent of the token
check) — rejected because it would have moved the report for both corpora
today (every blocked situation would gain a spurious `"witness:absent"`
entry), directly contradicting spec §4.2's "migration cost is zero."

*Ideonomy passes / overturns.* None — the brief's two failure-mode tests
admit more than one code-level distinction; the one chosen is the one the
staging code already draws for unrelated reasons (things vs. relations are
handled by separate blocks in `Session::start_held`), so no invention was
needed, only recognition.

*Capture actions.* `cli/src/tropes.rs` (`Witnesses`, `witnesses`,
`witness_stages`, `resolve`); `cli/tests/suite/trope_witness.rs`;
`docs/decisions/0577-the-realization-witness.md`; no situation's `Outcome`
variant changed on either frozen corpus, confirmed by `make rebaseline`
producing a byte-identical demand table (only report header prose moved).

---

#9 [G5] — **Task 4 review round 1: five fixes, and a decision-editing
mistake caught and reversed mid-round.**

*What happened.* The first review of Task 4 returned four Important
findings (F1-F4) plus two minor ones (F5-F6). F1: `witness_stages` looked a
tableau up by situation id alone, so a tableau relating two goblins by
`instance-of` witnessed any situation it happened to be filed under — fixed
with `witness_binds`, requiring every relation predicate a witness states
to be among the tokens its situation's own `requires` names. F2: the
committed report headers claimed more than the code checked — rewritten
into shared constants (`WITNESS_BOUNDARY_WHAT`/`COMPARABILITY`) so `render`
and `render_matrix` cannot state the claim two different ways. F3: a
witness-blocked `Blocked` reason rendered as a missing corpus token, and
the Leverage section's `closest`/`blocked` figures would have printed a
self-contradicting "still missing 0 bundles" the moment a witness-blocked
situation existed — both fixed, with the pre-existing tvtropes-2012
pluralization defect ("missing 1 bundles") confirmed unchanged rather than
fixed (out of scope, per instruction). F4: `Witnesses` stored only the
tableau, never decision 0330's mandatory distance record — `Witnesses` is
now `BTreeMap<String, WitnessEntry>` with a `realized: String` field
`WitnessEntry::new` refuses to leave empty. F5: decision 0577 and a doc
comment both claimed `Provision` is "supplied by the caller, never rebuilt
inside" — backwards; `resolve` builds it itself every call. F6: the report
overstated the new test count (said 14, meant 10).

*The mistake.* Fixing F5 by editing decision 0577's body in place, with an
inline "this record was amended" paragraph explaining why. `docs/CLAUDE.md`
states plainly: "Decisions are append-only. Never edit a ratified decision's
substance; supersede it with a new record" — with no carve-out for
"not yet merged to `main`". Caught on a self-review pass before reporting
back: 0577 was restored to its exact original text (verified against `git
show <round-1-commit>:docs/decisions/0577-the-realization-witness.md`) with
only its **Status** line changed to `Superseded by 0581`, and the corrected
design was written as a fresh, freestanding decision, 0581, following the
project's own `0014`→`0126` precedent exactly (a full restatement, not a
diff against the superseded record).

*Decision-number provenance.* 0581 is free inside this campaign's already-
reserved `0576`-`0585` block (spec §header) — 0578/0579/0580 are reserved
for Tasks 5/6/7's Kinship/Affect/Acts, confirmed by grepping the spec for
every `decision 057*` mention before claiming the number, per the standing
"decision numbers are reserved in blocks" rule.

*Verification.* Zero verdicts moved on either frozen corpus (still 0/36,
0/409) — confirmed by regenerating both reports and the matrix and diffing
against the previously committed artifacts before committing: every
per-situation row and the fan-in table are byte-identical; only header
prose changed. Full workspace: `HV_TEST_OK=1 cargo nextest run --workspace
--no-fail-fast` → 4953 passed, 0 failed, 211 skipped; doctests all green.
A side effect of the fix diff itself (a new doc line using "cell" in the
Markdown-table-cell, AREA sense) tripped `lexicon_guard::no_vertex_sense_
cell_comes_back` — waived inline with a reason, confirmed the count returns
to its pre-fix value.

*This entry itself is late.* Both decision 0577 and decision 0581's "See
also" sections cite "ledger entries #8 and #9" — #9 did not exist until
this entry was written, during round 2's own self-review. The citation was
written on the assumption a ledger entry would follow the same commit; it
did not, and nothing mechanical checks a ledger-entry-number citation
inside a decision record's prose (`docs_consistency` validates decision
NUMBER cites against `docs/decisions/`, not ledger-entry cites against this
file), so the gap was silent until re-read. Recorded here, in place, rather
than silently back-dated.

*Alternatives discarded.* Leaving 0577 edited in place with the amendment
paragraph, treating "not yet merged" as an implicit exception to
append-only — rejected on a second reading of the rule, which states no
such exception, and because the whole point of append-only is that a reader
should never have to ask whether an edit happened before or after some
merge boundary to trust a record's history.

*Ideonomy passes / overturns.* None — a review-response round, not a design
question.

*Capture actions.* `cli/src/tropes.rs` (`witness_binds`, `WitnessEntry`,
the rendering and Leverage fixes); `cli/tests/suite/trope_witness.rs`;
`docs/decisions/0577-the-realization-witness.md` (restored, marked
superseded); `docs/decisions/0581-the-witness-is-bound-to-its-situation.md`
(new); this entry, written late.

---

#10 [G5] — **Task 4 review round 2: the binding is bidirectional.**

*What happened.* The second review reopened F1 and F2 with a probe, not a
re-read: `Tableau::new().with_cast(["goblin","drow"])` — two creatures,
ZERO relations — filed under a situation requiring FIVE predicate tokens it
never touches, resolved `Stageable`, `witness_stages` returned `Ok(())`.
Round 1's `witness_binds` was `tableau.relations.iter().all(|rel|
required.contains(...))`, one direction only; `all()` over an empty
iterator is `true` whatever `required` holds, so the disclosed limit
("binds vacuously to any situation whose requirements name no predicate
token") was FALSE — the hole was universal, not confined to
no-predicate situations, and a tableau covering only SOME of a situation's
required predicates was an undisclosed sibling of the same gap.

*The fix.* `witness_binds` now requires SET EQUALITY between the
situation's required `predicate:` tokens and the predicates the tableau
states as relations (both directions at once), scoped to `predicate:`
tokens only since `concept:`/`phenomenon:` tokens cannot be stated by a
`StagedRelation` at all. This closes the empty case and the partial-
coverage family in one move, and makes `WITNESS_BOUNDARY_WHAT`'s claim true
by construction rather than requiring the prose to be narrowed to match a
thin check — the ruling the coordinator gave explicitly, and the one this
entry's decision (0582) follows.

*Verification the fix actually closes the reopened hole.* Two new tests —
`a_relation_less_tableau_does_not_bind_to_a_situation_requiring_predicates`
(the review's exact probe) and `a_tableau_covering_only_some_required_
predicates_is_unbound` (the partial-coverage sibling) — were run against
round 1's ONE-DIRECTIONAL `witness_binds` via `scripts/mutate.py` before
being accepted as passing: both went genuinely red (`Some(Stageable)`
where `Some(Blocked(["witness:unbound"]))` was expected), proving the
tests actually exercise the gap the review found rather than merely
asserting the new behavior in the abstract. Restored and reverified by
md5 (`0d9c5c8cc84a06261619667d7f83b60b` both before the mutation and after
restoration) — the SAME `git checkout --` trap entry #9 already named bit
again here (it discards uncommitted work, not just the mutation), so the
whole round-2 diff to `cli/src/tropes.rs` had to be reapplied from the
saved python scripts rather than merely reverting a small edit; noted so a
third occurrence does not recur.

*F3's three residual edges, closed in the same pass.* `closest`'s
`unwrap_or(0)` reintroduced the corrupted "still missing 0 bundles"
sentence in a narrower window (every blocked situation witness-blocked);
now kept as `Option<usize>` with an explicit `None` sentence. `blocked`
(the Leverage denominator) and the Demand table's total disagreed with no
reconciliation once witness-blocked situations existed to exclude; the
paragraph now states `witness_blocked` and reconciles all four counts
(`stageable + inapplicable + blocked + witness_blocked = out.len()`), the
same discipline the `inapplicable` disclosure already used.
`blocked_by_witness`'s doc claimed a collision-proof guarantee `expand`
does not actually provide (`expand`'s `None` arm returns any non-`bundle:`
token unchanged, unvalidated) — corrected to state it as true-by-inspection
for the two frozen corpora today, not a guarantee.

*N1: a drift-checked artifact the previous round missed.*
`docs/digest/decisions-in-force.md` was stale — decision 0581 never
appeared in it, because nothing in `docs_consistency` checks that file
against `docs/decisions/`; it surfaces only at the `digest render
decisions` regenerate-and-diff step, which round 1 never ran. Regenerated
here (`cargo run --manifest-path tools/digest/Cargo.toml -- render
decisions > docs/digest/decisions-in-force.md`); now lists `0576` and
`0582` as in force, correctly omitting `0577` and `0581` as superseded.
`docs/digest/intent-vs-reality.md` was also regenerated and found
unchanged.

*N2.* `describe_witness_reason`'s `_` catch-all now panics on an
unrecognized sentinel instead of silently describing it as
`"the registered witness failed to stage"` — a `&str` match cannot be
exhaustive in the type-checked sense, so this converts the "silently wrong"
failure mode the reviewer named into a loud one.

*Alternatives discarded.* The reviewer's own cheaper suggestion
(`!tableau.relations.is_empty()`) — explicitly rejected by the reviewer's
own ruling before this round began ("close it with the bidirectional
binding, not the cheap guard"), since it would have caught only the
zero-relation case and left the partial-coverage family (a tableau covering
some but not all required predicates) open. Requiring the FULL required-
token set (including `concept:`/`phenomenon:`) to equal the staged-relation
set — rejected because neither token kind can be stated by a
`StagedRelation` at all, which would make every situation requiring one
permanently unwitnessable.

*Ideonomy passes / overturns.* None — a review-response round.

*Capture actions.* `cli/src/tropes.rs` (`witness_binds`,
`blocked_by_witness`, `describe_witness_reason`, the Leverage section);
`cli/tests/suite/trope_witness.rs` (two new tests, both mutation-verified);
`docs/decisions/0581-the-witness-is-bound-to-its-situation.md` (restored,
marked superseded); `docs/decisions/0582-the-witness-binding-is-
bidirectional.md` (new); `docs/digest/decisions-in-force.md` (regenerated).

---

#11 [G5] — **Task 4 review round 3: F2 fails a third time, and the fix is
to stop asserting completeness, not to enumerate better.**

*What happened.* Round 2's `witness_binds` fix closed with "actant-role
assignment ... is now the ONLY disclosed limit." A third review found this
false with a live probe:

```
situation requires [predicate:instance-of, phenomenon:eclipse]
witness stages cast [goblin, drow] + instance-of(0,1), no phenomenon of any kind
-> outcome: Some(Stageable)
```

`witness_binds` is scoped to `predicate:` tokens — correctly, since
nothing in `hornvale_vessel::Tableau` can represent a `concept:`/
`phenomenon:` requirement — but that scoping was documented as an
implementation footnote and then directly contradicted by the record's own
closing sentence two paragraphs later. Both frozen corpora genuinely
require such tokens (`concept:child/die/god/parent/person/sibling/spirit`,
`phenomenon:eclipse/heliacal-rising/night-star/wandering-star`, plus
`phenomenon:cold/heat` in `polti-1895`), so this was not a theoretical gap.

*The recognized pattern.* Three consecutive records (0577, 0582, and — had
this round not caught it — a fourth) each asserted their own disclosure was
COMPLETE, and each was falsified by the next reader who looked harder.
"The limits include X and Y" survives discovering a third limit Z; "X is
the only limit" does not survive any Z at all, found or not. The ruling
this round follows: stop making the falsifiable claim rather than trying
to out-enumerate the next reviewer.

*The fix.* `witness_binds`'s own doc, the shared report constant
`WITNESS_BOUNDARY_WHAT` (used verbatim by `render` and `render_matrix`),
and the decision chain now list KNOWN limits as an explicitly OPEN list —
no closing punctuation of the "this is all of them" shape. Three limits
are named: actant-role assignment (spec §4.2's own stated limit),
`concept:`/`phenomenon:` requirements never being realized by a relation at
all (the gap this round's probe found), and the bar being name-level not
aptness-level (`PredicateDef` carries no object-type constraint, so a
tableau relating two goblins by `latitude` counts as realizing
`predicate:latitude` — a pre-existing `Provision`/decision-0576 limit,
inherited rather than introduced here, and named per the reviewer's own
"your call" offer since it belongs in an honest list). No code behavior
changed — this is a documentation-only round, and no new test was needed
for that reason; the existing 14-test `trope_witness::` suite and 17-test
in-module suite both still pass unmodified.

*Two N2 residuals from round 2's own fix, closed alongside.*
`blocked_by_witness`'s doc said a colliding corpus token "would collide
with this detector silently" — false as of round 2's own commit, since the
collision now reaches `describe_witness_reason`'s panic; corrected to say
so. That panic's message said "add an arm for it here", which is the right
advice for a fourth INTERNAL sentinel but misdirects a reader debugging a
CORPUS-authored collision (a `requires` token literally spelled to start
with `witness:`) — reworded to name both causes, since the panic is
reachable from corpus data, not only from a future code change.

*Decision-number provenance.* 0583 confirmed free inside the campaign's
`0576`-`0585` reserved block before use (0578-0580 remain reserved for
Tasks 5-7; 0581-0582 are now both superseded, not available for reuse —
append-only means a number is spent once claimed, superseded or not).

*Verification.* Zero verdicts moved on either frozen corpus (still 0/36,
0/409) — confirmed by regenerating all three artifacts and diffing every
`| ` table row: byte-identical; only header prose changed.
`docs/digest/decisions-in-force.md` regenerated (now lists `0583`, not
`0582`, as in force); `docs/digest/intent-vs-reality.md` regenerated and
found unchanged. `docs/audits/type-audit-report.md` regenerated and found
unchanged (no pub-boundary primitive moved this round).

*Alternatives discarded.* Leaving the "ONLY disclosed limit" claim and
merely adding the third item to the enumeration — explicitly rejected by
the ruling this round follows: a longer closed list is still a closed
list, and would invite exactly the same falsification a fourth time.
Mechanically closing the `concept:`/`phenomenon:` gap by requiring the
full token set to match the staged-relation set — rejected again (as in
0582) for making every situation requiring either kind permanently
unwitnessable.

*Ideonomy passes / overturns.* None — a review-response round.

*Capture actions.* `cli/src/tropes.rs` (`witness_binds`'s doc,
`WITNESS_BOUNDARY_WHAT`, `blocked_by_witness`'s doc,
`describe_witness_reason`'s panic message); `cli/tests/suite/
trope_witness.rs` (module doc only); `docs/decisions/0582-the-witness-
binding-is-bidirectional.md` (restored, marked superseded);
`docs/decisions/0583-the-witness-limits-list-is-open-not-closed.md` (new);
`docs/digest/decisions-in-force.md` (regenerated).

---

#12 [G5] — **Task 5: kinship as a genesis fact — predicate ownership,
`functional`, and the entity-identity route.**

*Question.* Three things Task 1's ledger entry #6 explicitly left open: (1)
does `domains/person` or `domains/history` own `parent-of`/`kin-of`; (2) is
`parent-of` `functional`; (3) how to resolve which promoted founder is a
given founder's forebear without inheriting `founder_of`'s ~3.5%
handle-collision risk.

*Decision.*

1. **Owner: `domains/person`.** Both ends of the relation are `is-person`
   entities — this crate's own subject type — mirroring how `pays-tribute-to`
   (occupation-to-occupation) stays in `domains/history`, which owns *that*
   subject type. `domains/history` computes the arithmetic the predicate
   reports the verdict of; that does not make it the owner any more than
   computing `occ-founded-from` would make it the owner of `person-founded`.
2. **~~`functional: true`, for both predicates~~ — SUPERSEDED by review round
   1 (entry #13). Both are now `functional: false`.** The reasoning below was
   sound for the direction shipped at the time (`(descendant, predicate,
   forebear)`, descendant as subject) but that direction was itself wrong
   (entry #13, I4) and is reversed; `functional` now follows the corrected
   direction. **Two further claims in this bullet were also false and are
   corrected here rather than left standing:** (a) "this is the first
   predicate in the workspace to exercise the functional-contradiction guard
   ... as a relation" is not true — `pays-tribute-to`, `person-founded` and
   `occ-founded-from` are all `functional: true` with `Value::Entity` objects
   and all predate this campaign, so the guard already runs on every commit
   of any of them; the review dispatch that produced this bullet inherited
   the same false framing from a Task 2 review comment and repeated it
   without checking, and the controller's own correction is recorded in
   entry #13. (b) "a case exercising it was added to `kinship_facts.rs`" is
   false outright — no such case was ever added there. The real, narrower gap
   the review actually found — no test anywhere forced a `Contradiction` on
   an `Entity` object specifically (both prior cases in
   `kernel/src/ledger.rs` used `Value::Text`) — is closed in entry #13
   (`kernel/src/ledger.rs::functional_contradiction_is_rejected_for_an_entity_object`).
   Original text, preserved for the record rather than deleted: "`forebear_of`'s
   own body opens `let mother = mother_of(world, occupation)?;`, and
   `mother_of` returns a single `Option<EntityId>` read off one
   `occ-founded-from` fact — so a founder has at most one recorded forebear,
   structurally, and therefore at most one `parent-of` **or** `kin-of` fact,
   never both (the `Kinship` classification chooses which predicate, not
   whether one fires)." That structural claim about the DESCENDANT side is
   still true and is exactly what entry #13 uses to explain why the
   CORRECTED (forebear-as-subject) direction is non-functional instead.
3. **Entity identity, never `RoleHandle`.** `windows/worldgen::person_promote::
   promote` maps each cast member's occupation `EntityId` (`Founder::
   community`) to its cast index, and reads the mother occupation's
   `EntityId` directly off `OccupationRecord::founded_from`
   (`Founding::From(EntityId)`) — no handle anywhere in the identity path.
   `forebear_of` is still called, but only for its `Kinship` half (`Sibling`
   vs `Ancestor(_)`, which chooses the predicate); its returned `RoleHandle`
   is discarded. Resolved in a SECOND pass, after `hornvale_person::genesis`
   returns `ids` — the forebear's PERSON `EntityId` is minted by `genesis`
   itself, in cast order, so it cannot be known before that call returns.

*Why.* All three were explicitly deferred to this task by Task 1's ledger
entry #6 ("flagged for Task 5's implementer to decide, not resolved here").
The ownership question is genuinely symmetric on mechanics (`register_
predicate` takes only `&str`s; either crate already has `register_concepts`)
so it is resolved by precedent (`pays-tribute-to`) rather than by
architecture. The `functional` question is answered by reading
`forebear_of`'s signature, per the brief's explicit instruction, not by
assumption. The identity question is not a judgment call at all — ruling 1 in
the task brief (from Task 1's finding) foreclosed handle-matching outright;
what remained was finding the entity-identity route, which `OccupationRecord::
founded_from` already carrying an `EntityId` (never a handle) made available
directly, with `Founder::community` as the other half of the map.

*Verified, not assumed (round 1's figures — SUPERSEDED by entry #13 for the
84/9 split specifically; the 93-fact total, the direction and the test names
below all changed).* Built and ran
`windows/worldgen/tests/suite/kinship_facts.rs` (5 tests: promoted-forebear →
`parent-of`; unpromoted-forebear → neither predicate; root founder → neither;
`Sibling` → `kin-of` never `parent-of`; two independent `BuildDepth::Full`
builds commit byte-identical ledgers — this last one was itself a tautology,
see entry #13, I3). `make rebaseline` + `make rebaseline-goldens`: seed 42
gained exactly 93 facts (84 `parent-of`, 9 `kin-of`) under the SHIPPED-THEN-
CORRECTED direction — matching spec §5's preregistered "+93 facts" **and**
Task 1's entity-identity cross-check figure (93/76/35/204) exactly, not
merely in the same ballpark, but the 84/9 SPLIT moves once `parent-of` is
restricted to `Ancestor(1)` (entry #13, C1) — see that entry for the
corrected split.
`bundle:consanguineal-kin` drops out of both corpora's "missing bundles"
tables (5/5, satisfied); `polti-1895` stays 0 of 36 and `tvtropes-2012` stays
0 of 409, exactly as preregistered — every situation the bundle used to block
is still blocked by at least one other missing bundle. `book/src/gallery/`
did not move (no epoch). Only `docs/audits/` and `book/src/reference/
concept-registry-generated.md` moved among the generated-path set, plus the
seam-guard roster (a second `ledger_day_of_bake_year` call site — the
kinship pass recomputes a founder's day rather than threading it through
`Founder`) — all regenerated in the same commit.

*Alternatives discarded.* `domains/history` as owner (rejected by the
subject-type precedent above); `functional: false` with de-duplication left
to the caller (would silently accept a ledger bug the guard exists to catch,
and the structural argument shows it is unreachable, so there is no cost to
declaring it); resolving identity by re-deriving `founder_of`'s handle and
matching (explicitly foreclosed — the ~3.5%-collision risk Task 1 measured);
folding the forebear into `PersonSeed` and committing inside
`hornvale_person::genesis` (rejected: the forebear's id does not exist until
`genesis` has already minted it, so the second-pass shape is not a style
choice but a sequencing necessity).

*Ideonomy passes / overturns.* None — an implementation task closing three
questions Task 1 explicitly deferred, not a design question.

*Capture actions.* `domains/person/src/lib.rs` (`PARENT_OF`, `KIN_OF`,
registration); `windows/worldgen/src/person_promote.rs` (`promote`'s second
pass); `windows/worldgen/tests/suite/kinship_facts.rs` (new);
`docs/decisions/0578-kinship-a-genesis-fact.md` (new);
`docs/digest/decisions-in-force.md` (regenerated).

---

#13 [G5] — **Task 5 review round 1: `Ancestor(n)` collapsed into `parent-of`,
the direction was backwards, and two committed-document false claims.**

*Question.* A review of Task 5's shipped commit (`88d04c6d7`, decision 0578)
returned spec ❌ on two brief steps and quality NOT APPROVED, with two
Critical findings (C1, C2) and three Important ones (I2, I3, I4). Which of
those are real defects requiring code changes, versus documentation debt?

*Correction owed, recorded here rather than only where the coordinator
raised it.* The coordinator's own dispatch told the implementer `parent-of`
"would be the workspace's first `functional: true` relation." That was
false — `pays-tribute-to`, `person-founded` and `occ-founded-from` are all
`functional: true` with `Value::Entity` objects and all predate this
campaign, and 0578 itself cited `pays-tribute-to` twice on the same page as
a relation while making the claim, a self-contradiction sitting in a
ratified decision. The coordinator inherited the framing from a Task 2
review comment and passed it on unchecked; entry #12 above is corrected in
place (not quietly) for the same reason C2 below is a Critical, not merely
an Important.

*Decision — all five findings are real, all five are fixed, in this
commit.*

1. **C1, `Ancestor(n)` collapsed to `parent-of` for every `n`.** Measured
   on seed 42 under 0578's shipped code: `Ancestor(1)` = 32, `Ancestor(2)` =
   17, `Ancestor(3..9)` = 22, `Ancestor(11..37)` = 13 — 52 of 84 `parent-of`
   facts (61.9%) were not parent-child, the deepest 37 generations removed,
   contradicting the registered `parent` concept's own "father or mother"
   definition. Fixed by restricting `parent-of` to `Kinship::Ancestor(1)`
   only; every other classification (`Sibling`, `Ancestor(n != 1)`) now
   commits `kin-of`.
2. **I4, direction inverted against registry naming rule 4.** 0578 shipped
   `(descendant, parent-of, forebear)`, which reads "the descendant is the
   parent of their own ancestor" — false whenever the remove is nonzero.
   Fixed by reversing to `(forebear, predicate, descendant)` for BOTH
   predicates, and flipping `functional` to `false` for both to match: the
   structurally single-valued side (an occupation has at most one recorded
   forebear) is now the OBJECT, not the subject, and a forebear may found
   more than one daughter community (seed 42 has one with three).
   `kin-of`'s direction is disclosed as a convention, not a truth
   requirement — kinship is symmetric, so either direction of `kin-of`
   reads true, and it follows `parent-of`'s direction only so the two
   predicates share one implementation. Cost disclosed rather than fixed:
   `kin-of` is queryable from the forebear's end only.
3. **C2, ledger entry #12 asserted a test that did not exist.** Corrected
   entry #12 in place, above, rather than silently rewriting it — a false
   coverage claim in a committed campaign document, on the exact point a
   prior review raised, is worse than the missing test itself.
4. **I2, 0578's false "first functional relation" claim, and the real gap
   behind it.** 0578 revised (now superseded by decision 0584, since the
   direction/functional changes are substantive enough to warrant a fresh
   record rather than an in-place edit — the same "supersede, never edit"
   discipline `docs/decisions/` already follows for every other correction
   this campaign). The real, narrower gap the review found — no test
   anywhere forced a `Contradiction` on a `Value::Entity` object specifically
   (both prior cases in `kernel/src/ledger.rs` used `Value::Text`) — is
   closed: `kernel/src/ledger.rs::
   functional_contradiction_is_rejected_for_an_entity_object`, ~40 lines
   including a new local test predicate (`belongs-to`).
5. **I3, `kinship_resolution_draws_no_stream` guarded nothing.** The
   reviewer inserted a real `.derive(...).stream().next_f64()` into the
   kinship pass and all five original tests still passed — verified
   independently here too (see *Verified* below, both the inert-draw and the
   value-affecting-draw experiments). The old test compared two live builds
   of the SAME code to each other, which is a tautology under determinism
   and cannot go red for this class of defect. Fixed two ways: (a) renamed
   to `kinship_pass_is_deterministic_across_two_independent_builds`, doc
   corrected to claim only what it proves; (b) a NEW test,
   `person_facts_are_unperturbed_relative_to_the_pre_task_baseline`, compares
   every `is-person`-scoped fact (`is-person`/`name`/`person-born`/
   `person-founded`/`person-died`) against an INDEPENDENT baseline —
   `windows/worldgen/tests/fixtures/pre-kinship-person-facts-seed-42.json`,
   captured from `cli/tests/fixtures/world-seed-42.json` at commit
   `93ef987e9`, the last commit before Task 5 ever touched `promote` — rather
   than against another run of the current code.

*Why.* Each finding traces to a specific, checkable fact (a golden diff, a
registry doc, a failing-to-fail test), not to a stylistic preference — the
brief for a fix round is to verify and repair, not to relitigate settled
ground, and none of the five reopen anything settled at Task 5's own
dispatch time (entity-identity route, ownership, no-stream-draw-in-principle,
the null bundle predictions).

*Verified, not assumed.* `windows/worldgen/tests/suite/kinship_facts.rs`
rewritten (8 tests, up from 5): direct-`Ancestor(1)`-forebear → `parent-of`
naming the descendant with the forebear as subject; `Sibling` → `kin-of`;
multi-generation `Ancestor` → `kin-of`; unpromoted forebear and root founder
→ no fact naming that person as object; a forebear with 3+ descendants
carries 3+ facts without a `Contradiction` (the reason `functional: false`
is correct, not merely declared); the renamed determinism test; the new
independent-baseline test. Two hand-run experiments, reverted by `cp`
backup + `md5` verification (never `git checkout --`), confirm the new
baseline test's power and its honestly-disclosed limit: (a) an INERT draw
(`.derive("mutation-test/inert").stream().next_f64()`, result discarded)
inserted into the kinship pass leaves all 8 tests green — the architecture's
own `Stream` design (local, ephemeral, never stored on `World` —
`kernel/src/seed.rs`) makes an unused draw leave no trace anywhere a test
could read, which is a property of the substrate, not a gap in this test;
(b) a USED draw (XORed into the handle fed to `Namer::name`) turns
`person_facts_are_unperturbed_relative_to_the_pre_task_baseline` red
immediately (`"Shngoshngokvo"` → `"Shngovnga"`), while the renamed
determinism test stays green throughout both experiments — demonstrating
exactly the gap I3 identified and exactly what closes it. `kernel/src/
ledger.rs`'s own suite: 41 tests (was 40), all green, including the new
Entity-object contradiction case. `make rebaseline` + `make
rebaseline-goldens`: relative to pre-campaign `main` (`93ef987e9`), still
**strictly additive** — 93 facts added, 0 removed, seed 42's total unchanged
— confirming the fix did not reopen any drift beyond the kinship facts
themselves. The SPLIT moved as C1 predicts in shape: **32 `parent-of` / 61
`kin-of`** (round 1 shipped 84/9) — measured after implementation, not
treated as confirming any prediction, per the reviewer's explicit
instruction. `bundle:consanguineal-kin` unaffected, still 5/5 (registry
membership only — both predicate names were already registered under 0578).
`polti-1895` stageable holds at 0 of 36, `tvtropes-2012` stageable holds at
0 of 409 — both nulls unchanged. `book/src/gallery/` did not move.
`docs/audits/trope-*.md`, `trope-matrix.md` and `type-audit-report.md` did
NOT move this round (token membership and pub-boundary shapes are unchanged
from round 1 — only direction, `functional`, and the Ancestor(1) split
moved); `book/src/reference/concept-registry-generated.md` and
`docs/audits/seam-guard-roster.md` DID move (predicate docs regenerated;
`ledger_day_of_bake_year`'s call-site line numbers shifted). `docs/digest/
decisions-in-force.md` regenerated for decision 0584 and 0578's superseded
status.

*Alternatives discarded.* Disclosure-only for C1 (a footnote on a false fact
is still a false fact — the reviewer's own framing, and correct); a
mechanical fix requiring the full token set to match for `kin-of` (rejected
for the same reason 0582/0583 rejected it for the witness: it would make
kinship unwitnessable for the exact situations that need it); a second,
reverse-direction `kin-of` fact so the predicate is queryable from either end
(rejected — doubles the fact count for a query need nothing in this campaign
demands yet, and the asymmetry is disclosed rather than hidden, which is the
cheaper and more honest fix); editing decision 0578 in place rather than
superseding it (rejected — the direction and `functional` changes are
substantive design reversals, not a wording fix, and this campaign's own
precedent, 0577→0581→0582→0583, is to supersede).

*Ideonomy passes / overturns.* None — a review-response round, closing five
findings against concrete evidence.

*Capture actions.* `domains/person/src/lib.rs` (`PARENT_OF`/`KIN_OF` docs,
direction, `functional: false`); `windows/worldgen/src/person_promote.rs`
(reversed subject/object, `Ancestor(1)`-only match arm); `windows/worldgen/
tests/suite/kinship_facts.rs` (rewritten, 8 tests); `windows/worldgen/tests/
fixtures/pre-kinship-person-facts-seed-42.json` (new, independent baseline);
`kernel/src/ledger.rs` (`functional_contradiction_is_rejected_for_an_entity_
object`, `belongs-to` test predicate); `docs/decisions/0578-kinship-a-
genesis-fact.md` (status updated to superseded);
`docs/decisions/0584-kinship-direction-and-the-parent-of-generation-cut.md`
(new); `docs/digest/decisions-in-force.md` (regenerated); this ledger's
entry #12 (corrected in place, not silently rewritten).

---

#14 [G5] — **Task 5 review round 2: the fix's own registry doc strings
asserted the direction the fix rejected.**

*Question.* Round 1's fix (entry #13) corrected the emitted DATA's direction
(forebear as subject) but rewrote both predicates' `register_predicate` doc
strings in the OLD, rejected direction ("the forebear whose community this
person's community was settled from…" — read under this file's own
`Meaning`-names-the-object convention, that says the object is a forebear
and the subject is the descendant, i.e. round 1's shipped-then-rejected
shape). Is this a real defect, and does it reach a durable record?

*Decision.* Yes to both. Fixed both doc strings to name the OBJECT, matching
every neighbouring valued relation in the registry (`held-by`,
`pays-tribute-to`, `person-founded`, `is-a`, `derived-from-phenomenon`):
`parent-of` → "a person whose community was settled from this person's
community, one generation removed"; `kin-of` → "a person whose community
descended or spun off from this person's community, at any remove other
than one generation." These strings are embedded in the keystone golden's
`registry` block and published in `book/src/reference/
concept-registry-generated.md`, so this needed `make rebaseline` +
`make rebaseline-goldens`, not a docs-only edit.

*Why this is the campaign's own thesis again.* The DATA was correct (round
1 fixed it) but the one place a future consumer looks up what the predicate
MEANS stated the inverted reading — a durable record asserting something
the code does not do, the exact failure mode this whole campaign exists to
close, now caught inside its own fix.

*Two smaller corrections, same round.* (a) `windows/worldgen/tests/suite/
kinship_facts.rs`'s doc comment on `person_facts_are_unperturbed_...`
misquoted the round-1 golden diff as "941 added lines"; the actual numstat
(`git diff --numstat 93ef987e9 88d04c6d7 --
cli/tests/fixtures/world-seed-42.json`) is 940. Corrected. (b)
`a_forebear_with_more_than_one_descendant_carries_more_than_one_fact_
without_contradiction` counted `PARENT_OF` and `KIN_OF` facts together per
subject, so a subject with one fact of EACH predicate could satisfy it
without either predicate individually ever needing a second object —
weaker than the name implied, though harmless (the build would already have
failed on a real functional violation). Tightened to check each predicate
separately; both are independently non-vacuous on seed 42 (`parent-of`: 6
subjects with 2 objects; `kin-of`: 5 subjects with up to 3, max
measured directly).

*One disclosure, out-of-scope but written down rather than left implicit.*
`place`/`day` on every committed fact are the DESCENDANT's community and
founding day — the object's, not the forebear-subject's. That was the
subject's own community before round 1's reversal; the emit code did not
change, only which end of the edge is the subject. Defensible (the fact
becomes observable when the daughter is founded) but never previously
stated as deliberate. Added to `PARENT_OF`'s doc in `domains/person/src/
lib.rs` and to decision 0584.

*Verified, not assumed.* `cargo test -p hornvale-worldgen --test suite
--release -- kinship_facts` → 8 passed (all, including the tightened
multiplicity test, individually). `make rebaseline` + `make
rebaseline-goldens`; relative to pre-campaign `main` (`93ef987e9`), still
**strictly additive at both granularities**: `git diff --numstat` reads 940
lines added / 0 removed (matching the corrected figure exactly), and the
fact-tuple diff reads 93 added / 0 removed, same 32 `parent-of` / 61
`kin-of` split as round 1 (doc-string-only changes do not touch fact data).
`bundle:consanguineal-kin` still 5/5; `polti-1895` stageable still 0 of 36;
`tvtropes-2012` stageable still 0 of 409 — none of `docs/audits/trope-*.md`
moved this round (predicate names and registration unchanged). `make
gate-commit` green, 1010/1010 sub-floor tests.

*Alternatives discarded.* Leaving the doc strings as-is with a comment
explaining the "actual" direction elsewhere — rejected for the same reason
0578's original false claim was corrected in place rather than footnoted:
the `Meaning` column IS the lookup surface, and a footnote a reader has to
already know to search for is not a fix.

*Ideonomy passes / overturns.* None — a review-response round.

*Capture actions.* `domains/person/src/lib.rs` (`PARENT_OF`/`KIN_OF`
`register_predicate` doc strings corrected to name the object; `place`/`day`
disclosure added); `windows/worldgen/tests/suite/kinship_facts.rs` (941→940
correction; multiplicity test tightened to check each predicate
independently); `docs/decisions/0584-kinship-direction-and-the-parent-of-
generation-cut.md` (place/day disclosure added); `cli/tests/fixtures/
world-seed-42.json`, `book/src/reference/concept-registry-generated.md`
(regenerated).

---

#15 [G5] — **Task 6: affect through the component home — predicate
ownership, the `Home::Component` payload, and the `registry_tokens`-vs-
`serves` reconciliation.**

*Question.* Three things the task brief left open: (1) who owns
`affect-kind`/`affect-intensity` given `windows/sentiment` cannot ("a window
may not declare vocabulary a domain must own"); (2) what real payload
replaces `Unwired` on `Home::Component`; (3) `cli/src/tropes.rs` calls a
private `registry_tokens` scan at two sites (the Leverage fan-in `held` set,
and the Columns section's token count) that agrees with `Provision::serves`
only as long as every served token is also a registry token — a component-
home row breaks that.

*Decision.*

1. **Owner: `domains/species`.** It already owns the `psyche_registry`/
   `society_registry` component data `snap_judgment` is computed from, is
   kernel-only (satisfies `domains/CLAUDE.md`'s one rule), and
   `windows/sentiment` already depends on it for exactly that data — so
   declaring the two constants there keeps `windows/sentiment` purely
   presentational. Same subject-type precedent entry #12 used for
   `parent-of`/`kin-of` in `domains/person`. Both are plain `pub const &str`,
   never passed to `register_predicate` — there is no `ConceptRegistry` row
   for either, on purpose, since neither is ever fact-worthy (§4.4: "no fact
   is committed").
2. **`Home::Component` carries `ComponentResolver = fn() -> bool`**, not an
   enum naming each producer. The only consumer (`Provision::serves`) needs
   nothing about a producer except "call it and see"; there is exactly one
   producer today, and an enum with one variant is an abstraction with
   nothing to justify it yet. A second component-home token later is a
   `declare` call away, not a match arm here. This required dropping
   `Home`'s `PartialEq`/`Eq` derive — a derived comparison of the `fn`
   payload by address is not meaningful and `rustc` warns on it under
   `-D warnings` (`unpredictable_function_pointer_comparisons`); nothing in
   the codebase ever compares `Home` values (every caller constructs or
   pattern-matches), so the fix removes the unused derive rather than
   suppressing the warning.
3. **`registry_tokens` is deleted outright, not kept alongside a second
   computation.** Both call sites (`tropes.rs`'s `held` set and the
   Columns-section token count) now read a new `Provision::served_tokens`
   — every token the table's rows currently serve, home-blind, computed by
   the same `serves()` the resolver itself calls. This is a real property
   fix, not tidiness: `render`'s `held.contains(t)` invariant comment ("a
   witness-blocked situation reached the witness check only because every
   one of its bundles is ALREADY held") is stated as true "by construction"
   of `resolve`'s `missing.is_empty()` check — and that construction breaks
   the moment `held` and `serves` are computed two different ways, which is
   exactly what a component-home row does. No witness touches `felt-affect`
   yet, so nothing was actually miscounted today; this closes the
   invariant before a future witness could expose it.

*Why.* (1) follows `domains/CLAUDE.md`'s one rule (a domain owns kernel-only
vocabulary; a window presents, never declares) and entry #12's precedent
directly — there was no live alternative once the layering rule is taken
seriously. (2) is a proportionality call: the brief says "give the component
home its own resolver", and a bare `fn` pointer is the smallest thing that
satisfies "call it and see" without inventing structure the codebase does
not need yet (Decision Framework: simplicity, reversibility — a second
producer only ever needs a `declare` call, never a new enum variant). (3) was
flagged explicitly by the review that wrote this task's brief, and verifying
it by hand (rather than trusting the "it hasn't broken yet" observation)
confirmed the reasoning: `render`'s `held` was a raw registry scan
independent of `Provision`, and would have silently disagreed with `serves`
for exactly the two tokens this task adds.

*Verified, not assumed.* `cargo test -p hornvale --test suite provision` (13
tests, all green) including the two honesty-guarantee tests named in the
brief: `feels_toward_does_not_resolve` (asserts `!serves` and that the row's
`Unserved::NotServed` reason names the grain ruling) and
`bundle_felt_affect_reads_two_of_three_and_stays_blocked` (asserts
`Outcome::Blocked(["predicate:feels-toward"])` exactly — the other two
tokens no longer appear as missing). **No fact committed**, verified on a
real world (`no_fact_is_committed_serving_affect_tokens`): serialize
`world.ledger` to JSON before and after `Provision::build`, `serves` (both
tokens), and a full `resolve` run requiring both tokens — byte-identical.
(`resolve` only ever takes `world: &World`, a shared reference, so the
borrow checker already forbids a mutation through this path; the test is the
executable record of that property, not a probe that could plausibly catch
what the type system missed.) `make rebaseline` + `make rebaseline-goldens`:
`bundle:felt-affect` moved **0/3 → 2/3** in both corpora's rendered `Blocked`
lists (every `affect-intensity`/`affect-kind` pair vanished from every
`missing` list; `feels-toward` remained in each); `polti-1895` stageable
stayed **0 of 36**, `tvtropes-2012` stayed **0 of 409** — the preregistered
null, confirmed by grepping `^Stageable` in both regenerated reports.
`trope-matrix.md`'s Columns preamble moved from "397 tokens" to "399 served
tokens" (397 registry + 2 component), and its own wording changed from
"registry" to "provision table" to stay honest about what it now counts.
`book/src/reference/layering-generated.md` gained exactly one dependency
edge (`cli → hornvale-sentiment`) and nothing else. `make gate-commit`:
green, 1010/1010 sub-floor tests, `cargo fmt --check` and
`cargo clippy --workspace --all-targets -- -D warnings` both clean, `type-
audit check` clean after tagging `served_tokens`'s `BTreeSet<String>` return
(`bare-ok(identifier-text: return)`). `cargo test -p hornvale --test suite`
(the full workspace-enforcement suite, per `cli/CLAUDE.md`'s "a crate-scoped
green is not a branch-green"): 286 passed, 2 failed — both
`repertory_corpus::every_founding_scene_passes_every_beat` and
`::no_scene_has_fallen_below_its_recorded_floor` (`walk-changes-the-room`),
**confirmed pre-existing on `main` at `8818619e4`** by running the identical
command in a disposable worktree checked out to that commit before this
task touched anything — same two failures, same `Err("b2")`. Unrelated to
this task (nothing here touches session state, walking, or room
perception); flagged rather than silently worked around, and left for
whoever owns that surface.

*Alternatives discarded.* Registering the two tokens as ordinary
`register_predicate` rows (would make them appear in `hornvale concepts` and
the registry-generated reference as if a fact could carry them, which is
false — the whole point is that no commit path exists at all, not merely
that it is discouraged); owning the constants in `windows/sentiment`
(forecloses by the layering rule itself); an enum-per-producer
`ComponentResolver` (premature abstraction — see decision above); leaving
`predicate:feels-toward` simply undeclared rather than an explicit `Absent`
row (would resolve identically, `Blocked`, but hides *why* from a reader of
`Provision::build` who has not also read this ledger or the decision).

*Ideonomy passes / overturns.* None — an implementation task closing
questions the brief posed, not a design question.

*Capture actions.* `cli/src/provision.rs` (`Home::Component` payload,
`ComponentResolver`, `Provision::build`, `Provision::served_tokens`,
`sentiment_affect_holds`, `Home`'s dropped `PartialEq`/`Eq`); `cli/src/
tropes.rs` (`registry_tokens` deleted; both call sites read `Provision::
served_tokens`; `resolve` calls `Provision::build`); `domains/species/
src/lib.rs` (`AFFECT_KIND`, `AFFECT_INTENSITY`); `cli/Cargo.toml`
(`hornvale-sentiment` dependency); `cli/tests/suite/provision.rs` (4 new
Task 6 tests); `docs/decisions/0579-affect-component-data-never-a-fact.md`
(new); `docs/audits/trope-coverage-*.md`, `docs/audits/trope-matrix.md`,
`docs/audits/type-audit-report.md`, `docs/digest/decisions-in-force.md`,
`book/src/reference/layering-generated.md` (regenerated).

---

#16 [G5] — **Task 6 review round 1: a stale doc sentence, a missing
negative control, and a word the code no longer earns.**

*Question.* Reviewer found three small things after approving Task 6 spec
and quality: (1) `ComponentResolver`'s own doc claims `Home` "keeps its
existing derives unchanged", thirty lines above `Home`'s own doc stating the
opposite (it dropped `PartialEq`/`Eq`); (2) the component home has no
negative control — the ledger home's `declared_but_unserved_token_is_refused`
proves a declared-but-refused row stays missing, and nothing proves the same
for `Home::Component`, whose one real resolver can never itself return
`false` (`hornvale_sentiment::catalog()` reads a hardcoded fifteen-people
table); (3) the Supply section's rendered text still says "registered
tokens" when its input (`held`) is now `Provision::served_tokens`, which can
include a token no `ConceptRegistry` row names.

*Decision.*

1. Deleted the stale sentence in `ComponentResolver`'s doc; replaced with a
   pointer to `Home`'s own (correct) doc rather than restating the reasoning
   a second place it could rot again.
2. Added a component-home negative control test, beside the ledger home's
   own in `cli/src/provision.rs`'s in-module test block — declares a row
   with a non-capturing closure (coercible to `ComponentResolver`) that
   always answers "no" as its resolver, and asserts `serves` refuses it.
   **Verified it actually fires, not merely added green**: inverted the
   resolver to always answer "yes" (kept a `cp`-backup, restored by `cp`
   after — not `git checkout --`, per the standing trap), re-ran, watched
   it fail its own assertion, restored, and confirmed it passes again with
   an `md5` match against the backup.
3. Changed the Supply header's rendered text from "N registered tokens" to
   "N served tokens", and updated the adjacent code comment the same way.
4. Documented the `Absent`-row-overwrite tripwire in decision 0579's
   Consequences section (it was verified correct at spec/quality review but
   not yet written down anywhere durable): `Provision::declare` overwrites
   same-token rows, `Provision::build` runs the ledger scan first and the
   `feels-toward` `Absent` declaration last, so a future person-scale
   registration would NOT surface — the audit and both honesty tests would
   stay green with a real producer sitting unused. The direction is safe
   (blocks a legitimate landing rather than admitting an illegitimate one)
   and the reason is discoverable at the row; completing `felt-affect` at
   person grain requires deleting the hardcoded `Absent` declaration in
   `Provision::build`, not merely registering the predicate elsewhere.

*Why.* All three were exactly what the reviewer found, verified rather than
taken on faith: (1) by reading both doc comments side by side and confirming
the derive line carries only two traits, not four, against the source; (2)
by the standard TDD proof-of-life (red before green); (3) by re-reading the
Supply block's rendered prose after decision 0579 changed what its input
computes, which is exactly the class of drift this campaign exists to catch
(a wrong word surviving in a committed artifact after its input changed
meaning).

*Verified, not assumed.* Formatting, linting and the type audit all clean.
The full `provision`-scoped test set (unit tests plus the integration suite
plus one doctest) all green, including the new negative control. A
regeneration pass moved only the two trope-coverage reports, each by exactly
one word on one line (the Supply count itself unchanged, confirming the
wording fix changed no computation). The commit gate is green at its full
sub-floor roster. The full workspace enforcement suite passes except the
same two pre-existing scene failures already flagged in entry #15
(coordinator independently verified these fail on pristine `origin/main` and
confirmed they are not this task's).

*Alternatives discarded.* Restating the `PartialEq`/`Eq` reasoning in
`ComponentResolver`'s doc instead of deleting and pointing at `Home`'s —
rejected, since the reviewer's own diagnosis of the original defect was that
the SAME fact stated in two places let one copy rot while the other stayed
correct; a pointer has nothing to rot. Verifying the negative control by
inspection alone (reading the resolver's call site) rather than an
inversion — rejected per the coordinator's explicit ask and per this
campaign's own recurring lesson that a green test nobody has watched fail is
not yet evidence.

*Ideonomy passes / overturns.* None — a review-response round.

*Capture actions.* `cli/src/provision.rs` (`ComponentResolver` doc fix; new
component-home negative-control test); `cli/src/tropes.rs` (Supply header
wording and its adjacent comment); `docs/decisions/0579-affect-component-
data-never-a-fact.md` (Consequences bullet documenting the `Absent`-row-
overwrite tripwire); `docs/audits/trope-coverage-{polti-1895,tvtropes-2012}.
md` (regenerated, one word each); `.superpowers/sdd/2026-09-01-the-avowal/
task-6-report.md` (fix-round appendix).

---

#17 [G5] — **Task 7: acts through the session home — the derived act view,
a real collision the tests caught rather than a hazard merely avoided, and
the `bundle:witnessing` null.**

*Question.* Three things the brief left the implementer to settle: (1) what
`ActHandle`'s hashing scheme actually is, given only the precedent shapes
(`RoleHandle`, `barrier_of`) and a named hazard (`ancestor()`'s fixed-point
collapse) to work from; (2) what the session home's resolver reads, given it
is asked with a bare `&ConceptRegistry` and never a live `Session` — the
same constraint `sentiment_affect_holds` already lives under; (3) whether
the hard constraint against editing `windows/vessel/src/session.rs` was
actually satisfiable, or whether this task needed to refuse and hand back a
`BLOCKED`.

*Decision.*

1. **`ActHandle` folds four constituents (`actor`, `deed`, `patient`,
   `day`) through four separate `mix` steps, never the same step iterated —
   structurally closing the class of hazard `ancestor()`'s doc names.**
   `windows/vessel/src/act.rs` (new file, chosen over editing `session.rs`
   at all — see decision 3).
2. **The session home's resolver (`session_act_view_holds`,
   `cli/src/provision.rs`) is a self-contained, deterministic proof over
   fixed constituents — exactly `sentiment_affect_holds`'s shape** — because
   `Provision::build`/`serves` never have a live `Session` to hand it, the
   same way they never have a live `World` for the component home. One
   resolver serves all five tokens (`witnessed`, `present-at`, `deed-of`,
   `act-precedes`, `act-occurred-on`), mirroring how both affect tokens
   share one resolver. `predicate:history-now` — the `act-chronology`
   bundle's fourth token — is declared through the LEDGER home instead: it
   is already a committed genesis fact
   (`hornvale_history::HISTORY_NOW`), not a derived session read, and this
   task does not move it.
3. **`session.rs` was not touched.** `Session::day`, `::agent_entity` and
   `::purview` were sufficient: `windows/vessel/tests/suite/act.rs` builds
   real `Act`s off a live session using only those three, and
   `anyone_present` reads `SurroundsScene`'s own `Mark`s (kind `"agent"`)
   for a co-presence signal — proving the public surface really was
   enough, not merely asserting it. The constraint held; no `BLOCKED` was
   needed.

*Why.* (1) is the load-bearing engineering choice, and it produced a real
finding rather than a clean pass: an early draft folded the patient-
presence tag directly against the raw `EntityId` (`mix(1, entity.get())`).
`mix`'s first step is a bare XOR, so `mix(a, a) == 0` for *any* `a`, and
`EntityId::new(1)` — the smallest legal entity id, certainly reachable —
collided with the literal tag `1` the FIRST time the property-test sweep
ran, not on inspection (`no_patient_never_collides_with_a_real_one`,
`a_combinatorial_sweep_has_no_collisions` and the degenerate-case test all
failed together, with the exact collision printed:
`ActHandle(16602847279475233179)` on both sides). This is the same
methodological point `ancestor()`'s own doc makes — the hazard is avoided
STRUCTURALLY where possible (no iterated fixed permutation here) and
verified EMPIRICALLY regardless, because "structurally avoided" is a
claim about the mix's shape, not a proof about its constants. The fix
folds each presence tag against the already-avalanched accumulator instead
of a raw id (`PATIENT_SOME_TAG`/`PATIENT_NONE_TAG`), closing the specific
collision and leaving the degenerate-all-zero-shaped test in place as a
permanent regression guard. (2) follows Task 6's own precedent exactly —
proportionality, not novelty, once `ComponentResolver`'s constraint
("neither home ever has live state to read") was recognized as identical
for the session home. (3) was the task's stated hard constraint, honoured
by design rather than negotiated: `act.rs` never imports anything from
`session.rs` beyond its three public methods, verified by grep before
reporting.

*Verified, not assumed.* `cargo test -p hornvale-vessel act::` (14 unit
tests, `windows/vessel/src/act.rs`'s own `mod tests`) and
`cargo test -p hornvale-vessel --test suite -- act` (5 integration tests,
`windows/vessel/tests/suite/act.rs`) both green, including the caught-
then-fixed collision above. `cargo test -p hornvale --test suite --
provision` (13 tests: the 8 pre-existing plus 5 Task 7 tests) green,
including `no_fact_is_committed_serving_act_tokens` (a real world's ledger
serialized before/after `Provision::build`/`serves`/a full `resolve` run
requiring all five tokens: byte-identical) and
`bundle_witnessing_reads_two_of_two_but_stays_blocked_on_absent_witness`
(`Outcome::Blocked(["witness:absent"])` exactly — both tokens resolve,
only the witness itself is missing). **The positive control the brief's
own trap-list warns about** (an empty diff needs a positive control, not
just an empty diff): `generating_many_acts_commits_nothing_and_the_ledger_
comparison_can_detect_a_real_commit` derives 400 acts and every read this
task ships off one live session, asserts byte-identity, and only THEN
performs a real in-character walk — the fact count and serialized ledger
both move, proving the earlier byte-identity assertions were not vacuous.
The same test's tail proves decision 0368's mechanism still carries a real
commit through `into_played_world`, even though no act-derived read ever
produces one for it to carry. `make rebaseline`: `docs/audits/trope-
coverage-{polti-1895,tvtropes-2012}.md` moved — `predicate:present-at` and
`predicate:witnessed` vanished from every `missing` list that named them
(e.g. polti-06-disaster's `missing` shrank from 6 tokens to 4; polti-19,
27, 32, 33 and 36 each lost the same two). No situation in either frozen
corpus requires `bundle:witnessing` alone — every one that named it also
named at least one still-missing bundle — so none crossed into
`witness:absent` territory; that reason only appears in this task's own
synthetic corpus test
(`bundle_witnessing_reads_two_of_two_but_stays_blocked_on_absent_witness`).
`docs/audits/trope-matrix.md`'s Columns preamble moved from "399
served tokens" to "404" (399 + 5). **`Stageable` moved on neither
corpus**, confirmed by grepping `^Stageable` in both regenerated reports
before and after: `polti-1895` **0 of 36** both times, `tvtropes-2012`
**0 of 409** both times — spec §5's preregistered null, held. `make
gate-commit`: green, 1010/1010 sub-floor tests. `HV_TEST_OK=1 cargo
nextest run --workspace --no-fail-fast`: **4992 passed, 0 failed, 211
skipped** (no pre-existing failures observed, unlike Task 6's entry —
whatever produced those two scene failures there is not present at this
commit). `HV_TEST_OK=1 cargo test --workspace --doc`: all green.
`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`: clean
after tagging `ActHandle`'s tuple field and `Act::deed` (both
`identifier-text`) and four new `bool` returns (`flag`) —
`docs/audits/type-audit-report.md`'s vessel row moved 397→403 tagged
primitives, exactly +6.

*Alternatives discarded.* Folding the patient-presence tag into the
`Act`'s own field order instead of a discriminant (would not distinguish
`patient: None` from a `patient: Some(_)` whose id happens to fold to the
same intermediate state — the exact bug found); reading co-location by
resolving `Mark::noun` strings against a name catalog rather than naming
the limit and taking an explicit `&[EntityId]` pool (a string match is not
an identity — two same-named NPCs would be indistinguishable, and the
honest answer is that the chart cannot say who, not a fragile workaround
pretending it can — recorded as `SCN-marks-carry-no-entity-id`); building
a full `Session` inside the session-home resolver itself (no `Provision`
caller ever hands it a world or session, so this would have required
either changing `Provision::serves`'s signature — out of scope and a
change Task 6 did not make either — or constructing a throwaway world on
every resolve call, which is neither what the component-home precedent
does nor affordable at the report-generation call sites).

*Ideonomy passes / overturns.* None — an implementation task closing
questions the brief posed, not a design question.

*Capture actions.* `windows/vessel/src/act.rs` (new: `ActHandle`, `Act`,
`witnessed`, `present_at`, `deed_of`, `act_precedes`, `act_occurred_on`,
`anyone_present`); `windows/vessel/src/lib.rs` (`pub mod act;`);
`windows/vessel/tests/suite/act.rs` (new, 5 tests) and
`windows/vessel/tests/suite.rs` (registers it); `cli/src/provision.rs`
(`Unwired` replaced by `SessionResolver`; `Home::Session` payload;
`Provision::build`'s five new rows; `session_act_view_holds`); `cli/tests/
suite/provision.rs` (5 new Task 7 tests); `docs/decisions/0580-acts-are-
addressable-without-being-stored.md` (new); `book/src/frontier/idea-
registry.md` (`SCN-marks-carry-no-entity-id`, per spec §7's "anything §4.5
defers"); `docs/audits/trope-coverage-*.md`, `docs/audits/trope-matrix.md`,
`docs/audits/type-audit-report.md`, `docs/digest/decisions-in-force.md`
(regenerated).
