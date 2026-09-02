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
