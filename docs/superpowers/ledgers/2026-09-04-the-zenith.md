# The Zenith — decision ledger

Campaign: **The Zenith** (`campaign/the-zenith`), retiring the astronomy
provider-tier system. Spec:
[`2026-09-04-the-zenith-design.md`](../specs/2026-09-04-the-zenith-design.md).
Decision block reserved: **0736–0745**.

Predecessor: The Wash's ledger entry
[#15](2026-09-03-the-wash.md) filed `SKY-retire-the-tier-system` and
deliberately did not fold it into that campaign.

---

#1 [Q] — **The registry row's scope estimate is wrong by roughly seventy
times, and the campaign's shape follows from the true number.**
· **Question:** is this "six production files", as
`SKY-retire-the-tier-system` states?
· **Decision:** no. Measured on `main` at `d1895029c`:

```
SkyChoice::Generated sites     382
SkyChoice::Constant sites       23
                              ----
SkyChoice sites total          405
build_world* call sites        335
Sky::Constant / ConstantSun     57   (across 17 files)
```

· **Why it matters rather than being a pedantic correction:** `SkyChoice` is
a positional parameter on `build_world`, the most-called function in the
workspace. "Six production files" is an accurate count of the files that
*handle* `Sky::Constant`; it is not the size of the change, because the
parameter's removal touches every construction site. Precedent for the
failure mode is in memory and in The Quire: *"`git add <two paths>` — that IS
the complete file set"* asserted a file set that adding a struct field
invalidated at every full-literal construction site. Same shape here.
· Alternatives discarded: taking the row's number on faith (it is the
campaign's own scoping input, and it was authored from a survey of
`Sky::Constant` handling only).
· ideonomy passes: 3 / overturns 2 (see #6).
· Capture: the registry row's **Where** cell is corrected at close.

#2 [Q] — **`sky_of` errors on a world carrying no `sky-provider` fact; it
does not fall back.**
· **Question:** `sky_of` today returns `Ok(Sky::Constant(ConstantSun))` when
a world's ledger holds no `sky-provider` fact — a documented 1a/1b-era
save-compat path. Deleting the variant deletes the fallback's target. What
replaces it: an error, or a fallback to `Generated`?
· **Decision:** **error**, with the predicate named.
· **Why (precedent):** decision
[0189](../../decisions/0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md) settles
the identical question one layer down — a pre-tick-flip `world.json` does not
load, no float-tolerant shim is added, because *"a world is a seed plus a
ledger"* and every world is re-derivable from its seed and pins. The 1a/1b
worlds this fallback serves are **strictly older** than the worlds 0189
already refuses.
· **Verified rather than assumed:** `sky-provider` is committed
unconditionally at `windows/worldgen/src/lib.rs:8139-8146` (the predicate
itself on 8142) — not inside any
branch — so **no world any current build produces can lack the fact.** The
fallback's only reachable callers today are hand-constructed bare `World`s in
tests (`absent_sky_provider_fact_falls_back_to_constant` builds one with
`World::new(Seed(1))`).
· Alternatives discarded: **fall back to `Generated`** — coherent, since a
sky is a pure function of (seed, pins) and absent pins are default pins, but
it makes `sky_of` succeed on a world that was never built, converting a
detectable bug into a silently plausible world; **make absence
unrepresentable in the type** — `World` is a serialized JSON document, so it
cannot be.
· ideonomy passes: 1 (negation on "the fallback exists", which produced the
three candidates above) / overturns 0.
· Capture: decision record 0737.

#3 [Q] — **`SkyChoice` is deleted outright, not reduced to a one-variant
enum, and the parameter leaves `build_world`'s signature.**
· **Question:** a two-variant enum losing one variant could stay as a
one-variant enum, costing zero call-site churn against ~405 sites.
· **Decision:** delete it; `build_world` and `build_world_to` lose the
argument.
· **Why (precedent):** the project deletes rather than deprecates, twice
ratified —
[0039](../../decisions/0039-epochs-replace-tiers-refine.md) refused to keep
terrain v1 as a coexisting fallback tier precisely because it *"would have to
be maintained forever"*, and 0189 refused a compatibility shim for the same
reason. A one-variant enum is the deprecation shape both decisions reject,
carrying a parameter that can no longer express a choice.
· **Cost, stated rather than hidden:** ~405 edits. 382 are the identical
deletion of a `SkyChoice::Generated` argument; 23 are the campaign's real
work (#4).
· ideonomy passes: 1 / overturns 0.
· Capture: spec §5, stage 3.

#4 [Q] — **The 23 constant-sky sites are flipped to `Generated` BEFORE the
parameter is deleted, not after.**
· **Question:** ordering. The obvious order is "delete the type, then fix
what breaks."
· **Decision:** invert it. Flip the 23 first, while `SkyChoice` still exists;
delete the parameter across all 405 afterwards.
· **Why:** the naive order produces one ~405-site diff in which the 23
semantic changes are indistinguishable from the 382 mechanical ones. Inverted,
the risky diff is 23 sites and reviewable, and the large diff is trivially
verifiable. This is the campaign's single largest risk-reduction and it costs
nothing.
· **The measurement that motivates it:** a constant-sky world is not a
cheaper generated world, it is a **different** world. Paired timings on the
Mac, n=5, spread ±1%, debug profile:

```
constant   0.86 s   21,008 facts    16 beliefs
generated  2.62 s   21,728 facts   145 beliefs   (+390 founding-solstice-azimuth facts)
```

3.05x and +1.76 s per world, but the fact composition is the point: 9x the
beliefs, and a whole predicate that exists only with a calendar. Every one of
the 23 sites is asserting against the left column. Flipping them is a change
of **subject**, not a re-timing, and that is invisible if it lands inside a
405-site diff.
· **Consequence for the plan:** the 23 do not share one treatment. Spec §4
sorts them into four kinds.
· ideonomy passes: 3 / overturns 1 (this ordering **is** the overturn).
· Capture: spec §5, stages 2–3.

#5 [Q] — **`tier_refinement.rs` is promoted, not deleted and not
"re-expressed".**
· **Question:** The Wash's #15 called the battery "the one real loss", whose
invariants "must be re-expressed as direct assertions."
· **Decision:** it is a smaller loss than that framing implies, and the right
verb is *promote*. Verified by reading all four tests: **two of the four
never mention `ConstantSun` at all** —
`refinement_adds_structure_only_beneath_the_sun` and
`the_suns_added_period_is_the_day_the_calendar_already_holds` are already
direct assertions over the generated sky. The other two use `ConstantSun`
only to *source two literals*: `kind == CELESTIAL_BODY`, `salience == 1.0`,
and the string `"the sun"`.
· **Why promotion is strictly stronger than the status quo:** today
`every_generated_sky_keeps_the_one_sun_tier_0_promises` derives its
expectation by calling `ConstantSun.phenomena()` — production code inside the
crate under test. Freezing those two literals in the test removes that
coupling. The battery stops asserting "the fine tier refines the coarse one"
and starts asserting "this is what a Hornvale sky **is**", which is the claim
that was always doing the work.
· **The reframe that produced this:** tier-0's entire definitional content is
*acyclicity* — no day, no year, no phases, no eclipses. Every `Option` guard
downstream is guarding acyclicity. So the retirement is not "delete a tier",
it is **"assert that every Hornvale world has a cyclic-capable sky"** — which
names exactly the invariant being added.
· **And the fact that makes it safe:** acyclicity is *already* representable
without tier-0. `--rotation locked` produces an aperiodic sun, the repo
already commits its almanac
(`book/src/gallery/almanac-seed-42-locked.md`: *"This world is tidally
locked: no local day exists"*), and `tier_refinement.rs` itself asserts the
equivalence — *"a locked sun is aperiodic, like tier 0's"*. Tier-0 is a
degenerate case of a regime the generated sky already covers.
· ideonomy passes: 3 / overturns 1 (the acyclicity reframe).
· Capture: spec §4 kind D; decision record 0738.

#6 [Q] — **The registry row's open question is answered: the keystone
fixture is tier-0 by inertia, not by design.**
· **Question:** The Wash's #15 left this open — *"why the keystone fixture is
tier-0 at all. It looks deliberate and I did not find its reason. If it was
chosen so the keystone stayed stable while astronomy churned, the retirement
has to answer that."*
· **Decision / finding:** it was not chosen. `book/src/gallery/almanac-seed-42.md`
was the **only** almanac that existed before the sky campaign. Commit
`0779d870c` (2026-07-06, *"feat(cli): sky pins as flags, generated-sky
default, and the scout verb"*) flipped `new`'s default to generated and in the
same commit added `--sky constant` to the CI seed-42 line — a one-line change
to `.github/workflows/ci.yml` whose whole purpose was to keep the
pre-existing artifact byte-identical through the default flip.
`almanac-seed-42-sky.md` was added later the same day (`03f0e577a`) as the
*new* generated artifact.
· **Why this settles it in the retirement's favour:** the hypothesis The Wash
raised — that tier-0 was chosen to insulate the keystone from astronomy churn
— is false. The pin is an artifact-stability freeze from a single campaign
transition, preserved by inertia for two months. Nothing depends on its being
tier-0.
· **Verified rather than reasoned:** `git log -S'--sky constant' --
.github/workflows/ci.yml` and `git show 0779d870c -- .github/workflows/ci.yml`.
· ideonomy passes: 1 / overturns 0.
· Capture: spec §3.

#7 [Q] — **The 405 sites are not homogeneous, so the mechanical pass is
compiler-driven, never a scripted rewrite.**
· **Question:** 382 identical-looking `SkyChoice::Generated` arguments invite
an `rg -r` or `sed` sweep.
· **Decision:** refused. Measured: **372 of the 382 are the plain
trailing-comma argument form; 10 are not.** The ten are three `assert_eq!`
comparisons (`cli/src/main.rs:2509, 2560, 2651`), an array literal
(`windows/worldgen/tests/suite/pin_enumeration.rs:66`), a match arm
(`windows/worldgen/src/lib.rs:8137`), an `if let` guarding the entire sky
genesis stage (`windows/worldgen/src/lib.rs:8155`), a doc-comment example
(`windows/worldgen/src/fixture.rs:62`), and three prose comments. A regex
sweep corrupts each while leaving the tree plausible.
· **Correction worth recording, because the first draft of this entry got it
wrong:** that draft cited `cli/src/main.rs:2554` and
`clients/world-wasm/src/lib.rs:95` as non-argument *`Generated`* sites. Both
lines are `SkyChoice::**Constant**`. The conclusion was right and the evidence
did not support it — the entry asserted a claim about one variant and cited
lines belonging to the other. Caught by re-reading the cited lines rather than
the cited claim.
· **What makes the compiler sufficient here, checked rather than assumed:**
there are **no `_ =>` wildcard arms** on any `Sky` match in
`windows/worldgen/src/lib.rs`. Memory's rule — *"for an enum widening the
compiler is the enumeration; a `_` arm voids it"* — applies in the narrowing
direction too, and the precondition holds.
· ideonomy passes: 3 / overturns 0 (an enrichment from the homogeneity
prompt, not a reversal).
· Capture: spec §5, stage 3's verification clause.

#8 [Q] — **The cheap-world mitigation already exists and is already the
project's pattern: `hornvale_worldgen::fixture::seed_42_world()`.**
· **Question:** #4 measures that flipping the 23 sites to `Generated` costs
+1.76 s per world. Does the campaign accept that, or mitigate it?
· **Decision:** mitigate at the sites where it applies, using the existing
loader rather than anything new. `windows/worldgen/src/fixture.rs` reads the
committed 5.5 MB `cli/tests/fixtures/world-seed-42.json` from disk instead of
rebuilding — and its own doc comment states it is byte-identical to
`build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated,
&TerrainPins::default(), &SettlementPins::default())`. That is *exactly* the
call the flipped sites would otherwise make.
· **Verified reach:** 56 call sites across 27 files already use it. This is
an established pattern, not a new seam invented for this campaign.
· **Why it is better than either alternative:** it is cheaper than the
constant-sky build the sites use today (a file read against 0.86 s), not
merely cheaper than the generated build. The campaign can therefore *reduce*
test cost while removing the tier.
· **The limit, stated rather than glossed:** it serves seed 42 under default
pins only. Nine of the 23 sites build exactly that; eight build a different
seed (0, 1, or a loop variable) and must pay a real build; six are structural
(the CLI flag, the wasm parse, a match arm, an `assert_eq!`, the enumeration
array) and build nothing.
· **And the trap the spec must not walk into:** a site whose subject IS the
build path cannot substitute the fixture for its own output without going
vacuous — `repose_byte_identity` compares a built world against that very
fixture. Memory's rule applies: *an empty diff needs a positive control.* So
the spec states a **decision rule** and the implementer applies it per site
after reading; it does not assign the 23 from outside the code.
· ideonomy passes: 3 / overturns 1 (this is the third overturn; the design
before it accepted a suite-wide cost increase that is in fact avoidable and
partly reversible).
· Capture: spec §4 and §5 stage 2.

#9 [Q] — **Nathan: the eternal-noon question is a CENSUS question. The
spec's "honest cost" section was wrong about the instrument, and the
correction removes the campaign's only flagged fidelity concern.**
· **What the G3 package flagged:** the Constitution names tier-0's purpose
as *"what religion develops under an eternal noon?"*, and this campaign's own
measurement answers it (16 beliefs vs 145) immediately before deleting the
ability to ask it. I presented that as a real capability loss and the
strongest argument against the campaign.
· **Nathan's correction:** *"the 'what $thing develops on a tidally locked
planet' is better described using the census."*
· **Why it settles rather than softens the concern.** Tier-0 can only answer
with **one stipulated world** — an anecdote, and one about a stipulation
rather than physics, so its religion is an artifact of the stub. The lab
answers with a population under a *derived* pin. My own memory carries the
rule I failed to apply: *one world is an anecdote.*
· **Verified before agreeing, not after** — the point was to check whether
the remedy Nathan named actually exists:
  - `pin_sets` is **live, not vestigial**: `windows/lab/src/study.rs`'s
    `PinSet` is iterated at `runner.rs:155`, each set built with its pins,
    emitted as a `pin_set` CSV column (`runner.rs:304`) and rendered per set
    by `chart.rs:175`. `PinSet::label`'s doc comment gives its example
    values as `"default"`, `"locked"` — the axis was designed for this.
  - The metrics exist: `pantheon-size`, `pantheon-verticality`,
    `belief-kind-*`, and **`pantheon-cyclic-share`**, which is the
    eternal-noon question stated as a number.
  - The study exists at scale: `census-of-faiths` runs over **10,000
    worlds**.
· **The finding that came out of checking, which neither of us had:** *no
committed study uses a non-empty pin set.* Every `pin_sets` array across
`studies/*.study.json` is `[{"label":"default","pins":[]}]`. The census
sweeps seeds and has **never** swept pins. So the machinery built for this
exact question has never been pointed at it — which means retiring tier-0
removes an anecdote nobody consumed, and the better instrument has been
sitting unused the whole time.
· Alternatives discarded: folding a `{"label":"locked","pins":
["rotation=locked"]}` set into `census-of-faiths` as part of this campaign.
Refused — moving a census study is a measurement decision under decision
0016's preregistration discipline, and a retirement campaign is the wrong
place to make one.
· ideonomy passes: 1 / overturns 1 — the pass was on "is the census really
the better instrument, or is that a convenient agreement?", and it overturned
my flagged item rather than confirming it.
· Capture: spec §2.2 rewritten; follow-up below.

#10 [G4] — **Plan self-review found a spec/plan contradiction, and the SPEC
was the wrong half.**
· **The contradiction:** spec §9's DoD said the zero-occurrence check must be
*"asserted, not eyeballed"*, which reads as a committed source-scan test. The
plan's Task 8 refused to write one.
· **Decision: the plan is right; the spec is amended.** Once `ConstantSun`,
`Sky::Constant` and `SkyChoice` are deleted, nothing can reintroduce them
without also failing to compile. A committed test asserting they do not
appear **could never go red** — and this project holds that a check that can
never fire is worse than an absent one, because it sits in a healthy-looking
tree reading as coverage. The compiler is the guard; the close-time grep is a
verification, not a ratchet.
· **Why it is worth a ledger entry rather than a quiet edit:** the wrong
instrument here would have looked like *more* rigour, not less. "Assert it,
do not eyeball it" is normally the right instinct in this repo, and the
phrase came from applying that instinct without asking what could make the
assertion fail.
· ideonomy passes: 1 (negation on "the check exists" — what would have to be
true for it to fire?) / overturns 1, the spec clause.
· Capture: spec §9 amended in place with the reasoning; plan Task 8 Step 1
carries the same note so an implementer does not helpfully add the test back.

#11 [G4] — **Three traps found while writing the plan, none of them in the
spec, all found by reading the tree rather than reasoning.**
· **Trap 1 — stacked guards.** `Sky::calendar()`'s `Option` is the tier;
`Calendar::day_length()`'s `Option` is **tidal locking**, a real regime that
survives. They sit *adjacent* at three verified sites —
`windows/worldgen/src/lib.rs:5192` (two `let … else` in a row),
`windows/vessel/tests/suite/the_detent.rs:153` (two chained `and_then`s), and
`worldgen::observation_time_is_zero_for_constant_and_locked_skies`, a test
that loses **half** its subject. An implementer told to "delete the Option
guards" deletes both halves and silently breaks locked worlds.
· **Trap 2 — "tier" is overloaded.**
`windows/scene/examples/tier_comparison_spike.rs` means *rendering
vocabularies* by tier 0/1/2 (ASCII / ANSI / CP437), not astronomy providers.
A `grep -r tier` sweep hits it. The plan names it as do-not-touch.
· **Trap 3 — a false artifact no gate can see, and this is the serious one.**
`kernel/examples/first_light.rs:152` hardcodes *"Under a golden sun fixed at
zenith"* into `book/src/gallery/world-seed-42.md`, a committed,
drift-checked artifact written by `regenerate-artifacts.sh:1002`. The example
imports **only `hornvale_kernel`** — it cannot reach `ConstantSun` and
derives nothing from it. So deleting the tier leaves the sentence in place,
`make rebaseline` reproduces it byte-identically, and the drift check
compares the file against itself and reports **no drift, forever**. The
artifact becomes false and every mechanism the project has says it is fine.
· **The general lesson, which outlives this campaign:** a drift check
verifies that an artifact still matches its generator. It says nothing about
whether the generator is still *right*. Hardcoded prose in a generator is
invisible to it by construction. `book/src/domains/religion.md` carries the
same sentence and the phrase "waiting for a sky that actually cycles" —
prose that went stale when Campaign 2b shipped, two months before this
campaign noticed.
· ideonomy passes: 0 — these came from reading the tree for the plan's file
lists, not from an expansion.
· Capture: plan's "Three traps" preamble; retrospective (Task 8 Step 5).

#12 [G5] — **A file-overlap conflict scan cannot see an artifact-to-test
coupling, and this one blocked Task 1 at its own commit gate.**
· **What happened.** Task 1 changes `book/src/gallery/almanac-seed-42.md`
from the tier-0 world to the generated one. `cli/tests/suite/
repose_byte_identity.rs::seed_42_almanac_is_unmoved_by_the_repose` builds a
constant-sky world and asserts it renders byte-identically to that file. The
test is false the instant the artifact lands, so `make gate-commit` refused
Task 1's commit. The implementer diagnosed it correctly and stopped rather
than reaching for `--no-verify`.
· **Why the pre-flight scan missed it.** The scan enumerated task pairs that
share a **file** or an interface. Task 1 owns the artifact; Task 2 owned the
test; they are different files in different crates. **The coupling is
artifact-to-test, and a file-overlap scan is structurally blind to it** — no
amount of diligence on that scan would have found this, because the thing it
compares is the wrong thing.
· **The general rule, which is the part worth keeping:** *an artifact change
and the test that pins that artifact must land in the same commit.* There is
no ordering of two commits that keeps the tree green in between, because the
commit gate runs on both. Any campaign that moves a committed artifact should
ask, before splitting tasks, which tests assert byte-identity against it —
`grep` for the artifact's **path**, not for the code that writes it.
· **Ruling R5:** the one test edit moves from Task 2 into Task 1. Task 2's
population drops 23 → 22. The fix needed no new code: `constant_sun_world()`
had exactly one caller and `generated_sky_world()` already existed beside it,
so it is a deletion plus a one-word swap.
· **Cost if wrong:** one `.rs` edit lands in the artifact commit rather than
the test commit — visible in the diff and trivially movable.
· ideonomy passes: 0 — a defect found by execution, with one correct fix.
· Capture: plan Task 1 Step 6b; SDD ledger; retrospective (this is the
campaign's best process finding so far).

#13 [G5] — **Task 4's implementation is behaviorally accepted, but its
editing-process breach is not erased by a green tree.**
· **What happened:** after using compiler diagnostics to enumerate the
call-site migration, the implementer used exact-token Perl substitutions for
the repeated `SkyChoice::Generated` argument and import removals. Task 4
explicitly prohibited scripted or regex rewrites because ten nonstandard
sites made a plausible partial rewrite dangerous. The task reviewer therefore
failed strict spec compliance even though it found no semantic corruption and
the workspace check, focused tests, clients, commit gate, and pre-commit hook
all passed.
· **Decision:** accept the resulting implementation after the review's three
stale-comment findings are fixed and re-reviewed; record the process breach as
a campaign finding rather than reverting and recreating the same 240-file
tree manually. Reverting cannot restore the forbidden process — it would only
manufacture an observationally identical diff at high cost — while accepting
without a ruling would teach that an explicit safety constraint becomes
optional when the output happens to be green.
· **Alternatives discarded:** (1) reject and manually replay every repeated
deletion: no added evidence, same final bytes, and the original breach remains
historical fact; (2) accept silently because gates passed: confuses output
validation with process compliance; (3) abandon the campaign: disproportionate
to a reversible, exhaustively compiled migration with no observed corruption.
· **Purpose/symmetry state machine:** the task moved from constrained-editing
to breached-but-unverified, then to breached-and-behaviorally-verified. It
cannot transition back to never-breached; the available accept state is
verified-plus-recorded, not fictional compliance. The asymmetric fact is
load-bearing: gates can validate the product but cannot retroactively validate
the method.
· **Cost if wrong:** an exact-token rewrite may have made a semantically wrong
deletion that compilation and the scoped review both missed; the whole-branch
review and later stage/merge gates remain independent backstops.
· ideonomy passes: 1 (tree-finding rendered as a purpose/symmetry state
machine) / overturns 0; it clarified that replay cannot reach a compliant
historical state, so evidence plus durable disclosure is the only honest
forward transition.
· Capture: this ledger entry; Task 4 report disclosure; Task 8 retrospective.

#14 [G5] — **Task 5 narrows to the Option boundary Task 4 deliberately left;
enum-arm cleanup already happened at the only compilable boundary.**
· **Question:** Task 5's table still assigns several `Sky::Constant` /
`Sky::Generated` match-arm deletions to itself, but Task 4 replaced the enum
with a one-provider wrapper and necessarily removed those arms. Should Task 5
recreate equivalent intermediate structure, or treat those rows as completed
preconditions?
· **Decision:** treat the enum-arm rows as completed verification targets.
Task 5 owns the remaining intentional seam: make `calendar()` and `system()`
non-`Option`, delete only guards produced by that Option, retain
`Calendar::day_length()`'s tidal-locking Option, and clean the surviving tier
prose. This preserves R2's actual task boundary rather than the stale file-line
allocation around it.
· **Why:** an enum's match arms have zero longevity after the enum is deleted;
the Option seam was intentionally kept for one more task and is the durable
boundary Task 5 exists to collapse. Recreating dead enum-shaped code would be
work for the plan's wording rather than the design.
· **Alternatives discarded:** restore an enum for one commit (compilable but
contrary to Task 4 and decision 0736); duplicate already-complete edits in the
Task 5 report (false authorship); silently ignore the stale table (leaves the
next implementer to rediscover the mismatch).
· ideonomy passes: 1 (organon-construction as a longevity/intentionality
spectrum) / overturns 0; it separated short-lived forced enum cleanup from the
intentionally preserved Option seam.
· Capture: plan Task 5 controller-correction note; SDD progress ledger.

#15 [G5] — **Retiring the fallback changes invalid bare-world test
populations; exact fixtures move, preregistered failures remain failures.**
· **Question:** Task 5's removal of the last sky-absence guards forces locale
tests that derived climate from `World::new(Seed(42))` onto a valid built
world. The generated population moves a desert count (15 → 0), a column
golden, one exact temperature (25.154255 → 21.91116), and the already-ignored
riparian witness (3/138 → 4/152). Is this an out-of-scope rebaseline, or the
required consequence of decision 0737?
· **Decision:** use the valid generated-sky population and accept ordinary
exact fixture/value movement with explicit provenance. Preserve the
preregistered riparian verdict unchanged — still ignored, still unmet — and
update its ignore reason, heavy-tier roster text, and registry evidence in one
commit so all three describe the newly measured witness. Do not widen its zero
tolerance. The vanished seed-42 desert exemplar must be replaced by a direct
deterministic test construction or another principled fixture; do not seed-hunt
until a convenient desert appears and do not weaken the assertion.
· **Why:** 0737 makes a bare world without `sky-provider` invalid; retaining
its constant-sky-derived values would preserve measurements of a population
the program no longer admits. Decisions 0016/0134 govern the asymmetric half:
a moved world may move a witness, but a preregistered failed criterion is not
rescued by retuning. Exact goldens and temperatures describe outputs and may
move; the riparian zero-tolerance claim describes a verdict and may not.
· **Alternatives discarded:** keep constructing bare worlds through a local
shim (reverses 0737); update only the riparian number (splits one evidence
claim across three disagreeing surfaces); re-enable/widen the riparian test
(rescues an unmet preregistration); search seeds for a desired desert
(decision 0093's seed-hunting anti-pattern); revert all locale fixtures
(leaves the suite asserting an impossible retired-provider population).
· **Distribution/symmetry lattice:** ordinary exact fixtures and
preregistered verdicts share the generated population but are incomparable
contracts. Population movement distributes across both; the response is
intentionally asymmetric — rebaseline the former, preserve and re-witness the
latter. The join is one coordinated commit whose prose names the population
change.
· **Cost if wrong:** generated-sky climate movement may be over-attributed to
the retirement rather than fixture validity; review must inspect each changed
test's stated subject, and the later campaign census remains the distributional
check.
· ideonomy passes: 1 (cross-domain re-instantiation captured as a
distribution/symmetry lattice) / overturns 0; it exposed that “calibration” is
not one ordered class — exact-output pins and preregistered verdicts require
different responses to the same population move.
· Capture: this ledger entry; Task 5 report; Task 8 retrospective.

#16 [G5] — **The close gained a narrow code reconciliation because absorbed
`main` added one world-build site after The Zenith's original census.**
· **Finding:** The Staple's committed `brief_axis_probe` still passed the
retired provider choice to `build_world`; three absorbed comments also named
the retired types. The one-time Definition-of-Done grep therefore returned
four Rust hits after a clean absorption even though the pre-absorb branch had
none.
· **Decision:** update the probe to the post-Zenith API without changing its
measurement, rewrite the three comments historically, and record those files
in Task 8's scope. This is merge reconciliation, not a reopening of the
retirement design.
· **Why:** the probe is durable evidence for The Staple and must survive; the
compiler is the correct guard for its API call, while the close grep is the
only instrument that catches stale identifiers in comments. Hiding the files
from the plan would make the close's actual scope false.
· **Incoming context read:** The Staple's chronicle and retrospective explain
why this ignored probe is committed rather than scratch: its four-thousand-
occupation measurement is the metaplan's load-bearing evidence. Deleting or
quieting it would destroy another campaign's product.
· **Cost if wrong:** the probe could silently change population or depth while
being made to compile; focused execution must preserve its output shape, and
review must inspect the call rather than accept compilation alone.
· ideonomy passes: 0 — a forced absorb-time reconciliation with one minimal
answer.
· Capture: Task 8 plan correction; Task 8 report and retrospective.

#17 [G5] — **Correcting the registered sky-provider description is a deliberate
fixture epoch, not a Repose C0 leak.**
· **What moved:** the source registry now says "the generated astronomy
provider this world uses" rather than describing the retired constant or
generated choice. Its generated reference page and the seed-42 world JSON
therefore changed at that one registry-doc field; no ledger fact, pin, or
serialization ordering changed.
· **Decision:** accept that three-file causal chain in one close commit. The
sanctioned `REBASELINE=1 cargo test -p hornvale --test suite -- lens_purity`
writer regenerated the fixture, then normal `lens_purity` and the independent
Repose byte-identity probe both passed.
· **Why:** the fixture embeds registered concept documentation, so a truthful
fact-registration correction necessarily changes its bytes. Calling it an
unexamined Repose C0 leak would collapse a reviewed source fact and an
unexplained world-identity change into one category; the diff demonstrates the
former only.
· Capture: this ledger entry and the dated retrospective postscript.

#18 [G5] — **The close-time absorption of 163 commits found three prose sites
the campaign's own freshness sweep could not have reached, and one it could.**
· **What was found:** absorbing `main` at G6 (two conflicts, both mechanical)
left a tree that compiled the library but not its tests: The Housemark's
`windows/vessel/tests/suite/housemark_readout.rs`, written on `main` after this
branch deleted `SkyChoice`, still imported the type and passed the retired
fifth argument. `main` also brought a root `AGENTS.md` — a hand-maintained,
name-substituted mirror of `CLAUDE.md` with no generator and no drift check
(The Weft's close, `0efa51c76`) — carrying the pre-retirement tier paragraph
and a stale almanac count in the present tense.
· **Decision:** fix all three on this branch. The test drops the import and the
argument, matching every peer call site in the same directory; `AGENTS.md`
takes the same two edits `CLAUDE.md` already carried.
· **Why:** a campaign that retires a doctrine owes the tree no live sentence
asserting it, and the mirror is a file agents read as instructions. Deferring
the test fix is not available at all — it is a compile error in the merge
product.
· **The fourth site is the one that matters, because the sweep could have
reached it and did not.** `book/src/open-questions.md`'s *Coarse constrains
fine* bet opens by naming `ConstantSun` as a live exemplar; the close had
already asserted in the retrospective that the Confidence Gradient did not
move. It does not move in *confidence*, which is why the original reading was
half right, but decision 0030 owes the chapter a re-score whenever a campaign
moves a bet's substance, and retiring a bet's opening exemplar is that. The row
is now re-scored and the retrospective's claim is corrected in place rather
than quietly dropped.
· **Alternatives discarded:** leaving `AGENTS.md` to its own maintainer (it has
none — the file is copied by hand, so "someone else will" resolves to nobody);
rewriting the bet's opening sentence instead of appending a re-score (the
chapter's convention is append-only re-scores, and the original sentence is
the historical claim being corrected).
· ideonomy passes: 0 — three forced reconciliations and one owed re-score, each
with a single correct answer.
· Capture: this entry; the corrected retrospective section; the re-scored
gradient row.
· **Not fixed, deliberately:** `AGENTS.md` also renders the campaign worktree
path as `.Codex/worktrees/<campaign>/`, an over-eager Claude→Codex substitution
of a real path that does not exist. It predates this campaign, has nothing to
do with the retirement, and belongs to whoever maintains the mirror. Flagged in
the G6 digest rather than fixed here.

#19 [G5] — **The first merge submission went red on formatting in crates this
campaign never edited.**
· **What happened:** `req-7bc26a6c5c44` reddened rc=11 in the chamber's
`clients` phase after 1292 s. Two `cargo fmt --check` diffs, both downstream of
the retired fifth `build_world` argument rather than of any edit:
`clients/world-wasm/src/lib.rs:114` (the call now fits on one line) and
`clients/game/bin/src/driver.rs:91` (the `hornvale_worldgen` import re-wraps
without `SkyChoice`).
· **Decision:** run `cargo fmt` over the four client manifests, then run the
whole `clients` phase locally (`make clients-check-run`, 7 min) before
resubmitting rather than fixing only the two files the log named.
· **Why:** `clients/` is excluded from the cargo workspace, so `gate-commit`'s
`cargo fmt --check` cannot reach it and a green local gate says nothing about
it. Fixing only the named files would have re-tested the same blind spot — the
log reports the first failure per manifest, not all of them.
· ideonomy passes: 0 — a mechanical formatter fix with one correct answer.
· Capture: this entry; the retrospective's absorption section.

## Follow-ups

- **Point the census at a pin axis for the first time.** Adding
  `{"label": "locked", "pins": ["rotation=locked"]}` to
  `census-of-faiths.study.json` would answer the Constitution's eternal-noon
  question properly — 10,000 worlds, a derived regime,
  `pantheon-cyclic-share` as the readout. The mechanism is live and unused
  (#9). Out of scope for The Zenith: it is a measurement decision under 0016,
  not a retirement task. Worth a registry row at close.
