# The Zenith — retiring the provider-tier system

*"A golden sun hangs fixed at zenith. It has never been seen to move."*
That sentence is the entire content of Hornvale's tier-0 astronomy. This
campaign deletes it, and with it the last surviving instance of the
constitutional provider-tier doctrine.

Decision ledger: [`2026-09-04-the-zenith.md`](../ledgers/2026-09-04-the-zenith.md).
Decision block: **0736–0745**.
Origin: `SKY-retire-the-tier-system`, filed by The Wash
([ledger #15](../ledgers/2026-09-03-the-wash.md)) on Nathan's call, 2026-09-03,
and deliberately not folded into that campaign.

---

## 1. What the tier actually is

The Constitution (§2) permits multiple **tiers of the same truth** to
coexist: *"higher fidelity refines, never contradicts, lower — coarse
constrains fine."* Decision
[0039](../../decisions/0039-epochs-replace-tiers-refine.md) already
half-retired the doctrine, refusing to let terrain v1 and v2 coexist as tiers
on the grounds that a generator which **contradicts** its predecessor is an
*epoch*, not a tier. It named astronomy as the one surviving licence for
coexistence, and cited it as the doctrine's exemplar:

> `ConstantSun` and the generated sky are both valid because the generated
> sky *refines* the constant one; every fact the coarse tier asserts, the
> fine tier preserves. That is the licence for coexistence.

Astronomy is now the only place tiers survive at all.

**Tier-0's definitional content is acyclicity.** Not "low fidelity" — the
constant sun is not a coarse *measurement* of anything, it is a stipulation
that there are no cycles: no day, no year, no phases, no eclipses, no
calendar. Every `Option` in the downstream API is guarding that one property.
`Sky::calendar()` returns `Option` because a tier-0 world has no calendar;
`Sky::system()` returns `Option` because it has no star system; ten
production `let … else` guards exist to handle the acyclic case.

So the retirement is not "delete a code path". Stated precisely, it is:

> **Every Hornvale world has a sky with a calendar and a star system.**

That is the invariant being added, and naming it that way is what makes the
rest of this spec decidable.

## 2. Why the tier can go: acyclicity is already representable

The obvious objection to §1 is that acyclicity is a real physical situation
and deleting the only world that exhibits it loses expressive range. It does
not, and the repository already contains the proof.

`--rotation locked` produces a tidally locked world whose sun does not move
across the sky and whose calendar has **no day at all**. The committed
artifact says so in its own words
(`book/src/gallery/almanac-seed-42-locked.md`):

> This world is tidally locked: no local day exists; the year is 368.1
> standard days.

And `tier_refinement.rs` — the battery this campaign retires — already
asserts the equivalence directly:

```rust
None => assert_eq!(
    sun.period_days, None,
    "seed {seed}: a locked sun is aperiodic, like tier 0's"
),
```

Tier-0 is a **degenerate case of a regime the generated sky already covers**,
with the difference that the locked world is *derived from physics* while
tier-0 is *stipulated*. `Calendar::day_length()` keeps its `Option` and keeps
carrying the real acyclicity; only the fake one goes.

### 2.1 The ladder was never a ladder

The Constitution describes the doctrine as a four-rung ladder, using astronomy
as its illustration:

> - Tier 0 `constant-sun`: the sun is always up (Zork).
> - Tier 1: 24-hour day/night cycle with visibility effects.
> - Tier 2: seasons, moon phases, derived calendar.
> - Tier 3: realistic multi-body configurations, constellations as perceived
>   from the surface, eclipses, wanderers, rings.

Every one of tiers 1, 2 and 3 shipped — and all three shipped **inside a
single provider**. `GeneratedSky` produces day/night and visibility, seasons
and moon phases and a derived calendar, and multi-body configurations with
constellations, eclipses, wanderers and rings. There is no tier-1 type, no
tier-2 type, no tier-3 type; there is `ConstantSun` and there is
`GeneratedSky`.

So the doctrine never had four instances in astronomy. It had two, and one of
them was the pre-existing stub. Retiring tier-0 does not collapse a ladder —
it removes the last rung of a ladder that was only ever one rung tall, and
the coexistence licence 0039 preserved was protecting a stipulation, not a
fidelity level.

### 2.2 The Constitution's own question is a census question

The Constitution gives tier-0 a purpose beyond cheapness, and an earlier
draft of this spec treated it as the strongest argument *against* the
campaign:

> A trivial provider's sparse answer is itself meaningful input downstream
> (what religion develops under an eternal noon?).

**That draft was wrong about the instrument, and Nathan's correction is the
one that settles it: this is a census question, not a tier question.** The
difference is not stylistic. Tier-0 can only ever answer it with **one
stipulated world** — the seed-42 comparison in §4.2, 16 beliefs against 145.
That is an anecdote, and it is an anecdote about a *stipulation*: the eternal
noon is not physics, so whatever religion develops under it is an artifact of
the stub rather than a finding about worlds.

The lab answers the same question with a population under a *derived*
regime, and every piece of the machinery already exists:

- **The pin-set axis is live, not vestigial.** `windows/lab/src/study.rs`'s
  `PinSet { label, pins, roster }` is iterated by `runner.rs:155`, which
  builds each set with its pins and emits a `pin_set` column into the CSV
  (`runner.rs:304`); `chart.rs` renders per set. `PinSet::label`'s own doc
  comment gives its example as — exactly — `"default"`, `"locked"`.
- **The metrics are already registered.** `pantheon-size`,
  `pantheon-verticality`, `belief-kind-*`, and above all
  **`pantheon-cyclic-share`** — the share of a pantheon derived from cyclic
  phenomena, which *is* the eternal-noon question stated as a number.
- **The study already exists at scale.** `census-of-faiths` runs pantheon
  size, cult form, verticality and head-deity periodicity over **10,000
  worlds**.

So the comparison the Constitution asked for is available at 10,000-world
scale against a real physical pin, and tier-0 was the strictly worse
instrument for it.

**What is actually true, and is a finding rather than a cost:** *no committed
study in the repository uses a non-empty pin set.* Every `pin_sets` array in
`studies/*.study.json` is `[{"label": "default", "pins": []}]`. The census
sweeps **seeds** and has never once swept **pins**, so the machinery built
for this question has never been pointed at it. Retiring tier-0 therefore
removes an anecdote nobody was consuming; it does not remove a capability.

Captured as a follow-up rather than folded in: adding a `{"label": "locked",
"pins": ["rotation=locked"]}` set to `census-of-faiths` is a one-line change
that would answer the Constitution's question properly for the first time.
It is **out of scope here** — it moves a census study, which is a
measurement decision under decision 0016's preregistration discipline and
belongs to its own campaign, not to a retirement.

## 3. Why the keystone fixture is tier-0 — the question The Wash left open

The Wash's ledger flagged one thing its successor owed an answer to:

> **Open question the successor spec owes an answer to:** why the keystone
> fixture is tier-0 at all. It looks deliberate and I did not find its
> reason. If it was chosen so the keystone stayed stable while astronomy
> churned, the retirement has to answer that.

**It was not deliberate.** `book/src/gallery/almanac-seed-42.md` was the only
almanac that existed before the sky campaign. Commit `0779d870c` (2026-07-06,
*"feat(cli): sky pins as flags, generated-sky default, and the scout verb"*)
flipped `hornvale new`'s default from constant to generated, and in the same
commit added `--sky constant` to CI's seed-42 line — a one-line change to
`.github/workflows/ci.yml` whose entire purpose was to keep the pre-existing
committed artifact byte-identical across the default flip.
`almanac-seed-42-sky.md` was added later the same day (`03f0e577a`) as the
*new* artifact for the *new* default.

The hypothesis The Wash raised — insulating the keystone from astronomy churn
— is false. The pin is an artifact-stability freeze from a single campaign
transition, carried by inertia for two months. Nothing depends on it.

**And the byte-golden keystones were never tier-0 in the first place.** Both
committed world fixtures carry `"sky-provider": "generated"`:

```
cli/tests/fixtures/world-seed-42.json           21,728 facts   generated
cli/tests/fixtures/pre-branches-seed-42-world.json   714 facts   generated
```

No identity keystone retires under 0039's clause, because no identity
keystone was frozen on a tier-0 world.

## 4. What is actually being changed

### 4.1 The measured scope

`SKY-retire-the-tier-system` estimates "six production files". That is an
accurate count of the files that *handle* `Sky::Constant`. It is not the size
of the change, because `SkyChoice` is a positional parameter on `build_world`
— the most-called function in the workspace. Measured on `main` at
`d1895029c`:

```
SkyChoice::Generated sites               382     (372 plain argument form, 10 not)
SkyChoice::Constant sites                 23
                                        ----
SkyChoice sites total                    405
build_world / build_world_to call sites  335
Sky::Constant / ConstantSun sites         57     across 17 files
```

### 4.2 Tier-0 is a different world, not a cheaper one

This is the fact that shapes the whole plan. Paired timings, Mac, debug
profile, n=5, spread ±1%:

```
                 wall      facts      beliefs    calendar-derived facts
constant        0.86 s    21,008         16      0
generated       2.62 s    21,728        145      390  (founding-solstice-azimuth)
```

3.05x and +1.76 s per world — but the fact composition is the point. Nine
times the beliefs, and a predicate that cannot exist without a calendar.
Every constant-sky test site is asserting against the left column. Moving one
to the right column is a **change of subject**, not a re-timing, and a change
of subject is exactly what disappears inside a 405-site diff.

### 4.3 The four kinds of site

The 23 constant-sky sites do not share one treatment. Sorted by what the tier
is *doing* at each:

```
 kind  what the tier is there for                treatment
------------------------------------------------------------------------
  A    the tier IS the subject                   RETIRE the test
  B    incidental cheap-world scaffolding        SUBSTITUTE the world source
  C    an enumerated axis                        HALVE the enumeration
  D    the cross-tier battery's anchor           PROMOTE to sky-conformance
  E    surface: CLI flag, wasm ABI, labels       DELETE the surface
------------------------------------------------------------------------
```

**The table deliberately carries no count column, and the reason is a defect
this spec caught in its own first draft.** That draft sized each kind — `A ~11,
B ~9, C 2, D 4, E 6` — by mixing two different populations: the 23
`SkyChoice::Constant` sites and the 57 `Sky::Constant`/`ConstantSun` sites,
which overlap by file but not by line. The tallies summed to 32 against a
population of 23 and looked entirely plausible. Only three kinds have a count
that was actually verified against a single population:

- **C = 2** — `pin_enumeration.rs:51` (the module doc's claim) and `:66` (the
  array literal).
- **D = 4** — the four tests in `tier_refinement.rs`.
- **E = 6** — `clients/world-wasm/src/lib.rs:95`, `cli/src/main.rs:240`
  (the flag) and `:2554` (its test), `windows/worldgen/src/lib.rs:8136` (the
  label map), plus the `--sky` usage line and `parse_sky_args`' constant arm.

**The A/B split is not sized here on purpose.** Whether a given site's subject
*is* the tier or merely uses it is a reading of that site's assertions, and
this spec is written from outside the code. Stage 2 determines it per site and
reports the split. What the spec commits to is that the **kinds are
exhaustive** — every affected site is one of these five.

**Kind A — the tier is the subject.** These tests assert tier-0-specific
behaviour and retire with it, the way 0039 retires a keystone at an epoch:
their evidentiary job was completed at their own campaign's merge. Examples:
`astronomy::the_sky_never_changes`,
`worldgen::absent_sky_provider_fact_falls_back_to_constant`,
`worldgen::sky_calendar_accessor_present_for_generated_absent_for_constant`,
`scene::system_scene_errors_on_a_constant_sun`,
`main::star_chart_on_a_constant_sun_world_is_a_loud_error`,
`exit_criterion::repl_answers_sky_village_and_belief` (campaign 1b's exit
criterion, asserting the word "zenith").

**The trap kind A carries, and it is the campaign's sharpest.** Several of
these tests exist to exercise an **error branch** whose only producer is
tier-0. Deleting the producer without deleting the branch manufactures live,
unreachable, untested code that reads as a guard. The rule is unconditional:

> If retiring a kind-A test leaves a production branch with no reachable
> producer, **delete the branch too**. Never leave it as `unreachable!()`,
> never leave a `None` arm that nothing can now return.

**Kind B — incidental scaffolding.** The test wants *a world*; the tier was
how it got one cheaply. §4.4 gives its decision rule.

**Kind C — an enumerated axis.** `pin_enumeration` enumerates
`SkyChoice × Rotation × Neighbor` = 48 combinations, of which 24 are
constant. After the retirement it enumerates 24, all generated. Its module
doc's own claim — *"Half the product (`SkyChoice::Constant`, 24 combinations)
is refusal-free by construction"* — describes the half being deleted and must
be rewritten, not merely renumbered. Cost is expected to fall or hold (24
generated builds replace 24 generated + 24 constant), and the tier is
depth-scoped to `BuildDepth::Terrain`; the plan measures rather than predicts.

**Kind D — the battery.** See §4.5.

**Kind E — surface.** `hornvale new --sky <choice>` and its usage line; the
wasm ABI's `"sky"` pin key (`clients/world-wasm/src/lib.rs`); the
`SkyChoice → "constant"|"generated"` label map; `parse_sky_args`'s constant
arm and its tests.

### 4.4 The decision rule for kind B — stated as branches, not a prediction

`windows/worldgen/src/fixture.rs::seed_42_world()` already reads the
committed 5.5 MB `cli/tests/fixtures/world-seed-42.json` from disk instead of
rebuilding it, and its own doc comment states it is byte-identical to
`build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated,
&TerrainPins::default(), &SettlementPins::default())` — exactly the call a
flipped site would otherwise make. It is used at **56 sites across 27 files**
today, so it is an established pattern, not a seam invented here.

For each kind-B site, in order:

- **The site asserts about the build path itself** (it compares a built world
  against the committed fixture, or exercises `build_world`'s own behaviour)
  → **build**, `SkyChoice::Generated`. Substituting the fixture would make it
  compare the fixture against itself. `repose_byte_identity` is the known
  instance; there may be others.
- **The site needs seed 42 under default pins**, and does not assert about
  the build path → **`fixture::seed_42_world()`**. This is *cheaper than the
  constant-sky build it replaces* — a file read against 0.86 s — so these
  sites get faster, not slower.
- **The site needs any other seed or non-default pins** → **build**,
  `SkyChoice::Generated`, and pay the +1.76 s. Eight sites use `Seed(0)`,
  `Seed(1)`, or a loop variable.
- **In every case**: run the test after the flip and read what it says. An
  assertion tuned to 16 beliefs does not survive a 145-belief world silently
  — it fails, loudly, which is the intended behaviour of this ordering.

The plan does not assign the 23 sites from outside the code. It states this
rule; the implementer applies it per site after reading, and reports any site
where the rule does not decide.

### 4.5 The battery: promote, do not "re-express"

The Wash called `domains/astronomy/tests/suite/tier_refinement.rs` "the one
real loss", whose invariants "must be re-expressed as direct assertions".
Read in full, it is a smaller loss than that implies.

**Two of the four tests never mention `ConstantSun` at all.**
`refinement_adds_structure_only_beneath_the_sun` and
`the_suns_added_period_is_the_day_the_calendar_already_holds` are already
direct assertions over the generated sky, and need only their prose changed.

The other two use `ConstantSun` solely to **source two literals**:

```rust
let tier0 = ConstantSun.phenomena(&ctx(0.0));   // -> kind CELESTIAL_BODY, salience 1.0
let coarse = ConstantSun.sky_at(WorldTime::GENESIS);  // -> bodies == ["the sun"]
```

Freezing those literals in the test is **strictly stronger than the status
quo**, because today the battery derives its expectation by calling
production code inside the crate under test. The file is renamed and its
claim restated: it stops asserting *"the fine tier refines the coarse one"*
and starts asserting *"this is what a Hornvale sky is"* — one top-salience
day-sky sun, nothing outranking it, a period equal to the calendar's own day
or none at all, the sun never retracted from the visible bodies. That was
always the claim doing the work; the tier was only its carrier.

The four `claim: sanctioned-sweep(...)` tags are preserved verbatim — the
sweep's cost and census-homelessness are unchanged by the rename.

### 4.6 The gallery: the contrast chapter gets better

`$w42` (tier-0) has exactly **one** reader in
`scripts/regenerate-artifacts.sh`:

```bash
spawn run -p hornvale -- new --seed 42 --sky constant --out "$w42"
spawn run -p hornvale -- almanac --world "$w42" > book/src/gallery/almanac-seed-42.md
```

Dropping `--sky constant` makes `$w42` and `$wsky` the same world, so
`almanac-seed-42.md` and `almanac-seed-42-sky.md` become byte-identical
files. This is structural, not a prediction: the two `new` lines differ only
in that flag.

**Resolution — the world builder loses `$w42`; `almanac-seed-42.md` becomes
the generated spinning almanac; `almanac-seed-42-sky.md` is deleted.** The
`-sky` suffix only ever meant "not the tier-0 one", and names nothing now.
Then:

- `book/src/gallery/almanac.md` — includes `almanac-seed-42.md`, as it does
  today. Its framing changes: it is no longer "Campaign 1b's exit artifact"
  (that artifact was the tier-0 world, and it is gone).
- `book/src/gallery/the-sky.md` — includes `almanac-seed-42-locked.md` only,
  and links to `almanac.md` for the spinning one. It stops including the
  spinning almanac inline, which would otherwise print the same page twice in
  one book.

The chapter's pedagogy — *"one pinned cause, one legible downstream
difference"* — survives and sharpens: the contrast becomes **spinning vs
locked**, two real physical regimes, instead of a real regime against a
stipulation. Three inbound links in `the-gods-seed-42.md`,
`the-meeting-seed-42.md` and `chronicle/campaign-y2-0.md` are repointed.

## 5. Stages

Order is load-bearing. Each stage compiles, passes `gate-commit`, and commits.

**Stage 1 — the artifact.** Drop `--sky constant` from
`regenerate-artifacts.sh`; collapse `$w42` into `$wsky`; rebaseline; resolve
the gallery per §4.6; repoint the three inbound links.
*Precondition:* none. *Postcondition:* one seed-42 spinning almanac; the book
builds; `git diff --exit-code` over `docs/generated-paths.txt` is clean.
*Failure mode:* an artifact outside `book/src/gallery/` moves. **Branch
table:** only `book/src/gallery/` and `docs/audits/` moved → regenerate and
commit together; `cli/tests/fixtures/` moved → **STOP**, the byte goldens are
generated-sky already (§3) and must not move here; anything else moved →
**STOP** and report before committing.

**Stage 2 — the 23 sites, tier still present.** Apply §4.4's rule and §4.3's
kind table. `SkyChoice` still exists throughout; every diff hunk is a
deliberate semantic decision.
*Precondition:* stage 1 landed. *Postcondition:* zero `SkyChoice::Constant`
sites outside kind E's surface; the full suite green.
*Failure mode:* an assertion tuned to the tier-0 world. That is the expected,
wanted signal — fix the assertion against the generated world and say so in
the task report; do not reach for the fixture to make a red go away.

**Stage 3 — delete the type and the surface.** Delete `ConstantSun`,
`Sky::Constant`, `SkyChoice`, the `--sky` flag and its usage line, the wasm
`"sky"` pin key, and the label map. Remove the parameter from `build_world`
and `build_world_to` at all 335 sites.
*Precondition:* stage 2 landed; **verified** that no `_ =>` wildcard arm
exists on any `Sky`/`SkyChoice` match (checked on `main`: none in
`windows/worldgen/src/lib.rs`), so the compiler enumerates the work.
*Postcondition:* `cargo check --workspace --all-targets` clean.
*Failure mode:* a scripted rewrite corrupting the 10 non-argument
`SkyChoice::Generated` sites (§4.1). **The edit is compiler-driven, never
`sed`/`rg -r`** — 372 of 382 are the plain trailing-comma form and 10 are
not, so a regex sweep leaves a plausible, wrong tree.

**Stage 4 — collapse the Option-ness.** `Sky::calendar()` and `Sky::system()`
become non-`Option`. Every `let … else`, `and_then`, and `if let Some` guard
that existed only for the acyclic case is **deleted**, not stubbed. `sky_of`
errors on an absent `sky-provider` fact (ledger #2). The battery is promoted
per §4.5.
*Precondition:* stage 3 landed. *Postcondition:* no production code branches
on the absence of a calendar or a star system.
*Failure mode:* a guard left behind as `unreachable!()` or a retained `None`
arm — see §4.3's unconditional rule.

**Stage 5 — close.** Decision records; chronicle; retrospective; the
registry row's **Where** cell corrected; book freshness sweep including a
Confidence Gradient re-score if any bet moved.

## 6. Out of scope

- **Re-deriving the tier ladder as real fidelity levels.** §2.1 shows tiers
  1–3 shipped as one provider. Splitting `GeneratedSky` into genuine rungs is
  a plausible future campaign and is not this one. (Amending the
  Constitution's *prose* to match reality is **in** scope — see §9's DoD; it
  was listed here in an earlier draft, which contradicted the DoD.)
- **`Calendar::day_length()`'s `Option`.** It carries real acyclicity (§2)
  and is untouched.
- **Terrain and crust "tiers".** 0039 already resolved those as epochs, and
  0038's crust *fields sampled at different resolutions* are a genuine
  fidelity ladder that this campaign does not reach.
- **The wasm catalog version.** The `"sky"` pin key is removed from the ABI;
  whether that warrants a catalog version bump is stage 3's call, noting that
  decision 0356 retired the external consumers, so no cross-repo contract
  binds it.

## 7. Decisions to ratify

- **0736** — the provider-tier doctrine is retired; astronomy was its last
  instance. Records that 0039's carve-out ("tier coexistence remains correct
  for genuine fidelity levels of one truth — the sky tiers") is now empty,
  and why acyclicity is not a lost capability (§2).
- **0737** — `sky_of` errors on a world with no `sky-provider` fact, on
  0189's precedent: a world is a seed plus a ledger, and no compatibility
  shim is added.
- **0738** — a cross-tier refinement battery, on retirement of its coarse
  tier, is **promoted to a conformance battery over the surviving provider**,
  with its expectations frozen as literals rather than sourced from the code
  under test. Generalizes past astronomy.

## 8. Risks

| risk | why it bites | mitigation |
| --- | --- | --- |
| An unreachable guard survives stage 4 | reads as a live check, can never fire, and the committed tree looks healthy | §4.3's unconditional delete-the-branch rule; stage 4's postcondition is stated as *no production code branches on absence* |
| A kind-B flip silently weakens an assertion | a test that passed on 16 beliefs may pass vacuously on 145 | stage 2 runs before the parameter is deleted, so each flip is a reviewable hunk; the task report names any assertion it had to change |
| The 405-site edit corrupts a non-argument site | 10 of 382 are match arms, comparisons, and an array literal | compiler-driven only; §5 stage 3 forbids regex |
| Stage 1 moves an artifact nobody expected | drift checks are silently vacuous against untracked paths | stage 1's branch table stops on any movement outside the two expected directories |
| The heavy tier's `pin_enumeration` cost moves unexpectedly | its combination count halves but each survivor gets more expensive | measured at stage 2, not predicted; the module doc's stale half-the-product claim is rewritten |
| Rewriting `domains/CLAUDE.md`'s tier section deletes a live terrain discipline | its `## Provider tiers coexist` section closes by citing `terrain/crust.rs`'s `strongest`/`None`-branch comments as *"code kept instruction-for-instruction unperturbed to protect a byte-identity contract"* — a **terrain** rule that has nothing to do with astronomy tiers and must survive the paragraph that houses it | the sweep rewrites the tier framing and **relocates**, never drops, the byte-identity sentence; stage 5 verifies by grepping for it |

## 9. Definition of Done

Beyond the standard close (chronicle entry, retrospective, book freshness
sweep, decision records):

- Zero occurrences of `ConstantSun`, `Sky::Constant`, or `SkyChoice` in
  tracked `.rs` files — asserted, not eyeballed.
- `Sky::calendar()` and `Sky::system()` return non-`Option`.
- One seed-42 spinning almanac in the gallery; no `-sky` suffixed artifact.
- The promoted battery is green and its `sanctioned-sweep` tags intact.
- `SKY-retire-the-tier-system`'s registry row moves to `shipped`, with its
  **Where** cell corrected: the row's "six production files" was wrong (§4.1),
  and the row should say so rather than be quietly deleted.
- **The root `CLAUDE.md` no longer asserts the doctrine as live.** It
  currently states, under the trace-protocol section:

  > **Provider tiers coexist:** the tier-0 `ConstantSun` and the generated
  > star system are both valid; worlds choose. Higher fidelity refines, never
  > contradicts, lower ("coarse constrains fine").

  That paragraph is this campaign's subject and must be rewritten, not
  deleted silently — a reader who finds the sentence gone learns nothing,
  while a reader who finds it corrected learns that tiers were tried, that
  0039 halved the doctrine and 0736 closed it, and that acyclicity survives
  as `--rotation locked`. `domains/CLAUDE.md` carries a whole
  `## Provider tiers coexist` section and gets the same treatment, under
  the risks table's caveat about its closing terrain sentence. And the
  root file's generated-artifact block (~line 665) says the script builds
  **"three seed-42 almanacs"**; after §4.6 it builds two — a stale count
  that `grep 'three seed-42'` will **not** find, because the phrase wraps
  across a line break in that comment block.
- **The Constitution's §2 tier paragraph** (`docs/superpowers/specs/
  2026-07-05-hornvale-longterm-plan-design.md`) is reconciled with 0736.
  The spec governs where it and `CLAUDE.md` disagree, so leaving it stating a
  doctrine `CLAUDE.md` has retired would invert the precedence rule.
