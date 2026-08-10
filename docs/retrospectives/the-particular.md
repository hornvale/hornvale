# The Particular — retrospective

**Completed:** 2026-08-10 (spec
`docs/superpowers/specs/2026-08-01-the-particular-design.md`, plan
`docs/superpowers/plans/2026-08-01-the-particular.md`, five tasks, opened
2026-08-01 and parked for eight days at Task 3). Ran under campaign-autopilot.
Process lessons only; the product is in `book/src/chronicle/the-particular.md`.

## Parking a campaign to fix the cause was right, and it is measurable

Task 3 promoted persons and moved six committed client fixtures — every NPC
identifier in them shifted by exactly the number of persons minted ahead of it,
at identical byte length. That is the signature of a positional identifier doing
a stable identity's job, and the available response was to rebaseline the
fixtures and call the churn expected.

The campaign parked instead. Three campaigns then attacked the cause: one split
the bake's private handle from the ledger's permanent one, one stopped derived
prose from reading an identifier for its value, and one changed the derivation
so an entity's identifier is its lineage rather than its position.

The measurement on unparking is the whole argument for the decision. With
promotion on, against `main` with it off, **all six fixtures are now
byte-identical** — where eight days earlier the same six moved (NPCs 537–543 →
654–660). And the probe still discriminates: `is-person` facts go 0 → **117** in
`cli/tests/fixtures/world-seed-42.json`, corroborated by `next_entity` moving
536 → 653 (a difference of exactly 117), and the world fixture's growth is purely
additive (4,701 insertions, the single deletion being the entity counter). The
same diff that was noise eight days ago is now evidence.

**Generalisable:** when a change's diff is dominated by churn you did not intend,
the cheap move is to rebaseline and the correct move is often to ask what the
churn is evidence *of*. Here it was evidence of a defect three campaigns wide,
and fixing it converted this campaign's own acceptance check from unreadable to
decisive.

## The seven predictions were scored numerically, and only that found the defect

The gate was green at Task 4. Every test passed. The campaign's most important
finding — that one of its four new predicates is committed by no world — was
invisible to all of it, and surfaced only when Task 5 wrote a throwaway probe to
produce the numbers the freeze demanded.

The mechanism is a unit mismatch: the history subsystem keeps time in years,
and promotion subtracts a maturity in *days*, adds a lifespan in *days*, and
compares the result against a present in years. Zero death facts on every seed
where a consistent reading gives 114 / 119 / 127.

Three separate things failed to catch it:

1. **The unit tests are correct and hand-built.** `a_living_founder_gets_no_death_fact`
   constructs both branches directly and asserts correctly on each. Correctness
   of a branch says nothing about its reachability.
2. **The live-world test has the branch and never enters it.**
   `windows/worldgen/tests/person_promotion.rs` walks every promoted person and
   asserts `died > born` inside `if let Some(died)`. On seed 42 the `Some` arm
   is entered zero times. The test is green and vacuous in exactly the half it
   was written for.
3. **The capability probe counts the predicate as held.** Registration is
   supply; production is not measured anywhere.

**Generalisable, and it is the campaign's headline process lesson: a prediction
that names a number must be scored by computing that number, not by observing
that the tests pass.** Everything the suite could tell us was already true.

## The plan text specified the defect

`Years::days()` is named in the plan's own type-consistency notes as "the only
lifespan conversion, and Task 3 uses it". It is the wrong conversion for an axis
kept in years, and the implementer transcribed it faithfully. This is the
project's most-recorded failure mode arriving again: the defect originated in
plan prose, was reviewed for faithful transcription, and passed.

**What would have caught it at plan time:** the plan lists the expression
`birth = founded − age_at_maturity` and separately the fact that `occ-founded` is
in years — the latter carries an explicit doc comment in
`windows/worldgen/src/descent.rs` warning that the "standard day" wording on the
predicate is wrong. Nothing in the plan required naming the unit of each term of
each arithmetic expression it froze. A one-line unit annotation per term would
have made the mismatch visible on the page.

## A preregistered ratio is only as durable as its denominator

P5 froze two clauses: an exact identity (added facts = 4 × cast + deaths) and a
bound (ledger growth ≤ 2.1%). The identity matched to the fact on all three
seeds; the bound failed at 6.25% / 3.57% / 6.72%.

**The identity was confirmed only in its degenerate half, and saying so is the
point.** Its second term — founders already dead at `now` — is **zero on every
seed, because of F9**, so what the measurement tested is `4 × cast` and the
`+ deaths` term was never exercised. The death rule remains unverified against
live data. This is precisely the vacuity shape the campaign is otherwise pleased
to have caught, arriving inside the campaign's own score, and it went unnoticed
in the first draft of both the chronicle and this document: an identity that
reads as corroboration of two mechanisms when one of its terms is identically
zero is corroboration of one.

The cause was not the mechanism. The spec's measured ground was taken on
2026-08-01 — 26,309 facts and 1,776 occupations for seed 42 — and by the time the
prediction was scored the branch had absorbed 992 commits and then 55 more, over
which the history subsystem was rebuilt (habitability became a relation, capacity
became era-dependent). The same seed now carries 7,486 facts and 400 occupations
before promotion. The denominator fell by 3.5× and the numerator rose by a third.

**Generalisable:** a preregistered prediction stated as a fraction of current
state silently re-aims itself whenever that state moves. Two counter-measures,
both cheap: prefer identities over ratios where the mechanism admits one, and —
when a campaign absorbs main mid-flight — re-measure the freeze's stated ground
in the same session and record whether it still holds. Nothing mechanical does
this today.

## P7's stated figure was wrong before the campaign started

"The cast spans all five peoples" was frozen against a five-people roster. All
three seeds carry **nine** peoples with occupations. The substance of the
prediction — every people present, per-people count exactly `min(depth,
occupations)` — is confirmed on every people on every seed; only the cardinality
in the sentence was wrong, and it was wrong for the same reason P5's bound was.

## The mechanism the campaign existed to give a consumer still has none

The spec's opening argument is that `domains/history::flesh::persona_of` is a
shipped derive-on-demand person whose only call site in the repository is its own
unit test. That is still true. The campaign added `founder_handle` beside it and
consumed *that*; names are drawn through `Namer` at genesis, because a persona's
name needs a per-species phonology the derive-on-demand path does not have.

Not a defect — the naming decision is argued and correct — but the campaign's
stated motivation was retired without being satisfied, and the retirement was not
noticed until the close. **Generalisable:** when a spec's "what already exists"
section is load-bearing, re-read it at close and check the claim is still the
claim you shipped against.

## Freshness sweep: the null, stated

**No Confidence Gradient bet moved, and none was re-scored** (decision 0030).
That is the campaign's own prediction rather than an omission: P2 froze
"stageable stays 0 of 36" and it did, so the campaign added *representability* —
a person can be the subject of a fact — and nothing in the gradient's tiers is
scored on representability alone. What did move is the chapter's *floor* thread,
which gained two paragraphs: a ninth instance of the check-that-cannot-fire
family in a new position (a **fact** that cannot be committed), and the
preregistration lesson that a bound stated as a fraction of current state
re-aims itself whenever that state moves.

Recorded here rather than only in the campaign's scratch, because that scratch
is git-ignored and dies with the worktree — a null that lives only there is a
null nobody can later check was considered.

---

## Promoted followups

The campaign's own scratch ledger is not durable, so everything it held is
recorded here.

### From the spec's §7 (found on the way, F1–F8)

**F1 — `occ-notability` and `occ-function` are constants, and the consequence is
silent presentation collapse.** `windows/worldgen/src/history_bake.rs`'s
`Bake::open` — the sole constructor of every occupation record — hardcodes
`Notability::Common` and `Function::Agrarian`. Measured `distinct = 1` for both,
on three seeds, all 5,451 occupations; 13.5% of the seed-42 ledger carrying no
information. What it strands, verified by grep: `flesh.rs`'s `ResidueItem::Reliquary`,
`Bauble` and `Inscription` (unreachable in every world); a second dead branch at
`flesh.rs:316`; `windows/almanac/src/history.rs`'s `notability_phrase`, which
returns "an ordinary place, neither famed nor forgotten" for **every settlement
in every world** while "a backwater at the region's edge" and "a regional seat of
power" are authored and unreachable; `windows/vessel/src/interior/pattern.rs`,
where chamber index 2 matches `Role::Hall` first and never fires, so **the third
room of every multi-chamber building in every world is a Loomroom**; and
`vestige.rs`'s documented undercity/ruin split that was never built. The
derivation was promised by The Living Community's design and no task built it:
this is a missing task, not a missed step. Three campaigns built consumer logic
on top over twelve days.

**F2 — no ledger-size ratchet exists anywhere.** `scene_cost.rs` and
`graph_cost.rs` gate wall time; `scene_cost.rs` computes scene bytes and only
asserts `> 0`. Nothing gates `world.json` size or `facts.len()`. This campaign
was the first to deliberately grow the save, and its own size bound was
falsified with nothing to catch it — see the ratio lesson above. A ratchet on
fact count per seed would have made P5's failure a red gate rather than a
close-time discovery.

**F3 — the history domain is calibrated for populations an order of magnitude
above what demography produces.** `HAMLET_POPULATION_CEILING = 150` is never
exceeded (max 127 / 90 / 119), so `hamlet_scale` is always true;
`LONGHOUSE_POPULATION_FLOOR = 200` is unreachable, so **every dwelling in every
world is a `Hut`**. Every settlement in Hornvale is, in the history domain's own
vocabulary, a hamlet. The `RoleHandle` doc comment's illustrative example — "the
chieftain who led the flight of 312" — describes a migration larger than any
settlement that has ever existed. Whether a 127-person maximum is intended is a
demography question, and it bears directly on how much persons are worth.

**F4 — a lexicon word satisfies a capability token.** `concept:person` is held
because `domains/language` registers a *word* for person. The probe forms
`concept:{name}` over every `ConceptDef` regardless of `kind` or `domain`, so it
cannot distinguish a modelled thing from vocabulary. The Supply section already
annotates `concept:` orphans with their owning domain for exactly this reason;
the **demand** side does not. Candidate fix for whichever campaign next touches
the probe: require a `kind`/`domain` qualifier in a corpus's `concept:` tokens.
Do not retrofit it onto the frozen Polti corpus.

**F5 — `EntityId` as positional identity, both tiers.** RESOLVED during the park
by The Scaffold, The Salt and The Signet. Recorded closed, with the unparking
measurement above as its evidence.

**F6 — a hand-built fixture proves correctness, never reachability.** Every
consumer of the constant fields has a unit test that hand-builds
`Notability::Seat` or `Function::Cult` and asserts correct handling. Every such
test passes; every such branch is unreachable. Clippy cannot see it — the code is
reachable by *type*, just not by *data*. **This campaign produced a third
instance of the shape and the sharpest one yet** (`person-died`, above), so the
candidate guard is now overdue: for any enum whose variants gate observable
output, assert every variant is produced by *some* world; the census already
builds ~2,000 worlds, and a metric counting distinct values per categorical field
would have caught F1 the day it landed. `distinct == 1` on a field with three or
more variants is the signature. **Widen the guard to cover predicates as well as
enum variants** — a registered predicate that no world commits is the same defect
in the vocabulary rather than in a field. Variant and predicate reachability are
default-*allow*, where the type audit and the trope ratchet are both default-deny.

**F7 — three instances of capability without a consumer.** `persona_of` (still
consumerless after this campaign — see above), the `Notability`/`Function`
variants (F1), and now `person-died` (F6). The pattern is that a mechanism ships
with tests, is documented as available, and is never reached; nothing in the gate
distinguishes "available" from "used".

**F8 — resolved, folded into the design during spec review** (the almanac needs a
founder's handle and the ledger does not carry it; settled by putting
`founder_handle` in `domains/history::flesh` beside `persona_of`, so both worldgen
and the almanac reach one derivation and nothing derived enters the save format).

### New, from this campaign's execution and close

**F9 — the years/days unit mismatch in `windows/worldgen/src/person_promote.rs`.**
`birth_day = f.founded - maturity_days` mixes a year-axis founding with a
day-valued maturity; `death` compounds it and is filtered against a year-valued
`now`. Consequences: every committed `person-born` value is off-axis, and
`person-died` is unreachable. **Do not fix this in isolation** — the correction
moves every promoted birth day in every world and turns on a fifth fact per
founder, so it is a save-shape change that needs its own campaign, its own
fixture regeneration, and the byte-golden discipline. The wider question it
raises is worth more than the fix: `occ-founded` is documented as "the standard
day founded" and is in fact years, with the correction living only in a doc
comment on `descent.rs::founded_year`. **One of the two should move.**

**F10 — `windows/worldgen/tests/person_promotion.rs` is half-vacuous.** Its
`if let Some(died)` arm is entered zero times on seed 42. Whoever fixes F9 should
turn the conditional into an assertion that *some* person in the world carries a
death fact, so the test can never silently lose its subject again.

**F11 — a count travelled through two documents without ever being re-derived,
and it was wrong.** The dispatch briefing this task quoted `is-person` facts
going 0 → 119 in the seed-42 fixture world. The true figure is **117**:
`next_entity` moves 536 → 653, a difference of exactly 117, and the fixture
holds 117 such facts. The error was the controller's, not the implementer's —
it came from counting string occurrences rather than facts — and it was
reproduced verbatim into the first draft of this retrospective by an
implementer who had independently measured 117 in the same session and did not
reconcile the two. **Both halves are the lesson**: a count in a handoff is an
assertion, not a datum, and a figure you have measured yourself should win over
one you were handed. Re-derive counts at the point of use, and notice when your
own measurement disagrees with the brief.

**F12 — the spec's §4 "Measured ground" table is stale and nothing marks it.**
Every figure in it (26,309 facts, 1,776 occupations, 5 peoples, cast 90) was
superseded by absorptions during the park. A spec is append-only in practice but
its measured-ground tables are read as current. Candidate: date-stamp measured
tables in specs, or state the commit they were taken at, so a reader can see the
claim's age without archaeology.

**F13 — the plan's Task 4 brief contained two defects the implementer had to
correct in flight**: a snippet reading `r.community` (a field The Scaffold had
deleted; the occupation's own entity is `r.id`), and a specified test that was
impossible because `hornvale-almanac` cannot depend on `hornvale-worldgen`
without a cycle. Both were caught and both were repaired well — the replacement
test was proved non-vacuous by mutation — but both were armchair-authored against
a tree the author had not compiled against.
