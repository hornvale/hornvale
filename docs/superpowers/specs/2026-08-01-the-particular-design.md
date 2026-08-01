# The Particular — design

A *particular* is the term of art for an individual as against a universal.
This campaign gives Hornvale its first ones: people who can be the subject of a
fact.

## 1. Why

The Repertoire measured Hornvale against Polti's 36 dramatic situations and
returned **0 of 36 stageable**, every one of them blocked on
`bundle:individual-persons` — fan-in 35 over blocked situations, 36
corpus-wide. It is the only bundle every situation requires, and its four
tokens are `concept:person`, `predicate:is-person`, `predicate:person-born`,
`predicate:person-died`. Three of the four are unregistered; the fourth is held
for a reason that turns out to matter, and D4a is about that.

The gap is not that Hornvale lacks people. It has 1,776 occupation records per
world, populations, kinship vocabulary, and a shipped mechanism for expanding a
role into a person. What it lacks is a person who can appear in the trace
protocol. `domains/history::flesh::RoleHandle` is a bare `u64`; a `Fact.subject`
is an `EntityId`. A handle cannot be a subject, so no situation can be staged.

## 2. What already exists

**The derive-on-demand person is shipped and has no consumer.**
`persona_of(handle: RoleHandle, seed: Seed) -> Persona`
(`domains/history/src/flesh.rs:46-63`) is pure splitmix bit arithmetic; its own
doc states "No `Stream` is drawn — this is bit arithmetic over the arguments
alone." Its **only call site in the entire repository is its own unit test.**
No window constructs a handle; `windows/almanac/src/history.rs` renders
"settlers" from `occ-people`, a collective noun. `SOC-10` is `shipped` and
claims history is "written with role-handles, not full individuals" — that
describes an intent no code fulfils.

**The Constitution already licenses the pattern.** §3.1.6: *"Agents are LOD'd
like everything else. An unobserved dragon is nothing but its field
contributions plus occasional statistically-generated ledger events; an
observed dragon is a full agent... There is no always-on agent simulation."*
And, of a named individual: *"Once a goblin king has a name, he has it
forever."*

**Population is already field-not-entity by name.** Decision 0047 split
`domains/demography` out of settlement on exactly that principle:
*"population-as-field is a distinct object from named-places."* Settlements are
already salience-bounded condensations of a population field. Persons are the
same move one level down.

## 3. Design decisions

**D1 — A person is a promoted `RoleHandle`, not a fresh draw.** Identity is the
handle; the `EntityId` is only an address so the trace protocol has something to
point at. This satisfies decision 0051 (*a procedural name's seed must never be
derived from `EntityId` or mint order*) **by construction rather than by care**:
`persona_of` salts from `handle ^ seed`, so a name is stable under any mint-order
churn.

**D2 — Memory belongs to a people, and its capacity is
`MEMORY_DEPTH: usize = 20` founders per people.** Not a world-level constant, and
not a ratio.

```
cast = Σ over peoples: min(MEMORY_DEPTH, occupations of that people)
```

Within a people, rank by `(peak_population DESC, site ASC, founded ASC)` — a
total order over `u32` and structural keys, with no float comparison anywhere.

**Why per-people rather than per-world.** A world-level "top 100" implies an
omniscient rememberer — which is nobody. Three things follow from giving memory a
holder that a world constant cannot give:

1. **Memory acquires a subject.** A founder remembered by gnolls and unknown to
   kobolds is a fact *about knowledge*, which is the seam the `KNOW-*` cluster
   needs later.
2. **The cast concentrates where relations can exist.** Polti needs pairs — a
   supplicant *and* a power. A global ranking scatters the cast across five
   peoples; a per-people ranking puts contemporaries in the same tradition.
3. **It composes with roster growth.** Add a species and the cast grows because
   there are more rememberers. No constant to retune —
   `[[SOC-species-scale-mechanism]]`'s eventual elves and dwarves slot in
   untouched.

**Why a constant per holder rather than a ratio or a log.** The anchor is
ethnographic: oral genealogies hold roughly constant *depth* regardless of how
much time has actually elapsed, because the binding constraint is transmission,
not history length. Middle generations telescope out. A ratio would say a people
that founded twice as many hamlets remembers twice as many founders, which gets
the constraint backwards.

**Measured, three seeds:** cast = 90 / 82 / 100. It varies because some peoples
barely exist — seed 7's kobolds have 2 occupations in total and therefore 2
remembered founders. That is the model working, not a shortfall.

**Measured design input:** `corr(peak_population, founded)` = −0.32 / −0.39 /
−0.33. Mildly negative and consistent, so ranking by magnitude is **not**
covertly ranking by age; and because older occupations skew slightly larger, the
earliest founders tend to enter the cast without a rule saying so.

The ledger cost stays a guarantee rather than an estimate: at most
`4 × cast` facts, exactly `3 × cast` plus one per founder already dead at `now`.

**D3 — Promotion runs last in `build_to`.** `mint_entity` is a monotonic counter
(`kernel/src/ledger.rs:145-150`), so appending mints cannot shift an existing
`EntityId` *provided nothing mints after*. This is already the load-bearing
convention for species entities, which are minted "AFTER every pre-species
subsystem ... so the new, Y2-1-only entities are appended last rather than
interleaved" (`windows/worldgen/src/lib.rs:5967-5973`). **Leaving this implicit
is the difference between correct and silently wrong.**

**D4 — Four facts per person, no committed name, and death is conditional.**
`is-person`, `person-founded` and `person-born` always; `person-died` **only if
the derived death day has already passed**. Birth is the occupation's `founded`
day; death is `founded + lifespan(species)` via
`domains/species::allometry::life_history`, which takes no `Seed` or `Stream`.

**`person-founded` exists for the reader, not the corpus.** It points at the
community entity whose occupation this founder opened. The bundle does not require
it — but without it nothing can find a founder from a settlement, and D9's almanac
consumer would be impossible. It is owned by `domains/person` (subject = the
person), not named `occ-founded-by`, because the `occ-*` namespace belongs to
`domains/history` and a domain may not write into another's vocabulary.

**The name is not committed.** It is derived by `persona_of(handle, seed)` at
presentation time, which is what that function exists for. Three reasons, in
ascending order of force:

1. The corpus never asked for it. `bundle:individual-persons` is
   `concept:person`, `is-person`, `person-born`, `person-died`.
2. A name that is a pure function of `(handle, seed)` buys nothing by being
   stored — the same argument the population-field spec used to refuse
   serializing fields: *"a new save-format contract to version, quantize, and
   drift-check, buying nothing."* Determinism makes storage redundant, not
   safer.
3. **Committing `name` would push persons into the one part of genesis nothing
   validates.** `windows/worldgen/src/schedule.rs`'s module doc explains that
   the classification tail is exempt from the capability schema because it
   writes `name`, religion *reads* `name`, and the schema's edges are
   predicate-granular but subject-blind — so a late name-writer is falsely
   forced before an early name-reader. A person stage writing `name` hits that
   same false cycle and lands in the unvalidated tail, which is exactly where
   D3's "run last" convention would then be carrying all the weight. Not
   writing `name` keeps the stage declarable and checked.

The Constitution's *"once a goblin king has a name, he has it forever"* is
satisfied: `persona_of` is total and deterministic, so the name is forever
without being stored.

A living person is represented by the **absence** of a death fact. This is the
asymmetry the data already carries — `OccupationRecord.ended` is
`Option<f64>` — and it is load-bearing rather than incidental: the corpus needs
subjects who can *act*, and a world in which every person is dead would satisfy
all four tokens while missing their point. Because lifespans are short against a
2,000-year bake, the living cast is small and concentrated in recently-founded
settlements. That is honest rather than convenient.

`name` is in `kernel::KERNEL_CORE_PREDICATES` (`kernel/src/world.rs:51`) and is
exempt from the single-writer check, so a new domain committing `name` facts is
not a violation — several domains already do. The four new predicates are
trivially single-writer, and none is read by any genesis stage, so none can
manufacture a schedule cycle the way `name` would.

**D4a — Only three of the four bundle tokens need registering, and the fourth
was already satisfied by a word.** `concept:person` is **already registered** —
by `domains/language`, as `ConceptKind::Living`, *"a person; a member of a
people (the autonym root)"*: a root the generated conlangs get a word for, with
no connection to any entity. The probe forms `concept:{name}`, so it counts as
held today.

So `domains/person` registers `is-person`, `person-founded`, `person-born`,
`person-died` and **no concepts** — `person` is owned by language, and re-registering it with a
different definition would be a `RegistryError::ConflictingDefinition`.

This is worth stating plainly rather than quietly benefiting from: one quarter
of the bundle every situation requires is satisfied by **vocabulary, not
capability.** The Repertoire recorded exactly this weakness as a deferred minor
— *"`concept:` tokens from the language lexicon are words, not modelled things;
no verdict inflated today"* — and it has now come due where it was predicted to.
The corpus is frozen and must not be edited after unblinding (0016), so the
instrument keeps this flaw for now; §7 F4 carries it forward.

**D5 — No new `Stream` draws, therefore no seed-derivation-label epoch.** Both
derivations in the promotion path are draw-free, verified: `persona_of` (above)
and `life_history(mass, class)` (`domains/species/src/allometry.rs:116`, module
doc: *"pure scaling laws... No draws, no world state"*).

The claim is stated as *no epoch is owed because no `Stream` draw exists to
bump* — **not** as "0084 confirms it." Decisions 0073, 0083 and 0084 all
arbitrate label-bumps for draw-*consuming* derivations; 0084's vocabulary
(RE-PIN / EPOCH / EMPTY / LATENT / UNDECLARED) presumes an already-declared
label. A zero-draw feature falls outside their literal scope. See §8.

**D6 — `domains/person`, a new domain.** Persisting world-state is a domain's
job: `windows/CLAUDE.md` is explicit that *"a window that draws has quietly
become a domain with no registry entry and no pin-isolation test."* Decision
0050's entity-hood test is satisfied — persons "can each be usefully addressed
on their own — queried, distinguished, extended with new per-member facts
later." The domain depends on `hornvale-kernel` and nothing else; it reads
occupation data handed to it by the composition root, never by importing
`domains/history`.

**D7 — Coarse constrains fine, and it is nearly free.** The gate reads
`occ-peak`, which *is* the coarse tier's own number, so the fine tier is
derived from the coarse one rather than asserted beside it. Stated invariant: a
settlement's promoted founders never exceed its `POPULATION`, and no person's
birth precedes their occupation's `founded` day.

**D9 — The almanac names remembered founders, so the cast has a reader.** Without
this, `is-person` would be registered and consumed by nothing — which is the exact
pattern this campaign found three times over on its way here (`persona_of`, whose
only caller is its own test; `Notability`, consumed richly and never produced;
species-aware capacity, wired to a Lab report instead of the world). Shipping a
fourth instance, in a campaign launched off a probe whose Supply section exists to
detect precisely that, would be self-parody.

`windows/almanac/src/history.rs` already narrates each settlement's history and
already renders `occ-people` as a collective noun. Where an occupation has a
remembered founder, name them — resolved through `person-founded` and expanded by
`persona_of`. Where none is remembered, **say nothing**: silence is the correct
rendering of a founder nobody remembers, and inventing a phrase for absence would
recreate F1's "an ordinary place, neither famed nor forgotten" for every settlement
that lacks one.

This is a window reading committed facts and rendering them — squarely what a
window is for, and it draws no world-state.

**D8 — Recognition is out of scope, and the corpus agrees.** Being a person,
holding a false belief, and being misrecognised are three separately-bundled
capabilities: `individual-persons` (fan-in 36), `agent-knowledge`
(`knows-that` / `believes-falsely` / `ignorant-of`, fan-in 7), and
`identity-and-recognition` (`presents-as` / `recognized-as` /
`unrecognized-by`, fan-in 2). `SOC-11` defers persona-vs-individual to "the
living-community sequel"; the corpus independently puts it at fan-in 2. Only
the first is this campaign's.

## 4. Measured ground

All figures measured on this branch, seeds 42 / 7 / 1000, at 218 bytes/fact.

| quantity | seed 42 | seed 7 | seed 1000 |
|---|---|---|---|
| ledger facts | 26,309 | 29,406 | 25,020 |
| `world.json` bytes | 5,745,708 | 6,424,096 | 5,461,378 |
| occupations | 1,776 | 1,996 | 1,679 |
| `occ-peak` max | 127 | 90 | 119 |
| peoples | 5 | 5 | 5 |
| cast at `MEMORY_DEPTH = 20` | 90 | 82 | 100 |
| added facts (≤ 4 × cast) | ≤ 360 | ≤ 328 | ≤ 400 |
| ledger growth | **≤ +1.37%** | **≤ +1.12%** | **≤ +1.60%** |
| `corr(peak, founded)` | −0.32 | −0.39 | −0.33 |

The bound is exact rather than estimated: two facts per founder unconditionally,
plus one per founder already dead at `now`. Because lifespans are short against a
2,000-year bake, nearly all will be dead, so the realised figure sits just under
the bound. The implementation reports the actual split.

For contrast, promoting *every* occupation at four facts each would add 7,104 /
7,984 / 6,716 facts — **+27%** — and is the branch that would have required a
forgetting
mechanism. `MEM-1` is the only such mechanism anywhere on the books and it is
unbuilt. Gating at promotion means the cast never reaches a size that would
need forgetting, so MEM-1 stays unbuilt *and* unneeded.

## 5. Preregistered predictions

Frozen before the code that would move them (decision 0016; a study JSON has no
hypothesis field, so the freeze lives here). Scored against
`docs/audits/trope-coverage.md` at close.

**Two tiers, and the distinction is load-bearing.** P1, P3 and P4 below are
**verification**, not prediction: registering the three missing tokens *must* remove the
bundle from Leverage, *must* leave 30 rows, *must* promote `intent` to the top.
They cannot fail unless the implementation is wrong, which is worth checking and
is not a hypothesis. Presenting them as predictions would be the same
cannot-fail defect a reviewer caught in The Repertoire's ratchet.

**P2, P6 and P7 are the actual predictions** — each could plausibly come out the
other way, and P6 most of all.

- **P1 — `bundle:individual-persons` leaves the Leverage table**, because all
  three unregistered tokens become registered (the fourth, `concept:person`, is
  already held — D4a). The table goes from **31 rows to 30**.
- **P2 — Stageable stays 0 of 36.** Persons alone unlock nothing; the closest
  blocked situation is missing four bundles and will still be missing three.
  *This is the honest prediction, and a campaign that moved the score would
  mean the corpus was decomposed wrong.*
- **P3 — The new top row is `bundle:intent`, fan-in 17**, unchanged. Because
  all 35 situations remain blocked, no other bundle's fan-in moves.
- **P4 — "The closest blocked situation is still missing N bundles" goes 4 → 3.**
- **P5 — Ledger growth is ≤ 2% on every seed measured**, and the added fact
  count is exactly `3 × cast + (founders already dead at now)` — an identity,
  not a bound, so a mismatch means the death rule misfired rather than that the
  estimate was off.
- **P6 — At least one same-people pair of remembered founders has overlapping
  lifespans.** This is the prediction I am least sure of and it matters most.
  Founding days span roughly 0–1900 and there are at most 20 founders per
  people, so the mean gap is near a century while lifespans are decades. **If
  this fails, the cast contains no contemporaries and cannot stage a two-actant
  situation even in principle** — which would be the single most useful thing
  this campaign could learn, and would reshape the next one.
- **P7 — The cast spans all five peoples on every seed**, with per-people counts
  equal to `min(MEMORY_DEPTH, occupations)`. A people missing entirely means the
  roster resolution or the per-people grouping is wrong.

A falsified prediction is a finding, not a failure. Nothing may be retuned to
rescue one after unblinding.

## 6. Non-goals

Intent. Other-directed affect. Recognition and misidentification. Kinship edges
between persons. Personality (`PSY-individual-deviation` stays `raw`). Any
per-tick simulation of a promoted person — promotion writes genesis facts and
stops. Fixing `occ-notability` or `occ-function` (§7, F1). Recalibrating
demography's population scale (§7, F3).

**In scope, deliberately:** the almanac consumer (D9). It is the smallest thing
that stops this campaign from registering a capability nothing reads, and it is
a window rendering committed facts rather than new world-state. What stays out
is any *other* reader — no `possess` encounter, no chronicle entry, no scene
surface for a promoted person.

## 7. Found on the way — followups, not this campaign's work

**F1 — `occ-notability` and `occ-function` are constants, and the consequence is
silent presentation collapse.** `windows/worldgen/src/history_bake.rs:1684`
hardcodes `Notability::Common` and `Function::Agrarian` in `Bake::open`, the sole
constructor of every occupation record. Measured `distinct=1` for both, on all
three seeds, all 5,451 occupations — 13.5% of the seed-42 ledger carrying no
information.

**The spec promised the derivation and no task built it.** The Living Community's
design says *"An optional 13th, `notability` (backwater ↔ seat of power), gates
whether residue is a doll or a reliquary"* (`2026-07-20-the-living-community-design.md:119`),
and its plan made the field non-optional from Task 1 and specified its consumers
in detail. Task 3, the bake, lays out the full algorithm and never mentions it.
The gap is a missing task, not a missed step.

What it strands, verified by grep:

- `flesh.rs:303` is the only producer of `ResidueItem::Reliquary`, `Bauble` and
  `Inscription`. Unreachable in every world.
- `flesh.rs:316`'s `if !hamlet_scale` branch — a second, independent dead branch
  (see F3).
- `windows/almanac/src/history.rs:823` — `notability_phrase` returns *"an
  ordinary place, neither famed nor forgotten"* for **every settlement in every
  world**. "A backwater at the region's edge" and "a regional seat of power" are
  authored, reader-facing, unreachable. `function_noun` always yields "steading".
- `windows/vessel/src/interior/pattern.rs:412` — chamber index 2 matches
  `(Some(Notability::Seat), _) => Role::Hall` first, which never fires, so
  `Role::Loomroom` always wins. **The third room of every multi-chamber building
  in every world is a Loomroom.** `Hall`, `Smithy` and `Shrine` are authored and
  never produced.
- `vestige.rs:90` documents an undercity/ruin split "riding on notability at the
  consumer" that was never built.

Three campaigns (The Vestige, The Lintel, The Blocking) built consumer logic on
top over twelve days. No record of the constancy exists before this campaign.
See F6 for why the suite stayed green.

**F2 — no ledger-size ratchet exists anywhere.** `scene_cost.rs` and
`graph_cost.rs` gate wall time; `scene_cost.rs` computes scene bytes but only
asserts `> 0`. Nothing gates `world.json` size or `facts.len()`. This campaign
is the first to deliberately grow the save.

**F6 — a hand-built fixture proves correctness, never reachability.** Every
consumer of the constant fields has a unit test that hand-builds
`Notability::Seat` or `Function::Cult` and asserts correct handling. Every such
test passes; every such branch is unreachable. The suite was green across all
twelve days and three campaigns. Clippy cannot see it — the code is reachable by
*type*, just not by *data*.

The missing assertion is reachability, and nothing in the repo makes it.
Candidate guard: for any enum whose variants gate observable output, assert every
variant is produced by *some* world. The census already builds ~2,000 worlds; a
metric counting distinct values per categorical field would have caught this the
day it landed. `distinct == 1` on a field with three or more variants is the
signature. Variant reachability is currently default-*allow*, where the type
audit and the trope ratchet are both default-deny.

**F4 — a lexicon word satisfies a capability token.** `concept:person` is held
because `domains/language` registers a *word* for person (D4a). The probe cannot
distinguish a modelled thing from vocabulary, because `registry_tokens` forms
`concept:{name}` over every `ConceptDef` regardless of `kind` or `domain`. The
Supply section already annotates `concept:` orphans with their owning domain for
exactly this reason; the **demand** side does not. Candidate fix, for whichever
campaign next touches the probe: require a `kind`/`domain` qualifier in a
corpus's `concept:` tokens, so a bundle can ask for a modelled concept rather
than any concept of that name. Do not retrofit it onto the frozen Polti corpus.

**F3 — the history domain is calibrated for populations an order of magnitude
above what demography produces.** `HAMLET_POPULATION_CEILING = 150` is never
exceeded (max 127 / 90 / 119), so `hamlet_scale` is always true.
`LONGHOUSE_POPULATION_FLOOR = 200` is unreachable, so **every dwelling in every
world is a `Hut`**. Every settlement in Hornvale is, in the history domain's own
vocabulary, a hamlet — and the `RoleHandle` doc comment's illustrative example,
*"the chieftain who led the flight of 312,"* describes a migration larger than
any settlement that has ever existed. Whether a 127-person maximum is intended
is a demography question, and it bears directly on how much persons are worth.

## 8. Flagged for review

**The epoch question is ambiguous, not settled.** D5 argues no epoch is owed.
The argument is sound on the facts — nothing draws — but it rests on a rule
nobody has written: *a zero-draw feature owes no seed-derivation-label epoch.*
0073/0083/0084 do not adjudicate this case. This may deserve its own decision
record.

**`cli/tests/lens_purity.rs::seed_42_world_json_matches_the_committed_fixture`
will redden, by design.** Its own doc anticipates it: drift *"must be
deliberate: regenerate the fixture in the same commit... and record why in the
chronicle."* Fixture regeneration plus a chronicle note are deliverables.
Note the distinction, so a reviewer does not read a red fixture test as broken
determinism: **fixture-vs-live** comparisons redden; **live-vs-live** ones
(`history_byte_identity`, `confluence`, `demesne`) stay green.

## 9. Verification

- Promotion is a pure function of (committed facts, seed): same seed → same
  cast, same names, same days. Asserted by a live-vs-live byte-identity test.
- No `Stream` draw in the promotion path — asserted by the stream manifest
  showing no new label for `domains/person`.
- Pin-isolation: a pinned build mints the same pre-person `EntityId`s as an
  unpinned one.
- Every promoted person's birth ≥ their occupation's `founded`; no person's
  death precedes their birth; the cast size is exactly
  `Σ min(MEMORY_DEPTH, occupations per people)`, and every people with at least
  one occupation contributes at least one founder.
- The four new predicates appear in `hornvale concepts`, and
  `hornvale tropes report` shows `individual-persons` absent from Leverage.
- The almanac names a founder for exactly those settlements that have a
  remembered one, and emits nothing for the rest — no placeholder phrase.
- Every `person-founded` object resolves to a community entity that exists,
  and every remembered founder's community has at least one occupation.

## 10. Definition of Done

The five predictions scored in the chronicle, including any falsified.
`docs/audits/trope-coverage.md` regenerated and its ratchet green.
`cli/tests/lens_purity.rs`'s fixture regenerated with the reason in the
chronicle, **and the seed-42 almanac goldens regenerated too** — D9 changes
narrated prose, so the close diff is wider than the world fixture alone. A campaign retrospective. Registry rows flipped, and rows minted for
the deferred bundles (`agent-knowledge`, `identity-and-recognition`) so the
sequel has a pointer. F1–F3 promoted into the retrospective's follow-up
section. A book freshness sweep, re-scoring any Confidence Gradient bet this
moves.

## 11. Candidate decision record

*A zero-draw feature owes no seed-derivation-label epoch.* Adding a derivation
that consumes no `Stream` draws, appends only new facts, and changes no existing
derivation does not owe an epoch under 0073/0083/0084 — because there is no
label whose bump status is in question. Worth ratifying if Nathan agrees the
rule is general rather than particular to this campaign.
