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

**D2 — The cast is the top `REMEMBERED_FOUNDERS: usize = 100` occupations by
peak population.** Not a population threshold. A fixed threshold would silently
mean something different the moment demography is recalibrated — and §7's F3
argues it should be. "The hundred most populous occupations in this world's
history" means the same thing at any scale, and it makes the ledger cost a
*guarantee* (100 × 4 facts, minus living founders' absent death facts) rather
than an estimate.

Selection order is `(peak_population DESC, site ASC, founded ASC)` — a total
order over `u32` and structural keys, no float comparison anywhere.

**D3 — Promotion runs last in `build_to`.** `mint_entity` is a monotonic counter
(`kernel/src/ledger.rs:145-150`), so appending mints cannot shift an existing
`EntityId` *provided nothing mints after*. This is already the load-bearing
convention for species entities, which are minted "AFTER every pre-species
subsystem ... so the new, Y2-1-only entities are appended last rather than
interleaved" (`windows/worldgen/src/lib.rs:5967-5973`). **Leaving this implicit
is the difference between correct and silently wrong.**

**D4 — Four facts per person, and death is conditional.** `is-person`, `name`,
`person-born` always; `person-died` **only if the derived death day has already
passed**. Birth is the occupation's `founded` day; death is
`founded + lifespan(species)` via `domains/species::allometry::life_history`,
which takes no `Seed` or `Stream`.

A living person is represented by the **absence** of a death fact. This is the
asymmetry the data already carries — `OccupationRecord.ended` is
`Option<f64>` — and it is load-bearing rather than incidental: the corpus needs
subjects who can *act*, and a world in which every person is dead would satisfy
all four tokens while missing their point. Because lifespans are short against a
2,000-year bake, the living cast is small and concentrated in recently-founded
settlements. That is honest rather than convenient.

`name` is in `kernel::KERNEL_CORE_PREDICATES` (`kernel/src/world.rs:51`) and is
exempt from the single-writer check, so a new domain committing `name` facts is
not a violation — several domains already do. The three new predicates are
trivially single-writer.

**D4a — The campaign registers three predicates, not four tokens, and one token
was already satisfied by a word.** `concept:person` is **already registered** —
by `domains/language`, as `ConceptKind::Living`, *"a person; a member of a
people (the autonym root)"*: a root the generated conlangs get a word for, with
no connection to any entity. The probe forms `concept:{name}`, so it counts as
held today.

So `domains/person` registers `is-person`, `person-born`, `person-died` and **no
concepts** — `person` is owned by language, and re-registering it with a
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
| cast at N=100 | 100 | 100 | 100 |
| added facts | 400 | 400 | 400 |
| ledger growth | **+1.5%** | **+1.4%** | **+1.6%** |

For contrast, promoting *every* occupation would add 7,104 / 7,984 / 6,716
facts — **+27%** — and is the branch that would have required a forgetting
mechanism. `MEM-1` is the only such mechanism anywhere on the books and it is
unbuilt. Gating at promotion means the cast never reaches a size that would
need forgetting, so MEM-1 stays unbuilt *and* unneeded.

## 5. Preregistered predictions

Frozen before the code that would move them (decision 0016; a study JSON has no
hypothesis field, so the freeze lives here). Scored against
`docs/audits/trope-coverage.md` at close.

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
  count is exactly `4 × REMEMBERED_FOUNDERS` on every seed.

A falsified prediction is a finding, not a failure. Nothing may be retuned to
rescue one after unblinding.

## 6. Non-goals

Intent. Other-directed affect. Recognition and misidentification. Kinship edges
between persons. Personality (`PSY-individual-deviation` stays `raw`). Any
per-tick simulation of a promoted person — promotion writes genesis facts and
stops. Fixing `occ-notability` or `occ-function` (§7, F1). Recalibrating
demography's population scale (§7, F3).

## 7. Found on the way — followups, not this campaign's work

**F1 — `occ-notability` and `occ-function` are constants.**
`windows/worldgen/src/history_bake.rs:1684` hardcodes `Notability::Common` and
`Function::Agrarian`. Measured: `distinct=1` for both, on all three seeds, all
5,451 occupations. That is 13.5% of the seed-42 ledger carrying no information.
Worse, `flesh.rs:303`'s `if occ.notability == Notability::Seat` is the *only*
producer of `ResidueItem::Reliquary`, `Bauble` and `Inscription`, so those three
residue items **cannot appear in any world** — and
`windows/almanac/src/history.rs:765-767` carries authored prose for all three
that no reader can reach. Dead by *data*, not reachability; no lint can see it.

**F2 — no ledger-size ratchet exists anywhere.** `scene_cost.rs` and
`graph_cost.rs` gate wall time; `scene_cost.rs` computes scene bytes but only
asserts `> 0`. Nothing gates `world.json` size or `facts.len()`. This campaign
is the first to deliberately grow the save.

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
  `min(REMEMBERED_FOUNDERS, occupation_count)`.
- The three new predicates appear in `hornvale concepts`, and
  `hornvale tropes report` shows `individual-persons` absent from Leverage.

## 10. Definition of Done

The five predictions scored in the chronicle, including any falsified.
`docs/audits/trope-coverage.md` regenerated and its ratchet green.
`cli/tests/lens_purity.rs`'s fixture regenerated with the reason in the
chronicle. A campaign retrospective. Registry rows flipped, and rows minted for
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
