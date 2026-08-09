# The Namesake — personal names for the world's historical figures

**Status:** spec, awaiting G3 · **Date:** 2026-08-02 · **Branch:**
`the-namesake` · **Campaign-autopilot:** engaged (ledger at
`.superpowers/sdd/decision-ledger.md`)

A *namesake* is a person named for another person. That is precisely the
mechanism at the centre of this campaign — a name that cites a relation
rather than describing a thing — so the campaign takes the word for its own.

---

## 1. The problem

Hornvale names places, gods, and epithets. It does not name people.

The naming machinery is not the gap. `hornvale_language::NameKind` carries
exactly three variants — `Settlement`, `Deity`, `Epithet` — but underneath
them sits a system that is almost entirely bearer-agnostic: drawn stems
(`Namer::name`), gloss-compounding from concept vocabulary
(`Namer::glossed_name` over `SiteConcepts`), a per-culture shape distribution
with a stereotypy exponent (`NameShape` / `shape_weights` / `shape_beta`),
corpus saturation (`NameCorpus`'s "wear", which makes a morpheme less
attractive the more of a culture's names already use it), and honorific
prefixing gated on `StatusBasis::Rank`. Almost none of that is specific to
settlements.

Nor is the *slot* a gap. `domains/history/src/flesh.rs:32` already defines:

```rust
pub struct Persona {
    /// Seed a presenting window expands into a name.
    pub name_seed: u64,
    /// Seed a presenting window expands into a trait set.
    pub trait_seed: u64,
}
```

reached through `persona_of(RoleHandle, Seed)` — documented as "a lazily-
expandable handle to the individual a role in an occupation's history implies
(a founder, the chieftain who led a flight)." It is authored, documented,
unit-tested, and **consumed by nothing**: no window expands `name_seed`, and
`windows/worldgen/src/history_bake.rs` never mints a `RoleHandle`. The slot
was cut deliberately at The Living Community and left waiting.

The gap is that **individuals stand in no relation to other individuals.**
Community lineage is committed (`occ-founded-from`, plus a bake-side
`lineage: BakeId` inherited by daughters and refoundings). Person lineage
does not exist anywhere in the repo. Every kin-based naming pattern — the
patronymic, teknonymy, generation names, papponymy, the inherited clan name —
blocks on that one absence, which is why this campaign builds the graph and
the grammar together rather than either alone.

The Watershed named this campaign in its own non-goals
(`docs/superpowers/specs/2026-07-29-the-watershed-design.md:184`):

> **Individual person names.** "Shaman's Creek" needs a named shaman. Real,
> and the richest remaining vein, but it is a different campaign — it needs
> people to be entities with names before places can be named after them.

Three idea-registry rows (`LANG-eponym`, `LANG-eponym-valence`,
`LANG-name-as-codec`, all from the 2026-07-31 ideonomy session) presuppose
named individuals and are blocked on the same thing.

### 1.1 What the world actually contains (measured)

Every number below is measured on a live seed-42 world
(`hornvale new --seed 42`, 26 309 facts), not inferred. This subsection is
the spec's factual base and the reason §3's design differs from the one this
campaign started with.

```
occupations                            1776    (2000-y bake, 25-y epochs)
  with an Entity-valued founded-from   1759    (99.0%)
  genesis roots                          17
descent-tree depth                   up to 29 links
branching                            1223 nodes with 1 child, 200 with 2,
                                       33 with 3, 8 with 4, 1 with 5
parent -> child founding gap         median 50 y | mean 106 y
                                     p25 25 | p75 150 | p95 375 | max 975
children founded while the parent
  occupation was still alive         1759 / 1759  (100%)
```

**The founding-gap distribution falsified this campaign's original design.**
The first proposal was that the founder of a daughter community is the
*child* of the founder of the mother community, making the committed
community tree directly a genealogy. A median gap of 50 years is already two
generations, and a 975-year gap cannot be a parent-child link under any
lifespan the roster carries. The edge is a **descent** relation at an unknown
remove, and the remove has to be derived rather than assumed.

`hornvale_species::allometry::LifeHistory::generation_length` already supplies
what the derivation needs — per species, from mass and metabolic class,
"MEM-7's handle", `Option<Years>`, computed on demand and never stored:

```
species     mass kg   class        generation_length
goblin         18.1   Endotherm         21.7 y
kobold         13.6   Ectotherm         30.2 y   (1.5x pace multiplier)
hobgoblin      74.8   Endotherm         30.9 y
bugbear       132.0   Endotherm         35.6 y
```

Dividing each edge's gap by the occupying people's generation length gives
the remove between two named founders:

```
IMPLIED GENERATIONS between a founder and their forebear   (n = 1115)
  median 2 | p75 6 | p90 10 | p95 13 | p99 20 | max 32 | mean 3.8
  60% of edges resolve in <= 3 implied ancestors
  13% are ZERO-hop -> same generation -> the two founders are SIBLINGS
```

Three consequences the design leans on. Lazy ancestor interpolation is cheap
(median 2, bounded at 32). Sibling relations fall out free at 13% of edges,
without modelling siblinghood at all. And the 1.6× spread in generation
length across the roster means a goblin lineage runs ~1.6× more generations
than a bugbear lineage over the same 2000 years, so lineage *depth* is
genuinely species-differentiated even though — per §5.0 — the phonological
consequence this campaign first expected from that spread is not.

---

## 2. Non-goals

Each is registered rather than dropped; see §8.

- **True names, name taboo, and the address/reference split.** The richest
  speculative-fiction vein — a name that confers power, a name of the dead
  that goes unspoken, a name you may refer to but never use in address — is
  an *epistemic* mechanic, not a naming one. It couples to UNI-16 (belief as
  a threshold) and belongs with that engine, not here.
- **Eponymous toponymy** ("Shaman's Creek"). This is where an epoch actually
  lands: a *committed* settlement name citing a *derived* person name binds
  that derivation forever, so changing the naming function afterwards
  silently rewrites every saved world's toponyms. Slice 1 deliberately keeps
  person names on the derived side of that line. See §4.
- **Living inhabitants and vessel NPCs.** Session-derived NPCs keep their
  current type label ("hobgoblin of Feefaenoagoo"). Naming them is a separate
  bearer population with a separate lifecycle.
- **Exonyms** — the same figure named in another people's language. LANG-10
  already holds that row and its mechanism is shared with settlements.
- **An explicit person genealogy in the bake.** New committed state, an
  epoch, and redundant with a tree the ledger already carries.
- **Naming every individual.** Only figures a role implies — founders, and
  the interpolated ancestors between them — are nameable. The engine
  materialises a persona when something observes one, never in bulk.

---

## 3. Design

### 3.1 The person-descent graph is a reprojection, not new state

No fact is added. No entity is minted. The graph is a pure, total function of
facts the ledger already carries.

- `founder(occupation) -> RoleHandle`, derived from the occupation's
  `EntityId` and the world seed, through the existing `persona_of` hash.
- For an edge `child -> parent` with founding gap `Δ` and the child people's
  generation length `G`: `g = round(Δ / G)`.
  - `g == 0` — the two founders are **siblings**, same generation, same
    forebear.
  - `g >= 1` — there are `g − 1` implied intermediate ancestors, each its own
    lazy `RoleHandle` on a deterministic walk from the child's handle toward
    the parent's.
- The transitive root of the chain (a genesis occupation) is the **clan**.

`g` uses the *child's* generation length, not the parent's, because the
people occupying the daughter site is what the ledger records and what the
descent runs through; a migration that changes people is a clan boundary, not
a continuation.

This is The Freshwater's pattern — a pure read is the cheapest safe
save-format change — and it is what makes the campaign Nathan selected
("lineage + grammar") cost no epoch, which was not true of the version it
replaced.

### 3.2 A name is an ordered list of elements

```rust
/// A figure's full name: every element it has accrued, in cultural order.
pub struct PersonName { pub elements: Vec<NameElement> }

pub struct NameElement {
    pub source: ElementSource,
    pub author: Author,
    /// The standard year it was conferred; `None` for conferred-at-birth.
    pub conferred: Option<f64>,
}

pub enum ElementSource {
    /// Drawn phonology — the given name.        `Namer::name`      SHIPPED
    Stem,
    /// Compounded concepts.                     `glossed_name`     SHIPPED
    Gloss(GlossBasis),
    /// A walk to another entity.                the §3.1 graph     NEW
    Relation(Cite),
    /// A position in a sequence.                arithmetic
    Index(IndexBasis),
    /// An event from the history bake.          a ledger query
    Deed,
}

pub enum Cite { Parent, Clan, Community, Place, Deity, Mentor, Child }
pub enum IndexBasis { BirthOrder, Generation }
pub enum Author { Kin, Community, Witnesses, Institution, Selfward, Outsiders, Inherent }
```

Two of the five sources are already built and merely pointed at settlements.
`Relation` is the only one that needs the new graph.

`Author` carries the dimension the surface vocabulary hides: who conferred an
element determines whether it can be revoked, who may confer another, and
whether it can be *false*. An `Outsiders`-authored element is one the bearer
cannot revoke — which is exactly what `NameKind::Epithet` already models for
deities, so epithets become a special case of this system rather than a
parallel one. `Selfward` authorship is the only source that can lie, which is
the seam UNI-16's deception mechanic will later use.

`Cite::Child` is **teknonymy** — Arabic *Abu Bakr*, "father of Bakr" — a
parent named for their child, so assignment flows backward. It is derivable
here from an inverted edge walk and is close to unused in the genre.

### 3.3 The pattern is derived from `SocietyVector`, never authored

Precedent: The Bane derived a whole threat niche "from what the creature
already is — no fresh authoring", and `morph_options` already derives
`honorifics: true` from `StatusBasis::Rank`. A per-culture authored naming
table would be exactly the lookup-table the project has rejected repeatedly
(decision 0021's anti-lookup-table discipline).

| Society dimension | Effect on the pattern |
|---|---|
| `Sociality::Hierarchic` | kin/institution authorship; cites `Parent` or `Clan` — *descent legitimates* |
| `Sociality::Communal` | community authorship; cites `Community` or `Deed` — *what you did legitimates* |
| `StatusBasis::Rank` | honorific prefix (shipped) + `Cite::Parent` |
| `StatusBasis::Knowledge` | `Cite::Mentor` — the transmission lineage, the anthroponymic twin of LANG-53/54's mentorship-distance drift |
| `StatusBasis::Generosity` | `Deed` |
| `in_group_radius` | how many elements the pattern carries (insular peoples need fewer to disambiguate) |
| `shape_beta` (shipped) | how stereotyped the pattern is across the culture — one dominant form versus a real tail |

### 3.4 Rendering: the shortest-prefix rule

`render(name, scope)` returns the **shortest element prefix that is unique
within that scope** — the given name inside a household, given plus one
byname inside a settlement, the full stack across a region.

This is decision 0024 generalised, not a new rule. That decision ratified
"uniqueness is a property of a reference, not of a name" for settlements,
accepting a measured 4.91% collision rate as the honest behaviour of
meaningful toponymy and disambiguating at render time from site facts.
Personal names collide far harder than toponyms and *should*; the identical
rule handles them. The structure is git's shortest-unique-SHA prefix, DNS's
search-domain suffixing, and *E. coli* after the first *Escherichia coli*.

Name length is therefore **computed, never authored**, which is what keeps
the output ergonomic rather than a wall of Sindarin.

---

## 4. Determinism and epoch

> **Correction, recorded at the merge review (not a rewrite).** The second
> bullet below — "No new stream label, so `stream_labels()` and the generated
> stream manifest do not move" — was **false as shipped**, and is left in
> place rather than deleted so the record shows what was believed and when.
> It was written before Task 4 existed. Task 4 added the given-name draw
> `language/<species>/name/person` (`domains/language/src/lib.rs`), so
> `stream_labels()` *did* move and `book/src/reference/stream-manifest-
> generated.md` gained a row (the layering page moved with it). The
> implementation plan caught this and corrected it; this spec section never
> was, which is why the chronicle inherited the error too.
>
> **What is still true, and is the claim that mattered:** the label was added
> **additively**. No existing label was renamed or reseeded, no epoch was
> declared, and no already-saved world's output changed — so the first
> bullet's "no epoch" and the third bullet's "every shipped world is
> byte-identical" both stand. See `docs/decisions/0084` (an epoch is declared
> only when a derivation *moved*), which is the reason a new label needs no
> epoch suffix.

**No epoch. No save-format change. No new fact, predicate, or concept.**

- Every name is a pure total function of committed facts plus the world seed,
  through `persona_of`'s existing bit arithmetic (no `Stream` draw, so no
  consumption-order contract is touched).
- No new stream label, so `stream_labels()` and the generated stream manifest
  do not move.
- No genesis path changes, so every shipped world is byte-identical and the
  gallery/reference artifacts do not drift.

The one place this could stop being true is named in §2: a **committed**
toponym citing a **derived** person name binds the derivation, at which point
changing it rewrites saved worlds. Decision 0084 (an epoch is declared only
when a derivation moved) and 0089 (an epoch freezes when it can be stamped on
a saved world) put that boundary exactly at eponymous toponymy, which is why
it is held back to its own campaign.

Determinism constraints that apply as usual: `BTreeMap`/`BTreeSet`/`Vec`
only, `total_cmp` with deterministic tie-breaks for any float ordering, no
wall-clock, and quantisation at emit boundaries only.

---

## 5. The preregistered claims

Frozen here, before the code that could move them (decision 0016).

### 5.0 The claim this spec started with, and why it is not preregistered

The original hypothesis was that **inherited names are phonological
fossils**: a `Relation(Clan)` element is minted once at the chain root and
runs through the sound-change cascade for the age of the clan, while a
`Relation(Parent)` element is re-derived each generation and always sounds
current — so archaism would measure inheritance depth, differentiated by
species through generation length.

It was withdrawn on two measurements, both taken before anything was built.

**First, the drift rate does not vary across the peoples.**
`windows/worldgen::cascade_regime_of` keys the drift rate on `SocialForm`,
and goblin, kobold, hobgoblin and bugbear are **all `SocialForm::Settled`**
(`domains/species/src/lib.rs:1537,1548,1559,1570`), so all four draw at
`CascadeRegime::SETTLED` — one identical rate. The regime only separates
`Gregarious` beasts, long-lived `Solitary` dragons, and `Sessile`. The
species-differentiation criterion was therefore false by construction, and
"measuring" it would have produced a null that was an artifact of misreading
the engine rather than a fact about the world.

**Second, and fatally, the cascade is inert for most peoples.** Counting
which sound rules actually fire in the committed seed-42 dictionary
(`book/src/reference/dictionary-generated.md`):

```
species      rows   distinct rules that FIRE
goblin         70        0     <- cascade inert
hobgoblin      74        0     <- cascade inert
kobold         78        0     <- cascade inert
bugbear        76        1     (lenition, 35/76 rows)
gnoll          84        2     (fortition 37, lenition 9)
```

A `Cascade` is 2–4 rules (`domains/language/src/etymology.rs:96`) applied
all-or-nothing by `evolve`, with no dating, so the finest "age" signal
available is *how many trailing rules a name underwent* — 3 to 5 buckets at
best. For three of the five peoples it is **zero** buckets: an inherited clan
name and a re-derived patronymic would come out byte-identical. There is no
signal to preregister against.

**This is one seed, so it is evidence and not yet a finding.** Whether the
cascade is inert across the battery, or seed 42 is unlucky, is unknown and is
**Task 1 of the implementation plan** — a cascade rule-firing metric over the
seed battery, which the lab has no metric for today. If inertness is general
it is an engine finding well outside this campaign's scope and should be
reported as one; the fossil claim then becomes testable only after it is
fixed. `LANG-name-fossil` carries the claim and this measured reason (§8).

### 5.1 Claim A — the derived pattern produces distinguishable cultures

The deliverable is "a flexible system enabling the patterns of human cultures
and of speculative fiction." The honest test of *flexible* is whether the
`SocietyVector`-derived patterns (§3.3) actually differ between peoples
rather than collapsing to one shape with cosmetic variation.

**Criteria, frozen.** Over ≥ 200 seeds:

1. The roster's peoples yield **≥ 3 distinct pattern signatures** (a
   signature being the ordered multiset of `(ElementSource, Author)` pairs).
2. A figure's people is recoverable from its name's element structure alone
   at **≥ 2× the chance baseline** (chance = 1/*n*peoples on that seed).

**The null.** If (1) fails, `SocietyVector` is too coarse to drive naming and
the pattern must key on something else — a real result that redirects §3.3
rather than merely disappointing it. If (1) holds and (2) fails, the patterns
differ on paper but not observably, which localises the problem to the
rendering rule in §3.4.

### 5.2 Claim B — the shortest-prefix rule earns its keep

§3.4 asserts that computing name length beats authoring it. That is only true
if disambiguation pressure is real but not universal.

**Criteria, frozen.** Over the same seeds:

1. At **settlement scope**, ≥ 80% of figures resolve in exactly 1 element.
2. At **region scope**, the median resolves in ≥ 2 elements, and **fewer than
   50%** require the full element stack.

**The null, and it is two-sided by design.** If everything resolves in one
element at every scope, disambiguation is unnecessary and §3.4 is dead code
dressed as a principle. If everything needs the full stack, the rule saves
nothing and names are simply long. Either outcome falsifies the ergonomics
claim, and the campaign says so rather than quietly shipping the machinery.

**No constant will be retuned to rescue any criterion after unblinding**
without saying so in the chronicle.

---

## 6. Verification

- **Unit** — `persona_of` purity and totality (already covered); the remove
  derivation `g = round(Δ/G)` at the zero-hop, one-hop, and 32-hop extremes;
  sibling detection at `g == 0`.
- **Property** — the descent walk terminates for every occupation on a
  battery of seeds (the tree is committed and acyclic, but the walk must not
  assume it); the same `(handle, seed)` always yields the same name; a name's
  element list is stable under ledger growth.
- **Byte-identity** — the full artifact set regenerates unchanged
  (`make rebaseline` then `git diff --exit-code book/src/gallery/
  book/src/reference/ book/src/laboratory/ docs/audits/`). This is the
  campaign's central safety claim and is asserted, not assumed.
- **Anti-vacuity** — a mutation step per the Timekeeper's lesson: each new
  assertion must be shown to go RED under a deliberate perturbation, since a
  test that asserts nothing ships green.
- **Lab** — a `the-namesake` study measuring §5's two criteria across the
  seed battery. One world is an anecdote; the claim needs the battery.
- **Checks the gate does not run**, enumerated so they are not forgotten:
  `cargo run -p hornvale -- concepts` regen + diff (no new concept is
  expected, so a diff here is a *finding*), and the type-audit report.

---

## 7. Flagged for review (G3)

1. **No epoch, and that is a claim worth challenging.** §4 asserts byte
   identity for every shipped world. It is the campaign's biggest structural
   bet; if a reviewer sees a path by which a derived person name reaches a
   committed value in slice 1, the whole shape changes.
2. **`occ-founded` is in years, doc-commented as "standard days."** Same for
   `Occupation::tenure` and `HISTORY_NOW`. Traced the one place it could bite
   (`vestige.rs` weathering against `PERISHABLE_MAX_AGE`, documented in
   years) and the units agree, so this is a naming inconsistency rather than
   a live arithmetic bug. Recorded as a followup; flagged because a reader of
   §1.1 will otherwise trip on it.
3. **The sound-change cascade appears inert for 3 of 5 peoples on seed 42**
   (§5.0): goblin, hobgoblin and kobold show zero sound rules firing across
   70–78 dictionary rows each. This killed the campaign's original headline
   claim before a line was written. It is one seed and so not yet a finding,
   but if it generalises it is an **engine-level result well outside this
   campaign** — a drawn cascade whose rules never fire means the language
   domain's whole etymological layer is decorative for most peoples. Task 1
   of the plan measures it across the battery. Nathan's call whether that
   measurement stays here or forks into its own campaign.
4. **Generation length is used outside the population it was calibrated on.**
   `LifeHistory::generation_length` is a mass-and-class allometry anchored on
   a 40 kg endotherm. §3.1 applies it to the *social* question of how many
   human-analogue generations separate two founders. That is a reuse across
   populations, and the memory note "measure the population you apply to"
   says to state it rather than let it pass silently. The alternative — a
   separately authored social generation length — is fresh authoring the
   project's discipline resists.
5. **`the-namesake` is the fourth active worktree** (`the-contour`,
   `the-teller`, `the-tithe` exist). CLAUDE.md puts the Mac's working ceiling
   at two to three campaigns and `make gate` at 22–37 min with `cpu_ratio`
   8.25–8.50 on ten cores. Brainstorm and spec work contend for nothing, but
   the first gate here must be staggered by hand.

---

## 8. Capture manifest

**Idea-registry rows — ADDED in `e52fb373`** (slugs per decision 0026, never
numbers; `docs_consistency` green):

- `LANG-truename` — the true name / name taboo / address-vs-reference
  epistemic layer; couples to UNI-16.
- `LANG-teknonymy` — the parent named for the child; assignment flowing
  backward along the descent edge.
- `LANG-name-fossil` — inherited elements as phonological fossils, carrying
  the measured reason it is withdrawn rather than merely deferred.
- `LANG-cascade-inertness` — **not a naming row**: the seed-42 evidence that
  the sound-change cascade may be inert for most peoples (§5.0, §7.3).
- `LANG-namescope` — the shortest-prefix render rule as a general naming
  primitive, shared with toponymy.

**Rows this campaign unblocks (already present, to be updated at close):**
`LANG-eponym`, `LANG-eponym-valence`, `LANG-name-as-codec`.

**Followups:** the years/days doc inconsistency (§7.2).

**Rejected branches, with reasons:** committing a `person-name` fact
(contradicts The Living Community §5.4; ~1776 facts for a nanosecond
re-derivation); an explicit bake genealogy (new state, an epoch, redundant);
grammar-without-lineage and lineage-without-grammar (both defer the same
blocker — Nathan chose against both at G1); a per-culture authored naming
table (the lookup-table anti-pattern, decision 0021).
