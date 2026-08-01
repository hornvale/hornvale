# The Scaffold — design

Scaffolding holds a structure up while it is built and comes away when it is
done. Hornvale's history bake has scaffolding it never took down: handles that
exist only during the simulation, wearing the type of the thing they are not.

## 1. Why

`Ledger::mint_entity` is a monotonic counter, so an entity's identity is its
creation position. A sibling campaign proved what that costs: ninety new persons,
appended **last** precisely to avoid disturbing anything, shifted a vessel
session's own NPC from entity 1865 to 1955 and reddened a save-format-class
fixture with no content change at all.

The campaign that fixes the cause is **The Signet**. This one clears its path.
Three things stand in the way, and each is worth removing on its own merits.

## 2. What is wrong

**`EntityId` is doing double duty — a lifetime mismatch expressed as type
reuse.** Bake handles live for the duration of one function call; `EntityId`s
live forever, in every saved world. Sharing a type across that gap is why the fix
is a type split rather than a rename.
`windows/worldgen/src/history_bake.rs:811`
holds a private `next_id: u64`, and `Bake::mint` (`:948`) hands out
`EntityId`-typed values from it that never touch the ledger. They are remapped at
emit through `bake_to_ledger` (`history_emit.rs:119`). So the ledger's primary key
type is also the bake's internal handle type, for a subsystem with no ledger
involvement.

**`OccupationRecord::community` means two different things**, and the code says
so. Bake-side it is *which community this occupation belongs to*. After
`occupation_records` reconstructs a record from facts it is *the occupation's own
entity* — described in its own doc comment as "the placeholder." Neither
`community` nor `lineage` is ever emitted as a fact, so a reconstructed record
cannot know either.

**Two read helpers break ties on mint order.** `occupations_at`
(`history_emit.rs:341`) and `occupations_by_cell` (`:365`) sort by `founded` and
then by `a.community.0.cmp(&b.community.0)` — which, post-reconstruction, is the
occupation's own entity id. The doc calls this "the palimpsest layers a site's
stratigraphy stacks in."

It fires often. Founding days quantise to 25-year epochs, so `(site, founded)`
collides **239 times in seed 42** — about 13% of occupations share a site and a
day with another. Under The Signet those ties would silently reorder, the
stratigraphy would rearrange, the almanac's prose would change, and nothing would
fail.

## 3. Design decisions

**D1 — `BakeId` is its own type.** A newtype over `u64` for handles that live and
die inside the bake. `Bake::mint` returns one; `bake_to_ledger` becomes
`BTreeMap<BakeId, EntityId>`, which is honest about what it translates.

**D2 — The type changes; the derivation does not.** `BakeId` stays counter-derived
here. The Signet later changes *how* it is computed — plausibly to an ancestry
hash — without changing *what* it is. Separating "what type is this" from "how is
it computed" is what keeps both campaigns' diffs legible, and it is why this one
can land without waiting on the community-identity question.

**D3 — Two record types over a shared core.** A plain `Occupation` holds the
eleven fields both sides share: `people`, `site`, `founded`, `ended`,
`peak_population`, `tech`, `function`, `deity`, `tongue`, `cause`, `notability`.

- `BakeOccupation` adds `community: BakeId`, `lineage: BakeId`, and
  `Founding<BakeId>` / `Ended<BakeId>`.
- `OccupationRecord` — **the public name is kept**, because
  `hornvale_worldgen::occupation_records` is consumed outside this crate — adds
  `id: EntityId` and `Founding<EntityId>` / `Ended<EntityId>`. It carries **no**
  `community` and **no** `lineage`, because a reconstructed record genuinely does
  not know them.

The two sides differ in *shape*, not merely in id type, which is why a generic
`OccupationRecord<I>` was rejected: it would make the lie type-safe rather than
remove it. Making `community` an `Option` was rejected for pushing the confusion
to runtime.

**D4 — The palimpsest orders by what it means, and ancestry is part of what it
means.** Same-epoch occupations sort by
`(founded ASC, ended ASC with None last, peak_population DESC, founded_from)`.
A layer that closed earlier lies deeper, which is what a stratigraphy *is*; a
still-living occupation is the top layer; peak breaks the remainder.

**The fourth key is not a fallback — it is measured to be necessary.** Within a
site, records tying on `(founded, ended, peak)` number **6 in seed 42, 4 in seed
7, 0 in seed 1000**. Adding `people` breaks none of them. Every non-entity field
ties: same `cause` (`fled` in all five pairs), same `tech`, same `function`, same
`notability`. The *only* discriminators are `founded_from` and `ended_by`.

So two occupations can share a site, an epoch, a fate and a size, and differ only
in who founded them. **The palimpsest cannot be totally ordered by observable
properties**, and that is a fact about the world rather than a defect in the
comparator — these are genuinely distinct histories that converged on identical
outcomes.

**`founded_from` is a material fact currently wearing a positional
representation.** Who founded a settlement is a fact about the world; that the
fact is presently *encoded* as a mint index is a defect in the encoding, not in
the comparator. Ordering the lattice of candidate comparators over **facts**
rather than over representations puts `(founded, ended, peak, founded_from)` at
the finest *material* order — and everything above it, reachable only by
consulting an artifact of write order, is fabrication.

So this campaign reaches the material ceiling. The Signet then fixes the
encoding, not the comparator. The practical consequence is unchanged — The Signet
will reorder those ~6 records per world — but the reason is that one fact's
representation improves, not that a residual defect persists here.

**D5 — No committed fact changes.** The tie-breaks live in read helpers.
`vestige.rs` states it of itself — "no live mutation, no committed facts" — and
the almanac only renders. So `world.json` stays byte-identical and `lens_purity`
must **not** move. If it does, something commits from a view and that is a finding
in its own right.

**D6 — The consumer surface is three sites.** Outside `history_bake.rs` and tests,
`.community` is read at `history_emit.rs:341`, `:365` and
`almanac/history.rs:587`. The third passes it to `query_by_object` as the
occupation's own entity, and becomes `r.id` under D3.

## 4. Preregistered

Frozen before the code (decision 0016). **Two of these are verification and one is
a measurement — none is a prediction**, and saying so is the point. A previous
campaign shipped three checks dressed as hypotheses; this one has no hypothesis
worth the name, and inventing one would be the same error.

- **V1 (verification) — `cli/tests/fixtures/world-seed-42.json` is byte-identical.**
  Cannot fail unless D5 is wrong, which would itself be the finding.
- **V2 (verification) — the `.community` field no longer exists on the
  reconstructed type**, and the workspace compiles. A grep for it outside the bake
  returns only `r.id`.
- **M1 (measurement, not prediction) — what fraction of multi-occupation sites
  change layer order under D4?** An earlier draft preregistered "between 5% and
  25%." That band had no basis: it was a guess wearing a prediction's clothes, and
  a preregistered range I cannot justify is weaker than an honest unknown. So it is
  recorded as a **measurement to report**, with no predicted value.

  **Measured before implementation, and therefore now a check rather than a
  discovery.** Both orders are computable from a world built today — the old
  tie-break is the occupation's own entity id, the new one is material facts — so
  the number was obtained from the spec's definition without writing any of it:

  | seed | multi-occupation sites | order changes |
  |---|---|---|
  | 42 | 299 | **19 (6.4%)** |
  | 7 | 341 | **6 (1.8%)** |
  | 1000 | 302 | **13 (4.3%)** |

  The implementation must **reproduce these**. A materially different figure means
  the comparator built is not the comparator specced, which is exactly the sort of
  divergence a plan's code listing can introduce silently.

  Recording what this says, since it is the substantive result: mint order was
  deciding far less than the 239 tied pairs implied. `founded` alone separates most
  layers, and the tie-break only bites where a site saw two events in one epoch
  *and* those events differ in end-date or size. The palimpsest has been mostly
  right by accident.

  Note also that the discarded 5-25% band would have been **falsified at the low
  end** — seed 7 lands at 1.8%. Preregistering it would have produced a chronicle
  entry explaining a surprise that was only surprising relative to an invented
  number.

## 5. Non-goals

Changing how any id is *derived* — that is The Signet. Community identity, which
waits on `SOC-self-conquest`. Anything about `Ledger::mint_entity`. Emitting
`community` or `lineage` as facts. The flagship rule, which is The Signet's.

## 6. Verification

- The workspace compiles with `BakeId` and `EntityId` non-interchangeable; passing
  one where the other belongs is a type error, checked by deliberately trying it
  once during implementation and reverting.
- `world-seed-42.json` byte-identical (V1).
- Gallery artifacts regenerate; the diff is read as prose, not just accepted.
- `occupations_at` and `occupations_by_cell` agree with each other on every site —
  there is an existing test asserting the batched and per-cell paths match, and it
  must still pass under the new comparator.
- **V3 — the comparator is a total order.** Assert that no two distinct records
  within a site compare `Equal`, as a test over the live corpus for seeds 42, 7
  and 1000. It should hold via `founded_from`; if it does not, two records share a
  predecessor as well, and D4 needs a fifth key.

## 7. Definition of Done

M1 reported with the measured number and no retrofitted expectation. Gallery artifacts regenerated with the prose
diff reviewed. A chronicle entry and a retrospective. `SOC-self-conquest` and the
other registry rows this brainstorm minted left in place for their own campaigns.
A book freshness sweep, re-scoring any Confidence Gradient bet this moves.

## 8. Flagged for review

**The comparator reaches the material ceiling; one of its facts is badly
encoded.** This section first flagged the totality question as unmeasured, then —
after measuring — as an unavoidable positional key. Both framings were wrong in
the same way: they ranked *representations* instead of *facts*. Who founded a
settlement is material. That it is stored as a mint index is The Signet's problem.

What measuring did establish: `(founded, ended, peak)` genuinely ties — 6 records
in seed 42, 4 in seed 7, 0 in seed 1000 — and no other non-entity field separates
them (same `cause`, `tech`, `function`, `notability`, `people` across all five
pairs). Those six are independent evidence that **ancestry is the right basis for
The Signet's community key**, since the world offers nothing else to tell them
apart.

**A stratigraphy is a partial order, and this campaign still returns a total
one.** Archaeology's Harris matrix records stratigraphic relations as a partial
order and leaves unrelatable contexts unrelated rather than inventing a sequence;
distributed systems reach the same answer with vector clocks, where concurrency is
a first-class result rather than an error. Hornvale's `occupations_at` returns a
`Vec`, which cannot say "these two are simultaneous."

Changing that return type is **out of scope** — it would ripple through every
consumer for a property only ~6 records per world exercise. But the *prose* should
not imply sequence where none exists: where two layers tie on every material fact
but ancestry, "two settlements held this site in the same generation" is truer
than describing one before the other. Recorded here rather than specced, as a
candidate for whichever campaign next touches the almanac's history rendering.

**Everything here is measured at one world size.** ~1,800 occupations across ~400
sites. Tie counts and ancestry chain depth both grow with world size, and
`SOC-species-scale-mechanism` would grow it deliberately. The design is sound at
the scale we have and unmeasured beyond it.

**The public type keeps its name while changing its shape.** `OccupationRecord`
loses two fields. Any out-of-crate consumer breaks at compile time rather than
silently, which is the right failure — but it is a breaking change to a public
type, and worth saying so plainly rather than describing this campaign as a pure
refactor.
