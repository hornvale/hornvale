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

**`EntityId` is doing double duty.** `windows/worldgen/src/history_bake.rs:811`
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

`founded_from` is therefore the semantically correct final key today, and it
becomes structurally correct once The Signet makes ancestry non-positional. Note
the honest consequence: for those ~6 records per world the order still depends on
a mint-order id, so The Signet *will* reorder them. That is a reduction from 239
mint-order-dependent pairs to six, with the residual explainable rather than
silent.

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
a prediction**; the distinction is stated because a previous campaign shipped
three checks dressed as hypotheses.

- **V1 (verification) — `cli/tests/fixtures/world-seed-42.json` is byte-identical.**
  Cannot fail unless D5 is wrong, which would itself be the finding.
- **V2 (verification) — the `.community` field no longer exists on the
  reconstructed type**, and the workspace compiles. A grep for it outside the bake
  returns only `r.id`.
- **P1 (prediction) — the reordering is visible but small.** Between 5% and 25% of
  sites with more than one occupation change their layer order under D4. Below 5%
  would mean mint order already matched the semantic order closely enough that
  The Signet's risk was overstated; above 25% would mean the almanac's
  stratigraphy prose has been substantially arbitrary. **I do not know which, and
  the number is worth having** — it is the first direct measurement of how much
  mint order was silently deciding.

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

P1 scored with the measured number. Gallery artifacts regenerated with the prose
diff reviewed. A chronicle entry and a retrospective. `SOC-self-conquest` and the
other registry rows this brainstorm minted left in place for their own campaigns.
A book freshness sweep, re-scoring any Confidence Gradient bet this moves.

## 8. Flagged for review

**The comparator needs a positional key, and I could not avoid it.** This section
originally flagged the totality question as unmeasured; measuring it found that
`(founded, ended, peak)` really does tie — 6 records in seed 42, 4 in seed 7 —
and that **no non-positional field separates them**. D4 therefore ends on
`founded_from`, an `EntityId`.

That is defensible: ancestry is what genuinely distinguishes those records, and
The Signet makes it non-positional. But it means this campaign does **not** fully
remove the mint-order dependency it exists to remove — it reduces it by roughly
40×, and converts the residual from invisible to explainable. Worth stating
plainly rather than claiming a clean sweep.

One consequence for sequencing: those six records are also evidence that
ancestry-based identity is the right answer for The Signet's community key, since
the world itself offers nothing else to tell them apart.

**The public type keeps its name while changing its shape.** `OccupationRecord`
loses two fields. Any out-of-crate consumer breaks at compile time rather than
silently, which is the right failure — but it is a breaking change to a public
type, and worth saying so plainly rather than describing this campaign as a pure
refactor.
