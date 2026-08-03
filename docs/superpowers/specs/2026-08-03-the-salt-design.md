# The Salt — design

**Campaign:** The Salt (slug-named, decision 0026) · **Date:** 2026-08-03 ·
**Sequence:** the second of three — The Scaffold (merged) → **The Salt** →
The Signet.

## 1. Why

The Particular is parked at nineteen commits because promoting ninety persons
shifted a vessel session's NPC from entity 1865 to 1955 and reddened a
save-format-class fixture with no content change at all. `EntityId` is a
positional identifier doing the job of a stable identity. The fix runs in
three steps: The Scaffold split the bake's private handle from the ledger's
and removed mint order from the stratigraphy comparator; **The Salt stops
derived prose from reading an id**; The Signet then changes how ids are
derived.

The Salt exists for one reason, and it is a reviewability reason rather than a
correctness one: **The Signet's artifact diff must contain only id changes.**
If prose still keys on ids, that diff carries both, and no reviewer can tell
"an id moved" from "prose changed because an id moved."

## 2. What is wrong

Measured, not inferred. The population was established by an **id-shift
rehearsal** rather than by grep: a temporary probe burns K extra mints at the
top of `build_to`, shifting every entity id by K, after which the full
artifact set is regenerated and diffed against a verified-clean baseline.
Whatever moves is coupled to id derivation by construction, so no channel can
hide from it. (Probe preserved at `.superpowers/sdd/id-shift-probe.patch`.)

Under `HV_SALT_ID_SHIFT=1000`, exactly three artifacts moved:

| artifact | what moved | whose |
|---|---|---|
| `book/src/gallery/history-seed-42.md` | one line: "a hut and a granary" → "two huts and a granary" | **The Salt's** |
| `book/src/gallery/possession-seed-42.md` | `settlement/7/name` → `settlement/1007/name`; the name itself unchanged | The Signet's |
| `book/src/gallery/possession-over-time-seed-42.md` | NPC ids `[819]` → `[1819]`; names unchanged | The Signet's |

Everything else held: all three almanacs, the book, `explain`, every reference
dump, the chorus study, the type-audit report. Settlement names are salted by
**cell**, and deity names were re-seeded from phenomenon identity by commit
`e023ef9a` ("naming /v2 epoch") — both already decoupled. That commit is this
campaign's precedent, one channel at a time; The Salt generalizes it.

Four channels remain, and they differ in a way that matters:

**C1 — the flesh seed.** `windows/almanac/src/history.rs`'s `flesh_seed`
derives `world.seed.derive(FLESH).derive(dynamic(&entity.0.get()))` and hands
it to `structures_of` and `residue_of`. Its own doc comment calls itself "a
rendering convention, not a save-format contract" directly above a derivation
from a save-format value.

**C2 — the founder handle.** `windows/worldgen/src/descent.rs`'s `founder_of`
mixes `u64::from(occupation.0)` with the world seed into a `RoleHandle`,
which `persona_of` expands into the `name_seed` every person name is drawn
from. **This channel is invisible to the rehearsal**: The Namesake's
person-name metrics live only in the census fixture, which
`regenerate-artifacts.sh` excludes by design. Measured directly instead — the
published metric code, 20 worlds, with and without the shift: **18 of 20
world-rows move.** The three `name-prefix-*` columns move;
`name-pattern-signatures` and `name-people-recoverability` hold, being
structural rather than name-content. A rehearsal bounded by the regen script
reports this channel as clean.

**C3 — `layer_key`'s fourth key.** The Scaffold replaced mint order with
"material facts," but the fourth key is `founded_from`, and
`Founding::From(e)` orders by the *predecessor's* `EntityId`
(`domains/history/src/record.rs:239`). The Scaffold's own doc says so — "a
compromise, not a material fact … itself a mint-order artifact … a future
encoding that gave a founding its own material identity (a 'signet') would
close this gap" — and deferred it to The Signet. **That deferral does not
work.** Leaving it means The Signet's diff contains stratigraphy
reorderings, which is exactly what this sequence exists to prevent. It
belongs one campaign earlier.

**C4 — `conquest_victim`'s tie-break.** `.min_by_key(|e| e.0.get())` under a
doc claim that a conqueror "can have at most one such victim." Verified
rather than trusted: **1718 candidate calls across seeds 42/7/1000, never
more than one candidate.** The tie-break is dead code.

## 3. Design decisions

**D1 — The type this campaign needs already exists.** `Occupation` (the
"core" The Scaffold carved out as "the facts both sides agree on") contains
`people`, `site`, `founded`, `ended`, `peak_population`, `tech`, `function`,
`deity`, `tongue`, `cause`, `notability` — **and no `EntityId`**. The three
id-bearing fields (`id`, `founded_from`, `ended_by`) all live on the
`OccupationRecord` wrapper. So the id-free material core is already a named
type, and this campaign adds derivations over it rather than a new type. That
is the dividend of The Scaffold's split.

**D2 — Two keys, each scoped to its causal horizon.** The tempting design is
one universal material key used four ways. It is wrong for C2: keying a
founder on the full core makes a person's name a function of *how their
community later died*, which is causally backwards. So:

- `material_key(&Occupation) -> u64` — a mix over the whole core. Feeds
  **C1**, correctly: a ruin reflects the whole life of the place, including
  its size and its ending.
- `founding_key(&Occupation, parent: Option<&Occupation>) -> u64` — a mix
  over `(people, site, founded)` plus the predecessor community's same
  triple. Feeds **C2**: a founder is identified by where, when and from whom
  they founded — which is what The Namesake's descent model already says a
  name cites.

Both are plain splitmix-style bit arithmetic in `domains/history`, beside
`persona_of`, which already does exactly this. No new dependency; nothing
transcendental; libm-free by construction. Float fields are read back from
the ledger already quantized (decision 0033), so `to_bits` on them is
cross-platform stable.

**D3 — Collisions are the correct output, not a defect to be broken.** Under
an id key, two occupations identical in every material fact get *different*
potsherds — entropy fabricated from mint order. Under a material key they get
the same ones, which is true. The house precedent is The Namesake itself,
which **drops** name elements it cannot resolve rather than filling
placeholders, on the stated grounds that a placeholder "would fabricate
disambiguating entropy the engine does not have." **No tie-break of last
resort will be added to restore today's distinctness.** Measured
consequences, all three seeds, in §4.

**D4 — C3 keys on the predecessor's material coordinates.** The fourth key
becomes the predecessor's `founding_key` rather than its `EntityId`. Where
the key still ties, the sort falls through to `sort_by_key`'s stability over
ledger iteration order — which is **commit order, and therefore invariant
under a change of id derivation**. The Signet moves ids, not the order in
which facts were committed. This is why a key that ties more often than the
id did is nevertheless safe, and it is the one place this campaign leans on
an invariant it does not own (The Scaffold flagged the same one).

**D5 — C4 loses the id rather than re-keying it.** Since the candidate set is
never larger than one, order the (at most one) candidate by its own
`(site, founded)` instead of its entity id. Deterministic, id-free, and
correct if a second candidate ever does appear — which the current
expression's `min` was already there to handle.

**D6 — `history/flesh` becomes `history/flesh/v2`.** The label is declared as
a permanent derivation contract in `domains/history/src/streams.rs`, so
changing what it derives from takes an epoch suffix, never a rename
(decision 0006). Precedent is exact: `e023ef9a` bumped naming to `/v2` for
this same reason. Flesh is never committed, so **this is not a save-format
break** — no saved world changes meaning — but the manifest is a published
artifact and must be regenerated.

**D7 — One greppable rule, with a mechanical backstop.** The invariant this
campaign establishes: *an `EntityId` may be stored, compared and looked up;
it may not be read for its value.* `map.get(&e)`, `a == b`, and
`Value::Entity(e)` in a committed fact are all fine and all untouched by The
Signet; `e.get()`, `u64::from(e.0)`, `min_by_key(|e| e.0.get())` and
`StreamLabel::dynamic(&e…to_string())` are the whole population. A
`disallowed-methods` entry in the manner of decision 0092 is **not**
available — `NonZeroU64::get` is far too general to ban workspace-wide — so
the backstop is a source-scan test in `cli/tests/`, the shape
`architecture.rs` already uses, scoped to the derived-prose paths
(`domains/history`, `windows/almanac/src/history.rs`,
`windows/worldgen/src/descent.rs`) with an explicit allowlist for the
legitimate id-valued surfaces.

## 4. Preregistered

Frozen before the code (decision 0016). Two verifications, two measurements,
one of them with a directional prediction.

- **V1 — the rehearsal goes quiet on prose.** After The Salt, with
  `HV_SALT_ID_SHIFT` set, `history-seed-42.md` is byte-identical. The two
  possession files still move (id-valued output — The Signet's, correctly
  present). Any *other* artifact moving means a channel was missed.
- **V2 — person-name metrics become id-invariant.** The probe study that
  today moves **18 of 20** world-rows must move **0 of 20**.
- **M1 — the collision rates the implementation must reproduce**, measured
  pre-implementation from the spec's own definitions:

  | key | seed 42 | seed 7 | seed 1000 |
  |---|---|---|---|
  | material core (C1) — occupations in a colliding group | 1.0% | 0.2% | 0.3% |
  | material core — **flesh-rendering layers** colliding | **0.0%** | **0.0%** | **0.0%** |
  | founding + one ancestry hop (C2) | 8.4% | 3.3% | 3.6% |

  A materially different figure means the keys built are not the keys
  specced. Note the second row: the almanac renders flesh for one layer per
  site, and **no** rendered layer collides at any seed — so C1's fidelity
  cost on committed output is zero, and the 1.0% is a property of the
  unrendered population.

- **M2 — does The Namesake's shortest-prefix rule start firing?**
  *Directional prediction, and the accepted cost of Nathan's D2 ruling.* The
  Namesake measured person stems as near-unique and concluded its rule "is
  priced for a collision rate the name generator declines to produce." At an
  8.4% stem-collision rate the rule should fire **more than the ≈0 it
  measured**. Report `name-prefix-settlement-scope`,
  `name-prefix-region-scope` and `name-prefix-region-full-stack` before and
  after, and say plainly whether a just-merged falsification has been
  partially reversed. **This must not be reported as a surprise** — it was
  chosen with the numbers in hand.
- **V3 — C3's order change is one site.** Switching the fourth key changes
  the rendered layer order at **0 sites (seed 42), 1 (seed 7), 0 (seed
  1000)**.
- **V4 — nothing committed moves.** `cli/tests/fixtures/world-seed-42.json`
  byte-identical; every one of these derivations is a read helper.

## 5. Non-goals

Changing how any id is derived — that is The Signet. The vessel session's
entity exposure (`session.rs:566`/`579`) and the possession transcripts'
`[819]` numbers: those are genuinely id-valued output and belong in The
Signet's diff. `SIG-agentid-entityid-confusion`. Unparking The Particular,
which waits on The Signet. `domains/language/src/account.rs`'s `value_text`
rendering an entity id as concept text — real, but inert (the lookup always
misses) and captured as a followup rather than fixed here.

## 6. Verification

- **The committed artifact exercises only half of C1.** The one line that
  moved is `structure_phrase(&structures)`, from `structures_of`. The residue
  half did not move *because seed 42's showcase site is still alive* ("no
  ruin to read"). So `residue_of` must be verified by a direct test over dead
  occupations, never by the gallery diff alone — the same trap The Scaffold
  fell into from the other side.
- Both flesh derivations tested directly for id-invariance, not only through
  the render.
- The three decoders (`occupations_at`, `occupations_by_cell`,
  `layers_at`) still agree on every site under the new fourth key; the
  existing batched-vs-per-cell agreement test must stay green.
- `make rebaseline` run and every gallery diff read **as prose**, not
  accepted on a clean exit.
- The stream manifest regenerated for D6 — a `stream_labels()` change needs
  its own manifest-regen step, which has been missed before.
- The type-audit report regenerated; new `pub` functions in
  `domains/history` will drift it.
- Both census fixtures refreshed at the close (`the-census` and
  `census-of-the-meeting`), since C2 moves `name-prefix-*` columns in both.

## 7. Definition of Done

M1 and V3 reproduced with the measured numbers and no retrofitted
expectation. M2 reported with its before/after and an explicit statement of
what it did to The Namesake's finding. The id-shift rehearsal promoted from a
throwaway patch into a real, committed test, so the property this campaign
establishes cannot silently rot — a campaign whose whole product is an
invariant must leave the invariant enforced. A chronicle entry, a
retrospective, a book freshness sweep re-scoring any Confidence Gradient bet
this moves, and the census refresh named in §6.

## 8. Flagged for review

**The census refresh is the one carve-out.** C2's fix moves committed census
fixtures, so the close needs `bash scripts/census-run.sh` (local, ~7 min
since The Local Census, decision 0081). Flagged because census regeneration
is an explicit-authorization item.

**D6 is the epoch call.** `history/flesh` → `history/flesh/v2`. Not a
save-format break (flesh is never committed), but it is a published
derivation contract and the manifest moves.

**D4 leans on an invariant it does not own.** The fourth key is not total,
and the three decoders agree on a tie only because `sort_by_key` is stable
over the same ledger iteration order. The Scaffold flagged this; The Salt
inherits it and makes ties *more* common. Neither campaign enforces it with
a test. The honest options are to accept it explicitly (as here) or to spend
a task pinning it — flagged rather than decided.

**M2 is a deliberate change to a merged campaign's result.** Stated here so
that if the reversal is unwelcome, it is cheaper to revisit D2 now than after
the census has been regenerated.
