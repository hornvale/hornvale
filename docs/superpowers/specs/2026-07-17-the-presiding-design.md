# The Presiding — a world's religion should belong to the people who live in it

**Campaign:** The Presiding
**Row:** SKY-25 — presiding-belief selection is dominance-blind
**Date:** 2026-07-17
**Status:** spec, awaiting G3

## 1. What SKY-25 says, and what is actually true

SKY-25 (banked by The Terminator) says the presiding belief is chosen by
species-registry alphabetical order plus the founder-floor flagship
guarantee, independent of dominance, and frames it as the second of **two**
gates on SKY-5's tide-religion payoff: fix habitability (SKY-24, shipped),
then fix this, and locked worlds head the tide again.

Measured on the merged tree (epoch v4 + ECS c3), that framing is wrong in
three ways. The underlying bug is real; its story and its promise are not.

### 1.1 The species is goblin, not bugbear — in three documents

SKY-25's row, `windows/lab/tests/terminator_acceptance.rs`'s diagnosis, and
`a_frozen_sky_never_heads_a_cyclic_pantheon`'s doc comment all state that the
founder floor guarantees **bugbear** a flagship on every seed, so bugbear's
pantheon commits first.

Bugbear places a flagship on **0 of 9** of the battery's own locked seeds
(8, 13, 70, 78, 80, 95, 116, 145, 183), and **0 of 30** seeds in 1–30. The
first committer is **goblin**, everywhere. Registry-first is not placed-first
(see The Named's retro; this is the same inference, made a third time).

### 1.2 The real mechanism is sharper than the row's

On every naturally-locked seed, the founder floor places exactly **one**
goblin, and that single soul's pantheon presides over a world holding 6–27
hobgoblins:

```
seed | chief settlements (species:pop)  | presides
   8 | goblin:1  hobgoblin:23 kobold:4  | goblin
  13 | goblin:1  hobgoblin:19           | goblin
  78 | goblin:1  hobgoblin:27           | goblin
 183 | goblin:1  hobgoblin:12 kobold:1  | goblin
```

Across spinning seeds 1–24, the alphabetically-first species differs from the
most populous on **22 of 22** worlds — goblin is population 1 on every one.
This is not an edge case. It is every world Hornvale generates.

### 1.3 The fix does NOT restore the tide — measured, not predicted

This is the finding that reshapes the campaign. On all 9 locked seeds,
**hobgoblin's own pantheon also heads Eternal**, not the tide. Making the
presiding belief dominance-aware swaps goblin → hobgoblin and the reading
stays `eternal`: **0/9 → 0/9**. No species heads Ambient on any locked seed.

Tide beliefs are still committed (locked seed 8 holds six of them) — they
simply never head anything. The binding gate on SKY-5 is the
**ambient-extinction** movement (`belief-kind` ambient 69 → 0 at the
2026-07-16 regen), already pinned honestly in `calibration.rs` and already
under a named investigation (rift-and-fit ledger #14/#19).

**This campaign therefore does not promise the tide.** Speccing it as SKY-5's
gate would set up a third falsification.

### 1.4 The metric never claimed what the prose claims

`belief-kind`'s own doc reads *"The first belief's sentiment tag"* — honest
and accurate. Nothing in the code calls it "presiding". The overreading lives
entirely in SKY-5's row, SKY-25's row, and the calibration's comment. There
is **no in-world consumer**: the almanac renders per-species pantheon blocks
(The Named), and the REPL's `beliefs` seam reads the flat list. "The world's
presiding belief" exists only as a census summary and the prose around it.

## 2. What is actually wrong

A world with four peoples has four pantheons. `beliefs_of(&world).first()` is
"whichever belief was minted first", and mint order follows `KindId`-ascending
component iteration. It is a fact about a loop, not about the world — the same
shape as The Named's `i == 0`, one layer up, in the metric instead of the
renderer.

The defensible world-level question is not *which belief was minted first* but
*whose belief would a visitor meet* — and that is the belief of the people who
actually live there.

## 3. The design

**A world's presiding belief is the head belief of its most populous people.**

### 3.1 `presiding_belief` lives in the composition root

```rust
/// The head belief of the world's most populous people — whose religion a
/// visitor would actually meet. Ranked by total committed population across
/// each peopled kind's settlements, descending; ties broken by `KindId`
/// ascending (determinism: no wall clock, no float order).
pub fn presiding_belief(world: &World) -> Option<hornvale_religion::Belief>
```

It **must** live in `windows/worldgen`, not `domains/religion`: religion
depends on `hornvale-kernel` and nothing else (constitutional layering), so it
cannot see settlement populations. The composition root is the only place
religion and demography legally meet.

`beliefs_of` is **not** touched. It keeps returning ledger order, which is
what it honestly is.

### 3.2 A new metric, not a redefined one

Add `presiding-belief-kind`. **Do not redefine `belief-kind`.** Silently
changing what a committed census column means is precisely how a drift check
freezes a wrong number in place (The Named's headline lesson): every
regeneration would re-ratify the new meaning against pins measured under the
old one, and nothing would turn red. `belief-kind` stays as-is — an honest
"first-minted" reading whose existing pins remain valid.

### 3.3 A live battery, not a census claim

The dominance property is proved by a **live** acceptance battery
(`windows/lab/tests/presiding_acceptance.rs`), modelled on
`terminator_acceptance.rs`: build worlds through the real pipeline and assert
`presiding_belief` names the most populous people. This measures reality now
rather than waiting on a regen, exactly as that battery's own docstring argues.

The battery also pins the **negative** result honestly (ADR 0016): on the 9
locked seeds, presiding-by-dominance still yields Eternal, 0/9 Ambient. The
campaign records that its fix does not move SKY-5, so no future reader has to
rediscover it.

## 4. Blast radius

**No epoch.** `presiding_belief` is a derived view over existing facts — no
seed derivation, no stream label, no draw order, no mint order, no ledger
write. Worlds stay byte-identical. (Changing *mint* order would have been an
epoch: entity ids are sequential. Deriving instead of reordering avoids it —
UNI-20's derive-never-store, cashed.)

**Census regen: REQUIRED, and it is a carve-out.** A new metric adds a column
to `the-census`, so the committed fixture no longer matches a live run. This
does not redden `make gate` (`fixture_staleness` is `#[ignore]`d to the heavy
tier) but it **does** redden `make gate-full` and CI's regenerate-and-diff.
That is the same accepted-debt shape as The Reckoning's 633/1000 staleness.
**Nathan's explicit authorization is required** — see G3 flag 1.

## 5. Correcting the record

- **SKY-25** — flip to `shipped`, repoint **Where**, and correct: goblin not
  bugbear; the pop-1 founder-floor mechanism; and that dominance-awareness
  does **not** restore the tide (0/9 → 0/9 measured).
- **SKY-24's row** — its "second, separate dominance-blindness the fix never
  touches" sentence is right; its bugbear attribution is not.
- **`terminator_acceptance.rs`** — correct the bugbear diagnosis in place;
  its measured 0/9 pin stands untouched and correct.
- **`a_frozen_sky_never_heads_a_cyclic_pantheon`** — its bugbear comment and
  its stale "37 tide-headed + 11 sun-headed" prose (the assertion below it
  already pins the true (48, 0)).

## 6. Testing

1. `presiding_belief` names the most populous people — live, over seeds 1–24,
   where alphabetical-first ≠ dominant on 22/22.
2. **Mutation-verified**: ranking by `KindId` instead of population must fail
   the battery. A test that passes under both rankings has not tested the fix
   (The Named shipped a vacuous invariant test; this is that lesson applied).
3. Determinism: equal populations tie-break `KindId`-ascending; same seed →
   same presiding belief across runs.
4. `beliefs_of` order is unchanged (byte-identity of the flat seam).
5. The honest negative: 0/9 locked seeds head Ambient under dominance ranking.

## 7. Out of scope

- **The ambient extinction** (`belief-kind` ambient 69 → 0) — the real gate on
  SKY-5, owned by rift-and-fit ledger #14/#19.
- **Retiring the world-level metric entirely** — see G3 flag 2.
- **Why goblin is population 1 on every seed.** The founder floor guarantees a
  flagship; nothing guarantees it grows. Whether a permanently-one-soul people
  is correct is a demography question, not a religion one. Recorded as a
  followup.

## 8. Definition of Done

- [ ] `make gate` green; `presiding_acceptance` battery green and
      mutation-verified.
- [ ] Chronicle + retrospective; SKY-25 flipped; the three stale documents
      corrected.
- [ ] Census regen authorized and run, or its staleness accepted explicitly.
- [ ] Confidence Gradient: no bet moves — no re-score (0030).
