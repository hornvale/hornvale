# 0074. Capacities are a chain, and species facts gate on components

**Status:** Accepted (2026-07-25) · **Decider:** Nathan · **Refines:** [0065](0065-socialform-and-the-nested-capacity-lattice.md), [0068](0068-society-gates-on-sociality-not-sedentism.md)

Two rulings from *The Vigil*, the campaign that authored perception for the
three chromatic dragons. Both correct a gap between what the Dragons program
*described* and what the code *enforced*.

## 1. The capability lattice is a chain, and the chain is enforced

Decision 0065 described the nested-capacity lattice, and the UNI-31 registry
row states it as **`speech ⊆ perception ⊆ mind`** — a chain. What
`check_integrity` actually enforced was a **fork**: `perception ⊆ psyche` and
`articulation ⊆ psyche`, two independent branches under mind, with no edge
between speech and perception.

The difference was unoccupied, and therefore invisible, until *The Solitary
Tongue* gave the dragons speech without perception — an arrangement the fork
permits and the chain forbids. It shipped only because `exposure_of_impl`
carried a hardcoded goblin-baseline `PerceptionVector` for any speaker without
one of its own, which is also how the goblin's `night-vision 0.5` came to be
printed in the published dictionary as a stated fact about dragons.

**The ruling.** The chain is the design and is now enforced. `check_integrity`
gains:

> for every `k` in `articulation`: `perception.contains(k)`

The direction is deliberate and is *not* equality. `perception ⊆ articulation`
would forbid a future non-speaking perceiver — an owl with eyes and no words —
which is a creature the world should be able to express. `articulation ⊆
perception` forbids only the incoherent case: a speaker with no senses.

The goblin-baseline stopgap is deleted. A missing perception component now
fails loudly with `BuildError::MalformedKind`, matching what
`chorus::account_params_from` already did for the same component. This also
removes a live panic: `observe_with_sources` used `.expect`, so the REPL's
`phenomena --as owlbear` terminated the process.

**Why it matters beyond the three dragons.** Nathan's roster is expected to
grow — metallic dragons, further chromatics, exotic types. Under the fork, a
new speaking kind authored without perception would have silently spoken with a
goblin's eyes. Under the chain it fails at load, with the kind named.

## 2. Species-registry facts gate on the component that produces them

Every fact describing a kind's mind, society, perception, and speech was
emitted behind one gate testing `SocialForm::Settled`. For the four settling
peoples this was invisible; for a `Solitary` minded speaker it withheld
everything — not because the dragon lacked a mind, a voice, or senses, but
because it lacked an address.

This is the same conflation decision 0068 corrected for `SocietyVector`
(sociality, not sedentism), left standing in the other three families. Fixing
perception alone would have reproduced it one field over.

**The ruling.** Each fact family is gated on the component that produces it:

| family | predicates | gated on |
|---|---|---|
| mind | `threat-response`, `deliberation-latency`, `time-horizon` | `psyche` present |
| society | `in-group-radius`, `sociality-mode`, `status-basis` | `society` present |
| perception | `activity-cycle`, `night-vision`, `sky-attention` | `perception` present |
| articulation | `labiality`, `vowel-space`, `voicing`, `sibilance`, `voice-loudness`, `tonality`, `exotic-manner` | `articulation` present |

A dragon therefore emits mind, perception, and speech facts, and no society
facts — absent now because it *keeps* no society, rather than because it fails
to settle. This makes 0068's semantics visible in the ledger for the first
time.

**Emission order is unchanged**, deliberately. The existing sequence
interleaves `in-group-radius` between two mind facts, so the mind block appears
twice rather than being merged. That shape is load-bearing: regrouping by
family would move a fact for four peoples in every world — a save-format change
with no epoch. It is commented as such in `windows/worldgen/src/lib.rs`.

## Determinism — a deliberate, additive world change

Unlike The Eremite, The Cloister, and The Solitary Tongue, this is **not**
byte-identical, and the drift was ratified by Nathan at spec review rather than
discovered.

- No epoch: no seed-derivation label, stream consumption order, or noise
  constant changes.
- No new predicates: all sixteen already existed and were registered.
- No draws move: fact commits consume no randomness and entity minting is
  untouched. Seed 42's village name is unchanged (`Qvooshtvoagootao`).
- The world grows **3514 → 3553 facts**: three dragons × 13.

The committed keystone fixture `cli/tests/fixtures/world-seed-42.json` was
re-baselined in the drifting commit. Its diff is the tightest available
statement of the change: **390 insertions, zero deletions**, thirteen
predicates × three dragons, no society predicates among them — and the
pre-change fact sequence is an in-order *subsequence* of the new one, so every
existing fact survives with unchanged value and unchanged relative position.

No census regeneration: no Lab metric reads ledger fact counts or
species-registry predicates, and no dragon appears in any study or census
golden. The only generated book artifact that moves is
`dictionary-generated.md`.

## Consequences

- Adding a dragon (or any speaking kind) now *requires* authoring perception;
  the omission is a load-time failure, not a silent substitution.
- A non-speaking perceiver remains expressible — the lattice permits
  `perception ⊃ articulation` — so a future campaign giving an owl or a bat
  senses without words is not blocked.
- `exposure_of` and `observed_phenomena_as` now return `Err` for plain fauna
  where they previously fell back or panicked. Both are public.
- The dragons' latent capacities from three prior campaigns become visible
  world state, which is what the next campaign — placement (BIO-35 Stage 2, the
  prey field and the landscape-of-fear cascade) — will read.
