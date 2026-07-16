# The Named — attribution is a property, not a position

**Campaign:** The Named
**Ticket:** hornvale#1 — "Almanac: lone non-goblin pantheon renders with the
anonymous legacy lead (third-species prerequisite)"
**Date:** 2026-07-16
**Status:** spec, awaiting G3

## 1. The problem

The almanac's **Gods** section chooses each pantheon's lead by its *index*:

```rust
// windows/almanac/src/lib.rs:309
if i == 0 { /* anonymous legacy lead */ } else { /* species-qualified lead */ }
```

The anonymous lead exists to preserve byte-identity with pre-species worlds.
Pinning it to index 0 assumed the registry-first species owns block 0, and
that the registry-first species is goblin.

Both halves of that assumption have expired:

- `domains/species/src/lib.rs:783` documents the species registry as a
  `BTreeMap` ordered **alphabetically**. With the roster now at four peopled
  species — **bugbear**, goblin, hobgoblin, kobold — bugbear sorts first.
- `windows/worldgen/src/lib.rs almanac_context` pushes one block per species
  in that order, so block 0 is simply *the alphabetically-first species that
  happens to hold beliefs* — a fact about iteration, not about the world.

### Evidence: this is live, not theoretical

The ticket filed this as "benign (do not fix yet)", gated on a third species
landing. Four peopled species have since landed. Sampling seeds confirms the
predicted rendering is in production today:

```
seed  2: placed=[goblin,hobgoblin,kobold]  pantheons=3  block0=goblin ANONYMOUS
seed  7: placed=[goblin,hobgoblin,kobold]  pantheons=3  block0=goblin ANONYMOUS
seed 42: placed=[goblin,hobgoblin]         pantheons=2  block0=goblin ANONYMOUS
```

Seed 2's Gods section reads, in order:

```
An organized priesthood tends a pantheon:                                  <- goblin, unattributed
In the legion of **Woogwaoweobwoaljeovzeof**, an organized priesthood ...  <- hobgoblin
In the warren of **Qshashngdashksashngkoshqsho**, an organized priesthood ...  <- kobold
```

Goblin's pantheon is the only one stripped of its species and settlement,
and nothing in the block says so. The Campaign Y2-2 retrospective predicted
exactly this: *"No pinned seed exercises this path, so it shipped
undetected — recorded here as a spec note for whichever campaign next
touches the Gods renderer."* This is that campaign.

## 2. The design

**Governing sentence: attribution is a property a pantheon has, never a
place it sits.**

### 2.1 Type the discriminator (`windows/almanac`)

`PantheonBlock` currently signals "legacy" with three empty-string
sentinels — `species: String::new()`, `noun`, `settlement` — documented as
"empty for legacy saves that predate species facts". The discriminator
already exists in the data; `render` was free to ignore it, so it did.
Promote it so the illegal state is unrepresentable:

```rust
/// The people a pantheon belongs to. Absent for legacy saves that predate
/// species facts, and for worlds with too few peoples to disambiguate.
pub struct PantheonAttribution {
    pub species: String,
    pub noun: String,
    pub settlement: String,
}

pub struct PantheonBlock {
    pub attribution: Option<PantheonAttribution>,
    pub cult_form: Option<String>,
    pub beliefs: Vec<BeliefLine>,
}
```

### 2.2 Make `render` homogeneous

No index, no special case. Every block renders by its own content:

- `attribution: None` → the anonymous lead, emitted only when `cult_form` is
  `Some` (unchanged — this is the legacy byte-identity contract; see §7).
- `attribution: Some(a)` → the species-qualified lead, organized/folk
  wording exactly as today.

The `enumerate()` disappears. This is strictly less code than the branch it
replaces.

### 2.3 The builder owns the policy (`windows/worldgen`)

`render` stays dumb; the composition root decides. The pantheons builder
sets `attribution: Some(..)` iff the world has **two or more placed peopled
species**, and `None` otherwise. The legacy fallback always sets `None`.

### 2.4 One predicate, shared

`settlement_lines` already computes `let multi_species = flagships.len() > 1`
for the People section. Factor it into a single helper consumed by both
`settlement_lines` and the pantheons builder. Two copies would let the two
sections drift apart again — which is the whole bug.

## 3. Why cardinality, not "legacy-only" — a divergence from the ticket

The ticket prescribes: *"the anonymous legacy lead is used only for a
genuine pre-species/legacy world (no `peopled-by` facts) … Any pantheon
attributable to a species should carry its species-qualified lead."*

**This spec deliberately does not do that.** Three reasons:

1. **Precedent.** The sibling People section already answers this question
   by cardinality, deliberately and in a doc comment: *"A world with exactly
   one such species keeps the legacy unprefixed wording — byte-stable for
   goblin-only worlds; two-or-more-species worlds prefix each chief line
   with its species."* The ticket's rule would have the Gods section name
   goblin on a world where the People section, two paragraphs above,
   pointedly does not. One almanac, one voice.

2. **Prose semantics.** The attributed lead is *contrastive*: "the village
   of **X** keeps **its own** folk pantheon" presupposes another pantheon to
   own a different one. In a one-people world it contrasts with nothing.

3. **Blast radius.** Cardinality preserves byte-identity for legacy *and*
   goblin-only worlds. The ticket's rule changes both, for no reader gain.

The almanac's convention is: **name the species only when there is more than
one to distinguish.** This campaign extends that existing convention to the
Gods section rather than inventing a second rule for it.

### 3.1 Why not naive pantheon-count

A tempting simplification — anonymous iff exactly one *pantheon* renders —
is wrong. If two species place but only one holds beliefs, it renders a
single unnamed pantheon beneath a People section listing two peoples: the
ambiguity this campaign exists to remove. The predicate must be **placed
peopled species**, matching People exactly.

## 4. Resulting semantics

```
placed species | pantheons | lead(s)                        | vs today
---------------+-----------+--------------------------------+-----------
0 (legacy)     | 1         | anonymous                      | unchanged
1 (goblin-only)| 1         | anonymous                      | unchanged
2 (seed 42)    | 2         | BOTH species-qualified         | CHANGED
3 (seed 2, 7)  | 3         | ALL species-qualified          | CHANGED
2              | 1         | species-qualified              | CHANGED
```

## 5. Blast radius

**No stream epoch.** The change touches rendered prose only — no seed
derivation, no stream labels, no draw or consumption order. World ledgers
stay byte-identical. Rebaseline, not epoch (precedent: The Speakable).

**No census regeneration.** Verified rather than assumed: every religion
metric (`pantheon-size`, `cult-form`, `pantheon-verticality`, the
per-species twins) computes from committed facts; no lab metric reads the
almanac renderer, and no census fixture embeds rendered prose. **The AWS
carve-out is not invoked.**

**Artifacts to rebaseline (3):** `book/src/gallery/almanac-seed-42.md`,
`almanac-seed-42-sky.md`, `almanac-seed-42-locked.md` — each gains a
species-qualified lead on its formerly-anonymous goblin block, via the
documented artifact commands in `.github/workflows/ci.yml`.

**Stale comments to correct:** `windows/worldgen/src/lib.rs` says "registry
order, goblin first" in two places (the `settlement_lines` and
`rendered_beliefs` doc comments). Alphabetical; bugbear is first.

## 6. Testing

Two existing tests pin the bug and must be rebaselined — they encode the
defect, not the contract:

- `a_single_pantheon_renders_exactly_as_before` (`:668`) builds **one goblin
  pantheon** and asserts "single-pantheon worlds name no species". Under
  this spec a one-*people* world still renders anonymously, so the assertion
  survives — but its fixture must become a genuinely attribution-less block,
  and the name should say *people*, not *pantheon*.
- `a_second_pantheon_gets_a_species_lead` (`:680`) asserts "goblin block
  renders first, exactly as before" — i.e. block 0 anonymous. Now both
  blocks are named; the ordering assertion stays, the anonymity one goes.

New coverage:

1. A legacy block (`attribution: None`) renders the anonymous lead —
   byte-identical to the pre-species section.
2. A one-people world renders anonymously (People/Gods symmetry).
3. **Two placed species, one pantheon → species-qualified.** The §3.1
   divergence case; the rule that naive pantheon-count would fail.
4. Three pantheons → all three named, none anonymous. The regression test
   hornvale#1 never had — it fails on today's code (seed 2 / seed 7).
5. Round-trip: seeds 172/257 (legacy path) render unchanged.

## 7. Out of scope

- The no-cult-form lead hole — `render`'s anonymous branch emits its lead
  only when `cult_form` is `Some`, so a legacy block with no recorded cult
  form renders its beliefs under no lead at all, while attributed blocks
  always emit one. Preserved deliberately: it is the legacy byte-identity
  contract, and changing it would alter pre-species saves' rendering.
  Recorded as followup 1 so the asymmetry is known, not rediscovered.
- Retiring the anonymous branch (followup 2) — belongs with dropping
  pre-species save support.
- Any change to `rendered_beliefs`' flat ordering (the REPL seam is
  unaffected — it never consulted block index).

## 8. Definition of Done

- [ ] `make gate` green; the three rebaselined artifacts drift-check clean.
- [ ] Book freshness sweep; chronicle entry (`book/src/chronicle/`).
- [ ] Campaign retrospective (`docs/retrospectives/`) carrying followups 1–4,
      including the process lesson: *a deferred ticket whose trigger is
      "when X lands" needs a test that fails when X lands, not a prose note
      asking a future campaign to remember.*
- [ ] hornvale#1 closed with a correction to its stale "registry-first
      species (goblin)" text.
- [ ] Confidence Gradient: no bet moved — no re-score required (0030).
