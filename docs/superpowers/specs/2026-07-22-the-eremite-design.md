# The Eremite — SocialForm as a universal axis; the solitary mind

**Campaign:** The Eremite — campaign 2 of the Dragons program (the keystone)
**Registry:** UNI-31 (personhood is a region of component-space, not a node) ·
dissolves the peopled-cluster all-or-none invariant into nested capacities ·
adds `BiosphereTraits.social_form` · authors the three dragons a solitary mind
**Status:** SHIPPED (2026-07-22, chronicle: the-eremite) — SocialForm axis +
nested-capacity lattice + the three dragons' solitary mind; decision 0065; idea
UNI-31 shipped. Byte-identical (the dragon mind is latent until placement),
verified before and after absorbing main. The determinism argument here proved
INCOMPLETE — it covered the re-keyed gates but not the many direct
`psyche_registry` consumers; the blast-radius sweep and its lesson are in the
retrospective. Deferred: dragon perception, the solitary tongue (BIO-37),
per-chromatic differentiation, minds for xorn/otyugh, the PsychVector split.
**Date:** 2026-07-22

---

## 1. Problem

The species model draws one hard line — four peoples who think, perceive, and
speak, and everything else filed as *fauna* — and it draws it in the wrong
place. The line is enforced as an all-or-none bundle: worldgen's
`check_integrity` requires `psyche.ids == perception.ids == articulation.ids ==
lexicon.ids`, so "has a mind" is welded to "perceives" is welded to "speaks" is
welded to "is one of the settling peoples." A dragon breaks the weld: in the
source material it is intelligent, proud, and speaks Draconic — yet it is
**solitary**, building no settlement and forming no people. The model cannot
express it: to give a dragon a mind, you must today make it a settling,
speaking people, which it is not.

*The Wilding* (just merged) made this conflation load-bearing. It agentifies the
wild, but its definition of a beast is literally the conflation:
`is_mobile_beast(label) := psyche_registry().get(label).is_none()` — "a beast is
a thing with no authored mind." So every wild creature walks with the **inert
default temperament**, because carrying a real one would, by the current rules,
stop it being wild at all. The dragon that hunts across a world does so
mindlessly, not because a dragon is mindless but because the model equates
"minded" with "peopled."

This campaign dissolves that line — the keystone of the Dragons program (idea
UNI-31). It makes **social organization a universal axis** distinct from the
**capacities** (mind, perception, speech) a creature may or may not carry, so a
creature can be solitary *and* minded. It then authors the three chromatic
dragons the solitary mind they should have. Perception and language stay
deferred to later campaigns (§8).

## 2. The principle

Every `BiosphereTraits` field is a trait **every** creature has — mass,
metabolic class, niche, potency. Psyche/perception/speech are **capacities**
only some carry, authored as optional components since *The Dissolution*. The
reframe is to stop confusing the two:

- **Universal trait** → lives on `BiosphereTraits`. *Social organization* is
  universal (every creature is solitary, or herds, or settles, or is rooted),
  so it joins the biosphere row.
- **Capacity** → an optional component, gated by nothing but authorship, and
  composing **coherently but freely** (speech implies a mind; a mind does not
  imply speech).

"Personhood" was the name we gave the accident that, for the four peoples, all
of these coincide. It is not a kind of thing; it is a **region** of the
composition space (minded ∧ perceiving ∧ speaking ∧ `Settled`). A dragon stands
in the region next door: minded ∧ solitary.

## 3. Design

### 3.1 `SocialForm` — the universal axis

A new enum, authored per kind, stored on `BiosphereTraits`:

```rust
/// How a creature organizes with its own kind — the universal social axis,
/// distinct from `Sociality` (a peopled society's authority shape). Ordered
/// by permanence of association. Only `Settled` builds settlements.
pub enum SocialForm {
    Sessile,    // rooted; placed, never agentified (autotrophs)
    Solitary,   // lives and ranges alone
    Gregarious, // moves in herds / packs, no fixed place
    Settled,    // forms sedentary communities (the peoples)
    // Colonial (superorganism) — banked; no kind needs it yet.
}
```

`BiosphereTraits` gains `pub social_form: SocialForm`. Authored per kind (like
`metabolic_class`); no draw, no epoch. Every roster kind's value:

```
kind             social_form    rationale
--------------   -----------    -------------------------------------------
goblin           Settled        the four peoples build settlements
kobold           Settled
hobgoblin        Settled
bugbear          Settled
--------------   -----------
treant           Sessile        autotroph, rooted
twig-blight      Sessile        autotroph, rooted
--------------   -----------
giant-elk        Gregarious     herd herbivore
woolly-mammoth   Gregarious     herd herbivore
giant-goat       Gregarious     herd/band
--------------   -----------
otyugh           Solitary       5E solitary scavenger
xorn             Solitary       5E solitary/pair
rust-monster     Solitary       5E solitary
owlbear          Solitary       5E solitary/pair
white-dragon     Solitary       5E solitary apex
red-dragon       Solitary
black-dragon     Solitary
```

**The byte-identity keystone:** the set `{Settled}` is exactly the four peoples,
which is exactly today's `psyche_registry` key-set. So every gate re-keyed below
selects the identical set it does today (§4).

### 3.2 The nested-capacity invariant

`check_integrity` (`windows/worldgen/src/components.rs`) changes from one
all-equal knot to a coherence lattice — **speech ⟹ a mind; perception ⟹ a
mind; a settling people carries the full cluster**:

- `articulation.ids == lexicon.ids` (the speech pair stays together);
- `perception.ids ⊆ psyche.ids` (only a minded creature perceives);
- `articulation.ids ⊆ psyche.ids` (only a minded creature speaks);
- every `articulation` kind has a `family_of` entry (+ the existing
  forward-proto coherence, unchanged);
- every `psyche` kind has a `biosphere` row (a mind needs a body);
- **`social_form == Settled` ⟹ the kind is in psyche ∧ perception ∧
  articulation ∧ lexicon** (a settling people is the full peopled cluster —
  this preserves exactly what the old all-equal invariant guaranteed for the
  peoples).

The four peoples sit in every store, satisfying every clause (the old all-equal
case). A dragon sits in `psyche` only: `perception`/`articulation` are empty for
it, and `∅ ⊆ psyche` holds; it is `Solitary`, so the Settled-implies-full clause
does not bind it. The invariant now *admits* the minded solitary it used to
reject.

### 3.3 Re-keying the two seams onto `SocialForm`

Both live in `windows/worldgen/src/lib.rs`:

- **`is_mobile_beast`** (wild agentification, ~line 1052): today
  `psyche.get(label).is_none() && !Autotroph`. Becomes
  **`social_form(label) ∈ {Solitary, Gregarious}`** (equivalently `∉ {Sessile,
  Settled}`) — one `SocialForm` read replacing the `¬psyche` + `¬Autotroph`
  pair. Selects the identical set today (the non-people, non-plant kinds).
- **The settlement-genesis roster** (~line 3730, *"the peopled kinds are exactly
  the psyche store's key-set"*): becomes **the `social_form == Settled`
  kinds**. Identical set today (the four peoples). The pin-resolution guard at
  ~line 3737 (`wc.psyche.contains(resolved)`) likewise checks `Settled`.

**`derive_wild_npcs` is not touched.** It already reads
`PsychVector.threat_response` / `time_horizon` from `psyche_registry`
per-agent, defaulting to `0.5` for a kind without a psyche entry
(`windows/vessel/src/liveness.rs:74`). Today that reader never sees a
psyche-carrying wild kind, because `wild ∩ psyche = ∅`. Once a dragon is both
`Solitary` (wild) and psyche-carrying, the existing reader picks up its authored
temperament automatically — the seam The Wilding built, now firing.

### 3.4 The solitary mind — authoring dragon psyche

The three chromatic dragons gain a `psyche_registry` row (`PsychVector`, the
same 6-dim vector the peoples carry; `0.5` is the goblin baseline). Proposed
values, one shared chromatic-dragon temperament (per-chromatic differentiation
— e.g. the white dragon's near-feral, lower cunning — is captured as a
refinement, §8):

```
dim (PsychVector)     dragon   baseline  reading
-------------------   ------   --------  ------------------------------------
threat_response        0.95      0.5     stands / never flees — an apex
in_group_radius        0.05      0.5     "us" = self; utterly insular
time_horizon           0.90      0.5     centuries-long hoarder, patient
deliberation_latency   0.50      0.5     banked dial; left at baseline
sociality           Hierarchic   —       relates by dominance
status_basis          Rank        —       esteems power / the hoard
```

**A note the vector forces (captured, not resolved here):** three dims are
*individual* temperament (`threat_response`, `time_horizon`,
`deliberation_latency`) and three are *society* structure (`in_group_radius`,
`sociality`, `status_basis`). Only the individual dims are read for a wild
agent; the society dims sit **inert** for a `Solitary` creature (nothing reads a
hermit's authority shape). This is harmless now, but it means `PsychVector`
itself mixes two concerns — a candidate future split (individual-mind vs.
social-mind sub-vectors), noted for BIO-37's neighbourhood, not this campaign.

Only the dragons gain a mind this campaign. `xorn` (5E average intelligence) and
`otyugh` (low but present) are plausible future minds; leaving them mindless
keeps the keystone focused and proves the seam on the clear case.

## 4. Determinism & blast radius

**Every shipped world is byte-identical.** The argument is set-equality, not
inspection:

- `{social_form == Settled}` ≡ the four peoples ≡ today's `psyche_registry`
  key-set. So the settlement-genesis roster (§3.3) iterates the identical set,
  in the identical order (ascending label), and emits identical facts.
- `{is_mobile_beast}` under the new rule ≡ `{non-people, non-autotroph}` ≡ the
  old `¬psyche ∧ ¬Autotroph` set. So `wild_concentrations` selects the identical
  beasts; the vessel goldens (`possession-*-seed-42.md`) are unchanged.
- The dragon psyche is **latent**: a dragon is read as a wild agent only if it
  *places* (appears in `wild_concentrations`), and the mighty menagerie is
  unplaced (obligate-apex `ANIMAL_PREY` carrying-capacity is a later demography
  campaign). So no committed world, almanac, or possession golden shows a dragon
  agent, and the authored temperament changes no bytes today — it is correct and
  waiting, exactly as potency is after The Assay.

**Verification is a required first task** (not an assumption): regenerate and
diff the vessel/possession goldens and the full artifact set; confirm clean.
`SocialForm` is a new serialized field on `BiosphereTraits` — **confirm whether
`BiosphereTraits` is serialized into any committed artifact**; if it is, the new
field is an additive schema change to audit (the biosphere registry is authored
data, re-derived at build, so this is expected to be a pure in-memory addition,
but it is checked, not assumed). No new seed draw, no stream label, no epoch.

## 5. Model-card delta

`social_form` joins the biosphere as a new **authored** universal dimension. The
species chapter's model card gains a line: social organization is authored per
kind, orthogonal to the mind/perception/speech capacities; the peopled cluster
is no longer all-or-none but a nested lattice (speech ⊆ perception ⊆ mind;
`Settled` ⟹ full cluster).

## 6. Test plan

- **`SocialForm` authoring (new unit test, species):** every roster kind has the
  authored `social_form` of §3.1; `{Settled}` equals the psyche-registry
  key-set (the byte-identity guard — this test *is* the determinism argument,
  and reddens if a future kind desyncs the two sets).
- **Nested invariant (new unit tests, worldgen `components`):** a fixture kind
  with psyche-but-no-speech **passes** `check_integrity` (the new capability);
  a kind with speech-but-no-psyche **fails** (speech ⊆ mind); a `Settled` kind
  missing any peopled component **fails** (Settled ⟹ full cluster). These are
  the mutation-resistant assertions the restructure must earn.
- **Dragon mind (new unit test, species):** each dragon carries the §3.4
  `PsychVector`; `threat_response` and `time_horizon` are the authored values,
  not the `0.5` default.
- **Re-key equivalence (new worldgen test):** the `is_mobile_beast` /
  settlement-roster sets computed via `SocialForm` equal the sets the old
  `psyche`-keyed predicates produced (pin the byte-identity at the seam).
- **Existing suites:** the peopled-cluster tests, `wild_concentrations`, the
  possession/vessel batteries, and the full `make gate` — all green with no
  golden change (§4). Vessel/possession goldens regenerated and diffed clean in
  Task 1.

## 7. Proposed decision (ratify at close)

**00NN — SocialForm is a universal biosphere axis; capacities are a nested
lattice.** `BiosphereTraits.social_form: SocialForm` (Sessile/Solitary/
Gregarious/Settled) is authored per kind and is the universal social axis;
`Settled` is the sole settlement-forming value and re-keys the settlement roster
and wild-agentification (replacing the "has a psyche" proxy). Mind, perception,
and speech become nested capacities (speech ⊆ perception ⊆ mind; `Settled` ⟹ the
full peopled cluster), so a creature may carry a mind without speech. Supersedes
the all-or-none peopled-cluster invariant from *The Dissolution*.

## 8. Non-goals (deferred, captured)

- **Perception & the solitary tongue** — dragons gain no perception or language
  here; those are the Dragons program's later campaigns (perception; then BIO-37
  the solitary tongue, where language-drift becomes a function of `SocialForm`).
- **Per-chromatic dragon differentiation** — one shared temperament ships; the
  white dragon's near-feral lower cunning (and red's tyranny, black's cruelty)
  is a fidelity refinement, captured for a follow-up.
- **Minds for other creatures** — xorn (average Int) and otyugh are plausible
  future minds; the seam this campaign builds makes each a cheap data addition.
- **Splitting `PsychVector`** into individual-mind vs. social-mind sub-vectors
  (§3.4's inert-society-dims observation) — **endorsed by Nathan (2026-07-22)**
  as a future direction, provisional names `SocioVector` / `SocialPsychVector`
  (possibly both: an individual-temperament vector every minded creature
  carries, and a social-mind vector only communal/`Settled` creatures do — the
  society dims a solitary leaves inert would move there, so a dragon carries
  only the individual vector). Noted, not undertaken here; a candidate campaign
  once a second minded solitary or the social-mind read makes it pay.
- **Placing the menagerie** — the obligate-apex `ANIMAL_PREY` carrying-capacity
  that would make dragons *appear* is a demography concern (BIO-35 Stage 2), not
  this program; the dragon mind stays latent until it lands.
