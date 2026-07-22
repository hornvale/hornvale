# 0065. SocialForm is a universal axis; capacities are a nested lattice

**Status:** Accepted (2026-07-22) · **Decider:** Nathan

In the context of [The Eremite](../../book/src/chronicle/the-eremite.md) — the
keystone of the Dragons program (idea-registry UNI-31) — we dissolved the
all-or-none "peopled cluster" and decoupled **social organization** from the
**capacities** a creature may carry.

**The decision.**

1. **`BiosphereTraits.social_form: SocialForm`** — a new universal biosphere
   dimension, authored per kind: `Sessile` (rooted autotrophs), `Solitary`,
   `Gregarious` (herds/packs), `Settled` (the settling peoples). `Colonial` is
   banked. **`Settled` is the sole settlement-forming value** and is the
   successor to the retired "has a psyche entry" proxy for peoplehood.

2. **Capacities compose as a nested lattice, not all-or-none.** `check_integrity`
   now enforces `articulation.ids == lexicon.ids`, `perception ⊆ psyche`,
   `articulation ⊆ psyche` (speech and perception each presuppose a mind),
   family for every speaker, a biosphere row for every minded kind, and
   **`Settled ⟹ the full peopled cluster`**. A creature may therefore carry a
   **mind without perception or speech**. This supersedes the all-equal
   peopled-cluster invariant from *The Dissolution* (the ECS program's third
   campaign).

3. **Every "has a psyche ⟹ is a settling people" proxy is re-keyed onto
   `Settled`** (settlement genesis roster, wild-agentification, prey-base,
   carrying-capacity aggregates); every "psyche ⟹ speaks" proxy is re-keyed onto
   the **articulation** (speaker) registry. `psyche_registry` now holds the
   *minded* set (peoples **and** the three dragons), not the peoples alone.

4. **The three chromatic dragons carry an authored `PsychVector`** — a shared
   solitary temperament (stands rather than flees, insular, patient; Hierarchic /
   Rank) — and **no** perception or speech. Only the dragons gain a mind this
   campaign (xorn/otyugh are plausible later additions).

**Determinism.** Byte-identical: `{social_form == Settled}` equals the four
peoples equals the pre-campaign `psyche_registry` key-set, so every re-keyed gate
selects the identical set; the genesis species-facts pass is `Settled`-gated so a
dragon emits only its `SPECIES_NAME` (as before); and dragons are unplaced, so
the authored mind is **latent** — read only if a dragon is ever agentified.
Verified: `regenerate-artifacts.sh` then `git diff` is clean, both before and
after absorbing main.

**Consequence — the blast radius.** Adding the dragons to `psyche_registry`
perturbed every consumer that had encoded `psyche == the peoples`: the genesis
species-facts pass (a `.expect` on the old all-equal cluster), the CLI
dictionary/phonology/audio/repl surfaces, the calibration carrying-capacity
aggregates, and the cognate renderer (the dragons' shared `"draconic"` family
cleared a guard and indexed a missing lexicon). All were re-keyed onto `Settled`
or the speaker set; see the retrospective for the process lesson.

**Scope.** This settles the sociality axis, the nested invariant, and the dragon
mind. **Deferred:** dragon perception and the solitary tongue (BIO-37; language
drift as a function of `SocialForm`); per-chromatic dragon differentiation; minds
for xorn/otyugh; and a possible split of `PsychVector` into individual-mind and
social-mind sub-vectors (`SocioVector` / `SocialPsychVector`, Nathan-endorsed).

**See also.** [The Eremite](../../book/src/chronicle/the-eremite.md) chronicle
and its [spec](../superpowers/specs/2026-07-22-the-eremite-design.md); decision
0064 (potency = CR/30, the program's warm-up); the ECS program metaplan (UNI-22);
`domains/species/src/lib.rs` (`SocialForm`, `biosphere_registry`,
`psyche_registry`); `windows/worldgen/src/components.rs` (`check_integrity`).
