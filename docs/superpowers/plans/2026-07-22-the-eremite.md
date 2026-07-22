# The Eremite Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make social organization a universal `BiosphereTraits.social_form` axis distinct from the mind/perception/speech capacities, dissolve the all-or-none peopled-cluster invariant into a nested lattice, re-key every "has a psyche ⇒ is a people" proxy onto `Settled`, and author the three dragons a solitary mind.

**Architecture:** A new `SocialForm` enum + `BiosphereTraits` field (authored per kind); a rewritten `check_integrity` (nested capacities); the same `Settled`-set substituted for the psyche key-set at every peoplehood gate; three new `psyche_registry` rows for the dragons. `derive_wild_npcs` is untouched — it already reads psyche-if-present.

**Tech Stack:** Rust (edition 2024), `cargo nextest` / `cargo test`, `make gate`.

## Global Constraints

- Determinism is constitutional. **Byte-identity rests on one set-equality:** `{social_form == Settled}` must equal the four peoples, which equals today's `psyche_registry` key-set. Every re-keyed gate then selects the identical set. Verify — do not assume (Task 5 regenerates and diffs every artifact incl. the vessel/possession goldens).
- `BiosphereTraits` is NOT serde (`#[derive(Clone, Debug, PartialEq)]`, decision 0002 — domain crates don't serialize; the window boundary does). Adding `social_form` is a pure in-memory change; if a golden moves, something else is wrong.
- Dragons stay **latent** (unplaced): the authored dragon mind changes no bytes today. No new seed draw, no stream label, no epoch.
- `serde`/`serde_json` only; no new crates. No `HashMap`/`HashSet`. `#![warn(missing_docs)]` — every new public item/variant gets a doc line. `cargo fmt` before every commit.
- **`SocialForm` variants:** `Sessile, Solitary, Gregarious, Settled` (Colonial banked, not added). **Only `Settled` gates behavior.**
- Task ordering is load-bearing: **1 → 2 → 3 → 4**. Adding dragon psyche (4) before re-keying the settlement roster (3) would route dragons into settlement genesis. Keep the order.
- End every commit message with the `Claude-Session:` trailer.

---

### Task 1: `SocialForm` — the universal axis, authored per kind

**Files:**
- Modify: `domains/species/src/lib.rs` (new enum; `BiosphereTraits` field; 16 authored values; the `biosphere_registry` fn doc)
- Modify: `windows/lab/src/roster.rs` (the awakened-owlbear synthetic constructs `BiosphereTraits` — add the field)
- Create: `domains/species/tests/social_form.rs`

**Interfaces:**
- Produces: `hornvale_species::SocialForm` (enum); `BiosphereTraits.social_form: SocialForm`.

- [ ] **Step 1: Write the failing test** — `domains/species/tests/social_form.rs`:

```rust
//! The Eremite (Dragons program C2): SocialForm is a universal biosphere axis.
//! The determinism keystone lives here — {Settled} must equal the psyche
//! key-set, so every gate re-keyed off psyche onto Settled selects the same set.

use hornvale_kernel::KindId;
use hornvale_species::{SocialForm, biosphere_registry, psyche_registry};

fn social_form_of(name: &'static str) -> SocialForm {
    biosphere_registry()
        .get(&KindId(name))
        .unwrap_or_else(|| panic!("{name} has a biosphere row"))
        .social_form
}

#[test]
fn every_kind_has_the_authored_social_form() {
    let expected: &[(&str, SocialForm)] = &[
        ("goblin", SocialForm::Settled),
        ("kobold", SocialForm::Settled),
        ("hobgoblin", SocialForm::Settled),
        ("bugbear", SocialForm::Settled),
        ("treant", SocialForm::Sessile),
        ("twig-blight", SocialForm::Sessile),
        ("giant-elk", SocialForm::Gregarious),
        ("woolly-mammoth", SocialForm::Gregarious),
        ("giant-goat", SocialForm::Gregarious),
        ("otyugh", SocialForm::Solitary),
        ("xorn", SocialForm::Solitary),
        ("rust-monster", SocialForm::Solitary),
        ("owlbear", SocialForm::Solitary),
        ("white-dragon", SocialForm::Solitary),
        ("red-dragon", SocialForm::Solitary),
        ("black-dragon", SocialForm::Solitary),
    ];
    for (name, sf) in expected {
        assert_eq!(social_form_of(name), *sf, "{name}");
    }
}

#[test]
fn settled_set_equals_the_psyche_key_set() {
    // The byte-identity keystone: re-keying any peoplehood gate from
    // "has a psyche" onto "is Settled" is a no-op iff these two sets are equal.
    let bio = biosphere_registry();
    let psy = psyche_registry();
    let settled: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| k.0)
        .collect();
    let peopled: Vec<&str> = psy.ids().map(|k| k.0).collect();
    assert_eq!(settled, peopled, "{{Settled}} must equal the psyche key-set");
}
```

- [ ] **Step 2: Run — expect failure** (`SocialForm` undefined):
Run: `cargo test -p hornvale-species --test social_form`
Expected: compile error, `cannot find type SocialForm`.

- [ ] **Step 3: Add the enum** — in `domains/species/src/lib.rs`, near the other species enums (after `Sociality`/`StatusBasis`/`ActivityCycle`):

```rust
/// How a creature organizes with its own kind — the universal social axis,
/// distinct from [`Sociality`] (a peopled society's *authority* shape).
/// Ordered by permanence of association. Only `Settled` builds settlements;
/// re-keying a "has a mind ⇒ is a people" proxy onto `Settled` is what lets a
/// solitary creature carry a mind without being a settling people.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SocialForm {
    /// Rooted; placed on the map, never agentified (autotrophs).
    Sessile,
    /// Lives and ranges alone (a dragon, a xorn).
    Solitary,
    /// Moves in herds or packs, forming no fixed place (an elk herd).
    Gregarious,
    /// Forms sedentary communities — the settling peoples.
    Settled,
}
```

- [ ] **Step 4: Add the field** — in `BiosphereTraits`, after `potency`:

```rust
    /// How this creature organizes socially (universal; every kind carries
    /// one). `Settled` is the sole settlement-forming value and the successor
    /// to the old "has a psyche entry" proxy for peoplehood.
    /// type-audit: bare-ok(identifier-text: social_form)
    pub social_form: SocialForm,
```

- [ ] **Step 5: Author `social_form` on all 16 `biosphere_registry` rows** — add the field to each `BiosphereTraits { … }` literal, per §3.1: `goblin/kobold/hobgoblin/bugbear` → `SocialForm::Settled`; `treant/twig-blight` → `SocialForm::Sessile`; `giant-elk/woolly-mammoth/giant-goat` → `SocialForm::Gregarious`; `otyugh/xorn/rust-monster/owlbear/white-dragon/red-dragon/black-dragon` → `SocialForm::Solitary`. Update the `biosphere_registry` fn doc to mention `social_form` as an authored universal dimension.

- [ ] **Step 6: Fix the Lab synthetic** — `windows/lab/src/roster.rs` builds `awakened_traits` by cloning the owlbear's biosphere (`.clone()`), so it inherits `social_form` automatically — **verify no other site constructs `BiosphereTraits` from scratch**: `grep -rn "BiosphereTraits {" domains windows --include="*.rs"`. Add `social_form:` to any literal the grep finds (expected: only `biosphere_registry`).

- [ ] **Step 7: Run the whole species suite + build the workspace:**
Run: `cargo test -p hornvale-species && cargo build --workspace`
Expected: `social_form` tests pass; workspace compiles (the field is added everywhere it is constructed).

- [ ] **Step 8: fmt + commit:**
```bash
cargo fmt
git add domains/species/src/lib.rs domains/species/tests/social_form.rs windows/lab/src/roster.rs
git commit -m "feat(species): SocialForm — the universal social axis (the-eremite)

Add SocialForm (Sessile/Solitary/Gregarious/Settled) as a BiosphereTraits
field, authored per kind. {Settled} == the four peoples == today's psyche
key-set (the byte-identity keystone, pinned by a test), so the later re-keys
off psyche onto Settled are no-ops. Not serde; pure in-memory.

Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ"
```

---

### Task 2: The nested-capacity invariant

**Files:**
- Modify: `windows/worldgen/src/components.rs` (`check_integrity` + its tests)

**Interfaces:**
- Consumes: `SocialForm` (Task 1), `BiosphereTraits.social_form`.

- [ ] **Step 1: Write the failing tests** — add to the `#[cfg(test)] mod tests` in `components.rs`, three cases (use the existing `from_stores`/fixture idiom already in that module; a helper that assembles stores from the canonical registries then mutates one):
  1. `psyche_without_speech_passes` — a kind added to `psyche` (and given a biosphere row with `social_form: Solitary`) but **not** to perception/articulation/lexicon → `check_integrity` returns `Ok`.
  2. `speech_without_psyche_fails` — a kind in `articulation`+`lexicon` but **not** `psyche` → `Err(MalformedKind)`.
  3. `settled_missing_a_peopled_component_fails` — a kind with `social_form: Settled` and a biosphere row but absent from `articulation` → `Err(MalformedKind)`.

(Write these against the same `check_integrity(&biosphere, &psyche, &perception, &articulation, &lexicon, &family_proto, &family_of)` signature the module already tests; `check_integrity` gains no new parameter — it reads `social_form` from the `biosphere` it already receives.)

- [ ] **Step 2: Run — expect failure** (the old all-equal invariant rejects case 1, accepts cases 2/3):
Run: `cargo test -p hornvale-worldgen --lib components`
Expected: `psyche_without_speech_passes` FAILS (old invariant demands equal key-sets).

- [ ] **Step 3: Rewrite `check_integrity`** — replace the three `psyche.ids().eq(...)` all-equal checks and the `for k in psyche.ids()` biosphere/family block with the nested lattice (keep the forward-proto coherence block below it unchanged):

```rust
    // Nested capacities (The Eremite): speech ⊆ mind, perception ⊆ mind, and a
    // Settled people carries the full peopled cluster. A creature may carry a
    // mind (psyche) without perception or speech — the solitary minded.
    if !articulation.ids().eq(lexicon.ids()) {
        return Err(BuildError::MalformedKind(
            "articulation and lexicon registries must share one key-set".into(),
        ));
    }
    for k in perception.ids() {
        if !psyche.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "kind {k:?} perceives but has no mind (psyche)"
            )));
        }
    }
    for k in articulation.ids() {
        if !psyche.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "kind {k:?} speaks but has no mind (psyche)"
            )));
        }
        if !family_of.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "speaking kind {k:?} has no family"
            )));
        }
    }
    for k in psyche.ids() {
        if !biosphere.contains(k) {
            return Err(BuildError::MalformedKind(format!(
                "minded kind {k:?} has no biosphere component"
            )));
        }
    }
    // A settling people is the full peopled cluster (what the old all-equal
    // invariant guaranteed for the peoples, now scoped to Settled).
    for (k, bio) in biosphere.iter() {
        if bio.social_form == hornvale_species::SocialForm::Settled
            && !(psyche.contains(k)
                && perception.contains(k)
                && articulation.contains(k)
                && lexicon.contains(k))
        {
            return Err(BuildError::MalformedKind(format!(
                "Settled kind {k:?} is missing a peopled component"
            )));
        }
    }
```

- [ ] **Step 4: Run the components suite:**
Run: `cargo test -p hornvale-worldgen --lib components`
Expected: all three new tests pass; `canonical_registries_pass_integrity` still passes (the four peoples satisfy every clause).

- [ ] **Step 5: fmt + commit** (`feat(worldgen): nested-capacity invariant — mind without speech (the-eremite)` + trailer).

---

### Task 3: Re-key every peoplehood proxy onto `SocialForm`

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (four production gates + one test invariant)
- Modify: `windows/vessel/src/liveness.rs` (one test invariant)

**Interfaces:**
- Consumes: `SocialForm` (Task 1). Introduce one private helper in `lib.rs` to avoid repetition:

```rust
/// Whether a kind settles (forms villages) — the successor to the "has a
/// psyche entry" peoplehood proxy. Reads the universal `social_form`.
fn is_settled(wc: &WorldComponents, label: &str) -> bool {
    wc.biosphere
        .get_by_label(label)
        .is_some_and(|b| b.social_form == hornvale_species::SocialForm::Settled)
}
```

(If a site has the `biosphere` store but not `wc`, read `biosphere.get_by_label(label)` directly — match the call site.)

- [ ] **Step 1: Re-key the four production gates** in `windows/worldgen/src/lib.rs` (each is byte-identical because `{Settled} == {psyche}`):
  - **~line 997 (prey base):** `&& psyche.get_by_label(kind.0).is_none()` → `&& bio.social_form != SocialForm::Settled` (the loop already binds `bio`; drop the now-unused `let psyche = …` if nothing else uses it, and the separate `!Autotroph` check stays or folds into `matches!(bio.social_form, Solitary|Gregarious)` — keep behavior identical).
  - **~line 1056 (`is_mobile_beast`):** `psyche.get_by_label(label).is_none() && !Autotroph` → `matches!(social_form, SocialForm::Solitary | SocialForm::Gregarious)`, reading `social_form` from `biosphere.get_by_label(label)`.
  - **~line 3734 (settlement roster default):** `None => wc.psyche.ids().map(|k| k.0).collect()` → iterate `wc.biosphere` for `social_form == Settled`, ascending `KindId` (BTreeMap order — same order the psyche key-set gave): `None => wc.biosphere.iter().filter(|(_, b)| b.social_form == SocialForm::Settled).map(|(k, _)| k.0).collect()`.
  - **~line 3737 (settlement pin check):** `if !wc.psyche.contains(&KindId(resolved))` → `if !is_settled(wc, resolved)`; keep the same `BuildError::Pins("'{name}' is not a settling people …")`.

- [ ] **Step 2: Re-state the two wild-invariant test assertions** (they pass on seed-42 data — no dragon places — but assert the wrong invariant now):
  - `windows/worldgen/src/lib.rs` ~line 5435: `psyche.get_by_label(species).is_none(), "{species} is a beast, not a people"` → assert the real predicate: the species' `social_form != SocialForm::Settled`, message `"{species} is wild (not Settled)"`.
  - `windows/vessel/src/liveness.rs` ~line 3505: `psyche.get_by_label(&n.species).is_none(), "…absent from the psyche registry"` → assert `social_form != Settled`. Keep the following "defaulted psyche" assertion but scope its comment: it holds for these seed-42 wild kinds because they carry no psyche; it is *not* a claim that every wild creature lacks one (a placed dragon would carry an authored mind).

- [ ] **Step 3: Add the equivalence test** — new test in `windows/worldgen/src/lib.rs` (or the worldgen tests dir): assemble the canonical `WorldComponents` and assert the `Settled` set equals the psyche key-set, and the `{Solitary,Gregarious}` set equals `{non-people, non-autotroph}` — pinning that the re-key is a no-op on the shipped roster.

- [ ] **Step 4: Run the affected suites:**
Run: `cargo test -p hornvale-worldgen && cargo test -p hornvale-vessel`
Expected: PASS. The re-keyed gates select identical sets; the wild tests assert the corrected invariant.

- [ ] **Step 5: fmt + commit** (`refactor(worldgen,vessel): re-key peoplehood gates onto SocialForm::Settled (the-eremite)` + trailer).

---

### Task 4: The solitary mind — dragon psyche

**Files:**
- Modify: `domains/species/src/lib.rs` (`psyche_registry` — three dragon rows)
- Modify: `domains/species/tests/social_form.rs` (or a new test) — assert the dragon mind

**Interfaces:**
- Consumes: the nested invariant (Task 2 — psyche-without-speech must be legal) and the re-keyed settlement roster (Task 3 — a psyche-carrying dragon must NOT settle). **Both must be committed first.**

- [ ] **Step 1: Write the failing test** — assert each dragon carries the §3.4 vector:

```rust
#[test]
fn the_dragons_have_a_solitary_mind() {
    use hornvale_species::{Sociality, StatusBasis, psyche_registry};
    let psy = psyche_registry();
    for name in ["white-dragon", "red-dragon", "black-dragon"] {
        let m = psy.get(&hornvale_kernel::KindId(name)).expect("dragon mind");
        assert_eq!(m.threat_response, 0.95, "{name} stands, never flees");
        assert_eq!(m.in_group_radius, 0.05, "{name} is utterly insular");
        assert_eq!(m.time_horizon, 0.90, "{name} is a patient hoarder");
        assert_eq!(m.sociality, Sociality::Hierarchic);
        assert_eq!(m.status_basis, StatusBasis::Rank);
    }
}
```

- [ ] **Step 2: Run — expect failure** (no dragon in `psyche_registry`):
Run: `cargo test -p hornvale-species --test social_form the_dragons_have_a_solitary_mind`
Expected: FAIL (`expect("dragon mind")` panics).

- [ ] **Step 3: Add three dragon rows to `psyche_registry`** — one shared chromatic temperament each (per §3.4):

```rust
        (
            KindId("white-dragon"),
            PsychVector {
                threat_response: 0.95,      // an apex — stands, never flees
                deliberation_latency: 0.5,  // banked dial, baseline
                in_group_radius: 0.05,      // "us" = self; utterly solitary
                time_horizon: 0.90,         // a centuries-long hoarder
                sociality: Sociality::Hierarchic, // relates by dominance
                status_basis: StatusBasis::Rank,  // esteems power / the hoard
            },
        ),
        // …identical vectors for KindId("red-dragon") and KindId("black-dragon").
```

(All three chromatics share the profile this campaign; per-chromatic differentiation is a deferred refinement, spec §8.)

- [ ] **Step 4: Run species suite + confirm the dragon does NOT settle** — the invariant must accept psyche-without-speech (Task 2), and the settlement roster must exclude the Solitary dragon (Task 3):
Run: `cargo test -p hornvale-species && cargo test -p hornvale-worldgen`
Expected: PASS — dragon mind test green; `check_integrity` accepts the three psyche-only dragons; no settlement/possession test regresses (dragons are Solitary and unplaced).

- [ ] **Step 5: fmt + commit** (`feat(species): the dragons gain a solitary mind (the-eremite)` + trailer).

---

### Task 5: Book model card, final gate, and byte-identity verification

**Files:**
- Modify: `book/src/domains/species.md`

- [ ] **Step 1: Update the species model card** — add a line to the biosphere/model-card prose: social organization (`SocialForm`) is an authored universal dimension orthogonal to the mind/perception/speech capacities; the peopled cluster is no longer all-or-none but a nested lattice (speech ⊆ perception ⊆ mind; `Settled` ⟹ full cluster), so a solitary creature (a dragon) can carry a mind without settling or speaking. Build: `mdbook build book`.

- [ ] **Step 2: Full gate:**
Run: `make gate` (timeout 3600000)
Expected: PASS — fmt, clippy (`-D warnings`), nextest, doctests.

- [ ] **Step 3: Byte-identity verification (the determinism gate)** — regenerate every artifact and diff, including the vessel/possession goldens:
Run: the artifact-regen command list from `.github/workflows/ci.yml` ("Artifacts are current" step) — at minimum `cargo run -p hornvale -- new --seed 42 …` + the almanac/registry/lab/possession regens — then:
`git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/`
Expected: **clean (exit 0)**. If the possession goldens or any artifact moved, STOP — the set-equality (`{Settled} == {psyche}`) or dragon-latency assumption was violated; investigate before proceeding (do not re-pin a drift here — this campaign must be byte-identical).

- [ ] **Step 4: Commit** (`docs(book): species model card — SocialForm & the nested capacity lattice (the-eremite)` + trailer).

---

## Definition of Done (close — via `closing-a-campaign`)

- **Chronicle** `book/src/chronicle/the-eremite.md` + SUMMARY wiring.
- **Retrospective** `docs/retrospectives/the-eremite.md`.
- **Decision** (spec §7) ratified with an ID computed at close.
- **Registry:** flip **UNI-31** → `spec'd`/`shipped`, point Where at this spec/chronicle; add the captured followups (PsychVector split → `SocioVector`/`SocialPsychVector`, Nathan-endorsed; per-chromatic differentiation; minds for xorn/otyugh; derive-social-form-from-ecology).
- **Confidence Gradient:** check `open-questions.md` (the peoples/species bets) — likely no bet moved; confirm.
- **Push main** after the fast-forward.

## Self-Review notes

- **Spec coverage:** §3.1 SocialForm+authoring → Task 1; §3.2 nested invariant → Task 2; §3.3 re-key seams (all 6+ sites) → Task 3; §3.4 dragon mind → Task 4; §5 model card → Task 5; §4 determinism/verification → Task 5 Step 3 + Task 1's `settled_set_equals_the_psyche_key_set`. §8 non-goals → DoD captures.
- **Ordering:** 1→2→3→4 enforced (Task 4 consumes Tasks 2+3). Stated in Global Constraints and each task's Interfaces.
- **Placeholder scan:** none — exact enum/field/values/psyche/tests; re-keys given as exact old→new at named line ranges.
- **Type consistency:** `SocialForm`, `social_form`, `is_settled`, `biosphere.get_by_label` used identically across tasks.
