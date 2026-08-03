# The Vernacular, part 3b — text stops being stored

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Delete `Phenomenon.description`, render from the referent at the point
of reading in the speaker's own words, and invert the direction of authority so
the ledger's content stops being downstream of a rendering decision.

**Architecture:** A phenomenon producer cannot know who is looking —
`ObserverContext` is `{place, time, lens, position}` by constitutional design —
so a stored `description` can never be per-culture. The field is **deleted**.
The two production readers render from the referent instead, with a **speaker**
supplied by the composition root (the pattern `AlmanacContext::place_labels`
already establishes). Separately, `Star.class_name` stops being the authority the
ledger string-matches against: the producer derives the concept from the physics
it already holds, and the display becomes purely derived.

**Tech Stack:** Rust edition 2024, `serde` only (decision 0004). No new
dependencies. `make gate` as the commit gate.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only.
- **No wall-clock time.**
- Every public item documented; every pub-boundary primitive carries a
  `type-audit:` verdict tag.
- **Zero `TODO` comments in the workspace. Do not add one.**
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on
  the kernel and nothing else; a window may depend on domains and on other
  windows; **a window may not reach back to the composition root** — the root
  fills window-facing structs.
- `cargo fmt` last. Commit every drifted committed artifact in the same commit.
- **Three repo Bash guards:** the raw whole-workspace nextest invocation is
  blocked (use `make gate`); bare `git stash` / `git stash pop` are blocked; two
  test runs in one Bash call are blocked — capture once and grep.
- **`git worktree remove` silently resets the shell's cwd to the main
  checkout.** If you make a throwaway worktree, use `git -C <path>` afterwards
  and echo `pwd` in any command whose output you report.

## What moves and what must not

**Zero committed facts may move.** Every task here is rendering; the ledger's
content is untouched except by Task 4, which changes *how* a concept is chosen
without changing *which* concept results. Baseline:
`.superpowers/sdd/2026-08-02-the-vernacular-part-3a-star-class/baseline-seed-42-post-contour.json`
(11434 facts, village `Godogododaga`). Compare **fact lists**, not whole files.

**Rendered artifacts WILL move, substantially, and that is correct.** This is the
first plan in the campaign where the world says something different. Expect
`book/src/gallery/almanac-seed-42*.md` and the possession transcripts to change.

## Scope

Implements §2, §3 and §5's plan 3b of
`docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`, including
the authority inversion recorded there on 2026-08-03.

**NOT in scope:** the colour path (3c — `daylight_words`, `twilight_words`,
`class_color` collapsing into `name_color`), `SkyReport`/`ClimateReport`, branch
C's frame abstraction, and the bidirectional lint (stage 4).

## What was verified before writing, so you need not re-check

- **Only two production readers** of `Phenomenon.description`:
  `windows/almanac/src/lib.rs:314` and `cli/src/repl.rs:326`. Both render the
  same salience-ranked list. Everything else is test assertions
  (`domains/astronomy/tests/tier_refinement.rs:93`,
  `domains/astronomy/tests/genesis_properties.rs:421`,
  `domains/astronomy/src/provider.rs:175,1010,1090,1099,1100`,
  `cli/tests/prose_is_not_a_contract.rs:106`).
- **23 producer sites** across `domains/astronomy/src` and `domains/climate/src`.
- The composition root **already has species and lexicons in scope** where it
  builds `AlmanacContext` (`windows/worldgen/src/lib.rs:7331`; the peopled-kind
  loop above it carries `"a peopled kind with a flagship has a lexicon"`), so
  threading a speaker needs no restructuring.

## The rendered line, decided

Today's committed gallery line carries **both** signs of the leak:

```
- [0.70] *celestial-body* — a golden sun fixed at zenith
```

`a golden sun fixed at zenith` is stored prose. `*celestial-body*` is a raw
registry key in reader-facing prose — the second sign §3.1 names, shipping
unnoticed because everyone was looking at the prose beside it.

**Nathan's decision (2026-08-03): drop the kind.** The line becomes salience
plus the rendered referent, in the speaker's own words:

```
- [0.70] the sun
```

The kind is machine grouping; a reader gets nothing from `celestial-body` that
`the sun` does not already tell them. Both signs go at once.

---

### Task 1: A speaker reaches the almanac

**Files:**
- Modify: `windows/almanac/src/lib.rs` (the `AlmanacContext` struct, `:125`)
- Modify: `windows/worldgen/src/lib.rs:7331` (where the root builds it)
- Test: `windows/almanac/src/lib.rs` (inline `mod tests`)

**Interfaces:**
- Produces: `AlmanacContext::speaker: Option<Speaker>`, where `Speaker` lives in
  `windows/almanac/src/lib.rs` beside the context and carries **all five**
  things `realize_tongue_deep` needs: `species`, `lexicon`, `grammar`, `morph`,
  `sky_animate` (see step 3 for the exact shape and why).
- Consumes: nothing from earlier tasks. **No rendering changes yet** — this task
  only makes a speaker *available*, so it moves no bytes.

`Option` because a placeless world has no peoples; the spec notes that case
largely dissolves (a placeless world returns empty phenomena) but the type must
still admit it.

- [ ] **Step 1: Write the failing test**

Add to `windows/almanac/src/lib.rs`'s inline `mod tests`:

```rust
/// The almanac is rendered for a particular people, so its context carries
/// one. The composition root fills it — a window may not reach back to the
/// root, the same reason `place_labels` is filled rather than derived.
#[test]
fn a_context_can_carry_a_speaker() {
    let ctx = sample_context();
    assert!(
        ctx.speaker.is_none(),
        "the hand-built sample context has no people; that must stay legal"
    );
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-almanac --lib a_context_can_carry_a_speaker`
Expected: FAIL — `no field 'speaker' on type 'AlmanacContext'`.

- [ ] **Step 3: Add the field and the type**

In `windows/almanac/src/lib.rs`, above `AlmanacContext`:

```rust
/// The people an almanac is rendered for. One world has as many almanacs as it
/// has peoples; a committed artifact is a projection that picks one, and §3 of
/// the campaign spec requires the document to name which.
///
/// The composition root fills this — a window may not reach back to the root,
/// which is why [`AlmanacContext::place_labels`] is filled rather than derived.
/// type-audit: bare-ok(identifier-text: species)
#[derive(Clone, Debug)]
pub struct Speaker {
    /// The species whose tongue voices this document.
    pub species: String,
    /// That species' vocabulary.
    pub lexicon: hornvale_language::Lexicon,
    /// Its clause-level grammar.
    pub grammar: hornvale_language::TongueGrammar,
    /// Its morphology.
    pub morph: hornvale_language::TongueMorphology,
    /// Whether this people's day-schema is agentive, which overrides the
    /// animacy of sky concepts. Stored as the bool rather than a closure so
    /// the struct stays plain data; a renderer rebuilds the classifier with
    /// `hornvale_worldgen::noun_class_with_sky(sky_animate, concept)`.
    pub sky_animate: bool,
}
```

**All five, not just the lexicon.** `realize_tongue_deep` — the production
path for saying anything in a species' own words — takes
`(clause, grammar, morph, noun_class_of, lexicon)`. A lexicon alone realizes
nothing.

and the field on `AlmanacContext`:

```rust
    /// The people this document is voiced by, or `None` for a world with no
    /// peoples. Filled by the composition root.
    pub speaker: Option<Speaker>,
```

Add `hornvale-language` to `windows/almanac/Cargo.toml` if it is not already a
dependency — check first with
`grep hornvale-language windows/almanac/Cargo.toml`. A window may depend on a
domain, so this is legal; if the architecture test objects, read its message
rather than working around it.

- [ ] **Step 4: Fill it at the composition root**

At `windows/worldgen/src/lib.rs:7331`, populate `speaker` from the **flagship**
people. **The working precedent is `windows/book/src/lib.rs:414-432`** — read
it; it assembles exactly these five per peopled kind:

```rust
let ph = hornvale_worldgen::language_of(world, kind);
let grammar = tongue_grammar(&world.seed, kind, &ph);
let lexicon = hornvale_worldgen::lexicon_from(world, kind, terrain, climate)?;
let morph = hornvale_worldgen::tongue_morphology_of(world, kind)?;
let sky_animate = hornvale_worldgen::day_schema_from(world, kind, terrain, climate)
    == Some(SchemaId::Agentive);
```

Reuse whatever the surrounding peopled-kind loop already built rather than
recomputing — `lexicon_from`'s own doc calls it *"almost all of the
post-name-gloss census cost"*, so **build it once per world, never per
phenomenon**. If the flagship's species is not among the peopled kinds, or any
of the five cannot be assembled, pass `None` for the whole speaker rather than
picking arbitrarily or assembling a partial one.

Every other `AlmanacContext { .. }` construction site (there is at least
`windows/almanac/src/lib.rs:557`'s `sample_context`) gets `speaker: None`.

- [ ] **Step 5: Run the tests and the gate**

Run: `cargo test -p hornvale-almanac --lib` — capture to a file and grep it.
Then `make gate 2>&1 | tee /tmp/hv-3b-t1.log`.
Expected: PASS. **Nothing renders differently yet**, so:

- [ ] **Step 6: Prove nothing moved**

Compare fact lists against `$BASELINE`, and run
`make rebaseline; git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`.
Expected: **zero facts moved and NO artifact drift** — this task adds an unread
field. A gallery diff here means something is reading the speaker already.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(almanac): the context carries the people it is voiced by

One world has as many almanacs as it has peoples, and a committed artifact is a
projection that picks one. The composition root fills the speaker from the
flagship people, reusing the lexicon the peopled-kind loop already built —
lexicon_from is almost all of the post-name-gloss census cost, so it is built
once per world.

Nothing reads it yet: no facts moved, no artifact drift."
```

---

### Task 2: Render a phenomenon from its referent

**Files:**
- Create: `windows/almanac/src/phenomenon_line.rs`
- Modify: `windows/almanac/src/lib.rs` (declare the module; use it at `:314`)
- Test: `windows/almanac/src/phenomenon_line.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: `Speaker` from Task 1; `hornvale_kernel::{Phenomenon, Referent}`;
  `hornvale_language::{TongueClause, Evidential, realize_tongue_deep, TongueGap}`
  for the speaker path and `realize_common` for the neutral path.
- Produces:
  `pub(crate) fn phenomenon_line(p: &Phenomenon, speaker: Option<&Speaker>) -> String`
  — the reader-facing text for one phenomenon, with **no** salience and **no**
  kind (the caller adds the salience; the kind is dropped per the decision
  above).

A new file rather than more of `lib.rs`: that file is already large, and this is
one clear responsibility — turning a referent into a sentence fragment.

- [ ] **Step 1: Write the failing test**

Create `windows/almanac/src/phenomenon_line.rs` with:

```rust
//! One phenomenon, as a reader sees it: rendered from its referent at the
//! moment of reading, never stored. A producer cannot know who is looking
//! (`ObserverContext` carries no species), so a stored string could only ever
//! be neutral or wrong — which is why `Phenomenon` no longer has one.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Phenomenon, Referent, Venue};

    fn moon() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            referent: Referent::qualified("moon", &["great"]),
            period_days: Some(27.3),
            salience: 0.7,
            venue: Venue::NightSky,
        }
    }

    /// With no speaker, the line is the culture-neutral Common realization —
    /// what an out-of-world reader gets when the world has no peoples.
    #[test]
    fn a_referent_renders_without_a_speaker() {
        let line = phenomenon_line(&moon(), None);
        assert!(
            line.contains("moon"),
            "the neutral rendering must name the concept: {line}"
        );
        assert!(
            !line.contains("celestial-body"),
            "a registry key must never reach prose: {line}"
        );
    }

    /// A qualifier reaches the rendering — `great` is a registered concept and
    /// the line must be able to say it.
    #[test]
    fn a_qualifier_reaches_the_line() {
        let plain = phenomenon_line(
            &Phenomenon { referent: Referent::of("moon"), ..moon() },
            None,
        );
        let great = phenomenon_line(&moon(), None);
        assert_ne!(
            plain, great,
            "a qualified referent must render differently from a bare one"
        );
    }
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-almanac --lib phenomenon_line`
Expected: FAIL to compile — `phenomenon_line` does not exist, and `Phenomenon`
still has a `description` field so the struct literals are incomplete. **That
second failure is expected**: Task 3 removes the field. For this task,
temporarily include `description: String::new()` in the fixtures and delete it
in Task 3 — note that in your report so the reviewer knows it is deliberate.

- [ ] **Step 3: Write the renderer**

**Two registers, and they are structurally different — this is the heart of
the task.** Read `windows/book/src/lib.rs:434-450` first; it is the working
precedent.

- **With a speaker: the tongue path.** `realize_tongue_deep(&clause, &grammar,
  &morph, &noun_class_of, &lexicon) -> Result<String, TongueGap>` does the
  concept→word lookup itself, handling `LexEntry::{Root, Compound, Gap}`. You
  build a `TongueClause` whose `complement_concept` is
  `p.referent.concept`, and rebuild the classifier as
  `|c: &str| hornvale_worldgen::noun_class_with_sky(speaker.sky_animate, c)`.
- **With no speaker: the neutral path.** `realize_common(&ClauseSpec)` takes
  `Subject::Name(String)` — a **literal string, not a concept id**. Common
  does not go through a lexicon at all. So the neutral rendering needs a word
  from somewhere else: use the concept's registered registry entry (its
  `doc`/gloss). **Never the raw key.**

**On a `TongueGap`, circumlocute — do not go silent and do not fall back to
the key.** §3.1 is explicit: an absent word means the thing gets *described*,
not refused. `TongueGap` carries `.concept` and `.reason`; the book's
`probe_tongue` call site shows a gap being recorded rather than papered over.
For the almanac, prefer the neutral rendering of that one concept over
dropping the line — a reader should still learn the phenomenon is there.

**Qualifiers.** `Referent.qualifiers` are themselves registered concepts, so
each renders the same way as the head. If `TongueClause` has no slot for
modifiers, render the head through the tongue and say in your report what
happened to the qualifiers rather than silently dropping them — a dropped
qualifier is a content loss, which is exactly what this campaign is trying to
stop.

**Signature and structure are yours to choose within that contract.** The
requirement is that the *words come from the speaker's lexicon* and that no
registry key reaches prose — not that any particular helper exists. Report the
shape you landed on.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-almanac --lib phenomenon_line`
Expected: PASS, 2 tests.

- [ ] **Step 5: Wire it in, and drop the kind**

At `windows/almanac/src/lib.rs:314`, replace the line's construction:

```rust
            doc.push_str(&format!(
                "- [{:.2}] {}\n",
                p.salience,
                phenomenon_line(p, ctx.speaker.as_ref())
            ));
```

- [ ] **Step 6: Gate, then accept the artifact movement**

Run `make gate 2>&1 | tee /tmp/hv-3b-t2.log`. Expect the almanac's own render
tests to red on the changed line — read each message; they are asserting the old
shape and should be updated to assert the new one, **not** deleted.

Then `make rebaseline` and **read the gallery diff before committing it**.
Quote the before/after of `book/src/gallery/almanac-seed-42.md`'s phenomena
block in your report. This is the first time the world says something
different; the diff is the deliverable, not a nuisance.

**Facts must not move.** Compare fact lists against `$BASELINE`.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(almanac): a phenomenon is rendered from its referent

The line was '- [0.70] *celestial-body* - a golden sun fixed at zenith', which
carried both signs of the leak at once: stored prose, and a raw registry key in
reader-facing text. It is now salience plus the referent rendered through the
speaker's lexicon, with the kind dropped — machine grouping a reader gains
nothing from.

Gallery artifacts move. Facts do not."
```

---

### Task 3: Delete `Phenomenon.description`

**Files:**
- Modify: `kernel/src/phenomena.rs` (the `Phenomenon` struct)
- Modify: all 23 producer sites in `domains/astronomy/src`, `domains/climate/src`
- Modify: `cli/src/repl.rs:326`
- Modify: the test assertions listed under "What was verified before writing"
- Modify: `cli/tests/prose_is_not_a_contract.rs`

**Interfaces:**
- Consumes: `phenomenon_line` from Task 2.
- Produces: `Phenomenon { kind, referent, period_days, salience, venue }` — no
  `description`.

**This is the task the campaign has been walking toward.** Text stops existing
inside the sim.

- [ ] **Step 1: Write the failing test**

Add to `kernel/src/phenomena.rs`'s inline `mod tests`:

```rust
/// A phenomenon carries no text. A producer cannot know who is looking —
/// `ObserverContext` is {place, time, lens, position} by constitutional design
/// (decision 0003) — so a stored string could only ever be neutral or wrong.
/// Rendering happens where the speaker is known.
///
/// This test is a structural assertion: it fails to COMPILE if the field
/// returns, which is the point.
#[test]
fn a_phenomenon_carries_no_text() {
    let p = Phenomenon {
        kind: "celestial-body".to_string(),
        referent: Referent::of("moon"),
        period_days: None,
        salience: 1.0,
        venue: Venue::NightSky,
    };
    assert_eq!(p.referent.concept, "moon");
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-kernel --lib a_phenomenon_carries_no_text`
Expected: FAIL to compile — `missing field 'description'`.

- [ ] **Step 3: Delete the field, then follow the compiler**

Remove `pub description: String` from `Phenomenon` and update its
`type-audit:` tag (drop `bare-ok(prose: description)`).

Then let `cargo check --workspace --all-targets` drive you through the 23
producers and the readers. For each:

- **Producers** — delete the `description:` line. Nothing else.
- **`cli/src/repl.rs:326`** — the REPL has no speaker; render with `None`. It
  needs `phenomenon_line`, which is `pub(crate)` in `hornvale-almanac` — either
  promote it to `pub` with a doc comment and a `type-audit:` tag, or give the
  REPL its own neutral rendering. **Prefer promoting**: two renderings of one
  thing is what this campaign exists to remove. Say which you chose.
- **Test assertions** — the seven listed sites assert on prose. Convert each to
  assert on the **referent**, which is what they were always trying to check:
  `provider.rs:175` compares against `neighbor.night_description()`;
  `:1010` checks `contains("wander")`; `:1090` finds by `inner_class_word`;
  `:1099-1100` check `"morning star"`/`"evening star"`;
  `genesis_properties.rs:421` filters `contains("moon")`;
  `tier_refinement.rs:93` prints it.

  **`provider.rs:1090` and `:1099` cannot be converted as-is** — every wanderer
  carries an identical `Referent::qualified("star", &["move"])`, so the referent
  cannot tell inner from outer, or morning from evening (this is the campaign's
  own recorded followup). For those two: assert what the referent *can* support
  and say in a plain doc comment what the test no longer distinguishes. **Do not
  add a `TODO`.** Record the residue in
  `.superpowers/sdd/.../followups.md`.
- **`cli/tests/prose_is_not_a_contract.rs`** — its whole premise was that
  rewording a description moves nothing. With no description to reword, the
  battery's headline test is obsolete. **Do not delete the file**: keep
  `every_referent_key_is_registered` and `a_referent_never_carries_prose`, and
  replace the reword test with a note in the module doc that the defect it
  guarded is now structurally impossible — the field is gone. That is the
  strongest form of the guarantee, and the file should say so.

- [ ] **Step 4: Run the gate**

Run `make gate 2>&1 | tee /tmp/hv-3b-t3.log`.
Expected: PASS. Read every failure before changing anything.

- [ ] **Step 5: Measure**

Facts against `$BASELINE`: **zero moved.** Then `make rebaseline` and read the
diff — the REPL is not an artifact, so expect movement only where the almanac
already moved in Task 2, plus `docs/audits/type-audit-report.md` (the struct
lost a tagged primitive).

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(kernel): a phenomenon carries no text

Phenomenon.description is deleted, not relocated. A producer cannot know who is
looking — ObserverContext is {place, time, lens, position} by constitutional
design — so a stored string could only ever be neutral or wrong. That is why
every leak in this campaign's audit clustered on this one field: its TYPE
guaranteed it.

Text now comes into being where the speaker is known. The reword-invariance
battery's headline test is retired because the defect it guarded is
structurally impossible: there is nothing left to reword."
```

---

### Task 4: Invert the direction of authority

**Files:**
- Modify: `domains/astronomy/src/star.rs` (add the mass → concept derivation)
- Modify: `domains/astronomy/src/neighborhood.rs` (add the variant → concept map)
- Modify: `domains/astronomy/src/facts.rs:274,508` (commit the derived concept)
- Test: both domain files' inline `mod tests`

**Interfaces:**
- Consumes: `SPECTRAL_CLASSES`, `class_display` from part 3a.
- Produces: `pub fn class_concept_of_mass(mass: f64) -> &'static str` in
  `star.rs`, and `pub fn class_concept(class: NeighborClass) -> &'static str` in
  `neighborhood.rs`. Both **total** — no `Option`, no `.expect()` at the call
  site.

**Why this task exists.** Part 3a left display prose as the authority:
`facts.rs` string-matches `class_concept(&system.star.class_name)` to decide what
to commit, guarded by `.expect()`. The ledger's content is downstream of a
rendering decision — the inverse of this campaign's thesis. **Nathan's call,
2026-08-03: invert it.**

It also dissolves part 3a's Critical structurally. That finding existed because
`SPECTRAL_CLASSES` was a lookup the ledger depended on, hand-kept in sync with
the registration list and guarded only by a test. Once the id is derived
directly, that table becomes purely a render table.

- [ ] **Step 1: Write the failing tests**

In `domains/astronomy/src/star.rs`'s `mod tests`:

```rust
/// The concept is derived from mass, not parsed from prose. Same boundaries as
/// `class_name_of_mass`, and the two must agree — the display is now derived
/// from the concept's side of the same physics, not the other way round.
#[test]
fn the_concept_and_the_display_are_derived_from_the_same_mass() {
    for mass in [0.6, 0.79, 0.8, 1.04, 1.05, 1.4] {
        let concept = class_concept_of_mass(mass);
        let display = class_name_of_mass(mass);
        assert_eq!(
            class_display(concept),
            Some(display),
            "mass {mass} derives concept {concept:?} and display {display:?}, which disagree"
        );
    }
}
```

In `domains/astronomy/src/neighborhood.rs`'s `mod tests`:

```rust
/// Every variant maps to a concept totally — no lookup, no Option, so no
/// call site needs an `.expect()`.
#[test]
fn every_variant_derives_a_concept_agreeing_with_its_display() {
    for class in [
        NeighborClass::RedDwarf,
        NeighborClass::SunLike,
        NeighborClass::WhiteDwarf,
        NeighborClass::OrangeGiant,
        NeighborClass::RedGiant,
        NeighborClass::BlueGiant,
    ] {
        assert_eq!(
            crate::star::class_display(class_concept(class)),
            Some(class_name(class)),
            "{} derives a concept whose display disagrees with class_name",
            class_name(class)
        );
    }
}
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cargo test -p hornvale-astronomy --lib derived` — capture and grep.
Expected: FAIL — `class_concept_of_mass` / `class_concept` do not exist.

- [ ] **Step 3: Write the derivations**

`star.rs` — same boundaries as `class_name_of_mass`, returning the id:

```rust
/// The registered concept for a star of this mass — the ledger's own value,
/// derived from the physics rather than parsed back out of a display string.
/// Boundaries are `class_name_of_mass`'s; the two are two views of one
/// decision, and `the_concept_and_the_display_are_derived_from_the_same_mass`
/// pins them together.
/// type-audit: bare-ok(ratio: mass), bare-ok(identifier-text: return)
pub fn class_concept_of_mass(mass: f64) -> &'static str {
    if mass < 0.8 {
        "orange-dwarf"
    } else if mass < 1.05 {
        "yellow-dwarf"
    } else {
        "yellow-white-dwarf"
    }
}
```

`neighborhood.rs` — a total match, mirroring `class_name`:

```rust
/// The registered concept for a neighbour of this class. Total by
/// construction: a new variant fails to compile here, so no call site needs a
/// fallible lookup.
/// type-audit: bare-ok(identifier-text: return)
pub fn class_concept(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red-dwarf",
        NeighborClass::SunLike => "sun-like-star",
        NeighborClass::WhiteDwarf => "white-dwarf",
        NeighborClass::OrangeGiant => "orange-giant",
        NeighborClass::RedGiant => "red-giant",
        NeighborClass::BlueGiant => "blue-giant",
    }
}
```

- [ ] **Step 4: Flip the commit sites and delete the `.expect()`s**

`facts.rs:274` becomes
`Value::Text(crate::star::class_concept_of_mass(system.star.mass.0).to_string())`,
and `:508` becomes
`Value::Text(crate::neighborhood::class_concept(neighbor.class).to_string())`.
**Both `.expect()`s go.**

- [ ] **Step 5: Run the tests and the gate**

`cargo test -p hornvale-astronomy --lib` then
`make gate 2>&1 | tee /tmp/hv-3b-t4.log`.
Expected: PASS.

- [ ] **Step 6: Prove the values are identical**

**Facts must not move.** The ledger must commit exactly the same nine ids it
committed before — this task changes *how* the concept is chosen, never *which*.
Compare fact lists against `$BASELINE`: **zero differences.** If any moved, a
boundary or a variant mapping disagrees with part 3a's table, and that is a
finding.

Then confirm the panic is gone: `grep -n "expect" domains/astronomy/src/facts.rs`
should no longer show the two class lookups.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor(astronomy): the id is primary, the display derived

Part 3a left display prose as the authority: facts.rs string-matched
class_concept(&star.class_name) to decide what to commit, guarded by .expect().
The ledger's content was downstream of a rendering decision — the inverse of
this campaign's thesis.

The producer now derives the concept from the physics it already holds: mass for
the star, the NeighborClass variant for a neighbour. Both derivations are TOTAL,
so both .expect()s are gone.

This also dissolves part 3a's Critical structurally rather than guarding it:
SPECTRAL_CLASSES stops being a lookup the ledger depends on and becomes purely a
render table.

Same nine ids committed. Zero facts moved."
```

---

### Task 5: The cost measurement, and the named speaker

**Files:**
- Modify: `windows/almanac/src/lib.rs` (the document header)
- Modify: `docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`
  (§6's prediction gets its readout)

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: the recorded readout 3c builds on.

- [ ] **Step 1: Name the speaker in the document**

§3 of the spec requires it: *"One world now has as many renderings as it has
peoples, and the committed artifact is a projection that picks one. A projection
whose choice is invisible reads as neutral fact."*

Add a line to the almanac's header naming whose account it is — for example
`*As reckoned among the <name>.*` — using the speaker's people. With
`speaker: None`, say nothing rather than inventing a neutral claim.

- [ ] **Step 2: Measure the cost**

§4.2's frozen prediction: **per-world speaker construction keeps `make
rebaseline` within 1.25× its current wall time.** `docs/timings.md` carries the
`rebaseline` rows — take the most recent pre-3b row as the baseline and compare
against a run now. Report both numbers and the ratio.

**If it exceeds 1.25×**, the spec says the "build the speaker once per world"
hoist is mandatory rather than optional — check Task 1 actually did that
(reused the peopled-kind loop's lexicon rather than calling `lexicon_from`
again) and report what you find.

- [ ] **Step 3: Write the readout into §6**

Add a subsection giving: the cost ratio measured, whether facts moved (zero),
which artifacts moved and how the phenomena block reads now, and the
before/after of one gallery line. **Leave §6's frozen predictions intact** —
append alongside, never over. Decision 0016's preregistration is worthless if
the frozen numbers can be edited after unblinding.

- [ ] **Step 4: Gate and commit**

```bash
make gate 2>&1 | tee /tmp/hv-3b-t5.log
cargo fmt
git add -A
git commit -m "docs(the-vernacular-3): 3b readout — the world says it in its own words"
```

---

## Self-review

**Spec coverage.** §1 (a stored description can never be per-culture) → Task 3's
test doc. §2 (delete, render downstream, speaker from the root) → Tasks 1–3. §2's
colour channel → **not here**, 3c. §2.1 (the one-way machine) → Task 3 removes
the last stored text on the phenomenon path. §3 (the almanac names its speaker)
→ Task 5 step 1. §4.1 (the flagship coupling) → made legible by Task 5 step 1's
header. §4.2 (the cost risk) → Task 5 step 2, measured against the frozen 1.25×.
§5's 3b plus the 2026-08-03 authority inversion → Task 4. §6's prediction → Task
5 step 3.

**Type consistency.** `Speaker { species: String, lexicon: Lexicon }` and
`AlmanacContext::speaker: Option<Speaker>` are used with those exact shapes in
Tasks 1, 2 and 5. `phenomenon_line(&Phenomenon, Option<&Speaker>) -> String` is
consistent in Tasks 2 and 3. `class_concept_of_mass(f64) -> &'static str` and
`class_concept(NeighborClass) -> &'static str` are total in Task 4 and used with
those signatures at both commit sites.

**Three things stated as instructions to check rather than assertions**, because
this campaign's briefs have twice carried a wrong claim into an implementer's
hands:

1. **Task 2's renderer is specified by contract, not by code.** Its body
   depends on whether `TongueClause` has a modifier slot, which I have not
   confirmed. Task 2 states the contract — words come from the speaker's
   lexicon, no key reaches prose, a gap circumlocutes — and names the working
   precedent (`windows/book/src/lib.rs:434-450`) rather than inventing one.
2. **Corrected after dispatching Task 1** (recorded here because the plan's
   first draft was wrong and the diff should show why): `Speaker` originally
   carried only `{species, lexicon}`. `realize_tongue_deep` needs
   `(clause, grammar, morph, noun_class_of, lexicon)` — a lexicon alone
   realizes nothing — so the struct carries all five. Related: `realize_common`
   takes a **literal string**, not a concept id, so the neutral path cannot go
   through a lexicon and needs the registry gloss instead. Both facts came from
   reading `domains/language/src/grammar.rs` and `clause.rs` directly.
3. **`provider.rs:1090` and `:1099` may be unconvertible.** Every wanderer
   carries an identical referent, so the referent cannot distinguish inner from
   outer or morning from evening. Task 3 says to assert what it can support and
   record the residue, not to fake it.
