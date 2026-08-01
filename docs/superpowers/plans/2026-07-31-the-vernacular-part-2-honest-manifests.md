# The Vernacular, part 2 — honest manifests

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Register the nine spectral-class concepts with
`lexeme: Absent(Void::Unnamed(...))` — the first honest use of a kernel
vocabulary that has existed since `manifest.rs` was written and has never held
anything — and enforce the claim so it is a constraint rather than a comment.

**Architecture:** `domains/astronomy`'s `register_concepts` gains nine
`Manifest`s whose lexeme edge is `Absent(Void::Unnamed)`: three for the star's
own class and six for the neighbour classes. The `star-class` fact still
carries prose, and changing that is part 3, which moves committed facts and
needs an epoch measurement.

**Corrected after Task 1 shipped.** An earlier draft claimed the declaration
was merely *inert* — that nothing would read it. That was wrong, and wrong in
the campaign's own characteristic way: the exposures map is keyed on
`world.registry.concepts()` (`windows/worldgen/src/lib.rs:3986`), so every
registered concept enters the language's universe and `build_lexicon` reserves
a proto-root for all of it, `Steeped`/`KnowsOf`/`Unknown` alike. Registering a
concept as unnameable therefore *minted a proto-goblinoid word for "yellow
dwarf"* and recorded every daughter as having forgotten it. So Task 2 exists:
`GapReason` gains a third provenance and the unnameable stop reserving roots.
Task 1 moves no facts; **Task 2 is expected to move them**, and measures rather
than assumes.

**Tech Stack:** Rust edition 2024, `serde` only (decision 0004). No new
dependencies. `make gate` as the commit gate.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only.
- **No wall-clock time.**
- Every public item documented; every pub-boundary primitive carries a
  `type-audit:` verdict tag.
- **The workspace contains zero `TODO` comments. Do not add one.**
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain depends on
  `hornvale-kernel` and nothing else — so astronomy may not import the language
  crate; the enforcement test in Task 2 lives where both are visible.
- **This plan must move zero committed facts.** **The gate is "no fact moved",
  NOT "the world JSON is identical"** — an earlier draft said the latter and it
  is wrong: `World` serializes the concept registry, so registering a concept
  necessarily changes the world file while moving no fact. Compare fact lists
  directly against `$BASELINE` =
  `.superpowers/sdd/2026-07-31-the-vernacular-part-1-the-referent-contract/baseline-seed-42.json`:

  ```python
  import json
  def facts(p):
      w = json.load(open(p))
      def walk(o):
          if isinstance(o, dict):
              yield o
              for v in o.values(): yield from walk(v)
          elif isinstance(o, list):
              for v in o: yield from walk(v)
      return [json.dumps(d, sort_keys=True) for d in walk(w) if isinstance(d.get('predicate'), str)]
  a, b = facts(BASELINE), facts(NEW)
  assert a == b, "a fact moved"
  ```
- **Commit any drifted committed artifact in the same commit.** Registering
  concepts drifts `book/src/reference/concept-registry-generated.md` and
  `book/src/reference/concept-manifest-generated.md`; the type-audit report
  drifts on any pub-boundary change. Omitting these is named in `CLAUDE.md` as
  a common miss and part 1 hit it once.
- **Three repo Bash guards:** the raw whole-workspace nextest invocation is
  blocked (use `make gate`); bare `git stash` / `git stash pop` are blocked;
  two test runs in one Bash call are blocked — capture once and grep.

## Scope

This plan implements the **first half of stage 3 item 1** of
`docs/superpowers/specs/2026-07-31-the-vernacular-design.md` (see §3.1 for the
three-registers argument that motivates it). Sequenced by reversibility, per
that section: declaring manifests is free and reversible and lands here;
changing the `star-class` fact's *value* from prose to a concept id is neither,
moves committed facts, and lands in part 3 with its own epoch measurement.

---

### Task 1: Register the nine unnameable concepts

**Files:**
- Modify: `domains/astronomy/src/lib.rs` (the `register_concepts` function —
  the existing manifest loops are around `:355-420`)
- Test: `domains/astronomy/src/lib.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: `hornvale_kernel::{Manifest, ConceptDef, ConceptKind, Correspondent, Void}`,
  all already imported by this file.
- Produces: nine registered concepts —
  `orange-dwarf`, `yellow-dwarf`, `yellow-white-dwarf`, `red-dwarf`,
  `sun-like-star`, `white-dwarf`, `orange-giant`, `red-giant`, `blue-giant` —
  each with `lexeme: Correspondent::Absent(Void::Unnamed(...))`.

- [ ] **Step 1: Write the failing test**

Add to `domains/astronomy/src/lib.rs`'s inline `mod tests`:

```rust
/// The spectral classes are objectively real and nameless here: a star has a
/// class whether or not anyone has invented spectroscopy. `Void::Unnamed` is
/// the kernel's word for exactly that, and before this campaign no domain had
/// ever used it.
#[test]
fn spectral_classes_are_registered_as_unnameable() {
    let mut registry = hornvale_kernel::ConceptRegistry::default();
    register_concepts(&mut registry).expect("astronomy registers");

    for name in [
        "orange-dwarf",
        "yellow-dwarf",
        "yellow-white-dwarf",
        "red-dwarf",
        "sun-like-star",
        "white-dwarf",
        "orange-giant",
        "red-giant",
        "blue-giant",
    ] {
        let manifest = registry
            .manifest(name)
            .unwrap_or_else(|| panic!("{name} should be registered"));
        assert!(
            matches!(
                manifest.lexeme,
                hornvale_kernel::Correspondent::Absent(hornvale_kernel::Void::Unnamed(_))
            ),
            "{name}'s lexeme must be Absent(Unnamed) — it is real and no one here can name it, \
             which is not the same as Gap (a hole in OUR coverage)"
        );
    }
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-astronomy --lib spectral_classes_are_registered`
Expected: FAIL — `orange-dwarf should be registered`.

- [ ] **Step 3: Register them**

In `domains/astronomy/src/lib.rs`'s `register_concepts`, after the existing
`eclipse`/`tide` loop, add:

```rust
    // The spectral classes (§3.1 of the campaign spec). These are the first
    // concepts in the workspace to use `Void::Unnamed`, and the distinction it
    // draws is the point: a star HAS a class whether or not anyone has
    // invented spectroscopy, so the fact is objective and must be
    // representable — but no culture here has encountered the main sequence,
    // so no word realizes it. That is `Unnamed`, not `Gap`: `Gap` says WE have
    // not got to it, and these are not waiting on us.
    //
    // The keys below are machine identifiers, never words. `Unnamed` is
    // precisely the assertion that no word exists; a renderer meeting one must
    // circumlocute (the way `packs.rs`'s compound recipes give `sea` as "many
    // water"), never emit the key.
    for (name, doc) in [
        ("orange-dwarf", "a cooler, dimmer main-sequence star"),
        ("yellow-dwarf", "a main-sequence star of the sun's own kind"),
        (
            "yellow-white-dwarf",
            "a hotter, brighter main-sequence star",
        ),
        ("red-dwarf", "the commonest and faintest main-sequence star"),
        (
            "sun-like-star",
            "a distant star resembling this world's own sun",
        ),
        ("white-dwarf", "the dense cinder a spent star leaves"),
        ("orange-giant", "a cooling star swollen off the main sequence"),
        ("red-giant", "a cool, vast star late in its life"),
        ("blue-giant", "a hot, brilliant, short-lived star"),
    ] {
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "astronomy".to_string(),
                kind: ConceptKind::Celestial,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Unnamed(
                "no culture here has encountered the main sequence",
            )),
            percept: Correspondent::Absent(Void::Imperceptible(
                "a spectral class is inferred from a spectrum, never seen; \
                 what is seen is the star's colour",
            )),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
```

**Note on `sun-like-star`.** It is the one class arguably expressible in world
terms — "a star like our sun" is a comparison an observer could actually make,
since the sun is the world's own. It is registered `Unnamed` anyway because the
*spectral class* is the concept, not the resemblance. If a later campaign gives
cultures a resemblance concept, that is a different concept, not a relaxation
of this one.

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-astronomy --lib spectral_classes_are_registered`
Expected: PASS.

- [ ] **Step 5: Run the workspace gate**

Run: `make gate 2>&1 | tee /tmp/hv-vern2-t1.log`
Expected: PASS. Watch for `cli/tests/correspondence.rs` — it asserts every
registered concept has a manifest, that the three correspondence ledgers foot
(covered + void = total), and that every `Present(Expected)` lexeme is actually
lexicalizable. Our nine are `Absent`, so the last check does not bind them; the
trial balance should simply show nine more on the void side.

- [ ] **Step 6: Prove zero facts moved**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-vern2-t1-world.json
diff "$BASELINE" /tmp/hv-vern2-t1-world.json && echo "IDENTICAL"
```

Expected: `IDENTICAL`. Registering a concept nothing references cannot move a
fact; if it does, something derives world-state from the registry's shape and
that is a finding worth reporting before continuing.

- [ ] **Step 7: Refresh the drifted artifacts**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: `book/src/reference/concept-registry-generated.md` and
`book/src/reference/concept-manifest-generated.md` change; **nothing under
`book/src/gallery/`**. Commit the regenerated files in this task's commit.

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(astronomy): the spectral classes are real and nameless

Nine concepts registered with lexeme Absent(Void::Unnamed) — the first use of
that vocabulary by any domain since manifest.rs was written. A star has a
class whether or not anyone has invented spectroscopy; no culture here has
encountered the main sequence. That is Unnamed, not Gap: Gap says WE have not
got to it, and these are not waiting on us.

Nothing reads them yet — seed 42 is byte-identical."
```

---

### Task 2: `GapReason::Unnameable`, and no proto-root for the unnameable

**Files:**
- Modify: `domains/language/src/lexicon.rs:49-56` (the `GapReason` enum) and
  its `Display` impl at `:58`
- Modify: `domains/language/src/lexicon.rs` (`build_lexicon`, the family-level
  proto-root assignment around `:277-290`)
- Modify: `windows/worldgen/src/lib.rs` (`exposure_of_impl` / whatever classifies
  a concept `Unknown`, around `:4333`) — it must be able to say *why*
- Test: `domains/language/src/lexicon.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: the nine `Void::Unnamed` concepts from Task 1.
- Produces: `GapReason::Unnameable(String)`, rendering as
  `gap (unnameable): <text>`; and the invariant that a concept gapped as
  `Unnameable` receives **no proto-root**.

**Why this task exists — found in Task 1, not predicted by the plan.** Every
registered concept enters the exposures map (`windows/worldgen/src/lib.rs:3986`:
*"the map's keys are always exactly `world.registry.concepts()`'s names"*), and
`build_lexicon` assigns "a distinct proto-root to every concept in the universe
at the family level… Steeped/KnowsOf/Unknown alike." So Task 1's registration
produced this, in a committed fixture:

```
proto-goblinoid root table:  yellow-dwarf: Nogae /nogae/
daughter lexicons:           yellow-dwarf: gap (experiential): goblin has no exposure
```

The reconstructed ancestor spoke of the main sequence and every daughter forgot
it. `Void::Unnamed` is not merely inert — the machinery downstream actively
contradicts it. **Nathan chose this fix** over omitting such concepts from the
language's universe entirely, because the lexicon dump should be *able to say*
that the world holds things no one can name; going silent hides the campaign's
own finding exactly where a reader would look for it.

- [ ] **Step 1: Write the failing test**

Add to `domains/language/src/lexicon.rs`'s inline `mod tests`:

```rust
/// A gap whose provenance is "no one here can name this" renders as its own
/// kind, distinct from a gap of lived experience. `gap (experiential)` says
/// the culture never met the referent; `gap (unnameable)` says the referent
/// is beyond anyone's vocabulary here, which is a different claim.
#[test]
fn an_unnameable_gap_renders_as_its_own_provenance() {
    let reason = GapReason::Unnameable(
        "no culture here has encountered the main sequence".to_string(),
    );
    assert_eq!(
        reason.to_string(),
        "gap (unnameable): no culture here has encountered the main sequence"
    );
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-language --lib an_unnameable_gap_renders`
Expected: FAIL — `no variant named Unnameable`.

- [ ] **Step 3: Add the variant**

In `domains/language/src/lexicon.rs`, extend `GapReason`:

```rust
    /// A gap rooted in the world rather than the culture — the referent is
    /// real and objective, and no culture here has the concept to name it at
    /// all (the registry records this as
    /// `Correspondent::Absent(Void::Unnamed(..))`). Distinct from
    /// [`GapReason::Experiential`], which says a particular culture never met
    /// a referent others do name.
    Unnameable(String),
```

and add the matching `Display` arm rendering `gap (unnameable): <text>`.

The enum is small and closed on purpose — the doc on `Void` calls adding a
reason "a reviewed vocabulary change". This is that change, and the review is
Nathan's, already given.

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-language --lib an_unnameable_gap_renders`
Expected: PASS.

- [ ] **Step 5: Skip proto-root assignment for unnameable concepts**

Two edits, and the ordering constraint matters more than either:

- `windows/worldgen/src/lib.rs` must classify a concept whose manifest lexeme
  is `Correspondent::Absent(Void::Unnamed(text))` as `Unknown` with
  `GapReason::Unnameable(text)`, not `Experiential`.
- `build_lexicon` must **not** reserve a proto-root for such a concept.

**Determinism note — corrected before this task ran.** An earlier draft warned
that removing concepts from the proto-root pass would shift every subsequent
assignment and re-mint the whole lexicon. **That is wrong, and The Accession is
why.** `assign_proto_roots_with_epoch`
(`domains/language/src/etymology.rs:360`) sorts by *accession epoch first*, and
its own comment states the property: "An assignment depends only on the
concepts sorted at or before it, so sorting by epoch first makes a later-epoch
concept land **STRICTLY LAST**." The nine concepts are in the epoch-6 cohort
Task 1 appended, so removing them removes only the last nine assignments and
perturbs nothing earlier. Two tests pin this
(`assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts`,
`a_later_epoch_concept_is_insertion_stable_from_any_alphabetical_position`),
one carrying its own non-vacuity guard.

So the honest expectation is **zero committed facts moved**, the same as every
other task in this campaign — not because this task is forbidden to move them,
but because the additivity was engineered. Measure it anyway (step 7). If facts
*do* move, that is a real finding about the epoch's reach and it decides whether
this task owes an epoch bump under decision 0084 — report it rather than
accepting it as expected, because it is not.

- [ ] **Step 6: Run the gate**

Run: `make gate 2>&1 | tee /tmp/hv-vern2-t2.log`
Expected: PASS, with fixture re-pins. Expect
`windows/worldgen/tests/fixtures/proto-goblinoid-root-table-seed-42.txt` and
`.../solitary-tongue-peoples-lexicons-seed-42.txt` to change — the nine should
vanish from the root table and read `gap (unnameable)` in the lexicons.

- [ ] **Step 7: Measure what moved**

Use the fact-list comparison from Global Constraints against `$BASELINE`.
Report the count of moved facts, and `deity-name` / `settlement` names among
them. **Do not adjust anything to make it zero** — this task is allowed to move
facts; it is not allowed to move them silently.

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(language): a gap can be unnameable, and the unnameable get no root

Task 1 registered nine concepts as Void::Unnamed and the language machinery
promptly gave proto-goblinoid a word for 'yellow dwarf' — every registered
concept enters the exposures map, and build_lexicon reserves a proto-root for
the whole universe. The reconstructed ancestor spoke of the main sequence and
every daughter forgot it.

GapReason gains a third provenance so the lexicon can say WHY: not 'this
culture never met it' but 'no one here can name it at all'. Concepts gapped
that way no longer reserve a proto-root."
```

---

### Task 3: Enforce the claim

**Files:**
- Create: `cli/tests/the_unnameable.rs`

**Interfaces:**
- Consumes: the nine concepts from Task 1.
- Produces: nothing later tasks rely on.

**Why this task exists.** `hornvale_language::build_lexicon` takes its universe
from an `exposures: &BTreeMap<String, ExposureClass>` map, **not** from the
concept registry. So declaring `lexeme: Absent(Void::Unnamed)` does not by
itself prevent a word being minted for that concept — the declaration is a
comment until something checks it. This campaign has already learned that a
prohibition living only in prose does not bind: `register.rs`'s header forbids
exactly what `windows/worldgen/src/lib.rs:6585` does.

This test lives in `cli/tests/` because that is where the workspace-wide
enforcement tests live, and because it is the only layer that can see astronomy
and language at once without violating the domain-isolation rule.

- [ ] **Step 1: Write the failing test**

`cli/tests/the_unnameable.rs`:

```rust
//! `Void::Unnamed` must be a constraint, not a comment.
//!
//! A concept whose lexeme correspondent is `Absent(Void::Unnamed)` is
//! objectively real and has no word in this world. Nothing enforces that on
//! its own: `hornvale_language::build_lexicon` draws its universe from the
//! exposures map rather than the registry, so a concept could be declared
//! unnameable and still be handed a word.

use hornvale_kernel::{ConceptRegistry, Correspondent, Void};

/// Every concept the roster registers as `Unnamed`.
fn unnameable(registry: &ConceptRegistry) -> Vec<String> {
    registry
        .manifests()
        .filter(|m| {
            matches!(
                m.lexeme,
                Correspondent::Absent(Void::Unnamed(_))
            )
        })
        .map(|m| m.concept.name.clone())
        .collect()
}

/// The roster registers at least one unnameable concept. Guards the guard:
/// every assertion below is vacuous if this set is empty, and it was empty
/// for the whole life of the project until The Vernacular.
#[test]
fn the_unnameable_set_is_not_empty() {
    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    let names = unnameable(&registry);
    assert!(
        names.len() >= 9,
        "expected the nine spectral classes to be registered unnameable, got {names:?}"
    );
}

/// An unnameable concept is never handed a word by any species' lexicon.
#[test]
fn no_unnameable_concept_is_ever_lexicalized() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"));

    let forbidden = unnameable(&world.registry);
    assert!(!forbidden.is_empty(), "the fixture must not be vacuous");

    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain builds");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate builds");

    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let Ok(lexicon) =
            hornvale_worldgen::lexicon_from(&world, species, &terrain, &climate)
        else {
            continue;
        };
        for (concept, entry) in lexicon.entries() {
            let named = matches!(
                entry,
                hornvale_language::LexEntry::Root { .. }
                    | hornvale_language::LexEntry::Compound { .. }
            );
            assert!(
                !(named && forbidden.iter().any(|f| f == concept)),
                "{species} minted a word for {concept:?}, which is registered \
                 Unnamed — the declaration must bind, not decorate"
            );
        }
    }
}
```

**Signatures verified against the tree, not assumed** — an earlier draft of
this task got two of them wrong:

- `hornvale_worldgen::register_all(&mut ConceptRegistry) -> Result<(), RegistryError>`
  (`windows/worldgen/src/lib.rs:309`) takes a `&mut` and returns `Result<()>`;
  it does **not** return a registry.
- `lexicon_of` **does not exist** — The Weir retired it among thirteen
  convenience readouts. The live seam is
  `hornvale_worldgen::lexicon_from(&World, &str, &GeneratedTerrain, &GeneratedClimate)
  -> Result<hornvale_language::Lexicon, BuildError>` (`:4522`), which is what
  `windows/lab/src/metrics.rs:4294` uses. It is deliberately expensive — the
  doc comment calls it almost all of the post-name-gloss census cost — so build
  terrain and climate **once** outside the species loop, as written above.
- `ConceptRegistry::manifest(&str) -> Option<&Manifest>` (`kernel/src/registry.rs:233`)
  and `manifests() -> impl Iterator<Item = &Manifest>` (`:238`) both exist.

If any of these has moved by the time you run, grep and adapt rather than
guessing — and say in your report which signature you found.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale --test the_unnameable`
Expected: FAIL to compile, or `the_unnameable_set_is_not_empty` fails if Task 1
is not yet in the tree. If it compiles and both pass immediately, confirm the
fixture is non-vacuous before believing it.

- [ ] **Step 3: Make it pass**

If Task 1 landed, `the_unnameable_set_is_not_empty` should pass on its own. For
`no_unnameable_concept_is_ever_lexicalized`, the expected outcome is **green
without production changes** — the nine concepts are not in any exposure map,
so no lexicon reaches them. If it goes red, that is a real finding: a concept
declared unnameable is being named, and the fix belongs in whatever builds the
exposures map. Report it rather than relaxing the test.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale --test the_unnameable`
Expected: PASS, with a non-vacuous unnameable set.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "test(cli): Void::Unnamed must bind, not decorate

build_lexicon draws its universe from the exposures map, not the registry, so
declaring a concept unnameable does not by itself stop a word being minted for
it. This campaign has already seen a prohibition that lived only in a doc
comment fail to bind. Asserts the unnameable set is non-empty (it was empty for
the project's whole life until now) and that no species' lexicon reaches it."
```

---

### Task 4: The first reading of the unnameable fraction

**Files:**
- Modify: `docs/superpowers/specs/2026-07-31-the-vernacular-design.md` (the
  stage 3.5 section)

**Interfaces:**
- Consumes: Tasks 1 and 2.
- Produces: the recorded baseline stages 3–5 build on.

Spec stage 3.5 calls the unnameable fraction "the campaign's most interesting
readout". This task takes its first reading — not by building a lab metric,
which is stage 3.5's own work, but by reading the number the registry report
already computes and has always shown as zero.

- [ ] **Step 1: Read the number**

```bash
cargo run -q -p hornvale -- concepts > /tmp/hv-concepts.txt
grep -iE "unnamed|imperceptible|gap|uncognized" /tmp/hv-concepts.txt
```

Record: how many concepts carry each `Void` on each edge, and the total
concept count. `cli/src/concepts.rs` tallies `Unnamed` and `Imperceptible`
explicitly, so the columns exist; before Task 1 both read zero.

- [ ] **Step 2: Write the reading into the spec**

Add to stage 3.5 a short subsection giving: the total registered concepts, the
count and fraction now `Unnamed` on the lexeme edge, the count now
`Imperceptible` on the percept edge, and the commit measured at. State plainly
that both columns read **zero** for the whole life of the project before this
campaign — that is the finding, and it is what makes the number interesting
rather than decorative.

Do **not** editorialize the fraction as high or low. Nine of several hundred is
a floor, not a measurement of the world's unnameability: it counts only what
astronomy has so far declared honestly, and §3.1 argues `Void::Gap`'s 23 uses
are absorbing cases that belong here. Say that.

- [ ] **Step 3: Run the gate**

Run: `make gate 2>&1 | tee /tmp/hv-vern2-t3.log`
Expected: PASS. Budget ~15 minutes; run on a quiet box and do not start a
second gate in parallel.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "docs(the-vernacular): the unnameable fraction, first reading"
```

---

## Self-review

**Spec coverage.** §3.1's argument → Task 1's registration and its doc comment.
§5 stage 3 item 1's first half (declare manifests, free and reversible) →
Task 1. The enforcement §3.1 implies but does not state → Task 2, which exists
because `build_lexicon`'s universe is the exposures map and the declaration
would otherwise be inert. §5 stage 3.5's first reading → Task 3. **Explicitly
NOT covered here, by design:** changing the `star-class` fact's value from
prose to a concept id (part 3 — moves facts, needs an epoch measurement under
0084), the renderer circumlocution fallback (stage 3), and the bidirectional
lint (stage 4).

**Type consistency.** `Void::Unnamed(&'static str)` and
`Void::Imperceptible(&'static str)` both take one `&'static str` — matching
`kernel/src/manifest.rs:86-98`. `Correspondent::Absent(V)` matches `:74-79`.
`ConceptKind::Celestial` matches `kernel/src/registry.rs:32` and is what
astronomy's existing manifests use. The nine key strings are identical in
Task 1's test, Task 1's registration, and Task 2's assertion.

**Known soft spot, stated rather than guessed.** Task 2 names
`hornvale_worldgen::register_all` and `lexicon_of` as the seams it wants, and
tells the implementer to grep first — `lexicon_of` was among the thirteen
convenience readouts The Weir deleted, so it may not exist. The task says what
to do if it does not, and forbids widening the public API without the
controller.
