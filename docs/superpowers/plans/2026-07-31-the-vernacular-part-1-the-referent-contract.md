# The Vernacular, part 1 — the referent contract

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give every phenomenon a machine-readable `referent` (a registered
concept id plus registered qualifier concepts), flip the gloss consumers onto
it, and prove that rewording a description now moves zero committed facts.

**Architecture:** `kernel/src/phenomena.rs` gains a `Referent` struct and
`Phenomenon` gains a non-optional `referent` field, so the compiler forces all
31 construction sites to declare what their phenomenon is *about*. The two
copies of `phenomenon_concept` (composition root and lab metric) then read
`referent.concept` instead of grepping `description`, through a shared roster
that preserves today's gloss codomain exactly — so this plan is a pure refactor
and must move zero facts. Widening the codomain (letting eclipses and tides
gloss, which they cannot today) is a deliberate world change and is **out of
scope here**.

**Decision 0094 governs Task 2's shape.** It landed on `main` on 2026-07-31,
after this campaign's spec was approved, and names `phenomenon_concept` in its
own scope list: a deliberate duplicate shares its *roster* (which kinds must be
answered for) and never its *derivation* (what the answer is). So the kind gate
becomes one `GLOSSING_KINDS` list read by both sides, worldgen keeps its closed
codomain match, and the lab's second opinion is **re-grounded on the concept
registry and the lexicon** — a source of truth the gloss path never consults —
rather than copying worldgen's match arms. This resolves what the spec's §8
risk 1 left open.

**Tech Stack:** Rust edition 2024, `serde` only (decision 0004). No new
dependencies. `cargo nextest` for tests, `make gate` as the commit gate.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only
  (enforced by `clippy.toml` `disallowed-types`).
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). Adding a pub
  field to `Phenomenon` drifts `docs/audits/type-audit-report.md`, which is a
  committed artifact and part of the drift check.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding.
- **Two Bash guards this repo enforces, found the hard way in Task 1:** the raw
  whole-workspace nextest invocation is blocked — use `make gate`, a strict
  superset — and bare `git stash` / `git stash pop` are blocked; use
  `git stash push -u -m <tag>`, `git stash apply <sha>`, `git stash drop`, and
  never `pop`. For byte-identity checks, prefer the pre-snapshotted
  `$BASELINE` file over any stash dance.
- **Two test runs in one Bash call are blocked too.** The suite's cost is test
  *runtime*, not compilation, so asking it two questions costs it twice.
  Capture once and grep the file.
- **This plan must move zero committed facts.** Any task that moves one has
  found a bug in that task, not a result.
- Layering is constitutional: `kernel/` → `domains/*` → `windows/*` → `cli/`.
  A domain depends on `hornvale-kernel` and nothing else.

## Scope

This plan implements **stages 1 and 2** of
`docs/superpowers/specs/2026-07-31-the-vernacular-design.md`. Stage 3 (deriving
renderings), stage 4 (the prose-audit lint) and stage 5 (book and close) are
separable and get their own plans — each of those produces working, testable
software on its own, and stage 1+2 alone already leaves the tree strictly
better: the semantic channel becomes typed and the reword hazard is closed.

---

### Task 1: The `Referent` type and the `Phenomenon` field

**Files:**
- Modify: `kernel/src/phenomena.rs:27-39` (the `Phenomenon` struct)
- Modify: `kernel/src/lib.rs` (re-export `Referent`)
- Modify: `domains/astronomy/src/lib.rs:476`, `domains/astronomy/src/provider.rs`
  (13 sites), `domains/climate/src/lib.rs:283`,
  `domains/climate/src/provider.rs` (4 sites), `domains/religion/src/lib.rs:469`,
  `windows/lab/src/metrics.rs:6134`, `windows/almanac/src/lib.rs`,
  `kernel/tests/determinism.rs`, `kernel/examples/first_light.rs`
- Test: `kernel/src/phenomena.rs` (inline `mod tests`)

**Interfaces:**
- Produces: `hornvale_kernel::Referent { concept: String, qualifiers: Vec<String> }`,
  `Referent::of(concept: &str) -> Referent`,
  `Referent::qualified(concept: &str, qualifiers: &[&str]) -> Referent`, and
  `Phenomenon.referent: Referent`.
- Consumes: nothing. No consumer reads `referent` in this task.

- [ ] **Step 1: Write the failing test**

Add to the inline `mod tests` in `kernel/src/phenomena.rs`:

```rust
#[test]
fn a_referent_names_a_concept_and_its_qualifiers() {
    let plain = Referent::of("moon");
    assert_eq!(plain.concept, "moon");
    assert!(plain.qualifiers.is_empty());

    let qualified = Referent::qualified("star", &["red", "new"]);
    assert_eq!(qualified.concept, "star");
    assert_eq!(qualified.qualifiers, vec!["red", "new"]);
}

#[test]
fn a_referent_round_trips_through_json() {
    let r = Referent::qualified("eclipse", &["sun"]);
    let json = serde_json::to_string(&r).expect("a referent serializes");
    let back: Referent = serde_json::from_str(&json).expect("a referent deserializes");
    assert_eq!(r, back);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel --lib phenomena::tests::a_referent`
Expected: FAIL — `cannot find type Referent in this scope`.

- [ ] **Step 3: Write the type**

In `kernel/src/phenomena.rs`, above `Phenomenon`:

```rust
/// What a phenomenon is *about*, in the world's own vocabulary: the
/// registered concept it refers to, plus the registered concepts that
/// qualify it.
///
/// This is the machine-facing half of a phenomenon and **the only field a
/// consumer may branch on**. [`Phenomenon::description`] is a rendering
/// derived from this one way; nothing may parse it back (decision 0022, and
/// `hornvale_language::register`'s content→render seam). Before this type
/// existed, `windows/worldgen` decided which concept a phenomenon glossed to
/// — and therefore what a people's deity was named — by grepping the
/// description for `"moon"`; rewording one description moved 73 committed
/// facts on seed 42.
///
/// Every id here is a **concept-registry key**, never prose: `moon`, not
/// `"a vast moon"`. Qualifiers are registry keys too, which is load-bearing
/// rather than tidy — a colour that a culture has not lexicalized under the
/// Berlin & Kay ladders simply has no key, so it cannot be said.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(identifier-text: qualifiers)
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Referent {
    /// The registered concept id this phenomenon is about.
    pub concept: String,
    /// Registered concept ids qualifying the head, in producer-declared
    /// order. Empty is the common case.
    pub qualifiers: Vec<String>,
}

impl Referent {
    /// A referent naming `concept` with no qualifiers.
    pub fn of(concept: &str) -> Referent {
        Referent {
            concept: concept.to_string(),
            qualifiers: Vec::new(),
        }
    }

    /// A referent naming `concept`, qualified by `qualifiers` in order.
    pub fn qualified(concept: &str, qualifiers: &[&str]) -> Referent {
        Referent {
            concept: concept.to_string(),
            qualifiers: qualifiers.iter().map(|q| (*q).to_string()).collect(),
        }
    }
}
```

Add the field to `Phenomenon`, immediately above `description` so the ordering
reads content-then-render:

```rust
    /// What this phenomenon is about, in registry keys. The only field a
    /// consumer may branch on.
    pub referent: Referent,
```

Re-export from `kernel/src/lib.rs` beside the existing `Phenomenon` export.

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-kernel --lib phenomena::tests::a_referent`
Expected: PASS (2 tests).

- [ ] **Step 5: Populate every construction site**

The build is now broken at all 31 `Phenomenon {` sites. This is the point of a
non-optional field. Populate them from this table — every value is a key
already present in `book/src/reference/concept-registry-generated.md`:

| File:line | kind | `referent` |
|---|---|---|
| `domains/astronomy/src/lib.rs:476` | celestial-body | `Referent::of("sun")` |
| `domains/astronomy/src/provider.rs:1551` | celestial-body | `Referent::of("sun")` |
| `domains/astronomy/src/provider.rs:1558` | celestial-body | `Referent::of("sun")` |
| `domains/astronomy/src/provider.rs:1597` | eclipse | `Referent::qualified("eclipse", &["sun"])` |
| `domains/astronomy/src/provider.rs:1610` | celestial-body | `Referent::qualified("moon", size_concept(angular))` |
| `domains/astronomy/src/provider.rs:1634` | eclipse | `Referent::qualified("eclipse", &["moon"])` |
| `domains/astronomy/src/provider.rs:1692` | eclipse | `Referent::qualified("eclipse", &["sun"])` |
| `domains/astronomy/src/provider.rs:1702` | eclipse | `Referent::qualified("eclipse", &["moon"])` |
| `domains/astronomy/src/provider.rs:1729` | tide | `Referent::qualified("tide", &["moon"])` |
| `domains/astronomy/src/provider.rs:1756` | tide | `Referent::qualified("tide", &["two", "moon"])` |
| `domains/astronomy/src/provider.rs:1771` | seasonal-cycle | `Referent::of("day")` |
| `domains/astronomy/src/provider.rs:1782` | night-star | `Referent::of("star")` |
| `domains/astronomy/src/provider.rs:1812` | heliacal-rising | `Referent::qualified("star", &["new"])` |
| `domains/astronomy/src/provider.rs:1821` | heliacal-setting | `Referent::qualified("star", &["old"])` |
| `domains/astronomy/src/provider.rs:1897` | wandering-star | `Referent::qualified("star", &["move"])` |
| `domains/climate/src/lib.rs:283` | ambient | `Referent::of("wind")` |
| `domains/climate/src/provider.rs:502` | ambient | `Referent::of("wind")` |
| `domains/climate/src/provider.rs:528` | heat | `Referent::of("heat")` |
| `domains/climate/src/provider.rs:536` | cold | `Referent::of("cold")` |
| `domains/climate/src/provider.rs:559` | rain / snow | `Referent::of(if frozen { "snow" } else { "rain" })` |

For `domains/astronomy/src/provider.rs:1610`, add this helper next to
`size_word` — **do not delete `size_word`; this plan changes no rendering**:

```rust
/// The registered qualifier concept for a moon of this angular diameter,
/// parallel to [`size_word`]'s prose at the same thresholds. `great` and
/// `little` are pack concepts; a middling moon takes no qualifier, which is
/// why this returns a slice rather than a single key.
fn size_concept(angular: f64) -> &'static [&'static str] {
    if angular >= 1.2 {
        &["great"]
    } else if angular >= 0.7 {
        &[]
    } else {
        &["little"]
    }
}
```

and construct with `Referent::qualified("moon", size_concept(angular))`.

The remaining sites are test fixtures and doc examples (all verified by
`grep -rn "Phenomenon {"`):

- `domains/religion/src/lib.rs:466` — a test helper
  `fn ph(kind, desc, period, salience, venue) -> Phenomenon`. Add a `concept:
  &str` parameter after `kind` and pass `Referent::of(concept)`; update its
  call sites in that module to name whatever their `desc` already names.
- `windows/lab/src/metrics.rs:6135` — the closure
  `|kind: &str, description: &str|` inside
  `presiding_concepts_are_phenomenon_concepts_codomain`. Add a `concept: &str`
  parameter; its six cases take **`moon`, `star`, `sun`, `day`, `star`,
  `wind`** in the order they appear. **These six are what keep the codomain
  test honest after Task 2** — get them right, and derive each one from what
  the old substring logic returned for that case's *description*, not from the
  order the cases happen to sit in. (Case 1's description is `"the moon rides
  high"`, so it is `moon`. An earlier draft of this plan said `sun` here; the
  error was inert while `phenomenon_concept` still read descriptions and only
  surfaced in Task 2, where it failed `assert_eq!(produced, listed)` because
  the produced set no longer contained `moon`.)
- `windows/almanac/src/lib.rs:568` — one site, inside a render fixture:
  `Referent::of("sun")`.
- `kernel/tests/determinism.rs:14` and `kernel/examples/first_light.rs:66` —
  one site each: `Referent::of("sun")`.

- [ ] **Step 6: Verify the workspace builds and the suite is green**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-vern-t1.txt`
Expected: PASS, same test count as `main` (no tests added beyond the two in
step 1). If any test fails, it is a mis-populated referent — read the failure,
do not adjust the test.

- [ ] **Step 7: Prove zero facts moved**

Run:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-vern-t1-world.json
diff "$BASELINE" /tmp/hv-vern-t1-world.json && echo "IDENTICAL"
```

where `$BASELINE` is
`.superpowers/sdd/2026-07-31-the-vernacular-part-1-the-referent-contract/baseline-seed-42.json`
— a seed-42 world the controller snapshotted at the task boundary. Do **not**
try to build a baseline with a stash dance: bare `git stash` / `git stash pop`
are blocked by this repo's Bash guards.

Expected: `IDENTICAL`. Adding an unread field cannot move a fact; if it does,
something reads `Phenomenon` by structural serialization and that is a finding
to report before continuing.

- [ ] **Step 8: Refresh the drifted artifacts**

Adding a pub field drifts the type-audit report. Run:

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: `docs/audits/type-audit-report.md` changes and **nothing else**. A
change under `book/src/gallery/` at this task means a rendering moved, which
this task must not do.

- [ ] **Step 9: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(kernel): phenomena carry a machine-readable referent

A Referent is a registered concept id plus registered qualifier concepts —
the machine-facing half of a phenomenon, and the only field a consumer may
branch on. The field is non-optional so the compiler forces all 31
construction sites to declare what their phenomenon is about.

Nothing reads it yet: seed 42 is byte-identical."
```

---

### Task 2: The gloss reads the referent

**Files:**
- Modify: `windows/worldgen/src/lib.rs:3464-3494` (function at `:3478`) (`phenomenon_concept`)
- Modify: `windows/lab/src/metrics.rs:4252-4276` (the second copy)
- Test: `windows/worldgen/src/lib.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: `hornvale_kernel::Referent` and `Phenomenon.referent` from Task 1.
- Produces: `hornvale_worldgen::GLOSSING_KINDS: &[&str]` (the shared roster) and
  `hornvale_worldgen::gloss_concept_of` (added in Task 3).
  `windows/worldgen`'s private `fn phenomenon_concept(&Phenomenon) ->
  Option<&'static str>` keeps its exact signature, so all seven of its call
  sites are untouched. `windows/lab`'s copy changes return type to
  `Option<&str>` and gains a sibling `referent_is_nameable`.

**The roster preserves the codomain, and that is the whole point of this task.**
Today `phenomenon_concept` returns `Some(..)` only for `celestial-body`,
`seasonal-cycle`, `night-star` and `ambient`, and `None` for everything else —
so eclipses, tides, heat, cold, rain and snow contribute nothing to a gloss
even though they now carry perfectly good referents. Keeping that set is what
makes this task a refactor. **Do not widen `GLOSSING_KINDS`**; widening is a
deliberate world change with its own spec flag and its own epoch measurement.

- [ ] **Step 1: Write the failing test**

Add to `windows/worldgen/src/lib.rs`'s inline `mod tests`:

```rust
/// The gloss reads the referent, not the prose. Rewording a description
/// must not change which concept a phenomenon glosses to — that coupling
/// moved 73 committed facts on seed 42 before the referent existed.
#[test]
fn the_gloss_ignores_the_description() {
    let moon = |description: &str| hornvale_kernel::Phenomenon {
        kind: hornvale_astronomy::CELESTIAL_BODY.to_string(),
        referent: hornvale_kernel::Referent::of("moon"),
        description: description.to_string(),
        period_days: None,
        salience: 1.0,
        venue: hornvale_kernel::Venue::NightSky,
    };
    assert_eq!(phenomenon_concept(&moon("a vast moon")), Some("moon"));
    assert_eq!(phenomenon_concept(&moon("a vast lunar disc")), Some("moon"));
    assert_eq!(phenomenon_concept(&moon("")), Some("moon"));
}

/// The codomain is unchanged: kinds that did not gloss before still do not,
/// even though they now carry referents.
#[test]
fn kinds_outside_the_gloss_codomain_stay_silent() {
    let eclipse = hornvale_kernel::Phenomenon {
        kind: hornvale_astronomy::ECLIPSE.to_string(),
        referent: hornvale_kernel::Referent::qualified("eclipse", &["sun"]),
        description: "the sun is devoured".to_string(),
        period_days: None,
        salience: 1.0,
        venue: hornvale_kernel::Venue::DaySky,
    };
    assert_eq!(phenomenon_concept(&eclipse), None);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-worldgen --lib the_gloss_ignores_the_description`
Expected: FAIL — `phenomenon_concept(&moon("a vast lunar disc"))` returns
`Some("sun")`, because the substring branch falls through.

- [ ] **Step 3: Publish the shared roster (decision 0094)**

**Decision 0094 lands on this task and governs its shape**: a deliberate
duplicate shares its *roster* — what classes of thing must be answered for —
and never its *derivation*. It names `phenomenon_concept` in its own scope
list. So the kind gate becomes one declarative list read by both sides, while
the two sides keep computing independently.

In `windows/worldgen/src/lib.rs`, beside `phenomenon_concept`:

```rust
/// The phenomenon kinds that gloss — the shared **roster** under decision
/// 0094, read by this crate's `phenomenon_concept` and independently by
/// `windows/lab`'s nameability check. It names *which questions must be
/// answered*, never *what the answers are*: adding a kind here obliges both
/// sides to account for it, and neither side learns the other's derivation.
///
/// Deliberately narrower than the set of phenomena that carry referents.
/// Eclipses, tides, heat, cold, rain and snow all name real registered
/// concepts and would gloss if listed — but they never have, so listing them
/// is a **world change**, not a refactor. Widen only with a spec behind it,
/// and expect `PRESIDING_CONCEPTS` in `windows/lab/src/metrics.rs` to red
/// until it is widened to match.
/// type-audit: bare-ok(identifier-text)
pub const GLOSSING_KINDS: &[&str] = &[
    hornvale_astronomy::CELESTIAL_BODY,
    hornvale_astronomy::SEASONAL_CYCLE,
    hornvale_astronomy::NIGHT_STAR,
    hornvale_climate::AMBIENT,
];
```

- [ ] **Step 4: Rewrite worldgen's derivation**

Replace the body, and replace the doc comment's "disambiguates by its
description text" paragraph:

```rust
/// The concept a phenomenon glosses to, for glossed naming (Task 9).
///
/// Reads `referent.concept` — never the description. The rostered kinds are
/// [`GLOSSING_KINDS`]; the closed codomain below is this side's own
/// derivation, which `windows/lab` does not share (decision 0094).
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&'static str> {
    if !GLOSSING_KINDS.contains(&phenomenon.kind.as_str()) {
        return None;
    }
    // Returned as `&'static str` so callers keep their existing signature:
    // the codomain is closed, and a referent outside it is a producer bug.
    match phenomenon.referent.concept.as_str() {
        "sun" => Some("sun"),
        "moon" => Some("moon"),
        "star" => Some("star"),
        "day" => Some("day"),
        "wind" => Some("wind"),
        _ => None,
    }
}
```

- [ ] **Step 5: Re-ground the lab's second opinion on the registry**

`windows/lab/src/metrics.rs:4252` reads the same roster but must **not** copy
worldgen's codomain match — that would be a second opinion that agrees by
construction, which 0094 says is not a second opinion. Its independent
computation is **nameability**, derived from a different source of truth: the
concept registry and the culture's lexicon, neither of which worldgen consults
when glossing.

Replace the copy with:

```rust
/// The concept a phenomenon glosses to, read from the shared roster
/// (`hornvale_worldgen::GLOSSING_KINDS`) and the phenomenon's own referent.
///
/// This is a READ, not a derivation — the derivation this crate owns is
/// [`referent_is_nameable`] below, which answers the same roster from the
/// concept registry and the lexicon rather than from worldgen's codomain.
/// Decision 0094: share the roster, never the derivation. Before The
/// Vernacular this function re-implemented worldgen's mapping by grepping the
/// phenomenon's English description, which made the gloss a function of prose.
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&str> {
    hornvale_worldgen::GLOSSING_KINDS
        .contains(&phenomenon.kind.as_str())
        .then(|| phenomenon.referent.concept.as_str())
}

/// This crate's own derivation over the shared roster: is a rostered
/// phenomenon's referent a concept the world can actually *say*?
///
/// Independent of worldgen by construction — it consults the concept registry
/// and the culture's lexicon, which the gloss path never reads. A referent
/// that is unregistered, outside the presiding codomain, or a lexical `Gap`
/// for this culture is a phenomenon whose deity could never be named after it,
/// which is exactly the defect The Vernacular exists to make visible.
fn referent_is_nameable(
    phenomenon: &Phenomenon,
    registry: &hornvale_kernel::ConceptRegistry,
    lexicon: &hornvale_language::Lexicon,
) -> Option<bool> {
    let concept = phenomenon_concept(phenomenon)?;
    Some(
        registry.concept(concept).is_some()
            && PRESIDING_CONCEPTS.contains(&concept)
            && !matches!(lexicon.entry(concept), None | Some(hornvale_language::LexEntry::Gap { .. })),
    )
}
```

Check `LexEntry`'s actual `Gap` variant shape before writing that last line —
`grep -n "enum LexEntry" -A 20 domains/language/src/lexicon.rs`. If `Gap`
carries fields the `{ .. }` pattern does not fit, match its real shape; do not
change `LexEntry`.

Note the return type change to `Option<&str>` (borrowed from the referent
rather than `&'static str`). Fix the call site at
`windows/lab/src/metrics.rs:4310` — it feeds `presiding` into the site-concept
vector; a borrow of the phenomenon lives long enough there, but if the borrow
checker disagrees, `.map(str::to_string)` at the call site rather than
reintroducing a `&'static` codomain match.

While here, delete the stale citation of `cli/tests/words_identity.rs` from
that comment: **that file does not exist**. It was added in `79dbe768` and
deleted in `4c3d3f7f` ("retire words/tongues identity, superseded by branches
keystone"); a workspace grep for `fn phenomenon_concept` finds two definitions,
not three. Decision 0094's own scope list repeats the same stale count — that
is Nathan's to handle, not this task's; do not edit the decision.

- [ ] **Step 5b: Wire the nameability check into a test**

Add to `windows/lab/src/metrics.rs`'s inline `mod tests`:

```rust
/// Every rostered phenomenon in seed 42 names a concept the world can say.
/// The lab's own derivation over the shared roster (decision 0094) — it asks
/// the registry and the lexicon, never worldgen's codomain.
#[test]
fn every_rostered_referent_is_nameable() {
    let view = WorldView::new(hornvale_kernel::Seed(42)).expect("seed 42 builds");
    let world = view.world();
    let lexicon = species_lexicon(&view, "goblin").expect("goblin has a lexicon");
    for p in hornvale_worldgen::observed_phenomena(world, 0.0).expect("phenomena") {
        if let Some(nameable) = referent_is_nameable(&p, &world.registry, &lexicon) {
            assert!(
                nameable,
                "rostered phenomenon {:?} refers to {:?}, which this world cannot name",
                p.kind, p.referent.concept
            );
        }
    }
}
```

`WorldView::new` and the species-lexicon helper already exist in this module
(the file builds lexicons for `name-gloss-true` today) — grep for
`fn species_lexicon` and for how the existing gloss metrics obtain a
`WorldView`, and use those names rather than the placeholders above if they
differ.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cargo nextest run -p hornvale-worldgen -p hornvale-lab 2>&1 | tail -20`
Expected: PASS, including
`presiding_concepts_are_phenomenon_concepts_codomain` — its six fixture cases
were given referents in Task 1 step 5 and its expected codomain
(`day, moon, star, sun, wind`) is unchanged.

- [ ] **Step 7: Prove zero facts moved — the task's real gate**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-vern-t2-world.json
diff "$BASELINE" /tmp/hv-vern-t2-world.json && echo "IDENTICAL"
```

Expected: `IDENTICAL`. This is the spec's §7 prediction for the refactor half.
If facts moved, a referent in Task 1's table disagrees with what the old
substring hack produced for that phenomenon — find which, and fix the
referent; do not adjust the gate to match.

- [ ] **Step 8: Confirm no artifact drift**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ && echo "NO DRIFT"
```

Expected: `NO DRIFT`.

- [ ] **Step 9: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor(worldgen,lab): gloss from the referent, not the prose

phenomenon_concept stops grepping the English display sentence for 'moon'
and reads referent.concept. The kind gate preserves today's codomain exactly
— eclipses and tides carry referents but still do not gloss — so this is a
refactor: seed 42 is byte-identical.

Also drops a stale citation of cli/tests/words_identity.rs, which does not
exist; there are two copies of this mapping, not three."
```

---

### Task 3: The reword-invariance test

**Files:**
- Create: `cli/tests/prose_is_not_a_contract.rs`

**Interfaces:**
- Consumes: `phenomenon_concept`'s new behaviour from Task 2 (indirectly, via
  world generation).
- Produces: nothing consumed by later tasks.

This is the campaign's preregistered instrument (spec §7), and it belongs in
the gate rather than in a scratch buffer: without it, the next campaign to
reword a description has no way to learn that it used to matter.

- [ ] **Step 1: Write the failing test**

`cli/tests/prose_is_not_a_contract.rs`:

```rust
//! The Vernacular's preregistered instrument: a phenomenon's English
//! description is a rendering, not a contract, so a semantically null reword
//! must move zero committed facts.
//!
//! Before the referent existed, rewording one description in
//! `domains/astronomy/src/provider.rs` from `"a {} moon"` to `"a {} lunar
//! disc"` moved 73 facts on seed 42 — 9 of 48 deity names and 7 of 48
//! epithets — because `phenomenon_concept` dispatched on
//! `description.contains("moon")`. This test is the standing proof that the
//! coupling is gone.
//!
//! It works by mutating the phenomenon list a world was built from, rather
//! than by editing source: every description is replaced with a string that
//! shares no substring with any concept id, and the gloss must be unmoved.

use hornvale_kernel::{Phenomenon, Referent, Seed, World};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, gloss_concept_of, observed_phenomena};

/// Seed 42 at default pins — the same world the gallery almanacs describe.
fn world() -> World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
}

/// Every description replaced by prose that names nothing.
fn reworded(phenomena: &[Phenomenon]) -> Vec<Phenomenon> {
    phenomena
        .iter()
        .enumerate()
        .map(|(i, p)| Phenomenon {
            description: format!("an occurrence of the {i}th kind"),
            ..p.clone()
        })
        .collect()
}

#[test]
fn rewording_every_description_leaves_the_gloss_unmoved() {
    let world = world();
    let phenomena = observed_phenomena(&world, 0.0)
        .unwrap_or_else(|e| panic!("seed 42 has phenomena: {e}"));

    let before: Vec<Option<&str>> = phenomena.iter().map(gloss_concept_of).collect();
    let after: Vec<Option<&str>> = reworded(&phenomena).iter().map(gloss_concept_of).collect();

    assert_eq!(
        before, after,
        "a null reword moved the gloss — the description is load-bearing again"
    );
    assert!(
        before.iter().any(Option::is_some),
        "the fixture must actually exercise the gloss, not pass vacuously"
    );
}

#[test]
fn a_referent_never_carries_prose() {
    let world = world();
    for p in observed_phenomena(&world, 0.0).expect("phenomena") {
        for key in std::iter::once(&p.referent.concept).chain(p.referent.qualifiers.iter()) {
            assert!(
                !key.contains(' ') && key.chars().all(|c| c.is_ascii_lowercase() || c == '-'),
                "referent key {key:?} is prose, not a registry key"
            );
        }
    }
}

/// The keys a referent names must exist in the registry — otherwise the
/// lexicon can never reach them, which is the whole defect this campaign
/// closes.
#[test]
fn every_referent_key_is_registered() {
    let world = world();
    for p in observed_phenomena(&world, 0.0).expect("phenomena") {
        for key in std::iter::once(&p.referent.concept).chain(p.referent.qualifiers.iter()) {
            assert!(
                world.registry.concept(key).is_some(),
                "referent key {key:?} is not a registered concept"
            );
        }
    }
}

/// Guards the guard: `reworded` must actually change every description, or
/// the first test passes for the wrong reason.
#[test]
fn the_rewording_fixture_changes_every_description() {
    let p = Phenomenon {
        kind: "celestial-body".to_string(),
        referent: Referent::of("moon"),
        description: "a vast moon".to_string(),
        period_days: None,
        salience: 1.0,
        venue: hornvale_kernel::Venue::NightSky,
    };
    let out = reworded(&[p.clone()]);
    assert_ne!(out[0].description, p.description);
    assert_eq!(out[0].referent, p.referent);
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale --test prose_is_not_a_contract`
Expected: FAIL to compile — `gloss_concept_of` is not public in
`hornvale_worldgen`. (`build_world` at `windows/worldgen/src/lib.rs` and
`observed_phenomena` at `:3064` are both already public and take the argument
lists used above — verified.)

- [ ] **Step 3: Expose the one missing seam**

In `windows/worldgen/src/lib.rs`, add a public wrapper beside the private
`phenomenon_concept`. Do **not** make `phenomenon_concept` itself public — its
privacy is what keeps the mapping a composition-root judgment call:

```rust
/// The concept a phenomenon glosses to — the public face of the private
/// [`phenomenon_concept`], exported so the reword-invariance battery in
/// `cli/tests/prose_is_not_a_contract.rs` can assert the gloss is a function
/// of the referent alone.
pub fn gloss_concept_of(phenomenon: &Phenomenon) -> Option<&'static str> {
    phenomenon_concept(phenomenon)
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale --test prose_is_not_a_contract`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "test(cli): prose is not a contract

The campaign's preregistered instrument, standing in the gate: replacing
every phenomenon description with prose that names nothing must leave the
gloss unmoved. Before the referent, the equivalent reword moved 73 facts on
seed 42.

Also asserts every referent key is a registered concept and looks like a
registry key rather than prose."
```

---

### Task 4: Retire the remaining description reads

**Files:**
- Modify: `windows/worldgen/src/lib.rs:7577` (the dedup key)
- Modify: `domains/astronomy/src/provider.rs` (test assertions at :98, :113,
  :129, :179, :233, :264, :293, :692, :777, :795)
- Modify: `windows/worldgen/src/lib.rs:8517`

**Interfaces:**
- Consumes: `Phenomenon.referent` from Task 1.
- Produces: nothing consumed by later tasks.

After Task 2 the gloss is clean, but the description is still read as a dedup
key and as a test oracle. Both are the same defect at lower stakes: the dedup
key silently collapses two semantically different skies that render the same
sentence, and the test oracles pin English so that stage 3 cannot change a
rendering without a wall of unrelated red.

- [ ] **Step 1: Write the failing test**

Add to `windows/worldgen/src/lib.rs`'s inline `mod tests`:

```rust
/// Two phenomena that render identically but mean different things must not
/// collapse into one another in the dedup pass.
#[test]
fn dedup_separates_referents_that_share_a_rendering() {
    let same_prose = |concept: &str| hornvale_kernel::Phenomenon {
        kind: hornvale_astronomy::CELESTIAL_BODY.to_string(),
        referent: hornvale_kernel::Referent::of(concept),
        description: "a light in the sky".to_string(),
        period_days: None,
        salience: 1.0,
        venue: hornvale_kernel::Venue::NightSky,
    };
    let mut seen = std::collections::BTreeSet::new();
    for p in [same_prose("sun"), same_prose("moon")] {
        seen.insert(p.referent.clone());
    }
    assert_eq!(seen.len(), 2, "sun and moon must not dedup together");
}
```

`Referent` needs `Ord` for `BTreeSet` — add `PartialOrd, Ord` to its derive
list in `kernel/src/phenomena.rs` (it is all-`String`, so the derive is total
and deterministic; no `total_cmp` concern).

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-worldgen --lib dedup_separates_referents`
Expected: FAIL to compile — `Referent: Ord` is not satisfied.

- [ ] **Step 3: Flip the dedup key and the oracles**

At `windows/worldgen/src/lib.rs:7577`, change `seen.insert(r.description)` to
`seen.insert(r.referent.clone())` and retype `seen` to
`BTreeSet<hornvale_kernel::Referent>`.

At `windows/worldgen/src/lib.rs:8517`, change
`ph.iter().any(|p| p.description.contains("sun"))` to
`ph.iter().any(|p| p.referent.concept == "sun")`.

In `domains/astronomy/src/provider.rs`, convert each listed test assertion from
a description substring to a referent check. Worked example — line 129:

```rust
// before
.filter(|p| p.kind == CELESTIAL_BODY && p.description.contains("moon"))
// after
.filter(|p| p.kind == CELESTIAL_BODY && p.referent.concept == "moon")
```

Line 98 (`p.description.starts_with("the sun")`) becomes
`p.referent.concept == "sun"`. Line 179
(`p.description == neighbor.night_description()`) becomes
`p.referent == Referent::of("star")` — the neighbour's *colour* is not yet in
the referent, so this assertion legitimately weakens.

**Do not mark that with a `TODO`.** The workspace contains **zero** TODO
comments (`grep -rn "// TODO" --include=*.rs kernel/ domains/ windows/ cli/`
returns none), and the Definition of Done bans TODOs without issue numbers.
Weakened-assertion debt goes in the campaign's followup register
(`.superpowers/sdd/followups.md`), which is promoted into the retrospective at
close — that is where this repo keeps such findings. Write a plain doc comment
on the assertion saying what it does and does not check, with no `TODO` token,
and append a followup entry naming the file, the line, and what stage 3 owes
it.

**Leave alone** any assertion whose subject is the *rendering itself* — lines
:717, :719 (`"The light is golden."`), :860, :867 (`"shows its full face"`),
:493, :522, :606. Those are rendering goldens and are stage 3's business; they
must stay red-able so stage 3 cannot silently change prose.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-vern-t4.txt`
Expected: PASS. Then confirm the byte-identity gate again:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-vern-t4-world.json
diff "$BASELINE" /tmp/hv-vern-t4-world.json && echo "IDENTICAL"
```

Expected: `IDENTICAL`. The dedup change is the one place in this plan where
behaviour *could* legitimately move — if it does, stop and report: it means
two phenomena were being collapsed that should not have been, which is a real
finding and a spec-§7 result rather than a bug to paper over.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor: dedup and test oracles read the referent

The dedup key was the rendering, so two phenomena that happened to render
the same sentence collapsed into one. Test oracles that asked 'does the
prose contain moon' now ask what the phenomenon is about; oracles that are
genuinely about the rendering are left alone for stage 3 to move."
```

---

### Task 5: Absorb main, gate, and record the readout

**Files:**
- Modify: `docs/superpowers/specs/2026-07-31-the-vernacular-design.md` (§7 gets
  its measured readout)

**Interfaces:**
- Consumes: everything above.
- Produces: the readout stages 3–5 build on.

- [ ] **Step 1: Absorb main**

```bash
make preflight
```

Expected: GO. On an ancestry NO-GO, `git merge main` into the branch and re-run
`make preflight` here. Do **not** absorb if main's checkout shows another
session mid-landing — preflight peeks and warns.

- [ ] **Step 2: Run the commit gate**

Run: `make gate 2>&1 | tee /tmp/hv-vern-gate.txt`
Expected: PASS. Budget ~15 minutes (`make gate` measured at 934.5 s on a quiet
Mac). Run it on a quiet box and do not start a second gate in parallel — a
single `make gate` already saturates ten cores at `cpu_ratio` 8.25–8.50.

- [ ] **Step 3: Write the measured readout into the spec**

Replace §7's prediction table's "after Stage 2" column with the measured
values, and add one line naming the commit each was measured at. If any number
is non-zero, **write it down as the result** — the spec says a falsified
prediction ships as the headline, and tuning the gate to rescue the prediction
is exactly what decision 0016's preregistration exists to prevent.

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/specs/2026-07-31-the-vernacular-design.md
git commit -m "docs(the-vernacular): §7 readout — the reword moves N facts"
```

---

## Self-review

**Spec coverage.** §4 (the split) → Task 1. §5 stage 1 → Task 1; stage 2 →
Tasks 2 and 4. §7 (the preregistered instrument) → Task 3, readout in Task 5.
§8 risk 1 (`name-gloss-true` independence) → recorded in Task 2 step 3's doc
comment and in the followup register; **not solved here**, by design. §11
(the `words_identity.rs` doc drift) → swept in Task 2 step 3. §5 stages 3–5
are deliberately out of this plan's scope, stated under **Scope** above.

**Type consistency.** `Referent { concept: String, qualifiers: Vec<String> }`
with `Referent::of` / `Referent::qualified` is used identically in Tasks 1, 2,
3 and 4. `phenomenon_concept` keeps its exact signature in both copies
(Task 2), and `gloss_concept_of` is the public wrapper (Task 3).
`Referent`'s derive list grows by `PartialOrd, Ord` in Task 4 step 1, which is
called out there rather than assumed.

**Claims verified against the tree rather than reasoned about**, per the
campaign-autopilot verification rule: all 31 `Phenomenon {` construction sites
were enumerated with `grep -rn "Phenomenon {" --include=*.rs`; every concept
and qualifier key in Task 1 step 5's table was checked against the 182 keys in
`book/src/reference/concept-registry-generated.md`; `build_world`,
`observed_phenomena` and `sky_of` were confirmed public with the argument lists
used in Task 3; `phenomena_of` was confirmed **not** to exist, so the plan uses
`observed_phenomena` instead; and `cli/tests/words_identity.rs` was confirmed
absent, which is why Task 2 sweeps the comment citing it.

**One key is worth a second look during Task 1.** `move` is used as the
wandering-star qualifier because it is a registered concept (a verb-ish pack
entry). If it reads oddly in review, drop the qualifier rather than inventing
a key — an unregistered qualifier fails
`every_referent_key_is_registered` in Task 3, which is the intended behaviour.
