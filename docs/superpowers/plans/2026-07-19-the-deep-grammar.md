# The Deep Grammar (C7) Implementation Plan

> **COMPLETE (2026-07-19).** 3 tasks + review fixes; blast radius matched the measured landscape exactly; post-absorption re-pins honest (the Confluence moved seed 3). Marker-collision fidelity question presented at G6.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Evidential and noun-class morphology in the tongues — depth
drawn per species, morpheme forms family-cognate through the cascade,
animacy derived from the C5 schema posture, assembly at the segment level
— with the folk-witnessed vs priesthood-taught contrast visible in the
Book.

**Architecture:** `domains/language` gains the depth vector, the
family-morpheme draws + cascade evolution, segment-level affixation, and
the extended `realize_tongue`; worldgen derives the readouts
(exposure→evidential, day-schema→animacy, doctrine→taught) into a
`TongueMorphology`; `windows/book` renders the new lines and re-pins the
changed tongue strings in the same commit. Spec:
`docs/superpowers/specs/2026-07-19-the-deep-grammar-design.md`
(G3-approved).

**Tech Stack:** Rust edition 2024, std + serde only.

## Global Constraints

- **No epoch. Zero new facts/concepts/predicates.** New render-time
  labels only (declared in language's streams module; the
  stream-manifest reference page regenerates in the SAME task — the C5
  F1 rule): `language/<species>/grammar/depth/evidential`,
  `language/<species>/grammar/depth/noun-class`,
  `language/<species>/grammar/class-position`,
  `language/family/<family>/morph/evidential/<value>` (values:
  `witnessed`, `taught`, `inferred`),
  `language/family/<family>/morph/class/<value>` (values: `animate`,
  `inanimate`).
- **The blast radius is declared and G3-approved:** deep tongues'
  existing Book lines CHANGE. Every test pinning an exact tongue string
  (C3/C4 suites, the committed artifact) **re-pins in the same commit
  that changes it** — measure, then pin; never defer, never leave a
  stale pin for the close.
- **Census stability verified, not assumed** (spec law 7): lexicons are
  untouched (affix draws are new streams, never lexeme-space) — the
  surface task regenerates the language reference pages + runs
  `make census-check` and requires byte-identity/ok. NO new metrics, no
  AWS.
- **Zero authored surface text in any tongue:** every morpheme form is a
  drawn proto-form (the copula's syllable-fill precedent) evolved
  through the cascade. Tongues stay render-only (no parse obligation —
  the Echo addendum).
- The anti-astrology line: depth/position are DRAWN; evidential VALUES
  and animacy are DERIVED from shipped state (never judged fresh, never
  drawn fresh).
- Fallback/unreachable arms PANIC (the C5 F2 lesson); loud guard on the
  floor-unreachable Inferred.
- No HashMap/HashSet; missing_docs; type-audit tags at introduction
  (+ report regen if tags are added — the C6 T1 lesson); fmt every
  commit; live files are the authority; exact values measured then
  pinned.

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-deep-grammar`,
branch `the-deep-grammar`.

**Preregistered numbers (frozen before measurement):**

- Depth weights (drawn via `Stream::weighted_index` over `[None,
  Particle, Affix]`): evidential `[60, 25, 15]`; noun-class
  `[55, 15, 30]`. Common: fixed None, no draws.
- Class-marker position: `[prefix 40, suffix 60]`
  (`language/<species>/grammar/class-position`). Evidential position:
  FIXED predicate-final (suffix on the overt copula; enclitic on the
  predicate nominal for zero-copula tongues). Particle-depth: the marker
  stands as a free word immediately after the predicate (evidential) /
  on the drawn position side of the noun (class).
- Morpheme proto-forms: ONE syllable each, drawn per family per value by
  the `proto_root`-style syllable fill at the PROTO phonology, evolved
  per daughter via its existing cascade.
- Animacy base table (derived, no draw): `<species>-kind` concepts and
  `person` → Animate; everything else → Inanimate; OVERRIDE: `sun`,
  `moon`, `star`, `earth` → Animate iff the culture's C5 day-schema draw
  is `SchemaId::Agentive`.
- Evidential readout: folk tongue lines (self-statement, emic
  world-statement) → `Witnessed` (their grounding concepts are Steeped);
  the doctrine's in-tongue line → `Taught`; `Inferred` = floor-unreachable
  (loud-guarded, synthetically tested).

---

### Task 1: The morphology substrate (`domains/language`)

**Files:**
- Modify: `domains/language/src/grammar.rs` (extend `TongueGrammar` — its
  doc names C7 as the seam; the depth/position draw fns; the extended
  realizer) — if the file grows unwieldy, a `morphology.rs` sibling
  module is acceptable (your call, documented).
- Modify: `domains/language/src/lib.rs` (streams labels + re-exports)
- Modify: `book/src/reference/stream-manifest-generated.md` (regenerate,
  SAME task)
- Tests: in-file

**Interfaces (produced — exact names for T2/T3):**

```rust
pub enum Evidential { Witnessed, Inferred, Taught }
pub enum MorphDepth { None, Particle, Affix }
pub enum NounClass { Animate, Inanimate }
pub enum ClassPosition { Prefix, Suffix }
/// The drawn + derived morphology bundle the realizer consumes.
/// Forms are ROMAN-RENDERED daughter forms (evolved segments re-rendered);
/// the segment-level join happens inside the realizer via the segment
/// sequences carried alongside (see MorphForm).
pub struct MorphForm { pub segments: Vec<Segment>, pub roman: String }
pub struct TongueMorphology {
    pub evidential_depth: MorphDepth,
    pub noun_class_depth: MorphDepth,
    pub class_position: ClassPosition,
    pub evidential: BTreeMap<&'static str, MorphForm>, // "witnessed"/"taught"/"inferred"
    pub class: BTreeMap<&'static str, MorphForm>,      // "animate"/"inanimate"
}
/// Draw the species-level depth/position (the three species labels).
pub fn morph_depths(seed: &Seed, species: &str) -> (MorphDepth, MorphDepth, ClassPosition)
/// Draw the family proto-forms and evolve them into this daughter.
pub fn morph_forms(seed: &Seed, family: &str, proto: &Phonology, cascade: &Cascade, daughter: &Phonology) -> (BTreeMap<&'static str, MorphForm>, BTreeMap<&'static str, MorphForm>)
/// Segment-level affixation: join stem + affix segments (order per
/// position), re-render romanization via render_views — never string
/// concat across the boundary.
pub fn affix(stem: &[Segment], affix: &[Segment], position: ClassPosition) -> MorphForm
// TongueClause gains: pub evidential: Evidential
// realize_tongue gains a morphology parameter:
pub fn realize_tongue(clause: &TongueClause, grammar: &TongueGrammar, morph: &TongueMorphology, noun_class_of: &dyn Fn(&str) -> NounClass, lexicon: &Lexicon) -> Result<String, TongueGap>
```

(Verify every consumed signature live: `proto_root`'s shape for the
syllable fill, `evolve(proto, cascade, ph) -> Derivation` (which field
carries the evolved segments — read `Derivation`), `render_views(&[Segment])
-> GeneratedName` (which view is `.roman`), how `build_lexicon` obtains
proto phonology + cascade per daughter — REUSE that path's types. If
`realize_tongue`'s existing callers make the added parameters awkward,
a parallel `realize_tongue_deep` that the old fn forwards to (shallow
morphology = all None) is acceptable — old call sites keep compiling and
C3's shallow behavior is IDENTICAL by construction.)

- [ ] **Step 1: Failing tests** (fixture: a synthetic phonology pair or
  the goblin/hobgoblin family drawn as the lexicon tests do — copy their
  fixture idiom):

```rust
#[test] fn depth_draws_are_deterministic_and_weighted() {
    // same seed -> same triple twice; across 200 derived species-name
    // streams the None bucket is the mode for evidential (measure the
    // counts against [60,25,15] loosely: None > Affix).
}
#[test] fn morph_forms_are_family_cognate() {
    // the cognate law: draw forms for a family with two daughters whose
    // cascades differ; assert each daughter form == evolve(proto_form,
    // its cascade).phonetic-segments re-rendered (read Derivation's
    // field), and the two daughters' romans differ where the cascades
    // differ (measure a seed where they do; if every seed coincides,
    // panic demanding a different fixture — fallback arms fail loudly).
}
#[test] fn affix_is_segment_level_not_string_concat() {
    // the assembly law: affix(stem, marker, Suffix).roman ==
    // render_views(stem ++ marker).roman EXACTLY, and != naive
    // format!("{}{}", stem_roman, marker_roman) for at least one fixture
    // where romanization differs across the boundary (construct one: a
    // stem-final + affix-initial segment pair whose joint romanization
    // differs — find such a pair in the romanization rules; if NONE
    // exists in the current romanizer, assert equality on the segment
    // path and DOCUMENT that the naive path is currently
    // indistinguishable, with the structural assertion (the fn consumes
    // segments, not strings) as the guarantee).
}
#[test] fn realize_tongue_marks_by_depth() {
    // depth None -> the C3 surface BYTE-IDENTICAL (shallow identity —
    // pin it); Affix evidential -> the witnessed marker appears
    // predicate-finally; Particle -> free word after the predicate;
    // class Affix + Prefix -> marker precedes the complement noun.
    // Zero-copula + Affix evidential -> enclitic on the predicate
    // nominal (the fixed-position rule's zero-copula arm).
}
#[test] fn inferred_is_defined_and_loud() {
    // Inferred renders with its drawn form when passed explicitly
    // (synthetic); no floor path constructs it — the readout fns in T2
    // own that guard; here: exhaustive-match future-proofing only.
}
```

- [ ] **Step 2:** `cargo test -p hornvale-language 2>&1 | tail -5` → FAIL.
- [ ] **Step 3:** Implement; module doc extends grammar.rs's C7 note; the
  streams module gains ALL the new labels; regenerate the manifest page.
- [ ] **Step 4:** language green; worldgen/book COMPILE and stay green
  (the realize_tongue signature — if you used the parallel-fn option,
  zero call-site churn; otherwise fix call sites mechanically with
  shallow morphology so behavior is identical this task). fmt; workspace
  clippy; type-audit check (+ report regen if tags added).
- [ ] **Step 5:** Commit —
  `feat(language): the deep grammar substrate — depth vector, cognate morphemes, segment affixation (C7 T1)`

---

### Task 2: The readouts (`windows/worldgen`)

**Files:**
- Modify: `windows/worldgen/src/chorus.rs` (+ minimal lib.rs re-exports)
- Test: `windows/worldgen/tests/deep_grammar.rs`

**Interfaces:**
- Consumes: T1's types; the existing family/proto/cascade access
  (`language_of`, the `Daughter` path `build_lexicon` uses — find how
  worldgen assembles family/proto/cascade for `lexicon_of` and REUSE);
  the C5 day-schema draw (the same derive chain `explain_day` uses).
- Produces:

```rust
/// The species' full morphology bundle (draws + family forms), derived
/// the same way lexicon_of derives its lexicon — nothing persisted.
pub fn tongue_morphology_of(world: &World, species: &str) -> Result<hornvale_language::TongueMorphology, BuildError>
/// The C5 day-schema outcome for this culture (the SAME draw explain_day
/// resolves — factor the shared path, do not re-implement the draw).
pub fn day_schema_of(world: &World, species: &str) -> Option<hornvale_language::SchemaId>
/// The derived noun-class assignment (base table + the agentive
/// override) as a closure/fn the realizer consumes.
pub fn noun_class_of(world: &World, species: &str, concept: &str) -> hornvale_language::NounClass
```

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn the_coherence_law() {
    // seeds 1..=5, every placed culture: noun_class_of(.., "sun") is
    // Animate IFF day_schema_of == Some(Agentive); same for "moon"/
    // "earth"/"star"; "<kind>-kind" always Animate; a terrain concept
    // (e.g. "forest") always Inanimate. (Measure which cultures are
    // agentive — C5 measured PathJourney/CycleReturn/Balance at 1..=3,
    // Agentive exists in the wild (kobold seed 2 per C5's chronicle
    // example) — assert at least one Animate-sky culture across the
    // sweep, else panic demanding a wider sweep.)
}
#[test] fn day_schema_of_matches_the_explained_entry() {
    // For a culture whose day entry IS Explained (seed 1 goblin):
    // day_schema_of == Some(that entry's schema) — the accessor and the
    // explain pass resolve the SAME draw (no drift between the two
    // readers of one stream).
}
#[test] fn morphology_is_deterministic() { /* tongue_morphology_of twice -> identical Debug */ }
#[test] fn depth_landscape_measured() {
    // Sweep seeds 1..=3 x placed species: print + pin the drawn depth
    // triple per species (exact values — the blast-radius map for T3).
    // At least one species must draw a non-None depth somewhere in
    // 1..=3, else PANIC demanding the weights be revisited at G6 (the
    // Book must SHOW morphology somewhere in its three volumes).
}
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement (factor
  `explain_day`'s schema-draw resolution into the shared accessor).
- [ ] **Step 4:** worldgen + language + book green (book untouched);
  fmt; workspace clippy; type-audit.
- [ ] **Step 5:** Commit — `feat(worldgen): morphology readouts — depths, cognate forms, animacy coherence (C7 T2)`

---

### Task 3: The Book surface + re-pins + census verification

**Files:**
- Modify: `windows/book/src/lib.rs`, `cli/src/main.rs` (if the emitter
  needs it — check how tongue_lines/doctrine sections flow to cmd_book);
  regenerate `book/src/gallery/the-book.md`; re-pin every changed tongue
  string across the workspace's tests.

**Interfaces:**
- Consumes: T1/T2 everything; the existing tongue-section machinery
  (self-statements + probe inventory) and doctrine sections.

Rendering:
- Tongues section, per placed people: the self-statement (now through the
  deep realizer with `Evidential::Witnessed`) and the NEW emic
  world-statement (`TongueClause { subject: ⟨planet name⟩,
  complement_concept: "earth", evidential: Witnessed }`) with its Common
  gloss (`("Vebe is the earth.")`). The C3 planet-gap lines stay (the
  etic concept is still unheld).
- Doctrine sections (organized cultures): ONE in-tongue taught line —
  the same world-statement with `Evidential::Taught`, glossed
  `("Vebe is the earth — as it is taught.")`.
- The world-statement law joins the self-statement law: every placed
  people renders it (`earth` is universal-stratum Steeped) — assert, no
  gaps.

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn every_people_states_the_world_in_its_tongue() { /* the law; seeds 1..=3 */ }
#[test] fn the_taught_contrast_is_visible_where_deep() {
    // For every organized culture whose evidential_depth != None: the
    // doctrine's taught line's surface DIFFERS from the folk
    // world-statement (the morpheme contrast); for depth None cultures
    // the two surfaces are IDENTICAL (no marking) — both arms asserted
    // per T2's measured depth landscape (pin per-species which arm).
}
#[test] fn the_readout_law() {
    // No tongue line ever renders Inferred at the floor (walk all
    // rendered clauses — the construction sites pass only Witnessed/
    // Taught); the loud guard: constructing the readout for an
    // Explained{Lost}-grounded tongue line panics with the
    // author-a-surface message (drive synthetically).
}
#[test] fn shallow_species_lines_are_byte_identical_to_c3() {
    // For every species T2 measured depth (None, None, _): its
    // self-statement line is BYTE-IDENTICAL to the pre-C7 committed
    // string (copy the literal from the current artifact) — the shallow
    // identity guarantee.
}
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement + fix every
  now-stale pinned tongue string ACROSS the workspace (C3/C4 exact-string
  tests, doctrine section tests if a deep tongue's line changed) — each
  re-pin gets a provenance comment
  (`C7 deep grammar: re-pinned (was "...") — <species> drew depth <d>`).
- [ ] **Step 4:** Regenerate the artifact; diff shows tongue-section
  changes ONLY where T2's measured depth landscape predicts (verify
  line-by-line against the map; anything else = BLOCKED) + the new
  world-statement/taught lines; paste seed-1's tongue + doctrine
  sections verbatim.
- [ ] **Step 5: Census stability (spec law 7):** regenerate the language
  reference pages exactly as ci.yml does; `git diff` must show them
  byte-identical; `make census-check 2>&1 | tail -1` → ok. Then the full
  sweep: `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` +
  `git status --porcelain` empty (post-commit).
- [ ] **Step 6:** `make gate 2>&1 | tail -4` → fully green;
  `mdbook build book` clean.
- [ ] **Step 7:** Commit — `feat(book): the tongues go deep — witnessed and taught, re-pins in-commit (C7 T3)`

---

## Close checklist (closing-a-campaign owns these)

1. Absorb main + preflight. 2. NO census regen (verified stable). 3.
Chronicle + retro + freshness (language chapter; LANG-40 → shipped full;
LANG-43 Where note — substrate shipped, row stays raw). 4. G6 hard stop.

## Plan self-review (done at write time)

- Spec §3.1→T1 (depths), §3.2→T1 (forms) + T2 (assembly per species),
  §3.3→T2 (animacy), §3.4→T1 (affix), §3.5→T3, §4 laws→T1 (cognate,
  assembly, depth), T2 (coherence, readout-accessor), T3 (readout,
  world-statement, shallow identity, census stability), §5→global
  constraints + T3 steps 4–5.
- Fallback arms PANIC (cognate fixture, Animate-sky sweep, non-None depth
  landscape, the Inferred guard).
- Type consistency: Evidential/MorphDepth/NounClass/ClassPosition/
  MorphForm/TongueMorphology names match across tasks; the
  realize_tongue-vs-realize_tongue_deep option is delegated with the
  shallow-identity requirement pinned either way.
