# The Handle Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every noun the world's prose names resolvable by `examine`, on
all four surfaces, and add the check that would have caught all four.

**Architecture:** A catalog entry becomes a `Noun { display, datum, words }` —
one display name the client shows, plus process-internal resolution words. Words
are the significant words of the *nameable* part of the name, which for most
entries is the whole name and for the room descriptor is the noun phrase the
generator already computes and currently discards. The wire shape
(`NounEntry { noun, datum }`) does not change.

**Tech Stack:** Rust edition 2024, no new dependencies, `cargo nextest`.

Spec: `docs/superpowers/specs/2026-08-06-the-handle-design.md`.

## Global Constraints

- **No new dependencies** (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, tests included.
- **No wall-clock time**; `std::time::Instant` is banned in test code too.
- **Quantize at emit only.**
- **`#![warn(missing_docs)]`** — every new pub item, field and variant documented.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag.** A field
  typed as a *newtype* does NOT take a tag — adding one is a stale-tag error.
- **`docs/audits/type-audit-report.md` regenerates in the same commit as any
  pub-boundary change** (the pre-commit hook fails otherwise; never `--no-verify`):
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- **`cargo fmt` last, before every commit.**
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`.
- **Determinism:** same seed → byte-identical world; no ledger predicate moves.
- **Commit messages via `git commit -F <file>`** — never a heredoc, never
  backticked prose through the shell (`PROC-commit-message-via-file`).
- **`vessel/session/v1` shape must not change.** Content may grow (new entries);
  `NounEntry`'s fields stay exactly `{ noun, datum }`. Additive content re-pins
  three fixtures; a *shape* change would be an epoch decision and is out of scope.

---

## File Structure

| file | responsibility | task |
|---|---|---|
| `windows/vessel/src/focalize.rs` | the `Noun` type, significant-word derivation, the prose catalog | 1, 2, 3 |
| `windows/vessel/src/session.rs` | `examine` resolution; the underworld arm; `lens_nouns` | 1, 4 |
| `windows/vessel/src/snapshot.rs` | `Noun` → `NounEntry` (drops words; wire unchanged) | 1 |
| `windows/locale/src/grammar.rs` | `render` returns its parts | 2 |
| `windows/locale/src/regime.rs` | `Regime.descriptor_noun` | 2 |
| `domains/astronomy/src/provider.rs` | per-body display phrases on `SkyReport` | 3 |
| `windows/vessel/tests/the_handle.rs` | the word-resolution gate | 5 |

---

### Task 1: `Noun`, resolution words, and the matcher

Fixes `examine forest`, `examine canopy`, `examine bugbear` — every entry whose
display name is *itself* the nameable phrase. The descriptor, sky and underworld
come in Tasks 2–4.

**Files:**
- Modify: `windows/vessel/src/focalize.rs` (the `Focalized` struct and `TemplateFocalizer::render`)
- Modify: `windows/vessel/src/session.rs` (`lens_nouns` ~:2168, `examine` ~:2186, and the snapshot builder ~:693)
- Modify: `windows/vessel/src/snapshot.rs` (the `NounEntry` mapping only — the struct itself does NOT change)
- Test: `windows/vessel/src/focalize.rs` test module

**Interfaces:**
- Produces: `pub struct Noun { pub display: String, pub datum: String, pub words: Vec<String> }` with `Noun::new(display: &str, nameable: &str, datum: &str) -> Noun`; `Focalized.nouns: Vec<Noun>`; `Session::lens_nouns() -> Result<Vec<Noun>, VesselError>`

- [ ] **Step 1: Write the failing tests**

In `windows/vessel/src/focalize.rs`'s test module:

```rust
#[test]
fn significant_words_skip_stopwords_and_short_words() {
    let n = Noun::new(
        "bugbear of Goodogododaga",
        "bugbear of Goodogododaga",
        "a bugbear.",
    );
    assert!(n.words.contains(&"bugbear".to_string()));
    assert!(n.words.contains(&"goodogododaga".to_string()));
    assert!(!n.words.contains(&"of".to_string()), "stopword: {:?}", n.words);
}

#[test]
fn a_noun_resolves_by_its_whole_name_and_by_each_significant_word() {
    let n = Noun::new(
        "tropical seasonal forest",
        "tropical seasonal forest",
        "warm.",
    );
    for w in ["tropical seasonal forest", "tropical", "seasonal", "forest"] {
        assert!(n.matches(w), "{w:?} must resolve: {:?}", n.words);
    }
    assert!(!n.matches("canopy"));
}

#[test]
fn the_nameable_part_can_differ_from_the_display_name() {
    // Task 2 needs this: the descriptor displays the whole phrase but only its
    // noun phrase is nameable.
    let n = Noun::new("a stream gully, shaded, in a hollow", "stream gully", "ground.");
    assert!(n.matches("stream"));
    assert!(n.matches("gully"));
    assert!(n.matches("a stream gully, shaded, in a hollow"), "the display name still resolves");
    assert!(!n.matches("shaded"), "a qualifier is not a noun");
}
```

- [ ] **Step 2: Record the BEHAVIOURAL red first, then the compile red**

A compile failure is not evidence that anything discriminates, and spec §7.1
requires an assertion-level red. The type does not exist yet, so the behavioural
red comes from the live surface rather than from a unit test — capture it before
touching any code:

```bash
printf 'examine forest\nexamine canopy\nexamine bugbear\nquit\n' > /tmp/hv-h1-before.txt
cargo run -p hornvale -- possess --seed 42 --script /tmp/hv-h1-before.txt | tee /tmp/hv-h1-before.log
```
Expected, and pasted into the commit body: three `You see no … here.` lines.
This is the defect, in the player's own terms, on the record.

Then: `cargo test -p hornvale-vessel --lib focalize`
Expected: FAIL to compile — `cannot find type Noun in this scope`.

- [ ] **Step 3: Write the type**

In `windows/vessel/src/focalize.rs`, above `Focalized`:

```rust
/// Words too small or too common to be a handle. Judgement, not a discovered
/// fact, and deliberately in one place so the judgement is visible.
const STOPWORDS: [&str; 14] = [
    "a", "an", "the", "of", "in", "on", "over", "under", "and", "by", "at",
    "with", "its", "into",
];

/// The shortest *derived* word that can be a handle. Three-letter words in
/// these phrases are adjectives and articles ("icy", "dry", "the"), not things,
/// so derivation starts at four.
///
/// A short **display name** is unaffected: `display` always enters `words`
/// verbatim, so the `sky` entry resolves as `sky` despite being three letters.
/// The floor governs what is split OUT of a phrase, never the phrase itself.
const MIN_WORD: usize = 4;

/// One examinable thing: what the prose called it, what `examine` prints, and
/// the words a player may type to reach it.
///
/// `display` is the single client-facing name — it is what
/// `snapshot::NounEntry` carries and what the chart legend shows. `words` is
/// **process-internal and never serialized**: putting aliases on the wire would
/// spray "forest, tropical, seasonal" into the browser client's legend beside
/// the real entry.
/// type-audit: bare-ok(identifier-text: display), bare-ok(prose: datum), bare-ok(identifier-text: words)
#[derive(Debug, Clone, PartialEq)]
pub struct Noun {
    /// The name the prose used, and the only one a client sees.
    pub display: String,
    /// What `examine` prints for it.
    pub datum: String,
    /// Lowercased words that resolve to this entry. Never serialized.
    pub words: Vec<String>,
}

impl Noun {
    /// Build an entry. `nameable` is the part of the name a player would say —
    /// for most entries the whole display name, but for a room descriptor only
    /// the noun phrase, because `a stream gully, shaded, in a hollow` is a
    /// sentence fragment and only `stream gully` is a thing.
    /// type-audit: bare-ok(identifier-text: display), bare-ok(identifier-text: nameable), bare-ok(prose: datum)
    pub fn new(display: &str, nameable: &str, datum: &str) -> Noun {
        let mut words: Vec<String> = vec![display.to_lowercase()];
        if !nameable.eq_ignore_ascii_case(display) {
            words.push(nameable.to_lowercase());
        }
        for w in nameable.split(|c: char| !c.is_alphanumeric()) {
            let w = w.to_lowercase();
            if w.chars().count() >= MIN_WORD && !STOPWORDS.contains(&w.as_str()) && !words.contains(&w)
            {
                words.push(w);
            }
        }
        Noun {
            display: display.to_string(),
            datum: datum.to_string(),
            words,
        }
    }

    /// Whether `wanted` (already trimmed) names this entry, case-insensitively.
    /// type-audit: bare-ok(identifier-text: wanted), bare-ok(flag: return)
    pub fn matches(&self, wanted: &str) -> bool {
        let w = wanted.trim().to_lowercase();
        self.words.iter().any(|x| *x == w)
    }
}
```

Note `MIN_WORD = 4` and the test's `"bugbear"`/`"forest"`/`"gully"` all pass it;
`"icy"` would not. If a test in Step 1 disagrees with this floor, the floor is
the thing to change — say so in the commit body rather than weakening the test.

- [ ] **Step 4: Change `Focalized` and its producer**

`Focalized.nouns` becomes `Vec<Noun>`. In `TemplateFocalizer::render`, replace
the `vec![(biome, …), (descriptor, …), (village, …), (sky_noun, …)]` literal
with `Noun::new` calls whose `display` and `nameable` are the same string for
all four (the descriptor's split arrives in Task 2):

```rust
        let nouns = vec![
            Noun::new(&biome, &biome, &format!(
                "{:.1} °C the year round, moisture {:.2}, {}.",
                v.locale.fields.temperature_c,
                v.locale.fields.moisture,
                height_phrase(v.locale.fields.height_asl_m)
            )),
            Noun::new(&descriptor, &descriptor, &format!(
                "The ground here: {} (strangeness {:.0}).",
                v.locale.regime.descriptor, v.locale.regime.strangeness
            )),
            Noun::new(&village, &village, &format!("{} souls call it home.", v.village.population)),
            Noun::new(&sky_noun, &sky_noun, &v.sky),
        ];
```

- [ ] **Step 5: Update the three consumers the compiler names**

`session.rs` `lens_nouns` (~:2168) returns `Vec<Noun>`; chart-legend entries
become `Noun::new(&e.noun, &e.noun, &e.datum)` — mechanically safe, because a
mark's name is a plain `<kind> of <place>` construction, not a qualified
descriptor. Dedup stays on `display`.

`session.rs` `examine` (~:2195) replaces the equality test:

```rust
        if let Some(n) = prose.iter().find(|n| n.matches(&wanted)) {
            return Turn::Out(n.datum.clone());
        }
```

`session.rs` snapshot builder (~:693) and `snapshot.rs` map `Noun → NounEntry`:

```rust
                nouns: focalized
                    .nouns
                    .into_iter()
                    .map(|n| NounEntry { noun: n.display, datum: n.datum })
                    .collect(),
```

**`NounEntry` itself does not change.** If you find yourself adding a field to
it, stop — that is a wire-shape change and out of scope.

- [ ] **Step 6: Run to verify green, then check the live surface**

```bash
cargo test -p hornvale-vessel --lib focalize
printf 'examine forest\nexamine canopy\nexamine bugbear\nquit\n' > /tmp/hv-h1.txt
cargo run -p hornvale -- possess --seed 42 --script /tmp/hv-h1.txt
```
Expected: all three answer. `examine stream`, `examine moon`, `examine rock`
still refuse — later tasks.

- [ ] **Step 7: Re-pin the fixtures and gate**

The catalog's *content* is unchanged in this task (same four entries, same
displays), so the fixtures should NOT move. Verify:
```bash
cargo nextest run -p hornvale-vessel 2>&1 | tee /tmp/hv-t1.txt
```
If `v1_bytes_are_pinned` or `the_client_fixtures_are_current` fails here, that
is a finding — report it rather than re-pinning, because this task was supposed
to be wire-neutral.

- [ ] **Step 8: Commit**

Subject: `feat(vessel): a noun carries the words that reach it`

---

### Task 2: The descriptor declares its noun phrase

Fixes `examine stream`.

**Files:**
- Modify: `windows/locale/src/grammar.rs:38-68` (`render`), and `derived_regime` at :15
- Modify: `windows/locale/src/regime.rs:109-118` (`Regime`)
- Modify: `windows/locale/src/lib.rs:557` (the placed-exotic call site)
- Modify: `windows/vessel/src/focalize.rs` (the descriptor entry)
- Test: `windows/locale/src/grammar.rs` test module

**Interfaces:**
- Consumes: `Noun::new(display, nameable, datum)` from Task 1
- Produces: `Regime.descriptor_noun: String` — the noun phrase inside `descriptor`

- [ ] **Step 1: Write the failing test**

In `windows/locale/src/grammar.rs`'s test module:

```rust
#[test]
fn render_reports_the_noun_phrase_it_composed() {
    // `render` already builds the descriptor as (variety + substrate_detail)
    // then qualifiers; the noun phrase is that first part, and a player names
    // it rather than the qualifiers.
    let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] };
    let (text, noun) = render(
        Negations {
            substrate: Substrate::Ordinary,
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::PlantAnimal,
            endemic: false,
        },
        micro0(),
        BiomeExpr::for_legacy(Biome::Desert),
        Seed(42),
        &addr,
    );
    assert!(!noun.is_empty(), "a descriptor always has a noun phrase");
    assert!(text.starts_with(&noun), "the noun phrase opens the descriptor: {text:?} / {noun:?}");
    assert!(!noun.contains(','), "qualifiers are not part of the noun phrase: {noun:?}");
}
```

`micro0()` (`grammar.rs` test module, ~:8) and
`BiomeExpr::for_legacy(Biome::Desert)` are the fixtures the neighbouring tests
in this module already use — neither `MicroField` nor `BiomeExpr` has a
`Default`, and this task does not add one for a test's convenience.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-locale --lib grammar`
Expected: FAIL to compile — `render` returns `String`, not a tuple.

- [ ] **Step 3: Return the parts**

`grammar.rs:58-68` becomes:

```rust
    let noun = [variety, substrate_detail]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(" ");
    let text = [noun.clone(), habitat, exotic]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    (text, noun)
```

and the signature's return type becomes `(String, String)`. Document the pair on
`render`: *"the rendered descriptor, and the noun phrase within it — the part a
player would name. Returned rather than re-derived because only this function
knows which clauses are the noun phrase and which are qualifiers; recovering it
by parsing the rendered string is what The Handle exists to avoid."*

- [ ] **Step 4: Carry it on `Regime`**

In `regime.rs`, after `descriptor`:

```rust
    /// The noun phrase within `descriptor` — the part a player would name.
    /// `descriptor` is the whole clause including qualifiers ("a stream gully,
    /// shaded, in a hollow"); this is "stream gully".
    pub descriptor_noun: String,
```

Populate it in `derived_regime` (grammar.rs:29) and at `lib.rs:557`, both of
which already call `render`.

`Regime` derives `Serialize` and rides in `locale/room/v2`, so this is an
**additive** schema change — no version bump (decision 0055), and it re-pins
`book/src/reference/locale-seed-42.json` plus the vessel fixtures in Task 5.

- [ ] **Step 5: Use it in the catalog**

In `focalize.rs`, the descriptor entry becomes:

```rust
            Noun::new(&descriptor, &v.locale.regime.descriptor_noun, &format!(
                "The ground here: {} (strangeness {:.0}).",
                v.locale.regime.descriptor, v.locale.regime.strangeness
            )),
```

- [ ] **Step 6: Verify**

```bash
cargo test -p hornvale-locale --lib grammar
cargo nextest run -p hornvale-locale -p hornvale-vessel 2>&1 | tee /tmp/hv-t2.txt
printf 'go n\nexamine stream\nquit\n' > /tmp/hv-h2.txt
cargo run -p hornvale -- possess --seed 42 --script /tmp/hv-h2.txt
```
The flagship room's descriptor is `buttressed canopy`, which has no comma — to
see the qualified case, the transcript walks one room first. If neither room
shows a comma-qualified descriptor, find one with
`cargo run -p hornvale -- locale --world <w> --sample 48 | grep ,` and use that
room id via `--room`.

- [ ] **Step 7: Commit** (regenerate the type-audit report; `Regime` gained a pub field)

Subject: `feat(locale): a descriptor reports the noun phrase inside it`

---

### Task 3: The sky's bodies become entries

Fixes `examine moon`.

**Files:**
- Modify: `domains/astronomy/src/lib.rs:504-509` (`SkyReport`)
- Modify: `domains/astronomy/src/provider.rs` (`sky_at_visibility`, ~:1480-1600) and `ConstantSun::sky_at` (~:513)
- Modify: `windows/vessel/src/vantage.rs` (carry the phrases onto `Vantage`)
- Modify: `windows/vessel/src/focalize.rs` (one entry per body)
- Test: `domains/astronomy/src/provider.rs` test module; `windows/vessel/src/focalize.rs`

**Interfaces:**
- Produces: `SkyReport.body_phrases: Vec<(String, String)>` — `(display noun, one-line datum)` per visible body, in the order the description names them

- [ ] **Step 1: Write the failing tests**

In astronomy's provider test module:

```rust
#[test]
fn a_night_sky_reports_each_body_as_its_own_phrase() {
    // Seed 42 at night names two moons in its description; each must also be
    // addressable on its own, because the vessel builds one examinable entry
    // per body and cannot parse them back out of the sentence.
    let sky = generated_sky();
    let report = sky.sky_at_visibility(WorldTime { day: 0.0 }, Visibility::CLEAR);
    assert!(!report.body_phrases.is_empty(), "a sky names something");
    for (noun, datum) in &report.body_phrases {
        assert!(!noun.is_empty() && !datum.is_empty());
        assert!(
            report.description.contains(noun.trim_start_matches("the ")),
            "the description must actually name {noun:?}: {}",
            report.description
        );
    }
}
```

`generated_sky()` stands for whatever the module's existing night-sky fixture is
actually called — find it with
`grep -nE "fn [a-z_]*sky|GeneratedSky" domains/astronomy/src/provider.rs`
restricted to the `mod tests` block, and use that name verbatim. Do **not** add
a new fixture, and do not build a world inside this test: the neighbouring tests
construct a sky directly and this one must cost the same.

The test also needs a night moment. `day: 0.0` is midnight on seed 42 (the
possession transcript at that day shows "Night. The vast moon…"), so the
daylight arm is not exercised here; the sun's own phrase is covered by the
`ConstantSun` arm in Step 3.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-astronomy --lib provider`
Expected: FAIL to compile — no field `body_phrases`.

- [ ] **Step 3: Add the field and populate it**

`SkyReport` gains:

```rust
    /// One `(noun, datum)` per body the description names, in description
    /// order — the sun, then each moon. `bodies` above is the machine-side
    /// list (`"moon 1"`); these are the words the prose actually used ("the
    /// vast moon"), which is what a reader can name.
    /// type-audit: bare-ok(identifier-text: body_phrases)
    pub body_phrases: Vec<(String, String)>,
```

In `sky_at_visibility`, the loop that builds each moon's clause already computes
`size_word(moon.angular_diameter_rel)` and the phase words. Push
`(format!("the {size} moon"), <that moon's own clause>)` as each clause is
built, and `("the sun", <the sun's clause>)` on the daylight arm. **Build the
phrase from the same values the sentence uses** — never re-derive, and never
split the finished sentence.

`ConstantSun::sky_at` gets `body_phrases: vec![("the sun".into(), "A golden sun hangs fixed at zenith.".into())]`.

Every other `SkyReport { … }` literal the compiler names gets an explicit
`body_phrases` — do not add `..Default::default()`.

- [ ] **Step 4: Carry it to the vessel and into the catalog**

`Vantage` currently holds `sky: String` (`vantage.rs:20`). Add
`sky_bodies: Vec<(String, String)>` beside it, filled from the report in
`vantage.rs`'s builder, and in `focalize.rs` append one entry per body after the
`sky` entry:

```rust
        for (noun, datum) in &v.sky_bodies {
            nouns.push(Noun::new(noun, noun, datum));
        }
```

`Noun::new`'s stopword pass drops "the", so `the vast moon` yields `vast` and
`moon`. Two moons both yielding `moon` is the expected collision, resolved by
first-in-order per spec §4 — assert that in the test below rather than
discovering it later.

- [ ] **Step 5: Write the vessel-side test**

```rust
#[test]
fn each_body_the_sky_names_is_examinable_and_moon_is_not_ambiguous_at_runtime() {
    let v = vantage_at(0.0);
    let f = TemplateFocalizer.render(&v);
    let moons: Vec<&Noun> = f.nouns.iter().filter(|n| n.matches("moon")).collect();
    assert!(!moons.is_empty(), "the night sky names at least one moon");
    // Deterministic priority: the first entry wins, and it is a MOON's datum,
    // not the whole sky report.
    let first = moons[0];
    assert!(first.datum.contains("moon"), "moon resolves to a moon: {:?}", first.datum);
    assert_ne!(first.datum, v.sky, "and not to the whole sky report");
}
```

- [ ] **Step 6: Verify live**

```bash
cargo nextest run -p hornvale-astronomy -p hornvale-vessel 2>&1 | tee /tmp/hv-t3.txt
printf 'examine moon\nexamine sky\nquit\n' > /tmp/hv-h3.txt
cargo run -p hornvale -- possess --seed 42 --script /tmp/hv-h3.txt
```
Expected: `examine moon` answers about a moon; `examine sky` still gives the
whole report.

- [ ] **Step 7: Commit** (regenerate the type-audit report; `SkyReport` gained a pub field)

Subject: `feat(astronomy): the sky names its bodies one at a time`

---

### Task 4: The underworld answers for its own rock

**Files:**
- Modify: `windows/vessel/src/session.rs:947` (the `examine` dispatch) and `:1188` (`describe_underground_here`)
- Test: `windows/vessel/src/session.rs` test module (in-crate, so `delve_at` is reachable)

**Interfaces:**
- Consumes: `Noun` from Task 1

- [ ] **Step 1: Reproduce the defect in-crate, RED first**

The controller could not reach a cave by walking (400 steps) and `delve_at` is
crate-private, so this is the first live confirmation. Use the module's existing
`find_cave_cell(&terrain, seed, true)` helper (`session.rs:3576`) and
`session.delve_at(cell, cave)`, exactly as
`lateral_movement_is_refused_underground` (~:3703) does.

```rust
#[test]
fn underground_examine_answers_for_the_rock_it_names() {
    let world = seam_world();
    let terrain = hornvale_worldgen::terrain_of(&world).unwrap();
    let (cell, cave) = find_cave_cell(&terrain, world.seed, true);
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let shown = match session.delve_at(cell, cave) {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("delve must not release"),
    };
    assert!(shown.starts_with("[underground]"), "not underground: {shown}");
    let reply = match session.handle("examine rock") {
        Turn::Out(t) => t,
        Turn::Released(_) => panic!("examine must not release"),
    };
    assert!(
        !reply.starts_with("You see no"),
        "the underworld names rock and then refuses it: {reply}"
    );
}
```

- [ ] **Step 2: Run and record the RED**

Run: `cargo test -p hornvale-vessel --lib underground_examine`
Expected: FAIL on the **assertion**, with `You see no rock here.` Paste that
message into the commit body — it is the first live evidence of this instance.

If instead it fails because `find_cave_cell` or `delve_at` has a different
signature than described, fix the call and re-run; do not weaken the assertion.

- [ ] **Step 3: Give the band its own catalog and dispatch arm**

Beside `describe_underground_here` (:1188), add:

```rust
    /// The underworld's examinable catalog. The band has its own because you
    /// cannot see the forest from inside the rock — resolving an underground
    /// `examine` against the surface locale's nouns is the defect this fixes.
    fn underground_nouns(&self) -> Vec<crate::focalize::Noun> {
        let chamber = self
            .underground
            .expect("guarded by self.underground.is_some() at the call site");
        let stratum = stratum_word(chamber.stratum);
        vec![
            crate::focalize::Noun::new(
                "the rock",
                "rock",
                &format!("The rock here is {stratum}."),
            ),
            crate::focalize::Noun::new(stratum, stratum, &format!("{stratum} — the rock of this chamber.")),
        ]
    }
```

and at :947, before the bare `"examine"` arm:

```rust
            "examine" if self.underground.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_underground(rest))
            }
```

with `examine_underground` mirroring `examine_chamber`'s shape: walk
`underground_nouns()`, return the first match's datum, else the byte-identical
`format!("You see no {noun} here.")` refusal the other two paths use.

- [ ] **Step 4: Verify**

Run: `cargo nextest run -p hornvale-vessel 2>&1 | tee /tmp/hv-t4.txt`
Expected: PASS, including the new test.

- [ ] **Step 5: Commit**

Subject: `fix(vessel): the underworld answers for the rock it names`

---

### Task 5: The gate, the fixtures, and the client

**Files:**
- Create: `windows/vessel/tests/the_handle.rs`
- Modify: the three committed fixtures, via `REBASELINE=1`
- Test: itself

- [ ] **Step 1: Write the gate**

```rust
//! The Handle's gate: every significant word of every catalog entry's display
//! name resolves. It fails 6-of-7 against pre-campaign code, which is why it
//! exists — all four defects this campaign fixed were words a player would
//! obviously type against a name the prose had just used.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

#[test]
fn every_significant_word_of_every_catalog_entry_resolves() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    // Two rooms, because one room is an anecdote.
    for step in 0..2 {
        if step > 0 {
            let _ = session.handle("go n");
        }
        let nouns = session.lens_nouns().expect("the lens has nouns");
        assert!(!nouns.is_empty(), "a room names something");
        for n in &nouns {
            for word in &n.words {
                let reply = match session.handle(&format!("examine {word}")) {
                    Turn::Out(t) => t,
                    Turn::Released(_) => panic!("examine must not release"),
                };
                assert!(
                    !reply.starts_with("You see no"),
                    "room {step}: entry {:?} declares the word {word:?}, and examine refuses it",
                    n.display
                );
            }
        }
    }
}
```

The gate reads `n.words` rather than re-deriving them, so it checks the
*declaration* against the *resolver* — the two things that drifted.

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-vessel --test the_handle`
Expected: PASS after Tasks 1–4. If it fails, the failure names the entry and the
word — that is a real gap, not a test to relax.

- [ ] **Step 3: Re-pin the three fixtures**

```bash
cargo nextest run -p hornvale-vessel 2>&1 | tee /tmp/hv-t5.txt
REBASELINE=1 cargo test -p hornvale-vessel --test session_snapshot
```

Then verify the change is additive, by parsing rather than by eye:

```bash
python3 - <<'PY'
import json, subprocess
for name in ['session-seed-42.json','snapshot-seed-42-walk.json','snapshot-seed-42-chamber.json']:
    p=f'windows/vessel/tests/fixtures/{name}'
    def load(t): return [json.loads(l) for l in t.strip().split('\n') if l.strip()]
    old=load(subprocess.run(['git','show',f'HEAD:{p}'],capture_output=True,text=True).stdout)
    new=load(open(p).read())
    ok = all('words' not in json.dumps(d) for d in new)
    print(name, 'docs', len(old), '->', len(new), '| no resolution words on the wire:', ok)
PY
```
**`words` must not appear anywhere in the fixtures.** If it does, `Noun` is
being serialized somewhere it should not be — fix that, do not accept the pin.

- [ ] **Step 4: Regenerate artifacts and run the client gate**

```bash
make rebaseline
git status --short
make vessel-check
```
`make gate` never runs the client checks and these fixtures feed the browser
client, so `vessel-check` is not optional here.

- [ ] **Step 5: Confirm no world byte moved**

Spec §8 claims no epoch and no save-format change. Verify it rather than
assuming — this campaign touched a domain (`astronomy`) and two windows, and
the claim is exactly the kind that goes unchecked:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-handle-world.json
python3 -c "
import json
d=json.load(open('/tmp/hv-handle-world.json'))
f=d['ledger']['facts'] if 'facts' in d.get('ledger',{}) else d['ledger']
print('facts:', len(f))
"
git diff --stat cli/tests/fixtures/
```
Expected: the keystone identity fixtures under `cli/tests/fixtures/` show **no
diff**. A moved world byte means a seeded draw changed, which nothing in this
campaign should do — stop and report it.

- [ ] **Step 6: Full gate**

Run: `make gate` with Bash `timeout: 3600000`.

- [ ] **Step 7: Commit**

Subject: `test(vessel): every word a name declares must resolve`

Body: quote the gate's own failure message from a deliberate mutation (revert
one `Noun::new`'s `nameable` to the display name and watch the gate fail), so
the commit records that the gate discriminates rather than merely passing.

---

## Close (not a task)

`closing-a-campaign` owns the chronicle, the retrospective, the Confidence
Gradient check, the registry flips (`LOC-examine-head-noun`,
`LOC-sky-is-one-noun`, `PROC-assert-both-directions` → `shipped`;
`LANG-examine-disambiguation` stays `raw`), and the G6 digest.

Freshness sweep must include any chapter describing `examine`'s resolution or
the noun catalog — `grep -rln "examine" book/src/chronicle/ book/src/*.md`.
