# The Chorus (C4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Per-culture emic accounts + sparse etic margins in The Book, derived
through the four-filter epistemic stack, with the LANG-41
distinctiveness × recoverability dial as census-visible Lab metrics and a
preregistered known-groups falsification test.

**Architecture:** Pure filter mechanism in `domains/language/src/account.rs`
(surface-free `Account`); per-culture `AccountParams` derived at the
composition root (`windows/worldgen/src/chorus.rs`, the `voice_params`
pattern); Common surface + margin in `windows/book`; dial metrics in
`windows/lab` reading accounts through worldgen. Spec:
`docs/superpowers/specs/2026-07-18-the-chorus-design.md` (G3-approved).

**Tech Stack:** Rust edition 2024, std + serde only. No new crates.

## Global Constraints

- **Zero new seeded draws, zero new concepts, zero new predicates, zero new
  facts.** Genesis must stay byte-identical for every seed (no epoch). If a
  task seems to need a draw or a concept, STOP and escalate.
- **Knowledge-state is derived, never stored** (LANG-36): no per-culture
  knowledge table anywhere; every param is a pure function of world + authored
  vectors.
- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec`; `total_cmp` for float
  sorts. No wall-clock. `#![warn(missing_docs)]` — every pub item, field, and
  variant gets a one-line doc comment. New pub-boundary primitives carry
  `type-audit:` tags (ratios → `bare-ok(ratio)`, prose text →
  `bare-ok(prose)`, identifier text → `bare-ok(identifier-text)`, flags →
  `bare-ok(flag)`).
- Domain isolation: `domains/language` imports **only** `hornvale_kernel`.
  Cross-domain predicate names appear only in `windows/worldgen`/`windows/book`.
- The live file is the authority: verify every signature you consume against
  the checked-out code before writing calls (C3's inclusive-bounds lesson).
- Every Common sentence the chorus emits must round-trip through the book's
  parser (the Echo corpus law). Scaffolding (headings, glosses) is exempt.
- Run `cargo fmt` before every commit. Iterate cost-ordered: fmt + clippy →
  scoped tests (`-p <crate>`) → full gate only at task end where stated.
- Censuses NEVER run locally. `studies/the-chorus.study.json` (50 seeds) is
  NOT a census and runs live; `studies/the-census.study.json` is AWS-only.

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-chorus`, branch
`the-chorus`. Commit after every task.

---

### Task 1: Render inventory + planet-probe promotion (C3's inheritance)

The per-tongue coverage report is currently hand-enumerated (exactly
{self-statement, planet-probe}) and the planet probe's `Ok` arm is silently
discarded (`windows/book/src/lib.rs:277-283` — only `Err` pushes a gap).
Derive the report from a probe inventory and land `Ok` realizations in
`tongue_lines`.

**Files:**
- Modify: `windows/book/src/lib.rs` (the C3 tongue section inside
  `render_volume`, currently lines ~240–284)
- Tests: in-file `#[cfg(test)] mod tests`

**Interfaces:**
- Produces: `pub struct TongueProbe { pub concept: String, pub subject: String }`,
  `pub fn tongue_probes(world: &World) -> Vec<TongueProbe>`, and
  `fn probe_tongue(probe, kind, grammar, lexicon) -> Result<String, hornvale_language::TongueGap>`
  (private; the pure per-probe runner the tests drive with a synthetic lexicon).
  `BookVolume.tongue_lines` / `.tongue_gaps` keep their exact current strings
  for today's worlds (no artifact drift from this task).

- [ ] **Step 1: Write the failing tests** (append to the tests module):

```rust
/// C4 T1: the coverage report is DERIVED — the probe inventory contains one
/// entry per committed `is-a` complement concept (today: `planet` only), so
/// a future renderable `is-a` kind auto-enters the report.
#[test]
fn tongue_probes_derive_from_committed_is_a_facts() {
    let world = generated(1);
    let probes = tongue_probes(&world);
    assert_eq!(probes.len(), 1, "seed 1 commits exactly one is-a fact");
    assert_eq!(probes[0].concept, "planet");
    assert_eq!(probes[0].subject, "Vebe");
}

/// C4 T1: the probe's SUCCESS path lands the realized line instead of
/// silently vanishing — driven with a synthetic lexicon that Steeps
/// `planet`, since no real culture holds it (mutation evidence: assert the
/// realized text, not just Ok-ness).
#[test]
fn probe_success_path_yields_a_line() {
    use hornvale_language::{ExposureClass, build_lexicon};
    let world = generated(1);
    let ph = hornvale_worldgen::language_of(&world, "goblin");
    let grammar = hornvale_language::tongue_grammar(&world.seed, "goblin", &ph);
    let mut exposures = std::collections::BTreeMap::new();
    exposures.insert("planet".to_string(), ExposureClass::Steeped);
    let lexicon = build_lexicon(&world.seed, "goblin", &ph, &exposures);
    let probe = TongueProbe { concept: "planet".to_string(), subject: "Vebe".to_string() };
    let line = probe_tongue(&probe, "goblin", &grammar, &lexicon)
        .expect("a Steeped concept realizes");
    assert!(!line.is_empty() && line.ends_with('.'), "a realized sentence: {line}");
    assert!(line.contains("Vebe"), "the probe subject appears: {line}");
}

/// C4 T1: the derived report reproduces C3's exact strings on seeds 1–3 —
/// no regression, no artifact drift from the derivation.
#[test]
fn derived_report_matches_the_shipped_strings() {
    let world = generated(1);
    let vol = render_volume(&world);
    assert!(vol.tongue_gaps.iter().any(
        |g| g == "goblin: gap — planet (no entry in this lexicon)"),
        "the derived gap line is byte-identical to C3's: {:?}", vol.tongue_gaps);
}
```

NOTE: `build_lexicon`'s exact signature and the gap type's name must be
verified against `domains/language/src/lexicon.rs` (the live file is the
authority) — adjust the test's calls, never the meaning. The realize error
type is whatever `realize_tongue` returns today (`lib.rs` re-exports; C3
calls it `gap` with fields `concept` and `reason`).

- [ ] **Step 2: Run them to verify they fail** —
`cargo test -p hornvale-book 2>&1 | tail -20` → the three new tests FAIL
(missing `TongueProbe`/`tongue_probes`/`probe_tongue`).

- [ ] **Step 3: Implement.** In `windows/book/src/lib.rs`:

```rust
/// One entry in the tongue render inventory: a concept some committed fact
/// asks every tongue to state, about a named subject. The inventory is
/// DERIVED from the ledger (C4 T1) — one probe per committed `is-a`
/// complement — so a future renderable kind auto-enters the coverage
/// report instead of waiting on a hand-list.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: subject)
pub struct TongueProbe {
    /// The concept the tongue is asked to state (an `is-a` complement).
    pub concept: String,
    /// The subject's surface name (the committed `name`, or the C3
    /// fallback text).
    pub subject: String,
}

/// The derived probe inventory: one probe per committed `is-a` fact,
/// ledger order.
pub fn tongue_probes(world: &World) -> Vec<TongueProbe> {
    let mut probes = Vec::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        let Value::Text(kind) = &fact.object else { continue };
        let subject = world
            .ledger
            .text_of(fact.subject, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", fact.subject.0));
        probes.push(TongueProbe { concept: kind.clone(), subject });
    }
    probes
}

/// Run one probe against one tongue: realize `⟨subject⟩ ⟨copula?⟩
/// ⟨concept⟩` — `Ok` is a rendered line (the success path C3 dropped),
/// `Err` the recountable gap.
fn probe_tongue(
    probe: &TongueProbe,
    _kind: &str,
    grammar: &hornvale_language::TongueGrammar,
    lexicon: &hornvale_language::Lexicon,
) -> Result<String, /* realize_tongue's error type, verified live */> {
    realize_tongue(
        &TongueClause {
            subject: probe.subject.clone(),
            complement_concept: probe.concept.clone(),
        },
        grammar,
        lexicon,
    )
}
```

Then rewrite `render_volume`'s C3 planet-probe block (~lines 277–283) to
iterate `tongue_probes(&world)` per people: `Ok(line)` →
`tongue_lines.push(format!("{line} (in the {kind} tongue: \"{subject} is a {concept}.\")"))`
— NOTE: build the gloss from the probe's own fields exactly as shown, and
keep the existing `Err` arm's gap string byte-identical
(`"{kind}: gap — {concept} ({reason})"`). The self-statement block above it
is untouched (its law is separate). Delete the now-unused hand-built
`planet_statement` (keep `planet_name` — the probes reuse it via
`tongue_probes`; if the borrow is awkward, `tongue_probes` recomputes the
name itself as shown and `planet_name`/`planet_subject` can be dropped —
whichever leaves the file cleaner, with the existing gap strings unchanged).

- [ ] **Step 4: Verify** —
`cargo test -p hornvale-book 2>&1 | tail -20` → all pass (including every
pre-existing C3 law test, unchanged).
`cargo fmt && cargo clippy -p hornvale-book --all-targets -- -D warnings`.

- [ ] **Step 5: Commit** — `git add -A && git commit -m "feat(book): derive the tongue coverage report from a probe inventory (C4 T1)"`

---

### Task 2: The account mechanism (`domains/language/src/account.rs`)

The campaign's core: the four-filter stack as one pure, surface-free
function, plus the dial's distance measures. Language imports only the
kernel; nothing here names another domain's predicates — the caller supplies
the observability table.

**Files:**
- Create: `domains/language/src/account.rs`
- Modify: `domains/language/src/lib.rs` (add `pub mod account;` and re-export
  the public names alongside the existing `clause`/`grammar` re-exports —
  match the file's existing re-export style)
- Tests: in-file `#[cfg(test)] mod tests`

**Interfaces (produced — later tasks call these EXACT names):**

```rust
pub struct GroundFact { pub subject: String, pub predicate: String, pub object: hornvale_kernel::Value }
pub enum NeededConcept { Object, ObjectKind, Fixed(&'static str) }
pub enum Requirement { Manifest, SkyGraded { threshold: f64 }, Instrumental, CrossReferential, Taxonomic }
pub struct Observability { pub requirement: Requirement, pub domain: &'static str, pub concept: NeededConcept }
pub enum Stance { Ourselves, Neighbors, Rivals, Strangers, Neutral }
pub enum OrderPolicy { Ground, Salience { sky_first: bool } }
pub enum LossReason { NoConcept, BeyondCapability { domain: &'static str } }
pub enum Disposition { Kept, Lost(LossReason), Substituted { truth: String, theirs: String } }
pub struct AccountEntry { pub fact: GroundFact, pub disposition: Disposition, pub stance: Stance }
pub struct Account { pub entries: Vec<AccountEntry> }
pub struct AccountParams {
    pub hold_all: bool,
    pub holdings: std::collections::BTreeSet<String>,
    pub observability: std::collections::BTreeMap<String, Observability>,
    pub sky_capability: f64,
    pub order: OrderPolicy,
    pub stances: std::collections::BTreeMap<String, Stance>,
    pub world_carving: Option<String>,
}
pub fn account_of(ground: &[GroundFact], params: &AccountParams) -> Account
pub fn identity_params() -> AccountParams
pub fn distortion(account: &Account) -> f64
pub fn domain_distortion(account: &Account, params: &AccountParams, domain: &str) -> f64
pub fn distinctiveness(a: &Account, b: &Account) -> f64
pub fn recoverability(account: &Account) -> f64
```

(`pathological_params` is Task 3's — it needs the observability table, which
is worldgen's. `identity_params` is table-free: `hold_all: true`, empty
maps, `sky_capability: 1.0`, `OrderPolicy::Ground`, no carving.)

**Filter semantics — implement `account_of` in exactly this order (LANG-36):**

For each ground fact, look up `observability.get(&fact.predicate)`
(a predicate absent from the table under `hold_all == false` is
`Lost(BeyondCapability { domain: "unknown" })` — fail closed; under
`hold_all == true` every fact is `Kept` regardless, the null filter):

1. **Lexicon:** `needed` = the concept per `NeededConcept` (`Object` = the
   object's text; `ObjectKind` = `format!("{}-kind", text)`; `Fixed(c)` = c).
   Held = `hold_all || holdings.contains(needed)`. A non-held,
   non-`Taxonomic` fact → `Lost(NoConcept)`.
2. **Knowledge:** `Manifest` → pass. `SkyGraded { threshold }` → pass iff
   `sky_capability >= threshold`, else `Lost(BeyondCapability { domain })`.
   `Instrumental` / `CrossReferential` → always
   `Lost(BeyondCapability { domain })` at the floor. `Taxonomic` → fall
   through to the ontology step (the classification as stated is beyond
   floor capability regardless of holdings).
3. **Ontology:** `Taxonomic` + `world_carving = Some(c)` →
   `Substituted { truth: object-text, theirs: c }`; `Taxonomic` + `None` →
   `Lost(NoConcept)`.
4. **Valence:** `stance` = for `ObjectKind` facts, `stances.get(object-text)`
   (default `Neutral`); everything else `Neutral`.

**Ordering (the salience half of ontology):** `OrderPolicy::Ground` keeps
input order. `Salience { sky_first: true }` stably partitions entries with a
table `domain == "sky"` first (then the rest, both halves in input order);
`sky_first: false` the reverse partition. Stable partition only — no sorting.

**Measures (each doc-commented with its formula):**

- `distortion` = mean of three components over `n = entries.len()`
  (0.0 for an empty account): loss fraction (`Lost` + `Substituted`)/n;
  order distance = inversions between entry order and ground (input) order ÷
  n(n−1)/2 (0 when n < 2) — track each entry's original index to count;
  stance fraction = non-`Neutral` stances / n. Null params → 0 exactly.
- `domain_distortion(account, params, d)` = over entries whose predicate's
  table row has `domain == d`: (`Lost`+`Substituted`)/count;
  `f64::NAN`-free: return 0.0 when no entry matches (callers gate on
  presence).
- `distinctiveness(a, b)` (accounts over the SAME ground list, asserted by
  `debug_assert_eq!` on lengths) = mean of: disposition disagreement
  fraction (compare variant + `theirs` where both `Substituted`); order
  distance between the two permutations of original indices (normalized
  inversions of one order read in the other's sequence); stance
  disagreement fraction.
- `recoverability` = recovered/n: `Kept` → recovered; `Substituted` →
  recovered iff its `theirs` is unique among the account's `Substituted`
  `theirs` values (injectivity — a collision destroys the inverse);
  `Lost` → not. 1.0 for an empty account.

- [ ] **Step 1: Write the failing tests** (same file, `#[cfg(test)]`).
Build a small fixture — NO world builds in this crate (language is
kernel-only): a 5-fact ground list
(`is-a`/planet, `moon-count`/2.0, `star-class`/"F", `day-length-std`/1.5,
`instance-of`/"goblin") and a hand-built observability table mirroring the
one worldgen will author (Task 3's table, copied here as test fixture —
the two are pinned against each other by Task 5's integration tests):

```rust
fn fixture_ground() -> Vec<GroundFact> { /* the 5 facts above, subjects "Vebe"/"Vavako" */ }
fn fixture_table() -> BTreeMap<String, Observability> {
    // is-a → Taxonomic/"sky"/Object; moon-count → SkyGraded{0.6}/"sky"/Fixed("moon");
    // star-class → Instrumental/"sky"/Fixed("star");
    // day-length-std → CrossReferential/"sky"/Fixed("sun");
    // instance-of → Manifest/"peoples"/ObjectKind
}

#[test] fn null_filter_keeps_everything_in_ground_order() {
    let acc = account_of(&fixture_ground(), &identity_params());
    assert!(acc.entries.iter().all(|e| matches!(e.disposition, Disposition::Kept)));
    assert_eq!(distortion(&acc), 0.0);
    assert_eq!(recoverability(&acc), 1.0);
}

#[test] fn sky_graded_facts_split_on_capability() {
    // params: holdings = {"moon","star","sun","earth","goblin-kind"}, carving Some("earth")
    // cap 0.5 → moon-count Lost(BeyondCapability{"sky"}); cap 0.9 → Kept.
}

#[test] fn taxonomic_substitutes_the_carving_or_loses() {
    // carving Some("earth") → is-a becomes Substituted{planet→earth};
    // carving None → Lost(NoConcept). Also: recoverability counts a UNIQUE
    // substitution as recovered; two facts substituted to the same target
    // are both unrecovered (injectivity).
}

#[test] fn instrumental_and_cross_referential_always_lose() { /* star-class + day-length Lost at cap 1.0 */ }

#[test] fn salience_order_partitions_sky_first_and_counts_inversions() {
    // sky_first=true → the 4 sky entries precede instance-of; ground order within.
    // sky_first=false → instance-of first; distortion's order component > 0.
}

#[test] fn stances_apply_only_to_object_kind_facts() {
    // stances {"goblin"→Rivals} → the instance-of entry carries Rivals, the
    // sky entries Neutral; distortion's stance component = 1/5 · 1/3 share.
}

#[test] fn strangers_stance_is_reachable() {
    // stances {"goblin"→Strangers} round-trips through the entry — the
    // roster never produces Strangers today (generator-coverage lesson:
    // exercise it here, not via worlds).
}

#[test] fn distinctiveness_is_zero_for_clones_and_positive_for_divergents() {
    // same params twice → 0.0; cap 0.5 vs 0.9 (moon disagrees) +
    // different stance → > 0.0; symmetric: d(a,b) == d(b,a).
}
```

Every test asserts exact dispositions/values (measure, don't narrate — no
`assert!(result.is_ok())`-only tests).

- [ ] **Step 2: Verify they fail** — `cargo test -p hornvale-language account 2>&1 | tail -15` → FAIL (module missing).

- [ ] **Step 3: Implement** `account.rs` per the semantics above; module doc
comment states the four-filter order and the LANG-36 derived-not-stored
guard. Wire `pub mod account;` + re-exports in `lib.rs`.

- [ ] **Step 4: Verify** — `cargo test -p hornvale-language 2>&1 | tail -10` → all pass;
`cargo fmt && cargo clippy -p hornvale-language --all-targets -- -D warnings`.

- [ ] **Step 5: Commit** — `feat(language): the epistemic account — four-filter stack + dial measures (C4 T2)`

---

### Task 3: Param derivation at the composition root (`windows/worldgen/src/chorus.rs`)

Derive every culture's `AccountParams` from existing authored state — the
`voice_params` twin — plus the authored observability table and the ground
fact list. New submodule: `windows/worldgen/src/lib.rs` is merge-hot; touch
it only for `mod chorus;` + re-exports.

**Files:**
- Create: `windows/worldgen/src/chorus.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `mod chorus; pub use chorus::{...};`
  next to the existing module decls — verify placement against the live file)
- Test: `windows/worldgen/tests/chorus_params.rs`

**Interfaces:**
- Consumes (Task 2): everything under `hornvale_language::account`.
- Consumes (existing, verify live): `exposure_of(world, species)`,
  `placed_peoples(world)`, `hornvale_species::{perception_registry, psyche_registry, PerceptionVector, PsychVector, ActivityCycle, KindId}`,
  `hornvale_astronomy::facts::{MOON_COUNT, STAR_CLASS, DAY_LENGTH_STD}`,
  `hornvale_kernel::{world::IS_A, INSTANCE_OF, NAME}`.
- Produces:

```rust
pub fn observability_table() -> BTreeMap<String, Observability>
pub fn sky_capability(p: &hornvale_species::PerceptionVector) -> f64
pub fn account_params_of(world: &World, species: &str) -> Result<AccountParams, BuildError>
pub fn pathological_params() -> AccountParams
pub fn chorus_ground(world: &World) -> Vec<GroundFact>
pub fn accounts_of(world: &World) -> Vec<ChorusVoice>   // one per placed people, placed_peoples order
pub struct ChorusVoice { pub kind: String, pub params: AccountParams, pub account: Account }
```

**The authored observability table (build-state; the spec's §3.2 values):**

| predicate | requirement | domain | concept |
|---|---|---|---|
| `is-a` | `Taxonomic` | `"sky"` | `Object` |
| `moon-count` | `SkyGraded { threshold: 0.6 }` | `"sky"` | `Fixed("moon")` |
| `star-class` | `Instrumental` | `"sky"` | `Fixed("star")` |
| `day-length-std` | `CrossReferential` | `"sky"` | `Fixed("sun")` |
| `instance-of` | `Manifest` | `"peoples"` | `ObjectKind` |

Use the predicate constants, not string literals, for the astronomy three.

**Derivations (each a doc-commented pure function):**

- `sky_capability(p)` = `((p.night_vision + p.sky_attention) / 2.0 + bonus).min(1.0)`
  where bonus: `Nocturnal` 0.15, `Crepuscular` 0.08, `Diurnal` 0.0.
  Authored-value expectations (assert exactly in tests): goblin 0.5,
  hobgoblin 0.55, kobold 1.0, bugbear 0.65.
- Holdings: `exposure_of(world, species)?` → insert every concept classed
  `Steeped` or `KnowsOf` (a `KnowsOf` compound is still a held word).
- `order` = `Salience { sky_first: p.sky_attention >= 0.6 }` (kobold true,
  the other three false).
- Stances, from `psyche_registry()`: for each placed kind `k` ≠ self:
  `in_group_radius >= 0.5` → `Neighbors`; else `threat_response >= 0.5` →
  `Rivals`; else `Strangers`. Self kind → `Ourselves`. (Current roster:
  goblin says Neighbors, the other three say Rivals — Strangers is
  unreachable from authored data; Task 2 exercises it synthetically.)
- `world_carving` = `Some("earth".into())` iff holdings contain `"earth"`
  (the universal stratum → always at the floor; do NOT hardcode — read
  holdings).
- `hold_all: false`; `sky_capability` per above.
- `pathological_params()` (the gibberish pole, deterministic build-state):
  `hold_all: false`, empty holdings, the real table with every requirement
  rewritten to `Taxonomic` for text-valued predicates (`is-a`,
  `star-class`, `instance-of`) and `Instrumental` for numeric ones
  (`moon-count`, `day-length-std`), `sky_capability: 0.0`,
  `order: Salience { sky_first: true }`, every stance `Rivals`,
  `world_carving: Some("earth")` — so every text fact substitutes to the
  SAME target (injectivity collisions → recoverability ≈ 0) and numerics
  are lost.
- `chorus_ground(world)`: mirror `render_volume`'s fact selection exactly —
  for each `is-a` fact: the classification `GroundFact` (subject = resolved
  `NAME` or the `Entity {id}` fallback), then one `GroundFact` per
  `CONSTRUCTION_ORDER` predicate with a committed value on that subject
  (order: moon-count, star-class, day-length-std — copy the constant's
  order; the constant itself is `windows/book`'s and can't be imported
  here — write the three astronomy constants in that order and comment the
  mirror obligation; Task 4's null-filter law test pins the two against
  each other); then per `instance-of` fact: subject = the collective's
  resolved `NAME` (no "The " prefix), object = the kind text.
- `accounts_of(world)`: for each `placed_peoples(world)` kind:
  `account_of(&chorus_ground(world), &account_params_of(world, kind)?)`;
  skip kinds whose params error (mirror `render_volume`'s `else { continue }`
  posture).

- [ ] **Step 1: Write failing tests** (`windows/worldgen/tests/chorus_params.rs`;
build worlds with the existing `build_world(Seed(n), …, SkyChoice::Generated, …)`
pattern — copy the `generated(seed)` helper from a neighboring integration
test):

```rust
#[test] fn sky_capability_matches_the_authored_roster() { /* 0.5 / 0.55 / 1.0 / 0.65 exact */ }

#[test] fn goblin_params_derive_from_existing_state_only() {
    // seed 1: holdings contain "earth","moon","sun","star","goblin-kind",
    // "hobgoblin-kind"; carving Some("earth"); order Salience{sky_first:false};
    // stance toward "hobgoblin" == Neighbors, self == Ourselves.
}

#[test] fn hobgoblin_reads_rivals_where_goblin_reads_neighbors() { /* seed 1, the stance divergence */ }

#[test] fn kobold_keeps_the_moons_goblin_loses() {
    // seed 2 (kobold placed): kobold account's moon-count entry Kept;
    // goblin's Lost(BeyondCapability{"sky"}). star-class + day-length Lost
    // for BOTH; is-a Substituted{planet→earth} for both.
}

#[test] fn accounts_are_deterministic() { /* accounts_of(seed 1) twice → identical Debug strings */ }

#[test] fn pathological_pole_exercises_loss_and_collision() {
    // pathological on seed 2's ground: every entry Lost or Substituted;
    // ≥ 2 Substituted sharing "earth" (collision) → recoverability == 0.0;
    // distortion > shipped goblin's distortion > 0.0 (the ordering claim,
    // asserted here at world level ahead of Task 6's standing test).
}
```

- [ ] **Step 2: Verify they fail** — `cargo test -p hornvale-worldgen --test chorus_params 2>&1 | tail -15`.

- [ ] **Step 3: Implement** `chorus.rs` (+ the two lib.rs lines). Doc
comments state the TECH-1 seam on `sky_capability` ("the capability
derivation is a seam: a future tech ladder replaces this function without
touching the filter contract") and the derived-not-stored guard on
`account_params_of`.

- [ ] **Step 4: Verify** — `cargo test -p hornvale-worldgen --test chorus_params 2>&1 | tail -10`;
fmt + clippy for the crate.

- [ ] **Step 5: Commit** — `feat(worldgen): derived chorus params + observability table (C4 T3)`

---

### Task 4: The chorus surface in The Book (`windows/book` + CLI + artifact)

Emic paragraphs + etic margins per placed people, corpus-law bidirectional;
the null-filter law pins the chorus to the god's-eye volume.

**Files:**
- Modify: `windows/book/src/lib.rs` (new `chorus` field + rendering; extend
  `parse_context`; chorus line parse/rerender helpers)
- Modify: `cli/src/main.rs` `cmd_book` (emit the chorus sections)
- Modify: `scripts/regenerate-artifacts.sh` is NOT touched (line 55 already
  regenerates `the-book.md` through `hornvale book`)
- Regenerate: `book/src/gallery/the-book.md`
- Tests: in-file

**Interfaces:**
- Consumes (Task 3): `hornvale_worldgen::{accounts_of, ChorusVoice}`;
  (Task 2): `Disposition`, `Stance`, `OrderPolicy`, account measures not
  needed here.
- Produces:

```rust
pub struct ChorusSection { pub kind: String, pub heading: String, pub emic: Vec<String>, pub margin: Vec<String> }
// BookVolume gains: pub chorus: Vec<ChorusSection>
/// The per-voice renderer, factored PURE over (account + naming inputs) so
/// the null-filter law test can inject an identity account directly:
fn voice_section(kind: &str, autonym: &str, account: &Account, world: &World) -> ChorusSection
pub fn parse_chorus_line(line: &str, ctx: &ParseContext) -> Result<ParsedLine, LineError>  // strips stance/margin dress, delegates to parse_line
pub fn rerender_chorus_line(parsed: &ParsedLine, dress: &ChorusDress) -> String            // exact inverse; ChorusDress records what was stripped
```

(`voice_section` may be private with a `#[cfg(test)]`-reachable seam or
crate-public — the test drives IT, not `accounts_of`, for the null law.)

(Design freedom: `parse_chorus_line` may instead return
`(ParsedLine, ChorusDress)` — whatever makes the round-trip test exact.
`ChorusDress` is a small pub enum/struct recording `stance: Option<&'static str>`
and `in_truth: bool`.)

**Rendering rules (implement in a `fn chorus_sections(world) -> Vec<ChorusSection>`
called from `render_volume`):**

For each `ChorusVoice { kind, params, account }` from `accounts_of(world)`:

- `heading` = `format!("As the {autonym} tell it")` (autonym = the
  collective's committed `NAME`; skip the voice if the people has no
  committed collective — mirror the C3 `continue`).
- **Emic lines**, in `account.entries` order, grouped by subject exactly as
  the god's-eye renderer groups (one sentence per classification subject,
  fragments folded in):
  - The world subject (its `is-a` entry): complement = `theirs` with
    `Definiteness::Def` when `Substituted` (→ "Vebe is the earth"), or the
    truth kind with `Indef` when `Kept` (the identity case — must
    byte-match the god's-eye line). Fold in that subject's `Kept` fragment
    entries only, via the existing `fragment_for` + `assemble_trailing`
    (kept moon-count → "with one moon" etc.). `Lost` fragments simply
    don't appear.
  - Each people subject (`instance-of` entries): the god's-eye collective
    `ClauseSpec` (same `species_label` plural path), then the stance
    appositive at the book layer: strip the terminal `'.'`, append
    `" — {stance}."` where stance text is the closed table
    `Ourselves → "ourselves"`, `Neighbors → "neighbors"`,
    `Rivals → "rivals"`, `Strangers → "strangers"`; `Neutral` → no
    appositive (identity case byte-matches god's-eye).
  - Fresh referring-expression scope per section (`subject_for` with a
    section-local `seen` set): every account names its subjects itself.
- **Margin lines** (`ground_truth \ emic`): subjects owning at least one
  `Lost`/`Substituted` entry get one margin sentence carrying ONLY that
  content: the world subject → `format!("In truth, {line}")` where `line` =
  the god's-eye `ClauseSpec` (truth kind, `Indef`) + `fragment_for` over the
  subject's `Lost` fragments only (kept ones are NOT repeated — sparseness).
  At the floor people entries are always `Kept` → no people margin; leave a
  comment noting the carrier-clause assumption (the world classification is
  substituted at the floor, so the margin's carrier is always itself margin
  content).

**`parse_context` extension:** also insert every `Substituted` entry's
`theirs` (i.e. `"earth"`) into `complements`, by walking
`accounts_of(world)` — the closed complement set stays derived from the
world.

**`cmd_book`:** after the Tongues block, emit:

```text
### The Chorus

#### As the Vavako tell it

The Vavako are goblins — ourselves.
The Babako are hobgoblins — neighbors.
Vebe is the earth.

*In truth, Vebe is a planet with two moons, orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.*
```

(headings are scaffolding; emic lines verbatim; each margin line wrapped in
`*…*` — the second typographic register. The exact seed-1 values above are
ILLUSTRATIVE of shape; the test in Step 1 asserts the real derived strings
and the committed artifact is whatever `hornvale book` emits.)

- [ ] **Step 1: Write failing tests:**

```rust
/// C4 T4, the null-filter law (spec §4.1): the identity params reproduce
/// the god's-eye volume byte-identically — the gazetteer IS the chorus's
/// degenerate case.
#[test]
fn identity_chorus_reproduces_the_gods_eye_lines() {
    let world = generated(1);
    let vol = render_volume(&world);
    let ground = hornvale_worldgen::chorus_ground(&world);
    let account = hornvale_language::account::account_of(
        &ground, &hornvale_language::account::identity_params());
    let section = voice_section("goblin", "Vavako", &account, &world);
    assert_eq!(section.emic, vol.lines, "identity filters == the god's-eye volume");
    assert!(section.margin.is_empty(), "the null filter loses nothing — no margin");
}

/// C4 T4: seed 1's goblin section — exact derived strings (real committed
/// values, the C2 exact-string discipline).
#[test]
fn goblin_section_speaks_and_margins_seed_1() {
    let vol = render_volume(&generated(1));
    let goblin = vol.chorus.iter().find(|s| s.kind == "goblin").expect("goblin voice");
    assert_eq!(goblin.heading, "As the Vavako tell it");
    assert!(goblin.emic.contains(&"Vebe is the earth.".to_string()),
        "planet substituted to the carving: {:?}", goblin.emic);
    assert!(goblin.emic.contains(&"The Babako are hobgoblins — neighbors.".to_string()),
        "goblin stance: {:?}", goblin.emic);
    assert!(goblin.margin.iter().any(|m| m.starts_with("In truth, Vebe is a planet")
        && m.contains("two moons") && m.contains("yellow-white dwarf")),
        "the margin carries what the stack lost: {:?}", goblin.margin);
}

/// C4 T4: hobgoblin reads rivals where goblin reads neighbors (seed 1) —
/// the chorus DIFFERS beyond vocabulary within one world.
#[test]
fn seed_1_voices_disagree_on_stance() { /* hobgoblin section: "The Vavako are goblins — rivals." */ }

/// C4 T4: kobold keeps the moons goblin loses (seed 2) — knowledge
/// divergence surfaces: kobold's emic world line contains "with one moon",
/// goblin's does not; goblin's margin does.
#[test]
fn seed_2_kobold_sees_moons_goblin_margins_them() { /* … */ }

/// C4 T4, the margin law (spec §4.3): parse every emic + margin line;
/// the union of recovered facts ⊇ chorus_ground, per culture — nothing
/// silently vanishes (checked THROUGH the parser, not narrated).
#[test]
fn emic_union_margin_covers_ground_truth() { /* seeds 1..=3, every section */ }

/// C4 T4, the corpus law extended: every chorus emic + margin line
/// round-trips byte-identically through parse_chorus_line + rerender.
#[test]
fn every_chorus_line_round_trips() { /* seeds 1..=3; mirror every_book_line_round_trips */ }
```

- [ ] **Step 2: Verify they fail** — `cargo test -p hornvale-book 2>&1 | tail -20`.

- [ ] **Step 3: Implement** rendering + parsing + `cmd_book`. Margin-line
parsing: strip a leading `"In truth, "`; stance parsing: try each of the
four closed suffixes `" — ourselves."` etc. (restore `'.'` after strip).
The `*…*` italics wrapper is `cmd_book`'s (artifact dress, not a Book
line) — `ChorusSection.margin` holds the bare sentence.

- [ ] **Step 4: Verify + regenerate** —
`cargo test -p hornvale-book 2>&1 | tail -10`; then
`cargo run -p hornvale -- book > book/src/gallery/the-book.md` and eyeball
the three volumes' chorus sections (report the seed-1 section verbatim in
your task report); `git diff book/src/gallery/the-book.md` must show ONLY
added chorus content — any changed god's-eye/tongue line is a defect.
fmt + clippy for the workspace (`cargo clippy --workspace --all-targets -- -D warnings`).

- [ ] **Step 5: Commit** — `feat(book): the chorus — emic accounts + etic margins (C4 T4)`

---

### Task 5: The dial metrics (`windows/lab`, census-visible)

Six metrics over structured accounts. They enter the census schema (the
census runs `"metrics": "all"`); the authorized AWS regen happens at close,
not in this task.

**Files:**
- Modify: `windows/lab/src/metrics.rs` (six `Metric` entries appended to
  `registry()`, plus private helpers)
- Test: extend `windows/lab`'s existing test surface (in-file tests module —
  verify where neighboring metric tests live and follow)

**Interfaces:**
- Consumes (Tasks 2–3): `accounts_of`, `distortion`, `domain_distortion`,
  `distinctiveness`, `recoverability`, `ChorusVoice`.
- Produces (metric names — the census schema contract, PERMANENT once
  regenerated): `chorus-distortion`, `chorus-distinctiveness`,
  `chorus-recoverability`, `chorus-variance`, `chorus-param-spread`,
  `chorus-sky-calibration`.

All `Extractor::Full`. Definitions (each `doc` states the formula and its
Absent rule):

- `chorus-distortion`: mean `distortion` over voices; `Absent` if 0 voices.
- `chorus-distinctiveness`: mean pairwise `distinctiveness`; `Absent` < 2.
- `chorus-recoverability`: mean `recoverability`; `Absent` if 0.
- `chorus-variance`: population variance of per-voice `distortion`;
  `Absent` < 2. (The vacuity number — read against `chorus-param-spread`.)
- `chorus-param-spread`: mean pairwise `|Δ sky_capability|`; `Absent` < 2.
- `chorus-sky-calibration`: Kendall tau between per-voice `sky_capability`
  and per-voice `domain_distortion(…, "sky")` over strictly-comparable
  pairs (both differ); `Absent` if < 2 voices or no strict pair.
  Expected sign ≤ 0 (distortion falls as capability rises).

`SummaryKind::Numeric` bucket edges: distortion/distinctiveness/
recoverability `&[0.1, 0.25, 0.5, 0.75, 0.9]`; variance
`&[0.001, 0.01, 0.05, 0.1]`; spread `&[0.05, 0.1, 0.2, 0.4]`; calibration
`&[-0.5, 0.0, 0.5]`.

- [ ] **Step 1: Failing tests** — assert on generated seeds (follow the
file's existing test idiom for building views). Hand-derived expectations
(recompute from Task 3's dispositions before pinning; the arithmetic here
is the plan author's and the live dispositions are the authority):
**seed 1** (goblin cap 0.5, hobgoblin 0.55 — both lose every sky fact):
the five non-calibration metrics present; `chorus-sky-calibration` is
**`Absent`** — both sky distortions tie at 1.0, so no strictly-comparable
pair exists (this Absent IS the tie rule's test);
`chorus-distinctiveness > 0` (the stance asymmetry: Ourselves/Neighbors vs
Rivals/Ourselves — accounts differ in VALUE even where distortion magnitude
ties). **Seed 2** (goblin 0.5, hobgoblin 0.55, kobold 1.0): calibration
== **−1.0** exactly (strict pairs g–k and h–k both discordant: higher cap,
lower sky distortion; g–h tie excluded); `chorus-distinctiveness` strictly
greater than seed 1's (knowledge divergence on the moons stacks on
stance). **Empty-voice edge**: drive the metric helpers directly with an
empty/singleton voice list → `Absent` for the <2 metrics (don't hunt for a
0-people seed).

- [ ] **Step 2: Verify they fail** — `cargo test -p hornvale-lab chorus 2>&1 | tail -15`.

- [ ] **Step 3: Implement** the six entries + helpers.

- [ ] **Step 4: Verify** — `cargo test -p hornvale-lab 2>&1 | tail -10`
(the full lab suite — the schema/list-metrics tests may pin the registry;
update any fixture the new metrics legitimately extend, e.g. the
`lab list-metrics` reference dump if drift-checked:
`cargo run -p hornvale -- lab list-metrics` → compare against
`book/src/reference/` — regenerate per the ci.yml artifact list). fmt +
clippy.

- [ ] **Step 5: Commit** — `feat(lab): the LANG-41 dial — six chorus metrics (C4 T5)`

---

### Task 6: The study + the preregistered known-groups test (the bet)

**Files:**
- Create: `studies/the-chorus.study.json`
- Create: `windows/lab/tests/the_dial.rs`
- Modify: `scripts/regenerate-artifacts.sh` (one live `lab run` line — NOT
  in the census block)
- Modify: `.github/workflows/ci.yml` ONLY if its "Artifacts are current"
  step enumerates studies explicitly (verify; mirror the live mechanism)
- Generated: `book/src/laboratory/generated/the-chorus/` (whatever
  `lab run` publishes — commit it)

**The study (data, decision 0011):**

```json
{ "name": "the-chorus",
  "description": "The chorus dial (C4/LANG-41): distinctiveness × recoverability × calibration over 50 unselected worlds — the calibration readout behind the known-groups gate.",
  "seeds": { "from": 0, "count": 50 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": ["chorus-distortion", "chorus-distinctiveness", "chorus-recoverability", "chorus-variance", "chorus-param-spread", "chorus-sky-calibration"] }
```

(50 Full builds ≈ well under the local-runtime budget; the census stays the
only 1000-world instrument.)

**The known-groups standing test (`the_dial.rs`) — the §6 criteria,
preregistered; thresholds are FROZEN here before first measurement:**

```rust
//! The bet, preregistered (spec §6) — AS AMENDED after T3's partial
//! unblinding (decision ledger #13; spec §6 addendum): the original C1
//! ordered the poles on the distortion AGGREGATE, and T3 measured that
//! clause false at seeds 1–6 (a shipped voice's lossless salience reorder
//! outweighs the pathological pole's total loss — order divergence is
//! distinctiveness, not destruction). The amended criteria return to
//! LANG-41's named axes; no threshold was relaxed.
//! C1a loss-monotonicity: loss_fraction(null)=0 < loss_fraction(shipped)
//!    < loss_fraction(pathological)=1, every measured multi-people world.
//! C1b recoverability ordering: recoverability(null)=1;
//!    recoverability(pathological) <= 0.25.
//! C1c the null pole is uncanny: pairwise distinctiveness of null-filter
//!    voices == 0.0 exactly.
//! C2 band: shipped mean distinctiveness >= 0.05 on at least one measured
//!    multi-people world; per world, shipped recoverability >=
//!    pathological recoverability + 0.25.
//! C3 non-vacuity: mean pairwise DISTINCTIVENESS > 0 on every measured
//!    multi-people world. (Distinctiveness, not distortion-variance:
//!    two accounts can differ in stance VALUES while their distortion
//!    magnitudes tie — seed 1 does exactly this, honestly. Variance stays
//!    the census population number, read against param spread; it is not
//!    the standing criterion.)
//! A C1/C3 failure FALSIFIES the bet (report at close; taste fallback —
//! do NOT tune these thresholds to pass); a C2 failure alone is
//! calibration work on the derived strengths.
```

Measured seeds: `[1, 2, 3, 42]`, skipping worlds with < 2 placed peoples
for the multi-people criteria (assert at least TWO multi-people worlds
survive the skip, so the test can't go vacuous — the Concordance lesson).
For each survivor: build shipped voices via `accounts_of`; null account via
`identity_params`; pathological via `pathological_params` on the same
ground. Assert C1–C3 exactly as frozen above.

- [ ] **Step 1: Write `the_dial.rs`** (it fails only if the bet fails —
this is the one test written EXPECTING green; a red here is a campaign
finding, not a code bug: STOP and report, do not tune).
- [ ] **Step 2: Run it** — `cargo test -p hornvale-lab --test the_dial 2>&1 | tail -15`.
Record pass/fail per criterion in the task report VERBATIM.
- [ ] **Step 3: Add the study + regen line** (`run_release -p hornvale -- lab run studies/the-chorus.study.json`
immediately after the `the-book.md` line in `regenerate-artifacts.sh`; check
ci.yml's artifact step and mirror if it enumerates studies), then run it:
`cargo run --release -p hornvale -- lab run studies/the-chorus.study.json`
and commit the generated `book/src/laboratory/generated/the-chorus/`
artifacts. Report the summary tally line.
- [ ] **Step 4: Full local gate** — `make gate 2>&1 | tail -25` → green
  EXCEPT the three census-schema files (`calibration.rs`,
  `branches_family_calibration.rs`, `gathering_calibration.rs`), which are
  red by design until the close-time authorized AWS regen refreshes the
  census fixtures (ledger #14: T5's registry growth changes the census
  schema; `load_rows`' exact-header check is deliberately strict). Verify
  the failure list is EXACTLY those files' fixture-load errors — any other
  red is a real defect.
- [ ] **Step 5: Commit** — `feat(lab): the-chorus study + the preregistered known-groups gate (C4 T6)`

---

### Task 7: Docs — registry flips, Laboratory chapter, coverage sweep

**Files:**
- Modify: `book/src/frontier/idea-registry.md` — LANG-36 status
  `raw` → `spec'd`, LANG-41 `raw` → `spec'd`, each Where column gaining the
  spec link (follow the existing `spec'd` rows' link style; IDs permanent;
  run the docs drift-check).
- Create: the Laboratory chapter page for the-chorus following the newest
  `book/src/laboratory/study-0NN.md` pattern (next free number; short,
  book-altitude: what the dial measures, the three poles, how to read
  variance against spread) + its `SUMMARY.md` line.
- Verify: `cargo run -p hornvale -- book` output committed (T4 did);
  `uncovered_predicates` unchanged (chorus renders no new predicates).

- [ ] **Step 1:** Make the edits.
- [ ] **Step 2:** `cargo test -p hornvale --test docs_consistency 2>&1 | tail -5` → green;
`mdbook build book 2>&1 | tail -3` → clean.
- [ ] **Step 3:** `make gate 2>&1 | tail -15` → green.
- [ ] **Step 4: Commit** — `docs(the-chorus): registry flips + the dial's laboratory chapter (C4 T7)`

---

## Close checklist (NOT tasks — the closing-a-campaign skill owns these)

1. Absorb main (`make preflight`; merge main INTO the-chorus on NO-GO
   ancestry) — parallel campaigns are landing daily (The Correspondence).
2. **The authorized census regen** (Nathan approved at G3): absorb +
   preflight FIRST (the regen-races-the-epoch lesson), then
   `make regen-remote`. The re-pin touches the standing 4-file set +
   `make census-check` (golden-pins tripwire).
3. Chronicle entry + retrospective + freshness sweep (incl. the language
   chapter and Confidence Gradient re-score if the bet's resolution moves
   the LANG-41 row's checkability story) + LANG-36/41 → `shipped` (or the
   honest `dial falsified` variant) + promote the scratch ledger/followups
   (git-ignored — dies with the worktree).
4. G6 hard stop: post-G3 ledger digest to Nathan.

## Plan self-review (done at write time)

- Spec coverage: §3.1→T2, §3.2→T3, §3.3→T4, §3.4→T1, §3.5→T5, §4 laws →
  T2/T4/T6 tests, §6→T6, §8 flips→T7. Census regen + chronicle → close
  checklist (G3-authorized).
- Types consistent across tasks (Account/AccountParams/ChorusVoice/
  ChorusSection names match task-to-task).
- No placeholders; where a live signature must be verified, the task says
  so explicitly instead of guessing.
