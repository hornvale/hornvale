# The Correspondence — Implementation Plan (Wave 0)

**Spec:** `docs/superpowers/specs/2026-07-18-the-correspondence-design.md`
**Branch/worktree:** `the-correspondence`
**Scope:** Wave 0 only — the mechanism + behavior-preserving migration. Wind
reconnection (Wave 1) and temperature/direction naming (Wave 2) are separate
follow-on campaigns (spec §6); Wave 2 waits on the parallel diurnal-temperature
campaign.

**Grounding correction (ledger #3):** the API map found (a) no
`compile_fail`/`trybuild` precedent — the house anti-vacuity pattern is the
exhaustive-destructure `#[allow(dead_code)]` tripwire (`biome.rs:160`); (b) the
lexeme edge is deliberately decoupled to `language` (packs + the worldgen
auto-gap loop `lib.rs:2026-2033`). Enforcement is therefore HYBRID: concept +
percept + cognition are compile-forced at the owner's registration call; the
lexeme edge is a declaration reconciled against the lexicon by a drift-check.

Real types (from the map): registrar is `ConceptRegistry`
(`kernel/src/registry.rs:85`); `register_concept(&mut self, name, domain,
kind, doc)` (`:161`); the `Domain::register_concepts(&self, &mut
ConceptRegistry) -> Result<(), RegistryError>` seam (`kernel/src/domain.rs:12`);
`DOMAINS` roster of 9 (`windows/worldgen/src/lib.rs:222`); lexeme output type
`LexEntry` (`domains/language/src/lexicon.rs:120`); percept type `Phenomenon`
(`kernel/src/phenomena.rs:28`), kinds string-keyed.

---

## Stage 1: The kernel types (additive, zero behavior change)
**Goal:** `Manifest`, `Correspondent`, `Void`, and the `Expected` lexeme marker
land in the kernel with the house tripwire + tests. Nothing calls them yet.
**Deliverable:** a new `kernel/src/manifest.rs` (re-exported from `lib.rs`):
- `pub struct Manifest { concept: ConceptDef, lexeme: Correspondent<Lexicalization, Void>, percept: Correspondent<PerceptKind, Void>, cognition: Correspondent<CognitiveHandle, Void> }` — no `Default`, all fields required.
- `pub enum Correspondent<T, V> { Present(T), Absent(V) }`.
- `pub enum Void { Unnamed(&'static str), Gap(&'static str), Imperceptible(&'static str), Uncognized { pending_wave: &'static str } }` — closed.
- Placeholder edge-payload types: `Lexicalization` (a declaration: `Expected` | a concept-key the language ledger must cover), `PerceptKind(String)` (references a registered phenomenon-kind key), `CognitiveHandle` (opaque, unit-ish for now — the whole column voids today).
- **Anti-vacuity tripwire (house pattern):** an `#[allow(dead_code)]` fn that destructures a `Manifest` with all fields named and no `..`, so adding a ledger edge (field) breaks compilation and forces every construction site to be revisited — the `biome.rs:160` shape applied to a struct.
- **PROC-13 exhibit (NEW house pattern, flagged):** a `///` doc-comment `compile_fail` doctest showing a `Manifest { concept, lexeme, percept }` literal (missing `cognition`) fails to compile. Zero-dep (rustdoc-native). First use of this pattern in the repo — noted in the module doc.
**Success criteria:** `cargo build -p hornvale-kernel` clean; the compile_fail
doctest is red-when-complete (mutation-check: temporarily add `..Default` proves
it flips); unit tests construct a `Manifest` with each `Void` variant. No world,
almanac, or artifact byte changes (nothing wired).
**Tests:** `cargo test -p hornvale-kernel manifest`; `cargo test -p hornvale-kernel --doc`.
**Status:** Complete — `kernel/src/manifest.rs`; 5 unit tests + the compile_fail
exhibit green; type-audit clean (report regenerated); fmt/clippy clean. No
registration wired (Stage 2).

## Stage 2: The choke-point + climate pilot (byte-identity proof)
**Goal:** the registration API forces a Manifest, proven behavior-preserving on
one real domain before the full roll.
**Deliverable:**
- `ConceptRegistry::register_manifest(&mut self, Manifest) -> Result<(), RegistryError>` — decomposes into the existing conflict-checked concept insert (and validates the percept edge references a registered phenomenon kind). Reuses the decision-0025 idempotency/conflict shape verbatim.
- Migrate **climate only** (`domains/climate/src/lib.rs:38-67`): `ice`/`snow`/`rain` + the biome-class concepts register through `register_manifest`, each declaring honest edges (lexeme `Expected` where a pack covers it / `Gap` otherwise; percept `Present(AMBIENT)` or `Imperceptible`; cognition `Uncognized{pending_wave:"wave-cognition"}`).
- Leave `register_concept` public but `#[deprecated]` for the duration of the migration (removed in Stage 3).
**Success criteria:** seed-42 world + the three almanacs + elevation map +
registry/manifest dumps + lab studies are **byte-identical** to pre-change
(`git diff --exit-code` on the committed artifacts after regen). Climate's
concepts still register with identical `ConceptDef`s.
**Tests:** `cargo test -p hornvale-climate`; the artifact drift-check
(`.github/workflows/ci.yml` "Artifacts are current" command list) run locally;
`make gate-fast` scoped to kernel+climate+worldgen.
**Status:** Complete — `register_manifest` (reuses decision-0025 conflict shape;
`#[serde(skip)] manifests` field keeps saves byte-identical); climate migrated;
**byte-identity independently verified** (regenerate-artifacts.sh + `git diff
--exit-code book/` → exit 0). 6 `register_manifest` tests incl. the
same-ConceptDef guarantee; type-audit/clippy/fmt clean. `register_concept`
`#[deprecated]`; remaining domains carry `#[allow(deprecated)]` until Stage 3.

## Stage 3: Full migration + close the public path
**Goal:** every concept registration across all 9 domains flows through
`register_manifest`; the incomplete state is unrepresentable.
**Deliverable:**
- Migrate astronomy, terrain, settlement, species, religion, language (the concept-registering domains; culture/paleoclimate register no concepts) to `register_manifest`. Each concept declares its edges with honest voids.
- Remove `register_concept` from the public API (or make it `pub(crate)`), so `register_manifest` is the only public path — the API-level construction proof.
- The `language` domain's own concepts (`wind`, `water`, …) carry the real `Lexicalization` (they ARE the lexeme owner); borrowed concepts carry `Expected`/`Gap` declarations.
**Success criteria:** byte-identity preserved across ALL committed artifacts;
`cargo build --workspace` clean; the only public registration path requires a
Manifest (grep proves no `register_concept` call sites remain).
**Tests:** full `make gate` (fmt + clippy + nextest + doctests); artifact
drift-check; the existing roster/order tests
(`domains_roster_registers_language_after_its_lenders`) still green.
**Status:** Complete — all 6 domains migrated to `register_manifest`; public
`register_concept` DELETED (private `insert_concept` helper is the shared insert;
`register_manifest` is the sole public path — the API-level construction proof).
Byte-identity independently verified (regen + `git diff --exit-code` = 0, dump
unchanged); `cargo test --workspace` green; roster/architecture/docs_consistency
green; clippy/type-audit/fmt clean.

## Stage 4: The manifestation view + reconciliation + the book
**Goal:** the folded UNI-28 view over all Manifests, with the lexeme
reconciliation drift-check and the trial-balance that foots.
**Deliverable:**
- `hornvale concepts --manifest` — the five-ledger coverage table (each concept × each edge: covered / typed-void), name-ordered, deterministic. A reference book page, drift-checked like `concepts`/`streams`.
- **Reconciliation test:** every concept whose Manifest declares lexeme `Expected` has an actual pack/lexicon realization; every silent auto-gap is now an explicit declared `Void::Gap`. The trial-balance: covered + Σ(void-class) per ledger == registered-concept count (PROC-11 shape); a concept in no ledger column fails.
- **Architecture test:** no central phenomenon enum exists; adding a concept in a fixture domain edits no other domain (the constitutional invariant, made a test) — extends `cli/tests/architecture.rs`.
- The void-list rendered as the backlog (the `Uncognized` column IS the cognitive-layer roadmap).
**Success criteria:** the view renders and drift-checks; the trial-balance
foots; the reconciliation test passes; the architecture test passes.
**Tests:** `cargo test -p hornvale --test docs_consistency` (new book page links);
`cargo test -p hornvale --test architecture`; the manifest-view drift-check;
full `make gate`.
**Status:** Not Started

---

## Definition of Done (this campaign)
- All four stages complete; `make gate` green; artifacts byte-identical (Wave 0 is behavior-preserving).
- Chronicle entry (`book/src/chronicle/`) + freshness sweep (decision 0013).
- Confidence Gradient re-score if a bet moved (PROC-5).
- Retrospective (`docs/retrospectives/`, decision 0020).
- Registry: flip PROC-16 `spec'd → shipped`; cross-link a decision if the
  void-grammar/verdict-vocabulary call (spec §5.2) is ratified.
- G6 digest to Nathan: leads with the save-format-adjacent registration
  boundary and the ledger-#3 hybrid-enforcement refinement.

## Deferred to follow-on campaigns (spec §6)
- **Wave 1** — reconnect `wind` lexeme ↔ climate wind vectors (net-new wiring; no code link exists today).
- **Wave 2** — name temperature + compass directions (waits on the diurnal-temperature data campaign).
- Filling cognition (the whole `Uncognized` column) — the campaigns the void-list scopes.
