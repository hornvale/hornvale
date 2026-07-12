# The Domain Trait & Registry Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Introduce a kernel `Domain` trait and a single worldgen roster so concept registration and the streams manifest iterate one list instead of two hand-maintained per-crate lists.

**Architecture:** The trait lives in `kernel` (every domain already depends on it). Each domain crate adds a zero-sized unit struct implementing the trait by delegating to its existing free functions. `worldgen` owns `pub const DOMAINS: &[&dyn Domain]`; `register_all` and the CLI streams manifest both iterate it. Genesis is untouched — it stays explicit composition-root wiring.

**Tech Stack:** Rust 2024, workspace crates, std-only (no new dependencies). Determinism gate: `cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`.

## Global Constraints

- **Dependencies:** `serde` + `serde_json` only, workspace-wide. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.**
- **type-audit:** primitives at a `pub` boundary carry a `type-audit:` verdict tag. The trait's `&'static str` returns are identifier/label text → `bare-ok(identifier-text)`.
- **Save-format contract:** the **concepts dump stays byte-identical**; the **streams manifest** gains exactly one section (`hornvale-paleoclimate`, `*(no seed-derivation streams)*`) as a deliberate, committed regeneration. No stream label is added, removed, or renamed.
- **Layering** (enforced by `cli/tests/architecture.rs`): `kernel → domains → windows → cli`. This plan adds no new dependency edges.
- **Work on a branch/worktree, never main.** main's CI is currently red for an unrelated cross-platform float issue; this work is orthogonal.

---

### Task 1: The `Domain` trait in the kernel

**Files:**
- Create: `kernel/src/domain.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod domain;` and re-export)
- Test: inline `#[cfg(test)]` module in `kernel/src/domain.rs`

**Interfaces:**
- Produces: `hornvale_kernel::Domain` — object-safe trait with `crate_name(&self) -> &'static str`, `register_concepts(&self, &mut ConceptRegistry) -> Result<(), RegistryError>`, and `stream_labels(&self) -> Vec<(&'static str, &'static str)>` (default empty).

- [ ] **Step 1: Write the failing test**

Add to a new `kernel/src/domain.rs`, below the (not-yet-written) trait:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::ConceptRegistry;

    struct Dummy;
    impl Domain for Dummy {
        fn crate_name(&self) -> &'static str {
            "dummy"
        }
        fn register_concepts(&self, _r: &mut ConceptRegistry) -> Result<(), RegistryError> {
            Ok(())
        }
        // stream_labels intentionally omitted — exercises the default.
    }

    #[test]
    fn default_stream_labels_is_empty() {
        assert!(Dummy.stream_labels().is_empty());
    }

    #[test]
    fn is_object_safe() {
        let d: &dyn Domain = &Dummy;
        assert_eq!(d.crate_name(), "dummy");
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel domain::`
Expected: FAIL to compile — `Domain` is not defined.

- [ ] **Step 3: Write minimal implementation**

At the top of `kernel/src/domain.rs`:

```rust
//! The `Domain` trait: a generative domain's declarative registration surface.
//!
//! The composition root aggregates these members uniformly across every
//! domain. Genesis is NOT here — its inputs are domain-specific composition
//! work (Constitution §2.6). Metrics and reference metadata are not here
//! either; future per-domain behaviors get their own traits, never new
//! `Domain` members.

use crate::registry::{ConceptRegistry, RegistryError};

/// A generative domain's declarative registration surface.
pub trait Domain {
    /// This domain's crate name — the streams-manifest key
    /// (e.g. `"hornvale-astronomy"`). Implementations return
    /// `env!("CARGO_PKG_NAME")` so the key is compiled from the crate itself.
    fn crate_name(&self) -> &'static str;

    /// Register this domain's predicates and concepts into the shared registry.
    fn register_concepts(&self, registry: &mut ConceptRegistry) -> Result<(), RegistryError>;

    /// This domain's seed-derivation stream labels (permanent save-format
    /// contracts). Empty for domains that draw no seed streams.
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        Vec::new()
    }
}
```

In `kernel/src/lib.rs`, add the module (alphabetical among `pub mod` lines, after `pub mod cast;`) and re-export (with the other `pub use` lines):

```rust
pub mod domain;
```
```rust
pub use domain::Domain;
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel domain::`
Expected: PASS (2 tests).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add kernel/src/domain.rs kernel/src/lib.rs
git commit -m "feat(kernel): add the Domain trait (A1 task 1)"
```

---

### Task 2: Implement `Domain` for all nine domains

Mechanical delegation — each ZST forwards to the domain's existing free functions, which stay in place. Compilation is the check (behavior is unchanged); Task 3's roster exercises every impl.

**Files (Modify, add ZST + impl near the existing `register_concepts`):**
- `domains/astronomy/src/lib.rs`, `domains/climate/src/lib.rs`, `domains/culture/src/lib.rs`, `domains/language/src/lib.rs`, `domains/paleoclimate/src/lib.rs`, `domains/religion/src/lib.rs`, `domains/settlement/src/lib.rs`, `domains/species/src/lib.rs`, `domains/terrain/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::Domain` (Task 1).
- Produces: `hornvale_astronomy::Astronomy`, `hornvale_climate::Climate`, `hornvale_culture::Culture`, `hornvale_language::Language`, `hornvale_paleoclimate::Paleoclimate`, `hornvale_religion::Religion`, `hornvale_settlement::Settlement`, `hornvale_species::Species`, `hornvale_terrain::Terrain` — unit structs implementing `Domain`.

- [ ] **Step 1: Add the eight stream-bearing impls**

Each of these domains has both `register_concepts` and `stream_labels` free functions in scope at its crate root. Add this block to each crate's `lib.rs` (replace `Astronomy`/`astronomy` with the crate's name — the ZST name and `crate::` paths shown are per-crate):

`domains/astronomy/src/lib.rs`:
```rust
/// Astronomy as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Astronomy;

impl hornvale_kernel::Domain for Astronomy {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}
```

Repeat verbatim (changing only the doc line, the `pub struct <Name>;`) for:
- `domains/climate/src/lib.rs` → `Climate`
- `domains/culture/src/lib.rs` → `Culture`
- `domains/language/src/lib.rs` → `Language`
- `domains/religion/src/lib.rs` → `Religion`
- `domains/settlement/src/lib.rs` → `Settlement`
- `domains/species/src/lib.rs` → `Species`
- `domains/terrain/src/lib.rs` → `Terrain`

(For `language` and `paleoclimate`, `crate::register_concepts` resolves through the existing `pub use` re-export at the crate root — `language` via `pub use packs::{… register_concepts}`, `paleoclimate` via `pub use facts::{genesis, register_concepts}`.)

- [ ] **Step 2: Add the paleoclimate impl (no streams → default)**

`domains/paleoclimate/src/lib.rs` — omit `stream_labels` to take the trait default:
```rust
/// Paleoclimate as a registrable unit for the composition-root roster.
/// It draws no seed streams, so it takes the empty `stream_labels` default.
/// type-audit: bare-ok(identifier-text: return)
pub struct Paleoclimate;

impl hornvale_kernel::Domain for Paleoclimate {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
}
```

- [ ] **Step 3: Verify the workspace compiles**

Run: `cargo build --workspace`
Expected: builds clean (no consumers yet; the impls are dead code until Task 3, which is fine — unit structs and trait impls do not trigger dead-code warnings).

- [ ] **Step 4: fmt + clippy + commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/*/src/lib.rs
git commit -m "feat(domains): implement kernel Domain for all nine domains (A1 task 2)"
```

---

### Task 3: The worldgen roster + `register_all` rewrite

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `DOMAINS`, rewrite `register_all` around line 136)
- Test: inline `#[cfg(test)]` in `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: the nine domain ZSTs (Task 2); `hornvale_kernel::Domain`.
- Produces: `hornvale_worldgen::DOMAINS: &[&dyn Domain]` — nine domains, alphabetical by crate name.

- [ ] **Step 1: Write the failing tests**

Add to the `#[cfg(test)]` module in `windows/worldgen/src/lib.rs`:

```rust
#[test]
fn domains_roster_crate_names_are_unique_and_nonempty() {
    let mut names: Vec<&str> = DOMAINS.iter().map(|d| d.crate_name()).collect();
    assert_eq!(names.len(), 9, "expected nine domains in the roster");
    assert!(names.iter().all(|n| !n.is_empty()));
    let before = names.len();
    names.sort_unstable();
    names.dedup();
    assert_eq!(names.len(), before, "duplicate crate_name in DOMAINS");
}

#[test]
fn domains_roster_is_alphabetical_by_crate_name() {
    let names: Vec<&str> = DOMAINS.iter().map(|d| d.crate_name()).collect();
    let mut sorted = names.clone();
    sorted.sort_unstable();
    assert_eq!(names, sorted, "DOMAINS must be alphabetical by crate name");
}

#[test]
fn register_all_via_roster_registers_name_gloss() {
    let mut registry = hornvale_kernel::ConceptRegistry::default();
    register_all(&mut registry).expect("register_all succeeds on a fresh registry");
    // NAME_GLOSS is registered as a predicate after the roster loop.
    assert!(registry.predicate(NAME_GLOSS).is_some());
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-worldgen domains_roster`
Expected: FAIL to compile — `DOMAINS` is not defined.

- [ ] **Step 3: Add the roster and rewrite `register_all`**

Ensure `Domain` is in worldgen's kernel import list (`use hornvale_kernel::{… Domain …};`). Add the roster just above `register_all`:

```rust
/// The composition root's domain roster — the single list every per-domain
/// aggregation iterates (registration, the streams manifest). Alphabetical by
/// crate name; adding a domain is one line here plus its `Domain` impl.
pub const DOMAINS: &[&dyn Domain] = &[
    &hornvale_astronomy::Astronomy,
    &hornvale_climate::Climate,
    &hornvale_culture::Culture,
    &hornvale_language::Language,
    &hornvale_paleoclimate::Paleoclimate,
    &hornvale_religion::Religion,
    &hornvale_settlement::Settlement,
    &hornvale_species::Species,
    &hornvale_terrain::Terrain,
];
```

Replace the body of `register_all` (currently the nine hardcoded calls + `NAME_GLOSS`) with:

```rust
/// Register every domain's concepts, plus the composition root's own.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    for domain in DOMAINS {
        domain.register_concepts(registry)?;
    }
    registry.register_predicate(
        NAME_GLOSS,
        true,
        "the glossed meaning of an entity's generated name",
    )?;
    Ok(())
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-worldgen domains_roster` then `cargo test -p hornvale-worldgen register_all_via_roster`
Expected: PASS.

- [ ] **Step 5: Verify no artifact drift on the concepts dump**

Run:
```bash
cargo run -p hornvale -- concepts > /tmp/hv-concepts-after.md
git diff --no-index book/src/reference/concept-registry-generated.md /tmp/hv-concepts-after.md || true
```
Expected: **no differences** (registration order is not observable — sorted emission). If there IS a diff, STOP — the byte-identity assumption is wrong and the roster order needs to match the old registration order instead; report before proceeding.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): DOMAINS roster; register_all iterates it (A1 task 3)"
```

---

### Task 4: CLI streams manifest iterates the roster (+ regenerate page)

**Files:**
- Modify: `cli/src/streams.rs` (rewrite `render_streams`; update the in-file test)
- Regenerate: `book/src/reference/stream-manifest-generated.md`

**Interfaces:**
- Consumes: `hornvale_worldgen::DOMAINS`, `hornvale_kernel::Domain`.

- [ ] **Step 1: Update the test to expect the paleoclimate section**

In `cli/src/streams.rs`'s `#[cfg(test)] mod tests`, extend `manifest_lists_every_crate_and_label`'s expected-substring list with:
```rust
            "### hornvale-paleoclimate",
            "*(no seed-derivation streams)*",
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale streams::`
Expected: FAIL — the current manifest omits paleoclimate.

- [ ] **Step 3: Rewrite `render_streams` to iterate `DOMAINS`**

Replace the hardcoded `sources` array and its loop with a roster walk. The new `render_streams` (header and kernel-section text unchanged, byte-for-byte):

```rust
//! Render the stream manifest: every seed-derivation label in the project.

use hornvale_kernel::Domain;

/// Render every registered crate's stream labels as the book's generated
/// reference page. Labels are permanent save-format contracts.
/// type-audit: bare-ok(artifact: return)
pub fn render_streams() -> String {
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->\n\n");
    doc.push_str(
        "Labels are permanent save-format contracts; regeneration uses epoch \
         suffixes (e.g. `settlement/name/v2`), never renames.\n\n",
    );
    for domain in hornvale_worldgen::DOMAINS {
        doc.push_str(&format!("### {}\n\n", domain.crate_name()));
        let labels = domain.stream_labels();
        if labels.is_empty() {
            doc.push_str("*(no seed-derivation streams)*\n\n");
            continue;
        }
        doc.push_str("| Label | Meaning |\n|---|---|\n");
        for (label, meaning) in labels {
            doc.push_str(&format!("| `{label}` | {meaning} |\n"));
        }
        doc.push('\n');
    }
    doc.push_str("### hornvale-kernel (internal)\n\n");
    doc.push_str("| Label | Meaning |\n|---|---|\n");
    doc.push_str("| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |\n");
    doc
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale streams::`
Expected: PASS (both `manifest_lists_every_crate_and_label` and `manifest_is_deterministic`).

- [ ] **Step 5: Regenerate the committed page and inspect the diff**

```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git diff book/src/reference/stream-manifest-generated.md
```
Expected: the **only** change is a new `### hornvale-paleoclimate` block with `*(no seed-derivation streams)*`, inserted between `hornvale-language` and `hornvale-religion`. If anything else changed, STOP and report.

- [ ] **Step 6: Run the full artifact drift check**

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: after staging the regenerated streams page, no *other* generated artifact differs. (Stage the streams page first: `git add book/src/reference/stream-manifest-generated.md`.)

- [ ] **Step 7: fmt + commit**

```bash
cargo fmt
git add cli/src/streams.rs book/src/reference/stream-manifest-generated.md
git commit -m "feat(cli): streams manifest iterates the DOMAINS roster; restore paleoclimate section (A1 task 4)"
```

---

### Task 5: Definition of Done — book + retrospective + full gate

**Files:**
- Create: `book/src/chronicle/the-domain-trait.md`
- Modify: `book/src/SUMMARY.md` (add the chronicle entry after Crust)
- Modify: `book/src/architecture/overview.md` (add an "Adding a domain" checklist)
- Create: `docs/retrospectives/the-domain-trait.md`

- [ ] **Step 1: Write the chronicle entry**

Create `book/src/chronicle/the-domain-trait.md` — a short entry (deliberate altitude: technical, comprehensible without the code). Cover: the two drifting per-domain lists it replaced; the `Domain` trait as the formalized template (declarative registration only, genesis stays composition wiring); `DOMAINS` as the score, `register_all`/manifest as extracted parts; the one deliberate artifact change (paleoclimate restored to the manifest); and the honest N→1-not-N→0 limitation. Reference the spec at `docs/superpowers/specs/2026-07-12-domain-trait-registry-design.md`.

- [ ] **Step 2: Link it in the ToC**

In `book/src/SUMMARY.md`, add after the Crust line (63/64):
```markdown
- [The Domain Trait](./chronicle/the-domain-trait.md)
```

- [ ] **Step 3: Add the "Adding a domain" checklist to the architecture overview**

In `book/src/architecture/overview.md`, add a short subsection:
```markdown
## Adding a domain

A new domain is two edits, neither of which touches an existing domain:

1. In the new crate: `pub fn register_concepts`, optional `pub fn stream_labels`,
   and `impl hornvale_kernel::Domain for <Name>` delegating to them
   (`crate_name` returns `env!("CARGO_PKG_NAME")`).
2. In `windows/worldgen/src/lib.rs`: add one `&<crate>::<Name>` line to `DOMAINS`,
   in alphabetical position.

`register_all` and the streams manifest pick it up automatically. Genesis
wiring (how the domain is generated and reconstructed) stays explicit in the
composition root — that is not registration, it is composition.
```

- [ ] **Step 4: Write the retrospective**

Create `docs/retrospectives/the-domain-trait.md` — one page of *process* lessons (not product): the value of the ideonomy passes (notation surfaced `env!(CARGO_PKG_NAME)` and the god-interface risk; reading the ground-truth artifact overturned the spec's "omit empty" rule); the discovery that a proposed refinement (error-wrap) didn't pay for itself once `RegistryError`'s shape was known; and the N→1 honesty.

- [ ] **Step 5: Run the full gate**

```bash
cargo test --workspace
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test -p hornvale --test docs_consistency
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: all green; no artifact drift.

- [ ] **Step 6: Commit**

```bash
git add book/src/chronicle/the-domain-trait.md book/src/SUMMARY.md book/src/architecture/overview.md docs/retrospectives/the-domain-trait.md
git commit -m "docs: chronicle, retrospective, and architecture checklist for the Domain trait (A1 task 5)"
```

---

## Notes for the executor

- **Determinism is unforgiving.** Task 3 step 5 and Task 4 steps 5–6 are the load-bearing checks. A non-empty concepts-dump diff means the roster order matters after all — stop and reorder `DOMAINS` to match the old `register_all` sequence rather than forcing it.
- **The free functions stay.** Do not delete any domain's `pub fn register_concepts` / `pub fn stream_labels`; the trait impls delegate to them and the per-domain unit tests still call them directly.
- **`env!("CARGO_PKG_NAME")`** yields the exact crate name (`hornvale-astronomy`, …), which is what the manifest headers and the roster tests expect.
