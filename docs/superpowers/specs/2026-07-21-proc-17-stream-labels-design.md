# Stream labels stop being magic strings

**Status:** Approved at G3

**Campaign:** PROC-17 — every `Seed::derive` call site currently takes a
bare `&str`, so a typo'd inline literal is a silent, unrelated-but-valid
stream instead of a compile error. Closes the gap between the discipline
CLAUDE.md already documents (seed-derivation labels "declared as
constants in each crate's `streams` module") and what call sites actually
do.

---

## The problem, precisely

`Seed::derive(&self, label: &str) -> Seed` (`kernel/src/seed.rs`) is FNV-1a
over the label's raw bytes, then splitmix64 — pure, and completely blind
to whether `label` came from a declared constant or was typed inline.
Three different states exist in the codebase today, none of them safe:

1. **A crate has a `streams.rs`, and production code uses it correctly.**
   `domains/astronomy` is the only clean example — every real call site
   references `streams::ROOT` etc. Only its *tests* fall back to literals
   (14 occurrences across `starfield.rs`/`figures.rs`/`facts.rs`/
   `neighborhood.rs`/`tests/genesis_properties.rs`).
2. **A crate has a `streams.rs`, but coverage is partial.** `domains/terrain`
   declares 17 constants and production code uses several of them
   correctly (`streams::CRATONS` at `crust.rs:294`) — but chains a raw
   literal onto the *next* leg in the same call (`.derive(streams::CRATONS)
   .derive("lobing")`), because no constant for `"lobing"` exists yet.
   `"slice-0"`/`"slice-1"`/`"slice-2"` (used identically in both
   `crust.rs` and `render.rs`) and `"crenulation"` (`rift.rs`) are in the
   same state: real, permanent legs with no declared home. 9 production
   sites, 2 test sites (`"test-craton"`, self-evidently test-only).
3. **A crate has no centralization at all.** `domains/language`'s
   `stream_labels()` (in `lib.rs`, not a separate file) hand-documents
   *full path patterns* (`"language/<species>/phonology/inventory"`) as
   prose for the generated manifest — completely decoupled from the real
   per-leg `.derive("language").derive(species).derive("phonology")
   .derive("inventory")` chains built in `phonology.rs`, `grammar.rs`,
   `naming.rs`, `numeracy.rs`, `lexicon.rs`, `etymology.rs`,
   `morphology.rs`, `paradigm.rs` (69 production sites, the majority of
   this campaign's scope). `windows/worldgen` (`chorus.rs` + `lib.rs`, 26
   production sites) and `windows/chronicle` (`generate.rs`/`simulate.rs`/
   `measure.rs`, 7 production sites) have no `streams.rs` and are not even
   listed in `cli/src/streams.rs`'s manifest sources — their labels are
   entirely undocumented today, a second, related gap.

`domains/climate` needs no work: it draws nothing beyond
`WEATHER_PHASE`, already a single declared constant, already referenced
correctly. `kernel/src/seed.rs`'s own 6 occurrences are `derive()`'s own
unit tests (testing the primitive itself — inherently need literals) and
stay as `StreamLabel::dynamic(...)`-wrapped literals, not centralized.

## The type

```rust
// kernel/src/seed.rs
/// A seed-derivation leg. The only way to call `Seed::derive` — wraps a
/// string so a bare `&str`, and therefore a typo'd inline literal, cannot
/// silently compile where a leg was expected. Zero-cost: borrows, never
/// allocates (mirrors the Precise/Quantized typestate's own "compute-only,
/// zero-cost" newtype precedent).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StreamLabel<'a>(&'a str);

impl StreamLabel<'static> {
    /// A permanent, save-format-contract leg. Called ONLY inside a
    /// crate's own `streams.rs` — never at an ad hoc use site (the
    /// type-audit check below enforces this).
    pub const fn from_static(s: &'static str) -> Self {
        StreamLabel(s)
    }
}

impl<'a> StreamLabel<'a> {
    /// A runtime-computed leg — a species name, a settlement's cell id, a
    /// salt. Legitimately dynamic, never centralizable, and never
    /// confusable with a permanent label because the call site names it.
    pub fn dynamic(s: &'a str) -> Self {
        StreamLabel(s)
    }

    /// The wrapped string. `pub` because `stream_labels()` in every OTHER
    /// crate (not just `kernel`, where `StreamLabel` lives) needs it to
    /// build the `Vec<(&'static str, &'static str)>` the manifest reads —
    /// see "Scope" below: that return type does NOT change, only how each
    /// crate builds it changes.
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}
```

`Seed::derive(&self, label: &str) -> Seed` becomes `Seed::derive(&self,
label: StreamLabel<'_>) -> Seed`, reading `label.as_str()` internally
where it currently reads `label`. The hash computation is byte-for-byte
unchanged — this is a pure call-site migration, never a value change, so
every world stays byte-identical (see Determinism, below).

**`stream_labels()`'s own signature is unchanged** — every crate's
`stream_labels() -> Vec<(&'static str, &'static str)>` keeps returning
raw string pairs for the manifest (`cli/src/streams.rs`'s renderer never
needs to know `StreamLabel` exists at all); each crate's implementation
calls `.as_str()` on its own `StreamLabel` constants when building that
vec, e.g. `(streams::ROOT.as_str(), "root label for astronomy")`. The type
change is confined to the *producing* boundary (declaration + `derive()`
call sites) and never leaks into the *display* boundary.

## Scope

Every crate that draws anything gets a complete `streams.rs`:

- **Fix in place** (constants exist, some call sites don't use them):
  `domains/astronomy` (test-only, 14 sites), `domains/terrain` (9
  production + 2 test sites; 4 new constants needed — `LOBING`,
  `CRUST_SLICE_0/1/2` shared between `crust.rs` and `render.rs`,
  `RIFT_CRENULATION`).
- **Author from scratch** (no `streams.rs`, or a disconnected one):
  `domains/language` (69 production sites — the majority of this
  campaign; the existing `stream_labels()` full-pattern prose in `lib.rs`
  stays as documentation, unmodified, since restructuring it to compose
  from the new leg constants is [[PROC-18]], deliberately deferred),
  `windows/worldgen` (26 production sites, `chorus.rs` + `lib.rs`),
  `windows/chronicle` (7 production sites).
- **No work needed:** `domains/climate` (already clean).
- **Stays a literal, explicitly:** `kernel/src/seed.rs`'s own `derive()`
  unit tests (6 sites — testing the primitive itself), wrapped in
  `StreamLabel::dynamic(...)` rather than centralized, since a test
  literal used only to prove `derive` is deterministic is not a
  save-format contract.

**`windows/worldgen` and `windows/chronicle` also get added to
`cli/src/streams.rs`'s `sources` list** — today they're entirely absent
from the generated stream manifest (`book/src/reference/
stream-manifest-generated.md`), a documentation-completeness gap
independent of, but naturally closed alongside, this campaign.

## The type-audit check

The type itself stops a bare `&str`; it does not stop `StreamLabel::
from_static("typo")` written inline at a random call site instead of
referencing the real constant. `tools/type-audit` (already built for
exactly this shape of rule — AST-scoped static analysis over the
workspace) gets one new check: flag any `StreamLabel::from_static(<string
literal>)` call expression whose containing file is not named
`streams.rs`. `StreamLabel::dynamic(...)` calls are never flagged (a
runtime value, not a literal, is always legitimate there) — the check
looks specifically for a *literal* argument to `from_static`, wherever it
appears.

## Determinism and save-format

No stream's underlying string VALUE changes — every migration replaces
`.derive("literal")` with `.derive(streams::CONST)` where `CONST`'s value
is byte-identical to the literal it replaces. `Seed::derive`'s hash
computation is untouched. Every world generated before and after this
campaign is byte-identical; this is provable per-crate by running that
crate's own existing determinism/property tests unchanged (a divergence
would mean a copy-paste error in a constant's value, not this campaign's
own logic). No epoch bump, no new stream label VALUES — only new names for
existing values, and net-new names for the handful of legs (`"lobing"`,
`"slice-0/1/2"`, `"crenulation"`, and every language/worldgen/chronicle
leg) that never had one.

## Explicitly out of scope

- **[[PROC-18]]** — a `stream_label!` declaration macro unifying constant-
  declaration with `stream_labels()` manifest-registration, closing the
  language crate's own prose/code disconnect structurally rather than by
  discipline. Real, deliberately deferred — new macro infrastructure
  beyond this campaign's core ask.
- **A hierarchy-aware, path-dependent `StreamLabel` type** (typing which
  child legs are valid after a given `.derive()` call) — considered
  during the ideonomy pass and rejected as real over-engineering; Rust's
  type system doesn't support it cleanly without heavy machinery, and it
  fights this project's own "boring over clever" posture.

---

## Decisions (promoted from the scratch ledger)

1. **Full standardization, not a partial fix** — every crate onto the same
   per-leg-constant pattern, not just the ones that already have a
   `streams.rs`. Nathan's direct choice among a cheaper partial option.
2. **`StreamLabel<'a>(&'a str)`, zero-cost, static/dynamic split** — an
   ideonomy negation pass confirmed the static/dynamic distinction is
   real (collapsing it loses the visible line between a permanent
   contract leg and a legitimate runtime value) and rejected a
   hierarchy-aware path-dependent-type alternative as over-engineering.
3. **Test-code call sites are fixed in the same pass** — the type change
   forces every call site to migrate regardless of the original
   production/test risk distinction; the only remaining choice per site
   is "reference a real constant" vs. "wrap the literal in
   `StreamLabel::dynamic(...)` because it's genuinely one-off."
4. **The `stream_label!` macro is captured, not built** ([[PROC-18]]) — a
   real ideonomy-surfaced enrichment, but new infrastructure beyond this
   campaign's core ask.
