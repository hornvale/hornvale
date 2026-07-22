# PROC-18: `stream_labels!` — one authored site for a seed-derivation label

**Working campaign name:** The Single Saying.

**Status:** SHIPPED. Approved at G3 ("Looks great!"), fully executed and
whole-branch reviewed (Ready to merge: Yes). Chronicled as
[The Single Saying](../../../book/src/chronicle/the-single-saying.md).

**Worktree:** `~/.config/superpowers/worktrees/hornvale/proc-18`, branch `proc-18`, off `main` @835fc844.

## 1. Problem

Every crate that draws deterministic seed streams (`domains/astronomy`,
`domains/terrain`, `domains/climate`, `windows/worldgen`,
`windows/chronicle`, `windows/locale`, `windows/vessel`, `kernel` itself)
declares its `StreamLabel` constants in a `streams.rs` file, then
*separately* hand-lists each one again in a `stream_labels()` function that
feeds the generated stream manifest
(`book/src/reference/stream-manifest-generated.md`). These are two
authored sites for one fact, and they have desynced repeatedly:

- **Few and Many** and **The Residue** both hit variants of "a new stream
  label constant was added but never reached `stream_labels()`."
- **The Corrigendum** found The Consonance's own close had left a gap of
  the same shape.
- **`domains/astronomy`'s own `STAR_AGE` incident** (The Reckoning, cited
  directly in `streams.rs`'s own test doc comment): a constant was added to
  `streams.rs` but omitted from `stream_labels()`, silently under-documenting
  a frozen save-format contract. Astronomy's fix was to add a *third*
  hand-maintained list (`ALL_LABELS`, a `#[cfg(test)]`-only array) plus a
  test (`every_stream_label_constant_is_published_in_stream_labels`) that
  cross-checks it against `stream_labels()`. This works, but at the cost of
  a third authored site instead of fewer — and it only catches drift
  *within* astronomy, at test time, if someone remembers to update
  `ALL_LABELS` too.
- **`domains/terrain` has no such test, and has already drifted, right now,
  on `main`.** Verified directly: `streams.rs` declares 22 `pub const`
  `StreamLabel`s (`ROOT` + 21 legs); `stream_labels()` in `lib.rs` lists only
  17. Five real, currently-drawn constants — `LOBING`, `CRUST_SLICE_0`,
  `CRUST_SLICE_1`, `CRUST_SLICE_2`, `RIFT_CRENULATION` — are entirely
  absent from the generated manifest, with `make gate` green throughout.
  This is not a hypothetical failure mode; it is a live one this campaign's
  own migration will fix as a structural side effect (§5).

The desync is periodic, not one-shot (four confirmed instances now,
counting terrain's live gap), and the strongest fix astronomy has tried
(a hand-maintained cross-check list + test) still requires remembering to
touch three places instead of one. A per-site lint could flag "a raw
literal outside `streams.rs`" (that check already exists —
`tools/type-audit`'s stream-label check, added by PROC-17) but that check
verifies an orthogonal invariant: *where* a literal is written, never
*whether a declared constant's name reached the manifest vec*. No
plausible lint closes that gap without independently re-deriving the same
"list of every constant" a fused macro already has for free.

## 2. Goal

A `stream_labels!` declarative macro (`macro_rules!`; decision 0019 permits
these, bans `syn`/`quote` proc-macros) that emits, from **one authored
list**, both:

1. every `pub const NAME: StreamLabel<'static>` declaration, and
2. the crate's `pub fn stream_labels() -> Vec<(&'static str, &'static
   str)>` manifest function,

so the two can never drift apart — there is no longer a second list to
forget. This is enforced at `cargo check` time (a compile error, the
strongest and earliest possible check in this codebase), not by a runtime
test or a static-analysis lint that must remember to run.

## 3. Scope

**In scope:** every crate whose `streams.rs` is "flat" — each constant's
own value is a complete or crate-locally-composed manifest key with no
further composition needed beyond an optional single crate-wide root
prefix. This is every current `streams.rs`: `kernel`, `domains/astronomy`,
`domains/terrain`, `domains/climate`, `windows/worldgen`,
`windows/chronicle`, `windows/locale`, `windows/vessel`. Per PROC-17's own
precedent (a campaign that introduces a new safer pattern migrates the
*whole* workspace to it, not one demonstration crate), this campaign
converts every one of these.

**Out of scope:** `domains/language`'s `stream_labels()` (in `lib.rs`).
It hand-documents composed **multi-leg path patterns** —
`"language/<species>/phonology/inventory"` — where `<species>` is a
runtime-resolved placeholder standing in for many different concrete
per-world derivations, not a single constant's value. Unifying that with
its real per-leg `.derive()` chain (`ROOT` → dynamic species leg →
`PHONOLOGY` → `INVENTORY`) is a materially different, harder problem
(composing a *documentation pattern* across several constants and a
runtime placeholder, not registering one constant). It is explicitly
banked as a separate future idea, exactly as the PROC-18 registry row
itself already scoped it, and `domains/language`'s `stream_labels()` is
left untouched by this campaign.

## 4. The two authoring shapes found in the codebase

Research (reading every existing `streams.rs` plus astronomy's and
terrain's real `stream_labels()`) found two conventions coexist today:

- **Flat** (`kernel`, `domains/climate`, `windows/worldgen`,
  `windows/chronicle`, `windows/locale`, `windows/vessel` — six of eight
  crates, and every crate authored or touched since PROC-17): each
  constant's own literal value *is* the complete manifest key
  (`"chronicle/species"`, `"locale/regime/micro"`, `"vessel/agent"`).
  `stream_labels()` just calls `.as_str()` on each constant directly.

- **Root+leg** (`domains/astronomy`, `domains/terrain` — the two oldest
  crates, predating the `streams.rs` convention): each constant stores a
  bare, crate-relative leg name (`"star-mass"`, `"plate-count"`); a
  separate `ROOT` constant holds the crate's own top-level prefix
  (`"astronomy"`, `"terrain"`); the *manifest* key is the two concatenated
  (`"astronomy/star-mass"`). Critically, **this composition is a
  documentation-only concern** — the real `Seed::derive()` call chain at
  every use site chains `.derive(streams::ROOT).derive(streams::STAR_MASS)`
  as two separate hash steps; it never derives from the concatenated
  string directly. Today, astronomy and terrain's `stream_labels()`
  functions hand-type the qualified string a *second* time as an
  independent literal (not even `format!`-composed from the constants) —
  the `&'static str` return type can't hold a runtime-composed `String`
  without leaking, so hand-retyping is the workaround in use today. This
  is the single biggest source of both crates' drift risk: every leg's
  qualified name is typed out twice, by hand, with no mechanical link
  between the copies.

The macro must support both shapes, and must **never** change what a real
`.derive()` call site consumes — only how the manifest's documentation
string is built. For the root+leg shape, the compile-time `concat!` macro
(a `std` builtin, not a new dependency) builds the qualified `&'static str`
from the root's literal and each leg's literal at compile time — no
allocation, no `Box::leak`, and critically no change to the two separate
`StreamLabel` constants a real derive chain still uses.

**A third shape stays hand-authored, outside the macro entirely:**
`kernel/src/streams.rs`'s `OCTAVE_LABELS: [StreamLabel<'static>; 16]` is a
single constant holding an *array* of labels (`"octave-0"` .. `"octave-15"`,
a performance table documented in PROC-17 with one shared doc comment) that
is deliberately **not** published as 16 manifest rows —
`stream_labels()` today lists only `ROOM_FACE` and `ROOM_CHILD`, never
`OCTAVE_LABELS`. This is a pre-existing, deliberate exception (an
algorithmically-named family, not 16 independently meaningful derivation
points), not a shape the macro needs to generalize to. `kernel/src/
streams.rs` migrates its two flat entries (`ROOM_FACE`, `ROOM_CHILD`)
through `stream_labels!` and leaves `OCTAVE_LABELS` exactly as it is today,
declared by hand in the same file, outside the macro invocation.

## 5. Design

### 5.1 The macro, two forms

Defined once, in `kernel/src/seed.rs` (co-located with `StreamLabel`
itself) and `#[macro_export]`ed so any crate invokes it as
`hornvale_kernel::stream_labels!`.

**Flat form** (six crates):

```rust
hornvale_kernel::stream_labels! {
    /// The species-generation stream.
    SPECIES = "chronicle/species" => "the species-generation stream";
    /// The carrying-capacity draw.
    CAPACITY = "chronicle/capacity" => "the carrying-capacity draw";
}
```

expands to every `pub const NAME: StreamLabel<'static> =
StreamLabel::from_static(value);` (with its `///` doc comment preserved
verbatim) plus:

```rust
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SPECIES.as_str(), "the species-generation stream"),
        (CAPACITY.as_str(), "the carrying-capacity draw"),
    ]
}
```

**Root+leg form** (astronomy, terrain):

```rust
hornvale_kernel::stream_labels! {
    root: ROOT = "astronomy" => "root stream for sky genesis";
    legs {
        /// Star mass draw.
        STAR_MASS = "star-mass" => "main-sequence star mass draw";
        /// Anchor mass draw.
        ANCHOR_MASS = "anchor-mass" => "anchor world mass draw";
    }
}
```

expands to `ROOT`'s own const, every leg's const (docs preserved), and:

```rust
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOT.as_str(), "root stream for sky genesis"),
        (concat!("astronomy", "/", "star-mass"), "main-sequence star mass draw"),
        (concat!("astronomy", "/", "anchor-mass"), "anchor world mass draw"),
    ]
}
```

`concat!` runs at compile time over the literal tokens captured by the
macro (the root's own literal, re-used as a qualifier, and each leg's own
literal) — it never reads a `StreamLabel`'s runtime value, so the result is
a genuine `&'static str`, matching `stream_labels()`'s existing, unchanged
return type. `ROOT`'s own value is authored exactly once (as the `root:`
line's literal) and used both to build its own constant and as every leg's
qualifying prefix — a single, crate-wide value, reused, versus today's
per-leg hand-retyping.

### 5.2 What the macro removes, beyond the two named authored sites

- **Astronomy's `ALL_LABELS` array and its cross-check test**
  (`domains/astronomy/src/streams.rs`, the `#[cfg(test)] mod tests` block)
  become redundant and are deleted on migration: a macro-generated vec
  cannot diverge from the consts it was generated from, so there is nothing
  left for that test to usefully catch.
- **The per-constant `/// type-audit: bare-ok(identifier-text: return)`
  tag**, currently hand-copied above every single `StreamLabel` constant
  in every `streams.rs` file, is dropped at consuming call sites. Why:
  `tools/type-audit`'s stream-label check
  (`tools/type-audit/src/stream_label.rs`) matches the literal, unexpanded
  token sequence `StreamLabel :: from_static ( <one literal token> )` in a
  crate's own source. Once a crate's `streams.rs` writes
  `NAME = "value" => "desc";` inside a `stream_labels!` invocation instead
  of a literal `StreamLabel::from_static(...)` call, that token sequence no
  longer appears in the file at all, so there is nothing left to tag. This
  is not a workaround or a loophole — the check's own job (a stray literal
  bypassing the constant, written directly at some ad hoc call site
  elsewhere in the crate) is untouched and keeps working exactly as
  before; it simply has nothing to say about a `streams.rs` file that no
  longer contains the pattern it looks for. **No new type-audit check is
  added by this campaign** — the macro's compile-time fusion is strictly
  stronger than a lint for the specific bug this campaign targets (a
  constant present but never reaching the manifest), and existing lints
  keep covering what they always covered.
- **Terrain's live 5-entry gap** (§1) is closed as a forced side effect:
  migrating terrain to the macro requires enumerating every constant in
  one list (a constant that's declared via the macro's repetition but
  omitted from that same repetition simply does not exist, breaking every
  call site that references it), so `LOBING`, `CRUST_SLICE_0/1/2`, and
  `RIFT_CRENULATION` must be given manifest descriptions as part of the
  migration, not as a separate follow-up fix.

### 5.3 Why the doc comment and the manifest description stay two separate strings

The macro requires both a `///` doc comment (or none) *and* an explicit
`=> "description"` argument per entry — it does not attempt to derive one
from the other. This was a real design fork (no direct precedent in the
decision log either way), resolved by:

- **A concrete counter-example already in this codebase.**
  `domains/language/src/streams.rs`'s `FAMILY_LEG` constant carries a doc
  comment explaining an internal implementation detail — avoiding an
  identifier collision with an unrelated `family` variable elsewhere in
  the crate. That prose is meaningful to a Rust reader hovering the
  constant and actively unhelpful, even confusing, if it were surfaced
  verbatim as the public description in the generated stream manifest a
  book reader sees.
- **The general shape recurs with the same answer elsewhere**: an OpenAPI
  operation's internal code comment vs. its public-facing description, or
  a migration's inline comment vs. its changelog entry, are conventionally
  kept as two separate texts because they serve two different audiences
  with two different lifecycles (implementation detail vs. public-facing
  summary), even when authored right next to each other.

This does mean two strings are authored per entry instead of one. Given
the goal is eliminating a *structural* desync (a constant existing in one
place but not the other), not minimizing keystrokes, and given the
concrete `FAMILY_LEG` counter-example, keeping them separate is the
correct tradeoff.

## 6. Determinism and save-format impact

**None.** `Seed::derive()` call sites are entirely untouched by this
campaign — they already reference `streams::NAME` constants (post-PROC-17),
and the macro changes only how those constants are *declared*, not their
values or how they're consumed. Every constant's `StreamLabel::from_static`
argument is copied verbatim from its current source; migrating a crate to
the macro must produce **byte-identical** `StreamLabel` values (verified by
running that crate's existing determinism/property tests unchanged before
and after — a pass proves the migration touched only *where* a value is
declared, never the value itself). `stream_labels()`'s return type and every
crate's `stream_labels()` call sites elsewhere (`cli/src/streams.rs`) are
unchanged. Terrain's five newly-published entries are pure manifest
additions (new documentation rows), not changes to any existing row or to
any `.derive()` call order.

## 7. Testing

- Each migrated crate's existing determinism/property test suite, run
  unchanged before and after migration (byte-identity proof, per §6).
- `cargo run -p hornvale -- streams` regenerated and diffed against
  `book/src/reference/stream-manifest-generated.md` after every crate's
  migration — this is the direct, mechanical proof that no manifest row
  was silently dropped or altered, and that terrain's five previously
  missing rows now appear.
- The macro itself gets unit tests in `kernel/src/seed.rs` (or a new
  `kernel/tests/stream_labels_macro.rs`) covering: flat form expansion,
  root+leg form expansion, and that a doc comment attaches correctly to
  the generated constant (`cargo doc` or a `#[test]` reading
  `env!("CARGO_PKG_NAME")`-independent doc-attribute presence is
  overkill; a simple expansion-and-use smoke test suffices — the macro
  has no runtime logic to unit-test beyond "does it compile to the right
  shape," which every migrated crate's own tests already exercise).
- `make gate` (which now includes `type-audit check` — verified directly
  against the `Makefile`, superseding a stale memory note that it did
  not) must stay green throughout; the type-audit report
  (`docs/audits/type-audit-report.md`) is regenerated after migration
  since every migrated constant's tag line disappears (§5.2).

## 8. Non-goals

- Unifying `domains/language`'s composed multi-leg pattern documentation
  (§3) — banked as a distinct future idea.
- Any new `tools/type-audit` check (§5.2) — none is needed.
- Changing `stream_labels()`'s signature, or any consumer of it
  (`cli/src/streams.rs`'s `sources` list) — unchanged.
- Enforcing that every crate's `streams.rs` *must* use the macro (e.g. a
  lint flagging a hand-written `StreamLabel::from_static` const outside
  the macro) — banked as a possible follow-up, not built here; this
  campaign converts every existing crate, so the question of a future
  holdout is hypothetical until one exists.

## 9. Decision ledger

Full reasoning, including the ideonomy pass and the two decisions this
document's design choices came from, is recorded in this campaign's
worktree at `.superpowers/sdd/decision-ledger.md` (entries #1-#4) and
promoted here where load-bearing: the macro-over-lint choice (§1, §5.2),
the separate-strings choice (§5.3), and the full-workspace migration scope
(§3) are all ledgered decisions, not ad hoc calls.
