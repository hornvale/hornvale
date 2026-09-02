# 0557. A handle is a convenience where a variant was a requirement

**Status:** Accepted (2026-09-01) · **Decider:** Nathan (autopilot) ·
**Relates:** [0556](0556-totality-by-registry-replaces-totality-by-compiler.md),
[0397](0397-the-knowledge-gate-denies-a-passage.md) ·
**Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md` #14, #15, #22,
#27 · **Spec:** The Wicket §4.2

In the context of replacing the closed `AnchorKind` enum with `KindId` and
needing production predicates not to become stringly typed — `kind ==
KindId("hearht")` compiles — we decided that **`domains/thing` publishes named
`KindId` constants beside the roster, and a kind with no constant is a
first-class kind**, accepting that the handle list is hand-written and can
therefore go short.

## Context

The obvious reading of the re-key is that the variants become constants and
nothing else changes. That reading preserves the defect. A variant was
**mandatory**: a kind with no variant could not be placed in a room, however
open the component stores behind it were. A constant is a **convenience for
code that must name the kind**, so the compiler can catch a typo in a
predicate.

The asymmetry is the whole campaign, stated in one line. Adding a kind is a
data edit — a `THING_KINDS` row, a `thing_registry` row, a chamber-prose row,
optionally an `object_registry` row, and (for a placeable kind) an appended
`Pattern`. No enum, no match arm, no macro, no handle required.

## What was decided

- **`domains/thing::kinds` publishes one `pub const KindId` per kind that code
  names**, and `EVERY_HANDLE` lists them for G-d (*named ⊆ rostered*,
  decision 0556).
- **The direction is one-way on purpose.** There is no converse
  *rostered ⊆ named* check, because such a check would make a handle mandatory
  again and undo the thing this record ratifies.
- **`EVERY_HANDLE` is hand-written, deliberately.** A macro generating both the
  constants and the list would make the list unable to disagree with them —
  which is exactly what made the deleted `anchor_kinds!` roster safe and is
  exactly what is not wanted here. This list is checked against a third party
  (the roster), so it must be able to go wrong.
- **Add a handle when you write code that names the kind, never "for
  completeness."** An unused handle is a name with no reader.
- **`chamber_prose::noun` takes `&str` and `detail` takes `KindId`, and the
  asymmetry is documented at the table.** `KindId(pub &'static str)` requires a
  `'static` label; three of `noun`'s callers read the ledger, where
  `Ledger::kind_of` returns `Option<&str>` borrowed from a `String` in the fact
  store. A runtime slice cannot be wrapped in a `KindId` at all. Unifying the
  two accessors on `&str` for surface symmetry would lose `detail`'s
  typo-safety at every interior call site; relaxing `KindId`'s lifetime is a
  kernel change with workspace-wide blast radius and was refused (ledger #14).

## Consequences

- **The roster is what a sweep must iterate, not the handle list.** G-d checks
  only *named ⊆ rostered*, so a handle dropped from `EVERY_HANDLE` silently
  narrows any sweep measured against it. `THING_KINDS` is frozen as an ordered
  set and cannot go short, so it is strictly stronger (ledger #15).
- **This rule is currently unexercised, and saying so is the point.** All 17
  rostered kinds happen to have a handle today, so no kind in the tree
  demonstrates the absence this record permits. The rule is stated for the
  campaign that adds a kind no predicate names — it is a licence, not an
  observation.
- **The bare-literal residual is accepted and named.** A `KindId("…")` written
  inline at a genuine future consumer site compiles silently. The census found
  no such site: over `windows/vessel/src` and `domains/thing/src`, discounting
  lines that already name a handle, all **78** occurrences are authoring
  tables, tests, or doc comments. G-a, G-b/G-c and G-e already check every
  literal the **three** authoring tables hold, from the other direction —
  against the roster, rather than by forbidding the literal (ledger #22, #61;
  the scoping matters, since the same bare pattern over every Rust source in
  the workspace is 671, and decision 0556 carries the commands).

  *Corrected before merge: this bullet first read "58 occurrences" and "the two
  authoring tables", carried over from a Task-3-era census rather than
  re-measured. Task 4 made prose a component table, which took
  `chamber_prose_registry` from 1 occurrence to 18 and made it the third.*
- **A handle is not a concept.** Registering a kind still obligates a
  `concept_doc` arm and an accession cohort (decision 0556's consequences), and
  neither of those is a handle. The convenience this record ratifies buys
  nothing on that side.

## See also

`domains/thing/src/lib.rs` (`kinds`, `EVERY_HANDLE`,
`every_named_handle_is_a_roster_row`); `windows/vessel/src/chamber_prose.rs`
(the `&str`/`KindId` asymmetry and why it is not tidied).
