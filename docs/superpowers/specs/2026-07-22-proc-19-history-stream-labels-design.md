# PROC-19: migrate `domains/history` to `stream_labels!`

**Working campaign name:** The Compound Word.

**Status:** draft, awaiting G3 (spec review).

**Worktree:** `~/.config/superpowers/worktrees/hornvale/proc-19`, branch `proc-19`, off `main` @5f6489cd.

## 1. Problem

[PROC-18 (The Single Saying)](../../../book/src/chronicle/the-single-saying.md)
migrated all 8 crates that drew deterministic seed streams to
`hornvale_kernel::stream_labels!`, a macro that fuses a crate's
`StreamLabel` constant declarations with their `stream_labels()` manifest
entries into one authored list. `domains/history` landed on `main`
mid-campaign, from an unrelated, parallel campaign (The Living Community
and its successors), and was deliberately left out of PROC-18's scope —
not because it doesn't need the same fix, but because its shape doesn't
fit either of the macro's two existing forms.

`domains/history/src/streams.rs` declares six constants:

- `ROOT = "history"`, `RESIDUE = "residue"`, `STRUCTURES = "structures"` —
  a genuine root+leg group. Real call sites chain
  `.derive(streams::ROOT).derive(streams::RESIDUE)` (or `STRUCTURES`), so
  these three fit the root+leg arm exactly, as documented.
- `BAKE = "history/bake"`, `GENESIS = "history/genesis"`,
  `FLESH = "history/flesh"` — three already-fully-qualified flat labels.
  These are **not** children of `ROOT` in the derivation sense: the doc
  comments are explicit that the deep-history bake which draws from these
  runs at the composition root (`windows/worldgen`), an entirely
  different call site from this crate's own local flesh derivations, and
  never chains off `streams::ROOT` at all. `GENESIS` already derives its
  own dynamic per-people sub-stream
  (`history/genesis/<people-kind>` via `StreamLabel::dynamic`), so it is,
  structurally, the root of its own (currently dynamic-only) subtree —
  just one with no *static* legs declared yet.

Neither macro arm accepts this combination. The flat arm has no concept
of a root/leg group; the root+leg arm's `legs { }` block always qualifies
every entry under the root via `concat!`, which would be actively wrong
for `BAKE`/`GENESIS`/`FLESH` (it would produce `"history/history/bake"`,
a manifest key that does not match how the label is ever actually
derived). Two separate macro invocations in the same file don't work
either: each invocation's expansion includes its own
`pub fn stream_labels()`, and Rust forbids two functions of the same name
in one module.

Unlike PROC-18's terrain/climate findings, this is **not** a live
documentation gap — `domains/history`'s current hand-typed
`stream_labels()` (in `lib.rs`) is already complete and correct for all
six entries, verified directly against `streams.rs`. This crate is too
recent for the desync bug to have had time to recur; the problem here is
purely structural (the macro can't express this crate's shape yet), not
a live drift.

## 2. Goal

Extend `hornvale_kernel::stream_labels!` with a third arm that supports
exactly this shape — a root+leg group plus one or more independent,
already-qualified flat entries, all combined into one generated
`stream_labels()` — then migrate `domains/history` to it. The two
existing arms (flat-only; root+leg-only) are untouched: this is a pure
textual addition, not a modification, so none of the 8 already-migrated
crates are put at any risk.

## 3. Scope

**In scope:** the new macro arm (`kernel/src/seed.rs`) and migrating
`domains/history/src/streams.rs` + `domains/history/src/lib.rs` to it.

**Out of scope:** a more general "N independent roots, each with an
optional legs block" grammar (see §8) — no crate needs more than one
root+leg group plus flat entries today, and building the fully general
form now would require either a recursive tt-muncher macro (real
complexity for a shape nothing currently needs) or an unjustified
combinatorial arm explosion. Banked as a named idea, not built.

## 4. Design

### 4.1 The new arm

```rust
hornvale_kernel::stream_labels! {
    /// Root stream label for history — reserved for the deep-history
    /// bake. The composition root derives `history/flesh` once per
    /// occupation before calling into this crate's flesh derivations,
    /// which further derive their own sub-labels (RESIDUE, STRUCTURES)
    /// from that already-scoped seed.
    root: ROOT = "history" => "root stream for history: reserved for the deep-history bake (run at the composition root); no draw is made against it directly";
    legs {
        /// Sub-label for `flesh::residue_of`'s deterministic flavor draws.
        RESIDUE = "residue" => "flesh::residue_of's deterministic flavor draws";
        /// Sub-label for `flesh::structures_of`'s dwelling-count variance draws.
        STRUCTURES = "structures" => "flesh::structures_of's dwelling-count variance draws";
    }
    flat {
        /// Root stream label for the deep-history bake's epoch dynamics.
        BAKE = "history/bake" => "the deep-history bake's epoch dynamics: grow/found/migrate/raid/collapse draws, taken sequentially from one stream in commit order at the composition root";
        /// Root stream label for the deep-history bake's genesis draws.
        GENESIS = "history/genesis" => "the deep-history bake's genesis draws: proto-community count, site picks, and tech-advance offset; further derives a per-people sub-stream history/genesis/<people-kind> via StreamLabel::dynamic";
        /// The per-occupation flesh seed the legibility surface derives.
        FLESH = "history/flesh" => "the per-occupation flesh seed the legibility surface derives before expanding residue/structures on demand (never committed)";
    }
}
```

expands to `ROOT`'s const, `RESIDUE`/`STRUCTURES`'s consts (docs
preserved), `BAKE`/`GENESIS`/`FLESH`'s consts (docs preserved, unchanged
from their current form), and:

```rust
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOT.as_str(), "root stream for history: reserved for the deep-history bake (run at the composition root); no draw is made against it directly"),
        (concat!("history", "/", "residue"), "flesh::residue_of's deterministic flavor draws"),
        (concat!("history", "/", "structures"), "flesh::structures_of's dwelling-count variance draws"),
        (BAKE.as_str(), "the deep-history bake's epoch dynamics: grow/found/migrate/raid/collapse draws, taken sequentially from one stream in commit order at the composition root"),
        (GENESIS.as_str(), "the deep-history bake's genesis draws: proto-community count, site picks, and tech-advance offset; further derives a per-people sub-stream history/genesis/<people-kind> via StreamLabel::dynamic"),
        (FLESH.as_str(), "the per-occupation flesh seed the legibility surface derives before expanding residue/structures on demand (never committed)"),
    ]
}
```

The generated vec's content is **identical** to what `domains/history`'s
current hand-typed `stream_labels()` already produces (§1) — this is a
pure declaration-site move, not a content change. `BAKE`/`GENESIS`/
`FLESH`'s own values are used directly via `.as_str()` (exactly like the
plain flat arm), never passed through `concat!` — only `legs {}` entries
get the root-prefix composition, matching the asymmetry documented in
§1 (flat entries are not derivation-children of `ROOT`).

### 4.2 Grammar placement and arm ordering

The new arm is placed **before** the existing root+leg-only arm in
`kernel/src/seed.rs` (most-qualified pattern first, matching ordinary
macro_rules! style — though as analyzed in the decision ledger, the
three arms' shapes are mutually exclusive by their required trailing
tokens, so order does not actually create ambiguity; leading with the
most specific arm is purely a readability convention). The existing
flat-only and root+leg-only arms are not modified in any way — this is a
pure addition of one new arm to the existing `macro_rules!` block.

## 5. Determinism and save-format impact

**None.** Every one of the six constants' `.as_str()` values is
unchanged (`"history"`, `"residue"`, `"structures"`, `"history/bake"`,
`"history/genesis"`, `"history/flesh"`) — verified against the current
file. No `.derive()` call site anywhere in `domains/history` or
`windows/worldgen`'s history-bake code changes; this campaign only moves
where these six constants are *declared* (into `streams.rs`, generated by
the macro, matching the convention every other migrated crate now
follows) and retires the hand-typed `stream_labels()` free function
currently in `lib.rs`.

**Correction (caught by Task 2's own implementer, not anticipated when
this section was first drafted):** the *generated manifest's* two leg
rows DO change content, even though the underlying constants don't.
`domains/history`'s current hand-typed `stream_labels()` publishes
`RESIDUE`/`STRUCTURES` under their bare, unqualified values (`"residue"`,
`"structures"`) — unlike `domains/astronomy` and `domains/terrain`, whose
already-migrated root+leg manifests always publish a leg root-qualified
(`"astronomy/star-mass"`, `"terrain/plate-count"`) via the macro's
`concat!` composition. §4.1's own worked example already shows this
correctly (`concat!("history", "/", "residue")`); this section's earlier
"every value unchanged" claim conflated *constant values* (genuinely
unchanged) with *manifest key presentation* (changes for the two legs,
by design — the new arm reuses the existing root+leg arm's `concat!`
logic exactly, deliberately, per §4.1). **This is accepted as a
deliberate correction**, not an unplanned regression: `domains/history`'s
bare-key rows were the one inconsistency in an otherwise fully saturated
pattern (every other root+leg crate already qualifies every leg), and
`domains/history` predates PROC-18's convention entirely, so there was
never a moment its author could have cross-checked it. No `.derive()`
call site, test, or downstream consumer anywhere in the codebase asserts
the old bare-key manifest text (verified by a full-workspace grep).

**Further correction (caught by the final whole-branch review, not by
the paragraph above):** the shipped manifest diff is not confined to the
two leg keys — the *description* text of all six `history`/`history/*`
rows changed too, dropping campaign-internal `(Task 2)`/`(Task 3)`
phase parentheticals and tightening a few phrases (e.g.
`StreamLabel::dynamic(people.0)` → `StreamLabel::dynamic`). This was a
deliberate editorial choice already present in this plan's own Task 2
replacement text (a permanent reference page shouldn't carry a
since-completed crate's internal task numbering), but the paragraph
above, written when only the key-qualification issue was in view,
under-stated it as "the generated book page's two `history/*` leg rows
are the only content affected." The accurate scope is: **all six rows'
description text was reworded (cosmetic, zero functional impact — these
strings are documentation prose, never fed into any `.derive()` call),
and two of the six rows' keys were additionally re-qualified** (the
leg-qualification correction above). Both changes are confined to
`domains/history`'s section of the generated manifest; no other crate's
rows are touched. This is itself a small instance of the same lesson
decision ledger #3 already named: a claim about a generated artifact's
diff should be checked against the actual regenerated output, not
inferred — including when *correcting* an earlier such claim.

## 6. Testing

- `domains/history`'s existing test suite (including
  `stream_labels_declare_the_root`, `flesh::residue_of`/
  `structures_of`'s own tests, and `windows/worldgen`'s history-bake
  byte-identity tests) run unchanged before and after — a pass proves
  byte-identity, per this project's determinism discipline (root
  `CLAUDE.md`, "Determinism" section).
- New unit tests for the macro's third arm, alongside the existing two
  in `kernel/src/seed.rs`'s `stream_labels_macro_tests` module: a
  synthetic root+legs+flat invocation, asserting the flat entries' bare
  values are unchanged AND their manifest keys are their own literal (not
  root-qualified), while the legs' manifest keys ARE root-qualified.
- Manifest regen (`cargo run -p hornvale -- streams`) diffed against the
  committed `book/src/reference/stream-manifest-generated.md` — expect
  **zero content diff** for `history/*` rows (this is a pure
  declaration-site move, not a content fix, unlike PROC-18's
  terrain/climate tasks).
- `make gate` (includes `tools/type-audit -- check` locally) green
  throughout.

## 7. Non-goals

- The general "N independent roots, each with an optional legs block"
  macro grammar (§3, §8) — banked, not built.
- Any change to `domains/history`'s actual stream values, derivation
  order, or the deep-history bake's logic in `windows/worldgen` — pure
  declaration-site refactor.
- Migrating any other crate — this campaign is scoped to `domains/
  history` alone, the one crate PROC-18 found didn't fit either existing
  arm.

## 8. Follow-up idea, banked not built

The three-arm design (flat; root+leg; root+leg+flat) is the minimal
in-style fix for the one shape a real crate needs today. A cleaner
general abstraction exists underneath it, surfaced during brainstorming
(ideonomy pass, decision ledger #1): `domains/history`'s "flat" entries
are not really peer labels the way, say, chronicle's `SPECIES`/`CAPACITY`
are — `BAKE` and `GENESIS` are each the root of their own (currently
dynamic-only) derivation subtree. The fully general shape is "a crate may
declare N independent roots in one `stream_labels!` invocation, each
optionally followed by a static `legs {}` block" — of which a plain flat
entry is the degenerate zero-legs case, and today's three arms are the
degenerate one-or-two-roots cases. Building the general form would
require either a recursive tt-muncher macro or an unbounded combinatorial
arm explosion — real complexity with no second crate needing it yet.
Worth revisiting only if a future crate needs a third distinct shape
(e.g. two independent root+leg groups in one file); until then, adding
arms one at a time, each a pure textual addition, stays the boring,
in-style choice.

## 9. Decision ledger

Full reasoning (including the ideonomy pass) is recorded in this
campaign's worktree at `.superpowers/sdd/decision-ledger.md` (entries
#1-#2).
