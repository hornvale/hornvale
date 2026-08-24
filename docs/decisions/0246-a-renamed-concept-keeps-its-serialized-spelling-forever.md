# 0246. A renamed concept keeps its serialized spelling forever, and the freeze needs a test that cannot be rebaselined

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Relates:**
[0006](0006-seed-derivation-is-labelled.md),
[0033](0033-quantize-at-the-emit-boundary.md),
[0055](0055-the-repo-boundary-is-the-determinism-boundary.md);
[The Lexicon of Place](../../book/src/chronicle/the-lexicon-of-place.md);
[The Lexicon of Place](../../book/src/reference/lexicon-of-place.md) (the glossary)

In the context of a campaign that renamed the spatial vocabulary across
~16,000 sites — `CellId` to `Vertex`, `RoomAddr` to `Facet` — while every
serialized spelling of those concepts stayed `"cell"` and `"room"`, we decided
that **a concept's serialized spelling is frozen independently of what the
code calls it, that the resulting mismatch is correct and permanent rather
than debt, and that each such spelling needs an assertion of the literal that
a rebaseline command cannot satisfy.**

## The rule, in two clauses

1. **A constant's NAME may be renamed; its string VALUE may not.** After this
   campaign the code says `Facet` while the seed-derivation label says
   `"room/face"`. That is the intended end state, not a half-finished rename.
2. **For a serde field the name IS the value**, so rename the Rust field and
   add `#[serde(rename = "<old wire name>")]`. Do it that way round rather
   than leaving the field spelled `cell`: an attribute is a *visible*
   tripwire, and a silently-old field name is exactly what the next sweep
   renames without noticing.

The frozen set is enumerated in the glossary, not here, because it grows: this
campaign's own audit found four classes the design spec had missed — two
census golden columns, three CSV fixture headers, the serialized JSON keys,
and two ledger *subject* strings (`"cell/{}"` in `volcano.rs`,
`"cell/{}/process/{}/block/{}"` in `hazard.rs`) that appear in every committed
world.

## Why prose alone will not hold it

The next campaign to notice the mismatch will want to finish the job. That is
the whole hazard, and the design spec located it correctly while getting its
severity exactly backwards — which is the part worth recording, because the
backwards version is the intuitive one.

The spec said the `"room/"` knowledge-key prefix had no cover at all: "two
literals, in two files, with nothing asserting they agree ... rename one and
the fog of war silently stops working: no test fails." Mutation testing with
`scripts/mutate.py` says otherwise, three ways.

**There are three sites, not two.** The spec names the writer and the
`strip_prefix` reader; it misses the `[segment, id]` arm of
`knowledge_is_subset`, a default-deny validator and the strongest cover of the
three.

**Tests do fail — and how many depends on how tidy you are:**

| mutation | tests red |
|---|---|
| the writer alone | 6, including the fog-of-war test the spec says goes silent |
| writer + `purview.rs` | 5 |
| **all three, consistently** | **2** |

**The third row is the finding.** A rename campaign tidies all three sites at
once, which is the third row — and the only two objectors are
`session_snapshot::v2_bytes_are_pinned` and
`session_snapshot::the_client_fixtures_are_current`, both **byte-goldens whose
documented remedy is `make rebaseline-goldens`**. So the suite goes red, one
command makes it green, and a changed session-snapshot wire format ships to
`clients/game` with a green gate behind it.

**A red with a one-command answer is worse than no cover.** No cover at least
leaves nothing that looks like a verdict. This leaves a verdict that a
reasonable person discharges in ten seconds.

## What follows for the shape of the guard

A byte-golden is not an assertion about a *contract*; it is an assertion about
current output, and its whole purpose is to be re-accepted when output moves
deliberately. So a wire-value freeze may never rest on one. The guard for a
frozen spelling is a test that **writes the literal out** and compares — one
that cannot be rebaselined, whose failure message names every site that
depends on the value and says why it may not move.

`the_locale_key_prefix_is_frozen` (`windows/vessel/src/knowledge.rs`) is the
worked example. It also states the direction it enforces, per the standing
rule that a check must name what it is blind to: it proves the string has not
moved, and is blind to a future author re-inlining a literal past the
constants.

## What we are not deciding

This does not say a serialized spelling may never change. It says changing one
is an **epoch** — a `/v2` suffix, never a rename or an edit in place — which
is decision 0006's existing rule, restated here only because a renaming
campaign is precisely the context in which people forget it applies.
