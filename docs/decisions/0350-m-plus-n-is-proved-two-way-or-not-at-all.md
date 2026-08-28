# 0350. M+N is proved two-way or not at all

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §11,
§6) · **Relates:**
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md)
(the same two-directional discipline, applied to duplicated rules),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md),
[0348](0348-object-properties-are-keyed-by-kind.md) · `MAP-27` ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of claiming that a world is authored M+N rather than M×N, we
decided that **the claim is only proved by asserting both directions at once**,
accepting that a campaign which can only demonstrate one has not demonstrated
the property.

## Context

The two directions are:

1. **A new object kind ships with properties only** — no dispatcher change —
   and the right verbs appear on it.
2. **A new verb ships declaring required properties only** — no object change
   — and it appears on every object that qualifies.

Each is individually satisfiable by the very table it is supposed to exclude.
(1) passes trivially if verbs are hardcoded per kind and you add a kind that
reuses an existing row. (2) passes trivially if objects are hardcoded per
verb. Only both, held at once, exclude the table.

## The rule

An M+N claim carries a test in each direction, and a structural check that the
forbidden table is absent. In this campaign `warm` is (2)'s live witness — the
one new verb, declaring `RadiatesHeat`, appearing on the hearth without the
hearth being edited.

## Consequences

- **The structural half is scoped, and the scope is stated rather than
  implied.** `no_verb_by_object_table_exists` scans exactly one file
  (`windows/vessel/src/affordance.rs`) for exactly one syntactic shape. A
  table in another file, one reached through a helper, or one keyed on
  something other than `AnchorKind` is not seen — and a real instance already
  exists in the tree, `interior/field.rs`'s `warmth_at`, which gates on
  `kind != AnchorKind::Hearth`. Acceptance clause (4) is therefore worded
  "no verb×object table exists **in the offer's own module**", not "anywhere".
- **The campaign's own new verb reintroduced the coupling this record
  abolishes**, and neither the M+N query tests nor a session-level success
  test could see it: `Session::warm` gated on `AnchorKind::Hearth` directly.
  The guard that catches it is a source scan of the dispatcher's own body,
  because `Bed` and `Hearth` co-occur in every real chamber, so no behavioural
  test can discriminate two correlated anchor kinds.
- The general form is worth carrying past this arc: **a claim about what the
  code does not contain needs a structural assertion, and a structural
  assertion must state the direction it enforces.**
