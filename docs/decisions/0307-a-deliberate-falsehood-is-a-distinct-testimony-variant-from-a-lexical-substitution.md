# 0307. A deliberate falsehood is a distinct testimony variant from a lexical substitution

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0256](0256-a-hosts-testimony-is-fallible-by-construction.md) (the two
incapacities this adds a third, chosen unreliability to), [0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md);
[The Reticence](../../book/src/chronicle/the-reticence.md)

In the context of giving a possessed host the ability to lie about its felt
state, we decided that **a deliberate falsehood is carried by a new type,
`Testimony`, that wraps `FeltStateWord` rather than a new variant added
inside it**, accepting two types where one might have looked sufficient, in
exchange for making a committed metric structurally incapable of conflating
them.

## Context

A lie and a lexical substitution have the same shape — report state X when
the truth is Y — and different causes: one is a culture with no word for the
truth, reaching for its nearest neighbour; the other is a host that has the
word and declines to use it. `misreport_distance_for`
(`windows/lab/src/metrics.rs`, feeding `docs/audits/the-confidant-report.md`)
already compares concept ids and cannot tell the two apart by construction —
it has no access to *why* a reported concept differs from the true one, only
that it does.

The spec's first draft considered adding a `Falsehood` arm directly to
`FeltStateWord`. That would have been reachable: `misreport_distance_for`
takes a `FeltStateWord` and would silently have started averaging two
phenomena into one number, and the committed report would keep reading as a
coverage measurement while having become a mixture — the exact failure §4.3
of the spec names.

## The rule

`Testimony` is the outer type `Session::ask` actually produces:
`Spoken(FeltStateWord)`, `Withheld`, `Falsehood { word, claimed }`, and
`Costly { word: FeltStateWord, revealed }`. Only `Spoken` and `Costly` carry
a `FeltStateWord` at all. `misreport_distance_for`'s signature is unchanged
and still takes a `FeltStateWord` — so a `Falsehood` is not filtered out of
its input by a runtime check, it is **not a value the function's type can
receive**.

## Consequences

- **The metric is protected by the type system, not by a convention someone
  has to remember to uphold.** A future change routing a `Falsehood` into the
  metric would have to change the metric's own signature, which is the
  moment to re-litigate this decision, not a silent drift.
- **The single lie this campaign ships is flat and deterministic**
  (`DISSEMBLING_CLAIM = AffectLabel::Content`, "I am fine"): the deliverable
  is that a host *can* lie, not a taxonomy of lies a chooser selects among.
- **What we give up:** two types to reason about instead of one, and a
  `render_testimony` that matches on four `Testimony` arms instead of two
  `FeltStateWord` arms. Given the choice, the campaign judged an
  unconflatable metric worth that cost.

## See also

Spec §4.2-4.3; `windows/vessel/src/testimony.rs`.
