# 0016. Studies preregister their hypotheses

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of the Lab testing substantive claims about generated worlds,
facing the fact that determinism cuts both ways — seeds can be swept until a
desired pattern appears — we decided that **a study states its hypothesis in
its JSON before the sweep runs, and negative results are published in the
book's laboratory pages**, accepting the small friction of writing down what
we expect before we look.

**Context.** The Lab is the instrument that makes every frontier claim
measurable ("cultures plagued by trickster gods develop more defensive
architecture"). An instrument whose operator can re-roll the universe until
the pattern appears produces anecdotes, not findings. Preregistration is the
standard scientific fix, and here it costs one field.

**Consequence.** Study JSON gains an optional-but-expected `hypothesis`
field (free text stating the expected result); a study run to test a claim
is committed before its results are analyzed; laboratory book chapters
report the hypothesis alongside the outcome, including when the outcome is
"no effect." Exploratory sweeps remain legitimate but are labeled as such
and are never promoted to confirmations after the fact.

**See also.** Decision 0011 (studies are data, metrics are code); the
frontier map's Laboratory item (`docs/vision/frontier.md`); chronicle L0.
