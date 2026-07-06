# 0009. Models author, dice roll

**Status:** Accepted (2026-07-06) · **Decider:** Nathan

In the context of an era where it is tempting to call a language model at
runtime to generate world content, facing the tension between that convenience
and the constitution's determinism ([0001](0001-determinism-is-constitutional.md)),
we decided that **no machine-learning model ever runs inside the simulation
core; models are offline authoring amplifiers whose output is committed and
drift-checked**, accepting that all runtime generation must be built from
deterministic, seeded, inspectable mechanisms.

**Context.** A runtime model invocation makes a world's content a function of a
model's weights and sampling rather than of its seed and ledger — unreproducible,
uninspectable, "passing the buck." Models remain welcome to *author* data
(grammars, motif libraries, lexicons, expert-system rule bases) that is then
frozen into the repo.

**Consequence.** The "models author, dice roll" phrase is a ratified
constitutional constraint. The long-term direction is LLMs → committed GOFAI
rule bases, not LLMs at play time.

**See also.** Constitution "Ratified constraint" (spec §2);
`docs/vision/frontier.md`; `CLAUDE.md` "Constraints".
