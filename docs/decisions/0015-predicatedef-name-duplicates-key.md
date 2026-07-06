# 0015. `PredicateDef.name` duplicates its registry key

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of the concept registry storing predicate definitions in a map
keyed by name, facing the mild redundancy of also storing the name inside the
`PredicateDef`, we decided to **let `PredicateDef.name` duplicate its registry
key**, accepting the small denormalization so a definition is self-describing
when handled apart from the map.

**Context.** Recorded as a "do not relitigate" item in a plan's Self-Review
Notes. The duplication makes iteration and rendering simpler and keeps a
`PredicateDef` meaningful on its own.

**Consequence.** No change; the record exists to close the question.

**See also.** Campaign 1b plan Self-Review Notes; `CLAUDE.md` "Ratified
decisions".
