# 0020. Campaigns write retrospectives

**Status:** Accepted (2026-07-07) · **Decider:** Nathan

In the context of process lessons living only in code comments and chat
transcripts (recurring review findings like fmt-gate skips, estimate
misses, spec assumptions the plan had to absorb), facing a documentation
set that captures *choices* (this log) and *product* (the book) but never
*lessons*, we decided that **every merged campaign adds a one-page
retrospective in `docs/retrospectives/`, written at merge time alongside
the chronicle entry**, accepting one more Definition-of-Done item.

**Context.** The decision log records what was settled; the chronicle
records what was built. Neither records what the process taught — which
review findings recur, where estimates broke, what the spec got wrong.
That knowledge currently evaporates or fossilizes into comments (the
fmt-gate note in CLAUDE.md is exactly this, homeless).

**Consequence.** Definition of Done (decision 0013) gains a retrospective:
one page maximum, process not product — recurring review findings,
estimate deltas, spec-versus-reality corrections. The chronicle entry
stays the product story; the retrospective is the process story.
Conventions and a template live in `docs/retrospectives/README.md`.

**See also.** Decision 0013 (Definition of Done includes the book);
`docs/retrospectives/README.md`.
