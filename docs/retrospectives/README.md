# Retrospectives

One page per merged campaign, written at merge time alongside the
chronicle entry (decision 0020). This is the **process** story — the
chronicle tells the product story, the decision log records settled
choices; this directory captures what the work taught about the working.

## Conventions

- One file per campaign, named after its slug: `campaign-<slug>.md`
  (matching the chronicle entry's naming).
- **One page maximum.** If it wants more, the surplus is probably a
  decision record or a spec correction, not retrospective prose.
- Process, not product: what the campaign shipped belongs in the
  chronicle. What belongs here:
  - **Recurring review findings** — anything a reviewer flagged that has
    been flagged before (the fmt-gate skip is the canonical example).
  - **Estimate deltas** — which stages ran long or short, and why.
  - **Spec versus reality** — assumptions the spec made that the plan or
    the code had to correct, so the next spec doesn't repeat them.
- Written once, at merge; not edited after (append a dated postscript if
  something is learned later).

## Template

```markdown
# Campaign <name> — retrospective

**Merged:** YYYY-MM-DD

**Recurring findings.** …
**Estimate deltas.** …
**Spec vs. reality.** …
**Do differently next time.** …
```
