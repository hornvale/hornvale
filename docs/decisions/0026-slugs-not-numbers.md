# 0026. Slugs, not numbers

**Status:** Superseded by 0043 (for decision records; the study/chronicle/registry-row provisions stand) · **Decider:** Nathan

In the context of parallel campaign sessions triple-colliding on
sequentially numbered artifacts in two days (two Campaign 20s, two decision
0023s, two study-010s, double-minted LANG-5..8 and RENDER-5 registry rows,
and a Campaign 27 claimed on a branch while another Campaign 27 merged),
facing the fact that sequential IDs require a coordinator that uncoordinated
sessions do not have, we decided that **new durable artifacts are identified
by unique slugs, not sequence numbers, forward-only**:

- **Decisions:** slug filenames (`decisions/<slug>.md`), date in the
  header; cited as ``decision `<slug>` ``. This file is the last numbered
  decision.
- **Studies:** keyed by the study's own name, which already exists and has
  never collided (`census-of-words` → `study-census-of-words.md`).
- **Chronicle entries and retrospectives:** slug-only filenames; reading
  order lives in `SUMMARY.md`, which was always the real ordering
  authority. Campaign designations are name-only ("The Words", not
  "Campaign 27") — this supersedes decision 0017's number+name convention.
- **Registry rows:** category+slug for new rows (`LANG-exonyms`, not
  `LANG-6`); categories stay as grep buckets. When two sessions mint the
  same slug they have usually minted the same idea, so a collision becomes
  a content merge instead of a renumbering.

accepting that citations lose their old brevity ("0016" → a slug) and that
the corpus reads mixed-era, since existing numbered artifacts freeze as
history: no renames, no renumbering, every existing cross-reference keeps
resolving.

**Context.** The repo ran the controlled experiment itself: specs and plans
have always been date+slug and survived the same week with zero collisions,
while every number-keyed family collided at least once. Chronology — the
one thing numbers provided — is recoverable from dates and git history, and
was never the load-bearing property; uniqueness was. Slug uniqueness is
enforced where ID uniqueness already is (`cli/tests/docs_consistency.rs`);
the registry's existing rules (IDs permanent, rows never deleted, statuses
flip) apply to slugs unchanged. The reservation-at-spec-time alternative
(TOOL-13) was considered and set aside: it patches coordination back in,
where slugs remove the need for it.

**Consequence.** The renumber-at-merge ritual retires for new artifacts.
The Type Audit campaign (in flight, claiming "Campaign 27" against the
merged Words chronicle at 27) adopts name-only designation instead of
renumbering — the first beneficiary. `docs_consistency` gains a check that
no new numbered decision/chronicle/study file appears once the freeze
lands.

**See also.** Decision 0017 (campaigns drop the Year naming — its
number+name convention superseded here); decision 0020 (retrospectives —
their filenames now match the chronicle slug); TOOL-13 (the set-aside
reservation alternative); `docs/CLAUDE.md` (registry ID rules, which now
read "next free number" as "a fresh category+slug").
