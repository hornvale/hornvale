# 0059. The Book is the primary published artifact; Chronicle and Frontier become appendices

**Status:** Accepted (2026-07-17) · **Decider:** Nathan (G3, The Self-Writing
Book program metaplan)

In the context of The Self-Writing Book program shipping a real, drift-
checked rendering of the world's own Fact graph — and the project book
(published at hornvale.github.io/hornvale) needing exactly one thing at its
front door — facing the choice between leaving the Chronicle (campaign
history) as the book's public face and demoting it in favor of the newly
generated Book, we decided that **The Book becomes the primary published
artifact; the Chronicle and the Frontier move to appendices**, preserving
both sections' existing drift-checks, registry-ID scoping rules, and their
role in every campaign's Definition of Done, only relocated in
`SUMMARY.md`.

**Context.** Today the project book's spine is process-first: chronicle
entries and frontier essays are what a visitor to hornvale.github.io/
hornvale sees first, because they are what campaigns have produced since
the book existed. But the Chronicle and Frontier describe the *project*
building Hornvale, not the *world* Hornvale simulates — they are meta-
documentation, valuable to a contributor tracing why a decision was made,
but not why a reader would come to a world-simulation project's website.
The Self-Writing Book inverts this: `hornvale book` renders the world's own
generated self-description — "Elthandil is a planet," and eventually a
tour of its peoples, places, and history — entirely from committed Facts,
with zero human- or LLM-authored surface text (metaplan §1). That is the
artifact this whole program exists to produce, and once C1 ships it as a
real, if minimal, drift-checked gazetteer, it is a *stronger* front door
than the campaign history: it demonstrates the sim rather than narrating
the process that built it. The metaplan's north star (UNI-29) makes this
more than a navigational preference — it names the long-term direction in
which the Chronicle and Frontier themselves eventually become views the
same engine renders, considers, and revises, rather than hand-authored
markdown.

**Decision.**

- The Book (the CLI-emitted, drift-checked rendering from `windows/book`)
  becomes the primary published artifact: first in `SUMMARY.md`, the
  landing experience at hornvale.github.io/hornvale.
- The Chronicle and the Frontier relocate to appendices in `SUMMARY.md`.
  This is a navigational demotion only — every property that made them DoD
  artifacts is preserved unchanged in the new location: the chronicle-entry
  requirement for every merged campaign (decision
  0013), the frontier
  registry's drift-check and idea-registry ID-scoping rule
  (`book/src/frontier/idea-registry.md`), and their status as durable,
  grep-able project history.
- This relocation is purely structural for C1 (a `SUMMARY.md` reorder plus
  whatever section-header renumbering mdBook needs); it commits to no
  content change in either section beyond that move.
- The long-term north star (frontier registry row UNI-29) — the same
  rendering engine eventually generating the frontier, chronicle, and
  studies as the program's own self-description — is named as vision here,
  not committed scope; C1 only takes the first structural step (making room
  at the front for The Book) and must not foreclose that direction.

**Consequence.** A visitor to the published book meets the simulated world
before the project that simulates it — the intended framing for a "sim
first, game as lens" project. Every campaign's existing DoD obligations to
the Chronicle and Frontier continue to apply verbatim; only the table of
contents changes, so no campaign's process is disrupted by this reorder.
Future Book-program campaigns (C2–C8) deepen The Book's content without
needing a second restructure decision, and UNI-29 remains a named,
trackable bet rather than a silent assumption smuggled into this
relocation.

**See also.** `docs/superpowers/specs/2026-07-17-the-self-writing-book-program-metaplan-design.md`
§§1, 3 (C1), 5 (item 2), §8 flag 2; `book/src/frontier/idea-registry.md`
(row UNI-29); decision
0013 (Definition of Done includes the book);
decision
0030 (Confidence Gradient re-scoring, the same book that hosts
`open-questions.md`).
