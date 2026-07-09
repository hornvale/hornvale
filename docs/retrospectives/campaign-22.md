# Campaign 22 (The Orrery) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** A determinism gate pins *byte-stability, not
correctness* — and this is now the second campaign burned by it. Campaign 19's
retro said it in as many words: the freshness gate "checks that the PNG is
byte-deterministic, not that it is *correct*," and would have regenerated a
malformed digit glyph forever. Campaign 22 walked straight into the same trap
one artifact type over: the committed `.cast` recording shipped with bare
line-feed line endings, which look right printed to a live terminal (the tty
translates LF→CRLF) but smear on a raw asciinema replay, which needs the full
carriage-return-and-line-feed. The drift check happily pinned the stably-wrong
recording; the CLI test asserted the recording's *event count and determinism*
but never that a frame was *legible*; a clean `mdbook build` said nothing. The
defect was caught by a human watching the animation play, not by any gate. The
generalizable rule, now twice-earned: **when you commit a rendered artifact,
one test must assert a property of its *content*, not only that the bytes are
reproducible** — a glyph is present, a frame carries CRLF, the picture is the
right picture. Byte-determinism keeps a correct artifact correct; it does
nothing for a wrong one.

**Estimate deltas.** No stage estimates were made. The one unbudgeted cost was
entirely self-inflicted (see below): the campaign's own plan omitted its book
Definition-of-Done, so a whole follow-up — chronicle, chapter description,
rendering-strategy amendment, this retrospective, and the line-ending fix that
surfaced alongside them — landed *after* the merge instead of within it.

**Spec vs. reality.** Two gaps, one in the plan and one in a review method:

- **The plan omitted the book.** Definition of Done for a merged campaign
  includes a chronicle entry and a freshness sweep (decision 0013), and every
  campaign also writes a retrospective (decision 0020). The Orrery's six-task
  plan had a task for the gallery *demonstration* (the embedded `.cast`) but no
  task for the *description* — no chronicle, no chapter prose, no
  rendering-strategy amendment its own spec (§9) had promised, no retro. So the
  campaign merged product-complete but book-incomplete, and stayed that way
  until someone asked "is this in the book?" The contrast is instructive: Firm
  Ground II (Campaign 20) carried its book work as an explicit closing plan and
  shipped whole; The Orrery treated the book as implied and shipped a hole. A
  DoD item that isn't a plan task is a DoD item that gets skipped.
- **A clean build is not a working page.** The book embed passed `mdbook build`
  and every wiring line was correct in isolation — right filenames, right
  player, right cast dimensions — yet the animation never played, because
  mdbook injects the player script *after* the page's inline initializer, so
  the initializer ran against an undefined global on every load. Only building
  the book and reading the rendered HTML surfaced it. A runtime defect needs a
  runtime check; a diff review and a green build both looked past it.

**Do differently next time.** Make the book DoD an explicit task (or a final
plan) in *every* campaign plan, not an assumed epilogue — the same discipline
that made Firm Ground II close cleanly. When a campaign commits a rendered
artifact — an image, a recording, a page that runs a script — pair the
determinism gate with a *correctness* assertion: test a property of the content
(a present glyph, a CRLF-terminated frame), and for anything with runtime
behavior, verify the *rendered output*, not just that the build was clean. The
cheapest place to catch "the picture is stably wrong" is a test that looks at
the picture; the most expensive is a human noticing after it shipped.
