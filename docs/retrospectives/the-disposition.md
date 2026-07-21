# The Disposition — retrospective

One page, process not product. Bundled `arbitrate`'s four loose disposition
scalars (latency, horizon, helpless, awake) into a `Disposition` struct — a pure,
byte-identical refactor that closes the standing "arbitrate tidy" followup the
last four campaigns kept flagging.

## What worked

- **The destructure kept the body untouched.** The one real risk of a
  signature refactor is quietly changing behaviour in the body. Binding `let
  Disposition { latency, horizon, helpless, awake } = *disposition;` at the top
  of `arbitrate` meant every downstream reference to those names compiled and ran
  exactly as before — the body diff is literally zero lines. The refactor's blast
  radius was the signature and the call sites, nothing else.

- **A thin test adapter beat rewriting 22 call sites.** The production callers
  (three of them) construct `Disposition` directly, so the safety win — named
  fields, no swappable positional `latency`/`horizon` — lands where it matters.
  The ~22 test call sites, which each vary these four values case by case, go
  through a one-line `arb` adapter that packs them into a `Disposition`; the
  conversion was a scoped `arbitrate(` → `arb(` rename inside the test module, not
  22 hand-edits with their attendant transcription risk. The `#[allow(too_many_
  arguments)]` moved from production onto that test-only adapter, which is the
  right place for it.

- **Byte-identity was checkable three ways, and all three held.** The 103
  arbitration tests passing *unchanged* (a moved expected value would have meant a
  behavioural change), the type-audit report coming out net-neutral (the four tags
  moved from the signature to the struct doc, no primitive added or dropped), and
  — the strongest — the artifact regeneration showing **zero** drift, possession
  galleries included. A refactor that claims byte-identity should be able to prove
  it without trusting the diff; this one could.

## What to watch

- **The test adapter preserves the positional footgun in tests.** `arb` still
  takes nine positional arguments, so a swapped `latency`/`horizon` in a *test*
  would compile — the readability win is production-only. That is an acceptable
  asymmetry (a wrong test value fails loudly, and the tests are the lower-stakes
  callers), but if a future campaign touches these tests heavily, converting them
  to explicit `Disposition` literals and deleting the adapter would finish the
  job.

- **The polarity split is still available if disposition grows.** `Disposition`
  mixes stable endowment (the two dials, from `PsychVector`) with volatile
  per-tick state (`helpless`, `awake`) — a considered-and-set-aside decomposition
  (a stable `Psyche` vs a volatile state struct). Fine at four fields; if the
  reserved engines add a fifth or sixth disposition input, revisit whether the
  bundle wants splitting along that axis.

## Followups

None opened — this campaign *closes* a followup rather than opening one. The
reserved engine seams (PSY-9 chronobiology, PSY-10 trophic/predation, PSY-11 fear,
PSY-12 affiliation) are the untouched content backlog, and each is now cleaner to
build atop a composed `arbitrate`.

## Confidence Gradient

Checked `open-questions.md`: a pure refactor in the cognition/drive layer, moving
no bet. No re-score.
