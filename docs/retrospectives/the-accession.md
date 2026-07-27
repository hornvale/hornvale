# Retrospective — The Accession

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-accession.md); the shipped
mechanism is the accession register and the epoch-first sort.

## What worked

- **Pricing the obvious repair before performing it.** The campaign's parent
  (The Actants) was about to spec "add twelve manifests with honest `Gap`
  lexemes" as the small safe change. Spiking it first — register the twelve,
  rebuild seed 42, diff the ledger — turned a one-line assumption into 70
  moved facts and a whole separate campaign. The spike cost about fifteen
  minutes. Every claim of the form "this should only touch X" in this session
  came from a command, not from reasoning, and the one time reasoning got
  ahead of measurement (the first `§3`, which called the cost inherent) it was
  wrong.

- **The owner's pushback was the highest-value review event.** "That should be
  purely additive, a patch not a major version" was a claim about how the
  system *ought* to behave, made without reading the code, and it was correct
  where the spec was not. The spec had measured the cost honestly and then
  concluded it was inherent — a diagnosis the measurement did not support.
  Measuring a cost and explaining it are different acts, and a green
  measurement can launder a wrong explanation.

- **Bisecting the perturbation instead of accepting the aggregate.** "Twelve
  concepts move 70 facts" invites the conclusion that each concept costs
  ~6 facts. One at a time: ten cost nothing, `treant` cost 5, `otyugh` cost
  65. The aggregate hid the actual mechanism completely, and the per-item
  breakdown pointed straight at collision-and-probe. It also handed the
  campaign its regression case for free.

- **The negative control caught a vacuous test before it shipped.** The
  insertion-stability test needed a witness that would actually collide. The
  first draft used a synthetic id in the periphery block; it passed, and it
  proved nothing — the universe was too roomy to collide at that position.
  The paired control (*the same insertion at epoch 0 must still displace*)
  failed immediately and forced a real witness. The two tests now differ in
  exactly one variable.

- **The parity check fired before its subject ran.** Three cohort literals had
  been wrapped across lines mid-token by the generator that wrote them, which
  Rust compiles happily into strings containing newlines. The check caught all
  three on its first execution. A guard that finds a real fault the first time
  it runs is a non-vacuous guard — the same signal The Correspondence recorded
  when its reconciliation found seven optimistic declarations on contact.

## What to carry forward

- **A test can be true and its implied guarantee false.** This is the
  campaign's central lesson and it generalizes past this repo. The existing
  test asserted insertion stability using a witness named `zzz-late` — chosen,
  reasonably, as "a new concept" — which happens to sort after everything.
  The assertion was true. The guarantee a reader took from it ("the registry
  can grow without reshuffling", in the test's own words) was false for every
  other position. **When a test's witness is drawn from the space of inputs,
  ask which region it came from and whether the failing region is
  reachable.** The existing memory note ("a passing test can pass by the wrong
  path") covers a neighbouring case; this is the sharper form — the test
  passes by the *right* path, from the only position where that path exists.

- **The defect class has other instances and nobody has looked.**
  `assign_proto_roots` was found by accident, during an unrelated campaign's
  spike. The shape is "a global ordered walk with rejection-probing, believed
  insertion-stable, ordered by a key that does not deliver it." Compound
  recipes, onomastics, and paradigm slots are all candidates and none has been
  checked. Registered as followup #4; it deserves a sweep, not a hope.

- **Fixing a silent-default bug with a silent default.** The repair's own
  failure mode — a forgotten cohort row reading as generation zero — is
  structurally identical to the bug it fixes, and was noticed only because an
  ideonomy pass on the *autonomy* axis asked what an authored table costs.
  Worth a standing habit: when a fix introduces a hand-maintained table, ask
  what a missing row does, and assume it will happen.

- **Stage-boundary absorption was untested this time.** `main` did not move
  during the campaign, so the first preflight was also the only one and it
  passed trivially. That is scheduling luck, not diligence — the same note
  The Correspondence's retro made. Nothing to fix here; recording it so the
  clean close is not read as evidence the cadence was exercised.

## Confidence Gradient

No re-score. Grepped `open-questions.md` for the campaign's territory
(determinism, byte-identity, save-format, lexicon): the nearest bet is *What
the world can already check itself on*, already at high confidence, and this
campaign is confirmatory rather than moving — one more instrument of a kind
the chapter already counts. Recorded per decision 0030 so the absence is a
finding, not an omission.
