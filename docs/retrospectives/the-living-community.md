# Retrospective — The Living Community

One page of process, not product. The product — history-first placement, the
commit-skeleton/derive-flesh persistence model, separated territories, the
impression — is chronicled. This is what the close learned about *how the
campaign was run*, on what turned out to be the highest-uncertainty engine work
the project has attempted.

## What went well

- **The annotation-vs-spine reframe happened before any code.** The first
  framing of the campaign was an annotation view: place the world as before,
  then narrate a history over it. Nathan's worry about the *long-term
  usefulness of the data* reshaped the persistence model at the design gate — an
  annotation moves neither the census nor leaves a doll, so the whole thing
  inverted to history-*first* placement with a committed skeleton. The
  overturn (materialise-present/replay-past → commit-skeleton/derive-flesh) came
  out of a single ideonomy pass that found the keystone by negation:
  displacement-coupling means the past is *not* locally replayable. Getting the
  spine right at G2 saved rewriting the engine at G5. The cheap place to change
  a design is before it exists.

- **Measure-don't-narrate, applied repeatedly, paid off every time.** Three
  separate honest measurements, none of them the predicted result: the
  territory metric was caught as *vacuous* (a Jaccard over one-settlement cells
  that is structurally always zero) and repaired to region-of-influence before
  it could ship a test that asserts nothing; the stratigraphy hypothesis was
  *falsified* (depth correlates negatively with capacity, not positively) and
  frozen to the measured sign rather than flipped to save the prediction; and
  the "no fresh dolls" finding was *measured*, not assumed — the youngest ruin
  is 250 years old because the climate pulses are millennial. Each is a
  discovery precisely because the gate measured reality instead of narrating the
  mechanism the plan expected.

- **The fidelity carve-out discipline worked as intended, five times.** A
  high-uncertainty campaign generated five mid-execution decisions that only
  Nathan could make, and each was brought to him rather than resolved silently:
  displacement-is-migration-not-raids (defer conflict to the criticality
  campaign); freshwater-weighting in the genesis seeding (restore the shipped
  near-river property rather than lower the gate); calibration re-derivation to
  the collapse ceiling (a units mismatch, re-derived from the model's constants,
  not fit to the measurement); per-world name uniqueness; and the
  durability/impression reframe (ancient ruins leave durable traces). The
  carve-out is not overhead — on a campaign where the world's *behaviour* is the
  unknown, it is the mechanism by which fidelity judgments stay Nathan's.

## What the campaign taught mid-execution

- **Post-data gate amendments must be labelled everywhere, at the moment.** Two
  preregistered gates were amended after seeing the data: migration replaced
  raids as the volume signal, and the stratigraphy correlation flipped from a
  predicted positive to a measured negative. Both are honest — the world does
  what it does — but preregistration honesty requires the amendment be visible
  in the source, the report artifact, and the chronicle, labelled as *amended
  after unblinding*, not quietly re-pinned. Done here; worth stating as the
  standing rule.

- **A genesis epoch's fallout sweep is broad, and golden-pin discipline carries
  it.** Because placement now originates in history, every world's ledger
  changed, and the sweep touched roughly two dozen clean re-pins plus judgment
  tests, the calibration invariant, and every regenerated artifact. Re-pinning
  *in the drifting commit* (never deferring the pins to the close), and
  re-deriving the calibration bound from the model's own constants rather than
  fitting it to the new numbers, kept the epoch legible. The census regen and
  keystone refreeze are the one deliberately-deferred piece — they belong to the
  controller's merge step on the canonical box, not to a mid-branch task.

- **One subagent parked on a backgrounded gate.** An implementer started a
  long-running gate in the background and ended its turn to "wait," which the
  dispatch contract forbids — the recovery was to foreground-poll it to
  completion. The standing rule (run long gates in the foreground, blocking)
  exists exactly to prevent a turn ending on an unobserved command; the lapse
  cost a round-trip, not correctness, but it is why the rule is verbatim in every
  brief.

## A follow-up worth promoting

The magic-string seed-label footgun recurred here (a bare string literal at a
`.derive(...)` / `register_predicate(...)` site is a byte-identity contract
spelled as an unchecked string — the type-audit disease, for labels). The cheap
half — a workspace grep-gate that default-denies bare literals at those call
sites — is worth folding into a near-term hardening pass; the durable half — a
`StreamLabel` newtype in the kernel so a bare literal becomes a compile error —
is its own cross-cutting campaign. Captured so the next recurrence closes it
instead of re-noticing it.
