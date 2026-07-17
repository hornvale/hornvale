# Retrospective — The Dissolution (ECS Campaign 3: dissolve `SpeciesDef`)

Process lessons, not product. The product is in the chronicle.

## What worked

- **Tight byte-identity checkpoints for a delicate endgame.** The "delete
  `SpeciesDef`" work looked like one task; it was five (thread the component set,
  migrate the build API, relocate genesis, migrate the residual readers, delete
  the struct + invert authoring). Splitting on the byte-identity boundary — every
  commit bit-identical, gated by the seed-42 world and the drift-check — meant
  each risky move was isolated and independently reviewable. No drift ever
  reached a second commit.

- **The equivalence-shadow as a standing guardrail.** A cross-crate test in
  `worldgen` (the one window that may depend on two domains) compared the new
  component registries against the old god-struct, field-by-field, at every step
  until the god-struct was deleted. It turned "did the transcription of 40+
  authored constants into `language` preserve them?" from a hope into a passing
  test. When it was later retargeted (the second source was about to vanish), the
  byte-identity oracle inherited its job — the right hand-off.

- **Opus for the byte-identity-sensitive integration tasks.** The endgame tasks
  (threading the component set through a deeply-woven build pipeline, relocating
  the genesis fact stream) each landed byte-identical in essentially one shot.
  The cost of the more capable model was repaid in fix-waves not spent.

- **The two-tier review earned its keep.** The final whole-branch review found a
  cross-cutting seam — the composed `family_proto` store was *stored* but never
  the authoritative *read* path — that no per-task review could have seen, because
  the gate and the value it guarded lived in different functions touched by
  different tasks. Byte-identical today; a latent split-brain for a later
  campaign. Per-task review + whole-branch review are not redundant.

## What to carry forward

- **Ground the model in *every* caller, not the canonical path.** The plan
  assumed `worldgen` would reassemble a kind from the canonical registries; the
  laboratory's synthetic rosters (a renamed goblin, a tonality-forced "serpent")
  broke that on the first integration task. The correction — *the roster is a
  component-set*, built from whatever is handed in — was more faithful to the
  thesis, but it should have been found at spec time by asking "who else calls
  the build, and with what?" A code sweep of build-API callers belongs in the
  spec's grounding, not the third task's surprise.

- **The type-audit class list is a recurring plan trap — mechanize the check into
  the brief.** Task 1's plan text specified `bare-ok(container)`, not a real
  audit class; it broke CI's own gate. This exact lesson is already recorded in a
  prior campaign's ledger. The plan's tags are *unverified suggestions*; only
  `type-audit check` is authoritative. Every implementer brief now says so, but
  the durable fix is a note in the writing-plans step: never emit a `type-audit:`
  tag in a plan without noting it is unverified.

- **Audience-check gate labels before sending.** The first scope question to
  Nathan used internal vocabulary ("normalization", "ComponentStore",
  "referential integrity") he could not parse. The ELI5 re-ask (index-cards in
  folders) landed immediately. The decision content was right; the framing was
  for my own reasoning, not for the reader. Read a gate question back as the
  recipient before it goes out.

## The one missed cadence (recorded per the close discipline)

The **stage-boundary main-absorption cadence was not held.** Main was absorbed
once, at execution start; then ten tasks and several fix-waves ran across a long
execution while main moved **twenty commits** (two parallel campaigns —
The Region, The Lens — plus a census regen). The close preflight caught it, and
the absorption was clean (one generated-file conflict, the lab calibration tests
auto-merged). It even paid a dividend: main's parallel census regen cleared the
32 census-schema reds this campaign had been carrying as "deferred," so the
merged gate came up fully green (1236/1236) rather than green-modulo-census. But
that was luck, not discipline. A mid-execution absorption at a plan-stage
boundary would have surfaced main's census regen — and any semantic collision in
the shared lab calibration tests — while the context was live, instead of at the
close. For a long, many-task campaign, "absorb at every stage boundary" means
*during* execution, not only at its ends.
