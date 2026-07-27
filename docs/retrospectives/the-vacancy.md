# Retrospective — The Vacancy

Process lessons only; the product story is the chronicle. A campaign framed as
coverage of the model's declared state space: an instrument, two supply axes,
thirteen fauna, and a fifth people.

## What worked

- **Building the instrument before the content, and pinning it drift-free.**
  Stages 1 and 2 changed no behaviour by construction, so when stages 3–4 moved
  numbers there was never a question of whether the instrument had been shaped
  to fit the roster. The occupancy readout found four ghost kinds on its first
  run — a defect that had been on main for campaigns, invisible because nothing
  had ever asked where each kind actually lives.
- **A refusal is cheaper than a witness and outlives the campaign.** The
  non-void test is a few dozen lines, runs in the commit gate, and converts a
  whole failure class from "found by hand months later" into "fails on arrival."
  Every kind authored afterward was validated by it before its doc comment was
  written.
- **Deleting the allowlist as the proof.** The non-void test shipped carrying an
  explicit allowlist of the four known-void kinds, and the task that fixed them
  deleted it. A test that passes while carrying an allowlist proves nothing
  about the kinds on the list; making the deletion the exit criterion made the
  fix self-demonstrating.
- **Refusing to tune toward a preregistered criterion.** The gnoll was authored
  for hot-arid desert and landed with zero desert occupancy. Leaving its niche
  alone and shipping the criterion as a failing, named test is the only version
  of that result worth anything.

## What bit, and the lesson

- **I stopped reading one sentence early, and it cost the campaign's scope.** The
  sea was scoped out on the grounds that the land mask zeroes all supply on
  submerged cells. The very next sentence of that doc explains that an aquatic
  kind arrives by adding a marine axis, not by an exemption — and two other
  docs say the same. Nathan's "why aren't we creating any aquatic species?"
  is what caught it. **A blocking fact needs re-verifying at the blocker,
  not recalled**, and the specific failure mode is stopping at the sentence that
  confirms the prior.
- **My drift check could not detect drift.** Every stage exit condition said to
  run `make gate` and then check `git status` for artifact drift. `make gate`
  never invokes the artifact regen, so nothing rewrites the almanacs, maps, or
  studies, and a clean status means only that the tree was already clean. **Two
  stages were signed off with that procedure** before a reviewer noticed. The
  conclusion survived — the regen was clean when actually run — but it was true
  by luck, not by the cited evidence.
- **A confident causal story passed its own gate.** A task reported that the prey
  field moved seed 42's settlements from 81 to 203, with a detailed mechanism
  about peoples' prey weights gaining supply. Measured at three points, the count
  was 203 throughout; 81 was a stale number in a comment, and the mechanism does
  not exist because the per-axis model is not the settlement provider. **The check
  that caught it was measuring the claimed before-value**, not re-reading the
  reasoning — the reasoning was perfectly plausible.
- **A test's value is what it fails on, and neither the implementer nor I
  checked.** The stage-2 byte-identity test passed while the axis was inserted
  mid-slice *and* while it was prepended. The reviewer mutation-verified and
  found it caught nothing. Worse, the doc comment justifying the rule gave the
  wrong reason — float non-associativity, which is false for a zero-weight axis;
  the real mechanism is positional tie-breaking. An author trusting that
  rationale would have checked ULPs, found nothing, and shipped the reorder.
- **Hand-maintained complements of the roster rot silently.** The potency assay's
  mundane list and the solitary-tongue golden's peoples list were both hardcoded,
  and both silently stopped covering the kinds this campaign added — while still
  passing, because they still covered the kinds they named. The potency one is now
  derived from the registry; the lexicon one is documented as a known ceiling.
- **I clobbered my own uncommitted work twice** with `git checkout -- <path>`
  while reverting a mutation-test probe. Commit real work *before* mutating,
  every time.

## Process gaps to close

- **Two censuses raced on the canonical box.** Another campaign regenerated the
  census on `lefford` within the hour, re-pinning the same four calibration
  files. The lock serialized the *runs* correctly, but nothing prevented two
  campaigns from each producing a census that described neither merged tree. The
  resolution was to take main's as a coherent base and re-run at the merged SHA
  — which is correct but expensive. A guard worth considering: refuse a census
  regen whose ref is not an ancestor-or-equal of `origin/main`'s tip, or record
  the SHA a census was produced from alongside the goldens so a race is visible.
- **The `golden-pins.sql` tripwire earned its keep twice**, catching that a census
  re-pin touches four files when I had done three. It is the only reason the
  SQL and Rust pins did not silently diverge. Worth knowing it exists *before*
  the first re-pin, not after.
- **The plan under-specified `make gate` for non-gate tasks.** Task 3 shipped two
  non-canonical `heavy:` ignore reasons because its plan steps never asked for a
  full gate, and its reviewer was told the implementer's targeted run was the
  evidence. Task 4 found it. Any task that adds an `#[ignore]`d test touches a
  convention that only the full gate checks.
- **Stage-boundary absorption was skipped.** The branch met main once, at close,
  by which point main had moved 70 commits and then 12 more mid-close. It
  merged cleanly both times, but the semantic collision underneath — a
  concurrent campaign ruling that every biosphere kind owes a name, while this
  one added thirteen kinds — only surfaced because that campaign had shipped a
  tripwire. CLAUDE.md's cadence exists for exactly this.

## Carried forward

- **BIO-46** — supply magnitude drowns the condition niche; the named prerequisite
  for this campaign's one unmet criterion, and the reason "centred on a biome" is
  not currently expressible.
- **BIO-47** — insolation is latitude, not light.
- **BIO-42** — `Autotroph` is witnessed but computed as an endotherm; deliberately
  not bundled here, so its fix gets clean attribution against a frozen roster.
- **BIO-48** — the walk layer's drive vector assumes every creature is a land
  animal; a prerequisite for an aquatic people rather than merely aquatic fauna.
- **BIO-43 / BIO-44 / BIO-45** — a kind is authored in six registries and validated
  in a seventh; trophic structure within the sea; chemotrophy as a fifth
  metabolic class.
