# Retrospective — The Cloister (Dragons program, campaign 4)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## The headline: The Eremite's blast-radius lesson was operationalized — and it worked

The Eremite's retro named the lesson in blood: *when you reshape a shared type,
the blast radius is every reader, not the gates the spec re-keys — so enumerate
the consumers at spec time.* The Cloister was the first campaign to apply it as a
method rather than learn it as a wound. The spec's §4 built a consumer table
up-front from a grep of every `PsychVector` reader, classified into five buckets
(type-rename-only / pure-individual / pure-society / mixed / Settled-gated). The
payoff was a **non-surprising** campaign: the implementer found a handful of
sites the table hadn't named (`social_form.rs`, `lab/roster.rs`, three
filtered-"peopled" test fixtures), but every one was the *same class* already in
the table — not a new directory or a new shape of reader. The prior campaign's
whack-a-mole became this campaign's checklist. The lesson generalizes: a
consumer-enumeration at spec time doesn't have to be complete to pay off; it has
to be complete *by class*, so the surprises are instances, not categories.

## Byte-identity: predicted as a verification, confirmed cleaner than the prediction

Autopilot's "verify generated-artifact claims, don't assert them" discipline was
honored: the spec said the catalogue was *predicted* byte-identical and named the
regen+diff that would *test* it, rather than asserting it. Task 3 ran the test
(clean). And the mechanism was cleaner than first stated — the split is fully
byte-identical because the dragons' one non-baseline society dim (`in_group_radius`
0.05) is read by *no* unplaced-path consumer; only `chorus` stance reads it, and
that runs over placed peoples. That fact was *found by tracing consumers*, not
assumed — the same method as the headline, one level down.

## Two disciplines from prior campaigns held

- **The atomic-commit constraint** (The Solitary Tongue: a shared-signature
  change can't commit alone under the workspace-wide clippy hook) shaped the task
  split directly — Task 1 a pure byte-identical rename, Task 2 the atomic
  extraction, each compiling workspace-wide in one commit. No mid-tree-broken
  commit, no `cargo build` blind spot (clippy `--all-targets` was the gate).
- **type-audit field verdicts on the struct doc** (The Solitary Tongue) — the new
  `MindVector`/`SocietyVector` `bare-ok(ratio)` tags went on the struct docs, not
  per-field. No re-learning.

## What the close itself caught

- **An "out-of-scope" stale comment became an active contradiction once the diff
  sat on top of it.** The final review flagged that the campaign added a correct
  new comment directly above a pre-existing stale one ("skip the minded
  solitaries (dragons)" — false since dragons became speakers in The Solitary
  Tongue). The plan had explicitly deferred the stale line as not-ours. **Lesson:
  "pre-existing, out of scope" stops applying the moment your edit sits adjacent
  to it — an adjacent contradiction you introduced is yours to fix.** Fixed inline
  at close (comment-only, byte-irrelevant).
- **The committed type-audit report lagged main.** Regenerating it at close swept
  up terrain/worldgen tag deltas from *other* campaigns that had landed without a
  report regen — benign (the report is now current, and the check is in the gate),
  but a live instance of the known "committed-report-freshness can still slip"
  gap. The report's own narrative briefly mis-attributed those deltas to this
  campaign; the reviewer caught it.

## A note on sequencing judgment

This was the first Dragons-program campaign that was the *enabler*, not the
payoff. Autopilot's ideonomy pass recommended Perception-next (Placement as the
capstone); Nathan overrode toward doing this architectural split *first*. The
pass had surfaced the framing that made the override legible: on the
*materiality* axis Placement is the only threshold-crosser, and on the
*reversibility* axis it is the only census-spending, hard-to-reverse campaign —
so doing the reversible, census-free cleanup first lands the eventual irreversible
payoff on a *complete* creature and avoids a double census. The enabler-before-
payoff ordering was a deliberate, argued choice, not a default.

## Follow-ups (promoted from the campaign register)

- **✅ Split `PsychVector`** — this campaign. The Eremite's Nathan-endorsed
  follow-up is done (as `MindVector` / `SocietyVector`, decision 0067).
- **Dragon perception** (next): replace the goblin-baseline exposure stopgap in
  `exposure_of_impl` with a real dragon perception — reusing this campaign's
  baseline-fallback pattern, one tier up the lattice.
- **Placement** (BIO-35 Stage 2 / the `ANIMAL_PREY` prey field): the payoff.
  Scope and *measure* it by the **landscape-of-fear cascade** (regions peoples
  avoid, settlement pattern bending around apex predators), not an occupant
  token; census-regenerating (a carve-out). Fix the ~7 placement-gated
  `.expect("peopled pass over a fauna kind")` sites with baseline fallbacks then.
- **Roster & Ages** — fill the sociality × lifespan periodic grid (BIO-37's
  roster half); best done with/after Placement so new kinds actually appear.
  Per-chromatic Draconic differentiation fits here.
