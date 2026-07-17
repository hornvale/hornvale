# The Terminator — regime-aware insolation (SKY-24)

> **Status: SHIPPED** (habitability fix; religion payoff banked as
> follow-up, 2026-07-16). The shared `substellar_cosine` helper and the
> `Locked` insolation branch merged as planned, spinning-world
> byte-identity held, and no roster re-tune was needed (measured, not
> guessed). The acceptance battery measured the campaign's own
> preregistered payoff — §8's Ambient-recovery prediction — and it was
> FALSIFIED: 0/9 locked worlds head Ambient post-fix, unchanged from the
> pre-fix 0/48, because presiding-belief selection is gated by a second,
> independent mechanism (species-registry order + the founder-floor
> flagship guarantee) the insolation fix never touches. Nathan's ruling:
> ship the correct habitability fix, bank the presiding-belief fix as
> SKY-25. Chronicle: [`the-terminator`](../../../book/src/chronicle/the-terminator.md);
> retrospective: [`the-terminator`](../../retrospectives/the-terminator.md).

**Campaign:** the-terminator · **Registry:** SKY-24 · **Relates:** SKY-5, SEQ-1
**Origin:** the ambient-extinction investigation during rift-and-fit
(2026-07-16); reports in `.superpowers/sdd/`.

## 1. The bug, precisely

The habitability substrate's `insolation` field is **rotation-regime-blind**.
`windows/worldgen::substrate_field` computes it as
`annual_mean_insolation(latitude, obliquity, scalar)` — a function of
**latitude only**, with no `RotationRegime` branch and no longitude term.
Applied to a tidally-locked world, whose sky is organized around a fixed
substellar point (`+x`, spec convention: latitude 0 sits on the substellar
axis), this reads identical insolation at the scorched substellar point, the
frozen antistellar point, and the twilight terminator meridian at the same
latitude. **The field cannot see the terminator.**

Both other substellar-physics fields already do this correctly:
`domains/climate::temperature::mean_temperature` **and**
`domains/climate::moisture` each branch on `RotationRegime::Locked`, each with
its **own private copy** of `SUBSTELLAR = [1,0,0]` and the inline
`cos_theta = dot(p, SUBSTELLAR)`. Insolation is the **lone holdout** — the one
substellar-driven field that forgot to branch. The tell is that
`substrate_field`'s own test
(`substrate_field_is_finite_and_insolation_peaks_at_the_equator`) destructures
the regime as `_regime` and discards it. The response *curve* the field feeds
(`kernel::ecology`'s Gaussian tolerance, an interior optimum) is correct; the
input field is the defect. That the constant is already duplicated twice is
itself the warning the divergence class was left unguarded.

**Consequence (measured).** The Niche's insolation-driven settlement dominance
(`niche_per_species_k`) piles onto the false latitude-0 maximum — the
substellar point — so a locked world's flagship presiding phenomenon becomes
the fixed sun (Eternal sentiment) instead of the tide (Ambient). This silently
**masks** the shipped SKY-5 payoff: the drift study went from 19/23 locked
worlds heading the tide to 0/48 across the 2026-07-16 canonical census. It is a
bug, not a design reversal.

## 2. Scope and blast radius

- **Spinning worlds are byte-identical.** A spinning planet's annual-mean
  insolation genuinely is a function of latitude (averaged over the day), so
  the `Spinning` path is already correct and stays untouched, instruction for
  instruction. The majority of worlds (including most default seeds) do not
  drift.
- **Only tidally-locked worlds re-derive.** Their `insolation` field changes →
  habitability → settlement dominance → presiding religion. This is a
  save-format change **scoped to the Locked regime**, not a full-world epoch,
  and carries no terrain-epoch version bump (it is climate/habitability).
- **Determinism unchanged**: same seed + pins → byte-identical; the fix is a
  pure function of position and regime, no new draws, no wall-clock.

## 3. The correction

Extract the shared substellar geometry into one helper in `domains/climate`
(which owns the substellar physics and which `windows/worldgen` already
depends on — correct layering), and **dedupe all three consumers** onto it:

```
// domains/climate: one definition, one SUBSTELLAR constant
pub fn substellar_cosine(p: [f64; 3]) -> f64 { dot(p, SUBSTELLAR) }
```

`temperature` and `moisture` drop their private `SUBSTELLAR`/`cos_theta`
copies and call it (byte-identical — same arithmetic); insolation becomes its
third caller. After this, adding a substellar-driven field without regime-
awareness is not something a developer can silently do — the geometry has one
home.

`substrate_field` gains a `regime: &RotationRegime` parameter (already in hand
at its call site — `stellar_inputs` returns it, currently dropped) and computes
insolation per cell as:

- **Spinning** → `annual_mean_insolation(latitude, obliquity, scalar)`
  (unchanged).
- **Locked** → a substellar-cosine flux: on the day side
  (`cos_theta > 0`), `insolation_scalar * f(cos_theta)`; on the night side, a
  floor at (or near) zero. The exact day-side falloff `f` is chosen to mirror
  temperature's physical shape (temperature uses `powf(cos_theta, 0.3)`); the
  insolation flux is more nearly linear in `cos_theta` (flux ∝ cosine of
  incidence), so the spec's default is `f(cos_theta) = cos_theta` (Lambert's
  cosine law), with the exact exponent a Stage-0-tunable knob if the probe
  demands it. Night side reads `0` (no starlight term this campaign).

The corrected Locked range widens from the old `[~0.005, 0.32]`
(latitude-only) to `[0, insolation_scalar]`, and — critically — its moderate
values now live in the **terminator ring**, where the peopled roster's modest
optima (0.04–0.19) belong.

## 4. Why a shared helper, not a merge

Temperature and insolation are distinct physical quantities (an equilibrium
vs a flux) and keep their own formulas. What they share is the *geometry* —
which way the star is, and the day/night boundary. Extracting only
`substellar_cosine` (and the `SUBSTELLAR` constant, if not already shared)
captures exactly that commonality. A future third consumer (photosynthesis,
albedo, a phenomenon) inherits regime-awareness for free, and the class of
bug this campaign fixes becomes structurally unrepresentable.

## 5. Roster optima — measure before tuning

The peopled species' `condition_niche.insolation` optima were authored against
the old latitude-only Locked range. Under the correction an optimum near 0.15
sits at `cos_theta ≈ 0.15` — about 81° from substellar, deep in the terminator
band — so the optima **may already produce terminator-dwelling cultures with
no roster edit**. Per the banked-on-demand discipline (decision 0057), the
roster is **not** pre-tuned: Stage 0 measures where dominant-species carrying
capacity peaks on locked seeds under the fix; only if the optima demonstrably
miss the terminator are they re-tuned, and then from a sweep against the worst
seeds, never guessed.

## 6. Stage 0 — the preregistered probe (before the roster is touched)

A read-only instrument, run and written up before any roster change:

- On a set of locked-regime seeds, build worlds with the corrected insolation
  field and report, per world, **where the dominant species' per-cell K
  peaks** — substellar / mid / terminator / antistellar — and the resulting
  presiding-belief sentiment (Ambient / Eternal).
- Publish the locked-world Ambient-share distribution under the fix. This
  number sets the §8 acceptance floor (defensible, not guessed).
- If dominance still lands substellar (optima too high for the new range),
  the probe sweeps candidate optima / the day-side exponent and reports the
  minimal change that moves the peak to the terminator.

## 7. Pipeline touch points

- `domains/climate`: the new `pub fn substellar_cosine` + the single
  `SUBSTELLAR` constant; `temperature` and `moisture` drop their private
  copies and call it (byte-identical — same arithmetic; proven by equality
  tests on the existing temperature and moisture batteries).
- `windows/worldgen`: `substrate_field` signature (+`regime`), its call site
  (thread the regime `stellar_inputs` already returns), and the Locked
  insolation branch calling `climate::substellar_cosine`.
- Roster (`domains/species`): touched only if Stage 0 demands it (§5).
- No `niche_per_species_k` / coexistence edit (§9).

## 8. Acceptance

- **Ambient recovery (the payoff, preregistered):** locked worlds head an
  Ambient (tide) presiding belief at a rate recovering materially off 0
  toward the SKY-5 baseline; the exact floor is set by the Stage-0 probe's
  measured distribution before the fix ships, in the spirit of SKY-5's ~80%.
  Measured on the canonical census's locked-regime seeds.
- **Spinning byte-identity (hard invariant):** every spinning-regime seed's
  world is byte-identical pre/post fix — asserted directly (a spinning seed's
  serialized world compares equal), not merely inferred. A fix that drifts a
  spinning world is a regression regardless of the Ambient outcome.
- **Temperature & moisture byte-identity:** the shared-helper refactor leaves
  `mean_temperature` and the moisture field bit-identical (equality tests over
  the existing batteries) — the dedupe changes structure, never a byte.
- **Determinism:** same seed + pins → byte-identical, as always.
- **The two fields agree:** on a locked world the terminator is now both the
  insolation sweet spot and temperate (temperature reads ~0 °C there), so
  habitability concentrates there on both axes — a coherence check, not a
  hard gate.

## 9. Non-goals

- **The Niche is not reopened.** `niche_per_species_k` and the coexistence
  stack are correct consumers of a corrected input; no coexistence code
  changes.
- **No sun-as-tidal-source** (SKY-5's own open thread) and no night-side
  starlight insolation term — both adjacent, both out of scope.
- **No terrain / rift interaction.** Independent of rift-and-fit; disjoint
  fields. Both re-drift the census, so the later merge carries the combined
  regen (standing parallel-campaign discipline) — a sequencing note, not a
  dependency.
- **No new pins, no new crates, no HashMap/HashSet, no wall-clock**; kernel
  math for transcendentals; quantization only at emit boundaries.

## 10. Staging sketch (for the plan)

- **Stage 0:** the probe (§6) + the shared-helper refactor (byte-identical,
  proves the temperature equality). Measurement + safe refactor first.
- **Stage 1:** the Locked insolation branch; the Spinning-byte-identity and
  temperature-byte-identity guards; locked-world fixture re-pins in-commit.
- **Stage 2:** roster re-tune **iff** Stage 0 demanded it, from the probe's
  sweep.
- **Close:** locked-dependent re-pins, AWS census regen (Nathan-authorized;
  coordinated with rift-and-fit if both are near merge), chronicle, retro,
  SKY-24 status flip, Confidence Gradient re-score of the locked-world /
  SEQ-1 thread if it moves.

## 11. Decisions promoted from the ledger

Ledger: `.superpowers/sdd/decision-ledger.md`. #1 shared-helper regime-aware
insolation; #2 locked-world-only save-format scope; #3 probe-before-roster-
tune; #4 preregistered Ambient-recovery + Spinning-byte-identity guard; #5
fixes The Niche's input, not The Niche.
