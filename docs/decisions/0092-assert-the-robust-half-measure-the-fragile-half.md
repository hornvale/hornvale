# 0092. Assert the robust half in the gate; measure the fragile half in the census

**Status:** Accepted (2026-07-30) · **Decider:** Nathan · **Relates:**
[0016](0016-preregistered-measurement.md),
[0073](0073-epoch-granularity-is-declared.md),
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md),
[0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)

In the context of *The Contour* moving another campaign's shipped invariant by
five rooms — and of the Confidence Gradient already carrying sixteen documented
instances of the *opposite* failure, checks that cannot fire — we decided that
**a claim's fragility, not its subject, determines which instrument enforces
it**: robust claims are asserted in the commit gate, and claims near a
threshold are measured as rates in the census.

## The failure this exists to prevent

`hornvale-lab::hearth_population_calibration` asserts that at least one seed is
cold-**dominated** (`cold*2 > built`), measured over 15 seeds. *The Hearth* had
already rewritten that test once — from per-seed value pins to invariants —
after absorbing main broke it, and its comment says plainly that *"a pin that
reddens for [upstream drift] is measuring the wrong thing."* The rewrite was
the right move and it landed on a claim that is **formally an invariant and
behaviourally a value pin**:

```
seed 13, the only seed near the bar:
  baseline    107 cold / 188 built = 56.9%   dominated ✓
  The Contour  97 cold / 199 built = 48.7%   dominated ✗   — five rooms
```

Totals moved +2% built and −23% cold-built, redistributed across seeds. Cold-
built settlements remained common by every other measure in the same test:
167 rooms across 7 of 15 seeds. **Only the existence claim flipped**, and it
flipped because an existence claim over 15 draws is decided by whichever single
world happens to sit nearest the threshold.

## The distinction

| pin kind | fails on MY true change | fails on ANOTHER campaign's true change | reports a number |
|---|---|---|---|
| value pin / golden | yes (correct) | yes (noise) | no |
| robust invariant | no | no | no |
| **existence claim near its threshold** | **maybe** | **maybe** | **no** |
| rate claim with a sampling bound | no | no | **yes** |

Row three carries **a value pin's noise profile with an invariant's
authority**, which is the worst available combination: it fires when nothing is
wrong, and its label discourages anyone from asking whether it should have.

The diagnostic question is **naturalness** — is the claim a fact about
Hornvale's physics, or about the current parameterisation? Split *The Hearth*'s
test and its two halves land on opposite sides:

- *"Cold-built settlements occur"* — physics. Survives any mechanism change.
- *"Some seed exceeds 50% cold"* — parameterisation. A statement about where a
  distribution's tail happens to sit, which no physical law protects.

Relabelling the second does not make it an invariant.

## The decision

1. **The commit gate asserts only claims robust to another campaign's true
   change.** If a plausible, correct change elsewhere in the world can turn a
   gate assertion red, it does not belong in the gate.

2. **A claim near its threshold is measured in the census as a rate with a
   sampling bound, not asserted anywhere.** At n = 15 the only sayable claim is
   *does one exist* — brittle by construction. At the census's n = 1000 the
   sayable claim becomes *what fraction*, with a confidence interval, and a
   movement is significant or it is not. **The sample size does not merely
   improve the test; it changes what kind of claim is available.**

3. **Both instruments, different jobs — never the same claim in both.** A
   duplicated assertion pays the gate's tax and the census's tax to answer one
   question, and the two copies drift.

4. **A census-measured claim must have its generator paired with its
   verifier.** `census-check` is not in `make gate`; *The Siding* found the
   census stale for **139 commits** while every gate ran green. An unpaired
   census claim scores as unchecked no matter how large its sample — this is
   the Confidence Gradient's existing floor, restated where it bites.

5. **Safety-critical invariants stay in the gate regardless of sample size.**
   The census is refreshed once per campaign at the close, so it gives feedback
   at merge, not while iterating. Determinism, layering, and save-format
   contracts are gate business and always will be.

## Consequences

- **`golden-pins.sql` is this practice's ancestor and should be brought under
  it.** It already recomputes pinned constants from the committed census as
  *"an independent second path from fixture to pin"*, and its header already
  distinguishes, by hand, *"a re-pinned measurement, not a broken invariant"*
  against a directional sampling bound. What it lacks is the formal split: it
  is a flat list of literals, with a documented history of silently missing
  every re-pin since its creation. Rows that are rate claims should carry their
  bound; rows that are exact contracts should say so.
- **This is the mirror of the never-firing check**, and the Gradient should say
  so. Sixteen instances of checks that could not fire are recorded there, with
  mutation testing as the practice that catches them. Nothing there addresses
  checks that fire when nothing is wrong. **Both end in a green suite nobody
  believes** — one by training you to trust a check that cannot speak, the
  other by training you to silence a check that cries wolf. [The Named]'s
  "drift checks freeze bugs" is the third corner of the same triangle.
- **A campaign-scoped claim enforced repo-wide is a collision generator.**
  Every campaign that moves the bake pays a tax to every threshold anyone has
  ever pinned against it, in the most corrosive currency available: a red suite
  that is correct to make green.
- **This does not license relaxing a threshold to clear a red.** Moving a bar
  to make a failure go away erases the finding. The sanctioned moves are to
  widen the sample, to convert the claim to a census-measured rate, or to
  report the invariant as genuinely no longer holding. Which of those applies
  is a judgment, and it is the claim-owner's.
- **Not retroactive.** Existing tests are not swept. This binds new assertions
  and any test being revised because a campaign moved it.
