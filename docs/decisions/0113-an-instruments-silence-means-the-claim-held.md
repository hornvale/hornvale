# 0113. An instrument's silence means the claim held, and nothing else

**Status:** Proposed (2026-08-09) · **Decider:** Nathan · **Relates to:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0032](0032-calibration-loads-the-census-fixture.md)

In the context of a detector that checks declared claims against measured
data, facing the fact that *nothing reported* was carrying two different
meanings, we decided that **silence means exactly one thing — the claim was
tested and held** — and that every other outcome is given its own named
verdict, accepting that the taxonomy of verdicts grows over time.

**Context.** The Armature declared thirty causal links across the census and
measured them once. Two of its findings were about the instrument rather than
the world, and both are the same shape: an outcome that is *not* "the claim
held" was being rendered as silence, or as a partial report indistinguishable
from a complete one.

- Six declared links have a constant input column — one distinct value across
  a thousand worlds — so no correlation exists to compute. The correlation
  function's zero-variance guard was defeated by summation residue and
  published a number computed from noise; the obvious repair, returning
  nothing, would have made those six rows silent, converting a visible wrong
  answer into an invisible one. Silence there would have read as *these six
  claims held*, which is the opposite of the truth.
- Three links measured a sign opposite the declared one, at a band that also
  differed. The strength branch withheld the sign, three lines below a comment
  stating that the sign is withheld only "when nothing was measured", so a
  measured `r = −0.755` against a declared positive link rendered as a bare
  magnitude.

**Decision.** Two rules, one principle.

1. **An untestable claim reports its untestability.** When a declared
   relationship's inputs cannot support the measurement, the detector emits a
   distinct verdict (`D5 unmeasurable`) naming which side is frozen and at what
   value. It does not fall silent and it does not report a number.
2. **A measured sign is never withheld.** Suppression is reserved for the
   quantity that was genuinely not measured — the sign of a near-zero `r`,
   which is noise. Anything the instrument computed, the instrument reports.

The principle beneath both: a reader who sees nothing must be able to conclude
one thing. Any outcome that would otherwise share silence's channel needs a
name of its own.

**Consequence.** The verdict vocabulary grows rather than staying fixed, and
each addition is a rendering change that moves committed artifacts, so adding
one is a drift-checked act rather than a quiet one. Rule 2 also means a
strength finding can carry a second, independent defect in its detail text
(wrong band *and* wrong sign) without being promoted to the more serious
verdict — deliberately left as-is, because changing that taxonomy after seeing
which rows it would move is the phase-order violation decision 0016 exists to
prevent. That question is open, registered, and belongs to a campaign that
preregisters it.

**Cost accepted.** Distinguishing outcomes costs branches, and a branch that
cannot fire on today's data is a branch that has to be justified rather than
trusted — the correlation function's retained `var <= 0.0` backstop still
routes to silence and is unreachable, which this record names rather than
resolves.

**See also.** [The Armature](../../book/src/chronicle/the-armature.md);
[the retrospective](../retrospectives/the-armature.md);
`windows/lab/src/domesday/detect.rs`.
