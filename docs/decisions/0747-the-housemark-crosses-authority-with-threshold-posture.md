# 0747. The housemark crosses authority with threshold posture, and unclassified radii refuse

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (G3 / autopilot) ·
**Campaign:** The Housemark · **Relates:**
[0067](0067-the-mind-society-vector-split.md) (the authored society vector),
[0104](0104-a-threshold-must-know-its-variates-distribution.md) (thresholds
name the distribution they interpret)

We decided that a `Housemark` has two independent fields:

- `Sociality::Hierarchic` maps to `AuthorityMark::Command`, while
  `Sociality::Communal` maps to `AuthorityMark::Common`.
- `in_group_radius` maps to `Inward` on `0.0..=0.35`, `Plain` on
  `0.5..=0.6`, and `Outward` on `0.65..=1.0`.

The open gaps and values outside `[0, 1]` **refuse with the original value**;
they are not rounded to a nearest band. Those bands are authored architectural
policy, not latent facts in the scalar, so a future society row in a gap must
reopen the choice explicitly. `status_basis` is not read: no direct spatial
consequence justified consuming it.

All fifteen present society rows derive successfully and populate all six
authority/posture combinations, but the implementation freezes the rule and
the coverage properties rather than a cast list of names.

**See also.** [The Housemark design](../superpowers/specs/2026-09-04-the-housemark-design.md)
§2 and H1; [campaign ledger](../superpowers/ledgers/2026-09-04-the-housemark.md)
#2.
