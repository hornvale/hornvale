# 0797. Commit the integral, never the path

**Status:** Accepted (2026-09-05) · **Decider:** Nathan

In the context of a deep-history bake that keeps a community's founding year,
its ending year and its peak population but no population trajectory, facing a
consumer that needs to weight a draw by how many people actually lived where
and when, we decided that **a live quantity's tenure-integral is committed as
one functional Number fact per record — `occ-person-years` — and the
trajectory that produced it is not** — accepting that any consumer wanting the
shape back must reconstruct it from the endpoints, the peak and the area, and
that the reconstruction is an approximation the reader is told about.

**Context.** `Occupation::delve_depth_m` had already made this argument for
itself, in its own doc comment: "the working is the integral of a live quantity
over a tenure … the ledger keeps neither trajectory … so the field commits
exactly the half nothing can re-derive." The Lot needed the same half for
population. The alternative shapes were priced and rejected: a per-epoch series
is on the order of twenty thousand Number facts per world for a quantity whose
median tenure is two epochs, and a path nobody else reads; `tenure × peak` as a
proxy is biased in exactly the direction that matters, over-weighting every
community that died before it ever reached its peak, and the probe measured a
tenth of all occupations opening and closing inside one epoch. See ledger #2.

**A committed integral carries its sampling convention, and that half must be
stated with the fact.** The first implementation credited person-years at each
site that opened or grew a community — six of them — and double-counted across
a same-epoch handoff: the epoch loop grows every living community first and
closes some of the same ones afterwards, so a community closed at year Y had
already been credited for the epoch beginning at Y, an epoch outside its own
tenure, while its successor opened at Y was credited for the same window. The
resolution is one sweep at the **end** of the epoch loop over every community
alive at that moment, and every per-site credit deleted (ledger #12). The
invariant this buys is exact and testable — a record's person-years are
`≤ (peak + ½) × epochs survived × epoch length` — and it decides the boundary
case in the open: an occupation that opened and closed inside a single epoch
carries `0.0`, because it was never alive at any sample point, and it contains
no whole year for a birth to sit in either.

**Consequence.** Every saved world's JSON grows by one fact per occupation, so
the byte-golden fixtures and the census regenerate; the fact is additive, draws
nothing, and moves no seeded value, so no epoch is taken. A world file written
before this campaign has no such fact, and the lot refuses it by name rather
than approximating. Between records the draw weight is now exact; within one it
is a rise-then-plateau fitted to the committed area and clamped so it can never
exceed the committed peak (ledger #15), and the payload names which of the
three shapes was used. That clamp is the standing cost of not keeping the path:
the ledger cannot distinguish a community that opened with eight settlers from
one that opened with the survivors of a conquest, so the reconstruction states
its assumption rather than pretending to a resolution it does not have.

**See also.** Spec
[`2026-09-05-the-lot-design.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-05-the-lot-design.md)
§4.1; ledger
[`2026-09-05-the-lot.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/ledgers/2026-09-05-the-lot.md)
#2, #12, #15; `Occupation::delve_depth_m` in `domains/history`;
[0001](0001-determinism-is-constitutional.md);
[The Lot](../../book/src/chronicle/the-lot.md).
