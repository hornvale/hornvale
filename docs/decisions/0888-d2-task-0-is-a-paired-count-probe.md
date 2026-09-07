# 0888. D2 Task 0 is a paired count probe

**Status:** Accepted (2026-09-06) · **Decider:** Nathan ·
**Campaign:** The Staple D2 · **Relates:** [0826](0826-a-dynamics-probe-falsifies-on-a-count-against-an-existing-ceiling.md)

Task 0 compares same-seed control and treatment over 200 seeds. It counts
activation as seeds with at least one settled exchange and counts instability
as treatment-only breaches of each existing demographic calibration bar. The
dead poles are zero activation and more than half of treatment seeds outside
the existing bars. The bars are settlement count `40..=400`, collapse share
`0.05`, and alive-at-now `50`, copied from `history_tumult.rs`. Exchange
outcome rates remain descriptive.
