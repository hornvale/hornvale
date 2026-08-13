# The defect ledger

One file per host, `defects-<host>.tsv`, appended by
[`scripts/defect-ledger.sh`](../../scripts/defect-ledger.sh) on every red gate.

**The question it exists to answer: has this test ever caught anything?**
Before The Sexton nothing recorded it. `docs/timings.md` had six RED rows and
each said only *that* it was red, so every "retire this test" or "run that one
less often" proposal rested on an opinion.

**Read it, do not gate on it.** Like `docs/timings.md` it is a record, not a
check. It is committed and per-host so `git log -p` is the archaeology.

**Known limits, stated so nobody over-reads a row.**

- `changed_crates` is a *correlate*, not a cause: it is the working tree's
  changed paths at the moment the gate ran, overapproximated to crate
  granularity. A test that fails for an unrelated reason still records
  whatever was dirty.
- A test that never appears here has not been *proven* useless. It may guard
  something no one has broken yet. Absence is weak evidence; presence is
  strong evidence.
- Only tests that run in `make gate` can appear. The heavy tier and the
  censuses are invisible to it.
