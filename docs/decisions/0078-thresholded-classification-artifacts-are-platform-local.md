# 0078. An artifact dominated by thresholded classifications is drift-checked platform-locally, not in CI

**Status:** Accepted (2026-07-26) · **Decider:** Nathan · **Refines:**
[0033](0033-quantize-serialized-floats.md),
[0063](0063-census-regen-is-local-again.md)

In the context of CI's committed-artifact drift check, facing the question of
which artifacts can be compared *across platforms*, we decided that **an
artifact whose content is dominated by thresholded classifications is excluded
from the CI diff and pinned by a platform-local golden instead** — accepting
that those artifacts get no cross-platform drift signal at all.

**Context.** Decision 0033 makes serialized floats byte-identical across
platforms by quantizing at every emit boundary. That works for *quantities*.
It does not work for **classifications**: a biome, a water kind, a relief band
is chosen by comparing a float against a threshold, and two platforms whose
libm differs in the last ULP can land on opposite sides of that comparison.
Quantizing after the comparison cannot undo a branch already taken. Decision
0063 makes this box the sole golden-authoring platform; CI runs on ubuntu.

The exclusion list had grown by precedent rather than by rule — the PNG maps,
`scene/tiles`, `scene/tiles-region`, `locale-seed-42.json` — and *The Purview*
shipped an artifact that plainly belonged with them and was strictly checked
anyway, because nobody had written the rule down.

**The rule.** *Exclude an artifact when classifications are what it is; keep it
when classifications are incidental to a broader signal.*

- **Excluded**: the scene tile documents, `locale-seed-42.json`, the surrounds
  document, and the generated ASCII chart snippets. Strip the classifications
  out of any of these and almost nothing remains.
- **Kept**: the possession transcripts, the almanac, the censuses. These *do*
  contain classification-derived text — a transcript names its biome in prose —
  but they are dominated by verb behaviour, narration, ledger contents, and
  numbers, and that is a drift signal worth far more than the classification
  noise costs. They carry a known, accepted flake risk on a foreign platform.

**What still guards an excluded artifact.** Not nothing, and this is the point:
byte pins under the producing crate's `tests/fixtures/`, run in the ordinary
commit gate on the authoring platform. Those catch a *meaning* change — a
reordered field, a renamed noun, a moved band boundary — which is what the CI
diff was actually protecting. What is given up is only the cross-platform
comparison, which was never sound for this class of data.

**Consequence.** Any new artifact must be placed on this axis when it is
committed, not a campaign later. An artifact that is mostly classifications and
has no platform-local byte pin is the shape to refuse: it neither gets the CI
signal nor has a substitute. The known cost is that a classification could flip
on this box and be caught only by the local pin — acceptable, because the local
pin runs on every commit and the box is canonical by 0063.

Ratified at *The Margin*'s merge gate, closing the "record the drift-check
decision" followup *The Purview*'s retrospective raised.
