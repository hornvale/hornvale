# The Reservoir — decision ledger

Campaign: **The Reservoir** — one built world, drawn many times. Branch
`campaign/the-reservoir`, based on `25ee1d830`. Decision block **0606-0615**
(main ceiling 0585 at reservation).

Autopilot is engaged (G3 and G6 are the hard stops).

---

#1 [G1] — **Which lever against redundant world builds in the test suite.**

*Question.* The suite builds the seed-42 world many times over. Which lever:
in-process memoisation, a committed fixture read as an input, depth-scoping, a
lint, or a ratchet?

*Decision.* **Ratchet plus fixture, staged** — a source-scan ratchet that refuses
a new build site, plus a fixture-loading path for the dominant seed-42 identity;
existing sites migrate opportunistically rather than in one pass.

*Why.* Nathan's own call at the approach gate, against three stated
alternatives. It is also the reading the ideonomy pass produced: on the
*decomposability* axis the problem is fully decomposable (site by site, and 74%
of sites sit behind 70 helper functions), and on the *reversibility* axis both
halves undo by deletion — which argues for a ratchet rather than a migration.

*Alternatives discarded.* (a) Aggressive fixture-by-default — largest and
fastest win, but the largest single change and the broadest coverage residue.
(b) Ratchet only, no fixture — introduces no new fixture role and no coverage
argument at all, but leaves the measured 71 redundant seed-42 Full builds in
place. (c) Measure the suite-wide prize first — declined: the ~200x
load-versus-build ratio makes the fix worth doing at any plausible count, and
the enforcement half is independent of the count entirely.

*ideonomy passes / overturns.* 1 / 1 — the pass overturned the mechanism half of
the original proposal (see #2).

*Capture.* Spec §3; `.superpowers/sdd/decision-ledger.md`.

---

#2 [Q] — **Clippy `disallowed-methods`, or a source-scan ratchet.**

*Question.* Nathan proposed marking the build pathway deprecated and
allowlisting sanctioned patterns. Decision 0092 already does exactly that for
three derivation entry points via `clippy.toml`. Extend that list, or build
something else?

*Decision.* **A source-scan enforcement test** in `cli/tests/suite/`, roster in
`cli/tests/fixtures/`, checked in both directions. Not a `clippy.toml` row.

*Why.* Decision 0092's own ratified text records the trap:
`disallowed-methods` is **one lint with one on/off switch per scope**. A single
crate-level `#[allow]` there silenced all 24 platform-libm bans (decision 0041)
across worldgen — a constitutional determinism guard, disabled as a side effect,
caught only in review. 0092 carried ~31 production sites. This would carry
hundreds of grandfathered sites, and therefore hundreds of scopes in which the
libm ban *and* the weir both go dark. That trades a cost problem for a
determinism-guard hole, which is the wrong trade in this repository above all
others. `cli/tests/suite/test_binary_ratchet.rs` is the shipped idiom for
precisely this shape, and CLAUDE.md names `cli/` as the home of the
workspace-wide enforcement tests.

*And it answers 0092's own objection to a roster file.* 0092 argues "the
`#[allow]` attribute at the site IS the sanctioned-site list — one source of
truth, greppable, never a second document to keep in sync." The objection is
sound against a *unidirectional* list. Bidirectional checking dissolves it: a
stale row is an error, so the document cannot rot. That is exactly why
`test_binary_ratchet.rs` checks both directions and says so in its module doc.

*Alternatives discarded.* (a) The clippy entry, above. (b) An `#[allow]`-only
scheme with no roster — not a ratchet at all, because nothing then measures
whether the grandfathered set shrinks.

*ideonomy passes / overturns.* 1 / 1. Tuple: substitution + organon-construction
over a matrix organon; dimensions homogeneity / decomposability / reversibility.
The *homogeneity* reading is what produced #4 — the population is homogeneous in
world **identity** (14 distinct identities across 107 observed builds, 71 of them
one identity) but heterogeneous in **why it builds**, and an allowlist must key
on the latter, not on whether a call site memoises.

*Capture.* Spec §3.1; decision 0606.

---

#3 [Q] — **Where the fixture loader lives, and whether it `include_str!`s.**

*Decision.* In `hornvale-worldgen` production source — the composition root —
reading the fixture at **runtime**, never `include_str!`.

*Why.* `kernel/src/golden.rs` is already test support living in production
kernel source, so the posture is settled precedent rather than a new smell.
Worldgen specifically because every crate that builds a world already depends on
it, so no new edge is added to the layering graph. Runtime read rather than
`include_str!` because 5.5 MB baked into N test binaries hits the
compilation-unit cost that *is* this project's gate cost — CLAUDE.md records the
260 test targets alone costing +157.5 s of kernel time merely to exist, and
`test_binary_ratchet.rs` exists to stop that re-accreting. Baking a 5.5 MB
literal into dozens of units would be a regression on the very axis this
campaign is trying to improve.

*Mechanism note — verified, and the tree's own idiom proved only a neighbouring
claim.* `env!("CARGO_MANIFEST_DIR")` expands against the crate containing the
macro — worldgen — so one fixed `../../` prefix resolves correctly regardless of
which crate calls the function. `windows/worldgen/tests/suite/repose_exposure.rs:1924`
looks like precedent but is worldgen's own test reading worldgen's dir, i.e. the
*cheaper neighbour* of the claim actually being made. Measured with a scratch
probe instead — a `pub fn` in worldgen returning `env!("CARGO_MANIFEST_DIR")`,
called from an example in `cli`:

```
  worldgen fn reports:               .../windows/worldgen   <-- called FROM cli
  cli's own env! is:                 .../cli
  cli-relative ../../ resolves?      false     <-- negative control
  worldgen-relative ../../ resolves? true
```

The negative control is the load-bearing half: the prefix is **crate-specific**,
so the loader must live in exactly one crate. A per-crate helper duplicating
`../../` would resolve outside the repository from `cli` and `kernel` (one level
deep) while working from every `windows/*` and `domains/*` crate (two deep) — a
bug that passes in most crates and fails in two. Scratch reverted; `git status`
clean.

*ideonomy passes / overturns.* 1 / 0.

*Capture.* Spec §3.3.

---

#4 [Q] — **What a roster row records.**

*Decision.* A **reason code** — `build-path` / `artifacts` / `identity` /
`production` / `unmigrated(<why>)` — and the ratchet is one assertion: the count
of `unmigrated(...)` rows never increases.

*Why.* This is the three-valued shape `tropes check`, the timings baseline,
type-audit's `waiver(...)` and seam-guard all already use. A guard that failed
on the mere *existence* of the grandfathered set would be red on day one and
trained away by day two — `test_binary_ratchet.rs` says exactly that in its own
module doc, having watched 13 test binaries creep back. Separating the permanent
reasons from `unmigrated(...)` is what makes the ratchet measurable without making it
a wall, and it puts the why-it-builds taxonomy where a reader will find it.

*ideonomy passes / overturns.* 1 / 0 — this entry is the homogeneity reading
from #2's pass, promoted to a decision of its own.

*Capture.* Spec §3.2; decision 0606.

---

#5 [Q] — **Whether `cli/tests/fixtures/world-seed-42.json` moves.**

*Decision.* No. It stays; worldgen reaches across to it.

*Why.* `docs/generated-paths.txt` carries explicit prose that
`cli/tests/fixtures/` is byte-golden and frozen-historical-pin territory — "A
golden has an accept path a human may deliberately take. A frozen historical pin
has NONE" — and that declaring the directory artifacts-authored "is a category
error rather than an inert mistake". Moving the file means rewriting that prose,
a category-sensitive edit that does not belong bundled into this campaign.

*Alternatives discarded.* Move it under `windows/worldgen/tests/fixtures/` —
tidier ownership, but drags the generated-paths rewrite along with it.

*ideonomy passes / overturns.* 1 / 0.

*Capture.* Spec §3.3. Flagged to Nathan as a judgment call at the design gate
and approved there.

---

## Parked findings

### P1 — `scene_surrounds_colour_cli.rs` uses a fixed temp path and flakes

`cli/tests/suite/scene_surrounds_colour_cli.rs:42` writes its world to
`std::env::temp_dir().join("hv-scene-surrounds-colour-test.json")` — a fixed,
unsuffixed name shared by every concurrent run of that test on the box.

Observed 2026-09-02 during this campaign's measurement pass, on a full-workspace
run: the test asserted `new` succeeded — so the CLI *had* written the file — then
panicked at line 80 with
`world.json reloads: Os { code: 2, kind: NotFound, message: "No such file or directory" }`.
Under the default fail-fast profile it took the whole run down at test 274 of
4869, which is how this campaign ended up with a 285-test sample instead of a
full census of build sites.

Not caused by this campaign's instrumentation: the probe appended only to an
absolute path under the session scratchpad and never touched that filename.

Unrelated to the reservoir work and **not fixed here**. Fix shape: a per-process
suffix, or `CARGO_TARGET_TMPDIR`.

### P2 — nothing pins the derived artifacts' bytes

`GeneratedTerrain` and `GeneratedClimate` are not `Serialize`, and no committed
artifact pins the terrain sculpt byte-for-byte. Pre-existing; this campaign
neither creates nor worsens it, but §5 of the spec has to name it as the one
real coverage residue that fixture-loading does not cover. Worth a `TOOL-*` row
if the registry does not already carry one.

---

## Capture manifest

- **The scratch ledger was created and then removed.** This campaign first wrote
  `.superpowers/sdd/decision-ledger.md`, following `campaign-autopilot`'s own
  instruction. That instruction is **superseded by The Cartulary** (2026-08-30):
  the root `CLAUDE.md` names that exact path as the collision-prone one — every
  campaign's scratch ledger lived at the identical path, so two campaigns editing
  it merged to one side silently — and rules that rulings, deferred minors and
  parked findings go in the committed per-campaign ledger *as they occur*. The
  scratch files were deleted and their content folded in here. Recorded because
  the skill and the doctrine currently disagree, and CLAUDE.md wins; the skill
  text is worth a `PROC-*` correction.
- **Two parked findings, P1 and P2 above** — the fixed-temp-path flake, and the
  absence of any byte pin on the derived artifacts. Neither is fixed here.
- **No new idea-registry row for the in-process-memo rejection.** Decision 0032
  already records it in terms this campaign only confirms: "cargo-nextest (its
  process-per-test model re-initialises the `LazyLock` census once *per test* —
  strictly worse for this suite, not better)". The repo holds the fact; a row
  would duplicate it.
- **MAP-25 relation.** The depth-scoping half of this problem is MAP-25, already
  `shipped` as the `BuildDepth` ladder. This campaign does not reopen it; spec §7
  makes depth-scoping an explicit non-goal so the two do not blur.
