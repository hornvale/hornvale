# Campaign 22 (The Atlas) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** None of this campaign's findings restate a prior
retrospective's by name. The closest cousin is Campaign 19's reproducibility
discipline — verify before you build on top — which this campaign applied to
a new kind of artifact: a bundled JavaScript file rather than a rendered
image. The bundle-freshness proof (rebuild from the checked-out source,
rebuild again from a second, differently located copy of the same source,
diff both against the committed file) is the same shape as the byte-identity
checks earlier campaigns ran on regenerated rasters, ported to a toolchain
that had never shipped a committed artifact before. It held on the first
try.

**Estimate deltas.** No stage-level estimates were made for this campaign,
so there is nothing to compare against — say so rather than pad.

**Spec vs. reality.** Four implementation tasks plus this closing one landed
against a plan that, per the SDD ledger, was followed closely enough that
review across the whole campaign found almost nothing wrong with the plan
itself. Three findings surfaced, all minor and all fixed or disclosed in the
same commit that found them:

- The scene parser, as specced, accepted a non-object top-level JSON value
  and fractional tile-grid dimensions without complaint. A reviewer probing
  five adversarial inputs against the committed fixture caught both gaps;
  both were closed in one commit with covering tests, and three further
  adversarial cases the same reviewer tried did not slip through.
- `main.ts`'s literal brief text failed `deno check`: `layerPixels` returns
  a bare `Uint8ClampedArray`, which this Deno's TypeScript (6.0.3) widens to
  a supertype that includes `SharedArrayBuffer`-backed views — a type
  `ImageData`'s constructor correctly refuses. The fix was scoped to the
  untested glue file alone (wrapping the call in a fresh, concretely
  `ArrayBuffer`-backed `Uint8ClampedArray`), leaving the tested `palette.ts`
  module untouched, and was disclosed in the task report rather than folded
  in silently. Worth naming plainly: this was a toolchain-version type
  deviation from a hand-authored plan, not a logic error, and the honest
  move was to say so and show the one-line fix rather than pretend the
  brief's code had compiled as written.
- A cosmetic import-order swap in the palette module, and a theoretical
  float-boundary case in the zoom math that a 1.8-million-sample fuzz pass
  did not actually reach — both noted, neither judged worth a fix.

**First non-Rust CI job — the two-gate structure held.** This campaign put
a second toolchain in the repository for the first time, and the design
question decision 0023 settled — a sibling job, its own pinned binary, its
own freshness check, zero edits to the Rust gate — turned out to cost the
existing gate nothing at all. `git diff .github/workflows/ci.yml` after
adding the `atlas` job shows a pure addition: the `gate` job's steps are
byte-identical before and after. The pinned-Deno-plus-path-independence
proof that decision 0023 asked for is exactly what the freshness-check step
now runs on every push, and it is the same proof the SDD tasks ran by hand
during development — the CI step did not have to invent a new verification
strategy, only automate the one already used to accept the bundle in the
first place.

**The dataviz-validated palette step paid for itself twice.** Task 3 built
the plate and unrest layers' colors against the dataviz skill's validated
categorical and sequential sets rather than picking colors by eye, and
review verified all twenty-two biome RGB values and the elevation ramp
against their Rust sources directly — every color in the atlas either
matches a committed raster exactly or comes from a set already checked for
perceptual soundness. The payoff showed up immediately: the unrest layer,
switched on for the first time in the book, visibly traces plate boundaries
rather than reading as noise, which would not have been obvious from a
hand-picked ramp without the validated step's monotonicity guarantee behind
it.

**Do differently next time — testing a canvas client by keeping the glue
thin.** The four modules
carrying real logic — scene parsing, viewport projection, palette mapping,
hit-testing — are unit-tested against both committed JSON artifacts, zoom
and pan clamps, palette totality and monotonicity, and hit radii; `main.ts`
itself has no unit tests, only DOM wiring. When no browser was available in
the development environment to screenshot the rendered page, this split is
what made verification possible anyway: the controller ran the tested
modules directly, feeding them the same seed-42 scene document the page
fetches, and reconstructed all five layers' pixel output outside a browser
entirely — confirming the biome layer pixel-identical to the already-
committed biome PNG without ever opening a canvas. Worth recording as the
pattern for future canvas clients: if the logic that decides *what to draw*
is pure and tested, the glue that *draws it* can be verified by running the
same logic headlessly, and a missing browser degrades the verification
story rather than blocking it outright.
