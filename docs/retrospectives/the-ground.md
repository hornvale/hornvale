# The Ground — retrospective

**Completed:** 2026-07-14 (name-only designation per decision
`slugs-not-numbers`; plan `docs/superpowers/plans/2026-07-14-the-ground.md`,
seven tasks: surface latent tectonic inputs, the material buffer, rock class
+ lithology lens, soil order + fertility, hydrogeology + karst, appearance +
prospectivity, almanac/census/close)

**A brief's predicted accessor shape and extractor rung were both wrong, and
the fix was cheaper than the prediction.** Task 7's dispatch, written from a
read of the design spec rather than the lab crate's actual code, named
`Extractor::Full` for the two soil-order metrics and asserted `TerrainView`
and `FullView` each exposed a `.terrain()`/`.climate()` accessor pair. Neither
held: `TerrainView` exposes its terrain through a public field, not a method,
and `FullView` had no `.terrain()` at all. Rather than adding the predicted
accessor to make the prediction true, reading the lab crate's existing
`dominant-land-biome`/`habitable-fraction` metrics (which need exactly the
same terrain+climate inputs) showed the `Extractor::Climate` rung already did
the job — cheaper to build (`BuildDepth::Terrain`, skipping settlement,
religion, language, and species genesis entirely) and requiring zero new
surface. The general lesson: a dispatch brief's guess at an internal shape,
even a careful one, is a hypothesis to verify against the code, not a
contract to satisfy by adding what the guess assumed — and the fix for a
wrong guess is often to find the existing precedent one metric away, not to
extend the surface to match the guess.

**Six prior tasks landed as six single, clean commits with no interspersed
fix commits visible in the log** (`22da2bb` through `856f2d8`) — a
data point, not a guarantee, since squashing before commit is invisible from
outside, but a real signal that each task's own review loop stayed inside
its own commit range across a seven-task, single-substrate campaign with a
deliberately additive scope fence. That fence — no consumer rewiring, ore
deposits and full stratigraphy explicitly deferred, the metaphysics overlay
banked rather than built — is very likely why: every task added a field or a
projection over the previous task's output and never had to reconcile with a
sibling system, the shape of scope that keeps a plan's tasks independently
reviewable.

**Six tasks of new stream labels and typed pub surface accumulated
unregenerated in the committed docs until the seventh task's artifact
regen ran.** `book/src/reference/stream-manifest-generated.md` was still
missing the `terrain/lithology` hash-noise label Task 3 added four tasks
earlier, and the type-audit report's terrain-crate count was short by 37
tagged items by the time Task 7 ran `regenerate-artifacts.sh` for what
turned out to be the first time all campaign. None of this was wrong — the
project's own policy defers the *census* regen to campaign close on purpose
— but the non-census artifacts (`stream-manifest-generated.md`,
`docs/audits/type-audit-report.md`) carry no such exemption and drifted
silently for six tasks anyway, simply because no task's own verification
happened to include the regen script. `git diff --exit-code` in CI would
have caught the drift at the campaign's PR boundary regardless, so nothing
shipped wrong; the cost was purely that six tasks' worth of doc drift landed
in one large, harder-to-attribute diff at the close task instead of six
small, self-explaining ones. A task that adds a stream label or a new
`pub` primitive is exactly the kind of task that would benefit from running
the (SKIP_CENSUS=1) regen script itself, the same way `cargo fmt` already
runs before every commit.

**The almanac's spread-fixture test pattern (`..sample_context()`) paid for
itself again.** Adding the seventh required field to `AlmanacContext` needed
exactly one new line in the shared fixture constructor; every other existing
test that builds its context via `..sample_context()` picked up a sensible
default automatically rather than needing an edit. A struct that grows a
field once per campaign is exactly the shape this pattern is for, and the
almanac crate's tests are proof it keeps working at the fourth or fifth
growth, not just the first.

## Estimate vs reality

I executed only the seventh (final) task directly, so the above is drawn
from a mix of first-hand observation (Task 7) and the visible commit
history for Tasks 1–6; I cannot speak to whether any of those six tasks
needed an internal fix-and-reverify cycle that never reached a separate
commit. Task 7 itself ran within its own scope with one real deviation (the
extractor-rung correction above) and no scope growth: the almanac section,
the five census metrics, the lithology map artifact, and the book
Definition of Done all landed as planned. No Confidence Gradient bet moved —
`book/src/open-questions.md` names no lithology- or geology-adjacent bet,
so the chapter is left as is rather than re-scored on invented grounds.
