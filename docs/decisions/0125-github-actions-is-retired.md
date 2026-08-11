# 0125. GitHub Actions is retired

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Supersedes:** 0042

In the context of the repository becoming **private** (2026-08-11 17:19 UTC),
which meters Actions minutes against a quota and withdraws GitHub Pages from
the org's plan, facing a `ci.yml` whose one distinctive job had already been
made redundant by lefford and a `book.yml` that would now fail on every push
to `main`, we decided to **delete `.github/workflows/` entirely** — no
workflow runs on any trigger, manual included — accepting that every gate is
now local, and that the book and the world-catalog release are published by
hand or not at all.

**Why 0042 no longer holds.** 0042 kept `ci.yml` in the tree as a
`workflow_dispatch` escape hatch for the one axis a macOS dev box cannot
check: Linux cross-platform byte-identity. It named `scripts/aws-gate/` as the
primary instrument for that axis and the runner as the backstop. Both premises
are gone. 0063 abandoned AWS; 0079 and 0086 made **lefford** — a 40-core Linux
box we own — the enforced canonical host for censuses and the heavy tier; 0090
(The Pyx) then *measured* the axis, finding a 40-world all-metric probe
byte-identical between x86_64/Linux and aarch64/Darwin. A hand-dispatched
2-core runner is strictly worse than `make heavy-remote REF=<full-sha>` at the
job 0042 was keeping it for. Going private only removed the last reason to
leave a dormant file in the tree.

**Why the book workflow went too.** `book.yml` was the only workflow that
still ran automatically (~1–2 min per push to `main`), and it is the reason
the quota question is not hypothetical. It is also already broken: the Pages
site and the Pages API both answer 404 as of the visibility change. Keeping it
would spend metered minutes to fail.

**Consequence — the coverage this costs, named rather than assumed.**

1. **`clients/atlas` now has no gate at all.** `vessel`, `world`, and `game`
   each have a local `make *-check`; atlas never got one, so its `deno fmt /
   lint / check / test` and its `book/src/gallery/atlas.js` bundle-freshness
   diff ran *only* in `ci.yml`. This is a deliberate, owner-decided gap
   (2026-08-11), not an oversight: a `make atlas-check` was offered and
   declined. Anyone touching `clients/atlas/` runs those four `deno` commands
   and the bundle diff by hand, or the drift lands uncaught.
2. **The book is unpublished.** `hornvale.github.io/hornvale` is dead. The
   book still builds (`mdbook build book`) and is still the project's primary
   artifact under 0059 — it simply has no public address. 0052 (the Casement
   wasm is deploy-built, never committed) still holds, but its only builder
   was `book.yml`, so `make wasm-vessel` is now a purely local step. Restoring
   publication is an open question, not a decided one.
3. **The world-catalog release is manual.** No tag fires anything.
   `world-wasm-v*` releases are built with `make world-check` and uploaded by
   hand. The external Orrery client consumes that release, so the cross-repo
   scene-schema contract now depends on a human remembering to cut it.
4. **`scripts/ci-census-probe.sh` lost its only caller.** It stays in the tree
   — it is a genuinely fast hand-runnable spot-check, and several specs and
   plans name it — but nothing invokes it automatically any more. Its `ci-`
   prefix is now a historical name.
5. **The artifact drift check has no automatic run.** It survives as
   `make rebaseline` plus the `git diff --exit-code` list in `CLAUDE.md` —
   which was always the authority `ci.yml` mirrored (TOOL-15), and which
   carries no pathspec exclusions because it never needed them.
   `ci.yml`'s eight `:(exclude)` pathspecs were **cross-platform** artifacts:
   they existed because the runner regenerated on Linux what had been
   authored on macOS, so the platform-local files (the PNG maps, the
   thresholded tiles/locale/surrounds scenes and their ASCII renderings —
   decision 0078) diverged for reasons no code change caused. Regeneration
   now always happens on the host that authored the artifact, so the
   exclusion problem dissolves rather than moves. The Sounding's two files
   were excluded for a different and still-live reason (0087: wall-clock
   columns are not byte-stable even on one host), but they are written by the
   heavy tier, not by `regenerate-artifacts.sh`, so they never enter this
   check's path. What `ci.yml`'s comment block held that nothing else did was
   the *reasoning* for each exclusion; that reasoning is preserved in 0078,
   0087, and this record. 0087's pointer at "the existing pathspec exclusions
   in `.github/workflows/ci.yml`" should be read as pointing here.

**What did not change.** `make gate` is still the commit gate and still the
only thing standing between a change and `main`. 0029 (only the 500-seed
census family is drift-checked) is untouched in substance — it now constrains
`make rebaseline` rather than a CI step. Nothing about determinism, layering,
or the type audit relaxes; those were always enforced by workspace tests, not
by the runner.

**See also.** Decision 0042 (superseded); 0063 / 0079 / 0086 (the canonical
Linux host); 0090 (the axis, measured); 0059 (the book is the primary
artifact); 0052 (deploy-built wasm); 0087 (a benchmark's timings are a record,
not a golden); `CLAUDE.md`'s gate-ladder and drift-check blocks.
