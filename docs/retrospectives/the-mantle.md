# Retrospective — The Mantle

A client-only campaign: re-render the globe cloud layer (The Firmament's
deferred visual) as a cloud-texture shell, plus a world-wasm-v11 release. No
sim change, worlds byte-identical. Process lessons only.

## What worked

- **The ideonomy pass found the keystone, not just an approach.** The obvious
  framing was "which primitive — particles, sprites, a shader?" Negating the
  cloud shell's definitional properties instead surfaced that the prior
  failure's real cause was *untextured colour blobs*, not the primitive: a
  flat-coloured shell would have failed identically. The load-bearing move —
  fbm noise gated by cloud type into the alpha — became the plan's explicit
  keystone, and the T2 tests were written to guard exactly it (soft partial-
  alpha edges must exist; gaps must exist). The final reviewer hand-traced
  that those tests are mechanism-sensitive (a binary or flat generator fails
  them), so the keystone can't silently regress.

- **The visual pass applied the previous campaign's own retro lesson and it
  paid off immediately.** The Firmament retro recorded that Playwright's
  webServer serves the built `dist/` and `reuseExistingServer` keeps a stale
  build, silently blinding the tuning loop. This campaign rebuilt (`npm run
  build`) and killed the reused server before every capture — so the very
  first fresh capture showed the real render, and the tuning loop was honest
  from the start. A retro lesson from the prior campaign directly prevented
  its own recurrence.

- **Splitting the deferral was the right call.** The Firmament shipped the
  felt-weather half and deferred the visual to a focused follow-up rather than
  grinding past the 3-attempts wall. The Mantle then delivered the visual in a
  clean, small campaign with a genuinely different technique — vindicating the
  "stop and bring the tradeoff to Nathan" decision.

## What was hard / notes

- **A dead export outlived its consumer by one commit.** Removing the stale
  `setCloudsAvailable` gate's `CLOUD_FRACTION_THRESHOLD` import left the export
  dead, but its test still imported it — and the commit landed red (the
  pre-commit hook did not catch it). Caught on the next full-suite run and
  amended. Lesson: when deleting an export, grep its test file too, and run the
  suite *before* the commit lands, not after.

- **The jsdom blind spot is structural for orrery visuals.** The reviewer
  could verify the UV/rotation math by hand-derivation but explicitly could
  NOT confirm the clouds aren't N/S-inverted in practice — only the
  controller's visual pass can. This is the standing division of labour: the
  reviewer owns code correctness, the controller owns the seen-check. It held.

## Follow-ups

- **Propensity-coverage unit test** (final review Minor): no test varies
  `weatherPropensity` to assert the coverage boost shifts alpha. Add one.
- **Cloud polish (optional garnish, all deferred in the spec):** a lit shell
  material for sunlit-white tops; a limb-fade; per-day cloud evolution
  (re-deriving types as the clock scrubs). None needed for the payoff.
