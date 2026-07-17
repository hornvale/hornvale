# Study 001: The Census of Skies

Ten thousand tier-0 worlds, unpinned — seeds 0 through 9,999, each built in
full and measured against every metric the registry knows. The census asks
two very different kinds of question at once. One we already knew the answer
to, and asked anyway, to check that the instrument tells the truth. The
other had never been asked, because until the Lab existed there was no way
to ask it.

*This chapter predates the land metrics of Campaign 3c.* Its committed
summary and charts measure only the sky; the tectonic and climate metrics
that the registry has since grown are the subject of
[Study 002: The Census of Lands](./study-002.md). Both are author-time
runs; the sky census stands here unchanged, historical, as the Lab's first
published measurement.

{{#include generated/census-of-skies/census-of-skies-summary.md}}

## The calibration holds

At tier 0 a world's religion heeds only its single most salient sky: a
tidally locked sun, fixed forever at one point in the heavens, yields a
belief in an eternal and unmoving power; a rising-and-setting sun yields a
cyclic one. This is not a hypothesis about the census — it is a fact about
the code, true by construction. So a world's tidal locking and the sentiment
of its peoples' head deities must agree, and if they ever disagreed the
instrument would be lying.

They agree. Of ten thousand worlds, **457 are tidally locked, and exactly
457 hold an eternal faith** — not 456, not 458. The remaining 9,543 spin,
and all 9,543 believe in a cyclic heaven.

*(A note on how this is measured now: the census once carried a single
per-world `belief-kind` column, the head deity of whichever people committed
its pantheon first. The Presiding retired it — a world has no religion, its
peoples do — for one reading per people. The invariant is unchanged and in
fact stronger: it now holds for every people's head on every locked world,
not one arbitrary people's. The frozen-sky calibration in
`windows/lab/tests/calibration.rs` checks exactly that.)*

<!-- `belief-kind` is a religion-domain metric, dropped from
`census-of-skies` when it went sky-only (chore(lab): census-of-skies goes
sky-only); its chart no longer regenerates here. The cross-domain
calibration this section describes was captured once, historically, at the
original 10,000-seed sky census and is retold in prose above — the same
equality now lives on as a live assertion over `the-census`
(`eternal_beliefs_coincide_exactly_with_tidal_locking`,
`windows/lab/tests/calibration.rs`), so the instrument is still kept
honest, just no longer illustrated on this historical page. -->

A calibration metric with known ground truth, validated to the single world
on the day the instrument shipped. Every other number the census reports is
now worth a little more trust, because the thing reporting them agrees with
reality exactly where reality is already known. This same equality is
asserted as a test over the drift study on every CI build — the instrument
does not merely happen to be calibrated, it is kept so.

## The first unknown number: the moon-refusal rate

When a world is dealt more moons than its gravity can hold in stable orbit,
the generator refuses the surplus and records why. How often does that
happen? Nobody knew — the refusal is buried deep in the moon-admission loop,
and no almanac ever aggregated it across worlds. The census does: **360
worlds in 10,000 — 3.6% — were denied a moon they were dealt.**

{{#include generated/census-of-skies/census-of-skies-default-refused-a-moon.svg}}

Each refusal leaves exactly one genesis note, and the `genesis-note-count`
metric confirms it independently: 360 worlds with one note, the rest with
none. The two counts are computed along entirely separate paths and match to
the world — a second, unplanned calibration falling out of the first real
measurement.

The refusal rate must not be confused with childlessness. 1,488 worlds
(14.9%) end with no moon at all — but most of those were simply never dealt
one, not refused. A refusal is the narrower, more interesting event: the
physics was asked for a moon and the stability inequalities said no. Only
3.6% of skies carry that particular scar.

{{#include generated/census-of-skies/census-of-skies-default-moons-admitted.svg}}

## The generator's envelope

Ten thousand samples reveal the shape of what the generator produces — and,
just as tellingly, what it never does.

{{#include generated/census-of-skies/census-of-skies-default-day-length-hours.svg}}

- **Day length** falls entirely between 16 and 40 hours; the `< 16` and
  `>= 40` buckets are empty across all ten thousand worlds, and the spinning
  worlds spread almost evenly across the band between.
- **Obliquity** never reaches 35°; its top bucket is empty.
- **Total tidal strength** never reaches 8 — the `>= 8` bucket is empty in
  every world. That is precisely the cap the moon-admission inequalities
  were written to enforce, now shown to hold at scale rather than merely in
  the handful of seeds anyone happened to test by hand.
- **Months per innermost moon** never drops below 5, and its `absent` count
  (1,488) matches the moonless worlds exactly.

These bounds were implicit in the draw ranges and the stability rules, but
the census is the first thing to *demonstrate* them: no seed in ten thousand
escapes the envelope. That is the quiet second duty of a calibration study —
not only to confirm what the instrument measures, but to confirm that the
world-generator respects its own declared limits.

## What the census does not yet say

The census counts; it does not yet correlate. It can tell you that 4.6% of
worlds are tidally locked and that 42.8% orbit an F-class star, but not
whether locked worlds cluster around any particular star class — that is a
two-metric question, and the runner records one row per world precisely so
such questions become answerable later without regenerating anything. The
first hypothesis study will ask one. For now the instrument is built,
calibrated, and honest, and the census stands as its first published
measurement.
