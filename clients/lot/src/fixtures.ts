// Test fixtures: the smallest payloads that still carry the shapes the
// modules branch on. Shaped after the real seed-42 payloads (`hl_lot(0)`,
// `hl_lot_curve`, `hl_lot_places(1500)`), reduced by hand so a test reads.
//
// Not `_test.ts`, because several test files share these; `deno test` only
// collects `*_test.ts`, so this file is compiled with the sources and
// carries no tests of its own.

import type { Curve, Life, Place } from "./payload.ts";

/** A curve over a 2,000-year span in 25-year epochs, growing toward the
 * present the way seed 42's does. */
export function growingCurve(): Curve {
  const bins = 80;
  const births_by_epoch = Array.from({ length: bins }, (_, at) => (at + 1) ** 2 / 10);
  return {
    schema: "lot/curve/v1",
    epoch_years: 25,
    start_year: 0,
    present_year: 2000,
    births_by_epoch,
    births_by_people: { kobold: births_by_epoch },
    souls_ever: 226972.15,
  };
}

/** The same span with a flat curve — the branch `hintFor` takes when a
 * world's population has stopped growing. */
export function flatCurve(): Curve {
  const births_by_epoch = Array.from({ length: 80 }, () => 3);
  return {
    schema: "lot/curve/v1",
    epoch_years: 25,
    start_year: 0,
    present_year: 2000,
    births_by_epoch,
    births_by_people: { human: births_by_epoch },
    souls_ever: 240,
  };
}

/** Three places, one of them unnamed. */
export function places(): Place[] {
  return [
    {
      occ: 1,
      entity: "10760661430244474881",
      site: 22195,
      people: "bugbear",
      name: "closed-canopy",
      latitude: -4.0014842,
      longitude: -145.74382,
      population: 68,
      births_per_year: 0.031407364,
    },
    {
      occ: 7,
      entity: "10760661430244474887",
      site: 10630,
      people: "kobold",
      name: "Raaxora",
      latitude: 16,
      longitude: 122.6,
      population: 40,
      births_per_year: 1.4,
    },
    {
      occ: 9,
      entity: "10760661430244474889",
      site: 33,
      people: "goblin",
      name: null,
      latitude: -60,
      longitude: 0,
      population: 12,
      births_per_year: 0.2,
    },
  ];
}

/** A life with one filled slot per kind the renderers branch on: filled,
 * `no-fact` silent, and `by-design` silent. Matured, unmoved, and ended by
 * the hazard — the common case. */
export function life(): Life {
  return {
    schema: "lot/life/v1",
    seed: 42,
    index: 0,
    pick: { year: null, site: null },
    occ: 451,
    occupation: "10760661430244475331",
    site: 10630,
    birth_year: 1751.6373,
    death_year: 1783.3546,
    age_at_death: 31.71731,
    matured: true,
    ending: { kind: "hazard", cause: "the flux" },
    projection: {
      kind: "composite",
      source_cohort: { people: "kobold", site: 10630, year: 1751.6373 },
      selection_lens: "representative",
      materiality: "analytical",
      sampling_bias: "representative",
      consequences_write_back: false,
    },
    moved_to: null,
    moved_year: null,
    shape: "rise-plateau",
    slots: [
      {
        key: "when",
        value: "born in year 1752, dead in year 1783, aged 32",
        silence: null,
        sources: [1, 2],
      },
      {
        key: "where",
        value: "Raaxora, a temperate-forest site, at 16.0°, 122.6°",
        silence: null,
        sources: [3],
      },
      { key: "name", value: "Xaararo", silence: null, sources: [4] },
      { key: "cause", value: "the flux", silence: null, sources: [2] },
      {
        key: "subsistence",
        value: null,
        silence: { kind: "no-fact", reason: "nothing in the record says how they fed themselves" },
        sources: [],
      },
      {
        key: "sex",
        value: null,
        silence: { kind: "by-design", reason: "no species in this world carries a sex model" },
        sources: [],
      },
      {
        key: "family",
        value: null,
        silence: { kind: "by-design", reason: "no fertility or household model exists" },
        sources: [],
      },
      {
        key: "work",
        value: null,
        silence: { kind: "by-design", reason: "no occupation is modelled" },
        sources: [],
      },
      {
        key: "literacy",
        value: null,
        silence: { kind: "by-design", reason: "nothing models literacy" },
        sources: [],
      },
    ],
    silences: { filled: 4, no_fact: 1, by_design: 4 },
    sources: [
      {
        number: 1,
        kind: "fact",
        entity: "10760661430244475331",
        predicate: "occ-founded",
        caption: "the standard day the occupation began",
        function: null,
        inputs: null,
      },
      {
        number: 2,
        kind: "derived",
        entity: null,
        predicate: null,
        caption: null,
        function: "lot::draw::draw",
        inputs: "the world's births-per-year curve at the lot's index",
      },
    ],
  };
}
