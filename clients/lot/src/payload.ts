// The four `lot/*/v1` payloads, as the exhibit reads them, and the
// validating parsers that turn a worker's JSON text into them.
//
// **These types are a mirror of `windows/lot/src/json.rs`, and that file is
// the contract.** Nothing here computes a demographic quantity; every field
// below is a number the sim drew and quantized at its own emit boundary
// (decision 0033). The exhibit's rule (spec §6.4) is that every number it
// shows is a field of a payload — so if a quantity is not in this file, the
// page may not display it.
//
// v1 is additive: the sim may append named slots and change an answerable
// slot from by-design to no-fact without changing the schema. Clients must
// ignore slot keys they do not render and read the sim-provided silence tally.
// The parsers check the `schema` string and the shape of the fields the
// page actually reads, and throw otherwise. A payload that arrived from a
// stale wasm is a confusing page, not a blank one, unless something says so.

/** One slot's silence and why it is silent. */
export interface Silence {
  /** `"no-fact"` — this world's ledger had no answer; `"by-design"` — no
   * world here models the thing at all (spec §4.4). */
  kind: string;
  /** The sim's own sentence for the silence. */
  reason: string;
}

/** One of the story's slots: a value, or `null` beside its silence. */
export interface Slot {
  key: string;
  value: string | null;
  silence: Silence | null;
  /** The `[n]` source numbers this slot's answer rests on. */
  sources: number[];
}

/** One entry in the flat source list. Both kinds share one shape with
 * `null` in the fields the other kind uses, exactly as `json.rs` emits. */
export interface Source {
  number: number;
  kind: string;
  entity: string | null;
  predicate: string | null;
  caption: string | null;
  function: string | null;
  inputs: string | null;
}

/** The story's silence tally, carried by the payload rather than counted
 * here — the campaign's headline number (decision 0798). */
export interface SilenceCounts {
  filled: number;
  no_fact: number;
  by_design: number;
}

/** How the life ended. */
export interface Ending {
  kind: string;
  cause: string | null;
}

/** The substrate cohort a projection came from. */
export interface SourceCohort {
  people: string;
  site: number;
  year: number;
}

/** The projection boundary carried by a life. */
export interface Projection {
  kind: string;
  source_cohort: SourceCohort;
  selection_lens: string;
  materiality: string;
  sampling_bias: string;
  consequences_write_back: boolean;
}

/** Which pins the reader applied, echoed back. */
export interface Pick {
  year: number | null;
  site: number | null;
}

/** `lot/life/v1`. */
export interface Life {
  schema: string;
  seed: number;
  index: number;
  pick: Pick;
  occ: number;
  /** A full-width entity id, emitted as decimal TEXT because JSON numbers
   * are IEEE-754 doubles and this one does not fit in 53 bits. Never
   * `Number()` it. */
  occupation: string;
  site: number;
  birth_year: number;
  death_year: number;
  age_at_death: number;
  matured: boolean;
  ending: Ending;
  projection: Projection;
  moved_to: number | null;
  moved_year: number | null;
  shape: string;
  slots: Slot[];
  silences: SilenceCounts;
  sources: Source[];
}

/** `lot/curve/v1`. */
export interface Curve {
  schema: string;
  epoch_years: number;
  start_year: number;
  present_year: number;
  births_by_epoch: number[];
  births_by_people: Record<string, number[]>;
  souls_ever: number;
}

/** One occupation alive at a year. */
export interface Place {
  occ: number;
  /** Decimal text, for the same reason as `Life.occupation`. */
  entity: string;
  site: number;
  people: string;
  name: string | null;
  latitude: number;
  longitude: number;
  population: number;
  births_per_year: number;
}

/** `lot/places/v1`. */
export interface Places {
  schema: string;
  year: number;
  places: Place[];
}

/** One attributed cause in `lot/odds/v1`. */
export interface CauseOdds {
  cause: string;
  share: number;
}

/** `lot/odds/v1`. */
export interface Odds {
  schema: string;
  e0: number;
  q_maturity: number;
  maturity_years: number;
  lifespan_years: number;
  strife: number;
  infant_share: number;
  background_share: number;
  senescent_share: number;
  causes: CauseOdds[];
}

function object(value: unknown, what: string): Record<string, unknown> {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${what}: expected an object`);
  }
  return value as Record<string, unknown>;
}

function schema(doc: Record<string, unknown>, want: string): void {
  if (doc.schema !== want) {
    throw new Error(`expected schema ${want}, got ${JSON.stringify(doc.schema)}`);
  }
}

function num(doc: Record<string, unknown>, key: string): number {
  const v = doc[key];
  if (typeof v !== "number" || !Number.isFinite(v)) {
    throw new Error(`${key}: expected a finite number`);
  }
  return v;
}

function optNum(doc: Record<string, unknown>, key: string): number | null {
  const v = doc[key];
  if (v === null || v === undefined) return null;
  if (typeof v !== "number" || !Number.isFinite(v)) {
    throw new Error(`${key}: expected a finite number or null`);
  }
  return v;
}

function text(doc: Record<string, unknown>, key: string): string {
  const v = doc[key];
  if (typeof v !== "string") throw new Error(`${key}: expected a string`);
  return v;
}

function optText(doc: Record<string, unknown>, key: string): string | null {
  const v = doc[key];
  if (v === null || v === undefined) return null;
  if (typeof v !== "string") throw new Error(`${key}: expected a string or null`);
  return v;
}

function list(doc: Record<string, unknown>, key: string): unknown[] {
  const v = doc[key];
  if (!Array.isArray(v)) throw new Error(`${key}: expected an array`);
  return v;
}

function numbers(doc: Record<string, unknown>, key: string): number[] {
  return list(doc, key).map((v, at) => {
    if (typeof v !== "number" || !Number.isFinite(v)) {
      throw new Error(`${key}[${at}]: expected a finite number`);
    }
    return v;
  });
}

/** Parse `lot/life/v1`. */
export function parseLife(json: string): Life {
  const doc = object(JSON.parse(json), "lot/life/v1");
  schema(doc, "lot/life/v1");
  const pick = object(doc.pick, "pick");
  const ending = object(doc.ending, "ending");
  const projection = object(doc.projection, "projection");
  const sourceCohort = object(projection.source_cohort, "projection.source_cohort");
  const silences = object(doc.silences, "silences");
  return {
    schema: "lot/life/v1",
    seed: num(doc, "seed"),
    index: num(doc, "index"),
    pick: { year: optNum(pick, "year"), site: optNum(pick, "site") },
    occ: num(doc, "occ"),
    occupation: text(doc, "occupation"),
    site: num(doc, "site"),
    birth_year: num(doc, "birth_year"),
    death_year: num(doc, "death_year"),
    age_at_death: num(doc, "age_at_death"),
    matured: doc.matured === true,
    ending: { kind: text(ending, "kind"), cause: optText(ending, "cause") },
    projection: {
      kind: text(projection, "kind"),
      source_cohort: {
        people: text(sourceCohort, "people"),
        site: num(sourceCohort, "site"),
        year: num(sourceCohort, "year"),
      },
      selection_lens: text(projection, "selection_lens"),
      materiality: text(projection, "materiality"),
      sampling_bias: text(projection, "sampling_bias"),
      consequences_write_back: projection.consequences_write_back === true,
    },
    moved_to: optNum(doc, "moved_to"),
    moved_year: optNum(doc, "moved_year"),
    shape: text(doc, "shape"),
    slots: list(doc, "slots").map((raw, at) => {
      const slot = object(raw, `slots[${at}]`);
      const silence = slot.silence === null || slot.silence === undefined
        ? null
        : object(slot.silence, `slots[${at}].silence`);
      return {
        key: text(slot, "key"),
        value: optText(slot, "value"),
        silence: silence === null
          ? null
          : { kind: text(silence, "kind"), reason: text(silence, "reason") },
        sources: numbers(slot, "sources"),
      };
    }),
    silences: {
      filled: num(silences, "filled"),
      no_fact: num(silences, "no_fact"),
      by_design: num(silences, "by_design"),
    },
    sources: list(doc, "sources").map((raw, at) => {
      const source = object(raw, `sources[${at}]`);
      return {
        number: num(source, "number"),
        kind: text(source, "kind"),
        entity: optText(source, "entity"),
        predicate: optText(source, "predicate"),
        caption: optText(source, "caption"),
        function: optText(source, "function"),
        inputs: optText(source, "inputs"),
      };
    }),
  };
}

/** Parse `lot/curve/v1`. */
export function parseCurve(json: string): Curve {
  const doc = object(JSON.parse(json), "lot/curve/v1");
  schema(doc, "lot/curve/v1");
  const byPeople = object(doc.births_by_people, "births_by_people");
  const series: Record<string, number[]> = {};
  for (const people of Object.keys(byPeople)) {
    series[people] = numbers(byPeople, people);
  }
  const curve: Curve = {
    schema: "lot/curve/v1",
    epoch_years: num(doc, "epoch_years"),
    start_year: num(doc, "start_year"),
    present_year: num(doc, "present_year"),
    births_by_epoch: numbers(doc, "births_by_epoch"),
    births_by_people: series,
    souls_ever: num(doc, "souls_ever"),
  };
  if (curve.epoch_years <= 0) throw new Error("epoch_years must be positive");
  if (curve.births_by_epoch.length === 0) throw new Error("births_by_epoch is empty");
  if (curve.present_year <= curve.start_year) {
    throw new Error("present_year must be after start_year");
  }
  return curve;
}

/** Parse `lot/places/v1`. */
export function parsePlaces(json: string): Places {
  const doc = object(JSON.parse(json), "lot/places/v1");
  schema(doc, "lot/places/v1");
  return {
    schema: "lot/places/v1",
    year: num(doc, "year"),
    places: list(doc, "places").map((raw, at) => {
      const place = object(raw, `places[${at}]`);
      return {
        occ: num(place, "occ"),
        entity: text(place, "entity"),
        site: num(place, "site"),
        people: text(place, "people"),
        name: optText(place, "name"),
        latitude: num(place, "latitude"),
        longitude: num(place, "longitude"),
        population: num(place, "population"),
        births_per_year: num(place, "births_per_year"),
      };
    }),
  };
}

/** Parse `lot/odds/v1`. */
export function parseOdds(json: string): Odds {
  const doc = object(JSON.parse(json), "lot/odds/v1");
  schema(doc, "lot/odds/v1");
  return {
    schema: "lot/odds/v1",
    e0: num(doc, "e0"),
    q_maturity: num(doc, "q_maturity"),
    maturity_years: num(doc, "maturity_years"),
    lifespan_years: num(doc, "lifespan_years"),
    strife: num(doc, "strife"),
    infant_share: num(doc, "infant_share"),
    background_share: num(doc, "background_share"),
    senescent_share: num(doc, "senescent_share"),
    causes: list(doc, "causes").map((raw, at) => {
      const row = object(raw, `causes[${at}]`);
      return { cause: text(row, "cause"), share: num(row, "share") };
    }),
  };
}
