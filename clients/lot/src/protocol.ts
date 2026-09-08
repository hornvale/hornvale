// The exhibit's worker protocol. Pure module: no DOM, no worker globals.
//
// Every reply carries the payload's RAW JSON TEXT rather than a parsed
// object. Structured-cloning a parsed payload would round `occupation` and
// `entity` — full-width entity ids that `windows/lot/src/json.rs`
// deliberately emits as decimal strings because JSON numbers are doubles —
// and would also make the worker responsible for validation the page's own
// parsers already do. Text in, text out; `payload.ts` is the only parser.

/** Page -> worker: derive a world. Seed travels as decimal text. */
export interface NewRequest {
  kind: "new";
  seed: string;
}

/** Page -> worker: draw lot `index`, with pins if any. */
export interface LotRequest {
  kind: "lot";
  /** Decimal text, for the same reason the seed is. */
  index: string;
  /** The birth year to pin, or null for a free draw. */
  year: number | null;
  /** The site vertex to pin, or null. */
  site: number | null;
}

/** Page -> worker: the births curve. */
export interface CurveRequest {
  kind: "curve";
}

/** Page -> worker: the occupations alive at `year`. */
export interface PlacesRequest {
  kind: "places";
  year: number;
}

/** Page -> worker: mortality odds for one occupation at one year. */
export interface OddsRequest {
  kind: "odds";
  occ: number;
  year: number;
}

/** Anything the page may ask. */
export type WorkerRequest = NewRequest | LotRequest | CurveRequest | PlacesRequest | OddsRequest;

/** Worker -> page: the answer to a request, as the payload's own JSON. */
export interface OkResponse {
  kind: "ok";
  /** Which request this answers, echoed so a late reply is discardable. */
  answers: WorkerRequest["kind"];
  /** A monotone request id, echoed. */
  id: number;
  /** The payload text, or "" for a `new` that carries no payload. */
  json: string;
}

/** Worker -> page: the sim's own error, or the wasm's. */
export interface ErrorResponse {
  kind: "error";
  answers: WorkerRequest["kind"];
  id: number;
  text: string;
}

/** Anything the worker may reply. */
export type WorkerResponse = OkResponse | ErrorResponse;

/** A request with the id the page tracks it by. */
export interface Envelope {
  id: number;
  request: WorkerRequest;
}

/** The sentinel `hl_lot_pinned` reads as "no site pin" — `u32::MAX`. Vertex
 * 0 is a legal site, so the ABI needs a value outside the range rather than
 * a zero. */
export const NO_SITE = 4294967295;

/** The site argument for a pin: the vertex, or the sentinel. */
export function siteArgument(site: number | null): number {
  return site === null ? NO_SITE : site;
}
