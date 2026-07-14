// The Casement's worker protocol and seed parsing. Pure module: no DOM,
// no worker globals — everything here is unit-tested.

/** Page -> worker: derive a world and start the possession. Seed travels
 * as a decimal string (already validated by parseSeed). */
export interface StartRequest {
  type: "start";
  seed: string;
}

/** Page -> worker: one verb line for the live session. */
export interface CommandRequest {
  type: "command";
  line: string;
}

export type WorkerRequest = StartRequest | CommandRequest;

/** Worker -> page: genesis succeeded; text is the opening. */
export interface StartedResponse {
  type: "started";
  text: string;
}

/** Worker -> page: genesis or wasm failure; text is the sim's own error. */
export interface ErrorResponse {
  type: "error";
  text: string;
}

/** Worker -> page: one verb's response; released ends the possession. */
export interface OutResponse {
  type: "out";
  text: string;
  released: boolean;
}

export type WorkerResponse = StartedResponse | ErrorResponse | OutResponse;

const U64_MAX = 18446744073709551615n;

/** Parse a decimal u64, or null. Trims whitespace; rejects signs, hex,
 * fractions, and anything past u64::MAX (a seed is a world's identity —
 * silent wrapping would possess the wrong world). */
export function parseSeed(input: string): bigint | null {
  const t = input.trim();
  if (!/^[0-9]+$/.test(t)) return null;
  const v = BigInt(t);
  return v <= U64_MAX ? v : null;
}

/** The seed named by ?seed=N in a URL search string, else 42n. */
export function seedFromSearch(search: string): bigint {
  const raw = new URLSearchParams(search).get("seed");
  return (raw === null ? null : parseSeed(raw)) ?? 42n;
}
