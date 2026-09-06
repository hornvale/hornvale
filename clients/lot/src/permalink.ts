// The permalink: `#<seed>/<index>`, or the pinned form
// `#<seed>/<index>?year=Y&site=V`.
//
// **The permalink IS the key** (spec §6.4, decision 0796). Two readers of
// one link must see one life, which is true only because the wasm draws it
// from `(seed, index)` and the pins — so this module's whole job is to
// carry those four values through a URL without losing a digit.
//
// Seed and index travel as DECIMAL STRINGS, never as `number`. Both are
// `u64` at the ABI; JavaScript's `number` is an IEEE-754 double and is
// lossy above 2^53, so parsing a large seed into one and formatting it back
// would silently name a different world. `BigInt` is the only safe carrier,
// and text is how it reaches the URL.

/** A parsed permalink. */
export interface Permalink {
  /** Decimal text, exactly as it appeared. */
  seed: string;
  /** Decimal text, exactly as it appeared. */
  index: string;
  /** The pinned birth year, or null. */
  year: number | null;
  /** The pinned site vertex, or null. */
  site: number | null;
}

/** The pins a draw was made with. */
export interface Pick {
  year: number | null;
  site: number | null;
}

const U64_MAX = 18446744073709551615n;

/** A decimal `u64` as text, or null. Rejects signs, hex, fractions, and
 * anything past `u64::MAX` — a seed is a world's identity, and silent
 * wrapping would show the wrong world (the Casement's `parseSeed`, same
 * rule for the same reason). */
function u64Text(input: string): string | null {
  const t = input.trim();
  if (!/^[0-9]+$/.test(t)) return null;
  return BigInt(t) <= U64_MAX ? t : null;
}

/** A finite decimal number, or null. Used for the year pin, which is an
 * `f64` at the ABI. */
function finite(input: string): number | null {
  const t = input.trim();
  if (!/^-?[0-9]+(\.[0-9]+)?$/.test(t)) return null;
  const value = Number(t);
  return Number.isFinite(value) ? value : null;
}

/** A `u32` vertex index, or null. */
function u32(input: string): number | null {
  const t = input.trim();
  if (!/^[0-9]+$/.test(t)) return null;
  const value = Number(t);
  return Number.isInteger(value) && value <= 4294967295 ? value : null;
}

/** Parse a location hash. Returns null for anything that is not a whole,
 * well-formed permalink — a half-parsed one would draw a different life
 * than the link names, which is worse than starting fresh. */
export function parse(hash: string): Permalink | null {
  const raw = hash.startsWith("#") ? hash.slice(1) : hash;
  if (raw === "") return null;
  const [path, query] = raw.split("?", 2);
  const parts = path.split("/");
  if (parts.length !== 2) return null;
  const seed = u64Text(parts[0]);
  const index = u64Text(parts[1]);
  if (seed === null || index === null) return null;

  const params = new URLSearchParams(query ?? "");
  const rawYear = params.get("year");
  const rawSite = params.get("site");
  const year = rawYear === null ? null : finite(rawYear);
  const site = rawSite === null ? null : u32(rawSite);
  if (rawYear !== null && year === null) return null;
  if (rawSite !== null && site === null) return null;
  // A site pin with no year is not a link this exhibit ever writes, and the
  // ABI cannot express it: `hl_lot_pinned` takes a finite year, and the
  // Where stage only offers a location once a year has settled. Refusing it
  // outright beats silently dropping half the pin and drawing a life the
  // link does not name.
  if (site !== null && year === null) return null;
  return { seed, index, year, site };
}

/** Format a permalink. Seed and index are already decimal text; a pin is
 * omitted entirely rather than written as an empty parameter, so an
 * unpinned link round-trips to exactly `#seed/index`. */
export function format(seed: string, index: string, pick: Pick): string {
  const params = new URLSearchParams();
  if (pick.year !== null) params.set("year", String(pick.year));
  if (pick.site !== null) params.set("site", String(pick.site));
  const query = params.toString();
  return `#${seed}/${index}${query === "" ? "" : `?${query}`}`;
}
