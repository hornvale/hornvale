// The Where stage's map: an equirectangular projection of `lot/places/v1`
// onto a canvas, and the hit test the "select a location" pick mode runs.
//
// The formula is the atlas's `latLonToCanvas` (`clients/atlas/src/
// projection.ts`) at a UNIT viewport — scale 1, no translation — because
// this map does not pan or zoom. Kept here rather than imported: the two
// clients are separate bundles with separate `deno.json` files, and the
// atlas's version carries a `Viewport` this exhibit has no use for.
//
// Nothing here computes a demographic quantity. A dot's radius is a
// function of the payload's own `births_per_year`, which is the quantity
// the When stage drew from — so the map shows, literally, where the next
// birth is likely to fall.

import type { Place } from "./payload.ts";

/** A point on the canvas, in canvas pixels. */
export interface Point {
  x: number;
  y: number;
}

/** Where a lat/lon lands on a `w` x `h` equirectangular canvas. */
export function project(latitude: number, longitude: number, w: number, h: number): Point {
  return { x: ((longitude + 180) / 360) * w, y: ((90 - latitude) / 180) * h };
}

/** The inverse: what lat/lon a canvas point names. */
export function unproject(x: number, y: number, w: number, h: number): {
  latitude: number;
  longitude: number;
} {
  return { latitude: 90 - (y / h) * 180, longitude: (x / w) * 360 - 180 };
}

/** The place nearest a canvas point, or null when there are none.
 *
 * Distance is measured in CANVAS pixels rather than on the sphere, which is
 * the right metric here: the reader is pointing at a picture, and the dot
 * they meant is the one their cursor is closest to on that picture. Ties
 * break on the first place in payload order, which is `occ` order, so the
 * same click always names the same place. */
export function nearestPlace(
  places: Place[],
  x: number,
  y: number,
  w: number,
  h: number,
): Place | null {
  let best: Place | null = null;
  let bestDistance = Infinity;
  for (const place of places) {
    const at = project(place.latitude, place.longitude, w, h);
    const distance = (at.x - x) ** 2 + (at.y - y) ** 2;
    if (distance < bestDistance) {
      bestDistance = distance;
      best = place;
    }
  }
  return best;
}

/** The place holding a given site vertex, or null. The drawn life names its
 * site; the map has to find that site among the year's places to put the
 * crosshair on it. */
export function placeAtSite(places: Place[], site: number): Place | null {
  return places.find((place) => place.site === site) ?? null;
}

/** A dot's radius in canvas pixels, from the payload's own
 * `births_per_year`.
 *
 * Square-root scaling, so a dot's AREA is proportional to the birth rate —
 * the reader compares areas, not radii, and a linear radius overstates the
 * big places by the square. The floor keeps a place with almost no births
 * visible rather than invisible: this is a map of where people are, and a
 * place with one birth a century is still a place. */
export function dotRadius(place: Place, peakBirths: number): number {
  const MIN = 1.2;
  const MAX = 7;
  if (peakBirths <= 0) return MIN;
  const share = Math.max(0, place.births_per_year) / peakBirths;
  return MIN + (MAX - MIN) * Math.sqrt(Math.min(1, share));
}

/** The largest `births_per_year` among the places — the scale `dotRadius`
 * measures against. */
export function peakBirths(places: Place[]): number {
  return places.reduce((most, place) => Math.max(most, place.births_per_year), 0);
}

/** How the caption names a place: its own name where the ledger has one,
 * and an honest description where it does not.
 *
 * A place with no name is NOT given an invented one (spec §2.1): the
 * caption says what it is — an unnamed occupation of that people — rather
 * than making something up. */
export function placeLabel(place: Place): string {
  return place.name ?? `an unnamed ${place.people} community`;
}
