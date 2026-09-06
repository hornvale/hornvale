import { assert, assertAlmostEquals, assertEquals } from "@std/assert";
import {
  dotRadius,
  nearestPlace,
  peakBirths,
  placeAtSite,
  placeLabel,
  project,
  unproject,
} from "./map.ts";
import { places } from "./fixtures.ts";

const W = 1024;
const H = 512;

Deno.test("the projection puts the corners where equirectangular says", () => {
  assertEquals(project(90, -180, W, H), { x: 0, y: 0 });
  assertEquals(project(-90, 180, W, H), { x: W, y: H });
  assertEquals(project(0, 0, W, H), { x: W / 2, y: H / 2 });
});

Deno.test("project and unproject round-trip", () => {
  for (
    const [lat, lon] of [[0, 0], [16, 122.6], [-4.0014842, -145.74382], [-60, 179.9], [89, -179]]
  ) {
    const at = project(lat, lon, W, H);
    const back = unproject(at.x, at.y, W, H);
    assertAlmostEquals(back.latitude, lat, 1e-9);
    assertAlmostEquals(back.longitude, lon, 1e-9);
  }
});

Deno.test("nearestPlace finds the dot under the cursor", () => {
  const p = places();
  for (const place of p) {
    const at = project(place.latitude, place.longitude, W, H);
    assertEquals(nearestPlace(p, at.x, at.y, W, H)?.site, place.site);
    // Nudged a couple of pixels, still the same dot.
    assertEquals(nearestPlace(p, at.x + 2, at.y - 2, W, H)?.site, place.site);
  }
});

Deno.test("nearestPlace on an empty year is null, not a throw", () => {
  assertEquals(nearestPlace([], 10, 10, W, H), null);
});

Deno.test("placeAtSite finds the drawn life's own site", () => {
  assertEquals(placeAtSite(places(), 10630)?.name, "Raaxora");
  assertEquals(placeAtSite(places(), 999999), null);
});

Deno.test("dot area is proportional to the payload's birth rate", () => {
  const p = places();
  const peak = peakBirths(p);
  assertEquals(peak, 1.4);
  const big = dotRadius(p[1], peak);
  const small = dotRadius(p[2], peak);
  assert(big > small, "a busier place draws larger");
  // The floor keeps a near-silent place visible rather than invisible.
  assert(small >= 1.2);
  assert(big <= 7);
});

Deno.test("dotRadius survives a year with no births at all", () => {
  assertEquals(dotRadius(places()[0], 0), 1.2);
});

Deno.test("an unnamed place is described, never named", () => {
  const p = places();
  assertEquals(placeLabel(p[1]), "Raaxora");
  assertEquals(placeLabel(p[2]), "an unnamed goblin community");
});
