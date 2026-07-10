import { assert, assertEquals } from "@std/assert";
import {
  canvasToTile,
  clampViewport,
  initialViewport,
  pan,
  tileLatLon,
  zoomAt,
} from "./projection.ts";
import type { TilesScene } from "./scene.ts";

// Only lattice dims matter for projection.
const scene = { width: 256, height: 128 } as TilesScene;
const CW = 1024, CH = 512; // canvas px: 4 px per tile at scale 1

Deno.test("initial viewport shows the whole lattice", () => {
  const v = initialViewport();
  assertEquals(v.scale, 1);
  assertEquals(canvasToTile(v, 0, 0, scene, CW, CH), { px: 0, py: 0 });
  assertEquals(canvasToTile(v, CW - 1, CH - 1, scene, CW, CH), { px: 255, py: 127 });
});

Deno.test("canvas-tile round trip at zoom", () => {
  let v = initialViewport();
  v = zoomAt(v, CW / 2, CH / 2, 4, CW, CH);
  const t = canvasToTile(v, CW / 2, CH / 2, scene, CW, CH);
  assert(t !== null);
  // The tile under the cursor is unchanged by zooming at the cursor.
  assertEquals(t, canvasToTile(initialViewport(), CW / 2, CH / 2, scene, CW, CH));
});

Deno.test("zoom clamps to 1..16", () => {
  let v = initialViewport();
  v = zoomAt(v, 0, 0, 0.01, CW, CH);
  assertEquals(v.scale, 1);
  for (let i = 0; i < 20; i++) v = zoomAt(v, 0, 0, 2, CW, CH);
  assertEquals(v.scale, 16);
});

Deno.test("pan clamps so the lattice never fully leaves view", () => {
  let v = initialViewport();
  v = zoomAt(v, 0, 0, 4, CW, CH);
  v = pan(v, 1e6, 1e6, CW, CH);
  const c = clampViewport(v, CW, CH);
  assertEquals(c.tx, 0);
  assertEquals(c.ty, 0);
  v = pan(v, -1e7, -1e7, CW, CH);
  const c2 = clampViewport(v, CW, CH);
  assertEquals(c2.tx, CW - CW * c2.scale);
  assertEquals(c2.ty, CH - CH * c2.scale);
});

Deno.test("tile centers follow the schema grid convention", () => {
  const { latitude, longitude } = tileLatLon(scene, 0, 0);
  // px 0, py 0: lon = (0.5/256)*360 - 180; lat = 90 - (0.5/128)*180
  assertEquals(longitude, 0.5 / 256 * 360 - 180);
  assertEquals(latitude, 90 - 0.5 / 128 * 180);
});

Deno.test("the clamp keeps the lattice covering the whole canvas", () => {
  // A consequence of the translation clamp: no in-canvas cursor position is
  // ever off-lattice, at any zoom, after any pan.
  let v = initialViewport();
  v = zoomAt(v, CW / 2, CH / 2, 4, CW, CH);
  for (const [dx, dy] of [[1e6, 0], [-1e7, -1e7], [0, 1e6]]) {
    v = pan(v, dx, dy, CW, CH);
    for (const [x, y] of [[0, 0], [CW - 1, 0], [0, CH - 1], [CW - 1, CH - 1]]) {
      assert(canvasToTile(v, x, y, scene, CW, CH) !== null, `corner ${x},${y} off-lattice`);
    }
  }
});
