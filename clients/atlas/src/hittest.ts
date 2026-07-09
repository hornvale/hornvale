import type { Feature, TilesScene } from "./scene.ts";
import { latLonToCanvas, tileLatLon, type Viewport } from "./projection.ts";

/** Feature hover radius, canvas px. */
const HIT_RADIUS = 8;

/** One readout line for a tile: position, elevation, biome, plate, unrest, water. */
export function readout(scene: TilesScene, px: number, py: number): string {
  const i = py * scene.width + px;
  const { latitude, longitude } = tileLatLon(scene, px, py);
  const water = scene.ocean[i] ? "ocean" : "land";
  return `${latitude.toFixed(1)}°, ${longitude.toFixed(1)}° — ` +
    `${scene.elevation_m[i].toFixed(1)} m, ${scene.biome_legend[scene.biome[i]]}, ` +
    `plate ${scene.plate[i]}, unrest ${scene.unrest[i].toFixed(2)}, ${water}`;
}

/** The nearest feature within the hit radius of a canvas point, or null. */
export function featureAt(
  scene: TilesScene,
  v: Viewport,
  x: number,
  y: number,
  cw: number,
  ch: number,
): Feature | null {
  let best: Feature | null = null;
  let bestD = HIT_RADIUS;
  for (const f of scene.features) {
    const p = latLonToCanvas(v, f.latitude, f.longitude, cw, ch);
    const d = Math.hypot(p.x - x, p.y - y);
    if (d <= bestD) {
      bestD = d;
      best = f;
    }
  }
  return best;
}

/** A feature's canvas position (marker drawing). */
export function featureCanvasXY(
  v: Viewport,
  f: Feature,
  cw: number,
  ch: number,
): { x: number; y: number } {
  return latLonToCanvas(v, f.latitude, f.longitude, cw, ch);
}
