/// <reference lib="dom" />

/** The world's real terrain as a spinning orthographic globe with a day/night terminator. */

import type { TilesScene } from "./scene.ts";
import { elevationColor } from "./palette.ts";

const DEG_PER_RAD = 180 / Math.PI;
const RAD_PER_DEG = Math.PI / 180;
const NIGHT: [number, number, number] = [8, 10, 20];
const NIGHT_MIX = 0.72;

/** A point on the visible sphere, in degrees; `lon` is unwrapped (not reduced mod 360). */
export interface SphereCoords {
  lat: number;
  lon: number;
}

/**
 * Inverse orthographic projection: a disc pixel offset `(px,py)` (already normalized
 * to [-1,1], y down) at rotation `rotationTurns` (turns) and axial tilt `tiltDeg`
 * (degrees) maps to sphere coordinates, or `null` if the pixel falls outside the disc.
 */
export function sample(
  px: number,
  py: number,
  _r: number,
  rotationTurns: number,
  tiltDeg: number,
): SphereCoords | null {
  const rr = px * px + py * py;
  if (rr > 1) return null;
  const z = Math.sqrt(1 - rr);
  const eps = tiltDeg * RAD_PER_DEG;
  const cosEps = Math.cos(eps);
  const sinEps = Math.sin(eps);
  const xPrime = px * cosEps + py * sinEps;
  const yPrime = -px * sinEps + py * cosEps;
  const lat = Math.asin(-yPrime) * DEG_PER_RAD;
  const lon = Math.atan2(xPrime, z) * DEG_PER_RAD + rotationTurns * 360;
  return { lat, lon };
}

/**
 * Is the disc point `(px,py)` (normalized, y down) on the day side, given the
 * on-screen sun direction `sunAngle` (radians, 0 = +x)?
 */
export function isLit(px: number, py: number, sunAngle: number): boolean {
  return px * Math.cos(sunAngle) + py * Math.sin(sunAngle) > 0;
}

function frac(x: number): number {
  return x - Math.floor(x);
}

function mix(
  color: [number, number, number],
  toward: [number, number, number],
  t: number,
): [number, number, number] {
  return [
    color[0] + (toward[0] - color[0]) * t,
    color[1] + (toward[1] - color[1]) * t,
    color[2] + (toward[2] - color[2]) * t,
  ];
}

/**
 * Render the world's committed terrain as an orthographic globe of radius `r`
 * centered at `(cx,cy)`, spun to `rotationTurns`, tilted by `tiltDeg`, lit from
 * on-screen angle `sunAngle` (radians).
 */
export function drawGlobe(
  ctx: CanvasRenderingContext2D,
  cx: number,
  cy: number,
  r: number,
  tiles: TilesScene,
  rotationTurns: number,
  tiltDeg: number,
  sunAngle: number,
): void {
  const size = 2 * r;
  const img = ctx.createImageData(size, size);
  const data = img.data;
  const { width, height, elevation_m: elevationM, sea_level_m: seaLevelM } = tiles;
  for (let row = 0; row < size; row++) {
    const y = (row - r) / r;
    for (let col = 0; col < size; col++) {
      const x = (col - r) / r;
      const s = sample(x, y, r, rotationTurns, tiltDeg);
      const idx = (row * size + col) * 4;
      if (s === null) {
        data[idx + 3] = 0;
        continue;
      }
      const tileCol = Math.min(width - 1, Math.floor(frac(s.lon / 360) * width));
      const tileRow = Math.min(height - 1, Math.max(0, Math.floor(((90 - s.lat) / 180) * height)));
      const elevation = elevationM[tileRow * width + tileCol];
      let color = elevationColor(elevation, seaLevelM);
      if (!isLit(x, y, sunAngle)) color = mix(color, NIGHT, NIGHT_MIX);
      data[idx] = color[0];
      data[idx + 1] = color[1];
      data[idx + 2] = color[2];
      data[idx + 3] = 255;
    }
  }
  ctx.putImageData(img, cx - r, cy - r);
}
