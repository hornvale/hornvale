import type { TilesScene } from "./scene.ts";

/** The pan/zoom state: lattice-to-canvas scale and translation, canvas px. */
export interface Viewport {
  scale: number;
  tx: number;
  ty: number;
}

/** Scale bounds (spec §3): the whole lattice at 1, 16 px-per-base-px at most. */
export const MIN_SCALE = 1;
export const MAX_SCALE = 16;

/** The whole lattice in view. */
export function initialViewport(): Viewport {
  return { scale: 1, tx: 0, ty: 0 };
}

/** Clamp translation so the lattice never fully leaves the canvas. */
export function clampViewport(v: Viewport, cw: number, ch: number): Viewport {
  const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.scale));
  const minTx = cw - cw * scale;
  const minTy = ch - ch * scale;
  return {
    scale,
    tx: Math.min(0, Math.max(minTx, v.tx)),
    ty: Math.min(0, Math.max(minTy, v.ty)),
  };
}

/** Zoom by `factor` keeping the lattice point under the cursor fixed. */
export function zoomAt(
  v: Viewport,
  cursorX: number,
  cursorY: number,
  factor: number,
  cw: number,
  ch: number,
): Viewport {
  const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.scale * factor));
  const ratio = scale / v.scale;
  return clampViewport(
    { scale, tx: cursorX - ratio * (cursorX - v.tx), ty: cursorY - ratio * (cursorY - v.ty) },
    cw,
    ch,
  );
}

/** Translate by a drag delta. */
export function pan(v: Viewport, dx: number, dy: number, cw: number, ch: number): Viewport {
  return clampViewport({ scale: v.scale, tx: v.tx + dx, ty: v.ty + dy }, cw, ch);
}

/** The tile under a canvas point, or null when off the lattice. */
export function canvasToTile(
  v: Viewport,
  x: number,
  y: number,
  scene: TilesScene,
  cw: number,
  ch: number,
): { px: number; py: number } | null {
  const fx = (x - v.tx) / v.scale / cw;
  const fy = (y - v.ty) / v.scale / ch;
  if (fx < 0 || fx >= 1 || fy < 0 || fy >= 1) return null;
  return { px: Math.floor(fx * scene.width), py: Math.floor(fy * scene.height) };
}

/** The tile center's lat/lon — the schema's grid convention. */
export function tileLatLon(
  scene: TilesScene,
  px: number,
  py: number,
): { latitude: number; longitude: number } {
  return {
    latitude: 90 - (py + 0.5) / scene.height * 180,
    longitude: (px + 0.5) / scene.width * 360 - 180,
  };
}

/** A lat/lon's canvas position under the viewport. */
export function latLonToCanvas(
  v: Viewport,
  latitude: number,
  longitude: number,
  cw: number,
  ch: number,
): { x: number; y: number } {
  const fx = (longitude + 180) / 360;
  const fy = (90 - latitude) / 180;
  return { x: v.tx + fx * cw * v.scale, y: v.ty + fy * ch * v.scale };
}
