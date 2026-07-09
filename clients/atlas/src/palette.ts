import type { TilesScene } from "./scene.ts";

/** The five switchable layers. */
export type Layer = "biome" | "elevation" | "plate" | "unrest" | "ocean";
/** Display order for the layer switcher. */
export const LAYERS: Layer[] = ["biome", "elevation", "plate", "unrest", "ocean"];

/** The 22 biome colors, legend order — the biome raster's exact values. */
const BIOME_RGB: [number, number, number][] = [
  [235, 235, 245], // ice
  [170, 175, 155], // tundra
  [70, 105, 80], // taiga
  [160, 180, 100], // temperate-grassland
  [155, 150, 95], // shrubland
  [60, 130, 70], // temperate-forest
  [35, 100, 60], // temperate-rainforest
  [210, 195, 130], // desert
  [180, 165, 85], // savanna
  [90, 150, 65], // tropical-seasonal-forest
  [25, 110, 45], // tropical-rainforest
  [150, 140, 135], // alpine
  [220, 230, 240], // sea-ice
  [230, 150, 160], // coral-reef
  [40, 90, 95], // kelp-forest
  [120, 60, 90], // hydrothermal-vent
  [10, 15, 45], // hadal-trench
  [60, 160, 170], // upwelling
  [70, 140, 200], // epipelagic
  [45, 95, 160], // mesopelagic
  [25, 55, 110], // bathypelagic
  [12, 30, 70], // abyssal
];

/** The reference categorical eight, fixed slot order (validated set). */
const PLATE_RGB: [number, number, number][] = [
  [0x2a, 0x78, 0xd6],
  [0x1b, 0xaf, 0x7a],
  [0xed, 0xa1, 0x00],
  [0x00, 0x83, 0x00],
  [0x4a, 0x3a, 0xa7],
  [0xe3, 0x49, 0x48],
  [0xe8, 0x7b, 0xa4],
  [0xeb, 0x68, 0x34],
];

/** Orange sequential ramp, light (calm) to dark (violent), monotonic lightness. */
const UNREST_RGB: [number, number, number][] = [
  [0xfd, 0xe3, 0xd3],
  [0xf8, 0xc4, 0xa5],
  [0xf2, 0xa2, 0x76],
  [0xeb, 0x68, 0x34],
  [0xd9, 0x59, 0x26],
  [0xb3, 0x43, 0x17],
  [0x8c, 0x30, 0x0d],
  [0x66, 0x21, 0x04],
];

/** Ocean-layer water (the elevation ramp's mid-ocean blue). */
const WATER: [number, number, number] = [45, 95, 160];
/** Ocean-layer land (warm parchment). */
const LAND: [number, number, number] = [225, 215, 190];

/** Settlement marker: near-black dot (a white ring is drawn around it). */
export const SETTLEMENT_MARK = "#1a1a19";
/** Flagship marker: gold. */
export const FLAGSHIP_MARK = "#eda100";

function lerp(a: [number, number, number], b: [number, number, number], t: number) {
  const c = Math.min(1, Math.max(0, t));
  return [
    Math.round(a[0] + (b[0] - a[0]) * c),
    Math.round(a[1] + (b[1] - a[1]) * c),
    Math.round(a[2] + (b[2] - a[2]) * c),
  ] as [number, number, number];
}

/** The elevation raster's exact ramp: ocean blues by depth, land green→tan→brown→white. */
function elevationColor(elevation: number, seaLevel: number): [number, number, number] {
  if (elevation < seaLevel) {
    return lerp([70, 130, 200], [10, 30, 80], (seaLevel - elevation) / 6000);
  }
  const height = elevation - seaLevel;
  if (height < 800) return lerp([60, 140, 70], [150, 160, 90], height / 800);
  if (height < 2500) return lerp([150, 160, 90], [140, 100, 70], (height - 800) / 1700);
  return lerp([140, 100, 70], [245, 245, 245], (height - 2500) / 2500);
}

function tileColor(scene: TilesScene, layer: Layer, i: number): [number, number, number] {
  switch (layer) {
    case "biome":
      return BIOME_RGB[scene.biome[i]];
    case "elevation":
      return elevationColor(scene.elevation_m[i], scene.sea_level_m);
    case "plate":
      return PLATE_RGB[scene.plate[i] % PLATE_RGB.length];
    case "unrest":
      return UNREST_RGB[Math.min(Math.floor(scene.unrest[i] * 8), 7)];
    case "ocean":
      return scene.ocean[i] ? WATER : LAND;
  }
}

/** One layer as RGBA pixels (width × height × 4, row-major, fully opaque). */
export function layerPixels(scene: TilesScene, layer: Layer): Uint8ClampedArray {
  const out = new Uint8ClampedArray(scene.width * scene.height * 4);
  for (let i = 0; i < scene.width * scene.height; i++) {
    const [r, g, b] = tileColor(scene, layer, i);
    out[i * 4] = r;
    out[i * 4 + 1] = g;
    out[i * 4 + 2] = b;
    out[i * 4 + 3] = 255;
  }
  return out;
}
