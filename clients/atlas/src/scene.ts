/** A named point on the lattice (scene/tiles/v1 `features`). */
export interface Feature {
  name: string;
  kind: string;
  latitude: number;
  longitude: number;
}

/** One scene/tiles/v1 document. Mirrors the schema reference page. */
export interface TilesScene {
  schema: string;
  seed: number;
  width: number;
  height: number;
  sea_level_m: number;
  elevation_m: number[];
  ocean: boolean[];
  biome: number[];
  biome_legend: string[];
  plate: number[];
  unrest: number[];
  features: Feature[];
}

/** A scene document violated the contract; the message names how. */
export class SceneFormatError extends Error {}

const SCHEMA = "scene/tiles/v1";

function fail(message: string): never {
  throw new SceneFormatError(message);
}

function numberArray(doc: Record<string, unknown>, key: string, length: number): number[] {
  const value = doc[key];
  if (!Array.isArray(value) || value.length !== length) {
    fail(`${key} must be an array of ${length} numbers`);
  }
  for (const v of value) if (typeof v !== "number") fail(`${key} holds a non-number`);
  return value as number[];
}

/** Parse and validate a scene/tiles/v1 document; throw SceneFormatError naming any violation. */
export function parseScene(text: string): TilesScene {
  let doc: Record<string, unknown>;
  try {
    doc = JSON.parse(text) as Record<string, unknown>;
  } catch (e) {
    fail(`not JSON: ${e}`);
  }
  if (typeof doc !== "object" || doc === null || Array.isArray(doc)) {
    fail("document must be a JSON object");
  }
  if (doc.schema !== SCHEMA) fail(`schema must be ${SCHEMA}, got ${String(doc.schema)}`);
  const width = doc.width;
  const height = doc.height;
  if (
    typeof width !== "number" || typeof height !== "number" || height * 2 !== width ||
    !Number.isInteger(width) || width <= 0 || !Number.isInteger(height) || height <= 0
  ) {
    fail(`height must be width / 2, got ${String(width)}×${String(height)}`);
  }
  if (typeof doc.seed !== "number") fail("seed must be a number");
  if (typeof doc.sea_level_m !== "number") fail("sea_level_m must be a number");
  const tiles = width * height;
  const legend = doc.biome_legend;
  if (!Array.isArray(legend) || legend.some((n) => typeof n !== "string")) {
    fail("biome_legend must be an array of strings");
  }
  const ocean = doc.ocean;
  if (
    !Array.isArray(ocean) || ocean.length !== tiles || ocean.some((v) => typeof v !== "boolean")
  ) {
    fail(`ocean must be an array of ${tiles} booleans`);
  }
  const biome = numberArray(doc, "biome", tiles);
  if (biome.some((b) => !Number.isInteger(b) || b < 0 || b >= legend.length)) {
    fail("biome holds an index outside the legend");
  }
  const features = doc.features;
  if (!Array.isArray(features)) fail("features must be an array");
  for (const f of features) {
    const feature = f as Record<string, unknown>;
    if (
      typeof feature.name !== "string" || typeof feature.kind !== "string" ||
      typeof feature.latitude !== "number" || typeof feature.longitude !== "number"
    ) {
      fail("feature must have string name/kind and numeric latitude/longitude");
    }
  }
  return {
    schema: SCHEMA,
    seed: doc.seed,
    width,
    height,
    sea_level_m: doc.sea_level_m,
    elevation_m: numberArray(doc, "elevation_m", tiles),
    ocean: ocean as boolean[],
    biome,
    biome_legend: legend as string[],
    plate: numberArray(doc, "plate", tiles),
    unrest: numberArray(doc, "unrest", tiles),
    features: features as unknown as Feature[],
  };
}
