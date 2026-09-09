import { FLAGSHIP_MARK, SETTLEMENT_MARK } from "./palette.ts";
import { initialViewport, type Viewport as AtlasViewport } from "./projection.ts";

const FRAME_SCHEMA = "observation/frame/v1";

export interface FramePacket {
  schema: string;
  episode_id: string;
  frame_index: number;
  world_seed: string;
  world_revision: string;
  time_day: number | null;
  title: string;
  labels: Record<string, string>;
  spatial: {
    source: string;
    readout: string;
  };
  source_digest: string;
}

export interface ViewportDimensions {
  width: number;
  height: number;
}

export interface Bounds {
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface RenderState {
  viewport: ViewportDimensions;
  layout: "phone" | "laptop";
  title: string;
  objectLabel: string;
  scaleLabel: string;
  countUnitLabel: string;
  legend: Array<{ key: string; value: string }>;
  map: {
    bounds: Bounds;
    source: string;
    content: string;
    projection: AtlasViewport;
    palette: {
      foreground: string;
      accent: string;
    };
  };
  annotation: {
    bounds: Bounds;
    text: string;
  };
  provenance: {
    episodeId: string;
    frameIndex: number;
    worldSeed: string;
    worldRevision: string;
    timeDay: number | null;
    sourceDigest: string;
  };
}

/** A frame cannot be composed without its known provenance contract. */
export class ObservationFrameError extends Error {}

function record(value: unknown, field: string): Record<string, unknown> {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new ObservationFrameError(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
}

function text(value: unknown, field: string): string {
  if (typeof value !== "string" || value.length === 0) {
    throw new ObservationFrameError(`${field} must be a non-empty string`);
  }
  return value;
}

function canonicalU64(value: unknown): string {
  const seed = text(value, "world_seed");
  if ((seed.length > 1 && seed.startsWith("0")) || !/^\d+$/.test(seed)) {
    throw new ObservationFrameError("world_seed must be a canonical decimal string");
  }
  try {
    if (BigInt(seed) > 18446744073709551615n) {
      throw new ObservationFrameError("world_seed exceeds u64 maximum");
    }
  } catch (error) {
    if (error instanceof ObservationFrameError) throw error;
    throw new ObservationFrameError("world_seed must be a canonical decimal string");
  }
  return seed;
}

/** Parse the producer packet and enforce the renderer's lossless input contract. */
export function parseObservationFramePacket(input: string): FramePacket {
  let value: unknown;
  try {
    value = JSON.parse(input);
  } catch (error) {
    throw new ObservationFrameError(`JSON is malformed: ${error}`);
  }
  const doc = record(value, "packet");
  if (doc.schema !== FRAME_SCHEMA) {
    throw new ObservationFrameError(`schema must be ${FRAME_SCHEMA}, got ${String(doc.schema)}`);
  }
  const labels = record(doc.labels, "labels");
  const spatial = record(doc.spatial, "spatial");
  const worldSeed = canonicalU64(doc.world_seed);
  const frameIndex = doc.frame_index;
  if (typeof frameIndex !== "number" || !Number.isInteger(frameIndex) || frameIndex < 0) {
    throw new ObservationFrameError("frame_index must be a non-negative integer");
  }
  if (
    (typeof doc.time_day !== "number" && doc.time_day !== null) ||
    (typeof doc.time_day === "number" && !Number.isFinite(doc.time_day))
  ) {
    throw new ObservationFrameError("time_day must be a number or null");
  }
  const labelValues: Record<string, string> = {};
  for (const [key, value] of Object.entries(labels)) {
    labelValues[key] = text(value, `labels.${key}`);
  }
  for (const key of ["object", "scale", "primary_axis", "observation_sentence", "count_unit"]) {
    text(labelValues[key], `labels.${key}`);
  }
  return {
    schema: FRAME_SCHEMA,
    episode_id: text(doc.episode_id, "episode_id"),
    frame_index: frameIndex,
    world_seed: worldSeed,
    world_revision: text(doc.world_revision, "world_revision"),
    time_day: doc.time_day as number | null,
    title: text(doc.title, "title"),
    labels: labelValues,
    spatial: {
      source: text(spatial.source, "spatial.source"),
      readout: text(spatial.readout, "spatial.readout"),
    },
    source_digest: text(doc.source_digest, "source_digest"),
  };
}

function bounds(x: number, y: number, width: number, height: number): Bounds {
  return { x, y, width: Math.max(1, width), height: Math.max(1, height) };
}

function compareCodePoints(left: string, right: string): number {
  const a = Array.from(left, (character) => character.codePointAt(0)!);
  const b = Array.from(right, (character) => character.codePointAt(0)!);
  for (let index = 0; index < Math.min(a.length, b.length); index++) {
    if (a[index] !== b[index]) return a[index] - b[index];
  }
  return a.length - b.length;
}

/**
 * Compose one observation frame without consulting the DOM or the simulated world.
 * Every semantic string in the result is copied from the validated packet.
 */
export function renderObservationFrame(
  packet: FramePacket,
  viewport: ViewportDimensions,
): RenderState {
  if (packet.schema !== FRAME_SCHEMA) {
    throw new ObservationFrameError(
      `schema must be ${FRAME_SCHEMA}, got ${String(packet.schema)}`,
    );
  }
  if (typeof packet.source_digest !== "string" || packet.source_digest.length === 0) {
    throw new ObservationFrameError("source_digest must be present");
  }
  canonicalU64(packet.world_seed);
  if (packet.time_day !== null && !Number.isFinite(packet.time_day)) {
    throw new ObservationFrameError("time_day must be finite");
  }
  if (
    !Number.isFinite(viewport.width) || !Number.isFinite(viewport.height) ||
    viewport.width <= 0 || viewport.height <= 0
  ) {
    throw new ObservationFrameError("viewport dimensions must be positive and finite");
  }

  const phone = viewport.width < 720;
  const gutter = phone ? 16 : 32;
  const headerBottom = phone ? 132 : 116;
  const annotationHeight = phone ? 132 : 116;
  const availableHeight = viewport.height - headerBottom - annotationHeight - gutter * 2;
  const laptopRailWidth = phone ? 0 : Math.min(320, viewport.width * 0.25);
  const mapWidth = viewport.width - gutter * 2 - laptopRailWidth - (phone ? 0 : gutter);
  const mapHeight = Math.min(mapWidth / 2, availableHeight);
  const mapBounds = bounds(gutter, headerBottom, mapWidth, mapHeight);
  const annotationBounds = phone
    ? bounds(gutter, mapBounds.y + mapBounds.height + gutter, mapWidth, annotationHeight)
    : bounds(
      mapBounds.x + mapBounds.width + gutter,
      headerBottom,
      laptopRailWidth,
      annotationHeight,
    );

  const legend = Object.entries(packet.labels)
    .filter(([key]) => key !== "observation_sentence")
    .sort(([left], [right]) => compareCodePoints(left, right))
    .map(([key, value]) => ({ key, value }));

  return {
    viewport: { width: viewport.width, height: viewport.height },
    layout: phone ? "phone" : "laptop",
    title: packet.title,
    objectLabel: packet.labels.object,
    scaleLabel: packet.labels.scale,
    countUnitLabel: packet.labels.count_unit,
    legend,
    map: {
      bounds: mapBounds,
      source: packet.spatial.source,
      content: packet.spatial.readout,
      projection: initialViewport(),
      palette: {
        foreground: SETTLEMENT_MARK,
        accent: FLAGSHIP_MARK,
      },
    },
    annotation: {
      bounds: annotationBounds,
      text: packet.labels.observation_sentence,
    },
    provenance: {
      episodeId: packet.episode_id,
      frameIndex: packet.frame_index,
      worldSeed: packet.world_seed,
      worldRevision: packet.world_revision,
      timeDay: packet.time_day,
      sourceDigest: packet.source_digest,
    },
  };
}

function escapeHtml(value: string): string {
  return value.replace(/[&<>"']/g, (character) =>
    ({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;",
    })[character]!);
}

/** Produce a self-contained browser-inspectable SVG/HTML preview for one frame. */
export function renderObservationFrameHtml(
  packet: FramePacket,
  viewport: ViewportDimensions,
): string {
  const state = renderObservationFrame(packet, viewport);
  const legend = state.legend.map(({ key, value }) =>
    `<li><span>${escapeHtml(key)}</span>: ${escapeHtml(value)}</li>`
  ).join("");
  const map = state.map;
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${escapeHtml(state.title)}</title>
<style>body{margin:0;background:#f3f0e8;color:#1a1a19;font:16px system-ui,sans-serif}main{box-sizing:border-box;padding:24px;max-width:1440px;margin:auto}svg{display:block;width:100%;background:#e1d7be;border:2px solid #1a1a19}pre{box-sizing:border-box;margin:0;padding:16px;overflow:auto;font:12px ui-monospace,monospace;line-height:1.35}aside{padding:16px 0}ul{padding-left:20px}</style></head>
<body><main data-layout="${state.layout}" data-width="${viewport.width}" data-height="${viewport.height}">
<header><h1>${escapeHtml(state.title)}</h1><p>${escapeHtml(state.objectLabel)} · ${
    escapeHtml(state.scaleLabel)
  }${state.countUnitLabel ? ` · ${escapeHtml(state.countUnitLabel)}` : ""}</p></header>
<section data-map data-source="${escapeHtml(map.source)}"><svg role="img" aria-label="${
    escapeHtml(map.source)
  }" viewBox="0 0 ${map.bounds.width} ${map.bounds.height}" width="${map.bounds.width}" height="${map.bounds.height}"><rect width="100%" height="100%" fill="#e1d7be"></rect><foreignObject x="0" y="0" width="100%" height="100%"><pre>${
    escapeHtml(map.content)
  }</pre></foreignObject></svg></section>
<section aria-label="Legend"><h2>Legend</h2><ul>${legend}</ul></section>
<aside aria-label="Observation"><p>${escapeHtml(state.annotation.text)}</p></aside>
<footer data-provenance><p>Episode ${
    escapeHtml(state.provenance.episodeId)
  } · frame ${state.provenance.frameIndex} · seed ${
    escapeHtml(state.provenance.worldSeed)
  } · revision ${escapeHtml(state.provenance.worldRevision)} · time ${
    String(state.provenance.timeDay)
  } · source ${escapeHtml(state.provenance.sourceDigest)}</p></footer>
</main></body></html>`;
}

export interface PreviewFrame {
  state: RenderState;
  html: string;
}

export interface ObservationPreview {
  phone: PreviewFrame;
  laptop: PreviewFrame;
}

/** Build both inspection targets from one validated packet for browser review. */
export function renderObservationPreview(packet: FramePacket): ObservationPreview {
  const phone = { width: 390, height: 844 };
  const laptop = { width: 1440, height: 900 };
  return {
    phone: {
      state: renderObservationFrame(packet, phone),
      html: renderObservationFrameHtml(packet, phone),
    },
    laptop: {
      state: renderObservationFrame(packet, laptop),
      html: renderObservationFrameHtml(packet, laptop),
    },
  };
}
