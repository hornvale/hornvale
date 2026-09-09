import { FLAGSHIP_MARK, SETTLEMENT_MARK } from "./palette.ts";
import { initialViewport, type Viewport as AtlasViewport } from "./projection.ts";

const FRAME_SCHEMA = "observation/frame/v1";

export interface FramePacket {
  /** Optional until the producer begins emitting an explicit schema tag. */
  schema?: string;
  episode_id: string;
  frame_index: number;
  world_seed: number;
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
    worldSeed: number;
    worldRevision: string;
    timeDay: number | null;
    sourceDigest: string;
  };
}

/** A frame cannot be composed without its known provenance contract. */
export class ObservationFrameError extends Error {}

function bounds(x: number, y: number, width: number, height: number): Bounds {
  return { x, y, width: Math.max(1, width), height: Math.max(1, height) };
}

/**
 * Compose one observation frame without consulting the DOM or the simulated world.
 * Every semantic string in the result is copied from the validated packet.
 */
export function renderObservationFrame(
  packet: FramePacket,
  viewport: ViewportDimensions,
): RenderState {
  if (packet.schema !== undefined && packet.schema !== FRAME_SCHEMA) {
    throw new ObservationFrameError(
      `schema must be ${FRAME_SCHEMA}, got ${String(packet.schema)}`,
    );
  }
  if (typeof packet.source_digest !== "string" || packet.source_digest.length === 0) {
    throw new ObservationFrameError("source_digest must be present");
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
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, value]) => ({ key, value }));

  return {
    viewport: { width: viewport.width, height: viewport.height },
    layout: phone ? "phone" : "laptop",
    title: packet.title,
    objectLabel: packet.labels.object,
    scaleLabel: packet.labels.scale,
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
