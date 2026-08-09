// The `vessel/session/v1` reader. Pure module: no DOM, no worker globals —
// everything here is unit-tested, matching protocol.ts's discipline.
//
// Every pane is a pure function of one snapshot (The Snapshot spec §3), and
// the snapshot is grouped by epistemic channel, so a pane reads one channel
// and cannot see outside it.

/** The schema tag this client understands. A different tag is refused. */
export const SESSION_SCHEMA = "vessel/session/v1";

/** One exit as `locale/room/v2` carries it. */
export interface Exit {
  kind: string;
  direction: { Compass: string } | string;
  to: number;
}

/** One distinct cell type in a `vessel/plan/v1` payload. */
export interface PaletteEntry {
  kind: string;
  chambers: number[];
  // `unknown`, not `[number, number, number] | null`: `pane_cell.ts`'s
  // `parseColor` narrows it, the same refuse-don't-guess treatment every
  // other field on the wire gets. `windows/vessel/src/plan.rs::PaletteEntry`
  // skips this key entirely when absent (`Option::None`, no building-fabric
  // or interior-illuminant model exists yet to fill it), so it may be
  // missing outright rather than merely `null`.
  color?: unknown;
}

/** One individual standing on a cell of the plan — `windows/vessel/src/plan.rs`'s
 * `PlanMark`, the `scene/surrounds/v2` `Mark` shape plus a lattice-local cell. */
export interface PlanMark {
  x: number;
  y: number;
  noun: string;
  kind: string;
  datum: string;
  salience: number;
}

/** A `vessel/plan/v1` document — the chamber band's cells. */
export interface PlanPayload {
  schema: string;
  chamber: number;
  at: number;
  of: number;
  extent: { x: number; y: number; w: number; h: number };
  palette: PaletteEntry[];
  cells: number[];
  you: { x: number; y: number };
  // Optional for the same reason `spatial` on `Snapshot` is: a client
  // bundle can outlive the sim that produced a payload, and an older sim
  // emits no `marks` key at all — Task 5's own fixture predates it too.
  marks?: PlanMark[];
}

/** The spatial channel, tagged by band. A client switches on `band` before
 * reading anything else. */
export type Spatial =
  | { band: "walk"; chart: unknown }
  | { band: "chamber"; plan: PlanPayload };

/** One turn, as the sim emitted it. Only the fields the client reads. */
export interface Snapshot {
  schema: string;
  turn: number;
  day: number;
  self: {
    // A decimal string, not a number: the sim emits a uniform 64-bit draw
    // that JS's IEEE-754 `number` cannot hold losslessly above 2^53.
    agent: string;
    species: string;
    settlement: string;
    population: number;
    room: number;
  };
  sensed: {
    room: { schema: string; id: number; exits: Exit[] };
    sky: string;
    present: { entity: number; label: string; felt: string }[];
  };
  known: { entries: { key: string; value: string }[] };
  social: { entity: number; label: string; grievance: number; hostile: boolean }[];
  narration: { prose: string; nouns: { noun: string; datum: string }[] };
  // Optional on purpose: a sim older than The Panes emits no spatial
  // channel, and the transcript must still work against one. The pane
  // renders nothing rather than the client throwing.
  spatial?: Spatial;
}

/** What eye a walk-band chart was coloured for, and what its projection
 * preserves — the fields this client reads off `hornvale_scene::surrounds::
 * Sight` (`windows/scene/src/surrounds.rs`). `sun_altitude_deg` is on the
 * wire too but no pane reads it, so it is not carried here — the same
 * "as much of it as this pane reads" discipline `pane_chart.ts`'s
 * `ChartCell` already applies to its own payload. */
export interface Sight {
  /** The species (or other named eye) this chart was coloured for. */
  observer: string;
  /** How many channels that eye senses with. */
  channels: number;
  /** How many of those channels are chromatic. */
  chromatic: number;
  /** The eye's projection name, or `"none"` for no projection to sRGB. */
  projection: string;
  /** What that projection preserves, in prose. */
  preserves: string;
}

/** The `sight` declaration off the walk-band chart, or `null` when there is
 * no walk-band spatial channel, no chart, or the `sight` key is absent or
 * malformed. `sight` is itself optional on the wire — `eyes.rs`'s "decline
 * the observer step" path omits the key entirely rather than emitting a
 * null — so a chart with no declared eye draws no caption, same as a sim
 * predating The Beholding. Refuse, don't guess, field by field: the same
 * discipline `pane_plan.ts` and `pane_chart.ts` already apply to their own
 * payloads, applied here rather than trusting a cast — a partially-typed
 * caption is worse than no caption at all. */
export function sightOf(snap: Snapshot): Sight | null {
  const spatial = snap.spatial;
  if (!spatial || spatial.band !== "walk") return null;
  const chart = spatial.chart as { sight?: unknown } | null | undefined;
  if (chart === null || typeof chart !== "object") return null;
  const raw = chart.sight;
  if (raw === null || typeof raw !== "object" || raw === undefined) return null;
  const s = raw as Record<string, unknown>;
  if (
    typeof s.observer !== "string" ||
    !Number.isInteger(s.channels) ||
    !Number.isInteger(s.chromatic) ||
    typeof s.projection !== "string" ||
    typeof s.preserves !== "string"
  ) {
    return null;
  }
  return {
    observer: s.observer,
    channels: s.channels as number,
    chromatic: s.chromatic as number,
    projection: s.projection,
    preserves: s.preserves,
  };
}

/** Parse a snapshot payload, or null if it is absent, junk, or a schema
 * this client does not understand. Never throws: a client that cannot read
 * the snapshot degrades to the prose transcript, which always works. */
export function parseSnapshot(json: string): Snapshot | null {
  if (json.length === 0) return null;
  let parsed: unknown;
  try {
    parsed = JSON.parse(json);
  } catch {
    return null;
  }
  const snap = parsed as Snapshot;
  return snap?.schema === SESSION_SCHEMA ? snap : null;
}

/** The prose this turn prints. Carried verbatim from the sim — the client
 * never re-derives narration from structure (decision 0022). */
export function narrationOf(snap: Snapshot): string {
  return snap.narration.prose;
}

/** The lateral ways on, filtered from the embedded room's own exits. The
 * snapshot carries no `ways` field on purpose: `locale/room/v2` already
 * owns exits, and two representations of one truth would drift. */
export function waysOf(snap: Snapshot): { dir: string; room: number }[] {
  // `?? []` tolerates a restructured room (a future `locale/room/v2` epoch
  // that renames or drops `exits`) rather than throwing on
  // `undefined.filter` — the client degrades to no ways on instead of
  // crashing the pane.
  return (snap.sensed.room?.exits ?? [])
    .filter((e) => e.kind === "Edge" && typeof e.direction === "object")
    .map((e) => ({
      dir: (e.direction as { Compass: string }).Compass,
      room: e.to,
    }));
}
