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

/** One turn, as the sim emitted it. Only the fields the client reads. */
export interface Snapshot {
  schema: string;
  turn: number;
  day: number;
  self: {
    agent: number;
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
  return snap.sensed.room.exits
    .filter((e) => e.kind === "Edge" && typeof e.direction === "object")
    .map((e) => ({
      dir: (e.direction as { Compass: string }).Compass,
      room: e.to,
    }));
}
