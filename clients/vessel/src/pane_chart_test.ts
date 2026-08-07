import { assert, assertEquals } from "@std/assert";
import { parseSnapshot } from "./snapshot.ts";
import type { Snapshot } from "./snapshot.ts";
import { chartCells } from "./pane_chart.ts";
import type { PaneGrid } from "./pane_cell.ts";

const WALK = Deno.readTextFileSync(
  new URL(
    "../../../windows/vessel/tests/fixtures/snapshot-seed-42-walk.json",
    import.meta.url,
  ),
);

/** Flatten a grid to plain glyph rows. Every test below this predates
 * colour and only ever asserted on shape and glyph — this lets them keep
 * doing exactly that against the new `PaneGrid` return, without each one
 * re-deriving `.map((c) => c.glyph).join("")` inline. */
function glyphRows(grid: PaneGrid | null): string[] | null {
  return grid ? grid.map((row) => row.map((c) => c.glyph).join("")) : null;
}

/** A minimal `vessel/session/v1` snapshot with one `scene/surrounds/v2`
 * chart, built from bare cell payloads — for tests that care about a
 * specific field (colour, water) rather than a real fixture. The four-entry
 * `WaterKind::LEGEND` order (`ocean`, `salt-basin`, `river`, `dry-land`) is
 * fixed so a caller can write `water: 0` / `water: 3` and mean it. */
function snapshotWithChart(cells: unknown[]): Snapshot {
  return parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["ocean", "salt-basin", "river", "dry-land"],
        cells,
      },
    },
  }))!;
}

Deno.test("a real walk snapshot renders a non-empty chart", () => {
  const rows = glyphRows(chartCells(parseSnapshot(WALK)!))!;
  assert(rows.length > 0, "the chart drew nothing");
  assert(rows.every((r) => r.length === rows[0].length), "rows are ragged");
});

Deno.test("the observer is marked, and exactly once", () => {
  const rows = glyphRows(chartCells(parseSnapshot(WALK)!))!;
  assertEquals(rows.join("").split("").filter((c) => c === "@").length, 1);
});

Deno.test("the ball is symmetric about the observer, not sheared", () => {
  // The `+ w` term in the column formula is what makes this true. Drop it
  // and the ball leans right — a plausible-looking, wrong map.
  //
  // A prior version of this test compared each row's total *span* (last
  // non-space minus first non-space) and asked whether the widest row sat
  // at the vertical centre. Both checks are blind to a pure horizontal
  // shear: dropping `+ cell.w` shifts every row's *start* column but not
  // its *width*, so the span sequence and the widest-row position are
  // identical whether the term is present or absent. Confirmed by mutation
  // (see the fix-round report) — the old assertions stayed green with the
  // term removed while the render visibly leaned right.
  //
  // What a shear actually moves is each row's *leading*-space count, and a
  // true (unsheared) hexagon's leading-space counts mirror around the
  // centre row: row i and row (n-1-i) indent equally. A shear breaks that
  // mirror because indentation grows (or shrinks) monotonically down the
  // rows instead of tapering symmetrically toward the ends.
  const rows = glyphRows(chartCells(parseSnapshot(WALK)!))!;
  const lead = rows.map((r) => r.length - r.trimStart().length);
  const n = lead.length;
  for (let i = 0; i < n; i++) {
    assertEquals(
      lead[i],
      lead[n - 1 - i],
      `row ${i}'s indent (${lead[i]}) does not mirror row ${n - 1 - i}'s (${
        lead[n - 1 - i]
      }): the ball is sheared`,
    );
  }
});

Deno.test("a chart with no schema tag draws nothing", () => {
  // `chartCells` must validate `chart.schema`, not merely read whichever
  // fields it happens to want — the same discipline `parseSnapshot` applies
  // to the envelope. Before this guard existed, an absent tag degraded to
  // "read what's there," which is luck, not design.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a chart with an unrecognised schema tag draws nothing", () => {
  // An allowlist, not a denylist: a tag this client has never heard of must
  // refuse, not render. A denylist would render it and could silently
  // mis-draw a field a future schema reused with new meaning.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v3",
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a chart with the current schema tag renders", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assert(chartCells(snap) !== null, "the current tag should render");
});

Deno.test("a chamber-band snapshot draws no chart", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "chamber", plan: {} },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a snapshot with no spatial channel draws no chart", () => {
  assertEquals(
    chartCells(parseSnapshot(JSON.stringify({ schema: "vessel/session/v1" }))!),
    null,
  );
});

Deno.test("seam cells are skipped, not drawn at a wrong place", () => {
  // A seam cell has null u/v/w: no honest local coordinate exists, so there
  // is nowhere correct to draw it. Dropping it is the honest choice.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        biome_legend: ["forest"],
        water_legend: ["none"],
        relief_legend: ["flat"],
        cells: [
          {
            v: 0,
            w: 0,
            up: true,
            seam: false,
            state: "here",
            biome: 0,
            water: 0,
            relief: 0,
            marks: [],
          },
          {
            v: null,
            w: null,
            up: null,
            seam: true,
            state: "sensed",
            biome: 0,
            water: 0,
            relief: 0,
            marks: [],
          },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a chart with no cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: {} },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a chart with an empty cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: { cells: [] } },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a spatial channel with a null chart draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: null },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a malformed cell (not an object) is skipped, not thrown on", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
          "not a cell",
          42,
          null,
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a cell missing v/w/up entirely (not even null) is skipped", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
          { seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a cell past the coordinate ceiling is refused, not drawn", () => {
  // With no bound, a cell at v: 20000 builds a 40,002-character row, and at
  // v: 1e9 the row exceeds V8's max string length and throws a RangeError —
  // landing on the same main.ts lockup path an uncaught TypeError would.
  // This pins MAX_COORD against that class of payload: only the in-bound
  // cell should ever be placed.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
          { v: 20000, w: 0, up: true, seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assert(rows.every((r) => r.length < 100), "a past-ceiling cell widened the chart");
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a non-string water_legend entry does not shift subsequent indices", () => {
  // `cell.water` is a positional index into `water_legend`. Dropping a
  // non-string entry (the previous `.filter`-based implementation) shifts
  // every later index — "river" moves from index 3 to index 2 and
  // `cell.water: 3` resolves past the end of the legend, silently reading
  // as land. This pins the position-preserving fix (`.map` to `""` rather
  // than `.filter`) against that regression.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        // Index 0 is malformed (not a string). If it were dropped instead
        // of preserved as "", "river" would shift from index 3 to index 2.
        water_legend: [null, "ocean", "salt-basin", "river"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
          { v: 1, w: 0, up: false, seam: false, state: "sensed", water: 3 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(
    rows.join("").split("").filter((c) => c === "~").length,
    1,
    "water at the correct (unshifted) index should still render as water",
  );
});

Deno.test("a real dry-land cell does not render as water", () => {
  // The water legend's non-water label is "dry-land", never "none" — a
  // `water !== "none"` check would misread every dry cell as water. This
  // pins the positive-match fix against that regression.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        water_legend: ["ocean", "salt-basin", "river", "dry-land"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 3 },
          { v: 1, w: 0, up: false, seam: false, state: "sensed", water: 3 },
          { v: -1, w: 0, up: false, seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  const glyphs = rows.join("").split("");
  assertEquals(glyphs.filter((c) => c === "~").length, 1, "only the ocean cell should be water");
  assertEquals(glyphs.filter((c) => c === ".").length, 1, "the dry-land cell should be land");
});

Deno.test("a chart cell carries the sim's colour, and only where it is ground", () => {
  const snap = snapshotWithChart([
    { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3, color: [10, 20, 30] },
    { v: 1, w: 0, up: false, seam: false, state: "sensed", water: 0, color: [40, 50, 60] },
  ]);
  const grid = chartCells(snap)!;
  const flat = grid.flat();
  const land = flat.find((c) => c.glyph === ".")!;
  assertEquals(land.color, [10, 20, 30]);
  const water = flat.find((c) => c.glyph === "~")!;
  assertEquals(
    water.color,
    null,
    "the tint is BEDROCK; a river must not be drawn the colour of the rock beneath it",
  );
});

Deno.test("a cell with no colour key is uncoloured, not crashed", () => {
  const snap = snapshotWithChart([
    { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3 },
  ]);
  assertEquals(chartCells(snap)!.flat()[0].color, null);
});

Deno.test("a malformed colour is refused, not passed through", () => {
  for (const bad of [[1, 2], [1, 2, 3, 4], ["1", 2, 3], "red", 7, [1, 2, 300], [1, 2, -1]]) {
    const snap = snapshotWithChart([
      { v: 0, w: 0, up: true, seam: false, state: "sensed", water: 3, color: bad },
    ]);
    assertEquals(
      chartCells(snap)!.flat()[0].color,
      null,
      `${JSON.stringify(bad)} must not survive`,
    );
  }
});
