import { assert, assertEquals } from "@std/assert";
import { parseSnapshot } from "./snapshot.ts";
import { chartRows } from "./pane_chart.ts";

const WALK = Deno.readTextFileSync(
  new URL(
    "../../../windows/vessel/tests/fixtures/snapshot-seed-42-walk.json",
    import.meta.url,
  ),
);

Deno.test("a real walk snapshot renders a non-empty chart", () => {
  const rows = chartRows(parseSnapshot(WALK)!)!;
  assert(rows.length > 0, "the chart drew nothing");
  assert(rows.every((r) => r.length === rows[0].length), "rows are ragged");
});

Deno.test("the observer is marked, and exactly once", () => {
  const rows = chartRows(parseSnapshot(WALK)!)!;
  assertEquals(rows.join("").split("").filter((c) => c === "@").length, 1);
});

Deno.test("the ball is symmetric about the observer, not sheared", () => {
  // The `+ w` term in the column formula is what makes this true. Drop it
  // and the ball leans right — a plausible-looking, wrong map. This is the
  // negative control on the one piece of real geometry in this module.
  const rows = chartRows(parseSnapshot(WALK)!)!;
  const widths = rows.map((r) => r.trimEnd().length - (r.length - r.trimStart().length));
  const first = widths.indexOf(Math.max(...widths));
  const last = widths.lastIndexOf(Math.max(...widths));
  assertEquals(
    first + last,
    rows.length - 1,
    "the widest row is not centred: the ball is sheared",
  );
});

Deno.test("a chamber-band snapshot draws no chart", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "chamber", plan: {} },
  }))!;
  assertEquals(chartRows(snap), null);
});

Deno.test("a snapshot with no spatial channel draws no chart", () => {
  assertEquals(
    chartRows(parseSnapshot(JSON.stringify({ schema: "vessel/session/v1" }))!),
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
        schema: "scene/surrounds/v1",
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
  const rows = chartRows(snap)!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a chart with no cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: {} },
  }))!;
  assertEquals(chartRows(snap), null);
});

Deno.test("a chart with an empty cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: { cells: [] } },
  }))!;
  assertEquals(chartRows(snap), null);
});

Deno.test("a spatial channel with a null chart draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: { band: "walk", chart: null },
  }))!;
  assertEquals(chartRows(snap), null);
});

Deno.test("a malformed cell (not an object) is skipped, not thrown on", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
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
  const rows = chartRows(snap)!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a cell missing v/w/up entirely (not even null) is skipped", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v1",
    spatial: {
      band: "walk",
      chart: {
        water_legend: ["none"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 0 },
          { seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = chartRows(snap)!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
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
        water_legend: ["ocean", "salt-basin", "river", "dry-land"],
        cells: [
          { v: 0, w: 0, up: true, seam: false, state: "here", water: 3 },
          { v: 1, w: 0, up: false, seam: false, state: "sensed", water: 3 },
          { v: -1, w: 0, up: false, seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = chartRows(snap)!;
  const glyphs = rows.join("").split("");
  assertEquals(glyphs.filter((c) => c === "~").length, 1, "only the ocean cell should be water");
  assertEquals(glyphs.filter((c) => c === ".").length, 1, "the dry-land cell should be land");
});
