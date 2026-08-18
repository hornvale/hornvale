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

/** A minimal `vessel/session/v2` snapshot with one `scene/surrounds/v2`
 * chart, built from bare cell payloads — for tests that care about a
 * specific field (colour, water) rather than a real fixture. The four-entry
 * `WaterKind::LEGEND` order (`ocean`, `salt-basin`, `river`, `dry-land`) is
 * fixed so a caller can write `water: 0` / `water: 3` and mean it. */
function snapshotWithChart(cells: unknown[]): Snapshot {
  return parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
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
  // Under the lattice projection this guarded the `+ w` term, which
  // cancelled a shear. North-up has no such term — a shear is impossible by
  // construction — but the assertion still earns its place, because it now
  // catches the projection getting the SIGN of a component wrong. Flip
  // `-cos` to `+cos` and the band still draws; mirror its rows and it does
  // not.
  //
  // What it measures is each row's *leading*-space count. A band drawn
  // about its own observer has leading-space counts that mirror around the
  // centre row: row i and row (n-1-i) indent equally.
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
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        radius: 1,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
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
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v3",
        radius: 1,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a chart with the current schema tag renders", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assert(chartCells(snap) !== null, "the current tag should render");
});

Deno.test("a chamber-band snapshot draws no chart", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: { band: "chamber", plan: {} },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a snapshot with no spatial channel draws no chart", () => {
  assertEquals(
    chartCells(parseSnapshot(JSON.stringify({ schema: "vessel/session/v2" }))!),
    null,
  );
});

Deno.test("a seam cell is drawn, at the box its bearing puts it in", () => {
  // A seam cell's lattice offsets are null — the lattice bends across a
  // base face there — and that is exactly why the old projection dropped
  // it, leaving half of a seam-crossing band blank. Its bearing and
  // distance are not null, so it draws like any other cell now.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        biome_legend: ["forest"],
        water_legend: ["none"],
        relief_legend: ["flat"],
        cells: [
          {
            u: null,
            v: null,
            w: null,
            up: null,
            seam: true,
            state: "sensed",
            biome: 0,
            water: 0,
            relief: 0,
            marks: [],
            bearing_deg: 90,
            distance_rad: 1,
          },
          {
            u: 0,
            v: 0,
            w: 0,
            up: true,
            seam: false,
            state: "here",
            biome: 0,
            water: 0,
            relief: 0,
            marks: [],
            bearing_deg: 0,
            distance_rad: 0,
          },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(
    rows.join("").split("").filter((c) => c !== " ").length,
    2,
    "both the observer and the seam cell must be drawn",
  );
  // Due east on the rim: same row as `@`, two columns right of it.
  const row = rows.find((r) => r.includes("@"))!;
  assertEquals(row.indexOf(".") - row.indexOf("@"), 2);
});

Deno.test("a chart with no cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: { band: "walk", chart: {} },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a chart with an empty cells array draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: { band: "walk", chart: { cells: [] } },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a spatial channel with a null chart draws nothing", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: { band: "walk", chart: null },
  }))!;
  assertEquals(chartCells(snap), null);
});

Deno.test("a malformed cell (not an object) is skipped, not thrown on", () => {
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
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

Deno.test("a cell with no bearing or distance is skipped, not placed at the observer", () => {
  // A `scene/surrounds/v2` payload predating the north-up fields carries
  // the same schema tag, so the allowlist alone does not catch it. Placing
  // such a cell would default it to the observer's own box and draw a band
  // claiming everything is right here — the class of silently-wrong map
  // this client exists to refuse.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
          { seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(rows.join("").split("").filter((c) => c !== " ").length, 1);
});

Deno.test("a chart whose radius is absent, fractional or past the ceiling draws nothing", () => {
  // The radius sets the scale AND bounds the grid: the pane builds
  // 2*radius+1 rows by 4*radius+1 columns, so `radius: 1e9` would try for
  // ~8e18 cells and hang the worker — the same main.ts lockup path an
  // uncaught TypeError lands on. It replaced a ceiling on the lattice
  // coordinates, which is the quantity the OLD projection's grid size
  // depended on and no longer the one that binds.
  //
  // The absent and fractional arms are here because a chart that does not
  // say how many rings it spans cannot be placed honestly, and defaulting
  // would be a guess: the pane refuses instead.
  for (const radius of [undefined, 1e9, 65, -1, 2.5, "4"]) {
    const snap = parseSnapshot(JSON.stringify({
      schema: "vessel/session/v2",
      spatial: {
        band: "walk",
        chart: {
          schema: "scene/surrounds/v2",
          radius,
          water_legend: ["none"],
          cells: [
            { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
          ],
        },
      },
    }))!;
    assertEquals(chartCells(snap), null, `radius ${radius} must be refused`);
  }
  // The positive control: the same chart with a real radius DOES draw, so
  // the refusals above are the radius check firing and not the fixture
  // being unrenderable for some other reason.
  const ok = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 4,
        water_legend: ["none"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
        ],
      },
    },
  }))!;
  assertEquals(glyphRows(chartCells(ok)), ["@"]);
});

Deno.test("a non-string water_legend entry does not shift subsequent indices", () => {
  // `cell.water` is a positional index into `water_legend`. Dropping a
  // non-string entry (the previous `.filter`-based implementation) shifts
  // every later index — "river" moves from index 3 to index 2 and
  // `cell.water: 3` resolves past the end of the legend, silently reading
  // as land. This pins the position-preserving fix (`.map` to `""` rather
  // than `.filter`) against that regression.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        // Index 0 is malformed (not a string). If it were dropped instead
        // of preserved as "", "river" would shift from index 3 to index 2.
        water_legend: [null, "ocean", "salt-basin", "river"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 0 },
          { bearing_deg: 90, distance_rad: 1, seam: false, state: "sensed", water: 3 },
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
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        water_legend: ["ocean", "salt-basin", "river", "dry-land"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 },
          { bearing_deg: 90, distance_rad: 1, seam: false, state: "sensed", water: 3 },
          { bearing_deg: 270, distance_rad: 1, seam: false, state: "sensed", water: 0 },
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
    {
      bearing_deg: 0,
      distance_rad: 0,
      seam: false,
      state: "sensed",
      water: 3,
      color: [10, 20, 30],
    },
    {
      bearing_deg: 90,
      distance_rad: 1,
      seam: false,
      state: "sensed",
      water: 0,
      color: [40, 50, 60],
    },
  ]);
  const grid = chartCells(snap)!;
  const flat = grid.flat();
  const land = flat.find((c) => c.glyph === ".")!;
  assertEquals(land.color, [10, 20, 30]);
  const water = flat.find((c) => c.glyph === "~")!;
  assertEquals(
    water.color,
    null,
    "the tint is the SURFACE; a river must not be drawn the colour of the ground beneath it",
  );
});

Deno.test("the observer's own cell withholds colour even when the payload supplies one", () => {
  // Ground rule (see `PaneCell`'s doc in `pane_cell.ts`): `@` names the
  // observer, not the surface beneath them. The payload supplies a real
  // colour here on purpose — an absent colour would let this pass whether
  // or not the withholding actually ran, which is not a discriminating
  // assertion (fix-round 1 verified this by mutation: see the task report).
  const snap = snapshotWithChart([
    { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3, color: [99, 88, 77] },
  ]);
  const cell = chartCells(snap)!.flat()[0];
  assertEquals(cell.glyph, "@");
  assertEquals(cell.color, null);
});

Deno.test("a cell with no colour key is uncoloured, not crashed", () => {
  const snap = snapshotWithChart([
    { bearing_deg: 0, distance_rad: 0, seam: false, state: "sensed", water: 3 },
  ]);
  assertEquals(chartCells(snap)!.flat()[0].color, null);
});

Deno.test("a malformed colour is refused, not passed through", () => {
  for (const bad of [[1, 2], [1, 2, 3, 4], ["1", 2, 3], "red", 7, [1, 2, 300], [1, 2, -1]]) {
    const snap = snapshotWithChart([
      { bearing_deg: 0, distance_rad: 0, seam: false, state: "sensed", water: 3, color: bad },
    ]);
    assertEquals(
      chartCells(snap)!.flat()[0].color,
      null,
      `${JSON.stringify(bad)} must not survive`,
    );
  }
});

/** The sim's own render of the same seed-42 walk band, generated by
 * `scripts/regenerate-artifacts.sh` and drift-checked
 * (`docs/generated-paths.txt`). It is read from
 * `clients/game/core/tests/fixtures/` rather than copied here on purpose:
 * a copy is a second thing to keep in step, and the whole value of this
 * reference is that it can only ever be re-captured FROM THE SIM. */
const SIM_REFERENCE = Deno.readTextFileSync(
  new URL(
    "../../game/core/tests/fixtures/chart-reference-seed-42.txt",
    import.meta.url,
  ),
);

/** Reduce a picture to which boxes are filled, not what fills them. This is
 * what makes comparing two renderers with deliberately different glyph
 * vocabularies legitimate: decision 0022 licenses the vocabulary to differ,
 * never the geometry. */
function shapeOf(lines: string[]): string[] {
  return lines.map((l) => l.replace(/[^ ]/g, "#").replace(/\s+$/, ""));
}

Deno.test("the pane places cells exactly where the sim's own renderer does", () => {
  // The Casement had no cross-renderer control at all before this: its
  // projection was checked only against its own expectations, which is
  // exactly the shape of the defect `clients/game/core/src/chart.rs`'s
  // module doc records — a plausible-looking wrong formula that passed
  // every test in its own file and was caught only by comparing against the
  // sim. `hornvale-game-core` has had that comparison since The Quire; this
  // gives the vessel pane the same one.
  const rows = glyphRows(chartCells(parseSnapshot(WALK)!))!;
  assertEquals(shapeOf(rows), shapeOf(SIM_REFERENCE.split("\n")));
});

Deno.test("the more salient of two colliding cells keeps the box", () => {
  // A FORCED collision: two cells at the identical bearing and distance
  // cannot help landing in one box. Measured across seventy real bands the
  // shipped projection collided zero times, so nothing but a forced fixture
  // exercises this rule at all — and an unreachable rule is exactly the
  // kind this project has repeatedly found pointed at nothing.
  //
  // `salience` is a RANK where lower is more salient, so the salience-5
  // ocean cell must beat the salience-20 dry-land one. Both document orders
  // are asserted, which is what separates "the rule ran" from "whichever
  // cell was written last won".
  const marked = (salience: number, water: number) => ({
    bearing_deg: 90,
    distance_rad: 1,
    seam: false,
    state: "sensed",
    water,
    marks: [{ noun: "x", kind: "settlement", datum: "d", salience }],
  });
  for (
    const cells of [
      [
        { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 },
        marked(20, 3),
        marked(5, 0),
      ],
      [
        { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 },
        marked(5, 0),
        marked(20, 3),
      ],
    ]
  ) {
    const glyphs = glyphRows(chartCells(snapshotWithChart(cells)))!.join("").split("");
    assertEquals(glyphs.filter((c) => c === "~").length, 1, "the salience-5 cell keeps the box");
    assertEquals(glyphs.filter((c) => c === ".").length, 0, "the salience-20 cell lost it");
  }
});

Deno.test("a marked cell outranks an unmarked one in either document order", () => {
  const marked = {
    bearing_deg: 90,
    distance_rad: 1,
    seam: false,
    state: "sensed",
    water: 0,
    marks: [{ noun: "Ka", kind: "settlement", datum: "d", salience: 40 }],
  };
  const bare = { bearing_deg: 90, distance_rad: 1, seam: false, state: "sensed", water: 3 };
  const here = { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 };
  for (const cells of [[here, marked, bare], [here, bare, marked]]) {
    const glyphs = glyphRows(chartCells(snapshotWithChart(cells)))!.join("").split("");
    assertEquals(glyphs.filter((c) => c === "~").length, 1, "the marked cell keeps the box");
  }
});

Deno.test("the observer never loses their own box", () => {
  // Clause 1 of the collision rule: the chart is egocentric, so `@` wins
  // its box against the most salient mark in the band. A chart that drew
  // over the observer would have lost the one cell the reader is standing
  // in.
  const rows = glyphRows(chartCells(snapshotWithChart([
    { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 },
    {
      bearing_deg: 0,
      distance_rad: 0,
      seam: false,
      state: "sensed",
      water: 0,
      marks: [{ noun: "Ka", kind: "settlement", datum: "d", salience: 0 }],
    },
  ])))!;
  assertEquals(rows, ["@"]);
});

Deno.test("rounding is half away from zero, matching the sim and not Math.round", () => {
  // `Math.round(-0.5)` is `-0`; Rust's `f64::round` — which the sim and
  // `clients/game/core` both use — gives `-1`. A bare `Math.round` here
  // would put a cell one row or column out from where the other two
  // renderers put it, on exactly the half-integers a symmetric chart is
  // full of.
  //
  // The fixture lands a row on EXACTLY -0.5, using only values JavaScript
  // represents exactly: `Math.cos(0)` is exactly `1`, `rings` is 1, and the
  // north cell sits at half the band's farthest distance, so its row is
  // `-1 * 0.5`. Under the correct rule it is one row above the observer;
  // under `Math.round` it lands in the observer's own box, loses it to
  // clause 1, and disappears — a one-row chart instead of a two-row one.
  const snap = parseSnapshot(JSON.stringify({
    schema: "vessel/session/v2",
    spatial: {
      band: "walk",
      chart: {
        schema: "scene/surrounds/v2",
        radius: 1,
        water_legend: ["ocean", "salt-basin", "river", "dry-land"],
        cells: [
          { bearing_deg: 0, distance_rad: 0, seam: false, state: "here", water: 3 },
          { bearing_deg: 90, distance_rad: 1, seam: false, state: "sensed", water: 3 },
          { bearing_deg: 0, distance_rad: 0.5, seam: false, state: "sensed", water: 0 },
        ],
      },
    },
  }))!;
  const rows = glyphRows(chartCells(snap))!;
  assertEquals(
    rows.length,
    2,
    `a row at exactly -0.5 must round AWAY from zero, to -1: ${JSON.stringify(rows)}`,
  );
  assertEquals(rows[0][0], "~", "the north cell draws one row above the observer");
  assertEquals(rows[1][0], "@");
});
