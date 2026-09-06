import { assert, assertEquals, assertThrows } from "@std/assert";
import { parseCurve, parseLife, parsePlaces } from "./payload.ts";
import { growingCurve, life, places } from "./fixtures.ts";

Deno.test("a life payload parses every field the stages read", () => {
  const parsed = parseLife(JSON.stringify(life()));
  assertEquals(parsed.schema, "lot/life/v1");
  assertEquals(parsed.seed, 42);
  assertEquals(parsed.slots.length, 8);
  assertEquals(parsed.silences.by_design, 4);
  assertEquals(parsed.sources[1].function, "lot::draw::draw");
});

Deno.test("the entity ids stay decimal text, never doubles", () => {
  // 10760661430244475331 does not fit in 53 bits; JSON.parse would round it
  // to 10760661430244475000. `json.rs` emits it as a string for exactly
  // this reason, and the parser must keep it one.
  const parsed = parseLife(JSON.stringify(life()));
  assertEquals(parsed.occupation, "10760661430244475331");
  assertEquals(typeof parsed.occupation, "string");
  assertEquals(parsed.sources[0].entity, "10760661430244475331");
});

Deno.test("a curve payload parses and keeps its per-people series", () => {
  const parsed = parseCurve(JSON.stringify(growingCurve()));
  assertEquals(parsed.births_by_epoch.length, 80);
  assertEquals(Object.keys(parsed.births_by_people), ["kobold"]);
  assertEquals(parsed.present_year, 2000);
});

Deno.test("a places payload parses, unnamed places included", () => {
  const doc = { schema: "lot/places/v1", year: 1500, places: places() };
  const parsed = parsePlaces(JSON.stringify(doc));
  assertEquals(parsed.year, 1500);
  assertEquals(parsed.places.length, 3);
  assertEquals(parsed.places[2].name, null);
});

Deno.test("the wrong schema is refused rather than half-read", () => {
  const wrong = { ...life(), schema: "lot/odds/v1" };
  assertThrows(() => parseLife(JSON.stringify(wrong)), Error, "expected schema lot/life/v1");
});

Deno.test("an error envelope from the sim is not mistaken for a payload", () => {
  // A refused pin puts the sim's own sentence in the out buffer; a
  // page that parsed it as a life would render a blank one.
  assertThrows(() => parseLife("the pinned year is outside this world's span"));
  assertThrows(() => parseCurve("{}"), Error, "expected schema lot/curve/v1");
});

Deno.test("a degenerate curve is refused before it can divide by zero", () => {
  const empty = { ...growingCurve(), births_by_epoch: [] };
  assertThrows(() => parseCurve(JSON.stringify(empty)), Error, "births_by_epoch is empty");
  const backwards = { ...growingCurve(), present_year: 0, start_year: 2000 };
  assertThrows(() => parseCurve(JSON.stringify(backwards)), Error, "present_year");
  const noEpoch = { ...growingCurve(), epoch_years: 0 };
  assertThrows(() => parseCurve(JSON.stringify(noEpoch)), Error, "epoch_years");
});

Deno.test("the real seed-42 payloads parse, written by make lot-check's smoke", async () => {
  // A belt-and-braces witness over the FIXTURES' fidelity: the hand-reduced
  // payloads above are shaped after real ones, and this reads the real ones
  // that `clients/lot/wasm/drive.mjs` (run by `make lot-check`, BEFORE this
  // suite) writes to these paths.
  //
  // A MISSING fixture is a FAILURE, not a skip. It used to `continue` past a
  // read error so `deno task test` could "stand alone" without a prior smoke
  // run — but a witness that cannot fail is not a witness: the smoke's own
  // producer went dead for an entire campaign (Task 10b's original
  // `lot-check-run` never wrote these three files at all) and this test
  // stayed green throughout, silently checking nothing. `make lot-check`
  // still runs the smoke first specifically so this test sees real fixtures
  // in its ordinary gate; run `deno task test` on its own and this is the
  // one test that is expected to fail, naming the fix.
  for (
    const [path, parse] of [
      ["/tmp/hv-lot-life-0.json", parseLife],
      ["/tmp/hv-lot-curve.json", parseCurve],
      ["/tmp/hv-lot-places.json", parsePlaces],
    ] as const
  ) {
    let text: string;
    try {
      text = await Deno.readTextFile(path);
    } catch (err) {
      throw new Error(
        `${path} is missing — run 'make lot-check' (its wasm smoke writes this fixture before this suite runs): ${err}`,
      );
    }
    const parsed = parse(text) as { schema: string };
    assert(parsed.schema.startsWith("lot/"), `${path} parsed to ${parsed.schema}`);
  }
});
