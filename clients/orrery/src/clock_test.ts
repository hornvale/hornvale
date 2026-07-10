import { assertAlmostEquals } from "@std/assert";
import { clockToDay } from "./clock.ts";

Deno.test("clock maps seconds to days by speed", () => {
  assertAlmostEquals(clockToDay(1000, 30), 30, 1e-9);
  assertAlmostEquals(clockToDay(500, 30), 15, 1e-9);
});
