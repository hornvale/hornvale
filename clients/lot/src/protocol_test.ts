import { assertEquals } from "@std/assert";
import { NO_SITE, siteArgument, type WorkerRequest } from "./protocol.ts";

Deno.test("no site pin travels as u32::MAX, never as zero", () => {
  // Vertex 0 is a legal site, so a zero sentinel would pin the reader to a
  // real place they never chose.
  assertEquals(NO_SITE, 4294967295);
  assertEquals(siteArgument(null), NO_SITE);
  assertEquals(siteArgument(0), 0);
  assertEquals(siteArgument(10630), 10630);
});

Deno.test("a request survives structured cloning without losing a digit", () => {
  // The seed and index travel as decimal TEXT; postMessage clones the
  // object, and a `number` would round a full-width seed on the way.
  const request: WorkerRequest = {
    kind: "lot",
    index: "9007199254740993",
    year: 1751.6373,
    site: 10630,
  };
  assertEquals(structuredClone(request), request);
});
