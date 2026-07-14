import { assertEquals } from "@std/assert";
import { parseSeed, seedFromSearch } from "./protocol.ts";

Deno.test("parseSeed accepts plain decimal u64", () => {
  assertEquals(parseSeed("42"), 42n);
  assertEquals(parseSeed("  7 "), 7n);
  assertEquals(parseSeed("0"), 0n);
  assertEquals(parseSeed("18446744073709551615"), 18446744073709551615n);
});

Deno.test("parseSeed rejects everything that is not a u64", () => {
  assertEquals(parseSeed(""), null);
  assertEquals(parseSeed("-1"), null);
  assertEquals(parseSeed("4.2"), null);
  assertEquals(parseSeed("0x2a"), null);
  assertEquals(parseSeed("forty-two"), null);
  assertEquals(parseSeed("18446744073709551616"), null); // u64::MAX + 1
});

Deno.test("seedFromSearch reads ?seed=N and falls back to 42", () => {
  assertEquals(seedFromSearch("?seed=7"), 7n);
  assertEquals(seedFromSearch("?seed=7&x=1"), 7n);
  assertEquals(seedFromSearch(""), 42n);
  assertEquals(seedFromSearch("?seed=banana"), 42n);
  assertEquals(seedFromSearch("?seed=-3"), 42n);
});
