import { assertEquals } from "@std/assert";
import { format, parse } from "./permalink.ts";

Deno.test("the bare form parses and formats back to itself", () => {
  const link = parse("#42/17");
  assertEquals(link, { seed: "42", index: "17", year: null, site: null });
  assertEquals(format(link!.seed, link!.index, link!), "#42/17");
});

Deno.test("the pinned form round-trips both pins", () => {
  const link = parse("#42/17?year=1500&site=10630");
  assertEquals(link, { seed: "42", index: "17", year: 1500, site: 10630 });
  assertEquals(format(link!.seed, link!.index, link!), "#42/17?year=1500&site=10630");
});

Deno.test("a year-only pin round-trips without a stray site parameter", () => {
  const link = parse("#42/17?year=1500");
  assertEquals(link, { seed: "42", index: "17", year: 1500, site: null });
  assertEquals(format(link!.seed, link!.index, link!), "#42/17?year=1500");
});

Deno.test("a fractional pinned year survives the trip", () => {
  const link = parse("#42/17?year=1751.6373");
  assertEquals(link?.year, 1751.6373);
  assertEquals(format("42", "17", link!), "#42/17?year=1751.6373");
});

Deno.test("a full-width seed and index survive as text, not as doubles", () => {
  // 18446744073709551615 is u64::MAX; Number() would round it to
  // 18446744073709551616 and name a world that does not exist.
  const link = parse("#18446744073709551615/9007199254740993");
  assertEquals(link?.seed, "18446744073709551615");
  assertEquals(link?.index, "9007199254740993");
  assertEquals(
    format(link!.seed, link!.index, link!),
    "#18446744073709551615/9007199254740993",
  );
});

Deno.test("a hash with no leading # parses the same way", () => {
  assertEquals(parse("42/17"), { seed: "42", index: "17", year: null, site: null });
});

Deno.test("malformed links are refused whole rather than half-parsed", () => {
  for (
    const bad of [
      "",
      "#",
      "#42",
      "#42/17/3",
      "#-1/17",
      "#42/0x11",
      "#42/1.5",
      "#18446744073709551616/0", // one past u64::MAX
      "#42/17?year=soon",
      "#42/17?site=nowhere",
      "#42/17?year=1500&site=4294967296", // one past u32::MAX
      "#42/17?site=10630", // a site with no year: the ABI cannot express it
    ]
  ) {
    assertEquals(parse(bad), null, `expected ${bad} to be refused`);
  }
});

Deno.test("format never writes an empty query string", () => {
  assertEquals(format("42", "0", { year: null, site: null }), "#42/0");
});
