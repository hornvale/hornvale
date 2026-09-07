import { assertEquals } from "@std/assert";
import { causeRows } from "./odds.ts";
import type { Odds } from "./payload.ts";

Deno.test("the Life stage renders the sim's ordered cause table", () => {
  const odds = {
    schema: "lot/odds/v1",
    e0: 30,
    q_maturity: 0.4,
    maturity_years: 15,
    lifespan_years: 60,
    strife: 0.2,
    infant_share: 0.4,
    background_share: 0.4,
    senescent_share: 0.2,
    causes: [
      { cause: "the flux", share: 0.352 },
      { cause: "age", share: 0.648 },
    ],
  } satisfies Odds;
  assertEquals(causeRows(odds), [
    ["the flux", "35.2%"],
    ["age", "64.8%"],
  ]);
});
