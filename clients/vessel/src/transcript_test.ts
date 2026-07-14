import { assertEquals } from "@std/assert";
import { splitResponse } from "./transcript.ts";

Deno.test("room header and ways lines are meta; prose is prose", () => {
  const text = "[room 1, day 0]\nYou stand in a test.\nWays on: N, SW.";
  assertEquals(splitResponse(text), [
    { cls: "casement-meta", text: "[room 1, day 0]" },
    { cls: "casement-prose", text: "You stand in a test." },
    { cls: "casement-meta", text: "Ways on: N, SW." },
  ]);
});

Deno.test("plain single-line responses are prose", () => {
  assertEquals(splitResponse("You let go."), [
    { cls: "casement-prose", text: "You let go." },
  ]);
});

Deno.test("empty lines are preserved as prose spacers", () => {
  assertEquals(splitResponse("a\n\nb"), [
    { cls: "casement-prose", text: "a" },
    { cls: "casement-prose", text: "" },
    { cls: "casement-prose", text: "b" },
  ]);
});
