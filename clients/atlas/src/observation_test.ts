import { assert, assertEquals, assertThrows } from "@std/assert";
import { type FramePacket, ObservationFrameError, renderObservationFrame } from "./observation.ts";

const PHONE = { width: 390, height: 844 };
const LAPTOP = { width: 1440, height: 900 };

async function fixture(): Promise<FramePacket> {
  const text = await Deno.readTextFile(
    new URL("../../../observations/fixtures/HV-001/render-input.json", import.meta.url),
  );
  return JSON.parse(text) as FramePacket;
}

Deno.test("frame composition preserves the authored title and object/scale labels", async () => {
  // Catches a renderer substituting generic client copy for packet-authored labels.
  const state = renderObservationFrame(await fixture(), LAPTOP);

  assertEquals(state.title, "How the underworld gathers into chambers");
  assertEquals(state.objectLabel, "geography");
  assertEquals(state.scaleLabel, "world");
});

Deno.test("legend values come only from supplied labels", async () => {
  // Catches client-side biome/resource classification leaking into the public frame.
  const packet = await fixture();
  packet.labels.material = "authored stone label";
  const state = renderObservationFrame(packet, LAPTOP);
  const rendered = JSON.stringify(state);

  assert(rendered.includes("authored stone label"));
  assert(!rendered.includes("temperate-rainforest"));
  assert(!rendered.includes("iron deposit"));
});

Deno.test("the same packet and viewport produce identical render state", async () => {
  // Catches hidden clock, random, DOM, or world-state reads.
  const packet = await fixture();
  assertEquals(
    renderObservationFrame(packet, LAPTOP),
    renderObservationFrame(packet, LAPTOP),
  );
});

Deno.test("phone composition retains the primary map and observation sentence", async () => {
  // Catches responsive layout changes that discard the evidence or its bounded claim.
  const packet = await fixture();
  const state = renderObservationFrame(packet, PHONE);

  assert(state.map.bounds.width > 0 && state.map.bounds.height > 0);
  assertEquals(state.map.content, packet.spatial.readout);
  assertEquals(
    state.annotation.text,
    "Chambers gather into connected cave systems across depth bands.",
  );
});

Deno.test("unknown frame schemas and missing source digests are refused", async () => {
  // Catches rendering packets whose provenance contract is unknown or absent.
  const packet = await fixture();
  assertThrows(
    () => renderObservationFrame({ ...packet, schema: "observation/frame/v2" }, LAPTOP),
    ObservationFrameError,
    "schema",
  );
  assertThrows(
    () => renderObservationFrame({ ...packet, source_digest: "" }, LAPTOP),
    ObservationFrameError,
    "source_digest",
  );
});

Deno.test("comparison metadata never enters public render state", async () => {
  // Catches accidental pass-through of internal comparison framing.
  const packet = {
    ...await fixture(),
    comparison_reference: { title: "PRIVATE COMPARISON" },
  } as FramePacket & { comparison_reference: { title: string } };
  const rendered = JSON.stringify(renderObservationFrame(packet, LAPTOP));

  assert(!rendered.includes("PRIVATE COMPARISON"));
  assert(!rendered.includes("comparison_reference"));
});
