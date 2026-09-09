import { assert, assertEquals, assertThrows } from "@std/assert";
import {
  type FramePacket,
  ObservationFrameError,
  parseObservationFramePacket,
  renderObservationFrame,
  renderObservationFrameHtml,
} from "./observation.ts";

const PHONE = { width: 390, height: 844 };
const LAPTOP = { width: 1440, height: 900 };

async function fixture(): Promise<FramePacket> {
  const text = await Deno.readTextFile(
    new URL("../../../observations/fixtures/HV-001/render-input.json", import.meta.url),
  );
  return parseObservationFramePacket(text);
}

async function fixtureText(): Promise<string> {
  return await Deno.readTextFile(
    new URL("../../../observations/fixtures/HV-001/render-input.json", import.meta.url),
  );
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
  packet.labels.zebra = "last by code point";
  packet.labels.äther = "after ASCII by code point";
  const state = renderObservationFrame(packet, LAPTOP);
  const rendered = JSON.stringify(state);

  assert(rendered.includes("authored stone label"));
  assert(!rendered.includes("temperate-rainforest"));
  assert(!rendered.includes("iron deposit"));
  assertEquals(
    state.legend.map(({ key }) => key),
    ["material", "object", "primary_axis", "scale", "zebra", "äther"],
  );
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
    () =>
      parseObservationFramePacket(JSON.stringify({ ...packet, schema: "observation/frame/v2" })),
    ObservationFrameError,
    "schema",
  );
  assertThrows(
    () => parseObservationFramePacket(JSON.stringify({ ...packet, source_digest: "" })),
    ObservationFrameError,
    "source_digest",
  );
});

Deno.test("packet parsing refuses malformed JSON and each required field", async () => {
  // Catches callers bypassing the validated packet boundary with partial or malformed input.
  assertThrows(() => parseObservationFramePacket("not json"), ObservationFrameError, "JSON");
  const packet = JSON.parse(await fixtureText()) as Record<string, unknown>;
  for (const field of ["episode_id", "title", "labels", "spatial", "source_digest"]) {
    const missing = { ...packet };
    delete missing[field];
    assertThrows(
      () => parseObservationFramePacket(JSON.stringify(missing)),
      ObservationFrameError,
      field,
    );
  }
  assertThrows(
    () =>
      parseObservationFramePacket(JSON.stringify({ ...packet, labels: { object: "geography" } })),
    ObservationFrameError,
    "labels",
  );
  assertThrows(
    () =>
      parseObservationFramePacket(
        JSON.stringify({ ...packet, spatial: { source: "", readout: 7 } }),
      ),
    ObservationFrameError,
    "spatial",
  );
});

Deno.test("packet parsing preserves a world seed above JavaScript's safe integer range", async () => {
  // Catches JSON number coercion that changes the observed world identity.
  const packet = parseObservationFramePacket(
    (await fixtureText()).replace('"world_seed": "42"', '"world_seed": "9007199254740993"'),
  );
  assertEquals(packet.world_seed, "9007199254740993");
  assertEquals(renderObservationFrame(packet, LAPTOP).provenance.worldSeed, "9007199254740993");
});

Deno.test("browser preview output exists at phone and laptop sizes with supplied evidence", async () => {
  // Catches a renderer that only returns state without a browser-inspectable visual surface.
  const packet = await fixture();
  for (const viewport of [PHONE, LAPTOP]) {
    const output = renderObservationFrameHtml(packet, viewport);
    assert(output.length > 0);
    assert(output.includes("<svg"));
    assert(output.includes(`data-width="${viewport.width}"`));
    assert(output.includes("How the underworld gathers into chambers"));
    assert(output.includes("geography"));
    assert(output.includes("world"));
    assert(output.includes("vertex 30 — fracture cave"));
    assert(output.includes("Chambers gather into connected cave systems across depth bands."));
    assert(!output.includes("comparison_reference"));
  }
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
