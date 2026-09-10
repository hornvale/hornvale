/// <reference lib="dom" />

import { assert, assertEquals, assertThrows } from "@std/assert";
import { parseHTML } from "npm:linkedom@0.18.12";
import {
  type FramePacket,
  ObservationFrameError,
  parseObservationFramePacket,
  renderObservationFrame,
  renderObservationFrameHtml,
  renderObservationPreview,
} from "./observation.ts";
import { mountObservationPreview } from "./observation_preview.ts";

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

async function neighborFixture(): Promise<FramePacket> {
  const text = await Deno.readTextFile(
    new URL("../../../observations/fixtures/HV-009/render-input.json", import.meta.url),
  );
  return parseObservationFramePacket(text);
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
    ["count_unit", "material", "object", "primary_axis", "scale", "zebra", "äther"],
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

Deno.test("neighbor observation preserves supplied stellar evidence at phone and laptop sizes", async () => {
  // Catches the public-scale preview dropping the scene payload, its unit, or
  // the seed provenance while adapting to either review viewport.
  const packet = await neighborFixture();
  const scene = JSON.parse(packet.spatial.readout) as Record<string, unknown>;
  assertEquals(packet.episode_id, "HV-009");
  assertEquals(packet.labels.count_unit, "stars");
  assertEquals(packet.spatial.source, "hornvale scene/neighbors/v1 stdout");
  assertEquals(scene.schema, "scene/neighbors/v1");
  assertEquals(scene.seed, 42);

  const preview = renderObservationPreview(packet);
  for (const frame of [preview.phone, preview.laptop]) {
    assertEquals(frame.state.map.content, packet.spatial.readout);
    assertEquals(frame.state.countUnitLabel, "stars");
    assertEquals(frame.state.provenance.worldSeed, "42");
    assert(frame.html.includes("The notable stars of seed 42"));
    assert(frame.html.includes("scene/neighbors/v1 stdout"));
    assert(frame.html.includes("Five notable neighbor stars stand at fixed positions"));
    assertEquals((frame.html.match(/data-star-kind=/g) ?? []).length, 153);
    assert(frame.html.includes('data-star-kind="neighbor" data-ra="81.841371"'));
    assert(frame.html.includes('data-star-kind="field"'));
    assert(!frame.html.includes("<foreignObject"));
  }
  assertEquals(preview.phone.state.viewport, PHONE);
  assertEquals(preview.laptop.state.viewport, LAPTOP);
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
  const missingCountUnit = JSON.parse(await fixtureText()) as Record<string, unknown>;
  (missingCountUnit.labels as Record<string, unknown>).count_unit = undefined;
  assertThrows(
    () => parseObservationFramePacket(JSON.stringify(missingCountUnit)),
    ObservationFrameError,
    "labels.count_unit",
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

Deno.test("packet parsing accepts only canonical decimal u64 seeds", async () => {
  // Catches aliases that identify a different serialized world or exceed Rust u64.
  const input = await fixtureText();
  for (const seed of ["0", "18446744073709551615"]) {
    assertEquals(
      parseObservationFramePacket(input.replace('"world_seed": "42"', `"world_seed": "${seed}"`))
        .world_seed,
      seed,
    );
  }
  for (const seed of ["00", "01", "+1", "1_0", "18446744073709551616"]) {
    assertThrows(
      () =>
        parseObservationFramePacket(input.replace('"world_seed": "42"', `"world_seed": "${seed}"`)),
      ObservationFrameError,
      "world_seed",
    );
  }
});

Deno.test("packet parsing refuses non-finite time values", async () => {
  // Catches Infinity/NaN escaping JSON validation into render state.
  const input = await fixtureText();
  assertThrows(
    () => parseObservationFramePacket(input.replace('"time_day": null', '"time_day": 1e309')),
    ObservationFrameError,
    "time_day",
  );
  assertThrows(
    () => parseObservationFramePacket(input.replace('"time_day": null', '"time_day": "day 1"')),
    ObservationFrameError,
    "time_day",
  );
});

Deno.test("packet parsing accepts only safe uint32 frame indexes", async () => {
  const packet = JSON.parse(await fixtureText()) as Record<string, unknown>;
  for (const frameIndex of [0, 4294967295]) {
    assertEquals(
      parseObservationFramePacket(JSON.stringify({ ...packet, frame_index: frameIndex }))
        .frame_index,
      frameIndex,
    );
  }
  for (const frameIndex of [-1, 1.5, 4294967296, Number.MAX_SAFE_INTEGER + 1]) {
    assertThrows(
      () => parseObservationFramePacket(JSON.stringify({ ...packet, frame_index: frameIndex })),
      ObservationFrameError,
      "frame_index",
    );
  }
});

Deno.test("browser preview output exists at phone and laptop sizes with supplied evidence", async () => {
  // Catches a renderer that only returns state without a browser-inspectable visual surface.
  const packet = await fixture();
  packet.time_day = 12.5;
  for (const viewport of [PHONE, LAPTOP]) {
    const output = renderObservationFrameHtml(packet, viewport);
    assert(output.length > 0);
    assert(output.includes("<svg"));
    assert(output.includes(`data-width="${viewport.width}"`));
    assert(output.includes("How the underworld gathers into chambers"));
    assert(output.includes("geography"));
    assert(output.includes("world"));
    assert(output.includes("chambers"));
    assert(output.includes("vertex 30 — fracture cave"));
    assert(output.includes("Chambers gather into connected cave systems across depth bands."));
    assert(output.includes(packet.world_revision));
    assert(output.includes("12.5"));
    assert(output.includes(packet.world_seed));
    assert(output.includes(packet.source_digest));
    assert(!output.includes("comparison_reference"));
  }
});

Deno.test("preview exposes independently inspectable phone and laptop frame states", async () => {
  // Catches a preview that only tests text fragments instead of both rendered frame outputs.
  const packet = await fixture();
  const preview = renderObservationPreview(packet);

  assertEquals(preview.phone.state.layout, "phone");
  assertEquals(preview.phone.state.viewport, PHONE);
  assertEquals(preview.phone.state.map.content, packet.spatial.readout);
  assertEquals(preview.phone.state.annotation.text, packet.labels.observation_sentence);
  assertEquals(preview.phone.state.countUnitLabel, "chambers");
  assert(preview.phone.html.length > 0);

  assertEquals(preview.laptop.state.layout, "laptop");
  assertEquals(preview.laptop.state.viewport, LAPTOP);
  assertEquals(preview.laptop.state.title, packet.title);
  assertEquals(preview.laptop.state.provenance.worldRevision, packet.world_revision);
  assertEquals(preview.laptop.state.provenance.sourceDigest, packet.source_digest);
  assert(preview.laptop.html.length > 0);
});

Deno.test("browser mount is exercised through a DOM-capable harness", async () => {
  // Catches a preview entry that is exported but never mounts inspectable browser frames.
  const { document } = parseHTML('<div id="preview"></div>') as unknown as { document: Document };
  const root = document.getElementById("preview")!;
  const previousDocument = globalThis.document;
  globalThis.document = document;
  try {
    mountObservationPreview(root, await fixtureText());
  } finally {
    globalThis.document = previousDocument;
  }

  const sections = root.querySelectorAll("section");
  assertEquals(sections.length, 2);
  assertEquals(sections[0].getAttribute("aria-label"), "Phone observation preview");
  assertEquals(sections[1].getAttribute("aria-label"), "Laptop observation preview");
  for (const section of sections) {
    const iframe = section.querySelector("iframe")!;
    assert(iframe.srcdoc.length > 0);
    const frame = (parseHTML(iframe.srcdoc) as unknown as { document: Document }).document;
    assertEquals(
      frame.querySelector("h1")?.textContent,
      "How the underworld gathers into chambers",
    );
    assertEquals(frame.querySelector("[data-map] pre")?.textContent?.includes("vertex 30"), true);
    assertEquals(
      frame.querySelector('[aria-label="Observation"] p')?.textContent,
      "Chambers gather into connected cave systems across depth bands.",
    );
    assertEquals(frame.querySelector("header p")?.textContent?.includes("chambers"), true);
    assertEquals(frame.querySelector("[data-provenance]")?.textContent?.includes("revision"), true);
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
