/// <reference lib="dom" />
// The atlas viewer's canvas/DOM glue. All logic lives in the tested
// modules; this file only wires events to redraws.

import { parseScene, type TilesScene } from "./scene.ts";
import { canvasToTile, initialViewport, pan, type Viewport, zoomAt } from "./projection.ts";
import { featureAt, featureCanvasXY, readout } from "./hittest.ts";
import { FLAGSHIP_MARK, type Layer, layerPixels, LAYERS, SETTLEMENT_MARK } from "./palette.ts";

const SCENE_URL = "./scene-tiles-seed-42.json";

function fallback(message: string) {
  const holder = document.getElementById("atlas-holder");
  if (!holder) return;
  holder.replaceChildren();

  const p = document.createElement("p");
  const em = document.createElement("em");
  em.textContent = message;
  p.append(em, " The data this page renders is ");

  const sceneLink = document.createElement("a");
  sceneLink.href = "./scene-tiles-seed-42.json";
  sceneLink.textContent = "the committed scene document";
  p.append(sceneLink, "; static renders are in ");

  const elevationLink = document.createElement("a");
  elevationLink.href = "./elevation-seed-42.html";
  elevationLink.textContent = "the elevation";
  p.append(elevationLink, " and ");

  const biomeLink = document.createElement("a");
  biomeLink.href = "./biome-seed-42.html";
  biomeLink.textContent = "biome";
  p.append(biomeLink, " pages.");

  holder.append(p);
}

async function boot() {
  const canvas = document.getElementById("atlas-canvas") as HTMLCanvasElement | null;
  const readoutEl = document.getElementById("atlas-readout");
  const controls = document.getElementById("atlas-layers");
  if (!canvas || !readoutEl || !controls) return;
  let text: string;
  try {
    const response = await fetch(SCENE_URL);
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    text = await response.text();
  } catch (e) {
    fallback(`The atlas needs to be served over HTTP to load its data (${e}).`);
    return;
  }
  let scene: TilesScene;
  try {
    scene = parseScene(text);
  } catch (e) {
    fallback(`The scene document failed validation: ${e}.`);
    return;
  }

  const cw = canvas.width;
  const ch = canvas.height;
  const ctx = canvas.getContext("2d")!;
  ctx.imageSmoothingEnabled = false;

  // Pre-render each layer once at lattice resolution.
  const layerImages = new Map<Layer, ImageBitmap>();
  for (const layer of LAYERS) {
    const data = new ImageData(
      new Uint8ClampedArray(layerPixels(scene, layer)),
      scene.width,
      scene.height,
    );
    layerImages.set(layer, await createImageBitmap(data));
  }

  let activeLayer: Layer = "biome";
  let viewport: Viewport = initialViewport();
  let hoverLine = "hover the map to inspect a tile";
  let dragging = false;
  let dragX = 0;
  let dragY = 0;

  function draw() {
    ctx.clearRect(0, 0, cw, ch);
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(
      layerImages.get(activeLayer)!,
      viewport.tx,
      viewport.ty,
      cw * viewport.scale,
      ch * viewport.scale,
    );
    for (const f of scene.features) {
      const { x, y } = featureCanvasXY(viewport, f, cw, ch);
      if (x < -8 || y < -8 || x > cw + 8 || y > ch + 8) continue;
      ctx.beginPath();
      ctx.arc(x, y, f.kind === "flagship" ? 5 : 3.5, 0, Math.PI * 2);
      ctx.fillStyle = f.kind === "flagship" ? FLAGSHIP_MARK : SETTLEMENT_MARK;
      ctx.fill();
      ctx.lineWidth = 2;
      ctx.strokeStyle = "#ffffff";
      ctx.stroke();
    }
    readoutEl!.textContent = hoverLine;
  }

  let raf = 0;
  function scheduleDraw() {
    if (raf) return;
    raf = requestAnimationFrame(() => {
      raf = 0;
      draw();
    });
  }

  // Layer switcher.
  for (const layer of LAYERS) {
    const label = document.createElement("label");
    const input = document.createElement("input");
    input.type = "radio";
    input.name = "atlas-layer";
    input.value = layer;
    input.checked = layer === activeLayer;
    input.addEventListener("change", () => {
      activeLayer = layer;
      scheduleDraw();
    });
    label.append(input, ` ${layer} `);
    controls.append(label);
  }

  // Hover.
  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    const x = (e.clientX - rect.left) * (cw / rect.width);
    const y = (e.clientY - rect.top) * (ch / rect.height);
    if (dragging) {
      viewport = pan(viewport, x - dragX, y - dragY, cw, ch);
      dragX = x;
      dragY = y;
      scheduleDraw();
      return;
    }
    const feature = featureAt(scene, viewport, x, y, cw, ch);
    if (feature) {
      hoverLine = `${feature.name} (${feature.kind})`;
    } else {
      const tile = canvasToTile(viewport, x, y, scene, cw, ch);
      hoverLine = tile ? readout(scene, tile.px, tile.py) : "—";
    }
    scheduleDraw();
  });

  // Pan.
  canvas.addEventListener("mousedown", (e) => {
    dragging = true;
    const rect = canvas.getBoundingClientRect();
    dragX = (e.clientX - rect.left) * (cw / rect.width);
    dragY = (e.clientY - rect.top) * (ch / rect.height);
  });
  globalThis.addEventListener("mouseup", () => {
    dragging = false;
  });

  // Zoom.
  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const rect = canvas.getBoundingClientRect();
    const x = (e.clientX - rect.left) * (cw / rect.width);
    const y = (e.clientY - rect.top) * (ch / rect.height);
    viewport = zoomAt(viewport, x, y, e.deltaY < 0 ? 1.25 : 0.8, cw, ch);
    scheduleDraw();
  }, { passive: false });

  draw();
}

boot();
