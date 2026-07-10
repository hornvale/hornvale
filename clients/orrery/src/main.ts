/// <reference lib="dom" />
// The live orrery: canvas render loop + controls. All logic lives in the
// tested modules (scene/ephemeris/palette/moon/globe/clock); this file only
// wires fetch → parse → per-frame draw and the play/scrub/speed controls.

import { parseSystem, parseTiles, type SystemScene, type TilesScene } from "./scene.ts";
import { moonPhase, rotationPhase, worldPhase } from "./ephemeris.ts";
import { starTint } from "./palette.ts";
import { drawMoon } from "./moon.ts";
import { drawGlobe } from "./globe.ts";
import { clockToDay } from "./clock.ts";

const TAU = Math.PI * 2;
const SYSTEM_URL = "./scene-system-seed-42.json";
const TILES_URL = "./scene-tiles-seed-42.json";

function fallback(message: string): void {
  const holder = document.getElementById("orrery-holder");
  if (!holder) return;
  holder.replaceChildren();
  const p = document.createElement("p");
  const em = document.createElement("em");
  em.textContent = message;
  p.append(em);
  holder.append(p);
}

/** World-orbit display radius, in canvas px, given the canvas half-size. */
function orbitRadiusPx(half: number): number {
  return half * 0.78;
}

function drawRing(
  ctx: CanvasRenderingContext2D,
  cx: number,
  cy: number,
  auToPx: number,
  au: number,
  color: string,
): void {
  ctx.beginPath();
  ctx.arc(cx, cy, au * auToPx, 0, TAU);
  ctx.strokeStyle = color;
  ctx.lineWidth = 1;
  ctx.stroke();
}

async function boot(): Promise<void> {
  const canvas = document.getElementById("orrery-canvas") as HTMLCanvasElement | null;
  const controls = document.getElementById("orrery-controls");
  if (!canvas || !controls) return;

  let systemText: string;
  let tilesText: string;
  try {
    const [systemResponse, tilesResponse] = await Promise.all([
      fetch(SYSTEM_URL),
      fetch(TILES_URL),
    ]);
    if (!systemResponse.ok) throw new Error(`HTTP ${systemResponse.status} for ${SYSTEM_URL}`);
    if (!tilesResponse.ok) throw new Error(`HTTP ${tilesResponse.status} for ${TILES_URL}`);
    [systemText, tilesText] = await Promise.all([systemResponse.text(), tilesResponse.text()]);
  } catch (e) {
    fallback(`The orrery needs to be served over HTTP to load its data (${e}).`);
    return;
  }

  let sys: SystemScene;
  let tiles: TilesScene;
  try {
    sys = parseSystem(systemText);
    tiles = parseTiles(tilesText);
  } catch (e) {
    fallback(`A scene document failed validation: ${e}.`);
    return;
  }

  const ctx = canvas.getContext("2d")!;
  const cw = canvas.width;
  const ch = canvas.height;
  const cx = cw / 2;
  const cy = ch / 2;
  const aspectX = cw / ch;
  const half = Math.min(cw, ch) / 2;
  const R = orbitRadiusPx(half);
  const auToPx = R / sys.world.orbitAu;
  const worldRadiusPx = Math.round(Math.max(6, half * 0.05));
  const moonRadiusPx = Math.max(2, worldRadiusPx * 0.28);
  const moonGapPx = 6;
  // Orbit radii start clear of the globe's edge and step outward by a full
  // moon diameter + gap per rank, so moons never overlap the globe or each other.
  const moonBaseRadiusPx = worldRadiusPx + moonRadiusPx + moonGapPx;
  const moonStepPx = moonRadiusPx * 2 + moonGapPx;
  const tint = starTint(sys.star.className);
  const moonOrder = sys.moons
    .map((moon, i) => ({ i, distanceMm: moon.distanceMm }))
    .sort((a, b) => a.distanceMm - b.distanceMm)
    .map(({ i }, rank) => ({ i, rank }));

  let t = 0;
  let playing = true;
  let daysPerSecond = 30;
  let lastFrameMs: number | null = null;

  const playButton = document.createElement("button");
  const scrub = document.createElement("input");
  const speed = document.createElement("input");
  const speedLabel = document.createElement("span");

  function updatePlayLabel(): void {
    playButton.textContent = playing ? "pause" : "play";
  }

  playButton.type = "button";
  updatePlayLabel();
  playButton.addEventListener("click", () => {
    playing = !playing;
    lastFrameMs = null;
    updatePlayLabel();
  });

  scrub.type = "range";
  scrub.min = "0";
  scrub.max = String(sys.world.yearDays);
  scrub.step = String(sys.world.yearDays / 1000);
  scrub.value = String(t);
  scrub.addEventListener("input", () => {
    t = Number(scrub.value);
    playing = false;
    lastFrameMs = null;
    updatePlayLabel();
    draw();
  });

  speed.type = "range";
  speed.min = "1";
  speed.max = "120";
  speed.step = "1";
  speed.value = String(daysPerSecond);
  function updateSpeedLabel(): void {
    speedLabel.textContent = `${daysPerSecond} days/sec`;
  }
  updateSpeedLabel();
  speed.addEventListener("input", () => {
    daysPerSecond = Number(speed.value);
    updateSpeedLabel();
  });

  const speedWrap = document.createElement("label");
  speedWrap.append("speed ", speed, " ", speedLabel);
  const scrubWrap = document.createElement("label");
  scrubWrap.append("t ", scrub);
  controls.append(playButton, " ", scrubWrap, " ", speedWrap);

  function draw(): void {
    ctx.clearRect(0, 0, cw, ch);

    drawRing(ctx, cx, cy, auToPx, sys.star.hzInnerAu, "rgba(120,200,140,0.25)");
    drawRing(ctx, cx, cy, auToPx, sys.star.hzOuterAu, "rgba(120,200,140,0.25)");
    drawRing(ctx, cx, cy, auToPx, sys.world.orbitAu, "rgba(255,255,255,0.15)");

    const glowR = worldRadiusPx * 2.2;
    const gradient = ctx.createRadialGradient(cx, cy, 0, cx, cy, glowR);
    gradient.addColorStop(0, `rgba(${tint[0]},${tint[1]},${tint[2]},1)`);
    gradient.addColorStop(1, `rgba(${tint[0]},${tint[1]},${tint[2]},0)`);
    ctx.fillStyle = gradient;
    ctx.beginPath();
    ctx.arc(cx, cy, glowR, 0, TAU);
    ctx.fill();

    const theta = TAU * worldPhase(sys, t);
    const wx = cx + aspectX * R * Math.cos(theta);
    const wy = cy + R * Math.sin(theta);
    const worldSunAngle = Math.atan2(cy - wy, cx - wx);
    drawGlobe(
      ctx,
      wx,
      wy,
      worldRadiusPx,
      tiles,
      rotationPhase(sys, t),
      sys.world.obliquityDeg,
      worldSunAngle,
    );

    for (const { i, rank } of moonOrder) {
      const ma = theta + Math.PI + moonPhase(sys, i, t) * TAU;
      const orbitR = moonBaseRadiusPx + rank * moonStepPx;
      const mx = wx + orbitR * Math.cos(ma);
      const my = wy + orbitR * Math.sin(ma);
      const moonSunAngle = Math.atan2(cy - my, cx - mx);
      drawMoon(ctx, mx, my, moonRadiusPx, moonPhase(sys, i, t), moonSunAngle);
    }

    scrub.value = String(t);
  }

  function frame(nowMs: number): void {
    if (playing) {
      if (lastFrameMs !== null) {
        t += clockToDay(nowMs - lastFrameMs, daysPerSecond);
        t %= sys.world.yearDays;
        if (t < 0) t += sys.world.yearDays;
      }
      lastFrameMs = nowMs;
      draw();
    } else {
      lastFrameMs = null;
    }
    requestAnimationFrame(frame);
  }

  draw();
  requestAnimationFrame(frame);
}

if (typeof document !== "undefined") boot();
