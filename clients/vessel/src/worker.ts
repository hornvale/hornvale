// The Casement's worker: owns the wasm instance so multi-second genesis
// never blocks the page. One worker = one world; a page wanting two
// casements (the diptych) spawns two workers.
import type { WorkerRequest, WorkerResponse } from "./protocol.ts";

interface VesselExports {
  memory: WebAssembly.Memory;
  hv_start(seed: bigint): number;
  hv_in_ptr(): number;
  hv_handle(len: number): number;
  hv_out_ptr(): number;
  hv_out_len(): number;
}

// Deno's default check lib types `self` for a window; cast to the small
// worker surface we actually use instead of pulling in a lib switch.
const scope = self as unknown as {
  postMessage(msg: WorkerResponse): void;
  onmessage: ((e: MessageEvent<WorkerRequest>) => void) | null;
  location: { href: string };
};

let vessel: VesselExports | null = null;

async function instantiate(): Promise<VesselExports> {
  if (vessel) return vessel;
  const url = new URL("./vessel.wasm", scope.location.href);
  const resp = await fetch(url);
  if (!resp.ok) {
    throw new Error(
      `vessel.wasm is missing (HTTP ${resp.status}) — local build? run 'make wasm-vessel'`,
    );
  }
  // Streaming needs an application/wasm MIME; fall back for local
  // mdbook-serve setups that mislabel it. Imports object is EMPTY —
  // the module may ask the host for nothing (spec guarantee).
  let instance: WebAssembly.Instance;
  try {
    ({ instance } = await WebAssembly.instantiateStreaming(resp.clone(), {}));
  } catch {
    ({ instance } = await WebAssembly.instantiate(await resp.arrayBuffer(), {}));
  }
  vessel = instance.exports as unknown as VesselExports;
  return vessel;
}

function readOut(v: VesselExports): string {
  return new TextDecoder().decode(
    new Uint8Array(v.memory.buffer, v.hv_out_ptr(), v.hv_out_len()),
  );
}

function writeIn(v: VesselExports, line: string): number {
  const bytes = new TextEncoder().encode(line);
  if (bytes.length > 4096) return -1;
  new Uint8Array(v.memory.buffer, v.hv_in_ptr(), bytes.length).set(bytes);
  return bytes.length;
}

scope.onmessage = async (e: MessageEvent<WorkerRequest>) => {
  const msg = e.data;
  try {
    const v = await instantiate();
    if (msg.type === "start") {
      const rc = v.hv_start(BigInt(msg.seed));
      scope.postMessage(
        rc === 0 ? { type: "started", text: readOut(v) } : { type: "error", text: readOut(v) },
      );
    } else {
      const len = writeIn(v, msg.line);
      if (len < 0) {
        scope.postMessage({ type: "out", text: "That command is too long.", released: false });
        return;
      }
      const rc = v.hv_handle(len);
      if (rc < 0) {
        scope.postMessage({ type: "error", text: `The casement is dark: protocol error ${rc}.` });
        return;
      }
      scope.postMessage({ type: "out", text: readOut(v), released: rc === 1 });
    }
  } catch (err) {
    scope.postMessage({ type: "error", text: `The casement is dark: ${err}` });
  }
};
