// The exhibit's worker: owns the wasm instance so multi-second genesis
// never blocks the page. One worker = one world, exactly as the Casement's
// worker is one worker = one possession — `hl_new` invalidates whatever
// world the instance held, so a second world means a second worker.
import {
  type Envelope,
  siteArgument,
  type WorkerRequest,
  type WorkerResponse,
} from "./protocol.ts";

/** The `hl_*` exports this exhibit uses (`clients/lot/wasm`). */
interface LotWasmExports {
  memory: WebAssembly.Memory;
  hl_new(seed: bigint): number;
  hl_lot(index: bigint): number;
  hl_lot_pinned(index: bigint, year: number, site: number): number;
  hl_lot_curve(): number;
  hl_lot_places(year: number): number;
  hl_out_ptr(): number;
  hl_out_len(): number;
}

// Deno's default check lib types `self` for a window; cast to the small
// worker surface we actually use instead of pulling in a lib switch.
const scope = self as unknown as {
  postMessage(msg: WorkerResponse): void;
  onmessage: ((e: MessageEvent<Envelope>) => void) | null;
  location: { href: string };
};

let lotWasm: LotWasmExports | null = null;

async function instantiate(): Promise<LotWasmExports> {
  if (lotWasm) return lotWasm;
  const url = new URL("./lot.wasm", scope.location.href);
  const resp = await fetch(url);
  if (!resp.ok) {
    throw new Error(
      `lot.wasm is missing (HTTP ${resp.status}) — local build? run 'make wasm-lot'`,
    );
  }
  // Streaming needs an application/wasm MIME; fall back for local
  // mdbook-serve setups that mislabel it. The imports object is EMPTY —
  // the module asks the host for nothing, which is what makes a world
  // derived here identical to one derived natively.
  let instance: WebAssembly.Instance;
  try {
    ({ instance } = await WebAssembly.instantiateStreaming(resp.clone(), {}));
  } catch {
    ({ instance } = await WebAssembly.instantiate(await resp.arrayBuffer(), {}));
  }
  lotWasm = instance.exports as unknown as LotWasmExports;
  return lotWasm;
}

/** The out buffer as UTF-8 — every `hl_*` answer, error envelope included,
 * arrives here. */
function readOut(c: LotWasmExports): string {
  return new TextDecoder().decode(
    new Uint8Array(c.memory.buffer, c.hl_out_ptr(), c.hl_out_len()),
  );
}

/** Run one request against the live instance, returning its status code. */
function dispatch(c: LotWasmExports, request: WorkerRequest): number {
  switch (request.kind) {
    case "new":
      return c.hl_new(BigInt(request.seed));
    case "lot":
      return request.year === null
        ? c.hl_lot(BigInt(request.index))
        : c.hl_lot_pinned(BigInt(request.index), request.year, siteArgument(request.site));
    case "curve":
      return c.hl_lot_curve();
    case "places":
      return c.hl_lot_places(request.year);
  }
}

scope.onmessage = async (e: MessageEvent<Envelope>) => {
  const { id, request } = e.data;
  try {
    const c = await instantiate();
    const rc = dispatch(c, request);
    if (rc !== 0) {
      // A non-zero status always leaves the sim's own reason in the out
      // buffer — a refused pin names the physical reason, and the page
      // shows that rather than a generic failure.
      scope.postMessage({ kind: "error", answers: request.kind, id, text: readOut(c) });
      return;
    }
    scope.postMessage({
      kind: "ok",
      answers: request.kind,
      id,
      json: request.kind === "new" ? "" : readOut(c),
    });
  } catch (err) {
    scope.postMessage({ kind: "error", answers: request.kind, id, text: `${err}` });
  }
};
