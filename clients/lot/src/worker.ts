// The exhibit's worker: owns the wasm instance so multi-second genesis
// never blocks the page. One worker = one world, exactly as the Casement's
// worker is one worker = one possession — `hw_new` invalidates whatever
// world the instance held, so a second world means a second worker.
import {
  type Envelope,
  siteArgument,
  type WorkerRequest,
  type WorkerResponse,
} from "./protocol.ts";

/** The `hw_*` catalog exports this exhibit uses (`clients/world-wasm`). */
interface CatalogExports {
  memory: WebAssembly.Memory;
  hw_new(seed: bigint): number;
  hw_lot(index: bigint): number;
  hw_lot_pinned(index: bigint, year: number, site: number): number;
  hw_lot_curve(): number;
  hw_lot_places(year: number): number;
  hw_out_ptr(): number;
  hw_out_len(): number;
}

// Deno's default check lib types `self` for a window; cast to the small
// worker surface we actually use instead of pulling in a lib switch.
const scope = self as unknown as {
  postMessage(msg: WorkerResponse): void;
  onmessage: ((e: MessageEvent<Envelope>) => void) | null;
  location: { href: string };
};

let catalog: CatalogExports | null = null;

async function instantiate(): Promise<CatalogExports> {
  if (catalog) return catalog;
  const url = new URL("./world.wasm", scope.location.href);
  const resp = await fetch(url);
  if (!resp.ok) {
    throw new Error(
      `world.wasm is missing (HTTP ${resp.status}) — local build? run 'make wasm-world'`,
    );
  }
  // Streaming needs an application/wasm MIME; fall back for local
  // mdbook-serve setups that mislabel it. The imports object is EMPTY —
  // the catalog asks the host for nothing, which is what makes a world
  // derived here identical to one derived natively.
  let instance: WebAssembly.Instance;
  try {
    ({ instance } = await WebAssembly.instantiateStreaming(resp.clone(), {}));
  } catch {
    ({ instance } = await WebAssembly.instantiate(await resp.arrayBuffer(), {}));
  }
  catalog = instance.exports as unknown as CatalogExports;
  return catalog;
}

/** The out buffer as UTF-8 — every `hw_*` answer, error envelope included,
 * arrives here. */
function readOut(c: CatalogExports): string {
  return new TextDecoder().decode(
    new Uint8Array(c.memory.buffer, c.hw_out_ptr(), c.hw_out_len()),
  );
}

/** Run one request against the live instance, returning its status code. */
function dispatch(c: CatalogExports, request: WorkerRequest): number {
  switch (request.kind) {
    case "new":
      return c.hw_new(BigInt(request.seed));
    case "lot":
      return request.year === null
        ? c.hw_lot(BigInt(request.index))
        : c.hw_lot_pinned(BigInt(request.index), request.year, siteArgument(request.site));
    case "curve":
      return c.hw_lot_curve();
    case "places":
      return c.hw_lot_places(request.year);
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
