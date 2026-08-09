// A side-effecting module: registers a `document` global before anything
// that needs one imports it. Deno's runtime carries no DOM of its own
// (`deno test`'s own error message on a bare `document` reference points
// at this exact fix: "Use a library like happy-dom, deno_dom, linkedom or
// JSDom"), and `main.ts`'s module-scope `document.getElementById("casement")`
// runs at IMPORT time, not at test-call time — so the registration has to
// land before `main.ts` is ever requested.
//
// This only works because of ES module evaluation order, not source-line
// order: a module's own top-level statements run only after every module it
// imports has finished evaluating. Setting `globalThis.document` from
// *inside* the same file that also does `import { renderInto } from
// "./main.ts"` is too late — `main.ts` is one of that file's requested
// modules, so `main.ts` (and its own `document.getElementById` call) would
// already have run by the time this file's own body executed. Putting the
// registration in its own leaf module, imported with a bare `import
// "./dom_shim.ts";` BEFORE the `./main.ts` import, makes it the earlier
// sibling in the requested-modules list — evaluated, body and all, before
// `./main.ts` is even instantiated.
import { parseHTML } from "linkedom";

const { document } = parseHTML("<!doctype html><html><body></body></html>");
// `document` is already ambient-typed as the real DOM's `Document` (via
// `main.ts`'s `/// <reference lib="dom" />`, shared across the whole
// program), so every caller's `document.createElement`/`querySelectorAll`/
// `textContent` is checked against that shape. Only the runtime value below
// is linkedom's — hence the cast, once, here.
globalThis.document = document as unknown as Document;
