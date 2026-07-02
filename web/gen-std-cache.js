// Generate the precompiled std typecheck-cache blob shipped as a static asset
// (web/site/std-cache.bin), so the browser IDE's very first compile — before
// IndexedDB has been populated — can reuse std instead of retypechecking it.
//
// It drives the freshly-built JS compiler (web/site/all.js) under Node: compile
// a tiny source that pulls the whole std closure, dump the std cache via the
// solcoreDumpStdCache FFI, and write the raw bytes. Using the same JS build that
// will consume the blob guarantees the content-hash keys match at runtime.
//
//   node web/gen-std-cache.js <all.js> <out.bin>
const fs = require("fs");
const path = require("path");

const allJs = process.argv[2];
const outFile = process.argv[3];
if (!allJs || !outFile) {
  console.error("usage: node gen-std-cache.js <all.js> <out.bin>");
  process.exit(2);
}

globalThis.window = globalThis;

// The GHCJS RTS calls process.exit(0) when Haskell main returns; neutralize it
// so our async work can run, and keep a real exit for the verdict.
const realExit = process.exit.bind(process);
process.exit = () => {};
function done(code) {
  process.exit = realExit;
  realExit(code);
}

require(path.resolve(allJs));

// Importing std and std.dispatch pulls the full std closure (dispatch imports
// opcodes); the trivial contract makes it a well-formed compile. Bare imports
// without a contract fail on ambiguous re-exports.
const warmup =
  "import std.{*};\n" + "import std.dispatch.{*};\n" + "contract W { constructor() {} }\n";

function whenReady(cb, tries) {
  if (typeof globalThis.compileSolcore === "function") return cb();
  if (tries <= 0) {
    console.error("gen-std-cache: compiler never registered");
    done(1);
  }
  setTimeout(() => whenReady(cb, tries - 1), 20);
}

whenReady(() => {
  const r = globalThis.compileSolcore(warmup, {});
  if (!r.ok) {
    console.error("gen-std-cache: warm-up compile failed:", r.errors);
    done(1);
  }
  const blob = globalThis.solcoreDumpStdCache();
  if (typeof blob !== "string" || blob.length === 0) {
    console.error("gen-std-cache: dump produced an empty blob");
    done(1);
  }
  const modules = globalThis.solcoreLoadStdCache(blob);
  if (modules <= 0) {
    console.error("gen-std-cache: blob decoded to no modules");
    done(1);
  }
  // blob is a Latin-1 string (one char per byte); write it back to raw bytes.
  fs.writeFileSync(outFile, Buffer.from(blob, "latin1"));
  console.log(`gen-std-cache: wrote ${outFile} (${blob.length} bytes, ${modules} std modules)`);
  done(0);
}, 500);
