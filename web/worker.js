// Web Worker hosting the GHCJS solcore compiler. Running the (synchronous)
// compile off the main thread keeps the UI responsive and lets the page show a
// live timer while compilation runs.
self.window = self;
importScripts("all.js");

function whenReady(cb) {
  typeof self.compileSolcore === "function" ? cb() : setTimeout(() => whenReady(cb), 20);
}

// Tell the page the compiler has registered.
whenReady(() => self.postMessage({ type: "ready" }));

self.onmessage = (e) => {
  if (!e.data || e.data.type !== "compile") return;
  whenReady(() => {
    const r = self.compileSolcore(e.data.source, e.data.flags);
    self.postMessage({ type: "result", ok: r.ok, output: r.output, yul: r.yul, errors: r.errors });
  });
};
