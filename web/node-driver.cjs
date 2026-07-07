// Node CLI shim for the browser compiler — a benchmarking/test tool, not for
// production. `web/build.sh` concatenates this ahead of the GHCJS `all.js` into
// `web/site/solcore-node.cjs`, so:
//
//   node web/site/solcore-node.cjs <file.solc> [iterations]
//
// `all.js` only runs correctly as a genuine top-level module (its RTS throws if
// eval'd or required indirectly), which is why we prepend rather than import it.
// It installs globalThis.compileSolcore, then Haskell main() returns and the RTS
// calls process.exit — stubbed here so the process survives long enough to drive
// a compile. The driver polls for the global (main runs on the event loop, after
// this synchronous prologue) and is wrapped in an IIFE so its locals don't
// collide with all.js's.
(function () {
  const fs = require("fs");
  const path = require("path");

  const realExit = process.exit.bind(process);
  process.exit = function () {}; // neutralise the RTS's exit-on-main-return

  const file = process.argv[2];
  const iterations = parseInt(process.argv[3] || "0", 10);
  if (!file) {
    console.error("usage: node solcore-node.cjs <file.solc> [iterations]");
    return realExit(2);
  }

  // std-cache.bin sits next to the assembled script (both under web/site).
  const cachePath = path.join(__dirname, "std-cache.bin");

  (function waitForCompiler(polls) {
    if (typeof globalThis.compileSolcore !== "function") {
      if (polls > 2000) {
        console.error("compileSolcore never registered");
        return realExit(1);
      }
      return setTimeout(function () {
        waitForCompiler(polls + 1);
      }, 5);
    }

    // Warm the std typecheck cache the way worker.js does in the browser, so the
    // measured path matches a warm in-browser compile.
    if (typeof globalThis.solcoreLoadStdCache === "function" && fs.existsSync(cachePath)) {
      globalThis.solcoreLoadStdCache(fs.readFileSync(cachePath, "latin1"));
    }

    const source = fs.readFileSync(file, "utf8");
    const result = globalThis.compileSolcore(source, {});
    if (!result.ok) {
      console.error(result.errors || "(compile failed with no diagnostics)");
      return realExit(1);
    }
    console.log("ok       :", result.ok);
    console.log("cache    :", result.cache);

    if (iterations > 0) {
      const times = [];
      for (let i = 0; i < iterations; i++) {
        const t0 = performance.now();
        globalThis.compileSolcore(source, {});
        times.push(performance.now() - t0);
      }
      times.sort(function (a, b) {
        return a - b;
      });
      const sum = times.reduce(function (a, b) {
        return a + b;
      }, 0);
      console.log(
        "bench    : " +
          iterations +
          " iters | min " +
          times[0].toFixed(1) +
          " | median " +
          times[times.length >> 1].toFixed(1) +
          " | mean " +
          (sum / iterations).toFixed(1) +
          " ms",
      );
    }
    return realExit(0);
  })(0);
})();
