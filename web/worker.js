// Web Worker hosting the GHCJS solcore compiler. Running the (synchronous)
// compile off the main thread keeps the UI responsive and lets the page show a
// live timer while compilation runs.
//
// It also persists the typecheck cache's std subset across page reloads via
// IndexedDB: on startup the stored blob is fed to the compiler so std is not
// retypechecked, and after the first successful compile the freshly-typechecked
// std dump is written back. The blob is a Latin-1 string (one char per byte),
// which IndexedDB stores directly.
self.window = self;
importScripts("all.js");

const DB_NAME = "solcore-tc-cache";
const STORE = "std";
const KEY = "blob";

let db = null;
let cachePersisted = false;
let seededModules = 0;

function openDb() {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open(DB_NAME, 1);
    req.onupgradeneeded = () => req.result.createObjectStore(STORE);
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

function idbGet(key) {
  return new Promise((resolve, reject) => {
    const req = db.transaction(STORE, "readonly").objectStore(STORE).get(key);
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

function idbPut(key, value) {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(STORE, "readwrite");
    tx.objectStore(STORE).put(value, key);
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
  });
}

// Seed the session cache from a previously persisted std dump. Any failure
// (IndexedDB unavailable, corrupt/foreign blob) degrades to no persistence:
// the compiler treats it as a cache miss and recomputes.
async function loadPersistedCache() {
  try {
    db = await openDb();
    const blob = await idbGet(KEY);
    if (typeof blob === "string" && blob.length > 0) {
      seededModules += self.solcoreLoadStdCache(blob);
    }
  } catch (e) {
    db = null;
  }
}

// First-ever load (IndexedDB empty): seed from the std cache blob shipped as a
// static asset, so even the first compile reuses std. Absent asset / offline
// degrades silently to a cold first compile. The blob is a Latin-1 byte stream;
// iso-8859-1 maps each byte back to the matching code point.
async function loadBundledCache() {
  try {
    const resp = await fetch("std-cache.bin");
    if (!resp.ok) return;
    const bytes = new Uint8Array(await resp.arrayBuffer());
    const blob = new TextDecoder("iso-8859-1").decode(bytes);
    if (blob.length > 0) {
      seededModules += self.solcoreLoadStdCache(blob);
    }
  } catch (e) {
    // No bundled cache available — recompute on first compile.
  }
}

// Write the std dump back once, after the first successful compile has
// populated it. std is stable within a session, so a single write suffices.
async function persistCache() {
  if (cachePersisted || !db) return;
  try {
    const blob = self.solcoreDumpStdCache();
    if (typeof blob === "string" && blob.length > 0) {
      await idbPut(KEY, blob);
      cachePersisted = true;
    }
  } catch (e) {
    // Leave cachePersisted false so a later compile can retry.
  }
}

function whenReady(cb) {
  typeof self.compileSolcore === "function" ? cb() : setTimeout(() => whenReady(cb), 20);
}

// Seed the session cache before telling the page the compiler is ready, so the
// first compile can already reuse std: prefer the per-session IndexedDB store,
// falling back to the bundled static blob when it is empty (first ever load).
whenReady(() => {
  loadPersistedCache()
    .then(() => (seededModules > 0 ? undefined : loadBundledCache()))
    .finally(() => self.postMessage({ type: "ready" }));
});

self.onmessage = (e) => {
  if (!e.data || e.data.type !== "compile") return;
  whenReady(() => {
    const r = self.compileSolcore(e.data.source, e.data.flags);
    self.postMessage({ type: "result", ok: r.ok, output: r.output, yul: r.yul, errors: r.errors });
    if (r.ok) persistCache();
  });
};
