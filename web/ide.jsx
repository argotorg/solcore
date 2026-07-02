// solcore IDE — React tabs/file-tree frontend over the GHCJS solcore backend.
// Compiled to plain JS by esbuild at build time (see web/build.sh); React and
// ReactDOM are provided as globals by the vendored UMD scripts. Compilation
// runs in a Web Worker (web/worker.js) so the main thread stays free to render
// a live timer.
const { useState, useReducer, useCallback, useEffect, useRef } = React;

// ---- seed virtual file system (FileType nodes, mirrors remix-ui/workspace) ----
// A node: { name, path, isDirectory, children? , content? }
const seedTree = {
  name: "workspace", path: "", isDirectory: true, children: [
    { name: "examples", path: "examples", isDirectory: true, children: [
      { name: "Answer.solc", path: "examples/Answer.solc", isDirectory: false,
        content: "contract Answer {\n  public function main() -> word {\n    return 42;\n  }\n}\n" },
      { name: "Counter.solc", path: "examples/Counter.solc", isDirectory: false,
        content: "import std.{*};\nimport std.dispatch.{*};\n\ncontract Counter {\n  counter : uint256;\n\n  constructor() { counter = uint256(42); }\n\n  public function get() -> uint256 {\n    return counter;\n  }\n}\n" },
      { name: "basic.solc", path: "examples/basic.solc", isDirectory: false,
        content: "import std.{*};\nimport std.dispatch.{*};\nimport std.opcodes.{address as address_};\n\nfunction self() -> address {\n    return address(address_());\n}\n\ncontract C {\n    constructor() {}\n    public function nothing() -> () {}\n\n    // Re-enters this very contract via raw_call(address(this), ...). The payload\n    // is the 4-byte selector of an existing entry point (something(), 0xa7a0d537),\n    // built by left-aligning it in a bytes32 and truncating to 4 bytes. The inner\n    // call succeeds, so raw_call reports ok == true and returns its returndata\n    // (the abi-encoded uint256(1)).\n    public function callSelf() -> (bool, memory(bytes)) {\n        let sel: bytes32 = bytes32(0xa7a0d53700000000000000000000000000000000000000000000000000000000);\n        let payload = truncate(to_bytes(sel), 4);\n        match raw_call(self(), uint256(0), payload) {\n            | (ok, ret) => return (ok, ret);\n        }\n    }\n\n    // Same shape, but the selector (0xdeadc0de) matches no entry point, so dispatch\n    // reverts (there is no fallback). raw_call swallows the inner revert and reports\n    // ok == false; this outer call itself still succeeds and returns the revert\n    // returndata (the 4-byte NoFallback error selector).\n    public function callSelfInvalid() -> (bool, memory(bytes)) {\n        let sel: bytes32 = bytes32(0xdeadc0de00000000000000000000000000000000000000000000000000000000);\n        let payload = truncate(to_bytes(sel), 4);\n        match raw_call(self(), uint256(0), payload) {\n            | (ok, ret) => return (ok, ret);\n        }\n    }\n\n    public function something() -> (uint256) {\n        return uint256(1);\n    }\n\n    public function add2(x : uint256, y : uint256) -> uint256 {\n        return Add.add(x,y);\n    }\n\n    public function add3(x : uint256, y : uint256, z : uint256) -> uint256 {\n        return Add.add(z, Add.add(x,y));\n    }\n\n    public function addmod3(x : uint256, y : uint256, k : uint256) -> uint256 {\n        return addmod(x, y, k);\n    }\n\n    public function mulmod3(x : uint256, y : uint256, k : uint256) -> uint256 {\n        return mulmod(x, y, k);\n    }\n\n    // Bitwise / modulo via the syntactic sugar only (no explicit class calls):\n    //   `^` -> BitXor.bxor, `|` -> BitOr.bor, `&` -> BitAnd.band, `%` -> Mod.mod.\n    public function bxor2(x : uint256, y : uint256) -> uint256 {\n        return x ^ y;\n    }\n\n    public function bor2(x : uint256, y : uint256) -> uint256 {\n        return x | y;\n    }\n\n    public function band2(x : uint256, y : uint256) -> uint256 {\n        return x & y;\n    }\n\n    public function mod2(x : uint256, y : uint256) -> uint256 {\n        return x % y;\n    }\n\n    public function id_bytes(b: memory(bytes)) -> memory(bytes) {\n        return b;\n    }\n\n    public function id_string(b: memory(string)) -> memory(string) {\n        return b;\n    }\n\n    public function id_bytes32(b: bytes32) -> bytes32 {\n        return b;\n    }\n\n    public function id_address(a: address) -> address {\n        return a;\n    }\n\n    public function id_pair() -> (uint256, uint256) {\n        return (uint256(7), uint256(11));\n    }\n\n    function hidden() -> (uint256) {\n        return uint256(42);\n    }\n}\n" },
    ]},
    { name: "Scratch.solc", path: "Scratch.solc", isDirectory: false,
      content: "contract Scratch {\n  public function main() -> word {\n    return 7;\n  }\n}\n" },
  ]
};

// Collect path -> content for all files, for the mutable editor buffers.
function collectFiles(node, acc) {
  if (node.isDirectory) node.children.forEach(c => collectFiles(c, acc));
  else acc[node.path] = node.content;
  return acc;
}

const basename = (p) => p.split("/").pop();

// ---- tab controller (mirrors apps/remix-ide tab-proxy: loadedTabs + open/close/select) ----
const initialTabs = { open: [], active: null };
function tabsReducer(state, action) {
  switch (action.type) {
    case "OPEN": {
      const open = state.open.includes(action.path) ? state.open : [...state.open, action.path];
      return { open, active: action.path };
    }
    case "SELECT":
      return { ...state, active: action.path };
    case "CLOSE": {
      const idx = state.open.indexOf(action.path);
      if (idx === -1) return state;
      const open = state.open.filter(p => p !== action.path);
      let active = state.active;
      if (state.active === action.path) {              // pick a neighbour, like tab-proxy
        active = open[idx - 1] || open[idx] || null;
      }
      return { open, active };
    }
    default: return state;
  }
}

// ---- tree view (mirrors remix-ui/tree-view: recursive ul/li with caret) ----
function TreeNode({ node, depth, activePath, onOpen }) {
  const [expanded, setExpanded] = useState(true);
  const pad = { paddingLeft: depth * 12 };
  if (node.isDirectory) {
    return (
      <li>
        <div className="node-row" style={pad} onClick={() => setExpanded(!expanded)}>
          <span className="caret">{expanded ? "▾" : "▸"}</span>{node.name}
        </div>
        {expanded && (
          <ul>{node.children.map(c =>
            <TreeNode key={c.path} node={c} depth={depth + 1} activePath={activePath} onOpen={onOpen} />)}
          </ul>
        )}
      </li>
    );
  }
  return (
    <li>
      <div className={"node-row" + (activePath === node.path ? " active" : "")} style={{ paddingLeft: depth * 12 + 14 }}
           onClick={() => onOpen(node.path)}>
        <span>📄</span>{node.name}
      </div>
    </li>
  );
}

// ---- tabs bar (mirrors remix-ui/tabs: title + middle-click / × to close) ----
function TabsBar({ open, active, onSelect, onClose }) {
  return (
    <div className="tabs">
      {open.map(path => (
        <div key={path} className={"tab" + (path === active ? " active" : "")}
             onClick={() => onSelect(path)}
             onMouseDown={(e) => { if (e.button === 1) { e.preventDefault(); onClose(path); } }}>
          <span>{basename(path)}</span>
          <span className="tab-close" onClick={(e) => { e.stopPropagation(); onClose(path); }}>×</span>
        </div>
      ))}
    </div>
  );
}

const FLAGS = [
  ["noGenDispatch", "-g no dispatch"],
];

function App() {
  const [files, setFiles] = useState(() => collectFiles(seedTree, {}));
  const [tabs, dispatch] = useReducer(tabsReducer, initialTabs);
  const [flags, setFlags] = useState({ noGenDispatch: true });
  const [result, setResult] = useState({ ok: true, hull: "", yul: "", errors: "" });
  const [ready, setReady] = useState(false);
  const [compiling, setCompiling] = useState(false);
  const [elapsed, setElapsed] = useState(null);   // ms; null before the first compile

  const workerRef = useRef(null);
  const timerRef = useRef(null);
  const startRef = useRef(0);

  // Spin up the compiler worker once; it posts "ready" when the bundle loads.
  useEffect(() => {
    const worker = new Worker("worker.js");
    workerRef.current = worker;
    worker.onmessage = (e) => {
      const msg = e.data;
      if (msg.type === "ready") { setReady(true); return; }
      if (msg.type === "result") {
        clearInterval(timerRef.current);
        setElapsed(performance.now() - startRef.current);   // freeze final time
        setCompiling(false);
        setResult(msg.ok
          ? { ok: true, hull: msg.output, yul: msg.yul, errors: "" }
          : { ok: false, hull: "", yul: "", errors: msg.errors });
      }
    };
    return () => worker.terminate();
  }, []);

  const openFile = useCallback((path) => dispatch({ type: "OPEN", path }), []);
  const editActive = (content) => setFiles(f => ({ ...f, [tabs.active]: content }));

  const compile = () => {
    if (!tabs.active || !ready || compiling) return;
    setCompiling(true);
    startRef.current = performance.now();
    setElapsed(0);
    // Tick the timer while the worker compiles.
    timerRef.current = setInterval(() => setElapsed(performance.now() - startRef.current), 47);
    workerRef.current.postMessage({ type: "compile", source: files[tabs.active], flags });
  };

  const status =
    !ready ? "loading compiler…"
    : compiling ? "compiling… " + Math.round(elapsed) + " ms"
    : elapsed != null ? (result.ok ? "compiled in " : "failed after ") + Math.round(elapsed) + " ms"
    : "compiler ready";

  return (
    <div className="ide">
      <div className="toolbar">
        <button onClick={compile} disabled={!ready || !tabs.active || compiling}>Compile</button>
        <div className="flags">
          {FLAGS.map(([key, label]) => (
            <label key={key}>
              <input type="checkbox" checked={!!flags[key]}
                     onChange={e => setFlags(f => ({ ...f, [key]: e.target.checked }))} />
              {" " + label}
            </label>
          ))}
        </div>
        <span className="status">{status}</span>
      </div>

      <div className="sidebar">
        <ul className="tree">
          <TreeNode node={seedTree} depth={0} activePath={tabs.active} onOpen={openFile} />
        </ul>
      </div>

      <div className="main">
        <TabsBar open={tabs.open} active={tabs.active}
                 onSelect={(p) => dispatch({ type: "SELECT", path: p })}
                 onClose={(p) => dispatch({ type: "CLOSE", path: p })} />
        {tabs.active ? (
          <textarea className="editor" spellCheck={false}
                    value={files[tabs.active]} onChange={e => editActive(e.target.value)} />
        ) : (
          <div className="empty">Open a file from the tree to edit and compile it.</div>
        )}
        <div className="panes">
          <div className="pane">
            <div className="pane-label">hull</div>
            <pre className={"pane-body" + (result.ok ? "" : " error")}>
              {result.ok ? result.hull : result.errors}
            </pre>
          </div>
          <div className="pane">
            <div className="pane-label">yul</div>
            <pre className="pane-body">{result.yul}</pre>
          </div>
        </div>
      </div>
    </div>
  );
}

ReactDOM.createRoot(document.getElementById("root")).render(<App />);
