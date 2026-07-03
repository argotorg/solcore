#!/usr/bin/env bash
# Build the in-browser solcore compiler and assemble a servable site in web/site.
# Run inside the ghcjs `nix develop` shell.
#
#   ./web/build.sh            dev build   (-O0, fast, unminified)
#   ./web/build.sh --release  release     (-O1, esbuild-minified, + precompressed .gz)
set -euo pipefail

cd "$(dirname "$0")/.."

release=0
[ "${1:-}" = "--release" ] && release=1

if [ "$release" = 1 ]; then
  project=cabal-ghcjs-o1.project
  builddir=dist-ghcjs-o1
else
  project=cabal-ghcjs.project
  builddir=dist-ghcjs
fi

# Wipe the build dir when package/project metadata changed since the last
# successful build. Cabal's incremental multi-package plan doesn't survive a
# change in module lists and fails with confusing package-id conflicts, so a
# clean rebuild is required in that case (and only that case).
stamp="$builddir/.build-stamp"
metadata=(sol-core.cabal web/solcore-web.cabal "$project")
clean=0
if [ ! -f "$stamp" ]; then
  clean=1
else
  for f in "${metadata[@]}"; do
    if [ "$f" -nt "$stamp" ]; then
      echo "metadata ($f) changed since last build — cleaning $builddir"
      clean=1
      break
    fi
  done
fi

# Guard against a JS build plan contaminated with a foreign GHC's package-ids.
# A `cabal update` (or any resolve run without the JS toolchain in scope) can
# rewrite $builddir/cache/plan.json with the *native* ghc's ids; the JS compiler
# then fails later with a cryptic `cannot satisfy -package-id base-...`. Compare
# the base package-id baked into the cached plan against the one this JS
# toolchain actually provides; on a mismatch the plan is stale and the builddir
# must be regenerated (deps stay in the global store, so only the local packages
# recompile).
plan="$builddir/cache/plan.json"
if [ "$clean" = 0 ] && [ -f "$plan" ]; then
  actual_base=$(javascript-unknown-ghcjs-ghc-pkg field base id 2>/dev/null | awk '{print $2}')
  if [ -n "$actual_base" ] && ! grep -q "\"$actual_base\"" "$plan"; then
    echo "JS build plan references a foreign base package-id (expected $actual_base) — cleaning $builddir"
    clean=1
  fi
fi

if [ "$clean" = 1 ] && [ -d "$builddir" ]; then
  rm -rf "$builddir"
fi

cabal build exe:solcore-web --project-file="$project" --builddir="$builddir"
# Record a successful configure/build so the next run only cleans on real changes.
touch "$stamp"
jsexe="$(cabal list-bin exe:solcore-web --project-file="$project" --builddir="$builddir").jsexe"

rm -rf web/site
mkdir -p web/site
cp web/index.html web/site/index.html    # the React IDE (entry point)
cp web/simple.html web/site/simple.html  # the minimal two-textarea page
cp web/worker.js web/site/worker.js
cp -r web/vendor web/site/vendor   # local React UMD (no CDN dependency)

# Precompile the IDE's JSX to plain JS with esbuild (no in-browser Babel).
# --bundle + the .solc text loader inline the real std sources (single source of
# truth) into ide.js; React/ReactDOM stay UMD globals (referenced, not imported).
npx --yes esbuild@0.24.0 web/ide.jsx --bundle --jsx=transform --loader:.solc=text \
  --minify --outfile=web/site/ide.js

if [ "$release" = 1 ]; then
  # Minify with esbuild (fetched on demand via npx). The win over raw GHCJS
  # output is small — the codegen is already compact — so fall back gracefully.
  if npx --yes esbuild@0.24.0 "$jsexe/all.js" --minify --outfile=web/site/all.js 2>/dev/null; then
    echo "minified all.js with esbuild"
  else
    echo "warning: esbuild unavailable, using unminified all.js" >&2
    cp "$jsexe/all.js" web/site/all.js
  fi
  # Precompress for hosts serving with `precompressed gzip` (e.g. Caddy).
  gzip -9 -kf web/site/all.js
else
  cp "$jsexe/all.js" web/site/all.js
fi

# Precompiled std typecheck cache, so the browser's first-ever compile (empty
# IndexedDB) starts warm. Generated with the native toolchain (deterministic,
# unlike driving the synchronous JS compile under Node); the content-hash keys
# and serialized AST are toolchain-independent, so the blob is byte-identical to
# one the JS build would dump. Uses the default (native) cabal project.
cabal run -v0 exe:gen-std-cache -- web/site/std-cache.bin
[ "$release" = 1 ] && gzip -9 -kf web/site/std-cache.bin

mode=$([ "$release" = 1 ] && echo "release (-O1, minified)" || echo "dev (-O0)")
echo "Built: $mode"
# .gz files only exist for release builds; tolerate their absence under pipefail.
ls -la web/site/all.js web/site/all.js.gz web/site/std-cache.bin web/site/std-cache.bin.gz 2>/dev/null | awk '{print "  " $5 "  " $NF}' || true
echo "Serve with:  (cd web/site && python3 -m http.server 8000)"
echo "  React IDE (main):   http://localhost:8000/index.html"
echo "  simple page:        http://localhost:8000/simple.html"
