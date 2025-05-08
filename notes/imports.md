# Imports, Modules & Tooling – design notes

These notes condense our internal discussion about **switching the compiler
output to native ES-modules** (`.mjs`) and what that means for *local* (not
yet published) development.  Compatibility knobs for old runtimes, dual CJS
builds, release strategy, etc. are deliberately glossed over – we have no
users yet so they are low-priority.

---

## 1 Repo layout & workspaces

```
repo/
│  package.json              # declares workspaces
│
└─ packages/
    ├─ pl-runtime/           # TypeScript → JS (ESM)
    └─ pl-lib/               # Prolog → JS (via our compiler)
```

*   `package.json` (root) has `"workspaces": ["packages/*"]`.  Running
    `npm install` (or pnpm / Yarn) creates **symlinks** in
    `node_modules/@pl-runtime` and `@pl-lib`.  No `npm link` dance; saving a
    source file is enough for downstream code to see the change.

*   Typical watch builds

    ```bash
    # Terminal A – runtime
    tsc -b packages/pl-runtime --watch --incremental

    # Terminal B – lib (our compiler in watch mode)
    swipl -g "compile_changed_files"            # sketch only
    ```

*   To run a sample program while editing:

    ```bash
    node --watch dist/demo.mjs        # Node ≥ 18 auto-restarts on change
    ```

That loop gives _save → rebuild (tens of ms) → automatic restart_ with zero
manual relinking.

---

## 2 Import specifiers – how Node resolves `@pl-runtime`

Two ergonomic options – both work fine inside the same monorepo.

### A. Workspace symlinks (the “publish-style” path)

Totally hands-off: the symlink created by the workspace is enough.

```js
import { Var } from '@pl-runtime';
```

Pros: identical to what will happen once the packages are published.

### B. Internal *imports map*

Keep the package names private and address them with `#` aliases:

```jsonc
// package.json (root)
{
  "type": "module",
  "imports": {
    "#pl-runtime": "./packages/pl-runtime/dist/runtime.mjs",
    "#pl-lib"    : "./packages/pl-lib/dist/index.mjs"
  }
}
```

Usage:

```js
import { Var } from '#pl-runtime';
```

Works in Node ≥ 16.  Each application package that wants the alias needs the
same `imports` block.

---

## 3 Browser story (bundled or not)

### 3.1 Bundled build (mainline approach)

Any modern bundler understands bare-specifier ESM, resolves the workspaces and
spits out one browser-ready file.

* **Vite** (Rollup underneath) – great DX, live-reload:  

  ```js
  // vite.config.js
  export default {
    resolve: { dedupe: ['@pl-runtime', '@pl-lib'] }
  }
  ```

* **esbuild** – blazingly fast CI builds:

  ```bash
  esbuild src/main.mjs --bundle --format=iife --outfile=dist/app.js
  ```

* **webpack / Rollup** – equally fine; just ensure `resolve.symlinks:false`
  (webpack) so that workspace symlinks are not duplicated.

### 3.2 No-bundle dev server (quick demos)

Serve the files as native modules and add an **HTML import map**:

```html
<script type="importmap">
  {
    "imports": {
      "@pl-runtime": "/packages/pl-runtime/dist/runtime.mjs",
      "@pl-lib"    : "/packages/pl-lib/dist/index.mjs"
    }
  }
</script>
<script type="module" src="/demo/main.mjs"></script>
```

Chromium 89+, Firefox 108+, Safari 16+ handle this natively.  Older browsers
can be shimmed, but that is future work.

---

## 4 Node-vs-browser API surface (quick checklist)

Keep `runtime.ts` and generated JS free of Node-specific modules such as `fs`,
`path`, `child_process`, etc.  `import.meta.url` is safe everywhere.  If a
polyfill becomes necessary (e.g. `TextEncoder` on ancient Safari) it can be
added as a regular dependency and will be bundled automatically.

---

### In scope vs. out of scope

* **In scope now** – workspace setup, watch builds, import resolution, basic
  bundling pipeline.
* **Out of scope for the moment** – legacy CJS builds, detailed language
  downgrades, source-map chain, ultra-old browser shims, etc.  We will circle
  back when real users appear.
