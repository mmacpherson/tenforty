# WASM tax calculator

The browser demo runs the Rust graph runtime entirely in WebAssembly; tax data
does not leave the browser. The deployed site is
[mmacpherson.github.io/tenforty](https://mmacpherson.github.io/tenforty/).

Build and serve it locally with:

```console
make wasm-serve
```

The Pages workflow builds a self-contained artifact, loads the generated WASM
module and a resolved tax graph on every relevant pull request, and deploys only
from `main`. GitHub Pages must be enabled for the repository with **GitHub
Actions** selected as its publishing source; the workflow token cannot enable a
Pages site itself.
