# WASM tax calculator

The browser calculator runs the Rust graph runtime entirely in WebAssembly; tax
data does not leave the browser. The deployed site is
[mmacpherson.github.io/tenforty](https://mmacpherson.github.io/tenforty/).

The checked-in [browser calculator contract](../../../docs/browser-calculator-contract.md)
defines the supported years, jurisdictions, public inputs and outputs, graph
provenance, error behavior, and current limitations. The UI renders public
concepts from that contract rather than graph-node names. Share links serialize
the same public scenario passed to the calculator boundary in the URL fragment,
which browsers do not send to the hosting server.

Build and serve it locally with:

```console
make wasm-serve
```

The Pages workflow builds a self-contained artifact and validates the generated
WASM tax engine, browser calculation boundary, share-link round trips, resolved
graphs, and pinned Python scenarios on every relevant pull request. It deploys
only from `main`. GitHub Pages must be enabled for the repository with **GitHub
Actions** selected as its publishing source; the workflow token cannot enable a
Pages site itself.
