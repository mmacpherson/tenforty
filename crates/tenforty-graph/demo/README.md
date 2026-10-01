# WASM tax calculator

The browser demo runs the Rust graph runtime entirely in WebAssembly; tax data
does not leave the browser. The deployed site is
[mmacpherson.github.io/tenforty](https://mmacpherson.github.io/tenforty/).

> **Current limitation:** The WASM engine and deployment work, but the calculator
> UI still uses obsolete graph node names. It stops during initialization at the
> first stale input; the dimmed zero figures are untouched placeholders. Later
> stale output and gradient lookups would also be swallowed as zeros. Repairing
> the bindings and adding a browser-level contract are tracked by `tenforty-ox4.4.3`.

The checked-in [browser calculator contract](../../../docs/browser-calculator-contract.md)
defines the supported years, jurisdictions, public inputs and outputs, graph
provenance, error behavior, and current limitations. The Pages smoke test
validates that contract against both resolved graphs and pinned Python scenarios.

Build and serve it locally with:

```console
make wasm-serve
```

The Pages workflow builds a self-contained artifact and validates the generated
WASM tax engine against the resolved tax graphs on every relevant pull request.
It deploys only from `main`; the browser UI itself will be covered when the
bindings are repaired. GitHub Pages must be enabled for the repository with
**GitHub Actions** selected as its publishing source; the workflow token cannot
enable a Pages site itself.
