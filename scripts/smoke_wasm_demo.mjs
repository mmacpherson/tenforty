import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const siteDirectory = path.resolve(process.argv[2] ?? "target/pages");
const modulePath = path.join(siteDirectory, "pkg", "graphlib.js");
const wasmPath = path.join(siteDirectory, "pkg", "graphlib_bg.wasm");
const contractModulePath = path.join(siteDirectory, "browser_contract.js");
const contractPath = path.join(siteDirectory, "browser_contract.json");
const graphlib = await import(pathToFileURL(modulePath));
const { BrowserContractError, BrowserTaxRuntime, validateBrowserContract } =
  await import(pathToFileURL(contractModulePath));
await graphlib.default({ module_or_path: await readFile(wasmPath) });
const contract = JSON.parse(await readFile(contractPath, "utf8"));

const graphs = new Map();

for (const year of contract.supported_years) {
  const graphPath = path.join(
    siteDirectory,
    "forms",
    `us_tax_graph_${year}.json`,
  );
  const graphJson = await readFile(graphPath, "utf8");
  const graphHash = createHash("sha256").update(graphJson).digest("hex");
  assert.equal(graphHash, contract.graph.metadata[String(year)].sha256);
  const graph = graphlib.Graph.fromJson(graphJson);
  validateBrowserContract(contract, graph, year);
  graphs.set(year, graph);
}

for (const testCase of contract.parity_cases) {
  const calculator = new BrowserTaxRuntime(
    graphlib,
    graphs.get(testCase.year),
    contract,
    {
      year: testCase.year,
      jurisdiction: testCase.jurisdiction,
      filingStatus: testCase.filing_status,
    },
  );
  calculator.setInputs(testCase.inputs);
  const actual = calculator.evaluate();

  for (const [name, expected] of Object.entries(testCase.expected)) {
    assert.ok(
      Math.abs(actual[name] - expected) <= 1e-6,
      `${testCase.id}/${name}: expected ${expected}, received ${actual[name]}`,
    );
  }
}

const staleContract = structuredClone(contract);
staleContract.inputs.w2_income.federal_nodes = ["us_1040_wages"];
assert.throws(
  () => validateBrowserContract(staleContract, graphs.get(2024), 2024),
  BrowserContractError,
);
assert.throws(
  () => validateBrowserContract(contract, graphs.get(2024), 2025),
  BrowserContractError,
);

console.log(
  `WASM browser contract passed: ${contract.parity_cases.length} parity cases across ${contract.supported_years.length} tax years and ${Object.keys(contract.jurisdictions).length} jurisdictions`,
);
