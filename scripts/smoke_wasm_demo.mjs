import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const siteDirectory = path.resolve(process.argv[2] ?? "target/pages");
const modulePath = path.join(siteDirectory, "pkg", "graphlib.js");
const wasmPath = path.join(siteDirectory, "pkg", "graphlib_bg.wasm");
const contractModulePath = path.join(siteDirectory, "browser_contract.js");
const calculatorModulePath = path.join(siteDirectory, "calculator.js");
const contractPath = path.join(siteDirectory, "browser_contract.json");
const graphlib = await import(pathToFileURL(modulePath));
const { BrowserContractError, validateBrowserContract } =
  await import(pathToFileURL(contractModulePath));
const {
  INPUT_GROUPS,
  SENSITIVITY_INPUTS,
  analyzeScenario,
  calculateScenario,
  parseScenario,
  scenarioFromParityCase,
  serializeScenario,
  sweepScenario,
} = await import(pathToFileURL(calculatorModulePath));
await graphlib.default({ module_or_path: await readFile(wasmPath) });
const contract = JSON.parse(await readFile(contractPath, "utf8"));
const visibleInputs = Object.values(INPUT_GROUPS).flat();

assert.deepEqual(
  new Set(visibleInputs),
  new Set(Object.keys(contract.inputs)),
  "calculator must expose every contracted browser input",
);
assert.equal(
  visibleInputs.length,
  new Set(visibleInputs).size,
  "calculator must expose each browser input exactly once",
);
assert.equal(
  SENSITIVITY_INPUTS.qualified_dividends.reclassification,
  true,
  "qualified-dividend sensitivity must be labeled as a reclassification",
);
assert.match(
  SENSITIVITY_INPUTS.qualified_dividends.action,
  /^Reclassify /,
  "qualified-dividend copy must not describe a new dollar of income",
);

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
  const scenario = scenarioFromParityCase(contract, testCase);
  const sharedScenario = parseScenario(
    contract,
    `#${serializeScenario(contract, scenario)}`,
  );
  assert.deepEqual(
    sharedScenario,
    scenario,
    `${testCase.id}: share URL changed the scenario`,
  );
  const actual = calculateScenario(
    graphlib,
    graphs.get(testCase.year),
    contract,
    sharedScenario,
  );

  for (const [name, expected] of Object.entries(testCase.expected)) {
    assert.ok(
      Math.abs(actual[name] - expected) <= 1e-6,
      `${testCase.id}/${name}: expected ${expected}, received ${actual[name]}`,
    );
  }
}

for (const testCase of contract.gradient_cases) {
  const scenario = scenarioFromParityCase(contract, testCase);
  const analysis = analyzeScenario(
    graphlib,
    graphs.get(testCase.year),
    contract,
    scenario,
  );
  for (const [inputName, expectedParts] of Object.entries(testCase.expected)) {
    for (const [part, expected] of Object.entries(expectedParts)) {
      const actual = analysis.gradients[inputName][part];
      assert.ok(
        Math.abs(actual - expected) <= 1e-6,
        `${testCase.id}/${inputName}/${part}: expected ${expected}, received ${actual}`,
      );
    }
  }
}

const ordinaryScenario = scenarioFromParityCase(
  contract,
  contract.gradient_cases.find(({ id }) => id === "ordinary-bracket"),
);
const ordinaryCurve = sweepScenario(
  graphlib,
  graphs.get(ordinaryScenario.year),
  contract,
  ordinaryScenario,
  "w2_income",
);
assert.equal(ordinaryCurve.length, 57);
assert.equal(ordinaryCurve[0].input, 0);
assert.equal(ordinaryCurve.at(-1).input, 200000);
assert.ok(
  ordinaryCurve.every(
    (point, index) => index === 0 || point.total >= ordinaryCurve[index - 1].total,
  ),
  "ordinary wage curve must remain monotone",
);

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
  `WASM browser contract passed: ${contract.parity_cases.length} value cases, ${contract.gradient_cases.length} gradient regions, ${contract.supported_years.length} tax years, and ${Object.keys(contract.jurisdictions).length} jurisdictions`,
);
