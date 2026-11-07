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
const {
  BrowserContractError,
  BrowserTaxRuntime,
  isValidatedJurisdiction,
  validateBrowserContract,
} = await import(pathToFileURL(contractModulePath));
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
  const validated = isValidatedJurisdiction(
    contract,
    testCase.jurisdiction,
    testCase.year,
  );
  const shareUrl = `#${serializeScenario(contract, scenario)}`;
  let computedScenario = scenario;
  if (validated) {
    computedScenario = parseScenario(contract, shareUrl);
    assert.deepEqual(
      computedScenario,
      scenario,
      `${testCase.id}: share URL changed the scenario`,
    );
  } else {
    assert.throws(
      () => parseScenario(contract, shareUrl),
      /not yet validated/,
      `${testCase.id}: a shared link to an unvalidated jurisdiction must be refused`,
    );
    assert.throws(
      () => calculateScenario(graphlib, graphs.get(testCase.year), contract, scenario),
      /not yet validated/,
      `${testCase.id}: the runtime must refuse an unvalidated jurisdiction`,
    );
  }
  const actual = calculateScenario(
    graphlib,
    graphs.get(testCase.year),
    contract,
    computedScenario,
    { allowUnvalidated: !validated },
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
    {
      allowUnvalidated: !isValidatedJurisdiction(
        contract,
        testCase.jurisdiction,
        testCase.year,
      ),
      includeStateSlopes: true,
    },
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

const offered = Object.keys(contract.jurisdictions).filter((code) =>
  contract.supported_years.every((year) =>
    isValidatedJurisdiction(contract, code, year),
  ),
);
assert.deepEqual(
  new Set(offered),
  new Set(["US", "IL", "MS", "OR", "AK", "FL", "NV", "SD", "TN", "TX", "WA", "WY"]),
  "the calculator offers exactly the validated jurisdictions",
);

const illinois = {
  year: 2025,
  jurisdiction: "IL",
  filingStatus: "single",
  inputs: { ...scenarioFromParityCase(contract, contract.gradient_cases[0]).inputs },
};
illinois.inputs.w2_income = 85000;
const illinoisAnalysis = analyzeScenario(
  graphlib,
  graphs.get(2025),
  contract,
  illinois,
);
assert.equal(contract.state_slopes.available, false);
assert.equal(illinoisAnalysis.gradients.w2_income.state, null);
assert.equal(illinoisAnalysis.gradients.w2_income.total, null);
assert.ok(
  illinoisAnalysis.gradients.w2_income.federal > 0,
  "federal slopes stay available on a state return",
);
assert.ok(illinoisAnalysis.results.state_total_tax > 0);

const mississippi = { ...illinois, jurisdiction: "MS" };
assert.ok(
  calculateScenario(graphlib, graphs.get(2025), contract, mississippi)
    .state_total_tax > 0,
  "a Single MS return computes",
);
for (const filingStatus of [
  "married_joint",
  "qualifying_widow",
  "head_of_household",
]) {
  assert.throws(
    () =>
      calculateScenario(graphlib, graphs.get(2025), contract, {
        ...mississippi,
        filingStatus,
      }),
    /browser calculator cannot compute a MS 2025/,
    `MS ${filingStatus} needs facts the browser cannot supply`,
  );
}
for (const name of ["schedule_1_income", "itemized_deductions"]) {
  assert.throws(
    () =>
      calculateScenario(graphlib, graphs.get(2025), contract, {
        ...mississippi,
        inputs: { ...mississippi.inputs, [name]: 1000 },
      }),
    new RegExp(`${name} is not supported for MS/2025`),
    `a nonzero ${name} is refused for MS, as in Python`,
  );
}

for (const value of ["true", 1, null]) {
  assert.throws(
    () =>
      new BrowserTaxRuntime(graphlib, graphs.get(2025), contract, {
        year: 2025,
        jurisdiction: "CA",
        filingStatus: "single",
        allowUnvalidated: value,
      }),
    /allowUnvalidated must be true or false/,
    `allowUnvalidated=${JSON.stringify(value)} must not acknowledge`,
  );
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
  `WASM browser contract passed: ${contract.parity_cases.length} value cases, ${contract.gradient_cases.length} gradient regions, ${contract.supported_years.length} tax years, and ${offered.length} of ${Object.keys(contract.jurisdictions).length} jurisdictions offered`,
);
