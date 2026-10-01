export class BrowserContractError extends Error {
  constructor(message) {
    super(message);
    this.name = "BrowserContractError";
  }
}

function requireEntry(collection, key, description) {
  const value = collection[key];
  if (value === undefined) {
    throw new BrowserContractError(`Unsupported ${description}: ${key}`);
  }
  return value;
}

function requireYear(contract, year) {
  if (!contract.supported_years.includes(year)) {
    throw new BrowserContractError(`Unsupported tax year: ${year}`);
  }
  return String(year);
}

function validateInputValue(name, specification, value) {
  if (specification.type === "boolean") {
    if (typeof value !== "boolean") {
      throw new BrowserContractError(`${name} must be true or false`);
    }
    return value;
  }
  if (specification.type === "choice") {
    if (!specification.choices.includes(value)) {
      throw new BrowserContractError(
        `${name} must be one of: ${specification.choices.join(", ")}`,
      );
    }
    return value;
  }
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new BrowserContractError(`${name} must be a finite number`);
  }
  if (!specification.allows_negative && value < 0) {
    throw new BrowserContractError(`${name} cannot be negative`);
  }
  return value;
}

function normalizeInputs(contract, suppliedInputs) {
  for (const name of Object.keys(suppliedInputs)) {
    requireEntry(contract.inputs, name, "browser input");
  }

  const values = {};
  for (const [name, specification] of Object.entries(contract.inputs)) {
    const supplied = Object.hasOwn(suppliedInputs, name)
      ? suppliedInputs[name]
      : specification.default;
    values[name] = validateInputValue(name, specification, supplied);
  }

  for (const normalization of contract.normalizations) {
    if (normalization.rule !== "at_least") {
      throw new BrowserContractError(
        `Unknown input normalization: ${normalization.rule}`,
      );
    }
    values[normalization.target] = Math.max(
      values[normalization.target],
      values[normalization.source],
    );
  }
  return values;
}

function setNode(runtime, node, value, context) {
  try {
    runtime.set(node, Number(value));
  } catch (error) {
    throw new BrowserContractError(
      `Browser contract input node is missing (${context}): ${node}; ${error}`,
    );
  }
}

function evaluateNode(runtime, node, context) {
  let value;
  try {
    value = runtime.eval(node);
  } catch (error) {
    throw new BrowserContractError(
      `Browser contract output node is missing (${context}): ${node}; ${error}`,
    );
  }
  if (!Number.isFinite(value)) {
    throw new BrowserContractError(
      `Browser contract output is not finite (${context}): ${node}`,
    );
  }
  return value;
}

function evaluateGradientVector(runtime, outputs, inputGroups, context) {
  const inputNodes = inputGroups.flat();
  const groupLengths = inputGroups.map((nodes) => nodes.length);
  let values;
  try {
    values = Array.from(
      runtime.gradientVector(outputs, inputNodes, groupLengths),
    );
  } catch (error) {
    throw new BrowserContractError(
      `Browser contract gradient failed (${context}): ${error}`,
    );
  }
  if (
    values.length !== inputGroups.length ||
    values.some((value) => !Number.isFinite(value))
  ) {
    throw new BrowserContractError(
      `Browser contract gradient is invalid (${context})`,
    );
  }
  return values;
}

function calculateDerivedOutput(specification, values) {
  if (specification.formula === "subtract") {
    return specification.values.reduce(
      (value, name) => value - values[name],
      values[specification.from],
    );
  }
  if (specification.formula === "sum") {
    return specification.values.reduce(
      (total, name) => total + values[name],
      0,
    );
  }
  if (specification.formula === "ratio_percent") {
    const denominator = values[specification.denominator];
    return denominator > 0
      ? (values[specification.numerator] / denominator) * 100
      : 0;
  }
  throw new BrowserContractError(
    `Unknown output formula: ${specification.formula}`,
  );
}

export async function loadBrowserContract(url = "browser_contract.json") {
  const response = await fetch(url);
  if (!response.ok) {
    throw new BrowserContractError(
      `Failed to load browser contract: ${response.status}`,
    );
  }
  return response.json();
}

export function validateBrowserContract(contract, graph, year) {
  if (
    contract.schema_version !== 1 ||
    contract.contract_id !== "tenforty-browser-calculator"
  ) {
    throw new BrowserContractError(
      `Unsupported browser contract schema: ${contract.schema_version}`,
    );
  }
  const yearKey = requireYear(contract, year);
  const graphMetadata = JSON.parse(graph.toJson()).meta;
  if (graphMetadata.year !== year) {
    throw new BrowserContractError(
      `Browser contract expected a ${year} graph, received ${graphMetadata.year}`,
    );
  }
  const graphInputs = new Set(graph.inputNames());
  const graphOutputs = new Set(graph.outputNames());
  const missingInputs = [];
  const missingOutputs = [];

  for (const specification of Object.values(contract.inputs)) {
    for (const node of specification.federal_nodes) {
      if (!graphInputs.has(node)) missingInputs.push(node);
    }
  }
  for (const specification of Object.values(contract.derived_inputs)) {
    for (const node of specification.federal_nodes) {
      if (!graphInputs.has(node)) missingInputs.push(node);
    }
  }
  for (const specification of Object.values(contract.outputs)) {
    if (!graphOutputs.has(specification.node)) {
      missingOutputs.push(specification.node);
    }
  }
  for (const jurisdiction of Object.values(contract.jurisdictions)) {
    for (const nodes of Object.values(jurisdiction.input_nodes[yearKey])) {
      for (const node of nodes) {
        if (!graphInputs.has(node)) missingInputs.push(node);
      }
    }
    for (const node of Object.values(jurisdiction.output_nodes[yearKey])) {
      if (!graphOutputs.has(node)) missingOutputs.push(node);
    }
  }

  if (missingInputs.length || missingOutputs.length) {
    const details = [
      ...new Set(missingInputs.map((node) => `missing input ${node}`)),
      ...new Set(missingOutputs.map((node) => `missing output ${node}`)),
    ].join(", ");
    throw new BrowserContractError(
      `Browser contract does not match the ${year} graph: ${details}`,
    );
  }
}

export class BrowserTaxRuntime {
  constructor(graphlib, graph, contract, { year, jurisdiction, filingStatus }) {
    const yearKey = requireYear(contract, year);
    const jurisdictionSpec = requireEntry(
      contract.jurisdictions,
      jurisdiction,
      "jurisdiction",
    );
    requireEntry(contract.filing_statuses, filingStatus, "filing status");
    validateBrowserContract(contract, graph, year);

    this.contract = contract;
    this.yearKey = yearKey;
    this.jurisdiction = jurisdiction;
    this.jurisdictionSpec = jurisdictionSpec;
    this.filingStatus = filingStatus;
    this.runtime = new graphlib.Runtime(
      graph,
      graphlib.FilingStatus.fromString(filingStatus),
    );
  }

  setInputs(suppliedInputs) {
    const values = normalizeInputs(this.contract, suppliedInputs);
    this.values = values;
    const unsupported = this.jurisdictionSpec.unsupported_inputs[this.yearKey];
    for (const name of unsupported) {
      if (values[name] !== 0 && values[name] !== false) {
        throw new BrowserContractError(
          `${name} is not supported for ${this.jurisdiction}/${this.yearKey}`,
        );
      }
    }

    for (const [name, specification] of Object.entries(this.contract.inputs)) {
      const graphValue = specification.encoding
        ? specification.encoding[values[name]]
        : values[name];
      for (const node of specification.federal_nodes) {
        setNode(this.runtime, node, graphValue, `federal ${name}`);
      }
    }
    for (const [name, nodes] of Object.entries(
      this.jurisdictionSpec.input_nodes[this.yearKey],
    )) {
      for (const node of nodes) {
        setNode(
          this.runtime,
          node,
          values[name],
          `${this.jurisdiction} ${name}`,
        );
      }
    }

    for (const [name, specification] of Object.entries(
      this.contract.derived_inputs,
    )) {
      if (specification.rule !== "w2_when_self_employed_non_joint") {
        throw new BrowserContractError(
          `Unknown derived input rule: ${specification.rule}`,
        );
      }
      const value =
        values.self_employment_income !== 0 &&
        this.filingStatus !== "married_joint"
          ? values.w2_income
          : 0;
      for (const node of specification.federal_nodes) {
        setNode(this.runtime, node, value, `derived ${name}`);
      }
    }
  }

  gradientVectors() {
    if (!this.values) {
      throw new BrowserContractError(
        "Set browser inputs before requesting gradients",
      );
    }
    const inputNames = Object.entries(this.contract.inputs)
      .filter(([, specification]) => specification.type === "money")
      .map(([name]) => name);
    const stateInputNodes = this.jurisdictionSpec.input_nodes[this.yearKey];
    const derivedWageNodes =
      this.values.self_employment_income !== 0 &&
      this.filingStatus !== "married_joint"
        ? this.contract.derived_inputs.schedule_se_ss_wages.federal_nodes
        : [];
    const inputGroups = inputNames.map((name) => [
      ...this.contract.inputs[name].federal_nodes,
      ...(stateInputNodes[name] ?? []),
      ...(name === "w2_income" ? derivedWageNodes : []),
    ]);
    const federalOutput = this.contract.outputs.federal_total_tax.node;
    const stateOutput =
      this.jurisdictionSpec.output_nodes[this.yearKey].state_total_tax;
    const federalValues = evaluateGradientVector(
      this.runtime,
      [federalOutput],
      inputGroups,
      "federal total tax",
    );
    const stateValues = stateOutput
      ? evaluateGradientVector(
          this.runtime,
          [stateOutput],
          inputGroups,
          `${this.jurisdiction} total tax`,
        )
      : federalValues.map(() => 0);

    return Object.fromEntries(
      inputNames.map((name, index) => [
        name,
        {
          federal: federalValues[index],
          state: stateValues[index],
          total: federalValues[index] + stateValues[index],
        },
      ]),
    );
  }

  evaluate() {
    const values = {};
    for (const [name, specification] of Object.entries(this.contract.outputs)) {
      values[name] = evaluateNode(this.runtime, specification.node, name);
    }

    const stateOutputs = this.jurisdictionSpec.output_nodes[this.yearKey];
    for (const name of Object.keys(this.contract.state_outputs)) {
      const node = stateOutputs[name];
      values[name] = node
        ? evaluateNode(this.runtime, node, `${this.jurisdiction} ${name}`)
        : name === "state_total_tax"
          ? 0
          : null;
    }

    for (const specification of this.contract.derived_outputs) {
      values[specification.name] = calculateDerivedOutput(
        specification,
        values,
      );
    }
    return values;
  }
}
