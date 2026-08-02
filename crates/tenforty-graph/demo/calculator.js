import { BrowserContractError, BrowserTaxRuntime } from "./browser_contract.js";

export const INPUT_GROUPS = {
  "income-fields": [
    "w2_income",
    "taxable_interest",
    "ordinary_dividends",
    "qualified_dividends",
  ],
  "other-income-fields": [
    "short_term_capital_gains",
    "long_term_capital_gains",
    "self_employment_income",
    "rental_income",
    "schedule_1_income",
  ],
  "deduction-fields": ["standard_or_itemized", "itemized_deductions"],
  "advanced-fields": [
    "incentive_stock_option_gains",
    "qbi_w2_wages",
    "qbi_ubia",
    "qbi_is_sstb",
  ],
};

export const SENSITIVITY_INPUTS = {
  w2_income: {
    action: "Earn $1 more in wages",
    shortLabel: "Wages",
    curveMaximum: 200000,
  },
  taxable_interest: {
    action: "Earn $1 more of taxable interest",
    shortLabel: "Interest",
    curveMaximum: 100000,
  },
  ordinary_dividends: {
    action: "Receive $1 more of ordinary dividends",
    shortLabel: "Ordinary dividends",
  },
  qualified_dividends: {
    action: "Reclassify $1 of ordinary dividends as qualified",
    shortLabel: "Qualified share",
    reclassification: true,
  },
  short_term_capital_gains: {
    action: "Realize $1 more of short-term gain",
    shortLabel: "Short-term gain",
  },
  long_term_capital_gains: {
    action: "Realize $1 more of long-term gain",
    shortLabel: "Long-term gain",
    curveMaximum: 150000,
  },
  self_employment_income: {
    action: "Earn $1 more from self-employment",
    shortLabel: "Self-employment",
    curveMaximum: 200000,
  },
  rental_income: {
    action: "Earn $1 more of rental income",
    shortLabel: "Rental income",
  },
  schedule_1_income: {
    action: "Earn $1 more of other income",
    shortLabel: "Other income",
  },
  itemized_deductions: {
    action: "Claim $1 more of itemized deductions",
    shortLabel: "Itemized deductions",
  },
  incentive_stock_option_gains: {
    action: "Exercise $1 more of ISO spread",
    shortLabel: "ISO spread",
    curveMaximum: 200000,
  },
  qbi_w2_wages: {
    action: "Add $1 of qualified-business W-2 wages",
    shortLabel: "QBI wages",
  },
  qbi_ubia: {
    action: "Add $1 of qualified-property UBIA",
    shortLabel: "QBI property",
  },
};

export const CURVE_INPUTS = [
  "w2_income",
  "long_term_capital_gains",
  "self_employment_income",
  "incentive_stock_option_gains",
];

function defaultInputs(contract) {
  return Object.fromEntries(
    Object.entries(contract.inputs).map(([name, specification]) => [
      name,
      specification.default,
    ]),
  );
}

export function createDefaultScenario(contract) {
  const inputs = defaultInputs(contract);
  inputs.w2_income = 75000;
  return {
    year: Math.max(...contract.supported_years),
    jurisdiction: "US",
    filingStatus: "single",
    inputs,
  };
}

export function scenarioFromParityCase(contract, testCase) {
  return {
    year: testCase.year,
    jurisdiction: testCase.jurisdiction,
    filingStatus: testCase.filing_status,
    inputs: { ...defaultInputs(contract), ...testCase.inputs },
  };
}

function parseBoolean(name, value) {
  if (value === "true" || value === "1") return true;
  if (value === "false" || value === "0") return false;
  throw new BrowserContractError(`${name} must be true or false`);
}

function parseInput(name, specification, value) {
  if (specification.type === "boolean") return parseBoolean(name, value);
  if (specification.type === "choice") {
    if (!specification.choices.includes(value)) {
      throw new BrowserContractError(
        `${name} must be one of: ${specification.choices.join(", ")}`,
      );
    }
    return value;
  }
  const number = Number(value);
  if (!Number.isFinite(number)) {
    throw new BrowserContractError(`${name} must be a finite number`);
  }
  return number;
}

export function parseScenario(contract, search, fallbackScenario = null) {
  const fallback = fallbackScenario ?? createDefaultScenario(contract);
  const parameters =
    search instanceof URLSearchParams
      ? search
      : new URLSearchParams(String(search).replace(/^[?#]/, ""));
  const year = Number(parameters.get("year") ?? fallback.year);
  const jurisdiction = parameters.get("jurisdiction") ?? fallback.jurisdiction;
  const filingStatus = parameters.get("filing_status") ?? fallback.filingStatus;

  if (!contract.supported_years.includes(year)) {
    throw new BrowserContractError(`Unsupported tax year: ${year}`);
  }
  if (!Object.hasOwn(contract.jurisdictions, jurisdiction)) {
    throw new BrowserContractError(`Unsupported jurisdiction: ${jurisdiction}`);
  }
  if (!Object.hasOwn(contract.filing_statuses, filingStatus)) {
    throw new BrowserContractError(
      `Unsupported filing status: ${filingStatus}`,
    );
  }

  const inputs = { ...defaultInputs(contract), ...fallback.inputs };
  for (const [name, specification] of Object.entries(contract.inputs)) {
    if (parameters.has(name)) {
      inputs[name] = parseInput(name, specification, parameters.get(name));
    }
  }

  return { year, jurisdiction, filingStatus, inputs };
}

function encodedInputValue(specification, value) {
  if (specification.type === "boolean") return value ? "true" : "false";
  return String(value);
}

export function serializeScenario(contract, scenario) {
  const parameters = new URLSearchParams();
  const defaults = createDefaultScenario(contract).inputs;
  parameters.set("year", String(scenario.year));
  parameters.set("jurisdiction", scenario.jurisdiction);
  parameters.set("filing_status", scenario.filingStatus);

  for (const [name, specification] of Object.entries(contract.inputs)) {
    const value = scenario.inputs[name];
    if (value !== defaults[name]) {
      parameters.set(name, encodedInputValue(specification, value));
    }
  }
  return parameters.toString();
}

export function calculateScenario(graphlib, graph, contract, scenario) {
  const calculator = new BrowserTaxRuntime(graphlib, graph, contract, {
    year: scenario.year,
    jurisdiction: scenario.jurisdiction,
    filingStatus: scenario.filingStatus,
  });
  calculator.setInputs(scenario.inputs);
  return calculator.evaluate();
}

export function analyzeScenario(graphlib, graph, contract, scenario) {
  const calculator = new BrowserTaxRuntime(graphlib, graph, contract, {
    year: scenario.year,
    jurisdiction: scenario.jurisdiction,
    filingStatus: scenario.filingStatus,
  });
  calculator.setInputs(scenario.inputs);
  return {
    results: calculator.evaluate(),
    gradients: calculator.gradientVectors(),
  };
}

export function sweepScenario(
  graphlib,
  graph,
  contract,
  scenario,
  inputName,
  pointCount = 57,
) {
  const metadata = SENSITIVITY_INPUTS[inputName];
  if (!metadata?.curveMaximum) {
    throw new BrowserContractError(
      `${inputName} is not available as a browser tax-curve axis`,
    );
  }
  const current = scenario.inputs[inputName];
  const minimum = Math.min(0, current * 1.35);
  const maximum = Math.max(metadata.curveMaximum, current * 1.35);
  const calculator = new BrowserTaxRuntime(graphlib, graph, contract, {
    year: scenario.year,
    jurisdiction: scenario.jurisdiction,
    filingStatus: scenario.filingStatus,
  });

  return Array.from({ length: pointCount }, (_, index) => {
    const value = minimum + ((maximum - minimum) * index) / (pointCount - 1);
    calculator.setInputs({ ...scenario.inputs, [inputName]: value });
    const results = calculator.evaluate();
    return {
      input: value,
      federal: results.federal_total_tax,
      state: results.state_total_tax,
      total: results.total_tax,
    };
  });
}
