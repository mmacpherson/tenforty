import init, * as graphlib from "./pkg/graphlib.js";
import {
  BrowserContractError,
  loadBrowserContract,
} from "./browser_contract.js";
import {
  INPUT_GROUPS,
  calculateScenario,
  createDefaultScenario,
  parseScenario,
  serializeScenario,
} from "./calculator.js";

const CURRENCY_FORMATTER = new Intl.NumberFormat("en-US", {
  style: "currency",
  currency: "USD",
  maximumFractionDigits: 0,
});

let contract;
let scenario;
let calculationSequence = 0;
let toastTimer;
const graphCache = new Map();

function byId(id) {
  return document.getElementById(id);
}

function formatCurrency(value) {
  return CURRENCY_FORMATTER.format(Math.abs(value) < 0.005 ? 0 : value);
}

function formatPercent(value) {
  return `${value.toFixed(1)}%`;
}

function createTextElement(tagName, className, text) {
  const element = document.createElement(tagName);
  element.className = className;
  element.textContent = text;
  return element;
}

function createMoneyField(name, specification) {
  const field = document.createElement("div");
  field.className = "input-field money-field";
  field.dataset.inputField = name;

  const inputId = `input-${name}`;
  const descriptionId = `${inputId}-description`;
  const label = createTextElement("label", "", specification.label);
  label.htmlFor = inputId;

  const wrapper = document.createElement("div");
  wrapper.className = "money-input-wrap";
  wrapper.appendChild(createTextElement("span", "money-prefix", "$"));

  const input = document.createElement("input");
  input.type = "number";
  input.id = inputId;
  input.name = name;
  input.dataset.browserInput = name;
  input.step = "100";
  input.inputMode = "decimal";
  input.setAttribute("aria-describedby", descriptionId);
  if (!specification.allows_negative) input.min = "0";
  wrapper.appendChild(input);

  const description = createTextElement(
    "p",
    "field-description",
    specification.description,
  );
  description.id = descriptionId;

  field.append(label, wrapper, description);
  return field;
}

function createChoiceField(name, specification) {
  const field = document.createElement("div");
  field.className = "input-field choice-field";
  field.dataset.inputField = name;

  const inputId = `input-${name}`;
  const descriptionId = `${inputId}-description`;
  const label = createTextElement("label", "", specification.label);
  label.htmlFor = inputId;

  const select = document.createElement("select");
  select.id = inputId;
  select.name = name;
  select.dataset.browserInput = name;
  select.setAttribute("aria-describedby", descriptionId);
  for (const choice of specification.choices) {
    const option = document.createElement("option");
    option.value = choice;
    option.textContent = choice === "Standard" ? "Automatic (larger)" : choice;
    select.appendChild(option);
  }

  const description = createTextElement(
    "p",
    "field-description",
    specification.description,
  );
  description.id = descriptionId;

  field.append(label, select, description);
  return field;
}

function createBooleanField(name, specification) {
  const field = document.createElement("div");
  field.className = "input-field boolean-field";
  field.dataset.inputField = name;

  const inputId = `input-${name}`;
  const descriptionId = `${inputId}-description`;
  const copy = document.createElement("div");
  const label = createTextElement("label", "", specification.label);
  label.htmlFor = inputId;
  const description = createTextElement(
    "p",
    "field-description",
    specification.description,
  );
  description.id = descriptionId;
  copy.append(label, description);

  const switchLabel = document.createElement("label");
  switchLabel.className = "switch";
  switchLabel.setAttribute("aria-label", specification.label);
  const input = document.createElement("input");
  input.type = "checkbox";
  input.id = inputId;
  input.name = name;
  input.dataset.browserInput = name;
  input.setAttribute("aria-describedby", descriptionId);
  const track = document.createElement("span");
  track.className = "switch-track";
  switchLabel.append(input, track);

  field.append(copy, switchLabel);
  return field;
}

function createInputField(name) {
  const specification = contract.inputs[name];
  if (specification.type === "boolean") {
    return createBooleanField(name, specification);
  }
  if (specification.type === "choice") {
    return createChoiceField(name, specification);
  }
  return createMoneyField(name, specification);
}

function populateInterface() {
  const yearSelect = byId("tax-year");
  for (const year of [...contract.supported_years].reverse()) {
    const option = document.createElement("option");
    option.value = String(year);
    option.textContent = String(year);
    yearSelect.appendChild(option);
  }

  const filingStatusSelect = byId("filing-status");
  for (const [value, label] of Object.entries(contract.filing_statuses)) {
    const option = document.createElement("option");
    option.value = value;
    option.textContent = label;
    filingStatusSelect.appendChild(option);
  }

  const jurisdictionSelect = byId("jurisdiction");
  const jurisdictions = Object.entries(contract.jurisdictions).sort(
    ([leftCode, left], [rightCode, right]) => {
      if (leftCode === "US") return -1;
      if (rightCode === "US") return 1;
      return left.name.localeCompare(right.name);
    },
  );
  for (const [code, specification] of jurisdictions) {
    const option = document.createElement("option");
    option.value = code;
    option.textContent = specification.name;
    jurisdictionSelect.appendChild(option);
  }

  for (const [containerId, inputNames] of Object.entries(INPUT_GROUPS)) {
    const container = byId(containerId);
    for (const name of inputNames)
      container.appendChild(createInputField(name));
  }

  const limitations = byId("limitations-list");
  for (const limitation of contract.limitations) {
    limitations.appendChild(createTextElement("p", "", limitation.summary));
  }
}

function renderScenario(nextScenario) {
  byId("tax-year").value = String(nextScenario.year);
  byId("filing-status").value = nextScenario.filingStatus;
  byId("jurisdiction").value = nextScenario.jurisdiction;

  for (const [name, specification] of Object.entries(contract.inputs)) {
    const input = byId(`input-${name}`);
    if (specification.type === "boolean") {
      input.checked = nextScenario.inputs[name];
    } else {
      input.value = String(nextScenario.inputs[name]);
    }
  }
  updateUnsupportedInputs(nextScenario.year, nextScenario.jurisdiction);
  updateExpandableCards(nextScenario);
}

function groupHasValues(nextScenario, groupName) {
  return INPUT_GROUPS[groupName].some(
    (name) => nextScenario.inputs[name] !== contract.inputs[name].default,
  );
}

function updateExpandableCards(nextScenario) {
  const narrowScreen = window.matchMedia("(max-width: 680px)").matches;
  byId("other-income-card").open =
    !narrowScreen || groupHasValues(nextScenario, "other-income-fields");
  byId("advanced-card").open = groupHasValues(nextScenario, "advanced-fields");
}

function readScenario() {
  const inputs = {};
  for (const [name, specification] of Object.entries(contract.inputs)) {
    const input = byId(`input-${name}`);
    if (specification.type === "boolean") {
      inputs[name] = input.checked;
    } else if (specification.type === "choice") {
      inputs[name] = input.value;
    } else {
      inputs[name] = Number.isFinite(input.valueAsNumber)
        ? input.valueAsNumber
        : 0;
    }
  }
  return {
    year: Number(byId("tax-year").value),
    jurisdiction: byId("jurisdiction").value,
    filingStatus: byId("filing-status").value,
    inputs,
  };
}

function updateUnsupportedInputs(year, jurisdiction) {
  document.querySelectorAll("[data-input-field]").forEach((field) => {
    field.classList.remove("unsupported");
    field.querySelector(".unsupported-message")?.remove();
  });

  const unsupported =
    contract.jurisdictions[jurisdiction].unsupported_inputs[String(year)];
  for (const name of unsupported) {
    const field = document.querySelector(`[data-input-field="${name}"]`);
    field.classList.add("unsupported");
    field.appendChild(
      createTextElement(
        "p",
        "unsupported-message",
        `${contract.inputs[name].label} is not supported for ${contract.jurisdictions[jurisdiction].name} in ${year}. Leave it at zero to calculate.`,
      ),
    );
  }
}

async function loadGraph(year) {
  if (!graphCache.has(year)) {
    const graphPromise = (async () => {
      const path = contract.graph.path_template.replace("{year}", String(year));
      const response = await fetch(path);
      if (!response.ok) {
        throw new BrowserContractError(
          `Failed to load the ${year} tax graph (${response.status})`,
        );
      }
      return graphlib.Graph.fromJson(await response.text());
    })();
    graphCache.set(year, graphPromise);
  }
  return graphCache.get(year);
}

function setText(id, value) {
  byId(id).textContent = value;
}

function setOptionalMoney(rowId, valueId, value) {
  const visible = value !== null && Math.abs(value) >= 0.005;
  byId(rowId).hidden = !visible;
  if (visible) setText(valueId, formatCurrency(value));
}

function selectedJurisdiction() {
  return contract.jurisdictions[scenario.jurisdiction];
}

function renderResults(results, elapsedMilliseconds) {
  const jurisdiction = selectedJurisdiction();
  const federalOnly = scenario.jurisdiction === "US";

  setText("result-year", String(scenario.year));
  setText("result-state-context", federalOnly ? "" : ` · ${jurisdiction.name}`);
  setText("total-tax", formatCurrency(results.total_tax));
  setText("federal-tax", formatCurrency(results.federal_total_tax));
  setText(
    "state-tax-label",
    federalOnly ? "No state selected" : jurisdiction.name,
  );
  setText(
    "state-tax",
    federalOnly ? "—" : formatCurrency(results.state_total_tax),
  );
  setText("effective-rate", formatPercent(results.effective_tax_rate));
  setText(
    "federal-effective-rate",
    formatPercent(results.federal_effective_tax_rate),
  );
  setText(
    "state-effective-rate",
    federalOnly ? "—" : formatPercent(results.state_effective_tax_rate),
  );

  setText("federal-agi", formatCurrency(results.federal_adjusted_gross_income));
  setText(
    "federal-taxable-income",
    formatCurrency(results.federal_taxable_income),
  );
  setOptionalMoney(
    "qbi-row",
    "federal-qbi-deduction",
    results.federal_qbi_deduction,
  );

  const stateAgiVisible =
    !federalOnly && results.state_adjusted_gross_income !== null;
  byId("state-agi-row").hidden = !stateAgiVisible;
  if (stateAgiVisible) {
    setText("state-agi-label", `${jurisdiction.name} adjusted gross income`);
    setText("state-agi", formatCurrency(results.state_adjusted_gross_income));
  }
  const stateTaxableVisible =
    !federalOnly && results.state_taxable_income !== null;
  byId("state-taxable-income-row").hidden = !stateTaxableVisible;
  if (stateTaxableVisible) {
    setText(
      "state-taxable-income-label",
      `${jurisdiction.name} taxable income`,
    );
    setText(
      "state-taxable-income",
      formatCurrency(results.state_taxable_income),
    );
  }

  setText("federal-income-tax", formatCurrency(results.federal_income_tax));
  setOptionalMoney("se-tax-row", "federal-se-tax", results.federal_se_tax);
  setOptionalMoney("niit-row", "federal-niit", results.federal_niit);
  setOptionalMoney(
    "medicare-tax-row",
    "federal-medicare-tax",
    results.federal_additional_medicare_tax,
  );
  setOptionalMoney("amt-row", "federal-amt", results.federal_amt);

  byId("calculation-status").classList.remove("calculating");
  setText(
    "calculation-status",
    `Calculated locally · ${Math.max(1, Math.round(elapsedMilliseconds))} ms`,
  );
  byId("error-banner").hidden = true;
  byId("results-heading").closest(".results-hero").classList.remove("is-stale");
  byId("results-heading")
    .closest(".results-hero")
    .setAttribute("aria-busy", "false");
}

function clearResults() {
  for (const id of [
    "total-tax",
    "federal-tax",
    "state-tax",
    "effective-rate",
    "federal-effective-rate",
    "state-effective-rate",
    "federal-agi",
    "federal-taxable-income",
    "federal-income-tax",
  ]) {
    setText(id, "—");
  }
  document.querySelectorAll(".optional-result").forEach((row) => {
    row.hidden = true;
  });
  const hero = byId("results-heading").closest(".results-hero");
  hero.classList.add("is-stale");
  hero.setAttribute("aria-busy", "false");
}

function showError(error) {
  clearResults();
  byId("error-message").textContent = error.message;
  byId("error-banner").hidden = false;
  byId("calculation-status").classList.remove("calculating");
  setText("calculation-status", "No result calculated");
}

function updateAddressBar() {
  const fragment = serializeScenario(contract, scenario);
  const url = `${window.location.pathname}#${fragment}`;
  window.history.replaceState(null, "", url);
}

function scenarioLocation() {
  return window.location.hash || window.location.search;
}

async function calculate() {
  const sequence = ++calculationSequence;
  scenario = readScenario();
  updateUnsupportedInputs(scenario.year, scenario.jurisdiction);
  byId("calculation-status").classList.add("calculating");
  setText("calculation-status", "Calculating locally…");
  byId("results-heading")
    .closest(".results-hero")
    .setAttribute("aria-busy", "true");

  try {
    const graph = await loadGraph(scenario.year);
    const startedAt = performance.now();
    const results = calculateScenario(graphlib, graph, contract, scenario);
    if (sequence !== calculationSequence) return;
    renderResults(results, performance.now() - startedAt);
    updateAddressBar();
  } catch (error) {
    if (sequence !== calculationSequence) return;
    showError(
      error instanceof Error ? error : new Error("Unknown calculation error"),
    );
  }
}

function scheduleCalculation() {
  const sequence = ++calculationSequence;
  window.requestAnimationFrame(() => {
    if (sequence === calculationSequence) calculate();
  });
}

function showToast(message) {
  const toast = byId("toast");
  window.clearTimeout(toastTimer);
  toast.textContent = message;
  toast.classList.add("visible");
  toastTimer = window.setTimeout(() => toast.classList.remove("visible"), 2200);
}

async function copyShareLink() {
  const url = window.location.href;
  try {
    await navigator.clipboard.writeText(url);
  } catch {
    const textarea = document.createElement("textarea");
    textarea.value = url;
    textarea.style.position = "fixed";
    textarea.style.opacity = "0";
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand("copy");
    textarea.remove();
  }
  showToast("Scenario link copied");
}

function bindEvents() {
  const form = byId("calculator-form");
  form.addEventListener("input", scheduleCalculation);
  form.addEventListener("change", scheduleCalculation);

  for (const id of ["tax-year", "filing-status", "jurisdiction"]) {
    byId(id).addEventListener("change", scheduleCalculation);
  }

  byId("share-scenario").addEventListener("click", copyShareLink);
  byId("reset-scenario").addEventListener("click", () => {
    scenario = createDefaultScenario(contract);
    renderScenario(scenario);
    scheduleCalculation();
    showToast("Scenario reset");
  });
  byId("dismiss-error").addEventListener("click", () => {
    byId("error-banner").hidden = true;
  });
  window.addEventListener("popstate", () => {
    try {
      scenario = parseScenario(contract, scenarioLocation());
      renderScenario(scenario);
      scheduleCalculation();
    } catch (error) {
      showError(error);
    }
  });
}

async function main() {
  let invalidShareLink = null;
  try {
    await init();
    contract = await loadBrowserContract();
    populateInterface();
    try {
      scenario = parseScenario(contract, scenarioLocation());
    } catch (error) {
      invalidShareLink = error;
      scenario = createDefaultScenario(contract);
    }
    renderScenario(scenario);
    bindEvents();
    await calculate();
    if (invalidShareLink) {
      showToast(`Invalid share link ignored: ${invalidShareLink.message}`);
    }
  } catch (error) {
    showError(
      error instanceof Error ? error : new Error("Initialization failed"),
    );
  } finally {
    byId("loading").classList.add("is-hidden");
  }
}

main();
