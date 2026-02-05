import init, {
  FilingStatus,
  Graph,
  GraphSet,
  Runtime,
} from "./pkg/graphlib.js";

let graph = null;
let runtime = null;
let taxCurveData = [];
let stackedCurveData = [];

const CHART_PADDING = { top: 20, right: 20, bottom: 40, left: 60 };

const TAX_COMPONENTS = [
  { key: "ordinary", label: "Ordinary Income", color: "#3498db" },
  { key: "ltcg", label: "Capital Gains", color: "#9b59b6" },
  { key: "amt", label: "AMT", color: "#e74c3c" },
  { key: "niit", label: "NIIT", color: "#e67e22" },
  { key: "medicare", label: "Add'l Medicare", color: "#f1c40f" },
  { key: "state", label: "State", color: "#1abc9c" },
];
const CHART_WIDTH = 600;
const CHART_HEIGHT = 300;

const BRACKET_COLORS = [
  "#2ecc71",
  "#27ae60",
  "#f1c40f",
  "#e67e22",
  "#e74c3c",
  "#c0392b",
  "#8e44ad",
];

const FEDERAL_FORMS = [
  "us_1040",
  "us_schedule_d",
  "us_schedule_1",
  "us_schedule_a",
  "us_form_8995",
  "us_schedule_2",
  "us_form_8812",
  "us_schedule_3",
  "us_form_8863",
  "us_schedule_se",
  "us_form_6251",
  "us_form_8959",
  "us_form_8960",
  "us_form_2441",
];

const STATE_FORMS = {
  none: [],
  california: ["ca_540", "ca_schedule_ca", "ca_ftb_3514"],
  new_york: ["ny_it201"],
};

const STATE_TAX_NODES = {
  california: "ca_540_L64_ca_total_tax",
  new_york: "ny_it201_L46_ny_total_state_tax",
};

const YEAR = 2024;

function formatCurrency(value) {
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
    minimumFractionDigits: 0,
    maximumFractionDigits: 0,
  }).format(value);
}

function formatPercent(value) {
  return value.toFixed(1) + "%";
}

function getFilingStatus(value) {
  switch (value) {
    case "single":
      return FilingStatus.single();
    case "married_joint":
      return FilingStatus.married_joint();
    case "married_separate":
      return FilingStatus.married_separate();
    case "head_of_household":
      return FilingStatus.head_of_household();
    case "qualifying_widow":
      return FilingStatus.qualifying_widow();
    default:
      return FilingStatus.single();
  }
}

async function loadFormGraph(formId) {
  const url = `forms/${formId}_${YEAR}.json`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to load ${url}: ${response.status}`);
  }
  const json = await response.text();
  return Graph.fromJson(json);
}

async function buildLinkedGraph(stateKey) {
  const gs = new GraphSet();

  for (const formId of FEDERAL_FORMS) {
    const g = await loadFormGraph(formId);
    gs.add(formId, g);
  }

  const stateForms = STATE_FORMS[stateKey] || [];
  for (const formId of stateForms) {
    const g = await loadFormGraph(formId);
    gs.add(formId, g);
  }

  const unresolved = gs.unresolvedImports();
  if (unresolved.length > 0) {
    console.warn(
      "Unresolved imports after linking:",
      unresolved.map((u) => `${u.form}:${u.line} (${u.year})`),
    );
  }

  return gs.link();
}

function createRuntime() {
  const statusValue = document.getElementById("filing-status").value;
  const status = getFilingStatus(statusValue);
  runtime = new Runtime(graph, status);
}

function getInputValues() {
  return {
    wages: parseFloat(document.getElementById("wages").value) || 0,
    interest: parseFloat(document.getElementById("interest").value) || 0,
    dividends: parseFloat(document.getElementById("dividends").value) || 0,
    stcg: parseFloat(document.getElementById("stcg").value) || 0,
    ltcg: parseFloat(document.getElementById("ltcg").value) || 0,
    iso: parseFloat(document.getElementById("iso").value) || 0,
  };
}

function setRuntimeInputs(values) {
  runtime.set("us_1040_wages", values.wages);
  runtime.set("us_1040_interest", values.interest);
  runtime.set("us_1040_dividends", values.dividends);
  runtime.set("us_schedule_d_short_term_capital_gains", values.stcg);
  runtime.set("us_schedule_d_long_term_capital_gains", values.ltcg);
  runtime.set("us_form_6251_iso_exercise_gains", values.iso);
}

function safeEval(nodeName) {
  try {
    const val = runtime.eval(nodeName);
    return isFinite(val) ? val : 0;
  } catch (e) {
    return 0;
  }
}

function safeGradient(output, input) {
  try {
    const val = runtime.gradient(output, input);
    return isFinite(val) ? val : 0;
  } catch (e) {
    return 0;
  }
}

function getStateKey() {
  return document.getElementById("state").value;
}

function showState() {
  return getStateKey() !== "none";
}

function getStateTaxNode() {
  return STATE_TAX_NODES[getStateKey()] || null;
}

function updateResults() {
  if (!graph || !runtime) return;

  const values = getInputValues();
  const stateActive = showState();

  try {
    setRuntimeInputs(values);

    const grossIncome = safeEval("us_1040_gross_income");
    const stdDeduction = safeEval("us_1040_standard_deduction");
    const taxableIncome = safeEval("us_1040_taxable_ordinary_income");
    const ordinaryTax = safeEval("us_1040_ordinary_income_tax");
    const ltcgTax = safeEval("us_1040_ltcg_tax");
    const niit = safeEval("us_form_8960_niit");
    const medicareTax = safeEval("us_form_8959_additional_medicare_tax");
    const amt = safeEval("us_form_6251_amt");
    const federalTax = safeEval("us_1040_federal_tax");
    const stateTaxNode = getStateTaxNode();
    const stateTax = stateTaxNode ? safeEval(stateTaxNode) : 0;
    const totalTax = federalTax + stateTax;
    const effectiveRate =
      grossIncome > 0 ? (federalTax / grossIncome) * 100 : 0;
    const marginalRate =
      safeGradient("us_1040_federal_tax", "us_1040_wages") * 100;

    document.getElementById("gross-income").textContent =
      formatCurrency(grossIncome);
    document.getElementById("std-deduction").textContent =
      "-" + formatCurrency(stdDeduction);
    document.getElementById("taxable-income").textContent =
      formatCurrency(taxableIncome);
    document.getElementById("ordinary-tax").textContent =
      formatCurrency(ordinaryTax);
    document.getElementById("ltcg-tax").textContent = formatCurrency(ltcgTax);

    const ltcgRow = document.getElementById("ltcg-tax-row");
    ltcgRow.style.display = ltcgTax > 0 ? "flex" : "none";

    const niitRow = document.getElementById("niit-row");
    niitRow.style.display = niit > 0 ? "flex" : "none";
    document.getElementById("niit").textContent = formatCurrency(niit);

    const medicareRow = document.getElementById("medicare-row");
    medicareRow.style.display = medicareTax > 0 ? "flex" : "none";
    document.getElementById("medicare-tax").textContent =
      formatCurrency(medicareTax);

    const federalBeforeAmt = safeEval("us_1040_federal_before_amt");
    const tentativeMinTax = safeEval("us_form_6251_tentative_minimum_tax");
    const amtCushion = federalBeforeAmt - tentativeMinTax;

    const amtRow = document.getElementById("amt-row");
    const amtLabel = document.getElementById("amt-label");
    const amtValue = document.getElementById("amt");

    if (amt > 0) {
      amtRow.style.display = "flex";
      amtRow.classList.add("in-amt");
      amtRow.classList.remove("amt-cushion");
      amtLabel.textContent = "AMT (paying extra)";
      amtValue.textContent = "+" + formatCurrency(amt);
    } else if (values.iso > 0 && amtCushion > 0) {
      amtRow.style.display = "flex";
      amtRow.classList.remove("in-amt");
      amtRow.classList.add("amt-cushion");
      amtLabel.textContent = "AMT Cushion";
      amtValue.textContent = formatCurrency(amtCushion);
    } else {
      amtRow.style.display = "none";
    }

    document.getElementById("federal-tax").textContent =
      formatCurrency(federalTax);

    const stateRow = document.getElementById("state-tax-row");
    const totalRow = document.getElementById("total-row");
    const stateTaxLabel = document.getElementById("state-tax-label");
    stateRow.style.display = stateActive ? "flex" : "none";
    totalRow.style.display = stateActive ? "flex" : "none";
    if (stateTaxLabel) {
      const stateKey = getStateKey();
      const stateNames = {
        california: "California",
        new_york: "New York",
      };
      stateTaxLabel.textContent =
        (stateNames[stateKey] || "State") + " State Tax";
    }
    document.getElementById("state-tax").textContent = formatCurrency(stateTax);
    document.getElementById("total-tax").textContent = formatCurrency(
      stateActive ? totalTax : federalTax,
    );

    document.getElementById("effective-rate").textContent =
      formatPercent(effectiveRate);
    document.getElementById("marginal-rate").textContent =
      formatPercent(marginalRate);

    updateSensitivity();
    updateBracketVisualization(taxableIncome);
    const isStacked = document.getElementById("stacked-toggle").checked;
    const chartTax = stateActive && isStacked ? totalTax : federalTax;
    updateTaxCurve(values.wages, chartTax);
  } catch (e) {
    console.error("Evaluation error:", e);
  }
}

function updateSensitivity() {
  const stateTaxNode = getStateTaxNode();

  const federalWagesGrad = safeGradient("us_1040_federal_tax", "us_1040_wages");
  const federalInterestGrad = safeGradient(
    "us_1040_federal_tax",
    "us_1040_interest",
  );
  const federalLtcgGrad = safeGradient(
    "us_1040_federal_tax",
    "us_schedule_d_long_term_capital_gains",
  );

  const stateWagesGrad = stateTaxNode
    ? safeGradient(stateTaxNode, "us_1040_wages")
    : 0;
  const stateInterestGrad = stateTaxNode
    ? safeGradient(stateTaxNode, "us_1040_interest")
    : 0;
  const stateLtcgGrad = stateTaxNode
    ? safeGradient(stateTaxNode, "us_schedule_d_long_term_capital_gains")
    : 0;

  const wagesGrad = federalWagesGrad + stateWagesGrad;
  const interestGrad = federalInterestGrad + stateInterestGrad;
  const ltcgGrad = federalLtcgGrad + stateLtcgGrad;

  document.getElementById("sens-wages").textContent =
    "+" + formatCurrency(wagesGrad * 1000);
  document.getElementById("sens-wages-rate").textContent = formatPercent(
    wagesGrad * 100,
  );

  document.getElementById("sens-interest").textContent =
    "+" + formatCurrency(interestGrad * 1000);
  document.getElementById("sens-interest-rate").textContent = formatPercent(
    interestGrad * 100,
  );

  document.getElementById("sens-ltcg").textContent =
    "+" + formatCurrency(ltcgGrad * 1000);
  document.getElementById("sens-ltcg-rate").textContent = formatPercent(
    ltcgGrad * 100,
  );
}

function updateBracketVisualization(taxableIncome) {
  const bracketBar = document.getElementById("bracket-bar");
  const bracketLegend = document.getElementById("bracket-legend");

  // Read brackets from graph by evaluating at known income levels
  const brackets = readBracketsFromGraph();

  const maxIncome = Math.max(taxableIncome * 1.5, 250000);
  let prevThreshold = 0;
  let currentBracketIndex = -1;

  bracketBar.innerHTML = "";
  bracketLegend.innerHTML = "";

  brackets.forEach((bracket, i) => {
    const bracketEnd = Math.min(bracket.threshold, maxIncome);
    const bracketWidth = bracketEnd - prevThreshold;

    if (bracketWidth <= 0) return;

    const widthPercent = (bracketWidth / maxIncome) * 100;
    const segment = document.createElement("div");
    segment.className = "bracket-segment";
    segment.style.flex = widthPercent;
    segment.textContent = bracket.rate * 100 + "%";

    if (taxableIncome > prevThreshold && taxableIncome <= bracket.threshold) {
      currentBracketIndex = i;
      segment.classList.add("bracket-marker");
    }

    bracketBar.appendChild(segment);

    const legendItem = document.createElement("div");
    legendItem.className =
      "legend-item" + (i === currentBracketIndex ? " current" : "");
    legendItem.innerHTML = `
            <span class="legend-color" style="background: ${
              BRACKET_COLORS[i % BRACKET_COLORS.length]
            }"></span>
            <span>${bracket.rate * 100}%: ${formatCurrency(prevThreshold)} - ${
              bracket.threshold === Infinity
                ? "above"
                : formatCurrency(bracket.threshold)
            }</span>
        `;
    bracketLegend.appendChild(legendItem);

    prevThreshold = bracket.threshold;
  });
}

function readBracketsFromGraph() {
  // Probe the marginal rate at various income levels to detect bracket boundaries.
  // This works by evaluating the gradient at many points and detecting rate changes.
  const probePoints = [];
  for (let i = 0; i <= 100; i++) {
    probePoints.push(i * 10000);
  }

  const brackets = [];
  let lastRate = -1;

  for (const income of probePoints) {
    runtime.set("us_1040_wages", income);
    // Zero out other inputs for bracket probing
    runtime.set("us_1040_interest", 0);
    runtime.set("us_1040_dividends", 0);
    runtime.set("us_schedule_d_short_term_capital_gains", 0);
    runtime.set("us_schedule_d_long_term_capital_gains", 0);
    runtime.set("us_form_6251_iso_exercise_gains", 0);

    const rate = safeGradient("us_1040_ordinary_income_tax", "us_1040_wages");
    const roundedRate = Math.round(rate * 10000) / 10000;

    if (roundedRate !== lastRate && roundedRate > 0) {
      if (brackets.length > 0) {
        brackets[brackets.length - 1].threshold = income;
      }
      brackets.push({ threshold: Infinity, rate: roundedRate });
      lastRate = roundedRate;
    }
  }

  // Restore actual inputs
  const values = getInputValues();
  setRuntimeInputs(values);

  if (brackets.length === 0) {
    // Fallback: hardcoded 2024 single brackets
    return [
      { threshold: 11600, rate: 0.1 },
      { threshold: 47150, rate: 0.12 },
      { threshold: 100525, rate: 0.22 },
      { threshold: 191950, rate: 0.24 },
      { threshold: 243725, rate: 0.32 },
      { threshold: 609350, rate: 0.35 },
      { threshold: Infinity, rate: 0.37 },
    ];
  }

  return brackets;
}

function computeTaxCurve(overrideValues = null) {
  const numPoints = 100;
  const maxIncome = 500000;
  const values = overrideValues || getInputValues();
  const stateTaxNode = getStateTaxNode();

  taxCurveData = [];
  stackedCurveData = [];

  for (let i = 0; i <= numPoints; i++) {
    const income = (i / numPoints) * maxIncome;
    runtime.set("us_1040_wages", income);
    runtime.set("us_1040_interest", values.interest);
    runtime.set("us_1040_dividends", values.dividends);
    runtime.set("us_schedule_d_short_term_capital_gains", values.stcg);
    runtime.set("us_schedule_d_long_term_capital_gains", values.ltcg);
    runtime.set("us_form_6251_iso_exercise_gains", values.iso);

    const tax = safeEval("us_1040_federal_tax");
    taxCurveData.push({ income, tax });

    stackedCurveData.push({
      income,
      ordinary: safeEval("us_1040_ordinary_income_tax"),
      ltcg: safeEval("us_1040_ltcg_tax"),
      amt: safeEval("us_form_6251_amt"),
      niit: safeEval("us_form_8960_niit"),
      medicare: safeEval("us_form_8959_additional_medicare_tax"),
      state: stateTaxNode ? safeEval(stateTaxNode) : 0,
    });
  }

  setRuntimeInputs(values);
}

function updateTaxCurve(currentIncome, currentTax) {
  const svg = document.getElementById("tax-chart");
  const width = CHART_WIDTH - CHART_PADDING.left - CHART_PADDING.right;
  const height = CHART_HEIGHT - CHART_PADDING.top - CHART_PADDING.bottom;
  const isStacked = document.getElementById("stacked-toggle").checked;
  const stateActive = showState();

  if (taxCurveData.length === 0) {
    computeTaxCurve();
  }

  const maxIncome = 500000;

  let maxTax;
  if (isStacked && stackedCurveData.length > 0) {
    const componentKeys = TAX_COMPONENTS.filter(
      (c) => c.key !== "state" || stateActive,
    ).map((c) => c.key);
    maxTax =
      Math.max(
        ...stackedCurveData.map((d) =>
          componentKeys.reduce((sum, key) => sum + (d[key] || 0), 0),
        ),
        currentTax,
      ) * 1.1;
  } else {
    maxTax = Math.max(...taxCurveData.map((d) => d.tax), currentTax) * 1.1;
  }

  const xScale = (val) => CHART_PADDING.left + (val / maxIncome) * width;
  const yScale = (val) => CHART_PADDING.top + height - (val / maxTax) * height;

  const gridGroup = svg.querySelector(".grid");
  gridGroup.innerHTML = "";

  for (let i = 0; i <= 5; i++) {
    const y = CHART_PADDING.top + (i / 5) * height;
    const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
    line.setAttribute("x1", CHART_PADDING.left);
    line.setAttribute("x2", CHART_WIDTH - CHART_PADDING.right);
    line.setAttribute("y1", y);
    line.setAttribute("y2", y);
    gridGroup.appendChild(line);
  }

  for (let i = 0; i <= 5; i++) {
    const x = CHART_PADDING.left + (i / 5) * width;
    const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
    line.setAttribute("x1", x);
    line.setAttribute("x2", x);
    line.setAttribute("y1", CHART_PADDING.top);
    line.setAttribute("y2", CHART_HEIGHT - CHART_PADDING.bottom);
    gridGroup.appendChild(line);
  }

  const labelsGroup = svg.querySelector(".axis-labels");
  labelsGroup.innerHTML = "";

  for (let i = 0; i <= 5; i++) {
    const income = (i / 5) * maxIncome;
    const x = xScale(income);
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("x", x);
    text.setAttribute("y", CHART_HEIGHT - 18);
    text.setAttribute("text-anchor", "middle");
    text.textContent = "$" + income / 1000 + "K";
    labelsGroup.appendChild(text);
  }

  const xLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
  xLabel.setAttribute("x", CHART_PADDING.left + width / 2);
  xLabel.setAttribute("y", CHART_HEIGHT - 2);
  xLabel.setAttribute("text-anchor", "middle");
  xLabel.setAttribute("font-size", "11");
  xLabel.setAttribute("fill", "#7f8c8d");
  xLabel.textContent = "Wages";
  labelsGroup.appendChild(xLabel);

  for (let i = 0; i <= 4; i++) {
    const tax = ((4 - i) / 4) * maxTax;
    const y = CHART_PADDING.top + (i / 4) * height;
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("x", CHART_PADDING.left - 5);
    text.setAttribute("y", y + 4);
    text.setAttribute("text-anchor", "end");
    text.textContent = "$" + Math.round(tax / 1000) + "K";
    labelsGroup.appendChild(text);
  }

  const stackedGroup = svg.querySelector(".stacked-areas");
  const simpleCurve = svg.querySelector(".tax-curve");
  const simpleArea = svg.querySelector(".tax-area");
  const legendDiv = document.getElementById("stacked-legend");

  stackedGroup.innerHTML = "";

  if (isStacked && stackedCurveData.length > 0) {
    simpleCurve.style.display = "none";
    simpleArea.style.display = "none";
    legendDiv.style.display = "flex";

    const activeComponents = TAX_COMPONENTS.filter(
      (c) => c.key !== "state" || stateActive,
    );

    const stacks = activeComponents.map((comp, idx) => {
      return stackedCurveData.map((d) => {
        const baseY = activeComponents
          .slice(0, idx)
          .reduce((sum, c) => sum + (d[c.key] || 0), 0);
        const topY = baseY + (d[comp.key] || 0);
        return { income: d.income, baseY, topY };
      });
    });

    stacks.forEach((stack, idx) => {
      const comp = activeComponents[idx];
      let areaPath = `M ${xScale(0)} ${yScale(stack[0].baseY)}`;

      stack.forEach((pt) => {
        areaPath += ` L ${xScale(pt.income)} ${yScale(pt.topY)}`;
      });

      for (let i = stack.length - 1; i >= 0; i--) {
        areaPath += ` L ${xScale(stack[i].income)} ${yScale(stack[i].baseY)}`;
      }
      areaPath += " Z";

      const path = document.createElementNS(
        "http://www.w3.org/2000/svg",
        "path",
      );
      path.setAttribute("d", areaPath);
      path.setAttribute("fill", comp.color);
      stackedGroup.appendChild(path);
    });

    legendDiv.innerHTML = activeComponents
      .map(
        (c) =>
          `<div class="stacked-legend-item">
                <span class="stacked-legend-color" style="background: ${c.color}"></span>
                <span>${c.label}</span>
            </div>`,
      )
      .join("");
  } else {
    simpleCurve.style.display = "";
    simpleArea.style.display = "";
    legendDiv.style.display = "none";

    let pathD = "";
    let areaD = `M ${xScale(0)} ${yScale(0)} `;

    taxCurveData.forEach((point, i) => {
      const x = xScale(point.income);
      const y = yScale(point.tax);
      if (i === 0) {
        pathD = `M ${x} ${y}`;
        areaD += `L ${x} ${y} `;
      } else {
        pathD += ` L ${x} ${y}`;
        areaD += `L ${x} ${y} `;
      }
    });

    areaD += `L ${xScale(maxIncome)} ${yScale(0)} Z`;

    simpleCurve.setAttribute("d", pathD);
    simpleArea.setAttribute("d", areaD);
  }

  const currentX = xScale(Math.min(currentIncome, maxIncome));
  const currentY = yScale(currentTax);

  const pointGroup = svg.querySelector(".current-point");
  const circle = pointGroup.querySelector("circle");
  const crosshairV = pointGroup.querySelector(".crosshair-v");
  const crosshairH = pointGroup.querySelector(".crosshair-h");

  circle.setAttribute("cx", currentX);
  circle.setAttribute("cy", currentY);

  crosshairV.setAttribute("x1", currentX);
  crosshairV.setAttribute("x2", currentX);
  crosshairV.setAttribute("y1", CHART_PADDING.top);
  crosshairV.setAttribute("y2", CHART_HEIGHT - CHART_PADDING.bottom);

  crosshairH.setAttribute("x1", CHART_PADDING.left);
  crosshairH.setAttribute("x2", CHART_WIDTH - CHART_PADDING.right);
  crosshairH.setAttribute("y1", currentY);
  crosshairH.setAttribute("y2", currentY);
}

function handleSolve() {
  if (!graph || !runtime) return;

  const targetTax =
    parseFloat(document.getElementById("target-tax").value) || 0;
  const values = getInputValues();

  const btn = document.getElementById("solve-btn");
  const btnText = btn.querySelector(".btn-text");
  const btnLoading = btn.querySelector(".btn-loading");
  const animationDiv = document.getElementById("solver-animation");
  const resultDiv = document.getElementById("solver-result");

  btn.disabled = true;
  btnText.style.display = "none";
  btnLoading.style.display = "inline";
  animationDiv.style.display = "block";
  resultDiv.style.display = "none";

  runtime.set("us_1040_interest", values.interest);
  runtime.set("us_1040_dividends", values.dividends);
  runtime.set("us_schedule_d_short_term_capital_gains", values.stcg);
  runtime.set("us_schedule_d_long_term_capital_gains", values.ltcg);
  runtime.set("us_form_6251_iso_exercise_gains", values.iso);

  animateSolver(targetTax, values.wages).then((result) => {
    btn.disabled = false;
    btnText.style.display = "inline";
    btnLoading.style.display = "none";

    if (result.success) {
      document.getElementById("solved-wages").textContent = formatCurrency(
        result.wages,
      );
      resultDiv.style.display = "flex";
    } else {
      document.getElementById("solved-wages").textContent =
        "Could not converge";
      resultDiv.style.display = "flex";
    }
  });
}

async function animateSolver(targetTax, initialGuess) {
  const svg = document.getElementById("solver-chart");
  const iterCountEl = document.getElementById("iter-count");
  const iterErrorEl = document.getElementById("iter-error");

  const minWages = 0;
  const maxWages = Math.max(initialGuess * 3, 300000);
  const padding = { left: 40, right: 20, top: 15, bottom: 25 };
  const width = 400 - padding.left - padding.right;
  const height = 200 - padding.top - padding.bottom;

  const curvePoints = [];
  for (let i = 0; i <= 50; i++) {
    const w = (i / 50) * maxWages;
    runtime.set("us_1040_wages", w);
    const tax = safeEval("us_1040_federal_tax");
    curvePoints.push({ wages: w, tax });
  }

  const maxTax = Math.max(...curvePoints.map((p) => p.tax), targetTax) * 1.1;

  const xScale = (w) => padding.left + (w / maxWages) * width;
  const yScale = (t) => padding.top + height - (t / maxTax) * height;

  let curveD = "";
  curvePoints.forEach((p, i) => {
    const x = xScale(p.wages);
    const y = yScale(p.tax);
    curveD += (i === 0 ? "M" : "L") + ` ${x} ${y} `;
  });

  svg.querySelector(".solver-curve").setAttribute("d", curveD);

  const targetLine = svg.querySelector(".target-line");
  targetLine.setAttribute("x1", padding.left);
  targetLine.setAttribute("x2", 400 - padding.right);
  targetLine.setAttribute("y1", yScale(targetTax));
  targetLine.setAttribute("y2", yScale(targetTax));

  svg.querySelector(".iteration-points").innerHTML = "";

  const iterations = [];
  let currentWages = initialGuess;
  const maxIter = 20;
  const tolerance = 1;

  for (let iter = 0; iter < maxIter; iter++) {
    runtime.set("us_1040_wages", currentWages);
    const tax = safeEval("us_1040_federal_tax");
    const gradient = safeGradient("us_1040_federal_tax", "us_1040_wages");
    const error = tax - targetTax;

    iterations.push({
      wages: currentWages,
      tax,
      gradient,
      error,
    });

    if (Math.abs(error) < tolerance || gradient === 0) {
      break;
    }

    currentWages = currentWages - error / gradient;
    currentWages = Math.max(0, Math.min(currentWages, maxWages * 2));
  }

  for (let i = 0; i < iterations.length; i++) {
    const iter = iterations[i];

    await new Promise((r) => setTimeout(r, 300));

    const x = xScale(iter.wages);
    const y = yScale(iter.tax);

    const pointsGroup = svg.querySelector(".iteration-points");
    const circle = document.createElementNS(
      "http://www.w3.org/2000/svg",
      "circle",
    );
    circle.setAttribute("cx", x);
    circle.setAttribute("cy", y);
    circle.setAttribute("r", 4);
    pointsGroup.appendChild(circle);

    const currentCircle = svg.querySelector(".current-iter");
    currentCircle.setAttribute("cx", x);
    currentCircle.setAttribute("cy", y);

    if (iter.gradient !== 0 && i < iterations.length - 1) {
      const tangentX1 = Math.max(xScale(iter.wages - 20000), padding.left);
      const tangentX2 = Math.min(
        xScale(iter.wages + 20000),
        400 - padding.right,
      );
      const tangentY1 =
        y -
        (tangentX1 - x) *
          ((-iter.gradient * (maxWages / width)) / (maxTax / height));
      const tangentY2 =
        y -
        (tangentX2 - x) *
          ((-iter.gradient * (maxWages / width)) / (maxTax / height));

      const tangentLine = svg.querySelector(".tangent-line");
      tangentLine.setAttribute("x1", tangentX1);
      tangentLine.setAttribute("y1", tangentY1);
      tangentLine.setAttribute("x2", tangentX2);
      tangentLine.setAttribute("y2", tangentY2);
    }

    iterCountEl.textContent = i + 1;
    iterErrorEl.textContent = formatCurrency(Math.abs(iter.error));
  }

  const finalIter = iterations[iterations.length - 1];
  return {
    success: Math.abs(finalIter.error) < 100,
    wages: finalIter.wages,
  };
}

function syncSliders() {
  const sliderPairs = [
    ["wages-slider", "wages"],
    ["interest-slider", "interest"],
    ["dividends-slider", "dividends"],
    ["stcg-slider", "stcg"],
    ["ltcg-slider", "ltcg"],
    ["iso-slider", "iso"],
  ];

  sliderPairs.forEach(([sliderId, inputId]) => {
    const slider = document.getElementById(sliderId);
    const input = document.getElementById(inputId);
    const invalidatesCurve = inputId !== "wages";

    slider.addEventListener("input", () => {
      input.value = slider.value;
      if (invalidatesCurve) {
        taxCurveData = [];
        computeTaxCurve(getInputValues());
      }
      updateResults();
    });

    input.addEventListener("input", () => {
      slider.value = Math.min(input.value, slider.max);
      if (invalidatesCurve) {
        taxCurveData = [];
        computeTaxCurve(getInputValues());
      }
      updateResults();
    });
  });
}

function setupCollapsibles() {
  document.querySelectorAll(".collapsible").forEach((section) => {
    const header = section.querySelector(".collapsible-header");
    header.addEventListener("click", () => {
      section.classList.toggle("open");
    });
  });
}

function showError(message) {
  const loading = document.getElementById("loading");
  loading.textContent = "";

  const errorP = document.createElement("p");
  errorP.style.color = "#e74c3c";
  errorP.textContent = "Failed to load: " + message;

  const helpP = document.createElement("p");
  helpP.textContent = "Make sure to run: ";
  const code = document.createElement("code");
  code.textContent = "make wasm-dev";
  helpP.appendChild(code);

  loading.appendChild(errorP);
  loading.appendChild(helpP);
}

async function main() {
  try {
    await init();

    document.getElementById("loading").querySelector("p").textContent =
      "Loading tax forms...";

    const stateKey = getStateKey();
    graph = await buildLinkedGraph(stateKey);

    createRuntime();
    syncSliders();
    setupCollapsibles();
    computeTaxCurve();
    updateResults();

    document.getElementById("filing-status").addEventListener("change", () => {
      createRuntime();
      taxCurveData = [];
      computeTaxCurve();
      updateResults();
    });

    document.getElementById("state").addEventListener("change", async () => {
      const newState = getStateKey();
      document.getElementById("loading").style.display = "flex";
      document.getElementById("loading").querySelector("p").textContent =
        "Switching state...";
      try {
        graph = await buildLinkedGraph(newState);
        createRuntime();
        taxCurveData = [];
        computeTaxCurve();
        updateResults();
      } catch (e) {
        console.error("Failed to load state:", e);
        showError(e.message);
      }
      document.getElementById("loading").style.display = "none";
    });

    document
      .getElementById("stacked-toggle")
      .addEventListener("change", updateResults);

    document.getElementById("solve-btn").addEventListener("click", handleSolve);
    document.getElementById("target-tax").addEventListener("keypress", (e) => {
      if (e.key === "Enter") handleSolve();
    });

    document.getElementById("loading").style.display = "none";
  } catch (e) {
    console.error("Initialization error:", e);
    showError(e.message);
  }
}

main();
