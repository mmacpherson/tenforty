// Function-level DOM stubs: exercise the shipped handlers, not a browser engine.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

const source = readFileSync(
  new URL("../crates/tenforty-graph/demo/app.js", import.meta.url),
  "utf8",
);
function functionSource(name) {
  const start = source.indexOf(`function ${name}(`);
  assert.notEqual(start, -1);
  const prefix = source.slice(start - 6, start) === "async " ? "async " : "";
  return prefix + source.slice(start, source.indexOf("\n}\n", start) + 2);
}
const elements = new Map();
function element(id) {
  if (!elements.has(id)) {
    elements.set(id, {
      textContent: "previous result",
      hidden: false,
      value: "",
      valueAsNumber: NaN,
      validity: { badInput: false },
      classList: { add() {}, remove() {} },
      setAttribute() {},
      closest() { return this; },
    });
  }
  return elements.get(id);
}
let evaluations = 0;
let addressUpdates = 0;
const context = vm.createContext({
  contract: { inputs: { w2_income: { type: "money", label: "Wages" } } },
  BrowserContractError: Error,
  byId: element,
  setText: (id, value) => { element(id).textContent = value; },
  document: { querySelectorAll: () => [] },
  updateUnsupportedInputs() {},
  loadGraph: async () => ({}),
  graphlib: {},
  performance: { now: () => 0 },
  calculateScenario() { evaluations++; return {}; },
  renderResults() {},
  updateAddressBar() { addressUpdates++; },
});
vm.runInContext(
  "let scenario; let calculationSequence = 0;\n" +
    ["readScenario", "clearResults", "showError", "calculate"].map(functionSource).join("\n"),
  context,
);
element("tax-year").value = "2025";
element("jurisdiction").value = "US";
element("filing-status").value = "single";
const wage = element("input-w2_income");
assert.equal(vm.runInContext("readScenario()", context).inputs.w2_income, 0);
wage.validity.badInput = true; // Native number inputs can be empty while invalid.
assert.throws(() => vm.runInContext("readScenario()", context), /Wages.*finite/);
await vm.runInContext("calculate()", context);
assert.equal(evaluations, 0);
assert.equal(addressUpdates, 0);
assert.equal(element("total-tax").textContent, "—");
assert.equal(element("error-banner").hidden, false);
wage.validity.badInput = false;
wage.value = "1e999";
wage.valueAsNumber = Infinity;
assert.throws(() => vm.runInContext("readScenario()", context), /Wages.*finite/);
wage.value = "1234.56";
wage.valueAsNumber = 1234.56;
assert.equal(vm.runInContext("readScenario()", context).inputs.w2_income, 1234.56);
await vm.runInContext("calculate()", context);
assert.equal(evaluations, 1);
assert.equal(addressUpdates, 1);
console.log("Browser UI handler regressions passed (DOM stubs).");
