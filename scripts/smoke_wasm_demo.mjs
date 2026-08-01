import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const siteDirectory = path.resolve(process.argv[2] ?? "target/pages");
const modulePath = path.join(siteDirectory, "pkg", "graphlib.js");
const wasmPath = path.join(siteDirectory, "pkg", "graphlib_bg.wasm");
const graphlib = await import(pathToFileURL(modulePath));
await graphlib.default({ module_or_path: await readFile(wasmPath) });

const cases = [
  { year: 2024, taxableIncome: 85_400, totalTax: 13_841 },
  { year: 2025, taxableIncome: 84_250, totalTax: 13_449 },
];

for (const { year, taxableIncome, totalTax } of cases) {
  const graphPath = path.join(
    siteDirectory,
    "forms",
    `us_tax_graph_${year}.json`,
  );
  const graph = graphlib.Graph.fromJson(await readFile(graphPath, "utf8"));
  const runtime = new graphlib.Runtime(graph, graphlib.FilingStatus.single());
  runtime.set("us_1040_L1a_wages", 100_000);

  assert.equal(runtime.eval("us_1040_L11_agi"), 100_000);
  assert.equal(runtime.eval("us_1040_L15_taxable_income"), taxableIncome);
  assert.equal(runtime.eval("us_1040_L24_total_tax"), totalTax);
}

console.log(`WASM smoke test passed: ${siteDirectory}`);
