#!/usr/bin/env python3
"""Script to discover bugs by fuzzing the OTS backend against the Graph backend."""

import random
import sys
import time
from dataclasses import dataclass
from typing import Any

try:
    from tenforty import evaluate_return
    from tenforty.backends import GraphBackend, OTSBackend
except ImportError as e:
    print(f"Error: Could not import tenforty. {e}")
    sys.exit(1)

# Configuration
NUM_ITERATIONS = 500  # Number of random scenarios to run
AGI_TOLERANCE = 1.0
TAXABLE_TOLERANCE = 1.0
TAX_TOLERANCE = 10.0
YEAR = 2024


@dataclass
class Discrepancy:
    """Dataclass to hold details of a discrepancy found during fuzzing."""

    inputs: dict[str, Any]
    ots_result: Any
    graph_result: Any
    diff_description: str


def generate_random_inputs():
    """Generate a random set of inputs for a tax return."""
    filing_status = random.choice(
        ["Single", "Married/Joint", "Head_of_House", "Married/Sep"]
    )

    # Base income
    w2_income = random.randint(0, 300_000)

    # Other income types (mostly small amounts, occasionally large)
    taxable_interest = 0
    if random.random() < 0.3:
        taxable_interest = random.randint(0, 20_000)

    ordinary_dividends = 0
    qualified_dividends = 0
    if random.random() < 0.3:
        ordinary_dividends = random.randint(0, 20_000)
        # Qualified must be <= ordinary
        if ordinary_dividends > 0:
            qualified_dividends = random.randint(0, ordinary_dividends)

    short_term_capital_gains = 0
    long_term_capital_gains = 0
    if random.random() < 0.3:
        short_term_capital_gains = random.randint(0, 20_000)
    if random.random() < 0.3:
        long_term_capital_gains = random.randint(0, 20_000)

    schedule_1_income = 0
    if random.random() < 0.2:
        schedule_1_income = random.randint(0, 20_000)

    itemized_deductions = 0
    standard_or_itemized = "Standard"
    if random.random() < 0.2:
        # Try to force itemized sometimes
        itemized_deductions = random.randint(15_000, 50_000)
        standard_or_itemized = "Itemized"

    return {
        "year": YEAR,
        "filing_status": filing_status,
        "w2_income": w2_income,
        "taxable_interest": taxable_interest,
        "ordinary_dividends": ordinary_dividends,
        "qualified_dividends": qualified_dividends,
        "short_term_capital_gains": short_term_capital_gains,
        "long_term_capital_gains": long_term_capital_gains,
        "schedule_1_income": schedule_1_income,
        "itemized_deductions": itemized_deductions,
        "standard_or_itemized": standard_or_itemized,
    }


def run_comparison():
    """Run the comparison loop."""
    print(f"Starting bug discovery... ({NUM_ITERATIONS} iterations)")

    if not OTSBackend().is_available():
        print("OTS Backend is not available. Skipping.")
        return
    if not GraphBackend().is_available():
        print("Graph Backend is not available. Skipping.")
        return

    discrepancies = []
    start_time = time.time()

    for i in range(NUM_ITERATIONS):
        inputs = generate_random_inputs()

        try:
            ots_res = evaluate_return(**inputs, backend="ots")
            graph_res = evaluate_return(**inputs, backend="graph")

            diffs = []

            # Check AGI
            agi_diff = abs(
                ots_res.federal_adjusted_gross_income
                - graph_res.federal_adjusted_gross_income
            )
            if agi_diff > AGI_TOLERANCE:
                diffs.append(
                    f"AGI diff: {agi_diff:.2f} (OTS: {ots_res.federal_adjusted_gross_income}, Graph: {graph_res.federal_adjusted_gross_income})"
                )

            # Check Taxable Income
            taxable_diff = abs(
                ots_res.federal_taxable_income - graph_res.federal_taxable_income
            )
            if taxable_diff > TAXABLE_TOLERANCE:
                diffs.append(
                    f"Taxable diff: {taxable_diff:.2f} (OTS: {ots_res.federal_taxable_income}, Graph: {graph_res.federal_taxable_income})"
                )

            # Check Total Tax
            tax_diff = abs(ots_res.federal_total_tax - graph_res.federal_total_tax)
            if tax_diff > TAX_TOLERANCE:
                diffs.append(
                    f"Tax diff: {tax_diff:.2f} (OTS: {ots_res.federal_total_tax}, Graph: {graph_res.federal_total_tax})"
                )

            if diffs:
                discrepancies.append(
                    Discrepancy(inputs, ots_res, graph_res, "; ".join(diffs))
                )
                print("X", end="", flush=True)
            else:
                if i % 50 == 0:
                    print(".", end="", flush=True)

        except Exception as e:
            print(f"\nError running scenario {inputs}: {e}")
            continue

    print("\n\n=== Summary ===")
    print(f"Time taken: {time.time() - start_time:.2f}s")
    print(f"Iterations: {NUM_ITERATIONS}")
    print(f"Discrepancies found: {len(discrepancies)}")

    if discrepancies:
        print("\n=== Discrepancy Details ===")
        log_file = "discovered_bugs.txt"
        with open(log_file, "w") as f:
            for idx, d in enumerate(discrepancies):
                msg = f"\nCase #{idx + 1}:\nInputs: {d.inputs}\nIssues: {d.diff_description}\n"
                print(msg)
                f.write(msg + "\n")
        print(f"\nFull details written to {log_file}")


if __name__ == "__main__":
    run_comparison()
