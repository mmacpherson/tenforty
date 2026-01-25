//! Throughput benchmark comparing interpreter vs JIT+SIMD across core counts.
//!
//! Run with: cargo run --example throughput_bench --features "jit parallel" --release

use graphlib::eval::Runtime;
use graphlib::graph::{FilingStatus, Graph};
use rayon::prelude::*;
use std::sync::Arc;
use std::time::Instant;

#[cfg(feature = "jit")]
use graphlib::jit::{JitBatchRuntime, JitCompiler, JitRuntime, BATCH_SIZE};

const NUM_SCENARIOS: usize = 100_000;

fn load_demo_graph() -> Graph {
    let json = include_str!("../../src/tenforty/forms/us_1040_simple.json");
    Graph::from_json(json).unwrap()
}

fn generate_scenarios(n: usize) -> Vec<(f64, f64, f64)> {
    (0..n)
        .map(|i| {
            let wages = 30000.0 + (i as f64 * 1000.0) % 200000.0;
            let interest = 100.0 + (i as f64 * 7.0) % 5000.0;
            let dividends = 50.0 + (i as f64 * 3.0) % 3000.0;
            (wages, interest, dividends)
        })
        .collect()
}

fn bench_interpreter_sequential(graph: &Graph, scenarios: &[(f64, f64, f64)]) -> f64 {
    let start = Instant::now();
    let mut sum = 0.0;
    let mut rt = Runtime::new(graph, FilingStatus::Single);

    for &(wages, interest, dividends) in scenarios {
        rt.set("wages", wages).unwrap();
        rt.set("interest", interest).unwrap();
        rt.set("dividends", dividends).unwrap();
        sum += rt.eval("federal_tax").unwrap();
    }

    let elapsed = start.elapsed().as_secs_f64();
    let throughput = scenarios.len() as f64 / elapsed;
    println!(
        "  Interpreter (1 core):     {:>10.0} scenarios/sec  (checksum={:.2})",
        throughput, sum
    );
    throughput
}

fn bench_interpreter_parallel(graph: &Graph, scenarios: &[(f64, f64, f64)], num_threads: usize) -> f64 {
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
        .unwrap();

    pool.install(|| {
        let start = Instant::now();

        let sum: f64 = scenarios
            .par_iter()
            .map(|&(wages, interest, dividends)| {
                let mut rt = Runtime::new(graph, FilingStatus::Single);
                rt.set("wages", wages).unwrap();
                rt.set("interest", interest).unwrap();
                rt.set("dividends", dividends).unwrap();
                rt.eval("federal_tax").unwrap()
            })
            .sum();

        let elapsed = start.elapsed().as_secs_f64();
        let throughput = scenarios.len() as f64 / elapsed;
        println!(
            "  Interpreter ({} cores):   {:>10.0} scenarios/sec  (checksum={:.2})",
            num_threads, throughput, sum
        );
        throughput
    })
}

#[cfg(feature = "jit")]
fn bench_jit_sequential(graph: &Graph, scenarios: &[(f64, f64, f64)]) -> f64 {
    let compiler = JitCompiler::new().unwrap();
    let compiled = compiler.compile(graph, FilingStatus::Single).unwrap();

    let start = Instant::now();
    let mut sum = 0.0;
    let mut rt = JitRuntime::new(compiled, graph);

    for &(wages, interest, dividends) in scenarios {
        rt.set("wages", wages).unwrap();
        rt.set("interest", interest).unwrap();
        rt.set("dividends", dividends).unwrap();
        sum += rt.eval("federal_tax").unwrap();
    }

    let elapsed = start.elapsed().as_secs_f64();
    let throughput = scenarios.len() as f64 / elapsed;
    println!(
        "  JIT scalar (1 core):      {:>10.0} scenarios/sec  (checksum={:.2})",
        throughput, sum
    );
    throughput
}

#[cfg(feature = "jit")]
fn bench_jit_parallel(graph: &Graph, scenarios: &[(f64, f64, f64)], num_threads: usize) -> f64 {
    let compiler = JitCompiler::new().unwrap();

    // Pre-compile one function per thread
    let compiled_funcs: Vec<_> = (0..num_threads)
        .map(|_| compiler.compile(graph, FilingStatus::Single).unwrap())
        .collect();
    let compiled_funcs = Arc::new(compiled_funcs);

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
        .unwrap();

    pool.install(|| {
        use std::cell::RefCell;
        use std::sync::atomic::{AtomicUsize, Ordering};

        let thread_counter = AtomicUsize::new(0);

        thread_local! {
            static THREAD_IDX: RefCell<Option<usize>> = const { RefCell::new(None) };
        }

        let start = Instant::now();

        // Process in chunks to allow runtime reuse within each thread
        let chunk_size = (scenarios.len() / num_threads).max(1000);

        let sum: f64 = scenarios
            .par_chunks(chunk_size)
            .map(|chunk| {
                // Get or assign thread index
                let compiled = compiler.compile(graph, FilingStatus::Single).unwrap();
                let mut rt = JitRuntime::new(compiled, graph);

                let mut local_sum = 0.0;
                for &(wages, interest, dividends) in chunk {
                    rt.set("wages", wages).unwrap();
                    rt.set("interest", interest).unwrap();
                    rt.set("dividends", dividends).unwrap();
                    local_sum += rt.eval("federal_tax").unwrap();
                }
                local_sum
            })
            .sum();

        let elapsed = start.elapsed().as_secs_f64();
        let throughput = scenarios.len() as f64 / elapsed;
        println!(
            "  JIT scalar ({} cores):    {:>10.0} scenarios/sec  (checksum={:.2})",
            num_threads, throughput, sum
        );
        throughput
    })
}

#[cfg(feature = "jit")]
fn bench_jit_simd_sequential(graph: &Graph, scenarios: &[(f64, f64, f64)]) -> f64 {
    let compiler = JitCompiler::new().unwrap();
    let compiled = compiler.compile_batch(graph, FilingStatus::Single).unwrap();
    let scalar_compiled = compiler.compile(graph, FilingStatus::Single).unwrap();

    let start = Instant::now();
    let mut sum = 0.0;
    let mut rt = JitBatchRuntime::new(compiled, graph);

    // Process in batches of BATCH_SIZE
    let mut iter = scenarios.chunks_exact(BATCH_SIZE);

    for chunk in iter.by_ref() {
        let mut wages = [0.0; BATCH_SIZE];
        let mut interest = [0.0; BATCH_SIZE];
        let mut dividends = [0.0; BATCH_SIZE];

        for (i, &(w, int, div)) in chunk.iter().enumerate() {
            wages[i] = w;
            interest[i] = int;
            dividends[i] = div;
        }

        rt.set_batch("wages", &wages).unwrap();
        rt.set_batch("interest", &interest).unwrap();
        rt.set_batch("dividends", &dividends).unwrap();
        let results = rt.eval_batch("federal_tax").unwrap();
        sum += results.iter().sum::<f64>();
    }

    // Handle remainder with scalar JIT
    let remainder = iter.remainder();
    if !remainder.is_empty() {
        let mut scalar_rt = JitRuntime::new(scalar_compiled, graph);
        for &(w, int, div) in remainder {
            scalar_rt.set("wages", w).unwrap();
            scalar_rt.set("interest", int).unwrap();
            scalar_rt.set("dividends", div).unwrap();
            sum += scalar_rt.eval("federal_tax").unwrap();
        }
    }

    let elapsed = start.elapsed().as_secs_f64();
    let throughput = scenarios.len() as f64 / elapsed;
    println!(
        "  JIT+SIMD (1 core):        {:>10.0} scenarios/sec  (checksum={:.2})",
        throughput, sum
    );
    throughput
}

#[cfg(feature = "jit")]
fn bench_jit_simd_parallel(graph: &Graph, scenarios: &[(f64, f64, f64)], num_threads: usize) -> f64 {
    let compiler = JitCompiler::new().unwrap();

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build()
        .unwrap();

    pool.install(|| {
        let start = Instant::now();

        // Process chunks of scenarios in parallel, each chunk processed with SIMD batches
        let chunk_size = ((scenarios.len() / num_threads) / BATCH_SIZE * BATCH_SIZE).max(BATCH_SIZE * 100);

        let sum: f64 = scenarios
            .par_chunks(chunk_size)
            .map(|big_chunk| {
                let compiled = compiler.compile_batch(graph, FilingStatus::Single).unwrap();
                let mut rt = JitBatchRuntime::new(compiled, graph);
                let mut local_sum = 0.0;

                let mut iter = big_chunk.chunks_exact(BATCH_SIZE);

                for chunk in iter.by_ref() {
                    let mut wages = [0.0; BATCH_SIZE];
                    let mut interest = [0.0; BATCH_SIZE];
                    let mut dividends = [0.0; BATCH_SIZE];

                    for (i, &(w, int, div)) in chunk.iter().enumerate() {
                        wages[i] = w;
                        interest[i] = int;
                        dividends[i] = div;
                    }

                    rt.set_batch("wages", &wages).unwrap();
                    rt.set_batch("interest", &interest).unwrap();
                    rt.set_batch("dividends", &dividends).unwrap();
                    let results = rt.eval_batch("federal_tax").unwrap();
                    local_sum += results.iter().sum::<f64>();
                }

                // Handle remainder
                let remainder = iter.remainder();
                if !remainder.is_empty() {
                    let scalar_compiled = compiler.compile(graph, FilingStatus::Single).unwrap();
                    let mut scalar_rt = JitRuntime::new(scalar_compiled, graph);
                    for &(w, int, div) in remainder {
                        scalar_rt.set("wages", w).unwrap();
                        scalar_rt.set("interest", int).unwrap();
                        scalar_rt.set("dividends", div).unwrap();
                        local_sum += scalar_rt.eval("federal_tax").unwrap();
                    }
                }

                local_sum
            })
            .sum();

        let elapsed = start.elapsed().as_secs_f64();
        let throughput = scenarios.len() as f64 / elapsed;
        println!(
            "  JIT+SIMD ({} cores):      {:>10.0} scenarios/sec  (checksum={:.2})",
            num_threads, throughput, sum
        );
        throughput
    })
}

fn main() {
    println!("=== Tax Computation Throughput Benchmark ===");
    println!("Scenarios: {}", NUM_SCENARIOS);
    #[cfg(feature = "jit")]
    println!("SIMD batch size: {}", BATCH_SIZE);
    println!();

    let graph = load_demo_graph();
    let scenarios = generate_scenarios(NUM_SCENARIOS);

    let num_cpus = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    println!("Available cores: {}", num_cpus);
    println!();

    // Determine which thread counts to test
    let thread_counts: Vec<usize> = {
        let mut counts = vec![1];
        let mut n = 2;
        while n <= num_cpus {
            counts.push(n);
            n *= 2;
        }
        if *counts.last().unwrap() != num_cpus && num_cpus > 1 {
            counts.push(num_cpus);
        }
        counts
    };

    println!("--- Sequential (single-threaded) ---");
    let interp_1 = bench_interpreter_sequential(&graph, &scenarios);

    #[cfg(feature = "jit")]
    let jit_1 = bench_jit_sequential(&graph, &scenarios);

    #[cfg(feature = "jit")]
    let simd_1 = bench_jit_simd_sequential(&graph, &scenarios);

    println!();
    println!("--- Parallel scaling ---");

    let mut results: Vec<(usize, f64, f64, f64)> = Vec::new();

    for &threads in &thread_counts {
        println!("\nThreads: {}", threads);
        let interp = bench_interpreter_parallel(&graph, &scenarios, threads);

        #[cfg(feature = "jit")]
        let jit = bench_jit_parallel(&graph, &scenarios, threads);

        #[cfg(feature = "jit")]
        let simd = bench_jit_simd_parallel(&graph, &scenarios, threads);

        #[cfg(feature = "jit")]
        results.push((threads, interp, jit, simd));

        #[cfg(not(feature = "jit"))]
        results.push((threads, interp, 0.0, 0.0));
    }

    println!();
    println!("=== Summary Table ===");
    println!();
    println!(
        "{:>8} {:>15} {:>15} {:>15} {:>12} {:>12}",
        "Cores", "Interpreter", "JIT scalar", "JIT+SIMD", "JIT/Interp", "SIMD/Interp"
    );
    println!("{}", "-".repeat(85));

    for (threads, interp, jit, simd) in &results {
        let jit_speedup = jit / interp;
        let simd_speedup = simd / interp;
        println!(
            "{:>8} {:>12.0} /s {:>12.0} /s {:>12.0} /s {:>11.1}x {:>11.1}x",
            threads, interp, jit, simd, jit_speedup, simd_speedup
        );
    }

    println!();
    println!("=== Scaling Efficiency (actual/ideal) ===");
    println!();

    let base_interp = results[0].1;
    let base_jit = results[0].2;
    let base_simd = results[0].3;

    println!(
        "{:>8} {:>15} {:>15} {:>15}",
        "Cores", "Interp scale", "JIT scale", "SIMD scale"
    );
    println!("{}", "-".repeat(60));

    for (threads, interp, jit, simd) in &results {
        let interp_scale = interp / base_interp;
        let jit_scale = jit / base_jit;
        let simd_scale = simd / base_simd;
        let ideal = *threads as f64;
        println!(
            "{:>8} {:>11.2}x/{:.0}x {:>11.2}x/{:.0}x {:>11.2}x/{:.0}x",
            threads, interp_scale, ideal, jit_scale, ideal, simd_scale, ideal
        );
    }

    println!();
    println!("=== Peak Throughput ===");
    let peak_interp = results.iter().map(|r| r.1).fold(0.0f64, f64::max);
    let peak_jit = results.iter().map(|r| r.2).fold(0.0f64, f64::max);
    let peak_simd = results.iter().map(|r| r.3).fold(0.0f64, f64::max);

    println!("Interpreter: {:>12.0} scenarios/sec", peak_interp);
    println!("JIT scalar:  {:>12.0} scenarios/sec ({:.1}x vs interpreter)", peak_jit, peak_jit / peak_interp);
    println!("JIT+SIMD:    {:>12.0} scenarios/sec ({:.1}x vs interpreter)", peak_simd, peak_simd / peak_interp);
}
