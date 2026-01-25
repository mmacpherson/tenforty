use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use graphlib::{eval::Runtime, graph::{FilingStatus, Graph}};

#[cfg(feature = "jit")]
use graphlib::jit::{JitBatchRuntime, JitCompiler, JitRuntime, BATCH_SIZE};

fn load_demo_graph() -> Graph {
    let json = include_str!("../../src/tenforty/forms/us_1040_simple.json");
    Graph::from_json(json).unwrap()
}

fn bench_interpreter(c: &mut Criterion) {
    let graph = load_demo_graph();

    c.bench_function("interpreter/single_eval", |b| {
        let mut rt = Runtime::new(&graph, FilingStatus::Single);
        rt.set("wages", 75000.0).unwrap();
        rt.set("interest", 500.0).unwrap();
        rt.set("dividends", 1000.0).unwrap();

        b.iter(|| {
            rt.set("wages", black_box(75000.0)).unwrap();
            black_box(rt.eval("federal_tax").unwrap())
        })
    });
}

#[cfg(feature = "jit")]
fn bench_jit(c: &mut Criterion) {
    let graph = load_demo_graph();
    let compiler = JitCompiler::new().unwrap();

    c.bench_function("jit/compile", |b| {
        b.iter(|| {
            black_box(compiler.compile(&graph, FilingStatus::Single).unwrap())
        })
    });

    c.bench_function("jit/compile_batch", |b| {
        b.iter(|| {
            black_box(compiler.compile_batch(&graph, FilingStatus::Single).unwrap())
        })
    });

    c.bench_function("jit/single_eval", |b| {
        let mut jit_rt = JitRuntime::new(
            compiler.compile(&graph, FilingStatus::Single).unwrap(),
            &graph,
        );
        jit_rt.set("wages", 75000.0).unwrap();
        jit_rt.set("interest", 500.0).unwrap();
        jit_rt.set("dividends", 1000.0).unwrap();

        b.iter(|| {
            jit_rt.set("wages", black_box(75000.0)).unwrap();
            black_box(jit_rt.eval("federal_tax").unwrap())
        })
    });

    c.bench_function("jit/batch_eval_2", |b| {
        let mut batch_rt = JitBatchRuntime::new(
            compiler.compile_batch(&graph, FilingStatus::Single).unwrap(),
            &graph,
        );
        batch_rt.set_batch("wages", &[50000.0, 100000.0]).unwrap();
        batch_rt.set_batch("interest", &[500.0; BATCH_SIZE]).unwrap();
        batch_rt.set_batch("dividends", &[1000.0; BATCH_SIZE]).unwrap();

        b.iter(|| {
            batch_rt.set_batch("wages", black_box(&[50000.0, 100000.0])).unwrap();
            black_box(batch_rt.eval_batch("federal_tax").unwrap())
        })
    });

    c.bench_function("jit/scalar_2x_sequential", |b| {
        let incomes = [50000.0, 100000.0];
        let mut jit_rt = JitRuntime::new(
            compiler.compile(&graph, FilingStatus::Single).unwrap(),
            &graph,
        );
        jit_rt.set("wages", 75000.0).unwrap();
        jit_rt.set("interest", 500.0).unwrap();
        jit_rt.set("dividends", 1000.0).unwrap();

        b.iter(|| {
            let mut results = [0.0; BATCH_SIZE];
            for (i, &income) in incomes.iter().enumerate() {
                jit_rt.set("wages", black_box(income)).unwrap();
                results[i] = jit_rt.eval("federal_tax").unwrap();
            }
            black_box(results)
        })
    });
}

fn bench_comparison(c: &mut Criterion) {
    let graph = load_demo_graph();

    let mut group = c.benchmark_group("eval_comparison");

    for income in [10000.0, 50000.0, 100000.0, 500000.0] {
        group.bench_with_input(
            BenchmarkId::new("interpreter", income),
            &income,
            |b, &income| {
                let mut rt = Runtime::new(&graph, FilingStatus::Single);
                rt.set("wages", income).unwrap();
                rt.set("interest", 500.0).unwrap();
                rt.set("dividends", 1000.0).unwrap();

                b.iter(|| {
                    rt.set("wages", black_box(income)).unwrap();
                    black_box(rt.eval("federal_tax").unwrap())
                })
            },
        );

        #[cfg(feature = "jit")]
        {
            let compiler = JitCompiler::new().unwrap();
            group.bench_with_input(
                BenchmarkId::new("jit", income),
                &income,
                |b, &income| {
                    let mut jit_rt = JitRuntime::new(
                        compiler.compile(&graph, FilingStatus::Single).unwrap(),
                        &graph,
                    );
                    jit_rt.set("wages", income).unwrap();
                    jit_rt.set("interest", 500.0).unwrap();
                    jit_rt.set("dividends", 1000.0).unwrap();

                    b.iter(|| {
                        jit_rt.set("wages", black_box(income)).unwrap();
                        black_box(jit_rt.eval("federal_tax").unwrap())
                    })
                },
            );
        }
    }

    group.finish();
}

#[cfg(feature = "jit")]
criterion_group!(benches, bench_interpreter, bench_jit, bench_comparison);

#[cfg(not(feature = "jit"))]
criterion_group!(benches, bench_interpreter, bench_comparison);

criterion_main!(benches);
