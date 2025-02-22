use std::rc::Rc;

use brimstone::js::{
    common::options::OptionsBuilder,
    parser::{analyze::analyze, parse_script, parser::ParseProgramResult, source::Source},
    runtime::{
        bytecode::generator::{BytecodeProgramGenerator, BytecodeScript},
        Context, ContextBuilder, EvalResult,
    },
};
use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

fn setup_step(file: &str) -> (Context, Rc<Source>) {
    // Use a 10 MB heap size
    let options = OptionsBuilder::new()
        .min_heap_size(10 * 1024 * 1024)
        .build();
    let cx = ContextBuilder::new().set_options(Rc::new(options)).build();
    let source = Rc::new(Source::new_from_file(&format!("benches/{}", file)).unwrap());
    (cx, source)
}

fn parse_step((cx, source): (Context, Rc<Source>)) -> (Context, ParseProgramResult) {
    let parse_result = parse_script(&source, cx.options.as_ref()).unwrap();
    (cx, parse_result)
}

fn analyze_step(
    (cx, mut parse_result): (Context, ParseProgramResult),
) -> (Context, ParseProgramResult) {
    analyze(&mut parse_result).unwrap();
    (cx, parse_result)
}

fn generate_step((cx, parse_result): (Context, ParseProgramResult)) -> (Context, BytecodeScript) {
    let bytecode_program = BytecodeProgramGenerator::generate_from_parse_script_result(
        cx,
        &parse_result,
        cx.initial_realm(),
    )
    .unwrap();
    (cx, bytecode_program)
}

fn execute_step((mut cx, bytecode_script): (Context, BytecodeScript)) -> EvalResult<()> {
    cx.run_script(bytecode_script)
}

/// Benchmark all phases of program execution.
/// - Parse
/// - Analyze
/// - Bytecode generation
/// - Bytecode execution
fn bench_program_all_steps(c: &mut Criterion, file: &str) {
    // Isolate parser phase
    c.bench_function(&format!("{} > parse", file), |b| {
        b.iter_batched(|| setup_step(file), parse_step, BatchSize::PerIteration)
    });

    // Isolate analysis phase
    c.bench_function(&format!("{} > analyze", file), |b| {
        b.iter_batched(
            || {
                let setup_result = setup_step(file);
                parse_step(setup_result)
            },
            analyze_step,
            BatchSize::PerIteration,
        )
    });

    // Isolate bytecode generation phase
    c.bench_function(&format!("{} > generate", file), |b| {
        b.iter_batched(
            || {
                let setup_result = setup_step(file);
                let parse_result = parse_step(setup_result);
                analyze_step(parse_result)
            },
            generate_step,
            BatchSize::PerIteration,
        );
    });

    // Isolate bytecode generation phase
    c.bench_function(&format!("{} > execute", file), |b| {
        b.iter_batched(
            || {
                let setup_result = setup_step(file);
                let parse_result = parse_step(setup_result);
                let analyzed_result = analyze_step(parse_result);
                generate_step(analyzed_result)
            },
            execute_step,
            BatchSize::PerIteration,
        );
    });
}

/// Benchmark context creation.
fn context_benches(c: &mut Criterion) {
    c.bench_function("context creation", |b| {
        b.iter_batched(|| {}, |_| Context::default(), BatchSize::PerIteration);
    });
}

pub fn program_benches(c: &mut Criterion) {
    bench_program_all_steps(c, "empty.js");
}

criterion_group!(context, context_benches);
criterion_group!(program, program_benches);
criterion_main!(context, program);
