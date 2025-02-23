use std::{
    rc::Rc,
    time::{Duration, Instant},
};

use brimstone::js::{
    common::options::OptionsBuilder,
    parser::{analyze::analyze, parse_script, parser::ParseProgramResult, source::Source},
    runtime::{
        bytecode::generator::{BytecodeProgramGenerator, BytecodeScript},
        Context, ContextBuilder, EvalResult,
    },
};
use criterion::{criterion_group, criterion_main, Criterion};

/// A test with a setup, routine, and cleanup phase. Only the routine phase is measured.
pub fn isolated_test<I, O, S, R, C>(
    c: &mut Criterion,
    name: &str,
    mut setup: S,
    mut routine: R,
    mut cleanup: C,
) where
    S: FnMut() -> I,
    R: FnMut(I) -> O,
    C: FnMut(O),
{
    c.bench_function(name, |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;

            for _ in 0..iters {
                // Perform setup for the test
                let input = setup();

                // Only measure the time taken by the routine
                let start = Instant::now();
                let output = routine(input);
                total += start.elapsed();

                cleanup(output);
            }

            total
        });
    });
}

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

fn execute_step((mut cx, bytecode_script): (Context, BytecodeScript)) -> (Context, EvalResult<()>) {
    let result = cx.run_script(bytecode_script);
    (cx, result)
}

fn cleanup_step<T>((cx, _): (Context, T)) {
    cx.drop();
}

/// Benchmark all phases of program execution.
/// - Parse
/// - Analyze
/// - Bytecode generation
/// - Bytecode execution
fn bench_program_all_steps(c: &mut Criterion, file: &str) {
    // Isolate parser phase
    isolated_test(c, &format!("{} > parse", file), || setup_step(file), parse_step, cleanup_step);

    // Isolate analysis phase
    isolated_test(
        c,
        &format!("{} > analyze", file),
        || {
            let setup_result = setup_step(file);
            parse_step(setup_result)
        },
        |(cx, parse_result)| analyze_step((cx, parse_result)),
        cleanup_step,
    );

    // Isolate bytecode generation phase
    isolated_test(
        c,
        &format!("{} > generate", file),
        || {
            let setup_result = setup_step(file);
            let parse_result = parse_step(setup_result);
            analyze_step(parse_result)
        },
        generate_step,
        cleanup_step,
    );

    // Isolate bytecode generation phase
    isolated_test(
        c,
        &format!("{} > execute", file),
        || {
            let setup_result = setup_step(file);
            let parse_result = parse_step(setup_result);
            let analyzed_result = analyze_step(parse_result);
            generate_step(analyzed_result)
        },
        execute_step,
        cleanup_step,
    );
}

/// Benchmark context creation.
fn context_benches(c: &mut Criterion) {
    isolated_test(c, "context creation", || {}, |_| (Context::default(), ()), cleanup_step);
}

pub fn program_benches(c: &mut Criterion) {
    bench_program_all_steps(c, "empty.js");
}

criterion_group!(context, context_benches);
criterion_group!(program, program_benches);
criterion_main!(context, program);
