use std::{
    rc::Rc,
    time::{Duration, Instant},
};

use bitflags::bitflags;
use brimstone::js::{
    common::options::OptionsBuilder,
    parser::{
        analyze::analyze, parse_module, parse_script, parser::ParseProgramResult, source::Source,
    },
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

bitflags! {
    #[derive(Clone, Copy)]
    struct TestFlags: u8 {
        const ANNEX_B = 1 << 0;
        const MODULE = 1 << 1;
    }
}

fn setup_step(file: &str, flags: TestFlags) -> (Context, Rc<Source>) {
    // Use a 10 MB heap size
    let options = OptionsBuilder::new()
        .annex_b(flags.contains(TestFlags::ANNEX_B))
        .min_heap_size(10 * 1024 * 1024)
        .build();
    let cx = ContextBuilder::new().set_options(Rc::new(options)).build();
    let source = Rc::new(Source::new_from_file(&format!("benches/{}", file)).unwrap());
    (cx, source)
}

fn parse_step(
    (cx, source): (Context, Rc<Source>),
    flags: TestFlags,
) -> (Context, ParseProgramResult) {
    let parse_result = if flags.contains(TestFlags::MODULE) {
        parse_module(&source, cx.options.clone()).unwrap()
    } else {
        parse_script(&source, cx.options.clone()).unwrap()
    };

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

/// Benchmark just the parsing phase.
fn bench_program_parser(c: &mut Criterion, file: &str, flags: TestFlags) {
    isolated_test(
        c,
        &format!("{} > parse", file),
        || setup_step(file, flags),
        |input| parse_step(input, flags),
        cleanup_step,
    );
}

/// Benchmark all phases of program execution.
/// - Parse
/// - Analyze
/// - Bytecode generation
/// - Bytecode execution
fn bench_program_all_steps(c: &mut Criterion, file: &str, flags: TestFlags) {
    bench_program_parser(c, file, flags);

    // Isolate analysis phase
    isolated_test(
        c,
        &format!("{} > analyze", file),
        || {
            let setup_result = setup_step(file, flags);
            parse_step(setup_result, flags)
        },
        |(cx, parse_result)| analyze_step((cx, parse_result)),
        cleanup_step,
    );

    // Isolate bytecode generation phase
    isolated_test(
        c,
        &format!("{} > generate", file),
        || {
            let setup_result = setup_step(file, flags);
            let parse_result = parse_step(setup_result, flags);
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
            let setup_result = setup_step(file, flags);
            let parse_result = parse_step(setup_result, flags);
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
    bench_program_all_steps(c, "empty.js", TestFlags::empty());
    bench_program_parser(c, "fixtures/acorn.js", TestFlags::empty());
    bench_program_parser(c, "fixtures/react.js", TestFlags::empty());
    bench_program_parser(c, "fixtures/typescript.js", TestFlags::empty());
}

criterion_group!(context, context_benches);
criterion_group!(program, program_benches);
criterion_main!(context, program);
