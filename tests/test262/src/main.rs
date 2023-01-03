use clap::Parser;

use std::path::Path;

use brimstone_test262::{index::TestIndex, runner::TestRunner, utils::GenericResult};

#[derive(Parser)]
#[command(about)]
struct Args {
    /// Path to the test262 repo
    #[arg(long, default_value_t = String::from("test262"))]
    test262_path: String,

    /// Path to the test index
    #[arg(long, default_value_t = String::from("test_index.json"))]
    index_path: String,

    /// Reindex the test262 test suite
    #[arg(long, default_value_t = false)]
    reindex: bool,

    /// Number of threads to use in test runner
    #[arg(short, long, default_value_t = 8)]
    threads: u8,

    /// Print extra information to console while running tests
    #[arg(short, long, default_value_t = false)]
    verbose: bool,

    /// Optional path to write result files to. Result files will have the names
    /// <path>_success.json and <path>_failure.json.
    #[arg(long)]
    save_result_files: Option<String>,

    /// Only run tests that match this feature
    #[arg(long)]
    feature: Option<String>,

    /// Only run tests that match this filter string
    filter: Option<String>,
}

fn main_impl() -> GenericResult {
    let args = Args::parse();

    let test262_root = Path::new(&args.test262_path).canonicalize().unwrap();
    let index_path = Path::new(&args.index_path);

    if args.reindex {
        println!("Indexing test262 test suite...");
        let index = TestIndex::new(&test262_root)?;

        println!("Finished indexing. Writing index to file.");
        index.write_to_file(index_path)?;

        return Ok(());
    }

    let index = TestIndex::load_from_file(index_path)?;
    let mut runner = TestRunner::new(index, args.threads, args.filter, args.feature);
    let results = runner.run(args.verbose);

    results.print_to_console();

    if let Some(result_files_path) = args.save_result_files {
        results.save_to_result_files(result_files_path)?;
    }

    if !results.is_successful() {
        std::process::exit(1);
    }

    Ok(())
}

/// Wrapper to pretty print errors
fn main() -> () {
    match main_impl() {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
