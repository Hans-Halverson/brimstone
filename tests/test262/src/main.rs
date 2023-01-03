use clap::Parser;

use std::path::Path;

use brimstone_test262::{index::TestIndex, utils::GenericResult};

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
    } else {
        let index = TestIndex::load_from_file(index_path)?;
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
