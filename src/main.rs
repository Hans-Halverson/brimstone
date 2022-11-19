mod js;

use std::env;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
enum CLIError {
    Usage,
}

impl Error for CLIError {}

impl fmt::Display for CLIError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CLIError::Usage => f.write_str(USAGE_MESSAGE),
        }
    }
}

const USAGE_MESSAGE: &str = "Usage: <FILENAME>";

fn main_impl() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(Box::new(CLIError::Usage));
    }

    let source = js::source::Source::new(&args[1])?;
    let ast = js::parser::parse_file(&source)?;
    println!("{}", js::printer::print_program(&ast, &source));

    return Ok(());
}

/// Wrapper to pretty print errors
fn main() -> () {
    match main_impl() {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    }
}
