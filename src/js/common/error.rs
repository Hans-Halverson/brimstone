pub fn print_error_message_and_exit(message: &str) {
    eprintln!("{}", message);
    std::process::exit(1);
}
