//! Binary entry point; delegates to [`type_audit::run`].

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    std::process::exit(type_audit::run(&args));
}
