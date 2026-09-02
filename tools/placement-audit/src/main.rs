//! Binary entry point; delegates to [`placement_audit::run`].

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    std::process::exit(placement_audit::run(&args));
}
