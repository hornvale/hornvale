//! Binary entry point; delegates to [`plumb::run`].

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    std::process::exit(plumb::run(&args));
}
