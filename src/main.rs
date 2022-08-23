use rustc_formality;
use std::env;

fn main() {
    // the path to the Rust source file is specified as an argument to the program
    let args = env::args().skip(1).collect::<Vec<_>>();
    let input_path = args
        .iter()
        .find(|arg| !arg.starts_with("--"))
        .expect("Specify input path");

    println!("{}", rustc_formality::run(input_path));
}
