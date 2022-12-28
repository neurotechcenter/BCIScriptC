mod parse;
mod ast;
mod err;
mod verify;
mod builtins;
mod generate;

fn main() -> Result<(),Box<dyn std::error::Error>> {
    let files = std::env::args();
    let filetext = String::new();
    for file in files {
        filetext.push_str(&String::from_utf8_lossy(&std::fs::read(file)?));
    }
    let program_parsed = parse::program(filetext)?;
    Ok(())
}
