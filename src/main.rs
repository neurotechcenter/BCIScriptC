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
    let program = parse::program(nom_locate::LocatedSpan::from(filetext.as_str()))?;
    verify::verify(&mut program).map_err(|e| simple_error::simple_error!(e.join("\n")));
    let program_out = generate::generate_program(program);
    Ok(())
}
