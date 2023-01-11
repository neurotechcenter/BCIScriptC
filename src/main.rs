mod parse;
mod ast;
mod err;
mod verify;
mod builtins;
mod generate;

use std::io::Write;

fn main() -> Result<(),Box<dyn std::error::Error>> {
    let files = std::env::args();
    let program: ast::Program = Vec::new();
    for file in files {
        let filetext = String::from_utf8_lossy(&std::fs::read(file)?);
        let filespan = ast::Span::new_extra(&filetext, file);
        program.append(&mut parse::program(filespan)?);
    }
    verify::verify(&mut program).map_err(|e| simple_error::simple_error!(e.join("\n")));
    let program_out = generate::generate_program(program);
    let mut outfile = std::fs::File::create("AppInitPartial.cpp")?;
    outfile.write_all(program_out.as_bytes());
    Ok(())
}
