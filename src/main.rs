mod parse;
mod ast;
mod err;
mod verify;
mod builtins;
mod generate;

use std::io::Write;


fn main() -> Result<(),Box<dyn std::error::Error>> {
    let files = std::env::args();

    let mut filebytes: Vec<(String, Vec<u8>)> = Vec::new();
    for file in files {
        filebytes.push((file.clone(), std::fs::read(&file)?));
    }
    let mut filespans: Vec<parse::Span> = Vec::new();
    for (filename, bytes) in &filebytes {
        filespans.push(parse::Span::new_extra(std::str::from_utf8(&bytes)?, filename.clone()));
    }
    let mut program: ast::Program = Vec::new();
    for span in filespans {
        program.append(&mut parse::program(span)?);
    }
    verify::verify(&mut program).map_err(|e| simple_error::simple_error!(e.join("\n")));
    let program_out = generate::generate_program(program);
    let mut outfile = std::fs::File::create("AppInitPartial.cpp")?;
    outfile.write_all(program_out.as_bytes());
    Ok(())
}
