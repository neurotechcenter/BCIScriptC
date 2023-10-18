mod lex;
mod tests;



#[allow(dead_code)] // remove after done
                    //
fn main() -> Result<(),Box<dyn std::error::Error>> {

    let tokens = lex::lex("ab cb cd/*aaa*/");

    tokens.for_each(|t| println!("{:?}", t));
    
    Ok(())


    /*
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
        program.append(&mut parse::program(span).map_err(
         |e| Box::new(simple_error::simple_error!(e.to_string()))
            )?
        );
    }
    verify::verify(&mut program).map_err(|e| simple_error::simple_error!(e.join("\n")));
    let program_out = generate::generate_program(program);
    let mut outfile = std::fs::File::create("AppInitPartial.cpp")?;
    outfile.write_all(program_out.as_bytes());
    Ok(())
    */
}
