use crate::ast::*;
use crate::verify::*;

pub type CError = String;
type CWarning = String;

fn generic_err(id: &Span, message: &str) -> CError{
    format!("Error in file {} in line {} at position {}: \n {} \nsymbol \"{}\"\n message: {}", 
            id.extra, id.location_line(), id.get_utf8_column(),
            std::str::from_utf8(id.get_line_beginning()).unwrap_or("Invalid Text"), id.fragment(), message)
}

pub fn err_invalid_top_level_dec(d: &Def) -> CError{
    match d {
        Def::OnEvent{name, seq} => generic_err(name, "\"when\" can only be used inside an actor"),
        Def::Graphics{ .. } => String::from("graphics can only be declared in an actor"),
        Def::Sounds{ .. } => String::from("sounds can only be declared in an actor"),
        _ => panic!("err_invalid_top_level_dec passed wrong def type")
    } 
}

pub fn err_state_in_actor(d: &Def) -> CError{
    match d {
        Def::State{name, statetype} => generic_err(name, "States cannot be defined within actors"),
        _ => panic!("err_state_in_actor passed wrong def type")
    }
}

pub fn err_redefinition(sig: &Signature, prev: &Signature) -> CError{
    generic_err(sig.name(), format!("Symbol is already defined in {} at line {}",
        sig.name().extra, sig.name().location_line()).as_str())
}
