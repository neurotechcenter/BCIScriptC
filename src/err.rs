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

pub fn err_actor_in_actor(d: &Def) -> CError {
    match d {
        Def::Actor { name, members } => generic_err(name, "Actors cannot be defined within another actor"),
        _ => panic!("err_actor_in_actor passed wrong def type")
    }
}

pub fn err_redefinition(sig: &Signature, prev: &Signature) -> CError{
    generic_err(sig.name(), format!("Symbol is already defined in {} at line {}",
        sig.name().extra, sig.name().location_line()).as_str())
}

pub fn err_wrong_deftype(s: &Span, ex: &str) -> CError {
    generic_err(s, format!("{} was defined, but is not a {}", s, ex).as_str())
}

pub fn err_undeclared_id(id: &Span) -> CError {
    generic_err(id, format!("Undeclared identifier: symbol {} not found", id.fragment()).as_str())
}

pub fn err_unary_mismatch(op: &Span, given_type: Type, possible_sigs: &Vec<(Type, Type)>) -> CError {
    generic_err(op, &format!("cannot invoke operator {0} on operand of type {1}, possible types on which operator {0} can be used: {2}",
                            op.fragment(), given_type.bcis_rep(), 
                            possible_sigs.iter().map(|s| s.0.bcis_rep()).collect::<Vec<String>>().join(", ")))
}

pub fn err_binary_left_mismatch(op: &Span, given_type: Type, possible_sigs: &Vec<(Type, Type, Type)>) -> CError {
    generic_err(op, &format!("cannot invoke operator {0} on left-hand operand of type {1}, possible types on which operator {0} can be used: {2}",
                            op.fragment(), given_type.bcis_rep(),
                            possible_sigs.iter().map(|s| s.0.bcis_rep()).collect::<Vec<String>>().join(", ")))
}

// The tuples are references because while the other two error functions will be used on borrowed
// vecs of tuples, this will be used with a vec produced by a filter operation, so its elements
// will be references.
pub fn err_binary_right_mismatch(op: &Span, given_ltype: Type, given_rtype: Type, possible_sigs: &Vec<&(Type, Type, Type)>) -> CError {
    generic_err(op, &format!("cannot invoke operator {0} on left-hand type {1} and right-hand type {2}, \n\
                            possible right-hand types on which operator {0} can be used with left-hand type {1}: {3}",
                            op.fragment(), given_ltype.bcis_rep(), given_rtype.bcis_rep(),
                            possible_sigs.iter().map(|s| s.1.bcis_rep()).collect::<Vec<String>>().join(", ")))
}
