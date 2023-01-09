use crate::ast::*;
use crate::verify::*;

pub type CError = String;
type CWarning = String;

pub fn generic_err(id: &Span, message: &str) -> CError{
    format!("Error in file {} in line {} at position {}: \n {} \nsymbol \"{}\"\n message: {}", 
            id.extra, id.location_line(), id.get_utf8_column(),
            std::str::from_utf8(id.get_line_beginning()).unwrap_or("Invalid Text"), id.fragment(), message)
}

pub fn err_invalid_top_level_dec(d: &Def) -> CError{
    match d {
        Def::OnEvent{name, seq} => generic_err(name, "\"when\" can only be used inside an actor"),
        Def::Graphics{ .. } => String::from("graphics can only be declared in an actor"),
        Def::Sounds{ .. } => String::from("sounds can only be declared in an actor"),
        Def::Proc{..} => String::from("procedures can only be declared in an actor"),
        _ => panic!("err_invalid_top_level_dec passed wrong def type")
    } 
}

pub fn err_invalid_actor_level_dec(d: &Def) -> CError{
    match d {
        Def::State{name, statetype} => generic_err(name, "States cannot be defined within actors"),
        Def::Actor { name, members } => generic_err(name, "Actors cannot be defined within another actor"),
        Def::StateEvent { name } => generic_err(name, "State Events cannot be defined within an actor"),
        _ => panic!("err_invalid_actor_level_dec passed wrong def type")
    }
}



pub fn err_redefinition(sig: &Signature, prev: &Signature) -> CError{
    generic_err(sig.name(), format!("Symbol is already defined in {} at line {}",
        sig.name().extra, sig.name().location_line()).as_str())
}

pub fn err_redefinition1(name: &Id, prev: &Signature) -> CError{
    generic_err(name, format!("Symbol is already defined in {} at line {}",
        prev.name().extra, prev.name().location_line()).as_str())
}

pub fn err_wrong_deftype(s: &Span, ex: &str) -> CError {
    generic_err(s, format!("{} was defined, but is not a {}", s, ex).as_str())
}

pub fn err_undeclared_id(id: &Span, ex: &str) -> CError {
    generic_err(id, format!("Undeclared identifier: symbol {} not found, or it is not a {}", id.fragment(), ex).as_str())
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

pub fn err_circular_def(id: &Span, sequence: &Vec<&Id>) -> CError {
    generic_err(id, &format!("Circular definition: this symbol is defined based on the value of a var/func which references it, called at {} {}",
                             sequence.iter()
                             .map(|id| format!("{}, (line {}, col {} in {})\n called by", id.fragment(), id.location_line(), id.get_utf8_column(), id.extra))
                             .collect::<Vec<String>>().join(""),
                             id.fragment()))
}

// The tuples are references because while the other two error functions will be used on borrowed
// vecs of tuples, this will be used with a vec produced by a filter operation, so its elements
// will be references.
pub fn err_binary_right_mismatch<'a, I>(op: &Span, given_ltype: Type, given_rtype: Type, possible_sigs: &I) -> CError
    where I: Iterator<Item = &'a (Type, Type, Type)>
{
    generic_err(op, &format!("cannot invoke operator {0} on left-hand type {1} and right-hand type {2}, \n\
                            possible right-hand types on which operator {0} can be used with left-hand type {1}: {3}",
                            op.fragment(), given_ltype.bcis_rep(), given_rtype.bcis_rep(),
                            possible_sigs.map(|s| s.1.bcis_rep()).collect::<Vec<String>>().join(", ")))
}
