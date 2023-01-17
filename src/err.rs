use crate::ast::*;
use crate::verify::*;

pub type CError = String;
type CWarning = String;

pub fn generic_err(id: &Token, message: &str) -> CError{
    format!("Error in file {} in line {} at position {}: \n {} \nsymbol \"{}\"\n message: {}", 
            id.file, id.position.location_line(), id.position.get_utf8_column(),
            std::str::from_utf8(id.position.get_line_beginning()).unwrap_or("Invalid Text"), id.content, message)
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
        sig.name().file, sig.name().position.location_line()).as_str())
}

pub fn err_redefinition1(name: &Id, prev: &Signature) -> CError{
    generic_err(name, format!("Symbol is already defined in {} at line {}",
        prev.name().file, prev.name().position.location_line()).as_str())
}

pub fn err_wrong_deftype(s: &Token, ex: &str) -> CError {
    generic_err(s, format!("{} was defined, but is not a {}", s.content, ex).as_str())
}

pub fn err_undeclared_id(id: &Token, ex: &str) -> CError {
    generic_err(id, format!("Undeclared identifier: symbol {} not found, or it is not a {}", id.content, ex).as_str())
}

pub fn err_unary_mismatch(op: &Token, given_type: Type, possible_sigs: &Vec<(Type, Type)>) -> CError {
    generic_err(op, &format!("cannot invoke operator {0} on operand of type {1}, possible types on which operator {0} can be used: {2}",
                            op.content, given_type.bcis_rep(), 
                            possible_sigs.iter().map(|s| s.0.bcis_rep()).collect::<Vec<String>>().join(", ")))
}

pub fn err_binary_left_mismatch(op: &Token, given_type: Type, possible_sigs: &Vec<(Type, Type, Type)>) -> CError {
    generic_err(op, &format!("cannot invoke operator {0} on left-hand operand of type {1}, possible types on which operator {0} can be used: {2}",
                            op.content, given_type.bcis_rep(),
                            possible_sigs.iter().map(|s| s.0.bcis_rep()).collect::<Vec<String>>().join(", ")))
}

pub fn err_circular_def(id: &Token, sequence: &Vec<&Id>) -> CError {
    generic_err(id, &format!("Circular definition: this symbol is defined based on the value of a var/func which references it, called at {} {}",
                             sequence.iter()
                             .map(|id| format!("{}, (line {}, col {} in {})\n called by", id.content, id.position.location_line(), id.position.get_utf8_column(), id.file))
                             .collect::<Vec<String>>().join(""),
                             id.content))
}

// The tuples are references because while the other two error functions will be used on borrowed
// vecs of tuples, this will be used with a vec produced by a filter operation, so its elements
// will be references.
pub fn err_binary_right_mismatch<'a, I>(op: &Token, given_ltype: Type, given_rtype: Type, possible_sigs: &I) -> CError
    where I: Iterator<Item = &'a (Type, Type, Type)>
{
    generic_err(op, &format!("cannot invoke operator {0} on left-hand type {1} and right-hand type {2}, \n\
                            possible right-hand types on which operator {0} can be used with left-hand type {1}: {3}",
                            op.content, given_ltype.bcis_rep(), given_rtype.bcis_rep(),
                            possible_sigs.map(|s| s.1.bcis_rep()).collect::<Vec<String>>().join(", ")))
}
