use std::collections::{LinkedList, HashMap};

use crate::ast::*;
use crate::builtins::{UNARY_OPERATORS, BINARY_OPERATORS};
use crate::err::*;
use crate::builtins::BUILTINS;

/*
 * Functions for verifying the validity of a program.
 *
 * Any code errors will be returned in a vector. 
 *
 * Any calls to unwrap or panic in this module are due to errors in the parser
 * or other BCIScriptC code, and should be treated as bugs with BCIScriptC.
 */

#[derive(Clone)]
pub enum Signature<'a>{
    Actor{name: &'a Id<'a>, locals: Signatures<'a>},
    OnEvent{name: &'a Id<'a>},
    Proc{name: &'a Id<'a>, args: &'a Vec<Type> },
    Func{name: &'a Id<'a>, args: &'a Vec<Type>, rettype: Type, referenced_symbols: Vec<(&'a Id<'a>, SymbolType)> },
    Event{name: &'a Id<'a>},
    State{name: &'a Id<'a>, statetype: &'a StateType},
    Var{name: &'a Id<'a>, vartype: Type, referenced_symbols: Vec<(&'a Id<'a>, SymbolType)>},
    StateEvent{name: &'a Id<'a>},
    Timer{name: &'a Id<'a>},
}

impl Signature<'_> {
    pub fn name(&self) -> &Span{
        match self {
            Signature::Actor{name, ..} => name,
            Signature::OnEvent{name} => name,
            Signature::Proc{name, ..} => name,
            Signature::Func{name, ..} => name,
            Signature::Event{name} => name,
            Signature::State{name, ..} => name,
            Signature::Var{name, ..} => name,
            Signature::StateEvent { name } => name,
            Signature::Timer { name } => name,
        }
    }
    pub fn str(&self) -> String {
        String::from(*self.name().fragment())
    }
}

impl PartialEq for Signature<'_>{
    fn eq(&self, other: &Signature) -> bool {
        self.name().fragment() == other.name().fragment()  
    } 
}

impl Eq for Signature<'_> {}

#[derive(Clone, Copy, PartialEq)]
enum SymbolType {
    Func,
    Var
}

pub fn verify(p: Program) -> Result<(), Vec<CError>> {
    let signatures = verify_declarations(p)?;
    verify_definitions(p, signatures)
}

type Signatures<'a> = HashMap<String, Signature<'a>>;

/**")
 * Verifies global declarations, collects their signatures into a vector
 * Also verifies assignment expressions of variables, as they need to be typed before use
 * Will return errors if declarations are found which can only be inside an actor,
 * (OnEvent, Graphics, Sounds) are found outside an actor, or if a symbol is defined more than
 * once.
 */
fn verify_declarations<'a>(p: Program) -> Result<Signatures<'a>, Vec<CError>> {
    let errs_from_onevent = p.into_iter()
        .filter(|d| match d { Def::OnEvent{..} | Def::Graphics{..} | Def::Sounds{..} | Def::Proc { .. } => true, _ => false })
        .map(|d| err_invalid_top_level_dec(&d))
        .collect::<Vec<CError>>();
    if errs_from_onevent.len() > 0{
        return Err(errs_from_onevent);
    }

    let mut signatures: Signatures = BUILTINS.clone().into_iter().map(|s| (s.str(), s)).collect(); //builtin signatures
    let mut errs: Vec<CError> = Vec::new();

    for def in p {
        /***
         * I would prefer to use map here, but since each call to verify_declaration depends
         * on the accumulation of Signatures returned from each previous call, it takes
         * some serious functional/combinatorial wizardry to implement that as a pure map function,
         * (i previously implemented this in Haskell)
         * so instead signatures and errors are collected externally.
         */
        let sig = verify_declaration(&def, &signatures);
        match sig {
            Ok(s) => match signatures.insert(s.str(), s) {None => (), Some(_) => panic!("attempted to add duplicate to signatures")},
            Err(e) => errs.append(&mut e)
        } 
    }
    
    errs_from_onevent.append(&mut errs);
    if errs_from_onevent.len() > 0{
        return Err(errs_from_onevent);
    }
    Ok(signatures)
}

fn verify_declaration<'a>(d: &Def<'a>, sigs: &Signatures) -> Result<Signature<'a>, Vec<CError>> {
    match d {
        Def::Actor{name, members}               => verify_dec_actor(name, &members, sigs),
        Def::Func{name, args, rettype, expr}    => not_already_declared(&name, sigs).map(
            |a| Signature::Func{name, args: &args.iter().map(|a| a.argtype).collect::<Vec<Type>>(), rettype: *rettype, referenced_symbols: get_referenced_symbols(&expr)}),
        Def::Event{name}                        => not_already_declared(&name, sigs).map(|_| Signature::Event { name }),
        Def::StateEvent { name }                => not_already_declared(&name, sigs).map(|_| Signature::StateEvent{name}),
        Def::Timer { name }                     => not_already_declared(&name, sigs).map(|_| Signature::Timer{name}),
        Def::State{name, statetype}             => not_already_declared(&name, sigs).map(|_| Signature::State{name, statetype}),
        Def::Var{name, vartype, value}          => verify_dec_variable(&name, &vartype, &value, sigs),
        _ => panic!("verify_declaration given invalid declaration type")
    }
}

fn not_already_declared(name: &Id, sigs: &Signatures) -> Result<(), Vec<CError>> {
    match sigs.get(&name.fragment().to_string()) {
        Some(s) => Err(vec!(err_redefinition1(name, s))),
        None => Ok(())
    }
}

fn verify_dec_variable<'a>(name: &Id, vartype: &Option<Type>, expr: &Option<Expr>, sigs: &Signatures) -> Result<Signature<'a>, Vec<CError>>{
    not_already_declared(name, sigs)?;
    match vartype {
        Some(t) => Ok(Signature::Var { name, vartype: *t, referenced_symbols:
            match expr {
                Some(expr) => get_referenced_symbols(expr),
                None => Vec::new()
                }
        }),
        None => Err(vec!(generic_err(name, "Member variables must have an explicit type")))
    }
}

fn get_referenced_symbols<'a>(expr: &Expr<'a>) -> Vec<(&'a Id<'a>, SymbolType)> {
    fn get_ref_symb1<'b>(expr: &UExpr) -> Vec<(&'b Id<'b>, SymbolType)> {
        match expr {
            UExpr::BinExpr { l, op, r } => {let mut lref = get_referenced_symbols(l); lref.append(&mut get_referenced_symbols(r)); lref},
            UExpr::UnExpr { op, expr } => get_referenced_symbols(expr),
            UExpr::Value(v) => match v {
                Value::Literal(_) => Vec::new(),
                Value::VarCall(v) => vec!((v, SymbolType::Var)),
                Value::FuncCall(f) => vec!((&f.id, SymbolType::Func))
            }
        }
    }
    get_ref_symb1(&expr.expr)
}

fn verify_dec_actor<'a>(name: &Id, members: &Vec<Def>, sigs: &Signatures) -> Result<Signature<'a>, Vec<CError>>{
    let mut errs: Vec<CError> = members.iter()
       .filter(|d| match d {Def::State{..} | Def::Actor {..} | Def::StateEvent{..} => true, _ => false})
       .map(err_invalid_actor_level_dec)
       .collect::<Vec<CError>>();    

    let mut local_sigs: Signatures = HashMap::new();

    if errs.len() > 0 {
        return Err(errs);
    }

    for member in members { 
            let s = ver_actor_dec(member, &local_sigs);
            match s {
                Some(sig) => match sig {
                    Ok(r) => match local_sigs.insert(r.str(), r) {
                        None => (),
                        Some(s) => panic!("attempted to add duplicate signature to local_sigs. This should not happen as a check for duplicates should have already been done.")
                    },
                    Err(e) => errs.append(&mut e)     
                },
                None => ()
            }
    }

    let sig = Signature::Actor{name, locals: local_sigs};
    errs.append(&mut get_redef_err(&sig, sigs));
    if errs.len() > 0 {
        return Err(errs);
    }
    return Ok(sig);
}

//takes local signatures, there can be the same symbol defined in different nested namespaces.
fn ver_actor_dec<'a>(def: &Def, sigs: &Signatures) -> Option<Result<Signature<'a>, Vec<CError>>> {
    match def {
        Def::Func{name, args, rettype, expr}    => Some(not_already_declared(&name, sigs).map(
            |a| Signature::Func{name, args: &args.iter().map(|a| a.argtype).collect::<Vec<Type>>(), rettype: *rettype, referenced_symbols: get_referenced_symbols(&expr)})),
        Def::Event{name}                        => Some(not_already_declared(&name, sigs).map(|_| Signature::Event { name })),
        Def::Timer { name }                     => Some(not_already_declared(&name, sigs).map(|_| Signature::Timer{name})),
        Def::Var{name, vartype, value}          => Some(verify_dec_variable(&name, &vartype, &value, sigs)),
        Def::Proc { name, args, seq }           => Some(not_already_declared(&name, sigs).map(|_| Signature::Proc { name,  args: &args.iter().map(|ad| ad.argtype).collect::<Vec<Type>>() })),
        Def::Graphics { files }                 => None,
        Def::Sounds { files }                   => None,
        _ => panic!("verify_declaration given invalid declaration type")
    } 
}

fn get_redef_err(sig: &Signature, sigs: &Signatures) -> Vec<CError> 
{
    let v: Vec<CError> = Vec::new();
    let prev = sigs.get(&sig.str());
    match prev {
        None => (),
        Some(p) => v.push(err_redefinition(&sig, &p))
    }
    return v;
}

fn verify_definitions(p: &mut Program, sigs: &Signatures) -> Result<(), Vec<CError>>  {
    let mut errs = p.iter_mut().filter(|d| match d {Def::Actor{..} => false, _ => true})
        .map(|d| verify_definition(d, vec!(sigs)))
        .fold(Vec::new(), |l, r| {l.append(&mut match r.err() {Some(v) => v, None => Vec::new()}); return l;});
    //Actors must be verified last because of how the circular definition check works (this is
    //because all expressions must be verified before any circular definition checking
    //happens.)
    let mut errsact = p.iter_mut().filter(|d| match d {Def::Actor{..} => true, _ => false})
        .map(|d| verify_definition(d, vec!(sigs)))
        .fold(Vec::new(), |l, r| {l.append(&mut match r.err() {Some(v) => v, None => Vec::new()}); return l;});
    errs.append(&mut errsact);
    let mut errscirc = p.iter().filter(|d| match d {Def::Var{..} | Def::Func{..} => true, _ => false}).map(|d| check_circular_def(d, vec!(sigs)))
        .fold(Vec::new(), |l, r| {l.append(&mut r); return l;});
    errs.append(&mut errscirc);
    return if errs.len() > 0 {Err(errs)} else {Ok(())};
}

fn verify_definition(d: &mut Def, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    match d {
        Def::Actor { name, members } => verify_def_actor(name, members, sigs),
        Def::Var { name, vartype, value } => verify_def_var(name, vartype, value, sigs),
        Def::Func { name, args, rettype, expr } => verify_def_func(name, vartype, value, sigs),
        Def::Event { name } => Ok(()), //Single names do not need to be verified
        Def::State { name, statetype } => Ok(()),
        Def::Timer { name } => Ok(()),
        Def::StateEvent { name } => Ok(()),

        Def::Proc { name, args, seq } => verify_def_proc(name, , sigs),
        Def::OnEvent { name, seq } => verify_def_onevent(name, seq),
        Def::Sounds { files } => Ok(()),
        Def::Graphics { files } => Ok(())
    }
}

fn verify_def_actor(name: &Id, members: &mut Vec<Def>, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    let mut local_sigs = vec!(match lookup_sig1(sigs, name.fragment(), |s| match s {Signature::Actor{..} => true, _ => false}) {
        Some(Signature::Actor { name, locals }) => locals,
        _ => panic!("Actor's signature not found.")
    });

    local_sigs.append(&mut sigs.clone());
    
    let mut errs = members.iter_mut().map(|d| verify_definition(d, local_sigs)).fold(Vec::new(), |l, r| {l.append(&mut match r.err() {Some(v) => v, None => Vec::new()}); return l;});

    let mut errscirc = members.iter().filter(|d| match d {Def::Var{..} | Def::Func{..} => true, _ => false}).map(|d| check_circular_def(d, sigs))
        .fold(Vec::new(), |l, r| {l.append(&mut r); return l;});
    errs.append(&mut errscirc);
    return if errs.len() > 0 {Err(errs)} else {Ok(())};
}

fn verify_def_var(name: &Id, vartype: &Option<Type>, value: &mut Expr, sigs: Vec<&Signatures> ) -> Result<(), Vec<CError>> {
    let var = match lookup_sig1(sigs, name.fragment(), |s| match s {Signature::Var{..} => true, _ => false}) {
        Some(Signature::Var { name, vartype, referenced_symbols }) => (name, vartype, referenced_symbols),
        _ => panic!("Var's signature not found")
    };
    match vartype {Some(_) => (), None => return Err(vec!(generic_err(name, "Member variables must have defined type")))}; 

    verify_expr(value, sigs)?;
                                                                                //
    return Ok(());
}

fn check_circular_def(d: &Def, sigs: Vec<&Signatures>) -> Vec<CError> {
    let possible_circ = match d {
        Def::Var { name, vartype, value }  => get_circular_def(name, name, SymbolType::Var, get_refs_of_def(name, SymbolType::Var, sigs), sigs),
        Def::Func { name, args, rettype, expr } => get_circular_def(name, name, SymbolType::Func, get_refs_of_def(name, SymbolType::Func, sigs), sigs),
        _ => panic!("check_circular_def called on def which is not a var or func")
    };
    let name = match d {
        Def::Var { name, vartype, value } => name,
        Def::Func { name, args, rettype, expr } => name,
        _ => unreachable!()
    };
    match possible_circ {
        Some(v) => Err(vec!(err_circular_def(name, v))  
    }
}

//recurisvely searches into the definition of a variable or function to see if it references itself
//at any point.
//This is so that members can be defined in any order throughout an actor or application.
//returns path to a circular reference as a vec of ids, with the first item being the reference to
//whatever symbol is being searched for.
//Will panic if any symbol in any expression is unknown
fn get_circular_def(target: &Id<'a>, name: &Id<'a>, t: SymbolType, referenced_symbols: Vec<(&Id, SymbolType)>, sigs: Vec<&Signatures>) -> Option<Vec<&Id>> {
    
    let refs = match t {
        SymbolType::Func => {
            let sig = lookup_sig1(sigs, target, |s| match s {Signature::Func{..} => true, _ => false});
            let args = match sig { Signature::Func { args, ..} => args, _ => panic!("function signature not found") };
            fn argscontains(n: &Id) -> bool {
                args.iter().any(|a| a.name == n)
            }
            referenced_symbols.iter().filter(|(a, b)| !(b == SymbolType::Var && argscontains(a))) //filter out the function parameters
        }
        SymbolType::Var => {
            referenced_symbols.iter()
        }
    };
    
    let self_ref = refs.find(|(a, t)| a == target && t == SymbolType::Func).0; //check if
                                                                               //
    match self_ref {
        Some(n) => return Some(vec!(n)),
        None() => ()
    }

    match ref_filtered.map(|(refname,symboltype)| get_circular_def(target, refname, t, get_refs_of_def(refname, symboltype, sigs), sigs)) //search for circular def in referenced symbols of referenced symbols
                .find(|d| match d {Some(_) => true, None => false})
                {
                    Some(Some(v)) => {v.push(name); return Some(v)},
                    Some(None) => unreachable!(),
                    None => return None,
                }
}

fn get_refs_of_def(name: &Id, tp: SymbolType, sigs: Vec<&Signatures>) -> Vec<(&Id, SymbolType)> {
    let matchfn = match tp {
        SymbolType::Var => |s| match s {Signature::Var { .. } => true, _ => false},
        SymbolType::Func => |s| match s {Signature::Func {..} => true, _ => false}
    }
    match lookup_sig1(sigs, name, matchfn) {
        Some(Signature::Var { name, vartype, referenced_symbols }) => referenced_symbols,
        Some(Signature::Func { name, args, rettype, referenced_symbols }) => referenced_symbols,
        _ => panic!("var or func signature not found")        
    }                            
}

fn verify_expr(e: &mut Expr, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>>{
   e.exprtype = Some(verify_uexpr(&mut e.expr, sigs)?); 
   Ok(())
}

fn verify_uexpr(e: &mut UExpr, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    match e {
        UExpr::Value(e) => ver_value(e, sigs),
        UExpr::UnExpr { op, expr } => ver_un_ex(op, expr, sigs),
        UExpr::BinExpr { l, op, r } => ver_bin_ex(l, op, r, sigs)
    }
}

fn ver_value(v: &Value, sigs: &Signatures) -> Result<Type, Vec<CError>> {
    match v {
        Value::Literal(Literal::IntLiteral(_)) => Ok(Type::Int),   
        Value::Literal(Literal::NumLiteral(_)) => Ok(Type::Num),   
        Value::Literal(Literal::BoolLiteral(_)) => Ok(Type::Bool),   
        Value::Literal(Literal::StringLiteral(_)) => Ok(Type::Str),   
        Value::VarCall(a) => {match lookup_sig(sigs, a) {
            Some(Signature::Var{name, vartype, ..}) => Ok(*vartype),
            Some(_) => Err(vec!(err_wrong_deftype(a, "variable"))),
            None => Err(vec!(err_undeclared_id(a, "variable")))

        }}
        Value::FuncCall(a) => { match lookup_sig(sigs, &a.id) {
            Some(Signature::Func { name, args, rettype, .. }) => ver_func_call(&mut a, args, *rettype, sigs),
            Some(_) => Err(vec!(err_wrong_deftype(&a.id, "function call"))),
            None => Err(vec!(err_undeclared_id(&a.id, "function")))
        }
        }
    }
}

fn ver_func_call(call: &mut FuncCall, args: &Vec<Type>, rettype: Type, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    for arg in &mut call.args { //verify call arguments
        verify_expr(arg, sigs)?;
    }
    if call.args.len() == args.len() {
        let mut no_match: bool;
        let no_match = args.iter().zip(call.args.iter()).any(|(target_type, arg_expr)| !type_match(arg_expr.exprtype.unwrap(), *target_type));
        if !no_match {
            return Ok(rettype);
        }
    }
    return Err(vec!(generic_err(&call.id, format!("function {} called with incorrect arguments, expected {}, found {}",
                                                 call.id,
                                                 args.iter().map(|a| a.bcis_rep()).collect::<Vec<String>>().join(", "),
                            call.args.iter().map(|arg| arg.exprtype.unwrap().bcis_rep()).collect::<Vec<String>>().join(", ")).as_str())));
}

fn type_match(given: Type, target: Type) -> bool {
    match given {
        Type::Int => {match target {Type::Int | Type::Num => true, _ => false}},
        Type::Num => {match target {Type::Num => true, _ => false}},
        Type::Str => {match target {Type::Num => true, _ => false}},
        Type::Bool => {match target {Type::Bool => true, _ => false}}
    }
}

fn ver_un_ex(op: &UnOp, expr: &mut Expr, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    verify_expr(expr, sigs)?;
    //Operand expression's type
    let exptype = expr.exprtype.unwrap(); // previous line should have given expr a type, or early returned,
                                          // there is no situation in which the exprtype would be None here
    //Possible signatures (In Type, Out Type) of the operator
    let op_sigs = UNARY_OPERATORS.get(op.fragment()).unwrap(); //Operator character will be valid
                                                               //because it was previously parsed.
    op_sigs.iter().find(|possible_signature| possible_signature.0 == exptype)
        .map(|sig| sig.1) // extract operator's return type on given value
        .ok_or(vec!(err_unary_mismatch(op, exptype, op_sigs)))
}

fn ver_bin_ex(l: &mut Expr, op: &BinOp, r: &mut Expr, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    verify_expr(l, sigs)?;
    verify_expr(r, sigs)?;
    let ltype = l.exprtype.unwrap();
    let rtype = r.exprtype.unwrap();

    let op_sigs = BINARY_OPERATORS.get(op.fragment()).unwrap();

    let left_matches: Vec<&(Type, Type, Type)> = op_sigs.iter().filter(|s| s.0 == ltype).collect();

    if left_matches.len() == 0 {
        return Err(vec!(err_binary_left_mismatch(op, ltype, op_sigs)))
    }

    left_matches.iter().find(|possible_signature| possible_signature.1 == rtype)
        .map(|sig| sig.2) // extract the operator's return type on given values
        .ok_or(vec!(err_binary_right_mismatch(op, ltype, rtype, &left_matches)))
}

/*
 * Looks up a signature within the given maps, with each map representing a nested namespace, going
 * from most local to most broad
 */
fn lookup_sig<'a>(v: Vec<&Signatures>, s: &str) -> Option<&'a Signature<'a>> {
    for sigl in v {
        match sigl.get(&s.to_string()) {
            Some(s) => return Some(s),
            None => ()
        }
    }
    return None;
}
//looks up a signature while matching against the signature type,
//as there may be multiple symbols of the same name within the accessible namespaces,
//so one needs to get the first one that is of the correct type.
//When given a matching function which returns true on the correct type of signature, it should
//simulate how BCIEvent calls functions, procedures, variables, etc, moving up from the sequence
//level to the actor level to the application level.
fn lookup_sig1<'a, F>(v: Vec<&Signatures>, s: &str, m: F) -> Option<&'a Signature<'a>> 
    where F: Fn(&Signature) -> bool
{
    for sigl in v {
        match sigl.get(&s.to_string()) {
            None => (),
            Some(s) => {
                if m(s) {
                    return Some(s);
                }
            }
        }
    }
    return None;
}
