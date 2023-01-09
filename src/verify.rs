use std::collections::HashMap;

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
    Func{name: &'a Id<'a>, args: &'a Vec<ArgDef<'a>>, rettype: Type, referenced_symbols: Vec<(&'a Id<'a>, SymbolType)> },
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

pub fn verify(p: &Program) -> Result<(), Vec<CError>> {
    let signatures = verify_declarations(p)?;
    let ver_errs = verify_definitions(&mut p, &signatures);
    return if !ver_errs.is_empty() {Err(ver_errs)} else {Ok(())};
}

type Signatures<'a> = HashMap<String, Signature<'a>>;

/**")
 * Verifies global declarations, collects their signatures into a vector
 * Will return errors if declarations are found which can only be inside an actor,
 * (OnEvent, Graphics, Sounds) are found outside an actor, or if a symbol is defined more than
 * once.
 */
fn verify_declarations<'a>(p: &Program) -> Result<Signatures<'a>, Vec<CError>> {
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
            |a| Signature::Func{name,  args, rettype: *rettype, referenced_symbols: get_referenced_symbols(&expr)}),
        Def::Event{name}                        => not_already_declared(&name, sigs).map(|_| Signature::Event { name }),
        Def::StateEvent { name }                => not_already_declared(&name, sigs).map(|_| Signature::StateEvent{name}),
        Def::Timer { name }                     => not_already_declared(&name, sigs).map(|_| Signature::Timer{name}),
        Def::State{name, statetype}             => not_already_declared(&name, sigs).map(|_| Signature::State{name, statetype}),
        Def::Var{name, vartype, value}          => verify_dec_variable(&name, &vartype, &value, sigs),
        Def::Timer { name }                     => not_already_declared(&name, sigs).map(|_| Signature::Timer { name }), 
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
            |a| Signature::Func{name, args, rettype: *rettype, referenced_symbols: get_referenced_symbols(&expr)})),
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


//Verifies definitions and checks for circular definitions.
//Returs a vec containing all errors found
fn verify_definitions(p: &mut Program, sigs: &Signatures) -> Vec<CError>  {
    let mut errs = p.iter_mut().filter(|d| match d {Def::Actor{..} => false, _ => true})
        .flat_map(|d| match verify_definition(d, vec!(sigs)) {Ok(()) => Vec::new(), Err(e) => e})
        .collect::<Vec<CError>>();
    //Actors must be verified last because of how the circular definition check works (this is
    //because all expressions must be verified before any circular definition checking
    //happens.)
    let mut errsact = p.iter_mut().filter(|d| match d {Def::Actor{..} => true, _ => false})
        .flat_map(|d| match verify_definition(d, vec!(sigs)) {Ok(()) => Vec::new(), Err(e) => e})
        .collect::<Vec<CError>>();
    errs.append(&mut errsact);
    let mut errscirc = p.iter().filter(|d| match d {Def::Var{..} | Def::Func{..} => true, _ => false}).map(|d| check_circular_def(d, vec!(sigs)))
        .fold(Vec::new(), |l, r| {l.append(&mut r); return l;});
    errs.append(&mut errscirc);
    return errs;
}

fn verify_definition(d: &mut Def, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    match d {
        Def::Actor { name, members } => verify_def_actor(name, members, sigs),
        Def::Var { name, vartype, value } => verify_def_var(name, vartype, value, sigs),
        Def::Func { name, args, rettype, expr } => verify_def_func(name, args, *rettype, expr, sigs),
            //Single names do not need to be verified
        Def::Event { .. }  
        | Def::State { .. }  
        | Def::Timer { .. } 
        | Def::StateEvent { .. }
        | Def::Sounds { .. }
        | Def::Graphics { .. } => Ok(()),

        Def::Proc { name, args, seq } => verify_def_proc(name, args, seq, sigs),
        Def::OnEvent { name, seq } => verify_def_onevent(seq, sigs),
    }
}

fn verify_def_actor(name: &Id, members: &mut Vec<Def>, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    let mut local_sigs = vec!(match lookup_sig1(sigs, name.fragment(), |s| match s {Signature::Actor{..} => true, _ => false}) {
        Some(Signature::Actor { name, locals }) => locals,
        _ => panic!("Actor's signature not found.")
    });

    local_sigs.append(&mut sigs.clone());
    
    let mut errs = members.iter_mut().flat_map(|d| match verify_definition(d, local_sigs) {Ok(()) => Vec::new(), Err(e) => e}).collect::<Vec<CError>>();

    let mut errscirc = members.iter().filter(|d| match d {Def::Var{..} | Def::Func{..} => true, _ => false}).map(|d| check_circular_def(d, sigs))
        .fold(Vec::new(), |l, r| {l.append(&mut r); return l;});
    errs.append(&mut errscirc);

    return if errs.len() > 0 {Err(errs)} else {Ok(())};
}

fn verify_def_var(name: &Id, vartype: &Option<Type>, value: &mut Option<Expr>, sigs: Vec<&Signatures> ) -> Result<(), Vec<CError>> {
    match vartype {Some(_) => (), None => return Err(vec!(generic_err(name, "Member variables must have defined type")))}; 

    match value {
        Some(e) => {
            verify_expr(e, sigs)?;
            if !type_match(e.exprtype.unwrap(), vartype.unwrap()) {
                Err(vec!(generic_err(name, 
                    format!("Type mismatch: Variable is defined as type {}, but is assigned a value of type {}", vartype.unwrap().bcis_rep(), e.exprtype.unwrap().bcis_rep()).as_str())))
            } else {Ok(())}
        }
        None => Ok(())
    }
}

fn verify_def_func(name: &Id, args: &Vec<ArgDef>, rettype: Type, value: &mut Expr, ext_sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    let sigs = vec!(&args.iter().map(|d| (d.name.fragment().to_string(), Signature::Var { name, vartype: d.argtype, referenced_symbols: Vec::new() })).collect::<Signatures>());
    sigs.append(&mut ext_sigs);
    

    verify_expr(value, sigs)?; 


    return if !type_match(value.exprtype.unwrap(), rettype) {
        Err(vec!(generic_err(name, format!("Function must return a value of type {}, but returns type {}", rettype.bcis_rep(), value.exprtype.unwrap().bcis_rep()).as_str())))
    } else {
        Ok(())
    }
}

fn verify_def_proc(name: &Id, args: &Vec<ArgDef>, seq: &mut Seq, ext_sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    let sigs = vec!(&args.iter().map(|d| (d.name.fragment().to_string(), Signature::Var { name, vartype: d.argtype, referenced_symbols: Vec::new() })).collect::<Signatures>());
    sigs.append(&mut ext_sigs);

    verify_seq(seq, sigs)
}

fn verify_def_onevent(seq: &mut Seq, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>>{
    verify_seq(seq, sigs)
}



fn verify_seq(seq: &mut Seq, ext_sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    let mut sigs: Vec<&Signatures> = vec!(&Signatures::new());
    sigs.append(&mut ext_sigs);
    let mut errs: Vec<CError> = Vec::new();

    for &mut stmt in seq {
        match verify_stmt(&mut stmt, sigs) {
            Ok(Some((n, s))) => {match sigs.get(0).unwrap().insert(n, s) {Some(_) => panic!("attempted to add a duplicate signature, this should not be possible"), _ => ()};},
            Ok(None) => (),
            Err(e) => {errs.append(&mut e);}
        } 
    }
    return if errs.len() > 0
    { Err(errs) } else {Ok(())}
}


fn verify_stmt<'a>(stmt: &mut Stm<'a>, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match stmt {
        Stm::Call { id, args } => verify_stm_call(id, args, sigs),
        Stm::If { kw, cond, seq } => verify_stm_if(kw, cond, seq, sigs),
        Stm::Var { name, vartype, value } => verify_stm_var(name, vartype, value, sigs),
        Stm::Timer { name, cmd } => verify_stm_timer(name, cmd, sigs),
        Stm::Assign { id, val } => verify_stm_assign(id, val, sigs),
        Stm::While { kw, cond, seq } => verify_stm_while(kw, cond, seq, sigs),
        Stm::Repeat { kw, val, seq } => verify_stm_repeat(kw, val, seq, sigs),
        Stm::Timed { kw, time, seq } => verify_stm_timed(kw, time, seq, sigs),
        Stm::IfElse { kw, cond, seq, elifs, elseq } => verify_stm_if_else(kw, cond, seq, elifs, elseq, sigs),
        Stm::CallEvent {tp, name} => verify_stm_callevent(tp, name, sigs)
    }
}

fn verify_stm_callevent<'a>(tp: &mut Option<EvType>, name: &Id<'a>, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match tp {
        Some(EvType::BCISEvent) => {
            match lookup_sig1(sigs, name, |s| match s {Signature::Event{..} => true, _ => false}) {
                Some(Signature::Event {..}) => Ok(None),
                Some(_) => unreachable!(),
                None => Err(vec!(err_undeclared_id(name, "Event")))
            }
        }
        Some(EvType::StateEvent) => {
            match lookup_sig1(sigs, name, |s| match s {Signature::StateEvent{..} => true, _ => false}) {
                Some(Signature::Event {..}) => Ok(None),
                Some(_) => unreachable!(),
                None => Err(vec!(err_undeclared_id(name, "StateEvent")))
            }
        }

        None => {  
            match (lookup_sig1(sigs, name, |s| match s {Signature::Event{..} => true, _ => false}),
                    lookup_sig1(sigs, name, |s| match s {Signature::StateEvent{..} => true, _ => false})) {

                (None, None) => Err(vec!(err_undeclared_id(name, "Event or stateevent"))),
                (Some(Signature::Event { name }), None) => {tp.insert(EvType::BCISEvent); Ok(None)},
                (None, Some(Signature::StateEvent { name })) => {tp.insert(EvType::StateEvent); Ok(None)},
                (Some(Signature::Event{..}), Some(Signature::StateEvent{..})) => {
                    Err(vec!(generic_err(name, format!("both an event and stateevent with the name {} exist within the current namespace.
                        add \".b\" or \".s\" to \"call\" to specify whether you want to call a BCIScript event or a StateEvent.", name.fragment()).as_str())))
                }
                _ => unreachable!()
            }
        }
    }
}

fn verify_stm_if_else<'a>(kw: &Span<'a>, cond: &mut Expr, seq: &mut Seq, elifs: &mut Vec<ElseIf>, elseq: &mut Seq, sigs: Vec<&Signatures>) 
    -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
        let errs:Vec<CError> = Vec::new();
        match &mut verify_expr(cond, sigs) {
            Ok(()) => {
                if !type_match(cond.exprtype.unwrap(), Type::Bool){
                    errs.push(generic_err(kw, format!("if statement expects an expression of type bool, but was given expression of type {}", cond.exprtype.unwrap().bcis_rep()).as_str()));
                }
            }
            Err(e) => errs.append(e),
        }
        match verify_seq(seq, sigs) {Ok(()) => (), Err(e) => errs.append(&mut e)}

        errs.append(&mut elifs.iter_mut().flat_map(|e| match verify_else_if(e, sigs) {Err(e) => e, _ => Vec::new()}).collect::<Vec<CError>>());

        match verify_seq(elseq, sigs) {Ok(()) => (), Err(e) => errs.append(&mut e)}

        return if errs.len() > 0 {Err(errs)} else {Ok(None)};
    }

fn verify_else_if<'a>(elif: &mut ElseIf, sigs: Vec<&Signatures>) -> Result<(), Vec<CError>> {
    verify_expr(&mut elif.cond, sigs)?;
    return if !type_match(elif.cond.exprtype.unwrap(), Type::Bool) {
        Err(vec!(generic_err(&elif.kw, format!("elif expects expression of type bool, but was given expression of type {}", elif.cond.exprtype.unwrap().bcis_rep()).as_str())))
    } else {
        verify_seq(&mut elif.seq, sigs)
    }
}

fn verify_stm_timed<'a>(kw: &Span<'a>, val: &mut Expr, seq: &mut Seq, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    verify_expr(val, sigs)?;
    return if !type_match(val.exprtype.unwrap(), Type::Num) {
        Err(vec!(generic_err(kw, format!("repeat expects expression of type num, but was given expression of type {}", val.exprtype.unwrap().bcis_rep()).as_str())))
    } else {
        verify_seq(seq, sigs).map(|_| None)
    }
    
}

fn verify_stm_repeat<'a>(kw: &Span<'a>, val: &mut Expr, seq: &mut Seq, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    verify_expr(val, sigs)?;
    return if !type_match(val.exprtype.unwrap(), Type::Int) {
        Err(vec!(generic_err(kw, format!("repeat expects expression of type int, but was given expression of type {}", val.exprtype.unwrap().bcis_rep()).as_str())))
    } else {
        verify_seq(seq, sigs).map(|_| None)
    }
}

fn verify_stm_while<'a>(kw: &Span<'a>, cond: &mut Expr, seq: &mut Seq, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    verify_expr(cond, sigs)?;
    return if !(type_match(cond.exprtype.unwrap(), Type::Bool)) {
        Err(vec!(generic_err(kw, format!("while expects expression of type bool, but was given expression of type {}", cond.exprtype.unwrap().bcis_rep()).as_str())))
    } else {
        verify_seq(seq, sigs).map(|_| None)
    }
}

fn verify_stm_assign<'a>(id: &Id<'a>, value: &mut Expr, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match lookup_sig1(sigs, id.fragment(), |s| match s {Signature::Var{..} => true, _ => false}) {
        Some(Signature::Var { name, vartype, referenced_symbols }) => {
            verify_expr(value, sigs)?;
            if !type_match(value.exprtype.unwrap(), *vartype) {
                Err(vec!(generic_err(id, format!("Cannot assign value of type {} to variable of type {}", value.exprtype.unwrap().bcis_rep(), vartype.bcis_rep()).as_str())))
            } else {
                Ok(None)
            }
        },
        Some(_) => unreachable!(),
        None => Err(vec!(err_undeclared_id(id, "variable")))
    }
}

fn verify_stm_timer<'a>(name: &Id<'a>, cmd: &TimerCmd, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match cmd {
        TimerCmd::Add => {
            match lookup_sig_local_any(sigs, name.fragment()) {
                Some(s) => Err(vec!(err_redefinition1(name, s))),
                Some(_) => unreachable!(),
                None => Ok(Some((name.fragment().to_string(), Signature::Timer{ name })))
            }
        }
        TimerCmd::Read => Err(vec!(generic_err(name, "Cannot read timer oustide of expression"))),
        _ => Ok(None)
    }
}

fn verify_stm_var<'a>(name: &Id<'a>, vartype: &mut Option<Type>, value: &mut Option<Expr>, sigs: Vec<&Signatures> ) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match lookup_sig_local_any(sigs, name.fragment()) {
        Some(s) => return Err(vec!(err_redefinition1(name, s))),
        None => ()
    }
    match (vartype, value) {
        (None, None) => Err(vec!(generic_err(name, "Variable type cannot be inferred. Either provide an explicit type annotation or initialize the variable."))),
        (Some(t), None) => Ok(Some((name.fragment().to_string(), Signature::Var{name, referenced_symbols: Vec::new(), vartype: *t}))),
        (None, Some(e)) => {verify_expr(e, sigs)?;
            vartype.insert(e.exprtype.unwrap());
            Ok(Some((name.fragment().to_string(), Signature::Var{name, referenced_symbols: Vec::new(), vartype: e.exprtype.unwrap()})))
        },
        (Some(t), Some(e)) => {verify_expr(e, sigs)?; 
            if !type_match(e.exprtype.unwrap(), *t) {
                Err(vec!(generic_err(name, format!("Variable is of type {}, but its assignment expression is of type {}", t.bcis_rep(), e.exprtype.unwrap().bcis_rep()).as_str())))
            } else {
                Ok(Some((name.fragment().to_string(), Signature::Var { name, vartype: *t, referenced_symbols: Vec::new() })))
            }
        }
    }
}

fn verify_stm_if<'a>(ifs: &Span, cond: &mut Expr, seq: &mut Seq, sigs: Vec<&Signatures>) ->  Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    verify_expr(cond, sigs)?;
    return if type_match(cond.exprtype.unwrap(), Type::Bool) {
        verify_seq(seq, sigs).map(|_| None)
    } else {
        Err(vec!(generic_err(ifs, format!("If statement expects a boolean expression, but was given expression of type {}", cond.exprtype.unwrap().bcis_rep()).as_str())))
    }

}

fn verify_stm_call<'a>(name: &Id<'a>, call_args: &Vec<Expr>, sigs: Vec<&Signatures>) -> Result<Option<(String, Signature<'a>)>, Vec<CError>> {
    match lookup_sig1(sigs, name, |s| match s {Signature::Proc{..} => true, _ => false}) {
        Some(Signature::Proc { name, args }) => {
            if call_args.len() == args.len() {
                let no_match = args.iter().zip(call_args.iter()).any(|(target_type, arg_expr)| !type_match(arg_expr.exprtype.unwrap(), *target_type));
                return if no_match {
                    Err(vec!(generic_err(name, format!("{} expects parameters of type {}, but was given parameters of type {}",
                                                     name.fragment(),
                                                     args.iter().map(|t| t.bcis_rep()).collect::<Vec<String>>().join(", "),
                                                     call_args.iter().map(|e| e.exprtype.unwrap().bcis_rep()).collect::<Vec<String>>().join(", ")).as_str())))
                }
                else {
                    Ok(None)
                }
            }
            return Err(vec!(generic_err(name, &format!("procedure expects {} parameters but was given {}", args.len(), call_args.len()))))
        }
        Some(_) => unreachable!(),
        None => Err(vec!(err_undeclared_id(name, "procedure")))
    }    
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
        _ => 
            unreachable!()
    };
    match possible_circ {
        Some(v) => vec!(err_circular_def(name, &v)),  
        None => Vec::new()
    }
}

//recurisvely searches into the definition of a variable or function to see if it references itself
//at any point.
//This is so that members can be defined in any order throughout an actor or application.
//returns path to a circular reference as a vec of ids, with the first item being the reference to
//whatever symbol is being searched for.
//Will panic if any symbol in any expression is unknown
fn get_circular_def<'a>(target: &Id<'a>, name: &Id<'a>, t: SymbolType, referenced_symbols: &Vec<(&Id<'a>, SymbolType)>, sigs: Vec<&Signatures>) -> Option<Vec<&'a Id<'a>>> {
    
    let refs = match t {
        SymbolType::Func => {
            let sig = lookup_sig1(sigs, target, |s| match s {Signature::Func{..} => true, _ => false});
            let args = match sig { Some(Signature::Func { args, ..}) => args, _ => panic!("function signature not found") };
            let argscontains = |n: &Id| args.iter().any(|a| a.name.fragment() == n.fragment());
            referenced_symbols.iter().filter((|(a, b)| !(b == SymbolType::Var && argscontains(a))) as Box<dyn Fn(_) -> bool>)  //filter out the function parameters
        }
        SymbolType::Var => {
            referenced_symbols.iter().filter((|_| true) as Box<dyn Fn(_) -> bool>)
        }
    };
    
    let self_ref = refs.find(|(nm, st)| nm == target && st == t).0; 
                                                                               
    match self_ref {
        Some(n) => return Some(vec!(n)),
        None => ()
    }

    match refs.map(|(refname,symboltype)| get_circular_def(target, refname, t, get_refs_of_def(refname, symboltype, sigs), sigs)) //search for circular def in referenced symbols of referenced symbols
                .find(|d| match d {Some(_) => true, None => false})
                {
                    Some(Some(v)) => {v.push(name); return Some(v)},
                    Some(None) => unreachable!(),
                    None => return None,
                }
}

fn get_refs_of_def<'a>(name: &Id<'a>, tp: SymbolType, sigs: Vec<&Signatures>) -> &'a Vec<(&'a Id<'a>, SymbolType)> {
    let matchfn = match tp {
        SymbolType::Var => |&s| match s {Signature::Var { .. } => true, _ => false},
        SymbolType::Func => |&s| match s {Signature::Func {..} => true, _ => false}
    };
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

fn verify_uexpr(e: &mut UExpr, sigs: Vec<&Signatures>) -> Result<Type, Vec<CError>> {
    match e {
        UExpr::Value(e) => ver_value(e, sigs),
        UExpr::UnExpr { op, expr } => ver_un_ex(op, expr, sigs),
        UExpr::BinExpr { l, op, r } => ver_bin_ex(l, op, r, sigs)
    }
}

fn ver_value(v: &Value, sigs: Vec<&Signatures>) -> Result<Type, Vec<CError>> {
    match v {
        Value::Literal(Literal::IntLiteral(_)) => Ok(Type::Int),   
        Value::Literal(Literal::NumLiteral(_)) => Ok(Type::Num),   
        Value::Literal(Literal::BoolLiteral(_)) => Ok(Type::Bool),   
        Value::Literal(Literal::StringLiteral(_)) => Ok(Type::Str),   
        Value::VarCall(a) => {match lookup_sig1(sigs, a, |s| match s {Signature::Var{..} => true, _ => false}) {
            Some(Signature::Var{name, vartype, ..}) => Ok(*vartype),
            Some(_) => unreachable!(),
            None => Err(vec!(err_undeclared_id(a, "variable")))

        }}
        Value::FuncCall(a) => { match lookup_sig1(sigs, &a.id,|s| match s {Signature::Func{..} => true, _ => false}) {
            Some(Signature::Func { name, args, rettype, .. }) => ver_func_call(&mut a, args, *rettype, sigs),
            Some(_) => unreachable!(),
            None => Err(vec!(err_undeclared_id(&a.id, "function")))
        }}
        Value::TimerCall { name, cmd } => { match lookup_sig1(sigs, name, |s| match s {Signature::Timer{..} => true, _ => false}) {
            None => Err(vec!(err_undeclared_id(name, "timer"))),
            Some(Signature::Timer { name }) => match cmd {
                TimerCmd::Read => Ok(Type::Num),
                _ => Err(vec!(generic_err(name, &format!("timer call must be expression. Did you mean `timer.{}.read`?", name.fragment()))))
            } 
            Some(_) => unreachable!()
        }}
    }
}

fn ver_func_call(call: &mut FuncCall, args: &Vec<ArgDef>, rettype: Type, sigs: Vec<&Signatures>) -> Result<Type, Vec<CError>> {
    for arg in &mut call.args { //verify call arguments
        verify_expr(arg, sigs)?;
    }
    if call.args.len() == args.len() {
        let no_match = args.iter().zip(call.args.iter()).any(|(target_type, arg_expr)| !type_match(arg_expr.exprtype.unwrap(), target_type.argtype));
        if !no_match {
            return Ok(rettype);
        }
    }
    return Err(vec!(generic_err(&call.id, format!("function {} called with incorrect arguments, expected {}, found {}",
                                                 call.id,
                                                 args.iter().map(|a| a.argtype.bcis_rep()).collect::<Vec<String>>().join(", "),
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

fn ver_un_ex(op: &UnOp, expr: &mut Expr, sigs: Vec<&Signatures>) -> Result<Type, Vec<CError>> {
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

fn ver_bin_ex(l: &mut Expr, op: &BinOp, r: &mut Expr, sigs: Vec<&Signatures>) -> Result<Type, Vec<CError>> {
    verify_expr(l, sigs)?;
    verify_expr(r, sigs)?;
    let ltype = l.exprtype.unwrap();
    let rtype = r.exprtype.unwrap();

    let op_sigs = BINARY_OPERATORS.get(op.fragment()).unwrap();

    let left_matches  = op_sigs.iter().filter(|s| s.0 == ltype);

    if iter_empty(&left_matches) {
        return Err(vec!(err_binary_left_mismatch(op, ltype, op_sigs)))
    }

    left_matches.find(|possible_signature| possible_signature.1 == rtype)
        .map(|sig| sig.2) // extract the operator's return type on given values
        .ok_or(vec!(err_binary_right_mismatch(op, ltype, rtype, &left_matches)))
}

fn iter_empty<F>(it: &F) -> bool
    where F: Iterator
{
    match it.peekable().peek() {
        Some(_) => true,
        None => false
    }
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
/* 
 * Checks if a signature has been defined locally by only checking the first member of the vector
 * of namespaces
 */
fn lookup_sig_local<'a, F>(v: Vec<&Signatures>, s: &str, m: F) -> Option<&'a Signature<'a>> 
    where F: Fn(&Signature) -> bool
{
    lookup_sig1(vec!(v.get(0).unwrap()), s, m)
}

fn lookup_sig_local_any<'a>(v: Vec<&Signatures>, s: &str) -> Option<&'a Signature<'a>> 
{
    lookup_sig(vec!(v.get(0).unwrap()), s)
}

