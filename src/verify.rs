use crate::ast::*;
use crate::err::*;
use crate::builtins::BUILTINS;

#[derive(Clone)]
pub enum Signature<'a>{
    Actor{name: Id<'a>},
    OnEvent{name: Id<'a>},
    Proc{name: Id<'a>, args: Vec<Type> },
    Func{name: Id<'a>, args: Vec<Type>, rettype: Type },
    Event{name: Id<'a>},
    State{name: Id<'a>, statetype: StateType},
    Var{name: Id<'a>, vartype: Type}
}

impl Signature<'_> {
    pub fn name(&self) -> &Span{
        match self {
            Signature::Actor{name} => name,
            Signature::OnEvent{name} => name,
            Signature::Proc{name, args} => name,
            Signature::Func{name, args, rettype} => name,
            Signature::Event{name} => name,
            Signature::State{name, statetype} => name,
            Signature::Var{name, vartype} => name
        }
    }
}

impl PartialEq for Signature<'_>{
    fn eq(&self, other: &Signature) -> bool {
        self.name().fragment() == other.name().fragment()  
    } 
}

impl Eq for Signature<'_> {}

pub fn verify(p: Program) -> Result<(), Vec<CError>> {
    let signatures = verify_declarations(p)?;
    verify_definitions(p, signatures)
}

/**
 * Verifies global declarations, collects their signatures into a vector
 * Also verifies assignment expressions of variables, as they need to be typed before use
 * Will return errors if declarations are found which can only be inside an actor,
 * (OnEvent, Graphics, Sounds) are found outside an actor, or if a symbol is defined more than
 * once.
 */
fn verify_declarations<'a>(p: Program) -> Result<Vec<Signature<'a>>, Vec<CError>> {
    let errs_from_onevent = p.into_iter()
        .filter(|d| match d { Def::OnEvent{..} | Def::Graphics{..} | Def::Sounds{..} => true, _ => false })
        .map(|d| err_invalid_top_level_dec(&d))
        .collect::<Vec<CError>>();
    let mut signatures: Vec<Signature> = Vec::new();
    let mut errs: Vec<CError> = Vec::new();
    let rest = p.into_iter()
        .filter(|d| match d { Def::OnEvent{ .. } | Def::Graphics{..} | Def::Sounds{..} => false, _ => true })
        /***
         * I would prefer to use map here, but since each call to verify_declaration depends
         * on the accumulation of Signatures returned from each previous call, it takes
         * some serious functional/combinatorial wizardry to implement that as a pure map function,
         * (i have previously implemented this in Haskell)
         * so instead signatures and errors are collected into external vecs.
         */
        .for_each(|d| {
            let sig = verify_declaration(d, &[&signatures[..], &BUILTINS[..]].concat());
            match sig {
                Ok(s) => signatures.as_mut().push(s),
                Err(e) => errs.as_mut().append(e)
            } 
        }
            );
    errs_from_onevent.append(&mut errs);
    if errs_from_onevent.len() > 0{
        return Err(errs_from_onevent);
    }
    Ok(signatures)
}

fn verify_declaration<'a>(d: Def<'a>, sigs: &[Signature]) -> Result<Signature, Vec<CError>> {
    match d {
        Actor{name, members} => verify_dec_actor(name, members),
        Proc{name, args, seq} => verify_dec_proc(name, args),
        Func{name, args, rettype, expr} => verify_dec_func(name, args, rettype, expr),
        Event{name} => verify_dec_event(name),
        State{name, statetype} => verify_dec_state(name, statetype),
        Var{name, vartype, value} => verify_dec_var(name, vartype, value),
        _ => panic!("verify_declaration given invalid declaration type")
    }
}

fn verify_dec_actor(name: Id, members: Vec<Def>, sigs: Vec<Signature>) -> Result<Signature, Vec<CError>>{
    let errs = members.into_iter()
       .filter(|d| match d {State => true, _ => false})
       .map(err_state_in_actor)
       .filter([d] match d {Actor => true, _ => false})
       .map(err_actor_in_actor)
       .collect::<Vec<CError>>;
    
    let sig = Signature::Actor{name};
    errs.push(get_redef_err(&sig, &sigs));
    if (errs.size() > 0) {
        return Err(errs);
    }
    return Ok(sig);
}


fn get_redef_err(sig: Signature, sigs: Vec<Signature>) -> Vec<CError> {
    let v: Vec<Signature> = Vec::new();
    let prev = sigs.iter().find(|s| s == sig);
    match prev {
        None => (),
        Some(p) => v.push(err_redefinition(&sig, &prev))
    }
    return v;
}

fn verify_definitions(p: Program, sigs: Vec<Signature>) -> Result<(), Vec<CError>>  {
    

}

fn verify_expr(e: &mut Expr, sigs: &Vec<Signature>) -> Result<(), Vec<CError>>{
   e.exprtype = ver_uexpr(e.expr)?; 
   Ok(())
}

fn verify_uexpr(e: &mut UExpr, sigs: Vec<Signature>) -> Result<Type, Vec<CError>> {
    match e {
        UExpr::Value(e) => ver_value(e, sigs),
        UExpr::UnExpr { op, expr } => ver_un_ex(op, expr),
        UExpr::BinExpr { l, op, r } => ver_bin_ex(l, op, r)
    }
}

fn ver_value(v: &Value, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    match v {
            Value::Literal(Literal::IntLiteral(_)) => Ok(Type::Int),   
        Value::Literal(Literal::NumLiteral(_)) => Ok(Type::Num),   
        Value::Literal(Literal::BoolLiteral(_)) => Ok(Type::Bool),   
        Value::Literal(Literal::StringLiteral(_)) => Ok(Type::Str),   
        Value::VarCall(a) => {match lookup_sig(sigs, a) {
            Some(Signature::Var{name, vartype}) => Ok(*vartype),
            Some(_) => Err(vec!(err_wrong_deftype(a, "variable"))),
            None => Err(vec!(err_undeclared_id(a)))
        }}

    }
}

fn ver_un_ex(op: UnOp, expr: &mut Expr, sigs: &Vec<Signature>) -> Result<Type, Vec<CError>> {
    verify_expr(expr, sigs)?;
    let extype = expr.exprtype;
    match op {
        UnOp::Neg(a) => if match extype {Type::Int | Type::Num => true, _ => false} {Ok(extype)} else {Err(vec!(err_unary_mismatch(a, extype)))},
        UnOp::Not(a) => if match extype {Type::Bool => true, _ => false} {Ok(extype)} else {Err(vec!(err_unary_mismatch(a, extype)))}
    }
}

fn lookup_sig<'a>(v: &Vec<Signature<'a>>, s: &str) -> Option<&'a Signature<'a>> {
    for sig in v {
        if sig.name().fragment() == &s {
            return Some(sig);
        }
    }
    return None;
}
