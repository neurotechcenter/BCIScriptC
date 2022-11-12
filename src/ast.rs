use std::Vec;
use nom_locate::LocatedSpan;

pub type Program = Vec<Def>;

pub enum Def {
    Actor{name: Id, members: Vec<Def>},
    OnEvent{name: Id, seq: Seq},
    Proc{name: Id, args: Vec<ArgDef>, seq: Seq},
    Func{name: Id, args: Vec<ArgDef>, rettype: Type, expr: Expr},
    Event{name: Id},
    State{name: id, statetype: StateType},
    Var{name: Id, vartype: Type, value: Literal}
}

/**
 * A Sequence of statements/ a block of imperative code
 */
pub type Seq = Vec<Stm>;

pub enum Stm {
    Call{id: Id, args: Vec<Expr>},
    Assign{id: Id, val: Expr},
    Repeat{val: Expr, seq: Seq},
    While{val: Expr, seq: Seq},
    If{cond: Expr, seq: Seq},
    IfElse{cond: Expr, seq: Seq, elifs: Vec<ElseIf>, elseq: Seq},
    Timed{time: Expr, seq: Seq}
}

pub struct ElseIf{cond: Expr, seq: Seq}

pub enum StateType { Bool, U8, U32, I32 }
/**
 * Types below this point are essentially token types
 * Each has associated location data
 */
//An identifier
pub type Id = LocatedSpan;

