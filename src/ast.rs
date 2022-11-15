extern crate nom_locate;
use nom_locate::LocatedSpan;

pub type Program = Vec<Def>;

pub enum Def {
    Actor{name: Id, members: Vec<Def>},
    OnEvent{name: Id, seq: Seq},
    Proc{name: Id, args: Vec<ArgDef>, seq: Seq},
    Func{name: Id, args: Vec<ArgDef>, rettype: Type, expr: Expr},
    Event{name: Id},
    State{name: Id, statetype: StateType},
    Var{name: Id, vartype: Type, value: Option<Literal>},
    Graphics{files: Vec<StrLit>},
    Sounds{files: Vec<StrLit>}
}

/**
 * A Sequence of statements/ a block of imperative code
 */
pub type Seq = Vec<Stm>;

pub enum Stm {
    Call{id: Id, args: Vec<Expr>},
    Assign{id: Id, val: Expr},
    Var{name: Id, vartype: Type, value: Option<Expr>},
    Repeat{val: Expr, seq: Seq},
    While{cond: Expr, seq: Seq},
    If{cond: Expr, seq: Seq},
    IfElse{cond: Expr, seq: Seq, elifs: Vec<ElseIf>, elseq: Seq},
    Timed{time: Expr, seq: Seq}
}

pub struct ElseIf{cond: Expr, seq: Seq}

pub enum StateType { Bool, U8, I8, U32, I32 }

pub struct ArgDef{ name: Id, argtype: Type }


/**
 * An expression with a type
 * expressions are given type in the verification phase
 */
pub struct Expr{
    expr: UExpr,
    exprtype: Option<Type>
}

/**
 * An untyped expression
 */
pub enum UExpr{
    BinExpr{ l: Box<Expr>, op: BinOp, r: Box<Expr> },
    UnExpr{ op: UnOp, expr: Box<Expr> },
    Value(Value)
}

/**
 * Possible value types
 */
pub enum Type {
    Int,
    Num,
    Bool,
    Str
}

pub enum Value {
    Literal(Literal),
    VarCall(Id),
    FuncCall(FuncCall)
}

pub enum Literal{
    IntLiteral(IntLit),
    NumLiteral(NumLit),
    BoolLiteral(BoolLit),
    StringLiteral(StrLit)
}

/**
 * A call to a function
 */
pub struct FuncCall {
    id: Id,
    args: Vec<Expr>
}

pub enum BinOp {
    Add(Add),
    Sub(Sub),
    Mult(Mult),
    Div(Div),
    And(And),
    Or(Or)
}

pub enum UnOp{
    Neg(Neg),
    Not(Not)
}

/**
 * Types below this point are essentially token types
 * Each has associated location data
 */
//An identifier
pub type Id = LocatedSpan<String>;

/**
 * Data value types
 */
pub type IntLit = LocatedSpan<i64>;
pub type NumLit = LocatedSpan<f64>;
pub type BoolLit = LocatedSpan<bool>;
pub type StrLit<'a> = LocatedSpan<&'a str>;

/**
 * Operators
 */
pub type Add = LocatedSpan<char>;
pub type Sub = LocatedSpan<char>;
pub type Mult = LocatedSpan<char>;
pub type Div = LocatedSpan<char>;
pub type And = LocatedSpan<char>;
pub type Or = LocatedSpan<char>;
pub type Neg = LocatedSpan<char>;
pub type Not = LocatedSpan<char>;
