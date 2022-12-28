extern crate nom_locate;
use std::fmt::Display;

use nom_locate::LocatedSpan;

pub type Program<'a> = Vec<Def<'a>>;

pub type Span<'a> = LocatedSpan<&'a str, String>;


pub enum Def<'a> {
    Actor{name: Id<'a>, members: Vec<Def<'a>>},
    OnEvent{name: Id<'a>, seq: Seq<'a>},
    Proc{name: Id<'a>, args: Vec<ArgDef<'a>>, seq: Seq<'a>},
    Func{name: Id<'a>, args: Vec<ArgDef<'a>>, rettype: Type, expr: Expr<'a>},
    Event{name: Id<'a>},
    State{name: Id<'a>, statetype: StateType},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Literal<'a>>},
    Graphics{files: Vec<StrLit<'a>>},
    Sounds{files: Vec<StrLit<'a>>}
}

/**
 * A Sequence of statements/ a block of imperative code
 */
pub type Seq<'a> = Vec<Stm<'a>>;

pub enum Stm<'a> {
    Call{id: Id<'a>, args: Vec<Expr<'a>>},
    Assign{id: Id<'a>, val: Expr<'a>},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Expr<'a>>},
    Repeat{val: Expr<'a>, seq: Seq<'a>},
    While{cond: Expr<'a>, seq: Seq<'a>},
    If{cond: Expr<'a>, seq: Seq<'a>},
    IfElse{cond: Expr<'a>, seq: Seq<'a>, elifs: Vec<ElseIf<'a>>, elseq: Seq<'a>},
    Timed{time: Expr<'a>, seq: Seq<'a>}
}

pub struct ElseIf<'a>{pub cond: Expr<'a>, pub seq: Seq<'a>}

#[derive(Clone)]
pub enum StateType { Bool, U8, I8, U32, I32 }

pub struct ArgDef<'a>{ pub name: Id<'a>, pub argtype: Type }


/**
 * An expression with a type
 * expressions are given type in the verification phase
 */
pub struct Expr<'a>{
    pub expr: UExpr<'a>,
    pub exprtype: Option<Type>
}

/**
 * An untyped expression
 */
pub enum UExpr<'a>{
    BinExpr{ l: Box<Expr<'a>>, op: BinOp<'a>, r: Box<Expr<'a>> },
    UnExpr{ op: UnOp<'a>, expr: Box<Expr<'a>> },
    Value(Value<'a>)
}

/**
 * Possible value types
 */
#[derive(Clone)]
pub enum Type {
    Int,
    Num,
    Bool,
    Str
}

impl Type {
    pub fn bcis_rep(&self) -> String {
        match self {
            Type::Int => "int",
            Type::Num => "num",
            Type::Bool => "bool",
            Type::Str => "str"
        }.to_string()
    }

    pub fn cpp_rep(&self) -> String {
        match self {
            Type::Int => "int",
            Type::Num => "double",
            Type::Bool => "bool",
            Type::Str => "std::string"
        }.to_string()
    }

}

pub enum Value<'a> {
    Literal(Literal<'a>),
    VarCall(Id<'a>),
    FuncCall(FuncCall<'a>)
}

pub enum Literal<'a>{
    IntLiteral(IntLit<'a>),
    NumLiteral(NumLit<'a>),
    BoolLiteral(BoolLit<'a>),
    StringLiteral(StrLit<'a>)
}

/**
 * A call to a function
 */
pub struct FuncCall<'a> {
    pub id: Id<'a>,
    pub args: Vec<Expr<'a>>
}

pub enum BinOp<'a> {
    Add(Add<'a>),
    Sub(Sub<'a>),
    Mult(Mult<'a>),
    Div(Div<'a>),
    And(And<'a>),
    Or(Or<'a>)
}

pub enum UnOp<'a>{
    Neg(Neg<'a>),
    Not(Not<'a>)
}

/**
 * Types below this point are essentially token types
 * Each has associated location data
 */
//An identifier
pub type Id<'a> = Span<'a>;

/**
 * Data value types
 */
pub type IntLit<'a> = Span<'a>;
pub type NumLit<'a> = Span<'a>;
pub type BoolLit<'a> = Span<'a>;
pub type StrLit<'a> = Span<'a>;

/**
 * Operators
 */
pub type Add<'a> = Span<'a>;
pub type Sub<'a> = Span<'a>;
pub type Mult<'a> = Span<'a>;
pub type Div<'a> = Span<'a>;
pub type And<'a> = Span<'a>;
pub type Or<'a> = Span<'a>;
pub type Neg<'a> = Span<'a>;
pub type Not<'a> = Span<'a>;
