extern crate nom_locate;

use nom_locate::LocatedSpan;

pub type Program<'a> = Vec<Def<'a>>;

pub type Span<'a> = LocatedSpan<&'a str, String>;


pub enum Def<'a> {
    Actor{name: Id<'a>, members: Vec<Def<'a>>},
    OnEvent{name: Id<'a>, seq: Seq<'a>},
    Proc{name: Id<'a>, args: Vec<ArgDef<'a>>, seq: Seq<'a>},
    Func{name: Id<'a>, args: Vec<ArgDef<'a>>, rettype: Type, expr: Expr<'a>},
    Event{name: Id<'a>},
    StateEvent{name: Id<'a>},
    Timer{name: Id<'a>},
    State{name: Id<'a>, statetype: StateType},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Expr<'a>>},
    Graphics{files: Vec<Literal<'a>>},
    Sounds{files: Vec<Literal<'a>>}
}

/**
 * A Sequence of statements/ a block of imperative code
 */
pub type Seq<'a> = Vec<Stm<'a>>;

pub enum Stm<'a> {
    Call{id: Id<'a>, args: Vec<Expr<'a>>, is_builtin: Option<bool>},
    Assign{id: Id<'a>, val: Expr<'a>},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Expr<'a>>},
    Timer{name: Id<'a>, cmd: TimerCmd},
    Repeat{kw: Span<'a>, val: Expr<'a>, seq: Seq<'a>},
    While{kw: Span<'a>, cond: Expr<'a>, seq: Seq<'a>},
    If{kw: Span<'a>, cond: Expr<'a>, seq: Seq<'a>},
    IfElse{kw: Span<'a>, cond: Expr<'a>, seq: Seq<'a>, elifs: Vec<ElseIf<'a>>, elseq: Seq<'a>},
    Timed{kw: Span<'a>, time: Expr<'a>, seq: Seq<'a>},
    CallEvent{tp: Option<EvType>, name: Id<'a>}
}

pub struct ElseIf<'a>{pub kw: Span<'a>, pub cond: Expr<'a>, pub seq: Seq<'a>}

#[derive(Clone)]
pub enum StateType { Bool, U8, I8, U32, I32 }

pub struct ArgDef<'a>{ pub name: Id<'a>, pub argtype: Type }

pub enum TimerCmd {Add, Start, Stop, Reset, Read}

pub enum EvType {BCISEvent, StateEvent}

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
#[derive(Clone, Copy, PartialEq)]
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
    FuncCall(FuncCall<'a>),
    TimerCall{name: Id<'a>, cmd: TimerCmd}
}

pub enum Literal<'a>{
    IntLiteral(Span<'a>),
    NumLiteral(Span<'a>),
    BoolLiteral(Span<'a>),
    StringLiteral(Span<'a>)
}
impl Literal<'_> {
    pub fn str(&self) -> &str {
        match self {
            Self::IntLiteral(i) => i.fragment(),
            Self::NumLiteral(i) => i.fragment(),
            Self::BoolLiteral(i) => i.fragment(),
            Self::StringLiteral(i) => i.fragment(),
        }
    }
}

/**
 * A call to a function
 */
pub struct FuncCall<'a> {
    pub id: Id<'a>,
    pub args: Vec<Expr<'a>>,
    pub is_builtin: Option<bool>
}


pub type BinOp<'a> = Span<'a>;
pub type UnOp<'a> = Span<'a>;

/**
 * Types below this point are essentially token types
 * Each has associated location data
 */
//An identifier
pub type Id<'a> = Span<'a>;
