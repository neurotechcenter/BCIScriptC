use std::cell::RefCell;

extern crate nom_locate;



pub type Program<'a> = Vec<Def<'a>>;

#[derive(Clone)]
pub struct Token<'a> {
    pub content: String,
    pub position: crate::parse::Span<'a>,
    pub file: String,
}

pub enum Def<'a> {
    Actor{name: Id<'a>, members: Vec<Def<'a>>},
    OnEvent{name: Id<'a>, seq: Seq<'a>},
    Proc{name: Id<'a>, args: Vec<ArgDef<'a>>, seq: Seq<'a>},
    Func{name: Id<'a>, args: Vec<ArgDef<'a>>, rettype: Type, expr: Expr<'a>, init_priority: RefCell<Option<u64>>},
    Event{name: Id<'a>},
    StateEvent{name: Id<'a>},
    Timer{name: Id<'a>},
    State{name: Id<'a>, statetype: StateType},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Expr<'a>>, init_priority: RefCell<Option<u64>>},
    Graphics{files: Vec<Literal<'a>>},
    Sounds{files: Vec<Literal<'a>>}
}

/**
 * A Sequence of statements/ a block of imperative code
 */
pub type Seq<'a> = Vec<Stm<'a>>;

//for explanation on why RefCell is used, see the definition of verify in verify.
pub enum Stm<'a> {
    Call{id: Id<'a>, args: Vec<Expr<'a>>, is_builtin: RefCell<Option<bool>>},
    Assign{id: Id<'a>, val: Expr<'a>},
    Var{name: Id<'a>, vartype: Option<Type>, value: Option<Expr<'a>>},
    Timer{name: Id<'a>, cmd: TimerCmd},
    Repeat{kw: Token<'a>, val: Expr<'a>, seq: Seq<'a>},
    While{kw: Token<'a>, cond: Expr<'a>, seq: Seq<'a>},
    If{kw: Token<'a>, cond: Expr<'a>, seq: Seq<'a>},
    IfElse{kw: Token<'a>, cond: Expr<'a>, seq: Seq<'a>, elifs: Vec<ElseIf<'a>>, elseq: Seq<'a>},
    Timed{kw: Token<'a>, time: Expr<'a>, seq: Seq<'a>},
    CallEvent{tp: Option<EvType>, name: Id<'a>}
}

pub struct ElseIf<'a>{pub kw: Token<'a>, pub cond: Expr<'a>, pub seq: Seq<'a>}

#[derive(Clone)]
pub enum StateType { Bool, U8, I8, U32, I32 }

#[derive(Clone)]
pub struct ArgDef<'a>{ pub name: Id<'a>, pub argtype: Type }

pub enum TimerCmd {Add, Start, Stop, Reset, Read}

pub enum EvType {BCISEvent, StateEvent}

/**
 * An expression with a type
 * expressions are given type in the verification phase
 */
pub struct Expr<'a>{
    pub expr: UExpr<'a>,
    pub exprtype: RefCell<Option<Type>>
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
    IntLiteral(Token<'a>),
    NumLiteral(Token<'a>),
    BoolLiteral(Token<'a>),
    StringLiteral(Token<'a>)
}
impl Literal<'_> {
    pub fn str(&self) -> &'_ String {
        match self {
            Self::IntLiteral(i) | Self::NumLiteral(i) | Self::BoolLiteral(i) | Self::StringLiteral(i) => &i.content
        }
    }
}

/**
 * A call to a function
 */
pub struct FuncCall<'a> {
    pub id: Id<'a>,
    pub args: Vec<Expr<'a>>,
    pub is_builtin: RefCell<Option<bool>>
}


pub type BinOp<'a> = Token<'a>;
pub type UnOp<'a> = Token<'a>;

pub type Id<'a> = Token<'a>;
