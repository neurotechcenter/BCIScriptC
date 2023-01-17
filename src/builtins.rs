use std::collections::HashMap;
use lazy_static::lazy_static;

use crate::ast::Type;
use crate::ast::Token;
use crate::parse::Span;
use crate::ast::ArgDef;
use crate::verify::Signature;

//The name of the hypothetical file where these built-in definitions are stored
//this is shown to the user when there is a compile error regarding these.
lazy_static!{
static ref BUILTIN_FILE_NAME: String = String::from("core");
}

macro_rules! emp_span {
    () => {Span::new_extra("  ", BUILTIN_FILE_NAME.to_string())};
}

macro_rules! proc{
    ($name:literal,$($tp:ident),*) => {
        {
            let mut v = Vec::new();
            $( v.push(ArgDef{name: Token{content:String::new(), position: emp_span!(), file:BUILTIN_FILE_NAME.clone()}, argtype: Type::$tp}); )*
            FakeSignature::Proc{name: Token{content: String::from($name), position: emp_span!(), file: BUILTIN_FILE_NAME.clone()}, args: v}
        }
    };
    ($name:literal) => {
        FakeSignature::Proc{name: Token{content: String::from($name), position: emp_span!(), file: BUILTIN_FILE_NAME.clone()}, args: Vec::new()}
    };
}

macro_rules! func{
    ($name:literal,$ret:ident,$($tp:ident),*) => {
        {
            let mut v: Vec<ArgDef> = Vec::new();
            $( v.push(ArgDef{name: Token{content: String::new(), position: emp_span!(), file: BUILTIN_FILE_NAME.clone()}, argtype: Type::$tp}); )*
            FakeSignature::Func{name: Token{content: String::from($name), position: emp_span!(), file: BUILTIN_FILE_NAME.clone()}, args: v, rettype: Type::$ret}
        }
    };
    ($name:literal,$ret:ident) => {
        FakeSignature::Func{name: Token{content: String::new("  "), position: Span::new(), file: BUILTIN_FILE_NAME}, rettype: Type::$ret, args: &Vec::new()}
    }
}

macro_rules! strsn{
    ($l:literal,$r:literal) => {($l, format!(".addNormalBlock([&] (Sequence& callingSequence) {{{}}})\n", $r))}
}

/*
 * Had to make a separate static vec of fake signatures which own their resources,
 *  which are then copied into the HashMap of signatures, as otherwise the values are not owned by
 *  anything.
 */

enum FakeSignature<'a>{
    Func{name: Token<'a>, rettype: Type, args: Vec<ArgDef<'a>> },
    Proc{name: Token<'a>, args: Vec<ArgDef<'a>> },
    Event{name: Token<'a>}
}

impl FakeSignature<'_> {
    pub fn sig(&self) -> Signature {
        match self {
            FakeSignature::Func { name, rettype, args } => Signature::Func { name: &name,  args: &args, rettype: *rettype, referenced_symbols: Vec::new(), is_builtin: true },
            FakeSignature::Proc { name, args } => Signature::Proc { name: &name, args: &args, is_builtin: true },
            FakeSignature::Event { name } => Signature::Event { name: &name }
        }
    }
    pub fn name(&self) -> String {
        match self {
            FakeSignature::Event{name, ..} => name.content.clone(),
            FakeSignature::Func{name, ..} => name.content.clone(),
            FakeSignature::Proc{name, ..} => name.content.clone()
        }
    }
}
lazy_static!{
static ref BUILTINS_GLOBAL_OWNED: Vec<FakeSignature<'static>> = vec![
    FakeSignature::Event{name: Token{content: String::from("start"), position: emp_span!(), file: BUILTIN_FILE_NAME.clone()}},
    func!("truncate", Int, Num),
    func!("intToStr", Str, Int),
    func!("randInt", Int, Int, Int),
    func!("rand", Num, Num, Num)
];
}

pub fn get_builtins_global() -> HashMap<String, Signature<'static>>{
    BUILTINS_GLOBAL_OWNED.iter().map(|s| (s.name(), s.sig())).collect()
}

lazy_static!{
static ref BUILTINS_ACTOR_OWNED: Vec<FakeSignature<'static>> = vec![
    proc!("move", Num, Num),
    proc!("moveTo", Num, Num),
    proc!("graphic", Int),
    proc!("nextGraphic", Int),
    proc!("displayStr", Str),
    proc!("setSize", Num),
    proc!("setSize2D", Num, Num),
    proc!("displayAsText"),
    proc!("displayAsImage"),
    proc!("waitForProcess"),
    proc!("wait", Num),
];
}

pub fn get_builtins_actor() -> HashMap<String, Signature<'static>> {
    BUILTINS_ACTOR_OWNED.iter().map(|s| (s.name(), s.sig())).collect()
}

lazy_static!{
pub static ref BUILTIN_SUBS: HashMap<&'static str, String> = [
    strsn!("move", "callingSequence.setPositionX(callingSequence.positionX() + $0); callingSequence.setPositionY(callingSequence.positionX + $1);"),
    strsn!("moveTo", "callingSequence.setPositionX($0); callingSequence.setPositionY($1);"),
    strsn!("graphic", "callingSequence.setGraphic($0);"),
    strsn!("nextGraphic", "callingSequence.advanceGraphic();"),
    strsn!("displayStr", "callingSequence.setDisplayStr($0)"),
    strsn!("setSize", "callingSequence.setSize($0, $0)"),
    strsn!("setSize2D", "callingSequence.setSize($0, $1)"),
    strsn!("displayAsText", "Set display mode"), //have to complete implementation of sequence
    strsn!("displayAsImage", "Set display mode"),
    strsn!("truncate", "((int) $0)"),
    strsn!("intToStr", "std::to_string($0)"),
    strsn!("randInt", "callingSequence.randInt($0, $1)"),
    strsn!("rand", "callingSequence.rand($0, $1)")
].to_vec().into_iter().collect();
}

//The binary operators, with the first two types in the tuple being inputs, and the last type being
//output.
lazy_static!{
pub static ref BINARY_OPERATORS: HashMap<&'static str, Vec<(Type, Type, Type)>> = [
    ("+", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num), (Type::Str, Type::Str, Type::Str))),
    ("-", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("*", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("/", vec!((Type::Int, Type::Int, Type::Num), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("&&", vec!((Type::Bool, Type::Bool, Type::Bool))),
    ("||", vec!((Type::Bool, Type::Bool, Type::Bool))),
    ("==", vec!((Type::Bool, Type::Bool, Type::Bool), (Type::Int, Type::Int, Type::Bool), (Type::Int, Type::Num, Type::Bool),
        (Type::Num, Type::Int, Type::Bool), (Type::Str, Type::Str, Type::Bool))),
].to_vec().into_iter().collect();


pub static ref UNARY_OPERATORS: HashMap<&'static str, Vec<(Type, Type)>> = [
    ("-", vec!((Type::Int, Type::Int), (Type::Num, Type::Num))),
    ("!", vec!((Type::Bool, Type::Bool)))
].to_vec().into_iter().collect();

pub static ref BINARY_OP_SUB: HashMap<&'static str, &'static str> = vec![
    ("+","+"),
    ("-","-"),
    ("*","*"),
    ("/","/"),
    ("=","=="),
    ("&","&&"),
    ("|","||")
].into_iter().collect();

pub static ref UNARY_OP_SUB: HashMap<&'static str,&'static str> = vec![
    ("-","-"),
    ("!","!")
].into_iter().collect();
}
