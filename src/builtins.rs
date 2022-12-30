use std::collections::HashMap;

use crate::ast::BinOp;
use crate::ast::Type;
use crate::ast::Span;
use crate::verify::Signature;

//The name of the hypothetical file where these built-in definitions are stored
//this is shown to the user when there is a compile error regarding these.
const BUILTIN_FILE_NAME: String = String::from("core");

macro_rules! proc{
    ($name:literal,$($tp:ident),*) => {
        {
            let mut v = Vec::new();
            $( v.push(Type::$tp); )*
            Signature::Proc{name: Span::new_extra($name, BUILTIN_FILE_NAME), args: v}
        }
    };
    ($name:literal) => {
        Signature::Proc{name: Span::new_extra($name, BUILTIN_FILE_NAME), args: Vec::new()}
    };
}

macro_rules! func{
    ($name:literal,$ret:ident,$($tp:ident),*) => {
        {
            let mut v: Vec<Type> = Vec::new();
            $( v.push(Type::$tp); )*
            Signature::Func{name: Span::new_extra($name, BUILTIN_FILE_NAME), args: v, rettype: Type::$ret}
        }
    };
    ($name:literal,$ret:ident) => {
        Signature::Func{name: Span::new_extra($name, BUILTINS_FILE_NAME), rettype: Type::$ret, args: Vec::new()}
    }
}

macro_rules! span{
    ($name:literal) => {Span::new_extra($name, String::from("builtins"))};
}

macro_rules! strs{
    ($l:literal,$r:literal) => {(String::from($l),String::from($r))};
}

macro_rules! strsn{
    ($l:literal,$r:literal) => {(String::from($l), format!(".addNormalBlock([&] (Sequence& callingSequence) {{{}}})\n", $r))}
}
pub const BUILTINS: Vec<Signature> = [
    Signature::Event{name: span!("start")},
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
    func!("truncate", Int, Num),
    func!("intToStr", Str, Int),
    func!("randInt", Int, Int, Int),
    func!("random", Num, Num, Num)
].to_vec();

pub const BUILTIN_SUBS: HashMap<String, String> = [
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


//The binary operators, with the first two types in the tuple being inputs, and the last type being
//output.
pub const BINARY_OPERATORS: HashMap<&str, Vec<(Type, Type, Type)>> = [
    ("+", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num), (Type::Str, Type::Str, Type::Str))),
    ("-", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("*", vec!((Type::Int, Type::Int, Type::Int), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("/", vec!((Type::Int, Type::Int, Type::Num), (Type::Int, Type::Num, Type::Num), (Type::Num, Type::Int, Type::Num))),
    ("&&", vec!((Type::Bool, Type::Bool, Type::Bool))),
    ("||", vec!((Type::Bool, Type::Bool, Type::Bool))),
    ("==", vec!((Type::Bool, Type::Bool, Type::Bool), (Type::Int, Type::Int, Type::Bool), (Type::Int, Type::Num, Type::Bool),
        (Type::Num, Type::Int, Type::Bool), (Type::Str, Type::Str, Type::Bool))),
].to_vec().into_iter().collect();

pub const UNARY_OPERATORS: HashMap<&str, Vec<(Type, Type)>> = [
    ("-", vec!((Type::Int, Type::Int), (Type::Num, Type::Num))),
    ("!", vec!((Type::Bool, Type::Bool)))
].to_vec().into_iter().collect();
