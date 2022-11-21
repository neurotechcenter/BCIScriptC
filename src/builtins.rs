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

const BUILTIN_SUBS: Vec<(String, String)> = [
    strs!("move", "callingSequence.setPositionX(callingSequence.positionX() + $0); callingSequence.setPositionY(callingSequence.positionX + $1);"),
    strs!("moveTo", "callingSequence.setPositionX($0); callingSequence.setPositionY($1);"),
    strs!("graphic", "callingSequence.setGraphic($0);"),
    strs!("nextGraphic", "callingSequence.advanceGraphic();"),
    strs!("displayStr", "callingSequence.setDisplayStr($0)"),
    strs!("setSize", "callingSequence.setSize($0, $0)"),
    strs!("setSize2D", "callingSequence.setSize($0, $1)"),
    strs!("displayAsText", "Set display mode"), //have to complete implementation of sequence
    strs!("displayAsImage", "Set display mode"),
    strs!("truncate", "((int) $0)"),
    strs!("intToStr", "std::to_string($0)"),
    strs!("randInt", "callingSequence.randInt($0, $1)"),
    strs!("rand", "callingSequence.rand($0, $1)")
].to_vec();


