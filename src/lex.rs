#![allow(dead_code)] //remove when done
use logos::{Logos, Lexer, Skip};

pub struct LexedFile<'input> {
    filename: String,
    tokens: Vec<Token<'input>>
}

pub fn lex<'input>(inp: &'input str) -> Lexer<'input, Token<'input>> {
    return Token::lexer(inp);
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = usize)] //Holds current line number
#[logos(skip r"[ \t\f\r]+")]
pub enum Token<'input> {
   //Keywords
    #[token("actor")]
    Actor,
    #[token("when")]
    OnEvent,
    #[token("proc")]
    Proc,
    #[token("func")]
    Func,
    #[token("event")]
    Event,
    #[token("timer")]
    Timer,
    #[token("stateevent")]
    StateEvent,
    #[token("var")]
    Var,
    #[token("graphics")]
    Graphics,
    #[token("sounds")]
    Sounds,
    #[token("param")]
    Param,
    #[token("repeat")]
    Repeat,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,


    //Types
    #[token("int")]
    IntType,
    #[token("num")]
    NumType,
    #[token("bool")]
    BoolType,
    #[token("str")]
    StrType,

    //Delimiters

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBlock,
    #[token("}")]
    RBlock,
    #[token("[")]
    LBrace,
    #[token("]")]
    RBrace,

    //Punctuation
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token("%")]
    Percent,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("^")]
    Xor,

    // Literals
    #[regex(r"[-]?[0-9]+", |l| l.slice())]
    Int(&'input str),
    #[regex(r"[+-]?[0-9]+\.[0-9]+", |l| l.slice())]
    Num(&'input str),
    #[regex("true|false", |l| l.slice())]
    Bool(&'input str),
    #[regex("\"(?:[^\"]|\\\\\")*\"", str_proc)]
    Str(&'input str), 

            
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |l| l.slice())] //Identifiers must be alphanumeric and must not begin with a number
    Identifier(&'input str),
   
    /* Matches block comments formatted like this one. */
    #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |_l| Skip)]
    Comment,
    
    #[token("\n", newline)]
    Newline
}


pub enum TokenKind {
    Keyword,
    Delimiter,
    Punc,
    Literal,
    Identifier,
    Comment,
    Other
}


fn newline<'input>(l: &mut Lexer<'input, Token<'input>>) -> Skip {
    l.extras += 1;
    return Skip;
}

/*
 * Strips away delimiting quotation marks
 */
fn str_proc<'input>(l: &mut Lexer<'input, Token<'input>>) -> &'input str {
    let len = l.slice().len();
    let s: &'input str = l.slice();
    let o: &'input str = &s[1..len-1];
    return o;
}
