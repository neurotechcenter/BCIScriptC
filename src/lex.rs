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
   Keyword(Keyword), 
   Delimiter(Delimiter),
   Punc(Punc),
   Literal(Literal<'input>),
   #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |l| l.slice())] //Identifiers must be alphanumeric and begin with a letter
   Identifier(&'input str),
   /* Matches block comments formatted like this one. */
   #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |l| l.slice())]
   Comment(&'input str),
   #[token(r"\n", newline)]
   Newline,
}

#[derive(Logos, Debug, PartialEq)]
pub enum Keyword {
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
    Elif
}

#[derive(Logos, Debug, PartialEq)]
pub enum Delimiter {
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
    RBrace
}

#[derive(Logos, Debug, PartialEq)]
pub enum Punc {
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
    Percent
}

#[derive(Logos, Debug, PartialEq)]
pub enum Literal<'input> {
    #[regex(r"[-]?[0-9]+", |l| l.slice())]
    Int(&'input str),
    #[regex(r"[+-]?[0-9]+.[0-9]+", |l| l.slice())]
    Num(&'input str),
    #[regex("true|false", |l| l.slice())]
    Bool(&'input str),
    #[regex("\"(?:[^\"]|\\\\\")*\"", |l| l.slice())]
    Str(&'input str)
}


fn newline<'input>(lex: &mut Lexer<'input, Token<'input>>) -> Skip {
    lex.extras += 1;
    return Skip;
}
