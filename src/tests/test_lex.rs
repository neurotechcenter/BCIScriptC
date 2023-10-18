use crate::lex::{lex, Token};
use lazy_static::lazy_static;


lazy_static!{
    static ref KEYWORDS: Vec<(&'static str, Option<Result<Token<'static>, ()>>)> = vec!(
        ("actor", Token::Actor),
        ("when", Token::OnEvent),
        ("proc", Token::Proc),
        ("func",Token::Func),
        ("event", Token::Event),
        ("timer", Token::Timer),
        ("stateevent", Token::StateEvent),
        ("var", Token::Var),
        ("graphics", Token::Graphics),
        ("sounds", Token::Sounds),
        ("param", Token::Param),
        ("repeat", Token::Repeat),
        ("while", Token::While),
        ("if", Token::If),
        ("else", Token::Else),
        ("elif", Token::Elif),
    ).into_iter().map(|(s, k)| (s, Some(Ok(k)))).collect();
}

#[test]
fn test_keyword() {
    for (kws, kwo) in KEYWORDS.iter() {
        let mut l = lex(kws);
        assert_eq!(&l.next(), kwo);
        assert_eq!(l.next(), None)
    }
}

#[test]
fn test_delimiters() {
    let input = vec!{
        ("{", Token::LBlock),
        ("}", Token::RBlock),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("[", Token::LBrace),
        ("]", Token::RBrace)
    };
    
    for (d, t) in input {
        let mut l = lex(d);
        assert_eq!(l.next(), Some(Ok(t)));
        assert_eq!(l.next(), None);
    }
}

#[test]
fn test_puctuation() {
    let input = vec!{
    (";", Token::Semi),
    (",", Token::Comma),
    (".", Token::Dot),
    ("+", Token::Plus),
    ("-", Token::Minus),
    ("*", Token::Star),
    ("/", Token::Slash),
    ("<", Token::Lt),
    (">", Token::Gt),
    ("<=", Token::Leq),
    (">=", Token::Geq),
    ("=", Token::Eq),
    ("==", Token::EqEq),
    ("%", Token::Percent)
    };
    
    for (d,t) in input {
        let mut l = lex(d);
        assert_eq!(l.next(), Some(Ok(t)));
        assert_eq!(l.next(), None);
    }
}

#[test]
fn test_int() {
    let input = vec! {
        ("0", "0"),
        ("1", "1"),
        ("100", "100"),
        (" 44 ", "44"),
        ("\n29", "29"),
        ("-4", "-4"),
        (" -54", "-54"),
        ("\n-5", "-5"),
    };

    for (s,e) in input {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Int(e))));
        assert_eq!(l.next(), None);
    }
}

#[test]
fn test_float() {
    let input = vec! {
        ("0.0", "0.0"),
        ("1.2", "1.2"),
        ("0.1", "0.8"),
        (" 0.1", "0.1"),
        (" 2.812 ", "2.812"),
        ("22222.8", "22222.8"),
        ("-1.0", "-1.0"),
        (" -1.0", "-1.0")
    };

    for (s,e) in input {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Num(e))));
        assert_eq!(l.next(), None);
    }
}

#[test]
fn test_bool() {
    let intrue = vec! {
        "true",
        " true ",
    };

    for s in intrue {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Bool("true"))));
        assert_eq!(l.next(), None);
    }
    let infalse = vec! {
        "false",
        " false ",
    };

    for s in infalse {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Bool("false"))));
        assert_eq!(l.next(), None);
    }
}


#[test]
fn test_comment() {
    let input = vec!{
        "/**/",
        "/***/",
        "/* aaa */"
    };

    for inp in input {
        let mut l = lex(inp);
        assert_eq!(l.next(), None, "Lexer failed to match comment with form '{}'", inp)
    }
}
