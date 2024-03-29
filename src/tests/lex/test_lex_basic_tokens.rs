use crate::lex::{lex, Token};



#[test]
fn test_keyword() {
    let keywords = vec!(
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
    ).into_iter().map(|(s, k)| (s, Some(Ok(k))));

    for (kws, kwo) in keywords {
        let mut l = lex(kws);
        assert_eq!(l.next(), kwo);
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
    (":", Token::Colon),
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
    ("%", Token::Percent),
    ("&", Token::And),
    ("|", Token::Or),
    ("^", Token::Xor)
    };
    
    for (d,t) in input {
        let mut l = lex(d);
        assert_eq!(l.next(), Some(Ok(t)));
        assert_eq!(l.next(), None);
    }
}


#[test]
fn test_ws() {
    let input = vec!{
        " ",
        "  ",
        "\n",
        "\n\n",
        "\n \n",
        "\n "
    };

    for s in input {
        let mut l = lex(s);
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
        ("0.8", "0.8"),
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
fn test_str() {
    let input = vec!{
        ("\"This is a string\"", "This is a string"),
        (" \"This is a string with whitespace outside the delimiters\" ", "This is a string with whitespace outside the delimiters"),
        ("\" This is a string with whitespace inside the delimiters \"", " This is a string with whitespace inside the delimiters "),
        ("\"This is a string \n with a line break\"", "This is a string \n with a line break"),
        ("\"This is a string \\n with an escaped line break\"", "This is a string \\n with an escaped line break"),
        ("\"This is a string with \\\"escaped quotation marks\\\"\"", "This is a string with \\\"escaped quotation marks\\\"")
    };

    for (s, e) in input {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Str(e))));
        assert_eq!(l.next(), None);
    }
}

//TODO: Implement working comments.
/*
#[test]
fn test_comment() {
    let input = vec!{
        "/**/",
        "/***/",
        "/*Comment*/",
        "/*Comment with whitespace*/",
        "/*Comment\nwith\nnewlines*/"
    };

    for inp in input {
        let mut l = lex(inp);
        assert_eq!(l.next(), None, "Lexer failed to match comment with form '{}'", inp)
    }
}
*/

#[test]
fn test_identifier() {
    let input = vec! {
        ("identifier", "identifier"),
        ("identifier2", "identifier2"),
        (" identifier ", "identifier"),
        ("i_dentifie_r", "i_dentifie_r"),
        ("__ID__", "__ID__"),
        ("__2_E_", "__2_E_")
    };

    for (s, e) in input {
        let mut l = lex(s);
        assert_eq!(l.next(), Some(Ok(Token::Identifier(e))));
        assert_eq!(l.next(), None);
    }
}
