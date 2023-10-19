use crate::lex::{lex, Token};


macro_rules! sequence_test {
    ($in:expr, $($expected:expr),+) => {
        let input: &str = $in;
        let expected_out = vec!($($expected),+)
            .into_iter().map(|t| Some(Ok(t)));

        let mut l = lex(input);

        for e in expected_out{
            assert_eq!(l.next(), e);
        }

        println!("macro running");
        assert_eq!(l.next(), None);
    };
}


#[test]
fn test_func_call() {
    sequence_test!("aFunc(identifier1, 4);",
        Token::Identifier("aFunc"),
        Token::LParen,
        Token::Identifier("identifier1"),
        Token::Comma,
        Token::Int("4"),
        Token::RParen,
        Token::Semi);
}

#[test]
fn test_event_dec() {
    sequence_test!("event event_name;",
                   Token::Event,
                   Token::Identifier("event_name"),
                   Token::Semi);
}


#[test]
fn test_graphics_dec(){
    sequence_test!("
        graphics[
        \"file1.png\",
        \"file2.png\",
        \"file_3.png\",
        \"path\\\\to\\\\file.png\"
        ];",
        Token::Graphics,
        Token::LBrace,
        Token::Str("file1.png"),
        Token::Comma,
        Token::Str("file2.png"),
        Token::Comma,
        Token::Str("file_3.png"),
        Token::Comma,
        Token::Str(r"path\\to\\file.png"),
        Token::RBrace,
        Token::Semi
        );
}


#[test]
fn test_repeat() {
    sequence_test!("
        repeat (+ 2 some_func(8.0)) {
            a_proc(7);
            var local2 = (/ another_func() 6);
        }
        ",
        Token::Repeat,
        Token::LParen,
        Token::Plus,
        Token::Int("2"),
        Token::Identifier("some_func"),
        Token::LParen,
        Token::Num("8.0"),
        Token::RParen,
        Token::RParen,
        Token::LBlock,
        Token::Identifier("a_proc"),
        Token::LParen,
        Token::Int("7"),
        Token::RParen,
        Token::Semi,
        Token::Var,
        Token::Identifier("local2"),
        Token::Eq,
        Token::LParen,
        Token::Slash,
        Token::Identifier("another_func"),
        Token::LParen,
        Token::RParen,
        Token::Int("6"),
        Token::RParen,
        Token::Semi,
        Token::RBlock
    );
}

#[test]
fn test_func_dec() {
    sequence_test!("
        func a_func(a: int, b: str, c: num): bool =
            (& 
                (< a len(b))
                (>= a c)
            )
        ",
        Token::Func,
        Token::Identifier("a_func"),
        Token::LParen,
        Token::Identifier("a"),
        Token::Colon,
        Token::IntType,
        Token::Comma,
        Token::Identifier("b"),
        Token::Colon,
        Token::StrType,
        Token::Comma,
        Token::Identifier("c"),
        Token::Colon,
        Token::NumType,
        Token::RParen,
        Token::Colon,
        Token::BoolType,
        Token::Eq,
        Token::LParen,
        Token::And,
        Token::LParen,
        Token::Lt,
        Token::Identifier("a"),
        Token::Identifier("len"),
        Token::LParen,
        Token::Identifier("b"),
        Token::RParen,
        Token::RParen,
        Token::LParen,
        Token::Geq,
        Token::Identifier("a"),
        Token::Identifier("c"),
        Token::RParen,
        Token::RParen
    );
}
