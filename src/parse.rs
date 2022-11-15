extern crate nom;
use nom::{
    IResult,
    error::ParseError,
    multi::many0,
    multi::separated_list0,
    branch::alt,
    sequence::delimited,
    sequence::tuple,
    character::complete::multispace0,
    character::complete::char,
    character::complete::alpha1,
    character::complete::alphanumeric0,
    bytes::complete::tag,
    combinator::opt,
    combinator::eof
};
use nom_locate::LocatedSpan;
use crate::ast::*;

type Span<'a> = LocatedSpan<&'a str>;

pub fn program(inp: Span) -> Result<Program, Box<dyn std::error::Error>> {
    Ok(tuple((multispace0, many0(def), eof))(inp)?.1.1)
}

fn def(inp: Span) -> IResult<Span, Def> {
    alt((
        actor,
        on_event,
        proc,
        func,
        event,
        state,
        var))(inp)
}

fn actor(inp: Span) -> IResult<Span, Def> { 
    let (rest, val) = tuple((tag("actor "), id, many0(def)))(inp)?;
    Ok((rest, Def::Actor{ name: val.1, members: val.2 }))
}

fn on_event(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("when "), id, block_sequence))(inp)?;
    Ok((rest, Def::OnEvent{name: val.1, seq: val.2}))
}

fn proc(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("proc "), id, arg_defs, block_sequence))(inp)?;
    Ok((rest, Def::Proc{name: val.1, args: val.2, seq: val.3}))
}

fn func(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("func "), id, arg_defs, data_type, expr))(inp)?;
    Ok((rest, Def::Func{name: val.1, args: val.2, rettype: val.3, expr: val.4 }))
}

fn event(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("event "), id))(inp)?;
    Ok((rest, Def::Event{name: val.1}))
}

fn state(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("state "), id, state_type))(inp)?;
    Ok((rest, Def::State{name: val.1, statetype: val.2}))
}

fn var(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("var "), id, lex(char(':')), data_type, opt(tuple((lex(char('=')), literal))), semi))(inp)?;
    Ok((rest, Def::Var{name: val.1, vartype: val.3, 
        value: 
            match val.4{
                Some((a, b)) => Some(b),
                None => None
            }
    }))
}

fn graphics(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("graphics"), delimited(lex(char('[')), separated_list0(lex(char(',')), lex(str_lit)), char(']'))))(inp)?;
    Ok((rest, Def::Graphics{files: val.1}))
}

fn sounds(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("sounds"), delimited(lex(char('[')), separated_list0(lex(char(',')), lex(str_lit)), char(']'))))(inp)?;
    Ok((rest, Def::Sounds{files: val.1}))
}

fn block_sequence(inp: Span) -> IResult<Span, Vec<Stm>> {
    delimited(lex(char('{')), many0(stm), lex(char('}')))(inp)
}

fn stm (inp: Span) -> IResult<Span, Stm> {
    alt((
        call,
        assign,
        local_var,
        loop_repeat,
        loop_while,
        cond_ifelse,
        cond_if,
        timed
        ))(inp)
}

fn call(inp: Span) -> IResult<Span, Stm>{
    let (out, val) = tuple((id, arg_list, semi))(inp)?;
    Ok((out, Stm::Call{id: val.0, args: val.1}))
}

fn assign(inp: Span) -> IResult<Span, Stm>{
    let (out, val) = tuple((id, lex(char('=')), expr, semi))(inp)?;
    Ok((out, Stm::Assign{id: val.0, val: val.2}))
}

fn loop_repeat(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("repeat"), expr, block_sequence))(inp)?;
    Ok((out, Stm::Repeat{val: val.1, seq: val.2}))
}

fn loop_while(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("while"), expr, block_sequence))(inp)?;
    Ok((out, Stm::While{cond: val.1, seq: val.2}))
}

fn local_var(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("var "), id, lex(char(':')), data_type, opt(tuple((lex(char('=')), expr))), semi))(inp)?;
    Ok((out, Stm::Var{name: val.1, vartype: val.3, value: 
        match val.4{
            Some((a,b)) => Some(b),
            None => None
        }
    }))
}

fn cond_if(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("if"), expr, block_sequence))(inp)?;
    Ok((out, Stm::If{cond: val.1, seq: val.2}))
}

fn cond_ifelse(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("if"), expr, block_sequence,  many0(else_if), opt(tuple((tag("else"), block_sequence)))))(inp)?;
    Ok((out, Stm::IfElse{cond: val.1, seq: val.2, elifs: val.3, elseq: 
        match val.4 {
            Some((a, b)) => b?,
            None => Vec::new()
        }
    }))
}

fn else_if(inp: Span) -> IResult<Span, ElseIf> {
    let (out, val) = tuple((tag("else if"), expr, block_sequence))(inp)?;
    Ok((out, ElseIf{cond: val.1, seq: val.2}))
}

fn timed(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("timed"), expr, block_sequence))(inp)?;
    Ok((out, Stm::Timed{time: val.1, seq: val.2}))
}


fn arg_defs(inp: Span) -> IResult<Span, Vec<ArgDef>> {
    separated_list0(tag(","), lex(arg_def))(inp)
}

fn arg_def (inp: Span) -> IResult<Span, ArgDef> {
    let (inp, name) = lex(id)(inp)?;
    let (inp, _) = lex(char(':'))(inp)?;
    let (inp, arg_type) = lex(data_type)(inp)?;
    return Ok((inp, ArgDef{ name: name, argtype: arg_type }));
}

fn arg_list (inp: Span) -> IResult<Span, Vec<Expr>> {
    delimited(char('('), separated_list0(lex(char(',')), expr), char(')'))(inp)
}

fn state_type(inp: Span) -> IResult<Span, StateType> {
    let (out, ret) = alt((lex(tag("bool")), lex(tag("u8")), lex(tag("i8")), lex(tag("u32")), lex(tag("i32"))))(inp)?;
    match ret.fragment()
    {
     x if x == &"bool" => Ok((out, StateType::Bool)),
     x if x == &"u8" => Ok((out, StateType::U8)),
     x if x == &"i8" => Ok((out, StateType::I8)),
     x if x == &"u32" => Ok((out, StateType::U32)),
     x if x == &"i32" => Ok((out, StateType::I32)),
     _ => Err(nom::Err::Failure(nom::error::Error::new(Span::new("This error should not happen"), nom::error::ErrorKind::Fail )))
    }
}

fn data_type(inp: Span) -> IResult<Span, Type> {
    let (out, ret) = alt((lex(tag("bool")), lex(tag("int")), lex(tag("num")), lex(tag("str"))))(inp)?;
    match ret.fragment()
    {
        x if x == &"bool" => Ok((out, Type::Bool)),
        x if x == &"int" => Ok((out, Type::Int)),
        x if x == &"num" => Ok((out, Type::Num)),
        x if x == &"str" => Ok((out, Type::Str)),
        _ => Err(nom::Err::Failure(nom::error::Error::new(Span::new("This error should not happen"), nom::error::ErrorKind::Fail )))
    
    }
}

fn id (inp: Span) -> IResult<Span, Id> {
    lex(id2)(inp)
}

fn id2 (inp: Span) -> IResult<Span, Id> {
    let (inp, start) = alpha1(inp)?;
    let (inp, end) = alphanumeric0(inp)?;
    return Ok((inp, Span::new_from_raw_offset(start.location_offset(), start.location_line(), String::from(*start.fragment()).push_str(end.fragment()), ())));
}

fn semi (inp: Span) -> IResult<Span, Id> {
    nom::combinator::map(char(';'), |s| String::from(s))(inp)
}
/**
 *  Equivalent of parsec's 'lexeme'
 *  Wraps a combinator to consume preceding and trailing whitespace
 *  used by every text-level parser used in this file except tags with required whitespace
 */
fn lex<'a, F: 'a, O, E: ParseError<Span<'a>>>(inner: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
    where 
    F: Fn(Span) -> IResult<Span, O, E>,
    {
        delimited(multispace0, inner, multispace0)
    }

