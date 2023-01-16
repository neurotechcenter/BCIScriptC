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
    character::{complete::char, streaming::none_of},
    character::complete::{alpha1, digit0},
    character::complete::alphanumeric0,
    bytes::complete::{tag, escaped},
    combinator::opt,
    combinator::eof,
};
use nom_locate::{LocatedSpan, position};
use crate::ast::*;

pub type Span<'a> = LocatedSpan<&'a str, String>;

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
        timer_def,
        var,
        graphics,
        sounds,
        stateevent))(inp)
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
    Ok((rest, Def::Func{name: val.1, args: val.2, rettype: val.3, expr: val.4, init_priority: None }))
}

fn event(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("event "), id))(inp)?;
    Ok((rest, Def::Event{name: val.1}))
}

fn state(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("state "), id, state_type))(inp)?;
    Ok((rest, Def::State{name: val.1, statetype: val.2}))
}

fn timer_def(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("timer "), id))(inp)?;
    Ok((rest, Def::Timer { name: val.1 }))
}

fn stateevent(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("stateevent "), id))(inp)?;
    Ok((rest, Def::StateEvent { name: val.1 }))
}

fn var(inp: Span) -> IResult<Span, Def> {
    let (rest, val) = tuple((tag("var "), id, opt(tuple((lex(char(':')), data_type))), opt(tuple((lex(char('=')), expr))), semi))(inp)?;
    Ok((rest, Def::Var{name: val.1, 
        vartype: 
            match val.2{ //output if opt(tuple(a,b))) where a is ':' and b is data_type
                Some((a,b)) => Some(b),
                None => None
            }, 
        value: 
            match val.3{
                Some((a, b)) => Some(b),
                None => None
            }, 
        init_priority: None
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
        timed,
        timer,
        callevent
        ))(inp)
}

fn callevent(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("call"), opt(alt((ev_type_b, ev_type_s))), id))(inp)?;
    Ok((out, Stm::CallEvent { tp: val.1, name: val.2 }))
}

fn ev_type_b(inp: Span) -> IResult<Span, EvType> {
    let (out, val) = tag(".b")(inp)?;
    Ok((out, EvType::BCISEvent))
}

fn ev_type_s(inp: Span) -> IResult<Span, EvType> {
    let (out, val) = tag(".s")(inp)?;
    Ok((out, EvType::StateEvent))
}

fn timer(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("timer."), timer_cmd, id))(inp)?;
    Ok((out, Stm::Timer { name: val.2, cmd: val.1 }))
}

fn timer_cmd(inp: Span) -> IResult<Span, TimerCmd> {
    alt((timer_cmd_add, timer_cmd_stop, timer_cmd_start, timer_cmd_reset, timer_cmd_read))(inp)
}

fn timer_cmd_add(inp: Span) -> IResult<Span, TimerCmd> {
    let (out, val) = tag("add.")(inp)?;
    Ok((out, TimerCmd::Add))
}

fn timer_cmd_stop(inp: Span) -> IResult<Span, TimerCmd> {
    let (out, val) = tag("stop.")(inp)?;
    Ok((out, TimerCmd::Stop))
}

fn timer_cmd_start(inp: Span) -> IResult<Span, TimerCmd> {
    let (out, val) = tag("start.")(inp)?;
    Ok((out, TimerCmd::Start))
}

fn timer_cmd_reset(inp: Span) -> IResult<Span, TimerCmd> {
    let (out, val) = tag("reset.")(inp)?;
    Ok((out, TimerCmd::Reset))
}

fn timer_cmd_read(inp: Span) -> IResult<Span, TimerCmd> {
    let (out, val) = tag("read.")(inp)?;
    Ok((out, TimerCmd::Read))
}




fn call(inp: Span) -> IResult<Span, Stm>{
    let (out, val) = tuple((id, arg_list, semi))(inp)?;
    Ok((out, Stm::Call{id: tokenize(&val.0), args: val.1, is_builtin: None}))
}

fn assign(inp: Span) -> IResult<Span, Stm>{
    let (out, val) = tuple((id, lex(char('=')), expr, semi))(inp)?;
    Ok((out, Stm::Assign{id: tokenize(&val.0), val: val.2}))
}

fn loop_repeat(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("repeat"), expr, block_sequence))(inp)?;
    Ok((out, Stm::Repeat{kw: tokenize(&val.0), val: val.1, seq: val.2}))
}

fn loop_while(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("while"), expr, block_sequence))(inp)?;
    Ok((out, Stm::While{kw: tokenize(&val.0), cond: val.1, seq: val.2}))
}

fn local_var(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("var "), id, opt(tuple((lex(char(':')), data_type,))), opt(tuple((lex(char('=')), expr))), semi))(inp)?;
    Ok((out, Stm::Var{name: val.1, 
        vartype: 
        match val.2 { //output of opt(tuple(a,b)) where a is ":" and b is the type
            Some((a,b)) => Some(b),
            None => None
        },
        value: 
        match val.3{
            Some((a,b)) => Some(b),
            None => None
        }
    }))
}

fn cond_if(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("if"), expr, block_sequence))(inp)?;
    Ok((out, Stm::If{kw: val.0, cond: val.1, seq: val.2}))
}

fn cond_ifelse(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("if"), expr, block_sequence,  many0(else_if), opt(tuple((tag("else"), block_sequence)))))(inp)?;
    Ok((out, Stm::IfElse{kw: val.0, cond: val.1, seq: val.2, elifs: val.3, elseq: 
        match val.4 {
            Some((a, b)) => b,
            None => Vec::new()
        }
    }))
}

fn else_if(inp: Span) -> IResult<Span, ElseIf> {
    let (out, val) = tuple((tag("else if"), expr, block_sequence))(inp)?;
    Ok((out, ElseIf{kw: val.0, cond: val.1, seq: val.2}))
}

fn timed(inp: Span) -> IResult<Span, Stm> {
    let (out, val) = tuple((tag("timed"), expr, block_sequence))(inp)?;
    Ok((out, Stm::Timed{kw: val.0, time: val.1, seq: val.2}))
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
     _ => unreachable!()
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
        _ => unreachable!()
    
    }
}


fn expr(inp: Span) -> IResult<Span, Expr> {
    alt((bin_ex, un_ex, func_call, var_call, literal_expr, timer_call))(inp)
}

fn bin_ex(inp: Span) -> IResult<Span, Expr> {
    let (out, binex) =
        alt((
        tuple((delimited(lex(char('(')), expr, lex(char(')'))), lex(bin_op), expr)),
        tuple((expr, lex(bin_op), delimited(lex(char('(')), expr, char(')')))),
        tuple((expr, lex(bin_op), expr))
        ))(inp)?;
    Ok((out, Expr{expr: UExpr::BinExpr{l: Box::new(binex.0), op: binex.1, r: Box::new(binex.2)}, exprtype: None}))
}

fn un_ex(inp: Span) -> IResult<Span, Expr> {
    let (out, unex) = alt((
            tuple((lex(un_op), delimited(lex(char('(')), expr, lex(char(')'))))),
            tuple((lex(un_op), expr))
            ))(inp)?;
    Ok((out, Expr{expr: UExpr::UnExpr{op: unex.0, expr: Box::new(unex.1)}, exprtype:None}))
}

fn func_call(inp: Span) -> IResult<Span, Expr> {
    let (out, val) = tuple((id, arg_list))(inp)?;
    Ok((out, Expr{expr: UExpr::Value(Value::FuncCall(FuncCall{id: val.0, args: val.1, is_builtin: None})), exprtype: None}))
}

fn var_call(inp: Span) -> IResult<Span, Expr> {
    let (out, val) = id(inp)?;
    Ok((out, Expr{expr: UExpr::Value(Value::VarCall(val)), exprtype: None}))
}

fn timer_call(inp: Span) -> IResult<Span, Expr> {
    let (out, val) = tuple((tag("timer."), timer_cmd, id))(inp)?;
    Ok((out, Expr{ expr: UExpr::Value(Value::TimerCall { name: val.2, cmd: val.1 }), exprtype: Some(Type::Num)}))
}

fn literal_expr(inp: Span) -> IResult<Span, Expr> {
    let (out, val) = literal(inp)?;
    Ok((out, Expr{expr: UExpr::Value(Value::Literal(val)), exprtype: None}))
}

fn un_op(inp: Span) -> IResult<Span, UnOp> {
    alt((tag("-"), tag("!")))(inp)
}

fn bin_op(inp: Span) -> IResult<Span, UnOp> {
    alt((tag("+"), tag("-"),tag("*"),tag("/"),tag("&"),tag("|"),tag("="),tag(">"),tag("<")))(inp)
}

fn literal(inp: Span) -> IResult<Span, Literal> {
    alt((
        bool_lit,
        num_lit,
        int_lit,
        str_lit
            ))(inp)
    }

fn bool_lit(inp: Span) -> IResult<Span, Literal> {
    let (out, val) = alt((lex(tag("true")), lex(tag("false"))))(inp)?;
    Ok((out, Literal::BoolLiteral(tokenize(&val))))
}

fn int_lit(inp: Span) -> IResult<Span, Literal> {
   let (out, val) = digit0(inp)?;
   Ok((out, Literal::BoolLiteral(tokenize(&val))))
}

fn num_lit<'a>(inp: Span<'a>) -> IResult<Span, Literal> {
    let (out, val) = tuple((opt(digit0), char('.'), digit0))(inp)?;
        Ok((out, Literal::NumLiteral(Token{content: format!("{}.{}", if let Some(i) = val.0 {i.fragment()} else {""}, val.2), position: position(val.1), file: val.0.extra}))) 
}

fn str_lit(inp: Span) -> IResult<Span, Literal> {
    let (out, val) = delimited(char('"'), escaped(none_of("\\"), '\\', char('"')), char('"'))(inp)?;
    Ok((out, Literal::StringLiteral(Token{content: format!("\"{}\"", val.1), position: position(val.1)?, file: val.1.extra })))
}

fn id (inp: Span) -> IResult<Span, Id> {
    lex(id2)(inp)
}

fn id2 (inp: Span) -> IResult<Span, Id> {
    let (inp, start) = alpha1(inp)?;
    let (inp, end) = alphanumeric0(inp)?;
    let mut full: String = String::from(*start.fragment());
    full.push_str(end.fragment());
    Ok((inp, Token{ content: full, position: position(start), file: start.extra }))
}

fn semi (inp: Span) -> IResult<Span, char> {
    lex(char(';'))(inp)
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


fn tokenize<'a>(span: &Span) -> Token<'a> {
    Token{content: String::from(span.fragment()), position: position(span), file: span.extra}
}
