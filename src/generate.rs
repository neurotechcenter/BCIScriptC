use crate::{ast::*, builtins::BUILTIN_SUBS};

/*
 * Functions for generating the AppInitPartial.cpp file from the parse tree.
 * This happens after previous verification for correctness, so these functions
 * assume that they are given a valid parse tree.
 * This is the reason for the liberal use of panic!, as this module is only expected
 * to panic in the case of a bug in program verification causing it to be given
 * an invalid parse tree. Any programming errors are expected to be caught during 
 * the previous verification step.
 *
 * All functions consume because they recurse directly down the parse tree without ever travelling
 * horizontally
 */

pub fn generate_program<'a>(program: Program<'a>) -> String {
    let ret = String::new();
    ret + 
        "#include \"BCIEvent.hpp\"\n"
        + "\n\n"
        + "using namespace BCIEvent;"
        + "\n\n"
        + "void BCIEventApplication::InitBCIEvent() {"
        + &program.into_iter().map(|x: Def<'a>| gen_def(x)).map(indent).fold(String::new(), |l:String, s:String| (l + "\n") + &s)
        + "\n}"
}

fn gen_def<'a>(def: Def<'a>) -> String { //The unwrapped values here should never be None, so a
                                          //panic here can only arise from a bug in the code.
    match def {
        Def::Var { name, vartype, value } => gen_var(name, value),
        Def::Proc { name, args, seq } => gen_proc(name, args, seq),
        Def::Func { name, args, rettype, expr } => gen_func(name, args, rettype, expr),
        Def::Actor { name, members } => gen_actor(name, members),
        Def::Event { name } => gen_event(name),
        Def::State { name, statetype } => gen_state(name, statetype),
        Def::StateEvent { name } => gen_stateevent(name),
        Def::Timer { name } => gen_timer(name),
        _ => panic!("gen_def passed invalid type")
    } 
}

fn gen_var(name: Id, value: Option<Expr>) -> String {
    format!("addVariable(\"{}\"{})", name.fragment(),
        match value {
            Some(e) => format!(", [&] (SequenceEnvironment &callingSequence){}", gen_expr(e, None)),
            None => String::new()
        }
    )
}

fn gen_proc( name: Id,  args: Vec<ArgDef>, seq: Seq) -> String {
    format!(".addProcedure(\"{}\",\nProtoSequence({{std::vector<std::string>{{ {} }} }})\n{})",
        name.fragment(),
        args.iter().map(|a| a.name.fragment().to_string()).collect::<Vec<String>>().join(", "),
        gen_seq(seq)
    )
}

fn gen_func(name: Id, args: Vec<ArgDef>, rettype: Type, expr: Expr) -> String {
    format!(".addFunction(\"{}\",{},[&] (std::vector<BCIEValue> args, SequenceEnvironment &callingSequence) {{{}}})", name.fragment(), args.len(), gen_expr(expr, Some(args)))
}

fn gen_actor(name: Id, members: Vec<Def>) -> String {
    String::new() 
        + "addActor(std::unique_ptr<Actor>(*makeActor()\n"
        + "->self()\n"
        + &members.into_iter().map(gen_actor_def).map(indent).fold(String::new(), |l,r| l + "\n" + &r)
        +"\n))"
}

fn gen_event( name: Id ) -> String {
    format!("addEvent(\"{}\");", name.fragment())
}

fn gen_state(name: Id, statetype: StateType) -> String {
    format!("addState(\"{}\", BCIState::StateType.{});", name.fragment(), 
            match statetype {
                StateType::U8 => "u8",
                StateType::I8 => "i8",
                StateType::U32 => "u32",
                StateType::I32 => "i32",
                StateType::Bool => "Boolean"
            }
            )
}

fn gen_stateevent( name: Id ) -> String {
    format!(".addStateEvent(\"{}\")", name.fragment())
}

fn gen_timer( name: Id ) -> String {
    format!(".addTimer(\"{}\")", name.fragment())
}

fn gen_actor_def(def: Def) -> String {
    match def {
        Def::Var { name, vartype, value } => indent1(2, gen_var(name,  value)),
        Def::Proc { name, args, seq } => gen_proc(name, args, seq),
        Def::Func { name, args, rettype, expr } => gen_func(name, args, rettype, expr),
        Def::Sounds { files } => files.iter().map(|f| indent1(2, format!(".addSound(std::vector<std::string>> {})", f.fragment()))).fold(String::new(), |l, r| format!("{}\n{}", l, r)),
        Def::Graphics { files } => files.iter().map(|f| indent1(2, format!(".addGraphic({})", f.fragment()))).fold(String::new(), |l, r| format!("{}\n{}", l, r)),
        Def::OnEvent { name, seq } => indent1(2, format!(".addEventListener(new EventListener(ProtoSequence(){}))", gen_seq(seq))),
        Def::Timer { name } => gen_timer(name),
        Def::Actor { .. } | Def::Event { .. } | Def::State { .. } | Def::StateEvent { .. } => panic!("gen_actor_def given invalid def type")
    } 

}


fn gen_seq(seq: Seq) -> String {
    seq.iter().map(|s| *s).map(gen_stmt).collect::<Vec<String>>().join("")
}

fn gen_stmt(stm: Stm) -> String {
    match stm {
        Stm::If { cond, seq } => gen_if(cond, seq),
        Stm::Var { name, vartype, value } => gen_stm_var(name, vartype.unwrap(), value.unwrap()),
        Stm::Call { id, args } => gen_call(id, args),
        Stm::While { cond, seq } => gen_while(cond, seq),
        Stm::Timed { time, seq } => gen_timed(time, seq),
        Stm::Assign { id, val } => gen_assign(id, val),
        Stm::Repeat { val, seq } => gen_repeat(val, seq),
        Stm::IfElse { cond, seq, elifs, elseq } => gen_if_else(cond, seq, elifs, elseq),
        Stm::Timer { name, cmd } => gen_timer(name)
    }

}

fn gen_if(cond: Expr, seq: Seq) -> String{
    match cond.exprtype {
        Some(Type::Bool) => 
            format!(".addIf({})\n{}.closeStatement()\n",
            gen_lambda(gen_expr(cond)),
            gen_seq(seq)
            ),
        Some(_) => panic!("gen_if given expr of type other than boolean"),
        _ => panic!("gen_if given expr without type")
    }
}

fn gen_stm_var(name: Id, vartype: Type, value: Expr) -> String {
    unimplemented!()
}


fn gen_assign(name: Id, val: Expr) -> String{
    format!(".addNormalBlock([&] (Sequence& callingSequence) {{callingSequence.setVariable({},{})}}\n", name.fragment(), gen_expr(val))
}


fn gen_call(id: Id, args: Vec<Expr>) -> String {
    match BUILTIN_SUBS.get(id.fragment()) {
        Some(s) => builtin_sub(s, args),
        None => unimplemented!()
    }
}

fn gen_while(cond: Expr, seq: Seq) -> String{
    match cond.exprtype {
        Some(Type::Bool) => 
            format!(".addWhile({})\n{}.closeStatement()\n",
            gen_lambda(gen_expr(cond)), gen_seq(seq)),
        Some(_) => panic!("gen_while given expr of type other than boolean"),
        None => panic!("gen_while given expr without type")
    }
}

fn gen_timed(time: Expr, seq: Seq) -> String {
    match time.exprtype {
        Some(Type::Int) => 
            format!(".addTimed({}){}.closeStatement()\n",
            gen_lambda(gen_expr(time)),
            gen_seq(seq)),
        Some(_) => panic!("gen_timed given expr of type other than int"),
        _ => panic!("gen_timed given expr without type")
    }
}

fn gen_repeat(val: Expr, seq: Seq) -> String {
    match val.exprtype {
        Some(Type::Int) => 
            format!(".addLoop({})\n{}.closeStatement()\n", gen_lambda(gen_expr(val)), gen_seq(seq)),
        Some(_) => panic!("gen_repeat given expr of type other than int"),
        _ => panic!("gen_repeat given expr without type")
    }
}

//See bciscript_elifs for details on how else if statements work
fn gen_if_else(cond: Expr, seq: Seq, elifs: Vec<ElseIf>, elseq: Seq ) -> String {
    match cond.exprtype {
        Some(Type::Bool) => 
            format!(".addIfElse({})\n{}\n.closeStatement()\n{}{}{}.closeStatement()\n",
                gen_lambda(gen_expr(cond)),
                gen_seq(seq),
                elifs.into_iter().map(gen_elif).collect::<Vec<String>>().join(""),
                gen_seq(elseq),
                {
                    let outstr = String::new();
                    for i in elifs {
                        outstr.push_str(".closeStatement()\n");
                    }
                    outstr
                }
            ),
        Some(_) => panic!("gen_if_else given expr with type other than bool"),
        _ => panic!("gen_if_else given expr without type")
    }

}

fn gen_elif(elif: ElseIf) -> String{
    match elif.cond.exprtype {
        Some(Type::Bool) => 
            format!(".addIfElse({})\n{}.closeStatement()",
            gen_lambda(gen_expr(elif.cond)),
            gen_seq(elif.seq)),
        Some(_) => panic!("gen_elif given expr of type other than bool"),
        _ => panic!("gen_elif given expression without type")
    }
}


fn builtin_sub(substr: String, args: Vec<Expr>) -> String {
    let x:usize  = 0;
    let ch: Vec<char> = substr.chars().collect();
    let outstr = String::new();
    while x < substr.len() {
        if ch[x] == '$' {
            x += 1;
            let num_str = String::new();
            while x < substr.len() && is_digit(ch[x]) {
                num_str.push(ch[x]);
                x += 1;
            }
            outstr.push_str(&gen_expr(args[num_str.parse::<usize>().unwrap()]));
        }
        if x < substr.len() {
            outstr.push(ch[x]);
        }
    }
    return outstr;
}

fn is_digit(c: char) -> bool {
    String::from("0123456789").chars().any(|i| i == c)
}


fn gen_expr(expr: Expr, funcargs: Option<Vec<ArgDef>>) -> String {
    match expr.expr {
        UExpr::BinExpr { l, op, r } => format!("({}{}{})", gen_expr(*l), gen_bop(op), gen_expr(*r)),
        UExpr::UnExpr { op, expr } => format!("({}{})", gen_uop(op), gen_expr(*expr)),
        UExpr::Value(val) => gen_val(val, expr.exprtype.unwrap())
    }
}

fn gen_bop(op: BinOp) -> String {
    match op {
        BinOp::Add(..) => "+",
        BinOp::Sub(..) => "-",
        BinOp::Mult(..) => "*",
        BinOp::Div(..) => "/",
        BinOp::And(..) => "&&",
        BinOp::Or(..) => "||"
    }.to_string()
}

fn gen_uop(op: UnOp) -> String {
    match op {
        UnOp::Neg(..) => "-",
        UnOp::Not(..) => "!"
    }.to_string()
}

fn gen_val(val: Value, t: Type, funcargs: Option<Vec<ArgDef>>) -> String {
    match val {
        Value::Literal(l) => gen_lit(l),
        Value::VarCall(v) => gen_var_call(v, t),
        Value::FuncCall(..) => unimplemented!()
    }
}


fn gen_var_call(v: Id, t: Type) -> String {
    format!("callingSequence.getVariable<{}>(\"{}\")",
        t.cpp_rep(),
        v.fragment()
    )
}

fn gen_lit(l: Literal) -> String {
    match l {
        Literal::IntLiteral(i) => i.fragment(),
        Literal::NumLiteral(n) => n.fragment(),
        Literal::BoolLiteral(b) => b.fragment(),
        Literal::StringLiteral(s) => format!("\"{}\"", s.fragment()).as_str(),
    }.to_string()
}

fn gen_lambda<T: std::fmt::Display>(c: T) -> String {
    format!("[&] (Sequence& callingSequence) {{{}}}", c)
}


fn indent(inp: String) -> String {
    let s = String::new();
    s += "\t";
    s += &inp; 
    return inp;

}

fn indent1(i: i64, inp: String) -> String {
    let s = String::new();
    for x in 0..i {
        s += "\t";
    }
    s += &inp;
    return s;
}


