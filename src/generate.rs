use crate::{ast::*, builtins::{BUILTIN_SUBS, UNARY_OP_SUB, BINARY_OP_SUB}};

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
                                          //panic here can only arise from a bug in BCIScriptC's code.
    match def {
        Def::Var { name, vartype, value, init_priority } => gen_var(name, value, init_priority.borrow().unwrap()),
        Def::Proc { name, args, seq } => gen_proc(name, args, seq),
        Def::Func { name, args, rettype, expr, .. } => gen_func(name, args, expr),
        Def::Actor { name, members } => gen_actor(name, members),
        Def::Event { name } => gen_event(name),
        Def::StateEvent { name } => gen_stateevent(name),
        Def::Timer { name } => gen_timer(name),
        _ => panic!("gen_def passed invalid type")
    } 
}

fn gen_var(name: Id, value: Option<Expr>, init_priority: u64) -> String {
    format!(".addVariable(\"{}\"{})", name.content,
        match value {
            Some(e) => format!(", [&] (SequenceEnvironment &callingSequence){{{}}}, {}", gen_expr(&e), init_priority),
            None => String::new()
        }
    )
}

fn gen_proc( name: Id,  args: Vec<ArgDef>, seq: Seq) -> String {
    format!(".addProcedure(\"{}\",\nProtoSequence({{std::vector<BCIEValue>{{ {} }} }})\n{})",
        &name.content,
        args.iter().map(|a| a.name.content.as_str()).collect::<Vec<&str>>().join(", "),
        gen_seq(seq)
    )
}

fn gen_func(name: Id, args: Vec<ArgDef>, expr: Expr) -> String {
    format!(".addFunction(\"{}\",{},[&] (std::vector<BCIEValue> args, SequenceEnvironment &callingSequence) {{return {};}})", &name.content, args.len(), gen_expr1(&expr, &Some(args)))
}

fn gen_actor(name: Id, members: Vec<Def>) -> String {
    String::new() 
        + "addActor(std::unique_ptr<Actor>(*makeActor()\n"
        + "->self()\n"
        + &members.into_iter().map(gen_actor_def).map(indent).fold(String::new(), |l,r| l + "\n" + &r)
        +"\n))"
}

fn gen_event( name: Id ) -> String {
    format!("addEvent(\"{}\");", &name.content)
}

fn gen_state(name: Id, statetype: StateType) -> String {
    format!("addState(\"{}\", BCIState::StateType.{});", name.content, 
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
    format!(".addStateEvent(\"{}\")", name.content)
}

fn gen_timer( name: Id ) -> String {
    format!(".addTimer(\"{}\")", name.content)
}

fn gen_actor_def(def: Def) -> String {
    match def {
        Def::Var { name, vartype, value, init_priority } => indent1(2, gen_var(name, value, init_priority.borrow().unwrap())),
        Def::Proc { name, args, seq } => gen_proc(name, args, seq),
        Def::Func { name, args, rettype, expr, .. } => gen_func(name, args,  expr),
        Def::Sounds { files } => files.iter().map(|f| indent1(2, format!(".addSound(std::vector<std::string>> {})", f.str()))).fold(String::new(), |l, r| format!("{}\n{}", l, r)),
        Def::Graphics { files } => files.iter().map(|f| indent1(2, format!(".addGraphic({})", f.str()))).fold(String::new(), |l, r| format!("{}\n{}", l, r)),
        Def::OnEvent { name, seq } => indent1(2, format!(".addEventListener(new EventListener(ProtoSequence(){}))", gen_seq(seq))),
        Def::Timer { name } => gen_timer(name),
        Def::Param { name, paramtype, value } => unimplemented!(),
        Def::Actor { .. } | Def::Event { .. } | Def::StateEvent { .. } => panic!("gen_actor_def given invalid def type")
    } 

}


fn gen_seq(seq: Seq) -> String {
    seq.into_iter().map(|s| s).map(gen_stmt).collect::<Vec<String>>().join("")
}

fn gen_stmt(stm: Stm) -> String {
    match stm {
        Stm::If { kw, cond, seq } => gen_if(cond, seq),
        Stm::Var { name, vartype, value } => gen_stm_var(name, value),
        Stm::Call { id, args, is_builtin } => gen_call(id, args, is_builtin.borrow().unwrap()),
        Stm::While { kw, cond, seq } => gen_while(cond, seq),
        Stm::Timed { kw, time, seq } => gen_timed(time, seq),
        Stm::Assign { id, val } => gen_assign(id, val),
        Stm::Repeat { kw, val, seq } => gen_repeat(val, seq),
        Stm::IfElse { kw, cond, seq, elifs, elseq } => gen_if_else(cond, seq, elifs, elseq),
        Stm::Timer { name, cmd } => gen_stm_timer(name, cmd),
        Stm::CallEvent { tp, name } => gen_callevent( tp.unwrap(), name )
    }

}

fn gen_stm_timer(name: Id, cmd: TimerCmd) -> String {
    if let TimerCmd::Add = cmd {
        format!(".addNormalBlock[&] (Sequence& callingSequence) {{callingSequence.addTimer({})}}", name.content)
    } else {
    format!(".addNormalBlock([&] (Sequence& callingSequence) {{callingSequence.getTimer(\"{}\").{}();}})", name.content,
        match cmd {
            TimerCmd::Start => "start",
            TimerCmd::Stop => "stop",
            TimerCmd::Reset => "reset",
            TimerCmd::Read => panic!("gen_stm_timer given invalid command"),
            TimerCmd::Add => unreachable!()
        }
    )
    }
}

fn gen_callevent(tp: EvType, name: Id) -> String{
    match tp {
        EvType::BCISEvent => format!(".addNormalBlock([&] (Sequence& callingSequence) {{callingSequence.callEvent(\"{}\")}}", name.content),
        EvType::StateEvent => format!(".addNormalBlock([&] (Sequence& callingSequence) {{callingSequence.callStateEvent(\"{}\")}}", name.content),
    }
}

fn gen_if(cond: Expr, seq: Seq) -> String{
    match *cond.exprtype.borrow() {
        Some(Type::Bool) => 
            format!(".addIf({})\n{}.closeStatement()\n",
            gen_expr_lambda(gen_expr(&cond)),
            gen_seq(seq)
            ),
        Some(_) => panic!("gen_if given expr of type other than boolean"),
        _ => panic!("gen_if given expr without type")
    }
}

fn gen_stm_var(name: Id,  value: Option<Expr>) -> String {
    format!(".addNormalBlock({})", gen_block_lambda(format!("callingSequence.addVariable({}{})", name.content, 
                        match value {
                            Some(e) => format!(",{}", gen_expr(&e)),
                            None => String::new()
                        })))
}


fn gen_assign(name: Id, val: Expr) -> String{
    format!(".addNormalBlock([&] (Sequence& callingSequence) {{callingSequence.setVariable({},{})}}\n", name.content, gen_expr(&val))
}


fn gen_call(id: Id, args: Vec<Expr>, is_builtin: bool) -> String {
    if is_builtin {
        builtin_sub(BUILTIN_SUBS.get(id.content.as_str()).unwrap(), &args)
    } else {
        format!(".addNormalBlock({})", gen_block_lambda(format!("callingSequence.callProc(std::vector<std::string> {{{}}})",
            args.into_iter().map(|a| format!("BCIEValue({})", gen_expr(&a))).collect::<Vec<String>>().join(", ")
        )))
    }
}

fn gen_while(cond: Expr, seq: Seq) -> String{
    match cond.exprtype.borrow().clone() {
        Some(Type::Bool) => 
            format!(".addWhile({})\n{}.closeStatement()\n",
            gen_expr_lambda(gen_expr(&cond)), gen_seq(seq)),
        Some(_) => panic!("gen_while given expr of type other than boolean"),
        None => panic!("gen_while given expr without type")
    }
}

fn gen_timed(time: Expr, seq: Seq) -> String {
    match time.exprtype.borrow().clone() {
        Some(Type::Int) => 
            format!(".addTimed({}){}.closeStatement()\n",
            gen_expr_lambda(gen_expr(&time)),
            gen_seq(seq)),
        Some(_) => panic!("gen_timed given expr of type other than int"),
        _ => panic!("gen_timed given expr without type")
    }
}

fn gen_repeat(val: Expr, seq: Seq) -> String {
    match val.exprtype.borrow().clone() {
        Some(Type::Int) => 
            format!(".addLoop({})\n{}.closeStatement()\n", gen_expr_lambda(gen_expr(&val)), gen_seq(seq)),
        Some(_) => panic!("gen_repeat given expr of type other than int"),
        _ => panic!("gen_repeat given expr without type")
    }
}

//See bciscript_elifs for details on how else if statements work
fn gen_if_else(cond: Expr, seq: Seq, elifs: Vec<ElseIf>, elseq: Seq ) -> String {
    let eliflen = elifs.len();
    match cond.exprtype.borrow().clone() {
        Some(Type::Bool) => 
            format!(".addIfElse({})\n{}\n.closeStatement()\n{}{}{}.closeStatement()\n",
                gen_expr_lambda(gen_expr(&cond)),
                gen_seq(seq),
                elifs.into_iter().map(gen_elif).collect::<Vec<String>>().join(""),
                gen_seq(elseq),
                {
                    let mut outstr = String::new();
                    for i in 1..eliflen {
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
    match elif.cond.exprtype.borrow().clone() {
        Some(Type::Bool) => 
            format!(".addIfElse({})\n{}.closeStatement()",
            gen_expr_lambda(gen_expr(&elif.cond)),
            gen_seq(elif.seq)),
        Some(_) => panic!("gen_elif given expr of type other than bool"),
        _ => panic!("gen_elif given expression without type")
    }
}


fn builtin_sub(substr: &String, args: &Vec<Expr>) -> String {
    let mut x: usize  = 0;
    let ch: Vec<char> = substr.chars().collect();
    let mut outstr = String::new();
    while x < substr.len() {
        if ch[x] == '$' {
            x += 1;
            let mut num_str = String::new();
            while x < substr.len() && is_digit(ch[x]) {
                num_str.push(ch[x]);
                x += 1;
            }
            outstr.push_str(&gen_expr(args.get(num_str.parse::<usize>().unwrap()).unwrap()));
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


fn gen_expr(expr: &Expr) -> String {
    gen_expr1(expr, &None)
}

/*
 * funcargs are exclusively for when this expr is part of a function definition, because
 * function arguments are accessed differently than normal
 */
fn gen_expr1(expr: &Expr, funcargs: &Option<Vec<ArgDef>>) -> String {
    match &expr.expr {
        UExpr::BinExpr { l, op, r } => format!("({}{}{})", gen_expr1(l.as_ref(), funcargs), gen_bop(op), gen_expr1(r.as_ref(), funcargs)),
        UExpr::UnExpr { op, expr } => format!("({}{})", gen_uop(op), gen_expr1(expr.as_ref(), funcargs)),
        UExpr::Value(val) => gen_val(val, expr.exprtype.borrow().unwrap(), funcargs)
    }
}

fn gen_bop(op: &BinOp) -> String {
    BINARY_OP_SUB.get(op.content.as_str()).unwrap().to_string()
}

fn gen_uop(op: &UnOp) -> String {
    UNARY_OP_SUB.get(op.content.as_str()).unwrap().to_string()
}

fn gen_val(val: &Value, t: Type, funcargs: &Option<Vec<ArgDef>>) -> String {
    match val {
        Value::Literal(l) => gen_lit(l),
        Value::VarCall(v) => gen_var_call(v, t, funcargs),
        Value::FuncCall(FuncCall { id, args, is_builtin }) => gen_func_call(id, args, is_builtin.borrow().unwrap()),
        Value::TimerCall { name, cmd } => format!("callingSequence.getTimer(\"{}\").read()", name.content)
    }
}

fn gen_func_call(id: &Id, args: &Vec<Expr>, is_builtin: bool) -> String {
    if is_builtin {
        builtin_sub(BUILTIN_SUBS.get(id.content.as_str()).unwrap(), args)
    } else {
        format!("callingSequence.callFunction(std::vector<BCIEValue>{})",
        args.into_iter().map(|a| format!("BCIEValue({})", gen_expr(&a))).collect::<Vec<String>>().join(", "))
    }
}

fn gen_var_call(v: &Id, t: Type, funcargs: &Option<Vec<ArgDef>>) -> String {
    match funcargs {
        Some(f) => match f.iter().position(|f| f.name.content == v.content) {
            Some(v) => format!("args.at({})", v),
            None => format!("callingSequence.getVariable(\"{}\")", v.content)
        }
        None => format!("callingSequence.getVariable(\"{}\")", v.content)
    }
}

fn gen_lit(l: &Literal) -> String {
    l.str().to_string()
}

fn gen_block_lambda<T: std::fmt::Display>(c: T) -> String {
    format!("[&] (Sequence& callingSequence) {{{}}}", c)
}

fn gen_expr_lambda<T: std::fmt::Display>(c: T) -> String {
    format!("[&] (SequenceEnvironment& callingSequence) {{{}}}", c)
}


fn indent(inp: String) -> String {
    let mut s = String::new();
    s += "\t";
    s += &inp; 
    return inp;

}

fn indent1(i: i64, inp: String) -> String {
    let mut s = String::new();
    for _x in 0..i {
        s += "\t";
    }
    s += &inp;
    return s;
}


