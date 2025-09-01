use qbe::{Function, Module};
use qbe;
use rand::Rng;
use std::fs;
use pest::Parser;
use pest_derive::Parser;
use pest::iterators::{Pair, Pairs};
use std::process::Command;

#[derive(Parser)]
#[grammar = "../syntax.pest"]
struct FaivyParser;

#[derive(Debug,Clone,PartialEq)]
enum DeclarationType {
    Extern(String),
    Global,
    Local,
}

#[derive(Debug,Clone,PartialEq)]
enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    And,
    Or,
    Xor,
    LAnd,
    LOr,
    Dot,
}

#[derive(Debug,Clone,PartialEq)]
enum UnaryOpKind {
    Not,
    Minus,
    Plus,
    LNot
}

#[derive(Debug,Clone,PartialEq)]
enum IR {
    Dummy,
    Str(String),
    ID(String),
    Integer(u64),
    Float(f32),
    Declaration(DeclarationType),
    DeclareFunction(DeclarationType, String, Box<IR>, Vec<CodeGenVariable>, Option<Box<IR>>),
    DeclareVariable(DeclarationType, String, Box<IR>, Option<Box<IR>>),
    DeclareStructure(String, Vec<CodeGenVariable>),
    Call(String, Vec<IR>),
    BinOp(BinOpKind, Box<IR>, Box<IR>),
    UnaryOp(UnaryOpKind, Box<IR>),
    Block(Vec<IR>),
    Import(String),
    Evalute(Box<IR>),
    If(Box<IR>, Box<IR>, Option<Box<IR>>),
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenVariable {
    name: String,
    typ: Option<IR>,
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenFunction {
    name: String,
    real_name: String,
    typ: IR,
    args: Vec<CodeGenVariable>,
    decl: DeclarationType,
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenStruct {
    name: String,
    fields: Vec<CodeGenVariable>,
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenState {
    variables: Vec<IR>,
    typ: Option<IR>
}

#[derive(Debug,Clone)]
struct CodeGen<'a> {
    output: Module<'a>,
    func: Option<Box<Function<'a>>>,
    last_block: Option<Box<qbe::Block<'a>>>,
    functions: Vec<CodeGenFunction>,
    global_variables: Vec<IR>,
    structs: Vec<CodeGenStruct>,
    oid: usize,
}

static TYPE_PTR: qbe::Type = qbe::Type::Long;

impl CodeGen<'_> {
    fn new() -> Self {
	Self {
	    output: Module::new(),
	    func: None,
	    last_block: None,
	    functions: vec![],
	    global_variables: vec![],
	    structs: vec![],
	    oid: 0,
	}
    }
    fn get_free_register(&mut self) -> String {
	let name = format!("reg{}", self.oid);
	self.oid += 1;
	name
    }
    fn get_function_def_args(args: Vec<CodeGenVariable>) -> Vec<(qbe::Type<'static>, qbe::Value)> {
	args.into_iter().map(|x| (Self::get_qbe_type(x.typ.unwrap()), qbe::Value::Temporary(x.name))).collect()
    }
    fn get_qbe_type(symname: IR) -> qbe::Type<'static> {
	if let IR::Call(ref name, ref args) = symname {
	    if *name == "Ptr".to_string() {
		return TYPE_PTR.clone();
	    }
	}
	else if symname == IR::ID("i32".to_string()) {
	    return qbe::Type::Word;
	}
	else if symname == IR::ID("u32".to_string()) {
	    return qbe::Type::Word;
	}
	else if symname == IR::ID("i64".to_string()) {
	    return qbe::Type::Long;
	}
	else if symname == IR::ID("u64".to_string()) {
	    return qbe::Type::Long;
	}
	else if symname == IR::ID("isize".to_string()) {
	    return TYPE_PTR.clone();
	}
	else if symname == IR::ID("usize".to_string()) {
	    return TYPE_PTR.clone();
	}
	else if symname == IR::ID("i8".to_string()) {
	    return qbe::Type::Byte;
	}
	else if symname == IR::ID("u8".to_string()) {
	    return qbe::Type::Byte;
	}
	else if symname == IR::ID("i16".to_string()) {
	    return qbe::Type::Halfword;
	}
	else if symname == IR::ID("u16".to_string()) {
	    return qbe::Type::Halfword;
	}
	return TYPE_PTR.clone();
    }
    fn get_value(&mut self, state: &CodeGenState, ir: IR) -> String {
	let reg = self.get_free_register();
	let mut state = state.clone();
	if let IR::BinOp(op, lhs, rhs) = ir {
	    let mut res: Vec<u8> = vec![];
	    if op == BinOpKind::Add {
		let lhs = self.get_value(&state, *lhs);
		let rhs = self.get_value(&state, *rhs);
	    }
	    else if op == BinOpKind::Sub {
		let lhs = self.get_value(&state, *lhs);
		let rhs = self.get_value(&state, *rhs);
	    }
	    else if op == BinOpKind::Dot {
		let lhs = self.get_value(&state, *lhs);
		let rhs = self.get_value(&state, *rhs);
	    }
	}
	else if let IR::ID(ref id) = ir {
	}
	else if let IR::Integer(ref int) = ir {
	    if let Some(ref mut func) = self.func {
		func.assign_instr(
		    qbe::Value::Temporary(reg.clone()),
		    TYPE_PTR.clone(),
		    qbe::Instr::Copy(qbe::Value::Const(int.clone())),
		);
	    }
	}
	else if let IR::Str(ref id) = ir {
	    let items = vec![
		(qbe::Type::Byte, qbe::DataItem::Str(id.to_string())),
		(qbe::Type::Byte, qbe::DataItem::Const(0)),
	    ];
	    self.output.add_data(qbe::DataDef::new(qbe::Linkage::private(), reg.clone(), None, items));
	    if let Some(ref mut func) = self.func {
		func.assign_instr(
		    qbe::Value::Temporary(reg.clone()),
		    TYPE_PTR.clone(),
		    qbe::Instr::Copy(qbe::Value::Global(reg.clone())),
		);
	    }
	}
	else if let IR::Call(ref name, ref args) = ir {
	    if *name == "return".to_string() {
		let res = self.get_value(&state, args[0].clone());
		if let Some(ref mut func) = self.func {
		    (*func).add_instr(qbe::Instr::Ret(Some(qbe::Value::Temporary(res))));
		}
		else {
		    todo!();
		}
		return reg;
	    }
	    for func in self.functions.clone() {
		if func.name == *name {
		    if args.len() != func.args.len() {
			panic!("Excepted {} arguments at {} but got {} arguments", func.args.len(), *name, args.len());
		    }
		    let mut arg_regs = vec![];
		    for arg in 0..func.args.len() {
			let areg = self.get_value(&state, args[arg].clone());
			arg_regs.push((Self::get_qbe_type(func.args[arg].typ.clone().unwrap()), qbe::Value::Temporary(areg)));
		    }
		    if let Some(ref mut sfunc) = self.func {
			sfunc.assign_instr(
			    qbe::Value::Temporary(reg.clone()),
			    Self::get_qbe_type(func.typ),
			    qbe::Instr::Call(func.real_name, arg_regs, None),
			);
			return reg;
		    }
		}
	    }
	    /*
	    for structure in self.structs.clone() {
		if structure.name == *name {
		    let mut res: Vec<u8> = vec![];
		    let mut args = args.clone();
		    if args.len() != structure.fields.len() {
			panic!("Excepted {} arguments but got {} arguments", structure.fields.len(), args.len());
		    }
		    res.push(b'(');
		    res.extend(name.bytes().collect::<Vec<u8>>());
		    res.push(b')');
		    res.push(b'{');
		    for arg in 0..args.len()-1 {
			res.extend(self.get_value(&state, args[arg].clone()).unwrap());
			res.push(b',');
		    }
		    if args.len() > 0 {
			let arg = args.len()-1;
			res.extend(self.get_value(&state, args[arg].clone()).unwrap());
		    }
		    res.push(b'}');
		    res.push(b'\n');
		    return Some(res);
		}
	}
	     */
	    panic!("Failed to found function/structure `{}`", name);
	}
	reg
    }
    fn codegen(&mut self, state: &CodeGenState, ir: IR) -> CodeGenState {
	let mut state = state.clone();
	if let IR::DeclareFunction(ref dt, ref name, ref typ, ref args, ref body) = ir {
	    if *dt == DeclarationType::Global {
		self.func = Some(Box::new(Function::new(
		    qbe::Linkage::public(),
		    name,
		    Self::get_function_def_args(args.to_vec()),
		    Some(Self::get_qbe_type(*typ.clone())),
		)));
		if let Some(ref mut func) = self.func {
		    self.last_block = Some(
			Box::new(
			    (*func).add_block("start").clone()
			)
		    );
		}
		self.codegen(&state, *body.clone().unwrap());
		self.output.add_function(*self.func.clone().unwrap());
	    }
	    else if *dt == DeclarationType::Local {
		let rname = self.get_free_register();
		self.functions.push(CodeGenFunction{name: rname.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
		self.func = Some(Box::new(Function::new(
		    qbe::Linkage::private(),
		    rname,
		    Self::get_function_def_args(args.to_vec()),
		    Some(Self::get_qbe_type(*typ.clone())),
		)));
		self.codegen(&state, *body.clone().unwrap());
		self.output.add_function(*self.func.clone().unwrap());
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		self.functions.push(CodeGenFunction{name: name.to_string(), real_name: ename.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
	    }
	}
	else if let IR::DeclareStructure(ref name, ref block) = ir {
	}
	else if let IR::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir {
	    if *dt == DeclarationType::Global {
		if let IR::ID(ref typ) = **typ {
		}
	    }
	    else if *dt == DeclarationType::Local {
		if let IR::ID(ref typ) = **typ {
		}
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		if let IR::ID(ref typ) = **typ {
		}
	    }
	}
	else if let IR::If(ref cond, ref then, ref otherwise) = ir {
	    let rcond = self.get_value(&state, *cond.clone());
	    let rthen = self.get_free_register();
	    let rotherwise = self.get_free_register();
	    let rend = self.get_free_register();
	    let mut sfunc = self.func.clone().unwrap();
	    sfunc.add_instr(
		qbe::Instr::Jnz(
		    qbe::Value::Temporary(rcond), rthen.clone(), if otherwise.is_some() {rotherwise.clone()} else {rend.clone()}
		)
	    );
	    self.last_block = Some(
		Box::new(
		    (*sfunc).add_block(rthen).clone()
		)
	    );
	    self.func = Some(sfunc.clone());
	    self.codegen(&state, *then.clone());
	    if otherwise.is_some() {
		sfunc = self.func.clone().unwrap();
		sfunc.add_instr(
		    qbe::Instr::Jmp(
			rend.clone()
		    )
		);
		
		self.last_block = Some(
		    Box::new(
			(*sfunc).add_block(rotherwise).clone()
		    )
		);
		self.func = Some(sfunc.clone());
		self.codegen(&state, *otherwise.clone().unwrap());
		sfunc = self.func.clone().unwrap();
	    }
	    self.last_block = Some(
		Box::new(
		    (*sfunc).add_block(rend).clone()
		)
	    );
	    self.func = Some(sfunc.clone());
	}
	else if let IR::Block(ref insts) = ir {
	    for inst in insts {
		self.codegen(&state, inst.clone());
	    }
	}
	else if ir == IR::Dummy {
	}
	else {
	    let _ = self.get_value(&state, ir);
	}
	state
    }
}

#[derive(Debug,Clone,PartialEq)]
struct PrettyAst {
    rule: Rule,
    span: String,
    inner: Vec<PrettyAst>,
}

impl FaivyParser {
    fn pretty_parse(ast: Pairs<Rule>) -> Vec<PrettyAst> {
	ast.map(|x| PrettyAst {
	    rule: x.as_rule(),
	    span: x.as_span().as_str().to_string(),
	    inner: Self::pretty_parse(x.into_inner())
	}).collect()
    }
}

struct IRBuilder {
}

impl IRBuilder {
    fn build_field(ast: PrettyAst) -> Option<CodeGenVariable> {
	if ast.rule == Rule::field {
	    Some(
		CodeGenVariable {
		    name: ast.inner[0].span.clone(),
		    typ: Some(Self::build(ast.inner[1].clone()))
		}
	    )
	}
	else {
	    None
	}
    }
    
    fn build_args(ast: PrettyAst) -> Option<Vec<CodeGenVariable>> {
	if ast.rule == Rule::args {
	    Some(
		ast.inner.into_iter().map(
		    |ast|
		    Self::build_field(ast.clone()).unwrap()
		).collect()
	    )
	}
	else {
	    None
	}
    }
    
    fn build_expr_args(ast: PrettyAst) -> Option<Vec<IR>> {
	if ast.rule == Rule::expr_args {
	    Some(
		ast.inner.into_iter().map(
		    |ast|
		    Self::build(ast.clone())
		).collect()
	    )
	}
	else {
	    None
	}
    }
    
    fn build(ast: PrettyAst) -> IR {
	println!("\n{:?}\n", ast);
	match ast.rule {
	    Rule::EOI => IR::Dummy,
	    Rule::WHITESPACE => IR::Dummy,
	    Rule::program => IR::Block(ast.inner.into_iter().map(Self::build).collect()),
	    Rule::stmt => IR::Block(ast.inner.into_iter().map(Self::build).collect()),
	    Rule::block => IR::Block(ast.inner.into_iter().map(Self::build).collect()),
	    Rule::decl_extern_type => {
		IR::Declaration(DeclarationType::Extern(ast.inner[0].span.clone()))
	    },
	    Rule::decl_global_type => {
		IR::Declaration(DeclarationType::Global)
	    },
	    Rule::decl_local_type => {
		IR::Declaration(DeclarationType::Local)
	    },
	    Rule::decl_type => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::identifier => {
		IR::ID(ast.span.clone())
	    },
	    Rule::string => {
		IR::Str(ast.span.clone()[1..ast.span.clone().len()-1].to_string())
	    },
	    Rule::number => {
		if ast.span.clone().contains(".") {
		    IR::Float(ast.span.clone().parse::<f32>().unwrap())
		}
		else {
		    IR::Integer(ast.span.clone().parse::<u64>().unwrap())
		}
	    },
	    Rule::primary => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::binop => {
		let lhs = Self::build(ast.inner[0].clone());
		let op = ast.inner[1].span.clone();
		let rhs = Self::build(ast.inner[2].clone());
		if op != "+".to_string() && op != "-".to_string() && op != "->".to_string() {
		    panic!("Currently supported only A+B, A-B, and A->B");
		}
		let binopkind = if op == "+".to_string() {
		    BinOpKind::Add } else if op == "->".to_string() {
		    BinOpKind::Dot } else if op == "-".to_string() {
		    BinOpKind::Sub } else {todo!()};
		IR::BinOp(binopkind, Box::new(lhs), Box::new(rhs))
	    },
	    Rule::expr_atom => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::expr => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::call => {
		IR::Call(ast.inner[0].span.clone(), Self::build_expr_args(ast.inner[1].clone()).unwrap())
	    },
	    Rule::if_stmt => {
		IR::If(Box::new(Self::build(ast.inner[0].clone())), Box::new(Self::build(ast.inner[1].clone())), if ast.inner.len() == 3 {Some(Box::new(Self::build(ast.inner[2].clone())))} else {None})
	    }
	    Rule::func => {
		let IR::Declaration(decl) = Self::build(ast.inner[0].clone()) else {panic!()};
		let IR::ID(name) = Self::build(ast.inner[1].clone()) else {panic!()};
		let typ = Self::build(ast.inner[2].clone());
		IR::DeclareFunction(decl, name, Box::new(typ), Self::build_args(ast.inner[3].clone()).unwrap(), if ast.inner.len() == 5 {Some(Box::new(Self::build(ast.inner[4].clone())))} else {None})
	    },
	    Rule::variable => {
		let IR::Declaration(decl) = Self::build(ast.inner[0].clone()) else {panic!()};
		let IR::ID(name) = Self::build(ast.inner[1].clone()) else {panic!()};
		let typ = Self::build(ast.inner[2].clone());
		IR::DeclareVariable(decl, name, Box::new(typ), if ast.inner.len() == 4 {Some(Box::new(Self::build(ast.inner[3].clone())))} else {None})
	    },
	    Rule::struct_decl => {
		let IR::ID(name) = Self::build(ast.inner[0].clone()) else {panic!()};
		IR::DeclareStructure(name, Self::build_args(PrettyAst{rule: Rule::args, span: String::new(), inner: ast.inner[1..].to_vec()}).unwrap())
	    },
	    _ => IR::Dummy,
	    
	}
    }
}

fn main() {
    
    let ast = FaivyParser::pretty_parse(
	FaivyParser::parse(Rule::program, &fs::read_to_string("input.faivy").unwrap()
	).unwrap()
    );
    println!("{:?}", ast);
    let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
    println!("{:?}", ir);
    let mut codegen = CodeGen::new();
    codegen.codegen(&CodeGenState {variables: vec![], typ: None}, IR::Block(ir));
    let mut code = String::new();
    code += &format!("{}", codegen.output);
    let mut rng = rand::thread_rng();
    let temp_qbe = format!("/tmp/{}.qbe", rng.r#gen::<u32>().to_string());
    let temp_as = "output.s".to_string();
    let _ = fs::write(temp_qbe.clone(), code);
    let mut qbe_work = Command::new("qbe");
    qbe_work.arg(temp_qbe).arg("-o").arg(temp_as.clone());
    println!("{:?}", qbe_work.output().unwrap());
    let mut cc_work = Command::new("cc");
    cc_work.arg(temp_as);
    println!("{:?}", cc_work.output().unwrap());
}

