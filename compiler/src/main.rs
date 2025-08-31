use std::fs;
use pest::Parser;
use pest_derive::Parser;
use pest::iterators::{Pair, Pairs};

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
enum IR { // TODO
    Dummy,
    Str(String),
    ID(String),
    Integer(isize),
    Float(f32),
    Declaration(DeclarationType),
    DeclareFunction(DeclarationType, String, Box<IR>, Vec<CodeGenVariable>, Option<Box<IR>>),
    DeclareVariable(DeclarationType, String, Box<IR>, Option<Box<IR>>),
    DeclareStructure(String, Vec<CodeGenVariable>),
    Call(String, Vec<IR>),
    BinOp(BinOpKind, Box<IR>, Box<IR>),
    UnaryOp(UnaryOpKind, Box<IR>),
    Block(Vec<IR>),
    Emit(u8),
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
struct CodeGenStruct {
    name: String,
    fields: Vec<CodeGenVariable>,
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenState {
    variables: Vec<IR>
}

#[derive(Debug,Clone)]
struct CodeGen {
    output: Vec<u8>,
    functions: Vec<IR>,
    global_variables: Vec<IR>,
    structs: Vec<CodeGenStruct>
}

impl CodeGen {
    fn new() -> Self {
	Self {
	    output: vec![],
	    functions: vec![],
	    global_variables: vec![],
	    structs: vec![],
	}
    }
    fn get_field(&mut self, state: &CodeGenState, field: CodeGenVariable) -> Vec<u8> {
	let mut res: Vec<u8> = vec![];
	res.extend(self.get_value(state, field.typ.unwrap()).unwrap());
	res.push(b' ');
	res.extend(field.name.bytes());
	res
    }
    fn get_fields(&mut self, state: &CodeGenState, fields: Vec<CodeGenVariable>) -> Vec<u8> {
	let mut res: Vec<u8> = vec![];
	if fields.len() != 0 {
	    for i in 0..fields.len()-1 {
		res.extend(self.get_field(state, fields[i].clone()));
		res.push(b',');
		res.push(b' ');
	    }
	    res.extend(self.get_field(state, fields[fields.len()-1].clone()));
	}
	res
    }
    fn get_sc_fields(&mut self, state: &CodeGenState, fields: Vec<CodeGenVariable>) -> Vec<u8> {
	let mut res: Vec<u8> = vec![];
	if fields.len() != 0 {
	    for i in 0..fields.len() {
		res.extend(self.get_field(state, fields[i].clone()));
		res.push(b';');
		res.push(b' ');
	    }
	}
	res
    }
    fn get_value(&mut self, state: &CodeGenState, ir: IR) -> Option<Vec<u8>> {
	let mut state = state.clone();
	if let IR::BinOp(op, lhs, rhs) = ir {
	    let mut res: Vec<u8> = vec![];
	    if op == BinOpKind::Add {
		let lhs = self.get_value(&state, *lhs).unwrap();
		let rhs = self.get_value(&state, *rhs).unwrap();
		res.extend("((".bytes().collect::<Vec<u8>>());
		res.extend(lhs);
		res.extend(")".bytes().collect::<Vec<u8>>());
		res.extend(" + ".bytes().collect::<Vec<u8>>());
		res.extend("(".bytes().collect::<Vec<u8>>());
		res.extend(rhs);
		res.extend("))".bytes().collect::<Vec<u8>>());
	    }
	    else if op == BinOpKind::Sub {
		let lhs = self.get_value(&state, *lhs).unwrap();
		let rhs = self.get_value(&state, *rhs).unwrap();
		res.extend("((".bytes().collect::<Vec<u8>>());
		res.extend(lhs);
		res.extend(")".bytes().collect::<Vec<u8>>());
		res.extend(" - ".bytes().collect::<Vec<u8>>());
		res.extend("(".bytes().collect::<Vec<u8>>());
		res.extend(rhs);
		res.extend("))".bytes().collect::<Vec<u8>>());
	    }
	    else if op == BinOpKind::Dot {
		let lhs = self.get_value(&state, *lhs).unwrap();
		let rhs = self.get_value(&state, *rhs).unwrap();
		res.extend("(".bytes().collect::<Vec<u8>>());
		res.extend(lhs);
		res.extend(" . ".bytes().collect::<Vec<u8>>());
		res.extend(rhs);
		res.extend(")".bytes().collect::<Vec<u8>>());
	    }
	    Some(res)
	}
	else if IR::ID("i32".to_string()) == ir {
	    let mut res: Vec<u8> = "i32".bytes().collect();
	    Some(res)
	}
	else if IR::ID("cstring".to_string()) == ir {
	    let mut res: Vec<u8> = "cstring".bytes().collect();
	    Some(res)
	}
	else if IR::ID("i64".to_string()) == ir {
	    let mut res: Vec<u8> = "i64".bytes().collect();
	    Some(res)
	}
	else if IR::ID("u64".to_string()) == ir {
	    let mut res: Vec<u8> = "u64".bytes().collect();
	    Some(res)
	}
	else if IR::ID("float32".to_string()) == ir {
	    let mut res: Vec<u8> = "float".bytes().collect();
	    Some(res)
	}
	else if IR::ID("void".to_string()) == ir {
	    let mut res: Vec<u8> = "void".bytes().collect();
	    Some(res)
	}
	else if let IR::ID(ref id) = ir {
	    let mut res: Vec<u8> = vec![];
	    res.extend(id.bytes().collect::<Vec<u8>>());
	    Some(res)
	}
	else if let IR::Integer(ref int) = ir {
	    let mut res: Vec<u8> = vec![];
	    res.extend(int.to_string().bytes().collect::<Vec<u8>>());
	    Some(res)
	}
	else if let IR::Str(ref id) = ir {
	    let mut res: Vec<u8> = vec![];
	    res.push(b'"');
	    res.extend(id.bytes().collect::<Vec<u8>>());
	    res.push(b'"');
	    Some(res)
	}
	else if let IR::Call(ref name, ref args) = ir {
	    if *name == "Ptr".to_string() {
		let mut res: Vec<u8> = self.get_value(&state, args[0].clone()).unwrap();
		res.push(b'*');
		return Some(res);
	    }
	    if *name == "return".to_string() {
		let mut res: Vec<u8> = vec![];
		res.extend("return (".bytes().collect::<Vec<u8>>());
		res.extend(self.get_value(&state, args[0].clone()).unwrap());
		res.push(b')');
		res.push(b';');
		return Some(res);
	    }
	    for func in self.functions.clone() {
		if let IR::DeclareFunction(DeclarationType::Local, ref fname, ref ftype, ref fargs, _) = func {
		    if fname == name {
			let mut res: Vec<u8> = vec![];
			let mut args = args.clone();
			if args.len() != fargs.len() {
			    panic!("Excepted {} arguments but got {} arguments", fargs.len(), args.len());
			}
			res.extend(fname.bytes().collect::<Vec<u8>>());
			res.push(b'(');
			for arg in 0..args.len()-1 {
			    res.extend(self.get_value(&state, args[arg].clone()).unwrap());
			    res.push(b',');
			}
			if args.len() > 0 {
			    let arg = args.len()-1;
			    res.extend(self.get_value(&state, args[arg].clone()).unwrap());
			}
			res.push(b')');
			res.push(b'\n');
			return Some(res);
		    }
		}
		else if let IR::DeclareFunction(DeclarationType::Extern(ref efname), ref fname, ref ftype, ref fargs, _) = func {
		    if fname == name {
			let mut res: Vec<u8> = vec![];
			let mut args = args.clone();
			if args.len() != fargs.len() {
			    panic!("Excepted {} arguments but got {} arguments", fargs.len(), args.len());
			}
			res.extend(efname.bytes().collect::<Vec<u8>>());
			res.push(b'(');
			for arg in 0..args.len()-1 {
			    res.extend(self.get_value(&state, args[arg].clone()).unwrap());
			    res.push(b',');
			}
			if args.len() > 0 {
			    let arg = args.len()-1;
			    res.extend(self.get_value(&state, args[arg].clone()).unwrap());
			}
			res.push(b')');
			res.push(b'\n');
			return Some(res);
		    }
		}
	    }
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
	    panic!("Failed to found function/structure `{}`", name);
	}
	else {
	    None
	}
    }
    fn codegen(&mut self, state: &CodeGenState, ir: IR) -> CodeGenState {
	let mut state = state.clone();
	if let IR::DeclareFunction(ref dt, ref name, ref typ, ref args, ref body) = ir {
	    if *dt == DeclarationType::Global {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(name.bytes());
		    self.output.push(b'(');
		    let fields = self.get_fields(&state, args.to_vec());
		    self.output.extend(fields);
		    self.output.push(b')');
		    self.output.push(b'{');
		    self.codegen(&state, *body.clone().unwrap());
		    self.output.push(b'}');
		}
	    }
	    else if *dt == DeclarationType::Local {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend("static ".bytes());
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(name.bytes());
		    self.output.push(b'(');
		    let fields = self.get_fields(&state, args.to_vec());
		    self.output.extend(fields);
		    self.output.push(b')');
		    self.output.push(b'{');
		    self.codegen(&state, *body.clone().unwrap());
		    self.output.push(b'}');
		}
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend("extern ".bytes());
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(ename.bytes());
		    self.output.push(b'(');
		    let fields = self.get_fields(&state, args.to_vec());
		    self.output.extend(fields);
		    self.output.push(b')');
		    self.output.push(b';');
		}
	    }
	    self.functions.push(ir);
	}
	else if let IR::DeclareStructure(ref name, ref block) = ir {
	    self.output.extend("typedef struct ".bytes());
	    self.output.extend(name.bytes());
	    self.output.push(b' ');
	    self.output.push(b'{');
	    let fields = self.get_sc_fields(&state, block.to_vec());
	    self.output.extend(fields);
	    self.output.push(b'}');
	    self.output.push(b' ');
	    self.output.extend(name.bytes());
	    self.output.push(b';');
	    self.structs.push(CodeGenStruct{name: name.to_string(), fields: block.to_vec()});
	}
	else if let IR::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir {
	    if *dt == DeclarationType::Global {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(name.bytes());
		    self.output.push(b'=');
		    self.codegen(&state, *body.clone().unwrap());
		    self.output.push(b';');
		}
	    }
	    else if *dt == DeclarationType::Local {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(name.bytes());
		    self.output.push(b'=');
		    self.codegen(&state, *body.clone().unwrap());
		    self.output.push(b';');
		}
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		if let IR::ID(ref typ) = **typ {
		    self.output.extend("extern ".bytes());
		    self.output.extend(typ.bytes());
		    self.output.push(b' ');
		    self.output.extend(name.bytes());
		    self.output.push(b'=');
		    self.codegen(&state, *body.clone().unwrap());
		    self.output.push(b';');
		}
	    }
	    self.functions.push(ir);
	}
	else if let IR::If(ref cond, ref then, ref otherwise) = ir {
	    self.output.extend("if ".bytes());
	    self.output.push(b'(');
	    self.codegen(&state, *cond.clone());
	    self.output.push(b')');
	    self.output.push(b'{');
	    self.codegen(&state, *then.clone());
	    self.output.push(b'}');
	    if let Some(otherwise) = otherwise {
		self.output.extend("else".bytes());
		self.output.push(b'{');
		self.codegen(&state, *otherwise.clone());
		self.output.push(b'}');
	    }
	}
	else if let IR::Block(ref insts) = ir {
	    for inst in insts {
		self.codegen(&state, inst.clone());
	    }
	}
	else if ir == IR::Dummy {
	}
	else {
	    if let Some(value) = self.get_value(&state, ir) {
		self.output.extend(value);
	    }
	    self.output.push(b';');
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
		    IR::Integer(ast.span.clone().parse::<isize>().unwrap())
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
    codegen.codegen(&CodeGenState {variables: vec![]}, IR::Block(ir));
    let mut code = String::new();
    code += "typedef unsigned long long u64;typedef unsigned int u32;typedef long long i64;typedef int i32;typedef char *cstring;typedef float float32;\n\n";
    code += &String::from_utf8(codegen.output).unwrap();
    let _ = fs::write("output.c", code);
}
