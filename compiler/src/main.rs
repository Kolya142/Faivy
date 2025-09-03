use rand::Rng;
use std::fs;
use pest::Parser;
use pest_derive::Parser;
use pest::iterators::Pairs;
use std::process::Command;
use std::env;

#[derive(Parser)]
#[grammar = "../syntax.pest"]
struct FaivyParser;

#[derive(Debug,Clone,PartialEq)]
enum DeclarationType {
    Extern(String),
    Global,
    Local,
    Forward,
}

#[derive(Debug,Clone,PartialEq)]
enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    LShft,
    RShft,
    And,
    Or,
    Xor,
    // Dot,
    Eq,
    NEq,
    Less,
    Bigger,
    LessOrEq,
    BiggerOrEq,
}

/*#[derive(Debug,Clone,PartialEq)]
enum UnaryOpKind {
    Not,
    Minus,
    Plus,
    LNot
}
*/

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
    Set(Box<IR>, Box<IR>),
    BinOp(BinOpKind, Box<IR>, Box<IR>),
    // UnaryOp(UnaryOpKind, Box<IR>),
    Block(Vec<IR>),
    Import(String),
    Asm(String),
    LLVM(String),
    // Evalute(Box<IR>),
    If(Box<IR>, Box<IR>, Option<Box<IR>>),
    While(Box<IR>, Box<IR>),
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenVariable {
    name: String,
    typ: Option<IR>,
}

#[derive(Debug,Clone,PartialEq)]
struct CodeGenFakeableVariable {
    real_name: String,
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

/*
#[derive(Debug,Clone,PartialEq)]
struct CodeGenStruct {
    name: String,
    fields: Vec<CodeGenVariable>,
}
*/

#[derive(Debug,Clone,PartialEq)]
struct CodeGenState {
    variables: Vec<CodeGenFakeableVariable>,
    typ: Option<IR>,
    expected_typ: Option<IR>
}

#[derive(Debug,Clone)]
struct CodeGen {
    output: String,
    data_output: String,
    end_of_block: bool,
    functions: Vec<CodeGenFunction>,
    // global_variables: Vec<CodeGenFakeableVariable>,
    local_variables: Vec<CodeGenFakeableVariable>,
    // structs: Vec<CodeGenStruct>,
    oid: usize,
}

impl CodeGen {
    fn new() -> Self {
	Self {
	    output: String::new(),
	    data_output: String::new(),
	    end_of_block: false,
	    functions: vec![],
	    // global_variables: vec![],
	    local_variables: vec![],
	    // structs: vec![],
	    oid: 0,
	}
    }
    fn get_free_register(&mut self) -> String {
	let name = format!("reg{}", self.oid);
	self.oid += 1;
	name
    }
    fn get_function_def_args(args: Vec<CodeGenVariable>) -> String {
	return args.into_iter().map(|x| format!("{} %{}", Self::get_backend_type(x.typ.unwrap()), x.name)).collect::<Vec<_>>().join(", ");
    }
    fn get_backend_type(symname: IR) -> &'static str {
	if let IR::Call(ref name, _) = symname {
	    if *name == "Ptr".to_string() {
		return "i64";
	    }
	}
 	else if symname == IR::ID("bool".to_string()) {
	    return "i1";
	}
 	else if symname == IR::ID("i32".to_string()) {
	    return "i32";
	}
	else if symname == IR::ID("u32".to_string()) {
	    return "i32";
	}
	else if symname == IR::ID("i64".to_string()) {
	    return "i64";
	}
	else if symname == IR::ID("f32".to_string()) {
	    return "float";
	}
	else if symname == IR::ID("u64".to_string()) {
	    return "i64";
	}
	else if symname == IR::ID("isize".to_string()) {
	    return "i64";
	}
	else if symname == IR::ID("usize".to_string()) {
	    return "i64";
	}
	else if symname == IR::ID("i8".to_string()) {
	    return "i8";
	}
	else if symname == IR::ID("u8".to_string()) {
	    return "i8";
	}
	else if symname == IR::ID("void".to_string()) {
	    return "void";
	}
	else if symname == IR::ID("i16".to_string()) {
	    return "i16";
	}
	else if symname == IR::ID("u16".to_string()) {
	    return "i16";
	}
	return "i64";
    }
    fn get_value(&mut self, state: &mut CodeGenState, ir: IR) -> String {
	let reg = format!("%{}", self.get_free_register());
	if let IR::BinOp(op, elhs, erhs) = ir {
	    // let res: Vec<u8> = vec![];
	    let lhs = self.get_value(state, *elhs.clone());
	    let lt = state.typ.clone();
	    let rhs = self.get_value(state, *erhs.clone());
	    let rt = state.typ.clone();
	    if lt != rt {
		panic!("Failed to binop ({:?}){:?}\n and \n({:?}){:?}", elhs, lt, erhs, rt);
	    }
	    let lt = Self::get_backend_type(lt.unwrap());
	    if lt == "float".to_string() {
		if op == BinOpKind::Add {
		    self.output += &format!("    {} = fadd {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Sub {
		    self.output += &format!("    {} = fsub {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Mul {
		    self.output += &format!("    {} = fmul {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Div {
		    self.output += &format!("    {} = fdiv {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Mod {
		    self.output += &format!("    {} = frem {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Eq {
		    self.output += &format!("    {} = fcmp oeq {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::NEq {
		    self.output += &format!("    {} = fcmp one {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::Less {
		    self.output += &format!("    {} = fcmp olt {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::Bigger {
		    self.output += &format!("    {} = fcmp ogt {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::LessOrEq {
		    self.output += &format!("    {} = fcmp ole {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::BiggerOrEq {
		    self.output += &format!("    {} = fcmp oge {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
	    }
	    else {
		if op == BinOpKind::Add {
		    self.output += &format!("    {} = add {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Sub {
		    self.output += &format!("    {} = sub {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Mul {
		    self.output += &format!("    {} = mul {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Div {
		    self.output += &format!("    {} = udiv {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Mod {
		    self.output += &format!("    {} = urem {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::LShft {
		    self.output += &format!("    {} = shl {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::RShft {
		    self.output += &format!("    {} = lshr {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::And {
		    self.output += &format!("    {} = and {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Or {
		    self.output += &format!("    {} = or {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Xor {
		    self.output += &format!("    {} = xor {} {}, {}\n", reg, lt, lhs, rhs);
		}
		else if op == BinOpKind::Eq {
		    self.output += &format!("    {} = icmp eq {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::NEq {
		    self.output += &format!("    {} = icmp ne {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::Less {
		    self.output += &format!("    {} = icmp ult {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::Bigger {
		    self.output += &format!("    {} = icmp ugt {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::LessOrEq {
		    self.output += &format!("    {} = icmp ule {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
		else if op == BinOpKind::BiggerOrEq {
		    self.output += &format!("    {} = icmp uge {} {}, {}\n", reg, lt, lhs, rhs);
		    state.typ = Some(IR::ID("bool".to_string()));
		}
	    }
	}
	else if let IR::ID(ref id) = ir {
	    for var in self.local_variables.clone() {
		if var.name == *id {
		    self.output += &format!("    {} = load {}, ptr {}\n", reg, Self::get_backend_type(var.typ.clone().unwrap()), var.real_name);
		    state.typ = var.typ.clone();
		    return reg;
		}
	    }
	    panic!("Failed to found variable `{}`", id);
	}
	else if let IR::Integer(ref int) = ir {
	    self.output += &format!("    {} = add i64 0, {}\n", reg, int);
	    state.typ = Some(IR::ID("usize".to_string()));
	}
	else if let IR::Float(ref int) = ir {
	    self.output += &format!("    {} = fadd float 0x{:016X}, 0x{:016X}\n", reg, (0.0_f32 as f64).to_bits(), ((*int) as f64).to_bits());
	    state.typ = Some(IR::ID("f32".to_string()));
	}
	else if let IR::Str(ref id) = ir {
	    let temp_reg = format!("%{}", self.get_free_register());
	    let str_reg = format!("@.str.{}", self.get_free_register());
	    self.data_output += &format!("{} = private constant [{} x i8] c\"{}\\00\", align 1\n", str_reg, id.len()+1, id);
	    self.output += &format!("    {} = getelementptr i64, ptr {}\n", temp_reg, str_reg);
	    self.output += &format!("    {} = ptrtoint ptr {} to i64\n", reg, temp_reg);
	    state.typ = Some(IR::ID("usize".to_string()));
	}
	else if let IR::Call(ref name, ref args) = ir {
	    if *name == "return".to_string() {
		let res = self.get_value(state, args[0].clone());
		self.output += &format!("    ret {} {}\n", Self::get_backend_type(state.expected_typ.clone().unwrap()), res);
		self.end_of_block = true;
		return reg;
	    }
	    if *name == "return_void".to_string() {
		self.output += "    ret void\n";
		self.end_of_block = true;
		return reg;
	    }
	    if *name == "cast".to_string() {
		let value = self.get_value(state, args[0].clone());
		let ntyp = Self::get_backend_type(args[1].clone());
		let lsize = Self::get_backend_type(state.typ.clone().unwrap())[1..].parse::<usize>().unwrap();
		let rsize = ntyp[1..].parse::<usize>().unwrap();
		if lsize == rsize {
		    self.output += &format!("    {} = add {} 0, {}\n", reg, ntyp, value);
		}
		else if lsize > rsize {
		    self.output += &format!("    {} = trunc {} {} to {}\n", reg, Self::get_backend_type(state.typ.clone().unwrap()), value, ntyp);
		}
		else if lsize < rsize {
		    self.output += &format!("    {} = zext {} {} to {}\n", reg, Self::get_backend_type(state.typ.clone().unwrap()), value, ntyp);
		}
		state.typ = Some(args[1].clone());
		return reg;
	    }
	    if *name == "peek".to_string() {
		let int_addr = self.get_value(state, args[0].clone());
		let addr = format!("%{}", self.get_free_register());
		
		self.output += &format!("    {} = inttoptr {} {} to ptr\n", addr, Self::get_backend_type(state.typ.clone().unwrap()), int_addr);
		
		if let Some(IR::Call(wrapper, args)) = &state.typ {
		    if *wrapper != "Ptr".to_string() {
			panic!("Tried to peek from not a pointer {:?}", args[0].clone());
		    }
		    let elem_type = Self::get_backend_type(args[0].clone());
		    
		    self.output += &format!("    {} = load {}, ptr {}\n", reg, elem_type, addr);

		    state.typ = Some(args[0].clone());
		}
		else {
		    panic!("Tried to peek from not a pointer {:?}", args[0].clone());
		}
		return reg;
	    }
	    if *name == "poke".to_string() {
		let int_addr = self.get_value(state, args[0].clone());
		let addr_typ = state.typ.clone();
		let value = self.get_value(state, args[1].clone());
		let value_typ = state.typ.clone();
		let addr = format!("%{}", self.get_free_register());
		let oargs = args.clone();
		
		self.output += &format!("    {} = inttoptr {} {} to ptr\n", addr, Self::get_backend_type(addr_typ.clone().unwrap()), int_addr);
		
		if let Some(IR::Call(wrapper, args)) = addr_typ.clone() {
		    if *wrapper != "Ptr".to_string() {
			panic!("Tried to poke to not a pointer {:?}({:?})", oargs[0].clone(), addr_typ);
		    }
		    if args[0].clone() != value_typ.clone().unwrap() {
			panic!("Tried to poke a value {:?} that doesn't compatiable with the pointer {:?}", oargs[1].clone(), oargs[0].clone());
		    }
		    let elem_type = Self::get_backend_type(value_typ.unwrap());
		    
		    self.output += &format!("    store {} {}, ptr {}\n", elem_type, value, addr);
		    state.typ = Some(IR::ID("i32".to_string()));
		}
		else {
		    panic!("Tried to poke to not a pointer {:?}({:?})", oargs[0].clone(), addr_typ);
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
			let areg = self.get_value(state, args[arg].clone());
			if state.typ.clone() != Some(func.args[arg].typ.clone().unwrap()) {
			    panic!("Excepted type {:?} at call {} but got type {:?}", func.args[arg].typ.clone().unwrap(), *name, state.typ.clone().unwrap());
			}
			arg_regs.push(format!("{} {}", Self::get_backend_type(func.args[arg].typ.clone().unwrap()), areg));
		    }
		    if Self::get_backend_type(func.typ.clone()) != "void".to_string() {
			self.output += &format!("    {} = call ccc {} @{}({})\n", reg, Self::get_backend_type(func.typ.clone()), func.real_name, arg_regs.join(", "));
			state.typ = Some(func.typ);
		    }
		    else {
			self.output += &format!("    call ccc void @{}({})\n", func.real_name, arg_regs.join(", "));
			state.typ = Some(IR::ID("i32".to_string()));
		    }
		    return reg;
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
			res.extend(self.get_value(state, args[arg].clone()).unwrap());
			res.push(b',');
		    }
		    if args.len() > 0 {
			let arg = args.len()-1;
			res.extend(self.get_value(state, args[arg].clone()).unwrap());
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
	    let variables = self.local_variables.clone();
	    if *dt == DeclarationType::Global {
		state.expected_typ = Some(*typ.clone());
		self.output += &format!("define ccc {} @{}({}) {{\nentry:\n", Self::get_backend_type(state.expected_typ.clone().unwrap()), name, Self::get_function_def_args(args.to_vec()));
		let argv =
		    args.into_iter()
		    .map(
			|x| {
			    let arg = self.get_free_register();
			    self.output += &format!("    %{} = alloca {}\n", arg.clone(), Self::get_backend_type(x.typ.clone().unwrap()));
			    self.output += &format!("    store {} %{}, ptr %{}\n", Self::get_backend_type(x.typ.clone().unwrap()), x.name.clone(), arg.clone());
			    CodeGenFakeableVariable{
				name: format!("{}", x.name.clone()),
				real_name: format!("%{}", arg),
				typ: x.typ.clone(),
			    }
			}
		    ).collect::<Vec<_>>();
		self.local_variables.extend(argv);
		self.codegen(&state, *body.clone().unwrap());
		self.end_of_block = false;
		if Self::get_backend_type(state.expected_typ.clone().unwrap()) == "float" {
		    self.output += "    ret float 0.0\n}\n";
		}
		else if Self::get_backend_type(state.expected_typ.clone().unwrap()) != "void" {
		    self.output += &format!("    ret {} 0\n}}\n", Self::get_backend_type(state.expected_typ.clone().unwrap()));
		}
		else {
		    self.output += &format!("    ret void\n}}\n");
		}
		self.functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
	    }
	    else if *dt == DeclarationType::Local {
		state.expected_typ = Some(*typ.clone());
		let reg = self.get_free_register();
		self.output += &format!("define ccc {} @{}({}) {{\nentry:\n", Self::get_backend_type(state.expected_typ.clone().unwrap()), reg, Self::get_function_def_args(args.to_vec()));
		let argv =
		    args.into_iter()
		    .map(
			|x| {
			    let arg = self.get_free_register();
			    self.output += &format!("    %{} = alloca {}\n", arg.clone(), Self::get_backend_type(x.typ.clone().unwrap()));
			    self.output += &format!("    store {} %{}, ptr %{}\n", Self::get_backend_type(x.typ.clone().unwrap()), x.name.clone(), arg.clone());
			    CodeGenFakeableVariable{
				name: format!("{}", x.name.clone()),
				real_name: format!("%{}", arg),
				typ: x.typ.clone(),
			    }
			}
		    ).collect::<Vec<_>>();
		self.local_variables.extend(argv);
		self.codegen(&state, *body.clone().unwrap());
		self.end_of_block = false;
		if Self::get_backend_type(state.expected_typ.clone().unwrap()) == "float" {
		    self.output += "    ret float 0.0\n}\n";
		}
		else if Self::get_backend_type(state.expected_typ.clone().unwrap()) != "void" {
		    self.output += &format!("    ret {} 0\n}}\n", Self::get_backend_type(state.expected_typ.clone().unwrap()));
		}
		else {
		    self.output += &format!("    ret void\n}}\n");
		}
		self.functions.push(CodeGenFunction{name: name.to_string(), real_name: reg.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		self.output += &format!("declare ccc i32 @{}({})\n", ename, Self::get_function_def_args(args.to_vec()));
		self.functions.push(CodeGenFunction{name: name.to_string(), real_name: ename.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
	    }
	    else if let DeclarationType::Forward = *dt {
		self.functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone()});
	    }
	    self.local_variables = variables;
	}
	/*
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
	 */
	
	else if let IR::Import(ref module) = ir {
	    let filename;
	    if module.starts_with("./") {
		filename = module.clone();
	    }
	    else {
		filename = format!("/opt/Faivy/modules/{}", module);
	    }
	    let ast = FaivyParser::pretty_parse(
		FaivyParser::parse(Rule::program, &fs::read_to_string(&filename).unwrap()
		).unwrap()
	    );
	    let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
	    self.codegen(&CodeGenState {variables: vec![], typ: None, expected_typ: None}, IR::Block(ir));
	}
	else if let IR::Asm(_) = ir {
	    todo!()
	}
	else if let IR::LLVM(ref code) = ir {
	    self.output += &format!("{}\n", code);
	}
	else if let IR::If(ref cond, ref then, ref otherwise) = ir {
	    let variables = self.local_variables.clone();
	    let rcond = self.get_value(&mut state, *cond.clone());
	    let rthen = self.get_free_register();
	    let rotherwise = self.get_free_register();
	    let rend = self.get_free_register();
	    self.output += &format!("    br i1 {}, label %{}, label %{}\n", rcond, rthen, rotherwise);
	    self.output += &format!("{}:\n", rthen);
	    self.codegen(&state, *then.clone());
	    self.end_of_block = false;
	    self.output += &format!("    br label %{}\n", rend);
	    self.output += &format!("{}:\n", rotherwise);
	    if otherwise.is_some() {
		self.codegen(&state, *otherwise.clone().unwrap());
		self.end_of_block = false;
	    }
	    self.output += &format!("    br label %{}\n", rend);
	    self.output += &format!("{}:\n", rend);
	    self.local_variables = variables;
	}
	else if let IR::While(ref cond, ref body) = ir {
	    let variables = self.local_variables.clone();
	    let rbody = self.get_free_register();
	    let rreal_body = self.get_free_register();
	    let rend = self.get_free_register();
	    self.output += &format!("    br label %{}\n", rbody);
	    self.output += &format!("{}:\n", rbody);
	    let rcond = self.get_value(&mut state, *cond.clone());
	    if Self::get_backend_type(state.typ.clone().unwrap()) != "i1".to_string() {
		panic!("Excepted bool but got {:?}", state.typ.clone());
	    }
	    self.output += &format!("    br i1 {}, label %{}, label %{}\n", rcond, rreal_body, rend);
	    self.output += &format!("{}:\n", rreal_body);
	    self.codegen(&state, *body.clone());
	    self.end_of_block = false;
	    self.output += &format!("    br label %{}\n", rbody);
	    self.output += &format!("{}:\n", rend);
	    self.local_variables = variables;
	}
	else if let IR::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir {
	    if *dt != DeclarationType::Local {
		panic!("Global and Extern variables currently doesn't avaliable yet");
	    }
	    let value = self.get_value(&mut state, *body.clone().unwrap());
	    if state.typ.clone() != Some(*typ.clone()) {
		panic!("Excepted {:?} but got {:?}", Some(*typ.clone()), state.typ.clone());
	    }
	    let var = self.get_free_register();

	    self.output += &format!("    %{} = alloca {}\n", var, Self::get_backend_type(*typ.clone()));
	    self.output += &format!("    store {} {}, ptr %{}\n", Self::get_backend_type(*typ.clone()), value, var);
	    
	    self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: format!("%{}", var), typ: Some(*typ.clone())});
	}
	else if let IR::Set(ref name, ref body) = ir {
	    let value = self.get_value(&mut state, *body.clone());
	    for var in self.local_variables.clone() {
		if IR::ID(var.name) == **name {
		    self.output += &format!("    store {} {}, ptr {}\n", Self::get_backend_type(var.typ.clone().unwrap()), value, var.real_name);
		    return state;
		}
	    }
	    panic!("Failed to found variable `{:?}`", name);
	}
	else if let IR::Block(ref insts) = ir {
	    for inst in insts {
		if self.end_of_block {
		    break;
		}
		self.codegen(&mut state, inst.clone());
	    }
	}
	else if ir == IR::Dummy {
        }
	else {
	    if self.end_of_block {
		return state;
	    }
	    let _ = self.get_value(&mut state, ir);
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
	    Rule::decl_forward_type => {
		IR::Declaration(DeclarationType::Forward)
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
		let binopkind = if op == "+".to_string() {
		    BinOpKind::Add } else if op == "*".to_string() {
		    BinOpKind::Mul } else if op == "-".to_string() {
		    BinOpKind::Sub } else if op == "/".to_string() {
		    BinOpKind::Div } else if op == "%".to_string() {
		    BinOpKind::Mod } else if op == "<<".to_string() {
		    BinOpKind::LShft } else if op == ">>".to_string() {
		    BinOpKind::RShft } else if op == "==".to_string() {
		    BinOpKind::Eq } else if op == ">".to_string() {
		    BinOpKind::Bigger } else if op == "<".to_string() {
		    BinOpKind::Less } else if op == "<=".to_string() {
		    BinOpKind::BiggerOrEq } else if op == ">=".to_string() {
		    BinOpKind::LessOrEq } else if op == "!=".to_string() {
		    BinOpKind::NEq } else if op == "&".to_string() {
		    BinOpKind::And } else if op == "|".to_string() {
		    BinOpKind::Or } else if op == "^".to_string() {
		    BinOpKind::Xor } else {todo!()};
		IR::BinOp(binopkind, Box::new(lhs), Box::new(rhs))
	    },
	    Rule::expr_atom => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::expr => {
		Self::build(ast.inner[0].clone())
	    },
	    Rule::import => {
		IR::Import(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string())
	    },
	    Rule::asm => {
		IR::Asm(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string())
	    },
	    Rule::llvm => {
		IR::LLVM(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string())
	    },
	    Rule::call => {
		IR::Call(ast.inner[0].span.clone(), Self::build_expr_args(ast.inner[1].clone()).unwrap())
	    },
	    Rule::if_stmt => {
		IR::If(Box::new(Self::build(ast.inner[0].clone())), Box::new(Self::build(ast.inner[1].clone())), if ast.inner.len() == 3 {Some(Box::new(Self::build(ast.inner[2].clone())))} else {None})
	    }
	    Rule::while_stmt => {
		IR::While(Box::new(Self::build(ast.inner[0].clone())), Box::new(Self::build(ast.inner[1].clone())))
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
	    Rule::set => {
		let dest = Self::build(ast.inner[0].clone());
		let value = Self::build(ast.inner[1].clone());
		IR::Set(Box::new(dest), Box::new(value))
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
    let argv: Vec<_> = env::args().collect();
    if argv.len() < 2 {
	eprintln!("Usage: {} <Faivy source code file name> [linker flags]", argv[0].clone());
	return;
    }
    let ast = FaivyParser::pretty_parse(
	FaivyParser::parse(Rule::program, &fs::read_to_string(argv[1].clone()).unwrap()
	).unwrap()
    );
    let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
    let mut codegen = CodeGen::new();
    codegen.codegen(&CodeGenState {variables: vec![], typ: None, expected_typ: None}, IR::Block(ir));
    let mut code = String::new();
    code += &codegen.data_output;
    code += &codegen.output;
    let mut rng = rand::rng();
    let temp_llvm_ir = format!("/tmp/{}.ll", rng.random::<u32>().to_string());
    let _ = fs::write(temp_llvm_ir.clone(), code);
    let mut cc = Command::new("clang");
    cc.arg("-lm").arg(temp_llvm_ir).arg("-o").arg(format!("{}.out", argv[1].clone()));
    for i in 2..argv.len() {
	cc.arg(argv[i].clone());
    }
    let cc_output = cc.output().unwrap();
    if !cc_output.clone().status.success() {
	panic!("Failed to link program:\n{:?}", cc_output);
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test() {
	let ast = FaivyParser::pretty_parse(
	    FaivyParser::parse(Rule::program, &fs::read_to_string("tests/smoke.faivy").unwrap()
	    ).unwrap()
	);
	let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
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
	assert_eq!(Command::new("./a.out").output().unwrap().status.success(), true);
    }

    #[test]
    fn unit_test00() {
	let ast = FaivyParser::pretty_parse(
	    FaivyParser::parse(Rule::program, &fs::read_to_string("tests/unit00.faivy").unwrap()
	    ).unwrap()
	);
	let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
	let mut codegen = CodeGen::new();
	codegen.codegen(&CodeGenState {variables: vec![], typ: None}, IR::Block(ir));
	let mut code = String::new();
	code += &format!("{}", codegen.output);
	println!("{:?}", code);
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
	let a_res = Command::new("./a.out").output().unwrap();
	assert_eq!(a_res.status.success(), true);
	assert_eq!(String::from_utf8(a_res.stdout).unwrap(), fs::read_to_string("tests/unit00.out").unwrap());
    }
}
*/
