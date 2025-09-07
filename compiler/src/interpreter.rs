use crate::{ir::*, codegen::*, system, parser::*};
use pest::Parser;
impl CodeGen {
/*
    fn const_interpret(ir: IR) -> IR {
	if let IR::Float(_) = ir {
	    return ir;
	}
	if let IR::Integer(_) = ir {
	    return ir;
	}
	if let IR::BinOp(ref op, ref elhs, ref erhs) = ir {
	    let op = op.clone();
	    let lhs = Self::const_interpret(*elhs.clone());
	    let rhs = Self::const_interpret(*erhs.clone());
	    if let IR::Float(ref lhs) = lhs {
		if let IR::Float(ref rhs) = rhs {
		    if op == BinOpKind::Add {
			return IR::Float(lhs+rhs);
		    }
		    else if op == BinOpKind::Sub {
			return IR::Float(lhs-rhs);
		    }
		    else if op == BinOpKind::Mul {
			return IR::Float(lhs*rhs);
		    }
		    else if op == BinOpKind::Div {
			return IR::Float(lhs/rhs);
		    }
		    else if op == BinOpKind::Mod {
			return IR::Float(lhs%rhs);
		    }
		    else if op == BinOpKind::Eq {
			return IR::Integer(if lhs==rhs {1} else {0});
		    }
		    else if op == BinOpKind::NEq {
			return IR::Integer(if lhs!=rhs {1} else {0});
		    }
		    else if op == BinOpKind::Less {
			return IR::Integer(if lhs<rhs {1} else {0});
		    }
		    else if op == BinOpKind::Bigger {
			return IR::Integer(if lhs>rhs {1} else {0});
		    }
		    else if op == BinOpKind::LessOrEq {
			return IR::Integer(if lhs<=rhs {1} else {0});
		    }
		    else if op == BinOpKind::BiggerOrEq {
			return IR::Integer(if lhs>=rhs {1} else {0});
		    }
		}
	    }
	    if let IR::Integer(ref lhs) = lhs {
		if let IR::Integer(ref rhs) = rhs {
		    if op == BinOpKind::Add {
			return IR::Integer(lhs+rhs);
		    }
		    else if op == BinOpKind::Sub {
			return IR::Integer(lhs-rhs);
		    }
		    else if op == BinOpKind::Mul {
			return IR::Integer(lhs*rhs);
		    }
		    else if op == BinOpKind::Div {
			return IR::Integer(lhs/rhs);
		    }
		    else if op == BinOpKind::Mod {
			return IR::Integer(lhs%rhs);
		    }
		    else if op == BinOpKind::Eq {
			return IR::Integer(if lhs==rhs {1} else {0});
		    }
		    else if op == BinOpKind::NEq {
			return IR::Integer(if lhs!=rhs {1} else {0});
		    }
		    else if op == BinOpKind::Less {
			return IR::Integer(if lhs<rhs {1} else {0});
		    }
		    else if op == BinOpKind::Bigger {
			return IR::Integer(if lhs>rhs {1} else {0});
		    }
		    else if op == BinOpKind::LessOrEq {
			return IR::Integer(if lhs<=rhs {1} else {0});
		    }
		    else if op == BinOpKind::BiggerOrEq {
			return IR::Integer(if lhs>=rhs {1} else {0});
		    }
		}
	    }
	}
	panic!("Failed to interpret {:?} constantly", ir);
    }
     */
    pub fn dyn_interpret(&mut self, state: &mut CodeGenState, ir: IR) -> IR {
	if let IRData::Float(_) = ir.data {
	    return ir;
	}
	if let IRData::Integer(_) = ir.data {
	    return ir;
	}
	if let IRData::Str(_) = ir.data {
	    return ir;
	}
	if let IRData::ID(ref id) = ir.data {
	    if *id == "__line__".to_string() {
		return IR::new(IRData::Integer(ir.row.unwrap() as u64), None, None);
	    }
	    if *id == "__col__".to_string() {
		return IR::new(IRData::Integer(ir.col.unwrap() as u64), None, None);
	    }
	    for var in self.local_variables.clone() {
		if var.name == *id {
		    if let Some(ref value) = var.value {
			state.typ = var.typ.clone();
			return value.clone();
		    }
		    panic!("Failed to found constant `{}` at {} because it not a constant", id, self.fmt_pos(&ir));
		}
	    }
	    panic!("Failed to found constant `{}` at {}", id, self.fmt_pos(&ir));
	}
	else if let IRData::Call(ref name, ref args) = ir.data {
	    if *name == "os_run".to_string() {
		unsafe {
		    let IRData::Str(ref cmd) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a string", self.fmt_pos(&ir))};
		    return IR::new(IRData::Integer(system(format!("{}\0", cmd).as_ptr() as *const i8) as u64), None, None);
		}
	    }
	    if *name == "print_str".to_string() {
		let IRData::Str(ref value) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a string", self.fmt_pos(&ir))};
		print!("{}", value);
		return IR::new(IRData::Dummy, None, None);
	    }
	    if *name == "print_char".to_string() {
		let IRData::Integer(ref value) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a number", self.fmt_pos(&ir))};
		print!("{}", *value as u8 as char);
		return IR::new(IRData::Dummy, None, None);
	    }
	    if *name == "print_usize".to_string() {
		let IRData::Integer(ref value) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a number", self.fmt_pos(&ir))};
		print!("{}", *value);
		return IR::new(IRData::Dummy, None, None);
	    }
	    if *name == "emit_faivy".to_string() {
		let IRData::Str(ref value) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a string", self.fmt_pos(&ir))};
		let ast = FaivyParser::pretty_parse(
		    FaivyParser::parse(Rule::program, value
		    ).unwrap()
		);
		let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
		self.codegen(state, IR::new(IRData::Scope(ir), None, None));
		return IR::new(IRData::Dummy, None, None);
	    }
	}
	else if let IRData::BinOp(ref op, ref elhs, ref erhs) = ir.data {
	    let op = op.clone();
	    let lhs = self.dyn_interpret(state, *elhs.clone()).data;
	    let rhs = self.dyn_interpret(state, *erhs.clone()).data;
	    if let IRData::Float(ref lhs) = lhs {
		if let IRData::Float(ref rhs) = rhs {
		    if op == BinOpKind::Add {
			return IR::new(IRData::Float(lhs+rhs), None, None);
		    }
		    else if op == BinOpKind::Sub {
			return IR::new(IRData::Float(lhs-rhs), None, None);
		    }
		    else if op == BinOpKind::Mul {
			return IR::new(IRData::Float(lhs*rhs), None, None);
		    }
		    else if op == BinOpKind::Div {
			return IR::new(IRData::Float(lhs/rhs), None, None);
		    }
		    else if op == BinOpKind::Mod {
			return IR::new(IRData::Float(lhs%rhs), None, None);
		    }
		    else if op == BinOpKind::Eq {
			return IR::new(IRData::Integer(if lhs==rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::NEq {
			return IR::new(IRData::Integer(if lhs!=rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::Less {
			return IR::new(IRData::Integer(if lhs<rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::Bigger {
			return IR::new(IRData::Integer(if lhs>rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::LessOrEq {
			return IR::new(IRData::Integer(if lhs<=rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::BiggerOrEq {
			return IR::new(IRData::Integer(if lhs>=rhs {1} else {0}), None, None);
		    }
		}
	    }
	    if let IRData::Integer(ref lhs) = lhs {
		if let IRData::Integer(ref rhs) = rhs {
		    if op == BinOpKind::Add {
			return IR::new(IRData::Integer(lhs+rhs), None, None);
		    }
		    else if op == BinOpKind::Sub {
			return IR::new(IRData::Integer(lhs-rhs), None, None);
		    }
		    else if op == BinOpKind::Mul {
			return IR::new(IRData::Integer(lhs*rhs), None, None);
		    }
		    else if op == BinOpKind::Div {
			return IR::new(IRData::Integer(lhs/rhs), None, None);
		    }
		    else if op == BinOpKind::Mod {
			return IR::new(IRData::Integer(lhs%rhs), None, None);
		    }
		    else if op == BinOpKind::Eq {
			return IR::new(IRData::Integer(if lhs==rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::NEq {
			return IR::new(IRData::Integer(if lhs!=rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::Less {
			return IR::new(IRData::Integer(if lhs<rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::Bigger {
			return IR::new(IRData::Integer(if lhs>rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::LessOrEq {
			return IR::new(IRData::Integer(if lhs<=rhs {1} else {0}), None, None);
		    }
		    else if op == BinOpKind::BiggerOrEq {
			return IR::new(IRData::Integer(if lhs>=rhs {1} else {0}), None, None);
		    }
		}
	    }
	}
	// TODOO: unaryop
	// TODOOO: call
	// TODOOO: casts
	// TODO: structs
	// TODOO: functions
	// TODO: defer
	else if let IRData::Asm(_) = ir.data {
	    todo!("Inline assembly doesn't supported yet")
	}
	else if let IRData::LLVM(ref code) = ir.data {
	    self.output += &format!("{}\n", code);
	    return IR::new(IRData::Dummy, None, None);
	}
	else if let IRData::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir.data {
	    if *dt == DeclarationType::Local {
		let rn = self.dyn_interpret(state, *body.clone().unwrap());
		self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: name.to_string(), value: Some(rn), typ: Some(*typ.clone())});
	    }
	    else if let DeclarationType::Extern(ref ename) = *dt {
		self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: ename.to_string(), value: None, typ: Some(*typ.clone())});
	    }
	    else if let DeclarationType::Const = *dt {
		let rn = self.dyn_interpret(state, *body.clone().unwrap());
		self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: name.to_string(), value: Some(rn), typ: Some(*typ.clone())});
	    }
	}
	else if let IRData::Set(ref name, ref body) = ir.data {
	    let value = self.dyn_interpret(state, *body.clone());
	    for var in &mut self.local_variables {
		if IRData::ID(var.clone().name) == (**name).data {
		    var.value = Some(value);
		    return IR::new(IRData::Dummy, None, None);
		}
	    }
	    panic!("Failed to found variable `{:?}` at {}", name, self.fmt_pos(&ir));
	}
	else if let IRData::If(ref cond, ref then, ref otherwise) = ir.data {
	    let rcond = self.dyn_interpret(state, *cond.clone());
	    if let IRData::Integer(ref doit) = rcond.data {
		if *doit != 0 {
		    self.dyn_interpret(state, *then.clone());
		}
		else if otherwise.is_some() {
		    self.dyn_interpret(state, *otherwise.clone().unwrap());
		}
	    }
	    else {
		panic!("Excepted int-like type but got {:?} at {}", rcond, self.fmt_pos(&ir));
	    }
	    return IR::new(IRData::Dummy, None, None);
	}
	else if let IRData::While(ref cond, ref body) = ir.data {
	    loop {
		let rcond = self.dyn_interpret(state, *cond.clone());
		if let IRData::Integer(ref doit) = rcond.data {
		    if *doit == 0 {
			break;
		    }
		}
		else {
		    panic!("Excepted int-like type but got {:?} at {}", rcond, self.fmt_pos(&ir));
		}
		self.dyn_interpret(state, *body.clone());
	    }
	    return IR::new(IRData::Dummy, None, None);
	}
	else if let IRData::Block(ref insts) = ir.data {
	    for inst in insts {
		if self.end_of_block {
		    break;
		}
		self.dyn_interpret(state, inst.clone());
	    }
	    return IR::new(IRData::Dummy, None, None);
	}
	else if let IRData::Scope(ref insts) = ir.data {
	    let dfb = self.defer_block.clone();
	    self.defer_block = vec![];
	    let variables = self.local_variables.clone();
	    for inst in insts {
		if self.end_of_block {
		    break;
		}
		self.dyn_interpret(state, inst.clone());
	    }
	    if !self.end_of_block {
		for block in self.defer_block.clone() {
		    self.dyn_interpret(state, block.clone());
		}
	    }
	    self.local_variables = variables;
	    self.defer_block = dfb;
	    return IR::new(IRData::Dummy, None, None);
	}
	panic!("Failed to interpret {:?} dynamicly at {}", ir, self.fmt_pos(&ir));
    }
}
