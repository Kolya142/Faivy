use crate::{ir::*, codegen::*, system, dlopen, dlclose, dlsym, malloc, free, memcpy, printf, parser::*};
use libffi::low::*;
use std::ffi::CString;
use std::io::Write;
impl CodeGen {
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
                    panic!("Failed to found `{}` at {} because it doesn't known at compile time", id, self.fmt_pos(&ir));
                }
            }
            panic!("Failed to found `{}` at {}", id, self.fmt_pos(&ir));
        }
        else if let IRData::Call(ref name, ref args) = ir.data {
            if *name == "os_run".to_string() {
                unsafe {
                    let IRData::Str(ref cmd) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a string", self.fmt_pos(&ir))};
                    return IR::new(IRData::Integer(system(format!("{}\0", cmd).as_ptr() as *const i8) as u64), None, None);
                }
            }
            if *name == "return".to_string() {
                // TODO: return void
                let value = self.dyn_interpret(state, args.clone()[0].clone());
                self.end_of_block = true;
                return value;
            }
            if *name == "SizeOf".to_string() {
                return IR::new(IRData::Integer(self.get_type_size(args[0].clone()).try_into().unwrap()), None, None);
            }
            if *name == "cast".to_string() {
                return args[0].clone();
            }
            if *name == "emit_faivy".to_string() {
                let IRData::Str(ref value) = self.dyn_interpret(state, args.clone()[0].clone()).data else {todo!("data at {} is not a string", self.fmt_pos(&ir))};
                /*
                let ast = FaivyParser::pretty_parse(
                    FaivyParser::parse(Rule::program, value
                    ).unwrap()
                );
                let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
                self.codegen(state, IR::new(IRData::Scope(ir), None, None));
                return IR::new(IRData::Dummy, None, None);
                 */
                todo!()
            }
            for func in self.comp_time_functions.clone() {
                if func.name == *name {
                    if args.len() != func.args.len() {
                        panic!("Excepted {} arguments at {} but got {} arguments", func.args.len(), *name, args.len());
                    }
                    let Some(body) = func.body else {
                        panic!("{}: Compiler bug", self.fmt_pos(&ir));
                    };
                    let saved_locals = self.local_variables.clone();
                    for i in 0..func.args.len() {
                        let x = func.args[i].clone();
                        let lv = CodeGenFakeableVariable{
                            name: format!("{}", x.name.clone()),
                            real_name: format!("{}", x.name.clone()),
                            typ: x.typ.clone(),
                            value: Some(self.dyn_interpret(state, args[i].clone()))
                        };
                        self.local_variables.push(lv);
                    }
                    let res = self.dyn_interpret(state, body);
                    self.end_of_block = false;
                    self.local_variables = saved_locals;
                    return res;
                }
            }
            for func in self.functions.clone() {
                if func.name == *name {
                    if args.len() != func.args.len() {
                        panic!("Excepted {} arguments at {} but got {} arguments", func.args.len(), *name, args.len());
                    }
                    let Some(body) = func.body else {
                        let args: Vec<IRData> = args.iter().map(|arg| self.dyn_interpret(state, arg.clone()).data).collect();
                        unsafe {
                            let mut targs: Vec<*mut ffi_type> = vec![];
                            let mut vargs: Vec<*mut _> = vec![];
                            let mut queue_to_free: Vec<*mut _> = vec![];
                            let mut cif: ffi_cif = Default::default();
                            let dl = dlopen(c"/lib/x86_64-linux-gnu/libc.so.6".as_ptr(), 2);
                            if dl == std::ptr::null_mut() {
                                panic!("Your system doesn't support x86_64 GNU libc 6");
                            }
                            let sym = dlsym(dl, CString::new(func.real_name.clone()).unwrap().as_ptr());
                            if sym == std::ptr::null_mut() {
                                panic!("Unknown GNU libc 6 symbol: `{}`", func.real_name);
                            }

                            for arg in args.clone() {
                                match arg {
                                    IRData::Integer(i) => {
                                        targs.push(&raw mut types::uint64);
                                        let a = malloc(8) as *mut usize;
                                        *a = i as usize;
                                        vargs.push(a as *mut _);
                                    }
                                    IRData::Str(s) => {
                                        let cs = CString::new(s).unwrap();
                                        targs.push(&raw mut types::uint64);
                                        let s = cs.count_bytes() + 1;
                                        let a = malloc(s) as *mut ();
                                        memcpy(a, cs.as_ptr() as *const (), s);
                                        let b = malloc(8) as *mut ();
                                        *(b as *mut usize) = a as usize;
                                        vargs.push(b as *mut _);
                                        queue_to_free.push(a as *mut _);
                                    }
                                    _ => todo!()
                                }
                            }
                            
                            prep_cif(&mut cif, ffi_abi_FFI_DEFAULT_ABI, targs.len(), &raw mut types::uint64, targs.as_mut_ptr()).unwrap();
                            
                            let res = call::<u64>(&mut cif, CodePtr(sym as *mut _), vargs.as_mut_ptr());
                            for varg in vargs {
                                free(varg as *mut ());
                            }
                            for f in queue_to_free {
                                free(f as *mut ());
                            }
                            dlclose(dl);
                            return IR::new(IRData::Integer(res), Some(0), Some(0));
                        }
                    };
                    let saved_locals = self.local_variables.clone();
                    for i in 0..func.args.len() {
                        let x = func.args[i].clone();
                        let lv = CodeGenFakeableVariable{
                            name: format!("{}", x.name.clone()),
                            real_name: format!("{}", x.name.clone()),
                            typ: x.typ.clone(),
                            value: Some(self.dyn_interpret(state, args[i].clone()))
                        };
                        self.local_variables.push(lv);
                    }
                    let res = self.dyn_interpret(state, body);
                    self.end_of_block = false;
                    self.local_variables = saved_locals;
                    return res;
                }
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
        // TODOOO: casts
        // TODO: structs
        // TODO: defer
        else if let IRData::Asm(_) = ir.data {
            todo!("Inline assembly doesn't supported yet")
        }
        else if let IRData::LLVM(ref code) = ir.data {
            todo!("Inline LLVM in interpret mode doesn't supported because it is interpret. if you want to emit to executable - use emit_faivy() instead")
        }
        else if let IRData::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir.data {
            if *dt == DeclarationType::Local {
                let rn = self.dyn_interpret(state, *body.clone().unwrap());
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: name.to_string(), value: Some(rn), typ: Some(*typ.clone())});
                return IR::new(IRData::Dummy, None, None);
            }
            else if let DeclarationType::Extern(ref ename) = *dt {
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: ename.to_string(), value: None, typ: Some(*typ.clone())});
                return IR::new(IRData::Dummy, None, None);
            }
            else if let DeclarationType::Const = *dt {
                let rn = self.dyn_interpret(state, *body.clone().unwrap());
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: name.to_string(), value: Some(rn), typ: Some(*typ.clone())});
                return IR::new(IRData::Dummy, None, None);
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
                let v = self.dyn_interpret(state, inst.clone());
                if self.end_of_block {
                    return v;
                }
            }
            return IR::new(IRData::Dummy, None, None);
        }
        else if let IRData::Scope(ref insts) = ir.data {
            let dfb = self.defer_block.clone();
            self.defer_block = vec![];
            let variables_length = self.local_variables.len();
            for inst in insts {
                let v = self.dyn_interpret(state, inst.clone());
                if self.end_of_block {
                    return v;
                }
            }
            if !self.end_of_block {
                for block in self.defer_block.clone() {
                    self.dyn_interpret(state, block.clone());
                }
            }
            self.local_variables = self.local_variables[0..variables_length].to_vec();
            self.defer_block = dfb;
            return IR::new(IRData::Dummy, None, None);
        }
        if let IRData::DeclareFunction(ref dt, ref name, ref typ, ref args, ref body) = ir.data {
            let variables = self.local_variables.clone();
            if *dt == DeclarationType::Global {
                let argv =
                    args.into_iter()
                    .map(
                        |x| CodeGenFakeableVariable{
                            name: format!("{}", x.name.clone()),
                            real_name: format!("{}", x.name.clone()),
                            typ: x.typ.clone(),
                            value: None
                        }
                    ).collect::<Vec<_>>();
                self.comp_time_functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: Some(*body.clone().unwrap()), other_implementations: vec![]});
                return IR::new(IRData::Dummy, None, None);
            }
            else if *dt == DeclarationType::Local {
                let argv =
                    args.into_iter()
                    .map(
                        |x| CodeGenFakeableVariable{
                            name: format!("{}", x.name.clone()),
                            real_name: format!("{}", x.name.clone()),
                            typ: x.typ.clone(),
                            value: None
                        }
                    ).collect::<Vec<_>>();
                self.comp_time_functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: Some(*body.clone().unwrap()), other_implementations: vec![]});
                return IR::new(IRData::Dummy, None, None);
            }
            else if let DeclarationType::Extern(ref ename) = *dt {
                panic!("DeclarationType::Extern declaration doesn't supported in interpret mode ({})\n", self.fmt_pos(&ir));
            }
            else if let DeclarationType::Forward = *dt {
                panic!("DeclarationType::Forward declaration doesn't supported in interpret mode ({})\n", self.fmt_pos(&ir));
            }
            self.local_variables = variables;
        }
        panic!("Failed to interpret {:?} dynamicly at {}", ir, self.fmt_pos(&ir));
    }
}
