use std::fs;
use std::collections::HashMap;
use crate::{parser::*, ir::*};

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGenVariable {
    pub name: String,
    pub typ: Option<IR>,
}

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGenFakeableVariable {
    pub real_name: String,
    pub name: String,
    pub value: Option<IR>,
    pub typ: Option<IR>,
}

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGenFunction {
    pub name: String,
    pub real_name: String,
    pub typ: IR,
    pub args: Vec<CodeGenVariable>,
    pub decl: DeclarationType,
    pub ispolymorph: bool,
    pub body: Option<IR>,
    pub other_implementations: Vec<CodeGenFunction>,
}

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGenStruct {
    pub real_name: String,
    pub name: String,
    pub fields: Vec<CodeGenVariable>,
}

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGenState {
    pub variables: Vec<CodeGenFakeableVariable>,
    pub typ: Option<IR>,
    pub expected_typ: Option<IR>
}

#[derive(Debug,Clone,PartialEq)]
pub struct CodeGen {
    pub output: String,
    pub data_output: String,
    pub end_of_block: bool,
    pub functions: Vec<CodeGenFunction>,
    pub comp_time_functions: Vec<CodeGenFunction>,
    // global_variables: Vec<CodeGenFakeableVariable>,
    pub defer_block: Vec<IR>,
    pub imported: Vec<String>,
    pub current_file: Option<String>,
    pub local_variables: Vec<CodeGenFakeableVariable>,
    pub structs: Vec<CodeGenStruct>,
    pub strings: HashMap<String, String>,
    pub oid: usize,
}

impl CodeGen {
    pub fn new(filename: Option<String>) -> Self {
	Self {
	    output: String::new(),
	    data_output: String::new(),
	    end_of_block: false,
	    functions: vec![],
	    comp_time_functions: vec![],
	    // global_variables: vec![],
	    defer_block: vec![],
            strings: HashMap::new(),
            imported: vec![],
            current_file: filename,
            local_variables: vec![],
            structs: vec![],
            oid: 0,
        }
    }
    pub fn get_free_register(&mut self) -> String {
        let name = format!("reg{}", self.oid);
        self.oid += 1;
        name
    }
    pub fn get_function_def_args(&self, args: Vec<CodeGenVariable>) -> String {
        return args.into_iter().map(|x| format!("{} %{}", self.get_backend_type(x.typ.unwrap()), x.name)).collect::<Vec<_>>().join(", ");
    }
    pub fn get_float_from_string(value: String) -> f32 {
        if value.starts_with("0x") {
            f64::from_bits(u64::from_str_radix(&value[2..], 16).unwrap()) as f32
        }
        else {
            value.parse::<f32>().unwrap()
        }
    }
    pub fn get_int_from_string(value: String) -> u64 {
        if value.clone().starts_with("0x") {
            u64::from_str_radix(&value[2..], 16).unwrap()
        }
        else {
            value.parse::<u64>().unwrap()
        }
    }
    pub fn get_backend_type(&self, symname: IR) -> String {
        if let IRData::Call(ref name, _) = symname.data {
            if *name == "Ptr".to_string() {
                return "i64".to_string();
            }
        }
         else if symname.data == IRData::ID("bool".to_string()) {
            return "i1".to_string();
        }
         else if symname.data == IRData::ID("i32".to_string()) {
            return "i32".to_string();
        }
        else if symname.data == IRData::ID("u32".to_string()) {
            return "i32".to_string();
        }
        else if symname.data == IRData::ID("i64".to_string()) {
            return "i64".to_string();
        }
        else if symname.data == IRData::ID("f32".to_string()) {
            return "float".to_string();
        }
        else if symname.data == IRData::ID("u64".to_string()) {
            return "i64".to_string();
        }
        else if symname.data == IRData::ID("isize".to_string()) {
            return "i64".to_string();
        }
        else if symname.data == IRData::ID("usize".to_string()) {
            return "i64".to_string();
        }
        else if symname.data == IRData::ID("i8".to_string()) {
            return "i8".to_string();
        }
        else if symname.data == IRData::ID("u8".to_string()) {
            return "i8".to_string();
        }
        else if symname.data == IRData::ID("void".to_string()) {
            return "void".to_string();
        }
        else if symname.data == IRData::ID("i16".to_string()) {
            return "i16".to_string();
        }
        else if symname.data == IRData::ID("u16".to_string()) {
            return "i16".to_string();
        }
        for structure in &self.structs {
            if IRData::ID(structure.name.clone()) == symname.data {
                return structure.real_name.clone();
            }
        }
        panic!("Unknown type name: {:?}", symname);
    }
    pub fn get_type_size(&self, symname: IR) -> usize {
        if let IRData::Call(ref name, _) = symname.data {
            if *name == "Ptr".to_string() {
                return 64;
            }
        }
         else if symname.data == IRData::ID("bool".to_string()) {
            return 1;
        }
         else if symname.data == IRData::ID("i32".to_string()) {
            return 32;
        }
        else if symname.data == IRData::ID("u32".to_string()) {
            return 32;
        }
        else if symname.data == IRData::ID("i64".to_string()) {
            return 64;
        }
        else if symname.data == IRData::ID("f32".to_string()) {
            return 32;
        }
        else if symname.data == IRData::ID("u64".to_string()) {
            return 64;
        }
        else if symname.data == IRData::ID("isize".to_string()) {
            return 64;
        }
        else if symname.data == IRData::ID("usize".to_string()) {
            return 64;
        }
        else if symname.data == IRData::ID("i8".to_string()) {
            return 8;
        }
        else if symname.data == IRData::ID("u8".to_string()) {
            return 8;
        }
        else if symname.data == IRData::ID("void".to_string()) {
            return 0;
        }
        else if symname.data == IRData::ID("i16".to_string()) {
            return 2;
        }
        else if symname.data == IRData::ID("u16".to_string()) {
            return 2;
        }
        for structure in &self.structs {
            if IRData::ID(structure.name.clone()) == symname.data {
                /*
                struct a {
                    i8 a;
                    i16 b;
                }

                sizeof(a) == 4
                */

                let mut size = 0usize;
                let mut max = 1usize;

                for field in structure.fields.clone() {
                    let fs = self.get_type_size(field.typ.unwrap());
                    if fs > max {
                        max = fs;
                    }
                    size += fs + (fs - (size % fs)) % fs;
                }
                
                return size + (max - (size % max)) % max;
            }
        }
        panic!("Unknown type name: {:?}", symname);
    }
    pub fn fmt_pos(&mut self, ir: &IR) -> String {
        format!("`{}`({}:{})", self.current_file.clone().or(Some("<scratch>".to_string())).unwrap(), ir.row.or(Some(0)).unwrap(), ir.col.or(Some(0)).unwrap())
    }
    pub fn get_value(&mut self, state: &mut CodeGenState, ir: IR) -> String {
        self.output += &format!("; {:?}\n", ir);
        let reg = format!("%{}", self.get_free_register());
        if let IRData::BinOp(ref op, ref elhs, ref erhs) = ir.data {
            
            if *op == BinOpKind::Dot {
                let lhs = self.get_value(state, *elhs.clone());
                let lt = state.typ.clone().unwrap();
                let ptr_reg = self.get_free_register();
                let temp_reg = self.get_free_register();
                let sp = self.get_free_register();
                
                for structure in &self.structs {
                    if IRData::ID(structure.name.clone()) == lt.data {
                        let mut fieldi = 0;
                        while fieldi < structure.fields.len() {
                            if IRData::ID(structure.fields[fieldi].name.clone()) == (*erhs).data {
                                break;
                            }
                            fieldi += 1;
                        }
                        if fieldi == structure.fields.len() {
                            panic!("Failed to find element {:?} of structure {:?} at {}", erhs, lt, self.fmt_pos(&ir));
                        }
                        self.output += &format!("    %{} = call ptr @llvm.stacksave()\n", sp);
                        self.output += &format!("    %{} = alloca i1, i64 {}, align 16\n", ptr_reg, self.get_type_size(lt.clone()));
                        self.output += &format!("    store {} {}, ptr %{}\n", self.get_backend_type(lt.clone()), lhs, ptr_reg);
                        self.output += &format!("    %{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}\n", temp_reg, self.get_backend_type(lt), ptr_reg, fieldi);
                        self.output += &format!("    {} = ptrtoint ptr %{} to i64\n", reg, temp_reg);
                        state.typ = Some(IR::new(IRData::Call("Ptr".to_string(), vec![structure.fields[fieldi].typ.clone().unwrap()]), None, None));
                        self.output += &format!("    call void @llvm.stackrestore(ptr %{})\n", sp);

                        return reg;
                    }
                }

                if let IRData::Call(ref name, ref ltv) = lt.data {
                    let lt = ltv[0].clone();
                    if *name == "Ptr".to_string() {
                        for structure in &self.structs {
                            if IRData::ID(structure.name.clone()) == lt.data {
                                let mut fieldi = 0;
                                while fieldi < structure.fields.len() {
                                    if IRData::ID(structure.fields[fieldi].name.clone()) == (*erhs).data {
                                        break;
                                    }
                                    fieldi += 1;
                                }
                                if fieldi == structure.fields.len() {
                                    panic!("Failed to find element {:?} of structure {:?} at {}", erhs, lt, self.fmt_pos(&ir));
                                }
                                self.output += &format!("    %{} = inttoptr i64 {} to ptr\n", ptr_reg, lhs);
                                self.output += &format!("    %{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}\n", temp_reg, self.get_backend_type(lt), ptr_reg, fieldi);
                                self.output += &format!("    {} = ptrtoint ptr %{} to i64\n", reg, temp_reg);
                                state.typ = Some(IR::new(IRData::Call("Ptr".to_string(), vec![structure.fields[fieldi].typ.clone().unwrap()]), None, None));

                                return reg;
                            }
                        }

                        panic!("Failed to find structure {:?} at {}", lt, self.fmt_pos(&ir));
                    }
                }
                panic!("{:?} - not a structure or pointer to a structure at {}", lt, self.fmt_pos(&ir));
            }
            else {
                // let res: Vec<u8> = vec![];
                let lhs = self.get_value(state, *elhs.clone());
                let lt = state.typ.clone();
                let rhs = self.get_value(state, *erhs.clone());
                let rt = state.typ.clone();
                if lt.clone().unwrap().data != rt.clone().unwrap().data {
                    panic!("Failed to binop ({:?}){:?}\n and \n({:?}){:?} at {}", elhs, lt, erhs, rt, self.fmt_pos(&ir));
                }
                let lt = self.get_backend_type(lt.unwrap());
                if lhs.chars().next().unwrap().is_digit(10) && rhs.chars().next().unwrap().is_digit(10) {
                    if lt == "float".to_string() {
                        if *op == BinOpKind::Add {
                            return self.get_value(state, IR::new(IRData::Float(Self::get_float_from_string(lhs)+Self::get_float_from_string(rhs)), None, None));
                        }
                        else if *op == BinOpKind::Sub {
                            return self.get_value(state, IR::new(IRData::Float(Self::get_float_from_string(lhs)-Self::get_float_from_string(rhs)), None, None));
                        }
                        else if *op == BinOpKind::Mul {
                            return self.get_value(state, IR::new(IRData::Float(Self::get_float_from_string(lhs)*Self::get_float_from_string(rhs)), None, None));
                        }
                        else if *op == BinOpKind::Div {
                            return self.get_value(state, IR::new(IRData::Float(Self::get_float_from_string(lhs)/Self::get_float_from_string(rhs)), None, None));
                        }
                        else if *op == BinOpKind::Mod {
                            return self.get_value(state, IR::new(IRData::Float(Self::get_float_from_string(lhs)%Self::get_float_from_string(rhs)), None, None));
                        }
                        else if *op == BinOpKind::Eq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)==Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::NEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)!=Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::Less {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)<Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::Bigger {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)>Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::LessOrEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)<=Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::BiggerOrEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_float_from_string(lhs)>=Self::get_float_from_string(rhs) {1} else {0}).to_string();
                        }
                    }
                    else {
                        if *op == BinOpKind::Add {
                            return (Self::get_int_from_string(lhs)+Self::get_int_from_string(rhs)).to_string();
                        }
                        else if *op == BinOpKind::Sub {
                            return (Self::get_int_from_string(lhs)-Self::get_int_from_string(rhs)).to_string();
                        }
                        else if *op == BinOpKind::Mul {
                            return (Self::get_int_from_string(lhs)*Self::get_int_from_string(rhs)).to_string();
                        }
                        else if *op == BinOpKind::Div {
                            return (Self::get_int_from_string(lhs)/Self::get_int_from_string(rhs)).to_string();
                        }
                        else if *op == BinOpKind::Mod {
                            return (Self::get_int_from_string(lhs)%Self::get_int_from_string(rhs)).to_string();
                        }
                        else if *op == BinOpKind::Eq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)==Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::NEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)!=Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::Less {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)<Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::Bigger {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)>Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::LessOrEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)<=Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                        else if *op == BinOpKind::BiggerOrEq {
                            state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                            return (if Self::get_int_from_string(lhs)>=Self::get_int_from_string(rhs) {1} else {0}).to_string();
                        }
                    }
                }
                if lt == "float".to_string() {
                    if *op == BinOpKind::Add {
                        self.output += &format!("    {} = fadd {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Sub {
                        self.output += &format!("    {} = fsub {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Mul {
                        self.output += &format!("    {} = fmul {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Div {
                        self.output += &format!("    {} = fdiv {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Mod {
                        self.output += &format!("    {} = frem {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Eq {
                        self.output += &format!("    {} = fcmp oeq {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::NEq {
                        self.output += &format!("    {} = fcmp one {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::Less {
                        self.output += &format!("    {} = fcmp olt {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::Bigger {
                        self.output += &format!("    {} = fcmp ogt {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::LessOrEq {
                        self.output += &format!("    {} = fcmp ole {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::BiggerOrEq {
                        self.output += &format!("    {} = fcmp oge {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                }
                else {
                    // TODO: unsigned ops
                    if *op == BinOpKind::Add {
                        self.output += &format!("    {} = add {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Sub {
                        self.output += &format!("    {} = sub {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Mul {
                        self.output += &format!("    {} = mul {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Div {
                        self.output += &format!("    {} = udiv {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Mod {
                        self.output += &format!("    {} = urem {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::LShft {
                        self.output += &format!("    {} = shl {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::RShft {
                        self.output += &format!("    {} = lshr {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::And {
                        self.output += &format!("    {} = and {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Or {
                        self.output += &format!("    {} = or {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Xor {
                        self.output += &format!("    {} = xor {} {}, {}\n", reg, lt, lhs, rhs);
                    }
                    else if *op == BinOpKind::Eq {
                        self.output += &format!("    {} = icmp eq {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::NEq {
                        self.output += &format!("    {} = icmp ne {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::Less {
                        self.output += &format!("    {} = icmp ult {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::Bigger {
                        self.output += &format!("    {} = icmp ugt {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::LessOrEq {
                        self.output += &format!("    {} = icmp ule {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                    else if *op == BinOpKind::BiggerOrEq {
                        self.output += &format!("    {} = icmp uge {} {}, {}\n", reg, lt, lhs, rhs);
                        state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                    }
                }
            }
        }
        else if let IRData::UnaryOp(op, evl) = ir.data {
            let vl = self.get_value(state, *evl.clone());
            let vlt = self.get_backend_type(state.typ.clone().unwrap());
            if vlt == "float".to_string() {
                if op == UnaryOpKind::Plus {
                    self.output += &format!("    {} = fadd {} 0x{:016X}, {}\n", reg, vlt, 0.0_f64.to_bits(), vl);
                }
                else if op == UnaryOpKind::Minus {
                    self.output += &format!("    {} = fsub {} 0x{:016X}, {}\n", reg, vlt, 0.0_f64.to_bits(), vl);
                }
                else if op == UnaryOpKind::Not {
                    todo!("binary not doesn't implemented for f32 yet");
                }
                else if op == UnaryOpKind::LNot {
                    self.output += &format!("    {} = fcmp oeq {} 0x{:016X}, {}\n", reg, vlt, 0.0_f64.to_bits(), vl);
                    state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                }
            }
            else {
                if op == UnaryOpKind::Plus {
                    self.output += &format!("    {} = add {} 0, {}\n", reg, vlt, vl);
                }
                else if op == UnaryOpKind::Minus {
                    self.output += &format!("    {} = sub {} 0, {}\n", reg, vlt, vl);
                }
                else if op == UnaryOpKind::Not {
                    self.output += &format!("    {} = xor {} -1, {}\n", reg, vlt, vl);
                }
                else if op == UnaryOpKind::LNot {
                    self.output += &format!("    {} = icmp eq {} 0, {}\n", reg, vlt, vl);
                    state.typ = Some(IR::new(IRData::ID("bool".to_string()), None, None));
                }
            }
        }
        else if let IRData::ID(ref id) = ir.data {
            if *id == "__line__".to_string() {
                return self.get_value(state, IR::new(IRData::Integer(ir.row.unwrap() as u64), None, None));
            }
            if *id == "__col__".to_string() {
                return self.get_value(state, IR::new(IRData::Integer(ir.col.unwrap() as u64), None, None));
            }
            for var in self.local_variables.clone() {
                if var.name == *id {
                    if let Some(ref value) = var.value {
                        let result = self.get_value(state, value.clone());
                        state.typ = var.typ.clone();
                        return result;
                    }
                    self.output += &format!("    {} = load {}, ptr {}\n", reg, self.get_backend_type(var.typ.clone().unwrap()), var.real_name);
                    state.typ = var.typ.clone();
                    return reg;
                }
            }
            panic!("Failed to found variable `{}` at {}", id, self.fmt_pos(&ir));
        }
        else if let IRData::Integer(ref int) = ir.data {
            state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
            return format!("{}", int);
        }
        else if let IRData::Float(ref int) = ir.data {
            state.typ = Some(IR::new(IRData::ID("f32".to_string()), None, None));
            return format!("0x{:016X}", (int.clone() as f64).to_bits());
        }
        else if let IRData::Str(ref id) = ir.data {
            if let Some(str_reg) = self.strings.clone().get(id) {
                let temp_reg = format!("%{}", self.get_free_register());
                self.output += &format!("    {} = getelementptr i64, ptr {}\n", temp_reg, str_reg);
                self.output += &format!("    {} = ptrtoint ptr {} to i64\n", reg, temp_reg);
                state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
            }
            else {
                let temp_reg = format!("%{}", self.get_free_register());
                let str_reg = format!("@.str.{}", self.get_free_register());
                self.data_output += &format!("{} = private constant [{} x i8] c\"{}\\00\", align 1\n", str_reg, id.len()+1, id);
                self.output += &format!("    {} = getelementptr i64, ptr {}\n", temp_reg, str_reg);
                self.output += &format!("    {} = ptrtoint ptr {} to i64\n", reg, temp_reg);
                state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
                self.strings.insert(id.to_string(), str_reg);
            }
        }
        else if let IRData::Call(ref name, ref args) = ir.data {
            if *name == "return".to_string() {
                if args.len() == 0 {
                    self.output += "    ret void\n";
                    return reg;
                }
                let res = self.get_value(state, args[0].clone());
                self.output += &format!("    ret {} {}\n", self.get_backend_type(state.expected_typ.clone().unwrap()), res);
                self.end_of_block = true;
                return reg;
            }
            if *name == "CompTimeEvalute".to_string() {
                let modified_ir = self.dyn_interpret(state, args[0].clone());
                return self.get_value(state, modified_ir);
            }
            if *name == "SizeOf".to_string() {
                state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
                return format!("{}", self.get_type_size(args[0].clone()));
            }
            if *name == "IStruct".to_string() {
                let typ = args[0].clone();
                state.typ = Some(typ.clone());
                let temp_reg = self.get_free_register();
                let sp = self.get_free_register();
                self.output += &format!("    %{} = call ptr @llvm.stacksave()\n", sp);
                self.output += &format!("    %{} = alloca i1, i64 {}, align 16\n", temp_reg, self.get_type_size(typ.clone()));
                self.output += &format!("    call void @llvm.memset.inline.p0.p0.i64(ptr %{}, i8 0, i64 {}, i1 0)\n", temp_reg, self.get_type_size(typ.clone()));
                self.output += &format!("    {} = load {}, ptr %{}\n", reg, self.get_backend_type(typ), temp_reg);
                self.output += &format!("    call void @llvm.stackrestore(ptr %{})\n", sp);
                return reg;
            }
            if *name == "RunTime_SizeOf".to_string() {
                let temp = self.get_free_register();
                self.output += &format!("     %{} = getelementptr {}, ptr null, i32 1\n", temp, self.get_backend_type(args[0].clone()));
                self.output += &format!("     {} = ptrtoint ptr %{} to i64\n", reg, temp);
                state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
                return reg;
            }
            if *name == "alloc_stack".to_string() {
                let res = self.get_value(state, args[0].clone());
                
                let tmp = self.get_free_register();

                self.output += &format!("    %{} = alloca i8, i64 {}, align 16\n", tmp, res);
                self.output += &format!("    {} = ptrtoint ptr %{} to i64\n", reg, tmp);
                state.typ = Some(IR::new(IRData::ID("usize".to_string()), None, None));
                return reg;
            }
            if *name == "cast".to_string() {
                let value = self.get_value(state, args[0].clone());
                let vtyp = self.get_backend_type(state.typ.clone().unwrap());
                let ntyp = self.get_backend_type(args[1].clone());
                let vsize = self.get_type_size(state.typ.clone().unwrap());
                let nsize = self.get_type_size(args[1].clone());
                
                if vsize == nsize {
                    self.output += &format!("    {} = bitcast {} {} to {}\n", reg, vtyp, value, ntyp);
                }
                else if vsize > nsize {
                    self.output += &format!("    {} = trunc {} {} to {}\n", reg, vtyp, value, ntyp);
                }
                else if vsize < nsize {
                    // TODO: signed extend
                    self.output += &format!("    {} = zext {} {} to {}\n", reg, vtyp, value, ntyp);
                }
                state.typ = Some(args[1].clone());
                return reg;
            }
            if *name == "peek".to_string() {
                let int_addr = self.get_value(state, args[0].clone());
                let addr = format!("%{}", self.get_free_register());
                
                self.output += &format!("    {} = inttoptr {} {} to ptr\n", addr, self.get_backend_type(state.typ.clone().unwrap()), int_addr);
                
                if let IRData::Call(wrapper, args) = state.typ.clone().unwrap().data {
                    if *wrapper != "Ptr".to_string() {
                        panic!("Tried to peek from not a pointer {:?}", args[0].clone());
                    }
                    let elem_type = self.get_backend_type(args[0].clone());
                    
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
                
                self.output += &format!("    {} = inttoptr {} {} to ptr\n", addr, self.get_backend_type(addr_typ.clone().unwrap()), int_addr);
                
                if let IRData::Call(wrapper, args) = addr_typ.clone().unwrap().data {
                    if *wrapper != "Ptr".to_string() {
                        panic!("Tried to poke to not a pointer {:?}({:?})", oargs[0].clone(), addr_typ);
                    }
                    if args[0].clone().data != value_typ.clone().unwrap().data {
                        panic!("Tried to poke a value {:?} that doesn't compatiable with the pointer {:?}({:?})", oargs[1].clone(), oargs[0].clone(), addr_typ);
                    }
                    let elem_type = self.get_backend_type(value_typ.unwrap());
                    
                    self.output += &format!("    store {} {}, ptr {}\n", elem_type, value, addr);
                    state.typ = Some(IR::new(IRData::ID("i32".to_string()), None, None));
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
                    if func.ispolymorph {
                        // TODO: multi generic at one type
                        let mut arg_regs = vec![];
                        let mut new_func_args = vec![];
                        let mut generics: HashMap<String, IR> = HashMap::new();
                        for arg in 0..func.args.len() {
                            let areg = self.get_value(state, args[arg].clone());
                            if Self::is_it_a_polymorph_type_declaration(func.args[arg].typ.clone().unwrap()) {
                            }
                            else {
                                if state.typ.clone().unwrap().data != func.args[arg].typ.clone().unwrap().data {
                                    panic!("Excepted type {:?} at call generic function {} but got type {:?}", func.args[arg].typ.clone().unwrap(), *name, state.typ.clone().unwrap());
                                }
                            }
                            let (new_typ, new_name) = self.get_type_from_generic(func.args[arg].typ.clone().unwrap(), state.typ.clone().unwrap());
                            if new_name.is_some() {
                                if let Some(prev_typ) = generics.get(&new_name.clone().unwrap()) {
                                    if *prev_typ != new_typ {
                                        panic!("Excepted type {:?} for generic `{}` at {} but got {:?}", prev_typ, new_name.unwrap(), self.fmt_pos(&args[arg].clone()), new_typ);
                                    }
                                }
                                generics.insert(new_name.clone().unwrap(), new_typ.clone());
                            }
                            new_func_args.push(CodeGenVariable{name: func.args[arg].name.clone(), typ: Some(new_typ.clone())});
                            arg_regs.push(format!("{} {}", self.get_backend_type(new_typ), areg));
                        }

                        let ret_typ = self.get_type_from_generics(func.clone().typ, generics);
                        let new_func_reg = self.get_free_register();
                        let older_output = self.output.clone();
                        self.output = String::new();
                        {
                            let reg = new_func_reg.clone();
                            state.expected_typ = Some(ret_typ.clone());
                            self.output += &format!("define ccc {} @{}({}) {{\nentry:\n", self.get_backend_type(state.expected_typ.clone().unwrap()), reg, self.get_function_def_args(new_func_args.to_vec()));
                            let argv =
                                new_func_args.into_iter()
                                .map(
                                    |x| {
                                        let arg = self.get_free_register();
                                        self.output += &format!("    %{} = alloca {}\n", arg.clone(), self.get_backend_type(x.typ.clone().unwrap()));
                                        self.output += &format!("    store {} %{}, ptr %{}\n", self.get_backend_type(x.typ.clone().unwrap()), x.name.clone(), arg.clone());
                                        CodeGenFakeableVariable{
                                            name: format!("{}", x.name.clone()),
                                            real_name: format!("%{}", arg),
                                            typ: x.typ.clone(),
                                            value: None
                                        }
                                    }
                                ).collect::<Vec<_>>();
                            self.local_variables.extend(argv);
                            self.codegen(&state, func.body.clone().unwrap());
                            self.end_of_block = false;
                            if self.get_backend_type(state.expected_typ.clone().unwrap()) == "float" {
                                self.output += "    ret float 0.0\n}\n";
                            }
                            else if self.get_backend_type(state.expected_typ.clone().unwrap()) != "void" {
                                self.output += &format!("    ret {} 0\n}}\n", self.get_backend_type(state.expected_typ.clone().unwrap()));
                            }
                            else {
                                self.output += &format!("    ret void\n}}\n");
                            }
                        }
                        self.data_output += &self.output;
                        self.output = older_output;
                        
                        if self.get_backend_type(func.clone().typ) != "void".to_string() {
                            self.output += &format!("    {} = call ccc {} @{}({})\n", reg, self.get_backend_type(func.typ.clone()), new_func_reg, arg_regs.join(", "));
                            state.typ = Some(ret_typ.clone());
                        }
                        else {
                            self.output += &format!("    call ccc void @{}({})\n", new_func_reg, arg_regs.join(", "));
                            state.typ = Some(IR::new(IRData::ID("i32".to_string()), None, None));
                        }
                    }
                    else {
                        let mut arg_regs = vec![];
                        for arg in 0..func.args.len() {
                            let areg = self.get_value(state, args[arg].clone());
                            if state.typ.clone().unwrap().data != func.args[arg].typ.clone().unwrap().data {
                                panic!("Excepted type {:?} at call {} but got type {:?}", func.args[arg].typ.clone().unwrap(), *name, state.typ.clone().unwrap());
                            }
                            arg_regs.push(format!("{} {}", self.get_backend_type(func.args[arg].typ.clone().unwrap()), areg));
                        }
                        
                        if self.get_backend_type(func.typ.clone()) != "void".to_string() {
                            self.output += &format!("    {} = call ccc {} @{}({})\n", reg, self.get_backend_type(func.typ.clone()), func.real_name, arg_regs.join(", "));
                            state.typ = Some(func.typ);
                        }
                        else {
                            self.output += &format!("    call ccc void @{}({})\n", func.real_name, arg_regs.join(", "));
                            state.typ = Some(IR::new(IRData::ID("i32".to_string()), None, None));
                        }
                    }
                    return reg;
                }
            }
            panic!("Failed to found function/structure `{}` at {}", name, self.fmt_pos(&ir));
        }
        reg
    }
    pub fn is_it_a_polymorph_type_declaration(typ: IR) -> bool {
        if let IRData::Call(ref typename, ref targs) = typ.data {
            if *typename == "PolyMorph".to_string() {
                return true;
            }
            for targ in targs {
                if Self::is_it_a_polymorph_type_declaration(targ.clone()) {
                    return true;
                }
            }
        }
        false
    }
    pub fn get_type_from_generic(&mut self, generic: IR, given: IR) -> (IR, Option<String>) {
        if let IRData::Call(ref typename, ref targs) = generic.clone().data {
            if *typename == "PolyMorph".to_string() {
                let IRData::ID(ref inner) = targs[0].clone().data else {panic!("Excepted ID at {} but got {:?}", self.fmt_pos(&targs[0].clone()), targs[0].clone())};
                return (given, Some(inner.to_string()));
            }
            if let IRData::Call(ref gname, ref gargs) = given.clone().data {
                if gargs.len() != targs.len() {
                    panic!("Excepted parameter type at {} like {:?} but got {:?}", self.fmt_pos(&given), generic, given);
                }
                let mut new_targs = vec![];
                let mut typ = None;
                for targi in 0..targs.len() {
                    let (new_arg, new_type) = self.get_type_from_generic(targs[targi].clone(), gargs[targi].clone());
                    if new_type.is_some() {
                        typ = Some(new_type.unwrap());
                    }
                    new_targs.push(new_arg);
                }
                let mut generic = generic.clone();
                generic.data = IRData::Call(typename.clone(), new_targs);
                return (generic, typ);
            }
            else {
                panic!("Excepted parameter type at {} like {:?} but got {:?}", self.fmt_pos(&given), generic, given);
            }
        }
        (generic, None)
    }
    pub fn get_type_from_generics(&mut self, generic: IR, generics: HashMap<String, IR>) -> IR {
        if let IRData::Call(ref typename, ref targs) = generic.clone().data {
            if *typename == "PolyMorph".to_string() {
                let IRData::ID(ref inner) = targs[0].clone().data else {panic!("Excepted ID at {} but got {:?}", self.fmt_pos(&targs[0].clone()), targs[0].clone())};
                if let Some(ref typ) = generics.get(inner) {
                    return typ.clone().clone();
                }
                else {
                    panic!("Unknown generic {:?} at {}", generic, self.fmt_pos(&generic));
                }
            }
            let mut new_targs = vec![];
            for targi in 0..targs.len() {
                new_targs.push(self.get_type_from_generics(targs[targi].clone(), generics.clone()));
            }
            let mut generic = generic.clone();
            generic.data = IRData::Call(typename.clone(), new_targs);
            return generic;
        }
        generic
    }
    pub fn codegen(&mut self, state: &CodeGenState, ir: IR) -> CodeGenState {
        let mut state = state.clone();
        if let IRData::DeclareFunction(ref dt, ref name, ref typ, ref args, ref body) = ir.data {
            let variables = self.local_variables.clone();
            {
                let mut i = 0;
                for func in self.functions.clone() {
                    if func.name == *name {
                        self.functions.remove(i);
                        break;
                    }
                    i += 1;
                }
            }
            if *dt == DeclarationType::Global {
                // TODO: generics for global functions
                state.expected_typ = Some(*typ.clone());
                self.output += &format!("define ccc {} @{}({}) {{\nentry:\n", self.get_backend_type(state.expected_typ.clone().unwrap()), name, self.get_function_def_args(args.to_vec()));
                let argv =
                    args.into_iter()
                    .map(
                        |x| {
                            let arg = self.get_free_register();
                            self.output += &format!("    %{} = alloca {}\n", arg.clone(), self.get_backend_type(x.typ.clone().unwrap()));
                            self.output += &format!("    store {} %{}, ptr %{}\n", self.get_backend_type(x.typ.clone().unwrap()), x.name.clone(), arg.clone());
                            CodeGenFakeableVariable{
                                name: format!("{}", x.name.clone()),
                                real_name: format!("%{}", arg),
                                typ: x.typ.clone(),
                                value: None
                            }
                        }
                    ).collect::<Vec<_>>();
                self.local_variables.extend(argv);
                self.codegen(&state, *body.clone().unwrap());
                self.end_of_block = false;
                if self.get_backend_type(state.expected_typ.clone().unwrap()) == "float" {
                    self.output += "    ret float 0.0\n}\n";
                }
                else if self.get_backend_type(state.expected_typ.clone().unwrap()) != "void" {
                    self.output += &format!("    ret {} 0\n}}\n", self.get_backend_type(state.expected_typ.clone().unwrap()));
                }
                else {
                    self.output += &format!("    ret void\n}}\n");
                }
                self.functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: Some(*body.clone().unwrap()), other_implementations: vec![]});
            }
            else if *dt == DeclarationType::Local {
                let mut ispolymorph = false;
                for arg in args.clone() {
                    if Self::is_it_a_polymorph_type_declaration(arg.typ.unwrap()) {
                        ispolymorph = true;
                        break;
                    }
                }
                if ispolymorph {
                    self.functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: true, body: Some(*body.clone().unwrap()), other_implementations: vec![]});
                }
                else {
                    state.expected_typ = Some(*typ.clone());
                    let reg = self.get_free_register();
                    self.output += &format!("define ccc {} @{}({}) {{\nentry:\n", self.get_backend_type(state.expected_typ.clone().unwrap()), reg, self.get_function_def_args(args.to_vec()));
                    let argv =
                        args.into_iter()
                        .map(
                            |x| {
                                let arg = self.get_free_register();
                                self.output += &format!("    %{} = alloca {}\n", arg.clone(), self.get_backend_type(x.typ.clone().unwrap()));
                                self.output += &format!("    store {} %{}, ptr %{}\n", self.get_backend_type(x.typ.clone().unwrap()), x.name.clone(), arg.clone());
                                CodeGenFakeableVariable{
                                    name: format!("{}", x.name.clone()),
                                    real_name: format!("%{}", arg),
                                    typ: x.typ.clone(),
                                    value: None
                                }
                            }
                        ).collect::<Vec<_>>();
                    self.local_variables.extend(argv);
                    self.codegen(&state, *body.clone().unwrap());
                    self.end_of_block = false;
                    if self.get_backend_type(state.expected_typ.clone().unwrap()) == "float" {
                        self.output += "    ret float 0.0\n}\n";
                    }
                    else if self.get_backend_type(state.expected_typ.clone().unwrap()) != "void" {
                        self.output += &format!("    ret {} 0\n}}\n", self.get_backend_type(state.expected_typ.clone().unwrap()));
                    }
                    else {
                        self.output += &format!("    ret void\n}}\n");
                    }
                    self.functions.push(CodeGenFunction{name: name.to_string(), real_name: reg.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: Some(*body.clone().unwrap()), other_implementations: vec![]});
                }
            }
            else if let DeclarationType::Extern(ref ename) = *dt {
                self.output += &format!("declare ccc i32 @{}({})\n", ename, self.get_function_def_args(args.to_vec()));
                self.functions.push(CodeGenFunction{name: name.to_string(), real_name: ename.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: None, other_implementations: vec![]});
            }
            else if let DeclarationType::Forward = *dt {
                self.functions.push(CodeGenFunction{name: name.to_string(), real_name: name.to_string(), typ: *typ.clone(), args: args.clone(), decl: dt.clone(), ispolymorph: false, body: None, other_implementations: vec![]});
            }
            self.local_variables = variables;
        }
        /*
        else if let IRData::DeclareStructure(ref name, ref block) = ir {
        }
        else if let IRData::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir {
            if *dt == DeclarationType::Global {
                if let IRData::ID(ref typ) = **typ {
                }
            }
            else if *dt == DeclarationType::Local {
                if let IRData::ID(ref typ) = **typ {
                }
            }
            else if let DeclarationType::Extern(ref ename) = *dt {
                if let IRData::ID(ref typ) = **typ {
                }
            }
    }
         */
        
        else if let IRData::Import(ref module) = ir.data {
            let filename;
            if module.starts_with("./") {
                filename = module.clone();
            }
            else {
                filename = format!("/opt/Faivy/modules/{}", module);
            }
            let mut already_imported = false;
            for imported in &self.imported {
                if *imported == filename {
                    already_imported = true;
                }
            }
            if !already_imported {
                return todo!();
                /*
                self.imported.push(filename.clone());
                let ast = FaivyParser::pretty_parse(
                    FaivyParser::parse(Rule::program, &fs::read_to_string(&filename).unwrap()
                    ).unwrap()
                );
                let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
                self.codegen(&CodeGenState {variables: vec![], typ: None, expected_typ: None}, IR::new(IRData::Block(ir), None, None));
                */
            }
        }
        else if let IRData::CompTimeRun(ref code) = ir.data {
            self.dyn_interpret(&mut state, *code.clone());
        }
        else if let IRData::Asm(_) = ir.data {
            todo!("Inline assembly doesn't supported yet")
        }
        else if let IRData::LLVM(ref code) = ir.data {
            self.output += &format!("{}\n", code);
        }
        else if let IRData::If(ref cond, ref then, ref otherwise) = ir.data {
            let variables = self.local_variables.clone();
            let rcond = self.get_value(&mut state, *cond.clone());
            if self.get_backend_type(state.typ.clone().unwrap()) != "i1".to_string() {
                panic!("Excepted bool but got {:?} at {}", state.typ.clone(), self.fmt_pos(&ir));
            }
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
        else if let IRData::Defer(ref body) = ir.data {
            self.defer_block = [vec![*body.clone()], self.defer_block.clone()].concat();
        }
        else if let IRData::While(ref cond, ref body) = ir.data {
            let variables = self.local_variables.clone();
            let rbody = self.get_free_register();
            let rreal_body = self.get_free_register();
            let rend = self.get_free_register();
            let sp = self.get_free_register();
            self.output += &format!("    br label %{}\n", rbody);
            self.output += &format!("{}:\n", rbody);
            self.output += &format!("    %{} = call ptr @llvm.stacksave()\n", sp);
            let rcond = self.get_value(&mut state, *cond.clone());
            if self.get_backend_type(state.typ.clone().unwrap()) != "i1".to_string() {
                panic!("Excepted bool but got {:?}", state.typ.clone());
            }
            self.output += &format!("    br i1 {}, label %{}, label %{}\n", rcond, rreal_body, rend);
            self.output += &format!("{}:\n", rreal_body);
            self.codegen(&state, *body.clone());
            self.end_of_block = false;
            self.output += &format!("    call void @llvm.stackrestore(ptr %{})\n", sp);
            self.output += &format!("    br label %{}\n", rbody);
            self.output += &format!("{}:\n", rend);
            self.output += &format!("    call void @llvm.stackrestore(ptr %{})\n", sp);
            self.local_variables = variables;
        }
        else if let IRData::DeclareStructure(ref name, ref fields) = ir.data {
            let sid = format!("%{}", self.get_free_register());
            self.output += &format!("{} = type {{{}}}\n", sid, fields.into_iter().map(|x| format!("{}", self.get_backend_type(x.typ.clone().unwrap()))).collect::<Vec<_>>().join(", "));
            
            self.structs.push(CodeGenStruct{name: name.to_string(), real_name: sid, fields: fields.to_vec()});
        }
        else if let IRData::DeclareVariable(ref dt, ref name, ref typ, ref body) = ir.data {
            if *dt == DeclarationType::Local {
                let value = self.get_value(&mut state, *body.clone().unwrap());
                if state.typ.clone().unwrap().data != (*typ.clone()).data {
                    panic!("Excepted {:?} but got {:?}", (*typ.clone()).data, state.typ.clone().unwrap().data);
                }
                let var = self.get_free_register();
                
                self.output += &format!("    %{} = alloca {}\n", var, self.get_backend_type(*typ.clone()));
                self.output += &format!("    store {} {}, ptr %{}\n", self.get_backend_type(*typ.clone()), value, var);
                
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: format!("%{}", var), value: None, typ: Some(*typ.clone())});
            }
            else if let DeclarationType::Extern(ref ename) = *dt {
                self.output += &format!("@{} = external global {}", ename.clone(), self.get_backend_type(*typ.clone()));
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: format!("@{}", ename), value: None, typ: Some(*typ.clone())});
            }
            else if let DeclarationType::Const = *dt {
                let rn = self.dyn_interpret(&mut state, *body.clone().unwrap());
                self.local_variables.push(CodeGenFakeableVariable{name: name.to_string(), real_name: name.to_string(), value: Some(rn), typ: Some(*typ.clone())});
            }
        }
        else if let IRData::Set(ref name, ref body) = ir.data {
            let value = self.get_value(&mut state, *body.clone());
            for var in self.local_variables.clone() {
                if IRData::ID(var.name) == (**name).data {
                    self.output += &format!("    store {} {}, ptr {}\n", self.get_backend_type(var.typ.clone().unwrap()), value, var.real_name);
                    return state;
                }
            }
            panic!("Failed to found variable `{:?}` at {}", name, self.fmt_pos(&ir));
        }
        else if let IRData::Block(ref insts) = ir.data {
            for inst in insts {
                if self.end_of_block {
                    break;
                }
                self.codegen(&mut state, inst.clone());
            }
        }
        else if let IRData::Scope(ref insts) = ir.data {
            let dfb = self.defer_block.clone();
            self.defer_block = vec![];
            let variables = self.local_variables.clone();
            let sp = self.get_free_register();
            self.output += &format!("    %{} = call ptr @llvm.stacksave()\n", sp);
            for inst in insts {
                if self.end_of_block {
                    break;
                }
                self.codegen(&mut state, inst.clone());
            }
            if !self.end_of_block {
                for block in self.defer_block.clone() {
                    self.codegen(&mut state, block.clone());
                }
                self.output += &format!("    call void @llvm.stackrestore(ptr %{})\n", sp);
            }
            self.local_variables = variables;
            self.defer_block = dfb;
        }
        else if ir.data == IRData::Dummy {
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
