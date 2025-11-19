use rand::Rng;
use std::process::Command;
use std::fs;
use std::env;
mod codegen;
use codegen::*;
mod parser;
use parser::*;
mod interpreter;
mod ir;
use ir::*;

unsafe extern "C" {
    fn system(command: *const i8) -> i32;
    fn dlopen(path: *const i8, flags: i32) -> *mut (); // WTF?
    fn dlclose(handle: *mut ()) -> i32;
    fn dlsym(handle: *mut (), symbol: *const i8) -> *mut ();
    fn malloc(size: usize) -> *mut (); // sorry.
    fn free(ptr: *mut ()) -> ();
    fn memcpy(dst: *mut (), src: *const (), len: usize) -> *mut ();
    fn printf(fmt: *const i8, ...) -> i32;
}

fn main() -> Result<(), ()> {
    println!("{:?}", lexer("church alan turing alphawolf69 69 96 42 64 52 37 #run #piper << >> <= := ^^ &*#* no god or king. only man. ".to_string()));
    /*
    let argv: Vec<_> = env::args().collect();
    if argv.len() < 2 {
        eprintln!("Usage: {} [--help] [--dry] [--emit-only-llvm] <Faivy source code file name> [linker flags]", argv[0].clone());
        return Err(());
    }

    let mut argi: usize = 1;
    let mut is_dry: bool = false;
    let mut is_emit_only_llvm: bool = false;
    while argi < argv.len() {
        if argv[argi] == "--help" {
            println!("Usage: {} [--help] [--dry] [--emit-only-llvm] <Faivy source code file name> [linker flags]", argv[0].clone());
            return Ok(());
        }
        else if argv[argi] == "--dry" {
            is_dry = true;
            argi += 1;
        }
        else if argv[argi] == "--emit-only-llvm" {
            is_emit_only_llvm = true;
            argi += 1;
        }
        else {
            break;
        }
    }
    
    let ast = FaivyParser::pretty_parse(
        FaivyParser::parse(Rule::program, &fs::read_to_string(argv[argi].clone()).unwrap()
        ).unwrap()
    );
    let ir = ast.into_iter().map(IRBuilder::build).collect::<Vec<_>>();
    let mut codegen = CodeGen::new(Some(argv[argi].clone()));
    argi += 1;
    codegen.codegen(&CodeGenState {variables: vec![], typ: None, expected_typ: None}, IR::new(IRData::Block(ir), None, None));
    let mut code = String::new();
    code += "declare ptr @llvm.stacksave()\n";
    code += "declare void @llvm.stackrestore(ptr)\n";
    code += "declare void @llvm.memset.p0.p0.i64(ptr, i8, i64, i1)\n";
    code += &codegen.data_output;
    code += &codegen.output;
    if is_emit_only_llvm {
        print!("{}", code);
    }
    else {
        if !is_dry {
            let mut rng = rand::rng();
            let temp_llvm_ir = format!("/tmp/{}.ll", rng.random::<u32>().to_string());
            let _ = fs::write(temp_llvm_ir.clone(), code);
            let mut cc = Command::new("clang");
            cc.arg("-lm").arg(temp_llvm_ir).arg("-o").arg(format!("{}.out", argv[1].clone()));
            for i in argi..argv.len() {
                cc.arg(argv[i].clone());
            }
            let cc_output = cc.output().unwrap();
            if !cc_output.clone().status.success() {
                panic!("Failed to link program:\n{:?}", cc_output);
            }
        }
}
    */
    return Ok(());
}
