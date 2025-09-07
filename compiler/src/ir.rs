use crate::codegen::*;

#[derive(Debug,Clone,PartialEq)]
pub enum DeclarationType {
    Extern(String),
    Global,
    Local,
    Forward,
    Const,
}

#[derive(Debug,Clone,PartialEq)]
pub enum BinOpKind {
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
    Dot,
    Eq,
    NEq,
    Less,
    Bigger,
    LessOrEq,
    BiggerOrEq,
}

#[derive(Debug,Clone,PartialEq)]
pub enum UnaryOpKind {
    Not,
    Minus,
    Plus,
    LNot
}

#[derive(Debug,Clone,PartialEq)]
pub enum IRData {
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
    UnaryOp(UnaryOpKind, Box<IR>),
    Block(Vec<IR>),
    Scope(Vec<IR>),
    Import(String),
    Asm(String),
    LLVM(String),
    Defer(Box<IR>),
    CompTimeRun(Box<IR>),
    If(Box<IR>, Box<IR>, Option<Box<IR>>),
    While(Box<IR>, Box<IR>),
}

#[derive(Debug,Clone)]
pub struct IR {
    pub data: IRData,
    pub row: Option<usize>,
    pub col: Option<usize>,
}

impl PartialEq for IR {
    fn eq(&self, other: &IR) -> bool {
	return self.data == other.data;
    }
}

impl IR {
    pub fn new(data: IRData, row: Option<usize>, col: Option<usize>) -> IR {
	IR {
	    data: data, row: row, col: col
	}
    }
}
