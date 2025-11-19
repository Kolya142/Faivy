use pest_derive::Parser;
use pest::iterators::Pairs;
use crate::{codegen::*, ir::*};

#[derive(Parser)]
#[grammar = "../syntax.pest"]
pub struct FaivyParser;

#[derive(Debug,Clone,PartialEq)]
pub struct PrettyAst {
    rule: Rule,
    span: String,
    inner: Vec<PrettyAst>,
    row: usize,
    col: usize,
}

impl FaivyParser {
    pub fn pretty_parse(ast: Pairs<Rule>) -> Vec<PrettyAst> {
        ast.map(|x|
                {
                    let (line, col) = x.line_col();
                    PrettyAst {
                        rule: x.as_rule(),
                        span: x.as_span().as_str().to_string(),
                        row: line,
                        col: col,
                        inner: Self::pretty_parse(x.into_inner())
                    }
                }
        ).collect()
    }
}

pub struct IRBuilder {
}

impl IRBuilder {
    pub fn build_field(ast: PrettyAst) -> Option<CodeGenVariable> {
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
    
    pub fn build_args(ast: PrettyAst) -> Option<Vec<CodeGenVariable>> {
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
    
    pub fn build_expr_args(ast: PrettyAst) -> Option<Vec<IR>> {
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
    
    pub fn build(ast: PrettyAst) -> IR {
        match ast.rule {
            Rule::EOI => IR::new(IRData::Dummy, Some(ast.row), Some(ast.col)),
            Rule::WHITESPACE => IR::new(IRData::Dummy, Some(ast.row), Some(ast.col)),
            Rule::program => IR::new(IRData::Block(ast.inner.into_iter().map(Self::build).collect()), Some(ast.row), Some(ast.col)),
            Rule::stmt => IR::new(IRData::Block(ast.inner.into_iter().map(Self::build).collect()), Some(ast.row), Some(ast.col)),
            Rule::block => IR::new(IRData::Scope(ast.inner.into_iter().map(Self::build).collect()), Some(ast.row), Some(ast.col)),
            Rule::decl_extern_type => {
                IR::new(IRData::Declaration(DeclarationType::Extern(ast.inner[0].span.clone())), Some(ast.row), Some(ast.col))
            },
            Rule::decl_global_type => {
                IR::new(IRData::Declaration(DeclarationType::Global), Some(ast.row), Some(ast.col))
            },
            Rule::decl_local_type => {
                IR::new(IRData::Declaration(DeclarationType::Local), Some(ast.row), Some(ast.col))
            },
            Rule::decl_forward_type => {
                IR::new(IRData::Declaration(DeclarationType::Forward), Some(ast.row), Some(ast.col))
            },
            Rule::decl_const_type => {
                IR::new(IRData::Declaration(DeclarationType::Const), Some(ast.row), Some(ast.col))
            },
            Rule::decl_type => {
                Self::build(ast.inner[0].clone())
            },
            Rule::identifier => {
                IR::new(IRData::ID(ast.span.clone()), Some(ast.row), Some(ast.col))
            },
            Rule::string => {
                let mut parsed_str = ast.span.clone()[1..ast.span.clone().len()-1].to_string();
                parsed_str = parsed_str.replace("\\n", "\n");
                parsed_str = parsed_str.replace("\\r", "\r");
                parsed_str = parsed_str.replace("\\t", "\t");
                parsed_str = parsed_str.replace("\\e", "\x1b");
                parsed_str = parsed_str.replace("\\\\", "\\");
                parsed_str = parsed_str.replace("\\'", "\'");
                parsed_str = parsed_str.replace("\\\"", "\"");
                for i in 0..256 {
                    parsed_str = parsed_str.replace(&format!("\\x{:2X}", i), &format!("{}", i as u8 as char));
                }
                IR::new(IRData::Str(parsed_str), Some(ast.row), Some(ast.col))
            },
            Rule::number => {
                if ast.span.clone().contains(".") {
                    IR::new(IRData::Float(ast.span.clone().parse::<f32>().unwrap()), Some(ast.row), Some(ast.col))
                }
                else {
                    if ast.span.clone().starts_with("0x") {
                        IR::new(IRData::Integer(u64::from_str_radix(&ast.span.clone()[2..], 16).unwrap()), Some(ast.row), Some(ast.col))
                    }
                    else {
                        IR::new(IRData::Integer(ast.span.clone().parse::<u64>().unwrap()), Some(ast.row), Some(ast.col))
                    }
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
                    BinOpKind::Xor } else if op == "->".to_string() {
                    BinOpKind::Dot } else {todo!("Add more binops")};
                IR::new(IRData::BinOp(binopkind, Box::new(lhs), Box::new(rhs)), Some(ast.row), Some(ast.col))
            },
            Rule::unaryop => {
                let op = ast.inner[0].span.clone();
                let vl = Self::build(ast.inner[1].clone());
                let unopkind = if op == "+".to_string() {
                    UnaryOpKind::Plus } else if op == "-".to_string() {
                    UnaryOpKind::Minus } else if op == "~".to_string() {
                    UnaryOpKind::Not } else if op == "!".to_string() {
                    UnaryOpKind::LNot } else {todo!("Add more unaryops")};
                IR::new(IRData::UnaryOp(unopkind, Box::new(vl)), Some(ast.row), Some(ast.col))
            },
            Rule::expr_atom => {
                Self::build(ast.inner[0].clone())
            },
            Rule::expr => {
                Self::build(ast.inner[0].clone())
            },
            Rule::defer => {
                IR::new(IRData::Defer(Box::new(Self::build(ast.inner[0].clone()))), Some(ast.row), Some(ast.col))
            },
            Rule::import => {
                IR::new(IRData::Import(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string()), Some(ast.row), Some(ast.col))
            },
            Rule::asm => {
                IR::new(IRData::Asm(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string()), Some(ast.row), Some(ast.col))
            },
            Rule::llvm => {
                IR::new(IRData::LLVM(ast.inner[0].span.clone()[1..ast.inner[0].span.clone().len()-1].to_string()), Some(ast.row), Some(ast.col))
            },
            Rule::comptime_run => {
                IR::new(IRData::CompTimeRun(Box::new(Self::build(ast.inner[0].clone()))), Some(ast.row), Some(ast.col))
            },
            Rule::call => {
                IR::new(IRData::Call(ast.inner[0].span.clone(), Self::build_expr_args(ast.inner[1].clone()).unwrap()), Some(ast.row), Some(ast.col))
            },
            Rule::if_stmt => {
                IR::new(IRData::If(Box::new(Self::build(ast.inner[0].clone())), Box::new(Self::build(ast.inner[1].clone())), if ast.inner.len() == 3 {Some(Box::new(Self::build(ast.inner[2].clone())))} else {None}), Some(ast.row), Some(ast.col))
            }
            Rule::while_stmt => {
                IR::new(IRData::While(Box::new(Self::build(ast.inner[0].clone())), Box::new(Self::build(ast.inner[1].clone()))), Some(ast.row), Some(ast.col))
            }
            Rule::func => {
                let IRData::Declaration(decl) = Self::build(ast.inner[0].clone()).data else {panic!()};
                let IRData::ID(name) = Self::build(ast.inner[1].clone()).data else {panic!()};
                let typ = Self::build(ast.inner[3].clone());
                IR::new(IRData::DeclareFunction(decl, name, Box::new(typ), Self::build_args(ast.inner[2].clone()).unwrap(), if ast.inner.len() == 5 {Some(Box::new(Self::build(ast.inner[4].clone())))} else {None}), Some(ast.row), Some(ast.col))
            },
            Rule::variable => {
                let IRData::Declaration(decl) = Self::build(ast.inner[0].clone()).data else {panic!()};
                let IRData::ID(name) = Self::build(ast.inner[1].clone()).data else {panic!()};
                let typ = Self::build(ast.inner[2].clone());
                IR::new(IRData::DeclareVariable(decl, name, Box::new(typ), if ast.inner.len() == 4 {Some(Box::new(Self::build(ast.inner[3].clone())))} else {None}), Some(ast.row), Some(ast.col))
            },
            Rule::set => {
                let dest = Self::build(ast.inner[0].clone());
                let value = Self::build(ast.inner[1].clone());
                IR::new(IRData::Set(Box::new(dest), Box::new(value)), Some(ast.row), Some(ast.col))
            },
            Rule::struct_decl => {
                let IRData::ID(name) = Self::build(ast.inner[0].clone()).data else {panic!()};
                IR::new(IRData::DeclareStructure(name, Self::build_args(PrettyAst{rule: Rule::args, span: String::new(), inner: ast.inner[1..].to_vec(), row: ast.row, col: ast.col}).unwrap()), Some(ast.row), Some(ast.col))
            },
            _ => IR::new(IRData::Dummy, None, None),
            
        }
    }
}
