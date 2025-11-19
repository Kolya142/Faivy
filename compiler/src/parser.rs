use crate::{codegen::*, ir::*};

#[derive(Debug,Clone,PartialEq)]
pub enum Token {
    Id(String),
    Str(String),
    Nat(usize),
    Float(f32),
    
    Comma,
    Colon,
    SemiColon,
    Plus,
    Minus,
    Asterisk,
    FrontSlash,
    Eq,
    EqEq,
    PlusEq,
    MinusEq,
    AsteriskEq,
    FrontSlashEq,
    Less,
    Greater,
    LessLess,
    GreaterGreater,
    Bang,
    BangEq,
    Hash,
    Ampersand,
    AmpersandEq,
    AmpersandAmpersand,
    Pipe,
    PipeEq,
    PipePipe,
    Carret,
    CarretEq,
    Dot
}

#[derive(Debug,Clone,PartialEq)]
pub struct TokenMeta {
    tok: Token,
    row: usize,
    col: usize,
}

pub fn lexer(text: String) -> Vec<TokenMeta> {
    let mut toks = vec![];
    let mut i = 0usize;
    let mut row = 1; // In Emacs first character at 1:0.
    let mut col = 0;
    let chars: Vec<char> = text.chars().collect();
    while i < chars.len() {
        let c = chars[i];
        match c {
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            },
            '\n' => {
                i += 1;
                row += 1;
                col = 0;
            },
            'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                let mut content = String::new();
                while i < chars.len() {
                    let c = chars[i];
                    match c {
                        'a' ..= 'z' | 'A' ..= 'Z' | '_' | '0' ..= '9' => {
                            i += 1;
                            col += 1;
                            content.push(c);
                        }
                        _ => break
                    }
                }
                toks.push(TokenMeta{tok: Token::Id(content), row: row, col: col});
            },
            '0' ..= '9' => {
                let mut content = String::new();
                let mut is_f = false;
                while i < chars.len() {
                    let c = chars[i];
                    match c {
                        '0' ..= '9' | '.' => {
                            i += 1;
                            col += 1;
                            content.push(c);
                        }
                        'f' => {
                            i += 1;
                            col += 1;
                            is_f = true;
                            break;
                        }
                        _ => break
                    }
                }
                if is_f {
                    toks.push(TokenMeta{tok: Token::Float(content.parse::<f32>().unwrap()), row: row, col: col});
                }
                else {
                    toks.push(TokenMeta{tok: Token::Nat(content.parse::<usize>().unwrap()), row: row, col: col});
                }
            },
            ',' | ':' | ';' | '+' | '-' | '&' | '/' | '=' | '<' | '>' | '!' | '*' | '#' | '|' | '^' | '.' => {
                i += 1;
                col += 1;
                if c == ',' {toks.push(TokenMeta{tok: Token::Comma, row: row, col: col});} else
                    if c == ':' {toks.push(TokenMeta{tok: Token::Colon, row: row, col: col});} else
                    if c == ';' {toks.push(TokenMeta{tok: Token::SemiColon, row: row, col: col});} else
                    if c == '#' {toks.push(TokenMeta{tok: Token::Hash, row: row, col: col});} else
                    if c == '^' {toks.push(TokenMeta{tok: Token::Carret, row: row, col: col});} else
                    if c == '.' {toks.push(TokenMeta{tok: Token::Dot, row: row, col: col});} else
                    if c == '+' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::PlusEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Plus, row: row, col: col});}} else
                    if c == '-' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::MinusEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Minus, row: row, col: col});}} else
                    if c == '*' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::AsteriskEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Asterisk, row: row, col: col});}} else
                    if c == '&' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::AmpersandEq, row: row, col: col});}
                        else if chars[i] == '&' {toks.push(TokenMeta{tok: Token::AmpersandAmpersand, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Ampersand, row: row, col: col});}} else
                    if c == '/' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::FrontSlashEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::FrontSlash, row: row, col: col});}} else
                    if c == '=' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::EqEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Eq, row: row, col: col});}} else
                    if c == '<' {
                        if chars[i] == '<' {toks.push(TokenMeta{tok: Token::LessLess, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Less, row: row, col: col});}} else
                    if c == '>' {
                        if chars[i] == '>' {toks.push(TokenMeta{tok: Token::GreaterGreater, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Greater, row: row, col: col});}} else
                    if c == '!' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::BangEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Bang, row: row, col: col});}} else
                    if c == '#' {toks.push(TokenMeta{tok: Token::Hash, row: row, col: col});} else
                    if c == '|' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::PipeEq, row: row, col: col});}
                        else if chars[i] == '|' {toks.push(TokenMeta{tok: Token::PipePipe, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Pipe, row: row, col: col});}} else
                    if c == '^' {
                        if chars[i] == '=' {toks.push(TokenMeta{tok: Token::CarretEq, row: row, col: col});}
                        else {toks.push(TokenMeta{tok: Token::Carret, row: row, col: col});}} else
                    if c == '.' {toks.push(TokenMeta{tok: Token::Dot, row: row, col: col});}
            },
            _ => todo!("{}", c)
        }
    }
    toks
}

fn parser_program(toks: &[TokenMeta]) -> (&[TokenMeta], IR) {
    todo!()
}
