#pragma once
#include <unordered_map>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <stdint.h>
#include <stdarg.h>
#include <vector>

namespace Faivy {
    enum ByteCodeInstKind {
        B_CALL, // stack --
        B_RET, // stack --
        B_STR, // n0 -- stack
        B_PUSH, // n0 -- stack
        B_ADD, // stack -- stack
        B_ALLOCA, // n0 -- s0 stack
        B_SAVE_STACK, // -- s0
        B_RES_STACK, // s0 --
        B_CPP, // s0 -- insert s0 as CPP code to output
        B_OPEN_PROC, // s0 -- 
    };
    struct ByteCodeInst {
        ByteCodeInstKind kind;
        std::string s0;
        std::string s1;
        size_t n0;
        size_t n1;
    };
    template<typename T>
    struct Slice {
        T *start;
        size_t size;
        Slice() {
        }
        Slice(T *start, size_t size) {
            this->start = start;
            this->size = size;
        }
        Slice<T> skip(size_t a = 1) {
            return Slice(start+a, size-a);
        }
    };
    std::string ssprintf(const char *fmt, ...);
    void interpret(Slice<ByteCodeInst> code, Slice<uint8_t> static_data);
    size_t istack_pop64(Slice<uint8_t> *s, size_t *sp);
    void istack_push64(Slice<uint8_t> *s, size_t *sp, size_t v);
    std::string compile(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs);
}

namespace Parser {
    enum TokenKind {
        TK_ID,
        TK_STR,
        TK_NAT,
        TK_PLUS,
        TK_SEMICOLON,
        TK_2COLON,
        TK_COLON,
        TK_LPAR,
        TK_RPAR,
        TK_LCB,
        TK_RCB,
        TK_COMMA,
    };
    struct Token {
        TokenKind kind;
        std::string s;
        size_t row, col;
        std::string to_string();
    };
    std::vector<Token> lexer(const char *code);
    enum AstKind {
        AK_ID,
        AK_STR,
        AK_NUM,
        AK_SUM,
        AK_CALL,
        AK_PROC,
        AK_SEQ,
        AK_FIELD,
    };
    struct Ast {
        AstKind kind;
        std::vector<Ast> inner;
        std::string s;
        size_t n;
        std::string to_string();
    };
    struct PR {
        Faivy::Slice<Token> rest;
        Ast ast;
        bool is_the;
    };
    PR parse_primary(Faivy::Slice<Token> toks);
    Faivy::Slice<Token> skip(Faivy::Slice<Token> toks, TokenKind tk);
    PR parse_rexpr(Faivy::Slice<Token> toks);
    PR parse_proc(Faivy::Slice<Token> toks);
    PR parse_stmt(Faivy::Slice<Token> toks);
    PR parse_block(Faivy::Slice<Token> toks);
}

// https://stackoverflow.com/questions/313432/c-extend-a-vector-with-another-vector#answer-64102335
#define vector_extend(a, b) (a).insert((a).end(), (b).begin(), (b).end())

namespace Compiler {
    void compile(std::vector<Faivy::ByteCodeInst> *prog, Parser::Ast ast, std::unordered_map<std::string, size_t> *procs, std::vector<uint8_t> *static_data);
}

